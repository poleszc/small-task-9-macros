use proc_macro::TokenStream;
use quote::{format_ident, quote};
use syn::{
    parse_macro_input, spanned::Spanned, Data, DeriveInput, Error, Field, Fields, GenericArgument,
    Ident, Lit, Meta, NestedMeta, PathArguments, Type,
};

#[proc_macro_derive(Builder, attributes(builder))]
pub fn derive(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    match run(input) {
        Ok(token_stream) => token_stream,
        Err(e) => e.to_compile_error().into(),
    }
}

fn run(input: DeriveInput) -> Result<TokenStream, Error> {
    let struct_name = &input.ident;
    let builder_name = format_ident!("{}Builder", struct_name);

    let raw_fields = get_raw_fields(&input)?;

    let fields = raw_fields
        .iter()
        .map(FieldData::from_field)
        .collect::<Result<Vec<_>, Error>>()?;

    let builder_struct_fields = get_fields(&fields);
    let builder_init_fields = get_field_inits(&fields);
    let builder_setters = get_setters(&fields)?;
    let build_method_body = get_build_method_body(&fields, struct_name);

    let all = quote! {
        pub struct #builder_name {
            #(#builder_struct_fields),*
        }

        impl #struct_name {
            pub fn builder() -> #builder_name {
                #builder_name {
                    #(#builder_init_fields),*
                }
            }
        }

        impl #builder_name {
            #(#builder_setters)*

            pub fn build(&mut self) -> ::std::result::Result<#struct_name, ::std::boxed::Box<dyn ::std::error::Error>> {
                ::std::result::Result::Ok(#build_method_body)
            }
        }
    };

    Ok(all.into())
}

fn get_raw_fields(
    input: &DeriveInput,
) -> Result<&syn::punctuated::Punctuated<Field, syn::token::Comma>, Error> {
    if let Data::Struct(data) = &input.data {
        if let Fields::Named(fields) = &data.fields {
            Ok(&fields.named)
        } else {
            Err(Error::new(input.span(), "Expected named fields"))
        }
    } else {
        Err(Error::new(input.span(), "Expected a struct"))
    }
}

fn get_fields(fields: &[FieldData]) -> Vec<proc_macro2::TokenStream> {
    fields
        .iter()
        .map(|f| {
            let name = f.name;
            let typ = f.typ;
            if f.option_typ.is_some() || f.each.is_some() {
                quote! { #name: #typ }
            } else {
                quote! { #name: ::std::option::Option<#typ> }
            }
        })
        .collect()
}

fn get_field_inits(fields: &[FieldData]) -> Vec<proc_macro2::TokenStream> {
    fields
        .iter()
        .map(|f| {
            let name = f.name;
            if f.each.is_some() {
                quote! { #name: ::std::vec::Vec::new() }
            } else {
                quote! { #name: ::std::option::Option::None }
            }
        })
        .collect()
}

fn get_setters(fields: &[FieldData]) -> Result<Vec<proc_macro2::TokenStream>, Error> {
    let mut setters = Vec::new();

    for f in fields {
        let name = f.name;
        let typ = f.typ;

        if let Some(ref each_name) = f.each {
            let inner_typ = f.vec_typ.ok_or_else(|| {
                Error::new(typ.span(), "Field with `each` attribute must be a Vec")
            })?;

            setters.push(quote! {
                pub fn #each_name(&mut self, item: #inner_typ) -> &mut Self {
                    self.#name.push(item);
                    self
                }
            });
        } else {
            let setter_ty = f.option_typ.unwrap_or(typ);
            setters.push(quote! {
                pub fn #name(&mut self, #name: #setter_ty) -> &mut Self {
                    self.#name = ::std::option::Option::Some(#name);
                    self
                }
            });
        }
    }
    Ok(setters)
}

fn get_build_method_body(fields: &[FieldData], struct_name: &Ident) -> proc_macro2::TokenStream {
    let field_assignments = fields.iter().map(|f| {
        let name = f.name;
        if f.option_typ.is_some() || f.each.is_some() {
            quote! { #name: self.#name.clone() }
        } else {
            quote! {
                #name: self.#name.clone().ok_or_else(|| {
                    ::std::format!("Field {} is required", ::std::stringify!(#name))
                })?
            }
        }
    });

    quote! {
        #struct_name {
            #(#field_assignments),*
        }
    }
}

struct FieldData<'a> {
    name: &'a Ident,
    typ: &'a Type,
    option_typ: Option<&'a Type>,
    vec_typ: Option<&'a Type>,
    each: Option<Ident>,
}

impl<'a> FieldData<'a> {
    fn from_field(field: &'a Field) -> Result<Self, Error> {
        let name = field
            .ident
            .as_ref()
            .ok_or_else(|| Error::new(field.span(), "Unnamed field"))?;

        let each = Self::get_each(field)?;

        Ok(FieldData {
            name,
            typ: &field.ty,
            option_typ: Self::get_inner_type(&field.ty, "Option"),
            vec_typ: Self::get_inner_type(&field.ty, "Vec"),
            each,
        })
    }

    fn get_inner_type(ty: &'a Type, wrapper: &str) -> Option<&'a Type> {
        let tp = match ty {
            Type::Path(tp) => tp,
            _ => return None,
        };

        let segment = tp.path.segments.last()?;
        if segment.ident != wrapper {
            return None;
        }

        if let PathArguments::AngleBracketed(args) = &segment.arguments {
            if let Some(GenericArgument::Type(inner)) = args.args.first() {
                return Some(inner);
            }
        }
        None
    }

    fn get_each(field: &Field) -> Result<Option<Ident>, Error> {
        for attr in &field.attrs {
            if attr.path.is_ident("builder") {
                let meta = attr.parse_meta()?;
                if let Meta::List(list) = meta {
                    for nested in &list.nested {
                        if let NestedMeta::Meta(Meta::NameValue(name)) = nested {
                            if name.path.is_ident("each") {
                                if let Lit::Str(s) = &name.lit {
                                    return Ok(Some(Ident::new(&s.value(), s.span())));
                                }
                            } else {
                                return Err(Error::new_spanned(
                                    &list,
                                    "expected `builder(each = \"...\")`",
                                ));
                            }
                        } else {
                            return Err(Error::new_spanned(
                                &list,
                                "expected `builder(each = \"...\")`",
                            ));
                        }
                    }
                }
            }
        }
        Ok(None)
    }
}
