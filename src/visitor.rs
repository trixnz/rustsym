use {Match, MatchKind};

use syntax::ast;
use syntax::codemap::{CodeMap, Span};
use syntax::visit::{self, FnKind, Visitor};

pub struct SymbolVisitor<'a> {
    pub matches: Vec<Match>,
    pub codemap: &'a CodeMap,
    pub search_children: bool,
    pub query: String,
}

impl<'a> SymbolVisitor<'a> {
    fn create_match(&mut self, name: &str, container: Option<&str>, kind: MatchKind, span: Span) {
        // Check if all of the characters in the query are present in the symbol
        if self.query.chars().any(|c| !name.to_lowercase().contains(c)) {
            return;
        }

        let filename = self.codemap.span_to_filename(span);
        let line = self.codemap.lookup_char_pos(span.lo).line;

        self.matches.push(Match {
            path: filename,
            name: name.into(),
            container: container.unwrap_or_default().into(),
            kind: kind,
            line: line,
        });
    }
}

fn get_ident_name(ident: &ast::Ident) -> String {
    ident.name.as_str().to_string()
}

impl<'a> Visitor<'a> for SymbolVisitor<'a> {
    // Catch free standing functions
    fn visit_fn(&mut self,
                fn_kind: FnKind<'a>,
                fn_decl: &'a ast::FnDecl,
                span: Span,
                _: ast::NodeId) {
        let fn_name = match fn_kind {
            FnKind::ItemFn(id, _, _, _, _, _, _) => Some(get_ident_name(&id)),
            _ => None,
        };

        if let Some(name) = fn_name {
            self.create_match(&name, None, MatchKind::Function, span);
        }

        visit::walk_fn(self, fn_kind, fn_decl, span);
    }

    // Catch pretty much everything else
    fn visit_item(&mut self, item: &'a ast::Item) {
        use syntax::ast::ItemKind;
        use syntax::ast::VariantData;
        use syntax::ast::TraitItemKind;
        use syntax::ast::ImplItemKind;

        let item_name = get_ident_name(&item.ident);

        match item.node {
            ItemKind::Mod(ref mod_) => {

                if !self.search_children {
                    if self.codemap.span_to_filename(item.span) != self.codemap.span_to_filename(mod_.inner) {
                        // Don't visit submodules that are not inline.
                        return;
                    }
                }

                self.create_match(&item_name, None, MatchKind::Module, item.span);
            }

            ItemKind::Struct(ref variant, _) => {
                self.create_match(&item_name, None, MatchKind::Struct, item.span);

                match *variant {
                    VariantData::Struct(ref fields, _) |
                    VariantData::Tuple(ref fields, _) => {
                        for field in fields {
                            if let Some(field_ident) = field.ident {
                                let field_name = get_ident_name(&field_ident);

                                self.create_match(&field_name,
                                                  Some(&item_name),
                                                  MatchKind::Field,
                                                  field.span);
                            }
                        }
                    }
                    _ => {}
                }
            }

            ItemKind::Trait(_, _, _, ref items) => {
                self.create_match(&item_name, None, MatchKind::Trait, item.span);

                for v in items {
                    let trait_item_name = get_ident_name(&v.ident);

                    let kind = match v.node {
                        TraitItemKind::Method(_, _) => Some(MatchKind::Method),
                        TraitItemKind::Const(_, _) => Some(MatchKind::Constant),
                        _ => None,
                    };

                    if let Some(kind) = kind {
                        self.create_match(&trait_item_name, Some(&item_name), kind, v.span);
                    }
                }
            }

            ItemKind::Enum(ref def, _) => {
                self.create_match(&item_name, None, MatchKind::Enum, item.span);

                for v in &def.variants {
                    let variant_name = get_ident_name(&v.node.name);

                    self.create_match(&variant_name, Some(&item_name), MatchKind::Constant, v.span);
                }
            }

            ItemKind::Const(_, _) => {
                self.create_match(&item_name, None, MatchKind::Constant, item.span);
            }

            ItemKind::Static(_, _, _) => {
                self.create_match(&item_name, None, MatchKind::Static, item.span);
            }

            ItemKind::Impl(_, _, _, _, _, ref ty, ref items) => {
                let mut struct_name = String::new();

                // Figure out the struct name on the right hand side of the `impl` expression
                if let ast::TyKind::Path(_, ref p) = ty.node {
                    struct_name = get_ident_name(&p.segments[0].identifier);
                }

                // Now populate the methods
                for item in items {
                    if let ImplItemKind::Method(_, _) = item.node {
                        self.create_match(&get_ident_name(&item.ident),
                                          Some(&struct_name),
                                          MatchKind::Method,
                                          item.span)
                    }
                }
            }

            ItemKind::MacroDef(_) => {
                self.create_match(&item_name, None, MatchKind::Macro, item.span);
            }

            _ => {}
        };

        visit::walk_item(self, item);
    }

    fn visit_mac(&mut self, _mac: &ast::Mac) {}
}
