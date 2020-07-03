open Ppxlib;

// type_op
let toNewTypeName = typeName => {
  typeName ++ "_op";
};

let toOptional = (~loc, field: label_declaration) => {
  let {pld_type} = field;
  let new_pld_type = [%type: option([%t pld_type])];
  {...field, pld_type: new_pld_type};
};

let makeNewType = (~loc, t, fields) => {
  let {ptype_name} = t;
  let {txt: type_name} = ptype_name;
  let (module Builder) = Ast_builder.make(loc);
  Builder.(
    {
      type_declaration(
        ~name={...t.ptype_name, txt: toNewTypeName(type_name)},
        ~params=t.ptype_params,
        ~cstrs=t.ptype_cstrs,
        ~kind=Ptype_record(Base.List.map(~f=toOptional(~loc), fields)),
        ~private_=t.ptype_private,
        ~manifest=t.ptype_manifest,
      );
    }
  );
};

// to op
let toOptionalExpr =
    (field: label_declaration)
    : (Ppxlib__.Import.longident_loc, Ppxlib__.Import.expression) => {
  let (module Builder) = Ast_builder.make(field.pld_loc);

  let {txt: fieldName} = field.pld_name;
  let longident: Ppxlib__.Import.longident_loc = {
    txt: Lident(fieldName),
    loc: field.pld_loc,
  };

  let expression = {
    Builder.(
      pexp_construct(
        {txt: Lident("Some"), loc: field.pld_loc},
        Some(pexp_field([%expr t], longident)),
      )
    );
  };

  (longident, expression);
};

let makeToOpFunction = (~loc, typeName: string, fields) => {
  let (module Builder) = Ast_builder.make(loc);

  // function argument
  let fun_type_anno = Builder.ptyp_constr({txt: Lident(typeName), loc}, []);
  let fun_pattern = [%pat? (t: [%t fun_type_anno])];

  // function body
  let fun_expr =
    Builder.pexp_constraint(
      Builder.pexp_record(Base.List.map(~f=toOptionalExpr, fields), None),
      Builder.ptyp_constr({txt: Lident(toNewTypeName(typeName)), loc}, []),
    );

  let pfun = Builder.pexp_fun(Nolabel, None, fun_pattern, fun_expr);

  let functionName = Loc.make(~loc, typeName ++ "ToOp");
  let functionPattern = Builder.ppat_var(functionName);

  [%stri let [%p functionPattern] = [%e pfun]];
};

// excpetion
let makeFieldLostException = (~loc) => {
  [%stri exception FieldLost(string)];
};

// from op
let makeGetExn = (~loc): list(Ppxlib__.Import.value_binding) => {
  let (module Builder) = Ast_builder.make(loc);
  let fun_pattern = [%pat? v];
  let fun_expr =
    switch%expr (v) {
    | None => raise(FieldLost(""))
    | Some(v) => v
    };

  [Builder.value_binding(fun_pattern, fun_expr)];
};
let raiseFieldNotFound = (~loc, fieldName) => {
  let (module Builder) = Ast_builder.make(loc);
  let ident_raise = Builder.pexp_ident({txt: Lident("raise"), loc});
  let lident_fieldLost = {txt: Lident("FieldLost"), loc};
  Builder.pexp_apply(
    ident_raise,
    [
      (
        Nolabel,
        Builder.pexp_construct(
          lident_fieldLost,
          Some(Builder.estring("lost at field " ++ fieldName)),
        ),
      ),
    ],
  );
};
let fromOptionalExpr =
    (field: label_declaration)
    : (Ppxlib__.Import.longident_loc, Ppxlib__.Import.expression) => {
  let (module Builder) = Ast_builder.make(field.pld_loc);

  let {txt: fieldName} = field.pld_name;
  let longident: Ppxlib__.Import.longident_loc = {
    txt: Lident(fieldName),
    loc: field.pld_loc,
  };

  let expression =
    Builder.(
      pexp_match(
        pexp_field([%expr t_op], longident),
        [
          case(
            ~lhs=[%pat? None],
            ~guard=None,
            ~rhs=raiseFieldNotFound(~loc=field.pld_loc, fieldName),
          ),
          case(~lhs=[%pat? Some(v)], ~guard=None, ~rhs=[%expr v]),
        ],
      )
    );

  (longident, expression);
};
let makeFromOpFunction = (~loc, typeName: string, fields) => {
  let (module Builder) = Ast_builder.make(loc);

  // function argument
  let fun_type_anno =
    Builder.ptyp_constr({txt: Lident(toNewTypeName(typeName)), loc}, []);
  let fun_pattern = [%pat? (t_op: [%t fun_type_anno])];

  // function body
  let fun_expr =
    Builder.pexp_constraint(
      Builder.pexp_record(Base.List.map(~f=fromOptionalExpr, fields), None),
      Builder.ptyp_constr({txt: Lident(typeName), loc}, []),
    );

  let pfun = Builder.pexp_fun(Nolabel, None, fun_pattern, fun_expr);

  let functionName = Loc.make(~loc, typeName ++ "FromOp");
  let functionPattern = Builder.ppat_var(functionName);

  [%stri let [%p functionPattern] = [%e pfun]];
};

let recordHandler = (loc, rec_, t: type_declaration, fields) => {
  let (module Builder) = Ast_builder.make(loc);

  let {ptype_name} = t;
  let {txt: type_name} = ptype_name;

  let new_type = Builder.pstr_type(rec_, [makeNewType(~loc, t, fields)]);
  let toOp = makeToOpFunction(~loc, type_name, fields);
  let excep = makeFieldLostException(~loc);
  let fromOp = makeFromOpFunction(~loc, type_name, fields);

  [new_type, toOp, excep, fromOp];
};

let str_gen =
    (
      ~loc: Location.t,
      ~path,
      (rec_, expr): (
        Ppxlib__.Import.rec_flag,
        Ppxlib__.Import.list(Ppxlib__.Import.type_declaration),
      ),
    )
    : Ppxlib__.Import.structure => {
  let t = Base.List.hd_exn(expr); // we dont handle recursive type

  switch (t.ptype_kind) {
  | Ptype_record(fields) => recordHandler(loc, rec_, t, fields)
  | _ => Location.raise_errorf(~loc, "deriveFunction only works on records.")
  };
};

let name = "optionalRecord";

let () = {
  let str_type_decl = Deriving.Generator.make_noarg(str_gen);
  Deriving.add(name, ~str_type_decl) |> Deriving.ignore;
} /*12*/ /* [{pstr_desc */ /*     [{ptype_name = {txt = "a"}; ptype_params = []; ptype_cstrs = []*/ /*        Ptype_recor*/ /*           pld_type = {ptyp_desc = Ptyp_constr ({txt = Lident "string"}, [])}}*/ /*           pld_type = {ptyp_desc = Ptyp_constr ({txt = Lident "int"}, [])}}]*/ /*       ptype_private = Public; ptype_manifest = None}])}*/ /*          {pld_name = {txt = "a2"}; pld_mutable = Immutable*/ /*         [{pld_name = {txt = "a1"}; pld_mutable = Immutable*/ /*       ptype_kind */ /*    Pstr_type (Recursive*/;