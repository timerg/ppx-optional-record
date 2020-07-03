open Jest;
open ExpectJs;

[@deriving optionalRecord]
type my_type = {
  foo: int,
  bar: option(string),
};

let my_type: my_type = {foo: 3, bar: Some("my_type")};

let _ =
  describe("test", () => {
    test("my_typeToOp", () => {
      let my_type_op = my_typeToOp(my_type);
      my_type_op
      |> expect
      |> toEqual({foo: Some(3), bar: Some(Some("my_type"))});
    });
    test("my_typeFromOp and my_typeToOp are inversed", () => {
      let my_type' = my_type->my_typeToOp->my_typeFromOp;

      my_type' |> expect |> toEqual(my_type);
    });
    test("my_typeFromOp raise FieldLost if field is None", () => {
      let my_type_op = {foo: None, bar: None};

      (() => my_typeFromOp(my_type_op))
      |> expect
      |> toThrowException(FieldLost("lost at field foo"));
    });
  });