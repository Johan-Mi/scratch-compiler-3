(comment) @comment.line

(string_literal) @string
(number_literal) @constant.numeric

[
  "false"
  "true"
] @constant.builtin.boolean

[
  "costumes"
  "inline"
] @keyword
[
  "if"
  "else"
] @keyword.control.conditional
[
  "while"
  "until"
  "repeat"
  "forever"
  "for"
] @keyword.control.repeat
"return" @keyword.control.return
"fn" @keyword.function
[
  "struct"
  "sprite"
  "let"
] @keyword.storage.type
"as" @keyword.operator

[
  "="
  "&"
  "+"
  "-"
  "*"
  "/"
  "%"
  "<"
  "=="
  ">"
] @operator
[
  ":"
  ","
] @punctuation.delimiter
[
  "("
  ")"
  "["
  "]"
  "{"
  "}"
] @punctuation.bracket

(identifier) @variable

(function_definition
  (identifier) @function)
(function_call
  (identifier) @function)

(struct
  (identifier) @constructor)

(parameter
  . (identifier) @label)

(named_argument
  . (identifier) @label)

(parameter (identifier) @variable.parameter . ":")

(binary_expression
  "." . (identifier) @variable.other.member)

(
  (identifier) @comment.unused
  (#match? @comment.unused "_")
)

(type_expression
  (identifier) @type)

(type_expression
  (identifier) @type.builtin
  (#match? @type.builtin "^(Unit|Num|String|Bool)$"))

(type_expression
  "%" (identifier) @type.parameter)
