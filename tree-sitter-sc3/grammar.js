module.exports = grammar({
  name: "sc3",

  extras: $ => [/[ \t\n]+/, $.comment],

  word: $ => $.identifier,

  rules: {
    source_file: $ =>
      repeat(
        choice(
          $.sprite,
          $.struct,
          $.function_definition,
          $.variable_definition,
        ),
      ),

    sprite: $ => seq("sprite", $.identifier, $.sprite_body),

    sprite_body: $ =>
      seq(
        "{",
        repeat(
          choice($.function_definition, $.variable_definition, $.costumes),
        ),
        "}",
      ),

    costumes: $ => seq("costumes", "{", repeat($.costume), "}"),

    costume: $ => seq($.string_literal, ":", $.string_literal, optional(",")),

    struct: $ =>
      seq("struct", optional($.identifier), optional($.function_parameters)),

    function_definition: $ =>
      seq(
        "fn",
        optional("inline"),
        choice($.identifier, "+", "-", "*", "/", "%", "<", "==", ">", "="),
        optional(choice($.function_parameters, $.string_literal)),
        optional($.type_expression),
        optional($.block),
      ),

    function_parameters: $ => seq("(", repeat($.parameter), ")"),

    parameter: $ =>
      seq(
        $.identifier,
        optional($.identifier),
        ":",
        $.type_expression,
        optional(","),
      ),

    block: $ => seq("{", repeat($._statement), "}"),

    _statement: $ =>
      choice(
        $.variable_definition,
        $._expression,
        $.if_statement,
        $.while_loop,
        $.until_loop,
        $.forever_loop,
        $.for_loop,
        $.return_statement,
      ),

    variable_definition: $ =>
      seq("let", field("variable", optional($.identifier)), "=", $._expression),

    if_statement: $ =>
      seq(
        "if",
        $._expression,
        $.block,
        optional(seq("else", choice($.if_statement, $.block))),
      ),

    while_loop: $ => seq("while", $._expression, $.block),
    until_loop: $ => seq("until", $._expression, $.block),
    forever_loop: $ => seq("forever", $.block),
    for_loop: $ => seq("for", $.identifier, $._expression, $.block),

    return_statement: $ => seq("return", $._expression),

    type_expression: $ =>
      seq(repeat(choice("*", "%", seq("[", "]"))), $.identifier),

    _expression: $ =>
      choice(
        $.string_literal,
        $.number_literal,
        $.list_literal,
        "false",
        "true",
        $.function_call,
        $.reference,
        $.parenthesized_expression,
        $.named_argument,
        $.binary_expression,
        $.index,
        $.identifier,
      ),

    list_literal: $ => seq("[", repeat(seq($._expression, optional(","))), "]"),

    function_call: $ => seq($.identifier, $.arguments),

    arguments: $ =>
      seq(token.immediate("("), repeat(seq($._expression, optional(","))), ")"),

    reference: $ => prec(5, seq("&", $._expression)),

    parenthesized_expression: $ => seq("(", $._expression, ")"),

    named_argument: $ =>
      seq(alias($.identifier, $.label), token.immediate(":"), $._expression),

    binary_expression: $ =>
      choice(
        prec.left(1, seq($._expression, "=", $._expression)),
        prec.left(2, seq($._expression, choice("<", "==", ">"), $._expression)),
        prec.left(3, seq($._expression, choice("+", "-"), $._expression)),
        prec.left(4, seq($._expression, choice("*", "/", "%"), $._expression)),
        prec.left(6, seq($._expression, "as", $.type_expression)),
        prec.left(6, seq($._expression, ".", $._expression)),
      ),

    index: $ =>
      prec(6, seq($._expression, token.immediate("["), $._expression, "]")),

    identifier: $ => /[\p{XID_Start}_][\p{XID_Continue}-]*/,

    number_literal: $ =>
      choice(
        /[+-]?0[bB][01]+/,
        /[+-]?0[oO][0-7]+/,
        /[+-]?0[xX][0-9a-fA-F]+/,
        /[+-]?[0-9]+(\.[0-9]+)?([eE][+-]?[0-9]+)?/,
      ),

    string_literal: $ => /"([^"\n\\]|\\[^\n])*[\\"]?/,

    comment: $ => /#.*/,
  },
});
