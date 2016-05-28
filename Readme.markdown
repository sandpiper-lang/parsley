
Parsley
========

This is, for the moment, just a playground to get a Sandpiper parser off the ground. We will call the parser microframework “Parsley.” It will likely not be released as a standalone framework, but should be suitable for adapting into many other kinds of Swift-based projects.


The Parsley DSL
----------------

The Sandpiper language grammar is specified for both human and machine consumption, simultaneously. Rather than using an established metagrammar like EBNF, Parsley defines its own grammar as a domain-specific language in Swift. As Sandpiper matures, we will change the DSL to be specified in Sandpiper itself.

A percent-prefixed string `%""` specifies a wildcard matcher that matches a run of one or more characters in the input, so long as they appear within the string. For instance, `%"0123456789abcdef"` would match any hexadecimal number such as `baddb33f`. The wildcard matcher can be inverted—to match any input *except* the specified characters—by prefixing with bang:

    let non_whitespace = !%"\n\r\t "
    // or
    let whitespace = %"\n\r\t "
    let non_whitespace = !whitespace

You can combine (union) two sets of matchable characters using the bitwise-or operator `|`. You can intersect them using bitwise-and `&`.

A literal string itself is overloaded to match itself verbatim in the input.

Matchers can be made optional by suffixing them with a tilde `~`. One thing you see a lot in the Sandpiper grammar is `ws`, which matches a run of whitespace characters (including comments). Sometimes whitespace is not strictly necessary, so you also often see it written as `ws~`. When a matcher is made optional, it may parse and return a result as the `Nil` AST kind (more on that later).

Matchers can be chained in-order, using the addition operator `+`. Here we look for an expression surrounded by parentheses, and optional whitespace on either side:

    let sub_expression = "(" + ws~ + expression + ws~ + ")"

Matchers can be combined into a “hopper,” which matches the first sub-grammar that can be successfully parsed. In the DSL, we construct these with the logical-or operator:

    let call_expression = function_application || receiver_call

will try to parse the input first as a function application, and if that fails, attempt to parse it as an objective call with a receiver.

And you can define a run of one or more matches from a parser by suffixing it with a faux-ellipsis `...`:

    let expressions = expression + (ws + expression)...

will match two or more expressions separated by whitespace. We can match *one* or more separated by whitespace by making the repeater clause optional:

    let expressions = expression + ((ws + expression)... )~

Once you have a parser, you actually parse some text by calling `pars()` on it:

    let my_grammar = ...
    let ast = try my_grammar.pars(input_text, parentOffset: 0)

If it doesn't throw a `ParsError.NoMatch` or `ParsError.EmptyInput`, it will return a tree of `ParsleyAST`s. Each `ParsleyAST` node has a `kind`, which represents the meaningful syntactic element the parsed node represents. This is where the parser becomes specialized to Sandpiper—we have very specific AST kinds for the different types of expressions, so that they can be meaningfully processed later. We tag a matcher with an `ASTKind` by putting ` -- .WhateverKind` after it. For instance,

    let ws = (literal_whitespace || commentary)...  -- .Whitespace

We use this to tag existing matchers with different meanings—for instance,

    let symbolic_reference_expression = bare_word   -- .Expression(.SymbolicReference)

