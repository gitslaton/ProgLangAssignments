# Programming Languages, Assignment 8

Building an Interpreter

This file contains a description of the tasks you have to perform for assignment 8.

To better view the file, make sure your editor "warps" lines. In Sublime Text this is the "Word Wrap" option in the View menu.

Alternately, you can view this file in a nice form in GitHub.

As in previous assignments, you should NOT specify the types of your functions. Let the system determine them for you.

**IMPORTANT**: Unlike the prior assignments, this assignment requires you to edit multiple files, contained in the `interpreter` folder. You will need to make additions to the following files:

- `types.ml`
- `types.mli`
- `parser.mly`
- `lexer.mll`
- `tests.ml`

The steps of this assignment are broken up depending on the concept that they add to the language. Each will require you to make additions to one or more of the files above.

The automatic tests are only meant to test that the desugar and interp methods are working properly. For the changes in the lexer and parser, you should interact with the `./lang` interpreter directly.

It will be important in the following to clarify which exceptions you should be raised when. Anything that violates the semantics of the language should be raising an appropriate exception, typically `Interp ...`. For instance if the description that follows says about "addition" that the two summands must be numbers, then your code will need to verify that before trying to perform the addition, and raise an appropriate exception.

You will probably want to create different milestones for each of these types of language extensions.

## Booleans

The first thing we will do is add booleans. We will add new values, boolean primitive, conditionals, as well as logical operators desugared into conditionals.

It is important to distinguish between talking about the core/surface language's booleans, that we are trying to define, and the implementation/OCAML language's booleans.

### Boolean primitives

- Add a new `Bool` variant in the `value` type in the `types.ml` file and the `types.mli` file. It should hold a OCAML boolean. Update the `valToString` function to handle this new kind of value. The `valToString` function is not used in any of the grading tests, you can choose to "print" booleans any way you like.
- Add a new `BoolC` variant in the `exprC` type in the `types.ml` file and the `types.mli` file. It should hold a OCAML boolean.
- Add a case in the `interp` match to correctly "interpret/evaluate" a `BoolC` to the appropriate `value`.
- Add some tests in `tests.ml` for the correct interpretation of `BoolC`.
- Add a new `BoolS` variant in the `exprS` type in the `types.ml` file and the `types.mli` file. It should hold a OCAML boolean.
- Add a new case in the `desugar` match to correctly desugar a `BoolS` expression to the appropriate `exprC`.
- Add tests for the appropriate desugaring.
- Add two new tokens `TRUE` and `FALSE` at the top of `parser.mly` (where the `FLOAT` token is defined). They should not "contain" anything (so no need for `<float>`). You can put them both in one line, like so: `%token TRUE FALSE`.

### Parser modifications for interpreter

The following add the appropriate parser/lexer instructions for processing booleans.

- Add two new cases to the `expr` set of rules near the bottom of `parser.mly`, one for `TRUE` and one for `FALSE`. The corresponding expressions in braces should simply create the appropriate surface language expressions (involving `BoolS`).
- In `lexer.mll`, add the two regular expression definitions `let true = "true" | "#t"` (and an analogous one for false). This would allow us to type `true` or `#t` and have it be recognized as a boolean in the language.
- Add cases to the `rule` section in `lexer.mll` that turns the `true` match to the `TRUE` token and similarly for `false`. The order is important, don't put the new cases too far down. The last two rules are catch-alls, and should remain at the end of the options.

To test this part, you should follow the compile steps in the README file. Then start the interpreter via `./lang`, and you should be able to type `true;;` or `#t;;` (and similarly for false), and have the interpreter reply appropriately.

### Conditionals

We will now add conditionals. This does not require a new value, but it does require changes to the core and surface language, and the parser and lexer.

- Add a new variant in the `exprC` type (in both `types.ml` and `types.mli`) for an `IfC` that takes 3 `exprC`s as arguments (a triple). The first of those is meant to be the conditional test, the second is the `then` branch, the third is the `else` branch.
- Add a case in `interp` for properly evaluating a conditional: The test expression should be evaluated first (recursively calling `interp`), and it should be an error (raise `Interp`) if the result is not a `Bool` value. If it is a `Bool` value, then depending on the value either the "then" branch or the "else" branch should be evaluated. Only one of these branches should be evaluated, and only after the conditional test has first been evaluated. It is *imperative* that you ensure the appropriate evaluation semantics for the conditional. Make sure to parenthesize any inner match that you use.
- Add tests for the correct behavior of the conditional. For example the expression `IfC (BoolC true, NumC 2.3, NumC 4.3)` should evaluate to `Num 2.3`. Make sure to allow the possibility where the "test" is not a `BoolC` directly, but something that evaluates to a boolean; for instance it could be another `IfC` expression.
- Add a corresponding expression `IfS` at the surface language, that takes as arguments three `exprS` expressions, and it desugars into an appropriate `IfC` expression.
- Add a case in the `desugar` match to handle `IfS` and convert it to `IfC`.

We will now add "or", "and" and "not" to the surface language. They will all convert to `IfC`s.

- Add an `OrS`, a `AndS` and a `NotS` to the surface language, the first two taking two expressions as arguments, the third taking only one expression.
- Add `desugar` cases for each of them based on the following pseudo-codes: "`not e` is the same as `if e then false else true`", "`e1 or e2` is the same as `if e1 then true else if e2 then true else false`", and something analogous for `and`. Note that we did not use `if e1 then true else e2` for the "or" case, because we want to enforce the behavior that both expressions are booleans. The conditional checks that already for its test, but not for its branch values.
- Add tests to verify the correct implementation of these constructs.

Now we will add the correct components in the parser and lexer.

- Add three new tokens in `parser.mly`, called `IF`, `THEN` and `ELSE`, all in one line.
- Add `%nonassoc ELSE` below the `%nonassoc FLOAT` line.
- Add a case in the `expr` rule in `parser.mly` that expects a `IF expr THEN expr ELSE expr` and turns it into an `IfC`. You will need to use `$2`, `$4` and so on to refer to the values of those `expr`s.
- Add three cases in the rules in `lexer.mll` for the strings `"if"`, `"then"` and `"else"` and converting them to the corresponding tokens `IF`, `THEN`, `ELSE`. No need to use `let` assignments for these single cases.

Compiling after these should allow you to write if-then-else cases. We will now add "or" and "else".

- Add three new tokens in `parser.mly`, called `OR`, `AND` and `NOT`.
- We need to specify precedence rules. This would be done by adding `%left OR AND` below the `%nonassoc ELSE` line. This says that both "or" and "and" should compute in a left-first direction, and they should be treated as having equal precedence to each other and stronger precedence to the `ELSE`. This way, an expression like `if true then false else false or true` would be interpreted as `if true then false else (false or true)` instead of `(if true then false else false) or true`.
- Add in the `expr` rule at the bottom of `parser.mly` cases for `expr OR expr` and `expr AND expr`.
- Add a precedence rule `%nonassoc NOT` below the one for OR and AND.
- Add an `expr` rule at the bottom of `parser.mly` for `NOT expr`.
- Add in `lexer.mll` three rules for `"or"`, `"and"` and `"not"`.

### Arithmetic Operators

We will now add arithmetic operators, followed by comparison operators and finally, equality.

- Add a new `exprC` type variant `ArithC` that takes a "string" for the operator symbol, and two `exprC`s for the two operands.
- Implement a new "helper method" called `arithEval` with type `string -> value -> value -> value` that takes the operator and two values. If the two values are not both `Num`s then it should raise an interpreter exception. If they are then it should perform the appropriate operation and return a "value" of the result. The operators we will allow will be "+", "-", "*" and "/". It should throw an interpreter error if the operator symbol is not one of these. It should also throw an interpreter error about division by zero if the operator is division and the denominator is 0. Remember that in our arithmetic world, all numbers are floating point numbers.
- Add a new case in `interp` to handle `ArithC` cases by evaluating the two operands in the correct order, then calling the `arithEval` helper method.
- Add tests to ensure these cases are handled properly.
- Add a new `exprS` type variant `ArithS` that models `ArithC`.
- Add a new case in the desugarer to convert `ArithS` to `ArithC`.
- Add tests for the desugaring.

Now we add parser/lexer operations corresponding to these.

- Add in `parser.mly` four new tokens, `PLUS`, `MINUS`, `TIMES`, `DIVIDE`.
- We next need to set up precedence rules. Place at the bottom of the current list, below the `%nonassoc NOT`, a line `%left PLUS MINUS`, and below that line add `%left TIMES DIVIDE`. This will ensure that all operations are performed in a left-associative way, and that multiplication and division will have higher precedence than addition and subtraction. This will agree with normal algebra rules.
- Add `expr` cases at the bottom of `parser.mly` for `expr PLUS expr` and similarly for the other operations. They should all convert to the appropriate `ArithS` expression.
- Add in `lexer.mll` near the bottom rules for `"+"`, `"-"` and so on, to be converted to the corresponding tokens `PLUS`, `MINUS` and so on. Remember that the last two rules there about `eof` and `any` are catchalls and need to be the last cases.

You should now be able to compile and try various arithmetic operations. Also test something like `if true then 4 else 2 + 3`, and make sure that the addition binds stronger than the conditionals. Also that `2 + 3 * 4` behaves properly.

### Comparison Operators

The comparison operators we will consider are ">", ">=", "<", "<=". We leave equality and non-equality as a separate case, as they make sense for more than just numbers.

- Add a new `exprC` type variant `CompC` that takes a string and two `exprC`s.
- Create a helper method `compEval` with type `string -> value -> value -> value` that takes as input a string and two values. If the two values are not both `Num`s then it should raise an interpreter error. If they are, then it should perform the appropriate operator and return a `Bool` value. It should be an interpreter error if the operator is not one of the four mentioned above.
- Add a new case in `interp` to handle the `CompC` construct, by evaluating the two expressions in the correct order then passing the work over to `compEval`.
- Add tests for the comparison semantics.
- Add a new `exprS` type variant `CompS` analogous to `CompC`, and the corresponding clause in `desugar` to convert it into `CompC`.
- Add tests for the desugaring.

Now to adjust the parser/lexer:

- Add a new token called `COMPOP` that contains a string in it. Model it after the `FLOAT` token.
- Place `%nonassoc COMPOP` between the `%nonassoc NOT` and `%left PLUS MINUS` lines. This should make arithmetic happen first, but comparison right after that, before logical operators are taken into account. So something like `3 < 4 and 1 < 2` would behave properly.
- Add `| expr COMPOP expr    { CompS ($2, $1, $3) }` as a case in the `expr` rule in `parser.mly`.
- In `lexer.mll`, add a new binding `let comp = ">" | ">=" | "<" | "<="`, then further down a new rule `| comp as s   { COMPOP s }`. This recognizes the above four operators and stores the match as part of the `COMPOP` token.

After you recompile everything, you should be ready to test the interpreter directly.

### Equality

Equality is surprisingly a more difficult concept than most. Programmers expect to be able to compare for equality any two expressions. This means that you must handle all value possibilities, both `Num` and `Bool` as well as other values we add in the future. Every time we add a new kind of value (pairs, lists etc) we will need to update our equality function to account for them. We will use `==` to denote equality.

Testing for non-equality, often denoted by `!=`, will be implemented simply as desugaring via `NotS` and equality. So in the core language we will need to only worry about equality.

- Add a new `exprC` type variant `EqC` that takes as input two expressions.
- Write a new helper function `eqEval` of type `value -> value -> value` that takes as input two values and returns if those values are equal as a `Bool` value. It should do so as follows: If they are both `Num`s, it should compare the corresponding floats stored there. If they are both `Bool`s it should compare the corresponding booleans stored there. Otherwise it should return `Bool false`. You will need to update this function in the future when we create new kinds of values.
- Add a new case in `interp` to handle the `EqC` by evaluating each operand in turn then calling `eqEval` to compare them.
- Add two `exprC` variants: a `EqS` to mirror `EqC` and a `NeqS` and adjust `desugar` accordingly. For `NeqS` you should convert it into a combination of `NotS` and `EqS`, then desugar that.

Parser/Lexer changes:

- Add two new tokens `EQ` and `NEQ` to `parser.mly`.
- For precedence, add `%nonassoc EQ NEQ` between `%left OR AND` and `%nonassoc NOT`. Make sure you understand the consequences of this choice.
- Add two more rules for `expr` in `parser.mly` to handle `expr EQ expr` and `expr NEQ expr`.
- Add to the rules at the bottom of `lexer.mll` cases for turning `"=="` to `EQ` and `"!="` to `NEQ`.

You should be ready to use equality tests now.
