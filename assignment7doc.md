# Programming Languages, Assignment 7

This file contains a description of the tasks you have to perform for assignment 7.

To better view the file, make sure your editor "warps" lines. In Sublime Text this is the "Word Wrap" option in the View menu.

Alternately, you can view this file in a nice form in GitHub.

As in previous assignments, you should NOT specify the types of your functions. Let the system determine them for you.

**IMPORTANT**: The vast majority of the functions you will implement in this module will be very short, 1-2 lines typically, usually requiring calls to library functions. You are NOT ALLOWED to make the functions recursive unless the question specifically tells you to. You should also strive to stay close to the suggested solution lengths.

This assignment is about learning to work with higher-order functions, both calling them but also creating some custom ones.

Before you attempt this assignment, you should familiarize yourself with the following functions. Most solutions involve a combination of these. Others involve a combination of previously defined functions. You will find the following functions in either the Pervasives module (google for "pervasives ocaml") or in the List module (google for "list ocaml"):

- `List.map`
- `List.fold_left`
- `List.fold_right`
- `List.rev`
- `@`
- `^`

You should not use other functions from the List module unless specifically instructed to do so.

## Helpers

In this section you will write some helper functions to use later in the assignment.

- You are provided with a function `range` that takes as input two integers and returns the list of integers from the first to the second. We implemented something very similar in class. It is here for use in the rest of the assignment.
- Write a function `range1` that takes as input a single integer as input and returns the list of integers from 1 to that integer. This should be extremely easy. Use `range`. It should have type `int -> int list`.
- Write a function `tabulate` that takes as input a function of type `int -> 'a` and also an integer `n` and uses it to generate the `'a list` consisting of the values of the provided function on the integers `1` through `n`. Reference solution is one line. Should have type: `(int -> 'a) -> int -> 'a list`

## Pictures

The main part of the assignment consists of working with the List module and higher order functions, by working with a simplistic version of "pictures". A "picture" consists of a list of "rows" of the same "length", a "row" being a list of "pixels". A "pixel" is either a dot (empty) or a "hash" (filled). We will represent a pixel via an enumeration type. These types are provided in the file. The "length" of a row is the number of pixels in it.

- A sample "pic" is provided, bound to the variable `sword`, for you to use in testing. It should show up somewhat like a sword (formed by the "H"s). You can visualize the sword by doing `print_string (string_of_pic sword)` once you have implemented `string_of_pic` below.
- Create your own "picture", named `doodad`, in a manner similar to "sword" above. The picture can be anything you like, but it must satisfy the following conditions which will be checked by the grading tests:
    - It must be bound to the variable `doodad`.
    - It must be of type "pic".
    - It must be "rectangular", in other words all rows must have the same number of elements.
    - The two "dimensions" (number of rows, number of columns) do not have to be equal but they have to each be at least 6. (no 4x2 doodads)

    Should have type: `pic`
- You are provided with a function `valid_pic` that is given a picture and returns a boolean indicating whether the picture is "valid", in the sense of having all rows of equal length. Every function that follows assumes that it is provided a valid picture. Study this function, and the next.
- You are provided a function `dims_pic` that is given a (valid) picture and returns a pair of the number of rows and number of columns of the picture.
- Write a function `string_of_pxl` that takes as input a pixel and it returns a string, a single dot if the pixel is a D, the hash/pound sign # if the pixel is H. This is a very simple function. Should have type: `pixel -> string`
- Write a function `string_of_row` that takes as input a row and returns a string consisting of the concatenation of strings corresponding to the pixels in the row.  The resulting string should include a newline `"\n"` at the end. Reference solution is 1 line. Should have type: `row -> string`
- Write a function `string_of_pic` that takes as input a picture and returns a string consisting of the concatenation of the strings produced by each row. The rows already contain newline characters. You do not need to add any extra newlines. Reference solution is 1 line. Should have type: `pic -> string`
- Write a function `flip_vertical` that takes as input a picture and returns a picture that is a vertical flip of the original one. Reference solution is 1 line.Should have type: `pic -> pic`
- Write a function `flip_horizontal` that takes as input a picture and returns a picture that is a horizontal flip of the original one. Reference solution is 1 line. Should have type: `pic -> pic`
- Write a function `flip_both` that takes as input a picture and returns a picture that is a "double flip" of the original one, doing both a horizontal and a vertical flip. The order of flips would not matter. Reference solution is 1 line. Should have type: `pic -> pic`
- Write a function `mirror_vertical` that takes as input a picture and returns a picture that contains two copies of the original picture, one below the other but where the second copy is a mirror image of the first one, as produced by `flip_vertical`. You do not have to use `flip_vertical` along the way. It will have twice the number of rows as the original one. Reference solution is 1 line. Should have type: `pic -> pic`
- Write a function `mirror_horizontal` that takes as input a picture and returns a picture that contains two copies of the original picture, one next the other horizontally, but where the second copy is a mirror image of the first one as produced for example by `flip_horizontal` (but you will likely not be able to call `flip_horizontal`). It will have twice as long rows as the original one. Reference solution is 1 line. Should have type: `pic -> pic`
- Write a function `mirror_both` that takes as input a picture and returns a picture that contains four copies of the original picture, arranged in a 2x2 format, and each obtained from the nearby one via a flip in that direction. When applied to the sword picture, it should produce four swords emanating from its center. Reference solution is 1 line. Should have type: `pic -> pic`
- Write a function `pixelate` that takes as input a function `f` of type `int -> int -> pixel` and two integers `m` and `n`, in that order, and produces a picture of `m` rows and `n` "columns". The pixel at the i-th row and j-th column (in other words the j-th pixel in the i-th row) is determined by the value of the function `f`. Use `tabulate` from earlier (in two places). Reference solution is 1-2 lines. Should have type: `(int -> int -> pixel) -> int -> int -> pic`
- Write a function `stack_vertical` that takes as input two pictures and places them one atop the other in a vertical fashion. It should raise the exception `IncompatibleDims` if the pictures have different number of columns. Reference solution is 2 lines. Should have type: `pic -> pic -> pic`.
- Write a function `stack_horizontal` that takes as input two pictures and places them next to each other in a horizontal fashion. It should raise the exception `IncompatibleDims` if the pictures have different number of rows. You should use `List.fold_right2`. You can handle the dimension check by either using the function `dims` provided earlier or letting `List.fold_right2` throw its exception (see documentation) and catching it. Or try them both! Reference solution is 2-4 lines. Should have type: `pic -> pic -> pic`.
- Write a function `invert` that takes as input a picture and returns the same picture with the two "colors" inverted. Reference solution is 2-4 lines. Should have type: `pic -> pic`
- Write a function `transpose` that takes as input a picture and returns the result of "transposing" the picture, i.e. turning its rows into columns. This one is short but tricky. The reference solution is 4 lines and uses `List.fold_right`, `List.map` and `List.map2` along with a let binding and a conditional. Start by working out manually in a small example how a recursive implementation might function (but your final solution is not meant to be recursive; this would just help you figure out the kind of work that your `fold_right` would have to do). Should have type: `pic -> pic`
