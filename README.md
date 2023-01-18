# Crafting interpreters

From https://craftinginterpreters.com/:

> Crafting Interpreters contains everything you need to implement a full-featured, efficient scripting language. You’ll learn both high-level concepts around parsing and semantics and gritty details like bytecode representation and garbage collection. Your brain will light up with new ideas, and your hands will get dirty and calloused. It’s a blast.
>
> Starting from main(), you build a language that features rich syntax, dynamic typing, garbage collection, lexical scope, first-class functions, closures, classes, and inheritance. All packed into a few thousand lines of clean, fast code that you thoroughly understand because you write each one yourself.

This book teaches you how to build two programs:
 - `jlox`, a Lox interpreter written in Java, which executes code while walking the AST
 - `clox`, a Lox interpreter written in C, which implements a VM with a garbage collector

Since I didn't want to just do a copy-paste of the code, and since I'm not really
interested in the mentioned languages, I've decided to substitute these programs
with the following:
 - `scalox`, a Lox interpreter written in Scala, replacing `jlox`
 - `rslox`, a Lox interpreter written in Rust, replacing `clox`

You can read the book online [here](https://craftinginterpreters.com/contents.html).
