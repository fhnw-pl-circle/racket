# Racket

## Introduction

Racket is a LISP-style programming language that features a strong macro system, (re)definable syntax, functional and imperative constructs, dynamic, gradual and static typing - basically, if you can imagine it, you can make Racket do it.

This project attempts to give an overview of Racket and its possibilities in 3 stages:

- A general Racket overview
- An intro to Macros
- Typed Racket

## Installation

- [Official Racket Download Page](https://download.racket-lang.org/) and use the installers.
- Chocolatey: `choco install racket`
- Linux: `apt install racket`

### Updating

If you update, you have to do two things:

- `raco pkg migrate 8.8`, replacing 8.8 with the old version
- `raco setup` as Admin!

## Part 1: How does Racket work?

A simple REPL interaction might look like this:

```bash
> racket
Welcome to Racket v8.11.1 [cs].
> ,rr "01-main/main.rkt"
; requiring "01-main/main.rkt"
23
> (+ 20 30)
50
> ,exit
```

To execute a file directly, use `raco exe file.rkt` or `racket file.rkt`.

### Data types and Syntax

```racket
#lang racket ; a file starts with a #lang directive

; Comments are prefixed with ;
20
1.2
1/3

"Strings"
'Symbols

#t #f    ; true and false
```

## Part 2: Typed Racket

![Numeric Types](https://docs.racket-lang.org/ts-reference/pict.png)
![Integer Types](https://docs.racket-lang.org/ts-reference/pict_2.png)

Typed Racket is a compatible dialect of Racket that adds explicit type annotations.

It has a quite expansive type system, even venturing into dependent and refinement typing
with newer versions, yet allows fine-grained control over the runtime impact of its typing
via Deep, Shallow and Optional dialects.

## Part 3: Macros, my ~~beloved~~ bane

### Phases

Phases are Racket's way of delineating expansions the code makes when it is evaluated.
This is comparable to a C/C++'s compiler's "Preprocessing" and "Compiling" phase, but
way more sophisticated.

All communication between phases is only in one direction: Forward.

```racket
(define x 10)
```

This binding of value `x` only exists at one phase: Phase level 0, the phase used for
"runtime" definitions. This means, were we to try use this within a macro, `x` would
not be resolved.

```racket
(begin-for-syntax
  (define x 10))
```

Instead, `begin-for-syntax` lifts this binding to Phase level 1. Both definitions can
coexist, in fact. This even has a shortcut in `define-for-syntax`.

This phase distinction is too simplistic though: 0 and 1 are by far not the only phase
levels that exist. In fact, it's all relative! And by that I mean that the "current phase
level" is a concept that is local to a module, and you can modify the phase level you import
any other module at.

```racket
(require "a.rkt")                ; import with no phase shift
(require (for-syntax "a.rkt"))   ; shift phase by +1
(require (for-template "a.rkt")) ; shift phase by -1
(require (for-meta 5 "a.rkt"))   ; shift phase by +5
```

This means there are potentially unbounded amounts of phase levels. Proceed with caution.

Why all this, though? It's simple: Data is code, code is data. The syntax with which you write
macros is near-identical to how your normal code looks like. No separate, less powerful syntax (C/C++),
no special magic incantations (Zig `comptime`, Rust), no new compilation internals
to learn (Rust proc macros), just the full power of the language, at your fingertips.

### A macro example

```racket
(define-syntax (parameters stx)
    (syntax-case stx (parameters define)
      [(_ (outer ...)
           (define (name param ...) body ...) ...) 
           
           #''(define-values (name ...)
                (let ([f (lambda (outer ...)
                            (local [(define (name param ...) body ...) ...]
                                (list name ...)))])
                                
                     (values
                       (case-lambda 
                         [(outer ...)
                            (match (f outer ...)
                                [(list name ...) name])]
                         [(outer ... . rest)
                            (match (f outer ...)
                                [(list name ...) (apply name rest)])])
                       ...)))]))
```

This macro allows a `(parameters (a ...) defs ...)` block, where the parameters defined by
`a ...` are available and unchangeable for all `defs` within, but appear as normal arguments
to all `defs` from outside.

This is useful in e.g. a context variable, for example while processing a structure through
multiple, mutually-recursive functions. One example, printing a JSON value with pretty-printing
settings to a certain [`output-port`](https://docs.racket-lang.org/reference/ports.html#%28tech._output._port%29).

```racket
(parameters (out indent-length align-values)
  (define (print-value   v) ...)
  (define (print-object  o) ...)
  (define (print-array   a) ...)
  (define (print-boolean b) ...)
  (define (print-null    b) (display "null" out))
  (define (print-string  s) (write   s      out))
  (define (print-number  n) (write   n      out)))
```

## Part 0: What lies beyond

- [Units](https://docs.racket-lang.org/guide/units.html) Composable, reusable components that abstract over names.
- [Reflection](https://docs.racket-lang.org/reference/security.html)
- Futures and Threads
- GUIs
- Custom DSLs
- Continuations

## Resources

- [Racket Guide](https://docs.racket-lang.org/guide/index.html) and [Racket Reference](https://docs.racket-lang.org/reference/index.html).
- [Fear of Macros](https://www.greghendershott.com/fear-of-macros/index.html).
- [Beautiful Racket](https://beautifulracket.com/).
- [Hackett](https://youtu.be/5QQdI3P7MdY)
  - A (sadly abandoned) language by Alexis King, that embeds the *Haskell* language within Racket, with full support for the macro system while remaining statically typed.
  - Got the `parameters` idea from this talk.