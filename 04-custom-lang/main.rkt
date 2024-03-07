#lang reader "lang.rkt"

let a = 20;

function double(x) {
    return x * 2;
}

let twice = f => x => f(f(x));

show(twice(double)(a));