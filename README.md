# Slang Compiler

A project to create a compiler in OCAML for CSC312

## Getting Started

Simply download the repo as a zip and run the MAKEFILE.

In addition, to enable git hooks, create a Symbolic link between the pre-commit file in githooks/ and .git/hooks/ using the command below.

```
ln -s githooks/ .git/hooks/
```

### Prerequisites

You will need OCAML but no accompanying libraries. You will also need a Linux distro or appropriate UNIX-like OS capable of running bash scripts.

```
apt-get install ocaml-nox # If you don't want X11 support.
apt-get install ocaml

pacman -S ocaml

brew install ocaml
brew install opam
```
You will also need menhir and utop:
```
opam install menhir
opam install utop
```
## Syntax

```
e ::= n | (e) | e1 + e1 | e1 - e2 | e1 * e2
        | b | e1 <= e2 | e1 >= e2 | e1 < e2
        | e1 > e2 | e1 = e2 | if e1 then e2 else e3
        | x | let x = e1 in e2 | fun x -> e | e1 e2
        | [] : t | e1 :: e2 | hd e | tl e | empty e

t ::= [t]
```
Example Program (Factorial):

```
(let fact : int-> int = (fix func (x:int) : int -> (if (x > 0) then (x * func (x - 1)) else 1)) in fact 26)
```
## Running the tests

The testing suite can be accessed by running

```
make test
```

## Authors

* **Yash Gupta**

See also the list of [contributors](https://github.com/yashdavisgupta/Slang/graphs/contributors) who participated in this project.

## License

This project is licensed under the GPL V.3 License - see the [LICENSE.md](LICENSE.md) file for details
