# Automatic Test Generator for SystemVerilog

This is a prototype of Concolic Testing engine for SystemVerilog.
This program generates 100% line-of-code coverage test cases for a circuit written in SystemVerilog by alternatively repeating concrete execution and simbolic execution in the similar way as DART[1] and HYBRO[2].

There are several limitations on supported SystemVerilog Features.

**Disclaimer: PFN provides no warranty or support for this software. Use it at your own risk.**

This software is developed as part of [PFN summer internship 2018](https://www.preferred-networks.jp/en/news/internship2018summer) and the main developer is Hiroki Oshikawa.

## How to use

### Dependencies

* OCaml (>= 4.05.0)
  * dune (>= 1.0.1)
  * z3 (>= 4.7.1)
  * ocamlgraph (>= 1.8.8)
  * ppx_deriving (>= 4.2.1)
* [Z3](https://github.com/Z3Prover/z3) (>= 4.7.1)
* [menhir](http://gallium.inria.fr/~fpottier/menhir/) (>= 20180703)
* SystemVerilog Simulator

OCaml libraries and menhir can be installed through [opam](https://opam.ocaml.org/).

### Build and Run

```sh
make
_build/default/src/main.exe -i file.sv -f file.bin -v log.vcd -s script.sh
```

Note: `make` emits many warnings for now.

* `file.sv` : Target program written in SystemVerilog
* `file.bin` : File used to read and write input vectors during execution
* `log.vcd` : VCD file
* `script.sh` : Shell script to run a simulator. It is assumed that `file.sh` runs `file.sv` against input vectors in `file.bin` and dump a result into `log.vcd`.

#### Options

```txt
-c              dump coverage information
-dparse         dump parsed tree
-dir            dump IR
-dcfg           generate dot files from CFG
-dtrace         dump trace
-dz3            dump z3 log
-dinputs file   dump all input vectors to [file]
-test           run without concrete execution
```

## Procedure of test case generation

1. Parse a program source code and generate Control Flow Graphs (CFG)
2. Generate input vectors randomly
3. Run simulator
4. Calculate which paths in CFGs were executed from the dumped VCD file
5. Generate constraints symbolically to execute a part of the program that has not been executed yet
6. Solve the constraints using SMT
7. Extract next input vectors from the solution
8. Repeat 3-7 until all parts of the program are executed

Input vectors generated above procedure comprise a test case.

![overview](overview.svg)

## Structure of `src/`

* lexer.mll:     Lexer
* parser.mly:    Parser
* parsetree.ml:  Abstract syntax tree
* ir.ml:         Simple intermediate representation(IR) and translation from AST
* cfg.ml:        CFG and translation from IR
* vis.ml:        Visualization of CFG
* ce.ml:         Calculate executed paths from VCD and CFG
* constraint.ml: Calculate constraints
* solve.ml:      Solve the constraints using Z3
* main.ml:       Put together above

## License

MIT License (see the [LICENSE](./LICENSE) file for details).

## References

* [1] P. Godefroid, N. Klarlund, and K. Sen, DART: Directed Automated Random Testing. In Proceedings of the 2005 ACM SIGPLAN conference on Programming language design and implementation (PLDI'05), 2005.
* [2] L. Liu and S. Vasudevan, Efficient validation input generation in RTL by hybridized source code analysis, In Proceedings of the 2011 Design, Automation & Test in Europe, Grenoble, 2011.
