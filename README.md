# Allium Compiler

## Introduction

Welcome to the Allium Compiler project! Allium is an innovative programming language that
seamlessly integrates systems programming with advanced academic and theoretical computer science concepts. This
language is designed to offer a unique blend of features like dependent types, function currying, and functional
programming, catering to both practical and theoretical applications in computer science.

## Current Status

As of now, the project is in its early stages, focusing on the foundational aspects of the language compiler. The
tokenizer, a crucial component for parsing the language, has been implemented. Future developments will include the
construction of the Abstract Syntax Tree (AST) and the LLVM front-end.

### Completed Features

- **Tokenizer**: The tokenizer is responsible for breaking down the source code into tokens, which are the basic
  building blocks for further
  parsing. [View Tokenizer Code](https://github.com/MiguelX413/allium/blob/master/tokenizer/src/lib.rs)

### Upcoming Features

- **Abstract Syntax Tree (AST)**: The AST will represent the hierarchical syntactic structure of the source code, which
  is essential for the subsequent stages of compilation.
- **LLVM Front-End**: Integration with LLVM will enable the compiler to generate efficient machine code, leveraging
  LLVM's powerful optimization and code generation capabilities.

## License

This project is licensed under [GPL-3.0 License](https://github.com/MiguelX413/allium/blob/master/LICENSE).
