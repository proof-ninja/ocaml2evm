# README

## Overview

The compiler that:
- generates a Yul (https://docs.soliditylang.org/en/latest/yul.html) code from an OCaml code,
- passes the compiled result to `solc`, and 
- saves the result `.json` file including an ABI and an EVM code.

## How to Build

By `dune exec ocamyulc <parent>/<dirname>/<filename>.ml`, 
`.json` file for `<filename>.ml` is generated and saved in `<parent>/contracts` directory.

## Examples

See `sample/READEME.md`.