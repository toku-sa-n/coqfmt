# Rocqfmt

A command line tool to format your Coq source code.

## Installation

Clone this repository and build using `opam`. You need `dune`. This package is
not available in the OPAM repository (yet).

```sh
git clone https://github.com/toku-sa-n/rocqfmt
cd rocqfmt
opam install .
```

## Usage

Just input your Coq code from stdin. Rocqfmt will print the formatted code to
stdout.

Rocqfmt is aware of commandline arguments in `_CoqProject` file. If you have a
`_CoqProject` file in your project, Rocqfmt will use the arguments in the file.

See `rocqfmt --help` for more detail.

## Don't like the formatted code?

Open an issue, please! We're still figuring out what the best style is.

## License

Copyright (C) 2023 Hiroki Tokunaga

This program is free software: you can redistribute it and/or modify it under
the terms of the GNU Affero General Public License as published by the Free
Software Foundation, either version 3 of the License, or (at your option) any
later version.

This program is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A
PARTICULAR PURPOSE. See the GNU Affero General Public License for more details.

You should have received a copy of the GNU Affero General Public License along
with this program. If not, see <https://www.gnu.org/licenses/>.

This project uses source code from [Logical
Foundations](https://softwarefoundations.cis.upenn.edu/lf-current/index.html),
and its license file is [LICENSE_LF](./LICENSE_LF).
