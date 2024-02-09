FROM ocaml/opam:ubuntu-23.04-ocaml-4.14

# Without `libgmp-dev`, Coq cannot be installed.
RUN sudo apt update
RUN sudo apt install -y libgmp-dev

RUN opam update

COPY . .

RUN opam install --yes .

ENTRYPOINT ["sh", "-c", "eval $(opam env) && opam exec -- coqfmt \"$@\"", "--"]
