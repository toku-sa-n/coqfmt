FROM ocaml/opam:ubuntu-23.04-ocaml-4.14

# Without `libgmp-dev`, Coq cannot be installed.
RUN sudo apt update && sudo apt install -y libgmp-dev

COPY . .

RUN opam install --yes .

ENTRYPOINT ["sh", "-c", "eval $(opam env) && opam exec -- coqfmt \"$@\"", "--"]
