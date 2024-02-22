FROM ocaml/opam:ubuntu-23.04-ocaml-4.14

USER root

# Without `libgmp-dev`, Coq cannot be installed.
RUN apt-get update && apt-get install -y libgmp-dev

USER opam

COPY . .

RUN opam install --yes .

ENTRYPOINT ["sh", "-c", "eval $(opam env) && opam exec -- coqfmt \"$@\"", "--"]
