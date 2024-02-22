FROM ocaml/opam:ubuntu-23.04-ocaml-4.14

USER root

# Without `libgmp-dev`, Coq cannot be installed.
RUN apt update && apt install -y libgmp-dev

USER opam

COPY . .

RUN opam install --yes .

ENTRYPOINT ["sh", "-c", "eval $(opam env) && opam exec -- coqfmt \"$@\"", "--"]
