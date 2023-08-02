FROM ocaml/opam:ubuntu-23.04-ocaml-4.14

COPY . .

RUN opam install --yes .

ENTRYPOINT [ "coqfmt" ]
