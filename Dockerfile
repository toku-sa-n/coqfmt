FROM ocaml/opam:ubuntu-ocaml-4.14

COPY . .

RUN opam install --yes

ENTRYPOINT [ "coqfmt" ]
