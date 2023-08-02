FROM ocaml/opam:ubuntu-23.04-ocaml-4.14

RUN apt update
RUN apt install -y opam

COPY . .

RUN opam install --yes .

ENTRYPOINT [ "coqfmt" ]
