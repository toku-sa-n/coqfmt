FROM ocaml/opam:ubuntu-23.04-ocaml-4.14

# Without this, Coq cannot be installed.
RUN sudo apt update
RUN sudo apt install -y libgmp-dev

COPY . .

RUN opam install --yes .

ENTRYPOINT [ "opam exec -- coqfmt" ]
