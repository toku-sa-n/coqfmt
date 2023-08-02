FROM ocaml/opam:ubuntu-23.04-ocaml-4.14

# Without this, Coq cannot be installed.
RUN sudo apt update
RUN sudo apt install -y build-essential

COPY . .

RUN opam install --yes .

ENTRYPOINT [ "coqfmt" ]
