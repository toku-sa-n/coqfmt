FROM ubuntu:22.04

RUN apt update
RUN apt install -y opam

# See https://github.com/ocaml/opam/issues/3498#issuecomment-410401597 for
# `--disable-sandboxing`
RUN opam init --disable-sandboxing --yes

COPY . .

RUN opam install --yes .

ENTRYPOINT [ "coqfmt" ]
