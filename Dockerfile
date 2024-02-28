FROM ocaml/opam:ubuntu-23.04-ocaml-4.14

USER root

# Without `libgmp-dev`, Coq cannot be installed.
RUN apt-get update \
    && apt-get install -y --no-install-recommends libgmp-dev=2:6.2.1+dfsg1-1.1ubuntu1 \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/*

USER opam

COPY . $HOME

# https://discuss.ocaml.org/t/which-ocaml-opam-docker-containers-to-use/8269/4
RUN opam repository set-url default "https://opam.ocaml.org" && opam install --yes .

ENTRYPOINT ["sh", "-c", "eval $(opam env) && opam exec -- coqfmt \"$@\"", "--"]
