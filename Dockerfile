FROM ubuntu:22.04

RUN apt update
RUN apt install -y opam

# `opam init` warns if the runner is root.
RUN useradd menyanya
USER menyanya

# See https://github.com/ocaml/opam/issues/3498#issuecomment-410401597 for
# `--disable-sandboxing`.
RUN opam init --disable-sandboxing --yes --shell-setup

COPY . .

# There is no way to separate these two commands.
# See https://stackoverflow.com/questions/34911622/dockerfile-set-env-to-result-of-command.
RUN eval $(opam env) && opam install --yes .

ENTRYPOINT [ "coqfmt" ]
