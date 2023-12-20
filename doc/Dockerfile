#syntax=docker/dockerfile:1.4

FROM ghcr.io/mizunashi-mana/docker-texlive AS spec-builder

SHELL ["bash", "-euxo", "pipefail", "-c"]

COPY spec /workdir/spec

WORKDIR /workdir/spec
RUN <<EOT
make main.pdf
EOT


FROM node:20 AS dist-builder

SHELL ["bash", "-euxo", "pipefail", "-c"]

COPY . /workdir
COPY --from=spec-builder /workdir/spec/main.pdf /workdir/spec/main.pdf

WORKDIR /workdir
RUN <<EOT
npm install
make dist
EOT


FROM busybox

COPY --from=dist-builder /workdir/dist /dist
