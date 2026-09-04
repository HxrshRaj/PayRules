# ---- build stage --------------------------------------------------------------
# The official Haskell image ships GHC 9.6.6 + Stack; 9.6.6 is exactly what
# snapshot lts-22.28 wants, so --system-ghc skips the toolchain download.
FROM haskell:9.6.6 AS build

WORKDIR /src

# Dependency layer: copy only the files that pin dependencies so this layer is
# cached until they change.
COPY stack.yaml stack.yaml.lock PayRules.cabal ./
RUN stack build --system-ghc --no-install-ghc --only-dependencies \
      PayRules:exe:payrules-server

# Application layer.
COPY . .
RUN stack build --system-ghc --no-install-ghc \
      --copy-bins --local-bin-path /out PayRules:exe:payrules-server

# ---- runtime stage ----------------------------------------------------------
FROM debian:bookworm-slim

RUN apt-get update \
 && apt-get install -y --no-install-recommends ca-certificates libgmp10 \
 && rm -rf /var/lib/apt/lists/*

COPY --from=build /out/payrules-server /usr/local/bin/payrules-server

ENV PORT=8080
EXPOSE 8080
CMD ["payrules-server"]
