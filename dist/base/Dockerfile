FROM fpco/stack-build:lts-12.20
MAINTAINER lierdakil

FROM fpco/pid1:16.04
MAINTAINER lierdakil

RUN apt-get update && apt-get -y install graphviz
COPY --from=0 /opt/ghc/8.4.4/lib/ghc-8.4.4/settings \
  /opt/ghc/8.4.4/lib/ghc-8.4.4/platformConstants \
  /opt/ghc/8.4.4/lib/ghc-8.4.4/llvm-targets \
  /opt/ghc/8.4.4/lib/ghc-8.4.4/
