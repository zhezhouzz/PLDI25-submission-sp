FROM ocaml/opam:debian-ocaml-4.14-flambda
RUN sudo apt-get update
RUN sudo apt install -y software-properties-common
RUN sudo apt-get install -y python3 python3-pip python3-venv
RUN python3 -m venv .venv
RUN . .venv/bin/activate
RUN sudo apt-get install -y libgmp-dev
RUN opam init --auto-setup
RUN opam update
RUN opam switch create HAT --package=ocaml-variants.4.14.1+options,ocaml-option-flambda
RUN eval $(opam env)
RUN opam repo add coq-released https://coq.inria.fr/opam/released
RUN opam update
RUN eval $(opam env)
RUN opam install z3.4.8.14
RUN opam install dune.3.11.1 core.v0.15.0 core_unix.v0.15.0 yojson.1.7.0 conf-c++.1.0 conf-python-3.1.0.0 qcheck.0.18.1 ocolor.1.3.0 dolog.6.0.0 ocamlbuild.0.14.1 ppx_deriving.6.0.2 ppx_deriving_yojson.3.9.0 ppx_here.v0.16.0
RUN sudo apt-get install -y vim
RUN .venv/bin/pip3 install tabulate
SHELL ["/bin/bash", "-lc"]
ARG CACHEBUST=1
ADD zutils zutils
RUN cd language_utils && opam install . && cd ..
ADD AutomataLibrary AutomataLibrary
RUN cd AutomataLibrary && opam install . && cd ..
RUN eval $(opam env)
ADD Clouseau Clouseau
WORKDIR Clouseau
USER root
