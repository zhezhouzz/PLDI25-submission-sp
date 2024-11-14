FROM ocaml/opam:debian-ocaml-4.14-flambda
RUN sudo apt-get update
RUN sudo apt install -y software-properties-common
RUN sudo apt-get install -y python3 python3-pip python3-venv
RUN python3 -m venv .venv
RUN . .venv/bin/activate
RUN sudo apt-get install -y libgmp-dev
RUN opam init --auto-setup
RUN opam update
RUN opam switch create Cloiseau --package=ocaml-variants.4.14.1+options,ocaml-option-flambda
RUN sudo apt-get install -y wget
RUN wget https://packages.microsoft.com/config/ubuntu/22.04/packages-microsoft-prod.deb -O packages-microsoft-prod.deb
RUN sudo dpkg -i packages-microsoft-prod.deb
RUN rm packages-microsoft-prod.deb
RUN sudo apt-get update
RUN sudo apt install -y default-jre
RUN sudo apt update && sudo apt install -y dotnet-sdk-8.0
RUN eval $(opam env)
RUN opam repo add coq-released https://coq.inria.fr/opam/released
RUN opam update
RUN eval $(opam env)
RUN opam install dune.3.11.1 core.v0.16.0 core_unix.v0.16.0 yojson.1.7.0 conf-c++.1.0 conf-python-3.1.0.0 qcheck.0.18.1 ocolor.1.3.0 dolog.6.0.0 ocamlbuild.0.14.1 ppx_deriving ppx_deriving_yojson ppx_here
RUN sudo apt-get install -y pkg-config libpcre3-dev
RUN opam install spectrum.0.6.0
RUN opam install z3.4.8.14
RUN sudo apt-get install -y vim
RUN .venv/bin/pip3 install tabulate
SHELL ["/bin/bash", "-lc"]
ARG CACHEBUST=1
ADD zutils zutils
ADD AutomataLibrary AutomataLibrary
ADD Clouseau Clouseau
RUN cd zutils && opam install . && cd ..
RUN cd AutomataLibrary && opam install . && cd ..
RUN eval $(opam env)
WORKDIR Clouseau
USER root
RUN dune build
