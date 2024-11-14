# PLDI25-submission-sp

The main code of our tool, **Clouseau**, is located in the `Clouseau` folder, with the benchmark suites found in `Clouseau/benchmarks`. The `zutils` folder contains utility functions, while `AutomaraLibrary` serves as a helper library for symbolic finite automata (SFA).

## Getting Started Guide

We recommend machines have at least 8 GB of memory and 8 GB of hard
disk space available when building and running Docker images. All
benchmarks were tested on a Linux machine having Intel i7-8700 CPU @ 3.20GHz with `64GB` of RAM. The estimated execution time in the rest of the document also fits this setting.

### Requirements

This artifact is built as a Docker image. Before proceeding, ensure
Docker is installed. (On *nix, `sudo docker run hello-world` will test
your installation.) If Docker is not installed, install it via the
[official installation guide](https://docs.docker.com/get-docker/). This guide was tested using Docker version `20.10.23`, but any contemporary Docker version is expected to work.

### Using the Pre-Built Docker Image

You may fetch the pre-built Docker image from Docker Hub:

    $ docker pull clouseau2025/clouseau:pldi-2025

You may also download docker image from Zenodo (https://zenodo.org/records/14166141), and load the docker image from the downloaded file `clouseau:pldi-2025.tar.gz`.

    $ docker load < clouseau:pldi-2025.tar.gz

### Building the Docker Image (Optional)

We also include a Dockerfile to build docker image for Clouseau:

```
docker build . --tag clouseau2025/clouseau:pldi-2025
```

**Resource Requirements:** Although our tool **Clouseau** doesn't have large memory usage, building the docker image needs more than `32GB` RAM available. This memory usage requirement comes from the installation of the SMT solver `z3` (https://github.com/Z3Prover/z3). When the RAM limit of Docker (by default, it is `8GB` on Mac, no limit on Linux machine) is lower than `32GB`, the installation of `z3` will be killed and the `docker build` will fail.
The memory error can be fixed by increasing the RAM limit in Docker; you can find instructions for doing so on Mac here: (https://docs.docker.com/desktop/settings/mac/#resources), for Windows here: (https://docs.docker.com/desktop/settings/windows/#resources), and for Linux here: (https://docs.docker.com/desktop/settings/linux/#resources). The pre-built docker image is built on a Linux machine having Intel i7-8700 CPU @ 3.20GHz with `64GB` of RAM, it took `30` min to build.

### Running the Docker Image

To launch a shell in the Docker image:

    $ docker run -it -m="8g" clouseau2025/clouseau:pldi-2025

To compile **Clouseau**:

    $ dune build && cp _build/default/bin/main.exe main.exe
The compilation result of **Clouseau** is an executable `_build/default/bin/main.exe`. For the sake of convenience, we copy it under the current directory. You can run **Clouseau** by executing `main.exe <args>` directly or executing it via `dune`, that is `dune exec -- bin/main.exe <args>`.

You can print **Clouseau**'s help message to verify the tool operating
successfully:

    $ ./main.exe --help

### Running Benchmarks of Clouseau

#### Run synthesizer

```
  dune exec -- bin/main.exe syn-benchmark [benchmark nane]
```

For example,

```
  dune exec -- bin/main.exe syn-benchmark Database
```

Will print following output that contains synthesized program and synthesis time:

```
let (x) = assume[(int)] true in
gen writeReq(x);
let (y) = assume[(int)] ¬y == x in
gen writeReq(y);
let (tmp_2) = obs writeRsp; assert tmp_2 == y in
let (tmp_1) = obs writeRsp; assert tmp_1 == x in
gen readReq();
let (tmp_0, s_0) = obs readRsp; assert tmp_0 == y ∧ s_0 in
()
synthesis time: 2.765237
```

#### Comprehensive Scripts

The following scripts execute the benchmark suite shown in Table 1 of the paper, which will take approximately one and a half hours to complete.

```
python3 script/run_bench.py
```
