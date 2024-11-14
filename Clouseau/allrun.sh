#!/bin/bash

./run.sh Database &&
./run.sh HeartBeat &&
    ./run.sh BankServer &&
    ./run.sh EspressoMachine &&
    ./run.sh Simplified2PC &&
    ./run.sh Kermit2PCModel &&
    ./run.sh RingLeaderElection &&
    ./run.sh Firewall &&
    ./run.sh ChainReplication &&
    ./run.sh Paxos &&
    ./run.sh Raft
