Chord
=====

An Erlang implementation of Chord peer-to-peer protocol [link to paper](https://pdos.csail.mit.edu/papers/chord:sigcomm01/chord_sigcomm.pdf).

- Nodes are run as Erlang Processes
- Communication is done using synchronous RPC usign OTP GenSever

This project is intended for benchmarking Chord and analyzing the overheads of hashing and communication.

[Project Report](report/project_report.pdf)

Build
-----

    $ make build


Benchmark Join
-----
To benchmark Nodes joining the network the benchmarking script run

    $ escript scripts/benchmark-join.escript <NumberOfNode>


Benchmark Key-Value
-----
To benchmark the Key-Value insertion and retieval benchmarks run 

    $ escript scripts/benchmark-kv.escript <NumberOfNodes> <NumberOfKeys>


Profile Key-Value Insertions
----
To profile Key-Value insertions and retievals run the command below to gather the profiling data

    $ escript scripts/profile-kv.escript <NumberOfNodes> <NumberOfKeys> <KeySizeInCharacters> <PathToOutputFile>

Then run the analysis script

    $ python3 analyze-profiling.py <PathToProfileData>
----
