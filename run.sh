#!/bin/sh

for sz in `seq 1 20`;
do
  for reps in `seq 1 100`;
  do
    .stack-work/dist/x86_64-linux/Cabal-1.22.5.0/build/bench-hlinear/bench-hlinear --matsize=$sz >>bench_timings/$(printf %04d.data $sz) 2>&1
  done
done
