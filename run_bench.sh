#!/bin/sh

# Script to run benchmarks
# Make sure the current directory is the same directory to
# this script file.

# Prepare benchmark data files

if [ -f './benchdata/american-english-shuf' ]; then
    echo american-english-shuf exists
else
    shuf /usr/share/dict/american-english > './benchdata/american-english-shuf'
fi

if [ -f './benchdata/externallinks.txt.1' ]; then
    echo eternallinks.txt.1 exists
else
    head -n 20000 './externallinks.txt.all' > './benchdata/externallinks.txt.1'
    head -n 30000 './externallinks.txt.all' | \
        tail -n 20000 > './benchdata/externallinks.txt.2'
fi

# Run benchmark
datestr="$(date -Iseconds)"
benchfile="benchdata/bench-${datestr}.csv"
cabal run trie-benchmark -- -s "--csv=$benchfile"
# Preprocess the output file
sed -e 's/Name,Mean/Dataset\/Name\/Category\/Method,MeanTime/' "$benchfile" | tr '/' ',' > ./benchdata/bench_preprocessed.csv
# Draw charts using R
R CMD BATCH bench/analyze.R

# Write results to doc/ directory
mv -f benchdata/*.png doc/
cp -f benchdata/bench_preprocessed.csv doc/benchmark.csv
echo "$datestr" > "doc/benchmark-generation-date"
