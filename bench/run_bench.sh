#!/bin/sh

# Script to run benchmarks
# Make sure the current directory is same directory to
# this script file.

# Prepare benchmark data files

if [ -f './american-english-shuf' ]; then
    echo american-english-shuf exists
else
    shuf /usr/share/dict/american-english > '../benchdata/american-english-shuf'
fi

if [ -f './externallinks.txt.1' ]; then
    echo eternallinks.txt.1 exists
else
    head -n 20000 './externallinks.txt.all' > '../benchdata/externallinks.txt.1'
    head -n 30000 './externallinks.txt.all' | \
        tail -n 20000 > '../benchdata/externallinks.txt.2'
fi

# Delete output file of previous run
rm -f ../benchdata/bench.csv
# Run benchmark
cabal run trie-benchmark -- -s --csv='tmp.csv'
# Preprocess the output file
sed -e 's/Name,Mean/Name\/Category\/Method,MeanTime/' ../benchdata/bench.csv | tr '/' ',' > ../benchdata/bench_preprocessed.csv
# Draw charts using R
R CMD BATCH analyze.R '../benchdata/analyze.Rout'
mv -f *.png ../doc/
