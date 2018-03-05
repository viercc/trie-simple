#!/usr/bin/env bash
word_set_name=hextalk_large8
grep -e '^[oizsqabcdef]\{8\}$' /usr/share/dict/words\
    | tr 'oizsqabcdef' '01259ABCDEF' > ${word_set_name}.txt
./words-to-graph.hs ${word_set_name}.txt > ${word_set_name}.dot
dot -Tsvg -o${word_set_name}.svg ${word_set_name}.dot
