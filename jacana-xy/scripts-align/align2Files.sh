#! /bin/sh

java -DJACANA_HOME=../ -jar ../build/lib/jacana-align.jar -m fr-en.model -src fr -tgt en -a s1.txt -b s2.txt -o s.align
