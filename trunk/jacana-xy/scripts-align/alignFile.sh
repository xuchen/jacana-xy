#! /bin/sh

java -DJACANA_HOME=../ -jar ../build/lib/jacana-align.jar -m fr-en.model -src fr -tgt en -a s.txt -o s.json
