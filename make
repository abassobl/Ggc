#!/bin/sh

set -e

TARGET=main
FLAGS="-libs unix,graph"
OCAMLOPT=" -ocamlopt -o2"
OCAMLBUILD=ocamlbuild

ocb()
{
  $OCAMLBUILD $FLAGS $*
}

rule() {
  case $1 in
    clean)  ocb -clean;;
    native) ocb $TARGET.native;;
    byte)   ocb $TARGET.byte;;
    all)    ocb $TARGET.native $TARGET.byte;;
    depend) echo "Not needed.";;
    *)      echo "Unknown action $1";;
  esac;
}
if [ $# -eq 0 ]; then
  rule all;
else
  for last; do true; done
  if [ $last -eq "-opt" ]; then FLAGS="$FLAGS $OCAMLOPT"; fi
  while [ $# -gt 0 && $1 -neq "-opt"]; do
    rule $1;
    shift
  done
fi