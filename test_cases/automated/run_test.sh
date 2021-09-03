#!/bin/bash
TCPATH=$(dirname "$BASH_SOURCE")

pushd "$TCPATH/../.." > /dev/null
cabal build
BLOCKTOROK=$(cabal exec -- which blocktorok)
popd > /dev/null

FAILURE_COUNT=0
FAILURES=""

for TEST in "$@"
do
  if [ -d $TEST ]; then
    OUT="output"
    pushd $TEST > /dev/null && \
      mkdir -p expected && \
      $BLOCKTOROK -t transform.oct -o $OUT input.blok &&  \
      diff -r -q expected $OUT
    if [ $? -ne 0 ]; then
      FAILURE_COUNT=$(($FAILURE_COUNT + 1))
      FAILURES="$FAILURES\n$TEST"
      echo -e "[\u001b[31m FAIL \u001b[0m] $TEST"
    else
      echo -e "[\u001b[32m OK   \u001b[0m] $TEST"
    fi
    popd > /dev/null
  fi
done

echo "Failures: $FAILURE_COUNT"
echo "Tests Failed:"
echo $FAILURES