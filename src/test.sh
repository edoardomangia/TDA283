#!/usr/bin/env bash
set -u

JLC=./jlc
GOOD_DIR=~/Chalmers/project/tester/testsuite/good
BAD_DIR=~/Chalmers/project/tester/testsuite/bad

if [ ! -x "$JLC" ]; then
  echo "jlc not found or not executable. Build first with 'make'." >&2
  exit 1
fi

echo "=== Testing good programs ==="
good_ok=0
good_fail=0

for f in "$GOOD_DIR"/*.jl; do
  [ -e "$f" ] || continue
  stderr=$(mktemp)
  "$JLC" "$f" 1>/dev/null 2>"$stderr"
  ec=$?
  first=$(head -n1 "$stderr" || echo "")
  rm -f "$stderr"

  if [ "$ec" -eq 0 ] && [ "$first" = "OK" ]; then
    echo " GOOD: $f"
    good_ok=$((good_ok+1))
  else
    echo " GOOD: $f   (exit=$ec, first line='$first')"
    good_fail=$((good_fail+1))
  fi
done

echo
echo "=== Testing bad programs ==="
bad_ok=0
bad_fail=0

for f in "$BAD_DIR"/*.jl; do
  [ -e "$f" ] || continue
  stderr=$(mktemp)
  "$JLC" "$f" 1>/dev/null 2>"$stderr"
  ec=$?
  first=$(head -n1 "$stderr" || echo "")
  rm -f "$stderr"

  if [ "$ec" -ne 0 ] && [ "$first" = "ERROR" ]; then
    echo " BAD : $f"
    bad_ok=$((bad_ok+1))
  else
    echo " BAD : $f   (exit=$ec, first line='$first')"
    bad_fail=$((bad_fail+1))
  fi
done

echo
echo "Summary:"
echo "  good: ${good_ok} ok, ${good_fail} fail"
echo "  bad : ${bad_ok} ok, ${bad_fail} fail"

if [ "$good_fail" -eq 0 ] && [ "$bad_fail" -eq 0 ]; then
  exit 0
else
  exit 1
fi

