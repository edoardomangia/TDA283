#!/usr/bin/env bash
set -u

JLC=./jlc
GOOD_DIR=~/Chalmers/project/tester/testsuite/good
BAD_DIR=~/Chalmers/project/tester/testsuite/bad
TMP_DIR=~/Chalmers/TDA283/tmp

# Ensure tmp dir exists
mkdir -p "$TMP_DIR"

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

  if [ "$ec" -eq 0 ] && [ "$first" = "OK" ]; then
    echo " GOOD: $f"
    good_ok=$((good_ok+1))
    rm -f "$stderr"
  else
    echo " GOOD: $f   (exit=$ec, first line='$first')"
    good_fail=$((good_fail+1))

    echo "  --- STDERR from jlc ---"
    cat "$stderr"
    echo "  --- Source ($f) ---"
    sed -n '1,120p' "$f"
    echo "  --- end ---"
    echo

    mv "$stderr" "$TMP_DIR/$(basename "$f").stderr"
	cp "$f" "$TMP_DIR/$(basename "$f")"

	cp "$f" "$TMP_DIR/$(basename "$f").txt"
	cp "$TMP_DIR/$(basename "$f").stderr" "$TMP_DIR/$(basename "$f").stderr.txt"
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

  if [ "$ec" -ne 0 ] && [ "$first" = "ERROR" ]; then
    echo " BAD : $f"
    bad_ok=$((bad_ok+1))
    rm -f "$stderr"
  else
    echo " BAD : $f   (exit=$ec, first line='$first')"
    bad_fail=$((bad_fail+1))

    echo "  --- STDERR from jlc ---"
    cat "$stderr"
    echo "  --- Source ($f) ---"
    sed -n '1,120p' "$f"
    echo "  --- end ---"
    echo

    mv "$stderr" "$TMP_DIR/$(basename "$f").stderr"
	cp "$f" "$TMP_DIR/$(basename "$f")"

	cp "$f" "$TMP_DIR/$(basename "$f").txt"
	cp "$TMP_DIR/$(basename "$f").stderr" "$TMP_DIR/$(basename "$f").stderr.txt"
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

