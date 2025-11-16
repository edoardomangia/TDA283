#!/usr/bin/env bash
set -e

JLC=./jlc
GOOD_DIR=~/Chalmers/project/tester/testsuite/good
BAD_DIR=~/Chalmers/project/tester/testsuite/bad
TMP_DIR=~/Chalmers/TDA283/tmp
RUNTIME_BC=../lib/runtime.bc

# Ensure tmp dir exists
mkdir -p "$TMP_DIR"

if [ ! -x "$JLC" ]; then
  echo "jlc not found or not executable. Build first with 'make'." >&2
  exit 1
fi

if [ ! -f "$RUNTIME_BC" ]; then
  echo "runtime.bc not found at $RUNTIME_BC. Build / place it there first." >&2
  exit 1
fi

# Optional: check LLVM tools exist
for tool in llvm-as llvm-link lli; do
  if ! command -v "$tool" >/dev/null 2>&1; then
    echo "Error: $tool not found in PATH." >&2
    exit 1
  fi
done

echo "=== Testing good programs (code generation + execution) ==="
good_ok=0
good_fail=0

for f in "$GOOD_DIR"/*.jl; do
  [ -e "$f" ] || continue

  base=$(basename "$f" .jl)
  ref_out="${GOOD_DIR}/${base}.output"

  if [ ! -f "$ref_out" ]; then
    echo " GOOD: $f   (missing reference output: $ref_out)"
    good_fail=$((good_fail+1))
    continue
  fi

  ll=$(mktemp)
  stderr=$(mktemp)

  # Run the compiler, capture LLVM on stdout and log on stderr
  "$JLC" "$f" 1>"$ll" 2>"$stderr"
  ec=$?
  first=$(head -n1 "$stderr" || echo "")

  # Check compiler success
  if [ "$ec" -ne 0 ] || [ "$first" != "OK" ]; then
    echo " GOOD: $f   (jlc failed: exit=$ec, first line='$first')"
    good_fail=$((good_fail+1))

    echo "  --- STDERR from jlc ---"
    cat "$stderr"
    echo "  --- Source ($f) ---"
    sed -n '1,120p' "$f"
    echo "  --- end ---"
    echo

    # Save debugging info
    cp "$f" "$TMP_DIR/${base}.jl"
    cp "$stderr" "$TMP_DIR/${base}.jlc.stderr"
    cp "$ll" "$TMP_DIR/${base}.ll"

    rm -f "$stderr" "$ll"
    continue
  fi

  # Assemble, link and run
  bc=$(mktemp)
  linked=$(mktemp)
  prog_out=$(mktemp)
  runerr=$(mktemp)

  if ! llvm-as "$ll" -o "$bc" 2>>"$stderr"; then
    echo " GOOD: $f   (llvm-as failed)"
    good_fail=$((good_fail+1))

    cp "$f" "$TMP_DIR/${base}.jl"
    cp "$stderr" "$TMP_DIR/${base}.jlc+llvm.stderr"
    cp "$ll" "$TMP_DIR/${base}.ll"

    rm -f "$stderr" "$ll" "$bc" "$linked" "$prog_out" "$runerr"
    continue
  fi

  if ! llvm-link "$bc" "$RUNTIME_BC" -o "$linked" 2>>"$stderr"; then
    echo " GOOD: $f   (llvm-link failed)"
    good_fail=$((good_fail+1))

    cp "$f" "$TMP_DIR/${base}.jl"
    cp "$stderr" "$TMP_DIR/${base}.jlc+llvm.stderr"
    cp "$ll" "$TMP_DIR/${base}.ll"
    cp "$bc" "$TMP_DIR/${base}.bc"

    rm -f "$stderr" "$ll" "$bc" "$linked" "$prog_out" "$runerr"
    continue
  fi

  if ! lli "$linked" >"$prog_out" 2>"$runerr"; then
    echo " GOOD: $f   (lli failed)"
    good_fail=$((good_fail+1))

    cp "$f" "$TMP_DIR/${base}.jl"
    cp "$stderr" "$TMP_DIR/${base}.jlc+llvm.stderr"
    cp "$runerr" "$TMP_DIR/${base}.run.stderr"
    cp "$ll" "$TMP_DIR/${base}.ll"
    cp "$bc" "$TMP_DIR/${base}.bc"
    cp "$linked" "$TMP_DIR/${base}.linked.bc"

    rm -f "$stderr" "$ll" "$bc" "$linked" "$prog_out" "$runerr"
    continue
  fi

  # Compare program output with reference
  if diff -u "$ref_out" "$prog_out" >/dev/null 2>&1; then
    echo " GOOD: $f"
    good_ok=$((good_ok+1))
  else
    echo " GOOD: $f   (output mismatch)"
    good_fail=$((good_fail+1))

    echo "  --- EXPECTED ($ref_out) ---"
    cat "$ref_out"
    echo "  --- GOT ---"
    cat "$prog_out"
    echo "  --- end ---"
    echo

    cp "$f" "$TMP_DIR/${base}.jl"
    cp "$stderr" "$TMP_DIR/${base}.jlc+llvm.stderr"
    cp "$ll" "$TMP_DIR/${base}.ll"
    cp "$bc" "$TMP_DIR/${base}.bc"
    cp "$linked" "$TMP_DIR/${base}.linked.bc"
    cp "$prog_out" "$TMP_DIR/${base}.output.actual"
    cp "$ref_out" "$TMP_DIR/${base}.output.expected"
  fi

  rm -f "$stderr" "$ll" "$bc" "$linked" "$prog_out" "$runerr"
done

echo
echo "=== Testing bad programs (frontend/typechecker) ==="
bad_ok=0
bad_fail=0

for f in "$BAD_DIR"/*.jl; do
  [ -e "$f" ] || continue

  stderr=$(mktemp)
  "$JLC" "$f" 1>/dev/null 2>"$stderr"
  ec=$?
  first=$(head -n1 "$stderr" || echo "")

  if [ "$ec" -ne 0 ] && grep -qx "ERROR" "$stderr"; then
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

    base=$(basename "$f" .jl)
    cp "$f" "$TMP_DIR/${base}.jl"
    cp "$stderr" "$TMP_DIR/${base}.stderr"

    rm -f "$stderr"
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

