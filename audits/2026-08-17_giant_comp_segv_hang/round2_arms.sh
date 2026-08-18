#!/bin/bash
# Round-2 arms for the giant_comp SIGSEGV/hang investigation.
# Contract is PREREGISTRATION.md in this directory — n=150 per arm, decision table
# for B x C committed there BEFORE any of this ran. Do not change n or the arm
# definitions without amending the prereg in the same commit.
#
# Usage:  ./round2_arms.sh [arm ...]      (default: A B C D)
#         SWIPL=/path/to/swipl ./round2_arms.sh A     (arm F: newer swipl)
set -u
cd "$(dirname "$0")/../../prolog" || exit 1
SWIPL=${SWIPL:-swipl}
N=${N:-150}
TMO=${TMO:-25}

corpus_fp () { ls testsets/*.pl | sort | xargs md5sum 2>/dev/null | md5sum | cut -d' ' -f1; }

# One arm. $1=label  $2=goal prefix  $3=extra modules  $4=n
run_arm () {
  local lab="$1" pre="$2" mods="$3" n="$4"
  local fp0 fp1 out hangs segvs fails
  fp0=$(corpus_fp)
  out=$(mktemp)
  for i in $(seq "$n"); do
    o=$(timeout "$TMO" $SWIPL -l stack.pl $mods \
          -g "${pre}run_giant_component_analysis, halt." 2>/dev/null)
    rc=$?
    printf '%d\trc=%d\t%d\n' "$i" "$rc" "${#o}" >> "$out"
  done
  fp1=$(corpus_fp)
  hangs=$(grep -c 'rc=124' "$out"); segvs=$(grep -c 'rc=139' "$out")
  fails=$(grep -vc 'rc=0' "$out")
  echo "### $lab"
  echo "    swipl:  $($SWIPL --version 2>/dev/null | head -1)"
  if [ "$fp0" != "$fp1" ]; then
    echo "    *** VOID: corpus fingerprint moved mid-arm ($fp0 -> $fp1) — re-run ***"
  else
    echo "    corpus: $fp0 (stable across the arm)"
  fi
  echo "    n=$n  failures=$fails  (hang=$hangs  segv=$segvs)  rate=$(awk "BEGIN{printf \"%.1f%%\", 100*$fails/$n}")"
  echo "    raw: $out"
}

for arm in "${@:-A B C D}"; do
  case "$arm" in
    A) run_arm "ARM A  baseline (default flags)" \
               "" "-l giant_component_analysis.pl" "$N" ;;
    B) run_arm "ARM B  gc_thread=false (AGC in the calling thread)" \
               "set_prolog_flag(gc_thread,false), " "-l giant_component_analysis.pl" "$N" ;;
    C) run_arm "ARM C  agc_margin enormous (AGC effectively never fires)" \
               "set_prolog_flag(agc_margin,1000000000), " "-l giant_component_analysis.pl" "$N" ;;
    D) run_arm "ARM D  line-buffered stdout (localize the true death point)" \
               "set_stream(user_output,buffer(line)), " "-l giant_component_analysis.pl" "$N" ;;
    *) echo "unknown arm: $arm" ;;
  esac
done
