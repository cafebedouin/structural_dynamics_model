#!/bin/bash
# Round-2 arms for the giant_comp SIGSEGV/hang investigation.
# Contract is PREREGISTRATION.md in this directory — n=150 per arm, decision table
# for B x C committed there BEFORE any of this ran. Do not change n or the arm
# definitions without amending the prereg in the same commit.
#
# Round-2 amendments (2026-08-23) are recorded in PREREGISTRATION.md under
# "Round-2 amendments (2026-08-23)". This is arm A = **Set R only** (record-keeping);
# the watcher (Set W) is built in Step 3 / arm A' if and only if arm A fails.
#
# Usage:  ./round2_arms.sh [arm ...]      (default: A B C D)
#         TAG=warmup N=1 ./round2_arms.sh A
#         TAG=ctl_hang GOAL="sleep(60)" N=1 ./round2_arms.sh A
#
# Env:  N TMO KILL_AFTER BOUND_BLOCKS SWIPL TAG GOAL WRAP
#   TAG   output namespace (default arm_<letter>); the driver REFUSES to start if
#         raw/<TAG>.tsv already exists — outputs are never shared or appended across runs.
#   GOAL  control runs only: replaces ONLY the analysis call. The corpus-path overlay
#         prefix, the arm's flag prefix and the `halt.` suffix are kept, so a plant runs
#         the same load path as a counted row.
#   WRAP  diagnostic runs only (e.g. "valgrind --error-limit=no"); sits INSIDE timeout.
set -u

HERE="$(cd "$(dirname "$0")" && pwd)"
cd "$HERE/../../prolog" || exit 1

SWIPL=${SWIPL:-swipl}
N=${N:-150}
TMO=${TMO:-25}
KILL_AFTER=${KILL_AFTER:-5}
# 512-byte blocks; = 4 x peak_rss_kb = 2 x peak RSS, measured out-of-band at Step 0.3
# (peak_rss_kb=25136 on 2026-08-23). NOT `unlimited`: a core whose size == this bound
# is presumed TRUNCATED (a-priori rule, Step 0.4).
BOUND_BLOCKS=${BOUND_BLOCKS:-100544}
GOAL=${GOAL:-run_giant_component_analysis}
WRAP=${WRAP:-}

SNAP="$HERE/corpus_snapshot"
RAW="$HERE/raw"

ulimit -c "$BOUND_BLOCKS" || { echo "FATAL: cannot set ulimit -c $BOUND_BLOCKS" >&2; exit 2; }

# ---------------------------------------------------------------- frozen corpus
# The snapshot is FROZEN: created once, never silently refreshed. Absolute path, so
# corpus_loader:resolve_corpus_dir/2 passes it through unchanged.
ensure_snapshot () {
  if [ -d "$SNAP" ]; then
    echo "    snapshot: reusing frozen $SNAP"
  else
    mkdir -p "$SNAP" || exit 2
    cp -a testsets/*.pl "$SNAP"/ || exit 2
    echo "    snapshot: CREATED $SNAP"
  fi
}
# fingerprint of the SNAPSHOT (not the live leg). The 2>/dev/null here suppresses
# md5sum's own per-file noise and is unrelated to the counted invocation's stderr.
corpus_fp () { ls "$SNAP"/*.pl | sort | xargs md5sum 2>/dev/null | md5sum | cut -d' ' -f1; }
snap_count () { ls "$SNAP"/*.pl 2>/dev/null | wc -l; }

# ------------------------------------------------------------------- the matcher
# Process identification is by /proc comm, NEVER `pgrep -f`: a -f pattern matches the
# `timeout` parent, matches `valgrind` under WRAP, and — witnessed 2026-08-23, F2 —
# matches the checking shell's own command line, which would VOID every arm at row 1.
swipl_pids () {
  local d p
  for d in /proc/[0-9]*; do
    p=${d#/proc/}
    [ -r "$d/comm" ] || continue
    [ "$(cat "$d/comm" 2>/dev/null)" = "swipl" ] && echo "$p"
  done
  return 0
}
# the ROW's process: a swipl whose PARENT is this row's `timeout`.
row_swipl_pids () {
  local p ppid
  for p in $(swipl_pids); do
    ppid=$(awk '/^PPid:/{print $2}' "/proc/$p/status" 2>/dev/null)
    [ -n "${ppid:-}" ] || continue
    [ "$(cat "/proc/$ppid/comm" 2>/dev/null)" = "timeout" ] && echo "$p"
  done
  return 0
}
# the arm-level census. Bracket-trick so the census cannot match its own shell (F2).
census () { pgrep -af '[s]wipl|[c]-orchestrator|[r]un_pipeline' || true; }

# ------------------------------------------------------------------------ one arm
# $1=label  $2=goal prefix  $3=extra modules  $4=n  $5=arm letter
run_arm () {
  local lab="$1" pre="$2" mods="$3" n="$4" letter="$5"
  local tag fp0 fp1 o rc i t0 t1 stray surv rowpid void goalstr
  local before after new mpid

  tag=${TAG:-arm_$letter}
  mkdir -p "$RAW/$tag" || exit 2
  if [ -e "$RAW/$tag.tsv" ]; then
    echo "REFUSED: $RAW/$tag.tsv already exists — pick a fresh TAG (outputs are never shared)" >&2
    exit 3
  fi

  echo "### $lab   [TAG=$tag]"
  echo "    swipl:  $($SWIPL --version 2>/dev/null | head -1)"
  echo "    detector: TMO=$TMO  kill_after=$KILL_AFTER  KTHRESH=$(( (TMO+KILL_AFTER)*1000 - 1000 ))"
  echo "    ulimit -c: $(ulimit -c) blocks"
  ensure_snapshot
  fp0=$(corpus_fp)
  echo "    snapshot: $(snap_count) files  fp=$fp0"
  echo "    census(start):"; census | sed 's/^/      /'

  goalstr="asserta(config:param(corpus_path,'$SNAP')), ${pre}${GOAL}, halt."
  echo "    goal:   $goalstr"

  void=""
  for i in $(seq "$n"); do
    stray=$(swipl_pids | wc -l)
    if [ "$stray" -ne 0 ]; then
      echo "    *** VOID(coresident): $stray stray swipl process(es) before row $i — STOP AND ASK ***"
      swipl_pids | sed 's/^/      stray pid /'
      void="coresident"; break
    fi

    # two-sided matcher control, PRESENT half — control TAGs ONLY, never a counted row
    mpid=""
    case "$tag" in
      ctl_*) ( sleep $((TMO-3)); row_swipl_pids > "$RAW/$tag/$i.matcher" 2>/dev/null ) & mpid=$! ;;
    esac

    before=$(ls /tmp/core.* 2>/dev/null | sort)
    t0=$(date +%s%3N)
    o=$(timeout -k "$KILL_AFTER" "$TMO" $WRAP $SWIPL -l stack.pl $mods \
          -g "$goalstr" 2> "$RAW/$tag/$i.stderr")
    rc=$?
    t1=$(date +%s%3N)
    after=$(ls /tmp/core.* 2>/dev/null | sort)

    [ -n "$mpid" ] && wait "$mpid" 2>/dev/null
    rowpid="na"
    if [ -s "$RAW/$tag/$i.matcher" ]; then rowpid=$(tr '\n' ',' < "$RAW/$tag/$i.matcher" | sed 's/,$//'); fi

    # cores: PRIMARY collector is the set difference around the row (F3 — core_pattern's
    # %e is the crashing process's comm at crash time, so the core.swipl.<pid> NAME is
    # confirmation, not the key).
    if [ "$rc" -ge 128 ]; then
      new=$(comm -13 <(printf '%s\n' "$before") <(printf '%s\n' "$after") | sed '/^$/d')
      if [ -n "$new" ]; then
        printf '%s\n' "$new" | while read -r cf; do
          echo "      row $i: core $cf size=$(stat -c%s "$cf") -> $RAW/$tag/"
          mv "$cf" "$RAW/$tag/" 2>/dev/null
        done
      else
        echo "      row $i: rc=$rc, NO new /tmp/core.* in [$t0,$t1]"
      fi
    fi

    # rss_kb is `na` for Set R: there is no watcher under arm A by construction (R3=(b)).
    printf 'i=%d\trc=%d\tbytes=%d\twall_ms=%d\trss_kb=%s\tstray=%d\tpid=%s\n' \
      "$i" "$rc" "${#o}" "$((t1-t0))" "na" "$stray" "$rowpid" >> "$RAW/$tag.tsv"

    # matcher ABSENT half: nothing may survive `timeout -k`
    surv=$(swipl_pids | wc -l)
    if [ "$surv" -ne 0 ]; then
      echo "    *** VOID(survivor): $surv swipl process(es) survived timeout -k after row $i — killed; STOP AND ASK ***"
      swipl_pids | while read -r p; do echo "      killing $p"; kill -9 "$p" 2>/dev/null; done
      void="survivor"; break
    fi
  done

  fp1=$(corpus_fp)
  echo "    census(end):"; census | sed 's/^/      /'
  echo "    dmesg(killed process):"
  dmesg -T 2>&1 | grep -i 'killed process' | tail | sed 's/^/      /'
  if [ "$fp0" != "$fp1" ]; then
    echo "    *** VOID(fingerprint): snapshot fingerprint moved mid-arm ($fp0 -> $fp1) ***"
    void="${void:-fingerprint}"
  else
    echo "    snapshot fp stable across the arm: $fp0"
  fi
  [ -n "$void" ] && echo "    *** ARM VOID — cause: $void ***"
  echo "    rows: $(wc -l < "$RAW/$tag.tsv")  raw: $RAW/$tag.tsv"
  echo "    (counts are re-derived from the TSV with the pinned expressions in"
  echo "     audit_log.md Step 0.3 — this driver prints NO failure summary by design)"
}

for arm in "${@:-A B C D}"; do
  case "$arm" in
    A) run_arm "ARM A  baseline (default flags)" \
               "" "-l giant_component_analysis.pl" "$N" A ;;
    B) run_arm "ARM B  gc_thread=false (AGC in the calling thread)" \
               "set_prolog_flag(gc_thread,false), " "-l giant_component_analysis.pl" "$N" B ;;
    C) run_arm "ARM C  agc_margin enormous (AGC effectively never fires)" \
               "set_prolog_flag(agc_margin,1000000000), " "-l giant_component_analysis.pl" "$N" C ;;
    D) run_arm "ARM D  line-buffered stdout (localize the true death point)" \
               "set_stream(user_output,buffer(line)), " "-l giant_component_analysis.pl" "$N" D ;;
    *) echo "unknown arm: $arm" ;;
  esac
done
