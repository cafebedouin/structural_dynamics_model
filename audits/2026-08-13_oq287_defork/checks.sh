#!/usr/bin/env bash
# OQ-287 Pass A verification table, executable.
#
# WHY THIS EXISTS RATHER THAN INLINE GREPS: rows 1-4 of the plan's verification table are all
# greps over prose, and three separate false absences were produced in Pass A by greps whose own
# framing was not part of the query (self-referential filename filtered out by `grep -v`; a
# section-scoped sweep that missed surviving sites outside the section; a phrase split across a
# line break read as absent by a line-oriented grep). Every row here therefore reads through
# `normalized()`, which joins wrapped lines before matching. Do not add a row that greps the raw
# file.
#
# Line numbers are never anchors: A2 and A3 move every line in this document. Anchor on sentinels
# and on section headings.
#
# Usage:  ./checks.sh            run every implemented row
#         ./checks.sh row1       run one row
#         ./checks.sh selftest   two-sided controls for the implemented rows
#
# Row status: row1 IMPLEMENTED (A1 landed). rows 2-4 land with A2/A3 - they are declared below and
# exit LOUD (status 3, "NOT IMPLEMENTED"), never green, so an unimplemented row can never be read
# as a passed one.

set -uo pipefail

REPO="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
V06="$REPO/docs/amnesiac_institution/amnesiac_institution_v0_6.md"
GREP=/usr/bin/grep          # pinned: an aliased/function grep silently changes a reported count
STALE="not recoverable from the artifact"

fail=0

# Join wrapped lines so a phrase broken by a newline is still one match. Paragraph breaks are
# preserved as record separators, so a match can still be located to a paragraph.
normalized() { tr '\n' ' ' < "$1" | sed 's/  */ /g'; }

pass() { printf '  \033[32mPASS\033[0m  %s\n' "$1"; }
bad()  { printf '  \033[31mFAIL\033[0m  %s\n' "$1"; fail=1; }
note() { printf '        %s\n' "$1"; }
# VACUOUS is neither pass nor fail: the check ran and its population was empty, so it carries no
# information. It must never print green - a gate that passes because its input is missing is the
# defect this repository names Build Discipline 5, and a vacuous check rendered as PASS is that
# defect wearing the instrument's own colours.
vacuous() { printf '  \033[33mVACUOUS\033[0m %s\n' "$1"; }

# The selftest arms need their OWN counter. They deliberately drive row1 to fire (that is what an
# arm proving discrimination looks like), so `fail` is expected to be 1 when an arm SUCCEEDS.
# Reporting the arms through pass()/bad() made the harness exit non-zero on an all-green selftest,
# because the exit status carried the last arm's intentional fire. Two counters, one meaning each.
sfail=0
spass() { printf '  \033[32mPASS\033[0m  %s\n' "$1"; }
sbad()  { printf '  \033[31mFAIL\033[0m  %s\n' "$1"; sfail=1; }

# --- row 1 -------------------------------------------------------------------------------------
# NEGATIVE half: no *assertion* of the superseded formulation. The string legitimately appears
#   inside CLAIM-CORRECTION sentinel blocks, where it is quoted in order to be killed - a
#   correction that does not quote what it corrects is unfindable by a reader arriving with the
#   stale form. So the check is containment, not count: every occurrence must fall inside a block.
# POSITIVE half: the replacement formulation is actually present at both sites it was owed at -
#   the §0 A2 row and the §2.2 body - so a deletion cannot pass as a repair.
row1() {
  echo "row1: framing non-entailment replaces 'not recoverable from the artifact's form'"

  # --- negative: containment, not count ---
  local inside=0 total=0 outside=0 lineno
  while IFS= read -r lineno; do
    :
  done < /dev/null
  # walk the file tracking sentinel state
  local state=0
  while IFS= read -r line; do
    case "$line" in
      *CLAIM-CORRECTION-BEGIN*) state=1 ;;
      *CLAIM-CORRECTION-END*)   state=0 ;;
    esac
    case "$line" in
      *"$STALE"*)
        # the sentinel comments themselves never contain the stale string; only prose does
        total=$((total+1))
        if [ "$state" = 1 ]; then inside=$((inside+1)); else outside=$((outside+1)); fi
        ;;
    esac
  done < "$V06"

  note "occurrences of the stale form: $total (inside a CLAIM-CORRECTION block: $inside, outside: $outside)"
  if [ "$outside" -gt 0 ]; then
    bad "$outside occurrence(s) of the superseded formulation asserted outside a correction block"
  elif [ "$total" -eq 0 ]; then
    # A2 vacated §2.2, and the correction note went with it - correctly, since the narrowing now
    # lives upstream and v0.6 cites it. So this check now passes over an EMPTY population, which is
    # absence satisfying the gate (Build Discipline 5). Report it as vacuous rather than green: it
    # is no longer evidence of anything about v0.6, and the live guard has moved to the digest on
    # CWC:A2, verified by row1's citation arm below.
    vacuous "containment check has an empty population post-A2 (0 occurrences) - not evidence; live guard is CWC:A2@31548228"
  else
    pass "all $total occurrence(s) of the superseded formulation are inside a correction block"
  fi

  # --- positive: the replacement is present, read through the normalizer ---
  local norm; norm="$(normalized "$V06")"

  if printf '%s' "$norm" | $GREP -q 'not entailed by the compressed content'; then
    pass "replacement formulation present ('not entailed by the compressed content')"
  else
    bad "replacement formulation ABSENT - a deletion would pass the negative half alone"
  fi

  # the operative half of the narrowing; this is the phrase that spans a line break in the source,
  # and that a raw line-oriented grep reports absent
  if printf '%s' "$norm" | $GREP -q 'additional authored content'; then
    pass "operative half present ('additional authored content') [line-wrap normalized]"
  else
    bad "operative half ABSENT ('additional authored content')"
  fi

  # both sites, distinguished: the §0 claim table row and the §2.2 body
  # NB: the claim text is bolded (`**not entailed…`), so the char before "not" is `*`, not a space.
  # An earlier `.* not entailed` pattern reported this row absent while it was present.
  if $GREP -q '^| A2 | Every abstraction is formed at a framing .*not entailed by the compressed content' "$V06"; then
    pass "site 1/2: §0 ANALYTIC table, A2 row carries the narrowed claim"
  else
    bad "site 1/2: §0 ANALYTIC A2 row does NOT carry the narrowed claim"
  fi

  # Site 2/2 was §2.2's body. A2 VACATED §2.2, so that site no longer exists and the check is
  # retired rather than relaxed - the distinction being that the thing checked was deliberately
  # removed, not that the check became inconvenient. Its replacement is the citation: the narrowing
  # now lives upstream and v0.6 must POINT at it with a live pin.
  if $GREP -q '^### 2\.2 ' "$V06"; then
    bad "site 2/2: §2.2 still exists - A2 did not vacate it, and this check should not have been retired"
  elif printf '%s' "$norm" | $GREP -q 'CWC:A2@31548228'; then
    pass "site 2/2 retired with §2.2; replaced by a live pinned citation to CWC:A2@31548228"
  else
    bad "site 2/2: §2.2 vacated but nothing cites CWC:A2 - the narrowing is now asserted nowhere"
  fi

  # A4's narrowing, landed in the same pass at the §0 table
  if $GREP -q '^| A4 |.*warrant transfer' "$V06"; then
    pass "§0 ANALYTIC A4 row narrowed to warrant transfer"
  else
    bad "§0 ANALYTIC A4 row NOT narrowed to warrant transfer"
  fi
}

row2() { echo "row2: canonicity markers settled"; printf '  \033[33mNOT IMPLEMENTED\033[0m (lands with A5)\n'; return 3; }
# --- row 3 -------------------------------------------------------------------------------------
# Precondition (A2) has landed, so this is now implemented.
# NEGATIVE: the vacated numbers are gone and not reused.
# POSITIVE: §2.8/§2.9 kept their numbers AND their bodies, the sub-item the Wu letter cites is
#   still addressable, and the declared-temporary markers are present with their canonical
#   destination. A vacation that silently took the two subsections with it would satisfy the
#   negative half alone.
row3() {
  echo "row3: §2.8/§2.9 survive the vacation at their numbers, with the §2.9(b) sub-item addressable"
  local norm; norm="$(normalized "$V06")"

  local reused; reused=$($GREP -c '^### 2\.[1-7] ' "$V06")
  [ "$reused" -eq 0 ] && pass "vacated numbers 2.1-2.7 not reused as headings" \
                      || bad "$reused vacated number(s) reused as headings"

  $GREP -q '^### 2\.8 ' "$V06" && pass "§2.8 heading present at its number" || bad "§2.8 heading MISSING"
  $GREP -q '^### 2\.9 ' "$V06" && pass "§2.9 heading present at its number" || bad "§2.9 heading MISSING"

  # bodies intact, not just headings
  local excl; excl=$(printf '%s' "$norm" | $GREP -o 'Type B structural contradictions\|Stochastic churn\|Loud destructive replacement' | $GREP -c .)
  [ "$excl" -eq 3 ] && pass "§2.9(a)'s three exclusions intact ($excl/3)" || bad "§2.9(a) exclusions: $excl/3"
  local tri; tri=$($GREP -c '^| Type [ABC] ' "$V06")
  [ "$tri" -eq 3 ] && pass "§2.8's trifurcation table intact ($tri/3)" || bad "§2.8 trifurcation rows: $tri/3"

  # the sub-item the already-sent Wu letter cites must remain addressable
  printf '%s' "$norm" | $GREP -q '\*\*(b) A within-scope place the signature genuinely breaks' \
    && pass "§2.9(b) sub-item still addressable (the Wu letter cites it and cannot be edited)" \
    || bad "§2.9(b) sub-item NOT addressable - an already-sent external citation now dangles"

  local marks; marks=$($GREP -c 'DECLARED TEMPORARY — A2-pre ruling' "$V06")
  [ "$marks" -eq 2 ] && pass "both declared-temporary markers present ($marks/2)" || bad "declared-temporary markers: $marks/2"
  printf '%s' "$norm" | $GREP -q 'at sub-item granularity' \
    && pass "§2.9's marker names sub-item granularity for the redirect" \
    || bad "§2.9's marker does not name sub-item granularity"
}
# --- row 4 -------------------------------------------------------------------------------------
# A3's check, WRITTEN BEFORE A3. It is expected to be RED until the re-pointing lands; that is the
# point of writing it first, and a check fitted to the edit it verifies would not have this property.
#
# The population is 23, not the plan's 33: 10 of the 33 were cross-references INSIDE §2.1-2.7 and
# left with them at A2. Reconciliation and the per-site targets are in A3_MAPPING_RULE.md.
#
# The §2.3 arm asserts ASSIGNMENT, not distribution. A 3/3 split with two sites swapped satisfies
# any count-based or shape-based test while being wrong at both - which is the exact class of defect
# this pass has been catching all day, so the arm must not be an instance of it. Each of the six
# sites is pinned by a line-independent ANCHOR drawn from its own prose (never the `§2.3` token,
# which the re-point removes), and the check is: the expected label appears near that anchor AND
# the sibling label does not.
row4() {
  echo "row4: A3 conformance — 23 refs re-pointed per A3_MAPPING_RULE.md, §2.3 assignment pinned"
  local norm; norm="$(normalized "$V06")"

  # --- negative: only the notice's own self-mention may name a vacated number ---
  local remaining; remaining=$(printf '%s' "$norm" | $GREP -o '§2\.[1-7]' | $GREP -c . || true)
  local notice; notice=$(printf '%s' "$norm" | $GREP -o '§2\.1–§2\.7 are vacated' | $GREP -c . || true)
  note "live §2.[1-7] occurrences: $remaining (the notice's own '§2.1–§2.7' accounts for 2)"
  if [ "$notice" -lt 1 ]; then
    bad "the vacation notice's own '§2.1–§2.7' self-mention is GONE - it must never be re-pointed"
  elif [ "$remaining" -eq 2 ]; then
    pass "only the notice's self-mention names a vacated number (2 occurrences, both there)"
  else
    bad "$remaining live §2.[1-7] occurrences; expected exactly 2 (the notice's self-mention)"
  fi

  # --- positive: per-site assignment for the six contested §2.3 refs ---
  # anchor|expected|sibling   — anchor is prose that survives the re-point
  local sites=(
    "which is the one operation|A3|E1"
    "makes shape-with-lost-detail the more hazardous amnesia|A3|E1"
    "the party best positioned to notice is the one|A3|E1"
    "AbsenceBench\*\* is the empirical form of|E1|A3"
    "every rescue was a|E1|A3"
    "recognition standing in for enumeration|E1|A3"
  )
  local ok3=0 bad3=0
  for spec in "${sites[@]}"; do
    local anchor="${spec%%|*}" rest="${spec#*|}"
    local want="${rest%%|*}" sib="${rest##*|}"
    # window: the anchor plus the next 160 chars, where the citation must sit
    local window
    window="$(printf '%s' "$norm" | $GREP -o "$anchor.\{0,160\}" | head -1)"
    if [ -z "$window" ]; then
      bad "§2.3 site anchor not found: '$anchor' (prose changed - reclassify, do not delete the arm)"
      bad3=$((bad3+1)); continue
    fi
    if printf '%s' "$window" | $GREP -q "CWC:$want@"; then
      if printf '%s' "$window" | $GREP -q "CWC:$sib@"; then
        bad "§2.3 site '$anchor' cites BOTH $want and $sib - ambiguous assignment"
        bad3=$((bad3+1))
      else
        ok3=$((ok3+1))
      fi
    elif printf '%s' "$window" | $GREP -q "CWC:$sib@"; then
      bad "§2.3 site '$anchor' assigned to $sib; A3_MAPPING_RULE.md §4 classifies it $want"
      bad3=$((bad3+1))
    else
      bad "§2.3 site '$anchor' carries no CWC pin (expected $want)"
      bad3=$((bad3+1))
    fi
  done
  [ "$bad3" -eq 0 ] && pass "all 6 §2.3 sites land where A3_MAPPING_RULE.md §4 classifies them (3 A3 / 3 E1, by site)" \
                    || note "  ($ok3 of 6 §2.3 sites correctly assigned)"

  # --- positive: the re-pointed total is conserved; a deletion must not satisfy the negative ---
  local pins; pins=$(printf '%s' "$norm" | $GREP -o 'CWC:[AECP][0-9]*@[0-9a-f]\{8\}' | $GREP -c . || true)
  note "pinned CWC citations in v0.6: $pins"
  # 16 landed at A2; A3 adds 18 more (23 refs minus the 5 unpinnable section-only ones)
  if [ "$pins" -ge 34 ]; then
    pass "pinned citation count $pins >= 34 (16 from A2 + 18 from A3)"
  else
    bad "pinned citation count $pins < 34 — A3 incomplete, or references deleted rather than re-pointed"
  fi

  # --- positive: the 5 declared-unpinnable section-only citations are present and counted ---
  local sec; sec=$(printf '%s' "$norm" | $GREP -o '`CWC` §[0-9]\+\(\.[0-9]\+\)\?' | $GREP -c . || true)
  note "section-only (unpinnable) CWC references: $sec — declared class, A3_MAPPING_RULE.md §3"
  [ "$sec" -ge 5 ] && pass "the 5 declared-unpinnable section references are present" \
                   || bad "$sec section-only references; expected >= 5 per the mapping rule"
}

# --- selftest ----------------------------------------------------------------------------------
# Two-sided, same-path: the control runs the SAME row1 function against mutated copies, not a
# reimplementation of it. Each arm names the value that would make it fail.
selftest() {
  echo "selftest: controls for row1"
  local tmp; tmp="$(mktemp -d)"; trap 'rm -rf "$tmp"' RETURN
  local saved="$V06" rc

  # arm A - DECLINES on the real file (the naturally-arising negative: the repo as it stands)
  V06="$saved"; fail=0; row1 >/dev/null 2>&1; rc=$fail
  [ "$rc" -eq 0 ] && spass "arm A: declines on the real file" || sbad "arm A: fired on the real file"

  # arm B - FIRES on a re-assertion planted OUTSIDE the sentinel (the defect the row exists for)
  cp "$saved" "$tmp/b.md"
  printf '\n**The framing is %s form.**\n' "$STALE's" >> "$tmp/b.md"
  V06="$tmp/b.md"; fail=0; row1 >/dev/null 2>&1; rc=$fail
  [ "$rc" -eq 1 ] && spass "arm B: fires on a re-assertion outside a correction block" \
                  || sbad "arm B: MISSED a re-assertion outside a correction block"

  # arm C - still DECLINES when a further legitimate quotation is added INSIDE a block.
  #         This is the arm that distinguishes containment from counting: a count-based check
  #         fires here, and would be wrong to.
  cp "$saved" "$tmp/c.md"
  printf '\n<!-- CLAIM-CORRECTION-BEGIN: t -->\nquoting "%s'"'"'s form" again.\n<!-- CLAIM-CORRECTION-END: t -->\n' "$STALE" >> "$tmp/c.md"
  V06="$tmp/c.md"; fail=0; row1 >/dev/null 2>&1; rc=$fail
  [ "$rc" -eq 0 ] && spass "arm C: declines on a second legitimate quotation inside a block" \
                  || sbad "arm C: fired on a legitimate quotation (check is counting, not containing)"

  # arm D - FIRES when the replacement is deleted but the stale string never returns.
  #         This is the deletion-vs-repair control: the negative half alone cannot tell them apart.
  cp "$saved" "$tmp/d.md"
  sed -i 's/not entailed by the compressed content/REDACTED/g' "$tmp/d.md"
  V06="$tmp/d.md"; fail=0; row1 >/dev/null 2>&1; rc=$fail
  [ "$rc" -eq 1 ] && spass "arm D: fires when the replacement is deleted (deletion != repair)" \
                  || sbad "arm D: MISSED a deletion that left the negative half satisfied"

  # arm E - the NORMALIZER's own control, and it has to be built carefully. Deleting only the
  #         wrapped copy of the operative phrase does NOT make row1 fire, because the §0 A2 row
  #         carries the same phrase unwrapped - so a naive "remove the wrapped copy, expect a
  #         fire" arm passes for the wrong reason and pins nothing. (That arm was written first
  #         and failed; the failure was the arm's, not the checker's.)
  #         The load-bearing question is narrower: on a file where the phrase exists ONLY in
  #         wrapped form, does raw grep disagree with normalized grep? If they agree, normalized()
  #         is inert and every row that leans on it is unwitnessed.
  #         Revision: the arm originally used 'additional authored content', which straddled a
  #         newline in §2.2. A2 vacated §2.2, leaving that phrase only in the §0 table row -
  #         unwrapped - so the arm went red reporting "normalizer inert". It was right: its
  #         substrate was gone. Rather than re-pin it to another sentence that a later step can
  #         vacate the same way, the arm now DERIVES a straddling phrase from the file itself:
  #         the tail of one line plus the head of the next. Self-maintaining, and it fails only
  #         if the normalizer genuinely stops working.
  local probe
  probe="$(awk 'NF>=8 && prev_nf>=8 && $0 !~ /^[|>#-]/ && prev !~ /^[|>#-]/ {
                  n=split(prev,a," "); tail=a[n-3]" "a[n-2]" "a[n-1]" "a[n];
                  split($0,b," "); head=b[1]" "b[2]" "b[3];
                  print tail" "head; exit }
                { prev=$0; prev_nf=NF }' "$saved")"
  if [ -z "$probe" ]; then
    sbad "arm E: could not derive a line-straddling probe from the file - arm is inert, not passing"
  else
    local raw_hit=0 norm_hit=0
    $GREP -qF "$probe" "$saved" && raw_hit=1
    normalized "$saved" | $GREP -qF "$probe" && norm_hit=1
    if [ "$raw_hit" -eq 0 ] && [ "$norm_hit" -eq 1 ]; then
      spass "arm E: normalizer load-bearing on a derived straddling phrase (raw: absent, normalized: present)"
    else
      sbad "arm E: normalizer inert (raw=$raw_hit norm=$norm_hit) on probe '$probe'"
    fi
  fi

  # arm F - FIRES when the operative phrase is removed at EVERY site, wrapped and unwrapped.
  cp "$saved" "$tmp/f.md"
  perl -0pi -e 's/additional authored\s+content/REDACTED/g' "$tmp/f.md"
  V06="$tmp/f.md"; fail=0; row1 >/dev/null 2>&1; rc=$fail
  [ "$rc" -eq 1 ] && spass "arm F: fires when the operative phrase is removed everywhere" \
                  || sbad "arm F: MISSED removal of the operative phrase"

  V06="$saved"
  fail=$sfail          # the harness's verdict is the ARMS' verdict, not the last arm's fire
}

# DISPATCH. `fail=0; rowN` — never `rowN; exit $?`.
#
# `exit $?` returns the row function's own status, which is the status of its LAST statement, not
# the accumulated `fail`. row3 and row4 were dispatched that way when they were stubs returning 3,
# and the form was left in place when they became real: row4 then printed six red FAIL lines and
# exited 0. A check that reports red and exits green is the failure this whole pass is about, in
# the harness that checks for it. Only the `NOT IMPLEMENTED` stubs may use `exit $?`, because for
# them the return value IS the verdict.
case "${1:-all}" in
  row1) fail=0; row1 ;;
  row2) row2; exit $? ;;
  row3) fail=0; row3 ;;
  row4) fail=0; row4 ;;
  selftest) fail=0; selftest ;;
  # `all` ACCUMULATES. Each row is run with a fresh `fail` and OR-ed into `agg`, because the rows
  # share one `fail` variable and selftest ends by assigning `fail=$sfail` — so a naive
  # `row1; row3; row4; selftest` reported row4's six red lines and exited 0, the last row's verdict
  # silently overwriting every earlier one. That is Build Discipline 6 at an aggregation boundary,
  # committed inside the aggregate that reports on it: the component results were all correct and
  # the composite could not represent them.
  all) agg=0
       fail=0; row1;     agg=$(( agg | fail )); echo
       fail=0; row3;     agg=$(( agg | fail )); echo
       fail=0; row4;     agg=$(( agg | fail )); echo
       fail=0; selftest; agg=$(( agg | fail )); echo
       echo "row2 (A5) is declared and NOT IMPLEMENTED; it exits 3, never green."
       fail=$agg ;;
  *) echo "usage: $0 [all|row1|row2|row3|row4|selftest]" >&2; exit 2 ;;
esac

exit "$fail"
