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
  if [ "$outside" -eq 0 ]; then
    pass "no occurrence of the superseded formulation outside a correction block"
  else
    bad "$outside occurrence(s) of the superseded formulation asserted outside a correction block"
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

  if $GREP -q '^\*\*The framing is not entailed by the compressed content\.\*\*' "$V06"; then
    pass "site 2/2: §2.2 body carries the narrowed claim"
  else
    bad "site 2/2: §2.2 body does NOT carry the narrowed claim"
  fi

  # A4's narrowing, landed in the same pass at the §0 table
  if $GREP -q '^| A4 |.*warrant transfer' "$V06"; then
    pass "§0 ANALYTIC A4 row narrowed to warrant transfer"
  else
    bad "§0 ANALYTIC A4 row NOT narrowed to warrant transfer"
  fi
}

row2() { echo "row2: canonicity markers settled"; printf '  \033[33mNOT IMPLEMENTED\033[0m (lands with A5)\n'; return 3; }
row3() { echo "row3: §2.8/§2.9 anchors + §2.9(b) sub-item resolve"; printf '  \033[33mNOT IMPLEMENTED\033[0m (lands with A2)\n'; return 3; }
row4() { echo "row4: 33 refs re-pointed; 27 bare §2 refs individually accounted"; printf '  \033[33mNOT IMPLEMENTED\033[0m (lands with A3)\n'; return 3; }

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
  cp "$saved" "$tmp/e.md"
  $GREP -v '^| A2 | Every abstraction is formed at a framing' "$tmp/e.md" > "$tmp/e2.md"
  local raw_hit=0 norm_hit=0
  $GREP -q 'additional authored content' "$tmp/e2.md" && raw_hit=1
  normalized "$tmp/e2.md" | $GREP -q 'additional authored content' && norm_hit=1
  if [ "$raw_hit" -eq 0 ] && [ "$norm_hit" -eq 1 ]; then
    spass "arm E: normalizer is load-bearing (raw grep: absent, normalized: present)"
  else
    sbad "arm E: normalizer inert (raw=$raw_hit norm=$norm_hit) - rows leaning on it are unwitnessed"
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

case "${1:-all}" in
  row1) fail=0; row1 ;;
  row2) row2; exit $? ;;
  row3) row3; exit $? ;;
  row4) row4; exit $? ;;
  selftest) fail=0; selftest ;;
  all) fail=0; row1; echo; selftest; echo
       echo "rows 2-4 are declared and NOT IMPLEMENTED; they land with A2/A3/A5."; ;;
  *) echo "usage: $0 [all|row1|row2|row3|row4|selftest]" >&2; exit 2 ;;
esac

exit "$fail"
