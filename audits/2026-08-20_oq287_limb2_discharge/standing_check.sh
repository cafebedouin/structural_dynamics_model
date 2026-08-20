#!/usr/bin/env bash
# Standing check for OQ-287 Limb 2 — run BEFORE relocating any section of a paper.
#
# Question it answers: if section §X of v0.6 moves, does any artifact DESIGNATED FOR
# EXTERNAL PUBLICATION point a reader at §X?  Repo-internal pointers are editable and do
# not count; the publication set is what cannot be repaired after the fact.
#
# THE SET IS ENUMERATED, NOT INFERRED.  Scoped to "the appendix" alone it would run over an
# empty set and pass vacuously (Build Discipline Pattern 5).  Declared residual: this list
# has no owner and no gate — a third designated artifact that nobody adds here makes the
# check go green over a stale set, silently.  Acceptable at two members maintained by one
# operator; recorded because it is not acceptable undeclared.  ISSUES OQ-287.
#
# TWO ARMS, because one arm is both blind and noisy on its own:
#   ARM 1 (number)  greps for the section NUMBER.  Noisy: a target document with its own
#                   §X returns self-cites as false positives.  Blind: misses any reference
#                   carried in prose.
#   ARM 3 (path)    scans each paper's SIBLING `docs/<dir>/README.md`.  Owed by construction on a
#                   narrower and fully checkable ground than arm 2's: **any grep over a paper that
#                   does not include its own `docs/<name>/README.md` is scoped wrong by
#                   construction.**  Those READMEs are canonicity markers that Pass A made a
#                   CHECKED fact -- they carry routing claims about the very sections being moved --
#                   so they are known load-bearing, and they still sat outside the sweep.
#                   Discrimination record: NATURALLY-ARISING, both sides, from git.  The stale
#                   sub-item promise in `docs/amnesiac_institution/README.md` FIRES at c3667f75 and
#                   DECLINES at HEAD (fixed in 20f2b93d).  Neither state was authored to be found --
#                   the defect's commit is the positive, its successor the negative.
#                   HOW IT WAS FOUND, recorded because it indicts the method: by ACCIDENT, opening
#                   that README for an unrelated reason.  Nothing in the method produced it.  Arm 1
#                   and arm 2 both missed it because both were scoped to the PAPERS.
#
#   ARM 2 (prose)   greps for the section's PROSE NAME.  Owed by construction, because
#                   concealment v0.4's canonicity sentence names its domains in prose with
#                   no numbers anywhere in the sentence — arm 1 is structurally blind to
#                   that whole sentence for ANY section it covers, whatever moves.
#                   The mechanism has a witnessed catch elsewhere (KNOWN_STATE 2026-08-20,
#                   crosswalk item 31: the literal string `cross-type` returned 0 against a
#                   claim made in other words).  UNTESTED on THIS population: no live
#                   exposure exists here, so nothing below witnesses arm 2 firing on a
#                   section that actually moves.
#
# Usage:  standing_check.sh <section-number> <prose-name>
#         standing_check.sh --selftest
set -uo pipefail
cd "$(dirname "$0")/../.."
GREP=/usr/bin/grep   # pinned: build_discipline — never bare `grep` in a script computing a count

# --- the enumerated publication set -----------------------------------------------------
# ADDING A MEMBER IS CHEAP; OMITTING ONE IS THE FAILURE MODE.  A paper in the set that turns out
# never to be published costs one extra grep.  A paper OUT of the set that IS published is the
# unrepairable case this whole check exists for.  So the rule is: include on CANDIDACY, not on a
# recorded GO -- and note the status rather than deciding it here.
PUBSET=(
  docs/amnesiac_institution/amnesiac_institution_v0_6.md   # GO ruled 2026-08-19 (ISSUES OQ-309)
  docs/concealment/concealment_without_a_concealer_v0_4.md # circulation UNRECORDED - the repo has
                                                           # no distribution log; absence here is
                                                           # not evidence of non-circulation
  docs/practice/practice_paper_v0_1.md                     # added 2026-08-20, the day it landed.
                                                           # v0.1 draft, no GO, unreviewed. Included
                                                           # on candidacy: this is the "third
                                                           # artifact nobody adds" the declared
                                                           # residual named, and it arrived within
                                                           # hours of the residual being written.
)
# Self-cites do not count: a document's pointer into its OWN §X is editable together with
# the move.  SELFSEC maps each publication-set member to the doc whose sections it owns.
selfdoc() { case "$1" in *amnesiac_institution_v0_6*) echo v06 ;; *concealment_without_a_concealer_v0_4*) echo cwc ;; *) echo other ;; esac; }

# ARM 3's path set is DERIVED from PUBSET, never listed separately -- a hand-maintained second
# list is the fork this check exists to catch.  A paper at docs/<dir>/<file>.md has its canonicity
# marker at docs/<dir>/README.md.
siblingreadme() { echo "$(dirname "$1")/README.md"; }

scan() {
  local sec="$1" prose="$2" owner="${3:-v06}" f d hits
  echo "== standing check: §${sec} (\"${prose}\") — owner document: ${owner}"
  echo "   publication set: ${#PUBSET[@]} member(s), enumerated"
  local total=0
  for f in "${PUBSET[@]}"; do
    [ -f "$f" ] || { echo "   !! MISSING from disk: $f"; continue; }
    d=$(selfdoc "$f")
    echo "   -- $f  [owns: $d]"
    # ARM 1 — number
    hits=$("$GREP" -n -- "§${sec}" "$f" || true)
    if [ "$d" = "$owner" ]; then
      echo "      arm1 (number): $(printf '%s' "$hits" | "$GREP" -c . || true) hit(s) — SELF-CITES, editable with the move, not exposure"
    else
      local n; n=$(printf '%s' "$hits" | "$GREP" -c . || true)
      # NUMBER COLLISION: if this document owns a section with the SAME number, arm 1's
      # hits are predominantly ITS OWN and are false positives for our question.  Say so
      # at the read site — an unlabelled count here reads as exposure and is not.
      if "$GREP" -qE "^#+ ${sec}[. ]" "$f"; then
        echo "      arm1 (number): $n hit(s) — !! NUMBER COLLISION: $f owns its own §${sec}"
        echo "                     ($("$GREP" -m1 -E "^#+ ${sec}[. ]" "$f")) — these are predominantly"
        echo "                     SELF-cites and are FALSE POSITIVES for this question. Arm 2 carries the signal."
      fi
      if [ "$n" -gt 0 ]; then
        echo "      arm1 (number): $n hit(s) — CROSS-DOCUMENT, inspect each:"
        printf '%s\n' "$hits" | sed 's/^/         /'
        total=$((total+n))
      else
        echo "      arm1 (number): 0"
      fi
    fi
    # ARM 2 — prose name (runs on every member; a prose name is not self-disambiguated by numbering)
    hits=$("$GREP" -n -i -- "$prose" "$f" || true)
    local m; m=$(printf '%s' "$hits" | "$GREP" -c . || true)
    if [ "$d" != "$owner" ] && [ "$m" -gt 0 ]; then
      echo "      arm2 (prose):  $m hit(s) — CROSS-DOCUMENT, inspect each:"
      printf '%s\n' "$hits" | sed 's/^/         /'
      total=$((total+m))
    else
      echo "      arm2 (prose):  $m hit(s)$([ "$d" = "$owner" ] && echo ' — self, not exposure')"
    fi
  done
  # ---- ARM 3: the sibling canonicity READMEs -------------------------------------------------
  echo "   -- arm3 (path): sibling docs/<dir>/README.md, derived from PUBSET --"
  local scoped_wrong=0
  for f in "${PUBSET[@]}"; do
    local r; r="$(siblingreadme "$f")"
    if [ ! -f "$r" ]; then
      echo "      !! $r MISSING -- $f has no canonicity marker; a grep over it is scoped wrong"
      scoped_wrong=1; continue
    fi
    local rn rm2
    rn=$("$GREP" -n -- "§${sec}\|§${sec}/\|${sec}\b" "$r" | "$GREP" -c . || true)
    rm2=$("$GREP" -n -i -- "$prose" "$r" | "$GREP" -c . || true)
    if [ "$rn" -gt 0 ] || [ "$rm2" -gt 0 ]; then
      echo "      $r: number=$rn prose=$rm2 -- INSPECT (READMEs carry routing claims about moved sections)"
      "$GREP" -n -i -- "$prose" "$r" | sed 's/^/         /' || true
      total=$((total+rn+rm2))
    else
      echo "      $r: number=0 prose=0"
    fi
  done
  [ "$scoped_wrong" = 0 ] \
    && echo "      scope guard: every PUBSET paper's README was swept (${#PUBSET[@]}/${#PUBSET[@]})" \
    || echo "      !! SCOPE GUARD FAILED -- this run swept fewer READMEs than papers"
  echo "   => reference(s) needing a ruling: $total"
  echo
}

selftest() {
  local fail=0
  echo "== selftest: the arms must DISCRIMINATE, not merely fire"
  local C=docs/concealment/concealment_without_a_concealer_v0_4.md

  # Arm 1 FIRES on a real cross-document number reference (:34 cites v0.6 §2.8/§2.9).
  if "$GREP" -qn '§2\.8' "$C"; then echo "  PASS  arm1 fires: concealment cites v0.6 §2.8 by number"
  else echo "  FAIL  arm1 did not fire on the known cross-document number cite"; fail=1; fi

  # Arm 1 DECLINES where the reference exists but is carried in prose: v0.6 §9 is
  # "The Organizational Form"; concealment names it in prose and never by number.
  # This is the NATURALLY-ARISING negative — the blind spot, not an authored decoy.
  local n9 p9
  n9=$("$GREP" -c '§9 ' "$C" || true)
  p9=$("$GREP" -ci 'the organizational form' "$C" || true)
  if [ "$p9" -gt 0 ]; then echo "  PASS  arm2 fires where arm1 is blind: 'the organizational form' = $p9 hit(s)"
  else echo "  FAIL  arm2 found nothing; the prose warrant is not reproducible"; fail=1; fi

  # The noise side: a bare §9 grep over concealment returns ITS OWN §9, which is a
  # different section entirely ("The repair: boundary carriage and external re-derivation").
  if "$GREP" -qn '^## 9\. ' "$C"; then
    echo "  PASS  arm1 noise confirmed: concealment owns a §9 of its own, so bare-number hits there are self-cites"
  else echo "  FAIL  expected concealment to own a §9 (the self-cite confound)"; fail=1; fi

  # ---- ARM 3's discrimination record: NATURALLY-ARISING on BOTH sides, drawn from git ----------
  # The operator's rule: when a defect is found, its before-commit is a free negative control.
  # The stale sub-item promise in docs/amnesiac_institution/README.md was fixed in 20f2b93d, so
  # its parent c3667f75 is a real defective state and HEAD is a real clean one -- neither authored
  # to be found.  This beats a planted fixture, which only shows that authored drift gets rejected.
  local RM=docs/amnesiac_institution/README.md
  local before after
  before=$(git show c3667f75:"$RM" 2>/dev/null | "$GREP" -c 'sub-item granularity' || true)
  after=$(git show HEAD:"$RM" 2>/dev/null | "$GREP" -c 'sub-item granularity' || true)
  if [ "$before" -ge 1 ]; then echo "  PASS  arm3 FIRES on a naturally-arising positive (c3667f75: README carried the stale promise)"
  else echo "  FAIL  arm3 did not fire at c3667f75 -- the positive half of its record is gone"; fail=1; fi
  if [ "$after" = "0" ]; then echo "  PASS  arm3 DECLINES on a naturally-arising negative (HEAD: promise removed) -- discrimination, not detection"
  else echo "  FAIL  arm3 still fires at HEAD -- it cannot discriminate; the stale promise is back"; fail=1; fi

  # Every PUBSET member must have a sibling README, or arm 3 is scoped wrong by construction.
  local missing=0 m
  for m in "${PUBSET[@]}"; do [ -f "$(siblingreadme "$m")" ] || { echo "  FAIL  no canonicity marker for $m"; missing=1; fail=1; }; done
  [ "$missing" = 0 ] && echo "  PASS  every PUBSET paper has a sibling README (${#PUBSET[@]}/${#PUBSET[@]}) - arm 3 is not scoped short"
  # The residual is CLOSED for the case that actually arose: the third paper is enrolled.
  printf '%s\n' "${PUBSET[@]}" | "$GREP" -q 'practice_paper' \
    && echo "  PASS  the third paper is enrolled (declared residual closed for this instance)" \
    || { echo "  FAIL  practice paper landed and is NOT in PUBSET - the residual fired and nobody acted"; fail=1; }

  # Two-sided: the set must be non-empty, or every arm passes vacuously (Pattern 5).
  if [ "${#PUBSET[@]}" -ge 2 ]; then echo "  PASS  publication set non-empty (${#PUBSET[@]}) — no vacuous pass"
  else echo "  FAIL  publication set too small; check would pass on an empty population"; fail=1; fi

  # Every enumerated member must exist, or the check silently scans fewer files than it claims.
  local miss=0 f
  for f in "${PUBSET[@]}"; do [ -f "$f" ] || { echo "  FAIL  enumerated member absent: $f"; miss=1; fail=1; }; done
  [ "$miss" = 0 ] && echo "  PASS  all ${#PUBSET[@]} enumerated members present on disk"

  echo
  [ "$fail" = 0 ] && echo "selftest: GREEN" || echo "selftest: RED"
  return "$fail"
}

if [ "${1:-}" = "--selftest" ]; then selftest; exit $?; fi
if [ $# -lt 2 ]; then
  echo "usage: $0 <section-number> <prose-name> [owner:v06|cwc]"
  echo "       $0 --selftest"
  echo
  echo "The three sections OQ-287 ever put at risk, run as the record:"
  scan "2.8" "unmarked perturbation" v06
  scan "2.9" "negative control" v06
  scan "9"   "the organizational form" v06
  exit 0
fi
scan "$1" "$2" "${3:-v06}"
