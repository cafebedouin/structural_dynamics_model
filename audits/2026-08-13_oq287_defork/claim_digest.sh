#!/usr/bin/env bash
# THE definition of a CWC claim digest. Not a description of one.
#
# WHY A SCRIPT AND NOT A PROSE RECIPE: the recipe was first written as prose ("strip, collapse
# whitespace, sha256, first 8") and implemented twice from that prose within one turn. The two
# implementations disagreed - one piped grep's output (trailing newline included), the other used
# printf '%s' (no trailing newline) - and sha256 is not indifferent to that. Every digest in the
# A2 draft was wrong by that one byte. A prose recipe restated at each call site is Pattern 2 with
# a hash function attached: two canonical things, no queryable fact of which is right. This file
# is the fact. claim_cite_check MUST shell out to it or import it; it must not reimplement it.
#
# NORMALISATION, pinned:
#   1. select the row from Appendix A only (labels are ambiguous elsewhere in the document)
#   2. strip leading and trailing whitespace
#   3. collapse internal whitespace runs to a single space
#   4. NO trailing newline  <- the byte the two implementations differed on
#   5. sha256, first 8 hex
#
# FAILS LOUD on an unknown label. It must never return a digest for a row that does not exist:
# sha256 of empty input is a perfectly well-formed value (e3b0c442...), so an absent row would
# otherwise render as a present digest - the exact absence-presents-as-presence shape this whole
# citation scheme exists to guard, committed by the guard. Witnessed: the first version of the
# comparison harness reported a brand-new C1 row as "MOVED" for precisely this reason.
#
# Usage:  ./claim_digest.sh <label> [file]      -> "<8 hex>"
#         ./claim_digest.sh --all  [file]       -> "<label> <digest>" per line
#         ./claim_digest.sh --selftest
# Exit:   0 ok | 2 usage | 4 label not found | 5 label found more than once

set -uo pipefail

HERE="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
DEFAULT_CWC="$HERE/../../docs/concealment/concealment_without_a_concealer_v0_4.md"
GREP=/usr/bin/grep

appendix_a() { sed -n '/^## Appendix A/,/^## Appendix B/p' "$1"; }

digest_of() {                      # $1 = label, $2 = file
  local label="$1" file="$2" rows n
  rows="$(appendix_a "$file" | $GREP "^| ${label} |")"
  n="$(printf '%s' "$rows" | $GREP -c . )"
  if [ -z "$rows" ]; then
    printf 'claim_digest: label %s not found in Appendix A of %s\n' "$label" "$file" >&2
    return 4
  fi
  if [ "$n" -gt 1 ]; then
    printf 'claim_digest: label %s matches %s rows in %s - ambiguous, refusing\n' "$label" "$n" "$file" >&2
    return 5
  fi
  printf '%s' "$rows" \
    | sed 's/^[[:space:]]*//; s/[[:space:]]*$//; s/[[:space:]][[:space:]]*/ /g' \
    | sha256sum | cut -c1-8
}

all_labels() { appendix_a "$1" | $GREP -oE '^\| [AECP][0-9]+ \|' | tr -d '| '; }

selftest() {
  local rc fail=0 tmp; tmp="$(mktemp -d)"; trap 'rm -rf "$tmp"' RETURN
  local f="$DEFAULT_CWC"
  p() { printf '  \033[32mPASS\033[0m  %s\n' "$1"; }
  b() { printf '  \033[31mFAIL\033[0m  %s\n' "$1"; fail=1; }

  # 1 - a real label yields 8 hex
  if digest_of A2 "$f" | $GREP -qE '^[0-9a-f]{8}$'; then p "real label yields 8 hex"; else b "real label did not yield 8 hex"; fi

  # 2 - an ABSENT label fails loud rather than returning the empty-string hash.
  #     This is the control for the defect that motivated the script.
  digest_of ZZ99 "$f" >"$tmp/o" 2>/dev/null; rc=$?
  if [ "$rc" -eq 4 ] && [ ! -s "$tmp/o" ]; then
    p "absent label exits 4 and prints nothing (no empty-string digest)"
  else
    b "absent label returned rc=$rc output='$(cat "$tmp/o")' - an absence rendered as a value"
  fi
  # 2b - and specifically: it must not be the empty-string sha256 prefix
  if [ "$(cat "$tmp/o")" = "e3b0c442" ]; then b "absent label returned the empty-string hash e3b0c442"; else p "absent label is not e3b0c442"; fi

  # 3 - trailing-newline invariance is PINNED, not incidental: the digest of the row must equal
  #     the digest computed with an explicit no-trailing-newline pipeline, and must NOT equal the
  #     one computed with a trailing newline. If those two ever coincide the normalisation is inert.
  local row d_pinned d_nl
  row="$(appendix_a "$f" | $GREP '^| A2 |' | sed 's/^[[:space:]]*//; s/[[:space:]]*$//; s/[[:space:]][[:space:]]*/ /g')"
  d_pinned="$(printf '%s' "$row" | sha256sum | cut -c1-8)"
  d_nl="$(printf '%s\n' "$row" | sha256sum | cut -c1-8)"
  [ "$(digest_of A2 "$f")" = "$d_pinned" ] && p "digest matches the no-trailing-newline form" \
                                           || b "digest does NOT match the pinned normalisation"
  [ "$d_pinned" != "$d_nl" ] && p "trailing newline changes the digest (normalisation is load-bearing)" \
                             || b "trailing newline does not change the digest - normalisation inert"

  # 4 - content change under an unchanged label MOVES the digest. This is the A4-narrowing case,
  #     and the whole reason digests exist rather than label resolution.
  cp "$f" "$tmp/m.md"
  sed -i 's/^| A2 | Framing non-identifiability: the framing/| A2 | Framing NON-IDENTIFIABILITY: the framing/' "$tmp/m.md"
  [ "$(digest_of A2 "$tmp/m.md")" != "$(digest_of A2 "$f")" ] \
    && p "content change under an unchanged label moves the digest" \
    || b "content change did NOT move the digest - the checker would pass silently"

  # 5 - a duplicated label is refused rather than silently taking the first
  cp "$f" "$tmp/d.md"
  appendix_a "$f" | $GREP '^| A2 |' >> "$tmp/d.md"   # lands after Appendix B; still inside no range
  sed -i '/^## Appendix B/i | A2 | duplicate planted by selftest | none |' "$tmp/d.md"
  digest_of A2 "$tmp/d.md" >/dev/null 2>&1; rc=$?
  [ "$rc" -eq 5 ] && p "duplicated label refused (exit 5)" || b "duplicated label not refused (rc=$rc)"

  return $fail
}

case "${1:-}" in
  --selftest) selftest ;;
  --all) f="${2:-$DEFAULT_CWC}"; for l in $(all_labels "$f"); do printf '%s %s\n' "$l" "$(digest_of "$l" "$f")"; done ;;
  "" | -h | --help) echo "usage: $0 <label> [file] | --all [file] | --selftest" >&2; exit 2 ;;
  *) digest_of "$1" "${2:-$DEFAULT_CWC}" ;;
esac
