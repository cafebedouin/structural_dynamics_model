#!/usr/bin/env bash
# v04_section7_check.sh — the check the manifest's own self-checks CANNOT perform.
#
# Operator, 2026-08-18: "Your named self-checks can't see this edit. grep -cE '^\| [0-9]+ \|'
# counts item-table rows; the §7 gap blocks aren't in it. The header-vs-§6 read doesn't cover
# §7 either. Both will pass green whether §7 reads 1-6, 1,2,3,4,5,6,5, or 1,2,3,4,5,6,7."
#
# This extracts §7's ordered-list numbers IN ORDER and asserts they are sequential from 1 with
# no repeats and no gaps. Expected count after the 2026-08-18 fold-and-delete: 6.
set -uo pipefail
F="docs/amnesiac_institution/V04_CONSOLIDATION_MANIFEST.md"
EXPECT="${1:-6}"

NUMS=$(/usr/bin/grep -n '^[0-9]\+\. ' "$F" | sed 's/:.*//;s/$//' > /dev/null; \
       /usr/bin/grep '^[0-9]\+\. ' "$F" | sed 's/^\([0-9]\+\)\..*/\1/' | tr '\n' ' ')
echo "section-7 block numbers, in file order: $NUMS"
COUNT=$(echo $NUMS | wc -w)
echo "count: $COUNT (expected $EXPECT)"

EXPECTED_SEQ=$(seq 1 "$COUNT" | tr '\n' ' ')
if [ "$NUMS" = "$EXPECTED_SEQ" ] && [ "$COUNT" = "$EXPECT" ]; then
  echo "PASS — sequential from 1, no repeats, no gaps, count matches."
  exit 0
fi
echo "FAIL — got [$NUMS], want [$EXPECTED_SEQ] at count $EXPECT."
echo "  (a repeated number is the 2026-08-18 defect: a superseding block inserted above the"
echo "   block it supersedes, with the old one left numbered.)"
exit 1
