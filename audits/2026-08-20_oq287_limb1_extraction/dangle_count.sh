#!/usr/bin/env bash
# Mechanical half of the acceptance test (EXTRACTION_PROMPT.md §5): after deleting the moved
# §2.8/§2.9 material, how many surviving sentences REFER to what was deleted?  A section that
# "still stands" strands ~none.
#
# TWO-SIDED BY CONSTRUCTION: the same probe runs against the INTACT paper, where every anchor
# must be PRESENT.  That arm is not decoration -- on its first run it reported the exclusion
# anchor ABSENT from the intact paper, because the pattern was written against v0.6's
# capitalisation ("EXCLUDE") and this paper writes it lowercase.  Read only on the test arm,
# that false negative would have looked exactly like the deletion working.  It then caught a
# SECOND probe defect on the same arm: 'At the instrument stratum it does not' straddles a line
# break in the source, so a line-based grep returns 0 on a phrase that is plainly there -- the
# wrap trap.  Anchors are therefore matched against a NEWLINE-NORMALISED copy.  Two probe
# defects, both found by the arm whose only job is to fail loudly when the probe is broken.
GREP=/usr/bin/grep
f="$1"; label="$2"
echo "== $label"
for pat in 'the same move' 'a named axis' 'the three axes' 'Perturb the ' \
           'the negative control' 'cannot certify itself' 'the unification' \
           'the lens discriminates'; do
  n=$("$GREP" -c -i -- "$pat" "$f"); rc=$?
  [ $rc -gt 1 ] && n="ERR"
  printf '   refs to deleted material: %-26s %s\n' "$pat" "$n"
done
# newline-normalised copy: collapses hard wraps so a straddling phrase is findable
norm="$(tr '\n' ' ' < "$f" | sed 's/  */ /g')"
echo "   -- the anchors those references point AT (matched on the NORMALISED text) --"
for probe in 'axis that varied unheld:three-axes table' \
             'must correctly exclude:exclusion control' \
             'At the instrument stratum it does not:the break (b)' \
             'Hold everything fixed but one dimension:the one move'; do
  pat="${probe%%:*}"; name="${probe##*:}"
  if printf '%s' "$norm" | "$GREP" -qi -- "$pat"; then printf '   %-22s PRESENT\n' "$name"
  else printf '   %-22s ABSENT  <-- the references above are DANGLING\n' "$name"; fi
done
