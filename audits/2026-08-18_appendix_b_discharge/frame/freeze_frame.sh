#!/usr/bin/env bash
# freeze_frame.sh — OQ-309 Appendix B discharge. Regenerate and FREEZE the census frame.
#
# DERIVED from audits/2026-08-10_oq277_rq2_crosscoding/frame/freeze_frame.sh (2026-08-10).
# Four changes, each deliberate:
#   (1) OUT re-pointed to this audit dir. The original hardcodes its own output path and
#       MUST NOT be re-run in place — doing so would overwrite a frozen 2026-08-10 artifact.
#   (2) SELF-EXCLUSION extended to BOTH arc dirs. The 08-10 dir carries the full P-lexicon
#       and the 08-18 dir (this one) carries it too, via the crosswalk and this script's own
#       comments. The rationale is CONTAMINATION, not hygiene: both would enter the numerator
#       for language about the census rather than about an audited defect.
#   (3) /usr/bin/grep pinned everywhere. The interactive shell carries a `grep` FUNCTION whose
#       output differs by one path component from the binary's; the census pipes grep into a
#       POSITIONAL parse (cut -d/ -f2), so that shift silently moves the extracted field from
#       directory to filename. This is the 77/175 unit error's mechanism.
#       (build_discipline.md: pin /usr/bin/grep in any script computing a reported count.)
#   (4) The DENOMINATOR INCLUSION RULE is computed BOTH WAYS — the current filename-glob
#       membership and the queued-for-v0.4 rule from ISSUES.md:11313-11318, "a directory counts
#       iff it contains at least one file". Both are emitted so the delta between them is
#       attributable before either is allowed into a published row.
set -euo pipefail

OUT="audits/2026-08-18_appendix_b_discharge/frame"
cd "$(git rev-parse --show-toplevel)"

SELF_A="2026-08-10_oq277_rq2_crosscoding"
SELF_B="2026-08-18_appendix_b_discharge"

# --- population, rule GLOB: every directory under audits/, as the paper's row counts them ---
ls -d audits/*/ | sed 's#^audits/##; s#/$##' | sort > "$OUT/all_dirs_raw.txt"
/usr/bin/grep -vxE "$SELF_A|$SELF_B" "$OUT/all_dirs_raw.txt" > "$OUT/all_dirs_glob.txt"

# --- population, rule NONEMPTY: a directory counts iff it contains at least one file --------
# (ISSUES.md:11313-11318, queued for v0.4 and never landed. "at least one file" is read as
#  at least one regular file at ANY depth — an audit dir whose content is all in a subdir is
#  still an audit dir. The stricter top-level-only reading is emitted too, so the choice of
#  reading is visible rather than assumed.)
: > "$OUT/all_dirs_nonempty.txt"
: > "$OUT/all_dirs_nonempty_toplevel.txt"
while read -r d; do
  [ -n "$(find "audits/$d" -type f -print -quit)" ] && echo "$d" >> "$OUT/all_dirs_nonempty.txt"
  [ -n "$(find "audits/$d" -maxdepth 1 -type f -print -quit)" ] && echo "$d" >> "$OUT/all_dirs_nonempty_toplevel.txt"
done < "$OUT/all_dirs_glob.txt"

# --- census split: Appendix B's §5.4 keyword proxy, VERBATIM but with grep pinned -----------
/usr/bin/grep -rl 'for its whole life\|never fired\|never ran\|read.*0 for\|was never\|silently' \
  --include='*.md' audits/ \
  | cut -d/ -f2 | sort -u > "$OUT/incident_bearing_raw.txt"
/usr/bin/grep -vxE "$SELF_A|$SELF_B" "$OUT/incident_bearing_raw.txt" > "$OUT/incident_bearing_dirs.txt"

comm -23 "$OUT/all_dirs_glob.txt" "$OUT/incident_bearing_dirs.txt" > "$OUT/non_census_dirs.txt"

# a numerator member that is NOT in the nonempty population would be a contradiction
# (it has a .md, so it has a file) — asserted rather than assumed.
comm -13 "$OUT/all_dirs_nonempty.txt" "$OUT/incident_bearing_dirs.txt" > "$OUT/numerator_outside_nonempty.txt"

N_GLOB=$(wc -l < "$OUT/all_dirs_glob.txt")
N_NE=$(wc -l < "$OUT/all_dirs_nonempty.txt")
N_NE_TOP=$(wc -l < "$OUT/all_dirs_nonempty_toplevel.txt")
N_INC=$(wc -l < "$OUT/incident_bearing_dirs.txt")

{
  echo "# OQ-309 census frame — FROZEN"
  echo "as_of_local_date: 2026-08-18"
  echo "repo_commit: $(git rev-parse HEAD)"
  echo "repo_dirty: $(test -n "$(git status --porcelain)" && echo yes || echo no)"
  echo "grep_binary: $(/usr/bin/grep --version | head -1)"
  echo
  echo "n_dirs_on_disk: $(wc -l < "$OUT/all_dirs_raw.txt")"
  echo "n_self_excluded: $(( $(wc -l < "$OUT/all_dirs_raw.txt") - N_GLOB ))  # want exactly 2 ($SELF_A, $SELF_B)"
  echo
  echo "## denominator, computed BOTH ways"
  echo "n_denominator_rule_glob:              $N_GLOB   # every dir under audits/"
  echo "n_denominator_rule_nonempty_anydepth: $N_NE   # >=1 regular file at any depth"
  echo "n_denominator_rule_nonempty_toplevel: $N_NE_TOP   # >=1 regular file at top level"
  echo "delta_glob_minus_nonempty:            $(( N_GLOB - N_NE ))"
  echo
  echo "## numerator (unchanged by the denominator rule)"
  echo "n_incident_bearing: $N_INC"
  echo "numerator_outside_nonempty_population: $(wc -l < "$OUT/numerator_outside_nonempty.txt")  # want 0"
  echo
  echo "## incidence, both ways"
  echo "incidence_rule_glob:     $N_INC / $N_GLOB = $(python3 -c "print(f'{$N_INC/$N_GLOB:.4f}')")"
  echo "incidence_rule_nonempty: $N_INC / $N_NE = $(python3 -c "print(f'{$N_INC/$N_NE:.4f}')")"
  echo
  echo "## integrity: the two strata must partition the glob population exactly"
  echo "partition_check: $(( N_INC + $(wc -l < "$OUT/non_census_dirs.txt") )) == $N_GLOB"
  echo
  echo "# md5 of each frozen listing"
  md5sum "$OUT/all_dirs_glob.txt" "$OUT/all_dirs_nonempty.txt" "$OUT/incident_bearing_dirs.txt" "$OUT/non_census_dirs.txt"
} > "$OUT/frame_manifest.txt"

cat "$OUT/frame_manifest.txt"
