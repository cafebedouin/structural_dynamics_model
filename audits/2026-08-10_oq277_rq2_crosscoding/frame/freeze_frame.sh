#!/usr/bin/env bash
# freeze_frame.sh — OQ-277 Phase 1, step 1. Regenerate and FREEZE the sampling frame.
#
# Run from the repository root. Deterministic: same repo state in, same files out.
# The listing is frozen because `audits/` grows (175 dirs on 2026-08-10 will be 176+
# tomorrow) — a sample drawn against "the audit dirs" is meaningless without the
# population it was drawn from pinned as an artifact.
#
# The census command below is Appendix B's §4.5 row VERBATIM (v0.3, discharged
# 2026-08-10). The path prefix is load-bearing: run from `audits/` instead of the
# repository root, `cut -d/ -f2` extracts unique FILENAMES rather than directories —
# that exact unit error produced the 77/175 figure v0.3 corrected to 73/175.
set -euo pipefail

OUT="audits/2026-08-10_oq277_rq2_crosscoding/frame"
cd "$(git rev-parse --show-toplevel)"

# --- population: every audit directory, sorted, one per line ---------------------
#
# SELF-EXCLUSION (pre-sample rule, stated before the seed is applied). This
# experiment's OWN directory is removed from the population. It is dated today, so it
# is in the frame; it sits in the non-census stratum, which is exactly the stratum the
# escape check samples from. Drawing it would (a) ask the coder to code the experiment
# that is coding it, and (b) guarantee an H2 leak — this directory's WRITEUP.md carries
# the full P-lexicon AND Wu's name, so the payload could never pass its own leak-grep.
# The exclusion is one named directory, applied to the population before any sampling,
# and is recorded in the manifest as a count so it can never be silent.
SELF="2026-08-10_oq277_rq2_crosscoding"
ls -d audits/*/ | sed 's#^audits/##; s#/$##' | sort > "$OUT/all_dirs_raw.txt"
grep -vx "$SELF" "$OUT/all_dirs_raw.txt" > "$OUT/all_dirs.txt"

# --- census split: Appendix B §4.5 keyword proxy, VERBATIM ------------------------
grep -rl 'for its whole life\|never fired\|never ran\|read.*0 for\|was never\|silently' \
  --include='*.md' audits/ \
  | cut -d/ -f2 | sort -u > "$OUT/incident_bearing_raw.txt"
grep -vx "$SELF" "$OUT/incident_bearing_raw.txt" > "$OUT/incident_bearing_dirs.txt"

comm -23 "$OUT/all_dirs.txt" "$OUT/incident_bearing_dirs.txt" > "$OUT/non_census_dirs.txt"

# --- manifest --------------------------------------------------------------------
{
  echo "# OQ-277 sampling frame — FROZEN"
  echo "as_of_local_date: 2026-08-10"
  echo "repo_commit: $(git rev-parse HEAD)"
  echo "repo_dirty: $(test -n "$(git status --porcelain)" && echo yes || echo no)"
  echo "n_dirs_on_disk: $(wc -l < "$OUT/all_dirs_raw.txt")"
  echo "n_self_excluded: $(( $(wc -l < "$OUT/all_dirs_raw.txt") - $(wc -l < "$OUT/all_dirs.txt") ))  # want exactly 1 ($SELF)"
  echo "n_all_dirs: $(wc -l < "$OUT/all_dirs.txt")"
  echo "n_incident_bearing: $(wc -l < "$OUT/incident_bearing_dirs.txt")"
  echo "n_non_census: $(wc -l < "$OUT/non_census_dirs.txt")"
  echo
  echo "# integrity: the two strata must partition the population exactly"
  echo "partition_check: $(( $(wc -l < "$OUT/incident_bearing_dirs.txt") + $(wc -l < "$OUT/non_census_dirs.txt") )) == $(wc -l < "$OUT/all_dirs.txt")"
  echo
  echo "# md5 of each frozen listing (these are what the sampler reads)"
  md5sum "$OUT/all_dirs.txt" "$OUT/incident_bearing_dirs.txt" "$OUT/non_census_dirs.txt"
} > "$OUT/frame_manifest.txt"

cat "$OUT/frame_manifest.txt"
