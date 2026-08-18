# PREREGISTRATION — pilot conversion of the two confirmed-shape predicates

Frozen BEFORE any write to engine files; md5 logged in audit_log.md above the first
Phase-3/4 result line. Recon (RECON.md, committed a76c21dd) already established a
non-empty live disagreement list (311 rows on `testsets/`), so the outcome arms below
are pre-stated with that known.

## The pilot transformation

1. `drl_core.pl` `classify_from_metrics/6`: rewrite the 9 atom-headed clauses to
   fresh-variable heads with `!, Type = <atom>` after each cut; terminal clause 10
   likewise (`Type = unknown`, no cut needed — last clause). Clause BODIES and ORDER
   unchanged. The variable-headed clause-1 Supp guard is already the immune shape —
   unchanged.
2. `signature_detection.pl` `constraint_signature/2`: same transformation on the 6 lock
   clauses; `classify_by_signature/3` likewise for its atom-headed clauses, KEEPING the
   terminal `ambiguous` catch-all (removing it is a semantics change outside this pass).

## Witness set

- Six clean-vs-edited same-session `classify_corpus` pairs: five legs
  (testsets/testsets_haiku/testsets_flash/testsets_kimi/testsets_sonnet, each with its
  `expected_model`) + `archives/datasets/kernel_v1` (`expected_model=None`). Serialized;
  md5 fingerprint of each leg dir around both halves; diff at `per_constraint`
  (manifest timestamp normalized); exit 0 AND output mtime advanced required before any
  "identical" is trusted.
- `python/golden_file_check.py` for the `dr_type/3` vector.
- `check_stack.pl` vs KNOWN_STATE baseline; `load_warning_gate` on the load chain.
- New plunit unit `dispatch_bound_call`: RED at pre-fix HEAD, GREEN at fix commit.
- Checker 4th discrimination run: declines on post-fix `classify_from_metrics/6` and
  `constraint_signature/2`.
- Timing: both halves of all six pairs timed (paired, same corpus, same session) + one
  full `run_pipeline.py` pre/post pair as the unpaired sanity number.

## Outcome semantics (stated before the run)

- **Zero-diff on all legs** ⇒ *output-preserving on the witness set,
  semantics-changing by construction*. The transformation permanently changes what a
  disagreeing bound call does (lie → fail) for every caller forever; zero diff means no
  witnessed disagreement REACHED per_constraint output on these corpora — NOT unchanged
  semantics, and NOT in tension with the 311 recon rows (those witness the is_X surface;
  per_constraint is built from `dr_type`, which routes through unbound
  `metric_based_type_indexed`). Commit message carries exactly the label
  "output-preserving on the witness set, semantics-changing by construction";
  **"behavior-preserving" is forbidden wording** for this commit.
- **Non-zero diff** ⇒ the diff rows ARE the defect witness (manufactured
  classifications had been reaching recorded output). Split into (a) the fix commit —
  output-changing, lands FIRST, diff enumerated per row — and (b) follow-on doc/test
  commits, per the output-changing-vs-behavior-preserving commit rule. Consumer
  enumeration per WRITEUP plan (the fcr_ablation shape, OQ-298 precedent).
- **A `dispatch_bound_call` unit that cannot go RED at pre-fix HEAD** ⇒ the unit is
  re-derived until it reproduces the over-acceptance (a unit green at both ends
  witnesses nothing; the RED half is the point).
- **Timing regression that is material** (a paired-leg slowdown well beyond pair noise)
  ⇒ a WRITEUP finding and a class-B rollout input — never a silent revert.

## Kill condition

If the transformation changes any **unbound** caller's result anywhere (six-leg diff
rows attributable to an unbound call path, golden-file failure on the dr_type vector
not explained by a bound-caller surface, or a `check_stack` regression) — STOP and
report: that falsifies the "mechanical transformation" premise. By construction a
fresh-variable head + unify-after-cut is answer-preserving for unbound calls; a
violated construction means the edit was not the transformation.

## Scope guards

- `bound_selector_check.py` registry UNCHANGED this pass.
- No engine edits beyond the two predicates named above.
- The epistemic_access_check:577 and cs_verdict repairs (RECON §4 finds) are NOT part
  of this pilot — proposed as follow-ons; folding them in would contaminate the
  pilot's diff attribution.
