# OQ-44 policy close — fail-closed-on-absence ruled; the two remaining engine sites dispositioned

**Date:** 2026-06-11. **Substrate:** live corpus 48 testsets / 46 stories, branch `oq44-close`
from `a4297632`. **Ruling text:** ISSUES.md OQ-44 (still-operative block). **Evidence base for the
ruling:** the gate census in `audits/2026-06-11_oq46_backed_reconciliation/` (probe1, per-process
positive controls).

## The ruling (summary; authoritative text in the OQ-44 entry)

Extracted from converged practice — five fail-closed conversions on real data, none reverted —
with the instance-counter condition satisfied as confirmation, not ground. **Statute** for
new/modified gates (fail-closed on absence; `unknown`/OPEN on empty; pass carries its witness);
**absence-to-provenance carve-out** (only positive-control inference at authoring/compile time,
the `suppression_profile` precedent — never emptiness-inference at the consumption site);
**common-law** per-instance for existing gates, prioritized by success-shapedness. OQ-43 closes
with the same stroke, its fifth instance dispositioned below.

## Commit B — `8b5a34b8` `has_viable_alternatives` fail-close (output-changing, landed alone)

Default `false`→`unknown`; `false` now requires authored evidence. Pre-registered: `natural_law`
count 1→0. Witnessed (`evidence/commitB_pipeline_diff.txt`, full pre/post JSONs alongside):

- `thermal_dissipation_constraint`: signature `natural_law` → `ambiguous` (the honest fallback
  cell). **Consequence reported faithfully:** the NL signature was the only thing holding all four
  perspectives at `mountain` (modal override); un-certified, `moderate`/`institutional` surface as
  `rope` and the diagnostic verdict flips green→red with a `perspectival_incoherence`
  informational alert. This is the previously-masked disagreement becoming visible — the
  un-certification the operator accepted in the ruling.
- All other 276 leaf diffs are corpus-relative metrics (`wasserstein_*`, `arakelov_height`,
  `signature_pressure`, distribution/monotonicity/contextuality tables) that move with the corpus
  signature distribution. Single cause, fully attributed.
- The `HasAlternatives == true` consumers (coordination_scaffold, successful_coordination) are
  unchanged — they never fired on the empty table and still do not.

## Commit C — `966d53c8` `get_raw_suppression` sentinel + guard (+ schema nullable)

`Value = 0` on absence → `unknown` sentinel; leading `number/1` guard clause at
`classify_from_metrics/6` fails closed (never throws, never computes on fabrication).

**Pre-derivation corrected by the witness.** The earlier claim "the fabricated 0 is never
consumed on the live corpus" was scoped to the classification path and wrong for the export
layer: the first pipeline run failed loudly at the enrich schema gate ("suppression is null but
required") — proving the two non-story `cs_axiom_contradiction` files had been exporting the
fabricated 0 all along, and each carried a `fingerprint_voids` diagnostic agreement computed on
it. Fix on the consumer side: `shared/schemas.py` marks `suppression` nullable (null = no
authored scalar; expected only for non-story files — null on a story constraint is an authoring
defect). Downstream already None-guards (`maxent.py:213`, `loader.py:110`).

Witnesses: `evidence/probe_c_sentinel_witness_output.txt` (absent → `unknown`;
`classify_from_metrics` on the sentinel fails cleanly; numeric positive control still classifies
`snare`); `evidence/commitC_pipeline_diff.txt` — 7 diffs = 3 manifest + 2× `suppression` 0→null +
2× `agreements` losing the fabrication-backed `fingerprint_voids` entry. Classification output
unchanged for all 46 stories; pipeline exit 0, no schema failures.

## Disposition 2 (no commit) — report-layer 0.0 defaults

`report_generator.pl:481/:500/:507` + `utils` print `MISSING (using default 0.0)` — the pass
already carries its witness; CONFORMING as-is. Instance #2's mark-dependent-verdicts-conditional
caveat stands.

## Evidence index

| File | Witnesses |
|---|---|
| `probe_c_sentinel_witness*` | sentinel on absence; fail-closed guard; numeric positive control |
| `commitB_pipeline_diff.txt` | 277 diffs, single-cause attribution (thermal un-certification) |
| `commitC_pipeline_diff.txt` | 7 diffs: honest nulls + fabrication-backed agreements dropping |
| `pipeline_output_pre.json` / `pipeline_output_post.json` | full A (pre-B, `a4297632`) and Z (post-C) outputs |
