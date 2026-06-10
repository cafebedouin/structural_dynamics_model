# OQ-81 wire-up — reading-typed upstream suppression (operator-ruled 2026-06-10)

**Ruling:** SUPPRESS. A reading's verdict is precisely the contested object; the CSR line in
`axis_source_desc` already delivers both readings' commitments verdict-free to every
supplementary-axis prompt (K ≈ N in the A/B proves the substrate path is live); the verdict
line's only measured effect is pulling theater_ratio toward one seat's archived conclusion.
Keep = measured distortion for unmeasured benefit.

## Change

- `agent/generate_kernel_corpus.py` `_flat_seeds_from_manifest`: reading-typed deps (csr
  reading_ids ∪ kernel-tagged generation_sequence claim_ids) are dropped from BOTH the
  seed's `downstream_of` (wave ordering) and the axis copy handed to `_seed_messages`
  (injection reads `axis["downstream_of"]` in `upstream_context`, story_generator_base.py:221
  — two read sites, one filter point). Suppression prints one line per affected axis.
  `upstream_context()` itself is untouched — flat §5.1 injection is not implicated by the
  A/B and is preserved.
- `agent/c-orchestrator.py` `_step_generate_serial`: same predicate, same suppression, kept
  in sync so the legacy serial escape hatch does not re-inject what the backend suppresses.

**Scope claimed:** unified backend (payload-witnessed below) + serial escape hatch
(predicate-synced by code-read, NOT payload-witnessed — graduation step: a serial-mode
capture, if the deletable-later hatch ever matters). Legacy app orchestrators
(`agent/orchestrator.py`, `agent/uke_narrative_orchestrator.py`, Streamlit-era, flat-only)
untouched and out of scope. Kernel-CONCEPT-typed deps (current SCOPE format; 21/21 dangling
per RECON F2) stay inert BY DESIGN post-ruling: the substrate they would deliver already
arrives via CSR — no injection handler is owed.

## Deterministic payload witness (pre-change baselines captured BEFORE the edit)

Inputs in `wireup/`: `witness_germline.manifest.json` (flat, 8 flat-to-flat dep edges,
5 waves — the flat-injection positive control) and `witness_dutch_plus_supp.manifest.json`
(the archived dutch kernel manifest + the OQ-cited `infrastructure_trust_paradox` axis
appended to generation_sequence — same construction as the merge's P4 synthetic witness).
Harness: `python/audits/capture_generation_payloads_unified.py` (fixed fake client; pipeline
transport, which also discharges the A/B's direct-API caveat for the regression).

- **Flat control:** `cmp baseline_germline.json post_germline.json` → **byte-identical**
  (all 8 flat verdict injections preserved exactly).
- **Kernel witness** (`diff_dutchsupp.txt`, keyed per constraint since wave membership is
  an expected change): seed set identical (3 readings + flat control + supplementary axis);
  4/5 payloads byte-identical; `infrastructure_trust_paradox` moved wave 2→1 and its payload
  diff is exactly the removed block:
  ```
  -UPSTREAM CONSTRAINT: husk_reading
  -  claimed_type: mountain
  -  affects_constraint: husk_reading → infrastructure_trust_paradox
  ```
  (claimed_type `mountain` = the canned story's value; plus the ctx block's blank lines).
  Nothing else perturbed; request params identical throughout.
- Baseline positive controls (witnessed before editing): the pre-change dutch capture
  contains the verdict line (injection fires); germline baseline contains 8 injection sites.

Both py_compile clean. No live generation was run for this wire-up; live corpus untouched.
