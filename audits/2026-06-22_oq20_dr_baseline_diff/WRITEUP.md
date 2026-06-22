# OQ-20 — Did CS-era *code* perturb DR output? Baseline code/data diff audit

**Date:** 2026-06-22 · **Tag baseline:** `v3-dev-baseline` = `3e75f90b` (2026-02-24, 707
commits behind HEAD `b32989a9`) · **Method:** corpus-fixed / code-varied (Arm 1) and
code-fixed / data-varied (Arm 2), per the staged plan. All findings cite captured run output
in `outputs/oq20/` (cells) and `outputs/oq20/reports/analysis.json` (per-field tables).

## Headline verdicts

- **Arm 1 (CS-era code effect on DR): PERTURBED.** CS-era code *did* change DR output — but
  with a sharp internal structure: the **core type classification is byte-stable**, while the
  **signature layer, MaxEnt distribution, and several auxiliary/added fields changed**, and the
  per-constraint **`id` was relabeled** (enumeration source swap).
- **Arm 2 (CS-era data effect on DR / Theorem-7 detection-independence): MOSTLY INERT, ONE
  DESIGNED COUPLING.** Stripping all `cs_*` facts leaves the entire DR *observer* core unchanged
  (claimed_type, χ/ε, signature, MaxEnt, classifications, purity scalars) — **except
  `contamination_network`**, which changes because `cs_reading_relation` feeds its `explicit`
  edges *by design* (`drl_purity_network.pl`), propagating even to cs-free neighbours.

Both arms ran with an **empty noise floor** (every cell byte-identical across repeats;
`mask_n = 0`), so every reported diff is a real code/data effect, not non-determinism.

---

## Arm 1 — CS-era code effect (cross-code on pre-CS corpora)

Cells (each a fresh cold-cache `swipl` process, `run_json_report`):
- **A** = tag code / `original_json` (1151) · **B** = HEAD code / `original_json`
- **C** = tag code / `original_v6_csfree` (3373) · **D** = HEAD code / `original_v6_csfree`

Within-corpus across-code (A↔B, C↔D) is the literal OQ-20 answer. Corpus hashes:
`corpus_hashes.json`.

### Finding A1 — the per-constraint `id` is relabeled (PERTURBED on `id`), attributed to commit `801390a5`

The output `id` field changed for **132 of 1151** stories between tag and HEAD. Mechanism
(traced in code, not inferred): `json_report.pl` swapped its constraint enumeration from
`logical_fingerprint:known_constraint/1` (the in-file `constraint_metric` subject) to
`corpus_loader:corpus_constraint/1` (the **filename base**). The change is commit **`801390a5`**
("fix(export): corpus-registry enumeration + manifest single-writer (output-affecting)"); the
filename-id machinery (`register_corpus_constraint`) landed `1460e873` (2026-06-04). This is a
CS-era, explicitly output-affecting change — **not** the UUID surrogate migration.

Character of the relabeling (the 1150 matched referents reconcile under a filename→internal
re-key with **zero collisions**, so it is a near-bijective relabeling, referent-stable):
- **1150 stories**: same referent, surface id string changed (e.g. `birthday_paradox_collision`
  → filename `…collison`; `cg_israelgaza…` → `CG_IsraelGaza…`).
- **+1 coverage gain at HEAD** (`8k_tv_limit_2026`): the in-file constraint id is a *digit-leading
  atom* (`8k_…`) that throws a Prolog syntax error, so the **tag drops the story entirely**;
  HEAD recovers it from the filename. (Witnessed: `atom_to_term('8k_tv_limit_2026',…)` →
  `Syntax error: Operator expected`; the tag run errors on `8k_tv_limit_2026.pl:81…`.)
- **−1 demo no longer leaks** (`catholic_church_1200`): the engine-demo constraint from
  `constraint_instances.pl` appears in tag output but is excluded at HEAD (filename enumeration
  has no file for it). This is the documented demo-leak that the same commit fixed.

So the id-derivation change is a relabeling **plus two correctness improvements** (digit-leading
recovery, demo exclusion) — stronger than a bland "unchanged".

### Finding A2 — the DR comparison, modulo the relabeling

The id relabeling has a second consequence the audit had to isolate: for the **133** stories whose
filename ≠ internal id, HEAD enumerates the *filename* id and then queries facts under it — but the
facts are authored under the *internal* id — so HEAD computes **null/`unknown` for every DR field**
on those stories. This is reported separately as a relabel-induced null artifact (a real but
mechanical consequence of A1, not an independent reclassification), and excluded from the value
comparison, which runs over the **1017 stories where both engines have facts**.

Over those 1017 clean stories (evidence: `analysis.json::arm1_AB`):

**Byte-stable across CS-era code (zero diffs) — the core classification:**
`claimed_type`, `classifications`, `base_extractiveness`, `suppression`, `theater_ratio`,
`victims`, `beneficiaries`, `topic_domain`, `human_readable`, `emerges_naturally`,
`requires_active_enforcement`, `resistance`, `resolution_strategy`.
Additionally, the **χ/ε/d/f_d metric values inside `perspective_chi` are identical to the digit**
(the field "changes" on all 1017 stories only because HEAD *adds* `f1_d`/`f2_d` sub-keys).

**Changed — added structure (new capability, not reclassification):**
`domain` (tag always `null` → HEAD computes a value), `perspective_chi` (+`f1_d`/`f2_d`),
`coupling` (category & score identical; +`scope_violations`/`power_violations`/`live_index`,
and `boltzmann` newly wired), `gaps` (tag computes a list → HEAD emits `null`: a regression to
investigate).

**Changed — genuine value changes (the substantive code effect):**
- `signature` (870/1017) — the signature-detection layer evolved heavily (e.g. `false_ci_rope`
  → `unknown`, `natural_law` → `unknown`).
- `raw_maxent_probs` / `maxent_probs` (≈1017/1004) — the MaxEnt distribution was recalibrated
  (different probabilities), though the **argmax is mostly stable** (`maxent_top_type` differs
  only 297/1017).
- `diagnostic_verdict` (908), `omegas` (810), `perspectives` (269), `h1_band` (159),
  `purity_score`/`purity_band` (66/46), `drift_events` (42) — downstream of the signature/maxent
  changes and added subsystems.

**Verdict (Arm 1): PERTURBED.** CS-era code changed DR output. The change is concentrated in the
signature layer, the MaxEnt distribution, and added auxiliary fields; the **core type verdict and
the χ/ε power-scaling metrics are unchanged**. Each changed field is a candidate child-OQ
(intentional shared-predicate evolution vs accidental) — see ISSUES.md.

### C↔D robustness replicate (original_v6) — replicates A↔B exactly

Run with a 2-repeat noise floor (A↔B established determinism; reduced reps logged, no silent
truncation; both C reps and both D reps byte-identical, `mask_n = 0`). Cleaner than A↔B in one
respect: original_v6 has **zero** filename≠internal mismatches (`null_artifact_n = 0`), so all
**3373** stories were compared directly (no relabel-null exclusion needed); the only orphan is the
tag-side `catholic_church_1200` demo again.

The **zero-diff stable core is the identical 13-field set** as A↔B
(`claimed_type`, `classifications`, `base_extractiveness`, `suppression`, `theater_ratio`,
`victims`, `beneficiaries`, `topic_domain`, `human_readable`, `emerges_naturally`,
`requires_active_enforcement`, `resistance`, `resolution_strategy`), and the same fields change
(`signature` 3269/3373, `raw_maxent_probs`/`maxent_probs` ≈3373/3368, `coupling`/`perspective_chi`/
`domain`/`gaps` all 3373, etc.). **The code-effect verdict replicates across both pre-CS corpora.**

---

## Arm 2 — CS-era data effect (cs-present vs cs-stripped, HEAD-only on kernel_v1)

Cells (HEAD code both): **E** = `kernel_v1_asis` (1106; 906 cs-bearing + 200 cs-free) ·
**F** = `kernel_v1_stripped` (cs facts removed by `oq20_strip_cs.py`, which passed its own
byte-identical / cs-only-removed / parse controls). Evidence: `analysis.json::arm2_EF`.

**The cs *output* fields change as expected** (they are the cs axis itself going null when its
input is stripped): `cs_pattern`/`cs_pattern_signals`/`cs_instance_count` (906 each),
`cs_reference_frame` (879), `cs_drift_*` (875/875/875/697), `cs_verdicts` (385),
`cs_axiom_foreclosed` (129). These are excluded from the detection-independence test.

**The DR *observer* axis is unchanged by cs-stripping** — `claimed_type`, `perspective_chi`,
`signature`, `maxent_*`, `classifications`, `purity_*` do **not** appear in the E↔F DR diff at
all. Detection-independence (v7 Theorem 7) holds for the core.

**The one coupling: `contamination_network`** changes on **180 stories** (152 cs-bearing + **28
cs-free**). The plan's "200 cs-free files must be byte-identical" negative control therefore
"fails" — but for a *principled* reason, not a strip bug: `contamination_network` is a
network-global field, and **`constraint_neighbors/3` reads `cs_reading_relation` edges and emits
them as `explicit` contamination neighbours** (`drl_purity_network.pl:67,92,257`, with an explicit
source comment that this was an intentional modification). Stripping `cs_reading_relation` removes
those edges, changing the network topology for cs-bearing stories *and* their cs-free neighbours.
The diffs are **semantic** (neighbour set / scalar changes, not list re-ordering — checked: 0/180
were ordering-only). A small tail (`diagnostic_verdict` 2, `verdict_join` 1, `drift_events` 1)
is downstream of cs by design.

**Verdict (Arm 2): detection-independence holds for the DR observer core; one *designed*
cross-axis coupling** routes `cs_reading_relation` into `contamination_network`. This is a
characterized design coupling, **not** an accidental Theorem-7 violation — but whether an
intentional committer→observer edge is consistent with the detection-independence claim is a
design question (new OQ, Ω_C, for operator ruling).

---

## Method integrity / controls (why these results are real, not success-shaped)

- **Instruments positive-controlled before use.** `oq20_strip_cs.py`: 200 cs-free byte-identical
  + cs-only-removed + parse controls all passed. `oq20_dr_diff.py --selftest`: planted-different
  (incl. an id present on one side only) and planted-noisy (mask→INDETERMINATE) controls passed.
- **Re-key is orthogonal to the compared fields** (built from the source `.pl` `constraint_metric`
  subject, never from matching DR outputs) → no circularity; **zero collisions** on both Arm-1
  corpora.
- **The lone re-key orphan was resolved, not waved away** (`8k_tv_limit_2026` = digit-leading-atom
  story the tag cannot emit; `catholic_church_1200` = engine demo). The witness for "clean re-key"
  is the resolved orphan, not the 1150/1151 ratio.
- **Re-key applied before mask construction** so noise-floor self-diffs and cross-code diffs share
  one canonical id-space.
- **Empty noise floor** (`mask_n = 0`, every cell byte-identical across repeats) — every diff is a
  real effect.
- **Load-path equivalence**: tag `testsets/` ≡ archived `original_json` byte-identical (1151 names,
  0 content diffs); both eras enumerate via SWI `expand_file_name` (sorted glob) → identical load
  order by construction.
- **`original_v6` corrected to cs-free** (7 cs-bearing files excluded; the plan's "0% CS" premise
  was false for original_v6) so Arm 1 holds facts truly identical across code.

## Artifacts
- `outputs/oq20/{A,B,C,D,E,F}_*.json` — captured per-cell run outputs (cold cache).
- `outputs/oq20/reports/analysis.json` — per-field diff tables (this writeup's evidence).
- `outputs/oq20/rekey/*.json` — orthogonal filename→internal-id maps.
- `python/audits/oq20_strip_cs.py`, `oq20_dr_diff.py`, `oq20_make_rekey.py`, `oq20_analyze.py`.
- `corpus_hashes.json` — content-hash pins of all four corpora.
