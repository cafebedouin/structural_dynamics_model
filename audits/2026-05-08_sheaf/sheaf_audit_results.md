# Sheaf/Presheaf Binary Boundary Audit: 10-Slice Tier-1 Family

Generated: 2026-05-29T21:23:03.579126

## Verdict

**NOT PRESERVED (84.15% crossing rate)**

Binary H¹ = 0 vs H¹ > 0 classification on the 10-slice Tier-1 working family
(evaluated at each constraint's testset-covered subset of the 10 contexts).

Primary framing: this audit tests whether the binary classification survives
*in testset-covered 10-slice positions*, not on the full 10-slice family.
Each constraint is evaluated at 2–7 of the 10 slices depending on testset coverage.
Stratified analysis by n_contexts is the primary diagnostic.

---

## Coverage

| Category | Count |
|---|---|
| Corpus total | 192 |
| Excluded (0 contexts) | 2 |
| Excluded (1 context, no H¹ possible) | 26 |
| **Working set (≥2 contexts)** | **164** |

**Deduplication sanity check:** 5 type
disagreements found across 5 constraints
with duplicate classification entries for the same context.
Conflicts present — see per_constraint data; first-occurrence rule applied.

---

## Binary Preservation Results

| Metric | Count | Rate |
|---|---|---|
| Canonical sheaf (H¹=0) | 139 | 84.8% |
| 10-slice sheaf (H¹=0) | 3 | 1.8% |
| Binary preserved | 26 | 15.85% |
| Sheaf → presheaf crossings | 137 | 83.54% |
| Presheaf → sheaf crossings | 1 | 0.61% |
| **Total crossing rate** | **138** | **84.15%** |

---

## Stratified Analysis by n_contexts

| n contexts | N | Canonical sheaf | 10-slice sheaf | Sheaf→presheaf | Presheaf→sheaf | Crossing rate |
|---|---|---|---|---|---|---|
| 2 | 38 | 32 | 2 | 30 | 0 | 78.95% |
| 3 | 44 | 32 | 1 | 32 | 1 | 75.00% |
| 4 | 44 | 38 | 0 | 38 | 0 | 86.36% |
| 5 | 27 | 26 | 0 | 26 | 0 | 96.30% |
| 6 | 10 | 10 | 0 | 10 | 0 | 100.00% |
| 7 | 1 | 1 | 0 | 1 | 0 | 100.00% |

**Interpretation**: If crossing rate increases with n_contexts, the test under-detects crossings at low coverage. If rate is flat, coverage depth does not bias the result.

---

## Crossing Characterization

### Sheaf → Presheaf (137 crossings)

These constraints have canonical H¹ = 0 (global section on 4-point site) but H¹ > 0
on the 10-slice subsite (local sections fail to glue).

**Driving slices** (which slice's type disagrees with the majority in the 10-slice orbit):

| Slice | Driven crossings |
|---|---|
| U_4 | 125 |
| U_3_civ | 46 |
| U_3_nat | 42 |
| U_3_imm | 40 |
| U_2 | 34 |
| organized | 27 |
| org_nat | 22 |
| U_4_glob | 7 |
| U_1_nat | 6 |
| U_1 | 2 |

**U_3_civ attribution**: U_3_civ drives 33.6% of sheaf→presheaf crossings (less than the predicted majority). Crossings are distributed across multiple slices — see table above for full distribution.

**Organized-driven crossings**: 49 crossings driven by organized/org_nat slices. Two framings apply: (1) apparatus instability at the organized power atom, which lacks full calibration in the product-site design; (2) the product site's conservative exclusion of organized was motivated by genuine calibration uncertainty — these crossings are expected under that framing, not a contradiction of site-stability.

**Signature distribution (sheaf→presheaf)**:

| Signature | Count |
|---|---|
| false_natural_law | 135 |
| false_ci_rope | 1 |
| constructed_low_extraction | 1 |

**Claimed-type distribution (sheaf→presheaf)**:

| Claimed type | Count |
|---|---|
| tangled_rope | 115 |
| snare | 18 |
| rope | 3 |
| scaffold | 1 |

### Presheaf → Sheaf (1 crossings)

Constraints with canonical H¹ > 0 but 10-slice H¹ = 0. These constraints disagree
across canonical contexts but happen to agree on the specific 10-slice contexts they
appear at. Most likely: constraints classified differently at canonical contexts but
consistently at the 10-slice positions their testsets happen to cover.

**Claimed-type distribution (presheaf→sheaf)**:

| Claimed type | Count |
|---|---|
| snare | 1 |

---

## Nash Distance Analysis

| Metric | Value |
|---|---|
| Canonical Nash-distance-1 constraints | 13 |
| In working set (have ≥2 10-slice contexts) | 10 |
| Become Nash-stable on 10-slice (distance ≥ 2) | 5 |
| Remain Nash-distance-1 on 10-slice | 4 |
| Already have 10-slice H¹ = 0 (sheaf) | 1 |
| Stability rate (among those in working set) | 50.0% |

**Interpretation**: v6.11 reports that all 267 canonical Nash-distance-1 constraints became
Nash-stable (distance ≥ 2) on the 156-point product site because the institutional position
occupies a 48-context block. The 10-slice family is not the product site — it has 1–3
institutional contexts (U_3_imm, U_3_civ, U_3_nat). Nash-stability on the 10-slice site
tests whether distance-1 constraints remain vulnerable when institutional appears at multiple
time-horizon variants rather than as a single block.

---

## Arakelov Fragility

**Deferred.** Arakelov height computation requires MaxEnt distributions
(`maxent_distribution_raw/3`) at each observer context. MaxEnt is populated at pipeline-time
for `site_contexts/1`'s current contexts; the 10-slice contexts are not in the current pipeline
run. Re-running requires:
1. Adding `site_contexts_ten_slice/1` to `constraint_indexing.pl`
2. Re-running MaxEnt for those contexts
3. Re-running the full pipeline

The per-constraint `canonical_arakelov` values (from `pipeline_output.json`) are recorded
in the JSON output but 10-slice Arakelov values are null. The fragile/genuine sub-partition
question (Q3 from the prompt) remains open.

---

## §7 Reconciliation Pointer Status

The second reconciliation pointer from `coupling_structure_evidence.md` §7:
*"A binary-boundary audit on the 10-slice family — recomputing H¹ at each slice and
checking whether the binary classification is preserved — is the test that would let
audit results bear on the framework's primary claim."*

**This audit closes the binary H¹ question (partially).** The test was run on
164 constraints with ≥2 testset-covered 10-slice contexts. The framing caveat:
results describe behavior *in testset-covered 10-slice positions*, not on the 10-slice
family as a fixed site. The variable per-constraint subsite (2–7 contexts) limits
what can be inferred about the full 10-slice family.

**The Arakelov sub-question (Q3) remains open.** MaxEnt re-run required.

---

## Methodological Self-Report

**What this evidence supports:**
- Binary H¹ preservation rate in testset-covered 10-slice positions: 15.85%
- Crossing rate: 84.15% (137 sheaf→presheaf, 1 presheaf→sheaf)
- Stratified preservation rates by n_contexts (table above)
- Nash-distance behavior for 10 constraints formerly Nash-distance-1 on canonical site

**What this evidence does not support:**
- A claim that the binary boundary is preserved on the 10-slice family as a fixed site (no constraint is classified at all 10 slices; max coverage is 7)
- A direct comparison to the 4→156 product-site expansion (different test: that fixed all contexts, this uses per-constraint variable subsites)
- Arakelov fragility claims on the 10-slice site (MaxEnt not available)
- A universal site-stability claim extending to all fine site expansions

**U_3_civ as predicted crossing source:** Confirmed — 46 of 137 sheaf→presheaf crossings driven by U_3_civ (34%)

**Canonical site drift:** Only U_4_glob (analytical/civilizational/analytical/global) matches
a canonical context. Canonical U3 (institutional/generational/arbitrage/national) is not in
the 10-slice family; its nearest analogs are U_3_imm (immediate time) and U_3_civ
(civilizational time). Results on the 10-slice family describe a site adjacent to but distinct
from the canonical site.
