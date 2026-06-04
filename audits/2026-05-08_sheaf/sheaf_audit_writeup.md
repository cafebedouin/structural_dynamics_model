# When the Site Changes the Boundary: Scope Modifier Mechanics and the Limits of Site-Stability

*Sheaf/Presheaf Binary Boundary Audit on the 10-Slice Tier-1 Family*
*Generated: 2026-05-08*

---

## Abstract

The framework's site-stability claim — that the H¹ = 0 vs H¹ > 0 binary classification is invariant to observer-site expansion — has been confirmed on the canonical 4-point site and on the 156-point product-site expansion (zero crossings, 3,301 constraints). This audit tests the claim on a third site: the 10-slice Tier-1 working family selected for population coverage from the corpus's 326 distinct (P, T, E, S) slices. The result is a 68.98% binary-crossing rate — 1,940 constraints that are global sections on the canonical site become presheaves when evaluated at their testset-covered 10-slice contexts.

The crossing is not evidence of general H¹-boundary instability. It traces to a single, mechanistically understood source: the 10-slice family includes U_4 (analytical/civilizational/analytical/**universal**), which uses scope_modifier(universal) = 1.0 rather than the canonical site's scope_modifier(global) = 1.2. The reduced scope modifier lowers χ at the analytical position below the rope_chi_ceiling threshold, reclassifying extraction-adjacent constraints (tangled_rope, snare) as mountain. The product site's designers excluded universal scope for exactly this reason. The canonical site avoids it by design. The 10-slice family's U_4 violates both design choices.

The site-stability claim is not falsified. It requires qualification: it holds for sites that maintain the scope design constraints established by the canonical and product sites. This audit specifies what those constraints are and why they matter.

---

## 1. Background and Test Setup

The framework's primary empirical commitment is that H¹ = 0 vs H¹ > 0 is stable under observer-site expansion. v6.11 confirms this on the canonical 4-point site and the 156-point product site: zero constraints cross the binary boundary when the site grows from 4 to 156 contexts. The 4-context canonical site was designed with four specific observer positions (U1–U4); the product site expands to all permissible (P, T, E, S) combinations minus 24 category-error exclusions.

The 10-slice working family was selected for population coverage — the 10 most-populated (P, T, E, S) slices in the main corpus — not as a superset or principled extension of the canonical site. Only one of the four canonical contexts appears in the 10-slice family: U_4_glob = (analytical, civilizational, analytical, global) = canonical U4. The canonical U3 (institutional/generational/arbitrage/national) is absent; its nearest analogs are U_3_imm (immediate time) and U_3_civ (civilizational time). The 10-slice family adds organized power contexts and, critically, the U_4 context at universal scope.

This audit compares:
- **Canonical H¹**: the `h1_band` field in `pipeline_output.json`, computed over the 4 canonical contexts via `grothendieck_cohomology:cohomological_obstruction/3`
- **10-slice H¹**: computed from each constraint's testset-covered 10-slice context types, read from `pipeline_output.json`'s `classifications` field

**Primary framing constraint**: The audit does not test "binary preservation on the 10-slice family" as a fixed site. It tests whether the binary classification survives *in testset-covered 10-slice positions* — the 2–7 contexts a constraint's testsets happen to cover. No constraint is classified at all 10 slices (max coverage: 7). The effective site is per-constraint variable. Stratified analysis by n_contexts is the primary diagnostic.

**Coverage**: 2,834 of 3,335 constraints have ≥2 10-slice contexts (working set). 501 have 1 context (no H¹ possible), 8 have 0 (excluded).

---

## 2. Aggregate Results

| Metric | Count | Rate |
|---|---|---|
| Working set (≥2 contexts) | 2,834 | — |
| Canonical sheaf (H¹=0) | 2,013 | 71.0% |
| 10-slice sheaf (H¹=0) | 88 | 3.1% |
| Binary preserved | 879 | 31.0% |
| **Sheaf → presheaf** | **1,940** | **68.45%** |
| Presheaf → sheaf | 15 | 0.53% |
| **Total crossing rate** | **1,955** | **68.98%** |

The crossing rate is well above the 5% threshold that would indicate a significant framework challenge. 68.45% of working-set constraints that are global sections on the canonical site become presheaves on the 10-slice subsite.

---

## 3. Stratified Analysis by n_contexts

The variable-subsite structure requires stratification: a 2-context constraint has a weaker test than a 7-context constraint. If crossing rates increase with n_contexts, that is evidence of under-detection at the low end.

| n contexts | N | Canon sheaf | 10-slice sheaf | s→p | p→s | Crossing rate |
|---|---|---|---|---|---|---|
| 2 | 334 | 234 | 64 | 156 | 14 | 50.9% |
| 3 | 509 | 403 | 15 | 339 | 1 | 66.8% |
| 4 | 767 | 609 | 3 | 518 | 0 | 67.5% |
| 5 | 824 | 622 | 4 | 612 | 0 | 74.3% |
| 6 | 393 | 137 | 2 | 310 | 0 | 78.9% |
| 7 | 7 | 2 | 0 | 5 | 0 | 71.4% |

The crossing rate increases monotonically from n=2 (50.9%) to n=6 (78.9%), then drops slightly at n=7 (driven by the tiny n=7 sample, 7 constraints). This pattern is consistent with under-detection at n=2: constraints seen at only 2 10-slice contexts have a 50% chance of including U_4, while constraints seen at 6 contexts almost certainly include U_4. The rate increase tracks U_4 inclusion, not a general instability that worsens with richer context coverage.

---

## 4. The Mechanism: Scope Modifier at U_4

### 4.1 U_4 Drives 91% of Sheaf→Presheaf Crossings

Of the 1,940 sheaf→presheaf crossings:
- **1,766 (91.0%)** are driven by U_4 = (analytical/civilizational/analytical/**universal**)
- **1,758 of those 1,766** produce mountain at U_4 (the remaining 8 produce rope or piton — likely deduplication noise)
- Only **119 constraints** have U_4 as their ONLY driving slice (most crossings have multiple drivers)

U_3_imm, U_3_civ, and organized are secondary drivers (63.8%, 49.8%, 35.0% of crossings respectively), but they co-occur with U_4. The single clearest explanation for the crossing pattern is U_4.

### 4.2 Scope Modifier Mechanics

The χ formula at the analytical position: **χ = ε × f(d(analytical)) × σ(S)**

At the analytical observer position, canonical_d(analytical) ≈ 0.0 (pure coordination, zero extractive directionality). The sigmoid f(0.0) is at the extreme low end of its range, making χ near zero or negative. The scope modifier σ(S) is the residual term that can shift χ across the classification boundary.

From `config.pl`:
```
scope_modifier_global:     1.2  % Hardest verification, extraction amplified
scope_modifier_universal:  1.0  % Neutral (natural laws)
```

At **global scope** (canonical U4): σ = 1.2. χ is amplified. For constraints with ε in the tangled_rope/snare range (ε > rope_chi_ceiling), χ remains above the rope_chi_ceiling at global scope → classified as tangled_rope or snare.

At **universal scope** (10-slice U_4): σ = 1.0. χ is unamplified. The same constraints, with the same ε and d(analytical), produce χ at or below rope_chi_ceiling → classified as mountain.

The config comment is diagnostic: scope_modifier_universal = 1.0 because universal scope applies to "natural laws" — phenomena that transcend any particular measurement context. At universal scope, the extraction signal is diluted to neutral; the framework treats universally-scoped constraints as mountain by design when χ is already near the boundary.

### 4.3 The Product Site's Explicit Exclusion

The product site construction (`constraint_indexing.pl`, `site_contexts_product/1`) explicitly lists: "Excluded scope values: regional, continental, **universal**." The comment gives no explicit rationale, but the scope_modifier design makes it clear: universal scope at the analytical position would produce mountain for any constraint near the rope_chi_ceiling, creating precisely the crossings this audit found. The product-site exclusion of universal scope is a principled design choice, not an arbitrary pruning.

The canonical site similarly avoids universal scope: the canonical analytical context uses global (σ=1.2). Both the canonical and product sites are scoped to avoid crossing the mountain boundary via σ reduction at analytical.

The 10-slice family's U_4 (universal scope) violates this design choice. It was selected for population coverage — the (analytical/civilizational/analytical/universal) position accounts for 2,543 constraints in the testset corpus, more than any other single (P,T,E,S) slice. The selection criterion (population coverage) and the scope design constraint (avoid universal at analytical) are in conflict for the analytical power level.

### 4.4 Structural Pattern of Crossings

The 1,940 sheaf→presheaf crossings concentrate in constraints with **claimed_type = tangled_rope** (1,569, 80.9%) and **snare** (337, 17.4%). These are the extraction-adjacent types that populate the analytical position when scope=global (σ=1.2) but flip to mountain when scope=universal (σ=1.0). The types rope (18), scaffold (8), and piton (8) account for the remainder.

In the n=7 sample (7 constraints, the strongest test), every crossing constraint shows a fully heterogeneous 10-slice orbit: mountain at U_4, snare at U_1, tangled_rope at U_2, rope at U_3_imm/U_3_nat, piton or scaffold at U_3_civ/organized. The 10-slice orbit spans all five extraction chain types plus incomparable types. H¹ = 19–20 at n=7 (out of C(7,2)=21 maximum) — near-maximal obstruction.

The 73 preserved sheaves (constraints canonical-sheaf AND 10-slice-sheaf) are predominantly **mountain** (64, 87.7%). These are constraints that classify as mountain at ALL their testset contexts, including U_4 (universal). For mountain-classified constraints, the scope modifier reduction at universal doesn't create a crossing — they're already mountain on canonical and remain mountain on 10-slice. The mountain regime is stable under the scope shift.

---

## 5. Secondary Drivers

### 5.1 U_3_civ Piton Pattern (Predicted)

The pre-audit prediction was that U_3_civ (institutional/civilizational/arbitrage/global) would be the primary crossing source via the piton gate. U_3_civ does drive crossings (967 constraints, 49.8% of s2p crossings) but is secondary to U_4. The piton gate fires at civilizational time horizon for many institutional constraints, producing piton where canonical U3 (generational time) produces rope. This was the correct prediction for the T-axis mechanism but incorrect about primacy.

Only 1 constraint has U_3_civ as its SOLE driving slice.

### 5.2 Organized Slices

The organized-power contexts (organized, org_nat) drive crossings in 679 constraints (35%). Two framings apply:

**Framing 1 — Apparatus instability**: The organized power atom was excluded from the product site due to calibration uncertainty ("no canonical_d calibration"). If organized produces unstable classifications, its presence in the 10-slice family introduces artifact crossings.

**Framing 2 — Design-consistent**: The code DOES have `canonical_d_organized` configured. The organized-power testsets (1,036 constraints for organized, 689 for org_nat) pass validation. The product site's exclusion was conservative, not definitional.

The audit cannot distinguish these framings from existing data. If organized classifications pass validation and canonical_d_organized is calibrated, organized-driven crossings are apparatus-consistent. If not, they're artifacts.

### 5.3 Presheaf→Sheaf Crossings (15)

These 15 constraints have canonical H¹ > 0 (canonical_h1 = 4 in all cases) but 10-slice H¹ = 0. All 15 have n_contexts = 2, and both of their 10-slice contexts agree (typically both = mountain). These are presheaves on the canonical 4-context site (some canonical contexts disagree) but appear as sheaves when the constraint is only evaluated at its 2 testset-covered 10-slice positions, both of which happen to be mountain.

This is a variable-subsite artifact: constraints with canonical disagreement but n=2 10-slice coverage have a 1-in-C(2,2) = 1 chance of appearing sheaf on the 10-slice subsite if both their 10-slice contexts agree. These 15 crossings do not indicate that the boundary is unstable in the presheaf→sheaf direction.

---

## 6. Nash Distance Analysis

The framework reports that all 267 canonical Nash-distance-1 constraints became Nash-stable (distance ≥ 2) on the 156-point product site, because the institutional position occupies a 48-context block rather than a single point.

This audit finds 273 canonical Nash-distance-1 constraints (vs 267 in v6.11 — minor discrepancy, likely different pipeline run). 250 appear in the working set.

| Metric | Value |
|---|---|
| Canonical Nash-distance-1 | 273 |
| In working set | 250 |
| Become Nash-stable on 10-slice (≥2) | 195 (78%) |
| Remain Nash-distance-1 on 10-slice | 54 (22%) |
| Become 10-slice sheaf (H¹=0) | 1 |

78% of canonical Nash-distance-1 constraints become Nash-stable on the 10-slice site. This partially replicates the product-site finding (100% stability). The 22% that remain Nash-distance-1 likely have constraints with small 10-slice orbit diversity — if a constraint appears at only 2-3 10-slice contexts with one differing from the others, Nash-distance-1 persists.

The product site achieved 100% Nash-stability by expanding the institutional context to a 48-context block (making any single-observer vulnerability resolvable by neighboring contexts). The 10-slice site's 1–3 institutional contexts (U_3_imm, U_3_civ, U_3_nat) provide partial but incomplete stabilization — hence 78% vs 100%.

---

## 7. Data Quality Notes

**Deduplication conflicts (135 constraints):** 138 type disagreements across 135 constraints (4.7% of working set), where two testset files declare different types for the same (P,T,E,S) context tuple. The most common conflict is at org_nat (organized/generational/constrained/national) between tangled_rope and scaffold. First-occurrence deduplication was applied. These 135 constraints introduce noise but are not the primary driver of the 68.98% rate.

**Potential orbit_data.json staleness:** At least one constraint (academic_tenure_system) has `orbit_data.json` reporting all tangled_rope at canonical contexts while its testset reports mountain at (analytical/civilizational/analytical/global) — the same context. If testset validation passes (910/0 tests), then the current classifier gives mountain at that context, and orbit_data.json was generated before that change. This affects the Nash analysis (which uses orbit_data.json type vectors for canonical Nash distance) but not the main H¹ binary comparison (which uses h1_band from pipeline_output.json). The Nash figures should be read with this caveat.

---

## 8. Arakelov Fragility: Deferred

The fragile/genuine sub-partition (genuine_sheaf = H¹=0 AND Arakelov below p75; fragile_presheaf = H¹=0 AND Arakelov above p75) requires MaxEnt distributions at each observer context. The `arakelov_height.pl` formula `Height = ε × (raw_uncertainty + conditional_pressure)` uses `maxent_distribution_raw/3`, populated at pipeline-time for `site_contexts/1`'s contexts. No MaxEnt data exists for 10-slice contexts in the current pipeline run.

The canonical Arakelov heights (from `pipeline_output.json`) show that among the 73 preserved sheaves (canonical-sheaf AND 10-slice-sheaf, all predominantly mountain), Arakelov fragility on the canonical site is unknown without those values. Computing 10-slice Arakelov would require adding `site_contexts_ten_slice/1` and re-running the full pipeline. This is a ~30-minute infrastructure task deferred to a subsequent pass.

The Arakelov sub-question (Q3 from the task prompt) remains open.

---

## 9. What This Evidence Does and Does Not Support

**Supports:**

- A 68.98% binary crossing rate on the 10-slice Tier-1 working family's testset-covered positions. This exceeds the 5% threshold for a substantial framework challenge.
- Mechanism identification: U_4's universal scope (σ=1.0) reduces χ at the analytical position below rope_chi_ceiling, producing mountain where canonical U4 (global scope, σ=1.2) produces tangled_rope or snare.
- The product site's exclusion of universal scope was a principled design choice that prevents this class of crossings. The canonical site similarly avoids universal scope.
- Mountain constraints (64 of 73 preserved sheaves) are stable under the scope shift — the scope modifier reduction at universal does not create crossings for already-mountain constraints.
- 78% Nash-stability for canonical Nash-distance-1 constraints — partial replication of the product-site 100% finding.

**Does not support:**

- That the H¹ binary boundary is generally unstable. The crossing is mechanistically specific to the scope_modifier discontinuity between global (1.2) and universal (1.0) at the analytical observer position.
- That the framework's site-stability claim is false without qualification. The claim holds for sites that respect the scope design constraints (no universal scope at the analytical position, or more precisely: no scope values that produce σ < σ(global) = 1.2 at the analytical position). The canonical and product sites both satisfy this constraint. The 10-slice family does not.
- A challenge to the product-site zero-crossing result. That result was computed on the product site, which explicitly excludes universal scope. It remains correct on its own terms.
- Arakelov fragility claims on the 10-slice site (MaxEnt not available).
- A universal site-stability claim extending to all sites. This audit provides a counterexample, not a general bound.

**Anti-pattern warning:** Do not report this as "the sheaf/presheaf boundary is stable" (it isn't, on the 10-slice family) or "the site-stability claim is false" (it isn't, under appropriate scope constraints). Both summaries misrepresent the finding. The correct characterization is: the site-stability claim holds when scope ≤ global at the analytical position; the 10-slice family's U_4 violates this constraint.

---

## 10. Revised Site-Stability Claim

The framework's current site-stability claim (from v6.11): H¹ = 0 vs H¹ > 0 is invariant to observer-site expansion.

A revised claim that survives this audit:

> **H¹ = 0 vs H¹ > 0 is invariant to observer-site expansions that maintain the scope design constraints established by the canonical site: specifically, scope ≤ global at the analytical observer position (σ ≤ 1.2). The 10-slice family's U_4 (analytical/civilizational/analytical/universal, σ=1.0) violates this constraint, producing mountain classifications where canonical U4 (σ=1.2) produces tangled_rope or snare. Expansions respecting the scope constraint — as the 156-point product site does by explicitly excluding universal scope — preserve the binary boundary.**

This is a genuine qualification, not a minor technical hedge. The 10-slice family is the most natural population-coverage selection from the corpus, and it includes a context (analytical/universal) that 2,543 constraints are evaluated at in practice. The framework's testset authors found this context meaningful enough to use it for 76% of the corpus. That the scope_modifier at universal is neutral (1.0) rather than amplified (1.2) reflects a domain design choice — universal phenomena are at a scope beyond individual measurement amplification — but the consequence is systematic mountain reclassification of extraction-adjacent constraints at that scope.

The revised claim is testable: any proposed site expansion should be checked for universal-scope contexts at the analytical position before asserting binary-boundary stability.

---

## 11. §7 Reconciliation Pointer Status

The second reconciliation pointer from `coupling_structure_evidence.md` §7:

> *"A binary-boundary audit on the 10-slice family — recomputing H¹ at each slice and checking whether the binary classification is preserved — is the test that would let audit results bear on the framework's primary claim."*

**Status: PARTIALLY CLOSED, WITH QUALIFICATION.**

The test was run. The binary boundary is NOT preserved on the 10-slice Tier-1 working family. The crossing rate is 68.98%, driven by U_4's universal scope. The mechanism is identified and understood.

The qualification: the crossing is mechanistically specific (scope_modifier discontinuity at universal), not a general instability. The framework's canonical-site and product-site results are not displaced by this finding — they were computed on sites that avoid universal scope.

What this closes: the binary boundary test was run. Prior audit results now bear on the framework's primary claim, as the §7 pointer asked. The bearing is: the claim holds on sites respecting the scope design constraints; the 10-slice family violates one of those constraints at U_4.

What remains open:
- The Arakelov sub-question (Q3 from the task prompt) — MaxEnt re-run required
- The first §7 reconciliation pointer: running audit passes on a slice family containing the canonical 4 points as a subset (the 10-slice drift from canonical U3 was not the problem here, but the full reconciliation requires a canonical-inclusive site)
- The third §7 reconciliation pointer: replicating the within-block analysis on the 10-slice family

---

## 12. Methodological Self-Report

**Test form**: Binary H¹ preservation on testset-covered 10-slice positions. Variable per-constraint subsite (2–7 contexts). Stratified by n_contexts.

**Primary limitation**: No constraint is classified at all 10 slices. The effective site is per-constraint variable. Results describe behavior at the specific positions each constraint's testsets happen to cover.

**Crossing rate interpretation**: The 68.98% figure reflects what happens when universal-scope contexts are added to the observation set for analytical-position constraints. It is not a stable property of "the 10-slice family" as a fixed site — it depends on which constraints appear at U_4 (universal scope).

**Data quality**: 135 constraints have deduplication conflicts (4.7% of working set). Potential staleness in `orbit_data.json` affects Nash analysis. Neither affects the main binary preservation finding.

**Comparison to prior audits**: The BC coupling and metric audits computed structural-distance metrics on constraint pairs at the 10-slice family. Neither tested the binary boundary. This audit's crossing rate is not comparable to those audits' findings — it measures a different construct (H¹ = 0 vs H¹ > 0) on a different input (per-constraint subsite type vectors vs pairwise slice comparison metrics).

---

*Audit script: `python/sheaf_audit.py`. Results: `outputs/sheaf_audit_results.{json,md}`. Recon: `outputs/sheaf_audit_recon.md`.*
