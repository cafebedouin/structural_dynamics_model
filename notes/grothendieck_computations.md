# Grothendieck Cohomological Computations

## Cech Cohomology of the DR Presheaf — Corpus Results

**Date:** 2026-02-14
**Module:** `grothendieck_cohomology.pl` v1.0
**Corpus:** 1023 constraints with extractiveness data (1025 testsets, 2 without metric data)

---

## 1. Corpus Summary

| Invariant | Value | Interpretation |
|-----------|-------|----------------|
| Corpus size | 1023 | Constraints with extractiveness metrics |
| \|H⁰\| (global sections) | 212 | Constraints where classification is context-invariant |
| \|H¹ > 0\| (descent failures) | 811 | Constraints with perspectival fracture |
| Sum(H¹) | 2977 | Total disagreeing context-pairs across all constraints |
| Mean H¹ (over H¹ > 0) | 3.67 | Average obstruction count among non-descending constraints |
| Descent rate | 0.2072 | Fraction of corpus admitting global sections |

**Key finding:** Roughly 1 in 5 constraints admits a global section (a "view from nowhere"). The remaining 4 in 5 have perspectival fracture that cannot be resolved by choosing a single type. This validates the system's design: the presheaf is emphatically not a sheaf.

---

## 2. H⁰ Breakdown by Preserved Type

| Type | Count | % of H⁰ | Interpretation |
|------|-------|----------|----------------|
| tangled_rope | 143 | 67.5% | Tangled from every perspective |
| rope | 34 | 16.0% | Legitimate coordination, universally recognized |
| mountain | 21 | 9.9% | True natural laws, context-invariant |
| scaffold | 14 | 6.6% | Temporary coordination, universally recognized |
| **Total** | **212** | **100%** | |

**Notable:** No snares, pitons, or unknown types appear in H⁰. This means:
- Snares are *never* universally recognized as snares — there is always at least one context that classifies them differently. This is philosophically precise: extraction always has a cover story.
- Mountains that genuinely descend (21) are the mathematical/physical constraints (thermodynamics, uncertainty, impossibility theorems).
- The largest H⁰ group is tangled_ropes (143), meaning many constraints are tangled from every perspective — the entanglement of coordination and extraction is visible to all observers.

---

## 3. H¹ Distribution

| H¹ Value | Count | % of Corpus | Obstruction Structure |
|----------|-------|-------------|----------------------|
| 0 | 212 | 20.7% | Global section exists (descent holds) |
| 1 | 0 | 0.0% | Single pair disagrees |
| 2 | 0 | 0.0% | Two pairs disagree |
| 3 | 515 | 50.3% | Three pairs disagree |
| 4 | 52 | 5.1% | Four pairs disagree |
| 5 | 240 | 23.5% | Five pairs disagree |
| 6 | 4 | 0.4% | All six pairs disagree (maximal obstruction) |

**Gap at H¹ = 1 and H¹ = 2:** No constraints have exactly 1 or 2 disagreeing pairs. This is a structural feature of the site, not an accident. The 4 standard contexts have a specific power ordering (powerless < moderate < institutional < analytical), and the classification cascade produces types that change at *threshold transitions*. When a constraint changes type, it typically changes at a single power threshold (e.g., institutional → analytical), creating 3 disagreeing pairs (the changed context disagrees with all 3 unchanged ones). A constraint cannot have exactly 1 disagreeing pair because the power ordering makes transitions occur at boundaries that affect multiple pair comparisons simultaneously.

**The dominant mode is H¹ = 3 (50.3%).** This corresponds to the most common orbit structure: three contexts agree on one type, one context sees another. The modal pattern is `[snare, snare, rope, snare]` — the institutional context (with arbitrage exit options and generational time horizon) sees a rope where powerless, moderate, and analytical contexts see a snare.

**H¹ = 5 is the second largest group (23.5%).** These constraints have 3 or more distinct types in their orbit, creating widespread disagreement. The typical pattern involves `unknown` in the orbit — contexts where classification falls to the residual category.

---

## 4. Maximum Obstruction Constraints (H¹ = 6)

Four constraints achieve maximal obstruction — all 6 context-pairs disagree:

| Constraint | Orbit | Types |
|-----------|-------|-------|
| ai_performance_watermark | [indexically_opaque, tangled_rope, rope, snare] | 4 distinct |
| openai_prism_development | [indexically_opaque, tangled_rope, rope, snare] | 4 distinct |
| semiconductor_fabrication_chokepoint | [indexically_opaque, tangled_rope, rope, snare] | 4 distinct |
| us_usmca_china_leverage | [indexically_opaque, tangled_rope, rope, snare] | 4 distinct |

All four share the same orbit structure: `[indexically_opaque, tangled_rope, rope, snare]`. This is the maximally fractured perspectival profile:
- **Powerless** sees opacity (cannot determine what the constraint is)
- **Moderate** sees tangled coordination/extraction
- **Institutional** sees legitimate coordination
- **Analytical** sees extraction

These are constraints where power level completely determines what you see. They are the system's clearest examples of perspectival dependence of classification.

---

## 5. Mean H¹ by Analytical-Context Type

| Analytical Type | Mean H¹ | N | Interpretation |
|----------------|---------|---|----------------|
| scaffold | 0.00 | 14 | Scaffolds always descend (H¹ = 0 for all) |
| rope | 1.04 | 130 | Ropes mostly descend; low perspectival variance |
| tangled_rope | 1.78 | 412 | Moderate perspectival fracture |
| mountain | 2.82 | 71 | Some mountains are context-dependent (!)  |
| snare | 3.75 | 272 | High perspectival fracture |
| unknown | 3.85 | 124 | Highest fracture — classification failure correlates with perspective-dependence |

**Key findings:**

1. **Scaffolds always descend.** Every constraint classified as scaffold from the analytical context has the same type from all contexts. This makes categorical sense: temporary coordination mechanisms are universally recognized as temporary.

2. **Ropes mostly descend (mean H¹ = 1.04).** Legitimate coordination is visible from most perspectives, with only slight perspectival variance.

3. **Snares have high fracture (mean H¹ = 3.75).** Extraction mechanisms look different depending on who is observing. This is the core insight of Deferential Realism: what appears as immutable natural constraint from one position appears as changeable extraction from another.

4. **Mountains show non-trivial H¹ (2.82).** This is surprising. It means many constraints classified as mountains from the analytical perspective are *not* classified as mountains from other perspectives. These are the "contested mountains" — constraints where the analytical observer's assessment differs from other observers.

5. **Unknown has the highest mean H¹ (3.85).** Constraints that fall to the residual category from the analytical perspective are precisely those with the most perspectival variance — classification failure IS perspectival fracture.

---

## 6. H¹ vs. Purity Score Correlation

| H¹ | Mean Purity | N | Pattern |
|----|------------|---|---------|
| 0 | 0.713 | 155 | High purity for descending constraints |
| 3 | 0.526 | 486 | Moderate purity for single-threshold transitions |
| 4 | 0.975 | 51 | Anomalously high — see note below |
| 5 | 0.342 | 233 | Low purity for high-obstruction constraints |
| 6 | 0.348 | 4 | Low purity for maximal obstruction |

**General trend:** H¹ correlates negatively with purity. Higher perspectival obstruction → lower structural purity. This confirms that cohomological fracture and naturality failure measure related but distinct phenomena.

**Anomaly at H¹ = 4:** The mean purity of 0.975 for H¹ = 4 constraints breaks the monotone trend. These 52 constraints have the orbit pattern `[mountain, unknown, unknown, mountain]` — they are classified as mountain from powerless and analytical contexts but as unknown from moderate and institutional contexts. The high purity arises because mountains have purity_score ≈ 1.0 (immune to contamination), and the unknown classification at moderate/institutional doesn't reduce purity because purity is computed at the analytical context. This reveals that purity and cohomology measure different things: purity measures structural health *at a single context*, while cohomology measures *cross-context consistency*.

---

## 7. Descent Analysis by Domain

### Constraints that satisfy descent (H¹ = 0)

The 212 descending constraints include:
- **Mathematical constraints:** halting_problem, godels_incompleteness, arrows_impossibility, chaitins_omega — these are genuine natural laws recognized from every perspective
- **Physical constraints:** second_law_of_thermodynamics, heisenberg_uncertainty — context-invariant physical laws
- **Well-known coordination mechanisms:** TCP/IP, metric system — universally recognized ropes
- **Visible entanglements:** academic_peer_review_gatekeeping, pharmaceutical_patent_system — tangled_ropes visible from every perspective

### Constraints that maximally fail descent (H¹ = 5–6)

The 244 constraints with H¹ >= 5 are the most perspectivally fractured:
- **Geopolitical constraints** with power-dependent classification
- **Technology governance** constraints where opacity increases with decreasing power
- **Emerging constraints** (2026 dataset) where classification is unsettled

---

## 8. Relationship Between H¹ and Dirac Orbit Families

The 24 Dirac orbit families (from `gauge_orbit/2`) partition constraints by their *set of types in the orbit*. H¹ adds quantitative resolution within this partition:

| Orbit Family (set) | H¹ | Count | Notes |
|--------------------|-----|-------|-------|
| {rope, snare} | 3 | 315 | Largest family — single threshold transition |
| {rope, snare, tangled_rope} | 5 | 164 | Three types → high H¹ |
| {tangled_rope} | 0 | 143 | Descends — universally tangled |
| {rope, tangled_rope} | 3 | 135 | Single threshold transition |
| {mountain, unknown} | 4 | 50 | Mountain-unknown boundary |
| {mountain} | 0 | 21 | True mountains — perfect descent |
| {rope} | 0 | 34 | True ropes — perfect descent |
| {scaffold} | 0 | 14 | True scaffolds — perfect descent |

**Observation:** Within a given orbit family, H¹ is constant. This is a consequence of the site structure: the number of disagreeing pairs is fully determined by the set of distinct types and how they distribute across the 4 canonical positions. Since all members of an orbit family have the same type-set, they have the same H¹.

This means H¹ does not add information *within* orbit families — its value is a function of the orbit family. What H¹ adds is a graduated numerical measure that allows comparison *across* families: the {rope, snare} family (H¹ = 3) is less fractured than the {rope, snare, tangled_rope} family (H¹ = 5).

---

## 9. Relationship to the 6 Genuine Abductive Findings

The abductive engine was not run in this computation pass (it requires `abductive_enabled = 1` in config and the MaxEnt/FPN pipeline). However, from the structural analysis:

- **Deep deception** (FNL + MaxEnt agreement on mountain): These constraints have H¹ > 0 by definition — their false-natural-law status means they classify as mountain from some contexts but fail Boltzmann compliance, creating perspectival fracture.

- **Metric-structural divergence** (high entropy + preserved orbit): These have H¹ = 0 (preserved orbit = global section exists). Cohomology sees them as "resolved" even though MaxEnt detects boundary proximity. This confirms that cohomology and MaxEnt measure different things.

- **Confirmed liminal** (triple confirmation): These are transition states with high H¹ — the constraint is actively shifting type across contexts.

- **Coverage gap** (Dirac detects what mismatch misses): These have H¹ > 0 (multi-type orbit). The coverage gap is precisely the discrepancy between the mismatch detector (which uses stored facts) and the on-the-fly Dirac computation.

- **Accelerating pathology** and **contamination cascade**: These FPN-based findings measure temporal dynamics, not cross-context cohomology. H¹ is static; drift is temporal. The two are complementary.

- **Dormant extraction**: Clean metrics + extractive voids. May have H¹ = 0 (metrics look clean from all contexts) or H¹ > 0 (the "dormant" extraction is perspectivally visible from some positions). This would require a per-constraint analysis with the abductive engine active.

---

## 10. Summary of What Cohomology Adds

**H⁰** provides a crisp count (212) of gauge-invariant constraints — those admitting a "view from nowhere." This is already detected by `preserved_under_context_shift/2`, so H⁰ adds vocabulary, not computation.

**H¹** provides a graduated measure of perspectival fracture (0–6) that reveals:
1. The gap at H¹ = 1,2 (a structural feature of the power ordering)
2. The dominance of single-threshold transitions (H¹ = 3)
3. The negative correlation with purity (except the H¹ = 4 anomaly)
4. The fact that H¹ is constant within orbit families (it is a function of the orbit type-set)

**Descent rate** (0.2072) is a corpus-level invariant that characterizes the system: roughly 80% of social constraints have irreducible perspectival dependence.

**What cohomology does NOT add:** New detections. The abductive engine, MaxEnt entropy, and Dirac orbits already identify every constraint that cohomology identifies. Cohomology provides a formal vocabulary and a numerical invariant, but no new algorithmic power.
