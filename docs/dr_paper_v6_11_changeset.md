# Deferential Realism Paper: v6.10 → v6.11 Changeset

## Summary

Product-site expansion (156 observer contexts, 3,301 constraints) + three validation studies produce findings that require corrections, new sections, and reframings in the DR paper. Core axioms, theorems 1-3, and engine architecture are unchanged. Theorem 4, Nash analysis, H¹ gap characterization, δ-sweep interpretation, and the "moderate as anchor" claim require revision. New empirical subsections document the product-site findings.

---

## Abstract

### Corrections

**Replace** "Single-position analysis with full information detects less than 3% of the observer-dependent structure that cross-position analysis reveals" **with:**

"Single-position analysis with full information detects approximately 88% of cross-position structure on the 4-point site; when combined with knowledge of the block structure (classification is approximately constant within power-level blocks), 4-point analysis captures 93% of the full 156-point product-site structure, with the remaining 7% residing in within-block Exit and Scope variation."

**Replace** "Nash distance and H¹ persistence are sharply anti-correlated — a phase transition rather than a gradient — identifying two structurally distinct extraction regimes. The institutional observer's vulnerability concentration (94% of resolvable cases) is a structural property confirmed across the full range of power modifier calibrations." **with:**

"Nash distance and H¹ are definitionally linked (Nash distance = 0 iff H¹ = 0), and the H¹ gap imposes a phase transition on Nash distance. The institutional observer generates 30% of inter-power-level classification disagreements on the product site, manifesting as a classification discontinuity at the institutional power threshold rather than a single-observer Nash vulnerability (the 94% single-observer resolution figure on the 4-point site was a projection artifact of having only one institutional context point)."

**Add** after the Arakelov height paragraph:

"A product-site expansion — computing classifications at 156 observer contexts (4 power levels × 3 time horizons × 5 exit options × 3 scope levels) — confirms that the binary sheaf/presheaf classification (H¹ = 0 vs. H¹ > 0) is preserved exactly across sites with zero crossings, establishing the classification boundary as stable under measurement refinement. The product site reveals a P × E × S lattice structure: power controls the primary classification boundary, while Exit and Scope cross thresholds within power-level blocks for 89% of manifest presheaves, with Time as an occasional modifier through the Hub 2 immutability table. A representation robustness study across 32 parameter settings confirms the binary boundary is invariant under exit modulation (0×–3×) and classification thresholds (±18%), and sensitive only to large deformations of the power sigmoid. The 4 canonical contexts are extremally sufficient for Arakelov height analysis — product-site heights are identical for all 3,301 constraints."

---

## §2. The Axioms

### Axiom 1

**Add** at the end of the Axiom 1 discussion (after "The invariants are geometry-relative..."):

"A product-site expansion validates this claim. The classification engine already accepts arbitrary (P, T, E, S) context tuples — `classify_from_metrics/6` is fully context-parameterized. A curated 156-point product site (4P × 3T × 5E × 3S, excluding category-error combinations where the analytical exit option is paired with non-civilizational time horizons) covers the Cartesian product of the core ontology values. On this expanded site, the binary sheaf/presheaf classification is preserved exactly (zero crossings across 3,301 constraints), confirming that the 4-element chain is a faithful oracle for the classification boundary. However, the product site reveals internal structure invisible to the 4-point projection: classification varies within power-level blocks along Exit and Scope axes for 89.3% of manifest presheaves, producing a P × E × S lattice rather than a 1D power chain."

---

## §3. The Theorems

### Theorem 2: H¹ Gap

**Add** after the existing proof sketch, corpus confirmation, and methodological note:

"**Product-site recharacterization.** On the 156-point product site, the H¹ gap transforms: the minimum nonzero H¹ is 3,380 (= 26 minority contexts × 130 majority contexts), reflecting classification changes in power-level blocks of approximately 39 contexts. Values between 0 and 3,380 are absent, preserving the gap structure at much higher magnitude. The gap does not close with higher resolution — it widens. Classification is quantized along the power axis, with the block structure ensuring that any departure from global agreement involves an entire power-level block, not individual contexts. The 4-point gap {0, 3, 4, 5, 6} and the product-site gap {0, 3380, ...} are both consequences of the same underlying phenomenon: classification thresholds are crossed at power-level boundaries, and the pair-counting formula translates block-level disagreement into H¹ values determined by the number of contexts in each block.

Within power-level blocks, secondary variation exists: Exit and Scope axes cross classification thresholds in 89.3% of manifest presheaves (760 of 851), producing a P × E × S lattice structure. The most common pattern (55%) is a snare/tangled_rope split driven by the scope modifier (σ(local) = 0.8 vs. σ(global) = 1.2 creating enough χ variation to cross snare_chi_floor). The second pattern (36%) involves naturalized/rope classification at the institutional power level, driven by the effective_immutability table. Only 91 constraints (10.7%) are pure power-axis presheaves with internally constant blocks — all mid-range-ε tangled_rope constraints where the institutional sign-flip is the only threshold crossing and E/S modulations are too small to reach any boundary."

### Theorem 3: Institutional Spectral Dominance

**Replace** the game-theoretic extension paragraph ("The institutional observer is the vulnerable position in 94%...") **with:**

"**Game-theoretic extension (4-point site).** On the 4-point canonical site, the institutional observer is the vulnerable position in 94% of resolvable Nash cases (237/252 cases where a single-observer type change would achieve H¹ = 0). This concentration survives π sensitivity sweeps and holds as a plateau across f(d) ∈ [−0.30, +0.60].

**Product-site correction.** On the 156-point product site, all 267 constraints that had Nash distance 1 on the 4-point site become Nash-stable (distance ≥ 2). The institutional observer occupies a 48-context block; no single-context change resolves the disagreement. The 94% figure was an artifact of the 4-point projection: one institutional context point meant one flip could achieve consensus. The corrected characterization: 30% of non-constant constraints (252 of 851) have institutional as the power-level odd-one-out — the block where the constraint is classified as coordination while other power levels classify it as extraction. This is a classification discontinuity at the institutional power threshold, not a single-observer Nash vulnerability. The philosophical claim (power constitutively shapes classification) is strengthened by this correction: the institutional classification is not a fragile single-point outlier but a stable region of index space — an entire power-level block where the world looks different.

Within the institutional block, secondary structure exists: different exit options produce different classifications (naturalized vs. rope) in 79% of cases, because near-zero χ values make the effective_immutability gate the tiebreaker. The moderate observer remains the classification anchor between power levels but is the most fragile link for within-block structure — its steep sigmoid derivative (4.5× higher than at the power extremes) makes it most sensitive to δ perturbation at sub-threshold resolution."

**Replace** the final sentence ("The moderate observer is the classification anchor of the system...") **with:**

"The moderate observer is the classification anchor for between-block structure: its d value places it in the sigmoid's stable region where perturbations rarely cross power-level boundaries. However, the product-site δ-sweep reveals that the moderate block is the most fragile link for within-block structure: 58 of 60 all-constant presheaves that develop within-block variation under δ perturbation do so at the moderate position (see §5.10)."

### Theorem 4: Oracle Gap

**Replace** the entire theorem statement and corpus confirmation **with:**

"### Theorem 4: Oracle Gap

The oracle gap — the fraction of cross-position classification structure invisible from any single position — depends on what structural knowledge the oracle possesses.

**4-point site, single observer.** An analyst at any single canonical observer position, even with full information about the constraint's metrics, can predict approximately 88% of the 4-point orbit structure. The best single observer is the powerless position. The remaining 12% is the cross-position structure that requires multi-position analysis.

**Product-site, 4 canonical contexts + block rule.** When the analyst knows all 4 canonical context classifications AND applies the block-structure rule (classification is approximately constant within power-level blocks), the 4-point site captures 93% of the full 156-point product-site classification structure. The remaining 7% is the information content of within-block Exit and Scope variation — structure the 4-point site cannot detect.

**Misprediction structure.** The 7% gap concentrates in mixed-block presheaves (760 constraints), where the 4-canonical-context + block-rule prediction misses an average of 47 of 156 context classifications per constraint (30%). Mispredictions distribute roughly evenly across power levels (powerless 20.5%, moderate 28.7%, institutional 23.3%, analytical 27.5%) and across secondary axes (T 28.1%, E 36.0%, S 35.9%).

**Corpus confirmation (both corpora).** The original <3% detectable figure from the 4-point single-observer analysis is confirmed but recontextualized: it describes how much of the 4-point orbit a single observer captures, not how much of the domain structure any site captures. The 4-point site itself captures 93% of the product-site structure. The dominant information loss is within-block E/S variation, invisible to any power-axis-only analysis."

---

## §5. Empirical Findings

### Add new §5.9: Product-Site Expansion

**Insert** after §5.8 (Arakelov / Fisher analysis):

"### 5.9 Product-Site Expansion

**Infrastructure.** The measurement layer has been refactored. Four independent hardcoded 4-context lists — in `grothendieck_cohomology.pl`, `dirac_classification.pl`, `measurement_layer.pl`, and `game_theory_nash.py` — have been unified into a single `site_contexts/1` predicate in `constraint_indexing.pl`, controlled by `config:param(site_mode, Mode)` (canonical or product). The classification engine (`drl_core.pl`) retains its own fixed 4-context list for structural classification semantics (snare/mountain gates), correctly distinguished from the configurable measurement site. A `sheaf_status/2` predicate in `sheaf_analysis.pl` composes `cohomological_obstruction/3`, `arakelov_height/2`, and `arakelov_threshold/1` to classify constraints into three regimes: genuine_sheaf (H¹ = 0, Arakelov low), fragile_presheaf (H¹ = 0, Arakelov high), and manifest_presheaf (H¹ > 0). A `block_consistency/2` predicate reports whether each power-level block is internally constant or mixed.

**Binary boundary stability.** The sheaf/presheaf classification (H¹ = 0 vs. H¹ > 0) is preserved exactly across the 4-point and 156-point sites: zero crossings in either direction across 3,301 constraints. 74.2% of the corpus (2,450 constraints) are sheaves on both sites; 25.8% (851 constraints) are manifest presheaves on both sites. No constraint that was a sheaf on the 4-point site becomes a presheaf on the product site, and no presheaf becomes a sheaf. The collapsed 4-point site is a sufficient oracle for the binary classification.

**Lattice structure.** Within manifest presheaves, classification varies in a P × E × S lattice. Power is the primary axis — it determines whether H¹ = 0 at all. Exit and Scope are secondary axes — they cross classification thresholds within power-level blocks for 89.3% of manifest presheaves (760 of 851). Time is tertiary — it appears in only 13.1% of mixed blocks, always in combination with Exit, through Hub 2 effective_immutability table interactions. The dimensional hierarchy is P > E ≈ S > T, reflecting the relative magnitudes of the index-specific transformations: the sigmoid produces sign flips (f(d) going negative at institutional), scope modifiers produce ±25% χ variation, exit modulation produces ±0.08 d variation, and time horizon enters only through a discrete immutability lookup at near-zero χ.

Of 851 manifest presheaves, 91 (10.7%) are pure power-axis presheaves with internally constant blocks — all mid-range-ε tangled_rope constraints (ε ∈ 0.35–0.66) with orbit signature [naturalized, tangled_rope] and product-site H¹ = exactly 4,563 (= 39 institutional × 117 non-institutional contexts). The remaining 760 (89.3%) have within-block variation along E and/or S. Whether secondary axes have classificatory leverage is a geometric property: proximity to threshold boundaries in the (ε, d, σ) parameter space determines whether E/S modulations are large enough to cross a classification boundary.

**Arakelov extremal sufficiency.** Product-site Arakelov heights are identical to 4-point heights for every constraint in the corpus. The 4 canonical contexts are the extremal positions where classification uncertainty is maximized — the 152 additional product-site contexts produce no new maxima. The fragile presheaf population (10 constraints at full-corpus p75 threshold) is unchanged. The 4-point site is a sufficient basis for Regime 2 (fragile presheaf) analysis.

**Representation robustness.** A systematic perturbation study across 32 parameter settings confirms the binary boundary's stability profile. The sheaf/presheaf classification is fully invariant under exit modulation rescaling (0×–3×) and classification threshold variation (rope_chi_ceiling ±14%, snare_chi_floor ±18%). It is sensitive to large deformations of the power sigmoid (steepness ≤50% of baseline produces 152 crossings out of 1,051 tested constraints). Crossings are asymmetric: P→S (presheaves collapsing to sheaves) dominates S→P, meaning sigmoid deformation primarily resolves existing disagreements rather than creating new ones. The current calibration sits at or near a maximum of presheaf production in the parameter space. The boundary is robust under index-specific modulations and sensitive only to structural deformations of the power transformation."

### Add new §5.10: Product-Site δ-Sweep

**Insert** after §5.9:

"### 5.10 Product-Site δ-Sweep

**Stability increase.** The product site is 26× more δ-stable than the 4-point site: 150 total binary sheaf/presheaf crossings across 4 δ values ({−0.10, −0.05, +0.05, +0.10}) compared to 3,882 on the 4-point site under the same perturbations. The 39-context power-level blocks act as a buffer — a single-context flip under δ perturbation is absorbed within the block without changing the block's dominant classification or the constraint's H¹ status. This confirms that the binary boundary is far more robust to cognitive displacement on the product site than the 4-point site suggested.

**Within-block smoothing.** Among the 760 mixed-block presheaves, δ perturbation decreases within-block minority fractions (mean change −0.055 to −0.097 depending on δ direction). Cognitive displacement pushes mixed blocks toward uniformity rather than increasing fragmentation — the same asymmetry observed in the representation robustness study, where perturbation resolves disagreement more often than it creates it.

**Moderate block fragility.** Of 91 all-constant presheaves, 18–60 develop within-block variation under δ perturbation (18 at δ = −0.10, 60 at δ = −0.05). The variation concentrates at the moderate block: 58 of 60 flips at δ = −0.05 occur in the moderate power-level block. The negative-δ asymmetry is significant: negative δ (shifting toward seeing more extraction) pushes moderate-block χ values upward toward threshold crossings that positive δ does not reach. This is the product-site expression of the 4-point finding that U₂ has the highest sigmoid derivative (4.5× higher than at power extremes, §5.6): the moderate position is the between-block classification anchor but the within-block fragile link.

**Recharacterization of δ-analysis.** On the 4-point site, the δ-sweep was primarily a binary-boundary diagnostic: does perturbation break sheaves or resolve presheaves? On the product site, it is primarily a within-block lattice diagnostic: does perturbation change the internal E/S classification surface within a power-level block without changing H¹? The 834 institutional-position flips from the original 4-point δ-sweep are real single-context flips, but on the product site they are absorbed within 48-context blocks and rarely change binary sheaf status. The δ-sweep's role shifts from binary boundary analysis to lattice structure analysis."

---

## §6. Honest Assessment

### §6.5 What Remains Open

**Add/update** the following items:

**Update** "Extend to infinite or non-linear sites":

"**Extend to non-linear and product sites (partially addressed).** The product-site expansion (156 contexts) confirms that the binary sheaf/presheaf boundary does not depend on site cardinality — it is preserved exactly from 4 to 156 contexts. The product site reveals a P × E × S lattice structure invisible to the 4-point linear site. Results that are site-dependent: H¹ values, Nash distance, δ-sweep binary crossings. Results that are site-independent: the H¹ = 0 vs. H¹ > 0 boundary, Arakelov heights. The dimensional hierarchy P > E ≈ S > T and the 91/760 all-constant/mixed-block split are properties of the product site; whether they persist on alternative sites (non-linear, DAG, continuous) remains open."

**Add** new item:

"**Validate dimensional hierarchy against non-WEIRD and non-LLM corpus.** The P > E ≈ S > T hierarchy — power dominant, Exit and Scope as secondary axes crossing thresholds in 81% and 76% of mixed blocks, Time as occasional modifier through Hub 2 — could reflect a genuine structural property of power-indexed social constraints or calibration choices in the axioms. The representation robustness study provides partial evidence: varying exit modulation amplitude from 0× to 3× produces zero sheaf/presheaf crossings, confirming Exit does not compete with Power for the binary boundary even at triple strength. Varying scope modifier magnitude from 0.5× to 2.0× produces 0–77 crossings, confirming Scope can compete at extreme amplifications. A non-WEIRD corpus, or one with domain-expert-assigned metrics and greater ε diversity, would test whether the hierarchy is universal or contingent."

### §6.6 What Would Strengthen the Framework

**Add** new item:

"17. **Product-site validation of all 4-point findings.** The product-site expansion has validated binary boundary stability, Arakelov extremal sufficiency, δ-sweep recharacterization, and oracle gap revision. Remaining items for product-site validation: Wasserstein L¹ transport computation on the 156-point site, spectral Laplacian analysis on the product-site graph, and FCR ablation under product-site cohomology. These are computationally feasible with the existing `site_contexts/1` infrastructure and would determine which of the remaining 4-point findings are site-invariant."

---

## §1. Introduction

**Add** to the paper roadmap sentence:

"...§5 reports empirical findings including a cognitive displacement analysis, a δ-band population analysis, an Arakelov height diagnostic, a product-site expansion confirming binary boundary stability on a 156-context observer site, and a representation robustness study identifying the power sigmoid as the load-bearing architectural feature of presheaf structure..."

---

## §4. The Computational Engine

### §4.1 The Rule Cascade

**Add** after "The cascade is deterministic...":

"The engine is fully context-parameterized: `classify_from_metrics/6` and `dr_type/3` accept arbitrary (P, T, E, S) context tuples, not only the 4 canonical observers. The measurement layer (`cohomological_obstruction/3`, `gauge_orbit/2`, `arakelov_height_pair/3`) iterates over a configurable site via `constraint_indexing:site_contexts/1`, switchable between a 4-point canonical site and a 156-point curated product site via `config:param(site_mode, canonical|product)`. Classification predicates in `drl_core.pl` (snare_immutability_check, dr_mismatch, cross_context_analysis) retain a fixed 4-context list for structural classification semantics, correctly distinguished from the configurable measurement site. New diagnostics: `sheaf_status/2` classifies constraints into genuine_sheaf / fragile_presheaf / manifest_presheaf; `block_consistency/2` reports whether product-site power-level blocks are internally constant or mixed."

### §4.3 Corpus Provenance

**Update** living corpus note:

"...at time of writing it contains 3,301 active constraints (up from 3,254 in v6.10)."

---

## Changelog (append to end of paper)

**What changed in v6.11 — Product-Site Expansion and Validation:**

Product-site expansion (156 observer contexts, 3,301 constraints) + three validation studies (representation robustness, minimal counterexample, Arakelov on product site) + two analytical studies (oracle gap re-examination, product-site δ-sweep).

**Corrections:**

1. **Theorem 3 (Institutional Spectral Dominance):** 94% Nash vulnerability concentration is a 4-point projection artifact. On the 156-point product site, all 267 Nash-distance-1 constraints become Nash-stable (distance ≥ 2). Corrected to: 30% of non-constant constraints have institutional as power-level odd-one-out, manifesting as a classification discontinuity rather than a single-observer Nash vulnerability. Philosophical interpretation (power shapes classification) preserved and strengthened.

2. **Theorem 4 (Oracle Gap):** Revised from "<3% detectable by single observer" to a hierarchy: 88% from best single observer (4-point), 93% from 4 canonical contexts + block rule (product site). Remaining 7% gap = information content of within-block E/S variation.

3. **H¹ Gap (Theorem 2):** Recharacterized. On the product site, minimum nonzero H¹ = 3,380 (26 minority × 130 majority contexts). The gap widens rather than closing. Classification is quantized in power-level blocks, with secondary E/S variation in 89% of manifest presheaves.

4. **Nash distance:** Scoped as site-dependent throughout. Nash distance = 0 iff H¹ = 0 remains a definitional identity. All Nash distance > 0 findings are properties of the 4-point canonical site and should not be interpreted as site-invariant.

5. **"Moderate as classification anchor":** Nuanced. The moderate observer is the between-block anchor (stable under power-level perturbation) but the within-block fragile link (58/60 of δ-induced within-block flips occur at the moderate position).

6. **Corpus count:** Updated from 3,254 to 3,301.

**New empirical findings (§5.9, §5.10):**

7. **Binary boundary stability:** Zero sheaf/presheaf crossings between 4-point and 156-point sites across 3,301 constraints.

8. **P × E × S lattice structure:** Power controls the binary boundary; Exit (81%) and Scope (76%) cross thresholds within power-level blocks; Time (13%) appears only in combination with Exit through Hub 2. 91 all-constant presheaves (pure power-axis) vs. 760 mixed (E/S-structured).

9. **Arakelov extremal sufficiency:** Product-site Arakelov heights identical to 4-point for all 3,301 constraints. The 4 canonical contexts maximize classification uncertainty everywhere.

10. **Representation robustness:** Binary boundary invariant under exit modulation (0×–3×), thresholds (±18%). Sensitive to large sigmoid deformations (steepness ≤50%). Crossings asymmetric: P→S dominates S→P. Current calibration near presheaf-production maximum.

11. **Product-site δ-sweep:** 26× more stable than 4-point (150 vs. 3,882 binary crossings). δ decreases within-block variation (smoothing). Moderate block is within-block fragile link (58/60 Q3 flips at δ = −0.05).

12. **Oracle gap revision:** 4-point + block rule captures 93% of product-site structure. Remaining 7% = within-block E/S variation. Mispredictions evenly distributed across power levels and secondary axes.

**Infrastructure:**

13. `constraint_indexing:site_contexts/1` — configurable observer site (canonical/product)
14. `sheaf_analysis:sheaf_status/2` — three-regime classification
15. `sheaf_analysis:block_consistency/2` — within-block uniformity diagnostic
16. `bifurcation_export:export_product_classifications/0` — product-site classification export
17. `python/oracle_gap_analysis.py` — oracle gap computation
18. `python/product_site_delta_sweep.py` — product-site δ-sweep
19. `python/game_theory_nash.py` — generalized from range(4) to N-context; post-product-site note added

**§6 updates:**

20. "Extend to non-linear sites" updated to partially addressed (product site confirms binary boundary stability; lattice structure and dimensional hierarchy are new findings requiring further site validation).
21. New item 17: Product-site validation of remaining 4-point findings (Wasserstein, spectral, FCR ablation).
22. New item: Validate dimensional hierarchy against non-WEIRD / non-LLM corpus.

**References:** Bridge paper (When Splitting Isn't Solving) cited for sheaf/presheaf theoretical framing.

**Not modified:** Axioms 1–6, Theorems 1–2 (proof sketches), engine classification logic, constraint data, structural signatures, Wasserstein analysis, FCR ablation, δ-band population analysis (§5.7), Arakelov diagnostic methodology (§5.8), cross-constraint cover story topology (§5.8), related work (§7), scope-limit discussion (§8).
