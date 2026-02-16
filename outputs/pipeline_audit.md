# Pipeline Audit Report

Generated: 2026-02-15

Full audit of the structural dynamics model pipeline. Two parts: (1) consistency and pattern checks across pipeline stages, (2) analytical findings from each report.

---

## PART 1: CONSISTENCY AND PATTERN CHECKS

### A. Count Validation

| Source | Count | Notes |
|---|---|---|
| meta_report.txt (tests) | 1025 | |
| corpus_data.json | 1026 | 1 constraint has `claimed_type: None` |
| variance_analysis.md | 1026 | Matches corpus |
| fingerprint_report.md | 1023 | 2 fewer than tests |
| orbit_data.json | 1022 | 3 fewer than tests |
| maxent_report.md | 1021 | 4 fewer than tests |
| fpn_report.md | 1021 | 4 fewer than tests |
| omega_data.json | 878 | Consistent across omega pipeline |
| enriched_omega_data.json | 878 | Consistent |
| meta_report omega count | 507 | **BUG** — see below |

**Progressive drop 1025 → 1023 → 1022 → 1021:** Expected. Each Prolog stage fails on constraints with syntax errors or missing data. Not a bug.

**corpus_data.json 1026 vs 1025:** One extra constraint has `claimed_type: None`. Minor extraction bug.

**Meta_report 507 vs 878 omegas — REGEX BUG in `python/meta_reporter.py:62`:** The regex `omega_\w+_(\w+)` is greedy: `\w` matches underscores, so it consumes all but the last segment of each omega name. Example: `omega_extraction_blindness_cg_israelgaza_20231012` captures only `"20231012"`. All 105 omegas ending in `_2026` collapse to the single tuple `("2026", "conceptual")`. 371 omegas lost to false deduplication. 894 total `Ω:` lines in output.txt − 371 collisions = 507. **Fix:** change capture group from `omega_\w+_(\w+)` to `(omega_\w+)` to capture the full omega name.

### B. Category Reports vs corpus_data.json

| Category | Report Count | Meta Count | Why Different |
|---|---|---|---|
| snare | 63 | 67 | Reporter requires specific perspective criteria |
| rope | 21 | 66 | Reporter requires ALL perspectives agree on rope |
| tangled_rope | 697 | 656 | Reporter catches ANY perspective = tangled_rope (broader) |
| true_mountain | 99 | 125 | Reporter requires ALL perspectives agree |
| false_mountain | 766 | N/A | Separate overlay (perspective mismatches) |
| scaffold | 16 | 21 | Reporter applies stricter criteria |
| piton | 67 | 90 | Reporter applies stricter criteria |

Differences are by design — each reporter uses different inclusion criteria. All sampled constraint IDs match their expected corpus types. Not bugs.

### C. Omega Pipeline Chain

878 omegas consistent across all four files:

- omega_report.md: 878 `###` entries
- omega_data.json: 878 list entries
- enriched_omega_data.json: 878 under `omegas` key
- enriched_omega_report.md: 878 omega entries

All omega names match between omega_data.json and enriched_omega_data.json (0 missing, 0 extra). Severity scores: both categorical (`critical`/`high`/`medium`/`low`) and numeric (`severity_score`) fields populated in enriched JSON. Report sorted by score descending (0.930 top to 0.025 bottom).

### D. Orbit and Fingerprint

- Orbit: 1022 constraints, 25 families, 159 gauge-invariant, 863 gauge-variant
- Fingerprint: 1023 constraints, 36 shift patterns, 371 scope-sensitive
- 1022 of 1023 fingerprint IDs appear in orbit data (99.9%). The 1 missing: `catholic_church_1200`
- Scope-sensitive (371) is a strict subset of gauge-variant (863): scope-sensitivity measures only scope-axis variation (holding power/time/exit constant), while gauge-variance counts any perspective axis variation. Overlap > 80% confirmed.

### E. MaxEnt Sanity

- maxent_report.md (current run, full corpus): **163** hard disagreements, 16 high-uncertainty, 0 soft disagreements
- maxent_diagnostic_report.md (earlier run, fnl_trace bug active, 669 constraints): **114** hard disagreements
- Neither is 150. Dashboard grep patterns correctly extract from the markdown table. The "150\n150" display was likely from an intermediate run that has since been overwritten.

**Numbers that can't be trusted in maxent_diagnostic_report.md:** The 669/1025 constraint count, all per-type breakdowns (zero pitons survived the KB wipe), the 114 hard disagreement count, and all Gaussian profile parameters (trained on post-wipe population). The diagnostic's analysis of the fnl_trace bug itself is valid; its quantitative findings are not.

### F. Empty or Trivial Files

| File | Size | Issue |
|---|---|---|
| verify_opaque_report.md | 0 bytes | Empty — report generation produced no output |
| structured_analysis.json | 2 bytes (`{}`) | Empty JSON object — analysis produced no results |
| lint_errors.txt | 0 bytes | Expected (clean lint run) |

### G. Orbit Coverage

- variance_analysis.md: 1022/1026 = 99.6% orbit coverage
- covering_analysis_post_patch.md: only 469 constraints analyzed (fnl_trace KB wipe). Its 4266 "missed transitions" figure is inflated — it measures gaps against a 35-cell expanded grid but only populated 469 of ~1025 cells. The transition counts should be divided roughly by ~2.2x to estimate full-corpus figures, but the proportional patterns (82% power-axis, 18% scope-axis) are likely stable.
- enrichment_targets.md: references 727 total corpus / 566 fingerprint-sourced (from an earlier run before the corpus grew to 1025)
- The 3 constraints with `gap_class=unknown` in enriched omegas are: `deferential_realism_core`, `mars_rovers_navigational_autonomy`, `udhr_1946`. The 1 constraint in fingerprint but not orbit is `catholic_church_1200`. **These are NOT the same set** — the 3 unresolvable omegas don't correspond to the missing orbit entries.

### H. Corpus ↔ Orbit ID Normalization Mismatch (137/133)

**Root cause: multiple normalization inconsistencies between Python extractor (corpus_data.json) and Prolog reporters (orbit_data.json).** Specific patterns:

| Pattern | Count | Examples |
|---|---|---|
| Case folding | 2 pairs | `CG_IsraelGaza_20231012` vs `cg_israelgaza_20231012` |
| Spelling typos | ~10 pairs | `birthday_paradox_collison` vs `collision`, `poincare_conjucture` vs `conjecture`, `udhr_1946` vs `udhr_1948` |
| Ulysses chapter vs episode naming | 18 pairs | `ulysses_chp01` (corpus) vs `ulysses_tower_1904` (orbit) |
| `constraint_` prefix in orbit only | 14 | `constraint_borsuk_ulam`, `constraint_ftc`, etc. |
| Truncated vs full names | ~20 pairs | `ai_superpowers_2026` vs `ai_superpowers_race_2026` |
| Singular/plural, qualifiers | ~30 pairs | `noethers_theorem` vs `noether_theorem` |
| Truly unmatched | ~20-30/side | Constraints genuinely present in only one pipeline |

**Impact:** Every downstream analysis that joins corpus and orbit data silently drops these ~130 constraints. This affects omega enrichment (orbit signatures not found), conflict_map (perspectival analysis incomplete), and any future cross-pipeline analysis.

**Fix approach:** Normalize IDs at the source — either in the Python extractor or in the Prolog test harness. The testset `.pl` filenames should be the canonical ID; both pipelines should derive IDs from the same filename-to-ID function.

---

## PART 2: INTERESTING FINDINGS

### 1. variance_analysis.md

**Summary:** 1026 constraints analyzed; 72.9% show high variance (>0.5) across index configurations. Legal domain has highest average variance (0.81), mathematical lowest (0.34). 5 constraints are genuinely stable across 5+ configs — all mountains or ropes representing physical/mathematical laws.

**Most surprising finding:** `26usc469_real_estate_exemption` has variance 3.00 — it produces 3 different types across a single index configuration. This is the highest in the corpus, driven by its claimed type (rope) conflicting with its high extraction (0.75) and suppression (0.80).

**Red flag:** Domain names not normalized — `Political` vs `political`, `Physics` vs `physics`, `Social` vs `social` appear as separate domains. This affects domain-level aggregation.

### 2. pattern_mining.md

**Summary:** 592 hybrid constraints (high extraction + high suppression), 55 structural twin groups (same metrics, different types), and 3 candidate categories: tangled_rope (380 matching), scaffold (137 matching), wings (108 matching — low extraction + low suppression, overwhelmingly mountains).

**Most surprising finding:** 55 structural twin groups exist — constraints with identical (extractiveness, suppression, emerges_naturally, requires_enforcement) tuples but different claimed types. The largest twin group (81 constraints sharing signature `(0.6, 0.7, False, True)`) contains piton, tangled_rope, and snare types, suggesting these constraints are classified by criteria beyond the 4 structural metrics.

**Red flag:** None — findings are consistent with the framework's reliance on boolean gates and perspective-dependent classification beyond raw metrics.

### 3. index_sufficiency.md

**Summary:** Verdict SUFFICIENT. 0% collision rate, 1.9% anomaly rate. 20 constraints tested across 5 index configs all produce the same type — genuinely invariant mountains (Banach, Cantor, Conway) and enforced ropes (copyleft, Wikipedia, microrobot manipulation).

**Most surprising finding:** The 20 stable constraints are exactly what the framework predicts should be index-invariant: physical/mathematical laws that don't change based on who's looking. This is a strong validation signal for the indexical framework's theoretical foundations.

**Red flag:** None.

### 4. maxent_report.md + maxent_diagnostic_report.md

**Summary:** MaxEnt shadow classifier analyzes 1021 constraints with mean entropy 0.1919. 163 hard disagreements where the probabilistic classifier disagrees with the deterministic pipeline. 93.9% of hard disagreements also appear in multi-type Dirac orbits — strong cross-validation.

**Most surprising finding:** 70% of hard disagreements (115 of 163) are snare → tangled_rope. This is the exact boundary where the framework predicts maximum difficulty: "is this extraction masked as complexity, or genuine complexity?" The MaxEnt classifier systematically prefers tangled_rope over snare because the Gaussian metric profiles overlap — only the boolean gates (`has_coordination_function`, `has_asymmetric_extraction`) discriminate. This concentration is a strong signal that the boundary is real, not an artifact.

**Red flag (code bug):** The maxent_diagnostic_report.md found the **critical fnl_trace_diagnostic.pl KB-wipe bug**: the `:- initialization(run_fnl_trace)` directive in `testsets/fnl_trace_diagnostic.pl` triggers `scenario_manager:clear_kb/0` during batch loading, wiping all previously-loaded `constraint_claim` facts (354 constraints). **Reports whose numbers can't be trusted due to this bug:** covering_analysis_post_fix.md (469 constraints, not ~1025), covering_analysis_post_patch.md (same), giant_component_analysis.md (469 nodes, not ~1025), coupling_protocol_post_fix.md (same), maxent_diagnostic_report.md itself (669 constraints, all profile parameters, piton count = 0).

**Additional red flags (calibration):** Mountain entropy is degenerate (all 47 get identical H=0.3114 — the boolean penalty and tight Gaussians create a near-deterministic gate, not a soft classifier). Scaffold profile based on n=10 with zero extractiveness variance. Piton profile uses hardcoded defaults from n=0 observations.

### 5. fpn_report.md

**Summary:** Fixed-point network analysis of 1021 constraints. Converged in 4 iterations with max residual 0.000421. Only 3 constraints show significant EP shift (>0.01), and 0 constraints changed purity zone.

**Most surprising finding:** The 3 significant movers are all tangled_rope: `cmr_001` (shift 0.0176), `us_canada_geopolitical_asymmetry` (0.0163), `tiktok_us_divestiture_mandate` (0.0163). Their shifts are tiny in absolute terms but they're the only constraints where multi-hop purity propagation has any measurable effect. This means the network coupling structure is almost entirely local — contamination doesn't cascade.

**Red flag:** None — the extreme stability is consistent with the giant_component analysis showing a fragmented network.

### 6. conflict_map.md

**Summary:** 717 constraints analyzed across analytical and powerless perspectives. Dominant gap class: powerless_blind (263, 37%) — the powerless perspective can't classify these constraints. Only 210 (29%) show consensus. 10 coordination-washing cases, 44 severity amplification.

**Most surprising finding:** The powerless_blind category dominates overwhelmingly. 195 constraints show `snare → unknown` (analytical sees snare, powerless can't classify). This suggests the framework's powerless perspective is asymmetrically expressive — it lacks vocabulary to name many forms of extraction that the analytical perspective can see. This may indicate a genuine feature of power asymmetry (the powerless lack the epistemic tools to classify their situation) or a modeling gap (the powerless context needs more classification capability).

**Red flag:** 8 of 10 coordination-washing hotlist entries are from `corpus_static` (hand-authored testset annotations) rather than engine-computed. When filtered to engine-computed only, coordination-washing drops to just 2 cases. The severity amplification hotlist is 100% corpus_static. This means the most dramatic perspectival gaps are narrative annotations, not structural findings.

### 7. covering_analysis (post_patch version)

**Summary:** Erdos-Selfridge covering analysis tests whether the 12-point index grid (4 powers x 3 scopes) adequately covers the constraint space. Expanded to a 35-cell grid (7 powers x 5 scopes) to find missed transitions. **However, only 469 constraints were analyzed due to fnl_trace KB wipe**, so all transition counts are proportionally deflated.

**Most surprising finding:** 4266 missed transitions detected on the expanded grid, 82% on the power axis vs 18% on scope axis. The `institutional → mid(inst-mod)` transition is where most constraints become `indexically_opaque` — there's a large gap in the power spectrum between institutional (d=0.00) and moderate (d=0.65) where the classification engine can't resolve a type.

**Red flag:** The 469-constraint count means these numbers can't be compared to the full corpus. The proportional breakdown (power vs scope) is likely stable, but the absolute missed-transition count would roughly double with the full corpus.

### 8. abductive_report.md

**Summary:** 32 hypotheses generated: 11 genuine (all `confirmed_liminal` — triple-confirmed liminality via high entropy + multi-type orbit + active drift), 21 artifacts (all `false_ci_rope` signature overrides). 12.9% of hard disagreements explained by override artifacts. 2 trigger classes inactive (accelerating_pathology, contamination_cascade) due to missing FPN data.

**Most surprising finding:** The 11 confirmed_liminal constraints include `quine_self_replication` (H=0.48, orbit=[mountain,rope]) and `institutional_trust_decay` (H=0.44, orbit=[rope,tangled_rope,unknown]). These are constraints where three independent diagnostics (entropy, orbit, drift) all agree the constraint is genuinely between categories — not misclassified, but structurally liminal.

**Red flag:** The 0 hypotheses for `deep_deception`, `metric_structural_divergence`, `coverage_gap`, `dormant_extraction` is suspicious. Either the corpus genuinely has none, or the trigger conditions are too strict. The FPN data gap (noted in report) disables 2 classes entirely.

### 9. giant_component_analysis.md

**Summary:** Network of 469 constraints (fnl_trace-affected count), 316 edges, 398 components. Largest component is 15 nodes (3.2%). No giant component at any threshold (0.10-0.90). **The threshold sweep produces identical results at every level** — all edges are explicit/shared_agent type, zero are inferred_coupling.

**Most surprising finding:** The network is inherently fragmented. The Erdos-Renyi prediction says a giant component should emerge at 234 edges (n/2), and the network has 316 edges — but they're distributed across 398 small components. The network is above the ER edge threshold but below the connectivity threshold because edges are concentrated in small cliques rather than forming bridges.

**Red flag (code bug):** The threshold sweep is vacuous — every threshold from 0.10 to 0.90 produces exactly 316 edges, 398 components, largest=15. This means `infer_structural_coupling` edges either aren't being generated or aren't being filtered by threshold. The sweep tests nothing. Context comparison (Phase 4) shows edge topology is context-independent, which is correct, but makes the sweep redundant.

### 10. coupling_protocol (post_fix version)

**Summary:** Also contaminated by fnl_trace_diagnostic.pl output (FNL diagnostic trace prepended). The actual coupling protocol analysis is buried after hundreds of lines of Prolog trace. Operating on the post-wipe population.

**Most surprising finding:** coupling_protocol_post_fix.md is byte-identical to coupling_protocol.md (both 73,048 bytes). The "post_fix" version didn't actually fix anything — it was regenerated under the same fnl_trace conditions.

**Red flag:** Both files contain the same data. The post_fix designation is misleading.

### 11. classification_audit_report.md

**Summary:** From an earlier run (2026-02-08) with 342 constraints / 666 .pl files. Found 252 False Mountains, partitioned into: 15 severe naturalization (A+), 51 standard naturalization (A), 5 theater-mountain conflicts (B), 31 legitimate gaps (C), 13 WHO suspects (D), 170 classification-metric inconsistencies (E4), 195 missing theater ratios (E2).

**Most surprising finding:** The 15 A+ severe naturalizations include `bay_of_pigs_operational_silo` (eps=0.90, theater=0.72, claimed snare, powerless sees mountain). This is a textbook false mountain — something that looks immovable from the powerless perspective but is actually an engineered extraction mechanism. The Ulysses chapters (chp03, chp06, chp14, chp15) also appear as A+ — their literary constraints show the same pattern of naturalized extraction.

**Red flag:** This report is from a significantly older/smaller corpus (342 vs 1025 constraints). Its counts and percentages don't apply to the current corpus. It should be re-run.

### 12. invertibility_report.md

**Summary:** Tests whether context-indexed classifications can be reconstructed from type information alone (Strategy B roundtrip test). 83% overall success (4731/5700 roundtrips). Single-type orbits ([mountain], [rope], [scaffold], [tangled_rope]) achieve 100%. The [rope,snare,tangled_rope] orbit (52 constraints) achieves only 42.8%.

**Most surprising finding:** The [rope,snare,tangled_rope] orbit carries **irreducible structural information** — 2 distinct context → type patterns coexist within the same orbit family. Pattern 1 (37 constraints): powerless=tangled_rope, moderate=tangled_rope, institutional=rope, analytical=snare. Pattern 2 (15 constraints): powerless=tangled_rope, moderate=snare, institutional=rope, analytical=snare. Knowing the orbit signature is NOT sufficient to predict which context sees which type. The difference is driven by the coalition upgrade mechanism and epsilon magnitude.

**Red flag:** None — the 17% failure rate is informative, not erroneous. The Scholze analogy section correctly identifies that the lossiness is by design (threshold gates intentionally discard distance-from-threshold information).

---

## PART 3: BUGS FOUND (prioritized)

### 1. CRITICAL: fnl_trace_diagnostic.pl KB wipe

`:- initialization(run_fnl_trace)` in `testsets/fnl_trace_diagnostic.pl` calls `scenario_manager:clear_kb/0` during batch loading, wiping 354 `constraint_claim` facts.

**Affected reports (numbers unreliable):** covering_analysis_post_fix.md, covering_analysis_post_patch.md, giant_component_analysis.md, coupling_protocol.md, coupling_protocol_post_fix.md, maxent_diagnostic_report.md.

**Fix:** Remove the initialization directive or move the file out of `testsets/`.

### 2. MODERATE: meta_reporter.py omega counting regex

Line 62: `omega_\w+_(\w+)` captures only the last segment of omega names, causing 371 false dedup collisions. Reports 507 instead of ~878.

**Fix:** Change to `(omega_\w+)`.

### 3. MODERATE: Corpus ↔ Orbit ID normalization

137/133 ID mismatches from case folding, spelling typos, naming convention differences, and Ulysses chapter-vs-episode naming. Silently degrades every cross-pipeline join.

**Fix:** Canonicalize IDs from testset filenames in both Python and Prolog pipelines.

### 4. MODERATE: FNL diagnostic trace contaminates report files

covering_analysis_post_fix.md, covering_analysis_post_patch.md, giant_component_analysis.md, coupling_protocol_post_fix.md all have hundreds of lines of Prolog diagnostic trace prepended to the actual report. The Prolog pipeline doesn't suppress stdout from the initialization directive.

**Fix:** Same as bug 1, or redirect stdout during batch loading.

### 5. MINOR: Domain name normalization

`Political` vs `political`, `Physics` vs `physics`, `Social` vs `social` appear as separate domains. Affects variance_analysis.md and index_sufficiency.md domain breakdowns.

### 6. MINOR: Empty/trivial files

verify_opaque_report.md (0 bytes), structured_analysis.json (2 bytes `{}`).

### 7. MINOR: corpus_data.json has 1 constraint with None type

Off-by-one vs meta_report's 1025.

### 8. MINOR: classification_audit_report.md is stale

Generated from 342/666 corpus, not current 1025.

### 9. MINOR: coupling_protocol_post_fix.md is identical to coupling_protocol.md

The "post_fix" regeneration ran under the same fnl_trace conditions, producing identical output.
