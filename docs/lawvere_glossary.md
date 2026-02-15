# Lawvere Glossary

## Three-Vocabulary Mapping: Code / Domain / Categorical

Each entry maps a predicate or concept across three vocabularies:
- **Code** — the Prolog predicate name, file, and line number
- **Domain** — the Deferential Realism term
- **Categorical** — the Lawvere/topos-theoretic concept

**Rigor levels:** STRICT (formal correspondence), STRUCTURAL (productive analogy, not formally proven), LOOSE (evocative only).

---

## 1. The Site

| Categorical Concept | Code (file:line) | Domain Term | Rigor | Notes |
|---------------------|-------------------|-------------|-------|-------|
| Site (small category) | `context/4` — convention throughout | Observer position | STRICT | 4-tuple (Power, Time, Exit, Scope) |
| Object of the site | `standard_context/1` — dirac_classification.pl:69 | Standard observer | STRICT | 4 standard contexts form the cover |
| Valid object | `valid_context/1` — constraint_indexing.pl:112 | Well-formed context | STRICT | Validates context tuple structure |
| Default object | `default_context/1` — constraint_indexing.pl:127 | Analytical perspective | STRICT | The "analytical" stalk |
| Morphism (power ordering) | `power_order/2` — drl_core.pl:198 | Power hierarchy | STRICT | powerless < ... < analytical |
| Morphism (scope ordering) | `scope_modifier/2` — constraint_indexing.pl:211 | Scope hierarchy | STRICT | local < ... < universal |

## 2. The Presheaf

| Categorical Concept | Code (file:line) | Domain Term | Rigor | Notes |
|---------------------|-------------------|-------------|-------|-------|
| Presheaf evaluation | `dr_type/3` — drl_core.pl:333 | Constraint classification | STRICT | Maps (constraint, context) → type; NOT a sheaf |
| Stalk computation | `classify_from_metrics/6` — drl_core.pl:252 | Threshold cascade | STRICT | Evaluates presheaf at a single context |
| Metric-level stalk | `metric_based_type_indexed/3` — drl_core.pl:356 | Raw metric classification | STRICT | Pre-signature stalk value |
| Temporal stalk | `dr_type_at/4` — drl_modal_logic.pl:269 | Historical classification | STRUCTURAL | Presheaf indexed by time + context |
| Cross-stalk query (∃) | `snare_immutability_check/1` — drl_core.pl:188 | Mutability gate | STRUCTURAL | Existential quantifier over site points |
| Signature integration | `integrate_signature_with_modal/3` — structural_signatures.pl | Override application | LOOSE | Post-composition modifying presheaf values |

## 3. The Type Space (Ω)

| Categorical Concept | Code (file:line) | Domain Term | Rigor | Notes |
|---------------------|-------------------|-------------|-------|-------|
| Carrier set of Ω | 8 type atoms — throughout | Constraint types | STRICT | {mountain, rope, tangled_rope, snare, scaffold, piton, indexically_opaque, unknown} |
| Binary operation on Ω | `composition_rule/3` — drl_modal_logic.pl:192 | Type composition | LOOSE | NOT a lattice meet — two absorbing elements |
| Identity element | `unknown` — drl_modal_logic.pl:219 | Unknown type | LOOSE | composition_rule(X, unknown, X) by fallback |
| Absorbing element 1 | `mountain` — drl_modal_logic.pl:192 | Natural law absorption | LOOSE | mountain ∘ X = mountain for all X |
| Absorbing element 2 | `piton` — drl_modal_logic.pl:215 | Performative absorption | LOOSE | piton ∘ X = piton (but mountain wins over piton) |
| Priority cascade | `classify_from_metrics/6` clauses — drl_core.pl:252-321 | Classification priority | LOOSE | Mountain > Snare > Scaffold > Rope > ... |

## 4. Signature Resolution

| Categorical Concept | Code (file:line) | Domain Term | Rigor | Notes |
|---------------------|-------------------|-------------|-------|-------|
| Priority override | `resolve_modal_signature_conflict/3` — structural_signatures.pl:741 | Signature-metric conflict | LOOSE | NOT a meet — conditional dispatch table |
| Signature detection | `constraint_signature/2` — structural_signatures.pl:84 | Structural signature | STRUCTURAL | Classifies constraint origin/structure |
| Override composition | `integrate_signature_with_modal/3` — structural_signatures.pl | Override pipeline | LOOSE | Combines metric type with signature |
| Natural law signal | `natural_law` signature — structural_signatures.pl:741 | Natural law claim | STRICT | Unconditional override → mountain |
| FNL signal | `false_natural_law` signature — structural_signatures.pl:745 | Physics-washed constraint | STRICT | Unconditional override → tangled_rope |
| CI_Rope signal | `coupling_invariant_rope` signature — structural_signatures.pl:749 | True coordination | STRICT | Unconditional override → rope |

## 5. Scaling and Perspective

| Categorical Concept | Code (file:line) | Domain Term | Rigor | Notes |
|---------------------|-------------------|-------------|-------|-------|
| Parametric family | `extractiveness_for_agent/3` — constraint_indexing.pl:351 | Power-scaled extractiveness | STRUCTURAL | χ = ε × sigmoid(π(P)) × σ(S); NOT a formal adjunction |
| Scaling function | `sigmoid_f/2` — constraint_indexing.pl:231 | Sigmoid transformation | STRUCTURAL | Monotone map on power dimension |
| Power parameter | `power_modifier/2` — constraint_indexing.pl:198 | Power index π | STRUCTURAL | Maps power level to scaling exponent |
| Scope parameter | `scope_modifier/2` — constraint_indexing.pl:211 | Scope scaling σ | STRUCTURAL | Maps scope level to multiplier |
| Reform threshold | `snare_reform_threshold/2` — drl_core.pl:217 | Minimum power for reform | STRUCTURAL | Threshold in the parametric family |

## 6. Naturality Tests

| Categorical Concept | Code (file:line) | Domain Term | Rigor | Notes |
|---------------------|-------------------|-------------|-------|-------|
| Naturality square test | `cross_index_coupling/2` — structural_signatures.pl:893 | Cross-index coupling | STRICT | Tests factorizability on Power × Scope grid |
| Naturality condition | `boltzmann_compliant/2` — structural_signatures.pl:824 | Boltzmann compliance | STRICT | compliant / non_compliant / inconclusive |
| Expected non-commutativity | `expected_power_divergence/4` — structural_signatures.pl:1033 | Expected variance | STRICT | Known-legitimate non-commutativity pairs |
| Epistemic access guard | `epistemic_access_check/2` — structural_signatures.pl | Sufficient data check | STRICT | Guards against testing with insufficient classifications |
| Complexity threshold | `complexity_adjusted_threshold/2` — structural_signatures.pl | Coupling threshold | STRICT | Adjusts naturality threshold for coordination type complexity |
| IB-adjusted threshold | `ib_adjusted_threshold/2` — structural_signatures.pl:1537 | Theater-aware threshold | STRICT | Reduces complexity offset when theater ratio is high |
| Nonsensical coupling | `detect_nonsensical_coupling/3` — structural_signatures.pl | Coupling without function | STRICT | Classification change without functional justification |
| Excess extraction | `excess_extraction/2` — structural_signatures.pl | Above-floor extraction | STRICT | Extraction above Boltzmann minimum |
| Universal naturality | `boltzmann_invariant_mountain/2` — structural_signatures.pl | Mountain invariance | STRICT | Classification constant across all dimensions |

## 7. Naturality Certificates and Failures

| Categorical Concept | Code (file:line) | Domain Term | Rigor | Notes |
|---------------------|-------------------|-------------|-------|-------|
| Naturality failure witness | `false_natural_law/2` — structural_signatures.pl:1345 | False Natural Law | STRICT | Claims naturality, fails Boltzmann |
| Naturality certificate | `coupling_invariant_rope/2` — structural_signatures.pl:1412 | CI Rope | STRICT | Passes all four naturality conditions |
| Partial failure witness | `false_ci_rope/2` — structural_signatures.pl:1591 | False CI Rope | STRICT | Appears as rope, fails structural tests |
| Scope invariance test | `scope_invariance_test/2` — structural_signatures.pl | Scope consistency | STRICT | Classification constant across scope levels |

## 8. Purity and Contamination

| Categorical Concept | Code (file:line) | Domain Term | Rigor | Notes |
|---------------------|-------------------|-------------|-------|-------|
| Naturality health scalar | `purity_score/2` — structural_signatures.pl:1724 | Purity score | STRICT | Weighted composite of 4 naturality subscores |
| Purity classification | `structural_purity/2` — structural_signatures.pl:1458 | Structural purity class | STRICT | pure_natural_law / pure_coordination / contaminated |
| Factorization subscore | `factorization_subscore/2` — structural_signatures.pl:1737 | Factorization health | STRICT | 1 - coupling_score |
| Scope invariance subscore | `scope_invariance_subscore/2` — structural_signatures.pl:1745 | Scope health | STRICT | Penalized per extra classification type |
| Coupling cleanliness subscore | `coupling_cleanliness_subscore/2` — structural_signatures.pl:1758 | Coupling health | STRICT | 1 - nonsensical coupling strength |
| Excess extraction subscore | `excess_extraction_subscore/2` — structural_signatures.pl:1767 | Extraction health | STRICT | 1 - min(1, excess × 2) |
| Contravariant purity flow | `effective_purity/4` — drl_modal_logic.pl:1500 | Effective purity | STRUCTURAL | Intrinsic - contamination × immunity |
| Contamination emission | `type_contamination_strength/2` — drl_modal_logic.pl:1474 | Contamination factor | STRUCTURAL | Per-type emission coefficient |
| Contamination susceptibility | `type_immunity/2` — drl_modal_logic.pl:1487 | Type immunity | STRUCTURAL | 0 (mountain, immune) to 1 (rope, fully susceptible) |
| Edge contamination | `compute_edge_contamination/7` — drl_modal_logic.pl:1542 | Neighbor contamination | STRUCTURAL | Delta × attenuation × type_factor, capped |

## 9. Fixed-Point Network (FPN)

| Categorical Concept | Code (file:line) | Domain Term | Rigor | Notes |
|---------------------|-------------------|-------------|-------|-------|
| Greatest fixed point computation | `fpn_run/2` — drl_modal_logic.pl:1859 | FPN run | STRUCTURAL | Discovers constraints, delegates to fpn_run/3 |
| Jacobi iteration | `fpn_iterate/5` — drl_modal_logic.pl:1890 | FPN iteration loop | STRUCTURAL | Simultaneous update until convergence |
| Fixed-point state | `fpn_ep/3` — drl_modal_logic.pl:113 (dynamic) | Effective purity at equilibrium | STRUCTURAL | State vector of the iteration |
| Initial state | `fpn_intrinsic/2` — drl_modal_logic.pl:114 (dynamic) | Intrinsic purity | STRUCTURAL | Top element for downward iteration |
| Per-node computation | `fpn_compute_ep/3` — drl_modal_logic.pl:1917 | Per-constraint EP | STRUCTURAL | Reads neighbors from fpn_ep, computes new EP |
| Convergence metadata | `fpn_iteration_info/4` — drl_modal_logic.pl (dynamic) | Iteration result | STRUCTURAL | (Context, Iterations, MaxDelta, Converged) |

## 10. Probabilistic Layer (MaxEnt)

| Categorical Concept | Code (file:line) | Domain Term | Rigor | Notes |
|---------------------|-------------------|-------------|-------|-------|
| Distribution on Ω | `maxent_distribution/3` — maxent_classifier.pl:365 | MaxEnt distribution | STRUCTURAL | Probability measure over 6 types; NOT full Giry monad |
| Uncertainty measure | `maxent_entropy/3` — maxent_classifier.pl:369 | Normalized Shannon entropy | STRUCTURAL | H/log(6) ∈ [0,1] |
| Confidence (1 - H) | `maxent_confidence/3` — maxent_classifier.pl:380 | MaxEnt confidence | STRUCTURAL | Complement of normalized entropy |
| Maximum-probability type | `maxent_top_type/3` — maxent_classifier.pl:385 | Shadow top type | STRUCTURAL | argmax of distribution |
| Cross-functor disagreement | `maxent_disagreement/3` — maxent_classifier.pl:401 | MaxEnt disagreement | STRUCTURAL | hard / soft / entropy_flag / none |
| Per-type log-likelihood | `maxent_type_log_likelihood/5` — maxent_classifier.pl:244 | Combined log-likelihood | STRUCTURAL | Continuous + boolean + prior |
| Log-sum-exp normalization | `normalize_log_probs/2` — maxent_classifier.pl:260 | Stable normalization | STRUCTURAL | Prevents overflow/underflow |
| Shannon entropy | `shannon_entropy/2` — maxent_classifier.pl:351 | Shannon entropy (nats) | STRUCTURAL | -Σ p log p |
| Gaussian log-likelihood | `gaussian_log_likelihood/4` — maxent_classifier.pl:105 | Gaussian kernel | STRUCTURAL | MaxEnt distribution given first two moments |
| Main entry point | `maxent_run/2` — maxent_classifier.pl:604 | MaxEnt pipeline | STRUCTURAL | Cleanup → precompute → classify all |

## 11. Abductive Layer

| Categorical Concept | Code (file:line) | Domain Term | Rigor | Notes |
|---------------------|-------------------|-------------|-------|-------|
| Cross-functor auditor | `abductive_run/2` — abductive_engine.pl:600 | Abductive pipeline | STRUCTURAL | Runs all 8 triggers across all constraints |
| Artifact diagnosis | `trigger_signature_override_artifact/3` — abductive_engine.pl:158 | Override artifact | STRUCTURAL | Explains disagreement as known override effect |
| Deep deception detector | `trigger_deep_deception/3` — abductive_engine.pl:209 | Deep deception | STRUCTURAL | FNL + MaxEnt P(mountain) > threshold |
| Metric-structural divergence | `trigger_metric_structural_divergence/3` — abductive_engine.pl:260 | Metric-structural gap | STRUCTURAL | High entropy + preserved orbit |
| Confirmed liminal | `trigger_confirmed_liminal/3` — abductive_engine.pl:314 | Confirmed transition | STRUCTURAL | Three independent signals agree on transition |
| Coverage gap | `trigger_coverage_gap/3` — abductive_engine.pl:372 | Diagnostic gap | STRUCTURAL | Dirac detects variance, mismatch misses it |
| Accelerating pathology | `trigger_accelerating_pathology/3` — abductive_engine.pl:424 | Active worsening | STRUCTURAL | FPN zone migration + drift |
| Contamination cascade | `trigger_contamination_cascade/3` — abductive_engine.pl:476 | Network spread | STRUCTURAL | Multi-hop worse than one-hop + network drift |
| Dormant extraction | `trigger_dormant_extraction/3` — abductive_engine.pl:528 | Hidden extraction | STRUCTURAL | Clean metrics + extractive voids |
| Genuine findings | `abductive_genuine/2` — abductive_engine.pl:694 | Non-artifact hypotheses | STRUCTURAL | Unexpected non-commutativity |
| Artifact census | `abductive_artifacts/2` — abductive_engine.pl:703 | Expected non-commutativity | STRUCTURAL | Known override-caused disagreements |

## 12. Trajectories and Isomorphism

| Categorical Concept | Code (file:line) | Domain Term | Rigor | Notes |
|---------------------|-------------------|-------------|-------|-------|
| Natural transformation representative | `constraint_trajectory/3` — trajectory_mining.pl:106 | Constraint trajectory | STRUCTURAL | Full presheaf evaluation across 4 contexts |
| Trajectory metric | `trajectory_distance/4` — trajectory_mining.pl:313 | 4-component distance | STRUCTURAL | shift + metric + stability + pathology |
| Type distance matrix | `type_distance/3` — trajectory_mining.pl:276 | Pairwise type distance | STRUCTURAL | Domain-aware distance between types |
| Natural isomorphism test | `structural_isomorphism/4` — trajectory_mining.pl:737 | Structural isomorphism | STRUCTURAL | strict / trajectory / family / none |
| Equivalence class | `structural_family/2` — trajectory_mining.pl:721 | Structural family | STRUCTURAL | HAC cluster membership |
| Cluster members | `cluster_members/2` — trajectory_mining.pl:727 | Family members | STRUCTURAL | All constraints in a family |
| HAC clustering | `trajectory_cluster/3` — trajectory_mining.pl:712 | Trajectory clustering | STRUCTURAL | Hierarchical agglomerative clustering |
| Cross-domain twins | `cross_domain_twins/3` — trajectory_mining.pl:810 | Cross-domain twins | STRUCTURAL | Same family, different domains |
| Main pipeline | `trajectory_run/2` — trajectory_mining.pl:844 | Trajectory mining | STRUCTURAL | Compute → distance → cluster → assign |

## 13. Gauge Orbits

| Categorical Concept | Code (file:line) | Domain Term | Rigor | Notes |
|---------------------|-------------------|-------------|-------|-------|
| Orbit under site automorphisms | `gauge_orbit/2` — dirac_classification.pl:156 | Gauge orbit | STRICT | All type evaluations across standard contexts |
| Invariance test | `preserved_under_context_shift/2` — dirac_classification.pl:178 | Context preservation | STRICT | preserved(Type) or violated(Transitions) |
| Orbit equivalence class | `dirac_class/3` — dirac_classification.pl:263 | Dirac class | STRICT | Classification by orbit structure |
| Fixed frame detection | `gauge_fixed/3` — dirac_classification.pl:214 | Gauge-fixed frame | STRICT | Observer sees mountain/snare but others disagree |
| Gauge freedom | `gauge_freedom/3` — dirac_classification.pl:350 | Gauge freedom | STRICT | Amount of classification variance across contexts |

## 14. Lifecycle and Drift

| Categorical Concept | Code (file:line) | Domain Term | Rigor | Notes |
|---------------------|-------------------|-------------|-------|-------|
| Temporal presheaf | `constraint_history/3` — drl_modal_logic.pl:252 | Constraint history | STRUCTURAL | Presheaf evaluations along time dimension |
| Transformation detection | `transformation_detected/5` — drl_modal_logic.pl:280 | Type transition | STRUCTURAL | (C, From, To, T1, T2) |
| Drift scanning | `scan_constraint_drift/2` — drl_lifecycle.pl:1052 | Drift event detection | STRUCTURAL | Collects all drift events for a constraint |
| Drift events | `drift_event/3` — drl_lifecycle.pl:various | Degradation signals | STRUCTURAL | theater_escalation, extraction_accumulation, etc. |

## 15. Logical Fingerprints

| Categorical Concept | Code (file:line) | Domain Term | Rigor | Notes |
|---------------------|-------------------|-------------|-------|-------|
| Presheaf invariant | `logical_fingerprint/2` — logical_fingerprint.pl:88 | Logical fingerprint | STRUCTURAL | 7-dimensional qualitative shape |
| Perspectival dimension | `fingerprint_shift/2` — logical_fingerprint.pl:110 | Shift pattern | STRUCTURAL | Type across 4 power levels |
| Negative space | `fingerprint_voids/2` — logical_fingerprint.pl:205 | Diagnostic absences | STRUCTURAL | Missing structural properties |
| Metric regime | `fingerprint_zone/2` — logical_fingerprint.pl:350 | Zone classification | STRUCTURAL | Categorical metric region |
| Coupling topology | `fingerprint_coupling/2` — logical_fingerprint.pl:413 | Boltzmann coupling shape | STRICT | Category + score + compliance + purity |
| Shift family | `shift_family/2` — logical_fingerprint.pl:478 | Constraints sharing shift | STRUCTURAL | Grouping by perspectival behavior |
| Fingerprint matching | `fingerprint_match/4` — logical_fingerprint.pl:504 | Dimension-by-dimension match | STRUCTURAL | Compare two constraints on selected dimensions |

## 16. Mismatch Detection

| Categorical Concept | Code (file:line) | Domain Term | Rigor | Notes |
|---------------------|-------------------|-------------|-------|-------|
| Presheaf non-commutativity | `dr_mismatch/4` — drl_core.pl:442 | Classification error | STRUCTURAL | Detects type vs. claim mismatches |
| Claim mismatch | `dr_claim_mismatch/4` — drl_core.pl:471 | Claim-type gap | STRUCTURAL | false_summit, snare_as_rope, piton_as_snare |
| Cross-context analysis | `cross_context_analysis/2` — drl_core.pl:557 | Multi-context report | STRUCTURAL | Full analysis across all standard contexts |

## 17. Cohomological Invariants

| Categorical Concept | Code (file:line) | Domain Term | Rigor | Notes |
|---------------------|-------------------|-------------|-------|-------|
| Cech H⁰ (global sections) | `cohomological_obstruction/3` — grothendieck_cohomology.pl | Gauge-invariant constraints | STRICT | Presheaf constant on cover ↔ context-invariant classification |
| Cech H¹ proxy (obstruction count) | `cohomological_obstruction/3` — grothendieck_cohomology.pl | Perspectival obstruction | STRUCTURAL | Disagreeing context-pairs (0..6); proxy for Cech 1-cocycle on discrete 4-point site |
| Descent condition | `descent_status/2` — grothendieck_cohomology.pl | Presheaf gluing test | STRICT | descends(Type) = global section exists; fails_descent(H1, Types) = gluing obstruction |
| Presheaf topos | PSh(C) — theoretical, documented in grothendieck_framing.md | Category of context-dependent classifications | STRICT | Functor category Set^{C^op} on the 4-object site |
| Sheafification | Not implemented — theoretical operation | Forced consensus classification | — | Would produce closest sheaf to the DR presheaf; discussed as open problem in framing document |
