# Binary Structural Gates

**Date:** 2026-02-22
**Prerequisite:** *Two-Hub Classification Architecture* (two_hub_architecture.md)
**Computational basis:** `outputs/boolean_independence_report.md` (1,148 constraints)
**Audience:** Anyone asking what the framework decides *before* the sigmoid machinery runs, or why two constraints with similar continuous metrics can receive different classifications.

---

## 1. Introduction

Binary structural gates are the hard classification boundaries that operate upstream of the continuous metrics (ε, σ, τ). They determine what *kind* of thing a constraint is before `classify_from_metrics/6` applies threshold logic to the continuous values. A constraint that fails `emerges_naturally/1` will never classify as mountain regardless of how low its extraction is. A constraint that passes `has_coordination_function/1` opens the scaffold gate regardless of its suppression score.

These gates matter because they are the observable residue of intent without requiring intent as an input. The framework does not ask *why* a constraint was created; it asks whether the constraint has structural properties — beneficiaries, victims, enforcement mechanisms, expiration dates — that correlate with different constraint architectures. The binary gate profile is what distinguishes structurally distinct constraints whose continuous metrics may overlap.

Consider the restricted-view case: a communal narcissist system and biological curiosity can both produce the distortion tangled_rope → rope when viewed through a restricted observer position. The continuous metrics in the restricted view may be similar. But their binary gate profiles differ completely: biological curiosity has `emerges_naturally=T`, `requires_active_enforcement=F`, `has_asymmetric_extraction=F`; a communal narcissist system has `emerges_naturally=F`, `requires_active_enforcement=T`, `has_asymmetric_extraction=T`. The gates tell you what you are looking at before the numbers run.

The binary gates interact with the two-hub architecture described in *two_hub_architecture.md*. As that document notes: "The raw metrics — base extractiveness ε, suppression σ, theater ratio τ, the boolean structural features — are not indexed." The boolean structural features are computed once from testset declarations and relational data. They are structural facts about the constraint. Classification is what introduces observer-dependence — but the gates constrain which classification paths are available.

---

## 2. Inventory by Structural Dimension

The framework contains approximately 65 binary gates. They are organized below by what structural dimension they discriminate, not by implementation category or file location.

### 2.1 Origin — Where the constraint comes from (7 gates)

These gates distinguish natural law from constructed coordination from designed extraction.

| Gate | Arity | Tests | Location | Consumed by |
|------|-------|-------|----------|-------------|
| `emerges_naturally` | /1 | Constraint arises from natural process, not human design | `domain_priors.pl:27` (multifile stub) | Mountain gate (`drl_core.pl:293`), rope gate (`:322`), `boolean_spec` (`:175`) |
| `is_constructed` | — | Constraint is a product of human construction | `python/boolean_independence.py` only | Independence analysis; not implemented in Prolog |
| `natural_law_without_beneficiary` | /1 | Emerges naturally, no enforcement, no human beneficiary | `drl_core.pl:283` | Blocks snare gate (`:297`), blocks tangled_rope gate (`:326`) |
| `natural_law_signature` | /1 | Profile matches natural law structural pattern | `signature_detection.pl:269` | `constraint_signature/2` (`:86`) |
| `false_natural_law` | /2 | Claims natural law but fails Boltzmann cross-index test | `signature_detection.pl:772` | `constraint_signature/2` (`:69`) |
| `effective_immutability = mountain` | /3 | (TimeHorizon, ExitOptions) → mountain perception | `constraint_indexing.pl:171` | Mountain gate (`drl_core.pl:293`), rope gate (`:322`) |
| `has_viable_alternatives` | /2 | Viable alternatives were considered (choice vs. necessity) | `signature_detection.pl:166` | Profile construction (`:133`), freedom description (`dirac_classification.pl:364`) |

`emerges_naturally` and `is_constructed` are near-inverses (NMI = 0.870 and 0.873 respectively against the type space). `natural_law_without_beneficiary` is the conjunction that blocks the snare and tangled_rope paths: if a constraint emerges naturally, requires no enforcement, and has no identifiable human beneficiary, it cannot be classified as extractive.

### 2.2 Mechanism — How the constraint operates (7 gates)

These gates distinguish enforcement-dependent from self-sustaining, coordinating from extracting, and genuine from theatrical.

| Gate | Arity | Tests | Location | Consumed by |
|------|-------|-------|----------|-------------|
| `requires_active_enforcement` | /1 | Constraint requires ongoing enforcement to persist | `domain_priors.pl:26` (multifile stub) | Tangled_rope gate (`drl_core.pl:335`), scaffold temporality (`:276`), `boolean_spec` (`:177`) |
| `has_coordination_function` | /1 | Constraint solves a collective action problem | `narrative_ontology.pl:251` | Scaffold gate (`drl_core.pl:309`), tangled_rope gate (`:336`) |
| `has_asymmetric_extraction` | /1 | Asymmetric beneficiary distribution (has victims) | `narrative_ontology.pl:259` | Tangled_rope gate (`drl_core.pl:337`) |
| `scaffold_temporality_check` | /1 | Has sunset clause OR does not require active enforcement | `drl_core.pl:273` | Scaffold gate (`:310`) |
| `coercion_without_coordination` | void | Active enforcement but no coordination function | `logical_fingerprint.pl:223` | Structural void diagnostic |
| `undocumented_coordination` | void | Enforcement + no beneficiaries + S > 0.30 | `logical_fingerprint.pl:260` | Structural void diagnostic |
| TR ≥ 0.70 | — | Theater ratio exceeds scaffold exclusion threshold | `drl_core.pl:312` | Scaffold gate exclusion, piton gate entry (`:346`) |

**Redundancy note:** `has_coordination_function/1` at `narrative_ontology.pl:251` checks `constraint_beneficiary(C, _)`. `structural_property_holds(C, has_beneficiaries)` at `logical_fingerprint.pl:180` checks the same predicate. These are the same underlying test. Similarly, `has_asymmetric_extraction/1` at `narrative_ontology.pl:259` checks `constraint_victim(C, _)`, which is the same test as `structural_property_holds(C, has_victims)` at `logical_fingerprint.pl:183`.

### 2.3 Lifecycle — Temporal properties of the constraint (9 gates)

These gates distinguish static from drifting from expiring.

| Gate | Arity | Tests | Location | Consumed by |
|------|-------|-------|----------|-------------|
| `has_sunset_clause` | /1 | Constraint has an explicit expiration mechanism | `narrative_ontology.pl:326` (dynamic, default fail) | Scaffold temporality (`drl_core.pl:274`), void checks |
| `has_temporal_data` | — | Measurement data exists for this constraint | `logical_fingerprint.pl:186` | Fingerprint construction |
| `drifting_without_limit` | void | Temporal data + no sunset + ε > rope ceiling | `logical_fingerprint.pl:250` | Structural void diagnostic |
| `detect_metric_substitution` | /1 | Drift: original metric replaced by proxy | `metric_drift_events.pl:103` | Standalone detector; parallel clause in `drift_event/3` (`:186`) |
| `detect_extraction_accumulation` | /1 | Drift: extraction increasing over time | `metric_drift_events.pl:114` | Standalone detector; parallel clause in `drift_event/3` (`:194`) |
| `detect_coordination_loss` | /1 | Drift: coordination function degrading | `metric_drift_events.pl:124` | Standalone detector; parallel clause in `drift_event/3` (`:202`) |
| `detect_function_obsolescence` | /1 | Drift: original function no longer served | `metric_drift_events.pl:138` | Standalone detector; parallel clause in `drift_event/3` (`:212`) |
| `detect_sunset_violation` | /1 | Drift: constraint persists past its sunset clause | `metric_drift_events.pl:150` | Standalone detector; parallel clause in `drift_event/3` (`:217`) |
| `detect_extraction_dried_up` | /1 | Drift: extraction dropped to near-zero | `metric_drift_events.pl:160` | Standalone detector; parallel clause in `drift_event/3` (`:223`) |

The six `detect_*` predicates are simple boolean detectors. Each has a parallel `drift_event/3` clause that tests the same drift pattern but returns structured evidence. The `drift_event/3` and `drift_event/4` (context-indexed) variants feed into severity classification (`drift_severity/3`) and drift velocity/acceleration computations.

### 2.4 Topology — Relational structure of the constraint (6 gates + thresholds)

These gates characterize the constraint's actor and coupling structure.

| Gate | Arity | Tests | Location | Consumed by |
|------|-------|-------|----------|-------------|
| `has_beneficiaries` | — | At least one documented beneficiary | `logical_fingerprint.pl:180` | Fingerprint properties |
| `has_victims` | — | At least one documented victim | `logical_fingerprint.pl:183` | Fingerprint properties |
| `fingerprint_actors` | /2 | Actor topology: (BenefTopology, VictimTopology) each ∈ {none, concentrated, distributed} | `logical_fingerprint.pl:296` | Fingerprint construction (`:94`), dimension matching (`:531`) |
| `nonsensically_coupled` | — | Strong coupling (score > 0.50) with no coordination function | `logical_fingerprint.pl:441` | Coupling categorization |
| `coupling_invariant_rope` | /2 | Passes Boltzmann compliance + scope invariance + low excess ε + has coordination | `signature_detection.pl:840` | `constraint_signature/2` (`:94`) |
| `false_ci_rope` | /2 | Appears as CI-rope but fails cross-index tests | `signature_detection.pl:986` | `constraint_signature/2` (`:76`) |

**Coupling thresholds** (from `config.pl`):
- ≤ 0.25 (`boltzmann_coupling_threshold`, line 267): independent
- \> 0.50 (`boltzmann_coupling_strong_threshold`, line 270): strongly coupled / nonsensically coupled

`has_beneficiaries` and `has_victims` are topology gates, not mechanism gates — they test the *shape* of the actor network, not the *function* of the constraint. The distinction matters because `has_coordination_function/1` checks the same underlying data (`constraint_beneficiary/2`) but interprets it as a mechanism property. This is a conceptual overload in the current implementation, noted below in § 7.

### 2.5 Diagnostic — Cross-index and structural integrity tests (8 gates)

These gates test whether a constraint's structural properties are internally consistent.

| Gate | Arity | Tests | Location | Consumed by |
|------|-------|-------|----------|-------------|
| `boltzmann_compliant` | /2 | Classification factorizes across Power × Scope dimensions | `boltzmann_compliance.pl:88` | Coupling fingerprint (`logical_fingerprint.pl:431`), signature detection |
| `epistemic_access_check` | /2 | Sufficient indexed classifications for reliable Boltzmann test | `boltzmann_compliance.pl:411` | Coupling fingerprint (`logical_fingerprint.pl:416`) |
| `snare_immutability_check` | /1 | Changeability from current OR any standard higher-power perspective | `drl_core.pl:224` | Snare gate (`:304`) |
| `extractive_immutable` | void | Claims mountain but has measurable extraction (ε > mountain max) | `logical_fingerprint.pl:269` | Structural void diagnostic |
| `self_sustaining_extraction` | void | Asymmetric extraction without active enforcement | `logical_fingerprint.pl:278` | Structural void diagnostic |
| `unenforced_suppression` | void | High suppression (S ≥ snare floor) without enforcement or natural emergence | `logical_fingerprint.pl:230` | Structural void diagnostic |
| `unaccountable_extraction` | void | Extraction at snare level (ε ≥ snare floor) without sunset clause | `logical_fingerprint.pl:214` | Structural void diagnostic |
| `no_exit_for_victims` | void | Documented victims + no sunset + ε ≥ tangled_rope floor | `logical_fingerprint.pl:240` | Structural void diagnostic |

The void predicates are diagnostic absences: they fire when structural properties are *missing* in combinations that are architecturally significant. A constraint with high extraction, documented victims, and no sunset clause (`no_exit_for_victims`) is not structurally invalid — it is structurally *informative*. The void tells you something about the constraint's design that the continuous metrics do not.

---

## 3. The Binary Fingerprint

Each constraint's binary gate profile yields approximately 12 independent bits of structural information. These bits encode the constraint's structural identity prior to any continuous metric computation.

| Dimension | Bits | Source |
|-----------|------|--------|
| Origin | 1 | `emerges_naturally` |
| Enforcement | 1 | `requires_active_enforcement` |
| Expiration | 1 | `has_sunset_clause` |
| Coordination | 1 | `has_coordination_function` |
| Extraction asymmetry | 1 | `has_asymmetric_extraction` |
| Extraction level | ~2 | ε across 4 thresholds (mountain max 0.25, rope ceiling 0.15, tangled_rope floor 0.16, snare floor 0.46) |
| Suppression level | ~1 | S across 3 thresholds (mountain ceiling 0.05, rope ceiling 0.16, snare floor 0.60) |
| Theater | 1 | TR ≥ 0.70 |
| Immutability perception | 1 | Hub 2 (`effective_immutability/3`) |
| Coupling topology | ~1.5 | independent / weakly coupled / strongly coupled + nonsensical flag |
| Actor topology | ~0.5 | concentrated / distributed (from `fingerprint_actors/2`) |
| Temporal data | 1 | `has_temporal_data` |

The MaxEnt classifier declares the expected boolean profiles for each type via `boolean_spec/3` declarations (`maxent_classifier.pl:169–179`):

| Type | Feature | Constraint |
|------|---------|------------|
| mountain | `emerges_naturally` | required |
| mountain | `requires_active_enforcement` | forbidden |
| snare | `natural_law_without_beneficiary` | forbidden |
| scaffold | `has_coordination_function` | required |
| rope | `emerges_naturally` | bonus |
| tangled_rope | `natural_law_without_beneficiary` | forbidden |
| tangled_rope | `requires_active_enforcement` | required |
| tangled_rope | `has_coordination_function` | required |
| tangled_rope | `has_asymmetric_extraction` | required |

### Independence from the type space

The boolean independence analysis (`python/boolean_independence.py`, corpus of 1,148 constraints) measured how much information each binary gate carries beyond what the type classification already captures.

| Feature | NMI | Independence Score | Verdict |
|---------|-----|--------------------|---------|
| `emerges_naturally` | 0.870 | — | Redundant with type |
| `is_constructed` | 0.873 | — | Redundant with type |
| `natural_law_without_beneficiary` | 0.719 | — | Mostly captured |
| `has_coordination_function` | 0.705 | — | Mostly captured |
| `has_asymmetric_extraction` | 0.663 | — | Mostly captured |
| `requires_active_enforcement` | 0.663 | highest independence = 0.199 | Weakly captured |

The independence criteria required both independence score > 0.15 AND NMI < 0.3. No feature met both criteria. The type space already captures most binary information — but not all. `requires_active_enforcement` carries the most independent information (NMI = 0.663, the lowest in the set), meaning it is the gate least predictable from knowing only the type. This makes structural sense: a tangled_rope *requires* active enforcement by `boolean_spec` declaration, but other types have no such requirement, and enforcement status within those types varies.

---

## 4. Sufficiency Analysis

Binary gates are sufficient for three common structural questions and insufficient for a fourth.

### Cases binary gates handle

**1. Natural law vs. constructed coordination vs. designed extraction.** Two to three bits suffice: `emerges_naturally`, `requires_active_enforcement`, and `has_asymmetric_extraction`. A constraint that emerges naturally without enforcement and without victims is natural law. A constraint that requires enforcement with coordination function but no victims is constructed coordination. A constraint with enforcement, coordination, and victims is designed extraction (or at least extraction-entangled coordination). These are the structural archetypes, and the binary profile identifies them reliably.

**2. Self-sustaining vs. enforcement-dependent.** One bit suffices: `requires_active_enforcement`. If enforcement stops and the constraint persists, the constraint is self-sustaining — either genuinely natural or so thoroughly internalized that it no longer needs external enforcement. The structural void `self_sustaining_extraction` (`logical_fingerprint.pl:278`) flags the pathological case: extraction that persists without enforcement.

**3. Static vs. drifting vs. expiring.** Two bits suffice: `has_sunset_clause` and `has_temporal_data`. A constraint with a sunset clause is expiring (scaffold). A constraint with temporal data but no sunset clause is drifting. A constraint with neither is static (from the framework's perspective — it may change in ways the framework cannot observe).

### The case binary gates cannot handle alone

**4. Coordination-washed extraction vs. genuine coordination.** The binary profile is necessary but not sufficient. Both coordination-washed extraction and genuine coordination have `has_coordination_function=T`. Both may have `has_asymmetric_extraction=T` (coordination can have side effects that look like extraction). The binary gates can tell you the structural ingredients — enforcement, coordination, asymmetry — but they cannot tell you whether the coordination is *genuine* or is providing cover for extraction.

This connects to the central finding of *deferential_realism_paper_v2.md*: "The persistence of extraction *requires* this cover story." Every constraint classified as a snare from some observer position is classified differently from at least one other position. The cover story is structurally necessary. Binary gates can identify the ingredients of the story (enforcement present, coordination claimed, victims documented); only the Boltzmann cross-index tests can determine whether the story is genuine — whether the coordination function *factorizes* correctly across independent observer dimensions, or whether it breaks down in ways that reveal the coordination as a structural fig leaf.

---

## 5. The Two-Tier Architecture

The binary gates form a two-tier system with distinct computational characteristics and diagnostic reach.

### Tier 1: Binary Screening

Five base boolean gates plus threshold gates. Fast, deterministic, sufficient for approximately 80% of structural questions.

**Gates:** `emerges_naturally/1`, `requires_active_enforcement/1`, `has_coordination_function/1`, `has_asymmetric_extraction/1`, `has_sunset_clause/1`, plus the threshold checks on ε, σ, and TR within `classify_from_metrics/6`.

**Properties:**
- Computed directly from testset declarations and relational data (beneficiary/victim relationships, measurement records, domain priors)
- No cross-index analysis required
- Deterministic: same input always produces same output
- These are the "structural facts" referenced in *two_hub_architecture.md*: they are not indexed by observer position

Tier 1 answers: *What structural properties does this constraint have?* Does it emerge naturally? Is it enforced? Does it coordinate? Does it extract asymmetrically? Does it expire?

### Tier 2: Cross-Index Structural Tests

Boltzmann compliance, false natural law detection, false CI-rope detection, coupling-invariant rope verification.

**Gates:** `boltzmann_compliant/2` (`boltzmann_compliance.pl:88`), `false_natural_law/2` (`signature_detection.pl:772`), `false_ci_rope/2` (`signature_detection.pl:986`), `coupling_invariant_rope/2` (`signature_detection.pl:840`).

**Properties:**
- Require cross-index analysis: constraint must be classified across multiple (Power × Scope) grid positions
- Depend on `epistemic_access_check/2` (`boltzmann_compliance.pl:411`) — minimum 3 indexed classifications for reliability (`config.pl:279`)
- Test factorization: do the classification dimensions behave independently?
- Non-deterministic in the sense that adding more indexed classifications can change the result

Tier 2 answers: *Do this constraint's structural properties factorize correctly across independent observer dimensions?* A constraint that claims to be natural law should look like natural law from every observer position. A constraint whose classification varies systematically with observer power — where the powerless see mountain but the analytical observer sees snare — has failed the factorization test. Its Tier 1 profile may be structurally valid, but the cross-index structure reveals that the profile has been assembled in a way that does not decompose independently.

### Why two tiers

Tier 1 tests structural properties of the constraint itself. Tier 2 tests whether those properties *factorize* correctly across independent observer dimensions. This is the structural signature of genuine natural law vs. constructed mimicry: genuine natural law classifications are Boltzmann-invariant (they factorize), while constructed mimicry introduces coupling between dimensions that should be independent.

The abductive trigger T15 (`trigger_epistemic_trap/3`, `abductive_triggers.pl:875`) exemplifies the gap between tiers. It fires when a constraint's restricted-view classification (what a powerless observer sees given only the features accessible to that power level) diverges from the full-data classification. The restricted view produces a Tier 1 binary profile that looks correct — all the gates fire as expected for the restricted type. But the full-data Tier 2 analysis reveals that the profile is an artifact of epistemic restriction: the observer cannot see the features that would change the classification. The binary gates are not wrong; they are working on incomplete data, and only the cross-index test reveals the incompleteness.

---

## 6. Known Gap: Lateral Extraction

The power axis in the framework's context tuple is vertical: powerless → moderate → institutional → analytical. The `feature_access/3` table (`constraint_indexing.pl:595–640`) and the `canonical_d_for_power/2` mapping (`constraint_indexing.pl:262`) both assume this vertical ordering. Hub 1's sigmoid amplification (`sigmoid_f/2`, `constraint_indexing.pl:251`) is calibrated to this axis.

Some extraction is horizontal. Peer manipulation, workplace bullying between equals, communal narcissism between same-level actors — these are extraction patterns where the power differential is not between structural levels but between individuals at the same level.

The current framework handles these cases by distorting the structural relationship: the victim is coded as "powerless/trapped" and the extractor as "institutional," which forces the vertical machinery to produce the correct classification (snare) at the cost of misrepresenting the actual structural geometry. The binary gates can flag the distortion — `requires_active_enforcement=T` and `has_asymmetric_extraction=T` fire correctly — but they cannot represent that extraction flows laterally rather than vertically. The coupling fingerprint will show coupling, but along an axis that does not correspond to the actual power relationship.

This may require a relational dimension in the context tuple: the constraint changes classification based on the *relationship* between two same-level observers, not on the power level of either one. Alternatively, it may be partially addressable through `exit_options` — lateral extraction traps through social mechanisms (reputation, community belonging, emotional dependency) rather than structural power asymmetry, and the effective_immutability table already captures some of this through the (TimeHorizon, ExitOptions) → perception mapping.

This is an open problem. The binary gates are not broken for lateral extraction — they fire correctly. The representation of the power axis is what is inadequate. Whether this requires architectural change or can be resolved within the existing parameter space is not yet determined.

---

## 7. Recommendations

Minor housekeeping items identified during the audit:

1. **Centralize the inline S > 0.30 threshold.** The suppression threshold in `structural_void(C, undocumented_coordination)` at `logical_fingerprint.pl:264` is a hardcoded literal. It should be declared as a `param/2` fact in `config.pl` alongside the other suppression thresholds (mountain ceiling 0.05, rope ceiling 0.16, tangled_rope floor 0.40, snare floor 0.60).

2. **Document the Chi ≤ 0 net-beneficiary flip.** The conditional at `drl_core.pl:320` — which skips the base extraction gate when Chi is non-positive — is a named structural property ("negative effective extraction means the constraint is experienced as coordination infrastructure"). It should be documented as a named gate with a reference from `config.pl`, even though it does not have a standalone threshold parameter.

3. **Consider promoting `is_constructed` to Prolog.** Currently computed only in `python/boolean_independence.py`. Near-redundant with `¬emerges_naturally` (NMI = 0.873 vs. 0.870). Low priority — the Python analysis confirms the redundancy, and adding a Prolog predicate that carries no independent information is not worth the maintenance cost unless other uses emerge.

4. **Consider declaring `is_network_member/1`.** This predicate does not currently exist anywhere in the codebase. If network topology becomes a classification input — if the framework needs to distinguish constraints that operate within a network of mutual reinforcement from constraints that operate in isolation — this would be the natural declaration point. Currently speculative; no current gate requires it.

5. **Resolve the `has_coordination_function` / `has_beneficiaries` overload.** These are the same underlying check (`constraint_beneficiary(C, _)`) used for two conceptually distinct purposes: mechanism (does the constraint coordinate?) and topology (does the constraint have beneficiaries?). This is not a bug — coordination implies beneficiaries — but it means the two gates cannot vary independently. If a future testset declares a constraint with beneficiaries that does not coordinate (a pure transfer, for instance), the current implementation cannot represent this distinction.
