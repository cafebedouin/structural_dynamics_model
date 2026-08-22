% ============================================================================
% CONSTRAINT STORY: price_formation_kernel__naturalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_price_formation_kernel__naturalist_reading, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

% --- Constraint Identity Rule (DP-001: ε-Invariance) ---
% Each constraint story must have a single, stable base extractiveness (ε).
% If changing the observable used to evaluate this constraint would change ε,
% you are looking at two distinct constraints. Write separate .pl files for
% each, link them with affects_constraint/2, and document the relationship
% in both files' narrative context sections.
%
% The context tuple is CLOSED at arity 4: (P, T, E, S).
% Do not add measurement_basis, beneficiary/victim, or any other arguments.
% Linter Rule 23 enforces context/4.
%
% See: epsilon_invariance_principle.md

% --- Namespace Hooks (Required for loading) ---
:- multifile
    domain_priors:base_extractiveness/2,
    domain_priors:suppression_score/2,
    domain_priors:theater_ratio/2,
    domain_priors:requires_active_enforcement/1,
    narrative_ontology:has_sunset_clause/1,
    narrative_ontology:interval/3,
    narrative_ontology:measurement/5,
    narrative_ontology:constraint_metric/3,
    narrative_ontology:constraint_beneficiary/2,
    narrative_ontology:constraint_victim/2,
    narrative_ontology:constraint_claim/2,
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:suppression_profile/2,
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    domain_priors:emerges_naturally/1,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_axiom_grounding/3,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:cs_created_at/2,
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: price_formation_kernel__naturalist_reading
 *   human_readable: Price Formation as Natural Equilibrium (Naturalist Reading)
 *   domain: economic/political/housing
 *
 * SUMMARY:
 *   This constraint story instantiates the naturalist reading of the
 *   price_formation_kernel: the claim that housing prices emerge from a
 *   natural equilibrium process reflecting objective scarcity and subjective
 *   preferences, with no constructed or extractive element. The reading
 *   asserts that price is discovered through market processes, not
 *   constructed by policy, and that interventions (rent control, zoning
 *   reform, affordability mandates) create deadweight loss by distorting the
 *   natural price signal. This is a Mountain claim — zero extractiveness,
 *   zero suppression, perfect accessibility collapse, negligible resistance.
 *   The reading carries no beneficiaries or victims in its own structural
 *   logic; all distributive outcomes are treated as the natural consequence
 *   of scarcity and preference.
 *
 * KEY AGENTS:
 *   - price_formation_process: Natural law-like mechanism — discovers equilibrium, bears no cost, collects no rent
 *   - market_participants: Symmetric participants — buyers and sellers both face the discovered price, neither privileged
 *   - policy_makers: External interveners — treated as distorters of the natural process, not constitutive of it
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(price_formation_kernel__naturalist_reading, 0.0).
domain_priors:suppression_score(price_formation_kernel__naturalist_reading, 0.0).
domain_priors:theater_ratio(price_formation_kernel__naturalist_reading, 0.0).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(price_formation_kernel__naturalist_reading, extractiveness, 0.0).
narrative_ontology:constraint_metric(price_formation_kernel__naturalist_reading, suppression_requirement, 0.0).
narrative_ontology:constraint_metric(price_formation_kernel__naturalist_reading, theater_ratio, 0.0).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(price_formation_kernel__naturalist_reading, accessibility_collapse, 0.95).
narrative_ontology:constraint_metric(price_formation_kernel__naturalist_reading, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(price_formation_kernel__naturalist_reading, mountain).
narrative_ontology:human_readable(price_formation_kernel__naturalist_reading, "Price Formation as Natural Equilibrium (Naturalist Reading)").
narrative_ontology:topic_domain(price_formation_kernel__naturalist_reading, "economic/political/housing").

domain_priors:emerges_naturally(price_formation_kernel__naturalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(price_formation_kernel__naturalist_reading, 'ddb0912f-9270-4209-8560-8ef6f5c3caf2').
narrative_ontology:cs_kernel_codification('ddb0912f-9270-4209-8560-8ef6f5c3caf2', implicit).
narrative_ontology:cs_authority_grounding('ddb0912f-9270-4209-8560-8ef6f5c3caf2', practice).
narrative_ontology:cs_reading_relation('ddb0912f-9270-4209-8560-8ef6f5c3caf2', price_formation_kernel__institutional_reading, forecloses).
narrative_ontology:cs_reading_relation('ddb0912f-9270-4209-8560-8ef6f5c3caf2', price_formation_kernel__georgist_reading, forecloses).
narrative_ontology:cs_reading_relation('ddb0912f-9270-4209-8560-8ef6f5c3caf2', price_formation_kernel__financialization_reading, forecloses).
narrative_ontology:cs_axiom('ddb0912f-9270-4209-8560-8ef6f5c3caf2', foundational, price_is_discovered_not_constructed).
narrative_ontology:cs_axiom_status(price_is_discovered_not_constructed, holdable).
narrative_ontology:cs_axiom_grounding('ddb0912f-9270-4209-8560-8ef6f5c3caf2', price_is_discovered_not_constructed, deontological).
narrative_ontology:cs_axiom('ddb0912f-9270-4209-8560-8ef6f5c3caf2', secondary, intervention_creates_deadweight_loss).
narrative_ontology:cs_axiom_status(intervention_creates_deadweight_loss, holdable).
narrative_ontology:cs_axiom_grounding('ddb0912f-9270-4209-8560-8ef6f5c3caf2', intervention_creates_deadweight_loss, empirically_contingent).
narrative_ontology:cs_reference_frame('ddb0912f-9270-4209-8560-8ef6f5c3caf2', natural_market_equilibrium).
narrative_ontology:cs_drift_state('ddb0912f-9270-4209-8560-8ef6f5c3caf2', contemporary_housing_crisis, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('ddb0912f-9270-4209-8560-8ef6f5c3caf2', '').
narrative_ontology:cs_kernel_id(price_formation_kernel__naturalist_reading, price_formation_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_vindicates(price_formation_kernel__naturalist_reading, marginal_utility_price_theory).
narrative_ontology:constraint_vindicates(price_formation_kernel__naturalist_reading, walrasian_equilibrium_existence).
narrative_ontology:constraint_vindicates(price_formation_kernel__naturalist_reading, scarcity_as_objective_constraint).
narrative_ontology:constraint_vindicates(price_formation_kernel__naturalist_reading, preference_revelation_via_market).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(price_formation_kernel__naturalist_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(price_formation_kernel__naturalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(price_formation_kernel__naturalist_reading, 0.0, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(price_formation_kernel__naturalist_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(price_formation_kernel__naturalist_reading, ExtMetricName, E),
    domain_priors:suppression_score(price_formation_kernel__naturalist_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(price_formation_kernel__naturalist_reading),
    narrative_ontology:constraint_metric(price_formation_kernel__naturalist_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(price_formation_kernel__naturalist_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(price_formation_kernel__naturalist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   All metrics are authored at the structural minimum for a genuine mountain: extractiveness = 0 (no transfer extracted by the constraint itself), suppression = 0 (no coercion required to maintain the natural law), theater_ratio = 0 (no performative maintenance of a natural process), accessibility_collapse = 0.95 (alternatives genuinely collapse once the equilibrium logic is understood — you cannot choose a different market-clearing price without creating shortage or surplus), resistance = 0.05 (only resistance is from parties who mistake the natural constraint for a constructed one). The claimed_type is mountain, matching the metric profile. This reading's ε is zero by its own lights — the referent is the standing arrangement (market price formation) assessed as natural law.
 *
 * DIRECTIONALITY LOGIC:
 *   No beneficiary or victim groups declared because the naturalist reading's structural logic has none — price formation as natural equilibrium extracts from no one and subsidizes no one. All agents face the same discovered price symmetrically. The directionality derivation finds no structural asymmetry: d = 0.5 for all market participants (symmetric costs ≈ benefits from the price signal itself). Policy makers are not governed by the constraint; they are external would-be interveners.
 *
 * MANDATROPHY ANALYSIS:
 *   Not applicable — a mountain has no mandate to atrophy. The naturalist reading claims the constraint is a feature of reality, not an institutional arrangement with a founding problem.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_disambiguation,
    'Is the naturalist reading''s claim that price formation is a mountain of natural law, or is it a constructed constraint that benefits identifiable agents by presenting market outcomes as inevitable?',
    'Compare the naturalist reading''s structural profile (zero extraction, zero suppression, emerges_naturally) against the sibling readings'' profiles. If sibling readings document extraction and suppression at the same referent, the naturalist reading is a false summit candidate.',
    'If the naturalist reading is a false summit, FSM triggers reclassification to tangled_rope. If genuine mountain, all sibling readings address a different referent or are false readings of the same constraint.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_disambiguation, conceptual, 'Whether the naturalist reading''s mountain claim is structurally valid or a false summit masking extraction').

omega_variable(
    reading_relations_ambiguity,
    'Does the naturalist reading''s core premise (price as discovered natural equilibrium) logically foreclose the institutional reading''s premise (price as constructed by policy), or do they coexist as descriptions of different analytical layers?',
    'Test whether a single policy framework could coherently hold both: that prices reflect scarcity AND that zoning/lending standards construct the scarcity reflected. If yes, coexists_with; if the naturalist reading treats any policy construction as distortion of a pre-existing natural order, forecloses.',
    'Forecloses means the readings cannot occupy the same framework; coexists_with means different parties hold them simultaneously in live dispute; influences means the naturalist reading''s dominance shifts the institutional reading''s operating conditions without resolving it.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_relations_ambiguity, conceptual, 'Structural relationship between naturalist and institutional readings of the price_formation_kernel').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(price_formation_kernel__naturalist_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pric_tr_t0, price_formation_kernel__naturalist_reading, theater_ratio, 0, 0.0).
narrative_ontology:measurement(pric_tr_t10, price_formation_kernel__naturalist_reading, theater_ratio, 10, 0.0).
narrative_ontology:measurement(pric_tr_t20, price_formation_kernel__naturalist_reading, theater_ratio, 20, 0.0).
narrative_ontology:measurement_basis(pric_tr_t20, observed).
narrative_ontology:measurement(pric_tr_t30, price_formation_kernel__naturalist_reading, theater_ratio, 30, 0.0).
narrative_ontology:measurement_basis(pric_tr_t30, observed).

% Extraction over time
narrative_ontology:measurement(pric_be_t0, price_formation_kernel__naturalist_reading, base_extractiveness, 0, 0.0).
narrative_ontology:measurement(pric_be_t10, price_formation_kernel__naturalist_reading, base_extractiveness, 10, 0.0).
narrative_ontology:measurement(pric_be_t20, price_formation_kernel__naturalist_reading, base_extractiveness, 20, 0.0).
narrative_ontology:measurement(pric_be_t30, price_formation_kernel__naturalist_reading, base_extractiveness, 30, 0.0).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(price_formation_kernel__naturalist_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(price_formation_kernel__naturalist_reading, resource_allocation).
narrative_ontology:affects_constraint(price_formation_kernel__naturalist_reading, price_formation_kernel__institutional_reading).
narrative_ontology:affects_constraint(price_formation_kernel__naturalist_reading, price_formation_kernel__georgist_reading).
narrative_ontology:affects_constraint(price_formation_kernel__naturalist_reading, price_formation_kernel__financialization_reading).

% DUAL FORMULATION NOTE:
% Price formation kernel decomposed into four readings per ε-invariance principle: naturalist (this story, ε=0, mountain), institutional (ε>0, extraction from policy-constructed scarcity), georgist (ε>0, rent extraction via land monopoly), financialization (ε>0, extraction via credit-asset feedback). The naturalist reading is upstream in the family — its claim of natural equilibrium is often cited as evidence against the extractive readings. All siblings link back to this reading as the baseline they contest.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
