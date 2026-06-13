% ============================================================================
% CONSTRAINT STORY: total_war_possibility_space__deterrence_equilibrium_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_total_war_possibility_space__deterrence_equilibrium_reading, []).

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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_axiom_grounding/3,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:cs_created_at/2,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: total_war_possibility_space__deterrence_equilibrium_reading
 *   human_readable: Deterrence Equilibrium of Total War
 *   domain: international_relations/strategic_studies
 *
 * SUMMARY:
 *   This constraint describes the strategic reality where total war remains a
 *   theoretical possibility, but its actualization is deterred by the
 *   catastrophic costs of mutual vulnerability, primarily through nuclear
 *   weapons. This reading emphasizes the rational calculation of costs and
 *   benefits, leading to continuous investment in war-fighting capabilities
 *   as a deterrent signal. It is one reading of the broader
 *   'total_war_possibility_space' kernel, distinct from those emphasizing
 *   normative taboos or the inherent contraction of strategic space.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(total_war_possibility_space__deterrence_equilibrium_reading, 0.6).
domain_priors:suppression_score(total_war_possibility_space__deterrence_equilibrium_reading, 0.7).
domain_priors:theater_ratio(total_war_possibility_space__deterrence_equilibrium_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(total_war_possibility_space__deterrence_equilibrium_reading, extractiveness, 0.6).
narrative_ontology:constraint_metric(total_war_possibility_space__deterrence_equilibrium_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(total_war_possibility_space__deterrence_equilibrium_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(total_war_possibility_space__deterrence_equilibrium_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(total_war_possibility_space__deterrence_equilibrium_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(total_war_possibility_space__deterrence_equilibrium_reading, tangled_rope).
narrative_ontology:human_readable(total_war_possibility_space__deterrence_equilibrium_reading, "Deterrence Equilibrium of Total War").
narrative_ontology:topic_domain(total_war_possibility_space__deterrence_equilibrium_reading, "international_relations/strategic_studies").

domain_priors:requires_active_enforcement(total_war_possibility_space__deterrence_equilibrium_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(total_war_possibility_space__deterrence_equilibrium_reading, 'f8e56f00-ac93-4f9d-a7c9-e3f2cc564b7f').
narrative_ontology:cs_kernel_codification('f8e56f00-ac93-4f9d-a7c9-e3f2cc564b7f', implicit).
narrative_ontology:cs_authority_grounding('f8e56f00-ac93-4f9d-a7c9-e3f2cc564b7f', practice).
narrative_ontology:cs_interpretation_layer_present('f8e56f00-ac93-4f9d-a7c9-e3f2cc564b7f').
narrative_ontology:cs_reading_relation('f8e56f00-ac93-4f9d-a7c9-e3f2cc564b7f', total_war_possibility_space__space_contraction_reading, coexists_with).
narrative_ontology:cs_reading_relation('f8e56f00-ac93-4f9d-a7c9-e3f2cc564b7f', total_war_possibility_space__nuclear_taboo_reading, coexists_with).
narrative_ontology:cs_axiom('f8e56f00-ac93-4f9d-a7c9-e3f2cc564b7f', foundational, rational_actors_maximize_utility).
narrative_ontology:cs_axiom_status(rational_actors_maximize_utility, holdable).
narrative_ontology:cs_axiom_grounding('f8e56f00-ac93-4f9d-a7c9-e3f2cc564b7f', rational_actors_maximize_utility, empirically_contingent).
narrative_ontology:cs_axiom('f8e56f00-ac93-4f9d-a7c9-e3f2cc564b7f', foundational, mutual_vulnerability_ensures_deterrence).
narrative_ontology:cs_axiom_status(mutual_vulnerability_ensures_deterrence, holdable).
narrative_ontology:cs_axiom_grounding('f8e56f00-ac93-4f9d-a7c9-e3f2cc564b7f', mutual_vulnerability_ensures_deterrence, empirically_contingent).
narrative_ontology:cs_reference_frame('f8e56f00-ac93-4f9d-a7c9-e3f2cc564b7f', cold_war_strategic_stability).
narrative_ontology:cs_drift_state('f8e56f00-ac93-4f9d-a7c9-e3f2cc564b7f', contemporary_multi_polar_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('f8e56f00-ac93-4f9d-a7c9-e3f2cc564b7f', '').
narrative_ontology:cs_kernel_id(total_war_possibility_space__deterrence_equilibrium_reading, total_war_possibility_space).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(total_war_possibility_space__deterrence_equilibrium_reading, nuclear_powers).
narrative_ontology:constraint_beneficiary(total_war_possibility_space__deterrence_equilibrium_reading, defense_industries).
narrative_ontology:constraint_victim(total_war_possibility_space__deterrence_equilibrium_reading, global_population).
narrative_ontology:constraint_victim(total_war_possibility_space__deterrence_equilibrium_reading, non_nuclear_states).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(total_war_possibility_space__deterrence_equilibrium_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(total_war_possibility_space__deterrence_equilibrium_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(total_war_possibility_space__deterrence_equilibrium_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(total_war_possibility_space__deterrence_equilibrium_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(total_war_possibility_space__deterrence_equilibrium_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.6) reflects the immense resources diverted to maintaining deterrence and the existential risk borne by the global population. Suppression (0.7) is high due to the active enforcement of strategic stability through military readiness, intelligence gathering, and the suppression of non-state actors acquiring WMDs. Theater ratio (0.2) is relatively low, as the threat is largely real, though some aspects of strategic posturing can be performative. Accessibility collapse (0.4) is moderate; while total war is deterred, conventional conflicts and proxy wars remain accessible. Resistance (0.3) is present from disarmament movements and non-nuclear states, but it has not fundamentally altered the deterrence dynamic.
 *
 * PERSPECTIVAL GAP:
 *   Nuclear powers experience this as a necessary, if costly, mechanism for national security and global stability. Non-nuclear states and the global population experience it as a constant threat and a drain on resources, with little agency to alter the underlying structure. The engine's per-seat classification will reflect this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Nuclear powers (agenda_setters) are beneficiaries of the stability, but also bear the direct costs of maintaining deterrence (d ~0.4). Non-nuclear states and the global population are victims, bearing the risks and indirect costs without direct control (d ~0.8-0.9). Defense industries are clear beneficiaries, profiting from the continuous arms race (d ~0.1).
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is a Tangled Rope because it genuinely coordinates the avoidance of total war (a collective good) but does so through an extractive and suppressive mechanism (maintaining nuclear arsenals, constant threat). It requires active enforcement (military readiness, intelligence) to hold. The founding problem (preventing existential conflict) is still live, preventing mandatrophy, but the means of solving it are highly extractive.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    rationality_of_actors,
    'Is the deterrence equilibrium sustained by purely rational actors making cost-benefit calculations, or do non-rational factors (e.g., misperception, accidental escalation) play a significant, unmodeled role?',
    'Historical analysis of near-miss incidents, psychological studies of decision-making under extreme stress, and computational modeling of complex adaptive systems.',
    'If non-rational factors are dominant, the constraint''s stability is lower than assumed, and its classification might shift towards a more precarious Snare, as the coordination function is less reliable. If rationality holds, the Tangled Rope classification is reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(rationality_of_actors, empirical, 'The degree to which deterrence relies on perfect rationality.').

omega_variable(
    deterrence_vs_taboo_causality,
    'To what extent is the absence of total war due to the material deterrence equilibrium (this reading) versus a constructed normative taboo against nuclear use (nuclear_taboo_reading)?',
    'Comparative historical analysis of non-nuclear great power conflicts, counterfactual reasoning about nuclear proliferation scenarios, and analysis of state rhetoric and doctrine regarding nuclear use.',
    'If the nuclear taboo is the primary driver, this constraint''s extractiveness (costs of maintaining arsenals) would be re-evaluated as less essential to the coordination function, potentially shifting it towards a Snare (pure extraction) or a Piton (inertial maintenance of unnecessary capabilities). If deterrence is primary, the Tangled Rope holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(deterrence_vs_taboo_causality, conceptual, 'Causal weight of material deterrence vs. normative taboo.').

omega_variable(
    strategic_reachability_vs_thinkability,
    'Is total war merely deterred (this reading), or has it been removed from the strategically thinkable space altogether (space_contraction_reading)?',
    'Analysis of military planning documents, wargame scenarios, and strategic doctrine development. If total war scenarios are still actively planned for and theorized, it remains reachable.',
    'If total war is truly unthinkable, this constraint''s ''requires_active_enforcement'' and ''extractiveness'' metrics would be over-attributed to a non-existent threat, potentially reclassifying it as a Piton (inertial maintenance) or a Snare (extraction without a live coordination problem).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(strategic_reachability_vs_thinkability, conceptual, 'Whether total war is deterred or unthinkable.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(total_war_possibility_space__deterrence_equilibrium_reading, 1945, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tota_tr_t1945, total_war_possibility_space__deterrence_equilibrium_reading, theater_ratio, 1945, 0.1).
narrative_ontology:measurement(tota_tr_t1960, total_war_possibility_space__deterrence_equilibrium_reading, theater_ratio, 1960, 0.15).
narrative_ontology:measurement(tota_tr_t1980, total_war_possibility_space__deterrence_equilibrium_reading, theater_ratio, 1980, 0.2).
narrative_ontology:measurement(tota_tr_t2000, total_war_possibility_space__deterrence_equilibrium_reading, theater_ratio, 2000, 0.18).
narrative_ontology:measurement(tota_tr_t2024, total_war_possibility_space__deterrence_equilibrium_reading, theater_ratio, 2024, 0.2).

% Extraction over time
narrative_ontology:measurement(tota_be_t1945, total_war_possibility_space__deterrence_equilibrium_reading, base_extractiveness, 1945, 0.4).
narrative_ontology:measurement(tota_be_t1960, total_war_possibility_space__deterrence_equilibrium_reading, base_extractiveness, 1960, 0.5).
narrative_ontology:measurement(tota_be_t1980, total_war_possibility_space__deterrence_equilibrium_reading, base_extractiveness, 1980, 0.6).
narrative_ontology:measurement(tota_be_t2000, total_war_possibility_space__deterrence_equilibrium_reading, base_extractiveness, 2000, 0.55).
narrative_ontology:measurement(tota_be_t2024, total_war_possibility_space__deterrence_equilibrium_reading, base_extractiveness, 2024, 0.6).

% Suppression requirement over time
narrative_ontology:measurement(tota_su_t1945, total_war_possibility_space__deterrence_equilibrium_reading, suppression_requirement, 1945, 0.5).
narrative_ontology:measurement(tota_su_t1960, total_war_possibility_space__deterrence_equilibrium_reading, suppression_requirement, 1960, 0.6).
narrative_ontology:measurement(tota_su_t1980, total_war_possibility_space__deterrence_equilibrium_reading, suppression_requirement, 1980, 0.7).
narrative_ontology:measurement(tota_su_t2000, total_war_possibility_space__deterrence_equilibrium_reading, suppression_requirement, 2000, 0.65).
narrative_ontology:measurement(tota_su_t2024, total_war_possibility_space__deterrence_equilibrium_reading, suppression_requirement, 2024, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(total_war_possibility_space__deterrence_equilibrium_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(total_war_possibility_space__deterrence_equilibrium_reading, 0.1).
narrative_ontology:affects_constraint(total_war_possibility_space__deterrence_equilibrium_reading, nuclear_taboo_reading).
narrative_ontology:affects_constraint(total_war_possibility_space__deterrence_equilibrium_reading, space_contraction_reading).
narrative_ontology:affects_constraint(total_war_possibility_space__deterrence_equilibrium_reading, arms_control_treaties).
narrative_ontology:affects_constraint(total_war_possibility_space__deterrence_equilibrium_reading, non_proliferation_regime).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'total_war_possibility_space' kernel. This 'deterrence_equilibrium_reading' focuses on the rational calculation of mutual vulnerability as the primary deterrent. The 'nuclear_taboo_reading' emphasizes normative prohibition, and the 'space_contraction_reading' argues total war is no longer strategically thinkable. All three are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
