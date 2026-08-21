% ============================================================================
% CONSTRAINT STORY: imposition_mechanism_kernel__hybrid_legitimation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_imposition_mechanism_kernel__hybrid_legitimation_reading, []).

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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: imposition_mechanism_kernel__hybrid_legitimation_reading
 *   human_readable: Hybrid Legitimation of New Norms (Imperial Example)
 *   domain: historical_sociology/state_formation/cultural_authority
 *
 * SUMMARY:
 *   This constraint describes the process by which new social norms achieve
 *   legitimacy in a historical context, specifically through a hybrid
 *   mechanism involving both symbolic authority transfer (e.g., an emperor's
 *   endorsement) and institutional incentives. It is neither a purely
 *   bottom-up 'climb' nor a purely top-down 'override' by coercion. This
 *   story is one reading of the 'imposition_mechanism_kernel', focusing on
 *   the interplay of charisma and institutional power. The metrics reflect
 *   moderate extractiveness and suppression, as the process involves both
 *   voluntary adoption and subtle pressures.
 *
 * KEY AGENTS:
 *   - imperial_court: Agenda setter (institutional/arbitrage)
 *   - new_elite_adherents: Beneficiary (powerful/mobile)
 *   - traditional_elites: Payer (powerful/constrained)
 *   - unaligned_populace: Payer (powerless/trapped)
 *   - historical_analysts: Analytical observer (analytical/analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(imposition_mechanism_kernel__hybrid_legitimation_reading, 0.45).
domain_priors:suppression_score(imposition_mechanism_kernel__hybrid_legitimation_reading, 0.35).
domain_priors:theater_ratio(imposition_mechanism_kernel__hybrid_legitimation_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(imposition_mechanism_kernel__hybrid_legitimation_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(imposition_mechanism_kernel__hybrid_legitimation_reading, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(imposition_mechanism_kernel__hybrid_legitimation_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(imposition_mechanism_kernel__hybrid_legitimation_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(imposition_mechanism_kernel__hybrid_legitimation_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(imposition_mechanism_kernel__hybrid_legitimation_reading, tangled_rope).
narrative_ontology:human_readable(imposition_mechanism_kernel__hybrid_legitimation_reading, "Hybrid Legitimation of New Norms (Imperial Example)").
narrative_ontology:topic_domain(imposition_mechanism_kernel__hybrid_legitimation_reading, "historical_sociology/state_formation/cultural_authority").

domain_priors:requires_active_enforcement(imposition_mechanism_kernel__hybrid_legitimation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(imposition_mechanism_kernel__hybrid_legitimation_reading, 'be2ddbef-fab7-4c2b-aef5-ac526a956e98').
narrative_ontology:cs_kernel_codification('be2ddbef-fab7-4c2b-aef5-ac526a956e98', implicit).
narrative_ontology:cs_authority_grounding('be2ddbef-fab7-4c2b-aef5-ac526a956e98', lineage).
narrative_ontology:cs_interpretation_layer_present('be2ddbef-fab7-4c2b-aef5-ac526a956e98').
narrative_ontology:cs_reading_relation('be2ddbef-fab7-4c2b-aef5-ac526a956e98', imposition_mechanism_kernel__endogenous_climb_reading, coexists_with).
narrative_ontology:cs_reading_relation('be2ddbef-fab7-4c2b-aef5-ac526a956e98', imposition_mechanism_kernel__exogenous_override_reading, coexists_with).
narrative_ontology:cs_axiom('be2ddbef-fab7-4c2b-aef5-ac526a956e98', foundational, legitimacy_is_stratified_and_hybrid).
narrative_ontology:cs_axiom_status(legitimacy_is_stratified_and_hybrid, holdable).
narrative_ontology:cs_axiom_grounding('be2ddbef-fab7-4c2b-aef5-ac526a956e98', legitimacy_is_stratified_and_hybrid, empirically_contingent).
narrative_ontology:cs_axiom('be2ddbef-fab7-4c2b-aef5-ac526a956e98', foundational, symbolic_authority_is_a_causal_force).
narrative_ontology:cs_axiom_status(symbolic_authority_is_a_causal_force, holdable).
narrative_ontology:cs_axiom_grounding('be2ddbef-fab7-4c2b-aef5-ac526a956e98', symbolic_authority_is_a_causal_force, empirically_contingent).
narrative_ontology:cs_reference_frame('be2ddbef-fab7-4c2b-aef5-ac526a956e98', imperial_legitimation_synthesis).
narrative_ontology:cs_drift_state('be2ddbef-fab7-4c2b-aef5-ac526a956e98', contemporary_historical_analysis, gap(stable, minor, true)).
narrative_ontology:cs_created_at('be2ddbef-fab7-4c2b-aef5-ac526a956e98', '').
narrative_ontology:cs_kernel_id(imposition_mechanism_kernel__hybrid_legitimation_reading, imposition_mechanism_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(imposition_mechanism_kernel__hybrid_legitimation_reading, imperial_court).
narrative_ontology:constraint_beneficiary(imposition_mechanism_kernel__hybrid_legitimation_reading, new_elite_adherents).
narrative_ontology:constraint_victim(imposition_mechanism_kernel__hybrid_legitimation_reading, traditional_elites).
narrative_ontology:constraint_victim(imposition_mechanism_kernel__hybrid_legitimation_reading, unaligned_populace).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Initiates and promotes new norms, leveraging imperial charisma and institutional power. Benefits from the consolidation of authority and the creation of a loyal, aligned elite.
narrative_ontology:constraint_stakeholder(imposition_mechanism_kernel__hybrid_legitimation_reading, imperial_court, agenda_setter,
    institutional, generational, arbitrage, national).

% Adopts the new norms early, gaining favor, status, and institutional advantages from the imperial court. Their adoption provides a crucial legitimating example for wider society.
narrative_ontology:constraint_stakeholder(imposition_mechanism_kernel__hybrid_legitimation_reading, new_elite_adherents, beneficiary,
    powerful, biographical, mobile, regional).

% Are pressured to adopt new norms, often at the cost of their existing status or influence derived from older traditions. They face a choice between compliance for continued power or resistance with potential marginalization.
narrative_ontology:constraint_stakeholder(imposition_mechanism_kernel__hybrid_legitimation_reading, traditional_elites, payer,
    powerful, biographical, constrained, regional).

% Experiences the new norms as a top-down imposition, often through the example of local elites and institutional incentives. Their adoption is gradual and influenced by both symbolic authority and practical benefits/penalties.
narrative_ontology:constraint_stakeholder(imposition_mechanism_kernel__hybrid_legitimation_reading, unaligned_populace, payer,
    powerless, immediate, trapped, local).

% Examines the historical process of norm legitimation, seeking to understand the interplay of symbolic authority, institutional power, and social adoption. Their analysis informs the classification of the constraint.
narrative_ontology:constraint_stakeholder(imposition_mechanism_kernel__hybrid_legitimation_reading, historical_analysts, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates social behavior and cultural identity across a diverse populace by establishing a unified set of imperial-sanctioned norms, reducing internal friction and consolidating state power.
% TRANSFER_FUNCTION: Transfers cultural legitimacy and social capital from the imperial center to the new norms, and from traditional elites to those who align with the new order. It also transfers compliance costs to those who resist or are slow to adopt.
% ABSENT_VOICES: Those deeply invested in older, superseded traditions or local customs, who would argue for the organic evolution of norms rather than top-down imposition, are gradually marginalized or absorbed into the new system.
% DISAPPEARANCE_RATIONALE: If this hybrid legitimation mechanism vanished, the imperial project of cultural unification would fail. Society would revert to fragmented local norms or face prolonged cultural conflict, as the means of transferring symbolic authority and incentivizing adoption would be absent.
% FOUNDING_PROBLEM: The imperial state faced the challenge of unifying a diverse population under a common cultural and social framework to ensure stability and consolidate its rule, beyond mere military coercion.
% FOUNDING_PROBLEM_CORROBORATION: Historical records from imperial chroniclers and independent sociological analyses of state formation corroborate that the problem of cultural unification was central to the imperial project and remains a live concern for understanding historical state power.
narrative_ontology:disappearance_verdict(imposition_mechanism_kernel__hybrid_legitimation_reading, world_rearranges).
narrative_ontology:founding_problem_status(imposition_mechanism_kernel__hybrid_legitimation_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(imposition_mechanism_kernel__hybrid_legitimation_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(imposition_mechanism_kernel__hybrid_legitimation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(imposition_mechanism_kernel__hybrid_legitimation_reading, 0.45, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(imposition_mechanism_kernel__hybrid_legitimation_reading_tests).
:- end_tests(imposition_mechanism_kernel__hybrid_legitimation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.45) is moderate because while the new norms benefit the imperial court and aligned elites, they also provide a degree of social cohesion. Suppression (0.35) is also moderate, as direct coercion is not the primary mechanism, but institutional incentives and social pressures limit alternatives. The theater ratio (0.20) is low, indicating that the legitimation process is genuinely functional, not merely performative. The claimed type is 'tangled_rope' because it combines a coordination function (social cohesion) with asymmetric extraction (benefiting the imperial center and new elites at the expense of traditional structures and unaligned groups), requiring active enforcement through institutional means.
 *
 * PERSPECTIVAL GAP:
 *   The imperial court and new elite adherents would perceive this as a 'rope' or even a 'mountain' (natural evolution of society), emphasizing the coordination benefits and the 'naturalness' of imperial authority. Traditional elites and the unaligned populace, however, would experience it as a 'snare' or 'tangled_rope', feeling the pressure and costs of adaptation. The engine's classification as 'tangled_rope' reflects the analytical observer's view of the underlying structural asymmetry.
 *
 * DIRECTIONALITY LOGIC:
 *   The imperial_court and new_elite_adherents are beneficiaries, as the new norms consolidate their power and status. Traditional_elites and the unaligned_populace are payers, bearing the costs of adapting or resisting. The directionality for the imperial_court is near 0.0 (full beneficiary), for new_elite_adherents around 0.2-0.3 (strong beneficiary), for traditional_elites around 0.7-0.8 (strong target), and for the unaligned_populace near 0.9-1.0 (full target, due to limited exit options).
 *
 * MANDATROPHY ANALYSIS:
 *   This classification prevents mislabeling the constraint as pure extraction (snare) by acknowledging the genuine coordination function of cultural unification, while also preventing it from being seen as pure coordination (rope) by highlighting the asymmetric benefits and costs. The 'tangled_rope' classification captures the dynamic where the mandate for social cohesion is intertwined with the extraction of power and loyalty by the imperial center. If the symbolic authority were to wane without institutional incentives, it would drift towards a snare; if institutional incentives became purely coercive, it would also become a snare. The hybrid nature is key to its current classification.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    symbolic_vs_material_leverage,
    'What was the precise ratio of symbolic authority (charisma, example) to material incentives (rewards, penalties) in driving norm adoption?',
    'Detailed historical-sociological analysis of specific case studies, quantifying the impact of imperial decrees versus economic or social benefits/penalties for compliance.',
    'A higher proportion of symbolic authority would push the constraint closer to a ''rope'' (more voluntary coordination), while a higher proportion of material incentives would push it closer to a ''snare'' (more direct extraction/coercion).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(symbolic_vs_material_leverage, empirical, 'Ambiguity in the relative weight of symbolic vs. material drivers of norm adoption.').

omega_variable(
    legitimacy_source_ambiguity,
    'Is the legitimacy of the new norms primarily derived from the imperial center''s charisma, or from the perceived benefits of social cohesion they provide?',
    'Comparative historical analysis of similar norm-imposition efforts with varying degrees of imperial charisma or perceived social benefit.',
    'If primarily from charisma, the constraint''s persistence is more fragile and tied to the imperial figure; if from social cohesion, it is more robust and self-sustaining, potentially shifting towards a ''rope'' over time.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(legitimacy_source_ambiguity, conceptual, 'Uncertainty regarding the ultimate source of the new norms'' legitimacy.').

omega_variable(
    reading_distinction_clarity,
    'How distinct are the ''hybrid_legitimation_reading'' from the ''endogenous_climb_reading'' and ''exogenous_override_reading'' in practice, given the historical complexities?',
    'Further refinement of historical data to identify clear breakpoints or thresholds where one mechanism definitively dominates the others, or where their interaction is uniquely ''hybrid''.',
    'If the distinctions blur, the kernel itself may be ill-defined, or the readings may represent phases rather than distinct mechanisms, potentially leading to a re-evaluation of the entire kernel decomposition.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_distinction_clarity, conceptual, 'Conceptual clarity of the ''hybrid'' mechanism versus its ''pure'' sibling readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(imposition_mechanism_kernel__hybrid_legitimation_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(impo_tr_t0, imposition_mechanism_kernel__hybrid_legitimation_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(impo_tr_t20, imposition_mechanism_kernel__hybrid_legitimation_reading, theater_ratio, 20, 0.15).
narrative_ontology:measurement(impo_tr_t40, imposition_mechanism_kernel__hybrid_legitimation_reading, theater_ratio, 40, 0.2).
narrative_ontology:measurement(impo_tr_t60, imposition_mechanism_kernel__hybrid_legitimation_reading, theater_ratio, 60, 0.18).
narrative_ontology:measurement(impo_tr_t80, imposition_mechanism_kernel__hybrid_legitimation_reading, theater_ratio, 80, 0.15).
narrative_ontology:measurement(impo_tr_t100, imposition_mechanism_kernel__hybrid_legitimation_reading, theater_ratio, 100, 0.2).

% Extraction over time
narrative_ontology:measurement(impo_be_t0, imposition_mechanism_kernel__hybrid_legitimation_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(impo_be_t20, imposition_mechanism_kernel__hybrid_legitimation_reading, base_extractiveness, 20, 0.38).
narrative_ontology:measurement(impo_be_t40, imposition_mechanism_kernel__hybrid_legitimation_reading, base_extractiveness, 40, 0.45).
narrative_ontology:measurement(impo_be_t60, imposition_mechanism_kernel__hybrid_legitimation_reading, base_extractiveness, 60, 0.42).
narrative_ontology:measurement(impo_be_t80, imposition_mechanism_kernel__hybrid_legitimation_reading, base_extractiveness, 80, 0.4).
narrative_ontology:measurement(impo_be_t100, imposition_mechanism_kernel__hybrid_legitimation_reading, base_extractiveness, 100, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(impo_su_t0, imposition_mechanism_kernel__hybrid_legitimation_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(impo_su_t20, imposition_mechanism_kernel__hybrid_legitimation_reading, suppression_requirement, 20, 0.3).
narrative_ontology:measurement(impo_su_t40, imposition_mechanism_kernel__hybrid_legitimation_reading, suppression_requirement, 40, 0.35).
narrative_ontology:measurement(impo_su_t60, imposition_mechanism_kernel__hybrid_legitimation_reading, suppression_requirement, 60, 0.32).
narrative_ontology:measurement(impo_su_t80, imposition_mechanism_kernel__hybrid_legitimation_reading, suppression_requirement, 80, 0.3).
narrative_ontology:measurement(impo_su_t100, imposition_mechanism_kernel__hybrid_legitimation_reading, suppression_requirement, 100, 0.35).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(imposition_mechanism_kernel__hybrid_legitimation_reading, identity_coordination).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'imposition_mechanism_kernel', alongside 'endogenous_climb_reading' and 'exogenous_override_reading'. Each reading offers a distinct explanation for how new norms achieve legitimacy.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
