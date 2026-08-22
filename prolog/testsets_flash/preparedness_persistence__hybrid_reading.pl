% ============================================================================
% CONSTRAINT STORY: preparedness_persistence__hybrid_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_preparedness_persistence__hybrid_reading, []).

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
 *   constraint_id: preparedness_persistence__hybrid_reading
 *   human_readable: Stratified Disaster Preparedness (Hybrid Reading)
 *   domain: disaster_preparedness/institutional_memory
 *
 * SUMMARY:
 *   This constraint describes the stratified nature of disaster preparedness,
 *   where some components (e.g., engineering inspections) maintain genuine
 *   competence and deliver real benefits, while others (e.g., certain
 *   evacuation drills, bureaucratic reporting) have become ritualized
 *   performances. The system as a whole is claimed as a 'Rope' (coordination)
 *   by public officials, but its hybrid nature means it functions as a
 *   'Tangled Rope' from the perspective of the general public and ritualized
 *   subsystems, with localized extraction and significant theatricality. This
 *   is a 'hybrid_reading' of the 'preparedness_persistence' kernel.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(preparedness_persistence__hybrid_reading, 0.45).
domain_priors:suppression_score(preparedness_persistence__hybrid_reading, 0.3).
domain_priors:theater_ratio(preparedness_persistence__hybrid_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(preparedness_persistence__hybrid_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(preparedness_persistence__hybrid_reading, suppression_requirement, 0.3).
narrative_ontology:constraint_metric(preparedness_persistence__hybrid_reading, theater_ratio, 0.55).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(preparedness_persistence__hybrid_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(preparedness_persistence__hybrid_reading, resistance, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(preparedness_persistence__hybrid_reading, tangled_rope).
narrative_ontology:human_readable(preparedness_persistence__hybrid_reading, "Stratified Disaster Preparedness (Hybrid Reading)").
narrative_ontology:topic_domain(preparedness_persistence__hybrid_reading, "disaster_preparedness/institutional_memory").

domain_priors:requires_active_enforcement(preparedness_persistence__hybrid_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(preparedness_persistence__hybrid_reading, 'e9339bd0-e6ce-425d-837b-9506437238ef').
narrative_ontology:cs_kernel_codification('e9339bd0-e6ce-425d-837b-9506437238ef', distributed).
narrative_ontology:cs_authority_grounding('e9339bd0-e6ce-425d-837b-9506437238ef', practice).
narrative_ontology:cs_interpretation_layer_present('e9339bd0-e6ce-425d-837b-9506437238ef').
narrative_ontology:cs_reading_relation('e9339bd0-e6ce-425d-837b-9506437238ef', preparedness_persistence__competence_reading, coexists_with).
narrative_ontology:cs_reading_relation('e9339bd0-e6ce-425d-837b-9506437238ef', preparedness_persistence__husk_reading, coexists_with).
narrative_ontology:cs_axiom('e9339bd0-e6ce-425d-837b-9506437238ef', foundational, preparedness_is_heterogeneous).
narrative_ontology:cs_axiom_status(preparedness_is_heterogeneous, holdable).
narrative_ontology:cs_axiom_grounding('e9339bd0-e6ce-425d-837b-9506437238ef', preparedness_is_heterogeneous, empirically_contingent).
narrative_ontology:cs_axiom('e9339bd0-e6ce-425d-837b-9506437238ef', secondary, functional_and_performative_coexist).
narrative_ontology:cs_axiom_status(functional_and_performative_coexist, holdable).
narrative_ontology:cs_axiom_grounding('e9339bd0-e6ce-425d-837b-9506437238ef', functional_and_performative_coexist, empirically_contingent).
narrative_ontology:cs_reference_frame('e9339bd0-e6ce-425d-837b-9506437238ef', mixed_operational_and_symbolic_readiness).
narrative_ontology:cs_drift_state('e9339bd0-e6ce-425d-837b-9506437238ef', contemporary, gap(stable, minor, false)).
narrative_ontology:cs_created_at('e9339bd0-e6ce-425d-837b-9506437238ef', '').
narrative_ontology:cs_kernel_id(preparedness_persistence__hybrid_reading, preparedness_persistence).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(preparedness_persistence__hybrid_reading, competent_subsystems).
narrative_ontology:constraint_beneficiary(preparedness_persistence__hybrid_reading, public_officials).
narrative_ontology:constraint_victim(preparedness_persistence__hybrid_reading, ritualized_subsystems).
narrative_ontology:constraint_victim(preparedness_persistence__hybrid_reading, general_public).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These are the parts of the preparedness system (e.g., structural engineering inspections, critical infrastructure maintenance) that retain genuine operational competence and deliver real safety benefits. They benefit from funding and public trust, but are constrained by overall budget and political will.
narrative_ontology:constraint_stakeholder(preparedness_persistence__hybrid_reading, competent_subsystems, beneficiary,
    institutional, biographical, constrained, local).

% These are the parts of the preparedness system (e.g., many evacuation drills, certain bureaucratic reporting) that have become performative. They consume resources and personnel time without significantly enhancing actual readiness. They are identity-locked by professional norms and institutional inertia, making it hard to challenge their own efficacy.
narrative_ontology:constraint_stakeholder(preparedness_persistence__hybrid_reading, ritualized_subsystems, payer,
    moderate, biographical, identity_locked, local).

% Responsible for overseeing and funding preparedness efforts. They benefit from the appearance of readiness and public reassurance, but bear the political cost of actual disaster failures. They enforce both competent and ritualized components, often without distinguishing them.
narrative_ontology:constraint_stakeholder(preparedness_persistence__hybrid_reading, public_officials, agenda_setter,
    institutional, immediate, constrained, regional).

% Pays for preparedness through taxes and participates in drills, often with a false sense of security from ritualized components. They are the ultimate victims of preparedness failures, bearing the costs of inadequate real readiness.
narrative_ontology:constraint_stakeholder(preparedness_persistence__hybrid_reading, general_public, payer,
    powerless, immediate, trapped, local).

% Analyze the effectiveness of preparedness systems, often identifying the stratification between competent and ritualized components. They can propose reforms but lack direct power to implement them.
narrative_ontology:constraint_stakeholder(preparedness_persistence__hybrid_reading, emergency_management_experts, observer,
    analytical, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To organize diverse public and private actors to mitigate, prepare for, respond to, and recover from disasters, ensuring a baseline level of societal resilience.
% TRANSFER_FUNCTION: Transfers public funds and personnel time into a mix of genuinely effective infrastructure/training and performative activities, from taxpayers and frontline personnel to institutional budgets and public reassurance.
% ABSENT_VOICES: Citizens who have experienced actual disaster failures and can distinguish effective from ritualized preparedness would object to the continued funding of performative elements. Their voices are often drowned out by official narratives of 'successful' drills and bureaucratic compliance.
% DISAPPEARANCE_RATIONALE: If the entire preparedness system vanished overnight, the competent subsystems would cease to function, leading to immediate and catastrophic failures in critical infrastructure and response capabilities. The ritualized components would also disappear, but their absence would have less direct impact on actual safety, though public confidence might initially collapse.
% FOUNDING_PROBLEM: The need to protect populations and infrastructure from predictable and unpredictable natural and man-made disasters, minimizing loss of life and economic disruption.
% FOUNDING_PROBLEM_CORROBORATION: The ongoing occurrence of disasters and their impacts, attested by scientific bodies, historical records, and international disaster relief organizations, corroborates the persistent need for preparedness. However, the effectiveness of current systems in addressing this problem is contested by independent experts and disaster-affected communities.
narrative_ontology:disappearance_verdict(preparedness_persistence__hybrid_reading, world_rearranges).
narrative_ontology:founding_problem_status(preparedness_persistence__hybrid_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(preparedness_persistence__hybrid_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_gemini+stakeholder_backfill', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(preparedness_persistence__hybrid_reading, 'none', 1).
narrative_ontology:epsilon_provenance(preparedness_persistence__hybrid_reading, 0.45, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(preparedness_persistence__hybrid_reading_tests).
:- end_tests(preparedness_persistence__hybrid_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.45) is moderate, reflecting the mixed nature: real benefits from competent subsystems are offset by resource consumption by ritualized ones. Suppression (0.30) is low, as the system relies more on institutional inertia and public trust than active coercion. Theater ratio (0.55) is high, indicating that more than half of the activity is performative rather than functionally effective. The claimed type is 'tangled_rope' because it genuinely coordinates some functions while extracting from others through the same structure, requiring active enforcement to maintain the ritualized components.
 *
 * PERSPECTIVAL GAP:
 *   Public officials and competent subsystems perceive the constraint as a necessary 'Rope' for societal safety, emphasizing its coordination function. However, the general public and emergency management experts, particularly those observing the ritualized components, experience it as a 'Tangled Rope' or even a 'Piton' due to the significant theatricality and misallocation of resources. The engine's per-seat classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Competent subsystems and public officials are beneficiaries, as they either deliver real value or gain political capital from the appearance of preparedness. Ritualized subsystems are payers, as they consume resources without proportional output, and the general public are victims, bearing the costs of both effective and ineffective components. Emergency management experts are observers, analyzing the system without direct participation in its operation or extraction.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    distinguishing_competent_from_ritualized,
    'What objective, measurable criteria can reliably distinguish genuinely competent preparedness subsystems from ritualized ones?',
    'Post-disaster performance audits, independent operational readiness exercises, and comparative analysis of resource allocation vs. outcome in different preparedness domains.',
    'Clearer criteria would allow for targeted resource allocation, reducing extraction from ritualized components and increasing overall system efficiency. This could shift the constraint towards a ''Rope'' by reducing its ''Tangled Rope'' aspects.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(distinguishing_competent_from_ritualized, empirical, 'Ambiguity in identifying functional vs. performative preparedness components.').

omega_variable(
    mandate_drift_in_ritualized_subsystems,
    'Have the original mandates for ritualized preparedness activities (e.g., specific drills) atrophied, or were they always primarily performative?',
    'Historical analysis of founding documents and early implementation records for specific preparedness activities, compared with their current operational execution and stated goals.',
    'If mandates have atrophied, it strengthens the ''Piton'' aspect of these subsystems. If always performative, it suggests a ''Snare'' from inception, where the coordination story was always cover for resource capture or political signaling.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(mandate_drift_in_ritualized_subsystems, empirical, 'Uncertainty about the historical function and current mandate of ritualized preparedness components.').

omega_variable(
    framing_of_preparedness_efficacy,
    'Is the public''s perception of preparedness efficacy primarily shaped by actual readiness or by the visible performance of drills and official statements?',
    'Public opinion surveys correlated with independent assessments of disaster readiness, and analysis of media framing during and after disaster events.',
    'If perception is driven by performance, it reinforces the ''theater_ratio'' and the ''Tangled Rope'' classification, as the performative aspects serve to maintain public consent for the extractive elements. If driven by actual readiness, it would pressure officials to reduce theatricality.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(framing_of_preparedness_efficacy, conceptual, 'The role of public perception in sustaining performative preparedness.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(preparedness_persistence__hybrid_reading, 1990, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(prep_tr_t1990, preparedness_persistence__hybrid_reading, theater_ratio, 1990, 0.3).
narrative_ontology:measurement(prep_tr_t1998, preparedness_persistence__hybrid_reading, theater_ratio, 1998, 0.4).
narrative_ontology:measurement(prep_tr_t2006, preparedness_persistence__hybrid_reading, theater_ratio, 2006, 0.5).
narrative_ontology:measurement(prep_tr_t2014, preparedness_persistence__hybrid_reading, theater_ratio, 2014, 0.53).
narrative_ontology:measurement(prep_tr_t2024, preparedness_persistence__hybrid_reading, theater_ratio, 2024, 0.55).

% Extraction over time
narrative_ontology:measurement(prep_be_t1990, preparedness_persistence__hybrid_reading, base_extractiveness, 1990, 0.3).
narrative_ontology:measurement(prep_be_t1998, preparedness_persistence__hybrid_reading, base_extractiveness, 1998, 0.35).
narrative_ontology:measurement(prep_be_t2006, preparedness_persistence__hybrid_reading, base_extractiveness, 2006, 0.4).
narrative_ontology:measurement(prep_be_t2014, preparedness_persistence__hybrid_reading, base_extractiveness, 2014, 0.43).
narrative_ontology:measurement(prep_be_t2024, preparedness_persistence__hybrid_reading, base_extractiveness, 2024, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(prep_su_t1990, preparedness_persistence__hybrid_reading, suppression_requirement, 1990, 0.2).
narrative_ontology:measurement(prep_su_t1998, preparedness_persistence__hybrid_reading, suppression_requirement, 1998, 0.23).
narrative_ontology:measurement(prep_su_t2006, preparedness_persistence__hybrid_reading, suppression_requirement, 2006, 0.26).
narrative_ontology:measurement(prep_su_t2014, preparedness_persistence__hybrid_reading, suppression_requirement, 2014, 0.28).
narrative_ontology:measurement(prep_su_t2024, preparedness_persistence__hybrid_reading, suppression_requirement, 2024, 0.3).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(preparedness_persistence__hybrid_reading, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
