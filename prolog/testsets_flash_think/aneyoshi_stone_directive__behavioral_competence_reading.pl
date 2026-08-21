% ============================================================================
% CONSTRAINT STORY: aneyoshi_stone_directive__behavioral_competence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_aneyoshi_stone_directive__behavioral_competence_reading, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: aneyoshi_stone_directive__behavioral_competence_reading
 *   human_readable: Aneyoshi Stone Directive: Behavioral Competence Reading
 *   domain: disaster_anthropology/institutional_memory/land_use_governance
 *
 * SUMMARY:
 *   This constraint story instantiates the 'behavioral competence' reading of
 *   the Aneyoshi Stone Directive. In this reading, the stone directive is
 *   understood as a binding land-use constraint that accurately reflects a
 *   physical geography limit (tsunami inundation height). Its persistence
 *   across 78 years without explicit re-validation is seen as evidence of its
 *   inherent truth and the community's competent adherence to a natural law,
 *   rather than institutional inertia or performative maintenance. The low
 *   extractiveness and suppression reflect its nature as a geophysical
 *   constraint, not a human-imposed rule.
 *
 * KEY AGENTS:
 *   - local_residents: Primary compliers, direct beneficiaries of safety from the natural limit.
 *   - local_government: Land-use planners, responsible for interpreting and integrating the directive into local policy.
 *   - disaster_scientists: Analytical observers, validate the underlying geophysical risk and the directive's efficacy.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(aneyoshi_stone_directive__behavioral_competence_reading, 0.05).
domain_priors:suppression_score(aneyoshi_stone_directive__behavioral_competence_reading, 0.1).
domain_priors:theater_ratio(aneyoshi_stone_directive__behavioral_competence_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(aneyoshi_stone_directive__behavioral_competence_reading, extractiveness, 0.05).
narrative_ontology:constraint_metric(aneyoshi_stone_directive__behavioral_competence_reading, suppression_requirement, 0.1).
narrative_ontology:constraint_metric(aneyoshi_stone_directive__behavioral_competence_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(aneyoshi_stone_directive__behavioral_competence_reading, accessibility_collapse, 0.9).
narrative_ontology:constraint_metric(aneyoshi_stone_directive__behavioral_competence_reading, resistance, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(aneyoshi_stone_directive__behavioral_competence_reading, mountain).
narrative_ontology:human_readable(aneyoshi_stone_directive__behavioral_competence_reading, "Aneyoshi Stone Directive: Behavioral Competence Reading").
narrative_ontology:topic_domain(aneyoshi_stone_directive__behavioral_competence_reading, "disaster_anthropology/institutional_memory/land_use_governance").

domain_priors:emerges_naturally(aneyoshi_stone_directive__behavioral_competence_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(aneyoshi_stone_directive__behavioral_competence_reading, 'cb23f922-660c-4e6d-a484-0549c9df08d8').
narrative_ontology:cs_kernel_codification('cb23f922-660c-4e6d-a484-0549c9df08d8', fixed_text).
narrative_ontology:cs_authority_grounding('cb23f922-660c-4e6d-a484-0549c9df08d8', practice).
narrative_ontology:cs_reading_relation('cb23f922-660c-4e6d-a484-0549c9df08d8', aneyoshi_stone_directive__commemorative_husk_reading, forecloses).
narrative_ontology:cs_axiom('cb23f922-660c-4e6d-a484-0549c9df08d8', foundational, tsunami_risk_is_permanent_geophysical_constraint).
narrative_ontology:cs_axiom_status(tsunami_risk_is_permanent_geophysical_constraint, holdable).
narrative_ontology:cs_axiom_grounding('cb23f922-660c-4e6d-a484-0549c9df08d8', tsunami_risk_is_permanent_geophysical_constraint, empirically_contingent).
narrative_ontology:cs_reference_frame('cb23f922-660c-4e6d-a484-0549c9df08d8', geophysical_imperative_framework).
narrative_ontology:cs_drift_state('cb23f922-660c-4e6d-a484-0549c9df08d8', pre_2011_tohoku_tsunami, gap(stable, minor, true)).
narrative_ontology:cs_created_at('cb23f922-660c-4e6d-a484-0549c9df08d8', '').
narrative_ontology:cs_kernel_id(aneyoshi_stone_directive__behavioral_competence_reading, aneyoshi_stone_directive).

% --- Structural relationships ---
narrative_ontology:constraint_vindicates(aneyoshi_stone_directive__behavioral_competence_reading, geophysical_imperative_doctrine).
narrative_ontology:constraint_vindicates(aneyoshi_stone_directive__behavioral_competence_reading, intergenerational_knowledge_transfer_efficacy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Not applicable; this is a natural limit, not a human coordination mechanism.
% TRANSFER_FUNCTION: None; the constraint itself does not transfer anything. Compliance with it transfers safety from potential disaster to current and future residents.
% ABSENT_VOICES: None. The constraint represents a geophysical reality; there are no 'voices' to be excluded from a natural law.
% DISAPPEARANCE_RATIONALE: If the Aneyoshi stone directive (as a marker of a natural limit) vanished overnight, the underlying geophysical reality of tsunami risk would persist unchanged. The physical geography would still dictate the safe elevation for settlement, and future tsunamis would still occur. Only the human memory and behavioral guidance would be lost, not the constraint itself.
% FOUNDING_PROBLEM: Preventing catastrophic loss of life and property from recurrent tsunamis by marking the safe elevation for human settlement.
% FOUNDING_PROBLEM_CORROBORATION: Geological evidence of past tsunamis, historical records of disaster, and contemporary disaster science all corroborate that the founding problem (tsunami risk) remains live. The 2011 Tohoku tsunami provided a stark empirical validation of the directive's efficacy where it was heeded.
narrative_ontology:disappearance_verdict(aneyoshi_stone_directive__behavioral_competence_reading, world_unchanged).
narrative_ontology:founding_problem_status(aneyoshi_stone_directive__behavioral_competence_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(aneyoshi_stone_directive__behavioral_competence_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(aneyoshi_stone_directive__behavioral_competence_reading, 'none', 1).
narrative_ontology:epsilon_provenance(aneyoshi_stone_directive__behavioral_competence_reading, 0.05, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(aneyoshi_stone_directive__behavioral_competence_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(aneyoshi_stone_directive__behavioral_competence_reading, ExtMetricName, E),
    domain_priors:suppression_score(aneyoshi_stone_directive__behavioral_competence_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(aneyoshi_stone_directive__behavioral_competence_reading),
    narrative_ontology:constraint_metric(aneyoshi_stone_directive__behavioral_competence_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(aneyoshi_stone_directive__behavioral_competence_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(aneyoshi_stone_directive__behavioral_competence_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The constraint is classified as a Mountain because, in this reading, it represents an unchangeable physical limit (tsunami height). Its 'directive' function is merely to communicate this natural law. Extraction, suppression, and theater ratio are all very low because the constraint is not a human construct designed to extract rents or coerce behavior, but a reflection of reality. Compliance yields safety, but no party 'collects' from its operation. Accessibility collapse is high because the physical reality of tsunami risk severely limits safe settlement options. Resistance is low because the constraint is understood as a natural fact.
 *
 * PERSPECTIVAL GAP:
 *   Not applicable for a pure Mountain. All seats, if they accurately perceive the geophysical reality, would experience the constraint similarly as an unchangeable limit.
 *
 * DIRECTIONALITY LOGIC:
 *   As a genuine Mountain, there are no beneficiaries or victims in the sense of collecting rents or being extracted from by the constraint itself. Local residents who comply benefit from safety, but this is a consequence of aligning with a natural law, not a transfer from the constraint's operation. Therefore, no specific beneficiaries or victims are declared.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    directive_vs_husk_ambiguity,
    'Is the Aneyoshi stone directive a binding land-use constraint reflecting geophysical reality, or a commemorative husk that lost its behavioral force during inter-catastrophe periods?',
    'Empirical observation of community behavior during a major tsunami event (e.g., the 2011 Tohoku tsunami) and post-event analysis of adherence to the directive''s elevation guidance.',
    'If the ''behavioral competence'' reading is validated (as it was in 2011), the constraint remains a Mountain. If the ''commemorative husk'' reading were validated (i.e., widespread non-adherence leading to disaster), the constraint would be reclassified as a Piton (a former directive whose function atrophied).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(directive_vs_husk_ambiguity, empirical, 'Distinguishing between a living directive and a historical artifact.').

omega_variable(
    human_adherence_vs_natural_law,
    'To what extent does the constraint''s persistence depend on active human institutional memory and adherence, versus the self-evident nature of the underlying geophysical imperative?',
    'Comparative study of similar coastal communities with and without such directives, observing long-term settlement patterns and disaster outcomes. Analysis of intergenerational knowledge transfer mechanisms.',
    'If human adherence is found to be the dominant factor, the constraint might have elements of a Rope (coordination around a shared risk) or even a Piton (if adherence becomes theatrical). If the geophysical imperative is truly self-evident, it reinforces the Mountain classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(human_adherence_vs_natural_law, conceptual, 'The balance between human agency and natural law in the constraint''s persistence.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(aneyoshi_stone_directive__behavioral_competence_reading, 1933, 2011).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(aney_tr_t1933, aneyoshi_stone_directive__behavioral_competence_reading, theater_ratio, 1933, 0.05).
narrative_ontology:measurement(aney_tr_t1950, aneyoshi_stone_directive__behavioral_competence_reading, theater_ratio, 1950, 0.05).
narrative_ontology:measurement(aney_tr_t1970, aneyoshi_stone_directive__behavioral_competence_reading, theater_ratio, 1970, 0.05).
narrative_ontology:measurement(aney_tr_t1990, aneyoshi_stone_directive__behavioral_competence_reading, theater_ratio, 1990, 0.05).
narrative_ontology:measurement(aney_tr_t2011, aneyoshi_stone_directive__behavioral_competence_reading, theater_ratio, 2011, 0.05).

% Extraction over time
narrative_ontology:measurement(aney_be_t1933, aneyoshi_stone_directive__behavioral_competence_reading, base_extractiveness, 1933, 0.05).
narrative_ontology:measurement(aney_be_t1950, aneyoshi_stone_directive__behavioral_competence_reading, base_extractiveness, 1950, 0.05).
narrative_ontology:measurement(aney_be_t1970, aneyoshi_stone_directive__behavioral_competence_reading, base_extractiveness, 1970, 0.05).
narrative_ontology:measurement(aney_be_t1990, aneyoshi_stone_directive__behavioral_competence_reading, base_extractiveness, 1990, 0.05).
narrative_ontology:measurement(aney_be_t2011, aneyoshi_stone_directive__behavioral_competence_reading, base_extractiveness, 2011, 0.05).

% Suppression requirement over time
narrative_ontology:measurement(aney_su_t1933, aneyoshi_stone_directive__behavioral_competence_reading, suppression_requirement, 1933, 0.1).
narrative_ontology:measurement(aney_su_t1950, aneyoshi_stone_directive__behavioral_competence_reading, suppression_requirement, 1950, 0.1).
narrative_ontology:measurement(aney_su_t1970, aneyoshi_stone_directive__behavioral_competence_reading, suppression_requirement, 1970, 0.1).
narrative_ontology:measurement(aney_su_t1990, aneyoshi_stone_directive__behavioral_competence_reading, suppression_requirement, 1990, 0.1).
narrative_ontology:measurement(aney_su_t2011, aneyoshi_stone_directive__behavioral_competence_reading, suppression_requirement, 2011, 0.1).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
