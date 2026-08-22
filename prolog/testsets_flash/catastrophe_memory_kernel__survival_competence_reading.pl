% ============================================================================
% CONSTRAINT STORY: catastrophe_memory_kernel__survival_competence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_catastrophe_memory_kernel__survival_competence_reading, []).

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
 *   constraint_id: catastrophe_memory_kernel__survival_competence_reading
 *   human_readable: Ritual as Catastrophe Survival Competence Transmission
 *   domain: religious_studies/collective_memory/ritual_practice
 *
 * SUMMARY:
 *   This constraint models ritual practice as a mechanism for encoding and
 *   transmitting adaptive capacity for persecution-survival within a
 *   community. It is one reading of the 'catastrophe_memory_kernel'. The
 *   ritual functions as a form of 'survival training' by rehearsing
 *   historical responses to threats, thereby strengthening community
 *   resilience. However, this comes at the cost of individual autonomy and
 *   requires active enforcement against assimilation pressures. The claimed
 *   type is 'tangled_rope' because it genuinely coordinates survival
 *   strategies but also extracts costs from individuals and actively
 *   suppresses alternatives (assimilation).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_memory_kernel__survival_competence_reading, 0.45).
domain_priors:suppression_score(catastrophe_memory_kernel__survival_competence_reading, 0.6).
domain_priors:theater_ratio(catastrophe_memory_kernel__survival_competence_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_memory_kernel__survival_competence_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(catastrophe_memory_kernel__survival_competence_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(catastrophe_memory_kernel__survival_competence_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_memory_kernel__survival_competence_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(catastrophe_memory_kernel__survival_competence_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_memory_kernel__survival_competence_reading, tangled_rope).
narrative_ontology:human_readable(catastrophe_memory_kernel__survival_competence_reading, "Ritual as Catastrophe Survival Competence Transmission").
narrative_ontology:topic_domain(catastrophe_memory_kernel__survival_competence_reading, "religious_studies/collective_memory/ritual_practice").

domain_priors:requires_active_enforcement(catastrophe_memory_kernel__survival_competence_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_memory_kernel__survival_competence_reading, 'c89f77af-c5ab-4fdf-b96e-c9dcff2e4ec7').
narrative_ontology:cs_kernel_codification('c89f77af-c5ab-4fdf-b96e-c9dcff2e4ec7', implicit).
narrative_ontology:cs_authority_grounding('c89f77af-c5ab-4fdf-b96e-c9dcff2e4ec7', practice).
narrative_ontology:cs_interpretation_layer_present('c89f77af-c5ab-4fdf-b96e-c9dcff2e4ec7').
narrative_ontology:cs_reading_relation('c89f77af-c5ab-4fdf-b96e-c9dcff2e4ec7', catastrophe_memory_kernel__symbol_continuity_reading, coexists_with).
narrative_ontology:cs_reading_relation('c89f77af-c5ab-4fdf-b96e-c9dcff2e4ec7', catastrophe_memory_kernel__trauma_encoding_reading, coexists_with).
narrative_ontology:cs_reading_relation('c89f77af-c5ab-4fdf-b96e-c9dcff2e4ec7', catastrophe_memory_kernel__boundary_maintenance_reading, coexists_with).
narrative_ontology:cs_axiom('c89f77af-c5ab-4fdf-b96e-c9dcff2e4ec7', foundational, ritual_as_adaptive_rehearsal).
narrative_ontology:cs_axiom_status(ritual_as_adaptive_rehearsal, holdable).
narrative_ontology:cs_axiom_grounding('c89f77af-c5ab-4fdf-b96e-c9dcff2e4ec7', ritual_as_adaptive_rehearsal, empirically_contingent).
narrative_ontology:cs_axiom('c89f77af-c5ab-4fdf-b96e-c9dcff2e4ec7', secondary, collective_survival_trumps_individual_autonomy).
narrative_ontology:cs_axiom_status(collective_survival_trumps_individual_autonomy, holdable).
narrative_ontology:cs_axiom_grounding('c89f77af-c5ab-4fdf-b96e-c9dcff2e4ec7', collective_survival_trumps_individual_autonomy, deontological).
narrative_ontology:cs_reference_frame('c89f77af-c5ab-4fdf-b96e-c9dcff2e4ec7', community_as_survival_unit).
narrative_ontology:cs_drift_state('c89f77af-c5ab-4fdf-b96e-c9dcff2e4ec7', contemporary_globalized_era, gap(practice_drift, minor, false)).
narrative_ontology:cs_created_at('c89f77af-c5ab-4fdf-b96e-c9dcff2e4ec7', '').
narrative_ontology:cs_kernel_id(catastrophe_memory_kernel__survival_competence_reading, catastrophe_memory_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_memory_kernel__survival_competence_reading, community_resilience_under_threat).
narrative_ontology:constraint_victim(catastrophe_memory_kernel__survival_competence_reading, assimilation_pressure).
narrative_ontology:constraint_victim(catastrophe_memory_kernel__survival_competence_reading, individual_autonomy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The collective capacity of the community to withstand and recover from persecution. This resilience is directly strengthened by the ritual's transmission of survival strategies and historical lessons. The community benefits from the continuity of these adaptive patterns.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__survival_competence_reading, community_resilience_under_threat, beneficiary,
    organized, generational, identity_locked, local).

% The external societal forces that seek to absorb or dilute the distinct identity of the community. These pressures are resisted by the ritual, which incurs a 'cost' in terms of the effort and resources required to maintain distinct practices and boundaries against the dominant culture.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__survival_competence_reading, assimilation_pressure, payer,
    institutional, generational, trapped, national).

% Individuals who actively participate in and transmit the ritual. They are the carriers of the adaptive knowledge and the enforcers of its continuity. Their identity is often deeply intertwined with the ritual, making exit difficult.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__survival_competence_reading, ritual_practitioners, agenda_setter,
    moderate, biographical, identity_locked, local).

% The freedom of individuals within the community to choose their own path, potentially diverging from traditional practices. The ritual's demands for adherence and participation can constrain individual choices, representing a cost to personal freedom in favor of collective survival.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__survival_competence_reading, individual_autonomy, payer,
    powerless, biographical, constrained, local).

% Academics who study the mechanisms of collective memory and ritual. They analyze how rituals function to transmit knowledge and identity, observing the dynamics of the constraint without direct participation or benefit/cost.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__survival_competence_reading, historical_memory_scholars, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the intergenerational transmission of practical knowledge and behavioral patterns necessary for a community's survival in the face of recurring persecution or existential threat, ensuring collective readiness and response.
% TRANSFER_FUNCTION: Transfers adaptive strategies, historical lessons, and a shared sense of vigilance from past generations to present and future members, at the cost of individual conformity and resistance to assimilation.
% ABSENT_VOICES: Those who have assimilated into dominant cultures, or individuals within the community who prioritize individual freedom over collective survival, would argue against the strictures of the ritual, viewing them as burdensome rather than adaptive.
% DISAPPEARANCE_RATIONALE: If the ritual vanished, the community would lose a primary mechanism for rehearsing and transmitting survival competence. Over generations, this would likely lead to a significant erosion of collective adaptive capacity, increased vulnerability to external pressures, and potentially the dissolution of the distinct community identity, forcing a reorganization of its social and cultural structures.
% FOUNDING_PROBLEM: The recurring experience of persecution and existential threats, necessitating a robust and reliable mechanism for transmitting survival knowledge and maintaining collective identity across generations.
% FOUNDING_PROBLEM_CORROBORATION: Community elders and historians attest to the ongoing relevance of historical threats and the need for continued vigilance. Sociologists studying minority groups corroborate the persistent pressures of assimilation and the adaptive role of cultural practices in maintaining group cohesion and survival.
narrative_ontology:disappearance_verdict(catastrophe_memory_kernel__survival_competence_reading, world_rearranges).
narrative_ontology:founding_problem_status(catastrophe_memory_kernel__survival_competence_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_memory_kernel__survival_competence_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_gemini+stakeholder_backfill', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(catastrophe_memory_kernel__survival_competence_reading, 'none', 1).
narrative_ontology:epsilon_provenance(catastrophe_memory_kernel__survival_competence_reading, 0.45, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(catastrophe_memory_kernel__survival_competence_reading_tests).
:- end_tests(catastrophe_memory_kernel__survival_competence_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.45) reflects the moderate cost imposed on individuals (conformity, limited autonomy) and the community (effort to maintain distinctiveness) for the benefit of collective survival. Suppression (0.6) is significant due to the active resistance against assimilation and the internal pressure to adhere to ritual practices for the sake of group cohesion. The theater ratio (0.2) is low, indicating that the ritual's function is largely genuine and not merely performative, though some elements might become ritualized beyond immediate practical utility over time. The 'identity_locked' exit option for community resilience and practitioners highlights the deep integration of the ritual with group and individual identity, making disengagement a profound loss of self or community.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of community resilience, the ritual is a vital 'rope' for survival. From the perspective of individual autonomy, it can feel like a 'snare' due to the demands for conformity and the suppression of alternative life paths. The engine's per-seat classification will capture this divergence, showing a 'tangled_rope' overall due to the dual function and asymmetric costs.
 *
 * DIRECTIONALITY LOGIC:
 *   Community resilience is the primary beneficiary, as the ritual directly enhances its capacity to survive. Assimilation pressure and individual autonomy are victims, bearing the costs of boundary maintenance and constrained choice, respectively. Ritual practitioners act as agenda-setters, actively maintaining and enforcing the practices. Scholars observe the dynamics analytically.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    survival_vs_identity_priority,
    'Does the ritual primarily transmit practical survival competence, or is its primary function the maintenance of symbolic continuity and collective identity?',
    'Comparative analysis of ritual content and historical outcomes: if changes in ritual directly correlate with changes in survival rates, it supports competence; if changes correlate with identity cohesion, it supports continuity.',
    'If primarily identity, the extractiveness might be re-evaluated as a cost of belonging rather than a cost of training, potentially shifting the classification towards a ''rope'' for identity coordination with lower extraction from individuals.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(survival_vs_identity_priority, conceptual, 'Ambiguity in the primary function of the ritual: survival training vs. identity maintenance.').

omega_variable(
    internalized_vs_structural_suppression,
    'Is the measured suppression primarily structural (external persecution, assimilation pressure) or internalized (cognitive patterns, identity fusion within the community)?',
    'Post-exit suppression trajectory: if individuals who leave the community continue to experience internal resistance to assimilation, it indicates internalized suppression. If resistance drops immediately upon exit, it''s primarily structural.',
    'If internalized, the effective suppression is higher than the structural measure suggests, as individuals carry the suppression with them. This would amplify the ''snare'' aspect for individual autonomy.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(internalized_vs_structural_suppression, empirical, 'Structural vs. internalized suppression mechanism in ritual adherence.').

omega_variable(
    mandate_obsolescence,
    'Is the ''persecution-survival'' mandate still genuinely live, or has the threat diminished such that the ritual''s costs now outweigh its adaptive benefits?',
    'Longitudinal sociological study of community vulnerability and external threats over several generations. Corroboration from independent security analysts on the nature of contemporary threats.',
    'If the mandate is found to be ''dead'', the constraint would shift towards a ''piton'' or ''snare'', as the coordination function has atrophied, leaving only the extraction and inertia.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(mandate_obsolescence, empirical, 'Whether the founding problem of persecution-survival remains relevant.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_memory_kernel__survival_competence_reading, 100, 200).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cata_tr_t100, catastrophe_memory_kernel__survival_competence_reading, theater_ratio, 100, 0.2).
narrative_ontology:measurement(cata_tr_t120, catastrophe_memory_kernel__survival_competence_reading, theater_ratio, 120, 0.18).
narrative_ontology:measurement(cata_tr_t140, catastrophe_memory_kernel__survival_competence_reading, theater_ratio, 140, 0.2).
narrative_ontology:measurement(cata_tr_t160, catastrophe_memory_kernel__survival_competence_reading, theater_ratio, 160, 0.22).
narrative_ontology:measurement(cata_tr_t180, catastrophe_memory_kernel__survival_competence_reading, theater_ratio, 180, 0.21).
narrative_ontology:measurement(cata_tr_t200, catastrophe_memory_kernel__survival_competence_reading, theater_ratio, 200, 0.2).

% Extraction over time
narrative_ontology:measurement(cata_be_t100, catastrophe_memory_kernel__survival_competence_reading, base_extractiveness, 100, 0.4).
narrative_ontology:measurement(cata_be_t120, catastrophe_memory_kernel__survival_competence_reading, base_extractiveness, 120, 0.42).
narrative_ontology:measurement(cata_be_t140, catastrophe_memory_kernel__survival_competence_reading, base_extractiveness, 140, 0.45).
narrative_ontology:measurement(cata_be_t160, catastrophe_memory_kernel__survival_competence_reading, base_extractiveness, 160, 0.43).
narrative_ontology:measurement(cata_be_t180, catastrophe_memory_kernel__survival_competence_reading, base_extractiveness, 180, 0.46).
narrative_ontology:measurement(cata_be_t200, catastrophe_memory_kernel__survival_competence_reading, base_extractiveness, 200, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(cata_su_t100, catastrophe_memory_kernel__survival_competence_reading, suppression_requirement, 100, 0.55).
narrative_ontology:measurement(cata_su_t120, catastrophe_memory_kernel__survival_competence_reading, suppression_requirement, 120, 0.58).
narrative_ontology:measurement(cata_su_t140, catastrophe_memory_kernel__survival_competence_reading, suppression_requirement, 140, 0.6).
narrative_ontology:measurement(cata_su_t160, catastrophe_memory_kernel__survival_competence_reading, suppression_requirement, 160, 0.59).
narrative_ontology:measurement(cata_su_t180, catastrophe_memory_kernel__survival_competence_reading, suppression_requirement, 180, 0.61).
narrative_ontology:measurement(cata_su_t200, catastrophe_memory_kernel__survival_competence_reading, suppression_requirement, 200, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_memory_kernel__survival_competence_reading, identity_coordination).
narrative_ontology:affects_constraint(catastrophe_memory_kernel__survival_competence_reading, catastrophe_memory_kernel__symbol_continuity_reading).
narrative_ontology:affects_constraint(catastrophe_memory_kernel__survival_competence_reading, catastrophe_memory_kernel__trauma_encoding_reading).
narrative_ontology:affects_constraint(catastrophe_memory_kernel__survival_competence_reading, catastrophe_memory_kernel__boundary_maintenance_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of several readings of the 'catastrophe_memory_kernel', each focusing on a distinct structural function of collective memory rituals. This reading emphasizes the transmission of adaptive survival competence.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
