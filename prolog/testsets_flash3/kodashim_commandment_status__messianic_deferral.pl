% ============================================================================
% CONSTRAINT STORY: kodashim_commandment_status__messianic_deferral
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_kodashim_commandment_status__messianic_deferral, []).

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
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: kodashim_commandment_status__messianic_deferral
 *   human_readable: Kodashim Commandment Status: Messianic Deferral Reading
 *   domain: religious_studies/halakhic_theory/commitment_system_analysis
 *
 * SUMMARY:
 *   This constraint represents the 'messianic deferral' reading of the
 *   Kodashim (sacrificial) commandments in Jewish law. In this reading, the
 *   commandments are not obsolete despite the absence of the Temple, but are
 *   rather suspended, awaiting future messianic restoration. The study of
 *   these laws is considered a vital act of readiness and spiritual
 *   engagement, maintaining their halakhic status and ensuring the community
 *   is prepared for their eventual re-implementation. This reading imposes an
 *   opportunity cost on the community by directing resources to theoretical
 *   study rather than immediate practical application.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(kodashim_commandment_status__messianic_deferral, 0.45).
domain_priors:suppression_score(kodashim_commandment_status__messianic_deferral, 0.6).
domain_priors:theater_ratio(kodashim_commandment_status__messianic_deferral, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(kodashim_commandment_status__messianic_deferral, extractiveness, 0.45).
narrative_ontology:constraint_metric(kodashim_commandment_status__messianic_deferral, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(kodashim_commandment_status__messianic_deferral, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(kodashim_commandment_status__messianic_deferral, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(kodashim_commandment_status__messianic_deferral, resistance, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(kodashim_commandment_status__messianic_deferral, tangled_rope).
narrative_ontology:human_readable(kodashim_commandment_status__messianic_deferral, "Kodashim Commandment Status: Messianic Deferral Reading").
narrative_ontology:topic_domain(kodashim_commandment_status__messianic_deferral, "religious_studies/halakhic_theory/commitment_system_analysis").

domain_priors:requires_active_enforcement(kodashim_commandment_status__messianic_deferral).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(kodashim_commandment_status__messianic_deferral, 'aa63736e-376f-417c-aca4-7d58781f6d45').
narrative_ontology:cs_kernel_codification('aa63736e-376f-417c-aca4-7d58781f6d45', fixed_text).
narrative_ontology:cs_authority_grounding('aa63736e-376f-417c-aca4-7d58781f6d45', lineage).
narrative_ontology:cs_interpretation_layer_present('aa63736e-376f-417c-aca4-7d58781f6d45').
narrative_ontology:cs_reading_relation('aa63736e-376f-417c-aca4-7d58781f6d45', kodashim_commandment_status__performance_only, coexists_with).
narrative_ontology:cs_reading_relation('aa63736e-376f-417c-aca4-7d58781f6d45', kodashim_commandment_status__study_as_performance, coexists_with).
narrative_ontology:cs_axiom('aa63736e-376f-417c-aca4-7d58781f6d45', foundational, commandment_temporally_suspended_not_obsolete).
narrative_ontology:cs_axiom_status(commandment_temporally_suspended_not_obsolete, holdable).
narrative_ontology:cs_axiom_grounding('aa63736e-376f-417c-aca4-7d58781f6d45', commandment_temporally_suspended_not_obsolete, theological).
narrative_ontology:cs_axiom('aa63736e-376f-417c-aca4-7d58781f6d45', foundational, study_maintains_readiness_for_future_restoration).
narrative_ontology:cs_axiom_status(study_maintains_readiness_for_future_restoration, holdable).
narrative_ontology:cs_axiom_grounding('aa63736e-376f-417c-aca4-7d58781f6d45', study_maintains_readiness_for_future_restoration, theological).
narrative_ontology:cs_reference_frame('aa63736e-376f-417c-aca4-7d58781f6d45', post_temple_halakhic_continuity).
narrative_ontology:cs_drift_state('aa63736e-376f-417c-aca4-7d58781f6d45', contemporary_secular_era, gap(stable, minor, true)).
narrative_ontology:cs_created_at('aa63736e-376f-417c-aca4-7d58781f6d45', '').
narrative_ontology:cs_kernel_id(kodashim_commandment_status__messianic_deferral, kodashim_commandment_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(kodashim_commandment_status__messianic_deferral, rabbinic_scholars).
narrative_ontology:constraint_beneficiary(kodashim_commandment_status__messianic_deferral, messianic_movements).
narrative_ontology:constraint_victim(kodashim_commandment_status__messianic_deferral, lay_adherents).
narrative_ontology:constraint_victim(kodashim_commandment_status__messianic_deferral, present_generation_needs).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interpret and transmit the halakhic tradition, emphasizing the ongoing relevance of Kodashim study as preparation for a future Temple. Their authority and intellectual careers are bound to this interpretive framework.
narrative_ontology:constraint_stakeholder(kodashim_commandment_status__messianic_deferral, rabbinic_scholars, agenda_setter,
    institutional, generational, identity_locked, global).

% Benefit from the deferral reading as it provides a framework for their eschatological aspirations, maintaining the expectation of a restored Temple and its sacrificial service. Their identity is fused with this future-oriented commitment.
narrative_ontology:constraint_stakeholder(kodashim_commandment_status__messianic_deferral, messianic_movements, beneficiary,
    organized, civilizational, identity_locked, global).

% Bear the opportunity cost of resources (time, intellectual effort) directed towards studying laws that cannot be currently fulfilled, potentially diverting from more immediately applicable religious or communal needs. Their commitment is to the tradition, but the deferral imposes a cost.
narrative_ontology:constraint_stakeholder(kodashim_commandment_status__messianic_deferral, lay_adherents, payer,
    moderate, biographical, constrained, local).

% Represents the collective needs and priorities of the current community that might be better served by reallocating resources from theoretical study of suspended laws to practical, present-day concerns. This 'agent' is a conceptual placeholder for the opportunity cost borne by the community.
narrative_ontology:constraint_stakeholder(kodashim_commandment_status__messianic_deferral, present_generation_needs, payer,
    powerless, immediate, trapped, local).
narrative_ontology:stakeholder_non_agent(kodashim_commandment_status__messianic_deferral, present_generation_needs).

% Those who believe the commandment is entirely suspended without a Temple, and that study is not a substitute for performance. They are marginalized in discourse that emphasizes the deferral reading's continuous relevance.
narrative_ontology:constraint_stakeholder(kodashim_commandment_status__messianic_deferral, performance_only_adherents, excluded,
    moderate, biographical, constrained, local).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the ongoing intellectual and spiritual engagement of the community with a central, but currently unfulfillable, set of commandments, ensuring continuity of tradition and readiness for a future messianic era.
% TRANSFER_FUNCTION: Transfers intellectual and spiritual resources (time, focus, scholarly effort) from immediate, actionable halakhic concerns to the theoretical study of Kodashim, from lay adherents to the maintenance of rabbinic authority and messianic expectation.
% ABSENT_VOICES: Those who advocate for a 'performance only' reading, arguing that the commandment is truly suspended and that resources should be reallocated to present-day needs, are often excluded from the dominant discourse which prioritizes the deferral reading.
% DISAPPEARANCE_RATIONALE: If this reading vanished, the intellectual and spiritual landscape of the community would significantly shift. Rabbinic authority would be challenged, messianic movements would lose a key interpretive framework, and resources would likely be reallocated towards more immediate communal needs, fundamentally altering the tradition's focus.
% FOUNDING_PROBLEM: How to maintain the integrity and relevance of the sacrificial commandments (Kodashim) after the destruction of the Temple, preventing their obsolescence while awaiting messianic restoration.
% FOUNDING_PROBLEM_CORROBORATION: Rabbinic literature and historical commentaries attest to the ongoing challenge of maintaining the relevance of these laws. While some contemporary voices (e.g., 'performance only' adherents) contest the 'live' status, the dominant scholarly tradition, supported by centuries of textual engagement, corroborates its continued importance.
narrative_ontology:disappearance_verdict(kodashim_commandment_status__messianic_deferral, world_rearranges).
narrative_ontology:founding_problem_status(kodashim_commandment_status__messianic_deferral, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(kodashim_commandment_status__messianic_deferral, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(kodashim_commandment_status__messianic_deferral, 'none', 1).
narrative_ontology:epsilon_provenance(kodashim_commandment_status__messianic_deferral, 0.45, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(kodashim_commandment_status__messianic_deferral_tests).
:- end_tests(kodashim_commandment_status__messianic_deferral_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.45) due to the opportunity cost of intellectual and spiritual resources dedicated to unfulfillable laws, diverting from present-day needs. Suppression is moderate (0.6) as this reading is actively enforced through rabbinic authority and educational curricula, marginalizing alternative interpretations. Theater ratio is low (0.1) because the study is genuinely seen as a preparation, not a mere performance, within this framework. Accessibility collapse is high (0.7) because the interpretive tradition strongly guides adherents towards this understanding, making alternative readings less accessible. Resistance is low (0.15) as the deferral reading is widely accepted within orthodox communities, with only minor internal dissent.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of rabbinic scholars and messianic movements, this is a vital rope, coordinating the community's spiritual continuity and future readiness. From the perspective of lay adherents and present-day needs, it functions more as a tangled rope, extracting resources for a deferred future while present needs go unmet.
 *
 * DIRECTIONALITY LOGIC:
 *   Rabbinic scholars and messianic movements are beneficiaries, as this reading reinforces their authority and eschatological vision. Lay adherents and 'present generation needs' (as a conceptual agent) are victims, bearing the opportunity cost. The 'performance only' adherents are excluded, as their interpretation is suppressed by the dominant narrative.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    opportunity_cost_quantification,
    'What is the quantifiable opportunity cost of dedicating significant intellectual and communal resources to the study of Kodashim laws, compared to alternative uses for present-day communal needs?',
    'Sociological and economic studies analyzing resource allocation within religious communities, comparing outcomes in communities with different interpretive emphases.',
    'A high quantifiable cost would strengthen the ''tangled rope'' classification by highlighting the extraction from present-generation needs; a low cost would lean towards a ''rope'' classification by minimizing the victim impact.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(opportunity_cost_quantification, empirical, 'Quantifying the real-world cost of deferred fulfillment.').

omega_variable(
    legitimacy_of_deferral_vs_obsolescence,
    'Is the ''messianic deferral'' reading a legitimate halakhic interpretation, or does it primarily serve to maintain institutional authority and messianic movements by preventing the obsolescence of a core textual body?',
    'Historical-critical analysis of rabbinic responsa and interpretive shifts over time, particularly during periods of messianic fervor or institutional consolidation, to discern underlying motivations.',
    'If primarily serving institutional maintenance, the ''extraction'' component of the constraint would be re-evaluated upward, potentially shifting to a ''snare'' for the lay adherents. If genuinely halakhically driven, the ''coordination'' aspect would be emphasized.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(legitimacy_of_deferral_vs_obsolescence, conceptual, 'Distinguishing genuine halakhic continuity from institutional self-preservation.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression of alternative readings structural (institutional barriers) or internalized (cognitive patterns within adherents)?',
    'Post-exposure trajectory: if adherents exposed to alternative readings still reject them due to internal conviction, reclassify as partially internalized. If rejection is primarily due to social pressure or lack of access to alternative texts, it is structural.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests — adherents carry the suppression with them. If purely structural, removing institutional barriers would lead to greater diversity of interpretation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for interpretive alternatives.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(kodashim_commandment_status__messianic_deferral, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(koda_tr_t0, kodashim_commandment_status__messianic_deferral, theater_ratio, 0, 0.1).
narrative_ontology:measurement(koda_tr_t25, kodashim_commandment_status__messianic_deferral, theater_ratio, 25, 0.1).
narrative_ontology:measurement(koda_tr_t50, kodashim_commandment_status__messianic_deferral, theater_ratio, 50, 0.1).
narrative_ontology:measurement(koda_tr_t75, kodashim_commandment_status__messianic_deferral, theater_ratio, 75, 0.1).
narrative_ontology:measurement(koda_tr_t100, kodashim_commandment_status__messianic_deferral, theater_ratio, 100, 0.1).

% Extraction over time
narrative_ontology:measurement(koda_be_t0, kodashim_commandment_status__messianic_deferral, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(koda_be_t25, kodashim_commandment_status__messianic_deferral, base_extractiveness, 25, 0.42).
narrative_ontology:measurement(koda_be_t50, kodashim_commandment_status__messianic_deferral, base_extractiveness, 50, 0.45).
narrative_ontology:measurement(koda_be_t75, kodashim_commandment_status__messianic_deferral, base_extractiveness, 75, 0.46).
narrative_ontology:measurement(koda_be_t100, kodashim_commandment_status__messianic_deferral, base_extractiveness, 100, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(koda_su_t0, kodashim_commandment_status__messianic_deferral, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(koda_su_t25, kodashim_commandment_status__messianic_deferral, suppression_requirement, 25, 0.58).
narrative_ontology:measurement(koda_su_t50, kodashim_commandment_status__messianic_deferral, suppression_requirement, 50, 0.6).
narrative_ontology:measurement(koda_su_t75, kodashim_commandment_status__messianic_deferral, suppression_requirement, 75, 0.6).
narrative_ontology:measurement(koda_su_t100, kodashim_commandment_status__messianic_deferral, suppression_requirement, 100, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(kodashim_commandment_status__messianic_deferral, identity_coordination).
narrative_ontology:affects_constraint(kodashim_commandment_status__messianic_deferral, kodashim_commandment_status__performance_only).
narrative_ontology:affects_constraint(kodashim_commandment_status__messianic_deferral, kodashim_commandment_status__study_as_performance).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'kodashim_commandment_status' kernel. It focuses on the messianic deferral of sacrificial laws. Sibling readings address the status of these laws as either purely suspended ('performance_only') or fulfilled through study ('study_as_performance').

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
