% ============================================================================
% CONSTRAINT STORY: sacrifice_obligation_continuity__messianic_suspension
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sacrifice_obligation_continuity__messianic_suspension, []).

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
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: sacrifice_obligation_continuity__messianic_suspension
 *   human_readable: Messianic Suspension of Sacrifice Obligation
 *   domain: religious_law/ritual_studies/textual_tradition
 *
 * SUMMARY:
 *   This constraint describes a specific reading within Jewish religious law
 *   concerning the obligation of Temple sacrifices after the destruction of
 *   the Second Temple. It posits that the obligation is neither fulfilled nor
 *   violated, but rather suspended, awaiting messianic restoration. The
 *   present-day activity of studying sacrifice laws is understood as a means
 *   of maintaining readiness for this future reactivation. This reading is
 *   one of several interpretations of how the commandment's normative force
 *   persists in the absence of its physical performance.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sacrifice_obligation_continuity__messianic_suspension, 0.35).
domain_priors:suppression_score(sacrifice_obligation_continuity__messianic_suspension, 0.2).
domain_priors:theater_ratio(sacrifice_obligation_continuity__messianic_suspension, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sacrifice_obligation_continuity__messianic_suspension, extractiveness, 0.35).
narrative_ontology:constraint_metric(sacrifice_obligation_continuity__messianic_suspension, suppression_requirement, 0.2).
narrative_ontology:constraint_metric(sacrifice_obligation_continuity__messianic_suspension, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(sacrifice_obligation_continuity__messianic_suspension, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(sacrifice_obligation_continuity__messianic_suspension, resistance, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sacrifice_obligation_continuity__messianic_suspension, rope).
narrative_ontology:human_readable(sacrifice_obligation_continuity__messianic_suspension, "Messianic Suspension of Sacrifice Obligation").
narrative_ontology:topic_domain(sacrifice_obligation_continuity__messianic_suspension, "religious_law/ritual_studies/textual_tradition").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(sacrifice_obligation_continuity__messianic_suspension, '586c25cd-f724-4083-a444-bf8fea5d7a29').
narrative_ontology:cs_kernel_codification('586c25cd-f724-4083-a444-bf8fea5d7a29', fixed_text).
narrative_ontology:cs_authority_grounding('586c25cd-f724-4083-a444-bf8fea5d7a29', lineage).
narrative_ontology:cs_interpretation_layer_present('586c25cd-f724-4083-a444-bf8fea5d7a29').
narrative_ontology:cs_reading_relation('586c25cd-f724-4083-a444-bf8fea5d7a29', sacrifice_obligation_continuity__archival_preservation, forecloses).
narrative_ontology:cs_reading_relation('586c25cd-f724-4083-a444-bf8fea5d7a29', sacrifice_obligation_continuity__performance_only, coexists_with).
narrative_ontology:cs_reading_relation('586c25cd-f724-4083-a444-bf8fea5d7a29', sacrifice_obligation_continuity__study_as_performance, coexists_with).
narrative_ontology:cs_axiom('586c25cd-f724-4083-a444-bf8fea5d7a29', foundational, obligation_is_temporally_suspended).
narrative_ontology:cs_axiom_status(obligation_is_temporally_suspended, holdable).
narrative_ontology:cs_axiom_grounding('586c25cd-f724-4083-a444-bf8fea5d7a29', obligation_is_temporally_suspended, deontological).
narrative_ontology:cs_axiom('586c25cd-f724-4083-a444-bf8fea5d7a29', foundational, study_maintains_future_readiness).
narrative_ontology:cs_axiom_status(study_maintains_future_readiness, holdable).
narrative_ontology:cs_axiom_grounding('586c25cd-f724-4083-a444-bf8fea5d7a29', study_maintains_future_readiness, conventional).
narrative_ontology:cs_reference_frame('586c25cd-f724-4083-a444-bf8fea5d7a29', post_destruction_rabbinic_consensus).
narrative_ontology:cs_drift_state('586c25cd-f724-4083-a444-bf8fea5d7a29', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('586c25cd-f724-4083-a444-bf8fea5d7a29', '').
narrative_ontology:cs_kernel_id(sacrifice_obligation_continuity__messianic_suspension, sacrifice_obligation_continuity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sacrifice_obligation_continuity__messianic_suspension, adherents_of_messianic_judaism).
narrative_ontology:constraint_beneficiary(sacrifice_obligation_continuity__messianic_suspension, rabbinic_scholars).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(sacrifice_obligation_continuity__messianic_suspension, adherents_of_messianic_judaism).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These individuals derive spiritual benefit, communal cohesion, and a sense of continuity from adhering to the belief that the sacrifice obligation is suspended but not abrogated. They bear the 'cost' of ongoing study and maintaining readiness, which is framed as a positive act of faith and commitment.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_continuity__messianic_suspension, adherents_of_messianic_judaism, beneficiary,
    moderate, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(sacrifice_obligation_continuity__messianic_suspension, adherents_of_messianic_judaism, payer).

% These scholars interpret and transmit the laws of sacrifice, guiding the community in their study and maintaining the textual tradition. They gain status, authority, and a central role in the community's religious life by upholding this interpretation.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_continuity__messianic_suspension, rabbinic_scholars, agenda_setter,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(sacrifice_obligation_continuity__messianic_suspension, rabbinic_scholars, beneficiary).

% The state of readiness and the preserved knowledge of sacrifice laws are maintained for the eventual arrival of the messianic era, when the Temple will be rebuilt and sacrifices reinstituted. This future state is the ultimate beneficiary of the present suspension and study.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_continuity__messianic_suspension, future_messianic_era, beneficiary,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(sacrifice_obligation_continuity__messianic_suspension, future_messianic_era).

% These individuals or academic bodies observe the religious practice and textual tradition from an external, non-adherent perspective, analyzing its historical, sociological, or philosophical implications without participating in its normative claims.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_continuity__messianic_suspension, secular_observers, observer,
    analytical, biographical, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a religious community around a shared understanding of a suspended divine commandment and a collective practice (study of sacrifice laws) to maintain readiness for its future reactivation upon messianic restoration.
% TRANSFER_FUNCTION: Transfers spiritual merit, communal cohesion, and a sense of historical continuity to adherents, and intellectual/interpretive authority to rabbinic scholars, in exchange for the time, effort, and intellectual engagement required for ongoing study.
% ABSENT_VOICES: Those who believe the sacrifice obligation is entirely abrogated (e.g., by a new covenant) or that study is an insufficient substitute for physical performance (e.g., certain Karaite or Samaritan traditions) would object. Their voices are absent from the dominant rabbinic discourse that upholds this reading.
% DISAPPEARANCE_RATIONALE: If this constraint vanished overnight, the entire framework of religious life for a significant segment of the Jewish community would be fundamentally altered. The expectation of restoration and the present practice of study as preparation are core to their identity and daily practice, necessitating a complete re-evaluation of their faith and purpose.
% FOUNDING_PROBLEM: The problem of how to maintain the continuity and normative force of divine commandments, specifically the Temple sacrifices, in the absence of the physical means (the Temple) for their performance after its destruction.
% FOUNDING_PROBLEM_CORROBORATION: The historical continuity of rabbinic tradition, spanning nearly two millennia, and the ongoing, widespread practice of studying sacrifice laws across diverse Jewish communities, attested by religious historians and ethnographers outside the immediate benefiting parties, corroborates that this problem remains live and central to their religious framework.
narrative_ontology:disappearance_verdict(sacrifice_obligation_continuity__messianic_suspension, world_rearranges).
narrative_ontology:founding_problem_status(sacrifice_obligation_continuity__messianic_suspension, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(sacrifice_obligation_continuity__messianic_suspension, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(sacrifice_obligation_continuity__messianic_suspension, 'none', 1).
narrative_ontology:epsilon_provenance(sacrifice_obligation_continuity__messianic_suspension, 0.35, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sacrifice_obligation_continuity__messianic_suspension_tests).
:- end_tests(sacrifice_obligation_continuity__messianic_suspension_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.35) because while there is a significant burden of time and intellectual effort dedicated to study, it is largely self-imposed and framed as a spiritual benefit rather than a coercive cost. Suppression is low (0.20) as there are no active external mechanisms preventing individuals from choosing not to study, though social and religious pressure exists. Theater ratio is low (0.10) because the study is genuinely believed to serve the purpose of maintaining readiness and is not merely performative. Accessibility collapse is moderate (0.60) because the physical performance of sacrifices is impossible, but the 'alternative' of ignoring the obligation is a choice, albeit one with religious consequences. Resistance is low (0.15) among adherents of this reading, as it provides a coherent framework for religious continuity.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of adherents and scholars, this constraint is a vital Rope, coordinating their religious life and maintaining a sacred tradition. From a secular or external observer's perspective, the ongoing study might appear as an elaborate, self-imposed burden with no immediate practical outcome, but it is not seen as actively extractive in a coercive sense.
 *
 * DIRECTIONALITY LOGIC:
 *   Adherents of Messianic Judaism are beneficiaries through spiritual and communal gains, but also 'payers' through the burden of study. Rabbinic scholars act as agenda-setters, guiding this study and benefiting from their institutional role and authority. The 'future messianic era' is an abstract beneficiary, for which the present readiness is maintained. There are no direct 'victims' as the obligation is suspended, not actively extracting from anyone.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint actively prevents mandatrophy by providing a framework for the continuity of a commandment whose physical performance is currently impossible. By defining the obligation as 'suspended' and study as 'readiness,' it ensures the mandate remains live and relevant, rather than atrophying into a mere historical curiosity or becoming a source of guilt for non-performance. The 'live' status of the founding problem directly counters mandatrophy.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    obligation_status_ambiguity,
    'Is the sacrifice obligation truly suspended, or is the act of study a form of substitute fulfillment, or is it merely archival preservation of a defunct practice?',
    'Theological consensus shift or a definitive messianic event. Absent such, the ambiguity persists as a core interpretive challenge.',
    'If study is deemed substitute fulfillment, the constraint''s extractiveness might be re-evaluated (as a ''cost'' of active fulfillment). If it''s archival, the normative force and thus extractiveness would drop significantly.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(obligation_status_ambiguity, conceptual, 'Ambiguity regarding the precise normative status of the sacrifice obligation in the present era.').

omega_variable(
    readiness_sufficiency_criteria,
    'What specific criteria define ''readiness'' for messianic restoration, and is the current level and nature of study sufficient to meet these criteria?',
    'Internal rabbinic debate and consensus, or external theological revelation. Empirical assessment of ''readiness'' is not applicable.',
    'If current study is deemed insufficient, it could lead to increased pressure for more intensive study (raising extractiveness) or a re-evaluation of the ''readiness'' claim.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(readiness_sufficiency_criteria, conceptual, 'Uncertainty about the definition and sufficiency of ''readiness'' through study.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sacrifice_obligation_continuity__messianic_suspension, 70, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sacr_tr_t70, sacrifice_obligation_continuity__messianic_suspension, theater_ratio, 70, 0.08).
narrative_ontology:measurement(sacr_tr_t400, sacrifice_obligation_continuity__messianic_suspension, theater_ratio, 400, 0.09).
narrative_ontology:measurement(sacr_tr_t800, sacrifice_obligation_continuity__messianic_suspension, theater_ratio, 800, 0.1).
narrative_ontology:measurement(sacr_tr_t1200, sacrifice_obligation_continuity__messianic_suspension, theater_ratio, 1200, 0.1).
narrative_ontology:measurement(sacr_tr_t1600, sacrifice_obligation_continuity__messianic_suspension, theater_ratio, 1600, 0.1).
narrative_ontology:measurement(sacr_tr_t2024, sacrifice_obligation_continuity__messianic_suspension, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(sacr_be_t70, sacrifice_obligation_continuity__messianic_suspension, base_extractiveness, 70, 0.3).
narrative_ontology:measurement(sacr_be_t400, sacrifice_obligation_continuity__messianic_suspension, base_extractiveness, 400, 0.32).
narrative_ontology:measurement(sacr_be_t800, sacrifice_obligation_continuity__messianic_suspension, base_extractiveness, 800, 0.33).
narrative_ontology:measurement(sacr_be_t1200, sacrifice_obligation_continuity__messianic_suspension, base_extractiveness, 1200, 0.34).
narrative_ontology:measurement(sacr_be_t1600, sacrifice_obligation_continuity__messianic_suspension, base_extractiveness, 1600, 0.35).
narrative_ontology:measurement(sacr_be_t2024, sacrifice_obligation_continuity__messianic_suspension, base_extractiveness, 2024, 0.35).

% Suppression requirement over time
narrative_ontology:measurement(sacr_su_t70, sacrifice_obligation_continuity__messianic_suspension, suppression_requirement, 70, 0.18).
narrative_ontology:measurement(sacr_su_t400, sacrifice_obligation_continuity__messianic_suspension, suppression_requirement, 400, 0.19).
narrative_ontology:measurement(sacr_su_t800, sacrifice_obligation_continuity__messianic_suspension, suppression_requirement, 800, 0.2).
narrative_ontology:measurement(sacr_su_t1200, sacrifice_obligation_continuity__messianic_suspension, suppression_requirement, 1200, 0.2).
narrative_ontology:measurement(sacr_su_t1600, sacrifice_obligation_continuity__messianic_suspension, suppression_requirement, 1600, 0.2).
narrative_ontology:measurement(sacr_su_t2024, sacrifice_obligation_continuity__messianic_suspension, suppression_requirement, 2024, 0.2).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sacrifice_obligation_continuity__messianic_suspension, identity_coordination).
narrative_ontology:affects_constraint(sacrifice_obligation_continuity__messianic_suspension, sacrifice_obligation_continuity__archival_preservation).
narrative_ontology:affects_constraint(sacrifice_obligation_continuity__messianic_suspension, sacrifice_obligation_continuity__performance_only).
narrative_ontology:affects_constraint(sacrifice_obligation_continuity__messianic_suspension, sacrifice_obligation_continuity__study_as_performance).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'sacrifice_obligation_continuity' kernel, focusing on the suspension of the obligation and the role of study in maintaining readiness for messianic restoration. It is structurally distinct from other readings that interpret the obligation as abrogated, fulfilled by study, or requiring only physical performance.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
