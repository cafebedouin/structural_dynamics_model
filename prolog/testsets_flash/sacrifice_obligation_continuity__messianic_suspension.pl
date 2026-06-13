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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
 *   constraint_id: sacrifice_obligation_continuity__messianic_suspension
 *   human_readable: Sacrifice Obligation Continuity (Messianic Suspension Reading)
 *   domain: religious_law/ritual_studies/textual_tradition
 *
 * SUMMARY:
 *   This constraint describes the 'messianic suspension' reading of the
 *   sacrifice obligation in a religious tradition. According to this reading,
 *   the obligation to perform sacrifices is not currently active (due to the
 *   absence of the Temple and messianic era) but is also not abrogated or
 *   violated. Instead, it is suspended, awaiting future messianic
 *   restoration. The study of sacrifice law is understood as a means of
 *   maintaining readiness for this future performance, ensuring the knowledge
 *   and capacity are preserved. This reading avoids the guilt of
 *   non-performance while maintaining the normative force of the commandment.
 *
 * KEY AGENTS:
 *   - religious_scholars: Agenda setter / Beneficiary (institutional/analytical) — interpret and transmit the tradition, derive status from its continuity
 *   - religious_community: Beneficiary (organized) — avoids guilt of non-performance, maintains connection to tradition
 *   - messianic_aspirants: Payer (moderate) — bear the burden of maintaining readiness and anticipation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sacrifice_obligation_continuity__messianic_suspension, 0.4).
domain_priors:suppression_score(sacrifice_obligation_continuity__messianic_suspension, 0.2).
domain_priors:theater_ratio(sacrifice_obligation_continuity__messianic_suspension, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sacrifice_obligation_continuity__messianic_suspension, extractiveness, 0.4).
narrative_ontology:constraint_metric(sacrifice_obligation_continuity__messianic_suspension, suppression_requirement, 0.2).
narrative_ontology:constraint_metric(sacrifice_obligation_continuity__messianic_suspension, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(sacrifice_obligation_continuity__messianic_suspension, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(sacrifice_obligation_continuity__messianic_suspension, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sacrifice_obligation_continuity__messianic_suspension, rope).
narrative_ontology:human_readable(sacrifice_obligation_continuity__messianic_suspension, "Sacrifice Obligation Continuity (Messianic Suspension Reading)").
narrative_ontology:topic_domain(sacrifice_obligation_continuity__messianic_suspension, "religious_law/ritual_studies/textual_tradition").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(sacrifice_obligation_continuity__messianic_suspension, '9521fa3c-51a5-457a-99d2-d5cd075bd496').
narrative_ontology:cs_kernel_codification('9521fa3c-51a5-457a-99d2-d5cd075bd496', fixed_text).
narrative_ontology:cs_authority_grounding('9521fa3c-51a5-457a-99d2-d5cd075bd496', lineage).
narrative_ontology:cs_interpretation_layer_present('9521fa3c-51a5-457a-99d2-d5cd075bd496').
narrative_ontology:cs_reading_relation('9521fa3c-51a5-457a-99d2-d5cd075bd496', sacrifice_obligation_continuity__study_as_performance, coexists_with).
narrative_ontology:cs_reading_relation('9521fa3c-51a5-457a-99d2-d5cd075bd496', sacrifice_obligation_continuity__performance_only, coexists_with).
narrative_ontology:cs_reading_relation('9521fa3c-51a5-457a-99d2-d5cd075bd496', sacrifice_obligation_continuity__archival_preservation, forecloses).
narrative_ontology:cs_axiom('9521fa3c-51a5-457a-99d2-d5cd075bd496', foundational, obligation_retains_normative_force_even_when_unperformable).
narrative_ontology:cs_axiom_status(obligation_retains_normative_force_even_when_unperformable, holdable).
narrative_ontology:cs_axiom_grounding('9521fa3c-51a5-457a-99d2-d5cd075bd496', obligation_retains_normative_force_even_when_unperformable, deontological).
narrative_ontology:cs_axiom('9521fa3c-51a5-457a-99d2-d5cd075bd496', secondary, study_maintains_readiness_for_future_performance).
narrative_ontology:cs_axiom_status(study_maintains_readiness_for_future_performance, holdable).
narrative_ontology:cs_axiom_grounding('9521fa3c-51a5-457a-99d2-d5cd075bd496', study_maintains_readiness_for_future_performance, conventional).
narrative_ontology:cs_reference_frame('9521fa3c-51a5-457a-99d2-d5cd075bd496', pre_destruction_normative_continuity).
narrative_ontology:cs_drift_state('9521fa3c-51a5-457a-99d2-d5cd075bd496', post_temple_destruction_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('9521fa3c-51a5-457a-99d2-d5cd075bd496', '').
narrative_ontology:cs_kernel_id(sacrifice_obligation_continuity__messianic_suspension, sacrifice_obligation_continuity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sacrifice_obligation_continuity__messianic_suspension, religious_scholars).
narrative_ontology:constraint_beneficiary(sacrifice_obligation_continuity__messianic_suspension, religious_community).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(sacrifice_obligation_continuity__messianic_suspension, messianic_aspirants).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interpret and transmit the complex body of sacrifice law, ensuring its continuity and relevance. They derive significant authority and social capital from maintaining this interpretive framework. Their professional identity is deeply fused with the tradition.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_continuity__messianic_suspension, religious_scholars, agenda_setter,
    institutional, generational, identity_locked, global).

% Adheres to this understanding of the sacrifice obligation, finding theological coherence and avoiding the guilt of non-performance. Participation in study groups and communal prayer reinforces this framework. Their identity is tied to the continuity of the tradition.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_continuity__messianic_suspension, religious_community, beneficiary,
    organized, generational, constrained, global).

% Bear the psychological and intellectual burden of maintaining active anticipation and readiness for the messianic era. This involves dedicated study and adherence to practices that symbolize future restoration. Their identity is strongly tied to this future-oriented commitment.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_continuity__messianic_suspension, messianic_aspirants, payer,
    moderate, biographical, identity_locked, global).

% Study the evolution of religious law and practice from an academic perspective, analyzing the social and intellectual functions of such interpretive frameworks without internalizing their normative claims. They provide external corroboration or critique.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_continuity__messianic_suspension, secular_historians, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(sacrifice_obligation_continuity__messianic_suspension, religious_scholars).
narrative_ontology:fixing_cost_class(sacrifice_obligation_continuity__messianic_suspension, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the religious community's understanding and practice regarding a central commandment (sacrifice) during a period when its physical performance is impossible, maintaining its normative force and future relevance.
% TRANSFER_FUNCTION: Transfers the burden of active ritual performance into a burden of study and anticipation, from the entire community to those dedicated to maintaining the tradition, while transferring theological coherence and continuity to the community.
% ABSENT_VOICES: Those who believe the obligation is either entirely abrogated or that study is insufficient for fulfillment are marginalized in this discourse. They would argue for either a complete re-evaluation or a more active, albeit symbolic, form of performance.
% DISAPPEARANCE_RATIONALE: If this understanding vanished, the religious community would face a profound theological crisis regarding the status of a core commandment. It would either lead to widespread guilt over non-performance, or a complete abandonment of the obligation, fundamentally altering the tradition's self-conception.
% FOUNDING_PROBLEM: The destruction of the Temple rendered physical sacrifice impossible, creating a crisis of continuity for a central religious obligation and threatening the coherence of religious practice.
% FOUNDING_PROBLEM_CORROBORATION: Religious texts and historical accounts from various periods attest to the crisis following the Temple's destruction. Secular historians corroborate the historical problem, while religious scholars and community leaders attest to its ongoing theological relevance, even if the practical problem is 'solved' by suspension.
narrative_ontology:disappearance_verdict(sacrifice_obligation_continuity__messianic_suspension, world_rearranges).
narrative_ontology:founding_problem_status(sacrifice_obligation_continuity__messianic_suspension, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(sacrifice_obligation_continuity__messianic_suspension, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(sacrifice_obligation_continuity__messianic_suspension, 'none', 1).

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
 *   The constraint is classified as a Rope because it coordinates the community's relationship to a core religious obligation in a period of non-performance, providing a coherent framework for continuity. Extractiveness is moderate (0.4) as it imposes a burden of study and anticipation without immediate ritual fulfillment. Suppression is low (0.2) as adherence is largely voluntary and driven by internal theological commitment rather than external coercion. Theater ratio is low (0.1) because the study is genuinely seen as functional for future readiness, not merely performative. Accessibility collapse is high (0.7) because, within this theological framework, there are few 'alternatives' to this understanding of the obligation's status.
 *
 * PERSPECTIVAL GAP:
 *   Religious scholars, as agenda setters, experience this as a robust framework for maintaining tradition and their own authority. The broader religious community experiences it as a comforting resolution to a theological dilemma. Those who intensely anticipate the messianic era might feel a greater 'burden of readiness' (payer seat), but still within a coordinated framework.
 *
 * DIRECTIONALITY LOGIC:
 *   Religious scholars and the religious community are beneficiaries, as this reading provides a coherent and guilt-free framework for a central religious practice. There are no direct 'victims' as the obligation is suspended, not violated. The 'burden of readiness' falls on those who actively engage in study and anticipation, but this is framed as a positive act of faith, not extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification prevents mislabeling the 'suspension' as either a 'piton' (if the readiness function were purely theatrical) or a 'snare' (if the study were a coercive, extractive burden). The 'rope' classification acknowledges the genuine coordination function of maintaining a living tradition and future option, while the moderate extractiveness reflects the real, albeit non-coercive, burden of readiness. The 'contested' status of the founding problem (is fragmentation still a live threat?) is central to the ongoing debate about the constraint's true nature.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    messianic_timing_uncertainty,
    'Is the messianic restoration a definite future event, or a symbolic/aspirational ideal?',
    'Theological consensus shift or empirical observation of messianic events.',
    'If symbolic, the ''suspension'' becomes a permanent deferral, potentially reclassifying the constraint towards ''archival_preservation'' or ''piton'' as the readiness function atrophies. If definite, the ''rope'' classification is strengthened.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(messianic_timing_uncertainty, conceptual, 'Uncertainty regarding the nature and timing of messianic restoration.').

omega_variable(
    study_efficacy_ambiguity,
    'Does the act of studying sacrifice law genuinely maintain readiness for future performance, or is it primarily a form of intellectual and spiritual engagement?',
    'Theological debate and re-evaluation of the relationship between theoretical knowledge and practical ritual capacity.',
    'If study is primarily engagement, the ''readiness'' claim weakens, potentially shifting the constraint towards ''study_as_performance'' or ''archival_preservation'' where the ''rope'' function of maintaining a future option is less central.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(study_efficacy_ambiguity, conceptual, 'Ambiguity regarding the practical efficacy of study in maintaining ritual readiness.').

omega_variable(
    kernel_reading_identification,
    'This constraint is one reading of the ''sacrifice_obligation_continuity'' kernel. What would change if a sibling reading were adopted?',
    'Explicit declaration by religious authorities or a shift in communal practice.',
    'If ''study_as_performance'' were adopted, the obligation would be considered fulfilled through study, removing the ''suspended'' status. If ''archival_preservation'' were adopted, the normative force of the obligation would cease, and study would become purely historical. If ''performance_only'' were adopted, the current state would be seen as a violation, not a suspension.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identification, conceptual, 'This constraint is the ''messianic_suspension'' reading of the ''sacrifice_obligation_continuity'' kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sacrifice_obligation_continuity__messianic_suspension, 0, 200).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sacr_tr_t0, sacrifice_obligation_continuity__messianic_suspension, theater_ratio, 0, 0.1).
narrative_ontology:measurement(sacr_tr_t100, sacrifice_obligation_continuity__messianic_suspension, theater_ratio, 100, 0.1).
narrative_ontology:measurement(sacr_tr_t200, sacrifice_obligation_continuity__messianic_suspension, theater_ratio, 200, 0.1).

% Extraction over time
narrative_ontology:measurement(sacr_be_t0, sacrifice_obligation_continuity__messianic_suspension, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(sacr_be_t100, sacrifice_obligation_continuity__messianic_suspension, base_extractiveness, 100, 0.38).
narrative_ontology:measurement(sacr_be_t200, sacrifice_obligation_continuity__messianic_suspension, base_extractiveness, 200, 0.4).

% Suppression requirement over time
narrative_ontology:measurement(sacr_su_t0, sacrifice_obligation_continuity__messianic_suspension, suppression_requirement, 0, 0.15).
narrative_ontology:measurement(sacr_su_t100, sacrifice_obligation_continuity__messianic_suspension, suppression_requirement, 100, 0.18).
narrative_ontology:measurement(sacr_su_t200, sacrifice_obligation_continuity__messianic_suspension, suppression_requirement, 200, 0.2).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sacrifice_obligation_continuity__messianic_suspension, identity_coordination).
narrative_ontology:boltzmann_floor_override(sacrifice_obligation_continuity__messianic_suspension, 0.08).
narrative_ontology:affects_constraint(sacrifice_obligation_continuity__messianic_suspension, sacrifice_obligation_continuity__study_as_performance).
narrative_ontology:affects_constraint(sacrifice_obligation_continuity__messianic_suspension, sacrifice_obligation_continuity__performance_only).
narrative_ontology:affects_constraint(sacrifice_obligation_continuity__messianic_suspension, sacrifice_obligation_continuity__archival_preservation).

% DUAL FORMULATION NOTE:
% This constraint is one of four readings of the 'sacrifice_obligation_continuity' kernel, each representing a distinct structural claim about the status and fulfillment of the sacrifice commandment in the absence of the Temple. Each reading has a distinct epsilon and stakeholder structure.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
