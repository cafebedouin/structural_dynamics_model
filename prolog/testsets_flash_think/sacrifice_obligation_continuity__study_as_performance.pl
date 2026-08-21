% ============================================================================
% CONSTRAINT STORY: sacrifice_obligation_continuity__study_as_performance
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sacrifice_obligation_continuity__study_as_performance, []).

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
 *   constraint_id: sacrifice_obligation_continuity__study_as_performance
 *   human_readable: Sacrifice Obligation Continuity: Study as Performance
 *   domain: religious/ritual_studies/textual_tradition
 *
 * SUMMARY:
 *   This constraint represents the reading that the study of sacrifice law is
 *   itself a fulfillment of the commandment, allowing the obligation to
 *   persist through textual engagement. This interpretation emerged
 *   historically in response to the destruction of the Temple and the
 *   cessation of physical sacrifices. It provides a viable and accessible
 *   path for adherents to maintain religious observance. The constraint is
 *   claimed as a 'rope' because it coordinates a collective religious
 *   practice around a shared interpretation, with low extraction and
 *   suppression, as study is generally accessible and seen as beneficial by
 *   its adherents.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sacrifice_obligation_continuity__study_as_performance, 0.15).
domain_priors:suppression_score(sacrifice_obligation_continuity__study_as_performance, 0.1).
domain_priors:theater_ratio(sacrifice_obligation_continuity__study_as_performance, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sacrifice_obligation_continuity__study_as_performance, extractiveness, 0.15).
narrative_ontology:constraint_metric(sacrifice_obligation_continuity__study_as_performance, suppression_requirement, 0.1).
narrative_ontology:constraint_metric(sacrifice_obligation_continuity__study_as_performance, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(sacrifice_obligation_continuity__study_as_performance, accessibility_collapse, 0.2).
narrative_ontology:constraint_metric(sacrifice_obligation_continuity__study_as_performance, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sacrifice_obligation_continuity__study_as_performance, rope).
narrative_ontology:human_readable(sacrifice_obligation_continuity__study_as_performance, "Sacrifice Obligation Continuity: Study as Performance").
narrative_ontology:topic_domain(sacrifice_obligation_continuity__study_as_performance, "religious/ritual_studies/textual_tradition").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(sacrifice_obligation_continuity__study_as_performance, '6a58d4a3-cedc-401d-a155-004e54c01c45').
narrative_ontology:cs_kernel_codification('6a58d4a3-cedc-401d-a155-004e54c01c45', fixed_text).
narrative_ontology:cs_authority_grounding('6a58d4a3-cedc-401d-a155-004e54c01c45', lineage).
narrative_ontology:cs_interpretation_layer_present('6a58d4a3-cedc-401d-a155-004e54c01c45').
narrative_ontology:cs_reading_relation('6a58d4a3-cedc-401d-a155-004e54c01c45', sacrifice_obligation_continuity__archival_preservation, forecloses).
narrative_ontology:cs_reading_relation('6a58d4a3-cedc-401d-a155-004e54c01c45', sacrifice_obligation_continuity__messianic_suspension, forecloses).
narrative_ontology:cs_reading_relation('6a58d4a3-cedc-401d-a155-004e54c01c45', sacrifice_obligation_continuity__performance_only, forecloses).
narrative_ontology:cs_axiom('6a58d4a3-cedc-401d-a155-004e54c01c45', foundational, textual_engagement_as_ritual_performance).
narrative_ontology:cs_axiom_status(textual_engagement_as_ritual_performance, holdable).
narrative_ontology:cs_axiom_grounding('6a58d4a3-cedc-401d-a155-004e54c01c45', textual_engagement_as_ritual_performance, deontological).
narrative_ontology:cs_axiom('6a58d4a3-cedc-401d-a155-004e54c01c45', foundational, obligation_transfers_to_intellectual_domain).
narrative_ontology:cs_axiom_status(obligation_transfers_to_intellectual_domain, holdable).
narrative_ontology:cs_axiom_grounding('6a58d4a3-cedc-401d-a155-004e54c01c45', obligation_transfers_to_intellectual_domain, deontological).
narrative_ontology:cs_reference_frame('6a58d4a3-cedc-401d-a155-004e54c01c45', rabbinic_interpretive_tradition).
narrative_ontology:cs_drift_state('6a58d4a3-cedc-401d-a155-004e54c01c45', contemporary_religious_practice, gap(stable, minor, true)).
narrative_ontology:cs_created_at('6a58d4a3-cedc-401d-a155-004e54c01c45', '').
narrative_ontology:cs_kernel_id(sacrifice_obligation_continuity__study_as_performance, sacrifice_obligation_continuity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sacrifice_obligation_continuity__study_as_performance, religious_scholars).
narrative_ontology:constraint_beneficiary(sacrifice_obligation_continuity__study_as_performance, adherents_of_this_reading).
narrative_ontology:constraint_vindicates(sacrifice_obligation_continuity__study_as_performance, textual_engagement_as_spiritual_fulfillment).
narrative_ontology:constraint_vindicates(sacrifice_obligation_continuity__study_as_performance, rabbinic_authority_in_interpreting_commandments).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Their intellectual and spiritual work is elevated to a form of ritual fulfillment, providing a central purpose and legitimizing their role within the religious community. Their identity is deeply intertwined with this interpretive tradition.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_continuity__study_as_performance, religious_scholars, beneficiary,
    institutional, generational, identity_locked, global).

% They gain a tangible, accessible means to fulfill a core religious commandment, alleviating the burden of an otherwise impossible obligation. Their spiritual practice is validated and given clear direction.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_continuity__study_as_performance, adherents_of_this_reading, beneficiary,
    moderate, biographical, constrained, local).

% They promulgate and uphold this interpretation, guiding their communities in its practice. Their authority is reinforced by providing a viable path to religious observance in challenging circumstances.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_continuity__study_as_performance, religious_authorities_of_this_reading, agenda_setter,
    institutional, generational, identity_locked, global).

% Academically analyze the historical development, theological implications, and social functions of this interpretation within the broader context of religious law and practice.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_continuity__study_as_performance, ritual_studies_scholars, observer,
    analytical, generational, analytical, global).

% They believe that only physical performance of sacrifice fulfills the commandment and view study as preparation, not fulfillment. They are excluded from the interpretive consensus of this reading and may feel their path to observance is blocked.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_continuity__study_as_performance, adherents_of_performance_only_reading, excluded,
    powerless, biographical, identity_locked, local).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a coherent and accessible framework for religious adherents to fulfill the commandment of sacrifice in the absence of a physical Temple and ritual, coordinating their spiritual and intellectual efforts.
% TRANSFER_FUNCTION: Transfers the locus of religious obligation and its fulfillment from the physical performance of animal sacrifice to the intellectual and spiritual engagement with the sacred texts detailing those laws.
% ABSENT_VOICES: Adherents of the 'performance_only' reading would object, arguing that study, while meritorious, cannot substitute for actual ritual performance. They are excluded from the interpretive authority that promulgates this reading.
% DISAPPEARANCE_RATIONALE: If this interpretation vanished, a significant portion of religious practice and scholarly endeavor would lose its primary justification for fulfilling a core commandment. Adherents would face a profound crisis of how to observe their faith, leading to a fundamental reorganization of religious life and theological discourse.
% FOUNDING_PROBLEM: The destruction of the Second Temple and the subsequent inability to perform physical animal sacrifices, leaving a central religious commandment unfulfillable and creating a crisis of religious observance.
% FOUNDING_PROBLEM_CORROBORATION: Historical religious texts, rabbinic commentaries, and contemporary theological discourse from various schools of thought (including those that disagree with this reading's conclusion) corroborate the historical problem of the Temple's destruction and the ongoing challenge of fulfilling commandments tied to it. The problem is widely acknowledged as still existing.
narrative_ontology:disappearance_verdict(sacrifice_obligation_continuity__study_as_performance, world_rearranges).
narrative_ontology:founding_problem_status(sacrifice_obligation_continuity__study_as_performance, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(sacrifice_obligation_continuity__study_as_performance, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(sacrifice_obligation_continuity__study_as_performance, 'none', 1).
narrative_ontology:epsilon_provenance(sacrifice_obligation_continuity__study_as_performance, 0.15, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sacrifice_obligation_continuity__study_as_performance_tests).
:- end_tests(sacrifice_obligation_continuity__study_as_performance_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.15) because the act of study is generally accessible and does not impose significant material costs beyond time and effort, which are seen as part of the spiritual benefit. Suppression is low (0.10) as this interpretation is widely accepted within its tradition, and there are few coercive mechanisms to enforce it; adherence is largely voluntary. Theater ratio is very low (0.05) because the act of study is considered genuine fulfillment, not a mere performance or substitute. Accessibility collapse is low (0.20) as study is a widely available alternative to physical sacrifice. Resistance is low (0.05) as this reading is a foundational aspect of the tradition.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of adherents of this reading, the constraint is a beneficial coordination mechanism that enables religious fulfillment. From the perspective of those who believe only physical performance counts, this reading might be seen as a theological compromise or even an evasion, but within this constraint's own framework, it functions as a genuine rope.
 *
 * DIRECTIONALITY LOGIC:
 *   Religious scholars and adherents of this reading are clear beneficiaries, as the constraint provides a meaningful path for their religious practice and legitimizes their roles. There are no direct victims, as the obligation is considered fulfilled, not extracted from. Religious authorities act as agenda-setters by upholding and transmitting this interpretation. Adherents of rival interpretations (e.g., 'performance_only') are structurally excluded from this reading's framework.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    study_as_fulfillment_vs_preparation,
    'Is the study of sacrifice law truly a fulfillment of the commandment, or is it merely a preparation for future physical performance?',
    'Further theological and jurisprudential debate within the religious tradition, potentially influenced by new scriptural interpretations or communal consensus shifts.',
    'If reclassified as mere preparation, the constraint''s extractiveness would rise (as the obligation remains unfulfilled), and its claimed type might shift towards a ''snare'' or ''tangled_rope'' for those trapped by an unfulfillable commandment.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(study_as_fulfillment_vs_preparation, conceptual, 'Ambiguity regarding the normative status of study in fulfilling the sacrifice commandment.').

omega_variable(
    obligation_locus_shift_legitimacy,
    'Is the shift of the obligation''s locus from physical ritual to intellectual engagement a legitimate reinterpretation or a theological innovation that deviates from original intent?',
    'Comparative analysis with other religious traditions facing similar ritual cessation, and deeper historical-critical study of early rabbinic responses to the Temple''s destruction.',
    'If deemed an illegitimate deviation, the authority grounding of this reading could be challenged, potentially leading to a ''piton'' classification if maintained purely by institutional inertia, or a ''snare'' if enforced coercively.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(obligation_locus_shift_legitimacy, conceptual, 'Legitimacy of reinterpreting the locus of religious obligation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sacrifice_obligation_continuity__study_as_performance, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sacr_tr_t0, sacrifice_obligation_continuity__study_as_performance, theater_ratio, 0, 0.05).
narrative_ontology:measurement(sacr_tr_t25, sacrifice_obligation_continuity__study_as_performance, theater_ratio, 25, 0.05).
narrative_ontology:measurement(sacr_tr_t50, sacrifice_obligation_continuity__study_as_performance, theater_ratio, 50, 0.05).
narrative_ontology:measurement(sacr_tr_t75, sacrifice_obligation_continuity__study_as_performance, theater_ratio, 75, 0.05).
narrative_ontology:measurement(sacr_tr_t100, sacrifice_obligation_continuity__study_as_performance, theater_ratio, 100, 0.05).

% Extraction over time
narrative_ontology:measurement(sacr_be_t0, sacrifice_obligation_continuity__study_as_performance, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(sacr_be_t25, sacrifice_obligation_continuity__study_as_performance, base_extractiveness, 25, 0.15).
narrative_ontology:measurement(sacr_be_t50, sacrifice_obligation_continuity__study_as_performance, base_extractiveness, 50, 0.15).
narrative_ontology:measurement(sacr_be_t75, sacrifice_obligation_continuity__study_as_performance, base_extractiveness, 75, 0.15).
narrative_ontology:measurement(sacr_be_t100, sacrifice_obligation_continuity__study_as_performance, base_extractiveness, 100, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(sacr_su_t0, sacrifice_obligation_continuity__study_as_performance, suppression_requirement, 0, 0.1).
narrative_ontology:measurement(sacr_su_t25, sacrifice_obligation_continuity__study_as_performance, suppression_requirement, 25, 0.1).
narrative_ontology:measurement(sacr_su_t50, sacrifice_obligation_continuity__study_as_performance, suppression_requirement, 50, 0.1).
narrative_ontology:measurement(sacr_su_t75, sacrifice_obligation_continuity__study_as_performance, suppression_requirement, 75, 0.1).
narrative_ontology:measurement(sacr_su_t100, sacrifice_obligation_continuity__study_as_performance, suppression_requirement, 100, 0.1).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sacrifice_obligation_continuity__study_as_performance, identity_coordination).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'sacrifice_obligation_continuity' kernel, focusing on study as fulfillment. It is structurally distinct from sibling readings that offer alternative interpretations of the same core problem.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
