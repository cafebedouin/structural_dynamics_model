% ============================================================================
% CONSTRAINT STORY: kodashim_commandment_status__study_as_performance
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_kodashim_commandment_status__study_as_performance, []).

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
 *   constraint_id: kodashim_commandment_status__study_as_performance
 *   human_readable: Kodashim Commandment Status: Study as Performance
 *   domain: religious_studies/halakhic_theory
 *
 * SUMMARY:
 *   This constraint represents the rabbinic Jewish interpretive tradition
 *   that the study of the laws of Temple sacrifices (Kodashim) is considered
 *   equivalent to, and fulfills, the commandment itself in the absence of the
 *   Temple. This reading ensures the continuity and relevance of a
 *   significant body of Halakha, providing adherents with a means to engage
 *   with divine will despite historical circumstances. It is one reading of
 *   the 'kodashim_commandment_status' kernel, which addresses the status of
 *   these laws after the Temple's destruction.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(kodashim_commandment_status__study_as_performance, 0.05).
domain_priors:suppression_score(kodashim_commandment_status__study_as_performance, 0.1).
domain_priors:theater_ratio(kodashim_commandment_status__study_as_performance, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(kodashim_commandment_status__study_as_performance, extractiveness, 0.05).
narrative_ontology:constraint_metric(kodashim_commandment_status__study_as_performance, suppression_requirement, 0.1).
narrative_ontology:constraint_metric(kodashim_commandment_status__study_as_performance, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(kodashim_commandment_status__study_as_performance, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(kodashim_commandment_status__study_as_performance, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(kodashim_commandment_status__study_as_performance, rope).
narrative_ontology:human_readable(kodashim_commandment_status__study_as_performance, "Kodashim Commandment Status: Study as Performance").
narrative_ontology:topic_domain(kodashim_commandment_status__study_as_performance, "religious_studies/halakhic_theory").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(kodashim_commandment_status__study_as_performance, '27838bae-caac-4851-a979-623f383bde92').
narrative_ontology:cs_kernel_codification('27838bae-caac-4851-a979-623f383bde92', fixed_text).
narrative_ontology:cs_authority_grounding('27838bae-caac-4851-a979-623f383bde92', lineage).
narrative_ontology:cs_interpretation_layer_present('27838bae-caac-4851-a979-623f383bde92').
narrative_ontology:cs_reading_relation('27838bae-caac-4851-a979-623f383bde92', kodashim_commandment_status__messianic_deferral, coexists_with).
narrative_ontology:cs_reading_relation('27838bae-caac-4851-a979-623f383bde92', kodashim_commandment_status__performance_only, coexists_with).
narrative_ontology:cs_axiom('27838bae-caac-4851-a979-623f383bde92', foundational, torah_study_as_divine_service).
narrative_ontology:cs_axiom_status(torah_study_as_divine_service, holdable).
narrative_ontology:cs_axiom_grounding('27838bae-caac-4851-a979-623f383bde92', torah_study_as_divine_service, deontological).
narrative_ontology:cs_axiom('27838bae-caac-4851-a979-623f383bde92', secondary, commandment_continuity_through_intellect).
narrative_ontology:cs_axiom_status(commandment_continuity_through_intellect, holdable).
narrative_ontology:cs_axiom_grounding('27838bae-caac-4851-a979-623f383bde92', commandment_continuity_through_intellect, theological).
narrative_ontology:cs_reference_frame('27838bae-caac-4851-a979-623f383bde92', post_temple_destruction_rabbinic_consensus).
narrative_ontology:cs_drift_state('27838bae-caac-4851-a979-623f383bde92', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('27838bae-caac-4851-a979-623f383bde92', '').
narrative_ontology:cs_kernel_id(kodashim_commandment_status__study_as_performance, kodashim_commandment_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(kodashim_commandment_status__study_as_performance, adherents_of_halakha).
narrative_ontology:constraint_beneficiary(kodashim_commandment_status__study_as_performance, rabbinic_scholars).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Individuals committed to Jewish law who find a path to fulfill the commandments related to Temple sacrifices through intellectual engagement, maintaining their religious obligations and identity in the absence of the Temple.
narrative_ontology:constraint_stakeholder(kodashim_commandment_status__study_as_performance, adherents_of_halakha, beneficiary,
    moderate, biographical, mobile, global).

% The intellectual and spiritual leaders whose ongoing study and interpretation of Kodashim (laws of sacrifices) are elevated to a form of divine service, thereby maintaining the vitality and relevance of this body of law and their own institutional role.
narrative_ontology:constraint_stakeholder(kodashim_commandment_status__study_as_performance, rabbinic_scholars, agenda_setter,
    institutional, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(kodashim_commandment_status__study_as_performance, rabbinic_scholars, beneficiary).

% Those who believe the commandment is truly suspended until the Messianic era and the rebuilding of the Temple, viewing study as preparation for future performance rather than present fulfillment. While they value study, they do not see it as fully occupying the commandment's status.
narrative_ontology:constraint_stakeholder(kodashim_commandment_status__study_as_performance, messianic_deferral_adherents, excluded,
    moderate, generational, constrained, global).

% A minority who hold that the commandment for sacrifices is entirely contingent on the physical existence of the Temple and altar, and thus is currently in a state of complete suspension. They would argue that study, while meritorious, cannot substitute for actual performance.
narrative_ontology:constraint_stakeholder(kodashim_commandment_status__study_as_performance, performance_only_adherents, excluded,
    powerless, biographical, constrained, global).

% Scholars who analyze the historical, theological, and sociological development of this interpretive tradition within Judaism, examining its implications for religious practice and identity.
narrative_ontology:constraint_stakeholder(kodashim_commandment_status__study_as_performance, analytical_theologians, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the ongoing fulfillment of divine commandments related to Temple sacrifices in the absence of the physical Temple, providing a continuous path for adherents to engage with and adhere to a central body of Halakhic law.
% TRANSFER_FUNCTION: Transfers the primary locus of commandment fulfillment from physical ritual and animal sacrifice to intellectual engagement and Torah study, from the Temple to the study hall.
% ABSENT_VOICES: Adherents of the 'performance_only' reading would object, arguing that study, while valuable, cannot truly substitute for actual sacrifice and that the commandment is genuinely suspended without the Temple. Their voices are marginalized by the dominant interpretive tradition.
% DISAPPEARANCE_RATIONALE: If this reading vanished, a core mechanism for maintaining the vitality and relevance of a significant portion of Halakha (laws of sacrifices) would disappear. This would lead to a profound crisis of religious practice, meaning, and identity for many adherents, as a central divine command would be perceived as unfulfillable.
% FOUNDING_PROBLEM: How to maintain the divine commandment regarding Temple sacrifices and their associated laws (Kodashim) after the destruction of the Second Temple, preventing their obsolescence and ensuring continuous adherence to divine will and the continuity of the Halakhic tradition.
% FOUNDING_PROBLEM_CORROBORATION: The ongoing existence of numerous yeshivot (academies) dedicated to studying Kodashim, the continued inclusion of these laws in daily prayer, and the theological writings of countless rabbinic authorities across centuries, all attest to the problem's live status and the centrality of this interpretive solution within mainstream rabbinic Judaism.
narrative_ontology:disappearance_verdict(kodashim_commandment_status__study_as_performance, world_rearranges).
narrative_ontology:founding_problem_status(kodashim_commandment_status__study_as_performance, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(kodashim_commandment_status__study_as_performance, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(kodashim_commandment_status__study_as_performance, 'none', 1).
narrative_ontology:epsilon_provenance(kodashim_commandment_status__study_as_performance, 0.05, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(kodashim_commandment_status__study_as_performance_tests).
:- end_tests(kodashim_commandment_status__study_as_performance_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The constraint exhibits very low extractiveness, suppression, and theater because it provides an accessible and widely accepted means of religious fulfillment, rather than imposing costs or coercing behavior. The 'study as performance' doctrine is a foundational principle in rabbinic Judaism, making it a highly stable and beneficial 'rope' for its adherents. Its persistence is due to its genuine coordination function in maintaining religious practice and identity.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of adherents and scholars, this constraint is a vital 'rope' that enables religious continuity. From the perspective of 'performance_only' adherents, it might be seen as a conceptual 'snare' that obscures the true, suspended nature of the commandment, preventing focus on its literal restoration. However, this story focuses on the dominant 'study_as_performance' reading.
 *
 * DIRECTIONALITY LOGIC:
 *   Adherents of Halakha are clear beneficiaries, as this reading provides a path to fulfilling a central religious obligation. Rabbinic scholars, as the custodians and perpetuators of this interpretive tradition, also benefit significantly, as their intellectual work is elevated to a form of divine service. There are no direct 'victims' of this reading, as it offers a solution rather than imposing a burden.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identification,
    'Is this constraint accurately identified as the ''study_as_performance'' reading of the ''kodashim_commandment_status'' kernel?',
    'Analysis of primary rabbinic texts and theological discourse to confirm the distinct interpretive stance and its relationship to other readings of the commandment''s status.',
    'If misidentified, the classification and network relationships would be inaccurate, leading to incorrect analysis of the commitment system''s internal dynamics.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identification, conceptual, 'Confirms the specific kernel and reading being analyzed.').

omega_variable(
    equivalence_of_study_and_performance,
    'To what extent is intellectual engagement truly equivalent to physical performance in fulfilling the divine commandment, or is it a substitute?',
    'Theological and philosophical analysis of the concept of ''fulfillment'' within Halakha, examining whether the intent (kavanah) of study fully replaces the physical act (ma''aseh).',
    'If study is deemed a mere substitute rather than full equivalence, the ''extractiveness'' might be slightly higher (representing a ''cost'' of non-ideal fulfillment), and the ''claimed_type'' might shift towards a ''tangled_rope'' if some perceive a hidden cost in this substitution.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(equivalence_of_study_and_performance, conceptual, 'Ambiguity regarding the nature of fulfillment through study versus physical performance.').

omega_variable(
    suppression_of_alternative_actions,
    'Does this reading, by providing a ''solution'' to the commandment''s fulfillment, inadvertently suppress or de-emphasize efforts towards the physical rebuilding of the Temple or other forms of direct action?',
    'Sociological and historical analysis of community priorities and resource allocation within rabbinic Judaism, examining whether the intellectual focus diverts attention or resources from more ''active'' messianic endeavors.',
    'If significant suppression of alternative actions is identified, the ''suppression'' metric would be higher, and the constraint might lean towards a ''tangled_rope'' or even ''snare'' for those who prioritize physical restoration.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_of_alternative_actions, empirical, 'Potential for the ''study as performance'' doctrine to suppress alternative forms of religious action or focus.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(kodashim_commandment_status__study_as_performance, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(koda_tr_t0, kodashim_commandment_status__study_as_performance, theater_ratio, 0, 0.1).
narrative_ontology:measurement(koda_tr_t25, kodashim_commandment_status__study_as_performance, theater_ratio, 25, 0.1).
narrative_ontology:measurement(koda_tr_t50, kodashim_commandment_status__study_as_performance, theater_ratio, 50, 0.1).
narrative_ontology:measurement(koda_tr_t75, kodashim_commandment_status__study_as_performance, theater_ratio, 75, 0.1).
narrative_ontology:measurement(koda_tr_t100, kodashim_commandment_status__study_as_performance, theater_ratio, 100, 0.1).

% Extraction over time
narrative_ontology:measurement(koda_be_t0, kodashim_commandment_status__study_as_performance, base_extractiveness, 0, 0.05).
narrative_ontology:measurement(koda_be_t25, kodashim_commandment_status__study_as_performance, base_extractiveness, 25, 0.05).
narrative_ontology:measurement(koda_be_t50, kodashim_commandment_status__study_as_performance, base_extractiveness, 50, 0.05).
narrative_ontology:measurement(koda_be_t75, kodashim_commandment_status__study_as_performance, base_extractiveness, 75, 0.05).
narrative_ontology:measurement(koda_be_t100, kodashim_commandment_status__study_as_performance, base_extractiveness, 100, 0.05).

% Suppression requirement over time
narrative_ontology:measurement(koda_su_t0, kodashim_commandment_status__study_as_performance, suppression_requirement, 0, 0.1).
narrative_ontology:measurement(koda_su_t25, kodashim_commandment_status__study_as_performance, suppression_requirement, 25, 0.1).
narrative_ontology:measurement(koda_su_t50, kodashim_commandment_status__study_as_performance, suppression_requirement, 50, 0.1).
narrative_ontology:measurement(koda_su_t75, kodashim_commandment_status__study_as_performance, suppression_requirement, 75, 0.1).
narrative_ontology:measurement(koda_su_t100, kodashim_commandment_status__study_as_performance, suppression_requirement, 100, 0.1).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(kodashim_commandment_status__study_as_performance, identity_coordination).
narrative_ontology:affects_constraint(kodashim_commandment_status__study_as_performance, kodashim_commandment_status__messianic_deferral).
narrative_ontology:affects_constraint(kodashim_commandment_status__study_as_performance, kodashim_commandment_status__performance_only).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'kodashim_commandment_status' kernel, each representing a distinct interpretive stance on the fulfillment of Temple sacrifice laws after the Temple's destruction. Each reading has a different ε value and structural implications.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
