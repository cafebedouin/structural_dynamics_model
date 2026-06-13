% ============================================================================
% CONSTRAINT STORY: speech_protection_boundary__harm_limited_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_speech_protection_boundary__harm_limited_reading, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: speech_protection_boundary__harm_limited_reading
 *   human_readable: Speech Protection Boundary (Harm-Limited Reading)
 *   domain: constitutional_law/political_philosophy/speech_regulation
 *
 * SUMMARY:
 *   This constraint represents the 'harm-limited' reading of speech
 *   protection, where freedom of expression is conditional on the absence of
 *   significant harm to dignity, equality, and freedom from harassment. This
 *   reading narrows the scope of protected speech compared to absolutist or
 *   pure balancing approaches, explicitly excluding categories like hate
 *   speech and harassment. It empowers state regulators to act as
 *   gatekeepers, with the attendant risk of abuse, but aims to create a more
 *   inclusive public sphere for vulnerable groups.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(speech_protection_boundary__harm_limited_reading, 0.65).
domain_priors:suppression_score(speech_protection_boundary__harm_limited_reading, 0.7).
domain_priors:theater_ratio(speech_protection_boundary__harm_limited_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(speech_protection_boundary__harm_limited_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(speech_protection_boundary__harm_limited_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(speech_protection_boundary__harm_limited_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(speech_protection_boundary__harm_limited_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(speech_protection_boundary__harm_limited_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(speech_protection_boundary__harm_limited_reading, tangled_rope).
narrative_ontology:human_readable(speech_protection_boundary__harm_limited_reading, "Speech Protection Boundary (Harm-Limited Reading)").
narrative_ontology:topic_domain(speech_protection_boundary__harm_limited_reading, "constitutional_law/political_philosophy/speech_regulation").

domain_priors:requires_active_enforcement(speech_protection_boundary__harm_limited_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(speech_protection_boundary__harm_limited_reading, '53e2217f-c9b3-4558-a443-8c61c219a5e0').
narrative_ontology:cs_kernel_codification('53e2217f-c9b3-4558-a443-8c61c219a5e0', formalized).
narrative_ontology:cs_authority_grounding('53e2217f-c9b3-4558-a443-8c61c219a5e0', lineage).
narrative_ontology:cs_interpretation_layer_present('53e2217f-c9b3-4558-a443-8c61c219a5e0').
narrative_ontology:cs_reading_relation('53e2217f-c9b3-4558-a443-8c61c219a5e0', speech_protection_boundary__absolutist_reading, forecloses).
narrative_ontology:cs_reading_relation('53e2217f-c9b3-4558-a443-8c61c219a5e0', speech_protection_boundary__balancing_reading, influences).
narrative_ontology:cs_axiom('53e2217f-c9b3-4558-a443-8c61c219a5e0', foundational, speech_causing_dignitary_harm_is_not_speech).
narrative_ontology:cs_axiom_status(speech_causing_dignitary_harm_is_not_speech, holdable).
narrative_ontology:cs_axiom_grounding('53e2217f-c9b3-4558-a443-8c61c219a5e0', speech_causing_dignitary_harm_is_not_speech, deontological).
narrative_ontology:cs_axiom('53e2217f-c9b3-4558-a443-8c61c219a5e0', foundational, equality_and_dignity_are_preconditions_for_free_speech).
narrative_ontology:cs_axiom_status(equality_and_dignity_are_preconditions_for_free_speech, holdable).
narrative_ontology:cs_axiom_grounding('53e2217f-c9b3-4558-a443-8c61c219a5e0', equality_and_dignity_are_preconditions_for_free_speech, deontological).
narrative_ontology:cs_reference_frame('53e2217f-c9b3-4558-a443-8c61c219a5e0', post_wwii_human_rights_framework).
narrative_ontology:cs_drift_state('53e2217f-c9b3-4558-a443-8c61c219a5e0', contemporary_digital_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('53e2217f-c9b3-4558-a443-8c61c219a5e0', '').
narrative_ontology:cs_kernel_id(speech_protection_boundary__harm_limited_reading, speech_protection_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(speech_protection_boundary__harm_limited_reading, vulnerable_groups).
narrative_ontology:constraint_beneficiary(speech_protection_boundary__harm_limited_reading, state_regulators).
narrative_ontology:constraint_victim(speech_protection_boundary__harm_limited_reading, speakers_of_controversial_views).
narrative_ontology:constraint_victim(speech_protection_boundary__harm_limited_reading, political_dissidents).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefit from legal protections against hate speech, harassment, and discrimination, which are deemed to cause significant harm to their dignity and equality. They advocate for robust enforcement of these limits to ensure their safety and full participation in society.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__harm_limited_reading, vulnerable_groups, beneficiary,
    organized, generational, constrained, national).

% Are tasked with defining and enforcing the boundaries of protected speech, balancing expressive freedom with the prevention of harm. They gain authority and legitimacy by protecting vulnerable groups, but also face pressure from those whose speech is restricted.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__harm_limited_reading, state_regulators, agenda_setter,
    institutional, generational, constrained, national).

% Bear the cost of having their speech restricted or chilled, particularly when their views are deemed to cause harm to dignity or equality. They may face legal penalties, social ostracization, or self-censorship, even if their intent was not malicious.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__harm_limited_reading, speakers_of_controversial_views, payer,
    moderate, biographical, constrained, national).

% Are particularly vulnerable to speech restrictions, as their challenges to the status quo may be interpreted as causing harm to established norms or groups. Their ability to organize and express dissent is curtailed, making their participation in political discourse more difficult.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__harm_limited_reading, political_dissidents, payer,
    powerless, biographical, identity_locked, national).

% Argue for a near-absolute protection of speech, with very narrow exceptions, believing that any state-imposed harm limitation inevitably leads to censorship and thought control. Their arguments are often dismissed or marginalized within the harm-limited framework.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__harm_limited_reading, absolutist_advocates, excluded,
    organized, generational, analytical, national).

% Analyze the theoretical underpinnings and practical implications of the harm-limited reading, debating its consistency with democratic values, its potential for abuse, and its effectiveness in achieving its stated goals. They do not directly participate in enforcement but influence its evolution through critique.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__harm_limited_reading, legal_scholars, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Aims to coordinate social interaction by establishing clear boundaries for acceptable public discourse, ensuring that all individuals can participate in society without facing speech-based harassment or degradation.
% TRANSFER_FUNCTION: Transfers the burden of tolerating harmful speech from vulnerable groups to speakers whose expression is deemed to cause significant harm, while simultaneously transferring interpretive authority over speech boundaries to state regulators.
% ABSENT_VOICES: Advocates for an absolutist view of free speech are structurally excluded from the core debate within this framework, as their foundational premise (minimal harm exceptions) is rejected. They would argue that the framework itself is a form of suppression.
% DISAPPEARANCE_RATIONALE: If this harm-limited reading vanished, the legal landscape for speech would revert to a more permissive standard, potentially leading to an increase in hate speech and harassment, and forcing vulnerable groups to seek protection through other, less direct, legal avenues. State regulators would lose a significant tool for social management.
% FOUNDING_PROBLEM: The problem of hate speech, harassment, and incitement to discrimination causing tangible harm to the dignity, equality, and safety of marginalized groups, which a purely absolutist speech doctrine failed to adequately address.
% FOUNDING_PROBLEM_CORROBORATION: Vulnerable groups and human rights organizations consistently attest to the ongoing problem of speech-related harm. International human rights law and comparative constitutional frameworks also corroborate the necessity of such limits, providing external validation beyond the direct beneficiaries.
narrative_ontology:disappearance_verdict(speech_protection_boundary__harm_limited_reading, world_rearranges).
narrative_ontology:founding_problem_status(speech_protection_boundary__harm_limited_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(speech_protection_boundary__harm_limited_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(speech_protection_boundary__harm_limited_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(speech_protection_boundary__harm_limited_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(speech_protection_boundary__harm_limited_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(speech_protection_boundary__harm_limited_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.65) is substantial because it imposes significant costs on speakers whose expression is deemed harmful, potentially chilling legitimate but controversial speech. Suppression (0.70) is high due to the active enforcement required to define and police speech boundaries, often involving legal penalties and social pressure. The theater ratio (0.20) is relatively low, as the stated goal of preventing harm is genuinely pursued, though the implementation may involve performative aspects to signal commitment to certain values. The rising extractiveness and suppression over time reflect the increasing societal focus on identity-based harms and the expansion of categories of unprotected speech.
 *
 * PERSPECTIVAL GAP:
 *   Vulnerable groups and state regulators experience this as a beneficial coordination mechanism that protects fundamental rights. Speakers of controversial views and political dissidents, however, experience it as a highly extractive and suppressive constraint that curtails their expressive freedom and ability to challenge power structures. The engine will compute these divergent classifications from the declared structural relationships.
 *
 * DIRECTIONALITY LOGIC:
 *   Vulnerable groups are primary beneficiaries (d=0.0-0.1) as the constraint directly protects them from harm. State regulators are also beneficiaries (d=0.1-0.2) as they gain authority and legitimacy by enforcing these protections. Speakers of controversial views and political dissidents are targets (d=0.8-1.0) as their speech is directly curtailed and they bear the costs of enforcement. Absolutist advocates are excluded, meaning their perspective is outside the framework's internal logic.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is not experiencing mandatrophy; its mandate (preventing harm to dignity and equality) is actively contested as 'live' by its beneficiaries and corroborated by external sources. The classification as Tangled Rope reflects the genuine coordination function (protecting vulnerable groups) intertwined with asymmetric extraction (from speakers whose views are restricted) and active enforcement, preventing it from being mislabeled as a pure Snare or a benign Rope.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    definition_of_harm,
    'How is ''significant harm to dignity, equality, and freedom from harassment'' precisely defined and consistently applied across diverse contexts and evolving social norms?',
    'Development of clear, judicially consistent, and empirically grounded definitions of harm, coupled with robust procedural safeguards against arbitrary application.',
    'If definitions are vague or inconsistently applied, the constraint''s suppression becomes more arbitrary and extractive, potentially chilling a wider range of speech than intended. If clear and consistent, it strengthens the coordination function and reduces unintended extraction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(definition_of_harm, conceptual, 'Ambiguity in the definition and application of ''harm'' in speech regulation.').

omega_variable(
    state_gatekeeper_abuse_risk,
    'To what extent does empowering the state as a gatekeeper of speech lead to politically motivated censorship or the suppression of legitimate dissent, rather than solely preventing harm?',
    'Longitudinal studies of enforcement patterns, analysis of judicial review outcomes, and comparative studies of jurisdictions with similar harm-limited frameworks, focusing on cases involving political speech or criticism of authorities.',
    'If abuse is widespread, the constraint functions more as a Snare, using harm prevention as a cover for political suppression. If abuse is rare and effectively checked, it reinforces the Tangled Rope classification, acknowledging the inherent tension but affirming the primary coordination goal.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(state_gatekeeper_abuse_risk, empirical, 'Risk of state abuse of power in enforcing harm-limited speech protections.').

omega_variable(
    kernel_reading_identification,
    'Is this constraint a genuine ''harm-limited'' reading of speech protection, or is it a ''balancing'' reading that merely prioritizes harm prevention?',
    'Analysis of foundational legal texts and judicial precedents: a harm-limited reading establishes harm as a categorical boundary, while a balancing reading weighs harm against speech interests on a case-by-case basis. The distinction lies in the structural role of ''harm''.',
    'If it''s a balancing reading, the extractiveness and suppression might be lower, as speech is not categorically unprotected. If it''s a true harm-limited reading, the categorical exclusion of certain speech types justifies the higher extractiveness and suppression for those categories.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identification, conceptual, 'Distinguishing harm-limited from balancing readings of speech protection.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(speech_protection_boundary__harm_limited_reading, 1980, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(spee_tr_t1980, speech_protection_boundary__harm_limited_reading, theater_ratio, 1980, 0.1).
narrative_ontology:measurement(spee_tr_t1990, speech_protection_boundary__harm_limited_reading, theater_ratio, 1990, 0.12).
narrative_ontology:measurement(spee_tr_t2000, speech_protection_boundary__harm_limited_reading, theater_ratio, 2000, 0.15).
narrative_ontology:measurement(spee_tr_t2010, speech_protection_boundary__harm_limited_reading, theater_ratio, 2010, 0.18).
narrative_ontology:measurement(spee_tr_t2020, speech_protection_boundary__harm_limited_reading, theater_ratio, 2020, 0.19).
narrative_ontology:measurement(spee_tr_t2024, speech_protection_boundary__harm_limited_reading, theater_ratio, 2024, 0.2).

% Extraction over time
narrative_ontology:measurement(spee_be_t1980, speech_protection_boundary__harm_limited_reading, base_extractiveness, 1980, 0.4).
narrative_ontology:measurement(spee_be_t1990, speech_protection_boundary__harm_limited_reading, base_extractiveness, 1990, 0.5).
narrative_ontology:measurement(spee_be_t2000, speech_protection_boundary__harm_limited_reading, base_extractiveness, 2000, 0.58).
narrative_ontology:measurement(spee_be_t2010, speech_protection_boundary__harm_limited_reading, base_extractiveness, 2010, 0.62).
narrative_ontology:measurement(spee_be_t2020, speech_protection_boundary__harm_limited_reading, base_extractiveness, 2020, 0.64).
narrative_ontology:measurement(spee_be_t2024, speech_protection_boundary__harm_limited_reading, base_extractiveness, 2024, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(spee_su_t1980, speech_protection_boundary__harm_limited_reading, suppression_requirement, 1980, 0.5).
narrative_ontology:measurement(spee_su_t1990, speech_protection_boundary__harm_limited_reading, suppression_requirement, 1990, 0.58).
narrative_ontology:measurement(spee_su_t2000, speech_protection_boundary__harm_limited_reading, suppression_requirement, 2000, 0.65).
narrative_ontology:measurement(spee_su_t2010, speech_protection_boundary__harm_limited_reading, suppression_requirement, 2010, 0.68).
narrative_ontology:measurement(spee_su_t2020, speech_protection_boundary__harm_limited_reading, suppression_requirement, 2020, 0.69).
narrative_ontology:measurement(spee_su_t2024, speech_protection_boundary__harm_limited_reading, suppression_requirement, 2024, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(speech_protection_boundary__harm_limited_reading, enforcement_mechanism).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'speech_protection_boundary' kernel. Other readings include 'absolutist_reading' and 'balancing_reading', which define the boundaries of protected speech differently based on their foundational premises regarding harm and expressive freedom.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
