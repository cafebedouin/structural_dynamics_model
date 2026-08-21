% ============================================================================
% CONSTRAINT STORY: speech_protection_kernel__dignity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_speech_protection_kernel__dignity_reading, []).

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
 *   constraint_id: speech_protection_kernel__dignity_reading
 *   human_readable: Speech Protection Conditional on Equal Dignity (Dignity Reading)
 *   domain: constitutional_law/political_philosophy/communication_rights
 *
 * SUMMARY:
 *   This constraint represents the 'dignity reading' of the speech protection
 *   kernel, where the protection afforded to speech is conditional on it not
 *   functioning as structural subordination of target groups. It recognizes
 *   group harm as distinct from individual harm and seeks to ensure equal
 *   dignity. The constraint is classified as a Tangled Rope because it
 *   genuinely coordinates the right to speak with the right to dignity, but
 *   it also extracts from speakers of subordinating speech through active
 *   enforcement and suppression of certain forms of expression.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(speech_protection_kernel__dignity_reading, 0.65).
domain_priors:suppression_score(speech_protection_kernel__dignity_reading, 0.7).
domain_priors:theater_ratio(speech_protection_kernel__dignity_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(speech_protection_kernel__dignity_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(speech_protection_kernel__dignity_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(speech_protection_kernel__dignity_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(speech_protection_kernel__dignity_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(speech_protection_kernel__dignity_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(speech_protection_kernel__dignity_reading, tangled_rope).
narrative_ontology:human_readable(speech_protection_kernel__dignity_reading, "Speech Protection Conditional on Equal Dignity (Dignity Reading)").
narrative_ontology:topic_domain(speech_protection_kernel__dignity_reading, "constitutional_law/political_philosophy/communication_rights").

domain_priors:requires_active_enforcement(speech_protection_kernel__dignity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(speech_protection_kernel__dignity_reading, 'badb734a-3474-4087-8342-730e46cc8061').
narrative_ontology:cs_kernel_codification('badb734a-3474-4087-8342-730e46cc8061', formalized).
narrative_ontology:cs_authority_grounding('badb734a-3474-4087-8342-730e46cc8061', lineage).
narrative_ontology:cs_interpretation_layer_present('badb734a-3474-4087-8342-730e46cc8061').
narrative_ontology:cs_reading_relation('badb734a-3474-4087-8342-730e46cc8061', speech_protection_kernel__absolutist_reading, forecloses).
narrative_ontology:cs_reading_relation('badb734a-3474-4087-8342-730e46cc8061', speech_protection_kernel__harm_threshold_reading, influences).
narrative_ontology:cs_reading_relation('badb734a-3474-4087-8342-730e46cc8061', speech_protection_kernel__marketplace_reading, coexists_with).
narrative_ontology:cs_reading_relation('badb734a-3474-4087-8342-730e46cc8061', speech_protection_kernel__democratic_participation_reading, coexists_with).
narrative_ontology:cs_axiom('badb734a-3474-4087-8342-730e46cc8061', foundational, equal_dignity_as_precondition_for_speech).
narrative_ontology:cs_axiom_status(equal_dignity_as_precondition_for_speech, holdable).
narrative_ontology:cs_axiom_grounding('badb734a-3474-4087-8342-730e46cc8061', equal_dignity_as_precondition_for_speech, deontological).
narrative_ontology:cs_axiom('badb734a-3474-4087-8342-730e46cc8061', foundational, group_harm_is_structural_and_distinct).
narrative_ontology:cs_axiom_status(group_harm_is_structural_and_distinct, holdable).
narrative_ontology:cs_axiom_grounding('badb734a-3474-4087-8342-730e46cc8061', group_harm_is_structural_and_distinct, empirically_contingent).
narrative_ontology:cs_reference_frame('badb734a-3474-4087-8342-730e46cc8061', post_wwii_human_rights_framework).
narrative_ontology:cs_drift_state('badb734a-3474-4087-8342-730e46cc8061', contemporary_digital_disinformation_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('badb734a-3474-4087-8342-730e46cc8061', '').
narrative_ontology:cs_kernel_id(speech_protection_kernel__dignity_reading, speech_protection_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(speech_protection_kernel__dignity_reading, marginalized_groups).
narrative_ontology:constraint_beneficiary(speech_protection_kernel__dignity_reading, social_cohesion).
narrative_ontology:constraint_victim(speech_protection_kernel__dignity_reading, speakers_of_subordinating_speech).
narrative_ontology:constraint_victim(speech_protection_kernel__dignity_reading, absolutist_free_speech_advocates).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefit from the recognition that speech can cause structural harm and that their equal dignity is a precondition for free expression. They advocate for restrictions on hate speech and other forms of subordinating expression. Their exit options are limited by their social position.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__dignity_reading, marginalized_groups, beneficiary,
    organized, generational, constrained, national).

% Bear the cost of having their speech restricted or unprotected when it is deemed to contribute to structural subordination. They may perceive this as an infringement on their individual liberty to express any viewpoint. Their exit is to modify their speech or face legal/social consequences.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__dignity_reading, speakers_of_subordinating_speech, payer,
    moderate, immediate, constrained, local).

% Are tasked with defining what constitutes structural subordination and balancing speech protection with the need to maintain equal dignity. They enforce restrictions on speech that crosses the line, often through legal precedent and administrative rulings. Their role is to interpret and apply the dignity principle.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__dignity_reading, courts_and_regulators, agenda_setter,
    institutional, generational, analytical, national).

% Are ideologically committed to a near-absolute protection of speech, regardless of content or impact, viewing any restriction as a dangerous precedent. They are excluded from the core premise of this reading, which conditions speech on dignity. Their identity is locked into the absolutist framework.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__dignity_reading, absolutist_free_speech_advocates, excluded,
    powerful, generational, identity_locked, national).

% Benefits from a legal framework that seeks to prevent speech from eroding the social fabric by structurally subordinating groups. This is an abstract good, not an active agent, but it is a declared beneficiary of the constraint's operation.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__dignity_reading, social_cohesion, beneficiary,
    analytical, civilizational, analytical, national).
narrative_ontology:stakeholder_non_agent(speech_protection_kernel__dignity_reading, social_cohesion).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the exercise of free speech with the fundamental right to equal dignity and non-subordination, aiming to create a public sphere where all can participate without fear of structural harm.
% TRANSFER_FUNCTION: Transfers the burden of potential harm from marginalized groups to speakers whose expression contributes to structural subordination, by conditioning speech protection on its non-subordinating function.
% ABSENT_VOICES: Those who believe that any content-based restriction on speech is inherently illegitimate, regardless of its impact on dignity or equality, are structurally excluded from the foundational premises of this reading. They would argue for a 'more speech' solution to harmful expression.
% DISAPPEARANCE_RATIONALE: If this reading vanished, the legal landscape for hate speech and group libel would revert to a more permissive standard, potentially increasing the vulnerability of marginalized groups to subordinating expression and eroding social cohesion. Courts would lose a key interpretive tool for balancing rights.
% FOUNDING_PROBLEM: The problem of speech being used to perpetuate and reinforce structural inequalities, leading to the marginalization and dehumanization of certain groups, undermining their ability to participate equally in society.
% FOUNDING_PROBLEM_CORROBORATION: Legal scholars, civil rights organizations, and international human rights bodies consistently attest to the ongoing problem of subordinating speech and its impact on vulnerable populations. Their analyses, independent of the courts, corroborate the live status of this problem.
narrative_ontology:disappearance_verdict(speech_protection_kernel__dignity_reading, world_rearranges).
narrative_ontology:founding_problem_status(speech_protection_kernel__dignity_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(speech_protection_kernel__dignity_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(speech_protection_kernel__dignity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(speech_protection_kernel__dignity_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(speech_protection_kernel__dignity_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(speech_protection_kernel__dignity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(speech_protection_kernel__dignity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.65) because it imposes significant costs on speakers whose expression is deemed subordinating, limiting their ability to express certain viewpoints. Suppression is also high (0.70) as it requires active legal and social enforcement to identify and restrict such speech. The theater ratio is low (0.20) because the constraint's function is genuinely pursued, not merely performed; the enforcement is aimed at achieving its stated goal of protecting dignity. Accessibility collapse is moderate (0.40) as alternatives for expression exist, but not for subordinating speech without consequence. Resistance is moderate (0.55) from those who advocate for broader speech protections.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of marginalized groups, this constraint is a necessary coordination mechanism to ensure their equal participation and dignity. From the perspective of speakers of subordinating speech, it is an extractive snare that curtails their freedom of expression. The engine's classification will reflect this divergence based on the structural positions.
 *
 * DIRECTIONALITY LOGIC:
 *   Marginalized groups and social cohesion are beneficiaries (d near 0.0) as the constraint aims to protect their dignity and foster an inclusive public sphere. Speakers of subordinating speech are targets (d near 1.0) as their expression is directly curtailed. Courts and regulators act as agenda-setters, interpreting and enforcing the constraint. Absolutist free speech advocates are excluded, as their core premise is incompatible with this reading.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    definition_of_structural_subordination,
    'How is ''structural subordination'' precisely defined and consistently applied across diverse contexts and forms of speech?',
    'Development of clear, judicially consistent, and empirically grounded criteria for identifying structural subordination, potentially through a series of landmark cases or legislative guidance.',
    'Lack of clarity could lead to arbitrary enforcement, increasing extractiveness and suppression for speakers, potentially shifting the classification towards a Snare. Clear definitions would strengthen its legitimacy as a Tangled Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(definition_of_structural_subordination, conceptual, 'Ambiguity in the core concept of structural subordination.').

omega_variable(
    balancing_individual_vs_group_rights,
    'How is the individual right to free expression balanced against the collective right to equal dignity and non-subordination, particularly when these rights appear to conflict?',
    'Philosophical and legal consensus on a hierarchy or framework for balancing these rights, or empirical studies demonstrating the actual impact of different balancing approaches on both individual expression and group dignity.',
    'If the balance consistently favors group dignity, individual speakers may experience higher extraction. If it consistently favors individual expression, marginalized groups may experience less protection. This could shift the effective extractiveness and suppression.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(balancing_individual_vs_group_rights, preference, 'The inherent tension between individual speech rights and group dignity rights.').

omega_variable(
    enforcement_burden_on_marginalized_groups,
    'Does the enforcement mechanism for this constraint disproportionately burden marginalized groups, requiring them to constantly litigate or advocate for their dignity?',
    'Empirical analysis of legal aid access, litigation rates, and the psychological burden on marginalized communities in enforcing anti-subordination speech laws. Policy changes to shift the enforcement burden to state actors.',
    'If enforcement disproportionately burdens marginalized groups, their effective beneficiary status is reduced, and the constraint''s overall extractiveness (from them) increases, potentially making it a Tangled Rope even for its intended beneficiaries.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(enforcement_burden_on_marginalized_groups, empirical, 'Whether the enforcement of dignity-based speech restrictions creates an additional burden on marginalized groups.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(speech_protection_kernel__dignity_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(spee_tr_t0, speech_protection_kernel__dignity_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(spee_tr_t5, speech_protection_kernel__dignity_reading, theater_ratio, 5, 0.2).
narrative_ontology:measurement(spee_tr_t10, speech_protection_kernel__dignity_reading, theater_ratio, 10, 0.2).
narrative_ontology:measurement(spee_tr_t15, speech_protection_kernel__dignity_reading, theater_ratio, 15, 0.2).
narrative_ontology:measurement(spee_tr_t20, speech_protection_kernel__dignity_reading, theater_ratio, 20, 0.2).

% Extraction over time
narrative_ontology:measurement(spee_be_t0, speech_protection_kernel__dignity_reading, base_extractiveness, 0, 0.6).
narrative_ontology:measurement(spee_be_t5, speech_protection_kernel__dignity_reading, base_extractiveness, 5, 0.62).
narrative_ontology:measurement(spee_be_t10, speech_protection_kernel__dignity_reading, base_extractiveness, 10, 0.64).
narrative_ontology:measurement(spee_be_t15, speech_protection_kernel__dignity_reading, base_extractiveness, 15, 0.65).
narrative_ontology:measurement(spee_be_t20, speech_protection_kernel__dignity_reading, base_extractiveness, 20, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(spee_su_t0, speech_protection_kernel__dignity_reading, suppression_requirement, 0, 0.65).
narrative_ontology:measurement(spee_su_t5, speech_protection_kernel__dignity_reading, suppression_requirement, 5, 0.67).
narrative_ontology:measurement(spee_su_t10, speech_protection_kernel__dignity_reading, suppression_requirement, 10, 0.69).
narrative_ontology:measurement(spee_su_t15, speech_protection_kernel__dignity_reading, suppression_requirement, 15, 0.7).
narrative_ontology:measurement(spee_su_t20, speech_protection_kernel__dignity_reading, suppression_requirement, 20, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(speech_protection_kernel__dignity_reading, identity_coordination).
narrative_ontology:affects_constraint(speech_protection_kernel__dignity_reading, hate_speech_legislation).
narrative_ontology:affects_constraint(speech_protection_kernel__dignity_reading, online_content_moderation_policies).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'speech_protection_kernel'. Its structural recognition of group harm and conditional protection influences related legislation and policy, while coexisting with other interpretive frameworks of free speech.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
