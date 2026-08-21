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
 *   constraint_id: speech_protection_kernel__dignity_reading
 *   human_readable: Speech Protection Conditional on Dignity Maintenance
 *   domain: constitutional_law/political_philosophy/communication_rights
 *
 * SUMMARY:
 *   This constraint represents the 'dignity reading' of speech protection,
 *   where the protection of speech is conditional on it not functioning as
 *   structural subordination of target groups. This reading recognizes group
 *   harm as distinct from individual harm and seeks to unprotected hate
 *   speech or group libel. It is a contested interpretation within
 *   constitutional law and political philosophy, actively enforced by courts
 *   and legislatures, and resisted by those who advocate for more expansive
 *   speech rights. The constraint is claimed as a Tangled Rope because it
 *   genuinely coordinates society around a principle of equal dignity but
 *   does so by extracting expressive freedom from those whose speech is
 *   deemed subordinating, requiring active enforcement.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(speech_protection_kernel__dignity_reading, 0.7).
domain_priors:suppression_score(speech_protection_kernel__dignity_reading, 0.8).
domain_priors:theater_ratio(speech_protection_kernel__dignity_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(speech_protection_kernel__dignity_reading, extractiveness, 0.7).
narrative_ontology:constraint_metric(speech_protection_kernel__dignity_reading, suppression_requirement, 0.8).
narrative_ontology:constraint_metric(speech_protection_kernel__dignity_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(speech_protection_kernel__dignity_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(speech_protection_kernel__dignity_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(speech_protection_kernel__dignity_reading, tangled_rope).
narrative_ontology:human_readable(speech_protection_kernel__dignity_reading, "Speech Protection Conditional on Dignity Maintenance").
narrative_ontology:topic_domain(speech_protection_kernel__dignity_reading, "constitutional_law/political_philosophy/communication_rights").

domain_priors:requires_active_enforcement(speech_protection_kernel__dignity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(speech_protection_kernel__dignity_reading, '998dbf7b-5816-4be2-9a8d-8e8e3b98997b').
narrative_ontology:cs_kernel_codification('998dbf7b-5816-4be2-9a8d-8e8e3b98997b', fixed_text).
narrative_ontology:cs_authority_grounding('998dbf7b-5816-4be2-9a8d-8e8e3b98997b', lineage).
narrative_ontology:cs_interpretation_layer_present('998dbf7b-5816-4be2-9a8d-8e8e3b98997b').
narrative_ontology:cs_reading_relation('998dbf7b-5816-4be2-9a8d-8e8e3b98997b', speech_protection_kernel__absolutist_reading, forecloses).
narrative_ontology:cs_reading_relation('998dbf7b-5816-4be2-9a8d-8e8e3b98997b', speech_protection_kernel__harm_threshold_reading, influences).
narrative_ontology:cs_reading_relation('998dbf7b-5816-4be2-9a8d-8e8e3b98997b', speech_protection_kernel__marketplace_reading, coexists_with).
narrative_ontology:cs_reading_relation('998dbf7b-5816-4be2-9a8d-8e8e3b98997b', speech_protection_kernel__democratic_participation_reading, coexists_with).
narrative_ontology:cs_axiom('998dbf7b-5816-4be2-9a8d-8e8e3b98997b', foundational, speech_must_not_structurally_subordinate).
narrative_ontology:cs_axiom_status(speech_must_not_structurally_subordinate, holdable).
narrative_ontology:cs_axiom_grounding('998dbf7b-5816-4be2-9a8d-8e8e3b98997b', speech_must_not_structurally_subordinate, deontological).
narrative_ontology:cs_axiom('998dbf7b-5816-4be2-9a8d-8e8e3b98997b', foundational, group_dignity_is_a_constitutional_value).
narrative_ontology:cs_axiom_status(group_dignity_is_a_constitutional_value, holdable).
narrative_ontology:cs_axiom_grounding('998dbf7b-5816-4be2-9a8d-8e8e3b98997b', group_dignity_is_a_constitutional_value, deontological).
narrative_ontology:cs_reference_frame('998dbf7b-5816-4be2-9a8d-8e8e3b98997b', equal_dignity_principle).
narrative_ontology:cs_drift_state('998dbf7b-5816-4be2-9a8d-8e8e3b98997b', contemporary_social_justice_discourse, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('998dbf7b-5816-4be2-9a8d-8e8e3b98997b', '').
narrative_ontology:cs_kernel_id(speech_protection_kernel__dignity_reading, speech_protection_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(speech_protection_kernel__dignity_reading, vulnerable_target_groups).
narrative_ontology:constraint_beneficiary(speech_protection_kernel__dignity_reading, society_as_a_whole).
narrative_ontology:constraint_victim(speech_protection_kernel__dignity_reading, speakers_of_subordinating_speech).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interpret and enforce the principle that speech protection is conditional on not functioning as structural subordination. They adjudicate cases, define the boundaries of protected speech, and legislate against hate speech or group libel.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__dignity_reading, courts_and_legislatures, agenda_setter,
    institutional, generational, constrained, national).

% Benefit from the protection against speech that structurally subordinates them, which helps maintain their equal dignity and participation in society. Their ability to exit the social structure is limited by their identity.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__dignity_reading, vulnerable_target_groups, beneficiary,
    powerless, generational, identity_locked, national).

% Benefits from a more inclusive and less hostile public discourse, fostering social cohesion and the equal standing of all citizens. The cost of exiting this arrangement would be a more fragmented and unequal society.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__dignity_reading, society_as_a_whole, beneficiary,
    organized, civilizational, constrained, national).

% Bear the cost of having their speech restricted or unprotected when it is deemed to function as structural subordination. Their options are to modify their speech, face legal consequences, or attempt to challenge the principle.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__dignity_reading, speakers_of_subordinating_speech, payer,
    moderate, immediate, constrained, local).

% Are structurally excluded from the core premise of this reading, as their view holds that listener harm is not grounds for restriction. They actively resist the principle but are not part of its internal interpretive framework.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__dignity_reading, absolutist_free_speech_advocates, excluded,
    powerful, generational, constrained, national).

% Analyze the application and implications of the dignity reading, contributing to its theoretical development and critique without directly enforcing or being subject to its restrictions.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__dignity_reading, legal_scholars, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates social interaction and public discourse around a principle of equal dignity, preventing speech from being used to structurally subordinate vulnerable groups and fostering an inclusive public sphere.
% TRANSFER_FUNCTION: Transfers expressive freedom from those whose speech functions as structural subordination to the dignity, safety, and equal standing of vulnerable target groups.
% ABSENT_VOICES: Absolutist free speech advocates are structurally excluded; they would argue that any content-based restriction on speech, even for dignity, is an unacceptable infringement on liberty and that the best response to harmful speech is more speech.
% DISAPPEARANCE_RATIONALE: If the principle of conditional speech protection for dignity vanished overnight, public discourse would likely become more hostile and exclusionary for vulnerable groups, potentially leading to increased social fragmentation, harm, and the need for new, potentially more coercive, mechanisms to address these issues.
% FOUNDING_PROBLEM: The historical and ongoing use of speech to perpetuate structural inequality, dehumanization, and marginalization of certain groups, undermining their equal participation and standing in society.
% FOUNDING_PROBLEM_CORROBORATION: Human rights organizations, sociological studies on the impact of hate speech, and testimony from affected communities consistently corroborate the ongoing nature and severity of this problem, providing external validation beyond the direct beneficiaries.
narrative_ontology:disappearance_verdict(speech_protection_kernel__dignity_reading, world_rearranges).
narrative_ontology:founding_problem_status(speech_protection_kernel__dignity_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(speech_protection_kernel__dignity_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(speech_protection_kernel__dignity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(speech_protection_kernel__dignity_reading, 0.7, 'gemini-2.5-flash', 'none', direct).

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
 *   The high extractiveness (0.7) and suppression (0.8) reflect the significant limitation placed on certain forms of speech, particularly those that contribute to structural subordination. The low theater ratio (0.2) indicates that the enforcement of this principle is generally genuine and effective, not merely performative. Resistance is high (0.7) due to ongoing philosophical and legal debates with absolutist free speech positions. Accessibility collapse is moderate (0.6) because while certain speech is restricted, a wide array of other expressive activities remains available.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of vulnerable target groups, this constraint is a vital protection (near-beneficiary). From the perspective of speakers whose speech is restricted, it is a significant imposition on their expressive freedom (near-target). The engine's computation of per-seat classifications will reflect this divergence based on the declared roles and positional atoms.
 *
 * DIRECTIONALITY LOGIC:
 *   Vulnerable target groups and society as a whole are the primary beneficiaries, as the constraint aims to protect their dignity and foster inclusive discourse. Speakers of subordinating speech are the clear targets, as their expressive freedom is curtailed. Courts and legislatures act as agenda-setters, interpreting and enforcing the principle. Absolutist free speech advocates are excluded from the framework's core premises, representing a persistent external challenge.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    definition_of_structural_subordination,
    'What constitutes ''structural subordination'' in practice, and how is it reliably distinguished from mere offense or criticism?',
    'Development of clear, judicially consistent, and empirically grounded criteria for identifying speech that contributes to systemic inequality, rather than isolated instances of harm.',
    'If the definition remains ambiguous, the constraint''s application could be perceived as arbitrary or overbroad, increasing resistance and potentially shifting its classification towards a Snare for speakers. If clear, it strengthens the Tangled Rope classification by clarifying the coordination function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(definition_of_structural_subordination, conceptual, 'Ambiguity in defining the core concept of ''structural subordination''.').

omega_variable(
    empirical_impact_of_restriction,
    'Does restricting subordinating speech genuinely reduce structural inequality and enhance the dignity of target groups, or does it merely drive such speech underground?',
    'Longitudinal sociological and psychological studies on the effects of hate speech regulation on social cohesion, intergroup relations, and the lived experiences of vulnerable groups.',
    'If restrictions prove ineffective or counterproductive, the justification for the constraint weakens, potentially leading to a re-evaluation of its extractiveness and a shift towards a Piton or Snare if the coordination function is not realized.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(empirical_impact_of_restriction, empirical, 'Empirical efficacy of speech restrictions in achieving dignity goals.').

omega_variable(
    dignity_reading_vs_absolutist_foreclosure,
    'Is the ''dignity reading'' truly capable of foreclosing the ''absolutist reading'' within a single coherent legal framework, or do they merely coexist as competing, irreconcilable positions?',
    'Analysis of judicial decisions and legislative acts that explicitly reject absolutist premises in favor of dignity-based limitations, demonstrating a shift in the foundational legal paradigm rather than mere policy preference.',
    'If foreclosure is not achieved, the ''dignity reading'' operates in a state of perpetual contestation, potentially increasing its suppression requirements and reducing its perceived legitimacy among a significant portion of the populace.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(dignity_reading_vs_absolutist_foreclosure, conceptual, 'The logical and practical relationship between the dignity and absolutist readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(speech_protection_kernel__dignity_reading, 1960, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(spee_tr_t1960, speech_protection_kernel__dignity_reading, theater_ratio, 1960, 0.1).
narrative_ontology:measurement(spee_tr_t1980, speech_protection_kernel__dignity_reading, theater_ratio, 1980, 0.15).
narrative_ontology:measurement(spee_tr_t2000, speech_protection_kernel__dignity_reading, theater_ratio, 2000, 0.18).
narrative_ontology:measurement(spee_tr_t2024, speech_protection_kernel__dignity_reading, theater_ratio, 2024, 0.2).

% Extraction over time
narrative_ontology:measurement(spee_be_t1960, speech_protection_kernel__dignity_reading, base_extractiveness, 1960, 0.4).
narrative_ontology:measurement(spee_be_t1980, speech_protection_kernel__dignity_reading, base_extractiveness, 1980, 0.55).
narrative_ontology:measurement(spee_be_t2000, speech_protection_kernel__dignity_reading, base_extractiveness, 2000, 0.65).
narrative_ontology:measurement(spee_be_t2024, speech_protection_kernel__dignity_reading, base_extractiveness, 2024, 0.7).

% Suppression requirement over time
narrative_ontology:measurement(spee_su_t1960, speech_protection_kernel__dignity_reading, suppression_requirement, 1960, 0.5).
narrative_ontology:measurement(spee_su_t1980, speech_protection_kernel__dignity_reading, suppression_requirement, 1980, 0.65).
narrative_ontology:measurement(spee_su_t2000, speech_protection_kernel__dignity_reading, suppression_requirement, 2000, 0.75).
narrative_ontology:measurement(spee_su_t2024, speech_protection_kernel__dignity_reading, suppression_requirement, 2024, 0.8).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(speech_protection_kernel__dignity_reading, identity_coordination).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
