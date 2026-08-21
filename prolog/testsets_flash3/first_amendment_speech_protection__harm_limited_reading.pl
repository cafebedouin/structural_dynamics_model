% ============================================================================
% CONSTRAINT STORY: first_amendment_speech_protection__harm_limited_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_first_amendment_speech_protection__harm_limited_reading, []).

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
 *   constraint_id: first_amendment_speech_protection__harm_limited_reading
 *   human_readable: First Amendment Speech Protection: Harm-Limited Reading
 *   domain: constitutional_law/political_philosophy/speech_regulation
 *
 * SUMMARY:
 *   This constraint represents the 'harm-limited' reading of First Amendment
 *   speech protection, where the scope of protected speech contracts when it
 *   causes demonstrable, unconsented-to harm. This reading prioritizes the
 *   protection of vulnerable groups from speech-induced harm over an
 *   expansive, content-neutral approach. It is one of several competing
 *   interpretations of the First Amendment's scope.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(first_amendment_speech_protection__harm_limited_reading, 0.65).
domain_priors:suppression_score(first_amendment_speech_protection__harm_limited_reading, 0.7).
domain_priors:theater_ratio(first_amendment_speech_protection__harm_limited_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(first_amendment_speech_protection__harm_limited_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(first_amendment_speech_protection__harm_limited_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(first_amendment_speech_protection__harm_limited_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(first_amendment_speech_protection__harm_limited_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(first_amendment_speech_protection__harm_limited_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(first_amendment_speech_protection__harm_limited_reading, tangled_rope).
narrative_ontology:human_readable(first_amendment_speech_protection__harm_limited_reading, "First Amendment Speech Protection: Harm-Limited Reading").
narrative_ontology:topic_domain(first_amendment_speech_protection__harm_limited_reading, "constitutional_law/political_philosophy/speech_regulation").

domain_priors:requires_active_enforcement(first_amendment_speech_protection__harm_limited_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(first_amendment_speech_protection__harm_limited_reading, '136c4551-c024-45d3-b1ce-e566459f13ae').
narrative_ontology:cs_kernel_codification('136c4551-c024-45d3-b1ce-e566459f13ae', fixed_text).
narrative_ontology:cs_authority_grounding('136c4551-c024-45d3-b1ce-e566459f13ae', lineage).
narrative_ontology:cs_interpretation_layer_present('136c4551-c024-45d3-b1ce-e566459f13ae').
narrative_ontology:cs_reading_relation('136c4551-c024-45d3-b1ce-e566459f13ae', first_amendment_speech_protection__absolutist_reading, coexists_with).
narrative_ontology:cs_reading_relation('136c4551-c024-45d3-b1ce-e566459f13ae', first_amendment_speech_protection__categorical_balancing_reading, coexists_with).
narrative_ontology:cs_axiom('136c4551-c024-45d3-b1ce-e566459f13ae', foundational, speech_rights_are_not_absolute).
narrative_ontology:cs_axiom_status(speech_rights_are_not_absolute, holdable).
narrative_ontology:cs_axiom_grounding('136c4551-c024-45d3-b1ce-e566459f13ae', speech_rights_are_not_absolute, deontological).
narrative_ontology:cs_axiom('136c4551-c024-45d3-b1ce-e566459f13ae', foundational, harm_prevention_is_a_compelling_state_interest).
narrative_ontology:cs_axiom_status(harm_prevention_is_a_compelling_state_interest, holdable).
narrative_ontology:cs_axiom_grounding('136c4551-c024-45d3-b1ce-e566459f13ae', harm_prevention_is_a_compelling_state_interest, instrumental).
narrative_ontology:cs_reference_frame('136c4551-c024-45d3-b1ce-e566459f13ae', speech_as_social_responsibility).
narrative_ontology:cs_drift_state('136c4551-c024-45d3-b1ce-e566459f13ae', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('136c4551-c024-45d3-b1ce-e566459f13ae', '').
narrative_ontology:cs_kernel_id(first_amendment_speech_protection__harm_limited_reading, first_amendment_speech_protection).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(first_amendment_speech_protection__harm_limited_reading, vulnerable_minorities).
narrative_ontology:constraint_beneficiary(first_amendment_speech_protection__harm_limited_reading, targets_of_hate_speech).
narrative_ontology:constraint_victim(first_amendment_speech_protection__harm_limited_reading, speakers_of_harmful_speech).
narrative_ontology:constraint_victim(first_amendment_speech_protection__harm_limited_reading, absolutist_free_speech_advocates).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefits from the ability to seek legal recourse or regulatory intervention against speech that causes demonstrable, unconsented-to harm, such as incitement to violence or severe harassment. Their ability to exit harmful environments is often limited by systemic factors.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__harm_limited_reading, vulnerable_minorities, beneficiary,
    powerless, immediate, trapped, national).

% Receives protection from speech acts that directly inflict emotional distress, physical threats, or discrimination, allowing them to participate in public life without constant exposure to targeted abuse. Their ability to avoid such speech is often limited.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__harm_limited_reading, targets_of_hate_speech, beneficiary,
    powerless, immediate, trapped, local).

% Bears the cost of having their speech restricted or penalized when it is deemed to cause demonstrable, unconsented-to harm. Their 'exit' involves modifying their speech or facing legal consequences, which they perceive as a suppression of their expressive rights.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__harm_limited_reading, speakers_of_harmful_speech, payer,
    moderate, immediate, constrained, national).

% Opposes any limitation on speech based on its content or potential for harm, viewing such restrictions as a dangerous precedent. They bear the cost of a legal framework that permits such limitations, which they actively challenge through litigation and advocacy.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__harm_limited_reading, absolutist_free_speech_advocates, payer,
    organized, generational, constrained, national).

% Interprets and enforces the harm-limited reading, balancing speech rights against the prevention of harm. They define what constitutes 'demonstrable unconsented-to harm' and apply these standards, shaping the boundaries of protected speech.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__harm_limited_reading, courts_and_regulators, agenda_setter,
    institutional, generational, analytical, national).

% Would argue for a system of predefined speech categories (e.g., obscenity, fighting words) that are either protected or unprotected, rather than a direct harm test. Their preferred framework is sidelined by the harm-limited approach, which they see as too subjective or overreaching.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__harm_limited_reading, categorical_balancing_advocates, excluded,
    organized, biographical, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates social interaction by establishing a boundary for acceptable speech, aiming to prevent certain types of harm and ensure a more inclusive public discourse, particularly for vulnerable groups.
% TRANSFER_FUNCTION: Transfers the burden of enduring harmful speech from vulnerable individuals and groups to speakers, who must now demonstrate their speech does not cause demonstrable, unconsented-to harm or face consequences.
% ABSENT_VOICES: Those who advocate for a purely categorical approach to speech regulation, where specific types of speech are deemed unprotected regardless of direct harm, are excluded from the core debate, which focuses on the harm principle.
% DISAPPEARANCE_RATIONALE: If the harm-limited reading vanished, the legal landscape for speech would revert to a more absolutist or categorical approach, potentially increasing exposure to harmful speech for vulnerable groups and shifting the burden of protection back to individuals. Legal challenges and social norms would rapidly reorganize.
% FOUNDING_PROBLEM: The problem of speech causing direct, severe, and unconsented-to harm to individuals and groups, particularly those historically marginalized, without adequate legal recourse.
% FOUNDING_PROBLEM_CORROBORATION: Advocates for vulnerable groups and civil rights organizations consistently attest to the ongoing problem of harmful speech and the need for this reading. Legal scholars and social scientists provide empirical evidence of speech-induced harm, corroborating the problem's live status from outside the immediate beneficiary group.
narrative_ontology:disappearance_verdict(first_amendment_speech_protection__harm_limited_reading, world_rearranges).
narrative_ontology:founding_problem_status(first_amendment_speech_protection__harm_limited_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(first_amendment_speech_protection__harm_limited_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(first_amendment_speech_protection__harm_limited_reading, 'none', 1).
narrative_ontology:epsilon_provenance(first_amendment_speech_protection__harm_limited_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(first_amendment_speech_protection__harm_limited_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(first_amendment_speech_protection__harm_limited_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(first_amendment_speech_protection__harm_limited_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate-high (0.65) because it imposes significant costs on speakers whose expression is deemed harmful, requiring them to self-censor or face legal consequences. Suppression is high (0.70) as it actively restricts certain forms of speech to protect others. Theater ratio is low (0.10) because the enforcement is direct and functional, not performative. Accessibility collapse is moderate (0.40) as alternatives for harmful speech are limited, but other forms of expression remain. Resistance is high (0.75) due to strong opposition from free speech absolutists.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of vulnerable minorities, this reading is a necessary coordination mechanism to ensure safety and dignity. From the perspective of absolutist free speech advocates, it is a snare that suppresses legitimate expression. The engine's classification will reflect this divergence based on the structural positions.
 *
 * DIRECTIONALITY LOGIC:
 *   Vulnerable minorities and targets of hate speech are clear beneficiaries, as the constraint aims to protect them from harm. Speakers of harmful speech and absolutist free speech advocates are victims, as their expressive freedom is curtailed. Courts and regulators act as agenda-setters, defining and enforcing the harm boundary. Categorical balancing advocates are excluded, as their framework is not central to this reading.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    demonstrable_harm_definition,
    'What constitutes ''demonstrable unconsented-to harm'' in a legally actionable sense, and how is it objectively measured?',
    'Development of clear, judicially recognized standards and evidentiary thresholds for various types of speech-induced harm (e.g., psychological, economic, physical incitement).',
    'A clear, objective definition would reduce the perceived extractiveness and suppression for speakers by providing predictable boundaries. An ambiguous definition would increase both, as speakers face uncertainty and potential arbitrary enforcement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(demonstrable_harm_definition, conceptual, 'Ambiguity in defining the harm threshold for speech regulation.').

omega_variable(
    reading_coexistence_stability,
    'Can the harm-limited reading coexist stably with absolutist or categorical balancing readings within a single legal system, or will one eventually displace the others?',
    'Long-term observation of judicial precedent and legislative action across jurisdictions where these readings are in tension. Analysis of whether legal frameworks converge or diverge.',
    'If stable coexistence is impossible, the system faces ongoing legal and political instability. If one reading forecloses others, the classification of the First Amendment kernel itself would shift to reflect the dominant interpretation.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_coexistence_stability, empirical, 'The long-term stability and compatibility of competing First Amendment readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(first_amendment_speech_protection__harm_limited_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(firs_tr_t0, first_amendment_speech_protection__harm_limited_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(firs_tr_t5, first_amendment_speech_protection__harm_limited_reading, theater_ratio, 5, 0.1).
narrative_ontology:measurement(firs_tr_t10, first_amendment_speech_protection__harm_limited_reading, theater_ratio, 10, 0.1).
narrative_ontology:measurement(firs_tr_t15, first_amendment_speech_protection__harm_limited_reading, theater_ratio, 15, 0.1).
narrative_ontology:measurement(firs_tr_t20, first_amendment_speech_protection__harm_limited_reading, theater_ratio, 20, 0.1).

% Extraction over time
narrative_ontology:measurement(firs_be_t0, first_amendment_speech_protection__harm_limited_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(firs_be_t5, first_amendment_speech_protection__harm_limited_reading, base_extractiveness, 5, 0.58).
narrative_ontology:measurement(firs_be_t10, first_amendment_speech_protection__harm_limited_reading, base_extractiveness, 10, 0.62).
narrative_ontology:measurement(firs_be_t15, first_amendment_speech_protection__harm_limited_reading, base_extractiveness, 15, 0.64).
narrative_ontology:measurement(firs_be_t20, first_amendment_speech_protection__harm_limited_reading, base_extractiveness, 20, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(firs_su_t0, first_amendment_speech_protection__harm_limited_reading, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(firs_su_t5, first_amendment_speech_protection__harm_limited_reading, suppression_requirement, 5, 0.63).
narrative_ontology:measurement(firs_su_t10, first_amendment_speech_protection__harm_limited_reading, suppression_requirement, 10, 0.66).
narrative_ontology:measurement(firs_su_t15, first_amendment_speech_protection__harm_limited_reading, suppression_requirement, 15, 0.68).
narrative_ontology:measurement(firs_su_t20, first_amendment_speech_protection__harm_limited_reading, suppression_requirement, 20, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(first_amendment_speech_protection__harm_limited_reading, enforcement_mechanism).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'first_amendment_speech_protection' kernel. Its structural delta is a contraction of protected speech around a harm boundary, influencing the scope of other speech-related regulations.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
