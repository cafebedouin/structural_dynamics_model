% ============================================================================
% CONSTRAINT STORY: speech_protection_boundary__balancing_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_speech_protection_boundary__balancing_reading, []).

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
 *   constraint_id: speech_protection_boundary__balancing_reading
 *   human_readable: Speech Protection Boundary (Balancing Reading)
 *   domain: constitutional_law/political_philosophy/speech_regulation
 *
 * SUMMARY:
 *   This constraint describes the 'balancing' reading of the speech
 *   protection boundary, where the scope of First Amendment protection is
 *   determined case-by-case by weighing speech interests against other
 *   constitutional values and demonstrated harms. This reading acknowledges
 *   that speech rights are not absolute and must be reconciled with other
 *   societal goods. The constraint is claimed as a Tangled Rope because it
 *   aims to coordinate competing values but inherently involves asymmetric
 *   extraction of speech rights in certain contexts, requiring active
 *   judicial enforcement.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(speech_protection_boundary__balancing_reading, 0.62).
domain_priors:suppression_score(speech_protection_boundary__balancing_reading, 0.58).
domain_priors:theater_ratio(speech_protection_boundary__balancing_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(speech_protection_boundary__balancing_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(speech_protection_boundary__balancing_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(speech_protection_boundary__balancing_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(speech_protection_boundary__balancing_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(speech_protection_boundary__balancing_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(speech_protection_boundary__balancing_reading, tangled_rope).
narrative_ontology:human_readable(speech_protection_boundary__balancing_reading, "Speech Protection Boundary (Balancing Reading)").
narrative_ontology:topic_domain(speech_protection_boundary__balancing_reading, "constitutional_law/political_philosophy/speech_regulation").

domain_priors:requires_active_enforcement(speech_protection_boundary__balancing_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(speech_protection_boundary__balancing_reading, '93508e53-8740-4b68-b97a-a46d6d7cbe9a').
narrative_ontology:cs_kernel_codification('93508e53-8740-4b68-b97a-a46d6d7cbe9a', fixed_text).
narrative_ontology:cs_authority_grounding('93508e53-8740-4b68-b97a-a46d6d7cbe9a', lineage).
narrative_ontology:cs_interpretation_layer_present('93508e53-8740-4b68-b97a-a46d6d7cbe9a').
narrative_ontology:cs_reading_relation('93508e53-8740-4b68-b97a-a46d6d7cbe9a', speech_protection_boundary__absolutist_reading, coexists_with).
narrative_ontology:cs_reading_relation('93508e53-8740-4b68-b97a-a46d6d7cbe9a', speech_protection_boundary__harm_limited_reading, coexists_with).
narrative_ontology:cs_axiom('93508e53-8740-4b68-b97a-a46d6d7cbe9a', foundational, speech_rights_are_not_absolute).
narrative_ontology:cs_axiom_status(speech_rights_are_not_absolute, holdable).
narrative_ontology:cs_axiom_grounding('93508e53-8740-4b68-b97a-a46d6d7cbe9a', speech_rights_are_not_absolute, deontological).
narrative_ontology:cs_axiom('93508e53-8740-4b68-b97a-a46d6d7cbe9a', foundational, state_has_compelling_interest_in_regulating_harm).
narrative_ontology:cs_axiom_status(state_has_compelling_interest_in_regulating_harm, holdable).
narrative_ontology:cs_axiom_grounding('93508e53-8740-4b68-b97a-a46d6d7cbe9a', state_has_compelling_interest_in_regulating_harm, deontological).
narrative_ontology:cs_reference_frame('93508e53-8740-4b68-b97a-a46d6d7cbe9a', post_brandenburg_era).
narrative_ontology:cs_drift_state('93508e53-8740-4b68-b97a-a46d6d7cbe9a', contemporary_digital_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('93508e53-8740-4b68-b97a-a46d6d7cbe9a', '').
narrative_ontology:cs_kernel_id(speech_protection_boundary__balancing_reading, speech_protection_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(speech_protection_boundary__balancing_reading, constitutional_order).
narrative_ontology:constraint_beneficiary(speech_protection_boundary__balancing_reading, vulnerable_groups).
narrative_ontology:constraint_beneficiary(speech_protection_boundary__balancing_reading, judiciary).
narrative_ontology:constraint_victim(speech_protection_boundary__balancing_reading, speakers_of_restricted_speech).
narrative_ontology:constraint_victim(speech_protection_boundary__balancing_reading, advocates_for_absolute_speech).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interprets the First Amendment and other constitutional values, applying balancing tests case-by-case to determine the scope of protected speech. Maintains its role as the ultimate arbiter of speech boundaries.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__balancing_reading, judiciary, agenda_setter,
    institutional, generational, analytical, universal).

% Bear the cost of speech restrictions when their expression is deemed to fall outside the protected zone after balancing. Their speech may be chilled or suppressed, and they face legal consequences for non-compliance.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__balancing_reading, speakers_of_restricted_speech, payer,
    moderate, immediate, constrained, national).

% Benefit from the balancing framework when it restricts speech that causes demonstrated harms, such as incitement, harassment, or threats, thereby protecting their dignity, safety, and equality.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__balancing_reading, vulnerable_groups, beneficiary,
    organized, generational, constrained, national).

% Experience the balancing framework as an erosion of fundamental speech rights, arguing for a more categorical, near-absolute protection. They bear the cost of a legal system that permits restrictions they view as illegitimate.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__balancing_reading, advocates_for_absolute_speech, payer,
    organized, generational, constrained, national).

% Benefits from a flexible framework that allows the reconciliation of competing constitutional values, preventing the absolute prioritization of one right (speech) over others (e.g., privacy, equality, public safety), thereby maintaining systemic coherence.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__balancing_reading, constitutional_order, beneficiary,
    institutional, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(speech_protection_boundary__balancing_reading, constitutional_order).

% Analyze and critique the application of balancing tests, contributing to the ongoing debate about the appropriate scope of speech protection. They do not directly benefit or pay, but their work influences the framework's evolution.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__balancing_reading, legal_scholars, observer,
    analytical, biographical, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To reconcile the First Amendment's broad protection of speech with other fundamental constitutional values and the state's legitimate interest in preventing demonstrated harms, ensuring that no single right is absolute.
% TRANSFER_FUNCTION: Transfers interpretive authority over speech boundaries to the judiciary, and in specific contexts, transfers the burden of harm from vulnerable groups to speakers whose expression is restricted.
% ABSENT_VOICES: Those who advocate for a purely categorical approach to speech, either absolute protection or absolute restriction based on content, are often marginalized in the balancing framework, as their positions are seen as too rigid for complex constitutional conflicts.
% DISAPPEARANCE_RATIONALE: If the balancing framework vanished, the legal system would either default to an absolutist interpretation of speech (leading to unchecked harms and conflicts with other rights) or an overly restrictive one (leading to censorship), fundamentally altering the nature of public discourse and constitutional jurisprudence.
% FOUNDING_PROBLEM: To establish a flexible and adaptable method for adjudicating conflicts between free speech and other societal interests or constitutional rights, avoiding rigid rules that could lead to unjust outcomes in unforeseen circumstances.
% FOUNDING_PROBLEM_CORROBORATION: Legal scholars, civil liberties organizations, and human rights advocates from diverse perspectives consistently engage with this problem, demonstrating its ongoing relevance in contemporary society, particularly with the rise of new communication technologies and forms of harm.
narrative_ontology:disappearance_verdict(speech_protection_boundary__balancing_reading, world_rearranges).
narrative_ontology:founding_problem_status(speech_protection_boundary__balancing_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(speech_protection_boundary__balancing_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(speech_protection_boundary__balancing_reading, 'none', 1).
narrative_ontology:epsilon_provenance(speech_protection_boundary__balancing_reading, 0.62, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(speech_protection_boundary__balancing_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(speech_protection_boundary__balancing_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(speech_protection_boundary__balancing_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.62) because while the framework aims for coordination, it frequently results in the restriction of speech, imposing costs on speakers. Suppression is also moderate (0.58) as the legal system actively enforces these restrictions, though it does not aim for total suppression of any particular viewpoint. Theater ratio is low (0.15) because the judicial weighing process is generally genuine, even if its outcomes are contested. Accessibility collapse is moderate (0.45) as alternative (e.g., absolutist) approaches to speech protection are not entirely foreclosed but are largely outside the mainstream legal framework. Resistance is high (0.70) due to ongoing legal challenges and academic debate from advocates for both broader and narrower speech protections.
 *
 * PERSPECTIVAL GAP:
 *   The judiciary (agenda-setter) experiences this framework as a necessary and legitimate Rope or Scaffold, enabling the coordination of complex constitutional values. In contrast, speakers whose speech is restricted, and advocates for absolute speech (payer seats), experience it as a Snare, perceiving it as an arbitrary or illegitimate extraction of fundamental rights. The engine computes this divergence from the structural data, reflecting the inherent tension in balancing competing interests.
 *
 * DIRECTIONALITY LOGIC:
 *   The constitutional order and vulnerable groups are beneficiaries, as the framework protects other rights and prevents harm. The judiciary also benefits by maintaining its interpretive authority and ensuring constitutional coherence. Speakers of restricted speech and advocates for absolute speech are targets, as they bear the costs of limitations on expression. Their exit options are constrained by the legal system's authority.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    balancing_vs_categorical_ambiguity,
    'Is speech protection best served by a flexible, case-by-case balancing test, or by clear, categorical rules that offer greater predictability?',
    'Empirical studies on chilling effects and judicial consistency under balancing tests versus hypothetical outcomes under categorical rules; comparative legal analysis of jurisdictions employing different approaches.',
    'If categorical rules prove superior in promoting speech and predictability without undue harm, the balancing framework''s extractiveness and suppression might be re-evaluated as unnecessary overhead. If balancing proves more adaptable and just, its coordination function is strengthened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(balancing_vs_categorical_ambiguity, conceptual, 'Debate over the optimal legal methodology for speech protection.').

omega_variable(
    scope_of_harm_definition,
    'How broadly should ''harm'' be defined when balancing against speech interests, particularly in the context of new forms of digital communication and systemic harms?',
    'Sociological and psychological research on the impact of speech, legislative action to define actionable harms, and judicial precedent refining the categories of unprotected speech.',
    'A broader definition of harm would increase the constraint''s effective extractiveness and suppression, shifting the boundary of protected speech. A narrower definition would reduce it, expanding protected speech.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(scope_of_harm_definition, empirical, 'The evolving definition of ''harm'' in speech regulation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(speech_protection_boundary__balancing_reading, 1969, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(spee_tr_t1969, speech_protection_boundary__balancing_reading, theater_ratio, 1969, 0.1).
narrative_ontology:measurement(spee_tr_t1980, speech_protection_boundary__balancing_reading, theater_ratio, 1980, 0.11).
narrative_ontology:measurement(spee_tr_t1990, speech_protection_boundary__balancing_reading, theater_ratio, 1990, 0.12).
narrative_ontology:measurement(spee_tr_t2000, speech_protection_boundary__balancing_reading, theater_ratio, 2000, 0.13).
narrative_ontology:measurement(spee_tr_t2010, speech_protection_boundary__balancing_reading, theater_ratio, 2010, 0.14).
narrative_ontology:measurement(spee_tr_t2024, speech_protection_boundary__balancing_reading, theater_ratio, 2024, 0.15).

% Extraction over time
narrative_ontology:measurement(spee_be_t1969, speech_protection_boundary__balancing_reading, base_extractiveness, 1969, 0.5).
narrative_ontology:measurement(spee_be_t1980, speech_protection_boundary__balancing_reading, base_extractiveness, 1980, 0.54).
narrative_ontology:measurement(spee_be_t1990, speech_protection_boundary__balancing_reading, base_extractiveness, 1990, 0.58).
narrative_ontology:measurement(spee_be_t2000, speech_protection_boundary__balancing_reading, base_extractiveness, 2000, 0.6).
narrative_ontology:measurement(spee_be_t2010, speech_protection_boundary__balancing_reading, base_extractiveness, 2010, 0.61).
narrative_ontology:measurement(spee_be_t2024, speech_protection_boundary__balancing_reading, base_extractiveness, 2024, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(spee_su_t1969, speech_protection_boundary__balancing_reading, suppression_requirement, 1969, 0.45).
narrative_ontology:measurement(spee_su_t1980, speech_protection_boundary__balancing_reading, suppression_requirement, 1980, 0.48).
narrative_ontology:measurement(spee_su_t1990, speech_protection_boundary__balancing_reading, suppression_requirement, 1990, 0.52).
narrative_ontology:measurement(spee_su_t2000, speech_protection_boundary__balancing_reading, suppression_requirement, 2000, 0.55).
narrative_ontology:measurement(spee_su_t2010, speech_protection_boundary__balancing_reading, suppression_requirement, 2010, 0.57).
narrative_ontology:measurement(spee_su_t2024, speech_protection_boundary__balancing_reading, suppression_requirement, 2024, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(speech_protection_boundary__balancing_reading, enforcement_mechanism).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'speech_protection_boundary' kernel. It focuses on the balancing approach, distinct from the absolutist and harm-limited readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
