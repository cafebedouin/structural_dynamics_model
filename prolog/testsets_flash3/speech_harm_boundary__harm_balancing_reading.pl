% ============================================================================
% CONSTRAINT STORY: speech_harm_boundary__harm_balancing_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_speech_harm_boundary__harm_balancing_reading, []).

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
 *   constraint_id: speech_harm_boundary__harm_balancing_reading
 *   human_readable: Speech Harm Boundary (Proportionality Balancing Reading)
 *   domain: constitutional_law/political_philosophy/communication_ethics
 *
 * SUMMARY:
 *   This constraint represents the 'harm balancing' reading of the speech
 *   harm boundary, where speech protection is presumptive but yields to
 *   demonstrated harm, subject to a proportionality test. This reading
 *   acknowledges a genuine coordination problem in managing conflicting
 *   rights but involves active enforcement and imposes costs on speakers of
 *   harmful speech. It is one of several competing interpretations of the
 *   fundamental tension between free speech and protection from harm.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(speech_harm_boundary__harm_balancing_reading, 0.45).
domain_priors:suppression_score(speech_harm_boundary__harm_balancing_reading, 0.3).
domain_priors:theater_ratio(speech_harm_boundary__harm_balancing_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(speech_harm_boundary__harm_balancing_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(speech_harm_boundary__harm_balancing_reading, suppression_requirement, 0.3).
narrative_ontology:constraint_metric(speech_harm_boundary__harm_balancing_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(speech_harm_boundary__harm_balancing_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(speech_harm_boundary__harm_balancing_reading, resistance, 0.25).

% --- Constraint claim ---
narrative_ontology:constraint_claim(speech_harm_boundary__harm_balancing_reading, tangled_rope).
narrative_ontology:human_readable(speech_harm_boundary__harm_balancing_reading, "Speech Harm Boundary (Proportionality Balancing Reading)").
narrative_ontology:topic_domain(speech_harm_boundary__harm_balancing_reading, "constitutional_law/political_philosophy/communication_ethics").

domain_priors:requires_active_enforcement(speech_harm_boundary__harm_balancing_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(speech_harm_boundary__harm_balancing_reading, '3974a3e5-5985-4fff-a412-cc4770c67aac').
narrative_ontology:cs_kernel_codification('3974a3e5-5985-4fff-a412-cc4770c67aac', formalized).
narrative_ontology:cs_authority_grounding('3974a3e5-5985-4fff-a412-cc4770c67aac', lineage).
narrative_ontology:cs_interpretation_layer_present('3974a3e5-5985-4fff-a412-cc4770c67aac').
narrative_ontology:cs_reading_relation('3974a3e5-5985-4fff-a412-cc4770c67aac', speech_harm_boundary__absolutist_reading, coexists_with).
narrative_ontology:cs_reading_relation('3974a3e5-5985-4fff-a412-cc4770c67aac', speech_harm_boundary__dignity_reading, coexists_with).
narrative_ontology:cs_axiom('3974a3e5-5985-4fff-a412-cc4770c67aac', foundational, speech_presumptively_protected_but_not_absolute).
narrative_ontology:cs_axiom_status(speech_presumptively_protected_but_not_absolute, holdable).
narrative_ontology:cs_axiom_grounding('3974a3e5-5985-4fff-a412-cc4770c67aac', speech_presumptively_protected_but_not_absolute, deontological).
narrative_ontology:cs_axiom('3974a3e5-5985-4fff-a412-cc4770c67aac', foundational, demonstrable_harm_justifies_restriction).
narrative_ontology:cs_axiom_status(demonstrable_harm_justifies_restriction, holdable).
narrative_ontology:cs_axiom_grounding('3974a3e5-5985-4fff-a412-cc4770c67aac', demonstrable_harm_justifies_restriction, empirically_contingent).
narrative_ontology:cs_reference_frame('3974a3e5-5985-4fff-a412-cc4770c67aac', liberal_democratic_balancing_tradition).
narrative_ontology:cs_drift_state('3974a3e5-5985-4fff-a412-cc4770c67aac', contemporary_social_media_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('3974a3e5-5985-4fff-a412-cc4770c67aac', '').
narrative_ontology:cs_kernel_id(speech_harm_boundary__harm_balancing_reading, speech_harm_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(speech_harm_boundary__harm_balancing_reading, society_at_large).
narrative_ontology:constraint_beneficiary(speech_harm_boundary__harm_balancing_reading, vulnerable_groups).
narrative_ontology:constraint_victim(speech_harm_boundary__harm_balancing_reading, speakers_of_harmful_speech).
narrative_ontology:constraint_victim(speech_harm_boundary__harm_balancing_reading, free_speech_advocates).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefits from a framework that allows for the restriction of speech demonstrably causing harm, fostering a more civil and safe public discourse. Bears the cost of some speech being restricted.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__harm_balancing_reading, society_at_large, beneficiary,
    institutional, generational, constrained, national).

% Receives protection from speech that directly incites violence, harassment, or discrimination, which can cause tangible harm. Advocates for robust enforcement of harm-based restrictions.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__harm_balancing_reading, vulnerable_groups, beneficiary,
    organized, biographical, constrained, local).

% Bears the direct cost of speech restrictions when their expression is deemed to cause demonstrable harm. Their speech may be censored, fined, or otherwise penalized.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__harm_balancing_reading, speakers_of_harmful_speech, payer,
    moderate, immediate, constrained, local).

% Perceives any restriction on speech as a cost to the overall principle of free expression, even when justified by harm. They actively challenge the scope and application of harm-based balancing tests.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__harm_balancing_reading, free_speech_advocates, payer,
    organized, generational, mobile, national).

% Responsible for applying the proportionality balancing test, weighing speech value against demonstrated harm. They define the categories of unprotected speech and enforce restrictions, bearing the institutional cost of complex adjudication.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__harm_balancing_reading, courts_and_regulators, agenda_setter,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Balances the societal interest in free expression with the need to protect individuals and groups from demonstrable harm, providing a framework for adjudicating conflicts between these values.
% TRANSFER_FUNCTION: Transfers the burden of restriction from those harmed by speech to the speakers of demonstrably harmful speech, and the cost of adjudication to the state.
% ABSENT_VOICES: Those who believe speech should be absolutely protected, regardless of harm, are often marginalized in the balancing process, arguing that any restriction opens the door to censorship. Conversely, those who believe dignity should always override speech are also often excluded from the current balancing framework.
% DISAPPEARANCE_RATIONALE: If this constraint vanished, society would face an immediate and profound conflict between unfettered expression and protection from harm. Either harmful speech would proliferate unchecked, or arbitrary restrictions would emerge, leading to a chaotic and unstable public sphere.
% FOUNDING_PROBLEM: The problem of how to reconcile the fundamental right to free speech with the equally fundamental need to protect individuals and society from direct and demonstrable harm caused by speech.
% FOUNDING_PROBLEM_CORROBORATION: Legal scholars, human rights organizations, and victims of hate speech consistently corroborate that this problem remains live and requires ongoing adjudication. While free speech advocates may contest the specific balancing outcomes, the underlying tension is widely acknowledged.
narrative_ontology:disappearance_verdict(speech_harm_boundary__harm_balancing_reading, world_rearranges).
narrative_ontology:founding_problem_status(speech_harm_boundary__harm_balancing_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(speech_harm_boundary__harm_balancing_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(speech_harm_boundary__harm_balancing_reading, 'none', 1).
narrative_ontology:epsilon_provenance(speech_harm_boundary__harm_balancing_reading, 0.45, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(speech_harm_boundary__harm_balancing_reading_tests).
:- end_tests(speech_harm_boundary__harm_balancing_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.45) reflects the cost imposed on speakers whose speech is restricted, as well as the societal cost of complex legal adjudication. Suppression (0.30) is moderate, as enforcement requires active legal processes rather than outright pre-emptive bans, and resistance from free speech advocates is ongoing. The theater ratio (0.10) is low, indicating that the balancing process is generally genuine, though its application can be contentious. The metrics reflect the dynamic nature of balancing, with slight fluctuations over time as societal norms and legal interpretations evolve.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of vulnerable groups, this constraint is a necessary rope, providing vital protection. From the perspective of some free speech advocates, it is a snare, eroding fundamental liberties. The courts, as agenda-setters, view it as a complex but essential tangled rope, balancing competing interests. The engine's classification will reflect these structural asymmetries.
 *
 * DIRECTIONALITY LOGIC:
 *   Society at large and vulnerable groups are beneficiaries, as they gain protection from harm. Speakers of harmful speech and free speech advocates are payers, bearing the costs of restriction and the perceived erosion of absolute speech rights. Courts and regulators act as agenda-setters, defining and enforcing the boundaries.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    demonstrated_harm_definition,
    'What constitutes ''demonstrated harm'' in practice, and how consistently is this standard applied across different contexts and jurisdictions?',
    'Empirical analysis of judicial decisions and regulatory enforcement actions, comparing outcomes against a clear, pre-defined set of harm criteria.',
    'If ''demonstrated harm'' is inconsistently applied or defined too broadly, the constraint''s extractiveness and suppression would be higher than measured, effectively operating as a snare for certain types of speech. If too narrowly, it would fail to protect vulnerable groups.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(demonstrated_harm_definition, empirical, 'Ambiguity in the definition and application of ''demonstrated harm'' in speech cases.').

omega_variable(
    proportionality_test_objectivity,
    'To what extent is the proportionality balancing test an objective legal standard versus a reflection of prevailing social and political values?',
    'Comparative legal analysis across different political systems and historical periods, examining how the ''balance'' shifts with changes in societal consensus or political power.',
    'If the test is highly subjective, its classification as a tangled rope might mask a more snare-like operation, where powerful groups can more easily define ''harm'' to suppress dissenting speech. If truly objective, it reinforces the coordination function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(proportionality_test_objectivity, conceptual, 'The objectivity and neutrality of the proportionality balancing test.').

omega_variable(
    reading_absolutist_vs_harm_balancing,
    'Is this constraint a genuine ''harm balancing'' framework, or does it implicitly foreclose an ''absolutist'' reading of speech protection?',
    'Analysis of legal precedent: if the ''harm balancing'' framework has systematically dismantled or rendered impossible the core tenets of the ''absolutist'' reading within the same legal system, then it forecloses it.',
    'If it forecloses the absolutist reading, the constraint''s effective suppression is higher, as it has eliminated a structural alternative. If it merely coexists, the contest remains live.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_absolutist_vs_harm_balancing, conceptual, 'Relationship between harm_balancing_reading and absolutist_reading.').

omega_variable(
    reading_dignity_vs_harm_balancing,
    'Does the ''harm balancing'' reading adequately address the concerns of the ''dignity'' reading, or does it influence it by setting a lower bar for protection?',
    'Comparative analysis of legal outcomes: if the ''harm balancing'' approach consistently leads to less protection for dignity-related harms than the ''dignity'' reading would demand, it influences it negatively.',
    'If it influences the dignity reading by setting a lower bar, it could lead to higher effective extraction for those seeking dignity-based protections, as their claims are systematically undervalued within this framework.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_dignity_vs_harm_balancing, conceptual, 'Relationship between harm_balancing_reading and dignity_reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(speech_harm_boundary__harm_balancing_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(spee_tr_t0, speech_harm_boundary__harm_balancing_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement(spee_tr_t10, speech_harm_boundary__harm_balancing_reading, theater_ratio, 10, 0.09).
narrative_ontology:measurement(spee_tr_t20, speech_harm_boundary__harm_balancing_reading, theater_ratio, 20, 0.1).
narrative_ontology:measurement(spee_tr_t30, speech_harm_boundary__harm_balancing_reading, theater_ratio, 30, 0.09).
narrative_ontology:measurement(spee_tr_t40, speech_harm_boundary__harm_balancing_reading, theater_ratio, 40, 0.11).
narrative_ontology:measurement(spee_tr_t50, speech_harm_boundary__harm_balancing_reading, theater_ratio, 50, 0.1).

% Extraction over time
narrative_ontology:measurement(spee_be_t0, speech_harm_boundary__harm_balancing_reading, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(spee_be_t10, speech_harm_boundary__harm_balancing_reading, base_extractiveness, 10, 0.42).
narrative_ontology:measurement(spee_be_t20, speech_harm_boundary__harm_balancing_reading, base_extractiveness, 20, 0.45).
narrative_ontology:measurement(spee_be_t30, speech_harm_boundary__harm_balancing_reading, base_extractiveness, 30, 0.43).
narrative_ontology:measurement(spee_be_t40, speech_harm_boundary__harm_balancing_reading, base_extractiveness, 40, 0.46).
narrative_ontology:measurement(spee_be_t50, speech_harm_boundary__harm_balancing_reading, base_extractiveness, 50, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(spee_su_t0, speech_harm_boundary__harm_balancing_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(spee_su_t10, speech_harm_boundary__harm_balancing_reading, suppression_requirement, 10, 0.28).
narrative_ontology:measurement(spee_su_t20, speech_harm_boundary__harm_balancing_reading, suppression_requirement, 20, 0.3).
narrative_ontology:measurement(spee_su_t30, speech_harm_boundary__harm_balancing_reading, suppression_requirement, 30, 0.29).
narrative_ontology:measurement(spee_su_t40, speech_harm_boundary__harm_balancing_reading, suppression_requirement, 40, 0.31).
narrative_ontology:measurement(spee_su_t50, speech_harm_boundary__harm_balancing_reading, suppression_requirement, 50, 0.3).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(speech_harm_boundary__harm_balancing_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(speech_harm_boundary__harm_balancing_reading, speech_harm_boundary__absolutist_reading).
narrative_ontology:affects_constraint(speech_harm_boundary__harm_balancing_reading, speech_harm_boundary__dignity_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'speech_harm_boundary' kernel. This 'harm_balancing_reading' focuses on proportionality and demonstrable harm, distinct from the 'absolutist_reading' (near-absolute protection) and the 'dignity_reading' (dignity as a primary limit).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
