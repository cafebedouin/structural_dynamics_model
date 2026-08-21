% ============================================================================
% CONSTRAINT STORY: first_amendment_speech_protection__categorical_balancing_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_first_amendment_speech_protection__categorical_balancing_reading, []).

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
 *   constraint_id: first_amendment_speech_protection__categorical_balancing_reading
 *   human_readable: First Amendment Speech Protection: Categorical Balancing Reading
 *   domain: constitutional_law/political_philosophy/speech_regulation
 *
 * SUMMARY:
 *   This constraint represents the 'categorical balancing' reading of the
 *   First Amendment, where speech is protected or unprotected based on
 *   judicial categories (e.g., obscenity, incitement, true threats) and a
 *   case-by-case balancing of speech value against potential harm. This
 *   reading grants significant interpretive power to the judiciary, allowing
 *   for flexible adaptation but at the cost of legal predictability and
 *   potential suppression of minority viewpoints. It is one of several
 *   competing interpretations of the First Amendment's scope.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(first_amendment_speech_protection__categorical_balancing_reading, 0.65).
domain_priors:suppression_score(first_amendment_speech_protection__categorical_balancing_reading, 0.7).
domain_priors:theater_ratio(first_amendment_speech_protection__categorical_balancing_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(first_amendment_speech_protection__categorical_balancing_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(first_amendment_speech_protection__categorical_balancing_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(first_amendment_speech_protection__categorical_balancing_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(first_amendment_speech_protection__categorical_balancing_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(first_amendment_speech_protection__categorical_balancing_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(first_amendment_speech_protection__categorical_balancing_reading, tangled_rope).
narrative_ontology:human_readable(first_amendment_speech_protection__categorical_balancing_reading, "First Amendment Speech Protection: Categorical Balancing Reading").
narrative_ontology:topic_domain(first_amendment_speech_protection__categorical_balancing_reading, "constitutional_law/political_philosophy/speech_regulation").

domain_priors:requires_active_enforcement(first_amendment_speech_protection__categorical_balancing_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(first_amendment_speech_protection__categorical_balancing_reading, 'f3cf0e62-ff27-4e86-b652-3d9b4b59edfb').
narrative_ontology:cs_kernel_codification('f3cf0e62-ff27-4e86-b652-3d9b4b59edfb', fixed_text).
narrative_ontology:cs_authority_grounding('f3cf0e62-ff27-4e86-b652-3d9b4b59edfb', lineage).
narrative_ontology:cs_interpretation_layer_present('f3cf0e62-ff27-4e86-b652-3d9b4b59edfb').
narrative_ontology:cs_reading_relation('f3cf0e62-ff27-4e86-b652-3d9b4b59edfb', first_amendment_speech_protection__absolutist_reading, coexists_with).
narrative_ontology:cs_reading_relation('f3cf0e62-ff27-4e86-b652-3d9b4b59edfb', first_amendment_speech_protection__harm_limited_reading, coexists_with).
narrative_ontology:cs_axiom('f3cf0e62-ff27-4e86-b652-3d9b4b59edfb', foundational, speech_value_vs_harm_balancing_is_necessary).
narrative_ontology:cs_axiom_status(speech_value_vs_harm_balancing_is_necessary, holdable).
narrative_ontology:cs_axiom_grounding('f3cf0e62-ff27-4e86-b652-3d9b4b59edfb', speech_value_vs_harm_balancing_is_necessary, conventional).
narrative_ontology:cs_axiom('f3cf0e62-ff27-4e86-b652-3d9b4b59edfb', foundational, judicial_discretion_in_category_definition_is_legitimate).
narrative_ontology:cs_axiom_status(judicial_discretion_in_category_definition_is_legitimate, holdable).
narrative_ontology:cs_axiom_grounding('f3cf0e62-ff27-4e86-b652-3d9b4b59edfb', judicial_discretion_in_category_definition_is_legitimate, conventional).
narrative_ontology:cs_reference_frame('f3cf0e62-ff27-4e86-b652-3d9b4b59edfb', evolving_judicial_precedent).
narrative_ontology:cs_drift_state('f3cf0e62-ff27-4e86-b652-3d9b4b59edfb', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('f3cf0e62-ff27-4e86-b652-3d9b4b59edfb', '').
narrative_ontology:cs_kernel_id(first_amendment_speech_protection__categorical_balancing_reading, first_amendment_speech_protection).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(first_amendment_speech_protection__categorical_balancing_reading, institutional_judiciary).
narrative_ontology:constraint_beneficiary(first_amendment_speech_protection__categorical_balancing_reading, majority_public_opinion).
narrative_ontology:constraint_victim(first_amendment_speech_protection__categorical_balancing_reading, minority_speech_advocates).
narrative_ontology:constraint_victim(first_amendment_speech_protection__categorical_balancing_reading, legal_predictability).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The primary interpreter and enforcer of First Amendment speech categories. Benefits from maintaining interpretive control and the flexibility to adapt categories to evolving social norms and perceived harms. Their professional identity is fused with this interpretive role.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__categorical_balancing_reading, institutional_judiciary, agenda_setter,
    institutional, generational, identity_locked, national).

% Benefits from the judiciary's ability to exclude speech deemed harmful or offensive (e.g., obscenity, incitement), aligning speech regulation with prevailing social values. This provides a sense of order and protection from perceived excesses of speech.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__categorical_balancing_reading, majority_public_opinion, beneficiary,
    organized, biographical, constrained, national).

% Often find their speech falling into 'unprotected' categories or facing greater scrutiny under the balancing test, leading to suppression of dissenting or unpopular viewpoints. They bear the cost of legal challenges and the chilling effect of uncertain protection.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__categorical_balancing_reading, minority_speech_advocates, payer,
    moderate, biographical, constrained, local).

% Suffers from the case-by-case nature of categorical balancing, making it difficult for speakers to know in advance whether their expression will be protected. This uncertainty acts as a cost, particularly for those with fewer resources to litigate.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__categorical_balancing_reading, legal_predictability, payer,
    powerless, immediate, trapped, national).
narrative_ontology:stakeholder_non_agent(first_amendment_speech_protection__categorical_balancing_reading, legal_predictability).

% Argue that the categorical balancing approach fundamentally misunderstands the First Amendment's text and intent, advocating for a near-absolute protection of speech. Their arguments are often considered in academic discourse but rarely adopted by the judiciary.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__categorical_balancing_reading, absolutist_scholars, excluded,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the boundaries of permissible speech by providing a framework for judicial review, allowing society to balance free expression against other values like public order, safety, and civility.
% TRANSFER_FUNCTION: Transfers interpretive authority over speech boundaries from the text or individual conscience to the institutional judiciary, which then allocates protection or suppression based on evolving categories and balancing tests.
% ABSENT_VOICES: Those whose speech is consistently deemed 'unprotected' or marginalized by the categorical balancing approach, particularly minority groups or radical speakers, are effectively excluded from shaping the interpretive framework. They would advocate for broader, more consistent protection.
% DISAPPEARANCE_RATIONALE: If this reading vanished, the legal landscape of speech would immediately shift. Either a more absolutist or a more harm-focused reading would likely take its place, fundamentally altering what speech is permissible and how it is regulated, leading to significant legal and social reorganization.
% FOUNDING_PROBLEM: To reconcile the broad language of the First Amendment with the perceived need to regulate certain types of speech (e.g., incitement to violence, obscenity) that society deemed harmful or without social value.
% FOUNDING_PROBLEM_CORROBORATION: The institutional judiciary and a significant portion of the public continue to attest that the problem of balancing speech against harm is live and requires ongoing judicial interpretation. Critics, however, argue that the 'problem' is often a pretext for suppressing unpopular views, a claim supported by historical patterns of speech suppression against marginalized groups.
narrative_ontology:disappearance_verdict(first_amendment_speech_protection__categorical_balancing_reading, world_rearranges).
narrative_ontology:founding_problem_status(first_amendment_speech_protection__categorical_balancing_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(first_amendment_speech_protection__categorical_balancing_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(first_amendment_speech_protection__categorical_balancing_reading, 'none', 1).
narrative_ontology:epsilon_provenance(first_amendment_speech_protection__categorical_balancing_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(first_amendment_speech_protection__categorical_balancing_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(first_amendment_speech_protection__categorical_balancing_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(first_amendment_speech_protection__categorical_balancing_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.65) is substantial because the judicial balancing often results in the suppression of speech, particularly from marginalized groups, under the guise of 'unprotected' categories. Suppression (0.70) is high due to the active enforcement by courts and the chilling effect of uncertain legal boundaries. Theater ratio (0.20) is moderate; while the balancing process is genuinely applied, it can sometimes serve to legitimize outcomes driven by majoritarian preferences rather than neutral principles. The claimed type is 'tangled_rope' because it genuinely coordinates the boundaries of speech but does so with significant asymmetric extraction and requires active judicial enforcement.
 *
 * PERSPECTIVAL GAP:
 *   From the judiciary's perspective, this is a necessary and legitimate coordination mechanism for a complex society. From the perspective of those whose speech is suppressed or who seek clear legal guidance, it is an extractive and unpredictable system that favors established power. The engine's per-seat classification will reflect this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   The institutional judiciary is the primary beneficiary, gaining interpretive control and flexibility (low d). Majority public opinion also benefits from the exclusion of 'harmful' speech. Minority speech advocates and legal predictability are the primary victims, bearing the costs of suppression and uncertainty (high d). Absolutist scholars are excluded, their alternative readings marginalized from the dominant interpretive practice.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    judicial_category_drift,
    'Are the ''unprotected'' speech categories (e.g., incitement, obscenity) stable, or do they drift over time to reflect changing social norms and political pressures?',
    'Longitudinal analysis of Supreme Court jurisprudence: track the evolution of categorical definitions and the types of speech included/excluded over several decades. Compare judicial reasoning to shifts in public opinion and political climate.',
    'If categories drift significantly, it suggests the constraint is more a reflection of judicial discretion and social power than fixed principles, increasing its effective extractiveness and suppression for disfavored speech. If stable, it supports the claim of principled judicial balancing.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(judicial_category_drift, empirical, 'Stability vs. fluidity of judicially defined unprotected speech categories.').

omega_variable(
    balancing_test_objectivity,
    'To what extent is the ''balancing'' of speech value against harm an objective legal exercise, versus a subjective reflection of judicial preferences or majoritarian values?',
    'Empirical studies of judicial decision-making, comparing outcomes in similar cases with different judicial panels or across different political eras. Analysis of dissenting opinions for consistent patterns of disagreement on balancing factors.',
    'If balancing is highly subjective, it undermines the legitimacy of the coordination function and increases the perceived extractiveness for those whose speech is disfavored. If objective, it strengthens the ''rope'' aspect of the constraint.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(balancing_test_objectivity, conceptual, 'Objectivity vs. subjectivity in judicial balancing of speech interests.').

omega_variable(
    chilling_effect_quantification,
    'What is the measurable chilling effect of the categorical balancing approach on speakers, particularly those with limited resources or unpopular views?',
    'Surveys of speakers and organizations, analysis of self-censorship patterns, and comparison of speech output in jurisdictions with different First Amendment interpretations. Case studies of individuals who chose not to speak due to legal uncertainty.',
    'A high chilling effect would demonstrate that the constraint''s suppression is more pervasive than direct enforcement actions suggest, increasing its effective suppression and extractiveness. A low effect would support the claim that the system provides sufficient clarity.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(chilling_effect_quantification, empirical, 'Quantification of the chilling effect on speech due to legal uncertainty.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(first_amendment_speech_protection__categorical_balancing_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(firs_tr_t0, first_amendment_speech_protection__categorical_balancing_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(firs_tr_t10, first_amendment_speech_protection__categorical_balancing_reading, theater_ratio, 10, 0.12).
narrative_ontology:measurement(firs_tr_t20, first_amendment_speech_protection__categorical_balancing_reading, theater_ratio, 20, 0.15).
narrative_ontology:measurement(firs_tr_t30, first_amendment_speech_protection__categorical_balancing_reading, theater_ratio, 30, 0.18).
narrative_ontology:measurement(firs_tr_t40, first_amendment_speech_protection__categorical_balancing_reading, theater_ratio, 40, 0.19).
narrative_ontology:measurement(firs_tr_t50, first_amendment_speech_protection__categorical_balancing_reading, theater_ratio, 50, 0.2).

% Extraction over time
narrative_ontology:measurement(firs_be_t0, first_amendment_speech_protection__categorical_balancing_reading, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(firs_be_t10, first_amendment_speech_protection__categorical_balancing_reading, base_extractiveness, 10, 0.55).
narrative_ontology:measurement(firs_be_t20, first_amendment_speech_protection__categorical_balancing_reading, base_extractiveness, 20, 0.6).
narrative_ontology:measurement(firs_be_t30, first_amendment_speech_protection__categorical_balancing_reading, base_extractiveness, 30, 0.63).
narrative_ontology:measurement(firs_be_t40, first_amendment_speech_protection__categorical_balancing_reading, base_extractiveness, 40, 0.64).
narrative_ontology:measurement(firs_be_t50, first_amendment_speech_protection__categorical_balancing_reading, base_extractiveness, 50, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(firs_su_t0, first_amendment_speech_protection__categorical_balancing_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(firs_su_t10, first_amendment_speech_protection__categorical_balancing_reading, suppression_requirement, 10, 0.6).
narrative_ontology:measurement(firs_su_t20, first_amendment_speech_protection__categorical_balancing_reading, suppression_requirement, 20, 0.65).
narrative_ontology:measurement(firs_su_t30, first_amendment_speech_protection__categorical_balancing_reading, suppression_requirement, 30, 0.68).
narrative_ontology:measurement(firs_su_t40, first_amendment_speech_protection__categorical_balancing_reading, suppression_requirement, 40, 0.69).
narrative_ontology:measurement(firs_su_t50, first_amendment_speech_protection__categorical_balancing_reading, suppression_requirement, 50, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(first_amendment_speech_protection__categorical_balancing_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(first_amendment_speech_protection__categorical_balancing_reading, first_amendment_speech_protection__absolutist_reading).
narrative_ontology:affects_constraint(first_amendment_speech_protection__categorical_balancing_reading, first_amendment_speech_protection__harm_limited_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'first_amendment_speech_protection' kernel. It is linked to the 'absolutist_reading' and 'harm_limited_reading' as sibling interpretations of the same constitutional text.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
