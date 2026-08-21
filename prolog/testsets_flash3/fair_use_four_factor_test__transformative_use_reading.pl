% ============================================================================
% CONSTRAINT STORY: fair_use_four_factor_test__transformative_use_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_fair_use_four_factor_test__transformative_use_reading, []).

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
 *   constraint_id: fair_use_four_factor_test__transformative_use_reading
 *   human_readable: Fair Use Four-Factor Test: Transformative Use Reading
 *   domain: legal/intellectual_property/cultural_production
 *
 * SUMMARY:
 *   This constraint represents the 'transformative use' reading of the fair
 *   use four-factor test in US copyright law. Under this reading, the first
 *   factor (purpose and character of the use, including whether such use is
 *   of a commercial nature or is for nonprofit educational purposes) is
 *   heavily weighted towards whether the new work 'adds something new, with a
 *   further purpose or different character, altering the first with new
 *   expression, meaning, or message.' When a use is deemed transformative,
 *   its commercial nature or potential market harm to the original work is
 *   often subordinated. This reading has significantly expanded the scope of
 *   fair use, particularly benefiting remix culture and platforms hosting
 *   user-generated content, while shifting costs to original creators and
 *   traditional publishers.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fair_use_four_factor_test__transformative_use_reading, 0.45).
domain_priors:suppression_score(fair_use_four_factor_test__transformative_use_reading, 0.3).
domain_priors:theater_ratio(fair_use_four_factor_test__transformative_use_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fair_use_four_factor_test__transformative_use_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(fair_use_four_factor_test__transformative_use_reading, suppression_requirement, 0.3).
narrative_ontology:constraint_metric(fair_use_four_factor_test__transformative_use_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(fair_use_four_factor_test__transformative_use_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(fair_use_four_factor_test__transformative_use_reading, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(fair_use_four_factor_test__transformative_use_reading, tangled_rope).
narrative_ontology:human_readable(fair_use_four_factor_test__transformative_use_reading, "Fair Use Four-Factor Test: Transformative Use Reading").
narrative_ontology:topic_domain(fair_use_four_factor_test__transformative_use_reading, "legal/intellectual_property/cultural_production").

domain_priors:requires_active_enforcement(fair_use_four_factor_test__transformative_use_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(fair_use_four_factor_test__transformative_use_reading, '7dbf462b-068a-4161-917b-4bba4455d44e').
narrative_ontology:cs_kernel_codification('7dbf462b-068a-4161-917b-4bba4455d44e', fixed_text).
narrative_ontology:cs_authority_grounding('7dbf462b-068a-4161-917b-4bba4455d44e', lineage).
narrative_ontology:cs_interpretation_layer_present('7dbf462b-068a-4161-917b-4bba4455d44e').
narrative_ontology:cs_reading_relation('7dbf462b-068a-4161-917b-4bba4455d44e', fair_use_four_factor_test__creator_centric_reading, influences).
narrative_ontology:cs_reading_relation('7dbf462b-068a-4161-917b-4bba4455d44e', fair_use_four_factor_test__user_centric_reading, coexists_with).
narrative_ontology:cs_axiom('7dbf462b-068a-4161-917b-4bba4455d44e', foundational, new_meaning_trumps_market_harm).
narrative_ontology:cs_axiom_status(new_meaning_trumps_market_harm, holdable).
narrative_ontology:cs_axiom_grounding('7dbf462b-068a-4161-917b-4bba4455d44e', new_meaning_trumps_market_harm, conventional).
narrative_ontology:cs_axiom('7dbf462b-068a-4161-917b-4bba4455d44e', secondary, public_interest_in_transformation).
narrative_ontology:cs_axiom_status(public_interest_in_transformation, holdable).
narrative_ontology:cs_axiom_grounding('7dbf462b-068a-4161-917b-4bba4455d44e', public_interest_in_transformation, deontological).
narrative_ontology:cs_reference_frame('7dbf462b-068a-4161-917b-4bba4455d44e', judicial_balancing_of_factors).
narrative_ontology:cs_drift_state('7dbf462b-068a-4161-917b-4bba4455d44e', contemporary_digital_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('7dbf462b-068a-4161-917b-4bba4455d44e', '').
narrative_ontology:cs_kernel_id(fair_use_four_factor_test__transformative_use_reading, fair_use_four_factor_test).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(fair_use_four_factor_test__transformative_use_reading, remix_artists).
narrative_ontology:constraint_beneficiary(fair_use_four_factor_test__transformative_use_reading, user_generated_content_platforms).
narrative_ontology:constraint_victim(fair_use_four_factor_test__transformative_use_reading, original_content_creators).
narrative_ontology:constraint_victim(fair_use_four_factor_test__transformative_use_reading, traditional_publishers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefit from the broad interpretation of 'transformative use,' allowing them to create new works from existing copyrighted material without needing explicit permission or licensing. Their ability to operate depends heavily on this reading.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__transformative_use_reading, remix_artists, beneficiary,
    moderate, biographical, constrained, global).

% Benefit from reduced liability for hosting user-generated content that incorporates copyrighted material, as long as it can be argued to be transformative. This reading supports their business model and user engagement.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__transformative_use_reading, user_generated_content_platforms, beneficiary,
    institutional, generational, arbitrage, global).

% Bear the cost of diminished control over their copyrighted works when those works are used transformatively. They may see reduced market opportunities for derivative works or licensing, even if direct market harm is subordinated.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__transformative_use_reading, original_content_creators, payer,
    powerful, biographical, constrained, global).

% Experience erosion of their licensing revenue streams and control over derivative markets as transformative uses expand. They must actively enforce their rights, often through costly litigation, against uses that this reading protects.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__transformative_use_reading, traditional_publishers, payer,
    institutional, generational, constrained, global).

% Interpret and apply the four-factor test, with a strong emphasis on transformativeness. Their rulings shape the boundaries of fair use, balancing creator rights with public interest in new expression. They are the primary enforcers of this reading.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__transformative_use_reading, courts_and_judges, agenda_setter,
    institutional, generational, analytical, national).

% Analyze the evolution and impact of the transformative use doctrine, debating its consistency with statutory language and its effects on cultural production and innovation. They influence future legal interpretations.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__transformative_use_reading, legal_scholars, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the balance between copyright holders' exclusive rights and the public's interest in using copyrighted works for new creative expression, fostering cultural production and innovation.
% TRANSFER_FUNCTION: Transfers a degree of control and potential economic value from original content creators to those who create new, transformative works, often mediated by platforms that host such content.
% ABSENT_VOICES: Creators of 'niche' or 'cult' works, whose market for derivative licenses might be entirely supplanted by transformative uses, often lack the resources to litigate and are effectively excluded from shaping the doctrine's evolution.
% DISAPPEARANCE_RATIONALE: If the transformative use doctrine vanished, remix culture would largely cease, user-generated content platforms would face immense liability, and original content creators would regain much stricter control over their works, fundamentally altering digital cultural production.
% FOUNDING_PROBLEM: The original fair use doctrine struggled to adapt to new technologies and forms of creative expression (e.g., sampling, parody, digital collage) that built upon existing works but did not directly compete with them.
% FOUNDING_PROBLEM_CORROBORATION: Legal scholars and cultural critics attest that the problem of balancing creator rights with new forms of expression remains live, especially with AI-generated content. Original creators and publishers, however, argue the doctrine has overshot its original intent, becoming a loophole for commercial exploitation.
narrative_ontology:disappearance_verdict(fair_use_four_factor_test__transformative_use_reading, world_rearranges).
narrative_ontology:founding_problem_status(fair_use_four_factor_test__transformative_use_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(fair_use_four_factor_test__transformative_use_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(fair_use_four_factor_test__transformative_use_reading, 'none', 1).
narrative_ontology:epsilon_provenance(fair_use_four_factor_test__transformative_use_reading, 0.45, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(fair_use_four_factor_test__transformative_use_reading_tests).
:- end_tests(fair_use_four_factor_test__transformative_use_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.45) because while it enables new creation, it does so by diminishing the exclusive rights of original creators, effectively extracting a portion of their control and potential market value. Suppression is low (0.3) as it primarily operates through legal interpretation rather than overt coercion, but it does suppress the ability of original creators to control certain derivative markets. Theater ratio is low (0.1) because the legal process of determining transformativeness is genuinely functional, though sometimes complex. The trend shows a slight increase in extractiveness and suppression as the doctrine has expanded, indicating a gradual shift in the balance of power.
 *
 * PERSPECTIVAL GAP:
 *   Original creators often perceive this reading as a 'snare' that undermines their property rights, while remix artists and platforms see it as a 'rope' that enables innovation and free expression. The courts, as agenda-setters, aim for a 'tangled rope' balance, but the strong emphasis on transformativeness leans towards benefiting new uses at the expense of original control. The engine's classification will reflect this structural asymmetry.
 *
 * DIRECTIONALITY LOGIC:
 *   Remix artists and UGC platforms are clear beneficiaries, as this reading enables their creative and business models. Original content creators and traditional publishers are payers, bearing the costs of reduced control and market opportunities. Courts and judges act as agenda-setters, actively shaping and enforcing this interpretation. Legal scholars serve as observers, analyzing its impact.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    transformative_threshold_ambiguity,
    'What is the precise threshold for ''transformativeness'' that subordinates market harm, and is this threshold consistently applied across different types of works and industries?',
    'Empirical analysis of court decisions over time, categorizing cases by work type and outcome, and identifying consistent criteria for ''new meaning or message.''',
    'If the threshold is inconsistent or arbitrary, the constraint''s predictability and fairness are compromised, increasing litigation costs for all parties. If consistently high, it supports creator rights; if consistently low, it further benefits transformative users.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(transformative_threshold_ambiguity, empirical, 'Ambiguity in the application of the transformative use standard.').

omega_variable(
    market_harm_subordination_justification,
    'Is the subordination of market harm in transformative use cases justified by a net societal benefit in cultural production, or does it primarily benefit specific commercial actors (e.g., platforms)?',
    'Economic studies comparing the growth of transformative cultural production against the economic impact on original creators, disaggregated by type of creator and commercial vs. non-commercial use.',
    'If the benefit is primarily commercial for platforms, the constraint leans more towards extraction. If it genuinely fosters diverse cultural production, it supports the coordination function. This would shift the perceived extractiveness.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(market_harm_subordination_justification, preference, 'Normative justification for subordinating market harm.').

omega_variable(
    reading_as_interpretation_or_amendment,
    'Is the transformative use reading a legitimate interpretation of the original fair use statute, or has it effectively amended the statute through judicial activism?',
    'Legal historical analysis tracing the legislative intent of the 1976 Copyright Act and comparing it with the evolution of judicial precedent, alongside constitutional analysis of judicial power.',
    'If deemed an amendment, it raises questions about the legitimacy of the constraint''s origin and could fuel legislative efforts to codify or restrict the doctrine. If a legitimate interpretation, it reinforces the stability of the current legal framework.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_as_interpretation_or_amendment, conceptual, 'Conceptual status of the transformative use doctrine within copyright law.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fair_use_four_factor_test__transformative_use_reading, 1994, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fair_tr_t1994, fair_use_four_factor_test__transformative_use_reading, theater_ratio, 1994, 0.05).
narrative_ontology:measurement(fair_tr_t2004, fair_use_four_factor_test__transformative_use_reading, theater_ratio, 2004, 0.08).
narrative_ontology:measurement(fair_tr_t2014, fair_use_four_factor_test__transformative_use_reading, theater_ratio, 2014, 0.09).
narrative_ontology:measurement(fair_tr_t2024, fair_use_four_factor_test__transformative_use_reading, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(fair_be_t1994, fair_use_four_factor_test__transformative_use_reading, base_extractiveness, 1994, 0.3).
narrative_ontology:measurement(fair_be_t2004, fair_use_four_factor_test__transformative_use_reading, base_extractiveness, 2004, 0.38).
narrative_ontology:measurement(fair_be_t2014, fair_use_four_factor_test__transformative_use_reading, base_extractiveness, 2014, 0.42).
narrative_ontology:measurement(fair_be_t2024, fair_use_four_factor_test__transformative_use_reading, base_extractiveness, 2024, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(fair_su_t1994, fair_use_four_factor_test__transformative_use_reading, suppression_requirement, 1994, 0.2).
narrative_ontology:measurement(fair_su_t2004, fair_use_four_factor_test__transformative_use_reading, suppression_requirement, 2004, 0.25).
narrative_ontology:measurement(fair_su_t2014, fair_use_four_factor_test__transformative_use_reading, suppression_requirement, 2014, 0.28).
narrative_ontology:measurement(fair_su_t2024, fair_use_four_factor_test__transformative_use_reading, suppression_requirement, 2024, 0.3).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(fair_use_four_factor_test__transformative_use_reading, enforcement_mechanism).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'fair_use_four_factor_test' kernel. It emphasizes transformative use, contrasting with creator_centric_reading and user_centric_reading, which prioritize creator incentives and public access, respectively. Each reading instantiates a distinct constraint with different beneficiary/victim sets and extractiveness profiles.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
