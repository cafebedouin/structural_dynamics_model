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
 *   constraint_id: fair_use_four_factor_test__transformative_use_reading
 *   human_readable: Fair Use Four-Factor Test: Transformative Use Reading
 *   domain: legal/intellectual_property/cultural_production
 *
 * SUMMARY:
 *   This constraint represents the 'transformative use' reading of the
 *   four-factor fair use test in US copyright law. Under this reading, the
 *   first factor (purpose and character of the use, including whether such
 *   use is of a commercial nature or is for nonprofit educational purposes)
 *   is heavily weighted towards whether the new work 'adds something new,
 *   with a further purpose or different character, altering the first with
 *   new expression, meaning, or message.' When a use is deemed
 *   transformative, it often subordinates the fourth factor (effect of the
 *   use upon the potential market for or value of the copyrighted work), even
 *   if there is some market harm. This interpretation has significantly
 *   shaped digital culture and the legal landscape for user-generated
 *   content.
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
narrative_ontology:constraint_metric(fair_use_four_factor_test__transformative_use_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(fair_use_four_factor_test__transformative_use_reading, tangled_rope).
narrative_ontology:human_readable(fair_use_four_factor_test__transformative_use_reading, "Fair Use Four-Factor Test: Transformative Use Reading").
narrative_ontology:topic_domain(fair_use_four_factor_test__transformative_use_reading, "legal/intellectual_property/cultural_production").

domain_priors:requires_active_enforcement(fair_use_four_factor_test__transformative_use_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(fair_use_four_factor_test__transformative_use_reading, '5395fc89-bcfd-4daa-8cb1-387a9475bd84').
narrative_ontology:cs_kernel_codification('5395fc89-bcfd-4daa-8cb1-387a9475bd84', fixed_text).
narrative_ontology:cs_authority_grounding('5395fc89-bcfd-4daa-8cb1-387a9475bd84', lineage).
narrative_ontology:cs_interpretation_layer_present('5395fc89-bcfd-4daa-8cb1-387a9475bd84').
narrative_ontology:cs_reading_relation('5395fc89-bcfd-4daa-8cb1-387a9475bd84', fair_use_four_factor_test__creator_centric_reading, coexists_with).
narrative_ontology:cs_reading_relation('5395fc89-bcfd-4daa-8cb1-387a9475bd84', fair_use_four_factor_test__user_centric_reading, coexists_with).
narrative_ontology:cs_axiom('5395fc89-bcfd-4daa-8cb1-387a9475bd84', foundational, new_meaning_trumps_market_harm).
narrative_ontology:cs_axiom_status(new_meaning_trumps_market_harm, holdable).
narrative_ontology:cs_axiom_grounding('5395fc89-bcfd-4daa-8cb1-387a9475bd84', new_meaning_trumps_market_harm, conventional).
narrative_ontology:cs_axiom('5395fc89-bcfd-4daa-8cb1-387a9475bd84', secondary, cultural_progress_requires_remix).
narrative_ontology:cs_axiom_status(cultural_progress_requires_remix, holdable).
narrative_ontology:cs_axiom_grounding('5395fc89-bcfd-4daa-8cb1-387a9475bd84', cultural_progress_requires_remix, instrumental).
narrative_ontology:cs_reference_frame('5395fc89-bcfd-4daa-8cb1-387a9475bd84', transformative_use_paradigm).
narrative_ontology:cs_drift_state('5395fc89-bcfd-4daa-8cb1-387a9475bd84', contemporary_ai_era, gap(practice_drift, minor, true)).
narrative_ontology:cs_created_at('5395fc89-bcfd-4daa-8cb1-387a9475bd84', '').
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

% Benefit from the broad interpretation of 'transformative use,' allowing them to create new works from existing copyrighted material without needing explicit licenses, fostering a vibrant remix culture. Their exit options are constrained by the need for legal certainty and the high cost of litigation if challenged.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__transformative_use_reading, remix_artists, beneficiary,
    moderate, biographical, constrained, global).

% Benefit from the transformative use doctrine by hosting vast amounts of user-generated content that often incorporates copyrighted material, reducing their liability and enabling their business models. They have significant legal resources to defend this interpretation.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__transformative_use_reading, user_generated_content_platforms, beneficiary,
    institutional, generational, arbitrage, global).

% Bear the cost of diminished control over their works and potential market dilution when transformative uses are permitted without compensation. They often face high legal costs to challenge such uses, making exit from the system difficult.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__transformative_use_reading, original_content_creators, payer,
    powerful, biographical, constrained, global).

% Experience reduced licensing revenue and market control due to the expansive interpretation of transformative use. They are forced to adapt business models or engage in costly litigation to protect their rights.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__transformative_use_reading, traditional_publishers, payer,
    institutional, generational, constrained, global).

% Are the primary interpreters and enforcers of the fair use doctrine, shaping its application through case law. Their decisions determine the balance between creator rights and user freedoms, with significant influence on cultural production.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__transformative_use_reading, courts_and_judges, agenda_setter,
    institutional, generational, analytical, national).

% Analyze and critique the evolving interpretation of fair use, influencing legal discourse and policy debates. They provide the theoretical frameworks that courts and policymakers draw upon.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__transformative_use_reading, legal_scholars, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Balances the exclusive rights of copyright holders with the public interest in promoting creativity, scholarship, and free expression, allowing for certain unlicensed uses of copyrighted material.
% TRANSFER_FUNCTION: Transfers the right to use copyrighted material without permission or compensation from original creators to those who create new, transformative works, often benefiting platforms that host such content.
% ABSENT_VOICES: Small, independent creators who lack the resources to litigate against transformative uses of their work are often effectively excluded from the conversation, bearing costs without a voice in shaping the doctrine.
% DISAPPEARANCE_RATIONALE: If the transformative use reading vanished, the landscape of cultural production, especially remix culture and user-generated content, would drastically change. Platforms would face immense liability, and many creative works would cease to exist or require prohibitive licensing, fundamentally altering how culture is produced and consumed.
% FOUNDING_PROBLEM: To provide a flexible defense against copyright infringement claims, allowing for uses that promote the progress of science and useful arts, even if they involve copyrighted material.
% FOUNDING_PROBLEM_CORROBORATION: Legal scholars and cultural commentators, in addition to the courts, corroborate that the problem of balancing creator rights with public benefit remains live, though the specific interpretation of that balance is highly contested.
narrative_ontology:disappearance_verdict(fair_use_four_factor_test__transformative_use_reading, world_rearranges).
narrative_ontology:founding_problem_status(fair_use_four_factor_test__transformative_use_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(fair_use_four_factor_test__transformative_use_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
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
 *   The extractiveness (0.45) is moderate because while it allows for unlicensed use, it also fosters new creative markets. Suppression (0.3) is relatively low, as the doctrine aims to enable, rather than restrict, certain uses, though original creators feel suppressed in their ability to control their work. Theater ratio (0.1) is low, indicating the doctrine is actively applied and serves a genuine function, not merely performative. The claimed type is 'tangled_rope' because it genuinely coordinates cultural production while simultaneously extracting value (control, potential revenue) from original creators for the benefit of transformative users and platforms.
 *
 * PERSPECTIVAL GAP:
 *   Original creators perceive this reading as highly extractive, eroding their property rights. Transformative users and platforms see it as a necessary coordination mechanism for modern cultural production. The engine's classification will reflect this divergence based on the structural positions of the stakeholders.
 *
 * DIRECTIONALITY LOGIC:
 *   Remix artists and UGC platforms are clear beneficiaries, as the constraint enables their creative and business models. Original content creators and traditional publishers are payers, as their exclusive rights are curtailed. Courts act as agenda-setters, interpreting and enforcing the doctrine. Legal scholars observe and influence the debate.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    transformative_threshold_ambiguity,
    'What constitutes ''transformative'' use, and how consistently is this threshold applied across different courts and technologies?',
    'Empirical analysis of court decisions over time, identifying consistent patterns or divergences in ''transformative'' rulings, especially concerning AI-generated content.',
    'If the threshold is highly ambiguous or inconsistently applied, it increases legal uncertainty, raising transaction costs for both creators and users, potentially shifting the constraint towards a Snare for those unable to bear litigation costs.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(transformative_threshold_ambiguity, empirical, 'Ambiguity in the definition and application of ''transformative'' use.').

omega_variable(
    market_harm_subordination_justification,
    'Is the subordination of market harm in transformative use cases genuinely promoting new markets and public benefit, or is it primarily subsidizing platforms and new creators at the expense of original creators?',
    'Economic studies analyzing the net impact of transformative use on overall cultural production, market growth, and creator compensation across different industries.',
    'If the primary effect is a subsidy to new creators/platforms without commensurate public benefit, the extractiveness of the constraint for original creators is higher than currently assessed, pushing it closer to a Snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(market_harm_subordination_justification, preference, 'Justification for subordinating market harm in transformative use.').


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

narrative_ontology:coordination_type(fair_use_four_factor_test__transformative_use_reading, identity_coordination).
narrative_ontology:affects_constraint(fair_use_four_factor_test__transformative_use_reading, copyright_term_extension).
narrative_ontology:affects_constraint(fair_use_four_factor_test__transformative_use_reading, digital_millennium_copyright_act).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'fair_use_four_factor_test' kernel. The other readings are 'creator_centric_reading' and 'user_centric_reading', each representing a distinct interpretation of fair use with different beneficiary/victim structures and extractiveness profiles.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
