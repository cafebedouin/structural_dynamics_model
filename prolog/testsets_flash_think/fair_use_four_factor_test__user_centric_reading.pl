% ============================================================================
% CONSTRAINT STORY: fair_use_four_factor_test__user_centric_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_fair_use_four_factor_test__user_centric_reading, []).

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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
 *   constraint_id: fair_use_four_factor_test__user_centric_reading
 *   human_readable: Fair Use Four-Factor Test (User-Centric Reading)
 *   domain: legal/intellectual_property/cultural_production
 *
 * SUMMARY:
 *   This constraint story instantiates a 'user-centric' reading of the fair
 *   use four-factor test, emphasizing its role as an affirmative right for
 *   users to access and build upon copyrighted works for public benefit and
 *   cultural production. This reading prioritizes public access, education,
 *   and new creation, viewing the four factors as tools to achieve this
 *   balance. It acknowledges that this comes at a cost to copyright holders,
 *   who are the primary targets of extraction, but frames this extraction as
 *   a necessary component of a healthy public domain and creative ecosystem.
 *   The constraint is claimed as a Tangled Rope due to its genuine
 *   coordination function (balancing interests, enabling cultural flow)
 *   coupled with asymmetric extraction from rights holders, maintained
 *   through active judicial enforcement.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fair_use_four_factor_test__user_centric_reading, 0.65).
domain_priors:suppression_score(fair_use_four_factor_test__user_centric_reading, 0.7).
domain_priors:theater_ratio(fair_use_four_factor_test__user_centric_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fair_use_four_factor_test__user_centric_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(fair_use_four_factor_test__user_centric_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(fair_use_four_factor_test__user_centric_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(fair_use_four_factor_test__user_centric_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(fair_use_four_factor_test__user_centric_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(fair_use_four_factor_test__user_centric_reading, tangled_rope).
narrative_ontology:human_readable(fair_use_four_factor_test__user_centric_reading, "Fair Use Four-Factor Test (User-Centric Reading)").
narrative_ontology:topic_domain(fair_use_four_factor_test__user_centric_reading, "legal/intellectual_property/cultural_production").

domain_priors:requires_active_enforcement(fair_use_four_factor_test__user_centric_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(fair_use_four_factor_test__user_centric_reading, '53f1e72e-6c57-4dbb-9a51-b9b1de7bb706').
narrative_ontology:cs_kernel_codification('53f1e72e-6c57-4dbb-9a51-b9b1de7bb706', fixed_text).
narrative_ontology:cs_authority_grounding('53f1e72e-6c57-4dbb-9a51-b9b1de7bb706', lineage).
narrative_ontology:cs_interpretation_layer_present('53f1e72e-6c57-4dbb-9a51-b9b1de7bb706').
narrative_ontology:cs_reading_relation('53f1e72e-6c57-4dbb-9a51-b9b1de7bb706', fair_use_four_factor_test__creator_centric_reading, coexists_with).
narrative_ontology:cs_reading_relation('53f1e72e-6c57-4dbb-9a51-b9b1de7bb706', fair_use_four_factor_test__transformative_use_reading, coexists_with).
narrative_ontology:cs_axiom('53f1e72e-6c57-4dbb-9a51-b9b1de7bb706', foundational, public_access_priority).
narrative_ontology:cs_axiom_status(public_access_priority, holdable).
narrative_ontology:cs_axiom_grounding('53f1e72e-6c57-4dbb-9a51-b9b1de7bb706', public_access_priority, deontological).
narrative_ontology:cs_axiom('53f1e72e-6c57-4dbb-9a51-b9b1de7bb706', secondary, cultural_commons_enrichment).
narrative_ontology:cs_axiom_status(cultural_commons_enrichment, holdable).
narrative_ontology:cs_axiom_grounding('53f1e72e-6c57-4dbb-9a51-b9b1de7bb706', cultural_commons_enrichment, instrumental).
narrative_ontology:cs_reference_frame('53f1e72e-6c57-4dbb-9a51-b9b1de7bb706', public_interest_balancing).
narrative_ontology:cs_drift_state('53f1e72e-6c57-4dbb-9a51-b9b1de7bb706', digital_age_application, gap(stable, minor, true)).
narrative_ontology:cs_created_at('53f1e72e-6c57-4dbb-9a51-b9b1de7bb706', '').
narrative_ontology:cs_kernel_id(fair_use_four_factor_test__user_centric_reading, fair_use_four_factor_test).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(fair_use_four_factor_test__user_centric_reading, public_users).
narrative_ontology:constraint_beneficiary(fair_use_four_factor_test__user_centric_reading, educational_institutions).
narrative_ontology:constraint_beneficiary(fair_use_four_factor_test__user_centric_reading, cultural_producers).
narrative_ontology:constraint_victim(fair_use_four_factor_test__user_centric_reading, copyright_holders).
narrative_ontology:constraint_vindicates(fair_use_four_factor_test__user_centric_reading, public_domain_enrichment_doctrine).
narrative_ontology:constraint_vindicates(fair_use_four_factor_test__user_centric_reading, free_speech_principles).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefit from the ability to access, use, and build upon copyrighted works for purposes like commentary, criticism, news reporting, teaching, scholarship, or research without needing explicit permission or payment. Their exit options are constrained by the need to engage with copyrighted material in daily life.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__user_centric_reading, public_users, beneficiary,
    moderate, biographical, constrained, global).

% Rely on fair use to incorporate copyrighted materials into curricula, research, and teaching without prohibitive licensing costs. Their mission is directly supported by the flexibility fair use provides, but they are constrained by legal interpretations and potential litigation.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__user_centric_reading, educational_institutions, beneficiary,
    organized, generational, constrained, national).

% Artists, writers, filmmakers, and other creators who build upon existing works (e.g., parody, sampling, remixing) benefit from fair use as a defense against infringement claims, enabling new cultural production. Their ability to create is constrained by the boundaries of fair use.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__user_centric_reading, cultural_producers, beneficiary,
    moderate, biographical, constrained, global).

% Bear the cost of fair use by losing some exclusive control over their copyrighted works and potentially some revenue from licensing. They are constrained by the legal framework that permits certain unauthorized uses, limiting their ability to fully monetize or control their creations.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__user_centric_reading, copyright_holders, payer,
    powerful, generational, constrained, global).

% Interpret and apply the four-factor test in specific cases, shaping the boundaries and application of fair use. They are the primary enforcers and adjudicators of this constraint, balancing competing interests within the legal framework.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__user_centric_reading, courts, agenda_setter,
    institutional, civilizational, analytical, national).

% Analyze and critique judicial decisions, propose theoretical frameworks, and influence the evolution of fair use doctrine. They provide an analytical perspective on the constraint's operation and impact.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__user_centric_reading, legal_scholars, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(fair_use_four_factor_test__user_centric_reading, diffuse).
narrative_ontology:fixing_cost_class(fair_use_four_factor_test__user_centric_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the balance between copyright holders' exclusive rights and the public's interest in accessing and building upon creative works, facilitating cultural production and public discourse.
% TRANSFER_FUNCTION: Transfers a limited degree of control and potential licensing revenue from copyright holders to the public and subsequent creators, enabling unauthorized uses for specific purposes.
% ABSENT_VOICES: Creators who advocate for absolute control over their works and believe all unauthorized use is theft; users who believe all cultural material should be freely available without any restrictions. Both are often excluded from the direct judicial balancing act, though their arguments influence legal discourse.
% DISAPPEARANCE_RATIONALE: If fair use vanished overnight, public access to copyrighted works for commentary, education, and parody would be severely curtailed. Cultural production that relies on building upon existing works would require extensive, often impossible, licensing, stifling creativity and public discourse. The digital economy, particularly platforms relying on user-generated content, would face immense legal challenges.
% FOUNDING_PROBLEM: The problem of how to prevent copyright from becoming an absolute monopoly that stifles creativity and public access, while still incentivizing creators. It seeks to balance the exclusive rights granted to authors with the broader public interest in the dissemination of knowledge and culture.
% FOUNDING_PROBLEM_CORROBORATION: Legal scholars, educators, public interest groups, and cultural organizations consistently attest to the ongoing necessity of fair use to prevent copyright overreach, promote cultural flourishing, and protect free speech. This corroboration comes from outside the direct benefiting parties (e.g., individual users) and is supported by extensive academic literature and policy debates.
narrative_ontology:disappearance_verdict(fair_use_four_factor_test__user_centric_reading, world_rearranges).
narrative_ontology:founding_problem_status(fair_use_four_factor_test__user_centric_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(fair_use_four_factor_test__user_centric_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(fair_use_four_factor_test__user_centric_reading, 'none', 1).
narrative_ontology:epsilon_provenance(fair_use_four_factor_test__user_centric_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(fair_use_four_factor_test__user_centric_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(fair_use_four_factor_test__user_centric_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(fair_use_four_factor_test__user_centric_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.65) is moderate-to-high because the constraint actively permits uses that would otherwise require licensing, thereby extracting control and potential revenue from copyright holders. Suppression (0.70) is also high, as it suppresses the ability of rights holders to fully control and monetize their works. The theater ratio (0.15) is low because the judicial application of fair use is a substantive legal process, not primarily performative. Accessibility collapse (0.50) is moderate; while fair use opens up alternatives for users, it also limits the absolute control of rights holders. Resistance (0.40) is moderate, primarily from copyright holders who frequently litigate to narrow fair use interpretations. The temporal measurements show a slight increase in extractiveness and suppression over time, reflecting a gradual expansion of fair use applications, particularly in the digital age.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of public users and cultural producers, fair use is a vital right that enables their activities and enriches society, making it feel like a Rope or even a Mountain (a fundamental right). From the perspective of copyright holders, it is an infringement on their property rights, an extraction that diminishes their control and revenue, making it feel like a Snare. The engine's classification as Tangled Rope captures this inherent tension and asymmetric experience.
 *
 * DIRECTIONALITY LOGIC:
 *   Public users, educational institutions, and cultural producers are beneficiaries (low directionality) as fair use grants them rights and reduces their costs. Copyright holders are victims/payers (high directionality) as the constraint limits their exclusive rights and potential revenue. Courts act as agenda-setters, interpreting and enforcing the balance. Legal scholars observe and analyze the system.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    optimal_balance_point,
    'What is the optimal balance point between incentivizing creators and promoting public access/cultural production, and does the current application of fair use achieve it?',
    'Longitudinal empirical studies on creator output and public engagement under varying fair use interpretations, combined with economic modeling of incentive structures.',
    'If the current balance is suboptimal, it could suggest that the constraint''s extractiveness from rights holders is either too high (disincentivizing creation) or too low (stifling public access), leading to calls for legislative or judicial recalibration.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(optimal_balance_point, empirical, 'Uncertainty regarding the ideal equilibrium between creator rights and public benefit.').

omega_variable(
    digital_age_applicability,
    'How effectively does the traditional four-factor test, developed in an analog era, apply to and govern uses of copyrighted material in the digital and AI-driven age?',
    'Analysis of judicial consistency and outcomes in digital fair use cases, and legislative efforts to update copyright law for new technologies.',
    'If the test is found to be poorly adapted, it could lead to increased legal uncertainty, higher litigation costs, and potentially a reclassification of the constraint as a Piton (if its function atrophies) or a Snare (if it becomes a tool for arbitrary enforcement).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(digital_age_applicability, conceptual, 'The challenge of applying an analog-era legal framework to digital and AI-driven cultural production.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fair_use_four_factor_test__user_centric_reading, 1976, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fair_tr_t1976, fair_use_four_factor_test__user_centric_reading, theater_ratio, 1976, 0.1).
narrative_ontology:measurement(fair_tr_t1990, fair_use_four_factor_test__user_centric_reading, theater_ratio, 1990, 0.12).
narrative_ontology:measurement(fair_tr_t2000, fair_use_four_factor_test__user_centric_reading, theater_ratio, 2000, 0.15).
narrative_ontology:measurement(fair_tr_t2010, fair_use_four_factor_test__user_centric_reading, theater_ratio, 2010, 0.15).
narrative_ontology:measurement(fair_tr_t2020, fair_use_four_factor_test__user_centric_reading, theater_ratio, 2020, 0.15).
narrative_ontology:measurement(fair_tr_t2024, fair_use_four_factor_test__user_centric_reading, theater_ratio, 2024, 0.15).

% Extraction over time
narrative_ontology:measurement(fair_be_t1976, fair_use_four_factor_test__user_centric_reading, base_extractiveness, 1976, 0.55).
narrative_ontology:measurement(fair_be_t1990, fair_use_four_factor_test__user_centric_reading, base_extractiveness, 1990, 0.6).
narrative_ontology:measurement(fair_be_t2000, fair_use_four_factor_test__user_centric_reading, base_extractiveness, 2000, 0.62).
narrative_ontology:measurement(fair_be_t2010, fair_use_four_factor_test__user_centric_reading, base_extractiveness, 2010, 0.64).
narrative_ontology:measurement(fair_be_t2020, fair_use_four_factor_test__user_centric_reading, base_extractiveness, 2020, 0.65).
narrative_ontology:measurement(fair_be_t2024, fair_use_four_factor_test__user_centric_reading, base_extractiveness, 2024, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(fair_su_t1976, fair_use_four_factor_test__user_centric_reading, suppression_requirement, 1976, 0.6).
narrative_ontology:measurement(fair_su_t1990, fair_use_four_factor_test__user_centric_reading, suppression_requirement, 1990, 0.65).
narrative_ontology:measurement(fair_su_t2000, fair_use_four_factor_test__user_centric_reading, suppression_requirement, 2000, 0.68).
narrative_ontology:measurement(fair_su_t2010, fair_use_four_factor_test__user_centric_reading, suppression_requirement, 2010, 0.7).
narrative_ontology:measurement(fair_su_t2020, fair_use_four_factor_test__user_centric_reading, suppression_requirement, 2020, 0.7).
narrative_ontology:measurement(fair_su_t2024, fair_use_four_factor_test__user_centric_reading, suppression_requirement, 2024, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(fair_use_four_factor_test__user_centric_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(fair_use_four_factor_test__user_centric_reading, fair_use_four_factor_test__creator_centric_reading).
narrative_ontology:affects_constraint(fair_use_four_factor_test__user_centric_reading, fair_use_four_factor_test__transformative_use_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three distinct readings of the 'fair_use_four_factor_test' kernel, each with different structural properties and classifications. They are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
