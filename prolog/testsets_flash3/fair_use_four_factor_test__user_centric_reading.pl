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
 *   This constraint represents a 'user-centric' reading of the fair use
 *   four-factor test in US copyright law. In this reading, fair use is
 *   understood as an affirmative right of users to engage with copyrighted
 *   material for public benefit, prioritizing public access and cultural
 *   production over strict creator control. The four factors (purpose and
 *   character of the use, nature of the copyrighted work, amount and
 *   substantiality of the portion used, and effect of the use upon the
 *   potential market) are weighed with a strong bias towards allowing
 *   unauthorized uses that serve public interest, education, or
 *   transformative expression. This reading aims to keep extractiveness low
 *   for users and suppression of new cultural works minimal.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fair_use_four_factor_test__user_centric_reading, 0.25).
domain_priors:suppression_score(fair_use_four_factor_test__user_centric_reading, 0.3).
domain_priors:theater_ratio(fair_use_four_factor_test__user_centric_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fair_use_four_factor_test__user_centric_reading, extractiveness, 0.25).
narrative_ontology:constraint_metric(fair_use_four_factor_test__user_centric_reading, suppression_requirement, 0.3).
narrative_ontology:constraint_metric(fair_use_four_factor_test__user_centric_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(fair_use_four_factor_test__user_centric_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(fair_use_four_factor_test__user_centric_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(fair_use_four_factor_test__user_centric_reading, rope).
narrative_ontology:human_readable(fair_use_four_factor_test__user_centric_reading, "Fair Use Four-Factor Test (User-Centric Reading)").
narrative_ontology:topic_domain(fair_use_four_factor_test__user_centric_reading, "legal/intellectual_property/cultural_production").

domain_priors:requires_active_enforcement(fair_use_four_factor_test__user_centric_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(fair_use_four_factor_test__user_centric_reading, 'd0591331-030a-46d4-a4ab-941c4872deea').
narrative_ontology:cs_kernel_codification('d0591331-030a-46d4-a4ab-941c4872deea', fixed_text).
narrative_ontology:cs_authority_grounding('d0591331-030a-46d4-a4ab-941c4872deea', lineage).
narrative_ontology:cs_interpretation_layer_present('d0591331-030a-46d4-a4ab-941c4872deea').
narrative_ontology:cs_reading_relation('d0591331-030a-46d4-a4ab-941c4872deea', fair_use_four_factor_test__creator_centric_reading, coexists_with).
narrative_ontology:cs_reading_relation('d0591331-030a-46d4-a4ab-941c4872deea', fair_use_four_factor_test__transformative_use_reading, coexists_with).
narrative_ontology:cs_axiom('d0591331-030a-46d4-a4ab-941c4872deea', foundational, fair_use_as_affirmative_right).
narrative_ontology:cs_axiom_status(fair_use_as_affirmative_right, holdable).
narrative_ontology:cs_axiom_grounding('d0591331-030a-46d4-a4ab-941c4872deea', fair_use_as_affirmative_right, deontological).
narrative_ontology:cs_axiom('d0591331-030a-46d4-a4ab-941c4872deea', foundational, public_access_priority).
narrative_ontology:cs_axiom_status(public_access_priority, holdable).
narrative_ontology:cs_axiom_grounding('d0591331-030a-46d4-a4ab-941c4872deea', public_access_priority, instrumental).
narrative_ontology:cs_reference_frame('d0591331-030a-46d4-a4ab-941c4872deea', public_domain_enrichment_framework).
narrative_ontology:cs_drift_state('d0591331-030a-46d4-a4ab-941c4872deea', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('d0591331-030a-46d4-a4ab-941c4872deea', '').
narrative_ontology:cs_kernel_id(fair_use_four_factor_test__user_centric_reading, fair_use_four_factor_test).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(fair_use_four_factor_test__user_centric_reading, public_users).
narrative_ontology:constraint_beneficiary(fair_use_four_factor_test__user_centric_reading, educational_institutions).
narrative_ontology:constraint_beneficiary(fair_use_four_factor_test__user_centric_reading, cultural_commentators).
narrative_ontology:constraint_victim(fair_use_four_factor_test__user_centric_reading, copyright_holders).
narrative_ontology:constraint_vindicates(fair_use_four_factor_test__user_centric_reading, public_domain_enrichment).
narrative_ontology:constraint_vindicates(fair_use_four_factor_test__user_centric_reading, free_speech_principles).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefits from broad access to copyrighted works for non-commercial, educational, or transformative purposes without needing permission or paying royalties. Their ability to engage in cultural production and commentary is enhanced.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__user_centric_reading, public_users, beneficiary,
    organized, generational, constrained, global).

% Relies on fair use to incorporate copyrighted materials into curricula, research, and teaching without prohibitive licensing costs. This reading prioritizes their mission of knowledge dissemination.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__user_centric_reading, educational_institutions, beneficiary,
    institutional, generational, constrained, national).

% Engages in criticism, parody, and commentary using copyrighted works, benefiting from the flexibility of fair use to avoid infringement claims. This enables vibrant public discourse and cultural critique.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__user_centric_reading, cultural_commentators, beneficiary,
    moderate, biographical, mobile, global).

% Experiences reduced control over their copyrighted works and potentially diminished revenue from certain uses that would otherwise require licensing. They bear the cost of broader public access.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__user_centric_reading, copyright_holders, payer,
    powerful, generational, constrained, global).

% Interprets and applies the four-factor test, shaping the boundaries of fair use. This reading guides them to prioritize public benefit and user rights in their decisions.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__user_centric_reading, courts, agenda_setter,
    institutional, civilizational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Balances the exclusive rights of copyright holders with the public's interest in accessing and using copyrighted works for purposes like criticism, commentary, news reporting, teaching, scholarship, or research, fostering cultural production and public discourse.
% TRANSFER_FUNCTION: Transfers the right to use copyrighted material without permission or payment from copyright holders to users (public, educational, commentators) for specific purposes, reducing the economic control of rights holders.
% ABSENT_VOICES: Creators who rely solely on licensing revenue for their livelihood might feel their voices are marginalized when user rights are broadly prioritized, arguing for stronger property protections.
% DISAPPEARANCE_RATIONALE: If the fair use doctrine vanished, public access to copyrighted works for educational and transformative purposes would be severely curtailed, requiring explicit licenses for nearly all uses. This would stifle cultural production, commentary, and education, fundamentally altering the digital and creative economies.
% FOUNDING_PROBLEM: Copyright law, without exceptions, would stifle creativity and public discourse by making all unauthorized uses illegal, even those that benefit society and do not directly harm the creator's market.
% FOUNDING_PROBLEM_CORROBORATION: Legal scholars, public interest groups, and educators consistently attest to the ongoing need for fair use to prevent copyright from becoming an absolute barrier to cultural progress and free expression. This is corroborated by numerous amicus briefs in copyright cases from outside the direct beneficiary set.
narrative_ontology:disappearance_verdict(fair_use_four_factor_test__user_centric_reading, world_rearranges).
narrative_ontology:founding_problem_status(fair_use_four_factor_test__user_centric_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(fair_use_four_factor_test__user_centric_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(fair_use_four_factor_test__user_centric_reading, 'none', 1).
narrative_ontology:epsilon_provenance(fair_use_four_factor_test__user_centric_reading, 0.25, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(fair_use_four_factor_test__user_centric_reading_tests).
:- end_tests(fair_use_four_factor_test__user_centric_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.25) is low because this reading minimizes the economic burden on users, treating many unauthorized uses as legitimate contributions to the public domain rather than infringements. Suppression (0.3) is also low, as the legal framework, under this interpretation, actively enables rather than restricts new cultural production and commentary. The theater ratio is low (0.1) because the doctrine genuinely functions to balance rights, with minimal performative enforcement. Accessibility collapse is moderate (0.4) as alternatives (e.g., licensing) are not entirely collapsed but are often unnecessary under this interpretation. Resistance (0.5) is moderate, reflecting ongoing legal challenges from copyright holders who advocate for a more restrictive interpretation.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of public users, this is a clear Rope, enabling their creative and educational activities. From the perspective of copyright holders, it might feel more like a Tangled Rope or even a Snare, as their property rights are actively curtailed without direct compensation. The engine's classification will reflect this divergence based on the declared structural relationships and metrics.
 *
 * DIRECTIONALITY LOGIC:
 *   Public users, educational institutions, and cultural commentators are the primary beneficiaries, experiencing low directionality as the constraint subsidizes their activities. Copyright holders are the victims/payers, experiencing higher directionality as their exclusive rights are curtailed for public benefit. Courts act as agenda-setters, interpreting the law in a way that aligns with this user-centric philosophy.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    market_harm_interpretation,
    'How is ''effect of the use upon the potential market'' (the fourth factor) interpreted in practice under this reading?',
    'Analysis of court decisions: if courts consistently find minimal market harm even when direct competition exists, it confirms this reading''s bias. If they frequently find market harm, the reading''s practical application deviates.',
    'If market harm is consistently downplayed, it reinforces the low extractiveness for users. If it''s often found, the effective extractiveness for copyright holders is higher than intended by this reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(market_harm_interpretation, empirical, 'Ambiguity in how market harm is assessed, which directly impacts the balance between user rights and creator compensation.').

omega_variable(
    scope_of_public_benefit,
    'What specific types of ''public access'' and ''cultural production'' are prioritized, and does this prioritization inadvertently exclude other forms of beneficial use?',
    'Content analysis of fair use defenses: identifying which types of uses (e.g., education, parody, news) are most consistently protected, and which (e.g., commercial aggregation, data mining) are still contested.',
    'If the definition of ''public benefit'' is too narrow, the constraint''s actual reach as a ''user right'' is limited, potentially increasing suppression for excluded user groups. If it''s broad, it reinforces the Rope classification.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(scope_of_public_benefit, conceptual, 'The conceptual boundary of ''public benefit'' and its practical application in fair use cases.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fair_use_four_factor_test__user_centric_reading, 1976, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Extraction over time
narrative_ontology:measurement(fair_be_t1976, fair_use_four_factor_test__user_centric_reading, base_extractiveness, 1976, 0.3).
narrative_ontology:measurement(fair_be_t1990, fair_use_four_factor_test__user_centric_reading, base_extractiveness, 1990, 0.28).
narrative_ontology:measurement(fair_be_t2000, fair_use_four_factor_test__user_centric_reading, base_extractiveness, 2000, 0.25).
narrative_ontology:measurement(fair_be_t2010, fair_use_four_factor_test__user_centric_reading, base_extractiveness, 2010, 0.23).
narrative_ontology:measurement(fair_be_t2024, fair_use_four_factor_test__user_centric_reading, base_extractiveness, 2024, 0.25).

% Suppression requirement over time
narrative_ontology:measurement(fair_su_t1976, fair_use_four_factor_test__user_centric_reading, suppression_requirement, 1976, 0.4).
narrative_ontology:measurement(fair_su_t1990, fair_use_four_factor_test__user_centric_reading, suppression_requirement, 1990, 0.35).
narrative_ontology:measurement(fair_su_t2000, fair_use_four_factor_test__user_centric_reading, suppression_requirement, 2000, 0.3).
narrative_ontology:measurement(fair_su_t2010, fair_use_four_factor_test__user_centric_reading, suppression_requirement, 2010, 0.28).
narrative_ontology:measurement(fair_su_t2024, fair_use_four_factor_test__user_centric_reading, suppression_requirement, 2024, 0.3).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(fair_use_four_factor_test__user_centric_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(fair_use_four_factor_test__user_centric_reading, fair_use_four_factor_test__creator_centric_reading).
narrative_ontology:affects_constraint(fair_use_four_factor_test__user_centric_reading, fair_use_four_factor_test__transformative_use_reading).
narrative_ontology:affects_constraint(fair_use_four_factor_test__user_centric_reading, copyright_term_extension).

% DUAL FORMULATION NOTE:
% This constraint is one of three distinct readings of the 'fair_use_four_factor_test' kernel. Each reading represents a different structural interpretation of the same legal doctrine, leading to different beneficiary/victim sets and extractiveness profiles. They are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
