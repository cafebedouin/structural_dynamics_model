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
 *   This constraint describes the 'fair use' doctrine in US copyright law,
 *   specifically interpreted through a 'user-centric' lens. In this reading,
 *   fair use is understood as an affirmative right of users to engage in
 *   certain unauthorized uses of copyrighted material, with the four
 *   statutory factors (purpose and character of the use, nature of the
 *   copyrighted work, amount and substantiality of the portion used, and
 *   effect of the use upon the potential market for or value of the
 *   copyrighted work) weighed primarily to preserve public access, cultural
 *   production, and educational activities. This interpretation prioritizes
 *   the public benefit over the exclusive rights of copyright holders,
 *   leading to lower perceived extraction from users and higher costs for
 *   rights holders.
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
narrative_ontology:cs_story_uid(fair_use_four_factor_test__user_centric_reading, '1c6572f4-8b78-4e02-8d88-23388da9cdcf').
narrative_ontology:cs_kernel_codification('1c6572f4-8b78-4e02-8d88-23388da9cdcf', formalized).
narrative_ontology:cs_authority_grounding('1c6572f4-8b78-4e02-8d88-23388da9cdcf', lineage).
narrative_ontology:cs_interpretation_layer_present('1c6572f4-8b78-4e02-8d88-23388da9cdcf').
narrative_ontology:cs_reading_relation('1c6572f4-8b78-4e02-8d88-23388da9cdcf', fair_use_four_factor_test__creator_centric_reading, coexists_with).
narrative_ontology:cs_reading_relation('1c6572f4-8b78-4e02-8d88-23388da9cdcf', fair_use_four_factor_test__transformative_use_reading, coexists_with).
narrative_ontology:cs_axiom('1c6572f4-8b78-4e02-8d88-23388da9cdcf', foundational, public_access_promotes_progress).
narrative_ontology:cs_axiom_status(public_access_promotes_progress, holdable).
narrative_ontology:cs_axiom_grounding('1c6572f4-8b78-4e02-8d88-23388da9cdcf', public_access_promotes_progress, deontological).
narrative_ontology:cs_axiom('1c6572f4-8b78-4e02-8d88-23388da9cdcf', foundational, user_rights_are_affirmative).
narrative_ontology:cs_axiom_status(user_rights_are_affirmative, holdable).
narrative_ontology:cs_axiom_grounding('1c6572f4-8b78-4e02-8d88-23388da9cdcf', user_rights_are_affirmative, conventional).
narrative_ontology:cs_reference_frame('1c6572f4-8b78-4e02-8d88-23388da9cdcf', public_domain_enrichment).
narrative_ontology:cs_drift_state('1c6572f4-8b78-4e02-8d88-23388da9cdcf', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('1c6572f4-8b78-4e02-8d88-23388da9cdcf', '').
narrative_ontology:cs_kernel_id(fair_use_four_factor_test__user_centric_reading, fair_use_four_factor_test).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(fair_use_four_factor_test__user_centric_reading, public_users).
narrative_ontology:constraint_beneficiary(fair_use_four_factor_test__user_centric_reading, educational_institutions).
narrative_ontology:constraint_beneficiary(fair_use_four_factor_test__user_centric_reading, cultural_critics).
narrative_ontology:constraint_beneficiary(fair_use_four_factor_test__user_centric_reading, remix_artists).
narrative_ontology:constraint_victim(fair_use_four_factor_test__user_centric_reading, copyright_holders).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefits from broad access to copyrighted works for commentary, criticism, news reporting, teaching, scholarship, or research without needing permission or paying royalties. Their ability to engage in cultural production is enhanced.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__user_centric_reading, public_users, beneficiary,
    moderate, biographical, mobile, global).

% Relies on fair use to incorporate copyrighted materials into curricula and research without prohibitive licensing costs, facilitating knowledge dissemination and academic freedom.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__user_centric_reading, educational_institutions, beneficiary,
    organized, generational, constrained, national).

% Utilizes fair use to quote, sample, and analyze copyrighted works in their critical discourse, contributing to public understanding and cultural dialogue.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__user_centric_reading, cultural_critics, beneficiary,
    moderate, biographical, mobile, global).

% Benefits from the ability to create new works by recontextualizing existing copyrighted material, fostering innovation and new forms of expression, though often operating at the edge of legal interpretation.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__user_centric_reading, remix_artists, beneficiary,
    moderate, biographical, constrained, global).

% Bears the cost of reduced exclusive control over their works, potentially leading to diminished licensing revenue or control over derivative uses. They must actively monitor and litigate against uses they deem infringing, even under a user-centric interpretation.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__user_centric_reading, copyright_holders, payer,
    powerful, generational, constrained, global).

% Interprets and applies the four-factor test, shaping the boundaries of fair use. Under this reading, courts prioritize public benefit and user rights in their balancing, often ruling against copyright holders in close cases.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__user_centric_reading, courts, agenda_setter,
    institutional, civilizational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Balances the exclusive rights of copyright holders with the public's interest in accessing and using copyrighted works for socially beneficial purposes, preventing a complete lockdown of cultural and educational materials.
% TRANSFER_FUNCTION: Transfers the right to use copyrighted material without permission or payment from copyright holders to users (public, educational, critical, artistic) for specific purposes, facilitating cultural production and public access.
% ABSENT_VOICES: The 'absent voices' are those who would advocate for an even broader public domain or a 'copyleft' approach, arguing that all cultural works should be freely shareable. They are excluded by the fundamental premise of copyright itself, which fair use only partially mitigates.
% DISAPPEARANCE_RATIONALE: If the user-centric fair use doctrine vanished, public access to copyrighted works for education, criticism, and new creation would be severely curtailed. Every use would require explicit permission, leading to a chilling effect on cultural production and a significant increase in transaction costs for legitimate uses. The legal landscape of intellectual property would fundamentally shift.
% FOUNDING_PROBLEM: The original problem was how to reconcile the constitutional mandate to 'promote the Progress of Science and useful Arts' by granting exclusive rights to authors, with the need for subsequent creators and the public to build upon existing works without perpetual monopolies.
% FOUNDING_PROBLEM_CORROBORATION: Legal scholars, public interest groups, and educators consistently attest that the tension between creator rights and public access remains a live and evolving problem, requiring ongoing judicial and legislative balancing. This corroboration comes from outside the direct beneficiaries of this specific reading.
narrative_ontology:disappearance_verdict(fair_use_four_factor_test__user_centric_reading, world_rearranges).
narrative_ontology:founding_problem_status(fair_use_four_factor_test__user_centric_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(fair_use_four_factor_test__user_centric_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(fair_use_four_factor_test__user_centric_reading, 'none', 1).

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
 *   The extractiveness is low (0.25) because this reading actively limits the ability of copyright holders to extract rents from uses deemed beneficial to the public. Suppression is moderate (0.3) as it still requires users to navigate a legal test, but the burden is shifted to justify restriction rather than permission. Theater ratio is low (0.1) as the test is genuinely applied to balance competing interests, not merely to maintain an illusion. Resistance is moderate (0.5) as copyright holders continually challenge this interpretation, seeking to narrow its scope.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of public users and educators, this reading of fair use is a vital 'rope' that enables cultural and intellectual progress. From the perspective of copyright holders, it is a 'tangled rope' or even a 'snare' that diminishes their property rights and incentives. The engine's classification will reflect the structural position of each seat, which this story's metrics and stakeholder declarations are designed to capture.
 *
 * DIRECTIONALITY LOGIC:
 *   Public users, educational institutions, cultural critics, and remix artists are the primary beneficiaries, as their ability to use copyrighted material is expanded. Copyright holders are the victims, as their exclusive rights are curtailed, leading to reduced potential for extraction. Courts act as the agenda-setters, actively shaping the interpretation to favor user rights in this reading.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    scope_of_public_benefit,
    'How broadly should ''public access'' and ''cultural production'' be interpreted in the four-factor test, and at what point does it unduly diminish creator incentives?',
    'Empirical studies on the impact of fair use rulings on creator output and market dynamics, combined with ongoing judicial interpretation and legislative clarification.',
    'A broader interpretation would further reduce extractiveness from users but could increase it for creators; a narrower interpretation would reverse these effects. This ambiguity directly affects the balance of benefits and costs.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(scope_of_public_benefit, conceptual, 'The conceptual boundary between public benefit and creator incentive.').

omega_variable(
    judicial_drift_towards_user_rights,
    'Is the judiciary consistently applying a user-centric interpretation, or are there shifts towards creator-centric or transformative-use readings over time?',
    'Longitudinal analysis of fair use case law, examining the weighting of the four factors and the outcomes of infringement claims over decades.',
    'If judicial practice drifts away from a user-centric reading, the effective extractiveness for users would increase, and the constraint might reclassify towards a ''tangled_rope'' or ''snare'' from the user''s perspective.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(judicial_drift_towards_user_rights, empirical, 'Tracking judicial interpretation trends in fair use cases.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fair_use_four_factor_test__user_centric_reading, 1976, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fair_tr_t1976, fair_use_four_factor_test__user_centric_reading, theater_ratio, 1976, 0.1).
narrative_ontology:measurement(fair_tr_t1990, fair_use_four_factor_test__user_centric_reading, theater_ratio, 1990, 0.1).
narrative_ontology:measurement(fair_tr_t2000, fair_use_four_factor_test__user_centric_reading, theater_ratio, 2000, 0.1).
narrative_ontology:measurement(fair_tr_t2010, fair_use_four_factor_test__user_centric_reading, theater_ratio, 2010, 0.1).
narrative_ontology:measurement(fair_tr_t2024, fair_use_four_factor_test__user_centric_reading, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(fair_be_t1976, fair_use_four_factor_test__user_centric_reading, base_extractiveness, 1976, 0.3).
narrative_ontology:measurement(fair_be_t1990, fair_use_four_factor_test__user_centric_reading, base_extractiveness, 1990, 0.28).
narrative_ontology:measurement(fair_be_t2000, fair_use_four_factor_test__user_centric_reading, base_extractiveness, 2000, 0.26).
narrative_ontology:measurement(fair_be_t2010, fair_use_four_factor_test__user_centric_reading, base_extractiveness, 2010, 0.24).
narrative_ontology:measurement(fair_be_t2024, fair_use_four_factor_test__user_centric_reading, base_extractiveness, 2024, 0.25).

% Suppression requirement over time
narrative_ontology:measurement(fair_su_t1976, fair_use_four_factor_test__user_centric_reading, suppression_requirement, 1976, 0.35).
narrative_ontology:measurement(fair_su_t1990, fair_use_four_factor_test__user_centric_reading, suppression_requirement, 1990, 0.32).
narrative_ontology:measurement(fair_su_t2000, fair_use_four_factor_test__user_centric_reading, suppression_requirement, 2000, 0.3).
narrative_ontology:measurement(fair_su_t2010, fair_use_four_factor_test__user_centric_reading, suppression_requirement, 2010, 0.28).
narrative_ontology:measurement(fair_su_t2024, fair_use_four_factor_test__user_centric_reading, suppression_requirement, 2024, 0.3).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(fair_use_four_factor_test__user_centric_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(fair_use_four_factor_test__user_centric_reading, copyright_term_extension).
narrative_ontology:affects_constraint(fair_use_four_factor_test__user_centric_reading, digital_millennium_copyright_act).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'fair_use_four_factor_test' kernel. The other readings are 'creator_centric_reading' and 'transformative_use_reading', each with distinct structural properties and classifications.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
