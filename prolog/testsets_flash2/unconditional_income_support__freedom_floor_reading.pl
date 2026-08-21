% ============================================================================
% CONSTRAINT STORY: unconditional_income_support__freedom_floor_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_unconditional_income_support__freedom_floor_reading, []).

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
 *   constraint_id: unconditional_income_support__freedom_floor_reading
 *   human_readable: Unconditional Income Support (Freedom Floor Reading)
 *   domain: political_economy/social_policy/welfare_state_theory
 *
 * SUMMARY:
 *   This constraint story instantiates the 'freedom floor' reading of
 *   unconditional income support. It posits that such a system primarily
 *   functions to enhance individual autonomy by removing the coercive
 *   elements of the labor market, eliminating welfare stigma, and providing a
 *   buffer against economic shocks. The reading emphasizes the positive
 *   coordination effects and minimal extraction, viewing any costs as
 *   necessary investments in societal well-being and individual freedom. This
 *   is one reading of the 'unconditional_income_support' kernel, alongside
 *   the 'dependency_trap_reading' and 'universality_paradox_reading'.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(unconditional_income_support__freedom_floor_reading, 0.25).
domain_priors:suppression_score(unconditional_income_support__freedom_floor_reading, 0.1).
domain_priors:theater_ratio(unconditional_income_support__freedom_floor_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(unconditional_income_support__freedom_floor_reading, extractiveness, 0.25).
narrative_ontology:constraint_metric(unconditional_income_support__freedom_floor_reading, suppression_requirement, 0.1).
narrative_ontology:constraint_metric(unconditional_income_support__freedom_floor_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(unconditional_income_support__freedom_floor_reading, accessibility_collapse, 0.2).
narrative_ontology:constraint_metric(unconditional_income_support__freedom_floor_reading, resistance, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(unconditional_income_support__freedom_floor_reading, rope).
narrative_ontology:human_readable(unconditional_income_support__freedom_floor_reading, "Unconditional Income Support (Freedom Floor Reading)").
narrative_ontology:topic_domain(unconditional_income_support__freedom_floor_reading, "political_economy/social_policy/welfare_state_theory").

domain_priors:requires_active_enforcement(unconditional_income_support__freedom_floor_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(unconditional_income_support__freedom_floor_reading, '9ea3f58f-46af-4d16-b7bd-be94100b193a').
narrative_ontology:cs_kernel_codification('9ea3f58f-46af-4d16-b7bd-be94100b193a', formalized).
narrative_ontology:cs_authority_grounding('9ea3f58f-46af-4d16-b7bd-be94100b193a', lineage).
narrative_ontology:cs_interpretation_layer_present('9ea3f58f-46af-4d16-b7bd-be94100b193a').
narrative_ontology:cs_reading_relation('9ea3f58f-46af-4d16-b7bd-be94100b193a', unconditional_income_support__dependency_trap_reading, coexists_with).
narrative_ontology:cs_reading_relation('9ea3f58f-46af-4d16-b7bd-be94100b193a', unconditional_income_support__universality_paradox_reading, coexists_with).
narrative_ontology:cs_axiom('9ea3f58f-46af-4d16-b7bd-be94100b193a', foundational, autonomy_as_foundational_good).
narrative_ontology:cs_axiom_status(autonomy_as_foundational_good, holdable).
narrative_ontology:cs_axiom_grounding('9ea3f58f-46af-4d16-b7bd-be94100b193a', autonomy_as_foundational_good, deontological).
narrative_ontology:cs_axiom('9ea3f58f-46af-4d16-b7bd-be94100b193a', foundational, economic_security_as_human_right).
narrative_ontology:cs_axiom_status(economic_security_as_human_right, holdable).
narrative_ontology:cs_axiom_grounding('9ea3f58f-46af-4d16-b7bd-be94100b193a', economic_security_as_human_right, deontological).
narrative_ontology:cs_reference_frame('9ea3f58f-46af-4d16-b7bd-be94100b193a', post_scarcity_social_contract).
narrative_ontology:cs_drift_state('9ea3f58f-46af-4d16-b7bd-be94100b193a', contemporary_neoliberal_era, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('9ea3f58f-46af-4d16-b7bd-be94100b193a', '2024-07-30T12:00:00Z').
narrative_ontology:cs_kernel_id(unconditional_income_support__freedom_floor_reading, unconditional_income_support).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(unconditional_income_support__freedom_floor_reading, precarious_workers).
narrative_ontology:constraint_beneficiary(unconditional_income_support__freedom_floor_reading, caregivers).
narrative_ontology:constraint_beneficiary(unconditional_income_support__freedom_floor_reading, artists).
narrative_ontology:constraint_beneficiary(unconditional_income_support__freedom_floor_reading, abuse_victims).
narrative_ontology:constraint_beneficiary(unconditional_income_support__freedom_floor_reading, all_citizens).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(unconditional_income_support__freedom_floor_reading, taxpayers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Receives a baseline income that reduces the pressure to accept exploitative labor conditions, enabling better job matching and reducing precarity. This group benefits directly from the autonomy-enhancing aspect.
narrative_ontology:constraint_stakeholder(unconditional_income_support__freedom_floor_reading, precarious_workers, beneficiary,
    powerless, immediate, constrained, national).

% Receives income that values unpaid care work, providing financial stability and recognition for essential social contributions, reducing their economic vulnerability.
narrative_ontology:constraint_stakeholder(unconditional_income_support__freedom_floor_reading, caregivers, beneficiary,
    moderate, biographical, constrained, national).

% Receives income that supports creative pursuits, reducing the need for commercial compromise and fostering cultural production, which often has long-term societal benefits.
narrative_ontology:constraint_stakeholder(unconditional_income_support__freedom_floor_reading, artists, beneficiary,
    moderate, biographical, constrained, national).

% Receives income that provides a financial escape route from abusive relationships, enabling them to leave without facing immediate destitution, thereby enhancing personal safety and autonomy.
narrative_ontology:constraint_stakeholder(unconditional_income_support__freedom_floor_reading, abuse_victims, beneficiary,
    powerless, immediate, identity_locked, local).

% Benefits from a more resilient economy, reduced social inequality, and a healthier, more engaged populace. The universal nature of the support removes stigma and administrative overhead.
narrative_ontology:constraint_stakeholder(unconditional_income_support__freedom_floor_reading, all_citizens, beneficiary,
    organized, generational, mobile, national).

% Administers the unconditional income program, managing distribution and ensuring compliance. Benefits from simplified welfare administration and reduced social costs associated with poverty and precarity.
narrative_ontology:constraint_stakeholder(unconditional_income_support__freedom_floor_reading, government_agencies, agenda_setter,
    institutional, generational, constrained, national).

% Contributes through taxes to fund the unconditional income program. While bearing the direct financial cost, they are also indirect beneficiaries of the societal improvements and economic stability it provides.
narrative_ontology:constraint_stakeholder(unconditional_income_support__freedom_floor_reading, taxpayers, payer,
    organized, biographical, mobile, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a universal economic floor that coordinates individual autonomy with societal stability, enabling individuals to pursue education, caregiving, or entrepreneurship without fear of destitution, and smoothing economic shocks across the population.
% TRANSFER_FUNCTION: Transfers a baseline income from the general tax base to all citizens, ensuring a minimum standard of living and decoupling income from traditional labor market participation.
% ABSENT_VOICES: Those who believe that any form of unconditional income fosters dependency or distorts market incentives are present in public discourse but are not considered 'excluded' in this reading, as their arguments are actively engaged and debated within the policy-making process.
% DISAPPEARANCE_RATIONALE: If unconditional income support vanished overnight, many precarious workers, caregivers, artists, and abuse victims would immediately face severe economic hardship, leading to increased poverty, social instability, and a return to coercive labor market dynamics. The social safety net would be severely weakened, and the economy would lose a key shock absorber.
% FOUNDING_PROBLEM: The problem of poverty, economic precarity, and the coercive nature of traditional labor markets, where individuals are forced to accept undesirable work due to lack of alternatives, leading to widespread social and economic insecurity.
% FOUNDING_PROBLEM_CORROBORATION: Economists, social scientists, and advocacy groups for labor rights and poverty reduction widely corroborate the persistence of economic precarity and the coercive aspects of the labor market, citing data on stagnant wages, rising cost of living, and the psychological toll of economic insecurity. Pilot programs in various countries also provide empirical corroboration of the problem's live status.
narrative_ontology:disappearance_verdict(unconditional_income_support__freedom_floor_reading, world_rearranges).
narrative_ontology:founding_problem_status(unconditional_income_support__freedom_floor_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(unconditional_income_support__freedom_floor_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(unconditional_income_support__freedom_floor_reading, 'none', 1).
narrative_ontology:epsilon_provenance(unconditional_income_support__freedom_floor_reading, 0.25, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(unconditional_income_support__freedom_floor_reading_tests).
:- end_tests(unconditional_income_support__freedom_floor_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.25) is low because the primary function is seen as a societal investment in autonomy and stability, with costs distributed broadly through taxation. Any 'extraction' is viewed as a necessary cost of coordination, not asymmetric rent-seeking. Suppression (0.1) is minimal as the system aims to reduce, not impose, coercion. Theater ratio (0.05) is low, reflecting a belief that the system's stated goals align closely with its actual operation. The metrics reflect the ideal operation of the system as envisioned by this reading.
 *
 * PERSPECTIVAL GAP:
 *   This reading emphasizes the autonomy-enhancing and stigma-reducing aspects, leading to a classification as a Rope. Other readings, such as the 'dependency_trap_reading', would likely classify it as a Snare due to perceived disincentives to work and misallocation of resources, leading to higher extractiveness and suppression metrics from their perspective. The 'universality_paradox_reading' would focus on the political and implementation challenges, potentially leading to a Tangled Rope or Piton classification depending on the specific policy outcomes.
 *
 * DIRECTIONALITY LOGIC:
 *   Precarious workers, caregivers, artists, and abuse victims are direct beneficiaries, experiencing enhanced autonomy and reduced precarity (low directionality). All citizens are also beneficiaries through improved societal resilience. Government agencies are agenda-setters, managing the system for collective benefit. Taxpayers are payers, but their contribution is framed as an investment with broad societal returns, making their effective directionality closer to symmetric than purely extractive.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    labor_supply_effects,
    'What are the actual long-term effects of unconditional income support on labor supply and participation rates?',
    'Longitudinal studies of large-scale unconditional income programs in diverse economic contexts, measuring changes in employment, hours worked, and entrepreneurial activity.',
    'If labor supply significantly decreases, it would challenge the ''autonomy-enabling'' claim by suggesting a trade-off with productivity, potentially shifting the classification towards a Tangled Rope or Snare from a different perspective. If labor supply remains stable or shifts towards more meaningful work, it would reinforce the Rope classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(labor_supply_effects, empirical, 'Uncertainty regarding the empirical impact on labor market dynamics.').

omega_variable(
    stigma_elimination_efficacy,
    'Does unconditional income truly eliminate welfare stigma, or does a new form of stigma emerge for recipients?',
    'Sociological studies and qualitative research on recipient experiences, public perception, and media representation of unconditional income programs.',
    'If significant stigma persists or new forms emerge, it would undermine a key claim of this reading, potentially increasing the perceived ''extraction'' (psychological cost) and shifting the classification towards a Tangled Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(stigma_elimination_efficacy, empirical, 'Uncertainty about the social and psychological effects on recipients.').

omega_variable(
    framing_of_cost_as_investment,
    'Is the cost of unconditional income support genuinely perceived as a societal investment by the broader public, or is it primarily viewed as a transfer payment from taxpayers to beneficiaries?',
    'Public opinion surveys, analysis of political discourse, and voting patterns related to taxation and social spending.',
    'If the cost is widely perceived as a pure transfer, it could increase perceived extractiveness for taxpayers, potentially shifting their seat''s classification towards a Snare, even if the overall constraint remains a Rope. This would highlight a significant perspectival gap.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(framing_of_cost_as_investment, conceptual, 'Ambiguity in public perception of the program''s financial nature.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(unconditional_income_support__freedom_floor_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(unco_tr_t0, unconditional_income_support__freedom_floor_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(unco_tr_t5, unconditional_income_support__freedom_floor_reading, theater_ratio, 5, 0.05).
narrative_ontology:measurement(unco_tr_t10, unconditional_income_support__freedom_floor_reading, theater_ratio, 10, 0.05).
narrative_ontology:measurement(unco_tr_t15, unconditional_income_support__freedom_floor_reading, theater_ratio, 15, 0.05).
narrative_ontology:measurement(unco_tr_t20, unconditional_income_support__freedom_floor_reading, theater_ratio, 20, 0.05).

% Extraction over time
narrative_ontology:measurement(unco_be_t0, unconditional_income_support__freedom_floor_reading, base_extractiveness, 0, 0.2).
narrative_ontology:measurement(unco_be_t5, unconditional_income_support__freedom_floor_reading, base_extractiveness, 5, 0.22).
narrative_ontology:measurement(unco_be_t10, unconditional_income_support__freedom_floor_reading, base_extractiveness, 10, 0.23).
narrative_ontology:measurement(unco_be_t15, unconditional_income_support__freedom_floor_reading, base_extractiveness, 15, 0.24).
narrative_ontology:measurement(unco_be_t20, unconditional_income_support__freedom_floor_reading, base_extractiveness, 20, 0.25).

% Suppression requirement over time
narrative_ontology:measurement(unco_su_t0, unconditional_income_support__freedom_floor_reading, suppression_requirement, 0, 0.08).
narrative_ontology:measurement(unco_su_t5, unconditional_income_support__freedom_floor_reading, suppression_requirement, 5, 0.09).
narrative_ontology:measurement(unco_su_t10, unconditional_income_support__freedom_floor_reading, suppression_requirement, 10, 0.09).
narrative_ontology:measurement(unco_su_t15, unconditional_income_support__freedom_floor_reading, suppression_requirement, 15, 0.1).
narrative_ontology:measurement(unco_su_t20, unconditional_income_support__freedom_floor_reading, suppression_requirement, 20, 0.1).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(unconditional_income_support__freedom_floor_reading, resource_allocation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
