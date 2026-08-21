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
 *   This constraint story instantiates the 'freedom_floor_reading' of
 *   unconditional income support. It posits that a universal basic income
 *   (UBI) functions as an autonomy-enabling floor, removing coercion from the
 *   labor market, eliminating welfare stigma, and protecting against market
 *   shocks. The reading emphasizes the positive coordination function of UBI
 *   in fostering individual agency and societal resilience. It claims a Rope
 *   classification, with beneficiaries including precarious workers,
 *   caregivers, artists, and abuse victims, and no identifiable victims, as
 *   it's seen as a Pareto improvement.
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
narrative_ontology:cs_story_uid(unconditional_income_support__freedom_floor_reading, '1dd7a128-aa27-4781-8746-44cc0c8f6a0b').
narrative_ontology:cs_kernel_codification('1dd7a128-aa27-4781-8746-44cc0c8f6a0b', formalized).
narrative_ontology:cs_authority_grounding('1dd7a128-aa27-4781-8746-44cc0c8f6a0b', expertise).
narrative_ontology:cs_interpretation_layer_present('1dd7a128-aa27-4781-8746-44cc0c8f6a0b').
narrative_ontology:cs_reading_relation('1dd7a128-aa27-4781-8746-44cc0c8f6a0b', unconditional_income_support__dependency_trap_reading, coexists_with).
narrative_ontology:cs_reading_relation('1dd7a128-aa27-4781-8746-44cc0c8f6a0b', unconditional_income_support__universality_paradox_reading, coexists_with).
narrative_ontology:cs_axiom('1dd7a128-aa27-4781-8746-44cc0c8f6a0b', foundational, economic_autonomy_is_foundational_freedom).
narrative_ontology:cs_axiom_status(economic_autonomy_is_foundational_freedom, holdable).
narrative_ontology:cs_axiom_grounding('1dd7a128-aa27-4781-8746-44cc0c8f6a0b', economic_autonomy_is_foundational_freedom, deontological).
narrative_ontology:cs_axiom('1dd7a128-aa27-4781-8746-44cc0c8f6a0b', foundational, unconditional_income_reduces_labor_market_coercion).
narrative_ontology:cs_axiom_status(unconditional_income_reduces_labor_market_coercion, holdable).
narrative_ontology:cs_axiom_grounding('1dd7a128-aa27-4781-8746-44cc0c8f6a0b', unconditional_income_reduces_labor_market_coercion, empirically_contingent).
narrative_ontology:cs_reference_frame('1dd7a128-aa27-4781-8746-44cc0c8f6a0b', post_industrial_autonomy_framework).
narrative_ontology:cs_drift_state('1dd7a128-aa27-4781-8746-44cc0c8f6a0b', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('1dd7a128-aa27-4781-8746-44cc0c8f6a0b', '2024-07-30T12:00:00Z').
narrative_ontology:cs_kernel_id(unconditional_income_support__freedom_floor_reading, unconditional_income_support).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(unconditional_income_support__freedom_floor_reading, precarious_workers).
narrative_ontology:constraint_beneficiary(unconditional_income_support__freedom_floor_reading, caregivers).
narrative_ontology:constraint_beneficiary(unconditional_income_support__freedom_floor_reading, artists).
narrative_ontology:constraint_beneficiary(unconditional_income_support__freedom_floor_reading, abuse_victims).
narrative_ontology:constraint_beneficiary(unconditional_income_support__freedom_floor_reading, all_citizens).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Receives a baseline income that reduces the pressure to accept exploitative labor conditions, enabling better job matching and reducing precarity. This group benefits directly from increased bargaining power.
narrative_ontology:constraint_stakeholder(unconditional_income_support__freedom_floor_reading, precarious_workers, beneficiary,
    powerless, immediate, constrained, national).

% Receives income that values unpaid care work, providing financial stability and recognition for essential social contributions, reducing their dependence on external employment or spousal income.
narrative_ontology:constraint_stakeholder(unconditional_income_support__freedom_floor_reading, caregivers, beneficiary,
    moderate, biographical, constrained, national).

% Receives income that allows for greater creative freedom and risk-taking, reducing the need to commercialize art prematurely or pursue unrelated work for survival. This fosters cultural production.
narrative_ontology:constraint_stakeholder(unconditional_income_support__freedom_floor_reading, artists, beneficiary,
    moderate, biographical, constrained, national).

% Receives financial independence that enables escape from abusive relationships, removing a key economic barrier to exit and increasing personal safety and autonomy.
narrative_ontology:constraint_stakeholder(unconditional_income_support__freedom_floor_reading, abuse_victims, beneficiary,
    powerless, immediate, identity_locked, local).

% Benefits from a more resilient economy, reduced social inequality, improved public health outcomes, and a more engaged citizenry. The universality of the program reduces stigma and administrative overhead.
narrative_ontology:constraint_stakeholder(unconditional_income_support__freedom_floor_reading, all_citizens, beneficiary,
    organized, generational, mobile, national).

% Administers the unconditional income support program, managing distribution and ensuring compliance. Bears the fiscal cost but benefits from reduced administrative complexity compared to means-tested welfare programs and improved social stability.
narrative_ontology:constraint_stakeholder(unconditional_income_support__freedom_floor_reading, government_agencies, agenda_setter,
    institutional, generational, constrained, national).

% Would argue that unconditional income support is too broad, potentially diverting funds from targeted programs for the most vulnerable, and that it may not adequately address complex needs beyond income. Their concerns are often marginalized in the push for universality.
narrative_ontology:constraint_stakeholder(unconditional_income_support__freedom_floor_reading, traditional_welfare_advocates, excluded,
    organized, biographical, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a baseline economic floor for all citizens, enabling individuals to participate in the labor market and society from a position of greater security and autonomy, rather than coercion. It simplifies welfare administration and reduces the 'poverty trap' of means-tested benefits.
% TRANSFER_FUNCTION: Transfers a regular, unconditional income from the national tax base to every citizen, ensuring a basic standard of living and enabling greater freedom in labor and life choices.
% ABSENT_VOICES: Critics who fear disincentives to work or argue for more targeted interventions are often excluded from the core 'freedom floor' narrative, which emphasizes universal benefits and autonomy. Their concerns about fiscal sustainability or potential for 'free riders' are not central to this reading.
% DISAPPEARANCE_RATIONALE: If unconditional income support vanished overnight, millions would immediately lose their economic floor, forcing many back into precarious labor, increasing poverty and inequality, and destabilizing social safety nets. The labor market would revert to its prior coercive dynamics, and social outcomes would degrade.
% FOUNDING_PROBLEM: The problem of economic precarity, labor market coercion, welfare stigma, and the administrative complexity of means-tested social support systems.
% FOUNDING_PROBLEM_CORROBORATION: Independent economic studies, social science research on labor market dynamics, and testimony from individuals experiencing precarity and welfare stigma corroborate the ongoing nature of these problems. Pilot programs in various countries (e.g., Finland, Kenya) provide empirical evidence of the problem's persistence and the intervention's effects.
narrative_ontology:disappearance_verdict(unconditional_income_support__freedom_floor_reading, world_rearranges).
narrative_ontology:founding_problem_status(unconditional_income_support__freedom_floor_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(unconditional_income_support__freedom_floor_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
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
 *   The extractiveness (0.25) is low, representing the necessary taxation to fund the program, which is viewed as a collective investment rather than extraction from specific victims. Suppression (0.1) is minimal, as the program aims to reduce, not impose, coercive pressures. Theater ratio (0.05) is low, reflecting a direct and functional transfer with little performative overhead. The accessibility collapse (0.2) is low because it expands, rather than restricts, options. Resistance (0.15) is also low, as this reading focuses on the broad societal benefits and widespread support for autonomy.
 *
 * PERSPECTIVAL GAP:
 *   The 'freedom_floor_reading' emphasizes the autonomy-enhancing aspects, leading to a Rope classification. Sibling readings, such as the 'dependency_trap_reading', would likely classify it as a Snare due to perceived disincentives to work and misallocation of resources, highlighting a significant perspectival gap on the constraint's fundamental nature. The engine's classification will reflect the structural data provided here, which aligns with the freedom-floor perspective.
 *
 * DIRECTIONALITY LOGIC:
 *   Precarious workers, caregivers, artists, and abuse victims are primary beneficiaries, experiencing a significant increase in autonomy and reduction in coercive pressure (d near 0.0). All citizens also benefit from a more stable and equitable society. Government agencies act as agenda-setters, managing the program for collective benefit. No specific victim group is identified in this reading, as the funding is drawn from general taxation and framed as a collective investment.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    labor_supply_effects,
    'What are the actual long-term effects of unconditional income support on labor supply and participation rates?',
    'Longitudinal studies of large-scale UBI pilot programs across diverse economic contexts, measuring changes in employment, hours worked, and entrepreneurial activity.',
    'If labor supply significantly decreases, it would challenge the ''autonomy-enabling'' aspect, potentially shifting the classification towards a ''dependency_trap_reading'' (Snare). If effects are minimal or positive (e.g., increased entrepreneurship), it reinforces the Rope classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(labor_supply_effects, empirical, 'Uncertainty regarding the empirical impact on labor market participation.').

omega_variable(
    fiscal_sustainability_and_funding_source,
    'Is the proposed funding mechanism for unconditional income support fiscally sustainable without creating new forms of extraction or economic distortion?',
    'Detailed macroeconomic modeling and real-world implementation data on tax base changes, inflation, and public debt, compared against alternative funding models (e.g., carbon tax, wealth tax).',
    'If funding proves unsustainable or requires highly extractive taxation, it could introduce a ''victim'' class not accounted for in this reading, pushing towards a Tangled Rope or even Snare classification. If sustainable and non-extractive, it strengthens the Rope classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fiscal_sustainability_and_funding_source, empirical, 'Uncertainty about the long-term fiscal viability and potential for hidden extraction in funding.').

omega_variable(
    coercion_definition_ambiguity,
    'Is the ''coercion'' removed from the labor market by UBI a genuine structural coercion, or is it a natural consequence of economic scarcity and individual choice?',
    'Philosophical and economic analysis of ''coercion'' in market contexts, distinguishing between structural constraints and voluntary exchange under scarcity. This would involve examining the counterfactuals of a world without UBI.',
    'If the ''coercion'' is re-framed as non-coercive scarcity, the core justification for UBI as a ''freedom floor'' weakens, potentially aligning with the ''dependency_trap_reading'' that views UBI as an unnecessary subsidy.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(coercion_definition_ambiguity, conceptual, 'Conceptual ambiguity in the definition of ''coercion'' in the labor market context.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(unconditional_income_support__freedom_floor_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(unco_tr_t0, unconditional_income_support__freedom_floor_reading, theater_ratio, 0, 0.03).
narrative_ontology:measurement(unco_tr_t5, unconditional_income_support__freedom_floor_reading, theater_ratio, 5, 0.04).
narrative_ontology:measurement(unco_tr_t10, unconditional_income_support__freedom_floor_reading, theater_ratio, 10, 0.04).
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

% DUAL FORMULATION NOTE:
% This story is one reading of the 'unconditional_income_support' kernel. It focuses on the 'freedom_floor' interpretation, distinct from 'dependency_trap_reading' and 'universality_paradox_reading'.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
