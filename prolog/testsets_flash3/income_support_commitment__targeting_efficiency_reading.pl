% ============================================================================
% CONSTRAINT STORY: income_support_commitment__targeting_efficiency_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_income_support_commitment__targeting_efficiency_reading, []).

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
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: income_support_commitment__targeting_efficiency_reading
 *   human_readable: Targeting Efficiency in Income Support
 *   domain: political_economy/social_policy/welfare_state_theory
 *
 * SUMMARY:
 *   This constraint represents the 'targeting efficiency' reading of income
 *   support, arguing that aid should be concentrated on demonstrated need
 *   rather than universally distributed. This reading, when instantiated,
 *   creates a system where current targeted program recipients are both
 *   beneficiaries (of existing programs) and victims (of the opportunity cost
 *   of a UBI that would replace them). The high extractiveness (0.85)
 *   reflects the net loss for the poorest under a UBI replacement scenario,
 *   where the 'efficiency' of targeting becomes a mechanism for extraction
 *   from the most vulnerable to fund a broader, less impactful distribution.
 *   The constraint is claimed as a 'snare' because its coordination story
 *   (efficient allocation) serves as cover for a system that can trap and
 *   disadvantage its nominal beneficiaries when alternatives are considered.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(income_support_commitment__targeting_efficiency_reading, 0.85).
domain_priors:suppression_score(income_support_commitment__targeting_efficiency_reading, 0.7).
domain_priors:theater_ratio(income_support_commitment__targeting_efficiency_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(income_support_commitment__targeting_efficiency_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(income_support_commitment__targeting_efficiency_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(income_support_commitment__targeting_efficiency_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(income_support_commitment__targeting_efficiency_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(income_support_commitment__targeting_efficiency_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(income_support_commitment__targeting_efficiency_reading, snare).
narrative_ontology:human_readable(income_support_commitment__targeting_efficiency_reading, "Targeting Efficiency in Income Support").
narrative_ontology:topic_domain(income_support_commitment__targeting_efficiency_reading, "political_economy/social_policy/welfare_state_theory").

domain_priors:requires_active_enforcement(income_support_commitment__targeting_efficiency_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(income_support_commitment__targeting_efficiency_reading, 'dc81b149-8d5c-4182-bf2b-3ad5425a39e8').
narrative_ontology:cs_kernel_codification('dc81b149-8d5c-4182-bf2b-3ad5425a39e8', formalized).
narrative_ontology:cs_authority_grounding('dc81b149-8d5c-4182-bf2b-3ad5425a39e8', lineage).
narrative_ontology:cs_interpretation_layer_present('dc81b149-8d5c-4182-bf2b-3ad5425a39e8').
narrative_ontology:cs_reading_relation('dc81b149-8d5c-4182-bf2b-3ad5425a39e8', income_support_commitment__freedom_floor_reading, coexists_with).
narrative_ontology:cs_reading_relation('dc81b149-8d5c-4182-bf2b-3ad5425a39e8', income_support_commitment__dependency_trap_reading, coexists_with).
narrative_ontology:cs_axiom('dc81b149-8d5c-4182-bf2b-3ad5425a39e8', foundational, resource_scarcity_requires_prioritization).
narrative_ontology:cs_axiom_status(resource_scarcity_requires_prioritization, holdable).
narrative_ontology:cs_axiom_grounding('dc81b149-8d5c-4182-bf2b-3ad5425a39e8', resource_scarcity_requires_prioritization, empirically_contingent).
narrative_ontology:cs_axiom('dc81b149-8d5c-4182-bf2b-3ad5425a39e8', secondary, universal_distribution_is_wasteful).
narrative_ontology:cs_axiom_status(universal_distribution_is_wasteful, holdable).
narrative_ontology:cs_axiom_grounding('dc81b149-8d5c-4182-bf2b-3ad5425a39e8', universal_distribution_is_wasteful, empirically_contingent).
narrative_ontology:cs_reference_frame('dc81b149-8d5c-4182-bf2b-3ad5425a39e8', efficient_targeted_welfare_state).
narrative_ontology:cs_drift_state('dc81b149-8d5c-4182-bf2b-3ad5425a39e8', contemporary_ubi_discourse, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('dc81b149-8d5c-4182-bf2b-3ad5425a39e8', '').
narrative_ontology:cs_kernel_id(income_support_commitment__targeting_efficiency_reading, income_support_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(income_support_commitment__targeting_efficiency_reading, current_targeted_program_recipients).
narrative_ontology:constraint_beneficiary(income_support_commitment__targeting_efficiency_reading, taxpayers_averse_to_universal_programs).
narrative_ontology:constraint_victim(income_support_commitment__targeting_efficiency_reading, current_targeted_program_recipients).
narrative_ontology:constraint_victim(income_support_commitment__targeting_efficiency_reading, low_income_workers_above_poverty_line).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These individuals currently receive substantial benefits from targeted programs based on demonstrated need. Under a universal basic income (UBI) system that replaces these programs, they would experience a net loss of income, making them victims of the shift despite being nominal beneficiaries of income support in general. They are trapped by the existing system's benefits and the lack of viable alternatives.
narrative_ontology:constraint_stakeholder(income_support_commitment__targeting_efficiency_reading, current_targeted_program_recipients, beneficiary,
    powerless, immediate, trapped, local).
narrative_ontology:stakeholder_secondary_role(income_support_commitment__targeting_efficiency_reading, current_targeted_program_recipients, payer).

% This group benefits from the perception of efficient use of tax dollars, believing that universal programs are wasteful. They support policies that concentrate resources on 'deserving' populations, reducing their perceived tax burden for non-targeted spending.
narrative_ontology:constraint_stakeholder(income_support_commitment__targeting_efficiency_reading, taxpayers_averse_to_universal_programs, beneficiary,
    organized, biographical, mobile, national).

% These administrators design and implement targeted welfare programs. Their careers and institutional structures are built around means-testing and program-specific eligibility, making them proponents of the targeting efficiency model. A shift to universal programs would disrupt their institutional power and expertise.
narrative_ontology:constraint_stakeholder(income_support_commitment__targeting_efficiency_reading, social_policy_administrators, agenda_setter,
    institutional, generational, constrained, national).

% These advocates argue for the simplicity, dignity, and broader economic benefits of UBI, which would replace targeted programs. They are excluded from the core policy-making process that prioritizes targeting efficiency, despite their growing public support.
narrative_ontology:constraint_stakeholder(income_support_commitment__targeting_efficiency_reading, advocates_for_universal_basic_income, excluded,
    moderate, generational, constrained, national).

% These individuals earn just enough to be ineligible for many targeted programs but still struggle financially. Under a UBI system, they would likely be net beneficiaries, but under the current targeting efficiency model, they are effectively victims, paying taxes that fund programs they cannot access while receiving no direct support.
narrative_ontology:constraint_stakeholder(income_support_commitment__targeting_efficiency_reading, low_income_workers_above_poverty_line, payer,
    powerless, immediate, constrained, local).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates social welfare spending by directing resources to individuals and families who demonstrate specific needs, aiming to maximize impact per dollar spent and prevent 'undeserving' recipients.
% TRANSFER_FUNCTION: Transfers tax revenue from the general populace to specific, means-tested individuals and families, based on criteria like income, family size, disability, or unemployment status.
% ABSENT_VOICES: Advocates for universal basic income and other universal programs are largely excluded from the policy design process, as their proposals fundamentally challenge the premise of targeting efficiency. Their voices would highlight the administrative burden, stigma, and poverty traps created by means-tested programs.
% DISAPPEARANCE_RATIONALE: If the commitment to targeting efficiency vanished overnight, the entire structure of the welfare state would need to be redesigned. Existing targeted programs would lose their justification, leading to a rapid shift towards universal or less conditional forms of support, fundamentally altering how income is distributed and how social safety nets operate.
% FOUNDING_PROBLEM: The problem of efficiently allocating limited public resources to alleviate poverty and address specific social needs without creating disincentives to work or perceived waste.
% FOUNDING_PROBLEM_CORROBORATION: Policy makers and social policy administrators attest that the problem of efficient resource allocation remains live, citing ongoing budget constraints and the need to justify public spending. Critics, however, argue that the 'efficiency' often comes at the cost of accessibility and dignity, and that the problem has evolved beyond simple allocation to include systemic issues of poverty and inequality that targeted programs cannot fully address.
narrative_ontology:disappearance_verdict(income_support_commitment__targeting_efficiency_reading, world_rearranges).
narrative_ontology:founding_problem_status(income_support_commitment__targeting_efficiency_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(income_support_commitment__targeting_efficiency_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(income_support_commitment__targeting_efficiency_reading, 'none', 1).
narrative_ontology:epsilon_provenance(income_support_commitment__targeting_efficiency_reading, 0.85, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(income_support_commitment__targeting_efficiency_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(income_support_commitment__targeting_efficiency_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(income_support_commitment__targeting_efficiency_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The high extractiveness (0.85) is driven by the scenario where a UBI, funded by cannibalizing existing targeted programs, would result in a net loss for the poorest recipients. This 'efficiency' extracts from the most vulnerable to fund a broader distribution that may not benefit them as much. Suppression (0.7) is high due to the administrative hurdles, stigma, and conditionality inherent in targeted programs, which limit access and create dependency. Theater ratio (0.2) is low because the administrative machinery for means-testing is genuinely complex and functional, though its 'efficiency' is contested. Accessibility collapse (0.6) is moderate; while alternatives like UBI are conceptually available, the political and institutional inertia behind targeted programs makes them difficult to access. Resistance (0.4) is moderate, coming from UBI advocates and some low-income groups who feel excluded or trapped by the current system.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of social policy administrators and taxpayers averse to universal programs, this constraint is a 'rope' or 'scaffold' – an efficient, necessary mechanism for resource allocation. From the perspective of current targeted program recipients (under a UBI counterfactual) and UBI advocates, it operates as a 'snare' – a system that, despite its stated goals, extracts from the most vulnerable and suppresses more equitable alternatives. The engine's classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Current targeted program recipients are structurally complex: they are beneficiaries of the existing system (receiving direct aid) but become victims when considering the alternative of a UBI that would offer more net benefit. Taxpayers averse to universal programs are clear beneficiaries, as their preference for 'efficient' spending is met. Social policy administrators are agenda-setters, benefiting from the institutional structures built around targeting. Advocates for UBI are excluded, bearing the cost of their policy being ignored. Low-income workers above the poverty line are victims, paying taxes for programs they don't qualify for.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate (efficient allocation to need) is still live, but its function is increasingly contested. The classification as a 'snare' prevents mislabeling it as pure coordination by highlighting the hidden extraction from the most vulnerable when a more equitable alternative (UBI) is considered. The 'efficiency' argument, while having a genuine coordination component, is shown to mask significant asymmetric extraction and suppression of alternatives.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    net_impact_of_ubi_replacement,
    'What is the precise net financial impact on current targeted program recipients if a UBI were implemented and fully replaced existing targeted programs?',
    'Detailed micro-simulation studies modeling various UBI designs and their interaction with existing welfare benefits, accounting for phase-outs and administrative savings.',
    'If the net loss for the poorest is consistently high, it strengthens the ''snare'' classification by demonstrating clear victims. If a UBI could be designed to be net-beneficial for all, it would weaken the extraction claim of this reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(net_impact_of_ubi_replacement, empirical, 'Quantifying the financial gain/loss for vulnerable populations under UBI replacement.').

omega_variable(
    political_feasibility_of_alternatives,
    'To what extent is the persistence of targeted programs due to genuine belief in their efficiency versus political resistance to universal programs from powerful stakeholders?',
    'Analysis of lobbying efforts, public discourse framing, and voting patterns of political parties and interest groups regarding welfare reform proposals.',
    'If political resistance is the primary driver, it would increase the ''suppression'' metric and reinforce the ''snare'' classification by highlighting the active suppression of alternatives for extractive purposes.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(political_feasibility_of_alternatives, conceptual, 'Distinguishing genuine policy preference from politically motivated suppression of alternatives.').

omega_variable(
    stigma_and_administrative_burden_cost,
    'How do the non-financial costs of targeted programs (stigma, administrative burden, poverty traps) compare to the ''efficiency'' gains?',
    'Qualitative sociological studies, recipient surveys, and comparative analysis of administrative overhead in targeted vs. universal systems.',
    'High non-financial costs would further justify the ''snare'' classification by revealing hidden forms of extraction and suppression that are not captured by financial metrics alone.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(stigma_and_administrative_burden_cost, empirical, 'Assessing the hidden costs of means-tested welfare programs.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(income_support_commitment__targeting_efficiency_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(inco_tr_t0, income_support_commitment__targeting_efficiency_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(inco_tr_t5, income_support_commitment__targeting_efficiency_reading, theater_ratio, 5, 0.17).
narrative_ontology:measurement(inco_tr_t10, income_support_commitment__targeting_efficiency_reading, theater_ratio, 10, 0.18).
narrative_ontology:measurement(inco_tr_t15, income_support_commitment__targeting_efficiency_reading, theater_ratio, 15, 0.19).
narrative_ontology:measurement(inco_tr_t20, income_support_commitment__targeting_efficiency_reading, theater_ratio, 20, 0.2).

% Extraction over time
narrative_ontology:measurement(inco_be_t0, income_support_commitment__targeting_efficiency_reading, base_extractiveness, 0, 0.75).
narrative_ontology:measurement(inco_be_t5, income_support_commitment__targeting_efficiency_reading, base_extractiveness, 5, 0.78).
narrative_ontology:measurement(inco_be_t10, income_support_commitment__targeting_efficiency_reading, base_extractiveness, 10, 0.81).
narrative_ontology:measurement(inco_be_t15, income_support_commitment__targeting_efficiency_reading, base_extractiveness, 15, 0.83).
narrative_ontology:measurement(inco_be_t20, income_support_commitment__targeting_efficiency_reading, base_extractiveness, 20, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(inco_su_t0, income_support_commitment__targeting_efficiency_reading, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(inco_su_t5, income_support_commitment__targeting_efficiency_reading, suppression_requirement, 5, 0.63).
narrative_ontology:measurement(inco_su_t10, income_support_commitment__targeting_efficiency_reading, suppression_requirement, 10, 0.66).
narrative_ontology:measurement(inco_su_t15, income_support_commitment__targeting_efficiency_reading, suppression_requirement, 15, 0.68).
narrative_ontology:measurement(inco_su_t20, income_support_commitment__targeting_efficiency_reading, suppression_requirement, 20, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(income_support_commitment__targeting_efficiency_reading, resource_allocation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
