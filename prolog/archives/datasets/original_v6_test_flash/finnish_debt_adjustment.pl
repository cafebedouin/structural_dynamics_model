% ============================================================================
% CONSTRAINT STORY: finnish_debt_adjustment
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-01-08
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_finnish_debt_adjustment, []).

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
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: finnish_debt_adjustment
 *   human_readable: Finnish Private Debt Adjustment System
 *   domain: economic/political
 *
 * SUMMARY:
 *   The Finnish private debt adjustment system is a legal framework designed
 *   to provide relief for over-indebted private individuals. It involves a
 *   structured process where debtors can negotiate with their creditors to
 *   reduce their debts and create a manageable repayment plan. The system
 *   aims to balance the interests of debtors and creditors, promoting
 *   financial stability and preventing long-term social and economic
 *   hardship.
 *
 * KEY AGENTS:
 *   - Debtors (compliant): Moderate power/constrained exit
 *   - Debtors (non-compliant): Powerless/trapped exit
 *   - Creditors: Institutional power/arbitrage exit
 *   - Taxpayers: Moderate power/constrained exit
 *   - Analytical Observer: Analytical power/analytical exit
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(finnish_debt_adjustment, 0.55).
domain_priors:suppression_score(finnish_debt_adjustment, 0.4).
domain_priors:theater_ratio(finnish_debt_adjustment, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(finnish_debt_adjustment, extractiveness, 0.55).
narrative_ontology:constraint_metric(finnish_debt_adjustment, suppression_requirement, 0.4).
narrative_ontology:constraint_metric(finnish_debt_adjustment, theater_ratio, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(finnish_debt_adjustment, tangled_rope).
narrative_ontology:human_readable(finnish_debt_adjustment, "Finnish Private Debt Adjustment System").
narrative_ontology:topic_domain(finnish_debt_adjustment, "economic/political").

domain_priors:requires_active_enforcement(finnish_debt_adjustment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(finnish_debt_adjustment, creditors).
narrative_ontology:constraint_beneficiary(finnish_debt_adjustment, debtors_compliant).
narrative_ontology:constraint_victim(finnish_debt_adjustment, debtors_noncompliant).
narrative_ontology:constraint_victim(finnish_debt_adjustment, taxpayers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: Non-compliant Debtor (SNARE) - Debtors who fail to meet the strict requirements of the program face continued extraction without the possibility of debt relief. They are trapped by the system.
constraint_indexing:constraint_classification(finnish_debt_adjustment, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: Compliant Debtor (TANGLED ROPE) - Debtors who successfully navigate the debt adjustment process benefit from debt relief but are constrained by the program's requirements and oversight. They experience both coordination and extraction.
constraint_indexing:constraint_classification(finnish_debt_adjustment, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: Creditors (ROPE) - Creditors benefit from the debt adjustment system by receiving a portion of the debt owed to them, even if it's less than the full amount. The system provides a structured mechanism for recovering some of their losses, reducing uncertainty and potentially avoiding costly legal battles. They can arbitrage the system by factoring potential losses into interest rates.
constraint_indexing:constraint_classification(finnish_debt_adjustment, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: Taxpayers (SNARE) - Taxpayers bear the costs of administering the debt adjustment program. While the program aims to reduce social costs associated with over-indebtedness, the administrative burden and potential for moral hazard create a situation where resources are extracted from taxpayers. They are constrained, as they cannot easily avoid funding the program.
constraint_indexing:constraint_classification(finnish_debt_adjustment, snare,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: Analytical Observer (TANGLED ROPE) - From a global, civilizational perspective, the Finnish debt adjustment system can be seen as a complex mechanism balancing the needs of debtors and creditors. It aims to provide a sustainable solution to over-indebtedness, preventing long-term social and economic consequences. However, the system also involves trade-offs, such as potential moral hazard and administrative costs, requiring careful evaluation and adjustment.
constraint_indexing:constraint_classification(finnish_debt_adjustment, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(finnish_debt_adjustment_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(finnish_debt_adjustment, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(finnish_debt_adjustment, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(finnish_debt_adjustment, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(finnish_debt_adjustment_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness is moderate (0.55) because the system does extract value from debtors (even compliant ones) through the required repayment plan and oversight. However, it also provides a significant benefit in the form of debt relief. The suppression is also moderate (0.40) because debtors have some agency in choosing whether to enter the program, but once enrolled, they are subject to its rules. The theater ratio is low (0.20) because the system is primarily functional, with limited performative aspects.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap arises from the different positions of debtors, creditors, and taxpayers within the system. Debtors, especially those who are non-compliant, may perceive the system as a snare, trapping them in a cycle of debt. Creditors, on the other hand, may view the system as a rope, providing a structured mechanism for recovering some of their losses. Taxpayers bear the costs of the program and may see it as a necessary but extractive measure to prevent greater social costs. The analytical observer attempts to reconcile these perspectives and evaluate the overall effectiveness and fairness of the system.
 *
 * DIRECTIONALITY LOGIC:
 *   Debtors who are compliant with the program experience both coordination and extraction, as they receive debt relief but must adhere to strict requirements. Non-compliant debtors experience the system as a snare because they do not receive the benefits of debt relief but are still subject to collection efforts. Creditors benefit from the system because it provides a structured mechanism for recovering some of their losses. Taxpayers are the victims because they bear the costs of administering the program.
 *
 * MANDATROPHY ANALYSIS:
 *   The Finnish debt adjustment system is classified as a tangled rope because it contains elements of both coordination (providing debt relief and promoting financial stability) and extraction (imposing requirements on debtors and burdening taxpayers). It's important to distinguish this system from a pure snare, which would involve only extraction without any benefit to debtors. It's also important to avoid mislabeling it as a pure rope, as the system does involve costs and constraints for certain stakeholders.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    moral_hazard_extent,
    'To what extent does the debt adjustment program create a moral hazard, encouraging irresponsible borrowing?',
    'Empirical studies analyzing borrowing behavior before and after the introduction of the program, controlling for other factors.',
    'If significant moral hazard: Program should be modified to reduce incentives for excessive borrowing. If minimal moral hazard: Current program design is appropriate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(moral_hazard_extent, empirical, 'Quantifying the moral hazard created by debt adjustment.').

omega_variable(
    program_effectiveness,
    'How effective is the debt adjustment program in reducing long-term social and economic costs associated with over-indebtedness?',
    'Longitudinal studies tracking debtors'' economic outcomes (employment, income, consumption) and social outcomes (mental health, family stability) after debt adjustment.',
    'If highly effective: Program should be expanded and promoted. If ineffective: Program should be redesigned or replaced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(program_effectiveness, empirical, 'Measuring the program''s long-term impact on debtors.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(finnish_debt_adjustment, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(finn_tr_t0, finnish_debt_adjustment, theater_ratio, 0, 0.15).
narrative_ontology:measurement(finn_tr_t5, finnish_debt_adjustment, theater_ratio, 5, 0.2).
narrative_ontology:measurement(finn_tr_t10, finnish_debt_adjustment, theater_ratio, 10, 0.25).

% Extraction over time
narrative_ontology:measurement(finn_be_t0, finnish_debt_adjustment, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(finn_be_t5, finnish_debt_adjustment, base_extractiveness, 5, 0.55).
narrative_ontology:measurement(finn_be_t10, finnish_debt_adjustment, base_extractiveness, 10, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(finnish_debt_adjustment, resource_allocation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
