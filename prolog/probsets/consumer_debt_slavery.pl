% ============================================================================
% CONSTRAINT STORY: consumer_debt_slavery
% ============================================================================
% Version: 1.1 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-01-08
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_consumer_debt_slavery, []).

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
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: consumer_debt_slavery
 *   human_readable: The Interest Trap (Act Your Wage)
 *   domain: economic
 *
 * SUMMARY:
 *   The interest trap describes a situation where low-income individuals and
 *   precarious workers become trapped in a cycle of debt due to high-interest
 *   rates and limited access to capital. This financial constraint results in
 *   a long-term state of financial servitude, with limited opportunities for
 *   upward mobility. Financial institutions benefit from this system, while
 *   government regulators struggle to effectively address the root causes.
 *
 * KEY AGENTS:
 *   - Low-income Borrowers: Primary target (powerless/trapped) - bear the brunt of high-interest rates and debt cycles
 *   - Precarious Workers: Secondary target (moderate/constrained) - reliant on credit but face challenges in repayment
 *   - Financial Institutions: Primary beneficiary (institutional/arbitrage) - profit from interest and fees
 *   - Government Regulators: Attempt to mitigate the trap (institutional/constrained)
 *   - Analytical Observer: Sees systemic issues (analytical/analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(consumer_debt_slavery, 0.85).
domain_priors:suppression_score(consumer_debt_slavery, 0.75).
domain_priors:theater_ratio(consumer_debt_slavery, 0.75).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(consumer_debt_slavery, extractiveness, 0.85).
narrative_ontology:constraint_metric(consumer_debt_slavery, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(consumer_debt_slavery, theater_ratio, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(consumer_debt_slavery, snare).
narrative_ontology:human_readable(consumer_debt_slavery, "The Interest Trap (Act Your Wage)").
narrative_ontology:topic_domain(consumer_debt_slavery, "economic").

domain_priors:requires_active_enforcement(consumer_debt_slavery).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(consumer_debt_slavery, financial_institutions).
narrative_ontology:constraint_beneficiary(consumer_debt_slavery, credit_card_companies).
narrative_ontology:constraint_victim(consumer_debt_slavery, low_income_borrowers).
narrative_ontology:constraint_victim(consumer_debt_slavery, precarious_workers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: LOW-INCOME BORROWERS (SNARE) - Trapped by high-interest rates and the need for essential consumption, exit is nearly impossible due to the debt cycle.
constraint_indexing:constraint_classification(consumer_debt_slavery, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: PRECARIOUS WORKERS (TANGLED ROPE) - Constrained by unstable employment and limited access to capital, they rely on credit but face challenges in repayment, leading to a tangled web of debt and dependence.
constraint_indexing:constraint_classification(consumer_debt_slavery, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: FINANCIAL INSTITUTIONS (ROPE) - Benefit from the interest and fees generated by lending, experiencing the debt cycle as a coordination mechanism that sustains their business model. They have the ability to arbitrage risk.
constraint_indexing:constraint_classification(consumer_debt_slavery, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: GOVERNMENT REGULATORS (PITON) - Regulatory bodies may attempt to mitigate the negative impacts of the interest trap, but often face challenges in effectively addressing the root causes and may become complicit through lobbying or regulatory capture. Their efficacy degrades over time.
constraint_indexing:constraint_classification(consumer_debt_slavery, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: ANALYTICAL OBSERVER (TANGLED ROPE) - Sees the complex interplay of extraction and coordination, recognizing the systemic nature of the debt cycle and its impact on individual financial well-being and economic inequality.
constraint_indexing:constraint_classification(consumer_debt_slavery, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(consumer_debt_slavery_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(consumer_debt_slavery, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(consumer_debt_slavery, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(consumer_debt_slavery, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(consumer_debt_slavery, TR),
    TR >= 0.70.

:- end_tests(consumer_debt_slavery_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.85): High, reflecting the significant financial burden imposed on borrowers. Suppression (0.75): High, due to limited access to alternative financial options and the essential nature of credit for basic needs. Theater ratio (0.75): The theater ratio is moderate, reflecting the performative aspects of financial institutions presenting themselves as helpful resources while extracting significant wealth.
 *
 * PERSPECTIVAL GAP:
 *   The low-income borrowers experience the system as a snare, with little hope for escape. Financial institutions view it as a well-functioning rope, providing them with a reliable revenue stream. Government regulators see themselves as constrained actors, attempting to balance the needs of borrowers and lenders. The analytical observer recognizes the systemic issues at play, seeing a tangled rope of extraction and coordination.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality is determined by the beneficiaries and victims of the constraint. Financial institutions benefit from the interest and fees, while low-income borrowers bear the costs of high-interest rates and debt cycles. This asymmetry drives the engine to classify this as a snare or tangled rope.
 *
 * MANDATROPHY ANALYSIS:
 *   The interest trap is primarily an extractive mechanism, rather than a coordination system. Although lenders provide a service by extending credit, the high-interest rates and fees extract a disproportionate amount of wealth from borrowers, perpetuating a cycle of debt. The classification prevents mislabeling this as a coordination system by focusing on the power imbalance and the coercive nature of the debt cycle. The high extractiveness is justified by the limited alternatives and essential nature of credit for basic survival in many cases.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    income_inequality_threshold,
    'What level of income inequality exacerbates the interest trap, making it more difficult for individuals to escape debt?',
    'Statistical analysis of income distribution and debt levels, identifying the tipping point where debt becomes unsustainable for a significant portion of the population.',
    'If inequality is high: Debt becomes inescapable for many. If inequality is low: Debt is manageable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(income_inequality_threshold, empirical, 'Income inequality threshold').

omega_variable(
    financial_literacy_effectiveness,
    'How effective are financial literacy programs in mitigating the interest trap, and do they address the underlying structural issues?',
    'Evaluation of financial literacy programs, measuring their impact on borrowing behavior and financial outcomes.',
    'If effective: Individuals are better equipped to manage debt. If ineffective: Programs are insufficient.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(financial_literacy_effectiveness, empirical, 'Financial literacy effectiveness').

omega_variable(
    alternative_financial_systems,
    'Can alternative financial systems (e.g., credit unions, microfinance) provide a viable alternative to traditional lending, reducing the reliance on high-interest debt?',
    'Comparative analysis of different financial systems, assessing their accessibility, affordability, and impact on financial well-being.',
    'If viable: Alternative systems alleviate the interest trap. If not: Traditional lending persists.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_financial_systems, empirical, 'Viability of alternative financial systems').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(consumer_debt_slavery, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cons_tr_t0, consumer_debt_slavery, theater_ratio, 0, 0.6).
narrative_ontology:measurement(cons_tr_t5, consumer_debt_slavery, theater_ratio, 5, 0.7).
narrative_ontology:measurement(cons_tr_t10, consumer_debt_slavery, theater_ratio, 10, 0.75).

% Extraction over time
narrative_ontology:measurement(cons_be_t0, consumer_debt_slavery, base_extractiveness, 0, 0.75).
narrative_ontology:measurement(cons_be_t5, consumer_debt_slavery, base_extractiveness, 5, 0.8).
narrative_ontology:measurement(cons_be_t10, consumer_debt_slavery, base_extractiveness, 10, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(consumer_debt_slavery, resource_allocation).
narrative_ontology:affects_constraint(consumer_debt_slavery, wage_stagnation).
narrative_ontology:affects_constraint(consumer_debt_slavery, predatory_lending).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
