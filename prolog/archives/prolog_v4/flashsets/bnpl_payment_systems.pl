% ============================================================================
% CONSTRAINT STORY: bnpl_payment_systems
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-01-08
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_bnpl_payment_systems, []).

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
 *   constraint_id: bnpl_payment_systems
 *   human_readable: "Buy Now, Pay Later" (BNPL) Payment Systems
 *   domain: economic
 *
 * SUMMARY:
 *   "Buy Now, Pay Later" (BNPL) services are integrated into online retail
 *   checkouts, offering consumers the ability to split payments for
 *   purchases, often interest-free. This system benefits BNPL providers and
 *   retailers by increasing sales, but it poses risks to consumers,
 *   particularly those with low incomes or limited financial literacy.
 *
 * KEY AGENTS:
 *   - BNPL Providers: Primary beneficiaries (institutional/arbitrage) - Increased sales and fees.
 *   - Online Retailers: Secondary beneficiaries (institutional/arbitrage) - Increased sales.
 *   - Low-Income Borrowers: Primary victims (powerless/trapped) - Debt traps and damaged credit scores.
 *   - Unwary Consumers: Secondary victims (moderate/constrained) - Hidden fees and accumulating debt.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(bnpl_payment_systems, 0.55).
domain_priors:suppression_score(bnpl_payment_systems, 0.4).
domain_priors:theater_ratio(bnpl_payment_systems, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(bnpl_payment_systems, extractiveness, 0.55).
narrative_ontology:constraint_metric(bnpl_payment_systems, suppression_requirement, 0.4).
narrative_ontology:constraint_metric(bnpl_payment_systems, theater_ratio, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(bnpl_payment_systems, tangled_rope).
narrative_ontology:human_readable(bnpl_payment_systems, "\"Buy Now, Pay Later\" (BNPL) Payment Systems").
narrative_ontology:topic_domain(bnpl_payment_systems, "economic").

domain_priors:requires_active_enforcement(bnpl_payment_systems).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(bnpl_payment_systems, bnpl_providers).
narrative_ontology:constraint_beneficiary(bnpl_payment_systems, online_retailers).
narrative_ontology:constraint_victim(bnpl_payment_systems, low_income_borrowers).
narrative_ontology:constraint_victim(bnpl_payment_systems, unwary_consumers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Perspective of low-income borrowers: BNPL can be a debt trap, leading to late fees and damaged credit scores. Trapped by limited financial literacy and impulsive purchasing.
constraint_indexing:constraint_classification(bnpl_payment_systems, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% Perspective of unwary consumers: Benefit from short-term convenience but can face hidden fees and accumulating debt. Constrained by lack of awareness of the risks.
constraint_indexing:constraint_classification(bnpl_payment_systems, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% Perspective of BNPL providers: Benefit from increased sales volume and fees. Can arbitrage by shifting risk to consumers and retailers.
constraint_indexing:constraint_classification(bnpl_payment_systems, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% Perspective of online retailers: Benefit from increased sales volume. Can arbitrage increased sales against the fees paid to BNPL providers.
constraint_indexing:constraint_classification(bnpl_payment_systems, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% Perspective of an analytical observer: BNPL systems facilitate increased consumption, but also introduce risks of debt accumulation for vulnerable populations. Exhibits both coordination (increased sales) and asymmetric extraction (fees and debt).
constraint_indexing:constraint_classification(bnpl_payment_systems, tangled_rope,
    context(agent_power(analytical),
            time_horizon(generational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(bnpl_payment_systems_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(bnpl_payment_systems, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(bnpl_payment_systems, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(bnpl_payment_systems, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(bnpl_payment_systems_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.55): Moderate. BNPL providers extract value through fees and interest (where applicable), but also provide a service to consumers and retailers by facilitating purchases. Suppression (0.40): Moderate. BNPL providers often use marketing and partnerships to suppress alternative payment methods. The convenience suppresses comparison shopping and sound budgeting.
 *
 * PERSPECTIVAL GAP:
 *   Low-income borrowers experience BNPL as a snare because they are trapped by their financial circumstances. BNPL Providers experience the system as pure coordination (rope) because they profit from increased sales without directly experiencing negative consequences. Analytical observer sees mixed coordination and extraction (tangled rope) because the system benefits some while harming others.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (BNPL providers and online retailers) have arbitrage options; d is low, so chi is low. Victims (low-income borrowers) are trapped; d is high, so chi is high. Unwary consumers are constrained, so d is moderate and chi is moderate.
 *
 * MANDATROPHY ANALYSIS:
 *   The system can be classified as tangled rope because it offers both a coordination function (facilitating transactions) and asymmetric extraction (fees and debt). The perspectives highlight the complex interplay of benefits and risks.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    debt_accumulation_threshold,
    'What level of BNPL debt leads to significant financial distress for borrowers?',
    'Analysis of BNPL usage and credit scores.',
    'Higher threshold would indicate lower risk of debt traps; lower threshold indicates a significant risk.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(debt_accumulation_threshold, empirical, 'Identifies significant levels of BNPL debt.').

omega_variable(
    regulation_effectiveness,
    'How effective are regulations in protecting consumers from BNPL risks?',
    'Analysis of regulations and their impact on consumer behavior and debt levels.',
    'High effectiveness would reduce extraction; low effectiveness would maintain or increase extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulation_effectiveness, empirical, 'Effectiveness of regulations surrounding BNPL.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(bnpl_payment_systems, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bnpl_tr_t0, bnpl_payment_systems, theater_ratio, 0, 0.1).
narrative_ontology:measurement(bnpl_tr_t5, bnpl_payment_systems, theater_ratio, 5, 0.2).
narrative_ontology:measurement(bnpl_tr_t10, bnpl_payment_systems, theater_ratio, 10, 0.3).

% Extraction over time
narrative_ontology:measurement(bnpl_be_t0, bnpl_payment_systems, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(bnpl_be_t5, bnpl_payment_systems, base_extractiveness, 5, 0.45).
narrative_ontology:measurement(bnpl_be_t10, bnpl_payment_systems, base_extractiveness, 10, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(bnpl_payment_systems, resource_allocation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
