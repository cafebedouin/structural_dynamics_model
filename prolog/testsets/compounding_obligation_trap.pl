% ============================================================================
% CONSTRAINT STORY: compounding_obligation_trap
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_compounding_obligation_trap, []).

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
    constraint_indexing:directionality_override/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: compounding_obligation_trap
 *   human_readable: Compounding Obligation Trap in Debt Systems
 *   domain: political_economy/labor_relations/debt_systems
 *
 * SUMMARY:
 *   The compounding obligation trap emerges when debt accumulation rates
 *   structurally exceed the bearer's maximum achievable payment capacity
 *   under actual conditions. This constraint operates across consumer debt
 *   (credit cards, payday loans, student loans), housing (subprime mortgages,
 *   rent-to-own schemes), and labor relations (wage advances, company store
 *   debt). The trap is structural: even if the bearer makes every possible
 *   payment, the compounding mechanism ensures the obligation grows faster
 *   than it can be serviced. The constraint exhibits both genuine
 *   coordination function (credit access solves real intertemporal resource
 *   allocation problems) and asymmetric extraction (compounding terms that
 *   exceed sustainable payment capacity extract wealth through interest,
 *   fees, penalties, and eventual asset seizure). The theater_ratio (0.58)
 *   reflects the performative elements of debt management: payment plans that
 *   cannot mathematically resolve the obligation, financial literacy programs
 *   that frame structural impossibility as personal failure, and forbearance
 *   options that add interest while appearing to provide relief. The
 *   constraint is downstream of unilateral_condition_control (administrators
 *   set terms without negotiation) and legitimation_through_objectivity
 *   (mathematical compounding appears neutral but encodes power asymmetry).
 *
 * KEY AGENTS:
 *   - Obligation Bearer: Primary victim (powerless/trapped) — faces structural impossibility where accumulation rate exceeds maximum payment capacity; cannot exit without severe consequences
 *   - Dependent Household Members: Secondary victims (powerless/identity_locked) — bear extraction consequences through household relationship; identity-constituted dependence prevents independent exit
 *   - Struggling Debtor with Options: Moderate victim (moderate/constrained) — has costly exit options (bankruptcy, consolidation, mobility); experiences both coordination and extraction
 *   - Obligation Administrator: Primary beneficiary (institutional/arbitrage) — captures interest, fees, penalties, and assets upon default; can exit to alternative investments or sell debt
 *   - Secondary Market Investors: Secondary beneficiaries (institutional/arbitrage) — purchase distressed debt at discount; structural trap is priced-in opportunity
 *   - Debt Relief Advocacy Coalition: Organized agents (organized/constrained) — see both coordination and extraction; constrained by political economy of reform
 *   - Regulatory Reform Agency: Institutional actor (institutional/mobile) — sees policy sunset through interest caps, payment ratio limits, bankruptcy reform; sunset is real but contested
 *   - Analytical Observer: Civilizational view (analytical/analytical) — identifies both genuine coordination function and structural extraction; constraint is mutable, not natural law
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(compounding_obligation_trap, 0.68).
domain_priors:suppression_score(compounding_obligation_trap, 0.78).
domain_priors:theater_ratio(compounding_obligation_trap, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(compounding_obligation_trap, extractiveness, 0.68).
narrative_ontology:constraint_metric(compounding_obligation_trap, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(compounding_obligation_trap, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(compounding_obligation_trap, tangled_rope).
narrative_ontology:human_readable(compounding_obligation_trap, "Compounding Obligation Trap in Debt Systems").
narrative_ontology:topic_domain(compounding_obligation_trap, "political_economy/labor_relations/debt_systems").

domain_priors:requires_active_enforcement(compounding_obligation_trap).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(compounding_obligation_trap, obligation_administrator).
narrative_ontology:constraint_beneficiary(compounding_obligation_trap, secondary_market_investors).
narrative_ontology:constraint_victim(compounding_obligation_trap, obligation_bearer).
narrative_ontology:constraint_victim(compounding_obligation_trap, dependent_household_members).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: OBLIGATION BEARER (SNARE) — Trapped by structural impossibility: obligation accumulation rate exceeds maximum achievable payment rate under actual conditions. Cannot exit without default consequences (credit destruction, asset seizure, wage garnishment). Experiences pure extraction with minimal coordination benefit — the 'access to credit' coordination function existed only at origination and has been consumed by the compounding mechanism.
constraint_indexing:constraint_classification(compounding_obligation_trap, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: DEPENDENT HOUSEHOLD MEMBER (SNARE) — Identity-locked rather than materially trapped: bears extraction consequences (reduced household resources, stress, instability) but cannot directly exit because identity is constituted through the household relationship. A child or non-working spouse experiences the trap through the bearer but has no independent exit capacity and often has internalized the obligation as legitimate family burden.
constraint_indexing:constraint_classification(compounding_obligation_trap, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(local))).

% PERSPECTIVE 3: STRUGGLING DEBTOR WITH OPTIONS (TANGLED ROPE) — Constrained but not trapped: has some exit capacity (bankruptcy, debt consolidation, geographic mobility to lower-cost region) at significant cost. Experiences both genuine coordination (the original credit access solved a real need) and extraction (compounding terms that exceed sustainable payment capacity). This agent can see the trap forming and has agency to escape it, unlike the fully trapped bearer.
constraint_indexing:constraint_classification(compounding_obligation_trap, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 4: OBLIGATION ADMINISTRATOR (ROPE) — Primary beneficiary with arbitrage exit options. Experiences the constraint as coordination: providing credit access, managing payment schedules, maintaining records. Extraction flows toward this agent (interest payments, fees, penalties, asset seizures upon default). Can exit to alternative investment vehicles or sell debt to secondary markets. The compounding mechanism is a feature, not a bug — it ensures sustained extraction even when bearer payment capacity is exhausted.
constraint_indexing:constraint_classification(compounding_obligation_trap, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: SECONDARY MARKET INVESTOR (ROPE) — Purchases distressed debt at discount, experiences pure coordination benefit. Arbitrage exit is trivial (sell the debt instrument). Sees the compounding trap as a pricing opportunity: the structural impossibility of full repayment is already priced into the discount, so extraction is 'fair market value.' This perspective reveals how financialization converts trapped debtors into tradable assets.
constraint_indexing:constraint_classification(compounding_obligation_trap, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: DEBT RELIEF ADVOCACY COALITION (TANGLED ROPE) — Organized agents (legal aid societies, debtor unions, bankruptcy reform advocates) see both coordination function (credit access is real) and extraction (compounding terms are predatory). Constrained by political economy: can advocate for reform but cannot unilaterally change contract terms or interest rate structures. Experiences moderate extraction because the constraint limits their advocacy effectiveness while also creating the constituency they organize.
constraint_indexing:constraint_classification(compounding_obligation_trap, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 7: REGULATORY REFORM AGENCY (SCAFFOLD) — Institutional actor with mobile exit (can shift focus to other regulatory domains). Sees the compounding trap as a temporary coordination failure with a policy sunset: interest rate caps, mandatory payment-to-income ratio limits, automatic debt forgiveness after threshold periods, and bankruptcy reform can structurally prevent accumulation rates from exceeding payment capacity. The sunset is real but contested — financial industry lobbying creates significant resistance.
constraint_indexing:constraint_classification(compounding_obligation_trap, scaffold,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER (TANGLED ROPE) — From a civilizational perspective, the compounding obligation trap exhibits both genuine coordination (credit markets solve real intertemporal resource allocation problems) and structural extraction (compounding terms that exceed sustainable payment capacity are not coordination — they are rent extraction enabled by power asymmetry and suppression of alternatives). The constraint is mutable: alternative credit structures (income-share agreements, zero-interest mutual aid, jubilee provisions) exist and have historical precedent. The analytical classification is Tangled Rope, not Mountain — this is not an immutable law of finance.
constraint_indexing:constraint_classification(compounding_obligation_trap, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(compounding_obligation_trap_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(compounding_obligation_trap, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(compounding_obligation_trap, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(compounding_obligation_trap, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(compounding_obligation_trap_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. The compounding mechanism extracts wealth through interest accumulation that exceeds sustainable payment capacity, plus fees, penalties, and eventual asset seizure. The extraction is structural: even perfect compliance cannot resolve the obligation. However, extraction is not maximal (not 0.85+) because some genuine coordination function exists — the original credit access solved a real need, and some bearers do successfully repay. The value reflects that extraction dominates but does not entirely consume the coordination function. Suppression (0.78): High. Alternatives are structurally suppressed through legal barriers (usury law preemption, bankruptcy means testing, criminalization of mutual aid in some contexts), cultural enforcement (debt as moral obligation, default as personal failure), and market concentration (limited access to non-compounding credit). Exit options are severely constrained: default triggers credit destruction, wage garnishment, asset seizure. Bankruptcy provides partial exit but at severe cost. Theater ratio (0.58): Moderate-high. Significant performative content includes payment plans that cannot mathematically resolve the debt, financial literacy programs that individualize structural problems, forbearance options that add interest while appearing to provide relief, and credit counseling that frames the trap as budgeting failure. The theater has increased over the interval as debt instruments have become more complex and the gap between nominal payment options and actual resolution capacity has widened.
 *
 * PERSPECTIVAL GAP:
 *   The compounding obligation trap demonstrates extreme perspectival divergence. The trapped bearer experiences pure extraction (Snare) — the coordination function has been consumed and only the extraction mechanism remains. The identity-locked dependent experiences the same extraction but through relational binding rather than material constraint. The constrained debtor with options experiences mixed coordination and extraction (Tangled Rope) — can see the trap forming and has agency to escape at cost. The obligation administrator experiences pure coordination (Rope) — the compounding mechanism is a feature that ensures sustained returns. Secondary market investors also experience coordination (Rope) — the trap is a pricing opportunity. The debt relief coalition experiences Tangled Rope — sees both the genuine coordination function (credit access) and the extraction mechanism (unsustainable terms). The regulatory reform agency sees a temporary problem with a policy sunset (Scaffold) — interest caps and payment ratio limits can structurally prevent the trap. The analytical observer sees Tangled Rope at the civilizational level — genuine coordination function exists but is entangled with structural extraction enabled by power asymmetry. The gap between Snare (trapped bearer) and Rope (administrator) is the core diagnostic: the same constraint is experienced as pure extraction from below and pure coordination from above.
 *
 * DIRECTIONALITY LOGIC:
 *   The obligation bearer is the primary victim with trapped exit options — derives high d (0.92) leading to maximum experienced extraction. Dependent household members are also victims but identity-locked rather than materially trapped — derives d (0.89) reflecting that the binding is cognitive/relational rather than purely structural. The struggling debtor with options is a victim but constrained rather than trapped — derives moderate d (0.68) reflecting costly but available exit paths. The obligation administrator is the primary beneficiary with arbitrage exit — derives very low d (0.08) leading to negative experienced extraction (net benefit). Secondary market investors are also beneficiaries with arbitrage exit — derives low d (0.12) reflecting that they experience pure coordination benefit (pricing opportunity). The debt relief coalition is neither pure beneficiary nor pure victim — they advocate for bearers but are constrained by political economy — derives moderate d (0.55) reflecting mixed experience. The regulatory reform agency is institutional with mobile exit — derives low d (0.25) reflecting that they can shift focus to other domains and experience the constraint primarily as a policy problem to solve. The analytical observer uses the canonical analytical d (0.72) reflecting the standard analytical context.
 *
 * MANDATROPHY ANALYSIS:
 *   The compounding obligation trap resolves the mandatrophy by demonstrating that Tangled Rope is the correct analytical classification when both coordination function and extraction mechanism are structurally present. The constraint is NOT pure extraction (Snare) from the analytical perspective because genuine coordination function exists: credit access solves real intertemporal resource allocation problems, and many bearers do successfully repay. The constraint is NOT pure coordination (Rope) because the compounding mechanism structurally extracts wealth beyond sustainable payment capacity — this is not a coordination cost, it is asymmetric extraction enabled by power asymmetry and suppression of alternatives. The Tangled Rope classification captures that the constraint has BOTH properties: it coordinates (credit access) AND extracts (unsustainable compounding). The perspectival gap is diagnostic: trapped bearers experience Snare (coordination function consumed), administrators experience Rope (extraction is invisible to them), and the analytical observer sees Tangled Rope (both functions present). The mandatrophy is resolved by recognizing that the analytical classification differs from the experiential classifications, and all are valid readings of the same structural data from different observation sites.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    payment_capacity_measurement,
    'What constitutes ''maximum achievable payment rate under actual conditions'' — gross income, disposable income after subsistence, or disposable income after socially-necessary expenses?',
    'Empirical tracking of default rates across different payment-to-income ratio thresholds; comparison of subsistence definitions across jurisdictions; longitudinal household budget studies',
    'If subsistence-only: many obligations classify as sustainable (low extraction). If socially-necessary expenses included: most obligations classify as extractive traps. The threshold determines whether the structural impossibility is real or a framing effect.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(payment_capacity_measurement, empirical, 'Definition of maximum sustainable payment capacity').

omega_variable(
    origination_intent_vs_outcome,
    'Does the compounding trap result from predatory origination (lenders knowingly issue unsustainable terms) or from exogenous shocks (borrowers could have repaid under expected conditions but faced unemployment, medical crisis, etc.)?',
    'Underwriting standards analysis; correlation between origination loan-to-income ratios and default rates; comparison of default rates in stable vs shock-exposed cohorts',
    'If predatory origination dominates: constraint is Snare from more perspectives (intentional extraction). If exogenous shocks dominate: constraint is Tangled Rope from more perspectives (coordination failure under uncertainty). Mixed causation (likely) supports Tangled Rope with high extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(origination_intent_vs_outcome, empirical, 'Whether trap results from predatory design or exogenous shocks').

omega_variable(
    alternative_suppression,
    'Is the suppression of alternatives (mutual aid, zero-interest lending, jubilee provisions) a structural feature of credit markets or a contingent result of legal and cultural enforcement?',
    'Cross-cultural comparison of credit systems; historical analysis of alternative credit structures (Islamic finance, credit unions, community land trusts); legal barriers to alternative arrangements',
    'If structural: suppression is inherent to credit markets (higher Boltzmann floor). If contingent: suppression is extractive enforcement (lower floor, higher excess extraction). Determines whether the constraint is a coordination necessity or a power asymmetry.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_suppression, conceptual, 'Whether alternative credit structures are structurally suppressed or legally suppressed').

omega_variable(
    bankruptcy_exit_effectiveness,
    'Does bankruptcy provide genuine exit from the compounding trap, or does credit score destruction and asset seizure constitute continued extraction after formal discharge?',
    'Post-bankruptcy outcome tracking: credit access recovery timeline, employment effects of credit checks, housing access with damaged credit; comparison of bankruptcy vs non-bankruptcy debtor trajectories',
    'If bankruptcy is genuine exit: suppression is lower than measured (constrained rather than trapped for some agents). If credit destruction continues extraction: suppression is higher (trapped even after formal discharge). Affects the powerless/moderate boundary.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(bankruptcy_exit_effectiveness, empirical, 'Whether bankruptcy provides genuine exit or continued extraction').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(compounding_obligation_trap, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(theater_origination, compounding_obligation_trap, theater_ratio, 0, 0.35).
narrative_ontology:measurement(theater_early, compounding_obligation_trap, theater_ratio, 3, 0.42).
narrative_ontology:measurement(theater_mid, compounding_obligation_trap, theater_ratio, 6, 0.51).
narrative_ontology:measurement(theater_current, compounding_obligation_trap, theater_ratio, 10, 0.58).

% Extraction over time
narrative_ontology:measurement(extract_origination, compounding_obligation_trap, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(extract_early, compounding_obligation_trap, base_extractiveness, 3, 0.52).
narrative_ontology:measurement(extract_mid, compounding_obligation_trap, base_extractiveness, 6, 0.61).
narrative_ontology:measurement(extract_current, compounding_obligation_trap, base_extractiveness, 10, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(compounding_obligation_trap, resource_allocation).

% DUAL FORMULATION NOTE:
% The compounding obligation trap is downstream of unilateral_condition_control (administrators set terms without negotiation, enabling unsustainable compounding) and legitimation_through_objectivity (mathematical compounding appears neutral but encodes power asymmetry). The trap is a specific instantiation of how unilateral condition-setting produces structural extraction when combined with suppression of alternatives and power asymmetry between bearer and administrator.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(compounding_obligation_trap, organized, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
