% ============================================================================
% CONSTRAINT STORY: debt_trap_compounding
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_debt_trap_compounding, []).

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
 *   constraint_id: debt_trap_compounding
 *   human_readable: Debt Trap Compounding in Institutional Labor Systems
 *   domain: organizational_systems/labor_economics/institutional_extraction
 *
 * SUMMARY:
 *   The debt trap compounding mechanism operates as a downstream consequence
 *   of quota ratchet asymmetry: when workers fall below quota due to
 *   ratchet-induced performance pressure, they face immediate income
 *   shortfalls that threaten their ability to meet basic expenses.
 *   Institutional loans are offered as a solution, framed as temporary
 *   assistance to bridge the gap until performance recovers. However, the
 *   loan terms (interest rates of 15-25% APR, compounding monthly, secured
 *   against future wages) systematically outpace wage growth (2-4% annually),
 *   converting what appears as short-term liquidity into long-term
 *   extraction. The compounding structure creates a debt service burden that
 *   grows faster than the worker's capacity to repay, leading to a
 *   predictable trajectory: initial loan → performance recovery insufficient
 *   to cover debt service → additional borrowing to cover shortfall →
 *   accelerating debt accumulation → wage garnishment → exit capital
 *   depletion → permanent institutional lock. The constraint exhibits high
 *   theater ratio (0.68) because the institutional apparatus surrounding the
 *   loans (financial wellness programs, mandatory counseling, disclosure
 *   forms) is largely performative — the counseling does not address the
 *   structural mismatch between compounding rates and wage growth, and the
 *   disclosure forms document terms that workers have no power to negotiate.
 *   The theater has increased over the measurement interval as regulatory
 *   pressure has forced institutions to add compliance rituals without
 *   changing the underlying extraction mechanism.
 *
 * KEY AGENTS:
 *   - Below-Quota Workers: Primary victims (powerless/trapped) — face immediate performance shortfall with no alternative; loan appears as only option to avoid termination
 *   - Debt-Trapped Workers: Primary victims (powerless/trapped) — wages consumed by debt service; cannot accumulate exit capital; structural lock with no escape path
 *   - Loan Administrators: Primary beneficiaries (institutional/arbitrage) — capture interest revenue with minimal risk; debt secured against future wages
 *   - Institutional Finance Division: Primary beneficiaries (institutional/arbitrage) — profit center extracting wealth from labor force; frames extraction as employee benefit
 *   - Above-Quota Workers: Secondary victims (moderate/constrained) — not currently trapped but constrained by threat; experience wage suppression and ambient coercion
 *   - Labor Organizing Coalition: Organized agents (organized/constrained) — building alternative support pathways through collective bargaining and regulatory reform
 *   - HR Compliance Officer: Institutional actor (institutional/constrained) — maintains performative compliance theater; recognizes dysfunction but constrained by institutional mandate
 *   - Analytical Observer: Civilizational view (analytical/analytical) — sees both genuine coordination function and dominant extraction mechanism
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(debt_trap_compounding, 0.78).
domain_priors:suppression_score(debt_trap_compounding, 0.82).
domain_priors:theater_ratio(debt_trap_compounding, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(debt_trap_compounding, extractiveness, 0.78).
narrative_ontology:constraint_metric(debt_trap_compounding, suppression_requirement, 0.82).
narrative_ontology:constraint_metric(debt_trap_compounding, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(debt_trap_compounding, snare).
narrative_ontology:human_readable(debt_trap_compounding, "Debt Trap Compounding in Institutional Labor Systems").
narrative_ontology:topic_domain(debt_trap_compounding, "organizational_systems/labor_economics/institutional_extraction").

domain_priors:requires_active_enforcement(debt_trap_compounding).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(debt_trap_compounding, loan_administrators).
narrative_ontology:constraint_beneficiary(debt_trap_compounding, institutional_finance_division).
narrative_ontology:constraint_victim(debt_trap_compounding, below_quota_workers).
narrative_ontology:constraint_victim(debt_trap_compounding, debt_service_trapped_workers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DEBT-TRAPPED WORKER (SNARE) — Cannot exit without defaulting; wages consumed by debt service prevent accumulation of exit capital. The loan that was framed as temporary assistance has converted into permanent extraction. Maximum experienced extraction — structural lock with no escape path.
constraint_indexing:constraint_classification(debt_trap_compounding, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: BELOW-QUOTA WORKER PRE-LOAN (SNARE) — Faces immediate performance shortfall with no alternative income source. The loan appears as the only option to avoid termination, but the terms guarantee future extraction. Even before entering the debt cycle, suppression is high — the choice is between accepting extractive terms or losing employment entirely.
constraint_indexing:constraint_classification(debt_trap_compounding, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 3: ABOVE-QUOTA WORKER OBSERVER (TANGLED ROPE) — Not currently trapped but constrained by the threat. Sees colleagues enter debt cycles and recognizes the system as both a safety net (genuine short-term assistance) and an extraction mechanism (compounding terms that guarantee default). Experiences moderate extraction through wage suppression (institutional capture of surplus that could fund higher base wages) and the ambient threat of quota failure.
constraint_indexing:constraint_classification(debt_trap_compounding, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 4: LOAN ADMINISTRATOR (ROPE) — Primary beneficiary. Experiences the constraint as coordination: providing liquidity to workers facing temporary shortfalls. The compounding structure is framed as risk compensation, and the administrator captures interest revenue with minimal risk (debt is secured against future wages, and default triggers termination, which eliminates the liability). Net beneficiary with arbitrage exit — can move to other institutional finance roles.
constraint_indexing:constraint_classification(debt_trap_compounding, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: LABOR ORGANIZING COALITION (SCAFFOLD) — Organized agents (unions, worker advocacy groups, regulatory reformers) see the debt trap as a temporary extraction mechanism with a sunset: collective bargaining for wage floors, regulatory caps on institutional lending rates, and mandatory financial literacy programs are building alternative support pathways. The coalition has constrained exit (faces institutional resistance and legal barriers) but sees a generational path to dismantling the mechanism.
constraint_indexing:constraint_classification(debt_trap_compounding, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: HR COMPLIANCE OFFICER (PITON) — Maintains the performative ritual of financial wellness programs and loan disclosure forms. The compliance theater (mandatory counseling sessions, signed acknowledgment forms, educational materials) persists through institutional inertia and legal liability management, but has minimal functional impact on worker outcomes. The officer recognizes the theater but is constrained by institutional mandate and career risk.
constraint_indexing:constraint_classification(debt_trap_compounding, piton,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (TANGLED ROPE) — From a civilizational perspective, the constraint exhibits both genuine coordination (short-term liquidity provision addresses real cash flow mismatches) and asymmetric extraction (compounding terms systematically transfer wealth from labor to capital). The analytical classification is Tangled Rope rather than Snare because the coordination function is structurally real, even though the extraction dominates. This perspective serves as the basis for the system's computed constraint_claim.
constraint_indexing:constraint_classification(debt_trap_compounding, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(debt_trap_compounding_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(debt_trap_compounding, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(debt_trap_compounding, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(debt_trap_compounding, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(debt_trap_compounding, TR),
    TR >= 0.70.

:- end_tests(debt_trap_compounding_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.78): High. The structural mismatch between compounding rates (15-25% APR) and wage growth (2-4% annually) guarantees that debt service burden grows faster than repayment capacity for the majority of borrowers. Longitudinal data shows median time-to-default of 18-24 months for workers who take initial loans, with 65-75% of borrowers entering accelerating debt cycles. The extraction is not total (0.78 rather than 0.95) because some workers do successfully repay and exit, and the initial loan does provide genuine short-term liquidity that prevents immediate termination. However, the dominant trajectory is extractive: wealth transfer from labor to capital through interest accumulation that systematically exceeds wage growth. Suppression (0.82): Very high. Workers face multiple barriers to exit: (1) debt service consumes 40-60% of wages, preventing accumulation of exit capital; (2) default triggers immediate termination and wage garnishment, eliminating the fallback of leaving without repayment; (3) institutional loans are not dischargeable in bankruptcy in many jurisdictions; (4) regional labor markets often lack alternative employers, making exit geographically costly; (5) the quota ratchet mechanism that triggered the initial shortfall continues to operate, making performance recovery difficult even without debt burden. Suppression is not total (0.82 rather than 0.95) because some workers do exit through geographic relocation, family financial support, or exceptional performance recovery. Theater ratio (0.68): High. The institutional apparatus surrounding the loans is substantially performative: financial wellness programs provide generic budgeting advice that does not address the structural mismatch between debt compounding and wage growth; mandatory counseling sessions document that workers were 'informed' of loan terms but do not provide meaningful alternatives or negotiation power; disclosure forms satisfy legal requirements without changing outcomes. The theater has increased over time as regulatory pressure has forced institutions to add compliance rituals (more forms, more counseling sessions, more educational materials) without addressing the underlying extraction mechanism. The theater is not total (0.68 rather than 0.85) because some compliance measures do provide marginal value (e.g., mandatory waiting periods that prevent impulsive borrowing, caps on total debt-to-income ratios that prevent the most extreme cases).
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates a stark perspectival gap between beneficiaries and victims. Loan administrators see Rope (coordination — providing liquidity to workers facing temporary shortfalls; the compounding structure is framed as legitimate risk compensation). Debt-trapped workers see Snare (pure extraction — the loan that was framed as temporary assistance has converted into permanent wealth transfer with no escape path). Above-quota workers see Tangled Rope (mixed coordination and extraction — the system both provides a safety net and creates a trap; they benefit from the option value of liquidity access but are constrained by the threat of entry and the wage suppression that funds the system). The labor organizing coalition sees Scaffold (temporary extraction with a sunset — collective bargaining and regulatory reform are building alternative support pathways that will dismantle the mechanism over a generational timeframe). The HR compliance officer sees Piton (degraded ritual — the financial wellness programs and disclosure forms persist through institutional inertia and legal liability management but have minimal functional impact on worker outcomes). The analytical observer sees Tangled Rope (genuine coordination function dominated by asymmetric extraction — the short-term liquidity provision is structurally real, but the compounding terms systematically transfer wealth from labor to capital). The gap is not a disagreement about facts but a difference in structural position: beneficiaries with arbitrage exit experience coordination; victims with trapped exit experience extraction; observers with constrained exit experience the hybrid; organized agents with generational time horizons see a sunset path.
 *
 * DIRECTIONALITY LOGIC:
 *   The constraint exhibits clear structural asymmetry in who benefits and who bears costs. Loan administrators and the institutional finance division are primary beneficiaries: they capture interest revenue (15-25% APR on a captive borrower base) with minimal risk (debt is secured against future wages, and default triggers termination, which eliminates the liability). These agents have arbitrage exit options — they can move to other institutional finance roles — and experience the constraint as coordination (providing liquidity services). Their directionality values are low (d ≈ 0.05-0.15), producing negative or near-zero effective extraction. Below-quota workers and debt-trapped workers are primary victims: they bear the full cost of the compounding structure, with wages consumed by debt service and exit options eliminated by negative equity. These agents are trapped (cannot exit without defaulting and losing employment) and experience maximum extraction. Their directionality values are very high (d ≈ 0.92-0.95), producing effective extraction well above the base rate. Above-quota workers occupy an intermediate position: they are not currently trapped but are constrained by the threat (any performance shortfall could trigger entry into the debt cycle) and experience wage suppression (institutional capture of surplus that could fund higher base wages or lower loan rates). Their directionality values are moderate (d ≈ 0.55-0.65), producing effective extraction near the base rate. The labor organizing coalition, despite being victims of the broader system, has organized power and sees a generational exit path through collective bargaining and regulatory reform, producing lower effective extraction (d ≈ 0.40-0.50). The analytical observer recognizes both the genuine coordination function (short-term liquidity provision) and the dominant extraction mechanism (compounding terms that systematically transfer wealth), classifying the constraint as Tangled Rope rather than pure Snare.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION: The constraint resolves the mandatrophy by demonstrating that the Snare classification from the victim's perspective and the Rope classification from the beneficiary's perspective are both structurally accurate readings of the same mechanism. The mandatrophy is not 'is this coordination or extraction?' but 'from which structural position are you measuring?' The loan does provide genuine short-term liquidity (coordination function) AND systematically transfers wealth through compounding terms that outpace wage growth (extraction function). The analytical classification is Tangled Rope because both functions are structurally real, even though extraction dominates. The key insight: the coordination function (liquidity provision) is front-loaded and highly visible, while the extraction function (compounding accumulation) is back-loaded and obscured by temporal discounting. Workers experience the coordination benefit immediately (loan prevents termination) and discount the future extraction cost (compounding burden that will exceed repayment capacity). Beneficiaries experience the reverse: they provide immediate liquidity (cost) and capture long-term interest revenue (benefit). The perspectival gap is not a cognitive error but a structural feature of how the constraint distributes costs and benefits across time and across agents. The Snare classification from the trapped worker's perspective is not 'wrong' — it accurately reflects their experienced reality (maximum extraction, no exit). The Rope classification from the administrator's perspective is not 'wrong' — it accurately reflects their experienced reality (coordination service, net benefit). The Tangled Rope classification from the analytical perspective integrates both: the constraint has a genuine coordination function (liquidity provision addresses real cash flow mismatches) AND an asymmetric extraction function (compounding terms systematically transfer wealth). The mandatrophy dissolves when we recognize that constraint type is indexical — it depends on the observer's structural position — rather than absolute.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    interest_rate_threshold,
    'What interest rate threshold distinguishes legitimate risk compensation from extractive compounding?',
    'Actuarial analysis of default rates vs interest charged; comparison to external lending markets with similar risk profiles; historical correlation between rate structure and worker outcomes',
    'If threshold is market-rate + 2%: many institutional loans are legitimate risk-adjusted lending. If threshold is market-rate + 10%: most institutional loans are extractive. Current institutional rates (15-25% APR) vs external payday lending (300-400% APR) vs credit union rates (8-12% APR) suggest institutional loans sit in an ambiguous middle zone.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(interest_rate_threshold, empirical, 'Interest rate threshold for distinguishing risk compensation from extraction').

omega_variable(
    wage_growth_counterfactual,
    'Would workers'' wages be higher in the absence of institutional lending programs?',
    'Cross-institutional comparison: wage levels at organizations with vs without company loan programs, controlling for industry and worker characteristics; analysis of wage negotiation outcomes before and after loan program implementation',
    'If wages would be higher: the loan program is a wage suppression mechanism disguised as a benefit (extraction is higher than measured). If wages would be the same: the loan program is a pure liquidity service (extraction is lower than measured). If wages would be lower: the loan program is a genuine coordination mechanism that enables employment of higher-risk workers.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(wage_growth_counterfactual, empirical, 'Counterfactual wage levels without institutional lending').

omega_variable(
    default_trigger_mechanism,
    'Is default primarily driven by worker behavior (overspending, poor planning) or by structural features (quota volatility, compounding rates, wage stagnation)?',
    'Longitudinal tracking of borrowers: correlation between default and quota volatility vs correlation between default and discretionary spending; survival analysis controlling for worker characteristics vs structural features',
    'If behavior-driven: the constraint is partly a coordination problem (workers need better financial literacy). If structure-driven: the constraint is pure extraction (the terms guarantee default regardless of worker behavior). Preliminary data suggests structure dominates: default rates correlate more strongly with quota ratchet events than with worker spending patterns.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(default_trigger_mechanism, empirical, 'Primary driver of default: worker behavior vs structural features').

omega_variable(
    exit_capital_threshold,
    'What level of savings constitutes sufficient exit capital for a debt-trapped worker to leave the institution?',
    'Survey data on actual exit decisions: savings levels at time of departure for workers who left while carrying institutional debt vs workers who left debt-free; regional variation in exit capital requirements based on labor market conditions',
    'If threshold is 1-2 months wages: many trapped workers are close to exit (suppression is lower than measured). If threshold is 6-12 months wages: few trapped workers can ever accumulate exit capital (suppression is higher than measured). Current data suggests 3-6 months wages, but this varies significantly by regional labor market tightness.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(exit_capital_threshold, empirical, 'Savings threshold for exit from debt-trapped employment').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(debt_trap_compounding, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(theater_initial, debt_trap_compounding, theater_ratio, 0, 0.45).
narrative_ontology:measurement(theater_early, debt_trap_compounding, theater_ratio, 3, 0.52).
narrative_ontology:measurement(theater_mid, debt_trap_compounding, theater_ratio, 6, 0.61).
narrative_ontology:measurement(theater_final, debt_trap_compounding, theater_ratio, 10, 0.68).

% Extraction over time
narrative_ontology:measurement(extract_initial, debt_trap_compounding, base_extractiveness, 0, 0.58).
narrative_ontology:measurement(extract_early, debt_trap_compounding, base_extractiveness, 3, 0.65).
narrative_ontology:measurement(extract_mid, debt_trap_compounding, base_extractiveness, 6, 0.72).
narrative_ontology:measurement(extract_final, debt_trap_compounding, base_extractiveness, 10, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(debt_trap_compounding, resource_allocation).

% DUAL FORMULATION NOTE:
% This constraint is downstream of quota_ratchet_asymmetry: the quota ratchet creates the performance shortfalls that trigger loan demand. The two constraints form a coupled extraction system: the ratchet generates the crisis, and the loan converts the crisis into permanent wealth transfer. They should be analyzed as a constraint family, with quota_ratchet_asymmetry → debt_trap_compounding in the network graph.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
