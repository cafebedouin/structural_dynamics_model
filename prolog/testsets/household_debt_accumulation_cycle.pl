% ============================================================================
% CONSTRAINT STORY: household_debt_accumulation_cycle
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_household_debt_accumulation_cycle, []).

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
    constraint_indexing:directionality_override/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: household_debt_accumulation_cycle
 *   human_readable: Household Debt Accumulation Cycle
 *   domain: economic/financial
 *
 * SUMMARY:
 *   The household debt accumulation cycle represents a structural constraint
 *   where financial institutions systematically extract wealth from
 *   households through asymmetric credit mechanisms while presenting the
 *   constraint as a coordination solution enabling consumption and
 *   investment. The cycle operates through reinforcing mechanisms: wage
 *   stagnation forces households to borrow for essential expenses (housing,
 *   healthcare, education); rising interest rates and fees increase debt
 *   service burden; debt service consumes income that would otherwise fund
 *   savings, forcing continued borrowing; longer debt repayment periods
 *   extend extraction window and increase total interest paid. This
 *   constraint exhibits the full range of DR classification depending on
 *   observer position. Financial institutions experience pure coordination
 *   (Rope) — credit enables efficient capital allocation. Wage-stagnant
 *   households experience pure extraction (Snare) — they are trapped in
 *   perpetual debt service with no deleveraging exit. Moderate borrowers
 *   experience mixed coordination and extraction (Tangled Rope) — credit
 *   access is genuinely useful for major life investments (home, education)
 *   but extraction mechanisms (compounding fees, rate adjustments,
 *   securitization spreads) are substantial and asymmetric. The policy
 *   coalition experiences constrained enforcement (Tangled Rope) — regulatory
 *   mechanisms exist but are captured by financial sector lobbying. The
 *   consumer credit narrative (Piton) maintains that household borrowing is a
 *   rational, optional choice for lifestyle enhancement, despite evidence
 *   that debt is driven by necessity (wage inadequacy) rather than choice.
 *   The analytical observer risks naturalizing the cycle as an inevitable
 *   feature of modern capitalism (Mountain), but the structural beneficiary
 *   declarations and escalating suppression metrics reveal this as a false
 *   summit.
 *
 * KEY AGENTS:
 *   - Wage-Stagnant Households: Primary victim (powerless/trapped) — lack bargaining power to demand higher wages; forced to borrow for essential expenses; debt service traps income
 *   - Financial Institutions: Primary beneficiary (institutional/arbitrage) — extract through interest margins, fees, securitization spreads; maintain arbitrage optionality by exiting individual borrower relationships
 *   - Rate-Sensitive Borrowers: Secondary victim (moderate/constrained) — use credit legitimately for homes and education but face extraction through interest rate adjustments and fee structures
 *   - Intergenerational Wealth Transfer: Systemic victim (analytical/trapped) — debt cycle suppresses household savings and asset accumulation, blocking multigenerational wealth building
 *   - Policy Coalition: Constrained enforcer (organized/constrained) — regulatory bodies and consumer advocates face political economy barriers to effective constraints on extraction
 *   - Consumer Credit Narrative: Institutional theater (institutional/arbitrage) — the rhetorical framing that presents debt as optional choice rather than forced necessity
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(household_debt_accumulation_cycle, 0.58).
domain_priors:suppression_score(household_debt_accumulation_cycle, 0.68).
domain_priors:theater_ratio(household_debt_accumulation_cycle, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(household_debt_accumulation_cycle, extractiveness, 0.58).
narrative_ontology:constraint_metric(household_debt_accumulation_cycle, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(household_debt_accumulation_cycle, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(household_debt_accumulation_cycle, tangled_rope).
narrative_ontology:human_readable(household_debt_accumulation_cycle, "Household Debt Accumulation Cycle").
narrative_ontology:topic_domain(household_debt_accumulation_cycle, "economic/financial").

domain_priors:requires_active_enforcement(household_debt_accumulation_cycle).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(household_debt_accumulation_cycle, financial_institutions).
narrative_ontology:constraint_beneficiary(household_debt_accumulation_cycle, creditors).
narrative_ontology:constraint_victim(household_debt_accumulation_cycle, wage_stagnant_households).
narrative_ontology:constraint_victim(household_debt_accumulation_cycle, intergenerational_wealth_transfer).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DEBT-TRAPPED HOUSEHOLD (SNARE) — Faces structural wage stagnation and cannot exit through savings or deleveraging. Rising housing costs, healthcare expenses, and education force borrowing for essential needs. Debt service consumes increasing share of income, trapping household in perpetual extraction cycle. No meaningful exit options; maximum experienced extractiveness.
constraint_indexing:constraint_classification(household_debt_accumulation_cycle, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: RATE-SENSITIVE BORROWER (TANGLED ROPE) — Uses credit for legitimate household needs (home purchase, education) but faces extraction through interest rate mechanisms and fee structures. Some agency (can refinance, consolidate debt) but constrained by credit score penalties and limited alternative lenders. Genuine coordination function exists alongside asymmetric extraction.
constraint_indexing:constraint_classification(household_debt_accumulation_cycle, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: FINANCIAL INSTITUTION (ROPE) — Experiences the constraint as coordination: credit markets allocate capital efficiently, enable consumption smoothing, and facilitate productive investment. Net beneficiary through interest margins, fees, and securitization spreads. High exit optionality (arbitrage to other markets, securitization, derivative hedging). Constraint appears as low-friction wealth transfer mechanism.
constraint_indexing:constraint_classification(household_debt_accumulation_cycle, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: POLICY COALITION (TANGLED ROPE) — Regulatory bodies, consumer protection agencies, credit counseling organizations see both coordination (credit access enables mobility) and extraction (predatory lending, fee spirals). Constrained by political economy: financial sector lobbying limits regulatory tightening; consumer protection mandates face compliance cost pass-through. Genuine enforcement exists but asymmetrically applied.
constraint_indexing:constraint_classification(household_debt_accumulation_cycle, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: CONSUMER CREDIT NARRATIVE (PITON) — The rhetorical justification for household debt ('building credit history,' 'investing in human capital,' 'consumption smoothing for lifecycle') persists as institutional theater despite growing evidence that debt accumulation is driven by wage stagnation, not consumption choice. The narrative maintains institutional legitimacy for extraction mechanisms. Theater ratio reflects the gap between stated coordination function and actual extraction driver.
constraint_indexing:constraint_classification(household_debt_accumulation_cycle, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, the argument claims that debt accumulation is an inevitable consequence of economic growth, financial innovation, and lifecycle consumption patterns. This perspective sees the cycle as arising naturally from individual rational choice and market equilibrium. However, the beneficiary declarations (financial institutions extracting through asymmetric information and fee structures) and suppression mechanisms (structural barriers to deleveraging, wage stagnation) indicate this is a false summit — naturalization of contingent institutional arrangements.
constraint_indexing:constraint_classification(household_debt_accumulation_cycle, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(household_debt_accumulation_cycle_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(household_debt_accumulation_cycle, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(household_debt_accumulation_cycle, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(household_debt_accumulation_cycle, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(household_debt_accumulation_cycle, TR),
    TR >= 0.70.

:- end_tests(household_debt_accumulation_cycle_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high and rising. The base extractiveness reflects that financial institutions capture substantial wealth through interest margins (average 6-8% on mortgages, 15-25% on credit cards), origination fees (1-5% on mortgages), annual fees, late fees, and prepayment penalties. The rising trajectory (0.32→0.58 over interval) reflects both increasing debt volumes and increasing fee structures and interest rates over the measurement period. The extraction is not maximal (0.58 < 0.66 snare threshold for χ) because some households access credit at reasonable rates and for legitimate productive purposes (home ownership, education). Suppression (0.68): High and escalating. Barriers to deleveraging include: credit scoring penalties that persist 7+ years after default; refinancing barriers (loan-to-value requirements exceed asset values for underwater borrowers); legal barriers (bankruptcy discharge exclusions for student loans); structural wage inadequacy that prevents savings accumulation; and psychology of debt shame/identity fusion. The rising trajectory (0.48→0.68) reflects intensifying enforcement mechanisms: stricter credit reporting, expanded debt collection practices, student loan payment garnishment provisions. Theater Ratio (0.65): Moderate-high. The 'consumer credit' narrative presents household borrowing as enabling choice (consumption smoothing, investment in human capital, lifecycle optimization) while empirical drivers are necessity (wage stagnation, healthcare costs, education inflation, housing cost growth). The rising trajectory (0.45→0.65) reflects increasing gap between stated purpose (choice-enabling) and actual driver (necessity-driven).
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap in this constraint is extreme and diagnostic. The financial institution sees Rope — efficient capital allocation enabling consumer choice and productive investment. The wage-stagnant household sees Snare — trapped in perpetual debt service with no exit. The rate-sensitive borrower sees Tangled Rope — legitimate access to credit for homes/education alongside extraction through fees and rate adjustments. The policy coalition sees Tangled Rope — genuine coordination function (credit access matters) constrained by enforcement capture. The piton perspective reveals that the consumer credit narrative (rationality, choice, lifecycle optimization) has become decoupled from the mechanism's actual function (necessity-driven extraction). The analytical observer risks the false-summit error: presenting the cycle as natural outcome of capitalism rather than as a constructed institutional arrangement. The perspectival gap reveals that the constraint's type depends entirely on which agent's structural position you measure from — and that the dominant beneficiary (financial institutions) experience the constraint as coordination while the dominant victims experience extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality derives from the agent's structural position relative to the extraction flow. Financial institutions are net beneficiaries with arbitrage optionality — they can exit individual borrower relationships, securitize debt, or deploy capital elsewhere. Their d-value is low (approximately 0.10-0.20), producing negative or near-zero effective extractiveness (χ) — they experience the constraint as pure coordination or mild benefit, not extraction. Wage-stagnant households are trapped victims with no exit options — they experience maximum extractiveness. Their d-value is approximately 0.95, producing high f(d) ≈ 1.42, multiplying their experienced χ significantly. Moderate borrowers with constrained exit (can refinance, change jobs with cost, but cannot simply walk away from home equity) have mid-range d ≈ 0.65-0.75, experiencing χ in the 0.35-0.50 range. Policy actors face asymmetric pressure: they can theoretically regulate, but financial sector lobbying constrains enforcement, producing constrained rather than mobile exit. The piton perspective captures the theater: the narrative justification persists (maintains legitimacy) while functional coordination has atrophied relative to extraction mechanics.
 *
 * MANDATROPHY ANALYSIS:
 *   The household debt accumulation cycle resolves the mandatrophy by showing that Tangled Rope is the only coherent classification that captures the genuine coordination function (credit enabling home ownership, education, consumption smoothing) alongside the asymmetric extraction mechanism (interest margins, fees, securitization spreads, and suppression of deleveraging). A Rope-only classification would require denying the extraction: 'credit is just efficient allocation.' A Snare-only classification would require denying the coordination: 'all household credit is predatory.' The tangled classification preserves both: credit markets perform genuine coordination functions AND financial institutions use asymmetric information, regulatory capture, and suppression mechanisms to extract substantially more than the coordination function requires. The rising extractiveness and suppression metrics over time suggest the extraction mechanism is strengthening relative to coordination function — the tangle is becoming more snare-like. The piton perspective reveals that the coordination narrative ('building credit history,' 'investing in human capital') is increasingly theater. This suggests the constraint may be drifting from Tangled Rope toward Snare over a longer interval, but at current metrics (suppression 0.68, χ ≈ 0.50-0.65), Tangled Rope remains the accurate classification.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    wage_stagnation_causation,
    'Is wage stagnation an exogenous macroeconomic trend or an endogenous result of extractive debt mechanisms that reduce household bargaining power?',
    'Time-series analysis correlating debt-to-income ratios with wage growth rates; international comparison with lower-debt-ratio economies; analysis of household bargaining power by debt quintile',
    'If exogenous: household borrowing is a rational response to external shock (Rope/Tangled Rope). If endogenous: debt accumulation suppresses wages, creating feedback cycle that locks households into extraction (shifts classification toward Snare).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(wage_stagnation_causation, empirical, 'Whether wage stagnation drives debt or debt suppresses wages').

omega_variable(
    credit_access_necessity,
    'To what extent is household credit access genuinely necessary for consumption smoothing vs. a mechanism that substitutes for wage adequacy and social safety nets?',
    'Comparative analysis: credit-dependent economies vs. high-wage/strong-safety-net economies (Nordic, German models); tracking of credit usage for essential (healthcare, education) vs. discretionary (consumption, lifestyle) purposes',
    'If necessary: genuine coordination function justifies Rope/Tangled Rope classification. If substitution: credit enables institutional extraction instead of filling coordination gap, shifting toward Snare for trapped households.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(credit_access_necessity, conceptual, 'Whether credit access is necessary coordination or substitution for adequate wages').

omega_variable(
    intergenerational_wealth_transfer_mechanism,
    'Does household debt accumulation systematically redirect wealth from wage-earning households to financial institutions and asset-owners, thereby suppressing intergenerational wealth transfer?',
    'Wealth-flow accounting: proportion of household income flowing to debt service vs. savings/investment; correlation between household debt levels and intergenerational wealth mobility; analysis of debt burden by race and ethnic group (revealing historical asset gaps)',
    'If yes: debt cycle is extraction mechanism that locks out multigenerational wealth building (Snare). If no or minor: debt is temporary burden recovered through income growth (Rope/Tangled Rope).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(intergenerational_wealth_transfer_mechanism, empirical, 'Whether debt accumulation suppresses intergenerational wealth transfer').

omega_variable(
    suppression_mechanism_structural_vs_psychological,
    'What proportion of the measured suppression (0.68) is structural (legal barriers to deleveraging, credit scoring penalties) vs. internalized (psychological commitment to debt repayment, identity fusion with consumer role)?',
    'Post-debt-forgiveness studies: how suppression changes after structured debt relief programs; psychological markers of debt-shame internalization; comparison of suppression levels in bankruptcy-reform regimes (Chapter 7 vs Chapter 13 states)',
    'If structural dominant: barriers persist even after debt cleared (high mandatrophy risk). If psychological dominant: households carry suppression after exit (internalized mechanism requires identity reframing, not just financial restructuring).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_vs_psychological, empirical, 'Structural vs. internalized suppression mechanisms in debt cycle').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(household_debt_accumulation_cycle, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hdac_tr_t0, household_debt_accumulation_cycle, theater_ratio, 0, 0.45).
narrative_ontology:measurement(hdac_tr_t5, household_debt_accumulation_cycle, theater_ratio, 5, 0.55).
narrative_ontology:measurement(hdac_tr_t10, household_debt_accumulation_cycle, theater_ratio, 10, 0.65).

% Extraction over time
narrative_ontology:measurement(hdac_be_t0, household_debt_accumulation_cycle, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(hdac_be_t5, household_debt_accumulation_cycle, base_extractiveness, 5, 0.45).
narrative_ontology:measurement(hdac_be_t10, household_debt_accumulation_cycle, base_extractiveness, 10, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(hdac_su_t0, household_debt_accumulation_cycle, suppression_requirement, 0, 0.48).
narrative_ontology:measurement(hdac_su_t5, household_debt_accumulation_cycle, suppression_requirement, 5, 0.58).
narrative_ontology:measurement(hdac_su_t10, household_debt_accumulation_cycle, suppression_requirement, 10, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(household_debt_accumulation_cycle, resource_allocation).
narrative_ontology:affects_constraint(household_debt_accumulation_cycle, wage_stagnation_structural).
narrative_ontology:affects_constraint(household_debt_accumulation_cycle, credit_score_systemic_penalty).
narrative_ontology:affects_constraint(household_debt_accumulation_cycle, student_debt_trap).
narrative_ontology:affects_constraint(household_debt_accumulation_cycle, healthcare_debt_bankruptcy).

% DUAL FORMULATION NOTE:
% The household debt accumulation cycle is upstream of specific debt traps (student loans, medical debt, housing debt) but represents a distinct structural constraint operating across all consumer credit categories. Each specific debt trap has its own extractiveness values reflecting domain-specific mechanisms; the accumulation cycle captures the meta-mechanism that binds them together: necessity-driven borrowing + asymmetric fee structures + suppression of deleveraging.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(household_debt_accumulation_cycle, institutional, 0.12).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
