% ============================================================================
% CONSTRAINT STORY: consumer_debt_slavery
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
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
    constraint_indexing:directionality_override/3,
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
 *   domain: economic/financial_servitude
 *
 * SUMMARY:
 *   The Interest Trap represents a structural mechanism of financial
 *   extraction targeting individuals with low capital reserves and limited
 *   income stability. The constraint operates across multiple time horizons —
 *   immediate (monthly payment obligations), biographical (lifetime debt
 *   servitude), and generational (inherited financial precarity). The
 *   mechanism is deceptively simple: provide access to credit at rates that
 *   compound faster than income growth, coupled with essential consumption
 *   needs and depreciating asset purchases that create recurring debt cycles.
 *   The constraint exhibits both Snare characteristics (high extraction, high
 *   suppression, victims with no exit) and institutional complexity (multiple
 *   perspectives from powerless wage earners to institutional beneficiaries).
 *   The theater ratio (0.45) is relatively low, indicating that the mechanism
 *   operates with minimal performative overlay — the extraction is
 *   structurally direct: borrow X, pay back 1.5X-2.5X through compound
 *   interest and fees. Regulatory apparatus (Truth in Lending, usury caps,
 *   bankruptcy reform) maintains theatrical appearance of consumer protection
 *   while structural extraction continues through regulatory arbitrage and
 *   loophole exploitation.
 *
 * KEY AGENTS:
 *   - Low-Capital Wage Earners: Primary victims (powerless/trapped) — bear extraction through interest payments and opportunity costs; no exit due to income precarity and essential consumption needs
 *   - Credit Issuers (Banks, Credit Card Companies): Primary beneficiaries (institutional/arbitrage) — extract through interest rates, fees, and portfolio securitization; have abundant exit options and capital allocation flexibility
 *   - Payday & Predatory Lenders: Secondary beneficiaries (powerful/mobile) — extract at highest rates; operate in regulatory gaps; highly mobile between jurisdictions and lending models
 *   - Multi-Generational Families: Extended victims (moderate/constrained) — experience debt propagation across generations; reduced intergenerational mobility and opportunity
 *   - Consumer Advocacy & Debt Relief Organizations: Organized agents (organized/constrained) — work to provide exits through forgiveness programs, counseling, policy reform; have constrained resources relative to extraction scale
 *   - Regulatory Agencies: Institutional performers (institutional/arbitrage) — maintain regulatory theater; weak enforcement; regulatory capture by lenders
 *   - High-Income Strategic Borrowers: Beneficiaries with mixed status (powerful/mobile) — use credit for wealth accumulation; experience same constraint as Tangled Rope (coordination + asymmetric extraction)
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing contingent policy design as economic inevitability
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(consumer_debt_slavery, 0.68).
domain_priors:suppression_score(consumer_debt_slavery, 0.75).
domain_priors:theater_ratio(consumer_debt_slavery, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(consumer_debt_slavery, extractiveness, 0.68).
narrative_ontology:constraint_metric(consumer_debt_slavery, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(consumer_debt_slavery, theater_ratio, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(consumer_debt_slavery, snare).
narrative_ontology:human_readable(consumer_debt_slavery, "The Interest Trap (Act Your Wage)").
narrative_ontology:topic_domain(consumer_debt_slavery, "economic/financial_servitude").

domain_priors:requires_active_enforcement(consumer_debt_slavery).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(consumer_debt_slavery, credit_issuers).
narrative_ontology:constraint_beneficiary(consumer_debt_slavery, financial_services_institutions).
narrative_ontology:constraint_victim(consumer_debt_slavery, low_capital_borrowers).
narrative_ontology:constraint_victim(consumer_debt_slavery, wage_earners).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: WAGE EARNER (SNARE) — No exit. Borrower with minimal capital reserves faces immediate consumption needs (medical, housing, transportation) and depreciating assets (cars, furniture). Interest accumulation and minimum payments consume 30-50% of monthly income. Refinancing extends the trap. Default risks homelessness or medical bankruptcy. Maximum experienced extraction — the constraint's raison d'être is extracting lifetime income from those who have no alternatives.
constraint_indexing:constraint_classification(consumer_debt_slavery, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: MULTI-GENERATIONAL FAMILY (SNARE/CONSTRAINED) — Parents in debt pass financial precarity to children; reduced investment in education, health, and opportunity accumulation. Children inherit reduced mobility and face same credit access constraints. Exit is theoretically possible (higher education, geographic mobility, delayed consumption) but practically constrained by parental debt burden and reduced social capital. High experienced extraction across generational time horizon.
constraint_indexing:constraint_classification(consumer_debt_slavery, snare,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: CREDIT ISSUER (ROPE) — Banks, credit card companies, payday lenders experience the debt structure as coordination: they allocate capital to borrowers who need immediate access; borrowers commit to repayment schedules; both parties benefit from the arrangement. From institutional perspective, interest payments are legitimate risk compensation and market pricing. Arbitrage available — can securitize debt, sell portfolios, or change lending criteria. Net beneficiary but sees the constraint as functioning coordination mechanism.
constraint_indexing:constraint_classification(consumer_debt_slavery, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: POLICY/CONSUMER ADVOCACY (SCAFFOLD) — Debt forgiveness programs, credit counseling, bankruptcy reform, and usury caps represent organized attempts to provide exits from the trap. These mechanisms have sunset logic: they aim to rebalance credit access by reducing extraction rates and interest caps. High suppression remains structural (poverty and income volatility persist), but these mechanisms reduce theater and create exit pathways. Organized agents with agency and policy levers see temporary enforcement, not permanent extraction.
constraint_indexing:constraint_classification(consumer_debt_slavery, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: WEALTHY BORROWER (TANGLED ROPE) — High-income earners with assets use credit strategically (mortgages, investment leverage) for appreciating assets. Interest rates are lower; debt-to-income ratios are sustainable; exit options are abundant (refinancing, early payoff, asset sale). The constraint functions as coordination: credit enables wealth accumulation and asset purchase. Both beneficiary status and active enforcement present, but extraction is asymmetric — the wealthy extract value from leverage while bearing lower suppression. The same constraint (credit system) appears as tangled rope from this perspective.
constraint_indexing:constraint_classification(consumer_debt_slavery, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: REGULATORY APPARATUS (PITON) — Truth in Lending Act, Dodd-Frank, state usury caps are performative regulatory theater. Enforcement is weak; loopholes proliferate (payday lending, title loans, credit card tricks); fines are minor relative to profits; compliance is largely theatrical. The regulatory system sees its own mechanisms as degraded — the rules persist through institutional inertia despite low effectiveness. Lenders navigate compliance; borrowers still trapped. Theater ratio is high because the regulatory apparatus maintains appearance of consumer protection while structural extraction continues.
constraint_indexing:constraint_classification(consumer_debt_slavery, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NECESSITY VIEW (MOUNTAIN) — From civilizational/universal scope, debt servitude appears inherent to any system where capital is scarce and future income is uncertain. Borrowing is structurally necessary; compound interest is mathematically inevitable; someone without capital must pay premium for immediate access. This perspective risks naturalizing contingent institutional arrangements (high interest rates, predatory terms, asymmetric information) as immutable features of capital markets. The false summit detection will reveal that the 'inherent scarcity' framing ignores policy choices (usury caps, direct lending, cooperative credit unions) that alter the extraction landscape.
constraint_indexing:constraint_classification(consumer_debt_slavery, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(consumer_debt_slavery_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(consumer_debt_slavery, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(consumer_debt_slavery, TypeOther, context(agent_power(institutional), _, _, _)),
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
 *   Extractiveness (0.68): High. The constraint extracts substantial lifetime income from low-capital borrowers. A $5,000 credit card balance at 20% APR with minimum 2% payments requires 10+ years to repay and costs $4,000+ in interest alone — extraction of 80% above principal. Payday loans at 400% APR are pure extraction. Car loans and mortgages, while lower-rate, still extract significant wealth through interest. The 0.68 value reflects that extraction is severe but not maximal (0.95) — some borrowers escape through income growth, some loans are at lower rates, and alternative credit mechanisms exist but remain marginal. The trajectory from 0.52 to 0.68 reflects increasing debt accumulation and securitization over the measured period. Suppression (0.75): High. Structural barriers to escape include: (1) income volatility and precarity limiting ability to service debt or build reserves, (2) essential consumption requirements (food, housing, medical, transportation) forcing borrowing, (3) information asymmetry (complex lending terms, hidden fees, credit score opacity), (4) psychological lock-in (normalized debt, anchored consumption standards, sunk cost fallacy), (5) legal barriers (bankruptcy costs, credit reporting systems), (6) social barriers (limited access to alternative credit). Theater ratio (0.45): Moderate-low. The debt mechanism operates with relatively little performative overlay. Interest is mathematically direct; fees are explicit (though sometimes hidden); repayment schedules are mechanical. The theater that does exist involves regulatory compliance theater (disclosure rules that borrowers don't read, usury caps with large loopholes, consumer protection that doesn't protect) and behavioral theater (marketing that encourages borrowing, credit scores that appear technical but are arbitrary, 'financial literacy' that assumes borrower choice rather than borrower constraint).
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates dramatic perspectival divergence across structural positions. The wage earner trapped in a debt cycle sees a Snare — no exit, maximum extraction, suppression of all alternatives. The financial institution sees a Rope or even neutral Coordination — they are performing the legitimate function of capital allocation, compensating for risk, enabling consumption and asset purchase. The regulatory apparatus sees a Piton — the consumer protection rules persist through institutional inertia despite low effectiveness; the rules are performed but not enforced. The advocacy organization sees a Scaffold — temporary enforcement with a sunset: debt relief programs, usury caps, public lending options are building alternatives that will eventually reduce the extraction. The wealthy borrower sees a Tangled Rope — the same credit system that extracts from low-income wage earners provides asymmetric benefits to those with assets and income stability. The philosophical observer risks seeing a Mountain — debt and interest are 'inherent to capitalism' — but the structural data reveals contingent policy choices: the mountain framing naturalizes what could be constrained or reformed by alternative institutional designs (public banks, credit unions, usury caps, income-based repayment). The perspectival gap is large because the same constraint (consumer credit markets) operates asymmetrically: extracting from those without exit options and coordinating for those with exit options.
 *
 * DIRECTIONALITY LOGIC:
 *   Borrower directionality (d): Low-income wage earners are victims with trapped exit options. Their d derives from: victim status (forced into borrowing by necessity) + trapped exit (cannot avoid borrowing without homelessness or medical crisis, cannot exit debt without income growth, cannot refinance without better credit/income). Structural derivation produces d ≈ 0.92-0.98 (near full target), yielding f(d) ≈ 1.40 (maximum experienced extraction). Creditor directionality (d): Financial institutions are beneficiaries with arbitrage exit options. Their d derives from: beneficiary status (profit from interest differential) + arbitrage exit (can change lending criteria, move to different markets, securitize debt, shift product mix). Structural derivation produces d ≈ 0.05-0.15 (beneficiary with low extraction), yielding f(d) ≈ -0.10 to 0.0 (negative effective extraction, i.e., subsidized position). Higher-income borrowers have different directionality: beneficiary status (borrow at lower rates for appreciating assets) + mobile exit (can refinance, pay early, default at lower cost) → d ≈ 0.35-0.45 → f(d) ≈ 0.30-0.45 (moderate experienced extraction, but asymmetric to beneficiary position). Scope modifier: national scope (σ = 1.0) — debt markets are primarily national in structure despite global capital flows. Effective extraction χ for powerless borrower: 0.68 × 1.40 × 1.0 = 0.952 (near-maximal).
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION: The constraint resolves the mandatrophy by disambiguating the 'credit is good/bad' mandate. The analytical observer wants to say: 'Credit is essential coordination — it enables people to smooth consumption over time, purchase homes and education, weather emergencies.' This is true from the beneficiary/high-income perspective. But the same observer also sees: 'Credit is extraction and predation — it transfers lifetime income from those without capital to those with capital, at rates that exceed economic growth.' This is also true from the low-income victim perspective. The mandatrophy is NOT resolved by choosing one. It is resolved by showing that the constraint has structurally different effects depending on the observer's position in the extraction flow. For low-capital wage earners, credit markets function as a Snare (pure extraction + suppression). For wealthy borrowers, the same markets function as Tangled Rope (coordination + asymmetric benefits). For financial institutions, they function as Rope (coordination). The mandatrophy dissolves when we recognize that 'credit' is not a single constraint but a bifurcated system: one architecture for those with assets and stable income (lower rates, longer terms, refinancing options, positive real returns on strategic borrowing), and a different architecture for those without assets and volatile income (higher rates, predatory terms, behavioral lock-in, lifetime extraction). The constraint story shows that the 'inherent to capitalism' framing (mountain view) is false — the extraction rates and suppression levels are policy-contingent. Countries with strong usury caps, public credit systems, and income-based repayment have significantly lower extraction values in identical economic conditions. The constraint is a Snare by design, not by necessity.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    depreciating_vs_essential_debt_boundary,
    'What is the threshold between legitimate credit for essential consumption (medical, housing, emergency) versus extractive credit for depreciating assets (car loans, high-interest furniture), and how does this boundary shift with income level?',
    'Longitudinal analysis of debt-to-income sustainability; comparison of default rates and lifetime wealth accumulation outcomes across asset types and income cohorts; behavioral economics research on access versus choice in borrowing decisions',
    'If most low-income debt is essential: constraint is partially coordination problem (access to capital for survival). If most low-income debt is discretionary: constraint is pure extraction (lifestyle consumption engineered by marketing). Classification shifts from Tangled Rope toward Snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(depreciating_vs_essential_debt_boundary, empirical, 'Boundary between essential and extractive debt categories').

omega_variable(
    alternative_credit_system_feasibility,
    'Are alternative credit systems (credit unions, cooperative banks, mutual aid, government lending) structurally capable of serving low-income populations at lower extraction rates, or do they face inherent cost barriers that commercial lending overcomes through predation?',
    'Comparative cost analysis of cooperative credit union lending versus commercial lending; study of countries with public credit systems; examination of why alternative models remain marginal in US economy despite lower extraction',
    'If alternatives are feasible: constraint is political choice enforced by regulatory capture (Snare + Piton). If alternatives face unavoidable cost barriers: constraint is partially a Mountain (some extraction is structurally necessary).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(alternative_credit_system_feasibility, empirical, 'Whether alternative credit systems can serve low-income borrowers sustainably').

omega_variable(
    behavioral_lock_in_versus_rational_choice,
    'To what degree do borrowers remain trapped by rational calculation (actual mathematical impossibility of escape given income and rates) versus behavioral lock-in (anchoring on consumption standards, present-bias, limited financial literacy)?',
    'Behavioral finance experiments; analysis of borrower decision-making at key junctures (consolidation options, refinancing, bankruptcy); comparison of escape rates when active intervention is offered versus passive availability',
    'If mostly rational: constraint is structural (Snare due to real income-rate gaps). If mostly behavioral: constraint could be significantly reduced through information architecture and choice design. Classification shifts from pure Snare toward Tangled Rope or Scaffold.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(behavioral_lock_in_versus_rational_choice, empirical, 'Relative contribution of rational structural barriers versus behavioral lock-in').

omega_variable(
    systemic_interest_rate_necessity,
    'Do interest rates above the rate of economic growth structurally require wealth transfer from borrowers to lenders, or can lending systems achieve positive real returns for lenders while keeping borrower extraction rates below the mountain threshold?',
    'Historical analysis of lending systems (Islamic banking, cooperative credit, government lending); mathematical modeling of sustainable lending rates given default risk and operational costs; empirical comparison of extraction rates across different regulatory regimes',
    'If interest above growth is necessary: significant portion of extraction is Mountain-like. If lower rates are achievable: high extraction rates are political choice (policy design by lenders). Classification shifts from Mountain-aspect to pure institutional Snare.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(systemic_interest_rate_necessity, conceptual, 'Whether interest-above-growth is structurally necessary or contingent').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(consumer_debt_slavery, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(debt_tr_t0, consumer_debt_slavery, theater_ratio, 0, 0.35).
narrative_ontology:measurement(debt_tr_t5, consumer_debt_slavery, theater_ratio, 5, 0.4).
narrative_ontology:measurement(debt_tr_t10, consumer_debt_slavery, theater_ratio, 10, 0.45).

% Extraction over time
narrative_ontology:measurement(debt_be_t0, consumer_debt_slavery, base_extractiveness, 0, 0.52).
narrative_ontology:measurement(debt_be_t5, consumer_debt_slavery, base_extractiveness, 5, 0.62).
narrative_ontology:measurement(debt_be_t10, consumer_debt_slavery, base_extractiveness, 10, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(consumer_debt_slavery, resource_allocation).
narrative_ontology:affects_constraint(consumer_debt_slavery, intergenerational_wealth_transfer).
narrative_ontology:affects_constraint(consumer_debt_slavery, housing_affordability_crisis).
narrative_ontology:affects_constraint(consumer_debt_slavery, wage_stagnation_trap).
narrative_ontology:affects_constraint(consumer_debt_slavery, student_debt_servitude).

% DUAL FORMULATION NOTE:
% Consumer debt slavery decomposes into multiple related constraints depending on asset type and borrower cohort. Student debt servitude (ε ≈ 0.72, Snare with longer timeline) is downstream of consumer debt slavery but has distinct extraction mechanisms (income-contingent terms, non-dischargeable bankruptcy). Housing debt (ε ≈ 0.45, Tangled Rope) involves appreciating assets so extraction asymmetry is less severe. Credit card debt (ε ≈ 0.78, Snare) has higher extraction and faster accumulation than other consumer debt. The family constraint decomposes into these specific mechanisms; all share the core feature of interest-above-income-growth targeting low-capital borrowers.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(consumer_debt_slavery, institutional, 0.12).
constraint_indexing:directionality_override(consumer_debt_slavery, powerless, 0.95).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
