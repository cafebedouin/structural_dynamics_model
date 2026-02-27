% ============================================================================
% CONSTRAINT STORY: debt_service_squeeze
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_debt_service_squeeze, []).

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
 *   constraint_id: debt_service_squeeze
 *   human_readable: Consumer Debt Service Squeeze on Higher-Income Households
 *   domain: economic/consumer_finance
 *
 * SUMMARY:
 *   The debt service squeeze on higher-income households represents a
 *   structural tension in consumer finance: the system coordinates credit
 *   allocation and consumption smoothing (legitimate coordination function)
 *   while systematically extracting wealth from borrowers through spread
 *   capture, rate volatility, and origination structures that embed rising
 *   minimum payments. Households with incomes above $75,000 occupy an
 *   intermediate position — they escape poverty-level financial fragility but
 *   lack the asset base and income security of true wealth. Rising interest
 *   rates combined with inflation create a mechanical debt service squeeze:
 *   monthly payments rise faster than incomes in real terms, reducing
 *   discretionary capacity and consumer purchasing power even as nominal
 *   incomes appear to grow. The constraint exhibits tangled rope
 *   characteristics: the system is genuinely functional (credit availability,
 *   payment scheduling) and genuinely extractive (interest spread, rate
 *   sensitivity, prepayment penalties). Unlike predatory lending focused on
 *   low-income households, the squeeze operates through 'legitimate'
 *   mechanisms (prime lending, credit scoring) that create rising extraction
 *   at higher nominal income levels through macro policy transmission (Fed
 *   rate increases) and structural debt volume growth. The theater ratio
 *   reflects that regulatory mechanisms (affordability standards,
 *   debt-to-income limits) exist but are largely performative — lenders
 *   conduct required calculations while continuing to originate loans at
 *   maximum stress-test thresholds.
 *
 * KEY AGENTS:
 *   - Higher-Income Households ($75k-$150k): Primary victims (moderate/trapped) — bear rising debt service burden while discretionary spending capacity declines; cannot easily exit due to necessity of housing/vehicle finance
 *   - Financial Institutions and Creditors: Primary beneficiaries (powerful/arbitrage) — capture interest spread and service fees; benefit from rising-rate environment and volume of outstanding debt
 *   - Federal Reserve and Central Bank: Secondary actor (institutional/constrained) — sets rate policy as inflation control mechanism; constrained by dual mandate but cannot avoid collateral damage
 *   - Consumer Credit Regulators: Institutional theater (institutional/constrained) — enforce affordability standards and truth-in-lending rules but lack enforcement capacity to prevent origination near stress-test limits
 *   - Debt Relief and Financial Reform Movements: Organized reform agents (organized/mobile) — advocate for rate caps, income-based repayment, and debt restructuring; see sunset pathway via policy change
 *   - Consumer Purchasing Power (Abstract Collective): Structural victim (powerless/trapped) — discretionary spending and consumption smoothing capacity collapses without advocacy or organization
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(debt_service_squeeze, 0.52).
domain_priors:suppression_score(debt_service_squeeze, 0.68).
domain_priors:theater_ratio(debt_service_squeeze, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(debt_service_squeeze, extractiveness, 0.52).
narrative_ontology:constraint_metric(debt_service_squeeze, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(debt_service_squeeze, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(debt_service_squeeze, tangled_rope).
narrative_ontology:human_readable(debt_service_squeeze, "Consumer Debt Service Squeeze on Higher-Income Households").
narrative_ontology:topic_domain(debt_service_squeeze, "economic/consumer_finance").

domain_priors:requires_active_enforcement(debt_service_squeeze).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(debt_service_squeeze, financial_institutions).
narrative_ontology:constraint_beneficiary(debt_service_squeeze, creditors).
narrative_ontology:constraint_beneficiary(debt_service_squeeze, debt_servicers).
narrative_ontology:constraint_victim(debt_service_squeeze, higher_income_households).
narrative_ontology:constraint_victim(debt_service_squeeze, consumer_purchasing_power).
narrative_ontology:constraint_victim(debt_service_squeeze, discretionary_spending_capacity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SQUEEZED HOUSEHOLD (SNARE) — Cannot easily exit debt obligations; faces rising minimum payments from variable-rate debt while real wages stagnate. Income above poverty line but insufficient to escape debt trap when combined with inflation and rate increases. d≈0.88, f(d)≈1.35, σ=1.0 → χ≈0.70.
constraint_indexing:constraint_classification(debt_service_squeeze, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: FINANCIAL INSTITUTIONS (TANGLED ROPE) — Experience constraint as profit mechanism (coordination of payment obligations + extraction of interest spread). Have exit option (arbitrage between lending markets) and benefit from the system. Enforcement requires active rate-setting and collection infrastructure. d≈0.08, f(d)≈-0.10, σ=1.0 → χ≈-0.05. Net beneficiary; constraint serves as revenue coordination.
constraint_indexing:constraint_classification(debt_service_squeeze, tangled_rope,
    context(agent_power(powerful),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: CENTRAL BANK (ROPE) — Uses rate increases as coordination mechanism to control inflation; faces constraint as policy tool with distributed side effects. Can adjust rates (constrained, not trapped exit) but cannot easily avoid collateral damage to debt-burdened households. d≈0.50, f(d)≈0.65, σ=1.0 → χ≈0.34. Mixed perspective: coordination tool but produces asymmetric pain.
constraint_indexing:constraint_classification(debt_service_squeeze, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: CONSUMER CREDIT REGULATION (PITON) — Debt-to-income ratio regulations, truth-in-lending laws, and affordability standards exist but are largely performative. Lenders conduct required affordability checks but originate loans near maximum DTI thresholds. Regulatory theater masks continued extraction. theater_ratio=0.58. d≈0.65, f(d)≈1.00, σ=1.0 → χ≈0.52.
constraint_indexing:constraint_classification(debt_service_squeeze, piton,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: REFORM MOVEMENTS (SCAFFOLD) — Organized agents (consumer advocates, progressive policy makers, refinancing platforms) see the squeeze as a temporary institutional design flaw with a potential sunset: income-based repayment, rate caps, debt restructuring protocols, and financial literacy mandates can reduce extraction. Exit is via policy change. d≈0.35, f(d)≈0.35, σ=1.0 → χ≈0.18. Low extraction because alternative pathways are conceivable within the time horizon.
constraint_indexing:constraint_classification(debt_service_squeeze, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (TANGLED ROPE) — Views debt service squeeze as structural feature of late-stage consumer finance capitalism: coordination of lending + asymmetric extraction are both essential and deliberate. The system coordinates credit allocation and consumption smoothing (rope function) while extracting from borrowers through spread, prepayment penalties, and rate volatility (snare function). d≈0.72, f(d)≈1.15, σ=1.0 → χ≈0.60. True tangled rope: genuine coordination mixed with systematic extraction.
constraint_indexing:constraint_classification(debt_service_squeeze, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(debt_service_squeeze_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(debt_service_squeeze, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(debt_service_squeeze, TR),
    TR >= 0.70.

:- end_tests(debt_service_squeeze_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The debt service squeeze extracts approximately 15-20% of gross income from higher-income households compared to 8-12% in 2019. This is a real wealth transfer to financial institutions, but not at the severe level of predatory subprime lending (which reached 0.70+). The extraction operates through 'legitimate' mechanisms (published interest rates, standard underwriting) rather than deception, which moderates the baseline ε. However, the macro transmission mechanism (Fed rate policy) means households cannot negotiate out of the increase — suppression is high. Suppression (0.68): High-moderate. Households cannot easily exit debt obligations (primary residence mortgage, essential vehicle finance); cannot easily refinance when rates rise; cannot force rate caps or payment modifications without policy intervention; face credit score penalties for defaulting. However, suppression is not total — some households can reduce consumption, accelerate paydown, or relocate. The primary suppression mechanism is structural (necessity of credit for modern life) rather than explicitly coercive (unlike criminal debt or labor bondage). Theater ratio (0.58): Moderate. Consumer protection regulations create performative compliance: affordability checks, truth-in-lending disclosures, credit counseling requirements. But the theater is not overwhelming (unlike peer review theater at 0.72) — regulations do catch some predatory origination. The ratio reflects that regulatory mechanisms exist but insufficient to prevent the macro-level squeeze that emerges from volume + rising rates.
 *
 * PERSPECTIVAL GAP:
 *   The constraint produces stark perspectival divergence. Creditors see rope or positive tangled rope (d≈0.08, low extraction, net benefit). Squeezed households see snare (d≈0.88, high extraction, trapped). Central banks see rope (coordination mechanism with side effects, d≈0.50). Reform movements see scaffold (temporary problem with policy sunset, d≈0.35). Regulatory agencies see piton (performative compliance maintaining degraded system, theater=0.58). The analytical observer sees true tangled rope (d≈0.72, both coordination and extraction mixed in necessary tension). The perspectival gap is extreme between beneficiary and victim — this is the signature of a hybrid constraint with asymmetric effects.
 *
 * DIRECTIONALITY LOGIC:
 *   Higher-income households: Victim + trapped → d≈0.88, f(d)≈1.35. Cannot exit mortgage/vehicle obligations; income exceeds poverty but insufficient to escape squeeze. Powerful creditors: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.10. Can choose lending volumes and pricing; profit from rate spreads. Central bank: Mixed + constrained → d≈0.50, f(d)≈0.65. Uses rate policy as coordination tool but cannot avoid side effects. Reform movements: Organized + mobile → d≈0.35, f(d)≈0.35. Organized agents with policy exit option. Regulators: Institutional + constrained → d≈0.65, f(d)≈1.00. Constrained by industry influence and statutory limits; moderate extraction despite theater. Analytical observer: analytical → d≈0.72, f(d)≈1.15. Neutral structural view sees both functions and extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   TANGLED ROPE RESOLUTION: This constraint resolves the mandatrophy by demonstrating that the squeeze is genuinely both coordination and extraction. The coordination function is real: consumer credit enables housing access, vehicle purchase, and consumption smoothing for the majority of households who cannot save full purchase prices upfront. The extraction function is equally real: financial institutions deliberately structure debt to capture spread and remain profitable even in low-rate environments. When rates rise, the spread-based extraction mechanism activates at full force — monthly payments rise faster than incomes, creating the squeeze. The mandatrophy asks: 'Is this system primarily a coordination mechanism that unfortunately has extraction costs, or primarily an extraction mechanism that disguises itself as coordination?' The evidence supports tangled rope: the system cannot function as coordination without the extraction incentive (lenders would not supply credit absent spread), and the extraction cannot persist without the coordination function (lending must actually deliver credit to borrowers, not just collect on phantom obligations). The theater ratio of 0.58 reflects that regulatory oversight is real but insufficient — affordability standards prevent the most egregious originations but do not prevent origination at maximum stress-test thresholds, which become dangerous when rates rise. The squeeze is not a bug in the system but a feature that emerges at the intersection of: (1) macro policy (Fed rate increases to fight inflation), (2) consumer preference for debt-financed purchases (inability/unwillingness to save), and (3) lender profit maximization (originating at maximum affordable levels). All three are structural. Policy interventions (rate caps, mandatory forbearance, income-based repayment) could reduce extraction without collapsing the coordination function, but would require political will to override financial industry opposition. The scaffold perspective (reform movements) is correct that sunset is achievable through policy, but the piton perspective (degraded regulatory theater) is also correct that existing mechanisms lack enforcement capacity.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    real_vs_nominal_income_threshold,
    'At what income level does debt service burden transition from temporary stress to structural trap?',
    'Longitudinal analysis of household consumption smoothing, retirement savings capacity, and debt default rates by income quintile; correlation between debt-to-income ratio at origination and long-term financial stability',
    'If threshold < 100k: high-income bracket is genuinely trapped (snare from most perspectives). If threshold > 150k: squeeze is temporary strain, not structural trap (reduces snare classification to moderate snare/rope hybrid).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(real_vs_nominal_income_threshold, empirical, 'Income threshold for structural debt trap vs temporary stress').

omega_variable(
    refinancing_option_accessibility,
    'Can higher-income borrowers with rising rates actually access refinancing, or is refinancing effectively closed when rates increase?',
    'Analysis of refinancing volumes and approval rates during rising-rate environments; examination of rate-and-term refinance availability vs cash-out refinance; credit score requirements at different rate levels',
    'If refinancing remains accessible: exit options are more mobile (rope from beneficiary perspective). If refinancing closes: households become trapped, confirming snare classification for victims.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(refinancing_option_accessibility, empirical, 'Whether refinancing remains accessible during rate increases').

omega_variable(
    debt_service_inflation_correlation,
    'Is the debt service squeeze primarily caused by rising interest rates or by nominal debt stock outpacing income growth during inflation?',
    'Decomposition of debt service burden growth into components: rate effect vs volume effect vs inflation-adjusted income effect; comparison of 2019-2022 (low rates, high volume) vs 2022-2025 (high rates, sticky volume) periods',
    'If primarily rate-driven: squeeze may reverse with rate cuts (scaffold perspective correct). If primarily volume-driven: squeeze persists despite rates (snare more durable; policy design failure, not monetary policy artifact).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(debt_service_inflation_correlation, empirical, 'Whether squeeze is rate-driven or volume-driven').

omega_variable(
    discretionary_spending_collapse_mechanism,
    'Does rising debt service squeeze out discretionary spending through tight cash flow, or do households maintain consumption through further borrowing?',
    'Analysis of unsecured borrowing trends alongside debt service increases; measurement of discretionary category spending (dining, entertainment, travel) during squeeze periods; credit card utilization rates',
    'If squeeze drives discretionary collapse: consumer spending weakens, reducing systemic risk from overleverage (squeeze is self-limiting). If squeeze drives further borrowing: debt spiral accelerates, increasing systemic fragility (snare becomes more extractive and unstable).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(discretionary_spending_collapse_mechanism, empirical, 'Whether squeeze causes discretionary collapse or triggers new borrowing').

omega_variable(
    policy_intervention_effectiveness,
    'Can policy mechanisms (rate caps, mandatory forbearance, income-based repayment, debt restructuring) actually reduce extraction without collapsing credit availability?',
    'Comparative analysis of jurisdictions with strong debt regulation (EU, Canada) vs light-touch regimes (US); examination of policy interventions during 2008-2010 and COVID periods; credit availability elasticity to regulatory tightening',
    'If policy effective: scaffold perspective is correct; sunset clauses are achievable. If policy ineffective or creates perverse incentives: extraction is structural and regulatory theater increases (piton perspective correct).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(policy_intervention_effectiveness, empirical, 'Whether policy interventions can reduce extraction without reducing credit').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(debt_service_squeeze, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dss_tr_t0, debt_service_squeeze, theater_ratio, 0, 0.42).
narrative_ontology:measurement(dss_tr_t3, debt_service_squeeze, theater_ratio, 3, 0.5).
narrative_ontology:measurement(dss_tr_t6, debt_service_squeeze, theater_ratio, 6, 0.58).

% Extraction over time
narrative_ontology:measurement(dss_be_t0, debt_service_squeeze, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(dss_be_t3, debt_service_squeeze, base_extractiveness, 3, 0.42).
narrative_ontology:measurement(dss_be_t6, debt_service_squeeze, base_extractiveness, 6, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(debt_service_squeeze, resource_allocation).
narrative_ontology:affects_constraint(debt_service_squeeze, housing_affordability_crisis).
narrative_ontology:affects_constraint(debt_service_squeeze, auto_loan_bubble).
narrative_ontology:affects_constraint(debt_service_squeeze, credit_card_spread_extraction).
narrative_ontology:affects_constraint(debt_service_squeeze, consumer_purchasing_power_collapse).

% DUAL FORMULATION NOTE:
% The debt service squeeze is downstream of multiple structural constraints in consumer finance. The upstream constraints (auto_loan_bubble, credit_card_spread_extraction) represent specific lending product vulnerabilities; the debt_service_squeeze represents the aggregate macro effect when macro policy (Fed rate increases) interacts with high consumer debt volumes. These are distinct constraints with different ε values: individual product extraction may be 0.35-0.45, but the macro squeeze effect on purchasing power reaches 0.52. Linked via network because the squeeze amplifies and compounds the upstream vulnerabilities.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(debt_service_squeeze, institutional, 0.12).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
