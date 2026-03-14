% ============================================================================
% CONSTRAINT STORY: household_financial_liquidity
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_household_financial_liquidity, []).

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
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: household_financial_liquidity
 *   human_readable: Household Financial Liquidity Constraints
 *   domain: economic/household_finance
 *
 * SUMMARY:
 *   Household financial liquidity constraints represent the structural
 *   binding mechanism through which precarious households are extracted from:
 *   they lack sufficient liquid reserves to weather income shocks, cannot
 *   access low-cost credit, and face binary choices between deprivation or
 *   high-cost borrowing. This constraint demonstrates the tangled
 *   interweaving of genuine coordination function (credit systems do
 *   coordinate consumption smoothing across income volatility) with
 *   systematic extraction (pricing and access structures concentrate benefits
 *   on financial institutions while concentrating costs on powerless
 *   households). The constraint exhibits all six types across different
 *   perspectives: pure extraction (snare) from the precarious household
 *   perspective, mixed coordination-extraction (tangled rope) from moderate
 *   working-class households, pure coordination (rope) from financial
 *   institutions' perspective, temporary policy failure with exit path
 *   (scaffold) from reform coalitions, degraded vestigial constraint (piton)
 *   from affluent households with unlimited liquidity, and false
 *   naturalization (mountain) from civilizational analytical perspectives
 *   that treat human time preference as immutable law. The extractiveness
 *   trajectory shows acceleration over the measurement interval: from 0.35
 *   (initial state) to 0.62 (final state), tracking the real-world expansion
 *   of high-cost lending, gig economy wage volatility, and erosion of
 *   traditional savings-based household financial strategies. Theater ratio
 *   remains low (0.45) because financial extraction mechanisms operate
 *   through actual material extraction of capital rather than performative
 *   ritual — unlike degraded constraints, the functional enforcement of
 *   liquidity barriers remains intact.
 *
 * KEY AGENTS:
 *   - Precarious households (powerless/trapped): primary victims bearing full extraction cost through high-interest debt, payday lending, overdraft fees
 *   - Working-class households (moderate/constrained): secondary victims and partial beneficiaries; access to credit enables consumption smoothing but at extractive costs
 *   - Financial institutions (institutional/arbitrage): primary beneficiaries capturing interest margin arbitrage, collateral extraction, and fee structure advantages
 *   - Low-income depositors (powerless/constrained): provide capital base for institutions while receiving minimal returns, subsidizing institutional profitability
 *   - Regulatory reform coalitions (organized/constrained): advocating for usury caps, access mandates, transparency requirements; perceive policy sunset as viable
 *   - Affluent households (powerful/mobile): experience constraint as functionally atrophied; theater of financial planning persists but material binding has vanished
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(household_financial_liquidity, 0.58).
domain_priors:suppression_score(household_financial_liquidity, 0.62).
domain_priors:theater_ratio(household_financial_liquidity, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(household_financial_liquidity, extractiveness, 0.58).
narrative_ontology:constraint_metric(household_financial_liquidity, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(household_financial_liquidity, theater_ratio, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(household_financial_liquidity, tangled_rope).
narrative_ontology:human_readable(household_financial_liquidity, "Household Financial Liquidity Constraints").
narrative_ontology:topic_domain(household_financial_liquidity, "economic/household_finance").

domain_priors:requires_active_enforcement(household_financial_liquidity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(household_financial_liquidity, financial_institutions).
narrative_ontology:constraint_beneficiary(household_financial_liquidity, credit_providers).
narrative_ontology:constraint_beneficiary(household_financial_liquidity, asset_managers).
narrative_ontology:constraint_victim(household_financial_liquidity, low_income_households).
narrative_ontology:constraint_victim(household_financial_liquidity, precarious_workers).
narrative_ontology:constraint_victim(household_financial_liquidity, unbanked_populations).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PRECARIOUS HOUSEHOLD (SNARE) — Trapped by wage volatility, no savings buffer, and high cost of credit. Faces binary choice: borrow at extractive rates or face immediate deprivation. No exit options; suppression through poverty itself. Experiences maximum extraction as liquidity constraints force dependence on high-cost credit.
constraint_indexing:constraint_classification(household_financial_liquidity, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: WORKING-CLASS HOUSEHOLD (TANGLED ROPE) — Constrained by limited liquid reserves and career mobility costs. Household financial system does coordinate consumption smoothing across income volatility, but extraction embedded in fees, interest rates, and collateral requirements. Benefits from credit access; bears asymmetric costs through suppression and extraction.
constraint_indexing:constraint_classification(household_financial_liquidity, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: FINANCIAL INSTITUTION (ROPE) — Experiences liquidity constraints as a coordination mechanism: deposit collection, lending standards, and reserve requirements enable their core function. Benefits from the constraint structure through interest margin arbitrage. Net beneficiary with full institutional power and exit options.
constraint_indexing:constraint_classification(household_financial_liquidity, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: REGULATORY REFORM COALITION (SCAFFOLD) — Community development financial institutions, microfinance advocates, and consumer protection organizers see liquidity barriers as temporary policy failures with sunset solutions. Regulations capping interest rates, expanding access, and mandating transparency create parallel lower-extraction pathways. Coalition has agency and perceives exit path through policy change.
constraint_indexing:constraint_classification(household_financial_liquidity, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: AFFLUENT HOUSEHOLD (PITON) — For wealthy households with substantial liquid reserves, 'financial liquidity constraints' is a vestigial label. The constraint persists discursively (financial planning, wealth management advice) but functionally has atrophied — liquid assets, credit access, and arbitrage options are unlimited. Theater ratio high: the constraint is maintained performatively through financial advice culture while material binding has vanished.
constraint_indexing:constraint_classification(household_financial_liquidity, piton,
    context(agent_power(powerful),
            time_horizon(civilizational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From civilizational scope, household liquidity constraints appear as immutable facts: human time preference, uncertainty about future income, and the irreducible cost of capital coordination are inherent to economic life. No household can have infinite liquid reserves; all face trade-offs between consumption today and insurance against tomorrow. However, the structural data reveals this as false naturalization — the specific extractive mechanisms (usury, predatory lending, asymmetric information) are policy-contingent, not laws of nature.
constraint_indexing:constraint_classification(household_financial_liquidity, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(household_financial_liquidity_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(household_financial_liquidity, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(household_financial_liquidity, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(household_financial_liquidity, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(household_financial_liquidity, TR),
    TR >= 0.70.

:- end_tests(household_financial_liquidity_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. Household liquidity constraints operate through real asymmetric extraction: interest rates on high-risk borrowing average 2-3x the cost of capital to financial institutions, and borrowing against volatile income is forced not optional. The 0.58 value reflects that extraction is substantial but not maximized — some households retain constrained-level agency (can shop for rates, refinance, reduce borrowing), and formal credit systems do provide genuine coordination function absent which households would face worse alternatives. The measurement trajectory (0.35→0.62) reflects historical acceleration of gig economy wage volatility, erosion of employer-provided liquidity insurance, and expansion of high-cost lending into previously informal networks. Suppression (0.62): Moderate-high. Barriers to exit include: limited savings capacity (trapped in paycheck-to-paycheck cycle), high collateral requirements (asset barriers), employment insecurity (cannot negotiate terms), information asymmetry (borrowers cannot evaluate true cost), and social dependency (kinship lending creates relational obligation). The suppression is not total — some households can improve outcomes through education, relocation, or career change — but the suppression is high enough that exit is effectively constrained rather than mobile. Theater ratio (0.45): Low-moderate. Financial extraction operates primarily through actual material transfer (interest payments, fees, collateral loss), not through performative ritual. Lending contracts do have theater (complexity, fine print, obligation framing), but the theater does not substitute for genuine extraction — it obscures extraction rather than replacing it. Theater is lower than, e.g., degraded legal systems that maintain purely symbolic function, because financial extraction remains functionally operative.
 *
 * PERSPECTIVAL GAP:
 *   The constraint exhibits maximum perspectival divergence across power positions. Precarious households see pure snare: no coordination benefit (crisis borrowing is not coordination, it is forced extraction), maximum suppression, maximum extraction experienced (χ≈0.82). Working-class households see tangled rope: genuine benefit from credit access for income smoothing (coordination component) partially offset by extractive pricing and suppression (constrained exit, asymmetric rates). Financial institutions see rope: the constraint is experienced as coordination mechanism (capital pooling, lending standards, reserve management) enabling their core function, with extraction running toward them (institutional arbitrage benefits). Reform coalitions see scaffold: they perceive the extractive structure as policy-contingent and fixable through regulatory change (usury caps, access mandates), with a feasible sunset timeline (15-25 years for alternative financial infrastructure to mature). Affluent households see piton: the constraint persists discursively (financial planning, wealth management) but functionally has atrophied — with sufficient liquid assets, 'liquidity constraints' becomes a vestigial label maintained through cultural practice rather than material binding. Civilizational analytical observer risks seeing mountain: time preference and capital scarcity appear as immutable economic laws. The structural data contradicts this — the specific extractive mechanisms (usury, asymmetric information, regulatory capture) are contingent policy choices, not laws of nature. The false summit is detected by the accessibility collapse gate: if liquidity constraints were natural law, they would apply equally to all households; the fact that affluent households experience them as fully accessible reveals the constraint as socially constructed.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) derives from structural position relative to extraction flow. Precarious households (powerless/trapped): d≈0.95 (maximum target status) because they have zero exit options and full victim status. Working-class households (moderate/constrained): d≈0.65 because they have partial exit (can improve through effort, career change) and partial beneficiary status (credit access enables consumption smoothing). Financial institutions (institutional/arbitrage): d≈0.05 because they are full beneficiaries with maximum exit options — they can arbitrage across jurisdictions, adjust lending criteria, exit unprofitable markets. The sigmoid f(d) transforms these d values into experienced extractiveness multipliers: f(0.95)≈1.42 (powerless experience maximum chi), f(0.65)≈1.00 (moderate experience baseline), f(0.05)≈-0.12 (institutional experience negative or subsidized cost). The scope modifier σ(S) scales by spatial scale: local liquidity constraints operate at σ=0.8 (dampened by informal networks), national at σ=1.0 (baseline), global at σ=1.2 (amplified by capital mobility asymmetries — institutions can move capital globally; households cannot). The tangled rope classification at baseline (analytical) perspective reflects: base extractiveness 0.58, effective extraction χ = 0.58 × f(0.72) × 1.0 ≈ 0.67, which exceeds snare threshold of 0.66 — BUT the constraint simultaneously exhibits genuine coordination function (credit access, consumption smoothing) that would classify as rope absent the extraction. The combination of coordination + extraction + enforced structure (financial regulation, collateral law) is the definitive tangled rope signature.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by showing that financial liquidity constraints are genuinely tangled — they serve a real coordination function (credit systems do smooth consumption across income volatility) AND operate as extraction mechanism (pricing and access are asymmetrically favorable to institutions). The analytical error is attempting to classify as either pure coordination (rope) or pure extraction (snare) when the structural reality is hybrid. The snare perspective (precarious household) is their genuine experience of the constraint as extraction-only — from their structural position, the coordination benefit is invisible because they lack bargaining power to negotiate terms. The rope perspective (financial institution) is also their genuine experience — the coordination mechanism IS the functional core that enables their business model. Both cannot be simultaneously 'the' classification because classification is indexical. The mandatrophy resolves by recognizing that: (1) the constraint has measurable extractiveness (0.58) AND measured beneficiary coordination function (credit access), making tangled rope the analytical classification; (2) perspectives diverge because power positions diverge — agents with power see coordination, agents without power see extraction; (3) the reform coalition's scaffold perspective is actionable — policy changes CAN reduce extraction while preserving coordination, because the extractive premium (interest markup, collateral requirements) is policy-determined, not functionally required. The false summit (mountain perspective) naturalizes what is policy-contingent: it claims liquidity constraints are inevitable features of human nature / economic law, when in fact the specific extractive form is produced by institutional design choices (lending standards, collateral law, regulatory capture) that could be reformed.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    subsistence_vs_preference_threshold,
    'What portion of measured extraction represents coordination cost of capital provision vs. predatory markup above cost?',
    'Comparative institutional analysis: examine interest rate differentials across regulatory regimes, social credit systems, and informal lending networks to identify floor rates vs. extractive premiums',
    'If markup < 15%: constraint is largely functional coordination (shift toward Rope). If markup > 40%: constraint is substantially extractive (confirm Snare/Tangled Rope). Determines whether suppression is inherent to liquidity or policy-driven.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(subsistence_vs_preference_threshold, empirical, 'Extractive markup above capital coordination cost').

omega_variable(
    alternative_liquidity_sufficiency,
    'Do informal lending networks, mutual aid, and kinship credit provide functionally equivalent liquidity access when formal channels are inaccessible?',
    'Ethnographic analysis of informal vs formal liquidity; measurement of transaction costs, coercion levels, and sustainability of informal networks under stress',
    'If informal systems are effective: suppression is lower (constrained rather than trapped). If informal systems depend on coercion or social obligation: they transfer extraction to relational domain (identity_locked dynamics). If informal networks collapse under individual shocks: trapped exit is structurally inevitable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_liquidity_sufficiency, empirical, 'Functional equivalence of informal vs formal liquidity access').

omega_variable(
    income_volatility_exogeneity,
    'To what degree is household income volatility (the triggering problem the constraint coordinates) structurally exogenous vs. endogenous to labor market extraction?',
    'Historical analysis of employment stability, wage volatility trends, gig economy expansion; comparison of income volatility across labor regimes with different power asymmetries',
    'If exogenous: liquidity constraint is genuine coordination response to random shocks (stronger Rope classification). If endogenous to labor market power: the ''problem'' the constraint solves is itself extractive, creating stacked extraction (Tangled Rope confirmed, severity increased).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(income_volatility_exogeneity, conceptual, 'Whether income volatility is exogenous shock or structural extraction').

omega_variable(
    digital_payment_accessibility,
    'Does expansion of digital payment infrastructure and fintech credit genuinely reduce liquidity barriers for unbanked populations, or does it extend extractive mechanisms into previously informal networks?',
    'Comparative study of unbanked household financial outcomes before/after digital finance access; analysis of fee structures, algorithm-driven credit denial, and data extraction in fintech platforms',
    'If access genuinely expands options: suppression decreases (constrained rather than trapped). If fintech replaces traditional extraction with data-driven extraction and algorithmic redlining: suppression persists at new level (Piton progression). If both: creates stratified markets with different constraints at different levels (separate stories needed).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(digital_payment_accessibility, empirical, 'Whether fintech expands access or extends extraction').

omega_variable(
    political_economy_of_liquidity_policy,
    'Why do liquidity-expanding policies (usury caps, access mandates, financial inclusion programs) persistently face regulatory capture and institutional resistance despite demonstrated welfare gains?',
    'Historical policy analysis: track regulatory changes, lobbying patterns, and enforcement levels across jurisdictions with different political economies; examine captured vs independent regulatory bodies',
    'If capture is fundamental: the constraint''s extractive structure is politically sustained and Snare classification is deepened (systematic extraction, not accidental). If capture is contingent: policy change is viable, Scaffold perspective is strengthened, sunset timeline becomes actionable. Determines whether financial institutions benefit from constraint or merely adapt to it.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(political_economy_of_liquidity_policy, conceptual, 'Regulatory capture in liquidity policy persistence').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(household_financial_liquidity, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hfl_tr_t0, household_financial_liquidity, theater_ratio, 0, 0.38).
narrative_ontology:measurement(hfl_tr_t5, household_financial_liquidity, theater_ratio, 5, 0.41).
narrative_ontology:measurement(hfl_tr_t10, household_financial_liquidity, theater_ratio, 10, 0.45).
narrative_ontology:measurement(hfl_tr_t15, household_financial_liquidity, theater_ratio, 15, 0.48).

% Extraction over time
narrative_ontology:measurement(hfl_be_t0, household_financial_liquidity, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(hfl_be_t5, household_financial_liquidity, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(hfl_be_t10, household_financial_liquidity, base_extractiveness, 10, 0.58).
narrative_ontology:measurement(hfl_be_t15, household_financial_liquidity, base_extractiveness, 15, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(household_financial_liquidity, resource_allocation).
narrative_ontology:boltzmann_floor_override(household_financial_liquidity, 0.18).
narrative_ontology:affects_constraint(household_financial_liquidity, wage_volatility).
narrative_ontology:affects_constraint(household_financial_liquidity, consumer_debt_accumulation).
narrative_ontology:affects_constraint(household_financial_liquidity, informal_credit_dependencies).
narrative_ontology:affects_constraint(household_financial_liquidity, wealth_inequality).

% DUAL FORMULATION NOTE:
% Household financial liquidity is a composite constraint family that decomposes into three structurally distinct stories: (1) income_volatility_coordination (ε=0.25, Rope): the coordination function for consumption smoothing across legitimate income shocks; (2) predatory_lending_extraction (ε=0.72, Snare): the extractive mechanisms of payday loans, overdraft fees, and subprime credit; (3) wealth_inequality_structure (ε=0.55, Tangled Rope): the institutional mechanisms that convert temporary liquidity constraints into permanent wealth deficits. The present story aggregates all three, weighted by observability and institutional salience. Separate stories are warranted for specialized policy analysis.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(household_financial_liquidity, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
