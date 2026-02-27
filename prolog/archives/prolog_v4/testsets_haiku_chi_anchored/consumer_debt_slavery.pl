% ============================================================================
% CONSTRAINT STORY: consumer_debt_slavery
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
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
 *   constraint_id: consumer_debt_slavery
 *   human_readable: The Interest Trap (Act Your Wage)
 *   domain: economic/financial_servitude
 *
 * SUMMARY:
 *   The Interest Trap represents a structural extraction mechanism disguised
 *   as consumer choice and financial inclusion. Low-income households utilize
 *   credit for depreciating assets (vehicles, appliances) or essential
 *   consumption (healthcare, emergency expenses) when wages prove
 *   insufficient. Once indebted, borrowers enter a state of long-term
 *   financial servitude: interest compounds faster than income growth,
 *   reducing ability to accumulate capital and exit the debt cycle. The
 *   constraint exhibits snare characteristics (high extraction, high
 *   suppression, trapped exits for victims) alongside rope-like performative
 *   framing from credit issuers (portrayed as coordinating capital
 *   allocation). The 'Act Your Wage' colloquialism captures the core
 *   mechanism: workers consume based on gross income, ignoring debt service
 *   obligations, leading to persistent undercapitalization. Unlike acute
 *   financial crises, the Interest Trap is chronic — it persists through wage
 *   cycles, affecting an estimated 40% of U.S. households carrying credit
 *   card debt with active interest charges. The constraint's theater ratio
 *   (0.38) remains moderate because the extraction function is transparent:
 *   interest is explicitly charged, not hidden. However, the constraint's
 *   legitimacy theater — framing credit access as 'democratized' financial
 *   inclusion — obscures that access without affordability is extraction.
 *
 * KEY AGENTS:
 *   - Low-income borrowers (precarious workers): Primary victim (powerless/trapped) — must service debt through wage labor with no capital accumulation pathway
 *   - Credit issuers (banks, fintech, credit card companies): Primary beneficiary (institutional/arbitrage) — capture surplus through interest margins and fee structures
 *   - Financial regulatory agencies (Federal Reserve, SEC, consumer finance bureaus): Nominal coordinator (powerful/arbitrage) — claim consumer protection role while permitting high-cost credit for vulnerable populations
 *   - Credit unions and community finance organizations: Alternative pathway agents (organized/constrained) — build lower-cost credit structures; see constraint as addressable through institutional substitution
 *   - Debt abolition and financial justice movements: Organized challengers (organized/constrained) — advocate structural solutions (debt jubilee, living wage, universal basic income) to undermine extraction mechanism
 *   - Analytical observer: Civilizational view — sees constraint as systematic rent extraction from precariat, not market failure
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(consumer_debt_slavery, 0.68).
domain_priors:suppression_score(consumer_debt_slavery, 0.72).
domain_priors:theater_ratio(consumer_debt_slavery, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(consumer_debt_slavery, extractiveness, 0.68).
narrative_ontology:constraint_metric(consumer_debt_slavery, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(consumer_debt_slavery, theater_ratio, 0.38).

% --- Constraint claim ---
narrative_ontology:constraint_claim(consumer_debt_slavery, snare).
narrative_ontology:human_readable(consumer_debt_slavery, "The Interest Trap (Act Your Wage)").
narrative_ontology:topic_domain(consumer_debt_slavery, "economic/financial_servitude").

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(consumer_debt_slavery, credit_issuers).
narrative_ontology:constraint_beneficiary(consumer_debt_slavery, financial_intermediaries).
narrative_ontology:constraint_victim(consumer_debt_slavery, low_income_borrowers).
narrative_ontology:constraint_victim(consumer_debt_slavery, precarious_workers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DEBT-BOUND BORROWER (SNARE) — Powerless, trapped in biographical servitude. No exit: must service debt through wage labor. Interest compounds faster than income growth for precarious workers. d≈0.95, f(d)≈1.42, σ=1.0 → χ≈0.96. Pure extraction with no coordination benefit.
constraint_indexing:constraint_classification(consumer_debt_slavery, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: CREDIT ISSUER (ROPE) — Institutional beneficiary (arbitrage exit). Experiences constraint as coordination mechanism: allocating capital to borrowers, earning risk premium. d≈0.08, f(d)≈-0.10, σ=1.0 → χ≈-0.07. Net beneficiary; extraction hidden under term 'credit intermediation'.
constraint_indexing:constraint_classification(consumer_debt_slavery, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: REGULATORY FRAMEWORK (TANGLED ROPE) — Powerful institutional actor (organized banking sector + government). Claims coordination function: capital allocation, consumer protection (Truth in Lending Act, fair lending laws). d≈0.35, f(d)≈0.30, σ=1.0 → χ≈0.20. Moderate extraction masked as consumer welfare.
constraint_indexing:constraint_classification(consumer_debt_slavery, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: DEBT ABOLITION MOVEMENT (SCAFFOLD) — Organized agents (nonprofits, faith organizations, mutual aid networks) building alternative credit pathways (credit unions, community lending, debt jubilee proposals). See constraint as temporary, addressable through policy reform. d≈0.60, f(d)≈0.85, σ=1.0 → χ≈0.58. High suppression but sees sunset path through redistribution and alternative finance.
constraint_indexing:constraint_classification(consumer_debt_slavery, scaffold,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: CONSUMER CREDIT MARKET IDEOLOGY (PITON) — Institutional persistence through theater. Market ideology frames debt as 'consumer choice' and 'financial empowerment' despite empirical servitude. theater_ratio=0.38 slightly below piton gate (0.70), but the performative justification ('democratizing credit') masks extraction. Theater is lower because the constraint's function (extracting surplus) is directly observable, not hidden behind ritual.
constraint_indexing:constraint_classification(consumer_debt_slavery, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (SNARE) — From civilizational/global view, consumer debt operates as systematic extraction mechanism with suppressed alternatives: 'Act Your Wage' framing masks that wage earners cannot accumulate capital before consuming. ε=0.68 confirms snare classification at analytical level. d≈0.72, f(d)≈1.15, σ=1.0 → χ≈0.78. High effective extraction from entire precariat.
constraint_indexing:constraint_classification(consumer_debt_slavery, snare,
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
 *   Extractiveness (0.68): High. Base extraction reflects structural surplus transfer: interest margins (8-25% APR for subprime), fees (late fees, over-limit fees), and opportunity cost (capital not accumulating) systematically drain income. The measurement shows accumulation over 30 years (0.48 → 0.68) as debt burdens compound and alternative financing becomes less accessible. Suppression (0.72): High. Exit barriers include: (1) credit-score dependency (default damages future borrowing), (2) wage garnishment legal mechanisms, (3) inadequate income relative to debt stock, (4) absence of alternative credit pathways, (5) bankruptcy stigma and legal barriers. Suppression increased over the interval as debt-collection enforcement strengthened and alternative lending contracted. Theater ratio (0.38): Moderate. The constraint's function (extracting surplus) is transparent, reducing pure theater. However, performative framing ('financial inclusion,' 'access to credit') masks that access without affordability is extraction. The measurement shows slight increase (0.22 → 0.38) as market ideology normalized high-cost credit as consumer empowerment.
 *
 * PERSPECTIVAL GAP:
 *   The debt-bound borrower experiences a Snare: trapped in biographical servitude, bearing all extraction cost. The credit issuer experiences a Rope: legitimate capital allocation with risk premium (their genuine experience). The regulatory framework claims Tangled Rope (coordination + mild extraction) but functions closer to Snare for borrowers. The debt abolition movement sees a Scaffold: temporary extraction addressable through policy reform and alternative finance. The consumer credit ideology persists as Piton: theater of 'financial democratization' maintains extraction despite low functional role. The analytical observer sees Snare: systematic extraction from precariat class. The perspectival gap reveals that 'credit access' means opposite things to different agents: to issuers, it is profitable capital allocation; to borrowers, it is necessary consumption financing that locks them into servitude.
 *
 * DIRECTIONALITY LOGIC:
 *   Low-income borrowers: Victim + trapped → d≈0.95, f(d)≈1.42. Maximum extraction experience. Credit issuers: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.10. Net benefit; extraction is their coordination function. Regulatory framework: Beneficiary (permits high-cost credit) + arbitrage (regulatory capture by banking sector) → d≈0.35, f(d)≈0.30. Moderate extraction masked as consumer protection. Debt abolition movement: Victim (systemic) + constrained → d≈0.60, f(d)≈0.85. High extraction but organized agency and viable exit pathways. Analytical observer: Observer + analytical → d≈0.72, f(d)≈1.15. Standard observational positionality; sees high extraction.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    interest_rate_baseline_determination,
    'What interest rate differential constitutes extraction vs legitimate risk compensation?',
    'Comparative analysis: credit access costs across income cohorts; correlation between default rates and interest charged; modeling of sustainable vs extractive rate structures',
    'If ''legitimate'' baseline is high: constraint appears moderate. If baseline is low: reveals excessive extraction margin built into standard rates.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(interest_rate_baseline_determination, empirical, 'The threshold between legitimate risk pricing and extractive rents').

omega_variable(
    alternative_capital_access_feasibility,
    'Are alternative credit mechanisms (credit unions, peer lending, community finance) structurally viable at scale as substitutes for predatory consumer credit?',
    'Longitudinal comparison of default rates, affordability, and portfolio composition between credit unions and commercial lenders; cost analysis of scaling community lending; regulatory barrier assessment',
    'If viable at scale: scaffold perspective is real (sunset through alternative finance). If structurally constrained: suppression is absolute, constraint is permanent snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_capital_access_feasibility, empirical, 'Whether alternative credit systems can replace commercial predatory lending').

omega_variable(
    wage_stagnation_causality,
    'Does consumer debt trap cause wage stagnation through debt servicing reducing labor mobility, or is wage stagnation exogenous driver making debt necessary?',
    'Causal inference: cross-national comparison of debt levels and wage growth; natural experiments in debt forgiveness or credit expansion; labor mobility analysis by debt level',
    'If debt causes stagnation: constraint is self-reinforcing (high d, high f(d)). If exogenous: stagnation is separate constraint; debt is response, not cause.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(wage_stagnation_causality, empirical, 'Causal direction between consumer debt and wage stagnation').

omega_variable(
    cultural_norming_substitutability,
    'Can cultural norms (''Save First'' ascendancy) structurally replace ''Act Your Wage'' consumption norms and shift credit dependency?',
    'Historical comparison of credit/savings ratios across cultures and time periods; correlation with intergenerational wealth transmission; impact of financial literacy on debt trajectories',
    'If norms are plastic: constraint is partially cultural (lower ε). If norms are sticky: constraint is structural (ε remains high).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(cultural_norming_substitutability, preference, 'Whether consumption norms are malleable relative to structural credit conditions').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(consumer_debt_slavery, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cds_tr_t0, consumer_debt_slavery, theater_ratio, 0, 0.22).
narrative_ontology:measurement(cds_tr_t15, consumer_debt_slavery, theater_ratio, 15, 0.3).
narrative_ontology:measurement(cds_tr_t30, consumer_debt_slavery, theater_ratio, 30, 0.38).

% Extraction over time
narrative_ontology:measurement(cds_be_t0, consumer_debt_slavery, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(cds_be_t15, consumer_debt_slavery, base_extractiveness, 15, 0.58).
narrative_ontology:measurement(cds_be_t30, consumer_debt_slavery, base_extractiveness, 30, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(consumer_debt_slavery, resource_allocation).
narrative_ontology:boltzmann_floor_override(consumer_debt_slavery, 0.35).
narrative_ontology:affects_constraint(consumer_debt_slavery, wage_stagnation_structural).
narrative_ontology:affects_constraint(consumer_debt_slavery, housing_affordability_crisis).
narrative_ontology:affects_constraint(consumer_debt_slavery, medical_bankruptcy_trap).
narrative_ontology:affects_constraint(consumer_debt_slavery, intergenerational_wealth_capture).

% DUAL FORMULATION NOTE:
% Consumer debt slavery intersects with multiple constraints: it is downstream of inadequate wages (wage_stagnation_structural) and housing costs (housing_affordability_crisis), and upstream of intergenerational poverty (intergenerational_wealth_capture). Each constraint has its own ε value reflecting distinct mechanisms; the network linkage indicates structural causality and shared beneficiary class.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(consumer_debt_slavery, institutional, 0.4).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
