% ============================================================================
% CONSTRAINT STORY: sotu_1949_truman_fiscal_surplus_mandate
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sotu_1949_truman_fiscal_surplus_mandate, []).

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
 *   constraint_id: sotu_1949_truman_fiscal_surplus_mandate
 *   human_readable: Federal Budget Surplus Mandate for Inflation Control (Truman 1949)
 *   domain: economics/fiscal_policy/macroeconomic_stabilization
 *
 * SUMMARY:
 *   Truman's 1949 State of the Union address frames federal budget surplus as
 *   the primary macroeconomic stabilization tool for controlling postwar
 *   inflation. The constraint operates by requiring sustained tax revenue
 *   above spending levels so that the difference can be sequestered (in
 *   Treasury accounts or deflationary instruments) rather than circulated in
 *   the economy. This mechanism makes fiscal austerity (reduced public
 *   expenditure and/or elevated taxation) the mandatory response to
 *   inflationary pressure. The constraint exhibits Tangled Rope structure: it
 *   coordinates a genuine macroeconomic function (absorbing excess money
 *   supply) while simultaneously extracting from identifiable groups (social
 *   program beneficiaries and lower-income taxpayers who fund the surplus
 *   while bearing its costs). The beneficiaries are financial interests
 *   (creditors, savers, and fixed-income earners) who benefit from inflation
 *   control and debt stability. The extraction is structural and intentional,
 *   not accidental — the surplus mandate explicitly subordinates spending on
 *   social programs and public investment to the requirement for deficit
 *   reduction.
 *
 * KEY AGENTS:
 *   - Truman Administration and Congress: Institutional beneficiaries (institutional/arbitrage) — control the fiscal mechanism and benefit from perceived economic competence and inflation control
 *   - Financial Sector and Fixed-Income Creditors: Primary beneficiaries (institutional/arbitrage) — inflation control protects real returns on debt; beneficiaries of reduced money supply growth
 *   - Low-Income Social Program Beneficiaries: Primary victims (powerless/trapped) — direct extraction through benefit deferrals and program underfunding; no exit or organizational voice
 *   - Low to Middle Income Taxpayers: Secondary victims (moderate/constrained) — bear extraction through sustained high taxation; also gain from inflation control (mixed experience)
 *   - Labor Unions and Public Sector: Organized actors (organized/constrained) — pressure for wage restraint and hiring freezes; can organize collective response but face high exit costs
 *   - Federal Reserve and Monetary Policymakers: Analytical observers (institutional/arbitrage) — see surplus as coordination mechanism (money absorption) but recognize it as crude tool compared to interest rate policy
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sotu_1949_truman_fiscal_surplus_mandate, 0.52).
domain_priors:suppression_score(sotu_1949_truman_fiscal_surplus_mandate, 0.58).
domain_priors:theater_ratio(sotu_1949_truman_fiscal_surplus_mandate, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sotu_1949_truman_fiscal_surplus_mandate, extractiveness, 0.52).
narrative_ontology:constraint_metric(sotu_1949_truman_fiscal_surplus_mandate, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(sotu_1949_truman_fiscal_surplus_mandate, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sotu_1949_truman_fiscal_surplus_mandate, tangled_rope).
narrative_ontology:human_readable(sotu_1949_truman_fiscal_surplus_mandate, "Federal Budget Surplus Mandate for Inflation Control (Truman 1949)").
narrative_ontology:topic_domain(sotu_1949_truman_fiscal_surplus_mandate, "economics/fiscal_policy/macroeconomic_stabilization").

domain_priors:requires_active_enforcement(sotu_1949_truman_fiscal_surplus_mandate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sotu_1949_truman_fiscal_surplus_mandate, financial_sector).
narrative_ontology:constraint_beneficiary(sotu_1949_truman_fiscal_surplus_mandate, fixed_income_creditors).
narrative_ontology:constraint_beneficiary(sotu_1949_truman_fiscal_surplus_mandate, middle_class_savers).
narrative_ontology:constraint_victim(sotu_1949_truman_fiscal_surplus_mandate, social_program_beneficiaries).
narrative_ontology:constraint_victim(sotu_1949_truman_fiscal_surplus_mandate, low_income_taxpayers).
narrative_ontology:constraint_victim(sotu_1949_truman_fiscal_surplus_mandate, public_sector_workers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SOCIAL PROGRAM BENEFICIARIES (SNARE) — Cannot exit the constraint; face direct extraction through benefit cuts and program deferrals. Suppression is structural: economic dependency, lack of political voice, inability to redirect demand. Maximum experienced extraction with no exit option or organizational capacity.
constraint_indexing:constraint_classification(sotu_1949_truman_fiscal_surplus_mandate, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: LOW TO MIDDLE INCOME TAXPAYERS (TANGLED ROPE) — Bear extraction through sustained high taxation to fund surplus-building; also benefit from inflation control that protects purchasing power and savings. Suppression is high (legal obligation to pay; few tax reduction mechanisms available) but not absolute — exit costs are severe (tax evasion risk) but surmountable. Mixed experience: genuine macroeconomic benefit coupled with direct extraction.
constraint_indexing:constraint_classification(sotu_1949_truman_fiscal_surplus_mandate, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: FINANCIAL SECTOR AND CREDITORS (ROPE) — Primary beneficiary. Surplus-driven inflation control protects debt values, real returns on bonds, and purchasing power of savings. Benefits from reduced money supply growth. Experiences the constraint as pure coordination: budget discipline solves the monetary overheating problem that threatens financial stability. High exit capacity (can shift to alternative jurisdictions, instruments); arbitrage options available.
constraint_indexing:constraint_classification(sotu_1949_truman_fiscal_surplus_mandate, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: LABOR UNIONS AND PUBLIC SECTOR (SCAFFOLD) — Organized actors (unions, civil service associations) see the constraint as temporary austerity with an implicit sunset: once inflation is controlled, the surplus mandate should ease, allowing wage growth and public investment to resume. Theater is moderate — the constraint is functionally justified (inflation control) rather than purely performative. Exit cost is high (cannot defect from federal employment easily) but the sunset logic provides organizational rationale: suppress wages now to stabilize macro, restore growth later.
constraint_indexing:constraint_classification(sotu_1949_truman_fiscal_surplus_mandate, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: CONGRESSIONAL BUDGET COMMITTEES (PITON) — The institutional mechanism for enforcing the surplus mandate persists through procedural inertia and political theater. The constraint remains formally in place even as economic conditions change. Theater ratio reflects that surplus-building continues partly because the institutional framework has ossified, not solely because inflation remains a active threat. Multiple competing interest groups maintain performative support despite underlying pressure to relax the constraint.
constraint_indexing:constraint_classification(sotu_1949_truman_fiscal_surplus_mandate, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (TANGLED ROPE) — From an institutional perspective, the surplus mandate solves the real coordination problem of macroeconomic stabilization (Rope function: constraining money growth requires constraint on spending) while simultaneously redistributing wealth from lower-income groups to financial interests (Snare function: extraction from social programs and taxpayers). The constraint has both genuine coordination value and asymmetric extraction. Neither function can be eliminated without structural redesign.
constraint_indexing:constraint_classification(sotu_1949_truman_fiscal_surplus_mandate, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sotu_1949_truman_fiscal_surplus_mandate_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(sotu_1949_truman_fiscal_surplus_mandate, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(sotu_1949_truman_fiscal_surplus_mandate, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(sotu_1949_truman_fiscal_surplus_mandate, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(sotu_1949_truman_fiscal_surplus_mandate, TR),
    TR >= 0.70.

:- end_tests(sotu_1949_truman_fiscal_surplus_mandate_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The constraint extracts from identifiable groups (social program beneficiaries and lower-income taxpayers) to fund the surplus. The extraction is not as severe as a pure Snare (0.66+) because: (1) some groups benefit from inflation control, (2) the mechanism is justified by a genuine macroeconomic problem (postwar monetary overhang), (3) the constraint is theoretically temporary (should end once inflation is controlled). However, extraction is clear and material — benefit cuts and tax burden increases are real costs borne disproportionately by low-income groups. Suppression (0.58): High. Multiple barriers limit agents' ability to exit or resist: (1) legal obligation to pay taxes (no exit for taxpayers), (2) political powerlessness of social program beneficiaries (trapped), (3) labor market constraints (public sector employees cannot easily find alternative employment), (4) institutional lock-in (Congress can formally renew the mandate). Theater ratio (0.48): Moderate-low. The constraint has substantial functional justification (money absorption does address inflation) but also includes performative elements: rhetorical emphasis on fiscal responsibility, budget 'discipline' as moral virtue, and bureaucratic budget-balancing theater. As the interval progresses, theater increases (0.32 → 0.48) as the initial inflation crisis passes but the constraint persists through institutional inertia.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates a full perspectival gap between beneficiaries and victims. Financial interests see a Rope (pure coordination to stabilize the monetary system). Social program beneficiaries see a Snare (extraction with no coordination benefit for them — they bear costs without receiving macroeconomic protection). Taxpayers see Tangled Rope (mixed: they contribute to inflation control but at cost to their income). Labor unions see Scaffold (temporary austerity with implicit sunset once inflation is controlled). Congress and budget committees see Piton (the institutional mechanism persists through budgetary ritual, maintaining constraints even as the original inflation problem fades). The analytical observer sees the full Tangled Rope structure: genuine coordination function (money absorption) coupled with asymmetric extraction (costs borne by vulnerable groups). This perspectival gap reveals that the constraint's 'naturalness' (treating fiscal surplus as inherent to economic stability) masks a political choice about who bears stabilization costs.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) is derived from each agent's structural relationship to the extraction flow. Financial creditors are full beneficiaries with high exit capacity (d ≈ 0.08) — they can move capital to other countries, instruments, or borrowers if US fiscal policy changes; the sigmoid produces low/negative f(d), reflecting their position as net beneficiaries. Social program beneficiaries are full targets with zero exit capacity (d ≈ 0.95) — they face direct benefit cuts with no alternative; the sigmoid produces high f(d) ≈ 1.42. Taxpayers are mixed: they are targets (extraction through taxation) but also partial beneficiaries (inflation control protects purchasing power); moderate power, constrained exit (d ≈ 0.65 with organized exit, falling to 0.72 if isolated) produces moderate f(d) ≈ 1.00. Effective extraction (χ) = ε × f(d) × σ(S) scales the base extractiveness by directionality and scope. At national scope (σ = 1.0), financial beneficiaries experience χ ≈ 0.52 × -0.12 ≈ -0.06 (subsidy); social program victims experience χ ≈ 0.52 × 1.42 ≈ 0.74 (high extraction); taxpayers experience χ ≈ 0.52 × 1.00 ≈ 0.52 (moderate extraction).
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLUTION PATHWAY: The Tangled Rope classification resolves the mandatrophy by acknowledging both the genuine coordination function (money absorption for inflation control) and the asymmetric extraction (regressive incidence). Neither can be eliminated without redesign. The constraint could be rebalanced by: (1) progressive taxation (shift extraction burden upward), (2) alternative money absorption mechanisms (Federal Reserve open market operations instead of fiscal surplus), or (3) combined approaches (modest surplus + Fed coordination). The current mandate embeds a specific political choice: fiscal austerity is the primary tool, cost is distributed regressively. Recognition of this structure enables policy adjustment without denying either the coordination problem or the extraction reality.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    inflation_threat_measurement,
    'How severe was the actual inflation threat in 1949, and did it justify the magnitude of suppression and extraction?',
    'Comparison of Truman''s inflation projections (1949) against actual inflation outcomes (1950-1952); analysis of Federal Reserve''s own threat assessment; counterfactual modeling of macro outcomes under alternative fiscal rules',
    'If threat was overestimated: constraint reclassifies toward Snare (extraction without coordination justification). If threat was genuine: constraint remains Tangled Rope (justified mix of coordination and extraction). Magnitude determines whether extraction level was proportionate.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(inflation_threat_measurement, empirical, 'Whether inflation threat justified fiscal suppression magnitude').

omega_variable(
    surplus_mechanism_efficacy,
    'Did budget surplus actually reduce inflation, or did other factors (Federal Reserve policy, post-WWII supply normalization, strategic reserve drawdown) drive the outcome?',
    'Econometric decomposition of inflation drivers (1945-1955); counterfactual analysis isolating fiscal impact from monetary policy and supply-side factors; cross-national comparison with countries that did not adopt surplus mandates',
    'If surplus was the primary driver: coordination function is real (constraint justified). If other factors dominated: theater ratio should increase, coordination function weakens, constraint reclassifies toward Piton.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(surplus_mechanism_efficacy, empirical, 'Whether budget surplus was the primary inflation control mechanism').

omega_variable(
    distribution_incidence_accuracy,
    'How accurately did policymakers understand who would bear the extraction cost of the surplus mandate, and were alternatives considered that would spread suppression more evenly?',
    'Analysis of congressional debate (1949-1950); examination of proposed alternatives (progressive tax increases vs. across-the-board cuts); comparison of actual vs. contemplated policy incidence',
    'If policymakers understood but chose regressive extraction: constraint is intentionally extractive, not accidentally so (strengthens Snare reading from victims'' perspective). If alternatives were available but rejected for political reasons: theater increases.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(distribution_incidence_accuracy, empirical, 'Policymaker understanding of extraction incidence and availability of alternatives').

omega_variable(
    sunset_mechanism_clarity,
    'Was there an explicit or implicit understanding that the surplus mandate would be temporary (sunset when inflation was controlled), or was it presented as permanent policy?',
    'Analysis of Truman''s rhetoric (1949 SOTU); Federal Reserve communications about conditions for mandate relaxation; subsequent policy statements (1950-1952) about when surplus would be allowed to decline',
    'If sunset was clear and enforced: constraint is legitimately Scaffold (temporary austerity with restoration logic). If presented as permanent or never actually relaxed: constraint is Piton (inertial, degraded).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sunset_mechanism_clarity, empirical, 'Whether surplus mandate had explicit or implicit sunset conditions').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sotu_1949_truman_fiscal_surplus_mandate, 0, 4).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sotu49_tr_t0, sotu_1949_truman_fiscal_surplus_mandate, theater_ratio, 0, 0.32).
narrative_ontology:measurement(sotu49_tr_t2, sotu_1949_truman_fiscal_surplus_mandate, theater_ratio, 2, 0.4).
narrative_ontology:measurement(sotu49_tr_t4, sotu_1949_truman_fiscal_surplus_mandate, theater_ratio, 4, 0.48).

% Extraction over time
narrative_ontology:measurement(sotu49_be_t0, sotu_1949_truman_fiscal_surplus_mandate, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(sotu49_be_t2, sotu_1949_truman_fiscal_surplus_mandate, base_extractiveness, 2, 0.48).
narrative_ontology:measurement(sotu49_be_t4, sotu_1949_truman_fiscal_surplus_mandate, base_extractiveness, 4, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sotu_1949_truman_fiscal_surplus_mandate, resource_allocation).
narrative_ontology:affects_constraint(sotu_1949_truman_fiscal_surplus_mandate, postwar_inflation_expectation_anchoring).
narrative_ontology:affects_constraint(sotu_1949_truman_fiscal_surplus_mandate, federal_spending_freeze_1949_1950).
narrative_ontology:affects_constraint(sotu_1949_truman_fiscal_surplus_mandate, monetary_policy_substitution_debate).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(sotu_1949_truman_fiscal_surplus_mandate, institutional, 0.12).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
