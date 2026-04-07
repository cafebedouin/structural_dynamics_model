% ============================================================================
% CONSTRAINT STORY: sotu_1976_ford_federal_expenditure_restraint
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-27
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sotu_1976_ford_federal_expenditure_restraint, []).

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
 *   constraint_id: sotu_1976_ford_federal_expenditure_restraint
 *   human_readable: Federal Expenditure and Borrowing Caps (Ford 1976 SOTU Proposal)
 *   domain: economic_policy/fiscal_governance
 *
 * SUMMARY:
 *   President Ford's 1976 State of the Union proposal to impose structural
 *   limits on federal expenditures and borrowing represents a critical shift
 *   in macroeconomic governance from the post-WWII Keynesian consensus toward
 *   what Ford frames as 'new realism' — fiscal discipline as a cure for
 *   inflation rather than an impediment to full employment. The constraint
 *   operates through dual mechanisms: (1) a coordination function that aligns
 *   fiscal and monetary policy goals to reduce money supply growth and
 *   inflation expectations, and (2) an asymmetric extraction mechanism that
 *   concentrates costs on beneficiaries of expansionary domestic programs
 *   (welfare, public employment, social infrastructure) while protecting the
 *   purchasing power of inflation-sensitive creditors and fixed-income
 *   beneficiaries. The constraint's classification as Tangled Rope reflects
 *   genuine coordination alongside distributional extraction: the spending
 *   cap does coordinate macroeconomic policy, but the coordination is
 *   achieved by targeting specific constituencies for austerity. The
 *   constraint exhibits theater (theater_ratio = 0.48) because the framing of
 *   spending limits as economic necessity obscures the political choice of
 *   which adjustment mechanism (spending cuts vs. monetary tightening vs.
 *   incomes policy vs. supply-side reform) bears the distributional costs.
 *   The analytical observer risks misclassifying this as a Mountain (fiscal
 *   limits as economic law) when the structural data reveals identifiable
 *   beneficiaries whose interests are being naturalized as universal
 *   macroeconomic truth.
 *
 * KEY AGENTS:
 *   - Inflation-Sensitive Creditors and Fixed-Income Beneficiaries: Primary beneficiary (institutional/arbitrage) — purchasing power protection and asset value preservation from reduced inflation and money supply growth
 *   - Domestic Program Beneficiaries (welfare recipients, disabled, elderly on fixed income): Primary victim (powerless/trapped) — programs become politically indefensible when framed as inflation cause; no exit from dependence on federal spending
 *   - Public Sector Workers and Local Governments: Secondary victim (moderate/constrained) — face employment risk from federal spending reductions; organized but constrained by fiscal authority of federal government
 *   - Congressional Appropriations Process: Institutional actor (organized/constrained) — loses discretionary policy authority to spending caps; faces pressure to coordinate with Federal Reserve on macroeconomic targets
 *   - Federal Reserve and Monetary Policy Establishment: Beneficiary institution (institutional/arbitrage) — spending caps coordinate with monetary tightening and reinforce inflation-fighting credibility
 *   - Keynesian Economic Consensus: Institutional framework (institutional/arbitrage) — displaced by new macroeconomic paradigm but persists through inertia (Piton classification)
 *   - Wage Earners and Labor Force: Mixed victim-beneficiary (moderate/constrained) — protected from inflation on purchasing power dimension but face employment risk from spending contraction
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sotu_1976_ford_federal_expenditure_restraint, 0.58).
domain_priors:suppression_score(sotu_1976_ford_federal_expenditure_restraint, 0.65).
domain_priors:theater_ratio(sotu_1976_ford_federal_expenditure_restraint, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sotu_1976_ford_federal_expenditure_restraint, extractiveness, 0.58).
narrative_ontology:constraint_metric(sotu_1976_ford_federal_expenditure_restraint, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(sotu_1976_ford_federal_expenditure_restraint, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sotu_1976_ford_federal_expenditure_restraint, tangled_rope).
narrative_ontology:human_readable(sotu_1976_ford_federal_expenditure_restraint, "Federal Expenditure and Borrowing Caps (Ford 1976 SOTU Proposal)").
narrative_ontology:topic_domain(sotu_1976_ford_federal_expenditure_restraint, "economic_policy/fiscal_governance").

domain_priors:requires_active_enforcement(sotu_1976_ford_federal_expenditure_restraint).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sotu_1976_ford_federal_expenditure_restraint, inflation_sensitive_creditors).
narrative_ontology:constraint_beneficiary(sotu_1976_ford_federal_expenditure_restraint, wage_earners_fixed_income).
narrative_ontology:constraint_beneficiary(sotu_1976_ford_federal_expenditure_restraint, savers_purchasing_power).
narrative_ontology:constraint_victim(sotu_1976_ford_federal_expenditure_restraint, domestic_program_beneficiaries).
narrative_ontology:constraint_victim(sotu_1976_ford_federal_expenditure_restraint, discretionary_spending_constituencies).
narrative_ontology:constraint_victim(sotu_1976_ford_federal_expenditure_restraint, public_employment_sector).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DOMESTIC PROGRAM BENEFICIARIES (SNARE) — No exit from federal spending constraints. Benefits from Social Security, welfare, public employment, and social infrastructure become politically undefendable when framed as causing inflation. Trapped by dependence on programs and by the rhetorical collapse of legitimacy for expansionary spending. Maximum experienced extraction — the constraint operates by making these programs targets for reduction.
constraint_indexing:constraint_classification(sotu_1976_ford_federal_expenditure_restraint, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: INFLATION-SENSITIVE CREDITORS (ROPE) — Benefits from spending restraint that reduces money supply growth and protects purchasing power of savings and fixed-income streams (pensions, bonds, annuities). Experiences the constraint as coordination — reducing inflation stabilizes the macroeconomic system that protects their assets. Net beneficiary with arbitrage options (can shift asset allocation if inflation expectations change).
constraint_indexing:constraint_classification(sotu_1976_ford_federal_expenditure_restraint, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: WAGE EARNERS AND LABOR FORCE (TANGLED ROPE) — Benefit from inflation reduction and price stability (purchasing power protection) but face constrained exit because reduced federal spending may contract labor demand in both public and private sectors. Mixed extraction: protect from inflation on one dimension, harmed by employment risk on another. Moderate power — can organize collectively but face real macroeconomic constraints.
constraint_indexing:constraint_classification(sotu_1976_ford_federal_expenditure_restraint, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: CONGRESSIONAL APPROPRIATIONS PROCESS (TANGLED ROPE) — Experiences genuine coordination function (spending caps coordinate fiscal discipline and macroeconomic targets) alongside asymmetric extraction (loses discretionary policy authority and constituency responsiveness). Organized agents (committee chairs, agency leaders) face constrained exit — can lobby for exceptions but cannot abandon the spending cap framework without losing macroeconomic credibility. Enforcement mechanism is active but permeable.
constraint_indexing:constraint_classification(sotu_1976_ford_federal_expenditure_restraint, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: FEDERAL RESERVE AND MONETARY POLICY INSTITUTIONS (SCAFFOLD) — Spending caps coordinate with monetary policy goals (inflation reduction via tighter money). Sees the constraint as temporary support scaffolding the transition to 'new realism' in macroeconomic management. Suppression is tolerated because the Fed can see a sunset: as inflation expectations stabilize and fiscal discipline becomes internalized norm, the hard caps can be relaxed. Moderate theater — the constraint's function (coordinating fiscal and monetary policy) is explicit and measurable.
constraint_indexing:constraint_classification(sotu_1976_ford_federal_expenditure_restraint, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: KEYNESIAN ECONOMIC CONSENSUS (PITON) — The post-WWII Keynesian framework (countercyclical spending, full employment targets) is being displaced but persists institutionally through econometric models and policy rhetoric. Spending caps represent explicit repudiation of Keynesian coordination logic, but the Keynesian framework sustains inertial resistance through academic consensus and policy muscle memory. The piton classification reflects the theater of defending Keynesian policies with inflation-focused language rather than demand-management theory — economists perform Keynesian analysis while legislating against Keynesian policy.
constraint_indexing:constraint_classification(sotu_1976_ford_federal_expenditure_restraint, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational/universal perspective, fiscal sustainability has intrinsic limits: debt-to-GDP ratios, currency stability, and interest rate constraints are mathematical properties of macroeconomic systems, not political choices. This view sees spending caps as discovering an underlying law of economics rather than imposing a political constraint. However, the structural data reveals this as a false summit: identifiable beneficiaries (creditors, inflation-sensitive savers) gain from the framing of fiscal limits as natural law, and the analytical observer risks naturalizing a coalition interest as economic inevitability.
constraint_indexing:constraint_classification(sotu_1976_ford_federal_expenditure_restraint, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sotu_1976_ford_federal_expenditure_restraint_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(sotu_1976_ford_federal_expenditure_restraint, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(sotu_1976_ford_federal_expenditure_restraint, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(sotu_1976_ford_federal_expenditure_restraint, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(sotu_1976_ford_federal_expenditure_restraint, TR),
    TR >= 0.70.

:- end_tests(sotu_1976_ford_federal_expenditure_restraint_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderately high. The constraint imposes real costs on domestic program beneficiaries through reduced federal spending and employment, but the extraction is not as severe as a pure Snare (0.66+) because: (1) the coordination function is genuine — spending caps do align fiscal and monetary policy; (2) benefits are real for some constituencies (inflation reduction); (3) the constraint operates through ostensibly neutral fiscal targets rather than explicit coercion. The extractiveness increased from 0.35 to 0.58 over the 4-year interval as the constraint became more binding — initial proposals were aspirational; implementation faced stronger resistance and required more aggressive enforcement, driving up the real cost to program beneficiaries. Suppression (0.65): High. Significant barriers to resistance include: (1) the framing of spending as inflationary removes the Keynesian legitimacy for expansionary policy; (2) the political organization of inflation-sensitive constituencies (creditors, savers) is diffuse but well-resourced (financial sector backing); (3) the organization of program beneficiaries is concentrated but under-resourced (welfare organizations, unions); (4) media and intellectual capture by monetarist economic theory narrows the space for alternative macroeconomic framings. Theater ratio (0.48): Moderate. The constraint exhibits some theatrical elements (the framing of spending limits as economic necessity, the naturalization of distributional choice), but the coordination function is explicit and measurable, reducing the pure performative content. The theater increased over the interval as enforcement required more ideological work to maintain — actual spending cuts are politically costly, so more rhetoric is required to justify continued enforcement.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates a sharp divide between beneficiary and victim perspectives. The inflation-sensitive creditors and savers see Rope (pure coordination of macroeconomic policy); the Federal Reserve sees Scaffold (temporary support for transition to 'new realism' with sunset as inflation expectations stabilize); Congress sees Tangled Rope (genuine coordination with asymmetric loss of discretionary authority); wage earners see Tangled Rope (inflation protection with employment risk); domestic program beneficiaries see Snare (no exit, no benefit, pure extraction through reduced program spending); the Keynesian consensus sees Piton (their own framework being degraded through institutional inertia). The analytical observer at the civilizational level risks seeing Mountain (fiscal limits as economic law) — but the presence of identifiable beneficiaries reveals this as a false summit. The perspectival gap is not an observational ambiguity; it is a structural reality. The constraint genuinely benefits some constituencies and harms others. The disagreement about type reflects real distributional conflict, not measurement uncertainty.
 *
 * DIRECTIONALITY LOGIC:
 *   The constraint's directionality (d) varies sharply across perspectives, driven by structural position in the fiscal system. Inflation-sensitive creditors have d ≈ 0.10 (full beneficiaries with arbitrage options — can reallocate assets if inflation expectations change); domestic program beneficiaries have d ≈ 0.90 (full victims with no exit); wage earners have d ≈ 0.55 (mixed: protected from inflation, threatened by employment contraction); congressional institutional actors have d ≈ 0.60 (constrained by loss of discretionary authority, but some ability to pressure for exceptions). The Federal Reserve has d ≈ 0.15 (beneficiary institution with arbitrage — can adjust monetary policy independently). These divergent d values produce the sharp perspectival gap: the same constraint appears as a beneficial coordination mechanism (Rope/Scaffold) from the beneficiary perspectives and as pure extraction (Snare) from the victim perspectives. The engine's sigmoid f(d) amplifies these differences: f(0.10) ≈ -0.01 (negative experienced extraction); f(0.90) ≈ 1.39 (high experienced extraction). This perspectival gap is diagnostic — it reveals that the constraint's legitimacy depends entirely on your structural position in the fiscal system.
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLVED via perspectival mapping. The mandatrophy (Can fiscal discipline be both coordination and extraction?) is resolved by recognizing that it IS both — the constraint achieves coordination on one dimension (aligning fiscal and monetary policy) while achieving extraction on another (concentrating adjustment costs on program beneficiaries rather than on inflation-sensitive creditors or monetary policy itself). The classification as Tangled Rope reflects this hybrid character: genuine coordination (χ includes beneficiary function) alongside asymmetric extraction (χ reflects high suppression and concentrated costs). The false summit risk (mountain classification naturalizing fiscal limits as economic law) is addressed by the beneficiary structure: identifiable agents (creditors, savers, financial sector) gain from the 'natural law' framing, revealing it as rationalization of distributional choice rather than discovery of economic necessity. The engine's false summit detector should flag this constraint as a FSM candidate because: (1) it is classified as Mountain from the analytical/civilizational perspective; (2) it declares beneficiaries (inflation-sensitive creditors); (3) the network/institutional structure reveals that the 'natural law' framing serves specific interests. The schema requires omegas for FSM candidates — these are provided and explicitly document the natural-law-vs-political-choice ambiguity.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    inflation_causation_attribution,
    'How much of 1970s inflation is caused by federal spending excess vs. oil shocks, wage-price spirals, monetary accommodation, and supply constraints?',
    'Econometric decomposition of inflation sources; comparison of spending reduction effects across different macroeconomic scenarios; international comparison of inflation in countries with different spending levels',
    'If spending is primary driver: spending caps are well-targeted coordination mechanism (Rope/Scaffold more plausible). If spending is secondary: caps are misdirected extraction targeting the wrong mechanism (Snare more accurate for beneficiaries of misdirected policy).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(inflation_causation_attribution, empirical, 'Attribution of inflation causation to federal spending vs. other sources').

omega_variable(
    distributional_alternatives_availability,
    'Are spending caps the only institutional mechanism to coordinate inflation reduction, or do alternatives exist (monetary policy tightening alone, incomes policy, supply-side reform)?',
    'Historical counterfactual analysis of alternative policy regimes; econometric simulation of inflation reduction under different policy mixes; examination of international cases achieving disinflation without spending caps',
    'If alternatives available with lower distributional cost: spending caps are extractive choice rather than structural necessity (Snare classification confirmed). If spending caps are least-extractive option: Rope/Tangled Rope classification confirmed.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(distributional_alternatives_availability, empirical, 'Availability of alternative mechanisms for inflation coordination').

omega_variable(
    credibility_gap_in_enforcement,
    'Can political institutions enforce multi-year spending caps credibly, or will exceptions and supplemental appropriations erode the constraint''s real effect (converting Tangled Rope to Piton)?',
    'Historical tracking of spending cap adherence (actual vs. capped levels); frequency of formal and informal exceptions; measurement of theater_ratio over time (rising theater indicates inertial enforcement without real effect)',
    'If enforcement is credible and persistent: Tangled Rope classification holds. If enforcement degrades into ritual: constraint transitions to Piton (high theater, low function). This determines whether the constraint is a real coordination mechanism or a performative policy framework.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(credibility_gap_in_enforcement, empirical, 'Credibility and stability of political enforcement of spending caps').

omega_variable(
    natural_law_vs_political_choice,
    'Is the mountain perspective (fiscal limits as economic law) a genuine discovery or a rationalization of distributional choice (naturalizing a coalition interest as inevitable)?',
    'Historical analysis of pre-1976 fiscal policy debates revealing that spending levels were politically chosen, not economically determined. Identification of beneficiaries of the natural-law framing (creditors, inflation-sensitive actors). Post-resolution analysis of whether fiscal ''limits'' persist when inflation is controlled or whether spending policy reverts to discretionary choice.',
    'If mountain is false summit: the analytical observer is participating in a distributional rationalization (the organized articulation of beneficiary interests as universal law). This triggers FSM reclassification to Tangled Rope, revealing the constraint as coalition extraction, not economic necessity.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(natural_law_vs_political_choice, conceptual, 'Whether fiscal limits are economic law or political choice rationalization').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sotu_1976_ford_federal_expenditure_restraint, 0, 4).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ford76_tr_t0, sotu_1976_ford_federal_expenditure_restraint, theater_ratio, 0, 0.32).
narrative_ontology:measurement(ford76_tr_t2, sotu_1976_ford_federal_expenditure_restraint, theater_ratio, 2, 0.4).
narrative_ontology:measurement(ford76_tr_t4, sotu_1976_ford_federal_expenditure_restraint, theater_ratio, 4, 0.48).

% Extraction over time
narrative_ontology:measurement(ford76_be_t0, sotu_1976_ford_federal_expenditure_restraint, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(ford76_be_t2, sotu_1976_ford_federal_expenditure_restraint, base_extractiveness, 2, 0.48).
narrative_ontology:measurement(ford76_be_t4, sotu_1976_ford_federal_expenditure_restraint, base_extractiveness, 4, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sotu_1976_ford_federal_expenditure_restraint, enforcement_mechanism).
narrative_ontology:affects_constraint(sotu_1976_ford_federal_expenditure_restraint, great_inflation_monetary_policy_regime).
narrative_ontology:affects_constraint(sotu_1976_ford_federal_expenditure_restraint, welfare_state_retrenchment_political_feasibility).
narrative_ontology:affects_constraint(sotu_1976_ford_federal_expenditure_restraint, labor_market_employment_policy_framework).

% DUAL FORMULATION NOTE:
% The federal expenditure restraint constraint is structurally distinct from the underlying inflation itself (which has multiple causes — oil shocks, wage-price spirals, monetary accommodation). The spending cap is a policy response that coordinates fiscal and monetary tightening but imposes distributional costs. Upstream constraints include the monetary policy regime that drives the need for fiscal coordination; downstream constraints include specific welfare and employment policy frameworks that must adapt to spending caps. The network reflects causal and institutional coupling: changes in the spending cap constraint affect the feasibility and design of retrenchment programs, which in turn affect labor market coordination.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(sotu_1976_ford_federal_expenditure_restraint, institutional, 0.6).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
