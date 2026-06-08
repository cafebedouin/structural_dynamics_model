% ============================================================================
% CONSTRAINT STORY: x_date_timing
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_x_date_timing, []).

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
    narrative_ontology:constraint_vindicates/2,
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
 *   constraint_id: x_date_timing
 *   human_readable: X-Date Timing Constraint in Debt Ceiling Crises
 *   domain: public_finance/constitutional_law/political_economy
 *
 * SUMMARY:
 *   The X-date is the projected date when the Treasury Department exhausts
 *   both its extraordinary measures and available cash, rendering it unable
 *   to meet all federal obligations in full and on time. This constraint
 *   emerges from the interaction of three structural elements: (1) the
 *   statutory debt ceiling limiting total federal borrowing, (2) Treasury's
 *   legal obligation to pay all congressionally appropriated obligations, and
 *   (3) the temporal gap between when the ceiling is reached and when
 *   Congress acts to raise it. The X-date timing is not a natural fiscal
 *   limit but a constructed crisis point that grants Treasury substantial
 *   discretionary authority over cash management and payment timing while
 *   imposing asymmetric costs on obligation recipients who cannot exit the
 *   federal payment system. The constraint exhibits both genuine coordination
 *   function (managing cash flow under legal borrowing limits requires
 *   sophisticated forecasting and extraordinary measures deployment) and
 *   substantial extraction (the uncertainty creates payment risk for trapped
 *   beneficiaries, enables political brinksmanship, and has become
 *   increasingly theatrical as the debt ceiling ritual has lost its original
 *   fiscal discipline function). The interval spans 1995-2015, covering the
 *   modern era of recurring debt ceiling crises and the evolution of
 *   extraordinary measures from emergency tools to routine crisis management.
 *
 * KEY AGENTS:
 *   - Treasury Department: Primary beneficiary (institutional/arbitrage) — gains discretionary authority over cash management, payment timing, and extraordinary measures deployment; controls the crisis timeline and forecast
 *   - Federal Program Beneficiaries: Primary victim (powerless/trapped) — Social Security recipients, Medicare providers, federal contractors face immediate payment disruption risk with no exit option
 *   - State Governments: Secondary victim (moderate/constrained) — depend on federal transfers but have some fiscal autonomy; forced to maintain larger reserves due to X-date uncertainty
 *   - Financial Market Participants: Beneficiary (institutional/arbitrage) — profit from volatility and mispricing; can exit to alternative assets
 *   - Fiscal Reform Coalition: Organized agents (organized/constrained) — advocacy groups and reform legislators seeking to eliminate the debt ceiling mechanism
 *   - Congressional Budget Process: Institutional actor (institutional/constrained) — maintains the debt ceiling ritual despite its degraded function
 *   - Fiscal Policy Predictability: Abstract victim (powerless/trapped) — the collective good of stable fiscal planning; cannot organize or exit
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(x_date_timing, 0.48).
domain_priors:suppression_score(x_date_timing, 0.62).
domain_priors:theater_ratio(x_date_timing, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(x_date_timing, extractiveness, 0.48).
narrative_ontology:constraint_metric(x_date_timing, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(x_date_timing, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(x_date_timing, tangled_rope).
narrative_ontology:human_readable(x_date_timing, "X-Date Timing Constraint in Debt Ceiling Crises").
narrative_ontology:topic_domain(x_date_timing, "public_finance/constitutional_law/political_economy").

domain_priors:requires_active_enforcement(x_date_timing).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(x_date_timing, treasury_department).
narrative_ontology:constraint_beneficiary(x_date_timing, executive_branch_negotiators).
narrative_ontology:constraint_beneficiary(x_date_timing, financial_market_participants).
narrative_ontology:constraint_victim(x_date_timing, obligation_recipients).
narrative_ontology:constraint_victim(x_date_timing, federal_program_beneficiaries).
narrative_ontology:constraint_victim(x_date_timing, state_governments).
narrative_ontology:constraint_victim(x_date_timing, fiscal_policy_predictability).
narrative_ontology:constraint_vindicates(x_date_timing, executive_discretion_doctrine).
narrative_ontology:constraint_vindicates(x_date_timing, extraordinary_measures_legitimacy).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: FEDERAL PROGRAM BENEFICIARIES (SNARE) — Social Security recipients, Medicare providers, federal contractors, and other obligation recipients cannot exit the system and face immediate payment disruption risk. No alternative income source during X-date uncertainty. Maximum extraction: the timing constraint creates existential payment risk for agents with no structural mobility.
constraint_indexing:constraint_classification(x_date_timing, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: STATE GOVERNMENTS (TANGLED ROPE) — Depend on federal transfers (Medicaid, highway funds, education grants) but have some fiscal autonomy and borrowing capacity. The X-date timing creates genuine coordination need (federal-state fiscal planning) alongside extraction (uncertainty forces states to maintain larger reserves, reducing available resources for state programs). Constrained exit: can borrow short-term but cannot exit federal funding dependency.
constraint_indexing:constraint_classification(x_date_timing, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: TREASURY DEPARTMENT (ROPE) — Primary beneficiary. The X-date timing constraint grants Treasury extraordinary discretion over cash management, payment prioritization, and the political leverage that comes from controlling the crisis timeline. Experiences the constraint as coordination: managing cash flow under the debt ceiling requires sophisticated forecasting and extraordinary measures deployment. Net beneficiary through enhanced institutional authority and negotiating position.
constraint_indexing:constraint_classification(x_date_timing, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: FINANCIAL MARKET PARTICIPANTS (ROPE) — Sophisticated actors with arbitrage capacity. The X-date creates trading opportunities (Treasury bill mispricing, volatility plays, safe-haven flows) and coordination benefits (clear timeline for hedging strategies). Can exit to alternative assets or jurisdictions. Low effective extraction despite market volatility because these agents have structural mobility and profit from the uncertainty.
constraint_indexing:constraint_classification(x_date_timing, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: FISCAL REFORM COALITION (SCAFFOLD) — Organized advocacy groups, think tanks, and reform-minded legislators see the X-date constraint as a temporary crisis mechanism that should sunset through structural reform (automatic debt ceiling increases, elimination of the ceiling, or mandatory continuing appropriations). The constraint's dysfunction is the argument for its elimination. Constrained exit: can advocate for reform but cannot unilaterally change the system.
constraint_indexing:constraint_classification(x_date_timing, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: CONGRESSIONAL BUDGET PROCESS (PITON) — The debt ceiling vote was originally designed as a coordination mechanism (streamline borrowing authorization) but has atrophied into theater. The X-date timing is now primarily performative: Congress knows the ceiling will be raised, Treasury knows it will be raised, markets know it will be raised, yet the ritual persists. The constraint's original function (fiscal discipline) has degraded into a recurring crisis performance maintained through institutional inertia.
constraint_indexing:constraint_classification(x_date_timing, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (TANGLED ROPE) — The X-date timing constraint exhibits genuine coordination function (Treasury must manage cash flow under legal borrowing limits; some mechanism for authorizing debt is necessary) alongside substantial extraction (the uncertainty creates asymmetric costs for trapped agents, grants discretionary authority to Treasury, and enables political brinksmanship). The constraint is not a natural law (the debt ceiling is a statutory artifact, not a fiscal necessity) but also not pure extraction (some borrowing authorization mechanism is required in a constitutional system with separated powers).
constraint_indexing:constraint_classification(x_date_timing, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(x_date_timing_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(x_date_timing, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(x_date_timing, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(x_date_timing, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(x_date_timing, TR),
    TR >= 0.70.

:- end_tests(x_date_timing_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.48): Moderate-high. The X-date timing creates substantial asymmetric costs. Obligation recipients face payment disruption risk they cannot hedge or exit. State governments must maintain larger cash reserves, reducing resources available for state programs. The uncertainty itself is extractive: even if the ceiling is always raised, the recurring crisis imposes planning costs, market volatility, and political leverage asymmetry. However, extraction is not maximal because: (1) some agents benefit (Treasury gains authority, markets profit from volatility), (2) the constraint has genuine coordination function (some mechanism for authorizing debt is constitutionally necessary), and (3) the crisis has never resulted in actual default (the threat is real but the outcome has been averted). The value has increased over the interval as debt ceiling crises have become more frequent and more severe. Suppression (0.62): Moderate-high. Obligation recipients cannot exit the federal payment system. State governments cannot exit federal funding dependency. The legal structure (Anti-Deficiency Act, debt ceiling statute, appropriations law) creates binding constraints with criminal penalties for violation. However, suppression is not total: Treasury has some discretion over payment timing and extraordinary measures deployment, Congress can raise the ceiling at any time, and sophisticated actors (financial markets) can hedge or exit. The suppression has increased over the interval as political polarization has made debt ceiling increases more difficult and as the stock of outstanding debt has grown, making the consequences of breach more severe. Theater ratio (0.58): Moderate-high. The debt ceiling vote was originally designed as a coordination mechanism (streamline borrowing authorization during WWI) but has atrophied into recurring crisis theater. All parties know the ceiling will be raised, yet the ritual persists. The X-date forecast itself has theatrical elements: Treasury has discretion over the precision and timing of its projections, and the forecast serves political functions (creating urgency for negotiations) beyond its technical function (cash management). However, the theater is not total: Treasury's cash management is genuinely constrained by the ceiling, the X-date forecast requires sophisticated technical analysis, and the consequences of breach would be real (even if never realized). The theater ratio has increased over the interval as the debt ceiling has become more routinely used for political leverage and as the gap between the constraint's original function (fiscal discipline) and its current function (crisis performance) has widened.
 *
 * PERSPECTIVAL GAP:
 *   The X-date timing constraint demonstrates how the same structural phenomenon appears radically different depending on the observer's position. Treasury sees coordination (Rope): managing cash flow under legal limits requires sophisticated forecasting and extraordinary measures, and the constraint grants them legitimate authority. Federal program beneficiaries see pure extraction (Snare): they face existential payment risk with no exit option and no benefit from the constraint. Financial markets see coordination (Rope): the X-date creates clear timelines for hedging and trading opportunities. State governments see mixed coordination and extraction (Tangled Rope): they need federal fiscal planning but the uncertainty forces costly reserve accumulation. The fiscal reform coalition sees a temporary problem with a sunset (Scaffold): the constraint's dysfunction is the argument for its elimination. The congressional budget process sees its own degraded ritual (Piton): the debt ceiling vote persists through inertia despite having lost its original fiscal discipline function. The analytical observer sees tangled rope because the constraint genuinely combines coordination (borrowing authorization is constitutionally necessary) with extraction (the recurring crisis imposes asymmetric costs and has become increasingly theatrical). The perspectival gap is not a disagreement about facts but a structural consequence of different agents' relationships to the constraint: beneficiaries see coordination, victims see extraction, organized reformers see a sunset, degraded institutions see performance, and the analytical view integrates all perspectives into a recognition that the constraint is genuinely hybrid.
 *
 * DIRECTIONALITY LOGIC:
 *   Treasury Department and financial market participants are primary beneficiaries. Treasury gains discretionary authority over cash management, payment prioritization (if legal), and the political leverage that comes from controlling the crisis timeline. The X-date forecast is a Treasury product, and the uncertainty grants Treasury enhanced institutional authority. Financial markets profit from volatility, mispricing opportunities, and safe-haven flows. Both groups have arbitrage-level exit options and experience low or negative effective extraction. Federal program beneficiaries and fiscal policy predictability are primary victims. Obligation recipients face immediate payment disruption risk with no exit option. The uncertainty itself is extractive even if payments are never actually missed. These agents are powerless and trapped, experiencing maximum effective extraction. State governments are secondary victims with constrained exit. They depend on federal transfers but have some fiscal autonomy and borrowing capacity. The X-date uncertainty forces them to maintain larger reserves (opportunity cost) but they can partially hedge through short-term borrowing. They experience moderate extraction. The fiscal reform coalition sees the constraint as temporary (scaffold perspective) because they are organized and working toward structural reform. The congressional budget process sees its own ritual as degraded (piton perspective) because the original coordination function has atrophied into performance. The analytical observer sees tangled rope because the constraint exhibits both genuine coordination function (some borrowing authorization mechanism is constitutionally necessary) and substantial extraction (the uncertainty creates asymmetric costs and enables political brinksmanship).
 *
 * MANDATROPHY ANALYSIS:
 *   The X-date timing constraint resolves the mandatrophy by demonstrating that tangled rope classification is structurally stable across multiple perspectives when both coordination and extraction are genuine. The constraint is not mislabeled coordination (it genuinely requires Treasury to manage cash flow under legal borrowing limits) and not mislabeled extraction (it genuinely imposes asymmetric costs on trapped obligation recipients and has become increasingly theatrical). The mandate (authorize federal borrowing in a constitutional system with separated powers) persists, but the mechanism (debt ceiling with recurring crises) has accumulated substantial extractive overhead. The analytical perspective confirms tangled rope because: (1) beneficiaries are clearly identified (Treasury, financial markets), (2) victims are clearly identified (obligation recipients, state governments, fiscal predictability), (3) active enforcement is required (Treasury must deploy extraordinary measures, Congress must vote to raise the ceiling), and (4) the constraint exhibits both coordination function (borrowing authorization) and extraction (payment risk, political leverage, theatrical performance). The constraint is not a false summit (it is not a natural fiscal limit being naturalized) but a genuine institutional hybrid where coordination and extraction are inseparable.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    prioritization_authority_ambiguity,
    'Does Treasury have legal authority to prioritize certain payments over others after the X-date, or does the Anti-Deficiency Act require equal treatment of all obligations?',
    'Judicial resolution of a test case; OLC opinion with binding force; or actual X-date breach revealing Treasury''s operational choice',
    'If prioritization is legal: Treasury''s discretionary authority is even greater than currently assumed, increasing extractiveness toward non-prioritized recipients. If illegal: Treasury has no choice but default, making the constraint more mountain-like (legal impossibility) from Treasury''s perspective.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(prioritization_authority_ambiguity, conceptual, 'Legal ambiguity over Treasury''s payment prioritization authority').

omega_variable(
    extraordinary_measures_exhaustion_precision,
    'How precisely can Treasury forecast the X-date given uncertainty in daily cash flows, tax receipts, and extraordinary measures headroom?',
    'Historical analysis of Treasury X-date forecast accuracy; comparison of projected vs actual exhaustion dates across multiple debt ceiling episodes',
    'If precision is high (±1 week): the X-date is a genuine coordination tool and Treasury''s forecasting authority is justified. If precision is low (±1 month): the X-date is partly theatrical, and the crisis timeline is more constructed than discovered.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraordinary_measures_exhaustion_precision, empirical, 'Precision of X-date forecasting given cash flow uncertainty').

omega_variable(
    market_discipline_vs_political_theater,
    'Does the X-date constraint impose genuine market discipline on fiscal policy (forcing periodic debt sustainability review), or is it pure political theater that markets have learned to ignore?',
    'Analysis of sovereign risk pricing during debt ceiling episodes; correlation between X-date proximity and Treasury yield spreads; comparison of US borrowing costs to countries without debt ceiling mechanisms',
    'If genuine discipline: the constraint has coordination value beyond the immediate crisis (Rope from more perspectives). If pure theater: the constraint is extraction and performance with no fiscal benefit (Snare/Piton from more perspectives).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(market_discipline_vs_political_theater, empirical, 'Whether X-date timing imposes market discipline or is political theater').

omega_variable(
    constitutional_debt_validity_clause,
    'Does the 14th Amendment''s public debt validity clause grant the President authority to ignore the debt ceiling and continue borrowing to meet obligations?',
    'Supreme Court ruling on a test case; executive invocation of the clause followed by judicial review; or scholarly consensus on constitutional interpretation',
    'If the clause grants authority: the debt ceiling is constitutionally void, and the X-date constraint is a self-imposed political ritual (Piton from all perspectives). If the clause does not grant authority: the constraint is a genuine legal limit (Mountain from institutional perspectives).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(constitutional_debt_validity_clause, conceptual, 'Constitutional validity of debt ceiling under 14th Amendment').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(x_date_timing, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(xdate_theater_1995, x_date_timing, theater_ratio, 0, 0.35).
narrative_ontology:measurement(xdate_theater_2000, x_date_timing, theater_ratio, 5, 0.42).
narrative_ontology:measurement(xdate_theater_2005, x_date_timing, theater_ratio, 10, 0.48).
narrative_ontology:measurement(xdate_theater_2010, x_date_timing, theater_ratio, 15, 0.54).
narrative_ontology:measurement(xdate_theater_2015, x_date_timing, theater_ratio, 20, 0.58).

% Extraction over time
narrative_ontology:measurement(xdate_extract_1995, x_date_timing, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(xdate_extract_2000, x_date_timing, base_extractiveness, 5, 0.33).
narrative_ontology:measurement(xdate_extract_2005, x_date_timing, base_extractiveness, 10, 0.38).
narrative_ontology:measurement(xdate_extract_2010, x_date_timing, base_extractiveness, 15, 0.44).
narrative_ontology:measurement(xdate_extract_2015, x_date_timing, base_extractiveness, 20, 0.48).

% Suppression requirement over time
narrative_ontology:measurement(xdate_suppress_1995, x_date_timing, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(xdate_suppress_2005, x_date_timing, suppression_requirement, 10, 0.55).
narrative_ontology:measurement(xdate_suppress_2015, x_date_timing, suppression_requirement, 20, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(x_date_timing, enforcement_mechanism).

% DUAL FORMULATION NOTE:
% The X-date timing constraint is downstream of four structural constraints: (1) extraordinary_measures_duration (mountain) — the technical limits on how long Treasury can operate under the ceiling using accounting maneuvers, (2) treasury_cash_discretion (rope) — Treasury's authority to manage cash balances and payment timing, (3) iran_conflict_spending (snare) — exogenous spending shocks that accelerate X-date arrival, and (4) economic_condition_uncertainty (tangled_rope) — revenue and outlay volatility that affects cash flow forecasting. Each upstream constraint has its own extractiveness value reflecting its specific structural dynamics. The X-date timing constraint integrates these upstream dynamics into a single crisis point where Treasury's cash management authority, legal borrowing limits, and political negotiation timelines converge.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
