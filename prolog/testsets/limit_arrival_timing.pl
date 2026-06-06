% ============================================================================
% CONSTRAINT STORY: limit_arrival_timing
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_limit_arrival_timing, []).

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
 *   constraint_id: limit_arrival_timing
 *   human_readable: Debt Limit Arrival Timing as Fiscal Constraint
 *   domain: public_finance/constitutional_law/political_economy
 *
 * SUMMARY:
 *   The debt limit arrival timing constraint describes the structural dynamic
 *   by which the U.S. Treasury Department controls when the statutory debt
 *   ceiling is reached through cash management decisions, extraordinary
 *   measures deployment, and auction schedule adjustments. While the debt
 *   limit itself is a congressional constraint on borrowing authority, the
 *   timing of when that limit binds is substantially controlled by executive
 *   branch discretion. This creates an asymmetric information and power
 *   relationship: Treasury knows the cash flow trajectory and can modulate
 *   it; Congress observes only lagged public data and must react to
 *   Treasury's declared X-date (the date when extraordinary measures are
 *   exhausted and default risk becomes imminent). The constraint exhibits
 *   tangled rope characteristics from the analytical perspective: it serves a
 *   genuine coordination function (preventing unilateral executive spending
 *   beyond appropriations, forcing periodic fiscal review) while
 *   simultaneously enabling extraction (Treasury's timing control creates
 *   artificial urgency that favors executive preferences, undermines
 *   congressional deliberation, and transfers de facto fiscal authority from
 *   the legislative to the executive branch). The theater ratio has increased
 *   over the interval as debt limit crises have become routine and the
 *   congressional oversight function has atrophied into performative
 *   brinkmanship.
 *
 * KEY AGENTS:
 *   - Treasury Department: Primary beneficiary (institutional/arbitrage) — controls timing levers and captures institutional advantage during crisis windows
 *   - Congressional Budget Authority: Primary victim (powerless/trapped) — constitutional appropriations power is nominally preserved but functionally hollowed out by executive timing control
 *   - Bond Market Participants: Secondary victim (moderate/constrained) — experience coordination benefits (liquidity management) alongside extraction (volatility from timing uncertainty)
 *   - Fiscal Transparency Advocates: Organized observers (organized/constrained) — see both coordination and extraction; advocate for reform but cannot unilaterally change statutory structure
 *   - Debt Limit Abolition Coalition: Organized reformers (organized/mobile) — building alternative constitutional interpretations and legislative vehicles for repeal
 *   - Congressional Appropriations Committees: Institutional actors (institutional/constrained) — maintain ritual oversight but functional authority has atrophied (piton perspective)
 *   - Analytical Observer: Civilizational view (analytical/analytical) — identifies tangled rope structure with genuine coordination coexisting with substantial extraction
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(limit_arrival_timing, 0.48).
domain_priors:suppression_score(limit_arrival_timing, 0.62).
domain_priors:theater_ratio(limit_arrival_timing, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(limit_arrival_timing, extractiveness, 0.48).
narrative_ontology:constraint_metric(limit_arrival_timing, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(limit_arrival_timing, theater_ratio, 0.68).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(limit_arrival_timing, accessibility_collapse, 0.18).
narrative_ontology:constraint_metric(limit_arrival_timing, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(limit_arrival_timing, tangled_rope).
narrative_ontology:human_readable(limit_arrival_timing, "Debt Limit Arrival Timing as Fiscal Constraint").
narrative_ontology:topic_domain(limit_arrival_timing, "public_finance/constitutional_law/political_economy").

domain_priors:requires_active_enforcement(limit_arrival_timing).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(limit_arrival_timing, treasury_department).
narrative_ontology:constraint_beneficiary(limit_arrival_timing, executive_branch_fiscal_discretion).
narrative_ontology:constraint_victim(limit_arrival_timing, congressional_budget_authority).
narrative_ontology:constraint_victim(limit_arrival_timing, fiscal_transparency).
narrative_ontology:constraint_victim(limit_arrival_timing, bond_market_predictability).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CONGRESSIONAL BUDGET AUTHORITY (SNARE) — Trapped by constitutional appropriations power that has been structurally bypassed. Cannot exit the constraint because the debt limit mechanism itself is statutory, yet the timing of arrival is controlled by Treasury cash management decisions Congress cannot observe in real time. Bears maximum extraction: constitutional authority over spending is nominally preserved but functionally hollowed out by the executive's control over when the crisis arrives.
constraint_indexing:constraint_classification(limit_arrival_timing, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: BOND MARKET PARTICIPANTS (TANGLED ROPE) — Constrained by exposure to U.S. sovereign debt but also benefit from Treasury's liquidity management expertise. Experience genuine coordination (Treasury provides advance notice, maintains orderly markets) alongside extraction (uncertainty about X-date creates volatility, Treasury's cash choices can accelerate or delay crisis unpredictably). High exit costs but not trapped — can reduce exposure or hedge, though at significant cost.
constraint_indexing:constraint_classification(limit_arrival_timing, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: TREASURY DEPARTMENT (ROPE) — Primary beneficiary with arbitrage-level exit options. Experiences the constraint as coordination: managing cash flow to avoid default while Congress resolves the limit is a legitimate operational necessity. Controls the timing levers (extraordinary measures deployment, cash balance drawdown rate, auction schedule adjustments) and captures institutional advantage during the crisis window. Net beneficiary — the constraint amplifies Treasury's discretion relative to congressional oversight.
constraint_indexing:constraint_classification(limit_arrival_timing, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: FISCAL TRANSPARENCY ADVOCATES (TANGLED ROPE) — Organized coalitions (Government Accountability Office, Congressional Budget Office, budget watchdog groups) see both coordination and extraction. Coordination: the debt limit mechanism forces periodic fiscal review. Extraction: Treasury's control over arrival timing undermines the review's integrity by creating artificial urgency that favors executive preferences. Constrained exit — can advocate for reform but cannot unilaterally change the statutory structure.
constraint_indexing:constraint_classification(limit_arrival_timing, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: DEBT LIMIT ABOLITION COALITION (SCAFFOLD) — Organized reformers (some economists, legal scholars, progressive legislators) see the entire debt limit mechanism as a temporary pathology with a clear sunset: either statutory repeal or judicial/executive nullification via 14th Amendment Section 4 ('validity of the public debt shall not be questioned'). The arrival timing constraint is a symptom of a larger dysfunction that is being actively dismantled. Mobile exit — the coalition is building alternative constitutional interpretations and legislative vehicles.
constraint_indexing:constraint_classification(limit_arrival_timing, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 6: CONGRESSIONAL APPROPRIATIONS COMMITTEES (PITON) — The committees maintain the ritual of debt limit votes and oversight hearings, but the functional authority has atrophied. The theater ratio is high: hearings focus on symbolic fiscal responsibility while the actual timing and resolution are controlled by Treasury cash management and executive-legislative brinkmanship outside the appropriations process. The committees see their own process as degraded — maintained because no alternative institutional arrangement has replaced it, not because it effectively constrains spending.
constraint_indexing:constraint_classification(limit_arrival_timing, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (TANGLED ROPE) — From a civilizational analytical perspective, the constraint exhibits both genuine coordination (preventing unilateral executive spending beyond appropriations) and substantial extraction (Treasury's timing control creates asymmetric information and artificial crisis dynamics that favor executive preferences over congressional deliberation). The structural data supports tangled_rope: beneficiaries and victims are clearly identifiable, active enforcement is required, and the coordination function coexists with extractive asymmetry.
constraint_indexing:constraint_classification(limit_arrival_timing, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(limit_arrival_timing_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(limit_arrival_timing, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(limit_arrival_timing, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(limit_arrival_timing, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(limit_arrival_timing, TR),
    TR >= 0.70.

:- end_tests(limit_arrival_timing_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.48): Moderate-high. Treasury's control over arrival timing creates substantial asymmetric information advantage and transfers de facto fiscal authority from Congress to the executive branch. The extraction is not maximal because some coordination function remains (the limit does force periodic review, and Treasury's cash management does prevent operational disruptions), but the career and institutional advantages captured by Treasury during crisis windows are significant. The value reflects that roughly half of the constraint's operation is extractive overhead beyond the coordination floor. Suppression (0.62): Moderate-high. Congressional budget authority faces significant barriers to effective oversight: real-time cash flow data is not publicly available, extraordinary measures deployment is at Treasury's discretion, and the statutory structure creates artificial urgency that favors executive preferences. Suppression has increased over the interval as the executive branch has refined its timing control techniques and Congress has become habituated to crisis-driven resolutions. Theater ratio (0.68): High. Congressional appropriations oversight and debt limit votes are substantially performative. The committees hold hearings and issue reports, but the actual fiscal decisions are made through executive-legislative brinkmanship outside the appropriations process, with Treasury controlling the crisis timeline. The theater has increased as debt limit crises have become routine and the oversight function has degraded into symbolic fiscal responsibility posturing.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates how the same structural phenomenon — Treasury's control over debt limit arrival timing — appears as pure extraction (snare) from Congress's trapped position, as coordination (rope) from Treasury's beneficiary position, as a temporary pathology with a sunset (scaffold) from the abolition coalition's organized position, as degraded ritual (piton) from the appropriations committees' institutional position, and as mixed coordination-extraction (tangled_rope) from the analytical observer's civilizational perspective. The gap is not a measurement error — it reflects genuine differences in structural position. Congress is trapped by constitutional appropriations authority that has been functionally bypassed; Treasury is a beneficiary with arbitrage-level control over the timing levers; the abolition coalition sees a clear exit path through constitutional reinterpretation or statutory repeal; the appropriations committees see their own process as atrophied performance. The analytical perspective integrates these views: the constraint does serve a coordination function (forcing periodic fiscal review), but that function coexists with substantial extraction (Treasury's timing control undermines congressional deliberation and transfers fiscal authority to the executive branch).
 *
 * DIRECTIONALITY LOGIC:
 *   Treasury Department is the primary beneficiary with arbitrage-level exit options — it controls the timing levers and can modulate cash flow to influence when the crisis arrives. The engine derives low d (beneficiary + arbitrage) → negative f(d) → negative or very low chi. Treasury experiences the constraint as coordination, not extraction. Congressional Budget Authority is the primary victim with trapped exit options — it cannot exit the constraint because the debt limit mechanism is statutory, yet it cannot effectively oversee the timing because Treasury controls the cash flow information and extraordinary measures deployment. The engine derives high d (victim + trapped) → high f(d) → high chi. Congress experiences maximum extraction. Bond Market Participants are secondary victims with constrained exit options — they face high costs to reduce exposure but are not fully trapped. The engine derives moderate-high d (victim + constrained) → moderate f(d) → moderate chi. They experience both coordination (Treasury's liquidity management) and extraction (volatility from timing uncertainty). Fiscal Transparency Advocates are organized observers with constrained exit — they can advocate for reform but cannot unilaterally change the structure. The engine derives moderate d (mixed beneficiary/victim + constrained) → moderate f(d) → moderate chi. The Debt Limit Abolition Coalition has mobile exit options — they are building alternative frameworks and have agency to pursue structural change. The engine derives lower d (organized + mobile) → lower f(d) → lower chi. They see the constraint as temporary (scaffold). Congressional Appropriations Committees are institutional actors with constrained exit — they maintain the oversight ritual but have lost functional authority. The engine derives moderate d (victim + constrained) → moderate f(d), but the piton classification comes from the theater gate, not from high chi. The Analytical Observer has analytical exit and sees the full structure — the engine derives d from the structural data (beneficiaries + victims + enforcement) and computes tangled_rope from the metrics.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint resolves the mandatrophy by showing that the debt limit's original mandate (congressional control over borrowing to enforce fiscal discipline) has been substantially hollowed out by Treasury's control over arrival timing, yet the coordination function (forcing periodic review) persists. The mandate has not been fully abandoned (Congress still votes on the limit, and the votes do occasionally produce fiscal policy changes), but it has been degraded by the executive's timing control. The tangled_rope classification captures this: genuine coordination coexists with substantial extraction. The scaffold perspective (abolition coalition) represents one possible resolution: the constraint is temporary and will be dismantled through constitutional reinterpretation or statutory repeal. The piton perspective (appropriations committees) represents another: the constraint persists as degraded ritual maintained through institutional inertia. The analytical perspective integrates both: the constraint is currently a tangled rope (coordination + extraction), with a possible future state as either dissolved (scaffold sunset realized) or fully atrophied (piton endpoint).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    extraordinary_measures_discretion,
    'Does Treasury''s discretion in deploying extraordinary measures (which extends the time before limit arrival) constitute legitimate operational flexibility or extractive manipulation of congressional oversight timing?',
    'Historical analysis of extraordinary measures deployment patterns correlated with political composition of Congress and proximity to elections; comparison of Treasury''s stated cash management rationale vs observed timing outcomes',
    'If legitimate flexibility: coordination function dominates, extraction is lower. If extractive manipulation: Treasury is strategically controlling the crisis window to maximize executive leverage, extraction is higher.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraordinary_measures_discretion, empirical, 'Whether extraordinary measures deployment is operational necessity or strategic timing control').

omega_variable(
    cash_balance_target_ambiguity,
    'Is Treasury''s target cash balance (which affects how quickly the limit is reached) determined by genuine liquidity risk management or by strategic considerations about when to trigger the crisis?',
    'Econometric analysis of Treasury cash balance decisions controlling for observable liquidity risk factors (auction calendar, seasonal tax flows, market volatility); identification of residual variation correlated with political variables',
    'If liquidity-driven: the constraint is primarily coordination with incidental timing effects. If politically-driven: the constraint is substantially extractive, with Treasury using cash management as a timing lever.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(cash_balance_target_ambiguity, empirical, 'Whether cash balance targets are liquidity management or strategic timing tools').

omega_variable(
    fourteenth_amendment_nullification,
    'Does the 14th Amendment Section 4 (''validity of the public debt shall not be questioned'') constitutionally nullify the statutory debt limit, making the entire constraint legally void?',
    'Supreme Court adjudication of a debt limit challenge; executive branch legal opinion asserting constitutional authority to ignore the limit; scholarly consensus on constitutional interpretation',
    'If nullified: the constraint dissolves entirely (scaffold sunset realized immediately). If upheld: the statutory limit remains binding and the timing extraction persists.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(fourteenth_amendment_nullification, conceptual, 'Whether the debt limit is constitutionally void under 14th Amendment').

omega_variable(
    deficit_projection_accuracy,
    'How much of the uncertainty in limit arrival timing is due to genuine economic forecasting difficulty vs Treasury''s strategic withholding or selective disclosure of cash flow information?',
    'Comparison of Treasury''s internal cash flow forecasts (obtained via FOIA or congressional subpoena) with publicly disclosed projections; analysis of forecast error patterns for systematic bias',
    'If forecasting difficulty: the coordination story is stronger (Treasury is managing genuine uncertainty). If strategic disclosure: the extraction story is stronger (Treasury is manufacturing uncertainty to control the narrative).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(deficit_projection_accuracy, empirical, 'Whether arrival timing uncertainty is forecasting difficulty or strategic information control').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(limit_arrival_timing, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(limit_timing_theater_2011, limit_arrival_timing, theater_ratio, 0, 0.52).
narrative_ontology:measurement(limit_timing_theater_2013, limit_arrival_timing, theater_ratio, 2, 0.58).
narrative_ontology:measurement(limit_timing_theater_2015, limit_arrival_timing, theater_ratio, 4, 0.63).
narrative_ontology:measurement(limit_timing_theater_2017, limit_arrival_timing, theater_ratio, 6, 0.66).
narrative_ontology:measurement(limit_timing_theater_2019, limit_arrival_timing, theater_ratio, 8, 0.68).
narrative_ontology:measurement(limit_timing_theater_2021, limit_arrival_timing, theater_ratio, 10, 0.68).

% Extraction over time
narrative_ontology:measurement(limit_timing_extract_2011, limit_arrival_timing, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(limit_timing_extract_2013, limit_arrival_timing, base_extractiveness, 2, 0.42).
narrative_ontology:measurement(limit_timing_extract_2015, limit_arrival_timing, base_extractiveness, 4, 0.45).
narrative_ontology:measurement(limit_timing_extract_2017, limit_arrival_timing, base_extractiveness, 6, 0.47).
narrative_ontology:measurement(limit_timing_extract_2019, limit_arrival_timing, base_extractiveness, 8, 0.48).
narrative_ontology:measurement(limit_timing_extract_2021, limit_arrival_timing, base_extractiveness, 10, 0.48).

% Suppression requirement over time
narrative_ontology:measurement(limit_timing_suppress_2011, limit_arrival_timing, suppression_requirement, 0, 0.48).
narrative_ontology:measurement(limit_timing_suppress_2016, limit_arrival_timing, suppression_requirement, 5, 0.58).
narrative_ontology:measurement(limit_timing_suppress_2021, limit_arrival_timing, suppression_requirement, 10, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(limit_arrival_timing, enforcement_mechanism).

% DUAL FORMULATION NOTE:
% This constraint is downstream of four structural inputs: treasury_cash_discretion (rope — Treasury's operational cash management flexibility), iran_conflict_spending (snare — emergency spending that accelerates deficit accumulation), tariff_revenue_volatility (snare — revenue uncertainty that affects cash flow projections), and economic_condition_uncertainty (tangled_rope — macroeconomic forecasting difficulty that affects deficit projections). Each upstream constraint has its own extractiveness value reflecting its specific structural dynamics; this constraint's extractiveness reflects the career and institutional asymmetry created by Treasury's control over when the limit binds, given those upstream inputs.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
