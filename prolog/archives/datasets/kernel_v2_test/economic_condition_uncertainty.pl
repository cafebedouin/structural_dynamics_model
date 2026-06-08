% ============================================================================
% CONSTRAINT STORY: economic_condition_uncertainty
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2025-01-10
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_economic_condition_uncertainty, []).

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
    narrative_ontology:affects_constraint/2,
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
 *   constraint_id: economic_condition_uncertainty
 *   human_readable: Economic Condition Uncertainty in Public Finance
 *   domain: public_finance/constitutional_law/political_economy
 *
 * SUMMARY:
 *   Economic condition uncertainty in public finance creates a structural
 *   tension between the need for fiscal stability (predictable revenue and
 *   spending) and the reality of volatile economic conditions driven by
 *   energy prices, business cycles, and structural shocks. This constraint
 *   exhibits the tangled rope pattern: genuine coordination problems
 *   (governments need revenue forecasts to plan; automatic stabilizers smooth
 *   shocks) are layered with asymmetric extraction (discretionary budget
 *   controllers benefit from uncertainty; safety net populations bear the
 *   cost of procyclical cuts). The constraint's theater_ratio (0.58) reflects
 *   that economic forecasting has become substantially performative: CBO and
 *   OMB produce precise 10-year revenue projections despite systematic
 *   forecast errors, and the ritual of baseline scoring persists even as
 *   actual budget outcomes diverge wildly from projections. The 2008
 *   financial crisis, 2020 pandemic, and 2022 energy price shock all revealed
 *   the forecasting apparatus as partly theatrical — the models could not
 *   predict the shocks and could not quickly adapt to the new conditions. Yet
 *   the apparatus persists because it serves institutional functions beyond
 *   accuracy: it provides political cover for discretionary decisions and
 *   maintains the legitimacy of centralized budget control.
 *
 * KEY AGENTS:
 *   - Safety Net Dependent Populations: Primary victim (powerless/trapped) — face benefit uncertainty and potential cuts during economic downturns when need is highest; cannot exit fiscal jurisdiction or organize quickly enough to counter political pressure
 *   - Fiscal Stability (abstract): Primary victim (powerless/trapped) — the collective good of predictable public finance; no advocate, no exit option, bears full cost of volatility
 *   - Long-Term Budget Planners: Secondary victim (moderate/constrained) — state and federal budget offices that need credible multi-year projections but face structural revenue volatility; benefit from some coordination tools but bear extraction through impossible planning requirements
 *   - Discretionary Budget Controllers: Primary beneficiary (institutional/arbitrage) — OMB, Treasury, legislative appropriations committees that control spending allocations; benefit from uncertainty as justification for centralized authority and flexible appropriations
 *   - Countercyclical Policy Advocates: Organized agents (organized/mobile) — coalition building automatic stabilizers and indexed safety net programs; see uncertainty as temporary problem with structural solution (scaffold perspective)
 *   - Economic Forecasting Apparatus: Institutional actor (institutional/arbitrage) — CBO, OMB, private forecasters maintaining elaborate projection models; sees own function as partly degraded (piton perspective) but persists through statutory requirements and institutional inertia
 *   - State Fiscal Authorities: Inter-institutional victim (institutional/constrained) — state budget offices facing same volatility as federal but with stricter balanced budget constraints and less borrowing capacity; benefit from federal coordination but bear extraction through unfunded mandates
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(economic_condition_uncertainty, 0.48).
domain_priors:suppression_score(economic_condition_uncertainty, 0.62).
domain_priors:theater_ratio(economic_condition_uncertainty, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(economic_condition_uncertainty, extractiveness, 0.48).
narrative_ontology:constraint_metric(economic_condition_uncertainty, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(economic_condition_uncertainty, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(economic_condition_uncertainty, tangled_rope).
narrative_ontology:human_readable(economic_condition_uncertainty, "Economic Condition Uncertainty in Public Finance").
narrative_ontology:topic_domain(economic_condition_uncertainty, "public_finance/constitutional_law/political_economy").

domain_priors:requires_active_enforcement(economic_condition_uncertainty).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(economic_condition_uncertainty, countercyclical_policy_advocates).
narrative_ontology:constraint_beneficiary(economic_condition_uncertainty, discretionary_budget_controllers).
narrative_ontology:constraint_beneficiary(economic_condition_uncertainty, economic_forecasting_industry).
narrative_ontology:constraint_victim(economic_condition_uncertainty, fiscal_stability).
narrative_ontology:constraint_victim(economic_condition_uncertainty, safety_net_dependent_populations).
narrative_ontology:constraint_victim(economic_condition_uncertainty, long_term_budget_planners).
narrative_ontology:constraint_vindicates(economic_condition_uncertainty, keynesian_stabilization_doctrine).
narrative_ontology:constraint_vindicates(economic_condition_uncertainty, automatic_stabilizer_necessity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SAFETY NET DEPENDENT (SNARE) — Trapped by economic dependency with no exit options during volatility cycles. When energy-driven inflation spikes and revenue collections lag, safety net spending faces immediate political pressure despite rising need. Cannot exit the fiscal jurisdiction, cannot organize fast enough to counter budget cuts, bears maximum extraction through benefit uncertainty and potential reductions during the exact moments of greatest need.
constraint_indexing:constraint_classification(economic_condition_uncertainty, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: LONG-TERM BUDGET PLANNER (TANGLED ROPE) — Constrained by constitutional and statutory frameworks requiring balanced budgets or debt limits while facing revenue volatility. Benefits from the coordination function (automatic stabilizers smooth some shocks, forecasting models provide planning tools) but also bears extraction through the impossibility of credible long-term commitments when revenue streams are structurally volatile. Can partially exit through reserve funds and rainy-day accounts, but these are themselves constrained by political economy.
constraint_indexing:constraint_classification(economic_condition_uncertainty, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: DISCRETIONARY BUDGET CONTROLLER (ROPE) — Institutional actors (OMB, Treasury, legislative budget committees) who control discretionary spending allocations benefit from revenue volatility as a coordination mechanism. Uncertainty creates legitimate need for centralized forecasting, emergency authorities, and flexible appropriations. Can arbitrage across budget categories and time horizons. Experiences the constraint as coordination: volatility justifies the institutional apparatus they control.
constraint_indexing:constraint_classification(economic_condition_uncertainty, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: COUNTERCYCLICAL POLICY COALITION (SCAFFOLD) — Organized advocates for automatic stabilizers, unemployment insurance expansion, and indexed safety net programs see revenue volatility as a temporary coordination problem with a structural solution: better automatic stabilizers that respond to economic conditions without discretionary intervention. The sunset logic: as automatic mechanisms mature and gain political durability, the discretionary uncertainty extraction diminishes. Mobile because they can shift advocacy across jurisdictions and policy domains.
constraint_indexing:constraint_classification(economic_condition_uncertainty, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: STATE FISCAL AUTHORITY (TANGLED ROPE) — State-level budget offices face the same revenue volatility as federal but with stricter balanced budget requirements and less borrowing capacity. Benefits from federal coordination (grants, revenue sharing, disaster relief) but bears extraction through unfunded mandates and the procyclical fiscal trap: forced to cut spending exactly when economic conditions deteriorate. Constrained exit: can lobby for federal relief but cannot escape the constitutional fiscal framework.
constraint_indexing:constraint_classification(economic_condition_uncertainty, tangled_rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 6: ECONOMIC FORECASTING APPARATUS (PITON) — CBO, OMB, and private forecasting firms maintain elaborate revenue projection models whose accuracy has degraded substantially in the face of energy price shocks, pandemic disruptions, and structural economic shifts. The forecasting ritual persists through institutional inertia and statutory requirements (budget baseline projections, 10-year windows) despite systematic forecast errors. The apparatus sees its own function as partly theatrical: the precision of the models (revenue projections to the nearest billion) is performative given the actual volatility.
constraint_indexing:constraint_classification(economic_condition_uncertainty, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (TANGLED ROPE) — From a civilizational perspective, revenue volatility driven by energy prices and economic cycles is a genuine coordination problem (governments need stable revenue to provide public goods) layered with asymmetric extraction (discretionary budget controllers benefit from uncertainty; safety net populations bear the cost). The constraint requires active enforcement (balanced budget rules, debt limits, appropriations processes) to maintain the extraction mechanism. Pure coordination would allow full countercyclical flexibility; pure extraction would eliminate automatic stabilizers entirely. The actual system is hybrid.
constraint_indexing:constraint_classification(economic_condition_uncertainty, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(economic_condition_uncertainty_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(economic_condition_uncertainty, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(economic_condition_uncertainty, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(economic_condition_uncertainty, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(economic_condition_uncertainty, TR),
    TR >= 0.70.

:- end_tests(economic_condition_uncertainty_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.48): Moderate-high. Revenue volatility creates genuine coordination problems (governments need stable funding for public goods) but also enables asymmetric extraction. Discretionary budget controllers benefit from uncertainty through expanded authority and flexible appropriations. Safety net populations bear costs through benefit uncertainty and procyclical cuts. The extraction is substantial but not maximal because automatic stabilizers do provide some genuine smoothing, and the coordination function is real. The value increased from 0.35 (2000) to 0.48 (2025) as energy price volatility intensified, forecast errors accumulated, and political polarization made discretionary responses more extractive. Suppression (0.62): Moderate-high. Constitutional balanced budget requirements, statutory debt limits, and appropriations processes create significant barriers to countercyclical fiscal policy. Safety net populations cannot exit the fiscal jurisdiction and face high costs to organize politically. Long-term planners are constrained by statutory frameworks. The suppression increased from 0.50 (2000) to 0.62 (2020-2025) as balanced budget rules tightened at state level and federal debt limit crises became more frequent. Theater ratio (0.58): Moderate-high. Economic forecasting for budget baselines has become substantially performative. CBO produces revenue projections to the nearest billion for 10-year windows despite systematic forecast errors exceeding hundreds of billions. The precision is theatrical given actual volatility. OMB baseline scoring persists even as actual appropriations diverge wildly from projections. The theater increased from 0.40 (2000) to 0.60 (2022) as forecast errors accumulated through financial crisis, pandemic, and energy shocks, then declined slightly to 0.58 (2025) as some forecasting reforms were adopted.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates the tangled rope pattern across multiple perspectives. Safety net populations see pure extraction (snare) — volatility creates benefit uncertainty and procyclical cuts with no coordination benefit to them. Long-term budget planners see mixed coordination and extraction (tangled rope) — forecasting tools and automatic stabilizers provide some smoothing, but structural volatility makes credible planning impossible. Discretionary budget controllers see coordination (rope) — uncertainty justifies the institutional apparatus they control. The countercyclical policy coalition sees a temporary problem with a sunset (scaffold) — better automatic stabilizers can eliminate the discretionary extraction. State fiscal authorities see tangled rope from a different angle — federal coordination provides some benefit, but unfunded mandates and balanced budget constraints create extraction. The forecasting apparatus sees its own degraded function (piton) — the models persist through statutory requirements despite systematic errors. The analytical observer sees the hybrid structure: revenue volatility is a genuine coordination problem (governments need stable funding) layered with asymmetric extraction (discretionary controllers benefit, safety net populations bear costs).
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is derived from structural position. Safety net dependent populations are victims with trapped exit options — they experience maximum extraction (high d, high chi). Long-term budget planners are victims with constrained exit — they experience substantial extraction but have some agency through reserve funds and rainy-day accounts (moderate d, moderate chi). Discretionary budget controllers are beneficiaries with arbitrage exit — they experience the constraint as coordination or even subsidy (low d, low or negative chi). The countercyclical policy coalition is organized with mobile exit — they experience moderate extraction but have agency to shift advocacy (moderate d, moderate chi). State fiscal authorities are victims with constrained exit — they benefit from federal coordination but bear extraction through unfunded mandates and balanced budget constraints (moderate-high d, moderate-high chi). The economic forecasting apparatus is a beneficiary with arbitrage exit — they maintain institutional position regardless of forecast accuracy (low d, low chi). The analytical observer sees the hybrid structure: genuine coordination layered with asymmetric extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint resolves mandatrophy by identifying the genuine coordination function (automatic stabilizers, revenue forecasting for planning) while measuring the asymmetric extraction (discretionary authority concentration, procyclical safety net cuts). The coordination function is real: governments do need revenue projections to allocate resources, and automatic stabilizers do smooth some economic shocks. But the extraction is also real: discretionary budget controllers benefit from uncertainty, and safety net populations bear disproportionate costs during downturns. The tangled rope classification captures this hybrid structure. The scaffold perspective (countercyclical policy coalition) identifies a potential sunset: as automatic stabilizers mature and gain political durability, the discretionary extraction diminishes. The piton perspective (forecasting apparatus) identifies the degraded function: the precision of revenue projections is performative given actual volatility, but the ritual persists through institutional inertia. The analytical classification is tangled rope because both coordination and extraction are structurally present and neither can be eliminated without changing the constraint's fundamental character.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    automatic_stabilizer_sufficiency,
    'Are existing automatic stabilizers (progressive taxation, unemployment insurance, SNAP) sufficient to smooth economic volatility without discretionary intervention, or is the discretionary apparatus structurally necessary?',
    'Comparative analysis of fiscal volatility in jurisdictions with strong vs weak automatic stabilizers; counterfactual modeling of revenue/spending paths under pure automatic vs pure discretionary regimes',
    'If sufficient: scaffold perspective confirmed — discretionary extraction is eliminable through better design. If insufficient: tangled rope is structural — some discretionary authority is coordination, not just extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(automatic_stabilizer_sufficiency, empirical, 'Whether automatic stabilizers can replace discretionary fiscal intervention').

omega_variable(
    energy_price_exogeneity,
    'Is energy price volatility an exogenous shock (mountain) or partly endogenous to fiscal and monetary policy choices (constructed constraint)?',
    'Causal analysis of energy price drivers: geopolitical supply shocks vs domestic policy (strategic petroleum reserve releases, renewable subsidies, carbon pricing); correlation between fiscal/monetary stance and energy price paths',
    'If exogenous: revenue volatility is partly mountain (unavoidable response to external shock). If endogenous: revenue volatility is more extractive than claimed (policy choices create the instability that justifies discretionary control).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(energy_price_exogeneity, empirical, 'Whether energy price volatility is exogenous or policy-driven').

omega_variable(
    balanced_budget_necessity,
    'Do constitutional and statutory balanced budget requirements reflect genuine fiscal sustainability constraints (mountain) or political economy extraction mechanisms (snare)?',
    'Cross-national comparison of fiscal outcomes in jurisdictions with vs without balanced budget rules; analysis of sovereign borrowing costs and default risk as function of deficit levels; historical case studies of fiscal crises',
    'If necessary: suppression is coordination (preventing fiscal crisis). If extractive: suppression is a political choice that amplifies volatility and concentrates discretionary power.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(balanced_budget_necessity, conceptual, 'Whether balanced budget rules are fiscal necessity or political extraction').

omega_variable(
    forecast_error_attribution,
    'Are systematic forecast errors in revenue projections due to irreducible economic uncertainty (mountain) or to model choices that serve institutional interests (piton)?',
    'Decomposition of forecast errors into exogenous shocks vs structural model bias; comparison of official forecasts (CBO, OMB) vs independent forecasts; analysis of forecast error patterns (directional bias, volatility underestimation)',
    'If irreducible: forecasting apparatus is coordination despite degradation. If institutional: forecasting apparatus is theater maintaining discretionary authority through constructed uncertainty.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(forecast_error_attribution, empirical, 'Whether forecast errors are irreducible or institutionally motivated').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(economic_condition_uncertainty, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(econ_uncert_theater_2000, economic_condition_uncertainty, theater_ratio, 0, 0.4).
narrative_ontology:measurement(econ_uncert_theater_2008, economic_condition_uncertainty, theater_ratio, 8, 0.48).
narrative_ontology:measurement(econ_uncert_theater_2012, economic_condition_uncertainty, theater_ratio, 12, 0.52).
narrative_ontology:measurement(econ_uncert_theater_2020, economic_condition_uncertainty, theater_ratio, 20, 0.55).
narrative_ontology:measurement(econ_uncert_theater_2022, economic_condition_uncertainty, theater_ratio, 22, 0.6).
narrative_ontology:measurement(econ_uncert_theater_2025, economic_condition_uncertainty, theater_ratio, 25, 0.58).

% Extraction over time
narrative_ontology:measurement(econ_uncert_extract_2000, economic_condition_uncertainty, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(econ_uncert_extract_2008, economic_condition_uncertainty, base_extractiveness, 8, 0.42).
narrative_ontology:measurement(econ_uncert_extract_2012, economic_condition_uncertainty, base_extractiveness, 12, 0.38).
narrative_ontology:measurement(econ_uncert_extract_2020, economic_condition_uncertainty, base_extractiveness, 20, 0.45).
narrative_ontology:measurement(econ_uncert_extract_2022, economic_condition_uncertainty, base_extractiveness, 22, 0.48).
narrative_ontology:measurement(econ_uncert_extract_2025, economic_condition_uncertainty, base_extractiveness, 25, 0.48).

% Suppression requirement over time
narrative_ontology:measurement(econ_uncert_suppress_2000, economic_condition_uncertainty, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(econ_uncert_suppress_2008, economic_condition_uncertainty, suppression_requirement, 8, 0.58).
narrative_ontology:measurement(econ_uncert_suppress_2012, economic_condition_uncertainty, suppression_requirement, 12, 0.6).
narrative_ontology:measurement(econ_uncert_suppress_2020, economic_condition_uncertainty, suppression_requirement, 20, 0.62).
narrative_ontology:measurement(econ_uncert_suppress_2025, economic_condition_uncertainty, suppression_requirement, 25, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(economic_condition_uncertainty, resource_allocation).
narrative_ontology:affects_constraint(economic_condition_uncertainty, balanced_budget_amendment_proposals).
narrative_ontology:affects_constraint(economic_condition_uncertainty, automatic_stabilizer_design).
narrative_ontology:affects_constraint(economic_condition_uncertainty, federal_state_fiscal_relations).

% DUAL FORMULATION NOTE:
% Economic condition uncertainty is a family of related constraints: revenue volatility (this story), expenditure volatility (safety net demand fluctuations), and forecasting uncertainty (model error accumulation). Each has its own extractiveness reflecting different structural mechanisms. This story focuses on the revenue/spending volatility driven by energy prices and economic cycles; separate stories would be needed for pure forecasting error (piton) and pure safety net demand volatility (which may be more rope-like if automatic stabilizers function well).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
