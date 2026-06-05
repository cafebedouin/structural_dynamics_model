% ============================================================================
% CONSTRAINT STORY: sotu_1977_ford_federal_spending_restraint
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sotu_1977_ford_federal_spending_restraint, []).

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
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: sotu_1977_ford_federal_spending_restraint
 *   human_readable: Federal Spending Restraint and Tax Reduction Policy (1977 Ford Administration)
 *   domain: economics/fiscal_policy
 *
 * SUMMARY:
 *   The federal spending restraint and tax reduction policy announced in the
 *   1977 Ford State of the Union addresses the perceived macroeconomic crisis
 *   of stagflation (simultaneous high inflation and economic stagnation). The
 *   constraint operates by limiting federal budgetary expansion while cutting
 *   income taxes, intending to reduce aggregate demand pressure on prices and
 *   to return resource allocation decisions from Washington to state/local
 *   and private actors. This is a canonical tangled_rope constraint: it
 *   coordinates the fiscal policy stance across federal agencies
 *   (establishing clear spending ceilings) and solves the problem of how to
 *   distribute authority between federal and subnational governments, while
 *   simultaneously extracting from federal program beneficiaries and
 *   state/local governments dependent on federal transfers. The
 *   extractiveness rises from 0.38 to 0.52 over the measurement interval as
 *   spending caps bind and program beneficiaries bear cumulative costs. The
 *   theater ratio also rises from 0.32 to 0.48, reflecting that restraint
 *   relies increasingly on political rhetoric about fiscal discipline and
 *   inflation control rather than on effective mechanisms — supplemental
 *   appropriations, off-budget spending, and creative accounting emerge as
 *   spending pressure persists.
 *
 * KEY AGENTS:
 *   - Taxpayer Constituencies and Private Enterprise Sector: Primary beneficiary (institutional/arbitrage) — capture tax cuts and reduced federal competition for capital; experience lowest effective extraction
 *   - Federal Program Beneficiaries: Primary victim (powerless/trapped) — lose access to federal transfers, services, and purchasing power as real benefits decline; no exit option
 *   - Federal Workforce and Agencies: Secondary victim (moderate/constrained) — face hiring freezes and budget caps; constrained exit through career costs; also benefit from budgetary clarity
 *   - State and Local Governments: Mixed victim/agent (moderate/constrained) — bear downward pressure on federal transfers and unfunded mandates but gain autonomy from reduced federal control; constrained exit through local capacity limitations
 *   - State Reform Coalitions: Organized agents (organized/mobile) — governors and state leaders see restraint as temporary devolution mechanism with sunset; have mobile exit options as political coalitions shift
 *   - The Federal Budget Process: Institutional actor (institutional/arbitrage) — maintains appearance of restraint through rhetoric; actual functional discipline degrades as theater ratio rises
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sotu_1977_ford_federal_spending_restraint, 0.52).
domain_priors:suppression_score(sotu_1977_ford_federal_spending_restraint, 0.58).
domain_priors:theater_ratio(sotu_1977_ford_federal_spending_restraint, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sotu_1977_ford_federal_spending_restraint, extractiveness, 0.52).
narrative_ontology:constraint_metric(sotu_1977_ford_federal_spending_restraint, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(sotu_1977_ford_federal_spending_restraint, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sotu_1977_ford_federal_spending_restraint, tangled_rope).
narrative_ontology:human_readable(sotu_1977_ford_federal_spending_restraint, "Federal Spending Restraint and Tax Reduction Policy (1977 Ford Administration)").
narrative_ontology:topic_domain(sotu_1977_ford_federal_spending_restraint, "economics/fiscal_policy").

domain_priors:requires_active_enforcement(sotu_1977_ford_federal_spending_restraint).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sotu_1977_ford_federal_spending_restraint, taxpayer_constituencies).
narrative_ontology:constraint_beneficiary(sotu_1977_ford_federal_spending_restraint, inflation_averse_middle_class).
narrative_ontology:constraint_beneficiary(sotu_1977_ford_federal_spending_restraint, private_enterprise_sector).
narrative_ontology:constraint_victim(sotu_1977_ford_federal_spending_restraint, federal_program_beneficiaries).
narrative_ontology:constraint_victim(sotu_1977_ford_federal_spending_restraint, federal_workforce).
narrative_ontology:constraint_victim(sotu_1977_ford_federal_spending_restraint, state_local_governments_dependent_on_federal_transfers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: FEDERAL PROGRAM BENEFICIARIES (SNARE) — Trapped by dependence on federal transfers and services with no alternative source. Restraint policy directly reduces their access to healthcare, nutrition assistance, housing, and infrastructure investment. No meaningful exit option: cannot replace federal programs with private alternatives at comparable cost. Bears maximum extraction.
constraint_indexing:constraint_classification(sotu_1977_ford_federal_spending_restraint, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: FEDERAL WORKFORCE AND AGENCIES (TANGLED ROPE) — Face hiring freezes, budget caps, and service reduction mandates. Constrained exit: can seek private sector employment but at career and pension cost. Also benefit from coordination function: the restraint mechanism does establish clear budgetary rules that reduce uncertainty compared to stop-gap funding cycles. Mixed extraction and coordination.
constraint_indexing:constraint_classification(sotu_1977_ford_federal_spending_restraint, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: STATE AND LOCAL GOVERNMENTS (TANGLED ROPE) — Federal spending restraint shifts costs and responsibilities downward (devolution). Constrained exit: must raise local taxes or cut services. Also benefit from coordination function: reduced federal mandates paired with reduced transfers provide budget clarity and local autonomy. The policy solves a coordination problem (who decides spending levels?) while extracting from those dependent on federal support.
constraint_indexing:constraint_classification(sotu_1977_ford_federal_spending_restraint, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: TAXPAYER CONSTITUENCIES AND PRIVATE ENTERPRISE (ROPE) — Primary beneficiaries. Tax cuts increase disposable income and capital availability for private investment. Reduced federal spending frees market mechanisms to allocate resources. Experience the constraint as coordination: enables private enterprise expansion and consumer choice. Net extraction runs toward these agents — they capture the benefit.
constraint_indexing:constraint_classification(sotu_1977_ford_federal_spending_restraint, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: STATE REFORM COALITIONS (SCAFFOLD) — Organized state and local leaders see the spending restraint as temporary coordination mechanism with sunset logic: federal spending is high due to inflation and stagflation crisis; as inflation moderates, spending pressure naturally eases. The devolution of authority back to states is viewed as temporary restructuring toward sustainable federalism. Mobile exit: states can adjust policies independent of federal constraints as crisis passes.
constraint_indexing:constraint_classification(sotu_1977_ford_federal_spending_restraint, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 6: THE FEDERAL BUDGET PROCESS (PITON) — The restraint mechanism persists through electoral cycles and political transitions, but its functional content has degraded: spending caps are bypassed through off-budget mechanisms, supplemental appropriations, and creative accounting. Theater ratio reflects that the 'discipline' is largely performative — the actual constraint operates through political rhetoric (deficit anxiety) rather than through institutional enforcement. The process is maintained by inertia, not by effective restraint.
constraint_indexing:constraint_classification(sotu_1977_ford_federal_spending_restraint, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / MACRO NATURAL LAW VIEW (MOUNTAIN) — From a civilizational/global perspective, spending restraint is presented as an immutable macroeconomic principle: inflation requires demand reduction; demand reduction requires fiscal tightness; fiscal tightness requires spending cuts. This naturalizes what is actually a contested policy choice among competing macroeconomic theories. The engine's false summit detector will identify this as naturalization of a contingent institutional arrangement with clear beneficiaries.
constraint_indexing:constraint_classification(sotu_1977_ford_federal_spending_restraint, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sotu_1977_ford_federal_spending_restraint_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(sotu_1977_ford_federal_spending_restraint, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(sotu_1977_ford_federal_spending_restraint, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(sotu_1977_ford_federal_spending_restraint, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(sotu_1977_ford_federal_spending_restraint, TR),
    TR >= 0.70.

:- end_tests(sotu_1977_ford_federal_spending_restraint_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The policy creates clear winners (taxpayers, private enterprise) and losers (program beneficiaries, federal employees, state/local governments dependent on federal revenue). The extraction is not maximal because: (1) the coordination function is genuine — spending restraint does establish fiscal rules that reduce uncertainty for budget planning, and (2) some program beneficiaries retain baseline services even under restraint. However, the extractiveness rises from 0.38 to 0.52 over the interval as cumulative effects compound and political resistance to cuts generates off-budget workarounds that hide rather than eliminate spending (inflation erosion of real benefits). Suppression (0.58): Moderate-high. Federal program beneficiaries face significant barriers to exit or alternative service provision. State governments dependent on federal transfers cannot easily replace that revenue. Federal employees cannot organize effectively against hiring freezes due to civil service restrictions. However, suppression is not total (0.70+) because: (1) states can raise local taxes, (2) private charities and non-profits provide some alternative services, (3) some federal workers can migrate to private sector. Theater ratio (0.48): Moderate-increasing. The restraint mechanism relies on political commitment and public rhetoric about fiscal discipline more than on institutional enforcement. The theater rises during the interval as actual spending discipline weakens (hidden spending, supplemental appropriations, off-budget mechanisms) while political rhetoric about restraint intensifies.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates divergent classification across power levels. Institutional beneficiaries (taxpayers, private sector) see coordination (Rope) — restraint enables market mechanisms. Moderate actors see mixed effects (Tangled Rope) — states gain autonomy but lose transfers. Powerless actors see pure extraction (Snare) — program beneficiaries lose services with no alternative. Organized reformers see temporary restructuring (Scaffold) — devolution is viewed as transitional federalism reset. The institutional budget process itself sees its function degraded (Piton) — restraint operates through rhetoric more than enforcement. The analytical observer risks naturalizing a contested policy choice as inevitable economic law (Mountain). The perspectival gaps reveal that this constraint is primarily about power distribution and ideology, not about immutable economic constraints.
 *
 * DIRECTIONALITY LOGIC:
 *   Each agent's experienced extractiveness (χ) is computed from base extractiveness (0.52), their directionality (d), and scope modifier (national = 1.0). Taxpayers with arbitrage exit and beneficiary status get d ≈ 0.12, producing χ ≈ 0.52 × 0.65 × 1.0 ≈ 0.34 (moderate extraction directed toward them — net benefit). Federal program beneficiaries with trapped exit and victim status get d ≈ 0.95, producing χ ≈ 0.52 × 1.42 × 1.0 ≈ 0.74 (severe extraction directed away from them — maximum burden). States with constrained exit get d ≈ 0.70, producing χ ≈ 0.52 × 1.05 × 1.0 ≈ 0.55 (moderate extraction). The beneficiary/victim asymmetry drives the tangled_rope classification rather than rope classification: the coordination function (fiscal rules, federalism clarity) is real and enables some agents to plan, but it is paired with asymmetric extraction from program beneficiaries who bear costs without receiving coordination benefits.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by the tangled_rope classification, which explicitly models the hybrid structure: genuine coordination function (fiscal rule-setting, federalism recalibration) paired with asymmetric extraction (program beneficiaries bear costs without coordination benefit). The alternative misclassifications would be: (1) pure rope (ignoring the extraction of program beneficiaries — false because beneficiaries lose access with no benefit); (2) pure snare (ignoring the coordination benefit of fiscal clarity — false because federal agencies and state planners do benefit from spending rules); (3) pure mountain (naturalizing restraint as economic necessity — false because the omega variables reveal competing macroeconomic theories and contested empirical claims). The tangled_rope captures the actual structure: policy solves a coordination problem while concentrating extraction on the least-capable exit actors (trapped program beneficiaries).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    inflation_causal_mechanism,
    'Does federal spending restraint actually reduce inflation, or is inflation driven by monetary policy and external shocks (OPEC, commodity prices)?',
    'Econometric analysis: compare inflation trajectories in countries with similar monetary policy but different fiscal stances; isolate federal spending variance from money supply variance using VAR models',
    'If federal spending is primary driver: policy achieves stated coordination goal (demand-destruction brake) and classification remains Tangled Rope. If monetary policy dominates: spending restraint is extraction without functional benefit and reclassifies toward Snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(inflation_causal_mechanism, empirical, 'Whether federal spending restraint causally reduces inflation').

omega_variable(
    devolution_versus_abandonment,
    'Is the downward shift of authority to states genuine federalism reform or de facto abandonment of federal responsibilities?',
    'Post-restraint analysis: did states maintain service levels by raising local taxes, or did service provision collapse? Did federal mandates decrease or merely defund existing obligations?',
    'If genuine federalism reform: state/local governments benefit from clarity and autonomy — Scaffold classification is structural. If abandonment: services collapse in low-capacity states and devolution is extraction mechanism — reclassify toward Snare for poorest populations.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(devolution_versus_abandonment, empirical, 'Whether spending restraint represents federalism reform or abandonment').

omega_variable(
    distributional_incidence_collapse,
    'Does tax reduction primarily benefit high-income earners through capital gains treatment and rate cuts, while spending cuts disproportionately affect low-income program beneficiaries?',
    'Incidence analysis: compare effective tax rates for top 10% vs bottom 50% before and after; compare benefit reductions by program and beneficiary income distribution',
    'If incidence is regressive: policy is pure extraction from poor to rich, suppression rises above 0.70, and snare classification dominates — mandatrophy reclassification to pure extraction. If progressive: coordination interpretation holds.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(distributional_incidence_collapse, empirical, 'Whether spending restraint distributes costs regressively').

omega_variable(
    crowding_out_versus_stimulus,
    'Do tax cuts stimulate private investment and consumption (crowding-out narrative) or simply reduce aggregate demand (Keynesian narrative)?',
    'Time-series analysis: investment and consumption trajectories post-tax cut; compare against counterfactual (no tax cut) using synthetic control methods',
    'If stimulus works: policy achieves coordination goal (private expansion offsets public contraction). If demand falls: policy is pure extraction from dependent populations for ideological reasons — reclassify toward Snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(crowding_out_versus_stimulus, empirical, 'Whether tax cuts stimulate private investment or reduce aggregate demand').

omega_variable(
    hidden_extraction_through_inflation,
    'Does restraint policy reduce nominal spending but allow inflation to erode program beneficiary purchasing power, creating hidden extraction through currency debasement?',
    'Real (inflation-adjusted) vs nominal analysis: track real federal benefits trajectory; compare nominal restraint against inflation rate during interval',
    'If real benefits fall: suppression and extraction are higher than nominal measures suggest. If real benefits maintained: policy achieves stated goals.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(hidden_extraction_through_inflation, empirical, 'Whether inflation allows extraction while nominal spending appears controlled').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sotu_1977_ford_federal_spending_restraint, 0, 4).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ford_spend_tr_t0, sotu_1977_ford_federal_spending_restraint, theater_ratio, 0, 0.32).
narrative_ontology:measurement(ford_spend_tr_t2, sotu_1977_ford_federal_spending_restraint, theater_ratio, 2, 0.4).
narrative_ontology:measurement(ford_spend_tr_t4, sotu_1977_ford_federal_spending_restraint, theater_ratio, 4, 0.48).

% Extraction over time
narrative_ontology:measurement(ford_spend_be_t0, sotu_1977_ford_federal_spending_restraint, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(ford_spend_be_t2, sotu_1977_ford_federal_spending_restraint, base_extractiveness, 2, 0.45).
narrative_ontology:measurement(ford_spend_be_t4, sotu_1977_ford_federal_spending_restraint, base_extractiveness, 4, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sotu_1977_ford_federal_spending_restraint, resource_allocation).
narrative_ontology:affects_constraint(sotu_1977_ford_federal_spending_restraint, reagan_era_federal_devolution).
narrative_ontology:affects_constraint(sotu_1977_ford_federal_spending_restraint, welfare_reform_1996_work_requirements).

% DUAL FORMULATION NOTE:
% Federal spending restraint is the upstream constraint enabling devolution of authority to states and privatization of service provision. Later welfare reform (1996) inherits the restraint logic but operationalizes it through explicit time limits and work requirements. The three constraints form a 20-year trajectory from general fiscal restraint (1977) to workfare devolution (1990s).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
