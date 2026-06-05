% ============================================================================
% CONSTRAINT STORY: institutional_inertia_assumption
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2025-01-02
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_institutional_inertia_assumption, []).

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
 *   constraint_id: institutional_inertia_assumption
 *   human_readable: Institutional Inertia Assumption in AI Labor Displacement Scenarios
 *   domain: macroeconomics/labor_economics/financial_systems
 *
 * SUMMARY:
 *   The institutional inertia assumption is a structural constraint embedded
 *   in many AI labor displacement scenarios. It holds that as AI systems
 *   automate labor, policy institutions (central banks, fiscal authorities,
 *   regulatory bodies) remain passive observers rather than active
 *   responders. This assumption enables clean scenario logic by isolating the
 *   AI capability variable, but it suppresses the historical pattern of
 *   institutional crisis response. The constraint exhibits tangled rope
 *   characteristics: it serves a legitimate coordination function
 *   (simplifying complex forecasts) while extracting from policy institutions
 *   and displaced workers by foreclosing countervailing mechanisms (UBI, job
 *   guarantees, wealth redistribution). The assumption's extractiveness has
 *   increased over the 2020-2026 interval as AI capabilities have accelerated
 *   but scenario models have not incorporated adaptive policy responses,
 *   creating a growing gap between model assumptions and institutional
 *   reality. Theater ratio reflects the performative aspect of policy debate:
 *   much discussion of AI displacement risks occurs without serious
 *   engagement with fiscal/monetary response capacity.
 *
 * KEY AGENTS:
 *   - Displaced Workers: Primary victim (powerless/trapped) — wages vanish in scenario with no modeled income recirculation; cannot exit institutional framework
 *   - Policy Institutions: Primary victim (moderate/constrained) — modeled as passive despite historical pattern of crisis response; constrained by scenario assumptions that foreclose adaptation
 *   - Scenario Authors: Primary beneficiary (institutional/arbitrage) — assumption simplifies forecasting and produces attention-grabbing results; can revise if challenged
 *   - Labor Unions and Advocacy Groups: Secondary victim (organized/mobile) — scenario forecloses policy mechanisms labor would advocate for, but organized groups can shift strategies
 *   - Capital Owners: Mixed position (powerful/mobile) — benefit from automation coordination but face demand collapse risk if no income recirculation
 *   - Policy Reform Coalitions: Organized agents (organized/constrained) — see assumption as temporary analytical device with sunset as political pressure forces adaptation
 *   - Analytical Observer: Civilizational view (analytical/analytical) — sees both coordination function and extraction; measures gap between scenario assumptions and institutional capacity
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(institutional_inertia_assumption, 0.48).
domain_priors:suppression_score(institutional_inertia_assumption, 0.52).
domain_priors:theater_ratio(institutional_inertia_assumption, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(institutional_inertia_assumption, extractiveness, 0.48).
narrative_ontology:constraint_metric(institutional_inertia_assumption, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(institutional_inertia_assumption, theater_ratio, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(institutional_inertia_assumption, tangled_rope).
narrative_ontology:human_readable(institutional_inertia_assumption, "Institutional Inertia Assumption in AI Labor Displacement Scenarios").
narrative_ontology:topic_domain(institutional_inertia_assumption, "macroeconomics/labor_economics/financial_systems").

domain_priors:requires_active_enforcement(institutional_inertia_assumption).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(institutional_inertia_assumption, scenario_authors).
narrative_ontology:constraint_beneficiary(institutional_inertia_assumption, ai_capability_forecasters).
narrative_ontology:constraint_beneficiary(institutional_inertia_assumption, automation_advocates).
narrative_ontology:constraint_victim(institutional_inertia_assumption, policy_institutions).
narrative_ontology:constraint_victim(institutional_inertia_assumption, displaced_workers).
narrative_ontology:constraint_victim(institutional_inertia_assumption, fiscal_sustainability).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DISPLACED WORKERS (TANGLED ROPE) — Workers whose wages vanish in the scenario have no exit from the institutional framework and no voice in whether countervailing mechanisms emerge. The assumption coordinates discourse around automation risk (genuine concern) but extracts by foreclosing policy responses that would recirculate income. d≈0.92, f(d)≈1.38, σ=1.0 → χ≈0.66.
constraint_indexing:constraint_classification(institutional_inertia_assumption, tangled_rope,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: POLICY INSTITUTIONS (TANGLED ROPE) — Central banks, fiscal authorities, and regulatory bodies are modeled as passive observers of tax base erosion and demand collapse. The assumption coordinates scenario logic (simplifies forecasting) but suppresses their historical pattern of crisis response (New Deal, WWII mobilization, 2008 interventions, COVID fiscal expansion). Constrained exit because institutions can adapt but the scenario forecloses this. d≈0.78, f(d)≈1.12, σ=1.0 → χ≈0.54.
constraint_indexing:constraint_classification(institutional_inertia_assumption, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: LABOR UNIONS AND ADVOCACY GROUPS (TANGLED ROPE) — Organized labor sees both coordination (the scenario highlights real risks requiring collective response) and extraction (the passive institution assumption forecloses the policy mechanisms labor would advocate for: UBI, job guarantees, wealth taxes). Mobile exit because organized groups can shift advocacy strategies. d≈0.58, f(d)≈0.82, σ=1.2 → χ≈0.47.
constraint_indexing:constraint_classification(institutional_inertia_assumption, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 4: SCENARIO AUTHORS (ROPE) — Authors benefit from the assumption's simplifying power: holding institutions constant isolates the AI capability variable and produces stark, attention-grabbing forecasts. The constraint coordinates discourse around automation risk. Arbitrage exit because authors can revise assumptions if challenged. d≈0.12, f(d)≈-0.04, σ=1.2 → χ≈-0.02. Negative effective extraction = net beneficiary.
constraint_indexing:constraint_classification(institutional_inertia_assumption, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: CAPITAL OWNERS (TANGLED ROPE) — Capital owners benefit from automation but also depend on aggregate demand. The passive institution assumption coordinates investment (automation looks unambiguously profitable) but extracts via demand collapse risk (no consumer base if wages vanish). Mobile exit because capital can shift sectors or geographies. d≈0.52, f(d)≈0.68, σ=1.1 → χ≈0.36.
constraint_indexing:constraint_classification(institutional_inertia_assumption, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(continental))).

% PERSPECTIVE 6: POLICY REFORM COALITIONS (SCAFFOLD) — Groups advocating for UBI, sovereign wealth funds, robot taxes, or job guarantees see the passive institution assumption as a temporary analytical device that will be superseded by actual policy response. The assumption has a sunset: as displacement accelerates, political pressure forces institutional adaptation. Constrained exit because reform requires political coalition-building. d≈0.45, f(d)≈0.52, σ=1.2 → χ≈0.30.
constraint_indexing:constraint_classification(institutional_inertia_assumption, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (TANGLED ROPE) — The assumption serves a legitimate analytical function (isolating variables) but also extracts by naturalizing institutional passivity. Historical precedent shows institutions adapt to crises, yet the scenario treats adaptation rate as exogenous. The coordination function (clear scenario logic) coexists with extraction (suppressed policy counterfactuals). d≈0.68, f(d)≈1.03, σ=1.2 → χ≈0.59.
constraint_indexing:constraint_classification(institutional_inertia_assumption, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(institutional_inertia_assumption_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(institutional_inertia_assumption, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(institutional_inertia_assumption, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(institutional_inertia_assumption, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(institutional_inertia_assumption_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.48): Moderate. The assumption extracts from policy institutions by modeling them as passive when historical precedent shows active crisis response. It extracts from displaced workers by foreclosing income recirculation mechanisms. But extraction is not high because the assumption serves a legitimate analytical function (isolating variables) and coordinates discourse around genuine automation risks. The coordination benefit is substantial enough to pull extractiveness below the snare threshold. Suppression (0.52): Moderate. The assumption suppresses policy counterfactuals (UBI, job guarantees, wealth taxes, sovereign AI ownership) and institutional adaptation capacity. Historical examples of rapid policy response (New Deal, WWII mobilization, 2008 interventions, COVID fiscal expansion) are treated as irrelevant to AI displacement scenarios. But suppression is not high because reform coalitions can advocate for alternative framings and the assumption is revisable. Theater ratio (0.45): Moderate. Policy debate about AI displacement often proceeds without serious fiscal modeling of response capacity or political economy analysis of implementation barriers. The debate is partly performative (acknowledging risk without engaging mechanisms) but also partly functional (identifying genuine coordination problems).
 *
 * PERSPECTIVAL GAP:
 *   Displaced workers and policy institutions see tangled rope: the assumption coordinates discourse around real automation risks but extracts by foreclosing their structural capacity to respond. Labor unions see tangled rope: the scenario highlights real risks (coordination) but suppresses policy mechanisms labor would advocate for (extraction). Scenario authors see rope: the assumption coordinates discourse and simplifies forecasting with minimal perceived cost. Capital owners see tangled rope: automation looks profitable (coordination) but demand collapse risk emerges (extraction). Reform coalitions see scaffold: the assumption is temporary, with a sunset as political pressure forces adaptation. The analytical observer sees tangled rope: legitimate analytical function coexists with suppressed policy counterfactuals. The perspectival gap reveals that the assumption's classification depends on whether you experience it as a simplifying device (beneficiary view) or as a foreclosed response capacity (victim view), but the coordination function is visible from all perspectives.
 *
 * DIRECTIONALITY LOGIC:
 *   Displaced workers: Victim + trapped → d≈0.92, f(d)≈1.38. High extraction because workers cannot exit and scenario forecloses income recirculation, but coordination function (highlighting automation risk) is present. Policy institutions: Victim + constrained → d≈0.78, f(d)≈1.12. Moderate-high extraction because institutions are modeled as passive despite historical adaptation capacity, but scenario serves analytical purpose. Labor unions: Victim + mobile → d≈0.58, f(d)≈0.82. Moderate extraction because organized groups can shift advocacy strategies and see coordination benefit. Scenario authors: Beneficiary + arbitrage → d≈0.12, f(d)≈-0.04. Net beneficiary because assumption simplifies forecasting and produces attention-grabbing results. Capital owners: Both + mobile → d≈0.52, f(d)≈0.68. Mixed position because automation benefits coexist with demand collapse risk. Reform coalitions: Beneficiary (scenario highlights need for reform) + constrained → d≈0.45, f(d)≈0.52. Low-moderate extraction because coalitions see sunset path. Analytical observer: analytical → d≈0.68, f(d)≈1.03. Moderate extraction because suppressed policy counterfactuals are visible from civilizational perspective, but coordination function is substantial.
 *
 * MANDATROPHY ANALYSIS:
 *   The institutional inertia assumption resolves mandatrophy by revealing that the classification depends on the observer's structural relationship to policy institutions. From the scenario author's perspective, the assumption is a legitimate simplifying device (rope): it isolates the AI capability variable and enables clear forecasting. From the policy institution's and displaced worker's perspectives, the assumption is a tangled rope: it coordinates discourse around automation risk (genuine benefit) while extracting by modeling institutions as passive when they have historical capacity for crisis response. From the reform coalition's perspective, the assumption is temporary (scaffold): political pressure will force adaptation as displacement accelerates. The tangled rope classification at the analytical level captures that the assumption serves both functions simultaneously: it coordinates discourse around automation risk (genuine coordination benefit) while suppressing policy counterfactuals (asymmetric extraction from institutions and workers). The mandatrophy is not 'is this assumption valid?' but 'whose structural position determines validity?' The presheaf over observation sites shows that all three readings (rope, tangled_rope, scaffold) are legitimate perspectival classifications of the same structural constraint.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    adaptation_rate_threshold,
    'At what rate of labor share decline do political institutions shift from passive observation to active intervention?',
    'Historical analysis of fiscal/monetary response to unemployment shocks; identification of threshold unemployment rates or GDP decline rates that trigger major policy shifts (e.g., New Deal at 25% unemployment, 2008 interventions at 10%, COVID response at 15%)',
    'If threshold is low (5-10% unemployment): institutional inertia assumption is empirically false for plausible AI timelines. If threshold is high (20-30%): assumption holds for longer displacement trajectories.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(adaptation_rate_threshold, empirical, 'Unemployment threshold triggering institutional policy response').

omega_variable(
    recirculation_mechanism_sufficiency,
    'Do proposed income recirculation mechanisms (UBI, job guarantees, sovereign wealth funds) scale to offset 40-60% labor share collapse?',
    'Fiscal modeling of revenue sources (wealth taxes, land value taxes, robot taxes, sovereign AI ownership) vs required transfer magnitudes; political economy analysis of implementation barriers',
    'If mechanisms scale: institutional inertia is a choice, not a constraint (Snare from more perspectives). If mechanisms fail to scale: inertia reflects genuine coordination failure (Rope from more perspectives).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(recirculation_mechanism_sufficiency, empirical, 'Whether policy mechanisms can offset large-scale labor displacement').

omega_variable(
    scenario_framing_effect,
    'Does the passive institution assumption function as a self-fulfilling prophecy by coordinating expectations around policy inaction?',
    'Discourse analysis of how AI displacement scenarios influence policy debate; comparison of policy response in jurisdictions exposed to different scenario framings',
    'If self-fulfilling: the assumption is extractive (creates the passivity it assumes). If not: the assumption is descriptive (reflects genuine institutional constraints).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(scenario_framing_effect, conceptual, 'Whether scenario assumptions influence policy response').

omega_variable(
    ai_capability_timeline_uncertainty,
    'Does the pace of AI capability improvement leave sufficient time for institutional adaptation, or does it outrun policy response capacity?',
    'Comparison of AI capability doubling time vs historical policy response lag (e.g., 1930s: 3-5 years from crisis to New Deal; 2008: 6-12 months to major interventions; COVID: 2-3 months to fiscal expansion). If AI capabilities double every 6-18 months, does this outpace institutional learning?',
    'If AI timeline is slow (5-10 years to transformative impact): institutions have time to adapt, inertia assumption is false. If timeline is fast (1-3 years): institutions cannot adapt quickly enough, inertia assumption is structural.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(ai_capability_timeline_uncertainty, empirical, 'Whether AI capability improvement outpaces institutional adaptation capacity').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(institutional_inertia_assumption, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(inst_inertia_tr_t0, institutional_inertia_assumption, theater_ratio, 0, 0.32).
narrative_ontology:measurement(inst_inertia_tr_t3, institutional_inertia_assumption, theater_ratio, 3, 0.38).
narrative_ontology:measurement(inst_inertia_tr_t6, institutional_inertia_assumption, theater_ratio, 6, 0.45).

% Extraction over time
narrative_ontology:measurement(inst_inertia_be_t0, institutional_inertia_assumption, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(inst_inertia_be_t3, institutional_inertia_assumption, base_extractiveness, 3, 0.42).
narrative_ontology:measurement(inst_inertia_be_t6, institutional_inertia_assumption, base_extractiveness, 6, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(institutional_inertia_assumption, information_standard).

% DUAL FORMULATION NOTE:
% This constraint is downstream of labor_share_collapse (mountain: the physical fact of automation capability) and ghost_gdp_circulation (rope: the coordination problem of income recirculation). The institutional inertia assumption is a distinct constraint with its own ε value (0.48) reflecting the scenario modeling choice to hold institutions constant. The upstream constraints have different ε values: labor_share_collapse ≈ 0.15 (low extraction, mostly natural law of automation capability), ghost_gdp_circulation ≈ 0.25 (low extraction, coordination problem with known solutions). The institutional inertia assumption has higher ε because it actively suppresses policy counterfactuals rather than describing a natural limit or coordination challenge, but the coordination function (isolating variables for clear scenario logic) is substantial enough to keep ε in the tangled_rope range rather than snare.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
