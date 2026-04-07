% ============================================================================
% CONSTRAINT STORY: sotu_1947_truman_wartime_emergency_controls_termination
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sotu_1947_truman_wartime_emergency_controls_termination, []).

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
 *   constraint_id: sotu_1947_truman_wartime_emergency_controls_termination
 *   human_readable: Systematic Repeal of Wartime Emergency Economic Controls (1947)
 *   domain: regulatory/economic_policy
 *
 * SUMMARY:
 *   In 1947, President Truman proposed systematic repeal of federal emergency
 *   economic controls imposed during World War II. Wartime controls had
 *   regulated prices, production, and resource allocation across agriculture,
 *   manufacturing, transportation, and consumer goods to coordinate war
 *   production and prevent profiteering. The constraint examined here is the
 *   legislative process of decontrolling this economy: Congress would review
 *   statutes classified as temporary or emergency measures and selectively
 *   repeal them, returning decision-making from government mandate to private
 *   enterprise. This constraint exhibits a stark distributional asymmetry:
 *   business enterprises and agricultural producers benefit from price and
 *   production decontrol, while wage workers and urban consumers face price
 *   increases without corresponding wage decontrol. The temporal asymmetry
 *   creates a window of extraction: workers and consumers bear the cost of
 *   rising prices immediately, while wage adjustment lags by months or
 *   longer. The repeal process is ostensibly orderly and legislative,
 *   creating appearances of coordination, but the underlying mechanism
 *   transfers wealth from labor and consumers to capital and agriculture.
 *
 * KEY AGENTS:
 *   - Wage Workers: Primary victim (powerless/trapped) — face real-wage decline as prices decontrol without wage adjustment
 *   - Urban Consumers: Primary victim (powerless/trapped) — dependent on market availability at affordable prices; no exit from consumption
 *   - Business Enterprises: Primary beneficiary (institutional/arbitrage) — gain profit optimization, pricing power, and production autonomy; organized lobby capacity
 *   - Agricultural Producers: Primary beneficiary (institutional/arbitrage) — gain market pricing for agricultural commodities; organized farm lobby
 *   - Labor Organizations: Secondary actor (organized/constrained) — can lobby and negotiate but face weak bargaining position post-WWII
 *   - Congress: Institutional actor with temporary authority (institutional/arbitrage) — holds selective repeal authority but faces business/agricultural lobbying pressure
 *   - War-Era Administrative State: Institutional persistence (analytical/analytical) — wartime controls apparatus persists through inertia despite peacetime conditions
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sotu_1947_truman_wartime_emergency_controls_termination, 0.52).
domain_priors:suppression_score(sotu_1947_truman_wartime_emergency_controls_termination, 0.58).
domain_priors:theater_ratio(sotu_1947_truman_wartime_emergency_controls_termination, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sotu_1947_truman_wartime_emergency_controls_termination, extractiveness, 0.52).
narrative_ontology:constraint_metric(sotu_1947_truman_wartime_emergency_controls_termination, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(sotu_1947_truman_wartime_emergency_controls_termination, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sotu_1947_truman_wartime_emergency_controls_termination, tangled_rope).
narrative_ontology:human_readable(sotu_1947_truman_wartime_emergency_controls_termination, "Systematic Repeal of Wartime Emergency Economic Controls (1947)").
narrative_ontology:topic_domain(sotu_1947_truman_wartime_emergency_controls_termination, "regulatory/economic_policy").

domain_priors:requires_active_enforcement(sotu_1947_truman_wartime_emergency_controls_termination).
narrative_ontology:has_sunset_clause(sotu_1947_truman_wartime_emergency_controls_termination).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sotu_1947_truman_wartime_emergency_controls_termination, business_enterprises).
narrative_ontology:constraint_beneficiary(sotu_1947_truman_wartime_emergency_controls_termination, agricultural_producers).
narrative_ontology:constraint_victim(sotu_1947_truman_wartime_emergency_controls_termination, wage_workers).
narrative_ontology:constraint_victim(sotu_1947_truman_wartime_emergency_controls_termination, urban_consumers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: WAGE WORKER (SNARE) — Faces decontrol of prices without corresponding wage decontrol. Real wages decline as prices rise. No exit mechanism: workers cannot opt out of the consumer economy or leave the labor market without severe cost. Trapped by economic necessity. Maximum experienced extraction from the standpoint of lost purchasing power.
constraint_indexing:constraint_classification(sotu_1947_truman_wartime_emergency_controls_termination, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: URBAN CONSUMER (SNARE) — Faces decontrol of food, fuel, and housing prices. Cannot produce subsistence goods independently. Dependent on market availability at affordable prices. Decontrol creates immediate extraction through price spikes. No exit option from the consumer role. Trapped by geographic and economic circumstances in wage-dependent cities.
constraint_indexing:constraint_classification(sotu_1947_truman_wartime_emergency_controls_termination, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 3: BUSINESS ENTERPRISES (ROPE) — Primary beneficiary. Decontrol removes ceiling prices and production mandates, enabling profit optimization. Experiences constraint as coordination mechanism: the legislative process for selective repeal is orderly, transparent, and enables business planning. Can arbitrage regulatory gaps (lobbying for favorable repeal terms). Net beneficiary with significant exit capacity.
constraint_indexing:constraint_classification(sotu_1947_truman_wartime_emergency_controls_termination, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: AGRICULTURAL PRODUCERS (ROPE) — Primary beneficiary. Price controls have suppressed farm income; decontrol enables market pricing. Farm organizations have organized lobby power. Can arbitrage legislative process to ensure favorable repeal terms. Experienced constraint as manageable coordination problem with clear exit pathway (lobbying for farmer-favorable statutes).
constraint_indexing:constraint_classification(sotu_1947_truman_wartime_emergency_controls_termination, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: LABOR ORGANIZATIONS (TANGLED ROPE) — Organized agents with moderate constraints. Can lobby Congress and organize wage demands, but face political headwinds post-WWII. The constraint has genuine coordination function (orderly legislative transition from wartime to peacetime economy) but asymmetrically benefits business over labor. Labor gains some agency through organization and negotiation capacity but faces suppression through weak bargaining position and anti-union political momentum.
constraint_indexing:constraint_classification(sotu_1947_truman_wartime_emergency_controls_termination, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: CONGRESSIONAL OVERSIGHT (SCAFFOLD) — Congress has sunset mechanism built in: statutes are classified as temporary/emergency and Congress must affirmatively repeal each one. This creates opportunity for negotiated phase-out with conditions (price stabilization, wage adjustments, consumer protections) rather than wholesale decontrol. The oversight function is real and has agency, creating a temporary constraint with a sunset: as Congress repeals statutes, the decontrol accelerates but in discretionary increments rather than all-at-once shock.
constraint_indexing:constraint_classification(sotu_1947_truman_wartime_emergency_controls_termination, scaffold,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 7: WAR-ERA ADMINISTRATIVE STATE (PITON) — From the civilizational perspective, wartime economic controls were a temporary response to singular crisis conditions (war production coordination). Peacetime return to market mechanisms is framed as natural resumption of normal economic order. The administrative apparatus that enforced controls persists through institutional inertia but its function has largely atrophied — war is over, emergency is nominally ended, so peacetime 'normalcy' seems inevitable. This perspective obscures that the 'normal' state involves accepting price volatility and consumer vulnerability that wartime controls prevented.
constraint_indexing:constraint_classification(sotu_1947_truman_wartime_emergency_controls_termination, piton,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(national))).

% PERSPECTIVE 8: MARKET EQUILIBRIUM NATURALIZATION (MOUNTAIN) — The analytical observer at civilizational scope risks reading this constraint as a natural law: markets are 'naturally' efficient, price controls 'naturally' create distortion, decontrol 'naturally' restores balance. The economy moves from artificial wartime constraint back to natural market clearing. This perspective naturalizes contingent policy choices as immutable economic laws. False summit risk: the 'natural' market is itself constructed through legal frameworks (contract law, property rights enforcement, bankruptcy rules) and benefits identifiable agents (business, agriculture) while imposing costs on others (workers, consumers).
constraint_indexing:constraint_classification(sotu_1947_truman_wartime_emergency_controls_termination, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sotu_1947_truman_wartime_emergency_controls_termination_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(sotu_1947_truman_wartime_emergency_controls_termination, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(sotu_1947_truman_wartime_emergency_controls_termination, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(sotu_1947_truman_wartime_emergency_controls_termination, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(sotu_1947_truman_wartime_emergency_controls_termination, TR),
    TR >= 0.70.

:- end_tests(sotu_1947_truman_wartime_emergency_controls_termination_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The decontrol process systematically transfers wealth from workers/consumers to business/agriculture through the pricing asymmetry. However, extraction is not maximal because: (1) Congressional oversight creates potential for phase-in and conditioned repeal, (2) labor organizations retain some bargaining capacity, (3) business beneficiaries face competition that limits pricing power. The measured value reflects that this is genuine extraction (not coordination) but tempered by institutional constraints. Theater ratio (0.48): Moderate. The legislative repeal process is substantially real — Congress must vote, debate statutes, and make explicit choices about which controls to repeal. This is lower theater than pure administrative fiat would be. However, the framing of decontrol as 'returning to normalcy' obscures the policy choice being made and the distributional consequences. Suppression (0.58): Moderate-high. Suppression includes: wage workers' inability to exit wage dependence or consumer markets; political weakness of labor organizations post-WWII; lack of information available to consumers about price trajectory; institutional inertia favoring business positions. However, suppression is not maximal because Congress retains formal authority and could condition repeal.
 *
 * PERSPECTIVAL GAP:
 *   The constraint exhibits maximum perspectival divergence across the eight perspectives. Wage workers and urban consumers experience this as a snare: they face extraction with no exit option, and the legislative process is theater — the outcome (price increases) is predetermined by business/agricultural lobbying power, and Congress's nominal role obscures the distributional asymmetry. Business enterprises and agricultural producers experience this as rope: they see genuine coordination (orderly legislative transition from wartime to peacetime economy) and clear benefit from decontrol. Labor organizations experience it as tangled rope: they have some agency through organization and negotiation but face structural suppression and asymmetric extraction. Congress experiences it as scaffold: it holds formal authority to condition repeal and phase-in decontrol, creating potential for negotiated transition with protections. The war-era administrative state experiences it as piton: wartime controls persist through institutional inertia, and decontrol is framed as natural resumption of normalcy. The civilizational analytical observer risks seeing mountain: markets naturally return to equilibrium after artificial wartime constraint, decontrol is inevitable economic law. This false summit naturalizes contingent policy as inevitable economic reality.
 *
 * DIRECTIONALITY LOGIC:
 *   The constraint's directionality derives from the structural position of each agent relative to the extraction flow. Wage workers (powerless/trapped) have d ≈ 0.95: they face material barriers to exit the labor market and consumer economy, and they bear the cost of price increases without corresponding income adjustment. The d value is high because the binding constraint is material (economic necessity), not choice. Urban consumers (powerless/trapped) have similar d ≈ 0.92: geographic and economic dependence on market availability creates structural entrenchment. Business enterprises (institutional/arbitrage) have d ≈ 0.08: they benefit from decontrol and have organized lobbying capacity (arbitrage option) to influence which statutes are repealed. Agricultural producers (institutional/arbitrage) have d ≈ 0.12: they benefit from price decontrol and have farm organization lobbying power. Labor organizations (organized/constrained) have d ≈ 0.60: they can negotiate and lobby but face structural constraints (weak post-WWII bargaining position, anti-union political momentum). The directionality computation via the sigmoid f(d) amplifies the extractiveness experienced by trapped victims while dampening it for beneficiaries with exit options, producing chi values that reflect the perspectival gap between workers (high chi = high experienced extraction) and business (low or negative chi = experienced coordination benefit).
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by showing that the same structural phenomenon (decontrol of WWII-era price and production controls) constitutes both genuine coordination and asymmetric extraction, depending on the agent's position. Business enterprises see pure coordination (Rope) — decontrol enables collective economic planning transition from wartime to peacetime, orderly and beneficial. Wage workers see pure extraction (Snare) — decontrol imposes costs without consent or compensation. The tangled rope classification correctly captures the constraint's hybrid nature: there is a genuine coordination function (managing the economic transition, enabling private enterprise planning) AND asymmetric extraction (wealth transfer from workers/consumers to business/agriculture). The ambiguity is not in the classification but in whether the coordination benefit justifies the extraction cost — a question that depends on values (how much suffering is acceptable for economic efficiency?) rather than empirical facts. The false summit mountain perspective reveals that naturalizing this constraint as economic law obscures the contingent policy choice being made. The scaffold perspective shows that Congress has actual agency to phase-in decontrol and negotiate conditions, making the constraint temporarily empowered rather than inherently immutable. The piton perspective shows that wartime controls persist through institutional inertia and framed as 'abnormal', preparing ground for the assumption that decontrol is 'normal' — another mechanism that supports the false summit.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    price_path_after_decontrol,
    'Will price increases after decontrol be temporary (supply catches up, inflation moderates) or persistent (structural scarcity, oligopolistic pricing)?',
    'Post-1947 price data tracking: inflation rate trajectory for key commodities (food, fuel, housing) 1947-1952; comparison to pre-1941 price baselines and post-WWII production capacity',
    'If temporary: extraction is transitional cost of economic adjustment, scaffold sunset is appropriate. If persistent: extraction is permanent wealth transfer from workers/consumers to business/agriculture, classification should be snare rather than tangled_rope.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(price_path_after_decontrol, empirical, 'Whether post-decontrol price increases are temporary or persistent').

omega_variable(
    wage_adjustment_lag,
    'Do wages adjust at rates comparable to price increases, or is there systematic lag that enables real-wage decline?',
    'Real wage indices 1947-1952: nominal wage growth vs. CPI inflation for key wage-earning sectors (manufacturing, agriculture, service); union contract negotiation cycles and strike frequency data',
    'If lag < 6 months: workers retain purchasing power, extraction is moderate. If lag > 12 months: systematic real-wage decline, extraction is severe and suppression is higher than measured (workers cannot bargain effectively).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(wage_adjustment_lag, empirical, 'Timeline lag between price increases and wage adjustments').

omega_variable(
    congressional_repeal_selectivity,
    'Does Congress selectively repeal (negotiated, conditioned decontrol) or wholesale repeal (all statutes repealed in batch)?',
    'Legislative history 1947-1948: count of repeal bills, amendments, conditions attached; timing of repeal vs. price spike events; evidence of labor/consumer lobbying influence on repeal terms',
    'If selective: scaffold perspective confirmed, Congress has real agency to phase decontrol and negotiate protections. If wholesale: Congress is nominal actor with limited discretion, decontrol is de facto determined by executive/business pressure, constraint reclassifies toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(congressional_repeal_selectivity, empirical, 'Whether Congressional repeal is selective/conditioned or wholesale').

omega_variable(
    black_market_emergence,
    'Did wartime controls prevent black markets, or will decontrol prevent price gouging through competitive markets?',
    'Historical data on black market activity during WWII vs. post-decontrol price gouging complaints and enforcement actions; analysis of whether controlled prices prevented or merely displaced extraction',
    'If controls prevented black markets: decontrol reduces total extraction (legal markets are more transparent/competitive). If controls created black markets: decontrol legitimizes existing extraction, reclassifies from snare to rope/tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(black_market_emergence, empirical, 'Whether wartime controls prevented or displaced extraction into black markets').

omega_variable(
    beneficiary_coalition_cohesion,
    'Do business and agricultural beneficiaries form stable coalition for coordinated repeal, or compete for favorable terms?',
    'Lobbying activity data: shared vs. conflicting interests (e.g., food processors vs. farmers on agricultural price controls); legislative voting patterns by industry sector; evidence of business-labor coalition attempts',
    'If cohesive: beneficiaries have organized power and can impose maximum extraction. If fractured: labor/consumer advocates may exploit divisions and negotiate better terms. Affects suppression level and whether scaffold phase-in is real.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_coalition_cohesion, empirical, 'Cohesion of beneficiary coalition (business and agricultural producers)').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sotu_1947_truman_wartime_emergency_controls_termination, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(warctrl_tr_t0, sotu_1947_truman_wartime_emergency_controls_termination, theater_ratio, 0, 0.42).
narrative_ontology:measurement(warctrl_tr_t2, sotu_1947_truman_wartime_emergency_controls_termination, theater_ratio, 2, 0.45).
narrative_ontology:measurement(warctrl_tr_t4, sotu_1947_truman_wartime_emergency_controls_termination, theater_ratio, 4, 0.48).
narrative_ontology:measurement(warctrl_tr_t6, sotu_1947_truman_wartime_emergency_controls_termination, theater_ratio, 6, 0.51).

% Extraction over time
narrative_ontology:measurement(warctrl_be_t0, sotu_1947_truman_wartime_emergency_controls_termination, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(warctrl_be_t2, sotu_1947_truman_wartime_emergency_controls_termination, base_extractiveness, 2, 0.38).
narrative_ontology:measurement(warctrl_be_t4, sotu_1947_truman_wartime_emergency_controls_termination, base_extractiveness, 4, 0.52).
narrative_ontology:measurement(warctrl_be_t6, sotu_1947_truman_wartime_emergency_controls_termination, base_extractiveness, 6, 0.61).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sotu_1947_truman_wartime_emergency_controls_termination, resource_allocation).
narrative_ontology:affects_constraint(sotu_1947_truman_wartime_emergency_controls_termination, inflation_wage_lag_1947_1952).
narrative_ontology:affects_constraint(sotu_1947_truman_wartime_emergency_controls_termination, post_war_labor_movement_suppression).

% DUAL FORMULATION NOTE:
% This constraint is part of the post-WWII economic settlement cluster. Upstream: wartime price/production control mechanism (coordination function; ε ≈ 0.10, Mountain). This story: decontrol repeal process (ε = 0.52, Tangled Rope). Downstream: inflation and wage adjustment dynamics (ε ≈ 0.48-0.65 depending on sector and timeline).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(sotu_1947_truman_wartime_emergency_controls_termination, organized, 0.6).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
