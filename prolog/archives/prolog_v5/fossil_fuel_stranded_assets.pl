% ============================================================================
% CONSTRAINT STORY: fossil_fuel_stranded_assets
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_fossil_fuel_stranded_assets, []).

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
 *   constraint_id: fossil_fuel_stranded_assets
 *   human_readable: Fossil Fuel Stranded Assets Lock-In
 *   domain: economic_policy/energy_transition
 *
 * SUMMARY:
 *   Fossil fuel stranded assets represent a structural constraint on energy
 *   transition created by the mismatch between the economic lifetime of
 *   hydrocarbon infrastructure (30-40 years) and the policy timelines for
 *   decarbonization (10-20 years). Incumbent energy corporations have
 *   deployed capital into long-lived assets (coal plants, oil refineries, LNG
 *   terminals, exploration leases) with expected returns extending decades.
 *   Climate commitments and renewable cost decline threaten asset values
 *   before payback, creating stranded assets. Rather than accept losses,
 *   incumbents use political influence to slow transition, maintain
 *   subsidies, and secure regulatory protection for asset values. This
 *   creates a snare constraint: future generations and climate stability are
 *   trapped by continued fossil fuel dependence; energy transition investors
 *   are trapped by incumbent market dominance and suppressed renewable sector
 *   returns; grid operators are trapped by sunk cost fallacy and regulatory
 *   rate-of-return guarantees. Incumbents experience the constraint as a
 *   coordination problem to manage rather than a lock-in to escape. The
 *   constraint exhibits high suppression (regulatory capture, subsidy
 *   lock-in, externality non-pricing) but moderate theater because the
 *   underlying mechanism (asset protection through policy influence) is
 *   relatively transparent — the extraction is explicit rather than hidden.
 *
 * KEY AGENTS:
 *   - Climate System and Future Populations: Primary victim (powerless/trapped) — cannot exit or advocate; bears catastrophic cost of delayed transition through warming damages
 *   - Energy Transition Investors: Secondary victim (powerless/trapped) — trapped by incumbent market dominance, suppressed by subsidy redirection, facing extraction through delayed transition window extending investment returns
 *   - Grid Operators and Utilities: Secondary institutional actor (moderate/constrained) — experience coordination (stable baseload) and extraction (path dependency on fossil infrastructure and regulatory protection of incumbent returns)
 *   - Incumbent Energy Corporations: Primary beneficiary (institutional/arbitrage) — can reallocate capital, lobby for favorable terms, delay asset write-downs through policy influence
 *   - Just Transition Coalition: Organized challengers (organized/constrained) — see exit pathway through policy (carbon pricing, renewable mandates, workforce retraining) but face suppression from incumbent political influence
 *   - Financial System: Structural participant (institutional/arbitrage) — holds stranded asset values on balance sheets; both beneficiary (through protection mechanisms) and potentially constrained party (if write-downs trigger cascade)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fossil_fuel_stranded_assets, 0.58).
domain_priors:suppression_score(fossil_fuel_stranded_assets, 0.65).
domain_priors:theater_ratio(fossil_fuel_stranded_assets, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fossil_fuel_stranded_assets, extractiveness, 0.58).
narrative_ontology:constraint_metric(fossil_fuel_stranded_assets, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(fossil_fuel_stranded_assets, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(fossil_fuel_stranded_assets, snare).
narrative_ontology:human_readable(fossil_fuel_stranded_assets, "Fossil Fuel Stranded Assets Lock-In").
narrative_ontology:topic_domain(fossil_fuel_stranded_assets, "economic_policy/energy_transition").

domain_priors:requires_active_enforcement(fossil_fuel_stranded_assets).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(fossil_fuel_stranded_assets, incumbent_energy_corporations).
narrative_ontology:constraint_beneficiary(fossil_fuel_stranded_assets, financial_institutions_holding_fossil_assets).
narrative_ontology:constraint_victim(fossil_fuel_stranded_assets, climate_stability).
narrative_ontology:constraint_victim(fossil_fuel_stranded_assets, energy_transition_investors).
narrative_ontology:constraint_victim(fossil_fuel_stranded_assets, future_generations).
narrative_ontology:constraint_victim(fossil_fuel_stranded_assets, renewable_energy_sector).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CLIMATE STABILITY (SNARE) — Cannot exit the constraint imposed by sunk fossil fuel infrastructure; bears catastrophic cost of delayed transition. Global climate system and future populations have no advocacy mechanism and no exit option. Trapped agent experiencing maximum extraction through forced continuation of carbon-intensive energy systems.
constraint_indexing:constraint_classification(fossil_fuel_stranded_assets, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: ENERGY TRANSITION INVESTORS (SNARE) — Trapped by incumbent market dominance, regulatory capture, and stranded asset write-downs that depress renewable sector valuations. Smaller energy firms and transition-focused investors face systematic extraction through subsidy flow redirection, regulatory favoritism toward incumbent assets, and capital cost inflation. Limited exit options for market participants; high suppression.
constraint_indexing:constraint_classification(fossil_fuel_stranded_assets, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 3: GRID OPERATORS AND UTILITIES (TANGLED ROPE) — Experience genuine coordination function: incumbent assets provide stable baseload power and existing transmission infrastructure. Simultaneously experience extraction through path dependency: sunk costs in fossil infrastructure lock them into continued reliance despite renewable alternatives becoming cheaper. Constrained exit due to regulatory frameworks that protect incumbent asset ROI and rate-of-return guarantees.
constraint_indexing:constraint_classification(fossil_fuel_stranded_assets, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: INCUMBENT ENERGY CORPORATIONS (ROPE) — Primary beneficiary. Experience the constraint as pure coordination: managing transition risk, maintaining production ramp-down control, influencing policy timelines. Arbitrage exit (can reallocate capital, lobby for favorable terms) creates asymmetric extraction flowing toward this agent. The stranded asset constraint preserves their market position during transition window.
constraint_indexing:constraint_classification(fossil_fuel_stranded_assets, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: JUST TRANSITION COALITION (SCAFFOLD) — Organized agents (labor unions, climate coalitions, progressive governments) perceive the constraint as temporary with a sunset: carbon pricing, renewable deployment mandates, and workforce retraining programs are creating exit pathways. Low effective extraction because organized agents see policy mechanisms and timelines for constraint obsolescence. However, implementation gaps and industry influence reduce practical sunset certainty.
constraint_indexing:constraint_classification(fossil_fuel_stranded_assets, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: FOSSIL FUEL SUBSIDY APPARATUS (PITON) — Institutional mechanisms (tax breaks, direct subsidies, externality non-pricing) that protect stranded asset values are largely performative. The functional coordination role (managing energy supply) is being displaced by renewables and grid modernization, yet subsidies persist through inertia and political capture. Theater ratio high because subsidies are justified as 'energy security' and 'economic stability' while their primary function is stranded asset protection.
constraint_indexing:constraint_classification(fossil_fuel_stranded_assets, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / PHYSICAL CONSTRAINT VIEW (MOUNTAIN) — From a thermodynamic and civilizational perspective, the energy transition is an immutable constraint: physical laws dictate that renewable energy must eventually replace finite fossil reserves, and climate physics dictates response urgency. This perspective risks naturalizing what is actually a contingent economic-political arrangement (stranded asset protection mechanisms). The engine will identify this as a false summit — energy transition physics constrains outcomes, but stranded asset lock-in is a policy choice, not a law of nature.
constraint_indexing:constraint_classification(fossil_fuel_stranded_assets, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(fossil_fuel_stranded_assets_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(fossil_fuel_stranded_assets, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(fossil_fuel_stranded_assets, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(fossil_fuel_stranded_assets, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(fossil_fuel_stranded_assets, TR),
    TR >= 0.70.

:- end_tests(fossil_fuel_stranded_assets_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. Incumbents capture substantial economic rent through policy-protected asset values beyond marginal cost of production. The extraction is not as severe as pure monopoly (0.72+) because renewable cost decline is real and cannot be suppressed indefinitely — incumbents cannot extract indefinitely, only delay. Suppression (0.65): High. Multiple mechanisms suppress transition: direct subsidies ($7+ trillion globally including externality non-pricing), regulatory rate-of-return guarantees that protect incumbent returns even as demand declines, lobby influence on carbon pricing stringency, and externality non-pricing that makes fossil energy appear cheaper than renewables. Theater ratio (0.48): Moderate. The constraint is relatively transparent — subsidies and lobby influence are documented and observable. However, rhetorical cover (energy security, economic stability, baseload reliability) provides moderate theater. As renewables mature, the rhetoric becomes increasingly performative rather than functional.
 *
 * PERSPECTIVAL GAP:
 *   The snare classification at powerless/trapped/generational is not disputed by any other perspective — all agents except the beneficiary agree that forced continuation of fossil dependency is extractive. The disagreement is whether the extraction is immutable (mountain view) or has a time horizon (scaffold view) or is being actively managed as coordination (rope view from beneficiary). The coal plant operator in declining region (identity_locked in coal industry) may classify at biographical timeframe as mountain (cannot imagine non-coal future) while the same asset at analytical level classifies as piton (degraded institutional protection maintaining obsolete infrastructure). This perspective gap reveals how identity fusion and institutional inertia prevent recognition of constraint mutability.
 *
 * DIRECTIONALITY LOGIC:
 *   Incumbent corporations are primary beneficiaries with arbitrage exit (can reallocate, influence policy) — derived d ≈ 0.10-0.20, producing negative or low chi (constraint subsidizes them). Trapped climate system and future populations have zero exit and pure victim status — derived d ≈ 0.95, producing high chi (constraint extracts maximum). Energy transition investors have victim status but some geographic/sectoral mobility (constrained exit) — derived d ≈ 0.65-0.75, producing moderate-high chi. Grid operators have victim status but regulatory voice (constrained exit) — derived d ≈ 0.50-0.60. Organized coalitions have victim status but strong organized exit pathways (policy leverage, alternative technology deployment) — derived d ≈ 0.40-0.50. Financial institutions have structural participation status with arbitrage exit (can reallocate) — derived d ≈ 0.30-0.40.
 *
 * MANDATROPHY ANALYSIS:
 *   SNARE CLASSIFICATION CONFIRMED: The constraint exhibits mandatory characteristics of snare classification without ambiguity. Extractiveness 0.58 ≥ 0.46, suppression 0.65 ≥ 0.60, requiring victim declarations (climate, future populations, transition investors) which are present. The snare does not resolve into rope (no genuine coordination function for trapped victims), and does not resolve into tangled_rope (the beneficiary does not perceive obligation to maintain coordination for victim benefit — incumbents would happily exit if they could write down assets without loss). The constraint's resolution requires external intervention (policy, carbon pricing, regulatory mandate) not internal stabilization. The scaffold perspective represents genuine alternative pathways (renewable deployment, carbon pricing, just transition frameworks) but these are external to the snare itself — the snare persists until these alternatives actually displace incumbent power. The piton perspective correctly identifies that subsidy-based asset protection is increasingly theatrical as renewable costs fall, but the theater does not yet undermine suppression or extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    write_down_velocity_threshold,
    'At what write-down rate do stranded assets transition from locked-in to mobilized, and does this threshold vary by asset type and geography?',
    'Empirical analysis of asset write-down timelines (coal plants, oil fields, LNG terminals) and correlation with capital reallocation to renewables; institutional case studies of rapid transition regions vs entrenched regions',
    'If threshold < 5 years globally: many incumbents forced to rapid reallocation, extraction window collapses. If threshold > 20 years: lock-in persists across planning horizon, snare classification sustained. Threshold variance by region determines whether constraint is global or decomposable by jurisdiction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(write_down_velocity_threshold, empirical, 'Rate threshold for stranded asset mobilization and transition initiation').

omega_variable(
    regulatory_capture_persistence,
    'Does incumbent influence over energy policy weaken or strengthen as renewable costs decline and stranded asset write-downs accumulate?',
    'Longitudinal analysis of fossil fuel lobby expenditure vs policy outcomes (subsidies, carbon pricing stringency, transmission investment); comparative study of high-capture vs low-capture jurisdictions; measurement of lag between technical feasibility and policy adoption',
    'If influence weakens: scaffold sunset becomes real, constraint transforms to temporary (Scaffold). If influence strengthens through crisis framing: snare classification deepens, suppression increases. Persistence of capture determines whether constraint is self-limiting or requires external intervention.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(regulatory_capture_persistence, empirical, 'Whether regulatory capture by fossil incumbents persists through energy transition').

omega_variable(
    externality_pricing_sufficiency,
    'Do carbon pricing mechanisms and externality internalization reach levels sufficient to eliminate stranded asset extraction advantage, and at what carbon price point does this occur?',
    'Analysis of effective carbon price (explicit + implicit through regulations) vs marginal renewable energy cost; breakeven analysis for fossil vs renewable assets under various pricing scenarios; tracking of carbon price trajectories in high-ambition jurisdictions',
    'If pricing reaches breakeven (estimated $80-150/tCO2): incumbent extraction advantage disappears, snare classification transitions to rope or dissolves. If pricing stalls below $50/tCO2: extraction persists, snare classification hardens.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(externality_pricing_sufficiency, empirical, 'Carbon price threshold for eliminating stranded asset extraction advantage').

omega_variable(
    financial_system_decoupling,
    'Can financial institutions decouple from fossil asset values without systemic instability, or does the banking system''s fossil fuel exposure create a Gordian knot of mutual capture?',
    'Stress testing of financial institutions under rapid asset write-down scenarios; measurement of fossil fuel asset concentration in major banks and pension funds; analysis of whether coordinated divestment creates feedback loops or enables managed transition',
    'If decoupling possible: policy interventions can force write-down. If Gordian knot exists: financial system stability becomes apparent constraint forcing slower transition, extending snare lock-in period. Determines whether constraint is primarily political or fundamentally structural.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(financial_system_decoupling, empirical, 'Whether financial system can decouple from fossil fuel asset values without systemic shock').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fossil_fuel_stranded_assets, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ffsa_tr_t0, fossil_fuel_stranded_assets, theater_ratio, 0, 0.35).
narrative_ontology:measurement(ffsa_tr_t5, fossil_fuel_stranded_assets, theater_ratio, 5, 0.42).
narrative_ontology:measurement(ffsa_tr_t10, fossil_fuel_stranded_assets, theater_ratio, 10, 0.48).

% Extraction over time
narrative_ontology:measurement(ffsa_be_t0, fossil_fuel_stranded_assets, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(ffsa_be_t5, fossil_fuel_stranded_assets, base_extractiveness, 5, 0.52).
narrative_ontology:measurement(ffsa_be_t10, fossil_fuel_stranded_assets, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(fossil_fuel_stranded_assets, global_infrastructure).
narrative_ontology:affects_constraint(fossil_fuel_stranded_assets, renewable_energy_deployment_bottleneck).
narrative_ontology:affects_constraint(fossil_fuel_stranded_assets, carbon_pricing_political_feasibility).
narrative_ontology:affects_constraint(fossil_fuel_stranded_assets, financial_system_fossil_asset_exposure).

% DUAL FORMULATION NOTE:
% Stranded assets constraint is upstream of specific renewable deployment barriers and carbon pricing deadlock. The constraint family includes: (1) stranded_assets (this story, ε=0.58, snare) — policy protection of incumbent assets; (2) renewable_deployment_bottleneck (ε=0.48, tangled_rope) — grid integration and transmission constraints; (3) carbon_pricing_political_feasibility (ε=0.62, snare) — regulatory capture preventing price signals from reaching economically efficient level. Each has distinct ε and primary mechanism. Stranded assets constraint creates political economy conditions that suppress carbon pricing stringency and renewable deployment investment.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(fossil_fuel_stranded_assets, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
