% ============================================================================
% CONSTRAINT STORY: sotu_1978_carter_national_energy_program
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sotu_1978_carter_national_energy_program, []).

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
 *   constraint_id: sotu_1978_carter_national_energy_program
 *   human_readable: Carter's 1978 National Energy Program: Oil Import Reduction and Consumption Control
 *   domain: infrastructure/energy_policy/macroeconomic
 *
 * SUMMARY:
 *   In 1978, after five years of stalemate since the 1973 oil embargo,
 *   President Carter proposes a comprehensive national energy program
 *   designed to reduce oil imports and domestic consumption through
 *   coordinated domestic production increases, conservation mandates, and a
 *   shift toward abundant fuels. The constraint exhibits tangled rope
 *   structure: it genuinely coordinates a macroeconomic problem (inflation,
 *   unemployment, currency weakness caused by oil dependency on volatile
 *   global markets) while simultaneously extracting from powerless households
 *   through consumption limits and price increases. The benefit (lower
 *   inflation, stronger dollar, lower unemployment) accrues broadly across
 *   the economy, justifying the coordination classification. The cost (higher
 *   energy prices, consumption restrictions, disrupted supply chains) falls
 *   on those least able to absorb it, justifying the extraction
 *   classification. The constraint requires active federal enforcement (price
 *   controls, consumption quotas, production mandates) to maintain the
 *   hybrid, which distinguishes it from pure coordination. The theater ratio
 *   reflects that the program performs energy autonomy (national independence
 *   narrative) while global OPEC pricing power remains determinative of
 *   outcomes. The baseline extractiveness (0.20) reflects that the program
 *   begins as a coordination response to genuine macroeconomic dysfunction;
 *   the extractiveness rises to 0.38 as enforcement tightens and powerless
 *   households bear the accumulated burden.
 *
 * KEY AGENTS:
 *   - High-Consumption Working Households: Primary victims (powerless/trapped) — face mandatory consumption limits, price increases, and no exit options due to geographic isolation and employment dependence
 *   - Energy-Intensive Manufacturers: Secondary victims (powerless/trapped) — face immediate cost spikes with no transition support; cannot relocate or reduce energy use without closure
 *   - Fossil Fuel Industries: Mixed beneficiary/victim (organized/constrained) — face extraction via price controls and import restrictions, but benefit from domestic production mandates and synthetic fuels investment
 *   - Federal Executive and Macroeconomic Authorities: Primary beneficiary (institutional/arbitrage) — coordinates response to inflation and currency crisis; benefits flow broadly across economy
 *   - Transition-Oriented Firms and Communities: Secondary beneficiary (organized/constrained) — coal regions, solar manufacturers, efficiency firms see temporary coordination problem with built-in sunset as alternatives mature
 *   - Global Oil Markets and OPEC: Structural power holder (powerful/arbitrage) — remains determinative of outcomes; program's extraction function depends on performative framing of U.S. autonomy
 *   - Analytical Observer: Civilizational view (analytical/analytical) — sees unavoidable tangled rope: macroeconomic coordination requires extraction from powerless agents; no policy design escapes this coupling given global constraints
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sotu_1978_carter_national_energy_program, 0.38).
domain_priors:suppression_score(sotu_1978_carter_national_energy_program, 0.48).
domain_priors:theater_ratio(sotu_1978_carter_national_energy_program, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sotu_1978_carter_national_energy_program, extractiveness, 0.38).
narrative_ontology:constraint_metric(sotu_1978_carter_national_energy_program, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(sotu_1978_carter_national_energy_program, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sotu_1978_carter_national_energy_program, tangled_rope).
narrative_ontology:human_readable(sotu_1978_carter_national_energy_program, "Carter's 1978 National Energy Program: Oil Import Reduction and Consumption Control").
narrative_ontology:topic_domain(sotu_1978_carter_national_energy_program, "infrastructure/energy_policy/macroeconomic").

domain_priors:requires_active_enforcement(sotu_1978_carter_national_energy_program).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sotu_1978_carter_national_energy_program, broad_economy).
narrative_ontology:constraint_beneficiary(sotu_1978_carter_national_energy_program, consumers_via_macroeconomic_benefit).
narrative_ontology:constraint_beneficiary(sotu_1978_carter_national_energy_program, domestic_energy_producers).
narrative_ontology:constraint_victim(sotu_1978_carter_national_energy_program, fossil_fuel_industries).
narrative_ontology:constraint_victim(sotu_1978_carter_national_energy_program, high_consumption_households).
narrative_ontology:constraint_victim(sotu_1978_carter_national_energy_program, energy_intensive_manufacturers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: HIGH-CONSUMPTION WORKING HOUSEHOLD (SNARE) — Faces mandatory consumption limits, higher energy prices, and no real exit option. Geographic isolation and commute dependence make fuel conservation painful rather than optional. Trapped by circumstance (family location, employment, housing stock design) with no arbitrage available. Experiences maximum extraction with suppression through necessity (cannot choose to consume less without lifestyle collapse).
constraint_indexing:constraint_classification(sotu_1978_carter_national_energy_program, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: ENERGY-INTENSIVE MANUFACTURERS (SNARE) — Small manufacturers without political capital face immediate cost spikes with no transition support. Cannot relocate overseas (not yet normalized), cannot reduce energy use without shuttering, cannot pass costs to consumers without losing market share. Trapped by sunk capital and market structure. Experiences high extraction with high suppression (regulatory mandate removes alternatives).
constraint_indexing:constraint_classification(sotu_1978_carter_national_energy_program, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 3: FOSSIL FUEL INDUSTRIES (TANGLED ROPE) — Large oil and coal producers face price controls and production quotas (extraction), but also benefit from increased domestic production mandates and synthetic fuels investment (coordination). Organized enough to negotiate (constrained exit rather than trapped), with political voice. Net structure: extraction via price caps and import restrictions paired with genuine coordination benefit (domestic production incentives, synthetic fuels subsidies). Active enforcement required to maintain the hybrid.
constraint_indexing:constraint_classification(sotu_1978_carter_national_energy_program, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: FEDERAL EXECUTIVE / MACROECONOMIC AUTHORITIES (ROPE) — Sees the constraint as necessary coordination: reducing oil dependency enables lower inflation, stronger dollar, lower unemployment. Benefits flow broadly across economy. Exit available through international energy markets and fiscal flexibility (arbitrage position). Experiences the constraint as coordination with manageable enforcement costs. The program is solution-seeking, not predatory, from this perspective.
constraint_indexing:constraint_classification(sotu_1978_carter_national_energy_program, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: TRANSITION-ORIENTED FIRMS / COMMUNITIES (SCAFFOLD) — Coal mining regions, solar/wind equipment manufacturers, energy-efficient building material producers see a temporary coordination problem with built-in sunset: the program creates transition incentives (synthetic fuels, renewable research funding) that will eventually displace fossil fuels entirely. Low effective extraction because these actors have agency in shaping the transition and see an exit path as alternatives mature. Sunset logic: the program is deliberately temporary, designed to phase in alternatives.
constraint_indexing:constraint_classification(sotu_1978_carter_national_energy_program, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: GLOBAL OIL MARKETS / OPEC (PITON) — The constraint is performative at the global scale. OPEC production decisions dwarf any U.S. domestic policy impact. The national energy program theater (consumption targets, efficiency mandates) persists as constraint on U.S. actors while the global extraction mechanism (OPEC pricing power) remains unchanged. The program's extraction function depends on suppressing awareness that U.S. oil independence is impossible without global reordering. Theater ratio high because the program performs energy autonomy while global power dynamics persist.
constraint_indexing:constraint_classification(sotu_1978_carter_national_energy_program, piton,
    context(agent_power(powerful),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / GLOBAL VIEW (TANGLED ROPE) — The constraint genuinely coordinates a macroeconomic problem (inflation, unemployment, currency weakness tied to oil dependency) while simultaneously extracting from powerless households through consumption limits and price increases. The tangled rope structure is unavoidable: the coordination benefit (lower inflation through lower consumption) requires extraction (higher prices, consumption restrictions) on those least able to bear the burden. No way to achieve one without the other given the constraint's design.
constraint_indexing:constraint_classification(sotu_1978_carter_national_energy_program, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sotu_1978_carter_national_energy_program_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(sotu_1978_carter_national_energy_program, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(sotu_1978_carter_national_energy_program, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(sotu_1978_carter_national_energy_program, TR),
    TR >= 0.70.

:- end_tests(sotu_1978_carter_national_energy_program_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The program begins as a macroeconomic coordination response (ε=0.20 at t=0) to genuine dysfunction (inflation, unemployment, currency weakness). As enforcement tightens and powerless households absorb price increases and consumption restrictions, effective extractiveness rises to 0.38. This is not maximum extraction (which would require complete suppression of alternatives) but significant extraction because powerless agents cannot arbitrage or exit. Suppression (0.48): Moderate-high. Conservation mandates and price controls remove alternatives for working households, but suppression is not total — black markets exist, some consumption shifting occurs, and federal enforcement can be evaded. The suppression derives primarily from necessity (geographic isolation, employment dependence) rather than pure coercion. Theater ratio (0.55): Moderate. The program performs energy autonomy and national strength while global market power remains with OPEC. The rhetoric (independence from foreign oil) exceeds the material reality (global prices remain determinative), but the theater is not complete — actual domestic production increases and consumption reductions are real physical facts. Claimed type: tangled_rope. The program requires both genuine coordination function (solving macroeconomic crisis) and asymmetric extraction (costs on powerless, benefits broadly distributed). Active enforcement is necessary to maintain the hybrid without one element collapsing into pure coordination or pure extraction.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates how different structural positions produce radically different classifications from identical base properties. The working household sees a snare: mandatory consumption limits with no exit. The fossil fuel industry sees tangled rope: extraction (price controls) paired with genuine coordination benefit (domestic production investment). The federal executive sees rope: coordination of macroeconomic crisis with manageable enforcement costs. The transition-oriented firm sees scaffold: a temporary coordination problem with a built-in sunset as alternatives mature. The global oil market sees a piton: the program performs energy autonomy while remaining dependent on OPEC pricing. The perspectival gap reflects real structural differences in power, exit options, and relationship to the extraction flow. All perspectives are analytically valid; none is the 'true' classification.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values are derived from each agent's structural position: their power level relative to the constraint, their exit options, and their relationship to the extraction flow. High-consumption households are beneficiaries of the macroeconomic benefit (lower inflation) but victims of the consumption restrictions and price increases; they are trapped (no geographic or employment mobility) with no arbitrage option, yielding high d (0.85-0.95). Fossil fuel industries are victims of price controls and import restrictions but beneficiaries of domestic production mandates; they are organized with constrained but non-zero exit, yielding moderate d (0.45-0.55). The federal executive experiences the constraint as coordination (d=0.15-0.25) with arbitrage options (international energy markets, fiscal flexibility). The analytical observer sees the constraint as unavoidable tangled rope at civilizational scale — no way to achieve macroeconomic coordination without extracting from those least able to bear the burden given global market constraints and domestic energy endowments.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by recognizing that the constraint is legitimately tangled rope from the analytical perspective: it coordinates a real macroeconomic problem (inflation, unemployment, currency crisis tied to global oil dependency) while simultaneously extracting from those least able to bear burden. The tension between coordination and extraction is not resolvable through better policy design at this scale — it reflects the global constraint (OPEC pricing power) and domestic constraint (oil dependency, geographic structure). The program's design optimizes the tangled rope: it maximizes coordination benefit (lower inflation through lower consumption) while distributing extraction burden as equitably as possible given the constraints (uniform consumption caps rather than progressive taxation, which would be politically infeasible). The false summit trap would be classifying this as pure rope (coordination) while ignoring the real extraction on powerless households. The correct classification is tangled rope because the extraction is genuine and necessary, not elimination of the extraction mechanism would collapse the coordination benefit.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    distributional_burden_necessity,
    'Are the costs borne by high-consumption working households and energy-intensive manufacturers a necessary feature of macroeconomic adjustment, or could alternative policy designs distribute burden more equitably?',
    'Comparative analysis of energy policy designs from other countries facing oil shocks (Germany, Japan, Sweden) and their distributional outcomes; counterfactual modeling of progressive energy taxes with rebates vs. across-the-board consumption caps',
    'If necessary: the snare classification reflects structural inevitability (constraint is optimization problem with hard constraints). If avoidable: the snare classification reflects a policy choice that extracts from powerless agents when alternatives existed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(distributional_burden_necessity, conceptual, 'Whether distributional burden on powerless households is structurally necessary or a policy choice').

omega_variable(
    effectiveness_of_demand_destruction,
    'Do consumption mandates and price signals actually reduce oil imports, or do they primarily redistribute consumption to black markets, regional scarcity, and economic disruption without improving trade balance?',
    'Time-series analysis of oil import volumes pre- and post-program; tracking of gasoline lines, heating oil shortages, and their duration; comparison to outcomes in countries that did not implement mandatory conservation',
    'If effective: the extraction is paired with real macroeconomic benefit, justifying tangled rope. If ineffective: the extraction persists while benefit evaporates, degrading classification toward snare.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(effectiveness_of_demand_destruction, empirical, 'Whether consumption mandates reduce oil imports vs. causing redistribution and scarcity').

omega_variable(
    fossil_fuel_subsidy_paradox,
    'Do synthetic fuels and domestic production incentives in the program constitute genuine coordination benefit for fossil fuel industries, or do they represent extraction masked as investment?',
    'Analysis of synthetic fuels program outcomes: capital deployed, energy returned, ultimate market viability; comparison of subsidy levels to price controls'' impact on industry margins',
    'If genuine coordination: tangled rope classification holds (extraction + benefit). If masked extraction: fossil fuel industries are also net targets, and broader classification shifts toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fossil_fuel_subsidy_paradox, empirical, 'Whether synthetic fuels/domestic production incentives provide real benefit or masked extraction').

omega_variable(
    global_opec_power_asymmetry,
    'To what extent do the program''s extraction mechanisms depend on suppressing awareness that U.S. oil independence from OPEC is structurally impossible without global reordering?',
    'Historical analysis of program rhetoric vs. actual geopolitical capacity; tracking of policymaker statements about energy independence; long-term trends in U.S. oil imports and global market share',
    'If high suppression dependence: the piton classification is correct (theater ratio ≥0.70). If low: the program''s extraction function might be more real than performative.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(global_opec_power_asymmetry, empirical, 'Dependency of extraction mechanism on suppressing global power asymmetries').

omega_variable(
    transition_timeline_credibility,
    'Is the scaffold sunset clause (transition to alternatives) credible given the actual deployment timelines for synthetic fuels, solar, and other alternatives in 1978?',
    'Historical tracking of energy program transition timelines; deployment rates for alternatives; whether the program''s sunset was de facto (alternatives never materialized) or de jure (program ended and was replaced)',
    'If credible: scaffold perspective holds (temporary with real exit path). If not credible: scaffold is aspirational and the program is de facto permanent snare/tangled rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(transition_timeline_credibility, empirical, 'Credibility of scaffold sunset clause for transition to alternative energy').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sotu_1978_carter_national_energy_program, 0, 4).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(carter_energy_tr_t0, sotu_1978_carter_national_energy_program, theater_ratio, 0, 0.42).
narrative_ontology:measurement(carter_energy_tr_t2, sotu_1978_carter_national_energy_program, theater_ratio, 2, 0.5).
narrative_ontology:measurement(carter_energy_tr_t4, sotu_1978_carter_national_energy_program, theater_ratio, 4, 0.55).

% Extraction over time
narrative_ontology:measurement(carter_energy_be_t0, sotu_1978_carter_national_energy_program, base_extractiveness, 0, 0.2).
narrative_ontology:measurement(carter_energy_be_t2, sotu_1978_carter_national_energy_program, base_extractiveness, 2, 0.32).
narrative_ontology:measurement(carter_energy_be_t4, sotu_1978_carter_national_energy_program, base_extractiveness, 4, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sotu_1978_carter_national_energy_program, resource_allocation).
narrative_ontology:affects_constraint(sotu_1978_carter_national_energy_program, opec_pricing_power_constraint).
narrative_ontology:affects_constraint(sotu_1978_carter_national_energy_program, domestic_energy_production_limits).
narrative_ontology:affects_constraint(sotu_1978_carter_national_energy_program, household_consumption_patterns_infrastructure).

% DUAL FORMULATION NOTE:
% The national energy program constraint is downstream of the OPEC pricing power constraint (global) and the domestic energy production limit constraint (physical). The program's extraction mechanism depends on suppressing awareness of these upstream constraints. Separate constraint stories should model (a) OPEC cartel power as global snare and (b) domestic energy endowments and resource distribution as mountain-adjacent constraint. The energy program links these into a national policy vector.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(sotu_1978_carter_national_energy_program, moderate, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
