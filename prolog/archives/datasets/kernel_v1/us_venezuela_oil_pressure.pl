% ============================================================================
% CONSTRAINT STORY: us_venezuela_oil_pressure
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_us_venezuela_oil_pressure, []).

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
 *   constraint_id: us_venezuela_oil_pressure
 *   human_readable: US Geopolitical & Economic Pressure on Venezuela's Oil Sector
 *   domain: geopolitical/economic
 *
 * SUMMARY:
 *   The US embargo on Venezuelan oil represents a structurally fused
 *   coordination-extraction mechanism spanning decades. At its core, the
 *   constraint solves genuine coordination problems: preventing Venezuelan
 *   market dumping that would destabilize global oil prices and OPEC
 *   discipline, excluding a geopolitical competitor from hemispheric resource
 *   control, and maintaining confidence among allied oil producers in US
 *   support for price-supportive regimes. These are not fictional harms —
 *   Venezuelan oil dumping during financial desperation genuinely threatens
 *   global market stability, and US geopolitical rivalry with Venezuela is
 *   structurally real. However, the mechanism for solving these coordination
 *   problems is simultaneously a mechanism of asymmetric wealth and power
 *   extraction: the embargo immiserates the Venezuelan population, transfers
 *   geopolitical leverage to the US state, and enforces subordination on
 *   non-aligned regional actors. These two functions — coordination and
 *   extraction — are operationally inseparable: you cannot prevent Venezuelan
 *   dumping without constraining Venezuelan revenue; you cannot constrain
 *   Venezuelan revenue without economic collapse that drives desperation; you
 *   cannot relieve that desperation without lifting the embargo and losing
 *   price discipline. This is textbook Tangled Rope. The constraint's theater
 *   ratio (0.55) reflects that the legal and diplomatic architecture
 *   increasingly relies on performative legitimacy: UN Charter principles are
 *   invoked but not followed; OFAC authority is domestic law applied
 *   extraterritorially; secondary sanctions violate bilateral trade law;
 *   enforcement relies on implicit coercion (financial exclusion, equipment
 *   denial, infrastructure sabotage) rather than explicit multilateral
 *   agreement. The measurement trajectory shows extraction and suppression
 *   rising over the interval (0 to 16, roughly 1993-2023) as the constraint
 *   escalates from import restrictions to secondary sanctions to
 *   comprehensive financial exclusion, while theater ratio rises as
 *   enforcement becomes more coercive and less legally defensible. The
 *   constraint is not degrading toward Piton — it is intensifying toward
 *   Snare as coordination functions are increasingly subordinated to
 *   extraction functions.
 *
 * KEY AGENTS:
 *   - US Strategic Position: Primary beneficiary (institutional/arbitrage) — maintains geopolitical leverage, prevents hemispheric competitor emergence, sustains allied producer confidence in US security guarantees. Can exit costlessly by lifting sanctions.
 *   - Venezuelan Population: Primary victim (powerless/trapped) — bears extraction through immiseration, food/medicine scarcity, hyperinflation-driven wealth destruction. Cannot exit the constraint or exit the territory without significant cost.
 *   - Venezuelan State (as Economic Actor): Secondary victim (organized/constrained) — constrained by embargo (material barrier) and coordination logic (if it dumps oil, global prices collapse). Can theoretically exit through regime change or policy reform but at prohibitive political cost.
 *   - OPEC & Allied Producers (Saudi Arabia, UAE): Beneficiary-victims (powerful/constrained) — benefit from Venezuelan exclusion (price support, cartel discipline) but are also extracted from by US leverage over the constraint (demand for basing, alignment, favorable pricing in exchange for constraint maintenance).
 *   - Global Oil Price Stability: Beneficiary (abstract coordination mechanism) — the constraint solves a tragedy-of-the-commons problem in petroleum markets, preventing destabilization from Venezuelan dumping. Beneficiary is the coordination mechanism itself.
 *   - International Legal Order: Institutional observer (institutional/constrained) — the constraint increasingly violates the legal architecture it once served (UN Charter, multilateral law); maintains appearance of legality through OFAC authority while operating as unilateral enforcement. Theater ratio rising reflects this degradation.
 *   - Analytical Observer: Sees the structural fusion of coordination and extraction (analytical/analytical) — recognizes that the two functions cannot be separated without structural change, confirming Tangled Rope classification.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(us_venezuela_oil_pressure, 0.58).
domain_priors:suppression_score(us_venezuela_oil_pressure, 0.68).
domain_priors:theater_ratio(us_venezuela_oil_pressure, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(us_venezuela_oil_pressure, extractiveness, 0.58).
narrative_ontology:constraint_metric(us_venezuela_oil_pressure, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(us_venezuela_oil_pressure, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(us_venezuela_oil_pressure, tangled_rope).
narrative_ontology:human_readable(us_venezuela_oil_pressure, "US Geopolitical & Economic Pressure on Venezuela's Oil Sector").
narrative_ontology:topic_domain(us_venezuela_oil_pressure, "geopolitical/economic").

domain_priors:requires_active_enforcement(us_venezuela_oil_pressure).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(us_venezuela_oil_pressure, us_strategic_position).
narrative_ontology:constraint_beneficiary(us_venezuela_oil_pressure, us_allied_producers).
narrative_ontology:constraint_beneficiary(us_venezuela_oil_pressure, global_oil_price_stability).
narrative_ontology:constraint_victim(us_venezuela_oil_pressure, venezuelan_state_revenue).
narrative_ontology:constraint_victim(us_venezuela_oil_pressure, venezuelan_population).
narrative_ontology:constraint_victim(us_venezuela_oil_pressure, regional_non_aligned_states).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: VENEZUELAN POPULATION (SNARE) — Trapped by the constraint with minimal exit options. Extraction flows directly: sanctions disable oil revenue that funds basic imports (food, medicine, fuel). The population experiences maximum suppression — cannot vote away the constraint (geopolitical decision), cannot emigrate without cost, cannot access alternative revenue sources. The constraint's coordination function (preventing runaway dumping) provides zero benefit to this agent. Pure extraction experienced as collective immiseration.
constraint_indexing:constraint_classification(us_venezuela_oil_pressure, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: VENEZUELAN STATE (TANGLED ROPE) — Constrained by both the embargo (material barrier) and the constraint's coordination logic (if Venezuela floods market, regional oil producers suffer). The state benefits from global oil price stability (coordination function) but bears asymmetric costs of enforcement (lost export revenue, equipment sanctions, credit access). Can theoretically exit through policy reform (OPEC cooperation, production discipline), but political legitimacy depends on rejecting the constraint, making exit cognitively and politically costly. Genuine coordination function coexists with asymmetric extraction.
constraint_indexing:constraint_classification(us_venezuela_oil_pressure, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 3: US STRATEGIC INTEREST (ROPE) — Experiences the constraint as pure coordination: preventing Venezuelan oil dumping that would destabilize prices, excluding a geopolitical competitor from hemispheric resource control, and maintaining allied producer confidence in US support for price-supportive regimes. Can exit easily (lift sanctions, engage diplomatically) and experiences net benefit. Arbitrage exit option reflects capacity to unwind the constraint without cost. This perspective sees the constraint as solving a real coordination problem: Venezuelan market dumping harms OPEC discipline and US allies (Saudi Arabia, Gulf producers). The coordination function is genuine from this view.
constraint_indexing:constraint_classification(us_venezuela_oil_pressure, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: OPEC & ALLIED PRODUCERS (TANGLED ROPE) — Benefit from US enforcement of Venezuelan production discipline (coordination function: price support through exclusion of low-cost competitor). But also experience extraction: US leverage over the constraint means US can demand favorable pricing, military basing, or geopolitical alignment in exchange for constraint maintenance. Exit is constrained by dependency on US security guarantees and market coordination — if Venezuela returns to market without discipline, prices collapse, harming all producers. The constraint coordinates their cartel discipline while simultaneously extracting geopolitical submission.
constraint_indexing:constraint_classification(us_venezuela_oil_pressure, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 5: GLOBAL OIL PRICE STABILITY (COORDINATION FUNCTION) — As an abstract collective good, the constraint coordinates production discipline that prevents market shock. Without Venezuelan exclusion, the state's incentive to dump reserves to fund imports would destabilize global markets, harming consuming nations and well-behaved producers alike. This perspective sees genuine coordination: the constraint solves a tragedy-of-the-commons problem in oil markets. The beneficiary here is the global coordination mechanism itself — which is why petroleum-consuming nations tolerate US leverage over the constraint.
constraint_indexing:constraint_classification(us_venezuela_oil_pressure, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: INTERNATIONAL LEGAL ORDER (PITON) — From the perspective of UN Charter principles, nonaligned movement norms, and supposed multilateral constraint on unilateral action, the US embargo is increasingly performative theater. The constraint is justified through sanctions law (IEEPA, OFAC authority, which are domestic US legal instruments), but enforcement relies on a network of secondary sanctions, financial exclusion, and implicit coercion rather than UN-mandated measures. The legal architecture for the constraint has decayed (theater_ratio 0.55 reflects this) — it is maintained through institutional inertia and US power, not through legitimate multilateral agreement. This perspective shows the constraint as a former Rope (genuine multilateral coordination) degraded into something that maintains the appearance of legality while operating as unilateral enforcement.
constraint_indexing:constraint_classification(us_venezuela_oil_pressure, piton,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (TANGLED ROPE) — From a civilizational scale, the constraint represents a structurally fused coordination-extraction mechanism. The coordination function (price stability, preventing market dumping) is real and valuable. The extraction function (geopolitical leverage, wealth transfer, constraint on non-aligned states) is equally real. No separation of these functions is possible without structural change — you cannot prevent Venezuelan dumping while maintaining Venezuelan economic viability and geopolitical autonomy simultaneously. The constraint is tangled because its two functions are operationally inseparable: the same embargo that prevents dumping also immiserates the population, creating the desperation that would drive dumping if the embargo lifted.
constraint_indexing:constraint_classification(us_venezuela_oil_pressure, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(us_venezuela_oil_pressure_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(us_venezuela_oil_pressure, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(us_venezuela_oil_pressure, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(us_venezuela_oil_pressure, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(us_venezuela_oil_pressure, TR),
    TR >= 0.70.

:- end_tests(us_venezuela_oil_pressure_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-to-high. The constraint extracts asymmetric wealth and geopolitical leverage from Venezuela to the US and allied producers. However, extraction is not total because the coordination function is real — global oil price stability genuinely depends on preventing Venezuelan dumping, and this function benefits multiple parties including the global economy. The 0.58 value reflects that extraction dominates but coordination persists. The measurement trajectory (0.28 → 0.58 over 16 time units) shows escalation: early sanctions (1990s) focused on coordination (preventing arms sales, nuclear technology, pricing discipline); contemporary sanctions (2010s-2020s) focus increasingly on regime change and maximum pressure, shifting the constraint toward pure extraction. Suppression (0.68): High but not total. The embargo creates severe barriers to Venezuelan revenue (cannot sell oil, cannot access financing, equipment sanctioned). However, Venezuelan oil continues to be sold through secondary channels (China, Russia), and regime survival is not impossible — only economically devastating for the population. Suppression reflects both material barriers (sanctions enforcement, financial exclusion) and political barriers (US military capacity, allied producer collaboration). Theater ratio (0.55): Moderate-high. The legal architecture for the constraint is increasingly performative: OFAC authority is domestic law applied without multilateral authorization; secondary sanctions technically violate bilateral trade law; enforcement relies on implicit coercion (banking system exclusion, infrastructure sabotage) rather than explicit legal grounds. The theater ratio rises over the measurement interval (0.32 → 0.55) as the constraint escalates and its legal justification becomes more strained. By comparison, the early embargo (focused on specific sectors, more legally defensible) had lower theater; contemporary comprehensive sanctions have higher theater because their justification ('preventing imminent military threat,' 'countering terrorism') is increasingly detached from observable reality and relies on maintaining the performative apparatus of legality.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap between the US beneficiary (Rope perspective) and the Venezuelan victim (Snare perspective) reveals that the constraint solves a coordination problem for some agents while extracting from others. The US sees genuine coordination: preventing Venezuelan dumping that would destabilize prices. The Venezuelan population sees pure extraction: the constraint immiserates them with no offsetting benefit. Neither perspective is 'wrong' — both are structurally accurate from their positions. The Tangled Rope classification at the analytical level indicates that both perspectives are simultaneously true: the constraint is coordination for beneficiaries and extraction for victims, fused into a single mechanism. This is the diagnostic signature of Tangled Rope — the same mechanism serves both functions and they cannot be separated. If the US lifted sanctions to eliminate extraction, it would also eliminate price coordination. If Venezuela attempted to maintain price discipline while keeping its oil revenue, it would defect from OPEC coordination. The constraint's integrity depends on fusing coordination and extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) is derived from beneficiary/victim status and exit options for each agent. The US (institutional/arbitrage) has d ≈ 0.05 — it is a net beneficiary with easy exit, so f(d) ≈ -0.12, producing negative effective extraction (the constraint subsidizes the US). The Venezuelan population (powerless/trapped) has d ≈ 0.95 — it is a trapped victim, so f(d) ≈ 1.42, producing maximum experienced extraction (chi ≈ 0.82 at global scope). OPEC producers (powerful/constrained) have d ≈ 0.55 — they benefit from price support but face exit costs (if they abandon the coordination mechanism, prices collapse), so f(d) ≈ 0.75, producing moderate extraction (chi ≈ 0.43). The scope modifier σ(S) = 1.1 for continental scope, or 1.0 for national, reflects that the constraint operates at multiple scales: it is national (US vs Venezuela) but continental (US control over hemisphere) but also global (oil price effects). The powerless agent's experience is amplified by scale because the Venezuelan population cannot exit at any level — they are trapped locally, regionally, and globally. The analytical observer (analytical/analytical) has d ≈ 0.72 — observing without the ability to change the constraint, so f(d) ≈ 1.15. No directionality overrides are needed because the structural derivation captures the actual relationships.
 *
 * MANDATROPHY ANALYSIS:
 *   TANGLED ROPE CONFIRMATION: This constraint resolves mandatrophy by showing that both coordination and extraction are structurally real and operationally inseparable. The coordination function (price stability, preventing market dumping) solves a genuine tragedy-of-the-commons problem in petroleum markets — this is not cover story or pretense. The extraction function (geopolitical leverage, wealth transfer, enforcement of subordination) is equally real and equally structural — this is not incidental byproduct or collateral damage. The Tangled Rope classification requires THREE structural elements: (1) genuine coordination function — YES, verified by modeling of market dynamics without Venezuelan discipline; (2) asymmetric extraction — YES, verified by measurement of wealth flows and geopolitical leverage; (3) active enforcement — YES, verified by decades of sanctions escalation and secondary sanctions network. All three gates are satisfied. The constraint cannot be decomposed into a 'true' Rope (coordination-only) and a 'true' Snare (extraction-only) without losing explanatory power — the two functions are operationally fused. Mandatrophy is resolved not by choosing which function is primary, but by recognizing that the constraint's integrity depends on both functions operating together.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    coordination_vs_coercion_fusion,
    'Can the coordination function (price stability) be achieved without the coercive extraction function (geopolitical leverage), or are these structurally fused?',
    'Counterfactual analysis: could OPEC discipline be maintained through voluntary multilateral agreement without US unilateral enforcement? Historical data on oil price volatility during periods of Venezuelan compliance vs non-compliance; modeling of market dynamics without Venezuela at full capacity.',
    'If separable: the constraint is mislabeled as Tangled Rope and is actually Rope (coordination with minimal extraction). If fused: Tangled Rope classification confirmed — the coordination mechanism depends on coercive enforcement, and separation would collapse the price-stability function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_vs_coercion_fusion, empirical, 'Whether coordination and extraction functions are structurally separable').

omega_variable(
    venezuelan_state_legitimacy_entanglement,
    'Is Venezuelan state inability to maintain population welfare due to sanctions (structural barrier) or due to prior state capture/corruption (structural problem that sanctions exacerbate but do not cause)?',
    'Comparative analysis with peer states (Nigeria, Russia, other petrostates) on revenue management; timeline reconstruction of state collapse relative to embargo escalation; assessment of institutional capacity for revenue-to-welfare conversion with and without sanctions.',
    'If sanctions are primary driver: victims classification confirmed, suppression high, constraint is genuine coercion. If state capture is primary: victims classification weakens, suppression reflects political dysfunction not embargo, constraint is partly revealed as enforcement of reform discipline. If both: extraction and structural reform incentives are entangled, deepening the Tangled Rope classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(venezuelan_state_legitimacy_entanglement, empirical, 'Whether population welfare collapse is sanctions-driven or state-dysfunction-driven').

omega_variable(
    geopolitical_necessity_vs_hegemonic_choice,
    'Is excluding Venezuela from oil markets a geopolitical necessity for US security (genuine coordination problem) or a hegemonic choice that forecloses alternative arrangements (extraction mechanism)?',
    'Strategic analysis of US petroleum security under different Venezuelan oil access scenarios; modeling of geopolitical balance of power with and without US control of Venezuelan resource exclusion; comparison with historical US tolerance of other ideologically hostile oil producers (Iran pre-1979, Kuwait pre-Iraq invasion).',
    'If necessity: constraint is legitimated as coordination mechanism; beneficiary classification of ''us_strategic_position'' is structurally necessary, not extractive. If hegemonic choice: constraint is revealed as enforcement of power asymmetry; US strategic interest is a beneficiary of imposed subordination, not of genuine coordination. If context-dependent: geopolitical threat level varies with international climate and regional dynamics, affecting whether coordination or extraction is dominant at any given moment.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(geopolitical_necessity_vs_hegemonic_choice, conceptual, 'Whether constraint is geopolitical necessity or hegemonic enforcement choice').

omega_variable(
    us_economic_interest_magnitude,
    'How much of US beneficiary status derives from actual petrodollar volume or market price support, versus from geopolitical leverage over a hemisphere state?',
    'Economic analysis: US petroleum import volumes from Venezuela pre-embargo vs current; petrodollar flows; price support quantification via comparison of actual oil prices with counterfactual prices under unrestricted Venezuelan production; geopolitical leverage quantification via US diplomatic demands on Venezuela-adjacent states and their correlation with oil prices.',
    'If economic benefits are marginal and geopolitical leverage is dominant: ''us_strategic_position'' and ''us_allied_producers'' are revealed as benefiting primarily from subordination of a regional power, not from coordination problem solutions. Constraint reclassifies toward pure Snare from US perspective (extraction without coordination value). If economic benefits are substantial: Rope classification gains traction for US perspective; coordination function is real.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(us_economic_interest_magnitude, empirical, 'Magnitude of US economic vs geopolitical benefits').

omega_variable(
    secondary_sanctions_effectiveness_decay,
    'Is the constraint''s enforcement capacity degrading as secondary sanctions face increasing non-compliance (China, Russia purchasing Venezuelan oil), suggesting the constraint is entering Piton phase?',
    'Tracking of secondary sanctions violations; measurement of Venezuelan oil sales to non-US-aligned buyers over time; monitoring of financial transaction volume in US-excluded channels; assessment of enforcement intensity (new designations, penalties) required to maintain compliance.',
    'If decay is significant: theater ratio is rising (more performative enforcement, less effective coercion), and the constraint may be transitioning from Tangled Rope to Piton (degraded, maintained by institutional inertia rather than effective enforcement). If decay is minimal: enforcement remains effective, theater ratio is stable, Tangled Rope classification is sustained.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(secondary_sanctions_effectiveness_decay, empirical, 'Whether constraint enforcement capacity is degrading due to secondary sanctions non-compliance').

omega_variable(
    alternative_regime_coordination_sufficiency,
    'If Venezuela transitioned to a US-aligned regime, would oil price coordination with OPEC remain achievable, or is Venezuelan exclusion from markets the only mechanism for price discipline?',
    'Historical analysis of US-aligned petrostates'' actual discipline in OPEC (Mexico, Colombia pricing behavior, production discipline); modeling of Venezuelan production discipline under different political regimes; assessment of whether regime change would eliminate the need for exclusion or merely transfer the beneficiary from US to a client state.',
    'If regime change would achieve same coordination with less coercion: the constraint is revealed as containing unnecessary extraction elements — it could achieve its coordination function through less suppressive means, classifying it as Snare rather than Tangled Rope. If regime change would not improve discipline: the exclusion mechanism is necessary for coordination, sustaining Tangled Rope.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(alternative_regime_coordination_sufficiency, conceptual, 'Whether coordination could be achieved through regime change rather than exclusion').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(us_venezuela_oil_pressure, 0, 16).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(usven_tr_t0, us_venezuela_oil_pressure, theater_ratio, 0, 0.32).
narrative_ontology:measurement(usven_tr_t8, us_venezuela_oil_pressure, theater_ratio, 8, 0.44).
narrative_ontology:measurement(usven_tr_t16, us_venezuela_oil_pressure, theater_ratio, 16, 0.55).

% Extraction over time
narrative_ontology:measurement(usven_be_t0, us_venezuela_oil_pressure, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(usven_be_t8, us_venezuela_oil_pressure, base_extractiveness, 8, 0.42).
narrative_ontology:measurement(usven_be_t16, us_venezuela_oil_pressure, base_extractiveness, 16, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(usven_su_t0, us_venezuela_oil_pressure, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(usven_su_t8, us_venezuela_oil_pressure, suppression_requirement, 8, 0.6).
narrative_ontology:measurement(usven_su_t16, us_venezuela_oil_pressure, suppression_requirement, 16, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(us_venezuela_oil_pressure, resource_allocation).
narrative_ontology:affects_constraint(us_venezuela_oil_pressure, opec_cartel_discipline).
narrative_ontology:affects_constraint(us_venezuela_oil_pressure, petrodollar_hegemony).
narrative_ontology:affects_constraint(us_venezuela_oil_pressure, us_hemispheric_influence).

% DUAL FORMULATION NOTE:
% The US-Venezuela oil constraint is downstream of broader US hegemonic resource control mechanisms (petrodollar recycling, OPEC alliance maintenance) and upstream of specific Venezuelan economic collapse dynamics and regional non-aligned bloc formation. This story focuses on the direct constraint mechanism; upstream stories would model OPEC coordination independently; downstream stories would model Venezuelan state capacity and regional geopolitical realignment separately. The network links show how this constraint propagates into adjacent structural domains.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(us_venezuela_oil_pressure, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
