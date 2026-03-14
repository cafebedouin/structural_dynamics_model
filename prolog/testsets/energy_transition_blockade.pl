% ============================================================================
% CONSTRAINT STORY: energy_transition_blockade
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_energy_transition_blockade, []).

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
 *   constraint_id: energy_transition_blockade
 *   human_readable: Energy Transition Blockade: Fossil Fuel Lock-In and Renewable Infrastructure Extraction
 *   domain: energy_policy/political_economy
 *
 * SUMMARY:
 *   The energy transition blockade represents a structural constraint that
 *   locks economies into fossil fuel infrastructure and slows renewable
 *   deployment despite falling renewable costs and accelerating climate
 *   impacts. The blockade operates through multiple mechanisms: regulatory
 *   capture (fossil fuel interests block renewable permitting and grid
 *   access), financial lock-in (stranded asset owners lobby against rapid
 *   transition), labor capture (incumbent unions resist diversification), and
 *   theater governance (policymakers announce climate commitments while
 *   maintaining status quo policy). From different structural positions, the
 *   same constraint appears as an immutable feature of industrial economies
 *   (mountain), a temporary coordination problem being solved by policy
 *   (scaffold), a degraded regulatory system (piton), a mixed
 *   extraction-coordination hybrid (tangled rope), or pure extraction from
 *   vulnerable populations (snare). The extractiveness and theater metrics
 *   show increasing trend over the first two decades (0.35→0.58, theater
 *   0.42→0.68) as commitment-action gap widened and regulatory theater
 *   intensified, followed by slight decline (extractiveness 0.52, theater
 *   0.55 at year 30) as renewable costs fell below fossil fuel costs
 *   globally, eroding the blockade's structural basis. The constraint
 *   exhibits all hallmarks of Tangled Rope: genuine coordination function
 *   (managing complex grid, workforce transition, investment scaling) paired
 *   with asymmetric extraction (incumbent rents protected, renewable
 *   developers constrained, vulnerable populations bear cost).
 *
 * KEY AGENTS:
 *   - Climate-Constrained Populations: Primary victim (powerless/trapped) — subsistence communities, low-income nations bearing full cost of delayed transition with no exit options; geographical immobility, economic dependency on climate-vulnerable sectors
 *   - Renewable Energy Developers: Secondary victim (moderate/constrained) — face permitting delays, grid access barriers, financing discrimination; constrained by capital requirements and regulatory timelines but capable of organizing for policy change
 *   - Incumbent Fossil Fuel Producers: Primary beneficiary (institutional/arbitrage) — capture rents from stranded asset discounts, maintain infrastructure utilization, diversify arbitrage options; can exit fossil fuel entirely (some major oil/gas companies actively transitioning)
 *   - Incumbent-Aligned Labor: Mixed victim/beneficiary (organized/constrained) — fossil fuel workers receive current employment coordination but face extraction of future employment pathways through resistance to just transition investment
 *   - Climate Justice Movements: Organized pushback agents (organized/constrained) — mobilizing for policy change to enforce transition; see blockade as temporary (Scaffold). Success (IRA, EU Green Deal) suggests sunset mechanism is real, though execution remains contested
 *   - Regulatory/Financial Institutions: Theater maintainers (institutional/arbitrage) — announce climate commitments while preserving incumbent access; maintain legitimacy without enforcing transition
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing political blockade as inherent thermodynamic or economic constraint
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(energy_transition_blockade, 0.58).
domain_priors:suppression_score(energy_transition_blockade, 0.65).
domain_priors:theater_ratio(energy_transition_blockade, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(energy_transition_blockade, extractiveness, 0.58).
narrative_ontology:constraint_metric(energy_transition_blockade, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(energy_transition_blockade, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(energy_transition_blockade, tangled_rope).
narrative_ontology:human_readable(energy_transition_blockade, "Energy Transition Blockade: Fossil Fuel Lock-In and Renewable Infrastructure Extraction").
narrative_ontology:topic_domain(energy_transition_blockade, "energy_policy/political_economy").

domain_priors:requires_active_enforcement(energy_transition_blockade).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(energy_transition_blockade, incumbent_fossil_fuel_producers).
narrative_ontology:constraint_beneficiary(energy_transition_blockade, stranded_infrastructure_investors).
narrative_ontology:constraint_beneficiary(energy_transition_blockade, regulatory_capture_agents).
narrative_ontology:constraint_victim(energy_transition_blockade, renewable_energy_developers).
narrative_ontology:constraint_victim(energy_transition_blockade, climate_constrained_populations).
narrative_ontology:constraint_victim(energy_transition_blockade, future_generations).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CLIMATE-CONSTRAINED POPULATIONS (SNARE) — Low-income nations and subsistence-dependent communities bear maximum cost of delayed transition: intensifying extreme weather, agricultural collapse, resource scarcity. No structural exit: geographically immobile, economically dependent on climate-vulnerable sectors, politically powerless in global energy negotiations. Experience extraction without any coordination benefit. Maximum experienced extractiveness.
constraint_indexing:constraint_classification(energy_transition_blockade, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: RENEWABLE ENERGY DEVELOPERS (TANGLED ROPE) — Face genuine coordination challenge: grid integration, storage, supply-chain coordination, financing mechanisms. But also face extractive barriers: preferential grid access for incumbent generators, subsidies locked into fossil fuel infrastructure, regulatory delay-and-denial tactics. Constrained by capital requirements, permitting timelines, political opposition. Some coordination function (the renewable sector genuinely solves collective electrification problems) but asymmetric extraction (incumbents capture rents from the transition).
constraint_indexing:constraint_classification(energy_transition_blockade, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: INCUMBENT FOSSIL FUEL PRODUCERS (ROPE) — Institutional actors with arbitrage options: can diversify into renewables, exit fossil fuels (as some majors are doing), or shift geographic markets. Experience the transition as a coordination problem (managing stranded assets, workforce transition) with immediate exit options available. Sees constraint as manageable coordination, not extraction. Low effective extraction from this perspective.
constraint_indexing:constraint_classification(energy_transition_blockade, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: CLIMATE JUSTICE MOVEMENTS (SCAFFOLD) — Organized agents pushing for mandated transition timelines, just transition programs, carbon pricing, and infrastructure investment. See the blockade as a temporary problem solvable through policy mobilization. Sunset clause implicit: as climate pressure mounts and renewable costs fall, the structural basis for the blockade erodes. Constrained by political economy but with clear exit trajectory: legislative wins, technological inevitability of renewables cost curve.
constraint_indexing:constraint_classification(energy_transition_blockade, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: REGULATORY/FINANCIAL INSTITUTIONS (PITON) — Central banks, securities regulators, energy agencies maintain transition theater: climate commitments, ESG mandates, Net Zero pledges with no enforcement. Actual function atrophied — institutions are captured by incumbent interests and cannot enforce transition against market power. Continued through inertia and institutional legitimacy maintenance, not because the institutions work. Theater ratio high: performative climate action masking the structural blockade.
constraint_indexing:constraint_classification(energy_transition_blockade, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: INCUMBENT-ALIGNED LABOR (TANGLED ROPE) — Workers in fossil fuel industries experience the blockade as providing genuine coordination of their employment and income (coal mining towns, oil rig workers). But also face extraction: operators resist just transition investment, capture union leadership, prevent workers from sharing in the renewable sector's job growth. Constrained: retraining costs, geographic immobility, pension dependencies. Mixed: the blockade coordinates their immediate survival but extracts from their future by delaying alternative employment pathways.
constraint_indexing:constraint_classification(energy_transition_blockade, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational timescale, the constraint appears as an immutable property of industrial economies: transition requires simultaneous coordination of supply, demand, investment, and international agreements; transition has inherent friction and delay. However, this naturalizes a contingent political choice. The structural data contradicts the mountain classification — the blockade is maintained through active regulatory enforcement and financial capture, not through laws of thermodynamics or economics.
constraint_indexing:constraint_classification(energy_transition_blockade, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(energy_transition_blockade_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(energy_transition_blockade, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(energy_transition_blockade, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(energy_transition_blockade, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(energy_transition_blockade, TR),
    TR >= 0.70.

:- end_tests(energy_transition_blockade_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The blockade extracts from climate-vulnerable populations through delayed mitigation (costs externalized to future and to Global South). Extracts from renewable developers through regulatory barriers and financing discrimination. But extraction is not absolute — renewable costs are falling faster than transition timelines increase (the 'crossover year' phenomenon where renewables become cheapest energy source). Moderate value reflects that the blockade's structural power is eroding even as its theater intensifies. Suppression (0.65): High. Barriers include regulatory gatekeeping (permitting timelines, grid access denial), financial barriers (fossil fuel subsidy lock-in, stranded asset risk on renewable lending), political barriers (incumbent lobbying dominance), and labor barriers (incumbent unions resisting diversification). But suppression is incomplete — some countries achieve 80%+ renewable penetration (Denmark, Costa Rica, Uruguay), proving exit is possible. Suppression measures institutional barriers, not technological inevitability. Theater ratio (0.68): Moderately high. Climate commitments (Net Zero pledges, ESG mandates, Paris Agreement) are largely performative — governments announce transitions while maintaining fossil fuel subsidies and preventing renewable permitting. Regulatory theater increases as fossil fuel market share declines and legitimacy crisis deepens. However, theater is not total — renewable deployment does accelerate, and some policies are substantive rather than merely theatrical. Measurement trend shows extractiveness rising (0.35→0.58) through mid-period as incumbent resistance intensifies, then slight decline (0.52) as renewable cost inevitability overcomes political blockade.
 *
 * PERSPECTIVAL GAP:
 *   Why do these perspectives contradict? Because they occupy genuinely different structural positions relative to the extraction flow. The incumbent benefits from delay; the vulnerable population bears its cost. The developer faces barriers but can organize; the vulnerable population cannot exit. The analysis that 'transition is hard for everyone' (mountain view) obscures the asymmetry: transition is hard for some agents in ways that extract from others. The gap is not perceptual — it is structural.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary groups are identified as incumbent fossil fuel producers and stranded asset investors who capture rents from blockade maintenance — their directionality toward the blockade is beneficiary (negative extraction flow). Victim groups are renewable developers (facing permitting, financing, grid barriers), climate-vulnerable populations (bearing delay costs), and future generations (locked into higher warming). The constraint is asymmetric: beneficiaries have arbitrage options (diversify into renewables, relocate operations, lobby for protection), victims face suppression (regulatory barriers, financing discrimination, geographical immobility). This asymmetry drives the Tangled Rope classification: there is a genuine coordination function (transition infrastructure, worker retraining, grid scaling) but execution extracts from those least able to escape (vulnerable populations, renewable startups) and protects those most able to escape (incumbent majors with diversification options).
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint exhibits classic Tangled Rope mandatrophy: requires explicit distinction between coordination and extraction components to prevent mislabeling as pure coordination (Rope) or pure extraction (Snare). At face value, the transition looks like pure coordination — everyone must solve the same supply-demand problem together. But the asymmetric extraction is real: incumbent actors use regulatory gatekeeping to slow their own necessary transition, extracting rents from the delay, while vulnerable populations and future generations bear the cost of delay (carbon lock-in). The Tangled Rope classification prevents false Rope (which would imply the blockade is coordination-only, ignoring extraction) and false Snare (which would imply there is no coordination function, ignoring that the grid and workforce DO require coordinated transition). The mandatrophy is resolved by requiring both genuine coordination (beneficiaries + enforcement), genuine extraction (victims + asymmetry), and the gate conditions (0.40 ≤ χ ≤ 0.90). At χ ≈ 0.60-0.75 depending on perspective, the constraint sits in the Tangled Rope range, confirming the classification.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    incumbent_adaptation_vs_resistance,
    'Will incumbent fossil fuel actors genuinely transition to renewables (Rope narrative) or continuously resist to extract rents from the stranded asset discount (Snare narrative)?',
    'Historical analysis of major oil & gas companies'' renewable investment patterns; tracking of whether renewables capacity substitutes for or supplements fossil fuel operations; measurement of lobbying spend and carbon intensity of corporate portfolios over time',
    'If adaptation: blockade is temporary coordination problem (Scaffold accurate). If resistance: blockade is structural extraction (Snare accurate). Current evidence mixed — some companies diversify, others double down on fossils.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(incumbent_adaptation_vs_resistance, empirical, 'Whether incumbents genuinely transition or extract through resistance').

omega_variable(
    grid_integration_complexity_threshold,
    'Does renewable grid integration create genuinely irreducible coordination complexity (suggesting Rope) or is the complexity primarily a pretext for regulatory delay and incumbent gatekeeping (suggesting Snare)?',
    'Comparative analysis of grid stability in high-renewable regions (Denmark 80% wind, Uruguay 98% renewables, Costa Rica periods >99%); identification of whether grid problems are technical or political (permitting delays, insufficient storage investment, unnecessary frequency reserves)',
    'If genuinely irreducible: Rope classification for grid-integration constraint is correct. If primarily political: Snare or Tangled Rope classification accurate; the blockade is extractive theater, not technical necessity.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(grid_integration_complexity_threshold, empirical, 'Whether grid complexity is technical or political').

omega_variable(
    just_transition_funding_availability,
    'Is the transition blockade driven by insufficient capital for renewable buildout and worker retraining (true coordination gap) or by political unwillingness to redistribute incumbent rents (extraction mechanism)?',
    'Comparison of renewable buildout rates in countries with high transition funding (Germany, Denmark green bonds; US IRA investments) vs those with low funding; analysis of whether capital constraints or regulatory barriers limit renewable deployment in well-funded contexts',
    'If capital constraints: Rope/Scaffold narrative correct — blockade is solvable through investment. If political capture: Snare/Tangled Rope narrative correct — blockade persists despite available capital because incumbents use regulation to block competition.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(just_transition_funding_availability, empirical, 'Whether blockade is capital-constrained or politically-enforced').

omega_variable(
    climate_tipping_point_timeline,
    'Will climate tipping points (permafrost melt feedback, Amazon dieback, AMOC collapse) force rapid emergency transition before the Scaffold sunset can operate (legislative victory timeline), converting the blockade into permanent structural damage?',
    'Paleoclimate tipping-point threshold analysis; comparison of transition timeline required to avoid tipping vs actual policy-enabled transition timelines; tracking of leading indicators (permafrost carbon release, tropical forest dieback, polar ice loss rates)',
    'If tipping points trigger before legislative success: the Scaffold''s sunset fails; the blockade''s extractive cost becomes catastrophic and irreversible. If transition accelerates ahead of tipping points: Scaffold narrative holds; blockade is temporary.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(climate_tipping_point_timeline, empirical, 'Whether tipping points force transition before policy-driven sunset').

omega_variable(
    labor_transition_credibility,
    'Are just transition and green job promises from policymakers credible commitments (supporting Scaffold perspective) or performative theater that will be abandoned post-transition (converting Scaffold into Piton)?',
    'Historical tracking of transition funding delivery; comparison of promised vs actual retraining programs and income support; longitudinal wage and employment data for workers in regions transitioning away from fossil fuels (Appalachia coal closure, oil rig closures, coal plant retirements)',
    'If credible: Scaffold''s sunset with just transition is real. If theater: Incumbent-aligned labor will remain trapped (Snare) or constrained (Tangled Rope); labor exit from the blockade becomes extraction rather than coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(labor_transition_credibility, empirical, 'Whether just transition commitments are credible or performative').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(energy_transition_blockade, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(etb_tr_t0, energy_transition_blockade, theater_ratio, 0, 0.42).
narrative_ontology:measurement(etb_tr_t10, energy_transition_blockade, theater_ratio, 10, 0.62).
narrative_ontology:measurement(etb_tr_t20, energy_transition_blockade, theater_ratio, 20, 0.68).
narrative_ontology:measurement(etb_tr_t30, energy_transition_blockade, theater_ratio, 30, 0.55).

% Extraction over time
narrative_ontology:measurement(etb_be_t0, energy_transition_blockade, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(etb_be_t10, energy_transition_blockade, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(etb_be_t20, energy_transition_blockade, base_extractiveness, 20, 0.58).
narrative_ontology:measurement(etb_be_t30, energy_transition_blockade, base_extractiveness, 30, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(energy_transition_blockade, resource_allocation).
narrative_ontology:affects_constraint(energy_transition_blockade, stranded_asset_financial_system).
narrative_ontology:affects_constraint(energy_transition_blockade, fossil_fuel_subsidy_lock_in).
narrative_ontology:affects_constraint(energy_transition_blockade, grid_integration_gatekeeping).
narrative_ontology:affects_constraint(energy_transition_blockade, just_transition_promises).

% DUAL FORMULATION NOTE:
% The energy transition blockade decomposes into four structurally distinct constraints: (1) stranded asset financial risk (banks exposed to fossil fuel write-downs; ε≈0.35, Mountain/Rope depending on perspective); (2) fossil fuel subsidy lock-in (persistent government support for incumbent fuel extraction; ε≈0.50, Tangled Rope); (3) grid integration gatekeeping (regulatory barriers to renewable grid access; ε≈0.45, Tangled Rope); (4) just transition promises (labor transition credibility; ε≈0.55, Scaffold/Piton). Each has different extractiveness and different beneficiary/victim profiles. The blockade story models the aggregate extractive effect across all four mechanisms.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(energy_transition_blockade, institutional, 0.18).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
