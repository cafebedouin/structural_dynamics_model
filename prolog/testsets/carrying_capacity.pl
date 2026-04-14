% ============================================================================
% CONSTRAINT STORY: carrying_capacity
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_carrying_capacity, []).

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
 *   constraint_id: carrying_capacity
 *   human_readable: Management of Ecological Carrying Capacity (K)
 *   domain: economic/technological/social
 *
 * SUMMARY:
 *   The management of ecological carrying capacity (K) represents the
 *   constraint governing maximum sustainable population and consumption
 *   levels within Earth's biophysical boundaries. This constraint exhibits
 *   the full range of DR classification types across different temporal and
 *   institutional perspectives. Industrial economies and high-consumption
 *   populations benefit from treating K as an expandable coordination problem
 *   (rope, institutional perspective) or a temporary challenge with
 *   technological solutions (scaffold). Future generations and ecosystem
 *   integrity bear the costs of overshoot with no exit option (snare). The
 *   sustainability reporting apparatus maintains the appearance of K
 *   management through performative metrics while actual extraction continues
 *   (piton). From a civilizational analytical perspective, K appears as an
 *   immutable physical law (mountain). Yet the structural mechanisms
 *   revealing the constraint are economic incentives, governance structures,
 *   and consumption norms — all human-controllable variables. The 50-100 year
 *   interval shows extraction rising from 0.35 to 0.58 and theater rising
 *   from 0.35 to 0.62, indicating that as material impacts become undeniable,
 *   institutional responses emphasize reporting and targets over structural
 *   change.
 *
 * KEY AGENTS:
 *   - Industrial Economies: Primary beneficiary (institutional/arbitrage) — capture resource value and growth benefits during overshoot; have technology substitution options
 *   - High-Consumption Populations: Beneficiary class (powerful/arbitrage) — enjoy consumption levels enabled by K overshoot; can substitute through price and technology
 *   - Extraction Industries: Institutional beneficiary (institutional/arbitrage) — capture short-term rents from resource depletion; fund enforcement of property rights
 *   - Future Generations: Primary victim (powerless/trapped) — inherit depleted resource base; cannot exit or renegotiate terms
 *   - Ecosystem Integrity: Victim (powerless/trapped) — absorbs external costs of overshoot; has no negotiating power or representation
 *   - Resource-Dependent Populations: Mixed victim-participant (moderate/constrained) — depend on local ecosystem services but also participate in extraction decisions through subsistence and market participation
 *   - Conservation Coalition: Organized agents (organized/constrained) — see carrying-capacity management as solvable through protected areas, sustainable development, and technology transition; experience constraint as temporary
 *   - Sustainability Reporting Apparatus: Institutional actor (institutional/arbitrage) — maintains measurement fiction; benefits from continuation of status quo while appearing to manage it
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing contingent extraction patterns as immutable physical laws
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(carrying_capacity, 0.58).
domain_priors:suppression_score(carrying_capacity, 0.68).
domain_priors:theater_ratio(carrying_capacity, 0.62).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(carrying_capacity, extractiveness, 0.58).
narrative_ontology:constraint_metric(carrying_capacity, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(carrying_capacity, theater_ratio, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(carrying_capacity, tangled_rope).
narrative_ontology:human_readable(carrying_capacity, "Management of Ecological Carrying Capacity (K)").
narrative_ontology:topic_domain(carrying_capacity, "economic/technological/social").

domain_priors:requires_active_enforcement(carrying_capacity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(carrying_capacity, industrial_economies).
narrative_ontology:constraint_beneficiary(carrying_capacity, high_consumption_populations).
narrative_ontology:constraint_beneficiary(carrying_capacity, extraction_industries).
narrative_ontology:constraint_victim(carrying_capacity, future_populations).
narrative_ontology:constraint_victim(carrying_capacity, ecosystem_integrity).
narrative_ontology:constraint_victim(carrying_capacity, biodiversity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: FUTURE GENERATION (SNARE) — Cannot exit the constraint; bears the full cost of carrying-capacity overshoot through degraded resource availability, climate instability, and reduced ecological services. No negotiating power or arbitrage option. Maximum experienced extraction — inherits depleted commons.
constraint_indexing:constraint_classification(carrying_capacity, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: RESOURCE-DEPENDENT RURAL POPULATION (TANGLED ROPE) — Constrained by dependence on local ecosystem services (water, fisheries, forestry) but also participates in extraction decisions through subsistence practices. Benefits from access to productive land; bears costs of degradation. Mixed coordination-extraction: genuine local coordination needs coexist with top-down resource allocation that extracts value toward industrial centers.
constraint_indexing:constraint_classification(carrying_capacity, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: INDUSTRIAL ECONOMY BENEFICIARY (ROPE) — Institutional actor (corporations, high-income nations) experiences carrying capacity as a coordination problem: managing resource extraction efficiency to maintain supply chains. Has arbitrage options (substitute materials, technological efficiency, geographic diversification). Net beneficiary during overshoot period — extracts value while externalizing ecosystem costs. Low suppression from their perspective because they control the enforcement mechanisms.
constraint_indexing:constraint_classification(carrying_capacity, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: CONSERVATION COALITION (SCAFFOLD) — Organized agents (IUCN, conservation NGOs, some governments) frame carrying-capacity management as a temporary coordination problem with a sunset: sustainable development targets, marine protected areas, and rewilding programs represent transition pathways toward equilibrium within K. Has agency and sees exit toward lower-extraction equilibrium. Theater is lower than status quo because conservation mechanisms emphasize measurable ecological targets rather than performative resource accounting.
constraint_indexing:constraint_classification(carrying_capacity, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: SUSTAINABILITY REPORTING APPARATUS (PITON) — ESG reporting, carbon accounting, biodiversity indices, and 'sustainable development' frameworks maintain the appearance of carrying-capacity management while doing minimal actual enforcement. Theater ratio high (0.62): corporations publish sustainability reports while expanding extraction. The reporting ritual persists through institutional inertia — required by regulation and investor pressure — but has degraded from any original coordination function. Performative measurement substitutes for structural change.
constraint_indexing:constraint_classification(carrying_capacity, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / PHYSICAL CONSTRAINTS VIEW (MOUNTAIN) — From a civilizational perspective, carrying capacity is a physical law: a finite biosphere can support a finite total metabolic throughput. Overshoot by definition leads to degradation — this is not contingent on policy or technology. However, the structural data contradicts the mountain classification. Actual K depends on consumption patterns, technology, and waste resorption capacity — all human-controllable variables. The 'immutable physical law' framing naturalizes what is actually a contingent set of choices about extraction rates and technology deployment.
constraint_indexing:constraint_classification(carrying_capacity, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(carrying_capacity_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(carrying_capacity, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(carrying_capacity, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(carrying_capacity, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(carrying_capacity, TR),
    TR >= 0.70.

:- end_tests(carrying_capacity_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): The constraint exhibits significant extraction because overshoot wealth accumulates to present-generation beneficiaries while costs defer to future generations and ecosystem integrity (both powerless/trapped). Industrial economies arbitrage the gap between internal sustainable-K and external apparent-K through geographic resource mobility and temporal deferral. The 0.35→0.58 trajectory reflects increasing material impact forcing higher institutional extraction effort to maintain the overshoot. Suppression (0.68): Strong suppression mechanisms include information asymmetry (complex systems dynamics are difficult to predict), institutional capture (extraction industries fund enforcement policy), property-right enforcement (nations defend resource access), and consumption-norm normalization (high-extraction lifestyles appear inevitable). Yet suppression is not total (0.68 not 0.90) because: alternative pathways exist (renewable technology, conservation), awareness is growing (climate science, biodiversity monitoring), and some populations have exit options (wealthy nations can transition). Theater ratio (0.62): Sustainability reporting, carbon accounting, ESG metrics, and 'net-zero' commitments represent significant performative activity. Many sustainability initiatives measure inputs (renewable capacity) rather than actual outcomes (emissions reductions, material throughput reduction). The 0.35→0.62 trajectory shows theater rising as material impacts become undeniable — institutions respond with more measurement and targets rather than structural change.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap is extreme. Industrial beneficiaries experience rope (coordination) or scaffold (temporary problem with technology solution). Future generations and ecosystems experience snare (trapped, no exit). The sustainability apparatus experiences piton (performative ritual). The analytical observer risks experiencing mountain (immutable law). These are not disagreements about facts but different structural positions experiencing the same biophysical constraint differently. Beneficiaries have genuine arbitrage options (substitute materials, efficiency, geographic diversification, technology timing) that appear as real problem-solving from their perspective. Trapped agents have no options and see pure extraction. The gap reveals that 'carrying capacity management' is not a neutral technical problem but an allocation problem: who extracts value during overshoot, who bears costs, and who has exit options?
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality is determined by structural position relative to extraction flows. Industrial beneficiaries with arbitrage exits derive low d values (0.05-0.20): they benefit from the constraint and can exit through technology or substitution. Their experienced extractiveness (chi) is low or negative — the constraint subsidizes them. Trapped agents (future generations, ecosystems) derive high d values (0.90-0.95): they bear full extraction cost with zero exit option. Their experienced extractiveness is maximum. Resource-dependent populations are mixed: they benefit from local resource access (low d component) but are also constrained by degradation (high d component), producing moderate d around 0.55-0.65. Conservation coalitions are organized with constrained (not trapped) exits — they see real alternative pathways (protected areas, rewilding, sustainable-yield management), producing moderate-low d (0.35-0.45). The piton perspective derives from performance theater ratio rather than from high experienced extraction — the sustainability apparatus has low actual chi because it has arbitrage exits (can switch targets, redefine metrics, perform without enforcing).
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by showing that different institutions genuinely perceive different classifications from the same biophysical facts. The industrial economy's rope classification is real from their structural perspective: they ARE solving a coordination problem (how to allocate scarce resources across competing uses; how to synchronize extraction timing). The future generation's snare classification is also real: they ARE trapped with no options. The piton classification is real: sustainability reporting IS performative (most ESG improvements are greenwashing; carbon targets are missed; biodiversity continues declining despite protected area expansion). The mountain classification is the false summit: it naturalizes as 'immutable physical law' what is actually a contingent institutional arrangement. The resolution is not to pick one correct type but to show that mandatrophy analysis reveals the deep structure: carrying-capacity constraint is actually a DISTRIBUTION constraint — the biophysical floor (absolute K) may or may not be hit soon, but the extraction-incentive structure ensures that present-generation beneficiaries capture value while costs defer to future populations and ecosystems. The 'mandatrophy' is the attempt to frame this as a purely technical problem (can we increase K through technology?) rather than as an allocation problem (who gets to extract, who bears costs, can the trapped agents organize?).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    technological_substitution_feasibility,
    'Can renewable energy, circular economy, and efficiency technologies expand effective carrying capacity (K_effective) sufficiently to accommodate projected population and consumption growth?',
    'Long-term resource accounting: actual renewable resource depletion rates vs. theoretical substitution models; empirical scaling laws for renewable transition timelines; entropic analysis of circular economy closure rates',
    'If feasible (K_effective can grow): carrying-capacity constraint becomes primarily a coordination problem (rope, scaffold). If infeasible: constraint remains fixed or declining (snare, mountain). This determines whether the industrial economy''s arbitrage exit is real or illusory.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(technological_substitution_feasibility, empirical, 'Feasibility of technological expansion of carrying capacity').

omega_variable(
    consumption_pattern_flexibility,
    'Are current high-consumption patterns in wealthy economies economically and socially necessary, or do they reflect contingent institutional arrangements that could be restructured?',
    'Comparative analysis of consumption levels, wellbeing metrics, and social satisfaction across income levels and cultures; identification of status competition vs. utility-driven consumption; historical analysis of how consumption norms change',
    'If necessary: carrying-capacity management requires technology scaling (mountain-like constraint). If contingent: redistribution and norm-change become viable — constraint shifts to tangled_rope with different beneficiaries/victims.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(consumption_pattern_flexibility, conceptual, 'Necessity vs. contingency of consumption patterns').

omega_variable(
    ecosystem_resilience_threshold,
    'At what level of overshoot do ecosystem collapse dynamics become self-reinforcing and irreversible?',
    'Paleoclimatic data on ecosystem state transitions; modeling of feedback loops (permafrost methane, ocean circulation, Amazon tipping points); empirical monitoring of leading indicators for ecosystem collapse',
    'If threshold is far above current overshoot: snare classification premature. If threshold is close or already crossed: snare classification is understated — constraint may already be physically mountain-like (irreversible). Timing determines whether scaffold sunset is realistic.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ecosystem_resilience_threshold, empirical, 'Distance to ecosystem collapse thresholds').

omega_variable(
    enforcement_capacity_asymmetry,
    'Can enforcement mechanisms (regulation, pricing, monitoring) actually constrain extraction behavior or do they represent pure performance theater?',
    'Empirical measurement: carbon pricing and actual emissions reductions; protected area designation and actual habitat loss prevention; resource quota enforcement and actual catch/extraction rates',
    'If enforcement effective: suppression is real constraint; classification stands. If enforcement theater: suppression is illusory; actual constraint is information-flow based (does extraction community know K? do they care?); classification shifts toward piton.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(enforcement_capacity_asymmetry, empirical, 'Real vs. performative enforcement capacity').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(carrying_capacity, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cc_tr_t0, carrying_capacity, theater_ratio, 0, 0.35).
narrative_ontology:measurement(cc_tr_t50, carrying_capacity, theater_ratio, 50, 0.5).
narrative_ontology:measurement(cc_tr_t100, carrying_capacity, theater_ratio, 100, 0.62).

% Extraction over time
narrative_ontology:measurement(cc_be_t0, carrying_capacity, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(cc_be_t50, carrying_capacity, base_extractiveness, 50, 0.48).
narrative_ontology:measurement(cc_be_t100, carrying_capacity, base_extractiveness, 100, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(carrying_capacity, resource_allocation).
narrative_ontology:affects_constraint(carrying_capacity, climate_stability).
narrative_ontology:affects_constraint(carrying_capacity, biodiversity_loss).
narrative_ontology:affects_constraint(carrying_capacity, freshwater_depletion).
narrative_ontology:affects_constraint(carrying_capacity, nutrient_cycling).

% DUAL FORMULATION NOTE:
% Carrying capacity as a constraint family decomposes into domain-specific carrying-capacity constraints: fishery-K (marine resource renewal), agricultural-K (soil fertility and water), forest-K (tree growth and carbon sequestration), aquifer-K (groundwater renewal), and atmosphere-K (carbon absorption). Each has its own epsilon (fishery overshoot around 0.65, forest overshoot around 0.48, atmosphere-K near 0.75). This story concerns the meta-constraint governing all domain-specific K values. The network shows how degradation in one domain (fishery collapse, soil depletion, forest loss, aquifer exhaustion) cascades to others through ecosystem coupling.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(carrying_capacity, organized, 0.4).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
