% ============================================================================
% CONSTRAINT STORY: energy_sufficiency_norm
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_energy_sufficiency_norm, []).

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
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: energy_sufficiency_norm
 *   human_readable: Energy Sufficiency Norm and Extraction
 *   domain: energy_policy/subsistence/political_economy
 *
 * SUMMARY:
 *   The energy sufficiency norm represents a global institutional arrangement
 *   defining adequate energy access as a basic right while simultaneously
 *   creating asymmetric extraction mechanisms that concentrate benefits among
 *   producers and industrial capital while suppressing alternatives and
 *   externalizing future costs. The constraint originated as a coordination
 *   mechanism solving real problems: preventing energy poverty, enabling
 *   economic development, and distributing resources across populations. Over
 *   decades, the norm has accumulated extractive functions layered atop the
 *   original coordination goal. Incumbent fossil fuel producers benefit from
 *   regulatory frameworks built around sufficiency narratives that lock in
 *   their centrality. Energy-poor populations remain trapped by
 *   infrastructure designed for centralized production and consumption
 *   patterns. Policy institutions experience identity fusion with fossil fuel
 *   paradigms. Meanwhile, renewable alternatives are being framed as threats
 *   to 'sufficiency' rather than as solutions, creating theatrical
 *   enforcement of outdated infrastructure. The constraint now exhibits all
 *   characteristics of a tangled rope: genuine coordination function (stable,
 *   universal energy access is real and valuable) combined with asymmetric
 *   extraction (benefits concentrated, costs externalized, alternatives
 *   suppressed).
 *
 * KEY AGENTS:
 *   - Energy-poor populations: Primary victims (powerless/trapped) — structurally immobile, bearing full cost of high prices and infrastructure dependency, no exit options
 *   - Energy producers and fossil fuel companies: Primary beneficiaries (institutional/arbitrage) — capture rents from sufficiency frameworks, regulatory protection, and carbon externality subsidies
 *   - Middle-income energy consumers: Secondary actors (moderate/constrained) — benefit from access but constrained by costs, utility monopolies, and inability to shift to alternatives
 *   - Climate stabilization movement: Organized challengers (organized/mobile) — see the norm as degraded, pushing for recognition that renewable alternatives solve the coordination problem
 *   - Renewable energy transition coalition: Organized beneficiaries of change (organized/constrained) — pushing sunset clause through technology and norms shift
 *   - Energy policy institutions: Institutional mediators (institutional/constrained) — captured by incumbent interests while nominally responsible for balancing multiple goals
 *   - Future generations: Structural victims (powerless/trapped) — bearing carbon costs that current energy sufficiency frameworks suppress from accounting
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(energy_sufficiency_norm, 0.58).
domain_priors:suppression_score(energy_sufficiency_norm, 0.65).
domain_priors:theater_ratio(energy_sufficiency_norm, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(energy_sufficiency_norm, extractiveness, 0.58).
narrative_ontology:constraint_metric(energy_sufficiency_norm, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(energy_sufficiency_norm, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(energy_sufficiency_norm, tangled_rope).
narrative_ontology:human_readable(energy_sufficiency_norm, "Energy Sufficiency Norm and Extraction").
narrative_ontology:topic_domain(energy_sufficiency_norm, "energy_policy/subsistence/political_economy").

domain_priors:requires_active_enforcement(energy_sufficiency_norm).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(energy_sufficiency_norm, energy_producers).
narrative_ontology:constraint_beneficiary(energy_sufficiency_norm, industrial_capital).
narrative_ontology:constraint_victim(energy_sufficiency_norm, energy_poor_populations).
narrative_ontology:constraint_victim(energy_sufficiency_norm, future_generation_carbon_budget).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ENERGY POOR (SNARE) — Trapped by material dependency on fossil fuel systems with no feasible exit. Cannot reduce consumption below subsistence levels, cannot access alternatives, and face extraction through both price volatility and infrastructure control. Maximum experienced extractiveness — structurally immobile, bearing all costs.
constraint_indexing:constraint_classification(energy_sufficiency_norm, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: MIDDLE-INCOME CONSUMERS (TANGLED ROPE) — Benefit from energy access enabling employment, transportation, heating; also bear extraction through utility monopolies, price controls that prevent market adjustment, and hidden carbon costs. Can reduce consumption at significant lifestyle cost. Mixed coordination and extraction.
constraint_indexing:constraint_classification(energy_sufficiency_norm, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: ENERGY PRODUCERS (ROPE) — Institutional beneficiaries experiencing the norm as pure coordination: provision of adequate energy supply to enable economic function. Low perceived extraction because the norm defines their role as positive contribution. Arbitrage options (shift to renewables, invest in grids) allow exit if incentives change.
constraint_indexing:constraint_classification(energy_sufficiency_norm, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: CLIMATE STABILIZATION MOVEMENT (PITON) — Sees the energy sufficiency norm as a degraded institutional framework maintained by inertia. The norm once solved real coordination problems (enable development, prevent destitution) but now persists despite recognition that the coordination function is being replaced by renewable infrastructure. Theater ratio high because much political effort goes to narrative maintenance ('reliable baseload') rather than solving actual grid problems. The movement has mobility but faces strong institutional resistance.
constraint_indexing:constraint_classification(energy_sufficiency_norm, piton,
    context(agent_power(organized),
            time_horizon(civilizational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: RENEWABLE ENERGY TRANSITION (SCAFFOLD) — Sees the current energy sufficiency framework as temporary, solvable through technology and norms shift. The constraint has a genuine sunset: distributed renewables, battery storage, demand flexibility create alternative pathways. Active enforcement (grid regulations, renewable mandates) is being redirected toward transition architecture. Extraction persists during transition but with explicit end date. Constraint-relative time horizon is generational because transition takes 15-30 years; exit options are constrained by lock-in and stranded assets.
constraint_indexing:constraint_classification(energy_sufficiency_norm, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: ENERGY POLICY INSTITUTIONS (TANGLED ROPE) — Institutional actors charged with balancing sufficiency, affordability, and sustainability. Experience the norm as both coordination mechanism (grid stability, universal access) and extractive constraint (regulatory capture by incumbent producers, path dependency in infrastructure investment, identity fusion with fossil fuel paradigm). Constrained exit because policy frameworks are locked into existing utility structures and carbon-intensive supply chains.
constraint_indexing:constraint_classification(energy_sufficiency_norm, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — Risks viewing energy sufficiency as inherent to human civilization rather than as a contingent institutional arrangement. The temptation: 'Humans need energy, therefore sufficiency norms are natural.' But the structural data contradicts this. The extractiveness (0.58), suppression (0.65), and active enforcement (true) indicate a contingent institutional choice, not a law of nature. This perspective instantiates the oracle gap: the analytical observer's own conceptual frame naturalizes what decomposed analysis reveals as extractive.
constraint_indexing:constraint_classification(energy_sufficiency_norm, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(energy_sufficiency_norm_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(energy_sufficiency_norm, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(energy_sufficiency_norm, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(energy_sufficiency_norm, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(energy_sufficiency_norm, TR),
    TR >= 0.70.

:- end_tests(energy_sufficiency_norm_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Elevated moderate-to-high. The energy sufficiency norm has evolved from a pure coordination mechanism into a hybrid structure. The coordination function remains real: universal energy access prevents destitution and enables economic participation. But extraction has accumulated through several mechanisms: (1) regulatory capture that locks in fossil fuel centrality, (2) pricing structures that allow monopoly rents, (3) suppression of alternatives (decentralized renewables, demand management), (4) externalization of carbon costs onto future generations, (5) narrative framing that naturalizes fossil fuel dependency as inherent to 'sufficiency.' The extractiveness trajectory shows increasing accumulation over the measured interval (0.32 → 0.58), consistent with regulatory capture dynamics where extraction layers accumulate over time. Suppression (0.65): High. Victims face multiple suppression mechanisms: (1) infrastructure lock-in (switching costs are high), (2) information suppression (renewable alternatives are narratively positioned as unreliable), (3) economic barriers (energy-poor cannot access alternatives even when technically available), (4) institutional barriers (policy favors incumbent producers), (5) cognitive suppression (the sufficiency narrative naturalizes the current system as necessary). Theater ratio (0.48): Moderate, indicating that roughly half of institutional activity is performative. Energy policy discourse contains significant theater: endless summit rhetoric about 'reliable baseload' and 'baseload necessity' despite technical evidence that renewable grids can be stable; performance of public consultation while decisions are predetermined; narrative maintenance of fossil fuel framing despite acknowledged climate imperatives. But some theater is lower than in degraded piton constraints because the coordination function is genuinely operational — the grid does need to function, sufficiency problems are real. The theater reflects capture and path dependency rather than pure institutional decay.
 *
 * PERSPECTIVAL GAP:
 *   The most severe perspectival gap is between the energy-poor (snare) and the energy producers (rope). The same constraint — the energy sufficiency norm — appears as pure predation from the powerless perspective and as pure coordination from the institutional perspective. This gap reflects real structural asymmetry: the norm solves the producer's problem (how to maintain centralized, profitable production) while creating the victim's problem (how to survive on inadequate access or high cost). The policy institutions occupy a middle position, experiencing genuine coordination function (grid stability, universal access is hard) but also experiencing extraction (locked into fossil fuel infrastructure, pressured by incumbent interests). The renewable transition coalition sees the constraint as piton (degraded ritual) while producers still see it as rope (living coordination), revealing that the coordination function is eroding but not yet dead. The climate-aware observer at civilizational scale risks naturalizing these arrangements as inevitable (mountain) rather than recognizing them as contingent institutional choices. The analytical oracle gap is sharp here: from outside the global energy system, 'sufficiency' appears as a necessity of civilization; from inside positions of extraction, it is a contingent power arrangement.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) is derived from each agent's structural position relative to extraction flow. Energy producers: high institutional power, arbitrage options, net beneficiary position → low d (0.10-0.20) → negative f(d) → low or negative experienced extraction χ. They perceive the constraint as rope because the extraction flows toward them. Energy-poor populations: low power, trapped exit → high d (0.90-0.95) → high f(d) → high χ. They perceive snare because they experience maximum extraction with no exit. Middle-income consumers: moderate power, constrained exit → intermediate d (0.55-0.65) → moderate f(d) → moderate χ. They perceive tangled rope because they both benefit (access) and pay costs (prices, restricted alternatives). Policy institutions: institutional power but identity-locked into fossil fuel paradigms, constrained exit from policy legacy → intermediate d (0.45-0.55) → moderate f(d) → moderate χ. They experience the constraint as tangled rope from their own perspective (both coordinate and extract) while being external extractors from the energy-poor perspective. Analytical observer at civilizational scope risks false summit by naturalizing the contingent institutional arrangements (d=0.72 canonical → mountain classification) but the structural data contradicts this: beneficiaries exist, victims exist, enforcement is active — all disqualify mountain.
 *
 * MANDATROPHY ANALYSIS:
 *   The energy sufficiency norm resolves the mandatrophy through structural differentiation by agent position and time horizon. Short-term perspectives dominated by immediate energy access needs (biographical or immediate) classify the constraint as snare (powerless) or rope (producers) — the extraction is real and present. Longer-term perspectives (generational, civilizational) can accommodate the scaffold view (transition is possible via renewable infrastructure, which has genuine sunset potential) or the piton view (the coordination function is eroding). The false summit (mountain) appears when the observer conflates the historical necessity of the sufficiency norm with its current contingency — the norm WAS a response to physical constraints (scarcity, capital requirements, centralization economies) but those constraints have shifted with renewable technology. The mandate trap is the temptation to see 'sufficiency' as inherently requiring fossil fuels, when the coordination problem (ensuring universal access to adequate energy) is technically solvable through distributed renewables. The classification portfolio confirms that the constraint is genuine tangled rope: it coordinates real access while extracting through lock-in, monopoly, and externalization.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sufficiency_threshold_definition,
    'What energy consumption level constitutes ''sufficiency'' — is it defined by basic needs, by competitive parity with high-consumption societies, or by infrastructure capacity?',
    'Cross-national comparative analysis of energy consumption patterns, welfare outcomes, and stated adequacy; identification of consumption threshold where wellbeing gains plateau',
    'If defined by basic needs (e.g., 2000 kWh/person/year): current global average is overconsumption, extraction is lower, and renewable transition is feasible. If defined by competitive parity (8000+ kWh/person/year): global sufficiency requires 3x current production, extraction is actually a coordination mechanism, and the snare classification collapses.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sufficiency_threshold_definition, conceptual, 'Definition of what constitutes adequate energy sufficiency').

omega_variable(
    carbon_internalization_mechanism,
    'Are future-generation climate costs being structurally suppressed from current decision-making, or is the energy sufficiency norm operating with honest accounting of carbon externalities?',
    'Comparison of energy infrastructure investment decisions under different carbon price scenarios; analysis of whether carbon cost internalization changes the energy sufficiency optimum',
    'If carbon costs are suppressed: the future-generation victim group is real but invisible, suppression value should be higher, and the snare classification is validated. If carbon costs are honestly priced in current decisions: the extraction is lower than measured, and the constraint becomes more like a legitimate rope with environmental tradeoffs.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(carbon_internalization_mechanism, empirical, 'Whether carbon costs are properly internalized in energy policy').

omega_variable(
    technological_lock_in_reversibility,
    'Is the fossil fuel infrastructure lock-in structurally irreversible or merely high-cost to reverse?',
    'Comparative analysis of jurisdiction transition costs (coal to renewable, gas to electric, etc.); identification of sunk assets and stranded investments; timeline and cost analysis for infrastructure replacement',
    'If irreversible: the trapped classification for energy-poor populations is structurally justified, and renewable transition prospects are overstated. If reversible at acceptable cost: scaffold perspective is validated, and the constraint has genuine sunset.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(technological_lock_in_reversibility, empirical, 'Reversibility of fossil fuel infrastructure lock-in').

omega_variable(
    renewables_intermittency_coordination_cost,
    'Does renewable infrastructure genuinely reduce coordination costs compared to fossil fuel centralization, or does it merely shift coordination problems from production to distribution and demand management?',
    'Comparative analysis of grid management complexity and cost under different energy mixes; study of real-world operational constraints in high-renewable grids (Denmark, South Australia, Costa Rica)',
    'If renewables reduce net coordination cost: the rope classification for producers is actually shifted toward snare as the coordination benefit erodes. If renewables shift but don''t reduce costs: the energy sufficiency norm persists under renewable infrastructure, and the scaffold''s sunset claim is weakened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(renewables_intermittency_coordination_cost, empirical, 'Whether renewables reduce overall grid coordination costs').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(energy_sufficiency_norm, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(esn_tr_t0, energy_sufficiency_norm, theater_ratio, 0, 0.35).
narrative_ontology:measurement(esn_tr_t2, energy_sufficiency_norm, theater_ratio, 2, 0.4).
narrative_ontology:measurement(esn_tr_t4, energy_sufficiency_norm, theater_ratio, 4, 0.44).
narrative_ontology:measurement(esn_tr_t6, energy_sufficiency_norm, theater_ratio, 6, 0.48).

% Extraction over time
narrative_ontology:measurement(esn_be_t0, energy_sufficiency_norm, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(esn_be_t2, energy_sufficiency_norm, base_extractiveness, 2, 0.41).
narrative_ontology:measurement(esn_be_t4, energy_sufficiency_norm, base_extractiveness, 4, 0.5).
narrative_ontology:measurement(esn_be_t6, energy_sufficiency_norm, base_extractiveness, 6, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(energy_sufficiency_norm, resource_allocation).
narrative_ontology:boltzmann_floor_override(energy_sufficiency_norm, 0.12).
narrative_ontology:affects_constraint(energy_sufficiency_norm, carbon_externality_suppression).
narrative_ontology:affects_constraint(energy_sufficiency_norm, energy_infrastructure_lock_in).
narrative_ontology:affects_constraint(energy_sufficiency_norm, utility_monopoly_pricing).

% DUAL FORMULATION NOTE:
% The energy sufficiency norm is upstream of three structurally distinct constraints: (1) carbon_externality_suppression (how climate costs are excluded from energy pricing, ε≈0.42), (2) energy_infrastructure_lock_in (stranded asset dynamics and switching costs, ε≈0.55), (3) utility_monopoly_pricing (rent extraction through regulatory protection, ε≈0.65). The norm influences all three by naturalizing the incumbent fossil fuel system as the baseline against which alternatives are evaluated. Each downstream constraint has its own extractiveness but shares the upstream sufficiency framing that makes them appear necessary rather than contingent.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(energy_sufficiency_norm, institutional, 0.48).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
