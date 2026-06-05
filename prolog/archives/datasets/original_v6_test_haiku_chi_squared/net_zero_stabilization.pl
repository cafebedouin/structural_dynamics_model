% ============================================================================
% CONSTRAINT STORY: net_zero_stabilization
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_net_zero_stabilization, []).

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
 *   constraint_id: net_zero_stabilization
 *   human_readable: The Net Zero Carbon Constraint
 *   domain: climate/energy/political_economy
 *
 * SUMMARY:
 *   The net-zero carbon constraint embodies a structural tension between
 *   climate stabilization as a genuine coordination problem and climate
 *   action as a mechanism for extracting continued development advantages for
 *   high-emission economies. Before 2005, scientific framings portrayed a
 *   'small carbon budget' compatible with stabilization — implicitly
 *   distributing that budget unequally, with wealthy nations capturing most
 *   of the permitted emissions while developing nations accepted constrained
 *   growth. Since then, the constraint has evolved from a relatively
 *   transparent climate-physics problem into a complex hybrid mechanism
 *   mixing real climate coordination (reducing emissions is necessary) with
 *   asymmetric extraction (permits for wealthy nations to continue
 *   high-emission development while poor nations accept development
 *   restrictions, at least until 2050). The constraint exhibits all six DR
 *   types: powerless island nations experience it as a snare, developing
 *   economies as tangled rope, renewable sectors as rope, incumbent fossil
 *   fuel industries as rope with active enforcement suppressing alternatives,
 *   climate justice coalitions as a scaffold with sunset potential,
 *   international governance as a degraded piton ritual, and analytical
 *   observers risk naturalizing it as an immutable law when it is actually a
 *   contingent political arrangement. The extractiveness has grown from 0.32
 *   (2005: relatively narrow technical question about carbon budgets) to 0.58
 *   (2026: major structural question about which nations can emit and which
 *   must constrain). Theater ratio has similarly increased from 0.38 to 0.65,
 *   reflecting growing disconnection between climate pledges and actual
 *   emissions trajectories, and between 'net-zero by 2050' promises and the
 *   physics-required reductions before 2035.
 *
 * KEY AGENTS:
 *   - Climate-Vulnerable Populations (Powerless/Trapped): Island nations and low-lying regions facing existential threat with no exit options. Bear extraction cost immediately; wealthy nations have 25+ year delay.
 *   - Developing Economies (Powerful/Constrained): Emerging markets needing energy for development but constrained by carbon budgets negotiated by historical high emitters. Experience tangled rope: benefit from climate stability norms but bear asymmetric development restrictions.
 *   - Fossil Fuel Incumbents (Institutional/Arbitrage): Primary beneficiaries during transition window. Net-zero framing permits continued emissions through offsets, delayed implementation, and carbon-capture mythology while capturing renewable transition opportunities. Experience rope: genuine first-mover advantage in low-carbon sectors.
 *   - Renewable Energy Sector (Institutional/Arbitrage): Beneficiaries of net-zero transition as coordination mechanism. Can arbitrage between carbon-constrained and emerging markets. Low extraction; pure coordination function.
 *   - Climate Justice Coalitions (Organized/Constrained): Indigenous groups, young people, south-global activists building pressure for faster transitions and reparations. Organized enough to create alternative pathways (loss-and-damage funds, technology transfer mandates) with sunset logic.
 *   - International Climate Governance (Institutional/Arbitrage): UNFCCC processes, carbon markets, NDCs that perform climate action without structural change. Institutional theater that maintains negotiation processes while permitting continued extraction.
 *   - Analytical Observer (Analytical/Analytical): Risks naturalizing contingent political arrangements (unequal carbon budgets, delayed action timelines) as inherent properties of climate physics.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(net_zero_stabilization, 0.58).
domain_priors:suppression_score(net_zero_stabilization, 0.68).
domain_priors:theater_ratio(net_zero_stabilization, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(net_zero_stabilization, extractiveness, 0.58).
narrative_ontology:constraint_metric(net_zero_stabilization, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(net_zero_stabilization, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(net_zero_stabilization, tangled_rope).
narrative_ontology:human_readable(net_zero_stabilization, "The Net Zero Carbon Constraint").
narrative_ontology:topic_domain(net_zero_stabilization, "climate/energy/political_economy").

domain_priors:requires_active_enforcement(net_zero_stabilization).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(net_zero_stabilization, fossil_fuel_incumbents).
narrative_ontology:constraint_beneficiary(net_zero_stabilization, high_emission_economies).
narrative_ontology:constraint_beneficiary(net_zero_stabilization, carbon_intensive_industries).
narrative_ontology:constraint_victim(net_zero_stabilization, climate_vulnerable_populations).
narrative_ontology:constraint_victim(net_zero_stabilization, future_generations).
narrative_ontology:constraint_victim(net_zero_stabilization, global_south_nations).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CLIMATE-VULNERABLE POPULATIONS (SNARE) — Island nations and low-lying regions face existential threat with zero exit options. The constraint traps them: carbon budgets sufficient for wealthy nations to continue high-emission development for decades while poor nations suffer immediate consequences. d≈0.95, f(d)≈1.42, σ=1.2 → χ≈0.99.
constraint_indexing:constraint_classification(net_zero_stabilization, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: DEVELOPING ECONOMIES (TANGLED ROPE) — Need energy for development but constrained by carbon budgets negotiated by historical emitters. Benefit from coordination (climate stability norms) but bear asymmetric extraction (development restrictions). d≈0.72, f(d)≈1.15, σ=1.0 → χ≈0.67.
constraint_indexing:constraint_classification(net_zero_stabilization, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: RENEWABLE ENERGY SECTOR (ROPE) — Benefits from net-zero transition as coordination mechanism. Can arbitrage between carbon-constrained and emerging-tech markets. Low extraction; genuine coordination function. d≈0.15, f(d)≈-0.01, σ=1.2 → χ≈-0.01.
constraint_indexing:constraint_classification(net_zero_stabilization, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: FOSSIL FUEL INCUMBENTS (TANGLED ROPE) — Primary beneficiaries during the 2005-2020 window. Net-zero framing provides coordination narrative (climate action) while permitting continued emissions through offsets, carbon capture mythology, and delayed implementation. d≈0.08, f(d)≈-0.10, σ=1.2 → χ≈-0.06. Negative chi because beneficiary status dominates; but requires_active_enforcement=true and suppression=0.68 because the constraint suppresses structural alternatives (rapid phase-out, nationalization, technology transfer) through decades of 'transition' rhetoric.
constraint_indexing:constraint_classification(net_zero_stabilization, tangled_rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: CLIMATE JUSTICE COALITIONS (SCAFFOLD) — Organized pressure (indigenous groups, young people, south-global activists) pushing for faster transitions and loss-and-damage funds. See net-zero as temporary constraint with sunset: reparations and technology transfer would create alternative coordination pathways. d≈0.48, f(d)≈0.62, σ=1.2 → χ≈0.44. Scaffold because coalition action is building institutional alternatives (Paris Agreement loss-and-damage mechanisms, renewable tech transfer) that could replace the current carbon-budget framework.
constraint_indexing:constraint_classification(net_zero_stabilization, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: INTERNATIONAL CLIMATE GOVERNANCE (PITON) — UNFCCC processes, carbon markets, and net-zero pledges perform climate action without requiring structural change. Theater ratio=0.65: Nationally Determined Contributions (NDCs) are largely aspirational; carbon accounting permits offsetting that doesn't reduce atmospheric CO2; 'net-zero by 2050' is 30+ years away (beyond election cycles). Governance persists through inertia — governments can claim climate leadership while maintaining emission trajectories. d≈0.10, f(d)≈-0.07, σ=1.2 → χ≈-0.05.
constraint_indexing:constraint_classification(net_zero_stabilization, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / FALSE NATURAL LAW VIEW (MOUNTAIN) — The pre-2005 scientific framing that 'some emissions are compatible with stabilization' naturalizes what is actually a political choice: wealthy nations can continue high-emission development while poor nations accept development constraints. This perspective risks treating unequal extraction as an inherent limit of climate physics rather than a contingent institutional arrangement. However, structural data (ε=0.58, suppression=0.68) contradicts mountain classification — engine detects false summit.
constraint_indexing:constraint_classification(net_zero_stabilization, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(net_zero_stabilization_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(net_zero_stabilization, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(net_zero_stabilization, TypeOther, context(agent_power(powerful), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(net_zero_stabilization, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(net_zero_stabilization, TR),
    TR >= 0.70.

:- end_tests(net_zero_stabilization_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): The constraint permits high-emission economies to continue development through 2050 while requiring immediate constraints on low-emission nations. This is extraction — the budget allocation is structured so wealthy nations consume most of the remaining atmospheric carbon capacity. The value (0.58, not higher) reflects that the extraction is mixed with genuine coordination (all nations do need to reduce emissions eventually) and that some coordination mechanisms (Paris Agreement, renewable cost reductions) genuinely do enable lower-emission development. Suppression (0.68): High, but not maximum. Structural alternatives are suppressed: rapid fossil fuel phase-out is not discussed seriously in policy (requires treating stranded assets as losses); technology transfer is limited by IP frameworks; loss-and-damage financing is blocked by wealthy nations; radical reorganization of energy systems is off the table. But suppression is not total — renewable energy is growing, some nations are achieving real decarbonization, and climate justice coalitions have institutional voice. Theater ratio (0.65): Mid-to-high. NDCs are largely aspirational; carbon markets permit offsets that don't reduce atmospheric CO2; 'net-zero by 2050' is far enough away that governments can claim leadership while maintaining emission trajectories; renewable technology is celebrated while fossil subsidies continue unabated. Theater has increased because the gap between announced targets and actual emissions is widening — the performative content is growing.
 *
 * PERSPECTIVAL GAP:
 *   Extreme perspectival divergence. Island nations see a snare — they have no exit and bear immediate costs. Developing economies see mixed coordination and extraction — they need the climate stability coordination but are constrained unfairly. Fossil incumbents see rope — they are solving the coordination problem AND capturing transition advantages. Renewable sectors see pure rope — they benefit from coordination without bearing extraction costs. Climate justice coalitions see a scaffold with sunset — they are organized enough to see alternatives (reparations, technology transfer) that could replace the current constraint. Governance institutions see a piton — their processes are largely performative but persist through inertia. The analytical observer risks seeing a mountain — 'some nations must constrain development to stabilize climate' sounds like a law of nature, but the allocation of constraints to poor nations rather than rich ones is a political choice, not physics.
 *
 * DIRECTIONALITY LOGIC:
 *   Climate-vulnerable populations: Victim + trapped → d≈0.95, f(d)≈1.42. Maximum extraction; no exit. Developing economies: Victim + constrained → d≈0.72, f(d)≈1.15. Significant extraction because development constraints are asymmetrically applied to them. Fossil fuel incumbents: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.10. Net beneficiary; they can exit high-emission development through investment in renewables while claiming climate leadership. Renewable sector: Beneficiary + arbitrage → d≈0.15, f(d)≈-0.01. Net beneficiary with minimal extraction. Climate justice coalitions: Organized + constrained → d≈0.48, f(d)≈0.62. Moderate extraction because they are constrained by power asymmetries but organized enough to build alternatives. International governance: Institutional + arbitrage → d≈0.10, f(d)≈-0.07. Low extraction because they capture institutional advantage (legitimacy, negotiation forums) and have exit via disengage.
 *
 * MANDATROPHY ANALYSIS:
 *   The net-zero constraint resolves mandatrophy through institutional asymmetry. Wealthy nations experience it as rope (coordination for mutual benefit + arbitrage opportunity in renewables). Poor nations experience it as snare (no exit, immediate costs). Fossil incumbents experience it as rope (first-mover advantage in transition). Climate justice coalitions experience it as scaffold (organized enough to build alternatives). The mandatrophy is resolved not by finding a single 'correct' type but by observing that: (1) the constraint IS a genuine coordination problem (climate stabilization requires global emissions reduction), AND (2) it IS also a mechanism of extraction (the burden is allocated unequally, with wealthy nations capturing most of the benefits of continued development while poor nations accept constraints). Both are true simultaneously. The constraint is a hybrid mechanism that solves the coordination problem while functioning as an extraction apparatus. Indexical classification reveals this: from the perspective of powerless island nations, it's pure snare; from the perspective of fossil incumbents, it's rope; from the perspective of analytical observers, it's tangled rope. No single type 'resolves' the mandatrophy — the multiperspectival view does.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    carbon_budget_allocation_legitimacy,
    'On what moral and economic principle should historical cumulative emissions be allocated in carbon budgets — equal per-capita future entitlements, historical responsibility discount, or development-stage adjustment?',
    'Philosophical consensus on equity principles (Rawlsian, capabilities-based, or utilitarian); empirical determination of whether any allocation principle is being consistently applied in UNFCCC negotiations',
    'If per-capita: developing nations have legitimate claim to vastly larger future budgets than assigned. If historical responsibility: wealthy nations owe reparations as extraction cost. If development-stage: creates permanent hierarchy. No allocation principle is natural law — all are political choices.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(carbon_budget_allocation_legitimacy, preference, 'Philosophical basis for carbon budget allocation across nations').

omega_variable(
    net_zero_offset_sufficiency,
    'Can engineered carbon removal and natural sinks (reforestation, wetland restoration) credibly offset residual emissions at net-zero targets, or is ''net'' semantics permitting continued extraction?',
    'Long-term atmospheric CO2 monitoring correlated with offset claims; life-cycle analysis of offset projects for permanence, additionality, and leakage; assessment of whether offset markets incentivize genuine removal or just accounting gimmicks',
    'If offsets work: net-zero is a legitimate transition pathway. If offsets fail: net-zero is pure theater masking continued extraction — classification shifts from tangled_rope toward snare at high extractiveness (ε→0.75+).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(net_zero_offset_sufficiency, empirical, 'Whether carbon offsets can achieve genuine atmospheric CO2 neutrality').

omega_variable(
    extraction_timeline_to_extraction_ratio,
    'What proportion of global decarbonization must occur by 2030 (this decade) to have >50% probability of limiting warming to 1.5°C, versus what is mandated by current policy commitments?',
    'IPCC AR6 emissions gap analysis; retrospective assessment of 2030 actual vs committed reductions; climate-model sensitivity to cumulative emissions through 2035',
    'If gap is >30%: current net-zero framing is extracting decades of continued emissions while pretending action is underway — suppression rises to 0.80+, ε→0.72, classification shifts toward snare. If gap is <10%: tangled_rope framing is accurate; extraction is real but mixed with genuine coordination.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(extraction_timeline_to_extraction_ratio, empirical, 'Emissions reduction gap between Paris targets and policy commitments').

omega_variable(
    technology_transfer_lock_in,
    'Do intellectual property frameworks and technology licensing terms for renewable energy prevent developing nations from building domestic capacity, or is ''technology transfer'' a viable pathway to decoupling emission constraints from development constraints?',
    'Analysis of renewable energy capital costs in high-IP-enforcement vs weak-IP-enforcement regions; mapping of renewable tech patents and licensing barriers; empirical comparison of domestic capacity buildout timelines with and without IP restrictions',
    'If IP blocks transfer: developing nations are locked into fossil fuels by structural constraint (licensing costs), making the extraction mechanism explicit (snare from south-global perspective; ε→0.75+). If transfer works: tangled_rope framing stands; coordination function is real but imperfectly executed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(technology_transfer_lock_in, empirical, 'Effectiveness of IP frameworks in enabling or blocking clean technology transfer').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(net_zero_stabilization, 2005, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(nzcs_tr_t0, net_zero_stabilization, theater_ratio, 0, 0.38).
narrative_ontology:measurement(nzcs_tr_t10, net_zero_stabilization, theater_ratio, 10, 0.52).
narrative_ontology:measurement(nzcs_tr_t21, net_zero_stabilization, theater_ratio, 21, 0.65).

% Extraction over time
narrative_ontology:measurement(nzcs_be_t0, net_zero_stabilization, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(nzcs_be_t10, net_zero_stabilization, base_extractiveness, 10, 0.45).
narrative_ontology:measurement(nzcs_be_t21, net_zero_stabilization, base_extractiveness, 21, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(net_zero_stabilization, resource_allocation).
narrative_ontology:affects_constraint(net_zero_stabilization, carbon_market_permanence).
narrative_ontology:affects_constraint(net_zero_stabilization, renewable_technology_access).
narrative_ontology:affects_constraint(net_zero_stabilization, loss_and_damage_compensation).
narrative_ontology:affects_constraint(net_zero_stabilization, energy_system_lock_in).

% DUAL FORMULATION NOTE:
% The net-zero constraint decomposes into multiple structural claims with different ε values: (1) Climate stabilization requires reduced atmospheric CO2 (mountain, ε≈0.02); (2) Global emissions must decline (rope, ε≈0.15, pure coordination); (3) Wealthy nations can delay action to 2050 while poor nations constrain now (snare, ε≈0.72); (4) Carbon offsets and technology capture permit continued high-emission development (tangled rope, ε≈0.58). This story captures the institutional constraint at the political-economic level (ε=0.58). Upstream constraints (climate physics, emission pathways) have different ε values. Downstream constraints (renewable access, loss-and-damage financing) are affected by this constraint's allocation decisions.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(net_zero_stabilization, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
