% ============================================================================
% CONSTRAINT STORY: semiconductor_supply_chain_bottleneck
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_semiconductor_supply_chain_bottleneck, []).

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
 *   constraint_id: semiconductor_supply_chain_bottleneck
 *   human_readable: Semiconductor Supply Chain Bottleneck
 *   domain: industrial/economic/technology
 *
 * SUMMARY:
 *   The semiconductor supply chain bottleneck emerged post-2020 as a
 *   structural constraint on global manufacturing and technology access. The
 *   constraint combines genuine capacity scarcity with incumbent gatekeeping,
 *   geographic concentration, and long lead times for fab construction.
 *   Extractiveness increased from 0.35 (2020, perceived as temporary) to 0.61
 *   (2022, peak scarcity) and has stabilized at 0.58 (2026) as government
 *   subsidies begin supporting new capacity while incumbent advantages
 *   persist. Theater ratio (0.48) reflects moderate performative activity:
 *   emergency stockpiling, supply chain diversification announcements, and
 *   resilience protocols address symptoms without solving structural capacity
 *   constraint. The constraint exhibits all six DR types from different
 *   structural positions: powerless device manufacturers face a snare;
 *   emerging domestic fabs navigate a tangled coordination problem; incumbent
 *   fabs experience rope benefits; government actors deploying subsidies see
 *   a scaffold with sunset; the supply chain ritual maintains piton theater;
 *   and analytical observers risk mistaking policy-contingent scarcity for
 *   physical law.
 *
 * KEY AGENTS:
 *   - Incumbent Fab Operators (TSMC, Samsung, Intel Foundry): Primary beneficiaries (institutional/arbitrage) — capture premium pricing, demand security, and negotiating advantage during scarcity. Extract through capacity withholding and long-term contract terms.
 *   - Device Manufacturers (Apple, automotive suppliers, IoT manufacturers): Primary victims (powerless/trapped) — no alternative sourcing at required scale/cost/lead time. Experience maximum suppression through geographic dependency and qualification requirements.
 *   - Fab Equipment Suppliers (ASML, Tokyo Electron, Applied Materials): Secondary beneficiaries (institutional/arbitrage) — benefit from new fab capacity investment and upgrade cycles driven by scarcity premium.
 *   - Emerging Domestic Fabs (Intel Foundry, Samsung US expansion, TSMC Arizona): Mixed-position actors (moderate/constrained) — constrained by capital requirements and yield ramp challenges but benefiting from government subsidies and strategic importance narratives.
 *   - Government Industrial Policy Actors (CHIPS Act, EU subsidies, Japan semiconductors strategy): Organized agents (organized/constrained) — deploying capital to build alternative supply, viewing bottleneck as temporary with generational sunset.
 *   - Logistics and Supply Chain Intermediaries: Secondary beneficiaries (institutional/arbitrage) — extract through inventory management, customs brokerage, and allocation services during scarcity.
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing contingent incumbent consolidation as immutable physics of chip manufacturing
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(semiconductor_supply_chain_bottleneck, 0.58).
domain_priors:suppression_score(semiconductor_supply_chain_bottleneck, 0.65).
domain_priors:theater_ratio(semiconductor_supply_chain_bottleneck, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(semiconductor_supply_chain_bottleneck, extractiveness, 0.58).
narrative_ontology:constraint_metric(semiconductor_supply_chain_bottleneck, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(semiconductor_supply_chain_bottleneck, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(semiconductor_supply_chain_bottleneck, tangled_rope).
narrative_ontology:human_readable(semiconductor_supply_chain_bottleneck, "Semiconductor Supply Chain Bottleneck").
narrative_ontology:topic_domain(semiconductor_supply_chain_bottleneck, "industrial/economic/technology").

domain_priors:requires_active_enforcement(semiconductor_supply_chain_bottleneck).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(semiconductor_supply_chain_bottleneck, incumbent_fabs).
narrative_ontology:constraint_beneficiary(semiconductor_supply_chain_bottleneck, fab_equipment_suppliers).
narrative_ontology:constraint_beneficiary(semiconductor_supply_chain_bottleneck, logistics_intermediaries).
narrative_ontology:constraint_victim(semiconductor_supply_chain_bottleneck, downstream_device_manufacturers).
narrative_ontology:constraint_victim(semiconductor_supply_chain_bottleneck, emerging_fabs).
narrative_ontology:constraint_victim(semiconductor_supply_chain_bottleneck, global_tech_accessibility).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DEVICE MANUFACTURER (SNARE) — Trapped by geographic fab concentration and long lead times. No alternative sourcing available at scale or cost. Experiences pure extraction: cannot exit, cannot negotiate, cannot build redundancy within biographical timeframe. Maximum suppression.
constraint_indexing:constraint_classification(semiconductor_supply_chain_bottleneck, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: EMERGING DOMESTIC FAB (TANGLED ROPE) — Constrained by capital requirements and incumbent resistance, but also benefits from government subsidies and long-term supply security narratives. Mixed coordination (supply reliability assurance) and extraction (incumbent gatekeeping). Exit options exist but carry high cost (relocation, competing against entrenched supply chains).
constraint_indexing:constraint_classification(semiconductor_supply_chain_bottleneck, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: INCUMBENT FAB OPERATOR (ROPE) — Experiences the bottleneck as coordination benefit: capacity scarcity justifies premium pricing, long-term contracts ensure demand stability. Can arbitrage across geographies and customers. Net beneficiary — extraction flows toward this agent. Suppression maintains premium pricing but coordination function is genuine (capacity allocation under constraint is real problem).
constraint_indexing:constraint_classification(semiconductor_supply_chain_bottleneck, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: GOVERNMENT INDUSTRIAL POLICY (SCAFFOLD) — Organized agents (CHIPS Act, EU Chips Act, subsidy programs) see the bottleneck as temporary supply coordination failure with a generational sunset. Investing in new fab capacity and supply diversification to replace concentrated supply. Constraint has sunset clause (10-15 year estimate for new capacity to mature). Low effective extraction because this actor has agency and clear exit pathway through capacity investment.
constraint_indexing:constraint_classification(semiconductor_supply_chain_bottleneck, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: SUPPLY CHAIN THEATER (PITON) — Elaborate risk mitigation theater (strategic reserves, inventory requirements, long-term contracts, supply chain diversification initiatives) persists despite limited impact on actual availability. Theater ratio (0.48) reflects substantial performative activity: emergency stockpiles, resilience reporting, supplier qualification protocols — much maintains institutional legitimacy rather than solving underlying capacity constraint. The mechanism persists through inertia and insurance logic.
constraint_indexing:constraint_classification(semiconductor_supply_chain_bottleneck, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / PHYSICS LIMIT (MOUNTAIN) — From a civilizational timeframe, semiconductor manufacturing complexity creates inherent verification and validation bottlenecks: process nodes require billions in R&D, multi-year qualification cycles, and irreducible manufacturing defect rates. Some supply constraint is immutable physics. However, the structural data contradicts pure mountain classification — the current bottleneck is artificial scarcity created by incumbent consolidation and capacity underinvestment, not physical limits. This is a false summit: naturalizing a policy-contingent constraint as physical law.
constraint_indexing:constraint_classification(semiconductor_supply_chain_bottleneck, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(semiconductor_supply_chain_bottleneck_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(semiconductor_supply_chain_bottleneck, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(semiconductor_supply_chain_bottleneck, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(semiconductor_supply_chain_bottleneck, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(semiconductor_supply_chain_bottleneck, TR),
    TR >= 0.70.

:- end_tests(semiconductor_supply_chain_bottleneck_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderately high. The bottleneck creates genuine scarcity — fab capacity genuinely cannot meet demand at all price points simultaneously — but the scarcity is amplified by incumbent consolidation and underinvestment. Base extractiveness reflects that capacity constraint is real; the value is not higher because government capacity investment is reducing artificial scarcity. The trajectory shows extractiveness peaked at 0.61 in 2022 (maximum scarcity perception) and declined as subsidized capacity came online. Suppression (0.65): Moderately high. Barriers to exit include: geographic fab concentration (Taiwan, South Korea); 2-3 year fab construction timelines; 1-2 year process qualification cycles; billion-dollar capital requirements; incumbent long-term contracts locking access; supply chain opacity and information asymmetry. However, suppression is not total (0.90+) because government subsidies, alternative suppliers, and demand substitution reduce absolute barriers. Theater ratio (0.48): Moderate. Supply chain theater includes: strategic inventory reserves, supply chain diversification announcements, resilience protocols, supplier qualification tiering, emergency response plans. These address symptoms and build institutional legitimacy but do not directly increase available capacity. Theater has increased from 0.32 (early bottleneck period, purely demand shock) to 0.48 (mature bottleneck period, substantial institutional response). Theater remains below 0.50 because genuine capacity constraint remains — the rituals are not yet purely performative.
 *
 * PERSPECTIVAL GAP:
 *   The gap between beneficiary and victim perspectives is substantial. Incumbent fabs see rope — the constraint solves a real coordination problem (matching demand to limited capacity via price and contract terms). Device manufacturers see snare — the constraint offers no coordination benefit, only extraction and trap. Government actors see scaffold — the constraint is temporary policy problem being solved through capacity investment with clear timeline. The analytics view risks mountain — 'chip manufacturing is intrinsically complex and capacity-constrained' — but this naturalizes what is actually a contingent industrial structure. The field's epistemic reliability perspective (absent but structurally important) would see snare: the global technology ecosystem is trapped in dependency on concentrated suppliers with no exit pathway.
 *
 * DIRECTIONALITY LOGIC:
 *   Incumbent fabs are structural beneficiaries: scarcity increases their margin and contracts secure demand. Their d-value is low (~0.15), yielding negative or minimal effective extraction (they experience χ as coordination benefit). Device manufacturers are structural victims with trapped exit options: d-value is high (~0.90), yielding maximum f(d) and experienced χ. Government actors have constrained exit (cannot instantly build fabs) but agency (can deploy capital to reduce scarcity), yielding moderate d (~0.50) and moderate χ, classified as scaffold because they have sunset pathway. The logistics intermediaries benefit from allocation friction, similar to incumbents but with slightly higher d due to less direct control. The bottleneck's beneficiary/victim structure is asymmetric: benefits concentrate on incumbents and suppliers; costs distribute widely across device manufacturers and end-user sectors. This asymmetry drives the tangled_rope classification: genuine coordination function (scarcity pricing equilibrates demand) coexists with extractive incumbent gatekeeping.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by disambiguating genuine scarcity from incumbent extraction. The genuine scarcity (rope function): fab capacity truly is limited; complex node transitions require years; capital is genuinely scarce. The extraction mechanism (snare overlay): incumbents maintain pricing power and contract terms that exceed what pure scarcity would require; geographic concentration creates artificial barriers; capacity underinvestment during demand surges reflects strategic gatekeeping. The constraint is tangled_rope because both elements are structurally real. If the analysis had focused only on coordination (genuine capacity scarcity matching demand), the classification would underestimate extraction. If focused only on extraction (incumbent gatekeeping), it would overestimate because genuine scarcity contributes to the problem. The mandatrophy resolution: acknowledge both functions, measure their relative contribution (extractiveness 0.58 means extraction contributes more to constraint force than pure coordination would), and track the sunset pathway (government capacity investment should reduce both genuine scarcity AND extractive gatekeeping over 10-15 year horizon).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    incumbent_collusion_vs_coordination,
    'Are incumbent fabs maintaining supply constraint through collusion/coordination of capacity withholding, or through legitimate supply-demand coordination?',
    'Antitrust analysis; capacity utilization data vs announced capacity; pricing correlation with slack capacity; investment patterns during demand surges',
    'If collusion: snare classification applies to all perspectives; extraction is deliberate rent-seeking. If coordination: rope classification applies more broadly; extraction is legitimate scarcity pricing. Changes χ values for all victim perspectives by 0.15-0.25.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(incumbent_collusion_vs_coordination, empirical, 'Whether supply constraint is collusive or legitimate scarcity').

omega_variable(
    new_capacity_realistic_timeline,
    'Can government-subsidized new fab capacity meaningfully reduce bottleneck within 10-15 year horizon, or will foundry consolidation persist as structural feature?',
    'Tracking announced fab construction timelines, yield ramp-up rates, and market capture of new fabs; comparison to Intel foundry, TSMC expansion, Samsung foundry historical performance; cost trajectory for nodes below 5nm',
    'If realistic: scaffold sunset is real, constraint lifecycle ends generationally. If unrealistic: bottleneck persists indefinitely despite investment; scaffold reclassifies to failed-sunset tangled_rope. Changes theater_ratio and measurement trajectory.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(new_capacity_realistic_timeline, empirical, 'Whether new fab capacity can resolve bottleneck on subsidized timeline').

omega_variable(
    geographic_dependency_irreducibility,
    'Is concentration of advanced fab capacity in Asia (Taiwan, South Korea, Japan) a temporary artifact of capital allocation or a fundamental feature of semiconductor physics/economics?',
    'Historical analysis of fab geography shifts (1990s US vs 2010s Asia); cost modeling for advanced nodes in different regions; supply chain feedback loops favoring concentration; talent and ecosystem dependency analysis',
    'If temporary: diversification efforts will succeed; bottleneck is policy-solvable. If fundamental: geographic concentration will persist despite subsidies; bottleneck is structural; emerging fabs remain constrained indefinitely. Changes classificatory confidence for all perspectives.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(geographic_dependency_irreducibility, empirical, 'Whether fab concentration is temporary or structural').

omega_variable(
    device_substitution_availability,
    'Can device manufacturers substitute older-node semiconductors or alternative architectures for newer-node designs in 70% of use cases?',
    'Application-by-application analysis of performance requirements; cost modeling for older-node alternatives; design flexibility survey of major device manufacturers; architectural substitution case studies',
    'If high substitution: many victims can exit bottleneck without capacity expansion; snare classification weakens to tangled_rope for moderate fraction of affected manufacturers. If low substitution: trapped classification holds; snare persists. Changes measurement trajectory and victim group composition.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(device_substitution_availability, empirical, 'Whether device manufacturers can substitute older process nodes').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(semiconductor_supply_chain_bottleneck, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(semisupply_tr_t0, semiconductor_supply_chain_bottleneck, theater_ratio, 0, 0.32).
narrative_ontology:measurement(semisupply_tr_t3, semiconductor_supply_chain_bottleneck, theater_ratio, 3, 0.42).
narrative_ontology:measurement(semisupply_tr_t6, semiconductor_supply_chain_bottleneck, theater_ratio, 6, 0.5).
narrative_ontology:measurement(semisupply_tr_t10, semiconductor_supply_chain_bottleneck, theater_ratio, 10, 0.48).

% Extraction over time
narrative_ontology:measurement(semisupply_be_t0, semiconductor_supply_chain_bottleneck, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(semisupply_be_t3, semiconductor_supply_chain_bottleneck, base_extractiveness, 3, 0.52).
narrative_ontology:measurement(semisupply_be_t6, semiconductor_supply_chain_bottleneck, base_extractiveness, 6, 0.61).
narrative_ontology:measurement(semisupply_be_t10, semiconductor_supply_chain_bottleneck, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(semiconductor_supply_chain_bottleneck, resource_allocation).
narrative_ontology:affects_constraint(semiconductor_supply_chain_bottleneck, device_manufacturing_lead_time_uncertainty).
narrative_ontology:affects_constraint(semiconductor_supply_chain_bottleneck, emerging_fab_capital_requirement).
narrative_ontology:affects_constraint(semiconductor_supply_chain_bottleneck, geopolitical_technology_dependency).

% DUAL FORMULATION NOTE:
% The semiconductor bottleneck decomposes into multiple structurally distinct constraints: (1) fab capacity scarcity (resource_allocation coordination problem); (2) incumbent consolidation and gatekeeping (extraction mechanism); (3) geographic dependency (geopolitical constraint). This story integrates all three as a tangled_rope. Upstream constraints in the family include the physics of process node scaling (immutable, but not the bottleneck driver) and downstream constraints include device manufacturer supply chain risk (derivative of this bottleneck).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(semiconductor_supply_chain_bottleneck, institutional, 0.18).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
