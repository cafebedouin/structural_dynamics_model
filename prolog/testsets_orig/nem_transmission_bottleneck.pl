% ============================================================================
% CONSTRAINT STORY: nem_transmission_bottleneck
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_nem_transmission_bottleneck, []).

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
 *   constraint_id: nem_transmission_bottleneck
 *   human_readable: National Electricity Market Transmission Bottleneck
 *   domain: energy/infrastructure/economic
 *
 * SUMMARY:
 *   Transmission bottlenecks in the National Electricity Market (NEM) are
 *   structural constraints on power flow created by the physical limits of
 *   existing transmission lines, combined with regulatory and investment
 *   frameworks that determine when and how those limits are expanded. The
 *   constraint emerges from the coupling of geography (renewable resources
 *   are concentrated in specific regions far from load centers), physics
 *   (transmission wire capacity is fixed by Ohm's law and material
 *   properties), and economics (transmission upgrades cost billions of
 *   dollars and take 10-15 years to plan and build). This creates a temporal
 *   gap: renewable generators built in high-solar or high-wind regions often
 *   exceed the transmission capacity of lines connecting them to demand
 *   centers. During high renewable output periods, network operators must
 *   curtail (reduce) renewable generation to prevent line overloads.
 *   Curtailed generators receive partial or no compensation, while consumers
 *   in congested regions pay higher electricity prices (congestion charges).
 *   The constraint exhibits all six DR types from different positions:
 *   trapped remote renewables see snare extraction; incumbent generators in
 *   uncongested regions benefit without effort (piton); the network operator
 *   coordinates system stability (rope); grid modernization advocates see a
 *   solvable 15-year temporary problem (scaffold); the analytical observer
 *   risks naturalizing engineering choices as physical law (false mountain).
 *   The extractiveness value has been rising (0.42 → 0.58 over 14 years) as
 *   renewable penetration increases and physical constraints tighten, while
 *   theater ratio rises as the constraint becomes more central to market
 *   design narratives.
 *
 * KEY AGENTS:
 *   - Remote Renewable Generators: Primary victims (powerless/trapped) — wind and solar farms located in high-resource regions with limited transmission capacity; lose 15-40% of potential output via curtailment; cannot relocate infrastructure or access alternative markets
 *   - Network Operator (Transmission System Operator): Primary beneficiary (institutional/arbitrage) — manages congestion through pricing and curtailment; captures congestion rents; controls operational decisions and investment signals
 *   - Incumbent Coal/Gas Generators: Secondary beneficiary (organized/constrained) — located in uncongested regions; face reduced competition from renewables during congestion events; benefit from higher market prices; but benefit is declining as coal retires (piton effect)
 *   - Consumers in Congested Regions: Moderate victim (moderate/constrained) — pay congestion surcharges; cannot access cheaper renewable supply; face electricity price increases of 5-15% during peak demand periods
 *   - Grid Modernization Coalition: Organized advocates (organized/mobile) — renewable developers, forward-looking utilities, policy advocates; see constraint as temporary problem; building alternative infrastructure (HVDC, distributed storage, microgrids)
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks treating transmission physics as immutable natural law rather than contingent engineering choice
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(nem_transmission_bottleneck, 0.58).
domain_priors:suppression_score(nem_transmission_bottleneck, 0.68).
domain_priors:theater_ratio(nem_transmission_bottleneck, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(nem_transmission_bottleneck, extractiveness, 0.58).
narrative_ontology:constraint_metric(nem_transmission_bottleneck, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(nem_transmission_bottleneck, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(nem_transmission_bottleneck, tangled_rope).
narrative_ontology:human_readable(nem_transmission_bottleneck, "National Electricity Market Transmission Bottleneck").
narrative_ontology:topic_domain(nem_transmission_bottleneck, "energy/infrastructure/economic").

domain_priors:requires_active_enforcement(nem_transmission_bottleneck).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(nem_transmission_bottleneck, incumbent_generators_in_uncongested_regions).
narrative_ontology:constraint_beneficiary(nem_transmission_bottleneck, network_operator).
narrative_ontology:constraint_victim(nem_transmission_bottleneck, renewable_generators_constrained_by_lines).
narrative_ontology:constraint_victim(nem_transmission_bottleneck, remote_consumers).
narrative_ontology:constraint_victim(nem_transmission_bottleneck, system_efficiency).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: REMOTE RENEWABLE GENERATOR (SNARE) — Wind and solar farms in congestion-prone regions cannot exit the constraint. Built infrastructure is immobile; output curtailment is mandated by network operator; alternative markets require transmission upgrades they cannot fund. Maximum extraction through forced curtailment: renewable generators lose 15-40% of potential output while bearing no compensation for lost revenue. Suppression is structural: physical grid topology, regulatory design, and capital requirements for alternative transmission all create insurmountable exit barriers.
constraint_indexing:constraint_classification(nem_transmission_bottleneck, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: NETWORK OPERATOR (ROPE) — Manages system stability through congestion pricing and curtailment. Benefits from rent extraction via congestion charges (absorbed by all consumers) while experiencing the constraint as a coordination mechanism: managing supply-demand balance at transmission nodes is their core function. Exit option is arbitrage — they can trade off different congestion management strategies, regulatory frameworks, and investment signals. Net beneficiary through fee accumulation and operational control.
constraint_indexing:constraint_classification(nem_transmission_bottleneck, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: CONSUMER IN CONGESTED REGION (TANGLED ROPE) — Pays congestion charges embedded in electricity prices (coordination cost) while bearing asymmetric extraction: cheaper renewable supply cannot reach them, forcing reliance on more expensive incumbent generation. Constrained exit: can relocate or shift consumption patterns only at significant cost. Mixed experience: genuine coordination problem (balancing supply and demand) overlaid with asymmetric extraction (higher prices due to congestion rent).
constraint_indexing:constraint_classification(nem_transmission_bottleneck, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: INCUMBENT GENERATOR (PITON) — Structurally benefits from transmission bottlenecks that prevent cheaper renewable competitors from reaching market. However, this benefit is increasingly theatrical: coal/gas generation is declining in market share and economic viability regardless of transmission constraints. The constraint's extraction function persists through regulatory inertia and sunk asset protection (coal plants write down over decades) even as its primary beneficiary class atrophies. The generator experiences the constraint as a necessary market design feature, but its functional role (protecting stranded assets) is degraded.
constraint_indexing:constraint_classification(nem_transmission_bottleneck, piton,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: GRID MODERNIZATION COALITION (SCAFFOLD) — Policy advocates, renewable developers, and forward-looking network operators see transmission bottlenecks as temporary coordination failures with a structural sunset. Grid modernization investments (HVDC lines, advanced switching, distributed generation) are creating alternative pathways that bypass traditional bottlenecks. The coalition has mobile exit options (invest in alternative infrastructure, shift regulatory incentives, develop storage) and sees the current constraint as time-limited. High suppression during the transition (upgrades cost $50B+ and take 10-15 years) but with a clear sunset horizon.
constraint_indexing:constraint_classification(nem_transmission_bottleneck, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / PHYSICAL LIMITS (MOUNTAIN) — From a civilizational perspective, transmission constraints appear to be immutable laws of physics: the power flow equations (Ohm's law, Kirchhoff's laws) create hard limits on how much current any wire can carry. The bottleneck is inherent to electromagnetic reality, not a policy choice. However, this perspective naturalizes what are actually contingent engineering and regulatory decisions: wire capacity, grid topology, voltage standards, and investment signals are all within human design space. The analytical observer risks false summits by treating infrastructure architecture as physical law.
constraint_indexing:constraint_classification(nem_transmission_bottleneck, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(nem_transmission_bottleneck_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(nem_transmission_bottleneck, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(nem_transmission_bottleneck, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(nem_transmission_bottleneck, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(nem_transmission_bottleneck, TR),
    TR >= 0.70.

:- end_tests(nem_transmission_bottleneck_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): The constraint shows moderate-high extractiveness that has grown over 14 years as renewable penetration increased. The base extractiveness was 0.42 when renewable capacity was lower; it is now 0.58 as more renewable generators compete for limited transmission. This is not speculative — it reflects measurable curtailment rates (15-40% in worst-affected regions) and measurable congestion charges (5-15% price premiums). However, not all of this is pure extraction: some curtailment is legitimate demand-side balancing (genuine coordination cost), and some congestion charging reflects the cost of temporary supply-demand mismatch. Omega variable 'curtailment_baseline_ambiguity' flags that 30-50% of measured curtailment may be legitimate coordination rather than extraction. Conservative estimate keeps ε at 0.58 rather than 0.70. Suppression (0.68): High. Barriers to exit are substantial: renewable generators cannot move their plants (infrastructure is immobile); cannot access alternative markets without transmission upgrades costing billions; cannot collectively fund those upgrades (individual generator revenue is too small). Network operators' regulatory framework creates suppression through: (a) obligation to curtail to prevent line overloads (physical safety requirement), (b) lack of adequate compensation mechanisms for curtailment (generators bear most cost), and (c) slow investment cycles (10-15 years to plan and build upgrades). Consumers in congested regions face suppression from price floors (cannot reduce electricity consumption much, as it is essential service) and lack of alternative suppliers (congestion isolates them from cheaper regions). Theater ratio (0.55): Moderate. The constraint has performative elements — congestion pricing is partly real economic signal and partly theatrical rent mechanism. Network operator communications about 'essential reliability work' and 'grid stability' are partly justified (transmission does provide stability) and partly narrative cover for rent extraction. Grid studies claiming transmission 'must' be expanded at specified rates include genuine engineering assessment mixed with modeling assumptions that favor incumbent technologies. However, the theater is not dominant (as in piton, where theater ≥ 0.70) because the core constraint is structurally real: transmission wires have finite capacity, and renewable output sometimes exceeds that capacity.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates sharp perspectival divergence. Remote renewable generators (powerless/trapped) see snare — they are unambiguously extracted from via forced curtailment without adequate compensation. The network operator (institutional/arbitrage) sees rope — they experience the constraint as a coordination mechanism they manage daily, with some rent as a side benefit. Incumbent generators see piton — the extraction benefit they receive from reduced renewable competition is becoming theatrical as coal retires and renewables dominate new capacity. Consumers in congested regions see tangled_rope — they benefit from the coordination function (stable electricity supply) overlaid with extraction (higher prices). The grid modernization coalition sees scaffold — they frame the bottleneck as a temporary 10-15 year problem with a clear exit pathway (transmission upgrades + storage + distributed generation). The analytical observer risks false mountain — treating transmission physics as immutable law when the actual constraint combines physics (wire capacity) with economics (investment cycles) and regulation (curtailment rules). The perspectival gap widens along the powerless-institutional axis: powerless agents cannot see exit; institutional agents control investment decisions and have arbitrage options.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) is derived from beneficiary/victim status and exit options. Remote renewable generators are victims with trapped exit → d approaches 1.0 → high f(d) → high experienced extraction. Network operator is beneficiary with arbitrage exit → d approaches 0.0 → low f(d) → low/negative experienced extraction (extraction runs toward them). Consumers are victims with constrained exit (high cost to relocate or reduce consumption) → d ≈ 0.75-0.85 → high f(d). Incumbent generators are beneficiaries with constrained exit (retirements are planned decades ahead) → d ≈ 0.25-0.35 → moderate f(d). The scope modifier σ(S) is national (σ=1.0), so χ = ε × f(d) × 1.0. For the trapped remote generator, χ ≈ 0.58 × 1.42 ≈ 0.82 (snare territory). For the network operator, χ ≈ 0.58 × (-0.12) ≈ -0.07 (rope — negative effective extraction, they are subsidized by the constraint). This perspectival gap in χ is the diagnostic: same ε (0.58), same suppression (0.68), but wildly different experienced extractiveness (0.82 vs -0.07) because the structural position differs.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy through perspectival decomposition. The mandatrophy question is: 'Is transmission bottleneck a coordination problem (rope) or extraction problem (snare)?' The answer is both — it depends on whose perspective you inhabit. The network operator sees pure coordination (rope) — their experience is managing complex supply-demand balancing. The remote generator sees pure extraction (snare) — they are forced to reduce output with minimal compensation. Consumers see mixed (tangled_rope) — coordination for reliability, extraction for price. The analytical view risks false mountain by naturalizing the constraint as a consequence of physics alone. The actual resolution: the constraint IS a real coordination problem (transmission capacity is finite, balancing is necessary) OVERLAID with an extraction mechanism (who bears the cost of balancing, who captures the benefit). Disentangling requires asking: (1) Would the same coordination function (keeping the grid stable) require the same distribution of costs if compensation mechanisms were different? (2) If yes, then some extractiveness is unavoidable coordination cost. (3) If no, then the extraction is contingent on the regulatory design. Omega variable 'transmission_investment_cycle' tests exactly this: if regulatory incentives were reformed to accelerate transmission upgrades, would extractiveness decline? Affirmative answer indicates the extraction is regulatory, not physical.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    curtailment_baseline_ambiguity,
    'How much renewable curtailment is legitimate demand-side balancing vs. rent extraction by incumbent generators?',
    'Counterfactual analysis: model expected curtailment in a grid with (a) current transmission topology + current incentive structure, and (b) optimized topology + cost-reflective pricing. Difference isolates extractive curtailment.',
    'If extractive portion > 30%: snare classification solidifies. If < 10%: constraint reclassifies as pure rope (coordination) and suppression metric drops to 0.35-0.45.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(curtailment_baseline_ambiguity, empirical, 'Proportion of curtailment attributable to extraction vs. legitimate balancing').

omega_variable(
    transmission_investment_cycle,
    'Does the current regulatory framework adequately incentivize transmission upgrades, or do bottlenecks persist partly through perverse incentives?',
    'Analysis of regulatory rate-of-return mechanisms, capital cost recovery timelines, and comparison with jurisdictions using alternative investment models (merchant transmission, performance-based regulation, public investment).',
    'If incentive structure is adequate: bottleneck reflects genuine physical/coordination challenge (rope from more perspectives). If inadequate: regulatory design is part of the extraction mechanism (tangled_rope confirmed; extractiveness rises to 0.65+).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(transmission_investment_cycle, empirical, 'Whether regulatory framework adequately incentivizes transmission expansion').

omega_variable(
    storage_bypass_feasibility,
    'Could distributed battery storage (behind-the-meter or utility-scale) functionally bypass transmission bottlenecks without full grid upgrades?',
    'Techno-economic modeling: cost comparison of localized storage + generation clustering vs. traditional transmission upgrades. Real-world pilot data from grids implementing storage-first strategies.',
    'If storage bypass is feasible at <20% cost premium: scaffold sunset accelerates; constraint becomes temporary coordination problem (Scaffold from powerless perspective). If infeasible or cost-prohibitive: transmission upgrade becomes mandatory, suppression persists longer.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(storage_bypass_feasibility, empirical, 'Whether storage can functionally bypass transmission constraints').

omega_variable(
    congestion_rent_measurement,
    'What is the total annual economic value extracted via congestion pricing and curtailment, and who captures it?',
    'Detailed accounting of congestion charges, curtailment compensation shortfalls, and ancillary service revenue flows. Network operator accounting + generator profit analysis.',
    'If captured rent > $5B annually in large markets: extractiveness is understated (raise ε to 0.65-0.70). If < $1B: extraction is primarily efficiency cost, not rent (lower ε to 0.40-0.45).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(congestion_rent_measurement, empirical, 'Total economic rent extracted via congestion and curtailment mechanisms').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(nem_transmission_bottleneck, 0, 14).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(nem_trans_tr_t0, nem_transmission_bottleneck, theater_ratio, 0, 0.38).
narrative_ontology:measurement(nem_trans_tr_t7, nem_transmission_bottleneck, theater_ratio, 7, 0.48).
narrative_ontology:measurement(nem_trans_tr_t14, nem_transmission_bottleneck, theater_ratio, 14, 0.55).

% Extraction over time
narrative_ontology:measurement(nem_trans_be_t0, nem_transmission_bottleneck, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(nem_trans_be_t7, nem_transmission_bottleneck, base_extractiveness, 7, 0.52).
narrative_ontology:measurement(nem_trans_be_t14, nem_transmission_bottleneck, base_extractiveness, 14, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(nem_transmission_bottleneck, resource_allocation).
narrative_ontology:affects_constraint(nem_transmission_bottleneck, renewable_curtailment_exposure).
narrative_ontology:affects_constraint(nem_transmission_bottleneck, electricity_price_volatility).
narrative_ontology:affects_constraint(nem_transmission_bottleneck, grid_stability_cycling).

% DUAL FORMULATION NOTE:
% NEM transmission bottleneck decomposes into two structurally distinct constraints: (1) transmission_capacity_physics — the hard physical limit of existing wires (mountain, ε≤0.10), and (2) transmission_investment_underinvestment — the regulatory and financial barriers preventing adequate capacity expansion (tangled_rope, ε=0.58). This story addresses the combined effect. Separate stories can isolate the physics-only constraint from the institutional failure constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(nem_transmission_bottleneck, organized, 0.3).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
