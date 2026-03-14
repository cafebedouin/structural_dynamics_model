% ============================================================================
% CONSTRAINT STORY: battery_supply_chain_security
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_battery_supply_chain_security, []).

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
 *   constraint_id: battery_supply_chain_security
 *   human_readable: Battery Supply Chain Security and Mineral Extraction Dependencies
 *   domain: geopolitical/economic/environmental
 *
 * SUMMARY:
 *   The global battery supply chain for electrification and renewable energy
 *   storage creates a structural constraint that exhibits both genuine
 *   coordination (solving the logistics and technical problem of distributing
 *   energy storage technology at scale) and asymmetric extraction (monopoly
 *   control of critical minerals, suppression of labor and environmental
 *   costs, geographic lock-in of resource-dependent communities). The
 *   constraint has intensified over the past 15 years as demand for lithium,
 *   cobalt, nickel, and other critical minerals has accelerated. This
 *   acceleration is driven by energy transition policies and EV adoption
 *   targets that created sudden, inelastic demand for minerals with long
 *   development timelines and geographically concentrated supplies. The
 *   tangled_rope classification reflects that the constraint simultaneously
 *   solves a real coordination problem (manufacturing battery technology
 *   requires organized mineral supply chains) and extracts value
 *   asymmetrically (monopoly rents, suppressed labor costs, externalized
 *   environmental damage). The theater ratio (0.58) reflects corporate social
 *   responsibility reporting, supply chain audits, and sustainability
 *   certifications that create appearance of governance without changing core
 *   extraction mechanisms. The measurement trajectory shows extractiveness
 *   increasing from 0.35 to 0.58 and theater increasing from 0.42 to 0.65
 *   over 20 years, indicating both intensifying monopoly power and increasing
 *   performative governance activity — a classic Goodhart drift where metrics
 *   (sustainability scores, audit compliance) degrade while extraction
 *   mechanisms strengthen.
 *
 * KEY AGENTS:
 *   - Mining Communities and Resource-Dependent Nations: Primary victims (powerless/trapped) — face structural lock-in to extraction; limited education, alternative employment, or economic paths; subject to environmental degradation that destroys non-extractive options.
 *   - Battery Manufacturers and Electronics Producers: Primary beneficiaries (institutional/arbitrage) — control technology, manufacturing, and end-consumer relationships; can diversify suppliers, invest in substitutes, and capture value from supply chain coordination.
 *   - Mineral Extracting Corporations: Secondary beneficiaries (organized/constrained) — exercise monopoly/oligopoly control over supply; constrained by capital intensity and regulatory pressure; benefit from pricing power and supply discipline.
 *   - Energy-Transition-Dependent Nations: Secondary victims (moderate/constrained) — require mineral access for decarbonization but lack geological endowments; face vulnerability to price manipulation and supply coercion; higher exit cost (fossil fuel lock-in) than staying in mineral dependence.
 *   - Strategic Supply Chain Governance Institutions: Performative actors (organized/constrained) — maintain audit and certification regimes that create governance appearance without enforcing core standards; piton classification reflects institutional inertia.
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — sees constraint as genuine tangled_rope: coordination and extraction are inseparably entangled; energy transition requires minerals; mineral supply is geographically concentrated; extraction mechanisms exploit this concentration.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(battery_supply_chain_security, 0.58).
domain_priors:suppression_score(battery_supply_chain_security, 0.65).
domain_priors:theater_ratio(battery_supply_chain_security, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(battery_supply_chain_security, extractiveness, 0.58).
narrative_ontology:constraint_metric(battery_supply_chain_security, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(battery_supply_chain_security, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(battery_supply_chain_security, tangled_rope).
narrative_ontology:human_readable(battery_supply_chain_security, "Battery Supply Chain Security and Mineral Extraction Dependencies").
narrative_ontology:topic_domain(battery_supply_chain_security, "geopolitical/economic/environmental").

domain_priors:requires_active_enforcement(battery_supply_chain_security).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(battery_supply_chain_security, battery_manufacturers).
narrative_ontology:constraint_beneficiary(battery_supply_chain_security, mineral_extracting_corporations).
narrative_ontology:constraint_beneficiary(battery_supply_chain_security, consumer_electronics_producers).
narrative_ontology:constraint_victim(battery_supply_chain_security, mining_communities).
narrative_ontology:constraint_victim(battery_supply_chain_security, energy_transition_dependent_nations).
narrative_ontology:constraint_victim(battery_supply_chain_security, environmental_commons).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: MINING COMMUNITIES (SNARE) — Extractive dependence on mineral extraction creates locked-in vulnerability. Communities face structural barriers: geographic immobility, lack of alternative employment, political subordination to multinational extractors, environmental degradation that destroys other economic options. Exit cost is total — leaving means abandoning livelihood and ancestral lands. Suppression through state-corporate alignment and limited education access prevents collective action. No coordination benefit flows to this agent — the constraint purely extracts.
constraint_indexing:constraint_classification(battery_supply_chain_security, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: ENERGY-TRANSITION-DEPENDENT NATIONS (TANGLED ROPE) — Nations pursuing decarbonization face both genuine coordination benefits and asymmetric extraction. Benefit: electrification and renewable energy require battery technology. Cost: mineral supply dependence creates vulnerability to price manipulation, geopolitical coercion, and monopolistic control. Exit cost is high (fossil fuel lock-in is expensive) but not impossible (alternative tech paths exist). Constrained rather than trapped. Asymmetric extraction: major mineral producers (DRC lithium, Chile copper, Indonesia nickel) exercise monopoly power over nations lacking alternative mineral sources.
constraint_indexing:constraint_classification(battery_supply_chain_security, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: BATTERY MANUFACTURERS (ROPE) — Manufacturers experience the supply chain as coordination: organizing mineral flows, managing logistics, ensuring material quality. They benefit from price stability and supply reliability. Extraction is minimal relative to overall value — they capture margin through technology and efficiency. Arbitrage options are substantial: vertical integration into mining, geographic diversification of suppliers, investment in recycling infrastructure and alternative cathode chemistries that reduce mineral dependence. Net beneficiary position; sees constraint as enabling coordination.
constraint_indexing:constraint_classification(battery_supply_chain_security, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: MINING CORPORATIONS (TANGLED ROPE) — Extractors benefit from monopoly/oligopoly control (coordination of supply to maintain price premium). Also constrained: capital intensity, long lead times for opening new mines, geopolitical risk from resource nationalism, regulatory pressure on environmental standards. Extraction mechanism: capacity control, price discipline, lock-in contracts. Genuine coordination function: organizing exploration, risk capital, infrastructure. Active enforcement: vertical integration pressure, exclusive supply contracts, regulatory capture of environmental standards in resource-rich countries.
constraint_indexing:constraint_classification(battery_supply_chain_security, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: STRATEGIC SUPPLY CHAIN GOVERNANCE (PITON) — International frameworks (Critical Minerals Strategy, supply chain resilience initiatives) are largely performative. Real decisions are made through market power and state-corporate coordination, not through institutional governance. The governance ritual persists (supply chain audits, corporate social responsibility reporting, sustainability certifications) but core extraction mechanisms (monopoly pricing, labor cost suppression, environmental externalization) bypass these controls. Theater ratio elevated: audits find minimal violations; labor standards exist on paper; recycling targets are missed. The institutions maintain legitimacy through performative activity while structural extraction continues.
constraint_indexing:constraint_classification(battery_supply_chain_security, piton,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (TANGLED ROPE) — From civilizational scale, the battery supply chain constraint embeds genuine coordination (solving the technical problem of energy storage distribution) within extractive asymmetries (mineral monopolies, labor suppression, environmental externalization). The engine's classification as tangled_rope is appropriate: chi is elevated (0.58 × f(d) × σ(global)), but still below snare threshold, because significant coordination function is demonstrably present. The constraint is not pure extraction disguised as coordination; it is genuine coordination with embedded extraction.
constraint_indexing:constraint_classification(battery_supply_chain_security, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(battery_supply_chain_security_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(battery_supply_chain_security, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(battery_supply_chain_security, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(battery_supply_chain_security, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(battery_supply_chain_security, TR),
    TR >= 0.70.

:- end_tests(battery_supply_chain_security_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The constraint extracts value through multiple mechanisms: (1) mineral monopoly pricing—major producers exercise price discipline and extract economic rent above competitive levels; (2) labor cost suppression—mining communities lack bargaining power and wages remain low relative to commodity values; (3) environmental externalization—mining costs (water depletion, soil contamination, ecosystem destruction) are not borne by extractors or manufacturers; (4) geographic lock-in—mining communities lack alternative economic paths and cannot exit. The extractiveness value reflects that extraction is not universal (manufacturers can diversify, invest in alternatives) but is substantial for trapped agents. Suppression (0.65): High. Multiple barriers prevent exit or collective action: geographic immobility (mining is site-specific), lack of alternative employment (few other industries in mining regions), political subordination (resource-extracting states often have weak governance; corporations capture regulatory processes), educational barriers (limited technical training for alternative careers), and repression of labor organizing in many mining jurisdictions. Theater ratio (0.58): Moderate-high. Corporate social responsibility reporting, supply chain audits, sustainability certifications, and conflict minerals frameworks create governance appearance. Audits typically find minimal violations; suppliers 'pass' audits while core practices remain unchanged. Recycling targets are set and missed repeatedly without consequences. Labor standards exist on paper; enforcement is weak. Environmental impact assessments are required but remediation is absent. The theater has increased over time as regulatory scrutiny has grown, forcing corporations to invest in governance performance rather than in changing extraction mechanisms.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates striking perspectival divergence. Battery manufacturers see rope (coordination problem: organizing mineral flows, ensuring quality, managing logistics). They experience the constraint as enabling their business — without organized supply chains, they cannot scale battery production. Mining communities see snare (maximum extraction, no exit, no benefit). They experience the constraint as pure exploitation — they bear environmental and social costs while capturing minimal value. Energy-transition-dependent nations see tangled_rope (genuine need for mineral access for decarbonization, but vulnerable to monopoly pricing and supply discipline). They benefit from the coordination function (battery technology enables their energy transition) but suffer extraction through high input costs and vulnerability to coercion. Mineral extractors see tangled_rope with inverted directionality (they are extractors, not targets) — they benefit from monopoly pricing but are constrained by capital intensity, regulatory risk, and geopolitical volatility. Governance institutions see rope or piton (coordination mechanism or degraded ritual) — they perceive their audit and certification work as governing supply chains, unaware that core extraction mechanisms operate parallel to and independent of their governance frameworks. The analytical observer resolves the gap by recognizing that all perspectives are structurally accurate: the constraint IS coordination AND extraction simultaneously, embodied in different experiences by different agents.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values (d) are derived from beneficiary/victim status and exit options. Mining communities (primary victims, trapped exit) → d ≈ 0.95 (maximum target status) → f(d) ≈ 1.42 → χ ≈ 0.58 × 1.42 × 1.2 (global scope) ≈ 0.99 (snare threshold χ ≥ 0.66 is exceeded). Battery manufacturers (primary beneficiaries, arbitrage exit) → d ≈ 0.05 (near-maximum beneficiary) → f(d) ≈ -0.12 → χ ≈ 0.58 × (-0.12) × 1.2 ≈ -0.08 (negative extraction; rope classification). Energy-transition-dependent nations (secondary victims, constrained exit) → d ≈ 0.75 (substantial target status) → f(d) ≈ 1.15 → χ ≈ 0.58 × 1.15 × 1.2 ≈ 0.80 (snare threshold; but moderate power mitigates, producing tangled_rope). Mineral extractors (beneficiaries with constrained exit) → d ≈ 0.35 (mixed status: extracting but constrained) → f(d) ≈ 0.40 → χ ≈ 0.58 × 0.40 × 1.2 ≈ 0.28 (rope classification). The asymmetry is stark: d ranges from 0.05 to 0.95 across perspectives, producing chi values from -0.08 to 0.99. This asymmetry is the definition of tangled_rope: genuine coordination (manufacturers benefit, supply chains function) coexists with extreme extraction (mining communities face snare-level extraction). The constraint cannot be relabeled as pure rope because victims experience snare-level extraction; cannot be relabeled as pure snare because beneficiaries genuinely benefit from the coordination function.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION: The constraint resolves the mandatrophy by demonstrating that tangled_rope is the correct classification despite the presence of extreme extraction (snare-level from mining communities' perspective). The temptation to mislabel is high: (1) the snare alternative — 'this is pure extraction, relabel as snare' — is plausible because victims experience snare-level extraction; (2) the rope alternative — 'this is coordination with consensual distribution, relabel as rope' — is plausible because genuine coordination function exists and benefits accrue to some agents. Mandatrophy prevents both errors by enforcing the conjunction gate: tangled_rope REQUIRES (1) beneficiaries (yes: manufacturers, extractors), (2) victims (yes: mining communities, energy-transition nations), and (3) active enforcement (yes: exclusive supply contracts, vertical integration, regulatory capture). The constraint must simultaneously coordinate and extract. The asymmetry in directionality values (d from 0.05 to 0.95) and the behavioral evidence of active enforcement (vertical integration, supply discipline, regulatory capture) confirm that this is not rope with unfortunate distribution externalities; it is active coordination of an asymmetric extraction mechanism. The piton perspective (governance institutions) is the false summit to beware: institutions see rope or believe they are creating rope through audits and certifications. But the institutional governance is substantially performative (theater ratio 0.58 and rising). The real coordination happens through market power and state-corporate alignment, not through governance frameworks. Acknowledging this requires the tangled_rope classification and rejects the institutional narrative of 'governance improving supply chains.'
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    mineral_substitution_feasibility,
    'Can alternative battery chemistries (sodium-ion, solid-state, iron-air) reduce dependence on cobalt, lithium, and nickel sufficiently to break the mineral monopoly lock-in within the energy transition timeline?',
    'Technology maturation analysis: cost parity timelines, scalability demonstrations, supply chain buildout for alternative materials. If substitute chemistries achieve price parity and scale by 2035, the constraint''s extraction mechanism weakens; if not, mineral monopolies persist.',
    'If feasible: constraint reclassifies toward rope (coordination dominates); mineral monopoly power declines. If not: constraint remains tangled_rope with elevated snare characteristics; energy transition nations remain trapped in mineral dependence.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mineral_substitution_feasibility, empirical, 'Feasibility of alternative battery chemistries breaking mineral monopoly').

omega_variable(
    recycling_scale_trajectory,
    'Will closed-loop battery recycling achieve sufficient scale (>50% of demand) to reduce virgin mineral extraction dependence before mineral scarcity forces price collapse and stranded assets?',
    'Recycling infrastructure buildout tracking, secondary material cost curves vs virgin material, policy enforcement of producer responsibility for end-of-life batteries. Critical inflection point: when recycled material can undercut virgin extraction at scale.',
    'If recycling scales: constraint transitions toward rope or scaffold (alternative supply pathway reduces monopoly control). If recycling stalls: virgin extraction monopolies intensify; nations remain trapped in supplier dependence.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(recycling_scale_trajectory, empirical, 'Whether battery recycling reaches sufficient scale to reduce virgin mineral dependence').

omega_variable(
    resource_nationalism_coordination,
    'Can mineral-rich nations coordinate as a CARTEL to maintain supply discipline and enforce price floors, or will individual nations break ranks to maximize short-term export revenue (prisoner''s dilemma dynamic)?',
    'Historical cartel behavior analysis (OPEC, copper cartel dynamics); current policy alignment among DRC, Chile, Indonesia. If coordination breaks down, mineral competition increases and extraction mechanism weakens; if coordination strengthens, monopoly power increases.',
    'If cartel succeeds: extraction mechanism intensifies; victims experience higher-cost lock-in. If cartel fails: competition reduces prices and breaks monopoly power; constraint transitions toward rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(resource_nationalism_coordination, conceptual, 'Whether mineral-rich nations can coordinate as effective cartel').

omega_variable(
    environmental_externality_pricing,
    'Will regulatory frameworks (carbon pricing, water scarcity fees, ecosystem damage bonds) internalize environmental costs of extraction, raising virgin mineral prices to exceed recycled material cost parity?',
    'Policy implementation tracking; comparative cost analysis of virgin vs recycled material with and without environmental pricing. If externalities are priced, recycling becomes economically dominant; if not, extraction remains subsidized.',
    'If externalized: constraint reclassifies toward rope or scaffold (recycling becomes economically viable alternative, breaking mineral monopoly). If internalization fails: environmental commons continues to absorb suppression costs; snare characteristics intensify.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(environmental_externality_pricing, preference, 'Whether environmental externalities will be priced into mineral extraction').

omega_variable(
    labor_organizing_capacity,
    'Can mining communities organize collective action (unions, political representation, resource nationalism) to capture greater share of extraction value and enforce labor/environmental standards?',
    'Labor organizing activity, wage/benefit tracking relative to commodity prices, policy capture by mining communities (vs capture by corporations). If communities successfully organize, extraction mechanism weakens; if repression succeeds, communities remain trapped.',
    'If organizing succeeds: suppression decreases, exit options improve, constraint transitions toward tangled_rope from snare perspective. If organizing fails: communities remain maximally trapped; snare classification persists from their perspective.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(labor_organizing_capacity, empirical, 'Whether mining communities can effectively organize collective action').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(battery_supply_chain_security, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(batt_tr_t0, battery_supply_chain_security, theater_ratio, 0, 0.42).
narrative_ontology:measurement(batt_tr_t8, battery_supply_chain_security, theater_ratio, 8, 0.5).
narrative_ontology:measurement(batt_tr_t15, battery_supply_chain_security, theater_ratio, 15, 0.58).
narrative_ontology:measurement(batt_tr_t20, battery_supply_chain_security, theater_ratio, 20, 0.65).

% Extraction over time
narrative_ontology:measurement(batt_be_t0, battery_supply_chain_security, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(batt_be_t8, battery_supply_chain_security, base_extractiveness, 8, 0.47).
narrative_ontology:measurement(batt_be_t15, battery_supply_chain_security, base_extractiveness, 15, 0.58).
narrative_ontology:measurement(batt_be_t20, battery_supply_chain_security, base_extractiveness, 20, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(battery_supply_chain_security, resource_allocation).
narrative_ontology:affects_constraint(battery_supply_chain_security, geopolitical_mineral_coercion).
narrative_ontology:affects_constraint(battery_supply_chain_security, energy_transition_feasibility).
narrative_ontology:affects_constraint(battery_supply_chain_security, labor_organizing_capacity_mining).

% DUAL FORMULATION NOTE:
% The battery supply chain constraint family decomposes into multiple structurally distinct constraints: (1) mineral_monopoly_pricing (ε ≈ 0.62, snare from nations' perspective) — geological concentration enables extraction through supply discipline; (2) labor_suppression_in_mining (ε ≈ 0.70, snare) — communities trapped, suppressed, powerless; (3) environmental_externalization (ε ≈ 0.55, tangled_rope) — genuine coordination of supply requires environmental cost externalization; (4) geopolitical_coercion_via_minerals (ε ≈ 0.65, tangled_rope) — coordinated supply depends on state alignment; coercion is the enforcement mechanism. Each story has distinct observables and victim/beneficiary patterns. The present story (battery_supply_chain_security) integrates across all four at the global supply chain level; upstream stories should address component mechanisms separately.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(battery_supply_chain_security, organized, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
