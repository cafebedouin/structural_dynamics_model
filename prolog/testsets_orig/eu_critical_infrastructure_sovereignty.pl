% ============================================================================
% CONSTRAINT STORY: eu_critical_infrastructure_sovereignty
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_eu_critical_infrastructure_sovereignty, []).

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
 *   constraint_id: eu_critical_infrastructure_sovereignty
 *   human_readable: EU Critical Infrastructure Sovereignty and Energy Dependency
 *   domain: geopolitical/energy_security/institutional
 *
 * SUMMARY:
 *   EU critical infrastructure sovereignty represents a geopolitical
 *   constraint that manifests as energy dependency, institutional
 *   coordination, and asymmetric leverage. The constraint emerges from Cold
 *   War infrastructure topology (Soviet-era unidirectional pipelines)
 *   interacting with modern EU institutional integration and post-Cold War
 *   geopolitical realignment. The EU aimed to create a unified energy market
 *   with coordination benefits, but inherited infrastructure locked the
 *   system into vendor dependence and asymmetric extraction paths. The
 *   constraint exhibits all six DR types from different perspectives: smaller
 *   states trapped in snare dynamics; larger coalitions experiencing tangled
 *   rope (coordination with extraction); EU institutions perceiving pure
 *   rope; green advocates seeing a temporary scaffold with sunset; legacy
 *   infrastructure persisting as piton through inertia; exporting states
 *   wielding tangled rope leverage. The theater ratio (0.68) reflects that
 *   much EU energy policy is performative: repeated declarations of
 *   diversification and renewable transition without corresponding
 *   infrastructure replacement speed, repeated sanctions that are
 *   circumvented, and repeated 'energy security' protocols that fail during
 *   actual crises. The extractiveness has risen from 0.35 to 0.58 over 20
 *   years as geopolitical weaponization of energy accelerated, with a slight
 *   decline visible at year 30 reflecting early renewable capacity deployment
 *   and alternative supplier diversification.
 *
 * KEY AGENTS:
 *   - Smaller EU States (Bulgaria, Hungary, Slovakia): Primary victims (powerless/trapped) — bear 80%+ energy import costs and face political coercion during supply disputes; structurally unable to reroute infrastructure unilaterally
 *   - Larger Importing States (Germany, Italy, France): Secondary agents (organized/constrained) — coordinate through EU protocols to negotiate pricing and diversify suppliers; retain significant agency through coalition but bear asymmetric enforcement costs on smaller members
 *   - EU Institutional Framework (Commission, Council): Institutional beneficiary (institutional/arbitrage) — coordinates unified energy policy, sets renewable targets, manages interconnections; experiences constraint primarily as coordination problem to solve
 *   - Russia / Energy-Exporting State: Primary beneficiary (powerful/arbitrage) — captures revenue from energy sales, leverages dependency for geopolitical concessions, maintains infrastructure control; has full optionality to redirect supply
 *   - Green Energy Coalition: Organized agents (organized/mobile) — advocates for renewable transition with explicit sunset timelines; perceives current constraint as temporary transitional state, not permanent structure
 *   - Soviet-Era Infrastructure Legacy: Institutional persistence (institutional/inert) — the inherited pipeline topology that perpetuates vendor lock-in through sunk costs and path dependence
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — sees constraint as contingent institutional arrangement mistaken for natural necessity; identifies false mountain in claims that energy dependency is structurally inevitable
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(eu_critical_infrastructure_sovereignty, 0.58).
domain_priors:suppression_score(eu_critical_infrastructure_sovereignty, 0.65).
domain_priors:theater_ratio(eu_critical_infrastructure_sovereignty, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(eu_critical_infrastructure_sovereignty, extractiveness, 0.58).
narrative_ontology:constraint_metric(eu_critical_infrastructure_sovereignty, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(eu_critical_infrastructure_sovereignty, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(eu_critical_infrastructure_sovereignty, tangled_rope).
narrative_ontology:human_readable(eu_critical_infrastructure_sovereignty, "EU Critical Infrastructure Sovereignty and Energy Dependency").
narrative_ontology:topic_domain(eu_critical_infrastructure_sovereignty, "geopolitical/energy_security/institutional").

domain_priors:requires_active_enforcement(eu_critical_infrastructure_sovereignty).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(eu_critical_infrastructure_sovereignty, eu_member_states).
narrative_ontology:constraint_beneficiary(eu_critical_infrastructure_sovereignty, energy_providers).
narrative_ontology:constraint_beneficiary(eu_critical_infrastructure_sovereignty, nato_structural_stability).
narrative_ontology:constraint_victim(eu_critical_infrastructure_sovereignty, energy_independence).
narrative_ontology:constraint_victim(eu_critical_infrastructure_sovereignty, regional_autonomy).
narrative_ontology:constraint_victim(eu_critical_infrastructure_sovereignty, smaller_states).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SMALLER EU STATE (SNARE) — States like Bulgaria, Hungary, Slovakia face near-total energy dependency on Russian gas infrastructure. Exit options are materially constrained: rerouting infrastructure is capital-prohibitive, alternative suppliers demand premium prices, and immediate substitution is impossible. The constraint extracts through price volatility, political leverage during supply crises, and forced policy alignment. No coordination benefit perceived — only coercion. This agent bears maximum extraction burden.
constraint_indexing:constraint_classification(eu_critical_infrastructure_sovereignty, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: COALITION OF IMPORTING STATES (TANGLED ROPE) — Larger importers (Germany, Italy, France) coordinate through EU energy protocols to negotiate bulk pricing and diversify suppliers, creating genuine coordination benefits. Simultaneously, the constraint extracts through vendor lock-in, infrastructure sunk costs, and geopolitical leverage. Active enforcement of energy treaties and sanctions coordination creates asymmetric costs for smaller states. Mixed extraction and coordination — agents have agency through coalition but bear real asymmetric burdens.
constraint_indexing:constraint_classification(eu_critical_infrastructure_sovereignty, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 3: EU INSTITUTIONAL FRAMEWORK (ROPE) — The European Commission and Council coordinate energy policy to solve collective action problems: price pooling, network interconnection standards, strategic storage mandates, and renewable transition planning. From the institutional perspective, the constraint is primarily coordinative — it enables 27 divergent energy markets to function as a unified negotiating actor. The framework has significant arbitrage options (pivot to LNG, renewables, energy efficiency standards). Perceives constraint as low-extraction coordination.
constraint_indexing:constraint_classification(eu_critical_infrastructure_sovereignty, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: GREEN ENERGY TRANSITION COALITION (SCAFFOLD) — Climate and renewable advocates see the current gas-dependent infrastructure as a temporary transitional state with a declared sunset: EU renewable energy targets (80% by 2050) and phase-out timelines create explicit exit pathways. The constraint's extraction mechanism (gas dependency, geopolitical leverage) decays as wind, solar, and nuclear capacity mature. High suppression now (infrastructure lock-in) but declining per-policy if deployment succeeds. Theater ratio moderately high — much performative climate commitment rhetoric versus actual deployment speed.
constraint_indexing:constraint_classification(eu_critical_infrastructure_sovereignty, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(continental))).

% PERSPECTIVE 5: SOVIET-ERA INFRASTRUCTURE LEGACY (PITON) — Cold War pipelines (Druzhba, Brotherhood, Nord Stream variants) were designed for political integration and economic control. Their structural form (unidirectional, geographically routed, vendor-locked) persists decades after the Soviet Union dissolved, not because they are optimal (modern networks would be bidirectional, diversified, interconnected) but through institutional inertia. The constraint is largely theatrical maintenance of the inherited system. Theater ratio high (0.68) — much policy theater about 'infrastructure modernization' with minimal actual replacement or rerouting. The piton persists because alternatives haven't fully replaced it and the cost of simultaneous replacement is prohibitive.
constraint_indexing:constraint_classification(eu_critical_infrastructure_sovereignty, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ENERGY-EXPORTING STATE (TANGLED ROPE) — Russia benefits from EU energy dependency (primary beneficiary) and coordinates supply through Gazprom contracts, creating legitimate coordination function (reliability, pricing, infrastructure investment). Simultaneously, uses energy leverage for geopolitical extraction — conditioning supply on policy alignment, weaponizing price volatility during crises, and supporting political actors that weaken EU cohesion. Active enforcement of contracts coupled with asymmetric leverage. High arbitrage optionality (alternative buyers in Asia, pivot to LNG). Mixed genuine coordination and extractive leverage.
constraint_indexing:constraint_classification(eu_critical_infrastructure_sovereignty, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (TANGLED ROPE) — From a civilizational perspective, the constraint is a genuine hybrid: energy infrastructure coordination is necessary (interdependent European economy requires reliable supply networks), but the specific historical topology (Soviet-era unidirectional pipes) has encoded geopolitical asymmetries that enable extraction. The constraint could be pure coordination (Rope) if infrastructure were redesigned (bidirectional, diversified, interconnected). Instead, sunk costs and path dependence perpetuate the Soviet legacy even as geopolitical alliances have inverted. The analytical view shows the constraint as contingent institutional arrangement mistaken for natural necessity (false mountain) — actually a tangled rope with a decaying coordination function and persistent extraction mechanism.
constraint_indexing:constraint_classification(eu_critical_infrastructure_sovereignty, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(eu_critical_infrastructure_sovereignty_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(eu_critical_infrastructure_sovereignty, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(eu_critical_infrastructure_sovereignty, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(eu_critical_infrastructure_sovereignty, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(eu_critical_infrastructure_sovereignty, TR),
    TR >= 0.70.

:- end_tests(eu_critical_infrastructure_sovereignty_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. Russia captures primary economic rents from energy sales (~$40-50B annually) and geopolitical concessions through leverage. However, extraction is not total (0.9+) because the EU retains some optionality through LNG imports, renewables, and alternative suppliers at premium cost. The measurement trajectory shows extractiveness rising from 0.35 to 0.58 as geopolitical weaponization accelerated post-2014, with stabilization at year 20 and slight decline at year 30 as renewable capacity and LNG infrastructure began offsetting Russian dependency. Suppression (0.65): High. Barriers to exit include: physical infrastructure sunk costs ($100B+ invested in Russian pipeline networks over 30 years); technical incompatibility (Soviet-era pipes cannot easily reverse flow or accept alternative suppliers); capital requirements for LNG terminals and storage ($10-20B for major state); political and coordination costs of simultaneous infrastructure replacement; and short-term economic shock from supply disruption. These barriers are real and material, not merely policy choices. Theater ratio (0.68): High. Much EU energy policy is performative: climate commitments stated as binding (renewable targets to 2050) but with weak enforcement; diversification protocols announced but slow-moving; sanctions on energy transactions repeatedly circumvented; infrastructure modernization promised but delayed. The theater increased from 0.45 to 0.68 as the gap between stated policy (energy independence by 2030) and actual deployment (still 35-40% Russian gas) widened. The constraint is partially performative institutional maintenance of inherited infrastructure rather than continuous active extraction.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates stark divergence between powerless and powerful perspectives. Smaller states (powerless/trapped) classify the constraint as snare: they experience pure coercion with no coordination benefit. The EU institutional framework (institutional/arbitrage) classifies it as rope: they solve genuine coordination problems (price pooling, network standards, strategic storage) with significant optionality. Russia (powerful/arbitrage) classifies it as tangled rope: they coordinate supply and investment while extracting geopolitical leverage. The green coalition (organized/mobile) sees it as scaffold with sunset: the constraint decays as renewable infrastructure matures. The piton perspective (institutional/inert) observes that Soviet-era topology persists not because it is optimal but through inertia and cost of replacement. The analytical view at civilizational scope reveals that much framing treats energy dependency as natural or inevitable (mountain) when it is actually contingent on inherited infrastructure topology and path-dependent choices. The perspectival gap is maximal because the same energy pipeline system is simultaneously: a solution to coordination (EU framework perspective), a source of coercion (smaller states perspective), a source of revenue (exporting state perspective), and a degraded relic (piton perspective).
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality (d) derives from structural position: power level, exit options, and beneficiary/victim status. Smaller states (powerless/trapped) have d→1.0 (full target): they depend on energy imports with no exit options and bear vulnerability to supply disruption and price volatility. The EU framework (institutional/arbitrage) has low d→0.1-0.2: it benefits from coordination while retaining optionality through LNG, renewables, and supplier diversification. Russia (powerful/arbitrage) has d→0.0-0.1: it is the primary beneficiary capturing economic rents and geopolitical leverage. Larger importing states (organized/constrained) have intermediate d→0.5-0.6: they both benefit from unified market pricing and bear asymmetric coordination costs. The green coalition (organized/mobile) has d→0.4-0.5: they are organized enough to influence policy but constrained by infrastructure inertia. The derived d values feed into the sigmoid f(d) to produce effective extractiveness chi per the formula χ = ε × f(d) × σ(S). For smaller states: χ = 0.58 × f(1.0) × 0.9 (regional scope) ≈ 0.92, confirming snare classification. For EU framework: χ = 0.58 × f(0.15) × 1.1 (global scope) ≈ 0.08, confirming rope classification. The directionality logic shows why the same constraint produces radically different classifications: the d value is constraint-relative, derived from structural position, not from global power rankings.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by revealing that the tangled rope classification is the stabilizing middle-ground perspective from which the institutional system functions, while the extremes (snare from smaller states, rope from EU institutions) represent radically incompatible experience. The mandatrophy question is: 'Is energy coordination worth the extraction cost?' The tangled rope classification preserves both the genuine coordination function (unified pricing, infrastructure standards, emergency allocation protocols) AND the asymmetric extraction (geopolitical leverage, vendor lock-in, asymmetric enforcement burden). If classified as pure rope, the constraint appears to benefit all parties equally through coordination — masking the actual harm to trapped states and the actual leverage available to Russia. If classified as pure snare, the constraint appears to benefit only Russia through coercion — masking the genuine coordination benefits that make energy trade possible at all. The tangled rope preserves both truths simultaneously: the constraint both solves coordination problems AND enables extraction. The mandatrophy resolves by showing that the scaffold perspective (green energy transition) represents a real exit pathway that would transform the constraint's character: if renewable transition succeeds, the extraction mechanism (energy dependency) decays while coordination benefits (technology standards, grid integration) persist in new form. The false summit (mountain perspective that energy dependency is inevitable) is the critical mandatrophy risk — it naturalizes what is actually a contingent infrastructure choice.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    renewable_transition_timeline_credibility,
    'Are EU renewable energy targets (80% by 2050) actually achievable, or are they aspirational theater masking continued gas dependency?',
    'Tracking deployment rates against required exponential growth curves; cost analysis of required storage and grid upgrades; political commitment through budget allocation and enforcement mechanisms',
    'If achievable: scaffold perspective is correct, extraction mechanism decays over 25 years. If aspirational: scaffold is piton (performative), constraint persists as tangled rope indefinitely.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(renewable_transition_timeline_credibility, empirical, 'Viability of EU renewable energy transition timeline').

omega_variable(
    energy_arbitrage_true_optionality,
    'Does the EU actually have meaningful arbitrage options to Russian gas, or is LNG/renewables substitution economically impossible at scale?',
    'Cost comparison of LNG vs pipeline gas, including infrastructure; renewable capacity growth rates against demand; feasibility analysis of simultaneous replacement of piped infrastructure',
    'If true arbitrage exists: EU states have genuine optionality (mobile exit), constraint is lower-suppression tangled rope. If false: EU states are actually trapped despite formal optionality, suppression should be higher.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(energy_arbitrage_true_optionality, empirical, 'Whether EU has true arbitrage options to Russian gas dependency').

omega_variable(
    extraction_mechanism_reversibility,
    'Can geopolitical leverage through energy dependency be decoupled from the infrastructure itself, or are they structurally inseparable?',
    'Historical comparison with other commodity dependencies (Middle East oil, Chinese rare earth minerals); analysis of whether diversified supply alone breaks political leverage or requires institutional redesign',
    'If decoupled: energy diversification alone solves the problem (scaffold perspective holds). If inseparable: must redesign infrastructure topology AND diversify supply; single-axis solutions fail.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraction_mechanism_reversibility, conceptual, 'Structural relationship between energy infrastructure topology and geopolitical leverage').

omega_variable(
    smaller_state_coalition_power_threshold,
    'At what threshold of coalitional organization do smaller energy-dependent states transition from snare (trapped) to tangled rope (constrained with agency)?',
    'Empirical analysis of coalition effectiveness in recent energy crises; measurement of shared negotiating power vs unilateral vulnerability; tracking of coordinated infrastructure projects (LNG terminals, interconnections)',
    'If threshold low (~3-5 coordinated states): coalition power is real, classification shifts toward tangled rope. If threshold high: smaller states remain structurally trapped despite formal cooperation mechanisms.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(smaller_state_coalition_power_threshold, empirical, 'Threshold for smaller state coalition transition from snare to tangled rope').

omega_variable(
    nato_structural_stability_beneficiary_ambiguity,
    'Does energy dependency on Russia actually undermine NATO stability, or does it create mutual deterrence that prevents conflict escalation?',
    'Historical analysis of energy leverage during NATO crises; modeling of escalation dynamics with vs without energy interdependency; comparison with other mutual dependency relationships',
    'If destabilizing: energy dependency is purely extractive (snare). If stabilizing: it provides deterrent function (genuine coordination), reclassifying as rope or tangled rope with lower effective extraction.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(nato_structural_stability_beneficiary_ambiguity, conceptual, 'Whether energy dependency provides NATO structural stability or enables extraction').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(eu_critical_infrastructure_sovereignty, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(eucis_tr_t0, eu_critical_infrastructure_sovereignty, theater_ratio, 0, 0.45).
narrative_ontology:measurement(eucis_tr_t10, eu_critical_infrastructure_sovereignty, theater_ratio, 10, 0.62).
narrative_ontology:measurement(eucis_tr_t20, eu_critical_infrastructure_sovereignty, theater_ratio, 20, 0.68).
narrative_ontology:measurement(eucis_tr_t30, eu_critical_infrastructure_sovereignty, theater_ratio, 30, 0.65).

% Extraction over time
narrative_ontology:measurement(eucis_be_t0, eu_critical_infrastructure_sovereignty, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(eucis_be_t10, eu_critical_infrastructure_sovereignty, base_extractiveness, 10, 0.5).
narrative_ontology:measurement(eucis_be_t20, eu_critical_infrastructure_sovereignty, base_extractiveness, 20, 0.58).
narrative_ontology:measurement(eucis_be_t30, eu_critical_infrastructure_sovereignty, base_extractiveness, 30, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(eu_critical_infrastructure_sovereignty, global_infrastructure).
narrative_ontology:boltzmann_floor_override(eu_critical_infrastructure_sovereignty, 0.18).
narrative_ontology:affects_constraint(eu_critical_infrastructure_sovereignty, semiconductor_supply_chain_eu).
narrative_ontology:affects_constraint(eu_critical_infrastructure_sovereignty, nato_european_strategic_autonomy).
narrative_ontology:affects_constraint(eu_critical_infrastructure_sovereignty, eu_renewable_energy_transition).

% DUAL FORMULATION NOTE:
% EU critical infrastructure sovereignty decomposes into at least three structurally distinct constraints: (1) energy infrastructure topology (this story), (2) semiconductor supply chain vulnerability (upstream — EU depends on TSMC/Samsung for advanced chips, enabling different extraction mechanisms), (3) NATO strategic autonomy (downstream — energy dependency constrains military decisioning). Each has different ε values and different beneficiary/victim structures. Link them because disruption in one affects all three: renewable transition affects energy independence affects NATO autonomy; semiconductor supply constraints affect weapons systems that depend on reliable energy; NATO decisions constrain energy policy choices.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(eu_critical_infrastructure_sovereignty, institutional, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
