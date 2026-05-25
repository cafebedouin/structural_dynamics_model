% ============================================================================
% CONSTRAINT STORY: geopolitical_compute_dominance
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_geopolitical_compute_dominance, []).

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
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: geopolitical_compute_dominance
 *   human_readable: Geopolitical Compute Dominance and Strategic Asymmetry
 *   domain: geopolitical/technology/economic
 *
 * SUMMARY:
 *   Geopolitical compute dominance represents a structural asymmetry where
 *   control over advanced semiconductor fabrication, AI training
 *   infrastructure, and quantum research capacity translates directly into
 *   military, intelligence, and economic power. This constraint manifests
 *   through supply chain control (Taiwan, South Korea, Netherlands
 *   bottlenecks), export control regimes (CHIPS Act, strategic semiconductor
 *   restrictions), and technological lock-in (AI models trained on
 *   dominant-state infrastructure become dependent on continued access). The
 *   constraint exhibits all six DR types from different perspectives: trapped
 *   nations perceive Snare; coordinating regional coalitions perceive Tangled
 *   Rope; semiconductor exporters perceive pure Rope; decentralization
 *   movements perceive Scaffold; legacy export regimes perceive themselves as
 *   Piton; dominant states experience Tangled Rope through alliance
 *   constraints. The extractiveness has increased over two decades
 *   (0.42→0.68) as AI and autonomous systems have made compute access
 *   strategically critical. Theater ratio remains moderate (0.55) because
 *   enforcement mechanisms operate partly through formal export controls
 *   (performative) and partly through supply chain interdiction (functional).
 *
 * KEY AGENTS:
 *   - Compute-Dominant States: Institutional beneficiary (powerful/mobile) — United States, Israel, potentially allies. Extract through first-mover advantage in AI, military autonomy, and intelligence asymmetry.
 *   - Compute-Dependent Nations: Primary victim (powerless/trapped) — Nations without domestic chip fabrication or major AI research capacity. Cannot develop autonomous systems without access to dominant-state infrastructure.
 *   - Semiconductor Exporters: Secondary beneficiary (institutional/arbitrage) — Taiwan, South Korea, Netherlands. Coordinate production and maintain market pricing power while serving both sides of geopolitical divide.
 *   - Technology Access Restricted Nations: Secondary victim (organized/constrained) — China, Russia, Iran, others under sanction. Face explicit export controls and supply chain restrictions; investing heavily in alternative substrates.
 *   - Regional Technology Coalitions: Tertiary beneficiary (organized/constrained) — EU, ASEAN, India initiatives. Building alternative chip design and foundry capacity with significant capital investment but facing generational timelines.
 *   - Decentralization Movements: Organized agent (organized/constrained) — Edge computing, quantum research, photonic processor initiatives. See alternative compute substrates as sunset path out of silicon dominance.
 *   - Analytical Observer: Analytical context (analytical/identity_locked) — Cannot fully step outside compute infrastructure dependency to analyze constraint. Observer's own analytical capacity depends on compute systems controlled by dominant states.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(geopolitical_compute_dominance, 0.68).
domain_priors:suppression_score(geopolitical_compute_dominance, 0.72).
domain_priors:theater_ratio(geopolitical_compute_dominance, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(geopolitical_compute_dominance, extractiveness, 0.68).
narrative_ontology:constraint_metric(geopolitical_compute_dominance, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(geopolitical_compute_dominance, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(geopolitical_compute_dominance, tangled_rope).
narrative_ontology:human_readable(geopolitical_compute_dominance, "Geopolitical Compute Dominance and Strategic Asymmetry").
narrative_ontology:topic_domain(geopolitical_compute_dominance, "geopolitical/technology/economic").

domain_priors:requires_active_enforcement(geopolitical_compute_dominance).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(geopolitical_compute_dominance, compute_dominant_states).
narrative_ontology:constraint_beneficiary(geopolitical_compute_dominance, semiconductor_exporters).
narrative_ontology:constraint_beneficiary(geopolitical_compute_dominance, ai_capability_leaders).
narrative_ontology:constraint_victim(geopolitical_compute_dominance, compute_dependent_states).
narrative_ontology:constraint_victim(geopolitical_compute_dominance, technology_access_restricted_nations).
narrative_ontology:constraint_victim(geopolitical_compute_dominance, emerging_economies).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: COMPUTE-DEPENDENT NATION (SNARE) — Nations without domestic semiconductor fabrication or AI training infrastructure face structural entrapment. Cannot develop autonomous military, intelligence, or economic systems without compute access. Export controls, chip bans, and supply chain restrictions create asymmetric vulnerability. Maximum suppression through denial of alternatives. No meaningful exit without decades of capital investment and knowledge accumulation.
constraint_indexing:constraint_classification(geopolitical_compute_dominance, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: REGIONAL TECHNOLOGY COALITION (TANGLED ROPE) — States organizing collective chip design and foundry initiatives (e.g., RISC-V, European processor initiatives) benefit from coordination of technical standards and shared development costs, but remain constrained by capital requirements and process node catch-up timelines. Experience mixed extraction: dominant states extract through standards that entrench their advantage, while coordination enables some regional capability development.
constraint_indexing:constraint_classification(geopolitical_compute_dominance, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: SEMICONDUCTOR EXPORTING ALLIANCE (ROPE) — Taiwan, South Korea, and Netherlands coordinate advanced chip production through technical standards and supply agreements. Experience the compute dominance constraint as pure coordination: setting specs, verifying safety, managing supply chains. Institutional arbitrage—can renegotiate terms with buyers. Net beneficiary of the constraint structure through market pricing power.
constraint_indexing:constraint_classification(geopolitical_compute_dominance, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: DECENTRALIZATION MOVEMENT (SCAFFOLD) — Edge computing, quantum computing development, photonic processors, and neuromorphic systems represent genuinely alternative compute substrates with sunset logic. As these technologies mature, dependence on traditional semiconductor dominance erodes. Extraction is tolerated because organized actors (research consortia, consortia like IMEC) see an exit path with declining cost over 15-30 year horizon.
constraint_indexing:constraint_classification(geopolitical_compute_dominance, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: LEGACY TRADE REGIME (PITON) — Traditional COCOM lists, export control bureaucracies, and Cold War-era technology gatekeeping persist through institutional inertia despite reduced functional enforcement. The rituals (export license reviews, technical committee vetting) remain theatrical—actual enforcement happens through unilateral sanctions and supply chain interdiction, not through the formal mechanisms. Theater ratio high because the nominal coordination (multilateral export controls) is largely performative while real extraction flows through bilateral economic pressure.
constraint_indexing:constraint_classification(geopolitical_compute_dominance, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: DOMINANT COMPUTE STATE (TANGLED ROPE) — United States (and to lesser extent allies) benefit from compute dominance through military, intelligence, and economic asymmetries. But also constrained by alliance coordination requirements, manufacturing bottleneck risks, and need to maintain technical standards sufficient for other states to participate in the market. Mobile exit options (can redirect compute exports to allies, invest domestically) but constrained by need to maintain coalition coherence and prevent adversary leapfrogging.
constraint_indexing:constraint_classification(geopolitical_compute_dominance, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (SNARE with identity_locked exit) — At civilizational scale, the analytical observer recognizes compute dominance as a structural feature of technological development: knowledge of advanced manufacturing is geographically concentrated (Taiwan, South Korea, Netherlands), creating path dependencies that are difficult to escape. The observer's own capacity to analyze the problem is constrained by dependence on compute infrastructure of dominant states. This is identity_locked: the analytical position itself is constituted through access to compute systems controlled by dominant powers. The observer cannot step outside the constraint to fully analyze it.
constraint_indexing:constraint_classification(geopolitical_compute_dominance, snare,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(identity_locked),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(geopolitical_compute_dominance_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(geopolitical_compute_dominance, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(geopolitical_compute_dominance, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(geopolitical_compute_dominance, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(geopolitical_compute_dominance, TR),
    TR >= 0.70.

:- end_tests(geopolitical_compute_dominance_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High and increasing. The constraint extracts through multiple mechanisms: (1) Access asymmetry—dominant states can deny compute resources to competitors; (2) Knowledge lock-in—manufacturing knowledge concentrated in allied jurisdictions creates path dependency; (3) Alliance capture—dependent states must align politically to maintain access, surrendering policy autonomy; (4) Capability gap—military and intelligence asymmetries compound as compute becomes central to autonomous systems. The 26-point increase over 20 years reflects the strategic importance of AI and autonomous systems rising dramatically. Suppression (0.72): High. Barriers to exit are structural (billions in capital required for leading-edge fabs, 10-20 year technology gaps, concentrated materials supply). Active enforcement through export controls adds policy suppression. But suppression is not absolute—some alternative pathways exist (open ISAs, older process nodes, quantum computing). Theater ratio (0.55): Moderate. Formal mechanisms (COCOM lists, export licensing, multilateral agreements) are partly performative—actual enforcement happens through bilateral sanctions and supply chain control. Open-source hardware initiatives and public R&D create theater (symbolic moves toward decentralization) while structural dominance persists.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates how a single structural mechanism (control over advanced manufacturing) generates radically different classifications depending on the observer's position. The powerless nation sees entrapment (Snare). The dominant state sees alliance coordination (Tangled Rope, not Snare—they are constrained by need to maintain coalition and prevent supplier dependencies). The semiconductor exporter sees pure coordination (Rope—no asymmetric extraction experienced by them, only coordination of supply). The regional coalition sees mixed extraction and benefit (Tangled Rope). The decentralization movement sees temporary barrier with exit path (Scaffold). The legacy trade regime sees itself as degraded ritual (Piton). The analytical observer at civilizational scale risks naturalizing the constraint as an immutable feature of technological development, but the structural data contradicts this: the dominance is contingent on specific geopolitical alignments, specific manufacturing locations, and specific technology trajectories—all of which are shifting.
 *
 * DIRECTIONALITY LOGIC:
 *   The key directionality insight is that compute dominance is not unidirectional extraction but a complex flow with different directions for different agents. (1) Trapped nations: d ≈ 0.95 (maximum target, experience full suppression, no arbitrage). (2) Dominant states: d ≈ 0.15 (net beneficiary, but constrained by alliance management and supplier dependency—cannot fully exploit dominance without risking coalition collapse or supply chain retaliation). (3) Semiconductor exporters: d ≈ 0.20 (beneficiary but with arbitrage—can shift clients, renegotiate terms). (4) Regional coalitions: d ≈ 0.55 (symmetric—benefit from standards coordination but constrained by need to adopt dominant-state architectures). (5) Decentralization movements: d ≈ 0.40 (constrained by capital requirements but with exit path—d lower than permanent trapped agents). No directionality overrides needed—the derivation from beneficiary/victim declarations and exit options produces accurate d values that generate the perspectival gap.
 *
 * MANDATROPHY ANALYSIS:
 *   Compute dominance resolves the mandatrophy by showing that dominant-state beneficiaries experience Tangled Rope (coordination + extraction), NOT pure extraction (Snare). This is the key diagnostic: a powerful actor experiencing Tangled Rope is constrained by the very mechanism that grants them dominance. If the US is a net beneficiary of compute dominance (d ≈ 0.15), the classification should show low χ for the powerful agent's perspective. But the powerful agent's perspective shows TANGLED ROPE, not ROPE, because they are constrained by: (1) Alliance management—cannot shut out Taiwan/Korea without losing semiconductor supply and geopolitical partners; (2) Supply chain fragility—rare earths, advanced packaging, photolithography tools are concentrated in non-dominant suppliers; (3) Decentralization risk—quantum, edge, photonic substrates are reducing silicon centrality; (4) Export control enforcement costs—maintaining restrictions requires constant intelligence/interdiction. The Tangled Rope classification for the dominant state prevents false naturalization of 'American dominance' as inevitable or immutable—it shows that dominance is actively maintained through coordination mechanisms that can degrade, fragment, or be circumvented.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    open_source_erosion_of_dominance,
    'Does open-source hardware design (RISC-V, OpenPOWER) and distributed manufacturing reduce compute dominance extraction, or do process node advantages in leading-edge fabs create a permanent tier structure?',
    'Track adoption of open ISAs in military/AI applications; measure performance delta between open designs and proprietary designs at equivalent fab nodes; assess whether emerging economies can achieve strategic autonomy through open designs on 10+ year nodes',
    'If open designs achieve parity: compute dominance regresses to Rope (pure coordination around standards). If process node gap persists: dominance remains Snare (trapped by inability to access cutting-edge fabrication regardless of design).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(open_source_erosion_of_dominance, empirical, 'Whether open-source hardware design can overcome process node dominance').

omega_variable(
    quantum_computing_timeline_disruption,
    'Does quantum computing development on a 10-15 year timeline create a genuine alternative compute substrate that reduces silicon dominance, or is quantum computing itself captured by dominant states?',
    'Monitor quantum chip development in non-dominant states (China, EU); assess whether quantum advantage for cryptanalysis/optimization emerges before or after dominant states achieve quantum advantage; track IP and export control regimes for quantum systems',
    'If quantum emerges in non-dominant states first: compute dominance fragmentizes into dual-substrate system (Tangled Rope from multiple perspectives). If captured by dominant states: quantum becomes an amplification mechanism for existing dominance (Snare extraction increases).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(quantum_computing_timeline_disruption, empirical, 'Whether quantum computing disrupts silicon dominance or amplifies it').

omega_variable(
    intelligence_services_dependency_lock,
    'Do intelligence agencies in dependent states become structurally bound to dominant-state intelligence sharing (Five Eyes, NATO signals intelligence) through compute dependency, creating identity_locked exit options for state institutions?',
    'Declassified intelligence agreements; assessment of intelligence agency budget allocation to domestic vs allied systems; case studies of attempted exit from intelligence partnerships and technical barriers encountered',
    'If intelligence identity_lock is confirmed: compute dominance is not primarily a military/economic constraint but a surveillance & sovereignty constraint. Classification shifts from Snare (external barriers) to identity_locked (internal institutional identity constituted through dependence). This would imply deeper structural resistance to escape than military alone suggests.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(intelligence_services_dependency_lock, empirical, 'Whether compute dependency creates identity-locked intelligence agency relationships').

omega_variable(
    supply_chain_fragility_reversal,
    'Does the transition from silicon abundance (2010-2020) to silicon scarcity (2020-2025) reverse the directionality of compute dominance extraction, making dominant states dependent on intermediate suppliers for bottleneck resources?',
    'Track control over rare earth elements, advanced packaging materials, and photolithography tools; assess who holds single-point-of-failure suppliers in the compute stack; monitor substitution pathways for critical materials',
    'If scarcity creates dependency on non-dominant suppliers: extraction may reverse or redistribute across the supply chain. Classification could shift from powerless→trapped to organized→constrained for intermediate suppliers. Dominant states may face Tangled Rope constraints on their own dominance if they become dependent on critical input suppliers.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(supply_chain_fragility_reversal, empirical, 'Whether supply chain bottlenecks reverse dominance extraction direction').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(geopolitical_compute_dominance, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(geompute_tr_t0, geopolitical_compute_dominance, theater_ratio, 0, 0.38).
narrative_ontology:measurement(geompute_tr_t10, geopolitical_compute_dominance, theater_ratio, 10, 0.48).
narrative_ontology:measurement(geompute_tr_t20, geopolitical_compute_dominance, theater_ratio, 20, 0.55).
narrative_ontology:measurement(geompute_tr_t5, geopolitical_compute_dominance, theater_ratio, 5, 0.42).

% Extraction over time
narrative_ontology:measurement(geompute_be_t0, geopolitical_compute_dominance, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(geompute_be_t10, geopolitical_compute_dominance, base_extractiveness, 10, 0.58).
narrative_ontology:measurement(geompute_be_t20, geopolitical_compute_dominance, base_extractiveness, 20, 0.68).
narrative_ontology:measurement(geompute_be_t5, geopolitical_compute_dominance, base_extractiveness, 5, 0.5).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(geopolitical_compute_dominance, global_infrastructure).
narrative_ontology:affects_constraint(geopolitical_compute_dominance, semiconductor_supply_chain_concentration).
narrative_ontology:affects_constraint(geopolitical_compute_dominance, artificial_intelligence_training_asymmetry).
narrative_ontology:affects_constraint(geopolitical_compute_dominance, quantum_computing_race).
narrative_ontology:affects_constraint(geopolitical_compute_dominance, export_control_regime_enforcement).

% DUAL FORMULATION NOTE:
% Compute dominance is upstream of and influences multiple domain-specific constraints: semiconductor supply chain concentration (manufacturing bottleneck), AI training asymmetry (compute access determines training scale), quantum computing race (alternative substrate competition), and export control enforcement (policy mechanism). Each downstream constraint has its own ε value reflecting domain-specific extractiveness. This story captures the overarching geopolitical structure; domain-specific stories capture implementation.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
