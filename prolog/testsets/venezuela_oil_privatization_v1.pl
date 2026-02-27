% ============================================================================
% CONSTRAINT STORY: venezuela_oil_privatization_v1
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_venezuela_oil_privatization_v1, []).

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
 *   constraint_id: venezuela_oil_privatization_v1
 *   human_readable: Shadow Privatization of Venezuela's Oil Sector
 *   domain: geopolitical/economic
 *
 * SUMMARY:
 *   Venezuela's oil sector has undergone structural inversion from
 *   state-managed public resource to shadow privatization network. Responding
 *   to crippling U.S. sanctions (2017-present) and collapsing formal
 *   institutions, the regime shifted from centralized extraction through
 *   PDVSA to decentralized networks where military factions, foreign trading
 *   firms, and sanctions-evading intermediaries extract crude and sell it on
 *   global markets outside official channels. The state captures no revenue;
 *   ordinary citizens face fuel rationing despite living atop 300+ billion
 *   barrels of proven reserves; PDVSA employees watch crude they extract flow
 *   to parallel networks controlled by military competitors. This constraint
 *   exhibits snare characteristics from the perspective of trapped citizens
 *   and workers, tangled rope dynamics from military and foreign firms (who
 *   both benefit and face coercive constraints), and piton characteristics
 *   from the degraded official institutions that maintain performative
 *   control while real extraction happens elsewhere. The growth in
 *   theater_ratio from 0.42 to 0.65 reflects the increasing gap between
 *   regime claims of state control and the actual privatized extraction
 *   happening outside official channels. The extractiveness rise from 0.32 to
 *   0.58 reflects how sanctions and institutional collapse have intensified
 *   the parasitic extraction mechanism — more crude is produced and sold, but
 *   less reaches legitimate state use.
 *
 * KEY AGENTS:
 *   - Venezuelan ordinary citizens: Primary victim (powerless/trapped) — face fuel rationing despite living on oil reserves; dependent on regime distribution networks with no alternatives
 *   - PDVSA workers and oil field operators: Secondary victim (moderate/constrained) — produce crude that disappears into parallel channels; wages collapse, workplace degrades, zero revenue benefit
 *   - Military factions and security services: Primary beneficiary (organized/constrained) — control fuel distribution, operate parallel trading networks, capture rents from shadow extraction; benefit but constrained by sanctions and regime fragility
 *   - Foreign oil trading firms and extraction companies: Secondary beneficiary (institutional/arbitrage) — purchase crude at discounted rates through sanctions-evading channels, refine or resell at global market prices, capture arbitrage spreads; high exit options
 *   - U.S./coalition sanctions regime: Coercive actor (powerful/mobile) — imposes sanctions architecture that redirects oil flows toward shadow networks; exhibits both coordination (allied enforcement) and extraction (humanitarian costs)
 *   - Official PDVSA and regime government: Degraded institution (institutional/arbitrage) — maintains performative control fiction; actual extraction and distribution delegated to military networks; theater ratio increases as gap between claims and reality widens
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(venezuela_oil_privatization_v1, 0.58).
domain_priors:suppression_score(venezuela_oil_privatization_v1, 0.72).
domain_priors:theater_ratio(venezuela_oil_privatization_v1, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(venezuela_oil_privatization_v1, extractiveness, 0.58).
narrative_ontology:constraint_metric(venezuela_oil_privatization_v1, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(venezuela_oil_privatization_v1, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(venezuela_oil_privatization_v1, snare).
narrative_ontology:human_readable(venezuela_oil_privatization_v1, "Shadow Privatization of Venezuela's Oil Sector").
narrative_ontology:topic_domain(venezuela_oil_privatization_v1, "geopolitical/economic").

domain_priors:requires_active_enforcement(venezuela_oil_privatization_v1).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(venezuela_oil_privatization_v1, foreign_extraction_firms).
narrative_ontology:constraint_beneficiary(venezuela_oil_privatization_v1, regime_military_factions).
narrative_ontology:constraint_beneficiary(venezuela_oil_privatization_v1, sanctions_evading_networks).
narrative_ontology:constraint_victim(venezuela_oil_privatization_v1, venezuelan_state_revenue).
narrative_ontology:constraint_victim(venezuela_oil_privatization_v1, ordinary_citizens).
narrative_ontology:constraint_victim(venezuela_oil_privatization_v1, pdvsa_workers).
narrative_ontology:constraint_victim(venezuela_oil_privatization_v1, fuel_access).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: VENEZUELAN ORDINARY CITIZEN (SNARE) — Trapped in the collapsing economy. Cannot exit the fuel rationing, inflation spiral, or dependence on government distribution. Bears the full extraction cost — starved of gasoline in the world's most oil-rich nation, dependent on military-controlled distribution networks, with zero alternatives. Maximum experienced extraction: cannot organize, cannot exit, cannot escape the system's extraction logic.
constraint_indexing:constraint_classification(venezuela_oil_privatization_v1, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: PDVSA WORKERS AND FIELD OPERATORS (SNARE) — Organized as a workforce but constrained by capital control, salary confiscation, and the absence of alternative employment. The shadow privatization extraction bypasses them — they extract crude but see zero benefit as state revenues disappear into parallel financial channels. Constrained exit (immigration possible but costly); high experienced extraction (wages worthless, workplace degrading, no control over resource they produce).
constraint_indexing:constraint_classification(venezuela_oil_privatization_v1, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: REGIME MILITARY FACTIONS AND SECURITY SERVICES (TANGLED ROPE) — Organized and powerful domestically. Benefit from control of fuel distribution and illicit trade networks (extraction toward this actor). But constrained by international sanctions and dependence on regime survival — if the oil system collapses entirely, their power base evaporates. Exhibit both extraction (they capture rents) and coordination (they manage distribution networks, however predatory). Active enforcement through violence (checkpoints, fuel rationing, detention of rivals). Asymmetric — they benefit while society bears costs.
constraint_indexing:constraint_classification(venezuela_oil_privatization_v1, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: FOREIGN OIL EXTRACTION FIRMS AND TRADING NETWORKS (ROPE) — Institutional actors with exit options (can walk away if sanctions tighten or legal risk increases). Benefit from massive arbitrage: purchasing crude at prices far below market rate, refining or trading it globally, and capturing the spread. The constraint from their perspective is a pure coordination problem — how to extract without triggering enforcement action. They solve it through layered fronts (flag of convenience tankers, shell companies, trading hubs in sanctions-gray jurisdictions). No significant victims from their perspective; this is efficient resource allocation. Extractiveness toward them, not from them.
constraint_indexing:constraint_classification(venezuela_oil_privatization_v1, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: OFFICIAL PDVSA AND GOVERNMENT OIL MINISTRY (PITON) — Institutional structure that once had a clear coordination function (managing resource extraction and state revenue). Now substantially performative: maintains a facade of state control (official export contracts, ministry announcements, statistics) while actual production flows through parallel channels controlled by military/regime networks. Theater ratio high (0.65): regime claims to manage the oil sector, PDVSA publishes production figures, but real extraction and sales happen outside official channels. The original coordination function has atrophied; the constraint persists through institutional inertia and the performative need to claim legitimacy.
constraint_indexing:constraint_classification(venezuela_oil_privatization_v1, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: UNITED STATES AND COALITION SANCTIONS REGIME (TANGLED ROPE) — Powerful actors with high exit options (can escalate, de-escalate, or shift sanctions design). Imposed sanctions to coerce regime change but created the shadow privatization dynamic: unable to prevent extraction, only to redirect it through informal channels. Exhibit both coordination (allied governments enforcing a unified sanctions posture) and extraction (sanctions harm ordinary Venezuelans disproportionately, creating humanitarian costs that benefit coercive actors by strengthening blame assignment narratives). Asymmetric: powerful nations control the constraint architecture; ordinary Venezuelans bear costs. Active enforcement through secondary sanctions and interdiction operations.
constraint_indexing:constraint_classification(venezuela_oil_privatization_v1, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / GLOBAL POLITICAL ECONOMY VIEW (SNARE) — From a civilizational/global perspective, shadow privatization is a structural snare: it extracts value from both the Venezuelan state (via foregone revenue) and ordinary citizens (via fuel scarcity and economic collapse) while concentrating benefits in parallel networks immune to democratic or legal accountability. The constraint exhibits no coordination function at the global level — foreign firms, military factions, and sanctions enforcers all extract, none provide genuine public goods. Pure extraction masquerading as geopolitical necessity.
constraint_indexing:constraint_classification(venezuela_oil_privatization_v1, snare,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(venezuela_oil_privatization_v1_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(venezuela_oil_privatization_v1, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(venezuela_oil_privatization_v1, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(venezuela_oil_privatization_v1, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(venezuela_oil_privatization_v1, TR),
    TR >= 0.70.

:- end_tests(venezuela_oil_privatization_v1_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High and rising. The constraint extracts from citizens (fuel scarcity), from the state (lost revenue), and from workers (appropriated wages). Base extraction increased from 0.32 to 0.58 because sanctions and institutional collapse intensified the predatory extraction mechanism — military factions now extract crude and redistribute only through patronage networks, not market or welfare mechanisms. This is measurable extraction (fuel disappears, revenue disappears, purchasing power collapses) with minimal coordination value to victims. Suppression (0.72): Very high. Citizens cannot exit (capital controls, emigration barriers); workers cannot leave (skill specificity, economic desperation, visa constraints); alternative fuel sources don't exist (Venezuela has no alternatives to PDVSA crude for domestic consumption). Suppression reflects structural barriers to exit, not ideological suppression — the regime uses violent checkpoints and rationing, but the core suppression is economic and geographic. Theater ratio (0.65): Moderate-high. The regime maintains a facade of state control: PDVSA publishes production figures, the oil ministry issues directives, official export contracts are announced. But actual extraction and sales happen in parallel networks — tankers with flag-of-convenience registrations, trading through intermediaries in sanctions-gray jurisdictions, sales to foreign firms that negotiate directly with military networks rather than government. Theater increased from 0.42 because the gap between official claims and actual distribution widened. The performative claim of state control is increasingly hollow.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates the maximum perspectival divergence in the corpus. The Venezuelan citizen sees pure extraction (Snare) — crude they live above is rationed away, they receive no benefit, no exit exists. The military faction sees mixed extraction and coordination (Tangled Rope) — they benefit from parallel networks but are constrained by fragility and sanctions. The foreign firm sees pure coordination (Rope) — solving the problem of how to access cheap oil, no victimization from their perspective. The U.S. sanctions enforcer sees coordination layered with coercive extraction (Tangled Rope) — unified sanctions architecture, but immense humanitarian costs that serve no coercive goal effectively. The official PDVSA sees degradation (Piton) — a once-functional institution now performative, maintained through inertia rather than function. The analytical observer sees structural snare — a system designed to extract from the powerless and cannot be reformed through negotiation. No single perspective dominates; the presheaf over observation sites reveals that ordinary citizens face maximal extraction while military and foreign actors face favorable coordination or pure arbitrage.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values (d) map each agent's structural position to the extraction flow. Citizens: d ≈ 0.95 (full target — powerless, trapped, maximum extraction). PDVSA workers: d ≈ 0.85 (victim + constrained exit, high extraction). Military factions: d ≈ 0.35 (beneficiary + organized power but constrained by regime fragility, mixed extraction and benefit). Foreign firms: d ≈ 0.10 (beneficiary + arbitrage exit, negative/beneficial extraction). U.S. sanctions regime: d ≈ 0.55 (powerful but achieving opposite of intent, moderate extraction from perspective's own goals). Directionality overrides not needed — the derivation chain from beneficiary/victim declarations and exit options produces accurate d values for each agent. The engine's sigmoid f(d) will compute chi for each perspective and reveal that citizens experience χ ≈ 1.2-1.4 (high experienced extraction) while foreign firms experience χ ≈ -0.15 (subsidy-like benefit).
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLVED: This is a genuine Snare (not mislabeled as Rope). The mandatrophy test asks: 'Could victims benefit from the constraint in any framing?' Answer: No. Citizens cannot reframe fuel rationing as coordination (it provides no collective benefit). Workers cannot reframe wage theft as coordination (it does not enable their work, it sabotages it). The constraint has no hidden coordination function — it is pure extraction with military factions and foreign firms capturing all benefits while ordinary citizens and the state bear all costs. The tangled rope perspective (military factions) confirms that even beneficiaries see active enforcement required and asymmetric benefit — they gain rents but must maintain coercive capacity to hold power. The piton perspective shows that official institutions are indeed theatrical — the regime must perform state control even as real extraction bypasses state channels. No reframing resolves this as rope; the mandatrophy is resolved at ε=0.58 because all perspectives except the beneficiary/external-actor perspectives confirm pure extraction logic. The analytical observer's snare classification is primary; all others are perspectival readings of the same underlying extraction architecture.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    parallel_network_boundaries,
    'What distinguishes the shadow privatization network from ordinary sanctions evasion or black-market trading? Where is the boundary between military corruption and regime economic strategy?',
    'Financial flow analysis (banking records, shipping manifests, fuel distribution tracking); interviews with defected military/finance officials; comparison of regime directives with actual extraction patterns',
    'If shadow privatization is coordinated regime strategy: constraint is state-level snare (regime extracts from citizens via controlled scarcity). If it is decentralized military faction predation: constraint is warlordism/state collapse (multiple snares). Classification and mandatrophy implications differ significantly.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(parallel_network_boundaries, empirical, 'Boundary between coordinated regime strategy and decentralized military corruption').

omega_variable(
    sanctions_causality_attribution,
    'How much of the shadow privatization and fuel scarcity is caused by sanctions versus by mismanagement and corruption predating sanctions? Do sanctions create the incentive structure or merely expose it?',
    'Counterfactual analysis comparing fuel production/distribution efficiency before/after sanctions; economic modeling of investment decisions; comparison with other petrostates under different sanction regimes',
    'If sanctions are primary cause: U.S./coalition perspective shifts from tangled rope toward rope (coordination problem) or even mountain (unavoidable consequence). If corruption and underinvestment predated sanctions: constraint is endogenous to regime, and external actors see snare dynamics they did not create.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sanctions_causality_attribution, empirical, 'Attribution of shadow privatization and scarcity to sanctions versus regime mismanagement').

omega_variable(
    exit_capacity_of_workers,
    'Do Venezuelan oil workers face truly trapped exit, or is constrained exit (emigration, sector switching) more accurate? What is the empirical cost of exit?',
    'Survey data on migration patterns, wage replacement in destination countries, skills transferability; comparison with trapped-exit constraints in other contexts (e.g., company towns, bonded labor)',
    'If truly trapped: PDVSA worker perspective is maximum snare (d ≈ 0.95). If constrained but feasible: perspective drops to tangled rope (d ≈ 0.55). Classification and chi values shift accordingly.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(exit_capacity_of_workers, empirical, 'Whether PDVSA workers face trapped or constrained exit options').

omega_variable(
    foreign_firm_coordination_level,
    'Are foreign oil trading firms and extraction companies operating as a coordinated cartel in shadow privatization, or as independent profit-seekers exploiting asymmetries? Is there governance structure or just opportunism?',
    'Network analysis of trading relationships, price-setting mechanisms, communication infrastructure; comparison with structured cartels (OPEC, drug trafficking organizations); detection of formal or informal agreements',
    'If coordinated cartel: foreign firms perspective is rope (pure coordination with minimal coercion). If opportunistic competitors: perspective is snare (pure extraction, no coordination benefit to competitors). Beneficiary/victim structure and beneficiary_type implications differ.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(foreign_firm_coordination_level, empirical, 'Whether foreign firms form coordinated cartel or operate as independent profit-seekers').

omega_variable(
    regime_survival_dependence,
    'How dependent is the Venezuelan regime on shadow privatization revenue for survival? Could it survive without parallel oil sales, or are these revenues essential to maintaining coercive capacity?',
    'Military payroll data, defense spending levels, comparison with counterfactual budgets; analysis of regime resilience under different resource scenarios; interviews with regime finance officials',
    'If essential: military factions'' perspective shifts from tangled rope toward mountain (unavoidable structural necessity). If marginal: perspective remains tangled rope (extractive choice, not necessity). Mandatrophy resolution and classification robustness differ.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regime_survival_dependence, empirical, 'Dependency of regime survival on shadow privatization revenue').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(venezuela_oil_privatization_v1, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(venez_oil_theater_t0, venezuela_oil_privatization_v1, theater_ratio, 0, 0.42).
narrative_ontology:measurement(venez_oil_theater_t5, venezuela_oil_privatization_v1, theater_ratio, 5, 0.56).
narrative_ontology:measurement(venez_oil_theater_t10, venezuela_oil_privatization_v1, theater_ratio, 10, 0.65).

% Extraction over time
narrative_ontology:measurement(venez_oil_extractiveness_t0, venezuela_oil_privatization_v1, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(venez_oil_extractiveness_t5, venezuela_oil_privatization_v1, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(venez_oil_extractiveness_t10, venezuela_oil_privatization_v1, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(venezuela_oil_privatization_v1, resource_allocation).
narrative_ontology:affects_constraint(venezuela_oil_privatization_v1, u_s_venezuela_sanctions_regime).
narrative_ontology:affects_constraint(venezuela_oil_privatization_v1, pdvsa_institutional_collapse).
narrative_ontology:affects_constraint(venezuela_oil_privatization_v1, military_elite_fragmentation).
narrative_ontology:affects_constraint(venezuela_oil_privatization_v1, fuel_access_inequality_latin_america).

% DUAL FORMULATION NOTE:
% Shadow privatization of Venezuela's oil is downstream of the institutional collapse of PDVSA and the imposition of U.S. sanctions. Each upstream constraint has its own extractiveness profile; this constraint represents the emergent extraction mechanism that arises from their combination. The network links show how institutional degradation + external coercion → shadow privatization snare.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
