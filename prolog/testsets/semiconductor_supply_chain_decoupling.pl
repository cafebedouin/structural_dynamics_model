% ============================================================================
% CONSTRAINT STORY: semiconductor_supply_chain_decoupling
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_semiconductor_supply_chain_decoupling, []).

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
 *   constraint_id: semiconductor_supply_chain_decoupling
 *   human_readable: Semiconductor Supply Chain Decoupling and Strategic Autonomy
 *   domain: geopolitical_economic/industrial_policy
 *
 * SUMMARY:
 *   Semiconductor supply chain decoupling represents the enforcement of
 *   geopolitical boundaries onto a technologically integrated global
 *   production network. Beginning with export controls on advanced chip
 *   technology to China (2019-2022), escalating through the CHIPS Act and EU
 *   Chips Act (2022-2023), and materializing in regional fab capacity
 *   buildouts, decoupling creates a structural constraint that simultaneously
 *   coordinates state security objectives and extracts from nations and
 *   actors without independent fab capacity. The constraint manifests as a
 *   tangled hybrid: genuine coordination exists within alliance blocs
 *   (standards alignment, allied sourcing), but extraction operates through
 *   geographic fragmentation, scarcity artificially maintained by export
 *   controls, and the concentration of cutting-edge fab capacity in
 *   politically aligned nations. The classification exhibits all six types
 *   across perspectives, revealing decoupling as a policy-enforced constraint
 *   with contingent rather than natural boundaries.
 *
 * KEY AGENTS:
 *   - Dependent Nations (powerless/trapped) — lack domestic semiconductor capacity, face supply vulnerability, have no alternative sources outside geopolitical blocs
 *   - Mid-Tier Economies (moderate/constrained) — possess partial capacity (design, packaging) but depend on imported leading-edge wafers; can exit constraint at moderate cost through technology licensing or alliance switching
 *   - Dominant Fab Operators: TSMC, Samsung, Intel (institutional/arbitrage) — primary beneficiaries with high arbitrage capacity; can negotiate capacity allocation, partnerships, and subsidies with multiple state actors
 *   - Equipment & Materials Suppliers (powerful/mobile) — EUV lithography, rare earth magnets, specialty gases; mobile exit options but face export controls and customer pressure to choose geopolitical sides
 *   - National Industrial Policy Coalitions (organized/constrained) — CHIPS Act consortiums, EU Chips Act, national fab initiatives; attempting to build exit pathway through investment
 *   - Multilateral Trade Bodies (institutional/arbitrage) — WTO, WIPO, SEMI standards bodies; institutional actors maintaining degraded coordination role
 *   - Global Supply Chain Efficiency (powerless/trapped) — abstract collective good; bears full cost of redundancy, fragmentation, and supply disruption without advocacy or exit
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(semiconductor_supply_chain_decoupling, 0.52).
domain_priors:suppression_score(semiconductor_supply_chain_decoupling, 0.65).
domain_priors:theater_ratio(semiconductor_supply_chain_decoupling, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(semiconductor_supply_chain_decoupling, extractiveness, 0.52).
narrative_ontology:constraint_metric(semiconductor_supply_chain_decoupling, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(semiconductor_supply_chain_decoupling, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(semiconductor_supply_chain_decoupling, tangled_rope).
narrative_ontology:human_readable(semiconductor_supply_chain_decoupling, "Semiconductor Supply Chain Decoupling and Strategic Autonomy").
narrative_ontology:topic_domain(semiconductor_supply_chain_decoupling, "geopolitical_economic/industrial_policy").

domain_priors:requires_active_enforcement(semiconductor_supply_chain_decoupling).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(semiconductor_supply_chain_decoupling, domestic_semiconductor_manufacturers).
narrative_ontology:constraint_beneficiary(semiconductor_supply_chain_decoupling, national_security_apparatus).
narrative_ontology:constraint_beneficiary(semiconductor_supply_chain_decoupling, established_equipment_suppliers).
narrative_ontology:constraint_victim(semiconductor_supply_chain_decoupling, global_supply_chain_efficiency).
narrative_ontology:constraint_victim(semiconductor_supply_chain_decoupling, dependent_nations).
narrative_ontology:constraint_victim(semiconductor_supply_chain_decoupling, downstream_technology_sectors).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DEPENDENT NATIONS (SNARE) — Nations lacking domestic semiconductor fabrication capacity face structural entrapment. They cannot exit the decoupling regime without massive capital investment (years, billions of dollars) and technological transfers they cannot secure independently. They bear the full cost of supply disruption, price volatility, and geopolitical leverage while having zero negotiating power. Maximum suppression: alternative sourcing is blocked by export controls, technology restrictions, and the concentrated geography of advanced fab capacity. The constraint extracts from powerless agents with no exit option.
constraint_indexing:constraint_classification(semiconductor_supply_chain_decoupling, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: MID-TIER ECOSYSTEMS (TANGLED ROPE) — Countries with partial semiconductor capacity (design, packaging, assembly but not leading-edge fab) experience the decoupling as both coordination and extraction. They genuinely coordinate on equipment standards and intellectual property within their alliance bloc, but decoupling simultaneously extracts by: (a) fragmenting their supply chain, (b) forcing costly dual-sourcing and safety stock, (c) limiting access to bleeding-edge nodes. They have constrained exit options — moderate costs to shifting alliances but significant disruption. Extraction is real but constrained by their organizational capacity and allied relationships.
constraint_indexing:constraint_classification(semiconductor_supply_chain_decoupling, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: DOMINANT FAB OPERATORS (ROPE) — These institutional actors experience decoupling as pure coordination of their core business: they coordinate supply agreements, allocate limited fab capacity across geopolitical blocs, manage IP partitioning, and optimize logistics. They capture first-mover advantage in geographic expansion, government subsidies, and strategic partnerships. High arbitrage options — they can negotiate with both Western and non-Western partners. The constraint enables them to increase capacity utilization and lock in long-term contracts. Their extraction runs toward them, not away.
constraint_indexing:constraint_classification(semiconductor_supply_chain_decoupling, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: NATIONAL INDUSTRIAL POLICY COALITIONS (SCAFFOLD) — Organized state actors (CHIPS Act consortiums, EU Chips Act, advanced manufacturing initiatives) perceive decoupling as a temporary coordination failure with a sunset clause: massive public investment in domestic fab capacity and supply chain redundancy is intended to eventually reduce vulnerability and allow exit from dependency. Theater is moderate (50-70% is genuine coordination, 30-50% is performative national security rhetoric). Suppression is declining as investment matures — sunset logic embedded in the policy timeline (10-20 year horizon to achieve relative autonomy).
constraint_indexing:constraint_classification(semiconductor_supply_chain_decoupling, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 5: EQUIPMENT & INPUTS SUPPLIERS (TANGLED ROPE) — Companies providing extreme ultraviolet (EUV) lithography, rare earth magnets, specialty gases, and precision manufacturing equipment experience decoupling as hybrid coordination-extraction. Genuine coordination: they coordinate on standards compliance across geographic blocs. Extraction: decoupling forces geographic duplication of supply chains, creates artificial scarcity premiums, locks suppliers into exclusive regional partnerships, and fragments their customer base. They have mobile exit options — they can sell to multiple blocs, but face export controls, IP restrictions, and customer pressure to choose sides. Moderate extraction with active enforcement required.
constraint_indexing:constraint_classification(semiconductor_supply_chain_decoupling, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: MULTILATERAL TRADE BODIES (PITON) — WTO, WIPO, and standards bodies (SEMI, JEDEC) that historically coordinated global semiconductor governance are now largely performative. Their enforcement mechanisms have atrophied (national governments bypass them), their standards coordination is split along geopolitical lines, and their role persists through institutional inertia rather than functional necessity. Theater ratio very high (80%+) — meetings continue, standards are published, but decoupling decisions are made by states and corporations outside these forums. These institutions are degraded coordination mechanisms maintained by habit and legacy authority.
constraint_indexing:constraint_classification(semiconductor_supply_chain_decoupling, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational/universal perspective, semiconductor supply chain fragmentation along geopolitical lines appears as an immutable feature of strategic competition itself: whenever states perceive technologies as strategically critical, they will attempt to control supply chains. This is seen as a natural law of international relations rather than a contingent institutional arrangement. However, the structural data contradicts this mountain classification — supply chain decoupling is enforced through policy, export controls, and corporate partnerships, not through physical or logical limits. The engine will identify this as a false summit: naturalization of political choice.
constraint_indexing:constraint_classification(semiconductor_supply_chain_decoupling, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(semiconductor_supply_chain_decoupling_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(semiconductor_supply_chain_decoupling, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(semiconductor_supply_chain_decoupling, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(semiconductor_supply_chain_decoupling, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(semiconductor_supply_chain_decoupling, TR),
    TR >= 0.70.

:- end_tests(semiconductor_supply_chain_decoupling_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The decoupling constraint extracts from dependent nations and mid-tier economies through: (1) geographic scarcity of cutting-edge fab capacity maintained by export controls, (2) forced dual-sourcing and safety stock buffering, (3) geopolitical leverage exerted through allocation of limited capacity. Extraction is not maximal because some genuine coordination benefits exist (allied nations standardize, share IP, reduce duplicate R&D), and because invested capital in new regional fabs creates real supply increases that partially counteract scarcity artificially. Suppression (0.65): High. Barriers to exit are substantial: export controls with legal penalties, IP restrictions on advanced technology transfer, massive capital requirements (50+ billion dollars per new leading-edge fab), long lead times (3-5 years to operational capacity), and embedded geopolitical allegiances. Theater ratio (0.58): Moderate-high. Much of decoupling policy has performative content: strategic autonomy is declared (theater) while actual fab capacity investments are slow and uncertain, investment pledges exceed actual spending, and rhetoric about 'friend-shoring' and 'trusted suppliers' masks continuing global supply dependencies. However, some genuine coordination is real (allied standards, legitimate security measures), and physical fab construction is not theater.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap in semiconductor decoupling is extreme and diagnostic. The dependent nation (powerless/trapped) perceives a snare: immobile, involuntary, extractive. The dominant fab operator (institutional/arbitrage) perceives rope: flexible partnerships, profitable capacity allocation, voluntary coordination. The mid-tier economy (moderate/constrained) perceives tangled rope: some benefits (allied access) alongside real extraction (forced costs). The open-science-like coalition (organized) perceives a temporary constraint with a sunset (scaffold): new investment will eventually provide autonomy. The equipment supplier (powerful/mobile) perceives hybrid extraction with negotiating room (tangled rope): they supply both blocs but face pressure to choose sides. The trade bodies (institutional/piton) perceive their own degradation: they used to coordinate these decisions; now they're bypassed. The analytical observer (civilizational) risks perceiving natural law: 'states always protect strategic technologies.' But the gap reveals the truth: decoupling is not natural law but political choice. If the political choice reversed (if states decided to open semiconductor markets), the constraint would dissolve. The mountain perspective is false; the tangled rope is structural.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality chain reveals how different institutional and national actors experience different effective extraction. Dependent nations (powerless/trapped) have d ≈ 0.95 (full targets) — they derive from victim status + trapped exit → maximum f(d). Dominant fabs (institutional/arbitrage) have d ≈ 0.05 (near-beneficiaries) — they derive from beneficiary status + arbitrage exit → near-zero/negative f(d). Mid-tier economies (moderate/constrained) have d ≈ 0.55 (symmetric) — they are both beneficiaries (access to allied fab capacity through partnership) and victims (forced to accept higher costs than pre-decoupling), with constrained exit options placing them at moderate d. The divergence between powerless and institutional perspectives is extreme: the powerless experience χ ≈ 0.75 (snare territory), while institutional actors experience χ ≈ -0.10 (pure coordination). This perspectival gap is the diagnostic signal that decoupling is an extraction mechanism with coordination framing.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLVED: The semiconductor decoupling constraint resolves the mandatrophy by demonstrating that the tangled rope classification is the truth that prevents mislabeling. If the constraint were classified as ROPE (pure coordination), the analysis would miss the extraction: the genuine coordination of allied standards and shared IP would obscure the scarcity artificially maintained by export controls and the permanent dependency created for nations outside the allied bloc. If the constraint were classified as SNARE (pure extraction), the analysis would miss the coordination: the fact that some actors do benefit from integrated supply chain management within their bloc would appear as false consciousness rather than real partial benefit. The tangled rope captures both: GENUINE COORDINATION (allied standards, IP sharing, collective security) AND ASYMMETRIC EXTRACTION (dependent nations trapped, scarcity maintained, geographic fragmentation). The mandatrophy unfolds through the perspectives: the rope-seeing institutional actor is partially right (there is coordination), the snare-seeing powerless agent is partially right (there is extraction), and the tangled rope is the synthesis that prevents either side from dismissing the other's experience as illusion. The constraint cannot transition to pure coordination (rope) until the export control mechanisms dissolve. It cannot be pure extraction (snare) because some actors genuinely coordinate and benefit. It is fundamentally hybrid.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    geopolitical_bloc_stability,
    'Will the current US-led and China-centric semiconductor blocs remain stable, or will secondary powers achieve sufficient capacity to arbitrage between them?',
    'Longitudinal tracking of Taiwan, South Korea, Japan, and EU fab capacity maturation; measurement of technology node parity achieved by secondary suppliers; analysis of cross-bloc technology transfers and talent flows',
    'If blocs remain duopolar: extraction persists indefinitely (snare for dependent nations). If secondary powers gain parity: multipolar market emerges (rope for moderate powers) and extraction declines (transition to genuine coordination).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(geopolitical_bloc_stability, empirical, 'Long-term geopolitical bloc stability and multipolarity emergence').

omega_variable(
    public_investment_sufficiency,
    'Will state investments in domestic fab capacity (CHIPS Act, EU Chips Act) reach technology parity with incumbent leaders, or will they create permanently subsidized second-tier capacity?',
    'Comparison of fab efficiency metrics (cost per transistor, yield rates, R&D spending) between funded new capacity and incumbent leaders after 10-15 year horizon; analysis of whether new fabs can compete on cost rather than subsidy',
    'If parity achieved: scaffold sunset is real, decoupling transitions toward coordination. If second-tier persists: extraction mechanisms entrench (dependent states locked into permanent subsidized capacity dependency), classification shifts toward piton (ritual investment without functional independence).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(public_investment_sufficiency, empirical, 'Domestic fab investment capability to achieve technology parity').

omega_variable(
    alternative_technology_feasibility,
    'Can emerging technologies (chiplets, advanced packaging, software-defined hardware abstraction) reduce the absolute scarcity of leading-edge fab capacity, or do current roadmaps remain node-dependent?',
    'Technical analysis of chiplet scaling limits, packaging density achievements, and software abstraction effectiveness; comparison of chip performance when using older nodes with architectural innovation vs. bleeding-edge nodes',
    'If alternatives viable: leading-edge node scarcity declines, decoupling constraint loses extraction mechanism (transition from snare toward rope). If node-dependent persists: scarcity remains artificial, extraction mechanisms strengthen.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_technology_feasibility, empirical, 'Technical feasibility of non-node-dependent performance pathways').

omega_variable(
    export_control_enforcement_cost,
    'What is the true enforcement cost of maintaining semiconductor export controls, and can secondary powers sustain these costs indefinitely?',
    'Analysis of enforcement infrastructure (customs, technology tracking, intelligence operations); comparison of enforcement cost as percentage of GDP/defense budget across enforcing nations; measurement of enforcement leakage and smuggling rates',
    'If enforcement costs are high and leakage is significant: controls degrade over time (piton classification strengthens). If costs are manageable and leakage minimal: controls persist (snare extraction sustained). High enforcement costs + high leakage indicates the constraint is becoming theater-dependent.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(export_control_enforcement_cost, empirical, 'Feasibility of sustained export control enforcement').

omega_variable(
    strategic_autonomy_definition_ambiguity,
    'What level of semiconductor self-sufficiency qualifies as strategic autonomy: 50% domestic sourcing? 80%? Full independence? How does the answer shift the classification?',
    'Policy analysis across US, EU, China, India declaring what percentage domestic sourcing counts as ''strategic autonomy''; comparison with actual supply chain fragility and resilience metrics',
    'If autonomy threshold is low (50%): current policies already approaching sunset, scaffold classification valid. If threshold is high (80-100%): sunset is decades away, scaffold fades into permanent snare for dependent nations. Threshold ambiguity itself is an omega: it creates theater (policy declared without meeting functional criteria).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(strategic_autonomy_definition_ambiguity, conceptual, 'Definition ambiguity of strategic autonomy and self-sufficiency criteria').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(semiconductor_supply_chain_decoupling, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(semidec_tr_t0, semiconductor_supply_chain_decoupling, theater_ratio, 0, 0.42).
narrative_ontology:measurement(semidec_tr_t3, semiconductor_supply_chain_decoupling, theater_ratio, 3, 0.5).
narrative_ontology:measurement(semidec_tr_t6, semiconductor_supply_chain_decoupling, theater_ratio, 6, 0.58).

% Extraction over time
narrative_ontology:measurement(semidec_be_t0, semiconductor_supply_chain_decoupling, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(semidec_be_t3, semiconductor_supply_chain_decoupling, base_extractiveness, 3, 0.44).
narrative_ontology:measurement(semidec_be_t6, semiconductor_supply_chain_decoupling, base_extractiveness, 6, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(semiconductor_supply_chain_decoupling, resource_allocation).
narrative_ontology:affects_constraint(semiconductor_supply_chain_decoupling, rare_earth_supply_concentration).
narrative_ontology:affects_constraint(semiconductor_supply_chain_decoupling, lithography_equipment_choke_point).
narrative_ontology:affects_constraint(semiconductor_supply_chain_decoupling, taiwan_geopolitical_vulnerability).

% DUAL FORMULATION NOTE:
% Semiconductor supply chain decoupling is downstream of specific geographic and technological choke points (Taiwan fab concentration, EUV equipment suppliers, rare earth element processing) and upstream of downstream technology sectors (AI hardware, defense systems, consumer electronics). This story models the decoupling mechanism itself; related stories model the specific choke points and sectoral impacts.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(semiconductor_supply_chain_decoupling, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
