% ============================================================================
% CONSTRAINT STORY: civilizational_lifecycle_solara
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_civilizational_lifecycle_solara, []).

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
 *   constraint_id: civilizational_lifecycle_solara
 *   human_readable: The Lifecycle of Solaran Civilization
 *   domain: social/political
 *
 * SUMMARY:
 *   The Solaran civilization exhibits a complete lifecycle arc spanning
 *   approximately one century from integration to peak-phase extraction to
 *   incipient decline. The constraint models how institutional coordination
 *   mechanisms (bureaucratic standardization, centralized infrastructure,
 *   legal frameworks) that enable growth in early phases gradually transform
 *   into extraction mechanisms in peak phases as administrative elites
 *   concentrate resources and peripheral populations bear costs without
 *   visible coordination benefits. Theater ratio increases from 0.35 to 0.58
 *   as traditional authority forms (kinship networks, local autonomy) persist
 *   performatively while actual resource allocation is controlled by
 *   bureaucratic hierarchy. Extractiveness increases from 0.18 to 0.38 as the
 *   initial coordination surplus declines and administrative overhead
 *   accumulates. The constraint demonstrates how the same institutional
 *   apparatus — central administration — functions as rope (coordination)
 *   from the beneficiary's perspective, tangled_rope (mixed benefit and cost)
 *   from moderate populations, and snare (pure extraction) from powerless
 *   peripheral groups. The perspectival gap is maximal: the elite see their
 *   own necessity and coordination contribution; peripheral populations see
 *   only loss of autonomy and resource depletion. Reform movements see a
 *   temporary governance failure with potential for restructuring; legacy
 *   institutions see their functional erosion masked by ritual performance.
 *   The analytical observer risks naturalizing this trajectory as an
 *   immutable civilizational law, when it reflects specific institutional
 *   design choices and elite incentive structures.
 *
 * KEY AGENTS:
 *   - Administrative Elite: Primary beneficiary (institutional/arbitrage) — captures resource concentration benefits, controls infrastructure, experiences constraint as coordination necessity
 *   - Peripheral Populations: Primary victim (powerless/trapped) — subsistence communities dependent on central authority, subject to taxation and labor obligations, cannot exit territorial system
 *   - Regional Trading Communities: Secondary beneficiary/victim (moderate/constrained) — benefit from standardized infrastructure and security, but bear taxation and quota restrictions; limited exit capacity
 *   - Reform Movements: Organized agents (organized/mobile) — merchant guilds, intellectual networks, religious orders proposing institutional restructuring; perceive temporary governance failure with reform pathway
 *   - Legacy Institutional Structures: Inertial agents (institutional/constrained) — kinship authority, local autonomy forms persist through ritual despite functional obsolescence
 *   - Ecological Commons: Tertiary victim (powerless/trapped) — subject to extraction without agency; degradation accelerates as extractive pressure increases
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(civilizational_lifecycle_solara, 0.38).
domain_priors:suppression_score(civilizational_lifecycle_solara, 0.48).
domain_priors:theater_ratio(civilizational_lifecycle_solara, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(civilizational_lifecycle_solara, extractiveness, 0.38).
narrative_ontology:constraint_metric(civilizational_lifecycle_solara, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(civilizational_lifecycle_solara, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(civilizational_lifecycle_solara, tangled_rope).
narrative_ontology:human_readable(civilizational_lifecycle_solara, "The Lifecycle of Solaran Civilization").
narrative_ontology:topic_domain(civilizational_lifecycle_solara, "social/political").

domain_priors:requires_active_enforcement(civilizational_lifecycle_solara).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(civilizational_lifecycle_solara, administrative_elite).
narrative_ontology:constraint_beneficiary(civilizational_lifecycle_solara, resource_concentrators).
narrative_ontology:constraint_victim(civilizational_lifecycle_solara, peripheral_populations).
narrative_ontology:constraint_victim(civilizational_lifecycle_solara, future_generations).
narrative_ontology:constraint_victim(civilizational_lifecycle_solara, ecological_commons).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PERIPHERAL POPULATIONS (SNARE) — Subsistence populations bear extraction costs through labor obligations, resource restrictions, and ecological degradation without meaningful political voice or exit options. Trapped within territorial boundaries and dependent on central authority for infrastructure access. Maximum experienced extraction with no visible coordination benefit.
constraint_indexing:constraint_classification(civilizational_lifecycle_solara, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: REGIONAL TRADING COMMUNITIES (TANGLED ROPE) — Benefit from centralized infrastructure (roads, security, standardized weights/measures) that enables commerce, but also subject to taxation, tariffs, and quota systems. Have limited exit options due to infrastructure dependence and social ties, yet possess some negotiating capacity through collective action and trade network alternatives.
constraint_indexing:constraint_classification(civilizational_lifecycle_solara, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: ADMINISTRATIVE ELITE (ROPE) — Primary beneficiary experiencing the lifecycle constraint as a coordination mechanism. Central bureaucracy solves collective action problems: standardizes law, organizes defense, maintains hydraulic infrastructure. Has high exit capacity (can relocate, access alternative networks, reposition politically). Net beneficiary experiencing effective negative extraction.
constraint_indexing:constraint_classification(civilizational_lifecycle_solara, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: REFORM MOVEMENTS (SCAFFOLD) — Organized coalitions (merchant guilds, religious orders, intellectual networks) perceive the lifecycle's peak-phase extraction as temporary, advocating for institutional reforms, resource redistribution, and governance restructuring. See a sunset pathway through constitutional limitation, bureaucratic rotation, and decentralization. Low effective extraction because these agents maintain exit options (exit via network relocation, institutional reconfiguration) and have agency in reform trajectory.
constraint_indexing:constraint_classification(civilizational_lifecycle_solara, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(continental))).

% PERSPECTIVE 5: LEGACY INSTITUTIONAL FRAMEWORK (PITON) — Older forms of social organization (kinship networks, local autonomy, hereditary authority) persist through institutional inertia despite functional atrophy in an integrated civilization. Theater ratio reflects performative maintenance of traditional authority forms that no longer govern resource allocation effectively. High theater, low functional coordination.
constraint_indexing:constraint_classification(civilizational_lifecycle_solara, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LIFECYCLE VIEW (MOUNTAIN) — From a long-scale perspective, all civilizations follow universal patterns: growth, peak, decline, collapse. The Solaran lifecycle appears as an immutable law of complex societies — extractiveness and suppression are inherent to scale, not contingent institutional choices. However, this naturalization risks missing the structural contingencies (administrative design choices, resource distribution policies, technological alternatives) that shape actual historical trajectories.
constraint_indexing:constraint_classification(civilizational_lifecycle_solara, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(civilizational_lifecycle_solara_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(civilizational_lifecycle_solara, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(civilizational_lifecycle_solara, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(civilizational_lifecycle_solara, TR),
    TR >= 0.70.

:- end_tests(civilizational_lifecycle_solara_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The Solaran system achieves substantial early-phase coordination benefits (infrastructure, security, standardized law) that reduce friction costs below what fragmented local systems require. However, extractive overlay increases as elites consolidate control. The value reflects the accumulated asymmetry: initial extraction is justified as coordination overhead, but by peak-phase it represents pure rent-seeking. Theater ratio (0.58): Moderate-high. Traditional authority rituals (coronations, councils, legal proceedings) maintain symbolic legitimacy while actual resource allocation happens through bureaucratic channels. As the gap widens between ritual form and bureaucratic reality, theater increases. Suppression (0.48): Moderate. Peripheral populations face significant barriers to exit (territorial dependency, infrastructure lock-in, social ties) and to organization (dispersed populations, information constraints), but not absolute bars. Some exit occurs through migration to frontier zones or joining reform networks; some organization occurs through religious movements and guild participation.
 *
 * PERSPECTIVAL GAP:
 *   Maximum perspectival divergence emerges between the administrative elite (rope perspective) and peripheral populations (snare perspective). The elite genuinely experience the constraint as coordination — they solved the collective action problem of integrating fragmented communities and maintain essential infrastructure that requires central coordination. Peripheral populations genuinely experience the constraint as extraction — they see taxation without reciprocal services, resource restrictions without participation in decisions, labor obligations without consent. Regional traders see tangled_rope — they benefit from infrastructure and security but bear taxation; they have some negotiating power but cannot exit entirely. Reform movements see scaffold — they perceive the extraction as a temporary governance failure correctable through institutional innovation and see real pathways (bureaucratic rotation, resource redistribution, decentralization) toward lower-extraction equilibrium. Legacy institutions see piton — they maintain ritual forms (councils, hereditary authority) that no longer control resource allocation, performing legitimacy that has eroded. The analytical observer risks mountain — sees civilizational lifecycle as inevitable law of complex systems — but the structural data shows contingency: elite choices about distribution mechanisms, institutional design, and legitimation strategies actively shape the extraction trajectory.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values are determined by each agent's structural position within the extraction-coordination apparatus. Administrative elites receive low d (beneficiaries with arbitrage options) — they can reposition politically, access alternative networks, control information flow. Regional traders receive moderate d (beneficiaries-with-costs, constrained exit) — they benefit from infrastructure but bear taxation and depend on central approval. Peripheral populations receive high d (victims, trapped) — they have no exit options, face information asymmetry, bear costs without compensation. Reform movements receive moderate-low d (organized agents with mobile options) — they can exit via network relocation and have agency through institutional influence. The analytical observer receives neutral d (0.5, observational position) — observes the system from outside without structural interest. The piton institution receives moderate d (degraded but still enforcing) — continues to extract ritual legitimacy while losing functional role.
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLUTION: The mandatrophy is resolved by recognizing that early-phase coordination and later-phase extraction are not competing descriptions of the same mechanism but sequential functional states of the same institutional apparatus. The administrative hierarchy that solves coordination problems in early phases (achieving 0.18 extractiveness with genuinely lower costs than alternatives) gradually becomes a rent-extraction vehicle in peak phases (accumulating overhead and elite captures). This is neither false labeling (rope mislabeled as snare) nor confusion of perspectives — it reflects actual structural transformation of the elite's incentives as they consolidate power and face declining marginal returns on coordination investment. The scaffold perspective (reform movements) identifies the critical intervention point: institutional design innovations (bureaucratic rotation, term limits, audit mechanisms, resource redistribution mandates) can arrest the extractiveness trajectory before collapse. The piton perspective documents how older authority forms persist through theater after losing functional role, suggesting that decay has already begun. The snare perspective from peripheral populations indicates that extraction has reached structural dominance by the measurement interval. The constraint is tangled_rope because coordination and extraction are genuinely coupled: the same infrastructure that enables trade and security is the mechanism through which elite extraction occurs, and dismantling the extraction mechanism would require partial dismantling of coordination benefits. This coupling is not a measurement error but a real structural feature of how centralized civilization works.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sustainability_threshold,
    'What resource extraction rate and environmental degradation threshold determines the transition from growth to decline in the Solaran lifecycle?',
    'Paleoclimatic reconstruction of soil depletion, forest cover change, water table decline; archaeological evidence of settlement abandonment patterns; demographic collapse timing relative to environmental proxy records',
    'If threshold is crossed early (narrow margin): lifecycle is tightly constrained by ecology, extraction cannot proceed beyond ~50 years. If threshold is remote (~150+ years): extraction can accumulate substantially, enabling longer peak-phase asymmetry and larger eventual collapse.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sustainability_threshold, empirical, 'Environmental sustainability threshold for civilizational decline').

omega_variable(
    elite_coordination_capacity,
    'What institutional innovations (bureaucratic record-keeping, standardized law, rotation systems) sufficiently distribute elite power to prevent predatory extraction and enable sustainable equilibrium?',
    'Comparative analysis of institutional features in civilizations with extended stable phases vs. those with rapid decline; correlation between bureaucratic formalization and extraction rate stability; examination of succession mechanisms and power concentration patterns',
    'If innovations are sufficient: some civilizations achieve low-extraction rope states and avoid collapse. If innovations consistently fail: all complex societies eventually degrade to high-extraction snare states, making collapse universal.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(elite_coordination_capacity, empirical, 'Whether elite coordination systems can prevent extraction-driven decline').

omega_variable(
    legitimacy_decay_timeline,
    'How rapidly does perceived legitimacy of central authority erode when peripheral populations experience extraction without visible coordination benefits?',
    'Analysis of oral history, administrative records, and archaeological evidence of resistance patterns; correlation between taxation intensity and rebellion frequency; timing of institutional reform proposals relative to extraction rate changes',
    'If decay is rapid (~20-30 years of visible extraction produces major resistance): early reform becomes necessary, constraining elite extraction. If decay is slow (~60+ years): extraction can accumulate invisibly, leading to sudden catastrophic institutional failure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legitimacy_decay_timeline, empirical, 'Rate of legitimacy erosion under visible extraction').

omega_variable(
    collapse_inevitability,
    'Is civilizational collapse driven by structural constraints (ecological, administrative) or by contingent choices (leadership, institutional design)?',
    'Identification of causal bottlenecks in collapse narratives; comparison of civilizations with similar structural constraints but different outcomes; analysis of counterfactual reform scenarios; examination of whether early institutional interventions could alter trajectory',
    'If structural: lifecycle is mountain-like, collapse is inevitable regardless of elite choices. If contingent: lifecycle is snare/tangled_rope, collapse is a result of specific institutional failures and could be prevented through reform.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(collapse_inevitability, conceptual, 'Whether collapse is structurally inevitable or contingently chosen').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(civilizational_lifecycle_solara, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(solara_tr_t0, civilizational_lifecycle_solara, theater_ratio, 0, 0.35).
narrative_ontology:measurement(solara_tr_t50, civilizational_lifecycle_solara, theater_ratio, 50, 0.48).
narrative_ontology:measurement(solara_tr_t100, civilizational_lifecycle_solara, theater_ratio, 100, 0.58).

% Extraction over time
narrative_ontology:measurement(solara_be_t0, civilizational_lifecycle_solara, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(solara_be_t50, civilizational_lifecycle_solara, base_extractiveness, 50, 0.32).
narrative_ontology:measurement(solara_be_t100, civilizational_lifecycle_solara, base_extractiveness, 100, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(civilizational_lifecycle_solara, resource_allocation).
narrative_ontology:affects_constraint(civilizational_lifecycle_solara, ecological_carrying_capacity).
narrative_ontology:affects_constraint(civilizational_lifecycle_solara, elite_legitimacy_erosion).
narrative_ontology:affects_constraint(civilizational_lifecycle_solara, institutional_reform_capacity).

% DUAL FORMULATION NOTE:
% The Solaran lifecycle constraint is downstream of specific resource depletion patterns (ecological carrying capacity) and elite coordination failures (institutional reform capacity). Each upstream constraint has its own extractiveness reflecting empirical measurement of environmental thresholds and institutional capacity; the lifecycle constraint aggregates these into a compound constraint showing how the system state evolves over time.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(civilizational_lifecycle_solara, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
