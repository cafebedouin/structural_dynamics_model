% ============================================================================
% CONSTRAINT STORY: m23_armed_group_dynamics
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_m23_armed_group_dynamics, []).

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
 *   constraint_id: m23_armed_group_dynamics
 *   human_readable: M23 Armed Group Extraction and Coercion Dynamics
 *   domain: conflict_studies/governance/security
 *
 * SUMMARY:
 *   M23 represents a structural constraint operating across multiple levels:
 *   coercive extraction from civilian populations, forced labor and identity
 *   fusion in military recruitment, destabilization of state authority,
 *   coordination of resource extraction networks, and performative
 *   international governance responses. The constraint exhibits the full
 *   spectrum of classification types across different observer positions,
 *   making it a diagnostic case for how indexical realism reveals competing
 *   structural framings of the same phenomenon. The civilian perspective
 *   perceives pure extraction (snare); the state perspective perceives mixed
 *   coordination and extraction (tangled rope); the resource extraction
 *   networks perceive primarily coordination (rope); the international
 *   frameworks perceive performative response (piton); the analytical
 *   observer risks naturalizing contingent institutional arrangements as
 *   inevitable geopolitical facts (false summit mountain). Extractiveness has
 *   increased from 0.55 to 0.78 over the measurement interval, reflecting
 *   consolidation of M23's territorial control and deepening of extraction
 *   mechanisms. Theater ratio increased modestly (0.42 to 0.55), indicating
 *   that M23 invests in some legitimating narratives (ethnic protection,
 *   anti-colonial framing) alongside coercive control, but the constraint
 *   remains fundamentally coercive rather than performative.
 *
 * KEY AGENTS:
 *   - M23 Leadership: Primary beneficiary (institutional/arbitrage) — controls territory, tax revenue, forced labor, and strategic resources with minimal external constraints
 *   - Civilian Populations: Primary victims (powerless/trapped) — bear coercive taxation, recruitment pressure, violence, and movement restrictions with no exit capacity
 *   - Forced Recruits and Child Soldiers: Secondary victims (powerless/identity_locked) — structurally mobile but identity-fused with military role; cognitive capture prevents exit perception
 *   - DRC State Authority: Secondary actor (institutional/constrained) — experiences both genuine security coordination problem and extraction through loss of territorial authority
 *   - Regional State Actors: Secondary actor (organized/constrained) — benefit from and bear costs of M23's destabilizing presence; geographic interdependence constrains exit
 *   - Resource Extraction Networks and Mining Cartels: Secondary beneficiary (institutional/arbitrage) — benefit from M23's coercive enforcement of supply chains
 *   - International Humanitarian and Governance Frameworks: Performative observer (institutional/arbitrage) — maintain monitoring and reporting function with minimal impact on coercive dynamics
 *   - Civil Society and Resistance Networks: Secondary victim (organized/constrained) — face targeted suppression despite organizing capacity; resistance carries existential risk
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(m23_armed_group_dynamics, 0.78).
domain_priors:suppression_score(m23_armed_group_dynamics, 0.82).
domain_priors:theater_ratio(m23_armed_group_dynamics, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(m23_armed_group_dynamics, extractiveness, 0.78).
narrative_ontology:constraint_metric(m23_armed_group_dynamics, suppression_requirement, 0.82).
narrative_ontology:constraint_metric(m23_armed_group_dynamics, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(m23_armed_group_dynamics, snare).
narrative_ontology:human_readable(m23_armed_group_dynamics, "M23 Armed Group Extraction and Coercion Dynamics").
narrative_ontology:topic_domain(m23_armed_group_dynamics, "conflict_studies/governance/security").

domain_priors:requires_active_enforcement(m23_armed_group_dynamics).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(m23_armed_group_dynamics, m23_leadership).
narrative_ontology:constraint_beneficiary(m23_armed_group_dynamics, regional_resource_extractors).
narrative_ontology:constraint_victim(m23_armed_group_dynamics, civilian_populations).
narrative_ontology:constraint_victim(m23_armed_group_dynamics, state_authority).
narrative_ontology:constraint_victim(m23_armed_group_dynamics, regional_stability).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CIVILIAN POPULATIONS (SNARE) — Residents in M23-controlled territories face coercive taxation, forced recruitment, violent suppression of dissent, and movement restrictions. Exit options are severely constrained: physical barriers to movement, ethnic targeting in neighboring areas, and lack of safe passage. Maximum experienced extraction with minimal coordination benefit. Theater ratio reflects that M23 maintains coercive control through violence and threat, not through legitimating narratives or institutional performance.
constraint_indexing:constraint_classification(m23_armed_group_dynamics, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: FORCED RECRUITS AND CHILD SOLDIERS (SNARE) — Individuals pressed into M23 service face extreme suppression: isolation from family, violence-based control, identity fusion with military role, and asymmetric power dynamics. Many recruits (particularly children) are identity-locked — their self-concept becomes fused with the military identity imposed by the group. Even when structural opportunities for escape arise, the internalized role makes exit psychologically unthinkable. This represents a distinct snare mechanism from civilian taxation — pure coercive dependency with no exit perception.
constraint_indexing:constraint_classification(m23_armed_group_dynamics, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 3: REGIONAL STATE ACTORS (TANGLED ROPE) — States bordering M23-controlled territory face mixed incentives. Some benefit from M23's extractive capacity (destabilization of competitors, natural resource smuggling networks, refugee flows that weaken neighbors). Simultaneously, they bear costs (refugee burdens, spillover violence, international pressure). Their exit from the regional dynamic is constrained by geography and security interdependence but not impossible — they retain negotiation capacity and military options. The constraint exhibits both genuine coordination (security dilemma, conflict management) and asymmetric extraction (some actors benefit more than others).
constraint_indexing:constraint_classification(m23_armed_group_dynamics, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 4: DRC STATE AUTHORITY (TANGLED ROPE) — The DRC government faces a hybrid constraint: genuine need for coordination with regional actors on security (legitimate security problem) alongside institutional extraction by M23 and affiliated power brokers (loss of territorial control, tax revenue, monopoly on violence). The DRC has some exit capacity (military reorganization, international support, administrative reform) but faces severe constraints (limited fiscal capacity, institutional fragmentation, geography). The constraint functions partly as coordination mechanism (how do you govern contested territory?) and partly as extraction mechanism (powerful actors extract resources and authority).
constraint_indexing:constraint_classification(m23_armed_group_dynamics, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 5: RESOURCE EXTRACTION NETWORKS (ROPE) — Mining cartels, smuggling networks, and regional trading houses benefit from M23's coercive capacity. M23 provides violent enforcement of resource extraction arrangements, elimination of competing claims, and supply chain control that would otherwise require expensive institutional coordination. From this perspective, M23 appears as a coordination mechanism — a solution to the collective action problem of organizing mineral exports and trafficking networks in ungoverned space. Beneficiaries experience the constraint as solving a genuine coordination problem with manageable extraction costs.
constraint_indexing:constraint_classification(m23_armed_group_dynamics, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(continental))).

% PERSPECTIVE 6: INTERNATIONAL COMMERCIAL ACTORS (TANGLED ROPE) — Multinational corporations involved in mineral procurement face a mixed constraint. M23's control of mining areas creates both barriers and coordination benefits: barriers because it complicates supply chain verification and creates reputational risk; benefits because it stabilizes production and reduces transaction costs with local suppliers. These actors have exit options (supply switching, certification schemes, due diligence frameworks) but face high costs. The constraint coordinates resource supply while extracting rents through intermediary complexity and reputational risk transfer.
constraint_indexing:constraint_classification(m23_armed_group_dynamics, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 7: INTERNATIONAL FRAMEWORKS (PITON) — UN peacekeeping, humanitarian agencies, and governance frameworks maintain a performative relationship to M23 dynamics. These institutions conduct monitoring, report on violations, and coordinate humanitarian response, but their interventions have minimal impact on extraction mechanisms. The frameworks persist through institutional mandate and donor funding rather than functional effectiveness — theater ratio is high because monitoring and reporting create appearance of response without changing coercive dynamics. The piton classification reflects degraded function maintained by institutional inertia.
constraint_indexing:constraint_classification(m23_armed_group_dynamics, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 8: CIVIL SOCIETY AND RESISTANCE (SNARE) — Activist networks, community organizations, and local resistance movements face severe suppression: infiltration, targeted violence, family threats, and forced collaboration with security forces. While these actors have some organizing capacity (organized power atom), their ability to exit the constraint is constrained by omnipresent surveillance and the cost of active resistance. They experience the constraint as a snare because exit options are genuine but carry existential risk. Theater ratio is moderate here because resistance movements must maintain some performative legitimacy narratives to sustain membership and external support.
constraint_indexing:constraint_classification(m23_armed_group_dynamics, snare,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 9: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational scale, armed group persistence in ungoverned spaces might appear as an inevitable feature of state fragmentation and competition for resources — a natural law of geopolitics where coercive capacity replaces institutional authority. However, structural data contradicts this mountain classification. M23's extractiveness and suppression are products of specific institutional arrangements (weak state capacity, resource economics, geopolitical incentives), not immutable laws. The engine will flag this as a false summit, revealing that naturalizing M23 as inevitable obscures changeable structural conditions.
constraint_indexing:constraint_classification(m23_armed_group_dynamics, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(m23_armed_group_dynamics_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(m23_armed_group_dynamics, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(m23_armed_group_dynamics, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(m23_armed_group_dynamics, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(m23_armed_group_dynamics, TR),
    TR >= 0.70.

:- end_tests(m23_armed_group_dynamics_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.78): High and rising. M23 extracts through coercive taxation, forced recruitment, resource monopolization, and elimination of alternative authorities. The constraint's primary function is transfer of resources and labor from civilian populations to M23 leadership and affiliated networks. Suppression (0.82): Very high. Multiple suppression mechanisms operate simultaneously: physical barriers to movement, ethnic targeting, violence against organizers, mandatory conscription, infiltration of civilian organizations, and family-level coercion. Suppression is structural (external barriers dominate) rather than internalized, except in the specific case of recruited individuals who experience identity fusion. Theater ratio (0.55): Moderate. M23 uses some legitimating narratives (ethnic protection, anti-colonial resistance, governance restoration) but relies primarily on coercive enforcement rather than institutional performance. The theater increased slightly over the interval as M23 consolidated control and began establishing administrative structures, but coercion remains the dominant mechanism. This pattern distinguishes M23 from piton constraints (which have theater > 0.70 and degraded function) — M23's function is intact and purely coercive.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates how the same structural phenomenon produces radically different classifications from different observer positions. Civilian populations trapped in M23-controlled territory see pure extraction (snare) — coercion with no coordination benefit. M23 leadership and resource networks see coordination (rope) — the constraint solves the collective action problem of organizing resource extraction in ungoverned space. DRC state authority sees mixed coordination and extraction (tangled rope) — genuine security problems requiring cooperation alongside institutional damage from M23's territorial control. Regional state actors see tangled rope with different terms — they benefit from M23's destabilization of regional competitors while bearing refugee and spillover costs. International frameworks see a problem they monitor but cannot solve (piton) — their interventions are performative because coercive mechanisms operate beneath the institutional level. Civil society sees targeted suppression (snare) despite having some organizing capacity, because the cost of resistance is existential. The civilizational analytical observer risks a false summit: seeing M23 as inevitable geopolitical outcome rather than contingent institutional arrangement maintained by specific incentive structures.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values are derived from each agent's structural relationship to the extraction flow. Civilian populations and forced recruits are targets (d ≈ 0.95–1.00): they bear maximum extraction and lack exit options. M23 leadership are beneficiaries (d ≈ 0.05–0.15): they capture resources and face minimal constraints (arbitrage-level exit capacity). DRC state authority and regional actors occupy intermediate positions (d ≈ 0.55–0.65): they experience both coordination problems and extraction; their exit capacity is constrained by geography and interdependence. Resource extraction networks are beneficiaries with moderate d (≈0.25–0.35): they benefit from coercive supply chain control but face reputational and compliance costs. The perspectival gap emerges from these differential directionalities: powerless civilians experience snare; powerful beneficiaries experience rope or tangled rope; intermediate institutional actors experience tangled rope. The analytical observer at civilizational scale risks misclassifying the constraint as immutable (mountain) by naturalizing contingent institutional arrangements.
 *
 * MANDATROPHY ANALYSIS:
 *   Extractiveness (0.78) exceeds the snare threshold (0.66), requiring mandatrophy resolution. The resolution proceeds by disaggregating the constraint across multiple institutional contexts. At the civilian level, the constraint is unambiguously snare: ε = 0.78, suppression = 0.82, χ ≥ 0.66, all gates satisfied. At the regional state level, the constraint is tangled rope: genuine coordination problem (security dilemma) coexists with asymmetric extraction (some actors benefit more). At the resource extraction network level, the constraint is rope: the primary function is coordination, and experienced extraction is moderate or negative (beneficiaries). The mandatrophy is resolved not by adjusting the metrics but by recognizing that the single constraint story represents multiple structural realities from different agent positions. The analytical observer's false summit (mountain) is false precisely because the structural data shows high extractiveness and suppression — properties that contradict natural law classification. The mandatrophy resolution thus serves as a diagnostic tool: when a constraint exhibits all six types from different perspectives, and when the analytical observer's perspective is mountain while the data contradicts it, the system correctly identifies that the 'inevitable law' framing naturalizes contingent institutional arrangements.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    identity_lock_reversibility,
    'To what degree are forced recruits identity-locked versus temporarily coerced, and what conditions would enable cognitive frame shift?',
    'Longitudinal tracking of demobilized recruits; analysis of post-exit identity reconstruction; identification of specific cognitive triggers that enable frame shift (external validation, community reintegration, alternative identity structures)',
    'If identity lock is strong: educational and reintegration programs fail; classification of constraint shifts toward mountain. If identity lock is reversible: demobilization and reintegration pathways become viable; classification shifts toward tangled rope or scaffold with sunset clause.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_reversibility, empirical, 'Reversibility of identity fusion in forced recruits').

omega_variable(
    regional_coordination_necessity,
    'Is the regional state participation in M23 networks driven by genuine security interdependence (coordination problem) or by factional benefit and capture?',
    'Comparative analysis of security outcomes under M23 presence versus absence; measurement of state capability to govern without M23 coercive capacity; identification of specific faction-level benefits to state officials versus public security outcomes',
    'If genuine coordination: constraint is tangled rope and might be resolvable through alternative coordination mechanisms. If factional capture: constraint is snare disguised as coordination; eliminating capture requires institutional reform, not security cooperation frameworks.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regional_coordination_necessity, empirical, 'Whether regional state involvement reflects coordination necessity or factional capture').

omega_variable(
    extractiveness_measurement_baseline,
    'What is the counterfactual state of civilian life absent M23 control? Is extractiveness (0.78) measured against democratic governance or against pre-collapse ungoverned conditions?',
    'Comparison with extractiveness metrics from similar ungoverned territories; analysis of civilian welfare indicators in areas before M23 arrival versus after; establishment of baseline extractiveness for ungoverned space condition',
    'If baseline is democratic governance: extractiveness is 0.78. If baseline is ungoverned anarchy: extractiveness may be lower (~0.55) because M23 provides some security provision alongside extraction. Observable-dependent classification suggests constraint decomposition into multiple stories.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(extractiveness_measurement_baseline, empirical, 'Counterfactual baseline for extractiveness measurement').

omega_variable(
    theatrical_performance_mechanism,
    'Does M23''s moderate theater ratio (0.55) reflect genuine coordination function or strategic use of narrative to maintain legitimacy among some populations?',
    'Content analysis of M23 communications; ethnographic study of civilian perception of M23 legitimacy by region; identification of narratives M23 uses to justify control (security provision, ethnic protection, governance restoration) versus narratives focused on pure coercion',
    'If theater is strategic: M23 maintains some population support and coordination function; classification might shift toward tangled rope in some regions. If theater is minimal: coercion is primary mechanism; classification confirms snare across perspectives.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(theatrical_performance_mechanism, empirical, 'Strategic function of M23 narrative and legitimation efforts').

omega_variable(
    supply_chain_alternative_viability,
    'Can mineral supply chains operate from DRC-controlled territories without M23''s coercive enforcement? What compliance costs would legitimate supply chains require?',
    'Economic modeling of supply chain costs under different governance scenarios; comparison with conflict minerals certification schemes; analysis of price differentials for legitimately sourced versus M23-connected minerals',
    'If alternatives are viable: resource extraction networks'' benefit from M23 constraint is contingent; institutional reform could shift coordination away from coercive mechanisms. If alternatives are prohibitively expensive: constraint is nearly immutable for commercial actors; only state-level intervention can break the lock.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(supply_chain_alternative_viability, empirical, 'Economic viability of non-coercive mineral supply chain alternatives').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(m23_armed_group_dynamics, 0, 9).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(m23_tr_t0, m23_armed_group_dynamics, theater_ratio, 0, 0.42).
narrative_ontology:measurement(m23_tr_t3, m23_armed_group_dynamics, theater_ratio, 3, 0.48).
narrative_ontology:measurement(m23_tr_t6, m23_armed_group_dynamics, theater_ratio, 6, 0.53).
narrative_ontology:measurement(m23_tr_t9, m23_armed_group_dynamics, theater_ratio, 9, 0.55).

% Extraction over time
narrative_ontology:measurement(m23_be_t0, m23_armed_group_dynamics, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(m23_be_t3, m23_armed_group_dynamics, base_extractiveness, 3, 0.68).
narrative_ontology:measurement(m23_be_t6, m23_armed_group_dynamics, base_extractiveness, 6, 0.76).
narrative_ontology:measurement(m23_be_t9, m23_armed_group_dynamics, base_extractiveness, 9, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(m23_armed_group_dynamics, enforcement_mechanism).
narrative_ontology:affects_constraint(m23_armed_group_dynamics, drc_state_capacity).
narrative_ontology:affects_constraint(m23_armed_group_dynamics, east_african_regional_stability).
narrative_ontology:affects_constraint(m23_armed_group_dynamics, conflict_minerals_supply_chains).

% DUAL FORMULATION NOTE:
% M23 armed group dynamics should be decomposed into three structurally distinct constraint stories: (1) civilian coercion and taxation (snare, ε=0.78); (2) state territorial control loss (tangled rope, ε=0.62); (3) resource extraction coordination (rope, ε=0.35). Each has different beneficiaries, victims, and exit dynamics. The present story represents the aggregate constraint from all positions; decomposition enables targeted analysis of intervention points.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(m23_armed_group_dynamics, institutional, 0.42).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
