% ============================================================================
% CONSTRAINT STORY: trajans_bridge_lifecycle
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_trajans_bridge_lifecycle, []).

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
 *   constraint_id: trajans_bridge_lifecycle
 *   human_readable: Trajan's Bridge over the Danube (Lifecycle)
 *   domain: technological/military
 *
 * SUMMARY:
 *   Trajan's Bridge over the Danube (completed circa 105 AD) exemplifies a
 *   constraint that transforms across its lifecycle from extraction mechanism
 *   to performative monument. Built to consolidate the Roman conquest of
 *   Dacia, the bridge served initially as military-logistics infrastructure,
 *   requiring massive coercive extraction (forced labor, material
 *   requisitions, displacements) to construct and maintain. As the Dacian
 *   wars concluded and Dacia was integrated into Roman provincial
 *   administration (by 275 AD, after repeated barbarian raids and Roman
 *   military withdrawal), the bridge's functional military value declined
 *   while its symbolic and monumental value increased. The constraint
 *   exhibits all six DR types depending on temporal and perspectival
 *   positioning: a Snare for the conquered Dacian population bearing forced
 *   labor costs; a Tangled Rope for frontier settlements experiencing mixed
 *   trade benefits and military taxation; a Rope for Roman military command
 *   during the conquest phase; a Piton for the imperial monument system
 *   during its degraded phase; a Scaffold for provincial merchants perceiving
 *   a sunset as alternative routes matured; and a false Mountain for
 *   analytical observers risk of naturalizing Roman imperial dominance as
 *   geographic necessity. The constraint's lifecycle trajectory shows
 *   base_extractiveness declining from 0.72 (conquest phase, maximum
 *   coercion) to 0.35 (decline phase, as functionality eroded) while
 *   theater_ratio increased from 0.25 to 0.72 (performative maintenance
 *   replacing functional logistics). This measurement pattern is diagnostic
 *   of Goodhart drift: as the bridge's original extraction mechanism
 *   (military logistics) succeeded and stabilized, the apparatus shifted to
 *   maintaining the constraint through symbolic and theatrical performance
 *   rather than structural necessity.
 *
 * KEY AGENTS:
 *   - Roman Military Command: Primary beneficiary (institutional/arbitrage) — captures military logistics advantage, territorial consolidation, rapid deployment capability
 *   - Imperial Treasury: Secondary beneficiary (institutional/arbitrage) — extracts tax revenue from provincial Dacia and redirect via monument maintenance
 *   - Dacian Population (conquered): Primary victim (powerless/trapped) — forced labor for construction, resource extraction, displacement during conquest
 *   - Frontier Provincial Settlements: Mixed agent (moderate/constrained) — benefits from trade access and military protection; bears taxation and conscription costs
 *   - Provincial Trade Coalition: Organized agent (organized/constrained) — merchants and local elites perceiving temporary infrastructure with alternative route sunset
 *   - Imperial Monument System: Institutional actor (institutional/arbitrage) — maintains bridge post-functionally for symbolic dominion display
 *   - Barbarian Confederations (Sarmatians, Germanic tribes): Opposing victim collective (powerless/trapped-to-organized) — perceive bridge as military asset blocking river mobility
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(trajans_bridge_lifecycle, 0.52).
domain_priors:suppression_score(trajans_bridge_lifecycle, 0.65).
domain_priors:theater_ratio(trajans_bridge_lifecycle, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(trajans_bridge_lifecycle, extractiveness, 0.52).
narrative_ontology:constraint_metric(trajans_bridge_lifecycle, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(trajans_bridge_lifecycle, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(trajans_bridge_lifecycle, tangled_rope).
narrative_ontology:human_readable(trajans_bridge_lifecycle, "Trajan's Bridge over the Danube (Lifecycle)").
narrative_ontology:topic_domain(trajans_bridge_lifecycle, "technological/military").

domain_priors:requires_active_enforcement(trajans_bridge_lifecycle).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(trajans_bridge_lifecycle, roman_military_command).
narrative_ontology:constraint_beneficiary(trajans_bridge_lifecycle, imperial_treasury).
narrative_ontology:constraint_beneficiary(trajans_bridge_lifecycle, roman_frontier_economy).
narrative_ontology:constraint_victim(trajans_bridge_lifecycle, dacian_population).
narrative_ontology:constraint_victim(trajans_bridge_lifecycle, provincial_population_displacement).
narrative_ontology:constraint_victim(trajans_bridge_lifecycle, frontier_stability).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CONQUERED DACIAN POPULATION (SNARE) — Cannot exit the occupation; forced labor demands, resource extraction, and military coercion are inescapable. Bears full cost of bridge construction and military logistics. Maximum extraction with no alternatives.
constraint_indexing:constraint_classification(trajans_bridge_lifecycle, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: FRONTIER PROVINCIAL SETTLEMENT (TANGLED ROPE) — Constrained by military occupation and tax demands, but also benefits from Roman trade routes, military protection against external threats, and access to imperial markets. Mixed extraction and coordination — some genuine settlement benefits, but asymmetric extraction via taxation and conscription.
constraint_indexing:constraint_classification(trajans_bridge_lifecycle, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: ROMAN MILITARY COMMAND (ROPE) — Experiences the bridge as a pure coordination mechanism: solving the logistical problem of crossing the Danube to sustain the Dacian campaign. Benefits from rapid troop deployment, supply chain efficiency, and territorial consolidation. Zero-extraction perspective from a military standpoint — the bridge is a force multiplier.
constraint_indexing:constraint_classification(trajans_bridge_lifecycle, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(continental))).

% PERSPECTIVE 4: IMPERIAL MONUMENT SYSTEM (PITON) — After the Dacian wars conclude (circa 275 AD), the bridge transitions from functional logistics to monumentality. Its primary function becomes performative: displaying Roman engineering prowess, imperial dominion, and civilizational superiority. The bridge persists through institutional inertia and symbolic value long after its original military purpose degrades. Theater ratio rises as maintenance costs exceed functional logistics value.
constraint_indexing:constraint_classification(trajans_bridge_lifecycle, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: PROVINCIAL TRADE COALITION (SCAFFOLD) — Merchants, local elites, and transport operators see the bridge as temporary infrastructure enabling post-conquest economic integration. Organized actors perceive a sunset clause: as Danubian provinces stabilize and alternative routes develop (land roads, downstream fords), the bridge's monopoly on cross-river movement declines. Theater ratio moderate because real trade benefits exist alongside symbolic imperial performance.
constraint_indexing:constraint_classification(trajans_bridge_lifecycle, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / PHYSICAL CONSTRAINT VIEW (MOUNTAIN) — From a universal analytical perspective, the Danube's width, current, and seasonal flooding represent an immutable physical constraint on riverine crossing. Any civilization requires bridging infrastructure to manage this obstacle. Viewed this way, the constraint appears as a natural law of geography — no power can exit the requirement for bridge technology. However, the structural data contradicts this classification: the extraction derives from Roman military power and imperial control, not from physics alone. This is a false summit revealing naturalization of imperial dominance as geographic necessity.
constraint_indexing:constraint_classification(trajans_bridge_lifecycle, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(trajans_bridge_lifecycle_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(trajans_bridge_lifecycle, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(trajans_bridge_lifecycle, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(trajans_bridge_lifecycle, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(trajans_bridge_lifecycle, TR),
    TR >= 0.70.

:- end_tests(trajans_bridge_lifecycle_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness (0.52 at endpoint): Moderate. The bridge begins at 0.72 (conquest phase, maximum coercive extraction for construction and military logistics) and declines to 0.35 (degraded phase, post-functional maintenance). The 0.52 midpoint value reflects the constraint after military value has begun eroding but before the full piton transition to pure theater. Suppression (0.65): Moderate-high. During conquest, suppression is near-total (forced labor, military occupation, no exit for Dacian population). During stability and decline, suppression moderates as the constraint shifts from coercive logistics to symbolic display — passive compliance replaces active coercion. The 0.65 represents the average across the lifecycle with heavy weight on the early, higher-suppression period. Theater ratio (0.58): Moderate. The bridge begins as functional infrastructure (theater ~0.25: genuine logistics benefit) and transitions to performative monument (theater ~0.72: maintenance exceeds functional value). The 0.58 endpoint reflects the constraint's degraded state where symbolic performance and institutional inertia dominate structural function.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates perspectival divergence across time horizons and structural positions. The Roman military command (conquest phase, immediate horizon) sees pure coordination (Rope) — the bridge solves a genuine logistics problem. The Dacian population (trapped, biographical horizon) sees pure extraction (Snare) — maximum coercion with no exit. The frontier settlements (constrained, generational horizon) see mixed coordination and extraction (Tangled Rope) — trade benefits coexist with military taxation. The provincial merchants (organized, generational horizon) see temporary infrastructure (Scaffold) — perceiving that alternative routes will eventually mature and break the bridge's monopoly. The imperial monument system (institutional, civilizational horizon) sees its own degraded ritual (Piton) — the bridge persists through symbolic value and institutional inertia despite functional obsolescence. The analytical observer at civilizational/universal scope risks a false Mountain, naturalizng the bridge as a response to immutable geography when it is actually the product of imperial military strategy and dominance. The perspectival gaps widen over time: early in the lifecycle, most actors experience the constraint as tangled_rope (mixed extraction and coordination in military logistics); late in the lifecycle, the victim classes experience snare while the empire experiences piton.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality value (d) follows from the agent's structural relationship to the extraction flow. Roman military command and imperial treasury are beneficiaries with arbitrage exit options (can redeploy resources, choose to abandon the province) — they derive low or negative d values, experiencing the constraint as a force multiplier (Rope classification). Dacian population are victims with trapped exit (no alternative to occupation or coercive labor) — they derive high d values (0.85–0.95), experiencing maximum extraction (Snare classification). Frontier settlements are mixed agents with constrained exit (economically dependent on trade networks the bridge enables, but also taxed and conscripted) — they derive moderate-to-high d values (0.50–0.65), experiencing tangled_rope. Provincial merchants are organized agents with constrained exit (dependent on bridge logistics but can eventually develop alternatives) — they derive moderate d values (~0.45–0.55), experiencing scaffold with sunset logic. The piton perspective (imperial monument system) derives from theater_ratio exceeding 0.70, indicating that performative maintenance dominates structural function — this is independent of beneficiary/victim status and reflects institutional inertia.
 *
 * MANDATROPHY ANALYSIS:
 *   LIFECYCLE CONSTRAINT EXEMPLAR: This constraint resolves the mandatrophy by showing how the same physical structure (the bridge) transitions across constraint types as its functional basis changes. Early lifecycle (105–180 AD): Tangled Rope — military coordination (genuine logistics benefit) with asymmetric extraction (forced labor, provincial taxation). The constraint has a real coordination function (moving armies across a 300+ meter river without it is militarily impossible) and real extraction (coercive labor, displacement). Midlife (180–275 AD): Piton + Tangled Rope hybrid — logistics value diminishes as the Dacian frontier stabilizes; monument value increases. Theater rises as maintenance cost per unit transported declines. The constraint begins to persist through symbolic power rather than functional necessity. Terminal decline (post-275 AD): Piton — as barbarian pressure intensifies (leading to Roman withdrawal) and alternative crossing routes mature, the bridge's functional value approaches zero while symbolic (imperial monument) value maintains institutional investment. The bridge persists as inertia, not as necessity. No single type is 'correct' — the constraint's type IS ITS LIFECYCLE. The false summit at the analytical/civilizational perspective reveals the analytical observer's risk of naturalizing contingent Roman imperial strategy as immutable geographic necessity. The actual constraint is not 'the Danube is wide' (Mountain); it is 'the Roman Empire chooses to project power across the Danube via monumental bridge infrastructure' (tangled_rope → piton).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    functional_vs_symbolic_transition,
    'At what point does the bridge''s primary function transition from military logistics to symbolic monumentality?',
    'Analysis of maintenance expense records, traffic volume, and military deployment schedules relative to time; correlation with imperial policy shifts away from active Dacian conquest',
    'If transition is sharp (post-275 AD): supports piton classification as deliberate degradation. If gradual: suggests mixed rope-and-monument character persists longer.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(functional_vs_symbolic_transition, empirical, 'Timing of functional-to-symbolic transition in bridge''s lifecycle').

omega_variable(
    extraction_sustainability,
    'Could the Dacian conquest and bridge construction have been sustained without coercive extraction exceeding suppression threshold of 0.65?',
    'Comparison with voluntary trade-based Roman frontier expansion (e.g., Rhine-Danube limes without conquest); analysis of Dacian gold resources and voluntary tribute alternatives',
    'If yes: bridge could have been rope or scaffold with lower suppression. If no: suppression is structurally required for imperial project, supporting tangled_rope/snare classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraction_sustainability, conceptual, 'Whether extraction levels were minimally required or excess of military strategy').

omega_variable(
    alternative_route_viability,
    'Did viable alternative Danube crossing routes exist that Roman military deliberately suppressed to monopolize bridge logistics?',
    'Geographic survey of Danube narrowings, ford locations, and historical documentation of Roman interdiction policy; comparison with pre-conquest crossing patterns',
    'If suppressed alternatives existed: extraction classification strengthens (Snare from victim perspective). If no alternatives: constraint becomes more like Mountain (unavoidable infrastructure requirement).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(alternative_route_viability, empirical, 'Whether viable alternative Danube crossings existed pre-bridge').

omega_variable(
    barbarian_coalition_responsiveness,
    'Did organized barbarian coalitions (Sarmatians, Germanic tribes) view the bridge as a coordination mechanism for mutual defense or purely as Roman extraction infrastructure?',
    'Analysis of post-bridge barbarian military alliances, siege documentation, and treaty negotiations; correlation between bridge threat and confederation formation',
    'If perceived as military threat: supports Snare classification for barbarian perspective. If perceived as neutral infrastructure: weakens snare/tangled_rope reading.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(barbarian_coalition_responsiveness, empirical, 'Barbarian military perception of bridge as threat vs neutral infrastructure').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(trajans_bridge_lifecycle, 105, 275).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(trajan_theater_conquest_phase, trajans_bridge_lifecycle, theater_ratio, 0, 0.25).
narrative_ontology:measurement(trajan_theater_midlife, trajans_bridge_lifecycle, theater_ratio, 50, 0.45).
narrative_ontology:measurement(trajan_theater_decline, trajans_bridge_lifecycle, theater_ratio, 170, 0.72).

% Extraction over time
narrative_ontology:measurement(trajan_extraction_conquest_phase, trajans_bridge_lifecycle, base_extractiveness, 0, 0.72).
narrative_ontology:measurement(trajan_extraction_midlife, trajans_bridge_lifecycle, base_extractiveness, 50, 0.58).
narrative_ontology:measurement(trajan_extraction_decline, trajans_bridge_lifecycle, base_extractiveness, 170, 0.35).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(trajans_bridge_lifecycle, enforcement_mechanism).
narrative_ontology:affects_constraint(trajans_bridge_lifecycle, danubian_frontier_militarization).
narrative_ontology:affects_constraint(trajans_bridge_lifecycle, roman_frontier_limes_system).

% DUAL FORMULATION NOTE:
% Trajan's Bridge is downstream of broader Roman frontier strategy (Danube militarization) and upstream of specific Dacian conquest logistics. The bridge instantiates the coordination-extraction hybrid of frontier consolidation: genuine military logistics problem (Danube crossing) paired with asymmetric resource extraction (conquered province labor and tribute). Decomposition into separate stories enables analysis of the bridge's pure-logistics phase vs. degraded-monument phase.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(trajans_bridge_lifecycle, institutional, 0.18).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
