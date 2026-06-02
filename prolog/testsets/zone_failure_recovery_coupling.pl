% ============================================================================
% CONSTRAINT STORY: zone_failure_recovery_coupling
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_zone_failure_recovery_coupling, []).

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
 *   constraint_id: zone_failure_recovery_coupling
 *   human_readable: Zone Failure Recovery Coupling in Power Grid Infrastructure
 *   domain: infrastructure/systems_reliability
 *
 * SUMMARY:
 *   Zone failure recovery coupling in electrical power grids describes the
 *   structural interdependence between geographically separated transmission
 *   zones where the failure of one zone's generation or distribution
 *   infrastructure cascades to adjacent zones through power flow physics
 *   (Kirchhoff's laws create mandatory electromagnetic interdependence) and
 *   automated protection protocols (load-shedding relays disconnect
 *   distributed resources before conventional generators). This constraint
 *   exemplifies how the same structural phenomenon can classify as natural
 *   law, coordination mechanism, temporary problem, institutional inertia, or
 *   pure extraction depending on the observer's position and time horizon.
 *   The physics of electrical coupling is real — power flows follow voltage
 *   gradients regardless of zone boundaries. But the extraction mechanism
 *   (why distributed generators are shed before conventional generators, why
 *   load-shedding algorithms protect large centralized generation over
 *   distributed resources) is embedded in relay parameter choices, protection
 *   standards, and regulatory market design. The constraint exhibits the
 *   hallmark signature of a false summit: presented as immutable physics, but
 *   with identifiable beneficiaries (centralized grid operators, conventional
 *   generation monopolies) and victims (distributed generation, adjacent
 *   zones). The theater ratio (0.38) is relatively low because the protection
 *   relay logic has genuine function (preventing total blackout cascades),
 *   but recent technologies (microgrids, distributed frequency support, HVDC
 *   links with independent protection) show that equivalent function can be
 *   achieved with different architectural choices.
 *
 * KEY AGENTS:
 *   - Centralized Grid Operator: Primary beneficiary (institutional/arbitrage) — controls load-shedding authority, maintains operational centrality, benefits from information asymmetry about relay parameters and cascade thresholds
 *   - Conventional Generator Owners: Secondary beneficiary (powerful/arbitrage) — protected from shedding, benefit from preferential dispatch in recovery protocols, stable revenue during cascade events
 *   - Adjacent Zones: Primary victim (powerless/trapped) — electromagnetic coupling forces load shedding on them regardless of their generation capacity or demand levels; no exit from physics of shared transmission
 *   - Distributed Generation Operators: Secondary victim (organized/constrained) — first resources disconnected in cascades; asymmetric shedding burden; face relocation/islanding costs for exit
 *   - Microgrid Developers: Organized challenger (powerful/mobile) — see cascade coupling as temporary coordination problem; building alternative architecture (islanding, local control, peer-to-peer balancing) with 10-20 year sunset
 *   - Utility Relay Engineers: Institutional actor (institutional/constrained) — execute algorithms they inherited; parameter choices are path-dependent; standard certification creates barrier to algorithm evolution
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(zone_failure_recovery_coupling, 0.52).
domain_priors:suppression_score(zone_failure_recovery_coupling, 0.62).
domain_priors:theater_ratio(zone_failure_recovery_coupling, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(zone_failure_recovery_coupling, extractiveness, 0.52).
narrative_ontology:constraint_metric(zone_failure_recovery_coupling, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(zone_failure_recovery_coupling, theater_ratio, 0.38).

% --- Constraint claim ---
narrative_ontology:constraint_claim(zone_failure_recovery_coupling, tangled_rope).
narrative_ontology:human_readable(zone_failure_recovery_coupling, "Zone Failure Recovery Coupling in Power Grid Infrastructure").
narrative_ontology:topic_domain(zone_failure_recovery_coupling, "infrastructure/systems_reliability").

domain_priors:requires_active_enforcement(zone_failure_recovery_coupling).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(zone_failure_recovery_coupling, grid_operators).
narrative_ontology:constraint_beneficiary(zone_failure_recovery_coupling, load_balancing_algorithms).
narrative_ontology:constraint_victim(zone_failure_recovery_coupling, adjacent_zones).
narrative_ontology:constraint_victim(zone_failure_recovery_coupling, distributed_generation_operators).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ADJACENT ZONE UNDER CASCADE (SNARE) — No exit from electromagnetic coupling; load-shedding cascades are mandatory physics consequences. When Zone A fails, Zone B's protective relays shed load automatically. Zone B's operators cannot refuse this extraction — the coupling is physical law. Maximum suppression: no alternative exists except permanent disconnection from the grid.
constraint_indexing:constraint_classification(zone_failure_recovery_coupling, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: DISTRIBUTED GENERATION OPERATORS (TANGLED ROPE) — Coordinated with the grid (must synchronize frequency and phase) but asymmetrically extracting from them. When cascading failures occur, distributed resources are disconnected by protective relays before conventional generators. Generators benefit from grid access (coordination) but bear disproportionate shedding burden (extraction). Exit is costly (islanding requires specialized infrastructure; grid disconnection ends revenue).
constraint_indexing:constraint_classification(zone_failure_recovery_coupling, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: CENTRALIZED GRID OPERATOR (ROPE) — Experiences the coupling as coordination mechanism. Their authority to execute automated load-shedding protocols solves a genuine collective action problem (preventing total blackout cascades). They have arbitrage options: modify relay parameters, adjust zone boundaries, deploy HVDC interconnections. Net beneficiary — the coupling structures their control authority.
constraint_indexing:constraint_classification(zone_failure_recovery_coupling, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: MICROGRID AND ISLANDING DEVELOPERS (SCAFFOLD) — See the cascade coupling as a temporary coordination problem with a sunset. Distributed control, real-time voltage support, and intentional islanding protocols are building alternative failure pathways that don't propagate cascades across zones. Exit path is structural and visible (10-20 year transition to resilient distributed grids). Theater is low because the technical pathway is concrete.
constraint_indexing:constraint_classification(zone_failure_recovery_coupling, scaffold,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a universalizable perspective, electromagnetic coupling between adjacent power zones is a consequence of Kirchhoff's current law and nodal voltage equations. The interdependence is immutable at the physics level. However, the base properties show institutional beneficiaries, organized victims, and active enforcement — indicators of a false summit where physics gets naturalized to justify human-designed protection schemes.
constraint_indexing:constraint_classification(zone_failure_recovery_coupling, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 5: LEGACY RELAY CONFIGURATION STANDARDS (PITON) — The specific protective relay algorithms (over-current, under-voltage, rate-of-change-of-frequency thresholds) are institutionally maintained despite better alternatives being known. Utilities keep older relay settings in place due to standardization costs and certification delays. The theater is moderate (the relays function, but newer algorithms could prevent some cascades). Piton classification reflects institutional inertia rather than functional necessity.
constraint_indexing:constraint_classification(zone_failure_recovery_coupling, piton,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(zone_failure_recovery_coupling_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(zone_failure_recovery_coupling, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(zone_failure_recovery_coupling, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(zone_failure_recovery_coupling, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(zone_failure_recovery_coupling, TR),
    TR >= 0.70.

:- end_tests(zone_failure_recovery_coupling_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The coupling extracts from adjacent zones and distributed generators through mandatory load-shedding. Distributed resources are shed first, creating asymmetric burden. But extractiveness is not maximal (≥0.66 for snare) because: (1) the shedding prevents total blackout, providing genuine coordination benefit; (2) adjacent zones can build redundant generation/transmission (costly but possible, not physically impossible); (3) distributed generators can invest in islanding (constrained, not trapped). Suppression (0.62): Moderate-high. Significant barriers to exit include: electromagnetic coupling enforced by Kirchhoff's laws (no agent can escape), protective relay algorithms that prioritize conventional generators (embedded in decades of standards), regulatory frameworks that reward centralized dispatchability (market design), and certification delays for new relay logic (10-15 years for standards adoption). But suppression is not maximal because alternative architectures are technically feasible and being deployed. Theater ratio (0.38): Moderate-low. The protection relays have genuine function — they prevent cascading total blackouts. But recent evidence shows that equivalent reliability can be achieved with distributed control schemes (microgrids, frequency-responsive loads, HVDC with independent protection). The theater is in presenting the current centralized architecture as the only option.
 *
 * PERSPECTIVAL GAP:
 *   The central diagnostic gap is between the mountain perspective (zone coupling is immutable physics) and the scaffold perspective (coupling is a temporary institutional arrangement being displaced by better technology). The mountain appears justified because Kirchhoff's laws are real — power flows follow voltage equations. But the snare and tangled-rope perspectives reveal that the extraction mechanism (shedding prioritization, asymmetric burden allocation) is design-contingent. The grid operator's rope perspective is genuine — they are solving a real coordination problem. But their solution benefits them disproportionately (maintains their authority and information advantage). The piton perspective on legacy relay standards captures institutional inertia: the specific algorithms persist not because they are optimal but because they are locked in by standardization costs. The microgrid scaffold shows a real, concrete exit path with identifiable timeline (10-20 years for mature distributed grids to reach 20-30% penetration). This perspectival divergence indicates that the classification depends on whether one measures the physics-level constraint (mountain: Kirchhoff's laws are immutable) or the institution-level constraint (tangled-rope: relay design choices create extraction). These are two different constraints with different ε values.
 *
 * DIRECTIONALITY LOGIC:
 *   The grid operator benefits from the constraint (arbitrage: low d ≈ 0.10-0.15) because it centralizes their authority and information advantage. Conventional generators benefit (arbitrage: d ≈ 0.20) because they are protected from shedding. Distributed generators suffer (constrained victim: d ≈ 0.70-0.75) because they are shed first despite having no control over the cascade trigger. Adjacent zones suffer maximally (trapped victim: d ≈ 0.90-0.95) because electromagnetic coupling forces shedding regardless of their capacity or generation levels. The organized distributed generation sector has higher power than individual operators (d ≈ 0.55-0.65) because they can coordinate on technology standards, regulatory advocacy, and market participation. The microgrid developers have mobile exits (d ≈ 0.35-0.45) because they can build alternative architectures. These directionality values are stable over time because they reflect structural power asymmetries embedded in market design and regulatory frameworks, not temporary conditions.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy arises from distinguishing whether the zone coupling is a genuine coordination mechanism (rope: low-extraction collective action solution) or an extractive mechanism (snare or tangled-rope: high-extraction redistribution with a coordination cover story). The evidence distinguishes them: (1) Coordination function: The constraint does solve a real problem — preventing total blackout cascades through rapid load shedding. Equivalent function could theoretically be achieved through other means (demand response, generation scheduling, HVDC direct current links with independent protection), but the current mechanism is proven and reliable. This is genuine coordination. (2) Asymmetric extraction: Distributed generators are shed before conventional generators not because physics requires it but because protection relay parameters were set this way (historically, for simplicity; currently, maintained for administrative convenience). This is extraction — a design choice that benefits certain actors. (3) Enforcement: The coupling is actively maintained through relay certification standards (NERC, IEC 61850), utility operational procedures, and regulatory frameworks that reward centralized dispatchability. This is active enforcement, not passive physics. The tangled-rope classification is correct: the constraint has both genuine coordination function (preventing cascades) and asymmetric extraction (preferential shedding, authority concentration, market advantage). The false-summit omega documents whether this is presented as natural law (physics immutable) when in fact the extraction mechanism is policy-contingent.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    physics_vs_design_boundary,
    'Is zone failure coupling a consequence of Kirchhoff''s laws (immutable) or a consequence of centralized relay design (contingent)?',
    'Analyze alternative grid architectures (peer-to-peer HVDC, mesh topologies with distributed protection logic). If cascades persist despite different topology and control schemes, the coupling is physics-constrained. If cascades disappear with alternative architectures, the coupling is design-contingent.',
    'If physics-constrained: mountain classification is justified (though beneficiaries suggest false summit). If design-contingent: tangled-rope or scaffold classification is appropriate, and the coupling is subject to policy change.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(physics_vs_design_boundary, empirical, 'Whether coupling is immutable physics or contingent design').

omega_variable(
    distributed_generation_shedding_prioritization,
    'Why are distributed generators shed before conventional generators in cascade scenarios? Is this a required consequence of physical coupling or a choice embedded in relay algorithm design?',
    'Historical analysis of relay parameter selection; expert elicitation from protection engineers on design rationale; simulation of alternative shedding priorities (e.g., shed load uniformly, shed large generators first, shed conventional generators first). If alternative shedding orders prevent cascades, the current prioritization is design choice; if all alternatives fail similarly, the prioritization reflects inevitable physics constraints.',
    'If design choice: tangled-rope classification confirmed (asymmetric extraction via algorithm choice). If inevitable: snare classification for distributed generators (no escape from physics-mandated shedding). If hybrid: the coupling extracts via both physics and design, making mandatrophy analysis depend on which level of abstraction is being addressed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(distributed_generation_shedding_prioritization, empirical, 'Shedding prioritization as physics consequence vs. algorithmic choice').

omega_variable(
    islanding_technical_readiness,
    'Can distributed microgrids actually island successfully and achieve stable operation without central grid coordination? At what scale and cost?',
    'Technical performance data from pilot microgrids (Denmark, Japan, USA) on islanding success rates, frequency stability without external support, cost per megawatt. Deployment timeline estimates for reaching 20-30% of grid capacity as managed microgrids.',
    'If islanding is mature and cost-effective: scaffold sunset is real, and the coupling is temporarily necessary but structurally doomed. If islanding requires expensive infrastructure or unstable operation: scaffold perspective is aspirational, and the coupling persists indefinitely (reclassifies as permanent tangled-rope or snare).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(islanding_technical_readiness, empirical, 'Technical and economic viability of microgrid islanding as escape path').

omega_variable(
    centralized_optimization_necessity,
    'Does centralized grid-level optimization (demand-response, generator scheduling, cross-zone power flows) require the zone-coupling architecture, or is it an artifact of how current markets and regulatory frameworks are structured?',
    'Analysis of alternative market designs (localized peer-to-peer energy trading, agent-based grid optimization, fully distributed economic dispatch). Simulation comparing social cost (blackout risk, generation cost) under centralized vs. distributed dispatch. Feasibility assessment of regulatory redesign required for each alternative.',
    'If centralized optimization is functionally necessary: the coupling is a rational (if extractive) solution to a real coordination problem. If centralized optimization is contingent: the coupling structure could be redesigned entirely, and the current beneficiary advantage (grid operators'' authority) is policy-granted rather than inevitable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(centralized_optimization_necessity, conceptual, 'Whether centralized optimization requires zone-coupled architecture').

omega_variable(
    false_summit_natural_law,
    'Is the zone coupling presented as a natural law of physics when in fact it is a human institutional choice embedded in protection relay algorithms and zone boundary definitions?',
    'Examine regulatory filings, utility documentation, and grid standards (NERC, IEC 61850) for language naturalizing the coupling. Conduct interviews with engineers and policy makers: do they discuss the coupling as immutable physics or as a design choice? Cross-check with the empirical record: have relay parameters, zone boundaries, or protection schemes ever been changed to reduce cascades, and if so, with what consequences?',
    'If false summit confirmed: the mountain perspective is a mislabeling of a contingent institutional arrangement. The coupling benefits certain actors (grid operators, incumbents) and harms others (adjacent zones, distributed generators). Reclassification would shift the policy debate from ''how do we live with immutable physics?'' to ''who benefits from this design and who should we redesign for?''',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(false_summit_natural_law, conceptual, 'Zone coupling as false summit: naturalizing contingent institutional choice as physics').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(zone_failure_recovery_coupling, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(zfrc_tr_t0, zone_failure_recovery_coupling, theater_ratio, 0, 0.32).
narrative_ontology:measurement(zfrc_tr_t5, zone_failure_recovery_coupling, theater_ratio, 5, 0.35).
narrative_ontology:measurement(zfrc_tr_t10, zone_failure_recovery_coupling, theater_ratio, 10, 0.38).

% Extraction over time
narrative_ontology:measurement(zfrc_be_t0, zone_failure_recovery_coupling, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(zfrc_be_t5, zone_failure_recovery_coupling, base_extractiveness, 5, 0.45).
narrative_ontology:measurement(zfrc_be_t10, zone_failure_recovery_coupling, base_extractiveness, 10, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(zfrc_su_t0, zone_failure_recovery_coupling, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(zfrc_su_t5, zone_failure_recovery_coupling, suppression_requirement, 5, 0.59).
narrative_ontology:measurement(zfrc_su_t10, zone_failure_recovery_coupling, suppression_requirement, 10, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(zone_failure_recovery_coupling, enforcement_mechanism).
narrative_ontology:affects_constraint(zone_failure_recovery_coupling, distributed_generation_integration).
narrative_ontology:affects_constraint(zone_failure_recovery_coupling, microgrid_autonomy_standards).
narrative_ontology:affects_constraint(zone_failure_recovery_coupling, grid_resilience_standards).

% DUAL FORMULATION NOTE:
% Zone failure coupling decomposes into two constraints with different ε values: (1) electromagnetic_interdependence (ε≈0.05, mountain) — Kirchhoff's laws create mandatory voltage coupling, immutable physics; (2) relay_shedding_prioritization (ε≈0.52, tangled-rope) — choice of which generators to shed first, extractive institutional design. These are linked: the physical interdependence is real, but the extraction mechanism (why distributed is shed before conventional) is design-contingent. Separate stories with different ε values.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(zone_failure_recovery_coupling, organized, 0.6).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
