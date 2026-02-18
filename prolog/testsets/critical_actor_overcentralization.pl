% ============================================================================
% CONSTRAINT STORY: critical_actor_overcentralization
% ============================================================================
% Version: 5.2 (Deferential Realism Core + Boltzmann + Purity + Network)
% Logic: 5.2 (Indexed Tuple P,T,E,S + Coupling + Purity + Network Drift)
% Generated: 2024-05-21
% ============================================================================

:- module(constraint_critical_actor_overcentralization, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * * constraint_id: critical_actor_overcentralization
 * human_readable: The Single Point of Failure
 * domain: logistical/technological/economic
 * * SUMMARY:
 * A scenario where a network’s functional survival depends entirely on a single
 * node—be it a clearinghouse bank, a cloud provider, or a charismatic leader.
 * This "Rope" for achieving massive coordination efficiency and
 * standardization becomes a "Snare" for the peripheral nodes, whose survival
 * agency is liquidated because they have no exit options or redundant
 * pathways. When the center falters or chooses to extract higher rents,
 * the periphery is trapped in a terminal dependency.
 * * KEY AGENTS:
 * - Peripheral Node (Small Business): Subject (Powerless)
 * - Central Clearinghouse: Beneficiary (Institutional)
 * - Systemic Risk Analyst: Auditor (Analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% High extraction (0.88) reflects the near-total siphoning of the periphery's
% strategic agency to maintain the center's dominance.
domain_priors:base_extractiveness(critical_actor_overcentralization, 0.88).
domain_priors:suppression_score(critical_actor_overcentralization, 0.78). % Decentralized alternatives are suppressed by "efficiency" mandates and high barriers to entry.
domain_priors:theater_ratio(critical_actor_overcentralization, 0.93).    % Extreme theater: "Redundancy Drills" and "Service Level Agreements" that fail to account for true systemic collapse.

% Constraint metric facts — primary keys used by the classification engine.
narrative_ontology:constraint_metric(critical_actor_overcentralization, extractiveness, 0.88).
narrative_ontology:constraint_metric(critical_actor_overcentralization, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(critical_actor_overcentralization, theater_ratio, 0.93).

% Constraint self-claim (what does the constraint claim to be?)
narrative_ontology:constraint_claim(critical_actor_overcentralization, tangled_rope).
narrative_ontology:human_readable(critical_actor_overcentralization, "The Single Point of Failure").
narrative_ontology:topic_domain(critical_actor_overcentralization, "logistical/technological/economic").

% Binary flags and structural properties
domain_priors:requires_active_enforcement(critical_actor_overcentralization). % Required for Tangled Rope

% Structural property derivation hooks:
narrative_ontology:constraint_beneficiary(critical_actor_overcentralization, central_clearinghouse).
narrative_ontology:constraint_victim(critical_actor_overcentralization, peripheral_node).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × π(P) × σ(S)
   ========================================================================== */

% PERSPECTIVE 1: THE SUBJECT (SNARE)
% The peripheral node is trapped: they cannot survive without the hub,
% yet the hub's existence liquidates their ability to ever become autonomous.
constraint_indexing:constraint_classification(critical_actor_overcentralization, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE BENEFICIARY (ROPE)
% The center views the overcentralization as a Rope—the only way to coordinate
% global-scale stability and ensure a legible, frictionless economy.
constraint_indexing:constraint_classification(critical_actor_overcentralization, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% The default analytical view detects the hybrid nature: a genuine coordination
% function (beneficiary exists) coupled with coercive, asymmetric extraction
% (victim exists) that requires active enforcement.
constraint_indexing:constraint_classification(critical_actor_overcentralization, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 4: THE SYSTEMS AUDITOR (PITON)
% An auditor focused on functional integrity sees the extreme theater (0.93)
% as the dominant feature. The "Risk Management" is performative, making the
% constraint a Piton—an inertial spike whose original function is atrophied.
constraint_indexing:constraint_classification(critical_actor_overcentralization, piton,
    context(agent_power(analytical),
            time_horizon(historical),
            exit_options(arbitrage),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(critical_actor_overcentralization_tests).

test(perspectival_gap) :-
    % Verify Snare for the powerless periphery vs Rope for the institutional center.
    constraint_indexing:constraint_classification(critical_actor_overcentralization, snare,
        context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(critical_actor_overcentralization, rope,
        context(agent_power(institutional), _, _, _)),
    constraint_indexing:constraint_classification(critical_actor_overcentralization, TypeAnalytical,
        context(agent_power(analytical), _, _, _)),
    TypeAnalytical \= rope,
    TypeAnalytical \= snare.

test(piton_trigger) :-
    % Ensure high theater ratio (0.93) correctly triggers the Piton classification.
    domain_priors:theater_ratio(critical_actor_overcentralization, TR), TR > 0.7,
    constraint_indexing:constraint_classification(critical_actor_overcentralization, piton,
        context(agent_power(analytical), _, exit_options(arbitrage), _)).

test(tangled_rope_structural_validity) :-
    % Verify that the structural requirements for Tangled Rope are met.
    domain_priors:requires_active_enforcement(critical_actor_overcentralization),
    narrative_ontology:constraint_beneficiary(critical_actor_overcentralization, _),
    narrative_ontology:constraint_victim(critical_actor_overcentralization, _).

:- end_tests(critical_actor_overcentralization_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The extraction score (0.88) reflects a "Mandatrophy" state where the
 * claimed "coordination" benefit of a central authority is achieved by
 * liquidating the resilience and agency of all sub-nodes in the network. The
 * suppression score (0.78) is high because the central hub's economies of
 * scale and network effects create insurmountable barriers to entry for any
 * decentralized alternative. The theater ratio (0.93) is extreme, representing
 * performative "resilience planning" that ignores the fundamental fragility
 * of the centralized model.
 *
 * * PERSPECTIVAL GAP:
 * The Peripheral Node feels a Snare because their "efficiency" is actually
 * a fragility they cannot escape. The Central Hub sees a Rope because
 * centralization coordinates the massive volume of traffic necessary
 * to stay globally competitive.
 *
 * * [RESOLVED MANDATROPHY]:
 * The system resolves the Mandatrophy of E=0.88 by refusing to assign a single
 * classification. Instead, it uses multiple analytical perspectives. The
 * default analytical view classifies it as a Tangled Rope, acknowledging both
 * its coordination function and its severe asymmetric extraction. A second
 * analytical view, focused on functional decay, classifies it as a Piton due
 * to the overwhelming theater ratio (0.93). This dual-classification prevents
 * the system from mislabeling the structure as either pure coordination (Rope)
 * or pure extraction (Snare), capturing the nuanced reality of a decayed,
 * extractive, yet still functional, coordination system.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% Required for high-extraction constraints (> 0.46).
omega_variable(
    omega_hub_collapse_velocity,
    'Does a hub collapse into a "Mountain" of chaos, or can decentralized Ropes be deployed in time (Snare vs Mountain)?',
    'Tracking the time-to-recovery of decentralized networks versus centralized hubs during 2026-style outages.',
    'If decentralized nodes recover faster: Snare of current hub policy. If they fail to coordinate: Mountain of Complexity.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(critical_actor_overcentralization, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% This constraint began as a high-extraction but functional system and decayed
% into a highly theatrical and extractive state.
%
% Theater ratio over time (triggers metric_substitution detection):
narrative_ontology:measurement(cao_tr_t0, critical_actor_overcentralization, theater_ratio, 0, 0.20).
narrative_ontology:measurement(cao_tr_t5, critical_actor_overcentralization, theater_ratio, 5, 0.60).
narrative_ontology:measurement(cao_tr_t10, critical_actor_overcentralization, theater_ratio, 10, 0.93).

% Extraction over time (triggers extraction_accumulation detection):
narrative_ontology:measurement(cao_ex_t0, critical_actor_overcentralization, base_extractiveness, 0, 0.50).
narrative_ontology:measurement(cao_ex_t5, critical_actor_overcentralization, base_extractiveness, 5, 0.75).
narrative_ontology:measurement(cao_ex_t10, critical_actor_overcentralization, base_extractiveness, 10, 0.88).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA (v5.0-5.2)
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
narrative_ontology:coordination_type(critical_actor_overcentralization, global_infrastructure).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */