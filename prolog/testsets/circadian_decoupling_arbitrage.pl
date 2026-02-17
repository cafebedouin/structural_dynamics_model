% ============================================================================
% CONSTRAINT STORY: circadian_decoupling_arbitrage
% ============================================================================
% Version: 5.2 (Deferential Realism Core + Boltzmann + Purity + Network)
% Logic: 5.2 (Indexed Tuple P,T,E,S + Coupling + Purity + Network Drift)
% Generated: 2024-07-27
% ============================================================================

:- module(constraint_circadian_decoupling_arbitrage, []).

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
    narrative_ontology:human_readable/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * * constraint_id: circadian_decoupling_arbitrage
 * human_readable: The Circadian Decoupling Arbitrage
 * domain: bio_industrial
 * * SUMMARY:
 * This constraint describes the structural bypass of the biological 24-hour
 * cycle in industrial settings. By using high-intensity blue light, melatonin
 * suppressors, and pharmaceuticals, subjects enter a state of "perpetual noon"
 * to enable 24/7 operations. While profitable for operators (Rope), it creates
 * an extraction of long-term health and social connectivity (Snare) from workers.
 * * KEY AGENTS:
 * - The Shift-Worker: Subject (Powerless). Trading biological health for wage premiums.
 * - The Global Logistics Hub: Beneficiary (Institutional). Requires 24/7 uptime.
 * - The Chronobiologist: Auditor (Analytical). Monitors the metabolic debt.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

domain_priors:base_extractiveness(circadian_decoupling_arbitrage, 0.62). % High: externalizes long-term metabolic cost (diabetes, cardiac stress).
domain_priors:suppression_score(circadian_decoupling_arbitrage, 0.70).   % High: Exit requires leaving high-wage industrial roles.
domain_priors:theater_ratio(circadian_decoupling_arbitrage, 0.15).       % Low: This is a highly functional, non-theatrical physical intervention.

% Constraint metric facts — primary keys used by the classification engine.
narrative_ontology:constraint_metric(circadian_decoupling_arbitrage, extractiveness, 0.62).
narrative_ontology:constraint_metric(circadian_decoupling_arbitrage, suppression_requirement, 0.70).
narrative_ontology:constraint_metric(circadian_decoupling_arbitrage, theater_ratio, 0.15).

% Constraint self-claim: The system is presented as a necessary coordination tool.
narrative_ontology:constraint_claim(circadian_decoupling_arbitrage, tangled_rope).
narrative_ontology:human_readable(circadian_decoupling_arbitrage, "The Circadian Decoupling Arbitrage").

% Binary flags and structural properties
domain_priors:requires_active_enforcement(circadian_decoupling_arbitrage). % Enforcement via environmental lighting, scheduling, and pharma protocols.

% Structural property derivation hooks for Tangled Rope classification:
narrative_ontology:constraint_beneficiary(circadian_decoupling_arbitrage, global_logistics_hubs).
narrative_ontology:constraint_victim(circadian_decoupling_arbitrage, shift_workers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × π(P) × σ(S)
   ========================================================================== */

% PERSPECTIVE 1: THE SHIFT-WORKER (SNARE)
% For the worker, the system is a Snare. The immediate paycheck is the bait,
% while the biographical cost is a trap they cannot easily exit.
% χ = 0.62 * π(powerless:1.5) * σ(local:0.8) = 0.744
constraint_indexing:constraint_classification(circadian_decoupling_arbitrage, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: THE LOGISTICS HUB (ROPE)
% To the institution, this is a Rope. It coordinates labor across global
% time zones, ensuring that "time" is never a bottleneck for capital.
% χ = 0.62 * π(institutional:-0.2) * σ(global:1.2) = -0.1488 (felt as a benefit)
constraint_indexing:constraint_classification(circadian_decoupling_arbitrage, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% Analytically, the system is a Tangled Rope. It has a genuine coordination
% function (enabling 24/7 logistics) but achieves it via high, asymmetric
% extraction from a specific group, requiring active enforcement.
% χ = 0.62 * π(analytical:1.15) * σ(global:1.2) = 0.8556
constraint_indexing:constraint_classification(circadian_decoupling_arbitrage, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(circadian_arbitrage_tests).

test(perspectival_gap) :-
    % Verify there is a perspectival gap between powerless and institutional.
    constraint_indexing:constraint_classification(circadian_decoupling_arbitrage, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(circadian_decoupling_arbitrage, TypeInstitutional, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeInstitutional,
    TypePowerless == snare,
    TypeInstitutional == rope.

test(tangled_rope_verification) :-
    % Verify the analytical perspective correctly identifies the hybrid nature.
    constraint_indexing:constraint_classification(circadian_decoupling_arbitrage, tangled_rope, context(agent_power(analytical), _, _, _)).

test(threshold_validation) :-
    narrative_ontology:constraint_metric(circadian_decoupling_arbitrage, extractiveness, E),
    E >= 0.46.

:- end_tests(circadian_arbitrage_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The core of this constraint is a "biological loan" with a high, deferred
 * interest rate. The base extractiveness (0.62) is high because the system
 * externalizes the long-term metabolic cost (diabetes, cardiac stress) onto
 * the individual worker while the beneficiary captures 100% of the additional
 * "awake" utility. The Perspectival Gap is stark: the institutional beneficiary
 * experiences it as pure coordination (Rope), while the powerless worker
 * experiences it as a health-depleting trap (Snare).
 *
 * MANDATROPHY ANALYSIS:
 * The analytical classification as a Tangled Rope is critical. It prevents the
 * system from being misclassified as either a pure Snare (ignoring its genuine
 * coordination function for global logistics) or a pure Rope (ignoring the
 * severe, asymmetric extraction). The system is a hybrid: it solves a real
 * coordination problem, but does so via a highly extractive and coercive
 * mechanism. The omega variable questions whether the extracted value (health)
 * can ever be repaid.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_circadian_reversibility,
    'Is the metabolic debt incurred during decoupling permanently reversible upon cessation?',
    'Longitudinal studies of retired night-shift workers vs. control groups, tracking biomarkers for metabolic syndrome and all-cause mortality.',
    'If irreversible, the constraint is a pure Snare with a coordination facade. If fully reversible, it functions more like a high-cost Scaffold.',
    confidence_without_resolution(low)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(circadian_decoupling_arbitrage, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% This is a high-extraction constraint, requiring temporal data.
% The model shows extraction increasing over time as the system is optimized
% to maximize uptime, further externalizing biological costs.
% Theater ratio remains low, as this is a functional, not performative, system.

% Theater ratio over time (metric_substitution detection):
narrative_ontology:measurement(cda_tr_t0, circadian_decoupling_arbitrage, theater_ratio, 0, 0.10).
narrative_ontology:measurement(cda_tr_t5, circadian_decoupling_arbitrage, theater_ratio, 5, 0.12).
narrative_ontology:measurement(cda_tr_t10, circadian_decoupling_arbitrage, theater_ratio, 10, 0.15).

% Extraction over time (extraction_accumulation detection):
narrative_ontology:measurement(cda_ex_t0, circadian_decoupling_arbitrage, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(cda_ex_t5, circadian_decoupling_arbitrage, base_extractiveness, 5, 0.55).
narrative_ontology:measurement(cda_ex_t10, circadian_decoupling_arbitrage, base_extractiveness, 10, 0.62).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA (v5.0-5.2)
   ========================================================================== */

% Coordination type: The system's primary function is to allocate human
% resources (labor) across a 24-hour cycle, bypassing natural time constraints.
narrative_ontology:coordination_type(circadian_decoupling_arbitrage, resource_allocation).

% Network relationships: This bio-industrial practice is coupled with and
% enables the stability of other complex systems.
narrative_ontology:affects_constraint(circadian_decoupling_arbitrage, global_supply_chain_fragility).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */