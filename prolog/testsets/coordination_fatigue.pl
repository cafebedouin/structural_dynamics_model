% ============================================================================
% CONSTRAINT STORY: coordination_fatigue
% ============================================================================
% Version: 5.2 (Deferential Realism Core + Boltzmann + Purity + Network)
% Logic: 5.2 (Indexed Tuple P,T,E,S + Coupling + Purity + Network Drift)
% Generated: 2024-05-21
% ============================================================================

:- module(constraint_coordination_fatigue, []).

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
    constraint_indexing:constraint_classification/3.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * * constraint_id: coordination_fatigue
 * human_readable: The Consensus Exhaustion Loop
 * domain: social/organizational/technological
 * * SUMMARY:
 * A scenario where the energy required to maintain consensus and synchronization
 * across a hyper-connected network exceeds the creative output of its members.
 * As the "Rope" of coordination thickens, it siphons the subject's agency into
 * endless administrative overhead, transforming the system into a Snare.
 * * KEY AGENTS:
 * - Distributed Contributor: Subject (Powerless)
 * - Protocol Governance: Beneficiary (Institutional)
 * - Network Health Auditor: Auditor (Analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% High extraction (0.78) as the system siphons the subject's primary
% cognitive labor into the maintenance of the coordination itself.
domain_priors:base_extractiveness(coordination_fatigue, 0.78).
domain_priors:suppression_score(coordination_fatigue, 0.62).
domain_priors:theater_ratio(coordination_fatigue, 0.74). % Piton threshold (> 0.70)

% Constraint metric facts — primary keys used by the classification engine.
narrative_ontology:constraint_metric(coordination_fatigue, extractiveness, 0.78).
narrative_ontology:constraint_metric(coordination_fatigue, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(coordination_fatigue, theater_ratio, 0.74).

% Constraint self-claim (what does the constraint claim to be?)
narrative_ontology:constraint_claim(coordination_fatigue, tangled_rope).

% Binary flags
domain_priors:requires_active_enforcement(coordination_fatigue). % Required for Tangled Rope

% Structural property derivation hooks:
%   has_coordination_function/1 is DERIVED from constraint_beneficiary/2
%   has_asymmetric_extraction/1 is DERIVED from constraint_victim/2
narrative_ontology:constraint_beneficiary(coordination_fatigue, protocol_governance).
narrative_ontology:constraint_victim(coordination_fatigue, distributed_contributor).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × π(P) × σ(S)
   ========================================================================== */

% PERSPECTIVE 1: THE SUBJECT (SNARE)
% The contributor is trapped: the cost of leaving or diverging from the
% collective sync is higher than the diminishing return of staying.
constraint_indexing:constraint_classification(coordination_fatigue, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE BENEFICIARY (ROPE)
% The governance layer views high synchronization as a Rope—the only way
% to ensure protocol stability and prevent fragmentation at scale.
constraint_indexing:constraint_classification(coordination_fatigue, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% Detects high extraction (0.78) and suppression (0.62) alongside a genuine
% coordination function as a Tangled Rope.
constraint_indexing:constraint_classification(coordination_fatigue, tangled_rope,
    context(agent_power(analytical),
            time_horizon(historical),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 4: THE SYSTEMS AUDITOR (PITON)
% Theater ratio (0.74) > 0.70 triggers Piton: the "consensus rituals" are
% now a non-functional spike of logic that maintains the illusion of progress.
constraint_indexing:constraint_classification(coordination_fatigue, piton,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(universal))) :-
    domain_priors:theater_ratio(coordination_fatigue, TR), TR > 0.70.

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(coordination_fatigue_tests).

test(perspectival_gap) :-
    % Verify Snare for the subject vs Rope for the institutional governance.
    constraint_indexing:constraint_classification(coordination_fatigue, snare,
        context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(coordination_fatigue, rope,
        context(agent_power(institutional), _, _, _)),
    constraint_indexing:constraint_classification(coordination_fatigue, tangled_rope,
        context(agent_power(analytical), _, _, _)).

test(piton_audit_logic) :-
    % Ensure high theater results in Piton detection for analytical auditors.
    constraint_indexing:constraint_classification(coordination_fatigue, piton,
        context(agent_power(analytical), _, _, _)).

test(threshold_validation) :-
    % Ensure extraction (0.78) is in the high-extraction range for Snare/Tangled Rope.
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(coordination_fatigue, ExtMetricName, E),
    (E =< 0.05 -> fail ; E >= 0.46).

:- end_tests(coordination_fatigue_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The extraction score (0.78) reflects a "Mandatrophy" state where the
 * cognitive energy of the network is consumed by the friction of synchronization.
 * The system started as a genuine coordination mechanism (Rope) but accumulated
 * overhead and performative rituals, causing extraction and theater to rise.
 *
 * * PERSPECTIVAL GAP:
 * The Distributed Contributor feels a Snare because they spend more time in
 * "alignment" than in "creation." The Protocol Governance sees a
 * Rope because consensus is the foundation of their institutional power.
 * * [RESOLVED MANDATROPHY]:
 * Resolved via the Piton and Tangled Rope classifications. For an analytical
 * observer, the coordination is no longer functional relative to reality
 * (Theater 0.74); the system is an inert spike siphoning the creative surplus.
 * The Tangled Rope classification correctly identifies the dual nature of the
 * constraint: it has a real coordination function (beneficiary exists) but also
 * imposes severe asymmetric extraction (victim exists), and requires active
 * enforcement to maintain.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% Required for high-extraction constraints (> 0.46).
omega_variable(
    omega_coordination_fatigue,
    'Is there a hard biological limit to coordination, or is this a design flaw (Snare vs Mountain)?',
    'Auditing the delta between synchronization speed and creative throughput over time.',
    'If throughput remains flat: Mountain of Biology. If throughput drops: Snare of Design.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(coordination_fatigue, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Temporal data models the system's degradation from a functional Rope into
% a high-extraction, high-theater Tangled Rope/Piton.
% Required for high-extraction constraints (base_extractiveness > 0.46).

% Theater ratio over time (triggers metric_substitution detection):
narrative_ontology:measurement(cf_tr_t0, coordination_fatigue, theater_ratio, 0, 0.10).
narrative_ontology:measurement(cf_tr_t5, coordination_fatigue, theater_ratio, 5, 0.45).
narrative_ontology:measurement(cf_tr_t10, coordination_fatigue, theater_ratio, 10, 0.74).

% Extraction over time (triggers extraction_accumulation detection):
narrative_ontology:measurement(cf_ex_t0, coordination_fatigue, base_extractiveness, 0, 0.20).
narrative_ontology:measurement(cf_ex_t5, coordination_fatigue, base_extractiveness, 5, 0.55).
narrative_ontology:measurement(cf_ex_t10, coordination_fatigue, base_extractiveness, 10, 0.78).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA (v5.0-5.2)
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% The system's primary function is to enforce consensus rules.
narrative_ontology:coordination_type(coordination_fatigue, enforcement_mechanism).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */