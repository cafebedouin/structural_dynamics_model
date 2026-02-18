% ============================================================================
% CONSTRAINT STORY: railway_gauge_standard
% ============================================================================
% Version: 5.2 (Deferential Realism Core + Boltzmann + Purity + Network)
% Logic: 5.2 (Indexed Tuple P,T,E,S + Coupling + Purity + Network Drift)
% Generated: 2024-05-21
% ============================================================================

:- module(constraint_railway_gauge_standard, []).

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
 * * constraint_id: railway_gauge_standard
 * human_readable: The Standard Railway Gauge (4 ft 8.5 in / 1435 mm)
 * domain: technological/economic
 * * SUMMARY:
 * The Standard Gauge is a classic example of technological lock-in and path dependence.
 * It was set by George Stephenson based on existing horse-drawn coal wagons. Despite
 * "Broad Gauge" alternatives offering better stability and speed, the Standard Gauge
 * became a global invariant due to network effects, creating an immutable 'Mountain'
 * of infrastructure that now functions as an inertial 'Piton'.
 * * KEY AGENTS:
 * - The Passenger/Freight User: Subject (Powerless)
 * - The National Transport Authority: Beneficiary (Institutional)
 * - Isambard Kingdom Brunel (Innovator): Subject (Moderate)
 * - The Systems Auditor: Auditor (Analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Numerical anchors for v3.4 thresholds
domain_priors:base_extractiveness(railway_gauge_standard, 0.30). % Extracts "optimal technical performance" by forcing a sub-optimal standard.
domain_priors:suppression_score(railway_gauge_standard, 0.20).   % Persists via inertia and network effects, not active suppression.
domain_priors:theater_ratio(railway_gauge_standard, 0.75).       % High ratio of performative compliance (maintaining the standard) to functional optimality.

% Constraint metric facts — primary keys used by the classification engine.
narrative_ontology:constraint_metric(railway_gauge_standard, extractiveness, 0.30).
narrative_ontology:constraint_metric(railway_gauge_standard, suppression_requirement, 0.20).
narrative_ontology:constraint_metric(railway_gauge_standard, theater_ratio, 0.75).

% Constraint self-claim (what does the constraint claim to be?)
narrative_ontology:constraint_claim(railway_gauge_standard, piton).
narrative_ontology:human_readable(railway_gauge_standard, "The Standard Railway Gauge (4 ft 8.5 in / 1435 mm)").
narrative_ontology:topic_domain(railway_gauge_standard, "technological/economic").

% Structural property derivation hooks:
narrative_ontology:constraint_beneficiary(railway_gauge_standard, interconnected_logistics_operators).
narrative_ontology:constraint_victim(railway_gauge_standard, high_speed_rail_engineers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × π(P) × σ(S)
   Power (P) and Scope (S) both affect effective extraction.
   Scope modifiers: local=0.8, regional=0.9, national=1.0,
                    continental=1.1, global=1.2, universal=1.0.
   ========================================================================== */

% PERSPECTIVE 1: THE PASSENGER / FREIGHT USER (MOUNTAIN)
% For the ordinary user, the gauge is an immutable fact of the world.
constraint_indexing:constraint_classification(railway_gauge_standard, mountain,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: THE NATIONAL TRANSPORT AUTHORITY (ROPE)
% For the regulator, the standard is a pure coordination mechanism.
constraint_indexing:constraint_classification(railway_gauge_standard, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(continental))).

% PERSPECTIVE 3: ISAMBARD KINGDOM BRUNEL (THE INNOVATOR) (SNARE)
% For the innovator with a superior but incompatible alternative, the standard is a snare.
constraint_indexing:constraint_classification(railway_gauge_standard, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: THE SYSTEMS AUDITOR (PITON)
% The analyst sees a constraint that was once a functional Rope but now persists
% through inertia, creating more friction than it resolves.
constraint_indexing:constraint_classification(railway_gauge_standard, piton,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))) :-
    domain_priors:theater_ratio(railway_gauge_standard, TR), TR > 0.70.

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(railway_gauge_standard_tests).

test(perspectival_gap) :-
    % Verify there is a perspectival gap between key agents.
    constraint_indexing:constraint_classification(railway_gauge_standard, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(railway_gauge_standard, TypeInstitutional, context(agent_power(institutional), _, _, _)),
    constraint_indexing:constraint_classification(railway_gauge_standard, TypeModerate, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeInstitutional,
    TypeInstitutional \= TypeModerate.

test(piton_classification_trigger) :-
    % Verify the Piton classification is correctly triggered by the theater ratio.
    domain_priors:theater_ratio(railway_gauge_standard, TR),
    ( TR > 0.70 ->
        constraint_indexing:constraint_classification(railway_gauge_standard, piton, context(agent_power(analytical), _, _, _))
    ;
        \+ constraint_indexing:constraint_classification(railway_gauge_standard, piton, context(agent_power(analytical), _, _, _))
    ).

:- end_tests(railway_gauge_standard_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * This constraint is a classic case of path dependence. The scores reflect its dual nature.
 * The base extractiveness (0.30) is moderate, representing the opportunity cost of not using a
 * technically superior gauge. Suppression (0.20) is low because the standard persists due to
 * massive network effects and sunk costs (inertia), not active coercion.
 * The key metric is the high theater_ratio (0.75). This captures the modern reality: immense
 * effort is spent maintaining a sub-optimal standard (performative activity) rather than
 * pursuing a more functional solution. This high ratio is what justifies the Piton classification
 * from an analytical perspective, showing its degradation from a functional Rope.
 *
 * PERSPECTIVAL GAP:
 * - To a user (powerless), it's an unchangeable Mountain.
 * - To a regulator (institutional), it's a valuable Rope for coordination.
 * - To an innovator (moderate), it's a Snare that strangles better technology.
 * - To an analyst (analytical), it's a Piton, a relic maintained by inertia.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_railway_gauge_standard,
    "Can digital 'automatic gauge-changing' wheels truly untie the 'Snare' of the Standard Gauge, or is the cost of retrofitting always an insurmountable 'Mountain'?",
    "Monitor the adoption rate and economic viability of variable-gauge axles in cross-border corridors (e.g., Spain/France).",
    "If successful: Different gauges can coexist more flexibly (Rope). If too costly: Incompatibility remains a permanent 'Mountain' of friction.",
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(railway_gauge_standard, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% This constraint did not start as a Piton. It was a functional Rope.
% The theater_ratio increased over time as the standard became more of a
% liability due to technological advances it could not accommodate.
% This drift from functional coordination to inertial maintenance is key.
% Base extractiveness is low, so temporal data is not strictly required, but
% is included for theater_ratio to model the lifecycle drift into a Piton.

% Theater ratio over time (triggers metric_substitution detection):
narrative_ontology:measurement(rgs_tr_t0, railway_gauge_standard, theater_ratio, 0, 0.10).
narrative_ontology:measurement(rgs_tr_t5, railway_gauge_standard, theater_ratio, 5, 0.45).
narrative_ontology:measurement(rgs_tr_t10, railway_gauge_standard, theater_ratio, 10, 0.75).

% Extraction over time (remains relatively stable):
narrative_ontology:measurement(rgs_ex_t0, railway_gauge_standard, base_extractiveness, 0, 0.30).
narrative_ontology:measurement(rgs_ex_t5, railway_gauge_standard, base_extractiveness, 5, 0.30).
narrative_ontology:measurement(rgs_ex_t10, railway_gauge_standard, base_extractiveness, 10, 0.30).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA (v5.0-5.2)
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% A technical gauge is a quintessential information standard.
narrative_ontology:coordination_type(railway_gauge_standard, information_standard).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */