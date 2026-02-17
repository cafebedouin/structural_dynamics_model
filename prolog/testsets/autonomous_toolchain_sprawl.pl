% ============================================================================
% CONSTRAINT STORY: autonomous_toolchain_sprawl
% ============================================================================
% Version: 5.2 (Deferential Realism Core + Boltzmann + Purity + Network)
% Logic: 5.2 (Indexed Tuple P,T,E,S + Coupling + Purity + Network Drift)
% Generated: 2024-05-21
% ============================================================================

:- module(constraint_autonomous_toolchain_sprawl, []).

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
 * * constraint_id: autonomous_toolchain_sprawl
 * human_readable: The Recursive Maintenance Trap
 * domain: technological
 * * SUMMARY:
 * An organization deploys autonomous agents and CI/CD tools to manage infrastructure.
 * The "meta-tools" required to coordinate these agents proliferate faster than the
 * primary output. This intended "Rope" for efficiency becomes a "Snare" for engineers,
 * who are trapped in "infinite maintenance" of the toolchain itself, liquidating
 * creative surplus into recursive upkeep.
 * * KEY AGENTS:
 * - DevOps Engineer: Subject (Powerless)
 * - Cloud Infrastructure Provider: Beneficiary (Institutional)
 * - Systems Auditor: Auditor (Analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Numerical anchors for v3.4 thresholds
domain_priors:base_extractiveness(autonomous_toolchain_sprawl, 0.84). % Mountain <= 0.15, Rope <= 0.15, Snare >= 0.46
domain_priors:suppression_score(autonomous_toolchain_sprawl, 0.68).   % Structural property (raw, unscaled).
domain_priors:theater_ratio(autonomous_toolchain_sprawl, 0.79).       % Piton detection (>= 0.70)

% Constraint metric facts — primary keys used by the classification engine.
narrative_ontology:constraint_metric(autonomous_toolchain_sprawl, extractiveness, 0.84).
narrative_ontology:constraint_metric(autonomous_toolchain_sprawl, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(autonomous_toolchain_sprawl, theater_ratio, 0.79).

% Constraint self-claim (what does the constraint claim to be?)
% The toolchain is presented as an essential coordination mechanism.
narrative_ontology:constraint_claim(autonomous_toolchain_sprawl, piton).
narrative_ontology:human_readable(autonomous_toolchain_sprawl, "The Recursive Maintenance Trap").

% Binary flags
% The complexity requires active enforcement of standards and practices to prevent collapse.
domain_priors:requires_active_enforcement(autonomous_toolchain_sprawl). % Required for Tangled Rope

% Structural property derivation hooks:
narrative_ontology:constraint_beneficiary(autonomous_toolchain_sprawl, cloud_infrastructure_provider).
narrative_ontology:constraint_victim(autonomous_toolchain_sprawl, devops_engineer).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × π(P) × σ(S)
   ========================================================================== */

% PERSPECTIVE 1: THE SUBJECT (SNARE)
% The engineer is trapped: they cannot "unplug" the automation without
% systemic collapse, yet they spend most of their time fixing the automation.
constraint_indexing:constraint_classification(autonomous_toolchain_sprawl, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE BENEFICIARY (ROPE)
% The institution views the complex toolchain as a Rope—the only way
% to coordinate global-scale infrastructure with sub-second latency.
constraint_indexing:constraint_classification(autonomous_toolchain_sprawl, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% The default analytical view, detecting both the coordination function and
% the severe asymmetric extraction (0.84).
constraint_indexing:constraint_classification(autonomous_toolchain_sprawl, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 4: THE SYSTEMS AUDITOR (PITON)
% A different analytical view focusing on inertia. The high theater ratio (0.79)
% triggers a Piton classification: the "Efficiency Metrics" are performative,
% and the toolchain is maintained out of habit, not current utility.
constraint_indexing:constraint_classification(autonomous_toolchain_sprawl, piton,
    context(agent_power(analytical),
            time_horizon(historical),
            exit_options(arbitrage),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(autonomous_toolchain_sprawl_tests).

test(perspectival_gap) :-
    % Verify there is a perspectival gap between powerless and institutional.
    constraint_indexing:constraint_classification(autonomous_toolchain_sprawl, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(autonomous_toolchain_sprawl, TypeInstitutional, context(agent_power(institutional), _, _, _)),
    TypePowerless == snare,
    TypeInstitutional == rope,
    TypePowerless \= TypeInstitutional.

test(piton_trigger) :-
    % Ensure high theater ratio (0.79) correctly triggers the Piton classification.
    domain_priors:theater_ratio(autonomous_toolchain_sprawl, TR),
    TR >= 0.70,
    constraint_indexing:constraint_classification(autonomous_toolchain_sprawl, piton, context(agent_power(analytical), _, exit_options(arbitrage), _)).

test(tangled_rope_properties) :-
    % Verify that all structural requirements for a Tangled Rope are met.
    narrative_ontology:constraint_beneficiary(autonomous_toolchain_sprawl, _), % -> has_coordination_function
    narrative_ontology:constraint_victim(autonomous_toolchain_sprawl, _),     % -> has_asymmetric_extraction
    domain_priors:requires_active_enforcement(autonomous_toolchain_sprawl).

:- end_tests(autonomous_toolchain_sprawl_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * This constraint models a common failure mode in complex technological systems,
 * where maintenance costs ("meta-work") consume the surplus generated by the
 * system itself. The base extractiveness of 0.84 represents the proportion of
 * engineering time and cognitive load siphoned into maintaining the automation
 * rather than producing primary value. The high theater ratio (0.79) reflects
 * the state where the organization continues to justify the system based on
 * its original promise ("automation ROI") even though it no longer delivers net
 * benefits, a classic sign of a Piton.
 *
 * The dual analytical perspectives (Tangled Rope vs. Piton) capture the lifecycle
 * of this decay. An analysis focused on structure sees a Tangled Rope: it has
 * beneficiaries, victims, and requires enforcement. An analysis focused on
 * function and inertia sees a Piton: its primary purpose has atrophied into
 * theatrical maintenance. The temporal data confirms this drift.
 *
 * * [RESOLVED MANDATROPHY]:
 * The high extraction is resolved by classifying the system as a degraded
 * Tangled Rope that has become a Piton. This prevents misclassifying it as a
 * simple Snare by acknowledging its coordination origins, while the Piton
 * classification correctly identifies its current inertial, non-functional state.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    omega_toolchain_convergence,
    'Is the observed complexity sprawl an artifact of current architectural choices (a fixable Snare) or an irreducible property of large-scale software systems (a Mountain)?',
    'Tracking the ratio of "Maintenance Tickets" to "Feature Releases" across multiple organizations adopting different "No-Ops" platforms over a 5-year horizon.',
    'If ratio drops significantly with new platforms: it was a Snare of architecture. If ratio remains high or rises: it is a Mountain of Software Complexity.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(autonomous_toolchain_sprawl, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio: Rising from functional toolchain coordination (0.15)
% to inertial "Automation ROI" theater (0.79) as maintenance outpaces output.
narrative_ontology:measurement(sprawl_tr_t0, autonomous_toolchain_sprawl, theater_ratio, 0, 0.15).
narrative_ontology:measurement(sprawl_tr_t5, autonomous_toolchain_sprawl, theater_ratio, 5, 0.48).
narrative_ontology:measurement(sprawl_tr_t10, autonomous_toolchain_sprawl, theater_ratio, 10, 0.79).

% Extraction: Progressive accumulation of maintenance debt liquidating
% the engineer's creative surplus into recursive meta-layer upkeep.
narrative_ontology:measurement(sprawl_ex_t0, autonomous_toolchain_sprawl, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(sprawl_ex_t5, autonomous_toolchain_sprawl, base_extractiveness, 5, 0.62).
narrative_ontology:measurement(sprawl_ex_t10, autonomous_toolchain_sprawl, base_extractiveness, 10, 0.84).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA (v5.0-5.2)
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% The toolchain is a form of infrastructure for managing other infrastructure.
narrative_ontology:coordination_type(autonomous_toolchain_sprawl, global_infrastructure).

% Network relationships (structural influence edges)
% The sprawl directly impacts the working conditions and well-being of engineers.
narrative_ontology:affects_constraint(autonomous_toolchain_sprawl, platform_engineer_burnout).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */