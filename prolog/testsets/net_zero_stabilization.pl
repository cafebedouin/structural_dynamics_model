% ============================================================================
% CONSTRAINT STORY: net_zero_stabilization
% ============================================================================
% Version: 5.2 (Deferential Realism Core + Boltzmann + Purity + Network)
% Logic: 5.2 (Indexed Tuple P,T,E,S + Coupling + Purity + Network Drift)
% Generated: 2024-07-15
% ============================================================================

:- module(constraint_net_zero_stabilization, []).

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
 * * constraint_id: net_zero_stabilization
 * human_readable: The Net Zero Carbon Constraint
 * domain: scientific/political/economic
 * * SUMMARY:
 * Before 2005, the scientific consensus was that global temperatures could be stabilized while still allowing for a small budget of CO2 emissions. Research by physicists David Frame and Myles Allen demonstrated that warming only stops when human-caused emissions reach net zero. This discovery fundamentally shifted global climate policy, creating a new, far more demanding constraint on industrial and economic activity.
 * * KEY AGENTS:
 * - Legacy Carbon Emitters: Subjects (Powerless) facing the extraction of their previously assumed "right to emit".
 * - Future Generations/Climate Bodies: Beneficiaries (Institutional) of a stabilized climate.
 * - Climate Scientists: The Analytical observers who discovered and defined the constraint.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Numerical anchors for v3.4 thresholds
domain_priors:base_extractiveness(net_zero_stabilization, 0.85). % Snare extraction >= 0.46. Reaching net zero requires total extraction of carbon-emitting capabilities from current systems.
domain_priors:suppression_score(net_zero_stabilization, 0.45).   % The previous consensus (allowing ~2.5Gt emissions) was suppressed by the new model.
domain_priors:theater_ratio(net_zero_stabilization, 0.10).       % The constraint is highly functional, not theatrical.

% Constraint metric facts — primary keys used by the classification engine.
narrative_ontology:constraint_metric(net_zero_stabilization, extractiveness, 0.85).
narrative_ontology:constraint_metric(net_zero_stabilization, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(net_zero_stabilization, theater_ratio, 0.10).

% Constraint self-claim (what does the constraint claim to be?)
% It is presented as a discovery of an immutable physical law.
narrative_ontology:constraint_claim(net_zero_stabilization, tangled_rope).
narrative_ontology:human_readable(net_zero_stabilization, "The Net Zero Carbon Constraint").

% Binary flags
domain_priors:requires_active_enforcement(net_zero_stabilization). % Required for Tangled Rope. Policy, treaties, and carbon markets are all enforcement mechanisms.

% Structural property derivation hooks:
% The Earth System (and future generations) benefit from temperature stability.
narrative_ontology:constraint_beneficiary(net_zero_stabilization, global_climate_stability).
% Carbon-heavy industries and high-emitting nations lose the "fair chunk" of emissions they once believed was safe.
narrative_ontology:constraint_victim(net_zero_stabilization, legacy_carbon_emitters).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × π(P) × σ(S)
   ========================================================================== */

% PERSPECTIVE 1: THE SUBJECT (SNARE)
% For legacy emitters, the rule is a Snare that extracts their ability to operate without costly removals.
constraint_indexing:constraint_classification(net_zero_stabilization, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE BENEFICIARY (ROPE)
% For institutions representing future generations, it is a pure Rope coordinating global action for survival.
constraint_indexing:constraint_classification(net_zero_stabilization, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% Analytically, it's a Tangled Rope: a necessary coordination mechanism (beneficiaries)
% that imposes severe, asymmetric extraction (victims) and requires active enforcement.
constraint_indexing:constraint_classification(net_zero_stabilization, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(net_zero_stabilization_tests).

test(perspectival_gap) :-
    % Verify the gap between the Subject (Snare) and Beneficiary (Rope).
    constraint_indexing:constraint_classification(net_zero_stabilization, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(net_zero_stabilization, TypeInstitutional, context(agent_power(institutional), _, _, _)),
    TypePowerless == snare,
    TypeInstitutional == rope,
    TypePowerless \= TypeInstitutional.

test(threshold_validation_and_analytical_type) :-
    % Verify high extraction and the correct analytical classification.
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(net_zero_stabilization, ExtMetricName, E),
    E >= 0.46,
    constraint_indexing:constraint_classification(net_zero_stabilization, tangled_rope, context(agent_power(analytical), _, _, _)).

:- end_tests(net_zero_stabilization_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The base extractiveness is set to 0.85 to reflect the totalizing nature of the "net zero" requirement, which extracts the entire operational paradigm of the carbon-based economy. The suppression score of 0.45 reflects the scientific and political displacement of the prior, more lenient consensus.
 *
 * The key insight is the perspectival gap. For legacy emitters, this is a pure Snare, removing their ability to operate. For beneficiaries (future generations represented by institutions), it is a pure Rope, coordinating survival. The analytical view must synthesize these facts. Because the constraint has a clear coordination function (stabilizing the climate), has asymmetric victims, and requires intense global enforcement, it is a canonical Tangled Rope. It is not a Mountain, because human policy (the "net" part, involving removals) is required to meet it; the physics alone just describes the problem.
 *
 * * MANDATROPHY ANALYSIS: [RESOLVED MANDATROPHY]
 * Classifying this as a pure Snare would be inaccurate, as it would ignore the genuine, world-saving coordination function. Classifying it as a Rope would ignore the immense coercive extraction imposed on specific sectors. The Tangled Rope classification resolves this by acknowledging both facets simultaneously. It correctly models a necessary, beneficial coordination effort that is inextricably tangled with high levels of asymmetric extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    omega_net_zero_stabilization_1,
    "Is the extraction of carbon space a physical necessity (Mountain) or a policy choice that ignores lower-impact alternatives?",
    "Audit of global temperature responses to small, sustained, sub-zero emissions over centuries.",
    "If necessity: Mountain. If choice: Tangled Rope/Snare.",
    confidence_without_resolution(medium)
).

omega_variable(
    omega_net_zero_stabilization_2,
    "Can 'equivalent removals' be achieved at the required gigatonne scale, or is the 'net' in 'net zero' a fiction?",
    "Monitor the deployment and energy-return-on-investment of scaled Carbon Capture and Storage (CCS) technology.",
    "If viable: Tangled Rope. If non-viable: The target becomes a systemic Snare.",
    confidence_without_resolution(low)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(net_zero_stabilization, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% This models the constraint solidifying from a scientific proposal into a hard policy requirement,
% increasing its effective extractiveness over the interval. Theater remains low.
%
% Theater ratio over time (triggers metric_substitution detection):
narrative_ontology:measurement(net_zero_tr_t0, net_zero_stabilization, theater_ratio, 0, 0.05).
narrative_ontology:measurement(net_zero_tr_t5, net_zero_stabilization, theater_ratio, 5, 0.08).
narrative_ontology:measurement(net_zero_tr_t10, net_zero_stabilization, theater_ratio, 10, 0.10).

% Extraction over time (triggers extraction_accumulation detection):
narrative_ontology:measurement(net_zero_ex_t0, net_zero_stabilization, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(net_zero_ex_t5, net_zero_stabilization, base_extractiveness, 5, 0.75).
narrative_ontology:measurement(net_zero_ex_t10, net_zero_stabilization, base_extractiveness, 10, 0.85).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA (v5.0-5.2)
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% The constraint functions as a global enforcement mechanism for emissions policy.
narrative_ontology:coordination_type(net_zero_stabilization, enforcement_mechanism).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */