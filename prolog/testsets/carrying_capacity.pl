% ============================================================================
% CONSTRAINT STORY: carrying_capacity
% ============================================================================
% Version: 5.2 (Deferential Realism Core + Boltzmann + Purity + Network)
% Logic: 5.2 (Indexed Tuple P,T,E,S + Coupling + Purity + Network Drift)
% Generated: 2024-07-15
% ============================================================================

:- module(constraint_carrying_capacity, []).

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
 * * constraint_id: carrying_capacity
 * human_readable: Management of Ecological Carrying Capacity (K)
 * domain: economic/technological/social
 * * SUMMARY:
 * Carrying capacity (K) represents the maximum population size of a species that a specific environment can sustain indefinitely. This constraint models the social and economic systems built to manage, ignore, or exploit this biophysical limit. While the limit itself is a natural phenomenon, its management creates a constructed system of allocation and extraction that has vastly different effects on different populations.
 * * KEY AGENTS:
 * - The Subsistence Inhabitant: Experiences the managed limit as a Snare, where scarcity of resources (e.g., in an overpopulated slum) extracts survival capacity.
 * - The Resource Manager: Views the management of K as a Rope for coordinating sustainable extraction (e.g., fishing quotas, urban planning) to ensure long-term stability.
 * - The Systems Ecologist (Analytical): Observes that the management system is a Tangled Rope, providing genuine coordination for some while creating severe, asymmetric extraction for others.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Numerical anchors for v3.4 thresholds
domain_priors:base_extractiveness(carrying_capacity, 0.70). % High. Approaching K extracts ecosystem health and future viability.
domain_priors:suppression_score(carrying_capacity, 0.60).   % High. The physical limit suppresses the alternative of "infinite growth."
domain_priors:theater_ratio(carrying_capacity, 0.10).       % Low. The consequences are biophysical, not performative.

% Constraint metric facts — primary keys used by the classification engine.
narrative_ontology:constraint_metric(carrying_capacity, extractiveness, 0.70).
narrative_ontology:constraint_metric(carrying_capacity, suppression_requirement, 0.60).
narrative_ontology:constraint_metric(carrying_capacity, theater_ratio, 0.10).

% Constraint self-claim (what does the constraint claim to be?)
% Claims to be a natural law, but its management is a constructed enforcement mechanism.
narrative_ontology:constraint_claim(carrying_capacity, tangled_rope).

% Binary flags
domain_priors:requires_active_enforcement(carrying_capacity). % Required for Tangled Rope

% Structural property derivation hooks:
narrative_ontology:constraint_beneficiary(carrying_capacity, long_term_survivors).
narrative_ontology:constraint_beneficiary(carrying_capacity, ecosystem_stability).
narrative_ontology:constraint_victim(carrying_capacity, over_expanding_populations).
narrative_ontology:constraint_victim(carrying_capacity, marginalized_resource_users).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × π(P) × σ(S)
   ========================================================================== */

% PERSPECTIVE 1: THE SUBJECT (SNARE)
% For the person living in an over-capacity urban environment, K is a Snare.
% The extraction of space and clean air has turned their environment into a trap.
constraint_indexing:constraint_classification(carrying_capacity, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(constrained),
            spatial_scope(local))).

% PERSPECTIVE 2: THE BENEFICIARY (ROPE)
% For the institutional manager, K is a Rope. It is a functional coordination
% mechanism to prevent a "Tragedy of the Commons."
constraint_indexing:constraint_classification(carrying_capacity, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% The ecologist sees both the valid coordination function (managing resources)
% and the severe asymmetric extraction imposed on the powerless. This dual
% nature is the definition of a Tangled Rope.
constraint_indexing:constraint_classification(carrying_capacity, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(carrying_capacity_tests).

test(perspectival_gap) :-
    % Verify there is a perspectival gap between powerless and institutional.
    constraint_indexing:constraint_classification(carrying_capacity, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(carrying_capacity, TypeInstitutional, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeInstitutional.

test(threshold_validation) :-
    % Verify the base extractiveness is in the high-extraction range for Snare/Tangled Rope.
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(carrying_capacity, ExtMetricName, E),
    E >= 0.46.

test(tangled_rope_conditions_met) :-
    % Verify the structural properties for a Tangled Rope are present.
    domain_priors:requires_active_enforcement(carrying_capacity),
    narrative_ontology:constraint_beneficiary(carrying_capacity, _),
    narrative_ontology:constraint_victim(carrying_capacity, _).

:- end_tests(carrying_capacity_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The original file incorrectly classified this constraint as a 'mountain' from the analytical
 * perspective while assigning it a high base extractiveness (0.7), a logical contradiction
 * since mountains must have ε <= 0.15. This regeneration corrects the error by reframing
 * the constraint as the *management* of carrying capacity, not the physical law itself.
 *
 * This reframing allows for a consistent high extraction score, reflecting how the costs of
 * approaching the limit are asymmetrically distributed. The analytical observer, seeing both
 * the valid coordination function for resource managers (beneficiaries) and the severe
 * consequences for marginalized populations (victims), correctly classifies the system as a
 * Tangled Rope. The powerless experience it as a pure Snare, while the institutional managers
 * who set the rules see it as a beneficial Rope. This creates the classic perspectival gap.
 *
 * * MANDATROPHY ANALYSIS: [RESOLVED MANDATROPHY]
 * The Tangled Rope classification is critical for resolving mandatrophy. A simpler analysis
 * might label the system a Snare (focusing only on the slum dweller) or a Mountain/Rope
 * (focusing only on the ecological law or the manager). The Tangled Rope classification
 * prevents this by forcing the model to acknowledge both the legitimate coordination function
 * AND the asymmetric extraction it produces. It correctly identifies that the "tool" for
 * managing the whole system has been co-opted to benefit some at the severe expense of others.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_carrying_capacity,
    'Can technology permanently expand planetary carrying capacity (making the limit a temporary Scaffold), or does it merely shift bottlenecks and hide the eventual Snare?',
    'Long-term tracking of net energy return (EROI) and resource replenishment rates vs. technological throughput.',
    'If Scaffold: Growth can continue. If Snare: Current technological "solutions" are creating a larger, more catastrophic future collapse.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(carrying_capacity, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Temporal data models the intensification of the constraint as a population
% approaches its environmental limits. Extraction increases as resources become
% scarcer and competition rises. Theater remains low.

% Theater ratio over time (remains low and non-performative):
narrative_ontology:measurement(carrying_capacity_tr_t0, carrying_capacity, theater_ratio, 0, 0.05).
narrative_ontology:measurement(carrying_capacity_tr_t5, carrying_capacity, theater_ratio, 5, 0.08).
narrative_ontology:measurement(carrying_capacity_tr_t10, carrying_capacity, theater_ratio, 10, 0.10).

% Extraction over time (increases as limits are approached):
narrative_ontology:measurement(carrying_capacity_ex_t0, carrying_capacity, base_extractiveness, 0, 0.50).
narrative_ontology:measurement(carrying_capacity_ex_t5, carrying_capacity, base_extractiveness, 5, 0.60).
narrative_ontology:measurement(carrying_capacity_ex_t10, carrying_capacity, base_extractiveness, 10, 0.70).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA (v5.0-5.2)
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% The management of K is a classic resource allocation problem.
narrative_ontology:coordination_type(carrying_capacity, resource_allocation).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */