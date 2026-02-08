% ============================================================================
% CONSTRAINT STORY: memetic_fitness_vs_truth
% ============================================================================
% Version: 5.2 (Deferential Realism Core + Boltzmann + Purity + Network)
% Logic: 5.2 (Indexed Tuple P,T,E,S + Coupling + Purity + Network Drift)
% Generated: 2024-05-21
% ============================================================================

:- module(constraint_memetic_fitness_vs_truth, []).

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
 * * constraint_id: memetic_fitness_vs_truth
 * human_readable: The Viral Distortion
 * domain: social/informational/technological
 * * SUMMARY:
 * A scenario where the "fitness" of an idea (its ability to spread and replicate
 * within a human-digital substrate) becomes decoupled from its "truth" (its
 * mapping to physical or logical reality). This "Rope" for rapid social
 * coordination becomes a "Snare" for the subject, whose cognitive agency is
 * liquidated as they are compelled to replicate high-fitness, low-truth memes
 * to remain socially relevant, trapping the collective in a delusional
 * equilibrium.
 * * KEY AGENTS:
 * - Information Consumer: Subject (Powerless)
 * - Platform Operator: Beneficiary (Institutional)
 * - Epistemic Ecologist: Auditor (Analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% High extraction (0.86) reflects the siphoning of the subject's attention and
% cognitive labor into the replication of non-functional, high-virality noise.
domain_priors:base_extractiveness(memetic_fitness_vs_truth, 0.86).
domain_priors:suppression_score(memetic_fitness_vs_truth, 0.75).   % Low-fitness "truth" is suppressed by the sheer volume of high-fitness "noise."
domain_priors:theater_ratio(memetic_fitness_vs_truth, 0.88).       % High theater: "Verification" symbols that only track social consensus, not truth.

% Constraint metric facts — primary keys used by the classification engine.
narrative_ontology:constraint_metric(memetic_fitness_vs_truth, extractiveness, 0.86).
narrative_ontology:constraint_metric(memetic_fitness_vs_truth, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(memetic_fitness_vs_truth, theater_ratio, 0.88).

% Constraint self-claim (what does the constraint claim to be?)
% The platform frames this as a necessary mechanism for coordinating attention.
narrative_ontology:constraint_claim(memetic_fitness_vs_truth, piton).

% Binary flags and structural properties for Tangled Rope classification
domain_priors:requires_active_enforcement(memetic_fitness_vs_truth). % Algorithmic promotion/demotion is active enforcement.
narrative_ontology:constraint_beneficiary(memetic_fitness_vs_truth, platform_operators).
narrative_ontology:constraint_victim(memetic_fitness_vs_truth, information_consumers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × π(P) × σ(S)
   ========================================================================== */

% PERSPECTIVE 1: THE SUBJECT (SNARE)
% The consumer is trapped: they must adopt the "trending" narrative to participate
% in social coordination, even if it contradicts their sensory data.
constraint_indexing:constraint_classification(memetic_fitness_vs_truth, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE BENEFICIARY (ROPE)
% The platform views memetic fitness as a Rope—the most efficient coordination
% substrate for aggregating human attention at a global scale.
constraint_indexing:constraint_classification(memetic_fitness_vs_truth, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 3: THE SYSTEMS AUDITOR (PITON)
% Theater ratio (0.88) > 0.70 triggers Piton: the "Trending" list is an
% inertial spike; it signals relevance without functional utility.
constraint_indexing:constraint_classification(memetic_fitness_vs_truth, piton,
    context(agent_power(analytical),
            time_horizon(historical),
            exit_options(analytical),
            spatial_scope(global))) :-
    domain_priors:theater_ratio(memetic_fitness_vs_truth, TR), TR > 0.70.

% PERSPECTIVE 4: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% Detects high extraction (0.86) masking as essential coordination (Rope).
% The classification engine requires enforcement, beneficiary, and victim.
constraint_indexing:constraint_classification(memetic_fitness_vs_truth, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(memetic_fitness_vs_truth_tests).

test(perspectival_gap) :-
    % Verify Snare for the powerless consumer vs Rope for the institutional platform.
    constraint_indexing:constraint_classification(memetic_fitness_vs_truth, snare,
        context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(memetic_fitness_vs_truth, rope,
        context(agent_power(institutional), _, _, _)),
    constraint_indexing:constraint_classification(memetic_fitness_vs_truth, tangled_rope,
        context(agent_power(analytical), _, _, _)).

test(piton_trigger) :-
    % Ensure high theater ratio (0.88) correctly triggers the Piton classification.
    constraint_indexing:constraint_classification(memetic_fitness_vs_truth, piton,
        context(agent_power(analytical), _, _, _)).

test(tangled_rope_structural_properties) :-
    % Verify that all three structural requirements for Tangled Rope are met.
    domain_priors:requires_active_enforcement(memetic_fitness_vs_truth),
    narrative_ontology:constraint_beneficiary(memetic_fitness_vs_truth, _),
    narrative_ontology:constraint_victim(memetic_fitness_vs_truth, _).

:- end_tests(memetic_fitness_vs_truth_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The extraction score (0.86) reflects a "Mandatrophy" state where the
 * "coordination" benefit of shared narratives is achieved by liquidating the
 * cognitive integrity of the subject population. Suppression (0.75) is high
 * because the algorithmic substrate actively demotes low-fitness (but true)
 * information in favor of high-fitness (often false) content that maximizes
 * engagement metrics.
 *
 * * PERSPECTIVAL GAP:
 * The Information Consumer feels a Snare because their worldview is
 * weaponized to maintain platform dwell-time. The Platform Operator sees a Rope
 * because memetic fitness is the only metric that can coordinate
 * billions of attention-slices in real-time. The Analytical Observer sees a
 * Tangled Rope, acknowledging the coordination function but identifying the
 * severe asymmetric extraction it enables.
 *
 * * [RESOLVED MANDATROPHY]:
 * Resolved via the Piton and Tangled Rope classifications. For an analytical
 * observer, the system's claim of "coordination" is revealed to be a highly
 * theatrical (Theater 0.88) and extractive (Extraction 0.86) process. The
 * Tangled Rope classification correctly identifies that a genuine coordination
 * function exists but has been coupled with a parasitic extraction mechanism,
 * preventing misclassification as a pure Snare.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% Required for high-extraction constraints (> 0.46).
omega_variable(
    omega_memetic_dominance,
    'Can a truth-signal ever out-compete a high-fitness delusion in this substrate, or is truth an evolutionary disadvantage (Snare vs Mountain)?',
    'Tracking the half-life and penetration depth of fact-checked corrections vs the original viral error across multiple platforms.',
    'If corrections consistently fail to penetrate: Mountain of Human Psychology/Cognitive Bias. If they succeed under different platform rules: Snare of current algorithmic design.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(memetic_fitness_vs_truth, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% This constraint intensified as platforms optimized for engagement over time.
% Initially, the coordination was more functional and less extractive.
%
% Theater ratio over time (triggers metric_substitution detection):
narrative_ontology:measurement(memetic_fitness_vs_truth_tr_t0, memetic_fitness_vs_truth, theater_ratio, 0, 0.40).
narrative_ontology:measurement(memetic_fitness_vs_truth_tr_t5, memetic_fitness_vs_truth, theater_ratio, 5, 0.75).
narrative_ontology:measurement(memetic_fitness_vs_truth_tr_t10, memetic_fitness_vs_truth, theater_ratio, 10, 0.88).

% Extraction over time (triggers extraction_accumulation detection):
narrative_ontology:measurement(memetic_fitness_vs_truth_ex_t0, memetic_fitness_vs_truth, base_extractiveness, 0, 0.50).
narrative_ontology:measurement(memetic_fitness_vs_truth_ex_t5, memetic_fitness_vs_truth, base_extractiveness, 5, 0.70).
narrative_ontology:measurement(memetic_fitness_vs_truth_ex_t10, memetic_fitness_vs_truth, base_extractiveness, 10, 0.86).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA (v5.0-5.2)
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% The constraint allocates the scarce resource of collective attention.
narrative_ontology:coordination_type(memetic_fitness_vs_truth, resource_allocation).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */