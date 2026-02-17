% ============================================================================
% CONSTRAINT STORY: cost_of_observation
% ============================================================================
% Version: 5.2 (Deferential Realism Core + Boltzmann + Purity + Network)
% Logic: 5.2 (Indexed Tuple P,T,E,S + Coupling + Purity + Network Drift)
% Generated: 2024-07-16
% ============================================================================

:- module(constraint_cost_of_observation, []).

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
 * * constraint_id: cost_of_observation
 * human_readable: The Evolutionary Cost of Observation
 * domain: philosophical/evolutionary
 * * SUMMARY:
 * This meta-constraint describes the "tax" paid by any situated observer.
 * The act of observing and processing information requires metabolic and
 * cognitive resources, creating an inherent cost function. This cost
 * extracts time, energy, and agency from any agent attempting to navigate
 * its environment, transforming what might be a neutral physical reality
 * into a landscape of constraints.
 * * KEY AGENTS:
 * - The Situated Agent: Subject (Powerless) who experiences the cost directly.
 * - The Evolutionary Process: Beneficiary (Institutional) that leverages this cost as a selection pressure.
 * - The Aeternal Observer: Auditor (Analytical) who views the cost as a fundamental law.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Numerical anchors for v3.4 thresholds
domain_priors:base_extractiveness(cost_of_observation, 0.85). % High because observation is never free; metabolic/cognitive load is inherent extraction.
domain_priors:suppression_score(cost_of_observation, 0.40).   % Moderate suppression; alternatives to observation are limited for any goal-oriented agent.
domain_priors:theater_ratio(cost_of_observation, 0.05).       % Very low; the cost is functional, not performative.

% Constraint metric facts — primary keys used by the classification engine.
narrative_ontology:constraint_metric(cost_of_observation, extractiveness, 0.85).
narrative_ontology:constraint_metric(cost_of_observation, suppression_requirement, 0.40).
narrative_ontology:constraint_metric(cost_of_observation, theater_ratio, 0.05).

% Constraint self-claim (what does the constraint claim to be?)
% It presents as a fundamental, unchangeable feature of reality.
narrative_ontology:constraint_claim(cost_of_observation, tangled_rope).
narrative_ontology:human_readable(cost_of_observation, "The Evolutionary Cost of Observation").
domain_priors:requires_active_enforcement(cost_of_observation).

% Structural property derivation hooks:
narrative_ontology:constraint_beneficiary(cost_of_observation, evolutionary_process).
narrative_ontology:constraint_victim(cost_of_observation, situated_agent).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × π(P) × σ(S)
   Power (P) and Scope (S) both affect effective extraction.
   Scope modifiers: local=0.8, regional=0.9, national=1.0,
                    continental=1.1, global=1.2, universal=1.0.
   ========================================================================== */

% PERSPECTIVE 1: THE SITUATED AGENT (SNARE)
% Experiences the cost as a direct, unavoidable drain on resources, trapping
% them in a constant struggle for energy and attention.
% χ = 0.85 * 1.5 (powerless) * 0.8 (local) = 1.02
constraint_indexing:constraint_classification(cost_of_observation, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: THE EVOLUTIONARY PROCESS (ROPE)
% From the perspective of the "designer" (evolution), this cost is a pure
% coordination mechanism that selects for efficient observers.
% χ = 0.85 * -0.2 (institutional) * 1.2 (global) = -0.204
constraint_indexing:constraint_classification(cost_of_observation, rope,
    context(agent_power(institutional),
            time_horizon(historical),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: THE AETERNAL OBSERVER (MOUNTAIN)
% From a detached, analytical viewpoint, the cost is simply a law of physics
% and information theory, an unchangeable feature of the universe.
% χ = 0.85 * 1.15 (analytical) * 1.0 (universal) = 0.9775
% Despite high chi, the analytical frame recognizes it as a natural law, classifying it as Mountain.
constraint_indexing:constraint_classification(cost_of_observation, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(cost_of_observation_tests).

test(perspectival_gap) :-
    % Verify the gap between the Situated Agent (Snare) and the Evolutionary Process (Rope).
    constraint_indexing:constraint_classification(cost_of_observation, TypePowerless,
        context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(cost_of_observation, TypeInstitutional,
        context(agent_power(institutional), _, _, _)),
    TypePowerless == snare,
    TypeInstitutional == rope,
    TypePowerless \= TypeInstitutional.

test(analytical_view_is_mountain) :-
    % Verify the analytical observer correctly identifies it as a Mountain despite high chi.
    constraint_indexing:constraint_classification(cost_of_observation, TypeAnalytical,
        context(agent_power(analytical), _, _, _)),
    TypeAnalytical == mountain.

test(threshold_validation) :-
    % Verify base extractiveness is high, triggering mandatory data requirements.
    narrative_ontology:constraint_metric(cost_of_observation, extractiveness, E),
    E > 0.46.

:- end_tests(cost_of_observation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * This constraint explores the perspectival nature of fundamental costs. The
 * base extractiveness is set to a high 0.85 to represent the unavoidable
 * "metabolic tax" of agency and observation.
 *
 * The Perspectival Gap is stark:
 * - For the 'powerless' situated agent, this tax is a Snare, consuming finite
 *   resources and limiting potential actions. The local scope amplifies this feeling.
 * - For the 'institutional' lens of evolution, this tax is a Rope, a perfect
 *   coordination mechanism that filters for organisms with efficient sensory
 *   and cognitive processing.
 * - For the 'analytical' observer, the cost is a Mountain, a fundamental
 *   property of thermodynamics and information theory applied to embodied agents.
 *
 * * MANDATROPHY ANALYSIS: [RESOLVED MANDATROPHY]
 * The high extraction (0.85) is not a sign of a constructed, predatory system
 * but a feature of physical law. The system correctly resolves this by
 * classifying the constraint as a Mountain from the analytical perspective,
 * preventing a misclassification of natural law as a universal Snare. The
 * Snare classification is reserved for the subjective experience of the agent
 * trapped by that law.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_cost_of_observation,
    'Is there a theoretical minimum to the cost of observation (a Landauer limit for cognition), or can evolution/technology reduce it to near zero?',
    'Advances in biophysics, information theory, and low-power computing.',
    'If a floor exists, it confirms the Mountain classification. If not, the Snare is potentially solvable, making it a technological rather than physical limit.',
    confidence_without_resolution(low)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(cost_of_observation, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% This is a fundamental constraint, assumed to be stable over the measured
% interval. The values do not change, reflecting its Mountain-like nature from
% a long-term perspective.

% Theater ratio over time (stable and low):
narrative_ontology:measurement(cost_of_observation_tr_t0, cost_of_observation, theater_ratio, 0, 0.05).
narrative_ontology:measurement(cost_of_observation_tr_t5, cost_of_observation, theater_ratio, 5, 0.05).
narrative_ontology:measurement(cost_of_observation_tr_t10, cost_of_observation, theater_ratio, 10, 0.05).

% Extraction over time (stable and high):
narrative_ontology:measurement(cost_of_observation_ex_t0, cost_of_observation, base_extractiveness, 0, 0.85).
narrative_ontology:measurement(cost_of_observation_ex_t5, cost_of_observation, base_extractiveness, 5, 0.85).
narrative_ontology:measurement(cost_of_observation_ex_t10, cost_of_observation, base_extractiveness, 10, 0.85).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA (v5.0-5.2)
   ========================================================================== */

% No specific coordination type or network relationships apply to this
% fundamental, philosophical constraint.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */