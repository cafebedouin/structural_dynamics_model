% ============================================================================
% CONSTRAINT STORY: hedonic_adaptation_baseline
% ============================================================================
% Version: 5.2 (Deferential Realism Core + Boltzmann + Purity + Network)
% Logic: 5.2 (Indexed Tuple P,T,E,S + Coupling + Purity + Network Drift)
% Generated: 2024-05-21
% ============================================================================

:- module(constraint_hedonic_adaptation_baseline, []).

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
 * * constraint_id: hedonic_adaptation_baseline
 * human_readable: The Hedonic Adaptation Baseline
 * domain: psychological/biological
 * * SUMMARY:
 * Hedonic adaptation is the biological tendency of humans to quickly return to a
 * relatively stable level of happiness despite major positive or negative life
 * events. This acts as a Mountain (fixed biology) for the individual, but its
 * predictable nature is exploited by economic systems, creating a Tangled Rope
 * where an evolutionary coordination mechanism (resilience) is coupled with
 * asymmetric extraction (the attention economy's need for constant novelty).
 * * KEY AGENTS:
 * - The Platform User: Subject (Powerless). Chasing the next "high" or plateau on the hedonic treadmill.
 * - The Attention Economy Platform: Beneficiary (Institutional). Captures the "treadmill" energy for engagement.
 * - The Evolutionary Biologist: Auditor (Analytical). Observes the survival utility and its modern exploitation.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% [RESOLVED MANDATROPHY]: Extraction (0.48) is not "stolen" by a person but
% "drained" by the biological mechanism itself, which forces continuous effort
% for diminishing returns, a process captured and amplified by modern platforms.
domain_priors:base_extractiveness(hedonic_adaptation_baseline, 0.48).
domain_priors:suppression_score(hedonic_adaptation_baseline, 1.00).   % Total: You cannot "exit" your own neurobiology.
domain_priors:theater_ratio(hedonic_adaptation_baseline, 0.10).       % Low: This is a raw biological limit, not a performance.

% Constraint metric facts — primary keys used by the classification engine.
narrative_ontology:constraint_metric(hedonic_adaptation_baseline, extractiveness, 0.48).
narrative_ontology:constraint_metric(hedonic_adaptation_baseline, suppression_requirement, 1.0).
narrative_ontology:constraint_metric(hedonic_adaptation_baseline, theater_ratio, 0.1).

% Constraint self-claim (what does the constraint claim to be?)
narrative_ontology:constraint_claim(hedonic_adaptation_baseline, tangled_rope).

% Binary flags and structural properties
domain_priors:requires_active_enforcement(hedonic_adaptation_baseline). % Internalized enforcement via dopamine regulation.

% Structural property derivation hooks for Tangled Rope:
narrative_ontology:constraint_beneficiary(hedonic_adaptation_baseline, attention_economy_platforms).
narrative_ontology:constraint_victim(hedonic_adaptation_baseline, platform_users).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × π(P) × σ(S)
   ========================================================================== */

% PERSPECTIVE 1: THE PLATFORM USER (MOUNTAIN)
% At the biographical level, the return to baseline feels like an
% unchangeable law of nature. You cannot "win" the game of permanent bliss.
constraint_indexing:constraint_classification(hedonic_adaptation_baseline, mountain,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: THE ATTENTION ECONOMY PLATFORM (SNARE)
% From an institutional view, this biological constant is a resource to be
% mined. It functions as a Snare ensuring user "retention." Since users adapt
% to features, new ones must be constantly fed to them to maintain engagement.
constraint_indexing:constraint_classification(hedonic_adaptation_baseline, snare,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% Viewed historically, adaptation is a Rope; it allows humans to coordinate
% and survive in extreme environments. However, this coordination function is
% now tangled with an extractive function by the attention economy. This hybrid
% nature makes it a Tangled Rope.
constraint_indexing:constraint_classification(hedonic_adaptation_baseline, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hedonic_adaptation_baseline_tests).

test(perspectival_gap) :-
    % Verify the constraint is a Mountain for the individual but a Snare for the institution.
    constraint_indexing:constraint_classification(hedonic_adaptation_baseline, mountain, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(hedonic_adaptation_baseline, snare, context(agent_power(institutional), _, _, _)),
    constraint_indexing:constraint_classification(hedonic_adaptation_baseline, tangled_rope, context(agent_power(analytical), _, _, _)).

test(threshold_validation) :-
    narrative_ontology:constraint_metric(hedonic_adaptation_baseline, extractiveness, E),
    E >= 0.46. % Ensures high extraction signature for the "treadmill" effect.

test(tangled_rope_properties) :-
    % Verify all structural requirements for Tangled Rope are present.
    domain_priors:requires_active_enforcement(hedonic_adaptation_baseline),
    narrative_ontology:constraint_beneficiary(hedonic_adaptation_baseline, _),
    narrative_ontology:constraint_victim(hedonic_adaptation_baseline, _).

:- end_tests(hedonic_adaptation_baseline_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The Suppression Score is 1.0 because there is no "exit" from the human
 * condition. The Perspectival Gap is extreme: Individuals experience it as a
 * "Mountain" of futility, institutions exploit it as a "Snare" for profit,
 * and a holistic analysis reveals a "Tangled Rope."
 *
 * MANDATROPHY ANALYSIS:
 * This is a "Natural Snare" that has been weaponized. The extraction is the
 * loss of psychic energy to a biological set point, a loss that is captured
 * and monetized. The Tangled Rope classification is critical because it
 * acknowledges the constraint's dual nature: it has a genuine, evolutionarily
 * beneficial coordination function (resilience) that prevents it from being a
 * pure Snare, but this function is now coupled with severe asymmetric extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_neuroplastic_drift,
    'Can the hedonic baseline itself be permanently shifted through non-chemical intervention?',
    'Longitudinal study of high-compliance meditative or stoic cohorts.',
    'If yes, the Mountain aspect becomes a Scaffold (a temporary limit that can be overcome). If no, it remains a Mountain.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Interval represents the rise of the digital attention economy.
narrative_ontology:interval(hedonic_adaptation_baseline, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% This constraint is ancient, but its exploitation is recent. The measurements
% model the increasing extractiveness as technology learned to capture the
% energy of the hedonic treadmill. Theater remains low as the underlying
% mechanism is biological, not performative.

% Theater ratio over time (consistently low):
narrative_ontology:measurement(hab_tr_t0, hedonic_adaptation_baseline, theater_ratio, 0, 0.10).
narrative_ontology:measurement(hab_tr_t5, hedonic_adaptation_baseline, theater_ratio, 5, 0.10).
narrative_ontology:measurement(hab_tr_t10, hedonic_adaptation_baseline, theater_ratio, 10, 0.10).

% Extraction over time (increasing as attention economy rises):
narrative_ontology:measurement(hab_ex_t0, hedonic_adaptation_baseline, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(hab_ex_t5, hedonic_adaptation_baseline, base_extractiveness, 5, 0.35).
narrative_ontology:measurement(hab_ex_t10, hedonic_adaptation_baseline, base_extractiveness, 10, 0.48).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA (v5.0-5.2)
   ========================================================================== */

% The evolutionary function of resilience is a form of psychic resource management.
narrative_ontology:coordination_type(hedonic_adaptation_baseline, resource_allocation).

% The hedonic treadmill is a fundamental driver of engagement algorithms.
narrative_ontology:affects_constraint(hedonic_adaptation_baseline, social_media_engagement_algorithms).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */