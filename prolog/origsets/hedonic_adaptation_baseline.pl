% ============================================================================
% CONSTRAINT STORY: hedonic_adaptation_baseline
% ============================================================================
% Version: 3.4 (Deferential Realism Core)
% Logic: 3.3 (Indexed Tuple P,T,E,S)
% Generated: 2026-01-28
% ============================================================================

:- module(constraint_adaptation_response, []).

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
    narrative_ontology:constraint_metric/3,
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
 * Adaptation Response is the "biological gravity" of the psyche. It ensures 
 * that no matter the change in environment (wealth, status, or tragedy), 
 * the subject eventually returns to a set point. It acts as a Mountain (fixed 
 * biology) that creates a Snare (the need for constant novelty/input to 
 * maintain a high).
 * * KEY AGENTS:
 * - The Aspirant: Subject (Powerless). Chasing the next "high" or plateau.
 * - The Attention Economy: Beneficiary (Institutional). Captures the "treadmill" energy.
 * - The Evolutionary Biologist: Auditor (Analytical). Observes the survival utility.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% [RESOLVED MANDATROPHY]: Extraction (0.48) is not "stolen" by a person but 
% "drained" by the biological mechanism itself, which forces continuous effort 
% for diminishing returns.
domain_priors:base_extractiveness(hedonic_adaptation_baseline, 0.48). 
domain_priors:suppression_score(hedonic_adaptation_baseline, 1.00).   % Total: You cannot "exit" your own neurobiology.
domain_priors:theater_ratio(hedonic_adaptation_baseline, 0.10).      % Low: This is a raw physical limit, not a performance.

% Constraint metric facts (bridge for classification engine)
narrative_ontology:constraint_metric(hedonic_adaptation_baseline, extractiveness, 0.48).
narrative_ontology:constraint_metric(hedonic_adaptation_baseline, suppression_requirement, 1.0).
narrative_ontology:constraint_metric(hedonic_adaptation_baseline, theater_ratio, 0.1).
domain_priors:requires_active_enforcement(hedonic_adaptation_baseline). % Internalized enforcement via dopamine regulation.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE ASPIRANT (MOUNTAIN)
% At the biographical level, the return to baseline feels like an 
% unchangeable law of nature. You cannot "win" the game of permanent bliss.
constraint_indexing:constraint_classification(hedonic_adaptation_baseline, mountain, 
    context(agent_power(powerless), 
            time_horizon(biographical), 
            exit_options(trapped), 
            spatial_scope(local))).

% PERSPECTIVE 2: THE CONSUMER TECH FIRM (SNARE)
% From an institutional view, this is a Snare used to ensure 
% "Retention." Since users adapt to features, new ones must be fed to them.
constraint_indexing:constraint_classification(hedonic_adaptation_baseline, snare, 
    context(agent_power(institutional), 
            time_horizon(immediate), 
            exit_options(mobile), 
            spatial_scope(global))).

% PERSPECTIVE 3: THE EVOLUTIONARY AUDITOR (ROPE)
% Viewed historically, adaptation is a Rope; it allows humans to 
% coordinate and survive in extreme environments without being 
% permanently paralyzed by trauma or euphoria.
constraint_indexing:constraint_classification(hedonic_adaptation_baseline, rope, 
    context(agent_power(analytical), 
            time_horizon(historical), 
            exit_options(analytical), 
            spatial_scope(universal))).

% PERSPECTIVE 4: THE NEURO-MODERATOR (PITON)
% In a world of ubiquitous pharmacological intervention, the biological 
% baseline becomes a Piton—an inertial legacy constraint that humans 
% actively try to bypass but remains "theatrically" present in clinical models.
constraint_indexing:constraint_classification(hedonic_adaptation_baseline, piton, 
    context(agent_power(organized), 
            time_horizon(generational), 
            exit_options(arbitrage), 
            spatial_scope(global))) :-
    domain_priors:theater_ratio(hedonic_adaptation_baseline, TR), TR > 0.05.

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(adaptation_response_tests).

test(type_variance) :-
    % Verify the constraint is a Mountain for the individual but a Rope for the species.
    constraint_indexing:constraint_classification(hedonic_adaptation_baseline, mountain, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(hedonic_adaptation_baseline, rope, context(agent_power(analytical), _, _, _)).

test(threshold_validation) :-
    domain_priors:base_extractiveness(hedonic_adaptation_baseline, E),

    E >= 0.46. % Ensures high extraction signature for the "treadmill" effect.

:- end_tests(adaptation_response_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The Adaptation Response is unique because its "Suppression Score" is 1.0. 
 * There is no "exit" from the human condition. 
 * The Perspectival Gap is extreme: Individuals experience it as a "Mountain" 
 * of futility, whereas the species (Analytical/Historical) uses it as a "Rope" 
 * for resilience.
 *
 * MANDATROPHY ANALYSIS:
 * This is a "Natural Snare." The extraction is the loss of psychic energy 
 * to a biological set point. By identifying it as a Mountain for the powerless, 
 * we acknowledge that "willpower" is an insufficient resolution mechanism.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_neuroplastic_drift,
    'Can the baseline itself be permanently shifted through non-chemical intervention?',
    'Longitudinal study of high-compliance meditative or stoic cohorts.',
    'If yes, the Mountain becomes a Scaffold (Temporary Limit).',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hedonic_adaptation_baseline, 0, 100). % Scaled by intensity of stimulus.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
