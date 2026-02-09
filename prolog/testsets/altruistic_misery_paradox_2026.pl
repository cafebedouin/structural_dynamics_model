% ============================================================================
% CONSTRAINT STORY: altruistic_misery_paradox_2026
% ============================================================================
% Version: 3.4 (Deferential Realism Core)
% Logic: 3.3 (Indexed Tuple P,T,E,S)
% Generated: 2026-02-05
% ============================================================================

:- module(constraint_altruistic_misery, []).

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
    narrative_ontology:interval/3,
    narrative_ontology:measurement/5,
    narrative_ontology:constraint_claim/2,
    narrative_ontology:constraint_beneficiary/2,
    narrative_ontology:constraint_victim/2,
    constraint_indexing:constraint_classification/3.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * * constraint_id: altruistic_misery_paradox_2026
 * human_readable: The Paradox of Altruistic Misery
 * domain: social/psychological
 * * SUMMARY:
 * This constraint maps the common social pressure to prioritize the happiness of 
 * others through self-sacrifice. It identifies the "Snare" of performative 
 * altruism, where a subject's unhappiness extracts value from their loved ones, 
 * whereas self-actualized happiness functions as a "Rope" for social stability.
 * * KEY AGENTS:
 * - [The Self-Sacrificing Individual]: Subject (Powerless) - Trapped in the 
 * "duty" of prioritizing others at the cost of self-actualization.
 * - [Loved Ones/Social Circle]: Beneficiary (Institutional) - Require the 
 * subject's happiness as a foundation for their own coordination.
 * - [The Moral Psychologist]: Auditor (Analytical) - Detects the theater of 
 * "unhappy service."
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% High extraction (0.62) because chronic unhappiness extracts emotional 
% and cognitive resources from the social unit.
domain_priors:base_extractiveness(altruistic_misery_paradox_2026, 0.62). 

% Suppression (0.68) of the "self-happiness" option due to social norms 
% regarding altruism.
domain_priors:suppression_score(altruistic_misery_paradox_2026, 0.68).   

% High theater ratio (0.75); the "service" of the unhappy is often a 
% performative mask that fails to deliver genuine coordination.
domain_priors:theater_ratio(altruistic_misery_paradox_2026, 0.75).       

% Constraint metric facts (bridge for classification engine)
narrative_ontology:constraint_metric(altruistic_misery_paradox_2026, extractiveness, 0.62).
narrative_ontology:constraint_metric(altruistic_misery_paradox_2026, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(altruistic_misery_paradox_2026, theater_ratio, 0.75).

% Constraint classification claim
narrative_ontology:constraint_claim(altruistic_misery_paradox_2026, piton).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE SUBJECT (SNARE)
% For the individual, the duty to "think of others" while being miserable is 
% a snare: a trap of performative virtue with no biographical exit.
constraint_indexing:constraint_classification(altruistic_misery_paradox_2026, snare, 
    context(agent_power(powerless), 
            time_horizon(biographical), 
            exit_options(trapped), 
            spatial_scope(local))).

% PERSPECTIVE 2: THE BENEFICIARY (ROPE)
% For those who love the individual, the individual's happiness is a rope: 
% it provides the psychological "rails" for a functional relationship.
constraint_indexing:constraint_classification(altruistic_misery_paradox_2026, rope, 
    context(agent_power(institutional), 
            time_horizon(generational), 
            exit_options(mobile), 
            spatial_scope(national))).

% PERSPECTIVE 3: THE SYSTEMS AUDITOR (PITON)
% The Auditor identifies a Piton: an inertial social norm ("sacrifice equals 
% love") that is no longer functional but persists through theater.
constraint_indexing:constraint_classification(altruistic_misery_paradox_2026, piton, 
    context(agent_power(analytical), 
            time_horizon(historical), 
            exit_options(arbitrage), 
            spatial_scope(global))) :-
    domain_priors:theater_ratio(altruistic_misery_paradox_2026, TR), TR > 0.70.
domain_priors:requires_active_enforcement(altruistic_misery_paradox_2026).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(altruistic_misery_tests).

test(perspectival_gap) :-
    % Subject feels a Snare; Institutional beneficiary needs a Rope.
    constraint_indexing:constraint_classification(altruistic_misery_paradox_2026, snare, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(altruistic_misery_paradox_2026, rope, context(agent_power(institutional), _, _, _)).

test(piton_trigger) :-
    % Ensure high theater_ratio correctly triggers Piton classification.
    constraint_indexing:constraint_classification(altruistic_misery_paradox_2026, piton, _).

:- end_tests(altruistic_misery_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The extraction score (0.62) represents the opportunity cost of misspent 
 * emotional labor. The Perspectival Gap exists because while the subject views 
 * their self-sacrifice as a "Snare" (mandatory duty), their loved ones view 
 * the subject's personal happiness as a "Rope" (the coordination required to 
 * love them).
 *
 * MANDATROPHY ANALYSIS:
 * The Piton classification prevents the system from mislabeling "misery-driven 
 * altruism" as a functional Rope for social cohesion.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    omega_emotional_contagion,
    'Is the unhappiness of the altruist a direct extraction from the observer?',
    'Empirical study of emotional contagion in high-duty vs. high-agency cohorts.',
    'If contagion is high, altruistic misery is a Snare for all involved.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(altruistic_misery_paradox_2026, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Model the drift from "Genuine Care" (T=0) to "Performative Sacrifice" (T=10).

% Theater ratio: Increases as the effort to "be happy for others" 
% becomes a theatrical performance rather than a functional state.
narrative_ontology:measurement(am_tr_t0, altruistic_misery_paradox_2026, theater_ratio, 0, 0.30).
narrative_ontology:measurement(am_tr_t5, altruistic_misery_paradox_2026, theater_ratio, 5, 0.55).
narrative_ontology:measurement(am_tr_t10, altruistic_misery_paradox_2026, theater_ratio, 10, 0.75).

% Extraction: Increases as the individual's lack of self-actualization 
% extracts more care-effort from their loved ones over time.
narrative_ontology:measurement(am_ex_t0, altruistic_misery_paradox_2026, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(am_ex_t5, altruistic_misery_paradox_2026, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(am_ex_t10, altruistic_misery_paradox_2026, base_extractiveness, 10, 0.62).

/* ==========================================================================
   9. STRUCTURAL ENRICHMENT (BENEFICIARY / VICTIM)
   ========================================================================== */

% Piton enrichment: vestigial extraction identified from narrative context.
% The social circle requires the subject's happiness as coordination infrastructure,
% while the self-sacrificing individual is trapped in performative altruism.
narrative_ontology:constraint_beneficiary(altruistic_misery_paradox_2026, social_circle).
narrative_ontology:constraint_victim(altruistic_misery_paradox_2026, self_sacrificing_individual).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
