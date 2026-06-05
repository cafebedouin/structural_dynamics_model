% ============================================================================
% CONSTRAINT STORY: memetic_fitness_vs_truth
% ============================================================================
% Version: 3.4 (Deferential Realism Core)
% Logic: 3.3 (Indexed Tuple P,T,E,S)
% Generated: 2026-01-28
% ============================================================================

:- module(memetic_fitness_vs_truth, []).

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
    constraint_indexing:constraint_classification/3.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * * constraint_id: memetic_fitness_vs_truth
 * human_readable: The Viral Distortion
 * domain: social/informational/biological
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
 * - Platform Algorithm: Beneficiary (Institutional)
 * - Epistemic Ecologist: Auditor (Analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% High extraction (0.86) reflects the siphoning of the subject's attention and 
% cognitive labor into the replication of non-functional, high-virality noise.
domain_priors:base_extractiveness(memetic_fitness_vs_truth, 0.86). 
domain_priors:suppression_score(memetic_fitness_vs_truth, 0.75). % Low-fitness "truth" is suppressed by the sheer volume of high-fitness "noise."
domain_priors:theater_ratio(memetic_fitness_vs_truth, 0.88).    % High theater: "Verification" symbols that only track social consensus, not truth.

% Constraint metric facts (bridge for classification engine)
narrative_ontology:constraint_metric(memetic_fitness_vs_truth, extractiveness, 0.86).
narrative_ontology:constraint_metric(memetic_fitness_vs_truth, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(memetic_fitness_vs_truth, theater_ratio, 0.88).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
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
            spatial_scope(global))).

% PERSPECTIVE 4: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% Detects high extraction (0.86) masking as essential coordination (Rope).
constraint_indexing:constraint_classification(memetic_fitness_vs_truth, tangled_rope, 
    context(agent_power(analytical), 
            time_horizon(civilizational), 
            exit_options(arbitrage), 
            spatial_scope(universal))) :-
    domain_priors:base_extractiveness(memetic_fitness_vs_truth, E), E >= 0.50,
    domain_priors:suppression_score(memetic_fitness_vs_truth, S), S > 0.40.

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(memetic_fitness_tests).

test(perspectival_gap) :-
    % Verify Snare for the powerless consumer vs Rope for the institutional platform.
    constraint_indexing:constraint_classification(memetic_fitness_vs_truth, snare, 
        context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(memetic_fitness_vs_truth, rope, 
        context(agent_power(institutional), _, _, _)).

test(piton_trigger) :-
    % Ensure high theater ratio (0.88) correctly triggers the Piton classification.
    constraint_indexing:constraint_classification(memetic_fitness_vs_truth, piton, 
        context(agent_power(analytical), _, _, _)).

test(extraction_threshold) :-
    % High extraction (> 0.70) requires [RESOLVED MANDATROPHY] hook.
    domain_priors:base_extractiveness(memetic_fitness_vs_truth, E),

    E > 0.70.

:- end_tests(memetic_fitness_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The extraction score (0.86) reflects a "Mandatrophy" state where the 
 * "coordination" benefit of shared narratives is achieved by liquidating the 
 * cognitive integrity of the subject population.
 * 
 * * PERSPECTIVAL GAP:
 * The Information Consumer feels a Snare because their worldview is 
 * weaponized to maintain platform dwell-time. The Platform sees a Rope 
 * because memetic fitness is the only metric that can coordinate 
 * billions of attention-slices in real-time.
 * * [RESOLVED MANDATROPHY]:
 * Resolved via the Piton and Tangled Rope classifications. For an analytical 
 * observer, "Viral Truth" is no longer functional (Theater 0.88); 
 * it is an inert spike siphoning 0.86 of the species' collective agency.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% Required for high-extraction constraints (> 0.46).
omega_variable(
    omega_memetic_dominance,
    'Can a truth-signal ever out-compete a high-fitness delusion, or is truth a biological "Snare" (Snare vs Mountain)?',
    'Tracking the half-life of fact-checked corrections vs the original viral error.',
    'If corrections fail to penetrate: Mountain of Human Psychology. If they hold: Snare of current design.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(memetic_fitness_vs_truth, 0, 10). 

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
