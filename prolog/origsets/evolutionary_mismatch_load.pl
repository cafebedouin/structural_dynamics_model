% ============================================================================
% CONSTRAINT STORY: evolutionary_mismatch_load
% ============================================================================
% Version: 3.4 (Deferential Realism Core)
% Logic: 3.3 (Indexed Tuple P,T,E,S)
% Generated: 2026-01-28
% ============================================================================

:- module(evolutionary_mismatch_load, []).

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
 * * constraint_id: evolutionary_mismatch_load
 * human_readable: The Paleolithic Circuit Break
 * domain: biological/technological/social
 * * SUMMARY:
 * This constraint represents the friction caused by human biological traits 
 * that were adaptive in an ancestral environment but have become maladaptive 
 * in a modern, hyper-mediated technological landscape. This mismatch functions 
 * as a Snare for the individual, whose biology is "hijacked" by novel stimuli, 
 * while serving as a Rope for institutions that coordinate behavior by 
 * optimizing for these deep-seated evolutionary drives.
 * * KEY AGENTS:
 * - Modern Human: Subject (Powerless)
 * - Algorithmic Curator: Beneficiary (Institutional)
 * - Evolutionary Psychologist: Auditor (Analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% High extraction (0.83) as the mismatch siphons biological health, attention, 
% and cognitive agency to maintain high-frequency engagement loops.
domain_priors:base_extractiveness(evolutionary_mismatch_load, 0.83). 
domain_priors:suppression_score(evolutionary_mismatch_load, 0.70). 
domain_priors:theater_ratio(evolutionary_mismatch_load, 0.35). % Low theater; the drives are raw and functional.

% Constraint metric facts (bridge for classification engine)
narrative_ontology:constraint_metric(evolutionary_mismatch_load, extractiveness, 0.83).
narrative_ontology:constraint_metric(evolutionary_mismatch_load, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(evolutionary_mismatch_load, theater_ratio, 0.35).

% This is an irreducible biological reality, not a temporary scaffold.
% narrative_ontology:has_sunset_clause(evolutionary_mismatch_load). 

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE SUBJECT (SNARE)
% The individual is trapped in their own biology; they cannot "opt out" of 
% how their dopamine or cortisol circuits respond to modern stimuli.
constraint_indexing:constraint_classification(evolutionary_mismatch_load, snare, 
    context(agent_power(powerless), 
            time_horizon(biographical), 
            exit_options(trapped), 
            spatial_scope(national))).

% PERSPECTIVE 2: THE BENEFICIARY (ROPE)
% Platforms view these biological "hooks" as a Rope—a way to coordinate 
% billions of agents into predictable, high-fidelity engagement patterns.
constraint_indexing:constraint_classification(evolutionary_mismatch_load, rope, 
    context(agent_power(institutional), 
            time_horizon(generational), 
            exit_options(mobile), 
            spatial_scope(global))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (MOUNTAIN)
% From a genetic standpoint, the mismatch is an irreducible Mountain; 
% biological evolution cannot keep pace with technological acceleration.
constraint_indexing:constraint_classification(evolutionary_mismatch_load, mountain, 
    context(agent_power(analytical), 
            time_horizon(historical), 
            exit_options(analytical), 
            spatial_scope(global))).

% PERSPECTIVE 4: THE SYSTEMS AUDITOR (TANGLED ROPE)
% High extraction (0.83) triggers the hybrid Tangled Rope signature at the 
% civilizational scale.
constraint_indexing:constraint_classification(evolutionary_mismatch_load, tangled_rope, 
    context(agent_power(analytical), 
            time_horizon(civilizational), 
            exit_options(arbitrage), 
            spatial_scope(universal))) :-
    domain_priors:base_extractiveness(evolutionary_mismatch_load, E), E >= 0.50,
    domain_priors:suppression_score(evolutionary_mismatch_load, S), S > 0.40.

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(evolutionary_mismatch_load_tests).

test(perspectival_gap) :-
    % Verify Snare for the powerless human vs Rope for the institutional platform.
    constraint_indexing:constraint_classification(evolutionary_mismatch_load, snare, 
        context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(evolutionary_mismatch_load, rope, 
        context(agent_power(institutional), _, _, _)).

test(extraction_mandatrophy) :-
    % Ensure extraction (0.83) triggers mandatory v3.4 resolution logic.
    domain_priors:base_extractiveness(evolutionary_mismatch_load, E),

    E > 0.70.

:- end_tests(evolutionary_mismatch_load_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The extraction score (0.83) reflects a "Mandatrophy" state where the 
 * species' biological foundations are being liquidated to drive digital metrics.
 * 
 * * PERSPECTIVAL GAP:
 * The Modern Human feels a Snare because their instincts—once essential for 
 * survival—now lead them toward health and focus depletion. The 
 * Algorithmic Curator sees a Rope because these instincts provide the 
 * most reliable coordination signal available.
 * * [RESOLVED MANDATROPHY]:
 * Resolved via the Tangled Rope classification. This recognizes that 
 * institutional coordination is effectively built on the "tangle" of 
 * ancestral drives and modern extraction, where 0.83 of the surplus is 
 * captured from the subject.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% Required for high-extraction constraints (> 0.46).
omega_variable(
    omega_cognitive_evolution,
    'Can neuroplasticity or cultural evolution mitigate the load (Snare vs Mountain)?',
    'Longitudinal studies of "digital native" cohorts compared to legacy cohorts.',
    'If load decreases: Snare of policy/tech. If load remains: Mountain of biology.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for structural_linter.py.
narrative_ontology:interval(evolutionary_mismatch_load, 0, 10). 

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
