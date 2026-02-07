% ============================================================================
% CONSTRAINT STORY: epistemic_free_rider_problem
% ============================================================================
% Version: 3.4 (Deferential Realism Core)
% Logic: 3.3 (Indexed Tuple P,T,E,S)
% Generated: 2026-01-28
% ============================================================================

:- module(epistemic_free_rider_problem, []).

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
 * * constraint_id: epistemic_free_rider_problem
 * human_readable: The Truth-Mining Exhaustion
 * domain: informational/social/economic
 * * SUMMARY:
 * A scenario where the cost of producing verified, grounded information is borne 
 * by a shrinking pool of "truth-miners," while the majority of the population 
 * consumes low-cost, unverified synthetic derivatives. This "Rope" for 
 * massive informational scaling becomes a "Snare" for the original producers, 
 * as the market value of grounded truth is liquidated by a flood of free-riding 
 * synthetic mimics, leading to the collapse of the epistemic infrastructure.
 * * KEY AGENTS:
 * - Investigative Journalist: Subject (Powerless)
 * - Synthetic Content Farm: Beneficiary (Institutional)
 * - Information Market Auditor: Auditor (Analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% High extraction (0.87) reflects the parasitic liquidation of the producer's 
% investigative surplus by actors who do not contribute to verification costs.
domain_priors:base_extractiveness(epistemic_free_rider_problem, 0.87). 
domain_priors:suppression_score(epistemic_free_rider_problem, 0.74). 
domain_priors:theater_ratio(epistemic_free_rider_problem, 0.83). % High theater: "Verification badges" that do not track primary source verification costs.

% Constraint metric facts (bridge for classification engine)
narrative_ontology:constraint_metric(epistemic_free_rider_problem, extractiveness, 0.87).
narrative_ontology:constraint_metric(epistemic_free_rider_problem, suppression_requirement, 0.74).
narrative_ontology:constraint_metric(epistemic_free_rider_problem, theater_ratio, 0.83).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE SUBJECT (SNARE)
% The journalist is trapped: they produce the "ground truth" that synthetic 
% models use to stay relevant, but they receive zero return, liquidating their agency.
constraint_indexing:constraint_classification(epistemic_free_rider_problem, snare, 
    context(agent_power(powerless), 
            time_horizon(biographical), 
            exit_options(trapped), 
            spatial_scope(national))).

% PERSPECTIVE 2: THE BENEFICIARY (ROPE)
% The content farm views the free-riding as a Rope—the only way to coordinate 
% the infinite supply of "content" required to satisfy global attention demand.
constraint_indexing:constraint_classification(epistemic_free_rider_problem, rope, 
    context(agent_power(institutional), 
            time_horizon(generational), 
            exit_options(mobile), 
            spatial_scope(global))).

% PERSPECTIVE 3: THE SYSTEMS AUDITOR (PITON)
% Theater ratio (0.83) > 0.70 triggers Piton: the "News Aggregator" 
% is an inertial spike; it performatively signals "News" while hollowing the producers.
constraint_indexing:constraint_classification(epistemic_free_rider_problem, piton, 
    context(agent_power(analytical), 
            time_horizon(historical), 
            exit_options(analytical), 
            spatial_scope(global))).

% PERSPECTIVE 4: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% Detects high extraction (0.87) masking as essential coordination (Rope).
constraint_indexing:constraint_classification(epistemic_free_rider_problem, tangled_rope, 
    context(agent_power(analytical), 
            time_horizon(civilizational), 
            exit_options(arbitrage), 
            spatial_scope(universal))) :-
    domain_priors:base_extractiveness(epistemic_free_rider_problem, E), E >= 0.50,
    domain_priors:suppression_score(epistemic_free_rider_problem, S), S > 0.40.

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(epistemic_free_rider_tests).

test(perspectival_gap) :-
    % Verify Snare for the powerless producer vs Rope for the institutional free-rider.
    constraint_indexing:constraint_classification(epistemic_free_rider_problem, snare, 
        context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(epistemic_free_rider_problem, rope, 
        context(agent_power(institutional), _, _, _)).

test(piton_trigger) :-
    % Ensure high theater ratio (0.83) correctly triggers the Piton classification.
    constraint_indexing:constraint_classification(epistemic_free_rider_problem, piton, 
        context(agent_power(analytical), _, _, _)).

test(extraction_threshold) :-
    % High extraction (> 0.70) requires [RESOLVED MANDATROPHY] hook.
    domain_priors:base_extractiveness(epistemic_free_rider_problem, E),

    E > 0.70.

:- end_tests(epistemic_free_rider_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The extraction score (0.87) reflects a "Mandatrophy" state where the 
 * "coordination" of global information is achieved by liquidating the 
 * viability of primary verification.
 * 
 * * PERSPECTIVAL GAP:
 * The Investigative Journalist feels a Snare because their labor is 
 * stolen by the very models that make their career obsolete. The Farm 
 * sees a Rope because the free-riding coordinates the distribution 
 * of knowledge to billions who cannot afford high-cost verification.
 * * [RESOLVED MANDATROPHY]:
 * Resolved via the Piton and Tangled Rope classifications. For an analytical 
 * observer, the "Information Market" is no longer functional (Theater 0.83); 
 * it is an inert spike siphoning 0.87 of the subject's epistemic agency.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% Required for high-extraction constraints (> 0.46).
omega_variable(
    omega_epistemic_exhaustion,
    'When the last human "miner" stops, does the synthetic model collapse or reach a Mountain of hallucination (Snare vs Mountain)?',
    'Tracking the perplexity of synthetic models after a complete primary-source blackout.',
    'If models explode: Snare of current extraction. If models stabilize: Mountain of synthetic intelligence.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(epistemic_free_rider_problem, 0, 10). 

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
