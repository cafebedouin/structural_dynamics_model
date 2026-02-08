% ============================================================================
% CONSTRAINT STORY: reputational_cascade_failure
% ============================================================================
% Version: 3.4 (Deferential Realism Core)
% Logic: 3.3 (Indexed Tuple P,T,E,S)
% Generated: 2026-01-28
% ============================================================================

:- module(reputational_cascade_failure, []).

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
 * * constraint_id: reputational_cascade_failure
 * human_readable: The Social Liquidity Trap
 * domain: social/economic/informational
 * * SUMMARY:
 * A scenario where a single piece of unverified but high-fitness negative 
 * information triggers a self-reinforcing loop of social disinvestment. 
 * This "Rope" for rapid group coordination and accountability becomes a 
 * "Snare" for the individual, whose entire life-surplus (economic access, 
 * social ties) is liquidated in seconds by an automated network reaction, 
 * trapping them in a state of permanent exclusion with no path for 
 * informational recovery or due process.
 * * KEY AGENTS:
 * - Targeted Individual: Subject (Powerless)
 * - Social Credit/Platform System: Beneficiary (Institutional)
 * - Network Dynamics Analyst: Auditor (Analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% High extraction (0.91) reflects the near-total liquidation of the subject's 
% social and economic agency by the network.
domain_priors:base_extractiveness(reputational_cascade_failure, 0.91). 
domain_priors:suppression_score(reputational_cascade_failure, 0.82). % Counter-evidence is suppressed by the velocity of the cascade.
domain_priors:theater_ratio(reputational_cascade_failure, 0.87).    % High theater: "Community Guidelines" and "Safety Checks" masking raw algorithmic enforcement.

% Constraint metric facts (bridge for classification engine)
narrative_ontology:constraint_metric(reputational_cascade_failure, extractiveness, 0.91).
narrative_ontology:constraint_metric(reputational_cascade_failure, suppression_requirement, 0.82).
narrative_ontology:constraint_metric(reputational_cascade_failure, theater_ratio, 0.87).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE SUBJECT (SNARE)
% The individual is trapped: the cascade moves faster than legal or 
% logical intervention, liquidating their ability to exist within the system.
constraint_indexing:constraint_classification(reputational_cascade_failure, snare, 
    context(agent_power(powerless), 
            time_horizon(biographical), 
            exit_options(trapped), 
            spatial_scope(national))).

% PERSPECTIVE 2: THE BENEFICIARY (ROPE)
% The platform/institution views the cascade as a Rope—the ultimate 
% coordination tool for enforcing "consensus" and maintaining 
% high-velocity social order.
constraint_indexing:constraint_classification(reputational_cascade_failure, rope, 
    context(agent_power(institutional), 
            time_horizon(generational), 
            exit_options(mobile), 
            spatial_scope(global))).

% PERSPECTIVE 3: THE SYSTEMS AUDITOR (PITON)
% Theater ratio (0.87) > 0.70 triggers Piton: the "Appeals Process" 
% is an inertial spike; it remains as an optic but lacks functional authority.
constraint_indexing:constraint_classification(reputational_cascade_failure, piton, 
    context(agent_power(analytical), 
            time_horizon(historical), 
            exit_options(analytical), 
            spatial_scope(global))).

% PERSPECTIVE 4: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% Detects extreme extraction (0.91) masking as coordination (Rope).
constraint_indexing:constraint_classification(reputational_cascade_failure, tangled_rope, 
    context(agent_power(analytical), 
            time_horizon(civilizational), 
            exit_options(arbitrage), 
            spatial_scope(universal))) :-
    domain_priors:base_extractiveness(reputational_cascade_failure, E), E >= 0.50,
    domain_priors:suppression_score(reputational_cascade_failure, S), S > 0.40.

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(reputational_cascade_tests).

test(perspectival_gap) :-
    % Verify Snare for the powerless subject vs Rope for the institutional platform.
    constraint_indexing:constraint_classification(reputational_cascade_failure, snare, 
        context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(reputational_cascade_failure, rope, 
        context(agent_power(institutional), _, _, _)).

test(piton_trigger) :-
    % Ensure high theater ratio (0.87) correctly triggers the Piton classification.
    constraint_indexing:constraint_classification(reputational_cascade_failure, piton, 
        context(agent_power(analytical), _, _, _)).

test(extraction_threshold) :-
    % Extraction (0.91) > 0.70 triggers [RESOLVED MANDATROPHY].
    domain_priors:base_extractiveness(reputational_cascade_failure, E),

    E > 0.70.

:- end_tests(reputational_cascade_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The extraction score (0.91) reflects a "Mandatrophy" state where the 
 * "coordination" benefit of social accountability is achieved by 
 * liquidating the subject's entire life-territory.
 *
 * 
 *
 * * PERSPECTIVAL GAP:
 * The Targeted Individual feels a Snare because the network consumes them 
 * based on a "hallucination" of consensus. The Platform sees a Rope because 
 * the cascade coordinates massive behavioral alignment at zero cost.
 *
 * * [RESOLVED MANDATROPHY]:
 * Resolved via the Piton and Tangled Rope classifications. For an analytical 
 * observer, the "Appeal" is no longer functional (Theater 0.87); it is 
 * an inert spike siphoning 0.91 of the subject's agency.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% Required for high-extraction constraints (> 0.46).
omega_variable(
    omega_cascade_reversal,
    'Can a reputation be "un-liquidated" once the network commits, or is the Snare terminal (Snare vs Mountain)?',
    'Tracking the success rate of public exoneration in restoring economic access scores.',
    'If recovery fails: Mountain of Permanent Stigma. If recovery holds: Snare of current system design.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(reputational_cascade_failure, 0, 10). 

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
