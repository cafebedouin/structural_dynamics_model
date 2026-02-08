% ============================================================================
% CONSTRAINT STORY: soft_authoritarian_drift
% ============================================================================
% Version: 3.4 (Deferential Realism Core)
% Logic: 3.3 (Indexed Tuple P,T,E,S)
% Generated: 2026-01-28
% ============================================================================

:- module(soft_authoritarian_drift, []).

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
 * * constraint_id: soft_authoritarian_drift
 * human_readable: The Incremental Compliance Web
 * domain: political/social/technological
 * * SUMMARY:
 * A scenario where a democratic or open system incrementally adopts restrictive 
 * measures—often under the guise of safety, efficiency, or "community standards"—
 * that gradually erode individual autonomy. This "Rope" for social 
 * coordination and risk mitigation becomes a "Snare" for the subject, as the 
 * cumulative weight of "soft" nudges and administrative requirements 
 * liquidates the practical possibility of dissent or exit.
 * * KEY AGENTS:
 * - Average Citizen: Subject (Powerless)
 * - Administrative Architect: Beneficiary (Institutional)
 * - Constitutional Systems Auditor: Auditor (Analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% High extraction (0.84) as the system siphons the subject's behavioral 
% optionality into the maintenance of a frictionless, managed equilibrium.
domain_priors:base_extractiveness(soft_authoritarian_drift, 0.84). 
domain_priors:suppression_score(soft_authoritarian_drift, 0.72). 
domain_priors:theater_ratio(soft_authoritarian_drift, 0.89). % High theater: maintaining democratic terminology while hollowing the substance.

% Constraint metric facts (bridge for classification engine)
narrative_ontology:constraint_metric(soft_authoritarian_drift, extractiveness, 0.84).
narrative_ontology:constraint_metric(soft_authoritarian_drift, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(soft_authoritarian_drift, theater_ratio, 0.89).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE SUBJECT (SNARE)
% For the citizen, the drift is a snare: each individual rule seems 
% reasonable, but the total set traps them in a high-compliance, low-agency state.
constraint_indexing:constraint_classification(soft_authoritarian_drift, snare, 
    context(agent_power(powerless), 
            time_horizon(biographical), 
            exit_options(trapped), 
            spatial_scope(national))).

% PERSPECTIVE 2: THE BENEFICIARY (ROPE)
% The architect views the drift as a Rope—the essential coordination of 
% modern, complex societies required to prevent "harmful" volatility.
constraint_indexing:constraint_classification(soft_authoritarian_drift, rope, 
    context(agent_power(institutional), 
            time_horizon(generational), 
            exit_options(mobile), 
            spatial_scope(global))).

% PERSPECTIVE 3: THE SYSTEMS AUDITOR (PITON)
% Theater ratio (0.89) > 0.70 triggers Piton: the "Democratic Processes" 
% are an inertial spike—they exist in form but no longer steer the outcome.
constraint_indexing:constraint_classification(soft_authoritarian_drift, piton, 
    context(agent_power(analytical), 
            time_horizon(historical), 
            exit_options(analytical), 
            spatial_scope(global))).

% PERSPECTIVE 4: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% Detects high extraction (0.84) and high suppression (0.72) as a hybrid 
% Tangled Rope, where coordination intent masks systemic hollowing.
constraint_indexing:constraint_classification(soft_authoritarian_drift, tangled_rope, 
    context(agent_power(analytical), 
            time_horizon(civilizational), 
            exit_options(arbitrage), 
            spatial_scope(universal))) :-
    domain_priors:base_extractiveness(soft_authoritarian_drift, E), E >= 0.50,
    domain_priors:suppression_score(soft_authoritarian_drift, S), S > 0.40.

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(soft_authoritarian_drift_tests).

test(perspectival_gap) :-
    % Verify Snare for the subject vs Rope for the institutional architect.
    constraint_indexing:constraint_classification(soft_authoritarian_drift, snare, 
        context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(soft_authoritarian_drift, rope, 
        context(agent_power(institutional), _, _, _)).

test(piton_trigger) :-
    % Ensure high theater ratio (0.89) correctly triggers the Piton classification.
    constraint_indexing:constraint_classification(soft_authoritarian_drift, piton, 
        context(agent_power(analytical), _, _, _)).

test(extraction_mandatrophy) :-
    % Ensure high extraction (0.84) triggers mandatory v3.4 resolution logic.
    domain_priors:base_extractiveness(soft_authoritarian_drift, E),

    E > 0.70.

:- end_tests(soft_authoritarian_drift_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The extraction score (0.84) reflects a "Mandatrophy" state where the 
 * "coordination" benefit of societal safety is effectively paid for by 
 * the liquidation of the subject's primary political agency.
 
 * * PERSPECTIVAL GAP:
 * The Citizen feels a Snare because their "freedom" is now restricted to 
 * choices within a pre-approved administrative range. The Architect sees 
 * a Rope because the drift ensures the coordination of predictable social 
 * outcomes and minimizes the "noise" of genuine dissent.
 * * [RESOLVED MANDATROPHY]:
 * Resolved via the Piton and Tangled Rope classifications. For an analytical 
 * observer, the "democratic framework" is no longer functional relative 
 * to its charter (Theater 0.89); it is an inert spike siphoning 0.84 of 
 * the subject's agency.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% Required for high-extraction constraints (> 0.46).
omega_variable(
    omega_democratic_resilience,
    'Can the drift be reversed by internal reform, or is the institutional inertia final (Snare vs Mountain)?',
    'Tracking the success rate of legislative reversals of "emergency-origin" compliance mandates.',
    'If reversal occurs: Snare of current policy. If reversal fails: Mountain of Institutional Sclerosis.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for structural_linter.py.
narrative_ontology:interval(soft_authoritarian_drift, 0, 10). 

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
