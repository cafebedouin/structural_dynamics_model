% ============================================================================
% CONSTRAINT STORY: legitimacy_without_capacity
% ============================================================================
% Version: 3.4 (Deferential Realism Core)
% Logic: 3.3 (Indexed Tuple P,T,E,S)
% Generated: 2026-01-28
% ============================================================================

:- module(legitimacy_without_capacity, []).

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
 * * constraint_id: legitimacy_without_capacity
 * human_readable: The Sovereign Ghost
 * domain: political/organizational
 * * SUMMARY:
 * A scenario where an institution retains the social and legal "right to rule" 
 * (legitimacy) but has lost the actual ability to provide services, security, 
 * or order (capacity). This "Rope" of historical loyalty becomes a "Snare" 
 * as the subject is legally bound to a defunct provider, liquidating their 
 * safety while being barred from seeking alternative, non-state coordination.
 * * KEY AGENTS:
 * - Local Resident: Subject (Powerless)
 * - Legacy Bureaucrat: Beneficiary (Institutional)
 * - Institutional Resilience Auditor: Auditor (Analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% High extraction (0.84) because the institution continues to extract taxes 
% and compliance (loyalty) without returning the expected functional utility.
domain_priors:base_extractiveness(legitimacy_without_capacity, 0.84). 
domain_priors:suppression_score(legitimacy_without_capacity, 0.71). 
domain_priors:theater_ratio(legitimacy_without_capacity, 0.91). % Extreme theater: symbols of statehood masking operational void.

% Constraint metric facts (bridge for classification engine)
narrative_ontology:constraint_metric(legitimacy_without_capacity, extractiveness, 0.84).
narrative_ontology:constraint_metric(legitimacy_without_capacity, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(legitimacy_without_capacity, theater_ratio, 0.91).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE SUBJECT (SNARE)
% The subject is trapped: they are legally prohibited from forming 
% self-defense or service groups because the "State" still claims a monopoly.
constraint_indexing:constraint_classification(legitimacy_without_capacity, snare, 
    context(agent_power(powerless), 
            time_horizon(biographical), 
            exit_options(trapped), 
            spatial_scope(national))).

% PERSPECTIVE 2: THE BENEFICIARY (ROPE)
% The institution views its legitimacy as a Rope—the only coordination tool 
% remaining to prevent total societal fragmentation, despite current failure.
constraint_indexing:constraint_classification(legitimacy_without_capacity, rope, 
    context(agent_power(institutional), 
            time_horizon(generational), 
            exit_options(mobile), 
            spatial_scope(global))).

% PERSPECTIVE 3: THE SYSTEMS AUDITOR (PITON)
% Theater ratio (0.91) > 0.70 triggers Piton: the "Sovereignty" is a 
% non-functional, performative artifact maintained by cultural inertia.
constraint_indexing:constraint_classification(legitimacy_without_capacity, piton, 
    context(agent_power(analytical), 
            time_horizon(historical), 
            exit_options(analytical), 
            spatial_scope(global))).

% PERSPECTIVE 4: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% Detects high extraction (0.84) masking as coordination (Rope).
constraint_indexing:constraint_classification(legitimacy_without_capacity, tangled_rope, 
    context(agent_power(analytical), 
            time_horizon(civilizational), 
            exit_options(arbitrage), 
            spatial_scope(universal))) :-
    domain_priors:base_extractiveness(legitimacy_without_capacity, E), E >= 0.50,
    domain_priors:suppression_score(legitimacy_without_capacity, S), S > 0.40.

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(legitimacy_without_capacity_tests).

test(perspectival_gap) :-
    % Verify Snare for the subject vs Rope for the institutional beneficiary.
    constraint_indexing:constraint_classification(legitimacy_without_capacity, snare, 
        context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(legitimacy_without_capacity, rope, 
        context(agent_power(institutional), _, _, _)).

test(piton_trigger) :-
    % Ensure high theater ratio (0.91) triggers Piton classification.
    constraint_indexing:constraint_classification(legitimacy_without_capacity, piton, 
        context(agent_power(analytical), _, _, _)).

test(extraction_mandatrophy) :-
    % Ensure high extraction (0.84) triggers mandatory v3.4 resolution logic.
    domain_priors:base_extractiveness(legitimacy_without_capacity, E),

    E > 0.70.

:- end_tests(legitimacy_without_capacity_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The extraction score (0.84) reflects a "Mandatrophy" state where the 
 * "coordination" is a parasitic liquidation of the subject's remaining capital.
 
 * * PERSPECTIVAL GAP:
 * The Local Resident feels a Snare because they are taxed for security that 
 * never arrives. The Bureaucrat sees a Rope because the "myth" of the state 
 * is the only thing preventing an immediate descent into a Mountain of chaos.
 * * [RESOLVED MANDATROPHY]:
 * Resolved via the Piton and Tangled Rope classifications. For an analytical 
 * observer, the "State" is no longer functional (Theater 0.91); it is an 
 * inert spike siphoning 0.84 of the subject's agency to feed a legacy model.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% Required for high-extraction constraints (> 0.46).
omega_variable(
    omega_sovereign_reconstitution,
    'Can legitimacy be re-tethered to capacity, or is the divorce final (Snare vs Mountain)?',
    'Tracking the success rate of local "self-help" groups in regaining state recognition.',
    'If recognized: Snare of current policy. If crushed/ignored: Mountain of Institutional Death.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for structural_linter.py.
narrative_ontology:interval(legitimacy_without_capacity, 0, 10). 

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
