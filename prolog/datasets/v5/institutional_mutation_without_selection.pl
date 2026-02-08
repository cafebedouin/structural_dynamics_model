% ============================================================================
% CONSTRAINT STORY: institutional_mutation_without_selection
% ============================================================================
% Version: 3.4 (Deferential Realism Core)
% Logic: 3.3 (Indexed Tuple P,T,E,S)
% Generated: 2026-01-28
% ============================================================================

:- module(institutional_mutation_without_selection, []).

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
 * * constraint_id: institutional_mutation_without_selection
 * human_readable: The Zombie Bureaucracy Drift
 * domain: organizational/political/sociological
 * * SUMMARY:
 * A scenario where an institution's internal rules and goals mutate over time 
 * (Rope) but are shielded from any external "selection" or accountability 
 * mechanism (e.g., market failure, democratic removal, or physical decay). 
 * This coordination substrate becomes a "Snare" for the subject, as the 
 * institution's primary function is liquidated and replaced by self-serving 
 * internal logic, trapping the user in a territory of "institutional 
 * hallucination" where the organization performatively claims to serve a 
 * goal it has long since mutated away from.
 * * KEY AGENTS:
 * - Public Service User: Subject (Powerless)
 * - Institutional Administrator: Beneficiary (Institutional)
 * - Organizational Forensic Auditor: Auditor (Analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% High extraction (0.84) reflects the liquidation of the public mandate to 
% maintain the institution's mutated internal status quo.
domain_priors:base_extractiveness(institutional_mutation_without_selection, 0.84). 
domain_priors:suppression_score(institutional_mutation_without_selection, 0.76). % Alternatives are suppressed by legal monopoly or regulatory capture.
domain_priors:theater_ratio(institutional_mutation_without_selection, 0.94).    % Extreme theater: "Strategic Vision" documents masking the lack of functional output.

% Constraint metric facts (bridge for classification engine)
narrative_ontology:constraint_metric(institutional_mutation_without_selection, extractiveness, 0.84).
narrative_ontology:constraint_metric(institutional_mutation_without_selection, suppression_requirement, 0.76).
narrative_ontology:constraint_metric(institutional_mutation_without_selection, theater_ratio, 0.94).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE SUBJECT (SNARE)
% The user is trapped: they must use the institution for essential services, 
% but the mutation liquidates their agency to receive the actual service.
constraint_indexing:constraint_classification(institutional_mutation_without_selection, snare, 
    context(agent_power(powerless), 
            time_horizon(biographical), 
            exit_options(trapped), 
            spatial_scope(national))).

% PERSPECTIVE 2: THE BENEFICIARY (ROPE)
% The administrator views the mutation as a Rope—the essential coordination 
% substrate for maintaining "institutional stability" in a complex world.
constraint_indexing:constraint_classification(institutional_mutation_without_selection, rope, 
    context(agent_power(institutional), 
            time_horizon(generational), 
            exit_options(mobile), 
            spatial_scope(global))).

% PERSPECTIVE 3: THE SYSTEMS AUDITOR (PITON)
% Theater ratio (0.94) > 0.70 triggers Piton: the "Official Mission Statement" 
% is an inertial spike; it performatively signals a purpose that no longer exists.
constraint_indexing:constraint_classification(institutional_mutation_without_selection, piton, 
    context(agent_power(analytical), 
            time_horizon(historical), 
            exit_options(analytical), 
            spatial_scope(global))).

% PERSPECTIVE 4: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% Detects high extraction (0.84) masking as essential coordination (Rope).
constraint_indexing:constraint_classification(institutional_mutation_without_selection, tangled_rope, 
    context(agent_power(analytical), 
            time_horizon(civilizational), 
            exit_options(arbitrage), 
            spatial_scope(universal))) :-
    domain_priors:base_extractiveness(institutional_mutation_without_selection, E), E >= 0.50,
    domain_priors:suppression_score(institutional_mutation_without_selection, S), S > 0.40.

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(institutional_mutation_tests).

test(perspectival_gap) :-
    % Verify Snare for the powerless user vs Rope for the institutional administrator.
    constraint_indexing:constraint_classification(institutional_mutation_without_selection, snare, 
        context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(institutional_mutation_without_selection, rope, 
        context(agent_power(institutional), _, _, _)).

test(piton_trigger) :-
    % Ensure extreme theater (0.94) correctly triggers the Piton classification.
    constraint_indexing:constraint_classification(institutional_mutation_without_selection, piton, 
        context(agent_power(analytical), _, _, _)).

test(extraction_threshold) :-
    % High extraction (> 0.70) requires [RESOLVED MANDATROPHY] hook.
    domain_priors:base_extractiveness(institutional_mutation_without_selection, E),

    E > 0.70.

:- end_tests(institutional_mutation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The extraction score (0.84) reflects a "Mandatrophy" state where the 
 * "coordination" benefit of organized governance is achieved by liquidating the 
 * subject's primary capacity for service-redress.
 *
 * 
 *
 * * PERSPECTIVAL GAP:
 * The Public Service User feels a Snare because the organization exists solely 
 * to consume its own budget. The Administrator sees a Rope because the 
 * mutated rules coordinate a perfectly legible and stable internal environment.
 *
 * * [RESOLVED MANDATROPHY]:
 * Resolved via the Piton and Tangled Rope classifications. For an analytical 
 * observer, the "Stakeholder Report" is no longer functional for accountability 
 * (Theater 0.94); it is an inert spike siphoning 0.84 of the societal surplus.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% Required for high-extraction constraints (> 0.46).
omega_variable(
    omega_selection_reintroduction,
    'Can "Sunset Clauses" restore the Rope, or is mutation a physical law of hierarchies (Snare vs Mountain)?',
    'Tracking the performance delta of institutions with vs without mandatory 10-year re-chartering audits.',
    'If re-chartering works: Snare of current design. If it fails: Mountain of Organizational Entropy.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(institutional_mutation_without_selection, 0, 10). 

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
