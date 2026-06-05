% ============================================================================
% CONSTRAINT STORY: institutional_inertia_lock
% ============================================================================
% Version: 3.4 (Deferential Realism Core)
% Logic: 3.3 (Indexed Tuple P,T,E,S)
% Generated: 2026-01-28
% ============================================================================

:- module(institutional_inertia_lock, []).

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
 * * constraint_id: institutional_inertia_lock
 * human_readable: The Sunk-Cost Regulatory Trap
 * domain: political/technological
 * * SUMMARY:
 * This constraint occurs when a legacy regulatory framework remains in place 
 * because the cost of institutional reorganization exceeds the perceived 
 * friction of the current inefficiency. It acts as a Piton—an inert but 
 * immovable spike in the system's architecture.
 * * KEY AGENTS:
 * - New Entrant: Subject (Powerless)
 * - Incumbent Bureaucracy: Beneficiary (Institutional)
 * - Efficiency Auditor: Auditor (Analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Extraction (0.52) is high enough to require Omega validation but below the 0.7 resolution hard-gate.
domain_priors:base_extractiveness(institutional_inertia_lock, 0.52).
domain_priors:suppression_score(institutional_inertia_lock, 0.45).
domain_priors:theater_ratio(institutional_inertia_lock, 0.82). % Triggers Piton classification.

% Constraint metric facts (bridge for classification engine)
narrative_ontology:constraint_metric(institutional_inertia_lock, extractiveness, 0.52).
narrative_ontology:constraint_metric(institutional_inertia_lock, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(institutional_inertia_lock, theater_ratio, 0.82).

% No sunset clause; this is a permanent structural drag until external shock.
% narrative_ontology:has_sunset_clause(institutional_inertia_lock). 

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE SUBJECT (SNARE)
% To a new entrant, the inertia is a snare that prevents market entry and extracts 'time-tax'.
constraint_indexing:constraint_classification(institutional_inertia_lock, snare, 
    context(agent_power(powerless), 
            time_horizon(biographical), 
            exit_options(trapped), 
            spatial_scope(national))).

% PERSPECTIVE 2: THE BENEFICIARY (ROPE)
% The institution perceives the inertia as a 'Rope' of stability and coordination that 
% prevents chaotic, unvetted systemic changes.
constraint_indexing:constraint_classification(institutional_inertia_lock, rope, 
    context(agent_power(institutional), 
            time_horizon(generational), 
            exit_options(mobile), 
            spatial_scope(global))).

% PERSPECTIVE 3: THE SYSTEMS AUDITOR (PITON)
% Since theater_ratio (0.82) > 0.70, the auditor identifies it as non-functional inertia.
constraint_indexing:constraint_classification(institutional_inertia_lock, piton, 
    context(agent_power(analytical), 
            time_horizon(civilizational), 
            exit_options(arbitrage), 
            spatial_scope(universal))) :-
    domain_priors:theater_ratio(institutional_inertia_lock, TR), TR > 0.70.

% PERSPECTIVE 4: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% High extraction/suppression triggers the Tangled Rope hybrid detection.
constraint_indexing:constraint_classification(institutional_inertia_lock, tangled_rope, 
    context(agent_power(analytical), 
            time_horizon(historical), 
            exit_options(analytical), 
            spatial_scope(global))) :-
    domain_priors:base_extractiveness(institutional_inertia_lock, E), E >= 0.50,
    domain_priors:suppression_score(institutional_inertia_lock, S), S > 0.40.

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(institutional_inertia_lock_tests).

test(perspectival_gap) :-
    % Verify Snare for powerless vs Rope for institutional.
    constraint_indexing:constraint_classification(institutional_inertia_lock, snare, 
        context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(institutional_inertia_lock, rope, 
        context(agent_power(institutional), _, _, _)).

test(piton_audit_logic) :-
    % Ensure high theater ratio forces a Piton classification.
    constraint_indexing:constraint_classification(institutional_inertia_lock, piton, 
        context(agent_power(analytical), _, _, _)).

:- end_tests(institutional_inertia_lock_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The extraction score (0.52) reflects a system where coordination is beginning 
 * to be outweighed by the 'rent' of maintaining an obsolete structure.
 * * PERSPECTIVAL GAP:
 * The Subject views the 'lock' as a Snare because it actively suppresses 
 * alternatives. The Institution views it as a Rope because it provides 
 * predictable (if inefficient) coordination.
 * * [RESOLVED MANDATROPHY]:
 * While extraction is below the 0.7 mandatory threshold, the 'Piton' classification 
 * at the analytical level proactively resolves the risk of mislabeling 
 * simple coordination as permanent extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% Required as extraction (0.52) > 0.46.
omega_variable(
    omega_inertia_source,
    'Is the lock caused by procedural complexity (Mountain) or active rent-seeking (Snare)?',
    'Standardization of API-driven regulatory submission across jurisdictions.',
    'If complexity persists: Mountain. If submission streamlines: Snare was present.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for structural_linter.py.
narrative_ontology:interval(institutional_inertia_lock, 0, 10). 

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
