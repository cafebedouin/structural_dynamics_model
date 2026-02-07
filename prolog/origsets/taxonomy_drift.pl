% ============================================================================
% CONSTRAINT STORY: taxonomy_drift
% ============================================================================
% Version: 3.4 (Deferential Realism Core)
% Logic: 3.3 (Indexed Tuple P,T,E,S)
% Generated: 2026-01-28
% ============================================================================

:- module(taxonomy_drift, []).

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
 * * constraint_id: taxonomy_drift
 * human_readable: The Semantic Slippage Trap
 * domain: social/linguistic/bureaucratic
 * * SUMMARY:
 * A scenario where the definitions used by a governing system drift away from 
 * the ground-truth reality of the subjects. As the taxonomy becomes 
 * self-referential, it functions as a Snare for those who no longer fit 
 * into the system's "legible" categories, yet are forced to interact with 
 * it for survival.
 * * KEY AGENTS:
 * - Non-Categorical Subject: Subject (Powerless)
 * - Classification Authority: Beneficiary (Institutional)
 * - Ontological Auditor: Auditor (Analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% High extraction (0.76) as subjects spend significant resources trying to 
% "perform" a category they don't inhabit to access basic services.
domain_priors:base_extractiveness(taxonomy_drift, 0.76). 
domain_priors:suppression_score(taxonomy_drift, 0.62). 
domain_priors:theater_ratio(taxonomy_drift, 0.85). % Extreme theater: the categories are purely performative.

% Constraint metric facts (bridge for classification engine)
narrative_ontology:constraint_metric(taxonomy_drift, extractiveness, 0.76).
narrative_ontology:constraint_metric(taxonomy_drift, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(taxonomy_drift, theater_ratio, 0.85).

% Not a scaffold; this is a failure of institutional semiotics.
% narrative_ontology:has_sunset_clause(taxonomy_drift). 

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE SUBJECT (SNARE)
% The subject is trapped in a category that is factually wrong but legally binding.
constraint_indexing:constraint_classification(taxonomy_drift, snare, 
    context(agent_power(powerless), 
            time_horizon(biographical), 
            exit_options(trapped), 
            spatial_scope(national))).

% PERSPECTIVE 2: THE BENEFICIARY (ROPE)
% The authority views the rigid taxonomy as a vital Rope for administrative 
% legibility and large-scale coordination.
constraint_indexing:constraint_classification(taxonomy_drift, rope, 
    context(agent_power(institutional), 
            time_horizon(generational), 
            exit_options(mobile), 
            spatial_scope(global))).

% PERSPECTIVE 3: THE SYSTEMS AUDITOR (PITON)
% Theater ratio (0.85) > 0.70 triggers Piton: the taxonomy is now a non-functional 
% inertial spike that maintains the illusion of order.
constraint_indexing:constraint_classification(taxonomy_drift, piton, 
    context(agent_power(analytical), 
            time_horizon(civilizational), 
            exit_options(arbitrage), 
            spatial_scope(universal))) :-
    domain_priors:theater_ratio(taxonomy_drift, TR), TR > 0.70.

% PERSPECTIVE 4: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% Detects high extraction (0.76) and high theater (0.85) as a Tangled Rope.
constraint_indexing:constraint_classification(taxonomy_drift, tangled_rope, 
    context(agent_power(analytical), 
            time_horizon(historical), 
            exit_options(analytical), 
            spatial_scope(global))) :-
    domain_priors:base_extractiveness(taxonomy_drift, E), E >= 0.50,
    domain_priors:suppression_score(taxonomy_drift, S), S > 0.40.

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(taxonomy_drift_tests).

test(perspectival_gap) :-
    % Verify Snare for the subject vs Rope for the institution.
    constraint_indexing:constraint_classification(taxonomy_drift, snare, 
        context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(taxonomy_drift, rope, 
        context(agent_power(institutional), _, _, _)).

test(piton_threshold) :-
    % Verify theater ratio triggers Piton for analytical auditors.
    constraint_indexing:constraint_classification(taxonomy_drift, piton, 
        context(agent_power(analytical), _, _, _)).

:- end_tests(taxonomy_drift_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The extraction score (0.76) reflects the 'Mandatrophy' threshold where the 
 * administrative cost of misclassification falls entirely on the subject.
 * * PERSPECTIVAL GAP:
 * 
 * The individual experiences a Snare because they must lie to the system to 
 * receive coordination. The institution sees a Rope because the rigid labels 
 * allow for easy automated processing.
 * * [RESOLVED MANDATROPHY]:
 * Resolved by the Piton classification. For an analytical observer, the 
 * taxonomy no longer coordinates reality; it is an inertial structure 
 * that siphons truth to maintain administrative theater.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% Required for constraints with base_extractiveness > 0.46.
omega_variable(
    omega_linguistic_anchor,
    'Can a bottom-up folksonomy displace the top-down taxonomy (Snare vs Mountain)?',
    'Tracking adoption of non-standard tags in public digital repositories.',
    'If folksonomy thrives: Snare (enforced choice). If it fails: Mountain (cognitive limit).',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for structural_linter.py.
narrative_ontology:interval(taxonomy_drift, 0, 10). 

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
