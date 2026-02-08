% ============================================================================
% CONSTRAINT STORY: naming_as_control
% ============================================================================
% Version: 3.4 (Deferential Realism Core)
% Logic: 3.3 (Indexed Tuple P,T,E,S)
% Generated: 2026-01-28
% ============================================================================

:- module(naming_as_control, []).

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
 * * constraint_id: naming_as_control
 * human_readable: The Ontological Hegemony
 * domain: social/political/linguistic
 * * SUMMARY:
 * This constraint occurs when a dominant institution exercises power by 
 * defining the legal and social vocabulary through which reality is 
 * interpreted. By naming a behavior "deviant," "efficient," or "essential," 
 * the institution constrains the possible actions of subjects who must use 
 * that same vocabulary to advocate for themselves.
 * * KEY AGENTS:
 * - Unnamed Subject: Subject (Powerless)
 * - Naming Authority: Beneficiary (Institutional)
 * - Semantic Auditor: Auditor (Analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% High extraction (0.77) as the subject's ability to conceptualize 
% alternatives is siphoned into the institution's linguistic framework.
domain_priors:base_extractiveness(naming_as_control, 0.77). 
domain_priors:suppression_score(naming_as_control, 0.82). % High suppression; "unnamed" concepts are invisible.
domain_priors:theater_ratio(naming_as_control, 0.55).    % Moderate theater; labels are presented as objective facts.

% Constraint metric facts (bridge for classification engine)
narrative_ontology:constraint_metric(naming_as_control, extractiveness, 0.77).
narrative_ontology:constraint_metric(naming_as_control, suppression_requirement, 0.82).
narrative_ontology:constraint_metric(naming_as_control, theater_ratio, 0.55).

% This is a structural property of symbolic power, not a scaffold.
% narrative_ontology:has_sunset_clause(naming_as_control). 

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE SUBJECT (SNARE)
% The subject is trapped in a linguistic snare: they cannot describe their 
% oppression without using the very words that justify it.
constraint_indexing:constraint_classification(naming_as_control, snare, 
    context(agent_power(powerless), 
            time_horizon(biographical), 
            exit_options(trapped), 
            spatial_scope(national))).

% PERSPECTIVE 2: THE BENEFICIARY (ROPE)
% The authority views naming as a vital Rope for social coordination and 
% administrative legibility across a complex population.
constraint_indexing:constraint_classification(naming_as_control, rope, 
    context(agent_power(institutional), 
            time_horizon(generational), 
            exit_options(mobile), 
            spatial_scope(global))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% High extraction (0.77) and suppression (0.82) trigger the hybrid Tangled Rope signature.
constraint_indexing:constraint_classification(naming_as_control, tangled_rope, 
    context(agent_power(analytical), 
            time_horizon(historical), 
            exit_options(analytical), 
            spatial_scope(global))) :-
    domain_priors:base_extractiveness(naming_as_control, E), E >= 0.50,
    domain_priors:suppression_score(naming_as_control, S), S > 0.40.

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(naming_as_control_tests).

test(perspectival_gap) :-
    % Verify Snare for the subject vs Rope for the institutional authority.
    constraint_indexing:constraint_classification(naming_as_control, snare, 
        context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(naming_as_control, rope, 
        context(agent_power(institutional), _, _, _)).

test(threshold_validation) :-
    % Ensure extraction (0.77) triggers high-extraction (>= 0.46) logic gates.
    domain_priors:base_extractiveness(naming_as_control, E),

    E >= 0.46.

:- end_tests(naming_as_control_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The extraction score (0.77) reflects the 'Mandatrophy' threshold where 
 * the "coordination" benefit of shared language is secondary to the 
 * predatory removal of conceptual optionality.
 * 
 * * PERSPECTIVAL GAP:
 * The Unnamed Subject feels a Snare because their lived experience is 
 * "illegalized" or "erased" by the dominant taxonomy. The Authority sees 
 * a Rope because standardized naming reduces the cost of large-scale 
 * societal management.
 * * [RESOLVED MANDATROPHY]:
 * Resolved via the Tangled Rope classification. This recognizes that while 
 * the naming authority provides the necessary "glue" for social coordination, 
 * the 0.77 extraction score identifies the predatory nature of 
 * linguistic monopoly.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% Required for high-extraction constraints (> 0.46).
omega_variable(
    omega_linguistic_drift,
    'Is the vocabulary a fixed biological limit (Mountain) or a mutable policy choice (Snare)?',
    'Historical tracking of "slang" adoption as a proxy for bottom-up semantic leverage.',
    'If slang redefines law: Snare. If law suppresses slang: Mountain of Hegemony.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for structural_linter.py.
narrative_ontology:interval(naming_as_control, 0, 10). 

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
