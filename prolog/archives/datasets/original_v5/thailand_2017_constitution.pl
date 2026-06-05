% ============================================================================
% CONSTRAINT STORY: THAILAND_2017_CONSTITUTION
% ============================================================================
% Version: 3.4 (Deferential Realism Core)
% Logic: 3.3 (Indexed Tuple P,T,E,S)
% Generated: 2026-01-27
% ============================================================================

:- module(constraint_thailand_2017_constitution, []).

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
    constraint_indexing:constraint_classification/3.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * * constraint_id: thailand_2017_constitution
 * human_readable: The 2017 Thai Constitution (Junta-Era Framework)
 * domain: political/legal
 * * SUMMARY:
 * Implemented under the National Council for Peace and Order (junta)[cite: 68], this 
 * constitution functions as a structural constraint on democratic expression. While 
 * transitory provisions granting the Senate voting rights for the PM have expired[cite: 68, 69], 
 * the rigid amendment process (Article 256) [cite: 423, 424] and the Senate's 
 * remaining veto power [cite: 451, 452] maintain a high extraction of political agency.
 * * KEY AGENTS:
 * - The People's Party: Subject (Individual Powerless/Organized) - Seeking a "People's Constitution"[cite: 111, 459].
 * - The Conservative Establishment: Beneficiary (Institutional) - Represented by parties like UTN that oppose a rewrite[cite: 459].
 * - The Constitutional Court: Auditor (Analytical) - Mandating a multi-step referendum process[cite: 417, 420].
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% High extraction due to the "Senate Veto" and restricted amendment paths.
domain_priors:base_extractiveness(thailand_2017_constitution, 0.65). 
% High suppression: Criticism was historically banned [cite: 407] and legal hurdles to rewrite remain extreme[cite: 409, 417].
domain_priors:suppression_score(thailand_2017_constitution, 0.75).   
% Significant theater in the "coordination" aspect of the Senate's role post-2024.
domain_priors:theater_ratio(thailand_2017_constitution, 0.45).       

% Constraint metric facts (bridge for classification engine)
narrative_ontology:constraint_metric(thailand_2017_constitution, extractiveness, 0.65).
narrative_ontology:constraint_metric(thailand_2017_constitution, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(thailand_2017_constitution, theater_ratio, 0.45).

% Binary flags
domain_priors:requires_active_enforcement(thailand_2017_constitution). % Required via Constitutional Court rulings[cite: 9, 104].

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE DISFRANCHISED VOTER (SNARE)
% Viewed as a trap designed to contain the influence of progressive mandates.
constraint_indexing:constraint_classification(thailand_2017_constitution, snare, 
    context(agent_power(powerless), 
            time_horizon(biographical), 
            exit_options(trapped), 
            spatial_scope(national))).

% PERSPECTIVE 2: THE CONSERVATIVE COALITION (ROPE)
% Viewed as essential infrastructure to prevent "disqualified politicians" from re-emerging[cite: 459].
constraint_indexing:constraint_classification(thailand_2017_constitution, rope, 
    context(agent_power(institutional), 
            time_horizon(generational), 
            exit_options(mobile), 
            spatial_scope(national))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% Detects the hybrid of genuine state stability (coordination) and military-backed extraction.
constraint_indexing:constraint_classification(thailand_2017_constitution, tangled_rope, 
    context(agent_power(analytical), 
            time_horizon(historical), 
            exit_options(analytical), 
            spatial_scope(national))) :-
    domain_priors:base_extractiveness(thailand_2017_constitution, E), E >= 0.46,
    domain_priors:suppression_score(thailand_2017_constitution, S), S > 0.40.

% PERSPECTIVE 4: THE REFORMIST BLOC (SCAFFOLD)
% The current charter is tolerated only as a temporary bridge to the 2026 referendum[cite: 394, 415].
constraint_indexing:constraint_classification(thailand_2017_constitution, scaffold, 
    context(agent_power(organized), 
            time_horizon(immediate), 
            exit_options(constrained), 
            spatial_scope(national))) :-
    narrative_ontology:has_sunset_clause(thailand_2017_constitution).

% Hook to enable Scaffold classification based on the Feb 8, 2026 referendum date[cite: 396].
narrative_ontology:has_sunset_clause(thailand_2017_constitution).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(thailand_2017_constitution_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(thailand_2017_constitution, snare, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(thailand_2017_constitution, rope, context(agent_power(institutional), _, _, _)).

test(threshold_validation) :-
    domain_priors:base_extractiveness(thailand_2017_constitution, E),

    E >= 0.46. % Correctly identifies as a high-extraction constraint.

test(scaffold_resolution) :-
    constraint_indexing:constraint_classification(thailand_2017_constitution, scaffold, context(agent_power(organized), _, _, _)).

:- end_tests(thailand_2017_constitution_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The extraction score (0.65) reflects the "Senate Veto" preserved in the Dec 2025 debates[cite: 451, 452]. 
 * While the Senate no longer elects the PM[cite: 68], its ability to block constitutional 
 * change creates a classic Perspectival Gap: reformists (Subject) see a Snare, while 
 * conservative parties like UTN see a Rope protecting "valuable provisions"[cite: 459].
 * * [RESOLVED MANDATROPHY]
 * The Tangled Rope classification acknowledges that the constitution provides a 
 * coordination framework for the 2026 elections, even if the underlying mechanics 
 * are extractive.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% High-extraction resolution hook.
omega_variable(
    omega_referendum_integrity,
    'Will the three-referendum mandate effectively finalize the Scaffold or entrench the Snare?',
    'Observation of the Feb 8, 2026 voter turnout and Constitutional Court response to the result[cite: 396, 420].',
    'Success (Scaffold expires) vs Failure (Constraint reverts to Snare/Piton)',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Tracking the 2017-2026 interval of the charter's primary friction period.
narrative_ontology:interval(thailand_2017_constitution, 2017, 2026). 

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
