% ============================================================================
% CONSTRAINT STORY: ai_banal_capture
% ============================================================================
% Version: 3.4 (Deferential Realism Core)
% Logic: 3.3 (Indexed Tuple P,T,E,S)
% Generated: 2026-02-03
% ============================================================================

:- module(constraint_ai_transition, []).

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
    narrative_ontology:measurement/5,
    constraint_indexing:constraint_classification/3.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * * constraint_id: ai_banal_capture
 * human_readable: The Banal Cognitive Engine
 * domain: technological/social
 * * SUMMARY:
 * A transition where A.I. moves from a "magic" breakthrough to a "banal" 
 * background utility like GPS or spreadsheets. The constraint 
 * functions by automating operations and creating a "psychological 
 * experiment" on billions of users.
 * * KEY AGENTS:
 * - The Student/User: Subject (Powerless) - At risk of relying on "mimicry 
 * machines" rather than critical thinking.
 * - The A.I. Firm: Beneficiary (Institutional) - Automating their own 
 * operations to accelerate progress.
 * - The Risk Assessor: Auditor (Analytical) - Monitoring catastrophic 
 * risks and "emergent" abilities.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Numerical anchors for v3.4 thresholds
% Extraction is high as A.I. takes over creative and cognitive labor.
domain_priors:base_extractiveness(ai_banal_capture, 0.68). 
% Suppression is high due to ubiquity; escaping A.I. becomes difficult.
domain_priors:suppression_score(ai_banal_capture, 0.75).   
% Theater Ratio reflects misconceptions of "magic" vs pattern-matching.
domain_priors:theater_ratio(ai_banal_capture, 0.58).       

% Constraint metric facts (bridge for classification engine)
narrative_ontology:constraint_metric(ai_banal_capture, extractiveness, 0.68).
narrative_ontology:constraint_metric(ai_banal_capture, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(ai_banal_capture, theater_ratio, 0.58).

% Binary flags
domain_priors:requires_active_enforcement(ai_banal_capture).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE SUBJECT (SNARE)
% The student perceives a snare if A.I. makes "shortcuts tempting".
constraint_indexing:constraint_classification(ai_banal_capture, snare, 
    context(agent_power(powerless), 
            time_horizon(biographical), 
            exit_options(trapped), 
            spatial_scope(global))).

% PERSPECTIVE 2: THE BENEFICIARY (ROPE)
% Firms view A.I. as a rope, powering everyday tools and productivity.
constraint_indexing:constraint_classification(ai_banal_capture, rope, 
    context(agent_power(institutional), 
            time_horizon(generational), 
            exit_options(mobile), 
            spatial_scope(global))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% Detects hybrid extraction of data and coordination of knowledge.
constraint_indexing:constraint_classification(ai_banal_capture, tangled_rope, 
    context(agent_power(analytical), 
            time_horizon(historical), 
            exit_options(analytical), 
            spatial_scope(global))) :-
    domain_priors:base_extractiveness(ai_banal_capture, E), E >= 0.50,
    domain_priors:suppression_score(ai_banal_capture, S), S > 0.40.

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ai_transition_tests).

test(perspectival_gap) :-
    % Verify the User sees a Snare (dependency) while the Firm sees a Rope (utility).
    constraint_indexing:constraint_classification(ai_banal_capture, snare, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(ai_banal_capture, rope, context(agent_power(institutional), _, _, _)).

test(extraction_threshold) :-
    domain_priors:base_extractiveness(ai_banal_capture, E),

    E >= 0.46.

:- end_tests(ai_transition_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The Perspectival Gap arises because A.I. captures human "cognitive abilities" 
 * like writing while offering "cheaper spreadsheets". 
 * * [RESOLVED MANDATROPHY]:
 * The extractiveness ($E=0.68$) is justified by the "slop factory" risk in 
 * creativity and the erosion of independent reasoning.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_agi_arrival,
    'Will A.G.I. arrive by 2032 or is it a "mimicry machine" limit?',
    'Empirical testing of flexible reasoning in face of the unknown.',
    'If A.G.I. arrives: Permanent Mountain. If not: Persistent Snare.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_banal_capture, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time (Transition from "Magic Narratives" to "Banal Utility")
narrative_ontology:measurement(ai_tr_t0, ai_banal_capture, theater_ratio, 0, 0.30).
narrative_ontology:measurement(ai_tr_t5, ai_banal_capture, theater_ratio, 5, 0.45).
narrative_ontology:measurement(ai_tr_t10, ai_banal_capture, theater_ratio, 10, 0.58).

% Extraction over time (From proof of concept to "Substantial Automation")
narrative_ontology:measurement(ai_ex_t0, ai_banal_capture, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(ai_ex_t5, ai_banal_capture, base_extractiveness, 5, 0.55).
narrative_ontology:measurement(ai_ex_t10, ai_banal_capture, base_extractiveness, 10, 0.68).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
