% ============================================================================
% CONSTRAINT STORY: protocol_capture_tangled_rope
% ============================================================================
% Version: 3.4 (Deferential Realism Core)
% Logic: 3.3 (Indexed Tuple P,T,E,S)
% Generated: 2026-01-28
% ============================================================================

:- module(constraint_protocol_capture, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

% --- Namespace Hooks ---
:- multifile 
    domain_priors:base_extractiveness/2,
    domain_priors:suppression_score/2,
    domain_priors:theater_ratio/2,
    domain_priors:requires_active_enforcement/1,
    narrative_ontology:constraint_metric/3,
    constraint_indexing:constraint_classification/3.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * * constraint_id: protocol_capture_eee
 * human_readable: The Captured Commons (EEE)
 * domain: technological/economic
 * * SUMMARY:
 * A scenario where a dominant entity embraces a decentralized protocol, 
 * extends it with proprietary "features" that create lock-in, and effectively 
 * extinguishes the original open alternative. 
 * It is a Tangled Rope: mandatory for survival but predatory in nature.
 * * KEY AGENTS:
 * - The Developer: Subject (Powerless) - Dependent on the proprietary extensions.
 * - The Platform Giant: Beneficiary (Institutional) - The entity performing the capture.
 * - The Antitrust Auditor: Auditor (Analytical) - Observing the extraction signature.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Numerical anchors for v3.4 thresholds
domain_priors:base_extractiveness(protocol_capture_eee, 0.55). % High: Surplus value captured via proprietary extensions.
domain_priors:suppression_score(protocol_capture_eee, 0.85).   % Very High: Network effects make exit nearly impossible.
domain_priors:theater_ratio(protocol_capture_eee, 0.40).       % Moderate: Claims of "openness" are increasingly theatrical.

% Constraint metric facts (bridge for classification engine)
narrative_ontology:constraint_metric(protocol_capture_eee, extractiveness, 0.55).
narrative_ontology:constraint_metric(protocol_capture_eee, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(protocol_capture_eee, theater_ratio, 0.4).

% Binary flags
domain_priors:requires_active_enforcement(protocol_capture_eee). % Legal/Technical enforcement of EULA/API terms.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE CAPTIVE USER (SNARE)
% To the user, the "extensions" are a trap; they cannot revert to the open standard without losing critical data/access.
constraint_indexing:constraint_classification(protocol_capture_eee, snare, 
    context(agent_power(powerless), 
            time_horizon(biographical), 
            exit_options(trapped), 
            spatial_scope(global))).

% PERSPECTIVE 2: THE SHAREHOLDER (ROPE)
% To the beneficiary, this is a Rope—it provides a stable, high-performance environment for commerce.
constraint_indexing:constraint_classification(protocol_capture_eee, rope, 
    context(agent_power(institutional), 
            time_horizon(generational), 
            exit_options(mobile), 
            spatial_scope(global))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% Detects the hybrid: The system provides genuine coordination (it works) but relies on extraction (lock-in).
constraint_indexing:constraint_classification(protocol_capture_eee, tangled_rope, 
    context(agent_power(analytical), 
            time_horizon(historical), 
            exit_options(analytical), 
            spatial_scope(global))) :-
    domain_priors:base_extractiveness(protocol_capture_eee, E), E >= 0.50,
    domain_priors:suppression_score(protocol_capture_eee, S), S > 0.40.

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(eee_capture_tests).

test(tangled_rope_verification) :-
    % Verify the analytical perspective identifies the Tangled Rope signature.
    constraint_indexing:constraint_classification(protocol_capture_eee, tangled_rope, context(agent_power(analytical), _, _, _)).

test(lock_in_severity) :-
    % Verify that for the powerless, the high suppression score manifests as a Snare.
    constraint_indexing:constraint_classification(protocol_capture_eee, snare, context(agent_power(powerless), _, _, _)).

:- end_tests(eee_capture_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The extraction (0.55) is high because the platform "extends" the protocol 
 * in ways that redirect value to itself (e.g., ad-tracking, transaction fees).
 * * [RESOLVED MANDATROPHY]
 * The Mandatrophy is resolved by identifying that the "coordination" is real 
 * (the network works), but the "extraction" is a choice. It is a Tangled Rope 
 * because one cannot easily separate the utility from the exploitation.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_extinguish_point,
    'At what point do the proprietary extensions render the base protocol non-functional for outsiders?',
    'Measure of protocol fork viability vs. market share of proprietary clients.',
    'If low viability: Permanent Snare; If high viability: The Tangled Rope can be untangled.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing.
narrative_ontology:interval(protocol_capture_eee, 0, 10). 

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
