% ============================================================================
% CONSTRAINT STORY: technocratic_overreach
% ============================================================================
% Version: 3.4 (Deferential Realism Core)
% Logic: 3.3 (Indexed Tuple P,T,E,S)
% Generated: 2026-01-28
% ============================================================================

:- module(constraint_technocratic_overreach, []).

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
 * * constraint_id: technocratic_overreach
 * human_readable: The Rule of the Expert
 * domain: political/technological
 * * SUMMARY:
 * A scenario where administrative or technical experts impose restrictive 
 * constraints under the guise of "optimization" or "safety," effectively 
 * removing the subject's ability to participate in decision-making.
 * * KEY AGENTS:
 * - The Citizen: Subject (Powerless) - Required to follow "optimized" mandates they cannot audit.
 * - The System Architect: Beneficiary (Institutional) - Enforces the technical Rope.
 * - The Ethicist: Auditor (Analytical) - Identifies the extraction of agency.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Numerical anchors for v3.4 thresholds
domain_priors:base_extractiveness(technocratic_overreach, 0.64). % High: Extracts agency and political liberty.
domain_priors:suppression_score(technocratic_overreach, 0.78).   % High: Complexity makes alternatives invisible.
domain_priors:theater_ratio(technocratic_overreach, 0.35).       % Moderate: The technical function is real, but the "intent" is often theatrical.

% Constraint metric facts (bridge for classification engine)
narrative_ontology:constraint_metric(technocratic_overreach, extractiveness, 0.64).
narrative_ontology:constraint_metric(technocratic_overreach, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(technocratic_overreach, theater_ratio, 0.35).

% Binary flags
domain_priors:requires_active_enforcement(technocratic_overreach).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE SUBJECT (SNARE)
% For the individual, the overreach is a Snare—a trap of "necessary" rules that 
% cannot be challenged without expert credentials.
constraint_indexing:constraint_classification(technocratic_overreach, snare, 
    context(agent_power(powerless), 
            time_horizon(biographical), 
            exit_options(trapped), 
            spatial_scope(national))).

% PERSPECTIVE 2: THE INSTITUTION (ROPE)
% To the state/architect, this is a Rope—the only way to manage modern 
% complexity safely and efficiently.
constraint_indexing:constraint_classification(technocratic_overreach, rope, 
    context(agent_power(institutional), 
            time_horizon(generational), 
            exit_options(mobile), 
            spatial_scope(national))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% Detects the hybrid: The technical coordination is functional (Rope), but 
% the removal of democratic consent is extractive (Snare).
constraint_indexing:constraint_classification(technocratic_overreach, tangled_rope, 
    context(agent_power(analytical), 
            time_horizon(historical), 
            exit_options(analytical), 
            spatial_scope(global))) :-
    domain_priors:base_extractiveness(technocratic_overreach, E), E >= 0.50,
    domain_priors:suppression_score(technocratic_overreach, S), S > 0.40.

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(technocratic_overreach_tests).

test(perspectival_variance) :-
    % Verify the shift from Snare (Powerless) to Rope (Institutional).
    constraint_indexing:constraint_classification(technocratic_overreach, snare, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(technocratic_overreach, rope, context(agent_power(institutional), _, _, _)).

test(omega_trigger_check) :-
    % High extraction (> 0.46) requires an omega variable.
    domain_priors:base_extractiveness(technocratic_overreach, E), E > 0.46.

:- end_tests(technocratic_overreach_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The extraction score (0.64) reflects the "agency tax." The subject pays for 
 * "optimization" by losing the right to choose sub-optimal but free paths.
 * * [RESOLVED MANDATROPHY]
 * The Mandatrophy is resolved by the "Tangled Rope" classification for auditors. 
 * This prevents the system from mislabeling the overreach as an unavoidable 
 * "Mountain" of technical necessity; it identifies that the complexity is being 
 * used as a tool for extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_algorithmic_bias,
    'Is the technical constraint truly objective, or is it a "Snare" designed to favor the Architect?',
    'Audit of algorithmic weights vs. public benefit metrics.',
    'If biased: Pure Snare; If objective: A high-tension Rope/Scaffold.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing.
narrative_ontology:interval(technocratic_overreach, 0, 10). 

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
