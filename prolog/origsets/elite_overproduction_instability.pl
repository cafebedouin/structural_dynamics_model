% ============================================================================
% CONSTRAINT STORY: elite_overproduction_instability
% ============================================================================
% Version: 3.4 (Deferential Realism Core)
% Logic: 3.3 (Indexed Tuple P,T,E,S)
% Generated: 2026-01-28
% ============================================================================

:- module(constraint_elite_overproduction_instability, []).

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
 * * constraint_id: elite_overproduction_instability
 * human_readable: The Aspirant's Bottleneck
 * domain: social
 * * SUMMARY:
 * A structural condition where the number of individuals educated and prepared 
 * for elite positions significantly exceeds the available slots in the social 
 * hierarchy. This results in a "Snare" for the overproduced aspirants and a 
 * "Tangled Rope" for the system itself.
 * * KEY AGENTS:
 * - The Aspirant: Subject (Powerless) - Highly credentialed but economically blocked.
 * - The Incumbent: Beneficiary (Institutional) - Controls entry to existing slots.
 * - The Sociologist: Auditor (Analytical) - Identifies the "Tangled Rope" logic.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Numerical anchors for v3.4 thresholds
domain_priors:base_extractiveness(elite_overproduction_instability, 0.58). % High: credential inflation extracts labor/debt.
domain_priors:suppression_score(elite_overproduction_instability, 0.70).   % High: few non-credentialed paths to power.
domain_priors:theater_ratio(elite_overproduction_instability, 0.45).       % Moderate: Meritocracy signals increasingly theatrical.

% Constraint metric facts (bridge for classification engine)
narrative_ontology:constraint_metric(elite_overproduction_instability, extractiveness, 0.58).
narrative_ontology:constraint_metric(elite_overproduction_instability, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(elite_overproduction_instability, theater_ratio, 0.45).

% Binary flags
domain_priors:requires_active_enforcement(elite_overproduction_instability).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE SUBJECT (SNARE)
% For the aspirant, the system is a trap of endless preparation with no exit.
constraint_indexing:constraint_classification(elite_overproduction_instability, snare, 
    context(agent_power(powerless), 
            time_horizon(biographical), 
            exit_options(trapped), 
            spatial_scope(national))).

% PERSPECTIVE 2: THE BENEFICIARY (ROPE)
% To the institution, the surplus of talent is a Rope—it ensures high competition and efficiency.
constraint_indexing:constraint_classification(elite_overproduction_instability, rope, 
    context(agent_power(institutional), 
            time_horizon(generational), 
            exit_options(mobile), 
            spatial_scope(national))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% Recognizes the hybrid: it facilitates talent selection but creates radicalizing surplus.
constraint_indexing:constraint_classification(elite_overproduction_instability, tangled_rope, 
    context(agent_power(analytical), 
            time_horizon(historical), 
            exit_options(analytical), 
            spatial_scope(global))) :-
    domain_priors:base_extractiveness(elite_overproduction_instability, E), E >= 0.50,
    domain_priors:suppression_score(elite_overproduction_instability, S), S > 0.40.

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(elite_overproduction_instability_tests).

test(perspectival_gap) :-
    % Verify variance: Snare for the powerless, Rope for the institution.
    constraint_indexing:constraint_classification(elite_overproduction_instability, snare, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(elite_overproduction_instability, rope, context(agent_power(institutional), _, _, _)).

test(tangled_threshold) :-
    % Verify that high extraction + high suppression triggers Tangled Rope for analysts.
    constraint_indexing:constraint_classification(elite_overproduction_instability, tangled_rope, context(agent_power(analytical), _, _, _)).

:- end_tests(elite_overproduction_instability_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The extraction score (0.58) reflects the cost of credentialing (debt/time) 
 * relative to the likelihood of a status-commensurate return.
 * * [RESOLVED MANDATROPHY]
 * The Mandatrophy is resolved by the "Tangled Rope" classification. It prevents 
 * the system from seeing the instability as mere bad luck; it is a structural 
 * byproduct of the coordination mechanism (meritocratic competition).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
% High-extraction (> 0.46) requires omega_variable.
omega_variable(
    omega_aspirant_radicalization,
    'Will the surplus aspirants reform the system (Scaffold) or attempt to dismantle it (Snare)?',
    'Longitudinal study of counter-elite political movements vs. institutional absorption.',
    'If reform: Transition to Scaffold; If dismantle: Collapse of the current Rope.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing.
narrative_ontology:interval(elite_overproduction_instability, 0, 10). 

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
