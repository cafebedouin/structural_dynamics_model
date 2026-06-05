% ============================================================================
% CONSTRAINT STORY: legitimacy_without_effectiveness
% ============================================================================
% Version: 3.4 (Deferential Realism Core)
% Logic: 3.3 (Indexed Tuple P,T,E,S)
% Generated: 2026-01-28
% ============================================================================

:- module(constraint_legitimacy_without_effectiveness, []).

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
 * * constraint_id: legitimacy_without_effectiveness
 * human_readable: The Hollow Mandate
 * domain: political
 * * SUMMARY:
 * A condition where a governing body retains its legal and social recognition 
 * as the rightful authority, but has lost the capacity to fulfill its primary 
 * functions (security, infrastructure, economic stability).
 * * KEY AGENTS:
 * - The Citizen: Subject (Powerless) - Obligated to obey a ghost system.
 * - The Bureaucrat: Beneficiary (Institutional) - Maintaining the facade of office.
 * - The Political Scientist: Auditor (Analytical) - Measuring the "Piton" status.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Numerical anchors for v3.4 thresholds
domain_priors:base_extractiveness(legitimacy_without_effectiveness, 0.75). % High: Extraction to maintain the facade.
domain_priors:suppression_score(legitimacy_without_effectiveness, 0.82).   % High: State monopoly prevents alternatives.
domain_priors:theater_ratio(legitimacy_without_effectiveness, 0.91).       % Very High: Purely theatrical governance.

% Constraint metric facts (bridge for classification engine)
narrative_ontology:constraint_metric(legitimacy_without_effectiveness, extractiveness, 0.75).
narrative_ontology:constraint_metric(legitimacy_without_effectiveness, suppression_requirement, 0.82).
narrative_ontology:constraint_metric(legitimacy_without_effectiveness, theater_ratio, 0.91).

% Binary flags
domain_priors:requires_active_enforcement(legitimacy_without_effectiveness).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE SUBJECT (SNARE)
% To the individual, the law is no longer a guide but a trap that extracts 
% taxes/compliance without providing safety.
constraint_indexing:constraint_classification(legitimacy_without_effectiveness, snare, 
    context(agent_power(powerless), 
            time_horizon(biographical), 
            exit_options(trapped), 
            spatial_scope(national))).

% PERSPECTIVE 2: THE BENEFICIARY (ROPE)
% To the institution, the mandate is a Rope—the essential coordination tool 
% that prevents total social dissolution.
constraint_indexing:constraint_classification(legitimacy_without_effectiveness, rope, 
    context(agent_power(institutional), 
            time_horizon(generational), 
            exit_options(mobile), 
            spatial_scope(national))).

% PERSPECTIVE 3: THE SYSTEMS AUDITOR (PITON)
% Detection of "Piton" status: high theater_ratio indicates a constraint 
% maintained by inertia rather than utility.
constraint_indexing:constraint_classification(legitimacy_without_effectiveness, piton, 
    context(agent_power(analytical), 
            time_horizon(historical), 
            exit_options(arbitrage), 
            spatial_scope(global))) :-
    domain_priors:theater_ratio(legitimacy_without_effectiveness, TR), TR > 0.70.

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(legitimacy_tests).

test(piton_signature) :-
    % Verify the analytical perspective identifies the Piton status.
    constraint_indexing:constraint_classification(legitimacy_without_effectiveness, piton, context(agent_power(analytical), _, _, _)).

test(mandatrophy_trigger) :-
    % High extraction (> 0.7) must be resolved in commentary.
    domain_priors:base_extractiveness(legitimacy_without_effectiveness, E), E > 0.70.

:- end_tests(legitimacy_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The extraction score (0.75) is high because the system consumes resources 
 * purely to perpetuate its own existence (salaries, symbolic rituals) rather 
 * than social output.
 * * [RESOLVED MANDATROPHY]
 * This constraint is not a "Mountain" of natural law; it is a "Piton." 
 * The Mandatrophy is resolved by identifying that the high "Theater Ratio" 
 * (0.91) differentiates it from a functioning Rope. The system "performs" 
 * legitimacy through ceremonies and legalisms to mask its operational 
 * non-existence.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
% High-extraction constraints (> 0.46) require omega_variable.
omega_variable(
    omega_collapse_velocity,
    'Will the Piton fail suddenly (Snap) or erode into a decentralized Scaffold?',
    'Measure of "Alternative Sovereignty" adoption vs. legacy compliance rates.',
    'If snap: Violent transition to Snare; If erosion: Potential for new Rope.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing.
narrative_ontology:interval(legitimacy_without_effectiveness, 0, 10). 

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
