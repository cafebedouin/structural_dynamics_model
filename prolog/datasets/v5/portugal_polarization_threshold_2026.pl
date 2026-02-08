% ============================================================================
% CONSTRAINT STORY: PORTUGAL_POLARIZATION_THRESHOLD_2026
% ============================================================================
% Version: 3.4 (Deferential Realism Core)
% Logic: 3.3 (Indexed Tuple P,T,E,S)
% Generated: 2026-01-27
% ============================================================================

:- module(constraint_portugal_polarization_2026, []).

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
 * * constraint_id: portugal_polarization_threshold_2026
 * human_readable: The "Cordon Sanitaire" / Polarization Threshold
 * domain: political/social
 * * SUMMARY:
 * As the 2026 Presidential election approaches, the "Polarization Threshold" 
 * between the right-wing populist party Chega and traditional parties (PSD, PS) 
 * functions as a Tangled Rope. While it coordinates the "democratic defense" 
 * of institutional norms, it simultaneously extracts political agency by 
 * forcing voters into binary choices and suppressing the viability of 
 * [cite_start]compromise candidates[cite: 5, 6].
 * * KEY AGENTS:
 * - [cite_start]André Ventura: Subject (Organized) - Leader of Chega, representing the "outsider" pole[cite: 5].
 * - [cite_start]Admiral Gouveia e Melo: Beneficiary (Institutional/Analytical) - Polling strongly as a candidate who potentially transcends this divide[cite: 5, 6].
 * - The Traditional Voter: Subject (Powerless) - Constrained by the tactical necessity of preventing "extremism" vs "stagnation."
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Tangled Rope requirement: High base extractiveness (hybrid coordination/extraction).
domain_priors:base_extractiveness(portugal_polarization_threshold_2026, 0.55). 
% Suppression: Significant; alternative centrist paths are squeezed by the poles.
domain_priors:suppression_score(portugal_polarization_threshold_2026, 0.60).   
% Theater: Moderate; the "cordon sanitaire" is both a real barrier and a campaign tool.
domain_priors:theater_ratio(portugal_polarization_threshold_2026, 0.40).       

% Constraint metric facts (bridge for classification engine)
narrative_ontology:constraint_metric(portugal_polarization_threshold_2026, extractiveness, 0.55).
narrative_ontology:constraint_metric(portugal_polarization_threshold_2026, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(portugal_polarization_threshold_2026, theater_ratio, 0.4).

% Binary flags
% No sunset clause as this is a systemic structural feature.
domain_priors:requires_active_enforcement(portugal_polarization_threshold_2026).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% Detects the dual-function of protecting norms while extracting voter choice.
constraint_indexing:constraint_classification(portugal_polarization_threshold_2026, tangled_rope, 
    context(agent_power(analytical), 
            time_horizon(historical), 
            exit_options(analytical), 
            spatial_scope(national))) :-
    domain_priors:base_extractiveness(portugal_polarization_threshold_2026, E), E >= 0.50,
    domain_priors:suppression_score(portugal_polarization_threshold_2026, S), S > 0.40.

% PERSPECTIVE 2: THE INSTITUTIONAL LOYALIST (ROPE)
% Viewed as an essential coordination mechanism to maintain the 1976 regime stability.
constraint_indexing:constraint_classification(portugal_polarization_threshold_2026, rope, 
    context(agent_power(institutional), 
            time_horizon(generational), 
            exit_options(mobile), 
            spatial_scope(national))).

% PERSPECTIVE 3: THE ANTI-ESTABLISHMENT VOTER (SNARE)
% Experienced as a trap designed to delegitimize dissent and fix the outcome.
constraint_indexing:constraint_classification(portugal_polarization_threshold_2026, snare, 
    context(agent_power(powerless), 
            time_horizon(biographical), 
            exit_options(trapped), 
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(portugal_polarization_2026_tests).

test(tangled_rope_detection) :-
    % Verify the analytical auditor detects the hybrid signature.
    constraint_indexing:constraint_classification(portugal_polarization_threshold_2026, tangled_rope, context(agent_power(analytical), _, _, _)).

test(perspectival_gap) :-
    % Verify the Snare vs Rope conflict between the powerless and the institution.
    constraint_indexing:constraint_classification(portugal_polarization_threshold_2026, snare, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(portugal_polarization_threshold_2026, rope, context(agent_power(institutional), _, _, _)).

:- end_tests(portugal_polarization_2026_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The Polarization Threshold is a textbook Tangled Rope. It provides high 
 * coordination value for the "establishment" by defining the boundaries of 
 * acceptable governance, but it is highly extractive for those outside that 
 * [cite_start]boundary[cite: 5]. The polling surge for Gouveia e Melo—an independent 
 * military figure—represents a potential "exit" from this Tangled Rope, as he 
 * [cite_start]currently appeals to segments of both poles[cite: 5, 6].
 *
 * [RESOLVED MANDATROPHY]
 * Extraction (0.55) > 0.46 triggers Omega validation. The system resolves this 
 * by acknowledging the genuine coordination function of the threshold 
 * (preventing systemic collapse) while documenting its asymmetric cost.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    omega_polarization_rupture,
    'Can the independent popularity of Gouveia e Melo untangle the polarization rope or will he be forced into one of the two poles?',
    'Analyzing the runoff (2nd round) vote transfers in late January 2026.',
    'Rupture (Tangled Rope breaks) vs Absorption (Tangled Rope persists)',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(portugal_polarization_threshold_2026, 2019, 2026). 

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
