% ============================================================================
% CONSTRAINT STORY: GLOBAL_WATER_BANKRUPTCY
% ============================================================================
% Version: 3.4 (Deferential Realism Core)
% Logic: 3.3 (Indexed Tuple P,T,E,S)
% Generated: 2026-01-26
% ============================================================================

:- module(constraint_water_bankruptcy, []).

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
 * * constraint_id: global_water_bankruptcy
 * human_readable: The Global Water Bankruptcy Constraint
 * domain: environmental/economic
 * * SUMMARY:
 * Humanity is currently living beyond its hydrological means, creating a 
 * state of "bankruptcy" where demand exceeds sustainable supply. 
 * This imposes a severe constraint on peace, food security, and 
 * biodiversity protection.
 * * KEY AGENTS:
 * - The Vulnerable Local: Subject (Powerless/Trapped in scarcity)
 * - UN Water / Member States: Beneficiary (Institutional/Coordinating)
 * - UNU-INWEH Researchers: Auditor (Analytical/Diagnostic)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Numerical anchors for v3.4 thresholds
% Extraction is extremely high (0.85) as bankruptcy extracts the physical 
% basis for life and stability. [RESOLVED MANDATROPHY]
domain_priors:base_extractiveness(global_water_bankruptcy, 0.85). 
domain_priors:suppression_score(global_water_bankruptcy, 0.60).   
domain_priors:theater_ratio(global_water_bankruptcy, 0.40).       

% Constraint metric facts (bridge for classification engine)
narrative_ontology:constraint_metric(global_water_bankruptcy, extractiveness, 0.85).
narrative_ontology:constraint_metric(global_water_bankruptcy, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(global_water_bankruptcy, theater_ratio, 0.4).

% Binary flags
domain_priors:requires_active_enforcement(global_water_bankruptcy). % Mandatory monitoring required.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE SCARCITY-IMPACTED INDIVIDUAL (SNARE)
% Water bankruptcy is experienced as a predatory trap where basic needs 
% are extracted by hydrological debt.
constraint_indexing:constraint_classification(global_water_bankruptcy, snare, 
    context(agent_power(powerless), 
            time_horizon(biographical), 
            exit_options(trapped), 
            spatial_scope(local))).

% PERSPECTIVE 2: THE UN WATER SYSTEM (ROPE)
% Viewed as a coordination framework (e.g., Water Action Decade) to 
% reset the global agenda and bridge peace.
constraint_indexing:constraint_classification(global_water_bankruptcy, rope, 
    context(agent_power(institutional), 
            time_horizon(generational), 
            exit_options(mobile), 
            spatial_scope(global))).

% PERSPECTIVE 3: THE HYDROLOGICAL SCIENTIST (TANGLED ROPE)
% Detects how policy "milestones" act as coordination (Rope) while 
% failing to stop the deep extraction of the bankruptcy itself.
constraint_indexing:constraint_classification(global_water_bankruptcy, tangled_rope, 
    context(agent_power(analytical), 
            time_horizon(historical), 
            exit_options(analytical), 
            spatial_scope(global))) :-
    domain_priors:base_extractiveness(global_water_bankruptcy, E), E >= 0.50.

% PERSPECTIVE 4: THE WATER ACTION DECADE (SCAFFOLD)
% A temporary support structure with a clear sunset clause (ends 2028) 
% to transition toward the 2030 SDG deadlines.
constraint_indexing:constraint_classification(global_water_bankruptcy, scaffold, 
    context(agent_power(organized), 
            time_horizon(immediate), 
            exit_options(constrained), 
            spatial_scope(continental))) :-
    narrative_ontology:has_sunset_clause(global_water_bankruptcy).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(water_bankruptcy_tests).

test(perspectival_gap) :-
    % Verify the constraint is a Snare for the powerless but a Rope for the UN.
    constraint_indexing:constraint_classification(global_water_bankruptcy, snare, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(global_water_bankruptcy, rope, context(agent_power(institutional), _, _, _)).

test(extraction_threshold) :-
    domain_priors:base_extractiveness(global_water_bankruptcy, E),

    E >= 0.46. % Correct for high-extraction Snare/Tangled logic.

:- end_tests(water_bankruptcy_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The extraction score (0.85) is critical because living beyond 
 * "hydrological means" is a net-loss scenario for the species. 
 * The perspectival gap exists because the UN sees the Conferences as 
 * coordination 'Ropes', while the diagnostic reality reveals an 
 * extractive 'Snare' driven by fragmentation.
 * * MANDATROPHY ANALYSIS:
 * [RESOLVED MANDATROPHY]
 * The high extraction is resolved through the 'Scaffold' status of the 
 * Water Action Decade. It acknowledges that current coordination 
 * is a temporary support structure intended to bridge toward a 
 * 2030 reset.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_bankruptcy_reset,
    'Will the 2026/2028 UN milestones successfully "reset" the agenda?',
    'Analysis of Global Water Bankruptcy monitoring data by 2029.',
    'Success = Transition to Mountain (Stable Law); Failure = Kinetic Snare.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
% Covers the period of the Water Action Decade and the 2030 SDG deadline.
narrative_ontology:interval(global_water_bankruptcy, 2018, 2030). 
narrative_ontology:has_sunset_clause(global_water_bankruptcy). % Linked to the end of the Decade in 2028.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
