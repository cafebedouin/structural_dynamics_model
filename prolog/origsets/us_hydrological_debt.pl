% ============================================================================
% CONSTRAINT STORY: US_HYDROLOGICAL_DEBT
% ============================================================================
% Version: 3.4 (Deferential Realism Core)
% Logic: 3.3 (Indexed Tuple P,T,E,S)
% Generated: 2026-01-26
% ============================================================================

:- module(constraint_us_water_debt, []).

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
 * * constraint_id: us_hydrological_debt
 * human_readable: The U.S. Hydrological Debt / Water Bankruptcy
 * domain: environmental/economic
 * * SUMMARY:
 * [cite_start]The U.S. is currently "living beyond its hydrological means"[cite: 5]. 
 * This debt is characterized by the over-extraction of groundwater and 
 * the over-allocation of surface water, creating an unsustainable 
 * economic dependency on finite water reserves.
 * * KEY AGENTS:
 * - The Central Valley Farmer: Subject (Powerless/Trapped in the debt cycle)
 * - Federal/State Water Bureaus: Beneficiary (Institutional/Coordinating)
 * - [cite_start]UNU-INWEH Hub (New York): Auditor (Analytical/Diagnostic) [cite: 5]
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Numerical anchors for v3.4 thresholds
% Extraction is 0.78 as the "bankruptcy" extracts the future's water security 
% [cite_start] to maintain current growth[cite: 5]. [RESOLVED MANDATROPHY]
domain_priors:base_extractiveness(us_hydrological_debt, 0.78). 
domain_priors:suppression_score(us_hydrological_debt, 0.65).   % Alternatives are suppressed by infrastructure.
domain_priors:theater_ratio(us_hydrological_debt, 0.25).       % Significant functional use, low "theater".

% Constraint metric facts (bridge for classification engine)
narrative_ontology:constraint_metric(us_hydrological_debt, extractiveness, 0.78).
narrative_ontology:constraint_metric(us_hydrological_debt, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(us_hydrological_debt, theater_ratio, 0.25).

% Binary flags
domain_priors:requires_active_enforcement(us_hydrological_debt). % Managed by water rights and permits.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE LOCAL RESIDENT (SNARE)
% Water scarcity is experienced as a predatory trap where residential 
% wells fail due to industrial/agricultural "debt" extraction.
constraint_indexing:constraint_classification(us_hydrological_debt, snare, 
    context(agent_power(powerless), 
            time_horizon(biographical), 
            exit_options(trapped), 
            spatial_scope(local))).

% PERSPECTIVE 2: THE WATER DISTRICT (ROPE)
% Viewed as an essential coordination mechanism for delivering 
% predictable supply to millions of users.
constraint_indexing:constraint_classification(us_hydrological_debt, rope, 
    context(agent_power(institutional), 
            time_horizon(biographical), 
            exit_options(mobile), 
            spatial_scope(regional))).

% PERSPECTIVE 3: THE HYDROLOGICAL AUDITOR (TANGLED ROPE)
% Detects the hybrid signature: coordination of the current economy 
% [cite_start] vs. the extraction of long-term hydrological solvency[cite: 5].
constraint_indexing:constraint_classification(us_hydrological_debt, tangled_rope, 
    context(agent_power(analytical), 
            time_horizon(historical), 
            exit_options(analytical), 
            spatial_scope(national))) :-
    domain_priors:base_extractiveness(us_hydrological_debt, E), E >= 0.50.

% PERSPECTIVE 4: POLICY REFORMER (SCAFFOLD)
% Current water compacts act as a temporary support (Scaffold) until 
% [cite_start] the "2030 SDG deadline" or mandatory reset[cite: 5].
constraint_indexing:constraint_classification(us_hydrological_debt, scaffold, 
    context(agent_power(organized), 
            time_horizon(generational), 
            exit_options(constrained), 
            spatial_scope(continental))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(us_water_debt_tests).

test(perspectival_gap) :-
    % Verify the constraint is a Snare for the resident but a Rope for the district.
    constraint_indexing:constraint_classification(us_hydrological_debt, snare, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(us_hydrological_debt, rope, context(agent_power(institutional), _, _, _)).

test(extraction_threshold) :-
    domain_priors:base_extractiveness(us_hydrological_debt, E),

    E >= 0.46. % Correct for high-extraction logic.

:- end_tests(us_water_debt_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The extraction score (0.78) is justified by the report's framing of 
 * [cite_start]"living beyond our hydrological means"[cite: 5]. This is a literal 
 * debt where the future's liquidity is spent today. 
 * The perspectival gap exists because institutions see coordination 
 * (Rope) for the current populace, while analytics reveal an 
 * extractive Snare for future cohorts.
 * * MANDATROPHY ANALYSIS:
 * [RESOLVED MANDATROPHY]
 * The high extraction is resolved through the 'Tangled Rope' classification. 
 * It acknowledges that the system provides genuine coordination for food 
 * [cite_start]security and industry[cite: 5], preventing its collapse into a pure 
 * predatory Snare while still signaling the critical debt level.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_aquifer_recharge,
    'Can engineered recharge (Managed Aquifer Recharge) convert the debt into a stable Mountain?',
    'Analysis of recharge rates vs extraction rates in the Central Valley by 2030.',
    'Success = Re-classification as Mountain (Stable Law); Failure = Hard Snare.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
% [cite_start] Covers the period leading to the 2030 SDG and Water Action Decade targets[cite: 5].
narrative_ontology:interval(us_hydrological_debt, 2020, 2030). 

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
