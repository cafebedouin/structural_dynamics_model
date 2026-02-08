% ============================================================================
% CONSTRAINT STORY: TAIWAN_STORM_2026
% ============================================================================
% Version: 3.4 (Deferential Realism Core)
% Logic: 3.3 (Indexed Tuple P,T,E,S)
% Generated: 2026-01-26
% ============================================================================

:- module(constraint_taiwan_storm, []).

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
 * * constraint_id: taiwan_storm_2026
 * human_readable: The 2026 Taiwan "Perfect Storm" Convergence
 * domain: political/geopolitical
 * * SUMMARY:
 * This constraint represents the alignment of three factors: Xi's 2027 legacy
 * deadline, a temporary window of U.S. indifference under a "non-interventionist"
 * administration, and the normalization of PLA blockade rehearsals.
 * * KEY AGENTS:
 * - The Taiwanese Citizen: Subject (Powerless/Trapped in the "Window")
 * - Xi Jinping / CCP: Beneficiary (Institutional/Coordinating Reunification)
 * - The U.S. Policy Auditor: Auditor (Analytical/Monitoring the "Davidson Window")
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Numerical anchors for v3.4 thresholds
% Extraction is high (0.82) because the goal is "controlling Taiwan,"
% which extracts total sovereignty. [RESOLVED MANDATROPHY]
domain_priors:base_extractiveness(taiwan_storm_2026, 0.82).
domain_priors:suppression_score(taiwan_storm_2026, 0.78).   % Alternatives (independence) are suppressed.
domain_priors:theater_ratio(taiwan_storm_2026, 0.45).       % Significant rehearsal/simulated blockades.

% Constraint metric facts (bridge for classification engine)
narrative_ontology:constraint_metric(taiwan_storm_2026, extractiveness, 0.82).
narrative_ontology:constraint_metric(taiwan_storm_2026, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(taiwan_storm_2026, theater_ratio, 0.45).

% Binary flags
domain_priors:requires_active_enforcement(taiwan_storm_2026).
% narrative_ontology:has_sunset_clause(taiwan_storm_2026). % Applied at index level (Trump term).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE TAIWANESE RESIDENT (SNARE)
% The subject experiences the "inevitability" of reunification as a predatory trap
% exacerbated by U.S. trade tariffs and perceived abandonment.
constraint_indexing:constraint_classification(taiwan_storm_2026, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE CCP LEADERSHIP (ROPE)
% Viewed as the essential coordination of the "reunification dream"
% and the stability required for the 21st Party Congress.
constraint_indexing:constraint_classification(taiwan_storm_2026, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(regional))).

% PERSPECTIVE 3: THE STRATEGIC OPPORTUNIST (SCAFFOLD)
% The "Trump Window" is perceived as a temporary support (Scaffold)
% that may expire after the midterm elections or the 2028 cycle.
constraint_indexing:constraint_classification(taiwan_storm_2026, scaffold,
    context(agent_power(organized),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: THE WASHINGTON DEFENSE ESTABLISHMENT (PITON)
% The "Davidson Window" (2027) acts as an inertial Piton—a deadline that
% drives funding ($7.1B) even as analysts question its operational validity.
constraint_indexing:constraint_classification(taiwan_storm_2026, piton,
    context(agent_power(analytical),
            time_horizon(historical),
            exit_options(analytical),
            spatial_scope(national))) :-
    domain_priors:theater_ratio(taiwan_storm_2026, TR), TR > 0.40.

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(taiwan_storm_tests).

test(perspectival_gap) :-
    % Verify the constraint is a Snare for the subject but a Rope for the institution.
    constraint_indexing:constraint_classification(taiwan_storm_2026, snare, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(taiwan_storm_2026, rope, context(agent_power(institutional), _, _, _)).

test(scaffold_expiration) :-
    % Verify that the opportunistic window is classified as a Scaffold.
    constraint_indexing:constraint_classification(taiwan_storm_2026, scaffold, context(agent_power(organized), _, _, _)).

:- end_tests(taiwan_storm_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The extraction score (0.82) reflects the existential threat to Taiwanese
 * autonomy. The "Perspectival Gap" is defined by Beijing's view of
 * "reunification" as a historical 'Rope' (coordination) vs. the Taiwanese
 * view of it as a 'Snare' (extraction).
 * * MANDATROPHY ANALYSIS:
 * [RESOLVED MANDATROPHY]
 * The high extraction is resolved through the 'Piton' and 'Scaffold'
 * classifications. It acknowledges that while the threat is extractive,
 * the 2027 deadline is also a coordination tool for Washington (funding)
 * and Beijing (readiness), preventing its collapse into a simple 'Snare'
 * across all indices.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_xi_succession,
    'Will Xi prioritize 2027 stability over the "perfect storm" opportunity?',
    'Observation of the 21st Party Congress personnel appointments in 2027.',
    'Stability focus = De-escalation; Opportunity focus = Decisive Campaign.',
    confidence_without_resolution(low)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
% The "Storm" spans from Davidson's 2021 testimony to the 2027 legacy deadline.
narrative_ontology:interval(taiwan_storm_2026, 2021, 2027).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
