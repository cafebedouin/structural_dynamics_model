% ============================================================================
% CONSTRAINT STORY: GLOBAL_STIMULUS_SPREE
% ============================================================================
% Version: 3.4 (Deferential Realism Core)
% Logic: 3.3 (Indexed Tuple P,T,E,S)
% Generated: 2026-01-26
% ============================================================================

:- module(global_stimulus_spree, []).

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
 * * constraint_id: global_stimulus_spree
 * human_readable: The 2026 Global Fiscal Stimulus Surge
 * domain: economic/political
 * * SUMMARY:
 * Governments are deploying multitrillion-dollar stimulus packages to fuel AI, 
 * green energy, and rearmament. While boosting near-term growth, this creates 
 * a "red flag" vulnerability of soaring debt and interest payments.
 * * KEY AGENTS:
 * - The Taxpayer: Subject (Powerless/Trapped in debt cycle)
 * - The G7/G20 Governments: Beneficiary (Institutional/Mobile)
 * - The Bond Market Auditor: Auditor (Analytical/Detection of risk)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Numerical anchors for v3.4 thresholds
% Extraction is high (0.62) due to record interest payments and debt-to-GDP 
% projected to exceed 100%.
domain_priors:base_extractiveness(global_stimulus_spree, 0.62). 
domain_priors:suppression_score(global_stimulus_spree, 0.45).   
domain_priors:theater_ratio(global_stimulus_spree, 0.30).       

% Constraint metric facts (bridge for classification engine)
narrative_ontology:constraint_metric(global_stimulus_spree, extractiveness, 0.62).
narrative_ontology:constraint_metric(global_stimulus_spree, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(global_stimulus_spree, theater_ratio, 0.3).

% Binary flags
domain_priors:requires_active_enforcement(global_stimulus_spree). 

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE FUTURE TAXPAYER (SNARE)
% The debt burden is a trap where future generations pay the bill 
% for current growth-sapping shocks.
constraint_indexing:constraint_classification(global_stimulus_spree, snare, 
    context(agent_power(powerless), 
            time_horizon(generational), 
            exit_options(trapped), 
            spatial_scope(national))).

% PERSPECTIVE 2: THE NATIONAL GOVERNMENT (ROPE)
% Viewed as essential coordination for generational investments 
% in infrastructure and sovereignty.
constraint_indexing:constraint_classification(global_stimulus_spree, rope, 
    context(agent_power(institutional), 
            time_horizon(biographical), 
            exit_options(mobile), 
            spatial_scope(national))).

% PERSPECTIVE 3: THE CHIEF ECONOMIST (TANGLED ROPE)
% Detects vulnerabilities bubbling under the surface where coordination 
% and extraction are inextricably linked.
constraint_indexing:constraint_classification(global_stimulus_spree, tangled_rope, 
    context(agent_power(analytical), 
            time_horizon(historical), 
            exit_options(analytical), 
            spatial_scope(global))) :-
    domain_priors:base_extractiveness(global_stimulus_spree, E), E >= 0.46.

% PERSPECTIVE 4: THE CAMPAIGN STRATEGIST (SCAFFOLD)
% Temporary support provided to voters before snap elections or transitions.
constraint_indexing:constraint_classification(global_stimulus_spree, scaffold, 
    context(agent_power(organized), 
            time_horizon(immediate), 
            exit_options(constrained), 
            spatial_scope(regional))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(stimulus_tests).

test(perspectival_gap) :-
    % Verify the stimulus is a Snare for the powerless but a Rope for the institution.
    constraint_indexing:constraint_classification(global_stimulus_spree, snare, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(global_stimulus_spree, rope, context(agent_power(institutional), _, _, _)).

test(threshold_validation) :-
    domain_priors:base_extractiveness(global_stimulus_spree, E),

    E >= 0.46. 

:- end_tests(stimulus_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The extraction score (0.62) is driven by the fact that global public 
 * debt is set to exceed 100% of GDP by 2029. The Perspectival Gap exists 
 * because politicians see Ropes of growth, while economists see Snares of 
 * historical interest payments.
 * * MANDATROPHY ANALYSIS:
 * [RESOLVED MANDATROPHY]
 * The Tangled Rope classification prevents the system from mislabeling this as 
 * pure extraction; it acknowledges that the spending targets genuine collective 
 * action problems like aging populations and energy transitions.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_ai_productivity,
    'Will AI investments yield enough productivity growth to outpace the debt interest spiral?',
    'Analysis of GDP-to-Interest ratios in U.S. and Germany by 2030.',
    'Success = Re-classification as Rope; Failure = Hard collapse into kinetic Snare.',
    confidence_without_resolution(low)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
% Tracks the surge from the pandemic explosion to the 2029 IMF projections.
narrative_ontology:interval(global_stimulus_spree, 2020, 2029). 

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */

