% ============================================================================
% CONSTRAINT STORY: silver_scarcity_mountain_2026
% ============================================================================
% Version: 3.4 (Deferential Realism Core)
% Logic: 3.3 (Indexed Tuple P,T,E,S)
% Generated: 2026-02-06
% ============================================================================

:- module(constraint_silver_scarcity, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

% --- Namespace Hooks ---
:- multifile
    domain_priors:base_extractiveness/2,
    domain_priors:suppression_score/2,
    domain_priors:theater_ratio/2,
    domain_priors:requires_active_enforcement/1,
    narrative_ontology:has_sunset_clause/1,
    narrative_ontology:interval/3,
    narrative_ontology:measurement/5,
    narrative_ontology:constraint_metric/3,
    narrative_ontology:constraint_beneficiary/2,
    narrative_ontology:constraint_victim/2,
    narrative_ontology:constraint_claim/2,
    constraint_indexing:constraint_classification/3.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * * constraint_id: silver_scarcity_2026
 * human_readable: The Silver Physical Scarcity Mountain
 * domain: economic/industrial/geopolitical
 * * SUMMARY:
 * By February 2026, silver has been designated a "Critical Mineral" by the USGS. 
 * A structural deficit of ~95-200 million ounces—the sixth consecutive year of shortfall—
 * has transformed the silver market from a speculative arena into a physical Mountain. 
 * Because 75% of silver is mined as a byproduct (copper/lead/zinc), the supply is 
 * price-inelastic, creating an irreducible limit for the green energy transition.
 * * KEY AGENTS:
 * - PV/EV Manufacturers: Subject (Powerless) - Unable to substitute silver in high-conductive paths.
 * - Sovereign Entities: Beneficiary (Institutional) - Stockpiling strategic reserves.
 * - Market Analysts: Auditor (Analytical) - Observing the 528M oz paper-to-113M oz physical disconnect.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Numerical anchors for v3.4 thresholds
domain_priors:base_extractiveness(silver_scarcity_2026, 0.04). % Low extraction as it is a natural/physical limit.
domain_priors:suppression_score(silver_scarcity_2026, 0.95).   % Near-total suppression of alternatives (no substitutes).
domain_priors:theater_ratio(silver_scarcity_2026, 0.40).       % Primarily functional scarcity, though strategic theater is rising.

% Metric keys for the system
narrative_ontology:constraint_metric(silver_scarcity_2026, extractiveness, 0.04).
narrative_ontology:constraint_metric(silver_scarcity_2026, suppression_requirement, 0.95).
narrative_ontology:constraint_metric(silver_scarcity_2026, theater_ratio, 0.40).

% Constraint classification claim
narrative_ontology:constraint_claim(silver_scarcity_2026, mountain).

% Identification of extraction targets (Manufacturers facing the "Shortage")
narrative_ontology:constraint_victim(silver_scarcity_2026, industrial_consumers).
narrative_ontology:constraint_beneficiary(silver_scarcity_2026, strategic_stockpilers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × π(P) × σ(S)
   ========================================================================== */

% PERSPECTIVE 1: THE INDUSTRIAL USER (MOUNTAIN)
% Effective Extraction: 0.04 * 1.5 (powerless) * 1.2 (global) = 0.072.
% χ <= 0.05 is the threshold for Mountain. The physical limit is felt as an unchangeable law.
constraint_indexing:constraint_classification(silver_scarcity_2026, mountain, 
    context(agent_power(powerless), 
            time_horizon(generational), 
            exit_options(trapped), 
            spatial_scope(global))).

% PERSPECTIVE 2: THE SOVEREIGN STATE (MOUNTAIN/ROPE)
% Viewed as a strategic foundation for national security.
constraint_indexing:constraint_classification(silver_scarcity_2026, mountain, 
    context(agent_power(institutional), 
            time_horizon(civilizational), 
            exit_options(mobile), 
            spatial_scope(global))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER
constraint_indexing:constraint_classification(silver_scarcity_2026, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(silver_scarcity_2026_tests).

test(mountain_verification) :-
    % Mountains must have low effective extraction (<= 0.05) and high suppression.
    narrative_ontology:constraint_metric(silver_scarcity_2026, extractiveness, E),
    domain_priors:suppression_score(silver_scarcity_2026, S),
    E =< 0.05,
    S >= 0.90.

test(substitution_suppression) :-
    % Verify that industrial agents are 'trapped' with no alternatives.
    constraint_indexing:constraint_classification(silver_scarcity_2026, mountain, context(_, _, exit_options(trapped), _)).

:- end_tests(silver_scarcity_2026_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The Silver Scarcity is classified as a Mountain because its primary 
 * driver is a multi-year physical production peak (2016) colliding with 
 * exponential industrial demand. Unlike a Snare, the 
 * "extraction" isn't a policy choice but a physical limit ($E=0.04$). 
 * The extreme suppression (0.95) reflects the fact that silver is the 
 * most conductive element; substitution in solar PV or AI semiconductors 
 * results in performance failure.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    omega_byproduct_elasticity,
    'Will a massive surge in copper/zinc mining (due to high prices) resolve the silver deficit?',
    'Review of 2026 global base-metal capex and byproduct silver yield.',
    'Elastic supply reverts the Mountain to a Rope; Inelasticity hardens the Mountain.',
    confidence_without_resolution(low)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(silver_scarcity_2026, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio (Tracking the shift from market trade to strategic stockpiling)
narrative_ontology:measurement(sv_tr_t0, silver_scarcity_2026, theater_ratio, 0, 0.25).
narrative_ontology:measurement(sv_tr_t5, silver_scarcity_2026, theater_ratio, 5, 0.40).
narrative_ontology:measurement(sv_tr_t10, silver_scarcity_2026, theater_ratio, 10, 0.55).

% Base Extraction (Remains low as this is a physical constraint, not a rent-seeking one)
narrative_ontology:measurement(sv_be_t0, silver_scarcity_2026, base_extractiveness, 0, 0.04).
narrative_ontology:measurement(sv_be_t5, silver_scarcity_2026, base_extractiveness, 5, 0.04).
narrative_ontology:measurement(sv_be_t10, silver_scarcity_2026, base_extractiveness, 10, 0.05).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
