% ============================================================================
% CONSTRAINT STORY: TAIWAN_GRAND_BARGAIN
% ============================================================================
% Version: 3.4 (Deferential Realism Core)
% Logic: 3.3 (Indexed Tuple P,T,E,S)
% Generated: 2026-01-26
% ============================================================================

:- module(constraint_grand_bargain, []).

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
 * * constraint_id: taiwan_grand_bargain
 * human_readable: The U.S.-China Taiwan Grand Bargain
 * domain: economic/geopolitical
 * * SUMMARY:
 * A potential diplomatic framework where the U.S. trades its endorsement of
 * China’s position on Taiwan for significant economic benefits, such as
 * massive domestic investment and trade concessions.
 * * KEY AGENTS:
 * - The Taiwanese Industry/Citizen: Subject (Powerless/Extracted)
 * - The U.S. Administration: Beneficiary (Institutional/Coordinating)
 * - The Geopolitical Analyst: Auditor (Analytical/Tangled Rope Detection)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Numerical anchors for v3.4 thresholds
% Extraction is high (0.65) due to the $250B investment requirement and
% the potential extraction of political autonomy.
domain_priors:base_extractiveness(taiwan_grand_bargain, 0.65).
domain_priors:suppression_score(taiwan_grand_bargain, 0.70).
domain_priors:theater_ratio(taiwan_grand_bargain, 0.50).

% Constraint metric facts (bridge for classification engine)
narrative_ontology:constraint_metric(taiwan_grand_bargain, extractiveness, 0.65).
narrative_ontology:constraint_metric(taiwan_grand_bargain, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(taiwan_grand_bargain, theater_ratio, 0.5).

% Binary flags
domain_priors:requires_active_enforcement(taiwan_grand_bargain). % Negotiated via tariffs/treaties.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE TAIWANESE SEMICONDUCTOR FIRM (SNARE)
% High extraction felt as a mandatory "pay-to-play" for market access,
% trading capital for a lower tariff environment (20% down to 15%).
constraint_indexing:constraint_classification(taiwan_grand_bargain, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE U.S. EXECUTIVE BRANCH (ROPE)
% Viewed as a master coordination mechanism to resolve trade deficits
% and secure U.S. tech leadership without military intervention.
constraint_indexing:constraint_classification(taiwan_grand_bargain, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% Detects the hybrid signature where genuine economic coordination ($250B
% investment) masks the extraction of security and sovereignty.
constraint_indexing:constraint_classification(taiwan_grand_bargain, tangled_rope,
    context(agent_power(analytical),
            time_horizon(historical),
            exit_options(analytical),
            spatial_scope(global))) :-
    domain_priors:base_extractiveness(taiwan_grand_bargain, E), E >= 0.50.

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(grand_bargain_tests).

test(perspectival_gap) :-
    % Verify the bargain is a Snare for the powerless firm but a Rope for the U.S. Gov.
    constraint_indexing:constraint_classification(taiwan_grand_bargain, snare, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(taiwan_grand_bargain, rope, context(agent_power(institutional), _, _, _)).

test(extraction_threshold) :-
    domain_priors:base_extractiveness(taiwan_grand_bargain, E),

    E >= 0.46. % Triggers high-extraction Snare/Tangled logic.

:- end_tests(grand_bargain_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The extraction score (0.65) reflects the massive forced investment and
 * the "conciliatory approach" that bypasses Taiwan's democratic agency.
 * The Perspectival Gap exists because the U.S. gains coordination (investment
 * and de-escalation) while Taiwan pays the premium in both capital and status.
 * * MANDATROPHY ANALYSIS:
 * [RESOLVED MANDATROPHY]
 * The Tangled Rope classification acknowledges that the bargain solves a
 * legitimate U.S.-China coordination problem (avoiding war), preventing
 * it from being dismissed as pure extraction despite the high costs
 * imposed on Taiwan.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_transactional_durability,
    'Is the economic bargain a stable coordination or a temporary lull before military action?',
    'Correlation of Taiwan chip investment milestones vs. PLA sortie frequency in 2026.',
    'Success = Permanent Tangled Rope; Failure = Collapse into a kinetic Snare.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
% Focuses on the trade deals and political shifts observed in 2025/2026.
narrative_ontology:interval(taiwan_grand_bargain, 2025, 2027).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
