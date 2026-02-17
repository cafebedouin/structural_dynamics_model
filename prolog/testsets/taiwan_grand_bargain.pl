% ============================================================================
% CONSTRAINT STORY: taiwan_grand_bargain
% ============================================================================
% Version: 5.2 (Deferential Realism Core + Boltzmann + Purity + Network)
% Logic: 5.2 (Indexed Tuple P,T,E,S + Coupling + Purity + Network Drift)
% Generated: 2024-05-21
% ============================================================================

:- module(constraint_taiwan_grand_bargain, []).

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
    narrative_ontology:interval/3,
    narrative_ontology:measurement/5,
    narrative_ontology:constraint_metric/3,
    narrative_ontology:constraint_beneficiary/2,
    narrative_ontology:constraint_victim/2,
    narrative_ontology:constraint_claim/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:human_readable/2.

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
 * massive domestic investment and trade concessions from China. This creates
 * a coordination benefit for the U.S. and China (de-escalation) while
 * extracting political autonomy and economic agency from Taiwan.
 * * KEY AGENTS:
 * - Taiwanese Industry/Citizen: Subject (Powerless)
 * - U.S. Administration & Beneficiary Corporations: Beneficiary (Institutional)
 * - Geopolitical Analyst: Auditor (Analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Numerical anchors for v3.4 thresholds
domain_priors:base_extractiveness(taiwan_grand_bargain, 0.65). % High extraction from forced investment and loss of political autonomy.
domain_priors:suppression_score(taiwan_grand_bargain, 0.70).   % High suppression as Taiwan has few alternatives to great power politics.
domain_priors:theater_ratio(taiwan_grand_bargain, 0.50).       % Significant functional components, but also high diplomatic theater.

% Constraint metric facts — primary keys used by the classification engine.
narrative_ontology:constraint_metric(taiwan_grand_bargain, extractiveness, 0.65).
narrative_ontology:constraint_metric(taiwan_grand_bargain, suppression_requirement, 0.70).
narrative_ontology:constraint_metric(taiwan_grand_bargain, theater_ratio, 0.50).

% Constraint self-claim (what does the constraint claim to be?)
narrative_ontology:constraint_claim(taiwan_grand_bargain, tangled_rope).
narrative_ontology:human_readable(taiwan_grand_bargain, "The U.S.-China Taiwan Grand Bargain").

% Binary flags
domain_priors:requires_active_enforcement(taiwan_grand_bargain). % Requires treaties, tariffs, and diplomatic pressure.

% Structural property derivation hooks for Tangled Rope:
narrative_ontology:constraint_beneficiary(taiwan_grand_bargain, us_administration).
narrative_ontology:constraint_victim(taiwan_grand_bargain, taiwanese_industry).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × π(P) × σ(S)
   ========================================================================== */

% PERSPECTIVE 1: THE TAIWANESE INDUSTRY (SNARE)
% χ = 0.65 * 1.5 (powerless) * 1.0 (national) = 0.975.
% A coercive trap demanding capital and political concessions for market access.
constraint_indexing:constraint_classification(taiwan_grand_bargain, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE U.S. EXECUTIVE BRANCH (ROPE)
% χ = 0.65 * -0.2 (institutional) * 1.0 (national) = -0.13.
% A net-positive coordination mechanism securing economic benefits and de-escalation.
constraint_indexing:constraint_classification(taiwan_grand_bargain, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% χ = 0.65 * 1.15 (analytical) * 1.2 (global) = 0.897.
% The observer sees the high effective extraction (0.897) and suppression (0.70)
% alongside a genuine coordination function (beneficiary exists) and asymmetric
% extraction (victim exists), classifying it as a Tangled Rope.
constraint_indexing:constraint_classification(taiwan_grand_bargain, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(taiwan_grand_bargain_tests).

test(perspectival_gap_snare_vs_rope) :-
    % Verify the bargain is a Snare for the powerless but a Rope for the institutional.
    constraint_indexing:constraint_classification(taiwan_grand_bargain, snare, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(taiwan_grand_bargain, rope, context(agent_power(institutional), _, _, _)),
    constraint_indexing:constraint_classification(taiwan_grand_bargain, tangled_rope, context(agent_power(analytical), _, _, _)).

test(tangled_rope_structural_requirements) :-
    % Verify all three structural requirements for Tangled Rope are met.
    domain_priors:requires_active_enforcement(taiwan_grand_bargain),
    narrative_ontology:constraint_beneficiary(taiwan_grand_bargain, _),
    narrative_ontology:constraint_victim(taiwan_grand_bargain, _).

test(extraction_threshold) :-
    narrative_ontology:constraint_metric(taiwan_grand_bargain, extractiveness, E),
    E >= 0.46. % Triggers high-extraction Snare/Tangled logic.

:- end_tests(taiwan_grand_bargain_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The extraction score (0.65) reflects the massive forced investment and
 * the "conciliatory approach" that bypasses Taiwan's democratic agency.
 * The Perspectival Gap is stark: the U.S. administration sees a brilliant Rope
 * that achieves coordination (investment, de-escalation) with negative perceived
 * cost (χ = -0.13), while Taiwan experiences it as a highly coercive Snare (χ = 0.975).
 * * MANDATROPHY ANALYSIS:
 * [RESOLVED MANDATROPHY]
 * The Tangled Rope classification is critical here. It acknowledges that the
 * bargain solves a legitimate U.S.-China coordination problem (avoiding war),
 * preventing the system from misclassifying it as a pure Snare. This captures
 * the dual nature of the constraint: it is both a coordination mechanism and
 * an instrument of asymmetric extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_taiwan_grand_bargain,
    'Is the economic bargain a stable coordination mechanism or a temporary lull before military action?',
    'Correlation of foreign direct investment milestones vs. PLA sortie frequency over a 5-year period.',
    'Success = Permanent Tangled Rope; Failure = Collapse into a kinetic Snare.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing. Normalized 10-year interval.
narrative_ontology:interval(taiwan_grand_bargain, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Models the bargain's evolution from a proposal to a fully enforced policy.
% Extraction and theater shift as the deal solidifies.
%
% Theater ratio over time:
narrative_ontology:measurement(tgb_tr_t0, taiwan_grand_bargain, theater_ratio, 0, 0.60).
narrative_ontology:measurement(tgb_tr_t5, taiwan_grand_bargain, theater_ratio, 5, 0.45).
narrative_ontology:measurement(tgb_tr_t10, taiwan_grand_bargain, theater_ratio, 10, 0.50).

% Extraction over time:
narrative_ontology:measurement(tgb_ex_t0, taiwan_grand_bargain, base_extractiveness, 0, 0.40).
narrative_ontology:measurement(tgb_ex_t5, taiwan_grand_bargain, base_extractiveness, 5, 0.55).
narrative_ontology:measurement(tgb_ex_t10, taiwan_grand_bargain, base_extractiveness, 10, 0.65).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA (v5.0-5.2)
   ========================================================================== */

% This is a geopolitical treaty enforced through economic policy.
narrative_ontology:coordination_type(taiwan_grand_bargain, enforcement_mechanism).

% This bargain would structurally alter the global semiconductor supply chain.
narrative_ontology:affects_constraint(taiwan_grand_bargain, semiconductor_supply_chain).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */