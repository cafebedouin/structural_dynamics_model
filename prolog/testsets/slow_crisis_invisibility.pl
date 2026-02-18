% ============================================================================
% CONSTRAINT STORY: slow_crisis_invisibility
% ============================================================================
% Version: 5.2 (Deferential Realism Core + Boltzmann + Purity + Network)
% Logic: 5.2 (Indexed Tuple P,T,E,S + Coupling + Purity + Network Drift)
% Generated: 2024-05-21
% ============================================================================

:- module(constraint_slow_crisis_invisibility, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * * constraint_id: slow_crisis_invisibility
 * human_readable: Generational Baseline Shift
 * domain: social/environmental
 * * SUMMARY:
 * This constraint describes the structural inability of short-term political
 * systems to perceive or respond to crises with decay rates slower than a
 * single biographical lifespan. It functions as a Snare for the future
 * while appearing as a Mountain (unalterable nature) to the present.
 * * KEY AGENTS:
 * - Current Citizen: Subject (Powerless)
 * - Legacy Institution: Beneficiary (Institutional)
 * - Historical Modeler: Auditor (Analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% High extraction (0.75) because it consumes the optionality of future generations.
domain_priors:base_extractiveness(slow_crisis_invisibility, 0.75).
domain_priors:suppression_score(slow_crisis_invisibility, 0.60).
domain_priors:theater_ratio(slow_crisis_invisibility, 0.20).

% Constraint metric facts — primary keys used by the classification engine.
narrative_ontology:constraint_metric(slow_crisis_invisibility, extractiveness, 0.75).
narrative_ontology:constraint_metric(slow_crisis_invisibility, suppression_requirement, 0.60).
narrative_ontology:constraint_metric(slow_crisis_invisibility, theater_ratio, 0.20).

% Constraint self-claim: It presents as an unchangeable fact of human nature/cognition.
narrative_ontology:constraint_claim(slow_crisis_invisibility, snare).
narrative_ontology:human_readable(slow_crisis_invisibility, "Generational Baseline Shift").
narrative_ontology:topic_domain(slow_crisis_invisibility, "social/environmental").

% Binary flags and structural properties for Tangled Rope classification.
domain_priors:requires_active_enforcement(slow_crisis_invisibility). % Enforcement is via short-term incentive structures (e.g., election cycles).

% Structural property derivation hooks for Tangled Rope.
narrative_ontology:constraint_beneficiary(slow_crisis_invisibility, legacy_institutions).
narrative_ontology:constraint_victim(slow_crisis_invisibility, future_generations).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × π(P) × σ(S)
   ========================================================================== */

% PERSPECTIVE 1: THE SUBJECT (SNARE)
% To an individual with a short time horizon, the slow crisis is invisible, but
% its effects (e.g., degraded environment, reduced opportunity) are felt as a
% high-extraction trap. It feels like a Mountain ("just how the world is") but
% is metrically a Snare. χ = 0.75 * 1.5 * 1.0 = 1.125.
constraint_indexing:constraint_classification(slow_crisis_invisibility, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE BENEFICIARY (ROPE)
% Institutions use the invisibility to maintain current coordination and social
% stability, avoiding the "cost" of radical transition. χ = 0.75 * -0.2 * 1.2 = -0.18.
constraint_indexing:constraint_classification(slow_crisis_invisibility, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (SNARE)
% When viewed across historical time, the invisibility is a predatory trap
% that extracts resources from the future to pay for the present. χ = 0.75 * 1.15 * 1.2 = 1.035.
constraint_indexing:constraint_classification(slow_crisis_invisibility, snare,
    context(agent_power(analytical),
            time_horizon(historical),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 4: THE SYSTEM AUDITOR (TANGLED ROPE)
% Detects both the coordination function (beneficiaries exist) and the massive
% asymmetric extraction (victims exist), plus the active enforcement.
constraint_indexing:constraint_classification(slow_crisis_invisibility, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(slow_crisis_invisibility_tests).

test(perspectival_gap) :-
    % Verify the gap: powerless see a Snare, while beneficiaries see a Rope.
    constraint_indexing:constraint_classification(slow_crisis_invisibility, snare,
        context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(slow_crisis_invisibility, rope,
        context(agent_power(institutional), _, _, _)).

test(threshold_validation) :-
    domain_priors:base_extractiveness(slow_crisis_invisibility, E),
    (E =< 0.15 ; E >= 0.46). % Ensure it's not in the ambiguous middle range.

test(tangled_rope_conditions_met) :-
    % Verify that all structural conditions for Tangled Rope are declared.
    narrative_ontology:constraint_beneficiary(slow_crisis_invisibility, _),
    narrative_ontology:constraint_victim(slow_crisis_invisibility, _),
    domain_priors:requires_active_enforcement(slow_crisis_invisibility).

:- end_tests(slow_crisis_invisibility_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The 0.75 extraction score represents the high degree of optionality taken
 * from future agents who are 'trapped' by the decisions of the present.
 * The suppression score of 0.60 reflects the structural difficulty of
 * implementing long-term policies within short-term political and economic cycles.
 *
 * * PERSPECTIVAL GAP:
 * The 'powerless' agent, trapped within a biographical time horizon, cannot
 * perceive the slow-moving causal chain. They experience the negative outcomes
 * as an inescapable trap (Snare), even if it feels as immutable as a Mountain.
 * The 'institutional' beneficiary, however, experiences the status quo as a
 * beneficial coordination mechanism (Rope) that ensures stability and predictability.
 *
 * * [RESOLVED MANDATROPHY]:
 * This resolution is achieved by the Tangled Rope classification. It acknowledges
 * that while extraction is severe, the constraint's invisibility serves a
 * "coordination" function by preventing social panic or costly disruption in
 * the short term, benefiting current institutions at the expense of future ones.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% Required because extraction (0.75) > 0.46
omega_variable(
    omega_baseline_shift,
    'Is the invisibility a cognitive hardware limit or a cultural software choice?',
    'Long-term immersive simulation of historical ecological baselines for cohorts.',
    'If cognitive: Mountain of Biology. If cultural: Snare of Education.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(slow_crisis_invisibility, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Temporal data for high-extraction constraint (0.75 > 0.46).
% Models the slow accumulation of the crisis over the interval. Extraction
% increases as the consequences of inaction compound. Theater remains low but
% creeps up as minor, ineffective gestures are made.

% Theater ratio over time (triggers metric_substitution detection):
narrative_ontology:measurement(sci_tr_t0, slow_crisis_invisibility, theater_ratio, 0, 0.10).
narrative_ontology:measurement(sci_tr_t5, slow_crisis_invisibility, theater_ratio, 5, 0.15).
narrative_ontology:measurement(sci_tr_t10, slow_crisis_invisibility, theater_ratio, 10, 0.20).

% Extraction over time (triggers extraction_accumulation detection):
narrative_ontology:measurement(sci_ex_t0, slow_crisis_invisibility, base_extractiveness, 0, 0.50).
narrative_ontology:measurement(sci_ex_t5, slow_crisis_invisibility, base_extractiveness, 5, 0.65).
narrative_ontology:measurement(sci_ex_t10, slow_crisis_invisibility, base_extractiveness, 10, 0.75).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA (v5.0-5.2)
   ========================================================================== */

% The coordination function is maintaining present stability by managing resources.
narrative_ontology:coordination_type(slow_crisis_invisibility, resource_allocation).

% This type of structural blindness enables other extractive constraints.
narrative_ontology:affects_constraint(slow_crisis_invisibility, fossil_fuel_dependency).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */