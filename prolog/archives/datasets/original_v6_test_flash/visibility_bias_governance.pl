% ============================================================================
% CONSTRAINT STORY: visibility_bias_governance
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-01-04
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_visibility_bias_governance, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

% --- Constraint Identity Rule (DP-001: ε-Invariance) ---
% Each constraint story must have a single, stable base extractiveness (ε).
% If changing the observable used to evaluate this constraint would change ε,
% you are looking at two distinct constraints. Write separate .pl files for
% each, link them with affects_constraint/2, and document the relationship
% in both files' narrative context sections.
%
% The context tuple is CLOSED at arity 4: (P, T, E, S).
% Do not add measurement_basis, beneficiary/victim, or any other arguments.
% Linter Rule 23 enforces context/4.
%
% See: epsilon_invariance_principle.md

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: visibility_bias_governance
 *   human_readable: The Dashboard Delusion
 *   domain: political/institutional
 *
 * SUMMARY:
 *   A governance failure where policy is driven exclusively by
 *   high-visibility metrics (e.g., stock indices, surface-level crime stats)
 *   while ignoring low-visibility systemic decay (e.g., infrastructure
 *   entropy, trust decay).
 *
 * KEY AGENTS:
 *   - Incumbent Politicians: Primary beneficiary (institutional/arbitrage) - benefit from short-term gains
 *   - Media Outlets: Secondary beneficiary (powerful/mobile) - benefit from easily digestible narratives
 *   - Long-Term Institutional Health: Primary victim (powerless/trapped) - cannot advocate for itself
 *   - Future Generations: Secondary victim (powerless/trapped) - bear the cost of deferred maintenance
 *   - Informed Public: Moderate actor (moderate/constrained) - constrained by limited data, but can exert some influence
 *   - Traditional Watchdog Institutions: Institutional actor (institutional/constrained) - constrained by political pressure
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(visibility_bias_governance, 0.6).
domain_priors:suppression_score(visibility_bias_governance, 0.7).
domain_priors:theater_ratio(visibility_bias_governance, 0.8).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(visibility_bias_governance, extractiveness, 0.6).
narrative_ontology:constraint_metric(visibility_bias_governance, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(visibility_bias_governance, theater_ratio, 0.8).

% --- Constraint claim ---
narrative_ontology:constraint_claim(visibility_bias_governance, tangled_rope).
narrative_ontology:human_readable(visibility_bias_governance, "The Dashboard Delusion").
narrative_ontology:topic_domain(visibility_bias_governance, "political/institutional").

domain_priors:requires_active_enforcement(visibility_bias_governance).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(visibility_bias_governance, incumbent_politicians).
narrative_ontology:constraint_beneficiary(visibility_bias_governance, media_outlets).
narrative_ontology:constraint_victim(visibility_bias_governance, long_term_institutional_health).
narrative_ontology:constraint_victim(visibility_bias_governance, future_generations).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Future generations are trapped by the neglect of long-term systemic health, with no ability to exit or influence current policy decisions. They bear the full cost of deferred maintenance and institutional decay.
constraint_indexing:constraint_classification(visibility_bias_governance, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% Incumbent politicians benefit from focusing on visible metrics that can be quickly improved to demonstrate success during their term, enabling arbitrage of short-term gains for long-term costs. They can exit problematic policies by passing the buck to future administrations.
constraint_indexing:constraint_classification(visibility_bias_governance, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% The informed public is constrained by the limited availability of comprehensive data and the dominance of easily digestible metrics, but benefits from the political system as a whole and can exert some influence through voting and advocacy. They face significant extraction due to their limited ability to change policy.
constraint_indexing:constraint_classification(visibility_bias_governance, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% Traditional watchdog institutions (e.g., Government Accountability Office) are increasingly constrained by political pressure and resource limitations. The theater ratio is high as they performatively maintain their oversight function, but lack the power to effectively challenge the dominance of visible metrics.
constraint_indexing:constraint_classification(visibility_bias_governance, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% The analytical observer sees the tangled rope nature of the dashboard delusion, recognizing the coordination function (short-term political stability) intertwined with asymmetric extraction (long-term institutional decay). This perspective understands the structural incentives that drive the focus on visible metrics.
constraint_indexing:constraint_classification(visibility_bias_governance, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(visibility_bias_governance_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(visibility_bias_governance, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(visibility_bias_governance, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(visibility_bias_governance, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(visibility_bias_governance, TR),
    TR >= 0.70.

:- end_tests(visibility_bias_governance_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.60): High. The focus on visible metrics extracts resources and attention from long-term investments in institutional health. Suppression (0.70): High. The dominance of visible metrics suppresses alternative policy approaches that prioritize long-term sustainability. Theater ratio (0.80): Very High. Political actions are often performative, aimed at improving visible metrics rather than addressing underlying problems.
 *
 * PERSPECTIVAL GAP:
 *   Future generations are trapped in a snare, bearing the long-term consequences of short-sighted policies. Incumbent politicians benefit from a rope, using visible metrics to demonstrate short-term success. The informed public is tangled in a rope, constrained by limited data and the dominance of visible metrics. Watchdog institutions operate as pitons, performatively maintaining oversight without effectively challenging the status quo. The analytical observer recognizes the tangled rope nature of the governance failure.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (incumbent politicians, media outlets) experience low or negative directionality. Victims (future generations, long-term institutional health) experience high directionality. The informed public experiences moderate directionality due to their limited ability to influence policy.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    discount_rate_sensitivity,
    'How sensitive are policy decisions to the discount rate applied to future costs and benefits?',
    'Conduct policy simulations with varying discount rates to assess the impact on investment decisions related to long-term infrastructure and institutional health.',
    'If highly sensitive, policies will consistently favor short-term gains over long-term sustainability. If insensitive, policies are more likely to prioritize future benefits.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(discount_rate_sensitivity, empirical, 'Sensitivity of policy decisions to the discount rate.').

omega_variable(
    metric_capture_vulnerability,
    'How vulnerable are high-visibility metrics to manipulation and gaming by political actors?',
    'Analyze historical trends in metric performance following policy interventions to identify instances of metric capture and unintended consequences.',
    'If highly vulnerable, policies will be driven by manipulated data, leading to further institutional decay. If resilient, policies are more likely to achieve their intended outcomes.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(metric_capture_vulnerability, empirical, 'Vulnerability of high-visibility metrics to manipulation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(visibility_bias_governance, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(visi_tr_t0, visibility_bias_governance, theater_ratio, 0, 0.4).
narrative_ontology:measurement(visi_tr_t5, visibility_bias_governance, theater_ratio, 5, 0.6).
narrative_ontology:measurement(visi_tr_t10, visibility_bias_governance, theater_ratio, 10, 0.8).

% Extraction over time
narrative_ontology:measurement(visi_be_t0, visibility_bias_governance, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(visi_be_t5, visibility_bias_governance, base_extractiveness, 5, 0.45).
narrative_ontology:measurement(visi_be_t10, visibility_bias_governance, base_extractiveness, 10, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
