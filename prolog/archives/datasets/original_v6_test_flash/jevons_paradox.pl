% ============================================================================
% CONSTRAINT STORY: jevons_paradox
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-02-29
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_jevons_paradox, []).

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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: jevons_paradox
 *   human_readable: Jevons Paradox (The Rebound Effect)
 *   domain: economic/technological
 *
 * SUMMARY:
 *   Jevons Paradox occurs when technological progress increases the
 *   efficiency with which a resource is used, but the rate of consumption of
 *   that resource rises because of increasing demand. This increased demand
 *   often negates the benefits of the increased efficiency. It represents a
 *   complex interplay of technological advancement, consumer behavior, and
 *   resource economics.
 *
 * KEY AGENTS:
 *   - Consumers: Moderate power, constrained exit. Benefit from lower prices but are constrained by resource availability.
 *   - Technology Producers: Institutional power, arbitrage exit. Benefit from increased demand for their technologies.
 *   - Environmental Commons: Powerless, trapped exit. Suffers from increased resource consumption.
 *   - Future Generations: Powerless, trapped exit. Inherit depleted resources and degraded environment.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jevons_paradox, 0.55).
domain_priors:suppression_score(jevons_paradox, 0.4).
domain_priors:theater_ratio(jevons_paradox, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jevons_paradox, extractiveness, 0.55).
narrative_ontology:constraint_metric(jevons_paradox, suppression_requirement, 0.4).
narrative_ontology:constraint_metric(jevons_paradox, theater_ratio, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jevons_paradox, tangled_rope).
narrative_ontology:human_readable(jevons_paradox, "Jevons Paradox (The Rebound Effect)").
narrative_ontology:topic_domain(jevons_paradox, "economic/technological").

domain_priors:requires_active_enforcement(jevons_paradox).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jevons_paradox, consumers).
narrative_ontology:constraint_beneficiary(jevons_paradox, technology_producers).
narrative_ontology:constraint_victim(jevons_paradox, environmental_commons).
narrative_ontology:constraint_victim(jevons_paradox, future_generations).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Perspective 1: Environmental Commons (Snare). The environmental commons, lacking agency, is trapped and bears the brunt of increased resource consumption. No exit option; powerless to counteract the paradox.
constraint_indexing:constraint_classification(jevons_paradox, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% Perspective 2: Technology Producers (Rope). Technology producers benefit from increased demand for their efficiency-enhancing technologies. They experience the Jevons Paradox as a positive feedback loop, driving further innovation and sales. Arbitrage opportunities abound.
constraint_indexing:constraint_classification(jevons_paradox, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% Perspective 3: Consumers (Tangled Rope). Consumers benefit from lower per-unit resource costs due to increased efficiency, but are constrained by the overall increase in consumption. They experience both benefits (lower costs) and costs (environmental degradation).
constraint_indexing:constraint_classification(jevons_paradox, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% Perspective 4: Analytical Observer (Tangled Rope). Analyzes the system holistically, recognizing both the coordination (increased efficiency) and extraction (increased consumption) aspects of the Jevons Paradox. Sees the long-term implications for resource depletion and environmental damage.
constraint_indexing:constraint_classification(jevons_paradox, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jevons_paradox_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(jevons_paradox, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(jevons_paradox, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(jevons_paradox, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(jevons_paradox_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The Jevons Paradox is classified as a Tangled Rope because it involves both coordination (increased efficiency) and extraction (increased consumption). Extractiveness is rated at 0.55 because the overall effect leads to greater resource depletion. Suppression is at 0.40 as the market forces drive the consumer behavior. The Theater Ratio is low because there is not really a significant theatrical performance component.
 *
 * PERSPECTIVAL GAP:
 *   Technology Producers see the Jevons Paradox as a benefit (Rope), while the Environmental Commons experience it as a loss (Snare). Consumers experience a mix of benefits and costs (Tangled Rope). The Analytical Observer attempts to understand the systemic implications (Tangled Rope).
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality is derived from the beneficiaries and victims. Technology producers benefit (low d), environmental commons are harmed (high d). Consumers experience a mixed effect, with directionality values between 0 and 1 based on the elasticity of demand and their capacity for exit (shifting to less resource-intensive consumption).
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    demand_elasticity,
    'How elastic is the demand for the resource in question? High elasticity exacerbates the rebound effect.',
    'Empirical analysis of demand curves and consumer behavior in response to price changes.',
    'If demand is highly elastic, the Jevons Paradox is more likely to occur and be more severe. If inelastic, the rebound effect is weaker.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(demand_elasticity, empirical, 'Elasticity of demand for the resource.').

omega_variable(
    consumer_preferences,
    'To what extent do consumer preferences favor resource-intensive lifestyles?',
    'Sociological studies of consumer attitudes, values, and consumption patterns.',
    'If consumers prioritize resource-intensive goods and services, the Jevons Paradox is more pronounced. If preferences shift towards sustainability, the rebound effect can be mitigated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(consumer_preferences, preference, 'Consumer preferences impacting resource consumption.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jevons_paradox, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jevo_tr_t0, jevons_paradox, theater_ratio, 0, 0.1).
narrative_ontology:measurement(jevo_tr_t5, jevons_paradox, theater_ratio, 5, 0.15).
narrative_ontology:measurement(jevo_tr_t10, jevons_paradox, theater_ratio, 10, 0.2).

% Extraction over time
narrative_ontology:measurement(jevo_be_t0, jevons_paradox, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(jevo_be_t5, jevons_paradox, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(jevo_be_t10, jevons_paradox, base_extractiveness, 10, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jevons_paradox, resource_allocation).
narrative_ontology:affects_constraint(jevons_paradox, resource_depletion).
narrative_ontology:affects_constraint(jevons_paradox, climate_change).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
