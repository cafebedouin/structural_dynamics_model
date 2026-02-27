% ============================================================================
% CONSTRAINT STORY: subscription_economy_model
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-01-08
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_subscription_economy_model, []).

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
 *   constraint_id: subscription_economy_model
 *   human_readable: The Subscription Economy Model
 *   domain: economic/technological
 *
 * SUMMARY:
 *   The subscription economy model represents a widespread shift in business
 *   strategy, where companies offer products and services on a recurring
 *   subscription basis rather than through one-time purchases. This model
 *   creates recurring revenue streams for businesses while granting consumers
 *   ongoing access. However, it also introduces potential for extractive
 *   practices due to vendor lock-in and dark patterns making cancellation
 *   difficult.
 *
 * KEY AGENTS:
 *   - Subscription Service Providers: Beneficiary (institutional/arbitrage) - companies offering subscriptions.
 *   - Platform Operators: Beneficiary (institutional/arbitrage) - platforms enabling subscription services.
 *   - Individual Consumers: Victim (powerless/trapped) - individuals subscribing to services.
 *   - Small Businesses: Victim (moderate/constrained) - businesses relying on subscription services.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(subscription_economy_model, 0.55).
domain_priors:suppression_score(subscription_economy_model, 0.45).
domain_priors:theater_ratio(subscription_economy_model, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(subscription_economy_model, extractiveness, 0.55).
narrative_ontology:constraint_metric(subscription_economy_model, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(subscription_economy_model, theater_ratio, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(subscription_economy_model, tangled_rope).
narrative_ontology:human_readable(subscription_economy_model, "The Subscription Economy Model").
narrative_ontology:topic_domain(subscription_economy_model, "economic/technological").

domain_priors:requires_active_enforcement(subscription_economy_model).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(subscription_economy_model, subscription_service_providers).
narrative_ontology:constraint_beneficiary(subscription_economy_model, platform_operators).
narrative_ontology:constraint_victim(subscription_economy_model, individual_consumers).
narrative_ontology:constraint_victim(subscription_economy_model, small_businesses).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: Individual Consumer (SNARE) - Consumers often find themselves locked into subscriptions they no longer need or use, facing dark patterns that make cancellation difficult, representing a pure extraction scenario. Limited exit options due to convenience lock-in and platform effects.
constraint_indexing:constraint_classification(subscription_economy_model, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: Small Business (TANGLED ROPE) - Small businesses rely on subscription software and services, creating operational dependencies. They benefit from access to advanced tools, but are also vulnerable to price increases and vendor lock-in, representing a mixed coordination and extraction dynamic.
constraint_indexing:constraint_classification(subscription_economy_model, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: Subscription Service Providers (ROPE) - Service providers experience the subscription model as a coordination mechanism, enabling predictable revenue streams and long-term customer relationships. They can arbitrage pricing strategies and scale efficiently, making this classification a coordination dynamic.
constraint_indexing:constraint_classification(subscription_economy_model, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: Platform Operators (TANGLED ROPE) - Platforms enable and profit from subscription-based services. They extract a percentage from each subscription, but also coordinate access and customer acquisition, reflecting a mixed coordination and extraction.
constraint_indexing:constraint_classification(subscription_economy_model, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: Analytical Observer (TANGLED ROPE) - From a broad, long-term perspective, the subscription model is a mixed system with extraction (consumers pay more over time, vendor lock-in) and coordination (access to updated services, predictable revenue). Requires active enforcement through marketing, platform effects and contractual agreements.
constraint_indexing:constraint_classification(subscription_economy_model, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(subscription_economy_model_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(subscription_economy_model, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(subscription_economy_model, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(subscription_economy_model, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(subscription_economy_model_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The subscription economy has aspects of both coordination and extraction. Businesses gain predictable revenue and long-term relationships, but consumers risk overpaying for unused services and face difficulty switching. The extractiveness score (0.55) reflects this balance, while the suppression score (0.45) indicates a moderate level of lock-in and difficulty in switching. The theater ratio is low (0.20), signifying less performative activity and more genuine ongoing value.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap arises from the differing positions of service providers and consumers. Providers see a beneficial coordination mechanism, while consumers can experience it as extractive. Small businesses sit in the middle, constrained by their dependence on these subscriptions but also benefiting from access to tools. The analytical observer views the system as a tangled rope because it requires active enforcement via marketing and platform lock-in.
 *
 * DIRECTIONALITY LOGIC:
 *   Service providers benefit from predictable revenue and increased customer lifetime value (low d). Individual consumers bear the cost of ongoing payments and potential lock-in (high d). The derived d values are influenced by the varying exit options (arbitrage for providers, trapped for some consumers).
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy here is avoiding mischaracterization: The model isn't purely beneficial nor exploitative. Classifying it as Tangled Rope acknowledges the coordination benefit of recurring revenue and ongoing service alongside the extraction of consumer payments, vendor lock-in, and potential for dark patterns. Individual perspectives are then resolved to the extent that they are structural to that index.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    consumer_lock_in_intensity,
    'How easily can consumers switch between competing subscription services?',
    'Analyze switching costs: contract terms, data portability, and competitive landscape.',
    'High switching costs: increased extraction, shifts towards Snare. Low switching costs: more coordination, shifts towards Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(consumer_lock_in_intensity, empirical, 'Intensity of consumer lock-in affects extraction level.').

omega_variable(
    service_value_proposition,
    'Does the subscription provide ongoing value and updates, or is it a static product sold under a recurring fee?',
    'Evaluate service updates, new features, and customer satisfaction data.',
    'High ongoing value: increased coordination, shifts towards Rope. Static product: increased extraction, shifts towards Snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(service_value_proposition, empirical, 'Value delivered by subscription impacts coordination vs. extraction.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(subscription_economy_model, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(subs_tr_t0, subscription_economy_model, theater_ratio, 0, 0.1).
narrative_ontology:measurement(subs_tr_t5, subscription_economy_model, theater_ratio, 5, 0.15).
narrative_ontology:measurement(subs_tr_t10, subscription_economy_model, theater_ratio, 10, 0.2).

% Extraction over time
narrative_ontology:measurement(subs_be_t0, subscription_economy_model, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(subs_be_t5, subscription_economy_model, base_extractiveness, 5, 0.45).
narrative_ontology:measurement(subs_be_t10, subscription_economy_model, base_extractiveness, 10, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(subscription_economy_model, resource_allocation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
