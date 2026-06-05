% ============================================================================
% CONSTRAINT STORY: subscription_economy_model_u2_sed_r3
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2023-10-27
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_subscription_economy_model_u2_sed_r3, []).

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
 *   constraint_id: subscription_economy_model_u2_sed_r3
 *   human_readable: The Subscription Economy Model
 *   domain: economic/technological
 *
 * SUMMARY:
 *   This constraint describes the widespread business model shift from
 *   one-time product sales (perpetual licenses) to recurring subscription
 *   fees. While offering a coordination benefit (continuous updates, service
 *   access), it asymmetrically extracts value by creating consumer inertia,
 *   making cancellation difficult (a 'hassle tax'), and revoking ownership
 *   rights. The model is actively enforced by Digital Rights Management (DRM)
 *   and the suppression of non-subscription alternatives.
 *
 * KEY AGENTS:
 *   - Individual Consumers: Primary target (powerless/trapped) — bear the costs of subscription fatigue, loss of ownership, and cancellation friction.
 *   - Subscription-based Corporations: Primary beneficiary (institutional/arbitrage) — benefit from predictable recurring revenue and increased customer lifetime value.
 *   - Investors/Venture Capital: Secondary beneficiary (institutional/arbitrage) — strongly favor the predictable growth model of subscription services.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(subscription_economy_model_u2_sed_r3, 0.68).
domain_priors:suppression_score(subscription_economy_model_u2_sed_r3, 0.72).
domain_priors:theater_ratio(subscription_economy_model_u2_sed_r3, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(subscription_economy_model_u2_sed_r3, extractiveness, 0.68).
narrative_ontology:constraint_metric(subscription_economy_model_u2_sed_r3, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(subscription_economy_model_u2_sed_r3, theater_ratio, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(subscription_economy_model_u2_sed_r3, tangled_rope).
narrative_ontology:human_readable(subscription_economy_model_u2_sed_r3, "The Subscription Economy Model").
narrative_ontology:topic_domain(subscription_economy_model_u2_sed_r3, "economic/technological").

domain_priors:requires_active_enforcement(subscription_economy_model_u2_sed_r3).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(subscription_economy_model_u2_sed_r3, subscription_based_corporations).
narrative_ontology:constraint_beneficiary(subscription_economy_model_u2_sed_r3, venture_capital_investors).
narrative_ontology:constraint_victim(subscription_economy_model_u2_sed_r3, individual_consumers).
narrative_ontology:constraint_victim(subscription_economy_model_u2_sed_r3, small_businesses).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% For the end-user, the lack of perpetual alternatives, high cancellation friction, and cumulative cost makes this a snare.
constraint_indexing:constraint_classification(subscription_economy_model_u2_sed_r3, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% For the corporation, this is a pure coordination mechanism to smooth revenue and manage service delivery, with negative effective extraction.
constraint_indexing:constraint_classification(subscription_economy_model_u2_sed_r3, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% The analytical view recognizes both the coordination function (continuous updates) and the asymmetric extraction (loss of ownership, inertia exploitation).
constraint_indexing:constraint_classification(subscription_economy_model_u2_sed_r3, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% A moderate actor with some resources can switch providers, but the pervasiveness of the model and switching costs still result in high effective extraction.
constraint_indexing:constraint_classification(subscription_economy_model_u2_sed_r3, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(subscription_economy_model_u2_sed_r3_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(subscription_economy_model_u2_sed_r3, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(subscription_economy_model_u2_sed_r3, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(subscription_economy_model_u2_sed_r3, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(subscription_economy_model_u2_sed_r3_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness (0.68) is high due to the conversion of ownership into rental, locking consumers into perpetual payment streams for tools they previously owned. Suppression (0.72) is high because corporations actively phase out perpetual license options and use DRM to prevent offline use or ownership, effectively removing alternatives.
 *
 * PERSPECTIVAL GAP:
 *   A significant gap exists between the consumer and the corporation. The consumer experiences a Snare, trapped by high switching costs, a lack of alternatives, and 'dark patterns' designed to make cancellation difficult. The corporation views it as a Rope, a superior coordination model for delivering value and securing predictable revenue. The analytical observer sees a Tangled Rope, acknowledging the real coordination function but also the coercive and extractive structure built around it.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality is clear: value flows from consumers and small businesses (victims) to the corporations and their investors (beneficiaries). The former lose asset ownership and are subject to price hikes, while the latter gain a stable, high-margin revenue stream.
 *
 * MANDATROPHY ANALYSIS:
 *   The Tangled Rope classification is crucial for avoiding mandatrophy. Labeling this model a pure Snare would ignore the genuine coordination benefits (e.g., cloud software updates) that make it attractive. Labeling it a Rope would ignore the immense, non-consensual extraction from captive users. The Tangled Rope correctly identifies it as a hybrid system where a coordination function is used to legitimize and enable a highly extractive architecture.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    subscription_value_vs_inertia,
    'Is the recurring revenue primarily derived from the continuous value provided (updates, service) or from exploiting consumer inertia and cancellation friction (the 'hassle tax')?',
    'Analysis of user engagement data vs. churn rates correlated with cancellation process complexity and the availability of perpetual alternatives.',
    'If primarily value-driven, it trends towards a Tangled Rope. If primarily inertia-driven, it is a pure Snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(subscription_value_vs_inertia, empirical, 'Distinguishing between value delivery and inertia exploitation in subscription models.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(subscription_economy_model_u2_sed_r3, 2008, 2023).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(subs_tr_t0, subscription_economy_model_u2_sed_r3, theater_ratio, 0, 0.15).
narrative_ontology:measurement(subs_tr_t8, subscription_economy_model_u2_sed_r3, theater_ratio, 8, 0.3).
narrative_ontology:measurement(subs_tr_t15, subscription_economy_model_u2_sed_r3, theater_ratio, 15, 0.45).

% Extraction over time
narrative_ontology:measurement(subs_be_t0, subscription_economy_model_u2_sed_r3, base_extractiveness, 0, 0.2).
narrative_ontology:measurement(subs_be_t8, subscription_economy_model_u2_sed_r3, base_extractiveness, 8, 0.55).
narrative_ontology:measurement(subs_be_t15, subscription_economy_model_u2_sed_r3, base_extractiveness, 15, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(subscription_economy_model_u2_sed_r3, resource_allocation).
narrative_ontology:affects_constraint(subscription_economy_model_u2_sed_r3, right_to_repair_movement).
narrative_ontology:affects_constraint(subscription_economy_model_u2_sed_r3, digital_asset_ownership).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
