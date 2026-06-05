% ============================================================================
% CONSTRAINT STORY: subscription_economy_model_u2_sed_r4
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-05-21
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_subscription_economy_model_u2_sed_r4, []).

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
 *   constraint_id: subscription_economy_model_u2_sed_r4
 *   human_readable: The Subscription Economy Model
 *   domain: economic/technological
 *
 * SUMMARY:
 *   This constraint describes the widespread business model shift from
 *   one-time product sales (perpetual licenses) to recurring subscription
 *   fees. This model presents a genuine coordination function by funding
 *   continuous development and providing ongoing service access. However, it
 *   simultaneously functions as a highly extractive mechanism by revoking
 *   ownership rights, creating consumer inertia, making cancellation
 *   difficult (a 'hassle tax'), and locking users into ecosystems. The model
 *   is actively enforced by Digital Rights Management (DRM) and the strategic
 *   suppression of non-subscription alternatives.
 *
 * KEY AGENTS:
 *   - Individual Consumers: Primary target (powerless/trapped) — bear the costs of subscription fatigue, loss of ownership, and cancellation friction.
 *   - Subscription-based Corporations: Primary beneficiary (institutional/arbitrage) — benefit from predictable recurring revenue and increased customer lifetime value.
 *   - Second-hand Market Participants: Secondary victim (powerless/trapped) — eliminated by the non-transferability of subscription licenses.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(subscription_economy_model_u2_sed_r4, 0.68).
domain_priors:suppression_score(subscription_economy_model_u2_sed_r4, 0.72).
domain_priors:theater_ratio(subscription_economy_model_u2_sed_r4, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(subscription_economy_model_u2_sed_r4, extractiveness, 0.68).
narrative_ontology:constraint_metric(subscription_economy_model_u2_sed_r4, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(subscription_economy_model_u2_sed_r4, theater_ratio, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(subscription_economy_model_u2_sed_r4, tangled_rope).
narrative_ontology:human_readable(subscription_economy_model_u2_sed_r4, "The Subscription Economy Model").
narrative_ontology:topic_domain(subscription_economy_model_u2_sed_r4, "economic/technological").

domain_priors:requires_active_enforcement(subscription_economy_model_u2_sed_r4).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(subscription_economy_model_u2_sed_r4, subscription_based_corporations).
narrative_ontology:constraint_beneficiary(subscription_economy_model_u2_sed_r4, venture_capital_investors).
narrative_ontology:constraint_victim(subscription_economy_model_u2_sed_r4, individual_consumers).
narrative_ontology:constraint_victim(subscription_economy_model_u2_sed_r4, second_hand_market_participants).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% For the individual consumer, the model is a snare due to high switching costs, cancellation friction, and the elimination of ownership alternatives.
constraint_indexing:constraint_classification(subscription_economy_model_u2_sed_r4, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% For the corporation, this is a pure coordination rope that smooths revenue streams and deepens customer relationships.
constraint_indexing:constraint_classification(subscription_economy_model_u2_sed_r4, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% The analytical view recognizes both the coordination function (service delivery) and the asymmetric extraction (loss of ownership, inertia exploitation).
constraint_indexing:constraint_classification(subscription_economy_model_u2_sed_r4, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% A professional or small business owner who can choose some non-subscription tools but is still constrained by industry standards sees a tangled rope.
constraint_indexing:constraint_classification(subscription_economy_model_u2_sed_r4, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(subscription_economy_model_u2_sed_r4_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(subscription_economy_model_u2_sed_r4, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(subscription_economy_model_u2_sed_r4, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(subscription_economy_model_u2_sed_r4, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(subscription_economy_model_u2_sed_r4_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The high extractiveness (0.68) reflects the conversion of a capital good (software, media) into a rental service, capturing all future value for the provider. The high suppression (0.72) comes from the active phasing out of perpetual licenses and the use of DRM to prevent offline use or resale, effectively removing alternatives. The model requires active enforcement via terms of service and technological locks, fulfilling a key requirement for a Tangled Rope.
 *
 * PERSPECTIVAL GAP:
 *   A significant gap exists between the corporation and the consumer. The corporation sees a Rope: a rational system for funding ongoing development and smoothing revenue. The consumer experiences a Snare: a system that exploits behavioral biases (inertia, forgetfulness) to create a recurring charge that is difficult to escape and which strips them of the asset equity they once had in a purchased product.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality is clear: value flows from a broad base of individual consumers (victims) to a concentrated group of corporations and their investors (beneficiaries). Consumers pay continuously for access without building equity, while corporations secure a predictable, long-term revenue stream that is highly valued by financial markets.
 *
 * MANDATROPHY ANALYSIS:
 *   Classifying this as a Tangled Rope correctly avoids two errors. It is not a pure Rope, because that would ignore the significant, non-consensual extraction from consumers via lock-in and friction. It is not a pure Snare, because that would ignore the genuine coordination benefit of funding continuous updates and cloud services. The Tangled Rope classification captures this duality of function and extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    service_vs_ownership_value,
    'Does the value of continuous updates and services provided by subscriptions outweigh the value lost from the elimination of perpetual ownership and the friction of recurring costs?',
    'Comparative total cost of ownership (TCO) analysis between subscription and hypothetical perpetual models, adjusted for feature velocity, support longevity, and consumer surplus.',
    'If service value is high and TCO is favorable, the constraint shifts towards Rope. If extraction dominates, it reinforces the Snare classification for victims.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(service_vs_ownership_value, empirical, 'Whether the service value justifies the loss of ownership and recurring fees.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(subscription_economy_model_u2_sed_r4, 2010, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(subs_tr_t0, subscription_economy_model_u2_sed_r4, theater_ratio, 0, 0.15).
narrative_ontology:measurement(subs_tr_t7, subscription_economy_model_u2_sed_r4, theater_ratio, 7, 0.3).
narrative_ontology:measurement(subs_tr_t14, subscription_economy_model_u2_sed_r4, theater_ratio, 14, 0.4).

% Extraction over time
narrative_ontology:measurement(subs_be_t0, subscription_economy_model_u2_sed_r4, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(subs_be_t7, subscription_economy_model_u2_sed_r4, base_extractiveness, 7, 0.55).
narrative_ontology:measurement(subs_be_t14, subscription_economy_model_u2_sed_r4, base_extractiveness, 14, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(subscription_economy_model_u2_sed_r4, resource_allocation).
narrative_ontology:affects_constraint(subscription_economy_model_u2_sed_r4, right_to_repair_movement).
narrative_ontology:affects_constraint(subscription_economy_model_u2_sed_r4, digital_asset_ownership).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
