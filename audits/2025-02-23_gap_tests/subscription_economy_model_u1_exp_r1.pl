% ============================================================================
% CONSTRAINT STORY: subscription_economy_model_u1_exp_r1
% ============================================================================
% Version: 2.1 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2023-10-27
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_subscription_economy_model_u1_exp_r1, []).

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
 *   constraint_id: subscription_economy_model_u1_exp_r1
 *   human_readable: Subscription Economy Model
 *   domain: economic/technological
 *
 * SUMMARY:
 *   This constraint describes the widespread business model shift from
 *   one-time product sales (perpetual licenses) to recurring subscription
 *   fees. While offering a coordination benefit (continuous updates, service
 *   access), it asymmetrically extracts value by creating consumer inertia,
 *   making cancellation difficult (a 'hassle tax'), and revoking ownership
 *   rights. The model is actively enforced by DRM and the suppression of
 *   non-subscription alternatives.
 *
 * KEY AGENTS:
 *   - Individual Consumers: Primary target (powerless/trapped) — bear the costs of subscription fatigue, loss of ownership, and cancellation friction.
 *   - Subscription-based Corporations: Primary beneficiary (institutional/arbitrage) — benefit from predictable recurring revenue and increased customer lifetime value.
 *   - Regulatory Bodies: Potential moderator (organized/constrained) — can influence the constraint through consumer protection laws.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(subscription_economy_model_u1_exp_r1, 0.68).
domain_priors:suppression_score(subscription_economy_model_u1_exp_r1, 0.72).
domain_priors:theater_ratio(subscription_economy_model_u1_exp_r1, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(subscription_economy_model_u1_exp_r1, extractiveness, 0.68).
narrative_ontology:constraint_metric(subscription_economy_model_u1_exp_r1, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(subscription_economy_model_u1_exp_r1, theater_ratio, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(subscription_economy_model_u1_exp_r1, tangled_rope).
narrative_ontology:human_readable(subscription_economy_model_u1_exp_r1, "Subscription Economy Model").
narrative_ontology:topic_domain(subscription_economy_model_u1_exp_r1, "economic/technological").

domain_priors:requires_active_enforcement(subscription_economy_model_u1_exp_r1).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(subscription_economy_model_u1_exp_r1, subscription_corporations).
narrative_ontology:constraint_victim(subscription_economy_model_u1_exp_r1, individual_consumers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% From the consumer's view, the model is a trap. Ownership is revoked, costs are perpetual, and high friction cancellation plus a lack of alternatives makes exit nearly impossible.
constraint_indexing:constraint_classification(subscription_economy_model_u1_exp_r1, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% For the corporation, this is a pure coordination mechanism to smooth revenue, fund continuous development, and maintain a direct service relationship with customers.
constraint_indexing:constraint_classification(subscription_economy_model_u1_exp_r1, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% The analytical view recognizes both the coordination function (service delivery) and the asymmetric extraction (consumer lock-in, loss of ownership), classifying it as a Tangled Rope.
constraint_indexing:constraint_classification(subscription_economy_model_u1_exp_r1, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% A regulator might view the model as a temporary scaffold for industry growth, intending to later impose rules (e.g., 'one-click cancel') that reduce its extractive potential.
constraint_indexing:constraint_classification(subscription_economy_model_u1_exp_r1, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(subscription_economy_model_u1_exp_r1_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(subscription_economy_model_u1_exp_r1, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(subscription_economy_model_u1_exp_r1, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(subscription_economy_model_u1_exp_r1, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(subscription_economy_model_u1_exp_r1_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The high extractiveness (0.68) reflects the transfer of value from consumers (who lose asset ownership and pay in perpetuity) to corporations (who gain predictable revenue streams). The high suppression (0.72) is due to the active phasing out of perpetual license alternatives by market leaders and the use of DRM to enforce the terms of the subscription, locking users into the ecosystem.
 *
 * PERSPECTIVAL GAP:
 *   The gap is stark: corporations see an efficient Rope for delivering value and managing revenue, while consumers experience a Snare of endless payments for products they can never own, with high barriers to exit. The analytical Tangled Rope classification captures this duality.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is clear. Corporations are the beneficiaries, designing and enforcing the system for their financial benefit (d -> 0). Consumers are the victims, bearing the financial burden and loss of autonomy (d -> 1). Their trapped exit options and powerless status maximize the effective extraction they experience.
 *
 * MANDATROPHY ANALYSIS:
 *   The Tangled Rope classification is crucial for avoiding mandatrophy. A Snare classification would ignore the genuine coordination function of providing continuous updates and services. A Rope classification would ignore the coercive extraction, loss of ownership, and high suppression. Tangled Rope correctly identifies that a legitimate service is being used as a vehicle for asymmetric, non-consensual value extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    subscription_value_vs_rent,
    'Is the recurring fee a fair price for continuous service and updates, or is it rent-seeking on a captured user base with suppressed alternatives?',
    'Comparative analysis of perpetual license TCO vs. subscription TCO, coupled with data on cancellation friction and user churn rates when friction is removed.',
    'If deemed fair value for service, it leans towards Rope. If deemed rent-seeking, it solidifies as a Snare from most non-beneficiary perspectives.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(subscription_value_vs_rent, empirical, 'Distinguishing between fair service pricing and rent-seeking on a captured user base.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(subscription_economy_model_u1_exp_r1, 2008, 2023).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(subs_tr_t0, subscription_economy_model_u1_exp_r1, theater_ratio, 0, 0.15).
narrative_ontology:measurement(subs_tr_t8, subscription_economy_model_u1_exp_r1, theater_ratio, 8, 0.3).
narrative_ontology:measurement(subs_tr_t15, subscription_economy_model_u1_exp_r1, theater_ratio, 15, 0.45).

% Extraction over time
narrative_ontology:measurement(subs_be_t0, subscription_economy_model_u1_exp_r1, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(subs_be_t8, subscription_economy_model_u1_exp_r1, base_extractiveness, 8, 0.55).
narrative_ontology:measurement(subs_be_t15, subscription_economy_model_u1_exp_r1, base_extractiveness, 15, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(subscription_economy_model_u1_exp_r1, resource_allocation).
narrative_ontology:affects_constraint(subscription_economy_model_u1_exp_r1, digital_right_to_repair).
narrative_ontology:affects_constraint(subscription_economy_model_u1_exp_r1, consumer_data_portability).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
