% ============================================================================
% CONSTRAINT STORY: subscription_economy_model_u4_exp_r2
% ============================================================================
% Version: 2.1 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-29
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_subscription_economy_model_u4_exp_r2, []).

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
 *   constraint_id: subscription_economy_model_u4_exp_r2
 *   human_readable: Subscription Economy Model
 *   domain: economic/technological
 *
 * SUMMARY:
 *   This constraint describes the widespread business model shift from
 *   one-time product sales (perpetual licenses) to recurring subscription
 *   fees. This model provides a coordination benefit by funding continuous
 *   updates and service access. However, it asymmetrically extracts value by
 *   revoking ownership rights, creating consumer inertia and data lock-in,
 *   and imposing cancellation friction. The model is actively enforced by
 *   Digital Rights Management (DRM) and the systematic suppression of
 *   non-subscription alternatives in the market.
 *
 * KEY AGENTS:
 *   - Individual Consumers: Primary target (powerless/trapped) — bear the costs of subscription fatigue, loss of ownership, and cancellation friction.
 *   - Subscription-based Corporations: Primary beneficiary (institutional/arbitrage) — benefit from predictable recurring revenue and increased customer lifetime value.
 *   - Regulatory Bodies: Potential secondary actor (institutional/constrained) — often unable to effectively regulate dark patterns or anti-consumer terms of service.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(subscription_economy_model_u4_exp_r2, 0.68).
domain_priors:suppression_score(subscription_economy_model_u4_exp_r2, 0.72).
domain_priors:theater_ratio(subscription_economy_model_u4_exp_r2, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(subscription_economy_model_u4_exp_r2, extractiveness, 0.68).
narrative_ontology:constraint_metric(subscription_economy_model_u4_exp_r2, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(subscription_economy_model_u4_exp_r2, theater_ratio, 0.25).

% --- Constraint claim ---
narrative_ontology:constraint_claim(subscription_economy_model_u4_exp_r2, tangled_rope).
narrative_ontology:human_readable(subscription_economy_model_u4_exp_r2, "Subscription Economy Model").
narrative_ontology:topic_domain(subscription_economy_model_u4_exp_r2, "economic/technological").

domain_priors:requires_active_enforcement(subscription_economy_model_u4_exp_r2).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(subscription_economy_model_u4_exp_r2, subscription_corporations).
narrative_ontology:constraint_victim(subscription_economy_model_u4_exp_r2, individual_consumers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% From the consumer's perspective, the model is a snare. Exit is costly due to data lock-in, loss of access, and a lack of perpetual alternatives. The recurring cost feels extractive, especially when features are not used.
constraint_indexing:constraint_classification(subscription_economy_model_u4_exp_r2, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% For the corporation, this is a pure coordination mechanism (Rope) for delivering continuous service and securing predictable revenue. The negative effective extraction (chi < 0) reflects the immense financial and strategic benefit.
constraint_indexing:constraint_classification(subscription_economy_model_u4_exp_r2, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% The analytical view recognizes both the genuine coordination function (continuous updates, cloud services) and the asymmetric extraction (loss of ownership, high lifetime cost, cancellation friction), classifying it as a Tangled Rope.
constraint_indexing:constraint_classification(subscription_economy_model_u4_exp_r2, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(subscription_economy_model_u4_exp_r2_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(subscription_economy_model_u4_exp_r2, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(subscription_economy_model_u4_exp_r2, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(subscription_economy_model_u4_exp_r2, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(subscription_economy_model_u4_exp_r2_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The high extractiveness (0.68) stems from the conversion of a finite purchase into a potentially infinite revenue stream, coupled with the non-monetized loss of ownership and control. The high suppression (0.72) reflects the active removal of perpetual license options from the market and the use of DRM to enforce compliance. The model requires active enforcement to prevent users from retaining access after a subscription lapses, which is a key gate for the Tangled Rope classification.
 *
 * PERSPECTIVAL GAP:
 *   The gap is stark: corporations view the model as a Rope, a superior coordination method for service delivery and customer relations. Consumers, trapped by high switching costs and a lack of alternatives, experience it as a Snare, a coercive system that extracts rent indefinitely. The analytical view must account for both realities.
 *
 * DIRECTIONALITY LOGIC:
 *   Corporations are the clear beneficiaries, gaining predictable, high-margin revenue. Their arbitrage exit option allows them to selectively apply this model to maximize profit. Consumers are the victims, as they are structurally trapped and their directionality (d) is driven towards 1.0, maximizing the effective extraction (chi) they experience.
 *
 * MANDATROPHY ANALYSIS:
 *   The Tangled Rope classification is critical here. It prevents the mislabeling of this system as a pure Rope (the corporate claim), by acknowledging the high, enforced extraction and suppression. It also avoids a pure Snare classification, which would ignore the genuine (if overpriced) coordination function of providing ongoing services and updates. The model's success relies on bundling these two functions inextricably.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    ownership_vs_access,
    'Is the loss of perpetual ownership a necessary trade-off for continuous service and updates, or is it a purely extractive measure to maximize customer lifetime value?',
    'Comparative analysis of total cost of ownership (TCO) for subscription vs. perpetual models, adjusted for feature velocity, security patching, and service uptime.',
    'If deemed a necessary trade-off, the base extractiveness score would decrease, potentially shifting the classification towards Rope. If purely extractive, it reinforces the Snare classification for consumers.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ownership_vs_access, empirical, 'Ambiguity between necessary service cost and extractive rent-seeking in revoking ownership.').

omega_variable(
    cancellation_friction_intent,
    'Is the difficulty in cancelling subscriptions a result of administrative overhead or an intentional 'hassle tax' designed to retain customers against their will?',
    'Analysis of user interface design patterns, customer support transcripts, and internal corporate documents related to churn reduction strategies.',
    'If intentional, this directly increases the suppression score. If unintentional, it is a correctable flaw rather than a structural feature of the constraint.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(cancellation_friction_intent, empirical, 'Whether cancellation difficulty is an intentional suppression tactic or incidental friction.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(subscription_economy_model_u4_exp_r2, 2010, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(subs_tr_t0, subscription_economy_model_u4_exp_r2, theater_ratio, 0, 0.1).
narrative_ontology:measurement(subs_tr_t8, subscription_economy_model_u4_exp_r2, theater_ratio, 8, 0.2).
narrative_ontology:measurement(subs_tr_t15, subscription_economy_model_u4_exp_r2, theater_ratio, 15, 0.25).

% Extraction over time
narrative_ontology:measurement(subs_be_t0, subscription_economy_model_u4_exp_r2, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(subs_be_t8, subscription_economy_model_u4_exp_r2, base_extractiveness, 8, 0.55).
narrative_ontology:measurement(subs_be_t15, subscription_economy_model_u4_exp_r2, base_extractiveness, 15, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(subscription_economy_model_u4_exp_r2, resource_allocation).
narrative_ontology:affects_constraint(subscription_economy_model_u4_exp_r2, digital_rights_management).
narrative_ontology:affects_constraint(subscription_economy_model_u4_exp_r2, consumer_debt_cycles).
narrative_ontology:affects_constraint(subscription_economy_model_u4_exp_r2, right_to_repair).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
