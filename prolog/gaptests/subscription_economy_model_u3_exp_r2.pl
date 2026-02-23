% ============================================================================
% CONSTRAINT STORY: subscription_economy_model_u3_exp_r2
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-05-21
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_subscription_economy_model_u3_exp_r2, []).

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
 *   constraint_id: subscription_economy_model_u3_exp_r2
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
 *   - Regulators: Secondary actor (institutional/constrained) — attempt to mitigate the most extractive elements (e.g., cancellation laws) but operate within the established paradigm.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(subscription_economy_model_u3_exp_r2, 0.68).
domain_priors:suppression_score(subscription_economy_model_u3_exp_r2, 0.72).
domain_priors:theater_ratio(subscription_economy_model_u3_exp_r2, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(subscription_economy_model_u3_exp_r2, extractiveness, 0.68).
narrative_ontology:constraint_metric(subscription_economy_model_u3_exp_r2, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(subscription_economy_model_u3_exp_r2, theater_ratio, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(subscription_economy_model_u3_exp_r2, tangled_rope).
narrative_ontology:human_readable(subscription_economy_model_u3_exp_r2, "Subscription Economy Model").
narrative_ontology:topic_domain(subscription_economy_model_u3_exp_r2, "economic/technological").

domain_priors:requires_active_enforcement(subscription_economy_model_u3_exp_r2).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(subscription_economy_model_u3_exp_r2, subscription_based_corporations).
narrative_ontology:constraint_victim(subscription_economy_model_u3_exp_r2, individual_consumers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% From the consumer's perspective, the inability to own software and the friction of cancellation creates a trap where they perpetually pay for access, with high effective extraction.
constraint_indexing:constraint_classification(subscription_economy_model_u3_exp_r2, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% From the corporation's view, this is a superior coordination mechanism for managing revenue, development cycles, and customer relationships, with extraction being a feature, not a bug.
constraint_indexing:constraint_classification(subscription_economy_model_u3_exp_r2, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% The analytical view recognizes both the genuine coordination function (continuous updates, service delivery) and the highly asymmetric extraction enabled by suppressing ownership models.
constraint_indexing:constraint_classification(subscription_economy_model_u3_exp_r2, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(subscription_economy_model_u3_exp_r2_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(subscription_economy_model_u3_exp_r2, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(subscription_economy_model_u3_exp_r2, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(subscription_economy_model_u3_exp_r2, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(subscription_economy_model_u3_exp_r2_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The high extractiveness (0.68) reflects the conversion of a one-time capital expense into a perpetual operational expense for the consumer, capturing far more value over the product lifecycle. The high suppression (0.72) reflects the deliberate phasing out of perpetual license models in major markets (e.g., creative software, office suites), leaving consumers with no viable alternative.
 *
 * PERSPECTIVAL GAP:
 *   The gap is stark: corporations see a Rope that smooths revenue and aligns developer incentives with ongoing service. Consumers experience a Snare where they are trapped in a cycle of payments for tools they once could own, with the total cost of 'access' far exceeding the old cost of ownership.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality is unambiguous. Value flows from a broad base of individual consumers (victims) to a concentrated set of corporate platform owners (beneficiaries). The structure is designed to maximize this flow through recurring billing and high exit costs (both financial and in terms of workflow disruption).
 *
 * MANDATROPHY ANALYSIS:
 *   Classifying this as a pure Snare would miss the genuine coordination function it provides (e.g., cloud services, security updates). Classifying it as a Rope would ignore the coercive and highly extractive nature of eliminating ownership. The Tangled Rope classification is therefore essential, as it correctly identifies the hybrid nature of a system that provides a service while simultaneously locking in and extracting from its user base.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    consumer_value_vs_inertia,
    'Is the recurring revenue primarily justified by continuous value delivery (updates, services) or is it a capture of consumer inertia and cancellation friction?',
    'Comparative analysis of user engagement with new features vs. churn rates when cancellation processes are simplified ('one-click cancel' laws).',
    'If primarily value-driven, the extractiveness score would decrease, potentially shifting the classification toward Rope. If inertia-driven, it confirms the high suppression and Snare-like nature.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(consumer_value_vs_inertia, empirical, 'Distinguishing between value delivery and inertia capture in subscription models.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(subscription_economy_model_u3_exp_r2, 2010, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(subs_tr_t2010, subscription_economy_model_u3_exp_r2, theater_ratio, 2010, 0.15).
narrative_ontology:measurement(subs_tr_t2018, subscription_economy_model_u3_exp_r2, theater_ratio, 2018, 0.3).
narrative_ontology:measurement(subs_tr_t2025, subscription_economy_model_u3_exp_r2, theater_ratio, 2025, 0.4).

% Extraction over time
narrative_ontology:measurement(subs_be_t2010, subscription_economy_model_u3_exp_r2, base_extractiveness, 2010, 0.3).
narrative_ontology:measurement(subs_be_t2018, subscription_economy_model_u3_exp_r2, base_extractiveness, 2018, 0.55).
narrative_ontology:measurement(subs_be_t2025, subscription_economy_model_u3_exp_r2, base_extractiveness, 2025, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(subscription_economy_model_u3_exp_r2, resource_allocation).
narrative_ontology:affects_constraint(subscription_economy_model_u3_exp_r2, right_to_repair).
narrative_ontology:affects_constraint(subscription_economy_model_u3_exp_r2, consumer_debt_burden).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
