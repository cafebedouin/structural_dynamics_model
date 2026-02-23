% ============================================================================
% CONSTRAINT STORY: subscription_economy_model_u2_exp_r1
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2023-10-27
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_subscription_economy_model_u2_exp_r1, []).

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
 *   constraint_id: subscription_economy_model_u2_exp_r1
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
 *   - Market Regulators: Secondary actor (institutional/constrained) — attempt to mitigate the most extractive aspects (e.g., mandating easy cancellation) but operate within the dominant paradigm.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(subscription_economy_model_u2_exp_r1, 0.68).
domain_priors:suppression_score(subscription_economy_model_u2_exp_r1, 0.72).
domain_priors:theater_ratio(subscription_economy_model_u2_exp_r1, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(subscription_economy_model_u2_exp_r1, extractiveness, 0.68).
narrative_ontology:constraint_metric(subscription_economy_model_u2_exp_r1, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(subscription_economy_model_u2_exp_r1, theater_ratio, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(subscription_economy_model_u2_exp_r1, tangled_rope).
narrative_ontology:human_readable(subscription_economy_model_u2_exp_r1, "Subscription Economy Model").
narrative_ontology:topic_domain(subscription_economy_model_u2_exp_r1, "economic/technological").

domain_priors:requires_active_enforcement(subscription_economy_model_u2_exp_r1).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(subscription_economy_model_u2_exp_r1, subscription_corporations).
narrative_ontology:constraint_victim(subscription_economy_model_u2_exp_r1, individual_consumers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% From the consumer's perspective, the model is a trap. Ownership is revoked, costs are perpetual, and cancellation is often intentionally difficult, locking them into a system where alternatives are scarce.
constraint_indexing:constraint_classification(subscription_economy_model_u2_exp_r1, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% From the corporation's view, this is a superior coordination mechanism for aligning revenue with ongoing development and service costs, creating predictable cash flow and deeper customer relationships.
constraint_indexing:constraint_classification(subscription_economy_model_u2_exp_r1, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% The analytical view recognizes both the genuine coordination function (continuous service) and the asymmetric extraction (consumer inertia, lock-in, suppressed alternatives), classifying it as a Tangled Rope.
constraint_indexing:constraint_classification(subscription_economy_model_u2_exp_r1, tangled_rope,
    context(agent_power(analytical),
            time_horizon(generational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(subscription_economy_model_u2_exp_r1_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(subscription_economy_model_u2_exp_r1, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(subscription_economy_model_u2_exp_r1, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(subscription_economy_model_u2_exp_r1, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(subscription_economy_model_u2_exp_r1_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The high extractiveness (0.68) reflects the shift from a single capital outlay to a potentially infinite stream of payments, coupled with the loss of asset ownership. The high suppression (0.72) reflects the active phasing out of perpetual license models by major software, media, and hardware companies, leaving consumers with few alternatives. Active enforcement via DRM is critical to the model's function.
 *
 * PERSPECTIVAL GAP:
 *   The gap is stark. Corporations see a Rope, a model for sustainable service delivery and predictable revenue. Consumers experience a Snare, where they are trapped in a cycle of payments for services they may not fully use, with high friction to exit. The analytical view must account for both realities, hence Tangled Rope.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality is clear: value flows from a broad base of individual consumers (victims) to a concentrated set of corporations (beneficiaries). Consumers lose ownership and are subject to price increases and feature changes, while corporations gain a stable, predictable, and highly valuable revenue stream.
 *
 * MANDATROPHY ANALYSIS:
 *   The Tangled Rope classification is crucial for avoiding mandatrophy. A simple Snare classification would ignore the genuine coordination benefits (e.g., cloud services, security updates) that make the model attractive in the first place. A Rope classification would ignore the coercive and extractive elements that are fundamental to its profitability. The Tangled Rope correctly identifies that a coordination function is being used as a vehicle for asymmetric extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    consumer_value_vs_inertia,
    'Is the recurring revenue primarily justified by continuous value delivery, or is it extracted from consumer inertia and cancellation friction?',
    'Comparative analysis of user engagement data vs. churn rates, correlated with the measured difficulty of cancellation processes across major platforms.',
    'If primarily value-driven, the classification leans towards Tangled Rope. If primarily inertia-driven, it approaches a pure Snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(consumer_value_vs_inertia, empirical, 'Whether recurring revenue stems from delivered value or from exploiting consumer inertia.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(subscription_economy_model_u2_exp_r1, 2005, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(subs_tr_t0, subscription_economy_model_u2_exp_r1, theater_ratio, 0, 0.1).
narrative_ontology:measurement(subs_tr_t10, subscription_economy_model_u2_exp_r1, theater_ratio, 10, 0.2).
narrative_ontology:measurement(subs_tr_t20, subscription_economy_model_u2_exp_r1, theater_ratio, 20, 0.3).

% Extraction over time
narrative_ontology:measurement(subs_be_t0, subscription_economy_model_u2_exp_r1, base_extractiveness, 0, 0.2).
narrative_ontology:measurement(subs_be_t10, subscription_economy_model_u2_exp_r1, base_extractiveness, 10, 0.55).
narrative_ontology:measurement(subs_be_t20, subscription_economy_model_u2_exp_r1, base_extractiveness, 20, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(subscription_economy_model_u2_exp_r1, resource_allocation).
narrative_ontology:affects_constraint(subscription_economy_model_u2_exp_r1, right_to_repair).
narrative_ontology:affects_constraint(subscription_economy_model_u2_exp_r1, digital_ownership_rights).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
