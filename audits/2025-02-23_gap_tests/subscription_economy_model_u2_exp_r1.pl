% ============================================================================
% CONSTRAINT STORY: subscription_economy_model_u2_exp_r1
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-05-21
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
 *   fees. This model presents a genuine coordination benefit for producers by
 *   creating predictable revenue streams to fund ongoing development and
 *   service. However, it simultaneously functions as a highly extractive
 *   mechanism by revoking consumer ownership, exploiting inertia, creating
 *   high friction for cancellation (a 'hassle tax'), and using technical
 *   (DRM) and market power to suppress non-subscription alternatives.
 *
 * KEY AGENTS:
 *   - Individual Consumers: Primary target (powerless/trapped) — bear the costs of subscription fatigue, loss of ownership, and cancellation friction.
 *   - Subscription-based Corporations: Primary beneficiary (institutional/arbitrage) — benefit from predictable recurring revenue and increased customer lifetime value.
 *   - Software Developers: Internal agents (moderate/mobile) — execute the strategy, often caught between user value and business metrics.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(subscription_economy_model_u2_exp_r1, 0.68).
domain_priors:suppression_score(subscription_economy_model_u2_exp_r1, 0.72).
domain_priors:theater_ratio(subscription_economy_model_u2_exp_r1, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(subscription_economy_model_u2_exp_r1, extractiveness, 0.68).
narrative_ontology:constraint_metric(subscription_economy_model_u2_exp_r1, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(subscription_economy_model_u2_exp_r1, theater_ratio, 0.45).

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

% From the consumer's view, the loss of ownership, recurring costs for previously-owned goods, and high friction for cancellation constitute a Snare. Alternatives (perpetual licenses) are actively suppressed.
constraint_indexing:constraint_classification(subscription_economy_model_u2_exp_r1, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% For the corporation, this model is a pure coordination solution (Rope) that smooths revenue streams, enables continuous development funding, and increases shareholder value. The extractive component is framed as payment for ongoing service.
constraint_indexing:constraint_classification(subscription_economy_model_u2_exp_r1, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% The analytical view recognizes both the genuine coordination function (stable revenue for continuous service) and the highly asymmetric extraction (consumer inertia, loss of ownership, cancellation friction). This duality is the hallmark of a Tangled Rope.
constraint_indexing:constraint_classification(subscription_economy_model_u2_exp_r1, tangled_rope,
    context(agent_power(analytical),
            time_horizon(generational),
            exit_options(analytical),
            spatial_scope(global))).

% For developers within these corporations, the model can feel like a Piton. The original goal of funding continuous improvement is often replaced by metrics-chasing (subscriber count, churn reduction) that becomes theatrical and disconnected from creating user value.
constraint_indexing:constraint_classification(subscription_economy_model_u2_exp_r1, piton,
    context(agent_power(moderate),
            time_horizon(immediate),
            exit_options(mobile),
            spatial_scope(national))).

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

test(piton_threshold) :-
    domain_priors:theater_ratio(subscription_economy_model_u2_exp_r1, TR),
    TR >= 0.70.

:- end_tests(subscription_economy_model_u2_exp_r1_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The high extractiveness (0.68) reflects the conversion of a one-time capital expense for an owned asset into a perpetual rental liability for the consumer. The high suppression (0.72) is justified by the active phasing out of perpetual license options in major software markets (e.g., Adobe, Microsoft Office) and the use of DRM to enforce the terms of the subscription, preventing offline use or ownership.
 *
 * PERSPECTIVAL GAP:
 *   The gap is stark. Corporations view the model as a pure Rope, a superior coordination mechanism for aligning revenue with ongoing costs. Consumers, stripped of ownership and facing a growing burden of recurring payments for tools they once owned outright, experience it as a Snare. The analytical classification of Tangled Rope is necessary to hold both truths at once.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is unambiguous. Value flows from the 'individual_consumers' (victims) to the 'subscription_corporations' (beneficiaries). The consumers' trapped exit options and powerless status maximize their derived directionality (d≈1.0), leading to high effective extraction (χ). The corporations' arbitrage options (they can acquire firms, pivot models) and beneficiary status give them a low directionality (d≈0.0), making the constraint a net subsidy for them.
 *
 * MANDATROPHY ANALYSIS:
 *   Classifying this as a Tangled Rope correctly avoids two errors. It is not a pure Rope, because the extraction is asymmetric and coercive, not just a fee for service. It is not a pure Snare, because there is a non-trivial coordination function being served (funding for continuous updates and cloud services). The Tangled Rope classification captures the essential conflict: a coordination mechanism has been weaponized for extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    inertia_vs_coercion,
    'Is the high customer retention rate primarily due to genuine product value or is it an artifact of coercive design (e.g., cancellation friction, bundling, data lock-in)?',
    'Comparative analysis of churn rates between services with easy vs. difficult cancellation processes, controlling for product category.',
    'If primarily value-driven, the constraint leans more towards Rope. If primarily coercive, it is a clear Snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(inertia_vs_coercion, empirical, 'Distinguishing between value-driven retention and coercive retention via 'hassle tax'.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(subscription_economy_model_u2_exp_r1, 2010, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(subs_tr_t2010, subscription_economy_model_u2_exp_r1, theater_ratio, 2010, 0.2).
narrative_ontology:measurement(subs_tr_t2017, subscription_economy_model_u2_exp_r1, theater_ratio, 2017, 0.35).
narrative_ontology:measurement(subs_tr_t2024, subscription_economy_model_u2_exp_r1, theater_ratio, 2024, 0.45).

% Extraction over time
narrative_ontology:measurement(subs_be_t2010, subscription_economy_model_u2_exp_r1, base_extractiveness, 2010, 0.3).
narrative_ontology:measurement(subs_be_t2017, subscription_economy_model_u2_exp_r1, base_extractiveness, 2017, 0.55).
narrative_ontology:measurement(subs_be_t2024, subscription_economy_model_u2_exp_r1, base_extractiveness, 2024, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(subscription_economy_model_u2_exp_r1, resource_allocation).
narrative_ontology:affects_constraint(subscription_economy_model_u2_exp_r1, right_to_repair).
narrative_ontology:affects_constraint(subscription_economy_model_u2_exp_r1, consumer_debt_burden).
narrative_ontology:affects_constraint(subscription_economy_model_u2_exp_r1, digital_asset_ownership).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
