% ============================================================================
% CONSTRAINT STORY: subscription_economy_model_u2_sed_r5
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-05-21
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_subscription_economy_model_u2_sed_r5, []).

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
 *   constraint_id: subscription_economy_model_u2_sed_r5
 *   human_readable: The Subscription Economy Model
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
 *   - Investors: Secondary beneficiary (institutional/arbitrage) — reward companies with higher valuations for Monthly Recurring Revenue (MRR).
 *   - Regulators: Analytical/Institutional observer — attempts to mitigate the most extractive elements (e.g., mandating easy cancellation).
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(subscription_economy_model_u2_sed_r5, 0.68).
domain_priors:suppression_score(subscription_economy_model_u2_sed_r5, 0.75).
domain_priors:theater_ratio(subscription_economy_model_u2_sed_r5, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(subscription_economy_model_u2_sed_r5, extractiveness, 0.68).
narrative_ontology:constraint_metric(subscription_economy_model_u2_sed_r5, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(subscription_economy_model_u2_sed_r5, theater_ratio, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(subscription_economy_model_u2_sed_r5, tangled_rope).
narrative_ontology:human_readable(subscription_economy_model_u2_sed_r5, "The Subscription Economy Model").
narrative_ontology:topic_domain(subscription_economy_model_u2_sed_r5, "economic/technological").

domain_priors:requires_active_enforcement(subscription_economy_model_u2_sed_r5).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(subscription_economy_model_u2_sed_r5, subscription_corporations).
narrative_ontology:constraint_beneficiary(subscription_economy_model_u2_sed_r5, investors_valuing_mrr).
narrative_ontology:constraint_victim(subscription_economy_model_u2_sed_r5, individual_consumers).
narrative_ontology:constraint_victim(subscription_economy_model_u2_sed_r5, small_businesses_as_users).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% For the consumer, the loss of ownership, recurring costs, and high friction for cancellation make this a snare. Alternatives are systematically removed from the market.
constraint_indexing:constraint_classification(subscription_economy_model_u2_sed_r5, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% For the corporation, this is a pure coordination mechanism to smooth revenue, increase business valuation, and manage customer relationships.
constraint_indexing:constraint_classification(subscription_economy_model_u2_sed_r5, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% A moderate actor can choose some services but cannot escape the overall market structure. The cumulative cost and mental overhead feel extractive, hence a snare.
constraint_indexing:constraint_classification(subscription_economy_model_u2_sed_r5, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% The analytical view recognizes both the coordination function (continuous service) and the asymmetric extraction (loss of ownership, inertia exploitation), classifying it as a tangled rope.
constraint_indexing:constraint_classification(subscription_economy_model_u2_sed_r5, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(subscription_economy_model_u2_sed_r5_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(subscription_economy_model_u2_sed_r5, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(subscription_economy_model_u2_sed_r5, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(subscription_economy_model_u2_sed_r5, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(subscription_economy_model_u2_sed_r5_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The high extractiveness (0.68) reflects the fundamental shift from consumer ownership to corporate rental, capturing all future value. The high suppression (0.75) represents the active removal of perpetual-license alternatives from the market by major players, leaving consumers with no other choice. Active enforcement is true due to DRM and restrictive terms of service.
 *
 * PERSPECTIVAL GAP:
 *   The gap is stark: corporations see a Rope, a simple tool for predictable revenue and service delivery. Consumers experience a Snare, where they are trapped in ever-increasing recurring payments for tools they used to own, with high friction to escape. The analytical view of Tangled Rope acknowledges both realities.
 *
 * DIRECTIONALITY LOGIC:
 *   The flow of value is clear. It moves from consumers (victims), who lose assets and pay in perpetuity, to corporations and their investors (beneficiaries), who gain predictable, high-margin revenue streams and higher market valuations. This is a classic case of asymmetric extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   Classifying this as a pure Snare would miss the genuine coordination function that does exist (e.g., cloud services, security updates). Classifying it as a Rope would be a gross misrepresentation that ignores the coercive elements and loss of consumer rights. The Tangled Rope classification is essential to correctly identify that a legitimate service is being used as a vehicle for disproportionate and coercive extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    consumer_value_vs_inertia,
    'Is the recurring revenue primarily derived from delivering continuous value, or from exploiting consumer inertia and cancellation friction?',
    'Analysis of churn rates correlated with the introduction/removal of cancellation 'dark patterns' and feature updates.',
    'If primarily value-driven, the classification leans towards Tangled Rope. If primarily inertia-driven, it leans towards Snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(consumer_value_vs_inertia, empirical, 'Distinguishing between value delivery and inertia exploitation in subscription models.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(subscription_economy_model_u2_sed_r5, 2010, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(subs_tr_t0, subscription_economy_model_u2_sed_r5, theater_ratio, 0, 0.15).
narrative_ontology:measurement(subs_tr_t7, subscription_economy_model_u2_sed_r5, theater_ratio, 7, 0.3).
narrative_ontology:measurement(subs_tr_t14, subscription_economy_model_u2_sed_r5, theater_ratio, 14, 0.4).

% Extraction over time
narrative_ontology:measurement(subs_be_t0, subscription_economy_model_u2_sed_r5, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(subs_be_t7, subscription_economy_model_u2_sed_r5, base_extractiveness, 7, 0.55).
narrative_ontology:measurement(subs_be_t14, subscription_economy_model_u2_sed_r5, base_extractiveness, 14, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(subscription_economy_model_u2_sed_r5, enforcement_mechanism).
narrative_ontology:affects_constraint(subscription_economy_model_u2_sed_r5, right_to_repair).
narrative_ontology:affects_constraint(subscription_economy_model_u2_sed_r5, consumer_debt_burden).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
