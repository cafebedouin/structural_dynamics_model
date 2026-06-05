% ============================================================================
% CONSTRAINT STORY: subscription_economy_model_u2_sed_r2
% ============================================================================
% Version: 2.1 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-05-21
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_subscription_economy_model_u2_sed_r2, []).

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
 *   constraint_id: subscription_economy_model_u2_sed_r2
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
 *   - Investors: Secondary beneficiary (institutional/arbitrage) — favor the predictable, high-margin revenue streams of SaaS models.
 *   - Regulators: Analytical/Institutional actor — observes the structure but is often constrained in its ability to intervene.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(subscription_economy_model_u2_sed_r2, 0.6).
domain_priors:suppression_score(subscription_economy_model_u2_sed_r2, 0.72).
domain_priors:theater_ratio(subscription_economy_model_u2_sed_r2, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(subscription_economy_model_u2_sed_r2, extractiveness, 0.6).
narrative_ontology:constraint_metric(subscription_economy_model_u2_sed_r2, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(subscription_economy_model_u2_sed_r2, theater_ratio, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(subscription_economy_model_u2_sed_r2, tangled_rope).
narrative_ontology:human_readable(subscription_economy_model_u2_sed_r2, "The Subscription Economy Model").
narrative_ontology:topic_domain(subscription_economy_model_u2_sed_r2, "economic/technological").

domain_priors:requires_active_enforcement(subscription_economy_model_u2_sed_r2).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(subscription_economy_model_u2_sed_r2, subscription_based_corporations).
narrative_ontology:constraint_beneficiary(subscription_economy_model_u2_sed_r2, investors_in_saas).
narrative_ontology:constraint_victim(subscription_economy_model_u2_sed_r2, individual_consumers).
narrative_ontology:constraint_victim(subscription_economy_model_u2_sed_r2, small_businesses_reliant_on_tools).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% For a consumer reliant on essential software (e.g., for their profession), the recurring cost and lack of perpetual alternatives constitute a trap.
constraint_indexing:constraint_classification(subscription_economy_model_u2_sed_r2, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% From the corporation's view, this is a superior coordination model for delivering continuous value and securing predictable revenue streams.
constraint_indexing:constraint_classification(subscription_economy_model_u2_sed_r2, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% A user with some resources can switch between services, but the cumulative cognitive and financial load of managing many subscriptions still feels extractive.
constraint_indexing:constraint_classification(subscription_economy_model_u2_sed_r2, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% The analyst sees both the genuine coordination function (service delivery) and the asymmetric extraction enabled by suppressing ownership models.
constraint_indexing:constraint_classification(subscription_economy_model_u2_sed_r2, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(subscription_economy_model_u2_sed_r2_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(subscription_economy_model_u2_sed_r2, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(subscription_economy_model_u2_sed_r2, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(subscription_economy_model_u2_sed_r2, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(subscription_economy_model_u2_sed_r2_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The base extractiveness (0.60) reflects the conversion of a one-time capital expense into a perpetual operational expense for the consumer, effectively capturing all future value. The high suppression score (0.72) is due to the active phasing out of perpetual licenses for major software and the use of DRM to prevent offline use or ownership transfer, leaving consumers with no viable alternatives.
 *
 * PERSPECTIVAL GAP:
 *   A significant gap exists between the corporation, which views the model as a Rope for efficient, continuous service delivery, and the consumer, who experiences it as a Snare that erodes their wealth and autonomy. The corporation focuses on the service provided, while the consumer focuses on the rights and capital lost.
 *
 * DIRECTIONALITY LOGIC:
 *   The flow of value is unidirectional. Corporations (beneficiaries) gain predictable, compounding revenue and a captive customer base. Consumers (victims) lose ownership of their tools, face endlessly recurring costs for the same functionality, and are subject to price increases at the provider's discretion.
 *
 * MANDATROPHY ANALYSIS:
 *   The Tangled Rope classification is crucial here. Labeling this model a pure Snare would ignore the genuine coordination function it serves (providing updates, cloud services). Labeling it a Rope would ignore the coercive, extractive nature of eliminating ownership. The Tangled Rope correctly identifies it as a hybrid system where a valid service is used to justify an extractive and suppressive revenue model.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    consumer_value_vs_inertia,
    'Is the recurring revenue primarily justified by continuous value delivery or by exploiting consumer inertia and cancellation friction?',
    'Analysis of user engagement data versus churn rates following price hikes or feature removal, controlling for the difficulty of cancellation.',
    'If primarily value-driven, the constraint is closer to a Rope. If primarily inertia-driven, it is a Snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(consumer_value_vs_inertia, empirical, 'Distinguishing between value delivery and inertia exploitation in the subscription model.').

omega_variable(
    ownership_paradigm_shift,
    'Is the decline of digital ownership an inevitable consequence of networked technology or a constructed preference enforced by suppliers?',
    'Comparative analysis of markets with and without strong consumer protection laws regarding digital ownership.',
    'If inevitable, it has Mountain-like properties. If constructed, it is a classic Tangled Rope or Snare.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(ownership_paradigm_shift, conceptual, 'Whether the loss of digital ownership is a natural or an artificial outcome.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(subscription_economy_model_u2_sed_r2, 2010, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(subs_tr_t2010, subscription_economy_model_u2_sed_r2, theater_ratio, 2010, 0.2).
narrative_ontology:measurement(subs_tr_t2018, subscription_economy_model_u2_sed_r2, theater_ratio, 2018, 0.35).
narrative_ontology:measurement(subs_tr_t2025, subscription_economy_model_u2_sed_r2, theater_ratio, 2025, 0.45).

% Extraction over time
narrative_ontology:measurement(subs_be_t2010, subscription_economy_model_u2_sed_r2, base_extractiveness, 2010, 0.25).
narrative_ontology:measurement(subs_be_t2018, subscription_economy_model_u2_sed_r2, base_extractiveness, 2018, 0.5).
narrative_ontology:measurement(subs_be_t2025, subscription_economy_model_u2_sed_r2, base_extractiveness, 2025, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(subscription_economy_model_u2_sed_r2, resource_allocation).
narrative_ontology:affects_constraint(subscription_economy_model_u2_sed_r2, digital_ownership_rights).
narrative_ontology:affects_constraint(subscription_economy_model_u2_sed_r2, right_to_repair).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
