% ============================================================================
% CONSTRAINT STORY: subscription_economy_model_u3_exp_r1
% ============================================================================
% Version: 2.1 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_subscription_economy_model_u3_exp_r1, []).

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
 *   constraint_id: subscription_economy_model_u3_exp_r1
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
 *   - Platform Investors: Secondary beneficiary (institutional/arbitrage) — favor the predictable, high-margin revenue streams of subscription models.
 *   - Regulatory Agencies: Analytical/Enforcer (organized/constrained) — attempt to mitigate the most extractive aspects (e.g., mandating easy cancellation).
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(subscription_economy_model_u3_exp_r1, 0.68).
domain_priors:suppression_score(subscription_economy_model_u3_exp_r1, 0.75).
domain_priors:theater_ratio(subscription_economy_model_u3_exp_r1, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(subscription_economy_model_u3_exp_r1, extractiveness, 0.68).
narrative_ontology:constraint_metric(subscription_economy_model_u3_exp_r1, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(subscription_economy_model_u3_exp_r1, theater_ratio, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(subscription_economy_model_u3_exp_r1, tangled_rope).
narrative_ontology:human_readable(subscription_economy_model_u3_exp_r1, "Subscription Economy Model").
narrative_ontology:topic_domain(subscription_economy_model_u3_exp_r1, "economic/technological").

domain_priors:requires_active_enforcement(subscription_economy_model_u3_exp_r1).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(subscription_economy_model_u3_exp_r1, subscription_based_corporations).
narrative_ontology:constraint_beneficiary(subscription_economy_model_u3_exp_r1, platform_investors).
narrative_ontology:constraint_victim(subscription_economy_model_u3_exp_r1, individual_consumers).
narrative_ontology:constraint_victim(subscription_economy_model_u3_exp_r1, small_businesses_as_consumers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% For the consumer, the loss of ownership, recurring costs, and high friction for cancellation constitute a Snare. Alternatives (perpetual licenses) are actively suppressed.
constraint_indexing:constraint_classification(subscription_economy_model_u3_exp_r1, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% For the corporation, this is a pure coordination mechanism to smooth revenue, manage service delivery, and maintain a direct customer relationship. The extraction is viewed as a fair price for continuous service.
constraint_indexing:constraint_classification(subscription_economy_model_u3_exp_r1, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% The analytical view recognizes both the genuine coordination function (service delivery, updates) and the highly asymmetric extraction enabled by consumer inertia and the suppression of ownership models.
constraint_indexing:constraint_classification(subscription_economy_model_u3_exp_r1, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% Regulators see a hybrid system. They acknowledge the business model's legitimacy but are increasingly concerned with the extractive elements, such as 'dark patterns' that make cancellation difficult.
constraint_indexing:constraint_classification(subscription_economy_model_u3_exp_r1, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(subscription_economy_model_u3_exp_r1_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(subscription_economy_model_u3_exp_r1, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(subscription_economy_model_u3_exp_r1, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(subscription_economy_model_u3_exp_r1, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(subscription_economy_model_u3_exp_r1_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The base extractiveness (ε=0.68) is high, reflecting the model's capacity to capture far more lifetime value than a perpetual license, often by capitalizing on user inattention. The suppression score (0.75) is also high due to the active phasing out of one-time purchase options and the implementation of digital rights management (DRM) and complex cancellation procedures to lock users in.
 *
 * PERSPECTIVAL GAP:
 *   A significant gap exists between the corporation, which views the model as a Rope for efficient service delivery and revenue prediction, and the consumer, who experiences it as a Snare due to the loss of asset ownership and the coercive nature of recurring payments for essential tools. The analytical view reconciles this by identifying the dual nature of the constraint.
 *
 * DIRECTIONALITY LOGIC:
 *   The flow of value is directed from a broad base of individual consumers to a concentrated group of corporations and their investors. Consumers are the victims, paying a continuous 'rent' on tools they previously would have owned. Corporations are the beneficiaries, securing stable, long-term revenue streams that are highly valued by financial markets.
 *
 * MANDATROPHY ANALYSIS:
 *   The Tangled Rope classification is crucial here. A simple Snare classification would ignore the genuine coordination benefit that subscriptions can offer (e.g., cloud services, continuous security updates). A Rope classification would ignore the immense, asymmetric extraction and coercion involved. Tangled Rope correctly identifies that a legitimate coordination function has been coupled with a powerful extractive mechanism, preventing misclassification.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    consumer_value_vs_inertia,
    'Is the recurring revenue primarily derived from continuous value delivery or from exploiting consumer inertia and cancellation friction?',
    'Comparative analysis of user engagement data versus churn rates following price increases or service degradation, controlling for cancellation process complexity.',
    'If primarily value-driven, the classification remains Tangled Rope. If primarily inertia-driven, it degrades towards a pure Snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(consumer_value_vs_inertia, empirical, 'Distinguishes whether revenue is based on delivered value or exploited consumer inertia.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(subscription_economy_model_u3_exp_r1, 2010, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(subs_tr_t2010, subscription_economy_model_u3_exp_r1, theater_ratio, 2010, 0.15).
narrative_ontology:measurement(subs_tr_t2018, subscription_economy_model_u3_exp_r1, theater_ratio, 2018, 0.3).
narrative_ontology:measurement(subs_tr_t2025, subscription_economy_model_u3_exp_r1, theater_ratio, 2025, 0.4).

% Extraction over time
narrative_ontology:measurement(subs_be_t2010, subscription_economy_model_u3_exp_r1, base_extractiveness, 2010, 0.3).
narrative_ontology:measurement(subs_be_t2018, subscription_economy_model_u3_exp_r1, base_extractiveness, 2018, 0.55).
narrative_ontology:measurement(subs_be_t2025, subscription_economy_model_u3_exp_r1, base_extractiveness, 2025, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(subscription_economy_model_u3_exp_r1, resource_allocation).
narrative_ontology:affects_constraint(subscription_economy_model_u3_exp_r1, digital_right_to_repair).
narrative_ontology:affects_constraint(subscription_economy_model_u3_exp_r1, perpetual_software_ownership).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
