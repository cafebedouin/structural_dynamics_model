% ============================================================================
% CONSTRAINT STORY: subscription_economy_model_u2_exp_r5
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2023-10-27
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_subscription_economy_model_u2_exp_r5, []).

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
 *   constraint_id: subscription_economy_model_u2_exp_r5
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
domain_priors:base_extractiveness(subscription_economy_model_u2_exp_r5, 0.68).
domain_priors:suppression_score(subscription_economy_model_u2_exp_r5, 0.75).
domain_priors:theater_ratio(subscription_economy_model_u2_exp_r5, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(subscription_economy_model_u2_exp_r5, extractiveness, 0.68).
narrative_ontology:constraint_metric(subscription_economy_model_u2_exp_r5, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(subscription_economy_model_u2_exp_r5, theater_ratio, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(subscription_economy_model_u2_exp_r5, tangled_rope).
narrative_ontology:human_readable(subscription_economy_model_u2_exp_r5, "Subscription Economy Model").
narrative_ontology:topic_domain(subscription_economy_model_u2_exp_r5, "economic/technological").

domain_priors:requires_active_enforcement(subscription_economy_model_u2_exp_r5).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(subscription_economy_model_u2_exp_r5, subscription_based_corporations).
narrative_ontology:constraint_victim(subscription_economy_model_u2_exp_r5, individual_consumers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% For a consumer with high switching costs and no alternatives, the model is a pure Snare, extracting value without commensurate benefit and revoking ownership.
constraint_indexing:constraint_classification(subscription_economy_model_u2_exp_r5, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% From the corporation's view, this is a pure coordination mechanism (Rope) for smoothing revenue, funding continuous development, and managing customer relationships.
constraint_indexing:constraint_classification(subscription_economy_model_u2_exp_r5, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% A moderately powerful user recognizes the coordination benefits but also feels the extraction from inertia and cancellation friction, seeing a Tangled Rope.
constraint_indexing:constraint_classification(subscription_economy_model_u2_exp_r5, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% The analytical view correctly identifies the dual nature: a genuine coordination function (service delivery) coupled with high, asymmetric extraction, defining it as a Tangled Rope.
constraint_indexing:constraint_classification(subscription_economy_model_u2_exp_r5, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(subscription_economy_model_u2_exp_r5_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(subscription_economy_model_u2_exp_r5, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(subscription_economy_model_u2_exp_r5, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(subscription_economy_model_u2_exp_r5, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(subscription_economy_model_u2_exp_r5_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness (0.68) is high due to the conversion of ownership into rental, capturing all future consumer surplus. Suppression (0.75) is high because perpetual-license alternatives are systematically eliminated from the market, leaving consumers with no other choice. Active enforcement is required via DRM and terms of service.
 *
 * PERSPECTIVAL GAP:
 *   The gap is stark. Corporations view the model as a Rope, a superior coordination method for delivering value. Consumers trapped within it experience it as a Snare, a system designed to make exit costly and to extract payment through inertia. The analytical view of Tangled Rope reconciles these by acknowledging both the coordination function and the coercive, extractive elements.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality is unambiguous. Value flows from a broad base of individual consumers (victims) to a concentrated set of corporations (beneficiaries). The structure is designed to maximize this flow by increasing friction for victims and decreasing revenue volatility for beneficiaries.
 *
 * MANDATROPHY ANALYSIS:
 *   Classifying this as a Tangled Rope prevents two errors. It avoids dismissing the model as a pure Snare, which would ignore the genuine coordination/service function it provides (continuous updates, cloud access). It also avoids accepting the beneficiary's claim that it is a pure Rope, which would ignore the coercive extraction, loss of ownership, and high suppression of alternatives.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    consumer_value_vs_inertia,
    'Is the recurring revenue primarily funding continuous innovation (a genuine service) or exploiting consumer inertia and cancellation friction (a rent-seeking mechanism)?',
    'Analysis of R&D spending vs. customer churn/retention costs, and A/B testing of cancellation process difficulty.',
    'If primarily innovation, it trends towards a legitimate Rope. If primarily inertia, it is functionally a Snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(consumer_value_vs_inertia, empirical, 'Distinguishing between value creation and inertia exploitation in subscription models.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(subscription_economy_model_u2_exp_r5, 2008, 2023).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(subs_tr_t0, subscription_economy_model_u2_exp_r5, theater_ratio, 0, 0.1).
narrative_ontology:measurement(subs_tr_t7, subscription_economy_model_u2_exp_r5, theater_ratio, 7, 0.25).
narrative_ontology:measurement(subs_tr_t15, subscription_economy_model_u2_exp_r5, theater_ratio, 15, 0.4).

% Extraction over time
narrative_ontology:measurement(subs_be_t0, subscription_economy_model_u2_exp_r5, base_extractiveness, 0, 0.2).
narrative_ontology:measurement(subs_be_t7, subscription_economy_model_u2_exp_r5, base_extractiveness, 7, 0.5).
narrative_ontology:measurement(subs_be_t15, subscription_economy_model_u2_exp_r5, base_extractiveness, 15, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(subscription_economy_model_u2_exp_r5, resource_allocation).
narrative_ontology:affects_constraint(subscription_economy_model_u2_exp_r5, right_to_repair).
narrative_ontology:affects_constraint(subscription_economy_model_u2_exp_r5, digital_ownership).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
