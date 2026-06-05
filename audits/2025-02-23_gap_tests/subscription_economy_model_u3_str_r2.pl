% ============================================================================
% CONSTRAINT STORY: subscription_economy_model_u3_str_r2
% ============================================================================
% Version: 2.1 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-05-21
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_subscription_economy_model_u3_str_r2, []).

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
 *   constraint_id: subscription_economy_model_u3_str_r2
 *   human_readable: Subscription Economy Model
 *   domain: economic/technological
 *
 * SUMMARY:
 *   This constraint describes the widespread business model shift from
 *   one-time product sales (perpetual licenses) to recurring subscription
 *   fees. This model presents a genuine coordination benefit by funding
 *   continuous updates and service access. However, it simultaneously enables
 *   asymmetric extraction by creating consumer inertia, making cancellation
 *   difficult (a 'hassle tax'), revoking ownership rights, and locking users
 *   into ecosystems. The model is actively enforced by Digital Rights
 *   Management (DRM) and the strategic suppression of non-subscription
 *   alternatives.
 *
 * KEY AGENTS:
 *   - Individual Consumers: Primary target (powerless/trapped) — bear the costs of subscription fatigue, loss of ownership, and cancellation friction.
 *   - Subscription-based Corporations: Primary beneficiary (institutional/arbitrage) — benefit from predictable recurring revenue and increased customer lifetime value.
 *   - Regulatory Bodies: Secondary enforcer/observer (institutional/constrained) — often lack the frameworks to address the extractive aspects of the model.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(subscription_economy_model_u3_str_r2, 0.68).
domain_priors:suppression_score(subscription_economy_model_u3_str_r2, 0.75).
domain_priors:theater_ratio(subscription_economy_model_u3_str_r2, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(subscription_economy_model_u3_str_r2, extractiveness, 0.68).
narrative_ontology:constraint_metric(subscription_economy_model_u3_str_r2, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(subscription_economy_model_u3_str_r2, theater_ratio, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(subscription_economy_model_u3_str_r2, tangled_rope).
narrative_ontology:human_readable(subscription_economy_model_u3_str_r2, "Subscription Economy Model").
narrative_ontology:topic_domain(subscription_economy_model_u3_str_r2, "economic/technological").

domain_priors:requires_active_enforcement(subscription_economy_model_u3_str_r2).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(subscription_economy_model_u3_str_r2, subscription_corporations).
narrative_ontology:constraint_victim(subscription_economy_model_u3_str_r2, individual_consumers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% From the perspective of a consumer with high switching costs, data lock-in, and no viable alternatives, the model is a pure Snare. They are forced to pay continuously for access to their own data or essential tools.
constraint_indexing:constraint_classification(subscription_economy_model_u3_str_r2, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% For the corporation, the model is a pure Rope. It solves the coordination problem of funding continuous development and service provision while creating a predictable, defensible revenue stream.
constraint_indexing:constraint_classification(subscription_economy_model_u3_str_r2, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% The analytical view recognizes both the genuine coordination function (funding updates) and the asymmetric extraction (consumer inertia, loss of ownership), classifying it as a Tangled Rope.
constraint_indexing:constraint_classification(subscription_economy_model_u3_str_r2, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% A user who actively manages subscriptions and can switch providers still experiences the extractive pressure but is not fully trapped. They see the trade-offs and experience it as a Tangled Rope.
constraint_indexing:constraint_classification(subscription_economy_model_u3_str_r2, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(subscription_economy_model_u3_str_r2_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(subscription_economy_model_u3_str_r2, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(subscription_economy_model_u3_str_r2, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(subscription_economy_model_u3_str_r2, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(subscription_economy_model_u3_str_r2_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The base extractiveness (ε=0.68) is high, reflecting the value extracted through mechanisms beyond the core service, such as inertia, price hikes on captive customers, and the loss of perpetual ownership. Suppression (0.75) is also high because corporations actively phase out perpetual license options and use DRM to prevent off-platform use, effectively removing alternatives.
 *
 * PERSPECTIVAL GAP:
 *   A significant gap exists between the corporation and the consumer. The corporation experiences a highly efficient coordination mechanism (Rope) for aligning revenue with ongoing costs. The trapped consumer experiences a Snare, where they are forced into perpetual payments with high exit barriers. The analytical observer sees both sides, identifying the structure as a Tangled Rope.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is clear. Corporations are the beneficiaries, designing the system to maximize recurring revenue (low 'd'). Consumers are the victims, bearing the financial and ownership costs (high 'd'). The system structurally transfers wealth and control from the consumer base to the service provider over the long term.
 *
 * MANDATROPHY ANALYSIS:
 *   Classifying this as a Tangled Rope is crucial for avoiding mandatrophy. A pure Snare classification would ignore the real coordination benefits (e.g., security updates, cloud services) that justify the model's existence to many users. A pure Rope classification would ignore the severe, asymmetric extraction that defines the consumer experience. The Tangled Rope classification correctly identifies that a legitimate coordination function is being used as a vehicle for extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    value_vs_inertia,
    'Is the high customer lifetime value derived from continuous value delivery or from exploiting consumer inertia and cancellation friction?',
    'Analysis of user churn rates correlated with cancellation process complexity ('dark patterns') versus feature adoption rates.',
    'If value is from inertia, it confirms the high extractiveness score (ε=0.68). If from value delivery, ε would be lower, potentially shifting the classification towards Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(value_vs_inertia, empirical, 'Whether customer value is from service quality or exploited inertia.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(subscription_economy_model_u3_str_r2, 2005, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(subs_tr_t2005, subscription_economy_model_u3_str_r2, theater_ratio, 2005, 0.1).
narrative_ontology:measurement(subs_tr_t2015, subscription_economy_model_u3_str_r2, theater_ratio, 2015, 0.2).
narrative_ontology:measurement(subs_tr_t2025, subscription_economy_model_u3_str_r2, theater_ratio, 2025, 0.3).

% Extraction over time
narrative_ontology:measurement(subs_be_t2005, subscription_economy_model_u3_str_r2, base_extractiveness, 2005, 0.25).
narrative_ontology:measurement(subs_be_t2015, subscription_economy_model_u3_str_r2, base_extractiveness, 2015, 0.5).
narrative_ontology:measurement(subs_be_t2025, subscription_economy_model_u3_str_r2, base_extractiveness, 2025, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(subscription_economy_model_u3_str_r2, resource_allocation).
narrative_ontology:affects_constraint(subscription_economy_model_u3_str_r2, digital_rights_management).
narrative_ontology:affects_constraint(subscription_economy_model_u3_str_r2, consumer_data_privacy).
narrative_ontology:affects_constraint(subscription_economy_model_u3_str_r2, right_to_repair).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
