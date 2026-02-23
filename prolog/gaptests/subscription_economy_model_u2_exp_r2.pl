% ============================================================================
% CONSTRAINT STORY: subscription_economy_model_u2_exp_r2
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2023-10-27
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_subscription_economy_model_u2_exp_r2, []).

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
 *   constraint_id: subscription_economy_model_u2_exp_r2
 *   human_readable: Subscription Economy Model
 *   domain: economic/technological
 *
 * SUMMARY:
 *   This constraint describes the widespread business model shift from
 *   one-time product sales (perpetual licenses) to recurring subscription
 *   fees. While offering a coordination benefit (continuous updates, service
 *   access), it asymmetrically extracts value by creating consumer inertia,
 *   making cancellation difficult (a 'hassle tax'), and revoking ownership
 *   rights. The model is actively enforced by Digital Rights Management (DRM)
 *   and the strategic suppression of non-subscription alternatives.
 *
 * KEY AGENTS:
 *   - Individual Consumers: Primary target (powerless/trapped) — bear the costs of subscription fatigue, loss of ownership, and cancellation friction.
 *   - Subscription-based Corporations: Primary beneficiary (institutional/arbitrage) — benefit from predictable recurring revenue and increased customer lifetime value.
 *   - Regulators: Secondary actor (institutional/constrained) — struggle to apply ownership and consumer protection laws to a service-based model.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(subscription_economy_model_u2_exp_r2, 0.68).
domain_priors:suppression_score(subscription_economy_model_u2_exp_r2, 0.72).
domain_priors:theater_ratio(subscription_economy_model_u2_exp_r2, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(subscription_economy_model_u2_exp_r2, extractiveness, 0.68).
narrative_ontology:constraint_metric(subscription_economy_model_u2_exp_r2, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(subscription_economy_model_u2_exp_r2, theater_ratio, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(subscription_economy_model_u2_exp_r2, tangled_rope).
narrative_ontology:human_readable(subscription_economy_model_u2_exp_r2, "Subscription Economy Model").
narrative_ontology:topic_domain(subscription_economy_model_u2_exp_r2, "economic/technological").

domain_priors:requires_active_enforcement(subscription_economy_model_u2_exp_r2).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(subscription_economy_model_u2_exp_r2, subscription_corporations).
narrative_ontology:constraint_victim(subscription_economy_model_u2_exp_r2, individual_consumers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% From the consumer's view, the loss of ownership, subscription fatigue, and high friction for cancellation make this a coercive, extractive system with no viable alternatives.
constraint_indexing:constraint_classification(subscription_economy_model_u2_exp_r2, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% From the corporation's perspective, this is a superior coordination model for aligning revenue with ongoing development and service costs, creating predictable cash flow.
constraint_indexing:constraint_classification(subscription_economy_model_u2_exp_r2, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% The analytical view recognizes both the genuine coordination function (continuous service) and the highly asymmetric extraction enabled by suppressing ownership and creating inertia.
constraint_indexing:constraint_classification(subscription_economy_model_u2_exp_r2, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(subscription_economy_model_u2_exp_r2_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(subscription_economy_model_u2_exp_r2, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(subscription_economy_model_u2_exp_r2, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(subscription_economy_model_u2_exp_r2, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(subscription_economy_model_u2_exp_r2_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The high extractiveness (0.68) reflects the conversion of a finite capital expense into a potentially infinite operational expense for the consumer, coupled with the loss of the asset itself. The high suppression (0.72) is due to the active removal of perpetual license options from the market, the use of DRM to prevent offline use, and the deliberate design of high-friction cancellation processes.
 *
 * PERSPECTIVAL GAP:
 *   The gap is stark: corporations view the model as a Rope, a fair exchange of recurring payment for recurring service. Consumers experience it as a Snare, where they are trapped in endless payments for tools they used to own, with exit made costly and difficult. The analytical view must be Tangled Rope to capture both realities.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality is clear. Value flows from a broad base of individual consumers (victims) to a concentrated set of corporations (beneficiaries). The structure is designed to maximize this flow by increasing the cost and difficulty of opting out.
 *
 * MANDATROPHY ANALYSIS:
 *   The Tangled Rope classification is critical here. A simple Snare classification would miss the genuine coordination function that makes the model attractive in the first place (e.g., cloud software always being up-to-date). A Rope classification would ignore the immense, non-consensual extraction. Tangled Rope correctly identifies the structure as a coordination mechanism that has been weaponized for asymmetric extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    value_vs_inertia,
    'Is the recurring revenue primarily justified by continuous value delivery (updates, service) or by exploiting consumer inertia and cancellation friction?',
    'Analysis of churn rates correlated with cancellation process complexity and the availability of perpetual alternatives.',
    'If primarily value-driven, the classification leans towards Rope. If primarily inertia-driven, it solidifies as a Snare from most non-beneficiary perspectives.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(value_vs_inertia, empirical, 'Whether revenue stems from continuous value or from exploiting consumer inertia.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(subscription_economy_model_u2_exp_r2, 2005, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(subs_tr_t0, subscription_economy_model_u2_exp_r2, theater_ratio, 0, 0.15).
narrative_ontology:measurement(subs_tr_t10, subscription_economy_model_u2_exp_r2, theater_ratio, 10, 0.3).
narrative_ontology:measurement(subs_tr_t20, subscription_economy_model_u2_exp_r2, theater_ratio, 20, 0.4).

% Extraction over time
narrative_ontology:measurement(subs_be_t0, subscription_economy_model_u2_exp_r2, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(subs_be_t10, subscription_economy_model_u2_exp_r2, base_extractiveness, 10, 0.5).
narrative_ontology:measurement(subs_be_t20, subscription_economy_model_u2_exp_r2, base_extractiveness, 20, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(subscription_economy_model_u2_exp_r2, resource_allocation).
narrative_ontology:affects_constraint(subscription_economy_model_u2_exp_r2, right_to_repair).
narrative_ontology:affects_constraint(subscription_economy_model_u2_exp_r2, digital_ownership_rights).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
