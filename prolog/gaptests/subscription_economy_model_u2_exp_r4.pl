% ============================================================================
% CONSTRAINT STORY: subscription_economy_model_u2_exp_r4
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-05-21
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_subscription_economy_model_u2_exp_r4, []).

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
 *   constraint_id: subscription_economy_model_u2_exp_r4
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
 *   - Regulators: Secondary actor (institutional/constrained) — struggle to adapt consumer protection laws to the new model.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(subscription_economy_model_u2_exp_r4, 0.68).
domain_priors:suppression_score(subscription_economy_model_u2_exp_r4, 0.72).
domain_priors:theater_ratio(subscription_economy_model_u2_exp_r4, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(subscription_economy_model_u2_exp_r4, extractiveness, 0.68).
narrative_ontology:constraint_metric(subscription_economy_model_u2_exp_r4, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(subscription_economy_model_u2_exp_r4, theater_ratio, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(subscription_economy_model_u2_exp_r4, tangled_rope).
narrative_ontology:human_readable(subscription_economy_model_u2_exp_r4, "Subscription Economy Model").
narrative_ontology:topic_domain(subscription_economy_model_u2_exp_r4, "economic/technological").

domain_priors:requires_active_enforcement(subscription_economy_model_u2_exp_r4).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(subscription_economy_model_u2_exp_r4, subscription_based_corporations).
narrative_ontology:constraint_victim(subscription_economy_model_u2_exp_r4, individual_consumers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% From the consumer's view, the model is a trap. Ownership is revoked, costs are perpetual, and cancellation is often intentionally difficult, creating a high-friction exit.
constraint_indexing:constraint_classification(subscription_economy_model_u2_exp_r4, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% From the corporation's perspective, this is a superior coordination mechanism for delivering continuous value and securing predictable, recurring revenue streams, reducing market volatility.
constraint_indexing:constraint_classification(subscription_economy_model_u2_exp_r4, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% The analytical view recognizes both the genuine coordination function (continuous updates) and the asymmetric extraction (loss of ownership, consumer inertia, high TCO).
constraint_indexing:constraint_classification(subscription_economy_model_u2_exp_r4, tangled_rope,
    context(agent_power(analytical),
            time_horizon(generational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(subscription_economy_model_u2_exp_r4_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(subscription_economy_model_u2_exp_r4, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(subscription_economy_model_u2_exp_r4, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(subscription_economy_model_u2_exp_r4, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(subscription_economy_model_u2_exp_r4_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The high extractiveness (0.68) reflects the conversion of a finite purchase into a potentially infinite payment stream, coupled with the loss of asset ownership. The high suppression (0.72) stems from the active phasing out of perpetual license alternatives and the use of DRM to enforce compliance, leaving consumers with few choices.
 *
 * PERSPECTIVAL GAP:
 *   The gap is stark. Corporations view the model as a Rope, a pure coordination solution for service delivery and revenue stabilization. Consumers experience it as a Snare, where they are trapped into perpetual payments for tools they once could own outright. The analytical perspective must classify it as a Tangled Rope to capture this duality of function and extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality is unambiguous. Value flows from a broad base of individual consumers to a concentrated group of corporations. Consumers are the victims, paying more over time and losing autonomy. Corporations are the beneficiaries, gaining financial stability and market control. This structure ensures a high directionality (d) for consumers and a low d for corporations.
 *
 * MANDATROPHY ANALYSIS:
 *   Classifying this as a Tangled Rope is crucial for avoiding mandatrophy. A simple Snare classification would ignore the genuine coordination benefits (e.g., security patches, feature updates) that providers cite as justification. A Rope classification would ignore the immense, asymmetric extraction and the coercive suppression of alternatives. The Tangled Rope correctly identifies that a valid coordination function is being used as a vehicle for a highly extractive and coercive arrangement.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    subscription_value_vs_rent,
    'Is the recurring fee a fair price for continuous service and updates, or is it primarily rent-seeking on a captured customer base?',
    'Comparative analysis of total cost of ownership (TCO) for subscription vs. perpetual models, adjusted for the actual utility of updates and services provided.',
    'If the value exchange is equitable, the constraint leans towards Rope. If it's primarily rent-seeking, it solidifies as a Snare from most non-beneficiary perspectives.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(subscription_value_vs_rent, empirical, 'Distinguishing fair value exchange from rent-seeking in recurring revenue models.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(subscription_economy_model_u2_exp_r4, 2005, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(subs_tr_t2005, subscription_economy_model_u2_exp_r4, theater_ratio, 2005, 0.15).
narrative_ontology:measurement(subs_tr_t2015, subscription_economy_model_u2_exp_r4, theater_ratio, 2015, 0.3).
narrative_ontology:measurement(subs_tr_t2025, subscription_economy_model_u2_exp_r4, theater_ratio, 2025, 0.45).

% Extraction over time
narrative_ontology:measurement(subs_be_t2005, subscription_economy_model_u2_exp_r4, base_extractiveness, 2005, 0.2).
narrative_ontology:measurement(subs_be_t2015, subscription_economy_model_u2_exp_r4, base_extractiveness, 2015, 0.5).
narrative_ontology:measurement(subs_be_t2025, subscription_economy_model_u2_exp_r4, base_extractiveness, 2025, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(subscription_economy_model_u2_exp_r4, resource_allocation).
narrative_ontology:affects_constraint(subscription_economy_model_u2_exp_r4, digital_rights_management).
narrative_ontology:affects_constraint(subscription_economy_model_u2_exp_r4, platform_lock_in).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
