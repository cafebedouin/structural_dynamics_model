% ============================================================================
% CONSTRAINT STORY: subscription_economy_model_u2_exp_r3
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-29
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_subscription_economy_model_u2_exp_r3, []).

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
 *   constraint_id: subscription_economy_model_u2_exp_r3
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
 *   - Regulators: Secondary actor (institutional/constrained) — attempt to mitigate the most extractive elements (e.g., mandating easy cancellation) but operate within the dominant paradigm.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(subscription_economy_model_u2_exp_r3, 0.55).
domain_priors:suppression_score(subscription_economy_model_u2_exp_r3, 0.65).
domain_priors:theater_ratio(subscription_economy_model_u2_exp_r3, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(subscription_economy_model_u2_exp_r3, extractiveness, 0.55).
narrative_ontology:constraint_metric(subscription_economy_model_u2_exp_r3, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(subscription_economy_model_u2_exp_r3, theater_ratio, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(subscription_economy_model_u2_exp_r3, tangled_rope).
narrative_ontology:human_readable(subscription_economy_model_u2_exp_r3, "Subscription Economy Model").
narrative_ontology:topic_domain(subscription_economy_model_u2_exp_r3, "economic/technological").

domain_priors:requires_active_enforcement(subscription_economy_model_u2_exp_r3).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(subscription_economy_model_u2_exp_r3, subscription_based_corporations).
narrative_ontology:constraint_victim(subscription_economy_model_u2_exp_r3, individual_consumers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% From the consumer's perspective, the model is a snare. Alternatives are suppressed, cancellation is difficult, and the total lifetime cost often exceeds a one-time purchase, representing significant extraction.
constraint_indexing:constraint_classification(subscription_economy_model_u2_exp_r3, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% For the corporation, this is a pure coordination mechanism (Rope) that smooths revenue, simplifies software updates, and maintains a direct customer relationship. The extraction is viewed as a fair price for continuous service.
constraint_indexing:constraint_classification(subscription_economy_model_u2_exp_r3, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% The analytical view recognizes both the genuine coordination function (service delivery, updates) and the asymmetric extraction (inertia tax, loss of ownership, high suppression of alternatives), classifying it as a Tangled Rope.
constraint_indexing:constraint_classification(subscription_economy_model_u2_exp_r3, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(subscription_economy_model_u2_exp_r3_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(subscription_economy_model_u2_exp_r3, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(subscription_economy_model_u2_exp_r3, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(subscription_economy_model_u2_exp_r3, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(subscription_economy_model_u2_exp_r3_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The base extractiveness (ε=0.55) reflects the value captured through consumer inertia, lock-in, and the effective elimination of a secondary market for goods. The high suppression score (0.65) is due to the active phasing out of perpetual license models and the use of DRM to prevent ownership, leaving consumers with few alternatives.
 *
 * PERSPECTIVAL GAP:
 *   The gap is stark: corporations view the model as an efficient Rope for delivering continuous value, while trapped consumers experience it as a Snare due to high cumulative costs and a lack of control or ownership. The analytical view must account for both the coordination function and the coercive, extractive elements.
 *
 * DIRECTIONALITY LOGIC:
 *   Corporations are the clear beneficiaries, gaining predictable, high-margin revenue streams. Individual consumers are the victims, trading ownership for access and becoming subject to price increases and cancellation friction. This structural relationship directly informs the directionality calculation, leading to negative effective extraction (χ) for beneficiaries and high positive χ for victims.
 *
 * MANDATROPHY ANALYSIS:
 *   The Tangled Rope classification is crucial for avoiding mandatrophy. Labeling this model as a pure Snare would ignore its genuine coordination benefits (e.g., SaaS platforms, streaming services). Labeling it a pure Rope, as corporations do, would ignore the significant, coercive extraction and suppression of alternatives. The Tangled Rope classification correctly identifies the hybrid nature of the constraint.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    value_vs_inertia,
    'Is the recurring revenue primarily payment for ongoing value (updates, service) or a tax on consumer inertia and cancellation friction?',
    'Comparative analysis of user engagement data versus active subscription data, alongside metrics on cancellation difficulty ('dark patterns').',
    'If primarily for value, the constraint leans towards Rope. If primarily a tax on inertia, it is a Snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(value_vs_inertia, empirical, 'Distinguishes whether the model's success stems from value delivery or behavioral exploitation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(subscription_economy_model_u2_exp_r3, 2010, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(subs_tr_t2010, subscription_economy_model_u2_exp_r3, theater_ratio, 2010, 0.1).
narrative_ontology:measurement(subs_tr_t2017, subscription_economy_model_u2_exp_r3, theater_ratio, 2017, 0.15).
narrative_ontology:measurement(subs_tr_t2024, subscription_economy_model_u2_exp_r3, theater_ratio, 2024, 0.2).

% Extraction over time
narrative_ontology:measurement(subs_be_t2010, subscription_economy_model_u2_exp_r3, base_extractiveness, 2010, 0.2).
narrative_ontology:measurement(subs_be_t2017, subscription_economy_model_u2_exp_r3, base_extractiveness, 2017, 0.4).
narrative_ontology:measurement(subs_be_t2024, subscription_economy_model_u2_exp_r3, base_extractiveness, 2024, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(subscription_economy_model_u2_exp_r3, resource_allocation).
narrative_ontology:affects_constraint(subscription_economy_model_u2_exp_r3, digital_ownership_rights).
narrative_ontology:affects_constraint(subscription_economy_model_u2_exp_r3, consumer_debt_burden).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
