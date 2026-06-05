% ============================================================================
% CONSTRAINT STORY: subscription_economy_model_u1_exp_r2
% ============================================================================
% Version: 2.1 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-29
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_subscription_economy_model_u1_exp_r2, []).

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
 *   constraint_id: subscription_economy_model_u1_exp_r2
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
 *   - Regulators: Institutional actor (institutional/constrained) — attempt to balance consumer protection with market innovation.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(subscription_economy_model_u1_exp_r2, 0.68).
domain_priors:suppression_score(subscription_economy_model_u1_exp_r2, 0.72).
domain_priors:theater_ratio(subscription_economy_model_u1_exp_r2, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(subscription_economy_model_u1_exp_r2, extractiveness, 0.68).
narrative_ontology:constraint_metric(subscription_economy_model_u1_exp_r2, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(subscription_economy_model_u1_exp_r2, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(subscription_economy_model_u1_exp_r2, tangled_rope).
narrative_ontology:human_readable(subscription_economy_model_u1_exp_r2, "Subscription Economy Model").
narrative_ontology:topic_domain(subscription_economy_model_u1_exp_r2, "economic/technological").

domain_priors:requires_active_enforcement(subscription_economy_model_u1_exp_r2).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(subscription_economy_model_u1_exp_r2, subscription_based_corporations).
narrative_ontology:constraint_victim(subscription_economy_model_u1_exp_r2, individual_consumers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% From the consumer's perspective, the model is a snare. They lose ownership, face subscription fatigue, and are trapped by cancellation friction and the lack of perpetual license alternatives.
constraint_indexing:constraint_classification(subscription_economy_model_u1_exp_r2, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% For the corporation, this is a pure coordination mechanism (Rope) for securing predictable revenue streams, funding ongoing development, and managing customer relationships.
constraint_indexing:constraint_classification(subscription_economy_model_u1_exp_r2, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% The analytical view recognizes both the coordination function (stable funding for services) and the asymmetric extraction (consumer inertia, loss of ownership, high lifetime cost), classifying it as a Tangled Rope.
constraint_indexing:constraint_classification(subscription_economy_model_u1_exp_r2, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% Regulators see the consumer harm but are constrained by existing legal frameworks built for physical goods and lobbying from powerful corporations, making effective intervention difficult.
constraint_indexing:constraint_classification(subscription_economy_model_u1_exp_r2, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(subscription_economy_model_u1_exp_r2_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(subscription_economy_model_u1_exp_r2, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(subscription_economy_model_u1_exp_r2, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(subscription_economy_model_u1_exp_r2, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(subscription_economy_model_u1_exp_r2_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The high extractiveness (0.68) reflects the total lifetime cost often exceeding perpetual license prices and the value extracted from 'forgotten' subscriptions. The high suppression (0.72) is due to the active phasing out of purchase options and the use of DRM to prevent offline use or ownership.
 *
 * PERSPECTIVAL GAP:
 *   A significant gap exists between the corporation, which views the model as a beneficial Rope for managing revenue and development, and the consumer, who experiences it as a Snare due to lock-in and loss of agency. The analytical perspective reconciles these by identifying the structure as a Tangled Rope.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality is clear: value flows from a broad base of individual consumers (victims) to a concentrated set of corporations (beneficiaries). Consumers are trapped by network effects, data lock-in, and a lack of alternatives, while corporations have arbitrage power to deploy this model globally.
 *
 * MANDATROPHY ANALYSIS:
 *   The Tangled Rope classification is critical here. It prevents the mischaracterization of the model as a pure Rope (the corporate PR view) by acknowledging the high, coercive extraction. It also avoids classifying it as a pure Snare by recognizing the genuine, albeit often overstated, coordination function of funding continuous service and updates.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    value_vs_rent,
    'Does the value of continuous updates and services justify the recurring cost, or is the model primarily a rent-seeking mechanism exploiting consumer inertia?',
    'Comparative analysis of feature velocity and value delivery in subscription vs. perpetual models, controlling for market sector.',
    'If value is commensurate with cost, it leans towards Rope. If cost significantly outweighs delivered value, it solidifies the Snare/Tangled Rope classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(value_vs_rent, empirical, 'Whether the subscription model delivers fair value or primarily extracts rent.').

omega_variable(
    ownership_vs_convenience,
    'Is the erosion of product ownership a fundamental harm to consumer rights, or a fair and accepted trade-off for the convenience of access-based services?',
    'Longitudinal studies of consumer sentiment and legal challenges regarding digital ownership rights.',
    'If deemed a fundamental harm, suppression and extractiveness scores would be justified or even increased. If accepted as a fair trade, the model appears less coercive.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(ownership_vs_convenience, preference, 'The fundamental trade-off between digital ownership and service convenience.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(subscription_economy_model_u1_exp_r2, 2010, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(subs_tr_t2010, subscription_economy_model_u1_exp_r2, theater_ratio, 2010, 0.15).
narrative_ontology:measurement(subs_tr_t2017, subscription_economy_model_u1_exp_r2, theater_ratio, 2017, 0.28).
narrative_ontology:measurement(subs_tr_t2024, subscription_economy_model_u1_exp_r2, theater_ratio, 2024, 0.35).

% Extraction over time
narrative_ontology:measurement(subs_be_t2010, subscription_economy_model_u1_exp_r2, base_extractiveness, 2010, 0.25).
narrative_ontology:measurement(subs_be_t2017, subscription_economy_model_u1_exp_r2, base_extractiveness, 2017, 0.55).
narrative_ontology:measurement(subs_be_t2024, subscription_economy_model_u1_exp_r2, base_extractiveness, 2024, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(subscription_economy_model_u1_exp_r2, resource_allocation).
narrative_ontology:affects_constraint(subscription_economy_model_u1_exp_r2, digital_rights_management).
narrative_ontology:affects_constraint(subscription_economy_model_u1_exp_r2, consumer_data_privacy).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
