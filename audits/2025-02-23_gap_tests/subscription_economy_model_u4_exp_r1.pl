% ============================================================================
% CONSTRAINT STORY: subscription_economy_model_u4_exp_r1
% ============================================================================
% Version: 2.1 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-05-21
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_subscription_economy_model_u4_exp_r1, []).

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
 *   constraint_id: subscription_economy_model_u4_exp_r1
 *   human_readable: Subscription Economy Model
 *   domain: economic/technological
 *
 * SUMMARY:
 *   The subscription economy model represents a structural shift from
 *   one-time product sales (perpetual ownership) to recurring fees for access
 *   to a service. This provides a coordination benefit for both producers
 *   (predictable revenue, direct customer relationship) and consumers
 *   (continuous updates, lower upfront cost). However, it simultaneously
 *   functions as a highly extractive mechanism by eliminating ownership,
 *   creating high exit costs through data/platform lock-in, and exploiting
 *   consumer inertia and 'subscription fatigue'. Its prevalence is maintained
 *   by the active suppression of perpetual-license alternatives and
 *   enforcement via Digital Rights Management (DRM).
 *
 * KEY AGENTS:
 *   - Individual Consumers: Primary target (powerless/trapped) - Bear the costs of perpetual payments, loss of ownership, and cancellation friction.
 *   - Subscription-based Corporations: Primary beneficiary (institutional/arbitrage) - Gain predictable recurring revenue, increased customer lifetime value, and market control.
 *   - Venture Capital Investors: Secondary beneficiary (institutional/arbitrage) - Strongly favor the predictable growth metrics of subscription models.
 *   - Perpetual License Holders: Secondary victim (powerless/constrained) - Existing owners of products who are pressured to convert or lose support/functionality.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(subscription_economy_model_u4_exp_r1, 0.68).
domain_priors:suppression_score(subscription_economy_model_u4_exp_r1, 0.72).
domain_priors:theater_ratio(subscription_economy_model_u4_exp_r1, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(subscription_economy_model_u4_exp_r1, extractiveness, 0.68).
narrative_ontology:constraint_metric(subscription_economy_model_u4_exp_r1, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(subscription_economy_model_u4_exp_r1, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(subscription_economy_model_u4_exp_r1, tangled_rope).
narrative_ontology:human_readable(subscription_economy_model_u4_exp_r1, "Subscription Economy Model").
narrative_ontology:topic_domain(subscription_economy_model_u4_exp_r1, "economic/technological").

domain_priors:requires_active_enforcement(subscription_economy_model_u4_exp_r1).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(subscription_economy_model_u4_exp_r1, subscription_based_corporations).
narrative_ontology:constraint_beneficiary(subscription_economy_model_u4_exp_r1, venture_capital_investors).
narrative_ontology:constraint_victim(subscription_economy_model_u4_exp_r1, individual_consumers).
narrative_ontology:constraint_victim(subscription_economy_model_u4_exp_r1, perpetual_license_holders).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% From the consumer's perspective, the model is a snare. Exit is costly due to data lock-in, loss of access, and the deliberate friction of cancellation processes. The lifetime cost often exceeds the one-time purchase price, representing pure extraction.
constraint_indexing:constraint_classification(subscription_economy_model_u4_exp_r1, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% For the corporation, this is a pure coordination mechanism (Rope) that smooths revenue, enables continuous development, and deepens customer relationships. The negative effective extraction (chi) reflects the immense financial and strategic benefit.
constraint_indexing:constraint_classification(subscription_economy_model_u4_exp_r1, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% The analytical view recognizes both the genuine coordination function (service delivery, updates) and the highly asymmetric extraction and suppression of alternatives (ownership), classifying it as a Tangled Rope.
constraint_indexing:constraint_classification(subscription_economy_model_u4_exp_r1, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% Organized groups see the coordination benefits but are primarily focused on the extractive elements and the difficulty of exit, fighting a constant battle against a superior institutional force.
constraint_indexing:constraint_classification(subscription_economy_model_u4_exp_r1, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(subscription_economy_model_u4_exp_r1_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(subscription_economy_model_u4_exp_r1, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(subscription_economy_model_u4_exp_r1, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(subscription_economy_model_u4_exp_r1, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(subscription_economy_model_u4_exp_r1_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The high extractiveness (0.68) reflects the conversion of a finite transaction into a potentially infinite revenue stream, often far exceeding the original product's value. The high suppression (0.72) is justified by the industry-wide phasing out of perpetual licenses, the use of DRM to enforce access, and the creation of 'dark patterns' to make cancellation difficult. The model requires active enforcement to prevent users from accessing services after a subscription lapses.
 *
 * PERSPECTIVAL GAP:
 *   A significant gap exists between the corporation and the consumer. The corporation experiences a Rope: a brilliant coordination tool for managing revenue and development. The consumer experiences a Snare: they are trapped in a system of perpetual payments for tools they once could have owned, with exit made deliberately difficult.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is clear. Corporations and their investors are the beneficiaries, receiving a continuous flow of capital. Consumers are the victims, providing that capital flow while losing asset ownership and autonomy. The structure is designed to transfer wealth and control from the latter to the former.
 *
 * MANDATROPHY ANALYSIS:
 *   Classifying this as a pure Snare would miss the genuine coordination function (e.g., cloud services, live game updates) that provides its initial justification. Classifying it as a Rope would ignore the coercive and highly extractive nature. The Tangled Rope classification is essential to capture this duality, where a legitimate service delivery model is coupled with powerful mechanisms for rent-seeking and control.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    subscription_value_vs_rent,
    'Is the recurring fee a fair price for continuous service and updates, or is it rent-seeking on a captured user base?',
    'Comparative analysis of total subscription cost vs. the cost of perpetual licenses plus paid major upgrades over a product's lifecycle, adjusted for feature velocity.',
    'If the value of continuous service consistently outweighs the cost differential, the constraint leans towards Rope. If not, it is a Snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(subscription_value_vs_rent, empirical, 'Distinguishing fair value for service from rent-seeking on captured users.').

omega_variable(
    ownership_paradigm_shift,
    'Is the erosion of digital ownership an inevitable consequence of networked services or a deliberate strategy to maximize extraction?',
    'Analysis of internal corporate strategy documents and technical feasibility studies for hybrid ownership/subscription models.',
    'If inevitable, it has Mountain-like properties. If a deliberate choice among viable alternatives, it confirms the high suppression score of a Tangled Rope or Snare.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(ownership_paradigm_shift, conceptual, 'Determining if the loss of ownership is a necessary trade-off or a manufactured condition.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(subscription_economy_model_u4_exp_r1, 2010, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(subs_tr_t2010, subscription_economy_model_u4_exp_r1, theater_ratio, 2010, 0.15).
narrative_ontology:measurement(subs_tr_t2017, subscription_economy_model_u4_exp_r1, theater_ratio, 2017, 0.28).
narrative_ontology:measurement(subs_tr_t2024, subscription_economy_model_u4_exp_r1, theater_ratio, 2024, 0.35).

% Extraction over time
narrative_ontology:measurement(subs_be_t2010, subscription_economy_model_u4_exp_r1, base_extractiveness, 2010, 0.25).
narrative_ontology:measurement(subs_be_t2017, subscription_economy_model_u4_exp_r1, base_extractiveness, 2017, 0.55).
narrative_ontology:measurement(subs_be_t2024, subscription_economy_model_u4_exp_r1, base_extractiveness, 2024, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(subscription_economy_model_u4_exp_r1, resource_allocation).
narrative_ontology:affects_constraint(subscription_economy_model_u4_exp_r1, digital_ownership_rights).
narrative_ontology:affects_constraint(subscription_economy_model_u4_exp_r1, consumer_debt_burden).
narrative_ontology:affects_constraint(subscription_economy_model_u4_exp_r1, right_to_repair).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
