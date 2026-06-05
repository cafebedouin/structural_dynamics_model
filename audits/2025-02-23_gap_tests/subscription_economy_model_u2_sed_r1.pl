% ============================================================================
% CONSTRAINT STORY: subscription_economy_model_u2_sed_r1
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-05-21
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_subscription_economy_model_u2_sed_r1, []).

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
 *   constraint_id: subscription_economy_model_u2_sed_r1
 *   human_readable: The Subscription Economy Model
 *   domain: economic/technological
 *
 * SUMMARY:
 *   The subscription economy model represents a structural shift in commerce
 *   from the one-time sale of goods (and perpetual licenses for software) to
 *   a model of recurring rental and service access. This provides a
 *   coordination benefit for producers, creating predictable revenue streams
 *   to fund ongoing development and support. However, it asymmetrically
 *   extracts value from consumers by revoking ownership, creating lock-in,
 *   and exploiting behavioral inertia through cancellation friction.
 *
 * KEY AGENTS:
 *   - Individual Consumers: Primary target (powerless/trapped) — bear the costs of subscription fatigue, loss of ownership, and cancellation friction.
 *   - Subscription-based Corporations: Primary beneficiary (institutional/arbitrage) — benefit from predictable recurring revenue and increased customer lifetime value.
 *   - Small Businesses / Prosumers: Secondary target (moderate/constrained) — depend on the services but are exposed to price and platform risk.
 *   - Second-hand Markets: Systemic victim (powerless/trapped) — eliminated by the non-transferability of digital licenses.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(subscription_economy_model_u2_sed_r1, 0.68).
domain_priors:suppression_score(subscription_economy_model_u2_sed_r1, 0.72).
domain_priors:theater_ratio(subscription_economy_model_u2_sed_r1, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(subscription_economy_model_u2_sed_r1, extractiveness, 0.68).
narrative_ontology:constraint_metric(subscription_economy_model_u2_sed_r1, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(subscription_economy_model_u2_sed_r1, theater_ratio, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(subscription_economy_model_u2_sed_r1, tangled_rope).
narrative_ontology:human_readable(subscription_economy_model_u2_sed_r1, "The Subscription Economy Model").
narrative_ontology:topic_domain(subscription_economy_model_u2_sed_r1, "economic/technological").

domain_priors:requires_active_enforcement(subscription_economy_model_u2_sed_r1).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(subscription_economy_model_u2_sed_r1, subscription_based_corporations).
narrative_ontology:constraint_beneficiary(subscription_economy_model_u2_sed_r1, venture_capital_investors).
narrative_ontology:constraint_victim(subscription_economy_model_u2_sed_r1, individual_consumers).
narrative_ontology:constraint_victim(subscription_economy_model_u2_sed_r1, second_hand_markets).
narrative_ontology:constraint_victim(subscription_economy_model_u2_sed_r1, small_businesses).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% For the individual consumer, the model is a snare. High switching costs, data lock-in, and the elimination of alternatives create a trapped condition with perpetual costs.
constraint_indexing:constraint_classification(subscription_economy_model_u2_sed_r1, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% For the corporation, this is a pure coordination mechanism (rope) for stabilizing revenue, funding continuous development, and maximizing customer lifetime value.
constraint_indexing:constraint_classification(subscription_economy_model_u2_sed_r1, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% For small businesses or 'prosumers' who rely on subscription software, the constraint is a tangled rope. They benefit from the service but are vulnerable to price hikes and platform risk.
constraint_indexing:constraint_classification(subscription_economy_model_u2_sed_r1, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% The analytical view recognizes both the genuine coordination function (service delivery) and the asymmetric extraction (loss of ownership, inertia exploitation), classifying it as a tangled rope.
constraint_indexing:constraint_classification(subscription_economy_model_u2_sed_r1, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(subscription_economy_model_u2_sed_r1_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(subscription_economy_model_u2_sed_r1, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(subscription_economy_model_u2_sed_r1, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(subscription_economy_model_u2_sed_r1, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(subscription_economy_model_u2_sed_r1_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The high extractiveness (0.68) reflects the conversion of a one-time capital expense into a perpetual operational expense for the consumer, capturing all future value. The high suppression (0.72) reflects the active phasing out of perpetual license alternatives, the use of DRM to enforce terms, and the creation of high switching costs (data/ecosystem lock-in).
 *
 * PERSPECTIVAL GAP:
 *   The gap is stark. For corporations (beneficiaries with arbitrage), the model is a Rope that solves the problem of lumpy revenue and funds innovation. For consumers (victims who are trapped), it is a Snare that extracts wealth indefinitely with no path to ownership. The analytical view must account for both the real service provided and the coercive extraction, hence the Tangled Rope classification.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is derived from the clear beneficiary/victim structure. Corporations, as beneficiaries with high exit/arbitrage options (they can choose which products to offer this way), have a low `d` value, perceiving low extraction. Consumers, as victims with low exit options (trapped in ecosystems), have a high `d` value, perceiving high extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   Classifying this as a pure Snare would incorrectly dismiss the genuine coordination function of funding continuous software updates and cloud services. Classifying it as a pure Rope would ignore the coercive and extractive elements of eliminating ownership and exploiting inertia. The Tangled Rope classification is essential to capture this dual nature, preventing the mislabeling of coercive extraction as benign coordination.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    subscription_necessity_vs_choice,
    'Is the elimination of perpetual ownership a necessary technical trade-off for continuous service delivery, or a purely extractive business model choice?',
    'Analysis of hybrid models that successfully offer both subscription and perpetual options, and technical audits of service delivery costs versus revenue.',
    'If a necessary trade-off, the coordination function is stronger, reinforcing the Tangled Rope classification. If a purely extractive choice, the constraint is closer to a pure Snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(subscription_necessity_vs_choice, empirical, 'Whether the subscription model is a technical necessity or an extractive choice.').

omega_variable(
    consumer_inertia_quantification,
    'What percentage of recurring revenue is attributable to consumer inertia and cancellation friction (the 'hassle tax') versus perceived value?',
    'Large-scale consumer surveys, analysis of churn rates following simplification of cancellation processes, and internal company data on customer support tickets related to cancellation.',
    'A high percentage attributable to inertia would increase the base extractiveness score (ε) and strengthen the Snare classification from the consumer perspective.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(consumer_inertia_quantification, empirical, 'Quantifying the value extracted from consumer inertia vs. perceived service value.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(subscription_economy_model_u2_sed_r1, 2010, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(subs_tr_t0, subscription_economy_model_u2_sed_r1, theater_ratio, 0, 0.2).
narrative_ontology:measurement(subs_tr_t7, subscription_economy_model_u2_sed_r1, theater_ratio, 7, 0.35).
narrative_ontology:measurement(subs_tr_t14, subscription_economy_model_u2_sed_r1, theater_ratio, 14, 0.45).

% Extraction over time
narrative_ontology:measurement(subs_be_t0, subscription_economy_model_u2_sed_r1, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(subs_be_t7, subscription_economy_model_u2_sed_r1, base_extractiveness, 7, 0.55).
narrative_ontology:measurement(subs_be_t14, subscription_economy_model_u2_sed_r1, base_extractiveness, 14, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(subscription_economy_model_u2_sed_r1, enforcement_mechanism).
narrative_ontology:affects_constraint(subscription_economy_model_u2_sed_r1, right_to_repair).
narrative_ontology:affects_constraint(subscription_economy_model_u2_sed_r1, consumer_data_privacy).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
