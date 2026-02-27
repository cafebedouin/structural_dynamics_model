% ============================================================================
% CONSTRAINT STORY: meta_pay_or_okay_model
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-02-29
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_meta_pay_or_okay_model, []).

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
 *   constraint_id: meta_pay_or_okay_model
 *   human_readable: Meta's "Pay or Okay" Data Monetization Model in the EU
 *   domain: technological
 *
 * SUMMARY:
 *   Meta's 'Pay or Okay' model presents a structural dilemma for EU users,
 *   who are compelled to either relinquish their personal data for targeted
 *   advertising or pay a subscription fee to access services like Facebook
 *   and Instagram. This policy has sparked considerable debate about data
 *   privacy, user consent, and the market dominance of tech giants.
 *
 * KEY AGENTS:
 *   - EU Users: Primary targets (powerless/trapped) - must either pay or surrender their data.
 *   - Meta Corporation: Primary beneficiary (institutional/arbitrage) - gains revenue or data.
 *   - Advertisers: Secondary beneficiaries (powerful/arbitrage) - continue to have a platform to reach users.
 *   - Privacy Advocacy Groups: Watchdogs (moderate/constrained) - monitor and challenge data practices.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(meta_pay_or_okay_model, 0.6).
domain_priors:suppression_score(meta_pay_or_okay_model, 0.7).
domain_priors:theater_ratio(meta_pay_or_okay_model, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(meta_pay_or_okay_model, extractiveness, 0.6).
narrative_ontology:constraint_metric(meta_pay_or_okay_model, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(meta_pay_or_okay_model, theater_ratio, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(meta_pay_or_okay_model, tangled_rope).
narrative_ontology:human_readable(meta_pay_or_okay_model, "Meta's \"Pay or Okay\" Data Monetization Model in the EU").
narrative_ontology:topic_domain(meta_pay_or_okay_model, "technological").

domain_priors:requires_active_enforcement(meta_pay_or_okay_model).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(meta_pay_or_okay_model, meta_corporation).
narrative_ontology:constraint_beneficiary(meta_pay_or_okay_model, advertisers).
narrative_ontology:constraint_victim(meta_pay_or_okay_model, eu_users).
narrative_ontology:constraint_victim(meta_pay_or_okay_model, privacy_advocacy_groups).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% EU users are essentially trapped if they want to use social media platforms like Facebook and Instagram. The choice is to pay or give up their data, with limited real alternatives, turning this into a snare.
constraint_indexing:constraint_classification(meta_pay_or_okay_model, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(regional))).

% Privacy advocacy groups are constrained by the resources required to fight Meta's practices, but also benefit from increased awareness of data privacy issues due to the model, leading to a tangled rope classification.
constraint_indexing:constraint_classification(meta_pay_or_okay_model, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% Meta benefits from this model by either gaining revenue from subscriptions or continuing to monetize user data. They have arbitrage options because they can adjust subscription fees and data usage strategies. The effective extraction is low, presenting a Rope.
constraint_indexing:constraint_classification(meta_pay_or_okay_model, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% Advertisers continue to have a platform to reach users, either through targeted ads from those who consent or through general ads to those who pay. They have arbitrage because they can choose where to allocate their ad spend. Presents as a rope constraint.
constraint_indexing:constraint_classification(meta_pay_or_okay_model, rope,
    context(agent_power(powerful),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% The analytical observer recognizes the dual nature of this system: it provides a revenue stream for Meta while extracting data or money from users. The scale of Meta and its global influence makes this a tangled rope.
constraint_indexing:constraint_classification(meta_pay_or_okay_model, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(meta_pay_or_okay_model_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(meta_pay_or_okay_model, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(meta_pay_or_okay_model, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(meta_pay_or_okay_model, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(meta_pay_or_okay_model_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness: The model extracts either data or money from users, justifying a high extractiveness score. Suppression: Users have limited alternatives, given Meta's market dominance and the network effects of its platforms. Theater Ratio: The relatively low theater ratio indicates that the model has a functional purpose beyond mere appearance, as it directly impacts Meta's revenue and data collection.
 *
 * PERSPECTIVAL GAP:
 *   EU users perceive the model as a snare because they are essentially trapped: give up data or pay. Meta views it as a rope, a reasonable exchange for services. Privacy groups see a tangled rope because they have limited influence but are still fighting for users' rights. The analytical observer sees a mixed model that needs careful scrutiny.
 *
 * DIRECTIONALITY LOGIC:
 *   EU users have a high directionality since they are the targets of extraction. Meta has a low directionality as they benefit from the model. Privacy groups are in the middle, constrained in their actions, but still trying to influence the outcome.
 *
 * MANDATROPHY ANALYSIS:
 *   The model is analyzed from the perspective of different stakeholders. It clarifies the distinction between a voluntary transaction (rope) and a forced extraction (snare) by examining the degree of choice EU users truly have.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    user_consent_validity,
    'To what extent is user consent truly voluntary when the alternative is exclusion from a dominant social platform?',
    'Behavioral studies analyzing user decision-making under pressure; legal analysis of consent definitions under GDPR.',
    'If consent is not truly voluntary: The model functions as a pure extraction snare. If consent is generally valid: There is a coordination benefit of providing services in exchange for data or payment.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(user_consent_validity, empirical, 'The degree to which user consent is truly voluntary.').

omega_variable(
    competition_effects,
    'How will the ''pay or okay'' model impact competition within the social media landscape?',
    'Market analysis of user migration patterns; tracking the rise of alternative platforms emphasizing privacy.',
    'If the model entrenches Meta''s dominance: It reinforces the extraction dynamic. If it spurs competition: It could be seen as a rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(competition_effects, empirical, 'The effects of the model on competition.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(meta_pay_or_okay_model, 0, 12).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(meta_tr_t0, meta_pay_or_okay_model, theater_ratio, 0, 0.2).
narrative_ontology:measurement(meta_tr_t6, meta_pay_or_okay_model, theater_ratio, 6, 0.3).
narrative_ontology:measurement(meta_tr_t12, meta_pay_or_okay_model, theater_ratio, 12, 0.35).

% Extraction over time
narrative_ontology:measurement(meta_be_t0, meta_pay_or_okay_model, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(meta_be_t6, meta_pay_or_okay_model, base_extractiveness, 6, 0.6).
narrative_ontology:measurement(meta_be_t12, meta_pay_or_okay_model, base_extractiveness, 12, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(meta_pay_or_okay_model, resource_allocation).
narrative_ontology:affects_constraint(meta_pay_or_okay_model, gdpr_compliance).
narrative_ontology:affects_constraint(meta_pay_or_okay_model, digital_markets_act).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
