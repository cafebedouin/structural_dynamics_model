% ============================================================================
% CONSTRAINT STORY: dn_paywall
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-05-02
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_dn_paywall, []).

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
 *   constraint_id: dn_paywall
 *   human_readable: Dagens Nyheter Digital Subscription Paywall
 *   domain: economic/technological
 *
 * SUMMARY:
 *   Dagens Nyheter's digital paywall is a mechanism to monetize online
 *   content, balancing revenue generation with restricted access. It presents
 *   a structural dilemma between sustaining quality journalism and limiting
 *   information dissemination. The paywall impacts different actors in
 *   various ways, leading to differing perspectives on its overall effect.
 *
 * KEY AGENTS:
 *   - Dagens Nyheter: Primary beneficiary (institutional/arbitrage) - revenue from subscriptions
 *   - Occasional Readers: Primary victim (powerless/trapped) - restricted access to individual articles
 *   - Moderate News Consumers: (moderate/constrained) - balances cost with access
 *   - Information Access: (powerless/trapped) - abstract collective good, potentially affected by the restriction of information
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dn_paywall, 0.55).
domain_priors:suppression_score(dn_paywall, 0.45).
domain_priors:theater_ratio(dn_paywall, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dn_paywall, extractiveness, 0.55).
narrative_ontology:constraint_metric(dn_paywall, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(dn_paywall, theater_ratio, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dn_paywall, tangled_rope).
narrative_ontology:human_readable(dn_paywall, "Dagens Nyheter Digital Subscription Paywall").
narrative_ontology:topic_domain(dn_paywall, "economic/technological").

domain_priors:requires_active_enforcement(dn_paywall).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dn_paywall, dagens_nyheter).
narrative_ontology:constraint_victim(dn_paywall, occasional_readers).
narrative_ontology:constraint_victim(dn_paywall, information_access).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% The occasional reader who only needs access to a single article faces a high barrier to entry. They are trapped by the paywall with no easy exit option.
constraint_indexing:constraint_classification(dn_paywall, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(national))).

% Regular readers are constrained. They benefit from the quality of journalism funded by the paywall but face the cost of subscription.
constraint_indexing:constraint_classification(dn_paywall, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% Dagens Nyheter benefits from the paywall through increased revenue, enabling the continuation of journalistic activities.
constraint_indexing:constraint_classification(dn_paywall, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% From an analytical perspective, the paywall represents a tangled rope. It solves the collective action problem of funding quality journalism but also restricts access to information.
constraint_indexing:constraint_classification(dn_paywall, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(dn_paywall_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(dn_paywall, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(dn_paywall, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(dn_paywall, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(dn_paywall_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.55): Moderate. The paywall extracts value from readers in exchange for content access. The extraction is not maximal as some content remains free. Suppression (0.45): Moderate. There are alternative news sources, but the paywall restricts access to DN's specific content. Theater ratio (0.20): Low. The paywall is primarily functional, focusing on revenue generation rather than performative aspects.
 *
 * PERSPECTIVAL GAP:
 *   The occasional reader sees a snare because they are effectively blocked from accessing content without a subscription. Regular readers experience a tangled rope, balancing the costs of subscription with the benefits of access. Dagens Nyheter views the paywall as a rope, enabling the funding of quality journalism. The analytical observer sees the overall structure as a tangled rope, balancing the benefits and costs to society.
 *
 * DIRECTIONALITY LOGIC:
 *   Dagens Nyheter benefits directly from the paywall through subscription revenue (low d). Occasional readers bear the cost of restricted access (high d). Regular readers experience a mix of benefits and costs (moderate d). The analytical observer assesses the net impact on society, considering both the value of quality journalism and the cost of restricted access (moderate d).
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    quality_vs_access,
    'What is the optimal balance between funding high-quality journalism and ensuring broad access to information?',
    'Empirical studies on the impact of paywalls on readership and revenue, coupled with societal values regarding information accessibility.',
    'Determines the degree to which paywalls are socially beneficial or detrimental.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(quality_vs_access, preference, 'Optimal balance between funding quality journalism and ensuring access').

omega_variable(
    alternative_funding_models,
    'Are there alternative funding models that could provide similar revenue without restricting access to the same extent?',
    'Experiments with different funding models, such as micropayments, philanthropic support, or public funding.',
    'Could lead to the development of more equitable and sustainable models for funding journalism.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(alternative_funding_models, empirical, 'Alternative funding models for journalism').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dn_paywall, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dn_p_tr_t0, dn_paywall, theater_ratio, 0, 0.1).
narrative_ontology:measurement(dn_p_tr_t5, dn_paywall, theater_ratio, 5, 0.15).
narrative_ontology:measurement(dn_p_tr_t10, dn_paywall, theater_ratio, 10, 0.2).

% Extraction over time
narrative_ontology:measurement(dn_p_be_t0, dn_paywall, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(dn_p_be_t5, dn_paywall, base_extractiveness, 5, 0.5).
narrative_ontology:measurement(dn_p_be_t10, dn_paywall, base_extractiveness, 10, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dn_paywall, resource_allocation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
