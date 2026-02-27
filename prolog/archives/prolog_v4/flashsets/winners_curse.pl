% ============================================================================
% CONSTRAINT STORY: winners_curse
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-01-04
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_winners_curse, []).

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
 *   constraint_id: winners_curse
 *   human_readable: The Winner's Curse in Common Value Auctions
 *   domain: economic/social
 *
 * SUMMARY:
 *   The Winner's Curse is a phenomenon in common value auctions where the
 *   winning bidder tends to overpay, because their winning bid suggests they
 *   have the most optimistic (and likely inflated) estimate of the item's
 *   true worth. The auction benefits the seller and potentially
 *   better-informed bidders at the expense of the naive winning bidder.
 *
 * KEY AGENTS:
 *   - Naive Winning Bidders: Victims (powerless/trapped) who overpay due to inaccurate valuation.
 *   - Auction Sellers: Beneficiaries (institutional/arbitrage) who gain higher revenue.
 *   - Better Informed Bidders: Beneficiaries (powerful/arbitrage) who exploit the curse.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(winners_curse, 0.6).
domain_priors:suppression_score(winners_curse, 0.4).
domain_priors:theater_ratio(winners_curse, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(winners_curse, extractiveness, 0.6).
narrative_ontology:constraint_metric(winners_curse, suppression_requirement, 0.4).
narrative_ontology:constraint_metric(winners_curse, theater_ratio, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(winners_curse, tangled_rope).
narrative_ontology:human_readable(winners_curse, "The Winner's Curse in Common Value Auctions").
narrative_ontology:topic_domain(winners_curse, "economic/social").

domain_priors:requires_active_enforcement(winners_curse).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(winners_curse, auction_sellers).
narrative_ontology:constraint_beneficiary(winners_curse, better_informed_bidders).
narrative_ontology:constraint_victim(winners_curse, naive_winning_bidders).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Perspective of the naive winning bidder who overpays. They are trapped because they've already committed to the auction.
constraint_indexing:constraint_classification(winners_curse, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(local))).

% Perspective of the auction seller who benefits from the winner's curse. They set up the auction and obtain revenue.
constraint_indexing:constraint_classification(winners_curse, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(local))).

% Analytical perspective, classifying the winner's curse as a tangled rope due to the combination of extraction and coordination (price discovery).
constraint_indexing:constraint_classification(winners_curse, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% Perspective of a better informed bidder who exploits the curse by making more accurate bids and winning with better certainty. They can decide to participate or not, thus arbitrage.
constraint_indexing:constraint_classification(winners_curse, rope,
    context(agent_power(powerful),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(local))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(winners_curse_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(winners_curse, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(winners_curse, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(winners_curse, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(winners_curse_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness is 0.6 because the winning bidder loses value due to the curse. Suppression is 0.4 because there are some strategies to mitigate the curse, but they're not always effective or accessible to all bidders. The theater ratio is low because this is a real economic phenomenon, not a performative one.
 *
 * PERSPECTIVAL GAP:
 *   The naive winning bidder sees a snare because they are trapped and lose money. The auction seller sees a rope because they benefit from increased revenue. The better-informed bidder also sees a rope as they can strategically exploit the curse. The analytical observer sees the combined effect as a tangled rope.
 *
 * DIRECTIONALITY LOGIC:
 *   The auction seller and better-informed bidders benefit (low d), while the naive bidder is harmed (high d). The analytical observer sees both extraction and coordination, leading to the tangled rope classification.
 *
 * MANDATROPHY ANALYSIS:
 *   The combination of extraction and coordination requires distinguishing between a pure snare and a tangled rope. The coordination aspect is price discovery, although skewed by the curse. Bidders are attempting to ascertain the true value. Mitigation strategies exist, even though not always successful.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    information_asymmetry,
    'How significant is the information asymmetry among bidders?',
    'Analysis of bidder expertise and available information sources.',
    'High asymmetry strengthens the Snare aspect; Low asymmetry shifts toward Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(information_asymmetry, empirical, 'The degree of information asymmetry affects the intensity of the curse.').

omega_variable(
    bidder_rationality,
    'To what extent are bidders aware of and able to adjust for the winner''s curse?',
    'Behavioral studies of bidding behavior in common value auctions.',
    'Full rationality mitigates the curse (moves toward Rope); irrationality intensifies it (moves toward Snare).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(bidder_rationality, empirical, 'The level of rationality in bidders influences the effects of the curse.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(winners_curse, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(winn_tr_t0, winners_curse, theater_ratio, 0, 0.1).
narrative_ontology:measurement(winn_tr_t5, winners_curse, theater_ratio, 5, 0.2).
narrative_ontology:measurement(winn_tr_t10, winners_curse, theater_ratio, 10, 0.2).

% Extraction over time
narrative_ontology:measurement(winn_be_t0, winners_curse, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(winn_be_t5, winners_curse, base_extractiveness, 5, 0.5).
narrative_ontology:measurement(winn_be_t10, winners_curse, base_extractiveness, 10, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(winners_curse, resource_allocation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
