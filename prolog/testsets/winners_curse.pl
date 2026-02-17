% ============================================================================
% CONSTRAINT STORY: winners_curse
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-15
% ============================================================================

:- module(constraint_winners_curse, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    domain_priors:emerges_naturally/1,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2.

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
 *   The Winner's Curse is a statistical phenomenon in common value auctions where
 *   the winning bidder tends to overpay. This occurs because the winner is
 *   typically the agent with the most optimistic (and likely overestimated)
 *   valuation of an item with uncertain true worth. The auction format itself
 *   functions as a coordination mechanism for price discovery, but systematically
 *   extracts this "optimism surplus" from the least-informed or most
 *   psychologically committed bidders.
 *
 * KEY AGENTS (by structural relationship):
 *   - Aggressive/Overconfident Bidders: Primary target (powerless/trapped) — bears the extraction by overpaying.
 *   - Sellers & Auctioneers: Primary beneficiary (institutional/arbitrage) — benefits from the inflated winning bid.
 *   - Sophisticated Bidders: Secondary beneficiary (institutional/arbitrage) — uses models to avoid the curse, benefiting from the price discovery mechanism.
 *   - Analytical Observer: Sees the full structure of coordination and extraction.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(winners_curse, 0.65).
domain_priors:suppression_score(winners_curse, 0.60).   % Structural property (raw, unscaled).
domain_priors:theater_ratio(winners_curse, 0.65).       % Piton detection (>= 0.70)

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(winners_curse, extractiveness, 0.65).
narrative_ontology:constraint_metric(winners_curse, suppression_requirement, 0.60).
narrative_ontology:constraint_metric(winners_curse, theater_ratio, 0.65).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(winners_curse, tangled_rope).
narrative_ontology:human_readable(winners_curse, "The Winner's Curse in Common Value Auctions").

% --- Binary flags ---
domain_priors:requires_active_enforcement(winners_curse). % Winner is legally bound to pay.

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(winners_curse, sellers).
narrative_ontology:constraint_beneficiary(winners_curse, auctioneers).
%
% Who bears disproportionate cost?
narrative_ontology:constraint_victim(winners_curse, aggressive_bidders).
narrative_ontology:constraint_victim(winners_curse, overconfident_investors).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × f(d) × σ(S)
   ========================================================================== */

% PERSPECTIVE 1: THE OVERCONFIDENT BIDDER (SNARE)
% Trapped by bidding "frenzy" and psychological commitment at the immediate horizon.
% The high extraction and suppression of rational valuation models create a snare.
constraint_indexing:constraint_classification(winners_curse, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: THE SELLER / AUCTIONEER (ROPE)
% For the beneficiary, the auction is a pure coordination mechanism for price
% discovery that maximizes their return. The negative effective extraction reflects this.
constraint_indexing:constraint_classification(winners_curse, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% Sees the dual function: a genuine coordination mechanism (price discovery)
% coupled with a systematic, asymmetric extraction from a specific class of
% participants. This is the definition of a Tangled Rope.
constraint_indexing:constraint_classification(winners_curse, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(winners_curse_tests).

test(perspectival_gap) :-
    % Verify Snare for the powerless amateur vs Rope for the institutional beneficiary.
    constraint_indexing:constraint_classification(winners_curse, snare, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(winners_curse, rope, context(agent_power(institutional), _, _, _)),
    constraint_indexing:constraint_classification(winners_curse, tangled_rope, context(agent_power(analytical), _, _, _)).

test(extraction_threshold) :-
    % High extraction (> 0.46) triggers lifecycle drift measurement requirement.
    narrative_ontology:constraint_metric(winners_curse, extractiveness, E),
    E >= 0.46.

test(tangled_rope_conditions_met) :-
    % Verify the structural properties required for a Tangled Rope classification are declared.
    narrative_ontology:constraint_beneficiary(winners_curse, _),
    narrative_ontology:constraint_victim(winners_curse, _),
    domain_priors:requires_active_enforcement(winners_curse).

:- end_tests(winners_curse_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The base extractiveness is set to 0.65. This value is high enough to
 *   classify as a Snare for the trapped victim (χ ≈ 0.65 * 1.42 * 0.8 ≈ 0.74),
 *   but low enough to classify as a Tangled Rope for the analytical observer
 *   (χ ≈ 0.65 * 1.15 * 1.2 ≈ 0.897), which correctly captures the dual nature
 *   of the constraint. The original `emerges_naturally` flag was removed; while
 *   the curse is a statistical inevitability *within* an auction, the auction
 *   itself is a human-designed, enforced system, not a law of nature.
 *
 * PERSPECTIVAL GAP:
 *   The gap is stark. For the overconfident bidder, the auction is a Snare that
 *   exploits their optimism. For the seller, it's a Rope—an efficient tool for
 *   price discovery. The analytical observer sees both sides, classifying it as
 *   a Tangled Rope: a system with a valid coordination function that also
 *   produces asymmetric, predictable extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   - Beneficiaries: `sellers` and `auctioneers` directly profit from the highest
 *     possible bid, which the curse helps generate.
 *   - Victims: `aggressive_bidders` and `overconfident_investors` are the agents
 *     whose valuations are most likely to be statistical outliers, causing them
 *     to overpay and bear the cost.
 *   This clear division of costs and benefits drives the directionality calculation
 *   and the resulting perspectival gap.
 *
 * MANDATROPHY ANALYSIS:
 *   [RESOLVED MANDATROPHY]
 *   The framework correctly avoids mislabeling the auction system as pure
 *   predation (Snare) or pure coordination (Rope). By identifying the Tangled
 *   Rope structure from the analytical view, it acknowledges the valid price
 *   discovery function while simultaneously accounting for the predictable,
 *   extractive harm to a specific class of participants.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% /5 form: narrative detail for story context
omega_variable(
    winners_curse_extraction_intent,
    'Is the overpayment captured by the curse a functional necessity of price discovery or a feature exploited by predatory design?',
    'Comparative audit of auction formats (e.g., English vs. Vickrey vs. sealed-bid) and their effect on the magnitude of overpayment in high-stakes environments.',
    'If necessity: The system is closer to a pure Rope with a known flaw. If exploited design: The system is closer to a pure Snare.',
    confidence_without_resolution(medium)
).

% /3 form: typed classification for reporting engine (REQUIRED)
narrative_ontology:omega_variable(omega_winners_curse_extraction_intent, empirical, 'Distinguishing between overpayment as a system bug vs. an exploited feature.').

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(winners_curse, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio: Modeling the intensification of performative "Auction Frenzy" (0.35 -> 0.65).
narrative_ontology:measurement(winners_tr_t0, winners_curse, theater_ratio, 0, 0.35).
narrative_ontology:measurement(winners_tr_t5, winners_curse, theater_ratio, 5, 0.50).
narrative_ontology:measurement(winners_tr_t10, winners_curse, theater_ratio, 10, 0.65).

% Extraction: Tracking the intensification of the "Optimism Surplus" capture (0.55 -> 0.65).
narrative_ontology:measurement(winners_ex_t0, winners_curse, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(winners_ex_t5, winners_curse, base_extractiveness, 5, 0.60).
narrative_ontology:measurement(winners_ex_t10, winners_curse, base_extractiveness, 10, 0.65).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% An auction is a classic resource_allocation mechanism.
narrative_ontology:coordination_type(winners_curse, resource_allocation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% No overrides needed. The structural derivation from beneficiary/victim
% declarations accurately models the dynamics of this constraint.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */