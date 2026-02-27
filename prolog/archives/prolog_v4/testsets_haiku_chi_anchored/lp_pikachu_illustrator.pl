% ============================================================================
% CONSTRAINT STORY: lp_pikachu_illustrator
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_lp_pikachu_illustrator, []).

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
    constraint_indexing:directionality_override/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: lp_pikachu_illustrator
 *   human_readable: Artificial Value Creation in High-End Collectibles Market
 *   domain: economic/technological
 *
 * SUMMARY:
 *   The Logan Paul Pikachu Illustrator case exemplifies a constraint system
 *   where grading monopolies, celebrity amplification, and information
 *   asymmetry combine to create artificial value extraction from retail
 *   participants. The 1999 Pikachu Illustrator card is genuinely scarce
 *   (fewer than 40 known copies exist), but the $5.275M sale price (2021) and
 *   subsequent market volatility suggest extraction mechanisms layered onto
 *   authentic rarity. The constraint operates at the intersection of
 *   collectibles authentication (PSA grading services), cultural
 *   amplification (celebrity social media), and market structure (auction
 *   houses vs. direct sales). From the retail collector's perspective, the
 *   system is a snare: trapped by information asymmetry about grading
 *   reliability, unable to verify celebrity-driven valuations, and locked
 *   into FOMO-driven purchasing decisions. From the beneficiary groups
 *   (grading services, celebrity promoters, early card holders), the system
 *   is a coordination mechanism or pure profit opportunity. The theater_ratio
 *   (0.81) reflects the heavily performative character of auction house
 *   rituals, celebrity announcements, and 'investment narrative' marketing in
 *   a market where authentic price discovery has largely migrated to social
 *   media and celebrity influence.
 *
 * KEY AGENTS:
 *   - Retail Collectors: Primary victims (powerless/trapped) — absorb valuation volatility and cannot verify grading authenticity or exit at comparable prices
 *   - PSA/BGS Grading Services: Primary beneficiary (institutional/arbitrage) — monopoly gatekeeper for authentication; benefits from network effects and cannot be easily competed away
 *   - Celebrity Promoters (Logan Paul): Secondary beneficiary (powerful/arbitrage) — leverage cultural capital to amplify prices; extract through influence with no participation in grading or curation
 *   - Early Card Holders: Tertiary beneficiary (institutional/arbitrage) — benefit from price appreciation before celebrity amplification; can arbitrage between collectors and speculators
 *   - Auction Houses (Sotheby's, Heritage): Institutional actor (institutional/constrained) — maintain performative role (theater=0.81) but have ceded price discovery to social media; theater increasing over interval
 *   - Secondary Buyers: Secondary victim (moderate/constrained) — trap effect: bought at peak valuations following celebrity promotion; cannot exit at comparable prices
 *   - Market Price Discovery: Abstract victim (powerless/trapped) — contaminated by celebrity signals; no mechanism for returning to fundamental scarcity valuation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(lp_pikachu_illustrator, 0.58).
domain_priors:suppression_score(lp_pikachu_illustrator, 0.68).
domain_priors:theater_ratio(lp_pikachu_illustrator, 0.81).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(lp_pikachu_illustrator, extractiveness, 0.58).
narrative_ontology:constraint_metric(lp_pikachu_illustrator, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(lp_pikachu_illustrator, theater_ratio, 0.81).

% --- Constraint claim ---
narrative_ontology:constraint_claim(lp_pikachu_illustrator, snare).
narrative_ontology:human_readable(lp_pikachu_illustrator, "Artificial Value Creation in High-End Collectibles Market").
narrative_ontology:topic_domain(lp_pikachu_illustrator, "economic/technological").

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(lp_pikachu_illustrator, grading_services).
narrative_ontology:constraint_beneficiary(lp_pikachu_illustrator, celebrity_promoters).
narrative_ontology:constraint_beneficiary(lp_pikachu_illustrator, early_card_holders).
narrative_ontology:constraint_beneficiary(lp_pikachu_illustrator, auction_houses).
narrative_ontology:constraint_victim(lp_pikachu_illustrator, retail_collectors).
narrative_ontology:constraint_victim(lp_pikachu_illustrator, market_price_discovery).
narrative_ontology:constraint_victim(lp_pikachu_illustrator, secondary_buyers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: RETAIL COLLECTOR (SNARE) — Small-scale collectors cannot verify grading authenticity or exit the celebrity-driven valuation regime. Trapped by information asymmetry and FOMO-driven marketing. d≈0.92, f(d)≈1.40, σ=1.2 → χ≈0.96. Experiences pure extraction with no coordination benefit.
constraint_indexing:constraint_classification(lp_pikachu_illustrator, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: INSTITUTIONAL COLLECTOR (TANGLED ROPE) — Museums, funds, and established collectors benefit from liquidity and price discovery but constrained by need to participate in celebrity-driven auctions to maintain competitive positions. d≈0.58, f(d)≈0.72, σ=1.0 → χ≈0.42. Mixed extraction and coordination.
constraint_indexing:constraint_classification(lp_pikachu_illustrator, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: GRADING SERVICE (ROPE) — Solves coordination problem of authentication and grade standardization. Benefits from network effects and scale. d≈0.08, f(d)≈-0.10, σ=1.2 → χ≈-0.07. Net beneficiary from coordination function. Can arbitrage between markets.
constraint_indexing:constraint_classification(lp_pikachu_illustrator, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: CELEBRITY PROMOTER (SNARE) — Wields outsized influence over market via social media and cultural capital. Creates artificial scarcity narratives and FOMO. Extracts through attention capture and price inflation. d≈0.02, f(d)≈-0.14, σ=1.2 → χ≈-0.08. Negative chi (net beneficiary); classification as snare because they wield extraction mechanism even though they don't experience extraction.
constraint_indexing:constraint_classification(lp_pikachu_illustrator, snare,
    context(agent_power(powerful),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: AUCTION HOUSE ECOSYSTEM (PITON) — Traditional auction mechanisms (Sotheby's, Heritage Auctions) maintain theater through formal bidding rituals and prestige associations but increasingly hollowed by social media-driven price setting. theater_ratio≈0.81 satisfies piton gate. Institutional inertia drives continued participation despite celebrity social media as primary price discovery mechanism.
constraint_indexing:constraint_classification(lp_pikachu_illustrator, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (SNARE) — Sees the structural mechanism: grading monopoly + celebrity amplification + information asymmetry = extraction from retail participants into beneficiary groups. d≈0.72, f(d)≈1.15, σ=1.2 → χ≈0.80. This is the civilizational-scope reality: the constraint is a universal extraction mechanism using the Pikachu card as exemplar.
constraint_indexing:constraint_classification(lp_pikachu_illustrator, snare,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(lp_pikachu_illustrator_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(lp_pikachu_illustrator, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(lp_pikachu_illustrator, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(lp_pikachu_illustrator, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(lp_pikachu_illustrator, TR),
    TR >= 0.70.

:- end_tests(lp_pikachu_illustrator_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. The constraint extracts value through: (1) grading monopoly: PSA/BGS control quality signal; (2) information asymmetry: retail buyers cannot verify grade reliability or detect overvaluation; (3) celebrity amplification: creates artificial FOMO and herd behavior; (4) illiquidity trap: high-value cards have narrow buyer bases, trapping retail purchasers. The value extraction is not total (some authentic scarcity premium is justified), but the celebrity-driven premium layer (estimated 30-70% of post-promotion prices) is pure extraction. Suppression (0.68): High. Multiple barriers prevent exit and price discovery: retail buyers lack expertise to contest grades, celebrity influence creates social cost of disagreeing with promoted valuations, grading service reputations constrain independent verification, auction house rituals create prestige associations that inhibit price haggling. Theater_ratio (0.81): Very high. The constraint's theatrical elements include: auction house bidding rituals (bids are public performance, not price-setting), celebrity social media announcements (performative content designed to generate buzz), 'investment narrative' marketing (positioning collectibles as asset class rather than cultural artifacts), PSA slab aesthetics (the cardboard case with grade label is more marketable than the card itself). The theater has increased from 0.52 → 0.81 over the interval as celebrity influence has displaced traditional curatorship. The functional core (authentication, scarcity documentation) is real but comprises < 20% of observed market behavior.
 *
 * PERSPECTIVAL GAP:
 *   Retail collectors see a snare: trapped in illiquid positions at inflated valuations with no exit mechanism at comparable prices. Institutional collectors see tangled rope: participate willingly in celebrity-amplified auctions (for prestige and price discovery) while being extracted from by volatility. Grading services see rope: providing genuine coordination service (authentication), benefiting from network effects, able to arbitrage between markets. Celebrity promoters see a profit opportunity, not a constraint — they are the extraction mechanism itself, not subject to extraction. Auction houses see themselves as coordinating mechanism but analysts see them as pitons: maintaining theater through prestige rituals that no longer drive prices. The analytical observer sees snare: the system is structured extraction from information-asymmetric retail buyers into beneficiary groups (graders, celebrities, early holders). The gap between retail and analytical perspectives is the core diagnostic.
 *
 * DIRECTIONALITY LOGIC:
 *   Retail Collectors: Victim + trapped → d≈0.92, f(d)≈1.40. Maximum extraction. Cannot exit, cannot verify, cannot compete with celebrity influence. Institutional Collectors: Victim + constrained → d≈0.58, f(d)≈0.72. Moderate extraction but with agency; participate strategically. Grading Services: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.10. Net beneficiary. Provide genuine coordination function and can arbitrage across markets. Celebrity Promoters: Extractor + arbitrage → d≈0.02, f(d)≈-0.14. Net beneficiary (negative extraction = they profit). Do not experience the constraint; they ARE the constraint mechanism. Auction Houses: Institutional + constrained → d≈0.45, f(d)≈0.54. Mixed; maintaining performative role but losing functional control. Secondary Buyers: Victim + mobile (post-purchase, trapped) → d≈0.85, f(d)≈1.18. High extraction; trapped after purchase despite mobility before. Market Price Discovery: Victim + trapped → d≈1.0, f(d)≈1.42. Maximum extraction; abstract collective that cannot exit or organize.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLVED: This constraint is classified as Snare (extractiveness=0.58, suppression=0.68, mandatrophy_resolved=true) because: (1) It has a clear victim group (retail collectors, secondary buyers) experiencing high directional extraction (d≈0.92); (2) It has clear beneficiary groups (grading services, celebrity promoters, early holders) experiencing negative extraction (net beneficiaries); (3) The suppression mechanism is structural: information asymmetry about grading reliability, social cost of public disagreement with promoted valuations, and illiquidity trap prevent alternative coordination mechanisms from emerging; (4) The constraint does NOT have a genuine coordination function that justifies the extraction — the coordination function (authentication via PSA) comprises <20% of observed market behavior and could be satisfied by lower-extraction mechanisms (decentralized grading, transparent methodologies). The mandatrophy is resolved by distinguishing the REAL coordination function (authentication, 0.05 extraction equivalent) from the THEATRICAL addition (celebrity amplification, 0.53 extraction equivalent). The Snare classification covers the full 0.58 because the theatrical layer SUPPRESSES alternative coordination mechanisms from emerging. If PSA grading operated independently of celebrity signals and celebrity amplification were removed as a suppression mechanism, the residual constraint would be Rope (authentication coordination, ε≈0.05). The current Snare reflects the institutional bundling of genuine coordination with extraction-enabling theater.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    intrinsic_card_value_threshold,
    'What fraction of the $5.275M sale price (2021) represents authentic scarcity value vs. artificial celebrity-amplified premium?',
    'Hedonic pricing model controlling for card properties (print year, condition pre-grading, rarity within print run) vs. celebrity holder factor. Comparison with identical cards held by non-celebrities.',
    'If authentic fraction > 70%: constraint is coordination problem (Rope from all perspectives). If < 30%: constraint is pure extraction (Snare from all perspectives confirmed).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(intrinsic_card_value_threshold, empirical, 'Separation of intrinsic scarcity from celebrity amplification premium').

omega_variable(
    grading_service_independence,
    'Are PSA/BGS grades assigned independently of market signals and holder reputation, or are there systematic biases favoring high-profile cards?',
    'Comparative analysis of grade distributions for identical card conditions across holders of different prominence. Longitudinal tracking of re-grading outcomes when cards change hands.',
    'If independent: grading is genuine coordination mechanism. If biased: grading service is captured by market forces and becomes enforcement mechanism for artificial value creation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(grading_service_independence, empirical, 'Whether grading services maintain independence from market reputation effects').

omega_variable(
    secondary_market_liquidity_trap,
    'Does the Pikachu Illustrator market exhibit genuine two-way liquidity at high price points, or is it a one-way extraction where retail buyers cannot exit at comparable prices?',
    'Time-series analysis of bid-ask spreads, volume at different price levels, and holding period distributions. Count of failed sell attempts at declining prices after purchase.',
    'If genuine two-way liquidity: constraint has coordination function (market mechanism works). If one-way: buyers are trapped in illiquid positions, confirming snare classification and extraction from retail participants.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(secondary_market_liquidity_trap, empirical, 'Whether high-value card markets exhibit genuine two-way liquidity or one-way extraction').

omega_variable(
    cultural_capital_substitution,
    'To what degree does celebrity holder status substitute for authentic card properties in determining price, and is this substitution mechanism stabilizing or destabilizing?',
    'Price sensitivity analysis to celebrity promotions vs. card authentication updates. Correlation between celebrity social media activity and market volatility. Recovery analysis following celebrity reputation crises.',
    'If highly substitutable and destabilizing: constraint is speculative bubble mechanism (Snare confirmed). If weakly substitutable: celebrity factor is minor market seasoning (Rope with Rope-majority perspectives).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cultural_capital_substitution, empirical, 'Degree of cultural capital substitution and market stability implications').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(lp_pikachu_illustrator, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(lppi_theater_presale, lp_pikachu_illustrator, theater_ratio, 0, 0.52).
narrative_ontology:measurement(lppi_theater_postpurchase, lp_pikachu_illustrator, theater_ratio, 3, 0.68).
narrative_ontology:measurement(lppi_theater_peak, lp_pikachu_illustrator, theater_ratio, 6, 0.81).

% Extraction over time
narrative_ontology:measurement(lppi_extract_presale, lp_pikachu_illustrator, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(lppi_extract_postpurchase, lp_pikachu_illustrator, base_extractiveness, 3, 0.48).
narrative_ontology:measurement(lppi_extract_peak, lp_pikachu_illustrator, base_extractiveness, 6, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(lp_pikachu_illustrator, information_standard).
narrative_ontology:affects_constraint(lp_pikachu_illustrator, collectibles_authentication_monopoly).
narrative_ontology:affects_constraint(lp_pikachu_illustrator, celebrity_amplification_market_distortion).
narrative_ontology:affects_constraint(lp_pikachu_illustrator, fomo_driven_asset_bubbles).

% DUAL FORMULATION NOTE:
% This constraint decomposes into three structurally distinct claims: (1) PSA grading as authentication mechanism (Rope, ε≈0.05, genuine coordination); (2) Celebrity influence as price discovery mechanism (Snare, ε≈0.50, pure extraction); (3) Auction house role in market structure (Piton, ε≈0.25, degraded ritual). The integrated story reflects the bundled institutional reality: grading monopoly + celebrity amplification suppress alternative authentication mechanisms. When these are disaggregated (grading as independent service vs. celebrity as separate signal channel), the snare classification transfers entirely to the celebrity amplification layer.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(lp_pikachu_illustrator, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
