% ============================================================================
% CONSTRAINT STORY: winners_curse
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
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
 *   constraint_id: winners_curse
 *   human_readable: The Winner's Curse in Common Value Auctions
 *   domain: economic/social
 *
 * SUMMARY:
 *   The winner's curse in common-value auctions presents a structural
 *   paradox: it is simultaneously a coordination mechanism that prevents bid
 *   collusion and disciplines strategy, and an extraction mechanism that
 *   systematically transfers wealth from winning bidders to organizers and
 *   informed non-winners. The curse emerges when bidders compete in an
 *   auction where the item's true value is unknown and common to all bidders,
 *   but each bidder has only a private, noisy estimate. Conditional on
 *   winning, a bidder's estimate is biased upward by selection: the winner is
 *   the bidder whose estimate was highest, which, in a world of estimation
 *   noise, tends to be the most optimistic. The winner therefore overpays
 *   relative to the true value. This phenomenon was first documented
 *   empirically in oil lease auctions (Capen, Clapp, Campbell 1971) and has
 *   since been found in mergers and acquisitions, securities trading, and
 *   experimental settings. The constraint exhibits all characteristics of a
 *   Tangled Rope: it requires active enforcement (bidders must be taught
 *   about and adjust for the curse), it provides genuine coordination
 *   (prevents collusion, enables price discovery), and it extracts asymmetric
 *   value (winners lose, organizers and informed non-winners gain). The
 *   curse's extractiveness (0.52) has increased over the interval from 0.28,
 *   reflecting both greater awareness of the phenomenon and insufficient
 *   bidder adjustment—bidders know about the curse but are constrained by
 *   cognitive and institutional factors that prevent full correction.
 *
 * KEY AGENTS:
 *   - Winning Bidder: Primary victim (powerless/trapped) — bears overpayment cost; no exit option after bidding. Extraction occurs at contract signing.
 *   - Auction Organizer: Primary beneficiary (institutional/arbitrage) — benefits from revenue that reflects overbidding. Mobile: can adjust rules.
 *   - Non-Winning Bidders: Secondary beneficiary (organized/mobile) — indirectly benefit from systematic overbidding by competitors. Can exit by choosing which auctions to enter.
 *   - Repeat Bidders: Constrained victim (moderate/constrained) — must bid-shade to account for curse; reduces their surplus even when they win. Face learning lag.
 *   - Auction Market (Analytical Level): Sees curse as both coordination and extraction. Civilizational view reveals structural necessity.
 *   - Classical Economic Theory: Institutional observer (institutional/constrained) — theory predicts curse; practice doesn't correct. Performative acceptance without implementation.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(winners_curse, 0.52).
domain_priors:suppression_score(winners_curse, 0.65).
domain_priors:theater_ratio(winners_curse, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(winners_curse, extractiveness, 0.52).
narrative_ontology:constraint_metric(winners_curse, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(winners_curse, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(winners_curse, tangled_rope).
narrative_ontology:human_readable(winners_curse, "The Winner's Curse in Common Value Auctions").
narrative_ontology:topic_domain(winners_curse, "economic/social").

domain_priors:requires_active_enforcement(winners_curse).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(winners_curse, auction_organizers).
narrative_ontology:constraint_beneficiary(winners_curse, non_winning_bidders).
narrative_ontology:constraint_victim(winners_curse, winning_bidder).
narrative_ontology:constraint_victim(winners_curse, epistemic_accuracy).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: WINNING BIDDER (SNARE) — Trapped in asymmetric information environment. Bidder's own estimate is biased high when winning (selection effect). No exit option once the bid is submitted and won. Bears full extraction cost: overpayment = (own estimate) - (true value). d≈0.92, f(d)≈1.38, σ=0.8 → χ≈0.57.
constraint_indexing:constraint_classification(winners_curse, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: REPEAT BIDDERS (TANGLED ROPE) — Constrained by learning lag and coordination failures. Benefit from the coordination function of auction mechanisms (price discovery, efficient allocation intent). But suffer extraction: rational bidders must lower bids to account for winners curse, reducing their own probability of winning. Coordination (auction mechanism allocates to highest-value bidder) + extraction (bid-shading reduces surplus). d≈0.68, f(d)≈1.05, σ=1.0 → χ≈0.55.
constraint_indexing:constraint_classification(winners_curse, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: AUCTION ORGANIZER (ROPE) — Institutional beneficiary. Organizer benefits from the winners curse as a coordination mechanism: the curse's existence creates bid discipline and reduces strategic underbidding. Organizer has exit options (can adjust auction rules, format, or reserve price). Experiences the curse as a beneficial equilibrium feature, not as coercion. d≈0.08, f(d)≈-0.11, σ=1.0 → χ≈-0.06. Net beneficiary.
constraint_indexing:constraint_classification(winners_curse, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: NON-WINNING BIDDERS (ROPE) — Organized group that indirectly benefits from winners curse. Their lower bids succeed when other bidders overshoot due to the curse. Mobile: can choose which auctions to enter. Benefit from the coordination function of accurate price discovery and from the extraction cost borne by overbidders. d≈0.25, f(d)≈0.15, σ=1.0 → χ≈0.08. Small positive extraction in their favor.
constraint_indexing:constraint_classification(winners_curse, rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: ANALYTICAL OBSERVER (TANGLED ROPE) — Long-horizon view reveals that winners curse serves a dual function: (1) Coordination — the curse disciplines bidding and prevents collusive underbidding; (2) Extraction — the curse systematically transfers wealth from winners to organizers and non-winners. The curse persists because both functions are structurally necessary in equilibrium. Cannot be eliminated without breaking either auction discipline or price discovery. d≈0.55, f(d)≈0.78, σ=1.2 → χ≈0.51.
constraint_indexing:constraint_classification(winners_curse, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 6: CLASSICAL ECONOMIC THEORY (PITON) — The winners curse is now largely understood through Bayesian game theory and information economics. Yet auction practice often ignores the theoretical predictions. Real-world bidders don't adjust as theory predicts, suggesting the curse persists partly through institutional inertia (bidders haven't internalized the correction) rather than as a feature of optimal equilibrium. theater_ratio=0.58 indicates moderate performative content: the narrative of 'competitive bidding determines value' persists despite evidence that selection bias dominates. The theory exists; the correction mechanism doesn't deploy.
constraint_indexing:constraint_classification(winners_curse, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(winners_curse_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(winners_curse, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(winners_curse, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(winners_curse, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(winners_curse, TR),
    TR >= 0.70.

:- end_tests(winners_curse_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): The curse extracts an average of 15-30% of value from winning bidders in common-value settings (empirical estimates from oil leases, M&A). The extraction is not universal—private-value auctions avoid the curse entirely. The measured 0.52 reflects that most auctions contain mixed common and private value components. Suppression (0.65): High. Barriers to correction include (1) cognitive: bidders struggle to perform the Bayesian adjustment required; (2) institutional: auction organizers have no incentive to educate bidders; (3) structural: information asymmetry cannot be fully eliminated without destroying the auction mechanism. The suppression reflects active resistance to awareness and correction. Theater ratio (0.58): Moderate. The curse is partly real (selection effect in estimation noise is genuine) and partly performative (organizers narrative-frame it as 'market discipline' rather than 'extraction'). The theater has increased over the interval as the phenomenon became well-known but bidder behavior didn't change proportionally—suggesting institutional performance substituted for actual correction.
 *
 * PERSPECTIVAL GAP:
 *   The winning bidder perceives pure extraction (Snare) — they are trapped and overpay. The repeat bidder sees mixed coordination and extraction (Tangled Rope) — auctions coordinate supply and demand, but winners curse bids them down. The non-winning bidders see pure coordination (Rope) — the mechanism works to their advantage. The organizer sees beneficial coordination (Rope) — the curse prevents collusion and generates revenue. The analytical observer sees the full system: the curse is both coordination (prevents collusion) and extraction (transfers wealth from winners). The classical economist sees the curse as a known phenomenon that persists despite being well-understood (Piton) — the persistence is due to institutional inertia, not structural necessity. The perspectival gap reaches its maximum when comparing the winning bidder (snare: d≈0.92) to the organizer (rope: d≈0.08). The same constraint appears as severe extraction to one and beneficial coordination to the other.
 *
 * DIRECTIONALITY LOGIC:
 *   Winning bidder: Victim + trapped → d≈0.92, f(d)≈1.38. Maximum extraction. No exit option. Winning bid is locked in; realized value is almost always below bid. Repeat bidders: Victim + constrained → d≈0.68, f(d)≈1.05. High extraction but not maximal. Can exit by choosing not to bid, but constrained by need to participate in market. Must bid-shade, reducing own surplus. Non-winning bidders: Beneficiary + mobile → d≈0.25, f(d)≈0.15. Low extraction cost; actually benefit from others' overbidding. Mobile: can choose which auctions to enter. Auction organizer: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.11. Net beneficiary. Can exit (set reserve price, change rules). Benefits directly from winning bid inflated by curse. Classical economist: Analytical perspective. The curse is well-modeled but not corrected, suggesting theater is masking structural necessity.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by recognizing that the winners curse serves both coordination and extraction functions simultaneously, and neither can be eliminated without breaking the auction mechanism. Coordination benefit: The curse disciplines bidders and prevents collusive underbidding. If winners curse were eliminated (through perfect information or alternative mechanism), bidders would face incentive to collude, reducing revenue and misallocating items. Extraction harm: Winners systematically overpay relative to true value. This wealth transfer is real and asymmetric. The resolution: The curse is a necessary feature of auction equilibrium under common values. It is not a policy failure but a structural feature. The Tangled Rope classification correctly identifies it as both coordination (auction mechanism works) and extraction (winners pay premium). The increase in theater_ratio from 0.38 to 0.58 over the interval indicates that awareness of the curse has grown without proportional correction in bidder behavior, suggesting performative knowledge (people know about the curse) is substituting for real correction (adjusting bids accordingly). The organizer's silence on correction mechanisms indicates complicity: they benefit from the curse and have no incentive to reduce theater or suppression.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    common_value_vs_private_value,
    'To what degree is a specific auction common-value vs. private-value? How is the distinction empirically verified when bidders hold heterogeneous beliefs about fundamentals?',
    'Post-auction data: comparison of actual realized value to winning bid. Strong correlation (realized ≈ winning bid) suggests private value. Large gap (realized << winning bid) suggests common value with curse. But confounded by bidder irrationality.',
    'If common-value component is small (ε→0.20): winners curse is weak, constraint approaches rope-only. If large (ε→0.75): curse is severe, constraint approaches snare.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(common_value_vs_private_value, empirical, 'Empirical degree of common value in auction').

omega_variable(
    bidder_rationality_and_curse_persistence,
    'Do experienced bidders gradually correct for the winners curse through learning, or does the curse persist as a stable feature of bidder irrationality?',
    'Longitudinal auction data: track individual bidders across multiple auctions; measure whether overbidding decreases with experience. Lab experiments with repeated rounds.',
    'If bidders learn (curse diminishes): suppression ↓, theater ↑ (curse becomes performative spectacle). If curse persists: suppression stable, extraction structural.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(bidder_rationality_and_curse_persistence, empirical, 'Whether bidder learning corrects for the curse').

omega_variable(
    auction_format_dependence,
    'Is the winners curse an inherent feature of common-value auctions or an artifact of specific auction formats (English, sealed-bid, Dutch)? Can alternative formats eliminate the curse?',
    'Comparative auction experiments: run same item under different formats; measure overbidding rates and revenue. Real-world auctions with format variations.',
    'If curse is format-independent: structural constraint (high ε). If format-dependent: curse is design flaw, not coordination feature (low ε, high theater).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(auction_format_dependence, empirical, 'Whether curse is format-specific or structural').

omega_variable(
    information_asymmetry_as_extraction_vs_coordination,
    'Does the information asymmetry that creates the winners curse serve a genuine coordination function (preventing bid collusion, disciplining strategies) or is it purely extractive mechanism benefiting informed players?',
    'Game-theoretic analysis: does eliminating information asymmetry (full transparency about all bidder estimates) improve or degrade allocative efficiency? Experimental comparison of efficiency metrics.',
    'If coordination benefit is real: tangled_rope justified. If pure extraction: constraint downgrades toward snare (suppression↑, beneficiary role of organizer becomes predatory).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(information_asymmetry_as_extraction_vs_coordination, conceptual, 'Whether information asymmetry provides coordination benefit').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(winners_curse, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(wc_tr_t0, winners_curse, theater_ratio, 0, 0.38).
narrative_ontology:measurement(wc_tr_t5, winners_curse, theater_ratio, 5, 0.48).
narrative_ontology:measurement(wc_tr_t10, winners_curse, theater_ratio, 10, 0.58).

% Extraction over time
narrative_ontology:measurement(wc_be_t0, winners_curse, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(wc_be_t5, winners_curse, base_extractiveness, 5, 0.4).
narrative_ontology:measurement(wc_be_t10, winners_curse, base_extractiveness, 10, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(winners_curse, resource_allocation).
narrative_ontology:affects_constraint(winners_curse, information_asymmetry_in_markets).
narrative_ontology:affects_constraint(winners_curse, adverse_selection_mechanism).

% DUAL FORMULATION NOTE:
% The winners curse can be decomposed into two structurally distinct constraints: (1) ESTIMATION_NOISE_SELECTION (ε≈0.15, Mountain) — selection effect in common-value estimation is a natural consequence of information structure; (2) BIDDER_CORRECTION_FAILURE (ε≈0.62, Snare) — bidders fail to adjust for the selection effect due to cognitive and institutional barriers. These constraints are linked: the first is a natural law of Bayesian inference; the second is the extraction mechanism that exploits the first.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(winners_curse, institutional, 0.12).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
