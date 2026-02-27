% ============================================================================
% CONSTRAINT STORY: matching_markets
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_matching_markets, []).

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
 *   constraint_id: matching_markets
 *   human_readable: Matching Market Congestion Externality
 *   domain: economic/market_design
 *
 * SUMMARY:
 *   Matching markets (ride-sharing, online dating, job boards, meal delivery)
 *   solve a real coordination problem: finding compatible pairs among
 *   millions of participants is computationally hard and informationally
 *   complex. The platform provides genuine value by reducing search costs.
 *   However, as participation grows, the matching probability per participant
 *   declines—a negative externality borne unequally by late entrants and
 *   marginal participants. The platform operator benefits from increased
 *   network size and can monetize congestion through surge pricing and
 *   premium matching algorithms. Early adopters captured high-quality matches
 *   before congestion. Late entrants face degraded matching probability with
 *   limited ability to improve their position. This creates a tangled
 *   constraint: the matching mechanism itself is valuable coordination, but
 *   platform design choices (capacity allocation, pricing, algorithmic
 *   opacity) layer extraction on top. The constraint exhibits different faces
 *   from different positions: coordination mechanism (rope) from the
 *   beneficiary's view, mixed coordination and extraction (tangled rope) from
 *   the moderate participant's view, pure extraction (snare) from the trapped
 *   late entrant's view, and a performative regulatory theater (piton) from
 *   the regulator's view.
 *
 * KEY AGENTS:
 *   - Platform Operator: Primary beneficiary (institutional/arbitrage) — benefits from scale, network effects, and ability to monetize congestion through pricing and data capture
 *   - Early Adopters: Secondary beneficiary (institutional/arbitrage) — captured high-quality matches before market congestion; built reputation and social capital during low-congestion period
 *   - Late Entrants: Primary victim (powerless/trapped) — face severe congestion externality with no ability to improve through individual action; sunk search investment creates lock-in
 *   - Marginal Participants: Secondary victim (moderate/constrained) — benefit from coordination mechanism but face meaningful congestion costs; can exit but at significant cost
 *   - Competing Platforms: Organized agents (organized/constrained) — see congestion as both an opportunity (build better algorithm) and a trap (network lock-in prevents switching)
 *   - Regulatory Authority: Institutional actor (institutional/constrained) — attempts to regulate through transparency and fairness mandates but encounters algorithmic opacity and platform power
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing contingent platform design choices as inherent features of matching mechanisms
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(matching_markets, 0.38).
domain_priors:suppression_score(matching_markets, 0.42).
domain_priors:theater_ratio(matching_markets, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(matching_markets, extractiveness, 0.38).
narrative_ontology:constraint_metric(matching_markets, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(matching_markets, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(matching_markets, tangled_rope).
narrative_ontology:human_readable(matching_markets, "Matching Market Congestion Externality").
narrative_ontology:topic_domain(matching_markets, "economic/market_design").

domain_priors:requires_active_enforcement(matching_markets).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(matching_markets, platform_operator).
narrative_ontology:constraint_beneficiary(matching_markets, early_adopters).
narrative_ontology:constraint_victim(matching_markets, late_entrants).
narrative_ontology:constraint_victim(matching_markets, marginal_participants).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: LATE ENTRANT (SNARE) — New participants entering a congested market face degraded matching probability with no ability to improve their position through individual action. Exit is costly (sunk search investment, reputation capital). Market congestion is a trap they cannot escape; they bear the full externality cost. d≈0.92, f(d)≈1.38, σ=1.2 → χ≈0.63.
constraint_indexing:constraint_classification(matching_markets, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: MARGINAL PARTICIPANT (TANGLED ROPE) — Benefits from the coordination service (matching mechanism reduces search costs vs alternatives) but also bears congestion costs that reduce matching quality. Exit is costly but possible (switch to alternative platforms or markets). The constraint both enables and extracts. d≈0.68, f(d)≈1.06, σ=0.9 → χ≈0.39.
constraint_indexing:constraint_classification(matching_markets, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: PLATFORM OPERATOR (ROPE) — Benefits from growth in participant count, which increases network effects and platform value. Experiences the congestion constraint as a coordination problem to solve: matching algorithms, queue management, and surge pricing all serve to allocate scarce matching capacity. d≈0.08, f(d)≈-0.10, σ=1.2 → χ≈-0.04. Net beneficiary through monopoly-like positioning.
constraint_indexing:constraint_classification(matching_markets, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: EARLY ADOPTERS (ROPE) — Benefit from first-mover advantage: access the best matches before congestion degrades quality; build reputation and social capital early. For them, the constraint functions purely as coordination (the matching mechanism itself). They exit into arbitrage opportunities as congestion rises (switching to new platforms or markets). d≈0.12, f(d)≈-0.05, σ=1.2 → χ≈-0.02.
constraint_indexing:constraint_classification(matching_markets, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: COMPETING PLATFORM COALITION (TANGLED ROPE) — Organized competitors (DoorDash vs Uber Eats, eHarmony vs Match, LinkedIn vs alternative hiring platforms) see congestion as both a coordination failure they can exploit (better algorithms, lower congestion) and an extraction mechanism imposed by incumbent's network lock-in. They have agency (can build alternatives) but face switching costs and network effects. d≈0.50, f(d)≈0.65, σ=1.1 → χ≈0.28.
constraint_indexing:constraint_classification(matching_markets, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: REGULATORY AUTHORITY (PITON) — Platforms claim to self-regulate congestion through algorithmic matching and pricing mechanisms, but regulatory scrutiny reveals this is substantially performative: actual congestion metrics are opaque, algorithms are proprietary, and the extraction via degraded match quality is systemic. Theater ratio (0.35) is moderate—some genuine algorithmic improvement occurs, but capacity constraints are real and deliberately maintained to maximize platform value. Regulatory mechanisms (disclosure requirements, fair matching audits) persist without strongly constraining the underlying extraction. d≈0.42, f(d)≈0.42, σ=1.1 → χ≈0.16.
constraint_indexing:constraint_classification(matching_markets, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a game-theoretic perspective, matching market congestion externality is an inherent feature of any market where participants are heterogeneous and matching capacity is finite. This appears as a natural law: no matching mechanism can avoid the tragedy of the commons when uncongested entry is incentivized. However, the structural data (ε=0.38, suppression=0.42, theater=0.35) contradicts mountain classification—this is a contingent institutional fact (platform design choices, capacity allocation, pricing mechanisms) masquerading as a law of nature.
constraint_indexing:constraint_classification(matching_markets, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(matching_markets_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(matching_markets, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(matching_markets, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(matching_markets, TR),
    TR >= 0.70.

:- end_tests(matching_markets_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The base extraction reflects that platform operators deliberately maintain congestion to maximize value and pricing power. Early entrants and the platform benefit substantially, while late entrants bear costs. However, the extraction is not maximal (ε < 0.46) because the coordination function is genuine—the platform does solve a real matching problem that would be much harder to solve through decentralized alternatives. The 0.38 value represents a meaningful but not dominant extraction component overlaid on legitimate coordination. Suppression (0.42): Moderate. Late entrants face real barriers: network effects lock in the incumbent platform, sunk reputation and search investment make switching costly, algorithmic opacity prevents informed assessment of match quality degradation, and capacity constraints are structured into platform design. However, suppression is not total (< 0.60) because competing platforms do exist and can provide alternatives, even if constrained by network effects. Participants can exit, but at significant cost. Theater ratio (0.35): Low-moderate. The platform's algorithmic matching mechanisms are substantively real (they do solve matching problems) but are overlaid with performative elements: claimed efficiency often masks capacity constraints that are deliberately maintained, surge pricing claims to optimize congestion but also extracts rent, and algorithmic fairness claims encounter opacity. The theater has increased slightly over the interval as algorithmic complexity has increased without corresponding transparency.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates significant perspectival divergence driven by temporal position (early vs late adoption) and structural role (operator vs participant). Early adopters see pure coordination (rope) because they entered before congestion degraded quality. The platform operator sees coordination with opportunity to monetize (rope/institutional arbitrage position). Late entrants see pure extraction (snare/powerless trap). Marginal participants see mixed coordination and extraction (tangled rope/moderate constrained). Competing platforms see a mixed constraint: they recognize the coordination function but experience it as an extraction mechanism imposed by network lock-in (tangled rope/organized). The regulatory authority sees performative management (piton) — platforms claim to self-regulate through algorithms and pricing, but actual congestion metrics and matching quality remain opaque. The analytical observer risks seeing an immutable natural law (mountain) — matching markets must have congestion — but the structural data reveals this as contingent on platform design choices.
 *
 * DIRECTIONALITY LOGIC:
 *   Platform operator: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.10. Net beneficiary. Early adopters: Beneficiary + arbitrage → d≈0.12, f(d)≈-0.05. Net beneficiary. Late entrants: Victim + trapped → d≈0.92, f(d)≈1.38. Maximum extraction — cannot exit without severe cost. Marginal participants: Victim + constrained → d≈0.68, f(d)≈1.06. Significant extraction but not maximal. Competing platforms: Victim (of network lock-in) + constrained → d≈0.50, f(d)≈0.65. Moderate-high extraction; they have agency (can build alternatives) but face switching costs. Regulatory authority: Constrained institutional position → d≈0.42, f(d)≈0.42. Low-moderate effective extraction through regulatory mechanisms. Analytical observer: analytical → d≈0.72, f(d)≈1.15. Mountain classification is perspectival (observer naturalizes constraint); the engine's false summit detector will flag this.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION: The constraint resolves the mandatrophy by distinguishing between the genuine coordination function (matching mechanism) and the contingent extraction mechanism (congestion-induced degradation of match quality for late entrants). The platform's rope perspective is NOT false—there is genuine coordination value. The late entrant's snare perspective is NOT false—they genuinely experience extraction without exit. The resolution is that both are accurate readings of a tangled rope structure: coordination (rope gate: matching mechanism solves real problem) PLUS asymmetric extraction (snare gate: beneficiaries capture early-adopter advantage, late entrants bear congestion cost). The tangled rope classification is correct because: (1) there is a genuine coordination function (the matching algorithm reduces search costs compared to alternatives), (2) there is asymmetric extraction (platform and early adopters benefit disproportionately from growth-driven congestion), and (3) the constraint requires active enforcement (platform's capacity allocation, pricing, and algorithmic decisions maintain the extraction). The mandatrophy is NOT resolved by claiming the constraint is 'really' a rope with no extraction, nor by claiming it is 'really' a snare with no coordination. Both components are structurally necessary. The analytical observer's mountain view (congestion is inherent to matching markets) is a FALSE SUMMIT because: (a) competing platforms with different congestion management strategies exist, (b) regulatory interventions (capacity sharing, pricing caps, transparency mandates) demonstrably reduce congestion, and (c) the ε=0.38 is contingent on platform design choices, not fixed by mathematical necessity.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    algorithmic_opacity_vs_structural_inevitability,
    'Is congestion externality a structural inevitability of matching markets or a contingent outcome of opaque algorithmic allocation and deliberately limited capacity?',
    'Comparison of congestion levels across platforms with different algorithmic transparency and capacity policies; A/B testing with full-capacity matching vs congestion-inducing algorithms on same platform',
    'If structural inevitability: mountain classification is correct (but unlikely). If contingent: constraint is tangled_rope at best, and regulatory intervention (platform transparency mandates, capacity sharing requirements) could shift it toward rope or even scaffold.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(algorithmic_opacity_vs_structural_inevitability, empirical, 'Whether congestion is inherent or contingent on platform design choices').

omega_variable(
    competing_platform_viability,
    'Can competing platforms with better congestion management actually overcome network effects and incumbency advantages, or are they permanently trapped by latecomers'' disadvantage?',
    'Historical case studies of platform transitions (e.g., Uber to competitors, Facebook to alternatives); measurement of congestion differential required to trigger switching; analysis of network effects magnitude',
    'If viability high: competing platforms provide real exit option → classify as constrained (not trapped) for many participants. If viability low: late entrants are truly trapped → snare classification is robust.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(competing_platform_viability, empirical, 'Whether competing platforms provide viable exit from incumbent congestion').

omega_variable(
    dynamic_matching_price_efficiency,
    'Do surge pricing and algorithmic matching allocation mechanisms actually achieve efficient congestion management or primarily extract rent from desperate participants?',
    'Empirical comparison of matching quality under dynamic pricing vs fixed pricing; measurement of participant surplus before/after price spikes; analysis of whether price increases correlate with match quality improvements or just revenue extraction',
    'If efficient: platform''s rope perspective (coordination) is accurate. If primarily extractive: constraint is snare for price-sensitive late entrants and tangled rope for others.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(dynamic_matching_price_efficiency, empirical, 'Whether dynamic pricing achieves efficiency or extracts rent').

omega_variable(
    sunk_investment_lock_in,
    'How much of late entrants'' inability to exit is due to sunk reputation/search investment vs actual capacity constraints?',
    'Measurement of average sunk investment per participant cohort; tracking of participant lifetime value and switching costs; comparison of switching rates to new platforms when alternatives emerge',
    'If lock-in is primarily sunk investment: suppression rating should be higher (~0.60+). If primarily capacity constraints: suppression is accurate (~0.42).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sunk_investment_lock_in, empirical, 'Extent of lock-in from sunk reputation investment vs capacity constraints').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(matching_markets, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mmce_tr_t0, matching_markets, theater_ratio, 0, 0.22).
narrative_ontology:measurement(mmce_tr_t5, matching_markets, theater_ratio, 5, 0.28).
narrative_ontology:measurement(mmce_tr_t10, matching_markets, theater_ratio, 10, 0.35).

% Extraction over time
narrative_ontology:measurement(mmce_be_t0, matching_markets, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(mmce_be_t5, matching_markets, base_extractiveness, 5, 0.28).
narrative_ontology:measurement(mmce_be_t10, matching_markets, base_extractiveness, 10, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(matching_markets, information_standard).
narrative_ontology:affects_constraint(matching_markets, ride_sharing_surge_pricing).
narrative_ontology:affects_constraint(matching_markets, job_market_signaling_cascade).
narrative_ontology:affects_constraint(matching_markets, online_dating_user_fatigue).

% DUAL FORMULATION NOTE:
% The matching market congestion externality is upstream of domain-specific constraints (ride-sharing surge pricing, job market signaling cascades). Each domain instantiates the general congestion mechanism with different structural parameters. ε=0.38 for the abstract mechanism may vary across domains (ride-sharing may be ε≈0.42 due to time-sensitive matching; job markets may be ε≈0.35 due to lower switching costs), but the core tangled rope structure persists.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
