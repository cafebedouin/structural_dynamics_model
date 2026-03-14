% ============================================================================
% CONSTRAINT STORY: long_tail_economics
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_long_tail_economics, []).

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
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: long_tail_economics
 *   human_readable: Long Tail Economics: Aggregation and Distribution Asymmetry
 *   domain: economics/digital_markets
 *
 * SUMMARY:
 *   The long tail economics constraint emerges from the intersection of
 *   digital distribution technology, algorithmic ranking systems, and market
 *   concentration dynamics. Chris Anderson's 'long tail' thesis (2004)
 *   claimed that digital distribution enables niche creators to reach global
 *   audiences, replacing the scarcity-driven gatekeeping of traditional media
 *   with abundance-driven coordination. Two decades of empirical data reveal
 *   a more complex picture: while distribution barriers have collapsed,
 *   algorithmic sorting has created new extraction mechanisms. The constraint
 *   exhibits different character across different creator power levels — it
 *   genuinely enables niche creators while simultaneously suppressing
 *   mid-tier creators and extracting from emerging talent. Platform
 *   aggregators (YouTube, Spotify, TikTok) benefit from the coordination
 *   function (connecting supply and demand at scale) while extracting through
 *   opaque algorithmic ranking, revenue share asymmetry, and attention
 *   concentration. The constraint has evolved from pure coordination (early
 *   platform period, 2005-2012) toward tangled rope (mixed
 *   coordination-extraction, 2012-present) as platforms have shifted from
 *   curation costs to algorithmic opacity and network lock-in.
 *
 * KEY AGENTS:
 *   - Platform Aggregators: Primary beneficiary (institutional/arbitrage) — captures 30-55% revenue share; benefits from network effects and data extraction; maintains low transaction costs for coordination
 *   - Mid-Tier Creators: Primary victim (powerless/trapped) — eroded revenue viability; algorithmic invisibility; no exit path without audience loss; full extraction experienced
 *   - Emerging/Niche Creators: Secondary beneficiary (moderate/mobile) — unprecedented access to tools and global distribution; low barrier to entry; benefits from coordination function; some extraction through ranking opacity
 *   - Mega-Star Creators: Secondary beneficiary (powerful/arbitrage) — algorithmic amplification; premium tools and support; concentrated attention; can negotiate favorable terms or exit
 *   - Creator Collectives and Unions: Organized actors (organized/constrained) — building alternative platforms and collective bargaining; see extraction as solvable through counter-institutional mechanisms
 *   - Traditional Media Gatekeepers: Former institutional actors (institutional/arbitrage) — functional role degraded; maintain theater through brand and legacy authority; declining relevance
 *   - Audiences/Consumers: Indirect victims (powerless/constrained) — lock-in through algorithmic feeds; filtering and recommender system opacity; reduced discovery autonomy
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(long_tail_economics, 0.52).
domain_priors:suppression_score(long_tail_economics, 0.48).
domain_priors:theater_ratio(long_tail_economics, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(long_tail_economics, extractiveness, 0.52).
narrative_ontology:constraint_metric(long_tail_economics, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(long_tail_economics, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(long_tail_economics, tangled_rope).
narrative_ontology:human_readable(long_tail_economics, "Long Tail Economics: Aggregation and Distribution Asymmetry").
narrative_ontology:topic_domain(long_tail_economics, "economics/digital_markets").

domain_priors:requires_active_enforcement(long_tail_economics).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(long_tail_economics, platform_aggregators).
narrative_ontology:constraint_beneficiary(long_tail_economics, niche_content_creators).
narrative_ontology:constraint_victim(long_tail_economics, mid_tier_creators).
narrative_ontology:constraint_victim(long_tail_economics, traditional_gatekeepers).
narrative_ontology:constraint_victim(long_tail_economics, creator_autonomy).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: MID-TIER CREATOR (SNARE) — Trapped by algorithmic sorting that makes discoverability costly and unpredictable. Cannot exit distribution platforms without abandoning audience. Suppressed by ranking mechanisms that concentrate attention at extremes (megastars and niche micro-audiences) while eroding the viable middle. Maximum extraction — algorithmic invisibility is the coercive mechanism.
constraint_indexing:constraint_classification(long_tail_economics, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: EMERGING CREATOR (TANGLED ROPE) — Benefits from low barrier to entry (genuine coordination value: anyone can publish) but constrained by algorithmic sorting (high extraction cost for visibility). Mixed: the platform coordinates supply and demand, enabling unprecedented access to tools and distribution, while simultaneously extracting through opacity of ranking mechanisms and attention asymmetry.
constraint_indexing:constraint_classification(long_tail_economics, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 3: NICHE AUDIENCE / SUPER-MARGINAL CREATOR (ROPE) — Pure coordination benefit. The long tail model enables previously impossible matches between niche creators and dispersed global audiences. Transaction costs of discovery and distribution are genuinely solved. These actors experience the constraint as enabling, not extractive, because the platform's coordination function matches their structural needs. No arbitrage required — they simply benefit from the coordination.
constraint_indexing:constraint_classification(long_tail_economics, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: CREATOR COLLECTIVE MOVEMENT (SCAFFOLD) — Organized agents (unions, collectives, alternative platforms) see the extraction as temporary and solvable through counter-institutional mechanisms (creator cooperatives, decentralized protocols, collective bargaining). Low effective extraction because organized actors see and are building exit paths. Sunset clause: emerging platforms (YouTube alternatives, Patreon, blockchain-based distribution) are building parallel distribution with different incentive structures.
constraint_indexing:constraint_classification(long_tail_economics, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: TRADITIONAL MEDIA GATEKEEPER (PITON) — Once functioned as essential coordinator (scarcity of distribution channels required professional curation and gatekeeping). Now largely performative: gatekeeping persists through brand authority and legacy institutional advantage, but the functional necessity is gone. Publishers, studios, and broadcasters maintain the gatekeeper role through inertia while the market has moved to algorithmic distribution. Theater ratio reflects that traditional gatekeeping for media and culture is maintained ceremonially, not functionally.
constraint_indexing:constraint_classification(long_tail_economics, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, attention is fundamentally scarce and power-law-distributed. The long tail is a mathematical property of attention markets: human cognition cannot process infinite choices, so selection mechanisms (whether curator or algorithm) will always concentrate attention toward extremes. The constraint appears immutable — an inherent property of how attention economics must function. However, structural data contradicts this: the distribution of attention among creators is socially constructed through platform design choices, incentive structures, and algorithmic ranking. The 'natural law' framing naturalizes contingent extraction.
constraint_indexing:constraint_classification(long_tail_economics, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(long_tail_economics_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(long_tail_economics, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(long_tail_economics, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(long_tail_economics, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(long_tail_economics, TR),
    TR >= 0.70.

:- end_tests(long_tail_economics_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The constraint extracts through three mechanisms: (1) revenue asymmetry (platforms take 30-55% of creator revenue), (2) algorithmic opacity that concentrates attention toward extremes and away from mid-tier creators, and (3) platform lock-in that forces creators into dependency relationships. The value has increased from 0.28 (early period when coordination benefit was highest relative to extraction) to 0.52 (current period when algorithmic extraction mechanisms are mature). The constraint is not a snare because it genuinely coordinates supply and demand, enabling distribution that would be impossible otherwise. It is tangled rope because both coordination and extraction coexist, not because one masks the other. Suppression (0.48): Moderate. Mid-tier creators face significant barriers to visibility but can theoretically build audiences through content quality or community. Niche creators face lower suppression (their audiences are findable). Emerging creators face high initial suppression (ranking mechanisms favor established creators). The average suppression reflects these divergent experiences. Theater ratio (0.58): Moderate-high. Platforms present algorithmic ranking as a neutral mechanism (theater) while it functions as an extraction tool. Creator support programs and 'partner' status maintain the theater that platforms are acting in creators' interest rather than extracting from them. Curation theater persists despite being largely algorithmic.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap reveals that the long tail constraint is simultaneously enabling and extractive depending on creator position. This is the key diagnostic feature of tangled rope: both coordination and extraction are structural and real. The platform genuinely solves a coordination problem (matching supply and demand at global scale with low transaction costs). The platform genuinely extracts (through revenue share, algorithmic opacity, and attention concentration). These are not competing interpretations — they are different structural realities for different agents. The danger is conflating 'the long tail enables niche creators' with 'the long tail is not extractive' — both are true but neither cancels the other.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality differs sharply across creator types and reflects their structural relationship to the extraction flow. Mega-stars benefit from algorithmic amplification (d ≈ 0.10, low extraction). Niche creators benefit from coordination (d ≈ 0.20, very low extraction). Mid-tier creators face full suppression without benefiting from coordination (d ≈ 0.90, very high extraction). Emerging creators face initial suppression but can potentially escape it (d ≈ 0.70, high extraction). Audiences face lock-in but benefit from vast content availability (d ≈ 0.65, moderate extraction). Platforms benefit from network effects, data, and revenue share (d ≈ -0.15, negative extraction / subsidy). The power atom modulates how these structural positions translate to experienced extraction: powerless mid-tier creators experience high chi despite moderate epsilon because f(d) amplifies their extraction. Institutional platforms experience negative chi (institutionalized subsidy effect) that masks the extraction they impose on others.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by showing that creator position (power level) determines which classification predominates. For mega-stars and niche creators, rope classification is accurate — coordination benefit exceeds extraction. For mid-tier creators, snare classification is accurate — extraction dominates with no coordination benefit. For emerging and organized creators, tangled rope is accurate — both forces operate. No single type is 'correct' because the constraint genuinely embodies both coordination and extraction. The classification divergence across perspectives is not perspectival disagreement — it is structural reality. The mandate-driven error would be claiming 'the long tail is pure coordination' (rope) or 'the long tail is pure extraction' (snare). The correct analysis admits both forces simultaneously.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    algorithmic_opacity_measurement,
    'Is the suppression of mid-tier creators driven by mathematical properties of attention scarcity or by opaque algorithmic choices that concentrate visibility?',
    'Comparative analysis of attention distribution across platforms with different ranking transparency (fully open vs proprietary); historical analysis of how ranking algorithm changes affect creator visibility trajectories',
    'If mathematical inevitability: suppression is a structural property (mountain-like). If algorithmic choice: suppression is enforced (extraction mechanism), reclassifying constraint toward snare. Platform design decisions determine classification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(algorithmic_opacity_measurement, empirical, 'Whether suppression is inherent to attention economics or contingent on algorithmic design').

omega_variable(
    extraction_magnitude_threshold,
    'What revenue share threshold for platform aggregators distinguishes fair coordination overhead from asymmetric extraction?',
    'Comparison of platform revenue share (30% take rate for app stores, 45% for music streaming, 55% for YouTube) against transaction costs and value-added services provided; survey data on creator perception of fairness by revenue share level',
    'If platform takes 50%+: primarily extractive (snare classification). If platform takes 20-30%: primarily coordinative (rope classification). Current range (30-55%) suggests tangled rope, but threshold varies by creator power level.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraction_magnitude_threshold, empirical, 'Revenue share threshold for distinguishing coordination from extraction').

omega_variable(
    alternative_distribution_viability,
    'Are emerging alternative platforms (creator cooperatives, blockchain distribution, direct-to-audience models) genuinely capable of replacing centralized aggregators, or are they niche solutions that cannot achieve coordination at scale?',
    'Historical tracking of alternative platform adoption; measurement of transaction costs and network effects required for viable alternatives; comparative analysis of transaction costs across models (platform 55% take, direct-to-audience 10% processor fee, blockchain platform 2% gas costs)',
    'If viable alternatives emerge: scaffold perspective confirmed, sunset is real. If alternatives remain niche: organizes agents are constrained to use extractive platforms (reclassifies scaffold toward tangled rope or snare).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_distribution_viability, empirical, 'Whether alternative distribution models can achieve coordination at scale').

omega_variable(
    attention_inequality_directionality,
    'Does the long tail constraint primarily extract from creators (who lose revenue share and visibility to algorithmic prioritization) or from audiences (who lose choice through algorithmic filtering and recommender system lock-in)?',
    'Directional analysis: measurement of creator revenue losses vs audience attention losses; analysis of whether aggregators extract more from creator side (revenue share) or audience side (data/attention). Audience lock-in measured through switching costs and alternative service evaluation.',
    'If extraction primarily from creators: directionality favors creative labor (d=0.85 for creator victims). If from audiences: directionality shifts toward consumer lock-in (d=0.70 for audience victims). This affects effective extraction chi for different agent classes.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(attention_inequality_directionality, empirical, 'Whether long tail extracts primarily from creators or audiences').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(long_tail_economics, 0, 16).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(longtail_tr_t0, long_tail_economics, theater_ratio, 0, 0.35).
narrative_ontology:measurement(longtail_tr_t8, long_tail_economics, theater_ratio, 8, 0.48).
narrative_ontology:measurement(longtail_tr_t16, long_tail_economics, theater_ratio, 16, 0.58).

% Extraction over time
narrative_ontology:measurement(longtail_be_t0, long_tail_economics, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(longtail_be_t8, long_tail_economics, base_extractiveness, 8, 0.42).
narrative_ontology:measurement(longtail_be_t16, long_tail_economics, base_extractiveness, 16, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(long_tail_economics, resource_allocation).
narrative_ontology:boltzmann_floor_override(long_tail_economics, 0.18).
narrative_ontology:affects_constraint(long_tail_economics, attention_economy_concentration).
narrative_ontology:affects_constraint(long_tail_economics, creator_labor_extraction).
narrative_ontology:affects_constraint(long_tail_economics, platform_network_lock_in).

% DUAL FORMULATION NOTE:
% The long tail constraint decomposes into three structurally distinct stories: (1) Long-tail-enabling coordination: platform distribution technologies that collapse transaction costs (ε≈0.05, rope); (2) Algorithmic ranking extraction: opacity and sorting that concentrate attention (ε≈0.65, snare); (3) Revenue-share asymmetry: platform commission structures that extract from creators (ε≈0.55, tangled rope). These stories share the same platform infrastructure but have different ε values reflecting different measurement observables. The current story aggregates all three; network edges indicate where decomposition is warranted.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(long_tail_economics, institutional, 0.05).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
