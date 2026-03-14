% ============================================================================
% CONSTRAINT STORY: advertising_market_concentration
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_advertising_market_concentration, []).

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
 *   constraint_id: advertising_market_concentration
 *   human_readable: Advertising Market Concentration and Digital Extraction
 *   domain: economic/technology/digital_platforms
 *
 * SUMMARY:
 *   The advertising market has undergone radical concentration in the past
 *   two decades, shifting from a distributed ecosystem of publishers,
 *   agencies, and advertisers to a duopoly of Google and Meta controlling
 *   approximately 60% of global digital advertising revenue. This constraint
 *   exhibits the classical characteristics of a snare: trapped publishers
 *   dependent on platform distribution, users subjected to algorithmic
 *   manipulation and behavioral harvesting, and suppression maintained
 *   through network effects that make individual exit irrational. The
 *   extractiveness has increased from 0.35 (2005-2010, when advertising
 *   remained somewhat competitive) to 0.68 (2025), tracking the consolidation
 *   of platform power. The theater_ratio (0.58) reflects that much of the
 *   advertising ecosystem consists of performative activity — audience
 *   metrics, brand safety initiatives, fraud prevention — that does not
 *   translate to user value or publisher revenue. The constraint's
 *   suppression (0.72) is sustained by the structural lock-in: users cannot
 *   exit because their social graph is platform-specific; publishers cannot
 *   exit because audience discovery depends on algorithmic distribution;
 *   advertisers cannot exit because reaching users at scale requires platform
 *   access.
 *
 * KEY AGENTS:
 *   - Dominant Ad Platforms (Google, Meta, Amazon, TikTok): Primary beneficiaries (institutional/arbitrage) — capture 60%+ of digital ad revenue; control algorithmic allocation of advertiser budgets and user attention
 *   - Independent Publishers (news sites, blogs, content creators): Primary victims (powerless/trapped) — dependent on platform distribution; face algorithmic rank changes that can cut revenue by 50%; cannot negotiate ad rates
 *   - Ad-Supported Users: Primary victims (powerless/trapped) — subjected to attention harvesting and behavioral manipulation; cannot opt out of tracking without losing platform access
 *   - Regional Media Organizations: Secondary victims (moderate/constrained) — have some negotiating power through scale but constrained by lack of viable alternatives
 *   - Publisher Coalitions (news associations, media consortia): Organized response (organized/constrained) — extracting joint benefits through platform negotiations while remaining dependent
 *   - Legacy Ad Industry (agencies, media buyers): Institutional actors (institutional/arbitrage) — maintain degraded function through inertia
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — sees extraction mechanism maintained by network effects and algorithmic control
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(advertising_market_concentration, 0.68).
domain_priors:suppression_score(advertising_market_concentration, 0.72).
domain_priors:theater_ratio(advertising_market_concentration, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(advertising_market_concentration, extractiveness, 0.68).
narrative_ontology:constraint_metric(advertising_market_concentration, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(advertising_market_concentration, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(advertising_market_concentration, snare).
narrative_ontology:human_readable(advertising_market_concentration, "Advertising Market Concentration and Digital Extraction").
narrative_ontology:topic_domain(advertising_market_concentration, "economic/technology/digital_platforms").

domain_priors:requires_active_enforcement(advertising_market_concentration).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(advertising_market_concentration, dominant_ad_platforms).
narrative_ontology:constraint_beneficiary(advertising_market_concentration, large_advertisers).
narrative_ontology:constraint_victim(advertising_market_concentration, small_publishers).
narrative_ontology:constraint_victim(advertising_market_concentration, independent_media).
narrative_ontology:constraint_victim(advertising_market_concentration, users).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: INDEPENDENT PUBLISHER (SNARE) — Trapped in the advertising market. Cannot build audience reach without platform distribution; cannot monetize without ad networks; faces algorithmic demotions and revenue clawbacks from dominant platforms. Exit costs are insurmountable: switching ad networks means losing established networks, facing discovery penalties, and reduced CPM rates. No meaningful alternatives for small-scale content monetization at scale.
constraint_indexing:constraint_classification(advertising_market_concentration, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: AD-SUPPORTED USER (SNARE) — Trapped in the attention extraction system. Faces algorithmic manipulation of feed content optimized for engagement rather than utility; cannot exit without abandoning digital communication and commerce. Data harvesting through ad networks is non-consensual at scale — users cannot meaningfully opt out without losing service access. Suppression through network effects: all users' peers are on the dominant platforms, making exit individually irrational.
constraint_indexing:constraint_classification(advertising_market_concentration, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 3: REGIONAL MEDIA ORGANIZATION (SNARE) — Constrained but not fully trapped. Can exit by shifting to subscription models or local advertising, but this incurs significant revenue loss and customer acquisition costs. Experiences platform dependency: algorithmic changes can instantly reduce referral traffic by 30-50%, forcing reactive changes. High suppression from market concentration means limited negotiating power with platforms or alternative ad networks.
constraint_indexing:constraint_classification(advertising_market_concentration, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: PUBLISHER COALITION (TANGLED ROPE) — Organized publishers (news associations, media consortia) have begun extracting joint value through coordinated negotiation with platforms (e.g., Google News Initiative, Facebook Journalism Project). They see genuine coordination benefits: platform support for fact-checking, revenue-sharing agreements, distribution guarantees. But the coordination comes embedded in extraction: platforms retain algorithmic control, can unilaterally change terms, and benefit disproportionately from user data. The coalition has agency but constrained options.
constraint_indexing:constraint_classification(advertising_market_concentration, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: DOMINANT AD PLATFORM (ROPE) — Experiences the constraint as pure coordination: connecting advertisers to audiences at scale, matching ad inventory to user interests. The platform benefits from network effects and data aggregation but sees its function as genuine coordination rather than extraction. Can arbitrage by shifting between advertising, subscriptions, or other models. From this perspective, the constraint is a scalable coordination mechanism that solves a real problem: connecting willing participants in an attention market.
constraint_indexing:constraint_classification(advertising_market_concentration, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: LEGACY ADVERTISING INDUSTRY (PITON) — Traditional advertising intermediaries (ad agencies, media buying networks) continue to operate but have degraded function: platforms have automated much of the matching and targeting, reducing the intermediary role to execution and compliance. Theater ratio is high — the industry maintains institutional rituals (pitch meetings, creative reviews, planning cycles) while the actual strategic decisions flow through algorithmic optimization. The constraint persists through institutional inertia as platforms have captured the value-creation layer.
constraint_indexing:constraint_classification(advertising_market_concentration, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (SNARE) — From a civilizational perspective, the advertising market concentration represents a pure extraction mechanism disguised as coordination. The platforms have systematized the capture of user attention and behavioral data; publishers have been reduced to content suppliers with no pricing power; small advertisers face opaque algorithmic allocation. The constraint's suppression (0.72) is maintained by network effects that make switching irrational for any individual actor. The extraction is not incidental to coordination — it IS the coordination.
constraint_indexing:constraint_classification(advertising_market_concentration, snare,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(advertising_market_concentration_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(advertising_market_concentration, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(advertising_market_concentration, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(advertising_market_concentration, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(advertising_market_concentration, TR),
    TR >= 0.70.

:- end_tests(advertising_market_concentration_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High and increasing. The platforms extract through multiple mechanisms: (1) Revenue asymmetry — platforms keep 30-50% of advertiser spend, publishers receive declining percentage; (2) Data asymmetry — platforms harvest user behavioral data to improve targeting, which increases advertiser returns but harms user privacy; (3) Algorithmic suppression — platforms can demote publishers' content without transparency, forcing dependence on platform support; (4) Information asymmetry — publishers and advertisers lack transparency into algorithmic decisions, pricing mechanisms, and ad delivery. The upward trajectory reflects platform consolidation: as Google and Meta's market share has grown from 40% (2010) to 60%+ (2025), their ability to extract has increased through stronger network effects and reduced competitive pressure. Suppression (0.72): High and structural. Users cannot exit without abandoning social networks and digital commerce; publishers cannot exit without losing 40-70% of revenue; advertisers cannot exit without losing access to user bases. The suppression is maintained by: (1) Network effects — platform utility increases with user count; (2) Algorithmic ranking control — platforms determine publisher visibility; (3) API restrictions — competing ad networks cannot match platform scale; (4) Data advantages — platforms' proprietary user data enables targeting competitors cannot match. Theater ratio (0.58): Moderate-high. The advertising industry maintains performative layers: (1) Brand safety rituals — fraud detection, viewability metrics, brand association controls that address real problems but are largely procedural; (2) Auction mechanics — real-time bidding and algorithmic allocation that obscure the actual decision-making (platform's proprietary algorithms); (3) Reporting and analytics — platforms provide detailed metrics that create illusion of control while reserving actual algorithmic decisions. Theater has increased as platforms have automated decision-making and made algorithms opaque.
 *
 * PERSPECTIVAL GAP:
 *   The gap between perspectives reveals the extraction mechanism itself. Why do independent publishers and users see Snare while platforms see Rope? Because they occupy opposite positions in the extraction flow. The platform's 'coordination' function (matching advertisers to audiences) is real and valuable. But the way this coordination is achieved — through proprietary algorithms, user data harvesting, and publisher algorithmic suppression — concentrates all benefits to the platform and all costs to publishers and users. The organized publishers' Tangled Rope perspective is instructive: they have gained some benefits (revenue-sharing deals, platform support) while remaining fundamentally constrained. This suggests the constraint might be redeemable through regulation or market opening (moving it toward pure Rope). But the analytical observer's Snare classification is more structurally defensible: the network effects and algorithmic control mechanisms are not incidental features but core to how the extraction works. Without these mechanisms, the platforms would have less ability to extract.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is derived from beneficiary/victim declarations plus exit options. Dominant platforms are beneficiaries with arbitrage exit → d ≈ 0.05 → f(d) ≈ -0.12 → χ amplifies platform benefits and masks extraction. Independent publishers are victims with trapped exit → d ≈ 0.95 → f(d) ≈ 1.42 → χ amplifies extraction they experience. Users are victims with trapped exit (cannot opt out without losing service access) → d ≈ 0.95 → f(d) ≈ 1.42. The scope modifier (global, σ = 1.2) amplifies extraction across large scope because network effects compound at scale. Smaller markets with viable local alternatives would see lower σ and lower χ; global dominance increases both. The directionality overrides are not needed — the structural derivation accurately captures the extraction flow.
 *
 * MANDATROPHY ANALYSIS:
 *   EXTRACTIVENESS > 0.70 REQUIRES MANDATROPHY RESOLUTION: The advertising market concentration constraint has extractiveness = 0.68 at current state, approaching 0.75 at the 15-year projection point. At ε > 0.70, the mandatrophy gate requires explicit resolution. The question is: can this constraint be redeemed as Tangled Rope (coordination with embedded extraction) or is it irredeemable Snare? The mandatrophy is resolved by observing that: (1) Genuine coordination function exists (platforms do solve real advertiser-audience matching problem); (2) Asymmetric extraction is documented (publishers, users, and smaller advertisers bear costs; platforms capture surplus); (3) Active enforcement is required (algorithmic ranking, API restrictions, data controls maintain the asymmetry). These are the Tangled Rope gates. However, the suppression (0.72) is higher than typical Tangled Ropes (which cluster around 0.40-0.60). The high suppression indicates that the coordination function has been captured: the original problem (matching advertisers to audiences) is now secondary to the extraction mechanism (maintaining platform control and data monopoly). At the civilizational/analytical perspective, the constraint is Snare — the coordination narrative is theater. The mandatrophy resolution is that this is a Tangled Rope undergoing degradation toward Snare. The measurements show extractiveness rising from 0.35 to 0.68 over 15 years; theater rising from 0.42 to 0.58. These trends indicate that the coordination function is being hollowed out: theater is not rising because coordination is becoming more complex, but because the actual decision-making (platform algorithms) is becoming more opaque. A Tangled Rope that is degrading toward Snare remains a Tangled Rope in structure but approaching Snare in function.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    algorithmic_personalization_necessity,
    'Is the degree of algorithmic personalization and behavioral tracking required for genuine coordination of advertiser-audience matching, or is much of it extractive overhead masquerading as technical necessity?',
    'Comparative analysis of ad effectiveness across targeting granularity levels; measurement of how much tracking data is actually necessary for matching vs. how much is collected for behavioral prediction; analysis of user utility from targeted vs. untargeted content',
    'If targeting granularity > 50% is necessary: extractiveness drops to 0.45-0.50 (Tangled Rope). If < 30% of tracking is necessary for ad matching: extractiveness rises to 0.75+ (pure Snare).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(algorithmic_personalization_necessity, empirical, 'Necessity of behavioral tracking for advertising coordination').

omega_variable(
    alternative_monetization_viability,
    'Do realistic alternative monetization models (subscription, micropayments, patronage, public funding) exist that could sustainably replace advertising-dependent media, or is the advertising model structurally inevitable?',
    'Historical analysis of media survival rates under alternative models; economic modeling of news/content production costs vs. subscription feasibility; case studies of successful non-advertising-dependent media platforms',
    'If alternatives are viable: the trap classification is too strong (exit is constrained, not trapped). If alternatives structurally fail: trap classification is confirmed.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(alternative_monetization_viability, empirical, 'Viability of alternative media monetization models').

omega_variable(
    network_effect_inevitability,
    'Are network effects in digital advertising fundamentally technological/mathematical, or contingent on specific platform design choices (e.g., algorithmic ranking, data sharing restrictions, API terms)?',
    'Analysis of network effect magnitude under alternative design regimes (open APIs, interoperable data standards, algorithmic transparency); historical comparison across platforms with different architectural choices',
    'If network effects are contingent on design: suppression could drop significantly (0.40-0.50) if regulation enforces interoperability. If effects are fundamental: current suppression (0.72) is structurally necessary.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(network_effect_inevitability, empirical, 'Whether network effects are technological or design-contingent').

omega_variable(
    platform_coordination_vs_extraction_boundary,
    'At what point does coordinating attention matching between advertisers and users become extraction? Is the boundary at data transparency, algorithmic control, or pricing asymmetry?',
    'Normative analysis of stakeholder interests; comparison with other coordination mechanisms (markets, auctions, standards); measurement of how surplus is distributed across stakeholders',
    'Different boundaries move the classification between Tangled Rope (genuine coordination with embedded extraction) and Snare (extraction disguised as coordination). This omega determines whether the constraint is redeemable through regulation (Tangled Rope) or inherently exploitative (Snare).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(platform_coordination_vs_extraction_boundary, conceptual, 'Definitional boundary between coordination and extraction in attention markets').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(advertising_market_concentration, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(adm_tr_t0, advertising_market_concentration, theater_ratio, 0, 0.42).
narrative_ontology:measurement(adm_tr_t5, advertising_market_concentration, theater_ratio, 5, 0.5).
narrative_ontology:measurement(adm_tr_t10, advertising_market_concentration, theater_ratio, 10, 0.58).
narrative_ontology:measurement(adm_theater_t15_projection, advertising_market_concentration, theater_ratio, 15, 0.65).

% Extraction over time
narrative_ontology:measurement(adm_be_t0, advertising_market_concentration, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(adm_be_t5, advertising_market_concentration, base_extractiveness, 5, 0.52).
narrative_ontology:measurement(adm_be_t10, advertising_market_concentration, base_extractiveness, 10, 0.68).
narrative_ontology:measurement(adm_extractiveness_t15_projection, advertising_market_concentration, base_extractiveness, 15, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(advertising_market_concentration, resource_allocation).
narrative_ontology:boltzmann_floor_override(advertising_market_concentration, 0.18).
narrative_ontology:affects_constraint(advertising_market_concentration, news_ecosystem_collapse).
narrative_ontology:affects_constraint(advertising_market_concentration, user_data_harvesting).
narrative_ontology:affects_constraint(advertising_market_concentration, algorithmic_ranking_opacity).

% DUAL FORMULATION NOTE:
% Advertising market concentration decomposes into structurally distinct constraints: (1) resource_allocation (how ad budgets are distributed across publishers and platforms); (2) attention_extraction (behavioral manipulation and data harvesting from users); (3) publisher_dependency (algorithmic control maintaining lock-in). This story focuses on the market concentration mechanism; the linked stories handle the downstream effects on news sustainability and user privacy. The high boltzmann_floor_override (0.18) reflects that genuine resource allocation complexity justifies some baseline extraction; but extraction above this floor indicates rent-seeking beyond coordination costs.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(advertising_market_concentration, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
