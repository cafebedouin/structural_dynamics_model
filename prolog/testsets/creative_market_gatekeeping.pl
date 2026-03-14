% ============================================================================
% CONSTRAINT STORY: creative_market_gatekeeping
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_creative_market_gatekeeping, []).

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
 *   constraint_id: creative_market_gatekeeping
 *   human_readable: Creative Market Gatekeeping (Publishing, Music, Film)
 *   domain: cultural_economics/market_access
 *
 * SUMMARY:
 *   Creative market gatekeeping — the control exercised by publishers, record
 *   labels, film studios, and distribution networks over access to creative
 *   professionals and audiences — represents a complex constraint spanning
 *   pure coordination, hybrid coordination-extraction, and pure extraction
 *   depending on the observer's structural position. The constraint exhibits
 *   all six DR types, revealing how the same institutional arrangement
 *   functions simultaneously as genuine market coordination (rope from the
 *   gatekeeper's perspective), temporary problem-solving infrastructure
 *   (scaffold from organizers building alternatives), degraded institutional
 *   theater (piton from the long-term view), mixed cooperation and extraction
 *   (tangled rope from independent artists with partial alternatives), pure
 *   extraction (snare from emerging creators with no alternatives), and
 *   naturalized law of creative markets (false summit mountain from the
 *   analytical observer). Gatekeeping has intensified over the 10-year
 *   interval as consolidation (mergers reducing major publishers from ~six to
 *   ~four, consolidation in music labels and studios) and algorithmic
 *   intermediation (streaming platform monopoly on discovery) have increased
 *   barriers to independent creator success. The theater ratio rising from
 *   0.45 to 0.65 reflects the performative nature of institutional
 *   gatekeeping increasing — traditional validation (book blurbs, radio play,
 *   studio backing) becoming more about cultural prestige signaling than
 *   about actual market necessity, as self-published bestsellers,
 *   TikTok-driven music hits, and direct-to-streaming films demonstrate that
 *   bypass is possible.
 *
 * KEY AGENTS:
 *   - Emerging Creators: Primary victims (powerless/trapped) — lack capital, relationships, and information to bypass gatekeepers; depend on institutional approval for career viability
 *   - Independent Artists with Niche Following: Secondary victims (moderate/constrained) — have partial alternatives through direct-to-fan channels but face coordination costs and discoverability limits without institutional backing
 *   - Incumbent Gatekeepers (Publishers, Labels, Studios): Primary beneficiaries (institutional/arbitrage) — capture economic rents through distribution monopoly, contract terms, and cultural legitimacy
 *   - Creator Coalitions and Guilds: Organized victims (organized/constrained) — have growing capacity to negotiate but remain constrained by gatekeeper market power
 *   - Alternative Platforms (Patreon, Bandcamp, YouTube, blockchain): Emerging beneficiaries (organized/constrained) — building sunset infrastructure but not yet fully matured; also exhibit gatekeeping dynamics themselves (algorithmic curation, platform monopoly)
 *   - Open-Source and Cooperative Networks: Infrastructure builders (organized/constrained) — decentralized publishing, music protocols, film collectives providing sunset pathways
 *   - Institutional Legitimacy Apparatus: Symbolic gatekeeper (institutional/arbitrage) — cultural prestige system maintaining gatekeeping through ritual and prestige association rather than market necessity
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(creative_market_gatekeeping, 0.58).
domain_priors:suppression_score(creative_market_gatekeeping, 0.62).
domain_priors:theater_ratio(creative_market_gatekeeping, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(creative_market_gatekeeping, extractiveness, 0.58).
narrative_ontology:constraint_metric(creative_market_gatekeeping, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(creative_market_gatekeeping, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(creative_market_gatekeeping, tangled_rope).
narrative_ontology:human_readable(creative_market_gatekeeping, "Creative Market Gatekeeping (Publishing, Music, Film)").
narrative_ontology:topic_domain(creative_market_gatekeeping, "cultural_economics/market_access").

domain_priors:requires_active_enforcement(creative_market_gatekeeping).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(creative_market_gatekeeping, incumbent_publishers).
narrative_ontology:constraint_beneficiary(creative_market_gatekeeping, major_record_labels).
narrative_ontology:constraint_beneficiary(creative_market_gatekeeping, studio_conglomerates).
narrative_ontology:constraint_victim(creative_market_gatekeeping, emerging_creators).
narrative_ontology:constraint_victim(creative_market_gatekeeping, independent_artists).
narrative_ontology:constraint_victim(creative_market_gatekeeping, underrepresented_communities).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: EMERGING CREATOR (SNARE) — Trapped by capital requirements (recording studio access, distribution infrastructure, advance funding), information asymmetry (contract terms), and career path dependency. Cannot reach audiences without gatekeepers' permission. Experiences pure extraction: must accept unfavorable terms or abandon creative ambitions entirely. No alternative distribution channels available at scale.
constraint_indexing:constraint_classification(creative_market_gatekeeping, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: INDEPENDENT ARTIST WITH NICHE FOLLOWING (TANGLED ROPE) — Can partially escape gatekeepers through direct-to-fan channels (Patreon, Kickstarter, Bandcamp, self-publishing platforms), but faces high coordination costs and limited discoverability without institutional backing. Benefits from gatekeepers' marketing infrastructure and distribution networks (coordination function) while bearing extraction through unequal profit splits and contract restrictions (asymmetric extraction).
constraint_indexing:constraint_classification(creative_market_gatekeeping, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: INCUMBENT GATEKEEPER (ROPE) — Experiences the constraint as pure coordination: institutional infrastructure (distribution networks, marketing, discovery algorithms, retail relationships) solves the collective action problem of connecting creators to audiences. Net beneficiary — extraction flows toward them. Can exit or arbitrage between markets without constraint.
constraint_indexing:constraint_classification(creative_market_gatekeeping, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: CREATOR COALITION & DIGITAL PLATFORMS (TANGLED ROPE) — Organized agents (author guilds, musician collectives, indie film networks, emerging platforms like Spotify Direct, YouTube, TikTok) see both genuine coordination (audience discovery, payment systems, copyright enforcement) and extraction (platform monopoly margins, algorithm opacity, promotional advantages for major-label content). Growing agency and alternative pathways reduce effective suppression.
constraint_indexing:constraint_classification(creative_market_gatekeeping, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: OPEN INFRASTRUCTURE MOVEMENT (SCAFFOLD) — Organized agents (open-source publishing tools, blockchain-based royalty tracking, decentralized music protocols, artist cooperatives) are building sunset mechanisms into the gatekeeper constraint. As these alternatives mature, direct-to-fan distribution and cooperative models bypass traditional gatekeepers. Low effective extraction because the movement has agency and sees a structural exit path. Estimated sunset: 10-15 years for alternatives to reach mainstream adoption in select genres.
constraint_indexing:constraint_classification(creative_market_gatekeeping, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: INSTITUTIONAL LEGITIMACY RITUAL (PITON) — Traditional gatekeeper validation (book blurbs from major publishers, placement on radio/TV, major label promotion, studio film distribution) is increasingly performative. Institutional prestige remains symbolically valued, but actual commercial success is increasingly independent of gatekeeper approval (as evidenced by self-published bestsellers, TikTok-driven music hits, direct-to-streaming films). The ritual persists through institutional inertia and cultural prestige associations, not functional necessity. Theater ratio reflects the gap between gatekeepers' claimed necessity and their actual market function.
constraint_indexing:constraint_classification(creative_market_gatekeeping, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, some gatekeeping appears inherent: attention is scarce, discovery requires curation, quality assurance requires judgment. This perspective naturalizes gatekeeping as an immutable property of creative markets. However, the structural data reveals this as a false summit — the gatekeepers' necessity derives from technical/institutional factors (distribution costs, capital requirements, information asymmetries) that are contingent and eroding with technology change.
constraint_indexing:constraint_classification(creative_market_gatekeeping, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(creative_market_gatekeeping_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(creative_market_gatekeeping, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(creative_market_gatekeeping, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(creative_market_gatekeeping, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(creative_market_gatekeeping, TR),
    TR >= 0.70.

:- end_tests(creative_market_gatekeeping_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderately high, reflecting genuine economic asymmetry between gatekeepers and creators while acknowledging that some coordination value exists. The 10-year trend (0.35 → 0.58) shows intensification due to platform consolidation and algorithmic intermediation. Suppression (0.62): High. Multiple barriers constrain creator alternatives: capital requirements (professional recording/publishing infrastructure), information asymmetries (contract complexity, industry knowledge), regulatory barriers (copyright clearance, rights management), and career path dependency (institutional validation remains culturally valuable despite market erosion). However, suppression is eroding as digital infrastructure reduces capital barriers. Theater ratio (0.65): Moderate-high. Traditional gatekeeping functions divide into genuine coordination (connecting creators to audiences at scale, quality curation, rights management, marketing) and performative ritual (institutional validation, prestige signaling, exclusivity claims). The ratio rising reflects that performative content has grown as market barriers erode — gatekeepers increasingly emphasize cultural prestige rather than market function to justify continued control.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates the full range of DR classification from a single institutional arrangement. The gatekeeper sees coordination (Rope) — they genuinely solve the discovery and distribution problem. The emerging creator sees extraction (Snare) — they have no choice but to accept unfavorable terms. The independent artist sees hybrid coordination-extraction (Tangled Rope) — the system both enables and constrains. The coalition sees a temporary problem with sunset mechanisms (Scaffold) — alternative platforms are maturing. The institutional legitimacy apparatus sees its own theatrical degradation (Piton) — the ritual persists but its functional necessity erodes. The analytical observer risks seeing an immutable natural law of creative markets (false summit Mountain) — but the empirical trend shows extractiveness rising precisely because technological barriers are falling and gatekeepers are extracting more rent from pure institutional control rather than actual market value.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is determined by each agent's structural position relative to the gatekeeping constraint. Emerging creators with no alternatives face maximum extraction (d ≈ 0.95, trapped powerless agents). Independent artists with partial alternatives face moderate extraction (d ≈ 0.65, constrained moderate agents). Incumbent gatekeepers as beneficiaries face zero or negative extraction (d ≈ 0.10, arbitrage institutional agents — extraction flows toward them). Creator coalitions with growing bargaining power face moderate extraction (d ≈ 0.50, constrained organized agents). The piton classification derives from the theater gate — institutional legitimacy ritual persists despite eroding market function, creating a gap between claimed necessity and actual performance.
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLUTION: This constraint resolves mandatrophy by showing that the classification depends critically on the agent's structural position and exit options. The constraint is not 'one type that applies to all observers' but a perspectival presheaf over different structural positions. Emerging creators experience Snare (no exit, pure extraction). Independent artists experience Tangled Rope (partial exit, mixed coordination-extraction). Gatekeepers experience Rope (full arbitrage, pure coordination). Organizers experience Scaffold (agency and sunset mechanism). The false summit (analytical Mountain) reveals the risk of naturalizing institutional arrangements as laws of nature when they are actually contingent on eroding infrastructure (distribution monopoly, capital barriers, information asymmetry). The mandatrophy is resolved by recognizing that gatekeeping is neither pure coordination nor pure extraction, but rather a constraint that performs BOTH functions simultaneously at different levels, creating extractive redistribution (toward gatekeepers) while solving genuine coordination problems (discovery, distribution, quality assurance).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    gatekeeping_necessity_erosion,
    'What proportion of traditional gatekeeping functions are truly necessary (quality curation, audience connection) versus contingent on outdated distribution infrastructure?',
    'Comparative analysis of successful direct-to-audience creators vs gatekeeper-approved works; measurement of audience discovery mechanisms with/without gatekeepers; cost accounting for curation vs distribution vs marketing',
    'If necessary > 60%: gatekeeping is coordination-dominant (Rope/Tangled Rope at all perspectives). If necessary < 30%: gatekeeping is extraction-dominant (Snare/Tangled Rope for powerless agents). Distribution determines classification threshold.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(gatekeeping_necessity_erosion, empirical, 'Proportion of gatekeeping functions that are inherently necessary versus infrastructure-dependent').

omega_variable(
    platform_replacement_timeline,
    'At what point do direct-to-fan platforms (Patreon, Kickstarter, YouTube, TikTok, blockchain royalty systems) achieve sufficient maturity and scale to functionally replace traditional gatekeepers for viable creators?',
    'Longitudinal tracking of creator earnings distribution, audience reach, and venture funding in alternative platforms; correlation between platform maturity and reduction of gatekeeper contract terms',
    'If timeline < 5 years: scaffold sunset is likely for niche/independent creators. If timeline > 15 years: scaffold perspective is aspirational; gatekeeping constraint persists in Snare/Tangled Rope form.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(platform_replacement_timeline, empirical, 'Timeline for alternative platforms to achieve functional replacement of traditional gatekeepers').

omega_variable(
    institutional_capture_of_alternatives,
    'Do emerging platforms (Spotify, YouTube, TikTok) replicate gatekeeper extraction patterns or create genuinely more egalitarian access?',
    'Algorithm transparency analysis; measurement of discovery probability by creator tier; compensation equity vs traditional gatekeepers; barrier to entry (capital, technical, legal) for independent use of platform infrastructure',
    'If alternatives replicate gatekeeping: constraint reshapes rather than dissolves — extraction mechanism migrates to algorithmic gatekeeping. Scaffold perspective becomes aspirational mythology rather than structural reality.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(institutional_capture_of_alternatives, empirical, 'Whether emerging platforms replicate or genuinely replace gatekeeper extraction patterns').

omega_variable(
    identity_lock_in_creative_profession,
    'To what extent do aspiring creators remain in gatekeeper-dependent pathways due to identity fusion with ''legitimate'' creative legitimacy (traditional publisher, major label, major studio) rather than material barriers?',
    'Qualitative analysis of creator narratives; comparison of aspiration trajectories with capability assessments; interview data on perceived legitimacy of alternative paths; correlation between identity-lock markers and gatekeeper contract acceptance rates among creators with exit options',
    'If identity-lock is primary: many powerless perspectives should reclassify as identity_locked rather than trapped, revealing cognitive capture mechanism. If identity-lock is secondary: material barriers are primary constraint driver.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_in_creative_profession, conceptual, 'Extent of identity fusion with institutional legitimacy as a gatekeeping mechanism').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(creative_market_gatekeeping, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cmg_tr_t0, creative_market_gatekeeping, theater_ratio, 0, 0.45).
narrative_ontology:measurement(cmg_tr_t5, creative_market_gatekeeping, theater_ratio, 5, 0.58).
narrative_ontology:measurement(cmg_tr_t10, creative_market_gatekeeping, theater_ratio, 10, 0.65).

% Extraction over time
narrative_ontology:measurement(cmg_be_t0, creative_market_gatekeeping, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(cmg_be_t5, creative_market_gatekeeping, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(cmg_be_t10, creative_market_gatekeeping, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(creative_market_gatekeeping, resource_allocation).
narrative_ontology:affects_constraint(creative_market_gatekeeping, attention_scarcity_markets).
narrative_ontology:affects_constraint(creative_market_gatekeeping, creator_labor_exploitation).
narrative_ontology:affects_constraint(creative_market_gatekeeping, algorithmic_intermediation).

% DUAL FORMULATION NOTE:
% Creative market gatekeeping decomposes into three structurally distinct constraints: (1) attention scarcity and discovery coordination (ε ≈ 0.15, primarily Rope), (2) economic extraction from creators via contract terms and distribution monopoly (ε ≈ 0.68, primarily Snare for powerless creators), (3) institutional legitimacy theater as cultural gatekeeper (ε ≈ 0.35, primarily Piton). This story focuses on the hybrid coordination-extraction system as a unified constraint. Decomposition recommended if analysis requires distinguishing which functions drive which extractiveness value.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(creative_market_gatekeeping, institutional, 0.12).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
