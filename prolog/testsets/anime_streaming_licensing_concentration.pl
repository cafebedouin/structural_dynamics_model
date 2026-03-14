% ============================================================================
% CONSTRAINT STORY: anime_streaming_licensing_concentration
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_anime_streaming_licensing_concentration, []).

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
 *   constraint_id: anime_streaming_licensing_concentration
 *   human_readable: Anime Streaming Licensing Concentration
 *   domain: media_distribution/entertainment_economics
 *
 * SUMMARY:
 *   The global anime streaming market has undergone rapid consolidation since
 *   2015, driven by three dominant platforms (Crunchyroll, Netflix, Amazon
 *   Prime) acquiring exclusive distribution rights to major studio content.
 *   This constraint exhibits the full diagnostic range of DR classification,
 *   making it an exemplar for how indexical position determines perceived
 *   extractiveness. From the platforms' perspective, exclusive licensing is
 *   coordination infrastructure enabling global anime discovery at scale.
 *   From independent creators' perspective, it is pure extraction: they have
 *   no alternative distribution channel and must accept platform-dictated
 *   terms. From the Japanese studio perspective, it is mixed: major studios
 *   have negotiating power, but smaller studios do not. The theater_ratio
 *   (0.48) indicates that the licensing mechanism is largely functional
 *   rather than performative—the actual content flows and revenue
 *   distributions occur, though the exclusivity architecture obscures
 *   alternatives. The extractiveness has increased from 0.35 (2016-era
 *   fragmented licensing) to 0.58 (current state), reflecting platform
 *   consolidation and increasing market concentration.
 *
 * KEY AGENTS:
 *   - Major Streaming Platforms (Crunchyroll, Netflix, Amazon Prime): Institutional/arbitrage — primary beneficiaries of exclusive licensing concentration; capture subscription revenue and audience data
 *   - Independent Anime Creators: Powerless/trapped — primary victims; no viable alternative distribution channels; revenue share heavily favors platforms
 *   - Major Japanese Animation Studios (MAPPA, Wit Studio, ufotable): Powerful/mobile — secondary beneficiaries; have negotiating leverage due to content value; can extract some concessions
 *   - Smaller Japanese Studios & Production Committees: Moderate/constrained — secondary victims; face unfavorable terms but have some exit options (direct-to-fan, regional licensing, genre pivot)
 *   - Regional Anime Distributors: Moderate/constrained — mixed position; platforms enable global reach but extract through exclusivity deals and territory restrictions
 *   - Consumer Anime Audience: Moderate/mobile — ambiguous position; benefit from accessible content but face geographic fragmentation and subscription multiplication
 *   - Open Distribution Coalition: Organized/constrained — emerging alternative pathway (HiDive, fan communities, decentralized experiments); currently constrained by network effects but building non-exclusive infrastructure
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(anime_streaming_licensing_concentration, 0.58).
domain_priors:suppression_score(anime_streaming_licensing_concentration, 0.65).
domain_priors:theater_ratio(anime_streaming_licensing_concentration, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(anime_streaming_licensing_concentration, extractiveness, 0.58).
narrative_ontology:constraint_metric(anime_streaming_licensing_concentration, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(anime_streaming_licensing_concentration, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(anime_streaming_licensing_concentration, tangled_rope).
narrative_ontology:human_readable(anime_streaming_licensing_concentration, "Anime Streaming Licensing Concentration").
narrative_ontology:topic_domain(anime_streaming_licensing_concentration, "media_distribution/entertainment_economics").

domain_priors:requires_active_enforcement(anime_streaming_licensing_concentration).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(anime_streaming_licensing_concentration, major_streaming_platforms).
narrative_ontology:constraint_beneficiary(anime_streaming_licensing_concentration, japanese_studios_with_platform_deals).
narrative_ontology:constraint_victim(anime_streaming_licensing_concentration, independent_anime_creators).
narrative_ontology:constraint_victim(anime_streaming_licensing_concentration, regional_distributors).
narrative_ontology:constraint_victim(anime_streaming_licensing_concentration, consumer_choice_ecosystem).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: INDEPENDENT ANIME CREATORS (SNARE) — Small studios and independent producers have no meaningful access to distribution infrastructure. Major platforms control the mechanisms through which anime reaches global audiences. Exit is structurally impossible: the creator cannot bypass the gatekeeper without abandoning the market entirely. Maximum extraction: licensing terms are dictated, revenue share heavily favors platforms, and the creator bears all production risk.
constraint_indexing:constraint_classification(anime_streaming_licensing_concentration, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: REGIONAL ANIME DISTRIBUTORS (TANGLED ROPE) — Mid-sized regional distributors (streaming services in specific countries, DVD/Blu-ray importers, local licensing agents) experience both coordination function and extraction. The major platforms enable them to reach audiences they could not serve alone (coordination benefit), but also extract through exclusive territory deals, forced bundling, and unfavorable licensing terms. Exit is costly but possible: they can pivot to other genres, but anime specialization makes this expensive. Significant extraction constrained by real alternatives.
constraint_indexing:constraint_classification(anime_streaming_licensing_concentration, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: MAJOR STREAMING PLATFORMS (ROPE) — The three dominant global platforms (Crunchyroll, Netflix, Amazon Prime) experience the licensing concentration as a pure coordination mechanism. They compete for exclusive content, yes, but the architecture fundamentally solves the discovery and distribution problem that anime faced in the pre-streaming era. The platforms have multiple exit options: they can invest in original anime, license competitors' content, or pivot to other genres. From their position, the constraint is a coordination win. They are beneficiaries with full agency.
constraint_indexing:constraint_classification(anime_streaming_licensing_concentration, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: TRADITIONAL ANIME INDUSTRY INCUMBENTS (PITON) — Japanese studios, production committees, and legacy distributors that once controlled international anime distribution now maintain vestigial roles through contractual inertia. They negotiated long-term exclusive deals with major platforms in the mid-2010s (when platform power was still consolidating) and now find those terms locked in while market power has shifted further toward platforms. The licensing ecosystem persists through these old contracts, but the actual coordination function has atrophied — platforms now directly negotiate with studios, bypassing traditional intermediaries. Theater_ratio is moderate (0.48) because the traditional ecosystem is still actively maintained through contracts, not yet fully theatrical.
constraint_indexing:constraint_classification(anime_streaming_licensing_concentration, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: OPEN ANIME DISTRIBUTION COALITION (SCAFFOLD) — Organized alternative initiatives (fan-subbed communities, independent streaming platforms like HiDive and HIDIVE, decentralized distribution experiments, and open-licensing anime studios) represent a sunset pathway. While currently constrained by network effects and content scarcity, these alternatives are building lower-extraction distribution infrastructure. Some indie studios are experimenting with direct-to-fan models, blockchain-based licensing, and cross-studio licensing pools. These are not yet fully functional but represent a genuinely emerging exit path. The scaffold classification assumes 10-15 year sunset horizon as alternative distribution matures.
constraint_indexing:constraint_classification(anime_streaming_licensing_concentration, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: MAJOR JAPANESE STUDIOS WITH PLATFORM POWER (TANGLED ROPE) — Large studios (MAPPA, Wit Studio, ufotable) have significant leverage: their content is so valuable that platforms compete for exclusive rights. These studios experience both coordination (platforms provide global distribution they could not build alone) and extraction (licensing terms are still platform-favorable, but negotiated from strength). They have mobile options: they can license to multiple platforms, invest in co-productions, or develop direct distribution. Unlike independent creators, they can extract concessions.
constraint_indexing:constraint_classification(anime_streaming_licensing_concentration, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, this appears to be an immutable feature of digital content distribution: scale economies in streaming infrastructure (CDN, licensing negotiation, content discovery algorithms) naturally concentrate distribution into a few platforms. The mathematical structure of network effects and first-mover advantage seems to make concentration inevitable. However, the structural data contradicts this — the concentration is contingent on: (a) current DRM and territorial licensing regimes that are regulatory choices, not physical laws; (b) platform exclusive licensing strategies that are business decisions, not technical necessities; (c) the absence of open-source or cooperative alternatives that could exist but don't due to coordination barriers. The engine will flag this as a false summit, revealing naturalization of contingent choices.
constraint_indexing:constraint_classification(anime_streaming_licensing_concentration, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(anime_streaming_licensing_concentration_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(anime_streaming_licensing_concentration, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(anime_streaming_licensing_concentration, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(anime_streaming_licensing_concentration, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(anime_streaming_licensing_concentration, TR),
    TR >= 0.70.

:- end_tests(anime_streaming_licensing_concentration_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. The constraint extracts significantly from independent creators (who have no exit) and moderately from smaller studios. However, it is not maximal because major studios can extract concessions, and the platforms do provide genuine coordination value (global distribution at scale). The increase from 0.35 to 0.58 reflects consolidation—early streaming (2015-16) had more platform competition and non-exclusive licensing; current state (2024+) shows concentration. Suppression (0.65): High. Multiple suppression mechanisms: (1) Network effects create natural monopoly tendencies in content discovery, (2) DRM and territorial licensing regimes legally restrict alternative distribution, (3) Exclusive contracts lock out competitors from high-value content, (4) Platform control over recommendation algorithms gives them unilateral power over visibility. Independent creators cannot realistically exit. Theater_ratio (0.48): Moderate-low. The licensing mechanism is largely functional—content genuinely flows, licensing terms are negotiated and enforced, revenue is distributed (though unfavorably). Some theater exists (performative exclusivity deals announced as major victories; bundling strategies that obscure actual value transfer) but less than in piton-class constraints.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates maximal perspectival divergence. The institutional beneficiary (platforms) perceives Rope—the constraint solves a coordination problem that didn't have a solution before streaming. The powerless victim (independent creators) perceives Snare—their extraction is nearly absolute. The moderately-positioned regional distributor perceives Tangled Rope—real coordination benefits mixed with real extraction costs. The scaffold perspective perceives a temporary constraint with a real sunset horizon as alternatives emerge. The piton perspective perceives a vestigial arrangement maintained through contractual inertia. The mountain perspective risks false naturalization—treating network effects as immutable laws rather than contingent artifacts of current regulatory and technical choices.
 *
 * DIRECTIONALITY LOGIC:
 *   The beneficiary/victim declarations feed directly into the directionality derivation. Platforms are declared beneficiaries: they capture subscription revenue (average anime viewer subscription is ~$15/month per platform, global market ~$5B annually), control recommendation algorithms (enabling bundling), and build audience data. Independent creators are declared victims: licensing deals typically offer 10-30% of viewer subscription revenue (depending on negotiation power), platforms retain unilateral control over content removal and payment terms, and creators bear all production risk. Regional distributors are both beneficiaries (platforms enable access to content they couldn't negotiate individually) and victims (exclusive territory restrictions reduce their market). This mixed position yields the Tangled Rope classification. The directionality override mechanism is unnecessary here—the structural data cleanly determines each agent's position through beneficiary/victim declarations.
 *
 * MANDATROPHY ANALYSIS:
 *   DIAGNOSTIC EXEMPLAR: This constraint resolves the mandatrophy by showing that all types (except Mountain) are legitimate perspectival readings. The mandatrophy question 'Is this coordination or extraction?' has different answers depending on position. For platforms, coordination: 100% (they coordinate global distribution, which solves a real problem). For creators, extraction: 100% (they have no alternatives and no bargaining power). For studios, mixed: 50-60% coordination, 40-50% extraction. The Tangled Rope classification captures this genuine mixing—it is not a confused compromise but a reflection that the constraint's function differs structurally by position. The mountain classification is a false summit: the constraint appears immutable from the analytical position but is actually contingent on regulatory and contractual choices. The scaffold classification is realistic: open-licensing alternatives are genuinely emerging, and the current extraction mechanism depends on maintaining platform monoply before alternatives mature. The piton classification is accurate: traditional industry intermediaries are maintained through old contracts even though their functional role has been superseded by direct platform-studio relationships.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    exclusive_licensing_necessity,
    'Are exclusive licensing deals necessary for platform profitability and content acquisition, or are they purely profit-maximizing strategies?',
    'Comparative analysis of platforms with exclusive vs non-exclusive licensing models; financial analysis of marginal revenue from exclusivity vs coordination costs; consumer demand studies on content-specific subscriptions',
    'If necessary: licensing concentration is a fundamental feature of the business model (higher suppression). If purely strategic: alternative non-exclusive models are viable (lower suppression, classification may shift to Rope).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(exclusive_licensing_necessity, empirical, 'Whether exclusive licensing is necessary for platform viability').

omega_variable(
    territorial_licensing_regime_dependency,
    'How much of the licensing concentration is driven by legacy territorial licensing laws and DRM regimes rather than by inherent distribution economics?',
    'Jurisdictional analysis: compare licensing concentration in territories with strong territorial licensing enforcement vs weak enforcement; analyze impact of DMCA anti-circumvention rules on platform licensing power; model counterfactual with international licensing simplification',
    'If highly dependent on territorial regime: regulatory reform could dramatically shift power away from platforms (extractiveness could drop from 0.58 to 0.25). If regime-independent: technical distribution economics dominate regardless of legal framework.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(territorial_licensing_regime_dependency, empirical, 'Territorial licensing regime dependency of concentration').

omega_variable(
    cooperative_platform_viability,
    'Could a cooperative or open-source anime streaming platform achieve sustainable operation with non-exclusive licensing?',
    'Business model analysis of existing cooperative streaming initiatives (Criterion, Letterboxd experimental streaming); technical feasibility study of federated streaming; analysis of coordination costs for multi-studio licensing pools',
    'If viable: scaffold sunset is realistic and extractiveness may decline as alternative emerges. If unviable: current concentration reflects necessary scaling economics (supports mountain perspective).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(cooperative_platform_viability, empirical, 'Viability of cooperative anime streaming alternatives').

omega_variable(
    content_creation_incentive_trade_off,
    'Does the concentration constraint (high extractiveness toward creators) reduce anime production quality or quantity?',
    'Longitudinal analysis of anime production volume and budget trends; comparison of studio revenue before/after platform consolidation; analysis of content diversity and risk-taking in original anime funded by platforms vs traditional production committees',
    'If production increases: extraction may be justified as enabling expansion (classification toward Rope). If production decreases or quality degrades: extraction is pure value transfer from creators to platforms.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(content_creation_incentive_trade_off, empirical, 'Whether licensing concentration impacts anime production incentives').

omega_variable(
    consumer_welfare_paradox,
    'Is the licensing concentration constraint extractive from consumers (high prices, geographic fragmentation) or beneficial (abundant accessible content at low cost)?',
    'Consumer price tracking across platforms and territories; accessibility analysis (fragmentation friction); comparison to pre-streaming era pricing and availability; welfare analysis including discovery quality and convenience',
    'If extractive from consumers: classification strengthens (higher ε toward consumer harm). If beneficial: constraint is coordination-dominant from consumer perspective (classification shift toward Rope).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(consumer_welfare_paradox, empirical, 'Consumer welfare impact of licensing concentration').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(anime_streaming_licensing_concentration, 0, 8).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(anime_lic_tr_t0, anime_streaming_licensing_concentration, theater_ratio, 0, 0.52).
narrative_ontology:measurement(anime_lic_tr_t4, anime_streaming_licensing_concentration, theater_ratio, 4, 0.5).
narrative_ontology:measurement(anime_lic_tr_t8, anime_streaming_licensing_concentration, theater_ratio, 8, 0.48).

% Extraction over time
narrative_ontology:measurement(anime_lic_be_t0, anime_streaming_licensing_concentration, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(anime_lic_be_t4, anime_streaming_licensing_concentration, base_extractiveness, 4, 0.48).
narrative_ontology:measurement(anime_lic_be_t8, anime_streaming_licensing_concentration, base_extractiveness, 8, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(anime_streaming_licensing_concentration, resource_allocation).
narrative_ontology:affects_constraint(anime_streaming_licensing_concentration, anime_production_financing).
narrative_ontology:affects_constraint(anime_streaming_licensing_concentration, fan_subtitling_legality).
narrative_ontology:affects_constraint(anime_streaming_licensing_concentration, digital_rights_management_enforcement).

% DUAL FORMULATION NOTE:
% Anime streaming licensing concentration is the distributional constraint upstream of content production decisions (studios choose projects based on platform funding availability), fan adaptation practices (platforms enforce DMCA-backed geographic restrictions forcing fan subbing in underserved regions), and digital rights enforcement regimes (platforms lobby for stronger anti-circumvention laws to maintain exclusive territorial control).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
