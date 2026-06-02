% ============================================================================
% CONSTRAINT STORY: hub_short_form_tv_market_fragmentation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_hub_short_form_tv_market_fragmentation, []).

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
 *   constraint_id: hub_short_form_tv_market_fragmentation
 *   human_readable: Short-Form Video's Impact on Traditional TV Market Fragmentation
 *   domain: economic/media
 *
 * SUMMARY:
 *   The fragmentation of traditional television by short-form video platforms
 *   represents a structural shift in how consumer attention and advertising
 *   revenue are allocated across media formats. Rather than a simple market
 *   displacement, this constraint combines genuine coordination functions
 *   (matching viewers to preferred content experiences, enabling creator
 *   access without gatekeeping) with asymmetric extraction (platform capture
 *   of attention through algorithmic engagement maximization, data asymmetry
 *   with advertisers, regulatory arbitrage). Legacy broadcast and cable
 *   networks face trapped exits due to sunk infrastructure costs and
 *   regulatory obligations, while short-form platforms enjoy low friction
 *   mobility and algorithmic scale advantages. The constraint exhibits a full
 *   spectrum of DR types depending on the agent's structural position, from
 *   pure snare (trapped networks) to rope (platform operators). Theater ratio
 *   remains low (0.35) because the fragmentation is functionally
 *   real—algorithmic distribution genuinely does reach audiences more
 *   efficiently than traditional linear scheduling—rather than performative.
 *   The trajectory shows extractiveness rising sharply from 2015–2025 as
 *   platform dominance solidifies, while theater remains stable, indicating
 *   the coordination function persists alongside increasing extraction.
 *
 * KEY AGENTS:
 *   - Short-Form Platform Operators (institutional/arbitrage) — Primary beneficiaries; capture audience attention, ad revenue, and creator data; operate in lighter regulatory regimes
 *   - Traditional Broadcast Networks (powerless/trapped) — Primary victims; lose audience and revenue; sunk infrastructure and must-carry obligations prevent easy exit
 *   - Cable/ISP Providers (moderate/constrained) — Secondary victims; bundle value erodes as video consumption shifts; can exit video but lose revenue
 *   - Independent Short-Form Creators (organized/mobile) — Secondary beneficiaries; access algorithmic distribution without traditional gatekeeping; can switch platforms
 *   - Media Conglomerates (powerful/mobile) — Mixed position; must coordinate across traditional and short-form channels; can exit traditional TV but face complexity
 *   - Advertisers (moderate/mobile) — Mixed experience; gain targeting precision through algorithmic data but face opacity and extraction through CPM control
 *   - Broadcast Regulators (institutional/constrained) — Maintain must-carry and public interest rules that increasingly operate in shadow (piton); regulate traditional platforms while short-form platforms operate with lighter oversight
 *   - Viewers/Audiences (powerless/trapped) — Fragmented perspective; benefit from content choice but experience algorithmic extraction of autonomy
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hub_short_form_tv_market_fragmentation, 0.52).
domain_priors:suppression_score(hub_short_form_tv_market_fragmentation, 0.48).
domain_priors:theater_ratio(hub_short_form_tv_market_fragmentation, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hub_short_form_tv_market_fragmentation, extractiveness, 0.52).
narrative_ontology:constraint_metric(hub_short_form_tv_market_fragmentation, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(hub_short_form_tv_market_fragmentation, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hub_short_form_tv_market_fragmentation, tangled_rope).
narrative_ontology:human_readable(hub_short_form_tv_market_fragmentation, "Short-Form Video's Impact on Traditional TV Market Fragmentation").
narrative_ontology:topic_domain(hub_short_form_tv_market_fragmentation, "economic/media").

domain_priors:requires_active_enforcement(hub_short_form_tv_market_fragmentation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hub_short_form_tv_market_fragmentation, short_form_platform_operators).
narrative_ontology:constraint_beneficiary(hub_short_form_tv_market_fragmentation, content_creators_short_form).
narrative_ontology:constraint_victim(hub_short_form_tv_market_fragmentation, traditional_broadcast_networks).
narrative_ontology:constraint_victim(hub_short_form_tv_market_fragmentation, cable_providers).
narrative_ontology:constraint_victim(hub_short_form_tv_market_fragmentation, theatrical_distributors).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: LEGACY BROADCAST NETWORK (SNARE) — Trapped by regulatory constraints (broadcast licenses requiring local service), infrastructure capital investment in transmission networks, and contractual obligations to affiliates and content providers. Cannot exit the market without massive write-down. Fragmentation of audience directly reduces ad revenue and audience measurement. Zero negotiating power with short-form platforms; no viable alternative revenue model that preserves the traditional business.
constraint_indexing:constraint_classification(hub_short_form_tv_market_fragmentation, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: CABLE PROVIDER (SNARE) — Constrained by sunk infrastructure cost (fiber/coax network), regulatory franchise agreements with municipalities, and customer lock-in through bundling (broadband + video + phone). As short-form video consumption grows, bundle value erodes. Can marginally exit by deprioritizing video, but loses revenue. Cord-cutting accelerates as viewer preference shifts to streaming and short-form platforms.
constraint_indexing:constraint_classification(hub_short_form_tv_market_fragmentation, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: MEDIA CONGLOMERATE (TANGLED ROPE) — Powerful institutional actor with diverse revenue streams (broadcast, cable, streaming services, content production). Experiences fragmentation as a genuine coordination problem: it must maintain content pipelines across multiple formats and platforms simultaneously. Also extracts through exclusive content licensing, control of IP, and strategic platform partnerships. Can exit traditional TV to focus on streaming (has mobile options), but coordination across legacy and new platforms adds complexity and cost.
constraint_indexing:constraint_classification(hub_short_form_tv_market_fragmentation, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 4: SHORT-FORM PLATFORM OPERATOR (ROPE) — Primary beneficiary with full arbitrage exit. Controls algorithmic curation (low marginal cost of reaching new audiences), owns audience attention data, and extracts through advertising-based revenue model. Experiences fragmentation as beneficial coordination: user engagement metrics and network effects drive platform value. Can arbitrage audience attention across geographies and content types. Minimal regulatory burden and no legacy infrastructure obligations.
constraint_indexing:constraint_classification(hub_short_form_tv_market_fragmentation, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: INDEPENDENT SHORT-FORM CREATOR (ROPE) — Organized agent (creator collectives, talent agencies for influencers) with mobile exit options. Can switch between platforms (TikTok, YouTube Shorts, Instagram Reels). Experiences constraint as enabling coordination: algorithmic distribution and creator fund programs lower barriers to audience access vs traditional media gatekeeping. Benefits from fragmentation through reduced competition for visibility and direct monetization pathways.
constraint_indexing:constraint_classification(hub_short_form_tv_market_fragmentation, rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: BROADCAST REGULATION (PITON) — Public interest media regulations (FCC must-carry rules, local news requirements, educational programming mandates) are substantially performative. These rules were designed to ensure universal access in a scarcity environment; they persist through institutional inertia despite abundance. Short-form platforms operate in lighter regulatory regimes, creating asymmetric enforcement. The regulation maintains theater (compliance documentation, local content commitments) but diminishing functional protection for broadcast public interest goals.
constraint_indexing:constraint_classification(hub_short_form_tv_market_fragmentation, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 7: ADVERTISER (TANGLED ROPE) — Moderate institutional power with mobile exit options across platforms. Experiences both coordination benefit (fragmentation enables targeted microtargeting by algorithm) and extraction (platform algorithm opacity, opaque CPM/CPC pricing, privacy data asymmetry). Can exit to different platforms, but extraction through algorithmic control and data asymmetry remains constant. Must coordinate across multiple platforms to reach fragmented audiences.
constraint_indexing:constraint_classification(hub_short_form_tv_market_fragmentation, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER (MOUNTAIN VIEW) — From a civilizational perspective, human attention is fundamentally scarce. The rise of short-form video reflects a natural equilibrium allocation of fixed attention budget toward formats that maximize engagement per unit time. Fragmentation is not extraction but rather optimal sorting of viewers to their preferred content experiences. This perspective risks naturalizing market capture through algorithmic engagement maximization as an immutable law rather than a contingent design choice.
constraint_indexing:constraint_classification(hub_short_form_tv_market_fragmentation, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hub_short_form_tv_market_fragmentation_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(hub_short_form_tv_market_fragmentation, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(hub_short_form_tv_market_fragmentation, TypeOther, context(agent_power(powerful), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(hub_short_form_tv_market_fragmentation, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(hub_short_form_tv_market_fragmentation, TR),
    TR >= 0.70.

:- end_tests(hub_short_form_tv_market_fragmentation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The constraint exhibits measurable extraction at multiple levels: (1) short-form platforms capture 30–40% of daily media attention in young demographics, directly reducing traditional TV consumption and advertising revenue; (2) algorithmic control creates attention lock-in that users cannot easily escape; (3) data asymmetry between platforms and advertisers enables extraction of CPM/CPC value. However, extractiveness is not at snare levels (≥0.66) because coordination functions are genuine—algorithmic distribution does match users to content more efficiently than linear scheduling—and the extraction coexists with real consumer benefit (choice). Suppression (0.48): Moderate. Barriers exist but are not total: (1) regulatory constraints on traditional broadcasters (must-carry, licensing) but minimal constraints on platforms (regulatory arbitrage); (2) switching costs for creators are low; (3) capital barriers to entry for new platforms (moderate, not prohibitive). Suppression would be higher if platforms had genuine monopoly power, but multi-platform presence is feasible for creators and advertisers. Theater ratio (0.35): Low. The fragmentation is structurally real—engagement metrics are genuinely predictive of user behavior, algorithmic ranking does alter content discovery, and ad targeting does improve advertiser ROI. Theater would be higher if platforms maintained expensive compliance rituals (like broadcast must-carry) without functional effect; instead, platform operations are lean and directly tied to engagement metrics.
 *
 * PERSPECTIVAL GAP:
 *   The constraint generates maximum perspectival disagreement across structural positions. (1) Platform operators see a pure coordination gain (Rope): fragmenting audience to match content preference is efficient. (2) Broadcast networks see pure extraction (Snare): losing audience and revenue with no exit. (3) Media conglomerates see mixed coordination-extraction (Tangled Rope): must coordinate pipelines across platforms but also extract through IP licensing leverage. (4) Independent creators see coordination benefit (Rope): algorithmic distribution bypasses traditional gatekeeping. (5) Broadcast regulators see their own rules becoming degraded (Piton): must-carry and public interest mandates maintain ceremonial weight while platforms operate in lighter regimes. (6) Analytical observer risks naturalizing fragmentation as efficient sorting (Mountain): attentional equilibrium rather than extraction—but this perspectival choice obscures the role of algorithmic engagement maximization in driving that equilibrium. The perspectival gaps reveal that 'fragmentation' is not an objective phenomenon but rather the aggregate effect of asymmetric structural positions. Networks experience extraction because they cannot exit; platforms don't experience extraction because they control exit options.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) derives from each agent's structural position relative to attention extraction. Platform operators (institutional/arbitrage) have d ≈ 0.05–0.15 (full beneficiaries): they control algorithmic distribution and benefit from network effects; their exit options are arbitrage (can shift to new markets). Broadcast networks (powerless/trapped) have d ≈ 0.92 (full targets): they lose audience and revenue with no viable exit due to sunk costs. Independent creators (organized/mobile) have d ≈ 0.25–0.35 (slight beneficiaries): they gain distribution access but lose direct revenue control to platforms. Advertisers (moderate/mobile) have d ≈ 0.50–0.65 (symmetric to slight victim): they gain precision targeting but face data extraction and pricing opacity. The large perspectival gaps (platform d = 0.10 vs network d = 0.92) explain why platforms perceive Rope (coordination) while networks perceive Snare (extraction). Media conglomerates have intermediate d ≈ 0.45–0.55 because they can arbitrage across legacy and short-form, making the constraint a Tangled Rope from their perspective: genuine coordination burden (maintaining multiple pipelines) alongside genuine extraction (platform demand for exclusive content).
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by showing that classification varies coherently with agent structural position rather than collapsing into incoherence. The platform operator genuinely experiences Rope (low-friction coordination of creator distribution). The broadcast network genuinely experiences Snare (high-friction exit from legacy business). Both descriptions are structurally sound given their different exit options and beneficiary/victim status. The mandatrophy is NOT 'which classification is correct?' but 'what are the internal contradictions within each perspective?' Within the platform operator's Rope classification, there are no internal contradictions—low extraction (coordination), low suppression (multiple platforms available for creators), low theater (engagement metrics drive design). Within the broadcast network's Snare classification, there are no internal contradictions—high extraction (audience loss), high suppression (trapped by infrastructure and regulatory obligations), moderate theater (broadcasting ritual persists but is increasingly ineffectual). The perspectival gap is not a sign of failure but rather evidence that the constraint's structure is being correctly modeled. The analytical observer's Mountain perspective (attentional scarcity as natural law) IS internally contradictory: if fragmentation is just optimal equilibrium, why has theater_ratio remained stable at 0.35 rather than converging to 0 (pure coordination)? The stability of theater suggests hidden extraction mechanisms—platform engagement optimization and attention lock-in—that prevent the system from reaching true equilibrium. This contradiction surfaces the false summit: the constraint is not a natural law but a contingent institutional arrangement.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    algorithmic_engagement_vs_user_autonomy,
    'Is algorithmic short-form recommendation extraction (loss of user agency in content discovery) or genuine coordination (matching users to content they prefer)?',
    'User autonomy studies: comparison of choice architecture between short-form algorithm and traditional TV guide/EPG; measurement of user regret (post-viewing satisfaction delta); randomized intervention to disable algorithmic ranking',
    'If extraction dominates: short-form platforms are Snare from user perspective, not Rope. If coordination dominates: fragmentation is efficient equilibrium, not extractive capture. Changes classification for ''viewer as victim'' from Snare to Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(algorithmic_engagement_vs_user_autonomy, empirical, 'Whether algorithmic engagement is user preference matching or manipulative extraction').

omega_variable(
    public_interest_content_production,
    'Do short-form platforms produce or fund public interest content (local news, educational programming, civic information) at rates comparable to traditional broadcasters?',
    'Content audit: volume and reach of public interest programming on short-form platforms vs broadcast networks; advertiser funding for news and educational content; longitudinal tracking of local journalism funding across platforms',
    'If yes: fragmentation is neutral economic reallocation, not extraction of public goods. If no: extraction includes removal of public interest production — piton perspective on regulation strengthened, and broadcast must-carry rules become asymmetric.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(public_interest_content_production, empirical, 'Public interest content production on short-form vs traditional platforms').

omega_variable(
    platform_switching_friction,
    'Do creator and user switching costs between platforms represent genuine mobile exit or constrained exit in practice?',
    'Cross-platform creator analysis: measuring switching rates for creators with equivalent success on multiple platforms; creator cost accounting (time to rebuild audience, algorithm learning curves); user switching frequency tracking',
    'If high friction despite nominal mobility: exit_options should be ''constrained'' not ''mobile'' for creators. If low friction: arbitrage exit is real and sustained across creators. Affects directionality for creator perspectives.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(platform_switching_friction, empirical, 'Creator and user switching costs between short-form platforms').

omega_variable(
    geopolitical_extraction_dimension,
    'Does foreign ownership of short-form platforms (e.g., ByteDance/TikTok) constitute structural extraction of US/Western audience attention and data that should be modeled as a separate constraint?',
    'Data flow analysis: where user attention metrics and engagement data are processed; national security assessments of algorithmic control points; comparison to domestic platform architecture',
    'If yes: constraint should decompose into domestic market fragmentation (tangled rope) plus geopolitical data extraction (snare). If no: ownership structure is operationally irrelevant to market fragmentation mechanics. Changes scope from ''global'' to ''national'' in some perspectives.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(geopolitical_extraction_dimension, conceptual, 'Whether foreign platform ownership constitutes separate structural extraction').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hub_short_form_tv_market_fragmentation, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sfv_tr_t0, hub_short_form_tv_market_fragmentation, theater_ratio, 0, 0.28).
narrative_ontology:measurement(sfv_tr_t5, hub_short_form_tv_market_fragmentation, theater_ratio, 5, 0.31).
narrative_ontology:measurement(sfv_tr_t10, hub_short_form_tv_market_fragmentation, theater_ratio, 10, 0.35).

% Extraction over time
narrative_ontology:measurement(sfv_be_t0, hub_short_form_tv_market_fragmentation, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(sfv_be_t5, hub_short_form_tv_market_fragmentation, base_extractiveness, 5, 0.35).
narrative_ontology:measurement(sfv_be_t10, hub_short_form_tv_market_fragmentation, base_extractiveness, 10, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hub_short_form_tv_market_fragmentation, information_standard).
narrative_ontology:affects_constraint(hub_short_form_tv_market_fragmentation, broadcast_must_carry_regulatory_arbitrage).
narrative_ontology:affects_constraint(hub_short_form_tv_market_fragmentation, advertising_market_concentration).
narrative_ontology:affects_constraint(hub_short_form_tv_market_fragmentation, creator_economic_dependency).

% DUAL FORMULATION NOTE:
% This constraint should be decomposed into three structurally distinct claims: (1) market segmentation by format (short-form vs long-form consumption preferences)—low extractiveness, pure coordination; (2) algorithmic attention capture and lock-in—moderate-to-high extractiveness, platform extraction; (3) regulatory asymmetry (light regulation of platforms vs heavy regulation of broadcasters)—separate snare story for broadcasters. The 0.52 extractiveness value reflects aggregation across these mechanisms. For fine-grained policy analysis, decompose into network-linked stories by mechanism.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(hub_short_form_tv_market_fragmentation, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
