% ============================================================================
% CONSTRAINT STORY: manga_distribution_duopoly
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_manga_distribution_duopoly, []).

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
 *   constraint_id: manga_distribution_duopoly
 *   human_readable: Manga Distribution Duopoly in North America
 *   domain: economic/publishing
 *
 * SUMMARY:
 *   The North American manga distribution market exhibits a structural
 *   duopoly where Viz Media and Yen Press control the primary channels for
 *   licensing, printing, and distributing manga to retailers and readers.
 *   This constraint creates asymmetric extraction: the duopolists benefit
 *   from first-mover advantages, established relationships with Japanese
 *   publishers, and control of physical retail gatekeeping; creators, small
 *   publishers, and readers seeking non-mainstream titles bear the costs of
 *   limited access and constrained negotiating power. The constraint is a
 *   snare because the suppression (barriers to entry, network effects,
 *   licensing exclusivity) prevents meaningful exit, and extractiveness
 *   (margin concentration at distribution layer, creator royalty compression)
 *   exceeds legitimate coordination benefits. However, the constraint is
 *   under pressure from digital distribution channels (webcomics platforms,
 *   e-books, direct-to-reader sales) that bypass the physical retail
 *   bottleneck. The theater ratio has declined over the interval because the
 *   functional gatekeeping (distribution logistics, retail relationships) is
 *   real—this is not performative constraint—but the rise of digital
 *   alternatives makes the theater increasingly visible as contingent rather
 *   than inevitable. The duopoly persists through active enforcement
 *   (licensing exclusivity, retailer incentives, print-run control) rather
 *   than natural law or pure coordination.
 *
 * KEY AGENTS:
 *   - Viz Media and Yen Press: Primary beneficiaries (institutional/arbitrage) — extract margin from licensing fees, distribution control, and retail relationships
 *   - Independent Manga Creators: Primary victims (powerless/trapped) — cannot reach North American market scale without duopoly intermediation; self-publishing generates negligible revenue
 *   - Small Publishing Houses: Secondary victims (moderate/constrained) — limited negotiating leverage; dependent on duopoly distribution channels; forced to accept terms
 *   - Independent Bookstores: Secondary victims (moderate/constrained) — dependent on distributor inventory and terms; cannot curate independently; exit via direct wholesale blocked
 *   - Niche Manga Readers: Tertiary victims (powerless/trapped) — limited retail availability of non-mainstream titles; high search costs for alternative sources
 *   - Japanese Publishers (Kodansha, Shueisha, Kadakawa): Beneficiary-victims (organized/constrained) — benefit from duopoly coordination but trapped by dependence on single North American channel
 *   - Digital Platforms (Webtoon, Tapas, Comixology): Organized alternatives (organized/constrained) — building exit pathways for creators and readers; experiencing their own extraction through platform revenue shares
 *   - Legacy Retail System: Institutional actor (institutional/arbitrage) — gatekeeping enforced through physical shelf space; increasingly performative as digital distribution grows
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(manga_distribution_duopoly, 0.54).
domain_priors:suppression_score(manga_distribution_duopoly, 0.68).
domain_priors:theater_ratio(manga_distribution_duopoly, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(manga_distribution_duopoly, extractiveness, 0.54).
narrative_ontology:constraint_metric(manga_distribution_duopoly, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(manga_distribution_duopoly, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(manga_distribution_duopoly, snare).
narrative_ontology:human_readable(manga_distribution_duopoly, "Manga Distribution Duopoly in North America").
narrative_ontology:topic_domain(manga_distribution_duopoly, "economic/publishing").

% --- Structural relationships ---
narrative_ontology:constraint_victim(manga_distribution_duopoly, independent_manga_creators).
narrative_ontology:constraint_victim(manga_distribution_duopoly, small_publishing_houses).
narrative_ontology:constraint_victim(manga_distribution_duopoly, manga_readers_seeking_niche_titles).
narrative_ontology:constraint_victim(manga_distribution_duopoly, retail_bookstores).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: INDEPENDENT MANGA CREATOR (SNARE) — Trapped by the duopoly's gatekeeping. To reach the North American market at meaningful scale, creators must work through Viz or Yen Press. Self-publishing avenues (print-on-demand, direct e-distribution) generate negligible revenue. No viable exit from the constraint; extraction is total.
constraint_indexing:constraint_classification(manga_distribution_duopoly, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: SMALL PUBLISHING HOUSE (SNARE) — Constrained by distribution barriers. Small publishers can license manga but have minimal leverage in negotiations with distributors. Print runs are limited by retail shelf space (controlled by duopoly relationships). Margins are thin, and exit via direct-to-consumer is capital-intensive and reaches only niche audiences.
constraint_indexing:constraint_classification(manga_distribution_duopoly, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: INDEPENDENT BOOKSTORE (SNARE) — Constrained by supply chain. Bookstores must work through the duopoly's distribution channels to stock manga. Forced to stock what the distributors incentivize (often lower-margin titles), unable to curate specialized selections. Exit via direct wholesale relationships is blocked by publisher contracts; exit via non-manga inventory provides only partial relief.
constraint_indexing:constraint_classification(manga_distribution_duopoly, snare,
    context(agent_power(moderate),
            time_horizon(immediate),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: DUOPOLY DISTRIBUTOR (ROPE) — Experiences the constraint as coordination. Viz and Yen Press solve the genuine problem of aggregating diverse manga titles, managing licensing complexity with Japanese publishers, and distributing efficiently across North America. Their beneficiary position reflects legitimate functions: they coordinate a complex value chain. Exit options via arbitrage (negotiating better terms with retailers, licensing competitive titles, expanding direct sales) are available. They perceive the system as coordination, not extraction.
constraint_indexing:constraint_classification(manga_distribution_duopoly, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: MANGA READER (NICHE SEEKERS) (SNARE) — Trapped by limited retail availability. Readers seeking titles outside the duopoly's commercial focus (experimental works, non-commercial manga, genre-specific deep catalogs, translated works from smaller Japanese publishers) face minimal retail options and must rely on online imports or international editions. Digital alternatives exist but licensing fragmentation creates high search costs.
constraint_indexing:constraint_classification(manga_distribution_duopoly, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 6: WEBCOMICS/SELF-PUBLISHING COALITION (TANGLED ROPE) — Organized alternative distribution emerging through digital platforms (Webtoon, TAPAS, Comixology, Kickstarter). These platforms solve coordination (aggregating indie creators and readers) while extracting through revenue shares and algorithmic curation. Constrained by the duopoly's retail dominance but not fully trapped—digital distribution scales without traditional supply chain. Creators on these platforms have more agency than traditional publishing path victims.
constraint_indexing:constraint_classification(manga_distribution_duopoly, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: LEGACY RETAIL/LIBRARY SYSTEM (PITON) — The physical bookstore and library infrastructure that enforces the duopoly is increasingly performative. Digital distribution and direct-to-consumer channels reduce reliance on physical retail gatekeeping. The duopoly maintains control through institutional inertia (established relationships, contracted shelf space, publishing schedules optimized for brick-and-mortar distribution) rather than functional necessity. Theater ratio high because the system persists even as digital alternatives provide superior discovery and access.
constraint_indexing:constraint_classification(manga_distribution_duopoly, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 8: JAPANESE MANGA PUBLISHER (LICENSING PRINCIPAL) (TANGLED ROPE) — Japanese publishers (Shueisha, Kodansha, Kadokawa) benefit from Viz/Yen Press' distribution (coordination function) but are constrained by their dependence on a duopoly channel. They cannot easily diversify distribution without abandoning North American market share. Extract payment from both sides: they negotiate licensing fees with North American distributors and benefit from the distributors' market power. Organized but constrained by the duopoly relationship.
constraint_indexing:constraint_classification(manga_distribution_duopoly, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 9: DIGITAL PLATFORMS / DIRECT-TO-CONSUMER (SCAFFOLD) — Online distribution (Kindle, Comixology, Webtoon, Tapas, author websites, Patreon) is building an alternative pathway with sunset characteristics. As digital adoption increases and shipping costs matter less, the physical retail gatekeeping loses force. The constraint is temporary—digital channels reduce the duopoly's extraction mechanism. High mobility: creators and readers can bypass physical distribution entirely. Sunset visible in generational shift toward digital-native readers.
constraint_indexing:constraint_classification(manga_distribution_duopoly, scaffold,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 10: ANALYTICAL OBSERVER (SNARE) — From a civilizational/global scope, the duopoly is a structural extraction mechanism. The constraint exhibits high suppression (barriers to entry, network effects, licensing control), moderate-high extractiveness (profit margins concentrated at distribution layer), and low theater (actual gatekeeping function is real, not purely performative—retail shelf space and distribution logistics are genuine bottlenecks). The analytical view confirms the snare classification across most victim perspectives.
constraint_indexing:constraint_classification(manga_distribution_duopoly, snare,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(manga_distribution_duopoly_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(manga_distribution_duopoly, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(manga_distribution_duopoly, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(manga_distribution_duopoly, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(manga_distribution_duopoly, TR),
    TR >= 0.70.

:- end_tests(manga_distribution_duopoly_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.54): Moderate-high. The duopoly extracts through: (1) margin concentration at distribution layer (15-25% of retail price captured by distributor vs creator royalties typically 10%), (2) licensing exclusivity that prevents competitive distribution, (3) print-run control that limits small publisher and indie creator access. The extractiveness is not maximal (0.70+) because the duopolists do provide genuine coordination services (licensing management, print logistics, retail relationship management) that smaller players cannot replicate independently. Over the 10-year interval, extractiveness has increased as the duopolists have consolidated market share and standardized terms increasingly in their favor. Suppression (0.68): High. Barriers to entry include: (1) capital requirements for printing and warehousing, (2) network effects (retailers prefer single-distributor relationships for efficiency), (3) licensing control (Japanese publishers grant exclusive distribution rights), (4) retail gatekeeping (limited shelf space favors established titles), (5) knowledge barriers (international licensing, customs, currency). Exit options are severely constrained: self-publishing reaches minimal audience; digital alternatives exist but are not yet economically viable for most creators. Theater ratio (0.35): Moderate-low. This is a real functional gatekeeping constraint, not theatrical. The distributors actually control essential logistics and relationships. However, theater has declined over the interval as digital alternatives make it increasingly clear that the physical retail gatekeeping is contingent (not inevitable) and that alternative distribution models are viable.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap is pronounced and reflects the inequality in power and exit options. Institutional duopolists perceive Rope (pure coordination—solving genuine distribution problems). Organized alternatives perceive Scaffold (temporary problem being solved by digital disruption). Legacy retail perceives Piton (performative gatekeeping maintaining itself through inertia). Organized Japanese publishers perceive Tangled Rope (mixed coordination and dependence-based extraction). Moderate victims (bookstores, small publishers) perceive Snare with some agency (constrained snare). Powerless creators perceive pure Snare (trapped, no exit). Niche readers perceive Snare (limited availability, no power to change it). The analytical observer perceives Snare across victim perspectives and Rope from beneficiary perspective, producing a Tangled Rope analytical classification that resolves into Snare when victim interests are weighted. The gap reflects that the same structural arrangement is genuinely beneficial for coordinators but extractive for those dependent on the coordination.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values (d) follow from structural position and exit options. Powerless victims with trapped exits (independent creators) experience maximum d ≈ 0.95, yielding high f(d) ≈ 1.42 and perceived extraction chi. Constrained moderate victims (bookstores, small publishers) experience moderate d ≈ 0.70-0.80, yielding f(d) ≈ 1.00-1.15 and moderate-high chi. Institutional beneficiaries with arbitrage exits (duopolists) experience low d ≈ 0.10-0.15, yielding f(d) ≈ -0.05-(-0.01) and negative perceived extraction (they experience coordination benefit). Organized actors with constrained exits (Japanese publishers, digital platforms) occupy the middle: d ≈ 0.45-0.55, f(d) ≈ 0.40-0.70, experiencing the constraint as mixed coordination-extraction. The analytical observer at global scope sees the constraint's structural pattern: extraction is real and unequally distributed, flows from many powerless/moderate victims to few institutional beneficiaries, and persists through suppression rather than coordination superiority.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by noting that the constraint is a Snare (extractiveness 0.54 > 0.46, suppression 0.68 > 0.60, multiple victim groups with high d values, no meaningful coordination benefit exceeding extraction for powerless victims). The temptation to classify it as Rope (the duopolists do solve real distribution problems) is correctly rejected because the classification is victim-weighted and because the analytical observer sees asymmetric extraction. The analytical observer's snare classification (not rope) confirms that the duopoly's coordination function does not dominate its extraction mechanism. The Scaffold perspective (digital alternatives) is real and represents genuine exit pathway emergence, but it is aspirational rather than current—the scaffold is not yet mature enough to displace the snare constraint. The Piton perspective (legacy retail gatekeeping) correctly identifies that theater is present (the system persists partly through inertia) but correctly weights it as subordinate to actual gatekeeping function (theater_ratio = 0.35, below piton threshold of 0.70). Mandatrophy resolved: this is a Snare under active enforcement, with Scaffold alternatives emerging but not yet dominant, and with theater ratios too low to classify as Piton or dismiss as merely performative.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    digital_tipping_point,
    'At what digital market penetration does the physical retail duopoly lose effective control over manga accessibility?',
    'Longitudinal tracking of manga title availability: traditional retail vs digital platforms vs international imports; reader survey data on format preferences; creator revenue analysis across distribution channels',
    'If tipping point < 40% digital adoption: duopoly constraint weakens significantly within 5-10 years (scaffold perspective confirmed). If > 60%: physical gatekeeping persists as primary constraint longer than digital advocates predict.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(digital_tipping_point, empirical, 'Digital adoption threshold for duopoly weakening').

omega_variable(
    licensing_fragmentation_severity,
    'Does digital licensing fragmentation (different platforms have exclusive or region-locked titles) recreate the duopoly problem in digital form, or does it genuinely lower barriers to creator access?',
    'Comparative availability analysis: title coverage across digital platforms vs traditional distributors; transaction cost analysis for creators seeking multiple digital channels vs single physical distributor',
    'If fragmentation recreates extraction: digital channels are scaffold without real sunset—perpetual alternative gatekeeping. If fragmentation is manageable: true competitive pressure emerges and duopoly constraint weakens.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(licensing_fragmentation_severity, empirical, 'Whether digital fragmentation replaces or alleviates distribution control').

omega_variable(
    japanese_publisher_diversification,
    'Will major Japanese publishers (Kodansha, Shueisha, Kadokawa) establish direct-to-North America distribution, or will they continue relying on Viz/Yen Press intermediaries?',
    'Tracking of new English-language imprints, direct e-book distribution, licensing deals with non-traditional platforms; interviews with publisher international divisions',
    'If publishers diversify: duopoly loses upstream power and extractiveness drops. If they remain dependent: the snare persists because the duopoly controls the essential coordination function that publishers cannot replicate independently.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(japanese_publisher_diversification, empirical, 'Japanese publisher strategic diversification away from duopoly').

omega_variable(
    indie_manga_platform_viability,
    'Can indie manga platforms (Webtoon, Tapas, Patreon, direct author sites) achieve sufficient reader critical mass to generate sustainable creator revenue without duopoly-scale distribution?',
    'Revenue analysis of indie creators across platforms; platform growth rates; median creator earnings on indie vs traditional channels',
    'If viable: snare constraint weakens because creators have real alternative exit. If not viable: powerless creators remain trapped, and indie platforms remain niche, preserving the duopoly''s extraction mechanism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(indie_manga_platform_viability, empirical, 'Whether indie manga platforms can support creator livelihoods').

omega_variable(
    retail_consolidation_dynamics,
    'Does the decline of independent bookstores and rise of mega-retailers (Barnes & Noble, Amazon) strengthen or weaken the duopoly''s control?',
    'Historical analysis of distributor-retailer negotiations; market share of independent vs consolidated retailers; bargaining power dynamics over time',
    'If consolidation strengthens duopoly: fewer, larger retailers increase leverage for major distributors. If consolidation weakens duopoly: Amazon and B&N have scale to negotiate directly with publishers, creating competitive pressure.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(retail_consolidation_dynamics, empirical, 'Effect of retail consolidation on distributor market power').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(manga_distribution_duopoly, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(manga_dist_tr_t0, manga_distribution_duopoly, theater_ratio, 0, 0.42).
narrative_ontology:measurement(manga_dist_tr_t5, manga_distribution_duopoly, theater_ratio, 5, 0.38).
narrative_ontology:measurement(manga_dist_tr_t10, manga_distribution_duopoly, theater_ratio, 10, 0.35).

% Extraction over time
narrative_ontology:measurement(manga_dist_be_t0, manga_distribution_duopoly, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(manga_dist_be_t5, manga_distribution_duopoly, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(manga_dist_be_t10, manga_distribution_duopoly, base_extractiveness, 10, 0.54).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(manga_distribution_duopoly, resource_allocation).
narrative_ontology:affects_constraint(manga_distribution_duopoly, anime_streaming_licensing_concentration).
narrative_ontology:affects_constraint(manga_distribution_duopoly, international_publishing_access).

% DUAL FORMULATION NOTE:
% The manga distribution duopoly is structurally related to anime streaming licensing concentration (same Japanese publishers, same gatekeeping dynamics, different medium). The two constraints form a constraint family representing media access control in different channels. Independent analysis shows that streaming licenses are even more concentrated (fewer distributors) but with lower suppression (fewer capital barriers to entry), suggesting different ε values and classifications.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
