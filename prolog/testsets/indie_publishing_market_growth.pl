% ============================================================================
% CONSTRAINT STORY: indie_publishing_market_growth
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_indie_publishing_market_growth, []).

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
 *   constraint_id: indie_publishing_market_growth
 *   human_readable: Indie Publishing Market Growth and Author Extraction
 *   domain: publishing/digital_markets
 *
 * SUMMARY:
 *   The indie publishing market has grown from niche alternative to
 *   mass-market distribution channel (2009-2024), fundamentally restructuring
 *   author-reader relationships and literary gatekeeping. However, this
 *   growth has concentrated new extractive mechanisms: platform dependency,
 *   algorithmic opacity, and commission structures that capture 30-70% of
 *   author revenue while transferring discovery risk entirely to authors. The
 *   constraint exhibits dual structure: genuine coordination function
 *   (connecting authors globally to readers who would never find them through
 *   traditional retail) coexists with asymmetric extraction (platforms
 *   control visibility, pricing, and payment terms unilaterally). The
 *   constraint's evolution reveals the inadequacy of traditional gatekeeping
 *   (editorial review, bookstore placement) — it has not been eliminated but
 *   displaced by algorithmic gatekeeping, which is simultaneously more opaque
 *   and more dependent on author goodwill. The theatrical component (0.55)
 *   reflects that platform visibility mechanisms are partially performative:
 *   algorithms claim neutrality while optimizing for platform revenue, not
 *   author discoverability; authors perform marketing labor on platforms
 *   (reviews, cover optimization, launch timing) that the platforms capture
 *   as training data for recommendation systems.
 *
 * KEY AGENTS:
 *   - Emerging Authors: Primary victims (powerless/trapped) — lack brand recognition, face high discovery barriers, dependent on platform algorithms for reader access, pay highest commission rates
 *   - Mid-List Traditional Authors: Secondary victims (moderate/constrained) — caught between declining traditional advances and indie pressure, face choice between gatekeeping systems with no good exit
 *   - Established Indie Authors: Partial beneficiaries (powerful/mobile) — have built readership within platform ecosystem, retain some leverage, can threaten exit or diversify
 *   - Digital Platforms (Amazon KDP, Apple Books, Draft2Digital): Primary beneficiaries (institutional/arbitrage) — own distribution infrastructure, control algorithms, capture user data, arbitrage across regional markets
 *   - Aggregator Services (IngramSpark, BookBaby): Secondary beneficiaries (institutional/arbitrage) — take commission on physical printing and distribution, benefit from indie market growth
 *   - Direct-to-Reader Coalition: Organized resistance (organized/constrained) — Patreon, Substack, author websites, blockchain platforms building alternative discovery and payment mechanisms
 *   - Literary Institution (Awards, Reviews, Criticism): Degraded gatekeeper (institutional/arbitrage) — traditional prestige mechanisms persist through inertia; readers increasingly ignore gatekeeping signals in favor of peer recommendations
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(indie_publishing_market_growth, 0.52).
domain_priors:suppression_score(indie_publishing_market_growth, 0.48).
domain_priors:theater_ratio(indie_publishing_market_growth, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(indie_publishing_market_growth, extractiveness, 0.52).
narrative_ontology:constraint_metric(indie_publishing_market_growth, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(indie_publishing_market_growth, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(indie_publishing_market_growth, tangled_rope).
narrative_ontology:human_readable(indie_publishing_market_growth, "Indie Publishing Market Growth and Author Extraction").
narrative_ontology:topic_domain(indie_publishing_market_growth, "publishing/digital_markets").

domain_priors:requires_active_enforcement(indie_publishing_market_growth).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(indie_publishing_market_growth, digital_platforms).
narrative_ontology:constraint_beneficiary(indie_publishing_market_growth, aggregator_services).
narrative_ontology:constraint_beneficiary(indie_publishing_market_growth, early_adopter_authors).
narrative_ontology:constraint_victim(indie_publishing_market_growth, mid_list_traditional_authors).
narrative_ontology:constraint_victim(indie_publishing_market_growth, emerging_authors).
narrative_ontology:constraint_victim(indie_publishing_market_growth, literary_gatekeeping_stability).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: EMERGING AUTHOR (SNARE) — Trapped by the gatekeeping structure of indie publishing platforms. To reach readers, must use Amazon KDP, Apple Books, or equivalent; these platforms control distribution, pricing algorithms, and discovery ranking. No viable alternative for reaching global audience at scale. High suppression: platform commission (30-70%), algorithm opacity, payment delays, account termination risk. Minimal coordination benefit — platform exists primarily to extract author labor and content.
constraint_indexing:constraint_classification(indie_publishing_market_growth, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: MID-LIST TRADITIONAL AUTHOR (TANGLED ROPE) — Constrained by hybrid market: traditional publishing offers advance/credibility but declining backlist revenue and retail shelf space pressure. Indie publishing offers direct royalties (40-70%) but requires marketing, cover design, and distribution management. Cannot easily exit either market without career disruption. Genuine coordination function exists (matching readers to books across channels) alongside asymmetric extraction (platform retains 30-70% of transaction value, controls algorithm visibility).
constraint_indexing:constraint_classification(indie_publishing_market_growth, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: PUBLISHING PLATFORM (ROPE) — Net beneficiary with exit options. Coordinates legitimate function: matching author inventory to reader demand across global markets at minimal transaction cost. Captures network effects (reader audience attracts more authors; more authors attract more readers). Can arbitrage between regional markets, ebook formats, and traditional distribution partnerships. Experiences constraint as pure coordination with favorable terms — extraction flows toward this agent.
constraint_indexing:constraint_classification(indie_publishing_market_growth, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: ESTABLISHED INDIE AUTHOR (TANGLED ROPE) — Mobile with market power (established reader base, multi-book portfolio, negotiating leverage). Benefits from platform distribution and discovery (genuine coordination). Also experiences extraction through algorithm changes, genre market saturation, platform policy shifts. Can exit to direct sales or traditional publishing but faces switching costs (reader-base dependency on platform). Moderate extraction with some agency.
constraint_indexing:constraint_classification(indie_publishing_market_growth, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: DIRECT-TO-READER COALITION (SCAFFOLD) — Organized alternative ecosystem (Patreon, Substack, author websites, email lists, blockchain publishing platforms). Sees the platform-mediated indie publishing constraint as temporary — distributed author-to-reader relationships bypass algorithmic gatekeeping. Lower commission (5-10% vs 30-70%), higher author control, but smaller reach and higher marketing burden. Sunset logic: as reader comfort with direct subscriptions grows, platform dependency declines. Sunset timeline: 10-15 years for maturation across literary fiction and genre.
constraint_indexing:constraint_classification(indie_publishing_market_growth, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: LITERARY GATEKEEPING INSTITUTION (PITON) — Traditional literary criticism, awards, and institutional validation (National Book Award, reviews in major publications) persist through institutional inertia despite declining relevance. These gatekeeping mechanisms no longer effectively predict commercial success or reader satisfaction (indie bestsellers bypass them entirely). The constraint is maintained performatively for legitimacy and cultural prestige, even though readers increasingly ignore gatekeeping signals and follow algorithms or peer recommendations. Theater ratio high: gatekeeping ritual continues to confer status that readers no longer trust.
constraint_indexing:constraint_classification(indie_publishing_market_growth, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (TANGLED ROPE) — At civilizational scope, indie publishing market growth represents a structural shift from institutional gatekeeping to algorithmic gatekeeping. Genuine coordination function (connecting authors to readers) exists alongside sustained extraction (platform rents). Unlike traditional gatekeeping (performative but declining), algorithmic gatekeeping is functionally opaque and actively enforced through recommendation systems. The constraint evolves from piton (degraded institutional review) to tangled rope (active algorithmic enforcement of visibility asymmetry).
constraint_indexing:constraint_classification(indie_publishing_market_growth, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(indie_publishing_market_growth_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(indie_publishing_market_growth, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(indie_publishing_market_growth, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(indie_publishing_market_growth, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(indie_publishing_market_growth, TR),
    TR >= 0.70.

:- end_tests(indie_publishing_market_growth_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. Platform commission rates (30-70% for ebooks, 40-60% for print), algorithmic opacity preventing author discovery optimization, and unilateral policy enforcement create substantial value extraction. However, extraction is not maximal (0.70+) because authors retain ownership of content, can theoretically multi-publish across platforms, and the constraint includes genuine coordination (reader access that would not exist in traditional retail). The rising trajectory (0.28→0.52 over 10 years) reflects intensifying algorithmic gatekeeping as competition consolidates around major platforms and author dependency increases. Suppression (0.48): Moderate. Barriers include commission rates, algorithmic opacity, payment delays (30-90 days), account termination risk without appeal, and marketing burden on authors. However, suppression is not total because emerging authors CAN reach global audiences through indie platforms (impossible pre-2009), can bypass traditional gatekeeping entirely, and have alternative platforms and direct-to-reader channels available. Theater ratio (0.55): Moderate-high. Platform discovery claims (curated recommendations, bestseller lists, category rankings) are substantially performative — algorithms optimize for platform engagement metrics, not author discoverability; author visibility depends heavily on marketing spend and luck rather than algorithmic assessment of quality. The rising theater trajectory (0.35→0.55) reflects the growing gap between platform claims of neutral algorithmic discovery and author perception of algorithmic opacity and capriciousness.
 *
 * PERSPECTIVAL GAP:
 *   The constraint produces radically different classifications depending on structural position. Powerless emerging authors see pure extraction (Snare): trapped in platform ecosystem with no exit, facing high suppression and negligible coordination benefit. Moderate mid-list authors see hybrid extraction (Tangled Rope): both benefit from market access and bear costs of algorithmic risk and commission rates. Powerful established indie authors see coordination (Rope approaching): have audience leverage, multi-platform options, can negotiate better terms. Platforms see coordination (Rope): own the valuable discovery and distribution function, experience extraction flowing toward them. The organized direct-to-reader coalition sees a temporary constraint with a sunset (Scaffold): algorithms and platform dependence are problems being solved by distributed author-reader relationships. The traditional literary institution sees its own degradation (Piton): awards and reviews persist through prestige inertia despite reader preference for algorithmic or peer discovery. The civilizational analytical observer sees structural transformation (Tangled Rope): traditional gatekeeping replaced by algorithmic gatekeeping that coordinates discovery while extracting author labor and data.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is determined by each agent's structural position relative to extraction flows. Emerging authors (powerless/trapped) have maximum d (0.92-0.95) — full victims with no exit — producing highest experienced extraction chi. Mid-list authors (moderate/constrained) have moderate d (0.55-0.65) — face significant barriers but retain portfolio optionality — producing moderate chi. Established indie authors (powerful/mobile) have lower d (0.35-0.45) — can exit to competing platforms or direct-to-reader, negotiate terms, diversify — producing lower chi. Platforms (institutional/arbitrage) have low d (0.08-0.15) — beneficiaries with network effects and switching costs in their favor — producing negative or near-zero chi (extraction flowing toward them). The direct-to-reader coalition (organized/constrained) has moderate d (0.45-0.55) — constrained by current reader platform preference and discovery inertia but organized to build alternatives — producing moderate chi that declines over time as sunset progresses.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by recognizing the constraint as a structural transformation, not a binary classification. Indie publishing market growth is simultaneously solving the traditional gatekeeping problem (readers access more books, authors reach global audiences) and creating a new gatekeeping problem (algorithmic opacity, platform dependence, commission structures). The Snare classification (from powerless authors) is not wrong; the Rope classification (from platforms) is not wrong. They are different structural realities from different positions. The analytical observer cannot naturalize this as an immutable law (mountain) because the constraint is actively maintained through algorithmic design choices and commission enforcement — not inherent to publishing. Cannot reduce it to pure coordination (rope) because extraction is quantifiable and asymmetric. The Tangled Rope classification (claimed type) captures that coordination (author-reader matching, global distribution) and extraction (algorithmic gatekeeping, commission rates) are simultaneously present and structurally entangled. The constraint's future depends on whether direct-to-reader alternatives mature fast enough to undermine platform power (sunset trajectory) or whether platforms deepen algorithmic moats and lock-in (snare trajectory).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    platform_algorithmic_neutrality,
    'Are platform discovery algorithms neutral distributors or active gatekeepers extracting author labor for algorithmic training and recommendation value?',
    'Analysis of algorithm design incentives; comparison of visibility distribution for authors with vs without paid advertising; tracking of algorithmic changes that benefit platform interests vs author interests; audit of recommendation diversity across genres and popularity tiers',
    'If truly neutral: constraint is Rope (pure coordination) across all author perspectives. If extractive: constraint is Tangled Rope or Snare (asymmetric gatekeeping through algorithmic opacity).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(platform_algorithmic_neutrality, empirical, 'Whether platform algorithms function as neutral distribution or extractive gatekeeping').

omega_variable(
    author_coalition_power_threshold,
    'At what percentage of author defection do platform gatekeeping mechanisms break due to loss of content inventory?',
    'Historical precedent analysis (WordPress vs Blogger, Medium author exodus, Substack growth); modeling of critical mass thresholds for reader-following portability; measurement of author switching rates triggered by policy changes',
    'If threshold < 10%: platforms have low structural gatekeeping power — constraint is weaker than measured. If threshold > 30%: platforms have durable gatekeeping power despite author dissatisfaction — extraction is sustainable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(author_coalition_power_threshold, empirical, 'Critical author defection threshold for platform viability').

omega_variable(
    reader_discovery_preference,
    'Do readers trust algorithmic discovery or peer/community recommendations more, and does this preference shift the effective power of platform gatekeeping?',
    'Survey of reader discovery behavior across genres; analysis of book sales correlation with algorithm placement vs word-of-mouth; tracking of indie bestseller source attribution (organic discovery vs ads vs recommendations)',
    'If algorithms dominate: platform gatekeeping remains high-extraction. If peer recommendation dominates: direct-to-reader coalition sunset is accelerating, reducing platform extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reader_discovery_preference, empirical, 'Reader discovery preferences and algorithm vs community trust').

omega_variable(
    suppression_mechanism_internalization,
    'Is measured suppression (0.48) structural (technical barriers, commission rates, algorithmic opacity) or internalized (authors accept inequity as natural market outcome)?',
    'Pre- vs post-regulation suppression trajectory (if regulatory intervention reduces commissions or increases transparency, do authors perceive change?); comparison of author satisfaction with barriers vs satisfaction with income outcomes',
    'If structural: suppression persists after barrier removal. If internalized: authors carry suppression even when switching platforms; constraint''s effective suppression is higher than measured.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalization, empirical, 'Structural vs internalized suppression mechanism in indie author markets').

omega_variable(
    traditional_publishing_viability,
    'Is traditional publishing gatekeeping a viable alternative exit for powerless authors, or has midlist decline made it functionally inaccessible?',
    'Tracking of agent rejection rates for debut authors; analysis of advance trends for midlist vs bestseller authors; measurement of bookstore shelf allocation shrinkage',
    'If viable: powerless authors have exit option (constrained instead of trapped) — constraint becomes Tangled Rope from all perspectives. If inaccessible: powerless authors have no exit — constraint is Snare.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(traditional_publishing_viability, empirical, 'Traditional publishing viability as exit option for debut authors').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(indie_publishing_market_growth, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(indie_pub_tr_t0, indie_publishing_market_growth, theater_ratio, 0, 0.35).
narrative_ontology:measurement(indie_pub_tr_t5, indie_publishing_market_growth, theater_ratio, 5, 0.45).
narrative_ontology:measurement(indie_pub_tr_t10, indie_publishing_market_growth, theater_ratio, 10, 0.55).

% Extraction over time
narrative_ontology:measurement(indie_pub_be_t0, indie_publishing_market_growth, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(indie_pub_be_t5, indie_publishing_market_growth, base_extractiveness, 5, 0.4).
narrative_ontology:measurement(indie_pub_be_t10, indie_publishing_market_growth, base_extractiveness, 10, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(indie_publishing_market_growth, resource_allocation).
narrative_ontology:affects_constraint(indie_publishing_market_growth, traditional_publishing_decline).
narrative_ontology:affects_constraint(indie_publishing_market_growth, literary_prestige_gatekeeping).
narrative_ontology:affects_constraint(indie_publishing_market_growth, author_algorithmic_dependency).

% DUAL FORMULATION NOTE:
% Indie publishing market growth is downstream of traditional publishing's structural decline (retail consolidation, bookstore closures, midlist advance compression) and upstream of platform algorithmic dependency (the mechanism by which indie distribution creates new gatekeeping). These three constraints form a family: traditional_publishing_decline creates conditions for indie_publishing_market_growth, which in turn creates author_algorithmic_dependency. Each story has distinct extractiveness reflecting different causal mechanisms.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(indie_publishing_market_growth, institutional, 0.12).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
