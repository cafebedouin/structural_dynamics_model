% ============================================================================
% CONSTRAINT STORY: news_paywall_inequality
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_news_paywall_inequality, []).

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
 *   constraint_id: news_paywall_inequality
 *   human_readable: Information Asymmetry due to News Paywalls
 *   domain: social/media/economics
 *
 * SUMMARY:
 *   News paywalls have emerged as a dominant revenue model in digital
 *   journalism over the past 15 years, creating a structural information
 *   asymmetry. Publishers claim paywalls enable sustainable quality
 *   journalism by capturing reader willingness-to-pay. However, the paywall
 *   also functions as an extraction mechanism: it restricts access to quality
 *   information based on ability to pay, creating different epistemic
 *   conditions for affluent and low-income populations. This creates a
 *   tangled situation combining genuine coordination function (paywalls fund
 *   journalism production) with asymmetric extraction (information rationed
 *   by income). The constraint exhibits all six types from different observer
 *   positions: pure extraction (low-income readers trapped), mixed
 *   coordination-extraction (price-sensitive readers), pure coordination
 *   (premium publishers), inter-institutional tension (libraries negotiating
 *   access), degraded ritual (classified advertising ecosystem atrophy), and
 *   risk of natural law false summit (information scarcity as economic
 *   inevitability). The theater_ratio has risen from 0.35 to 0.58 as paywalls
 *   have shifted from innovation (justified by sustainability crisis) to
 *   normalized business practice (increasingly justified by brand
 *   differentiation and metering rather than journalism funding necessity).
 *
 * KEY AGENTS:
 *   - Low-Income Readers: Primary victim (powerless/trapped) — cannot afford quality journalism, forced into lower-quality alternatives, no exit option
 *   - Premium News Publishers: Primary beneficiary (institutional/arbitrage) — capture subscription revenue and reader willingness-to-pay; high exit optionality through multi-model strategies
 *   - Price-Sensitive Readers: Secondary victim (moderate/constrained) — occasional access through subscriptions or workarounds; partial agency through free trials, account sharing, institutional access
 *   - Public Library Systems: Organized victim (organized/constrained) — negotiate access on behalf of constituents; constrained by publisher licensing and budget limits
 *   - Epistemic Commons Reliability: Victim category (powerless/trapped) — quality of public discourse degrades when portions of population lack access to high-quality sources
 *   - Classified Advertising Ecosystem: Institutional actor (institutional/constrained) — original revenue model degraded; paywalls maintained through inertia
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing contingent business model as inherent to information economics
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(news_paywall_inequality, 0.52).
domain_priors:suppression_score(news_paywall_inequality, 0.65).
domain_priors:theater_ratio(news_paywall_inequality, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(news_paywall_inequality, extractiveness, 0.52).
narrative_ontology:constraint_metric(news_paywall_inequality, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(news_paywall_inequality, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(news_paywall_inequality, tangled_rope).
narrative_ontology:human_readable(news_paywall_inequality, "Information Asymmetry due to News Paywalls").
narrative_ontology:topic_domain(news_paywall_inequality, "social/media/economics").

domain_priors:requires_active_enforcement(news_paywall_inequality).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(news_paywall_inequality, premium_news_publishers).
narrative_ontology:constraint_beneficiary(news_paywall_inequality, affluent_readers).
narrative_ontology:constraint_victim(news_paywall_inequality, low_income_readers).
narrative_ontology:constraint_victim(news_paywall_inequality, epistemic_commons_reliability).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: LOW-INCOME READER (SNARE) — Trapped by financial barriers to quality journalism. Forced to rely on free but lower-quality, algorithmically-biased, or sensationalist sources. Cannot exit without accepting epistemic degradation. Maximum experienced extraction through information deprivation.
constraint_indexing:constraint_classification(news_paywall_inequality, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: PRICE-SENSITIVE READER (TANGLED ROPE) — Can occasionally afford subscriptions or benefits from institutional access (library, workplace), but faces recurring payment barriers. Experiences both the coordination function (news enables civic engagement) and asymmetric extraction (quality content withheld unless paying). Partial agency through workarounds (account sharing, free trials, selective subscriptions).
constraint_indexing:constraint_classification(news_paywall_inequality, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: PREMIUM NEWS PUBLISHER (ROPE) — Benefits from paywall as a coordination mechanism: establishes revenue model that funds quality journalism, differentiates premium from commodity content, and enables sustainable editorial investment. Experiences paywall as enabling coordination with readers willing to pay. High exit optionality through subscription flexibility, multi-platform distribution, and advertising arbitrage.
constraint_indexing:constraint_classification(news_paywall_inequality, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: PUBLIC LIBRARY SYSTEM (TANGLED ROPE) — Organized institutional actor negotiating access for constituents. Functions coordinatively (libraries provide free access, extending journalism reach) but faces extractive pressure: publishers charge subscription fees or restrict simultaneous user limits, limiting the library's capacity to serve. Limited exit options constrained by budget cycles and publisher licensing terms.
constraint_indexing:constraint_classification(news_paywall_inequality, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: CLASSIFIED ADVERTISING ECOSYSTEM (PITON) — The original economic model sustaining newspapers (classified ads revenue) has atrophied with the rise of digital marketplaces (Craigslist, Facebook, specialized platforms). Paywalls persist partially as substitute revenue but lack the functional necessity of the advertising model they replaced. Theater ratio high: paywalls are maintained through institutional inertia even as their original justification (funding quality journalism through ads) has degraded. Constrained exit due to path dependence in subscription infrastructure.
constraint_indexing:constraint_classification(news_paywall_inequality, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a universal perspective, information has inherent scarcity properties: production cost is positive, editorial judgment requires human labor, investigative journalism requires significant capital investment. The constraint appears as a natural economic law: high-quality information production cannot be free at scale without alternative revenue. However, this perspective risks naturalizing a contingent institutional arrangement (paywall architecture) as inherent to information economics. The engine's false summit detector will identify this as naturalization rather than genuine NL property.
constraint_indexing:constraint_classification(news_paywall_inequality, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(news_paywall_inequality_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(news_paywall_inequality, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(news_paywall_inequality, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(news_paywall_inequality, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(news_paywall_inequality, TR),
    TR >= 0.70.

:- end_tests(news_paywall_inequality_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. Publishers extract significant value by restricting access to content based on ability to pay. The extraction is real but not maximal because publishers claim and partially deliver a coordination function (paywalls fund quality journalism) and because some free alternatives exist (public broadcasting, nonprofit outlets, libraries). The value increased from 0.28 to 0.52 over the interval as paywalls became normalized and subscription pricing increased, shifting from sustainability mechanism to profit maximization strategy. Suppression (0.65): Moderate-high. Significant barriers to information access include: financial barriers (subscription costs), technical barriers (paywall architecture, registration requirements), geographic barriers (regional outlet variation), and behavioral barriers (subscription fatigue, free-trial friction). These barriers are high but not total — public libraries, nonprofit journalism, and institutional access provide partial alternatives. Theater ratio (0.58): Moderate-high. Paywalls combine genuine revenue-raising function (theater_low) with increasing brand-differentiation and metering theater (theater_high). Initially justified as sustainability crisis response, they are now maintained partly through institutional path dependence and partly through genuine revenue generation. The rising trajectory reflects increasing theater as paywalls shift from necessity to standard practice.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates how the same structural phenomenon appears across the full typology. The premium publisher sees coordination (Rope) — paywalls fund journalism quality and enable reader-publisher matching. The low-income reader sees extraction (Snare) — information is withheld and they have no exit. The price-sensitive reader experiences both (Tangled Rope) — the system enables civic access when they can afford it but extracts when they cannot. Libraries see both (Tangled Rope) — they coordinate access for populations but face extractive licensing restrictions. The classified advertising ecosystem sees its own degradation (Piton) — paywalls persist as revenue substitute for the advertising model that atrophied. The analytical observer risks false summit (Mountain) — information production does cost money, and paywalls appear as natural economic necessity. However, this final perspective naturalizes a particular institutional choice (paywall architecture) rather than identifying a genuine natural law. The perspectival gap reveals that the constraint is fundamentally about distributional choice, not economic inevitability.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality (d) is derived from the agent's structural position. Premium publishers, as beneficiaries with arbitrage exit options (can shift to advertising, sponsorships, nonprofits), have low d (≈0.15) yielding negative or minimal experienced extraction χ. Low-income readers, as victims with trapped exit options (must read news or risk civic incompetence), have high d (≈0.95) yielding maximum experienced extraction. Price-sensitive readers, with moderate power and constrained exit (can sometimes subscribe but face friction), have d ≈0.60, experiencing moderate extraction. Libraries, as organized institutional victims with constrained exit (must negotiate with publishers, limited budgets), have d ≈0.55. The paywall infrastructure itself enforces this directionality through technical architecture: payment gating, user-agent detection, cookie-based metering. The constraint derives its force from this technical-institutional coupling rather than from information scarcity alone.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy in news paywalls is the potential confusion between 'paywalls are necessary because information has production costs' (Mountain, natural law) and 'paywalls are a particular institutional choice to capture reader willingness-to-pay' (Snare or Tangled Rope, contingent). The constraint resolves this by showing that information economics allows multiple models (advertising, nonprofit, public funding, hybrid) with different distributional consequences. The paywall is not a law of nature but a profit-maximizing institutional choice. The fact that quality journalism does require funding (true) does NOT imply that paywalls are the only or best way to fund it (false). The analytical observer's mountain perspective is a false summit revealing how economic naturalization can mask distributional extraction. The true constraint is Tangled Rope: paywalls do provide coordination function (fund journalism) but also extract through information restriction (create inequality). Resolving the mandatrophy requires analyzing whether the coordination benefit justifies the extraction cost — a question that depends on empirical measurement of journalism quality by funding model and social cost of information inequality.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    quality_journalism_funding_necessity,
    'Is paywall-driven subscription revenue the only viable model for sustainable quality journalism, or do alternative models (public funding, nonprofit structures, advertising plus freemium) provide comparable quality at lower information inequality cost?',
    'Longitudinal comparison of journalistic quality metrics (investigative pieces, original reporting, fact-check accuracy) across funding models; cost-per-article analysis; correlation between funding model and editorial independence',
    'If paywalls necessary: constraint appears more Mountain-like (structural economic requirement). If alternatives viable: constraint appears more Snare-like (extractive choice rather than inevitability).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(quality_journalism_funding_necessity, empirical, 'Whether paywall revenue is necessary for quality journalism sustainability').

omega_variable(
    free_content_quality_degradation,
    'Does free-to-access news content (social media, aggregators, open-access reporting) actually provide substantively lower epistemic quality than paywall-protected content, or is the quality gap smaller than paywalled publishers claim?',
    'Comparative epistemology: error rates, source diversity, fact-check validation, replicability of claims across paywalled vs free sources; reader comprehension and decision-quality when using each source type',
    'If large gap confirmed: paywall extraction has offsetting epistemic benefit (snare classification appropriate). If gap is small or null: paywall extraction is largely distributional with minimal epistemic justification (snare classification strengthened).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(free_content_quality_degradation, empirical, 'Quality differential between paywalled and free news content').

omega_variable(
    information_access_as_civic_capability,
    'How much does paywall-restricted access to quality journalism degrade civic capability (voting, political engagement, informed consent in policy questions) for low-income populations, and what is the social cost of this degradation?',
    'Controlled comparison of civic outcomes in populations with vs without paywall access; analysis of information access by income quintile; correlation between paywall prevalence and voter knowledge/participation by income',
    'If degradation is severe: supports snare classification (extractive deprivation). If minimal: constraint appears more as coordinated sorting (rope/tangled_rope).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(information_access_as_civic_capability, empirical, 'Civic capability impact of paywall-restricted information access').

omega_variable(
    alternative_information_access_substitutability,
    'Are free alternative information sources (public broadcasting, nonprofit journalism, social media, library resources) actually accessible and substitutable for paywalled content for low-income readers, or are paywalls creating genuine information gaps?',
    'User study on information access patterns by income; analysis of geographic and demographic variation in free alternative availability; measurement of information gaps (topics covered by free sources but not accessed by low-income readers)',
    'If substitutes available: constraint is more distributional than access-blocking (tangled_rope). If genuine gaps exist: constraint is more extraction-oriented (snare).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_information_access_substitutability, empirical, 'Substitutability of free alternative information sources').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(news_paywall_inequality, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(npi_tr_t0, news_paywall_inequality, theater_ratio, 0, 0.35).
narrative_ontology:measurement(npi_tr_t5, news_paywall_inequality, theater_ratio, 5, 0.48).
narrative_ontology:measurement(npi_tr_t10, news_paywall_inequality, theater_ratio, 10, 0.58).

% Extraction over time
narrative_ontology:measurement(npi_be_t0, news_paywall_inequality, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(npi_be_t5, news_paywall_inequality, base_extractiveness, 5, 0.4).
narrative_ontology:measurement(npi_be_t10, news_paywall_inequality, base_extractiveness, 10, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(news_paywall_inequality, information_standard).
narrative_ontology:affects_constraint(news_paywall_inequality, algorithmic_information_curation).
narrative_ontology:affects_constraint(news_paywall_inequality, digital_literacy_inequality).

% DUAL FORMULATION NOTE:
% The news paywall constraint is downstream of broader digital platform economics and upstream of information-dependent civic capability. Separate constraint stories should address: (1) the epistemology of free vs paywalled content (information quality question), (2) the platform algorithmic curation that affects free content discovery (filtering question), and (3) the civic outcomes of information access inequality (capability question). These are distinct constraints with different ε values that interact through the network.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(news_paywall_inequality, institutional, 0.4).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
