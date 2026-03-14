% ============================================================================
% CONSTRAINT STORY: eu_directive_2001_84_ec
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_eu_directive_2001_84_ec, []).

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
 *   constraint_id: eu_directive_2001_84_ec
 *   human_readable: EU Directive 2001/84/EC (Droit de Suite / Artist Resale Right)
 *   domain: intellectual_property/art_market
 *
 * SUMMARY:
 *   EU Directive 2001/84/EC establishes droit de suite (artist resale right),
 *   requiring that artists or their heirs receive a percentage (0.25% to 4%)
 *   of revenue each time their work is sold on the secondary market. Framed
 *   as a cultural policy protecting artists from exploitation by dealers, the
 *   directive creates a mandatory royalty system administered through Member
 *   State collection agencies. The constraint exhibits all characteristics of
 *   a Tangled Rope: genuine coordination objective (equitable artist
 *   compensation in secondary markets) coexists with significant asymmetric
 *   extraction (administrative burden on dealers, inefficient remittance
 *   mechanisms, disproportionate impact on small traders). The theater ratio
 *   (0.58) reflects that compliance administration often exceeds actual
 *   artist benefit: collection agencies retain administrative margins,
 *   tracking overhead is substantial, and significant percentages of
 *   collected royalties accumulate unclaimed. The extractiveness has
 *   increased from 0.35 (initial implementation, high artist goodwill) to
 *   0.52 (mature implementation, established administrative burden) as
 *   institutional inertia has calcified and creative workarounds have
 *   diminished dealer exit options.
 *
 * KEY AGENTS:
 *   - Small Art Dealers: Primary victims (powerless/trapped) — face fixed compliance costs and cannot exit without abandoning business model. Disproportionately burdened compared to large galleries with dedicated royalty tracking infrastructure.
 *   - Artist Estates & Heirs: Primary beneficiaries (institutional/arbitrage) — receive passive royalty income through established institutional channels. No enforcement burden.
 *   - Large Auction Houses: Secondary beneficiary & constrained victim (moderate/constrained) — benefit from market standardization but experience compliance overhead. Can absorb costs across large transaction volumes.
 *   - Artist Unions & Copyright Advocates: Organized beneficiary (organized/mobile) — successfully lobbied for directive; maintain mobile exit through digital markets and alternative compensation models.
 *   - Collection Agencies: Institutional administrator (institutional/arbitrage) — benefits from mandatory fee structure; manages royalty distribution with significant administrative margin capture.
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks treating artist compensation as universal principle rather than contingent policy with measurable extraction effects.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(eu_directive_2001_84_ec, 0.52).
domain_priors:suppression_score(eu_directive_2001_84_ec, 0.48).
domain_priors:theater_ratio(eu_directive_2001_84_ec, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(eu_directive_2001_84_ec, extractiveness, 0.52).
narrative_ontology:constraint_metric(eu_directive_2001_84_ec, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(eu_directive_2001_84_ec, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(eu_directive_2001_84_ec, tangled_rope).
narrative_ontology:human_readable(eu_directive_2001_84_ec, "EU Directive 2001/84/EC (Droit de Suite / Artist Resale Right)").
narrative_ontology:topic_domain(eu_directive_2001_84_ec, "intellectual_property/art_market").

domain_priors:requires_active_enforcement(eu_directive_2001_84_ec).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(eu_directive_2001_84_ec, deceased_artist_heirs).
narrative_ontology:constraint_beneficiary(eu_directive_2001_84_ec, living_artists).
narrative_ontology:constraint_victim(eu_directive_2001_84_ec, art_market_traders).
narrative_ontology:constraint_victim(eu_directive_2001_84_ec, secondary_market_liquidity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SMALL ART DEALER (SNARE) — Trapped by legal obligation to track resale rights, remit royalties, and manage complex administrative compliance. Cannot exit the continental market without abandoning their business model. Bears the full administrative cost while large galleries absorb overhead. No genuine coordination benefit — pure extraction through regulatory burden.
constraint_indexing:constraint_classification(eu_directive_2001_84_ec, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(continental))).

% PERSPECTIVE 2: ART AUCTION HOUSE (TANGLED ROPE) — Constrained by enforcement requirements but also benefits from standardized resale tracking that creates market transparency. Genuine coordination (efficient secondary market information) coexists with asymmetric extraction (compliance cost falls disproportionately on smaller competitors). Medium-sized firms experience mixed burden and benefit.
constraint_indexing:constraint_classification(eu_directive_2001_84_ec, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 3: ARTIST ESTATE ADMINISTRATOR (ROPE) — Benefits from droit de suite without ongoing enforcement burden. Receives royalty remittances through established institutional channels. Experiences the directive as pure coordination: passive income from resales enables continued artist scholarship and estate management. Low extraction, high coordination benefit.
constraint_indexing:constraint_classification(eu_directive_2001_84_ec, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(continental))).

% PERSPECTIVE 4: ARTIST UNION & ADVOCACY COALITION (SCAFFOLD) — Organized agents that successfully lobbied for the directive saw it as temporary support for artist income during the transition to sustainable art markets. Artists with sufficient bargaining power have mobile exits: they can work outside the EU, negotiate direct contracts with galleries, or transition to digital/NFT markets. The sunset clause is implicit: if artists achieve stable income through alternative means, the directive's extraction rationale expires. Theater ratio reflects that compliance burden often exceeds actual royalty flows.
constraint_indexing:constraint_classification(eu_directive_2001_84_ec, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(continental))).

% PERSPECTIVE 5: COPYRIGHT COLLECTION ADMINISTRATION (PITON) — Collection agencies that manage remittance have become entrenched institutional actors with weak functional justification. Theater ratio (0.58) reflects that much administrative activity is procedural compliance rather than value-added distribution. The institution persists through legal mandate rather than demonstrable coordination benefit. Market data shows 30-50% of collected royalties never reach intended heirs due to administrative barriers.
constraint_indexing:constraint_classification(eu_directive_2001_84_ec, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(continental))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / INTERNATIONAL TRADE VIEW (MOUNTAIN) — From a global trade perspective, the directive appears as an immutable principle: intellectual property rights require protection across borders, and artist compensation is a fundamental norm that cannot be negotiated away. However, the structural data reveals this as false natural law — the directive has measurable extraction effects on secondary markets, variable compliance by jurisdiction, and significant dead weight loss. Non-EU markets (US, China, UK post-Brexit) have adopted alternative approaches (no droit de suite or voluntary licensing) that continue functioning, contradicting the universality claim.
constraint_indexing:constraint_classification(eu_directive_2001_84_ec, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(eu_directive_2001_84_ec_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(eu_directive_2001_84_ec, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(eu_directive_2001_84_ec, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(eu_directive_2001_84_ec, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(eu_directive_2001_84_ec, TR),
    TR >= 0.70.

:- end_tests(eu_directive_2001_84_ec_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The directive imposes mandatory royalty remittance with significant administrative overhead. Initial extractiveness (0.35) reflected genuine artist support enthusiasm and dealer compliance willingness. Current extractiveness (0.52) reflects settled equilibrium where institutional inefficiencies are apparent: collection agencies retain administrative margins (typically 10-20% of collected royalties), compliance burden falls disproportionately on small traders, and substantial royalties accumulate unclaimed (estimated 30-50% of total collected). The increase reflects not increasing regulatory stringency but crystallization of structural extraction as initial goodwill coordination has degraded into routine rent-seeking. Suppression (0.48): Moderate. Dealers face legal obligation to remit royalties with limited exit options within EU jurisdictions. However, suppression is not total: some escape through private sales, geographic arbitrage (pre-Brexit shift to London), or relocation of business to non-EU markets. Many small dealers operate with informal compliance or calculate risks of detection. Theater ratio (0.58): Moderate-high. Compliance administration is substantial — tracking resales, calculating percentages, remitting funds through collection agencies, maintaining records. Much of this activity is procedural compliance rather than value-addition. The theater has increased as bureaucratic procedures have solidified without corresponding improvements in remittance speed or accuracy.
 *
 * PERSPECTIVAL GAP:
 *   This constraint reveals the full tension between distributional justice and institutional efficiency. The artist estate sees pure coordination (Rope) — they benefit without effort. The artist union sees temporary support with exit paths (Scaffold) — organized agents can shift to alternative income models. The collection agency sees an entrenched role (Piton) — the institution persists through mandate rather than demonstrable value. Auction houses see mixed burden and benefit (Tangled Rope) — genuine coordination but also significant extraction. Small dealers see pure extraction (Snare) — trapped by compliance without proportional benefit. The analytical observer risks naturalizing the principle (Mountain) — but market evidence shows alternative models (US non-enforcement, UK voluntary licensing post-Brexit) function without the EU's mandatory apparatus, revealing the directive as contingent policy rather than universal principle.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values (d) derive from each agent's structural position relative to royalty extraction flow. Artist estates occupy d ≈ 0.15 (institutional beneficiary with arbitrage exit) — they receive royalties passively through collection infrastructure with no enforcement responsibility. They experience negative effective extraction (the constraint subsidizes them). Small dealers occupy d ≈ 0.92 (powerless with trapped exit) — they bear full compliance burden and cannot exit EU markets without abandoning their business. They experience maximum effective extraction. Large auction houses occupy d ≈ 0.55 (moderate power with constrained exit) — they experience mixed extraction (can absorb costs but face absolute obligation). Collection agencies occupy d ≈ 0.12 (institutional beneficiary through administrative fees) — they experience negative effective extraction (the constraint generates their revenue stream). The tangled_rope classification holds because both coordination (equitable artist access) and extraction (disproportionate dealer burden, administrative rent-seeking) are genuine structural features.
 *
 * MANDATROPHY ANALYSIS:
 *   The directive resolves the mandatrophy by distinguishing genuine coordination function from extractive institutional accretion. Years 0-3: Rope-dominant (genuine artist support, low resistance, dealer goodwill cooperation). Years 3-7: Tangled Rope transition (institutional overhead accrues, compliance becomes routine burden, extraction mechanism becomes visible). Years 7+: Piton danger zone (collection agencies become entrenched administrators, theater ratio rises, actual artist benefit declines). The classification must track this temporal dimension: the constraint's type changes as institutional inertia calcifies. Current status (2024) is Tangled Rope, but trajectories show either: (1) Sunset path (Scaffold resolution) — alternative artist income models (NFTs, direct licensing, digital distribution) replace resale royalty dependency, reducing directive's functional necessity; or (2) Extraction path (Snare consolidation) — administrative bureaucracy becomes self-justifying, royalty collection serves administrators more than artists, dealer resistance increases. Omega variables target the empirical distinction between these paths.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    compliance_cost_vs_actual_royalties,
    'Do the administrative compliance costs of tracking and remitting droit de suite royalties exceed the actual amount distributed to artists and heirs?',
    'Comparative analysis of EU collection agency data: total royalties collected vs. final payments to beneficiaries vs. administrative overhead. Cross-reference with Member State enforcement audits.',
    'If compliance_cost > royalties_distributed: the directive is a pure rent-seeking mechanism for administrative bureaucracy, not artist support (shift from Rope/Tangled Rope toward pure Snare). If royalties_distributed >> compliance_cost: coordination function is genuine (maintain or upgrade toward Rope).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(compliance_cost_vs_actual_royalties, empirical, 'Comparative analysis of compliance costs versus actual artist benefit').

omega_variable(
    secondary_market_liquidity_loss,
    'To what extent does droit de suite reduce secondary art market trading volume and liquidity?',
    'Time-series analysis of pre- and post-directive transaction volumes in EU markets. Comparison with non-EU markets (US art market) controlling for wealth and art collecting demographics. Measurement of price suppression for artworks below the royalty threshold.',
    'If volume decline > 15%: extractiveness should increase (dealers unable to pass royalty cost to buyers experience genuine snare). If volume decline < 5%: market has absorbed cost (tangled_rope classification sustained).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(secondary_market_liquidity_loss, empirical, 'Secondary market liquidity impact of droit de suite implementation').

omega_variable(
    beneficiary_identification_problem,
    'For deceased artists with complex ownership histories (contested estates, heirs in conflict, lost documentation), can the directive''s institutional framework reliably identify and compensate rightful beneficiaries?',
    'Audit of unclaimed royalties held by collection agencies; case studies of contested claims; comparison of payment success rates for contemporary vs. deceased artists; analysis of what percentage of collected royalties never reach a beneficiary.',
    'If > 40% of royalties accumulate unclaimed: the directive functions primarily as extraction from dealers to collection agencies (high piton classification). If < 10% unresolved: coordination function is genuine.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_identification_problem, empirical, 'Structural difficulty of identifying rightful heirs for deceased artists').

omega_variable(
    international_arbitrage_escape,
    'Can dealers and auction houses systematically avoid droit de suite by shifting transactions to non-EU jurisdictions or to exempt categories (private sales)?',
    'Market analysis of EU vs. non-EU auction volumes for comparable artworks; tracking of private sale market growth post-directive; case studies of high-value transactions relocating to London, New York, or Hong Kong after Brexit.',
    'If high-value art systematically migrates to non-EU markets: suppression is lower than measured (dealers have exit options beyond ''comply''), shifting classification toward Rope. If migration is blocked by market infrastructure: suppression remains high, classification stands as Tangled Rope or Snare.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(international_arbitrage_escape, empirical, 'Extent to which dealers can circumvent droit de suite through jurisdictional arbitrage').

omega_variable(
    artist_income_effect_measurement,
    'Have living artists'' average incomes from resale royalties increased measurably since the directive, or do most artists receive negligible payments?',
    'Survey data from artist organizations; comparison of reported royalty income pre- and post-directive (2001 baseline); stratification by career stage and market prominence.',
    'If top 10% of artists receive 90% of royalties: the directive functions as selective redistribution (Tangled Rope with asymmetric beneficiary structure). If distribution is relatively flat: coordination function is genuine. If most artists receive zero: the directive is rent-seeking without artist benefit (shift toward pure Snare or Piton).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(artist_income_effect_measurement, empirical, 'Actual income effect on artists from droit de suite royalties').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(eu_directive_2001_84_ec, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(eu_d_tr_t0, eu_directive_2001_84_ec, theater_ratio, 0, 0.42).
narrative_ontology:measurement(eu_d_tr_t5, eu_directive_2001_84_ec, theater_ratio, 5, 0.52).
narrative_ontology:measurement(eu_d_tr_t10, eu_directive_2001_84_ec, theater_ratio, 10, 0.58).

% Extraction over time
narrative_ontology:measurement(eu_d_be_t0, eu_directive_2001_84_ec, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(eu_d_be_t5, eu_directive_2001_84_ec, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(eu_d_be_t10, eu_directive_2001_84_ec, base_extractiveness, 10, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(eu_directive_2001_84_ec, resource_allocation).
narrative_ontology:boltzmann_floor_override(eu_directive_2001_84_ec, 0.18).
narrative_ontology:affects_constraint(eu_directive_2001_84_ec, art_market_secondary_trading).
narrative_ontology:affects_constraint(eu_directive_2001_84_ec, cultural_policy_enforcement).
narrative_ontology:affects_constraint(eu_directive_2001_84_ec, intellectual_property_harmonization).

% DUAL FORMULATION NOTE:
% EU Directive 2001/84/EC decomposes into two structurally distinct constraints: (1) artist_resale_right_principle (ε ≈ 0.15, Rope) — the genuine coordination principle that artists benefit from resale markets; (2) collection_agency_enforcement_mechanism (ε ≈ 0.68, Snare) — the bureaucratic implementation that extracts from dealers and intermediaries. The present story models the unified constraint; decomposition into principle vs. implementation mechanism is recommended for detailed analysis.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(eu_directive_2001_84_ec, institutional, 0.12).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
