% ============================================================================
% CONSTRAINT STORY: uk_artist_resale_right
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_uk_artist_resale_right, []).

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
 *   constraint_id: uk_artist_resale_right
 *   human_readable: UK Artist's Resale Right (ARR) Legislation
 *   domain: economic/legal
 *
 * SUMMARY:
 *   The UK Artist's Resale Right (ARR), enacted in 2006 and transposed from
 *   EU Directive 2001/84/EC, mandates that art market professionals remit a
 *   percentage of resale proceeds to the original artist or their estate. The
 *   constraint operates at the intersection of intellectual property law,
 *   market infrastructure, and artist welfare. ARR exhibits a stark
 *   perspectival gap: artists and estates experience it as either a rope
 *   (coordination enabling royalty claims) or a snare (administrative burden
 *   extracting more in compliance costs than in recovered royalties); dealers
 *   and auction houses experience it as tangled rope (mixed coordination and
 *   cost extraction); organized artist advocacy experiences it as
 *   legitimizing coordination with institutional overhead; digital art
 *   markets experience it as a temporary constraint being obsoleted by
 *   programmable smart-contract royalties. The constraint's extractiveness
 *   (0.38) reflects moderate administrative friction and dealer margin
 *   compression, partially offset by coordination benefits for institutional
 *   actors. Theater ratio (0.48) indicates that compliance remains
 *   substantially performative — many small dealers operate without
 *   systematic tracking, and enforcement is sporadic. Post-Brexit, the UK ARR
 *   persists through institutional inertia despite no longer being mandated
 *   by EU law, creating a piton dynamic where the original functional
 *   rationale (harmonization with EU markets) has been decoupled from the
 *   constraint's continued existence.
 *
 * KEY AGENTS:
 *   - Artists and Estates: Primary beneficiary (powerless/trapped) — legally entitled to royalties but face administrative barriers; trapped in system they cannot opt out of
 *   - Living Precarious Artists: Primary victim (moderate/constrained) — theoretically benefit from ARR but cannot effectively claim royalties; constrained exit from enforcement participation
 *   - Art Dealers and Galleries: Secondary beneficiary (institutional/arbitrage) — experience coordination benefits from transparent pricing and reduced fraud; arbitrage across borders to minimize ARR exposure
 *   - Auction Houses (Major): Secondary actor (powerful/arbitrage) — organized enforcement allows selective market focus (high-value pieces less impacted proportionally); mobile exit options
 *   - Artist Advocacy Organizations: Organized advocates (organized/constrained) — benefit from ARR as legitimizing institutional support but constrained by dependency on political will
 *   - UK Government/IPO: Enforcement institution (institutional/constrained) — maintains ARR framework through EU transposition legacy; constrained by limited enforcement resources
 *   - EU Regulatory Framework: Institutional ancestor (institutional/constrained) — original ARR source; post-Brexit decoupling creates inertial maintenance
 *   - Digital Art Market Platforms: Emerging alternative (organized/mobile) — programmable royalties create genuine exit pathway with sunset logic
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(uk_artist_resale_right, 0.38).
domain_priors:suppression_score(uk_artist_resale_right, 0.52).
domain_priors:theater_ratio(uk_artist_resale_right, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(uk_artist_resale_right, extractiveness, 0.38).
narrative_ontology:constraint_metric(uk_artist_resale_right, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(uk_artist_resale_right, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(uk_artist_resale_right, tangled_rope).
narrative_ontology:human_readable(uk_artist_resale_right, "UK Artist's Resale Right (ARR) Legislation").
narrative_ontology:topic_domain(uk_artist_resale_right, "economic/legal").

domain_priors:requires_active_enforcement(uk_artist_resale_right).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(uk_artist_resale_right, artists_and_estates).
narrative_ontology:constraint_beneficiary(uk_artist_resale_right, copyright_holders).
narrative_ontology:constraint_victim(uk_artist_resale_right, art_dealers_and_galleries).
narrative_ontology:constraint_victim(uk_artist_resale_right, auction_houses).
narrative_ontology:constraint_victim(uk_artist_resale_right, secondary_market_efficiency).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: EMERGING ARTIST ESTATE (SNARE) — Cannot exit the secondary market verification requirement. The artist's estate has no control over resales and cannot avoid the administrative burden of claiming royalties or the cost of enforcing rights. Trapped in a system where proving authorship and tracking sales requires substantial institutional capacity. Maximum experienced extraction through administrative friction.
constraint_indexing:constraint_classification(uk_artist_resale_right, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: PRECARIOUS LIVING ARTIST (SNARE) — Can theoretically benefit from ARR but faces severe barriers: poor market tracking, inability to afford enforcement, weak bargaining power against dealers who control access to buyers. The constraint extracts through enforced participation in a registration and claims system they cannot effectively navigate. Constrained exit — cannot ignore resales but cannot meaningfully claim royalties.
constraint_indexing:constraint_classification(uk_artist_resale_right, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: INTEGRATED ART DEALER (ROPE) — Benefits from coordination function: ARR creates transparent pricing signal, reduced fraud (forged authorship claims), and standardized terms for resales. Institutional actors with established dealer networks can arbitrage across borders — EU dealers face ARR as coordination mechanism; UK dealers post-ARR see it as normalized market infrastructure. Net beneficiary through efficiency gains.
constraint_indexing:constraint_classification(uk_artist_resale_right, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: MAJOR AUCTION HOUSE (TANGLED ROPE) — Experiences ARR as hybrid coordination-extraction. Coordination function: standardized royalty tables reduce dispute resolution costs and create transparent secondary market valuation. Extraction mechanism: ARR compliance creates administrative overhead and reduces margins on lower-value sales, shifting auction houses toward higher-value pieces where ARR impact is smaller proportionally. Organized enforcement allows arbitrage across EU/UK border to minimize exposure. Powerful agent with mobile exit options (shift business to non-ARR jurisdictions or focus on pre-1945 works).
constraint_indexing:constraint_classification(uk_artist_resale_right, tangled_rope,
    context(agent_power(powerful),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: ARTIST ADVOCACY ORG (TANGLED ROPE) — Benefits from ARR as legitimizing mechanism for artist rights discourse; gains power through coordinated artist representation. But advocacy org is constrained by dependency on institutional funding and political will — cannot exit if political support for ARR collapses. Enforcement mechanism extracts through administrative burden on advocacy org (tracking resales, processing claims on behalf of members). Mixed: genuine coordination benefit (collective voice) + extraction through institutional dependency.
constraint_indexing:constraint_classification(uk_artist_resale_right, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: EU RESALE RIGHT DIRECTIVE (PITON) — The UK ARR was originally transposed from EU Directive 2001/84/EC. Post-Brexit, the UK ARR persists through institutional inertia, maintaining the form of the EU directive despite no longer being bound by it. Theater ratio high (0.65): compliance reporting remains performative — many small dealers operate without systematic tracking, and enforcement is episodic rather than continuous. The constraint maintains itself through residual institutional legitimacy (EU origins) and weak enforcement capacity rather than active functional necessity. Negotiating UK/EU art market convergence creates theatrical compliance performance.
constraint_indexing:constraint_classification(uk_artist_resale_right, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: DIGITAL ART MARKET (SCAFFOLD) — Blockchain-based art provenance systems and NFT smart contracts embed resale royalties directly into the transaction layer, creating automatic ARR-equivalent enforcement without regulatory overhead. This is a genuinely emerging exit pathway with sunset logic: as digital art markets mature and smart-contract-mediated resales scale, the regulatory ARR framework becomes redundant infrastructure maintained only for physical art. Current theater_ratio is high (performative compliance), but sunset is real — programmable royalties eliminate the administrative fiction.
constraint_indexing:constraint_classification(uk_artist_resale_right, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER / NATURAL LAW (MOUNTAIN) — From a civilizational/universal perspective, resale rights are inherent to intellectual property law itself. Once an artist creates a work, the economic benefit of all subsequent transfers naturally follows from the work's authorship. The constraint appears immutable: property law universally attaches rights to creators. However, this is a false summit. ARR is a contingent policy choice (many countries with strong property law have no resale rights). The appearance of natural necessity reflects ideological naturalization of a distributional choice.
constraint_indexing:constraint_classification(uk_artist_resale_right, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(uk_artist_resale_right_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(uk_artist_resale_right, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(uk_artist_resale_right, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(uk_artist_resale_right, TR),
    TR >= 0.70.

:- end_tests(uk_artist_resale_right_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. ARR creates real costs for dealers (administrative tracking, royalty calculation, compliance verification) but the extraction is not severe because: (1) institutional dealers can systematize compliance, (2) artist royalty rates are fixed percentages (0.5%-4% depending on price tier), (3) auction houses and major galleries can pass costs forward through price adjustments. The extractiveness has increased slightly over the interval (0.28 → 0.38) as digital tracking has made non-compliance more detectable and enforcement more consistent. Suppression (0.52): Moderate-high. Dealers and galleries face significant barriers to opting out — statutory obligation applies to all professional resales. However, suppression is not total because: (1) private sales are exempt, (2) artists themselves are exempt when they resell directly, (3) works with unknown provenance can claim orphan status. The barrier is real but porous. Theater ratio (0.48): Moderate. ARR compliance is partially performative — many small dealers operate without systematic tracking, royalty remittance relies on self-reporting with sporadic audits, and enforcement by UK IPO is episodic. However, theater is not high because institutional actors (auction houses, major galleries) have systematized compliance and created transparent pricing signals that reduce ambiguity. The increase over the interval (0.35 → 0.48) reflects post-Brexit divergence where UK dealers maintain compliance performance without underlying EU harmonization mandate, creating residual institutional theater.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates the critical importance of power level and exit options in determining experience. The same legal requirement produces opposite classifications depending on the agent's structural position. A precarious artist with no market visibility (powerless/trapped) sees ARR as administrative extraction — compliance costs exceed expected royalties, and they cannot afford enforcement. A major auction house (powerful/arbitrage) sees ARR as coordination — it standardizes market practices and reduces fraud, costs are manageable at scale, and they can arbitrage across markets. An artist advocacy organization (organized/constrained) sees ARR as mixed — it provides institutional legitimacy and coordinating power but constrains the organization through dependency on political will. The piton perspective (EU directive) reveals how institutional frameworks persist after their functional rationale decouples — the UK maintained ARR post-Brexit despite no longer being bound by it, creating theatrical compliance divorced from the original harmonization purpose. The scaffold perspective (digital art markets) identifies the genuine exit pathway being built through programmable royalties, suggesting a real sunset for the statutory framework.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's experienced extractiveness derives from the agent's structural position relative to ARR's enforcement mechanism and royalty flow. Artists and estates occupy the beneficiary position but face high d (directionality toward extraction) because they lack exit options (trapped) and must participate in compliance infrastructure. Precarious artists experience moderate d because they are theoretically beneficiaries but functionally unable to claim (constrained exit). Institutional dealers and auction houses experience low d (beneficiary-to-neutral) because they have arbitrage options (shift to non-ARR markets, focus on pre-1945 exempt works, increase private sales). The artist advocacy organization experiences moderate-to-high d because it is structurally dependent on the ARR framework for institutional legitimacy (constrained exit). The EU directive perspective experiences moderate-high d (inertial maintenance post-Brexit). The digital art market platform experiences low d because smart-contract royalties offer a genuine exit pathway (mobile) with functional equivalence to ARR, making the statutory constraint increasingly optional.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION: The ARR constraint resolves the mandatrophy by clarifying that the classification varies legitimately by agent position, not because the constraint is mislabeled. The tangled_rope classification is correct: ARR exhibits genuine coordination function (transparent resale pricing, reduced fraud, institutional legitimacy) AND asymmetric extraction (administrative burden on dealers, barriers to artist claims, market margin compression). The constraint is neither pure coordination (rope) nor pure extraction (snare) — it is a hybrid requiring active enforcement. The mandatrophy arises from conflating the legal intent (artist benefit) with the structural reality (mixed extraction and coordination). Precarious artists see snare (extraction dominates their experience); institutional dealers see rope (coordination dominates); organized advocacy sees tangled rope (mixed); digital platforms see scaffold (temporary). All classifications are correct from their respective structural positions. The constraint is not misidentified; rather, it is legitimately experienced as different types depending on the agent's power level and exit options. The piton perspective flags institutional inertia (EU transposition legacy) as reducing the constraint's functional necessity. The scaffold perspective identifies the real sunset pathway (programmable royalties in blockchain systems). Resolution: tangled_rope is the canonical classification; perspectival variation is structural, not error.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    dealer_compliance_cost_threshold,
    'What compliance cost threshold determines whether dealers systematize ARR tracking versus operate in shadow market or exit UK altogether?',
    'Empirical cost accounting for dealer compliance infrastructure (registration, provenance tracking, royalty calculation, payment processing); correlation with dealer exit rates and shadow-market indicators',
    'If threshold < £500/sale: dealers systematically comply; ARR functions as intended. If threshold > £2000/sale: widespread non-compliance and exit; ARR enforcement becomes theater.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(dealer_compliance_cost_threshold, empirical, 'Cost threshold determining dealer compliance versus exit').

omega_variable(
    artist_claim_rate_disparity,
    'What explains the persistent gap between eligible artists and those actively claiming ARR royalties — institutional neglect, information asymmetry, or rational non-participation?',
    'Survey of artist awareness and claims rates; statistical analysis of claim patterns by artist power level, market price tier, and time since sale',
    'If disparity < 20%: ARR works as designed — most artists aware and claiming. If disparity > 60%: ARR primarily benefits organized/estate actors; precarious artists see extraction without benefit.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(artist_claim_rate_disparity, empirical, 'Gap between eligible artists and active claimants').

omega_variable(
    uk_eu_market_fragmentation_irreversibility,
    'Is post-Brexit divergence in resale rights treatment between UK and EU creating permanent market bifurcation or temporary regulatory fragmentation?',
    'Trade flow analysis (UK art exports to EU, EU art sales in UK) over 5-10 years post-Brexit; correlation with dealer relocation and auction house regional specialization',
    'If fragmentation persists > 5 years: distinct UK/EU art markets with different compliance regimes; ARR becomes locked-in constraint. If convergence occurs: regulatory harmonization or UK ARR reform; constraint becomes negotiable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(uk_eu_market_fragmentation_irreversibility, empirical, 'Whether UK-EU art market bifurcation is permanent or temporary').

omega_variable(
    smart_contract_royalty_legal_equivalence,
    'Will blockchain-embedded resale royalties (programmable royalties in NFT smart contracts) satisfy UK ARR legal requirements or require parallel statutory compliance?',
    'UK court decisions on smart-contract royalty enforceability; statutory amendment or regulatory guidance clarifying legal equivalence',
    'If legal equivalence granted: smart contracts become viable ARR alternative; scaffold sunset is real. If legal equivalence denied: dual-track compliance required; constraint persists through regulatory ossification.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(smart_contract_royalty_legal_equivalence, conceptual, 'Whether smart-contract royalties count as ARR compliance').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(uk_artist_resale_right, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(arr_tr_t0, uk_artist_resale_right, theater_ratio, 0, 0.35).
narrative_ontology:measurement(arr_tr_t5, uk_artist_resale_right, theater_ratio, 5, 0.42).
narrative_ontology:measurement(arr_tr_t10, uk_artist_resale_right, theater_ratio, 10, 0.48).

% Extraction over time
narrative_ontology:measurement(arr_be_t0, uk_artist_resale_right, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(arr_be_t5, uk_artist_resale_right, base_extractiveness, 5, 0.32).
narrative_ontology:measurement(arr_be_t10, uk_artist_resale_right, base_extractiveness, 10, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(uk_artist_resale_right, information_standard).
narrative_ontology:affects_constraint(uk_artist_resale_right, eu_directive_2001_84_ec).
narrative_ontology:affects_constraint(uk_artist_resale_right, uk_copyright_law_framework).
narrative_ontology:affects_constraint(uk_artist_resale_right, blockchain_royalty_standards).

% DUAL FORMULATION NOTE:
% ARR is downstream of the EU Directive 2001/84/EC (which it implements) and relates structurally to broader UK copyright law. Post-Brexit, ARR persists through institutional inertia despite decoupling from its original EU mandate. The blockchain royalty standards represent an emerging alternative constraint that would make ARR redundant if legal equivalence is granted to smart-contract-embedded royalties.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(uk_artist_resale_right, powerless, 0.85).
constraint_indexing:directionality_override(uk_artist_resale_right, institutional, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
