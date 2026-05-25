% ============================================================================
% CONSTRAINT STORY: underwriter_syndication_power
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_underwriter_syndication_power, []).

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
 *   constraint_id: underwriter_syndication_power
 *   human_readable: Underwriter Syndication Power in Capital Markets
 *   domain: financial/capital_markets
 *
 * SUMMARY:
 *   Underwriter syndication power in capital markets represents a core
 *   extraction mechanism embedded in the infrastructure for public capital
 *   raising. When a company decides to go public or raise capital via a
 *   public offering, a lead underwriter controls critical decision points:
 *   which investors receive shares (allocation discretion), at what price
 *   (pricing power), with what research coverage promises (information
 *   bundling). This creates an asymmetric relationship where the issuer
 *   (structurally trapped without capital access alternatives), the
 *   institutional investor (constrained by allocation dependencies), and the
 *   lead underwriter (positioned to extract rents) operate under different
 *   structural constraints. The constraint exhibits all six DR
 *   classifications depending on observer position: a snare for trapped
 *   issuers, tangled rope for institutional investors balancing coordination
 *   benefits against allocation extraction, pure rope for the lead
 *   underwriter seeing coordination, piton for the regulatory rituals that
 *   legitimize allocation outcomes, and a false natural law for observers who
 *   treat information asymmetry as immutable. The extractiveness has
 *   increased over 30 years (0.42 → 0.62) as spreads widened, relationship
 *   intensity deepened, and informal quid pro quo expectations (research
 *   coverage, capital access, mandate expansion) intensified. Theater ratio
 *   remained relatively stable (0.38 → 0.48) because formal allocation
 *   processes (pricing committees, syndicate agreements) continue
 *   performatively while real allocation is driven by bilateral
 *   relationships.
 *
 * KEY AGENTS:
 *   - Issuer (Company): Primary victim (powerless/trapped) — structurally dependent on underwriter for capital access, faces pricing extraction and allocation discretion, zero exit during offering window
 *   - Lead Underwriter: Primary beneficiary (institutional/arbitrage) — controls pricing, allocation, and investor matching; experiences constraint as coordination; operates within global capital markets with low switching costs
 *   - Institutional Investor: Secondary victim/beneficiary (moderate/constrained) — benefits from syndicate access (early allocation) but faces extraction through allocation discretion and implicit business flow expectations; has constrained exit options
 *   - Market Structure (Ecosystem Dynamics): Powerful agent (powerful/mobile) — coordinates capital allocation while perpetuating underwriter concentration; experiences gradual dilution from alternative capital paths (direct listings, SPACs, private credit)
 *   - Regulatory Framework: Institutional actor (institutional/arbitrage) — maintains ceremony around fair process and allocation while real allocation is determined by relationships; performs legitimation function
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing contingent institutional arrangements (underwriter bottleneck) as immutable features of capital markets
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(underwriter_syndication_power, 0.58).
domain_priors:suppression_score(underwriter_syndication_power, 0.68).
domain_priors:theater_ratio(underwriter_syndication_power, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(underwriter_syndication_power, extractiveness, 0.58).
narrative_ontology:constraint_metric(underwriter_syndication_power, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(underwriter_syndication_power, theater_ratio, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(underwriter_syndication_power, tangled_rope).
narrative_ontology:human_readable(underwriter_syndication_power, "Underwriter Syndication Power in Capital Markets").
narrative_ontology:topic_domain(underwriter_syndication_power, "financial/capital_markets").

domain_priors:requires_active_enforcement(underwriter_syndication_power).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(underwriter_syndication_power, lead_underwriter).
narrative_ontology:constraint_victim(underwriter_syndication_power, issuers).
narrative_ontology:constraint_victim(underwriter_syndication_power, capital_market_efficiency).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ISSUER STRUCTURAL POSITION (SNARE) — A company seeking to raise capital via public offering faces a lead underwriter who controls syndicate composition, pricing, allocation, and distribution. The issuer cannot exit without abandoning capital-raising entirely. The underwriter extracts through pricing spreads (typically 3-7% for equity offerings), allocation discretion (choosing which institutional investors receive shares, rewarding favored clients), and research coverage promises (implicit quid pro quo linking analyst coverage to future underwriting business). Maximum suppression because the issuer's alternatives (private equity, bank loans, retained earnings) are structurally inferior for growth funding. Zero degrees of freedom during the offering window.
constraint_indexing:constraint_classification(underwriter_syndication_power, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: INSTITUTIONAL INVESTOR (TANGLED ROPE) — Large asset managers benefit from syndicate access (early allocation of hot offerings) but face extraction through allocation discretion and implicit expectations of future business flow (mandate expansion, asset custody). They experience real coordination (underwriters match offerings to investor mandates, solve information asymmetry) alongside asymmetric extraction (allocation favors investors who feed the underwriter other business). High exit costs (switching underwriters means losing syndicate access for future offerings) but genuine options exist (direct offerings, secondary market accumulation, private placements). Moderate perceived extraction because they both use and fund the system.
constraint_indexing:constraint_classification(underwriter_syndication_power, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: LEAD UNDERWRITER (ROPE) — The lead underwriter controls the syndication structure and pricing, benefiting from spread revenue, client relationships, and information asymmetry. They experience the constraint as pure coordination: matching issuers to investors, managing roadshow logistics, building trust networks. Arbitrage exit options because they can shift between capital markets (equity, debt, M&A advisory) with minimal friction. Low extraction experienced from their position — they are the primary beneficiary.
constraint_indexing:constraint_classification(underwriter_syndication_power, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: MARKET STRUCTURE / ECOSYSTEM DYNAMICS (TANGLED ROPE) — Viewed as an evolving powerful agent, capital market structure coordinates a genuine function (capital allocation) while extracting through concentration of underwriter power. Over generations, new participants (direct listings, SPACs, private credit platforms) enter with mobile exit options, gradually reducing the lead underwriter's bottleneck power. The ecosystem experiences both coordination (issuers get funded, investors get access) and extraction (rents persist because new entrants take time to scale). Medium-high extraction because the ecosystem is moving but slowly.
constraint_indexing:constraint_classification(underwriter_syndication_power, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: UNDERWRITER ASSOCIATION & REGULATORY FRAMEWORK (PITON) — The formal structures governing syndication (FINRA rules, bank consortium arrangements, historical lead-following relationships) are partially theater. Underwriter associations and regulatory bodies maintain ceremonies around 'fair allocation' and 'proper process' while the real allocation (who gets the scarce early shares) is determined by bilateral relationships and implicit future business expectations. Institutional inertia persists: the rituals of roadshows, pricing committees, and allocation meetings continue because they legitimate the outcomes, not because they determine them. Theater ratio high (0.65+) because formal processes have diminished functional role as relationships and reputation dominate.
constraint_indexing:constraint_classification(underwriter_syndication_power, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURALIZATION VIEW (MOUNTAIN) — From a civilizational perspective, information asymmetry between issuers and capital markets is sometimes naturalized as an immutable feature ('capital markets always require intermediaries'; 'information gaps are inherent'). This framing treats the syndication power structure as an unchangeable consequence of market architecture. However, the structural data contradicts this: direct listings, PIPE structures, and private credit alternatives are not laws of nature but contingent institutional arrangements. The false summit classification reveals how financial system contingencies are naturalized as immutable.
constraint_indexing:constraint_classification(underwriter_syndication_power, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(underwriter_syndication_power_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(underwriter_syndication_power, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(underwriter_syndication_power, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(underwriter_syndication_power, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(underwriter_syndication_power, TR),
    TR >= 0.70.

:- end_tests(underwriter_syndication_power_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. The lead underwriter captures value through multiple channels: spread revenue (typically 3-7% of offering size), allocation discretion (choosing which investors receive shares in hot offerings, generating future business flow expectations), and implicit research coverage bundling (analyst coverage linked to underwriting relationships). The value is not as high as pure extraction (0.70+) because some spread reflects genuine coordination services (roadshow management, due diligence, risk management, investor matching). The victim (issuers) have no viable exit, but the extraction is not maximal because competitive pressure from alternative capital paths (PIPE structures, direct listings increasingly used since 2020) provides some constraint. Suppression (0.68): High. Issuers face severe barriers to exit: need capital to fund growth, private equity offers worse terms (equity loss, control loss), bank loans offer lower growth capital, retained earnings are slow. Institutional investors face allocation-dependent suppression: losing underwriter access means missing hot offerings, which affects mandate performance. Theater ratio (0.45): Moderate-low. The syndication process has genuine functional components (matching issuers to appropriate investor base, managing information flow, risk underwriting) alongside performative components (allocation committees, pricing meetings, formal due diligence). Theater has not collapsed because formal processes legitimize outcomes to regulators and issuers, but the functional content is real — this is not pure ceremony like some regulatory rituals.
 *
 * PERSPECTIVAL GAP:
 *   This constraint illustrates how the same structural mechanism (syndication control) appears as three different types depending on the observer's position: snare for the trapped issuer, tangled rope for the constrained institutional investor, and rope for the positioned underwriter. The analytical observer risks naturalizing the underwriter's coordination role as immutable ('capital markets always need intermediaries') while missing the contingency revealed by alternative capital paths. The piton perspective (regulatory framework as theater) reveals that formal processes legitimize outcomes they no longer functionally determine — the real allocation happens through relationships and reputation, not through the pricing committees and syndicate agreements. This perspective gap is diagnostic: if all perspectives produced the same classification, the constraint would be a natural law or pure rope. The fact that victims, beneficiaries, and observers disagree on type reveals the constraint's true nature as hybrid coordination-extraction (tangled rope).
 *
 * DIRECTIONALITY LOGIC:
 *   The lead underwriter is the primary beneficiary: they control pricing (spread revenue), allocation (future business flow from favored investors), and bundled services (research coverage). From their position, d ≈ 0.10 (beneficiary with arbitrage options), producing low or negative chi. The issuer is the primary victim: they pay spreads, receive allocation discretion, and implicitly support research coverage. From their position, d ≈ 0.92 (trapped victim with no exit), producing high chi. The institutional investor is mixed: they benefit from allocation (early access to growth companies) but pay through future business expectations and mandate expansion. From their position, d ≈ 0.58 (mixed beneficiary-victim with constrained exit), producing moderate chi. The ecosystem (as a powerful actor) experiences d ≈ 0.65 (partly victim to extraction, partly beneficiary from capital allocation coordination), producing moderate-high chi. These derived d values reflect that the constraint's extractiveness is concentrated on trapped actors (issuers) while distributed across constrained actors (investors) and benefits concentrated at beneficiary with arbitrage options (underwriter).
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLVED MANDATROPHY: The constraint's claimed type (tangled rope) is validated by the structural data. The lead underwriter coordinates capital allocation (genuine coordination function) while extracting rents through pricing power, allocation discretion, and bundling (asymmetric extraction). Both functions are real and necessary for the classification: remove the coordination function (investors don't get matched to appropriate capital sources) and the constraint becomes pure extraction (snare). Remove the extraction function (issuers get fair pricing and transparent allocation) and the constraint becomes pure coordination (rope). The constraint persists because both functions are embedded in syndication structure. The mandatrophy resolution strategy is to track which function (coordination vs extraction) is primary and whether alternatives can provide coordination with lower extraction. Direct listings and PIPE structures provide partial evidence: they offer lower extraction (no lead underwriter spread) with lower coordination (more limited investor base, less analyst coverage). This implies that some extraction premium currently goes to legitimate coordination costs, some to rents. The measurement trend (extractiveness 0.42 → 0.62, theater 0.38 → 0.45) suggests rents are increasing while coordination costs remain stable — indicating the constraint is degrading from hybrid toward pure extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    syndicate_power_concentration_threshold,
    'At what level of lead underwriter market concentration does syndication power become systemic extraction rather than legitimate coordination rent?',
    'Time series analysis of spreads vs market concentration; correlation between underwriter HHI and issuers'' cost of capital; event studies of new entrants'' pricing impact',
    'If concentration is primary driver: extraction is institutional (can be regulated). If relationship lock-in dominates: extraction is relational (harder to regulate).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(syndicate_power_concentration_threshold, empirical, 'Concentration threshold for syndication extraction').

omega_variable(
    alternative_capital_path_credibility,
    'Do alternative capital-raising paths (direct listings, private credit, venture platforms) meaningfully constrain lead underwriter pricing power, or are they substitutes that don''t compete on the same offerings?',
    'Analysis of which issuers use alternatives vs traditional syndication; pricing comparison across channels; issuer exit rate to alternatives as function of underwriter spreads',
    'If credible alternatives exist: syndication power is constrained (lower extraction). If alternatives serve different markets: syndication power persists (higher extraction).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(alternative_capital_path_credibility, empirical, 'Whether alternative capital paths constrain underwriter power').

omega_variable(
    allocation_discretion_as_extraction_mechanism,
    'Is underwriter allocation discretion (choosing which investors get scarce shares in hot offerings) a fair coordination reward or a hidden extraction tax on issuers?',
    'Tracking allocation outcomes across offerings: do favored investors systematically out-allocate others? Price discovery analysis: do allocated shares appreciate more than secondary shares, indicating allocation mattered to investor value? Quid pro quo analysis: do investors receiving favorable allocations subsequently send other business to the underwriter?',
    'If allocation is coordination reward: it justifies spread extraction. If allocation is hidden tax: total extraction is understated in the current 0.58 value.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(allocation_discretion_as_extraction_mechanism, empirical, 'Whether allocation discretion constitutes hidden extraction').

omega_variable(
    network_effects_vs_path_dependence,
    'Does the lead underwriter''s network effect (investors want access to their offerings) create efficient coordination or lock-in that persists despite alternatives?',
    'Analysis of investor switching costs: how many issuers switch underwriters after one offering? What triggers switching? Do switching issuers achieve lower spreads with alternative underwriters?',
    'If network effects are efficient: syndication structure is justified (Rope classification strengthens). If path dependence dominates: extraction persists despite inefficiency (Snare for issuers more accurate).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(network_effects_vs_path_dependence, empirical, 'Network effects vs path dependence in syndication').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(underwriter_syndication_power, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(usyn_tr_t0, underwriter_syndication_power, theater_ratio, 0, 0.38).
narrative_ontology:measurement(usyn_tr_t10, underwriter_syndication_power, theater_ratio, 10, 0.42).
narrative_ontology:measurement(usyn_tr_t20, underwriter_syndication_power, theater_ratio, 20, 0.45).
narrative_ontology:measurement(usyn_tr_t30, underwriter_syndication_power, theater_ratio, 30, 0.48).

% Extraction over time
narrative_ontology:measurement(usyn_be_t0, underwriter_syndication_power, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(usyn_be_t10, underwriter_syndication_power, base_extractiveness, 10, 0.5).
narrative_ontology:measurement(usyn_be_t20, underwriter_syndication_power, base_extractiveness, 20, 0.58).
narrative_ontology:measurement(usyn_be_t30, underwriter_syndication_power, base_extractiveness, 30, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(underwriter_syndication_power, resource_allocation).
narrative_ontology:affects_constraint(underwriter_syndication_power, ipo_access_inequality).
narrative_ontology:affects_constraint(underwriter_syndication_power, research_analyst_conflicts).
narrative_ontology:affects_constraint(underwriter_syndication_power, investment_banking_rent_extraction).

% DUAL FORMULATION NOTE:
% Underwriter syndication power is the primary constraint; it affects downstream constraints (IPO access inequality through allocation discretion, research analyst conflicts through bundling, banking rent extraction through spread concentration). This story focuses on the coordination-extraction hybrid; the downstream stories decompose specific extraction mechanisms (allocation as discriminatory, research as biased, spreads as predatory).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
