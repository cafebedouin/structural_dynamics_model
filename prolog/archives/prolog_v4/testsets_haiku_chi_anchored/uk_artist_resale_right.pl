% ============================================================================
% CONSTRAINT STORY: uk_artist_resale_right
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
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
 *   The UK Artist's Resale Right (ARR), enacted in 2006 as part of EU
 *   Directive 2006/115/EC implementation, mandates that original artists (or
 *   their heirs) receive a royalty payment when their artwork is resold by an
 *   art market professional. The royalty rate is typically 0.5–4% of the
 *   resale price, with a cap on total royalties per transaction. ARR
 *   exemplifies a hybrid coordination-extraction mechanism: it protects
 *   artists' interests (coordination function) while imposing compliance
 *   costs on art dealers and potentially reducing market liquidity
 *   (extraction effect). The constraint exhibits different classification
 *   profiles from different perspectives: artists see pure extraction
 *   (inability to control resale value); dealers see mixed
 *   coordination-extraction (legal clarity balanced against royalty
 *   remittance); the market ecosystem sees coordination (legitimacy and
 *   reduced litigation); advocacy coalitions see degraded implementation
 *   (theater); international dealers see either snare (constrained exit) or
 *   scaffold (with potential sunset via post-Brexit harmonization). Theater
 *   has increased over time (2006→2024) as the implementation gap widens: ARR
 *   is collected and remitted as a formal mechanism, but actual artist
 *   benefit is often negligible (most eligible works fall below payment
 *   thresholds, and high-value sales migrate to private channels to avoid
 *   triggers). The post-Brexit regulatory divergence between UK and EU
 *   frameworks adds pressure for either convergence (scaffold logic) or
 *   fragmentation (snare intensification).
 *
 * KEY AGENTS:
 *   - Original Artists & Artist Estates: Primary beneficiaries (institutional/powerless depending on sales history) — entitled to ARR royalties but have no control over resale decisions or prices
 *   - Art Market Professionals (Auction Houses, Dealers, Galleries): Institutional compliance agents (moderate/constrained) — required to identify eligible artworks, calculate royalties, collect and remit payments
 *   - Secondary Market Liquidity: Abstract victim (powerless/trapped) — reduced transaction velocity and deal flow due to ARR compliance costs and price transparency
 *   - Artist Advocacy Coalition (Artist Unions, Professional Associations): Organized beneficiary (organized/constrained) — achieved ARR policy victory post-2006 but sees implementation degraded by threshold effects and evasion
 *   - International Art Dealers (EU/Non-UK based): Secondary actors (moderate/mobile) — can exit ARR constraint by conducting transactions outside UK jurisdiction, but at market access cost
 *   - UK Art Market Ecosystem: Institutional framework (institutional/arbitrage) — benefits from ARR's legitimacy and legal clarity despite royalty remittance costs
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing ARR as inherent protection when it is contingent legal construct with significant implementation gaps
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(uk_artist_resale_right, 0.38).
domain_priors:suppression_score(uk_artist_resale_right, 0.48).
domain_priors:theater_ratio(uk_artist_resale_right, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(uk_artist_resale_right, extractiveness, 0.38).
narrative_ontology:constraint_metric(uk_artist_resale_right, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(uk_artist_resale_right, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(uk_artist_resale_right, tangled_rope).
narrative_ontology:human_readable(uk_artist_resale_right, "UK Artist's Resale Right (ARR) Legislation").
narrative_ontology:topic_domain(uk_artist_resale_right, "economic/legal").

domain_priors:requires_active_enforcement(uk_artist_resale_right).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(uk_artist_resale_right, original_artists).
narrative_ontology:constraint_beneficiary(uk_artist_resale_right, artist_estates).
narrative_ontology:constraint_victim(uk_artist_resale_right, art_market_professionals).
narrative_ontology:constraint_victim(uk_artist_resale_right, secondary_market_liquidity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: LIVING ARTIST POST-SALE (SNARE) — Trapped by illiquidity of their own work. Cannot exit the constraint; extractive mechanism (inability to participate in resales) persists across their lifetime. Artist has no capacity to renegotiate terms or recover value. d≈0.95, f(d)≈1.42, σ=1.0 → χ≈0.54. The ARR appears as compensation, but the constraint's suppression (0.48) reflects that even with ARR, artists lack agency in how their work appreciates and have no control over resale prices.
constraint_indexing:constraint_classification(uk_artist_resale_right, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: ART MARKET PROFESSIONAL (TANGLED ROPE) — Constrained by mandatory ARR collection/remittance but also benefits from the legitimacy and market confidence that ARR enforcement provides (reduced litigation risk, market transparency). Professional must collect and remit but gains coordination benefit (standardized, legally clear resale mechanism). d≈0.62, f(d)≈0.87, σ=1.0 → χ≈0.33. Tangled rope because the mechanism both extracts (compliance cost, royalty remittance) and coordinates (stable legal framework for resales).
constraint_indexing:constraint_classification(uk_artist_resale_right, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: UK ART MARKET ECOSYSTEM (ROPE) — Institutional view sees ARR as a coordination mechanism that stabilizes the market by protecting artists' interests and reducing legal uncertainty around resales. Market professionals accept the royalty as a cost of doing business in a regulated ecosystem. The ecosystem benefits from ARR's legitimacy even though individual transactions bear the cost. d≈0.15, f(d)≈-0.01, σ=1.0 → χ≈-0.00. Net beneficiary from coordination function.
constraint_indexing:constraint_classification(uk_artist_resale_right, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: ARTIST ADVOCACY COALITION (PITON) — Organized advocacy (artist unions, professional associations) initially achieved ARR as a major policy victory (post-2006). However, the implementation has become largely performative: artists rarely receive meaningful income from ARR (most eligible works fall below the 0.5–1.0% threshold; high-value resales are often handled privately to avoid ARR triggers). Theater_ratio=0.55 reflects that ARR collection and remittance occur, but actual artist benefit is theatrical relative to the administrative burden. d≈0.35, f(d)≈0.28, σ=1.0 → χ≈0.15. Low effective extraction because the organized coalition has achieved formal policy victory but sees its mechanism as degraded in practice.
constraint_indexing:constraint_classification(uk_artist_resale_right, piton,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: INTERNATIONAL ART DEALER (SNARE with mobile exit) — Dealers based in EU/non-UK jurisdictions can exit the ARR constraint by conducting resales outside UK legal jurisdiction, but only at the cost of losing access to UK market liquidity. This creates a genuine mobile exit (dealers can move transactions to London competitors or EU platforms), but the cost is high enough (market access) that many remain trapped. d≈0.72, f(d)≈1.15, σ=1.1 → χ≈0.48. Snare classification because exit is theoretically available but practically constrained by market geography.
constraint_indexing:constraint_classification(uk_artist_resale_right, snare,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(continental))).

% PERSPECTIVE 6: EU ARR FRAMEWORK (SCAFFOLD with built-in sunset logic) — EU Directive 2006/115/EC (the basis for UK ARR) includes a 'droit de suite' framework that the UK adopted then adapted post-Brexit. This perspective sees ARR as a temporary coordination mechanism with an implicit sunset: EU and UK frameworks are diverging post-Brexit, creating pressure for mutual recognition treaties or harmonization. If UK aligns with EU or creates reciprocal frameworks, the extractive friction disappears and ARR becomes pure coordination (Rope). If divergence continues, ARR becomes increasingly extractive (Snare). d≈0.45, f(d)≈0.48, σ=1.1 → χ≈0.25. Low effective extraction because organized actors (international galleries, auction houses) have sufficient agency to negotiate framework harmonization.
constraint_indexing:constraint_classification(uk_artist_resale_right, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (MOUNTAIN — FALSE SUMMIT) — This perspective risks framing ARR as an immutable law of art markets: 'artists must be compensated for resales because their labor is exploited by secondary market markup.' However, the structural data (ε=0.38, suppression=0.48, theater=0.55) contradicts a mountain classification. ARR is a contingent legal construct, not a natural law. The false summit reveals that the 'fairness argument' naturalizes what is actually a hybrid coordination-extraction mechanism whose costs and benefits are unevenly distributed by market geography and artist prominence.
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
    constraint_indexing:constraint_classification(uk_artist_resale_right, TypeOther, context(agent_power(moderate), _, _, _)),
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
 *   Extractiveness (0.38): Moderate. ARR does extract real costs from dealers (royalty collection/remittance, compliance, reduced transaction certainty), but the extraction is not as severe as a pure snare (0.66+) because: (1) dealers can price ARR costs into their sales; (2) the royalty is meant to flow to artists (beneficiaries), not to a third-party extractor; (3) organized dealers have collective bargaining power over implementation thresholds. The 0.38 reflects that the mechanism extracts from the market but with genuine coordination benefit (legal certainty). Suppression (0.48): Moderate. Art market professionals cannot fully exit ARR (UK jurisdiction applies to all registered resales), but suppression is not severe (0.60+) because: (1) international dealers retain the option of relocating transactions; (2) high-value sales migrate to private channels, reducing effective ARR impact; (3) thresholds create legitimate carve-outs for lower-value works. Suppression captures the real constraints (legal obligation, market friction) without claiming total closure. Theater ratio (0.55): Moderate-high. ARR is implemented as a formal mechanism (royalties are calculated and collected), but the function is increasingly theatrical: most artists receive negligible income from ARR (threshold effects exclude majority of eligible works), high-value resales evade ARR through private sales, and the administrative overhead (compliance, tracking, remittance) exceeds actual artist benefit for many works. Theater has risen from ~0.35 (2006, when ARR was novel and genuinely protective) to 0.55 (2024, as evasion strategies matured and threshold insufficiencies became apparent). The theater ratio indicates Goodhart drift: the formal mechanism (royalty remittance) persists, but the real function (protecting artist interests) has been partially substituted by evasion.
 *
 * PERSPECTIVAL GAP:
 *   ARR demonstrates sharp perspectival divergence across power levels and exit options. Artists and estates (powerless/trapped) see a snare: they cannot participate in their own work's value appreciation and the constraint persists across their lifetime. Dealers (moderate/constrained) see tangled rope: they must comply but also benefit from legal clarity. The UK art market (institutional/arbitrage) sees rope: legitimacy from artist protection is a coordination benefit worth the royalty cost. International dealers (moderate/mobile) see snare with exit: they are constrained by UK jurisdiction but can relocate transactions. Advocacy coalitions (organized/constrained) see piton: they achieved policy victory but see the mechanism as degraded in practice. The analytical observer risks seeing a natural law (mountain) that artists must be compensated, but the structural data reveals this as a false summit: ARR is contingent, unevenly protective, and partially evaded. The perspectival gaps reveal that ARR's extraction burden is borne by the market (dealers, liquidity) while its benefits accrue unevenly to artists (high-value creators benefit; low-value creators receive negligible income). The mechanism is not unjust coordination or pure extraction, but a hybrid constrained by enforcement gaps and evasion strategies.
 *
 * DIRECTIONALITY LOGIC:
 *   Original artists/estates: Beneficiary + trapped → d≈0.95, f(d)≈1.42. Maximum extraction in formal structure (they cannot exit), but actual benefit is partial (theater_ratio=0.55 indicates meaningful leakage). Art market professionals: Victim + constrained → d≈0.62, f(d)≈0.87. They bear compliance costs and royalty remittance but also benefit from market legitimacy. Exit is theoretically available (relocate to non-ARR jurisdiction) but practically costly (market access loss). Secondary market liquidity: Victim + trapped → d≈0.90, f(d)≈1.38. Cannot exit; bears transaction friction costs. International dealers: Victim + mobile → d≈0.72, f(d)≈1.15. Significant exit option (relocate transactions), but cost is high (market access). Artist advocacy coalition: Beneficiary + constrained → d≈0.35, f(d)≈0.28. Policy victory achieved (beneficiary status), but cannot implement fully (constrained by threshold effects and evasion). UK art market ecosystem: Beneficiary + arbitrage → d≈0.10, f(d)≈-0.05. Institutional beneficiary (legitimacy, legal clarity) with arbitrage exit (can adapt compliance costs into pricing). Analytical observer: Analytical → d≈0.73, f(d)≈1.15. Risks naturalizing contingent mechanism as inherent law.
 *
 * MANDATROPHY ANALYSIS:
 *   Mandatrophy is avoided by clear tangled rope classification. ARR exhibits genuine coordination function (legal clarity, market legitimacy, artist protection intent) AND genuine extraction (compliance costs, reduced liquidity, compliance burden on dealers, theater ratio indicating implementation gap). The mechanism is not pure extraction (a snare would have ε≥0.46, suppression≥0.60, χ≥0.66; ARR has lower metrics because coordination genuinely reduces uncertainty). It is not pure coordination (a rope would have ε≤0.45, suppression low; ARR's suppression=0.48 and extraction=0.38 reflect real market friction). The tangled rope classification correctly captures that ARR solves a coordination problem (how to compensate artists for resale value) while imposing asymmetric extraction (dealers bear compliance costs; artists' actual benefit is partial and unequal by prominence). The theater ratio drift (0.35→0.55) indicates that the coordination function is being eroded over time—the formal mechanism persists, but its actual protective effect is declining due to evasion and threshold effects. This is classic piton degradation (formal maintenance without real function), but ARR remains primarily tangled rope because the enforcement is still active and the coordination benefit is still meaningful (especially for high-value artists). The constraint does not resolve to pure snare because dealers retain (albeit costly) exit options and because the legitimacy/legal clarity function is genuine.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    threshold_enforcement_effect,
    'Does the 0.5% royalty threshold (or lower thresholds in some EU jurisdictions) effectively protect artists, or does it exclude most resales from the mechanism?',
    'Analysis of ARR payment distribution: percentage of eligible resales actually triggering payment, average royalty per artist, correlation between artist prominence and ARR income',
    'If threshold is too high: ARR is largely theatrical (piton). If threshold is appropriate: ARR has genuine coordination function (tangled rope with meaningful transfer).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(threshold_enforcement_effect, empirical, 'Whether ARR thresholds result in meaningful artist compensation').

omega_variable(
    private_sale_evasion_scope,
    'What fraction of eligible resales are conducted privately (avoiding ARR) versus through registered art market professionals?',
    'Market research comparing private vs professional resales; correlation with artwork value and jurisdiction; tracking of off-market transactions',
    'If evasion is widespread (>30%): ARR enforcement is weak (suppression lower than 0.48). If evasion is rare (<10%): ARR suppression is structural and genuine.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(private_sale_evasion_scope, empirical, 'Scale of private-sale evasion of ARR').

omega_variable(
    market_liquidity_drag,
    'Does ARR reduce the secondary market liquidity for mid-range artworks by making resales more costly and uncertain?',
    'Econometric analysis of resale volumes, transaction velocity, and price recovery pre/post-ARR implementation; comparison with non-ARR jurisdictions; survey of dealer behavior changes',
    'If ARR reduces liquidity significantly: extractiveness understated (should be >0.45). If liquidity drag is minimal: tangled rope classification confirmed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(market_liquidity_drag, empirical, 'Impact of ARR on secondary market liquidity').

omega_variable(
    post_brexit_divergence_timeline,
    'Will post-Brexit UK/EU ARR divergence lead to reciprocal treaties, harmonization, or regulatory fragmentation?',
    'Monitoring of trade negotiations, UK/EU regulatory alignment initiatives, international auction house strategy shifts, and dealer jurisdictional choices',
    'If harmonization occurs: scaffold sunset is real, extraction declines. If fragmentation persists: international dealers face increasing compliance costs and may exit UK market.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(post_brexit_divergence_timeline, preference, 'UK/EU post-Brexit ARR regulatory alignment trajectory').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(uk_artist_resale_right, 2006, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(arr_theater_2006, uk_artist_resale_right, theater_ratio, 0, 0.35).
narrative_ontology:measurement(arr_theater_2015, uk_artist_resale_right, theater_ratio, 9, 0.48).
narrative_ontology:measurement(arr_theater_2024, uk_artist_resale_right, theater_ratio, 18, 0.55).

% Extraction over time
narrative_ontology:measurement(arr_extract_2006, uk_artist_resale_right, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(arr_extract_2015, uk_artist_resale_right, base_extractiveness, 9, 0.33).
narrative_ontology:measurement(arr_extract_2024, uk_artist_resale_right, base_extractiveness, 18, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(uk_artist_resale_right, resource_allocation).
narrative_ontology:affects_constraint(uk_artist_resale_right, art_market_price_discovery).
narrative_ontology:affects_constraint(uk_artist_resale_right, artist_income_inequality).

% DUAL FORMULATION NOTE:
% ARR is a single structural constraint with multiple perspectival readings. It is not decomposed into separate constraints because the base ε (0.38) is stable across different observables (calculating ARR on transaction value vs. compliance cost vs. artist income all yield consistent structural metrics). The network links acknowledge that ARR influences both upstream (price discovery mechanisms in secondary markets) and downstream (artist income distribution) constraints, but these are separate structural problems, not alternative measurements of ARR itself.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(uk_artist_resale_right, institutional, 0.1).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
