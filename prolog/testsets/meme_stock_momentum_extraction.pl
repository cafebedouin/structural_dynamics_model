% ============================================================================
% CONSTRAINT STORY: meme_stock_momentum_extraction
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_meme_stock_momentum_extraction, []).

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
 *   constraint_id: meme_stock_momentum_extraction
 *   human_readable: Meme Stock Momentum Extraction
 *   domain: financial_markets/retail_investor_dynamics
 *
 * SUMMARY:
 *   Meme stock momentum extraction represents a contemporary constraint that
 *   reveals the structure of how collective retail action against
 *   institutional positioning can simultaneously generate genuine
 *   coordination benefits and severe asymmetric extraction. The constraint
 *   emerges when early-wave retail coordinators mobilize dispersed small
 *   investors to challenge institutional short positions, creating powerful
 *   collective action that drives prices up and extracts shorts from
 *   profitable positions. However, the momentum mechanism itself — the
 *   positive feedback loop of retail buying, price appreciation, and social
 *   proof — contains an embedded extraction mechanism: as momentum depletes
 *   (shorts close, price support evaporates, institutional algorithms detect
 *   liquidity windows), late-wave retail investors face maximum extraction.
 *   The constraint is classified as a snare from the perspective of late-wave
 *   retail participants because they face high suppression (social proof,
 *   FOMO narratives, momentum signals), no meaningful exit options (selling
 *   at losses triggers regret and community backlash), and maximum extraction
 *   (capital drain as momentum depletes). Early coordinators and
 *   institutional players experience the same structural mechanism as rope or
 *   arbitrage — they have exit options, information advantages, and clear
 *   profits. The constraint exhibits high extractiveness (0.68) because the
 *   capital transfer from late-wave retail to institutional shorters and
 *   early coordinators is systematic and severe. Suppression is high (0.62)
 *   because retail participation is sustained through social media
 *   narratives, community identity, FOMO cascades, and momentum signals that
 *   obscure exit timing. Theater ratio is moderate-to-high (0.58) because
 *   retail communities perform coordination rituals (Diamond Hand
 *   affirmations, Rocket emojis, community validation) that have genuine
 *   coordination function but increasingly serve entertainment and identity
 *   purposes as the momentum window closes.
 *
 * KEY AGENTS:
 *   - Late-Wave Retail Investors: Primary victims (powerless/trapped) — enter momentum peak with high conviction but lowest information; face maximum extraction and minimal exit options
 *   - Mid-Wave Coordinated Participants: Secondary victims (moderate/constrained) — participate in genuine retail coordination; experience both coordination benefits and extraction from early coordinators
 *   - Early Meme Coordinators: Primary beneficiaries (organized/arbitrage) — initiate and amplify retail coordination; capture timing advantage and exit before maximum extraction
 *   - Institutional Shorters: Extraction agents (institutional/arbitrage) — initially targeted by retail coordination but capture extraction during depletion phase; profit from momentum collapse
 *   - Trading Platforms: Secondary beneficiaries (institutional/arbitrage) — capture order flow monetization, volatility premiums, and transaction fees across entire momentum cycle
 *   - Regulatory Framework: Institutional observer (analytical/analytical) — securities regulations designed for institutional markets; degraded functional capacity to prevent retail momentum extraction
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(meme_stock_momentum_extraction, 0.68).
domain_priors:suppression_score(meme_stock_momentum_extraction, 0.62).
domain_priors:theater_ratio(meme_stock_momentum_extraction, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(meme_stock_momentum_extraction, extractiveness, 0.68).
narrative_ontology:constraint_metric(meme_stock_momentum_extraction, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(meme_stock_momentum_extraction, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(meme_stock_momentum_extraction, snare).
narrative_ontology:human_readable(meme_stock_momentum_extraction, "Meme Stock Momentum Extraction").
narrative_ontology:topic_domain(meme_stock_momentum_extraction, "financial_markets/retail_investor_dynamics").

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(meme_stock_momentum_extraction, institutional_shorters).
narrative_ontology:constraint_beneficiary(meme_stock_momentum_extraction, early_meme_coordinators).
narrative_ontology:constraint_beneficiary(meme_stock_momentum_extraction, trading_platforms).
narrative_ontology:constraint_victim(meme_stock_momentum_extraction, late_wave_retail_investors).
narrative_ontology:constraint_victim(meme_stock_momentum_extraction, long_term_bagholders).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: LATE-WAVE RETAIL INVESTOR (SNARE) — Emotionally and informationally entrapped by social proof, FOMO narratives, and the momentum signal itself. Faces maximum extraction with no meaningful exit — selling at a loss triggers regret, holding traps them in declining value. The constraint extracts capital through coordinated momentum depletion once early coordinators and institutional players exit.
constraint_indexing:constraint_classification(meme_stock_momentum_extraction, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: MID-WAVE COORDINATED PARTICIPANT (TANGLED ROPE) — Participates in genuine coordination (information sharing, collective action against institutional shorting) while also experiencing extraction from early coordinators and institutional players. Has some agency and modest exit options (can leave before maximum extraction) but faces social pressure and sunk-cost effects. Experiences both coordination benefits and asymmetric extraction.
constraint_indexing:constraint_classification(meme_stock_momentum_extraction, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: EARLY MEME COORDINATOR (ROPE) — Initiates genuine coordination of retail investors against institutional shorting. Benefits from first-mover information asymmetry and momentum amplification. Experiences the constraint as pure coordination: organizing retail action solves a collective-action problem. Low effective extraction because coordinators have exit options (stop posting, sell early) and clear coordination function (mobilizing retail capital).
constraint_indexing:constraint_classification(meme_stock_momentum_extraction, rope,
    context(agent_power(organized),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: INSTITUTIONAL SHORTER / PLATFORM OPERATOR (SNARE FROM INSTITUTIONAL VIEW) — Institutional actors (hedge funds with short positions, trading platforms profiting from volatility and order flow) extract through momentum capture. They have perfect exit options (close positions, route orders to highest bidder) and extract maximum value during the momentum window. From their perspective the constraint is a mechanism for asymmetric gain — retail retail provides liquidity for institutional extraction.
constraint_indexing:constraint_classification(meme_stock_momentum_extraction, snare,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: REGULATORY FRAMEWORK (PITON) — Securities regulations were designed for institutional markets with information asymmetries flowing through broker-dealers and research channels. The meme stock mechanism (social media coordination, retail order flow concentration, retail collateral cascades) operates in this regulatory framework but its primary function (protecting retail from institutional extraction) is heavily degraded. The regulations persist through institutional inertia despite low functional capacity to prevent retail momentum extraction. Theater ratio is high: compliance theater continues while the extraction mechanism evolves.
constraint_indexing:constraint_classification(meme_stock_momentum_extraction, piton,
    context(agent_power(analytical),
            time_horizon(generational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (TANGLED ROPE) — From a full analytical view, the meme stock constraint combines genuine coordination (retail collective action against institutional shorting) with systematic extraction (momentum depletion, information cascades, volatility capture). The constraint solves a real coordination problem (mobilizing dispersed retail capital) while simultaneously enabling asymmetric extraction by actors with superior exit options and information. This is a canonical tangled rope: both coordination and extraction are essential to understanding the mechanism.
constraint_indexing:constraint_classification(meme_stock_momentum_extraction, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(meme_stock_momentum_extraction_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(meme_stock_momentum_extraction, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(meme_stock_momentum_extraction, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(meme_stock_momentum_extraction, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(meme_stock_momentum_extraction, TR),
    TR >= 0.70.

:- end_tests(meme_stock_momentum_extraction_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High and increasing. The constraint starts at low extractiveness (0.18) during early retail mobilization when coordination benefits dominate and extraction is minimal. As the momentum window extends, extraction accelerates through three mechanisms: (1) momentum depletion — late-wave retail enters at peak prices with no liquidity support at lower prices; (2) cascade entrapment — social proof and community identity increase psychological holding costs, reducing exit behavior; (3) institutional extraction — shorters who closed early positions re-enter as volatility extractors, capturing remaining capital through directional trades and option strikes. By the measurement endpoint (0.68), extraction is severe and systematic. Suppression (0.62): Moderate-to-high and stable. Suppression mechanisms include social media echo chambers that reinforce bullish narratives, momentum signals that activate FOMO, community backlash against 'paperhand' sellers, and sunk-cost effects that increase holding beyond rational decision points. These are not structural barriers (like capital controls or legal restrictions) but internalized and social barriers that are highly effective. Theater ratio (0.58): Moderate and increasing. Early retail coordination has genuine coordination function (sharing analysis, organizing against shorters, collective action). As momentum window extends, theatrical performance increases: community rituals (Diamond Hands, Rocket emojis) serve identity purposes more than information transmission, and retail conviction becomes increasingly disconnected from underlying fundamentals. The theater is not primarily regulatory theater (like the piton perspective's degraded review ritual) but social theater — community identity performance that sustains participation even as information asymmetry increases.
 *
 * PERSPECTIVAL GAP:
 *   The analytical observer sees what individual participants cannot: that the constraint is fundamentally a tangled rope (mixed coordination and extraction) because it solves a real coordination problem (mobilizing retail capital against institutional positioning) while simultaneously implementing an extraction mechanism (momentum depletion targeting late arrivals). Early participants mistake it for pure rope because they exit before extraction hits maximum. Late participants mistake it for a snare because they enter after coordination benefits have depleted. Institutional participants see it as arbitrage because they control the extraction timing. The gap between perspectives reveals the constraint's true structure: it is neither pure coordination nor pure extraction but a hybrid that changes its character depending on entry timing. This is diagnostically significant — many financial constraints labeled as 'predatory' or 'manipulative' are actually tangled ropes that participants experience as snares because they enter at the wrong point in the cycle.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values are derived from each agent's structural position in the extraction flow. Late-wave retail investors are maximum victims with no exit options: d ≈ 0.95 → f(d) ≈ 1.42 → high experienced extraction. Early coordinators are beneficiaries with arbitrage exit (can stop posting, sell before peak): d ≈ 0.15 → f(d) ≈ 0.00 → low effective extraction experienced by this agent. Institutional shorters are beneficiaries with perfect exit options (close positions instantly): d ≈ 0.05 → f(d) ≈ -0.12 → negative effective extraction (they profit from the constraint). Mid-wave participants are mixed: they benefit from coordination but face extraction from early players and institutional competition; d ≈ 0.55 → f(d) ≈ 0.75 → moderate experienced extraction. The spatial scope is global (1.2 modifier) because the constraint operates across geographies — retail on social media platforms, institutional players across global markets. The scope amplifies extractiveness: χ = 0.68 × f(d) × 1.2 produces high chi for victims, confirming snare classification.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by recognizing that meme stock momentum extraction is a genuine snare for late-wave retail investors (high suppression, no exit options, maximum extraction) but NOT a false natural law or simple mislabeling of coordination. The constraint has real coordination function: it mobilizes retail capital and challenges institutional shorting positions effectively. But the coordination function is inseparable from the extraction mechanism. The early coordinators and institutional players do not experience the constraint as snare because they have exit options and information advantages. The constraint is genuinely mixed (tangled rope from analytical view) and genuinely predatory (snare from late-wave participant view) simultaneously. The resolution is to classify from the specific perspective rather than seeking a single 'true' type: the snare classification is correct for powerless/trapped agents; the rope classification is correct for organized/arbitrage coordinators; the tangled rope classification is correct for the analytical observer. The mandatrophy fails precisely when regulators try to regulate 'meme stocks' as a single constraint type rather than recognizing that different participants experience different constraints.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    momentum_depletion_mechanism,
    'Is momentum depletion a structural feature of retail coordination at scale, or a contingent outcome of specific institutional responses?',
    'Comparative analysis of meme stock episodes with and without institutional shorting responses; simulation of pure-retail momentum in absence of short-seller counter-positioning',
    'If structural: meme stock extraction is inherent to retail coordination dynamics (higher extractiveness). If contingent: extraction depends on institutional counter-play (lower inherent extractiveness, higher contingency).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(momentum_depletion_mechanism, empirical, 'Whether momentum depletion is structural or contingent on institutional response').

omega_variable(
    information_asymmetry_source,
    'Do early meme coordinators accumulate unfair information advantages (about sentiment trends, cascade timing, exit windows) or simply achieve first-mover timing advantage available to any participant?',
    'Network analysis of information flow in meme stock communities; comparison of early vs late adopter information content and timing of trade signals',
    'If unfair advantage: early coordinators are extracting through information monopoly (higher snare classification for early coordinators). If timing only: extraction is purely momentum-driven (rope classification for early coordinators is more accurate).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(information_asymmetry_source, empirical, 'Source of early coordinator advantage: information asymmetry vs timing').

omega_variable(
    social_proof_entrapment_intensity,
    'What proportion of late-wave retail retention is rational sunk-cost acceptance vs internalized social commitment to the meme identity?',
    'Behavioral analysis of exit timing; correlation between community participation level and holding duration beyond peak; identity fusion measurement in exit interviews',
    'If primarily rational: suppression is moderate, exit options are constrained but available (trap classification softer). If primarily identity-fusion: suppression is severe, exit options become identity_locked (trap classification stronger).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(social_proof_entrapment_intensity, empirical, 'Role of identity fusion vs rational sunk-cost in late-wave holding').

omega_variable(
    volatility_extraction_attribution,
    'How much of retail extraction occurs through volatility capture by institutional traders vs direct momentum depletion through strategic shorting?',
    'Decomposition of institutional profit sources in meme stock episodes; attribution of gains to directional shorts, volatility trading, and order flow monetization',
    'If volatility-dominant: institutional extraction is less targeted (lower perceived malice, but same amount of capital transfer). If short-dominant: extraction is adversarial positioning (higher perceived unfairness).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(volatility_extraction_attribution, empirical, 'Institutional extraction attribution: volatility vs short-directed').

omega_variable(
    collective_action_sustainability,
    'Can retail meme stock coordination sustain itself as a durable institutional form, or does it require periodic external shocks (short-seller escalation, regulatory announcement) to maintain momentum and community cohesion?',
    'Longitudinal analysis of meme stock communities post-peak; measurement of coordination stability in low-volatility regimes; survey of coordinator motivation persistence',
    'If sustainable: meme stocks represent a new institutional actor class (reevaluate from organized/powerful perspective). If shock-dependent: meme stocks are cyclical phenomena (remains snare for most participants).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(collective_action_sustainability, empirical, 'Sustainability of retail meme stock coordination as institutional form').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(meme_stock_momentum_extraction, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(meme_tr_t0, meme_stock_momentum_extraction, theater_ratio, 0, 0.35).
narrative_ontology:measurement(meme_tr_t2, meme_stock_momentum_extraction, theater_ratio, 2, 0.42).
narrative_ontology:measurement(meme_tr_t4, meme_stock_momentum_extraction, theater_ratio, 4, 0.52).
narrative_ontology:measurement(meme_tr_t6, meme_stock_momentum_extraction, theater_ratio, 6, 0.58).

% Extraction over time
narrative_ontology:measurement(meme_be_t0, meme_stock_momentum_extraction, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(meme_be_t2, meme_stock_momentum_extraction, base_extractiveness, 2, 0.35).
narrative_ontology:measurement(meme_be_t4, meme_stock_momentum_extraction, base_extractiveness, 4, 0.52).
narrative_ontology:measurement(meme_be_t6, meme_stock_momentum_extraction, base_extractiveness, 6, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(meme_stock_momentum_extraction, attachment_coordination).
narrative_ontology:affects_constraint(meme_stock_momentum_extraction, short_squeeze_institutional_cascade).
narrative_ontology:affects_constraint(meme_stock_momentum_extraction, retail_investor_momentum_bias).
narrative_ontology:affects_constraint(meme_stock_momentum_extraction, volatility_extraction_mechanisms).

% DUAL FORMULATION NOTE:
% Meme stock momentum extraction operates as both a genuine coordination mechanism (early retail mobilization against institutional shorting) and an extraction mechanism (momentum depletion capturing from late arrivals). The constraint family decomposes into three structurally distinct stories: (1) retail_coordination_against_shorts (ε ≈ 0.20, Rope) — the genuine coordination function of retail mobilization; (2) momentum_depletion_dynamics (ε ≈ 0.55, Tangled Rope) — the mixed coordination-extraction hybrid; (3) institutional_volatility_extraction (ε ≈ 0.42, Snare) — the extraction mechanism isolated. This story covers the middle term (momentum dynamics) where all three mechanisms are visible. The upstream story is retail coordination; the downstream story is institutional extraction.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(meme_stock_momentum_extraction, institutional, 0.05).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
