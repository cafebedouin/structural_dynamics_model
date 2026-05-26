% ============================================================================
% CONSTRAINT STORY: speculative_narrative_volatility
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_speculative_narrative_volatility, []).

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
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_axiom_grounding/3,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: speculative_narrative_volatility
 *   human_readable: Speculative Narrative Volatility in Cryptocurrency Markets
 *   domain: monetary_theory/financial_markets/distributed_systems_governance
 *
 * SUMMARY:
 *   Cryptocurrency markets exhibit a fundamental structural tension between
 *   three competing and partially incompatible interpretive frameworks: (1)
 *   sound money, which requires price stability and treats cryptocurrency as
 *   an inflation hedge and store of value; (2) speculative asset, which
 *   thrives on narrative-driven volatility and treats price discovery as the
 *   primary function; and (3) decentralization ideology, which claims to
 *   disintermediate financial institutions but practically depends on
 *   centralized exchanges and custodial infrastructure. This constraint
 *   manifests as a tangled rope at the institutional and trader level —
 *   genuine coordination of peer-to-peer value transfer coexists with
 *   extraction mechanisms (speculation rent, regulatory arbitrage,
 *   information asymmetry). For retail investors, the constraint operates as
 *   a snare: the sound-money narrative attracts them; volatility extracts
 *   their capital; identity fusion with revolutionary-technology ideology
 *   prevents exit. For the exchange operator and market maker, the constraint
 *   is pure coordination (rope) — they benefit from liquidity and fee
 *   collection without experienced extraction. For the decentralization
 *   governance coalition, it appears as a temporary scaffolding problem with
 *   a sunset clause — alternative infrastructure (DEXs, rollups, AMMs) offers
 *   genuine decentralization pathways that bypass centralized exchange
 *   dependency. The sound-money institutional narrative persists as piton:
 *   the framing is maintained through ideological inertia despite
 *   falsification (volatility contradicts sound-money requirements). From the
 *   civilizational analytical position, the constraint risks appearing as an
 *   immutable natural law of speculation, but the structural data reveals
 *   this as a false summit: volatility is contingent on specific
 *   institutional arrangements (exchange centralization, narrative-driven
 *   valuation, early-adopter information advantages), not inevitable.
 *
 * KEY AGENTS:
 *   - Retail Investors: Primary victim (powerless/trapped) — entered through sound-money narrative; trapped in volatility cycle with behavioral lock-in and identity fusion
 *   - Institutional Traders: Mixed beneficiary-victim (moderate/constrained) — benefit from volatility-driven liquidity but suppressed by regulatory uncertainty and exchange dependency
 *   - Exchange Operators / Market Makers: Primary beneficiary (institutional/arbitrage) — benefit from coordination function (liquidity provision) and rent extraction (transaction fees, spreads)
 *   - Early Adopters / Large Holders: Secondary beneficiary (powerful/mobile) — benefit from appreciation and information advantage but pay regulatory suppression costs
 *   - Decentralization Governance Coalition: Organized agents (organized/constrained) — attempting to build alternative infrastructure (DEXs, rollups) to bypass centralized exchange dependency
 *   - Sound Money Institutional Narrative: Ideological apparatus (institutional/arbitrage) — maintains frame despite incompatibility with volatility; theater ratio reveals degradation
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing contingent institutional arrangements as inevitable market properties
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(speculative_narrative_volatility, 0.58).
domain_priors:suppression_score(speculative_narrative_volatility, 0.65).
domain_priors:theater_ratio(speculative_narrative_volatility, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(speculative_narrative_volatility, extractiveness, 0.58).
narrative_ontology:constraint_metric(speculative_narrative_volatility, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(speculative_narrative_volatility, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(speculative_narrative_volatility, tangled_rope).
narrative_ontology:human_readable(speculative_narrative_volatility, "Speculative Narrative Volatility in Cryptocurrency Markets").
narrative_ontology:topic_domain(speculative_narrative_volatility, "monetary_theory/financial_markets/distributed_systems_governance").

domain_priors:requires_active_enforcement(speculative_narrative_volatility).

% --- Commitment system structure ---
narrative_ontology:cs_kernel_codification(speculative_narrative_volatility, distributed).
narrative_ontology:cs_authority_grounding(speculative_narrative_volatility, distributed).
narrative_ontology:cs_reading_relation(speculative_narrative_volatility, crypto_sound_money_reading, forecloses).
narrative_ontology:cs_reading_relation(speculative_narrative_volatility, crypto_decentralization_reading, coexists_with).
narrative_ontology:cs_reading_relation(speculative_narrative_volatility, crypto_speculative_asset_reading, coexists_with).
narrative_ontology:cs_axiom(speculative_narrative_volatility, foundational, volatility_is_extractive_mechanism).
narrative_ontology:cs_axiom_status(volatility_is_extractive_mechanism, holdable).
narrative_ontology:cs_axiom_grounding(speculative_narrative_volatility, volatility_is_extractive_mechanism, empirically_contingent).
narrative_ontology:cs_axiom(speculative_narrative_volatility, foundational, centralized_exchange_dependency_inevitable).
narrative_ontology:cs_axiom_status(centralized_exchange_dependency_inevitable, holdable).
narrative_ontology:cs_axiom_grounding(speculative_narrative_volatility, centralized_exchange_dependency_inevitable, empirically_contingent).
narrative_ontology:cs_axiom(speculative_narrative_volatility, secondary, narrative_driven_adoption_dominates_fundamentals).
narrative_ontology:cs_axiom_status(narrative_driven_adoption_dominates_fundamentals, holdable).
narrative_ontology:cs_axiom_grounding(speculative_narrative_volatility, narrative_driven_adoption_dominates_fundamentals, empirically_contingent).
narrative_ontology:cs_reference_frame(speculative_narrative_volatility, distributed_speculation_regime).
narrative_ontology:cs_drift_state(speculative_narrative_volatility, contemporary_post_2022_bear_market, gap(axiom_overriding, substantial, false)).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(speculative_narrative_volatility, early_adopters_and_speculators).
narrative_ontology:constraint_beneficiary(speculative_narrative_volatility, exchange_operators).
narrative_ontology:constraint_beneficiary(speculative_narrative_volatility, narrative_engineers).
narrative_ontology:constraint_victim(speculative_narrative_volatility, late_adopters).
narrative_ontology:constraint_victim(speculative_narrative_volatility, retail_investors).
narrative_ontology:constraint_victim(speculative_narrative_volatility, sound_money_believers).
narrative_ontology:constraint_victim(speculative_narrative_volatility, decentralization_ideology_adherents).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: RETAIL INVESTOR (SNARE) — Trapped in the volatility cycle with no exit. Entered through sound-money narrative (inflation hedge, store of value) but experiences pure extraction through price volatility. Cannot distinguish between legitimate value discovery and narrative manipulation. Suppression maintained through information asymmetry and behavioral lock-in (sunk-cost fallacy, identity fusion with the 'revolutionary technology'). No organized exit available; individual escape attempts trigger losses that reinforce the trap.
constraint_indexing:constraint_classification(speculative_narrative_volatility, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: INSTITUTIONAL TRADER (TANGLED ROPE) — Constrained by regulatory uncertainty and exchange operational dependency but benefits from liquidity coordination. The constraint coordinates price discovery (genuine function) while extracting rent from volatility (asymmetric cost). Traders experience the volatility as both mechanism for their profit and coordination signal. Suppression through regulatory fragmentation and exchange lock-in; extraction through their ability to exploit retail order flow. Partial agency — can hedge, diversify, or exit but at significant cost.
constraint_indexing:constraint_classification(speculative_narrative_volatility, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: EXCHANGE OPERATOR (ROPE) — Primary beneficiary with arbitrage options. Operates within and benefits from the volatility constraint: transaction fees scale with trading volume; volatility increases volume; narrative instability increases volatility. Experiences the constraint as coordination mechanism (enabling peer-to-peer trading) with no experienced extraction. Can arbitrage between exchanges, control liquidity provision, and exit if regulatory environment shifts. Net beneficiary with high agency.
constraint_indexing:constraint_classification(speculative_narrative_volatility, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: EARLY ADOPTER / LARGE HOLDER (TANGLED ROPE) — Powerful agent with mobile exit options. Benefits from appreciation driven by retail inflows but pays suppression costs through regulatory risk and volatility (even if profitable on net). Genuine coordination function: large holders stabilize value through long-term commitment (reducing volatility for others). But also extracts through information asymmetry and ability to move markets. Mixed experience: coordination benefits (network effect, value appreciation) with embedded extraction (ability to influence narrative, front-run retail flows). Declassified from Rope in v1.0 because of suppression magnitude and regulatory dependency.
constraint_indexing:constraint_classification(speculative_narrative_volatility, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: DECENTRALIZATION GOVERNANCE COALITION (SCAFFOLD) — Organized agents (protocol developers, node operators, privacy advocates) attempting to rebuild decentralized infrastructure that bypasses exchange dependency. See the volatility constraint as a temporary coordination failure caused by centralized exchanges and speculative narratives. Sunset logic: replacing centralized exchanges with AMMs (Automated Market Makers), rollups, cross-chain DEXs. Suppression maintained through technical barriers and network effects of incumbent exchanges. But coalition has agency and clear exit pathways (technological alternatives). Theater is low — they are building actual alternative infrastructure, not performing within the current system.
constraint_indexing:constraint_classification(speculative_narrative_volatility, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: SOUND MONEY INSTITUTIONAL NARRATIVE (PITON) — The intellectual apparatus framing Bitcoin/crypto as 'sound money' (inflation hedge, store of value) persists despite structural incompatibility with speculative volatility. The narrative is theater: sound money requires price stability; volatility is anti-correlated with sound-money function. The narrative persists through ideological commitment, not functional verification. High theater ratio (institutional inertia maintains the framing despite falsification). But the primary function has atrophied — sound money claims are maintained through mythic reiteration, not through practical validation of purchasing-power stability. Degraded rope → piton.
constraint_indexing:constraint_classification(speculative_narrative_volatility, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From civilizational scope, the volatility pattern appears as an immutable law of speculation: any asset without intrinsic cash flows will exhibit narrative-driven volatility; without regulatory friction or dominant pool, speculative markets naturally bifurcate into early adopters (information advantage) and late arrivals (information disadvantage). However, this perspective risks naturalizing what is actually a contingent institutional arrangement. The engine's false summit detector will identify this as naturalization: the beneficiaries (early adopters, exchanges) have structural interest in treating volatility as inevitable rather than contingent.
constraint_indexing:constraint_classification(speculative_narrative_volatility, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(speculative_narrative_volatility_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(speculative_narrative_volatility, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(speculative_narrative_volatility, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(speculative_narrative_volatility, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(speculative_narrative_volatility, TR),
    TR >= 0.70.

:- end_tests(speculative_narrative_volatility_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The constraint extracts through multiple mechanisms: (1) retail investor losses from volatility and poor timing; (2) exchange operator rent from transaction fees and spreads; (3) early adopter gains from appreciation driven by late-arrival flows. The measurement trajectory (0.32→0.58) shows accumulation over time as the speculative cycle matures and retail flows increase. Not maximal (0.58 < 0.70) because genuine coordination function persists — peer-to-peer value transfer is real, not purely theatrical. Suppression (0.65): High. Maintained through (1) information asymmetry (retail vs institutional traders and early adopters); (2) behavioral lock-in (sunk-cost fallacy, identity fusion); (3) regulatory fragmentation and exchange custodial dependency; (4) technical barriers to DEX adoption. Retail investors face maximum suppression; institutional traders and early adopters face regulatory suppression but retain exit pathways. Theater Ratio (0.68): High. The sound-money narrative performs an institutional function (attracting retail flows) despite fundamental incompatibility with volatility. The decentralization narrative performs ideological function despite practical centralization. Performative narratives far exceed genuine descriptive content of market dynamics. Theater increases over measurement interval (0.42→0.71) as institutional attention and celebrity endorsements amplify narrative sophistication.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates structural perspectival inversion: the beneficiary (exchange operator) experiences rope (pure coordination with no extraction); the victim (retail investor) experiences snare (pure extraction with no coordination benefit). The institutional trader experiences tangled rope (mixed coordination and extraction at moderate suppression). The decentralization coalition sees this as temporary scaffolding — alternative infrastructure offers genuine exit paths. The sound-money narrative persists as piton despite falsification. The analytical observer risks mountain (naturalizing volatility as inevitable) but the engine's false-summit detector identifies structural contingency. No single perspective is 'correct' — the constraint's structure is genuinely multi-perspectival.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality derivation from beneficiary/victim declarations and exit_options: Retail investors are victims with trapped exit → d ≈ 0.95 → f(d) ≈ 1.42 → high experienced χ. Exchange operators are beneficiaries with arbitrage exit → d ≈ 0.05 → f(d) ≈ -0.12 → negative experienced χ (pure benefit). Institutional traders are mixed (constrained exit, moderate power) → d ≈ 0.65 → f(d) ≈ 1.00 → moderate experienced χ. Early adopters are beneficiaries with mobile exit (but regulatory suppression) → d ≈ 0.30 → f(d) ≈ 0.10 → low experienced χ but high material benefit. Decentralization coalition are organized victims with constrained exit → d ≈ 0.55 → f(d) ≈ 0.75 → moderate experienced χ but declining as alternatives mature. Scope modifier σ(global) = 1.2 amplifies effective extraction at global scale — regulatory fragmentation and narrative reach both scale globally, intensifying the constraint's force.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by showing that tangled-rope classification is correct because: (1) genuine coordination function exists (peer-to-peer value transfer, liquidity provision, price discovery); (2) asymmetric extraction exists (volatility rent, information asymmetry, regulatory arbitrage); (3) active enforcement required (narrative maintenance, exchange operations, early-adopter capital retention). The constraint is NOT pure snare (which would eliminate the coordination function) and NOT pure rope (which would eliminate asymmetric extraction). The perspectival gap between beneficiary (rope) and victim (snare) does not resolve to a single type — it confirms tangled rope because different agents genuinely experience different functions. The false-summit natural-law perspective is detected by the FSM signature: analytical perspective classifies as mountain, but structural beneficiary data exists (early adopters, exchange operators), triggering reclassification to tangled_rope. The mandatrophy question 'Is this coordination or extraction?' resolves to 'both, for different agents'.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sound_money_versus_speculation_incompatibility,
    'Can cryptocurrency simultaneously function as sound money (price-stable inflation hedge) and speculative asset (narrative-driven volatility source)?',
    'Time-series analysis of price stability vs. functional store-of-value utility; correlation between adoption narratives and volatility; historical precedent from fiat currencies and commodity standards',
    'If incompatible (high probability): sound money reading is performative cover story for speculation. Classification shifts from mountain (immutable property of sound money) to piton (degraded narrative maintained through inertia). If compatible: constraints decompose into separate stories (sound money coordination vs. speculative volatility extraction).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sound_money_versus_speculation_incompatibility, empirical, 'Structural compatibility between sound-money function and speculative volatility').

omega_variable(
    centralized_exchange_dependency_contradiction,
    'How does practical reliance on centralized exchanges (which require KYC, regulatory compliance, custodial risk) coexist with decentralization ideology (which requires disintermediation from financial intermediaries)?',
    'Adoption flow analysis: percentage of transaction volume through DEXs vs CEXs; regulatory pressure vectors on each; technical maturity of decentralized alternatives; network-effect lock-in mechanisms',
    'If centralization remains dominant (high probability): decentralization is performative ideology, not functional constraint. Constraint reclassifies as snare (ideology naturalizes institutional dependency). If DEXs achieve functional parity: decentralization coalition''s scaffold perspective is validated, and sunset clause is real.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(centralized_exchange_dependency_contradiction, empirical, 'Whether decentralization ideology operationalizes or remains performative given exchange centralization').

omega_variable(
    narrative_volatility_feedback_loop,
    'Is volatility a mechanism for discovering fundamental value, or a self-referential narrative loop where volatility itself drives adoption narratives that drive more volatility?',
    'Volatility clustering analysis; correlation between narrative events (conferences, celebrity endorsements, regulatory announcements) and price movements; counterfactual: what fraction of price movement is unpredicted by fundamental crypto-specific developments vs. macro narratives',
    'If self-referential (high probability): volatility is pure extraction mechanism (snare dominant). If fundamental discovery: volatility is coordination cost (rope/tangled-rope dominant). If mixed: constraint requires decomposition into narrative-driven volatility (snare) and legitimate price discovery (rope).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(narrative_volatility_feedback_loop, empirical, 'Whether volatility reflects fundamental discovery or self-referential narrative feedback').

omega_variable(
    regulatory_arbitrage_sustainability,
    'Does the decentralization narrative emerge from genuine technical necessity, or from regulatory arbitrage strategy (avoiding KYC/AML/securities regulation by claiming to be ''decentralized'')?',
    'Comparative analysis: regulatory intensity in different jurisdictions vs. adoption rates; technological solutions to regulatory problems (privacy coins, rollups) vs. pure decentralization claims; institutional investor entry following regulatory clarity',
    'If regulatory arbitrage dominant: decentralization is performative framing for regulatory evasion. Constraint reclassifies as snare (decentralization ideology naturalizes regulatory arbitrage). If genuine technical necessity: decentralization coalition''s scaffold perspective is validated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_arbitrage_sustainability, conceptual, 'Whether decentralization is genuine technical alternative or regulatory arbitrage strategy').

omega_variable(
    identity_locked_ideology_fusion,
    'To what extent do retail investors'' identity become fused with cryptocurrency ideology (revolutionary technology, freedom from central banking, financial sovereignty), making exit psychologically impossible even when structural mobility exists?',
    'Exit behavior analysis: comparison of rational exit criteria (technical innovation, regulatory risk, price targets) vs. actual exit patterns; survey of retain-despite-loss motives; tracking of reinvestment despite documented losses in previous cycles',
    'If identity fusion dominant: retail exit_options reclassify from trapped to identity_locked. At biographical time horizon, identity_locked produces rope classification (constraint appears changeable in principle, is experientially unchangeable). This reveals binding mechanism is cognitive rather than material — the agent could exit but their identity frame prevents seeing this.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_locked_ideology_fusion, empirical, 'Degree of identity fusion with cryptocurrency ideology vs. material exit barriers').

omega_variable(
    false_summit_natural_law_naturalization,
    'Is the market volatility pattern actually a natural law of speculation (immutable, universal), or a contingent outcome of specific institutional structures (exchange centralization, narrative-driven valuation, speculation incentives)?',
    'Comparative institutional analysis: volatility patterns in DEX-dominant vs CEX-dominant ecosystems; volatility under different narrative regimes (institutional adoption vs retail speculation); technical controls for fundamentals-based variance',
    'If institutional contingency: analytical mountain perspective is a false summit. Engine reclassifies to snare or tangled_rope. If genuine natural law: mountain classification is validated and other perspectives'' suppression experience becomes inevitable coordination cost.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(false_summit_natural_law_naturalization, conceptual, 'Whether market volatility is natural law of speculation or contingent institutional outcome').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(speculative_narrative_volatility, 0, 12).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(snv_tr_t0, speculative_narrative_volatility, theater_ratio, 0, 0.42).
narrative_ontology:measurement(snv_tr_t3, speculative_narrative_volatility, theater_ratio, 3, 0.55).
narrative_ontology:measurement(snv_tr_t6, speculative_narrative_volatility, theater_ratio, 6, 0.68).
narrative_ontology:measurement(snv_tr_t9, speculative_narrative_volatility, theater_ratio, 9, 0.71).
narrative_ontology:measurement(snv_tr_t12, speculative_narrative_volatility, theater_ratio, 12, 0.65).

% Extraction over time
narrative_ontology:measurement(snv_be_t0, speculative_narrative_volatility, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(snv_be_t3, speculative_narrative_volatility, base_extractiveness, 3, 0.45).
narrative_ontology:measurement(snv_be_t6, speculative_narrative_volatility, base_extractiveness, 6, 0.58).
narrative_ontology:measurement(snv_be_t9, speculative_narrative_volatility, base_extractiveness, 9, 0.64).
narrative_ontology:measurement(snv_be_t12, speculative_narrative_volatility, base_extractiveness, 12, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(speculative_narrative_volatility, resource_allocation).
narrative_ontology:affects_constraint(speculative_narrative_volatility, centralized_exchange_regulatory_capture).
narrative_ontology:affects_constraint(speculative_narrative_volatility, monetary_policy_inflation_hedge_narrative).
narrative_ontology:affects_constraint(speculative_narrative_volatility, decentralized_finance_infrastructure_maturity).

% DUAL FORMULATION NOTE:
% Speculative narrative volatility decomposes into three structurally distinct constraint stories: (1) sound-money narrative (ε≈0.15, piton) — the ideological claim that cryptocurrency functions as inflation hedge despite volatility evidence; (2) speculative volatility extraction (ε≈0.72, snare) — pure extraction through narrative-driven price volatility with retail victims; (3) decentralized coordination (ε≈0.35, rope/scaffold) — genuine peer-to-peer value transfer and liquidity provision with sunset clause as DEXs mature. The present story (SNV) treats the tangled-rope coupling of coordination and extraction at the system level. Upstream constraints (monetary policy inflation narrative, exchange regulatory capture) feed narrative plausibility. Downstream constraints (DEX infrastructure maturity) determine whether scaffold exit paths are genuine or aspirational.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(speculative_narrative_volatility, institutional, 0.08).
constraint_indexing:directionality_override(speculative_narrative_volatility, moderate, 0.62).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
