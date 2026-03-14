% ============================================================================
% CONSTRAINT STORY: regulatory_arbitrage_crypto_jurisdictions
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_regulatory_arbitrage_crypto_jurisdictions, []).

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
 *   constraint_id: regulatory_arbitrage_crypto_jurisdictions
 *   human_readable: Regulatory Arbitrage in Cryptocurrency Jurisdictions
 *   domain: financial/regulatory
 *
 * SUMMARY:
 *   Regulatory arbitrage in cryptocurrency jurisdictions creates a structural
 *   tension between capital mobility and financial oversight. Platforms and
 *   sophisticated actors exploit fragmentation across regulatory regimes,
 *   choosing jurisdictions based on compliance burden and profit opportunity.
 *   This generates three simultaneous mechanisms: (1) extraction from retail
 *   investors through platform risk concentration, (2) coordination of global
 *   financial activity across jurisdictions, and (3) competition between
 *   restrictive and permissive jurisdictions for tax revenue and financial
 *   sector development. The constraint exhibits tangled rope characteristics
 *   — genuine coordination (global settlement networks, market access)
 *   coexists with asymmetric extraction (regulatory shopping transfers risk
 *   to unsophisticated users and destabilizes restrictive regimes). The
 *   theater ratio (0.48) reflects significant compliance signaling (KYC/AML
 *   certifications, self-regulatory organizations) that provides modest
 *   genuine safety screening but also creates an illusion of regulatory
 *   oversight that conceals platform insolvency risks (as FTX collapse
 *   demonstrated). The extractiveness trajectory (0.35 → 0.62 over the 9-year
 *   interval) shows accelerating rent capture as platform scale increased and
 *   regulatory divergence widened.
 *
 * KEY AGENTS:
 *   - Crypto Platform Operators: Primary beneficiary (institutional/arbitrage) — capture arbitrage spreads and avoid high-compliance jurisdictions; highest exit optionality
 *   - Retail Investors: Primary victim (powerless/trapped) — exposed to platform solvency risk, custody risk, and regulatory uncertainty across jurisdictions; cannot assess risk or exit selectively
 *   - Permissive Jurisdiction Authorities: Secondary beneficiary (institutional/arbitrage) — capture licensing fees, transaction taxes, and employment from platform relocation; benefit from arbitrage without enforcement cost
 *   - Restrictive Jurisdiction Regulators: Mixed (moderate/constrained) — must maintain strict standards to protect local investors but face capital flight and outflow pressure; constrained by inability to unilaterally relax without inviting contagion
 *   - International Regulatory Coalition (FATF, FSB, G7): Organized agents (organized/constrained) — attempt to harmonize standards and close arbitrage vectors; constrained by enforcement difficulty and sovereign power fragmentation
 *   - High-Net-Worth Traders: Secondary beneficiary (powerful/arbitrage) — exploit regulatory fragmentation for tax optimization and regulatory shopping; highest sophistication and exit optionality
 *   - Financial System Stability (Abstract): Victim (powerless/trapped) — bears aggregate systemic risk from platform concentration in permissive jurisdictions; no agent to advocate for this interest
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(regulatory_arbitrage_crypto_jurisdictions, 0.58).
domain_priors:suppression_score(regulatory_arbitrage_crypto_jurisdictions, 0.65).
domain_priors:theater_ratio(regulatory_arbitrage_crypto_jurisdictions, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(regulatory_arbitrage_crypto_jurisdictions, extractiveness, 0.58).
narrative_ontology:constraint_metric(regulatory_arbitrage_crypto_jurisdictions, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(regulatory_arbitrage_crypto_jurisdictions, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(regulatory_arbitrage_crypto_jurisdictions, tangled_rope).
narrative_ontology:human_readable(regulatory_arbitrage_crypto_jurisdictions, "Regulatory Arbitrage in Cryptocurrency Jurisdictions").
narrative_ontology:topic_domain(regulatory_arbitrage_crypto_jurisdictions, "financial/regulatory").

domain_priors:requires_active_enforcement(regulatory_arbitrage_crypto_jurisdictions).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(regulatory_arbitrage_crypto_jurisdictions, crypto_platforms).
narrative_ontology:constraint_beneficiary(regulatory_arbitrage_crypto_jurisdictions, permissive_jurisdictions).
narrative_ontology:constraint_beneficiary(regulatory_arbitrage_crypto_jurisdictions, high_net_worth_users).
narrative_ontology:constraint_victim(regulatory_arbitrage_crypto_jurisdictions, retail_investors).
narrative_ontology:constraint_victim(regulatory_arbitrage_crypto_jurisdictions, restrictive_jurisdictions).
narrative_ontology:constraint_victim(regulatory_arbitrage_crypto_jurisdictions, financial_system_stability).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: RETAIL INVESTOR (SNARE) — Trapped in regulatory fragmentation with no exit. Faces platform risk, custody risk, and compliance uncertainty across jurisdictions. Cannot verify which platforms are solvent or compliant. Bears full cost of platform collapses (FTX, Celsius, Luna) while extraction flows to platform operators and permissive regulators. Zero agency in jurisdiction selection.
constraint_indexing:constraint_classification(regulatory_arbitrage_crypto_jurisdictions, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: RESTRICTIVE JURISDICTION REGULATOR (TANGLED ROPE) — Constrained by capital flight and regulatory arbitrage: stricter rules drive activity to permissive jurisdictions, undermining local financial stability. Yet genuine coordination function exists — preventing local systemic risk. Extraction flows: platform operators arbitrage the jurisdiction's stability requirements, but the regulator's enforcement also protects local citizens who remain within the regime. Constrained exit because shifting to permissive stance invites capital and contagion.
constraint_indexing:constraint_classification(regulatory_arbitrage_crypto_jurisdictions, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: PERMISSIVE JURISDICTION AUTHORITY (ROPE) — Benefits from regulatory arbitrage through licensing fees, transaction taxes, and financial sector employment. Experiences the constraint as pure coordination: attracting platforms solves the local economic development problem. Can arbitrage upward if enforcement pressure increases (relocate licensing regime). Net beneficiary with high exit optionality.
constraint_indexing:constraint_classification(regulatory_arbitrage_crypto_jurisdictions, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: CRYPTO PLATFORM OPERATOR (ROPE) — Arbitrages across regulatory regimes, choosing jurisdiction for each service line (trading in permissive, staking in moderate, derivatives in another). High optionality (arbitrage exit) enables them to coordinate globally while avoiding local enforcement. Experiences the constraint as enabling — regulatory fragmentation creates the profitable spread. Net beneficiary.
constraint_indexing:constraint_classification(regulatory_arbitrage_crypto_jurisdictions, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: INTERNATIONAL REGULATORY COALITION (TANGLED ROPE) — Organized agents (FATF, FSB, national regulators via coordination) attempt to harmonize standards to eliminate arbitrage. Genuine coordination function: global financial stability requires reducing regulatory fragmentation. But extraction mechanism persists: harmonization imposes compliance costs on smaller jurisdictions while larger ones set the rules. Constrained because unilateral action by one regulator creates immediate outflow (arbitrage by operators and users). Must maintain organized coalition to enforce uniformity.
constraint_indexing:constraint_classification(regulatory_arbitrage_crypto_jurisdictions, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: COMPLIANCE THEATER SYSTEM (PITON) — KYC/AML rules, self-regulatory organizations, and compliance certifications have become largely performative in crypto: regulated platforms interface with unregulated stablecoins and decentralized exchanges, and high-value smuggling occurs through accepted channels (structured deposits, remittance corridors). The regulatory apparatus persists because alternatives haven't fully matured and because the coordination function (preventing obvious money laundering) still provides modest utility. But the primary effect is theater — compliance cost without corresponding risk reduction. Theater ratio 0.48 reflects that some genuine coordination remains (preventing obvious narcotics money) but much is ritual.
constraint_indexing:constraint_classification(regulatory_arbitrage_crypto_jurisdictions, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, regulatory arbitrage appears as an inevitable consequence of information asymmetry and sovereign power: any technology that enables capital flow without physical presence will exploit regulatory gaps as a natural law of distributed systems. Horizontal enforcement across jurisdictions is impossible without hegemonic authority. This perspective naturalizes the constraint as inherent to decentralized systems. The engine's false summit detection will reveal this as naturalization of a contingent institutional arrangement — arbitrage persists because international regulatory coordination has not been sufficiently organized, not because it is mathematically impossible.
constraint_indexing:constraint_classification(regulatory_arbitrage_crypto_jurisdictions, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(regulatory_arbitrage_crypto_jurisdictions_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(regulatory_arbitrage_crypto_jurisdictions, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(regulatory_arbitrage_crypto_jurisdictions, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(regulatory_arbitrage_crypto_jurisdictions, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(regulatory_arbitrage_crypto_jurisdictions, TR),
    TR >= 0.70.

:- end_tests(regulatory_arbitrage_crypto_jurisdictions_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high, reflecting the dual mechanisms. Platform operators capture significant economic rent through regulatory shopping (reducing compliance costs) and price discovery advantages. Retail investors bear custodial risk (platform insolvency, hacks) that concentrated platforms in permissive jurisdictions amplify. The extractiveness increased from 0.35 to 0.62 over the interval as platforms consolidated and regulatory divergence widened. Suppression (0.65): High. Retail investors face substantial barriers to exit: platform switching costs (tax liability, liquidity fragmentation), information asymmetry about platform solvency, and inability to verify custody arrangements. Regulatory fragmentation creates uncertainty (which jurisdiction's rules apply, what recourse exists in platform failure) that increases effective suppression. Theater ratio (0.48): Moderate. KYC/AML compliance and self-regulatory organization certifications (BitMEX's position limit regulations, Coinbase's transparency reports) provide genuine screening for obvious narcotics flows but create false confidence about platform safety. The gap between regulatory signaling and actual insolvency protection (FTX had SOC 2 compliance signaling while operating a secret hedge fund with customer deposits) indicates substantial theater without corresponding risk reduction.
 *
 * PERSPECTIVAL GAP:
 *   The primary perspectival gap is between the platform operators' rope (they coordinate global markets and benefit from regulatory diversity) and the retail investors' snare (they bear platform risk with no agency). The permissive jurisdiction authority sees rope (they benefit from licensing revenue and employment without compliance cost), but the restrictive jurisdiction regulator sees tangled rope (they must maintain overhead to protect local investors while facing arbitrage outflow). The international regulatory coalition sees tangled rope with constrained exit — harmonization imposes costs on smaller jurisdictions and creates centralization risk, but unilateral action fails. The analytical observer risks mountain (regulatory arbitrage is inherent to decentralized capital flows), but the structural data reveals this as naturalization: empirically, arbitrage could be closed through sufficient international coordination (federated surveillance, mutual legal assistance treaties, restrictions on fiat on-ramps in permissive jurisdictions), indicating that the arbitrage is contingent institutional arrangement, not natural law.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary-victim declarations map directly to exit options and power. Platform operators have arbitrage exit (can relocate operations to changing regulatory environment) and institutional power → net beneficiaries, low directionality, low effective extraction experienced. Retail investors have trapped exit (cannot selectively choose which platforms to use without incurring switching costs; constrained by information asymmetry) and powerless status → net victims, high directionality, high effective extraction. Permissive jurisdictions have arbitrage exit (can shift licensing stance if cost-benefit changes) and institutional power → net beneficiaries. Restrictive jurisdiction regulators have constrained exit (cannot unilaterally relax without inviting local instability; cannot unilaterally tighten without capital flight) and moderate power → mixed experience. The international coalition has constrained exit (cannot enforce unilaterally; depends on consensus, which is difficult to achieve) and organized power → experiences extraction asymmetry when smaller jurisdictions are forced to adopt standards set by larger financial centers.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint exemplifies how mandatrophy emerges when a genuine coordination function (global market access, cross-border settlement) becomes entwined with extraction (regulatory shopping externalizes risk to retail investors). The tangled rope classification resolves the mandatrophy by disaggregating the mechanisms: (1) coordination = platform operators providing global liquidity and price discovery (real value), (2) extraction = retail investors concentrated in jurisdictions with weakest consumer protections bearing custodial risk. Neither pure extraction (snare) nor pure coordination (rope) is correct because the mechanism requires BOTH: platforms benefit from arbitrage because fragmentation exists (coordination failure), and retail investors suffer because the fragmentation enables risk concentration (extraction enabled by coordination breakdown). The snare classification from the retail perspective is correct; the rope classification from the platform perspective is correct. The tangled rope from the international coalition perspective is correct because harmonization efforts attempt to preserve the coordination function (global markets) while eliminating the extraction mechanism (risk concentration). The piton perspective reveals compliance theater — KYC/AML has become ritualistic without corresponding safety improvement, maintained through inertia because alternatives (fully decentralized verification, on-chain compliance) have not matured.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    stablecoin_regulatory_status,
    'Are regulated stablecoins and central bank digital currencies sufficient to eliminate retail investor exposure to unregulated platform risk, or do they create new arbitrage vectors?',
    'Post-CBDC deployment measurement: tracking platform operator migration to CBDC-compatible systems vs emergence of synthetic stablecoin markets; comparison of retail loss rates under stablecoin-only vs mixed-custody regimes',
    'If CBDCs sufficient: snare classification for retail investors falls to rope (platform risk removed, regulatory fragmentation becomes pure coordination problem). If new arbitrage vectors emerge: snare persists with higher theater ratio.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(stablecoin_regulatory_status, empirical, 'Whether CBDC deployment eliminates platform risk arbitrage').

omega_variable(
    decentralized_exchange_regulatory_binding,
    'Can regulatory frameworks bind non-custodial decentralized exchanges (DEXs), or is the architectural separation between rule-making and code enforcement insurmountable?',
    'Analysis of DEX transaction volume pre- and post-regulatory action targeting smart contract deployers; tracking whether regulatory pressure on DEX infrastructure (RPC providers, liquidity providers, governance tokens) achieves compliance equivalent to centralized platforms',
    'If binding possible: the arbitrage vector narrows, tangled rope reclassifies closer to rope for organized regulators. If impossible: the natural law perspective gains empirical support — architectural decentralization is the true constraint, and regulatory arbitrage is its inevitable shadow.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(decentralized_exchange_regulatory_binding, empirical, 'Whether DEX architecture permits regulatory binding').

omega_variable(
    retail_investor_rational_ignorance,
    'Does retail investor risk exposure reflect active arbitrage decisions (sophisticated players managing multiple venues) or rational ignorance (users unable to assess platform solvency)?',
    'Behavioral analysis of user portfolio allocation across platforms; testing whether concentration on high-risk platforms is correlated with user sophistication, fee optimization strategies, or yield-chasing without risk assessment',
    'If rational ignorance dominant: snare classification is secure — retail investors are trapped by information asymmetry, not free choice. If arbitrage behavior dominant: some retail agents should be reclassified to moderate power (constrained rather than trapped), tempering the snare classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(retail_investor_rational_ignorance, empirical, 'Whether retail risk exposure reflects active arbitrage or information failure').

omega_variable(
    permissive_jurisdiction_capture_mechanism,
    'Do permissive jurisdictions actively seek platform relocation as economic development policy, or does arbitrage occur by default independent of regulatory intent?',
    'Document analysis of regulatory announcements, licensing fee structures, and enforcement patterns; comparison of jurisdictions with explicit crypto-friendly mandates vs those tolerating arbitrage through neglect',
    'If active capture: the permissive jurisdiction perspective (rope/arbitrage) requires directionality override downward — they are not pure beneficiaries but partially captured by the platform operator dynamics. If default arbitrage: the rope classification stands unmodified.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(permissive_jurisdiction_capture_mechanism, empirical, 'Whether permissive jurisdictions actively pursue crypto relocation').

omega_variable(
    international_regulatory_coordination_binding,
    'Can FATF mutual evaluation and FSB standards achieve sufficient coordination to close major arbitrage vectors, or is consensus enforcement impossible without hegemonic authority?',
    'Tracking jurisdictional adoption of FATF and FSB recommendations; measurement of regulatory divergence over time despite coordination efforts; analysis of whether platforms actually comply with coordinated standards or use sub-state regulatory shopping (city-level licensing, special economic zones)',
    'If coordination effective: the international coalition perspective (tangled rope/constrained) transitions toward rope as consensus enforcement reduces arbitrage profit. If ineffective: mountain perspective gains support — the natural law view (regulatory arbitrage is inevitable under distributed authority) becomes empirically validated.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(international_regulatory_coordination_binding, empirical, 'Whether international regulatory coordination closes arbitrage vectors').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(regulatory_arbitrage_crypto_jurisdictions, 0, 9).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(regcrypto_tr_t0, regulatory_arbitrage_crypto_jurisdictions, theater_ratio, 0, 0.32).
narrative_ontology:measurement(regcrypto_tr_t3, regulatory_arbitrage_crypto_jurisdictions, theater_ratio, 3, 0.4).
narrative_ontology:measurement(regcrypto_tr_t6, regulatory_arbitrage_crypto_jurisdictions, theater_ratio, 6, 0.48).
narrative_ontology:measurement(regcrypto_tr_t9, regulatory_arbitrage_crypto_jurisdictions, theater_ratio, 9, 0.55).

% Extraction over time
narrative_ontology:measurement(regcrypto_be_t0, regulatory_arbitrage_crypto_jurisdictions, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(regcrypto_be_t3, regulatory_arbitrage_crypto_jurisdictions, base_extractiveness, 3, 0.48).
narrative_ontology:measurement(regcrypto_be_t6, regulatory_arbitrage_crypto_jurisdictions, base_extractiveness, 6, 0.58).
narrative_ontology:measurement(regcrypto_be_t9, regulatory_arbitrage_crypto_jurisdictions, base_extractiveness, 9, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(regulatory_arbitrage_crypto_jurisdictions, resource_allocation).
narrative_ontology:affects_constraint(regulatory_arbitrage_crypto_jurisdictions, cross_border_capital_flight).
narrative_ontology:affects_constraint(regulatory_arbitrage_crypto_jurisdictions, stablecoin_reserve_fragmentation).
narrative_ontology:affects_constraint(regulatory_arbitrage_crypto_jurisdictions, retail_custody_concentration).
narrative_ontology:affects_constraint(regulatory_arbitrage_crypto_jurisdictions, financial_sanctions_enforcement).

% DUAL FORMULATION NOTE:
% Regulatory arbitrage in crypto is downstream of the fundamental problem of distributed sovereignty (no global enforcement authority) and upstream of platform-specific custody failures. The constraint family includes separate stories: (1) regulatory_arbitrage_crypto_jurisdictions (this story) — the macro institutional mechanism, (2) platform_operator_regulatory_capture — the firm-level dynamics, (3) retail_investor_custody_risk — the individual-level extraction. Each has distinct epsilon and perspectives. This story operates at the institutional/international level where the coordination and arbitrage mechanisms are most visible.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(regulatory_arbitrage_crypto_jurisdictions, institutional, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
