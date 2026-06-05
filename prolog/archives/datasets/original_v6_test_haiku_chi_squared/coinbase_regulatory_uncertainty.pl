% ============================================================================
% CONSTRAINT STORY: coinbase_regulatory_uncertainty
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_coinbase_regulatory_uncertainty, []).

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
 *   constraint_id: coinbase_regulatory_uncertainty
 *   human_readable: Crypto-Regulatory Ambiguity (Howey Test Application)
 *   domain: political/economic
 *
 * SUMMARY:
 *   The regulatory ambiguity surrounding crypto assets—specifically whether
 *   certain assets are 'securities' under the SEC's jurisdiction or
 *   'commodities' under the CFTC's—has created a persistent structural
 *   extraction mechanism disguised as a coordination problem. The United
 *   States lacks a unified, clear legal framework for classifying crypto
 *   assets. The SEC applies the Howey test (a 1946 standard designed for
 *   equity offerings) to token economics without legislative clarity. The
 *   CFTC regulates crypto futures and spot commodities but with different
 *   standards. The result is a Tangled Rope constraint: platforms must
 *   coordinate with users via compliance theater while bearing selective
 *   enforcement risk; regulators benefit from discretionary authority;
 *   incumbent financial institutions benefit from delayed competition; retail
 *   investors and crypto projects are trapped or constrained by retroactive
 *   enforcement. The theater ratio has risen from 0.35 to 0.64 over the
 *   interval as enforcement actions (SEC lawsuits against Coinbase, Ripple,
 *   Telegram) have intensified without producing clear precedent.
 *   Extractiveness has risen from 0.28 to 0.52 as selective enforcement has
 *   become the primary regulatory mechanism, allowing agencies to extract
 *   compliance costs and compliance scope uncertainty from platforms and
 *   projects.
 *
 * KEY AGENTS:
 *   - Retail Crypto Investors: Primary victims (powerless/trapped) — face retroactive reclassification risk with no legal recourse
 *   - Crypto Trading Platforms (Coinbase, Kraken, etc.): Secondary victims (moderate/constrained) — bear compliance burden and selective enforcement risk while coordinating with users
 *   - Crypto Asset Issuers (Projects): Secondary victims (moderate/constrained) — cannot exit tokenomics without project failure; face potential securities liability
 *   - Incumbent Financial Institutions (Traditional Banks): Primary beneficiaries (institutional/arbitrage) — benefit from delayed competition and market preservation
 *   - Regulatory Agencies (SEC/CFTC): Beneficiaries (institutional/arbitrage) — preserve discretionary authority; avoid legislative clarity that would reduce agency leverage
 *   - Crypto Industry Coalition: Organized agents (organized/constrained) — attempt self-regulation and industry standards but constrained by external enforcement
 *   - Howey Test Framework: Institutional artifact (piton) — persists through enforcement theater despite structural inadequacy for token economics
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing regulatory choice as inherent legal indeterminacy
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(coinbase_regulatory_uncertainty, 0.52).
domain_priors:suppression_score(coinbase_regulatory_uncertainty, 0.68).
domain_priors:theater_ratio(coinbase_regulatory_uncertainty, 0.64).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(coinbase_regulatory_uncertainty, extractiveness, 0.52).
narrative_ontology:constraint_metric(coinbase_regulatory_uncertainty, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(coinbase_regulatory_uncertainty, theater_ratio, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(coinbase_regulatory_uncertainty, tangled_rope).
narrative_ontology:human_readable(coinbase_regulatory_uncertainty, "Crypto-Regulatory Ambiguity (Howey Test Application)").
narrative_ontology:topic_domain(coinbase_regulatory_uncertainty, "political/economic").

domain_priors:requires_active_enforcement(coinbase_regulatory_uncertainty).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(coinbase_regulatory_uncertainty, incumbent_financial_institutions).
narrative_ontology:constraint_beneficiary(coinbase_regulatory_uncertainty, regulatory_agencies).
narrative_ontology:constraint_victim(coinbase_regulatory_uncertainty, retail_crypto_investors).
narrative_ontology:constraint_victim(coinbase_regulatory_uncertainty, crypto_platforms).
narrative_ontology:constraint_victim(coinbase_regulatory_uncertainty, asset_issuer_projects).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: RETAIL CRYPTO INVESTOR (SNARE) — Trapped in legal ambiguity. Cannot exit without realizing losses or incurring tax uncertainty. Faces sudden reclassification risk (asset declared security retroactively) without recourse. d≈0.94, f(d)≈1.41, σ=1.0 → χ≈0.73.
constraint_indexing:constraint_classification(coinbase_regulatory_uncertainty, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: CRYPTO PLATFORM (TANGLED ROPE) — Constrained by competing regulatory interpretations; must coordinate with users via clear terms of service while bearing liability risk. Benefits from ambiguity (lists ambiguous assets, captures trading volume) but also suffers (enforcement actions, fines, delisting requirements). Requires active compliance theater. d≈0.58, f(d)≈0.78, σ=1.0 → χ≈0.41.
constraint_indexing:constraint_classification(coinbase_regulatory_uncertainty, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: INCUMBENT FINANCIAL INSTITUTIONS (ROPE) — Primary beneficiary. Ambiguity delays crypto competition. Can arbitrage regulatory gaps: traditional banks operate under well-defined regimes while crypto platforms face uncertainty. Coordination function: ambiguity preserves existing market structure. d≈0.08, f(d)≈-0.10, σ=1.0 → χ≈-0.05. Net beneficiary.
constraint_indexing:constraint_classification(coinbase_regulatory_uncertainty, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: REGULATORY AGENCIES (ROPE) — Benefit from ambiguity (preserves turf, avoids clear delegation). Can coordinate through enforcement action rather than legislative clarity, maintaining flexibility. Experiences constraint as coordination function: preserves discretionary authority. d≈0.12, f(d)≈-0.06, σ=1.0 → χ≈-0.03. Net beneficiary.
constraint_indexing:constraint_classification(coinbase_regulatory_uncertainty, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: CRYPTO INDUSTRY COALITION (TANGLED ROPE) — Organized but constrained by regulatory fragmentation. Benefits from some coordination (industry standards, self-regulation norms) but suffers from asymmetric extraction (selective enforcement, compliance theater). Requires active enforcement of own standards while facing external enforcement risk. d≈0.62, f(d)≈0.82, σ=1.0 → χ≈0.43.
constraint_indexing:constraint_classification(coinbase_regulatory_uncertainty, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: HOWEY TEST FRAMEWORK (PITON) — The 1946 Howey test (investment contract definition) is applied to crypto through enforcement theater rather than legislative clarity. Courts and regulators cite Howey repeatedly, but application remains inconsistent and precedent-dependent. Theater ratio=0.64 reflects that much regulatory activity is interpretive litigation rather than functional boundary-setting. The framework persists through institutional inertia despite being inadequate for modern tokenomics. d≈0.10, f(d)≈-0.07, σ=1.0 → χ≈-0.004.
constraint_indexing:constraint_classification(coinbase_regulatory_uncertainty, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 7: CRYPTO ASSET ISSUER (SNARE) — Constrained but not fully trapped. Cannot exit crypto issuance without forfeiting project viability. Faces potential retroactive securities law liability if asset is reclassified. Must navigate Howey ambiguity in token design and distribution. d≈0.80, f(d)≈1.18, σ=1.0 → χ≈0.61.
constraint_indexing:constraint_classification(coinbase_regulatory_uncertainty, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER (MOUNTAIN RISK) — Temptation to see ambiguity as an irreducible feature of law itself: Howey test is inherently indeterminate because token economics are novel. However, structural data (ε=0.52, suppression=0.68, theater=0.64) reveals this as a false summit. The ambiguity is contingent institutional choice (regulatory coordination failure), not a natural law. d≈0.72, f(d)≈1.15, σ=1.0 → χ≈0.60.
constraint_indexing:constraint_classification(coinbase_regulatory_uncertainty, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(coinbase_regulatory_uncertainty_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(coinbase_regulatory_uncertainty, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(coinbase_regulatory_uncertainty, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(coinbase_regulatory_uncertainty, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(coinbase_regulatory_uncertainty, TR),
    TR >= 0.70.

:- end_tests(coinbase_regulatory_uncertainty_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The constraint extracts through two mechanisms: (1) Selective enforcement that creates compliance costs asymmetrically distributed across platforms and projects; (2) Delayed competition that allows incumbent financial institutions to preserve market position. The extractiveness has risen over the 12-year interval as enforcement actions have multiplied without producing clear precedent, increasing the cost of compliance theater. This is not pure extraction (snare-level ε≥0.66) because platforms do coordinate with users and genuine legal uncertainty exists; it is hybrid (tangled_rope range 0.40-0.90). Suppression (0.68): High. Significant barriers prevent exit from the ambiguity regime: (1) Retail investors cannot exit crypto without realizing losses or tax uncertainty; (2) Platforms cannot exit crypto listing without losing trading volume; (3) Regulatory agencies cannot be bypassed without international fragmentation risk; (4) No legislative pathway exists to clear the ambiguity because it requires congressional action (high transaction cost). The suppression reflects that organized escape is structurally blocked. Theater ratio (0.64): Moderate-high. Regulatory enforcement is substantially performative. The SEC's enforcement strategy is litigation-based (case-by-case Howey determinations) rather than rule-based (clear prophetic standards). Each enforcement action generates theater (regulatory pronouncements, platform policy changes, compliance theater) without producing stable legal precedent. For example, the SEC vs. Ripple case (2020-2023) produced no clear ruling on XRP's status despite years of litigation. The ratio has increased from 0.35 to 0.64 as agencies have shifted to enforcement-as-interpretation rather than legislative clarity.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits maximum perspectival divergence. The retail investor sees pure extraction (Snare) — they are trapped with no escape route and no coordination benefit. The platform sees coordination mixed with extraction (Tangled Rope) — they coordinate with users but suffer enforcement risk and compliance costs. The incumbent financial institution sees pure coordination (Rope) — the ambiguity functions to preserve market structure without requiring active coercion from the bank's perspective. The regulatory agency sees coordination (Rope) — ambiguity preserves discretionary authority and flexible policy space. The crypto industry coalition sees hybrid coordination-extraction (Tangled Rope) — they benefit from some coordination (industry standards, peer support) but suffer from asymmetric enforcement. The Howey test itself appears as a degraded institution (Piton) — it is applied repeatedly through enforcement theater despite being structurally inadequate. The analytical observer tempts toward seeing legal indeterminacy as natural law (Mountain false summit) but the structural data reveals this as a choice: clear statutory language could resolve the ambiguity.
 *
 * DIRECTIONALITY LOGIC:
 *   Retail crypto investor: Victim + trapped → d≈0.94, f(d)≈1.41. Maximum extraction. Cannot exit without realizing losses; faces retroactive reclassification. Crypto platform: Victim + constrained → d≈0.58, f(d)≈0.78. Significant extraction but not maximal. Has some exit options (reduce listings, exit markets) but at material cost. Incumbent financial institution: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.10. Net beneficiary. Captures competitive advantage during ambiguity period; can exit crypto market if clarity emerges without material loss. Regulatory agency: Beneficiary + arbitrage → d≈0.12, f(d)≈-0.06. Net beneficiary. Preserves discretionary authority; can shift policy if political winds change. Crypto industry coalition: Victim + constrained → d≈0.62, f(d)≈0.82. Significant extraction. Organized but constrained by enforcement risk and legal fragmentation. Crypto asset issuer: Victim + constrained → d≈0.80, f(d)≈1.18. High extraction. Cannot exit tokenomics without project failure; faces retroactive securities liability.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY UNRESOLVED (extractiveness 0.52, below 0.70 threshold but within snare range 0.46-0.66). The constraint exhibits genuine hybrid structure: (1) Coordination function: Ambiguity does coordinate regulatory uncertainty across multiple agencies rather than producing pure chaos. Platforms can operate despite ambiguity; users can trade despite ambiguity. This is Rope-like. (2) Extraction function: Ambiguity also enables selective enforcement, discretionary punishment, and incumbent preservation. This is Snare-like. The Tangled Rope classification resolves the mandate: it is not 'pure coordination' (Rope) because there are clear victims (retail investors facing retroactive reclassification). It is not 'pure extraction' (Snare) because platforms and users coordinate successfully on trading despite ambiguity. The mandatrophy is avoided by acknowledging the hybrid nature explicitly in perspectives. The beneficiary (incumbent institutions, regulatory agencies) and victim (retail investors, platforms) declarations make the extraction asymmetry visible. However, if extractiveness exceeded 0.70, additional analysis would be required to determine whether this is (a) a Snare mislabeled as coordination (regulatory ambiguity is entirely extractive, coordination is illusion), or (b) a legitimate Tangled Rope with high extraction cost but genuine coordination benefit. Current data (ε=0.52) supports Tangled Rope classification.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    howey_token_economics_fit,
    'Can the Howey test''s four prongs (investment of money, common enterprise, profits from effort of others, reasonable expectation of profit) be applied deterministically to token economics, or is indeterminacy irreducible?',
    'Comparative case analysis: map Howey applications to specific token types (staking tokens, governance tokens, utility tokens); identify which classes produce consistent regulatory outcomes across jurisdictions',
    'If deterministic: ambiguity is policy choice (agencies could clarify but don''t). If indeterminate: ambiguity is structural to the legal framework. Determines whether constraint is Snare (policy choice to trap) or Mountain (inherent legal limit).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(howey_token_economics_fit, conceptual, 'Whether Howey test admits determinate application to token economics').

omega_variable(
    sec_cftc_jurisdictional_coordination,
    'Are SEC and CFTC genuinely unable to coordinate on asset classification, or is maintained ambiguity instrumentally valuable to at least one agency?',
    'Process tracing: analysis of inter-agency negotiations (FOIA requests, GAO reports); interview data on agency coordination failures vs deliberate turf preservation',
    'If coordination failure: constraint is Tangled Rope with sunset potential (clear legislative delegation could resolve). If deliberate: constraint is Snare (agencies extract value from discretionary enforcement). Changes beneficiary identification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sec_cftc_jurisdictional_coordination, empirical, 'Whether regulatory ambiguity is coordination failure or deliberate agency choice').

omega_variable(
    retail_investor_exit_capacity,
    'What proportion of retail crypto investors can practically exit crypto holdings without prohibitive cost (tax realization, market impact, opportunity cost)?',
    'Survey data: measure cost of exit by investor cohort; identify critical mass threshold above which retail investors shift from trapped to constrained',
    'If majority trapped: powerless perspective (snare) is empirically accurate. If significant exit capacity: moderate perspective (tangled rope) better reflects retail structure. Affects classification of retail agent.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(retail_investor_exit_capacity, empirical, 'Empirical measurement of retail investor exit capacity').

omega_variable(
    incumbent_bank_extraction_magnitude,
    'What is the quantified competitive advantage to incumbent financial institutions from delayed crypto regulation? (Market share preservation, pricing power, time-to-compete)',
    'Economic analysis: measure market share gains by incumbents during periods of regulatory uncertainty vs periods of clarity; estimate pricing power differential',
    'If substantial (>5% margin preservation): incumbent beneficiary classification confirmed. If minimal (<1%): incumbent perspective may be rope without net extraction (coordination only). Changes judgment on whether constraint benefits or merely stabilizes incumbents.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(incumbent_bank_extraction_magnitude, empirical, 'Quantified competitive advantage to incumbents from regulatory ambiguity').

omega_variable(
    platform_compliance_cost_distribution,
    'Do platforms absorb compliance costs internally, or pass them through to users via higher fees/restrictions?',
    'Cost accounting: analyze platform fee structures before/after regulatory actions; model cost pass-through to retail users vs institutional arbitrageurs',
    'If fully absorbed: platforms are victims (tangled rope confirmed). If passed through: users are secondary victims (snare confirmed for retail). Identifies true cost bearer.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(platform_compliance_cost_distribution, empirical, 'Cost distribution mechanism for platform compliance burdens').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(coinbase_regulatory_uncertainty, 0, 12).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cbx_tr_t0, coinbase_regulatory_uncertainty, theater_ratio, 0, 0.35).
narrative_ontology:measurement(cbx_tr_t5, coinbase_regulatory_uncertainty, theater_ratio, 5, 0.5).
narrative_ontology:measurement(cbx_tr_t12, coinbase_regulatory_uncertainty, theater_ratio, 12, 0.64).

% Extraction over time
narrative_ontology:measurement(cbx_be_t0, coinbase_regulatory_uncertainty, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(cbx_be_t5, coinbase_regulatory_uncertainty, base_extractiveness, 5, 0.4).
narrative_ontology:measurement(cbx_be_t12, coinbase_regulatory_uncertainty, base_extractiveness, 12, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(coinbase_regulatory_uncertainty, enforcement_mechanism).
narrative_ontology:affects_constraint(coinbase_regulatory_uncertainty, stablecoin_regulatory_arbitrage).
narrative_ontology:affects_constraint(coinbase_regulatory_uncertainty, defi_securities_classification).
narrative_ontology:affects_constraint(coinbase_regulatory_uncertainty, crypto_custody_fragmentation).

% DUAL FORMULATION NOTE:
% Regulatory ambiguity decomposes into multiple constraint stories: (1) Howey test application to token economics (this story, ε=0.52, Tangled Rope); (2) Stablecoin regulation (separate story, ε>0.60, likely Snare); (3) DeFi securities classification (separate story, ε=0.35, likely Rope); (4) Custody and custody-lite regulation (separate story, ε=0.45, Tangled Rope). Each story has distinct beneficiary/victim structure and measurement timeline. Upstream constraint is SEC vs. Ripple precedent (impacts all downstream classification stories).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(coinbase_regulatory_uncertainty, institutional, 0.12).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
