% ============================================================================
% CONSTRAINT STORY: s1_visa
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_s1_visa, []).

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
 *   constraint_id: s1_visa
 *   human_readable: SEC S-1 Registration Framework for Initial Public Offerings
 *   domain: economic/political
 *
 * SUMMARY:
 *   The SEC S-1 registration framework represents the primary legal mechanism
 *   by which private companies transition to public ownership in the United
 *   States. Established under the Securities Act of 1933 as a response to
 *   pre-Depression securities fraud, the framework mandates comprehensive
 *   disclosure (typically 50-100 pages of financial, operational, and risk
 *   information) and underwriter gatekeeping before capital can be raised.
 *   This constraint exhibits the tangled-rope signature: it provides genuine
 *   coordination value (information symmetry, fraud prevention, standardized
 *   pricing mechanisms) while simultaneously creating asymmetric extraction
 *   (compliance cost borne by emerging companies; revenue streams captured by
 *   underwriters and securities counsel). The constraint's base
 *   extractiveness (0.38) reflects moderate cost imposition, suppression
 *   (0.52) reflects significant barriers to alternative pathways, and theater
 *   ratio (0.68) indicates that much of the disclosure burden is performative
 *   rather than informationally material. The framework has degraded over its
 *   90-year lifespan: original goal was fraud prevention and investor
 *   protection; contemporary function is partial monopoly enforcement
 *   (keeping capital formation trapped in regulated intermediaries) and
 *   rent-seeking by gatekeepers. Alternative mechanisms (JOBS Act Reg A+
 *   direct listings, SPACs, exempt offerings, blockchain-based offerings) are
 *   emerging as sunset mechanisms, suggesting a scaffold trajectory rather
 *   than eternal necessity.
 *
 * KEY AGENTS:
 *   - Emerging Company Founder: Primary victim (powerless/trapped) — depends on public capital, cannot exit S-1 pathway; bears full compliance cost ($1-3M legal fees, 6-12 month process, disclosure burdens, dilution from underwriter fees)
 *   - Investment Bank Underwriter: Primary beneficiary (institutional/arbitrage) — captures 3-7% of IPO proceeds; experiences framework as efficiency mechanism; incentive to maintain complexity and underwriter gatekeeping
 *   - Securities Counsel Firm: Secondary beneficiary (institutional/arbitrage) — captures $500K-$2M per IPO engagement; billing-hour interests aligned with complexity preservation
 *   - Retail Investor: Secondary victim (moderate/constrained) — benefits from standardized disclosure but cannot access pre-IPO information; bear costs of information asymmetry despite S-1 mandates
 *   - SEC Regulatory Authority: Mixed (organized/constrained) — enforcer with dual mandate: investor protection (coordination) and capital formation enablement (but enforcement creates barriers); constrained by Congressional mandate and resource limits
 *   - Public Equity Market System: Institutional actor (institutional/arbitrage) — S-1 creates path-dependent dominance; alternatives (direct listings, reg-tech) represent functional alternatives, not architectural necessity
 *   - Alternative Capital Formation Coalition: Organized agents (organized/mobile) — JOBS Act signatories, fintech platforms, blockchain infrastructure; building parallel pathways with different risk/return tradeoffs
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(s1_visa, 0.38).
domain_priors:suppression_score(s1_visa, 0.52).
domain_priors:theater_ratio(s1_visa, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(s1_visa, extractiveness, 0.38).
narrative_ontology:constraint_metric(s1_visa, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(s1_visa, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(s1_visa, tangled_rope).
narrative_ontology:human_readable(s1_visa, "SEC S-1 Registration Framework for Initial Public Offerings").
narrative_ontology:topic_domain(s1_visa, "economic/political").

domain_priors:requires_active_enforcement(s1_visa).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(s1_visa, investment_banks_and_underwriters).
narrative_ontology:constraint_beneficiary(s1_visa, securities_counsel_firms).
narrative_ontology:constraint_beneficiary(s1_visa, sec_regulatory_authority).
narrative_ontology:constraint_victim(s1_visa, emerging_company_founders).
narrative_ontology:constraint_victim(s1_visa, small_cap_equity_markets).
narrative_ontology:constraint_victim(s1_visa, information_asymmetry_victims).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: EMERGING COMPANY FOUNDER (SNARE) — Founder cannot exit public markets without losing growth capital access. Trapped by dependence on institutional funding and path-dependent equity structure. Bears full cost of compliance overhead, legal fees (typically $1-3M for S-1 process), and disclosure burdens. d≈0.92, f(d)≈1.38, σ=1.0 → χ≈0.53.
constraint_indexing:constraint_classification(s1_visa, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: RETAIL INVESTOR (TANGLED ROPE) — Benefits from standardized disclosure (coordination function: information symmetry is collective good). Constrained by inability to access pre-IPO shares and limited ability to verify claims independently. Bears information asymmetry costs despite regulatory framework. d≈0.68, f(d)≈1.02, σ=1.0 → χ≈0.39.
constraint_indexing:constraint_classification(s1_visa, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: INVESTMENT BANK UNDERWRITER (ROPE) — Primary beneficiary. S-1 framework creates recurring revenue stream (underwriting fees 3-7% of IPO proceeds). Experiences constraint as coordination: standardized process reduces transaction costs and creates predictable workflow. d≈0.08, f(d)≈-0.09, σ=1.0 → χ≈-0.03. Net beneficiary through arbitrage on compliance premium.
constraint_indexing:constraint_classification(s1_visa, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: SECURITIES COUNSEL FIRM (ROPE) — Secondary beneficiary. S-1 complexity drives recurring billable hours ($500K-$2M per IPO engagement). Experiences framework as coordination: standardized legal requirements create predictable service delivery. d≈0.05, f(d)≈-0.12, σ=1.0 → χ≈-0.04. Negative effective extraction = structural incentive to maintain complexity.
constraint_indexing:constraint_classification(s1_visa, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: SEC REGULATORY AUTHORITY (TANGLED ROPE) — Primary enforcer. Framework simultaneously provides: (a) coordination function (standardized disclosure prevents fraud and information asymmetry), (b) extraction function (regulatory moat: high compliance cost excludes competitors and entrenches incumbent players). Constrained by Congressional mandate (securities act) and institutional capability limits. d≈0.45, f(d)≈0.48, σ=1.0 → χ≈0.18. Mixed: coordination mission vs extraction side-effect.
constraint_indexing:constraint_classification(s1_visa, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: PUBLIC EQUITY MARKET SYSTEM (PITON) — S-1 regime is institutionally entrenched but functionally degraded. Theater_ratio=0.68: much of the disclosure process is performative (boilerplate risk factors, executive compensation tables, audited financial statements that retail investors cannot meaningfully interpret). Persists through institutional inertia and lack of realistic alternatives. Primary function (capital formation) achievable through multiple mechanisms (JOBS Act Reg A+, direct listings), but S-1 dominates through regulatory path-dependency. d≈0.08, f(d)≈-0.09, σ=1.0 → χ≈-0.06.
constraint_indexing:constraint_classification(s1_visa, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 7: ALTERNATIVE CAPITAL FORMATION COALITION (SCAFFOLD) — JOBS Act (2012) and subsequent reg-tech innovations (Reg A+, direct listings, SPACs) represent sunset mechanisms to S-1 hegemony. Organized agents (fintech platforms, blockchain infrastructure, exempt offerings market) are building parallel paths. S-1 seen as transitional requirement declining in relative importance. d≈0.35, f(d)≈0.32, σ=1.2 → χ≈0.12. Low effective extraction because alternatives exist and are scaling.
constraint_indexing:constraint_classification(s1_visa, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER / FALSE SUMMIT VIEW (MOUNTAIN) — Risk of naturalizing S-1 as inherent necessity: 'any securities offering requires disclosure, therefore S-1 is inevitable.' But base properties (ε=0.38, suppression=0.52, theater=0.68) contradict mountain classification. This is a contingent institutional arrangement (1933 legislation), not a law of nature. Disclosure requirements are necessary; this specific framework is not.
constraint_indexing:constraint_classification(s1_visa, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(s1_visa_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(s1_visa, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(s1_visa, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(s1_visa, TR),
    TR >= 0.70.

:- end_tests(s1_visa_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness (0.38): Moderate, reflecting asymmetric cost imposition and underwriter rent-seeking, tempered by genuine information standardization benefit. The 1933 framework imposed real coordination value (fraud prevention post-Depression); contemporary value is mixed with extraction. Compliance cost ($1-3M legal fees, 6-12 month process drag, underwriter fee grab) is borne entirely by emerging company (founder dilution). Suppression (0.52): Moderate-high. Significant barriers include: (1) regulatory monopoly on S-1 pathway (SEC gatekeeping), (2) underwriter distribution network lock-in, (3) Sarbanes-Oxley section 404 auditing requirements imposing permanent recurring cost post-IPO, (4) liability exposure under securities laws discouraging alternative disclosure formats. But suppression is not total — JOBS Act opened alternative pathways (Reg A+ up to $75M, direct listings, emerging growth company exemptions). Theater ratio (0.68): Moderately high. S-1 disclosure is substantially performative: (a) boilerplate risk factor sections read identically across industries ('we may face competition,' 'macroeconomic downturns affect us'), (b) executive compensation tables with formatting prescribed to decimal point, (c) MD&A (Management Discussion & Analysis) narrative designed to comply with form rather than inform, (d) audited financial statements inaccessible to retail investor interpretation. The theater has increased from 1933 baseline (when S-1 was genuinely novel information) to contemporary form (when templates dominate).
 *
 * PERSPECTIVAL GAP:
 *   This constraint reveals a stark perspectival gap between beneficiaries and victims. The founder (powerless/trapped) experiences maximum extraction: cannot access capital without paying gatekeepers' rents, cannot choose simpler disclosure, must absorb $1-3M cost. The underwriter (institutional/arbitrage) experiences pure coordination: standardized process, predictable fee structure, regulatory moat protecting distribution monopoly. The analyst observer risks seeing a Mountain ('disclosure is inherent to public offerings') when the structural data (ε=0.38, suppression=0.52, theater=0.68) reveals a contingent institutional arrangement. The scaffold perspective (alternative capital formation coalition) introduces the critical variable: if JOBS Act Reg A+, direct listings, and blockchain-based offerings can achieve 80%+ of S-1's informational goal with 40-50% lower cost, then S-1 is not inevitable but rather a degraded piton persisting through institutional inertia. The retail investor's tangled-rope perspective is crucial: they benefit from standardized disclosure (coordination) but remain exposed to information asymmetry because S-1 disclosure is not optimized for their comprehension — it is optimized for legal liability protection and underwriter gatekeeping.
 *
 * DIRECTIONALITY LOGIC:
 *   Founder (emerging company): Victim + trapped → d≈0.92, f(d)≈1.38. Maximum extraction. Cannot exit, cannot choose alternative disclosure, must pay rent to gatekeepers. Underwriter: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.09. Net beneficiary. Controls distribution network, extracts 3-7% of proceeds, experiences regulatory monopoly as profit protection. Securities counsel: Beneficiary + arbitrage → d≈0.05, f(d)≈-0.12. Net beneficiary. Billing-hour interests aligned with complexity. Retail investor: Victim + constrained → d≈0.68, f(d)≈1.02. Constrained by information access and analysis capacity despite S-1 framework. SEC regulator: Mixed + constrained → d≈0.45, f(d)≈0.48. Dual mandate (investor protection + capital formation) creates conflicted structural position. Alternative capital formation coalition: Organized + mobile → d≈0.35, f(d)≈0.32. Mobile through regulatory arbitrage (Reg A+, direct listings) and emerging tech infrastructure.
 *
 * MANDATROPHY ANALYSIS:
 *   TANGLED ROPE CLASSIFICATION RATIONALE: S-1 framework exhibits both genuine coordination function AND asymmetric extraction, satisfying the tangled-rope gate. Coordination function: standardized disclosure reduces information asymmetry, prevents fraud, enables price discovery. This is a real collective good (Rope component). Asymmetric extraction: compliance cost ($1-3M) borne entirely by emerging company founder; benefit (underwriter fees 3-7%, counsel fees $500K-$2M) captured by gatekeepers. This is real extraction rent-seeking (Snare component). Active enforcement: SEC enforcement of S-1 compliance (comment letters, deficiency notices, enforcement actions against fraudulent disclosures) is required to maintain the framework. The tangled-rope classification avoids two mandatrophy errors: (1) false Rope: calling this pure coordination ignores the asymmetric cost burden, underwriter rent-seeking, and theater, (2) false Snare: calling this pure extraction ignores the genuine informational benefit and fraud prevention achieved. The scaffold perspective (alternative capital formation coalition) introduces a sunset mechanism: as JOBS Act alternatives scale and blockchain-based offerings mature, the S-1 monopoly decays — this is the critical test of scaffold validity. If alternatives remain marginal (< 5% of IPO volume) after 10 years, S-1 is structural necessity (tangled rope persists); if alternatives reach 30-40% volume, S-1 transitions to piton (degraded, inertial, maintained by path-dependency rather than necessity).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    optimal_disclosure_standard,
    'What disclosure threshold minimizes information asymmetry harm without imposing excessive compliance cost on issuer and market?',
    'Comparative analysis of SEC filings (S-1 dense disclosure) vs JOBS Act Reg A+ (simplified) vs international regimes (EU prospectus rules); measurement of retail investor comprehension rates; correlation between disclosure granularity and investment error rates',
    'If S-1 standard is sub-optimal (theater > 0.70): compliance cost extraction exceeds informational benefit. If S-1 is optimal: framework is Rope, not Tangled Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(optimal_disclosure_standard, empirical, 'Whether S-1 disclosure threshold is informationally optimal').

omega_variable(
    underwriter_gatekeeping_necessity,
    'Does the underwriter role (risk assessment, reputation collateral, distribution network) provide genuine coordination value or function as a pure extraction rent-seeking gate?',
    'Historical analysis of underwriter selection effects on post-IPO performance; measurement of underwriter due-diligence quality (correlation between underwriter-assessed risk and realized defaults); comparison of IPO success rates in regimes with vs without underwriter gatekeeping (e.g., direct listings, SPAC mergers)',
    'If underwriter adds genuine value: S-1 framework is justified coordination mechanism (Rope dominant). If underwriter is rent-seeking gate: S-1 is primarily extraction mechanism (Snare dominant).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(underwriter_gatekeeping_necessity, empirical, 'Whether underwriter gatekeeping provides coordination value or enables extraction').

omega_variable(
    regtech_capital_formation_sufficiency,
    'Can decentralized disclosure systems (blockchain smart contracts, real-time filing, algorithmic verification) achieve the information-asymmetry reduction goal of S-1 with lower compliance cost and less theater?',
    'Pilot programs with blockchain-based offering disclosure; measurement of cost reduction vs S-1; measurement of investor protection outcomes; regulatory acceptance pathway testing',
    'If sufficient: scaffold sunset is real and S-1 is genuinely temporary. If insufficient: S-1 is structural necessity and scaffold perspective is aspirational.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(regtech_capital_formation_sufficiency, empirical, 'Whether regtech can replace S-1 framework').

omega_variable(
    information_asymmetry_severity,
    'In pre-offer environments, what fraction of IPO failure is attributable to information asymmetry (disclosure inadequacy) vs principal-agent misalignment or market fundamentals?',
    'Econometric decomposition of IPO performance drivers; measurement of IPO flops caused by hidden liabilities vs incorrect valuation vs market conditions; post-hoc analysis of whether S-1 disclosure would have prevented failures',
    'If information asymmetry is dominant failure mode: S-1 is Rope (coordination). If minority: S-1 is Snare (suppression of alternatives, minor info gain).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(information_asymmetry_severity, empirical, 'Fraction of IPO failure attributable to information asymmetry').

omega_variable(
    barrier_to_entry_level,
    'What is the minimum capital/company scale threshold below which S-1 compliance cost becomes prohibitive (i.e., suppression ≈ 1.0)?',
    'Survey of founders on cost perception; measurement of IPO frequency by company size; analysis of Reg A+ adoption as function of cost differential; threshold modeling',
    'If threshold is low ($50M revenue): suppression is high for most issuers, supporting Snare classification. If high ($500M+): suppression is moderate, supporting Tangled Rope.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(barrier_to_entry_level, empirical, 'Capital threshold below which S-1 compliance becomes prohibitive').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(s1_visa, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(visa_tr_t0, s1_visa, theater_ratio, 0, 0.52).
narrative_ontology:measurement(visa_tr_t25, s1_visa, theater_ratio, 25, 0.62).
narrative_ontology:measurement(visa_tr_t50, s1_visa, theater_ratio, 50, 0.68).

% Extraction over time
narrative_ontology:measurement(visa_be_t0, s1_visa, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(visa_be_t25, s1_visa, base_extractiveness, 25, 0.33).
narrative_ontology:measurement(visa_be_t50, s1_visa, base_extractiveness, 50, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(s1_visa, enforcement_mechanism).
narrative_ontology:affects_constraint(s1_visa, underwriter_gatekeeping_monopoly).
narrative_ontology:affects_constraint(s1_visa, sarbanes_oxley_section_404_compliance).
narrative_ontology:affects_constraint(s1_visa, information_asymmetry_in_capital_markets).

% DUAL FORMULATION NOTE:
% S-1 framework decomposes into two structurally distinct constraint stories: (1) disclosure_standardization_coordination (ε≈0.12, Rope) — the genuine informational benefit of standardized financial and operational disclosure; (2) visa_ipo_regulatory_compliance (ε=0.38, Tangled Rope) — the institutional arrangement coupling disclosure with underwriter gatekeeping and compliance cost barriers. The framework conflates these claims. S-1 as pure disclosure mechanism is Rope; S-1 as regulatory monopoly on capital formation is Tangled Rope. The extraction component emerges from the coupling to underwriter gatekeeping and the suppression of alternatives.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(s1_visa, institutional, 0.45).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
