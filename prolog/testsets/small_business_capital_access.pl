% ============================================================================
% CONSTRAINT STORY: small_business_capital_access
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_small_business_capital_access, []).

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
 *   constraint_id: small_business_capital_access
 *   human_readable: Small Business Capital Access Constraint
 *   domain: economic/financial/small_business
 *
 * SUMMARY:
 *   Small business capital access creates a structural tension between the
 *   legitimate coordination function of capital allocation and the asymmetric
 *   extraction of terms from entrepreneurs who have no viable alternatives.
 *   The constraint operates through a combination of information asymmetry
 *   (lenders cannot perfectly predict borrower outcomes), collateral
 *   requirements (which concentrate risk-bearing on borrowers), regulatory
 *   standards (which create compliance overhead), and institutional
 *   gatekeeping (which lenders maintain through relationship banking and
 *   credit scoring). This constraint exhibits all six DR types from different
 *   perspectives, making it a diagnostic exemplar for how institutional
 *   extraction disguises itself as coordination. The bootstrapping founder
 *   sees pure extraction (Snare) — no alternatives, trapped by initial
 *   capital requirements that prevent accessing better terms. The community
 *   bank officer sees genuine coordination with embedded extraction (Tangled
 *   Rope) — they do perform the real function of capital allocation while
 *   enforcing standards that systematically disadvantage marginal borrowers.
 *   The institutional lender sees coordination (Rope) — they are solving the
 *   legitimate problem of capital allocation while maintaining informational
 *   advantage. The small business advocacy coalition sees a temporary problem
 *   with emerging sunset (Scaffold) — alternative capital sources are
 *   building pathways that bypass traditional gatekeeping. The credit rating
 *   system sees its own degraded ritual (Piton) — credit scores persist
 *   through regulatory requirement despite limited predictive power for small
 *   business lending. The civilizational analytical observer risks
 *   naturalizing contingent institutional design as inherent to capital
 *   markets (false Mountain). The extractiveness has risen from 0.42 to 0.58
 *   over the 24-year interval, while theater ratio has risen from 0.35 to
 *   0.55, indicating that lenders have intensified gatekeeping while
 *   simultaneously performing compliance-theater that obscures the extraction
 *   mechanism.
 *
 * KEY AGENTS:
 *   - Bootstrapping Founder: Primary victim (powerless/trapped) — bears full cost of capital rationing; no viable exit options without accepting predatory terms
 *   - Underrepresented Entrepreneurs: Primary victim (powerless/trapped) — face additional gatekeeping based on demographic risk proxies and historical lending discrimination
 *   - Institutional Lender: Primary beneficiary (institutional/arbitrage) — captures spread advantage and maintains informational monopoly; net beneficiary of constraint
 *   - Venture Capital Firm: Secondary beneficiary (institutional/arbitrage) — benefits from capital access bottleneck by positioning as alternative (at the cost of equity dilution for entrepreneurs)
 *   - Community Bank Officer: Secondary actor (moderate/constrained) — performs genuine coordination function while enforcing extraction standards; constrained by federal regulations and capital adequacy standards
 *   - Small Business Administration: Organized agent (organized/constrained) — SBA loan guarantees represent explicit sunset logic for traditional bank gatekeeping; constrained by federal budget and regulatory authority
 *   - Credit Rating Agency: Institutional actor (institutional/arbitrage) — maintains scoring infrastructure that legitimates gatekeeping while providing limited predictive power; sees own process as degraded (piton)
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing institutional gatekeeping as inherent information asymmetry
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(small_business_capital_access, 0.58).
domain_priors:suppression_score(small_business_capital_access, 0.68).
domain_priors:theater_ratio(small_business_capital_access, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(small_business_capital_access, extractiveness, 0.58).
narrative_ontology:constraint_metric(small_business_capital_access, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(small_business_capital_access, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(small_business_capital_access, tangled_rope).
narrative_ontology:human_readable(small_business_capital_access, "Small Business Capital Access Constraint").
narrative_ontology:topic_domain(small_business_capital_access, "economic/financial/small_business").

domain_priors:requires_active_enforcement(small_business_capital_access).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(small_business_capital_access, institutional_lenders).
narrative_ontology:constraint_beneficiary(small_business_capital_access, venture_capital_firms).
narrative_ontology:constraint_beneficiary(small_business_capital_access, credit_rating_agencies).
narrative_ontology:constraint_victim(small_business_capital_access, small_business_owners).
narrative_ontology:constraint_victim(small_business_capital_access, underrepresented_entrepreneurs).
narrative_ontology:constraint_victim(small_business_capital_access, competitive_market_efficiency).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: BOOTSTRAPPING FOUNDER (SNARE) — Structurally trapped by collateral requirements and credit history gatekeeping. No viable alternative capital sources without accepting extractive terms (high-interest merchant cash advances, dilutive equity at unfavorable valuations). Suppression is structural: lack of initial capital prevents access to better terms; better terms require capital. Maximum experienced extraction — bears full cost of capital rationing without meaningful agency.
constraint_indexing:constraint_classification(small_business_capital_access, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: COMMUNITY BANK SMALL BUSINESS OFFICER (TANGLED ROPE) — Genuine coordination function: matching local capital with local entrepreneurs, facilitating risk assessment and relationship banking. Also genuine extraction: must follow federal lending guidelines, regulatory reserve requirements, and rating agency standards that systematically disadvantage marginal applicants. Benefits from the constraint through job security and institutional stability; bears costs through high default rates on the marginal loans they're pressured to book. Constrained by federal regulation and capital adequacy standards, but has meaningful discretion within those bounds.
constraint_indexing:constraint_classification(small_business_capital_access, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: INSTITUTIONAL LENDER (ROPE) — Primary beneficiary. Experiences the constraint as pure coordination: capital allocation mechanism that routes deposits to productive borrowers, collects interest, manages risk through standardized processes. Arbitrage options abundant (sovereign debt, securities, derivatives). Net beneficiary — extraction flows toward this agent; they coordinate capital markets while capturing spread advantage. The constraint maintains their informational advantage and institutional position.
constraint_indexing:constraint_classification(small_business_capital_access, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: SMALL BUSINESS ADVOCACY COALITION (SCAFFOLD) — Organized agents (SBA, SCORE, community development financial institutions, small business associations) see capital access barriers as a temporary coordination failure with emerging sunset logic. SBA loan guarantees, microfinance programs, community development financial institutions, and peer-to-peer lending platforms are building alternative pathways that bypass traditional bank gatekeeping. These mechanisms have low suppression because they explicitly accept higher default rates in exchange for broader access. Sunset clause rationale: as alternative capital sources mature (fintech lending, revenue-based financing, equity crowdfunding), traditional bank gatekeeping loses monopoly power. Estimated sunset: 15-25 years as regulatory frameworks catch up to alternative lending.
constraint_indexing:constraint_classification(small_business_capital_access, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: CREDIT RATING SYSTEM (PITON) — Traditional credit scoring for small business lending is substantially performative. Most small businesses lack the financial track record, collateral, and credit history that FICO-equivalent metrics require. Lenders know this — they rely on relationship banking, tax returns, and tacit assessment rather than scores. Yet the rating apparatus persists through institutional inertia: regulatory requirements mandate credit checks; banks maintain scoring infrastructure; the ritual persists despite limited predictive power for marginal borrowers. Theater ratio reflects that credit rating infrastructure is maintained more for regulatory compliance and risk shifting than for actual verification of creditworthiness. The system sees its own process as degraded — required to follow standards that don't actually predict outcomes for small businesses.
constraint_indexing:constraint_classification(small_business_capital_access, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational/universal perspective, some information asymmetry in capital allocation is inherent: lenders cannot perfectly predict borrower outcomes, and the cost of capital discovery is unavoidable. This perspective sees capital rationing as a natural property of financial markets under uncertainty. However, the structural data contradicts the mountain classification — the engine will compute this as a false summit, revealing that 'information asymmetry' naturalizes what is actually a contingent institutional arrangement (standardized credit scoring, regulatory arbitrage requirements, collateral concentration) that could be structured differently.
constraint_indexing:constraint_classification(small_business_capital_access, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(small_business_capital_access_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(small_business_capital_access, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(small_business_capital_access, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(small_business_capital_access, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(small_business_capital_access, TR),
    TR >= 0.70.

:- end_tests(small_business_capital_access_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The constraint extracts through multiple mechanisms: (1) Interest rate spread captures lender advantage (0.15-0.25 basis point premium for small business loans versus corporate); (2) Collateral concentration forces borrowers to pledge personal assets, creating asymmetric loss exposure; (3) Time cost of capital acquisition process (3-6 months for traditional bank lending versus 2-3 weeks for alternative sources) is borne by borrowers, not lenders; (4) Rejection cost (entrepreneurs rejected by traditional banks must pay time cost and reputation cost before accessing alternatives). The 0.58 value reflects that extraction is substantial but not maximal — alternative capital sources are emerging, reducing the lender monopoly. If alternatives were unavailable, extractiveness would be 0.75+. Suppression (0.68): High. Multiple barriers prevent exit: (1) Collateral requirements create stickiness (once pledged, switching costs are high); (2) Credit history gatekeeping (bad credit from one lender makes alternatives more expensive); (3) Information asymmetry prevents borrowers from assessing true terms; (4) Regulatory requirements create compliance overhead that small borrowers cannot absorb, making them dependent on institutional intermediaries. Theater ratio (0.55): Moderate. Credit scoring, compliance documentation, and application procedures are partially performative (limited predictive power for small business outcomes) and partially functional (legitimate risk assessment for some borrower populations). The ratio reflects that traditional banking processes include substantial theater (FICO scores, collateral appraisals, regulatory compliance forms) that legitimates gatekeeping without reliably predicting outcomes. Alternative lending (arXiv-equivalent for capital) reduces this theater by using transaction data, business metrics, and revenue-based underwriting instead.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates the full range of DR classification from a single set of base properties. The bootstrapping founder sees pure extraction (Snare) — they are trapped by collateral and credit history gatekeeping with no viable alternatives at reasonable terms. The community bank officer sees mixed coordination and extraction (Tangled Rope) — the system does allocate capital while enforcing standards that create asymmetric outcomes. The institutional lender sees coordination (Rope) — they are solving the legitimate problem of capital allocation while maintaining informational advantage. The small business advocacy coalition sees a temporary problem with emerging alternatives (Scaffold) — SBA guarantees, microfinance, and fintech lending are building pathways that reduce gatekeeping suppression. The credit rating system sees its own degraded ritual (Piton) — credit scoring persists through regulatory requirement despite limited predictive power for small business lending, where relationship banking and tax returns provide better signals. The civilizational analytical observer risks seeing inherent information asymmetry (Mountain) — capital discovery costs are unavoidable — but the structural data reveals this as naturalization of contingent institutional choices: regulatory arbitrage (federal standards applied strictly by institutions), collateral concentration (institutional choice, not inherent to capital allocation), and credit scoring (regulatory legacy, not optimal for small business assessment).
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is computed from beneficiary/victim declarations and exit options. The bootstrapping founder (powerless/trapped) has d close to 1.0 — they bear maximum extraction because they have no exit. The institutional lender (institutional/arbitrage) has d close to 0.0 — they benefit from the constraint, so f(d) produces negative or near-zero chi for them, meaning experienced extraction runs TOWARD them, not away. The community bank officer (moderate/constrained) has d around 0.65 — moderate exit costs create moderate extraction experience. The analytical observer (analytical/analytical) has d around 0.72 by canonical fallback — the observer sees the full structure from outside any constrained position. The scaffold perspective (organized/constrained) has d around 0.55 — organized agents with some exit paths experience moderate extraction, but they perceive it as temporary because they have agency and see sunset mechanisms working. The piton perspective (institutional/arbitrage) derives d from beneficiary status (the credit rating system benefits from its own persistence through regulatory requirement), resulting in low d and low experienced extraction — which is why piton perspective sees the system as performing its (degraded) function rather than experiencing extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   DIAGNOSTIC EXEMPLAR: This constraint resolves the mandatrophy by showing that all six types are legitimate perspectival readings of the same structural data. The mandatrophy is not 'which type is correct?' but 'which structural position are you measuring from?' The analytical observer's mountain is a false summit (naturalizing contingent institutional design as inherent information asymmetry). The lender's rope is their genuine experience (they do coordinate capital). The scaffold is a real structural feature (alternative capital sources with explicit sunset logic — 15-25 years as fintech and regulatory frameworks mature). The piton is a real observation (credit scoring persists through regulatory inertia despite limited predictive power for small business lending). The snare is the founder's structural reality (trapped by collateral and gatekeeping with no viable alternatives). The tangled rope is the community bank officer's mixed experience (genuine coordination function with embedded extraction standards). No single type is 'the' answer — the presheaf over the observation site IS the answer. The extractiveness trajectory (0.42 to 0.58 over 24 years) and theater ratio trajectory (0.35 to 0.55) show that lenders have intensified gatekeeping (higher chi) while performing compliance-theater (higher theater ratio), indicating Goodhart drift: the constraint's primary function (capital allocation) is being displaced by secondary functions (regulatory compliance, risk-shifting) that generate theater and extraction without coordination benefit.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    information_asymmetry_reducibility,
    'How much of the measured suppression is inherent information asymmetry versus contingent institutional design choices (standardized credit scoring, collateral requirements, relationship banking gatekeeping)?',
    'Comparative analysis of lending outcomes across jurisdictions with different regulatory regimes; longitudinal studies of alternative lending mechanisms (revenue-based financing, peer lending, community development finance) and their default/success rates',
    'If asymmetry is inherent (>70% of suppression): capital access constraint approaches mountain (unavoidable). If asymmetry is contingent (<40% of suppression): constraint is pure Snare/Tangled Rope (institutional design choices, not natural law). Current estimate suggests 45-55% institutional design.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(information_asymmetry_reducibility, empirical, 'Inherence versus contingency of information asymmetry in lending').

omega_variable(
    alternative_capital_substitutability,
    'Do alternative capital sources (SBA loans, microfinance, equity crowdfunding, venture capital, revenue-based financing) actually substitute for traditional bank loans or serve structurally different borrower pools?',
    'Longitudinal tracking of small business financing sources; correlation analysis between alternative capital availability and traditional bank lending terms; exit pattern analysis (do entrepreneurs with access to alternatives exit bank lending pools or accept worse terms)?',
    'If substitutable: scaffold sunset is real — alternative pathways will reduce traditional bank extraction. If complementary: alternative sources serve different niches, leaving traditional gatekeeping extraction intact for the core constrained pool.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_capital_substitutability, empirical, 'Whether alternative capital sources substitute or complement traditional lending').

omega_variable(
    collateral_requirement_functionality,
    'Do collateral requirements actually reduce lender risk or primarily serve as a power-asymmetry mechanism that prevents borrowers from negotiating terms?',
    'Historical analysis of loan performance with and without collateral; comparison of default rates and recovery rates across lending regimes; study of secured versus unsecured lending outcomes when borrower risk profiles are held constant',
    'If functional: collateral is coordination mechanism (justifies Rope classification for lenders). If power mechanism: collateral is pure extraction tool (justifies Snare classification for borrowers). Current evidence suggests mixed: 40% risk reduction, 60% power maintenance.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(collateral_requirement_functionality, empirical, 'Collateral as risk management versus power mechanism').

omega_variable(
    regulatory_arbitrage_extraction,
    'What portion of the measured suppression flows from federal regulatory requirements (capital adequacy, lending standards) versus institutional choice to apply those standards more strictly than required?',
    'Comparison of actual lending requirements versus regulatory minimums; analysis of institutional interpretations of ambiguous standards; study of lending behavior in low-regulation jurisdictions',
    'If regulatory (>60%): constraint is partly Mountain (imposed by federal structure). If institutional (>60%): constraint is pure Snare/Tangled Rope (institutional choice to extract). Current estimate suggests 50-50 split.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_arbitrage_extraction, empirical, 'Regulatory requirement versus institutional choice in suppression').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(small_business_capital_access, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sbca_tr_t0, small_business_capital_access, theater_ratio, 0, 0.35).
narrative_ontology:measurement(sbca_tr_t8, small_business_capital_access, theater_ratio, 8, 0.48).
narrative_ontology:measurement(sbca_tr_t16, small_business_capital_access, theater_ratio, 16, 0.55).
narrative_ontology:measurement(sbca_tr_t24, small_business_capital_access, theater_ratio, 24, 0.6).

% Extraction over time
narrative_ontology:measurement(sbca_be_t0, small_business_capital_access, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(sbca_be_t8, small_business_capital_access, base_extractiveness, 8, 0.5).
narrative_ontology:measurement(sbca_be_t16, small_business_capital_access, base_extractiveness, 16, 0.58).
narrative_ontology:measurement(sbca_be_t24, small_business_capital_access, base_extractiveness, 24, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(small_business_capital_access, resource_allocation).
narrative_ontology:boltzmann_floor_override(small_business_capital_access, 0.18).
narrative_ontology:affects_constraint(small_business_capital_access, venture_capital_equity_dilution).
narrative_ontology:affects_constraint(small_business_capital_access, predatory_merchant_cash_advance).
narrative_ontology:affects_constraint(small_business_capital_access, small_business_failure_rate).

% DUAL FORMULATION NOTE:
% Small business capital access is downstream of regulatory regime (federal lending standards, capital adequacy requirements) but represents a distinct institutional extraction mechanism. The regulatory constraints have their own ε values reflecting policy design; the capital access constraint has its own ε reflecting how institutions apply those regulations. The scaffold perspective's sunset mechanism (alternative capital sources, fintech lending) operates independently of regulatory change, creating parallel pathways that reduce traditional bank gatekeeping extraction.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(small_business_capital_access, institutional, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
