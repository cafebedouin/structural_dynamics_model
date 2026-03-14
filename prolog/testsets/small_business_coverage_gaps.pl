% ============================================================================
% CONSTRAINT STORY: small_business_coverage_gaps
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_small_business_coverage_gaps, []).

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
 *   constraint_id: small_business_coverage_gaps
 *   human_readable: Small Business Insurance Coverage Gaps
 *   domain: economic/regulatory
 *
 * SUMMARY:
 *   Small business insurance coverage gaps represent a structural tension
 *   between actuarial risk management, regulatory mandates, and economic
 *   viability. The constraint operates across multiple levels: individual
 *   small business owners unable to afford or qualify for coverage, state
 *   regulators attempting to expand access, large insurers optimizing
 *   portfolio composition, and the broader employment-based insurance system
 *   maintaining historical tax-incentive structures. The gap has widened over
 *   the 20-year interval as premiums have increased faster than small
 *   business revenues, even as mandates theoretically expanded coverage. The
 *   theater ratio has remained relatively stable (0.35 to 0.48) but
 *   increased, reflecting growing performative compliance with coverage
 *   mandates while actual access gaps persist. The extractiveness increase
 *   (0.42 to 0.58) reflects both rising premiums and tightening coverage
 *   limits. The constraint exhibits tangled rope structure because genuine
 *   coordination functions (risk pooling, premium sharing) coexist with
 *   asymmetric extraction (disproportionate premiums, coverage exclusions,
 *   profit extraction by large insurers). Different perspectives classify the
 *   gap as immutable natural law (adverse selection), temporary problem
 *   (scaffold sunset), institutional inertia (piton), or pure extraction
 *   (snare), depending on structural position.
 *
 * KEY AGENTS:
 *   - Small Business Owners: Primary victims (powerless/trapped) — legally required to carry coverage but unable to afford or qualify; face legal penalties and financial exposure from uncovered liabilities
 *   - Uninsured Workers in Small Firms: Secondary victims (powerless/trapped) — no access to coverage through employer; face health-related financial catastrophe
 *   - Large Insurers: Primary beneficiaries (institutional/arbitrage) — exclude small business segment and concentrate premiums on profitable segments; arbitrage by exiting unprofitable niches
 *   - State Insurance Commissioners & Advocacy Groups: Organized actors (organized/constrained) — push mandatory coverage expansions and create subsidized pools as alternative pathways with sunset logic
 *   - Employers in Large Firms: Secondary beneficiaries (powerful/mobile) — benefit from stable, underpriced group coverage due to risk pooling with low-risk segments
 *   - Tax Code Architects (Historical): Institutional designers (institutional/arbitrage) — created 1943 exemption for employer-provided coverage; designed system optimizes for large-firm administrative capacity
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(small_business_coverage_gaps, 0.58).
domain_priors:suppression_score(small_business_coverage_gaps, 0.65).
domain_priors:theater_ratio(small_business_coverage_gaps, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(small_business_coverage_gaps, extractiveness, 0.58).
narrative_ontology:constraint_metric(small_business_coverage_gaps, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(small_business_coverage_gaps, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(small_business_coverage_gaps, tangled_rope).
narrative_ontology:human_readable(small_business_coverage_gaps, "Small Business Insurance Coverage Gaps").
narrative_ontology:topic_domain(small_business_coverage_gaps, "economic/regulatory").

domain_priors:requires_active_enforcement(small_business_coverage_gaps).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(small_business_coverage_gaps, large_insurers).
narrative_ontology:constraint_beneficiary(small_business_coverage_gaps, regulatory_agencies).
narrative_ontology:constraint_victim(small_business_coverage_gaps, small_business_owners).
narrative_ontology:constraint_victim(small_business_coverage_gaps, uninsured_workers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: UNINSURABLE SMALL BUSINESS (SNARE) — Structurally trapped by minimum premium requirements, risk pooling logic, and lack of market alternatives. Cannot exit the insurance system entirely (legally required in most states) but cannot afford or qualify for coverage. Bears full cost of the gap: legal penalties, catastrophic financial exposure, inability to contract. Zero degrees of freedom.
constraint_indexing:constraint_classification(small_business_coverage_gaps, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: MARGINAL SMALL BUSINESS (TANGLED ROPE) — Constrained by high premiums and coverage limits but benefits from partial coordination: group purchasing programs, industry pools, state high-risk pools provide some access. Extraction is significant (premiums 2-4x larger peer groups) but not absolute — some exit options exist at substantial cost. Mixed experience of coordination mechanism (risk sharing) with asymmetric extraction.
constraint_indexing:constraint_classification(small_business_coverage_gaps, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: LARGE INSURER (ROPE) — Benefits from coordination of risk pooling across portfolios. Gap in small business coverage is solved by their internal solution: exclude high-risk segments and concentrate on profitable segments. Experiences the constraint as efficient market coordination — separation of risk pools is natural and functional. Arbitrage available through reinsurance and hedging.
constraint_indexing:constraint_classification(small_business_coverage_gaps, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: STATE REGULATOR & MANDATE COALITION (SCAFFOLD) — Organized agents (state insurance commissioners, small business advocacy groups) see the gap as a temporary coordination failure with a sunset: mandatory coverage expansions, small business health exchanges, and subsidized pools create alternative pathways. Sunset clause: as ACA exchanges mature and small group markets expand, traditional gap diminishes. Sees enforcement of coverage mandates as declining pressure once alternatives mature.
constraint_indexing:constraint_classification(small_business_coverage_gaps, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: EMPLOYMENT-BASED INSURANCE SYSTEM (PITON) — The constraint is sustained by institutional inertia: tax incentives for employer-provided coverage (established 1943 via wage controls exemption) create path dependency that benefits large firms with HR infrastructure but excludes small businesses. The system persists through performative compliance with historical incentive structure, not because it efficiently solves the coordination problem. Theater ratio high because the mechanism (tax break) produces coverage gaps while claiming to solve them.
constraint_indexing:constraint_classification(small_business_coverage_gaps, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / ADVERSE SELECTION NATURAL LAW (MOUNTAIN) — From a mathematical/economic perspective, small business insurance gaps reflect an immutable property of insurance markets: adverse selection makes low-risk small businesses subsidize high-risk ones, creating a natural threshold below which pooling breaks down. This perspective sees the coverage gap as inherent to information asymmetry and actuarial mathematics. However, structural data contradicts the mountain classification — gaps persist even where information is symmetric and pooling is feasible, revealing that contingent regulatory/tax arrangements, not natural law, produce the gap.
constraint_indexing:constraint_classification(small_business_coverage_gaps, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(small_business_coverage_gaps_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(small_business_coverage_gaps, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(small_business_coverage_gaps, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(small_business_coverage_gaps, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(small_business_coverage_gaps, TR),
    TR >= 0.70.

:- end_tests(small_business_coverage_gaps_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderately high, reflecting upward trend. The constraint extracts in multiple ways: (1) small businesses pay 2-4x higher per-employee premiums than large firms due to small group rating factors; (2) coverage limits and exclusions shift risk from insurers to small business owners; (3) regulatory compliance costs (documentation, reporting) are fixed and therefore proportionally higher for small firms. The value reflects meaningful extraction without the 0.66+ threshold of pure snare. Suppression (0.65): High. Barriers to exit include: legal mandates in most states requiring coverage, minimum premium thresholds below which insurers will not underwrite, lack of alternative risk mechanisms (self-insurance restricted for small firms), geographic isolation in markets with few underwriters, and information asymmetry (small business owners cannot easily assess true actuarial risk or alternatives). Theater ratio (0.48): Moderate. Some performative content in compliance reporting and risk documentation, but the mechanism is more functional than theatrical — premium structures do respond to actual risk factors, and coverage exclusions do reflect underwriting logic. Not as high as institutional systems relying primarily on ritual maintenance.
 *
 * PERSPECTIVAL GAP:
 *   The uninsurable small business sees pure extraction (snare) — they are trapped with no exit and bear maximum costs. The marginal small business sees mixed coordination and extraction (tangled rope) — they benefit from partial risk pooling but pay extractive premiums. The large insurer sees efficient coordination (rope) — risk segmentation and portfolio optimization are natural market functions solving the coordination problem of heterogeneous risk. The state regulator and mandate coalition see a temporary problem (scaffold) — new exchange mechanisms and subsidized pools create a sunset clause where traditional gaps diminish. The employment-based system sees institutional inertia (piton) — the tax code structure persists through historical path dependence, performing its original function while producing coverage gaps as a side effect. The analytical observer risks seeing immutable natural law (mountain) — adverse selection creates an apparently inevitable gap — but structural data reveals this as naturalization: gaps persist even in symmetric-information contexts and are contingent on regulatory/tax arrangements. The perspectival gap reflects that the same extractive structure appears necessary, efficient, temporary, degraded, or immutable depending on structural position.
 *
 * DIRECTIONALITY LOGIC:
 *   Large insurers experience low or negative directionality (d ≈ 0.15-0.25, beneficiaries with arbitrage) — they have exit options (reinsurance, portfolio diversification, market segmentation) and benefit from the gap (exclude high-risk segments). Small business owners experience high directionality (d ≈ 0.85-0.95, trapped victims) — no exit options, bear full extraction cost. State regulators experience moderate directionality (d ≈ 0.45-0.55, organized actors with constrained exit) — they face political pressure and institutional resistance to mandate expansion but have agency through regulatory tools. The d derivation chain computes effective extraction (chi) from baseline extractiveness (ε=0.58) scaled by these directionality values and scope modifier (σ=1.0 for national): large insurers see χ ≈ ε × f(0.20) × 1.0 ≈ -0.01 (net benefit); small business owners see χ ≈ ε × f(0.90) × 1.0 ≈ 0.75 (high extraction); regulators see χ ≈ ε × f(0.50) × 1.0 ≈ 0.38 (moderate extraction). The directionality structure explains the perspectival gap without additional axes.
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLVED: The tangled rope classification requires both coordination function (beneficiaries present) and asymmetric extraction (victims present). Large insurers benefit from risk pooling (coordination benefit) while small business owners bear disproportionate costs (asymmetric extraction). This is not pure rope (which would require χ ≤ 0.35) nor pure snare (which would require all perspectives to classify as snare). The beneficiary/victim split is structural: those who benefit from market segmentation (large insurers, large-firm employers) are distinct from those who bear the gap (small business owners, uninsured workers). The active enforcement requirement is satisfied: state mandates, coverage requirements, and regulatory oversight actively sustain the structure. The mandatrophy is avoided by recognizing that the apparent naturalness of adverse selection covers a contingent institutional arrangement (tax incentives for employer coverage, licensing requirements favoring large insurers, regulatory barriers to alternative mechanisms).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    adverse_selection_necessity,
    'Is the coverage gap a structural consequence of adverse selection (information asymmetry) or a contingent outcome of regulatory/tax arrangements?',
    'Comparative analysis of small business insurance access across regulatory regimes (US state-to-state, international comparisons); examine markets with symmetric information (mandatory disclosure, health screenings) vs. opaque markets; measure separating equilibria in pooled vs. risk-stratified markets.',
    'If adverse selection is primary: gap is near-universal and policy solutions face fundamental constraints. If regulatory arrangements are primary: gap is contingent and policy leverage is high. Classification hinges on whether the mountain view is accurate (accessibility_collapse ≥ 0.85) or naturalization of contingent arrangements.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(adverse_selection_necessity, empirical, 'Whether coverage gaps are inherent to information asymmetry or contingent on regulatory structure').

omega_variable(
    small_business_risk_distribution,
    'Do small business health risks genuinely cluster in higher-risk categories, or is the apparent risk concentration an artifact of selection bias in underwriting data?',
    'Population-level health surveys comparing uninsured small business owners to insured peers, controlling for self-selection; actuarial analysis of actual claims experience in pools with mandatory small business inclusion vs. voluntary pools; outcome tracking for small businesses forced into high-risk pools.',
    'If risk is genuinely higher: high premiums reflect actuarial reality, extraction is lower than snare classification suggests. If risk distribution is similar: high premiums are extractive markup on symmetrically-distributed risk, supporting snare classification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(small_business_risk_distribution, empirical, 'Whether high small business risk is genuine or selection artifact').

omega_variable(
    sunset_pathway_feasibility,
    'Do state-level insurance exchanges and subsidized small business pools actually reduce coverage gaps to the point where the scaffold sunset is achieved?',
    'Tracking enrollment in ACA small business exchanges (SHOP), subsidized pool utilization, and coverage rates among eligible small businesses over 15-20 year horizon; identification of remaining gaps and whether they reflect new selection pressure or initial gap persistence.',
    'If sunset is feasible: scaffold perspective is structurally sound; gaps are temporary. If sunset fails: scaffold is aspirational; constraints remain snare or tangled rope permanently.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sunset_pathway_feasibility, empirical, 'Whether state exchange expansion achieves coverage gap sunset').

omega_variable(
    tax_incentive_path_dependence,
    'Could the coverage gap be solved by eliminating or redirecting the 1943 employer tax exemption for health insurance, or does path dependence lock in the current system indefinitely?',
    'Policy analysis of alternative incentive structures (individual market subsidies, tax-neutral coverage equivalents, portable benefits); case studies of prior reform attempts and political constraints; modeling of transition costs vs. long-term efficiency gains.',
    'If path dependence is breakable: piton classification is temporary; the institutional inertia can be reformed. If path dependence is locked: piton is indefinite; the tax code constraint sustains the gap structurally.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(tax_incentive_path_dependence, preference, 'Whether tax incentive path dependence is breakable or locked').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(small_business_coverage_gaps, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sbcg_tr_t0, small_business_coverage_gaps, theater_ratio, 0, 0.35).
narrative_ontology:measurement(sbcg_tr_t10, small_business_coverage_gaps, theater_ratio, 10, 0.42).
narrative_ontology:measurement(sbcg_tr_t20, small_business_coverage_gaps, theater_ratio, 20, 0.48).

% Extraction over time
narrative_ontology:measurement(sbcg_be_t0, small_business_coverage_gaps, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(sbcg_be_t10, small_business_coverage_gaps, base_extractiveness, 10, 0.52).
narrative_ontology:measurement(sbcg_be_t20, small_business_coverage_gaps, base_extractiveness, 20, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(small_business_coverage_gaps, resource_allocation).
narrative_ontology:affects_constraint(small_business_coverage_gaps, healthcare_access_inequality).
narrative_ontology:affects_constraint(small_business_coverage_gaps, business_regulatory_compliance_burden).

% DUAL FORMULATION NOTE:
% Small business coverage gaps are downstream of the employment-based insurance system (tax incentive structure) and adverse selection dynamics in risk pooling. Three structurally related but distinct constraints: (1) tax_code_path_dependence (ε≈0.12, Mountain) — the 1943 exemption creates path-dependent lock-in; (2) small_business_coverage_gaps (ε≈0.58, Tangled Rope) — empirical insurance market gap produced by incentive structure; (3) uninsured_worker_health_outcomes (ε≈0.72, Snare) — health consequences of coverage gaps. Each has distinct beneficiaries and victims.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
