% ============================================================================
% CONSTRAINT STORY: uk_regulatory_capture_banking
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_uk_regulatory_capture_banking, []).

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
 *   constraint_id: uk_regulatory_capture_banking
 *   human_readable: UK Regulatory Capture in Banking
 *   domain: financial_regulation/political_economy
 *
 * SUMMARY:
 *   UK banking regulation exhibits classic regulatory capture: formal
 *   independence of the FCA and Bank of England masks systematic coordination
 *   with incumbent banks to prevent competitive entry, restrict consumer
 *   protections, and enable extraction of surplus from retail depositors and
 *   small business borrowers. The constraint operates through multiple
 *   mechanisms: (1) revolving-door employment cycling between FCA and major
 *   banks creating identity fusion among regulators, (2) industry
 *   consultation on regulatory standards giving banks effective veto power
 *   over rules, (3) enforcement asymmetry whereby major banks face negotiated
 *   penalties while smaller competitors face strict interpretation, (4)
 *   regulatory forbearance on capital requirements for too-big-to-fail
 *   institutions, and (5) post-Brexit regulatory divergence allowing banks to
 *   lobby for weaker UK standards than EU equivalents. The constraint
 *   satisfies the Tangled Rope definition: genuine macroprudential
 *   coordination function (systemic risk management) coexists with asymmetric
 *   extraction (retail depositors and small businesses bear disproportionate
 *   costs while major banks capture regulatory benefits). The theatrical
 *   component (formal consumer protection mandate, independent regulatory
 *   structure) has risen over the measurement interval as the gap between
 *   formal authority and actual enforcement has widened — the FCA maintains
 *   the appearance of consumer advocacy while enforcement intensity against
 *   major banks has declined. The measurement trajectory shows extraction
 *   accumulating from 2010 (post-crisis reform) through 2026, with theater
 *   ratio rising as the institution formalizes its capture through
 *   increasingly elaborate compliance theater.
 *
 * KEY AGENTS:
 *   - Retail Depositors: Primary victims (powerless/trapped) — forced to accept below-market deposit rates and hidden fees; no exit alternative without abandoning financial services entirely
 *   - Small Business Borrowers: Primary victims (powerless/trapped) — credit-dependent and face elevated lending rates; regulatory exemptions allow discriminatory pricing against small firms
 *   - Major UK Banks (HSBC, Barclays, Lloyds, NatWest): Primary beneficiaries (institutional/arbitrage) — capture regulatory design to maintain margin protection and prevent competitive entry; benefit from systemic risk management that stabilizes their assets
 *   - Mid-Tier Institutions: Secondary actors (moderate/constrained) — face compliance burden designed for major banks; high capital requirements relative to scale; constrained by regulatory capture preventing alternative models
 *   - Financial Conduct Authority: Institutional regulator (institutional/constrained) — formally independent but effectively captured; leadership rotates to financial sector employment; enforcement asymmetry reflects institutional constraints rather than formal policy
 *   - Bank of England: Institutional regulator (institutional/constrained) — macroprudential mandate creates genuine coordination function alongside extraction enabling; post-crisis reforms partially offset by forbearance toward major banks
 *   - Consumer Finance Advocates: Organized opponents (organized/mobile) — campaign for regulatory reform; access to global regulatory comparisons (EU, US) provide pressure points; constrained by UK legislative lock-in but mobile at ideological level
 *   - Analytical Observer: Sees structure comprehensively — identifies both genuine macroprudential coordination and asymmetric extraction within same regulatory framework
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(uk_regulatory_capture_banking, 0.58).
domain_priors:suppression_score(uk_regulatory_capture_banking, 0.65).
domain_priors:theater_ratio(uk_regulatory_capture_banking, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(uk_regulatory_capture_banking, extractiveness, 0.58).
narrative_ontology:constraint_metric(uk_regulatory_capture_banking, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(uk_regulatory_capture_banking, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(uk_regulatory_capture_banking, tangled_rope).
narrative_ontology:human_readable(uk_regulatory_capture_banking, "UK Regulatory Capture in Banking").
narrative_ontology:topic_domain(uk_regulatory_capture_banking, "financial_regulation/political_economy").

domain_priors:requires_active_enforcement(uk_regulatory_capture_banking).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(uk_regulatory_capture_banking, major_uk_banks).
narrative_ontology:constraint_beneficiary(uk_regulatory_capture_banking, financial_sector_lobbying_groups).
narrative_ontology:constraint_victim(uk_regulatory_capture_banking, retail_depositors).
narrative_ontology:constraint_victim(uk_regulatory_capture_banking, small_business_borrowers).
narrative_ontology:constraint_victim(uk_regulatory_capture_banking, regulatory_effectiveness).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: RETAIL DEPOSITOR (SNARE) — Trapped within the banking system with no viable alternative. Faces extraction through below-market deposit rates, hidden fees, and regulatory frameworks designed to protect bank margins. Cannot exit without abandoning financial services. Zero degrees of freedom — forced coordination with a system that extracts surplus.
constraint_indexing:constraint_classification(uk_regulatory_capture_banking, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: SMALL BUSINESS BORROWER (SNARE) — Trapped by credit dependency and high switching costs. Faces extraction through elevated lending rates, restrictive covenants, and regulatory exemptions that allow larger firms preferential treatment. Generational timescale shows intergenerational transfer of debt burden. Cannot exit the system without business failure.
constraint_indexing:constraint_classification(uk_regulatory_capture_banking, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 3: MAJOR UK BANKS (ROPE) — Benefit from regulatory arbitrage: regulatory fragmentation between UK and EU post-Brexit allows banks to avoid stricter capital requirements in EU. Immediate horizon reflects quarterly earnings pressure. Experience the constraint as pure coordination — captured regulation coordinates lending behavior to stabilize margins. Net beneficiary position. Low effective extraction from their perspective — the regulatory regime is designed to match their preferences.
constraint_indexing:constraint_classification(uk_regulatory_capture_banking, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: MID-TIER FINANCIAL INSTITUTIONS (TANGLED ROPE) — Constrained by regulatory compliance costs and capital requirements. Experience mixed coordination and extraction: the regulatory framework enables market stability but imposes asymmetric burden. Larger banks face lighter capital requirements per unit of assets (too big to fail premium). Must coordinate with captured regulators but lack the lobbying power of major banks. Significant extraction but not maximal — some agency through alternative banking models.
constraint_indexing:constraint_classification(uk_regulatory_capture_banking, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: FINANCIAL CONDUCT AUTHORITY (PITON) — Regulatory theater masking institutional capture. Formal independence and mandate for consumer protection contrast sharply with actual enforcement patterns: penalties negotiated downward, compliance standards set via industry consultation, leadership revolving door with financial sector. High theater ratio — formal regulatory apparatus persists despite degraded function. FCA sees its own constraints as structural (political pressure, industry lobbying capacity) rather than as evidence of capture. Piton classification: institutional inertia maintaining regulatory theater with minimal functional protection.
constraint_indexing:constraint_classification(uk_regulatory_capture_banking, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: CONSUMER FINANCE ACTIVISTS (TANGLED ROPE) — Organized agents with generational commitment and global scope. Benefit from regulatory transparency norms and international comparisons (EU regulations, US CFPB standards) but constrained by UK regulatory capture blocking reform. Mobile: can exit through advocacy in other jurisdictions or alternative finance models (digital-only banks, credit unions). Experience asymmetric extraction (regulation stacks against consumer advocates) but have agency and exit paths. Generational timescale reflects decades-long reform campaigns with limited success.
constraint_indexing:constraint_classification(uk_regulatory_capture_banking, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (TANGLED ROPE) — Views UK banking regulation as a genuine hybrid: the FCA and Bank of England do coordinate legitimate systemic risk management (post-2008 reforms genuinely improved macroprudential oversight), while simultaneously serving as extraction mechanisms for incumbent banks. The constraint has both coordination and asymmetric extraction components at civilizational scope. The global comparison (EU, US, Canadian regulation) reveals the UK's distinctive feature: regulatory forbearance toward major banks combined with strict rules for smaller competitors. Net assessment: hybrid constraint with both functions present, not a false summit naturalizing extraction as inevitable.
constraint_indexing:constraint_classification(uk_regulatory_capture_banking, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(uk_regulatory_capture_banking_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(uk_regulatory_capture_banking, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(uk_regulatory_capture_banking, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(uk_regulatory_capture_banking, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(uk_regulatory_capture_banking, TR),
    TR >= 0.70.

:- end_tests(uk_regulatory_capture_banking_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. The regulatory capture is not absolute extraction (snare-level ≥0.66) because the FCA and Bank of England do provide genuine macroprudential services — systemic risk genuinely declined after post-2008 reforms, and the regulatory framework does prevent catastrophic failures that would harm even the beneficiary banks. But extraction from retail depositors and small businesses is substantial: estimated at 40-60 basis points annually in suppressed deposit rates plus hidden fee structures. The extractiveness value reflects that roughly half of regulatory benefit accrues to incumbent banks while costs are distributed to depositors/borrowers. Suppression (0.65): High. Multiple mechanisms prevent exit: (1) retail depositors cannot access non-UK banking without significant friction; (2) small businesses have limited credit alternatives (credit unions are undercapitalized, peer-to-peer lending lacks scale for working capital); (3) regulatory structure makes legislative reform extremely difficult (regulator independence insulates from political pressure); (4) information asymmetry prevents depositors from understanding the extraction mechanism. Theater ratio (0.68): High and rising. The FCA's formal mandate is consumer protection; its enforcement record shows the opposite (major banks received £14.4B in fines 2010-2020 but changed no systemic behavior; small compliance failures attract disproportionate attention). The regulatory apparatus has become increasingly theatrical — elaborate compliance frameworks, consumer protection committees, and consumer communication strategies coexist with systematic tolerance of major-bank violations. Theater is rising because the legitimacy gap between formal authority and actual enforcement is widening, requiring increasing performative activity to maintain the fiction of regulation.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits maximum perspectival divergence. The beneficiary banks see pure coordination (Rope) — regulatory certainty and stability of margin protection. The FCA sees its constraints as structural/technical (Piton) — unable to enforce against large institutions without systemic risk. Consumer advocates see extraction with organized opposition (Tangled Rope) — can identify the mechanism but constrained by legislative barriers. Mid-tier institutions see mixed effects (Tangled Rope) — some regulatory benefit but asymmetric burden. Retail depositors see pure extraction with no escape (Snare) — forced participation in system designed to extract surplus. The analytical observer sees the full hybrid (Tangled Rope) — genuine coordination function alongside extraction. The perspectival gap reveals that capture is institutional rather than individual: no single regulator intends capture, but the institutional structure produces it through accumulated incentives.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality derivation follows the beneficiary-victim pipeline. Major banks are beneficiaries with arbitrage exit options (can shift operations to lower-cost EU subsidiaries) — d ≈ 0.20 (low directional pressure, negative chi component). Retail depositors are victims with trapped exit (no banking alternatives) — d ≈ 0.92 (maximum directional pressure, high chi component). Small business borrowers are victims with constrained exit (can exit via business failure but at catastrophic cost) — d ≈ 0.80 (high directional pressure). The FCA as regulator is constrained (formally independent but institutionally captured) — d ≈ 0.55 (moderate pressure reflecting internal tension between formal mandate and actual capture). Analytical observer remains neutral — d ≈ 0.73 by canonical fallback. No overrides are needed because the structural data (who benefits, who loses, what exit options are real) produces consistent directionality. The key insight: major banks' low d (beneficiary status + arbitrage exit) drives negative chi for them, meaning the regulatory structure is experienced as a pure benefit (Rope). But the same structure creates high d for retail depositors (victim status + trapped exit), driving χ > 1.0, which is experienced as pure extraction (Snare). One constraint, six types.
 *
 * MANDATROPHY ANALYSIS:
 *   REGULATORY CAPTURE AS MANDATROPHY EXEMPLAR: The FCA's mandate is consumer protection; its effective function is incumbent-bank stabilization. This creates a mandatrophy: the stated mission (protect consumers from financial harm) contradicts the actual output (protect major banks from competitive pressure and enforcement). The Tangled Rope classification resolves the mandatrophy by accepting both functions as real: (1) macroprudential coordination is genuinely valuable and prevents systemic collapse, and (2) asymmetric extraction is real and harms retail depositors. The resolution is not to choose one function but to recognize that institutional capture has fused incompatible missions. The theater ratio documents the mandatrophy growth: as the gap between stated mandate and actual enforcement has widened, the institution has elaborated performative structures (consumer panels, regulatory consultations, harm-mitigation frameworks) to maintain legitimacy. If the theater ratio continues rising while extractiveness holds stable, the constraint will degrade to Piton (inertial maintenance of regulatory apparatus with minimal function). If extractiveness rises above 0.66 while coordination function declines, it will upgrade to Snare (pure extraction behind regulatory theater). The current trajectory (both rising) suggests the mandatrophy is unresolved — the institution is trying to perform both functions simultaneously, with each becoming increasingly theatrical.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    capture_vs_preference_alignment,
    'Is regulatory behavior driven by industry capture or by genuine alignment of regulator preferences with industry interests?',
    'Counterfactual analysis: compare FCA decisions in cases where industry preferences diverge from systemic stability (e.g., during crisis periods); examine internal FCA communications; track enforcement against major banks vs smaller competitors in identical violations',
    'If capture: classification remains Tangled Rope with high suppression. If preference alignment: may downgrade to Rope (coordination without extraction). Evidence of internal dissent within FCA suggests capture; unanimous pro-bank positions suggest preference alignment.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(capture_vs_preference_alignment, empirical, 'Whether regulatory behavior reflects capture or genuine preference alignment').

omega_variable(
    revolving_door_causality,
    'Does the FCA-financial sector revolving door cause capture or reflect the natural career progression in specialized technical fields?',
    'Longitudinal study of FCA officials'' enforcement records pre- and post-financial sector employment; comparison with revolving door patterns in non-captured regulatory fields (food safety, aviation); measurement of cooling-off period compliance and its correlation with enforcement patterns',
    'If causal capture: strengthens snare classification for victims (regulatory capture prevents exit alternative through legislative reform). If natural progression: suggests capture is organizational rather than individual — the institution itself is captured regardless of personnel.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(revolving_door_causality, empirical, 'Whether revolving door relationship causes regulatory capture').

omega_variable(
    macroprudential_coordination_function,
    'Does the captured regulatory regime still provide meaningful macroprudential coordination (systemic risk reduction) alongside extraction?',
    'Measure crisis frequency and severity pre-regulation (2008 baseline) vs post-regulation (2010-2026); isolate impact of FCA rules from other factors (central bank policy, capital requirements, stress testing); compare UK systemic resilience to less-captured regulators (EU, Canada)',
    'If significant coordination function: Tangled Rope is correct (both coordination and extraction). If minimal coordination: classify as Snare (extraction with theatrical coordination narrative). This directly addresses whether the ''financial stability'' claim is genuine or post-hoc justification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(macroprudential_coordination_function, empirical, 'Whether regulatory capture provides genuine macroprudential coordination').

omega_variable(
    legislative_reform_constraint,
    'Is retail depositor and small business exit permanently constrained by UK legislative/regulatory lock-in, or are alternative financial systems technically available (credit unions, digital banking)?',
    'Map actual alternatives available to trapped agents (EU banking reciprocity post-Brexit status, credit union capacity, digital-only bank availability, peer-to-peer lending); measure adoption rates and barriers to switching; identify whether non-adoption reflects unavailability or active regulatory discouragement',
    'If constrained by law: Snare classification confirmed (trapped by regulatory enforcement). If constrained by convention/cost: may upgrade to Tangled Rope for some agents (constrained rather than trapped). Affects directionality: true trapping raises d → raises chi.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(legislative_reform_constraint, empirical, 'Whether exit alternatives are legally prohibited or economically/conventionally constrained').

omega_variable(
    post_brexit_regulatory_divergence,
    'Has post-Brexit regulatory freedom enabled UK capture to deepen (allowing banks to lobby for weaker rules) or enabled regulatory reform (allowing stricter consumer protections)?',
    'Timeline analysis: measure regulatory stringency (capital requirements, consumer protections, enforcement intensity) in 2020 (pre-divergence) vs 2024 (post-divergence full effect); correlate changes with bank vs consumer advocacy; compare divergence direction to EU regulation trajectory',
    'If capture deepened: extraction increased post-Brexit. If reform occurred: constraint may be loosening. Temporal measurement should show trend. Affects mandatrophy: if constraint is dissolving, reclassify toward Scaffold; if deepening, confirm Snare.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(post_brexit_regulatory_divergence, empirical, 'Whether post-Brexit regulatory divergence deepened capture or enabled reform').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(uk_regulatory_capture_banking, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ukcap_tr_t0, uk_regulatory_capture_banking, theater_ratio, 0, 0.48).
narrative_ontology:measurement(ukcap_tr_t5, uk_regulatory_capture_banking, theater_ratio, 5, 0.58).
narrative_ontology:measurement(ukcap_tr_t10, uk_regulatory_capture_banking, theater_ratio, 10, 0.68).
narrative_ontology:measurement(ukcap_tr_t15, uk_regulatory_capture_banking, theater_ratio, 15, 0.71).

% Extraction over time
narrative_ontology:measurement(ukcap_be_t0, uk_regulatory_capture_banking, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(ukcap_be_t5, uk_regulatory_capture_banking, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(ukcap_be_t10, uk_regulatory_capture_banking, base_extractiveness, 10, 0.58).
narrative_ontology:measurement(ukcap_be_t15, uk_regulatory_capture_banking, base_extractiveness, 15, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(uk_regulatory_capture_banking, enforcement_mechanism).
narrative_ontology:affects_constraint(uk_regulatory_capture_banking, uk_deposit_rate_suppression).
narrative_ontology:affects_constraint(uk_regulatory_capture_banking, uk_small_business_credit_rationing).
narrative_ontology:affects_constraint(uk_regulatory_capture_banking, brexit_regulatory_divergence).

% DUAL FORMULATION NOTE:
% UK regulatory capture in banking decomposes into three downstream constraints with different ε values: (1) deposit rate suppression (ε=0.52, Tangled Rope) — directly extracts from retail depositors through regulatory price-fixing; (2) small business credit rationing (ε=0.61, Snare) — creates artificial scarcity of credit at competitive rates; (3) post-Brexit regulatory divergence (ε=0.48, Tangled Rope) — allows banks to lobby for weaker standards. Each has distinct victims and operates through different mechanisms. The parent constraint (regulatory capture) is the institutional structure enabling all three. Link via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(uk_regulatory_capture_banking, institutional, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
