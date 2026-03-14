% ============================================================================
% CONSTRAINT STORY: real_estate_syndication_structure
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_real_estate_syndication_structure, []).

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
 *   constraint_id: real_estate_syndication_structure
 *   human_readable: Real Estate Syndication Structure
 *   domain: commercial_real_estate/investment_structure
 *
 * SUMMARY:
 *   Real estate syndication structures represent a hybrid
 *   coordination-extraction mechanism that pools capital from passive
 *   investors to acquire and manage properties under professional management.
 *   The constraint exhibits all six DR types from different perspectives.
 *   Syndicators coordinate capital aggregation and property management
 *   (genuine coordination benefits) while capturing asymmetric returns
 *   through carried interest, management fees, acquisition fees, and
 *   refinancing spreads. Passive investors receive diversification and
 *   professional management (coordination benefits) but bear full information
 *   asymmetry, illiquidity lockup costs, and fee structures weighted toward
 *   benefiting the syndicator. The measurement trajectory shows increasing
 *   extractiveness (0.38 → 0.58) and theater ratio (0.42 → 0.58) over the
 *   9-year interval, indicating fee layering and performance capture
 *   mechanisms accumulating over time. The constraint's claimed type is
 *   Tangled Rope: genuine coordination of capital pooling and property
 *   management exists alongside systematic asymmetric extraction through fee
 *   structures, carried interest allocation, and lockup periods.
 *
 * KEY AGENTS:
 *   - Syndicator: Primary beneficiary (institutional/arbitrage) — captures carried interest, management fees, acquisition fees, refinancing spreads; multiple exit options through disposition or fund rolling
 *   - Passive Investors: Primary victim (powerless/trapped) — capital locked in 7-10 year hold periods; information asymmetry regarding property performance and management decisions; bears full extraction cost through fee structures
 *   - Managing Members: Secondary beneficiary (institutional/arbitrage) — receives asset management fees, property-level supervision fees; aligned with syndicator incentives
 *   - Property Development Transparency: Systemic victim (institutional/analytical) — opacity in syndication structures prevents accurate cost-of-capital calculations across real estate markets; capital misallocation effects
 *   - Regulatory Reform Movement: Organized agents (organized/constrained) — SEC, state securities regulators, investor advocates building alternative verification pathways through enhanced disclosure rules
 *   - SEC Disclosure Framework: Institutional actor (institutional/arbitrage) — maintains performative Form D filing requirements that reveal limited information about actual risk and performance
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(real_estate_syndication_structure, 0.58).
domain_priors:suppression_score(real_estate_syndication_structure, 0.65).
domain_priors:theater_ratio(real_estate_syndication_structure, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(real_estate_syndication_structure, extractiveness, 0.58).
narrative_ontology:constraint_metric(real_estate_syndication_structure, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(real_estate_syndication_structure, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(real_estate_syndication_structure, tangled_rope).
narrative_ontology:human_readable(real_estate_syndication_structure, "Real Estate Syndication Structure").
narrative_ontology:topic_domain(real_estate_syndication_structure, "commercial_real_estate/investment_structure").

domain_priors:requires_active_enforcement(real_estate_syndication_structure).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(real_estate_syndication_structure, syndicator).
narrative_ontology:constraint_beneficiary(real_estate_syndication_structure, managing_members).
narrative_ontology:constraint_victim(real_estate_syndication_structure, passive_investors).
narrative_ontology:constraint_victim(real_estate_syndication_structure, property_development_transparency).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PASSIVE INVESTOR (SNARE) — Trapped by capital lock-in, complex waterfall structures, and illiquidity. Cannot exit without severe penalties. Syndication agreements restrict redemption rights, often 7-10 year hold periods. Information asymmetry regarding property performance and management decisions. Bears full extraction cost through fee structures and performance capture.
constraint_indexing:constraint_classification(real_estate_syndication_structure, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: SECONDARY INVESTOR (TANGLED ROPE) — Faces constrained exit through secondary market sales at substantial discounts. Genuine coordination benefit exists (diversification, professional management of pooled capital) but is bundled with asymmetric extraction (carried interest, management fees, GP preferences in waterfall). Some agency through eventual liquidity events but significant costs to early exit.
constraint_indexing:constraint_classification(real_estate_syndication_structure, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: SYNDICATOR (ROPE) — Experiences the constraint as pure coordination: assembling capital, managing properties, distributing returns. Captures carried interest (typically 20% of profits above preferred return), acquisition and management fees. Multiple exit options through refinancing, disposition, or rolling into larger funds. Net beneficiary position.
constraint_indexing:constraint_classification(real_estate_syndication_structure, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: REGULATORY REFORM MOVEMENT (SCAFFOLD) — Organized actors (SEC, state securities regulators, investor advocates) view syndication disclosure rules as temporary coordination failure with sunset logic. Proposed reforms (Rule 506(c) amendments, accredited investor redefinition, real-time performance reporting) represent alternative verification pathways. Constraint perceived as degraded practice with clear reform trajectory over 5-10 year horizon.
constraint_indexing:constraint_classification(real_estate_syndication_structure, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: SEC DISCLOSURE FRAMEWORK (PITON) — Form D filing requirements are substantially performative. Forms reveal only basic facts (property address, offering amount, investor count) but convey little actual risk assessment or performance capability. The filing ritual persists through regulatory convention despite limited transparency function. Theater persists because alternatives (continuous reporting, real-time distribution tracking) haven't fully replaced it institutionally.
constraint_indexing:constraint_classification(real_estate_syndication_structure, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, information asymmetry in real estate syndication appears as an inherent feature of capital markets: passive investors cannot realistically monitor complex properties; professional management requires agent discretion; principal-agent problems are structural to any delegation. However, the structural data contradicts the mountain classification — the engine will identify this as a false summit, revealing that 'inherent to capital markets' naturalizes what is actually a contingent legal and contractual arrangement.
constraint_indexing:constraint_classification(real_estate_syndication_structure, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(real_estate_syndication_structure_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(real_estate_syndication_structure, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(real_estate_syndication_structure, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(real_estate_syndication_structure, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(real_estate_syndication_structure, TR),
    TR >= 0.70.

:- end_tests(real_estate_syndication_structure_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. Syndicators systematically capture value through multiple mechanisms: carried interest (typically 20% of returns above preferred return), acquisition fees (1-2% of purchase price), asset management fees (0.5-1% annually), and refinancing spreads. The measurement trajectory shows accumulation over time, suggesting rent-seeking layering. However, extractiveness is not at snare levels (0.70+) because genuine coordination occurs: pooled capital reduces individual investor risk, professional management enables scale, and some fee structures reflect legitimate service. Suppression (0.65): High. Multiple barriers restrict passive investor exit: (1) illiquidity lockup (7-10 year typical hold periods), (2) information asymmetry (limited disclosure of property-level metrics), (3) secondary market discounts (forced sellers face 15-25% haircuts), (4) regulatory gatekeeping (accredited investor limits). Suppression is structural, not coercive, but functions as effective barrier to exit. Theater ratio (0.55): Moderate. Form D filings and quarterly distribution reports are partially performative — they satisfy regulatory requirements without conveying meaningful risk assessment. Property valuations rely on syndicator appraisals (conflict of interest), returns assume continued capital availability (refinancing risk), and distributions often include return of capital (not earnings). Theater has increased over the interval as complexity of fee structures and performance metrics has expanded.
 *
 * PERSPECTIVAL GAP:
 *   The gap between the syndicator's Rope and the passive investor's Snare reveals the structural extraction mechanism: asymmetric information access (syndicator knows property-level metrics; investor sees only quarterly distributions), asymmetric exit options (syndicator can refinance or sell; investor is locked), and fee structures weighted toward syndicator benefit. The scaffold perspective's sunset logic (regulatory reforms within 5-10 years) suggests the current suppression mechanisms rely on regulatory arbitrage — states with lenient syndication disclosure rules compete for fund registrations, preventing federal harmonization. The piton perspective reveals that SEC Form D filings maintain theater without function. The mountain perspective's false summit reveals that information asymmetry in real estate is not inherent but is enabled by specific legal arrangements: accreditation requirements (gatekeeping), lockup periods (exit restriction), and fee allocation formulas (incentive misalignment).
 *
 * DIRECTIONALITY LOGIC:
 *   Syndicators derive low or negative d (0.15-0.25) from institutional power + arbitrage exit + beneficiary status. They experience effective extraction χ as negative (the constraint subsidizes them). Passive investors derive high d (0.85-0.95) from powerless position + trapped exit + victim status. They experience maximum effective extraction χ. Secondary investors derive moderate d (0.55-0.65) from moderate power + constrained exit + mixed beneficiary/victim relationship. Managing members occupy institutional beneficiary positions similar to syndicators (low d, negative χ). The regulatory reform movement and SEC framework occupy institutional positions with different exit options (constrained, analytical) and mixed victim/beneficiary status, producing moderate d values. The analytical observer's d is derived from canonical value (0.73) and produces high f(d) ≈ 1.15, yet the mountain classification requires emerges_naturally: true and accessibility_collapse ≥ 0.85, which the structural data cannot satisfy — the engine flags this as a false summit.
 *
 * MANDATROPHY ANALYSIS:
 *   REAL ESTATE SYNDICATION RESOLVES THE MANDATROPHY through perspectival mapping. The syndicator's rope (genuine coordination of capital pooling) is real. The passive investor's snare (information asymmetry + illiquidity lockup) is real. The tangled rope (mixed coordination + extraction for secondary investors) is real. The scaffold (regulatory reform with sunset) is real. The piton (performative SEC disclosures) is real. The mountain false summit reveals that naturalizing capital market information asymmetry obscures the specific institutional arrangements (accreditation limits, contractual lockups, fee structures) that create it. The key analytical move is distinguishing (a) coordination benefits that are genuine and would persist under full transparency (professional management, pooled capital diversification) from (b) extractive mechanisms that depend on opacity (fee structures, carried interest allocation, performance metrics defined by syndicators). A reform scenario with transparent property-level reporting, competitive fee benchmarking, and liquid secondary markets would preserve (a) while reducing (b), reclassifying the constraint toward Rope or Scaffold from Tangled Rope. The constraint's extractiveness (0.58) sits in the range where genuine coordination and systematic extraction are difficult to disentangle empirically. The measurement trajectory (increasing extractiveness and theater) suggests extraction is layering onto coordination over time.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    information_asymmetry_reducibility,
    'Can real-time property-level performance reporting and transparent fee disclosure reduce information asymmetry to economically meaningful levels, or is the opacity inherent to the scale and complexity of managing diversified real estate portfolios?',
    'Pilot programs implementing continuous reporting (monthly NAV updates, itemized fee breakdowns, property-level metrics) and measurement of information discovery lag; correlation between reporting frequency and passive investor decision-making behavior',
    'If reducible: syndication constraint reclassifies toward Rope (information as solvable coordination problem). If irreducible: Mountain from analytical perspective confirmed, extractiveness derived from legitimate agent costs rather than opportunistic opacity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(information_asymmetry_reducibility, empirical, 'Whether real estate information asymmetry is reducible through transparency technology').

omega_variable(
    carried_interest_allocation_legitimacy,
    'Is the typical 20% carried interest allocation a fair incentive alignment for syndicator value-add work, or does it constitute extraction above what markets would bear with competitive alternatives?',
    'Comparative analysis of carried interest across syndicator performance quartiles; benchmarking against open-market property management fees; measurement of syndicator value-add vs. market appreciation in similar unmanaged portfolios',
    'If fair alignment: beneficiary relationship to syndicators justified, extractiveness floor raised. If excessive: extraction mechanism confirmed, suppression mechanism shifts toward market concentration (barriers to competitive entry).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(carried_interest_allocation_legitimacy, empirical, 'Whether carried interest allocation reflects fair incentive alignment').

omega_variable(
    illiquidity_lockup_necessity,
    'Do 7-10 year lockup periods genuinely represent the time required for property value stabilization and return realization, or do they primarily serve to capture asymmetric fees during periods when investors cannot exit?',
    'Cohort analysis of actual cash distribution timelines vs. contractual hold periods; measurement of early redemption requests and penalty structures; comparison with REITs (daily liquidity) and their return profiles',
    'If necessary: suppression reclassified as coordination overhead. If primarily extractive: suppression mechanism confirmed as structural barrier to exit, snare classification strengthened.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(illiquidity_lockup_necessity, empirical, 'Whether lockup periods reflect necessary holding periods or extractive design').

omega_variable(
    market_concentration_and_entry_barriers,
    'Do regulatory and capital requirements for syndication (accredited investor limits, minimum investments, legal complexity) genuinely prevent unqualified investor participation, or do they serve as gatekeeping mechanisms protecting incumbent syndicators from competitive pressure?',
    'Analysis of syndication market concentration; measurement of syndicator entry barriers vs. regulatory requirements; correlation between accreditation rules and syndication fee structures over time',
    'If protective: suppression mechanism confirmed as gated access. If regulatory-driven: justification for some suppression legitimate, but may still enable extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(market_concentration_and_entry_barriers, empirical, 'Whether accreditation requirements create protective gatekeeping').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(real_estate_syndication_structure, 0, 9).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(res_tr_t0, real_estate_syndication_structure, theater_ratio, 0, 0.42).
narrative_ontology:measurement(res_tr_t3, real_estate_syndication_structure, theater_ratio, 3, 0.5).
narrative_ontology:measurement(res_tr_t6, real_estate_syndication_structure, theater_ratio, 6, 0.55).
narrative_ontology:measurement(res_tr_t9, real_estate_syndication_structure, theater_ratio, 9, 0.58).

% Extraction over time
narrative_ontology:measurement(res_be_t0, real_estate_syndication_structure, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(res_be_t3, real_estate_syndication_structure, base_extractiveness, 3, 0.48).
narrative_ontology:measurement(res_be_t6, real_estate_syndication_structure, base_extractiveness, 6, 0.55).
narrative_ontology:measurement(res_be_t9, real_estate_syndication_structure, base_extractiveness, 9, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(real_estate_syndication_structure, resource_allocation).
narrative_ontology:affects_constraint(real_estate_syndication_structure, capital_access_inequality).
narrative_ontology:affects_constraint(real_estate_syndication_structure, real_estate_investment_transparency).

% DUAL FORMULATION NOTE:
% Real estate syndication structure decomposes into two distinct constraints: (1) syndication_fee_extraction (ε ≈ 0.65, Snare) — systematic fee capture through multiple mechanisms; (2) capital_pooling_coordination (ε ≈ 0.15, Rope) — genuine coordination benefit of professional management. This story models the hybrid constraint. Upstream constraints include accreditation_gating (regulatory gatekeeping) and lockup_period_necessity (legitimacy of illiquidity). Downstream constraints include capital_misallocation (information asymmetry effects on real estate pricing) and wealth_concentration (syndication access limits to accredited investors).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(real_estate_syndication_structure, institutional, 0.18).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
