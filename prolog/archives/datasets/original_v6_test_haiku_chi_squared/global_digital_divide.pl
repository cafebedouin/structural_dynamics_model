% ============================================================================
% CONSTRAINT STORY: global_digital_divide
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_global_digital_divide, []).

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
 *   constraint_id: global_digital_divide
 *   human_readable: Global Digital Divide
 *   domain: economic/infrastructure/governance
 *
 * SUMMARY:
 *   The global digital divide represents a hybrid coordination-extraction
 *   constraint where unequal access to digital technologies and internet
 *   connectivity creates both legitimate infrastructure coordination problems
 *   and systematic extraction mechanisms. The divide emerged from the
 *   combined effects of geographic distance (rural deployment costs),
 *   economic inequality (device and service pricing), and policy choices
 *   (spectrum allocation, subsidy regimes, regulatory capture by incumbent
 *   carriers). The constraint is not a natural law of technology adoption but
 *   a contingent institutional arrangement maintained by suppression
 *   mechanisms (high barriers to entry, monopolistic pricing, lack of
 *   alternative infrastructure) and beneficiary extraction (technology
 *   corporations, ISPs, and high-connectivity regions capture economic value
 *   while costs are borne by low-income populations and least-developed
 *   countries). The theater ratio reflects that development narratives
 *   emphasize universal access aspirations while actual implementation
 *   prioritizes profitable markets. The constraint exhibits the full
 *   perspectival range: for rural populations in developing regions, it is a
 *   Snare (pure extraction with no exit); for international development
 *   initiatives, it is a Scaffold (temporary coordination problem with
 *   explicit sunset); for technology corporations, it is a Rope (legitimate
 *   coordination with network effects); for legacy telecom monopolies, it is
 *   a degraded institutional mechanism; for national governments, it is a
 *   genuine hybrid. The analytical observer risks naturalizing the divide as
 *   inherent to technological diffusion, but the structural data (high
 *   suppression, moderate theater, beneficiary extraction) reveals it as a
 *   policy-contingent arrangement.
 *
 * KEY AGENTS:
 *   - Rural populations in developing regions: Primary victim (powerless/trapped) — bear full cost of infrastructure gaps; lack access to education, employment, financial services, market participation
 *   - Low-income urban populations: Secondary victim (moderate/constrained) — face device costs and bandwidth pricing; derive some coordination benefits from digital platforms
 *   - Technology corporations and ISPs: Primary beneficiary (institutional/arbitrage) — capture network effects, market expansion, and revenue from high-connectivity regions; operate under expansion logic
 *   - International development initiatives: Organized coalition (organized/mobile) — treat divide as temporary problem with sunset targets (SDG 2030); fund infrastructure programs and subsidy models
 *   - National governments: Inter-institutional actor (organized/constrained) — benefit from digital economy growth but bear cost of service deficit; constrained by funding and coordination barriers
 *   - Legacy telecom monopolies: Institutional actor (institutional/constrained) — maintain control through historical infrastructure and regulatory licensing; defend against disruptive competition
 *   - Analytical observer: Civilizational perspective (analytical/analytical) — risks naturalizing contingent institutional arrangements as inherent technological limits
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(global_digital_divide, 0.58).
domain_priors:suppression_score(global_digital_divide, 0.72).
domain_priors:theater_ratio(global_digital_divide, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(global_digital_divide, extractiveness, 0.58).
narrative_ontology:constraint_metric(global_digital_divide, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(global_digital_divide, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(global_digital_divide, tangled_rope).
narrative_ontology:human_readable(global_digital_divide, "Global Digital Divide").
narrative_ontology:topic_domain(global_digital_divide, "economic/infrastructure/governance").

domain_priors:requires_active_enforcement(global_digital_divide).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(global_digital_divide, technology_corporations).
narrative_ontology:constraint_beneficiary(global_digital_divide, high_connectivity_regions).
narrative_ontology:constraint_beneficiary(global_digital_divide, digital_service_providers).
narrative_ontology:constraint_victim(global_digital_divide, low_income_populations).
narrative_ontology:constraint_victim(global_digital_divide, rural_communities).
narrative_ontology:constraint_victim(global_digital_divide, least_developed_countries).
narrative_ontology:constraint_victim(global_digital_divide, digital_economic_participation).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: RURAL POPULATIONS IN DEVELOPING REGIONS (SNARE) — Trapped by infrastructure gaps and cost barriers. Cannot exit digital economy participation without severe economic penalty. Lack of access locks out education, employment, financial services, and market participation. d≈0.92, f(d)≈1.38, σ=1.2 → χ≈0.96.
constraint_indexing:constraint_classification(global_digital_divide, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: LOW-INCOME URBAN POPULATIONS (TANGLED ROPE) — Constrained by device costs and bandwidth pricing. Some access exists but quality and reliability are degraded. Also derive coordination benefits from digital platforms for informal economies, remittances, and social connection. d≈0.68, f(d)≈1.02, σ=1.0 → χ≈0.60.
constraint_indexing:constraint_classification(global_digital_divide, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: TECHNOLOGY CORPORATIONS AND ISPs (ROPE) — Primary beneficiaries. Operate under business model requiring network effects and market expansion. Benefit from infrastructure investment coordination protocols and regulatory frameworks that facilitate market entry. Experience the divide as a coordination problem: connecting new markets requires standard protocols, shared spectrum, and interoperable platforms. d≈0.08, f(d)≈-0.10, σ=1.2 → χ≈-0.07. Net beneficiary.
constraint_indexing:constraint_classification(global_digital_divide, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: INTERNATIONAL DEVELOPMENT INITIATIVES (SCAFFOLD) — Organized coalitions (World Bank, UN, NGOs, public-private partnerships) treating digital divide as temporary coordination failure with explicit sunset logic. See infrastructure investment programs, satellite internet, mesh networks, and subsidy models as transitional pathways toward universal access. Mobile exit options (can shift to alternative funding/implementation models). d≈0.35, f(d)≈0.28, σ=1.2 → χ≈0.19. Low effective extraction because coalition has agency and documented sunset timeline (Sustainable Development Goal 9: universal internet access target 2030).
constraint_indexing:constraint_classification(global_digital_divide, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: LEGACY TELECOM MONOPOLIES (PITON) — Maintain control through historical infrastructure investment and regulatory licensing. Theater ratio reflects that legacy carriers' stated commitment to universal service is largely performative: investment decisions are profit-driven, not access-driven. Access commitment theater persists through regulatory inertia despite market forces favoring private network investment. theater_ratio=0.48 is below piton threshold, but the institutional inertia mechanism is present — these actors maintain extraction mechanisms through degraded coordination rationale (universal service obligation) rather than active enforcement.
constraint_indexing:constraint_classification(global_digital_divide, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: NATIONAL GOVERNMENTS (TANGLED ROPE) — Experience dual structural position. As regulators/planners, they benefit from digital economy growth (tax base, innovation, financial system depth) — coordination function. As servants of low-connectivity populations, they bear extraction costs (service deficit, political pressure, development gap). Constrained exit: cannot withdraw from digital economy without severe economic penalty, but also cannot freely implement universal access without funding/coordination. d≈0.52, f(d)≈0.66, σ=1.0 → χ≈0.38.
constraint_indexing:constraint_classification(global_digital_divide, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (MOUNTAIN VIEW - FALSE SUMMIT) — Risk of naturalizing the divide as inherent to technological diffusion (S-curve adoption, infrastructure cost, capital accumulation rates). From civilizational scale, might appear that digital access follows natural law of economic development. However, structural data (ε=0.58, suppression=0.72) contradicts pure natural law classification. The divide is contingent on policy choices (subsidy models, spectrum allocation, regulatory frameworks), not immutable physical constraints. Engine detects false summit.
constraint_indexing:constraint_classification(global_digital_divide, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(global_digital_divide_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(global_digital_divide, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(global_digital_divide, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(global_digital_divide, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(global_digital_divide, TR),
    TR >= 0.70.

:- end_tests(global_digital_divide_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. Technology corporations and ISPs derive substantial benefits from the divide — they operate in profitable markets while avoiding the cost of universal deployment. The extraction is not total (some universal service mandates, some public funding) but significant. The 30-year trajectory shows increasing extractiveness (0.42→0.58) as the digital economy deepens and wealth concentration in high-connectivity regions accelerates. Suppression (0.72): High. Multiple barriers enforce the divide: (1) infrastructure cost asymmetry (last-mile deployment is orders of magnitude more expensive in rural areas), (2) pricing mechanisms (data plans in developing countries cost 3-5x more as % of income than in developed countries), (3) regulatory capture (spectrum allocation favors incumbents; subsidy programs are underfunded), (4) network effects (applications prioritize large markets, reinforcing concentration), (5) lack of alternative infrastructure (monopolistic control). Theater ratio (0.48): Moderate. Development narratives emphasize universal access commitments and SDG targets, but actual infrastructure investment and pricing decisions are profit-driven. The theater is lower than pure Piton cases because some genuine coordination mechanisms exist (standardization bodies, public-private partnerships, satellite internet competition emerging). However, the performative element is substantial: governments and corporations claim commitment to access while maintaining extraction through pricing and deployment prioritization.
 *
 * PERSPECTIVAL GAP:
 *   The constraint produces maximum perspectival divergence. Rural populations in developing regions see pure Snare — they cannot exit the digital economy without severe economic penalty, yet have no mechanism to demand fair terms. International development initiatives see Scaffold — they document infrastructure programs with 2030 sunset targets and believe the problem is solvable through coordination and investment. Technology corporations see Rope — network expansion into new markets is a legitimate coordination challenge with mutual benefit (access for populations, markets for companies). National governments see the constraint as Tangled Rope — they benefit from digital economy growth but also bear political and development costs from the divide. Legacy monopolies see their own degraded rationale (Piton) — they maintain universal service theater while defending against disruption. The analytical observer risks Mountain classification (natural diffusion law), but the high suppression and clear beneficiary extraction reveal this as a false summit. The perspectival gap is wider than almost any other constraint because the divide has fundamentally different meanings to actors in different structural positions.
 *
 * DIRECTIONALITY LOGIC:
 *   Rural/low-income populations: Victim + trapped → d≈0.92, f(d)≈1.38. Maximal extraction. These populations have no exit option without severe economic cost; they are fully exposed to the constraint's extraction mechanism. Low-income urban populations: Victim + constrained → d≈0.68, f(d)≈1.02. High extraction but not maximal. Some access exists (constrained rather than trapped), and some positive outcomes from platforms (remittances, informal economy). Technology corporations/ISPs: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.10. Net beneficiary. They can choose market entry points and have multiple alternative expansion strategies. International development initiatives: Organized + mobile → d≈0.35, f(d)≈0.28. Low effective extraction. Coalition has agency and documented sunset timeline. National governments: Victim + beneficiary + constrained → d≈0.52, f(d)≈0.66. Symmetric cost-benefit. They benefit from digital economy growth but bear cost of service deficit. Legacy monopolies: Institutional + constrained → d≈0.45, f(d)≈0.50. Moderate extraction defended through inertia (Piton mechanism) rather than active enforcement.
 *
 * MANDATROPHY ANALYSIS:
 *   The global digital divide resolves mandatrophy by showing that the constraint is structurally a Tangled Rope at the systemic level (has both coordination function — network standards, infrastructure interoperability — and asymmetric extraction — device/bandwidth pricing, deployment prioritization). However, the perspectival range is extreme: from the perspective of rural populations (Snare) vs international development coalitions (Scaffold) vs technology corporations (Rope). The mandatrophy resolution is NOT that all perspectives are equally valid, but rather that the system contains all three mechanisms simultaneously. The Rope mechanism (coordination benefits) is real — network effects, standardization, platform access enable genuine value creation. The Snare mechanism (extraction) is equally real — pricing, infrastructure barriers, regulatory capture create systematic exclusion. The Scaffold mechanism (sunset) is real in the framing of development initiatives but often aspirational in actual implementation (SDG 2030 targets are underfunded; technology corporations operate with profit-driven indefinite timelines, not sunset logic). The classification as Tangled Rope (claimed type) asserts that both coordination and asymmetric extraction are essential to understanding the constraint, not that it might be either type depending on observer. Snare classification is the structural reality for powerless agents; Scaffold is the aspirational framing by development coalitions; Rope is the experience of beneficiaries. The engine's job is to measure the actual balance of coordination vs extraction (ε, χ, suppression) and classify accordingly. At ε=0.58, suppression=0.72, this is clearly extraction-dominant Tangled Rope, not pure coordination.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    subsidy_sustainability,
    'Can public subsidy models for rural/low-income digital access achieve cost recovery sustainability, or do they require permanent extraction?',
    'Historical analysis of broadband subsidy programs (USDA RUS, EU broadband funds, Indian BharatNet); measurement of operational cost recovery ratios post-deployment',
    'If sustainable: Scaffold classification confirmed — sunset is real. If extraction persists: becomes Tangled Rope or Snare with theatrical development framing.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(subsidy_sustainability, empirical, 'Whether subsidy models achieve long-term sustainability').

omega_variable(
    satellite_internet_viability,
    'Do satellite internet systems (Starlink, OneWeb, Kuiper) provide genuinely competitive alternative to terrestrial infrastructure, or merely supplement legacy monopolies?',
    'Market penetration analysis; cost-per-Mbps comparison; regulatory approval patterns in developed vs developing markets; customer retention rates in high-latency conditions',
    'If competitive: Rope classification from more perspectives; extraction mechanism weakened. If supplementary: Scaffold sunset delayed; Snare classification persists for remote areas.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(satellite_internet_viability, empirical, 'Whether satellite internet provides genuine competitive alternative').

omega_variable(
    spectrum_allocation_prisoner_dilemma,
    'Is spectrum allocation a pure coordination problem (Rope) or does asymmetric access to spectrum create inherent extraction mechanism (Snare/Tangled Rope)?',
    'Comparative analysis of spectrum allocation policies (ITU framework vs national auctions vs commons-based); measurement of effective access for small/poor operators; tracking of market concentration over time',
    'If coordination problem: extraction is contingent on policy, not structural. If inherent asymmetry: divide is more persistent across policy interventions.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(spectrum_allocation_prisoner_dilemma, empirical, 'Whether spectrum allocation is coordination or extraction').

omega_variable(
    infrastructure_cost_slope,
    'What is the marginal cost of extending connectivity to the next 10% of unconnected population? Does it increase nonlinearly?',
    'Engineering cost analysis; comparative infrastructure projects (last-mile wireless, fiber-to-the-home, satellite); study of geographic/demographic variables affecting deployment cost',
    'If linear or sublinear: divide is policy choice, not natural limit. If superlinear: hard natural constraints emerge; Mountain classification gains credibility.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(infrastructure_cost_slope, empirical, 'Marginal cost curve for extending connectivity').

omega_variable(
    digital_skills_bottleneck,
    'Is infrastructure (the measured divide) the primary constraint on digital participation, or is digital literacy the binding constraint?',
    'Controlled studies comparing infrastructure availability vs literacy outcomes; measurement of device utilization rates in newly-connected populations; tracking of income/employment gains from access alone vs access+training',
    'If infrastructure binding: measured divide is accurate. If skills binding: Snare classification understates the true constraint; there is a hidden secondary divide in competency that infrastructure investment alone does not resolve.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(digital_skills_bottleneck, empirical, 'Whether infrastructure or skills is the binding constraint').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(global_digital_divide, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gdd_tr_t0, global_digital_divide, theater_ratio, 0, 0.35).
narrative_ontology:measurement(gdd_tr_t15, global_digital_divide, theater_ratio, 15, 0.42).
narrative_ontology:measurement(gdd_tr_t30, global_digital_divide, theater_ratio, 30, 0.48).

% Extraction over time
narrative_ontology:measurement(gdd_be_t0, global_digital_divide, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(gdd_be_t15, global_digital_divide, base_extractiveness, 15, 0.51).
narrative_ontology:measurement(gdd_be_t30, global_digital_divide, base_extractiveness, 30, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(global_digital_divide, global_infrastructure).
narrative_ontology:affects_constraint(global_digital_divide, digital_skills_divide).
narrative_ontology:affects_constraint(global_digital_divide, financial_inclusion_gap).
narrative_ontology:affects_constraint(global_digital_divide, educational_access_inequality).
narrative_ontology:affects_constraint(global_digital_divide, labor_market_segmentation).

% DUAL FORMULATION NOTE:
% The global digital divide is downstream of infrastructure investment decisions and spectrum allocation policies, but represents a distinct structural constraint on participation. Multiple constraint stories could decompose this: (1) spectral allocation as coordination problem vs extraction mechanism, (2) device affordability as pricing power vs network effects, (3) ISP monopoly power as natural geography vs regulatory capture. Each decomposition would have different ε values. This story treats the divide holistically at ε=0.58; specific sub-constraints have higher or lower extractiveness depending on which mechanism dominates in particular regions.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(global_digital_divide, institutional, 0.45).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
