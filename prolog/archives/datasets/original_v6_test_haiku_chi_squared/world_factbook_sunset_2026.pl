% ============================================================================
% CONSTRAINT STORY: world_factbook_sunset_2026
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_world_factbook_sunset_2026, []).

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
 *   constraint_id: world_factbook_sunset_2026
 *   human_readable: CIA World Factbook Termination (2026)
 *   domain: geopolitical/information
 *
 * SUMMARY:
 *   The CIA World Factbook, published continuously since 1962, represents a
 *   unique institutional product: a government intelligence agency producing
 *   unclassified, freely available, globally comprehensive reference data on
 *   every nation-state. Its 60+ year existence reflects a Cold War strategic
 *   choice to position U.S. intelligence as the custodian of objective
 *   geopolitical knowledge. In February 2026, CIA Director John Ratcliffe
 *   announced termination, citing budget reallocation to core intelligence
 *   missions within the context of White House-mandated staffing cuts. The
 *   constraint exhibits all characteristics of a Scaffold: a temporary
 *   coordination mechanism with explicit sunset (open alternatives maturing),
 *   moderate extraction (budget redirection), and declining performative
 *   value (soft-power function already attenuated). However, the termination
 *   also creates measurable snare-like extraction for powerless dependent
 *   communities (researchers, NGOs, developing nations), and carries
 *   potential piton characteristics if the soft-power rationale masks
 *   institutional degradation rather than genuine core-mission
 *   prioritization.
 *
 * KEY AGENTS:
 *   - CIA Core Operations: Primary beneficiary (institutional/arbitrage) — redirects Factbook budget to classified intelligence priorities
 *   - Open Intelligence Commons: Primary victim (powerless/trapped) — global researchers, journalists, fact-checkers dependent on single centralized baseline; no alternative with same completeness/accessibility
 *   - Academic and Development Sector: Secondary victim (moderate/constrained) — universities, USAID, World Bank analysts face switching costs to fragmented alternatives
 *   - Foreign Governments and Intelligence Services: Mixed (moderate/constrained) — benefit from U.S.-curated reference; lose public-good access but retain classified intelligence channels
 *   - Alternative Data Ecosystems: Organized beneficiary (organized/mobile) — Wikipedia, Wikidata, CSIS, UN databases accelerate adoption as Factbook exits; have exit pathways and coalition capacity
 *   - U.S. Public Diplomacy Infrastructure: Degraded beneficiary (institutional/arbitrage) — soft-power function residual; termination formalizes already-atrophied institutional role
 *   - White House Executive Oversight: Structural controller (institutional/arbitrage) — staffing mandate drives reallocation; may embed information control motives beneath efficiency rationale
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(world_factbook_sunset_2026, 0.38).
domain_priors:suppression_score(world_factbook_sunset_2026, 0.42).
domain_priors:theater_ratio(world_factbook_sunset_2026, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(world_factbook_sunset_2026, extractiveness, 0.38).
narrative_ontology:constraint_metric(world_factbook_sunset_2026, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(world_factbook_sunset_2026, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(world_factbook_sunset_2026, scaffold).
narrative_ontology:human_readable(world_factbook_sunset_2026, "CIA World Factbook Termination (2026)").
narrative_ontology:topic_domain(world_factbook_sunset_2026, "geopolitical/information").

domain_priors:requires_active_enforcement(world_factbook_sunset_2026).
narrative_ontology:has_sunset_clause(world_factbook_sunset_2026).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(world_factbook_sunset_2026, cia_core_operations).
narrative_ontology:constraint_beneficiary(world_factbook_sunset_2026, classified_intelligence_budget).
narrative_ontology:constraint_victim(world_factbook_sunset_2026, open_intelligence_commons).
narrative_ontology:constraint_victim(world_factbook_sunset_2026, academic_researchers).
narrative_ontology:constraint_victim(world_factbook_sunset_2026, development_organizations).
narrative_ontology:constraint_victim(world_factbook_sunset_2026, journalistic_fact_checking).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: OPEN INTELLIGENCE COMMONS (SNARE) — Global researchers, journalists, NGOs dependent on World Factbook data as freely accessible baseline. No alternative centralized source; cannot exit. d≈0.92, f(d)≈1.38, σ=1.2 → χ≈0.63. Termination extracts knowledge labor already performed and redistributes it as proprietary/paywalled.
constraint_indexing:constraint_classification(world_factbook_sunset_2026, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: ACADEMIC/NGO SECTOR (SNARE) — Universities, USAID, World Bank analysts integrated Factbook into citation pipelines and program baselines. Switching costs high; alternatives (various national sources, UN databases, commercial APIs) are less complete or require expensive aggregation. d≈0.75, f(d)≈1.10, σ=1.2 → χ≈0.50. Moderate extraction with constrained exit.
constraint_indexing:constraint_classification(world_factbook_sunset_2026, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 3: FOREIGN GOVERNMENTS (TANGLED ROPE) — Beneficiaries of Factbook as reliable U.S.-curated reference (reduces information ambiguity, standardizes reporting). Victims of termination (lose low-cost baseline). Also benefits from U.S. intelligence prioritization for bilateral relationships. d≈0.58, f(d)≈0.78, σ=1.0 → χ≈0.30. Mixed: loses public good but retains classified intelligence channel.
constraint_indexing:constraint_classification(world_factbook_sunset_2026, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: CIA CORE OPERATIONS (ROPE) — Primary beneficiary. Redirects ~$2-5M annual Factbook budget to classified collection, analysis, and White House access. Sees termination as pure coordination: reallocating resources to highest-value intelligence products. d≈0.08, f(d)≈-0.10, σ=1.0 → χ≈-0.04. Net beneficiary through budget reallocation.
constraint_indexing:constraint_classification(world_factbook_sunset_2026, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: ALTERNATIVE DATA ECOSYSTEMS (SCAFFOLD) — Wikipedia geodata, Wikidata, CSIS country profiles, UCDP conflict data, World Bank Indicators, UN COMTRADE are building decentralized alternatives. The Factbook termination accelerates adoption of these distributed sources (mobile exit for organized actors). d≈0.35, f(d)≈0.33, σ=1.2 → χ≈0.15. Low effective extraction; coalitions have agency and exit pathways. Sunset: 3-5 years for mature open alternatives to replace Factbook's primary use cases.
constraint_indexing:constraint_classification(world_factbook_sunset_2026, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: U.S. SOFT POWER INFRASTRUCTURE (PITON) — Historically, Factbook was a secondary soft-power tool: presenting U.S. as custodian of objective global knowledge (brand differentiation from Soviet propaganda). Theater_ratio=0.58: the Factbook's soft-power function has already degraded as alternative sources proliferate and U.S. credibility narratives weakened (2010s-2020s). Termination formalizes what is functionally already inert. The 'value to America's international standing' claim is performative — actually residual justification for institutional maintenance, now being dissolved.
constraint_indexing:constraint_classification(world_factbook_sunset_2026, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational/universal view, the Factbook termination risks being naturalized as inevitable cost-cutting or 'core mission focus.' But the structural data (ε=0.38, suppression=0.42, theater=0.58) contradicts mountain classification. The constraint is contingent (policy choice), not inevitable (information economics law). The engine will flag this as a false summit.
constraint_indexing:constraint_classification(world_factbook_sunset_2026, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(world_factbook_sunset_2026_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(world_factbook_sunset_2026, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(world_factbook_sunset_2026, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(world_factbook_sunset_2026, TR),
    TR >= 0.70.

:- end_tests(world_factbook_sunset_2026_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness (0.38): Moderate. The constraint extracts approximately $2-5M annually that was dedicated to open intelligence production and redirects it to classified operations. For users dependent on Factbook, this is genuine extraction of a previously available public good. However, the extraction is limited by existence of imperfect alternatives (not total monopoly like a snare) and explicit sunset pathway (alternative sources maturing, reducing dependence). Suppression (0.42): Moderate. Barriers to exit are significant but not total. Users face switching costs, aggregation labor, and data quality gaps when moving to alternatives — but alternatives do exist. Developing nations and low-resourced organizations face higher suppression than wealthy actors with capital for data infrastructure. Theater ratio (0.58): Moderate-high. The stated rationale ('return to core missions') has genuine plausibility — intelligence agencies do face resource constraints — but also obscures deeper questions about information control and data sovereignty. The soft-power framing (Factbook as U.S. soft power asset) is increasingly performative; the product's actual geopolitical influence has declined as alternative sources proliferated and U.S. intelligence credibility faced sustained challenges (post-2003 Iraq intelligence disputes, post-2016 social media disinformation, post-2020 election challenges).
 *
 * PERSPECTIVAL GAP:
 *   The core perspectival gap is between institutional beneficiaries (CIA core ops, White House) and distributed victims (researchers, NGOs, developing nations). CIA sees coordination: reallocation of resources to higher-value intelligence production. Academic researchers see snare: loss of access to comprehensive baseline data without migration path. Foreign governments see tangled rope: lose public good but retain classified channels. Alternative data coalitions see scaffold: temporary constraint being resolved by open alternatives. U.S. public diplomacy sees piton: soft-power justification is theatrical cover for budget cuts, not genuine strategic investment. The analytical observer risks seeing mountain (inevitable cost-cutting as natural law of budgetary scarcity) but the structural data reveals contingency: this is a policy choice, not a physical law.
 *
 * DIRECTIONALITY LOGIC:
 *   CIA core operations: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.10. Net beneficiary. Budget reallocation concentrates resources toward classified operations with higher institutional value to intelligence leadership. Open intelligence commons: Victim + trapped → d≈0.92, f(d)≈1.38. Maximum extraction. Dependent populations have no equivalent alternative; cannot exit without significant labor and cost. Academic/NGO sector: Victim + constrained → d≈0.70, f(d)≈1.05. High extraction but not maximal; alternatives exist but require switching and aggregation labor. Foreign governments: Beneficiary (public data loss) + victim (intelligence priorities), constrained → d≈0.55, f(d)≈0.75. Mixed, moderate extraction. Alternative data coalitions: Organized agents + mobile → d≈0.30, f(d)≈0.20. Low effective extraction; coalitions have agency and can mobilize substitute capacity. U.S. public diplomacy: Institutional + arbitrage, but functionally degraded → d≈0.05, f(d)≈-0.12. Classified as piton (theater=0.58), not snare, because soft-power function is already atrophied.
 *
 * MANDATROPHY ANALYSIS:
 *   SCAFFOLD WITH SNARE TEMPORALITY: The constraint resolves mandatrophy by showing that Scaffold classification is valid IF AND ONLY IF the sunset mechanism (alternative data sources maturing) functions as predicted. The omega variables measure this directly: alternative source consolidation timeline (omega_id: alternative_source_consolidation_timeline) determines whether users remain trapped (snare reclassifies) or successfully migrate (scaffold confirmed). The theater_ratio (0.58) indicates moderate performativity: the 'core mission' rationale has plausibility but also obscures information control motives. The constraint is NOT a pure coordination problem (rope) because the termination imposes asymmetric costs (powerless actors bear full cost, while CIA benefits). It is also NOT a pure extraction (snare) because organized alternatives are actively building exit pathways. Scaffold classification is therefore conditional: SCAFFOLD if alternative sources reach 80% functional parity within 3-5 years; SNARE if migration stalls and dependence persists. The measurement trajectory shows rising theater_ratio (0.42→0.58) indicating that performative justification is increasing relative to functional value — a piton signature. This suggests the constraint may be transitioning from temporary coordination mechanism (authentic scaffold) toward degraded institutional inertia (piton), with snare-like extraction persisting for immobilized populations. Confidence in sunset mechanism: MEDIUM. Alternative data initiatives (Wikidata, CSIS, UN indicators) are real and maturing, but their adoption by dependent communities (especially in global south) is uncertain.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    alternative_source_consolidation_timeline,
    'How long will it take for decentralized alternatives (Wikipedia, Wikidata, CSIS, UN data) to reach functional parity with Factbook for the 80% use-case coverage that justifies sunset?',
    'Citation tracking of Factbook vs alternative sources in academic papers, policy documents, journalistic reporting; completeness audits of geopolitical baseline coverage; researcher satisfaction surveys',
    'If < 2 years: scaffold sunset is realistic, constraint degrades predictably. If > 5 years: many users remain trapped longer than anticipated, snare classification persists.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_source_consolidation_timeline, empirical, 'Timeline for alternative sources to reach functional parity with Factbook').

omega_variable(
    classified_budget_reallocation_actual_use,
    'Will the redirected Factbook budget ($2-5M annually) actually flow to genuine intelligence collection/analysis, or will it be absorbed into administrative overhead and existing operations?',
    'GAO audit of CIA budget reallocation post-termination; tracking of hiring, collection priorities, and analysis product volume in successor projects',
    'If genuinely reallocated to core ops: rope classification confirmed, beneficiary experience is real. If absorbed into general overhead: beneficiary status unclear, termination may be primarily theater (piton reclassification).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(classified_budget_reallocation_actual_use, empirical, 'Whether redirected budget flows to core intelligence or administrative overhead').

omega_variable(
    information_asymmetry_amplification_scope,
    'Does Factbook termination increase structural dependence of developing nations and global south actors on fragmented proprietary data sources (World Bank, commercial APIs, bilateral intelligence channels)?',
    'Analysis of post-termination data access patterns; comparison of information acquisition costs for LDCs vs developed countries; measurement of policy-relevant data gaps in underrepresented regions',
    'If significant: termination creates new extractive asymmetry (developing nations lose low-cost baseline), snare classification amplifies for global-south actors. If marginal: alternative sources adequately substitute.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(information_asymmetry_amplification_scope, empirical, 'Whether termination increases information asymmetry for developing nations').

omega_variable(
    white_house_staffing_rationalization_genuineness,
    'Is the stated rationale (return to core missions amid staffing cuts) the primary driver, or is it a proxy for deprioritizing open intelligence and realigning information access toward White House political control?',
    'Analysis of other terminated intelligence products/transparency initiatives in same period; comparison to historical precedent for CIA budget reallocation under executive pressure; archival evidence of White House directives',
    'If primary driver: pure efficiency rationale (rope/scaffold logic). If proxy: extraction and information control motive (snare/tangled rope), suggesting deeper constraint than stated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(white_house_staffing_rationalization_genuineness, conceptual, 'Whether staffing cuts rationalize or proxy deeper information control motives').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(world_factbook_sunset_2026, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(wfb_tr_t0, world_factbook_sunset_2026, theater_ratio, 0, 0.42).
narrative_ontology:measurement(wfb_tr_t3, world_factbook_sunset_2026, theater_ratio, 3, 0.5).
narrative_ontology:measurement(wfb_tr_t6, world_factbook_sunset_2026, theater_ratio, 6, 0.58).

% Extraction over time
narrative_ontology:measurement(wfb_be_t0, world_factbook_sunset_2026, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(wfb_be_t3, world_factbook_sunset_2026, base_extractiveness, 3, 0.3).
narrative_ontology:measurement(wfb_be_t6, world_factbook_sunset_2026, base_extractiveness, 6, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(world_factbook_sunset_2026, information_standard).
narrative_ontology:affects_constraint(world_factbook_sunset_2026, intelligence_classification_regime).
narrative_ontology:affects_constraint(world_factbook_sunset_2026, developing_nation_data_infrastructure).
narrative_ontology:affects_constraint(world_factbook_sunset_2026, open_source_intelligence_capacity).

% DUAL FORMULATION NOTE:
% World Factbook termination is downstream of two distinct constraints: (1) U.S. White House budgetary/staffing prioritization (immediate driver), (2) Broader degradation of U.S. public diplomacy soft-power infrastructure (structural context). These are separate constraint stories with different ε values. Factbook termination itself (ε=0.38) represents the coordination/extraction hybrid of a temporary information access mechanism. The budgetary constraint upstream has higher extractiveness; the soft-power degradation is a piton. All three are linked by network causality.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(world_factbook_sunset_2026, institutional, 0.12).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
