% ============================================================================
% CONSTRAINT STORY: us_china_ai_competition
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_us_china_ai_competition, []).

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
 *   constraint_id: us_china_ai_competition
 *   human_readable: US-China AI Competition Constraint
 *   domain: geopolitical/technological/economic
 *
 * SUMMARY:
 *   The US-China AI competition constraint structures access to computational
 *   capacity, semiconductor supply, research collaboration, and technological
 *   standards globally. This is a geopolitical bifurcation mechanism that
 *   coordinates genuine national security objectives while extracting from
 *   research openness, international collaboration norms, and developing
 *   nation access to AI capabilities. The constraint exhibits the full range
 *   of DR classifications depending on structural position: powerless agents
 *   trapped in fragmented research ecosystems see pure extraction (snare);
 *   institutional security apparatuses see pure coordination (rope); moderate
 *   tech companies experience the hybrid (tangled rope); international
 *   governance sees degraded theater (piton); and organized allied coalitions
 *   see temporary barriers with exit paths (scaffold). The theater ratio
 *   (0.58) reflects significant performative content: international AI
 *   governance frameworks appear active but exercise minimal enforcement;
 *   regulatory compliance theater surrounds actual bilateral control
 *   mechanisms; public statements about 'responsible AI competition' mask
 *   actual capacity extraction.
 *
 * KEY AGENTS:
 *   - Academic AI Research Community: Primary victim (powerless/trapped) — faces collaboration freezes, visa restrictions, export controls; cannot exit without abandoning research career
 *   - Developing Nations: Primary victim (powerless/trapped) — dependent on US/Chinese chip access and software ecosystems with no alternative supply chains or indigenous capacity
 *   - Dual-Use Civilian Researchers: Secondary victim (moderate/constrained) — medical AI, climate modeling, drug discovery researchers face research restrictions and compliance overhead
 *   - US National Security Apparatus: Primary beneficiary (institutional/arbitrage) — captures technological dominance, supply chain control, and defense capability coordination
 *   - Chinese Central State Authority: Primary beneficiary (institutional/arbitrage) — achieves technological sovereignty and integrated state AI capacity development
 *   - US Tech Companies: Mixed position (moderate/constrained) — benefit from protected markets and government subsidies but constrained by export regulations and international restrictions
 *   - Chinese AI Research Ecosystem: Mixed position (moderate/constrained) — benefit from state coordination and indigenous development but constrained by chip embargoes
 *   - Allied Coalition (EU, Japan, South Korea, India): Organized secondary actors (organized/mobile) — increasing leverage through alternative partnerships and regional integration
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(us_china_ai_competition, 0.58).
domain_priors:suppression_score(us_china_ai_competition, 0.65).
domain_priors:theater_ratio(us_china_ai_competition, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(us_china_ai_competition, extractiveness, 0.58).
narrative_ontology:constraint_metric(us_china_ai_competition, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(us_china_ai_competition, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(us_china_ai_competition, tangled_rope).
narrative_ontology:human_readable(us_china_ai_competition, "US-China AI Competition Constraint").
narrative_ontology:topic_domain(us_china_ai_competition, "geopolitical/technological/economic").

domain_priors:requires_active_enforcement(us_china_ai_competition).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(us_china_ai_competition, us_defense_industrial_complex).
narrative_ontology:constraint_beneficiary(us_china_ai_competition, chinese_state_ai_capacity).
narrative_ontology:constraint_beneficiary(us_china_ai_competition, semiconductor_export_controllers).
narrative_ontology:constraint_victim(us_china_ai_competition, academic_ai_research_openness).
narrative_ontology:constraint_victim(us_china_ai_competition, developing_nation_ai_access).
narrative_ontology:constraint_victim(us_china_ai_competition, dual_use_civilian_researchers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ACADEMIC RESEARCH COMMUNITY (SNARE) — Trapped by export controls, visa restrictions, and collaboration freezes. Cannot exit participation in the constraint without abandoning international research partnerships. Bears full cost of research fragmentation and knowledge suppression with no exit option.
constraint_indexing:constraint_classification(us_china_ai_competition, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: DEVELOPING NATIONS (SNARE) — Trapped by dependency on US semiconductor access and Chinese manufacturing, with no alternative supply chains. Bears cost of technological bifurcation without capacity to influence outcomes. Maximum suppression through economic dependency.
constraint_indexing:constraint_classification(us_china_ai_competition, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 3: US TECH COMPANIES (TANGLED ROPE) — Constrained by export regulations and national security reviews, but also benefit from government subsidies, protected markets, and research funding. Experience mixed extraction and coordination: genuine coordination around supply chain security coexists with asymmetric extraction of market protection and technology transfer restrictions.
constraint_indexing:constraint_classification(us_china_ai_competition, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: CHINESE AI RESEARCH ECOSYSTEM (TANGLED ROPE) — Constrained by chip supply embargoes and IP restrictions, but benefits from state coordination of resources and integrated development of indigenous AI capacity. Mixed coordination and extraction: genuine coordination of state resources for technological sovereignty coexists with suppression of open research partnerships.
constraint_indexing:constraint_classification(us_china_ai_competition, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: US NATIONAL SECURITY APPARATUS (ROPE) — Primary beneficiary with maximal arbitrage options. Experiences constraint as pure coordination mechanism: organizing defense capability, capital concentration, and supply chain control. Can exit through regulatory shifts and sees the competition as solving genuine coordination problems around national defense. Net extraction flows toward this agent.
constraint_indexing:constraint_classification(us_china_ai_competition, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: CHINESE CENTRAL STATE AUTHORITY (ROPE) — Primary beneficiary with maximal arbitrage options. Experiences constraint as coordinating state capacity and technological sovereignty. Can reshape industrial policy and supply chains. Net extraction flows toward this agent, but framed as coordination of national development.
constraint_indexing:constraint_classification(us_china_ai_competition, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: INTERNATIONAL AI GOVERNANCE FRAMEWORKS (PITON) — UNESCO AI ethics, OECD principles, and multilateral coordination mechanisms persist despite being sidelined by bilateral competition. Theater ratio reflects that formal governance appears active but lacks enforcement capacity. Maintained through institutional inertia; actual AI governance happens through unilateral controls and state coordination, not through international frameworks.
constraint_indexing:constraint_classification(us_china_ai_competition, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 8: ALLIED COALITION (SCAFFOLD) — Organized coalition with increasing mobility and exit options through alternative partnerships (semiconductor partnerships, EU Chips Act, bilateral AI arrangements). See the bifurcation as temporary coordination failure with emerging exit pathways. Low effective extraction because coalition has agency and negotiating leverage, though constrained by larger power asymmetries.
constraint_indexing:constraint_classification(us_china_ai_competition, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 9: ANALYTICAL OBSERVER (TANGLED ROPE) — From civilizational scope, the competition coordinates genuine technological development and strategic positioning while simultaneously extracting from research openness and creating bifurcated technological worlds. Real coordination functions (defense capability, supply chain risk management) coexist with asymmetric extraction (knowledge suppression, market fragmentation). The constraint is neither pure coordination nor pure extraction, but a hybrid system with both functions actively operating.
constraint_indexing:constraint_classification(us_china_ai_competition, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(us_china_ai_competition_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(us_china_ai_competition, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(us_china_ai_competition, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(us_china_ai_competition, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(us_china_ai_competition, TR),
    TR >= 0.70.

:- end_tests(us_china_ai_competition_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The constraint extracts from research openness, international collaboration norms, and developing nation access. However, the extraction is not maximal because genuine security threats justify some restriction, and states have legitimate coordination needs around dual-use technology. The rising trajectory (0.32→0.58) reflects escalating controls from 2020-2024. Suppression (0.65): High. Multiple enforcement mechanisms: ECRA/BIS export controls (US), Catalog of Restricted Imports (China), visa restrictions, publication reviews, collaboration restrictions, standards fragmentation. Barriers to exit are substantial — researchers cannot simply move to alternative ecosystems; nations cannot source chips elsewhere; companies cannot freely operate in both markets. Theater ratio (0.58): Moderate-high. International governance frameworks (UNESCO, OECD, multilateral coordination) are performative — actual control happens through bilateral mechanisms. Regulatory compliance documentation creates theater around what are fundamentally unilateral power plays. Public commitment to 'responsible AI' masks actual capacity denial. The theater has grown as the bifurcation has hardened and institutional legitimacy narratives have developed.
 *
 * PERSPECTIVAL GAP:
 *   The original thesis (2022-2023) positioned US-China AI competition as pure rivalry, generating snare classifications across all perspectives. Updated analysis reveals structured beneficiaries (security apparatus, tech companies, state AI entities) and structured victims (academic researchers, developing nations, dual-use civil sectors). This generates the full perspectival range. The key gap: institutional beneficiaries experience rope (coordination of legitimate defense and sovereignty needs) while powerless victims experience snare (knowledge suppression with no exit). The allied coalition increasingly experiences scaffold (temporary barriers, exit paths through EU Chips Act and regional partnerships). The piton classification for international governance is novel — these frameworks persist but lack enforcement, maintained through institutional habit rather than function.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality derivation follows beneficiary/victim structure. US security apparatus and Chinese state are beneficiaries with arbitrage exit (can reshape policy), producing d ≈ 0.05→0.15, f(d) ≈ -0.12→0.02, low or negative χ. They experience rope. Academic researchers and developing nations are victims with trapped exit, producing d ≈ 0.95, f(d) ≈ 1.42, high χ. They experience snare. Tech companies are both (benefit from protected markets, constrained by controls), producing d ≈ 0.55, f(d) ≈ 0.75, moderate χ. They experience tangled rope. Scope modifier σ(S) amplifies for global scope (1.2): a constraint coordinating defense or supply chain at global scope has higher χ than the same constraint at national scope. This reflects that global bifurcation is harder to verify and easier to hide extraction within. The analytical observer at universal/civilizational scope produces d ≈ 0.72 via canonical fallback, seeing both coordination and extraction functions as structurally active.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint resolves the mandatrophy by decomposing the single 'US-China AI competition' label into its constituent extraction and coordination mechanisms. The genuine coordination components: defense capability, supply chain risk management, technological sovereignty. The extraction components: knowledge suppression, market fragmentation, developing nation exclusion. The classification task is not 'is this rope or snare?' but 'which mechanisms dominate for which agents?' The institutional beneficiaries legitimately coordinate (rope); the powerless victims are legitimately extracted from (snare); the moderate actors experience both (tangled rope). The rising extractiveness trajectory and theater ratio growth suggest that coordination functions motivated initial controls, but extraction has accumulated as the bifurcation has hardened and rent-seeking has layered on top. The apollo variable on extraction_vs_coordination_decomposition is critical: if extraction dominates, all classifications should shift toward snare; if coordination dominates, toward rope. Current data suggests roughly 55% coordination / 45% extraction, justifying tangled_rope as the analytical classification.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    technological_bifurcation_permanence,
    'Is the US-China AI bifurcation temporary geopolitical competition or structural divergence toward incompatible technological ecosystems?',
    'Long-term tracking of interoperability: chip architecture compatibility, software standards convergence, research collaboration recovery timelines. If standards diverge irreversibly, bifurcation is structural; if standards reconverge, competition was temporary.',
    'If temporary: constraint is scaffold with sunset. If structural: constraint is tangled_rope or snare permanently. Determines whether extraction is genuinely necessary security overhead or rent-seeking layered on coordination.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(technological_bifurcation_permanence, empirical, 'Whether AI bifurcation is temporary competition or permanent divergence').

omega_variable(
    extraction_vs_coordination_decomposition,
    'How much of the measured suppression (0.65) reflects genuine national security coordination costs versus political economy extraction?',
    'Comparative analysis: efficiency of US semiconductor controls vs Chinese indigenous capacity development vs theoretical minimum required for national security. Identify rent-seeking premium layers.',
    'If coordination-dominant: suppression ~0.35, snare perspectives downgrade to rope. If extraction-dominant: suppression ~0.75, all perspectives shift toward snare/tangled_rope. Fundamentally changes classification across all perspectives.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraction_vs_coordination_decomposition, empirical, 'Decomposition of suppression into security vs extraction components').

omega_variable(
    knowledge_spillover_asymmetry,
    'Does the bifurcation create asymmetric knowledge spillover favoring one side (industrial espionage, paper leakage, brain drain, reverse engineering)?',
    'Comparative analysis of uncontrolled knowledge flow: publication data, patent citation patterns, talent migration flows, archaeological reverse engineering of commercial chips. Identify which direction knowledge actually flows despite controls.',
    'If spillover favors China: US extraction targets are ineffective, making suppression counter-productive and extractiveness lower. If spillover favors US: US controls work as intended, extractiveness and suppression both justified by actual asymmetry. If symmetric: bifurcation genuinely limits both sides equally.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(knowledge_spillover_asymmetry, empirical, 'Asymmetry in uncontrolled knowledge flow between ecosystems').

omega_variable(
    dual_use_research_harm_quantification,
    'What is the quantified harm from suppressed dual-use AI research (medical imaging, climate modeling, drug discovery) versus security gain from export controls?',
    'Comparative harm assessment: medical AI applications delayed, climate modeling accuracy reduced, drug discovery cycles extended, versus security incidents prevented. Cost-benefit analysis.',
    'If harm >> gain: constraint is net-negative snare even for beneficiaries, suggesting classification should reflect efficiency loss. If gain >> harm: suppression is justified, classification accurate. If balanced: tangled_rope classification confirmed.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(dual_use_research_harm_quantification, empirical, 'Quantified harm from dual-use research suppression vs security gain').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(us_china_ai_competition, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(uschia_tr_t0, us_china_ai_competition, theater_ratio, 0, 0.42).
narrative_ontology:measurement(uschia_tr_t3, us_china_ai_competition, theater_ratio, 3, 0.5).
narrative_ontology:measurement(uschia_tr_t6, us_china_ai_competition, theater_ratio, 6, 0.58).

% Extraction over time
narrative_ontology:measurement(uschia_be_t0, us_china_ai_competition, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(uschia_be_t3, us_china_ai_competition, base_extractiveness, 3, 0.45).
narrative_ontology:measurement(uschia_be_t6, us_china_ai_competition, base_extractiveness, 6, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(us_china_ai_competition, global_infrastructure).
narrative_ontology:affects_constraint(us_china_ai_competition, semiconductor_supply_chain_control).
narrative_ontology:affects_constraint(us_china_ai_competition, ai_talent_migration_restrictions).
narrative_ontology:affects_constraint(us_china_ai_competition, dual_use_export_control_regimes).
narrative_ontology:affects_constraint(us_china_ai_competition, international_ai_governance_fragmentation).

% DUAL FORMULATION NOTE:
% US-China AI competition is upstream of specific export control regimes, semiconductor sourcing constraints, and talent migration restrictions. Each downstream constraint has its own ε reflecting domain-specific extraction; the competition constraint has its own ε reflecting the geopolitical bifurcation mechanism that affects all downstream constraints simultaneously.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(us_china_ai_competition, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
