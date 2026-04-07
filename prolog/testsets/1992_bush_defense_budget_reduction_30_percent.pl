% ============================================================================
% CONSTRAINT STORY: 1992_bush_defense_budget_reduction_30_percent
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_1992_bush_defense_budget_reduction_30_percent, []).

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
 *   constraint_id: 1992_bush_defense_budget_reduction_30_percent
 *   human_readable: 1992 Bush Defense Budget Reduction Constraint (30% by 1997, $50B additional savings)
 *   domain: economics/fiscal_policy/military_industrial
 *
 * SUMMARY:
 *   The 1992 Bush defense budget reduction constraint establishes a mandatory
 *   30% cut by fiscal year 1997 with an explicit additional $50 billion in
 *   savings, framed as the 'peace dividend' from Cold War's end. The
 *   constraint is bounded ('This deep, and no deeper'), creating a firm
 *   fiscal target while reserving substantial defense capacity. The structure
 *   generates cross-cutting beneficiaries (general taxpayers, domestic
 *   priorities) and victims (defense contractors, regional defense-industrial
 *   employment, military readiness if the cut is unsustainably deep). This
 *   creates the core mandatrophy: the constraint simultaneously coordinates a
 *   coherent post-Cold War fiscal adjustment AND extracts costs
 *   asymmetrically from defense-dependent regions and contractors. The
 *   constraint is not pure extraction (snare) because it enables real
 *   reallocation and is presented as a collective fiscal commitment. It is
 *   not pure coordination (rope) because the asymmetry is severe and
 *   concentrated. It is a tangled rope hybrid: genuine coordination function
 *   (fiscal discipline, explicit boundaries) combined with asymmetric
 *   extraction (costs concentrated on defense sectors and workers).
 *
 * KEY AGENTS:
 *   - General Taxpayers / Domestic Priority Beneficiaries: (moderate/mobile) — experience constraint as pure coordination enabling reallocation to infrastructure, education, healthcare; benefit from both constraint and downstream spending
 *   - Executive Branch / OMB: (institutional/arbitrage) — primary beneficiary; uses constraint as credible commitment device; has significant maneuvering room within 30% boundary through accounting and contract timing
 *   - Major Defense Contractors: (powerful/constrained) — primary victims; powerful enough to lobby and diversify but face mandatory revenue cuts; can arbitrage but cannot exit federal market
 *   - Defense Industry Workers: (powerless/trapped) — secondary victims; locked into geographic dependence on defense contracts and specialized skills; no alternative labor markets in regional clusters; most vulnerable to extraction
 *   - Military Leadership / Joint Chiefs: (organized/constrained) — experience coordination (force structure planning) and extraction (readiness capacity threat); retain organizational voice but bound by civilian mandate
 *   - Industrial Reconversion Coalition: (organized/constrained) — manage transition through diversification programs and worker retraining; have agency through policy but constrained by timeline and resource limitations
 *   - Cold War Institutional Framework: (civilizational perspective) — constraint represents degraded institutional response to genuine structural change; theater ratio reflects partial mismatch between budget reduction and actual force rationalization
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(1992_bush_defense_budget_reduction_30_percent, 0.52).
domain_priors:suppression_score(1992_bush_defense_budget_reduction_30_percent, 0.48).
domain_priors:theater_ratio(1992_bush_defense_budget_reduction_30_percent, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(1992_bush_defense_budget_reduction_30_percent, extractiveness, 0.52).
narrative_ontology:constraint_metric(1992_bush_defense_budget_reduction_30_percent, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(1992_bush_defense_budget_reduction_30_percent, theater_ratio, 0.38).

% --- Constraint claim ---
narrative_ontology:constraint_claim(1992_bush_defense_budget_reduction_30_percent, tangled_rope).
narrative_ontology:human_readable(1992_bush_defense_budget_reduction_30_percent, "1992 Bush Defense Budget Reduction Constraint (30% by 1997, $50B additional savings)").
narrative_ontology:topic_domain(1992_bush_defense_budget_reduction_30_percent, "economics/fiscal_policy/military_industrial").

domain_priors:requires_active_enforcement(1992_bush_defense_budget_reduction_30_percent).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(1992_bush_defense_budget_reduction_30_percent, general_taxpayers).
narrative_ontology:constraint_beneficiary(1992_bush_defense_budget_reduction_30_percent, domestic_budget_priorities).
narrative_ontology:constraint_victim(1992_bush_defense_budget_reduction_30_percent, defense_contractors).
narrative_ontology:constraint_victim(1992_bush_defense_budget_reduction_30_percent, military_industrial_employment).
narrative_ontology:constraint_victim(1992_bush_defense_budget_reduction_30_percent, military_readiness_capacity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DEFENSE INDUSTRY WORKERS (SNARE) — Trapped by geographic dependence on defense contracts, specialized skill sets, and lack of alternative employment in single-industry regions. The 30% reduction mandate imposes forced job losses with minimal exit options. Regional defense clusters (Southern California aerospace, St. Louis weapons systems, Connecticut submarines) cannot exit the constraint. Maximum experienced extraction — no alternative labor markets, no transferable skills in tight timeline, suppressed wages during transition.
constraint_indexing:constraint_classification(1992_bush_defense_budget_reduction_30_percent, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: MAJOR DEFENSE CONTRACTORS (TANGLED ROPE) — Powerful actors (Lockheed, General Dynamics, Northrop, McDonnell Douglas, Raytheon) face mandatory revenue cuts. Constrained exit: they cannot abandon federal markets entirely but can arbitrage—lobbying for contract preservation, converting military products to dual-use civilian markets, seeking foreign sales, restructuring around remaining contracts. Genuine coordination function: the 30% cap establishes predictable constraint boundaries ('This deep, and no deeper'), enabling planning. Asymmetric extraction: firms absorb cost shock but retain leverage through political pressure and supply chain indispensability.
constraint_indexing:constraint_classification(1992_bush_defense_budget_reduction_30_percent, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: EXECUTIVE BRANCH / OMB (ROPE) — Benefits from the fiscal constraint as a pure coordination mechanism. The 30% mandate is a commitment device that credibly redirects $50 billion to domestic priorities (infrastructure, education, deficit reduction). Experiences the constraint as coordination: it solves the political problem of defense spending reduction by making the target explicit and enforceable. Arbitrage exit: executive branch can modulate through contract delays, accounting adjustments, and base realignments—significant maneuvering room within the 30% boundary. Net beneficiary through fiscal discipline credibility.
constraint_indexing:constraint_classification(1992_bush_defense_budget_reduction_30_percent, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: MILITARY LEADERSHIP (TANGLED ROPE) — Organized institutional actors (JCS, service secretaries, combatant commanders) experience both coordination and extraction. Coordination function: the explicit 30% boundary enables force structure planning and procurement prioritization. Extraction: readiness capacity is threatened if cuts exceed sustainable levels; real force modernization is deferred. Constrained exit: cannot reject the civilian policy mandate but can lobby for relief, prioritize readiness investments, and defer less essential capabilities. Moderate extraction because military retains organizational coherence and political voice.
constraint_indexing:constraint_classification(1992_bush_defense_budget_reduction_30_percent, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: GENERAL TAXPAYERS (ROPE) — Experience the constraint as pure coordination enabling reallocation to domestic spending. The $50 billion in additional savings funds infrastructure, education, healthcare, and deficit reduction—genuine public goods. Mobile exit: taxpayers have no structural dependency on defense spending; they benefit from both the constraint itself and the downstream reallocation. Net beneficiary; experienced extraction is negligible or negative (constraint subsidizes this agent).
constraint_indexing:constraint_classification(1992_bush_defense_budget_reduction_30_percent, rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 6: COLD WAR INSTITUTIONALISM (PITON) — From a civilizational timescale, the 30% reduction constraint is a degraded vestigial mechanism. The framing ('peace dividend,' 'reduced Soviet threat') captures the genuine structural change (Soviet Union collapse 1989, end of bipolar competition), but the implementation—targeting a specific percentage reduction rather than rationalizing the entire defense posture—reflects institutional inertia. The constraint persists because defense budgeting institutions have not fundamentally reorganized around post-Cold War security architecture. Theater_ratio (0.38) reflects that the '30% by 1997' metric is partly performative: actual force structure changes trail the budget reduction, and the 'proof' of the peace dividend is largely accounting reclassification rather than capability reduction.
constraint_indexing:constraint_classification(1992_bush_defense_budget_reduction_30_percent, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: INDUSTRIAL RECONVERSION COALITION (SCAFFOLD) — Organized actors (regional development agencies, labor unions, defense diversification programs) see the 30% constraint as a temporary coordination challenge with a sunset. Defense Diversification Initiative, MARTA [Military Assistance to Regions in Transition], and conversion planning enable defense-dependent regions to build dual-use manufacturing capacity and retrain workers. Constrained exit: coalition members are invested in managing the transition but have agency through conversion policy. The scaffold has a natural sunset: once reconversion is complete (8-10 years), the constraint's extraction mechanism dissolves. Theater_ratio gradually declines as real economic diversification occurs.
constraint_indexing:constraint_classification(1992_bush_defense_budget_reduction_30_percent, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER / POST-COLD WAR INEVITABILITY (MOUNTAIN) — From a universal/civilizational perspective, the 30% reduction is framed as an immutable consequence of geopolitical structure: with the Soviet Union collapsed, sustained peacetime military spending at Cold War levels is inherently unsustainable. The constraint appears as natural law—the inevitable adjustment of means to reduced threat. However, the structural data reveals beneficiaries (general taxpayers, domestic priorities) and victims (defense contractors, regional employment), contradicting the mountain classification. This is a FALSE SUMMIT: the engine will detect beneficiary presence and reclassify to tangled_rope via the FSM signature override.
constraint_indexing:constraint_classification(1992_bush_defense_budget_reduction_30_percent, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(1992_bush_defense_budget_reduction_30_percent_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(1992_bush_defense_budget_reduction_30_percent, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(1992_bush_defense_budget_reduction_30_percent, TypeOther, context(agent_power(powerful), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(1992_bush_defense_budget_reduction_30_percent, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(1992_bush_defense_budget_reduction_30_percent, TR),
    TR >= 0.70.

:- end_tests(1992_bush_defense_budget_reduction_30_percent_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The constraint imposes real costs on defense contractors and workers while generating benefits for taxpayers and domestic budgets. The 30% magnitude is substantial—large enough to force genuine adjustment across the industrial base. However, extractiveness is not at snare levels (0.66+) because the constraint is bounded ('no deeper'), enabling contractors and military to plan adaptively, and because the beneficiary class (general taxpayers) is genuinely served. The rising trajectory (0.38 → 0.55 over the interval) reflects that initial contractual and labor commitments buffer the impact, but as the 1997 deadline approaches, real restructuring costs materialize. Suppression (0.48): Moderate. Barriers to exit include regional dependence, specialized skill requirements, political difficulty of reversing a presidential commitment, and limited dual-use market absorption capacity. But suppression is not severe because contractors retain profitable niches, military has planning time, and Industrial Reconversion Initiative provides policy scaffolding. Workers have the highest suppression within the contractor class (trapped), while contractors themselves have moderate suppression (constrained—they can lobby, diversify, seek foreign sales). Theater ratio (0.38): Moderate-low. The constraint has genuine functional content: the 30% reduction does redirect spending away from Cold War postures. However, some theater is present—the 'peace dividend' framing attributes the spending shift to geopolitical inevitability rather than explicit policy choice, and the timing (1997 deadline) is partly performative, chosen for post-election optics rather than force rationalization logic. Theater increases slightly over the interval as accounting adjustments become necessary to approach the target without disrupting force structure.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap manifests as disagreement over whether the constraint is a natural post-Cold War adjustment (mountain/rope from beneficiary perspectives) or an extractive policy regime (snare/tangled rope from contractor/worker perspectives). The beneficiary class (general taxpayers) sees the constraint as coordination enabling reallocation—pure benefit, no extraction. The executive branch sees coordination (credible commitment device) with significant arbitrage room. Defense contractors see a mixed regime: the explicit 30% boundary is coordination (enables planning), but the mandatory cut is extraction (forced revenue loss). Workers see pure extraction (snare)—no planning capacity, no alternative options, costs concentrated. Military leadership sees coordination (force structure planning) and extraction (readiness threat) in balance. The industrial reconversion coalition sees a temporary constraint with a built-in sunset—genuine agency and an exit path. The civilizational observer risks naturalizing the constraint as inevitable post-Cold War adjustment (mountain), but the beneficiary data reveals this as a false summit—the spending shift is a policy choice, not a natural law.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality (d) is derived from: (1) beneficiary/victim status—taxpayers and domestic priorities benefit (low d), contractors and workers bear costs (high d); (2) exit options—workers are trapped (high d), contractors constrained (moderate-high d), military organized (moderate d), taxpayers mobile (low d); (3) power level—powerful contractors can lobby (reduces experienced d), organized military retains voice (moderate d), powerless workers have no alternatives (high d). The executive branch's rope classification derives from arbitrage exit options (significant accounting and timing maneuvering room) combined with beneficiary status (fiscal credibility). Defense contractors' tangled rope derives from constrained exit (cannot abandon federal market but can adapt) plus both beneficiary and victim components—they benefit from remaining contracts and from the constraint's binding nature (prevents deeper cuts), but bear the mandatory revenue reduction. Workers' snare derives from trapped exit (no geographic or skill mobility) plus pure victim status (no downstream benefit).
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY AMBIGUITY: The constraint's core classification (tangled_rope) resolves a mandatrophy by showing that genuine coordination and asymmetric extraction coexist. The 30% boundary is a coordination mechanism that enables planning and credibly redirects $50 billion (rope function). But the mandate also imposes concentrated costs on contractors and especially workers (snare function). The mandatrophy is not 'which type is correct?' but 'how much coordination value is present alongside the extraction?' If the coordination value (fiscal discipline, explicit boundaries, reallocation enabling) justifies the asymmetry, tangled rope is appropriate. If the coordination value is thin cover for extraction, snare is more accurate. RESOLUTION PATH: Track whether industrial reconversion succeeds (indicating genuine coordination enabling adaptive response) vs contractor bankruptcies and regional economic collapse (indicating extraction with minimal coordination value). If reconversion succeeds and labor markets absorb displaced workers within 5-7 years, tangled rope is confirmed. If regional unemployment persists and contractor consolidation shows the cut was primarily extractive, reclassify to snare. FALSE SUMMIT CHECK: The mountain perspective ('post-Cold War reduction is inevitable natural law') is revealed as false by the beneficiary data. The constraint's extraction and coordination functions are both contingent on institutional choices, not structural inevitability. The engine's FSM detector will flag this and reclassify the mountain perspective as a false summit, validating the tangled rope type.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sustainable_force_readiness_threshold,
    'What defense spending level constitutes the minimum for sustainable force readiness, and how far below that threshold does the 30% reduction push?',
    'Joint Chiefs readiness assessments; correlation between budget cuts and measurable capability degradation (training hours, deployment cycles, equipment availability); historical comparison with previous reductions and their readiness impacts',
    'If 30% cut stays above sustainable threshold: extraction is moderate, readiness is preserved. If 30% exceeds sustainable threshold: extraction is severe, military effectiveness deteriorates in ways that emerge 3-5 years post-cut.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sustainable_force_readiness_threshold, empirical, 'Whether 30% reduction exceeds sustainable force readiness floor').

omega_variable(
    defense_contractor_survival_distribution,
    'Which defense contractors have sufficient diversification or adaptation capacity to survive the 30% cut without massive layoffs, and which are structurally dependent on maintained defense budgets?',
    'Contractor financial analysis; market share distribution; dual-use product development capacity; foreign military sale dependence; bankruptcy/restructuring outcomes 1997-2000',
    'If major contractors survive with workforce adjustments: extraction is moderate. If structural bankruptcies occur in concentrated regions: extraction approaches severe (snare for that regional labor market).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(defense_contractor_survival_distribution, empirical, 'Contractor survival and workforce impact distribution across defense industrial base').

omega_variable(
    domestic_reallocation_credibility,
    'Do the $50 billion in savings actually flow to domestic priorities (infrastructure, education, deficit reduction) or are they absorbed into other Pentagon categories (readiness, operations, nuclear arsenal)?',
    'Budget tracking 1992-1997; comparison of planned domestic allocations vs actual spending; accounting for cost inflation and unforeseen expenses',
    'If savings credibly flow to domestic priorities: beneficiary class (general taxpayers) gains real benefit. If savings are recapture by Pentagon through cost growth: constraint is largely performative, theater_ratio remains high.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(domestic_reallocation_credibility, empirical, 'Whether $50B savings materialize as domestic budget benefit or Pentagon cost growth absorption').

omega_variable(
    natural_law_vs_constructed_policy,
    'Is the 30% reduction an inevitable consequence of geopolitical structure (post-Cold War natural law) or a contingent policy choice reflecting specific institutional and political decisions?',
    'Historical counterfactual: alternative budget scenarios in 1992 literature; comparison with other states'' post-Cold War defense adjustments; analysis of pressure points where alternative outcomes were possible',
    'If inevitable natural law: mountain classification is correct. If contingent policy: false summit detected, tangled_rope (extraction/coordination hybrid) is more accurate.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_policy, conceptual, 'Whether post-Cold War defense reduction is structural necessity or constructed policy choice').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(1992_bush_defense_budget_reduction_30_percent, 0, 5).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(defbud_tr_t0, 1992_bush_defense_budget_reduction_30_percent, theater_ratio, 0, 0.28).
narrative_ontology:measurement(defbud_tr_t2, 1992_bush_defense_budget_reduction_30_percent, theater_ratio, 2, 0.33).
narrative_ontology:measurement(defbud_tr_t4, 1992_bush_defense_budget_reduction_30_percent, theater_ratio, 4, 0.38).
narrative_ontology:measurement(defbud_tr_t5, 1992_bush_defense_budget_reduction_30_percent, theater_ratio, 5, 0.38).

% Extraction over time
narrative_ontology:measurement(defbud_be_t0, 1992_bush_defense_budget_reduction_30_percent, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(defbud_be_t2, 1992_bush_defense_budget_reduction_30_percent, base_extractiveness, 2, 0.45).
narrative_ontology:measurement(defbud_be_t4, 1992_bush_defense_budget_reduction_30_percent, base_extractiveness, 4, 0.52).
narrative_ontology:measurement(defbud_be_t5, 1992_bush_defense_budget_reduction_30_percent, base_extractiveness, 5, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(1992_bush_defense_budget_reduction_30_percent, resource_allocation).
narrative_ontology:affects_constraint(1992_bush_defense_budget_reduction_30_percent, defense_contractor_consolidation_1990s).
narrative_ontology:affects_constraint(1992_bush_defense_budget_reduction_30_percent, regional_defense_industrial_employment_transition).

% DUAL FORMULATION NOTE:
% The 30% reduction constraint is upstream of sector-specific constraints (contractor consolidation, regional employment transition). The base extractiveness (0.52) reflects the macro fiscal policy level; downstream constraints in specific regions/firms will show higher extractiveness if adaptation fails (snare) or lower if diversification succeeds (rope).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(1992_bush_defense_budget_reduction_30_percent, powerless, 0.92).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
