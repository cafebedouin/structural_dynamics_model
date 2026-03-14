% ============================================================================
% CONSTRAINT STORY: ai_winner_picking
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ai_winner_picking, []).

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
 *   constraint_id: ai_winner_picking
 *   human_readable: AI Winner Picking in Venture Capital and Technology Markets
 *   domain: technology/finance/institutional_governance
 *
 * SUMMARY:
 *   AI winner picking describes the structural constraint by which capital
 *   allocation, computational resources, and institutional recognition
 *   concentrate among a small set of large incumbent labs and their backed
 *   startups, systematically excluding marginal research teams and
 *   alternative development pathways. The mechanism operates through multiple
 *   layered selection criteria: VC gatekeepers use founder pedigree, previous
 *   funding, and benchmark performance as allocation heuristics; major AI
 *   labs dominate compute resources and model access through scale economies;
 *   benchmark standards become self-fulfilling measures of progress; and
 *   institutional inertia maintains these patterns even as their predictive
 *   validity declines. This creates a tangled coordination-extraction hybrid:
 *   genuine coordination functions exist (resource concentration enables
 *   cumulative research progress, standardized benchmarks coordinate research
 *   directions, capital allocation funds high-risk ventures), but the same
 *   mechanisms extract value from excluded teams by limiting their access to
 *   capital, compute, datasets, and visibility. The constraint exhibits all
 *   six DR types depending on the observer's structural position, with an
 *   embedded false summit (naturalizing institutional patterns as innovation
 *   inevitability) and a real sunset mechanism (alternative funding and
 *   development pathways maturing in parallel).
 *
 * KEY AGENTS:
 *   - Large AI Lab Incumbents: Primary beneficiaries (institutional/arbitrage) — concentrate compute resources, model weights, and researcher talent; experience constraint as coordination mechanism for cumulative research
 *   - Venture Capital Gatekeepers: Primary beneficiaries (powerful/mobile) — extract information asymmetry rents and network access premiums; allocate capital using historical winner patterns
 *   - Excluded Startups and Marginal Teams: Primary victims (powerless/trapped) — barred from capital and compute resources needed to compete; face self-fulfilling prophecy of neglect
 *   - Market Information Quality: Secondary victim (powerless/trapped) — allocation bias creates false signals about capability and viability; suppresses discovery of alternative development pathways
 *   - Open Source and Alternative Funding Coalition: Organized agents (organized/constrained) — building parallel infrastructure (Hugging Face, open foundations, university consortia, government research initiatives) with sunset logic
 *   - Benchmark Evaluation System: Institutional actor (institutional/analytical) — maintains performative standards (piton); actual predictive validity for market success and real-world capability is weak
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_winner_picking, 0.58).
domain_priors:suppression_score(ai_winner_picking, 0.65).
domain_priors:theater_ratio(ai_winner_picking, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_winner_picking, extractiveness, 0.58).
narrative_ontology:constraint_metric(ai_winner_picking, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(ai_winner_picking, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_winner_picking, tangled_rope).
narrative_ontology:human_readable(ai_winner_picking, "AI Winner Picking in Venture Capital and Technology Markets").
narrative_ontology:topic_domain(ai_winner_picking, "technology/finance/institutional_governance").

domain_priors:requires_active_enforcement(ai_winner_picking).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_winner_picking, large_ai_lab_incumbents).
narrative_ontology:constraint_beneficiary(ai_winner_picking, venture_capital_gatekeepers).
narrative_ontology:constraint_beneficiary(ai_winner_picking, algorithm_designers).
narrative_ontology:constraint_victim(ai_winner_picking, early_stage_startups).
narrative_ontology:constraint_victim(ai_winner_picking, marginal_research_teams).
narrative_ontology:constraint_victim(ai_winner_picking, market_information_quality).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: EXCLUDED STARTUP (SNARE) — Structurally barred from capital and computing resources needed to compete. Winner-picking algorithms create self-fulfilling prophecies: neglected teams cannot demonstrate capability without resources, and allocation follows demonstrated capability. Maximum extraction with minimal escape options.
constraint_indexing:constraint_classification(ai_winner_picking, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: MARGINALIZED RESEARCH TEAM (TANGLED ROPE) — Experiences both coordination (access to shared models and datasets when included) and extraction (resource allocation biased toward teams matching historical winner profiles). High barriers to exit through institutional switching or geographic relocation; some agency through alternative funding paths.
constraint_indexing:constraint_classification(ai_winner_picking, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: LARGE AI LAB INCUMBENT (ROPE) — Benefits from coordination function: shared infrastructure, standardized evaluation metrics, and concentrated R&D enable cumulative advantage. Experiences constraint primarily as coordination mechanism allocating resources toward proven research directions. Arbitrage options allow exit if incentive structure changes.
constraint_indexing:constraint_classification(ai_winner_picking, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: VENTURE CAPITAL GATEKEEPER (TANGLED ROPE) — Coordinates capital allocation (genuine coordination function: matching capital to promising ventures) while extracting returns through information asymmetry and pattern-matching bias. Powerful actors can switch strategies but face opportunity costs if deviating from successful winner-picking heuristics.
constraint_indexing:constraint_classification(ai_winner_picking, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: OPEN SOURCE AND ALTERNATIVE FUNDING COALITION (SCAFFOLD) — Organized agents (open-source foundations, alternative funding mechanisms, university consortia, government research initiatives) are building parallel allocation systems that bypass winner-picking logic. These represent sunset mechanisms: distributed peer review, public research funding, and open model repositories create alternative pathways that reduce dependence on VC gatekeepers. Sunset horizon: 15-25 years as these alternatives mature.
constraint_indexing:constraint_classification(ai_winner_picking, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: BENCHMARK EVALUATION SYSTEM (PITON) — Standardized benchmarks (ImageNet, GLUE, SuperGLUE) were designed to coordinate research toward measurable progress but have become largely performative. Gaming of benchmarks is endemic; performance on standardized tests poorly predicts real-world capability or market success. The benchmark ritual persists through institutional inertia despite acknowledged limitations. Theater ratio reflects the gap between benchmark dominance and actual capability diversity.
constraint_indexing:constraint_classification(ai_winner_picking, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, allocation under uncertainty always exhibits preference for proven patterns — capital naturally flows to lower-variance targets and demonstrated competence. This perspective risks naturalizing the institutional patterns (VC concentration, benchmark dominance, network effects) as inevitable features of how innovation funding must work. However, this is a false summit: the constraint is contingent on specific institutional arrangements, not laws of nature.
constraint_indexing:constraint_classification(ai_winner_picking, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ai_winner_picking_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(ai_winner_picking, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(ai_winner_picking, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(ai_winner_picking, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(ai_winner_picking, TR),
    TR >= 0.70.

:- end_tests(ai_winner_picking_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high and rising. The constraint has intensified over 9 years as AI capability concentration has increased and compute resource disparities have widened. At time 0, alternatives (academic labs, open-source communities) remained competitive; by time 9, resource gaps are substantial. The trajectory shows accumulation of extraction through institutional network effects rather than single-point coercion. Suppression (0.65): High. Multiple barriers prevent alternative pathways: capital scarcity, compute access restrictions (through pricing and model weights closures), talent concentration in funded labs, benchmark-driven evaluation, publication bias toward well-resourced teams, and narrative framing of 'serious AI research' as requiring institutional backing. Suppression is structural (real resource constraints) but also performative (institutional gatekeeping through opacity). Theater ratio (0.68): High and rising. Benchmark dominance is increasingly detached from actual capability and market value. Gaming of benchmarks is endemic; multiple teams have achieved state-of-the-art performance through specialized optimization rather than general capability improvement. The performative content has increased as benchmarks have accumulated and diverged from real-world utility. Large labs now maintain benchmark performance partly through engineering effort divorced from fundamental capability.
 *
 * PERSPECTIVAL GAP:
 *   The constraint exhibits maximum perspectival divergence. Excluded teams perceive pure extraction (Snare) with no exit options and no coordination benefit. Marginalized teams perceive mixed coordination-extraction (Tangled Rope) — they benefit from shared infrastructure when included but face resource barriers and allocation bias. Large labs perceive coordination (Rope) — concentrated resources enable research progress. VC gatekeepers perceive mixed coordination-extraction (Tangled Rope) — they coordinate capital matching but extract returns through information asymmetry. Alternative pathways see a temporary institutional arrangement with a sunset (Scaffold) — parallel funding mechanisms are building exits. Benchmarks appear as degraded ritual (Piton) — performative rather than functional. The analytical observer risks seeing innovation allocation as necessarily concentrated (Mountain false summit). The perspectival gap reveals that the classification depends entirely on structural position: there is no single 'correct' type, but rather a presheaf of legitimate readings from different observation sites.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values are derived from beneficiary/victim status and exit options. Large labs and VC gatekeepers are beneficiaries with arbitrage/mobile exit options, yielding low d values and negative or zero effective extraction from their perspective (they experience the constraint as supporting their position). Excluded teams are victims with trapped exit options, yielding high d values and maximum effective extraction (they experience the constraint as barrier). Marginalized teams are victims with constrained exit options, yielding moderate-high d values and significant extraction. The open-source coalition has organized power and constrained exit options (they can build alternatives but face entrenched institutional advantages), yielding moderate d values. The analytical observer is positioned at d ≈ 0.72 (canonical for analytical power), experiencing high effective extraction because they see the structural distortion even if they are not personally subject to it. The gap between the beneficiary and victim perspectives (d ≈ 0.15 vs d ≈ 0.95) is the signature of a tangled rope: asymmetric extraction with genuine coordination function for beneficiaries and genuine harm for victims.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION: The constraint is classified as tangled rope because it satisfies all three gates: (1) base extractiveness ≥ 0.30 (measured at 0.58), (2) genuine beneficiaries exist and benefit from coordination (large labs experience real cumulative research advantage through resource concentration), (3) genuine victims exist and bear asymmetric costs (excluded teams have no access to the benefits). The mandatrophy is resolved by showing that 'winner picking' is NOT pure extraction (snare) despite high extractiveness, because the concentration mechanism does produce real coordination gains in AI capability development. A well-resourced lab with access to large compute clusters and collaborative talent CAN develop capabilities faster than distributed teams. This is not a false coordination claim. However, the same mechanism that produces coordination for beneficiaries produces extraction for victims — the asymmetry is irreducible. This is the defining structure of a tangled rope: the extraction is not secondary or avoidable; it is structurally necessary to the coordination function. Removing victim barriers would require distributing resources widely enough that no coordination gains from concentration remain. The false summit (mountain perspective) is naturalization of this institutional arrangement as necessary innovation structure rather than contingent policy choice. The real sunset (scaffold perspective) is the emergence of alternative development pathways that reduce dependence on concentrated resources.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    winner_picking_predictive_validity,
    'Do historical winner-picking patterns (VC backing, benchmark performance, founder pedigree) predict actual long-term innovation impact and market success, or do they primarily predict institutional resource concentration?',
    'Longitudinal analysis of funded vs unfunded startups, benchmark performance vs real-world capability, founder background vs venture outcomes; counterfactual analysis of what alternative allocation patterns would have produced',
    'If high predictive validity: winner-picking is primarily coordination (Rope dominates). If low predictive validity: patterns reflect self-fulfilling prophecy and extraction (Snare dominates). If mixed: the tangled rope classification is correct.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(winner_picking_predictive_validity, empirical, 'Predictive validity of winner-picking patterns versus resource concentration effect').

omega_variable(
    alternative_capability_discovery,
    'Can open-source development, academic consortia, and non-VC funding mechanisms discover and develop AI capabilities as efficiently as centralized VC-backed labs?',
    'Comparative analysis of capability development timelines, resource efficiency, and innovation emergence across funding regimes; tracking of major AI breakthroughs by funding source; evaluation of open-source model performance trajectories',
    'If alternative pathways are competitive: scaffold sunset is real and constraint extractiveness will decline. If alternative pathways lag significantly: scaffold is aspirational rather than structural, and winner-picking concentration remains necessary.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_capability_discovery, empirical, 'Competitive capability of alternative funding and development pathways').

omega_variable(
    information_asymmetry_necessity,
    'Is the information asymmetry between resource gatekeepers and entrepreneurs a necessary feature of innovation capital allocation, or does it reflect institutional convenience rather than structural requirement?',
    'Analysis of prediction markets, collective forecasting, and distributed evaluation mechanisms for AI capability assessment; comparison of accuracy across centralized expert judgment vs distributed assessment',
    'If asymmetry is necessary: gatekeeper extraction is justified by coordination cost (Rope perspective strengthens). If alternatives work: asymmetry is rent-seeking (Snare perspective strengthens, gatekeeper classification shifts to pure extraction).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(information_asymmetry_necessity, conceptual, 'Whether information asymmetry is necessary or contingent institutional choice').

omega_variable(
    suppression_mechanism_opacity,
    'How much of the observed suppression of alternative pathways is structural (actual resource scarcity, coordination requirements) versus performative (institutional gatekeeping through opacity and narrative control)?',
    'Analysis of documented exclusion mechanisms; counterfactual resource requirements for alternative pathways; transparency audits of funding decision-making processes',
    'If largely structural: suppression score and snare classification are justified. If largely performative: suppression reflects institutional choices that could be reformed, weakening the snare and strengthening the scaffold sunset narrative.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_opacity, empirical, 'Degree to which suppression is structural versus performative gatekeeping').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_winner_picking, 0, 9).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(aiwp_tr_t0, ai_winner_picking, theater_ratio, 0, 0.48).
narrative_ontology:measurement(aiwp_tr_t3, ai_winner_picking, theater_ratio, 3, 0.58).
narrative_ontology:measurement(aiwp_tr_t6, ai_winner_picking, theater_ratio, 6, 0.65).
narrative_ontology:measurement(aiwp_tr_t9, ai_winner_picking, theater_ratio, 9, 0.68).

% Extraction over time
narrative_ontology:measurement(aiwp_be_t0, ai_winner_picking, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(aiwp_be_t3, ai_winner_picking, base_extractiveness, 3, 0.45).
narrative_ontology:measurement(aiwp_be_t6, ai_winner_picking, base_extractiveness, 6, 0.55).
narrative_ontology:measurement(aiwp_be_t9, ai_winner_picking, base_extractiveness, 9, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ai_winner_picking, resource_allocation).
narrative_ontology:boltzmann_floor_override(ai_winner_picking, 0.18).
narrative_ontology:affects_constraint(ai_winner_picking, capability_moat_concentration).
narrative_ontology:affects_constraint(ai_winner_picking, benchmark_gaming_incentives).
narrative_ontology:affects_constraint(ai_winner_picking, startup_capital_scarcity).

% DUAL FORMULATION NOTE:
% AI winner picking is an institutional constraint family decomposed from three structurally distinct constraints: (1) capability moat concentration (the resource gap itself, ε≈0.35, Rope), (2) benchmark gaming incentives (performative evaluation driving allocation, ε≈0.52, Piton), (3) startup capital scarcity (financing bias excluding alternatives, ε≈0.61, Snare). Each has distinct metrics and mechanisms. Winner picking models the aggregate constraint integrating all three. Upstream: benchmark gaming affects winner picking (benchmarks drive allocation decisions). Upstream: capability moat affects both benchmark gaming and winner picking (resource advantage enables both benchmark dominance and capital access).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(ai_winner_picking, institutional, 0.22).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
