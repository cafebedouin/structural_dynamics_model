% ============================================================================
% CONSTRAINT STORY: alternative_diet_research_funding
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_alternative_diet_research_funding, []).

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
 *   constraint_id: alternative_diet_research_funding
 *   human_readable: Alternative Diet Research Funding Constraint
 *   domain: nutrition_science/research_funding
 *
 * SUMMARY:
 *   Alternative diet research faces systematic funding constraints that
 *   create a multi-layered extraction mechanism. Mainstream nutrition science
 *   and the food industry benefit from funding allocation that privileges
 *   conventional dietary paradigms (low-fat, calorie-counting, plant-forward
 *   recommendations), while heterodox diet researchers (ketogenic, carnivore,
 *   intermittent fasting, metabolically optimized nutrition) face suppressed
 *   funding access, higher publication barriers, and career penalties. This
 *   constraint exhibits Tangled Rope structure: genuine coordination
 *   functions exist (research funding mechanisms, peer review systems,
 *   journal hierarchy) alongside asymmetric extraction (benefits concentrate
 *   on industry and conventional researchers, costs concentrate on
 *   alternative diet researchers). The theater ratio (0.68) indicates that
 *   institutional legitimacy and peer review increasingly function
 *   performatively — reviewers assess novelty within accepted frameworks
 *   rather than evaluating mechanism validity, and funding agencies signal
 *   scientific rigor through conventional metrics rather than actual
 *   knowledge advancement. The extractiveness trajectory (0.38 → 0.58 over 15
 *   years) reflects growing institutional consolidation: as nutrition science
 *   becomes increasingly medicalized and industry-dominated, alternative
 *   frameworks face higher institutional barriers despite some empirical
 *   support.
 *
 * KEY AGENTS:
 *   - Alternative Diet Researchers: Primary victims (powerless/trapped) — face systematic funding denial, publication barriers, career penalties for pursuing heterodox nutritional frameworks
 *   - Heterodox Research Groups: Secondary victims (moderate/constrained) — established researchers can sometimes access alternative funding but face institutional isolation and reputation effects
 *   - Mainstream Nutrition Industry: Primary beneficiaries (institutional/arbitrage) — food, pharmaceutical, and supplement corporations benefit from funding that suppresses competing diet frameworks
 *   - Conventional Research Establishment: Secondary beneficiary (institutional/arbitrage) — universities, journals, and grant agencies benefit from funding concentration and reputation hierarchy
 *   - Open Science and Data Commons Movement: Organized agents (organized/mobile) — independent research platforms building alternative verification infrastructure; represent sunset mechanism
 *   - Institutional Peer Review System: Performative mechanism (institutional/arbitrage) — maintains gatekeeping ritual with declining functional verification capacity; piton classification
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(alternative_diet_research_funding, 0.58).
domain_priors:suppression_score(alternative_diet_research_funding, 0.65).
domain_priors:theater_ratio(alternative_diet_research_funding, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(alternative_diet_research_funding, extractiveness, 0.58).
narrative_ontology:constraint_metric(alternative_diet_research_funding, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(alternative_diet_research_funding, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(alternative_diet_research_funding, tangled_rope).
narrative_ontology:human_readable(alternative_diet_research_funding, "Alternative Diet Research Funding Constraint").
narrative_ontology:topic_domain(alternative_diet_research_funding, "nutrition_science/research_funding").

domain_priors:requires_active_enforcement(alternative_diet_research_funding).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(alternative_diet_research_funding, mainstream_nutrition_industry).
narrative_ontology:constraint_beneficiary(alternative_diet_research_funding, conventional_research_establishment).
narrative_ontology:constraint_victim(alternative_diet_research_funding, alternative_diet_researchers).
narrative_ontology:constraint_victim(alternative_diet_research_funding, public_health_knowledge_commons).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ALTERNATIVE DIET RESEARCHER (SNARE) — Early-career researchers pursuing non-mainstream dietary approaches face systematic barriers: funding agencies prioritize conventional nutrition paradigms, peer review gatekeepers dismiss novel diet frameworks, and career advancement requires publishing in journals controlled by establishment consensus. Exit is structurally impossible — pursuing alternative diets blocks access to grants, positions, and credibility. The constraint extracts intellectual labor and research potential while suppressing alternative epistemic frameworks.
constraint_indexing:constraint_classification(alternative_diet_research_funding, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: HETERODOX RESEARCHER GROUP (TANGLED ROPE) — Mid-career researchers with established reputations can sometimes access alternative funding (private foundations, international sources, industry that benefits from heterodox nutrition). They experience genuine coordination — collaboration networks, shared methods, collective knowledge-building on alternative diet mechanisms. But they also experience extraction: constant need to justify framework choice, higher publication barriers, career ceiling effects, and institutional isolation. Some coordination benefit but significant asymmetric cost.
constraint_indexing:constraint_classification(alternative_diet_research_funding, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: MAINSTREAM NUTRITION INDUSTRY (ROPE) — Food, pharmaceutical, and supplement corporations benefit from funding that suppresses competing diet frameworks. They experience the constraint as coordination: funding allocation mechanisms, journal access, conference prestige hierarchies all align with their product portfolios. Net beneficiary. Exit options are maximal — they can arbitrage to alternative research if benefits shift, but currently reap substantial returns from status quo.
constraint_indexing:constraint_classification(alternative_diet_research_funding, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: OPEN SCIENCE MOVEMENT (SCAFFOLD) — Open-source nutrition databases, preprint archives (PubPeer, bioRxiv), and independent research organizations are building alternative verification pathways. These create genuine coordination — distributed peer review, independent meta-analysis, transparent data sharing — while reducing reliance on traditional gatekeepers. The constraint has sunset logic: as open-data norms mature and alternative research infrastructure matures, dependence on conventional funding mechanisms declines. Estimated sunset: 15-25 years as digital-first nutrition science platforms mature.
constraint_indexing:constraint_classification(alternative_diet_research_funding, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: INSTITUTIONAL PEER REVIEW SYSTEM (PITON) — Traditional peer review for diet research is increasingly performative. Reviewers assess novelty and fit within accepted frameworks but cannot verify dietary intervention mechanisms across diverse human populations. The review ritual persists through institutional inertia: journals, universities, and grant agencies maintain peer review as status signifier despite low functional verification capacity for complex dietary outcomes. Theater ratio elevated because the mechanism is largely about institutional legitimation rather than knowledge validation.
constraint_indexing:constraint_classification(alternative_diet_research_funding, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (MOUNTAIN) — From civilizational perspective, dominant diet paradigms in any era risk naturalizing as 'how nutrition science works.' The current constraint can appear immutable: mainstream approaches have institutional density, funding concentration, and publication control. But structural data contradicts true mountain classification — this is a contingent institutional arrangement, not a law of nature. The analytical observer risks false summit error: treating extractive institutional capture as inherent to nutritional knowledge production.
constraint_indexing:constraint_classification(alternative_diet_research_funding, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(alternative_diet_research_funding_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(alternative_diet_research_funding, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(alternative_diet_research_funding, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(alternative_diet_research_funding, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(alternative_diet_research_funding, TR),
    TR >= 0.70.

:- end_tests(alternative_diet_research_funding_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. The constraint extracts through multiple vectors: funding denial (opportunity cost), publication barriers (intellectual labor without dissemination), and career penalties (wage/position loss). The extraction is not as severe as pure snare because some alternative funding sources exist and published alternative diet research does emerge, albeit at reduced rates. Suppression (0.65): High. Systematic barriers include NIH/NSF funding preferences for conventional nutrition, peer reviewer gatekeeping, journal editorial bias, and professional reputation effects. Suppression is not absolute — alternative researchers do publish and secure funding — but barriers are substantial and structural. Theater ratio (0.68): High. Institutional peer review for diet research is performative because reviewers assess fit within accepted frameworks rather than mechanistic validity. Diet intervention outcomes are highly heterogeneous across populations, and reviewers cannot verify individualized response mechanisms from conventional study designs. The institutional apparatus increasingly signals rigor through conventional markers (RCT design, long-term follow-up, publication volume) rather than evidence of mechanism. The trajectory increase (0.52 → 0.68) reflects growing medicalization and industry consolidation raising performative content.
 *
 * PERSPECTIVAL GAP:
 *   The original diet researcher trapped in the system experiences pure extraction (Snare) — funding denial, publication blockade, career penalty with no exit option. The established heterodox group with alternative funding access experiences mixed coordination and extraction (Tangled Rope) — some collaborative benefits but persistent institutional barriers. The mainstream industry benefits from the constraint as coordination (Rope) — funding mechanisms, journal access, and conference hierarchies align with their interests. The open science coalition sees a solvable problem with sunset (Scaffold) — alternative research platforms are building independent verification infrastructure. The peer review system itself experiences the constraint as degraded ritual (Piton) — reviewers acknowledge that diet verification is performative, but the system persists through institutional inertia. The civilizational analytical observer risks naturalizing contingent institutional arrangements (Mountain) — seeing funding concentration as inherent to how nutrition science must work. The perspectival gap reveals that the constraint's classification depends entirely on the agent's structural position relative to funding, gatekeeping, and reputation systems.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values are derived from each agent's position relative to the extraction flow. Alternative diet researchers face maximum extraction (d ≈ 0.95) because they are structurally trapped with no exit options — pursuing alternative diets blocks access to mainstream funding, positions, and credibility. Heterodox groups with alternative funding access have moderate directionality (d ≈ 0.60) because they face extraction but retain some agency and coordination benefits. The mainstream industry has low directionality as beneficiaries (d ≈ 0.10) because they arbitrage across funding sources and benefit from the status quo. The conventional establishment has similar low directionality (d ≈ 0.15) as beneficiary with arbitrage options. The open science movement has moderate directionality (d ≈ 0.50) because they face institutional opposition but possess mobile exit options and can build alternative infrastructure. The peer review system as institutional piton has low directionality (d ≈ 0.20) because it maintains theater without extraction flows — the system benefits from status quo but lacks agents actively extracting.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by demonstrating how institutional capture creates genuine extraction despite surface-level coordination mechanisms. The funding system, peer review, and journal hierarchy coordinate legitimate research functions (allocating resources, maintaining quality standards, establishing credibility). However, the constraints operate asymmetrically: benefits concentrate on industry and conventional researchers while costs concentrate on heterodox researchers who are systematically excluded or penalized. The mandatrophy resolution identifies the coordination function (funding allocation, peer verification) as real but asymmetric extraction as the dominant mechanism. The theater ratio indicates that institutional legitimacy increasingly functions performatively — the system signals rigor through conventional markers rather than validating mechanism. The constraint is Tangled Rope, not pure Snare, because some alternative diet research does occur and some coordination benefits exist for privileged participants. But the classification prevents misreading the constraint as pure coordination (Rope) — the asymmetric extraction component is substantial and measurable.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    funding_bottleneck_causality,
    'Does institutional funding control actually suppress alternative diet research, or does poor empirical support naturally limit heterodox diet scientific viability?',
    'Comparative analysis: funding allocation vs publication patterns for alternative diets with varying empirical support; tracking researchers who exit mainstream funding but continue productive work',
    'If funding control is causal: constraint is Tangled Rope/Snare driven by institutional gatekeeping. If poor evidence base is primary: constraint is Mountain (unavoidable due to epistemic requirements). If mixed: quantify extraction vs legitimate epistemic filtering.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(funding_bottleneck_causality, empirical, 'Whether funding gatekeeping suppresses viable research or filters low-evidence claims').

omega_variable(
    alternative_diet_empirical_heterogeneity,
    'Are ''alternative diets'' (ketogenic, carnivore, intermittent fasting, low-carb, plant-based variants) structurally distinct enough to require separate funding streams, or should they compete within unified nutritional science framework?',
    'Meta-analysis of mechanistic heterogeneity; identification of whether alternative diet claims require different experimental designs or can be tested within conventional nutritional epidemiology',
    'If structurally distinct: funding suppression is true constraint (researchers pursuing incommensurate frameworks). If within-framework competition: funding scarcity is legitimate selection mechanism, constraint is weaker (Rope rather than Snare).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(alternative_diet_empirical_heterogeneity, conceptual, 'Structural distinctness of alternative diet frameworks').

omega_variable(
    suppression_mechanism_persistence,
    'If institutional funding barriers were removed, would suppression persist through peer reviewer bias, publication journal gatekeeping, or professional reputation effects?',
    'Counterfactual analysis via alternative-funding tracking: researchers with private/international funding compared to grant-funded researchers on publication and career outcomes',
    'If suppression persists post-funding: constraint is multi-layered (funding + peer review + reputation). If suppression declines: funding gatekeeping is primary mechanism. Determines exit feasibility for constrained agents.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_persistence, empirical, 'Whether suppression persists after funding barriers removed').

omega_variable(
    industry_extraction_vector,
    'Does mainstream nutrition industry funding suppress alternative diets primarily through direct funding control, or through career incentive alignment (researchers pursuing industry-aligned projects for career advancement)?',
    'Institutional funding source tracking; career outcome analysis by funding source; researcher incentive interviews',
    'If direct gatekeeping: constraint is institutional suppression (Snare/Tangled Rope). If incentive alignment: constraint partially identity_locked (researchers internalize industry priorities). Affects exit option classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(industry_extraction_vector, empirical, 'Primary vector of industry funding influence on research suppression').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(alternative_diet_research_funding, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(altdiet_tr_t0, alternative_diet_research_funding, theater_ratio, 0, 0.52).
narrative_ontology:measurement(altdiet_tr_t5, alternative_diet_research_funding, theater_ratio, 5, 0.62).
narrative_ontology:measurement(altdiet_tr_t10, alternative_diet_research_funding, theater_ratio, 10, 0.68).
narrative_ontology:measurement(altdiet_tr_t15, alternative_diet_research_funding, theater_ratio, 15, 0.65).

% Extraction over time
narrative_ontology:measurement(altdiet_be_t0, alternative_diet_research_funding, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(altdiet_be_t5, alternative_diet_research_funding, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(altdiet_be_t10, alternative_diet_research_funding, base_extractiveness, 10, 0.58).
narrative_ontology:measurement(altdiet_be_t15, alternative_diet_research_funding, base_extractiveness, 15, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(alternative_diet_research_funding, resource_allocation).
narrative_ontology:affects_constraint(alternative_diet_research_funding, nutritional_science_paradigm_lock).
narrative_ontology:affects_constraint(alternative_diet_research_funding, food_industry_regulatory_capture).
narrative_ontology:affects_constraint(alternative_diet_research_funding, academic_journal_publication_bias).

% DUAL FORMULATION NOTE:
% Alternative diet research funding is downstream of broader academic funding concentration and peer review gatekeeping but represents a distinct constraint focused on diet-specific mechanisms. The upstream constraints (science paradigm lock, industry capture) have their own extractiveness values reflecting systemic institutional structures; alternative diet funding has its own extractiveness reflecting the specific suppression of heterodox nutritional frameworks within those broader systems.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(alternative_diet_research_funding, institutional, 0.18).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
