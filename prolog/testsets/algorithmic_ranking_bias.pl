% ============================================================================
% CONSTRAINT STORY: algorithmic_ranking_bias
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_algorithmic_ranking_bias, []).

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
 *   constraint_id: algorithmic_ranking_bias
 *   human_readable: Algorithmic Ranking Bias in Digital Platforms
 *   domain: platform_economics/computational_fairness
 *
 * SUMMARY:
 *   Algorithmic ranking bias in digital platforms creates a structural
 *   asymmetry between platform operators and content creators/consumers.
 *   Ranking algorithms optimize for engagement metrics that measurably favor
 *   certain content categories (sensational, emotionally charged,
 *   high-engagement content) while systematically deprioritizing other
 *   categories (niche communities, marginalized creators, factual reporting).
 *   This bias operates through three mechanisms: (1) Technical — optimization
 *   functions embed designer values and are blind to equity dimensions; (2)
 *   Structural — platforms benefit from engagement-driven ranking regardless
 *   of its distributional consequences; (3) Institutional — transparency
 *   theater (bias audits, fairness statements) creates regulatory compliance
 *   appearance while core ranking logic remains unchanged. The constraint
 *   exhibits all six DR types, revealing that what appears as an inevitable
 *   computational property (mountain) is actually a contingent institutional
 *   choice maintaining asymmetric extraction (tangled rope to snare,
 *   depending on perspective).
 *
 * KEY AGENTS:
 *   - Platform Operators: Primary beneficiary (institutional/arbitrage) — capture advertising revenue and user engagement through ranking algorithms that prioritize engagement signals over equity or accuracy
 *   - Marginalized Content Creators: Primary victim (powerless/trapped) — systematically deprioritized by algorithmic ranking with no exit to alternative platforms at comparable scale
 *   - Search/Content Consumers: Secondary victim (moderate/constrained) — face information quality degradation from ranking bias; constrained by limited algorithmic literacy and platform dependency
 *   - Algorithmic Fairness Researchers: Organized actor (organized/constrained) — constrained by platform opacity and data access limitations; derive research benefit but cannot implement solutions at scale
 *   - Regulatory Bodies: Organized actor (organized/constrained) — building transparency and auditing requirements that create temporary enforcement burden with sunset trajectory
 *   - Fairness Theater: Institutional maintenance (institutional/arbitrage) — bias audits, diversity statements, fairness certifications persist through regulatory/reputational pressure despite minimal functional impact
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks treating algorithmic bias as natural law rather than institutional choice
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(algorithmic_ranking_bias, 0.58).
domain_priors:suppression_score(algorithmic_ranking_bias, 0.65).
domain_priors:theater_ratio(algorithmic_ranking_bias, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(algorithmic_ranking_bias, extractiveness, 0.58).
narrative_ontology:constraint_metric(algorithmic_ranking_bias, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(algorithmic_ranking_bias, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(algorithmic_ranking_bias, tangled_rope).
narrative_ontology:human_readable(algorithmic_ranking_bias, "Algorithmic Ranking Bias in Digital Platforms").
narrative_ontology:topic_domain(algorithmic_ranking_bias, "platform_economics/computational_fairness").

domain_priors:requires_active_enforcement(algorithmic_ranking_bias).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(algorithmic_ranking_bias, platform_operators).
narrative_ontology:constraint_beneficiary(algorithmic_ranking_bias, privileged_content_producers).
narrative_ontology:constraint_victim(algorithmic_ranking_bias, marginalized_creators).
narrative_ontology:constraint_victim(algorithmic_ranking_bias, search_result_consumers).
narrative_ontology:constraint_victim(algorithmic_ranking_bias, algorithmic_transparency).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: MARGINALIZED CREATORS (SNARE) — Trapped by platform dependency. Cannot exit to alternative distribution channels with comparable reach. Algorithmic ranking bias systematically deprioritizes their content while they have no recourse, no transparency into ranking criteria, and no alternative platforms at scale. Maximum extraction experienced.
constraint_indexing:constraint_classification(algorithmic_ranking_bias, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: SEARCH RESULT CONSUMERS (TANGLED ROPE) — Constrained by information dependency and lack of algorithmic literacy. Experience both coordination benefit (relevance ranking provides functional filtering) and extraction (ranking amplifies engagement metrics over accuracy, burying important information). High suppression due to opacity of ranking criteria and limited alternative search interfaces.
constraint_indexing:constraint_classification(algorithmic_ranking_bias, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: PLATFORM OPERATORS (ROPE) — Experience ranking constraint as pure coordination. Algorithms solve the genuine problem of filtering content at scale. Can arbitrage between ranking schemes (ad-revenue optimization vs fairness) with low exit cost. Net beneficiary through engagement-driven ranking that serves advertising revenue.
constraint_indexing:constraint_classification(algorithmic_ranking_bias, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: ALGORITHMIC FAIRNESS RESEARCHERS (TANGLED ROPE) — Organized but constrained by limited access to actual platform algorithms and training data. Derive benefits from problem framing and research funding, but constrained by platform opacity. Effective extraction stems from reliance on proprietary data and inability to implement findings at scale.
constraint_indexing:constraint_classification(algorithmic_ranking_bias, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: REGULATORS AND CIVIL SOCIETY (SCAFFOLD) — Building alternative architectures through regulation (algorithmic impact assessments, ranking transparency mandates, algorithmic auditing), with sunset trajectory. EU Digital Services Act and similar regulations force disclosure and external auditing, creating temporary coordination burden (enforcement) that phases down as transparency becomes standard. Extraction is temporary and declining.
constraint_indexing:constraint_classification(algorithmic_ranking_bias, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: FAIRNESS THEATER (PITON) — Diversity statements, bias audits, and fairness certifications on platforms are largely performative. The theater persists (high theater_ratio) due to regulatory and reputational pressure, but the primary function — algorithmically fair ranking — remains unachieved. The constraint is maintained through inertia: platforms conduct audits without changing core ranking logic, and external certifiers lack enforcement power.
constraint_indexing:constraint_classification(algorithmic_ranking_bias, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL/NATURAL LAW VIEW (MOUNTAIN) — From a computational perspective, ranking systems fundamentally embed the values of their designers and optimize against specified metrics. Some form of ranking bias is inherent to any large-scale filtering mechanism. However, this naturalizes what is actually a choice: *which* values are embedded, *which* metrics are optimized, and whether the bias benefits privileged or marginalized agents. The structural data reveals this as false summit — the bias is not immutable but contingent on design choices.
constraint_indexing:constraint_classification(algorithmic_ranking_bias, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(algorithmic_ranking_bias_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(algorithmic_ranking_bias, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(algorithmic_ranking_bias, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(algorithmic_ranking_bias, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(algorithmic_ranking_bias, TR),
    TR >= 0.70.

:- end_tests(algorithmic_ranking_bias_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. Platform operators extract significant value through ranking that prioritizes engagement over equity. The extraction is not maximal (0.70+) because some coordination function exists — ranking does solve the problem of filtering content at scale — and because regulatory pressure is beginning to constrain the mechanism. Measurement trajectory shows increasing extractiveness from 0.38 to 0.58 over the interval as platforms scale and optimize more aggressively against engagement metrics. Suppression (0.65): High. Multiple barriers prevent exit and reduce alternatives: (1) platform-dependency creates switching costs for creators and consumers; (2) algorithmic opacity prevents informed critique or adaptation; (3) network effects concentrate reach in few platforms; (4) alternative ranking models lack capital for scaled deployment. Creators face suppression through limited visibility; consumers face suppression through information opacity. Theater ratio (0.68): High. The bias audit and fairness certification ecosystem is substantially performative. Platforms conduct algorithmic impact assessments and diversity studies while maintaining core engagement-optimization logic. The theater increased from 0.45 to 0.68 over the interval as regulatory pressure mounted, creating compliance theater without functional change. Regulators and civil society are beginning to penetrate the theater through transparency mandates (EU DSA, algorithmic auditing requirements), creating conditions for scaffold exit.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates sharp perspectival divergence. Platform operators perceive ranking as pure coordination (Rope) — they are solving a real filtering problem at scale. Marginalized creators perceive snare — trapped in a system designed to prioritize high-engagement content they cannot produce competitively. Consumers perceive tangled rope — the ranking system both filters content usefully and distorts their information access. Fairness researchers perceive partial organization (organized/constrained) building toward scaffold — regulatory mandates creating temporary enforcement burden (auditing, disclosure) with sunset trajectory as transparency becomes standard. The natural law perspective (mountain) naturalizes what is actually a design choice: that ranking should optimize engagement rather than equity, reach rather than accuracy. The structural data reveals this as false summit.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) measures each agent's structural position relative to the extraction flow. Platform operators as beneficiaries with arbitrage options derive low d (≈0.10-0.15) — they can switch ranking schemes with minimal cost and benefit from multiple ranking models. Marginalized creators as victims with no exit options derive maximum d (≈0.95) — trapped by platform dependency, zero arbitrage options. Consumers constrained by information dependency derive moderate-high d (≈0.65-0.75) — they can theoretically switch platforms but face high cognitive and network switching costs. Regulatory bodies with constrained but organized exit options (building alternatives through mandated transparency) derive moderate d (≈0.45-0.55). The application of f(d) sigmoid to these values shows how structured position generates experienced extractiveness: powerless + trapped agents experience chi at ~1.4x baseline; organized + constrained agents experience chi at ~0.65x baseline; institutional + arbitrage agents experience near-zero or negative chi.
 *
 * MANDATROPHY ANALYSIS:
 *   CLASSIFICATION RESOLUTION: The mandatrophy is resolved by recognizing that the 'inevitable' ranking bias framing (mountain) conflates two distinct claims: (1) that any ranking system embeds designer values (true, but trivial — applies to all ranking), and (2) that engagement-metric optimization is the only economically viable ranking choice (false — alternatives exist but require regulatory pressure to deploy). The constraint is Tangled Rope from the analytical perspective because ranking simultaneously solves a genuine coordination problem (filtering at scale) while enabling extraction (deprioritizing marginalized creators, distorting information access). The extraction is not intrinsic to ranking but contingent on metric choice. Regulatory intervention (transparency, external auditing, algorithmic impact assessment) shifts this toward Scaffold by creating temporary enforcement costs that decline as transparency norms mature. The piton classification (fairness theater) captures how compliance mechanisms proliferate without functional change, a diagnostic of false summit naturalization — the appearance of fairness work without algorithmic modification.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    fairness_metric_incompleteness,
    'Which fairness metric adequately captures algorithmic ranking bias without creating new extraction opportunities?',
    'Comparative analysis of fairness metrics (demographic parity, equalized odds, calibration) across real platforms; measurement of unintended consequences (e.g., demographic parity causing representation collapse in niche categories)',
    'If one metric dominates: bias shifts to unconstrained dimensions. If no metric is complete: impossibility theorem prevents global optimization, enabling platform discretion and continued extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fairness_metric_incompleteness, empirical, 'Incompleteness of fairness metrics under platform gaming').

omega_variable(
    transparency_paradox,
    'Does transparency of ranking criteria reduce or amplify algorithmic bias by enabling gaming of disclosed criteria?',
    'Comparative study of platform ranking bias before and after transparency mandates; analysis of optimization against transparent vs opaque algorithms; detection of adversarial adaptation',
    'If transparency increases gaming: regulatory mandates backfire, and the extraction mechanism shifts from algorithmic opacity to deliberate manipulation. If gaming is controllable: transparency is genuinely beneficial.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(transparency_paradox, empirical, 'Whether transparency enables algorithmic gaming and bias amplification').

omega_variable(
    pluralistic_ranking_feasibility,
    'Can platforms operationally support multiple ranking algorithms optimizing different fairness criteria simultaneously, or does multi-objective ranking collapse to single-metric dominance?',
    'Technical analysis of multi-objective ranking systems; user studies on ranking diversity; measurement of platform willingness to sacrifice engagement metrics for fairness',
    'If feasible: scaffold perspective strengthened (alternatives exist). If infeasible: tangled rope or snare classification confirmed (no real exit for disadvantaged creators).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(pluralistic_ranking_feasibility, empirical, 'Technical and economic feasibility of pluralistic ranking systems').

omega_variable(
    suppression_mechanism_source,
    'Is algorithmic ranking bias primarily structural (inherent to optimization under engagement metrics) or maintained through deliberate suppression (platform choice to preserve engagement)?',
    'Analysis of platform design decisions; examination of algorithmic changes under regulatory pressure; comparison of bias patterns across platforms with different explicit values',
    'If structural: constraint is harder to disrupt (requires fundamental redesign). If deliberate: extraction is intentional, snare classification is correct.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_source, conceptual, 'Structural vs deliberate suppression in algorithmic ranking bias').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(algorithmic_ranking_bias, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(arb_tr_t0, algorithmic_ranking_bias, theater_ratio, 0, 0.45).
narrative_ontology:measurement(arb_tr_t3, algorithmic_ranking_bias, theater_ratio, 3, 0.58).
narrative_ontology:measurement(arb_tr_t6, algorithmic_ranking_bias, theater_ratio, 6, 0.68).

% Extraction over time
narrative_ontology:measurement(arb_be_t0, algorithmic_ranking_bias, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(arb_be_t3, algorithmic_ranking_bias, base_extractiveness, 3, 0.48).
narrative_ontology:measurement(arb_be_t6, algorithmic_ranking_bias, base_extractiveness, 6, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(algorithmic_ranking_bias, resource_allocation).
narrative_ontology:affects_constraint(algorithmic_ranking_bias, platform_algorithmic_opacity).
narrative_ontology:affects_constraint(algorithmic_ranking_bias, creator_economic_dependence).
narrative_ontology:affects_constraint(algorithmic_ranking_bias, information_ecosystem_fragmentation).

% DUAL FORMULATION NOTE:
% Algorithmic ranking bias is downstream of platform profit incentives and upstream of information quality degradation. Separate constraint stories exist for platform opacity (epistemic constraint) and creator economic dependence (labor constraint). This story models the coordination/extraction hybrid that emerges when ranking algorithms optimize against engagement metrics regardless of distributional consequences.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(algorithmic_ranking_bias, analytical, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
