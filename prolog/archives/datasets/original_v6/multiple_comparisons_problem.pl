% ============================================================================
% CONSTRAINT STORY: multiple_comparisons_problem
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_multiple_comparisons_problem, []).

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
 *   constraint_id: multiple_comparisons_problem
 *   human_readable: Multiple Comparisons Problem in Statistical Inference
 *   domain: statistics/science_methodology
 *
 * SUMMARY:
 *   The multiple comparisons problem is a structural constraint in
 *   statistical inference where researchers conducting multiple hypothesis
 *   tests face a dilemma: either apply alpha level corrections that reduce
 *   statistical power and make positive findings harder to achieve, or
 *   conduct uncorrected tests and accumulate false positive results in the
 *   literature. This creates an asymmetric extraction mechanism: researchers
 *   who conduct multiple comparisons without correction benefit from easier
 *   paths to publication and positive career outcomes, while the field's
 *   epistemic integrity and researchers attempting replication bear the costs
 *   of false positive contamination. The constraint exhibits tangled rope
 *   structure: genuine coordination function (testing multiple hypotheses is
 *   scientifically necessary) exists alongside asymmetric extraction (career
 *   incentives suppress adoption of proper statistical correction). Theater
 *   ratio (0.68) reflects that editorial peer review claims to enforce
 *   statistical rigor but inconsistently applies correction standards — the
 *   gatekeeping ritual persists while actual correction enforcement remains
 *   patchy. Pre-registration and open science norms represent a sunset
 *   pathway, creating a scaffold perspective where organized agents are
 *   building alternative verification mechanisms.
 *
 * KEY AGENTS:
 *   - Career-Incentivized Researchers: Primary beneficiaries (institutional/arbitrage) — accrue citation advantage and publication velocity from positive results obtained through uncorrected multiple testing; can exit to pre-registered research but face opportunity costs
 *   - Field Statistical Integrity: Primary victim (powerless/trapped) — abstract collective good bearing accumulated false positive contamination; no exit option, no advocates, bears costs across generations
 *   - Replication & Null Result Researchers: Secondary victims (powerless/trapped) — encounter false positives from literature, face career penalties for null findings; publication bias and career structure trap them in low-status research
 *   - Methodologically Aware Mid-Career Researchers: Moderate/constrained agents — benefit from hypothesis testing coordination but bear costs of proper statistical correction; constrained by resource limitations and career risk
 *   - Open Science Coalition: Organized agents (organized/constrained) — pre-registration platforms, registered reports, replication networks building alternative pathways with sunset logic; see exit path as structured and visible
 *   - Journal Editorial Systems: Institutional gatekeeper (institutional/arbitrage) — maintains performative statistical rigor through inconsistent correction enforcement; benefits from appearance of methodological gatekeeping while actual enforcement remains weak
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(multiple_comparisons_problem, 0.58).
domain_priors:suppression_score(multiple_comparisons_problem, 0.65).
domain_priors:theater_ratio(multiple_comparisons_problem, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(multiple_comparisons_problem, extractiveness, 0.58).
narrative_ontology:constraint_metric(multiple_comparisons_problem, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(multiple_comparisons_problem, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(multiple_comparisons_problem, tangled_rope).
narrative_ontology:human_readable(multiple_comparisons_problem, "Multiple Comparisons Problem in Statistical Inference").
narrative_ontology:topic_domain(multiple_comparisons_problem, "statistics/science_methodology").

domain_priors:requires_active_enforcement(multiple_comparisons_problem).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(multiple_comparisons_problem, positive_result_claimants).
narrative_ontology:constraint_beneficiary(multiple_comparisons_problem, career_incentivized_researchers).
narrative_ontology:constraint_victim(multiple_comparisons_problem, field_statistical_integrity).
narrative_ontology:constraint_victim(multiple_comparisons_problem, replication_researchers).
narrative_ontology:constraint_victim(multiple_comparisons_problem, null_result_bearers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: FIELD STATISTICAL INTEGRITY (SNARE) — The epistemic commons cannot exit the false positive contamination problem. As researchers conduct multiple comparisons without correcting alpha thresholds, spurious correlations accumulate in the published literature. The field bears the full cost: literature contamination, wasted replication effort, cascading methodological distrust. No exit option exists for the collective; suppression is structural — alternative statistical procedures exist but career incentives suppress their adoption.
constraint_indexing:constraint_classification(multiple_comparisons_problem, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: REPLICATION & NULL RESULT RESEARCHERS (SNARE) — Trapped by publication bias and career penalties. When attempting replication, they encounter false positives from the literature and bear the cost of negative or null results. Career advancement, grant funding, and publication opportunities penalize null findings. No exit without career sacrifice. Suppression is institutional: journals deprioritize replication work, funding agencies reward novelty over confirmation.
constraint_indexing:constraint_classification(multiple_comparisons_problem, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 3: CAREER-INCENTIVIZED RESEARCHERS (ROPE) — From immediate time horizon, the multiple comparisons constraint is experienced as enabling coordination: testing multiple hypotheses is scientifically useful. They can exit by moving to fields with pre-registration norms or by adopting Bonferroni corrections voluntarily. Net beneficiary: citation advantage for surprising positive results, career acceleration through publication velocity. Sees the constraint as natural scientific freedom rather than extractive mechanism.
constraint_indexing:constraint_classification(multiple_comparisons_problem, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: METHODOLOGICALLY AWARE MID-CAREER RESEARCHERS (TANGLED ROPE) — Face real costs of proper statistical correction (reduced statistical power, harder to reach publication threshold) but also benefit from the coordination function (genuine hypothesis testing is enabled). High suppression: adopting stricter alpha corrections reduces probability of positive findings, threatening career continuation. Constrained by resource and career limitations, but some agency to implement corrections. Mixed experience of coordination benefit and extraction cost.
constraint_indexing:constraint_classification(multiple_comparisons_problem, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: OPEN SCIENCE COALITION (SCAFFOLD) — Pre-registration platforms (OSF, AsPredicted), registered report protocols, and replication networks (Many Labs, Psychological Science Accelerator) create alternative verification pathways with sunset logic. Organized agents see the multiple comparisons problem as a temporary coordination failure being solved through structural reform. As pre-registration norms mature, the ability to claim spurious positives through hidden multiple testing declines. Suppression decreases as norms embed. Exit path is visible and structured — sunset estimated at 15-20 years for full norm internalization in psychology; longer in biology and physics.
constraint_indexing:constraint_classification(multiple_comparisons_problem, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: JOURNAL EDITORIAL SYSTEMS (PITON) — The editorial process claims to enforce statistical rigor through peer review, but in practice applies correction standards inconsistently. The ritual persists (desk rejects for 'lack of novelty' of replication studies, positive bias in acceptance) despite reduced functional verification power. Theater ratio high: editors maintain the performance of methodological gatekeeping while actual correction enforcement remains patchy. Institutional inertia maintains the performative gate.
constraint_indexing:constraint_classification(multiple_comparisons_problem, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational/universal perspective, the multiple comparisons problem appears as an immutable mathematical truth: when testing N independent hypotheses at alpha=0.05, expected false positives approach N×0.05 as N grows. This perspective naturalizes the constraint as an inherent property of statistical inference itself. However, the structural data contradicts mountain classification — the false positive accumulation is not a mathematical law but a contingent consequence of choices about alpha correction, publication norms, and career incentives. The engine detects this as a false summit.
constraint_indexing:constraint_classification(multiple_comparisons_problem, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(multiple_comparisons_problem_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(multiple_comparisons_problem, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(multiple_comparisons_problem, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(multiple_comparisons_problem, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(multiple_comparisons_problem, TR),
    TR >= 0.70.

:- end_tests(multiple_comparisons_problem_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The multiple comparisons problem creates real asymmetry: uncorrected testing without correction is easier and faster than properly corrected testing, creating career incentives for the former. The extraction is not total (pre-registration and correction methods exist) but is substantial because the default career path (maximize positive findings) aligns with the exploitative direction. The measurement trajectory shows increasing extractiveness over the interval (0.35→0.58) as hypothesis testing has become more routine and computational power has enabled mining larger datasets for correlations without pre-specification. Suppression (0.65): High. Multiple barriers suppress adoption of proper correction: reduced statistical power under Bonferroni or FDR corrections makes publications harder; pre-registration imposes upfront planning costs; journals and funding agencies reward novelty and positive results over methodological rigor. Publication bias against null results creates career risk for replication researchers. These barriers are institutional and psychological, not material — agents could theoretically exit, but at high cost. Theater ratio (0.68): High. Editorial peer review claims to enforce statistical standards (alpha correction, multiple comparison awareness) but in practice applies these standards inconsistently. Positive results pass through with minimal correction scrutiny; replication studies face desk rejection for 'lack of novelty'; methodological rigor is discussed but not reliably enforced. The gatekeeping ritual persists despite reduced functional capacity to distinguish true signals from noise in exploratory studies.
 *
 * PERSPECTIVAL GAP:
 *   The tangled rope classification hinges on a genuine perspectival gap: the same structural feature (freedom to test multiple hypotheses) appears as enabling coordination to the researcher and as false positive contamination to the field. The researcher sees hypothesis testing as inherently multi-pronged and iterative — part of the scientific process. The field sees post-hoc multiple testing as mining for noise. Both perspectives are empirically grounded. The resolution is not to eliminate hypothesis testing (coordination function is real) but to separate exploratory testing (acknowledging multiple comparisons) from confirmatory testing (with proper alpha correction). Pre-registration achieves this separation structurally. The scaffold perspective shows this is achievable — organized agents are building norms and institutions that preserve the coordination function while suppressing the extraction mechanism.
 *
 * DIRECTIONALITY LOGIC:
 *   Career-incentivized researchers with institutional power and arbitrage options (can move to pre-registered fields or adopt corrections voluntarily without career-ending costs) experience low effective extraction despite moderate base extractiveness, because they have exit options and perceive the constraint as coordination. Replication researchers and null-result bearers with powerless status and trapped options (no exit without career sacrifice) experience maximum extraction, because they face both the costs of false positive contamination and publication bias penalties. Methodologically aware mid-career researchers with moderate power and constrained options experience moderate extraction — they recognize the problem and can implement corrections, but face real costs in statistical power and publication prospects. The directionality derivation flows from beneficiary/victim designation and exit options: those who benefit from uncorrected testing without incurring correction costs are net beneficiaries (low d); those who bear costs without capturing benefits are net victims (high d).
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled rope classification successfully resolves the mandatrophy by showing that the constraint has BOTH a genuine coordination function (testing multiple hypotheses is how science advances) AND an asymmetric extraction mechanism (uncorrected testing creates false positive bias that benefits some researchers and harms field integrity). Neither pure coordination (rope) nor pure extraction (snare) captures the constraint's structure. The coordination function cannot be eliminated without eliminating hypothesis testing; the extraction cannot be eliminated without proper alpha correction. The constraint is fundamentally hybrid. Pre-registration and open science norms represent not elimination of the constraint but structural separation: the coordination function (hypothesis testing) is preserved, the extraction mechanism (hidden multiple testing) is suppressed through pre-commitment. The sunset logic is real — as pre-registration norms mature, the extraction gradient flattens because the ability to profit from uncorrected testing declines.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    alpha_correction_adoption_threshold,
    'What adoption rate of proper alpha correction (Bonferroni, FDR, pre-registration) would functionally eliminate the multiple comparisons extraction mechanism?',
    'Survey of statistical practice across fields; tracking of pre-registration adoption rates and their correlation with replication success; empirical measurement of false positive rates in journals before and after correction norm adoption',
    'If threshold < 30% adoption: correction remains niche practice, extraction persists. If threshold > 60% adoption: extraction mechanism degrades substantially. Below 30%, the problem is structural; above 60%, the problem becomes non-compliance rather than design.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alpha_correction_adoption_threshold, empirical, 'Adoption threshold for alpha correction eliminating extraction').

omega_variable(
    publication_bias_persistence,
    'Does publication bias (preferential acceptance of positive results) persist even when researchers adopt pre-registration and proper multiple comparison corrections?',
    'Meta-analysis of registered report acceptance rates vs rejection rates by result type (significant/null/negative); longitudinal tracking of null result acceptance rates before and after pre-registration norm adoption',
    'If bias persists post-correction: the multiple comparisons problem is secondary to a deeper extraction mechanism (publication bias) — constraint should decompose. If bias substantially declines: scaffold perspective confirmed — pre-registration addresses the core extraction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(publication_bias_persistence, empirical, 'Whether publication bias persists post-correction').

omega_variable(
    effect_size_inflation_independence,
    'Is the problem fundamentally about false positives from multiple testing, or is it also about effect size inflation in the studies that do produce positive results due to publication selection?',
    'Comparison of effect sizes in published positive results vs pre-registered replications; meta-analysis of effect size distributions in confirmatory vs exploratory research; measurement of correlation between number of comparisons tested and magnitude of reported effect sizes',
    'If primarily multiple testing false positives: current constraint formulation is accurate. If effect size inflation is equally important: constraint should decompose into ''multiple comparisons false positives'' (ε≈0.35) and ''effect size inflation via publication selection'' (ε≈0.55).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(effect_size_inflation_independence, empirical, 'Whether effect size inflation is independent mechanism').

omega_variable(
    field_specific_norm_divergence,
    'Why do some fields (psychology, genetics) show rapid pre-registration adoption while others (physics, chemistry) show minimal adoption despite equivalent statistical risks?',
    'Qualitative analysis of field cultures, funder mandates, and journal policies; historical tracking of adoption rates by field; surveys of researcher perceptions of pre-registration burden and value',
    'If due to technical barriers: constraint mechanics are field-universal. If due to cultural factors: the constraint is partially contingent on field-specific norms — some fields may resolve the extraction via cultural reform faster than others.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(field_specific_norm_divergence, conceptual, 'Field-specific divergence in pre-registration adoption').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(multiple_comparisons_problem, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mcp_tr_t0, multiple_comparisons_problem, theater_ratio, 0, 0.52).
narrative_ontology:measurement(mcp_tr_t10, multiple_comparisons_problem, theater_ratio, 10, 0.62).
narrative_ontology:measurement(mcp_tr_t20, multiple_comparisons_problem, theater_ratio, 20, 0.68).

% Extraction over time
narrative_ontology:measurement(mcp_be_t0, multiple_comparisons_problem, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(mcp_be_t10, multiple_comparisons_problem, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(mcp_be_t20, multiple_comparisons_problem, base_extractiveness, 20, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(multiple_comparisons_problem, information_standard).
narrative_ontology:affects_constraint(multiple_comparisons_problem, publication_bias_in_peer_review).
narrative_ontology:affects_constraint(multiple_comparisons_problem, p_hacking_and_garden_of_forking_paths).
narrative_ontology:affects_constraint(multiple_comparisons_problem, file_drawer_problem).

% DUAL FORMULATION NOTE:
% The multiple comparisons problem is upstream of three related statistical constraints. Publication bias (downstream) amplifies the extraction effect by penalizing null results. P-hacking and researcher degrees of freedom (downstream) are the behavioral mechanism by which researchers exploit the multiple comparisons freedom. The file drawer problem (downstream) is the publication outcome of false positive accumulation. Each has its own extractiveness value reflecting its structural role: multiple comparisons problem (ε=0.58) provides the opportunity; p-hacking (ε=0.62) is the behavioral exploitation; publication bias (ε=0.70) is the institutional amplifier.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
