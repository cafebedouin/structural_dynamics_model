% ============================================================================
% CONSTRAINT STORY: pareto_principle
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_pareto_principle, []).

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
 *   constraint_id: pareto_principle
 *   human_readable: The Pareto Principle (80/20 Rule)
 *   domain: statistical/economic/social
 *
 * SUMMARY:
 *   The Pareto Principle is a statistical observation that has evolved from a
 *   descriptive pattern into a prescriptive justification for inequality.
 *   When Vilfredo Pareto observed that wealth concentrated in roughly 80/20
 *   proportions across multiple societies, he identified an empirical
 *   regularity. Over the twentieth century, the principle became
 *   institutionalized as an explanation for why concentration is inevitable —
 *   a pseudo-law of nature. This constraint exhibits a critical ambiguity:
 *   whether the 80/20 pattern is an immutable mathematical consequence of
 *   certain generative processes (power law distributions emerging from
 *   preferential attachment, multiplicative growth), a contingent outcome of
 *   specific institutional structures (hierarchical reward systems, network
 *   effects, first-mover advantage in markets), or a measurement-dependent
 *   artifact of how we choose to count 'causes' and 'consequences'. The
 *   constraint's theater ratio has increased over the interval (0.35 to 0.58)
 *   as the principle became invoked more frequently in business literature
 *   and policy analysis as an explanation for inequality, often with
 *   increasingly abstract justifications divorced from actual empirical
 *   measurement. The extractiveness has risen modestly (0.28 to 0.38) as the
 *   principle has been weaponized to discourage egalitarian redistribution
 *   policies — the principle functions as both a coordination insight for
 *   those optimizing within the constraint and as a suppression mechanism for
 *   those bearing the asymmetry.
 *
 * KEY AGENTS:
 *   - Dispersed Majority: Primary victim (powerless/trapped) — the 80% bearing consequences; atomized, lacking collective exit mechanism
 *   - Concentrated Resource Holders: Primary beneficiary (institutional/arbitrage) — the 20% capturing 80% of outcomes; benefits from principle as optimization guide and legitimization
 *   - Labor Organizing Coalition: Organized secondary agent (organized/constrained) — sees both coordination value and extraction; builds alternative collective action but faces regulatory embedding of hierarchy
 *   - Mid-Performing Individual: Moderate agent (moderate/mobile) — experiences mixed extraction and benefit; has partial exit options but concentration persists across domains
 *   - Alternative Distribution Movement: Powerful explorers (powerful/mobile) — building exits through cooperative structures, commons, flat hierarchies; treats principle as provisional
 *   - Economic Science Institution: Institutional maintainer (institutional/constrained) — holds principle as explanatory law through textbooks, education, policy discourse; theater-heavy, degraded function
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks false mountain classification by naturalizing institutional outcomes as mathematical law
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(pareto_principle, 0.38).
domain_priors:suppression_score(pareto_principle, 0.42).
domain_priors:theater_ratio(pareto_principle, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(pareto_principle, extractiveness, 0.38).
narrative_ontology:constraint_metric(pareto_principle, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(pareto_principle, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(pareto_principle, tangled_rope).
narrative_ontology:human_readable(pareto_principle, "The Pareto Principle (80/20 Rule)").
narrative_ontology:topic_domain(pareto_principle, "statistical/economic/social").

domain_priors:requires_active_enforcement(pareto_principle).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(pareto_principle, concentrated_resource_holders).
narrative_ontology:constraint_beneficiary(pareto_principle, power_law_exploiters).
narrative_ontology:constraint_victim(pareto_principle, dispersed_majority).
narrative_ontology:constraint_victim(pareto_principle, resource_scarcity_bears).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DISPERSED MAJORITY (SNARE) — The 80% bearing consequences from distributed effort have no collective exit mechanism. Atomized, unorganized, cannot coordinate to escape the asymmetric outcome distribution. Bears the full cost of concentration without capacity to reorganize around alternative distributions. Maximum experienced extraction due to trapped status and powerlessness.
constraint_indexing:constraint_classification(pareto_principle, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: CONCENTRATED RESOURCE HOLDER (ROPE) — Primary beneficiary. Experiences the principle as a coordination insight: identifying which 20% of inputs drive 80% of outputs enables efficient optimization. The constraint functions as a pure discovery tool for this agent — leveraging the asymmetry creates value. Net beneficiary with full arbitrage capacity.
constraint_indexing:constraint_classification(pareto_principle, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: LABOR ORGANIZING COALITION (TANGLED ROPE) — Organized agents with partial exit options see both coordination benefit (understanding productivity concentration) and extraction mechanism (the principle legitimizes inequality). Can mobilize collective action but faces pushback from concentrated beneficiaries who benefit from the principle as a justification for hierarchical reward. Constrained exit due to regulatory and cultural embedding of meritocratic hierarchy.
constraint_indexing:constraint_classification(pareto_principle, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: MID-PERFORMING INDIVIDUAL (TANGLED ROPE) — Individual agents in the 20-80% range experience both benefit and extraction. They are above the median (some coordination value from recognizing concentration) but below the concentrated beneficiaries (extraction of surplus value). Mobile exit option (can relocate, change sectors) but constrained by economic reality that the 80/20 concentration persists across most domains. Mixed extraction and coordination.
constraint_indexing:constraint_classification(pareto_principle, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(regional))).

% PERSPECTIVE 5: ALTERNATIVE DISTRIBUTION MOVEMENT (SCAFFOLD) — Powerful but not institutional actors (researchers, platform designers, cooperative advocates) see the Pareto principle as a contingent outcome of specific institutional structures (capitalist incentive systems, network effects, first-mover advantage), not as law. Building alternative platforms (cooperatives, commons-based peer production, flat-hierarchy organizations) that produce different distributions. Theater ratio for this perspective is low — the constraint is treated as provisional, with sunset through institutional experimentation. Has real exit pathways through alternative organizational forms.
constraint_indexing:constraint_classification(pareto_principle, scaffold,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: ECONOMIC SCIENCE INSTITUTION (PITON) — Academic and policy institutions have largely internalized the Pareto principle as an explanatory law ('power law distributions are universal'), maintaining its prominence through textbooks, business education, and policy analysis. The principle persists through institutional inertia despite mounting empirical evidence that many supposedly 80/20 distributions are contingent outcomes of specific reward structures rather than natural laws. Theater ratio high: the principle is invoked ritually to explain inequality as inevitable rather than as structurally maintained. Degraded function — the principle's capacity to predict has declined as institutional variation increases.
constraint_indexing:constraint_classification(pareto_principle, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / MATHEMATICAL VIEW (MOUNTAIN) — From a sufficiently abstract mathematical perspective, power law distributions emerge from simple generative processes (preferential attachment, multiplicative growth, selection effects). The Pareto distribution is a consequence of mathematical/statistical law — unavoidable given the constraints of finite resources and multiplicative growth. However, this analysis faces a critical challenge: the engine's false summit detector identifies this as naturalization of empirically contingent phenomena. Real-world distributions only approximate Pareto when specific institutional conditions hold; alternative institutional structures produce radically different distributions (e.g., Nordic income distribution, open-source contributor distribution, Wikipedia editor distribution).
constraint_indexing:constraint_classification(pareto_principle, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(pareto_principle_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(pareto_principle, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(pareto_principle, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(pareto_principle, TR),
    TR >= 0.70.

:- end_tests(pareto_principle_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The Pareto principle creates real asymmetry in outcomes — the top 20% do capture disproportionate shares in most measurable domains. However, the extraction is not pure: the principle enables genuine optimization for those managing complex systems (identifying high-impact effort). The principle also coordinates around a shared expectation, which has coordination value. The score reflects that extractiveness has increased over the interval as the principle has been institutionalized more explicitly as an inequality justification. Suppression (0.42): Moderate. Significant barriers to escaping Pareto concentration include: (a) network effects that reinforce first-mover advantage, (b) capital accumulation that compounds advantage, (c) cultural normalization of hierarchy as natural/inevitable, (d) institutional structures (corporate equity, venture funding) designed to concentrate, (e) narrative closure (the principle presented as law rather than choice). However, suppression is not total — cooperatives, open-source projects, and alternative distribution mechanisms demonstrate that different structures produce different distributions. Theater ratio (0.58): Moderate-high. The principle is increasingly invoked as a ritualistic explanation for inequality without empirical measurement of whether the actual distribution in a specific domain follows 80/20 (often it doesn't closely). Business literature, policy analysis, and economic textbooks cite the principle as explanatory law even when measuring different distributions. The theater has increased as the principle has become a cultural trope — used to close discussion rather than to analyze specific outcomes. Claimed type (Tangled Rope): The principle exhibits both coordination function (identifying high-impact interventions) and extraction mechanism (justifying inequality). Both beneficiaries and victims are essential to the story. Active enforcement (true) reflects that the principle is maintained through educational institutions, policy discourse, and business practice — it requires continual rearticulation to persist.
 *
 * PERSPECTIVAL GAP:
 *   The Pareto principle demonstrates extreme perspectival divergence. For the concentrated beneficiary (top 20%), it is a coordination mechanism and efficiency insight — a rope. For the dispersed majority, it is a snare: they bear the asymmetric consequences with no exit or reorganization option. For organized labor, it is a tangled rope: they see both the coordination value (identifying priority areas for labor organizing) and the extraction mechanism (the principle is used to justify why inequality is inevitable). For alternative distribution movements, the principle is a scaffold: it describes outcomes under current institutions, but deliberately different institutional structures (cooperatives, commons, flat hierarchies) produce genuinely different distributions, with sunset as these alternatives scale. For the economic science institution, the principle has become piton: it is maintained through textbooks and policy discourse despite mounting empirical evidence that distributions vary with institutional choice. For the analytical observer, there is a temptation to see the principle as a mountain (power laws are mathematically universal), but the engine's false summit detector identifies this as naturalization — real-world 80/20 concentrations only appear when specific institutional conditions align (capitalist markets, network effects, hierarchical reward systems). Remove those conditions and the distribution changes.
 *
 * DIRECTIONALITY LOGIC:
 *   The Pareto principle's directionality is complex because its effect depends on the agent's structural position relative to concentration. For institutional beneficiaries (top 20% resource holders), the principle yields low directionality (d ~ 0.15) — they benefit from both the insight and the justification, and they have arbitrage options (can apply the principle across multiple domains). For powerless agents in the dispersed majority, the principle yields high directionality (d ~ 0.90) — they cannot exit, bear the costs of asymmetric distribution, and have no mechanism to reorganize. For organized agents (labor movements, cooperatives), directionality is moderate (d ~ 0.45-0.55) — they can mobilize collective action against the principle's implications, have constrained exit options through alternative institutional forms, but face resistance from beneficiaries and cultural embedding of the principle as natural law. The sigmoid function f(d) converts these directionality values to experienced extractiveness multipliers: high d (trapped victims) produces f(d) ~ 1.15-1.42, making the principle feel like maximum coercion; low d (institutional beneficiaries) produces f(d) ~ -0.01, making the principle feel like pure coordination; medium d (organized agents) produces f(d) ~ 0.65-0.75, making it feel like mixed extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   The Pareto principle resolves the mandatrophy by demonstrating that the classification depends critically on the resolution of omega variables. If the principle is empirically universal (Omega 1 resolved to 'universal'), then it approaches mountain status — distribution concentration would be immutable. However, the current evidence suggests the principle is institutionally contingent: Nordic welfare states, open-source projects, and deliberately cooperative organizations produce radically different distributions. This makes the principle a tangled rope maintained by specific institutional choices. The mandatrophy is resolved by recognizing that the beneficiary (concentrated resource holders) benefits from the coordination function (optimization of high-impact effort) AND from the extraction mechanism (justification of inequality). The victim (dispersed majority) experiences the extraction without exit. The organized agent (labor coalition) can theoretically exit through institutional redesign, which explains the scaffold perspective and the alternative distribution movement's real (if difficult) exit pathways. The piton classification reflects that the principle's explanatory power has degraded — it is invoked ritualistic­ally to close discussion rather than to analyze specific outcomes. The false mountain (analytical observer) reveals the most critical insight: the principle is not a law of nature but a contingent outcome of specific institutional structures. Its apparent universality is an artifact of institutional homogeneity — once institutions diverge, distributions diverge.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    empirical_universality_boundary,
    'Is the 80/20 ratio an empirical universal (appearing across all human domains) or a domain-contingent outcome of specific institutional structures (capitalist markets, corporate hierarchies, network effects)?',
    'Cross-domain empirical survey: income (Gini coefficient, lorenz curves), firm size (Zipf exponent), scientific productivity (citations, papers), open-source contributions, cooperative organization outcomes, welfare state redistributed outcomes, wikipedia editor activity. Comparison of Pareto exponents across institutional contexts.',
    'If universal: the principle approaches mountain status (immutable law of resource distribution). If contingent: the principle is a tangled rope maintained by specific institutional choices, with alternative distributions achievable through institutional redesign. Would shift multiple perspectives from snare toward rope or scaffold.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(empirical_universality_boundary, empirical, 'Whether 80/20 is empirically universal or institutionally contingent').

omega_variable(
    causality_direction_ambiguity,
    'Does the Pareto principle cause inequality, or does inequality cause the observable Pareto pattern? Is concentration of resources the driver or the consequence?',
    'Historical analysis of institutional adoption of incentive structures that concentrate reward (corporate equity vesting, venture capital allocation, academic citation systems) and timing of observable Pareto distributions. Controlled institutional experiments (open-source projects, cooperative workplaces, flat-hierarchy firms) tracking whether removing concentrating incentives produces different distributions.',
    'If principle causes inequality: the constraint is a structuring force (tangled rope, snare). If inequality causes the observable pattern: the principle is a descriptive summary of outcomes from other constraints (redundant constraint, piton). Classification would shift: if redundant, the principle itself degrades to piton (explanatory theater, but no independent extraction mechanism).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(causality_direction_ambiguity, empirical, 'Whether Pareto principle causes or describes inequality').

omega_variable(
    measurement_dependent_definition,
    'Does the 80/20 ratio vary with which outcome measure is chosen (income vs wealth vs opportunity vs influence)? Is the principle measurement-robust or measurement-dependent?',
    'For a single population (e.g., US workers, internet users, academic institutions), compute Pareto exponent for multiple outcome measures: income, wealth, opportunity access, influence on decisions, time allocated to them, attention received. Check whether the 80/20 splits remain consistent across measures or whether they vary wildly.',
    'If measurement-dependent: the principle is not a constraint but a label for ''inequality tends to concentrate'' (true but not actionable). Different measures would require different constraint stories. If robust: the principle is a unified structural phenomenon. This determines whether to decompose into multiple constraints or maintain one story.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(measurement_dependent_definition, empirical, 'Whether 80/20 ratio is robust across outcome measures').

omega_variable(
    institutional_redesign_feasibility,
    'Can institutions deliberately design for equitable distributions (e.g., cooperative income ratios, lottery-based allocation, ranked-choice governance) without sacrificing coordination or productivity?',
    'Case studies of deliberately equitable institutions (worker cooperatives, nonprofit organizations, open-source projects with flat attribution, academic departments with shared governance). Comparison of productivity, innovation rates, retention, member satisfaction, and actual income distribution outcomes vs corporate hierarchies.',
    'If feasible: the scaffold perspective is grounded; alternative distributions are real exits from the Pareto constraint. If infeasible: concentration is inevitable (mountain). Most likely: feasible for specific domains (creative commons, volunteer projects) but not scaled to all economic activity (suggests domain-specific decomposition rather than universal constraint).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_redesign_feasibility, empirical, 'Whether institutions can deliberately design equitable distributions without losing productivity').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(pareto_principle, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pareto_tr_t0, pareto_principle, theater_ratio, 0, 0.35).
narrative_ontology:measurement(pareto_tr_t5, pareto_principle, theater_ratio, 5, 0.47).
narrative_ontology:measurement(pareto_tr_t10, pareto_principle, theater_ratio, 10, 0.58).

% Extraction over time
narrative_ontology:measurement(pareto_be_t0, pareto_principle, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(pareto_be_t5, pareto_principle, base_extractiveness, 5, 0.33).
narrative_ontology:measurement(pareto_be_t10, pareto_principle, base_extractiveness, 10, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(pareto_principle, resource_allocation).
narrative_ontology:affects_constraint(pareto_principle, wealth_inequality_amplification).
narrative_ontology:affects_constraint(pareto_principle, network_effects_concentration).
narrative_ontology:affects_constraint(pareto_principle, corporate_hierarchy_legitimation).

% DUAL FORMULATION NOTE:
% The Pareto principle decomposes into at least two distinct constraints: (1) Power law distributions in resource allocation (empirical/mathematical constraint, ε ~ 0.15, mountain or rope depending on whether the power law is universal or institutional), and (2) The principle as institutional justification for inequality (cultural/political constraint, ε ~ 0.42, tangled rope/snare depending on agent position). The present story focuses on the second decomposition — the principle as a constraint on redistribution and egalitarian institutional redesign. Stories on the mathematical/statistical constraint would have lower extractiveness and different perspectives.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(pareto_principle, institutional, 0.18).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
