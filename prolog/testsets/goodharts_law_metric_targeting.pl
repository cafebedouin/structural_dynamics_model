% ============================================================================
% CONSTRAINT STORY: goodharts_law_metric_targeting
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_goodharts_law_metric_targeting, []).

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
 *   constraint_id: goodharts_law_metric_targeting
 *   human_readable: Goodhart's Law: Metric Targeting and Goal Substitution
 *   domain: institutional_management/measurement_systems
 *
 * SUMMARY:
 *   Goodhart's Law — 'When a measure becomes a target, it ceases to be a good
 *   measure' — describes a fundamental constraint in institutional
 *   measurement systems. Organizational structures that rely on centralized
 *   performance metrics to coordinate activity create a systematic incentive:
 *   agents optimize for the metric rather than the underlying goal. This
 *   constraint exhibits six distinct classifications depending on observer
 *   position and time horizon. The trap's sophistication is that it appears
 *   to be a law of nature (immutable mathematical relationship between
 *   observation and optimization) while actually being an extractive
 *   institutional arrangement with genuine beneficiaries (metric
 *   administrators and gamers) and clear victims (target populations and
 *   actual goal achievement). The constraint's theater ratio has risen from
 *   0.52 to 0.85 over its 30-year interval, indicating increasing gap between
 *   reported and actual performance. This metric substitution is itself the
 *   extractive mechanism: resources and attention are diverted from genuine
 *   goal achievement to metric optimization, creating what Goodhart called
 *   the 'cobra effect' — where the solution becomes worse than the original
 *   problem.
 *
 * KEY AGENTS:
 *   - Target Population: Primary victims (powerless/trapped) — patients, students, workers whose welfare the metric ostensibly measures but who cannot exit or contest metric design
 *   - Frontline Practitioners: Secondary victims (moderate/constrained) — teachers, doctors, managers who must meet metrics to survive institutionally despite misalignment with actual goals
 *   - Metric Designers and Administrators: Primary beneficiaries (institutional/arbitrage) — gain control, visibility, and leverage from centralized measurement; experience metrics as pure coordination with no extraction cost
 *   - Metric Gamers and Optimization Specialists: Extractive beneficiaries (powerful/mobile) — consulting firms and algorithm designers who profit from helping organizations appear to meet metrics without achieving actual goals
 *   - Institutional Legacy Systems: Inertial actors (institutional/arbitrage) — performance measurement persists through institutional inertia despite documented degradation across all domains
 *   - Organized Reform Movements: Reformers (organized/constrained) — measurement reform advocates who identify both coordination function and extraction pathology; seek alternative measurement regimes
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing contingent institutional design as immutable law
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(goodharts_law_metric_targeting, 0.62).
domain_priors:suppression_score(goodharts_law_metric_targeting, 0.68).
domain_priors:theater_ratio(goodharts_law_metric_targeting, 0.81).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(goodharts_law_metric_targeting, extractiveness, 0.62).
narrative_ontology:constraint_metric(goodharts_law_metric_targeting, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(goodharts_law_metric_targeting, theater_ratio, 0.81).

% --- Constraint claim ---
narrative_ontology:constraint_claim(goodharts_law_metric_targeting, tangled_rope).
narrative_ontology:human_readable(goodharts_law_metric_targeting, "Goodhart's Law: Metric Targeting and Goal Substitution").
narrative_ontology:topic_domain(goodharts_law_metric_targeting, "institutional_management/measurement_systems").

domain_priors:requires_active_enforcement(goodharts_law_metric_targeting).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(goodharts_law_metric_targeting, metric_administrators).
narrative_ontology:constraint_beneficiary(goodharts_law_metric_targeting, metric_gamers).
narrative_ontology:constraint_victim(goodharts_law_metric_targeting, actual_goal_achievement).
narrative_ontology:constraint_victim(goodharts_law_metric_targeting, measurement_credibility).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: TARGET POPULATION (SNARE) — The population whose welfare the metric ostensibly measures (patients in health systems, students in schools, workers in productivity schemes) cannot exit the constraint. Their interests are subordinated to metric optimization. Suppression is structural: they cannot opt out of the system, cannot contest metric definitions, cannot escape misalignment between measured and actual outcomes. Maximum extraction with zero agency.
constraint_indexing:constraint_classification(goodharts_law_metric_targeting, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: FRONTLINE PRACTITIONERS (TANGLED ROPE) — Teachers, doctors, managers experience genuine coordination value (metrics enable communication across hierarchy, align resource allocation, facilitate comparison) alongside asymmetric extraction. They must meet metrics to survive institutionally, yet metrics often misalign with their actual goals. Exit is costly (career damage, loss of income) but possible. They experience moderate effective extraction with some agency.
constraint_indexing:constraint_classification(goodharts_law_metric_targeting, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: METRIC DESIGNERS AND ADMINISTRATORS (ROPE) — Experience the constraint as pure coordination. Metrics solve the genuine problem of aggregating performance across decentralized organizations. Net beneficiaries with no extraction cost — they gain control, visibility, and leverage. Arbitrage exit (can redefine metrics, change measurement systems) allows them to avoid the constraint's costs entirely.
constraint_indexing:constraint_classification(goodharts_law_metric_targeting, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: METRIC GAMERS AND OPTIMIZATION SPECIALISTS (SNARE) — Powerful actors (consulting firms, algorithm designers, incentive engineers) who game metrics for profit or status. They experience the constraint as pure extraction machinery they operate: creating ways for organizations to appear to meet metrics without achieving actual goals. High effective extraction, minimal suppression (they have agency and mobility), but they are the primary beneficiary class, not victims. Reclassified as snare from their perspective as victimizers rather than victims.
constraint_indexing:constraint_classification(goodharts_law_metric_targeting, snare,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: INSTITUTIONAL LEGACY SYSTEMS (PITON) — Performance measurement as a core institutional ritual persists despite Goodhart degradation being documented across all domains (education, healthcare, law enforcement, environmental management). The metric system is maintained through institutional inertia: organizations cannot conceive of alternatives, performance reviews are performed because 'that's what we do,' and the theater ratio remains high even as functional measurement decays. Sunset is absent — the constraint perpetuates indefinitely.
constraint_indexing:constraint_classification(goodharts_law_metric_targeting, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ORGANIZED REFORM MOVEMENTS (TANGLED ROPE) — Measurement reform advocates (goodhart.io, measurement commons, anti-metric activism) identify the coordination function (metrics do solve multi-level communication problems) but also the extraction pathology (metrics create perverse incentives). They see a constraint that could be restructured: multi-dimensional measurement, outcome triangulation, leading indicators, internal validity checks. Constrained by institutional resistance and lack of enforcement power, but organized enough to see exit paths.
constraint_indexing:constraint_classification(goodharts_law_metric_targeting, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / GOODHART FORMALISM (MOUNTAIN) — From first-principles analysis, Goodhart's Law states a mathematical truth: any metric that becomes a target ceases to be a good metric. The constraint appears immutable because it derives from fundamental properties of measurement, feedback, and optimization. 'The moment a measure becomes a target, it ceases to be a good measure.' This is a formal logical statement about the relationship between observation and optimization. However, the structural data (moderate suppression, high theater, presence of beneficiaries and victims) indicates this perspective naturalizes a social arrangement: the inevitability of metric targeting is not a law of physics but a consequence of institutional design choices (centralized metric definition, performance-based rewards, lack of alternative measurement regimes).
constraint_indexing:constraint_classification(goodharts_law_metric_targeting, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(goodharts_law_metric_targeting_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(goodharts_law_metric_targeting, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(goodharts_law_metric_targeting, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(goodharts_law_metric_targeting, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(goodharts_law_metric_targeting, TR),
    TR >= 0.70.

:- end_tests(goodharts_law_metric_targeting_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62): High-moderate. The constraint extracts significant value through metric substitution: resources and attention diverted from genuine goal achievement to metric optimization, career penalties for practitioners who prioritize actual goals over metrics, gaming industry profits. However, extraction is not total (0.66+, which would be a pure snare) because the metric system does provide genuine coordination value — it enables multi-level communication and resource allocation. Suppression (0.68): High. Multiple barriers prevent exit or resistance. Target populations are trapped by economic dependence, legal frameworks, or geographic isolation. Practitioners face career damage for missing metrics despite valid reasons. Metric systems are legally mandated in many domains (healthcare, education, criminal justice). Alternative measurement approaches face institutional resistance. Theater ratio (0.81): Very high and increasing. Modern performance metrics show massive gap between reported and actual outcomes. Test score inflation without reading comprehension gains (education), hospital readmission gaming through patient selection (healthcare), crime classification manipulation (law enforcement), environmental metric gaming through loopholes (sustainability). The theater has increased steadily as optimization sophistication increases — agents become better at satisfying metrics without achieving goals. This measurement signature is diagnostic of Goodhart degradation.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates sharp perspectival divergence. Metric administrators see pure coordination (Rope) — they are solving the real problem of multi-level organizational communication. Target populations see pure extraction (Snare) — their interests are subordinated to metric optimization with no exit or voice. Frontline practitioners see hybrid coordination and extraction (Tangled Rope) — metrics do enable legitimate communication but also create perverse incentives. Organized reformers see a system that could be restructured (Tangled Rope) — they identify both the coordination function and extraction pathology. The civilizational analytical perspective risks seeing an immutable law (Mountain) — Goodhart's Law as a mathematical truth about measurement and optimization — but the structural data (clear beneficiaries gaining at victims' expense, alternatives theoretically possible, theater ratio increasing) suggests this is a false summit naturalizing institutional arrangements.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) for each perspective is derived from structural position relative to the extraction flow. Metric administrators and designers benefit from centralized control (d ≈ 0.15, institutional arbitrage beneficiaries — low extraction experienced). Metric gamers actively operate the extraction machinery (d ≈ 0.10, powerful extractors). Frontline practitioners are caught between metric requirements and goal achievement (d ≈ 0.70, moderate power, constrained exit, dual victim/partial beneficiary status). Target populations bear maximum extraction cost (d ≈ 0.95, powerless, trapped, no benefit). The analytical observer treats the constraint as mathematical law (d ≈ 0.50, analytical symmetry) but this derivation should be overridden by the structural data showing clear beneficiaries and victims. The override corrects for the mountain perspective's naturalization bias.
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLUTION: The mandatrophy is resolved by recognizing that Goodhart's Law operates at two levels. At the formal level, the law is a true mathematical relationship: as metric optimization increases, the correlation between metric and goal achievement decreases. This is a logical necessity, not an empirical claim. However, the constraint story is not about this formal relationship — it is about institutional systems that enforce metric targeting, suppress alternatives, and create extraction through goal substitution. The institutional constraint (Tangled Rope/Snare) is analytically distinct from the formal law (Mountain). The false summit occurs when analysts treat the formal law as though it implies institutional inevitability: 'Goodhart's Law means metric targeting is unavoidable.' This naturalizes the social choice to rely on centralized performance metrics and to tie compensation to metrics. Alternative institutional designs are possible: decentralized measurement, multi-dimensional assessment, outcome triangulation, professional autonomy, intrinsic motivation structures. These reduce but do not eliminate Goodhart effects. The constraint's classification is Tangled Rope at institutional level (genuine coordination function with embedded extraction) and the mountain perspective is correctly flagged as a false summit that mistakes mathematical relationship for institutional necessity.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    goodhart_formalism_vs_institutional_choice,
    'Is Goodhart''s Law a mathematical invariant or a contingent feature of centralized performance measurement systems?',
    'Comparative institutional analysis: organizations with decentralized, multi-dimensional, qualitative measurement systems vs centralized single-metric systems; longitudinal tracking of metric validity decay rates across measurement regimes',
    'If mathematical invariant: the mountain classification is correct and metric targeting is inherently unavoidable. If institutional choice: the constraint is contingent on centralized metric authority and accountability structures, enabling redesign pathways that reduce extraction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(goodhart_formalism_vs_institutional_choice, conceptual, 'Whether Goodhart''s Law is a mathematical law or institutional design artifact').

omega_variable(
    theater_ratio_sustainability,
    'Can the theatrical performance of metric systems (appearing to achieve goals without achieving them) be sustained indefinitely, or does legitimacy decay force eventual system collapse or redesign?',
    'Historical analysis of metric system lifespans; measurement of public trust in institutional performance claims over time; correlation between perceived Goodhart gaming and policy legitimacy crises',
    'If theatrical performance is indefinitely sustainable: piton classification confirmed, institutions will perpetually use degraded metrics. If legitimacy decays: scaffold classification may apply, forcing periodic reformulation cycles.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(theater_ratio_sustainability, empirical, 'Sustainability of theatrical metric performance without legitimacy collapse').

omega_variable(
    alternative_measurement_regime_effectiveness,
    'Do multi-dimensional, qualitative, stakeholder-negotiated measurement systems actually reduce Goodhart targeting, or do they simply hide gaming in harder-to-audit dimensions?',
    'Comparative effectiveness of: single-metric systems vs outcome triangulation; quantitative vs mixed-methods measurement; top-down vs participatory metric definition. Track actual goal achievement (not reported metrics) across regimes.',
    'If alternatives are effective: reform movements (organized/constrained) have real exit paths and tangled rope classification is correct. If gaming persists: all measurement systems are equally compromised, and snare classification is more accurate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_measurement_regime_effectiveness, empirical, 'Whether alternative measurement regimes reduce Goodhart targeting').

omega_variable(
    incentive_structure_necessity,
    'Is performance-based compensation and punishment (the institutional mechanism driving metric gaming) a necessary feature of large-scale coordination, or an optional design choice?',
    'Organizational comparison: institutions using non-metric-based incentives (internal motivation, professional norms, intrinsic mission alignment) vs metric-tied rewards; measurement of goal achievement, employee satisfaction, and metric validity across regimes',
    'If necessary: extraction is unavoidable and snare/tangled rope classifications are correct. If optional: the constraint is architecturally contingent and could be dissolved by decoupling metrics from compensation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(incentive_structure_necessity, preference, 'Whether performance metrics must be tied to compensation and punishment').

omega_variable(
    suppression_mechanism_internalized_vs_structural,
    'Do target populations accept metric targeting because of structural inability to exit (economic dependence, legal prohibition, geographic isolation) or because they have internalized the metric system''s legitimacy frame?',
    'Surveys and ethnographic work tracking whether resistance persists after structural barriers are removed; analysis of whether metric legitimacy declines when actual outcomes diverge from reported metrics',
    'If structural: suppression is external and could be reduced by enabling exit. If internalized: population accepts metric substitution as legitimate, requiring different reform approaches (identity reframing, critical consciousness-raising).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_mechanism_internalized_vs_structural, empirical, 'Whether suppression of metric targeting resistance is structural or internalized').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(goodharts_law_metric_targeting, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(goodhart_tr_t0, goodharts_law_metric_targeting, theater_ratio, 0, 0.52).
narrative_ontology:measurement(goodhart_tr_t10, goodharts_law_metric_targeting, theater_ratio, 10, 0.68).
narrative_ontology:measurement(goodhart_tr_t20, goodharts_law_metric_targeting, theater_ratio, 20, 0.81).
narrative_ontology:measurement(goodhart_tr_t30, goodharts_law_metric_targeting, theater_ratio, 30, 0.85).

% Extraction over time
narrative_ontology:measurement(goodhart_be_t0, goodharts_law_metric_targeting, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(goodhart_be_t10, goodharts_law_metric_targeting, base_extractiveness, 10, 0.5).
narrative_ontology:measurement(goodhart_be_t20, goodharts_law_metric_targeting, base_extractiveness, 20, 0.62).
narrative_ontology:measurement(goodhart_be_t30, goodharts_law_metric_targeting, base_extractiveness, 30, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(goodharts_law_metric_targeting, resource_allocation).
narrative_ontology:affects_constraint(goodharts_law_metric_targeting, performance_incentive_misalignment).
narrative_ontology:affects_constraint(goodharts_law_metric_targeting, institutional_measurement_theater).
narrative_ontology:affects_constraint(goodharts_law_metric_targeting, goal_substitution_in_bureaucracy).

% DUAL FORMULATION NOTE:
% Goodhart's Law has two structurally distinct formulations: (1) the formal mathematical relationship between metric and goal (mountain-like necessity), and (2) the institutional system that enforces metric targeting and suppresses alternatives (tangled rope/snare). These are separate constraint stories. This story focuses on the institutional constraint. The formal mathematical relationship story would classify differently and have lower extractiveness.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(goodharts_law_metric_targeting, analytical, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
