% ============================================================================
% CONSTRAINT STORY: organizational_productivity_ceiling
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_organizational_productivity_ceiling, []).

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
 *   constraint_id: organizational_productivity_ceiling
 *   human_readable: Organizational Productivity Ceiling
 *   domain: organizational_dynamics/management_systems
 *
 * SUMMARY:
 *   The organizational productivity ceiling emerges from the structural
 *   tension between the need to coordinate large-scale human effort and the
 *   tools available for measurement-based management. As organizations grow
 *   beyond direct supervision capacity, they adopt metrics-based performance
 *   management to maintain coherence. This creates a compound constraint: (1)
 *   genuine coordination of complex interdependencies requires shared
 *   visibility into output and capacity, and (2) the measurement system
 *   itself incentivizes gaming, report optimization, and intensity escalation
 *   that extract value from workers while degrading actual productivity. The
 *   constraint manifests as a ceiling because organizations discover that
 *   pushing metrics-based productivity targets eventually produces zero or
 *   negative returns on actual business output — the measurement
 *   infrastructure consumes resources, worker autonomy declines, quality
 *   degrades, and turnover rises. Yet the constraint persists because the
 *   executive layer experiences it as pure coordination and the metrics
 *   bureaucracy maintains itself through institutional inertia. Theater ratio
 *   (0.58) reflects that measurement infrastructure spends 58% of its effort
 *   on performance demonstration rather than performance improvement —
 *   dashboards, review meetings, compliance documentation, and reporting all
 *   create theater rather than enabling action.
 *
 * KEY AGENTS:
 *   - Frontline Workers: Primary victims (powerless/trapped) — bear escalating productivity demands and surveillance; trapped by employment dependency with no organizational exit option
 *   - Middle Managers: Secondary victims (moderate/constrained) — enforce metrics while constrained by career dependence on those same metrics; experience both coordination and extraction
 *   - Executive Leadership: Primary beneficiaries (institutional/arbitrage) — capture organizational coherence gains and can reposition if the constraint becomes unfavorable; arbitrage exits available
 *   - Labor Organizations: Organized agents (organized/constrained) — see the ceiling as temporary and building alternative coordination pathways with clear sunset
 *   - Metrics Bureaucracy: Institutional infrastructure (institutional/arbitrage) — self-maintaining performance measurement system experiencing piton degradation
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing measurement-based coordination failures as inherent laws rather than contingent architectural choices
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(organizational_productivity_ceiling, 0.52).
domain_priors:suppression_score(organizational_productivity_ceiling, 0.65).
domain_priors:theater_ratio(organizational_productivity_ceiling, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(organizational_productivity_ceiling, extractiveness, 0.52).
narrative_ontology:constraint_metric(organizational_productivity_ceiling, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(organizational_productivity_ceiling, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(organizational_productivity_ceiling, tangled_rope).
narrative_ontology:human_readable(organizational_productivity_ceiling, "Organizational Productivity Ceiling").
narrative_ontology:topic_domain(organizational_productivity_ceiling, "organizational_dynamics/management_systems").

domain_priors:requires_active_enforcement(organizational_productivity_ceiling).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(organizational_productivity_ceiling, management_layer).
narrative_ontology:constraint_beneficiary(organizational_productivity_ceiling, coordination_infrastructure).
narrative_ontology:constraint_victim(organizational_productivity_ceiling, frontline_workers).
narrative_ontology:constraint_victim(organizational_productivity_ceiling, output_quality).
narrative_ontology:constraint_victim(organizational_productivity_ceiling, worker_autonomy).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: FRONTLINE WORKER (SNARE) — Trapped by employment dependency and skill-area specialization. Faces escalating productivity metrics, performance monitoring, and output quotas with no meaningful exit option. Bears the full cost of ceiling maintenance through intensified work demands, reduced autonomy, and surveillance overhead. Maximum experienced extraction.
constraint_indexing:constraint_classification(organizational_productivity_ceiling, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: MIDDLE MANAGER (TANGLED ROPE) — Constrained by career dependence on the metrics they enforce and relocation costs of seeking other roles. Experiences both genuine coordination (translating strategy into execution, resource allocation) and asymmetric extraction (pressured to hit targets through worker intensification rather than systems improvement). Moderate agency with significant extraction pressure.
constraint_indexing:constraint_classification(organizational_productivity_ceiling, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: EXECUTIVE LEADERSHIP (ROPE) — Benefits from the productivity ceiling architecture. Experiences the constraint as pure coordination: the metrics system enables reporting, benchmarking, and optimization decisions. High mobility — can reposition to different organizations or roles if this constraint becomes unfavorable. Net beneficiary with low experienced extraction.
constraint_indexing:constraint_classification(organizational_productivity_ceiling, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: LABOR ORGANIZATION (SCAFFOLD) — Organized agents (unions, worker advocacy groups) perceive the productivity ceiling as a temporary coordination failure with policy sunset potential. See alternative pathways: four-day weeks, output-based (vs. time-based) compensation, worker co-governance of metrics. Constraint has structural exit: labor norms are shifting toward discretionary work models that bypass the ceiling architecture. This is a genuine scaffold — high suppression is tolerated only because the coalition sees a deadline for the mechanism's obsolescence (15-25 years).
constraint_indexing:constraint_classification(organizational_productivity_ceiling, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: METRICS BUREAUCRACY (PITON) — The performance measurement infrastructure (KPIs, dashboards, quarterly reviews) is substantially performative. Theater ratio (0.58) reflects that much of the measurement activity is about demonstrating performance rather than enabling improvement. The bureaucracy persists through institutional inertia — it has become an end in itself, disconnected from actual productivity impact. Metrics are maintained because 'this is how we manage' rather than because they work.
constraint_indexing:constraint_classification(organizational_productivity_ceiling, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational/universal perspective, Goodhart's Law creates a structural ceiling on productivity: as metrics become targets, they cease to be good metrics. The measurement system inevitably degrades into optimization for the metric rather than for the underlying goal. This perspective sees the ceiling as an immutable law of any measurement-based management system. However, the structural data reveals this as a false summit — the extractive layer and suppression mechanisms are contingent institutional choices, not laws of measurement.
constraint_indexing:constraint_classification(organizational_productivity_ceiling, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(organizational_productivity_ceiling_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(organizational_productivity_ceiling, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(organizational_productivity_ceiling, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(organizational_productivity_ceiling, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(organizational_productivity_ceiling, TR),
    TR >= 0.70.

:- end_tests(organizational_productivity_ceiling_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The productivity ceiling extracts value through intensified work demands, reduced autonomy, and time spent on performance reporting rather than actual output. However, extractiveness is not as high as a pure snare (0.66+) because the system does produce genuine coordination value for large organizations — middle managers and executives experience real benefits in planning and decision-making. The extraction is a side effect of the coordination mechanism, not its primary function, though the mechanism is heavily exploited for extraction. Suppression (0.65): High. Barriers to exit and alternative arrangements include: employment dependency (health insurance, savings, mortgage), labor market concentration in the region or sector, legal constraints on employment terms, and internalized productivity culture. Suppression is not total (0.75+) because workers retain some negotiation capacity and alternative employers exist, but suppression is substantial. Theater ratio (0.58): Moderate. The metrics infrastructure involves genuine performance measurement (dashboards reflecting actual output) mixed with performative compliance (metrics reviews, standardized reports, compliance documentation that demonstrate rather than enable improvement). The ratio has increased from 0.35 (six time periods ago) as organizations layer new measurement tools on top of existing ones without retiring old systems — accretion rather than refinement.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap between the frontline worker (Snare) and executive leadership (Rope) is the core diagnostic feature. The same productivity ceiling infrastructure appears as pure extraction (Snare) to the trapped worker and as pure coordination (Rope) to the arbitrage-mobile executive. This gap is structurally real — the infrastructure genuinely enables executive coordination and genuinely extracts from workers. The tangled rope classification (claimed type) represents the analytical position that the constraint is primarily hybrid: genuine coordination function mixed with systematic extraction. The piton classification (metrics bureaucracy perspective) reveals that much of the performance measurement infrastructure is degraded and maintained through institutional inertia rather than functional necessity. The scaffold classification (labor organization) is crucial — it indicates that workers with organized power perceive a genuine sunset horizon and are building alternative coordination mechanisms (autonomy-based, outcome-only, co-governed metrics) that can replace the current architecture within a generation.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values (d) capture each actor's structural position relative to the extraction flow. Frontline workers with trapped exit options and victim status have d ≈ 0.95, producing maximum f(d) and maximum experienced chi — they bear the full ceiling weight. Middle managers with constrained exits and mixed beneficiary/victim status have d ≈ 0.55, producing moderate chi — they enforce the ceiling but also suffer under it. Executives with arbitrage exits and beneficiary status have d ≈ 0.15, producing low or negative chi — they experience the ceiling as enabling, not burdening. The labor organization with constrained exits but organized power has d ≈ 0.40, producing moderate chi that reflects their organized resistance capacity. The directionality derivation is supported by the structural relationship declarations: executives clearly benefit from metrics visibility and coordination infrastructure; frontline workers clearly bear extraction through intensified demands. This asymmetry is the analytical center of the constraint.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint avoids mandatrophy by explicitly acknowledging both the genuine coordination function and the asymmetric extraction. The tangled rope classification does not collapse into either pure rope (implying coordination-only) or pure snare (implying extraction-only). The measurement evidence supports the hybrid classification: theater ratio (0.58) shows mixed performative/functional content; extractiveness (0.52) is elevated but not snare-level (0.66+); suppression (0.65) is high but reflects both structural and internalized components. The scaffold perspective confirms that alternative coordination mechanisms exist and are becoming viable — this is not an immutable constraint but a temporary institutional arrangement. The piton perspective identifies the measurement bureaucracy as degraded infrastructure maintained through inertia. The mountain perspective (Goodhart's Law as immutable) is explicitly flagged as a false summit — measurement-based coordination has inherent challenges, but the ceiling is not an unchangeable law. The constraint resolves mandatrophy by distinguishing between unavoidable measurement challenges (real but manageable) and contingent institutional choices (removable through deliberate redesign).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    genuine_productivity_vs_metric_gaming,
    'How much of measured productivity gains represent genuine output improvement versus metric optimization and reporting artifacts?',
    'Longitudinal comparison of reported metrics vs. customer satisfaction scores, product quality measures, and repeat business rates; analysis of metric revision frequency and direction',
    'If genuine gains dominate: constraint enables real coordination (higher rope classification). If gaming dominates: constraint is primarily extractive cover for reduced actual output (higher snare classification). Current data suggests 60/40 split gaming/genuine.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(genuine_productivity_vs_metric_gaming, empirical, 'Genuine productivity gains versus metric gaming').

omega_variable(
    suppression_mechanism_internalization,
    'Is the measured suppression (0.65) structural (legal/economic barriers to exit) or internalized (workers have internalized the productivity culture as legitimate)?',
    'Post-exit suppression trajectory: track workers after leaving the organization — do suppression patterns persist (internalized) or dissipate (structural)? Compare quit rates during economic downturns (structural pressure) vs. baseline (internalization degree).',
    'If structural dominates: workers retain agency and can exit at cost. If internalized dominates: suppression travels with the worker — the constraint has captured identity/values. Higher internalization shifts piton classification (degraded) toward snare (extraction with cognitive capture).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalization, empirical, 'Whether suppression is structural or internalized').

omega_variable(
    alternative_coordination_viability,
    'Do alternative coordination mechanisms (trust-based autonomy, outcome-only metrics, worker-governed target-setting) produce comparable coordination efficiency to the current ceiling architecture?',
    'Comparative case studies from organizations using alternative models (Spotify, Gore-Tex, Basecamp); measurement of coordination failure rates, communication overhead, and execution fidelity across models',
    'If alternatives are viable: scaffold sunset is real and the constraint is temporary (15-25 year horizon credible). If alternatives fail: the ceiling is structurally necessary for large-scale coordination (constraint becomes more mountain-like, less scaffold-like).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(alternative_coordination_viability, empirical, 'Viability of alternative coordination mechanisms').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(organizational_productivity_ceiling, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(orgprod_tr_t0, organizational_productivity_ceiling, theater_ratio, 0, 0.35).
narrative_ontology:measurement(orgprod_tr_t3, organizational_productivity_ceiling, theater_ratio, 3, 0.48).
narrative_ontology:measurement(orgprod_tr_t6, organizational_productivity_ceiling, theater_ratio, 6, 0.58).

% Extraction over time
narrative_ontology:measurement(orgprod_be_t0, organizational_productivity_ceiling, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(orgprod_be_t3, organizational_productivity_ceiling, base_extractiveness, 3, 0.45).
narrative_ontology:measurement(orgprod_be_t6, organizational_productivity_ceiling, base_extractiveness, 6, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(organizational_productivity_ceiling, resource_allocation).
narrative_ontology:affects_constraint(organizational_productivity_ceiling, worker_autonomy_erosion).
narrative_ontology:affects_constraint(organizational_productivity_ceiling, metrics_gaming_feedback_loop).
narrative_ontology:affects_constraint(organizational_productivity_ceiling, middle_management_role_strain).

% DUAL FORMULATION NOTE:
% The organizational productivity ceiling decomposes into three structurally distinct constraints: (1) worker_autonomy_erosion (ε≈0.58) — direct extraction of discretion; (2) metrics_gaming_feedback_loop (ε≈0.65) — measurement system that incentivizes report optimization; (3) middle_management_role_strain (ε≈0.48) — conflicted position enforcing metrics while experiencing their burden. Each has distinct beneficiaries and exit pathways. This story captures the aggregate constraint; decomposed stories address individual mechanisms.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(organizational_productivity_ceiling, institutional, 0.18).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
