% ============================================================================
% CONSTRAINT STORY: goodharts_law_institutional_metrics
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_goodharts_law_institutional_metrics, []).

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
 *   constraint_id: goodharts_law_institutional_metrics
 *   human_readable: Goodhart's Law in Institutional Metrics
 *   domain: institutional_governance/performance_measurement
 *
 * SUMMARY:
 *   Goodhart's Law states that once a measure becomes a target, it ceases to
 *   be a good measure. This constraint captures how institutional metrics
 *   designed to coordinate behavior and align incentives become extractive
 *   mechanisms when optimization incentives distort the underlying mission.
 *   The constraint appears differently across institutional positions:
 *   administrators see coordination (Rope), front-line workers see mixed
 *   coordination and coercion (Tangled Rope), measured populations see pure
 *   extraction (Snare), and analytical observers risk seeing an immutable
 *   logical law (Mountain) rather than a contingent institutional
 *   arrangement. The theater_ratio increases monotonically from 0.35 (newly
 *   implemented metrics with tight mission coupling) to 0.78 (legacy metrics
 *   performing pure ritual verification with no connection to actual
 *   outcomes). Extractiveness also rises over the interval (0.28 to 0.58) as
 *   metric-optimization incentives increasingly drive behavior away from
 *   mission goals. The constraint exhibits the classic DR family structure: a
 *   coordination function (alignment of distributed agents) is nested within
 *   an extraction mechanism (capture of that alignment function for
 *   institutional gatekeeping authority), which enables a theater mechanism
 *   (ritual compliance masking mission degradation).
 *
 * KEY AGENTS:
 *   - Metric Administrators: Primary beneficiary (institutional/arbitrage) — capture authority and resource control through metric design; can shift frameworks without incurring costs
 *   - Institutional Gatekeepers: Primary beneficiary (institutional/arbitrage) — use metrics to regulate access, performance, and resource allocation; extract legitimacy from measurement authority
 *   - Measured Population: Primary victim (powerless/trapped) — subject to metric-driven service degradation with no exit option or voice
 *   - Front-Line Service Workers: Secondary victim (moderate/constrained) — trapped between mission goals and metric targets; bear costs of metric-chasing via performance management
 *   - Reform Movements & Auditors: Organized agents (organized/constrained) — attempting to build outcome-focused alternatives; see sunset clause in metric lifecycles
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing institutional measurement choices as logical impossibilities
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(goodharts_law_institutional_metrics, 0.58).
domain_priors:suppression_score(goodharts_law_institutional_metrics, 0.65).
domain_priors:theater_ratio(goodharts_law_institutional_metrics, 0.78).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(goodharts_law_institutional_metrics, extractiveness, 0.58).
narrative_ontology:constraint_metric(goodharts_law_institutional_metrics, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(goodharts_law_institutional_metrics, theater_ratio, 0.78).

% --- Constraint claim ---
narrative_ontology:constraint_claim(goodharts_law_institutional_metrics, tangled_rope).
narrative_ontology:human_readable(goodharts_law_institutional_metrics, "Goodhart's Law in Institutional Metrics").
narrative_ontology:topic_domain(goodharts_law_institutional_metrics, "institutional_governance/performance_measurement").

domain_priors:requires_active_enforcement(goodharts_law_institutional_metrics).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(goodharts_law_institutional_metrics, metric_administrators).
narrative_ontology:constraint_beneficiary(goodharts_law_institutional_metrics, institutional_gatekeepers).
narrative_ontology:constraint_victim(goodharts_law_institutional_metrics, actual_institutional_mission).
narrative_ontology:constraint_victim(goodharts_law_institutional_metrics, measured_population_welfare).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: MEASURED POPULATION (SNARE) — Trapped within institutional metrics that distort the services meant to benefit them. No exit option; subject to metric-driven incentive realignment that degrades actual welfare. High suppression: cannot voice that metrics have decoupled from their nominal purpose. Cannot exit the institution or the measurement regime.
constraint_indexing:constraint_classification(goodharts_law_institutional_metrics, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: FRONT-LINE SERVICE WORKERS (TANGLED ROPE) — Constrained by performance metrics that are both coordinating (ensuring minimal service standards) and extractive (enforcing metric-chasing over mission delivery). Cannot exit careers easily; coordination function genuine but buried under extraction. High suppression via performance management.
constraint_indexing:constraint_classification(goodharts_law_institutional_metrics, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: INSTITUTIONAL DESIGNERS (ROPE) — Perceive metrics as pure coordination mechanism for aligning distributed agents toward nominal goals. Have arbitrage exit: if metrics degrade, designers can shift to new measurement regimes. High information advantage; low experienced extraction. Extract value through measurement authority.
constraint_indexing:constraint_classification(goodharts_law_institutional_metrics, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: REFORM MOVEMENTS (SCAFFOLD) — Organized agents (auditors, effectiveness researchers, outcome-measurement reformers) see Goodhart's Law as a temporary measurement failure with a sunset: holistic evaluation, mixed methods, outcome-tracking over output metrics. Building alternative frameworks. Suppression is real but declining as measurement literacy improves.
constraint_indexing:constraint_classification(goodharts_law_institutional_metrics, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: LEGACY METRIC FRAMEWORKS (PITON) — Persist through institutional inertia despite recognized degradation. Everyone acknowledges metrics no longer measure what they claim, yet the apparatus persists. Theater ratio is very high (0.78): metric collection is largely performative ritual. Replacement frameworks exist; inertia prevents migration.
constraint_indexing:constraint_classification(goodharts_law_institutional_metrics, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW (MOUNTAIN) — From a logical/mathematical perspective, any proxy metric for an unmeasurable quantity will eventually be gamed. This is an iron law of measurement: once a measure becomes a target, it ceases to be a good measure. Appears as an immutable logical truth. Engine will detect as false summit — the 'inevitability' naturalizes contingent institutional choices about measurement authority.
constraint_indexing:constraint_classification(goodharts_law_institutional_metrics, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(goodharts_law_institutional_metrics_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(goodharts_law_institutional_metrics, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(goodharts_law_institutional_metrics, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(goodharts_law_institutional_metrics, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(goodharts_law_institutional_metrics, TR),
    TR >= 0.70.

:- end_tests(goodharts_law_institutional_metrics_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderately high. The constraint extracts institutional authority through the measurement system itself — metrics create the basis for gatekeeping and resource allocation control. However, the extraction is not as severe as pure Snare (ε ≥ 0.66) because the coordination function is genuine: metrics do align distributed agents, even if that alignment is toward gaming rather than mission. The rise from 0.28 to 0.58 over the interval reflects increasing decoupling: early metrics were tightly coupled to mission, later metrics are optimization targets divorced from original intent. Suppression (0.65): High. Measured populations cannot voice that metrics have decoupled from mission goals without risking institutional punishment (defunding, loss of services, reputational damage). Front-line workers cannot refuse to optimize for metrics without career consequences. The suppression mechanism includes both structural (institutional control of alternatives) and cognitive (internalization that metric optimization is legitimate). Theater ratio (0.78): Very high. Legacy metric frameworks persist despite universal acknowledgment that they no longer measure intended outcomes. The metric collection apparatus is largely performative — it validates institutional decisions already made and masks their divergence from stated mission. The high theater indicates Piton classification at institutional timescales: metrics survive through inertia, not function.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exemplifies the full range of DR types across institutional positions. Administrators designing new metrics experience Rope: they solve a coordination problem (aligning distributed agents) with minimal coercion, believing the metrics genuinely reflect institutional mission. Front-line workers experience Tangled Rope: the metrics coordinate their behavior AND extract performance metrics used against them in career decisions. Measured populations experience Snare: they cannot exit the measurement regime or voice objections without institutional retaliation; the metrics increasingly drive service degradation (metric-gaming behavior) while suppressing their complaints. Reform movements experience Scaffold: they see metric systems as temporary coordination failures with sunset clauses (new frameworks like outcome-tracking, mixed-methods evaluation, participatory measurement) that will eventually replace legacy metrics. Institutional legacy frameworks exhibit Piton dynamics: everyone acknowledges the metrics are degraded and performative, yet they persist through inertia despite cheaper, better alternatives existing. The analytical observer risks Mountain: seeing Goodhart's Law as a logical necessity (any proxy metric will be gamed, mathematical certainty) rather than a contingent institutional problem (specific measurement authority structures enable gaming; different structures resist it).
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary-victim mapping drives directionality differentiation. Metric administrators and gatekeepers benefit from measurement authority (d ≈ 0.15 for institutional/arbitrage: low extraction from their perspective). Front-line workers are constrained but not fully trapped; they face high costs to exit but retain some agency (d ≈ 0.60: moderate extraction). Measured populations are trapped with no exit option (d ≈ 0.95: maximum extraction). The analytical observer has analytical exit options (d ≈ 0.72: moderate extraction). The reform/audit coalition is organized with constrained exit (d ≈ 0.45: moderate extraction). Directionality distribution is wide, explaining the perspectival gap: the same institutional constraint produces Rope for beneficiaries, Tangled Rope for mixed stakeholders, Snare for trapped populations, and apparent Mountain for observers who fail to recognize contingency.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by recognizing that Goodhart's Law operates on a MEASUREMENT-LIFECYCLE gradient rather than being a single constraint type. At inception (t=0, theater=0.35, ε=0.28), metrics are primarily coordinating (Rope) because they are tightly coupled to mission and reflect genuine institutional goals. As metrics age and optimization incentives accumulate (t=5-10), they become Tangled Rope: the coordination function persists (metrics do align behavior) but is increasingly captured by extraction incentives (gaming behavior that decouples from mission). At maturity (t=15, theater=0.78, ε=0.58), they become Piton: the metric apparatus persists through inertia, not function, and is largely performative theater. The constraint's classification depends on measurement point in the lifecycle, not on a single eternal truth. Reform movements recognize this and propose Scaffold: deliberately designing sunset clauses into metric systems to trigger migration to new measurement frameworks before theater ratio becomes irreducibly high. The false Mountain classification (Goodhart's Law as logical law) naturalizes what is actually institutional path-dependence: the choice to build authority around metrics, rather than building authority around outcomes or participatory evaluation, makes the constraint appear inevitable when it is contingent.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    goodhart_empirical_vs_logical,
    'Is Goodhart''s Law an empirical phenomenon (metrics decouple due to strategic behavior) or a logical law (any proxy metric must eventually fail as a target)?',
    'Historical analysis of metric systems with different architectural constraints: immutable metrics vs revisable metrics, opaque optimization targets vs transparent ones, single-stakeholder vs multi-stakeholder systems. Identify institutional features that delay or prevent decoupling.',
    'If empirical: the constraint is a Tangled Rope driven by strategic behavior — architectural changes (transparency, metric rotation, outcome focus) can reduce extraction. If logical: the constraint is a Mountain — no architectural fix exists, only periodic metric replacement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(goodhart_empirical_vs_logical, conceptual, 'Whether Goodhart''s Law is empirical or logically necessary').

omega_variable(
    mission_specification_clarity,
    'Can the underlying institutional mission be specified clearly enough that any metric capturing it would resist gaming, or is mission inherently vague enough that metric proxies will always decouple?',
    'Systematic decomposition of stated mission into measurable outcomes; tracking whether high-quality specifications in some domains (public health, criminal justice, education) prevent Goodhartian decoupling better than low-specification domains (organizational culture, morale, trust)',
    'If mission specification prevents decoupling: constraint is solvable via better goal-clarification (Scaffold). If mission is irreducibly vague: constraint is inherent to complex institutional goals (Mountain or Piton).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mission_specification_clarity, empirical, 'Whether clearer mission specification can prevent metric decoupling').

omega_variable(
    suppression_mechanism_structural_vs_cognitive,
    'Is measured-population suppression driven by structural barriers to exit (institutional control, economic dependency) or cognitive/internalized suppression (belief that metric-optimized outcomes are legitimate even when harmful)?',
    'Comparative analysis: populations with genuine exit options (can switch institutions, can voice dissent) vs those without. Track whether metric decoupling is more readily acknowledged and corrected in high-exit environments.',
    'If structural: suppression continues unchanged after metric replacement (Snare). If cognitive: suppression persists even in low-barrier environments, indicating identity fusion or internalized metrics (identity_locked dynamics).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_vs_cognitive, empirical, 'Whether suppression is structural or internalized').

omega_variable(
    theater_ratio_rise_trajectory,
    'Does theater_ratio rise monotonically as metrics age, or does it plateau or oscillate depending on institutional renewal cycles?',
    'Longitudinal tracking of metric theater across institutional cohorts: new metrics vs 5-year-old vs 10-year-old vs 20-year-old measurement systems. Identify whether theater rise correlates with organizational reform cycles or crisis events.',
    'If monotonic rise: piton classification confirms — metrics degrade to pure ritual with age. If cyclical: constraint alternates between Rope (newly designed metrics) and Piton (degraded legacy metrics); suggests scaffold with natural sunset.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(theater_ratio_rise_trajectory, empirical, 'Theater ratio trajectory over metric lifecycle').

omega_variable(
    beneficial_gaming_ambiguity,
    'Can metric gaming sometimes align with institutional mission (accidentally) or produce secondary benefits that offset mission decay, or does optimization always degrade actual outcomes?',
    'Case studies of metric systems where strategic behavior produced outcomes better than intended; identification of unintended positive correlation between metric improvement and mission success. Cross-domain comparison.',
    'If gaming sometimes beneficial: extractiveness should be lower (0.35-0.45 range); some agents may see Rope or Scaffold instead of Snare. If always degrading: extractiveness confirmed; gaming is pure extraction mechanism.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(beneficial_gaming_ambiguity, empirical, 'Whether metric gaming can produce beneficial secondary effects').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(goodharts_law_institutional_metrics, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(goodhart_tr_t0, goodharts_law_institutional_metrics, theater_ratio, 0, 0.35).
narrative_ontology:measurement(goodhart_tr_t5, goodharts_law_institutional_metrics, theater_ratio, 5, 0.58).
narrative_ontology:measurement(goodhart_tr_t10, goodharts_law_institutional_metrics, theater_ratio, 10, 0.72).
narrative_ontology:measurement(goodhart_tr_t15, goodharts_law_institutional_metrics, theater_ratio, 15, 0.78).

% Extraction over time
narrative_ontology:measurement(goodhart_be_t0, goodharts_law_institutional_metrics, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(goodhart_be_t5, goodharts_law_institutional_metrics, base_extractiveness, 5, 0.42).
narrative_ontology:measurement(goodhart_be_t10, goodharts_law_institutional_metrics, base_extractiveness, 10, 0.55).
narrative_ontology:measurement(goodhart_be_t15, goodharts_law_institutional_metrics, base_extractiveness, 15, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(goodharts_law_institutional_metrics, enforcement_mechanism).
narrative_ontology:affects_constraint(goodharts_law_institutional_metrics, performance_management_pathology).
narrative_ontology:affects_constraint(goodharts_law_institutional_metrics, institutional_goalpost_shifting).
narrative_ontology:affects_constraint(goodharts_law_institutional_metrics, measurement_authority_concentration).

% DUAL FORMULATION NOTE:
% Goodhart's Law in institutional metrics is upstream of three downstream constraints: performance management systems that optimize for degraded metrics, goalpost-shifting by administrators to maintain the illusion of progress, and concentration of measurement authority. Each downstream constraint inherits the Goodhartian structure but amplifies it in specific institutional contexts (HR, resource allocation, evaluation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(goodharts_law_institutional_metrics, institutional, 0.2).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
