% ============================================================================
% CONSTRAINT STORY: goodhart_law_metric_erosion
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_goodhart_law_metric_erosion, []).

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
 *   constraint_id: goodhart_law_metric_erosion
 *   human_readable: Goodhart Law: Metric Erosion Through Optimization
 *   domain: institutional/organizational/policy
 *
 * SUMMARY:
 *   Goodhart's law — 'when a measure becomes a target, it ceases to be a good
 *   measure' — describes a structural constraint that operates across
 *   institutional scales: education systems gaming test scores, hospitals
 *   gaming readmission metrics, police departments gaming crime statistics,
 *   development organizations gaming impact metrics. The constraint exhibits
 *   a universal lifecycle: initial metric design solves a genuine
 *   coordination problem (organizations need comparable measures for
 *   accountability). Over time, subordinate units optimize for the metric,
 *   which erodes its validity as a proxy for actual outcomes. Administrators
 *   respond by adding new metrics, tightening definitions, or increasing
 *   audit frequency, which increases suppression and extraction. The theater
 *   ratio rises as everyone performs metric compliance while actual outcome
 *   measurement becomes increasingly theatrical. The constraint is not
 *   inevitable — some organizations and sectors successfully rotate metrics
 *   before erosion becomes severe, or shift to hybrid evaluation systems that
 *   lower optimization pressure. But the underlying tension is irreducible:
 *   measurement frameworks that enable coordination also enable behavior
 *   manipulation, and the erosion mechanism is structural to how incentives
 *   propagate.
 *
 * KEY AGENTS:
 *   - Subordinate Organizations: Primary victims (powerless/trapped) — must optimize for metrics to survive institutionally; participation erodes metric validity; exit costs institutional credibility
 *   - Metric Administrators: Primary beneficiaries (institutional/arbitrage) — control measurement framework; experience constraint as legitimate coordination infrastructure; can redefine metrics without subordinate consent
 *   - Field or Sector: Secondary actor (moderate/constrained) — faces mixed coordination (need for comparable measures) and extraction (metric optimization erodes field outcomes); can exit to alternative evaluation but at cost
 *   - Reform Coalitions: Organized agents (organized/mobile) — building alternative accountability systems (multi-metric dashboards, qualitative assessment); see metric erosion as solvable with sunset logic
 *   - Legacy Institutions: Institutional actors (institutional/constrained) — maintain eroded metrics through inertia; recognize degradation but lack will/resources for replacement; exhibit piton characteristics
 *   - Analytical Observer: Civilizational scope (analytical/analytical) — sees constraint as permanent structural feature of measurement-based governance, but identifies mitigation pathways (feedback loops, metric pluralism)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(goodhart_law_metric_erosion, 0.58).
domain_priors:suppression_score(goodhart_law_metric_erosion, 0.62).
domain_priors:theater_ratio(goodhart_law_metric_erosion, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(goodhart_law_metric_erosion, extractiveness, 0.58).
narrative_ontology:constraint_metric(goodhart_law_metric_erosion, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(goodhart_law_metric_erosion, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(goodhart_law_metric_erosion, tangled_rope).
narrative_ontology:human_readable(goodhart_law_metric_erosion, "Goodhart Law: Metric Erosion Through Optimization").
narrative_ontology:topic_domain(goodhart_law_metric_erosion, "institutional/organizational/policy").

domain_priors:requires_active_enforcement(goodhart_law_metric_erosion).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(goodhart_law_metric_erosion, metric_administrators).
narrative_ontology:constraint_beneficiary(goodhart_law_metric_erosion, goal_setters).
narrative_ontology:constraint_victim(goodhart_law_metric_erosion, actual_outcomes).
narrative_ontology:constraint_victim(goodhart_law_metric_erosion, subordinate_organizations).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SUBORDINATE ORGANIZATION (SNARE) — Trapped by metric-based performance evaluation. Must optimize for the assigned metric to survive, but optimization erodes the metric's validity as a proxy for real performance. No escape: exit the metric system and lose institutional credibility; stay and participate in the erosion. Coercion is administered through funding, promotion, and legitimacy gates. The constraint eliminates alternative accountability mechanisms.
constraint_indexing:constraint_classification(goodhart_law_metric_erosion, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: THE FIELD/SECTOR (TANGLED ROPE) — Faces real coordination problem: diverse organizations need comparable accountability measures. Metrics provide genuine coordination function (enabling comparison, aggregation, policy). But the coordination function is entangled with extraction: metric administrators capture authority and extract compliance labor, organizations must distort actual work to preserve metrics, and the field's epistemic health degrades. Exit is costly (loss of comparative standing) but possible (shift to qualitative assessment, local evaluation). Mixed experience.
constraint_indexing:constraint_classification(goodhart_law_metric_erosion, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: METRIC ADMINISTRATORS (ROPE) — Benefits from controlling the measurement framework. Experiences coordination: creating consistent metrics enables agency-wide oversight and resource allocation. Net beneficiary with arbitrage options (can switch metrics, redefine targets, claim exogenous changes). The constraint appears as legitimate management infrastructure, not extraction.
constraint_indexing:constraint_classification(goodhart_law_metric_erosion, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: LEGACY METRIC INSTITUTIONS (PITON) — Institutions maintaining old metrics despite acknowledged drift. Theater ratio (0.68) reflects performative maintenance: staff continue reporting metrics everyone knows are eroded, institutions continue treating them as meaningful, replacement systems are delayed indefinitely. The metric persists through inertia even when its functional value has collapsed.
constraint_indexing:constraint_classification(goodhart_law_metric_erosion, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: REFORM COALITIONS (SCAFFOLD) — Organized actors (measurement scientists, organizational behaviorists, reform administrations) see metric erosion as a solvable coordination problem with a sunset clause. Building alternative accountability mechanisms: multi-metric dashboards, outcome sampling, qualitative assessment integration. These parallel systems are gradually replacing pure metric optimization. Exit path is explicit: transition to hybrid evaluation over 10-15 years. High agency, declining suppression as alternatives mature.
constraint_indexing:constraint_classification(goodhart_law_metric_erosion, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (TANGLED ROPE) — From civilizational scope, Goodhart law reflects both a genuine coordination problem (how to measure complex outcomes across large systems) and an inherent extraction mechanism (whoever controls the measurement controls behavior). The constraint cannot be eliminated without losing the coordination function; optimization pressure is structural. But the erosion trajectory is not irreversible: feedback loops, metric rotation, and pluralism can slow degradation. Mitigation is possible but permanent tension remains.
constraint_indexing:constraint_classification(goodhart_law_metric_erosion, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(goodhart_law_metric_erosion_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(goodhart_law_metric_erosion, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(goodhart_law_metric_erosion, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(goodhart_law_metric_erosion, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(goodhart_law_metric_erosion, TR),
    TR >= 0.70.

:- end_tests(goodhart_law_metric_erosion_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high, increasing over time. Initial value (0.35) reflects genuine coordination function: metrics solve the real problem of comparing outcomes across decentralized units. As subordinate organizations optimize, erosion accumulates: metric becomes corrupted proxy, but extraction of compliance labor increases. Administrators capture authority over metric definition and revision. By observation point (0.58), the metric is significantly eroded but still enforced, producing net extraction of organizational autonomy and outcome distortion. Suppression (0.62): Moderate-high. Barriers to escape include funding conditionality on metric performance, career advancement tied to metric targets, institutional legitimacy requirements, and regulatory mandate. But suppression is not total — some organizations can negotiate metric modifications, reform coalitions are building alternatives, and metric cycling is possible. Theater ratio (0.68): High and increasing. Initially (0.38) metrics had genuine predictive validity. Over interval, gaming strategies proliferate, outcome measurement becomes divorced from actual performance, administrators continue ritual enforcement of eroded metrics despite awareness. Theater rises as gap between metric performance and actual outcomes widens but metric compliance persists as institutional requirement.
 *
 * PERSPECTIVAL GAP:
 *   The subordinate organization sees a snare: trapped by metric-based survival requirements, forced to participate in erosion. The metric administrator sees a rope: legitimate coordination infrastructure. The sector sees mixed coordination and extraction. The reform coalition sees a sunset opportunity — alternatives are buildable. The legacy institution sees inertial piton — we know metrics are eroded but institutional momentum perpetuates them. The analytical observer sees tangled rope with permanent tension: measurement coordination is necessary, but optimization pressure is inherent; mitigation is possible but cannot eliminate the underlying conflict. The perspectival gap reveals that the constraint cannot be resolved by simply eliminating metrics (loses coordination function) or by tightening them (accelerates erosion). The gap is unbridgeable from within a single-metric framework.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) in Goodhart erosion is determined by structural position in the metric hierarchy. Metric administrators have low d (beneficiaries with arbitrage exit) — they control definition and can defend metrics against challenge. Subordinate organizations have high d (victims trapped by performance requirements) — they must optimize regardless of erosion. The sector has intermediate d (faces both coordination benefit and extraction cost; constrained exit). The analytical observer has d ≈ 0.72 (external position, no direct stake but sees structure clearly). The reform coalition has low d (organized position with exit options — can build alternatives). Directionality feeds into the sigmoid f(d): trapped subordinates experience maximum χ; beneficiary administrators experience minimum or negative χ; organized reformers experience moderate χ with declining trajectory.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY PRESENT AND UNRESOLVED. Goodhart's law exhibits the core mandatrophy structure: the constraint is simultaneously a Rope (genuine coordination problem that justifies measurement) and a Snare (optimization erodes validity, forcing continued adjustment and increasing extraction). The tangled rope classification resolves this by acknowledging both dimensions are structural, not observational. However, the unresolved aspect is whether the constraint is *necessary* or *institutional*. A Mountain perspective (measurement is inherent to large-scale coordination) would claim the erosion cycle is immutable. The analytical tangled rope perspective denies this — the erosion is structural to metric-based governance specifically, not to coordination in general. Alternative governance models (outcome sampling, qualitative assessment, participatory evaluation) demonstrate that coordination is achievable without triggering the Goodhart erosion cycle. The mandatrophy is resolved by recognizing the constraint as contingent (tangled rope) rather than necessary (mountain), but this resolution depends on demonstrating that alternatives actually work, which is the role of the scaffold perspective and the reform coalition.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    coordination_necessity_vs_extraction,
    'Is the metric system primarily solving a coordination problem (organizations need comparable measures) or primarily enabling extraction (administrators control behavior)?',
    'Historical analysis: Did the metric system emerge from field consensus (coordination) or top-down mandate (extraction)? Do subordinate organizations retain input on metric design? Can metrics be revised without administrator approval?',
    'If primarily coordination: reclassify toward Rope from more perspectives; suppression decreases. If primarily extraction: maintain or increase Snare classifications; suppression represents core mechanism, not side effect.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(coordination_necessity_vs_extraction, empirical, 'Whether metrics serve genuine coordination or primarily enable extraction').

omega_variable(
    erosion_irreversibility,
    'Once a metric is optimized and eroded, can it be rehabilitated, or does it require permanent replacement?',
    'Measurement science analysis: Can metric definitions be refined, gaming patterns closed, or observer effects accounted for? Or is erosion fundamentally irreversible due to feedback adaptation?',
    'If reversible: metrics can be recycled; constraint is transient. If irreversible: metrics must be continuously rotated; constraint is permanent. Affects whether Scaffold''s sunset is genuine (alternative systems eventually replace) or aspirational (metric cycle repeats).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(erosion_irreversibility, empirical, 'Whether metric erosion is reversible or permanent').

omega_variable(
    administrator_awareness_paradox,
    'Do metric administrators genuinely believe in the metrics they enforce, or do they knowingly maintain eroded metrics for control purposes?',
    'Comparative institutional analysis: Document statements by administrators about metric validity; examine revisions/reforms initiated vs resisted; trace metric lifecycle when discovery of erosion occurs.',
    'If genuinely believed: administrators are victims of their own constraint (false consciousness), not beneficiaries; classification shifts toward institutional entrapment. If knowingly maintained: administrators are primary extractors; classification toward institutional beneficiary. Affects directionality for institutional perspectives.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(administrator_awareness_paradox, empirical, 'Whether metric administrators believe in metric validity').

omega_variable(
    multi_metric_sufficiency,
    'Do multi-metric dashboards, outcome sampling, and qualitative integration actually prevent Goodhart erosion, or do they simply distribute erosion across multiple measures?',
    'Empirical tracking of field transitions: Measure gaming behavior, outcome validity, and institutional learning in organizations/sectors that have shifted to hybrid evaluation. Compare erosion rate multi-metric systems vs single-metric systems over equivalent time horizons.',
    'If sufficiently preventive: Scaffold''s sunset is credible; Piton is transient. If erosion distributes rather than prevents: Piton and Snare phases cycle indefinitely; constraint is permanent (reclassify observed terminal type toward Snare or Piton).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(multi_metric_sufficiency, empirical, 'Whether multi-metric systems prevent or distribute Goodhart erosion').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(goodhart_law_metric_erosion, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(goodhart_tr_t0, goodhart_law_metric_erosion, theater_ratio, 0, 0.38).
narrative_ontology:measurement(goodhart_tr_t3, goodhart_law_metric_erosion, theater_ratio, 3, 0.53).
narrative_ontology:measurement(goodhart_tr_t6, goodhart_law_metric_erosion, theater_ratio, 6, 0.68).

% Extraction over time
narrative_ontology:measurement(goodhart_be_t0, goodhart_law_metric_erosion, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(goodhart_be_t3, goodhart_law_metric_erosion, base_extractiveness, 3, 0.47).
narrative_ontology:measurement(goodhart_be_t6, goodhart_law_metric_erosion, base_extractiveness, 6, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(goodhart_law_metric_erosion, information_standard).
narrative_ontology:affects_constraint(goodhart_law_metric_erosion, institutional_metric_gaming).
narrative_ontology:affects_constraint(goodhart_law_metric_erosion, bureaucratic_theater).

% DUAL FORMULATION NOTE:
% Goodhart's law decomposes into two distinct constraints: (1) information_standard_metric_coordination (ε≈0.25, Rope) — the genuine problem of creating comparable measures, which is necessary and solvable; (2) metric_erosion_extraction (ε≈0.58, Tangled Rope) — the adaptive response of subordinates to optimization pressure, which erodes validity. The stories should be separated to avoid conflating the coordination problem with the extraction mechanism. This story focuses on the erosion cycle; the coordination problem is upstream.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(goodhart_law_metric_erosion, institutional, 0.18).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
