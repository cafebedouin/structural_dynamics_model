% ============================================================================
% CONSTRAINT STORY: incentive_surface_warping
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_incentive_surface_warping, []).

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
 *   constraint_id: incentive_surface_warping
 *   human_readable: The Perverse Optimization Trap (Goodhart's Law)
 *   domain: economic/organizational/technological
 *
 * SUMMARY:
 *   Goodhart's Law—'when a measure becomes a target, it ceases to be a good
 *   measure'—describes a perverse optimization trap that emerges at the
 *   intersection of incentive alignment and incomplete information.
 *   Organizations implement metrics to coordinate effort (performance
 *   bonuses, student test scores, hospital readmission rates, algorithmic
 *   recommendation engagement) with the intent of aligning individual
 *   behavior toward system goals. But when individuals are measured against a
 *   single metric and rewarded/punished accordingly, the metric surface
 *   warps: actors optimize for the measure rather than the underlying
 *   objective. Teachers teach to the test; surgeons avoid complex cases to
 *   maintain low complication rates; algorithms maximize click-through
 *   without regard to user satisfaction. The constraint is not the metric
 *   itself but the perverse incentive structure it creates when enforced
 *   without parallel quality controls. This story models Goodhart's Law as a
 *   Tangled Rope: it begins as pure coordination (metrics do align effort)
 *   but becomes mixed extraction (metric gaming extracts value from system
 *   integrity) as the incentive warp develops. The theater_ratio rises from
 *   0.35 to 0.68 as compliance and audit functions become performative rather
 *   than preventive. The extractiveness rises from 0.28 to 0.52 as
 *   organizations accept metric gaming as a cost of doing business rather
 *   than a sign of constraint failure.
 *
 * KEY AGENTS:
 *   - Metric Designers / Executive Leadership: Primary beneficiary (institutional/arbitrage) — capture accountability benefits and short-term performance improvements; can exit to other metrics or organizations if damage becomes visible
 *   - Optimization Targets: Primary victim (powerless/trapped) — teachers, surgeons, workers must optimize for the metric or face career consequences; cannot exit without losing livelihood
 *   - System Integrity: Secondary victim (abstract/trapped) — quality, safety, user satisfaction, or true system objectives degrade as optimization warps the metric surface; no advocate, no exit
 *   - Mid-Level Managers: Secondary victim (moderate/constrained) — caught between pressure to meet metrics and awareness that gaming is occurring; constrained by both metric targets and organizational politics
 *   - Audit and Compliance Functions: Institutional actor (institutional/constrained) — nominally detect gaming but often function performatively; cannot exit easily but face pressure to certify gaming as acceptable
 *   - System Integrity Coalition: Organized observers (organized/constrained) — economists, organizational theorists, and reform advocates see the problem clearly but constrained by institutional resistance to metric change
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(incentive_surface_warping, 0.52).
domain_priors:suppression_score(incentive_surface_warping, 0.58).
domain_priors:theater_ratio(incentive_surface_warping, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(incentive_surface_warping, extractiveness, 0.52).
narrative_ontology:constraint_metric(incentive_surface_warping, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(incentive_surface_warping, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(incentive_surface_warping, tangled_rope).
narrative_ontology:human_readable(incentive_surface_warping, "The Perverse Optimization Trap (Goodhart's Law)").
narrative_ontology:topic_domain(incentive_surface_warping, "economic/organizational/technological").

domain_priors:requires_active_enforcement(incentive_surface_warping).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(incentive_surface_warping, metric_designers).
narrative_ontology:constraint_beneficiary(incentive_surface_warping, short_term_extractors).
narrative_ontology:constraint_victim(incentive_surface_warping, system_integrity).
narrative_ontology:constraint_victim(incentive_surface_warping, optimization_targets).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: OPTIMIZATION TARGET (SNARE) — Teachers, surgeons, customer service agents, or factory workers measured against a single metric face a snare: gaming the metric becomes rational, but the metric designer holds all power to adjust rules. Trapped by employment/livelihood dependency, no exit path. d≈0.95, f(d)≈1.42, σ=1.0 → χ≈0.74.
constraint_indexing:constraint_classification(incentive_surface_warping, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: MID-LEVEL MANAGER (TANGLED ROPE) — Constrained by both performance metrics and awareness that optimization is warping the system. Benefits from metric-based rewards if they hit targets, but also bears cost of detecting gaming and explaining it upward. Sees both coordination (metric alignment) and extraction (perverse incentive). d≈0.68, f(d)≈1.02, σ=1.0 → χ≈0.53.
constraint_indexing:constraint_classification(incentive_surface_warping, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: METRIC DESIGNER / EXECUTIVE LEADERSHIP (ROPE) — Institutional actors with arbitrage (ability to reformulate metrics, exit to other organizations/industries). Experience the constraint as pure coordination: metrics are tools to align effort with goals. Can adjust metrics if they detect gaming, but often don't until damage is severe. d≈0.08, f(d)≈-0.11, σ=1.2 → χ≈-0.08. Negative effective extraction = net beneficiary.
constraint_indexing:constraint_classification(incentive_surface_warping, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: AUDIT AND COMPLIANCE FUNCTION (PITON) — Audit and compliance teams nominally detect metric gaming but often function performatively: they certify processes without deep verification, and their own KPIs (audits completed, certifications issued) can become gamed. Theater ratio=0.68 reflects the ritual of control without functional prevention. d≈0.50, f(d)≈0.65, σ=1.0 → χ≈0.35.
constraint_indexing:constraint_classification(incentive_surface_warping, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: SYSTEM INTEGRITY COALITION (ORGANIZED ACTORS) — Economists, organizational theorists, and reform coalitions see the constraint as both coordinating (metrics enable accountability) and extractive (narrow metrics incentivize gaming). Constrained by institutional resistance to metric reform but increasingly have exit options (publish research, move to advisory roles, build alternative measurement frameworks). d≈0.45, f(d)≈0.50, σ=1.2 → χ≈0.31.
constraint_indexing:constraint_classification(incentive_surface_warping, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational/universal perspective, Goodhart's Law appears as an immutable law of incentive design: whenever a metric becomes a target, it ceases to be a good metric. This is a mathematical consequence of optimization under incomplete information. However, the base metrics (ε=0.52, suppression=0.58, theater=0.68) suggest this is NOT a true mountain — the 'law' is contingent on organizational choices, not fundamental physics.
constraint_indexing:constraint_classification(incentive_surface_warping, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(incentive_surface_warping_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(incentive_surface_warping, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(incentive_surface_warping, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(incentive_surface_warping, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(incentive_surface_warping, TR),
    TR >= 0.70.

:- end_tests(incentive_surface_warping_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The constraint extracts value from system integrity and true performance, redirecting effort toward metric optimization at the expense of underlying goals. But extraction is not as severe as a pure Snare (0.66+) because the metric still produces some legitimate coordination — teachers do improve student outcomes on tested material, surgeons do become more careful about documented complications, workers do increase effort. The moderate extractiveness reflects this mixed benefit-extraction nature. Suppression (0.58): Moderate-high. Significant barriers prevent optimization targets from exiting or resisting: employment dependency, career risk, lack of alternative employment pathways, and organizational culture that normalizes metric compliance. However, suppression is incomplete — high-skill workers (surgeons, elite teachers) have some exit options (private practice, independent schools), and public pressure can force metric reform. Theater ratio (0.68): High. Audit functions, compliance certifications, and metric reviews become increasingly performative over time. Auditors certify gaming as 'acceptable variance.' Compliance teams measure whether audit processes were completed rather than whether gaming was prevented. The metric about the metric (audit completion) itself becomes gamed. The claimed type (tangled_rope) reflects this: genuine coordination (metrics do align some effort) mixed with asymmetric extraction (metric designers benefit while targets bear costs).
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits a stark perspectival gap between metric designers and optimization targets. Designers see Rope: metrics are coordination tools that work. Targets see Snare: they are trapped optimizing for a measure that extracts their effort without improving actual performance. Managers see Tangled Rope: mixed coordination and extraction. Audit functions see Piton: their control rituals persist despite being unable to prevent gaming. The system integrity coalition sees Tangled Rope with a potential Scaffold path forward: multi-metric systems, participatory metric design, and transparency about gaming could enable a sunset. The analytical observer risks seeing Mountain: Goodhart's Law as immutable. But the measurement trajectory (extractiveness rising from 0.28 to 0.52, theater rising from 0.35 to 0.68) shows this is contingent, not inevitable — the system is learning to accept and work around the perverse incentive rather than solving it.
 *
 * DIRECTIONALITY LOGIC:
 *   Metric Designers: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.11. Can reformulate metrics or leave if damage becomes severe. Optimization Targets: Victim + trapped → d≈0.95, f(d)≈1.42. Livelihood dependent, no exit. System Integrity: Victim + trapped (abstract) → d≈0.95, f(d)≈1.42. No advocate, cannot exit. Mid-Level Managers: Victim + constrained → d≈0.68, f(d)≈1.02. Constrained by both metric pressure and organizational politics. Audit Functions: Mixed (beneficiary from metric system design + constrained by institutional resistance) → d≈0.50, f(d)≈0.65. Can't exit but also benefit from the metrics they nominally audit. System Integrity Coalition: Organized + constrained → d≈0.45, f(d)≈0.50. Increasingly have exit options (research, advising, alternative frameworks) but constrained by inertia.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION: The constraint resolves the Rope-vs-Snare ambiguity by showing that Goodhart's Law generates a genuine Tangled Rope, not a false dichotomy between 'pure coordination' and 'pure extraction.' The constraint BEGINS as Rope (metrics do coordinate effort effectively for 0-3 years). As optimization warps the metric surface and gaming becomes systemic, the constraint evolves toward Snare (extraction overtakes coordination). The Tangled Rope classification captures this intermediate state: the coordination function persists (metrics still produce some alignment) but extraction is now dominant (gaming costs exceed coordination benefits). The theater_ratio rising to 0.68 indicates that compliance and audit functions are becoming performative — a sign of degradation toward Piton. The measurement trajectory (extractiveness 0.28→0.52) shows the constraint is not a false positive: it is a real structural evolution from coordination to extraction. The mandatrophy is resolved by acknowledging that the constraint exhibits both properties genuinely, and their ratio determines the classification at each time point.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    metric_goodness_definition,
    'Is Goodhart''s Law a universal principle of incentive design or an artifact of poorly constructed metrics?',
    'Empirical: Identify metrics that consistently resist gaming across decades and organizations (e.g., mortality rates in hospitals, customer lifetime value in commerce). Theoretical: Develop principles for metrics robust to optimization.',
    'If universal: constraint is Mountain (ε→0.15). If contingent: constraint is Tangled Rope (ε remains 0.52, but beneficiaries/victims change).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(metric_goodness_definition, conceptual, 'Whether Goodhart''s Law is universal or contingent on metric design quality').

omega_variable(
    gaming_detection_lag,
    'What is the typical time lag between metric implementation and detection of systemic gaming?',
    'Longitudinal study of metric adoption in organizations (healthcare, education, finance); measure time from deployment to first documented gaming episode; correlate with metric complexity and organizational transparency.',
    'If lag < 6 months: metric designers learn quickly, tangled rope classification holds. If lag > 3 years: gaming becomes institutionalized before detection, constraint drifts toward Snare.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(gaming_detection_lag, empirical, 'Time lag between metric implementation and detection of gaming').

omega_variable(
    multi_metric_robustness,
    'Does combining multiple metrics significantly reduce gaming pressure, or does optimization simply distribute across the metric portfolio?',
    'Compare organizations using single vs multi-metric systems; measure gaming incidents per metric, total system distortion, and capability to achieve underlying goals.',
    'If multi-metric is robust: supplants perverse optimization (χ→0.25). If optimization distributes: theater increases but underlying extraction persists (theater→0.85, χ unchanged).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(multi_metric_robustness, empirical, 'Whether multi-metric systems reduce or redistribute gaming pressure').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(incentive_surface_warping, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(isw_tr_t0, incentive_surface_warping, theater_ratio, 0, 0.35).
narrative_ontology:measurement(isw_tr_t5, incentive_surface_warping, theater_ratio, 5, 0.52).
narrative_ontology:measurement(isw_tr_t10, incentive_surface_warping, theater_ratio, 10, 0.68).

% Extraction over time
narrative_ontology:measurement(isw_be_t0, incentive_surface_warping, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(isw_be_t5, incentive_surface_warping, base_extractiveness, 5, 0.4).
narrative_ontology:measurement(isw_be_t10, incentive_surface_warping, base_extractiveness, 10, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(incentive_surface_warping, resource_allocation).
narrative_ontology:affects_constraint(incentive_surface_warping, metric_gaming_cascades).
narrative_ontology:affects_constraint(incentive_surface_warping, organizational_goal_displacement).

% DUAL FORMULATION NOTE:
% The perverse optimization trap decomposes into two structurally distinct constraints: (1) the metric-target warping effect (incentive_surface_warping, ε=0.52, Tangled Rope) and (2) the organizational-level goal displacement that follows (organizational_goal_displacement, ε≈0.65, Snare). The warping constraint creates the conditions for goal displacement; goal displacement then reinforces the warping through institutional inertia. These are upstream and downstream of a causal chain, linked via network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(incentive_surface_warping, institutional, 0.12).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
