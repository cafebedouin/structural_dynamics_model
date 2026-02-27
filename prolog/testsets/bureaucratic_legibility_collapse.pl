% ============================================================================
% CONSTRAINT STORY: bureaucratic_legibility_collapse
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_bureaucratic_legibility_collapse, []).

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
 *   constraint_id: bureaucratic_legibility_collapse
 *   human_readable: The Administrative Whiteout: Legibility Collapse in Bureaucratic Systems
 *   domain: political/organizational/informational
 *
 * SUMMARY:
 *   Bureaucratic legibility collapse occurs when an institution's measurement
 *   and control systems become so decoupled from ground reality that the
 *   institution's corrective actions produce perverse outcomes —
 *   counterintuitively worsening the conditions they claim to address. This
 *   constraint represents a hybrid coordination-extraction mechanism: the
 *   measurement bureaucracy genuinely solves the coordination problem of
 *   aggregating information across large, decentralized organizations
 *   (coordination function) while simultaneously creating systematic
 *   incentives for frontline staff to optimize metrics rather than intended
 *   outcomes (extraction function). The constraint exhibits signatures across
 *   all six classification types, depending on observational position.
 *   Street-level workers experience it as a snare: trapped between
 *   contradictory directives (satisfy metrics vs serve actual needs), they
 *   game the system and experience extraction through ethical compromise.
 *   Intended beneficiaries experience it as a snare: service quality degrades
 *   as staff redirect effort to metric optimization, producing measured
 *   improvement alongside actual deterioration. The measurement bureaucracy
 *   experiences it as tangled rope: they coordinate information aggregation
 *   while extracting legitimacy from metrics they know are perverse. Senior
 *   administration experiences it as rope: they see pure coordination without
 *   experiencing frontline perversity. The analytical observer risks seeing
 *   this as an immutable natural law (Goodhart's Law: any metric good enough
 *   to target becomes gamed). However, the extractiveness value (0.58) and
 *   theater ratio (0.81) reveal this as a contingent institutional
 *   arrangement, not a mathematical inevitability. The constraint has
 *   intensified over the interval (theater rising from 0.52 to 0.81,
 *   extractiveness from 0.32 to 0.58) as audit regimes have proliferated and
 *   measurement proliferation has outpaced frontline capacity to
 *   simultaneously satisfy all targets.
 *
 * KEY AGENTS:
 *   - Street-Level Workers (Teachers, Nurses, Social Workers): Primary victims (powerless/trapped) — compressed between metric targets and actual operational needs; no exit without losing livelihood
 *   - Intended Beneficiaries (Citizens, Patients, Students): Primary victims (powerless/trapped) — experience degraded service quality as providers redirect effort to metrics; compulsory system participation
 *   - Measurement Bureaucracy (Auditors, Inspectors, Metric Designers): Primary beneficiaries (organized/constrained) — gain career legitimacy and institutional indispensability from metrics; constrained exit because admitting perversity would undermine authority
 *   - Senior Administration (Institutional Leadership): Secondary beneficiary (institutional/arbitrage) — benefits from aggregated metrics enabling central control; arbitrage exit allows dismissal of problems as implementation failure
 *   - Reform Coalition (Metric Modernizers, Evidence Advocates): Mixed (moderate/constrained) — advance genuine improvements while reinforcing measurement paradigm; constrained by political feasibility of alternatives
 *   - System Accountability (Abstract Public Good): Victim (powerless/trapped) — measurement theater masks failures; no mechanism to signal that metrics have inverted from accountability tools to extraction theater
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(bureaucratic_legibility_collapse, 0.58).
domain_priors:suppression_score(bureaucratic_legibility_collapse, 0.62).
domain_priors:theater_ratio(bureaucratic_legibility_collapse, 0.81).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(bureaucratic_legibility_collapse, extractiveness, 0.58).
narrative_ontology:constraint_metric(bureaucratic_legibility_collapse, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(bureaucratic_legibility_collapse, theater_ratio, 0.81).

% --- Constraint claim ---
narrative_ontology:constraint_claim(bureaucratic_legibility_collapse, tangled_rope).
narrative_ontology:human_readable(bureaucratic_legibility_collapse, "The Administrative Whiteout: Legibility Collapse in Bureaucratic Systems").
narrative_ontology:topic_domain(bureaucratic_legibility_collapse, "political/organizational/informational").

domain_priors:requires_active_enforcement(bureaucratic_legibility_collapse).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(bureaucratic_legibility_collapse, administrative_apparatus).
narrative_ontology:constraint_beneficiary(bureaucratic_legibility_collapse, metric_designers).
narrative_ontology:constraint_victim(bureaucratic_legibility_collapse, field_practitioners).
narrative_ontology:constraint_victim(bureaucratic_legibility_collapse, intended_beneficiaries).
narrative_ontology:constraint_victim(bureaucratic_legibility_collapse, system_accountability).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: STREET-LEVEL WORKER (SNARE) — Teachers, social workers, nurses, frontline staff are trapped between contradictory directives: gaming metrics to satisfy auditors while actual work demands deviate from measured performance. No exit option without losing livelihood. Bears extraction through emotional labor, ethical compromise, and career suppression. Maximum experienced extractiveness.
constraint_indexing:constraint_classification(bureaucratic_legibility_collapse, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: INTENDED BENEFICIARIES (SNARE) — Citizens, patients, students who the system claims to serve experience degraded service quality as frontline staff redirect effort to metric optimization. Cannot exit the system (compulsory education, healthcare dependency, civic participation). Bear extraction in form of worse-than-intended outcomes masked by statistical improvement.
constraint_indexing:constraint_classification(bureaucratic_legibility_collapse, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 3: MEASUREMENT BUREAUCRACY (TANGLED ROPE) — Auditors, inspectors, metric designers benefit from the legitimacy their measurements confer (career advancement, institutional indispensability, budget justification). Also genuinely coordinate by creating common performance language. But extraction mechanism is hidden: perverse incentives harm the system they purport to improve. Constrained exit — auditors cannot admit metrics are counterproductive without dismantling their own authority.
constraint_indexing:constraint_classification(bureaucratic_legibility_collapse, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: ACCOUNTABILITY THEATER (PITON) — Modern governance rituals of transparency and measurement have largely become performative. The metrics persist because removing them appears to abandon accountability (reputational cost), not because they produce accountability. Theater ratio is elevated: audit processes, performance reviews, compliance documentation continue despite widespread recognition of perversity. Institutional inertia maintains the ritual.
constraint_indexing:constraint_classification(bureaucratic_legibility_collapse, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: SENIOR ADMINISTRATION (ROPE) — Benefits from metrics as coordination solution: aggregated data enables central control, budget allocation appears rational, performance standards create common language across decentralized units. Extraction mechanism is invisible to this perspective — they see coordination benefits without experiencing frontline perversity. Arbitrage exit: can dismiss problems as poor implementation rather than system design.
constraint_indexing:constraint_classification(bureaucratic_legibility_collapse, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: REFORM COALITION (TANGLED ROPE) — Advocates for evidence-based reform, system improvement, metric modernization benefit from legitimacy of 'better measurement' framing. Also genuinely attempt to reduce perverse incentives. But constrained by political feasibility: cannot fully escape measurement discourse without losing credibility. Mixed benefit/extraction: they advance some improvements while reinforcing the measurement paradigm itself.
constraint_indexing:constraint_classification(bureaucratic_legibility_collapse, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / GOODHART'S LAW (MOUNTAIN) — From a universal/civilizational perspective, some decoupling between measure and reality is inherent to measurement itself: any metric good enough to use as a target becomes subject to manipulation. This perspective sees the bottleneck as an immutable mathematical truth — a constraint on governance systems. However, the structural data reveals this as potential false summit: the severity of decoupling (theater ratio 0.81, extraction 0.58) is contingent on institutional design choices, not inevitable.
constraint_indexing:constraint_classification(bureaucratic_legibility_collapse, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(bureaucratic_legibility_collapse_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(bureaucratic_legibility_collapse, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(bureaucratic_legibility_collapse, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(bureaucratic_legibility_collapse, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(bureaucratic_legibility_collapse, TR),
    TR >= 0.70.

:- end_tests(bureaucratic_legibility_collapse_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The measurement bureaucracy extracts institutional legitimacy, career advancement, and autonomy from their role as arbiter of organizational performance. Street-level workers extract negative value through enforced metric gaming. The extraction is not total because some coordination benefits persist (aggregated data does enable some rational allocation) and some metric improvements do reflect genuine work. The trajectory from 0.32 to 0.58 reflects metric proliferation outpacing capacity — as audit regimes expand, gaming becomes more necessary and perversity more severe. Suppression (0.62): Moderate-high. Multiple barriers prevent recognition and reform: (1) metric designers face career incentive to defend existing systems; (2) senior administration benefits from metrics and lacks ground-level visibility into perversity; (3) public accountability logic makes admitting metric failure politically costly; (4) reform advocates are captured by measurement paradigm language (can only propose 'better metrics,' not non-metric governance). Theater ratio (0.81): High. Contemporary governance audit processes have become substantially performative. Performance reviews, compliance documentation, audit trails, and measurement rituals continue despite widespread informal recognition that metrics have inverted from accountability tools to accountability theater. The theater persists because removing it appears to abandon accountability itself.
 *
 * PERSPECTIVAL GAP:
 *   This constraint reveals maximum perspectival divergence because the same institutional measurement system appears beneficent to senior administration and extractive to street-level workers. Senior administration (Rope perspective) sees pure coordination: metrics enable aggregation, control, and rational resource allocation — this is their genuine experience because they do not see perverse incentives. Street-level workers (Snare perspective) see pure extraction: they are forced to optimize metrics at the cost of actual work quality — this is their genuine experience because they live the contradictions. The measurement bureaucracy (Tangled Rope) genuinely coordinates while extracting legitimacy. Intended beneficiaries (Snare) experience the outcome inversion: measured improvement masks actual quality decline. The reform coalition (Tangled Rope) attempts to reduce perversity while reinforcing the measurement paradigm. The analytical observer (Mountain risk) risks naturalizing this as inevitable law-of-nature rather than contingent design choice. The perspectival gap exists because the constraint's extraction mechanism is invisible from the beneficiary's height of hierarchy and maximally visible from the victim's frontline position.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality derivation: (1) Street-level workers: powerless + trapped + victim → high d → high f(d) → high χ. They experience maximum extraction because they have no exit and no benefit from the system. (2) Intended beneficiaries: powerless + trapped + victim → high d. They bear costs (degraded service) without benefits. (3) Measurement bureaucracy: organized + constrained + beneficiary → moderate d. They benefit from metric legitimacy (low d from beneficiary status) but face constrained exit (can't admit perversity without losing authority). Derivation produces moderate d ≈ 0.40, intermediate f(d). (4) Senior administration: institutional + arbitrage + beneficiary → very low d (≈0.15). They experience benefits without experiencing victim costs; arbitrage exit allows them to reframe problems. (5) Reform coalition: moderate + constrained + mixed → d ≈ 0.50. They advance some improvements while reinforcing measurement language. (6) Analytical observer: analytical + analytical → d ≈ 0.72. They see the full structure but risk naturalizing it as inevitable.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLVED: This constraint's classification as Tangled Rope (not Snare, not Rope) is justified by both coordination and extraction mechanisms being structurally necessary. The measurement bureaucracy simultaneously (1) solves a genuine coordination problem (enabling aggregation and communication across decentralized units) AND (2) creates systematic extraction incentives (gaming by frontline staff, outcome inversion, ethical compromise). The classification would fail the mandatrophy if the coordination function were merely performative or if it could be eliminated without degrading administration. But evidence suggests: (1) removing metrics entirely would degrade coordination across large bureaucracies; (2) the coordination and extraction mechanisms are structurally coupled through the same institutional apparatus; (3) reforming metrics without recognizing the extraction function leads to metric proliferation and intensified gaming (the measured trajectory from 0.32 to 0.58). The Tangled Rope classification stands because no subset of the structure reduces to pure coordination or pure extraction. Any resolution requires simultaneously acknowledging coordination benefits and extraction costs — the constraint cannot be solved by privileging either logic over the other.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    measure_reality_decoupling_threshold,
    'At what degree of metric-reality decoupling does a measurement system transition from ''imperfect coordination'' to ''pure extraction theater''?',
    'Comparative analysis across jurisdictions with different audit intensities; correlation between metric compliance and actual outcome improvement; longitudinal tracking of metric predictive validity',
    'If threshold is low (decoupling occurs quickly): measurement systems are inherently destabilizing. If threshold is high (systems maintain validity under pressure): measurement is viable coordination tool and extraction mechanism is policy choice, not invariant.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(measure_reality_decoupling_threshold, empirical, 'Threshold at which metric-reality decoupling becomes severe').

omega_variable(
    frontline_metric_gaming_necessity,
    'Is metric gaming by frontline workers a rational response to contradictory demands, or a sign of moral failure in implementation?',
    'Ethnographic analysis of metric-setting decisions vs frontline operational constraints; comparison of gaming rates under different incentive structures; identification of whether gaming is mathematically necessary to satisfy conflicting targets',
    'If necessary: system design is the extraction mechanism. If optional: workers bear moral responsibility and extraction claim weakens. Determines whether victims classification is justified.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(frontline_metric_gaming_necessity, empirical, 'Whether metric gaming is forced by system design or chosen by workers').

omega_variable(
    alternative_coordination_feasibility,
    'Can complex multi-unit bureaucracies function without quantified performance metrics, or are metrics a necessary condition for governance above a certain scale?',
    'Historical comparison of pre-metric bureaucratic coordination mechanisms; study of organizations that have eliminated metrics; analysis of whether coordination can be maintained through narrative, reputation, or distributed decision-making',
    'If feasible: measurement systems are not invariant (Mountain classification fails). Alternative coordination is possible and extraction mechanism is policy choice. If infeasible: measurement is structural necessity and perversity is unavoidable consequence — shifts classification toward Mountain.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(alternative_coordination_feasibility, conceptual, 'Whether bureaucratic coordination without metrics is feasible').

omega_variable(
    metric_designer_awareness,
    'Do metric designers understand the perverse incentives their systems create, or are they genuinely surprised by real-world gaming and outcome inversion?',
    'Analysis of metric design literature; interviews with designers about anticipated gaming; comparison of predicted vs actual perverse outcomes; institutional memory of past metric failures',
    'If designers knew: extraction classification is stronger (intentional). If genuinely surprised: extraction mechanism is organizational blindness rather than designed capture. Affects directionality of measurement bureaucracy perspective.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(metric_designer_awareness, empirical, 'Degree of metric designer awareness of perverse incentives').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(bureaucratic_legibility_collapse, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bleq_tr_t0, bureaucratic_legibility_collapse, theater_ratio, 0, 0.52).
narrative_ontology:measurement(bleq_tr_t10, bureaucratic_legibility_collapse, theater_ratio, 10, 0.68).
narrative_ontology:measurement(bleq_tr_t20, bureaucratic_legibility_collapse, theater_ratio, 20, 0.81).

% Extraction over time
narrative_ontology:measurement(bleq_be_t0, bureaucratic_legibility_collapse, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(bleq_be_t10, bureaucratic_legibility_collapse, base_extractiveness, 10, 0.45).
narrative_ontology:measurement(bleq_be_t20, bureaucratic_legibility_collapse, base_extractiveness, 20, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(bureaucratic_legibility_collapse, information_standard).
narrative_ontology:affects_constraint(bureaucratic_legibility_collapse, goodharts_law_metric_targeting).
narrative_ontology:affects_constraint(bureaucratic_legibility_collapse, organizational_accountability_capture).

% DUAL FORMULATION NOTE:
% The administrative whiteout is a family of related constraints. Goodhart's Law (upstream, ε≈0.08, Mountain) represents the mathematical principle that any metric good enough to target becomes gamed. Organizational accountability capture (downstream, ε≈0.65, Snare) represents the institutional outcome where measurement systems completely invert to serve auditor interests. The bureaucratic legibility collapse (this story, ε=0.58, Tangled Rope) represents the hybrid stage where coordination and extraction are still structurally coupled but decomposable through institutional redesign. All three share the measurement theme but have different extractiveness values reflecting different observational foci.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(bureaucratic_legibility_collapse, organized, 0.42).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
