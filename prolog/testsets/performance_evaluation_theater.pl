% ============================================================================
% CONSTRAINT STORY: performance_evaluation_theater
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_performance_evaluation_theater, []).

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
 *   constraint_id: performance_evaluation_theater
 *   human_readable: Performance Evaluation Theater in Organizations
 *   domain: organizational/labor
 *
 * SUMMARY:
 *   Performance evaluation theater in organizations represents a structural
 *   constraint where legitimated surveillance and arbitrary judgment
 *   mechanisms extract compliance, psychological labor, and conformity from
 *   workers while providing minimal coordination benefit. The constraint
 *   operates through a combination of structural suppression (employment
 *   dependence, legal mandate to participate, career consequences) and
 *   internalized suppression (workers internalize the validity of evaluation,
 *   accept ratings as legitimate judgment of their worth, and self-regulate
 *   according to perceived evaluation criteria). The theater_ratio has
 *   increased from 0.62 to 0.78 over the measurement interval, indicating
 *   that the formal evaluation infrastructure has become increasingly
 *   performative as actual measurement validity has declined — evaluation
 *   cycles elaborate ritual content while predictive validity for job
 *   performance remains low. The constraint exhibits all six classification
 *   types depending on observer position, with the primary tension between
 *   management's benign view of evaluation as coordination mechanism (Rope)
 *   and workers' experience of it as pure extraction (Snare). The increasing
 *   theater_ratio combined with stable or rising extractiveness indicates
 *   metric substitution (Goodhart drift): the system optimizes for evaluation
 *   ritual completion rather than worker performance improvement.
 *
 * KEY AGENTS:
 *   - Evaluated Workers: Primary victims (powerless/trapped) — bear full cost of performative system through time expenditure, psychological stress, and arbitrary judgment with career consequences
 *   - Management Hierarchy: Primary beneficiary (institutional/arbitrage) — extracts legitimacy for personnel decisions, documented basis for terminations and pay adjustments, and organizational control mechanism
 *   - Human Resources Department: Institutional gatekeeper (institutional/arbitrage) — maintains and elaborates performative infrastructure; benefits from continued relevance and job security; sees own system as degraded but continues it through inertia
 *   - Organizational Reform Movement: Organized reformers (organized/mobile) — building alternative systems (peer feedback, skill-based mobility, continuous feedback) with sunset logic; reducing extraction through system replacement rather than demand reduction
 *   - Collective Workforce: Secondary victim/moderate agent (moderate/constrained) — benefits from some coordination functions (career clarity, feedback mechanisms) while bearing extraction costs; constrained by collective action problems and industry standardization
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing contingent institutional arrangements as immutable organizational requirements
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(performance_evaluation_theater, 0.58).
domain_priors:suppression_score(performance_evaluation_theater, 0.65).
domain_priors:theater_ratio(performance_evaluation_theater, 0.78).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(performance_evaluation_theater, extractiveness, 0.58).
narrative_ontology:constraint_metric(performance_evaluation_theater, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(performance_evaluation_theater, theater_ratio, 0.78).

% --- Constraint claim ---
narrative_ontology:constraint_claim(performance_evaluation_theater, snare).
narrative_ontology:human_readable(performance_evaluation_theater, "Performance Evaluation Theater in Organizations").
narrative_ontology:topic_domain(performance_evaluation_theater, "organizational/labor").

domain_priors:requires_active_enforcement(performance_evaluation_theater).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(performance_evaluation_theater, management_hierarchy).
narrative_ontology:constraint_victim(performance_evaluation_theater, evaluated_workers).
narrative_ontology:constraint_victim(performance_evaluation_theater, organizational_productivity).
narrative_ontology:constraint_victim(performance_evaluation_theater, worker_wellbeing).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: EVALUATED WORKER (SNARE) — Trapped by employment dependence and legal obligation to participate in evaluations. Bears full cost of the performative system: time spent preparing narratives, psychological stress from arbitrary judgment, career consequences from ratings unrelated to actual performance. No realistic exit without job loss. Maximum extraction experienced through legitimated surveillance and judgment.
constraint_indexing:constraint_classification(performance_evaluation_theater, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: COLLECTIVE WORKFORCE (TANGLED ROPE) — Constrained by limited job mobility, industry-wide standardization of evaluation practices, and reputational damage from refusing participation. Benefits from some coordination functions (feedback mechanisms, career pathway clarity) alongside significant extraction. Moderate agency through unionization and collective action, but normative expectations constrain exit.
constraint_indexing:constraint_classification(performance_evaluation_theater, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: MANAGEMENT HIERARCHY (ROPE) — Benefits from evaluation theater as coordination mechanism: provides documented basis for pay decisions, terminations, and promotion selections. Experiences constraint as enabling institutional function. Can arbitrage to alternative systems (e.g., flat hierarchies) but benefits from existing structure outweigh costs. Net beneficiary with high agency.
constraint_indexing:constraint_classification(performance_evaluation_theater, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: HUMAN RESOURCES DEPARTMENT (PITON) — Maintains performative evaluation rituals (forms, metrics, calibration sessions) that persist through institutional inertia despite acknowledged failure to predict performance or improve outcomes. Theater ratio (0.78) reflects that evaluation infrastructure is largely ceremonial: calibration meetings confirm existing biases, metrics measure effort not impact, ratings correlate more with likeability than actual contribution. HR sees the system as degraded but continues it because no replacement has gained institutional acceptance.
constraint_indexing:constraint_classification(performance_evaluation_theater, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: ORGANIZATIONAL REFORM MOVEMENT (SCAFFOLD) — Organized actors (management consultants, tech companies pioneering alternative systems, worker advocacy groups) are building temporary scaffolding around evaluation theater: peer feedback systems, skill-based mobility, manager training in reducing bias. These are explicit interventions with sunset logic — once internalized as norms, evaluation theater's extraction mechanism loses force. Low effective extraction because these agents see an exit path with defined timeline.
constraint_indexing:constraint_classification(performance_evaluation_theater, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(regional))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a universal perspective, some performance evaluation is inherent to any hierarchical organization: distributed information problem requires some mechanism for comparing and selecting agents. This perspective risks naturalizing contingent institutional arrangements (standardized metrics, documented ratings, annual cycles) as immutable features of organizational life. Engine false summit detection applies.
constraint_indexing:constraint_classification(performance_evaluation_theater, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(performance_evaluation_theater_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(performance_evaluation_theater, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(performance_evaluation_theater, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(performance_evaluation_theater, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(performance_evaluation_theater, TR),
    TR >= 0.70.

:- end_tests(performance_evaluation_theater_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The evaluation system extracts compliance (mandatory participation), psychological labor (stress and identity work around ratings), conformity (self-regulation according to perceived evaluation criteria), and material consequences (pay adjustments, termination justifications, mobility blocking). The value reflects that extraction is substantial but not absolute — some workers successfully navigate evaluations, and the system provides some legitimate feedback. The increase from 0.48 to 0.58 reflects metric substitution: as objective performance measurement has become harder (organizational complexity increased, individual contribution more distributed), the system has elaborated performative content to maintain legitimacy while actual measurement validity has declined. Suppression (0.65): Moderate-high. External suppression includes employment dependence, legal mandate to participate, career consequences of non-compliance, and lack of alternatives within the organization. Internalized suppression includes workers' acceptance of evaluation as legitimate judgment of their worth, internalized need for managerial approval, and identity fusion with performance ratings. The combined suppression creates high barriers to exit: workers cannot leave the organization without material cost, and even if they could, many have internalized the constraint as a valid organizational practice. Theater ratio (0.78): High and rising. Evaluation infrastructure has become increasingly elaborate and performative: calibration sessions that formalize existing biases, metrics that measure effort and conformity rather than impact, annual cycles that create artificial urgency, and documentation that serves legal defense rather than performance improvement. The rise from 0.62 indicates that as measurement validity has declined, ritual elaboration has increased — the system maintains legitimacy through performative content when actual measurement fails.
 *
 * PERSPECTIVAL GAP:
 *   The primary perspectival gap is between management's experience of evaluation as coordinate mechanism (Rope) and workers' experience of it as pure extraction (Snare). This gap reveals the structural dependence of perspectival classification on exit options and directionality. Management has arbitrage options (can implement alternative evaluation systems, can exit to organizations with different practices) and benefits from the system, producing low directionality (d ≈ 0.10) and low experienced extractiveness. Workers have trapped or constrained exit options (cannot exit without job loss or career damage) and bear costs, producing high directionality (d ≈ 0.90) and high experienced extractiveness. The perspectival gap is not mere disagreement but structural: the two parties occupy fundamentally different positions relative to the constraint. The gap also reveals how internalized suppression sustains the asymmetry: workers who accept evaluation as legitimate (internalized suppression) are less likely to organize collective action to exit, despite having numerical majority. Management benefits from this internalization because it reduces the likelihood that workers will demand constraint removal.
 *
 * DIRECTIONALITY LOGIC:
 *   Worker directionality is high (d ≈ 0.90): trapped agents bearing extraction costs with no exit option. Management directionality is low (d ≈ 0.10): beneficiaries with arbitrage options who can shift evaluation systems if needed. The sigmoid f(d) maps these to strongly asymmetric experienced extractiveness: workers experience chi ≈ 0.85 (high effective extraction), management experiences chi ≈ -0.05 (negative effective extraction — they benefit from the constraint). The collective workforce has intermediate directionality (d ≈ 0.60) reflecting their constrained but not trapped position: they cannot exit without career cost, but collective action provides some leverage. HR's directionality is low (d ≈ 0.15) — they benefit from continued use of the system even though they see it as degraded. This directionality structure explains why the constraint persists despite being widely acknowledged as ineffective: the beneficiaries (management, HR) have low d and experience low or negative chi, while the victims have high d and experience high chi. The system is stable because those who could change it experience it as beneficial or at worst neutral.
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLVED THROUGH PERSPECTIVAL SPECIFICATION. The mandatrophy question is 'Is evaluation theater coordination or extraction?' The answer is 'both, for different parties.' Management sees and experiences it as coordination: the constraint solves their information problem (comparing worker contributions, documenting promotion decisions, providing legal defense for terminations). This is genuine coordination function — without some evaluation mechanism, hierarchical organizations cannot make personnel decisions. Workers see and experience it as extraction: the constraint imposes arbitrary judgment, psychological stress, and conformity demands with minimal coordination benefit to them (feedback quality is poor, decisions are often predetermined, career paths are opaque). This is genuine extraction — workers bear costs without corresponding benefit. The classification solution is to specify the perspective: from management's institutional position with arbitrage options, it is Rope (coordination). From workers' powerless trapped position, it is Snare (pure extraction). From the HR perspective, it is Piton (performative ritual maintained through inertia). From the reform movement's organized position with sunset logic, it is Scaffold (temporary coordination with declining extraction). The mandatrophy resolves by recognizing that neither management nor workers has a privileged view of 'what evaluation theater really is' — the constraint genuinely exhibits different structural properties depending on the observer's position within it. The analytical perspective that naturalizes evaluation as an immutable organizational law (Mountain) fails the false summit test: alternatives exist (peer-based evaluation, skill-based mobility, outcome-only assessment), and the constraint persists through institutional choice rather than natural necessity.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    measurement_validity_ambiguity,
    'Do performance metrics measure actual worker contribution or primarily reflect manager perception, likeability, and conformity?',
    'Longitudinal correlation analysis: ratings vs. actual productivity metrics (output quality, customer satisfaction, innovation measures); comparison of within-manager consistency (same performance rated differently across employees) vs. actual performance differences',
    'If metrics have >0.5 correlation with objective performance: evaluation provides real information (reduces to Tangled Rope from worker perspective). If <0.3 correlation: ratings are pure status signal (confirms Snare classification).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(measurement_validity_ambiguity, empirical, 'Whether performance metrics measure actual contribution or managerial perception').

omega_variable(
    alternative_system_viability,
    'Can non-hierarchical evaluation systems (peer feedback, skill-based mobility, outcome-only assessment) actually replace annual ratings at scale?',
    'Case studies of organizations that eliminated traditional ratings; measurement of worker satisfaction, retention, and productivity under alternative systems; identification of organizational conditions under which alternatives fail',
    'If viable: scaffold perspective is structural (sunset is real, extraction will decline). If not viable: organizations will revert to traditional theater (extraction is immutable, snare classification confirmed).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_system_viability, empirical, 'Whether viable alternatives to traditional evaluation exist at scale').

omega_variable(
    suppression_internalization,
    'Is measured suppression (0.65) primarily external (legal/career barriers to exit) or internalized (workers accept evaluation as legitimate and necessary)?',
    'Post-departure worker interviews; measurement of psychological reactance in organizations; correlation of internalization levels with pre-existing identification with organizational values',
    'If primarily internalized: constraint''s effective suppression persists beyond structural removal (workers will self-regulate even without formal evaluation); workers carry internalized suppression to new organizations. If primarily external: eliminating formal evaluation will substantially reduce suppression.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_internalization, empirical, 'Whether suppression is structural or internalized in worker psychology').

omega_variable(
    management_genuine_benefit,
    'Does evaluation theater actually provide management with decision-quality information, or is it entirely performative from their perspective too?',
    'Analysis of manager decision-making in organizations with/without formal ratings; examination of post-evaluation personnel decisions to determine whether ratings influenced outcomes or were post-hoc justifications; manager interview data on rating confidence and utility',
    'If genuinely useful to management: constrains reclassification as Tangled Rope (both parties experience coordination alongside extraction). If entirely performative: confirms Snare (management extracts legitimacy from theater without actual benefit).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(management_genuine_benefit, empirical, 'Whether formal ratings provide management with genuine decision-quality information').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(performance_evaluation_theater, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(perf_tr_t0, performance_evaluation_theater, theater_ratio, 0, 0.62).
narrative_ontology:measurement(perf_tr_t3, performance_evaluation_theater, theater_ratio, 3, 0.7).
narrative_ontology:measurement(perf_tr_t6, performance_evaluation_theater, theater_ratio, 6, 0.78).

% Extraction over time
narrative_ontology:measurement(perf_be_t0, performance_evaluation_theater, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(perf_be_t3, performance_evaluation_theater, base_extractiveness, 3, 0.53).
narrative_ontology:measurement(perf_be_t6, performance_evaluation_theater, base_extractiveness, 6, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(performance_evaluation_theater, resource_allocation).
narrative_ontology:affects_constraint(performance_evaluation_theater, workplace_surveillance).
narrative_ontology:affects_constraint(performance_evaluation_theater, management_legitimacy).
narrative_ontology:affects_constraint(performance_evaluation_theater, worker_identity_commitment).

% DUAL FORMULATION NOTE:
% Performance evaluation theater is downstream of the resource allocation problem (who gets resources, advancement, and rewards in hierarchical organizations) and upstream of worker identity dynamics (workers internalize evaluation as legitimate judgment of their worth). The three-constraint family maps the causal chain: resource allocation creates need for evaluation mechanisms (parent) → evaluation theater provides performative solution (this constraint) → worker internalization of evaluation criteria sustains the system (child). Each constraint has distinct epsilon and classification.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(performance_evaluation_theater, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
