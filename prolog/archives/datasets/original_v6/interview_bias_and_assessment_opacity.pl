% ============================================================================
% CONSTRAINT STORY: interview_bias_and_assessment_opacity
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_interview_bias_and_assessment_opacity, []).

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
 *   constraint_id: interview_bias_and_assessment_opacity
 *   human_readable: Interview Bias and Assessment Opacity in Hiring
 *   domain: labor_market/human_resources
 *
 * SUMMARY:
 *   Interview bias and assessment opacity create a structural constraint that
 *   coordinates legitimate hiring functions (evaluating interpersonal fit,
 *   assessing motivation) while simultaneously enabling and concealing
 *   demographic extraction. The constraint exhibits core properties of
 *   Tangled Rope: a genuine coordination function (interviews do provide
 *   signal about job-relevant interpersonal dimensions that resumes cannot
 *   capture) is layered with systematic, asymmetric extraction
 *   (underrepresented candidates face compounded bias requiring higher
 *   performance for equivalent evaluation). The opacity of assessment
 *   criteria—the fact that candidates and hiring managers cannot articulate
 *   precise decision rules—amplifies both the coordination function
 *   (ambiguity leaves room for nuanced judgment) and the extraction mechanism
 *   (bias hides behind subjective interpretation). The constraint's
 *   theater_ratio (0.65) reflects that contemporary hiring processes
 *   increasingly perform compliance with diversity values while maintaining
 *   underlying bias mechanisms: rubrics are adopted but applied
 *   inconsistently; structured interviews are announced but implementation
 *   drifts; demographic monitoring produces metrics without behavior change.
 *   The extractiveness trajectory (0.35→0.58 over interval) shows
 *   accumulation as organizations respond to diversity pressure by adding
 *   performative layers rather than addressing underlying bias in assessment.
 *
 * KEY AGENTS:
 *   - Structurally Disadvantaged Candidates: Primary victim (powerless/trapped) — face compounded bias, no alternative recruitment pathways for most roles, cannot exit without abandoning career
 *   - Privileged Candidates: Secondary beneficiary (moderate/constrained) — benefit from implicit bias and cultural alignment signals; also genuinely coordinate through interview
 *   - Hiring Organizations: Institutional beneficiary (institutional/arbitrage) — view process as legitimate coordination mechanism; experience low extraction because bias aligns with their recruitment preferences
 *   - Diversity and Inclusion Professionals: Organized agents (organized/constrained) — implement structured interviews, rubrics, blind resume reviews as sunset-based interventions; face organizational resistance limiting intervention depth
 *   - Hiring Managers: Institutional actors (institutional/arbitrage) — maintain unstructured interview ritual through institutional inertia; identity-locked in meritocracy narrative preventing perception of bias in own assessments
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing contingent choice (synchronous interviews) as inherent hiring necessity
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(interview_bias_and_assessment_opacity, 0.58).
domain_priors:suppression_score(interview_bias_and_assessment_opacity, 0.68).
domain_priors:theater_ratio(interview_bias_and_assessment_opacity, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(interview_bias_and_assessment_opacity, extractiveness, 0.58).
narrative_ontology:constraint_metric(interview_bias_and_assessment_opacity, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(interview_bias_and_assessment_opacity, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(interview_bias_and_assessment_opacity, tangled_rope).
narrative_ontology:human_readable(interview_bias_and_assessment_opacity, "Interview Bias and Assessment Opacity in Hiring").
narrative_ontology:topic_domain(interview_bias_and_assessment_opacity, "labor_market/human_resources").

domain_priors:requires_active_enforcement(interview_bias_and_assessment_opacity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(interview_bias_and_assessment_opacity, hiring_gatekeepers).
narrative_ontology:constraint_beneficiary(interview_bias_and_assessment_opacity, dominant_demographic_groups).
narrative_ontology:constraint_victim(interview_bias_and_assessment_opacity, underrepresented_candidates).
narrative_ontology:constraint_victim(interview_bias_and_assessment_opacity, merit_discovery).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: STRUCTURALLY DISADVANTAGED CANDIDATE (SNARE) — Candidate from underrepresented group faces compounded barriers: unconscious bias in interview assessment, lack of social capital to signal unmeasurable qualities interviewers reward, inability to exit the hiring process without abandoning career aspirations. Maximum suppression — no alternative recruitment pathways for majority of entry-level positions. Extraction is severe: candidate must outperform peers on measurable dimensions yet receives lower subjective ratings.
constraint_indexing:constraint_classification(interview_bias_and_assessment_opacity, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: PRIVILEGED CANDIDATE (TANGLED ROPE) — Benefits from implicit bias (cultural alignment, network signaling, interpretation of gaps as 'thoughtfulness' rather than unemployment). Also benefits from coordination function: interview process does identify some skill mismatches and role fit. Constrained because candidates cannot skip interviews, but face lower barrier to success — cultural capital provides buffer. Mixed experience: real coordination benefit alongside asymmetric advantage from bias.
constraint_indexing:constraint_classification(interview_bias_and_assessment_opacity, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: HIRING ORGANIZATION (ROPE) — Views interview process as pure coordination mechanism solving the legitimate problem of assessing cultural fit, interpersonal skills, and motivation — dimensions that cannot be evaluated from resume alone. Organization experiences the process as beneficial: selects candidates who integrate well, articulate reasoning clearly in real time. Experiences low extraction because the process does provide genuine signal even if systematically biased.
constraint_indexing:constraint_classification(interview_bias_and_assessment_opacity, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: DIVERSITY AND INCLUSION MOVEMENT (SCAFFOLD) — Organized actors (compliance officers, DEI programs, standardized interview protocols, blind resume reviews) are implementing sunset-based interventions: structured interviews reduce subjective bias, rubric-based assessment limits interpretation, algorithmic screening removes demographic signals. These are explicitly temporary supports with measurable endpoints — when bias is reduced below threshold X, the intervention succeeds and phases out. Current implementation shows mixed success (theater_ratio rising as performative compliance spreads without underlying cultural change).
constraint_indexing:constraint_classification(interview_bias_and_assessment_opacity, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: THE UNSTRUCTURED INTERVIEW RITUAL (PITON) — The traditional 'have a coffee and chat' interview persists despite well-documented bias and poor predictive validity for job performance. Organizations maintain this ritual through institutional inertia — 'this is how we've always done it' — and through performative justification ('we need to see personality'). Theater ratio is high: the ritual is maintained largely for theatrical reasons (signaling organizational investment in candidates, performing meritocracy) rather than functional reasons (the interview does not reliably predict performance). Structured alternatives exist but have not displaced the ritual.
constraint_indexing:constraint_classification(interview_bias_and_assessment_opacity, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, some assessment uncertainty is inherent to hiring: no evaluation can perfectly predict job performance, and subjective judgment is unavoidable when assessing motivation and fit. This perspective sees interview bias as an immutable property of human decision-making. However, the structural data contradicts the mountain classification — the engine will detect this as a false summit, revealing that what is naturalized as 'human limitations' is actually a contingent institutional arrangement (the choice to rely on synchronous interviews rather than work samples, trials, or peer review).
constraint_indexing:constraint_classification(interview_bias_and_assessment_opacity, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(interview_bias_and_assessment_opacity_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(interview_bias_and_assessment_opacity, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(interview_bias_and_assessment_opacity, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(interview_bias_and_assessment_opacity, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(interview_bias_and_assessment_opacity, TR),
    TR >= 0.70.

:- end_tests(interview_bias_and_assessment_opacity_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate, reflecting genuine coordination layered with significant asymmetric extraction. The interview does solve the legitimate problem of assessing dimensions resumes cannot capture. However, the opacity of criteria and documented bias in subjective assessment mean that underrepresented candidates must outperform peers on measurable dimensions while receiving systematically lower subjective ratings. Extractiveness has increased (0.35→0.58) as organizations respond to diversity scrutiny with performative protocols (rubrics, standardized questions) that are adopted but applied inconsistently—adding theater without reducing bias. Suppression (0.68): High. Structurally disadvantaged candidates face multiple suppression layers: (1) practical—most career pathways route through interviews with no viable alternatives; (2) informational—assessment criteria are opaque, making it impossible to optimize preparation; (3) identity-based—internalized acceptance of bias as inevitable or even legitimate ('maybe I'm just not culture fit'). Theater ratio (0.65): Moderate-high. The traditional unstructured interview ('have a coffee and chat') persists despite documented poor predictive validity and well-known bias, maintained through institutional inertia (Piton perspective). Simultaneous adoption of performative compliance measures (rubrics that are announced but applied inconsistently, diversity monitoring that produces metrics without behavior change) increases theater as organizations perform meritocracy while maintaining bias mechanisms.
 *
 * PERSPECTIVAL GAP:
 *   Fundamental disagreement on constraint type reflects disagreement on whether assessment uncertainty is inherent or constructed. Hiring organizations and privileged candidates perceive genuine coordination function—the interview does assess job-relevant dimensions. Disadvantaged candidates and merit advocates perceive pure extraction—the process penalizes them without functional justification. The diversity movement perceives a temporary coordination failure (Scaffold) with technical solutions (structured interviews). The hiring manager perceives a ritual with performative function (Piton)—they maintain the interview process not because it works but because 'this is how we do things.' The analytical observer perceives an immutable human limitation (Mountain)—assessment always involves subjective judgment. The framework reveals this as false summit: the choice to rely on synchronous interviews is contingent. Work samples and peer review exist as alternatives; synchronous interviews are maintained through institutional inertia and alignment with organizational hiring preferences, not through structural necessity.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality derives from structural position within the constraint. Disadvantaged candidates occupy the victim position (high d, trapped exit): they experience maximum extraction because they cannot avoid interviews and face systematic assessment bias. Privileged candidates occupy the mixed position (moderate d, constrained exit): they benefit from bias (lower assessment bar) but also genuinely benefit from coordination function; derived d reflects both beneficiary and some victim status. Hiring organizations occupy the clear beneficiary position (low d, arbitrage exit): they benefit from interview-based selection that preserves their hiring preferences while maintaining plausible meritocratic narrative; they could adopt alternative assessment methods with modest cost but choose not to. The derived directionality for disadvantaged candidates (d≈0.88) applies the sigmoid f(d)≈1.25, producing high experienced extraction chi. For privileged candidates (d≈0.42) f(d)≈0.40, substantially lower experienced extraction. For hiring organizations (d≈0.05) f(d)≈-0.12, negative experienced extraction (extraction flows toward them). Scope (national, σ=1.0) does not amplify or dampen: this is a national-scale labor market constraint.
 *
 * MANDATROPHY ANALYSIS:
 *   TANGLED ROPE RESOLUTION: This constraint is neither pure coordination nor pure extraction—it is genuinely mixed. The interview process does solve the legitimate problem of assessing interpersonal fit and motivation (coordination function is real). The process also systematically advantages privileged candidates through implicit bias while maintaining a meritocratic narrative that obscures the asymmetry (extraction function is real). The mandatrophy resolves by acknowledging both functions and measuring their relative weight. The extractiveness (0.58) reflects the decomposition: genuine coordination might justify ε≈0.15-0.20 if applied equally; documented bias and opacity increase the value to 0.58, capturing the extraction component. The theater ratio (0.65) captures performative compliance: organizations announce commitment to meritocracy and adopt diversity measures, increasing theater as gap widens between stated values and actual practice. The constraint cannot be classified as pure Rope (no victims) or pure Snare (genuine coordination function exists) without losing diagnostic precision. Tangled Rope is the accurate classification because both the coordination and extraction functions are necessary to explain the structural data and the perspectival gap.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    bias_vs_legitimate_signal,
    'How much of the observed performance gap between demographic groups in interviews reflects genuine differences in interpersonal skills vs. systematic bias in assessment?',
    'Longitudinal hiring study: compare interview ratings with job performance outcomes; separate ratings from actual performance using randomized interview structures; analyze rater calibration across demographically similar candidates with varying backgrounds',
    'If legitimate signal dominates: interview process is coordination mechanism (Rope/Tangled Rope). If bias dominates: process is pure extraction (Snare). Affects whether assessments correlate with job performance or merely reproduce demographic hierarchy.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(bias_vs_legitimate_signal, empirical, 'Proportion of performance gap attributable to bias vs. genuine skill differences').

omega_variable(
    structured_interview_adoption_ceiling,
    'Why do structured interview protocols (rubrics, standardized questions, scoring) not achieve wider adoption despite consistent evidence of reduced bias and improved predictive validity?',
    'Organizational adoption analysis: survey hiring managers on barriers to structure adoption; analyze organizations that have implemented structure and measure persistence of adoption; identify whether abandonment correlates with hiring outcomes or with cultural factors',
    'If barrier is information (lack of awareness): scaffold sunset is achievable through dissemination. If barrier is cultural (preference for ''gut feel''): structured approaches hit institutional ceiling, and performance gap persists despite technical solutions.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(structured_interview_adoption_ceiling, conceptual, 'Adoption barriers for structured interview methodologies').

omega_variable(
    alternative_assessment_validity,
    'Do work sample tests, peer review, or trial project assessments predict job performance as well or better than interviews for roles where feasible?',
    'Meta-analysis of hiring study outcomes; randomized trials comparing interview-based selection with work sample or trial-based selection for specific role categories; longitudinal performance tracking of hired candidates across methodologies',
    'If alternatives are equally or more valid: interview process is maintained through performative ritual (Piton) rather than functional necessity; mountain classification is false summit. If interviews have unique predictive value: legitimate coordination function exists despite bias.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(alternative_assessment_validity, empirical, 'Comparative validity of alternative assessment methodologies').

omega_variable(
    suppression_mechanism_structural_vs_internalized,
    'Is the measured suppression (0.68) primarily structural (external barriers to alternative pathways, economic dependence on employment) or internalized (candidates accept bias frames as legitimate or inevitable)?',
    'Post-hiring trajectory analysis: survey candidates on perception of interview fairness; analyze whether bias acceptance persists after hiring; qualitative interviews on whether candidates view bias as legitimate constraint or extractive mechanism',
    'If structural: suppression persists; candidates must navigate barrier regardless of beliefs. If internalized: suppression is partially self-inflicted; identity frame shift could increase exit perception (raises biological timeframe exit_options from trapped toward constrained or mobile).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_vs_internalized, empirical, 'Structural vs. internalized suppression mechanism in hiring bias').

omega_variable(
    identity_lock_in_meritocracy_narrative,
    'To what extent are hiring managers identity-locked into the meritocracy narrative (''the best candidate won'') such that they cannot perceive systematic bias in their own assessments?',
    'Cognitive capture measurement: present hiring managers with identical application materials attributed to different demographic groups; measure consistency of assessment; assess whether managers can recognize bias in hypothetical scenarios vs. their own historical decisions',
    'If high identity lock: hiring managers require frame-shift interventions rather than data/training; structured protocols succeed only if they bypass subjective judgment. If low identity lock: training and transparency are sufficient; managers can recognize and correct for bias.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_in_meritocracy_narrative, empirical, 'Identity lock in meritocracy narrative among hiring gatekeepers').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(interview_bias_and_assessment_opacity, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(intview_tr_t0, interview_bias_and_assessment_opacity, theater_ratio, 0, 0.45).
narrative_ontology:measurement(intview_tr_t5, interview_bias_and_assessment_opacity, theater_ratio, 5, 0.58).
narrative_ontology:measurement(intview_tr_t10, interview_bias_and_assessment_opacity, theater_ratio, 10, 0.65).

% Extraction over time
narrative_ontology:measurement(intview_be_t0, interview_bias_and_assessment_opacity, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(intview_be_t5, interview_bias_and_assessment_opacity, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(intview_be_t10, interview_bias_and_assessment_opacity, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(interview_bias_and_assessment_opacity, attachment_coordination).
narrative_ontology:affects_constraint(interview_bias_and_assessment_opacity, demographic_representation_disparities).
narrative_ontology:affects_constraint(interview_bias_and_assessment_opacity, career_trajectory_inequality).

% DUAL FORMULATION NOTE:
% Interview bias is downstream of organizational hiring preferences and upstream of demographic representation disparities. The constraint operates at the individual assessment level but produces systemic effects in demographic composition and career trajectories. Decomposition into structured vs. unstructured interview mechanisms would yield separate constraints with different ε values (structured interviews show lower bias, ε≈0.35; unstructured interviews show higher bias, ε≈0.72), but this story treats the composite institutional practice as a single constraint reflecting mixed adoption.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(interview_bias_and_assessment_opacity, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
