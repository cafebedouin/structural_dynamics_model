% ============================================================================
% CONSTRAINT STORY: recalibration_interpretive_validity
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-01-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_recalibration_interpretive_validity, []).

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
    narrative_ontology:constraint_vindicates/2,
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
 *   constraint_id: recalibration_interpretive_validity
 *   human_readable: Recalibration Interpretive Validity in System Dynamics Modeling
 *   domain: system_dynamics/industrial_ecology/sustainability_science
 *
 * SUMMARY:
 *   The recalibration interpretive validity constraint emerges from a
 *   fundamental tension in system dynamics modeling: parameter adjustments
 *   that improve model fit to historical data may reflect either (a)
 *   discovered structural truths about the real system or (b) artifacts of
 *   fitting noise in incomplete proxy measurements via local optimization.
 *   This ambiguity creates asymmetric costs: modelers capture methodological
 *   authority and funding priority during the validation window, while policy
 *   decision-makers bear the full cost of model error with no independent
 *   capacity to distinguish genuine discovery from overfitting. The
 *   constraint exhibits tangled rope structure from the analytical
 *   perspective: it serves a genuine coordination function (iterative model
 *   improvement is how science advances) while embedding asymmetric
 *   extraction (modelers collect authority, decision-makers pay error costs).
 *   The theater ratio (0.58) reflects that validation rituals increasingly
 *   substitute for genuine out-of-sample testing: many recalibration studies
 *   report improved in-sample fit without demonstrating predictive validity
 *   on held-out data. Suppression has increased over the interval (0.45 →
 *   0.62) as policy mandates for quantitative evidence have hardened while
 *   validation capacity has not kept pace, trapping decision-makers in
 *   dependence on models they cannot independently verify.
 *
 * KEY AGENTS:
 *   - Model Validation Community: Primary beneficiary (institutional/arbitrage) — captures citation advantage, funding priority, and methodological authority during validation window; experiences constraint as coordination
 *   - Academic Modelers: Secondary beneficiary (institutional/arbitrage) — benefit from continuous model improvement doctrine that justifies ongoing research funding
 *   - Policy Decision Makers: Primary victim (powerless/trapped) — mandated to use quantitative evidence but cannot distinguish discovery from overfitting; bear full cost of model error
 *   - Public Resource Allocation: Secondary victim (powerless/trapped) — abstract collective good that suffers from misallocated resources based on overfit models
 *   - Independent Research Groups: Mixed position (moderate/constrained) — benefit from validation infrastructure but face career risk from challenging established models
 *   - Open Modeling Consortium: Organized agents (organized/constrained) — building transparency infrastructure but constrained by institutional pressure for legacy compatibility
 *   - Bayesian Calibration Advocates: Institutional actors (institutional/mobile) — see interpretive validity problem as temporary with sunset via formal uncertainty quantification
 *   - Analytical Observer: Civilizational view (analytical/analytical) — sees both genuine coordination function and asymmetric extraction as structural features
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(recalibration_interpretive_validity, 0.48).
domain_priors:suppression_score(recalibration_interpretive_validity, 0.62).
domain_priors:theater_ratio(recalibration_interpretive_validity, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(recalibration_interpretive_validity, extractiveness, 0.48).
narrative_ontology:constraint_metric(recalibration_interpretive_validity, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(recalibration_interpretive_validity, theater_ratio, 0.58).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(recalibration_interpretive_validity, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(recalibration_interpretive_validity, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(recalibration_interpretive_validity, tangled_rope).
narrative_ontology:human_readable(recalibration_interpretive_validity, "Recalibration Interpretive Validity in System Dynamics Modeling").
narrative_ontology:topic_domain(recalibration_interpretive_validity, "system_dynamics/industrial_ecology/sustainability_science").

domain_priors:requires_active_enforcement(recalibration_interpretive_validity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(recalibration_interpretive_validity, model_validation_community).
narrative_ontology:constraint_beneficiary(recalibration_interpretive_validity, academic_modelers).
narrative_ontology:constraint_victim(recalibration_interpretive_validity, policy_decision_makers).
narrative_ontology:constraint_victim(recalibration_interpretive_validity, public_resource_allocation).
narrative_ontology:constraint_vindicates(recalibration_interpretive_validity, continuous_model_improvement_doctrine).
narrative_ontology:constraint_vindicates(recalibration_interpretive_validity, quantitative_rigor_in_policy_analysis).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: POLICY DECISION MAKERS (SNARE) — Trapped by mandate to use quantitative evidence for resource allocation decisions. Cannot distinguish genuine structural discovery from overfitting artifacts. Face career risk from both acting on flawed models and from rejecting expert consensus. Maximum extraction: bear full cost of model error with no capacity to verify validity independently.
constraint_indexing:constraint_classification(recalibration_interpretive_validity, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: INDEPENDENT RESEARCH GROUPS (TANGLED ROPE) — Constrained by resource requirements for replication and career risk of challenging established models, but benefit from the validation ecosystem through methodological standards and collaborative infrastructure. Experience both coordination (shared validation protocols enable cumulative knowledge) and extraction (pressure to validate rather than challenge, publication bias toward positive results).
constraint_indexing:constraint_classification(recalibration_interpretive_validity, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 3: MODEL VALIDATION COMMUNITY (ROPE) — Primary beneficiary. Captures citation advantage, funding priority, and methodological authority during the window between recalibration publication and independent validation. Experiences the constraint as coordination: establishing validation standards enables cumulative model improvement. Net beneficiary with arbitrage exit options.
constraint_indexing:constraint_classification(recalibration_interpretive_validity, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: OPEN MODELING CONSORTIUM (TANGLED ROPE) — Organized agents (open-source model repositories, registered validation protocols, pre-registered parameter hypotheses) building transparency infrastructure. Experience coordination benefits (shared code and data reduce replication costs) but also extraction (institutional pressure to maintain compatibility with legacy model structures, funding concentration in established frameworks).
constraint_indexing:constraint_classification(recalibration_interpretive_validity, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: BAYESIAN CALIBRATION ADVOCATES (SCAFFOLD) — See the interpretive validity problem as temporary coordination failure with a sunset: Bayesian methods with proper priors and uncertainty quantification will replace point-estimate recalibration, making parameter changes interpretable as posterior updates rather than optimization artifacts. Sunset logic: as computational capacity increases and Bayesian workflows mature, the ambiguity between discovery and overfitting becomes resolvable through formal uncertainty propagation.
constraint_indexing:constraint_classification(recalibration_interpretive_validity, scaffold,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (TANGLED ROPE) — From a civilizational perspective, recalibration serves genuine coordination function (iterative model improvement is how science advances) but embeds asymmetric extraction (modelers capture authority and resources while decision-makers bear error costs). The constraint requires active enforcement through peer review standards, funding allocation, and policy mandates for quantitative evidence. Both coordination and extraction are structural features, not perspectival artifacts.
constraint_indexing:constraint_classification(recalibration_interpretive_validity, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(recalibration_interpretive_validity_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(recalibration_interpretive_validity, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(recalibration_interpretive_validity, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(recalibration_interpretive_validity, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(recalibration_interpretive_validity_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.48): Moderate-high. The model validation community captures career and funding benefits during the multi-year validation window, while policy decision-makers bear error costs with no independent verification capacity. However, extraction is not maximal because some recalibrations do reflect genuine structural discovery, and the validation ecosystem does provide real coordination benefits through shared methodological standards. The value reflects that roughly half of the parameter authority captured by modelers represents legitimate first-mover reward for methodological innovation, while the other half represents extraction from decision-makers' inability to distinguish discovery from overfitting. Suppression (0.62): Moderate-high and increasing. Policy mandates for quantitative evidence create structural dependence on models. Publication bias against negative validation results suppresses challenges to established models. Computational and data access barriers prevent independent validation. Career risk for challenging consensus models. But suppression is not total — some independent validation does occur, and open-science norms are reducing barriers. Theater ratio (0.58): Moderate-high and increasing. Validation rituals increasingly substitute for genuine out-of-sample testing. Many recalibration studies report improved in-sample fit (lower NRMSD on calibration data) without demonstrating predictive validity on held-out data. Peer review focuses on methodological rigor of the optimization procedure rather than on whether parameter changes have independent physical/economic justification. The theater has increased as model complexity has outpaced validation capacity. Accessibility collapse (0.35): Low-moderate. Alternatives to recalibration-based validation persist: ensemble modeling, scenario analysis, qualitative system mapping, participatory modeling. The constraint does not collapse alternatives as completely as a genuine natural law would. Resistance (0.55): Moderate-high. The constraint meets substantial resistance from decision-makers who distrust black-box recalibration, from Bayesian advocates who reject point-estimate optimization, and from qualitative systems thinkers who question the entire quantification paradigm.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates how the same structural phenomenon — parameter changes from model recalibration — appears differently depending on the observer's position. The model validation community sees coordination (Rope): they are solving the legitimate problem of iterative model improvement. Bayesian advocates see a temporary problem with a sunset (Scaffold): formal uncertainty quantification will resolve the interpretive ambiguity. Independent research groups see mixed coordination and extraction (Tangled Rope): the validation ecosystem both enables and constrains their work. Policy decision-makers see pure extraction (Snare): they are trapped in dependence on models they cannot verify, bearing full error costs. The analytical observer sees tangled rope: both the coordination function (science advances through iterative refinement) and the extraction mechanism (asymmetric cost distribution) are structural features, not perspectival artifacts. The perspectival gap is not a disagreement about facts but a consequence of different structural relationships to the constraint: beneficiaries experience coordination, victims experience extraction, and the analytical view sees both.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality is determined by structural position relative to the extraction flow. The model validation community are primary beneficiaries with arbitrage exit options — they capture methodological authority and can pivot to alternative research areas if validation standards shift. This produces low directionality toward the constraint (they benefit from it) and low or negative effective extraction. Policy decision-makers are primary victims with trapped exit options — mandated to use quantitative evidence, cannot distinguish discovery from overfitting, bear full error costs. This produces high directionality (they are targets) and high effective extraction. Independent research groups are in a mixed position: they benefit from validation infrastructure (coordination function) but face career risk from challenging established models (extraction function). Their constrained exit options and mixed beneficiary/victim status produce moderate directionality and moderate effective extraction. The open modeling consortium experiences coordination benefits from shared infrastructure but extraction from institutional pressure for legacy compatibility. Bayesian advocates see a sunset path (scaffold) because they have mobile exit options and see the interpretive validity problem as solvable via formal methods. The analytical observer sees both coordination and extraction as structural features (tangled rope) because the constraint genuinely enables cumulative model improvement while asymmetrically distributing costs.
 *
 * MANDATROPHY ANALYSIS:
 *   The recalibration interpretive validity constraint resolves the mandatrophy by demonstrating that tangled rope classification is structurally accurate when both coordination and extraction are genuine features rather than perspectival artifacts. The coordination function is real: iterative model improvement through recalibration does advance scientific understanding when parameter changes reflect discovered structural truths. The extraction mechanism is also real: modelers capture authority and resources while decision-makers bear error costs, and the validation ecosystem systematically favors confirmation over challenge. The constraint requires active enforcement through peer review standards (which focus on optimization rigor rather than predictive validity), funding allocation (which rewards continuous model improvement), and policy mandates (which create structural dependence on quantitative evidence). The tangled rope classification is not a compromise between rope and snare but a recognition that both coordination and extraction are structural properties of the same constraint. The mandate (iterative model improvement) has not outlived its function — science does advance through refinement — but the extraction mechanism (asymmetric cost distribution, validation theater, suppression of challenges) has accumulated over time as model complexity has outpaced validation capacity.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    structural_discovery_vs_overfitting,
    'Do parameter changes from recalibration reflect discovered structural truths about the real system or are they artifacts of fitting noise in incomplete proxies via local optimization?',
    'Out-of-sample validation: does the recalibrated model predict held-out data better than business-as-usual baseline? Do parameter changes have independent physical/economic justification beyond NRMSD reduction? Longitudinal tracking of recalibrated parameters across multiple validation windows.',
    'If structural discovery: recalibration is legitimate coordination (Rope from more perspectives). If overfitting artifacts: recalibration is extraction mechanism (Snare from more perspectives). Mixed evidence suggests Tangled Rope is structurally accurate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(structural_discovery_vs_overfitting, empirical, 'Whether recalibration discovers structure or fits noise').

omega_variable(
    proxy_completeness_threshold,
    'What level of proxy completeness is sufficient for recalibration to yield interpretable parameters rather than compensation artifacts?',
    'Systematic comparison of recalibration outcomes across models with varying proxy coverage. Identification of threshold below which parameter changes become uninterpretable. Analysis of which system components are most sensitive to proxy incompleteness.',
    'If threshold is low (e.g., 60% coverage sufficient): many existing recalibrations are valid. If threshold is high (e.g., 90%+ required): most recalibrations are fitting noise, and the constraint is more extractive than currently assessed.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(proxy_completeness_threshold, empirical, 'Proxy coverage threshold for interpretable recalibration').

omega_variable(
    bayesian_sunset_timeline,
    'Will Bayesian calibration methods with proper uncertainty quantification actually replace point-estimate recalibration in policy-relevant timelines, or will computational and institutional barriers prevent adoption?',
    'Tracking adoption rates of Bayesian methods in policy-facing system dynamics models. Assessment of computational feasibility for large-scale integrated assessment models. Analysis of institutional incentives for adopting methods that make uncertainty explicit.',
    'If adopted within 10-15 years: Scaffold perspective confirmed, interpretive validity problem has genuine sunset. If adoption stalls: the ''temporary'' framing is aspirational, and extraction persists indefinitely.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(bayesian_sunset_timeline, empirical, 'Whether Bayesian methods will replace point-estimate recalibration').

omega_variable(
    independent_validation_capacity,
    'Do independent research groups have sufficient resources and career incentives to validate recalibrated models, or does the validation ecosystem systematically favor confirmation over challenge?',
    'Analysis of validation study outcomes: ratio of confirmatory to disconfirmatory results. Assessment of publication bias in validation literature. Tracking of career outcomes for researchers who challenge vs. confirm established models.',
    'If validation capacity is adequate and unbiased: coordination function is real. If systematically biased toward confirmation: extraction mechanism is stronger than coordination function, shifting classification toward Snare.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(independent_validation_capacity, empirical, 'Whether validation ecosystem is structurally biased').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(recalibration_interpretive_validity, 0, 9).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(recalib_tr_t0, recalibration_interpretive_validity, theater_ratio, 0, 0.35).
narrative_ontology:measurement(recalib_tr_t3, recalibration_interpretive_validity, theater_ratio, 3, 0.42).
narrative_ontology:measurement(recalib_tr_t6, recalibration_interpretive_validity, theater_ratio, 6, 0.5).
narrative_ontology:measurement(recalib_tr_t9, recalibration_interpretive_validity, theater_ratio, 9, 0.58).

% Extraction over time
narrative_ontology:measurement(recalib_be_t0, recalibration_interpretive_validity, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(recalib_be_t3, recalibration_interpretive_validity, base_extractiveness, 3, 0.36).
narrative_ontology:measurement(recalib_be_t6, recalibration_interpretive_validity, base_extractiveness, 6, 0.42).
narrative_ontology:measurement(recalib_be_t9, recalibration_interpretive_validity, base_extractiveness, 9, 0.48).

% Suppression requirement over time
narrative_ontology:measurement(recalib_su_t0, recalibration_interpretive_validity, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(recalib_su_t3, recalibration_interpretive_validity, suppression_requirement, 3, 0.52).
narrative_ontology:measurement(recalib_su_t6, recalibration_interpretive_validity, suppression_requirement, 6, 0.58).
narrative_ontology:measurement(recalib_su_t9, recalibration_interpretive_validity, suppression_requirement, 9, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(recalibration_interpretive_validity, information_standard).

% DUAL FORMULATION NOTE:
% This constraint is downstream of proxy_measurement_validity (the upstream mountain constraint about whether proxy measurements capture real system dynamics). The upstream constraint establishes that proxy incompleteness is a structural feature of complex system modeling; this constraint addresses how that incompleteness interacts with recalibration procedures to create interpretive ambiguity. The two constraints have different extractiveness values: proxy_measurement_validity is a mountain (negligible extraction — it is a structural feature of measurement) while recalibration_interpretive_validity is a tangled rope (moderate extraction — it is a methodological practice that distributes costs asymmetrically).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
