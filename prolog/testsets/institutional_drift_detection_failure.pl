% ============================================================================
% CONSTRAINT STORY: institutional_drift_detection_failure
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_institutional_drift_detection_failure, []).

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
 *   constraint_id: institutional_drift_detection_failure
 *   human_readable: Institutional Drift Detection Failure
 *   domain: organizational/governance
 *
 * SUMMARY:
 *   Institutional drift detection failure describes a structural constraint
 *   in which organizations systematically fail to detect, measure, or correct
 *   divergence between their stated mission and actual function. This
 *   constraint operates at the level of organizational governance and
 *   feedback architecture: the information systems, accountability
 *   mechanisms, and performance metrics that should reveal mission
 *   misalignment have themselves become degraded or misaligned with detection
 *   function. The constraint exhibits a characteristic escalation pattern —
 *   as institutions grow in complexity and functional scope, the burden of
 *   drift detection increases precisely when the capacity to perform
 *   detection decreases, because resources are reallocated to new functions
 *   rather than to monitoring. The theater ratio progression (0.35 → 0.74)
 *   reflects Goodhart's Law in action: performance metrics (budget execution,
 *   activity counts, reporting compliance) replace mission alignment
 *   measures, and institutions optimize for measurable proxies rather than
 *   actual function. By the end of the interval, the monitoring and
 *   accountability apparatus is almost entirely performative — it reports
 *   that the institution is functioning while the mission itself has
 *   substantially drifted.
 *
 * KEY AGENTS:
 *   - Institutional Mission: Primary victim (powerless/trapped) — the stated purpose degrades silently through function creep; has no agency to resist or self-correct
 *   - External Stakeholders: Secondary victims (powerless/trapped) — locked into institutional dependency despite service degradation; cannot easily organize around alternatives due to coordination barriers
 *   - Field Practitioners: Tertiary victims (moderate/constrained) — share commitment to mission but face career and credential lock-in; experience extraction through misalignment of institutional incentives with field integrity
 *   - Entrenched Management: Primary beneficiaries (institutional/arbitrage) — expand authority and resource control through function creep; experience drift as coordination opportunity rather than failure
 *   - Monitoring and Accountability Systems: Secondary beneficiary (institutional/constrained) — persist through ritual rather than function; serve to provide plausible deniability of drift
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing contingent design failures as inherent to complex organizations
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(institutional_drift_detection_failure, 0.58).
domain_priors:suppression_score(institutional_drift_detection_failure, 0.65).
domain_priors:theater_ratio(institutional_drift_detection_failure, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(institutional_drift_detection_failure, extractiveness, 0.58).
narrative_ontology:constraint_metric(institutional_drift_detection_failure, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(institutional_drift_detection_failure, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(institutional_drift_detection_failure, tangled_rope).
narrative_ontology:human_readable(institutional_drift_detection_failure, "Institutional Drift Detection Failure").
narrative_ontology:topic_domain(institutional_drift_detection_failure, "organizational/governance").

domain_priors:requires_active_enforcement(institutional_drift_detection_failure).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(institutional_drift_detection_failure, entrenched_management).
narrative_ontology:constraint_beneficiary(institutional_drift_detection_failure, institutional_insiders).
narrative_ontology:constraint_victim(institutional_drift_detection_failure, institutional_mission).
narrative_ontology:constraint_victim(institutional_drift_detection_failure, external_stakeholders).
narrative_ontology:constraint_victim(institutional_drift_detection_failure, field_integrity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: INSTITUTIONAL MISSION (SNARE) — The stated purpose and integrity of the institution have no agency; cannot exit or resist drift. Bears full cost as the organization accumulates functions misaligned with its original mandate. Experiences maximum extraction through silent degradation of core purpose.
constraint_indexing:constraint_classification(institutional_drift_detection_failure, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: EXTERNAL STAKEHOLDERS (SNARE) — Communities, partners, and publics dependent on the institution for specific function become locked into serving the institution's drift rather than the institution serving their needs. High suppression: cannot reorganize around alternative providers without massive coordination costs. Trapped by institutional path-dependency.
constraint_indexing:constraint_classification(institutional_drift_detection_failure, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 3: FIELD PRACTITIONERS (TANGLED ROPE) — Professionals embedded in the institutional domain face mixed incentives: genuine coordination through shared standards and knowledge infrastructure, yet asymmetric extraction through mission drift that pulls resources and attention away from core function. Constrained exit due to career investment and credential lock.
constraint_indexing:constraint_classification(institutional_drift_detection_failure, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: ENTRENCHED MANAGEMENT (ROPE) — Experiences the constraint as pure coordination: expanding functions, budget authority, and reporting lines creates network effects that benefit institutional insiders. High arbitrage options: can reallocate resources, reshape governance, redefine metrics. Net beneficiary from drift — more scope means more control.
constraint_indexing:constraint_classification(institutional_drift_detection_failure, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: MONITORING AND ACCOUNTABILITY SYSTEMS (PITON) — The audit trails, performance metrics, and oversight mechanisms that should detect drift have become largely performative. Theater ratio is high because reporting systems measure proxy metrics (activity, spending, headcount) rather than mission alignment. The systems persist through regulatory requirement, not functional effectiveness — institutional inertia maintains the illusion of oversight.
constraint_indexing:constraint_classification(institutional_drift_detection_failure, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a universal perspective, organizational drift is an inevitable consequence of bounded rationality, incomplete information about internal states, and the natural tendency of complex systems toward entropy. Mission creep is presented as inherent to institutional growth. However, this view naturalizes what the structural data reveals as contingent institutional design: the absence of feedback mechanisms is not a law of nature but an active suppression of information visibility.
constraint_indexing:constraint_classification(institutional_drift_detection_failure, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(institutional_drift_detection_failure_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(institutional_drift_detection_failure, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(institutional_drift_detection_failure, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(institutional_drift_detection_failure, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(institutional_drift_detection_failure, TR),
    TR >= 0.70.

:- end_tests(institutional_drift_detection_failure_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Elevated. The constraint involves asymmetric benefit (entrenched management gains authority and resource control) paired with asymmetric cost (external stakeholders and mission integrity degrade). The measurement trajectory shows escalation — extractiveness grows as resources are redirected toward new functions that benefit insiders rather than toward mission alignment. The value is not extreme (not 0.75+) because some of the function creep is organizationally functional and reflects legitimate response to environmental change, not pure predation. However, the absence of mechanisms to distinguish legitimate evolution from mission drift means that even functional change is extracted as concentrated benefit to insider managers. Suppression (0.65): High. Multiple layers of suppression operate: (1) Technical — measurement infrastructure cannot capture mission alignment at organizational scale; (2) Institutional — audit and oversight systems have themselves drifted toward theater, creating plausible deniability; (3) Informational — drift is diffuse and incremental, making perception difficult for distributed stakeholders; (4) Structural — stakeholders have high switching costs and path-dependent dependencies, making exit infeasible. Theater ratio (0.68): High and rising. The performance metrics that institutions use to assess themselves (budget utilization, activity counts, hiring, publications, units produced) have decoupled from mission alignment. Monitoring systems report that institutions are functional while their core purpose drifts undetected. The escalation from 0.35 to 0.74 indicates progressive substitution of proxy goals (measurable activity) for mission outcomes (actual social/scientific/public service value).
 *
 * PERSPECTIVAL GAP:
 *   Why do entrenched managers and external stakeholders perceive this constraint so differently? The entrenched management group experiences drift as coordination — new functions expand their authority, create new reporting relationships, provide opportunities for strategic resource allocation. Their arbitrage exit options mean they can always shift to managing something else if current functions become too burdensome. The constraint serves them as a tool for accumulating organizational scope. External stakeholders, by contrast, experience the institution as a provider of a specific function, and they bear the cost when that function drifts. They cannot exit (high switching costs, no alternatives, path-dependent dependencies) and they have no ability to influence institutional direction (powerless position). For them, the same organizational expansion that benefits managers represents a deterioration of service quality. The monitoring systems fail to translate between these perspectives — they measure activity rather than alignment, so they cannot articulate either stakeholder loss or the source of insider benefit.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is determined by each agent's structural relationship to the extraction mechanism. Entrenched management derive d from institutional power + arbitrage exit options: they benefit from function creep and have full flexibility to reshape the organization, producing low d and negative effective extraction (they experience the constraint as beneficial). External stakeholders derive d from powerless + trapped exit options: they depend on the institution and cannot reorganize around alternatives, producing high d and maximum experienced extraction (they bear the cost of mission drift). Field practitioners derive d from moderate power + constrained exit options: they can perceive and articulate the drift but cannot exit without significant career cost, producing moderate-high d and correspondingly elevated chi. The beneficiary/victim declarations feed into this computation: institutional insiders are beneficiaries (derive low d), while mission and stakeholders are victims (derive high d). Monitoring systems are complex — they are institutional beneficiaries in the short term (their expansion is driven by the same resource dynamics), but their perceptual capture makes them victims of their own degradation (they cannot see the drift they are supposed to detect).
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy here is: 'Is institutional drift a natural law of complex organizations (mountain), or a failure of institutional design that can be corrected (tangled rope / snare)?' The analytical observer's mountain perspective asserts that mission creep is inevitable, that organizations naturally expand, that drift is the cost of organizational growth. However, the structural data contradicts this. If drift were inherent to organization complexity, it would be equally costly to all participants — it would appear as Rope (shared coordination burden) rather than as Snare (asymmetric extraction). The fact that institutional insiders benefit from drift while stakeholders lose reveals that drift is not a natural law but an extractive mechanism maintained by absent or performative oversight. The constraint resolves the mandatrophy by showing that: (1) The mountain perspective is a false summit — it naturalizes what is actually a contingent governance failure; (2) The Snare perspective is the true structural position for stakeholders and mission; (3) The Rope perspective is the true experience for entrenched managers; (4) The constraint is a Tangled Rope overall because it combines genuine coordination infrastructure (the institution genuinely does coordinate some activities) with asymmetric extraction (the coordination benefits insiders disproportionately). The critical insight: organizations with rigorous drift detection systems (measuring mission alignment alongside activity) show significantly lower asymmetry, proving that drift detection is not a natural law but a design choice. Institutions that implement continuous stakeholder feedback, mission audits, and real-time function-mapping show different classification patterns — they trend toward Rope rather than Tangled Rope, because the extraction is corrected by feedback.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    feedback_suppression_intentionality,
    'Is the absence of drift detection mechanisms the result of deliberate institutional design to avoid accountability, or the emergent consequence of coordination failure?',
    'Historical analysis of when monitoring systems were scaled down or metrics changed; interviews with governance designers; comparison of drift detection investment levels across peer institutions with and without mission change pressure',
    'If deliberate: suppression is active and the constraint is a pure Snare from the beneficiary perspective. If emergent: drift detection failure is a coordination problem (Tangled Rope or Rope depending on beneficiary awareness). Classification changes from extraction-dominant to coordination-with-extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(feedback_suppression_intentionality, empirical, 'Whether drift detection suppression is deliberate institutional design or emergent coordination failure').

omega_variable(
    drift_detection_feasibility,
    'Can institutional drift be detected in real-time given the complexity of modern organizations, or is lag between drift occurrence and detection an inherent structural limit?',
    'Analysis of detection latency in organizations with rigorous continuous monitoring; comparison with historical detection lags in organizations with periodic review; identification of drift types that are inherently latency-prone',
    'If real-time detection is feasible: current lag represents suppression (Snare confirmed). If inherent lag exists: some extraction is legitimate overhead (extractiveness should be reduced, classification shifts toward Scaffold with sunset).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(drift_detection_feasibility, empirical, 'Whether institutional drift can be detected in real-time or lag is structural').

omega_variable(
    mission_definition_stability,
    'Is the institution''s original mission stable enough to measure drift against, or does the mission itself legitimately evolve with context?',
    'Historical reconstruction of mission statements and stakeholder expectations; longitudinal analysis of legitimate mission evolution vs. undisclosed function creep; separation of authorized adaptation from unauthorized drift',
    'If missions are inherently plastic: drift detection fails because there is no fixed reference point (extractiveness should be reduced to 0.30, reclassify as Scaffold). If missions can be stable: drift is measurable and current absence of detection is suppression (Snare confirmed).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(mission_definition_stability, conceptual, 'Whether institutional missions can be stable reference points for drift measurement').

omega_variable(
    stakeholder_exit_barrier_mechanism,
    'What proportion of external stakeholder lock-in is due to genuine high coordination switching costs vs. institutional suppression of alternative pathways?',
    'Cost-benefit analysis of switching to alternative service providers; institutional analysis of barriers erected against competitors or alternatives; case studies of stakeholders who successfully exited despite institutional pressure',
    'If switching costs are genuine: suppression may be lower than measured (0.65 → 0.45), Snare may downgrade to Tangled Rope. If barriers are suppression: high suppression is confirmed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(stakeholder_exit_barrier_mechanism, empirical, 'Whether stakeholder lock-in is due to coordination costs or institutional suppression').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(institutional_drift_detection_failure, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(idd_tr_t0, institutional_drift_detection_failure, theater_ratio, 0, 0.35).
narrative_ontology:measurement(idd_tr_t3, institutional_drift_detection_failure, theater_ratio, 3, 0.52).
narrative_ontology:measurement(idd_tr_t6, institutional_drift_detection_failure, theater_ratio, 6, 0.68).
narrative_ontology:measurement(idd_tr_t9, institutional_drift_detection_failure, theater_ratio, 9, 0.74).

% Extraction over time
narrative_ontology:measurement(idd_be_t0, institutional_drift_detection_failure, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(idd_be_t3, institutional_drift_detection_failure, base_extractiveness, 3, 0.42).
narrative_ontology:measurement(idd_be_t6, institutional_drift_detection_failure, base_extractiveness, 6, 0.58).
narrative_ontology:measurement(idd_be_t9, institutional_drift_detection_failure, base_extractiveness, 9, 0.63).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(institutional_drift_detection_failure, enforcement_mechanism).
narrative_ontology:affects_constraint(institutional_drift_detection_failure, regulatory_capture).
narrative_ontology:affects_constraint(institutional_drift_detection_failure, metric_substitution).
narrative_ontology:affects_constraint(institutional_drift_detection_failure, stakeholder_voice_suppression).

% DUAL FORMULATION NOTE:
% Institutional drift detection failure is upstream of specific governance failures (regulatory capture, mission creep in particular sectors). The generic drift detection constraint affects any institution with complex functions and distributed stakeholders; sector-specific instances (healthcare mission drift, research institution function creep, government agency mission expansion) share this constraint family structure but instantiate it with different epsilon values reflecting sector-specific measurement feasibility.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(institutional_drift_detection_failure, institutional, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
