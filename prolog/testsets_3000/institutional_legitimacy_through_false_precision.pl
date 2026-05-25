% ============================================================================
% CONSTRAINT STORY: institutional_legitimacy_through_false_precision
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_institutional_legitimacy_through_false_precision, []).

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
 *   constraint_id: institutional_legitimacy_through_false_precision
 *   human_readable: Institutional Legitimacy Through False Precision
 *   domain: governance/institutional_dynamics
 *
 * SUMMARY:
 *   Institutional legitimacy through false precision describes the structural
 *   pattern in which organizations maintain perceived accountability by
 *   adopting precise quantitative metrics that misrepresent the actual
 *   complexity and discretion involved in professional judgment. The
 *   constraint operates at the interface between genuine coordination need
 *   (organizations must delegate authority and evaluate performance at scale)
 *   and the epistemological limits of measurement (no single metric captures
 *   the full dimensionality of professional work). Over time, metrics become
 *   entrenched through audit apparatus, compliance systems, and professional
 *   standardization even as practitioners, auditors, and reformers
 *   acknowledge that precision claims are false. The theater ratio rises as
 *   the gap between metric claim and reality widens, indicating Goodhart
 *   drift: metrics increasingly measure their own optimization rather than
 *   underlying performance. This constraint exhibits all six DR types,
 *   revealing how the same institutional arrangement appears as natural law
 *   to the detached observer, coordination mechanism to the executive,
 *   degraded ritual to the auditor, pure extraction to the trapped
 *   practitioner, temporary problem to organized reformers, and mixed
 *   coordination-extraction to professional associations.
 *
 * KEY AGENTS:
 *   - Institutional Management: Primary beneficiary (institutional/arbitrage) — maintains delegated authority through metric legitimacy without constant oversight; can shift metric regime if pressured
 *   - Field Practitioners: Primary victim (powerless/trapped) — required to perform work according to false precision frameworks; lack voice in metric design; face career risk for deviating from metrics
 *   - Professional Associations: Secondary actor (organized/constrained) — have capacity for collective voice and standard-setting but remain constrained by institutional power; coordinate legitimate measurement practices while enabling institutional control
 *   - Audit Apparatus: Institutional actor (institutional/arbitrage) — maintains compliance systems and audit rituals; sees own process as degraded but continues through inertia
 *   - Measurement Reform Coalition: Organized agents (organized/constrained) — civil society, research institutions, and practitioners building alternative legitimacy mechanisms; see sunset pathway through distributed accountability
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing false precision as intrinsic to bureaucratic governance rather than contingent institutional choice
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(institutional_legitimacy_through_false_precision, 0.52).
domain_priors:suppression_score(institutional_legitimacy_through_false_precision, 0.58).
domain_priors:theater_ratio(institutional_legitimacy_through_false_precision, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(institutional_legitimacy_through_false_precision, extractiveness, 0.52).
narrative_ontology:constraint_metric(institutional_legitimacy_through_false_precision, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(institutional_legitimacy_through_false_precision, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(institutional_legitimacy_through_false_precision, tangled_rope).
narrative_ontology:human_readable(institutional_legitimacy_through_false_precision, "Institutional Legitimacy Through False Precision").
narrative_ontology:topic_domain(institutional_legitimacy_through_false_precision, "governance/institutional_dynamics").

domain_priors:requires_active_enforcement(institutional_legitimacy_through_false_precision).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(institutional_legitimacy_through_false_precision, institutional_management).
narrative_ontology:constraint_beneficiary(institutional_legitimacy_through_false_precision, executive_decision_makers).
narrative_ontology:constraint_victim(institutional_legitimacy_through_false_precision, field_practitioners).
narrative_ontology:constraint_victim(institutional_legitimacy_through_false_precision, epistemic_accountability).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: FIELD PRACTITIONER (SNARE) — Frontline workers and professionals cannot exit the false precision regime without career termination. Required to document and justify decisions using metrics that misrepresent ground-truth complexity. Trapped between institutional mandates for precise measurement and experiential knowledge that precision is illusory. Maximum extraction — bear full cost of institutional legitimacy theater while lacking voice in metric design.
constraint_indexing:constraint_classification(institutional_legitimacy_through_false_precision, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: PROFESSIONAL ASSOCIATION (TANGLED ROPE) — Organized practitioners have some agency through collective voice but remain constrained by institutional power differentials. Genuine coordination function: standardizing measurement practices enables cross-institutional comparison. But also extractive: standardized metrics lock practitioners into false precision frameworks and enable surveillance of professional discretion. Both functions present simultaneously.
constraint_indexing:constraint_classification(institutional_legitimacy_through_false_precision, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: INSTITUTIONAL MANAGEMENT (ROPE) — Experiences false precision as a coordination solution to the agency problem: quantified metrics reduce ambiguity in performance evaluation and enable delegated authority without constant oversight. Genuine coordination benefit: clear metrics allow decentralized decision-making. Net beneficiary position with exit optionality — can shift metric regimes if pressured, but maintains underlying authority structure through new precision claims.
constraint_indexing:constraint_classification(institutional_legitimacy_through_false_precision, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: THE AUDIT APPARATUS (PITON) — Audit and compliance systems maintain the false precision regime through institutional inertia despite acknowledged dysfunction. Theater ratio dominates: auditors perform verification rituals that create appearance of accountability while systematically missing actual performance variation. The apparatus sees itself as degraded — compliance specialists report that metrics systematize what should be professional judgment — but persists because alternatives require surrendering institutional control.
constraint_indexing:constraint_classification(institutional_legitimacy_through_false_precision, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(continental))).

% PERSPECTIVE 5: MEASUREMENT REFORM COALITION (SCAFFOLD) — Civil society, research institutions, and practitioner groups are building alternative legitimacy mechanisms: narrative evaluation, mixed-method assessment, outcome bundling with qualitative explanation. These approaches have genuine sunset logic — as transparency mechanisms mature and distributed legitimacy sources proliferate, reliance on single-metric false precision declines. Constrained by institutional path dependency but organized enough to perceive exit pathway.
constraint_indexing:constraint_classification(institutional_legitimacy_through_false_precision, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, organizations face an intrinsic tradeoff between authentic performance and measurable simplification; some loss of fidelity in measurement is inherent to scaled coordination. This view naturalizes false precision as an unavoidable feature of bureaucratic governance. However, this perspective risks mislabeling contingent institutional choices as natural law — the constraint is not inevitable but enforced through authority structures and epistemic closure.
constraint_indexing:constraint_classification(institutional_legitimacy_through_false_precision, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(institutional_legitimacy_through_false_precision_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(institutional_legitimacy_through_false_precision, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(institutional_legitimacy_through_false_precision, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(institutional_legitimacy_through_false_precision, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(institutional_legitimacy_through_false_precision, TR),
    TR >= 0.70.

:- end_tests(institutional_legitimacy_through_false_precision_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The constraint extracts from practitioners through requirement to justify decisions using metrics that both they and institutional actors know misrepresent reality. The extraction is not total because genuine coordination benefits exist — metrics do enable scaled delegation and cross-case comparison. But the asymmetry is real: management benefits from appearance of control; practitioners bear cost of legitimacy theater. Theater ratio (0.68) reflects that institutional precision claims are substantially performative. The metrics create appearance of accountability while systematically obscuring actual performance variation, discretion, and context-specifence. Suppression (0.58): Moderate-high. Practitioners face significant barriers to exit or resistance: career damage for refusing to use institutional metrics, surveillance through data systems, professional standardization that makes alternative practices illegible. But suppression is not total — some practitioners maintain parallel documentation, some professional communities develop counter-metrics, some institutions experiment with alternatives. The measurement trajectory shows both theater and extractiveness rising over the interval as metric systems mature and become institutionalized.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates how indexical classification reveals divergent institutional experiences of the same structural phenomenon. Management and practitioners perceive fundamentally different constraints: management sees a coordination mechanism solving the delegation problem (rope); practitioners see pure extraction mechanism hiding behind legitimacy claims (snare). The piton perspective reveals that institutional actors know the metrics are degraded but maintain them anyway. The professional association navigates both functions: genuine coordination plus institutional lock-in. The reform coalition sees a temporary feature being displaced by alternative accountability mechanisms. The natural law perspective naturalizes what is actually contingent institutional choice — the engine's false summit detector flags this as mislabeling of institutional arrangement as natural law.
 *
 * DIRECTIONALITY LOGIC:
 *   Institutional management holds beneficiary position (d ≈ 0.15-0.20): they benefit from metric legitimacy and have arbitrage exit options (can shift metrics, can exit to different governance regimes). Field practitioners hold victim position (d ≈ 0.90-0.95): they bear cost of false precision requirement and face trapped exit options (career termination risk, professional surveillance). Professional associations hold mixed position: they derive d from beneficiary status (standardized metrics enable professional coordination) partially offset by victim status (standardization locks in false precision). The derivation chain produces low chi for management, high chi for practitioners, moderate chi for professional associations. The piton classification derives from theater ratio exceeding 0.70 gate threshold while extractiveness remains moderate, indicating institutional inertia of degraded ritual rather than active new extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by demonstrating that institutional legitimacy through false precision is NOT a natural law masquerading as coordination (false mountain), but rather a genuine tangled_rope with real coordination benefit alongside real extraction. The genuine coordination benefit: organizations do face authentic delegation problems at scale, and metrics provide one solution to those problems. The genuine extraction: practitioners bear asymmetric cost of precision theater, face suppression of alternative frameworks, and lack voice in metric design. The mandatrophy is resolved by acknowledging both: metrics serve a real institutional function AND extract from practitioners AND are increasingly performative (theater rising). The constraint is not mislabeled coordination (rope) or pure extraction (snare) but legitimately hybrid. The natural law perspective is the false mount — precision requirements are not intrinsic to scaled governance but contingent on institutional choice to use quantitative metrics as legitimacy mechanism rather than mixed-method or narrative approaches.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    measurability_epistemology,
    'Is the gap between metric and reality a structural feature of measurement or an artifact of institutional metric selection?',
    'Comparison of practitioner-designed metrics vs management-imposed metrics; analysis of measurement frameworks that preserve qualitative nuance while enabling cross-case comparison',
    'If structural: false precision is unavoidable cost of coordination (mountain classification vindicated). If institutional choice: false precision is enforced ignorance maintained through authority (snare classification confirmed).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(measurability_epistemology, conceptual, 'Whether measurement gap is structural or institutional').

omega_variable(
    discretion_tradeoff,
    'Does false precision genuinely reduce principal-agent risk, or does it create the illusion of control while actual performance variance remains hidden?',
    'Comparative institutional analysis: organizations with high-precision metrics vs those with mixed-method evaluation; measurement of whether precision metrics correlate with actual outcome variation; case studies of metric substitution and goal displacement',
    'If precision reduces actual risk: management perspective on coordination benefit is justified (rope classification). If precision creates false assurance: extraction mechanism operates while institutions believe they are solving a real problem (snare/tangled_rope confirmed).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(discretion_tradeoff, empirical, 'Whether false precision serves genuine coordination or creates illusion of control').

omega_variable(
    professional_accountability_alternatives,
    'What alternative accountability mechanisms can scale beyond dyadic relationships (direct supervision) without collapsing into single-metric theater?',
    'Institutional experimentation: narrative evaluation, peer review, outcome bundling with qualitative explanation, participatory performance assessment; measurement of legitimacy maintenance across different metric regimes',
    'If scalable alternatives exist: scaffold perspective confirmed, sunset is real. If all scaled accountability requires metric compression: false precision becomes unavoidable feature, shifting classification toward rope/mountain.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(professional_accountability_alternatives, empirical, 'Existence of alternative accountability mechanisms at scale').

omega_variable(
    suppression_internalization,
    'Is practitioner compliance with false precision driven by structural barriers (career risk, surveillance) or by internalized belief that precision is necessary?',
    'Analysis of practitioner behavior when institutional surveillance is reduced; study of professional communities with different metric cultures; longitudinal tracking of metric acceptance during implementation phases vs mature institutional stages',
    'If structural barriers dominate: suppression is external (trapped exit option, snare classification). If internalized: practitioners have become identity-locked to metric frameworks despite structural mobility (identity_locked exit option, classification shift to rope from biographical perspective).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_internalization, empirical, 'Suppression mechanism: structural barriers vs internalized belief').

omega_variable(
    metric_gaming_feedback,
    'Does awareness of metric gaming (practitioners optimizing for measurement rather than outcome) drive metric reform or metric multiplication?',
    'Historical analysis of institutional metric evolution; tracking of whether Goodhart-detected failures lead to metric retirement or metric addition; study of institutional meta-awareness of gaming',
    'If Goodhart detection drives reform: institutional learning exists, extraction mechanism self-corrects (shorter effective timeline for snare/tangled_rope). If metric multiplication: gaming is systematically invisible to institutional management, extraction deepens (longer snare persistence).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(metric_gaming_feedback, empirical, 'Institutional response to metric gaming feedback').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(institutional_legitimacy_through_false_precision, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(iltp_tr_t0, institutional_legitimacy_through_false_precision, theater_ratio, 0, 0.48).
narrative_ontology:measurement(iltp_tr_t10, institutional_legitimacy_through_false_precision, theater_ratio, 10, 0.58).
narrative_ontology:measurement(iltp_tr_t20, institutional_legitimacy_through_false_precision, theater_ratio, 20, 0.68).

% Extraction over time
narrative_ontology:measurement(iltp_be_t0, institutional_legitimacy_through_false_precision, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(iltp_be_t10, institutional_legitimacy_through_false_precision, base_extractiveness, 10, 0.44).
narrative_ontology:measurement(iltp_be_t20, institutional_legitimacy_through_false_precision, base_extractiveness, 20, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(institutional_legitimacy_through_false_precision, enforcement_mechanism).
narrative_ontology:affects_constraint(institutional_legitimacy_through_false_precision, metric_substitution_goodhart_drift).
narrative_ontology:affects_constraint(institutional_legitimacy_through_false_precision, professional_discretion_suppression).
narrative_ontology:affects_constraint(institutional_legitimacy_through_false_precision, organizational_goal_displacement).

% DUAL FORMULATION NOTE:
% Institutional legitimacy through false precision decomposes into three structurally distinct constraints: (1) the coordination problem that metrics solve (resource_allocation type, lower ε), (2) the extraction mechanism that metrics enable (enforcement_mechanism type, medium-high ε), and (3) the performative degradation of metrics over time (institutional_inertia type, theater-dominant). This story focuses on the unified constraint as experienced by practitioners; the decomposed stories capture the institutional dynamics separately.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(institutional_legitimacy_through_false_precision, institutional, 0.18).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
