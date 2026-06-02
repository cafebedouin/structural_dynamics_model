% ============================================================================
% CONSTRAINT STORY: bureaucratic_legibility_collapse
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-27
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
 *   human_readable: Legibility Collapse in Bureaucratic Systems
 *   domain: political/organizational/informational
 *
 * SUMMARY:
 *   Bureaucratic legibility collapse occurs when an institution's measurement
 *   and control systems become so decoupled from ground reality that the
 *   institution's corrective actions produce perverse outcomes —
 *   counterintuitively worsening the conditions they claim to address. This
 *   constraint represents a hybrid coordination-extraction mechanism: the
 *   measurement bureaucracy genuinely solves the coordination problem of
 *   aggregating information across large, decentralized organizations,
 *   enabling comparative learning and resource allocation across
 *   jurisdictions. But the measurement system simultaneously operates as an
 *   extraction mechanism: it concentrates defining power in the hands of
 *   metric gatekeepers (central authorities, auditors, performance managers)
 *   while suppressing the tacit knowledge and situational judgment of
 *   street-level practitioners and field operations. The constraint is
 *   neither pure coordination (Rope) nor pure extraction (Snare), but a
 *   structured hybrid where the coordination function is real but
 *   asymmetrically distributed — benefits flow to the measurement authority
 *   (which gains visibility and control), while costs are concentrated on
 *   practitioners and service recipients (who bear the burden of metric
 *   misalignment without power to reshape the metrics). Over the measurement
 *   interval, both theater_ratio and suppression_requirement rise as
 *   institutions layer increasingly elaborate metrics, counter-metrics, and
 *   audit apparatus. Extractiveness accumulates as metric gaming becomes
 *   endemic — practitioners learn to optimize for reported metrics rather
 *   than underlying outcomes, the measurement system's signal-to-noise ratio
 *   degrades, and the central authority responds by intensifying measurement
 *   rather than questioning the regime. This accumulation pattern is
 *   diagnostic of Tangled Rope drifting toward Snare: genuine coordination
 *   benefit is eroded by extraction mechanisms that eventually overwhelm the
 *   system's functional value.
 *
 * KEY AGENTS:
 *   - Street-Level Bureaucrat: Primary victim (powerless/trapped) — bears suppression of judgment and labor extraction; experiences pure snare with zero coordination benefit
 *   - Service Recipient: Primary victim (powerless/trapped) — bears costs of metric misalignment (worse actual outcomes); structurally dependent on institution
 *   - Central Measurement Authority: Primary beneficiary (institutional/arbitrage) — captures defining power over metrics; experiences constraint as pure coordination; can transition to new measurement regimes
 *   - Professional Association: Secondary beneficiary (organized/constrained) — coordinates across practitioners but must negotiate metric definitions; dependent on institutional legitimacy
 *   - Measurement-Compliant Organization: Secondary beneficiary (powerful/mobile) — gains coordination benefit through benchmarking but can exploit metric gaming; sufficiently resourced to navigate metric environment
 *   - Audit Apparatus: Institutional actor (institutional/arbitrage) — maintains compliance verification; theater-driven persistence; has vested interest in ongoing measurement regimes
 *   - Analytical Observer: Observes potential false summit — risks naturalizing contingent institutional choice as inherent to coordination
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(bureaucratic_legibility_collapse, 0.58).
domain_priors:suppression_score(bureaucratic_legibility_collapse, 0.62).
domain_priors:theater_ratio(bureaucratic_legibility_collapse, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(bureaucratic_legibility_collapse, extractiveness, 0.58).
narrative_ontology:constraint_metric(bureaucratic_legibility_collapse, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(bureaucratic_legibility_collapse, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(bureaucratic_legibility_collapse, tangled_rope).
narrative_ontology:human_readable(bureaucratic_legibility_collapse, "Legibility Collapse in Bureaucratic Systems").
narrative_ontology:topic_domain(bureaucratic_legibility_collapse, "political/organizational/informational").

domain_priors:requires_active_enforcement(bureaucratic_legibility_collapse).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(bureaucratic_legibility_collapse, central_measurement_authority).
narrative_ontology:constraint_beneficiary(bureaucratic_legibility_collapse, performance_metric_gatekeepers).
narrative_ontology:constraint_victim(bureaucratic_legibility_collapse, field_practitioners).
narrative_ontology:constraint_victim(bureaucratic_legibility_collapse, local_system_optimization).
narrative_ontology:constraint_victim(bureaucratic_legibility_collapse, unquantifiable_outcomes).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: STREET-LEVEL BUREAUCRAT (SNARE) — Trapped between institutional measurement requirements and ground reality. Cannot exit (career path depends on employment), cannot reshape the metrics (imposed from above), experiences pure extraction of labor and judgment credibility. The bureaucrat's tacit knowledge and situational discretion are systematically suppressed in favor of metric compliance. Maximum experienced extraction — no coordination benefit, only constraint.
constraint_indexing:constraint_classification(bureaucratic_legibility_collapse, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: SERVICE RECIPIENT (SNARE) — Trapped in system that optimizes for institutional metrics rather than actual outcomes. Cannot exit the institution's jurisdiction (legal mandate, geographic necessity, dependency). Bears costs of perverse outcomes (school taught to the test teaches poorly; hospital gaming wait-time metrics reduces emergency capacity; poverty census optimized for reporting reduces aid targeting). Zero exit options, maximum extraction of actual welfare for institutional performance signals.
constraint_indexing:constraint_classification(bureaucratic_legibility_collapse, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 3: PROFESSIONAL ASSOCIATION (TANGLED ROPE) — Professional bodies (social work associations, teachers unions, medical societies) experience both coordination and extraction. Genuine coordination: standardized measurement enables comparative learning and best-practice diffusion across organizations. Asymmetric extraction: the association must negotiate metric definitions while practitioners bear the cost of misalignment. Exit constrained by political dependency on institutional legitimacy. Some agency (can push back on metrics) but not arbitrage (survival depends on institutional system).
constraint_indexing:constraint_classification(bureaucratic_legibility_collapse, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: CENTRAL MEASUREMENT AUTHORITY (ROPE) — Genuine coordination function: aggregating information across decentralized systems solves the acute problem of central decision-making without local knowledge. The measurement authority experiences the constraint as pure coordination — enabling information flow, system visibility, comparative performance across jurisdictions. Beneficiary through metric gatekeeping (controls what counts as success), arbitrage exit (can relocate metric definitions, can transition to new measurement regimes). Low experienced extraction because the coordination function is real and the authority maintains defining power.
constraint_indexing:constraint_classification(bureaucratic_legibility_collapse, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: AUDIT APPARATUS (PITON) — Institutional machinery for verifying compliance with measurement regimes. Theater ratio high: audit generates compliance rituals that may be decoupled from actual institutional effectiveness. The audit system persists through formal requirement (accountability theater) despite degraded correlation with real outcomes. Institutional inertia maintains the apparatus — replaced not by deleting audits but by layering additional measurement and counter-measurement systems. Piton classification reflects that the audit function has lost primary purposiveness; it is maintained by institutional obligation and theater rather than actual corrective value.
constraint_indexing:constraint_classification(bureaucratic_legibility_collapse, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: MEASUREMENT-COMPLIANT ORGANIZATION (TANGLED ROPE) — Large institutions (hospital systems, school districts, welfare agencies) that can navigate the metric environment benefit from coordination (shared benchmarking, peer comparison, resource allocation logic) while bearing asymmetric extraction through metric gaming. Powerful enough to redesign internal structures around metrics; mobile enough to allocate talent toward metric optimization. Experience is hybrid: some genuine coordination benefit (learning from comparative performance), significant extraction through metric distortion (resources diverted to score-gaming rather than service). Moderate experienced extraction because the organization has agency and can profit from metric arbitrage.
constraint_indexing:constraint_classification(bureaucratic_legibility_collapse, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(continental))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational vantage, legibility collapse might appear inherent to scaled coordination: any system attempting to monitor decentralized actors inevitably generates measurement lag, metric distortion, and information asymmetry — these are invariant to organizational design. The observer risks naturalizing what is actually a contingent institutional arrangement (the choice to optimize for quantifiable metrics, the decision to suppress practitioner discretion, the structural lock-in of measurement regimes). The engine will identify this as a false summit revealing that 'inevitable coordination cost' naturalizes what are actually modifiable institutional choices.
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
 *   Extractiveness (0.58): Moderate-high. The measurement bureaucracy extracts defining power (what counts as success), labor compliance (practitioners work to metrics), and system optimization capability (resources diverted to metric compliance rather than outcome optimization). This is not total extraction because genuine coordination benefit exists — the authority does enable comparative learning and resource allocation that would be difficult without standardized metrics. The 0.58 value reflects that extraction has become substantial but coordination function is not yet fully displaced. The upward trajectory (0.32 → 0.58 over interval) indicates accumulating extraction as metric gaming proliferates and theater rises. Suppression (0.62): Moderate-high and rising. Suppression operates through multiple channels: formal accountability (metric compliance overrides judgment), career risk (non-compliance threatens employment), information asymmetry (practitioners' tacit knowledge is systematically excluded from institutional decision-making), and systematic devaluation of unquantifiable outcomes (care quality, relationship depth, situated wisdom). Suppression is not total because practitioner networks and informal knowledge exchange persist, but the formal institutional structure systematically marginalizes these alternatives. Theater ratio (0.68): High and rising. Theater accumulates as institutions layer metrics (initial performance measures → audit systems → meta-audits → compliance theater). The rising trajectory reflects that measurement apparatus has become increasingly performative: audit generates compliance rituals (documented procedures, checkbox compliance) that may be decoupled from actual institutional effectiveness. Much institutional activity is devoted to metric servicing rather than outcome production.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates structural disagreement across observer positions. The central measurement authority (institutional/arbitrage) perceives pure coordination — the bureaucracy genuinely solves the problem of aggregating information without local knowledge. The street-level bureaucrat (powerless/trapped) perceives pure extraction — their judgment is systematically suppressed, their labor is extracted without compensation, they bear costs of metric misalignment. The measurement-compliant organization (powerful/mobile) perceives hybrid structure with significant arbitrage potential — they can profit from metric gaming and comparative advantage in navigating the system. The professional association (organized/constrained) perceives mixed benefit and cost — coordination through peer benchmarking but extraction through negotiation asymmetry. The service recipient perceives extraction — outcomes worsen as institution optimizes for metrics. The audit apparatus perceives necessary accountability (though with degraded function). The civilizational analytical observer risks perceiving a natural law (coordination inevitably generates measurement lag and distortion) when the structural evidence points toward institutional choice and beneficiary interest. The perspectival gap reveals that 'measurement necessity' naturalizes what is actually a power asymmetry: those who define metrics benefit; those who must comply bear costs.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values reflect each agent's structural position relative to the extraction flow. The central measurement authority (beneficiary + arbitrage) derives d ≈ 0.05, producing negative effective extraction — the authority experiences the system as value-creating coordination. Street-level bureaucrats (victims + trapped) derive d ≈ 0.95, producing maximum experienced extraction — they have no exit and bear full cost. Service recipients (victims + trapped) similarly derive high d. The measurement-compliant organization (beneficiary + mobile) derives d ≈ 0.35, experiencing moderate extraction with arbitrage potential. Professional associations (organized + constrained) derive d ≈ 0.55, experiencing moderate extraction with some negotiation leverage. The audit apparatus (institutional + arbitrage) derives low d despite being arguably a victim of institutional inertia — because the apparatus itself benefits from metric proliferation (more metrics = more audit activity). Each d value produces a corresponding f(d) through the sigmoid, determining how the base extractiveness (0.58) is experienced. The power asymmetry is encoded in the directionality distribution: beneficiaries and high-power actors experience low or negative chi; victims and trapped actors experience high chi; moderates experience intermediate values. No single chi value is 'the' experienced extractiveness — chi varies across observers, revealing the extraction asymmetry itself.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint avoids mandatrophy by recognizing that Tangled Rope requires BOTH genuine coordination function AND asymmetric extraction. Both components are present: (1) Genuine coordination: the bureaucracy solves the acute problem of aggregating information across decentralized systems, enabling comparative learning and resource allocation that would be impossible otherwise. Practitioners and recipients do benefit from some outcomes of institutional coordination (resource prioritization, equity benchmarking, system-level learning). (2) Asymmetric extraction: the coordination function is structured to concentrate defining power in the measurement authority while suppressing practitioner judgment and field wisdom. Beneficiaries (measurement authority) gain defining power; victims (practitioners and recipients) bear costs of metric misalignment without power to reshape metrics. The upward trajectory of extractiveness and theater ratio indicates that over time, extraction mechanisms are overwhelming coordination function — the constraint is drifting toward Snare. But at the current state (ε=0.58, requiring_active_enforcement=true, beneficiaries + victims declared), Tangled Rope is appropriate. The false summit marker in the analytical perspective warns that the constraint risks naturalizing institutional choice as inherent law, but the structural data (beneficiary declaration, victim cost, enforcement requirement) prevents misclassification as genuine mountain.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    metric_distortion_threshold,
    'At what level of metric gaming does the measurement system cease to provide useful information about system state?',
    'Correlation analysis between reported metrics and independent outcome measures; historical cases where metric collapse triggered institutional crisis (e.g., well-known gaming cases in healthcare, education, welfare)',
    'If threshold low (< 30% gaming): measurement system retains value even with distortion, remains closer to Rope classification. If threshold high (> 60% gaming): measurement system becomes pure theater, system slides toward Snare. Current evidence suggests threshold near 45-55%, placing legibility collapse in tangled_rope/snare boundary.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(metric_distortion_threshold, empirical, 'Metric distortion threshold for information collapse').

omega_variable(
    practitioner_discretion_recovery,
    'Can local practitioners maintain genuine judgment despite institutional metrics, or do metrics structurally eliminate discretion?',
    'Ethnographic study of compliance patterns; observation of whether practitioners with professional autonomy (doctors, teachers, social workers) retain decision-making authority or are functionally constrained by metric accountability',
    'If discretion recoverable: constraint is primarily extractive (suppression mechanism), closer to Snare. If discretion structurally eliminated: constraint is coordination failure (measurement system breaks down), moves toward dysfunctional Tangled Rope or Mountain (inherent). Current evidence suggests partial recovery with high resource cost (practitioners ''work around'' metrics).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(practitioner_discretion_recovery, empirical, 'Whether practitioner discretion survives metric accountability').

omega_variable(
    alternative_aggregation_sufficiency,
    'Do non-metric aggregation methods (peer networks, qualitative reporting, participatory assessment) provide decision-useful information at comparable cost to formal metrics?',
    'Comparative organizational studies; cases where institutions abandoned or supplemented metrics with alternative information systems (e.g., participatory budgeting, peer learning networks, narrative outcome tracking)',
    'If alternative methods sufficient: legibility collapse is institutional choice, not structural necessity — constraint moves toward pure extraction (Snare). If alternatives insufficient: measurement bureaucracy may be necessary coordination mechanism despite distortion — constraint remains Tangled Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_aggregation_sufficiency, empirical, 'Whether alternatives to metrics provide decision-useful aggregation').

omega_variable(
    false_summit_institutional_naturalization,
    'Is legibility collapse an inherent property of scaled coordination, or a modifiable institutional choice that benefits measurement authorities?',
    'Historical comparison of institutions with metric-heavy vs practitioner-discretion-preserving designs; analysis of which designs remain stable without metric theater; examination of how metric necessity rhetoric serves institutional beneficiary interests',
    'If inherent (true mountain): correction requires accepting information loss or system scaling limits. If institutional choice (false summit): correction requires reorganizing incentive structures and measurement authority power. Current evidence strongly suggests false summit — metric optimization serves institutional beneficiary interests (gatekeepers) more than coordination needs.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(false_summit_institutional_naturalization, conceptual, 'Natural law vs institutional choice in legibility collapse').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(bureaucratic_legibility_collapse, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(blegh_tr_t0, bureaucratic_legibility_collapse, theater_ratio, 0, 0.42).
narrative_ontology:measurement(blegh_tr_t10, bureaucratic_legibility_collapse, theater_ratio, 10, 0.58).
narrative_ontology:measurement(blegh_tr_t20, bureaucratic_legibility_collapse, theater_ratio, 20, 0.68).

% Extraction over time
narrative_ontology:measurement(blegh_be_t0, bureaucratic_legibility_collapse, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(blegh_be_t10, bureaucratic_legibility_collapse, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(blegh_be_t20, bureaucratic_legibility_collapse, base_extractiveness, 20, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(blegh_su_t0, bureaucratic_legibility_collapse, suppression_requirement, 0, 0.48).
narrative_ontology:measurement(blegh_su_t10, bureaucratic_legibility_collapse, suppression_requirement, 10, 0.55).
narrative_ontology:measurement(blegh_su_t20, bureaucratic_legibility_collapse, suppression_requirement, 20, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(bureaucratic_legibility_collapse, resource_allocation).
narrative_ontology:affects_constraint(bureaucratic_legibility_collapse, metric_gaming_cascade).
narrative_ontology:affects_constraint(bureaucratic_legibility_collapse, practitioner_judgment_suppression).
narrative_ontology:affects_constraint(bureaucratic_legibility_collapse, audit_apparatus_inertia).

% DUAL FORMULATION NOTE:
% Bureaucratic legibility collapse is downstream of organizational scaling (the need to aggregate information across many decentralized units) but represents a distinct structural constraint. The upstream scaling problem has its own extractiveness reflecting coordination necessity; legibility collapse has its own extractiveness reflecting the specific institutional choices (metric gatekeeping, judgment suppression, measurement authority power) made in response to scaling. Decomposition recognizes that not all large organizations experience legibility collapse — some preserve practitioner discretion, some use alternative aggregation methods, some maintain transparency about metric limitations. Those differences reflect different constraint structures, not just different severity of the same constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(bureaucratic_legibility_collapse, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
