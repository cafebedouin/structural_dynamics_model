% ============================================================================
% CONSTRAINT STORY: public_sector_efficiency_erosion
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_public_sector_efficiency_erosion, []).

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
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: public_sector_efficiency_erosion
 *   human_readable: Public Sector Efficiency Erosion Through Metrics Gaming and Compliance Theater
 *   domain: political_economy/institutional_degradation
 *
 * SUMMARY:
 *   Public sector efficiency erosion through metrics-based performance
 *   management represents a structural constraint where ostensible
 *   coordination (uniform accountability, transparent measurement,
 *   performance incentives) has been layered with asymmetric extraction
 *   (compliance labor, career risk for non-compliance, benefit concentration
 *   among management). The constraint emerges from the New Public Management
 *   institutional framework, which institutionalized the principle that
 *   measurable outputs could drive efficiency and accountability. Initial
 *   implementation (t=0) captured genuine coordination benefits: metrics
 *   provided transparency and resource discipline. Over 15 years, gaming and
 *   compliance theater have accumulated (theater_ratio: 0.35→0.72), while
 *   extractiveness has nearly doubled (0.32→0.62). The constraint now
 *   exhibits all six DR types depending on observer position. Street-level
 *   bureaucrats experience it as a snare: trapped in compliance regimes that
 *   consume operational capacity with zero exit options. Senior leadership
 *   experiences it as pure coordination (rope): metrics solve principal-agent
 *   problems without costs borne by leadership. The analytical observer risks
 *   naturalizing this as an immutable iron law of bureaucracy, but the
 *   evidence suggests contingent institutional choices, not natural
 *   necessity. The constraint exhibits both genuine coordination function
 *   (sharing information upward, enabling resource allocation decisions) and
 *   substantial extraction (compliance labor, metric gaming incentives,
 *   theater maintenance), making tangled_rope the primary classification.
 *
 * KEY AGENTS:
 *   - Street-Level Bureaucrats (teachers, nurses, social workers): Victims (powerless/trapped) — bear full compliance burden with no exit; experience maximal extraction
 *   - Middle Managers: Secondary victims (moderate/constrained) — experience mixed coordination (aggregating local data) and extraction (responsibility without authority); could move but constrained by career dependency
 *   - Senior Leadership & Compliance Apparatus: Beneficiaries (institutional/arbitrage) — gain authority, budget allocation power, and consulting revenue from metrics regime; experience as pure coordination
 *   - Compliance Consultants & Audit Bodies: Beneficiaries (institutional/arbitrage) — extract consulting fees and institutional growth from compliance regime; direct financial interest in persistence
 *   - Service Users/Citizens: Secondary victims (moderate/constrained) — receive both coordinated service access (benefit) and degraded responsiveness due to gaming (cost); geographically and economically trapped
 *   - Public Sector Unions & Reform Coalition: Organized agents (organized/mobile) — see metrics regime as temporary problem with clear sunset into outcome-based or trust-based governance
 *   - Political Leadership: Beneficiaries (institutional/arbitrage) — use metrics for accountability theater to electorate; extract accountability credibility without operational accountability
 *   - Analytical Observer: Civilizational context (analytical/analytical) — risks naturalizing NPM as inherent to all bureaucracy, missing contingent institutional origins
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(public_sector_efficiency_erosion, 0.58).
domain_priors:suppression_score(public_sector_efficiency_erosion, 0.65).
domain_priors:theater_ratio(public_sector_efficiency_erosion, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(public_sector_efficiency_erosion, extractiveness, 0.58).
narrative_ontology:constraint_metric(public_sector_efficiency_erosion, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(public_sector_efficiency_erosion, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(public_sector_efficiency_erosion, tangled_rope).
narrative_ontology:human_readable(public_sector_efficiency_erosion, "Public Sector Efficiency Erosion Through Metrics Gaming and Compliance Theater").
narrative_ontology:topic_domain(public_sector_efficiency_erosion, "political_economy/institutional_degradation").

domain_priors:requires_active_enforcement(public_sector_efficiency_erosion).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(public_sector_efficiency_erosion, senior_bureaucrats).
narrative_ontology:constraint_beneficiary(public_sector_efficiency_erosion, compliance_consultants).
narrative_ontology:constraint_beneficiary(public_sector_efficiency_erosion, political_leadership).
narrative_ontology:constraint_victim(public_sector_efficiency_erosion, frontline_service_providers).
narrative_ontology:constraint_victim(public_sector_efficiency_erosion, public_service_beneficiaries).
narrative_ontology:constraint_victim(public_sector_efficiency_erosion, operational_capacity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: STREET-LEVEL BUREAUCRAT (SNARE) — Frontline workers (teachers, healthcare providers, social workers) face irreducible extraction through reporting mandates that consume 30-40% of operational time. Cannot exit (employment dependency, professional identity fusion) and cannot escape the metric regime (government-wide enforcement). Maximum experienced extraction with zero degrees of freedom.
constraint_indexing:constraint_classification(public_sector_efficiency_erosion, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: MIDDLE MANAGER (TANGLED ROPE) — Caught between frontline staff and senior leadership. Experiences genuine coordination function (aggregating local data, communicating upward) but faces asymmetric extraction through responsibility without authority. Can move laterally within government but constrained by career path dependency. Mixed position: some coordination benefit, significant extraction cost.
constraint_indexing:constraint_classification(public_sector_efficiency_erosion, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: SENIOR LEADERSHIP & COMPLIANCE APPARATUS (ROPE) — Executives, audit bodies, and compliance consultants benefit from the metrics regime through expanded authority, budget allocation power, and consulting fees. Experience the constraint as coordination: enforcing uniform metrics solves coordination problems across fragmented agencies. Net beneficiaries with exit options (can restructure or abandon metrics at will). Low experienced extraction.
constraint_indexing:constraint_classification(public_sector_efficiency_erosion, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: REFORM COALITION (SCAFFOLD) — Public sector unions, service quality advocates, and performance measurement reformers see this as a temporary problem with a sunset: outcome-based budgeting, participatory metrics design, and trust-based governance models are emerging as alternatives to command-and-control compliance theater. Organized agents with clear exit pathway into alternative models. Has sunset clause built into its analysis.
constraint_indexing:constraint_classification(public_sector_efficiency_erosion, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: NEW PUBLIC MANAGEMENT FRAMEWORK (PITON) — NPM-derived metrics regimes (cost per output, activity-based performance indicators) were designed to solve principal-agent problems and improve accountability. But the framework persists despite systematic evidence of theater and dysfunction: compliance reporting dominates over actual service delivery; metrics are gamed; perverse incentives multiply. The institutional apparatus maintains NPM ritual through inertia and because alternatives require political capital, not because it delivers on its functional promise. Theater ratio (0.68) reflects the performative nature of compliance measurement.
constraint_indexing:constraint_classification(public_sector_efficiency_erosion, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: SERVICE USER (TANGLED ROPE) — Citizens receiving public services experience both coordination benefit (standardized access, transparent processes) and extraction (reduced responsiveness, game-playing around metrics rather than genuine needs). Constrained by geographic and economic dependency on public services. No formal exit but some mobility (private alternatives for wealthy citizens). Moderate experienced extraction.
constraint_indexing:constraint_classification(public_sector_efficiency_erosion, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (MOUNTAIN?) — From a civilizational view, this might appear as an immutable iron law of large-scale bureaucracy: any measurable metric becomes gamed; any system with distant principals and local agents creates monitoring costs that exceed benefit; Goodhart's Law is inherent to governance. However, the structural data contradicts mountain classification — the constraint is contingent on specific institutional choices (NPM adoption, centralized metrics design, compliance enforcement structure), not inherent to public administration. Historical examples (pre-NPM public sectors, trust-based governance models) show alternatives. Engine will flag this as false summit: naturalization of institutional choices as immutable laws.
constraint_indexing:constraint_classification(public_sector_efficiency_erosion, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(public_sector_efficiency_erosion_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(public_sector_efficiency_erosion, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(public_sector_efficiency_erosion, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(public_sector_efficiency_erosion, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(public_sector_efficiency_erosion, TR),
    TR >= 0.70.

:- end_tests(public_sector_efficiency_erosion_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): The constraint extracts in multiple forms: (1) compliance labor (~30-40% of frontline time diverted from service delivery), (2) career risk for non-compliance (performance bonuses, job security tied to metrics), (3) distorted incentives (teachers teaching to tests, nurses gaming wait-time metrics, social workers reducing case complexity to improve metrics). Extractiveness is moderate-high but not maximum because some workers benefit from clearer role definitions and some genuine operational coordination occurs. Suppression (0.65): Substantial. Frontline workers cannot exit employment easily (professional identity, financial dependency); cannot refuse compliance (government-wide enforcement); face reputational and career costs for whistleblowing; alternative public sectors in same country use similar regimes (no geographic exit); private sector alternatives only available to privileged populations. However, suppression is not total — some workers do exit (brain drain to private sector, early retirement), and informal non-compliance persists. Theater ratio (0.68): High and increasing. Compliance reporting has become substantially decoupled from actual service delivery. Performance indicators are increasingly gamed: wait times artificially reduced through definitional changes, quality metrics improved through selecting easier cases, activity metrics inflated through reporting manipulation. The institutional apparatus now dedicates significant resources to compliance theater (audit, reporting, monitoring of metrics) with minimal feedback to actual service improvement.
 *
 * PERSPECTIVAL GAP:
 *   The constraint exhibits maximum perspectival divergence. Senior leadership sees rope (coordination solution), street-level workers see snare (pure extraction), middle managers see tangled rope (mixed), service users see tangled rope (both benefit and cost), and the analytical observer risks seeing mountain (immutable law). This gap reveals that the constraint's classification is observer-dependent in the strict sense: the same institutional structure produces qualitatively different experiences based on structural position. The gap is not resolved by 'objective' measurement; it is explained by directionality (d) computation: beneficiaries with exit options (d~0.15) experience low effective extraction; victims without exit (d~0.95) experience high extraction. The perspectival variance is itself diagnostic evidence of tangled_rope rather than pure coordination.
 *
 * DIRECTIONALITY LOGIC:
 *   Each agent's experienced extractiveness follows from their structural position relative to the constraint. Senior leadership and compliance apparatus are beneficiaries with high exit options (arbitrage): they can reshape, abandon, or restructure the metrics regime unilaterally. Derived d~0.20 (beneficiary + arbitrage exit) produces f(d)~0.02, yielding low effective extraction χ. They experience the constraint as pure coordination (rope). Street-level bureaucrats are victims with no exit (trapped): they face compliance mandates, cannot refuse, cannot resign without economic catastrophe, and carry professional identity that binds them to public service. Derived d~0.95 (victim + trapped exit) produces f(d)~1.42, yielding high effective extraction χ. They experience snare. Middle managers are victims with partial exit (constrained): they can move laterally within government or to private sector but face career path costs. Derived d~0.68 produces f(d)~0.88, yielding moderate-high χ, consistent with tangled_rope. This structure explains the perspectival gap: the same constraint has qualitatively different extraction profiles depending on exit options.
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLUTION: This constraint avoids mandatrophy through the tangled_rope classification. The temptation is to classify as pure coordination (rope) because genuine coordination function exists (information aggregation, accountability structures, resource discipline). But the presence of beneficiaries, extraction mechanism (compliance labor diversion, gaming incentives, theater maintenance), and asymmetry (leadership gains authority while frontline workers lose autonomy) triggers the tangled_rope gate. Tangled rope requires three conditions: (1) genuine coordination function — verified, metrics do enable resource allocation decisions; (2) asymmetric extraction — verified, benefits concentrate among leadership while costs concentrate among frontline workers; (3) active enforcement — verified, compliance regimes require dedicated audit and monitoring apparatus. All three conditions are met, confirming tangled_rope. The false classification would be pure rope (missing the extraction) or snare (missing the coordination). The constraint is genuinely both: a coordination mechanism that has been inverted into an extraction device through institutional capture and metric gaming. The theater accumulation (0.35→0.72) reveals the involution: as the coordination function degrades (metrics become gamed rather than informative), the extraction mechanism persists and intensifies through theater maintenance (dedicated resources to monitoring and enforcing compliance).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    metrics_gaming_threshold,
    'What proportion of reported performance reflects actual service delivery versus metric manipulation and gaming?',
    'Comparison of reported metrics against independent outcome measures; mystery shopper audits; analysis of actual vs reported time allocation; longitudinal tracking of discrepancies between metrics and service user satisfaction',
    'If >60% gaming: constraint is pure extraction (Snare from more perspectives). If 30-60% gaming: mixed extraction-coordination (Tangled Rope confirmed). If <30% gaming: metrics are mostly functional (Rope from more perspectives).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(metrics_gaming_threshold, empirical, 'Proportion of reported performance that reflects actual service delivery').

omega_variable(
    compliance_time_allocation,
    'What is the true cost of compliance reporting in terms of operational time diverted from service delivery?',
    'Time-use studies of frontline workers; before/after analysis of service metrics post-compliance mandate implementation; comparison across jurisdictions with different compliance regimes',
    'If >40% time: severe extraction mechanism confirmed (suppression ≥0.65 justified). If 20-40% time: moderate extraction (extractiveness >0.50 justified). If <20% time: theater ratio may be overstated.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(compliance_time_allocation, empirical, 'Percentage of frontline worker time consumed by compliance and reporting').

omega_variable(
    alternative_governance_feasibility,
    'Are trust-based, outcome-focused, or participatory governance models actually viable alternatives at scale, or are they subject to their own efficiency degradation dynamics?',
    'Comparative analysis of jurisdictions using alternative metrics frameworks (outcome budgeting, participatory measurement, trust-based governance); longitudinal tracking of gaming and theater emergence in alternative models',
    'If alternatives remain low-theater: scaffold sunset is real and extractive constraint can be dissolved. If alternatives degrade similarly: the problem may be inherent to scale/principal-agent dynamics (closer to mountain), suggesting the tangled_rope/snare classifications need revision.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(alternative_governance_feasibility, empirical, 'Whether alternative governance models avoid efficiency erosion').

omega_variable(
    senior_leadership_capture,
    'To what extent is the persistence of NPM metrics regime a function of senior leadership being insulated from its extraction costs, versus rational institutional design?',
    'Analysis of career outcomes for senior leaders under NPM (promotions, salary, stress levels) versus frontline workers; historical comparison of when NPM was introduced versus when it became extractive; identification of when leadership knowledge of theater emerged',
    'If primarily capture: tangled_rope classification confirmed — beneficiaries knowingly maintain extraction. If primarily ignorance: constraint may be re-classified as scaffold (solvable through information) rather than tangled_rope (structural hybrid).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(senior_leadership_capture, empirical, 'Whether NPM persistence reflects leadership capture or genuine belief in framework').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(public_sector_efficiency_erosion, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(psee_tr_t0, public_sector_efficiency_erosion, theater_ratio, 0, 0.35).
narrative_ontology:measurement(psee_tr_t5, public_sector_efficiency_erosion, theater_ratio, 5, 0.52).
narrative_ontology:measurement(psee_tr_t10, public_sector_efficiency_erosion, theater_ratio, 10, 0.68).
narrative_ontology:measurement(psee_tr_t15, public_sector_efficiency_erosion, theater_ratio, 15, 0.72).

% Extraction over time
narrative_ontology:measurement(psee_be_t0, public_sector_efficiency_erosion, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(psee_be_t5, public_sector_efficiency_erosion, base_extractiveness, 5, 0.45).
narrative_ontology:measurement(psee_be_t10, public_sector_efficiency_erosion, base_extractiveness, 10, 0.58).
narrative_ontology:measurement(psee_be_t15, public_sector_efficiency_erosion, base_extractiveness, 15, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(public_sector_efficiency_erosion, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(public_sector_efficiency_erosion, 0.18).
narrative_ontology:affects_constraint(public_sector_efficiency_erosion, teacher_labor_degradation).
narrative_ontology:affects_constraint(public_sector_efficiency_erosion, healthcare_rationing_by_metrics).
narrative_ontology:affects_constraint(public_sector_efficiency_erosion, social_service_gaming).

% DUAL FORMULATION NOTE:
% Public sector efficiency erosion is the primary constraint affecting downstream sector-specific manifestations. The same NPM metrics regime produces distinct extractive dynamics in education (teaching to tests), healthcare (prioritizing quantifiable over complex cases), and social services (caseload metrics over outcome quality). Each sector has its own constraint story reflecting sector-specific gaming mechanisms, but all are downstream of the general metrics enforcement apparatus. Network decomposition: general efficiency erosion → sector-specific gaming constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(public_sector_efficiency_erosion, institutional, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
