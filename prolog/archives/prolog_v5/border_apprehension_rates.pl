% ============================================================================
% CONSTRAINT STORY: border_apprehension_rates
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_border_apprehension_rates, []).

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
 *   constraint_id: border_apprehension_rates
 *   human_readable: Border Apprehension Rate Optimization Constraint
 *   domain: immigration_policy/border_enforcement
 *
 * SUMMARY:
 *   Border apprehension rate constraints embody a fundamental tension between
 *   coordination necessity and extractive optimization. Governments must
 *   process migrant flows at borders — this is a genuine coordination problem
 *   requiring standardized procedures, resource allocation, and performance
 *   measurement. However, the metric used to measure this function
 *   (apprehension rates) has increasingly substituted for the underlying
 *   objectives (security, humanitarian capacity, effective intake
 *   processing). The constraint exhibits genuine mixed coordination
 *   (enforcement agencies standardize procedures, allocate resources across
 *   jurisdictions) alongside asymmetric extraction (migrants and humanitarian
 *   systems bear costs disproportionately to benefits received). The theater
 *   ratio (0.68) reflects that apprehension numbers are reported as
 *   performance indicators demonstrating political commitment to enforcement,
 *   with limited correlation to actual security outcomes or humanitarian
 *   effectiveness. The extractiveness trajectory (0.42 → 0.58 over 15 years)
 *   indicates accumulation: initial coordination function has been layered
 *   with political performance pressure, metric gaming, and capacity
 *   extraction from humanitarian systems.
 *
 * KEY AGENTS:
 *   - Apprehended Migrants: Primary victims (powerless/trapped) — face physical confinement, legal barriers, information asymmetry, economic dependency during processing
 *   - Humanitarian Systems: Secondary victims (powerless/trapped) — NGOs, legal aid, medical services, shelter networks bear demand costs without service refusal option
 *   - Border Communities: Mixed (moderate/constrained) — gain security infrastructure and enforcement employment; bear strain on services, schools, healthcare
 *   - Immigration Enforcement Agency: Primary beneficiary (institutional/arbitrage) — apprehension metrics fund operations, staff positions, budget justification, career advancement
 *   - Political Leadership: Secondary beneficiary (institutional/arbitrage) — apprehension statistics provide measurable performance, electoral accountability narrative, visible enforcement credibility
 *   - Analytical Observer: System perspective (analytical/analytical) — sees drift from coordination function toward pure metric performance and theater maintenance
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(border_apprehension_rates, 0.58).
domain_priors:suppression_score(border_apprehension_rates, 0.65).
domain_priors:theater_ratio(border_apprehension_rates, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(border_apprehension_rates, extractiveness, 0.58).
narrative_ontology:constraint_metric(border_apprehension_rates, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(border_apprehension_rates, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(border_apprehension_rates, tangled_rope).
narrative_ontology:human_readable(border_apprehension_rates, "Border Apprehension Rate Optimization Constraint").
narrative_ontology:topic_domain(border_apprehension_rates, "immigration_policy/border_enforcement").

domain_priors:requires_active_enforcement(border_apprehension_rates).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(border_apprehension_rates, enforcement_agency).
narrative_ontology:constraint_beneficiary(border_apprehension_rates, political_leadership).
narrative_ontology:constraint_victim(border_apprehension_rates, migrant_communities).
narrative_ontology:constraint_victim(border_apprehension_rates, humanitarian_capacity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: APPREHENDED MIGRANT (SNARE) — Faces physical confinement, legal barriers to exit (immigration proceedings), economic dependency during detention, and information asymmetry about rights and processes. No meaningful alternatives to the apprehension-detention-processing pipeline. Maximum extraction with minimal coordination benefit.
constraint_indexing:constraint_classification(border_apprehension_rates, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: BORDER COMMUNITY RESIDENTS (TANGLED ROPE) — Communities gain some coordination benefits (security infrastructure, employment in enforcement agencies) alongside extraction costs (strain on humanitarian services, school system capacity, healthcare resources). Exit is possible but costly (relocation). Mixed coordination and extraction with genuine agency constraints.
constraint_indexing:constraint_classification(border_apprehension_rates, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: IMMIGRATION ENFORCEMENT AGENCY (ROPE) — Primary beneficiary. Apprehension metrics directly fund agency operations, staff positions, and budget justification. The constraint solves a genuine coordination problem: processing migrant flows requires standardized procedures and metrics. Agency experiences this as functional coordination with career and resource benefits.
constraint_indexing:constraint_classification(border_apprehension_rates, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: POLITICAL LEADERSHIP (ROPE) — Secondary beneficiary. Apprehension statistics serve as measurable performance indicators for electoral accountability and policy justification. Generates constituency demand for enforcement action and visible enforcement results. Net beneficiary with broad policy discretion.
constraint_indexing:constraint_classification(border_apprehension_rates, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: HUMANITARIAN CAPACITY SYSTEM (SNARE) — Abstract system (NGOs, legal aid, medical services, shelter networks) cannot exit constraint despite bearing extraction costs. Apprehension rates directly drive demand for humanitarian services beyond sustainable capacity. No mechanism to reduce demand or refuse service without violating humanitarian mandates. Theater: much humanitarian work is grant-dependent performance reporting rather than impact delivery.
constraint_indexing:constraint_classification(border_apprehension_rates, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (PITON) — The apprehension rate metric has drifted toward purely performative use. Originally conceived as a measure of enforcement effectiveness, it now functions as a proxy for political will and agency capacity rather than actual border security or humanitarian outcome improvement. Theater ratio reflects that apprehension numbers are reported to demonstrate activity and commitment, not to correlate with actual unauthorized entry prevention, organized trafficking disruption, or humanitarian outcomes. Classification as Piton indicates degraded function maintained through institutional inertia.
constraint_indexing:constraint_classification(border_apprehension_rates, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(border_apprehension_rates_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(border_apprehension_rates, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(border_apprehension_rates, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(border_apprehension_rates, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(border_apprehension_rates, TR),
    TR >= 0.70.

:- end_tests(border_apprehension_rates_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The constraint creates asymmetric costs: migrants and humanitarian systems bear suppression and detention costs; enforcement agencies capture budget and career benefits. The original coordination function (processing migrants at borders) is real but increasingly subordinated to metric optimization. Suppression (0.65): High. Multiple suppression mechanisms: physical detention, legal barriers during processing, information asymmetry about rights, economic dependency, geographic isolation at borders, learned helplessness from previous crossing attempts. Theater ratio (0.68): High. Apprehension reporting emphasizes political performance and agency capability rather than outcomes (actual security improvements, humanitarian capacity enhancement, or successful integration). The gap between reported metrics and actual measured security outcomes indicates substitution of metric performance for real function. Claimed type (Tangled Rope): Genuine coordination function (standardized procedures, resource allocation) exists alongside asymmetric extraction (migrants detained, humanitarian systems strained). Requires active enforcement (border operations are highly staffed and intensive).
 *
 * PERSPECTIVAL GAP:
 *   The constraint produces divergent classifications across perspectives due to differential power and exit options. Apprehended migrants (powerless/trapped) experience maximum extraction with minimal coordination benefit — snare classification. Enforcement agencies (institutional/arbitrage) experience coordination and career benefits — rope classification. Border communities (moderate/constrained) experience mixed benefits and costs — tangled rope. Humanitarian systems (powerless/trapped) experience pure extraction with no exit — snare. Political leadership (institutional/arbitrage) experience metric utility and electoral benefits — rope. The analytical observer (civilizational scope) sees that the original coordination function has been degraded and largely replaced by metric theater — piton classification. The perspectival gap reveals that the constraint simultaneously functions as coordination (for beneficiaries), extraction (for victims), and theater (for measurement systems).
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (enforcement_agency, political_leadership) are institutional actors with arbitrage exit options — they can reallocate enforcement effort or policy focus with minimal cost. Structural derivation produces low directionality (d ≈ 0.15-0.20 for arbitrage institutional) and negative effective extraction from their perspective — they benefit. Victims (migrant_communities, humanitarian_capacity) are trapped or powerless with no exit options — they cannot reduce enforcement demand or refuse service. Structural derivation produces high directionality (d ≈ 0.90-0.95 for trapped powerless) and high effective extraction from their perspective. Border communities are constrained (moderate cost to relocate) and mixed (both benefit and cost) — they experience moderate directionality (d ≈ 0.55-0.65). The suppression value (0.65) applies equally across all perspectives — it is a structural property of the constraint, not scaled by power or scope. Only effective extraction (chi) is scaled by f(d) and σ(S).
 *
 * MANDATROPHY ANALYSIS:
 *   The apprehension rate constraint resolves mandatrophy by showing how a single metric can sustain multiple structurally distinct constraint types depending on perspective. The snare perspective (trapped victims) is the authentic structural reality — migrants experience maximum extraction with suppression and no exit. The rope perspective (institutional beneficiaries) is also structurally valid — they genuinely coordinate and benefit. The piton perspective (analytical/civilizational) reveals that the original coordination function has been substantially replaced by metric theater and performance reporting. The tangled rope classification (claimed type) correctly identifies that genuine coordination exists (standardized procedures, resource allocation) alongside genuine asymmetric extraction (migrant suppression, humanitarian strain). The mandatrophy is resolved by recognizing that all four perspectives are simultaneously true: the constraint IS a coordination mechanism AND an extraction mechanism AND a performance theater — the debate about which is 'real' mistakes the structure for a classification error. The theater ratio (0.68) and rising extractiveness trajectory (0.42 → 0.58) together indicate Goodhart drift: the metric has increasingly substituted for the underlying coordination function.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    apprehension_rate_measurement_validity,
    'Do apprehension rates measure what they claim to measure — border security effectiveness — or do they primarily reflect enforcement effort intensity independent of security outcomes?',
    'Regression analysis: correlation between apprehension rates and (a) actual unauthorized entries detected, (b) trafficking organizations disrupted, (c) security incident prevention. Time-series decomposition separating enforcement effort from effectiveness.',
    'If highly correlated with outcomes: snare classification may be overstated; coordination function is real. If uncorrelated: apprehension rate is pure metric theater, and the snare is maintained through performative reporting rather than structural necessity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(apprehension_rate_measurement_validity, empirical, 'Whether apprehension rates measure border security effectiveness or enforcement effort').

omega_variable(
    suppression_mechanism_structural_vs_internalized,
    'Is migrant suppression of exit attempts primarily structural (physical barriers, legal/detention), or do internalized norms (belief in futility, identity as deportable, trauma-based learned helplessness) contribute equally?',
    'Post-apprehension behavior tracking: willingness to attempt crossing again, help-seeking, community resilience patterns. Comparison of structural barriers vs reported psychological factors in migrant interviews.',
    'If structural dominates: suppression declines when barriers are reduced. If internalized: suppression persists as trauma even after release; constraint is partially cognitive capture requiring different intervention.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_vs_internalized, empirical, 'Whether suppression is structural or internalized').

omega_variable(
    metric_substitution_drift,
    'Has the apprehension rate metric substituted for actual border security and humanitarian outcome goals (Goodhart drift), where agencies optimize the metric rather than the underlying objectives?',
    'Analysis of agency incentive structures, performance evaluation criteria, and resource allocation patterns. Interview data on agency staff goals and success indicators.',
    'If drift confirmed: theater ratio increases, extractiveness is maintained through metric theater rather than material processing necessity, and the piton classification is correct — function has atrophied but metric enforcement persists.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(metric_substitution_drift, empirical, 'Whether apprehension rate metric has substituted for actual security goals').

omega_variable(
    enforcement_capacity_constraint,
    'What fraction of apprehension extraction results from genuine physical/legal capacity limits versus deliberate enforcement throttling based on political/budgetary objectives?',
    'Comparative analysis across jurisdictions with different political contexts. Estimation of unused processing capacity during low-apprehension periods vs surge periods.',
    'If primarily capacity constraint: the tangled rope and snare classifications reflect unavoidable coordination costs. If primarily political throttling: suppression is more contingent and the constraint is more purely extractive.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_capacity_constraint, empirical, 'Whether enforcement capacity or political choices drive apprehension rates').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(border_apprehension_rates, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(border_tr_t0, border_apprehension_rates, theater_ratio, 0, 0.52).
narrative_ontology:measurement(border_tr_t5, border_apprehension_rates, theater_ratio, 5, 0.62).
narrative_ontology:measurement(border_tr_t10, border_apprehension_rates, theater_ratio, 10, 0.68).
narrative_ontology:measurement(border_tr_t15, border_apprehension_rates, theater_ratio, 15, 0.71).

% Extraction over time
narrative_ontology:measurement(border_be_t0, border_apprehension_rates, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(border_be_t5, border_apprehension_rates, base_extractiveness, 5, 0.5).
narrative_ontology:measurement(border_be_t10, border_apprehension_rates, base_extractiveness, 10, 0.58).
narrative_ontology:measurement(border_be_t15, border_apprehension_rates, base_extractiveness, 15, 0.61).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(border_apprehension_rates, enforcement_mechanism).
narrative_ontology:affects_constraint(border_apprehension_rates, migrant_detention_facility_capacity).
narrative_ontology:affects_constraint(border_apprehension_rates, humanitarian_service_funding_adequacy).
narrative_ontology:affects_constraint(border_apprehension_rates, asylum_processing_speed_bottleneck).

% DUAL FORMULATION NOTE:
% Border apprehension rates represent a constraint family decomposable into: (1) apprehension_rates as metric performance (this story, ε=0.58, Tangled Rope), (2) actual_border_security_outcomes as coordination problem (ε=0.15, Rope — genuine security coordination with low extraction), (3) migrant_suppression_mechanism as pure extraction (ε=0.78, Snare — the human cost of enforcement). The high extractiveness value (0.58) reflects the metric-level constraint; actual security coordination is lower extraction.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(border_apprehension_rates, institutional, 0.18).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
