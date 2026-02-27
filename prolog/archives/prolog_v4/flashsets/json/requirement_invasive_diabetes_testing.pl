% ============================================================================
% CONSTRAINT STORY: requirement_invasive_diabetes_testing
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-02-29
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_requirement_invasive_diabetes_testing, []).

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
 *   constraint_id: requirement_invasive_diabetes_testing
 *   human_readable: The Requirement for Invasive Blood Testing to Diagnose and Monitor Diabetes
 *   domain: technological/economic
 *
 * SUMMARY:
 *   The persistent reliance on invasive blood testing for diabetes management
 *   creates a structural tension between established medical practice and
 *   emerging non-invasive technologies. This constraint highlights the
 *   coordination benefits of standardized testing alongside the extractive
 *   burdens placed on patients. The inertial persistence of the blood-test
 *   requirement raises questions about technological lock-in and regulatory
 *   capture, making the overall landscape a tangled rope.
 *
 * KEY AGENTS:
 *   - Diabetes Patients: Primary victim (powerless/trapped) - faces discomfort, cost, access barriers.
 *   - Medical Device Manufacturers: Primary beneficiary (institutional/arbitrage) - benefits from stable market, but also faces incentives to innovate.
 *   - Primary Care Physicians: Moderate, (constrained)- Constrained by established protocols, time, and patient needs.
 *   - Healthcare Regulators: (organized/constrained) Balance between safety, progress, and political pressures
 *   - Traditional Medical Education System: Degraded Institution with inertia (institutional/constrained)
 *   - Analytical Observer: Sees the gap between available tech and actual practice (analytical/analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(requirement_invasive_diabetes_testing, 0.55).
domain_priors:suppression_score(requirement_invasive_diabetes_testing, 0.65).
domain_priors:theater_ratio(requirement_invasive_diabetes_testing, 0.75).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(requirement_invasive_diabetes_testing, extractiveness, 0.55).
narrative_ontology:constraint_metric(requirement_invasive_diabetes_testing, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(requirement_invasive_diabetes_testing, theater_ratio, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(requirement_invasive_diabetes_testing, tangled_rope).
narrative_ontology:human_readable(requirement_invasive_diabetes_testing, "The Requirement for Invasive Blood Testing to Diagnose and Monitor Diabetes").
narrative_ontology:topic_domain(requirement_invasive_diabetes_testing, "technological/economic").

domain_priors:requires_active_enforcement(requirement_invasive_diabetes_testing).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(requirement_invasive_diabetes_testing, medical_device_manufacturers).
narrative_ontology:constraint_beneficiary(requirement_invasive_diabetes_testing, medical_testing_laboratories).
narrative_ontology:constraint_victim(requirement_invasive_diabetes_testing, diabetes_patients).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Patients often feel trapped, needing regular testing for health management but face discomfort, cost, and access barriers due to the invasive nature of blood tests. Lack power to change the system alone.
constraint_indexing:constraint_classification(requirement_invasive_diabetes_testing, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% Physicians are constrained by established protocols and liability concerns. They benefit from reliable diagnostic tools but face time constraints and patient compliance issues. They have some mobility via technology adoption but face inertia.
constraint_indexing:constraint_classification(requirement_invasive_diabetes_testing, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% Medical device manufacturers benefit from the established market for blood glucose monitoring systems. They can arbitrage this position by innovating new technologies, but the existing market provides a stable revenue stream. They actively coordinate market share and innovation cycles.
constraint_indexing:constraint_classification(requirement_invasive_diabetes_testing, rope,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% Regulators benefit from maintaining standards and ensuring patient safety but are also constrained by the need to avoid stifling innovation and face lobbying from incumbent device manufacturers. They have constrained exits - must serve multiple groups.
constraint_indexing:constraint_classification(requirement_invasive_diabetes_testing, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% Medical education perpetuates the standard of care, even as new technologies emerge. It is a degraded institution, once necessary, now possibly inertial and theatrical, not truly beneficial.
constraint_indexing:constraint_classification(requirement_invasive_diabetes_testing, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

% Analytical observers see the mixed coordination/extraction inherent in relying on invasive testing when non-invasive options exist, highlighting how established practice can create unnecessary burdens and delay adoption of superior methods.
constraint_indexing:constraint_classification(requirement_invasive_diabetes_testing, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(requirement_invasive_diabetes_testing_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(requirement_invasive_diabetes_testing, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(requirement_invasive_diabetes_testing, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(requirement_invasive_diabetes_testing, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(requirement_invasive_diabetes_testing, TR),
    TR >= 0.70.

:- end_tests(requirement_invasive_diabetes_testing_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.55): Moderate. Patients experience extraction via pain, cost, inconvenience, and time. The economic rent is split with manufacturers who also take on liability with testing devices. Suppression (0.65): High. The traditional medical system suppresses alternatives such as new non-invasive techniques through regulations or habit. Theater ratio (0.75): High. While medical testing serves a direct practical function, the persistence of invasive methods despite the existence of less invasive alternatives suggests a significant degree of theatrical maintenance of established protocols and procedures.
 *
 * PERSPECTIVAL GAP:
 *   The core point of perspectival difference lies in the view of patients versus industry. Patients experience a SNARE - a costly, painful, and required intervention. Industry sees the traditional system as a ROPE - a coordinating mechanism to diagnose and treat diabetes. The other perspectives, as constrained groups, express elements of both.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values reflect the structural position of each agent. Patients (powerless/trapped) experience high extraction (d=1). Manufacturers (institutional/arbitrage) benefit from the established market (d ~ 0). Physicians (moderate/constrained) experience a mixed position (d ~ 0.5). Healthcare regulators (organized/constrained) similarly face a mixed position, balancing different priorities (d = 0.4). The analytical observer sees the system as a whole and its effects (d = 0.72).
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    non_invasive_accuracy_threshold,
    'What level of accuracy in non-invasive glucose monitoring is required to displace blood tests?',
    'Clinical trials comparing non-invasive and blood-based methods; analysis of cost-benefit ratios for different accuracy levels.',
    'Determines if the barrier is truly technological, or primarily regulatory and economic.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(non_invasive_accuracy_threshold, empirical, 'Minimum accuracy required for non-invasive testing to replace blood tests').

omega_variable(
    regulatory_capture_degree,
    'To what extent do incumbent medical device manufacturers influence regulatory decisions regarding non-invasive technologies?',
    'Lobbying expenditure analysis; examination of regulator-industry revolving door; Freedom of Information Act requests.',
    'Reveals if the ''extraction'' component is driven by structural advantage or active rent-seeking.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_capture_degree, empirical, 'Influence of incumbents on regulation of non-invasive technologies').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(requirement_invasive_diabetes_testing, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(requ_tr_t0, requirement_invasive_diabetes_testing, theater_ratio, 0, 0.6).
narrative_ontology:measurement(requ_tr_t10, requirement_invasive_diabetes_testing, theater_ratio, 10, 0.7).
narrative_ontology:measurement(requ_tr_t20, requirement_invasive_diabetes_testing, theater_ratio, 20, 0.75).

% Extraction over time
narrative_ontology:measurement(requ_be_t0, requirement_invasive_diabetes_testing, base_extractiveness, 0, 0.6).
narrative_ontology:measurement(requ_be_t10, requirement_invasive_diabetes_testing, base_extractiveness, 10, 0.58).
narrative_ontology:measurement(requ_be_t20, requirement_invasive_diabetes_testing, base_extractiveness, 20, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(requirement_invasive_diabetes_testing, information_standard).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
