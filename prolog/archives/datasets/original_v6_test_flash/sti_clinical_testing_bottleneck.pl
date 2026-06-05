% ============================================================================
% CONSTRAINT STORY: sti_clinical_testing_bottleneck
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-01-01
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sti_clinical_testing_bottleneck, []).

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
 *   constraint_id: sti_clinical_testing_bottleneck
 *   human_readable: Requirement for Clinical Lab Testing for Chlamydia/Gonorrhea
 *   domain: technological
 *
 * SUMMARY:
 *   The requirement for clinical lab testing for common STIs like chlamydia
 *   and gonorrhea creates a bottleneck that limits access to care and hinders
 *   public health efforts. While clinical labs and medical device
 *   manufacturers benefit from this system, patients and public health
 *   agencies bear the costs.
 *
 * KEY AGENTS:
 *   - Patients at Risk: Primary victim (powerless/trapped) - face barriers to accessing testing and treatment.
 *   - Public Health Agencies: Secondary victim (moderate/constrained) - rely on timely data but are hindered by the slow testing system.
 *   - Clinical Labs: Primary beneficiary (institutional/arbitrage) - profit from the demand for testing.
 *   - Medical Device Manufacturers: Primary beneficiary (institutional/arbitrage) - benefit from the widespread use of traditional testing methods.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sti_clinical_testing_bottleneck, 0.6).
domain_priors:suppression_score(sti_clinical_testing_bottleneck, 0.5).
domain_priors:theater_ratio(sti_clinical_testing_bottleneck, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sti_clinical_testing_bottleneck, extractiveness, 0.6).
narrative_ontology:constraint_metric(sti_clinical_testing_bottleneck, suppression_requirement, 0.5).
narrative_ontology:constraint_metric(sti_clinical_testing_bottleneck, theater_ratio, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sti_clinical_testing_bottleneck, tangled_rope).
narrative_ontology:human_readable(sti_clinical_testing_bottleneck, "Requirement for Clinical Lab Testing for Chlamydia/Gonorrhea").
narrative_ontology:topic_domain(sti_clinical_testing_bottleneck, "technological").

domain_priors:requires_active_enforcement(sti_clinical_testing_bottleneck).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sti_clinical_testing_bottleneck, clinical_labs).
narrative_ontology:constraint_beneficiary(sti_clinical_testing_bottleneck, medical_device_manufacturers).
narrative_ontology:constraint_victim(sti_clinical_testing_bottleneck, patients_at_risk).
narrative_ontology:constraint_victim(sti_clinical_testing_bottleneck, public_health_agencies).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Patients, especially those in underserved communities, often face significant barriers to accessing clinical testing, including cost, transportation, and stigma.
constraint_indexing:constraint_classification(sti_clinical_testing_bottleneck, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(local))).

% Public health agencies rely on accurate and timely STI data to track outbreaks and implement effective prevention strategies, but the traditional testing system can be slow and cumbersome.
constraint_indexing:constraint_classification(sti_clinical_testing_bottleneck, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% Clinical labs benefit from the demand for STI testing, as they receive payment for each test performed.
constraint_indexing:constraint_classification(sti_clinical_testing_bottleneck, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(national))).

% Manufacturers of clinical lab testing equipment and reagents benefit from the widespread use of traditional testing methods.
constraint_indexing:constraint_classification(sti_clinical_testing_bottleneck, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% The analytical observer recognizes the mixed coordination and extraction aspects of the clinical testing requirement. It facilitates diagnosis and treatment but also introduces delays, costs, and access barriers.
constraint_indexing:constraint_classification(sti_clinical_testing_bottleneck, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sti_clinical_testing_bottleneck_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(sti_clinical_testing_bottleneck, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(sti_clinical_testing_bottleneck, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(sti_clinical_testing_bottleneck, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(sti_clinical_testing_bottleneck_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.6): Moderate. The system extracts time, money, and access from patients and public health agencies. Suppression (0.5): Moderate. Alternative testing methods exist, but they are not widely adopted due to regulatory hurdles, cost, and lack of infrastructure. Theater ratio (0.3): Low. The testing process is largely functional, but there is some performative aspect, such as the need to maintain traditional lab infrastructure.
 *
 * PERSPECTIVAL GAP:
 *   Patients see a snare due to access barriers and delays. Public health agencies see a tangled rope because they need the data but are hindered by the system's limitations. Clinical labs and medical device manufacturers see a rope because they benefit financially from the system.
 *
 * DIRECTIONALITY LOGIC:
 *   Patients and public health agencies have high directionality values (close to 1) because they are primarily targets of extraction. Clinical labs and medical device manufacturers have low directionality values (close to 0) because they are beneficiaries.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled rope classification is appropriate because the system has both coordination and extraction aspects. It facilitates diagnosis and treatment but also creates barriers to access and hinders public health efforts. This avoids mislabeling pure extraction, because there is a valid function of delivering test results. The question becomes how that is delivered and what alternatives exist.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    point_of_care_accuracy,
    'Can point-of-care testing achieve comparable accuracy and reliability to clinical lab testing?',
    'Comparative studies of point-of-care and clinical lab testing accuracy, analysis of false positive/negative rates',
    'If point-of-care testing is sufficiently accurate, it could reduce the need for clinical lab testing and improve access to care. If not, it could compromise the quality of care.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(point_of_care_accuracy, empirical, 'Accuracy and reliability of point-of-care STI testing').

omega_variable(
    regulatory_approval_pathways,
    'Will regulatory approval pathways adapt to facilitate the adoption of new testing technologies?',
    'Analysis of FDA and other regulatory agency policies, stakeholder engagement, and advocacy efforts',
    'If regulatory pathways are streamlined, it could accelerate the adoption of innovative testing solutions. If not, it could stifle innovation and maintain the status quo.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_approval_pathways, preference, 'Adaptability of regulatory approval pathways for new testing technologies').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sti_clinical_testing_bottleneck, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sti__tr_t0, sti_clinical_testing_bottleneck, theater_ratio, 0, 0.2).
narrative_ontology:measurement(sti__tr_t5, sti_clinical_testing_bottleneck, theater_ratio, 5, 0.3).
narrative_ontology:measurement(sti__tr_t10, sti_clinical_testing_bottleneck, theater_ratio, 10, 0.4).

% Extraction over time
narrative_ontology:measurement(sti__be_t0, sti_clinical_testing_bottleneck, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(sti__be_t5, sti_clinical_testing_bottleneck, base_extractiveness, 5, 0.6).
narrative_ontology:measurement(sti__be_t10, sti_clinical_testing_bottleneck, base_extractiveness, 10, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sti_clinical_testing_bottleneck, resource_allocation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
