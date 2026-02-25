% ============================================================================
% CONSTRAINT STORY: aging_well_assessment
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_aging_well_assessment, []).

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
 *   constraint_id: aging_well_assessment
 *   human_readable: The Commercialized Functional Aging Assessment System
 *   domain: health/economic
 *
 * SUMMARY:
 *   This constraint describes the evolution of functional aging tests (e.g.,
 *   grip strength, walking speed) from academic research tools into a
 *   commercialized system. This system packages assessment, intervention
 *   recommendations, and billing into a product marketed to individuals,
 *   clinics, and insurers. While it offers a standardized language for
 *   discussing and tracking functional health (a coordination function), its
 *   commercial nature introduces significant extraction through service fees
 *   and the use of data for insurance risk stratification.
 *
 * KEY AGENTS:
 *   - Elderly Patients: Primary targets (powerless/trapped) — pay for services and bear the risk of negative insurance consequences.
 *   - Assessment Companies & Specialized Clinics: Primary beneficiaries (institutional/arbitrage) — profit from the sale of assessments and interventions.
 *   - Health Insurers: Secondary beneficiaries (institutional/constrained) — gain data to price risk more accurately, but operate within a regulated environment.
 *   - General Practitioners: Secondary victims (moderate/constrained) — pressured to adopt the system, which can supersede their holistic clinical judgment.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(aging_well_assessment, 0.55).
domain_priors:suppression_score(aging_well_assessment, 0.65).
domain_priors:theater_ratio(aging_well_assessment, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(aging_well_assessment, extractiveness, 0.55).
narrative_ontology:constraint_metric(aging_well_assessment, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(aging_well_assessment, theater_ratio, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(aging_well_assessment, tangled_rope).
narrative_ontology:human_readable(aging_well_assessment, "The Commercialized Functional Aging Assessment System").
narrative_ontology:topic_domain(aging_well_assessment, "health/economic").

domain_priors:requires_active_enforcement(aging_well_assessment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(aging_well_assessment, assessment_companies).
narrative_ontology:constraint_beneficiary(aging_well_assessment, specialized_clinics).
narrative_ontology:constraint_beneficiary(aging_well_assessment, health_insurers).
narrative_ontology:constraint_victim(aging_well_assessment, elderly_patients).
narrative_ontology:constraint_victim(aging_well_assessment, general_practitioners).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ELDERLY PATIENT (SNARE) — Experiences the system as coercive. Required by their physician or insurer, they pay for assessments and interventions of unclear value, and the results can be used to increase their insurance premiums. They are trapped by their health needs and lack of alternatives. d≈0.95, f(d)≈1.42, σ=0.8 → χ≈0.62. Just below the canonical snare threshold, but the high suppression (0.65) makes the snare classification appropriate.
constraint_indexing:constraint_classification(aging_well_assessment, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: ASSESSMENT COMPANY (ROPE) — As the primary beneficiary, the company experiences the system as a pure coordination mechanism. It standardizes a valuable service, connects clients with providers, and creates a market. From their view, they are simply facilitating better health outcomes. d≈0.05, f(d)≈-0.12, σ=1.0 → χ≈-0.07. Negative effective extraction indicates a net subsidy.
constraint_indexing:constraint_classification(aging_well_assessment, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: HEALTH INSURER (TANGLED ROPE) — An institutional actor that is both a beneficiary (gains valuable risk-stratification data) and constrained by regulations and market pressures. They see both the coordination value of standardized metrics and the extractive potential for pricing risk. d≈0.30, f(d)≈0.20, σ=1.0 → χ≈0.11. Low extraction reflects their mixed role.
constraint_indexing:constraint_classification(aging_well_assessment, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: GENERAL PRACTITIONER (TANGLED ROPE) — Constrained by patient demand (driven by marketing) and insurer requirements. They see the clinical utility (coordination) but also the pressure to bill and the potential for over-medicalization (extraction). They are a victim of the system's pressure but also an agent within it. d≈0.85, f(d)≈1.15, σ=0.8 → χ≈0.51.
constraint_indexing:constraint_classification(aging_well_assessment, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(local))).

% PERSPECTIVE 5: ANALYTICAL OBSERVER (TANGLED ROPE) — The system has a genuine coordination function (standardizing functional health metrics) but is coupled with a significant, asymmetric extraction mechanism (billing, insurance risk-pricing). This hybrid nature is the hallmark of a Tangled Rope. d≈0.72, f(d)≈1.15, σ=1.2 → χ≈0.76.
constraint_indexing:constraint_classification(aging_well_assessment, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(aging_well_assessment_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(aging_well_assessment, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(aging_well_assessment, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(aging_well_assessment, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(aging_well_assessment_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (ε=0.55) is high, reflecting the direct costs to patients and the indirect costs via insurance premium adjustments. Suppression (0.65) is also high, as patients have few 'official' alternatives if the system is endorsed by their doctor or insurer, and marketing creates strong social pressure to 'age well'. The theater ratio (0.40) is moderate; the tests have real clinical grounding, but the commercial packaging and marketing add a performative layer of promising precise control over the aging process.
 *
 * PERSPECTIVAL GAP:
 *   The gap is stark. The company providing the service sees a pure Rope, a tool for empowering users. The patient, facing costs and potential penalties, sees a Snare they cannot easily escape. The insurer and doctor, caught in the middle, see a Tangled Rope, recognizing both the utility of the data and the coercive, extractive pressures the system creates. The analytical view confirms the Tangled Rope, as the system's existence depends on both its coordination function and its ability to extract value.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (assessment companies, insurers) have low derived directionality (d), resulting in low or negative effective extraction (χ), classifying the constraint as a Rope or low-extraction Tangled Rope from their perspective. Victims (patients, GPs) have high derived directionality, leading to high χ and a Snare or high-extraction Tangled Rope classification. The system's structure creates these divergent experiences.
 *
 * MANDATROPHY ANALYSIS:
 *   This case is a classic example of how a seemingly beneficial coordination tool (standardizing health metrics) becomes a Tangled Rope when commercialized. A simple analysis might label it a 'scam' (Snare) or a 'health innovation' (Rope). The Deferential Realism framework correctly identifies its dual nature. The system is not purely one or the other; its stability comes from serving a genuine coordination need for some actors while simultaneously extracting from others. This resolves the mandatrophy by showing the structure is a hybrid, not a mis-classified pure type.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    predictive_utility_vs_risk_sorting,
    'Is the system''s primary function to improve patient health outcomes (coordination) or to provide insurers with granular data for risk-sorting and premium pricing (extraction)?',
    'Longitudinal studies comparing health outcomes of participants vs. non-participants, controlling for baseline health. Analysis of how insurers change premiums based on assessment scores.',
    'If outcomes improve significantly, it strengthens the Rope/Scaffold case. If premiums rise for low-scorers without commensurate support, it confirms the Snare/Tangled Rope classification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(predictive_utility_vs_risk_sorting, empirical, 'Distinguishing between the system''s health benefit and its insurance risk-sorting function.').

omega_variable(
    intervention_efficacy,
    'Do the standardized interventions recommended based on the assessments lead to clinically significant improvements, or do they primarily serve as a billable service?',
    'Randomized controlled trials on the recommended interventions (e.g., specific exercise regimens, supplements) versus generalized lifestyle advice.',
    'High efficacy supports the coordination function. Low efficacy suggests the interventions are part of the extractive theater, strengthening the Snare classification for victims.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(intervention_efficacy, empirical, 'Determining if recommended interventions are clinically effective or primarily for revenue.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(aging_well_assessment, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(agin_tr_t0, aging_well_assessment, theater_ratio, 0, 0.1).
narrative_ontology:measurement(agin_tr_t10, aging_well_assessment, theater_ratio, 10, 0.25).
narrative_ontology:measurement(agin_tr_t20, aging_well_assessment, theater_ratio, 20, 0.4).

% Extraction over time
narrative_ontology:measurement(agin_be_t0, aging_well_assessment, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(agin_be_t10, aging_well_assessment, base_extractiveness, 10, 0.35).
narrative_ontology:measurement(agin_be_t20, aging_well_assessment, base_extractiveness, 20, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(aging_well_assessment, information_standard).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
