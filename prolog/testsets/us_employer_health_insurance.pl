% ============================================================================
% CONSTRAINT STORY: us_employer_health_insurance
% ============================================================================
% Version: 5.2 (Deferential Realism Core + Boltzmann + Purity + Network)
% Logic: 5.2 (Indexed Tuple P,T,E,S + Coupling + Purity + Network Drift)
% Generated: 2026-02-08
% ============================================================================

:- module(constraint_us_employer_health_insurance, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

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
    constraint_indexing:constraint_classification/3.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * * constraint_id: us_employer_health_insurance
 * human_readable: US Employer-Sponsored Insurance (ESI) System
 * domain: economic/social
 * * SUMMARY:
 * A path-dependent artifact of WWII-era wage freezes that ties essential 
 * healthcare access to specific employment. It creates significant labor 
 * friction (job lock) and allows for opaque value extraction by intermediaries.
 * * KEY AGENTS:
 * - Employee: Subject (Powerless) - Faces job-lock and high switching costs.
 * - Large Enterprise: Beneficiary (Institutional) - Uses ESI for retention and tax-advantaged comp.
 * - Health Policy Analyst: Auditor (Analytical) - Observes the Tangled Rope dynamics.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Numerical anchors
domain_priors:base_extractiveness(us_employer_health_insurance, 0.55). 
domain_priors:suppression_score(us_employer_health_insurance, 0.65).   
domain_priors:theater_ratio(us_employer_health_insurance, 0.35).       

% Metric facts for classification engine
narrative_ontology:constraint_metric(us_employer_health_insurance, extractiveness, 0.55).
narrative_ontology:constraint_metric(us_employer_health_insurance, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(us_employer_health_insurance, theater_ratio, 0.35).

% Analytical Claim: It is a Tangled Rope (Hybrid coordination/extraction)
narrative_ontology:constraint_claim(us_employer_health_insurance, tangled_rope).

% Structural property derivations
narrative_ontology:constraint_beneficiary(us_employer_health_insurance, large_enterprises).
narrative_ontology:constraint_victim(us_employer_health_insurance, mobile_labor_force).
domain_priors:requires_active_enforcement(us_employer_health_insurance). % Tax code/ERISA enforcement

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × π(P) × σ(S)
   ========================================================================== */

% PERSPECTIVE 1: THE EMPLOYEE (SNARE)
% χ = 0.55 * 1.5 (powerless) * 1.0 (national) = 0.825
constraint_indexing:constraint_classification(us_employer_health_insurance, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE HR DEPARTMENT / INSTITUTION (ROPE)
% χ = 0.55 * -0.2 (institutional) * 1.0 (national) = -0.11 (Net benefit/subsidy felt)
constraint_indexing:constraint_classification(us_employer_health_insurance, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 3: THE POLICY ANALYST (TANGLED ROPE)
% Default analytical context for civilizational assessment.
constraint_indexing:constraint_classification(us_employer_health_insurance, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(us_employer_health_insurance_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(us_employer_health_insurance, snare, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(us_employer_health_insurance, rope, context(agent_power(institutional), _, _, _)).

test(extraction_threshold) :-
    domain_priors:base_extractiveness(us_employer_health_insurance, E),
    E > 0.46.

:- end_tests(us_employer_health_insurance_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The 0.55 extraction score reflects the 'job lock' phenomenon where employees 
 * stay in suboptimal roles purely for health coverage, representing a massive 
 * capture of labor mobility. The Perspectival Gap is extreme: employers see a 
 * "Rope" (a necessary tool for talent competition and tax efficiency), while 
 * vulnerable employees see a "Snare" (a trap that conditions physical 
 * survival on corporate subservience).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    omega_us_esi_portability,
    'Is ESI an accidental historical artifact (Mountain) or a deliberate corporate control mechanism (Snare)?',
    'Analysis of lobbying patterns regarding decoupled public options vs. ESI tax-exemption maintenance.',
    'If accidental, it is a Piton of inertia; if deliberate, it is a high-purity Snare.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(us_employer_health_insurance, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio (Increasing due to "wellness program" performative layers)
narrative_ontology:measurement(esi_tr_t0, us_employer_health_insurance, theater_ratio, 0, 0.20).
narrative_ontology:measurement(esi_tr_t5, us_employer_health_insurance, theater_ratio, 5, 0.28).
narrative_ontology:measurement(esi_tr_t10, us_employer_health_insurance, theater_ratio, 10, 0.35).

% Extraction (Increasing as healthcare costs outpace wage growth)
narrative_ontology:measurement(esi_ex_t0, us_employer_health_insurance, base_extractiveness, 0, 0.40).
narrative_ontology:measurement(esi_ex_t5, us_employer_health_insurance, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(esi_ex_t10, us_employer_health_insurance, base_extractiveness, 10, 0.55).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(us_employer_health_insurance, resource_allocation).

% Network: ESI structurally influences the labor market flexibility
narrative_ontology:affects_constraint(us_employer_health_insurance, us_labor_mobility).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
