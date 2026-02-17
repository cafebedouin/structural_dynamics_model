% ============================================================================
% CONSTRAINT STORY: prime_age_male_unwork
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-08
% ============================================================================

:- module(constraint_prime_age_male_unwork, []).

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
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:human_readable/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * constraint_id: prime_age_male_unwork
 * human_readable: The "New Misery" of Prime-Age Male Labor Force Exit
 * domain: social/economic
 *
 * SUMMARY:
 * A systemic "exodus" where roughly 7 million men (1 in 10) aged 25-54 are 
 * neither working nor looking for work. This condition is 
 * characterized by high rates of pain medication use, former felony status, 
 * and "deaths of despair".
 *
 * KEY AGENTS (by structural relationship):
 * - The Un-worker: Primary target (powerless/trapped) — bears extraction of potential
 * - Entitlement State: Primary beneficiary (institutional/arbitrage) — benefits from administrative maintenance
 * - Civil Society: Secondary victim (moderate/constrained) — suffers from fragility
 * - Political Economist: Analytical observer — identifies the "human arithmetic"
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(prime_age_male_unwork, 0.62).
domain_priors:suppression_score(prime_age_male_unwork, 0.70).   
domain_priors:theater_ratio(prime_age_male_unwork, 0.15).       

% --- Constraint metric facts ---
narrative_ontology:constraint_metric(prime_age_male_unwork, extractiveness, 0.62).
narrative_ontology:constraint_metric(prime_age_male_unwork, suppression_requirement, 0.70).
narrative_ontology:constraint_metric(prime_age_male_unwork, theater_ratio, 0.15).

% --- Constraint claim (matches analytical perspective) ---
narrative_ontology:constraint_claim(prime_age_male_unwork, snare).
narrative_ontology:human_readable(prime_age_male_unwork, "The \"New Misery\" of Prime-Age Male Labor Force Exit").

% --- Binary flags ---
domain_priors:requires_active_enforcement(prime_age_male_unwork). % Regulatory welfare cliffs/legal barriers

% --- Structural relationships ---
% Who benefits? The state apparatus managing dependency.
narrative_ontology:constraint_beneficiary(prime_age_male_unwork, entitlement_dispensing_state).
% Who bears the cost? The men removed from the labor force.
narrative_ontology:constraint_victim(prime_age_male_unwork, prime_age_unworkers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE UN-WORKER (SNARE)
% Victim + trapped exit → d ≈ 0.95 → f(d) ≈ 1.42 → χ ≈ 0.88
constraint_indexing:constraint_classification(prime_age_male_unwork, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE ENTITLEMENT BUREAUCRACY (ROPE)
% Beneficiary + arbitrage exit → d ≈ 0.05 → f(d) ≈ -0.12 → Negative/Low χ
constraint_indexing:constraint_classification(prime_age_male_unwork, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (SNARE)
% Derived d ≈ 0.72 → f(d) ≈ 1.15
constraint_indexing:constraint_classification(prime_age_male_unwork, snare,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(prime_age_male_unwork_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(prime_age_male_unwork, snare, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(prime_age_male_unwork, rope, context(agent_power(institutional), _, _, _)).

test(extraction_threshold) :-
    narrative_ontology:constraint_metric(prime_age_male_unwork, extractiveness, E),
    E >= 0.46.

:- end_tests(prime_age_male_unwork_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * Base extraction (0.62) reflects the massive loss of human agency and economic 
 * potential described in the "New Misery". Suppression (0.70) is 
 * driven by the lack of viable exit options for those with criminal records 
 * or chronic health issues (pain meds).
 *
 * PERSPECTIVAL GAP:
 * The Bureaucracy classifies this as a Rope (managed social coordination), 
 * while the Un-worker experiences a Snare (dependency trap).
 *
 * DIRECTIONALITY LOGIC:
 * The victim group (prime_age_unworkers) experiences the full weight of 
 * extraction (d=0.95), while the beneficiary (entitlement_state) uses the 
 * system as a coordination mechanism for debt-financed social stability.
 *
 * MANDATROPHY ANALYSIS:
 * Labeling this as a Snare prevents it from being mislabeled as a 
 * "Mountain" of inevitable social trends, reasserting the role of 
 * personal choice and agency.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    omega_unwork_agency,
    'Is the labor exit a structural necessity of automation or a policy-induced dependency?',
    'Analysis of participation rates in sectors with high human-capital reliance vs. automation density.',
    'If policy-induced, it remains a Snare; if automation-driven, it drifts toward Mountain.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(prime_age_male_unwork, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio: Rising as the "New Misery" is ignored by official "detectors"
narrative_ontology:measurement(unw_tr_t0, prime_age_male_unwork, theater_ratio, 0, 0.05).
narrative_ontology:measurement(unw_tr_t5, prime_age_male_unwork, theater_ratio, 5, 0.10).
narrative_ontology:measurement(unw_tr_t10, prime_age_male_unwork, theater_ratio, 10, 0.15).

% Extraction: Increasing as the gap between affluent private net worth and "unworker" misery grows
narrative_ontology:measurement(unw_ex_t0, prime_age_male_unwork, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(unw_ex_t5, prime_age_male_unwork, base_extractiveness, 5, 0.55).
narrative_ontology:measurement(unw_ex_t10, prime_age_male_unwork, base_extractiveness, 10, 0.62).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(prime_age_male_unwork, resource_allocation).

% Network effects: Dependency affects labor mobility and family stability
narrative_ontology:affects_constraint(prime_age_male_unwork, us_labor_mobility).
narrative_ontology:affects_constraint(prime_age_male_unwork, fragile_families).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
