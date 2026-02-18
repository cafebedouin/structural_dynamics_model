% ============================================================================
% CONSTRAINT STORY: us_labor_mobility
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-08
% ============================================================================

:- module(constraint_us_labor_mobility, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * constraint_id: us_labor_mobility
 * human_readable: US Geographic and Professional Labor Mobility
 * domain: economic/technological
 *
 * SUMMARY:
 * The ability of the US workforce to relocate for opportunity. While 
 * historically a high-fluidity "Rope," it has increasingly turned into 
 * a "Snare" due to the convergence of housing inventory locks, employer-
 * sponsored insurance (ESI), and regional licensing barriers.
 *
 * KEY AGENTS (by structural relationship):
 * - Locked Worker: Primary target (powerless/trapped) — bears costs of job-lock
 * - Local Monopolies: Primary beneficiary (institutional/arbitrage) — benefits from captured labor pools
 * - Remote-First Tech: Secondary beneficiary (institutional/mobile) — leverages mobility gaps
 * - Macroeconomist: Analytical observer — monitors interstate migration decline
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(us_labor_mobility, 0.48).
domain_priors:suppression_score(us_labor_mobility, 0.52).   
domain_priors:theater_ratio(us_labor_mobility, 0.25).       

% --- Constraint metric facts ---
narrative_ontology:constraint_metric(us_labor_mobility, extractiveness, 0.48).
narrative_ontology:constraint_metric(us_labor_mobility, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(us_labor_mobility, theater_ratio, 0.25).

% --- Constraint claim (matches analytical perspective) ---
narrative_ontology:constraint_claim(us_labor_mobility, tangled_rope).
narrative_ontology:human_readable(us_labor_mobility, "US Geographic and Professional Labor Mobility").
narrative_ontology:topic_domain(us_labor_mobility, "economic/technological").

% --- Binary flags ---
domain_priors:requires_active_enforcement(us_labor_mobility). % State licensing/zoning enforcement

% --- Structural relationships ---
% Who benefits? Regional employers and localized service industries.
narrative_ontology:constraint_beneficiary(us_labor_mobility, regional_labor_monopolies).
% Who bears the cost? Workers seeking optimal wage/productivity matches.
narrative_ontology:constraint_victim(us_labor_mobility, mobile_labor_force).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE MORTGAGE-LOCKED WORKER (SNARE)
% Victim + trapped exit → d ≈ 0.95 → f(d) ≈ 1.42 → χ ≈ 0.68
constraint_indexing:constraint_classification(us_labor_mobility, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE REMOTE EMPLOYER (ROPE)
% Beneficiary + arbitrage exit → d ≈ 0.05 → f(d) ≈ -0.12 → χ ≈ -0.05
constraint_indexing:constraint_classification(us_labor_mobility, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% Derived d ≈ 0.72 → f(d) ≈ 1.15
constraint_indexing:constraint_classification(us_labor_mobility, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(us_labor_mobility_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(us_labor_mobility, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(us_labor_mobility, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget \= TypeBeneficiary.

test(extraction_threshold) :-
    narrative_ontology:constraint_metric(us_labor_mobility, extractiveness, E),
    E >= 0.46.

:- end_tests(us_labor_mobility_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * Base extraction (0.48) reflects the widening gap between potential 
 * productivity and current geographic constraints. 
 *
 * PERSPECTIVAL GAP:
 * The "locked" worker perceives a Snare because the financial and social 
 * cost of moving (losing low mortgage rates or insurance continuity) exceeds 
 * potential gains. Institutions see it as a Rope—a stable, predictable labor market.
 *
 * DIRECTIONALITY LOGIC:
 * Regional monopolies benefit from reduced labor churn (d=0.05), while workers 
 * experience extraction of their market value (d=0.95).
 *
 * MANDATROPHY ANALYSIS:
 * Classification as a Tangled Rope prevents the system from viewing 
 * immobility as a "natural law" of aging populations, highlighting it 
 * as a policy-dependent artifact.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    omega_mobility_decoupling,
    'Will digital work-from-anywhere (WFA) decouple economic mobility from geographic relocation?',
    'Tracking 2026-2030 wage convergence between low-cost and high-cost hubs.',
    'If wages converge without movement, the constraint dissolves into a Piton.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(us_labor_mobility, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio: Rising as "work-life balance" initiatives mask structural job-lock
narrative_ontology:measurement(mob_tr_t0, us_labor_mobility, theater_ratio, 0, 0.15).
narrative_ontology:measurement(mob_tr_t5, us_labor_mobility, theater_ratio, 5, 0.20).
narrative_ontology:measurement(mob_tr_t10, us_labor_mobility, theater_ratio, 10, 0.25).

% Extraction: Rising due to the increasing spread between legacy mortgage rates and 2026 market rates
narrative_ontology:measurement(mob_ex_t0, us_labor_mobility, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(mob_ex_t5, us_labor_mobility, base_extractiveness, 5, 0.42).
narrative_ontology:measurement(mob_ex_t10, us_labor_mobility, base_extractiveness, 10, 0.48).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(us_labor_mobility, resource_allocation).

% Network: Directly throttled by healthcare and dependency patterns
narrative_ontology:affects_constraint(us_employer_health_insurance, us_labor_mobility).
narrative_ontology:affects_constraint(prime_age_male_unwork, us_labor_mobility).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
