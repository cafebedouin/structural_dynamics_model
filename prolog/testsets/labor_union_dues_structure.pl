% ============================================================================
% CONSTRAINT STORY: labor_union_dues_structure
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-10
% ============================================================================

:- module(constraint_labor_union_dues, []).

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
 * constraint_id: labor_union_dues
 * human_readable: Mandatory Union Dues-for-Grievance Mechanism
 * domain: economic/political
 *
 * SUMMARY:
 * A mandatory financial extraction (dues) from workers to fund a collective 
 * protection mechanism (grievance handling and bargaining). While it solves 
 * the free-rider problem, it creates a structural gap where leadership 
 * may prioritize institutional survival over individual grievance resolution.
 *
 * KEY AGENTS (by structural relationship):
 * - rank_and_file_workers: Primary target (powerless/trapped) — bears dues extraction.
 * - union_administrative_body: Primary beneficiary (institutional/arbitrage) — benefits from dues.
 * - management_negotiators: Secondary actor (institutional/constrained) — experiences the counter-power.
 * - labor_auditor: Analytical observer — sees the extraction/coordination ratio.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(labor_union_dues, 0.48).
domain_priors:suppression_score(labor_union_dues, 0.65).   % High: check-off clauses/closed shops.
domain_priors:theater_ratio(labor_union_dues, 0.40).       % Significant functional coordination.

% --- Constraint metric facts (engine primary keys) ---
narrative_ontology:constraint_metric(labor_union_dues, extractiveness, 0.48).
narrative_ontology:constraint_metric(labor_union_dues, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(labor_union_dues, theater_ratio, 0.40).

% --- Constraint claim (Analytical perspective type) ---
narrative_ontology:constraint_claim(labor_union_dues, tangled_rope).
narrative_ontology:human_readable(labor_union_dues, "Mandatory Union Dues-for-Grievance Mechanism").
narrative_ontology:topic_domain(labor_union_dues, "economic/political").

% --- Binary flags ---
domain_priors:requires_active_enforcement(labor_union_dues). % Legal enforcement of check-off.

% --- Structural relationships (REQUIRED for directionality) ---
narrative_ontology:constraint_beneficiary(labor_union_dues, union_administrative_body).
narrative_ontology:constraint_victim(labor_union_dues, rank_and_file_workers).

% Coordination type for Boltzmann analysis
narrative_ontology:coordination_type(labor_union_dues, resource_allocation).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE RANK-AND-FILE WORKER
% Low power, high d (victim), trapped exit (closed shop) -> High Chi -> Snare.
constraint_indexing:constraint_classification(labor_union_dues, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE UNION LEADERSHIP
% Institutional, low d (beneficiary), arbitrage exit -> Low/Negative Chi -> Rope.
constraint_indexing:constraint_classification(labor_union_dues, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (Systems Auditor)
% Analytical context sees both extraction and coordination -> Tangled Rope.
constraint_indexing:constraint_classification(labor_union_dues, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(labor_union_dues_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(labor_union_dues, snare, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(labor_union_dues, rope, context(agent_power(institutional), _, _, _)).

test(tangled_rope_gate) :-
    domain_priors:requires_active_enforcement(labor_union_dues),
    narrative_ontology:constraint_beneficiary(labor_union_dues, _),
    narrative_ontology:constraint_victim(labor_union_dues, _).

:- end_tests(labor_union_dues_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * Base extractiveness is set at 0.48, crossing the Snare threshold for 
 * powerless agents. Suppression is high (0.65) because the exit option is 
 * legally constrained in many jurisdictions (closed shop agreements).
 *
 * PERSPECTIVAL GAP:
 * The gap is driven by directionality (d). For the 'union_administrative_body',
 * the dues are the lifeblood of coordination (d approx 0.05). For the 
 * 'rank_and_file_worker', the dues are a non-negotiable cost deducted before 
 * they see their paycheck (d approx 0.95).
 *
 * DIRECTIONALITY LOGIC:
 * Beneficiary declaration (union_administrative_body) + arbitrage exit maps 
 * to negative Chi, identifying the constraint as a subsidizing coordination 
 * mechanism for the institution. 
 *
 * MANDATROPHY ANALYSIS:
 * The 'tangled_rope' classification prevents mislabeling this as pure 
 * extraction (Snare) by acknowledging the theater_ratio is below 0.70—there 
 * is still a functional core of collective bargaining happening.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    omega_labor_dues,
    'Is the grievance mechanism effectively processing claims or merely suppressing them?',
    'Analysis of grievance-to-dues throughput ratios across 5 years.',
    'High throughput confirms Tangled Rope; Low throughput suggests Piton or Snare.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(labor_union_dues, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Extraction over time (Triggering accumulation detection)
% Modeling a gradual drift towards rent-seeking (0.35 -> 0.48)
narrative_ontology:measurement(lud_ex_t0, labor_union_dues, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(lud_ex_t5, labor_union_dues, base_extractiveness, 5, 0.42).
narrative_ontology:measurement(lud_ex_t10, labor_union_dues, base_extractiveness, 10, 0.48).

% Theater ratio over time (Goodhart drift from function to ceremony)
narrative_ontology:measurement(lud_tr_t0, labor_union_dues, theater_ratio, 0, 0.20).
narrative_ontology:measurement(lud_tr_t5, labor_union_dues, theater_ratio, 5, 0.30).
narrative_ontology:measurement(lud_tr_t10, labor_union_dues, theater_ratio, 10, 0.40).

/* ==========================================================================
   9. NETWORK DATA
   ========================================================================== */

% This constraint is coupled with corporate labor policy.
narrative_ontology:affects_constraint(labor_union_dues, corporate_grievance_policy).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
