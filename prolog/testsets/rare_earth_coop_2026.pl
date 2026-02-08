% ============================================================================
% CONSTRAINT STORY: rare_earth_coop_2026
% ============================================================================
% Version: 3.4 (Deferential Realism Core)
% Logic: 3.3 (Indexed Tuple P,T,E,S)
% Generated: 2026-02-06
% ============================================================================

:- module(constraint_rare_earth_coop, []).

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
    constraint_indexing:constraint_classification/3.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * * constraint_id: rare_earth_coop_2026
 * human_readable: Manufacturer-Owned Rare Earth Cooperative (MOREC)
 * domain: economic/industrial
 * * SUMMARY:
 * A horizontal coordination mechanism where manufacturers pool capital to 
 * bypass the profit-seeking mandates of Project Vault. It functions as a 
 * Rope—a voluntary association where the "extraction" is limited to 
 * administrative cost-recovery, ensuring that the agents are the 
 * primary beneficiaries of the coordination.
 * * KEY AGENTS:
 * - Participating Manufacturers: Subject (Moderate) - The owners/users.
 * - Coop Board: Auditor (Analytical) - Ensuring price transparency.
 * - External Markets: Object - The source of physical minerals.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Base extraction is near-zero (cost-recovery only).
domain_priors:base_extractiveness(rare_earth_coop_2026, 0.02). 
domain_priors:suppression_score(rare_earth_coop_2026, 0.15).   % Low suppression; members can leave.
domain_priors:theater_ratio(rare_earth_coop_2026, 0.20).       % Functional transparency over spectacle.

% Primary keys for classification engine
narrative_ontology:constraint_metric(rare_earth_coop_2026, extractiveness, 0.02).
narrative_ontology:constraint_metric(rare_earth_coop_2026, suppression_requirement, 0.15).
narrative_ontology:constraint_metric(rare_earth_coop_2026, theater_ratio, 0.20).

% Constraint classification claim
narrative_ontology:constraint_claim(rare_earth_coop_2026, rope).

% Rope Structural Property Declarations
% A Rope is characterized by the agent being their own beneficiary.
narrative_ontology:constraint_beneficiary(rare_earth_coop_2026, participating_manufacturers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × π(P) × σ(S)
   ========================================================================== */

% PERSPECTIVE 1: THE MEMBER MANUFACTURER (ROPE)
% Effective Extraction: 0.02 * 1.0 (moderate) * 1.2 (global) = 0.024.
% χ < 0.05 + base_extraction < 0.10 triggers the Rope classification.
constraint_indexing:constraint_classification(rare_earth_coop_2026, rope, 
    context(agent_power(moderate), 
            time_horizon(biographical), 
            exit_options(mobile), 
            spatial_scope(global))).

% PERSPECTIVE 2: THE ANALYTICAL OBSERVER
constraint_indexing:constraint_classification(rare_earth_coop_2026, rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(rare_earth_coop_2026_tests).

test(rope_verification) :-
    % Ropes require low base extraction and low effective extraction.
    narrative_ontology:constraint_metric(rare_earth_coop_2026, extractiveness, E),
    E < 0.05.

test(beneficiary_identity) :-
    % In a Rope, the agent should ideally be the beneficiary.
    narrative_ontology:constraint_beneficiary(rare_earth_coop_2026, participating_manufacturers).

:- end_tests(rare_earth_coop_2026_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The MOREC model resolves the "Project Vault" paradox by removing the 
 * debt-servicing profit mandate. Because the manufacturers own the 
 * infrastructure, the extraction is recursive—funds taken for administrative 
 * overhead (0.02) return as supply security. This qualifies as a "Pure 
 * Coordination" Rope. 
 *
 * ASSUMPTIONS:
 * This model assumes that manufacturers can overcome the "Collective Action" 
 * problem without a central state enforcer, and that anti-trust regulations 
 * are waived for strategic mineral co-ops.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    omega_coop_stability,
    "Will a 'Free Rider' problem emerge if one member leaves during a market dip?",
    "Review of coop bylaw enforcement and member retention data in 2027.",
    "Stability preserves the Rope; defection triggers a Tangled Rope.",
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(rare_earth_coop_2026, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio remains low in a functional coop.
narrative_ontology:measurement(rec_tr_t0, rare_earth_coop_2026, theater_ratio, 0, 0.20).
narrative_ontology:measurement(rec_tr_t5, rare_earth_coop_2026, theater_ratio, 5, 0.20).
narrative_ontology:measurement(rec_tr_t10, rare_earth_coop_2026, theater_ratio, 10, 0.25).

% Extraction remains stable and low.
narrative_ontology:measurement(rec_ex_t0, rare_earth_coop_2026, base_extractiveness, 0, 0.02).
narrative_ontology:measurement(rec_ex_t5, rare_earth_coop_2026, base_extractiveness, 5, 0.02).
narrative_ontology:measurement(rec_ex_t10, rare_earth_coop_2026, base_extractiveness, 10, 0.03).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */

% ============================================================================
% ENRICHMENT: Structural predicates for remaining gaps
% Generated: 2026-02-08
% Template: v5.2 namespace alignment
% Source: Derived from narrative context in this file (rare_earth_coop_2026)
% ============================================================================
constraint_victim(rare_earth_coop_2026, non_member_market_participants).

% PERSPECTIVE 3: THE SMALL MANUFACTURER (ROPE)
% Even non-members benefit from price stabilization; membership is voluntary.
constraint_indexing:constraint_classification(rare_earth_coop_2026, rope,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(mobile),
            spatial_scope(global))).
