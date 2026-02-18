% ============================================================================
% CONSTRAINT STORY: trillion_bond_rush_2026
% ============================================================================
% Version: 3.5 (Schema-Corrected Realism)
% Logic: 3.3 (Indexed Tuple P,T,E,S)
% Generated: 2026-02-08
% ============================================================================

:- module(constraint_bond_rush, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

% --- Namespace Hooks ---
:- multifile
    domain_priors:base_extractiveness/2,
    domain_priors:suppression_score/2,
    domain_priors:theater_ratio/2,
    narrative_ontology:measurement/5,
    narrative_ontology:constraint_metric/3,
    narrative_ontology:constraint_beneficiary/2,
    narrative_ontology:constraint_victim/2,
    narrative_ontology:constraint_claim/2,
    narrative_ontology:interval/3,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * * constraint_id: trillion_bond_rush_2026
 * human_readable: Global $1 Trillion Bond Issuance Record
 * domain: economic/financial
 * * SUMMARY:
 * Global bond issuance surpassed $1 trillion on Feb 2, 2026—the fastest 
 * pace in financial history. Driven by fears of volatile interest 
 * rates and looming elections, borrowers are rushing to lock in funding 
 * costs, creating a high-extraction event for future revenue.
 * * KEY AGENTS:
 * - Future Taxpayers/Citizens: Subject (Powerless - Wealth pre-extracted)
 * - Financial Institutions/Issuers: Beneficiary (Institutional - Liquidity)
 * - Macro-Economists: Auditor (Analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Extraction is moderate-high (0.52). The "rush" front-loads future 
% interest obligations, extracting present liquidity at the cost of 
% generational fiscal space.
domain_priors:base_extractiveness(trillion_bond_rush_2026, 0.52). 

% Suppression is low (0.10). The market is highly transparent but 
% offers few exit options for the public who must service the debt.
domain_priors:suppression_score(trillion_bond_rush_2026, 0.10).   

% Theater ratio is low (0.10). The rush is driven by raw mathematical 
% cost-locking rather than purely performative signals.
domain_priors:theater_ratio(trillion_bond_rush_2026, 0.10).       

% Corrected Registry
narrative_ontology:constraint_metric(trillion_bond_rush_2026, extractiveness, 0.52).
narrative_ontology:constraint_metric(trillion_bond_rush_2026, suppression_requirement, 0.10).
narrative_ontology:constraint_metric(trillion_bond_rush_2026, theater_ratio, 0.10).

% Constraint self-claim (analytical classification)
narrative_ontology:constraint_claim(trillion_bond_rush_2026, snare).
narrative_ontology:human_readable(trillion_bond_rush_2026, "Global $1 Trillion Bond Issuance Record").
narrative_ontology:topic_domain(trillion_bond_rush_2026, "economic/financial").

narrative_ontology:constraint_beneficiary(trillion_bond_rush_2026, debt_capital_markets).
narrative_ontology:constraint_victim(trillion_bond_rush_2026, generational_fiscal_health).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE FUTURE TAXPAYER (SNARE)
% For future taxpayers, the rush is a Snare: a pre-extraction of fiscal
% space that locks them into decades of debt servicing with no exit.
constraint_indexing:constraint_classification(trillion_bond_rush_2026, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: THE INSTITUTIONAL ISSUER (ROPE)
% For banks and corporations, the rush is a Rope: a vital coordination 
% tool to secure runway before potential market closures or hikes.
constraint_indexing:constraint_classification(trillion_bond_rush_2026, rope, 
    context(agent_power(institutional), 
            time_horizon(generational), 
            exit_options(mobile), 
            spatial_scope(global))).

% PERSPECTIVE 2: THE MACRO OBSERVER (SNARE)
% Analysts view this as a Snare: a trap where high current issuance 
% crowds out future investment and "snakes" around long-term growth.
constraint_indexing:constraint_classification(trillion_bond_rush_2026, snare, 
    context(agent_power(analytical), 
            time_horizon(civilizational), 
            exit_options(analytical), 
            spatial_scope(global))).

narrative_ontology:interval(trillion_bond_rush_2026, 0, 10).

/* ==========================================================================
   4. MANDATROPHY RESOLUTION HOOK
   ========================================================================== */

omega_variable(
    omega_liquidity_trap,
    'Will this issuance surge trigger a systemic liquidity trap in Q3?',
    'Yield curve inversion analysis and debt-service-to-GDP ratio modeling.',
    'Success (stability) maintains the Rope; failure hardens the Global Debt Snare.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   5. TEMPORAL MEASUREMENTS (RESOLVED SCHEMA: 3x2 Matrix)
   ========================================================================== */

% Metrics: extractiveness (modeling the rapid escalation of debt front-loading)
narrative_ontology:measurement(br_ex_t0, trillion_bond_rush_2026, extractiveness, 0, 0.20). % Jan 1, 2026
narrative_ontology:measurement(br_ex_t5, trillion_bond_rush_2026, extractiveness, 5, 0.35). % Mid-Jan 2026
narrative_ontology:measurement(br_ex_t10, trillion_bond_rush_2026, extractiveness, 10, 0.52). % Feb 2 Record

% Metrics: theater_ratio (reflecting the clinical, cost-focused nature of the rush)
narrative_ontology:measurement(br_tr_t0, trillion_bond_rush_2026, theater_ratio, 0, 0.05).
narrative_ontology:measurement(br_tr_t5, trillion_bond_rush_2026, theater_ratio, 5, 0.08).
narrative_ontology:measurement(br_tr_t10, trillion_bond_rush_2026, theater_ratio, 10, 0.10).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
