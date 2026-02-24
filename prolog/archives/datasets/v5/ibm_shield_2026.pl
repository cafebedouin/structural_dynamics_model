% ============================================================================
% CONSTRAINT STORY: ibm_shield_2026
% ============================================================================
% Version: 3.4 (Deferential Realism Core)
% Logic: 3.3 (Indexed Tuple P,T,E,S)
% Generated: 2026-02-05
% ============================================================================

:- module(constraint_ibm_shield_2026, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

% --- Namespace Hooks (Required for loading) ---
:- multifile
    domain_priors:base_extractiveness/2,
    domain_priors:suppression_score/2,
    domain_priors:theater_ratio/2,
    narrative_ontology:interval/3,
    narrative_ontology:constraint_metric/3,
    narrative_ontology:measurement/5,
    narrative_ontology:constraint_beneficiary/2,
    narrative_ontology:constraint_victim/2,
    constraint_indexing:constraint_classification/3.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * * constraint_id: ibm_shield_2026
 * human_readable: IBM SHIELD Program (MDA Contract)
 * domain: political/technological
 * * SUMMARY:
 * IBM's $151B SHIELD contract with the Missile Defense Agency operationalizes 
 * AI-enabled sensing. While providing national protection, the IDIQ 
 * structure and mission-grade security create a "Snare" of technological 
 * opacity for the public sector.
 * * KEY AGENTS:
 * - General Public: Subject (Powerless)
 * - IBM / MDA: Beneficiary (Institutional)
 * - Defense Auditors: Auditor (Analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Extraction is high (0.62) due to the massive capital allocation ($151B) 
% and proprietary AI governance.
domain_priors:base_extractiveness(ibm_shield_2026, 0.62). 

% Suppression is moderate-high (0.75) as "mission-grade security" 
% limits public oversight.
domain_priors:suppression_score(ibm_shield_2026, 0.75).   

% Theater ratio is low (0.20) because the contract focuses on 
% rapid, functional delivery to the warfighter.
domain_priors:theater_ratio(ibm_shield_2026, 0.20).       

% Constraint metric facts (bridge for classification engine)
narrative_ontology:constraint_metric(ibm_shield_2026, extractiveness, 0.62).
narrative_ontology:constraint_metric(ibm_shield_2026, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(ibm_shield_2026, theater_ratio, 0.2).

narrative_ontology:constraint_beneficiary(ibm_shield_2026, ibm_defense_division).
narrative_ontology:constraint_victim(ibm_shield_2026, public_budgetary_transparency).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE SUBJECT (SNARE)
% The public sees the SHIELD program as a Snare: a massive, 
% opaque financial commitment they cannot exit or influence.
constraint_indexing:constraint_classification(ibm_shield_2026, snare, 
    context(agent_power(powerless), 
            time_horizon(generational), 
            exit_options(trapped), 
            spatial_scope(national))).

% PERSPECTIVE 2: THE BENEFICIARY (ROPE)
% MDA and IBM view this as a Rope—essential coordination of 
% sensing and AI to ensure national survival.
constraint_indexing:constraint_classification(ibm_shield_2026, rope, 
    context(agent_power(institutional), 
            time_horizon(generational), 
            exit_options(mobile), 
            spatial_scope(national))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% Analysts see a Tangled Rope: Genuine existential security 
% coordination mixed with high rent-extraction from the IDIQ structure.
constraint_indexing:constraint_classification(ibm_shield_2026, tangled_rope,
    context(agent_power(analytical),
            time_horizon(historical),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ibm_shield_2026_tests).

test(extraction_threshold) :-
    domain_priors:base_extractiveness(ibm_shield_2026, E),
    E > 0.46.

test(functional_validity) :-
    domain_priors:theater_ratio(ibm_shield_2026, TR),
    TR < 0.30.

:- end_tests(ibm_shield_2026_tests).

/* ==========================================================================
   5. OMEGA VARIABLES (Ω)
   ========================================================================= */

omega_variable(
    omega_shield_2026,
    'Will "mission-grade security" prevent effective civilian AI governance?',
    'Review of built-in governance protocols by non-defense agencies.',
    'Success implies a Rope; failure confirms a permanent Snare for AI ethics.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   6. INTEGRATION HOOKS & TEMPORAL DATA
   ========================================================================== */

narrative_ontology:interval(ibm_shield_2026, 0, 10).

% Drift: From initial award (T=0) to full operationalization (T=10).
narrative_ontology:measurement(sh_ex_t0, ibm_shield_2026, base_extractiveness, 0, 0.20).
narrative_ontology:measurement(sh_ex_t5, ibm_shield_2026, base_extractiveness, 5, 0.45).
narrative_ontology:measurement(sh_ex_t10, ibm_shield_2026, base_extractiveness, 10, 0.62).
