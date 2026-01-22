% ============================================================================
% CONSTRAINT STORY: social_credit_architecture
% ============================================================================
% Generated: 2026-01-21
% Model: Gemini 2.0 Flash
% Source: Global Social Credit & Algorithmic Governance
% Status: [RESOLVED MANDATROPHY]
% ============================================================================

:- module(constraint_social_credit, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

% --- Namespace Hooks (Required for loading) ---
:- multifile 
    domain_priors:base_extractiveness/2,
    domain_priors:suppression_score/2,
    domain_priors:requires_active_enforcement/1,
    constraint_indexing:constraint_classification/3.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * * constraint_id: social_credit_architecture
 * human_readable: The Social Credit Architecture
 * domain: economic/social/political
 * * SUMMARY:
 * A system that operationalizes "trustworthiness" by decoding behavioral data 
 * into indicators. It creates real-world consequences based on 
 * compliance with state-sanctioned norms.
 */

/* ==========================================================================
   2. CORE SYSTEM INTEGRATION (The "Reality" Layer)
   ========================================================================== */

narrative_ontology:interval(scs_operational_2026, 0, 10).
narrative_ontology:constraint_claim(social_credit_architecture, rope).

% Base extractiveness score (0.8)
% Rationale: High potential for coercive control; extracts political/social 
% conformity as the price of mobility.
domain_priors:base_extractiveness(social_credit_architecture, 0.8).

% Suppression score (0.7)
% Rationale: Mandatory digital integration makes exit into informal networks 
% difficult.
domain_priors:suppression_score(social_credit_architecture, 0.7).

domain_priors:requires_active_enforcement(social_credit_architecture).

% Metrics required for Section 1
narrative_ontology:constraint_metric(social_credit_architecture, extractiveness, 0.8).
narrative_ontology:constraint_metric(social_credit_architecture, suppression_requirement, 0.7).

% BENEFICIARIES & VICTIMS
constraint_beneficiary(social_credit_architecture, [state_governance, high_score_elites]).
constraint_victim(social_credit_architecture, [political_dissenters, debt_defaulters]).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: The Blacklisted Defaulter - Noose
   -------------------------------------------------------------------------- */
constraint_indexing:constraint_classification(
    social_credit_architecture,
    noose,
    context(
        agent_power(individual_powerless),
        time_horizon(biographical),
        exit_options(trapped),
        spatial_scope(national)
    )
) :-
    domain_priors:base_extractiveness(social_credit_architecture, E),
    E > 0.7, % High extraction + trapped = Noose
    !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: The Model Citizen - Rope
   -------------------------------------------------------------------------- */
constraint_indexing:constraint_classification(
    social_credit_architecture,
    rope,
    context(
        agent_power(individual_moderate),
        time_horizon(immediate),
        exit_options(mobile),
        spatial_scope(local)
    )
) :-
    !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: The State Architect - Mountain
   -------------------------------------------------------------------------- */
constraint_indexing:constraint_classification(
    social_credit_architecture,
    mountain,
    context(
        agent_power(institutional),
        time_horizon(generational),
        exit_options(arbitrage),
        spatial_scope(global)
    )
) :-
    !.

/* ==========================================================================
   4. TESTS (Mandatrophy Resolution)
   ========================================================================== */

:- begin_tests(social_credit_tests).

test(multi_perspective_variance) :-
    % Verify high extraction (0.8) is resolved via indexical shift
    constraint_indexing:constraint_classification(social_credit_architecture, Type1, 
        context(individual_powerless, biographical, trapped, national)),
    constraint_indexing:constraint_classification(social_credit_architecture, Type2, 
        context(individual_moderate, immediate, mobile, local)),
    Type1 = noose,
    Type2 = rope.

test(mandatrophy_validation) :-
    % Ensure the extraction score is correctly captured
    domain_priors:base_extractiveness(social_credit_architecture, E),
    E == 0.8.

:- end_tests(social_credit_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION (Commentary)
   ========================================================================== */

/**
 * LLM GENERATION NOTES
 * * [RESOLVED MANDATROPHY]: The 0.8 extraction score represents the systemic 
 * price of conformity. By indexing this as a NOOSE for the blacklisted, 
 * we demonstrate how the system uses "joint punishment" (travel bans, job exclusion) 
 * to coerce behavior. For high-scoring agents, the ROPE classification 
 * reflects the convenience and administrative perks offered by the system.
 * * OMEGAS:
 * omega_variable(dissident_visibility,
 * "Is the system primarily for debt enforcement or political repression?",
 * resolution_mechanism("Analyze the ratio of financial defaulters vs dissenters on blacklists"),
 * impact("Affects the human-readable classification of intent."),
 * confidence_without_resolution(low)
 * ).
 */

/* ==========================================================================
   6. INTEGRATION HOOKS
   ========================================================================== */

/**
 * TO USE:
 * ?- [social_credit_architecture].
 * ?- run_tests(social_credit_tests).
 */
