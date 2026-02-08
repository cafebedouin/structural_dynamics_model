% ============================================================================
% CONSTRAINT STORY: digital_identity_tether
% ============================================================================
% Version: 3.4 (Deferential Realism Core)
% Logic: 3.3 (Indexed Tuple P,T,E,S)
% Generated: 2026-01-28
% ============================================================================

:- module(constraint_identity_tether, []).

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
    narrative_ontology:constraint_metric/3,
    constraint_indexing:constraint_classification/3.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * * constraint_id: digital_identity_tether
 * human_readable: The Centralized Identity Anchor
 * domain: digital_socio_technical
 * * SUMMARY:
 * This constraint defines the inability of a digital subject to decouple 
 * their reputation, social graph, and authentication from a primary 
 * provider (the "Identity Anchor"). Initially a convenience (Rope), 
 * it becomes a tool of extraction as the cost of exit approaches total 
 * social/professional erasure.
 * * KEY AGENTS:
 * - The Freelancer: Subject (Powerless). Reputation is locked to the platform.
 * - The Platform Sovereign: Beneficiary (Institutional). Owns the namespace.
 * - The Protocol Architect: Auditor (Analytical). Evaluates interoperability.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% [RESOLVED MANDATROPHY]: Extraction (0.58) is driven by the "Tax on Exit." 
% The platform extracts behavioral data and loyalty by threatening identity-death.
domain_priors:base_extractiveness(digital_identity_tether, 0.58). 
domain_priors:suppression_score(digital_identity_tether, 0.92).   % Alternatives exist but lack network effects.
domain_priors:theater_ratio(digital_identity_tether, 0.35).      % Low: The constraint is functional and active, not yet a Piton.

% Constraint metric facts (bridge for classification engine)
narrative_ontology:constraint_metric(digital_identity_tether, extractiveness, 0.58).
narrative_ontology:constraint_metric(digital_identity_tether, suppression_requirement, 0.92).
narrative_ontology:constraint_metric(digital_identity_tether, theater_ratio, 0.35).
domain_priors:requires_active_enforcement(digital_identity_tether).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE FREELANCER (SNARE)
% To the user, the identity is a trap; leaving means losing 10 years of 
% verified work history and social connections.
constraint_indexing:constraint_classification(digital_identity_tether, snare, 
    context(agent_power(powerless), 
            time_horizon(biographical), 
            exit_options(trapped), 
            spatial_scope(global))).

% PERSPECTIVE 2: THE PLATFORM SOVEREIGN (ROPE)
% Viewed as a vital security coordination service (Anti-Sybil/KYC).
constraint_indexing:constraint_classification(digital_identity_tether, rope, 
    context(agent_power(institutional), 
            time_horizon(generational), 
            exit_options(mobile), 
            spatial_scope(global))).

% PERSPECTIVE 3: THE PROTOCOL ARCHITECT (SCAFFOLD)
% If the platform promises future interoperability or "Data Portability,"
% it is viewed as a temporary support system for a maturing web.
constraint_indexing:constraint_classification(digital_identity_tether, scaffold, 
    context(agent_power(analytical), 
            time_horizon(historical), 
            exit_options(analytical), 
            spatial_scope(global))) :-
    narrative_ontology:has_sunset_clause(digital_identity_tether).

% PERSPECTIVE 4: THE MARKET ANALYST (TANGLED ROPE)
% Detects the hybrid nature where the "Single Sign-On" convenience (Rope)
% is the mechanism that delivers the user into the data-harvesting silo (Snare).
constraint_indexing:constraint_classification(digital_identity_tether, tangled_rope, 
    context(agent_power(analytical), 
            time_horizon(historical), 
            exit_options(arbitrage), 
            spatial_scope(global))) :-
    domain_priors:base_extractiveness(digital_identity_tether, E), E > 0.46.

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(digital_identity_tether_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(digital_identity_tether, snare, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(digital_identity_tether, rope, context(agent_power(institutional), _, _, _)).

test(extraction_threshold) :-
    domain_priors:base_extractiveness(digital_identity_tether, E),

    E >= 0.46. % Validates high extraction for Snare/Tangled logic.

:- end_tests(digital_identity_tether_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The identity tether is highly "sticky" due to the Suppression Score (0.92). 
 * While technical alternatives (DIDs/Self-Sovereign Identity) exist, the 
 * "Social Exit Cost" remains the primary enforcement mechanism. 
 * The Perspectival Gap exists because the Platform views the tether as 
 * "Safety/Coordination," while the User (once established) views it as 
 * "Captivity/Extraction."
 *
 * MANDATROPHY ANALYSIS:
 * This case is a "Tangled Rope" because the coordination benefit (trust) 
 * is the source of the extraction potential. You cannot have the 
 * reputation (Rope) without the silo (Snare).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_portability_legitimacy,
    'Is "Data Portability" a genuine Sunset Clause or a Theatrical facade?',
    'Analysis of successful user migrations vs. technical friction logs.',
    'If genuine, it is a Scaffold; if theatrical, it is a Piton.',
    confidence_without_resolution(low)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(digital_identity_tether, 0, 1). % Binary: Tethered or Interoperable.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
