% ============================================================================
% CONSTRAINT STORY: notary_ink_dependency
% ============================================================================
% Version: 3.4 (Deferential Realism Core)
% Logic: 3.3 (Indexed Tuple P,T,E,S)
% Generated: 2026-01-28
% ============================================================================

:- module(constraint_notary_ink, []).

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
 * * constraint_id: notary_ink_dependency
 * human_readable: The Notary/Wet-Ink Persistence
 * domain: legal/institutional
 * * SUMMARY:
 * This constraint represents the mandatory requirement for physical 
 * presence and manual signatures for high-value legal documents. Despite 
 * the existence of superior cryptographic verification, the "Notary" 
 * remains a Piton: an inertial anchor used for psychological signaling 
 * and institutional risk-mitigation.
 * * KEY AGENTS:
 * - The Signatory: Subject (Powerless). Must travel and pay for a ritual.
 * - The Legal Institution: Beneficiary (Institutional). Relies on the 
 * tradition to manage liability and maintain professional gatekeeping.
 * - The Digital Auditor: Auditor (Analytical). Identifies the theater.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% [RESOLVED MANDATROPHY]: Extraction (0.35) is moderate, consisting of 
% "Friction Costs" (time, travel, fees). It is not predatory, but it is 
% no longer optimized for coordination.
domain_priors:base_extractiveness(notary_ink_dependency, 0.35). 
domain_priors:suppression_score(notary_ink_dependency, 0.88).   % High: Many documents are legally invalid without this specific ritual.
domain_priors:theater_ratio(notary_ink_dependency, 0.92).      % Very High: The primary value is now "performative" trust rather than technical security.

% Constraint metric facts (bridge for classification engine)
narrative_ontology:constraint_metric(notary_ink_dependency, extractiveness, 0.35).
narrative_ontology:constraint_metric(notary_ink_dependency, suppression_requirement, 0.88).
narrative_ontology:constraint_metric(notary_ink_dependency, theater_ratio, 0.92).
domain_priors:requires_active_enforcement(notary_ink_dependency).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE SIGNATORY (SNARE)
% For the individual, the need to find a physical notary during business 
% hours is a Snare—a low-utility friction that restricts movement.
constraint_indexing:constraint_classification(notary_ink_dependency, snare, 
    context(agent_power(powerless), 
            time_horizon(immediate), 
            exit_options(trapped), 
            spatial_scope(local))).

% PERSPECTIVE 2: THE BANKING SYSTEM (ROPE)
% Viewed institutionally, the notary is a Rope. It provides a shared 
% standard of "High-Trust" that coordinates risk across different jurisdictions.
constraint_indexing:constraint_classification(notary_ink_dependency, rope, 
    context(agent_power(institutional), 
            time_horizon(generational), 
            exit_options(mobile), 
            spatial_scope(national))).

% PERSPECTIVE 3: THE DIGITAL AUDITOR (PITON)
% Analytically, the notary is a Piton. It is an artifact of the 19th-century 
% coordination stack maintained solely by institutional habit.
constraint_indexing:constraint_classification(notary_ink_dependency, piton, 
    context(agent_power(analytical), 
            time_horizon(historical), 
            exit_options(arbitrage), 
            spatial_scope(global))) :-
    domain_priors:theater_ratio(notary_ink_dependency, TR), TR > 0.70.

% PERSPECTIVE 4: THE E-GOVERNANCE ADVOCATE (SCAFFOLD)
% In jurisdictions transitioning to digital-first, the notary is a 
% Scaffold—a bridge to keep the system running until DIDs are universal.
constraint_indexing:constraint_classification(notary_ink_dependency, scaffold, 
    context(agent_power(organized), 
            time_horizon(immediate), 
            exit_options(constrained), 
            spatial_scope(national))) :-
    narrative_ontology:has_sunset_clause(notary_ink_dependency).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(notary_ink_tests).

test(piton_detection) :-
    % Ensure the analytical perspective identifies this as a Piton due to theater_ratio.
    constraint_indexing:constraint_classification(notary_ink_dependency, piton, context(agent_power(analytical), _, _, _)).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(notary_ink_dependency, snare, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(notary_ink_dependency, rope, context(agent_power(institutional), _, _, _)).

:- end_tests(notary_ink_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The Notary Ink Dependency is a high-theater constraint ($TR = 0.92$). 
 * It functions as a Piton because its original "Rope" function (manual 
 * verification by a trusted witness) has been made obsolete by digital 
 * signatures, yet the institutional structure remains "hooked" into it. 
 * The Perspectival Gap exists because institutions confuse the "Ritual 
 * of Trust" with the "Mechanism of Security."
 *
 * MANDATROPHY ANALYSIS:
 * This represents "Inertial Mandatrophy." The coordination function 
 * (verifying identity) is still performed, but at a vastly higher cost 
 * than available alternatives, purely to preserve institutional tradition.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_psychological_deterrence,
    'Does the physical act of "wet ink" presence deter fraud more than digital signatures?',
    'Comparative study of fraud rates in e-notary vs. traditional notary states.',
    'If yes, the Piton is actually a Rope; if no, it is a pure Piton.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(notary_ink_dependency, 0, 1). % Presence of Notary requirement.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
