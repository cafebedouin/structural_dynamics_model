% ============================================================================
% CONSTRAINT STORY: usc_26_469_passive_loss
% ============================================================================
% Version: 3.4 (Deferential Realism Core)
% Logic: 3.3 (Indexed Tuple P,T,E,S)
% Generated: 2026-02-02
% Scenario: 26 U.S. Code § 469 - Passive Activity Loss Limitations
% ============================================================================

:- module(constraint_usc_26_469_passive_loss, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

% --- Namespace Hooks ---
:- multifile
    domain_priors:base_extractiveness/2,
    domain_priors:suppression_score/2,
    domain_priors:theater_ratio/2,
    domain_priors:requires_active_enforcement/1,
    narrative_ontology:has_sunset_clause/1,
    narrative_ontology:constraint_metric/3,
    narrative_ontology:interval/3,
    narrative_ontology:measurement/5,
    constraint_indexing:constraint_classification/3.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * * constraint_id: usc_26_469_passive_loss
 * human_readable: Passive Activity Loss (PAL) Rules
 * domain: economic/legal
 * * SUMMARY:
 * Established by the Tax Reform Act of 1986, Section 469 prohibits taxpayers 
 * from using losses from "passive activities" (rental properties or businesses 
 * without material participation) to offset active income (wages) or portfolio 
 * income. It creates a "walled garden" for losses that can only be exited 
 * upon the total disposition of the activity.
 * * KEY AGENTS:
 * - Small-scale Landlord: Subject (Powerless). Trapped by suspended losses.
 * - Internal Revenue Service: Beneficiary (Institutional). Protects tax base 
 * against "tax shelters."
 * - Tax Auditor: Auditor (Analytical). Tracks the drift from policy intent 
 * to administrative complexity.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% High extraction (0.58) because losses are effectively "confiscated" until 
% a future undetermined date of sale.
domain_priors:base_extractiveness(usc_26_469_passive_loss, 0.58). 

% High suppression (0.85) scaled by National scope; no legal alternative 
% exists for the taxpayer within the US jurisdiction.
domain_priors:suppression_score(usc_26_469_passive_loss, 0.85).   

% Low-moderate theater ratio; the law is highly functional but contains 
% "performative" complexity in "Material Participation" testing.
domain_priors:theater_ratio(usc_26_469_passive_loss, 0.25).       

% Constraint metric facts (bridge for classification engine)
narrative_ontology:constraint_metric(usc_26_469_passive_loss, extractiveness, 0.58).
narrative_ontology:constraint_metric(usc_26_469_passive_loss, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(usc_26_469_passive_loss, theater_ratio, 0.25).

domain_priors:requires_active_enforcement(usc_26_469_passive_loss).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE SUBJECT (SNARE)
% The individual investor perceives the disallowance as a predatory trap. 
% Losses are real cash-out-the-door, but the deduction is "snared" by the code.
constraint_indexing:constraint_classification(usc_26_469_passive_loss, snare, 
    context(agent_power(powerless), 
            time_horizon(biographical), 
            exit_options(trapped), 
            spatial_scope(national))).

% PERSPECTIVE 2: THE BENEFICIARY (ROPE)
% From the IRS perspective, Section 469 is essential coordination. It prevents 
% a "race to the bottom" where the wealthy use artificial paper losses to 
% zero out their tax liability, ensuring the "Rope" of the tax base holds.
constraint_indexing:constraint_classification(usc_26_469_passive_loss, rope, 
    context(agent_power(institutional), 
            time_horizon(generational), 
            exit_options(mobile), 
            spatial_scope(national))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% The auditor recognizes that while it solves the "tax shelter" problem (Coordination), 
% it also extracts high compliance costs and penalizes legitimate 
% small-business failures (Extraction).
constraint_indexing:constraint_classification(usc_26_469_passive_loss, tangled_rope, 
    context(agent_power(analytical), 
            time_horizon(historical), 
            exit_options(analytical), 
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(usc_26_469_passive_loss_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(usc_26_469_passive_loss, snare, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(usc_26_469_passive_loss, rope, context(agent_power(institutional), _, _, _)).

test(extraction_threshold) :-
    domain_priors:base_extractiveness(usc_26_469_passive_loss, E),

    E >= 0.46.

:- end_tests(usc_26_469_passive_loss_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The Perspectival Gap exists because Section 469 treats "Passive Activity" 
 * as a distinct ontological category of income. For the Subject, money is 
 * fungible, so the "walling off" of losses feels like an arbitrary Snare. 
 * For the Beneficiary, the wall is a necessary "Rope" to prevent the 
 * systemic extraction of public funds by private tax-arbitrageurs.
 *
 * [RESOLVED MANDATROPHY]
 * The high extraction (0.58) is not a result of "atrophy" but of a 
 * deliberate legislative choice to prioritize systemic stability over 
 * individual liquidity.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    omega_469_participation,
    'Is the 750-hour "Material Participation" threshold a functional filter or a theatrical barrier?',
    'Analysis of audit outcomes vs. actual business success in the Real Estate Professional category.',
    'If theatrical: Section 469 drifts toward Piton. If functional: it remains a Tangled Rope.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(usc_26_469_passive_loss, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time (Goodhart drift from simple rule to complex "Participation" games)
narrative_ontology:measurement(usc_469_tr_t0, usc_26_469_passive_loss, theater_ratio, 0, 0.10).
narrative_ontology:measurement(usc_469_tr_t5, usc_26_469_passive_loss, theater_ratio, 5, 0.18).
narrative_ontology:measurement(usc_469_tr_t10, usc_26_469_passive_loss, theater_ratio, 10, 0.25).

% Extraction over time (Increased accumulation of suspended losses in the economy)
narrative_ontology:measurement(usc_469_ex_t0, usc_26_469_passive_loss, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(usc_469_ex_t5, usc_26_469_passive_loss, base_extractiveness, 5, 0.52).
narrative_ontology:measurement(usc_469_ex_t10, usc_26_469_passive_loss, base_extractiveness, 10, 0.58).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */