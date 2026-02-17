% ============================================================================
% CONSTRAINT STORY: usc_26_469_passive_loss
% ============================================================================
% Version: 5.2 (Deferential Realism Core + Boltzmann + Purity + Network)
% Logic: 5.2 (Indexed Tuple P,T,E,S + Coupling + Purity + Network Drift)
% Generated: 2024-07-27
% ============================================================================

:- module(constraint_usc_26_469_passive_loss, []).

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
    narrative_ontology:human_readable/2.

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
 * - Small-scale Landlord / Passive Investor: Subject (Powerless). Trapped by suspended losses.
 * - Internal Revenue Service / US Treasury: Beneficiary (Institutional). Protects tax base
 * against "tax shelters."
 * - Tax Auditor / Policy Analyst: Auditor (Analytical). Tracks the drift from policy intent
 * to administrative complexity.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% High extraction (0.58) because losses are effectively "confiscated" by deferral
% until a future, undetermined date of sale, creating a significant time-value
% cost.
domain_priors:base_extractiveness(usc_26_469_passive_loss, 0.58).

% High suppression (0.85); no legal alternative exists for the taxpayer to
% recognize these economic losses against other income within the US jurisdiction.
domain_priors:suppression_score(usc_26_469_passive_loss, 0.85).

% Low-moderate theater ratio; the law is highly functional but contains
% "performative" complexity in "Material Participation" testing.
domain_priors:theater_ratio(usc_26_469_passive_loss, 0.25).

% Constraint metric facts — primary keys used by the classification engine.
narrative_ontology:constraint_metric(usc_26_469_passive_loss, extractiveness, 0.58).
narrative_ontology:constraint_metric(usc_26_469_passive_loss, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(usc_26_469_passive_loss, theater_ratio, 0.25).

% The law claims to be a coordination mechanism to ensure tax fairness.
narrative_ontology:constraint_claim(usc_26_469_passive_loss, tangled_rope).
narrative_ontology:human_readable(usc_26_469_passive_loss, "Passive Activity Loss (PAL) Rules").

% Binary flags and structural properties for Tangled Rope classification.
domain_priors:requires_active_enforcement(usc_26_469_passive_loss).
narrative_ontology:constraint_beneficiary(usc_26_469_passive_loss, us_treasury).
narrative_ontology:constraint_victim(usc_26_469_passive_loss, passive_activity_investors).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × π(P) × σ(S)
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
% small-business failures (Asymmetric Extraction).
constraint_indexing:constraint_classification(usc_26_469_passive_loss, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(usc_26_469_passive_loss_tests).

test(perspectival_gap) :-
    % Verify there is a perspectival gap between powerless and institutional.
    constraint_indexing:constraint_classification(usc_26_469_passive_loss, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(usc_26_469_passive_loss, TypeInstitutional, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeInstitutional,
    TypePowerless == snare,
    TypeInstitutional == rope.

test(analytical_classification_is_tangled_rope) :-
    % Verify the analytical observer correctly identifies the hybrid nature.
    constraint_indexing:constraint_classification(usc_26_469_passive_loss, tangled_rope, context(agent_power(analytical), _, _, _)).

test(extraction_threshold_for_high_extraction_rules) :-
    narrative_ontology:constraint_metric(usc_26_469_passive_loss, extractiveness, E),
    E >= 0.46.

:- end_tests(usc_26_469_passive_loss_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The Perspectival Gap is stark. For the Subject, money is fungible, so the
 * "walling off" of losses feels like an arbitrary Snare confiscating a real
 * economic loss. For the Beneficiary (the state), this wall is a necessary
 * Rope to prevent systemic extraction of public funds by private tax-arbitrageurs
 * creating artificial losses. The Tangled Rope classification is essential
 * because the constraint possesses both a genuine, system-preserving coordination
 * function AND a clear, asymmetrically extractive effect on a specific class
 * of investor.
 *
 * [RESOLVED MANDATROPHY]
 * The high extraction (0.58) is not a result of "atrophy" but of a
 * deliberate legislative choice to prioritize systemic tax base stability over
 * individual investor liquidity. The system functions as designed, albeit harshly.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    omega_469_participation,
    'Is the 750-hour "Material Participation" threshold a functional filter for active business engagement or a theatrical barrier designed to be difficult to meet?',
    'Analysis of audit outcomes vs. actual business success in the Real Estate Professional category.',
    'If theatrical: Section 469 drifts toward Piton, as its exceptions become performative. If functional: it remains a Tangled Rope.',
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
   9. BOLTZMANN & NETWORK DATA (v5.0-5.2)
   ========================================================================== */

% This law is a core part of the tax code's enforcement apparatus.
narrative_ontology:coordination_type(usc_26_469_passive_loss, enforcement_mechanism).

% The disposition of a passive activity, which releases suspended losses,
% is often a capital transaction, structurally linking this rule to capital gains.
narrative_ontology:affects_constraint(usc_26_469_passive_loss, capital_gains_taxation).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */