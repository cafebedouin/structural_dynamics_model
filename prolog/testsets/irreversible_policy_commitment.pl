% ============================================================================
% CONSTRAINT STORY: irreversible_policy_commitment
% ============================================================================
% Version: 5.2 (Deferential Realism Core + Boltzmann + Purity + Network)
% Logic: 5.2 (Indexed Tuple P,T,E,S + Coupling + Purity + Network Drift)
% Generated: 2024-07-20
% ============================================================================

:- module(constraint_irreversible_policy_commitment, []).

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
    constraint_indexing:constraint_classification/3.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * * constraint_id: irreversible_policy_commitment
 * human_readable: The Burned Bridge Protocol
 * domain: political/economic
 * * SUMMARY:
 * This constraint represents a policy decision that, once implemented,
 * permanently alters the institutional landscape or social contract such
 * that the cost of reversal is effectively infinite. It functions as a
 * Rope for immediate coordination but settles into a Snare or Mountain for all
 * future generations, who inherit the path without the leverage to pivot.
 * * KEY AGENTS:
 * - Future Citizen: Subject (Powerless)
 * - Founding Negotiator: Beneficiary (Institutional)
 * - Constitutional Auditor: Auditor (Analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% High extraction (0.72) because the commitment siphons future optionality
% to secure present-day stability or political wins.
domain_priors:base_extractiveness(irreversible_policy_commitment, 0.72).
domain_priors:suppression_score(irreversible_policy_commitment, 0.85). % Alternatives are structurally/legally annihilated.
domain_priors:theater_ratio(irreversible_policy_commitment, 0.35).    % Low theater; the irreversibility is functional and hard-coded.

% Constraint metric facts — primary keys used by the classification engine.
narrative_ontology:constraint_metric(irreversible_policy_commitment, extractiveness, 0.72).
narrative_ontology:constraint_metric(irreversible_policy_commitment, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(irreversible_policy_commitment, theater_ratio, 0.35).

% Constraint self-claim (what does the constraint claim to be?)
narrative_ontology:constraint_claim(irreversible_policy_commitment, tangled_rope).

% Binary flags & Structural properties
domain_priors:requires_active_enforcement(irreversible_policy_commitment). % Required for Tangled Rope

% Structural property derivation hooks:
narrative_ontology:constraint_beneficiary(irreversible_policy_commitment, founding_negotiators).
narrative_ontology:constraint_victim(irreversible_policy_commitment, future_citizens).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × π(P) × σ(S)
   ========================================================================== */

% PERSPECTIVE 1: THE SUBJECT (SNARE)
% To the future citizen, the policy is a predatory trap. The high base
% extraction (0.72) is amplified by their powerlessness (π=1.5), resulting
% in an effective extraction χ > 1.0. It feels like a Mountain but is
% structurally a Snare.
constraint_indexing:constraint_classification(irreversible_policy_commitment, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE BENEFICIARY (ROPE)
% The founding institution views this as a vital Rope for long-term coordination
% and credibility. Their institutional power (π=-0.2) inverts the extraction,
% making it feel beneficial.
constraint_indexing:constraint_classification(irreversible_policy_commitment, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% The auditor recognizes both the coordination function (beneficiaries exist)
% and the severe asymmetric extraction (victims exist), alongside the need for
% active enforcement. This hybrid nature is classified as a Tangled Rope.
constraint_indexing:constraint_classification(irreversible_policy_commitment, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(irreversible_policy_commitment_tests).

test(perspectival_gap) :-
    % Verify the "Inherited" Snare vs the "Founding" Rope gap.
    constraint_indexing:constraint_classification(irreversible_policy_commitment, snare,
        context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(irreversible_policy_commitment, rope,
        context(agent_power(institutional), _, _, _)),
    constraint_indexing:constraint_classification(irreversible_policy_commitment, tangled_rope,
        context(agent_power(analytical), _, _, _)).

test(threshold_validation) :-
    % Ensure extraction meets the high-extraction core limits (> 0.46).
    domain_priors:base_extractiveness(irreversible_policy_commitment, E),
    E >= 0.46.

:- end_tests(irreversible_policy_commitment_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The extraction score (0.72) represents the 'Mandatrophy' threshold where the
 * value of coordination is purchased by permanently liquidating the optionality
 * of all future participants. The suppression score (0.85) reflects the
 * structural annihilation of alternative paths.
 * * PERSPECTIVAL GAP:
 * The gap is stark: the Founding Negotiator (institutional) sees a Rope because
 * they are actively using the constraint to coordinate and their power inverts
 * the perceived extraction. The Future Citizen (powerless) inherits a Snare,
 * experiencing coercive extraction of their agency with no mechanism to
 * renegotiate the terms.
 * * [RESOLVED MANDATROPHY]:
 * This is resolved by the Tangled Rope classification at the civilizational level.
 * It acknowledges the coordination benefit (stability) while explicitly flagging
 * the 0.72 extraction of agency as a predatory structural outcome, preventing
 * misclassification as either a pure Rope or pure Snare.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% Required as extraction (0.72) > 0.46.
omega_variable(
    omega_reversal_cost,
    'Is the irreversibility a physical limit (Mountain) or a legal fiction (Snare)?',
    'Historical analysis of "Black Swan" events that previously broke similar commitments.',
    'If crisis breaks it: Snare. If crisis confirms it: Mountain of History.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(irreversible_policy_commitment, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Temporal data for high-extraction constraint (0.72 > 0.46).
% Models the solidification of the policy: extraction grows as future
% optionality is visibly lost, and theater rises slightly as the original
% functional justification becomes historical memory.

% Theater ratio over time:
narrative_ontology:measurement(ipc_tr_t0, irreversible_policy_commitment, theater_ratio, 0, 0.10).
narrative_ontology:measurement(ipc_tr_t5, irreversible_policy_commitment, theater_ratio, 5, 0.25).
narrative_ontology:measurement(ipc_tr_t10, irreversible_policy_commitment, theater_ratio, 10, 0.35).

% Extraction over time:
narrative_ontology:measurement(ipc_ex_t0, irreversible_policy_commitment, base_extractiveness, 0, 0.50).
narrative_ontology:measurement(ipc_ex_t5, irreversible_policy_commitment, base_extractiveness, 5, 0.65).
narrative_ontology:measurement(ipc_ex_t10, irreversible_policy_commitment, base_extractiveness, 10, 0.72).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA (v5.0-5.2)
   ========================================================================== */

% This policy acts as a long-term enforcement mechanism, locking in a
% particular institutional path.
narrative_ontology:coordination_type(irreversible_policy_commitment, enforcement_mechanism).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */