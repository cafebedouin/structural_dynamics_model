% ============================================================================
% CONSTRAINT STORY: power_without_responsibility
% ============================================================================
% Version: 5.2 (Deferential Realism Core + Boltzmann + Purity + Network)
% Logic: 5.2 (Indexed Tuple P,T,E,S + Coupling + Purity + Network Drift)
% Generated: 2024-07-22
% ============================================================================

:- module(constraint_power_without_responsibility, []).

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
 * * constraint_id: power_without_responsibility
 * human_readable: The Asymmetric Mandate
 * domain: political/organizational/legal
 * * SUMMARY:
 * A scenario where a "Rope" designed to grant an agent the authority to manage
 * high-stakes systems (e.g., emergency powers, automated enforcement, or
 * sovereign immunity) lacks a corresponding accountability mechanism.
 * This coordination substrate becomes a "Snare" for the subject, as the
 * agent's power is used to liquidate the subject's primary rights or assets
 * without legal recourse, trapping the user in a territory of state or
 * institutional whim where the "protector" cannot be held responsible
 * for the harms they inflict.
 * * KEY AGENTS:
 * - Unprotected Citizen: Subject (Powerless)
 * - Immune Authority: Beneficiary (Institutional)
 * - Governance Integrity Auditor: Auditor (Analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% High extraction (0.92) reflects the terminal liquidation of the subject's
% legal agency to maintain the authority's "decisive" operational Rope.
domain_priors:base_extractiveness(power_without_responsibility, 0.92).
domain_priors:suppression_score(power_without_responsibility, 0.85). % Alternative legal or social recourse is suppressed by the immunity mandate.
domain_priors:theater_ratio(power_without_responsibility, 0.94).    % Extreme theater: "Oversight Committees" that lack the legal power to issue binding judgments.

% Constraint metric facts — primary keys used by the classification engine.
narrative_ontology:constraint_metric(power_without_responsibility, extractiveness, 0.92).
narrative_ontology:constraint_metric(power_without_responsibility, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(power_without_responsibility, theater_ratio, 0.94).

% Constraint self-claim (what does the constraint claim to be?)
narrative_ontology:constraint_claim(power_without_responsibility, tangled_rope).
narrative_ontology:human_readable(power_without_responsibility, "The Asymmetric Mandate").

% Binary flags and structural properties for Tangled Rope classification
domain_priors:requires_active_enforcement(power_without_responsibility).
narrative_ontology:constraint_beneficiary(power_without_responsibility, immune_authority).
narrative_ontology:constraint_victim(power_without_responsibility, unprotected_citizen).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × π(P) × σ(S)
   ========================================================================== */

% PERSPECTIVE 1: THE SUBJECT (SNARE)
% The citizen is trapped: they are subject to the authority's power, but
% the lack of responsibility liquidates their primary defensive agency.
constraint_indexing:constraint_classification(power_without_responsibility, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE BENEFICIARY (ROPE)
% The authority views the immunity as a Rope—the essential coordination
% substrate for taking high-risk, high-speed actions without the friction of litigation.
constraint_indexing:constraint_classification(power_without_responsibility, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 3: THE SYSTEMS AUDITOR (PITON)
% Theater ratio (0.94) > 0.70 triggers Piton: the "Official Ethics Charter"
% is an inertial spike; it performatively signals responsibility while 0.92 extraction occurs.
constraint_indexing:constraint_classification(power_without_responsibility, piton,
    context(agent_power(analytical),
            time_horizon(historical),
            exit_options(analytical),
            spatial_scope(global))) :-
    domain_priors:theater_ratio(power_without_responsibility, TR), TR > 0.70.

% PERSPECTIVE 4: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% Detects high extraction (0.92) masking as essential coordination (Rope).
constraint_indexing:constraint_classification(power_without_responsibility, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(power_without_responsibility_tests).

test(perspectival_gap_snare_vs_rope) :-
    % Verify the core conflict: powerless sees a Snare, institutional sees a Rope.
    constraint_indexing:constraint_classification(power_without_responsibility, snare, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(power_without_responsibility, rope, context(agent_power(institutional), _, _, _)).

test(analytical_classification_piton) :-
    % Verify that high theater ratio correctly triggers a Piton classification for an analytical observer.
    domain_priors:theater_ratio(power_without_responsibility, TR),
    ( TR > 0.70 ->
        constraint_indexing:constraint_classification(power_without_responsibility, piton, context(agent_power(analytical), _, _, _))
    ; TR =< 0.70 ->
        \+ constraint_indexing:constraint_classification(power_without_responsibility, piton, context(agent_power(analytical), _, _, _))
    ).

test(analytical_classification_tangled_rope) :-
    % Verify that the combination of high extraction, suppression, and structural properties
    % correctly triggers a Tangled Rope classification.
    constraint_indexing:constraint_classification(power_without_responsibility, tangled_rope, context(agent_power(analytical), time_horizon(civilizational), _, _)).

:- end_tests(power_without_responsibility_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The extraction score (0.92) reflects a "Mandatrophy" state where the
 * claimed "coordination" benefit of decisive authority is achieved by liquidating
 * the subject's primary capacity for legal redress. The high suppression (0.85)
 * and theater (0.94) model a system where accountability mechanisms are
 * performative ("Independent Review Boards" with no power) and alternatives
 * are legally foreclosed.
 *
 * PERSPECTIVAL GAP:
 * The Unprotected Citizen feels a Snare because they have no shield against
 * systemic abuse. The Immune Authority sees a Rope because the lack of
 * responsibility coordinates a perfectly efficient and unhindered exercise
 * of power in high-stakes environments.
 *
 * MANDATROPHY ANALYSIS:
 * [RESOLVED MANDATROPHY]
 * The high extraction (0.92) risks a simplistic "Snare" classification that
 * would miss the constraint's dual nature. Mandatrophy is resolved by the
 * system's ability to hold multiple classifications. The Tangled Rope
 * classification correctly identifies that a genuine (if corrupted) coordination
 * function exists for the beneficiary, while the Piton classification correctly
 * identifies the accountability mechanism as inert and theatrical. This prevents
 * the system from mislabeling the coordination claim as pure fiction, instead
 * showing how it has been weaponized through extractive asymmetry.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% Required for high-extraction constraints (> 0.46).
omega_variable(
    omega_pwr_redress_recapture,
    'Can "Smart-Contract Penalties" restore the Rope, or is immunity a physical law of sovereignty (Snare vs Mountain)?',
    'Tracking the success rate of cryptographic bond-forfeiture in local governance 2026.',
    'If bonds restore agency: Snare of current legal design. If they fail: Mountain of Political Force.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(power_without_responsibility, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Temporal data models the degradation of the accountability mechanism over time.
% Initially, oversight had some function (low theater) and extraction was lower.
% Over the interval, the constraint decayed into a purely extractive system
% with performative oversight.

% Theater ratio over time (triggers metric_substitution detection):
narrative_ontology:measurement(pwr_tr_t0, power_without_responsibility, theater_ratio, 0, 0.20).
narrative_ontology:measurement(pwr_tr_t5, power_without_responsibility, theater_ratio, 5, 0.60).
narrative_ontology:measurement(pwr_tr_t10, power_without_responsibility, theater_ratio, 10, 0.94).

% Extraction over time (triggers extraction_accumulation detection):
narrative_ontology:measurement(pwr_ex_t0, power_without_responsibility, base_extractiveness, 0, 0.40).
narrative_ontology:measurement(pwr_ex_t5, power_without_responsibility, base_extractiveness, 5, 0.75).
narrative_ontology:measurement(pwr_ex_t10, power_without_responsibility, base_extractiveness, 10, 0.92).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA (v5.0-5.2)
   ========================================================================== */

% The constraint's claimed function is to provide a mechanism for decisive enforcement.
narrative_ontology:coordination_type(power_without_responsibility, enforcement_mechanism).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */