% ============================================================================
% CONSTRAINT STORY: structural_extraction_without_actor
% ============================================================================
% Version: 5.2 (Deferential Realism Core + Boltzmann + Purity + Network)
% Logic: 5.2 (Indexed Tuple P,T,E,S + Coupling + Purity + Network Drift)
% Generated: 2024-07-15
% ============================================================================

:- module(constraint_structural_extraction_without_actor, []).

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
 * * constraint_id: structural_extraction_without_actor
 * human_readable: The Inertial Rent Siphon
 * domain: economic/social
 * * SUMMARY:
 * This constraint represents a legacy fee-structure or bureaucratic
 * requirement where the original "service provider" or "beneficiary"
 * has long since vanished or automated, yet the extraction continues
 * via institutional inertia. It is an "actorless" siphon.
 * * KEY AGENTS:
 * - Current Payer: Subject (Powerless)
 * - Legacy System: Beneficiary (Institutional - though the 'human' beneficiary is absent)
 * - Systems Archeologist: Auditor (Analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% High extraction (0.78) as it is pure rent with zero service return.
domain_priors:base_extractiveness(structural_extraction_without_actor, 0.78).
domain_priors:suppression_score(structural_extraction_without_actor, 0.65).
domain_priors:theater_ratio(structural_extraction_without_actor, 0.85). % High theater/inertia.

% Constraint metric facts — primary keys used by the classification engine.
narrative_ontology:constraint_metric(structural_extraction_without_actor, extractiveness, 0.78).
narrative_ontology:constraint_metric(structural_extraction_without_actor, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(structural_extraction_without_actor, theater_ratio, 0.85).

% Constraint self-claim (what does the constraint claim to be?)
narrative_ontology:constraint_claim(structural_extraction_without_actor, piton).

% Binary flags
domain_priors:requires_active_enforcement(structural_extraction_without_actor). % Required for Tangled Rope

% Structural property derivation hooks:
% These are required for the Tangled Rope classification.
narrative_ontology:constraint_beneficiary(structural_extraction_without_actor, legacy_system_accounts).
narrative_ontology:constraint_victim(structural_extraction_without_actor, current_payers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × π(P) × σ(S)
   ========================================================================== */

% PERSPECTIVE 1: THE SUBJECT (SNARE)
% The payer is trapped in a loop where they pay for a non-existent utility.
constraint_indexing:constraint_classification(structural_extraction_without_actor, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE BENEFICIARY (ROPE)
% The institutional code views this as a "coordination" prerequisite for system solvency.
constraint_indexing:constraint_classification(structural_extraction_without_actor, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 3: THE SYSTEMS AUDITOR (PITON)
% Default analytical context. Because theater_ratio (0.85) > 0.70, the auditor sees it as a dead, inertial spike.
constraint_indexing:constraint_classification(structural_extraction_without_actor, piton,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 4: THE HISTORICAL ANALYST (TANGLED ROPE)
% A historical view detects the hybrid nature of the original coordination function
% that has since degraded into a piton.
constraint_indexing:constraint_classification(structural_extraction_without_actor, tangled_rope,
    context(agent_power(analytical),
            time_horizon(historical),
            exit_options(analytical),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(structural_extraction_without_actor_tests).

test(perspectival_gap) :-
    % Verify the Snare vs Rope conflict.
    constraint_indexing:constraint_classification(structural_extraction_without_actor, snare,
        context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(structural_extraction_without_actor, rope,
        context(agent_power(institutional), _, _, _)),
    constraint_indexing:constraint_classification(structural_extraction_without_actor, piton,
        context(agent_power(analytical), _, _, _)).

test(piton_trigger) :-
    % Verify the auditor detects the Piton via theater_ratio.
    constraint_indexing:constraint_classification(structural_extraction_without_actor, piton,
        context(agent_power(analytical), time_horizon(civilizational), _, _)).

test(tangled_rope_classification_present) :-
    % Verify the historical tangled rope classification is possible.
    constraint_indexing:constraint_classification(structural_extraction_without_actor, tangled_rope,
        context(agent_power(analytical), time_horizon(historical), _, _)).

:- end_tests(structural_extraction_without_actor_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The extraction score of 0.78 and theater ratio of 0.85 are key. The high
 * theater ratio makes the primary analytical classification a Piton: an inertial
 * constraint whose original function has atrophied. The high extraction ensures
 * powerless agents still experience it as a Snare. The system is effectively
 * actorless—the 'Beneficiary' is a legacy algorithm or automated account.
 * * PERSPECTIVAL GAP:
 * The Subject experiences a Snare (wealth transfer without benefit). The
 * Institution experiences a Rope (a necessary, automated entry in the ledger).
 * The Analytical observer sees a Piton (inertial dead weight).
 * * [RESOLVED MANDATROPHY]:
 * This is resolved by identifying the constraint as a Piton for the analytical
 * index. The extraction persists not because of an active predator, but
 * because the cost of removing the "Piton" exceeds the perceived benefit
 * for the institutional actors. The Tangled Rope classification is only
 * visible from a historical perspective, representing the constraint's
 * state before it fully degraded. The presence of beneficiary, victim, and
 * enforcement facts allows the classifier to recognize this historical state.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% Required for base_extractiveness > 0.46
omega_variable(
    omega_actorless_leak,
    'Is there a hidden beneficiary (Snare) or is the extraction truly an entropic byproduct (Mountain)?',
    'Full-stack ledger forensic audit of legacy escrow accounts.',
    'If hidden actor: Snare. If entropic: Mountain of the system.',
    confidence_without_resolution(low)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(structural_extraction_without_actor, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Temporal data shows the constraint degrading from a functional (if extractive)
% Tangled Rope into a purely inertial Piton.
% Required for high-extraction constraints (base_extractiveness > 0.46).

% Theater ratio over time (triggers metric_substitution detection):
narrative_ontology:measurement(sewa_tr_t0, structural_extraction_without_actor, theater_ratio, 0, 0.20).
narrative_ontology:measurement(sewa_tr_t5, structural_extraction_without_actor, theater_ratio, 5, 0.50).
narrative_ontology:measurement(sewa_tr_t10, structural_extraction_without_actor, theater_ratio, 10, 0.85).

% Extraction over time (triggers extraction_accumulation detection):
narrative_ontology:measurement(sewa_ex_t0, structural_extraction_without_actor, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(sewa_ex_t5, structural_extraction_without_actor, base_extractiveness, 5, 0.68).
narrative_ontology:measurement(sewa_ex_t10, structural_extraction_without_actor, base_extractiveness, 10, 0.78).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA (v5.0-5.2)
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% The legacy fee structure was originally an enforcement mechanism.
narrative_ontology:coordination_type(structural_extraction_without_actor, enforcement_mechanism).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */