% ============================================================================
% CONSTRAINT STORY: global_protocol_entrenchment
% ============================================================================
% Version: 5.2 (Deferential Realism Core + Boltzmann + Purity + Network)
% Logic: 5.2 (Indexed Tuple P,T,E,S + Coupling + Purity + Network Drift)
% Generated: 2024-07-27
% ============================================================================

:- module(constraint_global_protocol_entrenchment, []).

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
 * * constraint_id: global_protocol_entrenchment
 * human_readable: The Universal Standard Lock
 * domain: technological/economic
 * * SUMMARY:
 * This scenario shifts the scope of a structural constraint to the global level.
 * It describes a foundational communication or financial protocol that,
 * having achieved universal adoption, renders all localized alternatives
 * non-viable. At this scale, the constraint ceases to be a policy choice
 * and becomes a prerequisite for participation in the global species-stack.
 * * KEY AGENTS:
 * - Local Innovators: Subject (Powerless)
 * - Global Standards Body: Beneficiary (Institutional)
 * - Civilizational Auditor: Auditor (Analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% High extraction (0.80) because universal scope creates total capture of
% all peripheral innovation and transaction surplus.
domain_priors:base_extractiveness(global_protocol_entrenchment, 0.80).
domain_priors:suppression_score(global_protocol_entrenchment, 0.95). % Alternatives are effectively non-existent.
domain_priors:theater_ratio(global_protocol_entrenchment, 0.30).    % Low theater; the lock-in is technically absolute.

% Constraint metric facts — primary keys used by the classification engine.
narrative_ontology:constraint_metric(global_protocol_entrenchment, extractiveness, 0.80).
narrative_ontology:constraint_metric(global_protocol_entrenchment, suppression_requirement, 0.95).
narrative_ontology:constraint_metric(global_protocol_entrenchment, theater_ratio, 0.30).

% Constraint self-claim: The protocol presents itself as a pure public good.
narrative_ontology:constraint_claim(global_protocol_entrenchment, tangled_rope).

% Binary flags and structural properties for Tangled Rope classification.
domain_priors:requires_active_enforcement(global_protocol_entrenchment).
narrative_ontology:constraint_beneficiary(global_protocol_entrenchment, global_standards_body).
narrative_ontology:constraint_victim(global_protocol_entrenchment, local_innovators).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × π(P) × σ(S)
   Power (P) and Scope (S) both affect effective extraction.
   Scope modifiers: local=0.8, regional=0.9, national=1.0,
                    continental=1.1, global=1.2, universal=1.0.
   ========================================================================== */

% PERSPECTIVE 1: THE SUBJECT (MOUNTAIN)
% At a global scope, the individual sees no "outside"; the protocol is the environment.
constraint_indexing:constraint_classification(global_protocol_entrenchment, mountain,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: THE BENEFICIARY (ROPE)
% The institution views this as the ultimate Rope—the only way to coordinate 8 billion agents.
constraint_indexing:constraint_classification(global_protocol_entrenchment, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% The analytical view recognizes both the coordination function and the asymmetric extraction.
% This is the default analytical context.
constraint_indexing:constraint_classification(global_protocol_entrenchment, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(global_protocol_entrenchment_tests).

test(perspectival_gap_is_extreme) :-
    % Verify the extreme gap between Mountain (powerless) and Rope (institutional).
    constraint_indexing:constraint_classification(global_protocol_entrenchment, mountain, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(global_protocol_entrenchment, rope, context(agent_power(institutional), _, _, _)).

test(analytical_classification_is_tangled_rope) :-
    % Verify the analytical observer correctly identifies the hybrid nature.
    constraint_indexing:constraint_classification(global_protocol_entrenchment, tangled_rope, context(agent_power(analytical), _, _, _)).

:- end_tests(global_protocol_entrenchment_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The core of this story is the effect of the `global` spatial scope. It amplifies
 * the effective extraction (χ) via its modifier (σ=1.2) and, more importantly,
 * transforms the subjective experience for a powerless agent. With no jurisdictional
 * arbitrage, a constructed system with 0.95 suppression becomes indistinguishable
 * from a natural law, hence the `mountain` classification.
 *
 * The high extraction (0.80) and suppression (0.95) are justified by the total
 * lock-in effect of a universal standard, which captures all value at the periphery
 * and suppresses any non-compliant innovation.
 *
 * * PERSPECTIVAL GAP:
 * The gap is stark: Mountain vs. Rope vs. Tangled Rope.
 * - The powerless `local_innovator` experiences a Mountain because the "gravity" of universal
 *   adoption is total and inescapable.
 * - The `global_standards_body` sees a Rope because they have achieved the ideal of
 *   zero-friction global coordination.
 * - The analytical observer sees a Tangled Rope, acknowledging both the genuine coordination
 *   function (beneficiary exists) and the severe asymmetric extraction (victim exists),
 *   all held in place by active enforcement.
 *
 * * COALITION DYNAMICS:
 * While a critical mass of victims can sometimes upgrade their power to `organized`,
 * the global scale of this constraint makes effective coalition-building nearly
 * impossible, neutralizing this dynamic and preserving the `powerless` index.
 *
 * * [RESOLVED MANDATROPHY]:
 * The potential misclassification of this constraint as a pure Snare is resolved
 * by the Tangled Rope classification. Despite the extreme 0.80 extraction, the
 * protocol is not purely parasitic; it provides a real, species-level coordination
 * function. The Tangled Rope category correctly captures this duality of function
 * and extraction, preventing mandatrophy.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% Required for high-extraction constraints (> 0.46).
omega_variable(
    omega_gpe_exit,
    'Can a decentralized "shadow" protocol emerge that bypasses global entrenchment (is it a constructed Snare or a logical Mountain)?',
    'Tracking adoption rates of encrypted, non-standard transport layers and their ability to achieve network effects.',
    'If shadow protocols thrive: Tangled Rope/Snare (policy barrier). If they consistently fail to scale: Mountain (a logical limit of coordination at scale).',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(global_protocol_entrenchment, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% This constraint became more extractive over time as network effects solidified
% its monopoly, allowing for increased rent-seeking.
%
% Theater ratio over time (metric_substitution detection):
narrative_ontology:measurement(gpe_tr_t0, global_protocol_entrenchment, theater_ratio, 0, 0.10).
narrative_ontology:measurement(gpe_tr_t5, global_protocol_entrenchment, theater_ratio, 5, 0.20).
narrative_ontology:measurement(gpe_tr_t10, global_protocol_entrenchment, theater_ratio, 10, 0.30).

% Extraction over time (extraction_accumulation detection):
narrative_ontology:measurement(gpe_ex_t0, global_protocol_entrenchment, base_extractiveness, 0, 0.30).
narrative_ontology:measurement(gpe_ex_t5, global_protocol_entrenchment, base_extractiveness, 5, 0.65).
narrative_ontology:measurement(gpe_ex_t10, global_protocol_entrenchment, base_extractiveness, 10, 0.80).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA (v5.0-5.2)
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
narrative_ontology:coordination_type(global_protocol_entrenchment, global_infrastructure).

% Boltzmann floor override (only if domain knowledge justifies)
% narrative_ontology:boltzmann_floor_override(global_protocol_entrenchment, 0.25).

% Network relationships (structural influence edges)
% narrative_ontology:affects_constraint(global_protocol_entrenchment, another_constraint_id).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */