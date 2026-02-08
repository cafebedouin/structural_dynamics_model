% ============================================================================
% CONSTRAINT STORY: nuclear_vacuum_2026
% ============================================================================
% Version: 5.2 (Deferential Realism Core + Boltzmann + Purity + Network)
% Logic: 5.2 (Indexed Tuple P,T,E,S + Coupling + Purity + Network Drift)
% Generated: 2023-10-27
% ============================================================================

:- module(constraint_nuclear_vacuum_2026, []).

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
 * * constraint_id: nuclear_vacuum_2026
 * human_readable: The New START Expiration (Post-Arms Control Era)
 * domain: political
 * * SUMMARY:
 * At 12:00 AM on February 5, 2026, the New START treaty expired, ending 50 years 
 * of structured nuclear arms control between the U.S. and Russia. This creates 
 * a "Strategic Vacuum" where limits on warheads and mandatory inspections 
 * vanish, replaced by "Strategic Ambiguity." The shift encourages a tri-polar 
 * nuclear expansion (U.S., Russia, China) without a legal framework for verification.
 * * KEY AGENTS:
 * - Global Non-Combatants: Subject (Powerless)
 * - Nuclear Superpowers (U.S./Russia/China): Beneficiary (Institutional)
 * - Arms Control Experts/Diplomats: Auditor (Analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Extraction is high (0.58) as the lapse diverts civilizational resources 
% into a tri-polar arms race and extracts global security margin.
domain_priors:base_extractiveness(nuclear_vacuum_2026, 0.58).

% Suppression is high (0.85) as the lack of a legal framework suppresses 
% diplomatic alternatives and stabilizes through fear/ambiguity.
domain_priors:suppression_score(nuclear_vacuum_2026, 0.85).

% Theater ratio is moderate (0.42) as various back-channel talks provide a 
% "theater" of diplomacy while the core verifiable limits have dissolved.
domain_priors:theater_ratio(nuclear_vacuum_2026, 0.42).

% Constraint metric facts — primary keys used by the classification engine.
narrative_ontology:constraint_metric(nuclear_vacuum_2026, extractiveness, 0.58).
narrative_ontology:constraint_metric(nuclear_vacuum_2026, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(nuclear_vacuum_2026, theater_ratio, 0.42).

% Constraint self-claim: The new state of ambiguity is enforced by mutual threat.
narrative_ontology:constraint_claim(nuclear_vacuum_2026, tangled_rope).

% Binary flags: The new "stability" requires the active threat of retaliation.
domain_priors:requires_active_enforcement(nuclear_vacuum_2026).

% Structural property derivation hooks:
narrative_ontology:constraint_beneficiary(nuclear_vacuum_2026, defense_industrial_complexes).
narrative_ontology:constraint_victim(nuclear_vacuum_2026, global_civilian_population).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × π(P) × σ(S)
   Power (P) and Scope (S) both affect effective extraction.
   Scope modifiers: local=0.8, regional=0.9, national=1.0,
                    continental=1.1, global=1.2, universal=1.0.
   ========================================================================== */

% PERSPECTIVE 1: THE SUBJECT (SNARE)
% For the global population, the expiration is a Snare. The effective extraction
% is amplified by their powerlessness (χ = 0.58 * 1.5 * 1.2 = 1.044).
% It is a predatory trap of existential risk where they are "trapped" without any exit option.
constraint_indexing:constraint_classification(nuclear_vacuum_2026, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: THE BENEFICIARY (ROPE)
% Superpowers view this as a Rope. Their institutional power negates the
% extraction (χ = 0.58 * -0.2 * 1.2 = -0.1392), making it appear as pure,
% necessary coordination (mutual deterrence).
constraint_indexing:constraint_classification(nuclear_vacuum_2026, rope,
    context(agent_power(institutional),
            time_horizon(historical),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% The analytical view computes a Tangled Rope. The base extraction (0.58) is high,
% suppression (0.85) is high, and it has both a coordination function (beneficiary)
% and asymmetric extraction (victim), and requires active enforcement.
% Effective extraction χ = 0.58 * 1.15 * 1.2 = 0.80.
constraint_indexing:constraint_classification(nuclear_vacuum_2026, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(nuclear_vacuum_2026_tests).

test(perspectival_gap_subject_vs_beneficiary) :-
    constraint_indexing:constraint_classification(nuclear_vacuum_2026, snare, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(nuclear_vacuum_2026, rope, context(agent_power(institutional), _, _, _)).

test(analytical_classification_is_tangled_rope) :-
    % Verify the analytical observer correctly identifies the Tangled Rope structure.
    constraint_indexing:constraint_classification(nuclear_vacuum_2026, tangled_rope, context(agent_power(analytical), _, _, _)).

test(tangled_rope_structural_requirements_met) :-
    % A Tangled Rope requires all three of these properties.
    domain_priors:requires_active_enforcement(nuclear_vacuum_2026),
    narrative_ontology:constraint_beneficiary(nuclear_vacuum_2026, _),
    narrative_ontology:constraint_victim(nuclear_vacuum_2026, _).

:- end_tests(nuclear_vacuum_2026_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The scores reflect the shift from a managed, verifiable arms control regime (low extraction Rope)
 * to an unmanaged "Strategic Ambiguity" that functions as a Tangled Rope. The extraction (0.58)
 * represents the diversion of resources to an arms race and the erosion of global security.
 * The suppression (0.85) reflects how this ambiguity forecloses diplomatic alternatives and
 * enforces stability through existential threat.
 *
 * The Perspectival Gap is stark:
 * - The powerless global population experiences this as a Snare, feeling the full amplified
 *   extraction (χ=1.044) as pure risk imposed upon them.
 * - The institutional nuclear powers experience it as a Rope, as their power inverts the
 *   extraction (χ=-0.14), making the system feel like a cost-free coordination mechanism.
 * - The analytical view, balancing these, identifies the Tangled Rope: a system that performs
 *   a coordination function (deterrence) but does so via high asymmetric extraction and coercion.
 *
 * * MANDATROPHY ANALYSIS:
 * The Tangled Rope classification is critical for Mandatrophy resolution. It prevents the system
 * from accepting the institutional view (Rope) or the subject's view (Snare) at face value.
 * By requiring both a coordination function (beneficiary) and asymmetric extraction (victim),
 * it correctly identifies that this is not pure coordination, nor is it pure predation. It is a
 * hybrid system where a coordination claim is used to legitimize high extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    omega_new_start_2026,
    'Will "Strategic Ambiguity" settle into a stable, tacitly-agreed Rope or decay into a permanent, expansionist Snare?',
    'Analysis of warhead deployment data and launch-on-warning policy shifts from all three superpowers over the next 36 months.',
    'If tacit limits hold, it may re-classify as a Rope. If expansion accelerates, it solidifies as a Snare from all perspectives.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(nuclear_vacuum_2026, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Tracking the drift from the Treaty Regime (T=0) to the post-expiration era.
% Theater ratio rises as "talks" become the only proxy for real limits.
narrative_ontology:measurement(nuc_v_tr_t0, nuclear_vacuum_2026, theater_ratio, 0, 0.10).
narrative_ontology:measurement(nuc_v_tr_t5, nuclear_vacuum_2026, theater_ratio, 5, 0.25).
narrative_ontology:measurement(nuc_v_tr_t10, nuclear_vacuum_2026, theater_ratio, 10, 0.42).

% Extraction spikes as the legal framework dissolves.
narrative_ontology:measurement(nuc_v_ex_t0, nuclear_vacuum_2026, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(nuc_v_ex_t5, nuclear_vacuum_2026, base_extractiveness, 5, 0.38).
narrative_ontology:measurement(nuc_v_ex_t10, nuclear_vacuum_2026, base_extractiveness, 10, 0.58).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA (v5.0-5.2)
   ========================================================================== */

% Coordination type: The system's stability relies on mutual threat.
narrative_ontology:coordination_type(nuclear_vacuum_2026, enforcement_mechanism).

% Network relationships: The state of nuclear stability directly impacts
% the security of global infrastructure and supply chains.
narrative_ontology:affects_constraint(nuclear_vacuum_2026, global_shipping_security).
narrative_ontology:affects_constraint(nuclear_vacuum_2026, semiconductor_supply).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */