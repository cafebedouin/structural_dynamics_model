% ============================================================================
% CONSTRAINT STORY: ergo_storage_rent
% ============================================================================
% Version: 5.2 (Deferential Realism Core + Boltzmann + Purity + Network)
% Logic: 5.2 (Indexed Tuple P,T,E,S + Coupling + Purity + Network Drift)
% Generated: 2024-07-29
% ============================================================================

:- module(constraint_ergo_storage_rent, []).

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
 * * constraint_id: ergo_storage_rent
 * human_readable: Ergo Storage Rent (UTXO Demurrage)
 * domain: technological/economic
 * * SUMMARY:
 * Storage rent is a "demurrage" fee charged to UTXOs (unspent coins) that have 
 * not moved in four years. This mechanism prevents blockchain bloat from "dust"
 * and recycles lost or abandoned coins back to the miners, ensuring long-term
 * network security.
 * * KEY AGENTS:
 * - The Long-Term Holder: Subject (Powerless), especially if keys are lost.
 * - The Miner Collective: Beneficiary (Institutional), collects rent to secure the network.
 * - The Ergo Foundation: Auditor (Analytical), designers of the economic model.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Numerical anchors for v3.4 thresholds
domain_priors:base_extractiveness(ergo_storage_rent, 0.55). % Mountain suppression <= 0.05, Rope extraction <= 0.15, Snare extraction >= 0.46
domain_priors:suppression_score(ergo_storage_rent, 0.2).   % Structural property (raw, unscaled). Only extractiveness is scaled (by power and scope).
domain_priors:theater_ratio(ergo_storage_rent, 0.1).       % Piton detection (>= 0.70)

% Constraint metric facts — primary keys used by the classification engine.
% These mirror domain_priors values using the metric key names from config.pl.
narrative_ontology:constraint_metric(ergo_storage_rent, extractiveness, 0.55).
narrative_ontology:constraint_metric(ergo_storage_rent, suppression_requirement, 0.2).
narrative_ontology:constraint_metric(ergo_storage_rent, theater_ratio, 0.1).

% Constraint self-claim (what does the constraint claim to be?)
% Values: natural_law, coordination, constructed, enforcement
narrative_ontology:constraint_claim(ergo_storage_rent, tangled_rope).

% Binary flags
domain_priors:requires_active_enforcement(ergo_storage_rent). % Required for Tangled Rope

% Structural property derivation hooks:
%   has_coordination_function/1 is DERIVED from constraint_beneficiary/2
%   has_asymmetric_extraction/1 is DERIVED from constraint_victim/2
% Both are required for Tangled Rope. Coordination is also required for Scaffold.
narrative_ontology:constraint_beneficiary(ergo_storage_rent, ergo_miners).
narrative_ontology:constraint_victim(ergo_storage_rent, inactive_holders).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × π(P) × σ(S)
   Power (P) and Scope (S) both affect effective extraction.
   Scope modifiers: local=0.8, regional=0.9, national=1.0,
                    continental=1.1, global=1.2, universal=1.0.
   ========================================================================== */

% PERSPECTIVE 1: THE INACTIVE HOLDER (SNARE)
% For a user who has lost their keys or is unaware, the rent is an unstoppable
% trap that drains their assets.
% χ = 0.55 (ε) * 1.5 (powerless) * 0.8 (local) = 0.66. Meets Snare threshold.
constraint_indexing:constraint_classification(ergo_storage_rent, snare,
    context(agent_power(powerless),
            time_horizon(historical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: THE MINER (ROPE)
% For miners, this is a vital coordination mechanism for long-term network
% sustainability, guaranteeing revenue after block rewards diminish.
% χ = 0.55 (ε) * -0.2 (institutional) * 1.2 (global) = -0.132. Perceived as a net benefit.
constraint_indexing:constraint_classification(ergo_storage_rent, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% The designer sees a system that solves coordination (bloat, security) via
% asymmetric extraction from a specific group, requiring protocol enforcement.
% This is the definition of a Tangled Rope.
constraint_indexing:constraint_classification(ergo_storage_rent, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ergo_storage_rent_tests).

test(perspectival_gap) :-
    % Verify there is a perspectival gap between powerless and institutional.
    constraint_indexing:constraint_classification(ergo_storage_rent, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(ergo_storage_rent, TypeInstitutional, context(agent_power(institutional), _, _, _)),
    TypePowerless == snare,
    TypeInstitutional == rope,
    TypePowerless \= TypeInstitutional.

test(threshold_validation_for_tangled_rope) :-
    % A Tangled Rope must have high base extraction and require enforcement.
    domain_priors:base_extractiveness(ergo_storage_rent, E),
    E >= 0.46,
    domain_priors:requires_active_enforcement(ergo_storage_rent),
    narrative_ontology:constraint_beneficiary(ergo_storage_rent, _),
    narrative_ontology:constraint_victim(ergo_storage_rent, _).

:- end_tests(ergo_storage_rent_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The base extractiveness is set to 0.55, placing it firmly in the high-extraction category.
 * This score is necessary for the powerless perspective (χ=0.66) to correctly classify as a Snare.
 * For an individual who has lost their keys, the rent is a total, unavoidable loss.
 * For miners (institutional), the effective extraction is negative, making it a pure coordination
 * mechanism (Rope) that guarantees future income and network security.
 * The analytical view must reconcile these facts: a system with a clear coordination function
 * (beneficiary exists) that is funded by asymmetric extraction (victim exists) and requires
 * protocol-level enforcement. This is the canonical definition of a Tangled Rope.
 *
 * MANDATROPHY ANALYSIS:
 * [RESOLVED MANDATROPHY]
 * Classifying this as a Tangled Rope prevents the system from making two errors. It avoids
 * dismissing the mechanism as a pure Snare (ignoring its vital coordination role for network
 * security) and avoids idealizing it as a pure Rope (ignoring the real, extractive cost
 * imposed on inactive holders). The Tangled Rope classification correctly identifies the
 * inherent, designed trade-off at the heart of the constraint.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_ergo_storage_rent,
    'Will the social consensus around the 4-year rent period and rate hold, or will it be altered via a future fork?',
    'Monitoring miner voting, community sentiment, and developer proposals on Ergo governance channels over multiple years.',
    'A shorter period increases the Snare-like pressure and extraction; a longer period weakens the long-term security argument, potentially degrading the Rope function.',
    confidence_without_resolution(low)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(ergo_storage_rent, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Temporal data enables drift detection (metric_substitution,
% extraction_accumulation) by providing measurements at multiple time points.
% Required for high-extraction constraints (base_extractiveness > 0.46).
% This models a system that was designed with this extraction level from the start.

% Theater ratio over time (triggers metric_substitution detection):
narrative_ontology:measurement(ergo_storage_rent_tr_t0, ergo_storage_rent, theater_ratio, 0, 0.05).
narrative_ontology:measurement(ergo_storage_rent_tr_t5, ergo_storage_rent, theater_ratio, 5, 0.08).
narrative_ontology:measurement(ergo_storage_rent_tr_t10, ergo_storage_rent, theater_ratio, 10, 0.1).

% Extraction over time (triggers extraction_accumulation detection):
narrative_ontology:measurement(ergo_storage_rent_ex_t0, ergo_storage_rent, base_extractiveness, 0, 0.52).
narrative_ontology:measurement(ergo_storage_rent_ex_t5, ergo_storage_rent, base_extractiveness, 5, 0.54).
narrative_ontology:measurement(ergo_storage_rent_ex_t10, ergo_storage_rent, base_extractiveness, 10, 0.55).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA (v5.0-5.2)
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% This mechanism allocates a future resource (miner fees) and manages a
% current resource (scarce blockchain state space).
narrative_ontology:coordination_type(ergo_storage_rent, resource_allocation).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */