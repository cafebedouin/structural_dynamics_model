% ============================================================================
% CONSTRAINT STORY: multi_planetary_latency_lock
% ============================================================================
% Version: 5.2 (Deferential Realism Core + Boltzmann + Purity + Network)
% Logic: 5.2 (Indexed Tuple P,T,E,S + Coupling + Purity + Network Drift)
% Generated: 2024-07-15
% ============================================================================

:- module(constraint_multi_planetary_latency_lock, []).

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
 * * constraint_id: multi_planetary_latency_lock
 * human_readable: Earth-Centric Interplanetary Clearing
 * domain: technological/economic
 * * SUMMARY:
 * As civilization expands to Mars, the speed of light creates an irreducible
 * communication latency (3-22 minutes). Earth-based financial institutions
 * leverage this physical limit by creating a centralized, Earth-time clearing
 * system. They charge significant "latency surcharges" for guaranteeing
 * transactions across the time gap, turning a physical constant into an
 * extractive policy. This policy actively suppresses decentralized, local
 * clearing alternatives that would be more efficient for off-world colonies.
 * * KEY AGENTS:
 * - Martian Colonist: Subject (Powerless)
 * - Earth-Based Financial Consortium: Beneficiary (Institutional)
 * - Systems Auditor: Auditor (Analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% The constraint is the POLICY, not the physics. The policy is highly extractive.
domain_priors:base_extractiveness(multi_planetary_latency_lock, 0.75).
% Suppression is high because the central authority actively works to prevent
% the emergence of a decentralized, off-world financial system.
domain_priors:suppression_score(multi_planetary_latency_lock, 0.90).
domain_priors:theater_ratio(multi_planetary_latency_lock, 0.15). % Low theater; the enforcement and extraction are real.

% Constraint metric facts — primary keys used by the classification engine.
narrative_ontology:constraint_metric(multi_planetary_latency_lock, extractiveness, 0.75).
narrative_ontology:constraint_metric(multi_planetary_latency_lock, suppression_requirement, 0.90).
narrative_ontology:constraint_metric(multi_planetary_latency_lock, theater_ratio, 0.15).

% The system claims to be a necessary coordination mechanism for interplanetary stability.
narrative_ontology:constraint_claim(multi_planetary_latency_lock, tangled_rope).

% Binary flags
domain_priors:requires_active_enforcement(multi_planetary_latency_lock). % Required for Tangled Rope

% Structural property derivation hooks for Tangled Rope:
narrative_ontology:constraint_beneficiary(multi_planetary_latency_lock, earth_financial_consortium).
narrative_ontology:constraint_victim(multi_planetary_latency_lock, off_world_colonists).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × π(P) × σ(S)
   Power (P) and Scope (S) both affect effective extraction.
   Scope modifiers: universal=1.0.
   ========================================================================== */

% PERSPECTIVE 1: THE SUBJECT (SNARE)
% The colonist is trapped in an extractive system justified by physics they
% cannot change. Effective extraction χ = 0.75 * 1.5 * 1.0 = 1.125 (Snare).
constraint_indexing:constraint_classification(multi_planetary_latency_lock, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(universal))).

% PERSPECTIVE 2: THE BENEFICIARY (ROPE)
% The consortium sees the system as essential coordination.
% Effective extraction χ = 0.75 * -0.2 * 1.0 = -0.15 (Rope).
constraint_indexing:constraint_classification(multi_planetary_latency_lock, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(universal))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% The observer sees both the coordination function for Earth and the asymmetric
% extraction from the colonies, supported by active enforcement.
% Effective extraction χ = 0.75 * 1.15 * 1.0 = 0.8625 (Tangled Rope).
constraint_indexing:constraint_classification(multi_planetary_latency_lock, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(multi_planetary_latency_lock_tests).

test(perspectival_gap) :-
    % Verify the gap between the colonist (Snare) and the consortium (Rope).
    constraint_indexing:constraint_classification(multi_planetary_latency_lock, snare, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(multi_planetary_latency_lock, rope, context(agent_power(institutional), _, _, _)),
    constraint_indexing:constraint_classification(multi_planetary_latency_lock, tangled_rope, context(agent_power(analytical), _, _, _)).

test(tangled_rope_structural_properties) :-
    % Verify that all three requirements for Tangled Rope are met.
    narrative_ontology:constraint_beneficiary(multi_planetary_latency_lock, _),
    narrative_ontology:constraint_victim(multi_planetary_latency_lock, _),
    domain_priors:requires_active_enforcement(multi_planetary_latency_lock).

test(threshold_validation) :-
    % Ensure base extraction is high enough for a Snare/Tangled Rope classification.
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(multi_planetary_latency_lock, ExtMetricName, E),
    E >= 0.46.

:- end_tests(multi_planetary_latency_lock_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * This model was refactored to focus on the *policy* of Earth-centric clearing,
 * not the physical law of light speed itself. The physical law acts as an
 * unchangeable pretext (a "Mountain") that justifies the creation of a highly
 * extractive policy (a "Tangled Rope"). The base extractiveness of 0.75
 * reflects the "latency surcharge" and other fees imposed by the system.
 *
 * PERSPECTIVAL GAP:
 * The gap is stark. The Martian Colonist experiences a Snare: a costly,
 * inescapable system. The Earth-Based Financial Consortium, which profits
 * immensely and feels no negative effects (in fact, a net benefit), views it
 * as a pure Rope providing necessary coordination and stability. The Analytical
 * Observer, seeing both the coordination claim and the severe asymmetric
 * extraction, correctly classifies it as a Tangled Rope.
 *
 * MANDATROPHY ANALYSIS:
 * [RESOLVED MANDATROPHY] The Tangled Rope classification is critical here.
 * A simpler model might label this a Snare, ignoring the consortium's valid
 * (from their perspective) coordination function. Another might label it a
 * Rope, ignoring the crippling extraction felt by colonists. Tangled Rope
 * captures the dual-nature of the constraint: it performs a real coordination
 * function for one group while simultaneously functioning as a predatory
 * extraction mechanism for another.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    omega_ftl_comms,
    'Can non-local quantum entanglement or other novel physics bypass the light-speed latency lock for information transfer?',
    'Experimental validation of superluminal information transfer.',
    'If True: The physical pretext for the policy vanishes, causing the Tangled Rope to collapse or be revealed as a pure Snare. If False: The policy remains justifiable on physical grounds, solidifying its power.',
    confidence_without_resolution(low)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(multi_planetary_latency_lock, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% This system likely began as a mild coordination fee and grew more extractive
% as off-world dependence increased.
%
% Theater ratio over time (remains low):
narrative_ontology:measurement(mpll_tr_t0, multi_planetary_latency_lock, theater_ratio, 0, 0.05).
narrative_ontology:measurement(mpll_tr_t5, multi_planetary_latency_lock, theater_ratio, 5, 0.10).
narrative_ontology:measurement(mpll_tr_t10, multi_planetary_latency_lock, theater_ratio, 10, 0.15).

% Extraction over time (extraction_accumulation drift):
narrative_ontology:measurement(mpll_ex_t0, multi_planetary_latency_lock, base_extractiveness, 0, 0.50).
narrative_ontology:measurement(mpll_ex_t5, multi_planetary_latency_lock, base_extractiveness, 5, 0.65).
narrative_ontology:measurement(mpll_ex_t10, multi_planetary_latency_lock, base_extractiveness, 10, 0.75).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA (v5.0-5.2)
   ========================================================================== */

% This policy is a form of financial resource allocation.
narrative_ontology:coordination_type(multi_planetary_latency_lock, resource_allocation).

% This constraint would be structurally coupled with rules governing off-world rights.
narrative_ontology:affects_constraint(multi_planetary_latency_lock, off_world_resource_rights).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */