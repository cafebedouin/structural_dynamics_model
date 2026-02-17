% ============================================================================
% CONSTRAINT STORY: network_effects
% ============================================================================
% Version: 5.2 (Deferential Realism Core + Boltzmann + Purity + Network)
% Logic: 5.2 (Indexed Tuple P,T,E,S + Coupling + Purity + Network Drift)
% Generated: 2024-05-21
% ============================================================================

:- module(constraint_network_effects, []).

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
 * * constraint_id: network_effects
 * human_readable: Network Effects (Demand-Side Economies of Scale)
 * domain: economic/technological
 * * SUMMARY:
 * A phenomenon where a product or service gains additional value as more people
 * use it. This creates a powerful positive feedback loop that often leads to
 * "winner-take-all" dynamics, where a dominant player becomes nearly impossible
 * to displace due to the high cost for users to leave the established network.
 * * KEY AGENTS:
 * - Early Adopter/End User: Subject (Powerless)
 * - Platform Owner: Beneficiary (Institutional)
 * - Strategic Competitor: Subject (Moderate Power)
 * - Systems Auditor: Observer (Analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Numerical anchors for v3.4 thresholds
% Extraction is moderate: users gain utility, but the platform owner extracts
% disproportionate value through data, fees, or advertising once lock-in is achieved.
domain_priors:base_extractiveness(network_effects, 0.55).
% Suppression is high: incumbent networks suppress alternatives by preventing
% interoperability, raising switching costs, and leveraging their user base.
domain_priors:suppression_score(network_effects, 0.60).
% Theater is low; the network's function is real, not performative.
domain_priors:theater_ratio(network_effects, 0.1).

% Constraint metric facts — primary keys used by the classification engine.
narrative_ontology:constraint_metric(network_effects, extractiveness, 0.55).
narrative_ontology:constraint_metric(network_effects, suppression_requirement, 0.60).
narrative_ontology:constraint_metric(network_effects, theater_ratio, 0.1).

% Constraint self-claim (what does the constraint claim to be?)
% Often framed as an inevitable, natural law of markets (Metcalfe's Law).
narrative_ontology:constraint_claim(network_effects, tangled_rope).
narrative_ontology:human_readable(network_effects, "Network Effects (Demand-Side Economies of Scale)").

% Binary flags
% Enforcement is required to maintain the lock-in, e.g., by preventing data portability.
domain_priors:requires_active_enforcement(network_effects).

% Structural property derivation hooks:
narrative_ontology:constraint_beneficiary(network_effects, platform_owners).
narrative_ontology:constraint_victim(network_effects, innovative_startups).
narrative_ontology:constraint_victim(network_effects, end_users_facing_lock_in).


/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × π(P) × σ(S)
   Power (P) and Scope (S) both affect effective extraction.
   Scope modifiers: local=0.8, regional=0.9, national=1.0,
                    continental=1.1, global=1.2, universal=1.0.
   ========================================================================== */

% PERSPECTIVE 1: THE END USER (SNARE)
% Initially a Rope, but over time becomes a Snare due to high switching costs
% and extractive practices that emerge after the network is dominant.
constraint_indexing:constraint_classification(network_effects, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: THE PLATFORM OWNER (ROPE)
% Viewed as a pure coordination mechanism that creates immense value and a
% defensible market position. Extraction is seen as a fair return.
constraint_indexing:constraint_classification(network_effects, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: THE STRATEGIC COMPETITOR (MOUNTAIN)
% A startup with a superior product sees the incumbent's network effect as an
% unchangeable law of the market, an insurmountable barrier to entry.
constraint_indexing:constraint_classification(network_effects, mountain,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 4: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% Recognizes both the genuine coordination value (beneficiaries exist) and the
% asymmetric, coercive extraction (victims exist), classifying it as a hybrid.
constraint_indexing:constraint_classification(network_effects, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(network_effects_tests).

test(perspectival_gap_user_vs_owner) :-
    % Verify the user (powerless) and owner (institutional) have different views.
    constraint_indexing:constraint_classification(network_effects, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(network_effects, TypeInstitutional, context(agent_power(institutional), _, _, _)),
    TypePowerless == snare,
    TypeInstitutional == rope,
    TypePowerless \= TypeInstitutional.

test(analytical_observer_is_tangled_rope) :-
    % The analytical view must resolve to Tangled Rope given the metrics.
    constraint_indexing:constraint_classification(network_effects, tangled_rope, context(agent_power(analytical), _, _, _)).

test(competitor_sees_mountain) :-
    constraint_indexing:constraint_classification(network_effects, mountain, context(agent_power(moderate), _, _, _)).

:- end_tests(network_effects_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The scores reflect the dual nature of network effects. The base extractiveness (0.55)
 * and suppression (0.60) are high enough to qualify for a Snare or Tangled Rope,
 * but not so high as to negate the genuine coordination value it provides.
 *
 * The Perspectival Gap is stark:
 * - For the Platform Owner (Institutional), it's a perfect Rope, a tool for value creation.
 * - For the End User (Powerless), it becomes a Snare as switching costs rise and the platform begins extracting value.
 * - For a Competitor (Moderate), it's a Mountain, an immutable barrier to entry.
 * - The Analytical observer, weighing both the coordination function (beneficiaries) and the coercive extraction (victims, enforcement), correctly identifies it as a Tangled Rope.
 *
 * MANDATROPHY ANALYSIS:
 * Classifying this as a Tangled Rope is critical. A simpler analysis might label it a Snare (focusing only on user lock-in) or a Rope (focusing only on the platform's value creation). The Tangled Rope classification correctly captures the reality that it is BOTH a powerful coordination tool AND an extractive mechanism. This prevents the system from mischaracterizing a constructed market dynamic as either pure malevolence or pure public good.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_network_effects,
    'Are network effects an emergent, natural law of digital physics (Mountain) or a constructed barrier maintained by active suppression of interoperability (Tangled Rope)?',
    'Analysis of markets where interoperability is mandated (e.g., EU via DMA). If new competitors thrive, it supports the "constructed barrier" hypothesis. If the incumbent remains dominant, it supports the "natural law" view.',
    'If Mountain, policy interventions are futile. If Tangled Rope, regulatory action like forced data portability could dismantle the constraint.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(network_effects, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% This models the lifecycle of a successful platform: initial low extraction to
% attract users, which then increases as lock-in is achieved.
% Theater ratio remains low, as the function is always real.

% Theater ratio over time (metric_substitution):
narrative_ontology:measurement(network_effects_tr_t0, network_effects, theater_ratio, 0, 0.05).
narrative_ontology:measurement(network_effects_tr_t5, network_effects, theater_ratio, 5, 0.08).
narrative_ontology:measurement(network_effects_tr_t10, network_effects, theater_ratio, 10, 0.1).

% Extraction over time (extraction_accumulation):
narrative_ontology:measurement(network_effects_ex_t0, network_effects, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(network_effects_ex_t5, network_effects, base_extractiveness, 5, 0.40).
narrative_ontology:measurement(network_effects_ex_t10, network_effects, base_extractiveness, 10, 0.55).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA (v5.0-5.2)
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% Network effects create a shared digital space and user base.
narrative_ontology:coordination_type(network_effects, global_infrastructure).

% Network relationships (structural influence edges)
% Network effects are a primary driver of platform lock-in.
narrative_ontology:affects_constraint(network_effects, platform_lock_in).
narrative_ontology:affects_constraint(data_portability_mandates, network_effects).


/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */