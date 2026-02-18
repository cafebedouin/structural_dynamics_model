% ============================================================================
% CONSTRAINT STORY: trump_making_china_great_2026
% ============================================================================
% Version: 5.2 (Deferential Realism Core + Boltzmann + Purity + Network)
% Logic: 5.2 (Indexed Tuple P,T,E,S + Coupling + Purity + Network Drift)
% Generated: 2024-07-22
% ============================================================================

:- module(constraint_trump_making_china_great_2026, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * * constraint_id: trump_making_china_great_2026
 * human_readable: The Trumpian Post-Western Order
 * domain: political/economic
 * * SUMMARY:
 * The return of Donald Trump has accelerated a shift from a US-led liberal
 * international order to a multipolar "post-Western" world. This transition
 * acts as a constraint on European security and US global affinity while
 * expanding the agency of "middle powers" and China. The system moves from
 * a perceived coordination mechanism (Pax Americana) to a transactional,
 * extractive one.
 * * KEY AGENTS:
 * - European Leaders: Subject (Powerless)
 * - Middle Powers (India, Brazil, etc.): Beneficiary (Institutional with arbitrage)
 * - Geopolitical Analysts (e.g., ECFR): Auditor (Analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Numerical anchors for v3.4 thresholds
domain_priors:base_extractiveness(trump_making_china_great_2026, 0.65). % Mountain <= 0.15, Rope <= 0.15, Snare/Tangled >= 0.46
domain_priors:suppression_score(trump_making_china_great_2026, 0.40).   % Structural property (raw, unscaled).
domain_priors:theater_ratio(trump_making_china_great_2026, 0.15).       % Piton detection (>= 0.70)

% Constraint metric facts — primary keys used by the classification engine.
narrative_ontology:constraint_metric(trump_making_china_great_2026, extractiveness, 0.65).
narrative_ontology:constraint_metric(trump_making_china_great_2026, suppression_requirement, 0.40).
narrative_ontology:constraint_metric(trump_making_china_great_2026, theater_ratio, 0.15).

% Constraint self-claim (what does the constraint claim to be?)
% Claims to be enforcing national interest, a classic framing for extractive policy.
narrative_ontology:constraint_claim(trump_making_china_great_2026, tangled_rope).
narrative_ontology:human_readable(trump_making_china_great_2026, "The Trumpian Post-Western Order").
narrative_ontology:topic_domain(trump_making_china_great_2026, "political/economic").

% Binary flags
domain_priors:requires_active_enforcement(trump_making_china_great_2026). % Required for Tangled Rope

% Structural property derivation hooks:
narrative_ontology:constraint_beneficiary(trump_making_china_great_2026, china).
narrative_ontology:constraint_beneficiary(trump_making_china_great_2026, multipolar_middle_powers).
narrative_ontology:constraint_victim(trump_making_china_great_2026, european_union).
narrative_ontology:constraint_victim(trump_making_china_great_2026, liberal_internationalists).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × π(P) × σ(S)
   Power (P) and Scope (S) both affect effective extraction.
   Scope modifiers: local=0.8, regional=0.9, national=1.0,
                    continental=1.1, global=1.2, universal=1.0.
   ========================================================================== */

% PERSPECTIVE 1: THE EUROPEAN UNION (SNARE)
% Trapped by US unreliability and Russian aggression, the shift feels predatory.
% χ = 0.65 * π(powerless=1.5) * σ(continental=1.1) = 1.07 (High Snare)
constraint_indexing:constraint_classification(trump_making_china_great_2026, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(continental))).

% PERSPECTIVE 2: THE MIDDLE POWER DIPLOMAT (ROPE)
% Views multipolarity as a coordination opportunity, enabling arbitrage.
% χ = 0.65 * π(institutional=-0.2) * σ(global=1.2) = -0.156 (Net benefit, pure Rope)
constraint_indexing:constraint_classification(trump_making_china_great_2026, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% Recognizes the dual nature: it's a constructed system with both coordination
% functions (for some) and high asymmetric extraction (from others), requiring
% active enforcement. This is the canonical definition of a Tangled Rope.
% χ = 0.65 * π(analytical=1.15) * σ(global=1.2) = 0.897 (High Tangled Rope)
constraint_indexing:constraint_classification(trump_making_china_great_2026, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(trump_making_china_great_2026_tests).

test(perspectival_gap) :-
    % Verify there is a perspectival gap between powerless and institutional.
    constraint_indexing:constraint_classification(trump_making_china_great_2026, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(trump_making_china_great_2026, TypeInstitutional, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeInstitutional,
    TypePowerless = snare,
    TypeInstitutional = rope.

test(analytical_classification_is_tangled_rope) :-
    % Verify the analytical observer correctly identifies the Tangled Rope structure.
    constraint_indexing:constraint_classification(trump_making_china_great_2026, tangled_rope, context(agent_power(analytical), _, _, _)).

test(threshold_validation) :-
    % Ensures it's a high-extraction constraint, justifying Snare/Tangled Rope types.
    narrative_ontology:constraint_metric(trump_making_china_great_2026, extractiveness, E),
    E >= 0.46.

:- end_tests(trump_making_china_great_2026_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The base extractiveness (0.65) reflects the explicitly transactional and
 * predatory "America First" policy, which extracts loyalty and economic
 * concessions from traditional allies. The suppression score (0.40) is moderate
 * because the previous liberal order is still visible, just actively dismantled.
 *
 * The key insight is the perspectival gap. For the EU (powerless, trapped),
 * this new order is a Snare. For middle powers with arbitrage options
 * (institutional, mobile), it's a Rope that allows them to play superpowers
 * against each other.
 *
 * The analytical observer must classify this as a Tangled Rope. It is not a
 * Mountain (it is constructed and actively enforced) nor a pure Snare (it has
 * a genuine coordination function for a multipolar world). It has clear
 * beneficiaries and victims, and requires active enforcement (tariffs, threats)
 * to maintain, meeting all three structural requirements for a Tangled Rope.
 *
 * * MANDATROPHY ANALYSIS:
 * Classifying this as a Tangled Rope is critical. A simpler analysis might
 * label it a pure Snare, missing the fact that it creates a new, albeit
 * chaotic, coordination system for middle powers. This prevents mandatrophy
 * by acknowledging the system's dual function: it is simultaneously a tool of
 * extraction against one group and a coordination mechanism for another.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_trump_making_china_great_2026,
    'Is the shift to a transactional, multipolar order a permanent structural change or a temporary political phase tied to the Trump administration?',
    'Observe whether a post-Trump US administration (e.g., in 2028 or 2032) reverts to liberal internationalism or continues the transactional policy.',
    'If permanent, the constraint hardens into a Mountain for all actors. If temporary, it was a Scaffold for a new European security architecture.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(trump_making_china_great_2026, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% This constraint represents a rapid shift, with extraction accumulating quickly
% as the previous order is dismantled. The theater ratio remains low as the
% policies are bluntly functional rather than performative.
% Interval: 2020 (late first term) to 2026 (second term).

% Theater ratio over time (stable and low):
narrative_ontology:measurement(tmcg_tr_t0, trump_making_china_great_2026, theater_ratio, 0, 0.10).
narrative_ontology:measurement(tmcg_tr_t5, trump_making_china_great_2026, theater_ratio, 5, 0.12).
narrative_ontology:measurement(tmcg_tr_t10, trump_making_china_great_2026, theater_ratio, 10, 0.15).

% Extraction over time (rapid accumulation):
narrative_ontology:measurement(tmcg_ex_t0, trump_making_china_great_2026, base_extractiveness, 0, 0.40).
narrative_ontology:measurement(tmcg_ex_t5, trump_making_china_great_2026, base_extractiveness, 5, 0.55).
narrative_ontology:measurement(tmcg_ex_t10, trump_making_china_great_2026, base_extractiveness, 10, 0.65).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA (v5.0-5.2)
   ========================================================================== */

% The constraint fundamentally re-orders the global system, acting as a new
% (and chaotic) form of global infrastructure.
narrative_ontology:coordination_type(trump_making_china_great_2026, global_infrastructure).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */