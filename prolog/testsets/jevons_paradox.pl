% ============================================================================
% CONSTRAINT STORY: jevons_paradox
% ============================================================================
% Version: 5.2 (Deferential Realism Core + Boltzmann + Purity + Network)
% Logic: 5.2 (Indexed Tuple P,T,E,S + Coupling + Purity + Network Drift)
% Generated: 2024-07-15
% ============================================================================

:- module(constraint_jevons_paradox, []).

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
 * * constraint_id: jevons_paradox
 * human_readable: Jevons Paradox (The Rebound Effect)
 * domain: economic/technological
 * * SUMMARY:
 * Jevons Paradox occurs when technological progress increases the efficiency with which a resource is used, but the rate of consumption of that resource rises because of increasing demand. This counter-intuitive result suggests that efficiency alone cannot solve resource depletion; it often accelerates it by making the resource cheaper and more accessible.
 * * KEY AGENTS:
 * - Environmental Conservationist: Subject (Powerless)
 * - Industrial Capitalist: Beneficiary (Institutional)
 * - Systems Ecologist: Auditor (Analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Numerical anchors for v3.4 thresholds
domain_priors:base_extractiveness(jevons_paradox, 0.60). % Mountain <= 0.15, Rope <= 0.15, Snare >= 0.46
domain_priors:suppression_score(jevons_paradox, 0.50).   % Structural property (raw, unscaled).
domain_priors:theater_ratio(jevons_paradox, 0.10).       % Piton detection (>= 0.70)

% Constraint metric facts — primary keys used by the classification engine.
narrative_ontology:constraint_metric(jevons_paradox, extractiveness, 0.60).
narrative_ontology:constraint_metric(jevons_paradox, suppression_requirement, 0.50).
narrative_ontology:constraint_metric(jevons_paradox, theater_ratio, 0.10).

% Constraint self-claim (what does the constraint claim to be?)
% It claims to be a coordination mechanism for economic growth via efficiency.
narrative_ontology:constraint_claim(jevons_paradox, tangled_rope).
narrative_ontology:human_readable(jevons_paradox, "Jevons Paradox (The Rebound Effect)").
narrative_ontology:topic_domain(jevons_paradox, "economic/technological").

% Binary flags
domain_priors:requires_active_enforcement(jevons_paradox). % Required for Tangled Rope. Enforced by market price signals.

% Structural property derivation hooks:
%   has_coordination_function/1 is DERIVED from constraint_beneficiary/2
%   has_asymmetric_extraction/1 is DERIVED from constraint_victim/2
narrative_ontology:constraint_beneficiary(jevons_paradox, industrial_producers).
narrative_ontology:constraint_victim(jevons_paradox, planetary_ecosystems).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × π(P) × σ(S)
   Power (P) and Scope (S) both affect effective extraction.
   Scope modifiers: local=0.8, regional=0.9, national=1.0,
                    continental=1.1, global=1.2, universal=1.0.
   ========================================================================== */

% PERSPECTIVE 1: THE SUBJECT (SNARE)
% The environmentalist sees efficiency gains being extracted to fuel more consumption,
% trapping conservation efforts in a cycle of failure.
constraint_indexing:constraint_classification(jevons_paradox, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: THE BENEFICIARY (ROPE)
% The industrialist sees efficiency as a pure coordination tool (a Rope) to
% lower costs, expand markets, and increase production, benefiting everyone.
constraint_indexing:constraint_classification(jevons_paradox, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% The analyst sees both the coordination function (Rope for industry) and the
% asymmetric extraction (from the environment), enforced by market logic. This
% hybrid nature defines it as a Tangled Rope.
constraint_indexing:constraint_classification(jevons_paradox, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jevons_paradox_tests).

test(perspectival_gap) :-
    % Verify there is a perspectival gap between powerless and institutional.
    constraint_indexing:constraint_classification(jevons_paradox, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(jevons_paradox, TypeInstitutional, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeInstitutional,
    TypePowerless == snare,
    TypeInstitutional == rope.

test(analytical_classification_is_tangled_rope) :-
    constraint_indexing:constraint_classification(jevons_paradox, tangled_rope, context(agent_power(analytical), _, _, _)).

test(threshold_validation_high_extraction) :-
    narrative_ontology:constraint_metric(jevons_paradox, extractiveness, E),
    E >= 0.46. % Ensures it's classified as a high-extraction constraint.

:- end_tests(jevons_paradox_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The Jevons Paradox is a canonical example of a Tangled Rope. It has a genuine, undeniable coordination function: efficiency gains lower costs and expand access to goods, which benefits producers and consumers (the Rope perspective). However, it simultaneously imposes a severe negative externality by extracting from a non-consenting party (the global ecosystem and future generations), which is a classic Snare dynamic.
 *
 * The base extractiveness of 0.60 reflects the high cost imposed on the environment. The suppression score of 0.50 reflects how the dominant economic narrative of "growth through efficiency" marginalizes alternative models like "sufficiency" or "degrowth."
 *
 * The perspectival gap is stark: for the institutional beneficiary, it's a Rope for progress. For the powerless victim (the conservationist acting as a proxy for the ecosystem), it's a Snare that makes their efforts counter-productive. The analytical observer, seeing both sides, must classify it as a Tangled Rope.
 *
 * * MANDATROPHY ANALYSIS:
 * Classifying this as a Tangled Rope is critical. A naive analysis might see it as a "Mountain" (an immutable law of economics) or a "Snare" (a purely malicious trap). The Tangled Rope classification correctly identifies that the constraint's power comes from its *dual nature*. It persists because its coordination function is genuinely valuable to powerful actors, who are thus incentivized to ignore or downplay its extractive consequences. This prevents the system from mislabeling a constructed economic dynamic as a natural law or a simple conspiracy.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_jevons_paradox,
    'Is the rebound effect (>100% backfire) an inherent feature of market economies (Mountain), or could it be negated by cultural shifts towards sufficiency without coercive caps (making it a contingent Snare)?',
    'Long-term empirical studies of economies that attempt to decouple GDP growth from resource consumption through non-legislative means (e.g., cultural campaigns).',
    'If Mountain: Only hard caps can work. If contingent Snare: It is possible to change behavior to escape the trap.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(jevons_paradox, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Temporal data models how the paradox's extractive nature became more apparent
% and entrenched over the industrial and post-industrial eras.
% Required for high-extraction constraints (base_extractiveness > 0.46).

% Theater ratio over time (slight increase as "greenwashing" becomes a strategy):
narrative_ontology:measurement(jevons_paradox_tr_t0, jevons_paradox, theater_ratio, 0, 0.05).
narrative_ontology:measurement(jevons_paradox_tr_t5, jevons_paradox, theater_ratio, 5, 0.08).
narrative_ontology:measurement(jevons_paradox_tr_t10, jevons_paradox, theater_ratio, 10, 0.10).

% Extraction over time (as global consumption scales, the extraction from the commons intensifies):
narrative_ontology:measurement(jevons_paradox_ex_t0, jevons_paradox, base_extractiveness, 0, 0.50).
narrative_ontology:measurement(jevons_paradox_ex_t5, jevons_paradox, base_extractiveness, 5, 0.55).
narrative_ontology:measurement(jevons_paradox_ex_t10, jevons_paradox, base_extractiveness, 10, 0.60).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA (v5.0-5.2)
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% The paradox operates as a dynamic within resource allocation systems.
narrative_ontology:coordination_type(jevons_paradox, resource_allocation).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */