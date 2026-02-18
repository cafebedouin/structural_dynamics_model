% ============================================================================
% CONSTRAINT STORY: tail_risk_compression
% ============================================================================
% Version: 5.2 (Deferential Realism Core + Boltzmann + Purity + Network)
% Logic: 5.2 (Indexed Tuple P,T,E,S + Coupling + Purity + Network Drift)
% Generated: 2024-07-23
% ============================================================================

:- module(constraint_tail_risk_compression, []).

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
 * * constraint_id: tail_risk_compression
 * human_readable: The Volatility Suppression Trap
 * domain: economic/technological
 * * SUMMARY:
 * A scenario where systemic risks are artificially suppressed to maintain
 * short-term stability, effectively "compressing" volatility into a hidden
 * but catastrophic tail-end event. It functions as a Rope for current
 * managers but a massive, deferred Snare for those present when the
 * compression inevitably fails.
 * * KEY AGENTS:
 * - Market Participant: Subject (Powerless)
 * - Central Risk Authority: Beneficiary (Institutional)
 * - Actuarial Auditor: Auditor (Analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% High extraction (0.88) because the suppression siphons the system's ability
% to self-correct, charging a "stability fee" that pays out in total collapse.
domain_priors:base_extractiveness(tail_risk_compression, 0.88).
domain_priors:suppression_score(tail_risk_compression, 0.72).
domain_priors:theater_ratio(tail_risk_compression, 0.65).

% Constraint metric facts — primary keys used by the classification engine.
% These mirror domain_priors values using the metric key names from config.pl.
narrative_ontology:constraint_metric(tail_risk_compression, extractiveness, 0.88).
narrative_ontology:constraint_metric(tail_risk_compression, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(tail_risk_compression, theater_ratio, 0.65).

% Constraint self-claim (what does the constraint claim to be?)
% It claims to be a coordination mechanism for market stability.
narrative_ontology:constraint_claim(tail_risk_compression, tangled_rope).
narrative_ontology:human_readable(tail_risk_compression, "The Volatility Suppression Trap").
narrative_ontology:topic_domain(tail_risk_compression, "economic/technological").

% Binary flags
% Requires active enforcement to suppress natural market volatility.
domain_priors:requires_active_enforcement(tail_risk_compression).

% Structural property derivation hooks:
%   has_coordination_function/1 is DERIVED from constraint_beneficiary/2
%   has_asymmetric_extraction/1 is DERIVED from constraint_victim/2
% Both are required for Tangled Rope.
narrative_ontology:constraint_beneficiary(tail_risk_compression, central_risk_authority).
narrative_ontology:constraint_victim(tail_risk_compression, market_participant).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × π(P) × σ(S)
   Power (P) and Scope (S) both affect effective extraction.
   Scope modifiers: local=0.8, regional=0.9, national=1.0,
                    continental=1.1, global=1.2, universal=1.0.
   ========================================================================== */

% PERSPECTIVE 1: THE SUBJECT (SNARE)
% For the individual, the compression is a snare: they are forced into a system
% that masks its own impending destruction.
constraint_indexing:constraint_classification(tail_risk_compression, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE BENEFICIARY (ROPE)
% The authority views compression as a vital Rope—a coordination mechanism to
% prevent localized panics and ensure smooth institutional functioning.
constraint_indexing:constraint_classification(tail_risk_compression, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% Detects the hybrid nature of providing current coordination while
% engineering a future extraction event.
constraint_indexing:constraint_classification(tail_risk_compression, tangled_rope,
    context(agent_power(analytical),
            time_horizon(historical),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(tail_risk_compression_tests).

test(perspectival_gap) :-
    % Verify the constraint is a Snare for the powerless but a Rope for the institution.
    constraint_indexing:constraint_classification(tail_risk_compression, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(tail_risk_compression, TypeInstitutional, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeInstitutional,
    TypePowerless = snare,
    TypeInstitutional = rope.

test(threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(tail_risk_compression, ExtMetricName, E),
    (E =< 0.15 -> true ; E >= 0.46). % Ensures it's either a low-extraction type or high-extraction Snare/Tangled.

:- end_tests(tail_risk_compression_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The extraction score (0.88) reflects the extreme "Mandatrophy" where
 * coordination exists only as a byproduct of deferred systemic failure. The
 * suppression score (0.72) represents the active measures needed to prevent
 * natural price discovery and volatility.
 * * PERSPECTIVAL GAP:
 * The individual feels a Snare because they are trapped in a system that
 * privatizes short-term gains (for beneficiaries) and socializes long-term
 * catastrophic losses (onto victims). The institution sees a Rope because
 * the immediate lack of volatility allows for continued coordination and
 * operation, fulfilling its mandate for stability.
 * * [RESOLVED MANDATROPHY]:
 * Resolved by the Tangled Rope classification. This recognizes that the
 * system is still performing a coordination function (market order), even
 * if that function is highly predatory in its long-term extraction profile.
 * The presence of beneficiaries, victims, and active enforcement confirms this
 * hybrid nature.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% Required for high-extraction constraints (> 0.46).
omega_variable(
    omega_tail_risk_compression,
    'Is the compression a result of deliberate regulatory intent (Snare) or an emergent byproduct of computational efficiency and risk modeling limitations (Mountain)?',
    'Historical analysis of regulatory design documents vs. simulating system response to extreme out-of-distribution shocks.',
    'If intent-driven: Tangled Rope/Snare. If emergent byproduct: Mountain of technical debt.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing.
narrative_ontology:interval(tail_risk_compression, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Temporal data enables drift detection. This models a system where the
% underlying risk (extraction) accumulates over time, and the performative
% aspects of "managing" it also increase.
%
% Theater ratio over time (triggers metric_substitution detection):
narrative_ontology:measurement(trc_tr_t0, tail_risk_compression, theater_ratio, 0, 0.40).
narrative_ontology:measurement(trc_tr_t5, tail_risk_compression, theater_ratio, 5, 0.55).
narrative_ontology:measurement(trc_tr_t10, tail_risk_compression, theater_ratio, 10, 0.65).

% Extraction over time (triggers extraction_accumulation detection):
narrative_ontology:measurement(trc_ex_t0, tail_risk_compression, base_extractiveness, 0, 0.50).
narrative_ontology:measurement(trc_ex_t5, tail_risk_compression, base_extractiveness, 5, 0.75).
narrative_ontology:measurement(trc_ex_t10, tail_risk_compression, base_extractiveness, 10, 0.88).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA (v5.0-5.2)
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% The constraint actively enforces a state of low volatility.
narrative_ontology:coordination_type(tail_risk_compression, enforcement_mechanism).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */