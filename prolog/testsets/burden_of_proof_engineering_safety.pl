% ============================================================================
% CONSTRAINT STORY: burden_of_proof_engineering_safety
% ============================================================================
% Version: 5.2 (Deferential Realism Core + Boltzmann + Purity + Network)
% Logic: 5.2 (Indexed Tuple P,T,E,S + Coupling + Purity + Network Drift)
% Generated: 2024-05-21
% ============================================================================

:- module(constraint_burden_of_proof_engineering_safety, []).

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
 * * constraint_id: burden_of_proof_engineering_safety
 * human_readable: Reversal of the Engineering Burden of Proof
 * domain: technological
 * * SUMMARY:
 * This constraint represents a specific institutional failure where the
 * "burden of proof" shifted from requiring proof that it was SAFE to fly
 * to requiring proof that it was UNSAFE to fly. This technical/
 * engineering variety of the constraint is distinct from legal burdens
 * as it operates on the threshold of physical system failure rather
 * than social culpability, as seen in the 1986 Challenger Launch Decision.
 * * KEY AGENTS:
 * - Thiokol Engineers (e.g., Roger Boisjoly): Subject (Powerless)
 * - NASA Management (e.g., Lawrence Mulloy): Beneficiary (Institutional)
 * - The Rogers Commission: Auditor (Analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Numerical anchors for v3.4 thresholds
domain_priors:base_extractiveness(burden_of_proof_engineering_safety, 0.9). % Rationale: Reversing the burden extracts the safety margin, ultimately trading crew life for schedule adherence.
domain_priors:suppression_score(burden_of_proof_engineering_safety, 0.8).   % Rationale: The demand for "conclusive data" of failure suppresses valid engineering intuition and historical erosion data.
domain_priors:theater_ratio(burden_of_proof_engineering_safety, 0.1).       % Rationale: The constraint was brutally functional, not theatrical.

% Constraint metric facts — primary keys used by the classification engine.
narrative_ontology:constraint_metric(burden_of_proof_engineering_safety, extractiveness, 0.9).
narrative_ontology:constraint_metric(burden_of_proof_engineering_safety, suppression_requirement, 0.8).
narrative_ontology:constraint_metric(burden_of_proof_engineering_safety, theater_ratio, 0.1).

% Constraint self-claim (what does the constraint claim to be?)
% It claims to be a coordination mechanism for making decisions under uncertainty.
narrative_ontology:constraint_claim(burden_of_proof_engineering_safety, snare).

% Binary flags
domain_priors:requires_active_enforcement(burden_of_proof_engineering_safety). % NASA management explicitly demanded Thiokol "prove it" was unsafe.

% Structural property derivation hooks:
narrative_ontology:constraint_beneficiary(burden_of_proof_engineering_safety, nasa_launch_schedule).
narrative_ontology:constraint_victim(burden_of_proof_engineering_safety, crew_safety).
narrative_ontology:constraint_victim(burden_of_proof_engineering_safety, engineering_integrity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × π(P) × σ(S)
   Power (P) and Scope (S) both affect effective extraction.
   ========================================================================== */

% PERSPECTIVE 1: THE SUBJECT (SNARE)
% The engineer is trapped by a demand for impossible proof. The effective
% extraction χ = 0.9 (ε) * 1.5 (π(powerless)) * 0.8 (σ(local)) = 1.08, which
% is an extreme Snare. It feels like a Mountain due to its immovability, but
% its constructed, high-extraction nature makes it a Snare.
constraint_indexing:constraint_classification(burden_of_proof_engineering_safety, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: THE BENEFICIARY (ROPE)
% Management sees this as a tool to filter "emotional" engineering concerns
% and coordinate a complex launch. The effective extraction is negative:
% χ = 0.9 (ε) * -0.2 (π(institutional)) * 1.0 (σ(national)) = -0.18.
% This classifies as a Rope, creating a severe perspectival gap.
constraint_indexing:constraint_classification(burden_of_proof_engineering_safety, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (SNARE)
% The Rogers Commission, with analytical distance, sees the system's true
% nature. The effective extraction is very high:
% χ = 0.9 (ε) * 1.15 (π(analytical)) * 1.2 (σ(global)) = 1.242.
% This is a clear Snare that chokes out safety data to serve a schedule.
constraint_indexing:constraint_classification(burden_of_proof_engineering_safety, snare,
    context(agent_power(analytical),
            time_horizon(historical),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(burden_of_proof_engineering_safety_tests).

test(perspectival_gap_snare_vs_rope) :-
    % Verify the gap between the engineer (powerless) and management (institutional).
    constraint_indexing:constraint_classification(burden_of_proof_engineering_safety, TypePowerless,
        context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(burden_of_proof_engineering_safety, TypeInstitutional,
        context(agent_power(institutional), _, _, _)),
    assertion(TypePowerless == snare),
    assertion(TypeInstitutional == rope),
    TypePowerless \= TypeInstitutional.

test(high_extraction_and_suppression) :-
    narrative_ontology:constraint_metric(burden_of_proof_engineering_safety, extractiveness, E),
    narrative_ontology:constraint_metric(burden_of_proof_engineering_safety, suppression_requirement, S),
    assertion(E >= 0.9),
    assertion(S >= 0.8).

:- end_tests(burden_of_proof_engineering_safety_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The core of this constraint is the perspectival gap between a Snare and a Rope.
 * For NASA management (institutional), demanding conclusive proof of failure was
 * a coordination mechanism (Rope) to prevent delays based on what they perceived
 * as ambiguous engineering concerns. For the engineers (powerless), this demand
 * was an impossible task that suppressed valid safety data, extracting the crew's
 * lives to meet a schedule (Snare). The engineer feels trapped as if by a Mountain,
 * but the constraint is artificial and extractive, hence it is a Snare.
 * The analysis confirms that the presence of a safer, traditional alternative
 * (requiring proof of SAFETY) that was actively suppressed solidifies the Snare
 * classification from any non-beneficiary perspective.
 *
 * [RESOLVED MANDATROPHY]
 * The system correctly identifies this as a high-extraction Snare, not a
 * Tangled Rope, because the "coordination" function claimed by management is
 * entirely subsumed by its extractive purpose. There is no genuine, separable
 * coordination benefit for the victims; the coordination only serves the extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_burden_of_proof,
    "Can an engineering organization ever define a 'safe' burden of proof that is not ultimately a subjective value judgment?",
    "Comparative analysis of probabilistic risk assessment (PRA) standards across nuclear vs. aerospace industries.",
    "If Yes: The burden of proof can be a stable Rope. If No: It is always at risk of degrading into a Snare or an institutional Mountain.",
    confidence_without_resolution(low)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(burden_of_proof_engineering_safety, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Temporal data models the degradation of NASA's safety culture leading up
% to the Challenger disaster. Extraction (pressure to launch) increased while
% the process remained brutally functional (low theater).
%
% Theater ratio over time (metric_substitution):
narrative_ontology:measurement(bopes_tr_t0, burden_of_proof_engineering_safety, theater_ratio, 0, 0.05).
narrative_ontology:measurement(bopes_tr_t5, burden_of_proof_engineering_safety, theater_ratio, 5, 0.08).
narrative_ontology:measurement(bopes_tr_t10, burden_of_proof_engineering_safety, theater_ratio, 10, 0.1).

% Extraction over time (extraction_accumulation):
narrative_ontology:measurement(bopes_ex_t0, burden_of_proof_engineering_safety, base_extractiveness, 0, 0.7).
narrative_ontology:measurement(bopes_ex_t5, burden_of_proof_engineering_safety, base_extractiveness, 5, 0.8).
narrative_ontology:measurement(bopes_ex_t10, burden_of_proof_engineering_safety, base_extractiveness, 10, 0.9).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA (v5.0-5.2)
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% The constraint functions as a perverse rule for decision-making.
narrative_ontology:coordination_type(burden_of_proof_engineering_safety, enforcement_mechanism).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */