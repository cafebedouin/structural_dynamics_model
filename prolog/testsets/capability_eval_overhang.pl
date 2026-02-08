% ============================================================================
% CONSTRAINT STORY: capability_eval_overhang
% ============================================================================
% Version: 5.2 (Deferential Realism Core + Boltzmann + Purity + Network)
% Logic: 5.2 (Indexed Tuple P,T,E,S + Coupling + Purity + Network Drift)
% Generated: 2024-07-27
% ============================================================================

:- module(constraint_capability_eval_overhang, []).

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
 * * constraint_id: capability_eval_overhang
 * human_readable: The Blind Spot of Power
 * domain: technological/AI/governance
 * * SUMMARY:
 * A scenario where the actual capabilities of an AI system significantly exceed
 * the benchmarks and evaluation protocols used to measure them. This creates a
 * "Rope" for developers to coordinate rapid deployment and commercial scaling,
 * but acts as a "Snare" for society, as safety frameworks are based on
 * obsolete metrics, liquidating public security in favor of unpriced
 * computational progress.
 * * KEY AGENTS:
 * - Safety Researcher / Public: Subject (Powerless)
 * - Frontier AI Lab: Beneficiary (Institutional)
 * - Evaluative Auditor: Auditor (Analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% High extraction (0.83) as the "overhang" allows the beneficiary to capture
% the surplus of advanced capabilities while externalizing the "tail risk"
% of unmeasured power onto the subject.
domain_priors:base_extractiveness(capability_eval_overhang, 0.83).
domain_priors:suppression_score(capability_eval_overhang, 0.64).
domain_priors:theater_ratio(capability_eval_overhang, 0.86). % High theater: relying on "passed" evals that don't test emergent risks.

% Constraint metric facts — primary keys used by the classification engine.
narrative_ontology:constraint_metric(capability_eval_overhang, extractiveness, 0.83).
narrative_ontology:constraint_metric(capability_eval_overhang, suppression_requirement, 0.64).
narrative_ontology:constraint_metric(capability_eval_overhang, theater_ratio, 0.86).

% Constraint self-claim (what does the constraint claim to be?)
narrative_ontology:constraint_claim(capability_eval_overhang, piton).

% Binary flags and structural properties for Tangled Rope classification
domain_priors:requires_active_enforcement(capability_eval_overhang).
narrative_ontology:constraint_beneficiary(capability_eval_overhang, frontier_ai_labs).
narrative_ontology:constraint_victim(capability_eval_overhang, public_security).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × π(P) × σ(S)
   ========================================================================== */

% PERSPECTIVE 1: THE SUBJECT (SNARE)
% The subject is trapped: they are forced to participate in a "safety race"
% governed by metrics they know are inadequate to prevent catastrophic failure.
constraint_indexing:constraint_classification(capability_eval_overhang, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE BENEFICIARY (ROPE)
% The institution views the existing eval-stack as a Rope—the essential
% coordination signal required to validate "readiness" for market release
% and regulatory compliance.
constraint_indexing:constraint_classification(capability_eval_overhang, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 3: THE SYSTEMS AUDITOR (PITON)
% Theater ratio (0.86) > 0.70 triggers Piton: the "Benchmark Suite"
% is an inertial spike of logic maintained for optics, not functional safety.
constraint_indexing:constraint_classification(capability_eval_overhang, piton,
    context(agent_power(analytical),
            time_horizon(historical),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 4: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% Detects high extraction (0.83), a coordination function (beneficiary exists),
% asymmetric extraction (victim exists), and active enforcement. This is the
% canonical signature of a Tangled Rope.
constraint_indexing:constraint_classification(capability_eval_overhang, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(capability_eval_overhang_tests).

test(perspectival_gap) :-
    % Verify Snare for the subject vs Rope for the institutional beneficiary.
    constraint_indexing:constraint_classification(capability_eval_overhang, snare,
        context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(capability_eval_overhang, rope,
        context(agent_power(institutional), _, _, _)),
    constraint_indexing:constraint_classification(capability_eval_overhang, tangled_rope,
        context(agent_power(analytical), _, _, _)).

test(piton_trigger) :-
    % Ensure high theater ratio (0.86) correctly triggers the Piton classification.
    constraint_indexing:constraint_classification(capability_eval_overhang, piton,
        context(agent_power(analytical), _, _, _)).

test(tangled_rope_structural_properties) :-
    % Verify all three required properties for Tangled Rope are present.
    narrative_ontology:constraint_beneficiary(capability_eval_overhang, _),
    narrative_ontology:constraint_victim(capability_eval_overhang, _),
    domain_priors:requires_active_enforcement(capability_eval_overhang).

:- end_tests(capability_eval_overhang_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The extraction score (0.83) reflects a state where the "coordination"
 * benefit of institutional safety checks is eclipsed by the predatory
 * risk-taking of deploying unmeasured power. The high theater ratio (0.86)
 * confirms that the evaluation process is more performative than functional.
 *
 * * PERSPECTIVAL GAP:
 * The Safety Researcher and public feel a Snare because they are bound by a
 * system that only sees the "visible" capabilities, externalizing unmeasured
 * risk onto them. The Frontier Lab sees a Rope because the evals provide a
 * standardized coordination path to prove "safety" to investors and regulators.
 *
 * * MANDATROPHY ANALYSIS:
 * [RESOLVED MANDATROPHY] The system resolves this by classifying it as a
 * Tangled Rope from the analytical perspective. This acknowledges the valid
 * coordination function (passing evals) while correctly identifying the
 * asymmetric, coercive extraction (externalizing tail risk). The Piton
 * classification further highlights the functional decay of the safety mechanism.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% Required for high-extraction constraints (> 0.46).
omega_variable(
    omega_emergence_detection,
    'Can "black box" capabilities be forecasted, or is emergent power a surprise (Snare vs Mountain)?',
    'Tracking the delta between "predicted capability onset" and "actual bench discovery" over time.',
    'If predictable: Snare of current policy. If unpredictable: Mountain of Information Theory.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(capability_eval_overhang, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% This constraint models a system degrading over time. Initially, evals were
% functional (low theater, lower extraction). As AI capabilities outpaced the
% evals, the theater ratio and the extractive potential of the "overhang" grew.
%
% Theater ratio over time (triggers metric_substitution detection):
narrative_ontology:measurement(ceo_tr_t0, capability_eval_overhang, theater_ratio, 0, 0.20).
narrative_ontology:measurement(ceo_tr_t5, capability_eval_overhang, theater_ratio, 5, 0.65).
narrative_ontology:measurement(ceo_tr_t10, capability_eval_overhang, theater_ratio, 10, 0.86).

% Extraction over time (triggers extraction_accumulation detection):
narrative_ontology:measurement(ceo_ex_t0, capability_eval_overhang, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(ceo_ex_t5, capability_eval_overhang, base_extractiveness, 5, 0.71).
narrative_ontology:measurement(ceo_ex_t10, capability_eval_overhang, base_extractiveness, 10, 0.83).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA (v5.0-5.2)
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% The evaluation benchmarks serve as a standard for communicating safety.
narrative_ontology:coordination_type(capability_eval_overhang, information_standard).

% Network relationships (structural influence edges)
% The decay of evaluation integrity directly impacts public trust in the technology.
narrative_ontology:affects_constraint(capability_eval_overhang, public_trust_in_ai).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */