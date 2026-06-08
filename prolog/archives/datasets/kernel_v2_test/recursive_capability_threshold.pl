% ============================================================================
% CONSTRAINT STORY: recursive_capability_threshold
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_recursive_capability_threshold, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

% --- Constraint Identity Rule (DP-001: ε-Invariance) ---
% Each constraint story must have a single, stable base extractiveness (ε).
% If changing the observable used to evaluate this constraint would change ε,
% you are looking at two distinct constraints. Write separate .pl files for
% each, link them with affects_constraint/2, and document the relationship
% in both files' narrative context sections.
%
% The context tuple is CLOSED at arity 4: (P, T, E, S).
% Do not add measurement_basis, beneficiary/victim, or any other arguments.
% Linter Rule 23 enforces context/4.
%
% See: epsilon_invariance_principle.md

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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    domain_priors:emerges_naturally/1,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: recursive_capability_threshold
 *   human_readable: Recursive Capability Threshold: The Judgment Gap Between Automation and Autonomy
 *   domain: ai_development/technology_governance/organizational_dynamics
 *
 * SUMMARY:
 *   The recursive capability threshold describes the gap between AI systems
 *   that can execute specified experiments (automation) and systems that can
 *   select which experiments matter (autonomy). This is the judgment gap: the
 *   difference between optimizing over a known objective and reasoning about
 *   what objectives are worth pursuing. Current AI systems excel at the
 *   former — given a well-specified task, they can often match or exceed
 *   human performance. But selecting research directions, identifying
 *   promising problems, or determining what questions are worth asking
 *   remains firmly in human territory. This constraint is claimed as a
 *   Mountain: a genuine discontinuity in capability space that would persist
 *   regardless of institutional arrangements. However, the constraint
 *   declares beneficiaries (safety research community, evaluation labs,
 *   governance institutions), triggering False Summit evaluation. The
 *   beneficiary structure raises the question: is this a natural law of
 *   intelligence, or a contingent feature of current paradigms that serves
 *   coordination functions?
 *
 * KEY AGENTS:
 *   - Junior ML Engineer: Powerless/trapped — experiences the gap as an immediate technical barrier within development cycles
 *   - Research Lab Director: Moderate/constrained — observes the gap's persistence across career timescales and multiple approaches
 *   - AI Safety Research Community: Institutional/arbitrage — benefits from the threshold as a coordination point for safety work and governance proposals
 *   - Capability Evaluation Lab: Institutional/arbitrage — benefits from the gap's persistence as justification for evaluation infrastructure
 *   - Governance Policy Institution: Organized/mobile — uses the threshold as a regulatory trigger point and demarcation line for policy
 *   - Analytical Observer: Analytical/analytical — sees the gap as a genuine structural feature of intelligence, not a coordination problem
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(recursive_capability_threshold, 0.08).
domain_priors:suppression_score(recursive_capability_threshold, 0.12).
domain_priors:theater_ratio(recursive_capability_threshold, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(recursive_capability_threshold, extractiveness, 0.08).
narrative_ontology:constraint_metric(recursive_capability_threshold, suppression_requirement, 0.12).
narrative_ontology:constraint_metric(recursive_capability_threshold, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(recursive_capability_threshold, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(recursive_capability_threshold, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(recursive_capability_threshold, mountain).
narrative_ontology:human_readable(recursive_capability_threshold, "Recursive Capability Threshold: The Judgment Gap Between Automation and Autonomy").
narrative_ontology:topic_domain(recursive_capability_threshold, "ai_development/technology_governance/organizational_dynamics").

domain_priors:emerges_naturally(recursive_capability_threshold).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(recursive_capability_threshold, ai_safety_research_community).
narrative_ontology:constraint_beneficiary(recursive_capability_threshold, capability_evaluation_labs).
narrative_ontology:constraint_beneficiary(recursive_capability_threshold, governance_policy_institutions).
narrative_ontology:constraint_vindicates(recursive_capability_threshold, intelligence_explosion_hypothesis).
narrative_ontology:constraint_vindicates(recursive_capability_threshold, alignment_difficulty_thesis).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: JUNIOR ML ENGINEER (MOUNTAIN) — Experiences the judgment gap as an immutable technical barrier. Cannot distinguish between 'my model automated the task' and 'my model understands what tasks matter' from within the immediate development cycle. The threshold appears as a natural law of system capability.
constraint_indexing:constraint_classification(recursive_capability_threshold, mountain,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: RESEARCH LAB DIRECTOR (MOUNTAIN) — Over career timescales, observes repeated failures of systems to cross from execution to judgment. The gap persists across architectures, training regimes, and scale. Appears as a fundamental limit in current paradigms, not a coordination problem to be solved.
constraint_indexing:constraint_classification(recursive_capability_threshold, mountain,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: AI SAFETY RESEARCH COMMUNITY (MOUNTAIN) — Benefits from the threshold's existence as a coordination point for safety research and governance proposals. The gap between automation and autonomy provides a natural Schelling point for intervention. Experiences negligible extraction — the constraint enables rather than extracts from safety work.
constraint_indexing:constraint_classification(recursive_capability_threshold, mountain,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: CAPABILITY EVALUATION LAB (MOUNTAIN) — Designs benchmarks around the threshold. Benefits from the gap's persistence as it justifies evaluation infrastructure. The constraint is experienced as a natural feature of the capability landscape that evaluation must track, not as something the lab extracts from.
constraint_indexing:constraint_classification(recursive_capability_threshold, mountain,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: GOVERNANCE POLICY INSTITUTION (MOUNTAIN) — Uses the threshold as a regulatory trigger point. Benefits from its clarity as a demarcation line between 'tool AI' and 'agent AI' for policy purposes. The constraint provides a natural coordination point for international governance frameworks.
constraint_indexing:constraint_classification(recursive_capability_threshold, mountain,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (MOUNTAIN) — The judgment gap reflects a genuine discontinuity in capability space. Executing specified experiments requires optimization over a known objective; selecting which experiments matter requires meta-level reasoning about what constitutes progress. This is not a coordination problem or an institutional arrangement — it is a structural feature of intelligence itself. The threshold may eventually be crossed, but its existence is not contingent on who defends it.
constraint_indexing:constraint_classification(recursive_capability_threshold, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(recursive_capability_threshold_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(recursive_capability_threshold, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(recursive_capability_threshold, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(recursive_capability_threshold, ExtMetricName, E),
    domain_priors:suppression_score(recursive_capability_threshold, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(recursive_capability_threshold),
    narrative_ontology:constraint_metric(recursive_capability_threshold, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(recursive_capability_threshold, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(recursive_capability_threshold_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.08): Very low. The threshold extracts minimally from those it governs. Junior engineers face a technical barrier, but this is experienced as a learning challenge rather than systematic extraction. The slight extraction reflects that some resources flow toward threshold-adjacent work (safety research, evaluation infrastructure) that might otherwise be allocated differently. Suppression (0.12): Very low. The threshold does not suppress alternatives through coercion. Multiple approaches to crossing the gap remain viable and actively pursued. The low suppression reflects that the constraint operates as a technical barrier rather than an enforced prohibition. Theater ratio (0.15): Very low. Minimal performative content. The gap is measured through actual capability evaluations, not theatrical demonstrations. The slight theater reflects some definitional ambiguity around what constitutes 'judgment' versus 'execution,' but the core phenomenon is functionally real. Accessibility collapse (0.92): Very high. Once the distinction between automation and autonomy is understood, alternative framings collapse. The gap is not a matter of perspective — systems either can or cannot select their own research directions. Resistance (0.08): Very low. The threshold meets minimal active resistance. No one is defending the gap's existence; it persists despite substantial effort to cross it. The measurements show slight increases in extractiveness and theater over the 2015-2026 interval, reflecting growing institutional infrastructure around the threshold (evaluation labs, safety research programs, governance frameworks). But the increases are modest — the constraint remains primarily a technical barrier rather than an institutional arrangement.
 *
 * PERSPECTIVAL GAP:
 *   This constraint shows minimal perspectival gap — all six perspectives classify as Mountain. This uniformity is diagnostic: either the constraint is a genuine natural law (accessibility collapse is real, alternatives are exhausted), or it is a false summit so thoroughly naturalized that even analytical observers cannot see the construction. The beneficiary structure suggests the latter possibility. The safety research community, evaluation labs, and governance institutions all benefit from the threshold's persistence. They coordinate around it, build infrastructure on it, and use it as a Schelling point for intervention. This does not prove the threshold is constructed — genuine natural laws can have beneficiaries (the speed of light benefits physicists who study relativity). But it raises the question: would these institutions have the same incentive structure if the gap were bridged? The omega variables document this ambiguity. The constraint's low extraction and suppression support the Mountain classification — it does not operate through coercion or rent extraction. But the beneficiary structure and the paradigm-contingency question (omega_3) leave open the possibility that current ML methods create a capability gap that alternative approaches might not face.
 *
 * DIRECTIONALITY LOGIC:
 *   All perspectives classify as Mountain, but with different directionality values reflecting their structural positions. The junior engineer (powerless/trapped) experiences the threshold as a pure barrier — high d, but the mountain classification means effective extraction remains low because the constraint is not extractive in nature. The research lab director (moderate/constrained) has more context and resources but still faces the same fundamental gap. The institutional beneficiaries (safety community, evaluation labs, governance institutions) have low d values — they benefit from the threshold's existence as a coordination point. The analytical observer has d ≈ 0.5 (symmetric) — neither benefits nor bears costs, simply observes the structural feature. The beneficiary declarations trigger False Summit evaluation: if the threshold serves institutional coordination functions, it may be a constructed constraint naturalized as law. The omega variables document this irreducible uncertainty.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint demonstrates the False Summit detection mechanism. A claimed Mountain with declared beneficiaries triggers FSM evaluation. The engine will assess whether the constraint's metric profile (low extraction, negligible suppression, high accessibility collapse, low resistance) combined with beneficiary presence indicates naturalization of a contingent arrangement. The mandatrophy resolution here is not 'which type is correct?' but 'is this a genuine natural law or a constructed constraint that benefits identifiable agents?' The omega variables document the irreducible uncertainties: Is the gap discrete or continuous? Is it paradigm-contingent? Can judgment be operationalized without human framing? These questions cannot be resolved from within current capability evaluation frameworks — they require either crossing the threshold or developing alternative paradigms that reveal whether the gap was fundamental or contingent.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    threshold_vs_gradient,
    'Is the judgment gap a discrete threshold or a continuous gradient that appears threshold-like due to measurement artifacts?',
    'Fine-grained capability evaluation across the automation-autonomy spectrum; identification of intermediate stages where systems show partial judgment capacity',
    'If discrete threshold: Mountain classification holds — a genuine discontinuity in capability space. If continuous gradient: the ''threshold'' is a measurement artifact or social construction, potentially reclassifying toward Rope (coordination around an arbitrary cut-point).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(threshold_vs_gradient, empirical, 'Whether the capability gap is discrete or continuous').

omega_variable(
    beneficiary_naturalization_risk,
    'Do the declared beneficiaries (safety research community, evaluation labs, governance institutions) benefit from naturalizing a contingent capability gap as an immutable threshold?',
    'Historical analysis of capability evaluation discourse; identification of institutional incentives to maintain threshold framing; comparison with other capability gaps that were later bridged',
    'If naturalization is occurring: False Summit — the constraint is presented as natural law but serves institutional coordination and resource allocation functions. If genuinely natural: Mountain classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_naturalization_risk, conceptual, 'Whether beneficiary structure indicates false summit dynamics').

omega_variable(
    paradigm_contingency,
    'Is the judgment gap contingent on current ML paradigms (gradient descent, transformer architectures, supervised learning) or would it persist under alternative approaches to building intelligent systems?',
    'Cross-paradigm capability analysis; evaluation of symbolic AI, neurosymbolic systems, or future architectures on judgment tasks; theoretical analysis of what computational structures enable meta-level reasoning',
    'If paradigm-contingent: the threshold is a feature of current methods, not intelligence itself — potentially reclassifying toward Tangled Rope (coordination around current paradigm with extraction from alternative approaches). If paradigm-invariant: Mountain holds.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(paradigm_contingency, empirical, 'Whether the threshold is paradigm-specific or fundamental').

omega_variable(
    measurement_operationalization,
    'Can ''selecting which experiments matter'' be operationalized without smuggling in human judgment about what constitutes ''mattering''?',
    'Development of objective metrics for research direction quality; longitudinal tracking of self-directed AI research projects; comparison with human researcher success rates on equivalent open-ended problems',
    'If operationalizable: empirical resolution of threshold existence becomes possible. If not: the threshold may be a conceptual artifact of how we frame intelligence rather than a measurable capability gap.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(measurement_operationalization, conceptual, 'Whether the judgment gap can be measured objectively').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(recursive_capability_threshold, 0, 11).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(rct_theater_2015, recursive_capability_threshold, theater_ratio, 0, 0.1).
narrative_ontology:measurement(rct_theater_2018, recursive_capability_threshold, theater_ratio, 3, 0.12).
narrative_ontology:measurement(rct_theater_2021, recursive_capability_threshold, theater_ratio, 6, 0.13).
narrative_ontology:measurement(rct_theater_2024, recursive_capability_threshold, theater_ratio, 9, 0.14).
narrative_ontology:measurement(rct_theater_2026, recursive_capability_threshold, theater_ratio, 11, 0.15).

% Extraction over time
narrative_ontology:measurement(rct_extract_2015, recursive_capability_threshold, base_extractiveness, 0, 0.05).
narrative_ontology:measurement(rct_extract_2018, recursive_capability_threshold, base_extractiveness, 3, 0.06).
narrative_ontology:measurement(rct_extract_2021, recursive_capability_threshold, base_extractiveness, 6, 0.07).
narrative_ontology:measurement(rct_extract_2024, recursive_capability_threshold, base_extractiveness, 9, 0.075).
narrative_ontology:measurement(rct_extract_2026, recursive_capability_threshold, base_extractiveness, 11, 0.08).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(recursive_capability_threshold, information_standard).

% DUAL FORMULATION NOTE:
% This constraint is downstream of automation_velocity_vs_oversight_capacity and benchmark_saturation_vs_deployment_gap. The upstream constraints describe coordination and extraction dynamics in AI development; this constraint describes a claimed capability discontinuity that those dynamics operate around. The recursive capability threshold is presented as the natural law that justifies the coordination mechanisms in the upstream constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
