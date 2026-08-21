% ============================================================================
% CONSTRAINT STORY: total_war_winnability_post1945__structural_contraction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_total_war_winnability_post1945__structural_contraction_reading, []).

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
    narrative_ontology:affects_constraint/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    domain_priors:emerges_naturally/1,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_axiom_grounding/3,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:cs_created_at/2,
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: total_war_winnability_post1945__structural_contraction_reading
 *   human_readable: Total War Winnability: Structural Contraction Reading
 *   domain: international_relations_theory/strategic_studies/commitment_system_analysis
 *
 * SUMMARY:
 *   This constraint story instantiates the 'structural contraction' reading
 *   of the total_war_winnability_post1945 kernel. It posits that the advent
 *   of nuclear weapons fundamentally altered the physical possibility of
 *   total war as a winnable endeavor, moving it from a strategic option to a
 *   structural impossibility. This is distinct from arguments about normative
 *   illegitimacy or cultural shifts, which are seen as consequences rather
 *   than primary causes. The constraint is classified as a Mountain due to
 *   its basis in physical reality.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(total_war_winnability_post1945__structural_contraction_reading, 0.01).
domain_priors:suppression_score(total_war_winnability_post1945__structural_contraction_reading, 0.01).
domain_priors:theater_ratio(total_war_winnability_post1945__structural_contraction_reading, 0.01).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(total_war_winnability_post1945__structural_contraction_reading, extractiveness, 0.01).
narrative_ontology:constraint_metric(total_war_winnability_post1945__structural_contraction_reading, suppression_requirement, 0.01).
narrative_ontology:constraint_metric(total_war_winnability_post1945__structural_contraction_reading, theater_ratio, 0.01).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(total_war_winnability_post1945__structural_contraction_reading, accessibility_collapse, 0.95).
narrative_ontology:constraint_metric(total_war_winnability_post1945__structural_contraction_reading, resistance, 0.01).

% --- Constraint claim ---
narrative_ontology:constraint_claim(total_war_winnability_post1945__structural_contraction_reading, mountain).
narrative_ontology:human_readable(total_war_winnability_post1945__structural_contraction_reading, "Total War Winnability: Structural Contraction Reading").
narrative_ontology:topic_domain(total_war_winnability_post1945__structural_contraction_reading, "international_relations_theory/strategic_studies/commitment_system_analysis").

domain_priors:emerges_naturally(total_war_winnability_post1945__structural_contraction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(total_war_winnability_post1945__structural_contraction_reading, 'fb772f12-c87e-4967-9bfc-25c6d9462e1c').
narrative_ontology:cs_kernel_codification('fb772f12-c87e-4967-9bfc-25c6d9462e1c', implicit).
narrative_ontology:cs_authority_grounding('fb772f12-c87e-4967-9bfc-25c6d9462e1c', self_enforcing).
narrative_ontology:cs_reading_relation('fb772f12-c87e-4967-9bfc-25c6d9462e1c', total_war_winnability_post1945__normative_reading_drop, forecloses).
narrative_ontology:cs_reading_relation('fb772f12-c87e-4967-9bfc-25c6d9462e1c', total_war_winnability_post1945__strategic_culture_drift, forecloses).
narrative_ontology:cs_axiom('fb772f12-c87e-4967-9bfc-25c6d9462e1c', foundational, total_war_is_physically_unwinnable_post1945).
narrative_ontology:cs_axiom_status(total_war_is_physically_unwinnable_post1945, holdable).
narrative_ontology:cs_axiom_grounding('fb772f12-c87e-4967-9bfc-25c6d9462e1c', total_war_is_physically_unwinnable_post1945, empirically_contingent).
narrative_ontology:cs_reference_frame('fb772f12-c87e-4967-9bfc-25c6d9462e1c', pre_nuclear_total_war_paradigm).
narrative_ontology:cs_drift_state('fb772f12-c87e-4967-9bfc-25c6d9462e1c', post_nuclear_era, gap(axiom_overriding, severe, true)).
narrative_ontology:cs_created_at('fb772f12-c87e-4967-9bfc-25c6d9462e1c', '').
narrative_ontology:cs_kernel_id(total_war_winnability_post1945__structural_contraction_reading, total_war_winnability_post1945).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: None. This constraint describes a physical impossibility, not a coordination mechanism.
% TRANSFER_FUNCTION: None. This constraint does not involve transfers between parties.
% ABSENT_VOICES: None. The constraint is a physical reality, not a social construct from which voices could be excluded.
% DISAPPEARANCE_RATIONALE: If the physical impossibility of total war (due to nuclear weapons) were to 'disappear' overnight, it would imply a fundamental change in the laws of physics or the nature of nuclear weapons, which is outside the scope of social or political rearrangement. The physical reality itself would remain, or a new physical reality would emerge.
% FOUNDING_PROBLEM: The problem of total war's destructive potential and the perceived winnability of such conflicts prior to the nuclear age.
% FOUNDING_PROBLEM_CORROBORATION: Strategic theorists, physicists, and historical analysis of the Cold War era widely corroborate the shift in the nature of total war. While the *winnability* aspect is dead, the *destructive potential* remains a live problem, now managed through deterrence rather than direct conflict.
narrative_ontology:disappearance_verdict(total_war_winnability_post1945__structural_contraction_reading, world_unchanged).
narrative_ontology:founding_problem_status(total_war_winnability_post1945__structural_contraction_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(total_war_winnability_post1945__structural_contraction_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(total_war_winnability_post1945__structural_contraction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(total_war_winnability_post1945__structural_contraction_reading, 0.01, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(total_war_winnability_post1945__structural_contraction_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(total_war_winnability_post1945__structural_contraction_reading, ExtMetricName, E),
    domain_priors:suppression_score(total_war_winnability_post1945__structural_contraction_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(total_war_winnability_post1945__structural_contraction_reading),
    narrative_ontology:constraint_metric(total_war_winnability_post1945__structural_contraction_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(total_war_winnability_post1945__structural_contraction_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(total_war_winnability_post1945__structural_contraction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The metrics reflect a genuine Mountain: extractiveness, suppression, and theater_ratio are negligible because physical impossibility does not 'extract' or 'suppress' in a human-mediated sense, nor does it require performance. Accessibility collapse is very high (0.95) because the alternative (winnable total war) is physically foreclosed. Resistance is negligible because one cannot resist a physical law. The claimed type is Mountain, consistent with the structural argument.
 *
 * PERSPECTIVAL GAP:
 *   The core perspectival gap lies in whether the absence of total war is due to physical impossibility (this reading), normative shifts, or cultural evolution. This story asserts the physical basis as primary, implying that other explanations are secondary or derivative. The engine's classification will reflect the structural data, which aligns with a Mountain, while omegas capture the contest over the underlying cause.
 *
 * DIRECTIONALITY LOGIC:
 *   As a physical constraint, there are no direct beneficiaries or victims in an extractive sense. The 'victim set' (populations in a hypothetical nuclear exchange) is a consequence of the constraint's *failure* (i.e., nuclear war occurring despite its unwinnability), not an extraction by the constraint itself. Therefore, no specific agents are declared as beneficiaries or victims of the constraint's operation.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    physical_vs_social_impossibility,
    'Is the absence of total war primarily due to its physical unwinnability (structural contraction) or to social/normative/cultural factors?',
    'Counterfactual analysis of historical events, examination of state behavior in crises, and theoretical work on the nature of deterrence. If states consistently avoid total war even when normative/cultural barriers are weak, it supports the physical impossibility argument.',
    'If resolved towards social factors, the constraint would reclassify from Mountain to a more constructed type (e.g., Rope or Tangled Rope, depending on enforcement and extraction), with identifiable beneficiaries (e.g., states benefiting from the normative order).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(physical_vs_social_impossibility, conceptual, 'Distinguishing physical impossibility from social abandonment of total war.').

omega_variable(
    evidence_for_unwinnability,
    'What empirical evidence definitively demonstrates the physical unwinnability of total war, beyond the catastrophic consequences?',
    'Further development of climate models for nuclear winter scenarios, analysis of command and control vulnerabilities under attack, and studies on societal collapse thresholds. The more robust the evidence for systemic collapse, the stronger the physical impossibility claim.',
    'Stronger empirical evidence for physical unwinnability reinforces the Mountain classification. Weak or contested evidence would lend more weight to alternative, socially constructed readings.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(evidence_for_unwinnability, empirical, 'Empirical grounding for the claim of physical unwinnability.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(total_war_winnability_post1945__structural_contraction_reading, 1945, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(total_war_winnability_post1945__structural_contraction_reading, total_war_winnability_post1945__normative_reading_drop).
narrative_ontology:affects_constraint(total_war_winnability_post1945__structural_contraction_reading, total_war_winnability_post1945__strategic_culture_drift).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'total_war_winnability_post1945' kernel. This 'structural contraction' reading posits physical impossibility, which fundamentally alters the premises of the 'normative drop' and 'strategic culture drift' readings, making them secondary or derivative consequences.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
