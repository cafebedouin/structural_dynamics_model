% ============================================================================
% CONSTRAINT STORY: combatant_status_definition__functional_protection_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_combatant_status_definition__functional_protection_reading, []).

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
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
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
 *   constraint_id: combatant_status_definition__functional_protection_reading
 *   human_readable: Common Article 3 Universal Baseline Protections (Functional Reading)
 *   domain: international_humanitarian_law
 *
 * SUMMARY:
 *   The functional protection reading of combatant status removes status
 *   determination as a precondition for minimum protections. All detained
 *   persons receive Common Article 3 guarantees of humane treatment, medical
 *   care, fair trial rights, and protection from torture — independent of
 *   whether they are recognized combatants, unlawful combatants, or
 *   civilians. The constraint operates as a coordination mechanism: it solves
 *   the problem of how to guarantee humane treatment across a fragmented
 *   world where status is contested. The extraction is minimal (ε=0.15)
 *   because the constraint imposes real obligations but does not concentrate
 *   benefits or burdens asymmetrically — state militaries and non-state
 *   groups both pay the cost of implementation and benefit from reciprocal
 *   protection of their personnel. Theater is low (0.12) because the
 *   constraint's function is operational (detention conditions, trial
 *   procedures) rather than symbolic.
 *
 * KEY AGENTS:
 *   - All detained persons: beneficiaries of the floor protections; trapped with no exit options
 *   - State militaries: agenda-setters who define detention procedures; payers of implementation costs; beneficiaries of reciprocal protection
 *   - Non-state armed groups: payers of the same constraints; beneficiaries of reciprocal protection if captured
 *   - ICRC and humanitarian monitors: institutional enforcers via monitoring and advocacy
 *   - International courts: adjudicate violations and clarify the constraint's scope
 *   - State-centric authorities: excluded seats that contest the functional reading in favor of status-dependent assignment
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(combatant_status_definition__functional_protection_reading, 0.15).
domain_priors:suppression_score(combatant_status_definition__functional_protection_reading, 0.08).
domain_priors:theater_ratio(combatant_status_definition__functional_protection_reading, 0.12).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(combatant_status_definition__functional_protection_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(combatant_status_definition__functional_protection_reading, suppression_requirement, 0.08).
narrative_ontology:constraint_metric(combatant_status_definition__functional_protection_reading, theater_ratio, 0.12).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(combatant_status_definition__functional_protection_reading, accessibility_collapse, 0.78).
narrative_ontology:constraint_metric(combatant_status_definition__functional_protection_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(combatant_status_definition__functional_protection_reading, rope).
narrative_ontology:human_readable(combatant_status_definition__functional_protection_reading, "Common Article 3 Universal Baseline Protections (Functional Reading)").
narrative_ontology:topic_domain(combatant_status_definition__functional_protection_reading, "international_humanitarian_law").

domain_priors:requires_active_enforcement(combatant_status_definition__functional_protection_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(combatant_status_definition__functional_protection_reading, 'e2284d81-74d6-4db3-9a80-23a24bb50287').
narrative_ontology:cs_kernel_codification('e2284d81-74d6-4db3-9a80-23a24bb50287', fixed_text).
narrative_ontology:cs_authority_grounding('e2284d81-74d6-4db3-9a80-23a24bb50287', lineage).
narrative_ontology:cs_interpretation_layer_present('e2284d81-74d6-4db3-9a80-23a24bb50287').
narrative_ontology:cs_reading_relation('e2284d81-74d6-4db3-9a80-23a24bb50287', combatant_status_definition__state_centric_reading, coexists_with).
narrative_ontology:cs_reading_relation('e2284d81-74d6-4db3-9a80-23a24bb50287', combatant_status_definition__national_liberation_reading, coexists_with).
narrative_ontology:cs_axiom('e2284d81-74d6-4db3-9a80-23a24bb50287', foundational, detention_protection_independent_of_status).
narrative_ontology:cs_axiom_status(detention_protection_independent_of_status, holdable).
narrative_ontology:cs_axiom_grounding('e2284d81-74d6-4db3-9a80-23a24bb50287', detention_protection_independent_of_status, deontological).
narrative_ontology:cs_axiom('e2284d81-74d6-4db3-9a80-23a24bb50287', foundational, baseline_protections_precede_status_determination).
narrative_ontology:cs_axiom_status(baseline_protections_precede_status_determination, holdable).
narrative_ontology:cs_axiom_grounding('e2284d81-74d6-4db3-9a80-23a24bb50287', baseline_protections_precede_status_determination, deontological).
narrative_ontology:cs_reference_frame('e2284d81-74d6-4db3-9a80-23a24bb50287', universal_detainee_protection_baseline).
narrative_ontology:cs_drift_state('e2284d81-74d6-4db3-9a80-23a24bb50287', contemporary_counterterrorism_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('e2284d81-74d6-4db3-9a80-23a24bb50287', '').
narrative_ontology:cs_kernel_id(combatant_status_definition__functional_protection_reading, combatant_status_definition).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(combatant_status_definition__functional_protection_reading, all_detained_persons).
narrative_ontology:constraint_beneficiary(combatant_status_definition__functional_protection_reading, international_humanitarian_law_regime).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(combatant_status_definition__functional_protection_reading, non_state_armed_groups).
narrative_ontology:constraint_victim(combatant_status_definition__functional_protection_reading, state_militaries).
narrative_ontology:constraint_victim(combatant_status_definition__functional_protection_reading, non_state_armed_groups).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Receive minimum guarantees of humane treatment, medical care, and fair trial procedures on the basis of detention status alone, independent of whether they are recognized as lawful combatants, unlawful combatants, or civilians. The constraint removes status determination as a precondition; protections attach to the fact of detention, not to combatant classification.
narrative_ontology:constraint_stakeholder(combatant_status_definition__functional_protection_reading, all_detained_persons, beneficiary,
    powerless, immediate, trapped, universal).

% Are obligated to provide Common Article 3 protections to all persons in their custody, including suspected non-combatants and irregular fighters. They bear the operational cost of implementing the protections. They also set detention and interrogation procedures within the constraint, negotiating the boundary between security imperatives and humane treatment requirements.
narrative_ontology:constraint_stakeholder(combatant_status_definition__functional_protection_reading, state_militaries, payer,
    powerful, generational, constrained, universal).
narrative_ontology:stakeholder_secondary_role(combatant_status_definition__functional_protection_reading, state_militaries, agenda_setter).

% Are bound to apply the same protections to their detainees as state actors, regardless of their own status claim or lack of formal state recognition. They benefit from the principle in reverse: if captured, their members receive baseline protections independent of combatant status recognition. They pay through operational constraints on detention and interrogation.
narrative_ontology:constraint_stakeholder(combatant_status_definition__functional_protection_reading, non_state_armed_groups, payer,
    moderate, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(combatant_status_definition__functional_protection_reading, non_state_armed_groups, beneficiary).

% The institutional system of treaty law, ICRC doctrine, and customary law is vindicated by the functional reading: the baseline protections exist independent of status determination, which keeps the regime coherent across contested status claims and reduces incentives to contest status as a way to deny protections.
narrative_ontology:constraint_stakeholder(combatant_status_definition__functional_protection_reading, international_humanitarian_law_regime, beneficiary,
    institutional, generational, analytical, universal).
narrative_ontology:stakeholder_non_agent(combatant_status_definition__functional_protection_reading, international_humanitarian_law_regime).

% Monitor and enforce compliance with the protections through detention visits, fact-finding missions, and advocacy. They operationalize the functional reading by treating baseline protections as non-negotiable and status determination as a secondary issue; their monitoring legitimacy rests on the principle that all detainees receive minimum treatment.
narrative_ontology:constraint_stakeholder(combatant_status_definition__functional_protection_reading, icrc_and_humanitarian_monitors, agenda_setter,
    organized, generational, mobile, universal).

% Are not parties to this reading's framing and contest the functional baseline. They argue that status determination is a necessary precondition for protection level assignment; excluding combatant status claims undermines their preferred framework that ties protections to formal recognition. Their exclusion from this reading is structural: the reading's entire point is to supersede status-dependent protection assignment.
narrative_ontology:constraint_stakeholder(combatant_status_definition__functional_protection_reading, state_centric_authorities, excluded,
    powerful, generational, constrained, universal).

% Navigate the tension between the functional reading (which grants protections to all detainees uniformly) and their own security interests in controlling combatant status claims. They observe how the constraint operates in practice and how other actors interpret it, gauging whether the functional floor is enforceable or whether status determination inevitably resurfaces as a workaround.
narrative_ontology:constraint_stakeholder(combatant_status_definition__functional_protection_reading, non_aligned_states, observer,
    organized, generational, constrained, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(combatant_status_definition__functional_protection_reading, diffuse).
narrative_ontology:fixing_cost_class(combatant_status_definition__functional_protection_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a universal baseline of minimum protections for all detained persons, solving the coordination problem of how to guarantee humane treatment across a fragmented world of state and non-state actors with contested status claims. The functional reading removes status as a bottleneck: protections do not depend on agreement about combatant classification.
% TRANSFER_FUNCTION: Transfers the burden of proving that a person is NOT entitled to baseline protections from the capturing actor to the international legal system. Under the state-centric reading, capturing actors bear the burden of status determination and can withhold protections pending that determination. Under the functional reading, protections flow immediately unless the capturing actor can show a specific exemption (e.g., someone is not detained, or is detained only for temporary identification). The constraint redistributes this burden.
% ABSENT_VOICES: Non-state armed groups that reject international humanitarian law frameworks altogether are excluded (not present at the table where the reading is negotiated). States that interpret IHL in maximally restrictive ways on status are not absent but are actively excluded from setting the terms of this reading. Detainees themselves have no seat in setting the rules, though they are the primary beneficiaries.
% DISAPPEARANCE_RATIONALE: If this constraint disappeared, detention practices would immediately fragment by status claim: actors would withhold protections pending status determination, creating indefinite ambiguous detention. Humanitarian access would become conditional on status agreement. Interrogation practices would harden because the functional floor would no longer constrain them. The international law regime would lose one of its few truly universal anchors and would reorganize around competing status frameworks.
% FOUNDING_PROBLEM: Post-World War II concern that belligerents would use combatant status determination as a mechanism to deny protections: treating irregular fighters, partisans, and suspected non-combatants as having no rights unless they could prove lawful combatant status. Common Article 3 was drafted as a floor that applies in all circumstances, independently of status settlement.
% FOUNDING_PROBLEM_CORROBORATION: ICRC analysis, case law from international courts, and state military manuals confirm that the founding problem persists: actors continuously use status ambiguity to justify enhanced interrogation, solitary confinement, and trial delay for detainees whose status is contested. Humanitarian organizations document the practice globally. States do not dispute that the problem existed; they dispute whether the functional reading is the right solution (which routes to the sibling readings and the kernel contest).
narrative_ontology:disappearance_verdict(combatant_status_definition__functional_protection_reading, world_rearranges).
narrative_ontology:founding_problem_status(combatant_status_definition__functional_protection_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(combatant_status_definition__functional_protection_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(combatant_status_definition__functional_protection_reading, 'none', 1).
narrative_ontology:epsilon_provenance(combatant_status_definition__functional_protection_reading, 0.15, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(combatant_status_definition__functional_protection_reading_tests).
:- end_tests(combatant_status_definition__functional_protection_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.15) because the constraint's core function is to establish a coordination floor: minimum protections that all actors benefit from in reciprocity. While state militaries bear implementation costs, they also benefit when their own personnel are captured and protected under the same floor. Non-state groups face constrained exit (they cannot simply refuse IHL compliance without international condemnation), but they also benefit from the principle applied in reverse. The constraint creates a genuine mixed-incentive situation: there is a cost to compliance, but the benefits of reciprocal protection are substantial. Suppression is minimal (0.08) because the constraint's persistence relies on international legal legitimacy and monitoring (ICRC, courts) rather than coercion. Theater is low (0.12) because enforcement is primarily through shame and remedy rather than performance; the constraint's function is achieved through detention inspection and litigation, not ceremonial compliance. Accessibility collapse is high (0.78) because once the functional reading is in place, alternatives (status-dependent protection) collapse as viable frameworks for justifying rights denial — the functional floor becomes the fallback everyone expects. Resistance is moderate-high (0.62) because the constraint meets real resistance from actors who prefer status-based protection assignment and see the functional floor as constraining their security discretion.
 *
 * PERSPECTIVAL GAP:
 *   From the state-centric seat (state militaries who prefer status-based assignment), the constraint is extractive because it removes their discretion to withhold protections pending status determination. They experience high d (near target). From the functional reading's own seat, the constraint is coordination because it solves a real problem: guaranteeing humane treatment across status disagreement. The engine computes this divergence correctly from the structural data — the beneficiary/victim split captures it. State militaries are both beneficiary (reciprocal protection) and payer (implementation cost), which the secondary_role field captures.
 *
 * DIRECTIONALITY LOGIC:
 *   All detained persons sit at full target end (d=1.0) initially — they are the beneficiaries, receiving protections that attach to their status as detained, not contingent on combatant recognition. However, the constraint imposes no extraction from them; their directionality derives from the fact that they are subject to detention rules they cannot exit. State militaries sit near symmetric (d=0.5): they bear the cost of implementing the protections but gain reciprocal protection for their personnel and benefit from the regime's legitimacy. Non-state groups sit near the target end (d=0.8) because they are constrained by the rule without full power to enforce it, but they also benefit from the principle's universal application. The ICRC and humanitarian monitors sit near beneficiary (d=0.1) because the constraint's operation vindicates their legitimacy and mission. The measure of extraction is how much the constraint privileges any one actor; in the functional reading, privilege is distributed, so effective extraction is low.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint shows no mandatrophy — the founding problem is live, and the functional mechanism persists as the solution. The status-quo alternative (status-dependent protection) would emerge if the constraint were removed; it is actively resisted, not atrophied. Theater ratio is low because the operational burden is real; there is no evidence that compliance is performative rather than functional.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    status_determination_gap,
    'Is it possible to remove status determination as a precondition for protections while preserving meaningful distinctions between lawful combatants and others in the same legal framework?',
    'Jurisprudential analysis: do courts and treaty bodies enforce the functional floor while also recognizing status-dependent consequences (e.g., combatant immunity, POW privilege)? Historical case examination across International Criminal Court, International Court of Justice, and national courts.',
    'If status and protections can coexist independently, the functional reading is coherent as written. If courts consistently find that status determines protection level, then the functional reading is aspirational but encounters structural resistance at the level of legal reasoning itself.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(status_determination_gap, empirical, 'Whether status determination and universal protections are logically separable in practice').

omega_variable(
    non_state_compliance,
    'Do non-state armed groups implement Common Article 3 protections at materially different rates than state militaries? Is the difference attributable to the reading''s absence of status-based incentives or to other factors (capacity, training, institutional legitimacy)?',
    'Comparative empirical studies of detention practices by state vs. non-state actors; ICRC documentation of violations patterns; field investigation controlling for conflict intensity and actor capacity.',
    'If non-state groups systematically fail to implement the protections at comparable rates, it suggests the functional reading faces compliance challenges rooted in capacity or incentive structure that the reading itself does not address. If rates are comparable, the reading''s coordination function is empirically validated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(non_state_compliance, empirical, 'Empirical compliance gap between state and non-state actors').

omega_variable(
    kernel_reading_stability,
    'Can the functional reading coexist with the state-centric reading in the same legal system, or does one inevitably dominate through interpretation?',
    'Jurisprudential history: trace how courts have treated the relationship between Common Article 3 (universal floor) and Article 4 (status-based POW distinction) over time. Do they treat them as orthogonal (both apply) or hierarchical (status determines protection level)?',
    'If they are treated as orthogonal, both readings can be live simultaneously. If hierarchical, one reading will absorb the other through legal interpretation, and the functional reading''s independence is undermined. This determines whether the kernel contest is truly open or whether institutional practice resolves it.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_stability, conceptual, 'Whether the functional and state-centric readings are structurally stable as coexisting legal positions').

omega_variable(
    extraction_vs_reciprocity_boundary,
    'At what point does the cost of implementing the protections (military operational burden, detention facility overhead) exceed the benefit of reciprocal protection (probability of one''s own personnel being captured and protected), converting the constraint from genuine coordination to asymmetric extraction?',
    'Cost-benefit analysis per military actor: estimate the operational cost of Common Article 3 compliance and the probabilistic benefit of reciprocal protection given deployment profile, capture risk, and conflict geography. Compare across state militaries with different operational contexts.',
    'If reciprocal benefit exceeds cost for all major state militaries, the constraint is genuine rope. If cost exceeds benefit for some actors (low-deployment states, low-capture-risk forces), those actors face extraction pressure and the constraint fragments into different types per seat. This would suggest the engine should compute different types for different actors rather than a single story-level type.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(extraction_vs_reciprocity_boundary, empirical, 'Whether the constraint''s cost-benefit structure remains coordination or shifts to extraction under certain deployment profiles').

omega_variable(
    committer_frame_ambiguity,
    'Is the functional_protection_reading a genuinely distinct reading of the combatant_status_definition kernel, or is it a reframing of the entire kernel that abandons status definition as a central concern?',
    'Kernel conceptualization: does the functional reading operate within the framework of ''how to define combatant status'' or does it sidestep the question entirely by saying ''status is secondary to protection baseline''? If the latter, it is a reading of a different kernel (e.g., detention_protection_standards), not a sibling reading of combatant_status_definition.',
    'If the functional reading is truly a sibling (coexists with status-centric and national-liberation readings within one kernel), the committer frame is correct and the three constraint stories form a proper kernel family. If the functional reading has abandoned the kernel''s core question, it should be reclassified as a reading of a different kernel, and the family structure requires revision. This would affect how the three stories'' network relationships are structured.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(committer_frame_ambiguity, conceptual, 'Whether the functional reading is a reading of the combatant_status_definition kernel or a refusal of that kernel''s framing').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(combatant_status_definition__functional_protection_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comb_tr_t0, combatant_status_definition__functional_protection_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(comb_tr_t5, combatant_status_definition__functional_protection_reading, theater_ratio, 5, 0.11).
narrative_ontology:measurement(comb_tr_t10, combatant_status_definition__functional_protection_reading, theater_ratio, 10, 0.12).
narrative_ontology:measurement(comb_tr_t15, combatant_status_definition__functional_protection_reading, theater_ratio, 15, 0.12).
narrative_ontology:measurement(comb_tr_t20, combatant_status_definition__functional_protection_reading, theater_ratio, 20, 0.13).
narrative_ontology:measurement(comb_tr_t25, combatant_status_definition__functional_protection_reading, theater_ratio, 25, 0.13).
narrative_ontology:measurement(comb_tr_t30, combatant_status_definition__functional_protection_reading, theater_ratio, 30, 0.12).
narrative_ontology:measurement(comb_tr_t40, combatant_status_definition__functional_protection_reading, theater_ratio, 40, 0.12).

% Extraction over time
narrative_ontology:measurement(comb_be_t0, combatant_status_definition__functional_protection_reading, base_extractiveness, 0, 0.12).
narrative_ontology:measurement(comb_be_t5, combatant_status_definition__functional_protection_reading, base_extractiveness, 5, 0.13).
narrative_ontology:measurement(comb_be_t10, combatant_status_definition__functional_protection_reading, base_extractiveness, 10, 0.14).
narrative_ontology:measurement(comb_be_t15, combatant_status_definition__functional_protection_reading, base_extractiveness, 15, 0.15).
narrative_ontology:measurement(comb_be_t20, combatant_status_definition__functional_protection_reading, base_extractiveness, 20, 0.16).
narrative_ontology:measurement(comb_be_t25, combatant_status_definition__functional_protection_reading, base_extractiveness, 25, 0.16).
narrative_ontology:measurement(comb_be_t30, combatant_status_definition__functional_protection_reading, base_extractiveness, 30, 0.15).
narrative_ontology:measurement(comb_be_t40, combatant_status_definition__functional_protection_reading, base_extractiveness, 40, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(comb_su_t0, combatant_status_definition__functional_protection_reading, suppression_requirement, 0, 0.06).
narrative_ontology:measurement(comb_su_t5, combatant_status_definition__functional_protection_reading, suppression_requirement, 5, 0.07).
narrative_ontology:measurement(comb_su_t10, combatant_status_definition__functional_protection_reading, suppression_requirement, 10, 0.08).
narrative_ontology:measurement(comb_su_t15, combatant_status_definition__functional_protection_reading, suppression_requirement, 15, 0.08).
narrative_ontology:measurement(comb_su_t20, combatant_status_definition__functional_protection_reading, suppression_requirement, 20, 0.09).
narrative_ontology:measurement(comb_su_t25, combatant_status_definition__functional_protection_reading, suppression_requirement, 25, 0.09).
narrative_ontology:measurement(comb_su_t30, combatant_status_definition__functional_protection_reading, suppression_requirement, 30, 0.08).
narrative_ontology:measurement(comb_su_t40, combatant_status_definition__functional_protection_reading, suppression_requirement, 40, 0.08).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(combatant_status_definition__functional_protection_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(combatant_status_definition__functional_protection_reading, 0.12).
narrative_ontology:affects_constraint(combatant_status_definition__functional_protection_reading, combatant_status_definition__state_centric_reading).
narrative_ontology:affects_constraint(combatant_status_definition__functional_protection_reading, combatant_status_definition__national_liberation_reading).

% DUAL FORMULATION NOTE:
% The combatant_status_definition kernel has three structurally distinct readings, each with its own constraint story. This story (functional_protection_reading) removes status determination as a precondition for minimum protections. The state_centric_reading ties protections to Article 4 formal combatant status. The national_liberation_reading extends combatant status to organized non-state groups. These three readings coexist in contemporary IHL practice and produce different directionality patterns and type computations per seat. Network edges link all three; they are not a constraint family in the decomposition sense (one kernel split into independent constraints), but rather a kernel contest where each story models a live reading simultaneously held by different parties.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(combatant_status_definition__functional_protection_reading, institutional, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
