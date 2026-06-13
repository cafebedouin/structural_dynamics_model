% ============================================================================
% CONSTRAINT STORY: end_of_life_authority__slippery_slope_mechanism
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_end_of_life_authority__slippery_slope_mechanism, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: end_of_life_authority__slippery_slope_mechanism
 *   human_readable: End-of-Life Authority: Slippery Slope Mechanism Reading
 *   domain: medical_ethics/bioethics/policy
 *
 * SUMMARY:
 *   Jurisdictions that legalize assistance in dying on autonomy grounds
 *   initially restrict eligibility to competent adults facing terminal
 *   illness with unbearable suffering. Empirically, eligibility criteria
 *   drift: within a decade, incompetent patients enter through
 *   surrogate-decision mechanisms; within two decades, non-terminal chronic
 *   suffering becomes accepted grounds; eventually, psychological suffering
 *   without physical illness qualifies. This constraint models the
 *   slippery-slope mechanism—not as a logical entailment (competent choice
 *   does not logically imply incompetent choice) but as an empirical
 *   institutional pattern where the autonomy rationale becomes stretched to
 *   serve institutional efficiency and normative expansions that diverge from
 *   the original framework's epistemic warrant. Incompetent patients and
 *   disability communities become the victim set as the original coordination
 *   function (respecting autonomous choice of competent, dying patients)
 *   drifts toward institutional-gatekeeping of incompetent and non-dying
 *   persons' deaths.
 *
 * KEY AGENTS:
 *   - competent_terminal_patients: Original beneficiaries; retain formal authority within the scope but that scope expands to populations excluded from autonomy-grounding
 *   - incompetent_patients: Enter victim set as surrogate-decision mechanisms stretch autonomy rationale beyond its epistemic foundation; they cannot consent but death authority is exercised in their name
 *   - chronically_suffering_non_terminal_patients: Drawn into scope via normative extension of 'unbearable suffering'; eligibility drifts from terminal-specific to subjective-suffering standard
 *   - disability_communities: Structural targets of expanded definition; pathologization of disability as optional suffering; excluded from shaping the suffering-judgment standard
 *   - medical_institutions: Agenda-setters; benefit from operational simplification and scope expansion; control interpretive authority over eligibility criteria
 *   - clinical_gatekeepers: Dual-positioned; gain interpretive authority but bear liability; suppression through procedural burden and legal exposure
 *   - autonomy_rights_advocates: Beneficiaries; support and accelerate expansions beyond original terminal-only scope
 *   - sanctity_advocates: Structurally excluded; their core premises not heard as legitimate in the autonomy framework
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(end_of_life_authority__slippery_slope_mechanism, 0.68).
domain_priors:suppression_score(end_of_life_authority__slippery_slope_mechanism, 0.72).
domain_priors:theater_ratio(end_of_life_authority__slippery_slope_mechanism, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(end_of_life_authority__slippery_slope_mechanism, extractiveness, 0.68).
narrative_ontology:constraint_metric(end_of_life_authority__slippery_slope_mechanism, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(end_of_life_authority__slippery_slope_mechanism, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(end_of_life_authority__slippery_slope_mechanism, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(end_of_life_authority__slippery_slope_mechanism, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(end_of_life_authority__slippery_slope_mechanism, tangled_rope).
narrative_ontology:human_readable(end_of_life_authority__slippery_slope_mechanism, "End-of-Life Authority: Slippery Slope Mechanism Reading").
narrative_ontology:topic_domain(end_of_life_authority__slippery_slope_mechanism, "medical_ethics/bioethics/policy").

domain_priors:requires_active_enforcement(end_of_life_authority__slippery_slope_mechanism).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(end_of_life_authority__slippery_slope_mechanism, 'eb79e066-b7d6-4c90-adf0-b170687cb683').
narrative_ontology:cs_kernel_codification('eb79e066-b7d6-4c90-adf0-b170687cb683', fixed_text).
narrative_ontology:cs_authority_grounding('eb79e066-b7d6-4c90-adf0-b170687cb683', extraction).
narrative_ontology:cs_interpretation_layer_present('eb79e066-b7d6-4c90-adf0-b170687cb683').
narrative_ontology:cs_reading_relation('eb79e066-b7d6-4c90-adf0-b170687cb683', end_of_life_authority__autonomy_reading, influences).
narrative_ontology:cs_reading_relation('eb79e066-b7d6-4c90-adf0-b170687cb683', end_of_life_authority__sanctity_reading, influences).
narrative_ontology:cs_axiom('eb79e066-b7d6-4c90-adf0-b170687cb683', foundational, initial_scope_drift_empirically_documented).
narrative_ontology:cs_axiom_status(initial_scope_drift_empirically_documented, holdable).
narrative_ontology:cs_axiom_grounding('eb79e066-b7d6-4c90-adf0-b170687cb683', initial_scope_drift_empirically_documented, empirically_contingent).
narrative_ontology:cs_axiom('eb79e066-b7d6-4c90-adf0-b170687cb683', secondary, autonomy_framework_extends_beyond_epistemic_warrant).
narrative_ontology:cs_axiom_status(autonomy_framework_extends_beyond_epistemic_warrant, holdable).
narrative_ontology:cs_axiom_grounding('eb79e066-b7d6-4c90-adf0-b170687cb683', autonomy_framework_extends_beyond_epistemic_warrant, empirically_contingent).
narrative_ontology:cs_reference_frame('eb79e066-b7d6-4c90-adf0-b170687cb683', terminal_competent_only_eligibility).
narrative_ontology:cs_drift_state('eb79e066-b7d6-4c90-adf0-b170687cb683', contemporary_post_two_decades_empirical_expansion, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('eb79e066-b7d6-4c90-adf0-b170687cb683', '').
narrative_ontology:cs_kernel_id(end_of_life_authority__slippery_slope_mechanism, end_of_life_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(end_of_life_authority__slippery_slope_mechanism, medical_institutions).
narrative_ontology:constraint_beneficiary(end_of_life_authority__slippery_slope_mechanism, disability_minimization_advocates).
narrative_ontology:constraint_victim(end_of_life_authority__slippery_slope_mechanism, incompetent_patients).
narrative_ontology:constraint_victim(end_of_life_authority__slippery_slope_mechanism, chronically_suffering_non_terminal_patients).
narrative_ontology:constraint_victim(end_of_life_authority__slippery_slope_mechanism, disability_communities).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(end_of_life_authority__slippery_slope_mechanism, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(end_of_life_authority__slippery_slope_mechanism, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(end_of_life_authority__slippery_slope_mechanism_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(end_of_life_authority__slippery_slope_mechanism, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(end_of_life_authority__slippery_slope_mechanism_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises monotonically from 0.35 (narrow terminal-specific application) to 0.68 (broad suffering-based eligibility spanning competent and incompetent populations). This rise reflects the empirical observation that the same autonomy rationale that legitimizes competent terminal patients' choices becomes stretched to justify institutional decisions about incompetent and non-terminal patients—populations for whom the autonomy foundation is absent or attenuated. Theater ratio rises in parallel (0.15 to 0.41) because an increasing share of the framework's operation is not transparent autonomy-grounding but institutional reinterpretation of suffering, best-interest judgment, and liability management. Suppression requirement rises (0.45 to 0.72) because as eligibility expands into populations less able to defend their interests (incompetent patients) and populations ideologically opposed (disability communities), active suppression of dissent and gatekeeping becomes essential to maintain the expansion. The framework is tangled: it genuinely coordinates (at t0) around terminal competent patients' autonomous choice; it simultaneously extracts (incompetent patients bear costs they cannot refuse) and requires enforcement (suppressing disabled persons' objections, managing clinical liability). Measurements show a phase transition around t=20-25 where the constraint's character shifts from coordination-dominant to extraction-dominant. The projected plateau and slight decline after t=35 reflects anticipated legal and political resistance that caps further drift without dismantling the expanded scope already achieved.
 *
 * PERSPECTIVAL GAP:
 *   From the institutional and autonomy-advocate seat, the expansion is justified as correct normative drift: autonomy should apply wherever suffering exists and an agent can express a preference. From the incompetent-patient and disability-community seat, the same expansion is experienced as extractive harm—their non-consensual inclusion in a death-permission framework that uses autonomy language without their capacity to exercise it. The engine computes this divergence from the structural data: the institutional agenda-setter holds interpretive authority (powerful, arbitrage exit); the victim populations are powerless and trapped. The claim is tangled_rope (genuine coordination + asymmetric extraction); the metrics show how extraction accumulates as scope expands.
 *
 * DIRECTIONALITY LOGIC:
 *   Medical institutions are the structural beneficiary (collect interpretive authority, streamline operations through unified standards, avoid legal liability for rigid terminal-only rules). Incompetent and chronically-suffering patients are the targets (no choice in their inclusion, subjected to institutional or surrogate death-authorization in their names, no exit). Disability communities are targets (excluded from authority-shaping, pathologized by the suffering standard, subject to pressure that their existence is optional). Autonomy-rights advocates are beneficiaries (their preferred expansion is institutionalized, normalizing and scaling their reading). Clinical gatekeepers are dual-positioned: they gain institutional authority but incur liability and procedural burden—they are partly payers in the extraction-expansion. Competent terminal patients retain formal beneficiary status within the original scope but experience a shift in that scope's meaning as the rationale is stretched: their autonomy is used to justify other persons' deaths, which may or may not align with their understanding of their own right.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (competent terminal patients lack legal death control) is declared CONTESTED because the empirical drift shows the constraint's actual operation diverges from its founding rationale. Autonomy-advocates argue the problem is live and the solution is correct. Disability and sanctity communities argue the founding problem is framed falsely (the real problem is suffering, not autonomy; the solution creates worse problems). The measured extraction (0.68 endpoint) combined with theater ratio (0.41) indicates mandatrophy: the framework's operation no longer fits its founding justification. The coordination function (respecting competent autonomous choice) is genuine but dwarfed by the extraction function (institutional death-gatekeeping for incompetent and disabled populations). The constraint persists because institutional beneficiaries (hospitals, ethics committees, autonomy-advocates) have interests in the expanded scope, not because the founding problem requires it. The measured theater ratio rising to 0.41 indicates a phase where institutional legitimacy discourse (autonomy language, informed-consent theater) increasingly obscures the actual operation (institutional decision-making, surrogate judgment, elimination of disfavored populations). No single party is sufficiently hurt by the constraint to fix it (disability communities are excluded, incompetent patients cannot organize, clinicians are compensated through authority gains), and no party is sufficiently benefiting to maintain it against resistance (expansion is self-reinforcing, not deliberately planned). The constraint exhibits piton characteristics (persistence through inertia + theater) but is classified tangled_rope because the extraction is still active and asymmetric, not yet fully diffused into performance.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    suppression_structural_vs_internalized,
    'Is the measured suppression of disabled persons and clinical objectors primarily structural (legal barriers, institutional rules, liability exposure) or internalized (normalization of the autonomy narrative, professional identity fusion, disability-community acceptance of the suffering standard)?',
    'Post-legal-reversal outcome studies: if legal barriers to death-decisions are removed and suppression persists in clinician behavior and disabled-community internalization, suppression is substantially internalized. If suppression drops immediately upon legal reversal, it was primarily structural.',
    'High structural suppression suggests the constraint could be dismantled by legal/institutional change. High internalized suppression suggests the constraint would persist even after formal removal because affected populations have internalized its rationale.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_structural_vs_internalized, empirical, 'Whether suppression is enforced externally or internalized by affected populations.').

omega_variable(
    autonomy_framework_necessity,
    'Is the empirical expansion beyond terminal-competent cases logically entailed by the autonomy framework, or is it a contingent institutional choice that could have been prevented by narrower legal codification?',
    'Comparative legal analysis: do jurisdictions with explicit terminal-only statutory language show less drift than those with broad ''unbearable suffering'' language? Interviews with institutional actors about deliberate vs. accidental expansion.',
    'If expansion is logically entailed, the slippery slope is inevitable and the autonomy reading forecloses itself (it cannot be held without accepting its expansion). If expansion is contingent, the autonomy reading remains holdable in a narrow form, and the slope is a governance failure, not a logical consequence.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(autonomy_framework_necessity, conceptual, 'Whether expansion is built into autonomy-grounding or results from institutional drift avoidable by design.').

omega_variable(
    kernel_reading_epistemic_status,
    'Is the slippery_slope_mechanism reading a description of institutional reality, a normative critique of the autonomy reading, or a veiled instantiation of the sanctity reading (framing autonomy as a vehicle for sanctity)?',
    'Content analysis of how the slope-mechanism framing is used in public discourse: does it support autonomy-restraint arguments, sanctity arguments, or remain analytically neutral? Do disability advocates who use the mechanism claim to reject autonomy or to protect autonomy from institutional distortion?',
    'If the slope mechanism is a disguised sanctity reading, it should be classified as such and the sibling reading_relations revised. If it is analytically neutral, the reading should coexist with autonomy and sanctity readings. If it critiques autonomy from an autonomy-internalist perspective, it influences the autonomy reading but does not foreclose it.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_epistemic_status, conceptual, 'Whether the slippery-slope reading is independent of the autonomy and sanctity positions or serves as a vehicle for one of them.').

omega_variable(
    incompetent_patient_victim_status,
    'Are incompetent patients truly victims of this constraint (subjected to death-authorization without capacity to resist) or are they beneficiaries of a compassionate framework that prevents prolonged suffering?',
    'Qualitative data from surrogate decision-makers and families: do they report experiencing the constraint as liberation (enabling compassionate death) or constraint (institutional pressure to authorize death)? Outcome data on whether incompetent patients'' actual preferences (where knowable) align with institutional death-decisions made on their behalf.',
    'If incompetent patients are beneficiaries, the beneficiary set should be expanded and the victims set contracted. If they are victims, the extraction reading is accurate. The measured extractiveness (0.68) assumes victim status; evidence for beneficiary status would suggest the constraint is less extractive and more coordinative than modeled.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(incompetent_patient_victim_status, empirical, 'Whether incompetent patients are extracted from or protected by the expanded framework.').

omega_variable(
    disability_pathologization_causal_source,
    'Does the slippery-slope mechanism cause disability communities'' harm (by expanding the suffering-elimination framework), or does it reveal pre-existing institutional bias toward disability elimination that would persist in different frameworks?',
    'Comparative historical analysis: in jurisdictions without autonomy-based death frameworks, is there evidence of disability-elimination pressure via other mechanisms (forced sterilization, institutional neglect, physician discretion)? If so, the slope mechanism is not the source of the harm but a new vehicle for existing harm.',
    'If the slope mechanism causes the harm, restricting it would reduce disability harm. If it reveals pre-existing harm, the constraint is a symptom; fixing it would require addressing deeper institutional bias. Either way, disability communities are victims; the causal question affects remedy strategy.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(disability_pathologization_causal_source, empirical, 'Whether the slippery slope is a source of disability harm or a vehicle for harm that would exist anyway.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(end_of_life_authority__slippery_slope_mechanism, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(end__tr_t0, end_of_life_authority__slippery_slope_mechanism, theater_ratio, 0, 0.15).
narrative_ontology:measurement(end__tr_t5, end_of_life_authority__slippery_slope_mechanism, theater_ratio, 5, 0.19).
narrative_ontology:measurement(end__tr_t10, end_of_life_authority__slippery_slope_mechanism, theater_ratio, 10, 0.23).
narrative_ontology:measurement(end__tr_t15, end_of_life_authority__slippery_slope_mechanism, theater_ratio, 15, 0.27).
narrative_ontology:measurement(end__tr_t20, end_of_life_authority__slippery_slope_mechanism, theater_ratio, 20, 0.32).
narrative_ontology:measurement(end__tr_t25, end_of_life_authority__slippery_slope_mechanism, theater_ratio, 25, 0.36).
narrative_ontology:measurement(end__tr_t30, end_of_life_authority__slippery_slope_mechanism, theater_ratio, 30, 0.41).
narrative_ontology:measurement(end__tr_t35, end_of_life_authority__slippery_slope_mechanism, theater_ratio, 35, 0.44).
narrative_ontology:measurement(end__tr_t40, end_of_life_authority__slippery_slope_mechanism, theater_ratio, 40, 0.43).

% Extraction over time
narrative_ontology:measurement(end__be_t0, end_of_life_authority__slippery_slope_mechanism, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(end__be_t5, end_of_life_authority__slippery_slope_mechanism, base_extractiveness, 5, 0.42).
narrative_ontology:measurement(end__be_t10, end_of_life_authority__slippery_slope_mechanism, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(end__be_t15, end_of_life_authority__slippery_slope_mechanism, base_extractiveness, 15, 0.54).
narrative_ontology:measurement(end__be_t20, end_of_life_authority__slippery_slope_mechanism, base_extractiveness, 20, 0.61).
narrative_ontology:measurement(end__be_t25, end_of_life_authority__slippery_slope_mechanism, base_extractiveness, 25, 0.65).
narrative_ontology:measurement(end__be_t30, end_of_life_authority__slippery_slope_mechanism, base_extractiveness, 30, 0.68).
narrative_ontology:measurement(end__be_t35, end_of_life_authority__slippery_slope_mechanism, base_extractiveness, 35, 0.7).
narrative_ontology:measurement(end__be_t40, end_of_life_authority__slippery_slope_mechanism, base_extractiveness, 40, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(end__su_t0, end_of_life_authority__slippery_slope_mechanism, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(end__su_t5, end_of_life_authority__slippery_slope_mechanism, suppression_requirement, 5, 0.51).
narrative_ontology:measurement(end__su_t10, end_of_life_authority__slippery_slope_mechanism, suppression_requirement, 10, 0.56).
narrative_ontology:measurement(end__su_t15, end_of_life_authority__slippery_slope_mechanism, suppression_requirement, 15, 0.62).
narrative_ontology:measurement(end__su_t20, end_of_life_authority__slippery_slope_mechanism, suppression_requirement, 20, 0.67).
narrative_ontology:measurement(end__su_t25, end_of_life_authority__slippery_slope_mechanism, suppression_requirement, 25, 0.7).
narrative_ontology:measurement(end__su_t30, end_of_life_authority__slippery_slope_mechanism, suppression_requirement, 30, 0.72).
narrative_ontology:measurement(end__su_t35, end_of_life_authority__slippery_slope_mechanism, suppression_requirement, 35, 0.73).
narrative_ontology:measurement(end__su_t40, end_of_life_authority__slippery_slope_mechanism, suppression_requirement, 40, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(end_of_life_authority__slippery_slope_mechanism, resource_allocation).
narrative_ontology:boltzmann_floor_override(end_of_life_authority__slippery_slope_mechanism, 0.18).
narrative_ontology:affects_constraint(end_of_life_authority__slippery_slope_mechanism, end_of_life_authority__autonomy_reading).
narrative_ontology:affects_constraint(end_of_life_authority__slippery_slope_mechanism, end_of_life_authority__sanctity_reading).

% DUAL FORMULATION NOTE:
% The end_of_life_authority kernel decomposes into three constraint readings: (1) autonomy_reading grounds authority in individual choice (Mountain or Rope, minimal extraction); (2) sanctity_reading grounds authority in intrinsic life value (Mountain, negligible extraction, entrenched); (3) slippery_slope_mechanism (this story) documents institutional drift where the autonomy reading expands to encompass incompetent and non-dying populations, becoming a vehicle for sanctity-like elimination concerns. The three readings are structurally distinct constraints with different epsilon values, beneficiary/victim structures, and classifications. Each story links to the other two via affects_constraints to show the kernel family structure. The autonomy reading influences (and may foreclose) the sanctity reading depending on how broadly autonomy-grounding is interpreted; the slippery-slope reading influences both by documenting how autonomy-grounding in practice converges toward sanctity outcomes without sanctity premises.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(end_of_life_authority__slippery_slope_mechanism, organized, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
