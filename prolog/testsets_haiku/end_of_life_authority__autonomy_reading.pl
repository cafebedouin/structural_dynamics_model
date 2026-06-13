% ============================================================================
% CONSTRAINT STORY: end_of_life_authority__autonomy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_end_of_life_authority__autonomy_reading, []).

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
 *   constraint_id: end_of_life_authority__autonomy_reading
 *   human_readable: End-of-Life Autonomy Right (Individual Control Reading)
 *   domain: medical_ethics/bioethics/governance
 *
 * SUMMARY:
 *   The autonomy reading of the end-of-life-authority kernel asserts that
 *   individual autonomy grounds the right to control the timing and
 *   circumstances of death when facing unbearable suffering. This reading
 *   conflicts with the sanctity reading (intrinsic value of life prohibits
 *   intentional ending) and is empirically threatened by the slippery-slope
 *   reading (autonomy frameworks empirically expand beyond competent terminal
 *   cases to incompetent and non-terminal populations). The autonomy reading
 *   instantiates a tangled rope: it provides genuine coordination (patient
 *   choice, transparent process, reduced covert unmedical dying, physician
 *   guidance on suffering management) but also extracts cost. Patients denied
 *   choice suffer prolonged dying. Physicians navigate legal liability and
 *   moral role conflict. Regulatory bodies gain gatekeeping authority while
 *   claiming to serve autonomy. Disability advocates argue the autonomy right
 *   conflates disability with unbearable suffering and may coerce disabled
 *   people. The constraint's persistence depends on active enforcement: legal
 *   prohibition of non-compliant practices, medical licensure tied to
 *   guideline adherence, and institutional oversight. The claimed type
 *   (tangled rope) and the metrics (moderate-high extraction, high
 *   suppression, moderate theater) track together because the coordination
 *   function is real (patient agency, suffering reduction) but asymmetric
 *   (physicians and regulators decide who is competent and what suffering
 *   justifies choice).
 *
 * KEY AGENTS:
 *   - patients_facing_unbearable_suffering: beneficiary of autonomy right, trapped without it
 *   - patients_denied_choice: victim of suppression, in jurisdictions where autonomy is not recognized
 *   - physicians_administering_end_of_life_care: dual role (beneficiary of coordination, payer of legal/moral liability)
 *   - regulatory_bodies_and_legal_authorities: agenda-setter, administering competence assessment and eligibility criteria
 *   - medical_professional_societies: agenda-setter, gatekeeping through guidelines and competency standards
 *   - religious_and_sanctity_advocates: excluded from veto but actively contesting through legislation
 *   - disability_advocates_and_critics: excluded from clinical decisions but arguing autonomy conflates disability with suffering
 *   - family_members_of_dying_patients: dual role (beneficiary of choice availability, payer of relational costs)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(end_of_life_authority__autonomy_reading, 0.62).
domain_priors:suppression_score(end_of_life_authority__autonomy_reading, 0.78).
domain_priors:theater_ratio(end_of_life_authority__autonomy_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(end_of_life_authority__autonomy_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(end_of_life_authority__autonomy_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(end_of_life_authority__autonomy_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(end_of_life_authority__autonomy_reading, accessibility_collapse, 0.71).
narrative_ontology:constraint_metric(end_of_life_authority__autonomy_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(end_of_life_authority__autonomy_reading, tangled_rope).
narrative_ontology:human_readable(end_of_life_authority__autonomy_reading, "End-of-Life Autonomy Right (Individual Control Reading)").
narrative_ontology:topic_domain(end_of_life_authority__autonomy_reading, "medical_ethics/bioethics/governance").

domain_priors:requires_active_enforcement(end_of_life_authority__autonomy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(end_of_life_authority__autonomy_reading, '91ef6cdf-7f9d-4409-80b5-edce7d9d3676').
narrative_ontology:cs_kernel_codification('91ef6cdf-7f9d-4409-80b5-edce7d9d3676', fixed_text).
narrative_ontology:cs_authority_grounding('91ef6cdf-7f9d-4409-80b5-edce7d9d3676', lineage).
narrative_ontology:cs_interpretation_layer_present('91ef6cdf-7f9d-4409-80b5-edce7d9d3676').
narrative_ontology:cs_reading_relation('91ef6cdf-7f9d-4409-80b5-edce7d9d3676', end_of_life_authority__sanctity_reading, forecloses).
narrative_ontology:cs_reading_relation('91ef6cdf-7f9d-4409-80b5-edce7d9d3676', end_of_life_authority__slippery_slope_mechanism, coexists_with).
narrative_ontology:cs_axiom('91ef6cdf-7f9d-4409-80b5-edce7d9d3676', foundational, individual_autonomy_overrides_life_preservation).
narrative_ontology:cs_axiom_status(individual_autonomy_overrides_life_preservation, holdable).
narrative_ontology:cs_axiom_grounding('91ef6cdf-7f9d-4409-80b5-edce7d9d3676', individual_autonomy_overrides_life_preservation, deontological).
narrative_ontology:cs_axiom('91ef6cdf-7f9d-4409-80b5-edce7d9d3676', secondary, competence_determines_eligibility).
narrative_ontology:cs_axiom_status(competence_determines_eligibility, holdable).
narrative_ontology:cs_axiom_grounding('91ef6cdf-7f9d-4409-80b5-edce7d9d3676', competence_determines_eligibility, instrumental).
narrative_ontology:cs_reference_frame('91ef6cdf-7f9d-4409-80b5-edce7d9d3676', autonomy_and_dignity_of_competent_terminal_patients).
narrative_ontology:cs_drift_state('91ef6cdf-7f9d-4409-80b5-edce7d9d3676', contemporary_expanded_criteria_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('91ef6cdf-7f9d-4409-80b5-edce7d9d3676', '').
narrative_ontology:cs_kernel_id(end_of_life_authority__autonomy_reading, end_of_life_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(end_of_life_authority__autonomy_reading, patients_facing_unbearable_suffering).
narrative_ontology:constraint_victim(end_of_life_authority__autonomy_reading, patients_denied_choice).
narrative_ontology:constraint_victim(end_of_life_authority__autonomy_reading, physicians_under_legal_liability).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(end_of_life_authority__autonomy_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(end_of_life_authority__autonomy_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(end_of_life_authority__autonomy_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(end_of_life_authority__autonomy_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(end_of_life_authority__autonomy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness climbs from 0.45 to 0.62 over the interval, reflecting the observed expansion of criteria beyond strict terminal diagnosis (captured in the slippery_slope_mechanism constraint). At t0, the constraint is relatively bounded (early implementation, narrow criteria); at t40, data shows expansion toward chronic suffering, psychiatric cases, and non-terminal requests, increasing the extraction cost to those whose suffering does not fit the expanded criteria — the suppression barrier rises to prevent even broader expansion. Suppression remains high (0.78–0.82) throughout because the autonomy reading requires active legal enforcement against sanctity-based prohibition and unmedical alternatives. Theater is low-to-moderate (0.12–0.29) because the functional component (patient choice, suffering assessment, informed consent) is real, but performative elements grow as the constraint expands: more elaborate safeguards, more documentation, more regulation of borderline cases. The measurement series shares a single time grid so every metric can be compared at each point.
 *
 * PERSPECTIVAL GAP:
 *   The payer seat (physicians, patients denied choice) and the agenda-setter seat (regulators, professional societies) should compute different types. From the payer view, the constraint operates as enforced suppression and extraction (snare-flavored). From the agenda-setter view, it is genuine coordination they steward and defend (rope-flavored). The engine's per-seat classification computation should reveal this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Patients facing unbearable suffering benefit most clearly from the autonomy right (d approaches 0.0, beneficiary end). Patients denied choice and facing suppression of the right sit at d approaching 1.0 (victims). Physicians absorb moderate extraction (legal liability, role conflict, moral distress) while also coordinating patients through difficult decisions — their d sits roughly 0.55–0.65 (neither full target nor full beneficiary; the role is dual). Regulatory bodies sit near 0.4 (they benefit from the coordination function and from gatekeeping authority, but also bear oversight burden). Disability advocates sit in a peculiar position: they have no direct agency in the constraint but experience it as coercive pressure — their d would be high (1.0, target) if they were included, but they are explicitly excluded from the autonomy framework, which makes them victims of the constraint's operation even as they have no formal seat. Family members split: they benefit from choice availability (low d) but pay relational costs (elevated d). The directionality_overrides array is not needed because the structural derivation from beneficiary/victim + exit options + power level captures the asymmetry.
 *
 * MANDATROPHY ANALYSIS:
 *   The autonomy reading's founding problem was real: patients facing unbearable suffering lacked transparent, safe pathways for choice. Empirical data from permitting jurisdictions shows the founding problem is substantially addressed — patient choice is now available, covert dying is reduced, and physician guidance is structured. However, the constraint's persistence depends on whether the founding problem remains 'live' or has become 'dead' — and here the committer structure becomes salient. The autonomy reading claims the problem is live (patients still face unbearable suffering) and therefore the constraint remains necessary. The slippery-slope reading argues the problem is now dead in the terminal-competent-patient case (solved through the constraint) but the constraint persists and expands to non-terminal and incompetent populations — this is mandatrophy: the constraint outlives its function and becomes a vehicle for broader extraction (or coercion, from the disability view). The sanctity reading denies the founding problem was legitimate from the start (suffering, even unbearable, does not override life's intrinsic value). These three readings correspond to three different mandatrophy verdicts on the same kernel. The autonomy reading predicts its own constraint is non-mandarophied (problem is live, constraint is necessary); the slippery-slope reading predicts the autonomy reading will undergo mandatrophy (function dies, extraction expands); the sanctity reading predicts the autonomy reading was always illegitimate (foundational problem was misdefined). This triadic structure should be captured in the omegas and cs_structure.reading_relations.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    autonomy_vs_capacity_asymmetry,
    'Does the autonomy reading''s reliance on patient competence and informed consent legitimately protect vulnerable populations, or does it exclude patients with cognitive impairment, communication barriers, or diminished decision-making capacity from the very choice it claims to honor?',
    'Empirical study of denied-access cases and criteria-setting over time; ethical analysis of competence thresholds and who sets them; follow-up data on whether capacity-exclusion becomes a de facto limitation of the autonomy right to privileged patients.',
    'If competence thresholds become overly restrictive or applied unequally, the autonomy reading may be extractive toward vulnerable populations — physicians and regulators gain gatekeeping authority while claiming to serve autonomy. If thresholds remain permissive with strong support structures, the coordination function is genuine.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(autonomy_vs_capacity_asymmetry, empirical, 'Whether autonomy criteria exclude or include the most vulnerable.').

omega_variable(
    suffering_definition_drift,
    'What counts as unbearable suffering eligible for autonomy-based choice? Does the definition remain stable (terminal diagnosis + intractable pain + patient request) or drift to include chronic suffering, psychiatric suffering, or loss of meaning without terminal diagnosis?',
    'Longitudinal data on criteria changes in permitting jurisdictions; analysis of case law and regulatory guidance expansion; study of actual vs. stated eligibility over time; comparison across jurisdictions.',
    'If suffering definition expands empirically while the constraint claims to remain limited, the slippery-slope reading gains evidence and the autonomy reading becomes a Trojan horse for broader extraction. If definition remains stable, the autonomy reading is a genuine bounded coordination. The slippery_slope_mechanism constraint should register this drift.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(suffering_definition_drift, empirical, 'The scope creep question: does unbearable suffering expand beyond original boundaries?').

omega_variable(
    disability_coercion_ambiguity,
    'When a disabled person declines the autonomy right and chooses to live with their condition, does the socially-available autonomy to die (even if rejected) change their felt agency — making continued life feel like a burden rather than a choice?',
    'Qualitative research with disabled populations; longitudinal study of disability-death attitudes before and after autonomy legalization; survey of social pressure and internalized ableism in jurisdictions with and without autonomy rights.',
    'If the autonomy right becomes socially normalized as the way to escape disability, disabled people may experience internalized suppression even while formally choosing life. This would make the constraint extractive toward disability while claiming to serve autonomy. If disabled people report no change in felt coercion, the constraint''s autonomy promise holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(disability_coercion_ambiguity, empirical, 'Whether the autonomy right creates or reduces coercive pressure on disabled populations.').

omega_variable(
    physician_role_fusion,
    'Can physicians simultaneously be healers (preserving and extending life) and facilitators of patient-chosen death without role conflict, or does the autonomy reading require a fusion of contradictory missions that physicians absorb as moral distress?',
    'Study of physician moral distress, burnout, and professional identity in jurisdictions with and without autonomy rights; interview data on how physicians reconcile healing and death-facilitation; tracking of physician exits from end-of-life care.',
    'If physicians experience deep role conflict, the extraction (moral cost) falls on them even though they are structural agenda-setters. If physicians integrate both roles coherently, the coordination function is genuine. High distress would suggest the autonomy reading is tangled rope with physician extraction as the hidden cost.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(physician_role_fusion, empirical, 'Whether the physician role can absorb both healing and death-facilitation without pathological distress.').

omega_variable(
    autonomy_sanctity_foreclosure,
    'Is the autonomy reading (individual choice controls timing of death) logically compatible with the sanctity reading (life has intrinsic value beyond individual preference) within the same legal and ethical framework, or does recognizing autonomy-based choice foreclose the sanctity claim at the level of binding policy?',
    'Analysis of jurisdictions that attempt to hold both readings (e.g., permitting autonomy while maintaining symbolic/rhetorical commitment to sanctity); examination of whether sanctity can remain binding at the individual level once autonomy is permitted; philosophical analysis of the logical structure.',
    'If the readings foreclose each other, the autonomy reading''s adoption structurally defeats sanctity-based objections — sanctity becomes a voice excluded from binding decisions (confirming the exclusion slot). If they coexist (sanctity informs conscientious objection, autonomy informs choice), both readings remain live. The relation type for the cs_structure should reflect this.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(autonomy_sanctity_foreclosure, conceptual, 'Whether autonomy and sanctity readings can coexist in the same framework.').

omega_variable(
    slippery_slope_inevitability,
    'Does the autonomy reading''s legitimacy mechanically require expansion to non-terminal suffering and incompetent populations (slippery slope), or can it remain bounded to competent terminal patients indefinitely?',
    'Long-term longitudinal data on criteria expansion in permitting jurisdictions; comparative analysis of jurisdictions that expanded vs. remained stable; causal analysis of what drives expansion (patient demand, physician advocacy, bureaucratic drift, or something else); modeling of the constraint''s terminal state.',
    'If expansion is inevitable, the slippery_slope_mechanism reading is empirically vindicated and the autonomy reading is a way-station. If expansion is optional and stable boundaries are maintained, the autonomy reading is a genuine stable constraint. This is the core divergence between the two readings'' predictions.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(slippery_slope_inevitability, empirical, 'Whether the autonomy reading tends toward expansion or remains bounded.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(end_of_life_authority__autonomy_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(end__tr_t0, end_of_life_authority__autonomy_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(end__tr_t5, end_of_life_authority__autonomy_reading, theater_ratio, 5, 0.15).
narrative_ontology:measurement(end__tr_t10, end_of_life_authority__autonomy_reading, theater_ratio, 10, 0.18).
narrative_ontology:measurement(end__tr_t15, end_of_life_authority__autonomy_reading, theater_ratio, 15, 0.22).
narrative_ontology:measurement(end__tr_t20, end_of_life_authority__autonomy_reading, theater_ratio, 20, 0.25).
narrative_ontology:measurement(end__tr_t25, end_of_life_authority__autonomy_reading, theater_ratio, 25, 0.27).
narrative_ontology:measurement(end__tr_t30, end_of_life_authority__autonomy_reading, theater_ratio, 30, 0.29).
narrative_ontology:measurement(end__tr_t40, end_of_life_authority__autonomy_reading, theater_ratio, 40, 0.28).

% Extraction over time
narrative_ontology:measurement(end__be_t0, end_of_life_authority__autonomy_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(end__be_t5, end_of_life_authority__autonomy_reading, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(end__be_t10, end_of_life_authority__autonomy_reading, base_extractiveness, 10, 0.52).
narrative_ontology:measurement(end__be_t15, end_of_life_authority__autonomy_reading, base_extractiveness, 15, 0.57).
narrative_ontology:measurement(end__be_t20, end_of_life_authority__autonomy_reading, base_extractiveness, 20, 0.6).
narrative_ontology:measurement(end__be_t25, end_of_life_authority__autonomy_reading, base_extractiveness, 25, 0.62).
narrative_ontology:measurement(end__be_t30, end_of_life_authority__autonomy_reading, base_extractiveness, 30, 0.61).
narrative_ontology:measurement(end__be_t40, end_of_life_authority__autonomy_reading, base_extractiveness, 40, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(end__su_t0, end_of_life_authority__autonomy_reading, suppression_requirement, 0, 0.82).
narrative_ontology:measurement(end__su_t5, end_of_life_authority__autonomy_reading, suppression_requirement, 5, 0.8).
narrative_ontology:measurement(end__su_t10, end_of_life_authority__autonomy_reading, suppression_requirement, 10, 0.79).
narrative_ontology:measurement(end__su_t15, end_of_life_authority__autonomy_reading, suppression_requirement, 15, 0.78).
narrative_ontology:measurement(end__su_t20, end_of_life_authority__autonomy_reading, suppression_requirement, 20, 0.77).
narrative_ontology:measurement(end__su_t25, end_of_life_authority__autonomy_reading, suppression_requirement, 25, 0.78).
narrative_ontology:measurement(end__su_t30, end_of_life_authority__autonomy_reading, suppression_requirement, 30, 0.78).
narrative_ontology:measurement(end__su_t40, end_of_life_authority__autonomy_reading, suppression_requirement, 40, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(end_of_life_authority__autonomy_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(end_of_life_authority__autonomy_reading, 0.18).
narrative_ontology:affects_constraint(end_of_life_authority__autonomy_reading, end_of_life_authority__sanctity_reading).
narrative_ontology:affects_constraint(end_of_life_authority__autonomy_reading, end_of_life_authority__slippery_slope_mechanism).

% DUAL FORMULATION NOTE:
% The end_of_life_authority kernel decomposes into three structurally distinct constraints, each instantiating a different reading with different epsilon values and victim sets. The autonomy_reading centers individual choice and faces expansion pressure (victim set grows over time as criteria drift). The sanctity_reading centers intrinsic life-value and resists autonomy recognition (victims are patients whose autonomy claims are denied). The slippery_slope_mechanism readings traces the empirical fate: does autonomy expand beyond stated boundaries? All three readings must be authored as separate constraints linked by network.affects_constraints because their epsilon values and beneficiary/victim structures differ. The autonomy reading influences both siblings: it creates the institutional framework that the sanctity reading opposes and that the slippery-slope mechanism predicts will expand.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
