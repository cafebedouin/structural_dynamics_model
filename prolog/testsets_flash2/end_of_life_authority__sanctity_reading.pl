% ============================================================================
% CONSTRAINT STORY: end_of_life_authority__sanctity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_end_of_life_authority__sanctity_reading, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: end_of_life_authority__sanctity_reading
 *   human_readable: Intrinsic Value of Human Life (Sanctity Reading)
 *   domain: medical_ethics/bioethics/end_of_life_policy
 *
 * SUMMARY:
 *   This constraint represents the 'sanctity of life' reading of end-of-life
 *   authority, which posits an intrinsic value to human life that prohibits
 *   intentional life-ending, regardless of individual preference. It is a
 *   tangled rope because it genuinely coordinates a societal commitment to
 *   life preservation and protection of the vulnerable, but also extracts
 *   significantly from individuals seeking autonomy over their death,
 *   requiring active enforcement against alternative practices. The
 *   structural delta for this reading is that vulnerable populations
 *   (elderly, disabled, economically disadvantaged) are included in the
 *   victim set due to the perceived risk of coercion, and the physician's
 *   role is strictly limited to life preservation.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(end_of_life_authority__sanctity_reading, 0.65).
domain_priors:suppression_score(end_of_life_authority__sanctity_reading, 0.78).
domain_priors:theater_ratio(end_of_life_authority__sanctity_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(end_of_life_authority__sanctity_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(end_of_life_authority__sanctity_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(end_of_life_authority__sanctity_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(end_of_life_authority__sanctity_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(end_of_life_authority__sanctity_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(end_of_life_authority__sanctity_reading, tangled_rope).
narrative_ontology:human_readable(end_of_life_authority__sanctity_reading, "Intrinsic Value of Human Life (Sanctity Reading)").
narrative_ontology:topic_domain(end_of_life_authority__sanctity_reading, "medical_ethics/bioethics/end_of_life_policy").

domain_priors:requires_active_enforcement(end_of_life_authority__sanctity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(end_of_life_authority__sanctity_reading, '5e5f9c83-b07d-475e-8b71-4d2bf98e42d8').
narrative_ontology:cs_kernel_codification('5e5f9c83-b07d-475e-8b71-4d2bf98e42d8', formalized).
narrative_ontology:cs_authority_grounding('5e5f9c83-b07d-475e-8b71-4d2bf98e42d8', lineage).
narrative_ontology:cs_interpretation_layer_present('5e5f9c83-b07d-475e-8b71-4d2bf98e42d8').
narrative_ontology:cs_reading_relation('5e5f9c83-b07d-475e-8b71-4d2bf98e42d8', end_of_life_authority__autonomy_reading, forecloses).
narrative_ontology:cs_reading_relation('5e5f9c83-b07d-475e-8b71-4d2bf98e42d8', end_of_life_authority__slippery_slope_mechanism, influences).
narrative_ontology:cs_axiom('5e5f9c83-b07d-475e-8b71-4d2bf98e42d8', foundational, human_life_has_intrinsic_value).
narrative_ontology:cs_axiom_status(human_life_has_intrinsic_value, holdable).
narrative_ontology:cs_axiom_grounding('5e5f9c83-b07d-475e-8b71-4d2bf98e42d8', human_life_has_intrinsic_value, deontological).
narrative_ontology:cs_axiom('5e5f9c83-b07d-475e-8b71-4d2bf98e42d8', foundational, intentional_life_ending_is_morally_impermissible).
narrative_ontology:cs_axiom_status(intentional_life_ending_is_morally_impermissible, holdable).
narrative_ontology:cs_axiom_grounding('5e5f9c83-b07d-475e-8b71-4d2bf98e42d8', intentional_life_ending_is_morally_impermissible, deontological).
narrative_ontology:cs_reference_frame('5e5f9c83-b07d-475e-8b71-4d2bf98e42d8', traditional_medical_ethics_life_preservation).
narrative_ontology:cs_drift_state('5e5f9c83-b07d-475e-8b71-4d2bf98e42d8', contemporary_autonomy_movement, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('5e5f9c83-b07d-475e-8b71-4d2bf98e42d8', '').
narrative_ontology:cs_kernel_id(end_of_life_authority__sanctity_reading, end_of_life_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(end_of_life_authority__sanctity_reading, religious_institutions).
narrative_ontology:constraint_beneficiary(end_of_life_authority__sanctity_reading, pro_life_advocacy_groups).
narrative_ontology:constraint_beneficiary(end_of_life_authority__sanctity_reading, healthcare_systems_focused_on_preservation).
narrative_ontology:constraint_victim(end_of_life_authority__sanctity_reading, terminally_ill_patients_seeking_assisted_dying).
narrative_ontology:constraint_victim(end_of_life_authority__sanctity_reading, severely_disabled_individuals).
narrative_ontology:constraint_victim(end_of_life_authority__sanctity_reading, elderly_patients_facing_coercion_risk).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(end_of_life_authority__sanctity_reading, physicians_bound_by_sanctity_ethics).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Advocate for and enforce policies reflecting the sanctity of life, viewing all intentional life-ending as morally impermissible. They provide moral and theological grounding for the constraint, influencing legislation and medical practice.
narrative_ontology:constraint_stakeholder(end_of_life_authority__sanctity_reading, religious_institutions, agenda_setter,
    institutional, generational, identity_locked, global).

% Benefit from the legal and ethical frameworks that prohibit assisted dying, aligning with their core mission. They actively lobby for the maintenance and strengthening of these prohibitions.
narrative_ontology:constraint_stakeholder(end_of_life_authority__sanctity_reading, pro_life_advocacy_groups, beneficiary,
    organized, biographical, constrained, national).

% Their institutional mandate and funding structures are often aligned with life preservation at all costs, benefiting from policies that prohibit assisted dying. They implement the constraint through medical protocols and ethical guidelines.
narrative_ontology:constraint_stakeholder(end_of_life_authority__sanctity_reading, healthcare_systems_focused_on_preservation, beneficiary,
    institutional, generational, constrained, national).

% Are denied the option of physician-assisted dying, even when facing unbearable suffering and a clear prognosis. They bear the direct cost of prolonged suffering and loss of autonomy over their end-of-life choices.
narrative_ontology:constraint_stakeholder(end_of_life_authority__sanctity_reading, terminally_ill_patients_seeking_assisted_dying, payer,
    powerless, immediate, trapped, local).

% Are implicitly included in the victim set due to concerns that assisted dying frameworks could devalue their lives or expose them to pressure. While not directly seeking assisted dying, they bear the cost of a system that may not fully respect their autonomy in other end-of-life decisions.
narrative_ontology:constraint_stakeholder(end_of_life_authority__sanctity_reading, severely_disabled_individuals, payer,
    powerless, biographical, identity_locked, local).

% Are protected from potential coercion by family or economic pressures to end their lives, but also lose the option of choosing assisted dying if they genuinely desire it. They bear the cost of a blanket prohibition designed to protect the vulnerable.
narrative_ontology:constraint_stakeholder(end_of_life_authority__sanctity_reading, elderly_patients_facing_coercion_risk, payer,
    powerless, immediate, trapped, local).

% Are ethically and legally bound to preserve life, even when a patient requests assistance in dying. This limits their professional scope and can create moral distress when patient wishes conflict with the sanctity-of-life principle.
narrative_ontology:constraint_stakeholder(end_of_life_authority__sanctity_reading, physicians_bound_by_sanctity_ethics, payer,
    moderate, biographical, constrained, local).
narrative_ontology:stakeholder_secondary_role(end_of_life_authority__sanctity_reading, physicians_bound_by_sanctity_ethics, agenda_setter).

% Advocate for individual choice and control over end-of-life decisions, but their arguments are often sidelined or dismissed by the dominant sanctity-of-life framework in many jurisdictions. They are excluded from the core decision-making process regarding the legality of assisted dying.
narrative_ontology:constraint_stakeholder(end_of_life_authority__sanctity_reading, autonomy_advocacy_groups, excluded,
    organized, biographical, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates medical practice and public policy around a universal principle of life preservation, aiming to prevent the devaluation of human life and protect vulnerable individuals from coercion in end-of-life decisions.
% TRANSFER_FUNCTION: Transfers the ultimate authority over end-of-life decisions from the individual to a collective moral framework, enforced by legal and medical institutions. It transfers the burden of prolonged suffering to individuals in exchange for a societal commitment to life preservation.
% ABSENT_VOICES: Patients' rights advocates and individuals prioritizing autonomy in end-of-life decisions are often marginalized in policy debates dominated by sanctity-of-life arguments. Their perspectives on self-determination and relief from suffering are not fully integrated into the prevailing framework.
% DISAPPEARANCE_RATIONALE: If the sanctity-of-life principle as a categorical prohibition on intentional life-ending vanished, end-of-life care, medical ethics, and legal frameworks would undergo a profound reorganization. Assisted dying would likely become legal in many places, shifting the burden of choice to individuals and requiring new safeguards against coercion. The role of physicians would expand, and societal views on death and suffering would evolve.
% FOUNDING_PROBLEM: The problem of preventing the arbitrary or coerced ending of human life, particularly for vulnerable populations, and maintaining a societal respect for life's intrinsic value.
% FOUNDING_PROBLEM_CORROBORATION: Religious leaders, many medical ethicists, and pro-life organizations attest that the problem of protecting vulnerable lives and upholding the intrinsic value of life remains live. While autonomy advocates contest the scope of the prohibition, the underlying concern for vulnerability is widely acknowledged, even if the solution is debated.
narrative_ontology:disappearance_verdict(end_of_life_authority__sanctity_reading, world_rearranges).
narrative_ontology:founding_problem_status(end_of_life_authority__sanctity_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(end_of_life_authority__sanctity_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(end_of_life_authority__sanctity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(end_of_life_authority__sanctity_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(end_of_life_authority__sanctity_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(end_of_life_authority__sanctity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(end_of_life_authority__sanctity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.65) because the constraint imposes a categorical prohibition that overrides individual autonomy, leading to prolonged suffering for some. Suppression is also high (0.78) as it requires active legal and ethical enforcement to prevent assisted dying and to maintain the physician's role as a life-preserver. Theater ratio is low (0.15) because the core function of life preservation is genuinely pursued, though the scope of 'vulnerability' and 'protection' is contested. Accessibility collapse is high (0.70) as legal and medical alternatives for intentional life-ending are largely foreclosed. Resistance is moderate (0.45) due to ongoing advocacy for autonomy-based end-of-life options.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of beneficiaries like religious institutions, the constraint is a vital moral safeguard. From the perspective of victims like terminally ill patients, it is an oppressive denial of agency. The engine will compute these divergent classifications based on the declared structural relationships and exit options.
 *
 * DIRECTIONALITY LOGIC:
 *   Religious institutions and pro-life groups are clear beneficiaries, as the constraint aligns with their core values and institutional mandates. Healthcare systems focused on preservation also benefit from a clear, life-affirming directive. Terminally ill patients, severely disabled individuals, and elderly patients facing coercion risk are victims, as their autonomy is curtailed, and they may experience prolonged suffering or a perceived devaluation of their choices. Physicians bound by sanctity ethics are both payers (limited professional scope) and agenda-setters (enforcing the constraint). Autonomy advocacy groups are excluded, as their core arguments are often not integrated into the dominant framework.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate to protect vulnerable lives remains live, preventing it from being a piton. However, the scope of 'vulnerability' and the means of 'protection' are contested. The classification as a tangled rope acknowledges the genuine coordination function (protection) while highlighting the asymmetric extraction (denial of autonomy) and active enforcement required to maintain it. This prevents mislabeling it as a pure snare (ignoring the protective intent) or a pure rope (ignoring the extraction and coercion).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    scope_of_vulnerability,
    'What is the actual scope of ''vulnerable'' populations at risk of coercion, and does the categorical prohibition on assisted dying disproportionately impact competent individuals who are not vulnerable?',
    'Empirical studies on coercion rates in jurisdictions with assisted dying, and detailed analysis of patient demographics seeking assisted dying.',
    'If coercion risk is low and impact on competent individuals is high, the justification for the categorical prohibition weakens, potentially shifting the constraint towards a snare for those denied autonomy. If coercion risk is high, the protective function is reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(scope_of_vulnerability, empirical, 'Ambiguity regarding the actual risk of coercion versus the impact on individual autonomy.').

omega_variable(
    physician_role_conflict,
    'Does the sanctity-of-life reading create an unresolvable moral conflict for physicians between preserving life and alleviating suffering, especially when a patient''s suffering is unbearable and their desire for assisted dying is clear?',
    'Qualitative studies of physician experiences in jurisdictions with and without assisted dying, and ethical analysis of professional duties.',
    'If the conflict is severe and unresolvable, it suggests a structural flaw in the constraint''s application, potentially increasing the ''payer'' burden on physicians and highlighting the constraint''s extractive nature on their professional integrity. If the conflict is manageable, the constraint''s coherence is reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(physician_role_conflict, conceptual, 'Moral conflict for physicians between life preservation and suffering alleviation.').

omega_variable(
    sanctity_vs_autonomy_framing,
    'Is the ''intrinsic value of human life'' a universally shared and non-negotiable moral axiom, or is it a specific ethical framework that competes with other valid frameworks, such as individual autonomy?',
    'Philosophical and ethical discourse, cross-cultural comparative studies of end-of-life values, and legal precedent in diverse jurisdictions.',
    'If universally shared, the constraint''s ''mountain-like'' claim to naturalness is strengthened. If it is a competing framework, the constraint is more clearly a constructed ''tangled rope'' or ''snare'' that imposes one value system over others, increasing its perceived extractiveness for those who prioritize autonomy.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sanctity_vs_autonomy_framing, preference, 'Conceptual ambiguity regarding the foundational status of the sanctity-of-life principle.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(end_of_life_authority__sanctity_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(end__tr_t0, end_of_life_authority__sanctity_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(end__tr_t10, end_of_life_authority__sanctity_reading, theater_ratio, 10, 0.12).
narrative_ontology:measurement(end__tr_t20, end_of_life_authority__sanctity_reading, theater_ratio, 20, 0.15).
narrative_ontology:measurement(end__tr_t30, end_of_life_authority__sanctity_reading, theater_ratio, 30, 0.14).
narrative_ontology:measurement(end__tr_t40, end_of_life_authority__sanctity_reading, theater_ratio, 40, 0.16).
narrative_ontology:measurement(end__tr_t50, end_of_life_authority__sanctity_reading, theater_ratio, 50, 0.15).

% Extraction over time
narrative_ontology:measurement(end__be_t0, end_of_life_authority__sanctity_reading, base_extractiveness, 0, 0.6).
narrative_ontology:measurement(end__be_t10, end_of_life_authority__sanctity_reading, base_extractiveness, 10, 0.62).
narrative_ontology:measurement(end__be_t20, end_of_life_authority__sanctity_reading, base_extractiveness, 20, 0.65).
narrative_ontology:measurement(end__be_t30, end_of_life_authority__sanctity_reading, base_extractiveness, 30, 0.64).
narrative_ontology:measurement(end__be_t40, end_of_life_authority__sanctity_reading, base_extractiveness, 40, 0.66).
narrative_ontology:measurement(end__be_t50, end_of_life_authority__sanctity_reading, base_extractiveness, 50, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(end__su_t0, end_of_life_authority__sanctity_reading, suppression_requirement, 0, 0.7).
narrative_ontology:measurement(end__su_t10, end_of_life_authority__sanctity_reading, suppression_requirement, 10, 0.73).
narrative_ontology:measurement(end__su_t20, end_of_life_authority__sanctity_reading, suppression_requirement, 20, 0.76).
narrative_ontology:measurement(end__su_t30, end_of_life_authority__sanctity_reading, suppression_requirement, 30, 0.78).
narrative_ontology:measurement(end__su_t40, end_of_life_authority__sanctity_reading, suppression_requirement, 40, 0.77).
narrative_ontology:measurement(end__su_t50, end_of_life_authority__sanctity_reading, suppression_requirement, 50, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(end_of_life_authority__sanctity_reading, identity_coordination).
narrative_ontology:affects_constraint(end_of_life_authority__sanctity_reading, end_of_life_authority__autonomy_reading).
narrative_ontology:affects_constraint(end_of_life_authority__sanctity_reading, end_of_life_authority__slippery_slope_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
