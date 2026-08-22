% ============================================================================
% CONSTRAINT STORY: end_of_life_decision_authority__sanctity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_end_of_life_decision_authority__sanctity_reading, []).

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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: end_of_life_decision_authority__sanctity_reading
 *   human_readable: Sanctity-of-Life Prohibition on Intentional Life-Ending
 *   domain: medical ethics / bioethics / end-of-life policy
 *
 * SUMMARY:
 *   This story instantiates the sanctity reading of the
 *   end_of_life_decision_authority kernel: human life has intrinsic value not
 *   conferred by or contingent on individual will, and intentional
 *   life-ending — even at competent request — violates that value regardless
 *   of consent quality. The reading's ε refers to the standing arrangement it
 *   defends: the categorical criminal/professional prohibition on assisted
 *   death, as currently enforced. Under this reading, competent suffering
 *   patients who want death and cannot obtain it are the identifiable
 *   cost-bearers (payers), while a population of potentially
 *   pressured/vulnerable patients is protected by the categorical bar's very
 *   existence — they never enter a decision process that could be corrupted
 *   because the process does not exist. The physician's role is
 *   constitutionally healer-only: assisting death is foreclosed from
 *   professional identity, not merely disfavored. This is a genuinely
 *   distinct constraint from the autonomy and vulnerability-protection
 *   readings — not a different description of the same one — because its
 *   beneficiary/victim structure and its ε (extraction as measured against a
 *   categorical prohibition, not a regulated pathway) differ materially.
 *
 * KEY AGENTS:
 *   - competent_suffering_patients_seeking_death: Primary target (powerless/trapped) — bears the cost of no lawful exit
 *   - vulnerable_terminally_ill_at_risk_of_pressure: Primary beneficiary (powerless/trapped) — protected by the bar's categorical nature, without exercising choice about it
 *   - religious_and_traditional_medical_institutions: Agenda-setter (institutional/arbitrage) — articulates and enforces the doctrine
 *   - palliative_care_establishment: Beneficiary/agenda-setter (institutional/arbitrage) — sole lawful channel for terminal suffering
 *   - treating_physicians: Frontline enforcer and secondary payer (moderate/constrained) — bears moral distress of the healer-only boundary
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(end_of_life_decision_authority__sanctity_reading, 0.42).
domain_priors:suppression_score(end_of_life_decision_authority__sanctity_reading, 0.61).
domain_priors:theater_ratio(end_of_life_decision_authority__sanctity_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(end_of_life_decision_authority__sanctity_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(end_of_life_decision_authority__sanctity_reading, suppression_requirement, 0.61).
narrative_ontology:constraint_metric(end_of_life_decision_authority__sanctity_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(end_of_life_decision_authority__sanctity_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(end_of_life_decision_authority__sanctity_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(end_of_life_decision_authority__sanctity_reading, tangled_rope).
narrative_ontology:human_readable(end_of_life_decision_authority__sanctity_reading, "Sanctity-of-Life Prohibition on Intentional Life-Ending").
narrative_ontology:topic_domain(end_of_life_decision_authority__sanctity_reading, "medical ethics / bioethics / end-of-life policy").

domain_priors:requires_active_enforcement(end_of_life_decision_authority__sanctity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(end_of_life_decision_authority__sanctity_reading, 'dfa4a834-7752-47c9-ae33-6ac0cddfd803').
narrative_ontology:cs_kernel_codification('dfa4a834-7752-47c9-ae33-6ac0cddfd803', distributed).
narrative_ontology:cs_authority_grounding('dfa4a834-7752-47c9-ae33-6ac0cddfd803', lineage).
narrative_ontology:cs_interpretation_layer_present('dfa4a834-7752-47c9-ae33-6ac0cddfd803').
narrative_ontology:cs_reading_relation('dfa4a834-7752-47c9-ae33-6ac0cddfd803', end_of_life_decision_authority__autonomy_reading, forecloses).
narrative_ontology:cs_reading_relation('dfa4a834-7752-47c9-ae33-6ac0cddfd803', end_of_life_decision_authority__vulnerability_protection_reading, influences).
narrative_ontology:cs_axiom('dfa4a834-7752-47c9-ae33-6ac0cddfd803', foundational, life_value_independent_of_individual_will).
narrative_ontology:cs_axiom_status(life_value_independent_of_individual_will, holdable).
narrative_ontology:cs_axiom_grounding('dfa4a834-7752-47c9-ae33-6ac0cddfd803', life_value_independent_of_individual_will, deontological).
narrative_ontology:cs_axiom('dfa4a834-7752-47c9-ae33-6ac0cddfd803', secondary, physician_role_constitutively_healer_only).
narrative_ontology:cs_axiom_status(physician_role_constitutively_healer_only, holdable).
narrative_ontology:cs_axiom_grounding('dfa4a834-7752-47c9-ae33-6ac0cddfd803', physician_role_constitutively_healer_only, conventional).
narrative_ontology:cs_reference_frame('dfa4a834-7752-47c9-ae33-6ac0cddfd803', hippocratic_prohibition_tradition).
narrative_ontology:cs_drift_state('dfa4a834-7752-47c9-ae33-6ac0cddfd803', contemporary_secular_pluralist_era, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('dfa4a834-7752-47c9-ae33-6ac0cddfd803', '').
narrative_ontology:cs_kernel_id(end_of_life_decision_authority__sanctity_reading, end_of_life_decision_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(end_of_life_decision_authority__sanctity_reading, vulnerable_terminally_ill_at_risk_of_pressure).
narrative_ontology:constraint_beneficiary(end_of_life_decision_authority__sanctity_reading, disability_rights_advocates).
narrative_ontology:constraint_beneficiary(end_of_life_decision_authority__sanctity_reading, religious_and_traditional_medical_institutions).
narrative_ontology:constraint_beneficiary(end_of_life_decision_authority__sanctity_reading, palliative_care_establishment).
narrative_ontology:constraint_victim(end_of_life_decision_authority__sanctity_reading, competent_suffering_patients_seeking_death).
narrative_ontology:constraint_victim(end_of_life_decision_authority__sanctity_reading, families_bearing_prolonged_dying_burden).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(end_of_life_decision_authority__sanctity_reading, treating_physicians).
narrative_ontology:constraint_vindicates(end_of_life_decision_authority__sanctity_reading, intrinsic_human_dignity_doctrine).
narrative_ontology:constraint_vindicates(end_of_life_decision_authority__sanctity_reading, physician_healer_only_role).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Terminally ill or intractably suffering, mentally competent, and requesting assistance to end their life on their own terms. Under this reading, no legal pathway exists regardless of consent quality or documented capacity; the prohibition applies to them as a categorical bar, not a case-by-case judgment. Their only lawful exits are continued suffering, unassisted self-harm, or travel to a permissive jurisdiction if resources allow.
narrative_ontology:constraint_stakeholder(end_of_life_decision_authority__sanctity_reading, competent_suffering_patients_seeking_death, payer,
    powerless, immediate, trapped, national).

% Caregivers absorbing the emotional, financial, and physical costs of prolonged terminal decline that the prohibition extends by removing a hastened-death option. They can seek hospice or palliative sedation but cannot lawfully honor a patient's request to end suffering directly.
narrative_ontology:constraint_stakeholder(end_of_life_decision_authority__sanctity_reading, families_bearing_prolonged_dying_burden, payer,
    moderate, biographical, constrained, regional).

% Patients whose apparent 'choice' for death could be shaped by family financial strain, disability discrimination, inadequate palliative access, or depression rather than genuine considered will. Under this reading they are protected by the categorical bar itself, since no euthanasia pathway exists for anyone to be steered into. They do not choose this protection; it is structurally guaranteed by the absence of the option for all.
narrative_ontology:constraint_stakeholder(end_of_life_decision_authority__sanctity_reading, vulnerable_terminally_ill_at_risk_of_pressure, beneficiary,
    powerless, immediate, trapped, national).

% Organizations arguing that any legal death-hastening pathway inevitably signals disabled or dependent lives are less worth living and creates pressure toward death as a cost-saving or convenience measure. They actively lobby for the prohibition's maintenance and testify in legislative and judicial proceedings.
narrative_ontology:constraint_stakeholder(end_of_life_decision_authority__sanctity_reading, disability_rights_advocates, beneficiary,
    organized, generational, mobile, national).

% Churches, medical associations, and professional bodies that articulate and enforce the intrinsic-value doctrine through ethics codes, hospital policy, licensing standards, and public advocacy. They administer the boundary of the physician's healer-only role and can lobby to tighten or loosen enforcement without themselves bearing the costs of prolonged dying.
narrative_ontology:constraint_stakeholder(end_of_life_decision_authority__sanctity_reading, religious_and_traditional_medical_institutions, agenda_setter,
    institutional, civilizational, arbitrage, national).

% Hospice and palliative medicine institutions whose funding, staffing, and professional mandate depend on being the sole lawful response to terminal suffering. The prohibition channels all end-of-life demand toward their services and forecloses a competing pathway that would reduce reliance on comfort-care infrastructure.
narrative_ontology:constraint_stakeholder(end_of_life_decision_authority__sanctity_reading, palliative_care_establishment, beneficiary,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(end_of_life_decision_authority__sanctity_reading, palliative_care_establishment, agenda_setter).

% Bound by licensing law and professional codes to a healer-only role; cannot lawfully assist a patient's death even when they judge the request competent and the suffering irremediable. They enforce the boundary daily at the bedside and bear the moral distress of refusing patients they may privately believe should have the option.
narrative_ontology:constraint_stakeholder(end_of_life_decision_authority__sanctity_reading, treating_physicians, agenda_setter,
    moderate, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(end_of_life_decision_authority__sanctity_reading, treating_physicians, payer).

% Enact and adjudicate the criminal and civil prohibitions on assisted dying, define narrow exceptions (withdrawal of treatment, palliative sedation), and respond to litigation and ballot initiatives that test the boundary.
narrative_ontology:constraint_stakeholder(end_of_life_decision_authority__sanctity_reading, state_legislatures_and_courts, agenda_setter,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(end_of_life_decision_authority__sanctity_reading, diffuse).
narrative_ontology:fixing_cost_class(end_of_life_decision_authority__sanctity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single, uniform, non-negotiable floor under which no life-ending act by a physician or third party is ever lawful, removing the need for case-by-case adjudication of when killing a patient is acceptable and shielding vulnerable patients from any circumstance-dependent pressure calculus.
% TRANSFER_FUNCTION: Moves the burden of unrelieved suffering from a hypothetical population of steered/coerced patients (protected) onto the actual population of currently competent, non-coerced patients who want death and cannot lawfully obtain it, and onto the families who absorb the extended dying process.
% ABSENT_VOICES: Competent patients currently suffering and requesting death are not represented in the doctrine's formation; their testimony is treated as evidence of the underlying suffering to be palliated, not as authoritative preference-setting. Disability advocates and religious institutions who benefit from the categorical bar are well-represented in its maintenance; the patients paying its cost are not organized as a lobby because they die before mobilizing.
% DISAPPEARANCE_RATIONALE: If the prohibition vanished overnight without any replacement framework, physicians could legally comply with death requests with no safeguard structure in place; palliative care funding and referral patterns would likely shift, disability advocacy groups would mobilize in protest, and some vulnerable patients could face pressure absent the categorical protection the reading identifies as the doctrine's core function.
% FOUNDING_PROBLEM: Historically: prevent physicians and others from killing patients under cover of mercy, compassion, or convenience, and prevent the medical profession from ever treating any patient's death as a treatment option — rooted in traditions holding that life has value the individual does not confer and cannot revoke by choice.
% FOUNDING_PROBLEM_CORROBORATION: Religious and medical-tradition institutions and disability rights organizations (largely outside the group of currently suffering patients) attest the founding problem remains live: coercion and devaluation risk for vulnerable populations. Palliative care organizations, which benefit from the prohibition's exclusivity, also attest it is live but are not independent of the arrangement's benefits. Bioethicists studying jurisdictions with legalized assistance report no clear evidence of the predicted coercion at scale, which the sanctity reading's own institutions dispute as inconclusive or premature.
narrative_ontology:disappearance_verdict(end_of_life_decision_authority__sanctity_reading, world_rearranges).
narrative_ontology:founding_problem_status(end_of_life_decision_authority__sanctity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(end_of_life_decision_authority__sanctity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(end_of_life_decision_authority__sanctity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(end_of_life_decision_authority__sanctity_reading, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(end_of_life_decision_authority__sanctity_reading_tests).
:- end_tests(end_of_life_decision_authority__sanctity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.42) is moderate: the prohibition does not extract wealth or labor in the ordinary sense, but it extracts suffering-time from competent patients who are denied a pathway they seek, and this cost compounds over decades as the population of long-lived terminal illness grows relative to available palliative capacity — hence the modest upward drift in the temporal series. Suppression (0.61) is substantial because the bar is enforced through criminal law, licensing sanctions, and professional exclusion, not merely persuasion; there is no case-by-case override for documented competence. Accessibility collapse (0.58) is moderate-high: once a patient understands the legal landscape, essentially no lawful alternative exists within the jurisdiction, though jurisdiction-shopping is a partial (costly) alternative for the resourced. Resistance (0.72) is high, reflecting the sustained, well-organized advocacy from competent-death-seeking patients, right-to-die organizations, and reform-minded legislators contesting the doctrine.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setting institutional seat, the prohibition looks like categorical coordination protecting an irreducible value from case-by-case erosion — a Tangled Rope's coordination face. From the competent-patient payer seat, the same structure looks like enforced extraction of suffering-time with no consent mechanism and no exit, which is the extraction face the tangled_rope classification requires alongside the coordination face. Both faces are structurally present in this reading's own account, which is why tangled_rope (not mountain or snare alone) is the claimed type.
 *
 * DIRECTIONALITY LOGIC:
 *   Competent suffering patients are the clearest targets: the prohibition applies to them specifically and produces a direct, felt cost with no coordination benefit accruing back to them under this reading. Vulnerable-to-pressure patients are beneficiaries by structural default — they benefit from a prohibition they did not ask for and cannot decline, which is exactly the derivation chain's beneficiary case (low d) even though they never articulate consent to the protection. Religious/medical institutions and the palliative establishment sit furthest toward the beneficiary end with high exit options (arbitrage) since they administer rather than experience the bar. Physicians occupy a hybrid position — enforcing the doctrine (agenda_setter) while personally absorbing its moral cost (payer) — reflected in the dual role.
 *
 * MANDATROPHY ANALYSIS:
 *   The doctrine's founding problem — preventing coerced or convenience-motivated killing under medical cover — is contested rather than dead: disability advocates and religious institutions attest it remains live, while jurisdictions with legalized, safeguarded assistance report no clear-cut evidence of the predicted coercion at scale, which this reading's own institutions dispute as inconclusive. Because the founding-problem status is genuinely contested rather than either clearly live or clearly dead, mandatrophy is not resolved either way by this story; the classification should not be read as settling whether the prohibition currently serves a live protective function or has become inertial protection of institutional prerogative (palliative-care exclusivity, professional identity boundaries) that has outlived clear justification.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    empirical_coercion_evidence_under_alternative_readings,
    'Does empirical data from jurisdictions operating under the autonomy_reading or vulnerability_protection_reading show measurable coercion or devaluation effects on vulnerable populations, or does the sanctity reading''s predicted harm fail to materialize under regulated alternatives?',
    'Longitudinal comparative study of disability-community and low-income patient outcomes in jurisdictions with legalized, safeguarded assisted dying versus this reading''s categorical-prohibition jurisdictions, controlling for palliative care access quality.',
    'If coercion effects are empirically negligible under safeguarded alternatives, the sanctity reading''s core protective claim for vulnerable_terminally_ill_at_risk_of_pressure weakens substantially, suggesting the beneficiary designation reflects precautionary framing rather than demonstrated protection. If coercion effects are found, the reading''s beneficiary claim strengthens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(empirical_coercion_evidence_under_alternative_readings, empirical, 'Whether the predicted vulnerability-protection function is empirically borne out or is a precautionary construct.').

omega_variable(
    intrinsic_value_naturalness_vs_construction,
    'Is ''intrinsic value independent of individual will'' a discoverable moral fact this reading merely reports, or a constructed doctrine that happens to benefit specific institutions (religious authorities, palliative care establishment, disability advocacy organizations) who administer and enforce it?',
    'No empirical resolution exists for a metaethical claim of this kind; the ambiguity is irreducible and depends on background moral-realist versus constructivist commitments the framework cannot adjudicate.',
    'If intrinsic value is a genuine moral fact, the prohibition is closer to a mountain-like constraint reporting an irreducible limit. If it is constructed doctrine serving identifiable institutional beneficiaries, the tangled_rope classification (or even a snare reading) is the more accurate structural account, with the coordination story functioning as cover for extraction from competent suffering patients.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(intrinsic_value_naturalness_vs_construction, conceptual, 'Whether the sanctity claim is a discovered moral fact or a constructed doctrine serving identifiable beneficiaries.').

omega_variable(
    physician_role_boundary_stability,
    'Is the healer-only physician role a fixed professional identity constraint, or would it shift substantially if medical associations changed position (as several have, jurisdiction by jurisdiction)?',
    'Track professional association position changes over time and correlate with legislative outcomes to assess whether the healer-only boundary is institutionally load-bearing or contingent on current leadership consensus.',
    'If the boundary is contingent and has shifted elsewhere, the sanctity reading''s physician-role claim is a current institutional position rather than a structural feature of medicine, which weakens the case for treating it as anything more durable than policy.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(physician_role_boundary_stability, conceptual, 'Whether the physician healer-only role is structurally fixed or a shiftable institutional consensus.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(end_of_life_decision_authority__sanctity_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(end__tr_t0, end_of_life_decision_authority__sanctity_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(end__tr_t8, end_of_life_decision_authority__sanctity_reading, theater_ratio, 8, 0.12).
narrative_ontology:measurement(end__tr_t16, end_of_life_decision_authority__sanctity_reading, theater_ratio, 16, 0.14).
narrative_ontology:measurement(end__tr_t24, end_of_life_decision_authority__sanctity_reading, theater_ratio, 24, 0.16).
narrative_ontology:measurement(end__tr_t32, end_of_life_decision_authority__sanctity_reading, theater_ratio, 32, 0.18).
narrative_ontology:measurement(end__tr_t40, end_of_life_decision_authority__sanctity_reading, theater_ratio, 40, 0.2).

% Extraction over time
narrative_ontology:measurement(end__be_t0, end_of_life_decision_authority__sanctity_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(end__be_t8, end_of_life_decision_authority__sanctity_reading, base_extractiveness, 8, 0.33).
narrative_ontology:measurement(end__be_t16, end_of_life_decision_authority__sanctity_reading, base_extractiveness, 16, 0.36).
narrative_ontology:measurement(end__be_t24, end_of_life_decision_authority__sanctity_reading, base_extractiveness, 24, 0.39).
narrative_ontology:measurement(end__be_t32, end_of_life_decision_authority__sanctity_reading, base_extractiveness, 32, 0.41).
narrative_ontology:measurement(end__be_t40, end_of_life_decision_authority__sanctity_reading, base_extractiveness, 40, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(end__su_t0, end_of_life_decision_authority__sanctity_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(end__su_t8, end_of_life_decision_authority__sanctity_reading, suppression_requirement, 8, 0.53).
narrative_ontology:measurement(end__su_t16, end_of_life_decision_authority__sanctity_reading, suppression_requirement, 16, 0.56).
narrative_ontology:measurement(end__su_t24, end_of_life_decision_authority__sanctity_reading, suppression_requirement, 24, 0.58).
narrative_ontology:measurement(end__su_t32, end_of_life_decision_authority__sanctity_reading, suppression_requirement, 32, 0.6).
narrative_ontology:measurement(end__su_t40, end_of_life_decision_authority__sanctity_reading, suppression_requirement, 40, 0.61).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(end_of_life_decision_authority__sanctity_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(end_of_life_decision_authority__sanctity_reading, 0.12).
narrative_ontology:affects_constraint(end_of_life_decision_authority__sanctity_reading, end_of_life_decision_authority__autonomy_reading).
narrative_ontology:affects_constraint(end_of_life_decision_authority__sanctity_reading, end_of_life_decision_authority__vulnerability_protection_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the end_of_life_decision_authority kernel (per the ε-invariance decomposition principle). The autonomy_reading treats competent individual will as sovereign and would identify denied-choice patients as the primary victim class. The vulnerability_protection_reading treats institutional checkpoint distribution as the legitimate resolution and identifies both under-protected and over-restricted patients as conditional victims depending on checkpoint calibration. Each reading has a distinct ε, distinct beneficiary/victim sets, and a distinct claimed_type; they are linked here rather than merged because measuring 'end-of-life decision authority' by different observables (whose will counts, whose vulnerability counts, what institutional process counts) yields materially different extraction profiles — exactly the signal the ε-invariance test flags as requiring decomposition into separate stories.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
