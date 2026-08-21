% ============================================================================
% CONSTRAINT STORY: end_of_life_authority__autonomy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
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
 *   constraint_id: end_of_life_authority__autonomy_reading
 *   human_readable: Right to Self-Determination in End-of-Life Care (Autonomy Reading)
 *   domain: medical_ethics/bioethics/end_of_life_policy
 *
 * SUMMARY:
 *   This constraint story instantiates the 'autonomy_reading' of the
 *   'end_of_life_authority' kernel. It describes the structural implications
 *   of asserting individual autonomy as the grounding for the right to
 *   control the circumstances and timing of death when facing unbearable
 *   suffering. While the claimed type is 'rope' (representing the ideal of
 *   coordinated respect for autonomy), the metrics reflect the current
 *   reality where this right is often denied, leading to high extraction
 *   (prolonged suffering) and suppression (legal/medical barriers) for those
 *   seeking to exercise it. The 'suffering-prolonged' enter the victim set,
 *   and paternalistic restrictions are actively suppressed by proponents of
 *   this reading.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(end_of_life_authority__autonomy_reading, 0.78).
domain_priors:suppression_score(end_of_life_authority__autonomy_reading, 0.85).
domain_priors:theater_ratio(end_of_life_authority__autonomy_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(end_of_life_authority__autonomy_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(end_of_life_authority__autonomy_reading, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(end_of_life_authority__autonomy_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(end_of_life_authority__autonomy_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(end_of_life_authority__autonomy_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(end_of_life_authority__autonomy_reading, rope).
narrative_ontology:human_readable(end_of_life_authority__autonomy_reading, "Right to Self-Determination in End-of-Life Care (Autonomy Reading)").
narrative_ontology:topic_domain(end_of_life_authority__autonomy_reading, "medical_ethics/bioethics/end_of_life_policy").

domain_priors:requires_active_enforcement(end_of_life_authority__autonomy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(end_of_life_authority__autonomy_reading, '73032986-f992-4917-b4da-cc76b546b682').
narrative_ontology:cs_kernel_codification('73032986-f992-4917-b4da-cc76b546b682', formalized).
narrative_ontology:cs_authority_grounding('73032986-f992-4917-b4da-cc76b546b682', expertise).
narrative_ontology:cs_interpretation_layer_present('73032986-f992-4917-b4da-cc76b546b682').
narrative_ontology:cs_reading_relation('73032986-f992-4917-b4da-cc76b546b682', end_of_life_authority__sanctity_reading, forecloses).
narrative_ontology:cs_reading_relation('73032986-f992-4917-b4da-cc76b546b682', end_of_life_authority__slippery_slope_mechanism, influences).
narrative_ontology:cs_axiom('73032986-f992-4917-b4da-cc76b546b682', foundational, individual_self_ownership).
narrative_ontology:cs_axiom_status(individual_self_ownership, holdable).
narrative_ontology:cs_axiom_grounding('73032986-f992-4917-b4da-cc76b546b682', individual_self_ownership, deontological).
narrative_ontology:cs_axiom('73032986-f992-4917-b4da-cc76b546b682', secondary, relief_of_unbearable_suffering).
narrative_ontology:cs_axiom_status(relief_of_unbearable_suffering, holdable).
narrative_ontology:cs_axiom_grounding('73032986-f992-4917-b4da-cc76b546b682', relief_of_unbearable_suffering, instrumental).
narrative_ontology:cs_reference_frame('73032986-f992-4917-b4da-cc76b546b682', patient_centered_care_paradigm).
narrative_ontology:cs_drift_state('73032986-f992-4917-b4da-cc76b546b682', contemporary_legal_challenges, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('73032986-f992-4917-b4da-cc76b546b682', '').
narrative_ontology:cs_kernel_id(end_of_life_authority__autonomy_reading, end_of_life_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(end_of_life_authority__autonomy_reading, patients_seeking_maid).
narrative_ontology:constraint_beneficiary(end_of_life_authority__autonomy_reading, advocacy_groups).
narrative_ontology:constraint_victim(end_of_life_authority__autonomy_reading, patients_denied_maid_choice).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(end_of_life_authority__autonomy_reading, medical_professionals).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Individuals facing unbearable suffering who seek to exercise control over the timing and circumstances of their death. Their autonomy is the core right this reading seeks to uphold.
narrative_ontology:constraint_stakeholder(end_of_life_authority__autonomy_reading, patients_seeking_maid, beneficiary,
    powerless, immediate, identity_locked, local).

% Individuals whose requests for medical assistance in dying (MAID) are denied due to legal, institutional, or medical restrictions, forcing them to endure prolonged suffering against their will. They bear the direct cost of the constraint's non-recognition.
narrative_ontology:constraint_stakeholder(end_of_life_authority__autonomy_reading, patients_denied_maid_choice, payer,
    powerless, immediate, trapped, local).

% Organizations and individuals campaigning for the legal recognition and implementation of MAID based on patient autonomy. They benefit from the expansion of this right and work to overcome existing barriers.
narrative_ontology:constraint_stakeholder(end_of_life_authority__autonomy_reading, advocacy_groups, beneficiary,
    organized, generational, mobile, national).

% Physicians, nurses, and other healthcare providers who are responsible for assessing patient eligibility, providing care, and potentially administering MAID. They navigate complex ethical and legal frameworks, bearing the professional and emotional burden of these decisions.
narrative_ontology:constraint_stakeholder(end_of_life_authority__autonomy_reading, medical_professionals, agenda_setter,
    institutional, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(end_of_life_authority__autonomy_reading, medical_professionals, payer).

% Legislators and regulators responsible for drafting and implementing laws and policies concerning end-of-life care, including MAID. They respond to public pressure, ethical arguments, and legal challenges.
narrative_ontology:constraint_stakeholder(end_of_life_authority__autonomy_reading, policy_makers, agenda_setter,
    institutional, generational, mobile, national).

% Organizations that oppose MAID on grounds of the sanctity of life, often advocating for its prohibition or severe restriction. While influential in public discourse, their theological arguments are structurally excluded from the autonomy-based legal framework this reading seeks to establish.
narrative_ontology:constraint_stakeholder(end_of_life_authority__autonomy_reading, religious_institutions, excluded,
    organized, civilizational, constrained, global).

% Scholars and practitioners who analyze the ethical implications of end-of-life decisions, contributing to the conceptual framework and public debate around patient autonomy and MAID.
narrative_ontology:constraint_stakeholder(end_of_life_authority__autonomy_reading, bioethicists, observer,
    analytical, generational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To coordinate medical practice, legal frameworks, and patient wishes to ensure that individual autonomy is respected in end-of-life decisions, particularly regarding the timing and circumstances of death when facing unbearable suffering.
% TRANSFER_FUNCTION: Transfers ultimate decision-making authority over one's own death from medical or state paternalism to the individual patient, when facing unbearable suffering. This involves a transfer of control and a redefinition of rights.
% ABSENT_VOICES: Patients who are currently suffering but lack the capacity to express their wishes or advocate for MAID, as well as those in jurisdictions where this right is not recognized, are effectively silenced by existing legal and medical structures.
% DISAPPEARANCE_RATIONALE: If the principle of individual autonomy in end-of-life decisions vanished overnight, medical practice would revert to purely paternalistic models, legal challenges for MAID would cease, and countless patients would lose a fundamental right, leading to prolonged suffering and loss of dignity for many. The entire framework of patient-centered care would be undermined.
% FOUNDING_PROBLEM: Patients facing unbearable suffering lacked control over their final moments, leading to prolonged distress, loss of dignity, and a sense of powerlessness in the face of their own mortality and medical authority.
% FOUNDING_PROBLEM_CORROBORATION: Patient testimonials, extensive medical ethics literature, and international human rights declarations consistently corroborate the ongoing problem of suffering and the importance of individual autonomy in end-of-life decisions. Legal reforms in numerous jurisdictions also attest to the recognition of this problem.
narrative_ontology:disappearance_verdict(end_of_life_authority__autonomy_reading, world_rearranges).
narrative_ontology:founding_problem_status(end_of_life_authority__autonomy_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(end_of_life_authority__autonomy_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(end_of_life_authority__autonomy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(end_of_life_authority__autonomy_reading, 0.78, 'gemini-2.5-flash', 'none', direct).

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
 *   The high extractiveness (0.78) reflects the profound cost borne by patients denied control over their death, forced to endure suffering. Suppression (0.85) is high due to the significant legal, institutional, and cultural barriers that prevent the exercise of this right in many contexts. Theater ratio is low (0.10) because the debate is genuinely about fundamental rights and ethical principles, not performative maintenance of an atrophied function. Resistance is high (0.75) from patient advocacy groups and legal challenges pushing for recognition of this right. Accessibility collapse is moderate (0.60) as alternatives like palliative care exist, but not the desired option of self-determined death.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of patients denied MAID, the current system is highly extractive and suppressive. From the perspective of advocacy groups, the constraint is a necessary 'rope' to coordinate respect for autonomy, but one that requires significant enforcement to overcome existing barriers. Medical professionals face a tension between their duty to preserve life and their duty to respect patient autonomy, experiencing the constraint as a complex, often burdensome, regulatory framework.
 *
 * DIRECTIONALITY LOGIC:
 *   Patients seeking MAID and advocacy groups are the primary beneficiaries, as the constraint aims to empower them. Patients denied MAID choice are the primary victims, bearing the cost of non-recognition. Medical professionals and policy makers act as agenda-setters, shaping the implementation, but also bear costs (payer role) in navigating complex ethical and legal landscapes. Religious institutions are structurally excluded from the framework of this reading, as their core premises conflict.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is not subject to mandatrophy in the traditional sense, as the underlying problem of unbearable suffering and the desire for autonomy in death remain live and pressing. The classification as 'rope' (ideal state) with high extraction/suppression (current reality) prevents mislabeling the *right itself* as extractive, while accurately measuring the *costs of its denial*.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    slippery_slope_empirical_risk,
    'Does the implementation of autonomy-based MAID frameworks empirically lead to an expansion beyond competent, terminal cases to incompetent or non-terminal populations, as argued by the ''slippery_slope_mechanism'' reading?',
    'Longitudinal empirical studies tracking MAID eligibility and uptake criteria in jurisdictions where it is legalized, comparing actual outcomes against initial legislative intent.',
    'If a significant, unmanaged expansion is empirically demonstrated, it would challenge the ''rope'' classification by revealing unforeseen extractive consequences for vulnerable populations, potentially shifting the classification towards a ''tangled_rope'' or ''snare'' for those populations. If no such expansion occurs, it strengthens the ''rope'' classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(slippery_slope_empirical_risk, empirical, 'Empirical validity of the slippery slope argument in MAID.').

omega_variable(
    definition_of_unbearable_suffering,
    'How is ''unbearable suffering'' defined and consistently applied across medical contexts and patient populations? Is it purely subjective, or are there objective criteria?',
    'Development of standardized, inter-subjectively verifiable clinical guidelines for assessing ''unbearable suffering'' that balance patient subjectivity with medical objectivity, and legal precedent clarifying its scope.',
    'Ambiguity in this definition can lead to arbitrary denials (increasing extraction for some patients) or unwarranted approvals (raising concerns for others), potentially undermining the perceived fairness and coordination function of the constraint. Clarification would strengthen its ''rope'' function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(definition_of_unbearable_suffering, conceptual, 'Conceptual clarity and consistent application of ''unbearable suffering''.').

omega_variable(
    capacity_assessment_reliability,
    'Are current methods for assessing a patient''s decision-making capacity sufficiently reliable and robust to ensure that requests for MAID are truly autonomous and not influenced by depression, coercion, or other factors?',
    'Ongoing research into neuropsychological and psychiatric assessment tools for decision-making capacity in end-of-life contexts, coupled with interdisciplinary consensus on best practices for assessment.',
    'If capacity assessments are found to be unreliable, it introduces a significant risk of false positives (non-autonomous deaths) or false negatives (denial of autonomous choice), eroding trust and challenging the ethical foundation of the autonomy reading. Improved reliability would strengthen the constraint''s legitimacy.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(capacity_assessment_reliability, empirical, 'Reliability of patient capacity assessments for MAID.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(end_of_life_authority__autonomy_reading, 1970, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(end__tr_t1970, end_of_life_authority__autonomy_reading, theater_ratio, 1970, 0.05).
narrative_ontology:measurement(end__tr_t1985, end_of_life_authority__autonomy_reading, theater_ratio, 1985, 0.07).
narrative_ontology:measurement(end__tr_t2000, end_of_life_authority__autonomy_reading, theater_ratio, 2000, 0.08).
narrative_ontology:measurement(end__tr_t2010, end_of_life_authority__autonomy_reading, theater_ratio, 2010, 0.09).
narrative_ontology:measurement(end__tr_t2020, end_of_life_authority__autonomy_reading, theater_ratio, 2020, 0.1).
narrative_ontology:measurement(end__tr_t2025, end_of_life_authority__autonomy_reading, theater_ratio, 2025, 0.1).

% Extraction over time
narrative_ontology:measurement(end__be_t1970, end_of_life_authority__autonomy_reading, base_extractiveness, 1970, 0.9).
narrative_ontology:measurement(end__be_t1985, end_of_life_authority__autonomy_reading, base_extractiveness, 1985, 0.85).
narrative_ontology:measurement(end__be_t2000, end_of_life_authority__autonomy_reading, base_extractiveness, 2000, 0.82).
narrative_ontology:measurement(end__be_t2010, end_of_life_authority__autonomy_reading, base_extractiveness, 2010, 0.8).
narrative_ontology:measurement(end__be_t2020, end_of_life_authority__autonomy_reading, base_extractiveness, 2020, 0.79).
narrative_ontology:measurement(end__be_t2025, end_of_life_authority__autonomy_reading, base_extractiveness, 2025, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(end__su_t1970, end_of_life_authority__autonomy_reading, suppression_requirement, 1970, 0.95).
narrative_ontology:measurement(end__su_t1985, end_of_life_authority__autonomy_reading, suppression_requirement, 1985, 0.9).
narrative_ontology:measurement(end__su_t2000, end_of_life_authority__autonomy_reading, suppression_requirement, 2000, 0.88).
narrative_ontology:measurement(end__su_t2010, end_of_life_authority__autonomy_reading, suppression_requirement, 2010, 0.86).
narrative_ontology:measurement(end__su_t2020, end_of_life_authority__autonomy_reading, suppression_requirement, 2020, 0.85).
narrative_ontology:measurement(end__su_t2025, end_of_life_authority__autonomy_reading, suppression_requirement, 2025, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(end_of_life_authority__autonomy_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(end_of_life_authority__autonomy_reading, medical_liability_laws).
narrative_ontology:affects_constraint(end_of_life_authority__autonomy_reading, palliative_care_funding).
narrative_ontology:affects_constraint(end_of_life_authority__autonomy_reading, patient_rights_legislation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
