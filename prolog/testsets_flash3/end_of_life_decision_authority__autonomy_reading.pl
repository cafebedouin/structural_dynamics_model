% ============================================================================
% CONSTRAINT STORY: end_of_life_decision_authority__autonomy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_end_of_life_decision_authority__autonomy_reading, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: end_of_life_decision_authority__autonomy_reading
 *   human_readable: Competent Individual's End-of-Life Autonomy
 *   domain: medical_ethics/bioethics/end_of_life_policy
 *
 * SUMMARY:
 *   This constraint represents the 'autonomy reading' of end-of-life decision
 *   authority, asserting that competent individuals have sovereign control
 *   over their own death. It frames the denial of aid-in-dying to suffering
 *   patients as a form of extraction (prolonged suffering) and the
 *   legal/medical frameworks that enable patient choice as a coordination
 *   mechanism. The metrics reflect a historical trend where initial high
 *   suppression against patient choice has decreased, and extractiveness
 *   (from those denied choice) has risen as the right to choose becomes more
 *   recognized but still contested.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(end_of_life_decision_authority__autonomy_reading, 0.35).
domain_priors:suppression_score(end_of_life_decision_authority__autonomy_reading, 0.2).
domain_priors:theater_ratio(end_of_life_decision_authority__autonomy_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(end_of_life_decision_authority__autonomy_reading, extractiveness, 0.35).
narrative_ontology:constraint_metric(end_of_life_decision_authority__autonomy_reading, suppression_requirement, 0.2).
narrative_ontology:constraint_metric(end_of_life_decision_authority__autonomy_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(end_of_life_decision_authority__autonomy_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(end_of_life_decision_authority__autonomy_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(end_of_life_decision_authority__autonomy_reading, rope).
narrative_ontology:human_readable(end_of_life_decision_authority__autonomy_reading, "Competent Individual's End-of-Life Autonomy").
narrative_ontology:topic_domain(end_of_life_decision_authority__autonomy_reading, "medical_ethics/bioethics/end_of_life_policy").

domain_priors:requires_active_enforcement(end_of_life_decision_authority__autonomy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(end_of_life_decision_authority__autonomy_reading, 'aaa62d63-daf9-4712-b3df-cbff0a57fa7c').
narrative_ontology:cs_kernel_codification('aaa62d63-daf9-4712-b3df-cbff0a57fa7c', formalized).
narrative_ontology:cs_authority_grounding('aaa62d63-daf9-4712-b3df-cbff0a57fa7c', practice).
narrative_ontology:cs_interpretation_layer_present('aaa62d63-daf9-4712-b3df-cbff0a57fa7c').
narrative_ontology:cs_reading_relation('aaa62d63-daf9-4712-b3df-cbff0a57fa7c', end_of_life_decision_authority__sanctity_reading, coexists_with).
narrative_ontology:cs_reading_relation('aaa62d63-daf9-4712-b3df-cbff0a57fa7c', end_of_life_decision_authority__vulnerability_protection_reading, influences).
narrative_ontology:cs_axiom('aaa62d63-daf9-4712-b3df-cbff0a57fa7c', foundational, individual_self_determination_is_paramount).
narrative_ontology:cs_axiom_status(individual_self_determination_is_paramount, holdable).
narrative_ontology:cs_axiom_grounding('aaa62d63-daf9-4712-b3df-cbff0a57fa7c', individual_self_determination_is_paramount, deontological).
narrative_ontology:cs_axiom('aaa62d63-daf9-4712-b3df-cbff0a57fa7c', secondary, relief_of_intractable_suffering_is_a_moral_imperative).
narrative_ontology:cs_axiom_status(relief_of_intractable_suffering_is_a_moral_imperative, holdable).
narrative_ontology:cs_axiom_grounding('aaa62d63-daf9-4712-b3df-cbff0a57fa7c', relief_of_intractable_suffering_is_a_moral_imperative, deontological).
narrative_ontology:cs_reference_frame('aaa62d63-daf9-4712-b3df-cbff0a57fa7c', patient_centered_autonomy_framework).
narrative_ontology:cs_drift_state('aaa62d63-daf9-4712-b3df-cbff0a57fa7c', contemporary_legal_expansion_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('aaa62d63-daf9-4712-b3df-cbff0a57fa7c', '').
narrative_ontology:cs_kernel_id(end_of_life_decision_authority__autonomy_reading, end_of_life_decision_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(end_of_life_decision_authority__autonomy_reading, competent_patients).
narrative_ontology:constraint_beneficiary(end_of_life_decision_authority__autonomy_reading, advocacy_groups_for_patient_rights).
narrative_ontology:constraint_victim(end_of_life_decision_authority__autonomy_reading, patients_denied_aid_in_dying).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Individuals with decision-making capacity who wish to exercise control over the timing and manner of their death, particularly in the face of intractable suffering. They benefit from legal and medical frameworks that respect their choices.
narrative_ontology:constraint_stakeholder(end_of_life_decision_authority__autonomy_reading, competent_patients, beneficiary,
    moderate, immediate, constrained, local).

% Individuals who, despite meeting competency criteria and expressing a desire for aid-in-dying, are denied access due to legal restrictions or institutional policies. They bear the cost of prolonged suffering and loss of autonomy.
narrative_ontology:constraint_stakeholder(end_of_life_decision_authority__autonomy_reading, patients_denied_aid_in_dying, payer,
    powerless, immediate, trapped, local).

% Physicians, nurses, and other medical staff who are responsible for assessing patient competency, diagnosing conditions, and, where legally permitted, facilitating end-of-life choices. They navigate ethical guidelines, legal frameworks, and personal convictions.
narrative_ontology:constraint_stakeholder(end_of_life_decision_authority__autonomy_reading, healthcare_professionals, agenda_setter,
    institutional, biographical, constrained, national).

% Organizations that champion the rights of patients to make autonomous decisions about their medical care, including end-of-life choices. They work to influence legislation and public opinion, benefiting from the expansion of patient autonomy.
narrative_ontology:constraint_stakeholder(end_of_life_decision_authority__autonomy_reading, advocacy_groups_for_patient_rights, beneficiary,
    organized, generational, mobile, national).

% Organizations that often hold theological objections to intentional life-ending, viewing it as a violation of divine law or the sanctity of life. While influential in public discourse, their direct authority over individual medical decisions is limited in secular legal frameworks.
narrative_ontology:constraint_stakeholder(end_of_life_decision_authority__autonomy_reading, religious_institutions, excluded,
    institutional, civilizational, identity_locked, global).

% Academics and researchers who analyze the ethical, legal, and social implications of end-of-life decisions, contributing to policy debates and public understanding without direct involvement in patient care or policy enforcement.
narrative_ontology:constraint_stakeholder(end_of_life_decision_authority__autonomy_reading, bioethics_scholars, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the medical system's response to a competent patient's request for end-of-life care, ensuring that their autonomous wishes are respected within legal and ethical boundaries, and providing a framework for healthcare professionals to act.
% TRANSFER_FUNCTION: Transfers the ultimate decision-making authority regarding one's own death from external authorities (state, family, medical paternalism) to the competent individual, along with the responsibility for that choice.
% ABSENT_VOICES: Those who prioritize the sanctity of life or the protection of vulnerable populations (e.g., some religious groups, disability rights advocates) are often marginalized in policy discussions centered purely on individual autonomy, arguing that the focus on individual choice overlooks broader societal implications or risks.
% DISAPPEARANCE_RATIONALE: If the principle of individual autonomy over end-of-life decisions vanished, medical practice would revert to a more paternalistic model, legal frameworks for aid-in-dying would collapse, and patients facing suffering would lose a fundamental right to self-determination, leading to widespread ethical and legal challenges.
% FOUNDING_PROBLEM: The historical problem of medical paternalism and the denial of patient agency in end-of-life care, where decisions were often made by doctors or family without sufficient regard for the patient's wishes, leading to prolonged suffering and loss of dignity.
% FOUNDING_PROBLEM_CORROBORATION: Patient advocacy groups, legal scholars, and medical ethicists widely corroborate that the problem of ensuring patient autonomy in end-of-life decisions remains live, citing ongoing legal battles and ethical debates in various jurisdictions. While progress has been made, the full realization of this autonomy is still contested by other ethical frameworks.
narrative_ontology:disappearance_verdict(end_of_life_decision_authority__autonomy_reading, world_rearranges).
narrative_ontology:founding_problem_status(end_of_life_decision_authority__autonomy_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(end_of_life_decision_authority__autonomy_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(end_of_life_decision_authority__autonomy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(end_of_life_decision_authority__autonomy_reading, 0.35, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(end_of_life_decision_authority__autonomy_reading_tests).
:- end_tests(end_of_life_decision_authority__autonomy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.35) is moderate, reflecting the burden of prolonged suffering on patients denied their autonomous choice, but not as high as a pure snare because the constraint also enables choice for many. Suppression (0.20) is relatively low, indicating that while legal and medical barriers exist, there is significant societal and legal movement towards recognizing this autonomy. The claimed type is 'rope' because, from this reading's perspective, it genuinely coordinates patient choice with medical practice, even if some are still 'victims' of its incomplete application. The historical measurements show a decrease in suppression (legal barriers eroding) and a corresponding increase in extractiveness (as the 'right' is recognized, its denial becomes more salient and costly).
 *
 * PERSPECTIVAL GAP:
 *   Other readings (sanctity, vulnerability protection) would experience this constraint very differently. A 'sanctity' reading would see the constraint as highly extractive from society (violating intrinsic value of life) and highly suppressive of moral norms. A 'vulnerability protection' reading would see it as potentially extractive from vulnerable patients (due to coercion risks) and suppressive of safeguards. This autonomy reading focuses on the individual's right, externalizing these other concerns as 'slippery slope' risks rather than intrinsic features of the constraint.
 *
 * DIRECTIONALITY LOGIC:
 *   Competent patients are primary beneficiaries (d near 0.0) as the constraint empowers their choices. Healthcare professionals are agenda-setters, facilitating these choices within legal bounds. Patients denied aid-in-dying are victims (d near 1.0), bearing the cost of denied autonomy. Advocacy groups are beneficiaries, as their mission aligns with the constraint's function. Religious institutions are excluded, as their counter-claims are not central to this reading's framework.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    slippery_slope_risk,
    'Does the expansion of individual end-of-life autonomy inevitably lead to a ''slippery slope'' where vulnerable populations are coerced or pressured into choosing death?',
    'Longitudinal empirical studies from jurisdictions with aid-in-dying laws, tracking rates of coercion, demographic shifts in those utilizing the laws, and the evolution of safeguards.',
    'If a ''slippery slope'' is empirically demonstrated, the constraint''s effective extractiveness (from vulnerable groups) and suppression (of their true will) would be significantly higher, potentially reclassifying it towards a Snare from a vulnerability-protection perspective. If not, the autonomy reading''s externalization of this risk is vindicated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(slippery_slope_risk, empirical, 'The risk of unintended negative consequences for vulnerable populations from expanded end-of-life autonomy.').

omega_variable(
    autonomy_vs_sanctity_framing,
    'Is the ''sovereign authority over one''s own death'' a fundamental, inalienable right (autonomy reading), or does it fundamentally violate an intrinsic, non-negotiable value of human life (sanctity reading)?',
    'This is a conceptual/preference omega. Resolution depends on which foundational ethical framework is adopted. No empirical data can resolve this fundamental normative disagreement.',
    'If the sanctity-of-life framework is adopted, this constraint (autonomy reading) would be reclassified as highly extractive (from the intrinsic value of life) and suppressive (of moral order), likely a Snare. If the autonomy framework prevails, the sanctity reading''s claims are seen as suppressive of individual rights.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(autonomy_vs_sanctity_framing, conceptual, 'Fundamental normative disagreement between individual autonomy and the sanctity of life.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(end_of_life_decision_authority__autonomy_reading, 1970, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Extraction over time
narrative_ontology:measurement(end__be_t1970, end_of_life_decision_authority__autonomy_reading, base_extractiveness, 1970, 0.1).
narrative_ontology:measurement(end__be_t1985, end_of_life_decision_authority__autonomy_reading, base_extractiveness, 1985, 0.2).
narrative_ontology:measurement(end__be_t2000, end_of_life_decision_authority__autonomy_reading, base_extractiveness, 2000, 0.28).
narrative_ontology:measurement(end__be_t2010, end_of_life_decision_authority__autonomy_reading, base_extractiveness, 2010, 0.32).
narrative_ontology:measurement(end__be_t2024, end_of_life_decision_authority__autonomy_reading, base_extractiveness, 2024, 0.35).

% Suppression requirement over time
narrative_ontology:measurement(end__su_t1970, end_of_life_decision_authority__autonomy_reading, suppression_requirement, 1970, 0.8).
narrative_ontology:measurement(end__su_t1985, end_of_life_decision_authority__autonomy_reading, suppression_requirement, 1985, 0.6).
narrative_ontology:measurement(end__su_t2000, end_of_life_decision_authority__autonomy_reading, suppression_requirement, 2000, 0.4).
narrative_ontology:measurement(end__su_t2010, end_of_life_decision_authority__autonomy_reading, suppression_requirement, 2010, 0.3).
narrative_ontology:measurement(end__su_t2024, end_of_life_decision_authority__autonomy_reading, suppression_requirement, 2024, 0.2).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
