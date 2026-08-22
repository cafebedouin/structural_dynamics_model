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
 *   human_readable: Right to Die based on Individual Autonomy
 *   domain: medical_ethics/bioethics/policy
 *
 * SUMMARY:
 *   This constraint represents the 'autonomy reading' of the end-of-life
 *   authority kernel. It asserts that individual autonomy is the primary
 *   ground for the right to control the circumstances and timing of one's
 *   death, especially when facing unbearable suffering. This reading actively
 *   suppresses paternalistic restrictions and expands eligibility criteria
 *   over time, leading to a growing set of 'victims' (patients denied choice)
 *   under alternative readings. The constraint is claimed as a Rope by its
 *   proponents, but its metrics reflect a Tangled Rope due to the active
 *   enforcement required to overcome resistance from traditional medical
 *   ethics and sanctity-of-life perspectives, and the extraction from those
 *   denied choice.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(end_of_life_authority__autonomy_reading, 0.65).
domain_priors:suppression_score(end_of_life_authority__autonomy_reading, 0.7).
domain_priors:theater_ratio(end_of_life_authority__autonomy_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(end_of_life_authority__autonomy_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(end_of_life_authority__autonomy_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(end_of_life_authority__autonomy_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(end_of_life_authority__autonomy_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(end_of_life_authority__autonomy_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(end_of_life_authority__autonomy_reading, tangled_rope).
narrative_ontology:human_readable(end_of_life_authority__autonomy_reading, "Right to Die based on Individual Autonomy").
narrative_ontology:topic_domain(end_of_life_authority__autonomy_reading, "medical_ethics/bioethics/policy").

domain_priors:requires_active_enforcement(end_of_life_authority__autonomy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(end_of_life_authority__autonomy_reading, 'fba8ec62-7fed-402c-b858-e671a0c83a41').
narrative_ontology:cs_kernel_codification('fba8ec62-7fed-402c-b858-e671a0c83a41', formalized).
narrative_ontology:cs_authority_grounding('fba8ec62-7fed-402c-b858-e671a0c83a41', lineage).
narrative_ontology:cs_interpretation_layer_present('fba8ec62-7fed-402c-b858-e671a0c83a41').
narrative_ontology:cs_reading_relation('fba8ec62-7fed-402c-b858-e671a0c83a41', end_of_life_authority__sanctity_reading, coexists_with).
narrative_ontology:cs_reading_relation('fba8ec62-7fed-402c-b858-e671a0c83a41', end_of_life_authority__slippery_slope_mechanism, influences).
narrative_ontology:cs_axiom('fba8ec62-7fed-402c-b858-e671a0c83a41', foundational, individual_self_determination_is_paramount).
narrative_ontology:cs_axiom_status(individual_self_determination_is_paramount, holdable).
narrative_ontology:cs_axiom_grounding('fba8ec62-7fed-402c-b858-e671a0c83a41', individual_self_determination_is_paramount, deontological).
narrative_ontology:cs_axiom('fba8ec62-7fed-402c-b858-e671a0c83a41', foundational, relief_of_unbearable_suffering_is_a_moral_imperative).
narrative_ontology:cs_axiom_status(relief_of_unbearable_suffering_is_a_moral_imperative, holdable).
narrative_ontology:cs_axiom_grounding('fba8ec62-7fed-402c-b858-e671a0c83a41', relief_of_unbearable_suffering_is_a_moral_imperative, deontological).
narrative_ontology:cs_reference_frame('fba8ec62-7fed-402c-b858-e671a0c83a41', patient_centered_autonomy_framework).
narrative_ontology:cs_drift_state('fba8ec62-7fed-402c-b858-e671a0c83a41', contemporary_human_rights_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('fba8ec62-7fed-402c-b858-e671a0c83a41', '').
narrative_ontology:cs_kernel_id(end_of_life_authority__autonomy_reading, end_of_life_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(end_of_life_authority__autonomy_reading, patients_seeking_euthanasia).
narrative_ontology:constraint_beneficiary(end_of_life_authority__autonomy_reading, advocacy_groups_for_autonomy).
narrative_ontology:constraint_victim(end_of_life_authority__autonomy_reading, patients_denied_choice).
narrative_ontology:constraint_victim(end_of_life_authority__autonomy_reading, paternalistic_medical_institutions).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Individuals facing unbearable suffering who wish to exercise control over their death. They benefit directly from the legal and medical framework that permits this choice, but are trapped by their medical condition.
narrative_ontology:constraint_stakeholder(end_of_life_authority__autonomy_reading, patients_seeking_euthanasia, beneficiary,
    powerless, immediate, trapped, local).

% Organizations that champion individual rights and self-determination in end-of-life decisions. They actively lobby for and defend policies that expand access to euthanasia and assisted dying, shaping the legal landscape.
narrative_ontology:constraint_stakeholder(end_of_life_authority__autonomy_reading, advocacy_groups_for_autonomy, agenda_setter,
    organized, generational, mobile, national).

% Individuals who meet the criteria for unbearable suffering but are denied the option of euthanasia due to specific legal restrictions or institutional policies. They bear the cost of prolonged suffering against their will.
narrative_ontology:constraint_stakeholder(end_of_life_authority__autonomy_reading, patients_denied_choice, payer,
    powerless, immediate, trapped, local).

% Healthcare providers and systems that prioritize the preservation of life and may resist or refuse to participate in end-of-life procedures, often citing ethical or religious objections. They bear the cost of adapting to legal mandates that conflict with their traditional ethos.
narrative_ontology:constraint_stakeholder(end_of_life_authority__autonomy_reading, paternalistic_medical_institutions, payer,
    institutional, biographical, constrained, national).

% Groups who believe in the intrinsic value of human life and oppose intentional life-ending. While they actively resist the expansion of autonomy-based end-of-life policies, their arguments are often marginalized in the legal frameworks driven by autonomy.
narrative_ontology:constraint_stakeholder(end_of_life_authority__autonomy_reading, sanctity_of_life_advocates, excluded,
    organized, generational, constrained, national).

% The governmental bodies responsible for enacting and interpreting laws related to end-of-life care. They mediate between competing ethical frameworks and are subject to political and social pressures.
narrative_ontology:constraint_stakeholder(end_of_life_authority__autonomy_reading, legislators_and_judiciary, agenda_setter,
    institutional, generational, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a legal and medical framework for individuals to exercise self-determination over their death when facing unbearable suffering, coordinating medical practice with patient wishes.
% TRANSFER_FUNCTION: Transfers the authority over the timing and circumstances of death from medical professionals or state to the individual patient, in cases of unbearable suffering.
% ABSENT_VOICES: Advocates for the sanctity of life are often excluded from the core policy-making discussions once the autonomy principle is established, as their fundamental premise is seen as conflicting with the foundational right being asserted.
% DISAPPEARANCE_RATIONALE: If the legal recognition of individual autonomy in end-of-life decisions vanished, patients would lose a fundamental right, medical practice would revert to more paternalistic models, and the suffering of those denied choice would be prolonged. The entire framework of patient rights in this domain would need to be re-established.
% FOUNDING_PROBLEM: Patients facing terminal illness and unbearable suffering were historically denied agency over their final moments, leading to prolonged distress and loss of dignity.
% FOUNDING_PROBLEM_CORROBORATION: Patient testimonials, medical ethics literature, and international human rights reports consistently corroborate the ongoing problem of prolonged suffering and the desire for autonomous end-of-life choices. Legal challenges and legislative debates continue to attest to its live status.
narrative_ontology:disappearance_verdict(end_of_life_authority__autonomy_reading, world_rearranges).
narrative_ontology:founding_problem_status(end_of_life_authority__autonomy_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(end_of_life_authority__autonomy_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_gemini+stakeholder_backfill', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(end_of_life_authority__autonomy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(end_of_life_authority__autonomy_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

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
 *   The extractiveness (0.65) reflects the significant cost borne by medical institutions and professionals who must adapt to or participate in practices that may conflict with their traditional values, as well as the 'cost' of prolonged suffering for those who are denied choice under this framework's specific criteria. Suppression (0.70) is high because the constraint requires active legal and institutional enforcement to overcome resistance from sanctity-of-life advocates and paternalistic medical practices. The low theater ratio (0.10) indicates that the constraint's function is largely genuine, though some performative aspects may exist in the ongoing debates. The increasing extractiveness and suppression over time reflect the ongoing contestation and the need for active defense and expansion of this right against persistent opposition.
 *
 * PERSPECTIVAL GAP:
 *   Proponents of the autonomy reading perceive this as a fundamental right (Rope), while those who are compelled to participate against their ethical objections, or those who are denied choice by the specific criteria, experience it as an extractive imposition (Snare/Tangled Rope). The engine's classification as Tangled Rope captures this hybrid nature, where a genuine coordination function (patient autonomy) is intertwined with asymmetric extraction and active enforcement.
 *
 * DIRECTIONALITY LOGIC:
 *   Patients seeking euthanasia and autonomy advocacy groups are clear beneficiaries, as the constraint directly enables their desired outcomes. Patients denied choice are victims, as their suffering is prolonged by the constraint's specific boundaries. Paternalistic medical institutions are also victims, as they are compelled to act against their traditional ethos. Legislators and the judiciary act as agenda-setters, mediating the implementation and evolution of this constraint. Sanctity-of-life advocates are excluded, as their core premise is fundamentally at odds with the autonomy reading.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    scope_of_unbearable_suffering,
    'How is ''unbearable suffering'' defined, and does this definition remain stable or expand over time?',
    'Longitudinal study of legal precedents and medical guidelines across jurisdictions implementing autonomy-based end-of-life policies.',
    'If the definition expands beyond terminal physical suffering to include psychological or existential suffering, the victim set (patients denied choice) would shift, and the effective extractiveness on medical institutions might increase due to broader application. This would push the constraint closer to a Snare.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(scope_of_unbearable_suffering, empirical, 'Ambiguity in the definition of ''unbearable suffering'' and its potential for expansion.').

omega_variable(
    autonomy_vs_sanctity_grounding,
    'Is individual autonomy a more fundamental ethical principle than the sanctity of life, or are they incommensurable?',
    'Philosophical and legal analysis of foundational ethical frameworks; societal consensus shifts over generations.',
    'If sanctity of life is deemed equally or more fundamental, the legitimacy of the autonomy reading is challenged, potentially reducing its suppressive force and increasing resistance. This would shift the constraint towards a more contested Tangled Rope or even a Snare from the perspective of sanctity-of-life advocates.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(autonomy_vs_sanctity_grounding, conceptual, 'The foundational conceptual conflict between autonomy and sanctity of life.').

omega_variable(
    slippery_slope_empirical_validity,
    'Does the empirical evidence support the ''slippery slope'' argument that initial autonomy-based frameworks inevitably expand to include incompetent or non-terminal populations?',
    'Comparative empirical studies of jurisdictions with varying lengths of experience with autonomy-based end-of-life policies.',
    'If the slippery slope is empirically validated, the long-term extractiveness and suppression of the autonomy reading would be significantly higher, as it would encompass a much broader victim set. This would strengthen the case for reclassifying it as a Snare over time.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(slippery_slope_empirical_validity, empirical, 'The empirical validity of the ''slippery slope'' argument against autonomy-based end-of-life policies.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(end_of_life_authority__autonomy_reading, 1990, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(end__tr_t1990, end_of_life_authority__autonomy_reading, theater_ratio, 1990, 0.05).
narrative_ontology:measurement(end__tr_t2000, end_of_life_authority__autonomy_reading, theater_ratio, 2000, 0.08).
narrative_ontology:measurement(end__tr_t2010, end_of_life_authority__autonomy_reading, theater_ratio, 2010, 0.09).
narrative_ontology:measurement(end__tr_t2024, end_of_life_authority__autonomy_reading, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(end__be_t1990, end_of_life_authority__autonomy_reading, base_extractiveness, 1990, 0.4).
narrative_ontology:measurement(end__be_t2000, end_of_life_authority__autonomy_reading, base_extractiveness, 2000, 0.5).
narrative_ontology:measurement(end__be_t2010, end_of_life_authority__autonomy_reading, base_extractiveness, 2010, 0.58).
narrative_ontology:measurement(end__be_t2024, end_of_life_authority__autonomy_reading, base_extractiveness, 2024, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(end__su_t1990, end_of_life_authority__autonomy_reading, suppression_requirement, 1990, 0.45).
narrative_ontology:measurement(end__su_t2000, end_of_life_authority__autonomy_reading, suppression_requirement, 2000, 0.55).
narrative_ontology:measurement(end__su_t2010, end_of_life_authority__autonomy_reading, suppression_requirement, 2010, 0.63).
narrative_ontology:measurement(end__su_t2024, end_of_life_authority__autonomy_reading, suppression_requirement, 2024, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(end_of_life_authority__autonomy_reading, identity_coordination).
narrative_ontology:affects_constraint(end_of_life_authority__autonomy_reading, end_of_life_authority__sanctity_reading).
narrative_ontology:affects_constraint(end_of_life_authority__autonomy_reading, end_of_life_authority__slippery_slope_mechanism).

% DUAL FORMULATION NOTE:
% This constraint is the 'autonomy_reading' of the 'end_of_life_authority' kernel. It is structurally distinct from the 'sanctity_reading' and 'slippery_slope_mechanism' due to differing foundational axioms and victim sets, but all three are linked as components of the broader end-of-life authority debate.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
