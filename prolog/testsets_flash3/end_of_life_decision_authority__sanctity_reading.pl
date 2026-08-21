% ============================================================================
% CONSTRAINT STORY: end_of_life_decision_authority__sanctity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
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
 *   constraint_id: end_of_life_decision_authority__sanctity_reading
 *   human_readable: Sanctity of Life Principle in End-of-Life Decisions
 *   domain: medical_ethics/bioethics/end_of_life_policy
 *
 * SUMMARY:
 *   This constraint represents the 'sanctity of life' reading of end-of-life
 *   decision authority, asserting that human life possesses intrinsic value
 *   independent of individual will, and intentional life-ending violates this
 *   value. It is one reading of the 'end_of_life_decision_authority' kernel,
 *   alongside the 'autonomy_reading' and 'vulnerability_protection_reading'.
 *   This reading places vulnerable patients who might be pressured into
 *   euthanasia into the victim set, and strictly defines the physician's role
 *   as a healer, not an agent of death. The individual's suffering is
 *   externalized, becoming a problem to be managed rather than a
 *   justification for ending life.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(end_of_life_decision_authority__sanctity_reading, 0.65).
domain_priors:suppression_score(end_of_life_decision_authority__sanctity_reading, 0.78).
domain_priors:theater_ratio(end_of_life_decision_authority__sanctity_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(end_of_life_decision_authority__sanctity_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(end_of_life_decision_authority__sanctity_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(end_of_life_decision_authority__sanctity_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(end_of_life_decision_authority__sanctity_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(end_of_life_decision_authority__sanctity_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(end_of_life_decision_authority__sanctity_reading, tangled_rope).
narrative_ontology:human_readable(end_of_life_decision_authority__sanctity_reading, "Sanctity of Life Principle in End-of-Life Decisions").
narrative_ontology:topic_domain(end_of_life_decision_authority__sanctity_reading, "medical_ethics/bioethics/end_of_life_policy").

domain_priors:requires_active_enforcement(end_of_life_decision_authority__sanctity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(end_of_life_decision_authority__sanctity_reading, '1defe180-9f15-499c-bb55-a11b89055cbe').
narrative_ontology:cs_kernel_codification('1defe180-9f15-499c-bb55-a11b89055cbe', formalized).
narrative_ontology:cs_authority_grounding('1defe180-9f15-499c-bb55-a11b89055cbe', lineage).
narrative_ontology:cs_interpretation_layer_present('1defe180-9f15-499c-bb55-a11b89055cbe').
narrative_ontology:cs_reading_relation('1defe180-9f15-499c-bb55-a11b89055cbe', end_of_life_decision_authority__autonomy_reading, coexists_with).
narrative_ontology:cs_reading_relation('1defe180-9f15-499c-bb55-a11b89055cbe', end_of_life_decision_authority__vulnerability_protection_reading, influences).
narrative_ontology:cs_axiom('1defe180-9f15-499c-bb55-a11b89055cbe', foundational, life_intrinsic_value_absolute).
narrative_ontology:cs_axiom_status(life_intrinsic_value_absolute, holdable).
narrative_ontology:cs_axiom_grounding('1defe180-9f15-499c-bb55-a11b89055cbe', life_intrinsic_value_absolute, deontological).
narrative_ontology:cs_axiom('1defe180-9f15-499c-bb55-a11b89055cbe', secondary, intentional_life_ending_unjustifiable).
narrative_ontology:cs_axiom_status(intentional_life_ending_unjustifiable, holdable).
narrative_ontology:cs_axiom_grounding('1defe180-9f15-499c-bb55-a11b89055cbe', intentional_life_ending_unjustifiable, deontological).
narrative_ontology:cs_reference_frame('1defe180-9f15-499c-bb55-a11b89055cbe', traditional_medical_ethics_life_preservation).
narrative_ontology:cs_drift_state('1defe180-9f15-499c-bb55-a11b89055cbe', contemporary_bioethics_debate, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('1defe180-9f15-499c-bb55-a11b89055cbe', '').
narrative_ontology:cs_kernel_id(end_of_life_decision_authority__sanctity_reading, end_of_life_decision_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(end_of_life_decision_authority__sanctity_reading, religious_institutions).
narrative_ontology:constraint_beneficiary(end_of_life_decision_authority__sanctity_reading, pro_life_advocates).
narrative_ontology:constraint_beneficiary(end_of_life_decision_authority__sanctity_reading, healthcare_systems_with_sanctity_policies).
narrative_ontology:constraint_victim(end_of_life_decision_authority__sanctity_reading, terminally_ill_patients_seeking_euthanasia).
narrative_ontology:constraint_victim(end_of_life_decision_authority__sanctity_reading, physicians_seeking_to_alleviate_suffering).
narrative_ontology:constraint_victim(end_of_life_decision_authority__sanctity_reading, pressured_vulnerable_patients).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Advocate for and enforce policies reflecting the intrinsic value of life, often viewing life as a divine gift. They shape public opinion and influence legislation, seeing their role as protecting a fundamental moral truth.
narrative_ontology:constraint_stakeholder(end_of_life_decision_authority__sanctity_reading, religious_institutions, agenda_setter,
    institutional, generational, identity_locked, global).

% Benefit from the legal and ethical framework that prohibits intentional life-ending, aligning with their moral convictions. They actively lobby for the maintenance and strengthening of these prohibitions.
narrative_ontology:constraint_stakeholder(end_of_life_decision_authority__sanctity_reading, pro_life_advocates, beneficiary,
    organized, biographical, constrained, national).

% Implement and enforce policies that prioritize the preservation of life, often due to religious affiliation or deeply held ethical stances. They direct medical practice to focus on palliative care rather than life-ending interventions.
narrative_ontology:constraint_stakeholder(end_of_life_decision_authority__sanctity_reading, healthcare_systems_with_sanctity_policies, agenda_setter,
    institutional, generational, constrained, national).

% Are denied the option of physician-assisted dying or euthanasia, even in cases of unbearable suffering, due to the legal and ethical prohibitions. Their autonomy over their own death is suppressed, leading to prolonged suffering.
narrative_ontology:constraint_stakeholder(end_of_life_decision_authority__sanctity_reading, terminally_ill_patients_seeking_euthanasia, payer,
    powerless, immediate, trapped, local).

% Are ethically and legally constrained from directly assisting in ending a patient's life, even when faced with profound suffering and patient requests. Their professional role is strictly defined as preserving life, not ending it.
narrative_ontology:constraint_stakeholder(end_of_life_decision_authority__sanctity_reading, physicians_seeking_to_alleviate_suffering, payer,
    moderate, biographical, constrained, local).

% Are implicitly protected from potential coercion or undue influence to end their lives, as the option is not available. However, they also bear the cost of not having the option of a dignified exit if their suffering becomes intolerable.
narrative_ontology:constraint_stakeholder(end_of_life_decision_authority__sanctity_reading, pressured_vulnerable_patients, payer,
    powerless, immediate, trapped, local).

% Argue for individual self-determination in end-of-life decisions, but their perspective is often marginalized or legally overridden by sanctity-of-life frameworks. They are excluded from the decision-making authority regarding the availability of euthanasia.
narrative_ontology:constraint_stakeholder(end_of_life_decision_authority__sanctity_reading, autonomy_advocates, excluded,
    organized, biographical, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a societal commitment to the preservation of human life, ensuring that all lives are treated as inherently valuable and protected from intentional termination, particularly for the vulnerable.
% TRANSFER_FUNCTION: Transfers the authority over the timing and manner of death from the individual and their medical providers to a collective moral and legal framework that prioritizes life preservation.
% ABSENT_VOICES: Advocates for individual autonomy and those experiencing intractable suffering are often excluded from the policy-making process, as their perspectives challenge the foundational premise of the sanctity of life. They would argue for the right to choose a dignified death.
% DISAPPEARANCE_RATIONALE: If the sanctity of life principle vanished overnight, the legal and ethical landscape around end-of-life care would fundamentally shift. Euthanasia and physician-assisted suicide would likely become legal, medical practice would adapt, and societal norms around death and dying would reorganize around individual choice, potentially leading to new forms of pressure on vulnerable populations.
% FOUNDING_PROBLEM: To prevent the arbitrary taking of human life and to protect vulnerable individuals from coercion or devaluation, establishing a universal moral baseline for the value of human existence.
% FOUNDING_PROBLEM_CORROBORATION: Religious texts and traditions, historical legal precedents, and contemporary bioethical arguments from various philosophical schools (outside the direct beneficiaries) corroborate the founding problem of protecting life's intrinsic value and preventing its arbitrary termination.
narrative_ontology:disappearance_verdict(end_of_life_decision_authority__sanctity_reading, world_rearranges).
narrative_ontology:founding_problem_status(end_of_life_decision_authority__sanctity_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(end_of_life_decision_authority__sanctity_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(end_of_life_decision_authority__sanctity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(end_of_life_decision_authority__sanctity_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(end_of_life_decision_authority__sanctity_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(end_of_life_decision_authority__sanctity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(end_of_life_decision_authority__sanctity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The constraint is classified as a Tangled Rope because it genuinely coordinates a societal value (protection of life, especially for the vulnerable) but also involves significant asymmetric extraction from individuals whose autonomy over their own death is denied. Extractiveness (0.65) is moderate-high, reflecting the profound cost to those denied choice. Suppression (0.78) is high due to legal prohibitions and strong moral/institutional resistance to alternatives. Theater ratio (0.20) is low, as the enforcement of this principle is generally sincere, though some arguments for 'protection' may mask a desire to maintain institutional authority.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of religious institutions and pro-life advocates, this is a necessary moral safeguard (a Rope or even a Mountain). From the perspective of terminally ill patients and physicians, it is a coercive Snare that prolongs suffering. The engine's classification as Tangled Rope reflects this hybrid nature, acknowledging both the coordination function and the asymmetric extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   Religious institutions and pro-life advocates are beneficiaries, as the constraint aligns with their core values and strengthens their influence. Healthcare systems with sanctity policies also benefit by having a clear, life-preserving mandate. Terminally ill patients seeking euthanasia and physicians seeking to alleviate suffering are targets, as their choices and professional duties are constrained. Pressured vulnerable patients are also victims, as they lose the option of a dignified exit, even if they are 'protected' from coercion. Autonomy advocates are excluded, as their voice is not central to this framework.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate (protecting life's intrinsic value) is still live and actively defended. The classification as Tangled Rope prevents mislabeling it as pure extraction by acknowledging the genuine coordination function of protecting vulnerable lives, while also highlighting the extractive costs to those denied end-of-life choices. It is not a Piton because it is actively enforced and benefits identifiable groups, nor a Snare because it does have a genuine, widely accepted coordination function.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    intrinsic_value_vs_suffering,
    'How does the intrinsic value of life weigh against the experience of intractable suffering, and is there a threshold where suffering overrides the imperative to preserve life?',
    'Philosophical consensus on the hierarchy of values, or legal precedent from jurisdictions that balance these claims through case law.',
    'If suffering is deemed to override intrinsic value in extreme cases, the constraint''s extractiveness would decrease for terminally ill patients, potentially reclassifying it towards a Rope or Scaffold for those specific cases. If intrinsic value is absolute, the current classification holds.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(intrinsic_value_vs_suffering, conceptual, 'The conceptual tension between life''s intrinsic value and individual suffering.').

omega_variable(
    coercion_vs_autonomy_protection,
    'To what extent does the prohibition on euthanasia genuinely protect vulnerable patients from coercion, versus denying autonomous choice to those who are not coerced?',
    'Empirical studies on the incidence of coercion in jurisdictions where euthanasia is legal, compared to the number of autonomous requests denied in jurisdictions where it is illegal.',
    'If coercion is rare and safeguards are effective, the ''protection'' aspect of the constraint is less salient, increasing its effective extractiveness for autonomous individuals. If coercion is a significant risk, the protective function is stronger, justifying some level of suppression.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coercion_vs_autonomy_protection, empirical, 'Balancing protection from coercion with the denial of autonomous choice.').

omega_variable(
    physician_role_definition,
    'Is the physician''s role fundamentally limited to healing and preserving life, or does it extend to alleviating suffering through all means, including assisting in a dignified death?',
    'Professional medical ethics bodies'' evolving guidelines, or legal rulings on the scope of medical practice in end-of-life care.',
    'If the role expands, physicians would move from ''payer'' to ''beneficiary'' (or at least less constrained), reducing the constraint''s effective extractiveness on them. If the role remains strictly life-preserving, the current classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(physician_role_definition, conceptual, 'The scope and definition of the physician''s role in end-of-life care.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(end_of_life_decision_authority__sanctity_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(end__tr_t0, end_of_life_decision_authority__sanctity_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(end__tr_t10, end_of_life_decision_authority__sanctity_reading, theater_ratio, 10, 0.17).
narrative_ontology:measurement(end__tr_t20, end_of_life_decision_authority__sanctity_reading, theater_ratio, 20, 0.19).
narrative_ontology:measurement(end__tr_t30, end_of_life_decision_authority__sanctity_reading, theater_ratio, 30, 0.2).
narrative_ontology:measurement(end__tr_t40, end_of_life_decision_authority__sanctity_reading, theater_ratio, 40, 0.21).
narrative_ontology:measurement(end__tr_t50, end_of_life_decision_authority__sanctity_reading, theater_ratio, 50, 0.2).

% Extraction over time
narrative_ontology:measurement(end__be_t0, end_of_life_decision_authority__sanctity_reading, base_extractiveness, 0, 0.6).
narrative_ontology:measurement(end__be_t10, end_of_life_decision_authority__sanctity_reading, base_extractiveness, 10, 0.62).
narrative_ontology:measurement(end__be_t20, end_of_life_decision_authority__sanctity_reading, base_extractiveness, 20, 0.65).
narrative_ontology:measurement(end__be_t30, end_of_life_decision_authority__sanctity_reading, base_extractiveness, 30, 0.64).
narrative_ontology:measurement(end__be_t40, end_of_life_decision_authority__sanctity_reading, base_extractiveness, 40, 0.66).
narrative_ontology:measurement(end__be_t50, end_of_life_decision_authority__sanctity_reading, base_extractiveness, 50, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(end__su_t0, end_of_life_decision_authority__sanctity_reading, suppression_requirement, 0, 0.75).
narrative_ontology:measurement(end__su_t10, end_of_life_decision_authority__sanctity_reading, suppression_requirement, 10, 0.76).
narrative_ontology:measurement(end__su_t20, end_of_life_decision_authority__sanctity_reading, suppression_requirement, 20, 0.78).
narrative_ontology:measurement(end__su_t30, end_of_life_decision_authority__sanctity_reading, suppression_requirement, 30, 0.77).
narrative_ontology:measurement(end__su_t40, end_of_life_decision_authority__sanctity_reading, suppression_requirement, 40, 0.79).
narrative_ontology:measurement(end__su_t50, end_of_life_decision_authority__sanctity_reading, suppression_requirement, 50, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(end_of_life_decision_authority__sanctity_reading, identity_coordination).
narrative_ontology:affects_constraint(end_of_life_decision_authority__sanctity_reading, end_of_life_decision_authority__autonomy_reading).
narrative_ontology:affects_constraint(end_of_life_decision_authority__sanctity_reading, end_of_life_decision_authority__vulnerability_protection_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'end_of_life_decision_authority' kernel. This 'sanctity_reading' emphasizes the intrinsic value of life, influencing and coexisting with the 'autonomy_reading' and 'vulnerability_protection_reading'.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
