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
 *   constraint_id: end_of_life_authority__autonomy_reading
 *   human_readable: Right to Control End-of-Life Decisions (Autonomy Reading)
 *   domain: medical_ethics/bioethics/end_of_life_policy
 *
 * SUMMARY:
 *   This constraint story instantiates the 'autonomy_reading' of the
 *   'end_of_life_authority' kernel. From this perspective, the constraint is
 *   the denial of an individual's right to control the circumstances and
 *   timing of their death when facing unbearable suffering. This reading
 *   views the current system as highly extractive and suppressive, forcing
 *   individuals to endure suffering against their will, despite the system's
 *   stated coordination functions around patient care and dignity. The
 *   metrics reflect the high cost borne by patients and families due to this
 *   denial of autonomy.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(end_of_life_authority__autonomy_reading, 0.85).
domain_priors:suppression_score(end_of_life_authority__autonomy_reading, 0.9).
domain_priors:theater_ratio(end_of_life_authority__autonomy_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(end_of_life_authority__autonomy_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(end_of_life_authority__autonomy_reading, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(end_of_life_authority__autonomy_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(end_of_life_authority__autonomy_reading, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(end_of_life_authority__autonomy_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(end_of_life_authority__autonomy_reading, tangled_rope).
narrative_ontology:human_readable(end_of_life_authority__autonomy_reading, "Right to Control End-of-Life Decisions (Autonomy Reading)").
narrative_ontology:topic_domain(end_of_life_authority__autonomy_reading, "medical_ethics/bioethics/end_of_life_policy").

domain_priors:requires_active_enforcement(end_of_life_authority__autonomy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(end_of_life_authority__autonomy_reading, 'ce706145-7c8f-4762-845c-d61651a26105').
narrative_ontology:cs_kernel_codification('ce706145-7c8f-4762-845c-d61651a26105', formalized).
narrative_ontology:cs_authority_grounding('ce706145-7c8f-4762-845c-d61651a26105', lineage).
narrative_ontology:cs_interpretation_layer_present('ce706145-7c8f-4762-845c-d61651a26105').
narrative_ontology:cs_reading_relation('ce706145-7c8f-4762-845c-d61651a26105', end_of_life_authority__sanctity_reading, forecloses).
narrative_ontology:cs_reading_relation('ce706145-7c8f-4762-845c-d61651a26105', end_of_life_authority__slippery_slope_mechanism, influences).
narrative_ontology:cs_axiom('ce706145-7c8f-4762-845c-d61651a26105', foundational, individual_self_determination_absolute).
narrative_ontology:cs_axiom_status(individual_self_determination_absolute, holdable).
narrative_ontology:cs_axiom_grounding('ce706145-7c8f-4762-845c-d61651a26105', individual_self_determination_absolute, deontological).
narrative_ontology:cs_axiom('ce706145-7c8f-4762-845c-d61651a26105', secondary, relief_of_unbearable_suffering_moral_imperative).
narrative_ontology:cs_axiom_status(relief_of_unbearable_suffering_moral_imperative, holdable).
narrative_ontology:cs_axiom_grounding('ce706145-7c8f-4762-845c-d61651a26105', relief_of_unbearable_suffering_moral_imperative, deontological).
narrative_ontology:cs_reference_frame('ce706145-7c8f-4762-845c-d61651a26105', patient_centered_care_framework).
narrative_ontology:cs_drift_state('ce706145-7c8f-4762-845c-d61651a26105', contemporary_bioethics_era, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('ce706145-7c8f-4762-845c-d61651a26105', '').
narrative_ontology:cs_kernel_id(end_of_life_authority__autonomy_reading, end_of_life_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(end_of_life_authority__autonomy_reading, medical_institutions).
narrative_ontology:constraint_beneficiary(end_of_life_authority__autonomy_reading, sanctity_of_life_advocates).
narrative_ontology:constraint_victim(end_of_life_authority__autonomy_reading, patients_denied_choice).
narrative_ontology:constraint_victim(end_of_life_authority__autonomy_reading, families_of_suffering_patients).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Individuals facing unbearable suffering who are denied the legal and medical means to control the timing and circumstances of their death. They bear the cost of prolonged suffering and loss of dignity.
narrative_ontology:constraint_stakeholder(end_of_life_authority__autonomy_reading, patients_denied_choice, payer,
    powerless, immediate, trapped, national).

% Bear the emotional, financial, and caregiving burdens associated with prolonged suffering of loved ones, often feeling helpless due to legal restrictions on end-of-life choices.
narrative_ontology:constraint_stakeholder(end_of_life_authority__autonomy_reading, families_of_suffering_patients, payer,
    powerless, biographical, constrained, national).

% Bound by legal and ethical frameworks that often restrict their ability to assist in end-of-life choices, even when aligned with patient wishes. They navigate complex moral dilemmas and legal risks.
narrative_ontology:constraint_stakeholder(end_of_life_authority__autonomy_reading, medical_professionals, agenda_setter,
    institutional, biographical, constrained, national).

% Benefit from maintaining control over end-of-life decisions, avoiding legal complexities and potential liabilities associated with expanded patient autonomy. They set policies and guidelines within existing legal frameworks.
narrative_ontology:constraint_stakeholder(end_of_life_authority__autonomy_reading, medical_institutions, beneficiary,
    institutional, generational, constrained, national).

% Their moral and ethical frameworks, which prioritize the intrinsic value of human life above individual choice in ending it, are upheld by the existing restrictions. They actively lobby against expanded end-of-life options.
narrative_ontology:constraint_stakeholder(end_of_life_authority__autonomy_reading, sanctity_of_life_advocates, beneficiary,
    organized, generational, mobile, global).

% Actively lobby for the expansion of patient rights to control end-of-life decisions, challenging existing legal and medical restrictions. They represent the voices of patients and families seeking choice.
narrative_ontology:constraint_stakeholder(end_of_life_authority__autonomy_reading, patient_autonomy_advocates, observer,
    organized, generational, mobile, national).

% Responsible for creating and amending laws governing end-of-life care. They respond to public pressure, ethical debates, and legal challenges, holding the power to alter the constraint's structure.
narrative_ontology:constraint_stakeholder(end_of_life_authority__autonomy_reading, legislators_policymakers, agenda_setter,
    institutional, generational, mobile, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(end_of_life_authority__autonomy_reading, diffuse).
narrative_ontology:fixing_cost_class(end_of_life_authority__autonomy_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To manage the complex ethical, medical, and legal aspects of end-of-life care, ensuring patient dignity, preventing abuse, and upholding societal values regarding life and death.
% TRANSFER_FUNCTION: Transfers ultimate control over the timing and circumstances of death from the individual to the collective (medical and legal authorities), prolonging suffering for those denied choice and shifting associated burdens to families and palliative care.
% ABSENT_VOICES: Patients who have died enduring unbearable suffering due to lack of legal options, and future patients who will face similar dilemmas. Their voices are represented by advocacy groups but are not directly present in policy debates.
% DISAPPEARANCE_RATIONALE: If the constraint (denial of individual autonomy in end-of-life decisions) vanished overnight, legal frameworks, medical protocols, and societal norms around death and dying would fundamentally reorganize to accommodate patient choice. This would lead to significant shifts in medical practice, palliative care, and public discourse.
% FOUNDING_PROBLEM: The founding problem was to prevent abuse of vulnerable individuals, protect the sanctity of life, and ensure that medical decisions are made in the patient's best interest, particularly when they are incapacitated or coerced.
% FOUNDING_PROBLEM_CORROBORATION: Patient testimonials, ethical review boards, and legal scholars corroborate the need for autonomy. Opponents (sanctity of life advocates) dispute this, citing concerns about vulnerability and the 'slippery slope' argument.
narrative_ontology:disappearance_verdict(end_of_life_authority__autonomy_reading, world_rearranges).
narrative_ontology:founding_problem_status(end_of_life_authority__autonomy_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(end_of_life_authority__autonomy_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(end_of_life_authority__autonomy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(end_of_life_authority__autonomy_reading, 0.85, 'gemini-2.5-flash', 'none', direct).

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
 *   Extractiveness is high (0.85) because the constraint directly imposes prolonged suffering and loss of dignity on individuals, which is a severe cost. Suppression is also very high (0.90) due to the robust legal and medical barriers preventing individuals from exercising this choice. Accessibility collapse is high (0.80) as legal alternatives are severely limited or non-existent in many jurisdictions. Resistance is substantial (0.75) from patient advocacy groups and a growing segment of the public. The theater ratio is low (0.10) because the debate is genuinely about fundamental ethical principles, not performative maintenance of an atrophied function. The decreasing extractiveness and suppression over time reflect the gradual, albeit slow, legal and societal shifts towards recognizing greater patient autonomy in some regions.
 *
 * PERSPECTIVAL GAP:
 *   The 'autonomy_reading' experiences this constraint as a 'tangled_rope' or 'snare' due to the high extraction of personal control and the suppression of choice. In contrast, the 'sanctity_reading' would likely perceive the same constraint as a 'rope' or even a 'mountain,' viewing it as a necessary protection of life and prevention of harm. The engine's computation of per-seat classifications will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   From the autonomy reading's perspective, patients denied choice and their families are the primary targets (payers), bearing the direct costs of prolonged suffering and emotional distress. Medical institutions and sanctity-of-life advocates are beneficiaries, as the current system upholds their authority and moral frameworks. Medical professionals and legislators act as agenda-setters, navigating and enforcing the existing constraints.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    autonomy_vs_sanctity_core_conflict,
    'Is the core conflict in end-of-life policy an irreducible tension between individual autonomy and the intrinsic value of human life, or can these principles be reconciled through careful legal and medical frameworks?',
    'Philosophical and legal analysis of frameworks that successfully integrate both principles, or empirical observation of jurisdictions where such integration has been attempted.',
    'If irreducible, the constraint will always be highly contested, with one principle necessarily suppressing the other. If reconcilable, the potential for a ''rope''-like coordination mechanism increases, reducing extraction and suppression.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(autonomy_vs_sanctity_core_conflict, conceptual, 'Irreducibility of the autonomy vs. sanctity conflict.').

omega_variable(
    slippery_slope_empirical_validity,
    'Is the ''slippery slope'' argument (that initial autonomy-based frameworks will inevitably expand to vulnerable or non-terminal populations) an empirically validated prediction or a speculative moral claim?',
    'Longitudinal empirical studies of jurisdictions that have legalized medical assistance in dying (MAID) to track changes in eligibility criteria and incidence of abuse over time.',
    'If empirically validated, the perceived risks of expanded autonomy increase, potentially justifying higher suppression. If empirically refuted, the justification for current restrictions weakens, supporting reduced suppression and extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(slippery_slope_empirical_validity, empirical, 'Empirical validity of the slippery slope argument.').

omega_variable(
    unbearable_suffering_definition_ambiguity,
    'How is ''unbearable suffering'' defined and measured in a way that respects individual subjective experience while preventing abuse or misapplication?',
    'Development of robust, intersubjectively verifiable criteria for assessing unbearable suffering, potentially involving multidisciplinary medical and psychological evaluations, alongside patient self-report.',
    'Clearer definitions could reduce ambiguity and increase access for legitimate cases, lowering extraction. Ambiguity, conversely, can be used to deny access, maintaining high extraction and suppression.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(unbearable_suffering_definition_ambiguity, conceptual, 'Ambiguity in defining unbearable suffering.').


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
narrative_ontology:measurement(end__tr_t2000, end_of_life_authority__autonomy_reading, theater_ratio, 2000, 0.09).
narrative_ontology:measurement(end__tr_t2010, end_of_life_authority__autonomy_reading, theater_ratio, 2010, 0.1).
narrative_ontology:measurement(end__tr_t2020, end_of_life_authority__autonomy_reading, theater_ratio, 2020, 0.1).
narrative_ontology:measurement(end__tr_t2025, end_of_life_authority__autonomy_reading, theater_ratio, 2025, 0.1).

% Extraction over time
narrative_ontology:measurement(end__be_t1970, end_of_life_authority__autonomy_reading, base_extractiveness, 1970, 0.95).
narrative_ontology:measurement(end__be_t1985, end_of_life_authority__autonomy_reading, base_extractiveness, 1985, 0.9).
narrative_ontology:measurement(end__be_t2000, end_of_life_authority__autonomy_reading, base_extractiveness, 2000, 0.88).
narrative_ontology:measurement(end__be_t2010, end_of_life_authority__autonomy_reading, base_extractiveness, 2010, 0.87).
narrative_ontology:measurement(end__be_t2020, end_of_life_authority__autonomy_reading, base_extractiveness, 2020, 0.86).
narrative_ontology:measurement(end__be_t2025, end_of_life_authority__autonomy_reading, base_extractiveness, 2025, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(end__su_t1970, end_of_life_authority__autonomy_reading, suppression_requirement, 1970, 0.98).
narrative_ontology:measurement(end__su_t1985, end_of_life_authority__autonomy_reading, suppression_requirement, 1985, 0.95).
narrative_ontology:measurement(end__su_t2000, end_of_life_authority__autonomy_reading, suppression_requirement, 2000, 0.93).
narrative_ontology:measurement(end__su_t2010, end_of_life_authority__autonomy_reading, suppression_requirement, 2010, 0.92).
narrative_ontology:measurement(end__su_t2020, end_of_life_authority__autonomy_reading, suppression_requirement, 2020, 0.91).
narrative_ontology:measurement(end__su_t2025, end_of_life_authority__autonomy_reading, suppression_requirement, 2025, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(end_of_life_authority__autonomy_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(end_of_life_authority__autonomy_reading, medical_professional_ethics).
narrative_ontology:affects_constraint(end_of_life_authority__autonomy_reading, palliative_care_funding).
narrative_ontology:affects_constraint(end_of_life_authority__autonomy_reading, public_health_policy).

% DUAL FORMULATION NOTE:
% This constraint is the 'autonomy_reading' of the 'end_of_life_authority' kernel. Sibling readings include 'sanctity_reading' and 'slippery_slope_mechanism', each representing a distinct structural constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
