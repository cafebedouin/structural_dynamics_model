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
 *   human_readable: End-of-Life Authority: Autonomy Reading
 *   domain: medical_ethics/bioethics/end_of_life_policy
 *
 * SUMMARY:
 *   This constraint represents the 'autonomy reading' of the end-of-life
 *   authority kernel. It asserts that individual autonomy is the primary
 *   ground for the right to control the circumstances and timing of one's
 *   death when facing unbearable suffering. This reading actively suppresses
 *   paternalistic restrictions and expands the victim set to include patients
 *   denied choice. The metrics reflect a system that, while providing a
 *   coordination function for those seeking aid-in-dying, still extracts from
 *   those denied choice and requires active suppression of opposing views.
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
narrative_ontology:human_readable(end_of_life_authority__autonomy_reading, "End-of-Life Authority: Autonomy Reading").
narrative_ontology:topic_domain(end_of_life_authority__autonomy_reading, "medical_ethics/bioethics/end_of_life_policy").

domain_priors:requires_active_enforcement(end_of_life_authority__autonomy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(end_of_life_authority__autonomy_reading, 'dc03c9ac-001a-44ca-b9f0-dc797f59cb51').
narrative_ontology:cs_kernel_codification('dc03c9ac-001a-44ca-b9f0-dc797f59cb51', formalized).
narrative_ontology:cs_authority_grounding('dc03c9ac-001a-44ca-b9f0-dc797f59cb51', lineage).
narrative_ontology:cs_interpretation_layer_present('dc03c9ac-001a-44ca-b9f0-dc797f59cb51').
narrative_ontology:cs_reading_relation('dc03c9ac-001a-44ca-b9f0-dc797f59cb51', end_of_life_authority__sanctity_reading, coexists_with).
narrative_ontology:cs_reading_relation('dc03c9ac-001a-44ca-b9f0-dc797f59cb51', end_of_life_authority__slippery_slope_mechanism, influences).
narrative_ontology:cs_axiom('dc03c9ac-001a-44ca-b9f0-dc797f59cb51', foundational, individual_self_determination_is_paramount).
narrative_ontology:cs_axiom_status(individual_self_determination_is_paramount, holdable).
narrative_ontology:cs_axiom_grounding('dc03c9ac-001a-44ca-b9f0-dc797f59cb51', individual_self_determination_is_paramount, deontological).
narrative_ontology:cs_axiom('dc03c9ac-001a-44ca-b9f0-dc797f59cb51', foundational, relief_of_unbearable_suffering_is_a_moral_imperative).
narrative_ontology:cs_axiom_status(relief_of_unbearable_suffering_is_a_moral_imperative, holdable).
narrative_ontology:cs_axiom_grounding('dc03c9ac-001a-44ca-b9f0-dc797f59cb51', relief_of_unbearable_suffering_is_a_moral_imperative, deontological).
narrative_ontology:cs_reference_frame('dc03c9ac-001a-44ca-b9f0-dc797f59cb51', enlightenment_autonomy_principle).
narrative_ontology:cs_drift_state('dc03c9ac-001a-44ca-b9f0-dc797f59cb51', contemporary_medical_practice, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('dc03c9ac-001a-44ca-b9f0-dc797f59cb51', '').
narrative_ontology:cs_kernel_id(end_of_life_authority__autonomy_reading, end_of_life_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(end_of_life_authority__autonomy_reading, patients_seeking_aid_in_dying).
narrative_ontology:constraint_beneficiary(end_of_life_authority__autonomy_reading, advocacy_organizations_for_autonomy).
narrative_ontology:constraint_victim(end_of_life_authority__autonomy_reading, patients_denied_choice_by_paternalistic_restrictions).
narrative_ontology:constraint_victim(end_of_life_authority__autonomy_reading, medical_professionals_constrained_by_paternalistic_laws).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(end_of_life_authority__autonomy_reading, medical_professionals_supporting_autonomy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Individuals facing unbearable suffering who wish to exercise control over the timing and circumstances of their death. This reading grants them the right to make this choice, transforming their situation from one of forced suffering to one of agency.
narrative_ontology:constraint_stakeholder(end_of_life_authority__autonomy_reading, patients_seeking_aid_in_dying, beneficiary,
    powerless, immediate, trapped, local).

% Groups that champion individual rights and self-determination in end-of-life decisions. They actively lobby for legislation and policy changes that align with the autonomy reading, benefiting from the expansion of patient choice.
narrative_ontology:constraint_stakeholder(end_of_life_authority__autonomy_reading, advocacy_organizations_for_autonomy, agenda_setter,
    organized, generational, mobile, national).

% Physicians and other healthcare providers who believe in patient autonomy and are willing to provide aid-in-dying services within legal frameworks. This reading aligns with their ethical stance and allows them to act in accordance with patient wishes.
narrative_ontology:constraint_stakeholder(end_of_life_authority__autonomy_reading, medical_professionals_supporting_autonomy, beneficiary,
    moderate, biographical, constrained, local).

% Individuals whose suffering is prolonged against their will due to legal or institutional restrictions that prioritize the 'sanctity of life' over individual choice. They bear the cost of continued suffering and loss of agency.
narrative_ontology:constraint_stakeholder(end_of_life_authority__autonomy_reading, patients_denied_choice_by_paternalistic_restrictions, payer,
    powerless, immediate, trapped, local).

% Healthcare providers who, despite supporting patient autonomy, are legally prohibited from assisting in end-of-life decisions due to prevailing paternalistic laws. They face ethical dilemmas and professional constraints.
narrative_ontology:constraint_stakeholder(end_of_life_authority__autonomy_reading, medical_professionals_constrained_by_paternalistic_laws, payer,
    moderate, biographical, constrained, local).

% Organizations that oppose aid-in-dying on moral or religious grounds, advocating for the sanctity of life. While they are active in the broader debate, this specific reading of the kernel excludes their core premise from the framework of legitimate action.
narrative_ontology:constraint_stakeholder(end_of_life_authority__autonomy_reading, religious_institutions_and_pro_life_groups, excluded,
    organized, generational, constrained, national).

% Academics who analyze the legal, ethical, and societal implications of end-of-life policies. They contribute to the discourse but are not directly subject to the constraint's enforcement or beneficiaries.
narrative_ontology:constraint_stakeholder(end_of_life_authority__autonomy_reading, legal_scholars_and_ethicists, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the actions of patients, medical professionals, and legal systems to ensure that an individual's autonomous choice regarding their death, when facing unbearable suffering, is respected and facilitated within a regulated framework.
% TRANSFER_FUNCTION: Transfers the ultimate authority over end-of-life decisions from external moral or legal bodies to the suffering individual, enabling them to choose the timing and manner of their death.
% ABSENT_VOICES: The voices of those who believe in the absolute sanctity of life, often represented by religious institutions and pro-life groups, are structurally excluded from the core premise of this reading. They would argue that no individual has the right to intentionally end a life, regardless of suffering.
% DISAPPEARANCE_RATIONALE: If the autonomy reading vanished, patients facing unbearable suffering would lose their legal and ethical grounds for seeking aid-in-dying, forcing them to endure prolonged suffering. Medical professionals would revert to purely palliative care, and advocacy efforts would shift back to challenging paternalistic restrictions, fundamentally altering end-of-life care and policy.
% FOUNDING_PROBLEM: Individuals facing unbearable suffering at the end of life were denied agency over their own deaths, leading to prolonged agony and loss of dignity due to legal and medical paternalism.
% FOUNDING_PROBLEM_CORROBORATION: Patient testimonials, surveys of public opinion, and reports from medical ethicists consistently corroborate that the problem of prolonged suffering and lack of end-of-life autonomy remains a live issue, despite legal advancements in some jurisdictions. This corroboration comes from outside the direct beneficiaries of aid-in-dying laws.
narrative_ontology:disappearance_verdict(end_of_life_authority__autonomy_reading, world_rearranges).
narrative_ontology:founding_problem_status(end_of_life_authority__autonomy_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(end_of_life_authority__autonomy_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
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
 *   The extractiveness (0.65) reflects the cost borne by patients whose suffering is prolonged due to the absence or restriction of this right, and the ethical burden on medical professionals. Suppression (0.70) is high because this reading requires active legal and ethical enforcement to overcome deeply entrenched 'sanctity of life' doctrines and paternalistic medical traditions. Theater ratio is low (0.10) as the function is direct and not performative; the constraint genuinely aims to enable autonomous choice. Accessibility collapse is moderate (0.40) as alternatives (like palliative care) exist, but they do not address the core desire for control over death. Resistance (0.55) is significant from groups opposing aid-in-dying.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of patients seeking aid-in-dying, this constraint is a 'rope' or 'scaffold' providing essential support and agency. From the perspective of those denied choice, it is a 'snare' of prolonged suffering. The engine's classification will reflect this divergence based on the declared beneficiary/victim structure and exit options.
 *
 * DIRECTIONALITY LOGIC:
 *   Patients seeking aid-in-dying and autonomy advocacy groups are clear beneficiaries (low d). Medical professionals who support autonomy also benefit by being able to act ethically. Conversely, patients denied choice and medical professionals constrained by paternalistic laws are victims (high d). Religious institutions and pro-life groups are excluded, as their core tenets are incompatible with this reading's foundational premise.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    scope_of_unbearable_suffering,
    'How is ''unbearable suffering'' defined, and is this definition consistently applied across cases and jurisdictions?',
    'Standardized medical and psychological assessment protocols, coupled with legal precedent and inter-jurisdictional comparative analysis.',
    'A narrow, consistently applied definition would limit the scope of the constraint, potentially reducing perceived extractiveness from those who fear its expansion. A broad or inconsistently applied definition could increase extractiveness from those who believe it oversteps its bounds, and amplify ''slippery slope'' concerns.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(scope_of_unbearable_suffering, empirical, 'Ambiguity in the definition and application of ''unbearable suffering''.').

omega_variable(
    autonomy_vs_sanctity_of_life,
    'Is individual autonomy fundamentally incommensurable with the sanctity of life principle, or can a framework reconcile both?',
    'Philosophical and legal discourse exploring hybrid frameworks, or empirical observation of jurisdictions attempting to integrate both principles.',
    'If incommensurable, the ''autonomy reading'' and ''sanctity reading'' will remain in direct conflict, leading to persistent political and ethical struggle. If reconcilable, a new, less extractive constraint could emerge that balances both values.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(autonomy_vs_sanctity_of_life, conceptual, 'The fundamental conceptual conflict between autonomy and sanctity of life in end-of-life decisions.').

omega_variable(
    slippery_slope_empirical_validity,
    'Does the expansion of aid-in-dying criteria, as predicted by the ''slippery_slope_mechanism'' reading, empirically occur in jurisdictions that adopt the autonomy reading?',
    'Longitudinal empirical studies tracking the evolution of eligibility criteria and actual practice in jurisdictions with aid-in-dying laws over several decades.',
    'Empirical validation of the slippery slope would increase perceived extractiveness and suppression for those who fear its consequences, potentially leading to calls for stricter regulation or repeal. Empirical refutation would strengthen the autonomy reading''s legitimacy.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(slippery_slope_empirical_validity, empirical, 'The empirical validity of the ''slippery slope'' argument against expanding end-of-life autonomy.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(end_of_life_authority__autonomy_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(end__tr_t0, end_of_life_authority__autonomy_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(end__tr_t5, end_of_life_authority__autonomy_reading, theater_ratio, 5, 0.1).
narrative_ontology:measurement(end__tr_t10, end_of_life_authority__autonomy_reading, theater_ratio, 10, 0.1).
narrative_ontology:measurement(end__tr_t15, end_of_life_authority__autonomy_reading, theater_ratio, 15, 0.1).
narrative_ontology:measurement(end__tr_t20, end_of_life_authority__autonomy_reading, theater_ratio, 20, 0.1).

% Extraction over time
narrative_ontology:measurement(end__be_t0, end_of_life_authority__autonomy_reading, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(end__be_t5, end_of_life_authority__autonomy_reading, base_extractiveness, 5, 0.55).
narrative_ontology:measurement(end__be_t10, end_of_life_authority__autonomy_reading, base_extractiveness, 10, 0.6).
narrative_ontology:measurement(end__be_t15, end_of_life_authority__autonomy_reading, base_extractiveness, 15, 0.63).
narrative_ontology:measurement(end__be_t20, end_of_life_authority__autonomy_reading, base_extractiveness, 20, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(end__su_t0, end_of_life_authority__autonomy_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(end__su_t5, end_of_life_authority__autonomy_reading, suppression_requirement, 5, 0.6).
narrative_ontology:measurement(end__su_t10, end_of_life_authority__autonomy_reading, suppression_requirement, 10, 0.65).
narrative_ontology:measurement(end__su_t15, end_of_life_authority__autonomy_reading, suppression_requirement, 15, 0.68).
narrative_ontology:measurement(end__su_t20, end_of_life_authority__autonomy_reading, suppression_requirement, 20, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
