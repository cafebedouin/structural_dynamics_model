% ============================================================================
% CONSTRAINT STORY: dignified_death__sanctity_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_dignified_death__sanctity_primary, []).

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
 *   constraint_id: dignified_death__sanctity_primary
 *   human_readable: Dignity as Life's Intrinsic Value (Sanctity Primary Reading)
 *   domain: bioethics/medical_law/political_philosophy
 *
 * SUMMARY:
 *   This constraint, 'Dignity as Life's Intrinsic Value (Sanctity Primary
 *   Reading)', asserts that dignity resides in life's intrinsic value, making
 *   intentional life-termination a violation of transcendent moral law,
 *   regardless of consent. It is one reading of the broader 'dignified_death'
 *   kernel. While framed as a protection for vulnerable populations, its
 *   enforcement often results in the coercive prolongation of suffering for
 *   individuals seeking end-of-life options, leading to its classification as
 *   a Snare. The metrics reflect high extraction and suppression, as the
 *   constraint actively denies agency and imposes a specific moral framework.
 *
 * KEY AGENTS:
 *   - moral_order_community: Beneficiary (institutional/civilizational) — upholds transcendent moral law
 *   - vulnerable_patients: Payer (powerless/immediate) — bears prolonged suffering, denied agency
 *   - patients_seeking_euthanasia: Payer (powerless/immediate) — bears prolonged suffering, denied agency
 *   - healthcare_providers: Agenda-setter (institutional/biographical) — enforces non-termination
 *   - autonomy_advocates: Payer (organized/biographical) — resists denial of patient autonomy
 *   - legislators_judiciary: Agenda-setter (institutional/generational) — codifies/interprets the moral law
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dignified_death__sanctity_primary, 0.6).
domain_priors:suppression_score(dignified_death__sanctity_primary, 0.75).
domain_priors:theater_ratio(dignified_death__sanctity_primary, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dignified_death__sanctity_primary, extractiveness, 0.6).
narrative_ontology:constraint_metric(dignified_death__sanctity_primary, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(dignified_death__sanctity_primary, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(dignified_death__sanctity_primary, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(dignified_death__sanctity_primary, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dignified_death__sanctity_primary, snare).
narrative_ontology:human_readable(dignified_death__sanctity_primary, "Dignity as Life's Intrinsic Value (Sanctity Primary Reading)").
narrative_ontology:topic_domain(dignified_death__sanctity_primary, "bioethics/medical_law/political_philosophy").

domain_priors:requires_active_enforcement(dignified_death__sanctity_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dignified_death__sanctity_primary, '61e2db96-c742-4c12-8ff6-29889115c7dc').
narrative_ontology:cs_kernel_codification('61e2db96-c742-4c12-8ff6-29889115c7dc', formalized).
narrative_ontology:cs_authority_grounding('61e2db96-c742-4c12-8ff6-29889115c7dc', lineage).
narrative_ontology:cs_interpretation_layer_present('61e2db96-c742-4c12-8ff6-29889115c7dc').
narrative_ontology:cs_reading_relation('61e2db96-c742-4c12-8ff6-29889115c7dc', dignified_death__autonomy_primary, forecloses).
narrative_ontology:cs_reading_relation('61e2db96-c742-4c12-8ff6-29889115c7dc', dignified_death__relational_autonomy, forecloses).
narrative_ontology:cs_axiom('61e2db96-c742-4c12-8ff6-29889115c7dc', foundational, life_has_intrinsic_value).
narrative_ontology:cs_axiom_status(life_has_intrinsic_value, holdable).
narrative_ontology:cs_axiom_grounding('61e2db96-c742-4c12-8ff6-29889115c7dc', life_has_intrinsic_value, deontological).
narrative_ontology:cs_axiom('61e2db96-c742-4c12-8ff6-29889115c7dc', foundational, intentional_killing_is_wrong).
narrative_ontology:cs_axiom_status(intentional_killing_is_wrong, holdable).
narrative_ontology:cs_axiom_grounding('61e2db96-c742-4c12-8ff6-29889115c7dc', intentional_killing_is_wrong, deontological).
narrative_ontology:cs_reference_frame('61e2db96-c742-4c12-8ff6-29889115c7dc', intrinsic_value_of_life).
narrative_ontology:cs_drift_state('61e2db96-c742-4c12-8ff6-29889115c7dc', contemporary_secular_society, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('61e2db96-c742-4c12-8ff6-29889115c7dc', '').
narrative_ontology:cs_kernel_id(dignified_death__sanctity_primary, dignified_death).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dignified_death__sanctity_primary, moral_order_community).
narrative_ontology:constraint_victim(dignified_death__sanctity_primary, vulnerable_patients).
narrative_ontology:constraint_victim(dignified_death__sanctity_primary, patients_seeking_euthanasia).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(dignified_death__sanctity_primary, autonomy_advocates).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefits from the perceived upholding of a transcendent moral law, which asserts life's intrinsic value and prohibits intentional termination. This group sees itself as protecting fundamental human dignity.
narrative_ontology:constraint_stakeholder(dignified_death__sanctity_primary, moral_order_community, beneficiary,
    institutional, civilizational, analytical, universal).

% Are identified as potential victims of coercion or pressure to end their lives if intentional termination were permitted. However, they become victims of this constraint when it prolongs their suffering against their will, denying them agency over their death.
narrative_ontology:constraint_stakeholder(dignified_death__sanctity_primary, vulnerable_patients, payer,
    powerless, immediate, trapped, local).

% Are directly targeted by this constraint, as it denies them the option of intentional life-termination, even with consent, due to the overriding principle of life's intrinsic value. They bear the cost of prolonged suffering and loss of autonomy.
narrative_ontology:constraint_stakeholder(dignified_death__sanctity_primary, patients_seeking_euthanasia, payer,
    powerless, immediate, trapped, local).

% Are bound by the moral and legal frameworks derived from this principle, which prohibit them from assisting in intentional life-termination. They enforce the constraint through medical practice and ethical guidelines, often experiencing moral distress when patient suffering conflicts with the sanctity principle.
narrative_ontology:constraint_stakeholder(dignified_death__sanctity_primary, healthcare_providers, agenda_setter,
    institutional, biographical, constrained, national).

% Bear the cost of denied patient autonomy and actively resist this constraint through legal challenges, public education, and legislative lobbying. They argue that dignity includes the right to self-determination in end-of-life decisions.
narrative_ontology:constraint_stakeholder(dignified_death__sanctity_primary, autonomy_advocates, payer,
    organized, biographical, mobile, national).

% Codify and interpret the legal implications of this principle, often balancing it against other values like compassion and individual rights. Their decisions directly shape the enforceability and scope of the constraint.
narrative_ontology:constraint_stakeholder(dignified_death__sanctity_primary, legislators_judiciary, agenda_setter,
    institutional, generational, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(dignified_death__sanctity_primary, moral_order_community).
narrative_ontology:fixing_cost_class(dignified_death__sanctity_primary, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Aims to coordinate society around a shared understanding of life's intrinsic value, establishing a moral and legal boundary against intentional life-termination, thereby protecting vulnerable individuals from pressure to end their lives.
% TRANSFER_FUNCTION: Transfers the burden of prolonged suffering and loss of agency onto individuals (patients) and their families, while upholding a perceived moral order and protecting the community from the perceived harms of devaluing life.
% ABSENT_VOICES: Patients seeking autonomous end-of-life choices, and those who prioritize individual suffering and self-determination over abstract moral principles, are structurally excluded from the framing that defines dignity solely by life's intrinsic value.
% DISAPPEARANCE_RATIONALE: If this constraint vanished overnight, medical practice, legal frameworks, and social norms around death would fundamentally shift. The legal landscape for end-of-life care would be transformed, allowing for new forms of patient agency and potentially leading to widespread re-evaluation of medical ethics and societal responsibilities regarding death.
% FOUNDING_PROBLEM: To prevent the devaluation of human life, protect vulnerable individuals (elderly, disabled, poor) from coercion or pressure to end their lives, and uphold a transcendent moral order that views intentional life-termination as inherently wrong.
% FOUNDING_PROBLEM_CORROBORATION: Proponents (religious authorities, some bioethicists, conservative political groups) attest the problem of devaluing life and protecting the vulnerable is still live. Opponents (patient rights groups, some medical ethicists) argue the founding problem has largely shifted to one of denying patient autonomy and prolonging suffering, rather than preventing coercion.
narrative_ontology:disappearance_verdict(dignified_death__sanctity_primary, world_rearranges).
narrative_ontology:founding_problem_status(dignified_death__sanctity_primary, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(dignified_death__sanctity_primary, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(dignified_death__sanctity_primary, 'none', 1).
narrative_ontology:epsilon_provenance(dignified_death__sanctity_primary, 0.6, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(dignified_death__sanctity_primary_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(dignified_death__sanctity_primary, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(dignified_death__sanctity_primary_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.60) is high because the constraint imposes significant costs on individuals by denying them the choice of intentional life-termination, often leading to prolonged suffering. Suppression (0.75) is also high, as the constraint is actively enforced through legal prohibitions and medical ethical guidelines, effectively removing alternatives. The theater ratio (0.15) is low, indicating that the constraint's enforcement is direct and functional, not merely performative. Resistance (0.70) is substantial due to strong advocacy for patient autonomy and the right to self-determination in end-of-life decisions. Accessibility collapse (0.80) is very high, as the option of intentional life-termination is largely removed by this framework.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the 'moral_order_community', this constraint is a necessary protection of fundamental human dignity and a bulwark against societal moral decay. From the perspective of 'vulnerable_patients' and 'patients_seeking_euthanasia', it is a coercive mechanism that prolongs suffering and denies agency, effectively turning a protective norm into an extractive snare. The engine's classification as a Snare reflects the latter, highlighting the divergence from the claimed protective function.
 *
 * DIRECTIONALITY LOGIC:
 *   The 'moral_order_community' is the primary beneficiary, as the constraint upholds their perceived moral framework. 'Vulnerable_patients' and 'patients_seeking_euthanasia' are clear targets, bearing the direct costs of denied autonomy and prolonged suffering. 'Healthcare_providers' and 'legislators_judiciary' act as agenda-setters, enforcing the constraint, while 'autonomy_advocates' are payers who actively resist its imposition.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification as a Snare prevents mislabeling a coercive prolongation of suffering as a benign protection. While the founding problem aimed to protect the vulnerable, the constraint's current operation, particularly in contexts where individuals are competent and consenting, has shifted to an extractive function, denying agency under the guise of protection. The 'contested' status of the founding problem and the 'world_rearranges' disappearance verdict further support this, indicating that the constraint's persistence is not solely due to its original protective mandate.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    coercion_vs_protection_ambiguity,
    'To what extent does this constraint genuinely protect vulnerable populations from coercion, versus coercively prolonging suffering for competent individuals?',
    'Empirical studies on the incidence of coercion in jurisdictions with legalized end-of-life options, compared to the incidence of prolonged suffering in jurisdictions with strict prohibitions. Analysis of patient narratives and medical outcomes.',
    'If the primary effect is coercive prolongation of suffering for competent individuals, the Snare classification is strongly reinforced. If genuine protection of the vulnerable is demonstrably the dominant effect, the classification might shift towards a Tangled Rope or even Rope, depending on the balance of benefits and costs.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(coercion_vs_protection_ambiguity, empirical, 'Distinguishing between protective and coercive effects of the sanctity principle.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression primarily structural (legal prohibitions, medical guidelines) or internalized (moral/religious guilt, societal pressure to ''fight to the end'')?',
    'Post-legalization trajectory: if individuals continue to feel compelled to prolong life even after legal barriers are removed, it suggests a significant internalized component. Qualitative studies on patient and family decision-making processes.',
    'If internalized suppression is substantial, the constraint''s effective suppression is higher than the structural measure suggests, as individuals carry the suppression with them even in the absence of external barriers. This would amplify the effective extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism in end-of-life decisions.').

omega_variable(
    kernel_reading_divergence,
    'Given the ''dignified_death'' kernel, how do the ''sanctity_primary'', ''autonomy_primary'', and ''relational_autonomy'' readings structurally diverge in their victim sets, beneficiaries, and effective extraction?',
    'Comparative analysis of legal frameworks, medical practices, and patient outcomes in jurisdictions primarily guided by each reading. This story instantiates the ''sanctity_primary'' reading; separate stories for the other readings would provide the comparative data.',
    'The divergence in victim sets (e.g., ''sanctity_primary'' victims are those denied termination; ''autonomy_primary'' victims might be those denied assistance in living) and beneficiaries (moral order vs. individual agency) would confirm that these are distinct constraints, not merely different perspectives on the same one.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_divergence, conceptual, 'Structural differences between readings of the ''dignified_death'' kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dignified_death__sanctity_primary, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dign_tr_t0, dignified_death__sanctity_primary, theater_ratio, 0, 0.15).
narrative_ontology:measurement(dign_tr_t6, dignified_death__sanctity_primary, theater_ratio, 6, 0.15).
narrative_ontology:measurement(dign_tr_t12, dignified_death__sanctity_primary, theater_ratio, 12, 0.15).
narrative_ontology:measurement(dign_tr_t18, dignified_death__sanctity_primary, theater_ratio, 18, 0.15).
narrative_ontology:measurement(dign_tr_t24, dignified_death__sanctity_primary, theater_ratio, 24, 0.15).
narrative_ontology:measurement(dign_tr_t30, dignified_death__sanctity_primary, theater_ratio, 30, 0.15).

% Extraction over time
narrative_ontology:measurement(dign_be_t0, dignified_death__sanctity_primary, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(dign_be_t6, dignified_death__sanctity_primary, base_extractiveness, 6, 0.53).
narrative_ontology:measurement(dign_be_t12, dignified_death__sanctity_primary, base_extractiveness, 12, 0.56).
narrative_ontology:measurement(dign_be_t18, dignified_death__sanctity_primary, base_extractiveness, 18, 0.58).
narrative_ontology:measurement(dign_be_t24, dignified_death__sanctity_primary, base_extractiveness, 24, 0.59).
narrative_ontology:measurement(dign_be_t30, dignified_death__sanctity_primary, base_extractiveness, 30, 0.6).

% Suppression requirement over time
narrative_ontology:measurement(dign_su_t0, dignified_death__sanctity_primary, suppression_requirement, 0, 0.65).
narrative_ontology:measurement(dign_su_t6, dignified_death__sanctity_primary, suppression_requirement, 6, 0.68).
narrative_ontology:measurement(dign_su_t12, dignified_death__sanctity_primary, suppression_requirement, 12, 0.7).
narrative_ontology:measurement(dign_su_t18, dignified_death__sanctity_primary, suppression_requirement, 18, 0.72).
narrative_ontology:measurement(dign_su_t24, dignified_death__sanctity_primary, suppression_requirement, 24, 0.74).
narrative_ontology:measurement(dign_su_t30, dignified_death__sanctity_primary, suppression_requirement, 30, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dignified_death__sanctity_primary, identity_coordination).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'dignified_death' kernel. Other readings, such as 'autonomy_primary' and 'relational_autonomy', represent distinct constraints with different structural properties and classifications.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
