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
    narrative_ontology:constraint_vindicates/2,
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
 *   constraint_id: dignified_death__sanctity_primary
 *   human_readable: Dignity as Sanctity of Life (Sanctity-Primary Reading)
 *   domain: bioethics/medical_law/political_philosophy
 *
 * SUMMARY:
 *   This constraint, 'Dignity as Sanctity of Life (Sanctity-Primary
 *   Reading)', is one reading of the broader 'dignified_death' kernel. It
 *   asserts that dignity resides in life's intrinsic value, making
 *   intentional life-termination a violation of transcendent moral law,
 *   regardless of consent. This reading is instantiated as a Snare because
 *   while it claims to protect vulnerable life, its practical effect is to
 *   coercively prolong suffering for individuals who desire an end to life,
 *   benefiting moral order advocates and religious institutions at the
 *   expense of terminally ill, elderly, and disabled persons. The metrics
 *   reflect high extraction and suppression, as the constraint requires
 *   active enforcement to deny individual autonomy.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dignified_death__sanctity_primary, 0.6).
domain_priors:suppression_score(dignified_death__sanctity_primary, 0.7).
domain_priors:theater_ratio(dignified_death__sanctity_primary, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dignified_death__sanctity_primary, extractiveness, 0.6).
narrative_ontology:constraint_metric(dignified_death__sanctity_primary, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(dignified_death__sanctity_primary, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(dignified_death__sanctity_primary, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(dignified_death__sanctity_primary, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dignified_death__sanctity_primary, snare).
narrative_ontology:human_readable(dignified_death__sanctity_primary, "Dignity as Sanctity of Life (Sanctity-Primary Reading)").
narrative_ontology:topic_domain(dignified_death__sanctity_primary, "bioethics/medical_law/political_philosophy").

domain_priors:requires_active_enforcement(dignified_death__sanctity_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dignified_death__sanctity_primary, '2d8df589-a52e-4196-90c1-b16114f38eb1').
narrative_ontology:cs_kernel_codification('2d8df589-a52e-4196-90c1-b16114f38eb1', formalized).
narrative_ontology:cs_authority_grounding('2d8df589-a52e-4196-90c1-b16114f38eb1', lineage).
narrative_ontology:cs_interpretation_layer_present('2d8df589-a52e-4196-90c1-b16114f38eb1').
narrative_ontology:cs_reading_relation('2d8df589-a52e-4196-90c1-b16114f38eb1', dignified_death__autonomy_primary, coexists_with).
narrative_ontology:cs_reading_relation('2d8df589-a52e-4196-90c1-b16114f38eb1', dignified_death__relational_autonomy, coexists_with).
narrative_ontology:cs_axiom('2d8df589-a52e-4196-90c1-b16114f38eb1', foundational, life_has_intrinsic_value).
narrative_ontology:cs_axiom_status(life_has_intrinsic_value, holdable).
narrative_ontology:cs_axiom_grounding('2d8df589-a52e-4196-90c1-b16114f38eb1', life_has_intrinsic_value, deontological).
narrative_ontology:cs_axiom('2d8df589-a52e-4196-90c1-b16114f38eb1', foundational, intentional_killing_is_morally_illicit).
narrative_ontology:cs_axiom_status(intentional_killing_is_morally_illicit, holdable).
narrative_ontology:cs_axiom_grounding('2d8df589-a52e-4196-90c1-b16114f38eb1', intentional_killing_is_morally_illicit, theological).
narrative_ontology:cs_reference_frame('2d8df589-a52e-4196-90c1-b16114f38eb1', universal_moral_prohibition_on_killing).
narrative_ontology:cs_drift_state('2d8df589-a52e-4196-90c1-b16114f38eb1', contemporary_bioethics_debate, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('2d8df589-a52e-4196-90c1-b16114f38eb1', '').
narrative_ontology:cs_kernel_id(dignified_death__sanctity_primary, dignified_death).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dignified_death__sanctity_primary, moral_order_advocates).
narrative_ontology:constraint_beneficiary(dignified_death__sanctity_primary, religious_institutions).
narrative_ontology:constraint_victim(dignified_death__sanctity_primary, terminally_ill_patients).
narrative_ontology:constraint_victim(dignified_death__sanctity_primary, vulnerable_elderly).
narrative_ontology:constraint_victim(dignified_death__sanctity_primary, disabled_persons).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(dignified_death__sanctity_primary, medical_professionals).
narrative_ontology:constraint_vindicates(dignified_death__sanctity_primary, intrinsic_value_of_life_doctrine).
narrative_ontology:constraint_vindicates(dignified_death__sanctity_primary, transcendent_moral_law).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Forced to endure prolonged suffering against their will due to legal prohibitions on assisted dying, even with full mental capacity. Their only 'exit' is natural death, often after significant pain and loss of dignity as they define it.
narrative_ontology:constraint_stakeholder(dignified_death__sanctity_primary, terminally_ill_patients, payer,
    powerless, immediate, trapped, local).

% Subject to a societal norm that prioritizes biological existence over quality of life, potentially leading to unwanted medical interventions and prolonged dependency. They may internalize the moral imperative against self-determination in death.
narrative_ontology:constraint_stakeholder(dignified_death__sanctity_primary, vulnerable_elderly, payer,
    powerless, biographical, identity_locked, local).

% Often perceived as having lives of 'lesser' quality, this constraint can reinforce paternalistic attitudes and deny their agency in end-of-life decisions, even when they are not terminally ill but face severe, intractable conditions.
narrative_ontology:constraint_stakeholder(dignified_death__sanctity_primary, disabled_persons, payer,
    moderate, biographical, constrained, local).

% Benefit from the legal and social reinforcement of a moral framework that upholds the sanctity of life as an intrinsic value, seeing it as foundational to societal cohesion and ethical principles. They actively lobby for and defend these prohibitions.
narrative_ontology:constraint_stakeholder(dignified_death__sanctity_primary, moral_order_advocates, beneficiary,
    organized, generational, mobile, national).

% Act as primary proponents and enforcers of the sanctity-of-life doctrine, influencing legislation and public opinion. They derive moral authority and institutional legitimacy from upholding what they consider transcendent moral law.
narrative_ontology:constraint_stakeholder(dignified_death__sanctity_primary, religious_institutions, agenda_setter,
    institutional, civilizational, constrained, global).

% Bound by legal and ethical codes that prohibit active assistance in dying, even when faced with patients in extreme suffering who request it. This creates moral distress and professional conflict, forcing them to prolong life against patient wishes.
narrative_ontology:constraint_stakeholder(dignified_death__sanctity_primary, medical_professionals, payer,
    institutional, biographical, constrained, local).
narrative_ontology:stakeholder_secondary_role(dignified_death__sanctity_primary, medical_professionals, agenda_setter).

% Seek to legalize assisted dying based on individual self-determination. They are actively campaigning against this constraint but are largely excluded from the core decision-making bodies that uphold the sanctity-of-life principle.
narrative_ontology:constraint_stakeholder(dignified_death__sanctity_primary, autonomy_advocates, excluded,
    organized, biographical, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a societal commitment to protecting all human life, especially the vulnerable, by establishing a universal moral prohibition against intentional killing, thereby fostering a sense of collective responsibility and moral order.
% TRANSFER_FUNCTION: Transfers the ultimate decision-making authority over end-of-life choices from the individual to a transcendent moral framework, enforced by legal and social institutions. This prolongs the lives of individuals who wish to die, transferring the burden of suffering to them.
% ABSENT_VOICES: Terminally ill patients and their families who desire assisted dying are often marginalized in policy debates, their lived experience of suffering and desire for agency overridden by abstract moral principles. Autonomy advocates are actively campaigning but remain outside the institutional power structures that uphold this constraint.
% DISAPPEARANCE_RATIONALE: If this constraint vanished, legal frameworks would rapidly shift to permit various forms of assisted dying, leading to a profound re-evaluation of medical ethics, individual rights, and the role of the state in end-of-life decisions. The moral landscape of society would fundamentally reorganize.
% FOUNDING_PROBLEM: The problem of protecting human life from arbitrary or coerced termination, particularly for those unable to advocate for themselves, and upholding a universal moral standard against killing.
% FOUNDING_PROBLEM_CORROBORATION: Religious institutions and moral philosophers attest that the problem of protecting vulnerable life and upholding moral order is perpetually live. However, autonomy advocates and many medical professionals argue that the constraint's application has shifted from protecting against coercion to coercing prolongation of suffering, making its 'live' status contested in its current form.
narrative_ontology:disappearance_verdict(dignified_death__sanctity_primary, world_rearranges).
narrative_ontology:founding_problem_status(dignified_death__sanctity_primary, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(dignified_death__sanctity_primary, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
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
 *   Extractiveness is high (0.6) because it imposes unwanted prolongation of life and suffering on individuals, denying their agency. Suppression is also high (0.7) due to legal prohibitions and strong social/religious pressure that actively prevent alternatives like assisted dying. Theater ratio is low (0.1) because the constraint is genuinely enforced and its proponents sincerely believe in its moral necessity; there is little performative maintenance. Accessibility collapse is moderate (0.4) as alternatives are legally and socially suppressed but not conceptually impossible. Resistance is high (0.75) from patient advocacy groups and autonomy movements.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the victims, this constraint is a coercive imposition of suffering. From the perspective of the beneficiaries, it is a necessary protection of a fundamental moral good. The engine's classification will highlight this divergence, showing a Snare for the victims and potentially a Rope or even Mountain for the beneficiaries, reflecting the structural asymmetry.
 *
 * DIRECTIONALITY LOGIC:
 *   Terminally ill patients, vulnerable elderly, and disabled persons are the primary targets (high d) as they bear the direct costs of prolonged suffering and denied agency. Moral order advocates and religious institutions are beneficiaries (low d) as their moral framework is upheld and their institutional authority reinforced. Medical professionals are in a complex position, acting as enforcers (agenda_setter) but also experiencing moral distress (payer) from the constraint.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    coercion_vs_protection_boundary,
    'At what point does the ''protection of vulnerable life'' (coordination) transition into ''coercive prolongation of suffering'' (extraction) for individuals with full capacity?',
    'Empirical studies on patient-reported quality of life, psychological distress, and perceived autonomy in jurisdictions with and without assisted dying laws, focusing on the experiences of vulnerable populations.',
    'If the transition point is demonstrably crossed for a significant population, the constraint''s extractiveness is higher than currently measured, strengthening its Snare classification. If coercion is rare, the coordination function is more salient.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coercion_vs_protection_boundary, empirical, 'Distinguishing genuine protection from unwanted prolongation of life.').

omega_variable(
    transcendent_moral_law_status,
    'Is the ''transcendent moral law'' invoked by this reading a universally recognized and self-evident truth, or a culturally/religiously specific normative claim?',
    'Cross-cultural philosophical analysis and comparative legal studies of end-of-life ethics in diverse societies. Examination of the historical contingency of ''sanctity of life'' doctrines.',
    'If found to be culturally contingent, the ''emerges_naturally'' aspect of the constraint (if claimed as Mountain) would be undermined, and its reliance on active enforcement (Snare) would be more evident. If universal, its moral grounding is strengthened.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(transcendent_moral_law_status, conceptual, 'The epistemic status of the ''transcendent moral law'' claim.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (legal prohibitions, lack of access to alternatives) or internalized (patients'' belief in the moral wrongness of assisted dying, fear of social stigma)?',
    'Post-legalization suppression trajectory: if suppression of desire for assisted dying persists after legal barriers are removed, reclassify as partially internalized. Surveys on patient attitudes and family pressure.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests — the target carries the suppression with them after exit. This would indicate a deeper, more insidious form of control.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism in end-of-life decisions.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dignified_death__sanctity_primary, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dign_tr_t0, dignified_death__sanctity_primary, theater_ratio, 0, 0.1).
narrative_ontology:measurement(dign_tr_t5, dignified_death__sanctity_primary, theater_ratio, 5, 0.1).
narrative_ontology:measurement(dign_tr_t10, dignified_death__sanctity_primary, theater_ratio, 10, 0.1).
narrative_ontology:measurement(dign_tr_t15, dignified_death__sanctity_primary, theater_ratio, 15, 0.1).
narrative_ontology:measurement(dign_tr_t20, dignified_death__sanctity_primary, theater_ratio, 20, 0.1).

% Extraction over time
narrative_ontology:measurement(dign_be_t0, dignified_death__sanctity_primary, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(dign_be_t5, dignified_death__sanctity_primary, base_extractiveness, 5, 0.55).
narrative_ontology:measurement(dign_be_t10, dignified_death__sanctity_primary, base_extractiveness, 10, 0.6).
narrative_ontology:measurement(dign_be_t15, dignified_death__sanctity_primary, base_extractiveness, 15, 0.62).
narrative_ontology:measurement(dign_be_t20, dignified_death__sanctity_primary, base_extractiveness, 20, 0.6).

% Suppression requirement over time
narrative_ontology:measurement(dign_su_t0, dignified_death__sanctity_primary, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(dign_su_t5, dignified_death__sanctity_primary, suppression_requirement, 5, 0.65).
narrative_ontology:measurement(dign_su_t10, dignified_death__sanctity_primary, suppression_requirement, 10, 0.7).
narrative_ontology:measurement(dign_su_t15, dignified_death__sanctity_primary, suppression_requirement, 15, 0.72).
narrative_ontology:measurement(dign_su_t20, dignified_death__sanctity_primary, suppression_requirement, 20, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dignified_death__sanctity_primary, identity_coordination).
narrative_ontology:affects_constraint(dignified_death__sanctity_primary, dignified_death__autonomy_primary).
narrative_ontology:affects_constraint(dignified_death__sanctity_primary, dignified_death__relational_autonomy).

% DUAL FORMULATION NOTE:
% This constraint is the 'sanctity_primary' reading of the 'dignified_death' kernel. It is structurally distinct from the 'autonomy_primary' and 'relational_autonomy' readings, which emphasize individual self-determination and relational decision-making, respectively. All three are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
