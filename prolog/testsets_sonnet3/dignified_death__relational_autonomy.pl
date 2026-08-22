% ============================================================================
% CONSTRAINT STORY: dignified_death__relational_autonomy
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_dignified_death__relational_autonomy, []).

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
 *   constraint_id: dignified_death__relational_autonomy
 *   human_readable: Relational Autonomy Model of End-of-Life Decision Authority
 *   domain: bioethics/medical_law/political_philosophy
 *
 * SUMMARY:
 *   This story authors the relational-autonomy reading of the dignified-death
 *   kernel: dignity is treated as constituted through relationship rather
 *   than residing solely in individual will or in life's intrinsic sanctity.
 *   Decision authority over hastened death is distributed across a
 *   patient-family-clinician triad, bounded by procedural safeguards
 *   (capacity assessment, mandated consultation, waiting periods,
 *   documentation). This is a genuine coordination structure — it solves real
 *   problems of coercion-detection and shared accountability — but it also
 *   creates a new class of people for whom the relational premise does not
 *   hold: those without family, or in conflict with family, who lose ground
 *   relative to what a pure autonomy model would grant them. This story is
 *   ONE of three readings of the same kernel; the autonomy_primary and
 *   sanctity_primary readings are separate constraint files with their own ε
 *   and stakeholder sets, not alternate measurements of this one.
 *
 * KEY AGENTS:
 *   - terminally_ill_patient: primary decision subject (moderate/constrained) — one voice among three
 *   - family_decision_participants: seated co-decider (moderate/constrained) — gains formal standing
 *   - treating_clinicians: administers and enforces the procedural safeguards (institutional/constrained)
 *   - decisionally_isolated_patients: bears the cost of a relational premise that does not fit their situation (powerless/trapped)
 *   - patients_in_conflict_with_family_wishes: sustained competent wish overridden by seated family objection (powerless/constrained)
 *   - hospital_ethics_committees: adjudicates triad breakdown, sets institutional precedent (institutional/analytical)
 *   - disability_and_patient_advocacy_groups: analytical critic of family-capture risk, not seated in individual decisions (organized/analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dignified_death__relational_autonomy, 0.38).
domain_priors:suppression_score(dignified_death__relational_autonomy, 0.42).
domain_priors:theater_ratio(dignified_death__relational_autonomy, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dignified_death__relational_autonomy, extractiveness, 0.38).
narrative_ontology:constraint_metric(dignified_death__relational_autonomy, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(dignified_death__relational_autonomy, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(dignified_death__relational_autonomy, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(dignified_death__relational_autonomy, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dignified_death__relational_autonomy, rope).
narrative_ontology:human_readable(dignified_death__relational_autonomy, "Relational Autonomy Model of End-of-Life Decision Authority").
narrative_ontology:topic_domain(dignified_death__relational_autonomy, "bioethics/medical_law/political_philosophy").

domain_priors:requires_active_enforcement(dignified_death__relational_autonomy).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dignified_death__relational_autonomy, 'b84aac75-f978-4c47-9171-8a4d299a046f').
narrative_ontology:cs_kernel_codification('b84aac75-f978-4c47-9171-8a4d299a046f', distributed).
narrative_ontology:cs_authority_grounding('b84aac75-f978-4c47-9171-8a4d299a046f', practice).
narrative_ontology:cs_interpretation_layer_present('b84aac75-f978-4c47-9171-8a4d299a046f').
narrative_ontology:cs_reading_relation('b84aac75-f978-4c47-9171-8a4d299a046f', dignified_death__autonomy_primary, influences).
narrative_ontology:cs_reading_relation('b84aac75-f978-4c47-9171-8a4d299a046f', dignified_death__sanctity_primary, coexists_with).
narrative_ontology:cs_axiom('b84aac75-f978-4c47-9171-8a4d299a046f', foundational, relational_constitution_of_personhood).
narrative_ontology:cs_axiom_status(relational_constitution_of_personhood, holdable).
narrative_ontology:cs_axiom_grounding('b84aac75-f978-4c47-9171-8a4d299a046f', relational_constitution_of_personhood, conventional).
narrative_ontology:cs_axiom('b84aac75-f978-4c47-9171-8a4d299a046f', secondary, coercion_undetectable_without_relational_witness).
narrative_ontology:cs_axiom_status(coercion_undetectable_without_relational_witness, holdable).
narrative_ontology:cs_axiom_grounding('b84aac75-f978-4c47-9171-8a4d299a046f', coercion_undetectable_without_relational_witness, empirically_contingent).
narrative_ontology:cs_reference_frame('b84aac75-f978-4c47-9171-8a4d299a046f', relational_personhood_bioethics).
narrative_ontology:cs_drift_state('b84aac75-f978-4c47-9171-8a4d299a046f', contemporary_assisted_dying_legislation_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('b84aac75-f978-4c47-9171-8a4d299a046f', '').
narrative_ontology:cs_kernel_id(dignified_death__relational_autonomy, dignified_death).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dignified_death__relational_autonomy, relational_care_network).
narrative_ontology:constraint_beneficiary(dignified_death__relational_autonomy, treating_clinicians).
narrative_ontology:constraint_beneficiary(dignified_death__relational_autonomy, family_decision_participants).
narrative_ontology:constraint_victim(dignified_death__relational_autonomy, decisionally_isolated_patients).
narrative_ontology:constraint_victim(dignified_death__relational_autonomy, patients_with_estranged_or_absent_family).
narrative_ontology:constraint_victim(dignified_death__relational_autonomy, patients_in_conflict_with_family_wishes).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(dignified_death__relational_autonomy, terminally_ill_patient).
narrative_ontology:constraint_victim(dignified_death__relational_autonomy, terminally_ill_patient).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Facing a terminal or gravely suffering condition and seeking a hastened or assisted death. Under this model, the patient's expressed wish is one input weighed alongside family consultation and clinical judgment rather than a dispositive instruction. Benefits when the triad process surfaces considerations the patient alone would miss or affirms a wish under pressure; pays when the process delays or overrides a clear, competent, sustained wish because family or clinician disagree.
narrative_ontology:constraint_stakeholder(dignified_death__relational_autonomy, terminally_ill_patient, beneficiary,
    moderate, immediate, constrained, local).
narrative_ontology:stakeholder_secondary_role(dignified_death__relational_autonomy, terminally_ill_patient, payer).

% Spouses, adult children, or designated kin formally seated in the decision process. Gain legitimate standing to voice concerns, request additional time, or object to a course of action, and gain protection from being sidelined by a purely individualistic model. Their inclusion is the coordination function this arrangement exists to deliver.
narrative_ontology:constraint_stakeholder(dignified_death__relational_autonomy, family_decision_participants, beneficiary,
    moderate, biographical, constrained, local).

% Physicians and care teams who administer the procedural safeguards: capacity assessments, waiting periods, mandated family consultation, documentation of triad consensus or dissent. They set and enforce the process, are shielded from unilateral liability by following it, but also bear the burden of adjudicating disputes between patient wishes and family objections, and of documenting compliance under audit and legal review.
narrative_ontology:constraint_stakeholder(dignified_death__relational_autonomy, treating_clinicians, agenda_setter,
    institutional, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(dignified_death__relational_autonomy, treating_clinicians, beneficiary).

% Patients without accessible or willing family, or whose family relationships are estranged, absent, or actively hostile. The triad process assumes a functioning relational network; where none exists, these patients face additional institutional proxies, extended timelines, or de facto barriers to accessing what an autonomy-primary model would grant on their word alone. The relational premise becomes an obstacle rather than a protection for exactly this group.
narrative_ontology:constraint_stakeholder(dignified_death__relational_autonomy, decisionally_isolated_patients, payer,
    powerless, immediate, trapped, local).

% Competent patients whose sustained, clearly expressed wish is opposed by family members participating in the triad. The procedural safeguards give the objecting family formal standing to slow, contest, or in practice block the patient's stated wish, even where the patient would satisfy every capacity and voluntariness standard under an autonomy-primary reading.
narrative_ontology:constraint_stakeholder(dignified_death__relational_autonomy, patients_in_conflict_with_family_wishes, payer,
    powerless, immediate, constrained, local).

% Administer and interpret the procedural safeguards when triad consensus fails, hearing appeals and setting institutional precedent for how much weight family objection carries against patient wish. Their rulings shape whether the model functions as genuine coordination or as a veto point for whichever party is best positioned to invoke process.
narrative_ontology:constraint_stakeholder(dignified_death__relational_autonomy, hospital_ethics_committees, agenda_setter,
    institutional, generational, analytical, regional).
narrative_ontology:stakeholder_secondary_role(dignified_death__relational_autonomy, hospital_ethics_committees, observer).

% Argue that mandatory family/clinician involvement can be used to pressure or override vulnerable patients, particularly those with disabilities whose family members may have financial or caregiving-burden interests in the outcome. Not formally seated in individual triad decisions; their structural critique of the model is voiced in policy debate, not in the bedside process itself.
narrative_ontology:constraint_stakeholder(dignified_death__relational_autonomy, disability_and_patient_advocacy_groups, excluded,
    organized, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(dignified_death__relational_autonomy, diffuse).
narrative_ontology:fixing_cost_class(dignified_death__relational_autonomy, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the genuine problem that end-of-life decisions affect and are affected by an entire relational network — family members who will live with the aftermath, clinicians who bear legal and moral responsibility for the act, and a patient whose judgment may be affected by depression, coercion, or isolation that only people who know them well can detect. Distributing authority across the triad coordinates information and accountability that a solitary decision-maker cannot.
% TRANSFER_FUNCTION: Moves final decision authority away from the patient alone and distributes it across patient, family, and clinician, with procedural safeguards (capacity review, waiting periods, mandatory consultation, documentation) absorbing time, discretion, and in some cases outcome control from the patient and reallocating it to the family's voice and the clinician's institutional judgment.
% ABSENT_VOICES: Patients without family, or in irreconcilable conflict with family, have no formal channel to demonstrate that the relational premise does not apply to their case; disability advocates who fear family-interest capture of the process are heard in policy forums but are not seated in the individual triad decision.
% DISAPPEARANCE_RATIONALE: Clinicians and ethics committees would say the world rearranges sharply — unilateral patient decisions would proceed without the safeguards they see as catching coercion, depression, or family estrangement, and institutions would face unmediated liability. Patients in conflict with family, and decisionally isolated patients, would say the world barely changes for them for the better, or actively improves, since the mechanism that currently constrains their choice would be removed. The disagreement itself is the structural fact.
% FOUNDING_PROBLEM: Purely autonomy-based right-to-die frameworks were criticized for treating dying as a solitary transaction, missing coercion by family members with financial interests, missing depression or ambivalence a lone clinician session would not surface, and leaving family members who must live with the death without any legitimate voice in it.
% FOUNDING_PROBLEM_CORROBORATION: Clinicians, ethics committees, and relational-ethics scholars attest the founding problem (coercion-detection, depression screening, family involvement) remains live and is best served by triad process. Disability advocacy groups and some bioethicists outside the clinical establishment attest that the same procedural machinery is now used to slow or block clearly competent, uncoerced patients, particularly those without supportive family, which is a different and unaddressed problem from the one the arrangement was built to solve.
narrative_ontology:disappearance_verdict(dignified_death__relational_autonomy, contested).
narrative_ontology:founding_problem_status(dignified_death__relational_autonomy, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(dignified_death__relational_autonomy, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(dignified_death__relational_autonomy, 'none', 1).
narrative_ontology:epsilon_provenance(dignified_death__relational_autonomy, 0.38, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(dignified_death__relational_autonomy_tests).
:- end_tests(dignified_death__relational_autonomy_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is authored moderate (0.38) — this is the reading's own predicted band (0.30-0.45): the coordination function is real (catching coercion, distributing moral weight, protecting family standing) but it extracts decisional authority from patients whose situation does not fit the relational premise, and the extraction concentrates specifically on the isolated and the family-opposed. Suppression (0.42) reflects that the safeguards are mandatory, not optional — a patient cannot bypass the triad process even when competent and clear, which is real coercive overhead even though it serves protective goals for others. Theater ratio is modest (0.28) and rises slowly — most of the procedural apparatus does functional work (capacity review, documentation) though some consultation requirements persist past the point of adding new information, drifting toward compliance theater as institutions harden documentation practices defensively.
 *
 * DIRECTIONALITY LOGIC:
 *   The relational network (family, clinicians) is the structural beneficiary: family gains formal voice it would lack under autonomy_primary, and clinicians gain liability protection and a shared decision record. Decisionally isolated patients and patients in conflict with family are the structural targets: the same safeguards that protect against coercion for the well-supported patient become a barrier for the patient without a functioning relational network, or with an adversarial one. This is the reading's key structural asymmetry — the beneficiary set (relational network) and victim set (those for whom the relational premise fails) are defined by whether the patient actually has the relational context the model assumes.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (coercion-detection, depression-screening, family voice) remains partly live — family coercion and depression-driven ambivalence are real and clinically documented. But the arrangement's procedural weight now falls heaviest on exactly the patients least implicated in the founding problem: the isolated patient has no family to coerce them, yet faces extended institutional proxies in place of the missing family voice; the patient in clear, sustained conflict with family has already demonstrated the deliberative clarity the safeguards exist to test, yet remains subject to them. Classifying this as rope rather than tangled_rope or snare requires establishing that the coordination benefit is genuinely realized for the majority case (supported patients with engaged, good-faith family) even as it imposes disproportionate cost on the minority case — a rope with an uneven procedural tax, not a captured extraction mechanism, is the honest read given no concentrated beneficiary collects rent from the arrangement's operation.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    relational_premise_universality,
    'Is relational embeddedness a near-universal feature of end-of-life situations that the model correctly assumes, or a demographic pattern that systematically excludes an isolated minority the model fails to accommodate?',
    'Empirical survey of triad-model jurisdictions tracking outcomes for patients without accessible family versus those with engaged family: differential wait times, differential rates of institutional override, differential rates of granted requests.',
    'If the isolated-patient population is small and the model has robust fallback procedures, the extraction on that group is a minor, correctable friction. If the population is substantial and fallback procedures function as de facto denial, the relational premise is doing extractive work disguised as protective coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(relational_premise_universality, empirical, 'Whether the relational assumption underlying the model fits the actual patient population or systematically disadvantages a minority.').

omega_variable(
    family_veto_versus_family_voice,
    'Do the procedural safeguards give family members genuine consultative voice, or effective veto power over a competent patient''s sustained wish?',
    'Case-law and ethics-committee-ruling analysis: track how often stated family objection alone (absent capacity or coercion concerns) delays or blocks a request versus how often it merely triggers additional documented deliberation without changing the outcome.',
    'Voice-only findings support the rope classification as authored. Veto-in-practice findings would push the constraint toward tangled_rope, since family would then be a beneficiary capturing decisional control at the patient''s direct expense through the same structure claimed to coordinate them.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(family_veto_versus_family_voice, empirical, 'Whether family''s formal role functions as consultation or as concealed veto.').

omega_variable(
    kernel_framing_alternative,
    'Could this constraint instead be framed as the interpretive-layer buffer between the autonomy_primary and sanctity_primary readings, rather than as a distinct third commitment?',
    'Compare jurisdictions that explicitly adopt relational_autonomy as a foundational bioethical stance versus jurisdictions that arrive at similar triad procedures as a political compromise between autonomy and sanctity advocates without endorsing relational personhood theory.',
    'If the triad structure is mostly a political compromise mechanism rather than a distinct philosophical commitment, its axioms (relational_constitution_of_personhood) would be better modeled as instrumental/conventional rather than the deontological-adjacent foundational claim authored here, which would change how axiom_overriding drift is assessed under cs_structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_framing_alternative, conceptual, 'Whether this reading is a genuine third philosophical position or a procedural compromise between the other two readings, which would change its axiom grounding_type.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dignified_death__relational_autonomy, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dign_tr_t0, dignified_death__relational_autonomy, theater_ratio, 0, 0.18).
narrative_ontology:measurement(dign_tr_t4, dignified_death__relational_autonomy, theater_ratio, 4, 0.2).
narrative_ontology:measurement(dign_tr_t8, dignified_death__relational_autonomy, theater_ratio, 8, 0.22).
narrative_ontology:measurement(dign_tr_t12, dignified_death__relational_autonomy, theater_ratio, 12, 0.24).
narrative_ontology:measurement(dign_tr_t16, dignified_death__relational_autonomy, theater_ratio, 16, 0.26).
narrative_ontology:measurement(dign_tr_t20, dignified_death__relational_autonomy, theater_ratio, 20, 0.27).
narrative_ontology:measurement(dign_tr_t24, dignified_death__relational_autonomy, theater_ratio, 24, 0.28).

% Extraction over time
narrative_ontology:measurement(dign_be_t0, dignified_death__relational_autonomy, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(dign_be_t4, dignified_death__relational_autonomy, base_extractiveness, 4, 0.31).
narrative_ontology:measurement(dign_be_t8, dignified_death__relational_autonomy, base_extractiveness, 8, 0.33).
narrative_ontology:measurement(dign_be_t12, dignified_death__relational_autonomy, base_extractiveness, 12, 0.35).
narrative_ontology:measurement(dign_be_t16, dignified_death__relational_autonomy, base_extractiveness, 16, 0.36).
narrative_ontology:measurement(dign_be_t20, dignified_death__relational_autonomy, base_extractiveness, 20, 0.37).
narrative_ontology:measurement(dign_be_t24, dignified_death__relational_autonomy, base_extractiveness, 24, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(dign_su_t0, dignified_death__relational_autonomy, suppression_requirement, 0, 0.34).
narrative_ontology:measurement(dign_su_t4, dignified_death__relational_autonomy, suppression_requirement, 4, 0.36).
narrative_ontology:measurement(dign_su_t8, dignified_death__relational_autonomy, suppression_requirement, 8, 0.38).
narrative_ontology:measurement(dign_su_t12, dignified_death__relational_autonomy, suppression_requirement, 12, 0.39).
narrative_ontology:measurement(dign_su_t16, dignified_death__relational_autonomy, suppression_requirement, 16, 0.4).
narrative_ontology:measurement(dign_su_t20, dignified_death__relational_autonomy, suppression_requirement, 20, 0.41).
narrative_ontology:measurement(dign_su_t24, dignified_death__relational_autonomy, suppression_requirement, 24, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dignified_death__relational_autonomy, enforcement_mechanism).
narrative_ontology:affects_constraint(dignified_death__relational_autonomy, dignified_death__autonomy_primary).
narrative_ontology:affects_constraint(dignified_death__relational_autonomy, dignified_death__sanctity_primary).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the dignified_death kernel. autonomy_primary authors patient-sole-authority as the constraint (lower procedural overhead, victim set = anyone the patient's own judgment fails to protect from external pressure); sanctity_primary authors the prohibition itself as the constraint (victim set = anyone whose suffering is prolonged by the prohibition). This reading (relational_autonomy) authors the triad-distribution model, with moderate ε (0.30-0.45) reflecting genuine coordination overhead rather than the low ε expected for autonomy_primary's minimal-process reading or the high ε expected for sanctity_primary's absolute-prohibition reading as experienced by those it constrains. Each file has independently authored ε; none is derived from or averaged with the others.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
