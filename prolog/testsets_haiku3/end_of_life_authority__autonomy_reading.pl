% ============================================================================
% CONSTRAINT STORY: end_of_life_authority__autonomy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: end_of_life_authority__autonomy_reading
 *   human_readable: End-of-Life Autonomy: Individual Control Over Timing and Circumstances of Death
 *   domain: medical_ethics/bioethics/end_of_life_policy
 *
 * SUMMARY:
 *   This constraint instantiates the autonomy reading of the end-of-life
 *   authority kernel. Under this reading, individual autonomy grounds the
 *   right to control circumstances and timing of death when facing unbearable
 *   suffering. The constraint is CLAIMED as rope because the reading frames
 *   it as solving a genuine coordination problem — how to honor both patient
 *   autonomy and legitimate medical gatekeeping — through consent protocols
 *   and competency assessment. The authored metrics show substantial
 *   suppression (0.72) because the reading's implementation requires active
 *   suppression of paternalistic legal and medical restrictions that
 *   previously protected life regardless of patient preference.
 *   Extractiveness is low (0.31) because the benefit flow is primarily to
 *   patients (autonomy recognition) and physicians (role redefinition) rather
 *   than to an extractive agent. The key measurement dynamic is declining
 *   suppression_requirement over time: as recognition expands across
 *   jurisdictions and institutional acceptance consolidates, the machinery
 *   needed to hold back sanctity-based resistance decreases. Theater is low
 *   because the constraint performs its stated function (autonomy expression
 *   and medical assistance) rather than cloaking extraction.
 *
 * KEY AGENTS:
 *   - patients_facing_unbearable_suffering (primary beneficiary/agenda-setter; structurally powerless but granted decision authority)
 *   - informed_medical_practitioners (beneficiary; gains legitimate role)
 *   - patients_denied_choice (victim in non-recognition jurisdictions; trapped in forced prolongation)
 *   - legislators_and_regulators (agenda-setter; controls jurisdictional recognition and eligibility boundaries)
 *   - religious_institutional_actors (excluded; holds sanctity doctrine incompatible with autonomy reading)
 *   - disability_rights_advocates (excluded and structurally payer; risks pressure to accept non-treatment)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(end_of_life_authority__autonomy_reading, 0.31).
domain_priors:suppression_score(end_of_life_authority__autonomy_reading, 0.72).
domain_priors:theater_ratio(end_of_life_authority__autonomy_reading, 0.18).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(end_of_life_authority__autonomy_reading, extractiveness, 0.31).
narrative_ontology:constraint_metric(end_of_life_authority__autonomy_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(end_of_life_authority__autonomy_reading, theater_ratio, 0.18).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(end_of_life_authority__autonomy_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(end_of_life_authority__autonomy_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(end_of_life_authority__autonomy_reading, rope).
narrative_ontology:human_readable(end_of_life_authority__autonomy_reading, "End-of-Life Autonomy: Individual Control Over Timing and Circumstances of Death").
narrative_ontology:topic_domain(end_of_life_authority__autonomy_reading, "medical_ethics/bioethics/end_of_life_policy").

domain_priors:requires_active_enforcement(end_of_life_authority__autonomy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(end_of_life_authority__autonomy_reading, '7d4927b2-7393-4219-864b-bb44c9194e6d').
narrative_ontology:cs_kernel_codification('7d4927b2-7393-4219-864b-bb44c9194e6d', formalized).
narrative_ontology:cs_authority_grounding('7d4927b2-7393-4219-864b-bb44c9194e6d', lineage).
narrative_ontology:cs_interpretation_layer_present('7d4927b2-7393-4219-864b-bb44c9194e6d').
narrative_ontology:cs_reading_relation('7d4927b2-7393-4219-864b-bb44c9194e6d', end_of_life_authority__sanctity_reading, coexists_with).
narrative_ontology:cs_reading_relation('7d4927b2-7393-4219-864b-bb44c9194e6d', end_of_life_authority__slippery_slope_mechanism, influences).
narrative_ontology:cs_axiom('7d4927b2-7393-4219-864b-bb44c9194e6d', foundational, individual_autonomy_grounds_death_decisions).
narrative_ontology:cs_axiom_status(individual_autonomy_grounds_death_decisions, holdable).
narrative_ontology:cs_axiom_grounding('7d4927b2-7393-4219-864b-bb44c9194e6d', individual_autonomy_grounds_death_decisions, deontological).
narrative_ontology:cs_axiom('7d4927b2-7393-4219-864b-bb44c9194e6d', secondary, unbearable_suffering_justifies_choice_override).
narrative_ontology:cs_axiom_status(unbearable_suffering_justifies_choice_override, holdable).
narrative_ontology:cs_axiom_grounding('7d4927b2-7393-4219-864b-bb44c9194e6d', unbearable_suffering_justifies_choice_override, empirically_contingent).
narrative_ontology:cs_reference_frame('7d4927b2-7393-4219-864b-bb44c9194e6d', patient_autonomy_supreme_in_own_death).
narrative_ontology:cs_drift_state('7d4927b2-7393-4219-864b-bb44c9194e6d', contemporary_post_legalization_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('7d4927b2-7393-4219-864b-bb44c9194e6d', '2026-06-12T14:32:00Z').
narrative_ontology:cs_kernel_id(end_of_life_authority__autonomy_reading, end_of_life_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(end_of_life_authority__autonomy_reading, patients_facing_unbearable_suffering).
narrative_ontology:constraint_beneficiary(end_of_life_authority__autonomy_reading, informed_medical_practitioners).
narrative_ontology:constraint_victim(end_of_life_authority__autonomy_reading, patients_denied_choice).
narrative_ontology:constraint_victim(end_of_life_authority__autonomy_reading, family_decision_bearers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(end_of_life_authority__autonomy_reading, family_decision_bearers).
narrative_ontology:constraint_victim(end_of_life_authority__autonomy_reading, disability_rights_advocates).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Individuals with terminal diagnoses or severe chronic conditions producing pain, loss of bodily function, or existential distress that they experience as unbearable. Under the autonomy reading, they hold the right to choose when and how to end life rather than endure forced prolongation. They depend on medical and legal cooperation to exercise this right. Their structural powerlessness is mitigated by the autonomy framework's grant of decision authority, but their ability to implement the choice remains entirely dependent on physician willingness and jurisdictional legal status.
narrative_ontology:constraint_stakeholder(end_of_life_authority__autonomy_reading, patients_facing_unbearable_suffering, beneficiary,
    powerless, immediate, trapped, local).
narrative_ontology:stakeholder_secondary_role(end_of_life_authority__autonomy_reading, patients_facing_unbearable_suffering, agenda_setter).

% Physicians, nurse practitioners, and other qualified clinicians who work within frameworks where end-of-life autonomy is legally recognized. They gain a redefined professional role: rather than exclusively preserving life, they can provide medical assistance in death when competent patients explicitly request it and meet eligibility criteria. This redefines physician ethics from 'do no harm = always preserve life' to 'do no harm = respect patient autonomy and relieve suffering.' Their constraint exit is limited by professional licensing and jurisdictional law — they cannot simply opt out of the autonomy framework without losing practice privileges in recognition jurisdictions.
narrative_ontology:constraint_stakeholder(end_of_life_authority__autonomy_reading, informed_medical_practitioners, beneficiary,
    institutional, generational, constrained, national).

% Patients in non-recognition jurisdictions or those excluded from eligibility criteria (dementia, psychiatric conditions, non-terminal suffering, etc.). They bear the cost of the autonomy reading's non-implementation or partial implementation: forced continuation of life against preference, absence of legal mechanism to obtain physician assistance, no remedy for suffering. Their trapped status is total — they cannot exit the jurisdiction, cannot retroactively change medical status, and face legal barriers (physician can be prosecuted for assistance). The autonomy reading produces their victimhood by establishing the standard they are denied.
narrative_ontology:constraint_stakeholder(end_of_life_authority__autonomy_reading, patients_denied_choice, payer,
    powerless, immediate, trapped, local).

% Relatives and designated surrogate decision-makers. Under the autonomy reading, they benefit from having the burden of life-prolongation decisions lifted — the patient decides, not the family. However, in non-recognition jurisdictions they continue to bear the cost of surrogate decision-making without the option of patient autonomy. In recognition jurisdictions, they may experience moral distress if they hold life-sanctity values in conflict with the autonomy principle, or if they believe the patient's choice reflects depression or external pressure. Some families also bear the secondary cost of witnessing chosen death.
narrative_ontology:constraint_stakeholder(end_of_life_authority__autonomy_reading, family_decision_bearers, payer,
    moderate, biographical, constrained, local).
narrative_ontology:stakeholder_secondary_role(end_of_life_authority__autonomy_reading, family_decision_bearers, beneficiary).

% Faith traditions and their institutional representatives who hold doctrines of life's intrinsic sanctity and divine authority over death. They are structurally excluded from direct patient-decision processes — the autonomy reading places authority with the individual and physician, not with religious institutions. They exercise influence through moral advocacy, institutional opposition to legal reforms, and in some contexts political resistance. Many faith communities seek conscience protections (their members can refuse to participate) but do not seek to override patient autonomy — however, the reading is structured as an exclusion of their authority, not a negotiation with it.
narrative_ontology:constraint_stakeholder(end_of_life_authority__autonomy_reading, religious_institutional_actors, excluded,
    organized, generational, constrained, national).

% Disability communities and their advocates who contest whether the autonomy reading applies equitably or encodes ableist assumptions about quality of life and disabled persons' decisions. They are excluded from eligibility-design processes (physicians, legislators, bioethicists typically lead) but face downstream effects: as criteria expand and practices evolve, they risk experiencing structural pressure to accept non-treatment or to view their lives as candidates for assisted death. The reading's expansion can externalize costs onto disabled populations without their consent to the framework. They occupy a payer seat structurally by exclusion from design coupled with exposure to expansion.
narrative_ontology:constraint_stakeholder(end_of_life_authority__autonomy_reading, disability_rights_advocates, excluded,
    organized, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(end_of_life_authority__autonomy_reading, disability_rights_advocates, payer).

% Lawmakers and administrative bodies that enact, withhold, or modify legal recognition of end-of-life autonomy. They set eligibility criteria (terminal vs. non-terminal, competent vs. surrogate decision-making, psychological suffering vs. physical), procedural safeguards (waiting periods, multiple physician review, counseling requirements), and enforcement mechanisms. Their mobile exit reflects their ability to shift policies — they are not locked into any single reading but can change jurisdiction's recognition status. In recognition jurisdictions, they administer the constraint; in non-recognition jurisdictions, they enforce restrictions on physician assistance.
narrative_ontology:constraint_stakeholder(end_of_life_authority__autonomy_reading, legislators_and_regulators, agenda_setter,
    institutional, generational, mobile, national).

% Academic analysts and professional ethicists who study end-of-life frameworks, track eligibility criteria evolution, measure outcomes, and theorize about the relationship between autonomy and other values. They occupy an analytical seat — they do not directly decide cases or administer policy but provide expertise to decision-makers and document the constraint's operation. Their analytical exit is clean — they can change research focus without material consequence. They measure whether the autonomy reading's implementation produces the promised outcomes (respected choice, reduced suffering) or exhibits expansion patterns consistent with the slippery-slope mechanism reading.
narrative_ontology:constraint_stakeholder(end_of_life_authority__autonomy_reading, medical_ethicists, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(end_of_life_authority__autonomy_reading, patients_facing_unbearable_suffering).
narrative_ontology:fixing_cost_class(end_of_life_authority__autonomy_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Resolves the conflict between individual autonomy and medical duty to preserve life through a framework that honors both: patients gain decision authority; physicians gain a redefined role (respecting preference rather than exclusively preserving); society gains clarity on end-of-life ethics replacing prior prohibition. The coordination problem is: how do we structure end-of-life care so that neither patients suffer against their will nor physicians face impossible ethical conflicts between autonomy and life-preservation? Answer: consent protocols, competency assessment, and medical assistance when conditions are met.
% TRANSFER_FUNCTION: Transfers decision authority from medical and institutional gatekeepers (physicians, families, state restrictions) to individual patients. Specifically, patients gain the right to control timing and circumstances of death when facing unbearable suffering. The constraint also transfers burdens: families are relieved of surrogate decision-making weight (in recognition jurisdictions); physicians are relieved of the impossible conflict between respecting patient preference and exclusive life-preservation; but patients in non-recognition jurisdictions are burdened by forced prolongation, and disabled populations risk pressure to accept non-treatment as their lives are re-categorized.
% ABSENT_VOICES: Patients already deceased before the reading's recognition cannot testify to their preference for or against autonomy frameworks — decisions about them are made by survivors. Persons with dementia cannot participate in eligibility design even though they become subject to surrogate-autonomy decisions. Disability communities are often excluded from policy design tables even as expansion criteria affect them. Religious minorities' concerns about end-of-life authority are heard as advocacy but not as direct veto-holding seats. Future populations who will live under precedent-expansion are absent from initial implementation. Future clinicians who will face downstream expansion pressures are absent from founding-period design.
% DISAPPEARANCE_RATIONALE: If legal recognition of end-of-life autonomy vanished (e.g., jurisdictions retracted frameworks), end-of-life care would revert substantially: patients in severe suffering would lose the option of chosen death and physician assistance, forcing continued life-prolongation or underground clandestine physician assistance (with criminal jeopardy for providers). Family surrogate decision-making burden would return. Palliative sedation might increase as a proxy mechanism for patients seeking unconsciousness. Medical practice would revert to exclusive life-preservation ethics. Legal challenge and clandestine practice would likely resume, as they did before recognition began. The arrangement's disappearance would materially reshape end-of-life care infrastructure and patient experience.
% FOUNDING_PROBLEM: Patients in unbearable, unremediable suffering faced a structural conflict: medicine and law prohibited physician assistance in death even when patients explicitly consented, yet also offered no adequate acknowledgment or remedy for the suffering itself. Patients could not exercise autonomy over a decision about their own body and life-ending. Physicians faced ethical paralysis: respecting autonomy seemed to require assistance, but law and professional ethics prohibited it. Families bore impossible surrogate-decision weight without clear guidance. The founding problem: how can a person in severe suffering exercise autonomy over death's timing and circumstances, and how can medicine honor that autonomy without violating its duty to preserve life?
% FOUNDING_PROBLEM_CORROBORATION: Patient advocacy organizations (e.g., Right to Die societies, patient autonomy groups) independent of medical profession; clinicians working in recognition frameworks (Netherlands, Belgium, Canada, some US states) attesting to persistent patient demand for choice; bioethics literature on autonomy and suffering from secular philosophical and nursing ethics traditions; court decisions in multiple jurisdictions (Canadian Supreme Court, European human rights courts, state appellate courts) finding autonomy claims constitutionally or ethically valid; public opinion research showing sustained majority support for autonomy frameworks across decades and geographies. Corroboration is robust from seats outside the direct beneficiary group (courts, philosophers, patients themselves rather than only advocates).
narrative_ontology:disappearance_verdict(end_of_life_authority__autonomy_reading, world_rearranges).
narrative_ontology:founding_problem_status(end_of_life_authority__autonomy_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(end_of_life_authority__autonomy_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(end_of_life_authority__autonomy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(end_of_life_authority__autonomy_reading, 0.31, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(end_of_life_authority__autonomy_reading_tests).
:- end_tests(end_of_life_authority__autonomy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The autonomy reading instantiates genuine coordination: the conflict between respecting patient preference and maintaining medical authority is real, and consent protocols + competency assessment is a workable solution at scope (individual, local, or national depending on jurisdiction). Suppression is high (0.72) because the constraint's persistence depends on actively suppressing sanctity-based legal restrictions — the reading must overcome or sideline the prior regime's legal architecture. Extractiveness is low because the primary beneficiary (suffering patients) collects no rent and gains no material goods; they gain decision authority, which is redistributed power, not extraction. The measurement series shows suppression_requirement declining as institutional acceptance consolidates: early implementations (t=0) require high active suppression against legal barriers and institutional resistance; later implementations (t=35) settle into stable frameworks with lower ongoing suppression cost because opposition has been overcome or marginalized by precedent. Extractiveness plateaus at t=25 because the reading reaches steady-state coverage — terminal competent adults receive recognition early; expansion to non-terminal chronic suffering and eligibility-boundary questions continues but extractiveness does not rise because the core coordinate (autonomy + consent) is stable.
 *
 * PERSPECTIVAL GAP:
 *   From the patient's seat (trapped, immediate horizon), the reading grants autonomy and relief from forced suffering — a beneficiary experience. From the religious institutional actor's seat (organized, generational horizon), the reading forecloses their doctrine's applicability and removes their gatekeeping role — a victim/excluded experience. From the physician's seat (institutional, generational), the reading redefines their role from exclusive life-preserver to patient-preference agent, which can be experienced as liberation from impossible ethical conflicts or as liability expansion depending on the practitioner. From the disability advocate's seat (organized, generational), the reading risks being weaponized against disabled populations through eligibility expansion and quality-of-life judgments. The engine computes these divergent seats from the structural data; the autonomy reading itself instantiates only one interpretation of the kernel.
 *
 * DIRECTIONALITY LOGIC:
 *   Patients facing unbearable suffering are the primary beneficiaries (d near 0.0 — the reading grants them the right they previously lacked). Physicians gain a redefined role (secondary beneficiary; d moderately beneficiary). Patients denied choice in non-recognition jurisdictions are pure targets (d near 1.0 — they bear the cost of non-implementation through forced prolongation). Family decision-bearers have mixed directionality: they benefit from autonomy frameworks (burden lifted) but also pay in non-recognition contexts (continued surrogate decision burden). Disability advocates are structurally excluded and at risk of payer-position if expansion proceeds without their input (d weighted toward target). Religious institutions are excluded from direct decisions but exercise influence externally (d constrained/moderate). Legislators occupy the agenda-setter seat (d symmetric or slightly beneficiary depending on political alignment — they control the framework but are not primary collectors of benefit).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (how can a person in severe suffering exercise autonomy over death?) is live — patients continue to face this conflict in non-recognition jurisdictions and at eligibility boundaries in recognition ones. The disappearance verdict (world_rearranges) is consistent with a live founding problem: if the autonomy framework were removed, end-of-life care pathways would materially revert. There is no mandatrophy signal here — the constraint is not persisting past the dissolution of its original function. The risk is in the sibling readings: if the slippery-slope mechanism proves empirically valid (eligibility criteria expand beyond terminal competent adults to incompetent, non-terminal, and eventually coercively-applied scenarios), THEN the autonomy reading's founding problem (respect for competent choice) could become dead while the constraint persists — that would be mandatrophy. The current measurement series does not show this (extractiveness plateaus after institutional consolidation; theater remains low). This is the empirical question the constraint family's temporal measurements are designed to track.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    unbearable_suffering_criteria_ambiguity,
    'What constitutes ''unbearable suffering'' such that autonomy over death is justified? Is it defined by objective medical conditions, subjective patient report, or some combination? How are psychological suffering, existential distress, and quality-of-life judgments incorporated?',
    'Longitudinal analysis of jurisdictional eligibility criteria and case-law evolution; comparison of judicial and legislative definitions across recognition frameworks; audit of actual vs. stated criteria in practice.',
    'If suffering criteria remain narrow and objective (terminal physical deterioration), extractiveness stays low and the constraint remains coordination-focused. If criteria expand to include subjective suffering or non-terminal conditions, extractiveness may rise and victim set broadens — the constraint approaches the slippery-slope mechanism reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(unbearable_suffering_criteria_ambiguity, conceptual, 'Uncertainty in the definition of unbearable suffering and its expansion trajectory.').

omega_variable(
    coercive_expansion_risk,
    'Does the autonomy framework, once established for competent terminal patients, empirically expand to include incompetent patients (via substituted judgment), non-terminal chronic suffering, psychiatric conditions, and eventually persons who have not explicitly consented (ambient pressure to accept non-treatment)?',
    'Temporal measurement of eligibility criteria across decades in recognition jurisdictions; comparison of initial legislative language to actual practice evolution; qualitative research with patients and clinicians on pressure dynamics.',
    'If expansion occurs, the constraint transitions from autonomy-based rope to a coercive-expansion mechanism whose founding problem (respect for competent choice) becomes dead while the practices persist — mandatrophy signature. This is the slippery-slope reading''s empirical content.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coercive_expansion_risk, empirical, 'Whether autonomy frameworks empirically expand beyond their stated scope in ways that become coercive.').

omega_variable(
    disabled_life_quality_infiltration,
    'Do competency and suffering assessments encode ableist assumptions about disabled life? Are disabled individuals who request end-of-life autonomy assessed with the same scrutiny as non-disabled individuals, or are their requests treated as expressions of despair due to disability rather than autonomy?',
    'Comparative analysis of approval rates and clinical language by disability status; qualitative interviews with disabled patients and clinicians; audit of decision-making against criteria vs. expressed decision rationales.',
    'If ableist infiltration is substantial, disability communities transition from excluded to structurally payer — the autonomy reading becomes a mechanism that subjects them to pressure. This changes classification from rope to tangled_rope with asymmetric extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(disabled_life_quality_infiltration, empirical, 'Whether the autonomy reading''s application preserves non-discrimination or encodes quality-of-life judgments that harm disabled populations.').

omega_variable(
    kernel_reading_distinction,
    'Is the autonomy reading genuinely distinct from the sanctity reading (forecloses it) or merely opposed to it (coexists)? Does the core premise of autonomy — that individuals have the right to decide — logically entail the rejection of life-sanctity doctrine, or can a framework hold both?',
    'Philosophical analysis of the logical relationship between autonomy and sanctity premises; examination of actual jurisdictions that attempt both (e.g., strong autonomy protections + religious conscience protections for refusing physicians).',
    'If the readings foreclose each other, they cannot coexist in a single framework and one must dominate institutionally. If they coexist, both shape implementation (conscience protections, opt-in frameworks, religious facilities excepted). This determines whether the constraint''s suppression_requirement is fighting sanctity doctrine or negotiating with it.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_distinction, conceptual, 'Whether autonomy and sanctity readings are logically incompatible or can coexist within a single institutional framework.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(end_of_life_authority__autonomy_reading, 0, 35).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(end__tr_t0, end_of_life_authority__autonomy_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement(end__tr_t5, end_of_life_authority__autonomy_reading, theater_ratio, 5, 0.1).
narrative_ontology:measurement(end__tr_t10, end_of_life_authority__autonomy_reading, theater_ratio, 10, 0.12).
narrative_ontology:measurement(end__tr_t15, end_of_life_authority__autonomy_reading, theater_ratio, 15, 0.15).
narrative_ontology:measurement(end__tr_t20, end_of_life_authority__autonomy_reading, theater_ratio, 20, 0.17).
narrative_ontology:measurement(end__tr_t25, end_of_life_authority__autonomy_reading, theater_ratio, 25, 0.18).
narrative_ontology:measurement(end__tr_t30, end_of_life_authority__autonomy_reading, theater_ratio, 30, 0.18).
narrative_ontology:measurement(end__tr_t35, end_of_life_authority__autonomy_reading, theater_ratio, 35, 0.18).

% Extraction over time
narrative_ontology:measurement(end__be_t0, end_of_life_authority__autonomy_reading, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(end__be_t5, end_of_life_authority__autonomy_reading, base_extractiveness, 5, 0.19).
narrative_ontology:measurement(end__be_t10, end_of_life_authority__autonomy_reading, base_extractiveness, 10, 0.24).
narrative_ontology:measurement(end__be_t15, end_of_life_authority__autonomy_reading, base_extractiveness, 15, 0.27).
narrative_ontology:measurement(end__be_t20, end_of_life_authority__autonomy_reading, base_extractiveness, 20, 0.29).
narrative_ontology:measurement(end__be_t25, end_of_life_authority__autonomy_reading, base_extractiveness, 25, 0.3).
narrative_ontology:measurement(end__be_t30, end_of_life_authority__autonomy_reading, base_extractiveness, 30, 0.31).
narrative_ontology:measurement(end__be_t35, end_of_life_authority__autonomy_reading, base_extractiveness, 35, 0.31).

% Suppression requirement over time
narrative_ontology:measurement(end__su_t0, end_of_life_authority__autonomy_reading, suppression_requirement, 0, 0.88).
narrative_ontology:measurement(end__su_t5, end_of_life_authority__autonomy_reading, suppression_requirement, 5, 0.84).
narrative_ontology:measurement(end__su_t10, end_of_life_authority__autonomy_reading, suppression_requirement, 10, 0.8).
narrative_ontology:measurement(end__su_t15, end_of_life_authority__autonomy_reading, suppression_requirement, 15, 0.77).
narrative_ontology:measurement(end__su_t20, end_of_life_authority__autonomy_reading, suppression_requirement, 20, 0.75).
narrative_ontology:measurement(end__su_t25, end_of_life_authority__autonomy_reading, suppression_requirement, 25, 0.73).
narrative_ontology:measurement(end__su_t30, end_of_life_authority__autonomy_reading, suppression_requirement, 30, 0.72).
narrative_ontology:measurement(end__su_t35, end_of_life_authority__autonomy_reading, suppression_requirement, 35, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(end_of_life_authority__autonomy_reading, attachment_coordination).
narrative_ontology:boltzmann_floor_override(end_of_life_authority__autonomy_reading, 0.12).
narrative_ontology:affects_constraint(end_of_life_authority__autonomy_reading, end_of_life_authority__sanctity_reading).
narrative_ontology:affects_constraint(end_of_life_authority__autonomy_reading, end_of_life_authority__slippery_slope_mechanism).

% DUAL FORMULATION NOTE:
% The 'end-of-life authority' kernel is instantiated in three distinct constraint stories: (1) autonomy_reading: individual autonomy grounds the right to control death's timing; (2) sanctity_reading: intrinsic life-value prohibits intentional death regardless of preference; (3) slippery_slope_mechanism: empirical pattern of autonomy frameworks expanding beyond initial competent-terminal parameters. Each reading has distinct ε, beneficiary/victim structure, and persistence mechanism. They are not the same constraint viewed from different angles — they are structurally orthogonal constraints sharing a contested kernel. The autonomy reading influences both siblings: it provides the initial framework that the slippery-slope mechanism measures, and it creates institutional and legal pressure on sanctity frameworks to retract or reformulate.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(end_of_life_authority__autonomy_reading, organized, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
