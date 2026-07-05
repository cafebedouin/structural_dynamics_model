% ============================================================================
% CONSTRAINT STORY: dignified_death__relational_autonomy
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
    narrative_ontology:boltzmann_floor_override/2,
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
 *   human_readable: Relational Model of End-of-Life Decision Authority (Patient-Family-Clinician Triad)
 *   domain: bioethics/medical_law/political_philosophy
 *
 * SUMMARY:
 *   This story is one reading of the contested 'dignified death' kernel: the
 *   relational-autonomy reading, which holds that dignity at end of life
 *   emerges from a person's situated relationships rather than from isolated
 *   self-determination alone, and therefore distributes end-of-life decision
 *   authority across a patient-family-clinician triad bound by procedural
 *   safeguards (capacity assessment, mandated family consultation, waiting
 *   periods, ethics committee referral for contested cases). This is
 *   structurally distinct from the autonomy_primary reading (patient's
 *   contemporaneous wish is dispositive) and the sanctity_primary reading (no
 *   consented termination is ever legitimate) — those are separate
 *   constraints, not alternative measurements of this one. The coordination
 *   function here is real: catching coercion and capacity distortion that a
 *   pure-autonomy model would miss. The extraction is also real: patients
 *   without family, and patients whose family disagrees with them, pay a
 *   procedural tax the model's own logic cannot avoid imposing on them.
 *
 * KEY AGENTS:
 *   - terminally_ill_patient: bears the immediate cost of delay and distributed authority; powerless/trapped
 *   - family_decision_participants: gain formal standing and protection; moderate/constrained
 *   - treating_clinicians: administer and are protected by the safeguards; institutional/constrained
 *   - isolated_patients_without_family: structurally disadvantaged by a model requiring a relational third party they lack; powerless/trapped
 *   - patients_whose_stated_wishes_conflict_with_family: institutionally outweighed by family dissent; powerless/trapped
 *   - hospital_ethics_committees: analytical/adjudicating seat generating the procedural overhead
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dignified_death__relational_autonomy, 0.37).
domain_priors:suppression_score(dignified_death__relational_autonomy, 0.42).
domain_priors:theater_ratio(dignified_death__relational_autonomy, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dignified_death__relational_autonomy, extractiveness, 0.37).
narrative_ontology:constraint_metric(dignified_death__relational_autonomy, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(dignified_death__relational_autonomy, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(dignified_death__relational_autonomy, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(dignified_death__relational_autonomy, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dignified_death__relational_autonomy, rope).
narrative_ontology:human_readable(dignified_death__relational_autonomy, "Relational Model of End-of-Life Decision Authority (Patient-Family-Clinician Triad)").
narrative_ontology:topic_domain(dignified_death__relational_autonomy, "bioethics/medical_law/political_philosophy").

domain_priors:requires_active_enforcement(dignified_death__relational_autonomy).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dignified_death__relational_autonomy, 'b6cadd92-03c9-45dc-a0b2-ddef00ae0c44').
narrative_ontology:cs_kernel_codification('b6cadd92-03c9-45dc-a0b2-ddef00ae0c44', distributed).
narrative_ontology:cs_authority_grounding('b6cadd92-03c9-45dc-a0b2-ddef00ae0c44', distributed).
narrative_ontology:cs_reading_relation('b6cadd92-03c9-45dc-a0b2-ddef00ae0c44', dignified_death__autonomy_primary, influences).
narrative_ontology:cs_reading_relation('b6cadd92-03c9-45dc-a0b2-ddef00ae0c44', dignified_death__sanctity_primary, coexists_with).
narrative_ontology:cs_axiom('b6cadd92-03c9-45dc-a0b2-ddef00ae0c44', foundational, dignity_is_constituted_relationally_not_individually).
narrative_ontology:cs_axiom_status(dignity_is_constituted_relationally_not_individually, holdable).
narrative_ontology:cs_axiom_grounding('b6cadd92-03c9-45dc-a0b2-ddef00ae0c44', dignity_is_constituted_relationally_not_individually, deontological).
narrative_ontology:cs_axiom('b6cadd92-03c9-45dc-a0b2-ddef00ae0c44', secondary, procedural_distribution_of_authority_improves_decision_reliability).
narrative_ontology:cs_axiom_status(procedural_distribution_of_authority_improves_decision_reliability, holdable).
narrative_ontology:cs_axiom_grounding('b6cadd92-03c9-45dc-a0b2-ddef00ae0c44', procedural_distribution_of_authority_improves_decision_reliability, empirically_contingent).
narrative_ontology:cs_reference_frame('b6cadd92-03c9-45dc-a0b2-ddef00ae0c44', individual_informed_consent_baseline).
narrative_ontology:cs_drift_state('b6cadd92-03c9-45dc-a0b2-ddef00ae0c44', post_capacity_law_reform_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('b6cadd92-03c9-45dc-a0b2-ddef00ae0c44', '').
narrative_ontology:cs_kernel_id(dignified_death__relational_autonomy, dignified_death).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dignified_death__relational_autonomy, relational_care_network).
narrative_ontology:constraint_beneficiary(dignified_death__relational_autonomy, treating_clinicians).
narrative_ontology:constraint_beneficiary(dignified_death__relational_autonomy, family_decision_participants).
narrative_ontology:constraint_victim(dignified_death__relational_autonomy, isolated_patients_without_family).
narrative_ontology:constraint_victim(dignified_death__relational_autonomy, patients_whose_stated_wishes_conflict_with_family).
narrative_ontology:constraint_victim(dignified_death__relational_autonomy, estranged_or_abusive_family_excluded_by_process).
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

% Faces a terminal or gravely suffering condition and wants an end-of-life decision honored. Under this model, their stated wish is one input weighted alongside family consultation and clinical judgment rather than a dispositive instruction; where family and clinician disagree with the patient's expressed wish, the procedural safeguards can slow, condition, or override the individual's timeline. Benefits when family support genuinely reflects their values; is extracted from when the triad substitutes its own judgment for theirs.
narrative_ontology:constraint_stakeholder(dignified_death__relational_autonomy, terminally_ill_patient, payer,
    powerless, immediate, trapped, local).
narrative_ontology:stakeholder_secondary_role(dignified_death__relational_autonomy, terminally_ill_patient, beneficiary).

% Participate formally in the decision-making process, contributing information about the patient's values, relational history, and likely wishes. Gain legitimate voice and protection from guilt or exclusion, and in ambiguous cases their reading of the patient's dignity can carry as much or more weight than the patient's own contemporaneous statement, particularly when capacity is contested.
narrative_ontology:constraint_stakeholder(dignified_death__relational_autonomy, family_decision_participants, beneficiary,
    moderate, biographical, constrained, local).
narrative_ontology:stakeholder_secondary_role(dignified_death__relational_autonomy, family_decision_participants, agenda_setter).

% Administer the procedural safeguards: capacity assessments, family consultations, ethics committee referrals, waiting periods. Bear legal liability for the process and gain professional and institutional protection from the safeguards even when they slow contested cases. Have moderate exit (can decline participation on conscience grounds in many jurisdictions) but are bound by institutional protocol once engaged.
narrative_ontology:constraint_stakeholder(dignified_death__relational_autonomy, treating_clinicians, agenda_setter,
    institutional, biographical, constrained, national).

% Have no relational network to supply the 'relational context' the model requires to construct dignity. The triad model's procedural machinery has no third leg for them, so they either get shunted into a default clinician-heavy process, face additional scrutiny for lacking corroborating family testimony, or their case moves more slowly and more paternalistically than it would for a patient with family. They are structurally disadvantaged by a framework built around a relationship they do not have.
narrative_ontology:constraint_stakeholder(dignified_death__relational_autonomy, isolated_patients_without_family, payer,
    powerless, immediate, trapped, local).

% Have expressed a clear, capacity-verified wish that family members contest, whether from grief, religious conviction, financial interest, or genuine disagreement about the patient's welfare. The relational model gives this dissent institutional standing it would not have under pure autonomy, which can delay, complicate, or in edge cases block the patient's own decision through additional review layers triggered by family objection.
narrative_ontology:constraint_stakeholder(dignified_death__relational_autonomy, patients_whose_stated_wishes_conflict_with_family, payer,
    powerless, immediate, trapped, local).

% May be formally or informally screened out of the consultation process when clinicians judge the relationship as non-relational, coercive, or harmful, on the premise that only a genuine relational bond can ground dignity input. Their exclusion is usually protective of the patient but is discretionary and not fully proceduralized, meaning legitimate family members can occasionally be wrongly excluded and illegitimate influence can occasionally be wrongly admitted.
narrative_ontology:constraint_stakeholder(dignified_death__relational_autonomy, estranged_or_abusive_family_excluded_by_process, excluded,
    powerless, immediate, constrained, local).

% Adjudicate contested cases where patient, family, and clinician do not converge. Their existence is the procedural overhead the model requires; they both generate legitimacy for hard cases and slow down time-sensitive dying patients who are, by definition, running out of time to wait for review.
narrative_ontology:constraint_stakeholder(dignified_death__relational_autonomy, hospital_ethics_committees, observer,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(dignified_death__relational_autonomy, hospital_ethics_committees, agenda_setter).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(dignified_death__relational_autonomy, diffuse).
narrative_ontology:fixing_cost_class(dignified_death__relational_autonomy, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the genuine problem that end-of-life decisions made in extremis by a single isolated actor (patient alone, family alone, or clinician alone) are error-prone under conditions of pain, grief, incomplete information, and power asymmetry — distributing authority across three parties with procedural checkpoints is meant to catch coercion, undisclosed capacity loss, and unrepresented values that a single-party model would miss.
% TRANSFER_FUNCTION: Moves final decision weight away from the individual patient's contemporaneous wish and distributes it across family testimony and clinical judgment, mediated by procedural safeguards (capacity review, waiting periods, ethics consultation); moves legitimacy and liability protection toward clinicians and institutions, and moves relational validation toward family members who participate in good faith.
% ABSENT_VOICES: Patients without family are structurally unrepresented by the model's own logic — they have no third leg to the triad. Patients whose wishes conflict with family are heard but institutionally outweighed once family dissent triggers additional review. Neither group has a seat in defining the safeguards; the safeguards are designed by clinicians and ethicists, not negotiated with dying patients.
% DISAPPEARANCE_RATIONALE: If the relational model vanished overnight in favor of pure patient autonomy, patients with clear capacity and clear wishes would face fewer delays, and families currently given standing to object would lose it — some genuine coercion-detection would be lost, and some genuine autonomy-restoration would occur. If it vanished in favor of sanctity-primary, all of the above decision authority collapses into prohibition. Whether the world 'rearranges' or 'stays the same' depends entirely on which counterfactual regime replaces it, which is exactly the kernel contest this constraint is one reading of.
% FOUNDING_PROBLEM: Pure patient autonomy models of end-of-life decision-making were found in practice to be vulnerable to coercion, depression-driven distortion of stated preference, and family/clinician exclusion that produced isolated, sometimes reversible-regret decisions; relational models were built to reintroduce accountable, situated judgment without reverting to blanket prohibition.
% FOUNDING_PROBLEM_CORROBORATION: Palliative care associations and bioethicists outside the direct clinical relationship attest the coercion/depression-distortion problem is real and ongoing (citing psychiatric literature on decisional capacity in terminal illness). Disability-rights advocates, also outside the benefiting clinical/family network, attest the founding problem is real but argue the relational model has been captured by family and institutional interests and now underserves isolated and dissenting patients — corroboration exists but the two outside sources disagree on whether the remedy has become the new problem.
narrative_ontology:disappearance_verdict(dignified_death__relational_autonomy, contested).
narrative_ontology:founding_problem_status(dignified_death__relational_autonomy, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(dignified_death__relational_autonomy, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(dignified_death__relational_autonomy, 'none', 1).
narrative_ontology:epsilon_provenance(dignified_death__relational_autonomy, 0.37, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness is authored in the moderate 0.30-0.45 band (ending at 0.37) reflecting genuine but non-dominant extraction: most triads converge without conflict, and the extraction is concentrated on the two payer subgroups (unrepresented patients, dissented-against patients) rather than broadly diffuse. Suppression (0.42) is moderate — the safeguards are coercive in the sense that they can override or delay a capacity-verified individual wish, but they are also bounded by procedural due process (ethics committee review, appeal), unlike a pure prohibition regime. Theater ratio (0.28) is kept modest: most of the procedural overhead does real coordination work (capacity review genuinely catches distortion), though a growing share over the interval reflects institutional liability-covering ritual as case law and malpractice exposure accumulate. Accessibility collapse (0.4) reflects that once a patient enters the process, exit to a purely autonomous decision is substantially but not completely foreclosed — appeal and second-opinion routes remain. Resistance (0.55) is fairly high because dissenting patients, disability advocates, and some clinicians actively contest the model's paternalistic edges.
 *
 * PERSPECTIVAL GAP:
 *   From the family-participant and clinician seats, the triad structure is straightforwardly coordinative — it protects everyone from making an irreversible decision alone under distorted conditions. From the isolated-patient and wish-conflicted-patient seats, the identical procedural machinery operates as extraction: their autonomy is nominally honored but practically subordinated to a relational architecture they cannot supply (no family) or that actively opposes them (family dissent). The engine should compute divergent per-seat types from this same structural data — that divergence is the point of the relational reading, not a flaw in it.
 *
 * DIRECTIONALITY LOGIC:
 *   Family decision participants and treating clinicians sit near the beneficiary end: they gain standing, protection from liability and guilt, and legitimacy from the process (low d). Isolated patients and wish-conflicted patients sit near the target end: they are structurally disadvantaged by a model built around a relational architecture they either lack or that opposes them, with no meaningful exit once engaged (trapped, high d). Excluded family members are a mixed case — usually protectively excluded, occasionally wrongly excluded — placed as excluded/constrained rather than as clean beneficiaries or clean payers.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (coercion and distortion vulnerability of pure-autonomy decision-making) remains partly live — psychiatric literature on decisional capacity under terminal illness still corroborates real distortion risk — which argues against calling this pure mandatrophy. But the founding_problem_status is authored as contested because disability-rights advocates, external to the benefiting clinical/family network, argue the remedy has itself become a mechanism that primarily protects institutions and families rather than patients, particularly the isolated and dissenting subgroups. This is exactly the kind of case the classification system is built to hold open rather than resolve by fiat: a coordination function that is real, layered with extraction that is also real, on the same procedural machinery.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    relational_context_as_precondition_or_pretext,
    'Is ''relational context'' a genuine epistemic requirement for detecting coercion and capacity distortion, or is it a pretext that lets institutions and families retain decision leverage over dying patients under the language of dignity?',
    'Comparative outcome studies between relational-triad jurisdictions and pure-autonomy jurisdictions on measures of documented coercion detection versus documented wrongful delay/override of capacity-verified patient wishes.',
    'If coercion-detection gains substantially outweigh wrongful-override harms, the rope reading is well-supported; if the reverse, the constraint drifts toward tangled_rope or snare for the two payer subgroups even while remaining rope-like for the majority of convergent cases.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(relational_context_as_precondition_or_pretext, empirical, 'Whether relational procedural overhead is protective epistemics or institutional leverage.').

omega_variable(
    kernel_reading_selection_pressure,
    'Why does relational_autonomy occupy the dominant institutional position among the three kernel readings (over autonomy_primary and sanctity_primary) in most contemporary end-of-life statutory frameworks?',
    'Historical and comparative-law analysis of which stakeholder coalitions (medical associations, disability-rights groups, religious institutions, patient-autonomy advocates) shaped the drafting of assisted-dying and end-of-life statutes in jurisdictions that adopted the triad model.',
    'If the dominant reading was selected primarily because it satisfies institutional liability-management and professional-body interests rather than patient interests, that reframes the ''coordination'' claim as partly a settlement among powerful non-patient stakeholders rather than a pure epistemic improvement over the sibling readings.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_selection_pressure, conceptual, 'Documents the committer structure: this constraint is one reading of a three-way kernel contest, and the reasons for its institutional dominance are themselves contested.').

omega_variable(
    isolated_patient_representation_gap,
    'Can the relational model be structurally amended to represent isolated patients without simply defaulting to a clinician-only (quasi-sanctity or quasi-paternalist) process, or is the representation gap for this subgroup irreducible to the triad architecture itself?',
    'Pilot programs using patient advocates or independent capacity-focused ombudspersons as a substitute third leg for isolated patients, measured against both convergent-triad and pure-autonomy outcomes.',
    'If a substitute representation mechanism closes the gap, the victim status of isolated patients is remediable within the rope reading; if not, isolated patients represent a structural, non-remediable extraction the relational model cannot avoid producing.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(isolated_patient_representation_gap, empirical, 'Whether isolated-patient disadvantage is a fixable implementation gap or an irreducible structural feature of triad-based decision authority.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dignified_death__relational_autonomy, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dign_tr_t0, dignified_death__relational_autonomy, theater_ratio, 0, 0.12).
narrative_ontology:measurement(dign_tr_t4, dignified_death__relational_autonomy, theater_ratio, 4, 0.16).
narrative_ontology:measurement(dign_tr_t8, dignified_death__relational_autonomy, theater_ratio, 8, 0.2).
narrative_ontology:measurement(dign_tr_t12, dignified_death__relational_autonomy, theater_ratio, 12, 0.23).
narrative_ontology:measurement(dign_tr_t16, dignified_death__relational_autonomy, theater_ratio, 16, 0.26).
narrative_ontology:measurement(dign_tr_t20, dignified_death__relational_autonomy, theater_ratio, 20, 0.28).

% Extraction over time
narrative_ontology:measurement(dign_be_t0, dignified_death__relational_autonomy, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(dign_be_t4, dignified_death__relational_autonomy, base_extractiveness, 4, 0.27).
narrative_ontology:measurement(dign_be_t8, dignified_death__relational_autonomy, base_extractiveness, 8, 0.31).
narrative_ontology:measurement(dign_be_t12, dignified_death__relational_autonomy, base_extractiveness, 12, 0.33).
narrative_ontology:measurement(dign_be_t16, dignified_death__relational_autonomy, base_extractiveness, 16, 0.35).
narrative_ontology:measurement(dign_be_t20, dignified_death__relational_autonomy, base_extractiveness, 20, 0.37).

% Suppression requirement over time
narrative_ontology:measurement(dign_su_t0, dignified_death__relational_autonomy, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(dign_su_t4, dignified_death__relational_autonomy, suppression_requirement, 4, 0.34).
narrative_ontology:measurement(dign_su_t8, dignified_death__relational_autonomy, suppression_requirement, 8, 0.37).
narrative_ontology:measurement(dign_su_t12, dignified_death__relational_autonomy, suppression_requirement, 12, 0.39).
narrative_ontology:measurement(dign_su_t16, dignified_death__relational_autonomy, suppression_requirement, 16, 0.41).
narrative_ontology:measurement(dign_su_t20, dignified_death__relational_autonomy, suppression_requirement, 20, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dignified_death__relational_autonomy, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(dignified_death__relational_autonomy, 0.12).
narrative_ontology:affects_constraint(dignified_death__relational_autonomy, dignified_death__autonomy_primary).
narrative_ontology:affects_constraint(dignified_death__relational_autonomy, dignified_death__sanctity_primary).

% DUAL FORMULATION NOTE:
% This story is one of three siblings decomposing the colloquial 'dignified death' / assisted-dying dignity debate per the epsilon-invariance principle: autonomy_primary (epsilon dominated by exclusion of family/clinician input, tangled_rope or snare depending on jurisdiction's coercion safeguards), relational_autonomy (this story; moderate epsilon 0.30-0.45, rope with tangled procedural overhead, victims = unrepresented/dissented-against patients), and sanctity_primary (epsilon dominated by blanket prohibition regardless of consent, likely snare or tangled_rope from the suffering patient's seat). Each has a distinct victim set and a distinct claimed type; they are linked here rather than merged because measuring 'dignity' three different ways produces three different epsilon values, which by the epsilon-invariance rule means three different constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
