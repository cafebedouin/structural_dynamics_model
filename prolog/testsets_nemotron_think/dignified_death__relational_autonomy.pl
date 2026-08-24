% ============================================================================
% CONSTRAINT STORY: dignified_death__relational_autonomy
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: dignified_death__relational_autonomy
 *   human_readable: Relational Autonomy Model for Dignified Death Decisions
 *   domain: bioethics/medical_law/political_philosophy
 *
 * SUMMARY:
 *   The relational autonomy model for dignified death decisions distributes
 *   authority across a patient-family-clinician triad with procedural
 *   safeguards. It emerged in the 1980s-90s as a response to clinician
 *   paternalism and the perceived inadequacy of pure autonomy models. The
 *   constraint is claimed as a rope — a genuine coordination problem
 *   (aligning three perspectives on a high-stakes, irreversible decision)
 *   with high procedural overhead. Extraction is moderate (0.38): the triad
 *   structure extracts compliance from unrepresented patients and overridden
 *   clinicians, but the coordination function is real and the safeguards
 *   serve a protective purpose. Suppression is moderate (0.42): the model's
 *   legal enforcement blocks both unilateral patient requests and clinician
 *   unilateral decisions, but alternatives (pure autonomy, sanctity) remain
 *   live in public discourse and some jurisdictions. Theater is low-moderate
 *   (0.28) and rising: early ethics committees were substantive deliberative
 *   bodies; later proceduralization has made some safeguards performative
 *   (checkbox capacity assessments, mandatory waiting periods that serve no
 *   epistemic function).
 *
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
narrative_ontology:constraint_metric(dignified_death__relational_autonomy, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(dignified_death__relational_autonomy, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dignified_death__relational_autonomy, rope).
narrative_ontology:human_readable(dignified_death__relational_autonomy, "Relational Autonomy Model for Dignified Death Decisions").
narrative_ontology:topic_domain(dignified_death__relational_autonomy, "bioethics/medical_law/political_philosophy").

domain_priors:requires_active_enforcement(dignified_death__relational_autonomy).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dignified_death__relational_autonomy, '9e6a6ea1-6f1b-4751-8d65-bdd6f92b9b20').
narrative_ontology:cs_kernel_codification('9e6a6ea1-6f1b-4751-8d65-bdd6f92b9b20', distributed).
narrative_ontology:cs_authority_grounding('9e6a6ea1-6f1b-4751-8d65-bdd6f92b9b20', practice).
narrative_ontology:cs_interpretation_layer_present('9e6a6ea1-6f1b-4751-8d65-bdd6f92b9b20').
narrative_ontology:cs_reading_relation('9e6a6ea1-6f1b-4751-8d65-bdd6f92b9b20', dignified_death__autonomy_primary, coexists_with).
narrative_ontology:cs_reading_relation('9e6a6ea1-6f1b-4751-8d65-bdd6f92b9b20', dignified_death__sanctity_primary, coexists_with).
narrative_ontology:cs_axiom('9e6a6ea1-6f1b-4751-8d65-bdd6f92b9b20', foundational, dignity_grounded_in_relational_context).
narrative_ontology:cs_axiom_status(dignity_grounded_in_relational_context, holdable).
narrative_ontology:cs_axiom_grounding('9e6a6ea1-6f1b-4751-8d65-bdd6f92b9b20', dignity_grounded_in_relational_context, deontological).
narrative_ontology:cs_axiom('9e6a6ea1-6f1b-4751-8d65-bdd6f92b9b20', secondary, decision_authority_distributed_across_triad).
narrative_ontology:cs_axiom_status(decision_authority_distributed_across_triad, holdable).
narrative_ontology:cs_axiom_grounding('9e6a6ea1-6f1b-4751-8d65-bdd6f92b9b20', decision_authority_distributed_across_triad, conventional).
narrative_ontology:cs_reference_frame('9e6a6ea1-6f1b-4751-8d65-bdd6f92b9b20', shared_decision_making_emergence_1980s).
narrative_ontology:cs_drift_state('9e6a6ea1-6f1b-4751-8d65-bdd6f92b9b20', contemporary_legalization_wave, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('9e6a6ea1-6f1b-4751-8d65-bdd6f92b9b20', '').
narrative_ontology:cs_kernel_id(dignified_death__relational_autonomy, dignified_death).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dignified_death__relational_autonomy, relational_network).
narrative_ontology:constraint_victim(dignified_death__relational_autonomy, unrepresented_patients).
narrative_ontology:constraint_victim(dignified_death__relational_autonomy, overridden_clinicians).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(dignified_death__relational_autonomy, patient_in_triad).
narrative_ontology:constraint_beneficiary(dignified_death__relational_autonomy, family_members).
narrative_ontology:constraint_beneficiary(dignified_death__relational_autonomy, treating_clinician).
narrative_ontology:constraint_victim(dignified_death__relational_autonomy, patient_in_triad).
narrative_ontology:constraint_victim(dignified_death__relational_autonomy, family_members).
narrative_ontology:constraint_vindicates(dignified_death__relational_autonomy, shared_decision_making_doctype).
narrative_ontology:constraint_vindicates(dignified_death__relational_autonomy, relational_personhood_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Participates in shared deliberation with family and clinicians; values and preferences are central but not determinative. May experience procedural safeguards as protective or as dilution of self-determination. Exit means refusing the triad process and demanding unilateral authority (often legally blocked) or accepting clinical paternalism.
narrative_ontology:constraint_stakeholder(dignified_death__relational_autonomy, patient_in_triad, beneficiary,
    moderate, biographical, constrained, local).
narrative_ontology:stakeholder_secondary_role(dignified_death__relational_autonomy, patient_in_triad, payer).

% Hold recognized standing in the deliberation; their relational knowledge and caregiving burden give them procedural weight. May bear emotional cost of decision-making and subsequent guilt or conflict. Exit means withdrawing from the process (risking patient isolation) or seeking legal guardianship to override.
narrative_ontology:constraint_stakeholder(dignified_death__relational_autonomy, family_members, beneficiary,
    moderate, biographical, constrained, local).
narrative_ontology:stakeholder_secondary_role(dignified_death__relational_autonomy, family_members, payer).

% Frames medical options, assesses capacity, and often chairs the deliberation; professional authority and liability exposure give structural agenda-setting power. Benefits from shared responsibility and legal safe harbor. Exit means refusing to participate in assisted dying (conscience clause) or transferring care — both professionally costly but feasible.
narrative_ontology:constraint_stakeholder(dignified_death__relational_autonomy, treating_clinician, agenda_setter,
    powerful, biographical, mobile, local).
narrative_ontology:stakeholder_secondary_role(dignified_death__relational_autonomy, treating_clinician, beneficiary).

% Designs and enforces procedural safeguards (capacity assessment, cooling-off periods, documentation requirements). Collects institutional legitimacy from managing the process. Exit is not applicable — the committee IS the enforcement structure.
narrative_ontology:constraint_stakeholder(dignified_death__relational_autonomy, ethics_committee, agenda_setter,
    institutional, generational, analytical, regional).

% Enacts statutory frameworks that authorize or prohibit the triad model; courts adjudicate disputes when the process breaks down. The legal framework's legitimacy depends on appearing to balance autonomy and protection. Exit means legislative repeal or judicial reinterpretation — slow, high-stakes, politically mediated.
narrative_ontology:constraint_stakeholder(dignified_death__relational_autonomy, legal_system, agenda_setter,
    institutional, generational, analytical, national).

% Patients without family or social network to form the triad; the model's procedural safeguards assume a relational context that does not exist for them. They face either exclusion from the process (decision defaults to clinicians/state) or simulated participation with appointed surrogates who lack relational knowledge. No meaningful exit — they are structurally invisible to the coordination mechanism.
narrative_ontology:constraint_stakeholder(dignified_death__relational_autonomy, unrepresented_patients, payer,
    powerless, immediate, trapped, local).

% Clinicians whose medical judgment is subordinated to the triad's consensus (e.g., when family insists on treatment clinician judges futile, or when patient requests hastened death clinician cannot support). Bear moral distress and liability risk without decisional authority. Exit means conscience objection (career-costly) or leaving the specialty.
narrative_ontology:constraint_stakeholder(dignified_death__relational_autonomy, overridden_clinicians, payer,
    moderate, biographical, constrained, local).

% Advocate for unilateral patient self-determination (advance directives, voluntary assisted dying on request). View the triad model as paternalistic dilution of autonomy. Excluded from the triad's internal deliberation but active in legislative and litigation challenges to procedural safeguards.
narrative_ontology:constraint_stakeholder(dignified_death__relational_autonomy, autonomy_primary_advocates, excluded,
    organized, generational, mobile, national).

% Oppose any intentional life-termination as intrinsically wrong. View the triad model as a procedural veneer for euthanasia. Excluded from the deliberation but politically potent in blocking legalization. Their exit would be total withdrawal from the healthcare system — not observed.
narrative_ontology:constraint_stakeholder(dignified_death__relational_autonomy, sanctity_primary_advocates, excluded,
    organized, civilizational, mobile, global).

% Analyze the triad model's coherence, empirical outcomes, and philosophical foundations. No material stake in individual decisions; professional reputation tied to the discourse. Exit is intellectual disengagement — always available.
narrative_ontology:constraint_stakeholder(dignified_death__relational_autonomy, bioethics_scholars, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates end-of-life decisions across three epistemically distinct perspectives — patient's values, family's relational knowledge and caregiving reality, clinician's medical expertise — through structured deliberation with procedural safeguards (capacity assessment, cooling-off periods, documentation, ethics review) that aim to prevent both premature death and unwanted prolongation of suffering.
% TRANSFER_FUNCTION: Moves final decision authority from unilateral patient choice (autonomy model) or clinical paternalism (sanctity model) to a negotiated triad consensus. The transfer is not purely financial — it moves moral responsibility, legal liability, emotional burden, and epistemic authority across the three seats. The relational network (functioning triad) receives the benefit of shared legitimacy; unrepresented patients and overridden clinicians bear the cost of structural misfit.
% ABSENT_VOICES: Unrepresented patients (no family to complete the triad) are structurally silenced by the model's relational premise. Autonomy-primary advocates (patient as sole authority) and sanctity-primary advocates (life as inviolable) are excluded from the triad's internal deliberation but contest it externally through litigation and legislation. Dissenting family members whose views are overridden by consensus are present but not heard — the process records consensus, not dissent.
% DISAPPEARANCE_RATIONALE: If the triad model vanished overnight, jurisdictions would revert to either pure autonomy frameworks (patient-alone decision, as in some assisted dying laws) or clinical/legal paternalism (clinician or court decides, as in traditional medical ethics), or legislative prohibition (sanctity model). The coordination vacuum would be filled by one of the sibling readings — the world would rearrange around a different authority structure.
% FOUNDING_PROBLEM: Mid-20th century medicalization of death created a crisis: clinicians held de facto life-ending authority without accountability; patients had no recognized voice; families were excluded from decisions about their dying relatives. The triad model was built to solve the legitimacy vacuum by distributing authority across the three parties who actually live with the consequences.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem is attested by bioethics historians (Jonsen, Siegler, Winslade) and early clinical ethics committee records from outside the benefiting clinical establishment. Autonomy-primary advocates corroborate that clinician paternalism was the dominant problem but argue the triad over-corrected. Sanctity-primary advocates dispute the problem framing entirely, asserting the crisis was manufactured to legitimize killing. No single corroborator outside all three readings exists — the genealogy itself is contested terrain.
narrative_ontology:disappearance_verdict(dignified_death__relational_autonomy, world_rearranges).
narrative_ontology:founding_problem_status(dignified_death__relational_autonomy, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(dignified_death__relational_autonomy, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(dignified_death__relational_autonomy, 'none', 1).
narrative_ontology:epsilon_provenance(dignified_death__relational_autonomy, 0.38, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Extractiveness 0.38 reflects that the triad's procedural overhead (ethics review, cooling-off periods, documentation) imposes real costs — time, emotional labor, foregone autonomy — that fall disproportionately on unrepresented patients and clinicians whose judgment is subordinated. But the coordination function is genuine: without structured deliberation, the three perspectives cannot be integrated, leading to either unilateral decisions or paralyzed conflict. Suppression 0.42 reflects that the model actively prevents both pure autonomy and pure sanctity arrangements from operating where it is law, but does not eliminate them as live alternatives elsewhere. Theater 0.28 captures the drift from substantive ethics consultation to procedural compliance theater. The claim (rope) and metrics are independently authored — the engine will compute per-seat types from the structural data.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats (unrepresented_patients, overridden_clinicians) experience the constraint as a snare: extraction without coordination benefit, no meaningful exit. The beneficiary seats (patient_in_triad, family_members) experience it as a rope: genuine coordination with shared costs. The agenda_setter seats (clinician, ethics_committee, legal_system) experience it as a scaffold: transitional coordination meant to stabilize a domain in flux, but with no sunset clause — the 'temporary' procedural framework has become permanent. The engine will compute this divergence from the structural data; the authored claim (rope) reflects the coordinator's self-understanding, not the victims' experience.
 *
 * DIRECTIONALITY LOGIC:
 *   The relational_network (functioning triad) is the structural beneficiary: it collects the legitimacy and risk-distribution benefits of shared decision-making. Unrepresented_patients are full targets (d ≈ 0.9): the model's relational premise excludes them by design, and they have no exit — trapped. Overridden_clinicians are high targets (d ≈ 0.75): they bear moral distress and liability without authority, exit is constrained (conscience objection is career-costly). Patient_in_triad and family_members sit near symmetric (d ≈ 0.5): they gain protection and relational integrity but cede unilateral authority. Treating_clinician, ethics_committee, and legal_system are agenda_setters with beneficiary subsidy (d ≈ 0.15-0.25): they control the process and gain institutional legitimacy. Autonomy_primary_advocates and sanctity_primary_advocates are excluded (d not computed — they are not governed by the constraint but contest its legitimacy).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (clinician paternalism, patient voicelessness) is contested as live/dead. If dead (autonomy-primary view: advance directives and legal reforms solved it), the triad persists as mandatrophy — a coordination structure whose function has atrophied but whose procedural overhead remains. If live (relational view: new medical technologies create novel decision contexts), the constraint remains a rope. If the problem was never the true driver (sanctity view: the triad was always a Trojan horse for euthanasia), it is a snare from inception. The classification prevents mislabeling by forcing the structural question: who benefits from the triad's persistence, and does the coordination function still match the problem?
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    committer_structure_ambiguity,
    'Is the relational_autonomy reading a genuine coordination structure, or does it function as a procedural veneer for expanding assisted dying access (as sanctity_primary alleges) or as a paternalistic barrier to autonomy (as autonomy_primary alleges)?',
    'Compare jurisdictions with triad models vs. pure autonomy models on: (a) rate of assisted dying requests granted, (b) rate of unrepresented patients receiving default life-prolongation, (c) clinician moral distress scores. If triad jurisdictions show higher grant rates than autonomy jurisdictions, the veneer hypothesis gains support; if they show lower unrepresented-patient harm, the coordination hypothesis gains support.',
    'If veneer: reclassify as snare (coordination story is cover for extraction toward death). If paternalistic barrier: reclassify as tangled_rope (genuine coordination + asymmetric extraction against autonomy). If genuine coordination: rope stands.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(committer_structure_ambiguity, conceptual, 'Whether the triad model''s coordination function is authentic or instrumental to a sibling reading''s agenda.').

omega_variable(
    procedural_safeguard_authenticity,
    'Do the procedural safeguards (capacity assessment, cooling-off periods, ethics review) perform genuine epistemic work, or have they become performative compliance theater that extracts time and emotional labor without improving decision quality?',
    'Empirical study of safeguard outcomes: do cooling-off periods change decisions? Do ethics committee reviews alter clinical plans? Do capacity assessments using standardized tools correlate with independent psychiatric evaluation? Measure decision regret at 6/12 months across jurisdictions with varying safeguard intensity.',
    'If performative: theater_ratio is understated; constraint drifts toward piton. If genuine: rope classification holds; extraction is the price of coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(procedural_safeguard_authenticity, empirical, 'Whether the constraint''s rising theater_ratio reflects real functional decay or measurement artifact.').

omega_variable(
    unrepresented_patient_exclusion_mechanism,
    'Is the exclusion of unrepresented patients a necessary consequence of the relational premise (no relations = no triad), or a design choice that could be remedied by appointed relational proxies (e.g., patient advocates, community representatives)?',
    'Survey jurisdictions with triad models: how are unrepresented patients handled? Compare outcomes (treatment intensity, place of death, family satisfaction proxy) for unrepresented patients under triad vs. autonomy vs. sanctity frameworks. Test whether appointed proxies improve alignment with inferred patient values.',
    'If remediable design choice: the victim set is contingent, not structural — constraint could be reformed without abandoning relational premise. If necessary consequence: the relational premise itself generates structural victims, supporting tangled_rope or snare classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(unrepresented_patient_exclusion_mechanism, empirical, 'Whether the triad''s structural exclusion of the relationally isolated is fixable within the model.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dignified_death__relational_autonomy, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dignified_death_relational_autonomy_tr_t0, dignified_death__relational_autonomy, theater_ratio, 0, 0.15).
narrative_ontology:measurement(dignified_death_relational_autonomy_tr_t10, dignified_death__relational_autonomy, theater_ratio, 10, 0.2).
narrative_ontology:measurement(dignified_death_relational_autonomy_tr_t20, dignified_death__relational_autonomy, theater_ratio, 20, 0.25).
narrative_ontology:measurement(dignified_death_relational_autonomy_tr_t30, dignified_death__relational_autonomy, theater_ratio, 30, 0.28).
narrative_ontology:measurement(dignified_death_relational_autonomy_tr_t40, dignified_death__relational_autonomy, theater_ratio, 40, 0.28).

% Extraction over time
narrative_ontology:measurement(dignified_death_relational_autonomy_be_t0, dignified_death__relational_autonomy, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(dignified_death_relational_autonomy_be_t10, dignified_death__relational_autonomy, base_extractiveness, 10, 0.32).
narrative_ontology:measurement(dignified_death_relational_autonomy_be_t20, dignified_death__relational_autonomy, base_extractiveness, 20, 0.36).
narrative_ontology:measurement(dignified_death_relational_autonomy_be_t30, dignified_death__relational_autonomy, base_extractiveness, 30, 0.38).
narrative_ontology:measurement(dignified_death_relational_autonomy_be_t40, dignified_death__relational_autonomy, base_extractiveness, 40, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(dignified_death_relational_autonomy_su_t0, dignified_death__relational_autonomy, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(dignified_death_relational_autonomy_su_t10, dignified_death__relational_autonomy, suppression_requirement, 10, 0.4).
narrative_ontology:measurement(dignified_death_relational_autonomy_su_t20, dignified_death__relational_autonomy, suppression_requirement, 20, 0.42).
narrative_ontology:measurement(dignified_death_relational_autonomy_su_t30, dignified_death__relational_autonomy, suppression_requirement, 30, 0.42).
narrative_ontology:measurement(dignified_death_relational_autonomy_su_t40, dignified_death__relational_autonomy, suppression_requirement, 40, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dignified_death__relational_autonomy, identity_coordination).
narrative_ontology:boltzmann_floor_override(dignified_death__relational_autonomy, 0.08).
narrative_ontology:affects_constraint(dignified_death__relational_autonomy, dignified_death__autonomy_primary).
narrative_ontology:affects_constraint(dignified_death__relational_autonomy, dignified_death__sanctity_primary).

% DUAL FORMULATION NOTE:
% The dignified_death kernel decomposes into three constraint stories: autonomy_primary (patient-final-authority), relational_autonomy (this story — triad deliberation), sanctity_primary (life-inviolable). Each reading instantiates a distinct constraint with different beneficiary/victim structures and ε values. They form a constraint family linked by shared kernel_id. The relational_autonomy reading coordinates identity (what kind of death counts as dignified) across the triad; the autonomy_primary reading coordinates individual preference; the sanctity_primary reading coordinates adherence to transcendent norm. All three claim to solve the founding problem of medicalized death but disagree on what the problem IS.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(dignified_death__relational_autonomy, powerful, 0.3).
constraint_indexing:directionality_override(dignified_death__relational_autonomy, moderate, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
