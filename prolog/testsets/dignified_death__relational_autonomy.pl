% ============================================================================
% CONSTRAINT STORY: dignified_death__relational_autonomy
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: dignified_death__relational_autonomy
 *   human_readable: Relational Autonomy in End-of-Life Decision Authority
 *   domain: bioethics/medical_law/political_philosophy
 *
 * SUMMARY:
 *   End-of-life decision authority is a contested kernel instantiated through
 *   three incompatible readings: autonomy-primary (patient unilateral
 *   control), relational-autonomy (distributed triad with procedural
 *   safeguards), and sanctity-primary (absolute prohibition on intentional
 *   termination). THIS STORY instantiates ONLY the relational-autonomy
 *   reading as a clean, ε-invariant constraint. The relational-autonomy frame
 *   distributes decision authority across patient, family, and clinician with
 *   procedural requirements for consultation and professional gatekeeping. It
 *   is claimed as rope (genuine coordination of conflicting legitimate
 *   claims) and authored with moderate extractiveness (0.38) reflecting the
 *   procedural overhead and loss of unilateral authority. The constraint is
 *   NOT claimed as coordination of all parties across the kernel dispute —
 *   the excluded readings (autonomy-primary and sanctity-primary) are
 *   siblings, not integrated into this frame. The coordination being solved
 *   is WITHIN the relational-autonomy model: how to respect patient agency,
 *   relational bonds, and clinical judgment simultaneously.
 *
 * KEY AGENTS:
 *   - Patient with terminal illness: the party whose death and timing is at stake; holds co-decision authority within the triad but not unilateral control; faces procedural delay and loss of sole autonomy.
 *   - Family network: holds co-decision authority and represents relational values and continuity with the patient's life narrative; bears emotional cost and ethical responsibility.
 *   - Clinician team: holds gatekeeping authority (diagnosis verification, capacity assessment) and co-decision authority; enforces procedural safeguards and professional ethics standards.
 *   - Regulatory and legal authority: enforces the relational-autonomy frame through legislation and professional licensing; settles disputes and hears appeals.
 *   - Excluded autonomy-primary readers: object that procedural safeguards violate absolute patient self-determination; hold formal voice in legislative/professional discourse but no authority in the relational system.
 *   - Excluded sanctity-primary readers: object to intentional termination regardless of consent; hold formal voice in legislative/professional discourse but no authority in the relational system.
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
narrative_ontology:constraint_metric(dignified_death__relational_autonomy, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(dignified_death__relational_autonomy, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dignified_death__relational_autonomy, rope).
narrative_ontology:human_readable(dignified_death__relational_autonomy, "Relational Autonomy in End-of-Life Decision Authority").
narrative_ontology:topic_domain(dignified_death__relational_autonomy, "bioethics/medical_law/political_philosophy").

domain_priors:requires_active_enforcement(dignified_death__relational_autonomy).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dignified_death__relational_autonomy, '2f5a1397-1a01-4f9b-8323-5babf0547c6b').
narrative_ontology:cs_kernel_codification('2f5a1397-1a01-4f9b-8323-5babf0547c6b', fixed_text).
narrative_ontology:cs_authority_grounding('2f5a1397-1a01-4f9b-8323-5babf0547c6b', lineage).
narrative_ontology:cs_interpretation_layer_present('2f5a1397-1a01-4f9b-8323-5babf0547c6b').
narrative_ontology:cs_reading_relation('2f5a1397-1a01-4f9b-8323-5babf0547c6b', dignified_death__autonomy_primary, coexists_with).
narrative_ontology:cs_reading_relation('2f5a1397-1a01-4f9b-8323-5babf0547c6b', dignified_death__sanctity_primary, coexists_with).
narrative_ontology:cs_axiom('2f5a1397-1a01-4f9b-8323-5babf0547c6b', foundational, dignity_emerges_from_relational_context).
narrative_ontology:cs_axiom_status(dignity_emerges_from_relational_context, holdable).
narrative_ontology:cs_axiom_grounding('2f5a1397-1a01-4f9b-8323-5babf0547c6b', dignity_emerges_from_relational_context, deontological).
narrative_ontology:cs_axiom('2f5a1397-1a01-4f9b-8323-5babf0547c6b', foundational, distributed_decision_authority_respects_multiple_legitimate_claims).
narrative_ontology:cs_axiom_status(distributed_decision_authority_respects_multiple_legitimate_claims, holdable).
narrative_ontology:cs_axiom_grounding('2f5a1397-1a01-4f9b-8323-5babf0547c6b', distributed_decision_authority_respects_multiple_legitimate_claims, deontological).
narrative_ontology:cs_reference_frame('2f5a1397-1a01-4f9b-8323-5babf0547c6b', patient_relational_dignity).
narrative_ontology:cs_drift_state('2f5a1397-1a01-4f9b-8323-5babf0547c6b', contemporary_bioethics_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('2f5a1397-1a01-4f9b-8323-5babf0547c6b', '').
narrative_ontology:cs_kernel_id(dignified_death__relational_autonomy, dignified_death).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dignified_death__relational_autonomy, relational_network).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(dignified_death__relational_autonomy, patient_with_terminal_illness).
narrative_ontology:constraint_beneficiary(dignified_death__relational_autonomy, family_network).
narrative_ontology:constraint_victim(dignified_death__relational_autonomy, patient_with_terminal_illness).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Faces imminent death and seeks to control the timing and method. In the relational-autonomy frame, they are subject to procedural safeguards requiring consultation with family and clinicians, creating delay and loss of sole authority. They retain participatory authority within the triad rather than unilateral control. Exit (choosing unilateral action outside the system) is identity-locked because the relational framing has become their self-concept.
narrative_ontology:constraint_stakeholder(dignified_death__relational_autonomy, patient_with_terminal_illness, payer,
    powerless, immediate, identity_locked, local).
narrative_ontology:stakeholder_secondary_role(dignified_death__relational_autonomy, patient_with_terminal_illness, beneficiary).

% Holds co-decision authority with the patient and clinician. They advocate for values continuity with the patient's life narrative and relational bonds. The procedural safeguards require their voice but also constrain unilateral action by any single party. They bear emotional and ethical costs of participation in the decision.
narrative_ontology:constraint_stakeholder(dignified_death__relational_autonomy, family_network, agenda_setter,
    moderate, biographical, constrained, local).
narrative_ontology:stakeholder_secondary_role(dignified_death__relational_autonomy, family_network, beneficiary).

% Holds gatekeeping and co-decision authority: must verify terminal diagnosis, assess patient capacity, facilitate consultation, and can decline participation on conscience grounds. The procedural architecture requires their participation and expertise, making them enforcers of the relational-autonomy frame. They bear professional liability and conscience burden.
narrative_ontology:constraint_stakeholder(dignified_death__relational_autonomy, clinician_team, agenda_setter,
    institutional, generational, constrained, regional).

% Parties who read dignity primarily through individual self-determination and object to the procedural requirements as violations of absolute autonomy. They are structurally outside the decision triad and would argue for unilateral patient authority. Legislative testimony and bioethics literature record their voice, but they do not hold formal authority in the relational-autonomy system.
narrative_ontology:constraint_stakeholder(dignified_death__relational_autonomy, excluded_individual_autonomy_readers, excluded,
    organized, generational, constrained, national).

% Parties who read dignity as inherent in life itself and object to intentional termination regardless of consent. They object to the entire constraint, not merely its procedural form. They are structurally outside the decision framework and would argue for absolute prohibition. Religious communities and some bioethics traditions hold this position.
narrative_ontology:constraint_stakeholder(dignified_death__relational_autonomy, excluded_sanctity_readers, excluded,
    organized, civilizational, constrained, national).

% Enforces the relational-autonomy frame through legislation, professional licensing standards, and case law. They adjudicate disputes within the triad and hear appeals from excluded parties. Their enforcement maintains the procedural safeguards and the distributed authority model.
narrative_ontology:constraint_stakeholder(dignified_death__relational_autonomy, regulatory_and_legal_authority, observer,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Resolves a genuine collective-action problem: end-of-life decisions affect the patient's body, the family's relational bonds, the clinician's professional ethics, and the social legitimacy of medical practice. A purely individual decision excludes relational stakeholders; a purely professional decision excludes patient agency; a purely familial decision excludes clinical judgment of facts. The relational-autonomy frame coordinates these legitimate but conflicting claims through structured procedures requiring consultation across all three seats.
% TRANSFER_FUNCTION: Moves decision authority away from the patient's unilateral control and toward a distributed triad (patient + family + clinician). The patient transfers sole authority to shared authority; the family gains formal voice in a decision that affects relational bonds; the clinician gains gatekeeping power over medical judgment. What flows is procedural authority and the legitimacy to participate.
% ABSENT_VOICES: Readers who hold individual autonomy as primary dignity claim (the autonomy-primary reading) and readers who hold sanctity-of-life as primary dignity claim (the sanctity-primary reading) are structurally excluded. They would argue that the relational frame violates either absolute patient self-determination or absolute prohibition on intentional termination. Testimony from both traditions appears in legislative and bioethics discourse but does not hold formal authority in the relational-autonomy system.
% DISAPPEARANCE_RATIONALE: If the relational-autonomy constraint and its procedural safeguards disappeared, end-of-life decision authority would consolidate either in individual patient choice (if autonomy-primary replaced it) or in absolute prohibition (if sanctity-primary replaced it) or in clinician discretion without patient/family safeguards. The current relational frame structures how all three seats participate; its disappearance would reorganize medical practice, family involvement, and individual agency in end-of-life decisions.
% FOUNDING_PROBLEM: Early autonomy-centered models (patient unilateral control) and early medical-paternalism models (clinician unilateral control) both created harms: unilateral patient decisions could be made under duress or cognitive decline; unilateral clinician decisions could ignore patient and family values. The relational-autonomy frame emerged to coordinate legitimate but conflicting claims: respecting the patient's agency without severing relational bonds, respecting family relational stakes without giving them veto power, respecting clinician ethical judgment without allowing clinical gatekeeping to become paternalism.
% FOUNDING_PROBLEM_CORROBORATION: Bioethicists outside the relational tradition, clinicians implementing the framework, and family members in end-of-life consultations attest that the coordination problem is real and that the relational model addresses genuine stakeholder claims. Jurisdictions implementing relational-autonomy models (Canada, most European countries) report reduced litigation and family-clinician conflict compared to unilateral autonomy models. The sanctity-tradition objects that the founding problem was wrongly framed (that individual suffering does not override transcendent law), and autonomy-readers object that the solution violates absolute patient authority — both positions are documented in legislative and professional record.
narrative_ontology:disappearance_verdict(dignified_death__relational_autonomy, world_rearranges).
narrative_ontology:founding_problem_status(dignified_death__relational_autonomy, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(dignified_death__relational_autonomy, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(dignified_death__relational_autonomy, 'none', 1).

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
 *   Extractiveness is moderate (0.38 at endpoint) because the constraint genuinely solves a coordination problem (multiple legitimate claims on end-of-life decisions) but does extract unilateral authority from the patient and imposes procedural delays. Suppression is slightly lower (0.42) because the constraint is maintained through professional regulation and legal enforcement rather than coercion or economic dependence — clinicians follow it through licensing incentives and ethical training, families participate through relational bonds, patients consent (though not unilaterally) through the procedure. Theater ratio is moderate-low (0.28) because the procedural safeguards serve a real function (ensuring all three stakeholders' claims are heard) but a portion of the activity is performative legitimation of the constraint rather than functional deliberation. Accessibility collapse is moderate (0.65) because alternatives to the relational frame (unilateral autonomy, unilateral prohibition) are culturally and legally available in some jurisdictions, so alternatives have not fully collapsed; but within jurisdictions that have adopted relational-autonomy models, alternatives are partly erased from the legitimate option set. Resistance is substantial (0.58) because both excluded readings mount active resistance: autonomy-primary advocates litigate and organize legislative campaigns for unilateral patient authority; sanctity-primary traditions refuse to participate and advocate for absolute prohibition. The measurement series tracks a slight upward drift in extractiveness and suppression over the interval, suggesting increasing procedural formalization and stronger legal enforcement, then stabilization as the framework matures.
 *
 * PERSPECTIVAL GAP:
 *   The patient seat experiences the constraint as loss of unilateral authority and procedural delay; the family seat experiences it as co-decision authority they would not otherwise have; the clinician seat experiences it as gatekeeping responsibility and professional liability. The patient may compute the constraint as closer to snare (extraction of authority) while the family computes it as rope (coordination that gives them voice) and the clinician computes it as tangled rope (coordination of legitimate claims but with enforced participation). The engine computes each seat's type from the structural data; the patient's d is higher (toward target) because they lose unilateral control, the family's d is lower (closer to beneficiary) because they gain co-authority, and the clinician's d is symmetric (costs of gatekeeping balanced by professional authority). These divergent types are generated by the same constraint from different structural positions — that divergence is the measurement the corpus captures.
 *
 * DIRECTIONALITY LOGIC:
 *   The patient is the primary target of the constraint: they hold unilateral authority in the autonomy-primary frame and absolute prohibition in the sanctity-primary frame, but the relational-autonomy frame requires them to share authority. Their directionality (d) is high (toward 1.0, full target) because they lose exclusive control. However, the patient is listed as secondary beneficiary because they do benefit from the safeguards against their own cognitive decline and from family support and relational continuity — the relational frame prevents isolation and protects against decisions made under duress. The family network is the primary beneficiary: they gain formal co-decision authority and voice for relational values they would otherwise lack. Their directionality (d) is lower (closer to 0.0, beneficiary) because the constraint gives them authority. The clinician team is the secondary agenda-setter and partial beneficiary: they gain gatekeeping authority and professional liability protection through procedural requirements, but they also bear burden of enforcement and conscience-conflict. The excluded readers (autonomy-primary and sanctity-primary) are not stakeholders in the relational-autonomy frame — they are external positions advocating against the constraint itself.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's founding problem (unilateral autonomy harms / medical paternalism harms) remains live and is attested by clinicians and bioethicists outside the relational tradition. The constraint has not decayed into performance — the procedural safeguards serve a real coordination function, not theatrical legitimation. The theater ratio is moderate (0.28) not because the constraint is mostly performance, but because some portion of the activity is legitimation activity (formal consultations, documented deliberation) that serves legal and ethical purposes beyond functional decision-making. Mandatrophy is NOT present: the founding problem is not dead (end-of-life decisions still create coordination problems) and the constraint remains functional (the procedural framework continues to resolve conflicts). However, a future scenario in which the founding problem shifted would trigger mandatrophy: if technology enabled unambiguous capacity assessment and eliminated family-clinician conflict, or if a new reading consolidated all parties around a different dignity principle, the relational-autonomy frame could become zombie institutional theater.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    relational_frame_reading_contest,
    'Is dignity fundamentally relational (emerging from context and relationships) or fundamentally individual (residing in autonomous self-determination) or fundamentally transcendent (grounded in life''s intrinsic value)? Which reading of the dignified_death kernel is structurally correct?',
    'This is a preference and conceptual question, not empirical. Different philosophical frameworks and religious traditions hold different readings as foundational. The question is resolved only by choosing which framework grounds legitimacy — it cannot be settled by data. Jurisdictions adopting different readings will instantiate different constraints (autonomy-primary or relational-autonomy or sanctity-primary), each internally coherent, and the engine measures each reading separately as a constraint.',
    'If autonomy-primary is adopted: extractiveness drops (no family/clinician authority constrains the patient), suppression rises (legal barriers prevent clinician gatekeeping), constraint type shifts toward snare (patients in weak positions are exploited). If sanctity-primary is adopted: the entire constraint disappears and is replaced with absolute prohibition; extractiveness becomes zero and suppression becomes maximum (all intentional termination is prohibited). The relational-autonomy reading stands if dignity is held to be relational and coordination across legitimate claims is held to be legitimate.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(relational_frame_reading_contest, preference, 'Kernel-level disagreement about which reading grounds dignity correctly.').

omega_variable(
    procedural_overhead_as_extraction,
    'Is the procedural delay and loss of unilateral patient authority a necessary cost of coordination (rope function) or an extractive layer riding on a simpler decision problem (snare cover)?',
    'Comparative analysis: do patients in autonomy-primary jurisdictions (unilateral authority, minimal procedure) report better outcomes (dignity, autonomy satisfaction) than patients in relational-autonomy jurisdictions (distributed authority, procedural safeguards)? Do clinicians and families report lower conflict and better relational continuity in relational-autonomy systems?',
    'If the procedural overhead is necessary coordination: the constraint is rope and the measured extractiveness (0.38) is the price of solving a real coordination problem. If the overhead is extractive cover: the constraint is snare and the relational-autonomy reading is a false-summit framework that benefits clinicians and family at patient expense.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(procedural_overhead_as_extraction, empirical, 'Whether procedural safeguards are necessary coordination cost or extractive overhead.').

omega_variable(
    suppression_mechanism_internalized_vs_structural,
    'Is the measured suppression (0.42) structural (legal/professional barriers prevent unilateral action by any seat) or internalized (patients and families have internalized the relational dignity frame and suppress their own autonomy claims)?',
    'Post-legalization comparative study: in jurisdictions where relational-autonomy moves from informal ethics to legal requirement, does resistance increase initially (structural suppression visible) or does it plateau because participants have internalized the frame?',
    'If suppression is structural: the constraint relies on external enforcement (legal barriers, professional licensing) and could be rapidly destabilized by legal change. If suppression is internalized: participants have fused their identity with the relational frame; exit would require not just legal change but narrative reframing of dignity itself. Internalized suppression is more stable but also more extractive (agents carry the constraint with them after formal removal).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalized_vs_structural, empirical, 'Structural vs. internalized suppression mechanism in relational-autonomy frame.').

omega_variable(
    family_coercion_boundary,
    'At what point does family co-decision authority become family coercion of the patient''s wishes? Where is the boundary between relational legitimacy and coercive family pressure?',
    'Case-by-case analysis of end-of-life consultations: when does the engine register family influence as legitimate relational input vs. coercive distortion of patient autonomy? What structural differences (power asymmetry, information access, timing pressure) distinguish the two?',
    'If family co-authority frequently becomes family coercion: the constraint is partially snare for the patient (extracting autonomy in the name of relational coordination). If family authority remains legitimately consensual: the constraint is rope (genuine coordination). The relational-autonomy frame assumes the boundary can be maintained through procedural safeguards (capacity assessment, private patient-clinician consultation); if safeguards fail in practice, the constraint drifts toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(family_coercion_boundary, empirical, 'Family co-decision authority vs. coercive family pressure boundary.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dignified_death__relational_autonomy, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dign_tr_t0, dignified_death__relational_autonomy, theater_ratio, 0, 0.22).
narrative_ontology:measurement_basis(dign_tr_t0, observed).
narrative_ontology:measurement(dign_tr_t8, dignified_death__relational_autonomy, theater_ratio, 8, 0.24).
narrative_ontology:measurement_basis(dign_tr_t8, observed).
narrative_ontology:measurement(dign_tr_t16, dignified_death__relational_autonomy, theater_ratio, 16, 0.27).
narrative_ontology:measurement_basis(dign_tr_t16, observed).
narrative_ontology:measurement(dign_tr_t24, dignified_death__relational_autonomy, theater_ratio, 24, 0.29).
narrative_ontology:measurement_basis(dign_tr_t24, observed).
narrative_ontology:measurement(dign_tr_t32, dignified_death__relational_autonomy, theater_ratio, 32, 0.28).
narrative_ontology:measurement_basis(dign_tr_t32, observed).
narrative_ontology:measurement(dign_tr_t40, dignified_death__relational_autonomy, theater_ratio, 40, 0.28).
narrative_ontology:measurement_basis(dign_tr_t40, observed).

% Extraction over time
narrative_ontology:measurement(dign_be_t0, dignified_death__relational_autonomy, base_extractiveness, 0, 0.32).
narrative_ontology:measurement_basis(dign_be_t0, observed).
narrative_ontology:measurement(dign_be_t8, dignified_death__relational_autonomy, base_extractiveness, 8, 0.35).
narrative_ontology:measurement_basis(dign_be_t8, observed).
narrative_ontology:measurement(dign_be_t16, dignified_death__relational_autonomy, base_extractiveness, 16, 0.37).
narrative_ontology:measurement_basis(dign_be_t16, observed).
narrative_ontology:measurement(dign_be_t24, dignified_death__relational_autonomy, base_extractiveness, 24, 0.39).
narrative_ontology:measurement_basis(dign_be_t24, observed).
narrative_ontology:measurement(dign_be_t32, dignified_death__relational_autonomy, base_extractiveness, 32, 0.38).
narrative_ontology:measurement_basis(dign_be_t32, observed).
narrative_ontology:measurement(dign_be_t40, dignified_death__relational_autonomy, base_extractiveness, 40, 0.38).
narrative_ontology:measurement_basis(dign_be_t40, observed).

% Suppression requirement over time
narrative_ontology:measurement(dign_su_t0, dignified_death__relational_autonomy, suppression_requirement, 0, 0.38).
narrative_ontology:measurement_basis(dign_su_t0, observed).
narrative_ontology:measurement(dign_su_t8, dignified_death__relational_autonomy, suppression_requirement, 8, 0.4).
narrative_ontology:measurement_basis(dign_su_t8, observed).
narrative_ontology:measurement(dign_su_t16, dignified_death__relational_autonomy, suppression_requirement, 16, 0.42).
narrative_ontology:measurement_basis(dign_su_t16, observed).
narrative_ontology:measurement(dign_su_t24, dignified_death__relational_autonomy, suppression_requirement, 24, 0.44).
narrative_ontology:measurement_basis(dign_su_t24, observed).
narrative_ontology:measurement(dign_su_t32, dignified_death__relational_autonomy, suppression_requirement, 32, 0.43).
narrative_ontology:measurement_basis(dign_su_t32, observed).
narrative_ontology:measurement(dign_su_t40, dignified_death__relational_autonomy, suppression_requirement, 40, 0.42).
narrative_ontology:measurement_basis(dign_su_t40, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dignified_death__relational_autonomy, attachment_coordination).
narrative_ontology:boltzmann_floor_override(dignified_death__relational_autonomy, 0.12).
narrative_ontology:affects_constraint(dignified_death__relational_autonomy, dignified_death__autonomy_primary).
narrative_ontology:affects_constraint(dignified_death__relational_autonomy, dignified_death__sanctity_primary).

% DUAL FORMULATION NOTE:
% The dignified_death kernel is instantiated through three structurally distinct readings: autonomy-primary (patient unilateral authority, minimal procedural overhead, ε ~0.10-0.20, type snare-immune or rope-light), relational-autonomy (distributed triad authority with procedural safeguards, ε ~0.30-0.45, type rope), and sanctity-primary (absolute prohibition, ε ~0.0, constraint is replaced not modified). These are NOT the same constraint viewed from different perspectives; they are different constraints because they have different ε values, different beneficiary/victim structures, and different coordination functions. The relational-autonomy reading (this file) has moderate extractiveness because it solves genuine coordination but imposes procedural overhead. The autonomy-primary reading (sibling) has low extractiveness because it minimizes procedural burden but leaves the patient unprotected. The sanctity-primary reading (sibling) has zero extractiveness because it does not permit the decision at all — it is a replacement constraint, not a variant. Each reading is authored separately with its own ε-invariant claim; they are linked through network.affects_constraints to show structural influence. The readings are related through the commitment-system structure: they share a common kernel (dignified_death) but instantiate incompatible readings of what dignity means and therefore what decision authority should look like.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
