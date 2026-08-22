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
 *   human_readable: Relational Autonomy Decision Framework for Dignified Death
 *   domain: bioethics/medical_law/political_philosophy
 *
 * SUMMARY:
 *   This constraint instantiates the relational_autonomy reading of the
 *   dignified_death kernel. Dignity is understood as emerging from the
 *   patient's relational context — family bonds, clinical relationship,
 *   communal values — rather than from individual sovereignty alone or from
 *   life's intrinsic sanctity. Decision authority is distributed across a
 *   patient-family-clinician triad with procedural safeguards (ethics
 *   committee review, cooling-off periods, capacity assessment). The
 *   framework presents itself as a coordination solution to the legitimacy
 *   vacuum in end-of-life decisions.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dignified_death__relational_autonomy, 0.38).
domain_priors:suppression_score(dignified_death__relational_autonomy, 0.25).
domain_priors:theater_ratio(dignified_death__relational_autonomy, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dignified_death__relational_autonomy, extractiveness, 0.38).
narrative_ontology:constraint_metric(dignified_death__relational_autonomy, suppression_requirement, 0.25).
narrative_ontology:constraint_metric(dignified_death__relational_autonomy, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(dignified_death__relational_autonomy, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(dignified_death__relational_autonomy, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dignified_death__relational_autonomy, rope).
narrative_ontology:human_readable(dignified_death__relational_autonomy, "Relational Autonomy Decision Framework for Dignified Death").
narrative_ontology:topic_domain(dignified_death__relational_autonomy, "bioethics/medical_law/political_philosophy").

domain_priors:requires_active_enforcement(dignified_death__relational_autonomy).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dignified_death__relational_autonomy, '2bc774aa-1585-4f5d-ad72-436f7b39500c').
narrative_ontology:cs_kernel_codification('2bc774aa-1585-4f5d-ad72-436f7b39500c', distributed).
narrative_ontology:cs_authority_grounding('2bc774aa-1585-4f5d-ad72-436f7b39500c', practice).
narrative_ontology:cs_interpretation_layer_present('2bc774aa-1585-4f5d-ad72-436f7b39500c').
narrative_ontology:cs_reading_relation('2bc774aa-1585-4f5d-ad72-436f7b39500c', dignified_death__autonomy_primary, coexists_with).
narrative_ontology:cs_reading_relation('2bc774aa-1585-4f5d-ad72-436f7b39500c', dignified_death__sanctity_primary, coexists_with).
narrative_ontology:cs_axiom('2bc774aa-1585-4f5d-ad72-436f7b39500c', foundational, dignity_requires_relational_embedding).
narrative_ontology:cs_axiom_status(dignity_requires_relational_embedding, holdable).
narrative_ontology:cs_axiom_grounding('2bc774aa-1585-4f5d-ad72-436f7b39500c', dignity_requires_relational_embedding, deontological).
narrative_ontology:cs_axiom('2bc774aa-1585-4f5d-ad72-436f7b39500c', foundational, procedural_safeguards_legitimize_shared_authority).
narrative_ontology:cs_axiom_status(procedural_safeguards_legitimize_shared_authority, holdable).
narrative_ontology:cs_axiom_grounding('2bc774aa-1585-4f5d-ad72-436f7b39500c', procedural_safeguards_legitimize_shared_authority, conventional).
narrative_ontology:cs_reference_frame('2bc774aa-1585-4f5d-ad72-436f7b39500c', relational_dignity_framework).
narrative_ontology:cs_drift_state('2bc774aa-1585-4f5d-ad72-436f7b39500c', contemporary_implementation_era, gap(practice_drift, minor, true)).
narrative_ontology:cs_created_at('2bc774aa-1585-4f5d-ad72-436f7b39500c', '').
narrative_ontology:cs_kernel_id(dignified_death__relational_autonomy, dignified_death).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dignified_death__relational_autonomy, patient_family_clinician_triad).
narrative_ontology:constraint_beneficiary(dignified_death__relational_autonomy, relational_networks).
narrative_ontology:constraint_beneficiary(dignified_death__relational_autonomy, healthcare_institutions_with_ethics_committees).
narrative_ontology:constraint_victim(dignified_death__relational_autonomy, patients_excluded_from_process_by_autonomy_model).
narrative_ontology:constraint_victim(dignified_death__relational_autonomy, vulnerable_patients_excluded_by_sanctity_model).
narrative_ontology:constraint_victim(dignified_death__relational_autonomy, isolated_individuals_without_relational_network).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(dignified_death__relational_autonomy, patient_in_triad).
narrative_ontology:constraint_beneficiary(dignified_death__relational_autonomy, family_members).
narrative_ontology:constraint_victim(dignified_death__relational_autonomy, patient_in_triad).
narrative_ontology:constraint_vindicates(dignified_death__relational_autonomy, relational_autonomy_doctrine).
narrative_ontology:constraint_vindicates(dignified_death__relational_autonomy, procedural_safeguard_legitimacy).
narrative_ontology:constraint_vindicates(dignified_death__relational_autonomy, shared_decision_making_norm).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Participates in shared decision-making with family and clinician. Gains procedural voice and relational support but bears emotional burden of collective deliberation and potential family pressure. Exit from the triad structure is constrained by illness severity and institutional protocol.
narrative_ontology:constraint_stakeholder(dignified_death__relational_autonomy, patient_in_triad, beneficiary,
    moderate, biographical, constrained, local).
narrative_ontology:stakeholder_secondary_role(dignified_death__relational_autonomy, patient_in_triad, payer).

% Hold recognized authority in the decision process alongside patient and clinician. Gain formal role and moral standing but bear responsibility for collective outcome and potential intrafamily conflict. Their exit is constrained by kinship bonds and legal recognition of surrogate role.
narrative_ontology:constraint_stakeholder(dignified_death__relational_autonomy, family_members, beneficiary,
    moderate, biographical, constrained, local).

% Chairs the deliberative process, provides medical expertise, and ensures procedural safeguards are met. Gains professional legitimacy and legal protection but bears time cost and moral responsibility for process integrity. Can exit by transferring care but is bound by professional ethics and institutional policy.
narrative_ontology:constraint_stakeholder(dignified_death__relational_autonomy, treating_clinician, agenda_setter,
    institutional, biographical, mobile, local).

% Provides oversight, policy framework, and dispute resolution for the triad process. Gains institutional authority and normative influence but bears administrative cost. Operates at arms length with high structural mobility across cases.
narrative_ontology:constraint_stakeholder(dignified_death__relational_autonomy, ethics_committee, agenda_setter,
    institutional, generational, arbitrage, regional).

% Lacks family or relational network to constitute the triad. The framework's procedural safeguards presume a relational context that does not exist for them. They are structurally excluded from the coordination function and bear the full weight of the constraint without its benefits.
narrative_ontology:constraint_stakeholder(dignified_death__relational_autonomy, isolated_patient, excluded,
    powerless, immediate, trapped, local).

% Argues that dignity requires individual final authority; views the triad as diluting patient sovereignty. Observes from outside the clinical encounter, shaping public discourse and legal challenges.
narrative_ontology:constraint_stakeholder(dignified_death__relational_autonomy, autonomy_advocate, observer,
    organized, generational, analytical, national).

% Argues that intentional life-termination is intrinsically wrong regardless of process. Views the triad as legitimizing a prohibited act. Observes from doctrinal and legal-institutional positions.
narrative_ontology:constraint_stakeholder(dignified_death__relational_autonomy, sanctity_advocate, observer,
    organized, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the coordination problem of legitimate end-of-life decision-making when no single party's authority is socially accepted as sufficient — distributes authority across patient, family, and clinician so that decisions reflect relational embeddedness, medical reality, and procedural legitimacy simultaneously.
% TRANSFER_FUNCTION: Transfers decision authority from a sole actor (patient alone in autonomy model, physician alone in paternalism, state alone in sanctity model) to a structured triad with procedural safeguards. Moves moral responsibility, epistemic burden, and legal liability across the three seats.
% ABSENT_VOICES: Isolated patients without family networks, patients whose families are hostile or absent, clinicians who object to the triad model on conscience grounds, and future patients whose preferences may be overridden by current family dynamics. These voices are structurally absent because the framework presumes a functioning relational context.
% DISAPPEARANCE_RATIONALE: If the relational autonomy framework vanished overnight, end-of-life decisions would revert to either pure patient autonomy (where legal), physician paternalism, or state prohibition — each producing different outcomes for the same clinical situations. The triad structure is what currently holds the coordination equilibrium in jurisdictions that have adopted it.
% FOUNDING_PROBLEM: The founding problem was the legitimacy vacuum in end-of-life decisions: pure patient autonomy failed to protect vulnerable patients from implicit coercion; pure physician authority failed to respect patient values; state prohibition failed to accommodate suffering. The triad with safeguards was built to solve this three-way legitimacy deficit.
% FOUNDING_PROBLEM_CORROBORATION: Palliative care physicians and bioethicists outside the beneficiary set attest the triad improves decision quality for connected patients but creates new exclusion for the isolated. Disability rights organizations attest the framework still permits subtle coercion. No single external source corroborates the founding problem as fully solved or fully live — the contested status is itself the corroborated fact.
narrative_ontology:disappearance_verdict(dignified_death__relational_autonomy, world_rearranges).
narrative_ontology:founding_problem_status(dignified_death__relational_autonomy, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(dignified_death__relational_autonomy, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
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
 *   Extractiveness is moderate (0.38) because the constraint transfers decision authority from individuals to a collective process, imposing procedural costs on all three seats while extracting compliance from those who would prefer a different authority model. Suppression is low-moderate (0.25) because alternatives (pure autonomy, pure sanctity) remain legally available in some jurisdictions and intellectually live everywhere — the constraint coordinates rather than crushes. Theater is low (0.15) because the procedural safeguards are genuinely functional, not performative. The constraint claims rope type: a genuine coordination problem with procedural overhead but no asymmetric extraction — all three seats gain legitimacy and all bear costs.
 *
 * PERSPECTIVAL GAP:
 *   The triad seats experience the constraint differently: the patient gains voice but loses final authority; the family gains standing but bears moral weight; the clinician gains protection but loses unilateral discretion. The isolated patient seat experiences it as pure exclusion — a snare from their position. The engine computes this divergence from the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries are the relational network (patient-in-triad, family, clinician) — they gain a legitimate decision process. Victims are those excluded by the framework's structural presumptions: isolated patients, patients whose families are absent or hostile, and those who would choose differently under autonomy or sanctity models. The directionality derivation from beneficiary/victim declarations plus exit options produces d values near 0.5 for triad members (symmetric costs/benefits) and near 1.0 for isolated patients (full target, trapped exit).
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint prevents mislabeling coordination as extraction by making the procedural overhead transparent and distributing it across all three seats — no single party captures the gains. The mandate (legitimate end-of-life decision-making) remains live because the three-way legitimacy deficit persists. Mandatrophy would occur if the triad became a rubber stamp for family preferences or clinician convenience — current theater ratio suggests this has not happened.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    isolated_patient_exclusion_depth,
    'Is the exclusion of isolated patients a structural necessity of the relational model or a contingent implementation gap that could be closed by procedural substitutes (e.g., appointed advocates, community representatives)?',
    'Comparative analysis of jurisdictions using relational frameworks: do they have functional substitutes for isolated patients, and do those substitutes achieve equivalent legitimacy?',
    'If structural, the constraint is a tangled_rope (coordination + asymmetric extraction on the isolated). If contingent, it is a rope with an implementation defect. Changes victim set and effective extraction for the excluded seat.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(isolated_patient_exclusion_depth, conceptual, 'Whether isolated patient exclusion is intrinsic to relational autonomy or remediable').

omega_variable(
    family_coercion_vs_support,
    'Does the triad structure in practice amplify subtle family coercion (financial, emotional, cultural) under the guise of relational dignity, or does the procedural safeguard layer reliably detect and mitigate it?',
    'Empirical studies of triad deliberations measuring concordance between patient''s prior expressed wishes and triad outcomes, correlated with family dynamics indicators.',
    'If coercion amplification is systematic, the constraint extracts from patients under cover of coordination — effective extraction higher than authored, victim set expands. If safeguards work, the rope classification holds.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(family_coercion_vs_support, empirical, 'Whether procedural safeguards effectively distinguish relational support from relational coercion').

omega_variable(
    kernel_reading_foreclosure,
    'Does the relational_autonomy reading logically foreclose the autonomy_primary reading within a single legal framework, or do they coexist as options for different patients/jurisdictions?',
    'Legal analysis: can a jurisdiction offer both a pure advance directive pathway (autonomy) and a triad pathway (relational) without contradiction, or does adopting one as default foreclose the other''s legitimacy?',
    'If forecloses, the readings are mutually exclusive constraint families. If coexists_with, they are parallel options. Determines cs_structure.reading_relations classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_foreclosure, conceptual, 'Structural relationship between relational_autonomy and autonomy_primary readings').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dignified_death__relational_autonomy, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dign_tr_t0, dignified_death__relational_autonomy, theater_ratio, 0, 0.1).
narrative_ontology:measurement(dign_tr_t8, dignified_death__relational_autonomy, theater_ratio, 8, 0.12).
narrative_ontology:measurement(dign_tr_t16, dignified_death__relational_autonomy, theater_ratio, 16, 0.13).
narrative_ontology:measurement(dign_tr_t24, dignified_death__relational_autonomy, theater_ratio, 24, 0.14).
narrative_ontology:measurement(dign_tr_t32, dignified_death__relational_autonomy, theater_ratio, 32, 0.15).
narrative_ontology:measurement(dign_tr_t40, dignified_death__relational_autonomy, theater_ratio, 40, 0.15).

% Extraction over time
narrative_ontology:measurement(dign_be_t0, dignified_death__relational_autonomy, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(dign_be_t8, dignified_death__relational_autonomy, base_extractiveness, 8, 0.35).
narrative_ontology:measurement(dign_be_t16, dignified_death__relational_autonomy, base_extractiveness, 16, 0.37).
narrative_ontology:measurement(dign_be_t24, dignified_death__relational_autonomy, base_extractiveness, 24, 0.38).
narrative_ontology:measurement(dign_be_t32, dignified_death__relational_autonomy, base_extractiveness, 32, 0.38).
narrative_ontology:measurement(dign_be_t40, dignified_death__relational_autonomy, base_extractiveness, 40, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(dign_su_t0, dignified_death__relational_autonomy, suppression_requirement, 0, 0.2).
narrative_ontology:measurement(dign_su_t8, dignified_death__relational_autonomy, suppression_requirement, 8, 0.22).
narrative_ontology:measurement(dign_su_t16, dignified_death__relational_autonomy, suppression_requirement, 16, 0.23).
narrative_ontology:measurement(dign_su_t24, dignified_death__relational_autonomy, suppression_requirement, 24, 0.24).
narrative_ontology:measurement(dign_su_t32, dignified_death__relational_autonomy, suppression_requirement, 32, 0.25).
narrative_ontology:measurement(dign_su_t40, dignified_death__relational_autonomy, suppression_requirement, 40, 0.25).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dignified_death__relational_autonomy, identity_coordination).
narrative_ontology:boltzmann_floor_override(dignified_death__relational_autonomy, 0.08).
narrative_ontology:affects_constraint(dignified_death__relational_autonomy, dignified_death__autonomy_primary).
narrative_ontology:affects_constraint(dignified_death__relational_autonomy, dignified_death__sanctity_primary).

% DUAL FORMULATION NOTE:
% The dignified_death kernel decomposes into three constraint stories: autonomy_primary (patient sovereignty), relational_autonomy (this story — triad with safeguards), and sanctity_primary (intrinsic value prohibition). Each has distinct ε, beneficiary/victim sets, and constraint types. This story links to its siblings as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(dignified_death__relational_autonomy, powerless, 0.95).
constraint_indexing:directionality_override(dignified_death__relational_autonomy, moderate, 0.55).
constraint_indexing:directionality_override(dignified_death__relational_autonomy, institutional, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
