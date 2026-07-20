% ============================================================================
% CONSTRAINT STORY: dignified_death__autonomy_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_dignified_death__autonomy_primary, []).

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
 *   constraint_id: dignified_death__autonomy_primary
 *   human_readable: Dignified Death: Autonomy-Primary Reading
 *   domain: bioethics/medical_law/political_philosophy
 *
 * SUMMARY:
 *   This constraint story instantiates the autonomy_primary reading of the
 *   dignified_death kernel. It treats dignity as residing in
 *   self-determination, with the suffering individual holding final authority
 *   over the timing and method of death. The constraint is the legal-medical
 *   gatekeeping apparatus that permits assisted dying under strict
 *   eligibility criteria â coordinating autonomy for qualifying patients
 *   while extracting prolonged suffering from those who fail to qualify.
 *   Sibling readings (sanctity_primary, relational_autonomy) are documented
 *   in kernel_context and cs_structure but are not described within this
 *   constraint, per Îµ-invariance.
 *
 * KEY AGENTS:
 *   - qualifying_patients: Suffering individuals who meet eligibility criteria and receive authorized access to assisted dying (moderate power, constrained exit).
 *   - denied_suffering_patients: Individuals who fail eligibility criteria and bear the cost of continued existence against their will (powerless, trapped exit).
 *   - medical_gatekeepers: Clinicians and ethics boards administering eligibility assessments and maintaining professional gatekeeping authority (institutional, constrained exit, dual beneficiary/agenda_setter role).
 *   - disability_rights_advocates: Organized actors arguing the framework devalues disabled life, structurally excluded from legislative design (organized, constrained exit).
 *   - bioethics_analyst: Analytical observer tracking cross-jurisdictional outcomes and practice drift (analytical, analytical exit).
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dignified_death__autonomy_primary, 0.55).
domain_priors:suppression_score(dignified_death__autonomy_primary, 0.68).
domain_priors:theater_ratio(dignified_death__autonomy_primary, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dignified_death__autonomy_primary, extractiveness, 0.55).
narrative_ontology:constraint_metric(dignified_death__autonomy_primary, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(dignified_death__autonomy_primary, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(dignified_death__autonomy_primary, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(dignified_death__autonomy_primary, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dignified_death__autonomy_primary, tangled_rope).
narrative_ontology:human_readable(dignified_death__autonomy_primary, "Dignified Death: Autonomy-Primary Reading").
narrative_ontology:topic_domain(dignified_death__autonomy_primary, "bioethics/medical_law/political_philosophy").

domain_priors:requires_active_enforcement(dignified_death__autonomy_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dignified_death__autonomy_primary, '65f23acf-1308-45ca-aa51-c28681279758').
narrative_ontology:cs_kernel_codification('65f23acf-1308-45ca-aa51-c28681279758', formalized).
narrative_ontology:cs_authority_grounding('65f23acf-1308-45ca-aa51-c28681279758', lineage).
narrative_ontology:cs_interpretation_layer_present('65f23acf-1308-45ca-aa51-c28681279758').
narrative_ontology:cs_reading_relation('65f23acf-1308-45ca-aa51-c28681279758', dignified_death__sanctity_primary, forecloses).
narrative_ontology:cs_reading_relation('65f23acf-1308-45ca-aa51-c28681279758', dignified_death__relational_autonomy, coexists_with).
narrative_ontology:cs_axiom('65f23acf-1308-45ca-aa51-c28681279758', foundational, individual_final_authority_over_death).
narrative_ontology:cs_axiom_status(individual_final_authority_over_death, holdable).
narrative_ontology:cs_axiom_grounding('65f23acf-1308-45ca-aa51-c28681279758', individual_final_authority_over_death, deontological).
narrative_ontology:cs_axiom('65f23acf-1308-45ca-aa51-c28681279758', foundational, autonomy_as_means_to_end_suffering).
narrative_ontology:cs_axiom_status(autonomy_as_means_to_end_suffering, holdable).
narrative_ontology:cs_axiom_grounding('65f23acf-1308-45ca-aa51-c28681279758', autonomy_as_means_to_end_suffering, instrumental).
narrative_ontology:cs_reference_frame('65f23acf-1308-45ca-aa51-c28681279758', individual_autonomy_sovereignty).
narrative_ontology:cs_drift_state('65f23acf-1308-45ca-aa51-c28681279758', contemporary_medical_gatekeeping_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('65f23acf-1308-45ca-aa51-c28681279758', '').
narrative_ontology:cs_kernel_id(dignified_death__autonomy_primary, dignified_death).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dignified_death__autonomy_primary, qualifying_patients).
narrative_ontology:constraint_beneficiary(dignified_death__autonomy_primary, medical_gatekeepers).
narrative_ontology:constraint_victim(dignified_death__autonomy_primary, denied_suffering_patients).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Suffering individuals who meet statutory eligibility criteria and are granted a lawful, medically supervised pathway to end their lives. Their autonomy is real but conditional: they must satisfy residency, capacity, and prognostic thresholds, and endure mandatory waiting periods and multiple assessments.
narrative_ontology:constraint_stakeholder(dignified_death__autonomy_primary, qualifying_patients, beneficiary,
    moderate, biographical, constrained, national).

% Individuals experiencing severe, enduring suffering who fall outside eligibility criteria due to non-terminal condition, psychiatric diagnosis, incomplete residency requirements, or failure to demonstrate decisional capacity. They bear the cost of continued existence against their will and lack lawful exit options.
narrative_ontology:constraint_stakeholder(dignified_death__autonomy_primary, denied_suffering_patients, payer,
    powerless, immediate, trapped, national).

% Physicians, psychiatrists, and ethics committees who assess eligibility, certify capacity, and administer or prescribe the lethal regimen. They derive professional authority, legal protection, and institutional standing from their discretionary gatekeeping role, and are bound by statutory criteria and liability frameworks.
narrative_ontology:constraint_stakeholder(dignified_death__autonomy_primary, medical_gatekeepers, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(dignified_death__autonomy_primary, medical_gatekeepers, beneficiary).

% Organized actors who argue that eligibility criteria and social messaging devalue disabled and dependent life, creating perverse incentives toward premature termination. They are frequently sidelined from legislative drafting and eligibility design, appearing in hearings after frameworks are fixed.
narrative_ontology:constraint_stakeholder(dignified_death__autonomy_primary, disability_rights_advocates, excluded,
    organized, generational, constrained, national).

% Academic and policy analysts who track patient demographics, practice drift, and cross-jurisdictional outcomes. They evaluate whether the autonomy-primary framework realizes its stated aims without participating in gatekeeping.
narrative_ontology:constraint_stakeholder(dignified_death__autonomy_primary, bioethics_analyst, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a legal-medical framework through which competent, suffering individuals can access assisted dying without arbitrary state prohibition, while establishing procedural safeguards and eligibility criteria to coordinate patient autonomy with medical practice and criminal law.
% TRANSFER_FUNCTION: Moves authority over the timing and manner of death from absolute state prohibition to the qualifying patient, conditioned on medical gatekeeping approval; simultaneously transfers the cost of continued existence to patients who fail eligibility criteria.
% ABSENT_VOICES: Disability rights advocates who contend the framework embeds ableist assumptions and threatens disabled people; individuals who have lost decisional capacity but retain interests in avoiding suffering; and rival medical practitioners who would assist outside the authorized gatekeeping structure.
% DISAPPEARANCE_RATIONALE: If the constraint vanished overnight, qualifying patients would lose a protected legal pathway and face return to underground or violent methods; denied patients would remain in suffering but without the partial relief valve the framework provides; medical practice would lose its gatekeeping role and associated authority; the bioethical and legal landscape would reorganize around either full prohibition or unregulated decriminalization.
% FOUNDING_PROBLEM: Arbitrary state prohibition of assisted dying forces competent, suffering individuals to endure unwanted existence or resort to violent, unregulated methods, creating a coordination failure between patient autonomy, medical ethics, and legal accountability.
% FOUNDING_PROBLEM_CORROBORATION: Qualifying patients and autonomy-focused bioethicists attest the problem is live. Disability rights advocates and sanctity-of-life proponents attest the problem is misdiagnosed or that the remedy creates new harms. Empirical data from jurisdictions with legalization corroborate demand, while critics dispute whether the framework solves the founding problem or functions as managed extraction.
narrative_ontology:disappearance_verdict(dignified_death__autonomy_primary, world_rearranges).
narrative_ontology:founding_problem_status(dignified_death__autonomy_primary, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(dignified_death__autonomy_primary, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(dignified_death__autonomy_primary, 'none', 1).
narrative_ontology:epsilon_provenance(dignified_death__autonomy_primary, 0.55, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(dignified_death__autonomy_primary_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(dignified_death__autonomy_primary, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(dignified_death__autonomy_primary_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.55) reflects that the framework grants real autonomy to qualifying patients while systematically denying exit to others, creating asymmetric extraction. Suppression (0.68) is high because the constraint's persistence depends on actively prosecuting or disciplining clinicians who operate outside the authorized framework. Theater_ratio (0.42) captures the performative accumulation of procedural safeguards (repeated capacity assessments, waiting periods) that increasingly serve liability protection and institutional legitimacy rather than patient welfare. Accessibility_collapse (0.65) registers that once the legal-medical framework exists, unregulated alternatives collapse into criminality. Resistance (0.58) reflects sustained opposition from disability advocates, religious groups, and some clinicians. The measurement series share a single time grid to prevent misaligned substitution.
 *
 * PERSPECTIVAL GAP:
 *   Qualifying patients experience the constraint as coordination (a protected pathway), while denied suffering patients experience it as extraction (state-mandated endurance). Medical gatekeepers experience it as necessary professional practice that also secures their institutional role. The engine computes this divergence from the structural data; the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Qualifying patients are declared beneficiaries (low d), receiving subsidized autonomy through the legal pathway. Denied suffering patients are declared victims (high d), bearing the full cost of prolonged existence. Medical gatekeepers are dual-positioned: as agenda_setters they administer the constraint, and as beneficiaries they collect professional authority and legal protection, placing their d toward the beneficiary end. Disability advocates are excluded and bear no directional relationship to the constraint's operation.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint prevents mislabeling by requiring both a genuine coordination function (autonomy pathway for qualifying patients) and asymmetric extraction (denial and suffering for non-qualifying patients). Without the victim set, the framework would read as a rope or scaffold; without the coordination function, it would read as a snare. The tangled_rope classification captures the entanglement of genuine autonomy-granting with medical gatekeeping that excludes.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'Is the autonomy_primary reading of dignity the structurally correct one, or does it depend on contingent cultural framing that could shift to relational_autonomy or sanctity_primary?',
    'Cross-jurisdictional comparison of legalization outcomes and patient preference studies; tracking whether autonomy-primary frameworks drift toward relational models over time.',
    'If relational_autonomy is structurally dominant, the beneficiary/victim mapping shifts from individual-vs-state to triad-vs-isolated-patient; if sanctity_primary dominates, the coordination function collapses entirely and extraction redistributes to all seeking assisted dying.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Which reading of the dignity kernel is structurally instantiated.').

omega_variable(
    gatekeeping_extraction_ambiguity,
    'Does the medical gatekeeping in autonomy-primary frameworks function as necessary procedural safeguard, or as disguised state prohibition that extracts suffering from non-conforming patients?',
    'Natural experiment comparing jurisdictions with minimal gatekeeping versus high-gatekeeping models: if outcomes for patient welfare and non-voluntary deaths are equivalent, gatekeeping is extractive theater.',
    'If gatekeeping is primarily theater, theater_ratio rises and the constraint slides toward snare; if genuinely protective, extraction is the necessary cost of coordination and tangled_rope is stable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(gatekeeping_extraction_ambiguity, empirical, 'Whether medical gatekeeping is protective or extractive.').

omega_variable(
    capacity_assessment_as_moral_gate,
    'Do assessments of decisional capacity in assisted dying frameworks track genuine cognitive competence, or do they function as moral gates that exclude patients whose reasons for dying are deemed unacceptable?',
    'Audit studies comparing capacity determination rates across jurisdictions with different social attitudes; analysis of refusal reasons in denied applications.',
    'If capacity assessment is a moral gate, suppression and theater_ratio are higher than structurally claimed, pushing the constraint toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(capacity_assessment_as_moral_gate, empirical, 'Whether capacity assessment is neutral or moralized.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dignified_death__autonomy_primary, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dign_tr_t0, dignified_death__autonomy_primary, theater_ratio, 0, 0.2).
narrative_ontology:measurement(dign_tr_t5, dignified_death__autonomy_primary, theater_ratio, 5, 0.25).
narrative_ontology:measurement(dign_tr_t10, dignified_death__autonomy_primary, theater_ratio, 10, 0.31).
narrative_ontology:measurement(dign_tr_t15, dignified_death__autonomy_primary, theater_ratio, 15, 0.36).
narrative_ontology:measurement(dign_tr_t20, dignified_death__autonomy_primary, theater_ratio, 20, 0.4).
narrative_ontology:measurement(dign_tr_t25, dignified_death__autonomy_primary, theater_ratio, 25, 0.42).

% Extraction over time
narrative_ontology:measurement(dign_be_t0, dignified_death__autonomy_primary, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(dign_be_t5, dignified_death__autonomy_primary, base_extractiveness, 5, 0.42).
narrative_ontology:measurement(dign_be_t10, dignified_death__autonomy_primary, base_extractiveness, 10, 0.47).
narrative_ontology:measurement(dign_be_t15, dignified_death__autonomy_primary, base_extractiveness, 15, 0.51).
narrative_ontology:measurement(dign_be_t20, dignified_death__autonomy_primary, base_extractiveness, 20, 0.53).
narrative_ontology:measurement(dign_be_t25, dignified_death__autonomy_primary, base_extractiveness, 25, 0.55).

% Suppression requirement over time
narrative_ontology:measurement(dign_su_t0, dignified_death__autonomy_primary, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(dign_su_t5, dignified_death__autonomy_primary, suppression_requirement, 5, 0.52).
narrative_ontology:measurement(dign_su_t10, dignified_death__autonomy_primary, suppression_requirement, 10, 0.58).
narrative_ontology:measurement(dign_su_t15, dignified_death__autonomy_primary, suppression_requirement, 15, 0.63).
narrative_ontology:measurement(dign_su_t20, dignified_death__autonomy_primary, suppression_requirement, 20, 0.66).
narrative_ontology:measurement(dign_su_t25, dignified_death__autonomy_primary, suppression_requirement, 25, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dignified_death__autonomy_primary, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
