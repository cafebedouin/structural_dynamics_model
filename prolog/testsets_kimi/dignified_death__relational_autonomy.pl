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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:suppression_profile/2,
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
 *   constraint_id: dignified_death__relational_autonomy
 *   human_readable: Relational Autonomy in End-of-Life Decision-Making
 *   domain: bioethics/medical_law/political_philosophy
 *
 * SUMMARY:
 *   This constraint instantiates the relational_autonomy reading of the
 *   dignified_death kernel: dignity is understood to emerge from relational
 *   context rather than isolated self-determination or transcendent moral
 *   law. Decision authority is distributed across a patient-family-clinician
 *   triad bound by procedural safeguards. It is claimed as rope â a
 *   coordination mechanism solving the collective problem of end-of-life
 *   deliberation â while the metrics independently record moderate
 *   extraction and a victim set of excluded patients. The divergence between
 *   the rope claim and the presence of victims is intentional; the engine
 *   measures that gap.
 *
 * KEY AGENTS:
 *   - Triad patients (moderate/constrained): embedded in relational decision-making, gain support but lose unilateral command
 *   - Family members (moderate/constrained): included as relational partners, gain voice but assume burden
 *   - Clinicians (institutional/constrained): facilitate and enforce procedural safeguards, dual-positioned as beneficiaries and agenda-setters
 *   - Autonomy-seeking patients (powerless/constrained): bear costs of mandatory deliberation, excluded from sole authority
 *   - Vulnerable patients (powerless/trapped): exposed to potential familial pressure despite safeguards
 *   - Clinical institutions (institutional/constrained): codify protocols and absorb liability
 *   - Bioethicists (analytical/analytical): observe and refine the framework from outside
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dignified_death__relational_autonomy, 0.38).
domain_priors:suppression_score(dignified_death__relational_autonomy, 0.25).
domain_priors:theater_ratio(dignified_death__relational_autonomy, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dignified_death__relational_autonomy, extractiveness, 0.38).
narrative_ontology:constraint_metric(dignified_death__relational_autonomy, suppression_requirement, 0.25).
narrative_ontology:constraint_metric(dignified_death__relational_autonomy, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(dignified_death__relational_autonomy, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(dignified_death__relational_autonomy, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dignified_death__relational_autonomy, rope).
narrative_ontology:human_readable(dignified_death__relational_autonomy, "Relational Autonomy in End-of-Life Decision-Making").
narrative_ontology:topic_domain(dignified_death__relational_autonomy, "bioethics/medical_law/political_philosophy").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dignified_death__relational_autonomy, 'fc15d774-3e30-414a-848a-034ff60ffa0e').
narrative_ontology:cs_kernel_codification('fc15d774-3e30-414a-848a-034ff60ffa0e', formalized).
narrative_ontology:cs_authority_grounding('fc15d774-3e30-414a-848a-034ff60ffa0e', expertise).
narrative_ontology:cs_interpretation_layer_present('fc15d774-3e30-414a-848a-034ff60ffa0e').
narrative_ontology:cs_reading_relation('fc15d774-3e30-414a-848a-034ff60ffa0e', dignified_death__autonomy_primary, coexists_with).
narrative_ontology:cs_reading_relation('fc15d774-3e30-414a-848a-034ff60ffa0e', dignified_death__sanctity_primary, coexists_with).
narrative_ontology:cs_axiom('fc15d774-3e30-414a-848a-034ff60ffa0e', foundational, dignity_emerges_from_relational_context).
narrative_ontology:cs_axiom_status(dignity_emerges_from_relational_context, holdable).
narrative_ontology:cs_axiom_grounding('fc15d774-3e30-414a-848a-034ff60ffa0e', dignity_emerges_from_relational_context, deontological).
narrative_ontology:cs_axiom('fc15d774-3e30-414a-848a-034ff60ffa0e', foundational, authority_distributed_by_procedural_safeguards).
narrative_ontology:cs_axiom_status(authority_distributed_by_procedural_safeguards, holdable).
narrative_ontology:cs_axiom_grounding('fc15d774-3e30-414a-848a-034ff60ffa0e', authority_distributed_by_procedural_safeguards, conventional).
narrative_ontology:cs_reference_frame('fc15d774-3e30-414a-848a-034ff60ffa0e', relational_decision_framework).
narrative_ontology:cs_drift_state('fc15d774-3e30-414a-848a-034ff60ffa0e', contemporary_biomedical_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('fc15d774-3e30-414a-848a-034ff60ffa0e', '').
narrative_ontology:cs_kernel_id(dignified_death__relational_autonomy, dignified_death).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dignified_death__relational_autonomy, triad_patients).
narrative_ontology:constraint_beneficiary(dignified_death__relational_autonomy, family_members).
narrative_ontology:constraint_beneficiary(dignified_death__relational_autonomy, clinicians).
narrative_ontology:constraint_victim(dignified_death__relational_autonomy, autonomy_seeking_patients).
narrative_ontology:constraint_victim(dignified_death__relational_autonomy, vulnerable_patients).
narrative_ontology:constraint_vindicates(dignified_death__relational_autonomy, relational_personhood_theory).
narrative_ontology:constraint_vindicates(dignified_death__relational_autonomy, shared_decision_making_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Patients approaching the end of life whose decisions are embedded in facilitated conversation with family and clinicians; they receive emotional and informational support but must engage in deliberation rather than unilaterally directing outcomes.
narrative_ontology:constraint_stakeholder(dignified_death__relational_autonomy, triad_patients, beneficiary,
    moderate, biographical, constrained, local).

% Relatives brought into formal decision-making as bearers of the patient's narrative and values; they gain standing and voice but also assume emotional labor, potential guilt, and relational burden.
narrative_ontology:constraint_stakeholder(dignified_death__relational_autonomy, family_members, beneficiary,
    moderate, biographical, constrained, local).

% Physicians, nurses, and palliative specialists who facilitate triadic deliberation, provide prognostic information, and enforce procedural safeguards such as capacity assessments and documentation; they gain role clarity and reduced unilateral liability.
narrative_ontology:constraint_stakeholder(dignified_death__relational_autonomy, clinicians, beneficiary,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(dignified_death__relational_autonomy, clinicians, agenda_setter).

% Patients who hold strong preferences for self-determination and regard familial or clinical input as illegitimate interference; they are required by institutional policy to participate in triadic deliberation even when they desire sole authority.
narrative_ontology:constraint_stakeholder(dignified_death__relational_autonomy, autonomy_seeking_patients, payer,
    powerless, biographical, constrained, local).

% Cognitively impaired, economically dependent, or socially isolated patients susceptible to undue influence within family-clinician dynamics; procedural safeguards are intended to protect them but may fail, exposing them to pressure.
narrative_ontology:constraint_stakeholder(dignified_death__relational_autonomy, vulnerable_patients, payer,
    powerless, biographical, trapped, local).

% Hospitals and hospice organizations that codify triadic protocols, train staff in facilitated deliberation, and bear legal liability for ensuring due process in end-of-life decisions.
narrative_ontology:constraint_stakeholder(dignified_death__relational_autonomy, clinical_institutions, agenda_setter,
    institutional, generational, constrained, national).

% Academic and clinical ethicists who develop, critique, and refine relational autonomy theory; they operate outside the immediate clinical encounter and assess whether the triad model achieves its stated aims.
narrative_ontology:constraint_stakeholder(dignified_death__relational_autonomy, bioethicists, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Resolves the collective-action problem of end-of-life decision-making by distributing authority across patient, family, and clinician, thereby preventing both isolated patient abandonment and unilateral familial or medical override.
% TRANSFER_FUNCTION: Moves decisional authority from the individual patient into a triadic process; transfers emotional and deliberative burden across the triad; shifts liability risk from individual clinicians to institutionalized procedural compliance.
% ABSENT_VOICES: Patients who want sole self-determination without interference; religious communities holding sanctity-of-life views that reject intentional life-shortening; disability advocates who view triadic models as exposing vulnerable people to proxy pressure.
% DISAPPEARANCE_RATIONALE: If the relational autonomy framework vanished, end-of-life decisions would revert to pure patient autonomy or paternalistic/clerical models; family roles would shrink or expand unilaterally; clinician liability exposure would shift; the current balance of voices in the hospice room would reorganize.
% FOUNDING_PROBLEM: The failure of both pure autonomy (isolated patients making decisions without support) and pure paternalism (patients overridden by clinicians or family) to secure dignified deaths; the need for a structured process honoring both patient agency and relational embeddedness.
% FOUNDING_PROBLEM_CORROBORATION: Palliative care scholars and clinical ethics associations from within the practice corroborate the need for shared decision-making. Critical disability scholars and patient-self-determination advocates from outside the direct benefiting triad contest that the founding problem is solved, arguing the model displaces rather than resolves the core dilemma.
narrative_ontology:disappearance_verdict(dignified_death__relational_autonomy, world_rearranges).
narrative_ontology:founding_problem_status(dignified_death__relational_autonomy, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(dignified_death__relational_autonomy, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(dignified_death__relational_autonomy, 'none', 1).
narrative_ontology:epsilon_provenance(dignified_death__relational_autonomy, 0.38, 'kimi-k2.6', 'none', direct).

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
 *   Base extractiveness is moderate (0.38) because the procedural overhead of triadic deliberation, documentation, and capacity assessment imposes real costs on patients seeking rapid or unilateral resolution. Suppression is relatively low (0.25) because pure autonomy and pure sanctity alternatives remain legally and rhetorically available in other jurisdictions; the constraint coordinates rather than actively suppresses rivals. Theater ratio is moderate (0.30): family meetings, ethics consultations, and documentation rituals are partly functional but also create performative compliance that can substitute for genuine deliberation. Accessibility collapse (0.40) reflects that within the relational framework, pure autonomy reads as abandonment and pure sanctity as cruelty, yet strong alternatives persist outside. Resistance (0.45) is elevated because both autonomy-primary and sanctity-primary coalitions actively contest the triad model.
 *
 * PERSPECTIVAL GAP:
 *   Triad participants (patients, families, clinicians) experience the constraint as supportive coordination that prevents isolation and arbitrary override. Autonomy-seeking and vulnerable patients experience the same structure as a compulsory process that extracts their unilateral authority or exposes them to pressure. The engine computes this divergence from the same structural facts; the authored claim does not adjudicate the seat-level experience.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (triad_patients, family_members, clinicians) derive support, role clarity, and reduced liability from the constraint, placing their directionality near the beneficiary end. Victims (autonomy_seeking_patients, vulnerable_patients) bear the costs of mandatory deliberation and potential coercion, placing their directionality near the target end. The structural derivation produces divergent effective extraction (Ï) across seats without requiring overrides.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem â avoiding both isolated autonomy and paternalistic override â remains live in clinical practice, corroborated by palliative care scholarship. Because the problem is contested but not dead, the constraint does not read as piton. The procedural safeguards are actively used and periodically revised, indicating genuine coordination rather than inertial theater. Were the founding problem to die while the apparatus persisted, the rope claim would fail and the theater/extraction metrics would signal mandatrophy.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    relational_vs_autonomy_foreclosure,
    'Does the relational autonomy reading logically foreclose the autonomy_primary reading, or do they coexist as alternative framings held by different parties?',
    'Comparative legal and clinical-ethics analysis of whether jurisdictions adopting triadic models eliminate unilateral patient authority or preserve it as a fallback option.',
    'If foreclosed, the kernel is internally contradictory and the constraint functions as a disguised tangled rope; if coexisting, the rope classification among live alternatives is structurally sound.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(relational_vs_autonomy_foreclosure, conceptual, 'Whether relational autonomy forecloses pure autonomy or merely coexists with it').

omega_variable(
    procedural_overhead_extraction,
    'Does the procedural overhead of triadic decision-making constitute necessary coordination cost or extractive theater that obscures power asymmetries?',
    'Outcome studies comparing decision satisfaction, moral distress, and time-to-resolution across pure-autonomy, triadic, and paternalistic models in matched clinical populations.',
    'If overhead extracts more value than it coordinates, effective epsilon rises toward tangled_rope thresholds; if necessary cost, the rope classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(procedural_overhead_extraction, empirical, 'Whether procedural overhead is coordination cost or theater').

omega_variable(
    vulnerability_in_triad,
    'Does the triad model protect vulnerable patients from pressure or expose them to familial coercion masked as relational support?',
    'Longitudinal studies of end-of-life decisions for cognitively impaired, economically dependent, or socially isolated patients under triadic versus alternative decision models.',
    'If exposure is systematic, the victim set expands and directionality for vulnerable patients shifts toward full target, raising computed extraction for that seat.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(vulnerability_in_triad, empirical, 'Whether triadic models protect or expose vulnerable patients').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dignified_death__relational_autonomy, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dignified_death_relational_tr_t0, dignified_death__relational_autonomy, theater_ratio, 0, 0.1).
narrative_ontology:measurement(dignified_death_relational_tr_t8, dignified_death__relational_autonomy, theater_ratio, 8, 0.15).
narrative_ontology:measurement(dignified_death_relational_tr_t16, dignified_death__relational_autonomy, theater_ratio, 16, 0.22).
narrative_ontology:measurement(dignified_death_relational_tr_t24, dignified_death__relational_autonomy, theater_ratio, 24, 0.26).
narrative_ontology:measurement(dignified_death_relational_tr_t32, dignified_death__relational_autonomy, theater_ratio, 32, 0.29).
narrative_ontology:measurement(dignified_death_relational_tr_t40, dignified_death__relational_autonomy, theater_ratio, 40, 0.3).

% Extraction over time
narrative_ontology:measurement(dignified_death_relational_be_t0, dignified_death__relational_autonomy, base_extractiveness, 0, 0.2).
narrative_ontology:measurement(dignified_death_relational_be_t8, dignified_death__relational_autonomy, base_extractiveness, 8, 0.25).
narrative_ontology:measurement(dignified_death_relational_be_t16, dignified_death__relational_autonomy, base_extractiveness, 16, 0.3).
narrative_ontology:measurement(dignified_death_relational_be_t24, dignified_death__relational_autonomy, base_extractiveness, 24, 0.34).
narrative_ontology:measurement(dignified_death_relational_be_t32, dignified_death__relational_autonomy, base_extractiveness, 32, 0.37).
narrative_ontology:measurement(dignified_death_relational_be_t40, dignified_death__relational_autonomy, base_extractiveness, 40, 0.38).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(dignified_death__relational_autonomy, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dignified_death__relational_autonomy, attachment_coordination).
narrative_ontology:affects_constraint(dignified_death__relational_autonomy, dignified_death__autonomy_primary).
narrative_ontology:affects_constraint(dignified_death__relational_autonomy, dignified_death__sanctity_primary).

% DUAL FORMULATION NOTE:
% This constraint is the relational_autonomy reading of the dignified_death kernel. The kernel decomposes into three structurally distinct constraints (autonomy_primary, relational_autonomy, sanctity_primary) per the epsilon-invariance principle, each with distinct beneficiary/victim structures and epsilon profiles.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
