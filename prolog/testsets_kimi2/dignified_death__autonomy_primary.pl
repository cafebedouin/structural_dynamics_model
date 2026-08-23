% ============================================================================
% CONSTRAINT STORY: dignified_death__autonomy_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
 *   human_readable: Medical Gatekeeping of Autonomy in Dignified Death
 *   domain: bioethics/medical_law/political_philosophy
 *
 * SUMMARY:
 *   This constraint is the autonomy-primary reading of the dignified death
 *   kernel: dignity resides in self-determination, and the suffering
 *   individual holds final authority over the timing and method of death. The
 *   standing arrangement under contest is the legal-medical framework that
 *   entangles this autonomy norm with active gatekeepingâeligibility
 *   criteria, physician approval, waiting periods, and criminal prohibitions
 *   outside the authorized channel. From this reading's perspective, the
 *   constraint coordinates end-of-life decisions for the eligible while
 *   extracting from the ineligible through enforced prolongation of
 *   suffering. Sibling readings: sanctity_primary (life's intrinsic value
 *   absolutely prohibits intentional termination) and relational_autonomy
 *   (decision authority is distributed across a patient-family-clinician
 *   triad with procedural safeguards).
 *
 * KEY AGENTS:
 *   - state_medical_authority: Primary agenda_setter (institutional/constrained) â sets eligibility criteria and enforces prohibition outside the gatekept channel.
 *   - eligible_patients: Primary beneficiary (powerless/constrained) â receive the coordinated, lawful exit after navigating the medical protocol.
 *   - ineligible_suffering_patients: Primary target (powerless/trapped) â bear the extraction through prolonged suffering when they fail to qualify.
 *   - medical_gatekeepers: Dual-positioned agent (organized/constrained) â administers the criteria and collects professional authority.
 *   - disability_rights_advocates: Observer (organized/analytical) â resists expansion from a rights-based frame without directly paying or collecting.
 *   - sanctity_advocates: Excluded observer (organized/analytical) â categorically rejects the frame and is structurally bracketed by it.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dignified_death__autonomy_primary, 0.58).
domain_priors:suppression_score(dignified_death__autonomy_primary, 0.82).
domain_priors:theater_ratio(dignified_death__autonomy_primary, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dignified_death__autonomy_primary, extractiveness, 0.58).
narrative_ontology:constraint_metric(dignified_death__autonomy_primary, suppression_requirement, 0.82).
narrative_ontology:constraint_metric(dignified_death__autonomy_primary, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(dignified_death__autonomy_primary, accessibility_collapse, 0.78).
narrative_ontology:constraint_metric(dignified_death__autonomy_primary, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dignified_death__autonomy_primary, tangled_rope).
narrative_ontology:human_readable(dignified_death__autonomy_primary, "Medical Gatekeeping of Autonomy in Dignified Death").
narrative_ontology:topic_domain(dignified_death__autonomy_primary, "bioethics/medical_law/political_philosophy").

domain_priors:requires_active_enforcement(dignified_death__autonomy_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dignified_death__autonomy_primary, '7fecb5f0-37eb-4c70-9c41-a63a6a2b8f5c').
narrative_ontology:cs_kernel_codification('7fecb5f0-37eb-4c70-9c41-a63a6a2b8f5c', formalized).
narrative_ontology:cs_authority_grounding('7fecb5f0-37eb-4c70-9c41-a63a6a2b8f5c', expertise).
narrative_ontology:cs_interpretation_layer_present('7fecb5f0-37eb-4c70-9c41-a63a6a2b8f5c').
narrative_ontology:cs_reading_relation('7fecb5f0-37eb-4c70-9c41-a63a6a2b8f5c', dignified_death__sanctity_primary, forecloses).
narrative_ontology:cs_reading_relation('7fecb5f0-37eb-4c70-9c41-a63a6a2b8f5c', dignified_death__relational_autonomy, coexists_with).
narrative_ontology:cs_axiom('7fecb5f0-37eb-4c70-9c41-a63a6a2b8f5c', foundational, final_authority_over_death).
narrative_ontology:cs_axiom_status(final_authority_over_death, holdable).
narrative_ontology:cs_axiom_grounding('7fecb5f0-37eb-4c70-9c41-a63a6a2b8f5c', final_authority_over_death, deontological).
narrative_ontology:cs_axiom('7fecb5f0-37eb-4c70-9c41-a63a6a2b8f5c', secondary, suffering_as_legitimacy_condition).
narrative_ontology:cs_axiom_status(suffering_as_legitimacy_condition, holdable).
narrative_ontology:cs_axiom_grounding('7fecb5f0-37eb-4c70-9c41-a63a6a2b8f5c', suffering_as_legitimacy_condition, deontological).
narrative_ontology:cs_reference_frame('7fecb5f0-37eb-4c70-9c41-a63a6a2b8f5c', pure_autonomy_framework).
narrative_ontology:cs_drift_state('7fecb5f0-37eb-4c70-9c41-a63a6a2b8f5c', post_legalization_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('7fecb5f0-37eb-4c70-9c41-a63a6a2b8f5c', '').
narrative_ontology:cs_kernel_id(dignified_death__autonomy_primary, dignified_death).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dignified_death__autonomy_primary, eligible_patients).
narrative_ontology:constraint_beneficiary(dignified_death__autonomy_primary, medical_gatekeepers).
narrative_ontology:constraint_victim(dignified_death__autonomy_primary, ineligible_suffering_patients).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets the statutory eligibility criteria for assisted dying and enforces criminal prohibitions against non-sanctioned assistance. Licenses physicians, monitors compliance, and revises boundaries through regulatory review. Its exit from this role would require legislative repeal or constitutional reversal.
narrative_ontology:constraint_stakeholder(dignified_death__autonomy_primary, state_medical_authority, agenda_setter,
    institutional, generational, constrained, national).

% Meet diagnostic and prognostic criteria for lawful assisted dying and receive a supervised, pharmacological exit after mandatory assessments and waiting periods. They gain a legally protected death but must route their request through the designated medical channel and accept the timing and method determined by protocol.
narrative_ontology:constraint_stakeholder(dignified_death__autonomy_primary, eligible_patients, beneficiary,
    powerless, immediate, constrained, national).

% Experience prolonged suffering that falls outside the eligibility frameâwhether because of psychiatric diagnosis, non-terminal condition, early-stage dementia, or administrative delayâand are denied access to lawful assistance. Their alternatives are unassisted suicide, continuation of suffering, or illegal assistance carrying criminal risk for helpers.
narrative_ontology:constraint_stakeholder(dignified_death__autonomy_primary, ineligible_suffering_patients, payer,
    powerless, immediate, trapped, national).

% Physicians and review boards who assess competence, confirm prognosis, and prescribe or administer lethal medication. They control access to the coordinated exit and derive professional authority, fees, and institutional standing from their monopoly over the legitimate pathway. Conscience clauses permit limited individual exit but do not dissolve the profession's structural gatekeeping role.
narrative_ontology:constraint_stakeholder(dignified_death__autonomy_primary, medical_gatekeepers, agenda_setter,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(dignified_death__autonomy_primary, medical_gatekeepers, beneficiary).

% Argue that the eligibility framework creates perverse incentives to end life when social support is inadequate. They resist expansion of criteria but do not personally bear the constraint's extraction or collect its benefits; they observe from a rights-based frame that competes with the autonomy-primary narrative.
narrative_ontology:constraint_stakeholder(dignified_death__autonomy_primary, disability_rights_advocates, observer,
    organized, biographical, analytical, national).

% Hold that intentional life-termination is categorically impermissible regardless of consent. They are structurally excluded from the clinical and regulatory frame of the autonomy-primary reading, which brackets sanctity claims as irrelevant to the patient's dignity interest.
narrative_ontology:constraint_stakeholder(dignified_death__autonomy_primary, sanctity_advocates, excluded,
    organized, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a structured, medically supervised pathway for terminally ill individuals to end their lives, resolving the uncertainty and potential violence of unsanctioned suicide while nominally preserving patient self-determination through procedural safeguards.
% TRANSFER_FUNCTION: Moves authority over the timing and method of death from the unilateral control of the suffering individual to a mediated gatekeeping structure where medical professionals grant or withhold access based on eligibility criteria; transfers the burden of prolonged suffering onto those who fail to qualify.
% ABSENT_VOICES: Individuals experiencing existential suffering without terminal illness, those with psychiatric conditions seeking death, and sanctity-of-life advocates are structurally excluded from the autonomy frame; their objections are treated as outside the dignity discourse.
% DISAPPEARANCE_RATIONALE: If the constraint vanished, eligible patients would lose the protected, medically sanctioned pathway and revert to covert or violent methods; the medical profession would lose its gatekeeping role; the state would lose its regulatory apparatus; ineligible patients would no longer be formally denied by a declared procedure â the entire field of end-of-life decision-making would reorganize around either pure prohibition or pure autonomy.
% FOUNDING_PROBLEM: Unsanctioned suicide is often violent, lonely, and legally fraught; medical paternalism historically overrode patient wishes entirely; the absence of a recognized framework left dying individuals and families without clarity or protection.
% FOUNDING_PROBLEM_CORROBORATION: Medical ethicists and patient advocacy organizations outside the benefiting gatekeeping profession attest that the coordination function is real for some; disability rights organizations and critical bioethicists from outside the beneficiary set attest that the gatekeeping function has become a new source of harm.
narrative_ontology:disappearance_verdict(dignified_death__autonomy_primary, world_rearranges).
narrative_ontology:founding_problem_status(dignified_death__autonomy_primary, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(dignified_death__autonomy_primary, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(dignified_death__autonomy_primary, 'none', 1).
narrative_ontology:epsilon_provenance(dignified_death__autonomy_primary, 0.58, 'kimi-k2.6', 'none', direct).

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
 *   Extractiveness is set at 0.58 because the gatekeeping denies exit to a substantial subset of suffering individuals who fall outside eligibility. Suppression is high at 0.82 because the state actively enforces the boundary between approved and unapproved death through criminal law and professional licensing. Theater ratio is 0.45 because safeguards such as repeated psychiatric assessments and cooling-off periods perform respect for autonomy while functionally filtering access. Accessibility collapse is 0.78 because illegal alternatives are high-risk and the framework presents itself as the only legitimate path, causing alternatives to collapse once the patient enters the medical system. Resistance is 0.55 from disability advocates and sanctity coalitions who contest the framework's expansion.
 *
 * PERSPECTIVAL GAP:
 *   The eligible_patient seat experiences the constraint as rope-likeâa protected pathway to a dignified death. The ineligible_patient seat experiences it as snare-like: an actively enforced denial of exit dressed in the language of autonomy. The state seat experiences it as a governance scaffold maintaining moral order. The engine computes this divergence from the same structural data; the authored claim of tangled_rope does not adjudicate which seat is correct.
 *
 * DIRECTIONALITY LOGIC:
 *   The state_medical_authority sits near the beneficiary end: it defines the rules and maintains regulatory control over the life-death boundary, collecting moral and political order. Eligible_patients sit near the beneficiary end but with moderate d: they receive the coordinated exit, yet must conform to the state's timing and method. Medical_gatekeepers sit near the beneficiary end through secondary_role, collecting professional authority and fees. Ineligible_suffering_patients sit near the full-target end: they bear the costs of denied exit and are structurally trapped. Disability_rights_advocates and sanctity_advocates sit at the analytical periphery with high exit but no extraction or benefit.
 *
 * MANDATROPHY ANALYSIS:
 *   The arrangement was built to solve the problem of violent, unregulated suicide and unchecked medical paternalism. It retains a genuine coordination function for eligible patients, which prevents classification as pure snare. However, the asymmetric extraction from ineligible patients is enforced and substantial, preventing classification as pure rope. The tangled_rope type captures this hybridity: autonomy is the coordination story, but medical gatekeeping is the extraction mechanism.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    gatekeeping_as_extraction,
    'Is the medical eligibility framework an inevitable coordination cost of legalizing assisted dying, or a constructed barrier that could be removed while preserving patient dignity?',
    'Comparative jurisdictional analysis: if regimes with fewer gatekeeping steps produce comparable safety outcomes, the barrier is constructed extraction rather than necessary coordination cost.',
    'If the barrier is constructed, the constraint is more extractive than its coordination story claims and slides toward snare; if necessary, the extraction is the price of the rope function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(gatekeeping_as_extraction, conceptual, 'Whether gatekeeping is necessary coordination cost or constructed extraction').

omega_variable(
    eligibility_boundary_drift,
    'Does the eligibility boundary naturally expand to include more classes of suffering over time, or does it permanently exclude psychiatric, existential, and early-stage dementia cases regardless of evidence?',
    'Longitudinal tracking of legislative amendments and court decisions across jurisdictions that have legalized assisted dying.',
    'Persistent exclusion of broad suffering classes would confirm that the constraint extracts from a stable victim set; expansion would suggest the coordination function is absorbing more cases and the extraction ratio may fall.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(eligibility_boundary_drift, empirical, 'Whether eligibility expands or remains narrowly bounded').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression of non-sanctioned assisted death purely structural (criminal law, licensing barriers), or is it partially internalized by ineligible patients who accept the gatekeeping frame as legitimate?',
    'Post-exit trajectory analysis: whether ineligible patients who relocate to permissive jurisdictions still pursue assisted death at predicted rates, or whether they persist in their home jurisdiction despite legal pathways abroad.',
    'If internalized, effective suppression exceeds the structural measure because the target carries the constraint after exit; this would raise the computed extraction for the ineligible seat.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs internalized suppression mechanism').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dignified_death__autonomy_primary, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dign_tr_t0, dignified_death__autonomy_primary, theater_ratio, 0, 0.15).
narrative_ontology:measurement(dign_tr_t8, dignified_death__autonomy_primary, theater_ratio, 8, 0.25).
narrative_ontology:measurement(dign_tr_t16, dignified_death__autonomy_primary, theater_ratio, 16, 0.32).
narrative_ontology:measurement(dign_tr_t24, dignified_death__autonomy_primary, theater_ratio, 24, 0.38).
narrative_ontology:measurement(dign_tr_t32, dignified_death__autonomy_primary, theater_ratio, 32, 0.42).
narrative_ontology:measurement(dign_tr_t40, dignified_death__autonomy_primary, theater_ratio, 40, 0.45).

% Extraction over time
narrative_ontology:measurement(dign_be_t0, dignified_death__autonomy_primary, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(dign_be_t8, dignified_death__autonomy_primary, base_extractiveness, 8, 0.42).
narrative_ontology:measurement(dign_be_t16, dignified_death__autonomy_primary, base_extractiveness, 16, 0.5).
narrative_ontology:measurement(dign_be_t24, dignified_death__autonomy_primary, base_extractiveness, 24, 0.55).
narrative_ontology:measurement(dign_be_t32, dignified_death__autonomy_primary, base_extractiveness, 32, 0.58).
narrative_ontology:measurement(dign_be_t40, dignified_death__autonomy_primary, base_extractiveness, 40, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(dign_su_t0, dignified_death__autonomy_primary, suppression_requirement, 0, 0.85).
narrative_ontology:measurement(dign_su_t8, dignified_death__autonomy_primary, suppression_requirement, 8, 0.72).
narrative_ontology:measurement(dign_su_t16, dignified_death__autonomy_primary, suppression_requirement, 16, 0.75).
narrative_ontology:measurement(dign_su_t24, dignified_death__autonomy_primary, suppression_requirement, 24, 0.78).
narrative_ontology:measurement(dign_su_t32, dignified_death__autonomy_primary, suppression_requirement, 32, 0.8).
narrative_ontology:measurement(dign_su_t40, dignified_death__autonomy_primary, suppression_requirement, 40, 0.82).


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
