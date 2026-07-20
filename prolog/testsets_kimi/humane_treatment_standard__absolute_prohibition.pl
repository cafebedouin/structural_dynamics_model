% ============================================================================
% CONSTRAINT STORY: humane_treatment_standard__absolute_prohibition
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_humane_treatment_standard__absolute_prohibition, []).

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
 *   constraint_id: humane_treatment_standard__absolute_prohibition
 *   human_readable: Common Article 3 Absolute Prohibition of Torture and Degrading Treatment
 *   domain: international_humanitarian_law/state_security/human_rights
 *
 * SUMMARY:
 *   Common Article 3 of the 1949 Geneva Conventions establishes a
 *   non-derogable minimum standard prohibiting torture and degrading
 *   treatment in non-international armed conflicts. This constraint story
 *   instantiates the absolute prohibition reading of the
 *   humane_treatment_standard kernel, which holds that no
 *   circumstanceâincluding supreme security imperativesâpermits crossing
 *   this threshold. The reading treats detainees as full rights-holders and
 *   states as absolutely constrained. Sibling readings (contextual_necessity
 *   and proportionality_balancing) interpret the same treaty text as
 *   permitting security overrides or balancing tests; they are structurally
 *   distinct constraints linked through the kernel family network.
 *
 * KEY AGENTS:
 *   - Detainees: Primary beneficiaries (powerless/trapped) â depend on the absolute prohibition for physical protection.
 *   - Civilian populations in conflict: Secondary beneficiaries (powerless/trapped) â protected by the normative floor.
 *   - State parties: Agenda-setters with payer secondary role (institutional/constrained) â established the norm but bear compliance costs.
 *   - Non-state armed groups: Payers (organized/constrained) â bound by customary law without having shaped the treaty.
 *   - International courts: Agenda-setters (institutional/analytical) â enforce through criminal prosecution.
 *   - ICRC: Observers (institutional/analytical) â monitor, interpret, and promote compliance.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(humane_treatment_standard__absolute_prohibition, 0.1).
domain_priors:suppression_score(humane_treatment_standard__absolute_prohibition, 0.3).
domain_priors:theater_ratio(humane_treatment_standard__absolute_prohibition, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(humane_treatment_standard__absolute_prohibition, extractiveness, 0.1).
narrative_ontology:constraint_metric(humane_treatment_standard__absolute_prohibition, suppression_requirement, 0.3).
narrative_ontology:constraint_metric(humane_treatment_standard__absolute_prohibition, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(humane_treatment_standard__absolute_prohibition, accessibility_collapse, 0.82).
narrative_ontology:constraint_metric(humane_treatment_standard__absolute_prohibition, resistance, 0.25).

% --- Constraint claim ---
narrative_ontology:constraint_claim(humane_treatment_standard__absolute_prohibition, rope).
narrative_ontology:human_readable(humane_treatment_standard__absolute_prohibition, "Common Article 3 Absolute Prohibition of Torture and Degrading Treatment").
narrative_ontology:topic_domain(humane_treatment_standard__absolute_prohibition, "international_humanitarian_law/state_security/human_rights").

domain_priors:requires_active_enforcement(humane_treatment_standard__absolute_prohibition).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(humane_treatment_standard__absolute_prohibition, 'f7ef47ca-30cb-4984-8ff9-237f5ad6875c').
narrative_ontology:cs_kernel_codification('f7ef47ca-30cb-4984-8ff9-237f5ad6875c', fixed_text).
narrative_ontology:cs_authority_grounding('f7ef47ca-30cb-4984-8ff9-237f5ad6875c', lineage).
narrative_ontology:cs_interpretation_layer_present('f7ef47ca-30cb-4984-8ff9-237f5ad6875c').
narrative_ontology:cs_reading_relation('f7ef47ca-30cb-4984-8ff9-237f5ad6875c', humane_treatment_standard__contextual_necessity, forecloses).
narrative_ontology:cs_reading_relation('f7ef47ca-30cb-4984-8ff9-237f5ad6875c', humane_treatment_standard__proportionality_balancing, forecloses).
narrative_ontology:cs_axiom('f7ef47ca-30cb-4984-8ff9-237f5ad6875c', foundational, torture_and_degrading_treatment_categorically_prohibited).
narrative_ontology:cs_axiom_status(torture_and_degrading_treatment_categorically_prohibited, holdable).
narrative_ontology:cs_axiom_grounding('f7ef47ca-30cb-4984-8ff9-237f5ad6875c', torture_and_degrading_treatment_categorically_prohibited, deontological).
narrative_ontology:cs_axiom('f7ef47ca-30cb-4984-8ff9-237f5ad6875c', foundational, detainee_inherent_dignity_non_derogable).
narrative_ontology:cs_axiom_status(detainee_inherent_dignity_non_derogable, holdable).
narrative_ontology:cs_axiom_grounding('f7ef47ca-30cb-4984-8ff9-237f5ad6875c', detainee_inherent_dignity_non_derogable, deontological).
narrative_ontology:cs_reference_frame('f7ef47ca-30cb-4984-8ff9-237f5ad6875c', inviolable_human_dignity_framework).
narrative_ontology:cs_drift_state('f7ef47ca-30cb-4984-8ff9-237f5ad6875c', contemporary_counterterrorism_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('f7ef47ca-30cb-4984-8ff9-237f5ad6875c', '').
narrative_ontology:cs_kernel_id(humane_treatment_standard__absolute_prohibition, humane_treatment_standard).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(humane_treatment_standard__absolute_prohibition, detainees).
narrative_ontology:constraint_beneficiary(humane_treatment_standard__absolute_prohibition, civilian_populations_in_conflict).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(humane_treatment_standard__absolute_prohibition, state_parties).
narrative_ontology:constraint_victim(humane_treatment_standard__absolute_prohibition, non_state_armed_groups).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Persons taking no active part in hostilities who are detained during non-international armed conflicts. They depend entirely on the constraint for protection against torture and degrading treatment; they cannot exit the detention context or opt out of the legal regime.
narrative_ontology:constraint_stakeholder(humane_treatment_standard__absolute_prohibition, detainees, beneficiary,
    powerless, immediate, trapped, global).

% Non-combatants in internal conflict zones who benefit from the normative floor that limits how conflict parties treat those under their control. Exit options are constrained by geography and conflict dynamics.
narrative_ontology:constraint_stakeholder(humane_treatment_standard__absolute_prohibition, civilian_populations_in_conflict, beneficiary,
    powerless, immediate, trapped, global).

% States that have ratified the Geneva Conventions and are bound by Common Article 3 in all non-international armed conflicts. They established the norm but are absolutely constrained by it; no security exception permits derogation. Compliance requires restraining security services and accepting that certain interrogation methods are legally barred.
narrative_ontology:constraint_stakeholder(humane_treatment_standard__absolute_prohibition, state_parties, agenda_setter,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(humane_treatment_standard__absolute_prohibition, state_parties, payer).

% Armed opposition groups in internal conflicts that are bound by Common Article 3 as a matter of customary international law. They are constrained from using torture against detainees and civilians but lack the treaty-making power that states possess.
narrative_ontology:constraint_stakeholder(humane_treatment_standard__absolute_prohibition, non_state_armed_groups, payer,
    organized, biographical, constrained, national).

% International criminal tribunals and the International Criminal Court that prosecute violations of Common Article 3 as war crimes. They interpret the absolute nature of the prohibition and adjudicate state and non-state conduct.
narrative_ontology:constraint_stakeholder(humane_treatment_standard__absolute_prohibition, international_courts, agenda_setter,
    institutional, generational, analytical, global).

% The International Committee of the Red Cross promotes and monitors compliance with Common Article 3, interprets its scope through commentaries, and engages conflict parties confidentially on violations.
narrative_ontology:constraint_stakeholder(humane_treatment_standard__absolute_prohibition, icrc, observer,
    institutional, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a minimum baseline of humane treatment for all persons taking no active part in hostilities during non-international armed conflicts, enabling reciprocal restraint among conflict parties and protecting those rendered defenseless.
% TRANSFER_FUNCTION: Transfers the burden of restraint from unregulated military necessity to legally mandated minimum standards; moves detainees from unprotected status to protected rights-holder status.
% ABSENT_VOICES: Victims of past torture whose testimony would demonstrate the practical cost of dilution; non-state armed groups who reject the treaty framework entirely and are not represented in its negotiation or interpretation.
% DISAPPEARANCE_RATIONALE: The entire edifice of international humanitarian law minimum standards depends on this absolute floor; its disappearance would legitimize torture and degrading treatment, causing immediate rearrangement of detention practices and international legal accountability structures.
% FOUNDING_PROBLEM: The absence of any legal protection for detainees and non-combatants in internal armed conflicts prior to 1949, resulting in unchecked brutality against those hors de combat.
% FOUNDING_PROBLEM_CORROBORATION: Third-party historians and ICRC archives document pre-1949 internal conflict atrocities; the UN War Crimes Commission records corroborate the absence of legal protections. Contemporary international tribunals (ICTY, ICTR) affirmed the historical necessity of the standard independent of state self-interest.
narrative_ontology:disappearance_verdict(humane_treatment_standard__absolute_prohibition, world_rearranges).
narrative_ontology:founding_problem_status(humane_treatment_standard__absolute_prohibition, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(humane_treatment_standard__absolute_prohibition, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(humane_treatment_standard__absolute_prohibition, 'none', 1).
narrative_ontology:epsilon_provenance(humane_treatment_standard__absolute_prohibition, 0.1, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(humane_treatment_standard__absolute_prohibition_tests).
:- end_tests(humane_treatment_standard__absolute_prohibition_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.10) because the constraint does not extract rents; it imposes behavioral costs of compliance on conflict parties. Suppression is moderate (0.30) because the prohibition actively forecloses the alternative of torture through legal enforcement, but this is normative closure rather than coercive extraction. Theater ratio is low-moderate (0.20): genuine compliance efforts dominate, though performative claims of compliance by states practicing 'enhanced interrogation' have risen since the counterterrorism era. Accessibility collapse is very high (0.82) because once the absolute norm is internalized, the legal alternative of authorized torture collapses completely. Resistance is low (0.25) because the norm enjoys near-universal acceptance, though rogue doctrines periodically challenge it. The metrics are authored independently of the rope claim; if the engine detects divergence, that signals either hidden extraction or misidentification of the coordination function.
 *
 * PERSPECTIVAL GAP:
 *   Detainees experience the constraint as essential bodily protection; states experience it as a binding limitation on sovereignty and security policy; international courts experience it as a mandatory legal command. The engine computes per-seat classifications from these structural positions: detainees sit at the beneficiary end, states and armed groups sit toward the payer end, and courts sit in the analytical position.
 *
 * DIRECTIONALITY LOGIC:
 *   Detainees and civilian populations are declared beneficiaries, yielding low directionality (subsidy/protection). States and non-state armed groups are not declared victims because they are not targets of extraction; they are symmetric coordination participants who bear compliance costs. Their directionality derives from their constrained exit options and payer/agenda-setter roles, placing them in a symmetric-to-moderate range. No directionality overrides are needed because the structural derivation accurately captures the relationship.
 *
 * MANDATROPHY ANALYSIS:
 *   Classifying this constraint as rope rather than mountain acknowledges that it is a constructed legal norm requiring active enforcement and institutional maintenance, not a natural law. Classifying it as rope rather than snare prevents misidentifying states as victims of extraction; states bear compliance costs but are coordinated participants in a reciprocal regime. If enforcement were shown to asymmetrically target weak states while powerful states evade accountability, the computed classification could shift toward tangled_rope or snare, which would indicate that coordination has been captured by geopolitical extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    jus_cogens_vs_positive_law,
    'Is the absolute prohibition of torture a peremptory norm (jus cogens) existing independently of treaty consent, or is it a positive legal construction dependent on state agreement?',
    'Analysis of state practice and opinio juris for universal acceptance independent of Geneva Convention ratification; examination of ICJ and ICC jurisprudence treating the norm as non-derogable.',
    'If purely positive law, the constraint is vulnerable to state repudiation and functions as enforced coordination; if jus cogens, it approaches mountain-like immunity to unilateral withdrawal.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(jus_cogens_vs_positive_law, conceptual, 'Whether the prohibition derives from natural law or treaty construct').

omega_variable(
    enforcement_asymmetry,
    'Does the enforcement of the absolute prohibition asymmetrically target weak states and non-state actors while powerful states evade accountability?',
    'Empirical survey of international prosecutions, universal jurisdiction cases, and UN Human Rights Council special procedures by target state power.',
    'If enforcement is strongly asymmetric, the constraint may function as a tangled rope or snare in practice despite its rope classification, converting legal coordination into geopolitical extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_asymmetry, empirical, 'Whether enforcement patterns reveal asymmetric extraction').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(humane_treatment_standard__absolute_prohibition, 0, 75).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(humane_treatment_absolute_tr_t0, humane_treatment_standard__absolute_prohibition, theater_ratio, 0, 0.05).
narrative_ontology:measurement(humane_treatment_absolute_tr_t15, humane_treatment_standard__absolute_prohibition, theater_ratio, 15, 0.05).
narrative_ontology:measurement(humane_treatment_absolute_tr_t30, humane_treatment_standard__absolute_prohibition, theater_ratio, 30, 0.08).
narrative_ontology:measurement(humane_treatment_absolute_tr_t45, humane_treatment_standard__absolute_prohibition, theater_ratio, 45, 0.12).
narrative_ontology:measurement(humane_treatment_absolute_tr_t60, humane_treatment_standard__absolute_prohibition, theater_ratio, 60, 0.18).
narrative_ontology:measurement(humane_treatment_absolute_tr_t75, humane_treatment_standard__absolute_prohibition, theater_ratio, 75, 0.2).

% Extraction over time
narrative_ontology:measurement(humane_treatment_absolute_be_t0, humane_treatment_standard__absolute_prohibition, base_extractiveness, 0, 0.05).
narrative_ontology:measurement(humane_treatment_absolute_be_t15, humane_treatment_standard__absolute_prohibition, base_extractiveness, 15, 0.05).
narrative_ontology:measurement(humane_treatment_absolute_be_t30, humane_treatment_standard__absolute_prohibition, base_extractiveness, 30, 0.06).
narrative_ontology:measurement(humane_treatment_absolute_be_t45, humane_treatment_standard__absolute_prohibition, base_extractiveness, 45, 0.08).
narrative_ontology:measurement(humane_treatment_absolute_be_t60, humane_treatment_standard__absolute_prohibition, base_extractiveness, 60, 0.09).
narrative_ontology:measurement(humane_treatment_absolute_be_t75, humane_treatment_standard__absolute_prohibition, base_extractiveness, 75, 0.1).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(humane_treatment_standard__absolute_prohibition, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(humane_treatment_standard__absolute_prohibition, enforcement_mechanism).
narrative_ontology:affects_constraint(humane_treatment_standard__absolute_prohibition, humane_treatment_standard__contextual_necessity).
narrative_ontology:affects_constraint(humane_treatment_standard__absolute_prohibition, humane_treatment_standard__proportionality_balancing).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the humane_treatment_standard kernel. The absolute prohibition reading, contextual necessity reading, and proportionality balancing reading are structurally distinct constraints that share a common treaty text but instantiate different epsilon values and stakeholder relationships.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
