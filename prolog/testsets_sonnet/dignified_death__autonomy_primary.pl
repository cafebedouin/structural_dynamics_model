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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
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
 *   human_readable: Autonomy-Primary Reading of the Right to Die (Medical Gatekeeping of Self-Determined Death)
 *   domain: bioethics/medical_law/political_philosophy
 *
 * SUMMARY:
 *   This story instantiates the autonomy_primary reading of the contested
 *   dignified_death kernel: dignity is located in self-determination, and the
 *   suffering individual holds final authority over the timing and method of
 *   their own death. Under this reading, state prohibition on assisted death
 *   is a high-extraction constraint on the person denied exit, while
 *   jurisdictions that have legalized the right operate a tangled-rope
 *   structure — a genuine coordination function (safe, consented, supervised
 *   death replacing violent or unsupervised suicide) entangled with a medical
 *   gatekeeping apparatus (eligibility criteria, capacity assessment, waiting
 *   periods) whose administration determines, in practice, who among nominal
 *   rights-holders can actually exercise the right. This is one of three
 *   sibling readings of the same kernel (sanctity_primary and
 *   relational_autonomy are separate constraint files, not measured here) —
 *   per the ε-invariance principle, this file measures only this reading's
 *   structure and does not average across, hedge, or describe the contest
 *   with the siblings inside its own classification.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dignified_death__autonomy_primary, 0.52).
domain_priors:suppression_score(dignified_death__autonomy_primary, 0.68).
domain_priors:theater_ratio(dignified_death__autonomy_primary, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dignified_death__autonomy_primary, extractiveness, 0.52).
narrative_ontology:constraint_metric(dignified_death__autonomy_primary, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(dignified_death__autonomy_primary, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(dignified_death__autonomy_primary, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(dignified_death__autonomy_primary, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dignified_death__autonomy_primary, tangled_rope).
narrative_ontology:human_readable(dignified_death__autonomy_primary, "Autonomy-Primary Reading of the Right to Die (Medical Gatekeeping of Self-Determined Death)").
narrative_ontology:topic_domain(dignified_death__autonomy_primary, "bioethics/medical_law/political_philosophy").

domain_priors:requires_active_enforcement(dignified_death__autonomy_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dignified_death__autonomy_primary, '8c3c9ef6-6ac6-44b0-8b7a-b0292582c63a').
narrative_ontology:cs_kernel_codification('8c3c9ef6-6ac6-44b0-8b7a-b0292582c63a', distributed).
narrative_ontology:cs_authority_grounding('8c3c9ef6-6ac6-44b0-8b7a-b0292582c63a', distributed).
narrative_ontology:cs_reading_relation('8c3c9ef6-6ac6-44b0-8b7a-b0292582c63a', dignified_death__sanctity_primary, forecloses).
narrative_ontology:cs_reading_relation('8c3c9ef6-6ac6-44b0-8b7a-b0292582c63a', dignified_death__relational_autonomy, influences).
narrative_ontology:cs_axiom('8c3c9ef6-6ac6-44b0-8b7a-b0292582c63a', foundational, individual_will_is_final_authority_over_own_death).
narrative_ontology:cs_axiom_status(individual_will_is_final_authority_over_own_death, holdable).
narrative_ontology:cs_axiom_grounding('8c3c9ef6-6ac6-44b0-8b7a-b0292582c63a', individual_will_is_final_authority_over_own_death, deontological).
narrative_ontology:cs_axiom('8c3c9ef6-6ac6-44b0-8b7a-b0292582c63a', secondary, bodily_autonomy_extends_to_timing_of_death).
narrative_ontology:cs_axiom_status(bodily_autonomy_extends_to_timing_of_death, holdable).
narrative_ontology:cs_axiom_grounding('8c3c9ef6-6ac6-44b0-8b7a-b0292582c63a', bodily_autonomy_extends_to_timing_of_death, deontological).
narrative_ontology:cs_reference_frame('8c3c9ef6-6ac6-44b0-8b7a-b0292582c63a', common_law_bodily_autonomy_tradition).
narrative_ontology:cs_drift_state('8c3c9ef6-6ac6-44b0-8b7a-b0292582c63a', post_legalization_wave_contemporary, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('8c3c9ef6-6ac6-44b0-8b7a-b0292582c63a', '').
narrative_ontology:cs_kernel_id(dignified_death__autonomy_primary, dignified_death).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dignified_death__autonomy_primary, terminally_ill_patients_granted_access).
narrative_ontology:constraint_beneficiary(dignified_death__autonomy_primary, autonomy_rights_advocacy_organizations).
narrative_ontology:constraint_victim(dignified_death__autonomy_primary, suffering_patients_denied_exit).
narrative_ontology:constraint_victim(dignified_death__autonomy_primary, patients_failing_eligibility_criteria).
narrative_ontology:constraint_victim(dignified_death__autonomy_primary, patients_in_prohibition_jurisdictions).
narrative_ontology:constraint_vindicates(dignified_death__autonomy_primary, self_determination_grounds_dignity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Experience prolonged, unwanted suffering under state prohibition or restrictive eligibility rules and cannot obtain a legally sanctioned assisted death even when they judge their own suffering unbearable. Their only formal exits are suicide outside medical supervision, travel to a permissive jurisdiction if resources allow, or continued endurance. The autonomy framework names their situation as the core injustice but cannot itself remove the legal prohibition.
narrative_ontology:constraint_stakeholder(dignified_death__autonomy_primary, suffering_patients_denied_exit, payer,
    powerless, immediate, trapped, national).

% Live in a jurisdiction that has legalized assisted death but are excluded by prognosis thresholds (e.g. terminal-only rules), capacity assessments, mandatory waiting periods, or psychiatric evaluation requirements. They hold the autonomy claim in principle but the medical gatekeeping apparatus built to administer the right denies them its exercise in practice.
narrative_ontology:constraint_stakeholder(dignified_death__autonomy_primary, patients_failing_eligibility_criteria, payer,
    powerless, immediate, trapped, regional).

% Meet eligibility criteria in a permissive jurisdiction and successfully obtain a legally supervised assisted death on their own timeline. The autonomy-primary framework directly serves them: the coordination function (safe, witnessed, consented procedure) and the self-determination claim converge for this group specifically.
narrative_ontology:constraint_stakeholder(dignified_death__autonomy_primary, terminally_ill_patients_granted_access, beneficiary,
    moderate, immediate, mobile, regional).

% Advance the autonomy-primary framing in litigation, legislative campaigns, and public discourse. They benefit reputationally and organizationally from every legislative win, and they set the terms of the eligibility debate — pushing for expanded criteria while accepting some gatekeeping as the political price of legalization.
narrative_ontology:constraint_stakeholder(dignified_death__autonomy_primary, autonomy_rights_advocacy_organizations, beneficiary,
    organized, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(dignified_death__autonomy_primary, autonomy_rights_advocacy_organizations, agenda_setter).

% Administer the eligibility criteria, capacity assessments, and procedural safeguards that convert the abstract autonomy right into an operational medical pathway. They enforce waiting periods and second-opinion requirements, and their liability exposure shapes how conservatively the criteria are interpreted, which directly determines who among nominal rights-holders actually obtains access.
narrative_ontology:constraint_stakeholder(dignified_death__autonomy_primary, medical_licensing_and_review_boards, agenda_setter,
    institutional, generational, analytical, national).

% Reside where assisted death remains criminalized entirely. They have no legal pathway to invoke the autonomy claim at all; their only options are unassisted suicide, extralegal assistance carrying criminal risk to helpers, or 'death tourism' to a permissive jurisdiction if they have the means and mobility to travel while still capable.
narrative_ontology:constraint_stakeholder(dignified_death__autonomy_primary, patients_in_prohibition_jurisdictions, excluded,
    powerless, immediate, trapped, national).

% Warn that autonomy-primary framing, combined with underfunded palliative and disability support systems, produces situations where the 'choice' to die is shaped by unaddressed material deprivation rather than free self-determination. Their objection is aired in policy hearings but is generally treated as a caution about implementation rather than as authority over the framework's core premise.
narrative_ontology:constraint_stakeholder(dignified_death__autonomy_primary, disability_rights_advocates, excluded,
    organized, generational, constrained, national).

% Adjudicate the boundary of the legal right, weighing autonomy claims against sanctity-of-life objections and disability-rights concerns, and set or withhold the statutory or constitutional basis for the entire framework. Their rulings determine which jurisdiction a given patient lives in relative to the prohibition/permission line.
narrative_ontology:constraint_stakeholder(dignified_death__autonomy_primary, state_legislatures_and_courts, observer,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(dignified_death__autonomy_primary, state_legislatures_and_courts, agenda_setter).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a legally supervised, consented, witnessed pathway for a person judged to have decision-making capacity to end unbearable suffering on their own terms, replacing unsupervised or violent self-termination with a medically managed process.
% TRANSFER_FUNCTION: Moves final authority over the timing and method of death from the state/medical establishment's default prohibition to the individual patient, but only within a channel gatekept by eligibility criteria, capacity assessments, and procedural waiting periods administered by medical and legal authorities.
% ABSENT_VOICES: Disability rights advocates and patients in prohibition jurisdictions would object that autonomy framing masks material coercion (inadequate care, financial precarity) or simply does not apply to them at all; the first group is heard in hearings but treated as a caveat, the second group is not inside the legal conversation because their jurisdiction has not opened one.
% DISAPPEARANCE_RATIONALE: If the autonomy-primary framework and its accompanying legal infrastructure vanished, permissive jurisdictions would revert to blanket prohibition overnight, patients currently mid-process would lose access, advocacy organizations would lose their operative legal claim, and the population of patients now dying on a chosen timeline would instead face prolonged terminal suffering or unsupervised suicide — a substantial rearrangement of end-of-life practice.
% FOUNDING_PROBLEM: Terminally or unbearably suffering individuals were dying badly — through violent unsupervised suicide, prolonged agony under medical futility, or being kept alive against their expressed wishes by defensive medicine and blanket criminal prohibition on assistance.
% FOUNDING_PROBLEM_CORROBORATION: Palliative care physicians and independent bioethics commissions in jurisdictions that have studied outcomes (e.g. Oregon Death with Dignity Act annual reports, Canadian MAID federal review) corroborate that a subset of the founding problem — violent unsupervised suicide, refractory pain — is measurably reduced; disability rights researchers and some palliative care organizations, from outside the advocacy coalition, corroborate that a distinct problem (inadequate social and palliative support driving requests) remains live and is not addressed by the autonomy framework alone.
narrative_ontology:disappearance_verdict(dignified_death__autonomy_primary, world_rearranges).
narrative_ontology:founding_problem_status(dignified_death__autonomy_primary, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(dignified_death__autonomy_primary, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(dignified_death__autonomy_primary, 'none', 1).
narrative_ontology:epsilon_provenance(dignified_death__autonomy_primary, 0.52, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness (0.52 at interval end) reflects the state-prohibition side of the constraint: for patients under blanket prohibition or failing eligibility criteria, the constraint extracts continued suffering against expressed will, which is the harm the autonomy framework itself names as the founding injustice. Suppression is high (0.68, declining from 0.82) because the constraint's persistence has depended on criminal law backing the prohibition and, within legalized jurisdictions, procedural machinery (waiting periods, mandatory second opinions) that functions as a soft suppression layer even after the underlying right is recognized; the decline over the interval tracks liberalizing case law and expanding eligibility criteria in early-adopter jurisdictions. Theater ratio (0.40, declining from 0.50) captures that a meaningful share of the gatekeeping apparatus performs caution (defensive documentation, redundant capacity assessments) beyond what patient safety strictly requires, though this has been declining as clinical protocols matured. All three metrics ride one shared time grid.
 *
 * PERSPECTIVAL GAP:
 *   From the seat of a patient granted access, this reads as rope: a genuine, well-functioning coordination mechanism that delivered exactly what was needed. From the seat of a patient failing eligibility criteria or living under prohibition, the same legal-medical apparatus reads as extraction dressed in the language of the right meant to serve them — suffering continues, but now justified by the existence of a right they cannot access. The engine computes these as different per-seat classifications from the same structural data; the divergence is the finding, not an inconsistency to resolve.
 *
 * DIRECTIONALITY LOGIC:
 *   Suffering patients denied exit — either by blanket prohibition or by failing eligibility criteria within a legalized regime — are the clearest victims: high d, trapped exit, the constraint's operation directly extracts continued unwanted suffering from them. Terminally ill patients who are granted access are the direct beneficiaries: low d, the coordination function serves them as designed. Autonomy rights advocacy organizations are secondary beneficiaries whose organizational and reputational position depends on continued legal contest — they benefit from expansion but also from the fight itself, which is why they hold agenda_setter as a secondary role. Medical licensing and review boards sit as institutional agenda-setters whose liability exposure creates an incentive toward conservative interpretation of eligibility criteria, which is the mechanism generating the tangled-rope's asymmetric extraction from patients who technically qualify but face a slow or restrictive administrative pathway.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (violent unsupervised death, prolonged suffering under blanket prohibition) is only partially resolved by the autonomy-primary framework as implemented: it is resolved for the subset of patients who both live in a permissive jurisdiction and meet its eligibility criteria, and remains live for everyone else the framework claims to serve. This split status — corroborated by outcome data on one side and by disability-rights and palliative-access research on the other — is exactly the founding_problem_status: contested finding the six-questions genealogy is built to surface, rather than letting the advocacy coalition's self-report of success stand as the only account.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    autonomy_vs_sanctity_kernel_disagreement,
    'Is the correct locus of dignity self-determination (this reading), life''s intrinsic sanctity (sanctity_primary), or a relational construct distributed across patient-family-clinician (relational_autonomy)? This is the kernel-level disagreement this story does not adjudicate.',
    'Not empirically resolvable — this is a foundational normative disagreement about where moral authority over death resides. It is tracked structurally via cs_structure.reading_relations and cs_structure.axioms rather than resolved within this file.',
    'If sanctity_primary were adopted instead, the entire beneficiary/victim structure of this story inverts: the ''victims'' named here (those denied exit) would instead be understood as protected from a rights violation, and the ''beneficiaries'' (those granted access) would be reclassified as harmed by a permissive regime. The classification is reading-dependent by design, not an error to fix.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(autonomy_vs_sanctity_kernel_disagreement, preference, 'Which kernel reading (autonomy_primary, sanctity_primary, relational_autonomy) is normatively correct is not decidable within this story; each reading is authored as its own constraint file.').

omega_variable(
    eligibility_criteria_as_coordination_vs_extraction,
    'Are medical eligibility criteria (terminal prognosis thresholds, capacity assessments, waiting periods) a necessary safeguard against coercion and error, or do they function primarily as a rationing mechanism that converts a nominal right into a narrow, gatekept privilege?',
    'Comparative outcome studies across jurisdictions with different eligibility stringency (e.g. Oregon''s terminal-only model vs. Canada''s post-2021 non-terminal eligibility expansion) tracking rates of denial, appeal, and reported patient distress from delay.',
    'If criteria function mainly as safeguard, the tangled_rope''s enforcement component is closer to genuine coordination cost; if they function mainly as rationing, the extraction component is larger than the base metrics currently reflect, and the classification would drift toward snare for the subpopulation who technically qualify but face high administrative friction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(eligibility_criteria_as_coordination_vs_extraction, empirical, 'Whether eligibility gatekeeping is a genuine safeguard or a rationing/extraction mechanism riding on the coordination function.').

omega_variable(
    material_coercion_confound,
    'To what extent are requests for assisted death, even among eligible and legally autonomous patients, shaped by unmet material needs (inadequate palliative care, financial precarity, caregiver burden) rather than by autonomous preference in the philosophically thick sense the framework assumes?',
    'Longitudinal studies tracking whether access to expanded palliative and disability support services changes rates or reported motivations for assisted-death requests within the same jurisdiction.',
    'If material deprivation substantially drives requests, the ''autonomous agent'' beneficiary category is partly a false summit — the choice is less free than the framework claims, which would strengthen the case for the relational_autonomy sibling reading over this one for at least a subset of cases.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(material_coercion_confound, empirical, 'Whether autonomy as measured is confounded by unaddressed material coercion, which would blur this reading''s beneficiary/victim boundary.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dignified_death__autonomy_primary, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dign_tr_t0, dignified_death__autonomy_primary, theater_ratio, 0, 0.5).
narrative_ontology:measurement(dign_tr_t6, dignified_death__autonomy_primary, theater_ratio, 6, 0.47).
narrative_ontology:measurement(dign_tr_t12, dignified_death__autonomy_primary, theater_ratio, 12, 0.44).
narrative_ontology:measurement(dign_tr_t18, dignified_death__autonomy_primary, theater_ratio, 18, 0.42).
narrative_ontology:measurement(dign_tr_t24, dignified_death__autonomy_primary, theater_ratio, 24, 0.41).
narrative_ontology:measurement(dign_tr_t30, dignified_death__autonomy_primary, theater_ratio, 30, 0.4).

% Extraction over time
narrative_ontology:measurement(dign_be_t0, dignified_death__autonomy_primary, base_extractiveness, 0, 0.58).
narrative_ontology:measurement(dign_be_t6, dignified_death__autonomy_primary, base_extractiveness, 6, 0.56).
narrative_ontology:measurement(dign_be_t12, dignified_death__autonomy_primary, base_extractiveness, 12, 0.54).
narrative_ontology:measurement(dign_be_t18, dignified_death__autonomy_primary, base_extractiveness, 18, 0.53).
narrative_ontology:measurement(dign_be_t24, dignified_death__autonomy_primary, base_extractiveness, 24, 0.52).
narrative_ontology:measurement(dign_be_t30, dignified_death__autonomy_primary, base_extractiveness, 30, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(dign_su_t0, dignified_death__autonomy_primary, suppression_requirement, 0, 0.82).
narrative_ontology:measurement(dign_su_t6, dignified_death__autonomy_primary, suppression_requirement, 6, 0.78).
narrative_ontology:measurement(dign_su_t12, dignified_death__autonomy_primary, suppression_requirement, 12, 0.74).
narrative_ontology:measurement(dign_su_t18, dignified_death__autonomy_primary, suppression_requirement, 18, 0.71).
narrative_ontology:measurement(dign_su_t24, dignified_death__autonomy_primary, suppression_requirement, 24, 0.69).
narrative_ontology:measurement(dign_su_t30, dignified_death__autonomy_primary, suppression_requirement, 30, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dignified_death__autonomy_primary, enforcement_mechanism).
narrative_ontology:affects_constraint(dignified_death__autonomy_primary, dignified_death__sanctity_primary).
narrative_ontology:affects_constraint(dignified_death__autonomy_primary, dignified_death__relational_autonomy).

% DUAL FORMULATION NOTE:
% This is one of three sibling readings of the dignified_death kernel, each authored as a separate constraint file per the ε-invariance principle: autonomy_primary (this file, tangled_rope, patient self-determination as final authority), sanctity_primary (life's intrinsic value forecloses intentional termination regardless of consent), and relational_autonomy (decision authority distributed across patient-family-clinician triad). Each carries its own ε, beneficiary/victim structure, and classification; they are linked here for contamination-propagation analysis, not merged.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
