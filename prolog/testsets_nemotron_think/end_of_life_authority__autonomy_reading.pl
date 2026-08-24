% ============================================================================
% CONSTRAINT STORY: end_of_life_authority__autonomy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
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
 *   constraint_id: end_of_life_authority__autonomy_reading
 *   human_readable: Autonomy-Based Assisted Dying Framework
 *   domain: medical_ethics/bioethics/end_of_life_policy
 *
 * SUMMARY:
 *   This constraint story models the autonomy-based reading of end-of-life
 *   authority: a legal framework that grants individuals facing unbearable
 *   suffering the right to request medical assistance in dying. The framework
 *   is instantiated in jurisdictions such as Canada (MAID), the Netherlands,
 *   Belgium, and several US states. It is contested by sanctity-of-life
 *   readings and by empirical concerns about eligibility expansion (slippery
 *   slope). The constraint coordinates a solution to the problem of
 *   prohibited compassionate assistance but extracts risk from vulnerable
 *   populations who may be pressured into choosing death. Eligibility
 *   criteria have expanded empirically over time (e.g., Canada's removal of
 *   'reasonably foreseeable natural death' requirement, inclusion of mental
 *   illness as sole condition pending). The claimed type is tangled_rope:
 *   genuine coordination function for suffering patients, asymmetric
 *   extraction of risk onto the vulnerable, active enforcement via safeguards
 *   and oversight.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(end_of_life_authority__autonomy_reading, 0.42).
domain_priors:suppression_score(end_of_life_authority__autonomy_reading, 0.68).
domain_priors:theater_ratio(end_of_life_authority__autonomy_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(end_of_life_authority__autonomy_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(end_of_life_authority__autonomy_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(end_of_life_authority__autonomy_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(end_of_life_authority__autonomy_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(end_of_life_authority__autonomy_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(end_of_life_authority__autonomy_reading, tangled_rope).
narrative_ontology:human_readable(end_of_life_authority__autonomy_reading, "Autonomy-Based Assisted Dying Framework").
narrative_ontology:topic_domain(end_of_life_authority__autonomy_reading, "medical_ethics/bioethics/end_of_life_policy").

domain_priors:requires_active_enforcement(end_of_life_authority__autonomy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(end_of_life_authority__autonomy_reading, '19d39042-6e9e-4bbc-b59d-d159554558f7').
narrative_ontology:cs_kernel_codification('19d39042-6e9e-4bbc-b59d-d159554558f7', formalized).
narrative_ontology:cs_authority_grounding('19d39042-6e9e-4bbc-b59d-d159554558f7', lineage).
narrative_ontology:cs_interpretation_layer_present('19d39042-6e9e-4bbc-b59d-d159554558f7').
narrative_ontology:cs_reading_relation('19d39042-6e9e-4bbc-b59d-d159554558f7', end_of_life_authority__sanctity_reading, coexists_with).
narrative_ontology:cs_reading_relation('19d39042-6e9e-4bbc-b59d-d159554558f7', end_of_life_authority__slippery_slope_mechanism, influences).
narrative_ontology:cs_axiom('19d39042-6e9e-4bbc-b59d-d159554558f7', foundational, autonomy_grounds_right_to_die).
narrative_ontology:cs_axiom_status(autonomy_grounds_right_to_die, holdable).
narrative_ontology:cs_axiom_grounding('19d39042-6e9e-4bbc-b59d-d159554558f7', autonomy_grounds_right_to_die, deontological).
narrative_ontology:cs_axiom('19d39042-6e9e-4bbc-b59d-d159554558f7', secondary, suffering_justifies_exception_to_prohibition).
narrative_ontology:cs_axiom_status(suffering_justifies_exception_to_prohibition, holdable).
narrative_ontology:cs_axiom_grounding('19d39042-6e9e-4bbc-b59d-d159554558f7', suffering_justifies_exception_to_prohibition, instrumental).
narrative_ontology:cs_reference_frame('19d39042-6e9e-4bbc-b59d-d159554558f7', classical_liberal_autonomy).
narrative_ontology:cs_drift_state('19d39042-6e9e-4bbc-b59d-d159554558f7', contemporary_assisted_dying_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('19d39042-6e9e-4bbc-b59d-d159554558f7', '').
narrative_ontology:cs_kernel_id(end_of_life_authority__autonomy_reading, end_of_life_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(end_of_life_authority__autonomy_reading, suffering_patients).
narrative_ontology:constraint_beneficiary(end_of_life_authority__autonomy_reading, participating_physicians).
narrative_ontology:constraint_beneficiary(end_of_life_authority__autonomy_reading, palliative_care_providers).
narrative_ontology:constraint_victim(end_of_life_authority__autonomy_reading, vulnerable_populations).
narrative_ontology:constraint_vindicates(end_of_life_authority__autonomy_reading, bodily_autonomy_principle).
narrative_ontology:constraint_vindicates(end_of_life_authority__autonomy_reading, relief_of_unbearable_suffering).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Patients with terminal or grievous irremediable conditions who experience unbearable suffering. They gain a legally protected option to request assisted dying after meeting eligibility criteria. Their exit from suffering is now mediated by a regulated process rather than being denied or driven underground. The constraint structures their end-of-life choices; they cannot exit the constraint without forgoing the legal protection.
narrative_ontology:constraint_stakeholder(end_of_life_authority__autonomy_reading, suffering_patients, beneficiary,
    moderate, biographical, identity_locked, national).

% Physicians who assess eligibility, prescribe, and administer assisted dying. They operate under a legal framework that shields them from prosecution if they follow procedural safeguards. They benefit from professional clarity and legal protection, but their participation is constrained by conscience clauses, institutional policies, and the requirement to follow strict protocols. They cannot easily exit the role without leaving the clinical setting where such requests arise.
narrative_ontology:constraint_stakeholder(end_of_life_authority__autonomy_reading, participating_physicians, agenda_setter,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(end_of_life_authority__autonomy_reading, participating_physicians, beneficiary).

% The legislative body that enacts and amends the assisted dying law. It sets eligibility criteria, procedural safeguards, and reporting requirements. It bears political costs from both advocacy and opposition groups. Its exit from the constraint is electoral or constitutional; it can repeal or amend the law but faces high political stakes.
narrative_ontology:constraint_stakeholder(end_of_life_authority__autonomy_reading, legislature, agenda_setter,
    institutional, generational, analytical, national).

% Elderly, disabled, economically marginalized, or socially isolated individuals who may face implicit or explicit pressure to choose assisted dying as a cost-saving or burden-relieving measure. They bear the risk of coercion and the erosion of palliative care alternatives. They cannot easily exit the structural conditions that make them vulnerable; the constraint's expansion of eligibility over time increases their exposure.
narrative_ontology:constraint_stakeholder(end_of_life_authority__autonomy_reading, vulnerable_populations, payer,
    powerless, biographical, trapped, national).

% Clinicians and services providing palliative and hospice care. They are integrated into the assisted dying pathway as a mandatory referral or consultation step. They benefit from increased recognition and funding for palliative care that often accompanies such laws, but they also face professional tension between palliative goals and assisted dying. They can exit by specializing away from end-of-life contexts.
narrative_ontology:constraint_stakeholder(end_of_life_authority__autonomy_reading, palliative_care_providers, beneficiary,
    organized, biographical, mobile, national).

% Scholars, ethics committees, and appellate courts that interpret the law, adjudicate boundary cases, and track empirical outcomes. They do not directly collect benefits or pay costs but shape the constraint's evolution through precedent, policy advice, and public discourse. Their exit is intellectual disengagement.
narrative_ontology:constraint_stakeholder(end_of_life_authority__autonomy_reading, bioethicists_and_courts, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a regulated, transparent pathway for individuals facing unbearable suffering to voluntarily end their lives with medical assistance, replacing clandestine or violent self-harm and resolving clinical uncertainty for physicians.
% TRANSFER_FUNCTION: Transfers decisional authority over the timing and manner of death from the state/prohibition to the individual patient, mediated by physicians and oversight bodies. Moves legal risk from physicians to the regulatory framework. Moves resource allocation toward assessment and oversight infrastructure.
% ABSENT_VOICES: Future patients whose eligibility may be expanded by judicial interpretation or legislative amendment (e.g., mature minors, psychiatric-only suffering, advance directives for dementia). They are not present in current debates but will bear the consequences of eligibility drift. Also absent: families of vulnerable persons who may experience moral injury from a relative's assisted death.
% DISAPPEARANCE_RATIONALE: If the autonomy-based framework vanished overnight, patients would lose legal access to assisted dying, physicians would face criminal liability for assistance, and the regulatory infrastructure (review boards, reporting systems) would dissolve. The world would revert to a prohibition regime with clandestine practice and unresolved suffering for some patients.
% FOUNDING_PROBLEM: The prohibition on assisted dying forced individuals with unbearable suffering to either endure prolonged agony, attempt dangerous self-harm, or seek unregulated help, while physicians who acted compassionately faced criminal prosecution.
% FOUNDING_PROBLEM_CORROBORATION: Parliamentary committees, supreme court rulings (e.g., Carter v. Canada, 2015), and patient advocacy groups attest the founding problem persists. Opponents (religious organizations, some medical associations, disability rights groups) argue the problem is addressed by improved palliative care and that the law creates new harms. Independent coroner reports and qualitative studies of patient requests corroborate the ongoing existence of the founding problem.
narrative_ontology:disappearance_verdict(end_of_life_authority__autonomy_reading, world_rearranges).
narrative_ontology:founding_problem_status(end_of_life_authority__autonomy_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(end_of_life_authority__autonomy_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(end_of_life_authority__autonomy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(end_of_life_authority__autonomy_reading, 0.42, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Extractiveness (0.42) reflects the transfer of decisional authority and the risk externalized to vulnerable populations. Suppression (0.68) is high because the constraint's persistence depends on active enforcement of safeguards (independent assessments, waiting periods, reporting) and on suppressing the alternative of unregulated practice. Theater ratio (0.22) is low-moderate: safeguards are functional but a growing share of oversight activity manages eligibility expansion rather than core coordination. Accessibility collapse (0.35) is moderate: alternatives (palliative care, refusal of treatment) remain legally available but are practically constrained by resource gaps. Resistance (0.55) is significant: ongoing legal challenges, professional refusal, and political opposition. Measurements show extractiveness rising and suppression slightly declining as the system matures, consistent with eligibility expansion.
 *
 * PERSPECTIVAL GAP:
 *   From the patient seat, the constraint is a rope (coordination that solves their problem). From the vulnerable population seat, it is a snare (extraction of safety). The engine computes this divergence from the structural data. The autonomy reading's proponents emphasize the coordination function; critics emphasize the extraction. The claimed_type tangled_rope reflects the author's structural judgment that both are real and irreducible.
 *
 * DIRECTIONALITY LOGIC:
 *   Suffering patients are beneficiaries (d low) — they gain legal access to a previously prohibited option. Participating physicians are agenda_setters with secondary beneficiary role (d near symmetric) — they gain legal protection but bear procedural burden. Legislature is agenda_setter (d analytical). Vulnerable populations are payers (d high) — they bear coercion risk without consenting to the constraint. Palliative care providers are beneficiaries (d low-moderate). Bioethicists/courts are observers (d=0.5). Exit options differentiate: patients are identity_locked (their self-concept fuses with the right to choose), physicians constrained (professional identity), vulnerable populations trapped (structural vulnerability).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (prohibition forcing suffering underground) remains contested: palliative care has improved but gaps persist. The constraint has not become a piton; it is actively maintained and expanded. Mandatrophy is not resolved because the coordination function is still live for the core beneficiaries, even as extraction grows.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    coercion_mechanism_ambiguity,
    'Is the measured extraction from vulnerable populations structural (economic/policy pressure) or internalized (psychological sense of burden)?',
    'Longitudinal qualitative studies of patients'' decision-making contexts; comparison of assisted dying rates across socioeconomic strata controlling for illness severity.',
    'If internalized, the constraint''s effective suppression is higher than structural measures suggest — the target carries the pressure with them. If structural, policy interventions (income support, palliative care funding) could reduce extraction without repealing the constraint.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coercion_mechanism_ambiguity, empirical, 'Whether the asymmetric extraction on vulnerable populations operates through external structural pressure or internalized sense of obligation.').

omega_variable(
    kernel_reading_boundary,
    'Does the autonomy reading''s core premise (autonomy grounds a right to assisted dying) logically foreclose the sanctity reading, or do they coexist as competing frameworks?',
    'Jurisprudential analysis of whether a legal system can simultaneously recognize a right to assisted dying and uphold a general prohibition on killing (e.g., via narrow exceptions).',
    'If forecloses, the two readings cannot coexist in a single legal framework — adoption of one requires rejection of the other. If coexists_with, both can be institutionalized in different jurisdictions or at different times without logical contradiction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_boundary, conceptual, 'Logical relationship between autonomy and sanctity readings of the end-of-life authority kernel.').

omega_variable(
    eligibility_expansion_driver,
    'Is the empirical expansion of eligibility criteria driven by the internal logic of the autonomy principle (if autonomy grounds the right, why limit it?) or by external political advocacy?',
    'Comparative analysis of legislative debates, court rulings, and advocacy funding across jurisdictions that have expanded eligibility.',
    'If driven by internal logic, expansion is structurally inevitable once the autonomy premise is accepted (stronger influence on slippery_slope_mechanism). If driven by external advocacy, expansion is contingent and potentially arrestable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(eligibility_expansion_driver, empirical, 'Causal driver of the observed eligibility expansion pattern.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(end_of_life_authority__autonomy_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(eol_autonomy_tr_t0, end_of_life_authority__autonomy_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(eol_autonomy_tr_t4, end_of_life_authority__autonomy_reading, theater_ratio, 4, 0.12).
narrative_ontology:measurement(eol_autonomy_tr_t8, end_of_life_authority__autonomy_reading, theater_ratio, 8, 0.15).
narrative_ontology:measurement(eol_autonomy_tr_t12, end_of_life_authority__autonomy_reading, theater_ratio, 12, 0.18).
narrative_ontology:measurement(eol_autonomy_tr_t16, end_of_life_authority__autonomy_reading, theater_ratio, 16, 0.2).
narrative_ontology:measurement(eol_autonomy_tr_t20, end_of_life_authority__autonomy_reading, theater_ratio, 20, 0.22).

% Extraction over time
narrative_ontology:measurement(eol_autonomy_be_t0, end_of_life_authority__autonomy_reading, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(eol_autonomy_be_t4, end_of_life_authority__autonomy_reading, base_extractiveness, 4, 0.3).
narrative_ontology:measurement(eol_autonomy_be_t8, end_of_life_authority__autonomy_reading, base_extractiveness, 8, 0.35).
narrative_ontology:measurement(eol_autonomy_be_t12, end_of_life_authority__autonomy_reading, base_extractiveness, 12, 0.38).
narrative_ontology:measurement(eol_autonomy_be_t16, end_of_life_authority__autonomy_reading, base_extractiveness, 16, 0.4).
narrative_ontology:measurement(eol_autonomy_be_t20, end_of_life_authority__autonomy_reading, base_extractiveness, 20, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(eol_autonomy_su_t0, end_of_life_authority__autonomy_reading, suppression_requirement, 0, 0.75).
narrative_ontology:measurement(eol_autonomy_su_t4, end_of_life_authority__autonomy_reading, suppression_requirement, 4, 0.72).
narrative_ontology:measurement(eol_autonomy_su_t8, end_of_life_authority__autonomy_reading, suppression_requirement, 8, 0.7).
narrative_ontology:measurement(eol_autonomy_su_t12, end_of_life_authority__autonomy_reading, suppression_requirement, 12, 0.68).
narrative_ontology:measurement(eol_autonomy_su_t16, end_of_life_authority__autonomy_reading, suppression_requirement, 16, 0.67).
narrative_ontology:measurement(eol_autonomy_su_t20, end_of_life_authority__autonomy_reading, suppression_requirement, 20, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(end_of_life_authority__autonomy_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(end_of_life_authority__autonomy_reading, 0.08).
narrative_ontology:affects_constraint(end_of_life_authority__autonomy_reading, end_of_life_authority__sanctity_reading).
narrative_ontology:affects_constraint(end_of_life_authority__autonomy_reading, end_of_life_authority__slippery_slope_mechanism).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the end_of_life_authority kernel. The autonomy reading grounds authority in individual self-determination; the sanctity reading grounds it in intrinsic value of life; the slippery_slope_mechanism reading models the empirical dynamics of eligibility expansion. They form a constraint family linked by affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(end_of_life_authority__autonomy_reading, moderate, 0.2).
constraint_indexing:directionality_override(end_of_life_authority__autonomy_reading, powerless, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
