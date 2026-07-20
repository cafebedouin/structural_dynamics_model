% ============================================================================
% CONSTRAINT STORY: sex_gender_category__hybrid_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sex_gender_category__hybrid_reading, []).

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
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
 *   constraint_id: sex_gender_category__hybrid_reading
 *   human_readable: Medical Gatekeeping Model for Gender Category Membership
 *   domain: social/identity/legal
 *
 * SUMMARY:
 *   This constraint instantiates the hybrid_reading of the
 *   sex_gender_category kernel: legal and social sex/gender category
 *   membership is determined by a combination of assigned biological sex at
 *   birth and a medically supervised social transition. It sits between
 *   biology_reading (immutable anatomy/chromosomes) and identity_reading
 *   (subjective self-identification alone). The model concentrates authority
 *   in psychiatric and endocrine institutions, conditionally includes trans
 *   women who complete transition, and excludes non-transitioning trans
 *   individuals entirely. It has a genuine coordination
 *   functionâstandardizing a path to legal recognitionâbut operates with
 *   high gatekeeping costs and asymmetric extraction.
 *
 * KEY AGENTS:
 *   - Medical gatekeeping institutions (institutional/agenda_setter): Set diagnostic criteria, administer transition protocols, and certify individuals for legal recognition. They are the primary beneficiaries of the constraint's authority structure.
 *   - Conditionally recognized trans women (moderate/beneficiary+payer): Gain legal status and conditional inclusion but bear the direct financial, physical, and bureaucratic costs of medical gatekeeping.
 *   - Non-transitioning trans individuals (powerless/excluded): Are categorically excluded from recognition and bear the costs of documentation mismatch and legal vulnerability. They are the primary victims of the constraint's boundary.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sex_gender_category__hybrid_reading, 0.72).
domain_priors:suppression_score(sex_gender_category__hybrid_reading, 0.78).
domain_priors:theater_ratio(sex_gender_category__hybrid_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sex_gender_category__hybrid_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(sex_gender_category__hybrid_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(sex_gender_category__hybrid_reading, theater_ratio, 0.55).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(sex_gender_category__hybrid_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(sex_gender_category__hybrid_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sex_gender_category__hybrid_reading, tangled_rope).
narrative_ontology:human_readable(sex_gender_category__hybrid_reading, "Medical Gatekeeping Model for Gender Category Membership").
narrative_ontology:topic_domain(sex_gender_category__hybrid_reading, "social/identity/legal").

domain_priors:requires_active_enforcement(sex_gender_category__hybrid_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(sex_gender_category__hybrid_reading, 'c2dc1937-78aa-4996-a1ed-a53108b4c4d9').
narrative_ontology:cs_kernel_codification('c2dc1937-78aa-4996-a1ed-a53108b4c4d9', formalized).
narrative_ontology:cs_authority_grounding('c2dc1937-78aa-4996-a1ed-a53108b4c4d9', expertise).
narrative_ontology:cs_interpretation_layer_present('c2dc1937-78aa-4996-a1ed-a53108b4c4d9').
narrative_ontology:cs_reading_relation('c2dc1937-78aa-4996-a1ed-a53108b4c4d9', sex_gender_category__biology_reading, coexists_with).
narrative_ontology:cs_reading_relation('c2dc1937-78aa-4996-a1ed-a53108b4c4d9', sex_gender_category__identity_reading, coexists_with).
narrative_ontology:cs_axiom('c2dc1937-78aa-4996-a1ed-a53108b4c4d9', foundational, medical_gatekeeping_legitimate).
narrative_ontology:cs_axiom_status(medical_gatekeeping_legitimate, holdable).
narrative_ontology:cs_axiom_grounding('c2dc1937-78aa-4996-a1ed-a53108b4c4d9', medical_gatekeeping_legitimate, conventional).
narrative_ontology:cs_axiom('c2dc1937-78aa-4996-a1ed-a53108b4c4d9', foundational, biological_sex_medically_malleable).
narrative_ontology:cs_axiom_status(biological_sex_medically_malleable, holdable).
narrative_ontology:cs_axiom_grounding('c2dc1937-78aa-4996-a1ed-a53108b4c4d9', biological_sex_medically_malleable, empirically_contingent).
narrative_ontology:cs_reference_frame('c2dc1937-78aa-4996-a1ed-a53108b4c4d9', medical_gatekeeping_framework).
narrative_ontology:cs_drift_state('c2dc1937-78aa-4996-a1ed-a53108b4c4d9', post_self_id_advocacy_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('c2dc1937-78aa-4996-a1ed-a53108b4c4d9', '').
narrative_ontology:cs_kernel_id(sex_gender_category__hybrid_reading, sex_gender_category).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sex_gender_category__hybrid_reading, medical_gatekeeping_institutions).
narrative_ontology:constraint_beneficiary(sex_gender_category__hybrid_reading, conditionally_recognized_trans_women).
narrative_ontology:constraint_victim(sex_gender_category__hybrid_reading, non_transitioning_trans_individuals).
narrative_ontology:constraint_victim(sex_gender_category__hybrid_reading, conditionally_recognized_trans_women).
narrative_ontology:constraint_vindicates(sex_gender_category__hybrid_reading, medical_model_of_transsexuality).
narrative_ontology:constraint_vindicates(sex_gender_category__hybrid_reading, biological_sex_malleable_via_intervention).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Psychiatric, endocrine, and surgical bodies that set diagnostic criteria (e.g., gender dysphoria classifications), assess readiness for transition, and certify individuals for legal gender recognition. Their professional authority, training pipelines, and revenue streams depend on maintaining the gatekeeping function as a necessary prerequisite for category change.
narrative_ontology:constraint_stakeholder(sex_gender_category__hybrid_reading, medical_gatekeeping_institutions, agenda_setter,
    institutional, generational, constrained, national).

% Receive legal gender recognition and conditional social inclusion after completing medically supervised transition, including psychiatric evaluation, hormone therapy, and often surgery. Bear substantial direct costsâfinancial, physical, and psychologicalâfor the gatekeeping process. Cannot access recognition without institutional sign-off, and face re-evaluation or revocation risk if they deviate from expected transition pathways.
narrative_ontology:constraint_stakeholder(sex_gender_category__hybrid_reading, conditionally_recognized_trans_women, beneficiary,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(sex_gender_category__hybrid_reading, conditionally_recognized_trans_women, payer).

% Trans and gender-diverse individuals who do not pursue, cannot access, or do not desire medical transition. They are categorically excluded from legal gender recognition under this model, leaving them with documentation mismatches, reduced legal protections, and no administrative pathway to category membership regardless of gender identity or social presentation.
narrative_ontology:constraint_stakeholder(sex_gender_category__hybrid_reading, non_transitioning_trans_individuals, excluded,
    powerless, biographical, trapped, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(sex_gender_category__hybrid_reading, medical_gatekeeping_institutions).
narrative_ontology:fixing_cost_class(sex_gender_category__hybrid_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a standardized, medically supervised pathway for legal sex/gender category reassignment, creating administrative clarity for states, healthcare systems, and civil registries about who qualifies for binary category membership.
% TRANSFER_FUNCTION: Moves authority over gender category assignment from civil registry or self-identification to medical-psychiatric institutions; moves the financial, bodily, and psychological costs of transition to individuals seeking recognition; withholds legal status and protections from those who do not or cannot meet medical criteria.
% ABSENT_VOICES: Non-transitioning trans individuals are structurally excluded from the recognition framework despite being directly affected by its categorical boundaries. Self-identification advocates and intersex people whose bodies do not fit binary medical criteria are also absent from the gatekeeping conversation, though the system implicitly adjudicates their status.
% DISAPPEARANCE_RATIONALE: If this constraint vanished, legal sex/gender categories would revert to either biology-only or self-identification models, medical institutions would lose their adjudicatory role over civil status, and the class of legally recognized trans women would expand or contract dramatically depending on which replacement framework filled the vacuum.
% FOUNDING_PROBLEM: Legal and administrative systems needed a criterion for sex/gender category membership that could accommodate transsexual individuals while preserving a perceived link to biological sex, preventing what gatekeepers framed as fraudulent or frivolous claims, and managing scarce medical resources.
% FOUNDING_PROBLEM_CORROBORATION: Medical institutions and some second-wave feminist organizations attest the problem is still live, citing regret prevention and social order. Trans rights organizations, human rights bodies (e.g., Yogyakarta Principles Plus 10), and a growing body of public-health research attest the founding problem was based on pathologization and stigma rather than genuine administrative necessity; no independent corroboration from outside the benefiting parties exists without contestation.
narrative_ontology:disappearance_verdict(sex_gender_category__hybrid_reading, world_rearranges).
narrative_ontology:founding_problem_status(sex_gender_category__hybrid_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(sex_gender_category__hybrid_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(sex_gender_category__hybrid_reading, 'none', 1).
narrative_ontology:epsilon_provenance(sex_gender_category__hybrid_reading, 0.72, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sex_gender_category__hybrid_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(sex_gender_category__hybrid_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(sex_gender_category__hybrid_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness is high (0.72) because the constraint imposes costly medical interventions, psychiatric evaluations, and bureaucratic compliance as non-negotiable conditions for category membership. Suppression is higher (0.78) because the model's persistence depends on active enforcement by medical boards and legal statutes that block self-identification alternatives. Theater ratio is substantial (0.55) and rising: as the self-ID movement challenges medical authority, an increasing share of gatekeeping activity is performative maintenance of professional legitimacy rather than patient welfare. Accessibility collapse (0.62) reflects that while self-ID alternatives exist in some jurisdictions, they are actively blocked where this constraint holds. Resistance (0.72) is high due to sustained activism against pathologization and medical gatekeeping.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat (medical institutions) experiences the constraint as necessary coordinationâprotecting patients from regret, preserving social order, and standardizing care. The conditionally recognized seat experiences it as a mixed structure: genuine legal benefit fused with high extraction. The excluded seat experiences it as pure extractionâdenial of self-determination and legal personhood. The engine computes these divergent classifications from the same structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Medical institutions derive low directionality as concentrated beneficiaries of authority and revenue. Conditionally recognized trans women sit at mixed directionality because they are declared in both beneficiaries (legal recognition) and victims (gatekeeping costs), producing a net ambiguous structural position. Non-transitioning trans individuals derive high directionality as excluded victims with trapped exit options and no compensating benefit.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint was built to solve a genuine administrative problemâhow to process transsexual people through binary legal systemsâbut its mandate has been partially overtaken by evidence that self-identification models function without medical gatekeeping. It persists partly because medical institutions benefit from the authority, and because the conditionally recognized group is identity-locked to the only pathway that grants them legal status. The classification as tangled_rope prevents mislabeling it as pure coordination (it has identifiable victims and requires enforcement) or pure snare (it does produce real legal recognition for some).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'How does the structural classification change if the same kernel is read through biology_reading or identity_reading rather than hybrid_reading?',
    'Comparative analysis of sibling constraint stories in the sex_gender_category family; divergence in epsilon, beneficiary/victim structure, and directionality profiles reveals the kernel''s decomposition.',
    'If biology_reading dominates, the victim set expands to all trans individuals and extraction shifts toward total exclusion; if identity_reading dominates, medical authority collapses and the beneficiary set expands to all self-identified trans people. The disagreement is located at whether medical intervention and institutional assessment are necessary conditions for category membership.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'This constraint is one reading of a contested kernel; sibling readings would alter victim and beneficiary sets structurally.').

omega_variable(
    gatekeeping_cost_benefit_distribution,
    'Do the benefits of legal recognition outweigh the medical, financial, and psychological costs of gatekeeping for the conditionally recognized group?',
    'Comparative longitudinal study of wellbeing, legal security, and life outcomes for trans cohorts under hybrid gatekeeping models versus self-identification models.',
    'If net welfare is negative for the conditionally recognized group, their classification as beneficiaries is overstated and the constraint tilts toward snare; if positive, the coordination function is partially vindicated despite asymmetric extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(gatekeeping_cost_benefit_distribution, empirical, 'Whether conditional recognition produces net benefit or net harm for those who pass through gatekeeping.').

omega_variable(
    non_transitioning_exclusion_necessity,
    'Is the exclusion of non-transitioning trans individuals a necessary side effect of a functional category system, or an arbitrary restriction serving medical authority?',
    'Analysis of jurisdictions that have moved to self-identification models to determine whether legal and administrative function degraded or remained stable after removing medical gatekeeping.',
    'If category function persists without medical gatekeeping, the exclusion is arbitrary and the constraint''s extraction is higher than its coordination justification; if function collapses, part of the extraction may be inherent coordination cost.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(non_transitioning_exclusion_necessity, empirical, 'Whether exclusion of non-transitioning individuals is structurally necessary or institutionally self-serving.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sex_gender_category__hybrid_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sex__tr_t0, sex_gender_category__hybrid_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(sex__tr_t8, sex_gender_category__hybrid_reading, theater_ratio, 8, 0.32).
narrative_ontology:measurement(sex__tr_t16, sex_gender_category__hybrid_reading, theater_ratio, 16, 0.4).
narrative_ontology:measurement(sex__tr_t24, sex_gender_category__hybrid_reading, theater_ratio, 24, 0.46).
narrative_ontology:measurement(sex__tr_t32, sex_gender_category__hybrid_reading, theater_ratio, 32, 0.51).
narrative_ontology:measurement(sex__tr_t40, sex_gender_category__hybrid_reading, theater_ratio, 40, 0.55).

% Extraction over time
narrative_ontology:measurement(sex__be_t0, sex_gender_category__hybrid_reading, base_extractiveness, 0, 0.52).
narrative_ontology:measurement(sex__be_t8, sex_gender_category__hybrid_reading, base_extractiveness, 8, 0.61).
narrative_ontology:measurement(sex__be_t16, sex_gender_category__hybrid_reading, base_extractiveness, 16, 0.7).
narrative_ontology:measurement(sex__be_t24, sex_gender_category__hybrid_reading, base_extractiveness, 24, 0.74).
narrative_ontology:measurement(sex__be_t32, sex_gender_category__hybrid_reading, base_extractiveness, 32, 0.73).
narrative_ontology:measurement(sex__be_t40, sex_gender_category__hybrid_reading, base_extractiveness, 40, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(sex__su_t0, sex_gender_category__hybrid_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(sex__su_t8, sex_gender_category__hybrid_reading, suppression_requirement, 8, 0.55).
narrative_ontology:measurement(sex__su_t16, sex_gender_category__hybrid_reading, suppression_requirement, 16, 0.65).
narrative_ontology:measurement(sex__su_t24, sex_gender_category__hybrid_reading, suppression_requirement, 24, 0.72).
narrative_ontology:measurement(sex__su_t32, sex_gender_category__hybrid_reading, suppression_requirement, 32, 0.76).
narrative_ontology:measurement(sex__su_t40, sex_gender_category__hybrid_reading, suppression_requirement, 40, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sex_gender_category__hybrid_reading, identity_coordination).
narrative_ontology:affects_constraint(sex_gender_category__hybrid_reading, biology_reading).
narrative_ontology:affects_constraint(sex_gender_category__hybrid_reading, identity_reading).

% DUAL FORMULATION NOTE:
% The sex_gender_category kernel decomposes into three structurally distinct constraints: biology_reading (membership by immutable reproductive biology), hybrid_reading (membership by biology plus medical transition), and identity_reading (membership by subjective self-identification). Each reading has a different epsilon, beneficiary/victim structure, and empirical status. They form a constraint family linked by network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
