% ============================================================================
% CONSTRAINT STORY: woman_female_category__sex_biology_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_woman_female_category__sex_biology_reading, []).

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
 *   constraint_id: woman_female_category__sex_biology_reading
 *   human_readable: Sex-Based Category Membership for Female Protections
 *   domain: political_philosophy/bioethics/gender_studies/law
 *
 * SUMMARY:
 *   This constraint story instantiates the sex-biology reading of the
 *   contested kernel 'woman_female_category'. The reading defines 'woman' and
 *   'female' by chromosomal sex (XX), reproductive anatomy, and gamete
 *   production capacity. It operates as the default legal criterion in many
 *   jurisdictions for access to female-only spaces (prisons, shelters,
 *   changing rooms, sports). The constraint coordinates protection for natal
 *   females but extracts heavily from trans women, particularly in
 *   high-stakes institutional settings (prisons, shelters). The claimed type
 *   is tangled_rope: genuine coordination function (stable criterion for
 *   sex-segregated provisions) AND asymmetric extraction (trans women bear
 *   disproportionate safety and dignity costs). Active enforcement is
 *   required: legal and administrative machinery maintains the exclusion. The
 *   kernel context: this is one of three readings (sex_biology_reading,
 *   gender_identity_reading, hybrid_contextual_reading) of the same kernel;
 *   they are structurally distinct constraints with different ε, different
 *   victim sets, and different claimed types.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(woman_female_category__sex_biology_reading, 0.28).
domain_priors:suppression_score(woman_female_category__sex_biology_reading, 0.62).
domain_priors:theater_ratio(woman_female_category__sex_biology_reading, 0.18).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(woman_female_category__sex_biology_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(woman_female_category__sex_biology_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(woman_female_category__sex_biology_reading, theater_ratio, 0.18).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(woman_female_category__sex_biology_reading, accessibility_collapse, 0.78).
narrative_ontology:constraint_metric(woman_female_category__sex_biology_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(woman_female_category__sex_biology_reading, tangled_rope).
narrative_ontology:human_readable(woman_female_category__sex_biology_reading, "Sex-Based Category Membership for Female Protections").
narrative_ontology:topic_domain(woman_female_category__sex_biology_reading, "political_philosophy/bioethics/gender_studies/law").

domain_priors:requires_active_enforcement(woman_female_category__sex_biology_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(woman_female_category__sex_biology_reading, 'edfe63bd-8d35-4b54-8ba1-f97c7500e76d').
narrative_ontology:cs_kernel_codification('edfe63bd-8d35-4b54-8ba1-f97c7500e76d', formalized).
narrative_ontology:cs_authority_grounding('edfe63bd-8d35-4b54-8ba1-f97c7500e76d', lineage).
narrative_ontology:cs_interpretation_layer_present('edfe63bd-8d35-4b54-8ba1-f97c7500e76d').
narrative_ontology:cs_reading_relation('edfe63bd-8d35-4b54-8ba1-f97c7500e76d', woman_female_category__gender_identity_reading, forecloses).
narrative_ontology:cs_reading_relation('edfe63bd-8d35-4b54-8ba1-f97c7500e76d', woman_female_category__hybrid_contextual_reading, coexists_with).
narrative_ontology:cs_axiom('edfe63bd-8d35-4b54-8ba1-f97c7500e76d', foundational, biological_sex_is_immutable_and_binominal).
narrative_ontology:cs_axiom_status(biological_sex_is_immutable_and_binominal, holdable).
narrative_ontology:cs_axiom_grounding('edfe63bd-8d35-4b54-8ba1-f97c7500e76d', biological_sex_is_immutable_and_binominal, empirically_contingent).
narrative_ontology:cs_axiom('edfe63bd-8d35-4b54-8ba1-f97c7500e76d', secondary, sex_based_rights_are_distinct_from_gender_identity_claims).
narrative_ontology:cs_axiom_status(sex_based_rights_are_distinct_from_gender_identity_claims, holdable).
narrative_ontology:cs_axiom_grounding('edfe63bd-8d35-4b54-8ba1-f97c7500e76d', sex_based_rights_are_distinct_from_gender_identity_claims, deontological).
narrative_ontology:cs_reference_frame('edfe63bd-8d35-4b54-8ba1-f97c7500e76d', classical_sex_based_legal_protections).
narrative_ontology:cs_drift_state('edfe63bd-8d35-4b54-8ba1-f97c7500e76d', contemporary_gender_identity_contestation, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('edfe63bd-8d35-4b54-8ba1-f97c7500e76d', '').
narrative_ontology:cs_kernel_id(woman_female_category__sex_biology_reading, woman_female_category).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(woman_female_category__sex_biology_reading, natal_females_seeking_sex_based_protections).
narrative_ontology:constraint_victim(woman_female_category__sex_biology_reading, trans_women_excluded_from_female_spaces).
narrative_ontology:constraint_victim(woman_female_category__sex_biology_reading, trans_women_in_prisons_shelters).
narrative_ontology:constraint_vindicates(woman_female_category__sex_biology_reading, biological_sex_is_immutable_and_binominal).
narrative_ontology:constraint_vindicates(woman_female_category__sex_biology_reading, sex_based_rights_are_distinct_from_gender_identity_claims).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Rely on sex-segregated spaces (prisons, shelters, changing rooms, sports) for physical safety and fair competition. Experience the constraint as a coordination mechanism that preserves access to these spaces against pressures to include trans women. View the biology-based definition as necessary to maintain the protective function of female-only provisions. Exit is constrained: legal reform could remove these protections, and alternative protective frameworks are not established.
narrative_ontology:constraint_stakeholder(woman_female_category__sex_biology_reading, natal_females_seeking_sex_based_protections, beneficiary,
    moderate, biographical, constrained, national).

% Are categorically excluded from female-designated spaces (prisons, shelters, restrooms, sports, DV refuges) under this reading. Experience the constraint as extraction of dignity, safety, and access to services appropriate to their lived gender. Exit is identity-locked: their gender identity is constitutive of self-understanding; the constraint demands they accept a category assignment that contradicts their identity. No alternative pathway to female spaces exists under this reading.
narrative_ontology:constraint_stakeholder(woman_female_category__sex_biology_reading, trans_women_excluded_from_female_spaces, payer,
    powerless, biographical, identity_locked, national).

% Face acute physical safety risks when housed in male facilities due to the biology-based rule. The constraint's operation here is directly extractive of bodily safety — assault rates for trans women in male prisons are markedly higher. Exit is trapped: institutional assignment is coercive and inescapable; legal challenges are slow and uncertain. This stakeholder bears the highest ε of the constraint's operation.
narrative_ontology:constraint_stakeholder(woman_female_category__sex_biology_reading, trans_women_in_prisons_shelters, payer,
    powerless, immediate, trapped, national).

% Set and enforce the legal definition of 'woman' and 'female' in statute, regulation, and case law. Their choices determine which reading governs access to sex-segregated provisions. They hold the power to switch the operative reading but face political costs from all sides. Exit is arbitrage-grade: they can move between readings by legislative act or judicial interpretation.
narrative_ontology:constraint_stakeholder(woman_female_category__sex_biology_reading, policy_makers_legislators, agenda_setter,
    institutional, generational, arbitrage, national).

% Advocate for gender-identity-based category membership. Are structurally excluded from the constraint's operation under this reading — their preferred framework is not recognized. Would object to the exclusion of trans women from female spaces and the safety harms documented in prisons/shelters. Exit is mobile: they can campaign, litigate, and shift public opinion to change the operative reading.
narrative_ontology:constraint_stakeholder(woman_female_category__sex_biology_reading, gender_identity_advocates, excluded,
    organized, biographical, mobile, national).

% Analyze the competing readings' coherence, empirical foundations, and normative implications. Track how the constraint's enforcement affects different populations. Neither collect nor pay under the constraint; their seat is analytical. Exit is analytical: they can adopt any reading in their work without personal cost.
narrative_ontology:constraint_stakeholder(woman_female_category__sex_biology_reading, bioethics_legal_scholars, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a stable, observable criterion for allocating sex-segregated protections and provisions (prisons, shelters, sports, intimate spaces) that is grounded in reproductive biology rather than self-declaration. Solves the coordination problem of who qualifies for female-only spaces without requiring subjective verification.
% TRANSFER_FUNCTION: Moves access to female-designated spaces and legal protections from trans women to natal females. The constraint operates by denying trans women entry to spaces where natal females' safety and fair competition are the stated rationale. The transfer is categorical: trans women lose access; natal females retain guaranteed exclusion of male-bodied persons.
% ABSENT_VOICES: Trans women directly affected by exclusion — particularly those in prisons and shelters — are often absent from legislative hearings and policy consultations where the biology-based reading is codified. Their testimony about safety harms is frequently excluded or marginalized in the debates that set the constraint. Intersex persons with variations in sex development are also absent; the binary XX/XY framework does not account for their existence, yet they are subject to its categorization.
% DISAPPEARANCE_RATIONALE: If the biology-based constraint vanished overnight, jurisdictions would need an immediate replacement rule for sex-segregated provisions. The hybrid reading would likely become operative by default in many domains (biological criteria for sports/prisons, identity for social recognition), but the transition would be contested and uneven. Female-only spaces would lose their current legal basis; trans women would gain access to some spaces but face new uncertainty about which criteria apply where.
% FOUNDING_PROBLEM: The need for stable, administrable criteria to implement sex-segregated legal protections (prisons, shelters, sports, single-sex services) that were originally designed to address male violence against females and ensure fair female competition in sports.
% FOUNDING_PROBLEM_CORROBORATION: Feminist legal scholars (e.g., Catharine MacKinnon, Kathleen Stock) attest the founding problem remains live: male violence against females and competitive fairness in sports persist, and biology-based criteria are necessary to address them. Trans rights advocates and human rights bodies (e.g., UN Special Rapporteur on Violence Against Women, European Court of Human Rights in recent jurisprudence) attest the founding problem is substantially addressed by other means and the biology-based constraint now functions as exclusion rather than protection. Medical bodies (WPATH, APA) attest the binary sex model does not reflect biological complexity (intersex variations).
narrative_ontology:disappearance_verdict(woman_female_category__sex_biology_reading, world_rearranges).
narrative_ontology:founding_problem_status(woman_female_category__sex_biology_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(woman_female_category__sex_biology_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_nemotron+rescue1', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(woman_female_category__sex_biology_reading, 'none', 1).
narrative_ontology:epsilon_provenance(woman_female_category__sex_biology_reading, 0.28, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(woman_female_category__sex_biology_reading_tests).
:- end_tests(woman_female_category__sex_biology_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.28) is moderate but rising: the constraint's operation transfers safety and access from trans women to natal females, with the highest extraction on the most vulnerable trans women (prisons, shelters). Suppression (0.62) is high: the constraint requires active legal enforcement to maintain categorical exclusion, and alternatives (gender-identity-based or hybrid rules) are suppressed through legislation and litigation. Theater ratio (0.18) is low-moderate: the protective function for natal females is real, but a growing share of enforcement activity serves to police the boundary rather than deliver protections. Accessibility collapse (0.78) is high: once the binary biological criterion is accepted, alternative categorizations appear incoherent to proponents. Resistance (0.55) is significant: trans rights advocates, human rights bodies, and medical associations actively contest the reading. The measurement series runs on a shared time grid (2015–2025) with 6 points per metric, showing rising extraction and suppression as the constraint becomes a contested political fault line.
 *
 * PERSPECTIVAL GAP:
 *   The beneficiary seat (natal females) experiences the constraint as a genuine coordination mechanism — a necessary, stable rule that solves the problem of who qualifies for female-only protections. The payer seats (trans women, especially in prisons/shelters) experience it as enforced extraction that denies their identity and endangers their bodies. The agenda-setter seat (policy-makers) sees a political choice between competing rights claims with no stable equilibrium. The engine computes these per-seat classifications from the structural data; the divergence between seats is the measurement.
 *
 * DIRECTIONALITY LOGIC:
 *   Natal females seeking sex-based protections are beneficiaries (d near 0.0): the constraint subsidizes their safety and fair competition by excluding male-bodied persons. Trans women excluded from female spaces are payers with identity_locked exit (d near 1.0): their gender identity is constitutive, making exit from the constraint's categorization impossible without self-betrayal. Trans women in prisons/shelters are payers with trapped exit (d = 1.0): institutional assignment is coercive and inescapable; the constraint directly extracts bodily safety. Policy-makers are agenda_setters with arbitrage exit (d ~ 0.5): they control the operative reading but face political costs from all sides. Gender-identity advocates are excluded (d undefined): their preferred framework is not recognized. Bioethics scholars are observers (d = 0.5): analytical seat.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (male violence against females, fair female competition) remains live but is contested as to whether this constraint still addresses it. Natal females argue the biology-based rule is necessary; trans advocates argue it has become exclusion for its own sake. The mandate has not atrophied — the protective function is still claimed and partially real — but the extraction on trans women has grown as the constraint became a front in culture war politics. This is not a piton: the constraint is actively maintained and its coordination function is genuinely invoked, not merely performed.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    committer_frame_reading,
    'This constraint is one reading (sex_biology_reading) of the contested kernel ''woman_female_category''. What would the sibling readings (gender_identity_reading, hybrid_contextual_reading) change structurally?',
    'Author the sibling readings as separate constraint stories with their own ε, beneficiary/victim structures, and claimed types. Compare the three stories'' computed classifications.',
    'If all three readings compute as different constraint types, the kernel is genuinely polysemous — not one constraint with measurement ambiguity. If two compute identically, the distinction may be rhetorical rather than structural.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(committer_frame_reading, conceptual, 'Kernel/reading decomposition: sex_biology_reading vs. gender_identity_reading vs. hybrid_contextual_reading').

omega_variable(
    intersex_exclusion_ambiguity,
    'Does the binary XX/XY criterion structurally exclude intersex persons with variations in sex development (DSDs), and if so, is this exclusion a feature or a bug of the reading?',
    'Examine whether the reading''s proponents address intersex persons explicitly (e.g., ''female = XX and ovaries'' vs. ''female = developmental pathway toward large gamete production''). Survey legal applications: are intersex women categorised as female or excluded?',
    'If intersex women are excluded, the victim set expands and the reading''s empirical grounding (empirically_contingent axiom) is falsified. If they are included via ''developmental pathway'' language, the criterion becomes less binary and more continuous, potentially lowering suppression.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(intersex_exclusion_ambiguity, conceptual, 'Whether the binary sex model excludes intersex persons and the structural consequence').

omega_variable(
    protective_function_empirical_basis,
    'Is the constraint''s coordination function (protecting natal females in prisons/shelters/sports) empirically sustained, or has it become a cover story for exclusion?',
    'Compare outcomes in jurisdictions with biology-based rules vs. gender-identity-based rules vs. hybrid rules: rates of violence against natal females in female spaces, rates of violence against trans women in male spaces, competitive fairness metrics in female sports.',
    'If biology-based rules show no safety advantage for natal females over hybrid/identity rules, the coordination function is not empirically sustained and the constraint trends toward snare. If they show clear advantage, the tangled_rope classification is reinforced.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(protective_function_empirical_basis, empirical, 'Whether the protective rationale is empirically supported or has become pretextual').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (legal barriers, institutional assignment) or internalized (trans women self-excluding from female spaces anticipating rejection)?',
    'Post-exit suppression trajectory: if suppression persists after legal barriers are removed (e.g., in jurisdictions that adopt self-ID), reclassify as partially internalized.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests — the target carries the suppression with them after exit.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(woman_female_category__sex_biology_reading, 2015, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(woma_tr_t2015, woman_female_category__sex_biology_reading, theater_ratio, 2015, 0.08).
narrative_ontology:measurement(woma_tr_t2017, woman_female_category__sex_biology_reading, theater_ratio, 2017, 0.1).
narrative_ontology:measurement(woma_tr_t2019, woman_female_category__sex_biology_reading, theater_ratio, 2019, 0.12).
narrative_ontology:measurement(woma_tr_t2021, woman_female_category__sex_biology_reading, theater_ratio, 2021, 0.15).
narrative_ontology:measurement(woma_tr_t2023, woman_female_category__sex_biology_reading, theater_ratio, 2023, 0.17).
narrative_ontology:measurement(woma_tr_t2025, woman_female_category__sex_biology_reading, theater_ratio, 2025, 0.18).

% Extraction over time
narrative_ontology:measurement(woma_be_t2015, woman_female_category__sex_biology_reading, base_extractiveness, 2015, 0.12).
narrative_ontology:measurement(woma_be_t2017, woman_female_category__sex_biology_reading, base_extractiveness, 2017, 0.15).
narrative_ontology:measurement(woma_be_t2019, woman_female_category__sex_biology_reading, base_extractiveness, 2019, 0.19).
narrative_ontology:measurement(woma_be_t2021, woman_female_category__sex_biology_reading, base_extractiveness, 2021, 0.23).
narrative_ontology:measurement(woma_be_t2023, woman_female_category__sex_biology_reading, base_extractiveness, 2023, 0.26).
narrative_ontology:measurement(woma_be_t2025, woman_female_category__sex_biology_reading, base_extractiveness, 2025, 0.28).

% Suppression requirement over time
narrative_ontology:measurement(woma_su_t2015, woman_female_category__sex_biology_reading, suppression_requirement, 2015, 0.35).
narrative_ontology:measurement(woma_su_t2017, woman_female_category__sex_biology_reading, suppression_requirement, 2017, 0.42).
narrative_ontology:measurement(woma_su_t2019, woman_female_category__sex_biology_reading, suppression_requirement, 2019, 0.5).
narrative_ontology:measurement(woma_su_t2021, woman_female_category__sex_biology_reading, suppression_requirement, 2021, 0.55).
narrative_ontology:measurement(woma_su_t2023, woman_female_category__sex_biology_reading, suppression_requirement, 2023, 0.59).
narrative_ontology:measurement(woma_su_t2025, woman_female_category__sex_biology_reading, suppression_requirement, 2025, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(woman_female_category__sex_biology_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(woman_female_category__sex_biology_reading, 0.08).
narrative_ontology:affects_constraint(woman_female_category__sex_biology_reading, woman_female_category__gender_identity_reading).
narrative_ontology:affects_constraint(woman_female_category__sex_biology_reading, woman_female_category__hybrid_contextual_reading).
narrative_ontology:affects_constraint(woman_female_category__sex_biology_reading, prison_housing_policy__biology_based).
narrative_ontology:affects_constraint(woman_female_category__sex_biology_reading, shelter_access_policy__biology_based).
narrative_ontology:affects_constraint(woman_female_category__sex_biology_reading, sports_eligibility__biology_based).

% DUAL FORMULATION NOTE:
% This is the sex_biology_reading of the kernel 'woman_female_category'. It forecloses the gender_identity_reading within any single legal framework, coexists_with the hybrid_contextual_reading across jurisdictions, and influences the hybrid reading by setting the default for high-stakes institutional contexts (prisons, sports). The sibling readings are separate constraint stories with their own ε and victim/beneficiary structures.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(woman_female_category__sex_biology_reading, powerless, 0.95).
constraint_indexing:directionality_override(woman_female_category__sex_biology_reading, moderate, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
