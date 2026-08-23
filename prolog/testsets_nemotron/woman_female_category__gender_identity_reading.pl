% ============================================================================
% CONSTRAINT STORY: woman_female_category__gender_identity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_woman_female_category__gender_identity_reading, []).

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
 *   constraint_id: woman_female_category__gender_identity_reading
 *   human_readable: Gender Identity Reading of Woman/Female Category
 *   domain: political_philosophy/bioethics/gender_studies/law
 *
 * SUMMARY:
 *   This constraint story captures the gender-identity reading of the
 *   'woman/female' category — the claim that category membership is
 *   determined solely by internal self-identification, independent of
 *   biological sex. This reading has been codified in self-ID laws (Argentina
 *   2012, Ireland 2015, multiple US states, Scotland's blocked GRA reform),
 *   institutional policies (sports governing bodies, prison services, NHS
 *   trusts), and corporate diversity mandates. The constraint operates as a
 *   tangled rope: it performs a genuine coordination function (providing a
 *   clear, administrable legal status for trans people, replacing opaque
 *   medical gatekeeping) while simultaneously extracting from female-bodied
 *   people the integrity of their single-sex spaces, sports, data, and
 *   dignity. The coordination function is real — trans people faced genuine
 *   exclusion — but the extraction is asymmetric: the costs fall on a class
 *   (adult human females) that had no role in designing the solution and
 *   cannot exit the redefinition. Enforcement has intensified dramatically
 *   2018-2024: suppression of dissent (no-platforming, employment loss,
 *   police investigation of 'misgendering'), capture of regulatory bodies,
 *   and rewriting of equality guidance to treat sex-based objections as hate
 *   speech.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(woman_female_category__gender_identity_reading, 0.72).
domain_priors:suppression_score(woman_female_category__gender_identity_reading, 0.78).
domain_priors:theater_ratio(woman_female_category__gender_identity_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(woman_female_category__gender_identity_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(woman_female_category__gender_identity_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(woman_female_category__gender_identity_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(woman_female_category__gender_identity_reading, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(woman_female_category__gender_identity_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(woman_female_category__gender_identity_reading, tangled_rope).
narrative_ontology:human_readable(woman_female_category__gender_identity_reading, "Gender Identity Reading of Woman/Female Category").
narrative_ontology:topic_domain(woman_female_category__gender_identity_reading, "political_philosophy/bioethics/gender_studies/law").

domain_priors:requires_active_enforcement(woman_female_category__gender_identity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(woman_female_category__gender_identity_reading, '9832dbdb-227b-4d06-b72e-78f2d175e562').
narrative_ontology:cs_kernel_codification('9832dbdb-227b-4d06-b72e-78f2d175e562', formalized).
narrative_ontology:cs_authority_grounding('9832dbdb-227b-4d06-b72e-78f2d175e562', extraction).
narrative_ontology:cs_interpretation_layer_present('9832dbdb-227b-4d06-b72e-78f2d175e562').
narrative_ontology:cs_reading_relation('9832dbdb-227b-4d06-b72e-78f2d175e562', woman_female_category__sex_biology_reading, forecloses).
narrative_ontology:cs_reading_relation('9832dbdb-227b-4d06-b72e-78f2d175e562', woman_female_category__hybrid_contextual_reading, influences).
narrative_ontology:cs_axiom('9832dbdb-227b-4d06-b72e-78f2d175e562', foundational, gender_identity_determines_category_membership).
narrative_ontology:cs_axiom_status(gender_identity_determines_category_membership, holdable).
narrative_ontology:cs_axiom_grounding('9832dbdb-227b-4d06-b72e-78f2d175e562', gender_identity_determines_category_membership, deontological).
narrative_ontology:cs_axiom('9832dbdb-227b-4d06-b72e-78f2d175e562', secondary, sex_category_has_no_independent_moral_significance).
narrative_ontology:cs_axiom_status(sex_category_has_no_independent_moral_significance, holdable).
narrative_ontology:cs_axiom_grounding('9832dbdb-227b-4d06-b72e-78f2d175e562', sex_category_has_no_independent_moral_significance, deontological).
narrative_ontology:cs_reference_frame('9832dbdb-227b-4d06-b72e-78f2d175e562', gender_identity_self_determination_framework).
narrative_ontology:cs_drift_state('9832dbdb-227b-4d06-b72e-78f2d175e562', contemporary_post_cass_review, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('9832dbdb-227b-4d06-b72e-78f2d175e562', '').
narrative_ontology:cs_kernel_id(woman_female_category__gender_identity_reading, woman_female_category).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(woman_female_category__gender_identity_reading, transgender_individuals).
narrative_ontology:constraint_beneficiary(woman_female_category__gender_identity_reading, gender_identity_advocacy_organizations).
narrative_ontology:constraint_victim(woman_female_category__gender_identity_reading, cisgender_women_in_female_only_spaces).
narrative_ontology:constraint_victim(woman_female_category__gender_identity_reading, female_athletes_in_segregated_competition).
narrative_ontology:constraint_victim(woman_female_category__gender_identity_reading, survivors_of_sex_based_violence_in_single_sex_services).
narrative_ontology:constraint_victim(woman_female_category__gender_identity_reading, medical_patients_requiring_sex_specific_care).
narrative_ontology:constraint_vindicates(woman_female_category__gender_identity_reading, gender_identity_self_determination).
narrative_ontology:constraint_vindicates(woman_female_category__gender_identity_reading, trans_inclusion_in_legal_category_woman).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Seek legal recognition and access to spaces, services, and protections aligned with their gender identity. For trans women, the reading grants access to female-only spaces (shelters, prisons, sports, changing facilities) without medical gatekeeping. The benefit is existential — recognition of identity and relief from dysphoria-inducing exclusion. Exit from the constraint is identity_locked: transition is irreversible for many, and the constraint's recognition constitutes their social/legal reality.
narrative_ontology:constraint_stakeholder(woman_female_category__gender_identity_reading, transgender_individuals, beneficiary,
    moderate, biographical, identity_locked, global).

% Drive litigation, policy advocacy, and cultural campaigns to entrench gender-identity-based category definitions in law and institutional practice. They gain institutional legitimacy, funding, and political capital from each successful codification. Their exit is arbitrage-grade: they operate across jurisdictions and can shift focus to other rights fronts if this reading loses traction.
narrative_ontology:constraint_stakeholder(woman_female_category__gender_identity_reading, gender_identity_advocacy_organizations, beneficiary,
    organized, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(woman_female_category__gender_identity_reading, gender_identity_advocacy_organizations, agenda_setter).

% Experience the constraint as loss of single-sex provision: shelters, prisons, changing rooms, and intimate-care settings become mixed-sex by policy. The harm is dignitary (loss of privacy from male-bodied persons), safety (statistically elevated risk in mixed-sex environments), and political (erasure of sex-based organizing). Exit is constrained: they can leave specific spaces but cannot exit the legal regime that redefines 'woman' to include male-bodied persons.
narrative_ontology:constraint_stakeholder(woman_female_category__gender_identity_reading, cisgender_women_in_female_only_spaces, payer,
    moderate, biographical, constrained, regional).

% Face competitive displacement and record erosion when male-puberty-advantaged athletes enter female categories under self-ID policies. The harm is material (scholarships, prizes, qualification slots) and symbolic (the category's communicative function — 'this is the female class' — collapses). Exit is constrained: they can quit sport or move to mixed competition, but the constraint follows them through governing-body policies.
narrative_ontology:constraint_stakeholder(woman_female_category__gender_identity_reading, female_athletes_in_segregated_competition, payer,
    moderate, biographical, constrained, global).

% Depend on single-sex refuges, counseling, and medical examination services for recovery from male violence. The reading's inclusion of trans women in these spaces removes the guarantee of male-free environments. Many survivors report re-traumatization and service avoidance. Exit is trapped: they are in crisis, often economically dependent, and the constraint governs the only services available to them.
narrative_ontology:constraint_stakeholder(woman_female_category__gender_identity_reading, survivors_of_sex_based_violence_in_single_sex_services, payer,
    powerless, biographical, trapped, regional).

% Require accurate sex recording for diagnosis, dosing, screening, and epidemiological tracking. The reading's conflation of gender identity with sex category introduces clinical errors (e.g., missed prostate cancer in trans women recorded as female, inappropriate cervical screening invitations for trans men). Exit is constrained: patients cannot choose medical reality; they bear the cost of data corruption.
narrative_ontology:constraint_stakeholder(woman_female_category__gender_identity_reading, medical_patients_requiring_sex_specific_care, payer,
    moderate, biographical, constrained, national).

% Legislatures, courts, and regulatory bodies that codify or adjudicate the category definition. They bear the enforcement cost (litigation, compliance monitoring, institutional redesign) and gain legitimacy from aligning with rights-expansion narratives. Their exit is analytical: they can revise the rule but face massive political and legal inertia.
narrative_ontology:constraint_stakeholder(woman_female_category__gender_identity_reading, legal_and_policy_institutions, agenda_setter,
    institutional, generational, analytical, national).

% Feminist organizations, biologists, and legal scholars who hold that 'woman' denotes adult human female. They are structurally excluded from policy formation under this reading — their framework is treated as hate speech rather than a competing interpretation. Their exit is identity_locked: their intellectual and political identity is constituted by this dispute; abandoning it dissolves their project.
narrative_ontology:constraint_stakeholder(woman_female_category__gender_identity_reading, sex_biology_reading_adherents, excluded,
    organized, generational, identity_locked, global).

% Analyze the constraint's coherence, its collision with other rights frameworks, and its long-term effects on sex-based protections and scientific taxonomy. They neither collect nor pay but map the structural tension between identity-based and biology-based category systems.
narrative_ontology:constraint_stakeholder(woman_female_category__gender_identity_reading, bioethics_and_philosophy_observers, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a unified, administrable legal category 'woman' that includes trans women, enabling anti-discrimination law, institutional policy, and social recognition to operate without case-by-case adjudication of transition status or medical history.
% TRANSFER_FUNCTION: Transfers the communicative, legal, and spatial integrity of the category 'woman' — its function as a stable reference class for female-only spaces, sports, data, and protections — from cisgender women (who lose exclusive claim) to transgender individuals (who gain inclusive access). Also transfers enforcement costs to institutions and dignity/safety costs to vulnerable female subpopulations.
% ABSENT_VOICES: Women in the Global South whose sex-based rights (maternal health, FGM prevention, girl-child education) depend on a stable biological category; detransitioners whose testimony about medical harm is marginalized; parents of gender-questioning children who lose sex-based safeguarding frameworks. These voices are excluded from the 'inclusive' consensus because their objections are framed as anti-trans rather than pro-woman/child.
% DISAPPEARANCE_RATIONALE: If the gender-identity reading vanished overnight, legal definitions would revert to sex-based criteria in most jurisdictions. Female-only spaces, sports, and data categories would be legally reinstated. Trans women would lose automatic access to female spaces but retain anti-discrimination protections on gender-reassignment grounds (as in UK Equality Act 2010). The world rearranges because the constraint actively structures law, institutional policy, and social norms across dozens of jurisdictions.
% FOUNDING_PROBLEM: Transgender individuals faced systemic exclusion from legal recognition, healthcare, employment, and public life. The binary sex classification system provided no pathway for trans women to be recognized as women, producing material harm (denial of services, violence, documentation mismatches) and existential harm (social erasure).
% FOUNDING_PROBLEM_CORROBORATION: The founding problem is corroborated by extensive medical, legal, and sociological literature on trans marginalization (e.g., WPATH Standards of Care, UN Free & Equal campaign reports, European Court of Human Rights case law). However, the status is contested: gender-critical feminists and some bioethicists argue the founding problem has been substantially solved by gender-reassignment protections that do not require category collapse, and that the current reading over-solves by eliminating the sex category itself.
narrative_ontology:disappearance_verdict(woman_female_category__gender_identity_reading, world_rearranges).
narrative_ontology:founding_problem_status(woman_female_category__gender_identity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(woman_female_category__gender_identity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_nemotron+rescue1', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(woman_female_category__gender_identity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(woman_female_category__gender_identity_reading, 0.72, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(woman_female_category__gender_identity_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(woman_female_category__gender_identity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(woman_female_category__gender_identity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.72) is high because the constraint transfers the entire communicative and protective function of the sex category to serve identity claims, while the harms to female-bodied people are diffuse, structural, and dismissed as non-harms. Suppression (0.78) is higher still because the constraint's persistence depends on active enforcement: compelled speech (pronoun mandates), institutional capture (equality bodies, sports federations, medical colleges), and social/legal punishment of the rival reading. Theater ratio (0.28) reflects that the coordination function (legal clarity for trans people) is real but shrinking as a proportion of total enforcement activity — more energy now goes into policing boundaries of the reading itself (defining 'woman' circularly, suppressing 'adult human female' as hate speech) than maintaining the trans-inclusion coordination. Accessibility collapse (0.42) is moderate: the sex-based reading remains intellectually and legally coherent (UK Equality Act 2010, Cass Review, multiple state laws), but its institutional expression has been severely restricted. Resistance (0.68) is high and rising: litigation (For Women Scotland, Bostock dissent, sports federation reversals), legislative pushback (US state bills, UK Cass Review implementation), and the emergence of the hybrid reading as a structural competitor.
 *
 * PERSPECTIVAL GAP:
 *   From the beneficiary seat (trans individuals), the constraint is a rope: it solves a genuine coordination problem (legal recognition without medical gatekeeping) with minimal coercive overhead — the harm to others is denied or minimized. From the payer seats (cis women, female athletes, survivors), the constraint is a snare: the coordination story is cover; persistence depends on coercion and suppression of the sex-based alternative. The engine computes this divergence from the structural data. The agenda-setter seat (legal institutions) experiences it as a tangled rope: genuine coordination function, real extraction, active enforcement required. The observer seat sees the full structure: a constraint family in active contest with two sibling readings.
 *
 * DIRECTIONALITY LOGIC:
 *   Transgender individuals and their advocacy organizations are structural beneficiaries (d ≈ 0.15-0.25): they collect the recognition, access, and legal protections the constraint creates. Cisgender women in female-only spaces, female athletes, survivors, and medical patients are structural targets (d ≈ 0.75-0.95): they bear the costs (lost privacy, safety, fairness, clinical accuracy) with constrained or trapped exit. Legal institutions sit near symmetric (d ≈ 0.5): they gain legitimacy from rights-expansion narratives but bear enforcement costs and legitimacy risks. Sex-biology adherents are excluded (d ≈ 0.9): their framework is structurally suppressed, not coordinated. The directionality gradient is steep because the constraint redefines the victim class out of existence — 'woman' no longer names the people harmed by the redefinition.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (trans exclusion from legal recognition) was substantially solved in many jurisdictions by gender-reassignment protections that preserved the sex category (UK Equality Act 2010, GRA 2004). The gender-identity reading over-solves by collapsing the category itself, creating new victims where none existed before. The constraint persists not because the founding problem remains live in its original form, but because the advocacy infrastructure, institutional capture, and identity-locked beneficiaries now depend on the expanded reading. Mandatrophy is unresolved: the arrangement has outgrown its founding justification but the machinery of enforcement has become self-sustaining.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    coordination_extraction_boundary,
    'Is the gender-identity reading''s coordination function (clear legal status for trans people) structurally separable from its extraction function (elimination of sex-based category integrity for females), or are they necessarily bundled?',
    'Natural experiment from jurisdictions adopting the hybrid reading (context-dependent category): if trans legal recognition and female single-sex provisions can coexist stably, the functions are separable and this reading''s extraction is optional. If the hybrid proves unstable (pressure toward full self-ID or full sex-based), the functions may be structurally coupled.',
    'If separable, this reading is a tangled rope where extraction is a design choice, not a coordination necessity — supports reform toward hybrid. If inseparable, the reading is either a rope (if extraction is the price of coordination) or a snare (if coordination is cover).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_extraction_boundary, conceptual, 'Whether the constraint''s coordination and extraction components are structurally separable — the core empirical question for the hybrid reading''s viability.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the high suppression (0.78) primarily structural (institutional capture, legal mandates, employment consequences) or internalized (self-censorship, identity fusion with the reading, fear of moral contamination)?',
    'Longitudinal study of dissent expression: track whether individuals who privately reject the reading but publicly conform show suppression persistence after institutional pressure relaxes (e.g., post-Cass Review NHS guidance changes). If internalized suppression persists, the constraint''s effective suppression exceeds structural measures.',
    'If substantially internalized, the constraint''s effective suppression is higher than institutional metrics suggest — the target population carries the suppression with them. This would increase effective extraction for payer seats and strengthen snare classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism in the gender-identity reading''s enforcement.').

omega_variable(
    kernel_framing_underdetermination,
    'Does the woman_female_category kernel admit only three coherent readings (identity, biology, hybrid), or is there a fourth: the ''category abolition'' reading that rejects the category''s political salience entirely?',
    'Map the full discursive field: identify actors who reject the kernel''s premise (e.g., post-gender feminists, queer theorists rejecting sex/gender binary, materialists rejecting category politics). Assess whether their position instantiates a distinct constraint with its own ε, beneficiaries, and victims.',
    'If a fourth reading exists, the kernel''s constraint family is larger than declared. The current three-reading decomposition may misattribute extraction that belongs to the abolition reading''s structural pressure.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_framing_underdetermination, conceptual, 'Whether the kernel decomposition captures the full space of structurally distinct readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(woman_female_category__gender_identity_reading, 2010, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(woma_tr_t2010, woman_female_category__gender_identity_reading, theater_ratio, 2010, 0.12).
narrative_ontology:measurement(woma_tr_t2014, woman_female_category__gender_identity_reading, theater_ratio, 2014, 0.15).
narrative_ontology:measurement(woma_tr_t2018, woman_female_category__gender_identity_reading, theater_ratio, 2018, 0.18).
narrative_ontology:measurement(woma_tr_t2020, woman_female_category__gender_identity_reading, theater_ratio, 2020, 0.22).
narrative_ontology:measurement(woma_tr_t2022, woman_female_category__gender_identity_reading, theater_ratio, 2022, 0.26).
narrative_ontology:measurement(woma_tr_t2024, woman_female_category__gender_identity_reading, theater_ratio, 2024, 0.28).

% Extraction over time
narrative_ontology:measurement(woma_be_t2010, woman_female_category__gender_identity_reading, base_extractiveness, 2010, 0.35).
narrative_ontology:measurement(woma_be_t2014, woman_female_category__gender_identity_reading, base_extractiveness, 2014, 0.45).
narrative_ontology:measurement(woma_be_t2018, woman_female_category__gender_identity_reading, base_extractiveness, 2018, 0.58).
narrative_ontology:measurement(woma_be_t2020, woman_female_category__gender_identity_reading, base_extractiveness, 2020, 0.65).
narrative_ontology:measurement(woma_be_t2022, woman_female_category__gender_identity_reading, base_extractiveness, 2022, 0.7).
narrative_ontology:measurement(woma_be_t2024, woman_female_category__gender_identity_reading, base_extractiveness, 2024, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(woma_su_t2010, woman_female_category__gender_identity_reading, suppression_requirement, 2010, 0.45).
narrative_ontology:measurement(woma_su_t2014, woman_female_category__gender_identity_reading, suppression_requirement, 2014, 0.55).
narrative_ontology:measurement(woma_su_t2018, woman_female_category__gender_identity_reading, suppression_requirement, 2018, 0.65).
narrative_ontology:measurement(woma_su_t2020, woman_female_category__gender_identity_reading, suppression_requirement, 2020, 0.72).
narrative_ontology:measurement(woma_su_t2022, woman_female_category__gender_identity_reading, suppression_requirement, 2022, 0.76).
narrative_ontology:measurement(woma_su_t2024, woman_female_category__gender_identity_reading, suppression_requirement, 2024, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(woman_female_category__gender_identity_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(woman_female_category__gender_identity_reading, 0.08).
narrative_ontology:affects_constraint(woman_female_category__gender_identity_reading, woman_female_category__sex_biology_reading).
narrative_ontology:affects_constraint(woman_female_category__gender_identity_reading, woman_female_category__hybrid_contextual_reading).

% DUAL FORMULATION NOTE:
% This reading and its two siblings form the woman_female_category constraint family. Each reading has a distinct ε (this reading: 0.72; sex_biology_reading: ~0.15; hybrid_contextual_reading: ~0.35) because they extract from different populations via different mechanisms. The gender-identity reading extracts from female-bodied people via category collapse; the sex-biology reading extracts from trans people via exclusion; the hybrid reading attempts to minimize total extraction by context-splitting. The family is linked because each reading is cited as evidence against the others in policy and litigation.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(woman_female_category__gender_identity_reading, organized, 0.3).
constraint_indexing:directionality_override(woman_female_category__gender_identity_reading, powerless, 0.95).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
