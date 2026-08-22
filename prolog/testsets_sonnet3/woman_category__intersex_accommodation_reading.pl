% ============================================================================
% CONSTRAINT STORY: woman_category__intersex_accommodation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_woman_category__intersex_accommodation_reading, []).

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
 *   constraint_id: woman_category__intersex_accommodation_reading
 *   human_readable: Sex/Gender Category Boundary — Intersex Accommodation Reading
 *   domain: political_philosophy/law/bioethics
 *
 * SUMMARY:
 *   This story instantiates the intersex-accommodation reading of the
 *   contested 'woman' category kernel: 'woman' is defined to include typical
 *   female biology plus intersex variations that do not reduce cleanly to the
 *   male category. Under this reading the category boundary is drawn around a
 *   biological spectrum rather than a binary, which is low-stakes in most
 *   administrative and legal contexts (a small population, modest ε) but
 *   becomes acutely extractive in elite sport, where the same boundary
 *   question determines competitive eligibility, income, and public standing
 *   for a specific set of DSD athletes. This is a distinct constraint from
 *   the sex_biology_reading (chromosomal/anatomical binary) and the
 *   gender_identity_reading (self-identification) — see kernel_context for
 *   the relationship.
 *
 * KEY AGENTS:
 *   - dsd_athletes_in_elite_female_sport: primary target (powerless/trapped) — bears eligibility and verification extraction
 *   - intersex_individuals_subject_to_verification_testing: primary target (powerless/trapped) — bears testing and disclosure burden
 *   - intersex_advocacy_organizations: primary beneficiary (organized/mobile) — gains recognition and policy wins without bearing enforcement costs
 *   - sport_governing_bodies: agenda-setting institution (institutional/arbitrage) — sets and can shift the eligibility line
 *   - female_typical_competitors: excluded party (moderate/constrained) — competitive-fairness concern rarely centered
 *   - legal_and_administrative_systems: analytical observer (institutional/analytical) — adjudicates without resolving the underlying kernel contest
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(woman_category__intersex_accommodation_reading, 0.42).
domain_priors:suppression_score(woman_category__intersex_accommodation_reading, 0.55).
domain_priors:theater_ratio(woman_category__intersex_accommodation_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(woman_category__intersex_accommodation_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(woman_category__intersex_accommodation_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(woman_category__intersex_accommodation_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(woman_category__intersex_accommodation_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(woman_category__intersex_accommodation_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(woman_category__intersex_accommodation_reading, tangled_rope).
narrative_ontology:human_readable(woman_category__intersex_accommodation_reading, "Sex/Gender Category Boundary — Intersex Accommodation Reading").
narrative_ontology:topic_domain(woman_category__intersex_accommodation_reading, "political_philosophy/law/bioethics").

domain_priors:requires_active_enforcement(woman_category__intersex_accommodation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(woman_category__intersex_accommodation_reading, 'ae71ca6b-6766-4084-af91-d1faa5aad00e').
narrative_ontology:cs_kernel_codification('ae71ca6b-6766-4084-af91-d1faa5aad00e', distributed).
narrative_ontology:cs_authority_grounding('ae71ca6b-6766-4084-af91-d1faa5aad00e', distributed).
narrative_ontology:cs_reading_relation('ae71ca6b-6766-4084-af91-d1faa5aad00e', woman_category__sex_biology_reading, influences).
narrative_ontology:cs_reading_relation('ae71ca6b-6766-4084-af91-d1faa5aad00e', woman_category__gender_identity_reading, coexists_with).
narrative_ontology:cs_axiom('ae71ca6b-6766-4084-af91-d1faa5aad00e', foundational, biological_sex_is_not_strictly_binary).
narrative_ontology:cs_axiom_status(biological_sex_is_not_strictly_binary, holdable).
narrative_ontology:cs_axiom_grounding('ae71ca6b-6766-4084-af91-d1faa5aad00e', biological_sex_is_not_strictly_binary, empirically_contingent).
narrative_ontology:cs_axiom('ae71ca6b-6766-4084-af91-d1faa5aad00e', foundational, category_membership_grounded_in_biology_not_self_report).
narrative_ontology:cs_axiom_status(category_membership_grounded_in_biology_not_self_report, holdable).
narrative_ontology:cs_axiom_grounding('ae71ca6b-6766-4084-af91-d1faa5aad00e', category_membership_grounded_in_biology_not_self_report, conventional).
narrative_ontology:cs_reference_frame('ae71ca6b-6766-4084-af91-d1faa5aad00e', clinical_dsd_recognition_framework).
narrative_ontology:cs_drift_state('ae71ca6b-6766-4084-af91-d1faa5aad00e', post_semenya_arbitration_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('ae71ca6b-6766-4084-af91-d1faa5aad00e', '').
narrative_ontology:cs_kernel_id(woman_category__intersex_accommodation_reading, woman_category).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(woman_category__intersex_accommodation_reading, intersex_advocacy_organizations).
narrative_ontology:constraint_beneficiary(woman_category__intersex_accommodation_reading, medical_and_legal_taxonomists_seeking_accurate_classification).
narrative_ontology:constraint_victim(woman_category__intersex_accommodation_reading, dsd_athletes_in_elite_female_sport).
narrative_ontology:constraint_victim(woman_category__intersex_accommodation_reading, intersex_individuals_subject_to_verification_testing).
narrative_ontology:constraint_vindicates(woman_category__intersex_accommodation_reading, biological_sex_is_a_spectrum_not_a_binary).
narrative_ontology:constraint_vindicates(woman_category__intersex_accommodation_reading, female_category_boundary_is_underdetermined_at_the_margins).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Athletes with differences of sex development (naturally elevated testosterone, XY karyotype with androgen insensitivity variants, or other intersex conditions) who were raised and legally registered as women, but whose eligibility for female competition is periodically challenged or conditioned on hormone suppression. Under this reading their biology is a legitimate variant of female sex, not a disqualifying deviation from it, but sport governing bodies often treat them as needing to be regulated toward the male-typical boundary. Their career, income, and public identity all ride on where the line is drawn; exit means abandoning elite competition entirely.
narrative_ontology:constraint_stakeholder(woman_category__intersex_accommodation_reading, dsd_athletes_in_elite_female_sport, payer,
    powerless, biographical, trapped, global).

% People whose bodies do not sort cleanly into the chromosomal/anatomical binary and who are subjected to genital exams, karyotyping, or hormone panels to adjudicate their category membership for sport, prisons, or legal documents. This reading says the testing apparatus itself is a category error — treating a spectrum as a binary requiring adjudication. They bear the material cost (invasive testing, public disclosure of medical information, exclusion) of institutions refusing to build accommodation into the category definition.
narrative_ontology:constraint_stakeholder(woman_category__intersex_accommodation_reading, intersex_individuals_subject_to_verification_testing, payer,
    powerless, biographical, trapped, national).

% Advocacy groups and clinicians who campaign for legal and medical taxonomies to explicitly include intersex variation within (or adjacent to) the female category rather than forcing a binary sort. They gain legitimacy, funding, and policy wins when institutions adopt spectrum-based definitions; they do not personally bear the enforcement costs the constraint imposes on individual athletes or applicants.
narrative_ontology:constraint_stakeholder(woman_category__intersex_accommodation_reading, intersex_advocacy_organizations, beneficiary,
    organized, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(woman_category__intersex_accommodation_reading, intersex_advocacy_organizations, agenda_setter).

% Bodies such as World Athletics that set eligibility rules for women's competition. They must decide whether to accommodate the spectrum reading (risking perceived unfairness to female-typical competitors) or enforce a binary/testosterone-threshold rule (risking exclusion of legitimately female-identified intersex athletes). They control the enforcement machinery — testing protocols, eligibility panels, appeals processes — and can shift the line without needing to justify it to the athletes it falls on.
narrative_ontology:constraint_stakeholder(woman_category__intersex_accommodation_reading, sport_governing_bodies, agenda_setter,
    institutional, generational, arbitrage, global).

% Athletes with typical XX female biology who compete in the same category and who argue that including higher-testosterone DSD variants erodes the competitive protection the female category exists to provide. Their objection is rarely centered in medical or legal taxonomy debates, which tend to be framed around intersex inclusion or exclusion rather than the competitive-fairness tradeoff they experience directly.
narrative_ontology:constraint_stakeholder(woman_category__intersex_accommodation_reading, female_typical_competitors, excluded,
    moderate, biographical, constrained, global).

% Courts, registries, and legislatures that must decide how far 'woman' as a legal category stretches to accommodate biological variation, in contexts from birth certificates to single-sex spaces to sports law. They observe and sometimes adjudicate the tension between this reading and its siblings without being able to resolve it definitively, since the underlying kernel is contested across multiple domains simultaneously.
narrative_ontology:constraint_stakeholder(woman_category__intersex_accommodation_reading, legal_and_administrative_systems, observer,
    institutional, civilizational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a taxonomy that can classify real intersex bodies without forcing a false binary sort, allowing legal, medical, and social systems to process cases that do not fit cleanly into 'typical male' or 'typical female' categories without erasing or misclassifying the people involved.
% TRANSFER_FUNCTION: In most policy domains this reading transfers very little — mainly recognition and administrative accommodation from institutions to a small population. In elite sport specifically, it shifts competitive opportunity and prize/sponsorship income between female-typical athletes and DSD athletes, depending on which direction the eligibility line is drawn; it also transfers scrutiny and invasive verification burden onto intersex individuals whenever institutions resist accommodating the spectrum outright.
% ABSENT_VOICES: DSD athletes themselves are rarely direct parties to the governance bodies that set eligibility rules; their objections are filtered through advocacy organizations or litigation (e.g., Semenya's CAS and ECHR proceedings) rather than voiced in the rule-making room. Female-typical competitors' fairness concerns are similarly often litigated through media and advocacy rather than being seated at the definitional table alongside intersex advocates.
% DISAPPEARANCE_RATIONALE: If this reading vanished as a recognized framework, most administrative and legal systems would likely default back toward a strict binary (sex_biology_reading) or a self-identification standard (gender_identity_reading), each of which would concretely rearrange eligibility outcomes for the athletes and individuals it currently covers — for elite sport specifically the world clearly rearranges (eligibility rules would revert to pure testosterone-threshold or chromosomal tests); for most other legal contexts the practical effect is smaller because so few cases are adjudicated on this exact boundary.
% FOUNDING_PROBLEM: Existing binary sex categories in law, medicine, and sport could not accommodate real, non-pathological biological variation (intersex conditions, DSDs) without either misclassifying people or subjecting them to invasive proof-of-sex procedures.
% FOUNDING_PROBLEM_CORROBORATION: Clinical geneticists and endocrinologists outside intersex advocacy (e.g., researchers publishing on DSD prevalence and classification in peer-reviewed medical literature) corroborate that biological sex characteristics do not sort into a clean binary at the population level; sports arbitration bodies (CAS, ECHR in the Semenya rulings) independently corroborate that the boundary-drawing problem is real and unresolved, even where they reach different conclusions than intersex advocacy organizations about how to resolve it.
narrative_ontology:disappearance_verdict(woman_category__intersex_accommodation_reading, contested).
narrative_ontology:founding_problem_status(woman_category__intersex_accommodation_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(woman_category__intersex_accommodation_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(woman_category__intersex_accommodation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(woman_category__intersex_accommodation_reading, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(woman_category__intersex_accommodation_reading_tests).
:- end_tests(woman_category__intersex_accommodation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored moderate (0.42) rather than low or high because the reading's effect is domain-dependent: near-zero in most legal/administrative contexts (a birth certificate or ID document rarely turns on this exact boundary) but substantial in elite sport, where eligibility panels and testosterone-threshold rules directly determine income and career continuation for a small, identifiable group. Suppression (0.55) reflects the active enforcement machinery — verification testing, eligibility panels, appeals — required to adjudicate boundary cases; this is a raw structural property and is not scaled by scope in this authored value. Accessibility collapse is moderate (0.4): alternative framings (binary sort, self-identification) remain live and contested rather than eliminated, so alternatives have not collapsed the way they would under a settled natural-law-like constraint. Resistance is authored high (0.6) because this reading is actively contested by proponents of both sibling readings, and by female-typical competitors' fairness advocates.
 *
 * PERSPECTIVAL GAP:
 *   From the sport governing body's seat, defining the accommodation is a coordination problem it is trying to solve fairly across competing claims; from a DSD athlete's seat, the same governance apparatus is a periodically-hostile verification regime that can end a career at will. The engine should compute these divergently from the declared power/exit structure — the agenda-setter has arbitrage-level exit and institutional power, while the payer seats are powerless and trapped.
 *
 * DIRECTIONALITY LOGIC:
 *   Intersex advocacy organizations are declared beneficiaries: they campaign for and gain from spectrum-inclusive taxonomy without directly bearing the enforcement costs it can trigger in specific institutional contexts (e.g., sport). DSD athletes and individuals subject to verification are declared victims: even though this reading is nominally protective of them (it says their biology counts as female), the *institutions* that must operationalize the reading often respond by building verification and threshold-based exclusion machinery, and the cost of that machinery falls on the very people the reading is meant to accommodate. This is the tangled-rope signature — a genuine coordination function (accurate taxonomy) paired with asymmetric extraction (verification burden, competitive exclusion) running through the same structure.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (accommodating real, non-binary biological variation without forcing a false binary sort) remains live — DSD conditions have not disappeared and legal/medical systems still need a taxonomy that can process them. This blocks a mandatrophy read: the arrangement is not vestigial theater over a dead problem. But the founding-problem status is corroborated as contested precisely because the mechanism built to solve it (verification testing, threshold rules) has, in elite sport, become a site of extraction that arguably exceeds what accommodation alone would require — a tangled_rope classification captures this: real coordination function, real asymmetric cost, both persisting simultaneously.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    spectrum_vs_binary_naturalness,
    'Is the underlying biological reality genuinely a spectrum requiring accommodation, or is intersex variation better modeled as rare deviation from an otherwise real binary — and does the answer change by domain (birth registration vs. elite sport eligibility vs. medical treatment)?',
    'Population-level clinical data on DSD prevalence and phenotypic distribution, combined with domain-specific analysis of whether a given administrative purpose (eligibility, documentation, medical care) requires resolving the underlying philosophical question at all.',
    'If the biological reality is better modeled as rare deviation rather than true spectrum, this reading''s coordination claim weakens in domains where a binary-with-exceptions rule would serve the same administrative purpose with less enforcement overhead; if genuinely a spectrum, the accommodation framing is closer to a natural-law-adjacent claim about biology itself.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(spectrum_vs_binary_naturalness, empirical, 'Whether intersex variation is structurally a spectrum or a rare deviation from a real binary.').

omega_variable(
    committer_kernel_disagreement_location,
    'This constraint is one reading of the woman_category kernel (siblings: sex_biology_reading, gender_identity_reading). Where exactly does the disagreement between readings live — is it about what biological sex IS, about whether biology or identity should ground the category, or about how much administrative accommodation a spectrum-reality requires?',
    'Structural decomposition of each reading''s foundational axioms (see cs_structure.axioms) to identify whether the disagreement is empirical (what is biological sex, factually), definitional (what should the word ''woman'' track), or purely a policy-accommodation question (how much does law need to bend to biological edge cases).',
    'If the disagreement is purely empirical, resolving the underlying biology question could substantially narrow the contest; if it is definitional/normative, no amount of biological data resolves which reading should govern legal category membership — this determines whether the kernel contest is resolvable in principle.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(committer_kernel_disagreement_location, conceptual, 'Locating the structural site of disagreement among the three sibling readings of the woman_category kernel.').

omega_variable(
    elite_sport_ceiling_effect,
    'Does the sharply higher ε in elite sport (relative to near-zero ε in most other administrative domains) mean this reading should actually be decomposed further into a sport-specific constraint and a general-administrative constraint, per the ε-invariance principle?',
    'Compare whether the same beneficiary/victim structure and coordination function hold across domains, or whether elite sport eligibility constitutes a sufficiently distinct extraction mechanism (competitive/economic stakes) to warrant its own story with its own ε.',
    'If elite sport genuinely functions as a structurally distinct constraint (different stakes, different enforcement mechanism, different victim population characteristics), the single ε=0.42 authored here averages across two different underlying realities and should be split into two linked stories rather than one blended ε.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(elite_sport_ceiling_effect, conceptual, 'Whether elite-sport application of this reading warrants decomposition into its own constraint story.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(woman_category__intersex_accommodation_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(woma_tr_t0, woman_category__intersex_accommodation_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(woma_tr_t4, woman_category__intersex_accommodation_reading, theater_ratio, 4, 0.22).
narrative_ontology:measurement(woma_tr_t8, woman_category__intersex_accommodation_reading, theater_ratio, 8, 0.25).
narrative_ontology:measurement(woma_tr_t12, woman_category__intersex_accommodation_reading, theater_ratio, 12, 0.27).
narrative_ontology:measurement(woma_tr_t16, woman_category__intersex_accommodation_reading, theater_ratio, 16, 0.29).
narrative_ontology:measurement(woma_tr_t20, woman_category__intersex_accommodation_reading, theater_ratio, 20, 0.3).

% Extraction over time
narrative_ontology:measurement(woma_be_t0, woman_category__intersex_accommodation_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(woma_be_t4, woman_category__intersex_accommodation_reading, base_extractiveness, 4, 0.32).
narrative_ontology:measurement(woma_be_t8, woman_category__intersex_accommodation_reading, base_extractiveness, 8, 0.38).
narrative_ontology:measurement(woma_be_t12, woman_category__intersex_accommodation_reading, base_extractiveness, 12, 0.4).
narrative_ontology:measurement(woma_be_t16, woman_category__intersex_accommodation_reading, base_extractiveness, 16, 0.41).
narrative_ontology:measurement(woma_be_t20, woman_category__intersex_accommodation_reading, base_extractiveness, 20, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(woma_su_t0, woman_category__intersex_accommodation_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(woma_su_t4, woman_category__intersex_accommodation_reading, suppression_requirement, 4, 0.44).
narrative_ontology:measurement(woma_su_t8, woman_category__intersex_accommodation_reading, suppression_requirement, 8, 0.48).
narrative_ontology:measurement(woma_su_t12, woman_category__intersex_accommodation_reading, suppression_requirement, 12, 0.5).
narrative_ontology:measurement(woma_su_t16, woman_category__intersex_accommodation_reading, suppression_requirement, 16, 0.53).
narrative_ontology:measurement(woma_su_t20, woman_category__intersex_accommodation_reading, suppression_requirement, 20, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(woman_category__intersex_accommodation_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(woman_category__intersex_accommodation_reading, 0.1).
narrative_ontology:affects_constraint(woman_category__intersex_accommodation_reading, woman_category__sex_biology_reading).
narrative_ontology:affects_constraint(woman_category__intersex_accommodation_reading, woman_category__gender_identity_reading).

% DUAL FORMULATION NOTE:
% This is one of three linked readings of the woman_category kernel. sex_biology_reading grounds category membership in chromosomal/anatomical binary; gender_identity_reading grounds it in self-identification; intersex_accommodation_reading (this file) grounds it in a biological spectrum that includes non-binary biological variation within 'woman.' Each reading has its own ε, beneficiary/victim structure, and classification. This reading's ε is domain-conditional (low in most policy contexts, high in elite sport) but is authored as a single blended value here per the ε-invariance principle's disambiguation requirement — see the elite_sport_ceiling_effect omega for the case that this itself should be split further.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
