% ============================================================================
% CONSTRAINT STORY: gendered_category_membership__biological_sex_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_gendered_category_membership__biological_sex_reading, []).

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
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: gendered_category_membership__biological_sex_reading
 *   human_readable: Biological Sex as Sole Basis for Gender Category Membership
 *   domain: social_ontology/political_philosophy/bioethics
 *
 * SUMMARY:
 *   This constraint story captures the 'biological sex reading' of the
 *   contested kernel 'gendered category membership.' The reading asserts that
 *   category membership in 'woman' and 'man' is grounded exclusively in
 *   immutable biological markers (chromosomes, gametes, reproductive anatomy
 *   at birth). It claims the status of natural law (Mountain) — that sex is a
 *   biological fact, not a social construct, and that sex-based rights depend
 *   on this ontological foundation. However, the authored metrics reveal a
 *   constraint that requires active legislative and judicial enforcement,
 *   suppresses the self-identification and lived reality of trans and
 *   non-binary people, and extracts from them the recognition, safety, and
 *   resources that flow from category membership. Cisgender women are
 *   declared as beneficiaries (protected from 'category dilution'), but this
 *   beneficiary claim is structurally contested — trans advocates argue cis
 *   women are not harmed by inclusion, and the 'protection' narrative
 *   functions as extraction cover. The measurement series (2010-2030) tracks
 *   the constraint's intensification: rising extractiveness as anti-trans
 *   legislation proliferates, rising theater as 'women's rights' rhetoric
 *   increasingly masks animus, and rising suppression as legal recognition of
 *   gender identity is rolled back. The claimed Mountain type diverges
 *   sharply from the metric profile — a classic False Summit candidate.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gendered_category_membership__biological_sex_reading, 0.78).
domain_priors:suppression_score(gendered_category_membership__biological_sex_reading, 0.85).
domain_priors:theater_ratio(gendered_category_membership__biological_sex_reading, 0.32).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gendered_category_membership__biological_sex_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(gendered_category_membership__biological_sex_reading, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(gendered_category_membership__biological_sex_reading, theater_ratio, 0.32).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gendered_category_membership__biological_sex_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(gendered_category_membership__biological_sex_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gendered_category_membership__biological_sex_reading, mountain).
narrative_ontology:human_readable(gendered_category_membership__biological_sex_reading, "Biological Sex as Sole Basis for Gender Category Membership").
narrative_ontology:topic_domain(gendered_category_membership__biological_sex_reading, "social_ontology/political_philosophy/bioethics").

domain_priors:requires_active_enforcement(gendered_category_membership__biological_sex_reading).
domain_priors:emerges_naturally(gendered_category_membership__biological_sex_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gendered_category_membership__biological_sex_reading, '0f7b3ebc-c39a-4c63-9122-464fa13af1ab').
narrative_ontology:cs_kernel_codification('0f7b3ebc-c39a-4c63-9122-464fa13af1ab', formalized).
narrative_ontology:cs_authority_grounding('0f7b3ebc-c39a-4c63-9122-464fa13af1ab', extraction).
narrative_ontology:cs_interpretation_layer_present('0f7b3ebc-c39a-4c63-9122-464fa13af1ab').
narrative_ontology:cs_reading_relation('0f7b3ebc-c39a-4c63-9122-464fa13af1ab', gendered_category_membership__gender_identity_reading, forecloses).
narrative_ontology:cs_reading_relation('0f7b3ebc-c39a-4c63-9122-464fa13af1ab', gendered_category_membership__social_role_reading, forecloses).
narrative_ontology:cs_axiom('0f7b3ebc-c39a-4c63-9122-464fa13af1ab', foundational, sex_is_immutable_binary).
narrative_ontology:cs_axiom_status(sex_is_immutable_binary, holdable).
narrative_ontology:cs_axiom_grounding('0f7b3ebc-c39a-4c63-9122-464fa13af1ab', sex_is_immutable_binary, empirically_contingent).
narrative_ontology:cs_axiom('0f7b3ebc-c39a-4c63-9122-464fa13af1ab', foundational, women_have_sex_based_rights).
narrative_ontology:cs_axiom_status(women_have_sex_based_rights, holdable).
narrative_ontology:cs_axiom_grounding('0f7b3ebc-c39a-4c63-9122-464fa13af1ab', women_have_sex_based_rights, deontological).
narrative_ontology:cs_reference_frame('0f7b3ebc-c39a-4c63-9122-464fa13af1ab', biological_sex_essentialism).
narrative_ontology:cs_drift_state('0f7b3ebc-c39a-4c63-9122-464fa13af1ab', contemporary_gender_identity_recognition, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('0f7b3ebc-c39a-4c63-9122-464fa13af1ab', '2026-08-15T14:30:00Z').
narrative_ontology:cs_kernel_id(gendered_category_membership__biological_sex_reading, gendered_category_membership).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gendered_category_membership__biological_sex_reading, cisgender_women).
narrative_ontology:constraint_beneficiary(gendered_category_membership__biological_sex_reading, sex_based_rights_advocates).
narrative_ontology:constraint_victim(gendered_category_membership__biological_sex_reading, transgender_women).
narrative_ontology:constraint_victim(gendered_category_membership__biological_sex_reading, non_binary_people).
narrative_ontology:constraint_victim(gendered_category_membership__biological_sex_reading, intersex_people).
narrative_ontology:constraint_vindicates(gendered_category_membership__biological_sex_reading, sex_binary_immutability).
narrative_ontology:constraint_vindicates(gendered_category_membership__biological_sex_reading, sex_based_protection_necessity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Positioned as the primary beneficiaries of sex-segregated spaces (prisons, shelters, sports, changing rooms). They organize politically to maintain biological sex as the legal criterion for 'woman' category, arguing that gender identity inclusion threatens their safety, fairness, and hard-won sex-based rights. Exit from this position means abandoning a core political identity and advocacy infrastructure.
narrative_ontology:constraint_stakeholder(gendered_category_membership__biological_sex_reading, cisgender_women, beneficiary,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(gendered_category_membership__biological_sex_reading, cisgender_women, agenda_setter).

% Excluded from 'woman' category and sex-segregated spaces aligned with their gender. Bear costs of legal exclusion, denial of appropriate facilities, heightened violence risk in male-designated spaces, and systematic identity invalidation. Cannot exit the constraint without detransition (which carries its own severe costs) because gender identity is experienced as immutable and the constraint denies its validity.
narrative_ontology:constraint_stakeholder(gendered_category_membership__biological_sex_reading, transgender_women, payer,
    powerless, biographical, identity_locked, national).

% Erased by the binary biological sex framework entirely. Neither male nor female category fits; legal recognition is denied or requires misgendering. Bear costs of administrative invisibility, healthcare barriers, and identity invalidation. Exit is identity-locked: the constraint denies the existence of their category.
narrative_ontology:constraint_stakeholder(gendered_category_membership__biological_sex_reading, non_binary_people, payer,
    powerless, biographical, identity_locked, national).

% Biological sex markers (chromosomes, anatomy) do not cleanly fit the binary the constraint asserts. Historically subjected to non-consensual medical interventions to fit the binary. The constraint's claim of 'immutable biological markers' directly contradicts their embodied reality. Exit options are minimal: legal systems rarely accommodate intersex variations.
narrative_ontology:constraint_stakeholder(gendered_category_membership__biological_sex_reading, intersex_people, payer,
    powerless, biographical, trapped, national).

% Organizations and legal actors who lobby, litigate, and draft legislation to enshrine biological sex as the sole criterion for gender category. They control the policy agenda in multiple jurisdictions. Have institutional power and resources; can shift strategies across forums (courts, legislatures, international bodies).
narrative_ontology:constraint_stakeholder(gendered_category_membership__biological_sex_reading, sex_based_rights_advocates, agenda_setter,
    institutional, generational, arbitrage, national).

% Advocate for gender identity as the basis for category membership. Structurally excluded from the biological-sex framework's definition of legitimacy; their testimony is dismissed as 'ideology' in bio-sex legislative hearings. They operate parallel advocacy infrastructures and have achieved legal recognition in many jurisdictions.
narrative_ontology:constraint_stakeholder(gendered_category_membership__biological_sex_reading, gender_identity_advocates, excluded,
    organized, generational, mobile, national).

% Feminist theorists and activists who ground 'woman' in sustained social role/oppression rather than biology. Excluded from both the bio-sex and gender-identity framings as primary reference points. They argue both readings miss the material reality of gendered socialization and structural oppression.
narrative_ontology:constraint_stakeholder(gendered_category_membership__biological_sex_reading, social_role_feminists, excluded,
    moderate, biographical, mobile, national).

% State actors who enact and adjudicate the legal definition of 'sex' and 'gender'. They hold enforcement power (laws, court orders, administrative rules). Their decisions determine which reading governs access to sex-segregated spaces, identity documents, and anti-discrimination protections. They can switch between readings via legislation or precedent.
narrative_ontology:constraint_stakeholder(gendered_category_membership__biological_sex_reading, legislators_courts, agenda_setter,
    institutional, generational, arbitrage, national).

% Scholars in philosophy, sociology, law, biology who analyze the constraint from outside the partisan contest. They map the structural relationships, track the empirical effects of different legal regimes, and evaluate the coherence of each reading's claims. No material stake in the outcome.
narrative_ontology:constraint_stakeholder(gendered_category_membership__biological_sex_reading, analytical_observers, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a bright-line, administratively simple rule for allocating access to sex-segregated spaces (prisons, shelters, sports, changing rooms, single-sex services) and for collecting sex-disaggregated data. The binary biological criterion is claimed to be objective, verifiable, and stable — solving the coordination problem of 'who counts as a woman' without subjective assessment.
% TRANSFER_FUNCTION: Moves access to woman-only spaces, resources, legal protections, and social recognition from transgender women, non-binary people, and intersex people to cisgender women. Transfers the burden of proof: under this rule, trans women must prove they are 'really male' to be excluded; under identity rules, the burden shifts. Transfers epistemic authority from self-knowledge to medical/legal verification of biology.
% ABSENT_VOICES: Transgender women, non-binary people, and intersex people are structurally excluded from the legislative and judicial forums where bio-sex definitions are codified. Their testimony is often excluded from hearings on 'women's rights' bills, or presented only as 'opposing views' rather than as directly affected parties. Intersex people are almost never consulted on binary-sex legislation that erases their existence.
% DISAPPEARANCE_RATIONALE: If the biological-sex rule vanished overnight, sex-segregated spaces would immediately face the coordination problem it claims to solve: who qualifies for access? Prisons, shelters, sports leagues, and data systems would need new eligibility criteria. Legal protections framed in terms of 'sex' would become ambiguous. The world would reorganize around either gender identity, social role, or a patchwork of context-specific rules — a substantial rearrangement.
% FOUNDING_PROBLEM: The need for a clear, stable, and objectively verifiable criterion to define the category 'woman' for the purpose of sex-based rights, protections, and single-sex spaces — rights won by the feminist movement on the basis of female biology (reproductive vulnerability, physical dimorphism, historical subordination). The biological sex reading claims this problem remains live because gender identity is subjective and unverifiable.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem's liveness is attested by sex-based rights organizations (e.g., Women's Declaration International, Fair Play For Women) and some UN human rights mechanisms that recognize sex-based protections. It is contested by trans rights organizations (e.g., ILGA World, Transgender Europe), medical associations (WPATH, AMA), and many feminist groups who argue the problem is solved by gender identity recognition. No neutral arbiter corroborates either side; the dispute is the live political conflict.
narrative_ontology:disappearance_verdict(gendered_category_membership__biological_sex_reading, world_rearranges).
narrative_ontology:founding_problem_status(gendered_category_membership__biological_sex_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gendered_category_membership__biological_sex_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(gendered_category_membership__biological_sex_reading, 'none', 1).
narrative_ontology:epsilon_provenance(gendered_category_membership__biological_sex_reading, 0.78, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gendered_category_membership__biological_sex_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(gendered_category_membership__biological_sex_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(gendered_category_membership__biological_sex_reading, ExtMetricName, E),
    domain_priors:suppression_score(gendered_category_membership__biological_sex_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(gendered_category_membership__biological_sex_reading),
    narrative_ontology:constraint_metric(gendered_category_membership__biological_sex_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(gendered_category_membership__biological_sex_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(gendered_category_membership__biological_sex_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.78) is high because the constraint transfers category membership and its material benefits (safe spaces, fair competition, legal recognition) from trans/non-binary/intersex people to cis women, while denying the former any pathway to inclusion. Suppression (0.85) is very high because the constraint's persistence depends on legal bans on gender recognition, exclusion from facilities, censorship of trans-affirming care, and the criminalization of 'misgendering' in some jurisdictions — active coercion, not passive coordination. Theater ratio (0.32) is moderate: the coordination function (bright-line rule for sex-segregated spaces) is real but increasingly overshadowed by performative 'protection' rhetoric that does not withstand scrutiny (e.g., no evidence of trans women threatening cis women in shelters). Accessibility collapse (0.68) is substantial: once the biological binary is accepted as the only legitimate criterion, alternative frameworks (identity, social role) are treated as incoherent or dangerous. Resistance (0.72) is high: trans people, allies, medical bodies, courts, and international human rights mechanisms actively contest the constraint.
 *
 * PERSPECTIVAL GAP:
 *   From the bio-sex reading's seat (agenda_setter/beneficiary), the constraint appears as a Mountain: a natural fact that organizes reality. From the trans woman's seat (identity_locked payer), it is a Snare: pure extraction enforced by law, suppressing her existence. From the non-binary person's seat, it is a Mountain of erasure: a natural law that says they do not exist. From the legislator's seat, it is a Tangled Rope: they must coordinate competing rights claims but the bio-sex rule gives them a simple (if brutal) decision procedure. The engine computes these divergent seat types from the single structural dataset — the claim/metric independence is essential here. The reading's self-description as Mountain is the False Summit; the metrics reveal the extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   The biological sex reading claims cis women are beneficiaries (d → 0.0) because the rule 'protects' their spaces. But structurally, cis women are also agenda_setters who wield institutional power to enforce the rule — their directionality is complex. Trans women are identity-locked payers (d → 1.0): the constraint denies their identity, and exit requires detransition (existentially costly). Non-binary and intersex people are trapped (d = 1.0): the binary erases them entirely. Sex-based rights advocates are institutional agenda_setters with arbitrage exit (d → 0.0). Gender identity advocates and social role feminists are excluded organized actors with mobile exit — they operate parallel frameworks. Legislators/courts are institutional agenda_setters with arbitrage exit (they can choose which reading to enforce). Analytical observers sit at d = 0.5 (symmetric). The engine will compute per-seat χ from these structural positions.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (clear criterion for sex-based rights) was live in 1970s feminism. The biological sex reading claims it remains live because gender identity is 'subjective.' But the constraint has outlived its coordination function: sex-segregated spaces can be (and are) administered via gender identity with safeguards; sports can use hormone thresholds; prisons use individualized risk assessment. The mandate has atrophied into a vehicle for excluding trans people — the extraction (exclusion) persists while the coordination justification thins. This is mandatrophy: the original problem (male violence against women, unfair competition) is not solved by excluding trans women (who are not the primary perpetrators), but the arrangement persists because it now serves a different function (boundary maintenance for a political coalition). The 'protection' narrative is the theatrical maintenance of a degraded function.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_constructed_boundary,
    'Is the binary biological sex criterion a genuine natural law (Mountain) or a constructed boundary that serves political interests (False Summit)?',
    'Examine whether the constraint''s operation matches Mountain metrics (negligible extraction, no enforcement needed, no victims) or whether the high extraction/suppression/enforcement profile reveals a constructed constraint. Track whether ''biological sex'' remains stable as a legal criterion when intersex variations, hormonal conditions, and medical transitions are accounted for.',
    'If natural law: the constraint is a genuine Mountain; beneficiaries/victims are misidentified. If constructed: False Summit Mountain → reclassifies to Tangled Rope or Snare via FSM signature. The beneficiary claim (cis women protected) would be exposed as extraction cover.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_boundary, conceptual, 'FSM candidate: Mountain with declared beneficiaries and high extraction/suppression. The natural-law claim is the core ambiguity.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression (0.85) primarily structural (legal bans, facility exclusion, medical gatekeeping) or does it include a substantial internalized component (trans people internalizing invalidation, cis women internalizing fear)?',
    'Longitudinal study of trans people''s psychological distress in jurisdictions that switch from identity-recognition to bio-sex rules. If distress persists after legal barriers are removed (internalized), suppression is higher than structural measure. If distress tracks legal regime (structural), suppression is accurately measured.',
    'If internalized suppression is substantial, the constraint''s effective suppression exceeds the structural measure — targets carry the suppression after legal exit. This would increase χ for identity-locked payers beyond the engine''s structural calculation.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression in interpersonal/identity constraints.').

omega_variable(
    cis_women_as_beneficiaries_or_cover,
    'Are cisgender women genuine net beneficiaries of the biological sex rule, or is their beneficiary status a political cover story for anti-trans extraction?',
    'Comparative analysis of cis women''s outcomes (safety, sports participation, shelter access) in jurisdictions with bio-sex rules vs. gender identity rules. If outcomes are equivalent or better under identity rules, the beneficiary claim is falsified. Track whether bio-sex advocacy organizations also oppose other women''s rights (reproductive rights, LGBTQ+ rights) — suggesting coalition politics over material benefit.',
    'If cis women are not genuine beneficiaries, the constraint has NO concentrated beneficiary — it extracts from trans/non-binary/intersex people without a clear recipient. This would reclassify toward Piton (diffuse extraction, no capturer) or Snare (extraction for its own sake). The FSM override would target Snare, not Tangled Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cis_women_as_beneficiaries_or_cover, empirical, 'Whether the declared beneficiary group actually receives net benefit or is instrumentally positioned.').

omega_variable(
    kernel_committer_structure,
    'How does the kernel-reading structure (three mutually exclusive universal criteria) shape the constraint''s classification dynamics?',
    'Analyze whether the foreclosure relations among readings create a structural dynamic where each reading''s enforcement intensity is driven by the threat of the others'' legitimacy. Model the kernel as a three-way contest where each reading''s ε is inflated by the need to foreclose alternatives.',
    'If foreclosure pressure drives extraction, the high ε is not intrinsic to the bio-sex rule but relational — it extracts BECAUSE it must defeat identity and social role readings. This would link the three constraint stories in a contamination network where each reading''s classification depends on the others'' status.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_committer_structure, conceptual, 'Committer-frame structural delta: foreclosure relations among kernel readings as extraction driver.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gendered_category_membership__biological_sex_reading, 2010, 2030).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gcm_bio_sex_tr_t2010, gendered_category_membership__biological_sex_reading, theater_ratio, 2010, 0.12).
narrative_ontology:measurement(gcm_bio_sex_tr_t2014, gendered_category_membership__biological_sex_reading, theater_ratio, 2014, 0.18).
narrative_ontology:measurement(gcm_bio_sex_tr_t2018, gendered_category_membership__biological_sex_reading, theater_ratio, 2018, 0.24).
narrative_ontology:measurement(gcm_bio_sex_tr_t2022, gendered_category_membership__biological_sex_reading, theater_ratio, 2022, 0.28).
narrative_ontology:measurement(gcm_bio_sex_tr_t2026, gendered_category_membership__biological_sex_reading, theater_ratio, 2026, 0.3).
narrative_ontology:measurement(gcm_bio_sex_tr_t2030, gendered_category_membership__biological_sex_reading, theater_ratio, 2030, 0.32).

% Extraction over time
narrative_ontology:measurement(gcm_bio_sex_be_t2010, gendered_category_membership__biological_sex_reading, base_extractiveness, 2010, 0.45).
narrative_ontology:measurement(gcm_bio_sex_be_t2014, gendered_category_membership__biological_sex_reading, base_extractiveness, 2014, 0.52).
narrative_ontology:measurement(gcm_bio_sex_be_t2018, gendered_category_membership__biological_sex_reading, base_extractiveness, 2018, 0.63).
narrative_ontology:measurement(gcm_bio_sex_be_t2022, gendered_category_membership__biological_sex_reading, base_extractiveness, 2022, 0.71).
narrative_ontology:measurement(gcm_bio_sex_be_t2026, gendered_category_membership__biological_sex_reading, base_extractiveness, 2026, 0.76).
narrative_ontology:measurement(gcm_bio_sex_be_t2030, gendered_category_membership__biological_sex_reading, base_extractiveness, 2030, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(gcm_bio_sex_su_t2010, gendered_category_membership__biological_sex_reading, suppression_requirement, 2010, 0.55).
narrative_ontology:measurement(gcm_bio_sex_su_t2014, gendered_category_membership__biological_sex_reading, suppression_requirement, 2014, 0.62).
narrative_ontology:measurement(gcm_bio_sex_su_t2018, gendered_category_membership__biological_sex_reading, suppression_requirement, 2018, 0.71).
narrative_ontology:measurement(gcm_bio_sex_su_t2022, gendered_category_membership__biological_sex_reading, suppression_requirement, 2022, 0.78).
narrative_ontology:measurement(gcm_bio_sex_su_t2026, gendered_category_membership__biological_sex_reading, suppression_requirement, 2026, 0.82).
narrative_ontology:measurement(gcm_bio_sex_su_t2030, gendered_category_membership__biological_sex_reading, suppression_requirement, 2030, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gendered_category_membership__biological_sex_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(gendered_category_membership__biological_sex_reading, 0.12).
narrative_ontology:affects_constraint(gendered_category_membership__biological_sex_reading, gendered_category_membership__gender_identity_reading).
narrative_ontology:affects_constraint(gendered_category_membership__biological_sex_reading, gendered_category_membership__social_role_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the kernel 'gendered_category_membership'. The kernel decomposes into: biological_sex_reading (this story), gender_identity_reading, and social_role_reading. Each has distinct ε, beneficiaries, victims, and claimed_type. They are linked by foreclosure relations: this reading forecloses both siblings (biology-as-sole-criterion logically excludes identity and social role as criteria). The upstream constraint (biological sex as natural kind) influences downstream constraints (sex-segregated space allocation, identity document policy, anti-discrimination law).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(gendered_category_membership__biological_sex_reading, organized, 0.15).
constraint_indexing:directionality_override(gendered_category_membership__biological_sex_reading, powerless, 0.95).
constraint_indexing:directionality_override(gendered_category_membership__biological_sex_reading, institutional, 0.1).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
