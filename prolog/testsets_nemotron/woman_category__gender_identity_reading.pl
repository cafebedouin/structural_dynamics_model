% ============================================================================
% CONSTRAINT STORY: woman_category__gender_identity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-17
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_woman_category__gender_identity_reading, []).

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
 *   constraint_id: woman_category__gender_identity_reading
 *   human_readable: Woman Category — Gender Identity Reading
 *   domain: political/legal/social/bioethics
 *
 * SUMMARY:
 *   The gender-identity reading of 'woman' replaces biological sex with
 *   internal self-identification as the criterion for category membership
 *   across law, policy, sport, and language. It presents as a coordination
 *   solution for transgender dignity but operates as a tangled rope: genuine
 *   coordination (unified recognition standard, removal of medical
 *   gatekeeping) fused with asymmetric extraction (displacement of female
 *   people from their own category, erosion of sex-based protections,
 *   compulsion of speech and belief). The constraint requires active
 *   enforcement (policy mandates, institutional capture, speech suppression)
 *   to persist — without enforcement, the category reverts to sex-based
 *   membership. The claimed type is tangled_rope; the metrics reflect high
 *   extraction in high-stakes domains (sport, prisons, shelters) and moderate
 *   extraction in administrative domains (identity documents).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(woman_category__gender_identity_reading, 0.68).
domain_priors:suppression_score(woman_category__gender_identity_reading, 0.71).
domain_priors:theater_ratio(woman_category__gender_identity_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(woman_category__gender_identity_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(woman_category__gender_identity_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(woman_category__gender_identity_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(woman_category__gender_identity_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(woman_category__gender_identity_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(woman_category__gender_identity_reading, tangled_rope).
narrative_ontology:human_readable(woman_category__gender_identity_reading, "Woman Category — Gender Identity Reading").
narrative_ontology:topic_domain(woman_category__gender_identity_reading, "political/legal/social/bioethics").

domain_priors:requires_active_enforcement(woman_category__gender_identity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(woman_category__gender_identity_reading, '2c47bc8d-274a-44e4-a435-e39e427cd999').
narrative_ontology:cs_kernel_codification('2c47bc8d-274a-44e4-a435-e39e427cd999', distributed).
narrative_ontology:cs_authority_grounding('2c47bc8d-274a-44e4-a435-e39e427cd999', extraction).
narrative_ontology:cs_interpretation_layer_present('2c47bc8d-274a-44e4-a435-e39e427cd999').
narrative_ontology:cs_reading_relation('2c47bc8d-274a-44e4-a435-e39e427cd999', woman_category__sex_biology_reading, forecloses).
narrative_ontology:cs_reading_relation('2c47bc8d-274a-44e4-a435-e39e427cd999', woman_category__intersex_accommodation_reading, influences).
narrative_ontology:cs_axiom('2c47bc8d-274a-44e4-a435-e39e427cd999', foundational, gender_identity_determines_category_membership).
narrative_ontology:cs_axiom_status(gender_identity_determines_category_membership, holdable).
narrative_ontology:cs_axiom_grounding('2c47bc8d-274a-44e4-a435-e39e427cd999', gender_identity_determines_category_membership, deontological).
narrative_ontology:cs_axiom('2c47bc8d-274a-44e4-a435-e39e427cd999', foundational, transwomen_are_women).
narrative_ontology:cs_axiom_status(transwomen_are_women, holdable).
narrative_ontology:cs_axiom_grounding('2c47bc8d-274a-44e4-a435-e39e427cd999', transwomen_are_women, deontological).
narrative_ontology:cs_axiom('2c47bc8d-274a-44e4-a435-e39e427cd999', secondary, sex_based_protection_is_discriminatory).
narrative_ontology:cs_axiom_status(sex_based_protection_is_discriminatory, holdable).
narrative_ontology:cs_axiom_grounding('2c47bc8d-274a-44e4-a435-e39e427cd999', sex_based_protection_is_discriminatory, instrumental).
narrative_ontology:cs_reference_frame('2c47bc8d-274a-44e4-a435-e39e427cd999', pre_gatekeeping_trans_recognition).
narrative_ontology:cs_drift_state('2c47bc8d-274a-44e4-a435-e39e427cd999', contemporary_self_id_regime, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('2c47bc8d-274a-44e4-a435-e39e427cd999', '').
narrative_ontology:cs_kernel_id(woman_category__gender_identity_reading, woman_category).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(woman_category__gender_identity_reading, transgender_women).
narrative_ontology:constraint_beneficiary(woman_category__gender_identity_reading, nonbinary_people_identifying_as_women).
narrative_ontology:constraint_beneficiary(woman_category__gender_identity_reading, institutional_dei_bureaucracy).
narrative_ontology:constraint_beneficiary(woman_category__gender_identity_reading, gender_affirming_care_providers).
narrative_ontology:constraint_victim(woman_category__gender_identity_reading, female_athletes).
narrative_ontology:constraint_victim(woman_category__gender_identity_reading, female_prisoners).
narrative_ontology:constraint_victim(woman_category__gender_identity_reading, women_shelter_users).
narrative_ontology:constraint_victim(woman_category__gender_identity_reading, lesbian_community).
narrative_ontology:constraint_victim(woman_category__gender_identity_reading, sex_based_rights_advocates).
narrative_ontology:constraint_vindicates(woman_category__gender_identity_reading, gender_identity_determines_category_membership).
narrative_ontology:constraint_vindicates(woman_category__gender_identity_reading, transwomen_are_women).
narrative_ontology:constraint_vindicates(woman_category__gender_identity_reading, sex_based_protection_is_discriminatory).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Gain legal recognition and access to women's spaces, sports categories, and services based on self-declared gender identity. Their inclusion is the reading's central coordination achievement; exit from this category is structurally impossible because it would require renouncing their gender identity, which is constitutive of their self-concept.
narrative_ontology:constraint_stakeholder(woman_category__gender_identity_reading, transgender_women, beneficiary,
    organized, biographical, identity_locked, global).

% Access women's categories and protections under the same self-ID framework. Their inclusion extends the beneficiary set beyond binary transition but deepens the identity-lock dynamic — the category depends entirely on internal identification with no external verification.
narrative_ontology:constraint_stakeholder(woman_category__gender_identity_reading, nonbinary_people_identifying_as_women, beneficiary,
    moderate, biographical, identity_locked, global).

% Administers policy frameworks (Title IX guidance, corporate DEI mandates, government identity-document rules) that enforce the reading. Collects institutional legitimacy, funding, and career advancement from maintaining the framework. Can pivot to alternative frameworks if political winds shift — exit is arbitrage-grade.
narrative_ontology:constraint_stakeholder(woman_category__gender_identity_reading, institutional_dei_bureaucracy, agenda_setter,
    institutional, generational, arbitrage, global).

% Medical and therapeutic professionals whose practice model and reimbursement depend on the gender-identity framework. Benefit from expanded patient populations and insurance mandates. Have professional exit options (other specialties) but face reputational costs for dissent.
narrative_ontology:constraint_stakeholder(woman_category__gender_identity_reading, gender_affirming_care_providers, beneficiary,
    organized, biographical, mobile, national).

% Lose fair competition, scholarships, records, and roster spots when male-puberty advantages are admitted into female categories. Exit from elite sport is constrained by career investment and lack of alternative leagues; exit from the constraint requires accepting exclusion or quitting sport.
narrative_ontology:constraint_stakeholder(woman_category__gender_identity_reading, female_athletes, payer,
    moderate, biographical, constrained, global).

% Housed with male-bodied prisoners (including those with intact male anatomy and histories of sexual violence against women) under self-ID placement policies. No meaningful exit — cannot leave prison, cannot choose housing, and face retaliation for complaints.
narrative_ontology:constraint_stakeholder(woman_category__gender_identity_reading, female_prisoners, payer,
    powerless, biographical, trapped, national).

% Survivors of male violence who lose single-sex refuge when shelters admit male-bodied people on self-ID. Exit is trapped — they need shelter precisely when they have nowhere else to go, and the constraint removes the sex-segregated option that made refuge functional.
narrative_ontology:constraint_stakeholder(woman_category__gender_identity_reading, women_shelter_users, payer,
    powerless, immediate, trapped, national).

% Lose same-sex attraction as a coherent category when 'lesbian' is redefined to include male-bodied people who identify as women. Community spaces, dating pools, and political organization are restructured around identity rather than biology. Exit is constrained — they can leave organizations but not the conceptual erasure.
narrative_ontology:constraint_stakeholder(woman_category__gender_identity_reading, lesbian_community, payer,
    organized, biographical, constrained, global).

% Feminist and human-rights organizations arguing for sex-based protections. Excluded from policy consultation, deplatformed, labeled as hate groups for asserting that sex matters. Their exclusion is active — the constraint's enforcement machinery targets their speech and organizing.
narrative_ontology:constraint_stakeholder(woman_category__gender_identity_reading, sex_based_rights_advocates, excluded,
    organized, generational, constrained, global).

% Sees the full structure: a coordination function (dignity/recognition for trans people) fused with an extraction function (displacement of sex-based rights for females). The reading resolves the coordination problem for one group by imposing costs on another group that has no structural power to refuse.
narrative_ontology:constraint_stakeholder(woman_category__gender_identity_reading, analytical_observer, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single, administrable rule for category membership (self-declared gender identity) that resolves ambiguity for transgender and nonbinary people across identity documents, healthcare, sports, prisons, shelters, and language — replacing a patchwork of medical gatekeeping with a unified self-ID standard.
% TRANSFER_FUNCTION: Moves access rights (to women's sports, prisons, shelters, shortlists, awards, language) from the class 'adult human females' to the class 'anyone who identifies as a woman', transferring the costs of inclusion (lost fairness, safety, privacy, and coherence of the category 'woman') onto females as a class while the benefits of recognition and access accrue to transgender women and nonbinary people identifying as women.
% ABSENT_VOICES: Female prisoners and shelter users are the most trapped payers and are almost never consulted in policy design. Intersex people are invoked rhetorically by all three readings but rarely centered as agents. Detransitioners (who would attest to the reading's coercive dynamics) are structurally excluded from the conversation.
% DISAPPEARANCE_RATIONALE: If the self-ID reading vanished overnight, sports federations would revert to sex-based eligibility, prisons would resume sex-segregated housing, shelters would restore single-sex intake, and identity documents would require medical or legal process for sex-marker change. The world would rearrange around sex-based category membership — the reading is not a natural fact but an active policy regime.
% FOUNDING_PROBLEM: Transgender people faced medical gatekeeping, legal invisibility, and systemic discrimination when category membership required medical transition or surgical history. The self-ID reading was built to solve the problem of access to recognition without bodily modification as a precondition.
% FOUNDING_PROBLEM_CORROBORATION: Trans advocacy organizations attest the founding problem remains live (medical gatekeeping persists in many jurisdictions; nonbinary people still lack recognition). Sex-based rights advocates attest the founding problem has been substantially solved in Western jurisdictions (self-ID laws enacted, medical barriers reduced) and the reading now functions as expansion beyond its original justification. UK Cass Review and Swedish/U-turn policy changes corroborate the shifted-function reading from outside the beneficiary set.
narrative_ontology:disappearance_verdict(woman_category__gender_identity_reading, world_rearranges).
narrative_ontology:founding_problem_status(woman_category__gender_identity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(woman_category__gender_identity_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(woman_category__gender_identity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(woman_category__gender_identity_reading, 0.68, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(woman_category__gender_identity_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(woman_category__gender_identity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(woman_category__gender_identity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68) reflects the high-stakes transfer of female-only resources and protections to a male-bodied population. Suppression (0.71) captures the active enforcement needed: policy capture, deplatforming of dissent, compelled pronoun use, and legal penalties for 'misgendering'. Theater ratio (0.28) acknowledges real coordination function (dignity, administrative simplicity) while measuring the growing gap between stated purpose (inclusion) and actual operation (displacement). Accessibility collapse (0.45) is moderate — alternatives (sex-based categories) remain conceptually available and are defended by excluded voices, but institutional capture makes them practically inaccessible in many domains. Resistance (0.62) is high and rising — litigation, policy reversals (UK, Sweden, Finland), and public opinion shifts indicate the constraint meets organized opposition.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter/beneficiary seats, the constraint appears as rope (genuine coordination solving a real recognition problem). From the payer seats (female athletes, prisoners, shelter users), it appears as snare (pure extraction enforced by power). The engine computes this divergence from the structural data: same constraint, different effective extraction by seat. The identity-locked exit of transgender beneficiaries (cannot exit without renouncing identity) contrasts with the trapped exit of female prisoners (cannot exit the constraint's harm) — both are high-d but for opposite reasons.
 *
 * DIRECTIONALITY LOGIC:
 *   Transgender women and nonbinary people identifying as women are structural beneficiaries (d ≈ 0.15-0.2): the constraint subsidizes their access and recognition. Institutional DEI bureaucracy and gender-affirming-care providers are agenda-setters and secondary beneficiaries (d ≈ 0.1-0.3) who control and profit from the framework. Female athletes, prisoners, shelter users, and lesbians are payers (d ≈ 0.7-0.9) bearing concentrated costs with trapped or constrained exit. Sex-based-rights advocates are excluded (d ≈ 0.8) — their structural position is opposition, not participation. The analytical observer sits at d = 0.5 (symmetric).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (medical gatekeeping for trans recognition) has been substantially solved in many jurisdictions, yet the constraint expands (nonbinary inclusion, childhood transition, compelled speech). This is classic mandatrophy: the mandate outlives its function and becomes a vehicle for further extraction. The reading prevents mislabeling by naming both the coordination function (real) and the extraction function (real) — it is neither pure rope nor pure snare. The identity-locked exit of beneficiaries and trapped exit of payers create a pincer: neither side can walk away, so the constraint persists without either side's consent.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    committer_frame_identification,
    'This constraint is one reading (gender_identity_reading) of the contested kernel ''woman_category''. What structural changes would the sibling readings (sex_biology_reading, intersex_accommodation_reading) instantiate?',
    'Author separate constraint stories for each reading with their own ε, beneficiaries, victims, and metrics. Compare the victim sets: sex_biology_reading victims = transgender women (excluded from woman category); intersex_accommodation_reading victims = depends on boundary-drawing of ''typical female biology''.',
    'If sex_biology_reading has high ε for transgender women and low ε for females, the kernel contains a genuine structural conflict — not a measurement ambiguity. The ε-invariance principle requires separate stories.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(committer_frame_identification, conceptual, 'Commitment-system framing: kernel_id = woman_category, reading_id = gender_identity_reading. Sibling readings are separate constraints with distinct ε.').

omega_variable(
    coordination_extraction_separability,
    'Is the coordination function (unified recognition standard for trans people) structurally separable from the extraction function (displacement of sex-based rights)? Could a policy achieve the former without the latter?',
    'Examine jurisdictions with self-ID for identity documents but sex-based exceptions for sport/prisons/shelters (e.g., some US states, proposed UK amendments). If coordination persists without high-stakes extraction, the functions are separable and the high ε in sport/prisons is a policy choice, not a structural necessity of the reading.',
    'If separable, the tangled_rope classification is correct but the extraction is contingent — a scaffold-like reform could preserve coordination while removing extraction. If inseparable, the reading itself structurally requires the displacement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_extraction_separability, empirical, 'Whether the reading''s coordination and extraction components are structurally bound or policy-contingent.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression (0.71) primarily structural (legal mandates, institutional policies, deplatforming) or internalized (women self-censoring, believing they have no right to sex-segregated space, identity fusion with ''inclusivity'' as moral imperative)?',
    'Post-policy-reversal trajectories: if suppression persists in women''s self-reporting after legal mandates are removed (e.g., after UK Supreme Court ruling on sex definition), reclassify as partially internalized. Track detransitioner and desister accounts of internalized suppression.',
    'If internalized, effective suppression is higher than structural measure suggests — the constraint colonizes the target''s self-conception. This would increase χ for payer seats beyond the engine''s structural derivation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism in the gender-identity reading.').

omega_variable(
    intersex_rhetorical_use,
    'Does the intersex_accommodation_reading represent a genuine distinct structural position, or is intersex primarily a rhetorical device used by the gender_identity_reading to destabilize the sex binary?',
    'Analyze whether intersex advocacy organizations independently advance the accommodation_reading or are co-opted by gender-identity organizations. Examine if the accommodation_reading has distinct beneficiaries/victims/ε from the gender_identity_reading.',
    'If rhetorical device, the intersex_accommodation_reading is not a genuine sibling constraint — the kernel has only two structurally distinct readings. This affects network.affects_constraints and cs_structure.reading_relations.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(intersex_rhetorical_use, conceptual, 'Whether intersex_accommodation_reading is a genuine structural sibling or a rhetorical extension of gender_identity_reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(woman_category__gender_identity_reading, 2015, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(wcid_gir_tr_t2015, woman_category__gender_identity_reading, theater_ratio, 2015, 0.12).
narrative_ontology:measurement(wcid_gir_tr_t2017, woman_category__gender_identity_reading, theater_ratio, 2017, 0.18).
narrative_ontology:measurement(wcid_gir_tr_t2019, woman_category__gender_identity_reading, theater_ratio, 2019, 0.22).
narrative_ontology:measurement(wcid_gir_tr_t2021, woman_category__gender_identity_reading, theater_ratio, 2021, 0.26).
narrative_ontology:measurement(wcid_gir_tr_t2023, woman_category__gender_identity_reading, theater_ratio, 2023, 0.28).
narrative_ontology:measurement(wcid_gir_tr_t2025, woman_category__gender_identity_reading, theater_ratio, 2025, 0.28).

% Extraction over time
narrative_ontology:measurement(wcid_gir_be_t2015, woman_category__gender_identity_reading, base_extractiveness, 2015, 0.42).
narrative_ontology:measurement(wcid_gir_be_t2017, woman_category__gender_identity_reading, base_extractiveness, 2017, 0.51).
narrative_ontology:measurement(wcid_gir_be_t2019, woman_category__gender_identity_reading, base_extractiveness, 2019, 0.59).
narrative_ontology:measurement(wcid_gir_be_t2021, woman_category__gender_identity_reading, base_extractiveness, 2021, 0.65).
narrative_ontology:measurement(wcid_gir_be_t2023, woman_category__gender_identity_reading, base_extractiveness, 2023, 0.68).
narrative_ontology:measurement(wcid_gir_be_t2025, woman_category__gender_identity_reading, base_extractiveness, 2025, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(wcid_gir_su_t2015, woman_category__gender_identity_reading, suppression_requirement, 2015, 0.48).
narrative_ontology:measurement(wcid_gir_su_t2017, woman_category__gender_identity_reading, suppression_requirement, 2017, 0.56).
narrative_ontology:measurement(wcid_gir_su_t2019, woman_category__gender_identity_reading, suppression_requirement, 2019, 0.64).
narrative_ontology:measurement(wcid_gir_su_t2021, woman_category__gender_identity_reading, suppression_requirement, 2021, 0.7).
narrative_ontology:measurement(wcid_gir_su_t2023, woman_category__gender_identity_reading, suppression_requirement, 2023, 0.71).
narrative_ontology:measurement(wcid_gir_su_t2025, woman_category__gender_identity_reading, suppression_requirement, 2025, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(woman_category__gender_identity_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(woman_category__gender_identity_reading, 0.12).
narrative_ontology:affects_constraint(woman_category__gender_identity_reading, woman_category__sex_biology_reading).
narrative_ontology:affects_constraint(woman_category__gender_identity_reading, woman_category__intersex_accommodation_reading).
narrative_ontology:affects_constraint(woman_category__gender_identity_reading, sport_eligibility__female_category).
narrative_ontology:affects_constraint(woman_category__gender_identity_reading, prison_placement__sex_based).
narrative_ontology:affects_constraint(woman_category__gender_identity_reading, shelter_access__single_sex).

% DUAL FORMULATION NOTE:
% Woman_category kernel decomposes into three constraint stories with distinct ε and victim sets: gender_identity_reading (ε=0.68, victims=female class), sex_biology_reading (ε=0.35, victims=transgender women), intersex_accommodation_reading (ε=0.42, victims=depends on boundary). This story is the gender_identity_reading. The kernel's natural-language label 'woman' conflates three structurally distinct category-membership rules.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(woman_category__gender_identity_reading, powerless, 0.88).
constraint_indexing:directionality_override(woman_category__gender_identity_reading, moderate, 0.75).
constraint_indexing:directionality_override(woman_category__gender_identity_reading, organized, 0.2).
constraint_indexing:directionality_override(woman_category__gender_identity_reading, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
