% ============================================================================
% CONSTRAINT STORY: woman_category__intersex_accommodation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
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
 *   constraint_id: woman_category__intersex_accommodation_reading
 *   human_readable: Woman Category — Intersex Accommodation Reading
 *   domain: political_philosophy/law/social_policy/bioethics
 *
 * SUMMARY:
 *   This constraint story instantiates the intersex_accommodation_reading of
 *   the woman_category kernel. It holds that 'woman' includes typical female
 *   biology plus intersex variations that do not fit the male category — a
 *   non-binary spectrum view of biological sex. The reading emerged from
 *   intersex human rights advocacy (1990s–present) and has been partially
 *   codified in UN treaty body interpretations, Malta's 2015 GIGESC Act, and
 *   clinical guideline shifts (Chicago Consensus 2006, global updates 2016+).
 *   Its extraction profile is domain-dependent: near-zero in most
 *   civil/administrative law (small population, accommodation is low-cost)
 *   but rises to ~0.55 in elite sports where the Semenya/DSD boundary case
 *   creates a measurable transfer from typical-female athletes to intersex
 *   athletes. The reading challenges binary enforcement in both sibling
 *   readings: it rejects the sex_biology_reading's chromosomal essentialism
 *   and the gender_identity_reading's decoupling of category from biology
 *   entirely.
 *
 * KEY AGENTS:
 *   - intersex_individuals: Primary beneficiary (powerless/identity_locked) — gains legal recognition and bodily autonomy
 *   - elite_female_athletes_dsd: Primary payer/victim in sports domain (moderate/constrained) — bears competitive fairness cost at performance-advantage boundary
 *   - inclusive_policy_advocates: Agenda setter (organized/mobile) — authors and advances the framing
 *   - human_rights_institutions: Agenda setter/beneficiary (institutional/arbitrage) — codifies the reading into binding norms
 *   - sex_biology_advocates: Excluded (organized/mobile) — hold competing binary-biology reading
 *   - gender_identity_advocates: Excluded (organized/mobile) — hold competing self-ID reading
 *   - sports_governing_bodies: Observer (institutional/analytical) — implements contested eligibility rules
 *   - medical_profession_clinicians: Observer (organized/analytical) — translates reading into clinical practice
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(woman_category__intersex_accommodation_reading, 0.18).
domain_priors:suppression_score(woman_category__intersex_accommodation_reading, 0.22).
domain_priors:theater_ratio(woman_category__intersex_accommodation_reading, 0.12).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(woman_category__intersex_accommodation_reading, extractiveness, 0.18).
narrative_ontology:constraint_metric(woman_category__intersex_accommodation_reading, suppression_requirement, 0.22).
narrative_ontology:constraint_metric(woman_category__intersex_accommodation_reading, theater_ratio, 0.12).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(woman_category__intersex_accommodation_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(woman_category__intersex_accommodation_reading, resistance, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(woman_category__intersex_accommodation_reading, rope).
narrative_ontology:human_readable(woman_category__intersex_accommodation_reading, "Woman Category — Intersex Accommodation Reading").
narrative_ontology:topic_domain(woman_category__intersex_accommodation_reading, "political_philosophy/law/social_policy/bioethics").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(woman_category__intersex_accommodation_reading, '350d58d3-f8d5-4178-8979-3b90b2ab4687').
narrative_ontology:cs_kernel_codification('350d58d3-f8d5-4178-8979-3b90b2ab4687', distributed).
narrative_ontology:cs_authority_grounding('350d58d3-f8d5-4178-8979-3b90b2ab4687', lineage).
narrative_ontology:cs_interpretation_layer_present('350d58d3-f8d5-4178-8979-3b90b2ab4687').
narrative_ontology:cs_reading_relation('350d58d3-f8d5-4178-8979-3b90b2ab4687', woman_category__sex_biology_reading, coexists_with).
narrative_ontology:cs_reading_relation('350d58d3-f8d5-4178-8979-3b90b2ab4687', woman_category__gender_identity_reading, coexists_with).
narrative_ontology:cs_axiom('350d58d3-f8d5-4178-8979-3b90b2ab4687', foundational, biological_sex_is_spectrum_not_binary).
narrative_ontology:cs_axiom_status(biological_sex_is_spectrum_not_binary, holdable).
narrative_ontology:cs_axiom_grounding('350d58d3-f8d5-4178-8979-3b90b2ab4687', biological_sex_is_spectrum_not_binary, empirically_contingent).
narrative_ontology:cs_axiom('350d58d3-f8d5-4178-8979-3b90b2ab4687', foundational, intersex_variations_are_natural_human_diversity_not_pathology).
narrative_ontology:cs_axiom_status(intersex_variations_are_natural_human_diversity_not_pathology, holdable).
narrative_ontology:cs_axiom_grounding('350d58d3-f8d5-4178-8979-3b90b2ab4687', intersex_variations_are_natural_human_diversity_not_pathology, deontological).
narrative_ontology:cs_reference_frame('350d58d3-f8d5-4178-8979-3b90b2ab4687', intersex_human_rights_framework_post_2013_un_report).
narrative_ontology:cs_drift_state('350d58d3-f8d5-4178-8979-3b90b2ab4687', contemporary_2026, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('350d58d3-f8d5-4178-8979-3b90b2ab4687', '').
narrative_ontology:cs_kernel_id(woman_category__intersex_accommodation_reading, woman_category).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(woman_category__intersex_accommodation_reading, intersex_individuals).
narrative_ontology:constraint_beneficiary(woman_category__intersex_accommodation_reading, inclusive_policy_advocates).
narrative_ontology:constraint_beneficiary(woman_category__intersex_accommodation_reading, human_rights_institutions).
narrative_ontology:constraint_victim(woman_category__intersex_accommodation_reading, elite_female_athletes_dsd).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Have innate variations in sex characteristics (chromosomal, gonadal, hormonal, anatomical) that do not fit typical male/female binary. This reading acknowledges their biology as part of the spectrum of 'woman' where female-typical or ambiguous, enabling legal recognition and anti-discrimination protection without forced medicalization. Exit from the category is identity-locked: their embodied biology and self-understanding are inseparable from the constraint's recognition.
narrative_ontology:constraint_stakeholder(woman_category__intersex_accommodation_reading, intersex_individuals, beneficiary,
    powerless, biographical, identity_locked, global).

% Competitors in the female category in elite sport who are directly affected when intersex athletes with high endogenous testosterone (e.g., 5-ARD) are included without restriction. They experience the constraint as extracting competitive fairness — the performance-advantage boundary case (Semenya line) makes ε high in this domain. Their exit from elite sport is constrained by career investment and lack of alternative competitive structures at that level.
narrative_ontology:constraint_stakeholder(woman_category__intersex_accommodation_reading, elite_female_athletes_dsd, payer,
    moderate, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(woman_category__intersex_accommodation_reading, elite_female_athletes_dsd, payer).

% Civil society organizations, legal scholars, and bioethicists who advance the intersex-accommodation framing in law, human rights treaties, and medical guidelines. They author the constraint's operational definition and lobby for its adoption. Their exit is mobile: they can shift advocacy strategies or coalition partners.
narrative_ontology:constraint_stakeholder(woman_category__intersex_accommodation_reading, inclusive_policy_advocates, agenda_setter,
    organized, generational, mobile, global).

% UN treaty bodies, regional human rights courts, and national human rights institutions that codify intersex protections and non-binary sex recognition. They benefit from the constraint's legitimacy as a human rights norm. Their exit is arbitrage-grade: they hold interpretive authority across multiple frameworks and can pivot to adjacent norms.
narrative_ontology:constraint_stakeholder(woman_category__intersex_accommodation_reading, human_rights_institutions, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(woman_category__intersex_accommodation_reading, human_rights_institutions, beneficiary).

% Feminist and legal actors who hold the sex_biology_reading: 'woman' = adult human female defined by reproductive biology. They are excluded from the intersex-accommodation framing's policy venues because their binary boundary is treated as exclusionary. They would object that the reading erases the material basis of sex-based oppression. Their exit is mobile: they build parallel legal/political infrastructures.
narrative_ontology:constraint_stakeholder(woman_category__intersex_accommodation_reading, sex_biology_advocates, excluded,
    organized, biographical, mobile, global).

% Trans-rights organizations and gender-affirming clinicians who hold the gender_identity_reading: 'woman' = person who identifies as a woman. They are partially aligned on anti-discrimination but structurally excluded from the intersex-accommodation reading's biological grounding, which treats gender identity as distinct from sex characteristics. They would object that the reading pathologizes or gatekeeps trans inclusion. Their exit is mobile: they operate in overlapping but distinct advocacy channels.
narrative_ontology:constraint_stakeholder(woman_category__intersex_accommodation_reading, gender_identity_advocates, excluded,
    organized, biographical, mobile, global).

% World Athletics, IOC, and national federations that must set eligibility rules for the female category. They observe the intersex-accommodation reading's claims and the sex-biology and gender-identity counter-claims, then produce regulations (e.g., testosterone thresholds) that attempt to balance inclusion, fairness, and legal compliance. Their seat is analytical: they do not collect from the constraint but bear the implementation cost of its contested status.
narrative_ontology:constraint_stakeholder(woman_category__intersex_accommodation_reading, sports_governing_bodies, observer,
    institutional, generational, analytical, global).

% Endocrinologists, geneticists, pediatric surgeons, and bioethicists who manage intersex variations clinically. They observe the constraint's influence on clinical guidelines (e.g., Chicago Consensus shift toward deferring irreversible surgery). Their seat is analytical: they interpret the constraint's medical implications but do not set its political boundaries.
narrative_ontology:constraint_stakeholder(woman_category__intersex_accommodation_reading, medical_profession_clinicians, observer,
    organized, biographical, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single coherent category 'woman' that accommodates biological diversity without fragmenting into infinite sub-categories, enabling anti-discrimination law, data collection, and social recognition to operate on a stable but non-binary basis.
% TRANSFER_FUNCTION: Moves legal recognition, anti-discrimination protection, and medical autonomy from the default binary enforcement regime to intersex individuals. In elite sport, moves competitive opportunity from typical-female athletes to intersex athletes with performance-advantage variations — a narrow but high-stakes transfer.
% ABSENT_VOICES: Intersex infants and children subject to early irreversible surgeries (still routine in many jurisdictions) cannot consent to or contest the category that will shape their lives. Typical-female athletes outside elite sport (school, collegiate, recreational) are not represented in the sports-governance negotiation but bear downstream rule changes. Detransitioners and gender-critical feminists who see all non-binary sex categories as undermining sex-based rights are excluded from the human-rights venues where this reading is codified.
% DISAPPEARANCE_RATIONALE: If the intersex-accommodation reading vanished, legal frameworks would revert to binary sex classification (sex_biology_reading default in most jurisdictions) or to pure self-identification (gender_identity_reading). Intersex individuals would lose specific anti-discrimination protections for sex characteristics. Elite sports would adopt either strict chromosomal criteria or unrestricted self-ID — either way, the current accommodation boundary (e.g., testosterone thresholds for DSD athletes) would dissolve, reorganizing competitive structures and human rights jurisprudence.
% FOUNDING_PROBLEM: The historical binary legal/medical system forced intersex bodies into male or female categories through coercive assignment and often irreversible infant surgery, violating bodily autonomy and producing lifelong harm. The intersex-accommodation reading was built to solve this: to recognize biological sex as a spectrum and secure legal personhood without mandatory medicalization.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem is attested by intersex-led organizations (e.g., OII, InterACT) and UN human rights treaty bodies (CAT, CRC, CEDAW concluding observations) — sources outside the direct beneficiary set of policy advocates. The 'contested' status reflects that sex_biology_reading proponents argue the binary system already accommodates intersex via 'disorders of sex development' medical exemptions, while gender_identity_reading proponents argue the problem is solved by self-ID without biological anchoring. No single account holds consensus.
narrative_ontology:disappearance_verdict(woman_category__intersex_accommodation_reading, world_rearranges).
narrative_ontology:founding_problem_status(woman_category__intersex_accommodation_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(woman_category__intersex_accommodation_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(woman_category__intersex_accommodation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(woman_category__intersex_accommodation_reading, 0.18, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Base extractiveness (0.18) reflects the reading's domain variance: averaged across all policy domains it is low (small intersex population, accommodation costs are administrative), but the sports sub-domain spikes to ~0.55. Suppression (0.22) has fallen sharply from 1990 (0.65) as binary enforcement (forced surgery, legal invisibility) has been challenged — the reading's spread reduces structural suppression. Theater ratio (0.12) is low: the constraint's operational content (anti-discrimination law, clinical guidelines, sports eligibility) is mostly functional, not performative. Accessibility collapse (0.35) is moderate: alternatives (binary law, pure self-ID) remain live and contested. Resistance (0.48) is significant: both sibling readings actively contest this reading's boundary, and sports governance resists its accommodation logic.
 *
 * PERSPECTIVAL GAP:
 *   The intersex_individuals seat (powerless, identity_locked) experiences the constraint as a rare protective rope — it coordinates recognition without extraction. The elite_female_athletes_dsd seat (moderate, constrained) experiences it as a snare in the sports sub-domain — extraction is real, enforcement active, exit constrained. The human_rights_institutions seat (institutional, arbitrage) experiences it as a coordination accomplishment — they authored it and benefit from its legitimacy. The engine computes this per-seat divergence from the structural data; the claimed_type 'rope' reflects the dominant coordination function across most domains.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries: intersex_individuals (direct recognition/autonomy), inclusive_policy_advocates (movement success), human_rights_institutions (normative authority). Victims: elite_female_athletes_dsd (only in sports domain, where performance-advantage boundary creates measurable transfer). The directionality derivation assigns low d to beneficiaries (d~0.1), high d to the sports-domain victims (d~0.8), symmetric d to observers. The low aggregate ε reflects that the victim set is narrow and domain-specific, while the beneficiary set is the constraint's primary organizing population.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (coercive binary assignment and infant surgery) remains live in many jurisdictions — the constraint is not mandatrophic. However, in jurisdictions that have enacted strong protections (Malta, Portugal, Iceland, parts of Australia), the constraint's mandate is substantially fulfilled and persistence without adaptation risks piton drift. The 'contested' founding_problem_status captures this split: the arrangement persists globally but its original justification is partially achieved in leading jurisdictions.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sports_domain_epsilon_spike,
    'Does the elite-sports sub-domain (where ε ≈ 0.55 for the DSD boundary case) constitute a separate constraint that should be decomposed per ε-invariance, or is it a domain-conditioned manifestation of the same accommodation reading?',
    'Trace whether the sports eligibility rules (World Athletics DSD regulations) structurally derive from the intersex-accommodation reading''s logic or from a distinct ''fairness-in-female-sport'' coordination problem that merely references the reading. If the former, single constraint with domain-conditioned ε; if the latter, separate constraint story linked by network.affects_constraints.',
    'If decomposed, the sports constraint would carry its own claimed_type (likely tangled_rope or snare), its own stakeholders (athletes, federations, CAS), and its own ε — clarifying that the accommodation reading''s low aggregate ε does not mask a high-extraction sub-constraint.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sports_domain_epsilon_spike, conceptual, 'Whether the sports performance-advantage boundary case is a separate constraint or a domain manifestation of the accommodation reading.').

omega_variable(
    intersex_heterogeneity_coverage,
    'Does ''intersex variations that do not fit male category'' coherently cover all intersex people, or does the reading''s boundary (female-typical or ambiguous) exclude some intersex variations (e.g., 46,XY complete androgen insensitivity) that are phenotypically female but chromosomally male?',
    'Survey intersex-led organizations'' position statements on whether the accommodation reading''s boundary matches their lived taxonomy, and compare with clinical classification systems (DSD nosology).',
    'If the reading excludes some intersex people, its beneficiary set is narrower than claimed and its coordination function has a structural gap — the constraint would be a partial accommodation, not a spectrum recognition.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(intersex_heterogeneity_coverage, empirical, 'Whether the reading''s ''female-typical or ambiguous'' boundary coherently covers the intersex population it claims to serve.').

omega_variable(
    committer_structure_kernel_reading,
    'How does this reading''s structural relationship to the woman_category kernel differ from its siblings, and where is the disagreement located?',
    'Map the three readings'' divergent structural elements: victim sets (sex_biology: trans_women; gender_identity: gender_critical_women; intersex_accommodation: elite_female_athletes_dsd), coordination functions, and authority groundings. The disagreement is located in the boundary criterion: biology (chromosomes vs. spectrum vs. identity).',
    'Documents the committer-frame structure required by kernel-reading discipline: this reading instantiates one ε-invariant constraint; siblings are other constraints. The omega records the structural delta (victim set shift, ε profile) without folding the contest into this constraint''s classification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(committer_structure_kernel_reading, conceptual, 'Kernel-reading committer structure: this reading''s victim set (elite_female_athletes_dsd) and ε profile (low general, high sports) distinguish it from sex_biology_reading (victims = trans_women, ε low general) and gender_identity_reading (victims = gender_critical_women, ε moderate general).').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(woman_category__intersex_accommodation_reading, 1990, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(woma_tr_t1990, woman_category__intersex_accommodation_reading, theater_ratio, 1990, 0.05).
narrative_ontology:measurement(woma_tr_t2000, woman_category__intersex_accommodation_reading, theater_ratio, 2000, 0.07).
narrative_ontology:measurement(woma_tr_t2010, woman_category__intersex_accommodation_reading, theater_ratio, 2010, 0.09).
narrative_ontology:measurement(woma_tr_t2015, woman_category__intersex_accommodation_reading, theater_ratio, 2015, 0.1).
narrative_ontology:measurement(woma_tr_t2018, woman_category__intersex_accommodation_reading, theater_ratio, 2018, 0.11).
narrative_ontology:measurement(woma_tr_t2021, woman_category__intersex_accommodation_reading, theater_ratio, 2021, 0.11).
narrative_ontology:measurement(woma_tr_t2024, woman_category__intersex_accommodation_reading, theater_ratio, 2024, 0.12).
narrative_ontology:measurement(woma_tr_t2026, woman_category__intersex_accommodation_reading, theater_ratio, 2026, 0.12).

% Extraction over time
narrative_ontology:measurement(woma_be_t1990, woman_category__intersex_accommodation_reading, base_extractiveness, 1990, 0.05).
narrative_ontology:measurement(woma_be_t2000, woman_category__intersex_accommodation_reading, base_extractiveness, 2000, 0.08).
narrative_ontology:measurement(woma_be_t2010, woman_category__intersex_accommodation_reading, base_extractiveness, 2010, 0.12).
narrative_ontology:measurement(woma_be_t2015, woman_category__intersex_accommodation_reading, base_extractiveness, 2015, 0.15).
narrative_ontology:measurement(woma_be_t2018, woman_category__intersex_accommodation_reading, base_extractiveness, 2018, 0.16).
narrative_ontology:measurement(woma_be_t2021, woman_category__intersex_accommodation_reading, base_extractiveness, 2021, 0.17).
narrative_ontology:measurement(woma_be_t2024, woman_category__intersex_accommodation_reading, base_extractiveness, 2024, 0.18).
narrative_ontology:measurement(woma_be_t2026, woman_category__intersex_accommodation_reading, base_extractiveness, 2026, 0.18).

% Suppression requirement over time
narrative_ontology:measurement(woma_su_t1990, woman_category__intersex_accommodation_reading, suppression_requirement, 1990, 0.65).
narrative_ontology:measurement(woma_su_t2000, woman_category__intersex_accommodation_reading, suppression_requirement, 2000, 0.55).
narrative_ontology:measurement(woma_su_t2010, woman_category__intersex_accommodation_reading, suppression_requirement, 2010, 0.35).
narrative_ontology:measurement(woma_su_t2015, woman_category__intersex_accommodation_reading, suppression_requirement, 2015, 0.28).
narrative_ontology:measurement(woma_su_t2018, woman_category__intersex_accommodation_reading, suppression_requirement, 2018, 0.24).
narrative_ontology:measurement(woma_su_t2021, woman_category__intersex_accommodation_reading, suppression_requirement, 2021, 0.22).
narrative_ontology:measurement(woma_su_t2024, woman_category__intersex_accommodation_reading, suppression_requirement, 2024, 0.22).
narrative_ontology:measurement(woma_su_t2026, woman_category__intersex_accommodation_reading, suppression_requirement, 2026, 0.22).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(woman_category__intersex_accommodation_reading, identity_coordination).
narrative_ontology:affects_constraint(woman_category__intersex_accommodation_reading, woman_category__sex_biology_reading).
narrative_ontology:affects_constraint(woman_category__intersex_accommodation_reading, woman_category__gender_identity_reading).
narrative_ontology:affects_constraint(woman_category__intersex_accommodation_reading, elite_sport_female_category_eligibility).

% DUAL FORMULATION NOTE:
% The woman_category kernel decomposes into three constraint stories: sex_biology_reading (binary chromosomal boundary, victims = trans_women), gender_identity_reading (self-ID boundary, victims = gender_critical_women), intersex_accommodation_reading (spectrum boundary, victims = elite_female_athletes_dsd in sports). Each has distinct ε, victim set, and coordination function. Linked via affects_constraints because they compete for the same legal/policy adjudication surface — sports eligibility, anti-discrimination law, identity documents.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(woman_category__intersex_accommodation_reading, moderate, 0.78).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
