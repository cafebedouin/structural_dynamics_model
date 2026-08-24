% ============================================================================
% CONSTRAINT STORY: woman_category__sex_biology_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_woman_category__sex_biology_reading, []).

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
 *   constraint_id: woman_category__sex_biology_reading
 *   human_readable: Sex-Biology Category Boundary for 'Woman'
 *   domain: political_philosophy/law/social_policy/bioethics
 *
 * SUMMARY:
 *   This constraint story captures the 'sex biology reading' of the contested
 *   kernel 'woman_category' — the position that category membership is
 *   determined by chromosomal (XX), anatomical, and reproductive biology. It
 *   instantiates a specific constraint: the legal and policy boundary that
 *   defines 'woman' as adult human female with typical female biology. This
 *   reading operates as a tangled rope: it coordinates sex-segregated
 *   provisions (genuine coordination function) while extracting through
 *   categorical exclusion of transgender women and ambiguous positioning of
 *   intersex people (asymmetric extraction). The constraint requires active
 *   enforcement (legislative definitions, sports eligibility rules, prison
 *   placement policies) and its persistence depends on suppressing the
 *   alternative gender-identity-based reading. The ε referent is the standing
 *   arrangement of sex-based law and policy as this reading sees it — not the
 *   gender-identity alternative.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(woman_category__sex_biology_reading, 0.72).
domain_priors:suppression_score(woman_category__sex_biology_reading, 0.68).
domain_priors:theater_ratio(woman_category__sex_biology_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(woman_category__sex_biology_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(woman_category__sex_biology_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(woman_category__sex_biology_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(woman_category__sex_biology_reading, accessibility_collapse, 0.71).
narrative_ontology:constraint_metric(woman_category__sex_biology_reading, resistance, 0.73).

% --- Constraint claim ---
narrative_ontology:constraint_claim(woman_category__sex_biology_reading, tangled_rope).
narrative_ontology:human_readable(woman_category__sex_biology_reading, "Sex-Biology Category Boundary for 'Woman'").
narrative_ontology:topic_domain(woman_category__sex_biology_reading, "political_philosophy/law/social_policy/bioethics").

domain_priors:requires_active_enforcement(woman_category__sex_biology_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(woman_category__sex_biology_reading, 'a9352d49-7602-4710-a2bc-3abc505f96b5').
narrative_ontology:cs_kernel_codification('a9352d49-7602-4710-a2bc-3abc505f96b5', formalized).
narrative_ontology:cs_authority_grounding('a9352d49-7602-4710-a2bc-3abc505f96b5', extraction).
narrative_ontology:cs_interpretation_layer_present('a9352d49-7602-4710-a2bc-3abc505f96b5').
narrative_ontology:cs_reading_relation('a9352d49-7602-4710-a2bc-3abc505f96b5', woman_category__gender_identity_reading, forecloses).
narrative_ontology:cs_reading_relation('a9352d49-7602-4710-a2bc-3abc505f96b5', woman_category__intersex_accommodation_reading, influences).
narrative_ontology:cs_axiom('a9352d49-7602-4710-a2bc-3abc505f96b5', foundational, sex_based_category_membership).
narrative_ontology:cs_axiom_status(sex_based_category_membership, holdable).
narrative_ontology:cs_axiom_grounding('a9352d49-7602-4710-a2bc-3abc505f96b5', sex_based_category_membership, deontological).
narrative_ontology:cs_axiom('a9352d49-7602-4710-a2bc-3abc505f96b5', foundational, biological_sex_immutability).
narrative_ontology:cs_axiom_status(biological_sex_immutability, holdable).
narrative_ontology:cs_axiom_grounding('a9352d49-7602-4710-a2bc-3abc505f96b5', biological_sex_immutability, empirically_contingent).
narrative_ontology:cs_axiom('a9352d49-7602-4710-a2bc-3abc505f96b5', secondary, sex_segregated_provisions_require_binary_boundary).
narrative_ontology:cs_axiom_status(sex_segregated_provisions_require_binary_boundary, holdable).
narrative_ontology:cs_axiom_grounding('a9352d49-7602-4710-a2bc-3abc505f96b5', sex_segregated_provisions_require_binary_boundary, instrumental).
narrative_ontology:cs_reference_frame('a9352d49-7602-4710-a2bc-3abc505f96b5', sex_based_legal_category_framework).
narrative_ontology:cs_drift_state('a9352d49-7602-4710-a2bc-3abc505f96b5', contemporary_gender_identity_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('a9352d49-7602-4710-a2bc-3abc505f96b5', '').
narrative_ontology:cs_kernel_id(woman_category__sex_biology_reading, woman_category).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(woman_category__sex_biology_reading, cisgender_women_in_sex_segregated_spaces).
narrative_ontology:constraint_beneficiary(woman_category__sex_biology_reading, womens_sports_governing_bodies).
narrative_ontology:constraint_beneficiary(woman_category__sex_biology_reading, violence_against_women_data_collectors).
narrative_ontology:constraint_victim(woman_category__sex_biology_reading, transgender_women_excluded_from_protections).
narrative_ontology:constraint_victim(woman_category__sex_biology_reading, intersex_people_ambiguously_positioned).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(woman_category__sex_biology_reading, womens_rights_advocates_sex_based).
narrative_ontology:constraint_vindicates(woman_category__sex_biology_reading, biological_sex_as_immutable_category).
narrative_ontology:constraint_vindicates(woman_category__sex_biology_reading, sex_based_rights_framework).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Rely on sex-segregated spaces (shelters, prisons, changing rooms) and sex-based protections (Title IX, VAWA data collection) for safety and fair competition. The biology-based category secures these arrangements. Exit would mean losing guaranteed access to single-sex provisions; alternative frameworks (self-ID) are experienced as threatening the coherence of those provisions.
narrative_ontology:constraint_stakeholder(woman_category__sex_biology_reading, cisgender_women_in_sex_segregated_spaces, beneficiary,
    organized, biographical, constrained, national).

% Are categorically excluded from sex-segregated female spaces and protections under this reading. The constraint treats their gender identity as irrelevant to category membership. They bear the cost of exclusion: loss of legal recognition, barriers to healthcare, housing, and safety resources, and exposure to violence. Exit from the constraint's logic is identity-locked — the category boundary defines them out of existence as women regardless of transition, legal recognition, or lived reality.
narrative_ontology:constraint_stakeholder(woman_category__sex_biology_reading, transgender_women_excluded_from_protections, payer,
    moderate, biographical, identity_locked, national).

% Have chromosomal, anatomical, or reproductive variations that do not fit the XX/female-reproductive-anatomy typical case. This reading includes them ambiguously — sometimes as women, sometimes not, depending on which biological marker is prioritized. They bear the cost of incoherent categorization: forced surgical interventions, legal limbo, exclusion from both male and female provisions. Exit is trapped — no framework within this reading resolves their status; they are structurally invisible to the binary logic.
narrative_ontology:constraint_stakeholder(woman_category__sex_biology_reading, intersex_people_ambiguously_positioned, payer,
    powerless, biographical, trapped, national).

% Set eligibility rules for female competition categories. The biology-based reading provides a clear, enforceable boundary (testosterone thresholds, chromosomal screening) that legitimizes their regulatory authority. They benefit from the constraint's coordination function (fair competition) and its extraction function (exclusion of trans women athletes). Exit options are arbitrage-grade — they can shift between chromosomal, hormonal, or anatomical criteria as scientific and political winds change.
narrative_ontology:constraint_stakeholder(woman_category__sex_biology_reading, womens_sports_governing_bodies, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(woman_category__sex_biology_reading, womens_sports_governing_bodies, beneficiary).

% Enact and enforce laws defining 'woman' in statutes (VAWA, Title IX, prison placement, census). The biology-based reading offers a putatively objective, administrable definition that resists judicial expansion. They are constrained by constitutional challenges, federalism, and electoral politics — they cannot simply ignore court rulings or public opinion, but they control the legislative text.
narrative_ontology:constraint_stakeholder(woman_category__sex_biology_reading, legislators_policymakers, agenda_setter,
    institutional, generational, constrained, national).

% Argue that gender identity, not biology, should determine category membership. They are structurally excluded from the constraint's internal logic — the biology-based reading treats their core premise (identity determines category) as a category error. They can litigate, lobby, and shift public opinion, but they cannot participate in the constraint's operation on its own terms.
narrative_ontology:constraint_stakeholder(woman_category__sex_biology_reading, trans_rights_advocates, excluded,
    organized, biographical, constrained, national).

% Campaign to preserve sex-based category definitions in law and policy. They set the agenda for 'sex-not-gender' legislative strategies and benefit from the constraint's maintenance of sex-segregated provisions. Their exit is constrained — they are committed to a framework that is increasingly contested in courts and international human rights bodies.
narrative_ontology:constraint_stakeholder(woman_category__sex_biology_reading, womens_rights_advocates_sex_based, agenda_setter,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(woman_category__sex_biology_reading, womens_rights_advocates_sex_based, beneficiary).

% Analyze the constraint's coherence, its effects on different populations, and its compatibility with constitutional and human rights frameworks. They neither collect rents nor pay costs from the constraint's operation; they map its structural logic and document its consequences.
narrative_ontology:constraint_stakeholder(woman_category__sex_biology_reading, bioethics_legal_scholars, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a stable, administrable category boundary for sex-segregated provisions: single-sex spaces, women's sports categories, violence-against-women data collection, and sex-based anti-discrimination law. The biology-based definition coordinates expectations, enforcement, and resource allocation across institutions without requiring individualized assessment.
% TRANSFER_FUNCTION: Moves access to sex-segregated protections, competitive categories, and legal recognition from transgender women and ambiguously-positioned intersex people to cisgender women and the institutions that administer sex-segregated provisions. The transfer is mediated through categorical exclusion: the boundary itself is the mechanism.
% ABSENT_VOICES: Transgender women and intersex people are structurally excluded from the constraint's authoring logic — the biology-based definition treats their self-understanding and bodily reality as irrelevant to category membership. They would object to being defined out of the category 'woman' but have no seat at the table where the boundary is drawn. Their absence is not accidental; it is constitutive of the reading's coherence.
% DISAPPEARANCE_RATIONALE: If the biology-based category boundary vanished overnight, sex-segregated sports, shelters, prisons, and data collection would lose their definitional anchor. Institutions would face immediate pressure to adopt alternative criteria (self-ID, hormonal thresholds, case-by-case assessment). Legal protections framed in sex-based language would become unenforceable or would be reinterpreted. The world of sex-segregated provision would reorganize — not collapse, but rearrange around a new boundary.
% FOUNDING_PROBLEM: The need for a stable, objective, and administrable definition of 'woman' in law and policy to secure sex-segregated protections for female people against male intrusion, ensure fair competition in women's sports, and enable accurate data collection on violence against women. The biology-based reading presents itself as the solution to the problem of category instability and administrative unworkability.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem is attested by the constraint's beneficiaries (cisgender women's organizations, sports bodies) and by the legislative record of sex-based statutes. It is contested by transgender rights advocates, intersex advocacy organizations, human rights bodies (UN Special Rapporteurs, Council of Europe), and a growing number of legal scholars who argue the problem is misdiagnosed — that inclusion does not undermine protection, and that the 'administrability' claim masks exclusionary intent. No neutral arbiter has settled the dispute.
narrative_ontology:disappearance_verdict(woman_category__sex_biology_reading, world_rearranges).
narrative_ontology:founding_problem_status(woman_category__sex_biology_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(woman_category__sex_biology_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(woman_category__sex_biology_reading, 'none', 1).
narrative_ontology:epsilon_provenance(woman_category__sex_biology_reading, 0.72, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(woman_category__sex_biology_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(woman_category__sex_biology_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(woman_category__sex_biology_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.72) is high because the constraint concentrates the costs of categorical exclusion on transgender women (complete exclusion from sex-segregated protections) and intersex people (incoherent categorization) while the coordination benefits (stable category for sports, shelters, data) accrue to cisgender women and administering institutions. Suppression (0.68) reflects active legislative and regulatory enforcement: bills defining sex biologically, sports bans, prison placement rules, and the judicial effort to maintain the boundary against self-ID challenges. Theater ratio (0.38) captures the gap between the stated coordination rationale ('fairness,' 'safety,' 'data integrity') and the enforcement energy directed at a small population (trans women are ~0.5% of adults; intersex ~1.7% of births). Accessibility collapse (0.71) is high because the binary logic leaves no room for the alternative — once the biology-based frame is accepted, self-ID appears as category error, not policy choice. Resistance (0.73) is high from trans rights advocates, human rights bodies, medical associations, and courts in multiple jurisdictions.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter/beneficiary seats (sports bodies, legislators, sex-based feminists), the constraint appears as a necessary coordination mechanism — a rope that solves the problem of category instability. From the payer seats (trans women, intersex people), it appears as a snare — pure extraction enforced through categorical denial. The engine computes this divergence; the authored claim (tangled_rope) acknowledges both functions exist simultaneously.
 *
 * DIRECTIONALITY LOGIC:
 *   Cisgender women in sex-segregated spaces are beneficiaries (d ~0.2) — they collect the coordination benefits. Transgender women are full targets (d ~0.95) — identity-locked exclusion means they cannot exit the constraint's logic. Intersex people are trapped targets (d ~0.9) — the binary schema has no place for them. Sports governing bodies and legislators are agenda-setters with arbitrage/constrained exit — they administer the boundary and can shift its operationalization. Trans rights advocates are excluded (no directionality in the constraint's logic). The engine will compute per-seat χ from these structural positions.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (stable category for sex-segregated provisions) remains live in the sense that sex-segregated provisions still exist and need definitions. But the constraint's current operation — particularly the legislative energy directed at trans women in sports (where participation numbers are minuscule) and the refusal of intersex accommodation — suggests the mandate has expanded beyond its coordination function into identity-boundary enforcement. The mandatrophy is unresolved: the constraint persists because the coordination function provides cover for the extraction function, and no institutional actor has the incentive to decouple them.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is the sex_biology_reading a distinct constraint from the gender_identity_reading and intersex_accommodation_reading, or are they measurement variants of a single constraint?',
    'Apply the ε-invariance test: if changing the category criterion (biology vs identity vs spectrum) changes the extractiveness, suppression, and victim set, they are distinct constraints. The structural delta (victim set, ε in sports/VAW policy) confirms distinctness.',
    'Confirms this JSON should model one reading only, linked to siblings via network.affects_constraints and cs_structure.reading_relations. Prevents conflation of structurally distinct constraints under one label.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Commitment that this reading instantiates a distinct constraint with its own ε, stakeholder structure, and classification.').

omega_variable(
    intersex_inclusion_ambiguity,
    'Does the sex biology reading structurally include or exclude intersex people, and does the ambiguity itself function as extraction?',
    'Track legal and policy outcomes for intersex people under biology-based definitions: are they categorized as women, men, or neither? Does the ambiguity produce forced medicalization, legal limbo, or exclusion from both male and female provisions?',
    'If ambiguity functions as extraction (intersex people bear costs without coherent categorization), the constraint''s victim set is larger and its extraction higher than the ''trans women only'' framing suggests. Affects ε and victim declarations.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(intersex_inclusion_ambiguity, empirical, 'Whether the reading''s typical-case biology definition (XX, female reproductive anatomy) structurally marginalizes intersex variations as collateral damage or as a targeted exclusion.').

omega_variable(
    sports_performance_advantage_evidence,
    'Does the performance advantage framework in women''s sports rest on validated evidence of retained advantage post-transition, or on theoretical extrapolation from male puberty?',
    'Longitudinal studies of transgender women athletes on hormone therapy measuring actual performance metrics against cisgender women, controlling for training, funding, and selection effects.',
    'If evidence shows minimal retained advantage, the sports coordination function is largely pretextual and extraction is higher. If advantage is substantial, the coordination function has stronger empirical grounding, though the categorical exclusion may still be overbroad.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sports_performance_advantage_evidence, empirical, 'Empirical basis for the highest-ε domain of this constraint (sports eligibility).').

omega_variable(
    vaw_data_collection_necessity,
    'Is sex-based (biology-based) data collection on violence against women structurally necessary, or could gender-identity-inclusive data collection serve the same policy function?',
    'Compare policy outcomes in jurisdictions with biology-based vs identity-based VAW data collection: does the change affect resource allocation, prevalence tracking, or intervention design?',
    'If identity-inclusive collection works equally well, the VAW policy coordination function is not dependent on biology-based category, reducing the constraint''s claimed coordination scope and increasing its extractive proportion.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(vaw_data_collection_necessity, empirical, 'Whether the second major coordination domain (VAW policy) genuinely requires the biology-based boundary.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (legislative bans, sports eligibility rules, prison policies) or internalized (trans women and intersex people absorbing the category denial as personal failure)?',
    'Post-policy-change suppression trajectory: if suppression metrics persist after legal barriers are removed in a jurisdiction, reclassify as partially internalized.',
    'If internalized, effective suppression is higher than structural measure suggests — the constraint''s logic continues to operate through identity fusion after formal enforcement relaxes.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs internalized suppression mechanism in the constraint''s operation on transgender and intersex people.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(woman_category__sex_biology_reading, 2010, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(woman_category__sex_biology_reading_tr_t2010, woman_category__sex_biology_reading, theater_ratio, 2010, 0.25).
narrative_ontology:measurement(woman_category__sex_biology_reading_tr_t2013, woman_category__sex_biology_reading, theater_ratio, 2013, 0.28).
narrative_ontology:measurement(woman_category__sex_biology_reading_tr_t2016, woman_category__sex_biology_reading, theater_ratio, 2016, 0.31).
narrative_ontology:measurement(woman_category__sex_biology_reading_tr_t2019, woman_category__sex_biology_reading, theater_ratio, 2019, 0.34).
narrative_ontology:measurement(woman_category__sex_biology_reading_tr_t2022, woman_category__sex_biology_reading, theater_ratio, 2022, 0.37).
narrative_ontology:measurement(woman_category__sex_biology_reading_tr_t2025, woman_category__sex_biology_reading, theater_ratio, 2025, 0.38).

% Extraction over time
narrative_ontology:measurement(woman_category__sex_biology_reading_be_t2010, woman_category__sex_biology_reading, base_extractiveness, 2010, 0.45).
narrative_ontology:measurement(woman_category__sex_biology_reading_be_t2013, woman_category__sex_biology_reading, base_extractiveness, 2013, 0.48).
narrative_ontology:measurement(woman_category__sex_biology_reading_be_t2016, woman_category__sex_biology_reading, base_extractiveness, 2016, 0.55).
narrative_ontology:measurement(woman_category__sex_biology_reading_be_t2019, woman_category__sex_biology_reading, base_extractiveness, 2019, 0.63).
narrative_ontology:measurement(woman_category__sex_biology_reading_be_t2022, woman_category__sex_biology_reading, base_extractiveness, 2022, 0.69).
narrative_ontology:measurement(woman_category__sex_biology_reading_be_t2025, woman_category__sex_biology_reading, base_extractiveness, 2025, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(woman_category__sex_biology_reading_su_t2010, woman_category__sex_biology_reading, suppression_requirement, 2010, 0.52).
narrative_ontology:measurement(woman_category__sex_biology_reading_su_t2013, woman_category__sex_biology_reading, suppression_requirement, 2013, 0.55).
narrative_ontology:measurement(woman_category__sex_biology_reading_su_t2016, woman_category__sex_biology_reading, suppression_requirement, 2016, 0.6).
narrative_ontology:measurement(woman_category__sex_biology_reading_su_t2019, woman_category__sex_biology_reading, suppression_requirement, 2019, 0.64).
narrative_ontology:measurement(woman_category__sex_biology_reading_su_t2022, woman_category__sex_biology_reading, suppression_requirement, 2022, 0.67).
narrative_ontology:measurement(woman_category__sex_biology_reading_su_t2025, woman_category__sex_biology_reading, suppression_requirement, 2025, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(woman_category__sex_biology_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(woman_category__sex_biology_reading, 0.08).
narrative_ontology:affects_constraint(woman_category__sex_biology_reading, woman_category__gender_identity_reading).
narrative_ontology:affects_constraint(woman_category__sex_biology_reading, woman_category__intersex_accommodation_reading).
narrative_ontology:affects_constraint(woman_category__sex_biology_reading, sex_segregated_sports_eligibility).
narrative_ontology:affects_constraint(woman_category__sex_biology_reading, violence_against_women_data_collection).
narrative_ontology:affects_constraint(woman_category__sex_biology_reading, single_sex_space_access_policy).
narrative_ontology:affects_constraint(woman_category__sex_biology_reading, prison_placement_policy).

% DUAL FORMULATION NOTE:
% This constraint is the sex_biology_reading of the woman_category kernel. It decomposes the colloquial label 'definition of woman' into a structurally precise claim with its own ε (0.72), stakeholder structure, and tangled_rope classification. The gender_identity_reading and intersex_accommodation_reading are separate constraint stories with different ε values, victim sets, and claimed types. They are linked via affects_constraints and cs_structure.reading_relations. The ε-invariance principle requires this decomposition because the label 'woman category' conflates three distinct structural arrangements.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(woman_category__sex_biology_reading, institutional, 0.15).
constraint_indexing:directionality_override(woman_category__sex_biology_reading, moderate, 0.95).
constraint_indexing:directionality_override(woman_category__sex_biology_reading, powerless, 0.9).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
