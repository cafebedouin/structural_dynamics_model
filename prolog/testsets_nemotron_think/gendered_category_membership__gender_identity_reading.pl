% ============================================================================
% CONSTRAINT STORY: gendered_category_membership__gender_identity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_gendered_category_membership__gender_identity_reading, []).

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
 *   constraint_id: gendered_category_membership__gender_identity_reading
 *   human_readable: Gender Category Membership via Self-Declaration (Gender Identity Reading)
 *   domain: social_ontology/political_philosophy/bioethics
 *
 * SUMMARY:
 *   This constraint story captures the gender identity reading of the
 *   contested kernel 'gendered category membership.' The reading holds that
 *   gender categories (woman, man, non-binary) are constituted by subjective
 *   self-declaration rather than biological markers or social performance.
 *   Institutions adopting this reading replace sex-segregated spaces with
 *   gender-segregated ones, implement self-ID legal recognition, and treat
 *   resistance as exclusionary. The constraint exhibits tangled_rope
 *   structure: it coordinates genuine recognition for trans people
 *   (coordination function) while extracting from cis women who lose
 *   sex-based protections and from gender-critical feminists who face
 *   professional/social sanctions for dissent (asymmetric extraction). Active
 *   enforcement is required — self-ID policies depend on institutional
 *   suppression of sex-based advocacy, deplatforming of gender-critical
 *   voices, and legal compulsion of speech (pronouns, category admission).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gendered_category_membership__gender_identity_reading, 0.42).
domain_priors:suppression_score(gendered_category_membership__gender_identity_reading, 0.58).
domain_priors:theater_ratio(gendered_category_membership__gender_identity_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gendered_category_membership__gender_identity_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(gendered_category_membership__gender_identity_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(gendered_category_membership__gender_identity_reading, theater_ratio, 0.35).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gendered_category_membership__gender_identity_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(gendered_category_membership__gender_identity_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gendered_category_membership__gender_identity_reading, tangled_rope).
narrative_ontology:human_readable(gendered_category_membership__gender_identity_reading, "Gender Category Membership via Self-Declaration (Gender Identity Reading)").
narrative_ontology:topic_domain(gendered_category_membership__gender_identity_reading, "social_ontology/political_philosophy/bioethics").

domain_priors:requires_active_enforcement(gendered_category_membership__gender_identity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gendered_category_membership__gender_identity_reading, '46a1a76d-e487-4a21-be77-f55ad892c29d').
narrative_ontology:cs_kernel_codification('46a1a76d-e487-4a21-be77-f55ad892c29d', distributed).
narrative_ontology:cs_authority_grounding('46a1a76d-e487-4a21-be77-f55ad892c29d', distributed).
narrative_ontology:cs_reading_relation('46a1a76d-e487-4a21-be77-f55ad892c29d', gendered_category_membership__biological_sex_reading, forecloses).
narrative_ontology:cs_reading_relation('46a1a76d-e487-4a21-be77-f55ad892c29d', gendered_category_membership__social_role_reading, coexists_with).
narrative_ontology:cs_axiom('46a1a76d-e487-4a21-be77-f55ad892c29d', foundational, gender_identity_self_determination).
narrative_ontology:cs_axiom_status(gender_identity_self_determination, holdable).
narrative_ontology:cs_axiom_grounding('46a1a76d-e487-4a21-be77-f55ad892c29d', gender_identity_self_determination, deontological).
narrative_ontology:cs_axiom('46a1a76d-e487-4a21-be77-f55ad892c29d', secondary, sex_segregation_as_exclusion).
narrative_ontology:cs_axiom_status(sex_segregation_as_exclusion, holdable).
narrative_ontology:cs_axiom_grounding('46a1a76d-e487-4a21-be77-f55ad892c29d', sex_segregation_as_exclusion, deontological).
narrative_ontology:cs_reference_frame('46a1a76d-e487-4a21-be77-f55ad892c29d', medical_gatekeeping_model).
narrative_ontology:cs_drift_state('46a1a76d-e487-4a21-be77-f55ad892c29d', contemporary_self_id_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('46a1a76d-e487-4a21-be77-f55ad892c29d', '').
narrative_ontology:cs_kernel_id(gendered_category_membership__gender_identity_reading, gendered_category_membership).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gendered_category_membership__gender_identity_reading, trans_women).
narrative_ontology:constraint_beneficiary(gendered_category_membership__gender_identity_reading, trans_men).
narrative_ontology:constraint_beneficiary(gendered_category_membership__gender_identity_reading, non_binary_people).
narrative_ontology:constraint_victim(gendered_category_membership__gender_identity_reading, cis_women).
narrative_ontology:constraint_victim(gendered_category_membership__gender_identity_reading, gender_critical_feminists).
narrative_ontology:constraint_vindicates(gendered_category_membership__gender_identity_reading, gender_identity_self_determination).
narrative_ontology:constraint_vindicates(gendered_category_membership__gender_identity_reading, trans_inclusion_as_justice).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Gain access to women's spaces, legal recognition, and social validation through self-declaration without medical gatekeeping. Their category membership depends on the constraint's enforcement; exit means losing recognition and facing misgendering.
narrative_ontology:constraint_stakeholder(gendered_category_membership__gender_identity_reading, trans_women, beneficiary,
    organized, biographical, identity_locked, national).

% Gain access to men's spaces and legal recognition through self-declaration. Similar identity-locked position as trans women but with different spatial and institutional dynamics.
narrative_ontology:constraint_stakeholder(gendered_category_membership__gender_identity_reading, trans_men, beneficiary,
    organized, biographical, identity_locked, national).

% Gain recognition of non-binary gender categories and escape binary classification. Benefit from the constraint's expansion of category ontology beyond male/female.
narrative_ontology:constraint_stakeholder(gendered_category_membership__gender_identity_reading, non_binary_people, beneficiary,
    moderate, biographical, identity_locked, national).

% Lose sex-segregated spaces (shelters, prisons, sports, changing rooms) as these become gender-segregated. Face moral and institutional pressure to accept self-ID inclusion; resistance is framed as exclusionary bigotry. Exit options limited — can advocate for sex-based rights but face professional and social sanction.
narrative_ontology:constraint_stakeholder(gendered_category_membership__gender_identity_reading, cis_women, payer,
    organized, generational, constrained, national).

% Advocate for sex-based category membership and retention of sex-segregated spaces. Structurally excluded from mainstream feminist and LGBTQ+ institutions; deplatformed, denied funding, and professionally sanctioned for their position. Bear costs of both the constraint's operation and their resistance to it.
narrative_ontology:constraint_stakeholder(gendered_category_membership__gender_identity_reading, gender_critical_feminists, payer,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(gendered_category_membership__gender_identity_reading, gender_critical_feminists, excluded).

% Administer gender recognition certificates, update legal documents, set clinical guidelines for gender-affirming care, and adjudicate access disputes. Capture institutional authority and funding streams from gatekeeping function; can pivot between affirmation and gatekeeping models.
narrative_ontology:constraint_stakeholder(gendered_category_membership__gender_identity_reading, medical_legal_institutions, agenda_setter,
    institutional, generational, arbitrage, national).

% Philosophers, sociologists, legal scholars, and bioethicists analyzing the constraint's structure, justification, and effects. No material stake in the category membership outcome; provide the analytical seat the engine computes from.
narrative_ontology:constraint_stakeholder(gendered_category_membership__gender_identity_reading, analytical_observers, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a stable, administrable criterion for gender category membership that avoids invasive medical gatekeeping and respects individuals' self-knowledge. Solves the coordination problem of how institutions (legal, medical, social) classify people without requiring bodily verification.
% TRANSFER_FUNCTION: Moves the authority to define category boundaries from biological criteria (chromosomes, gametes, anatomy) to self-declaration. Transfers the cost of boundary disputes from trans people (who previously bore medical/legal transition burdens) to cis women (who lose sex-segregated spaces) and gender-critical feminists (who bear professional/social sanctions for dissent).
% ABSENT_VOICES: Women in the Global South where gender self-ID laws are imported via international human rights frameworks without local consultation; working-class women dependent on single-sex shelters and prisons who lack platforms to object; detransitioners whose experiences complicate the self-ID narrative but are marginalized in advocacy spaces.
% DISAPPEARANCE_RATIONALE: If self-ID gender recognition vanished overnight, legal systems would revert to medical/biological gatekeeping for gender marker changes; sex-segregated spaces would be reinstated by default; trans people would face renewed barriers to recognition; feminist organizations currently split would reorganize around sex-based or gender-identity-based lines; the institutional infrastructure built around self-ID (certificates, guidelines, training) would require dismantling.
% FOUNDING_PROBLEM: Medical gatekeeping of gender transition created inhumane barriers: pathologizing trans identity, requiring sterilization, imposing years-long waitlists, and granting clinicians veto power over legal recognition. The self-ID model was built to remove state and medical coercion from gender recognition.
% FOUNDING_PROBLEM_CORROBORATION: Trans advocacy organizations and human rights bodies attest the founding problem remains live (ongoing pathologization in many jurisdictions, non-binary recognition gaps). Gender-critical feminists and some medical professionals attest the founding problem is substantially solved in progressive jurisdictions and the arrangement now functions as category capture rather than liberation. UN Special Rapporteur reports and national human rights commission findings from outside the primary beneficiary set document both persistent medical barriers in some regions and emerging sex-based rights conflicts in others.
narrative_ontology:disappearance_verdict(gendered_category_membership__gender_identity_reading, world_rearranges).
narrative_ontology:founding_problem_status(gendered_category_membership__gender_identity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gendered_category_membership__gender_identity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(gendered_category_membership__gender_identity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(gendered_category_membership__gender_identity_reading, 0.42, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gendered_category_membership__gender_identity_reading_tests).
:- end_tests(gendered_category_membership__gender_identity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.42) reflects moderate but real transfer: cis women lose sex-segregated spaces and face moral costs; gender-critical feminists bear professional sanctions; trans people gain recognition without medical gatekeeping. Suppression (0.58) is significant — the constraint persists through active institutional enforcement (hate speech laws, workplace policies, platform moderation, funding conditions) that silence dissent. Theater ratio (0.35) captures performative inclusion rhetoric masking the material transfer of spaces and resources. Accessibility collapse (0.45) is moderate: sex-based alternatives persist in some jurisdictions and private spheres but are legally and culturally marginalized. Resistance (0.62) is high and growing — evidenced by legal challenges, feminist splintering, and cross-political coalitions forming against self-ID.
 *
 * PERSPECTIVAL GAP:
 *   From trans beneficiaries' seat: the constraint is a rope — genuine coordination removing medical coercion. From cis women's seat: it is a snare — extraction of sex-based rights under cover of inclusion. From gender-critical feminists' seat: it is a snare with foreclosed exit. From institutions' seat: it is a coordination mechanism they administer for legitimacy and funding. The engine computes this divergence; the authored claim (tangled_rope) reflects the structural reality that both coordination and extraction are simultaneously real.
 *
 * DIRECTIONALITY LOGIC:
 *   Trans people (beneficiaries) sit at d ≈ 0.15 — the constraint subsidizes their recognition. Cis women (payers) sit at d ≈ 0.75 — they bear the spatial, competitive, and moral costs. Gender-critical feminists (payers/excluded) sit at d ≈ 0.85 — they bear costs plus exclusion from discourse. Medical/legal institutions (agenda_setters) sit at d ≈ 0.10 — they capture administrative authority and funding. The engine will compute per-seat effective extraction from these structural positions.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (medical gatekeeping) is contested — substantially solved in some jurisdictions, ongoing in others. The constraint persists beyond its liberation function into category capture: self-ID now operates as the *only* legitimate criterion, foreclosing sex-based alternatives. Mandatrophy is unresolved — the arrangement has outlived its original justification in progressive jurisdictions but expands globally via human rights frameworks.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is this constraint a genuine coordination mechanism for trans recognition, or a category capture operation that extracts from cis women under cover of inclusion?',
    'Track whether jurisdictions adopting self-ID retain any sex-based provisions (prisons, sports, shelters) or fully substitute gender for sex. Full substitution with no carve-outs indicates capture; meaningful carve-outs indicate genuine coordination with bounded extraction.',
    'If capture, the constraint reclassifies toward snare for cis women''s seat. If bounded coordination with carve-outs, tangled_rope holds with lower effective extraction for payers.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Whether the gender identity reading''s coordination function is genuine or a cover for extraction.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (legal penalties, deplatforming, funding cuts) or internalized (cis women self-silencing due to moral pressure, fear of being labeled bigots)?',
    'Post-policy-change speech surveys: if cis women''s expressed views shift toward gender-critical positions when anonymity is guaranteed, internalized suppression is significant.',
    'If internalized suppression is substantial, the constraint''s effective suppression exceeds the structural measure — targets carry the suppression with them. This would raise effective extraction for payer seats.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism in gender category enforcement.').

omega_variable(
    cs_framing_underdetermination,
    'Does the gender identity reading ground its authority in a distributed kernel (no single adjudicator) or in an extraction-based authority (institutions benefiting from foreclosing sex-based alternatives)?',
    'Analyze whether medical/legal institutions adopting self-ID gain budget, staff, and jurisdictional expansion from administering gender recognition vs. sex classification.',
    'If extraction-based authority, cs_structure.authority_grounding shifts from distributed to extraction, changing the commitment-system classification and drift trajectory.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(cs_framing_underdetermination, conceptual, 'Whether the constraint''s authority structure is genuinely distributed or extracts from foreclosing alternatives.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gendered_category_membership__gender_identity_reading, 2010, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gend_tr_t2010, gendered_category_membership__gender_identity_reading, theater_ratio, 2010, 0.1).
narrative_ontology:measurement(gend_tr_t2014, gendered_category_membership__gender_identity_reading, theater_ratio, 2014, 0.15).
narrative_ontology:measurement(gend_tr_t2018, gendered_category_membership__gender_identity_reading, theater_ratio, 2018, 0.25).
narrative_ontology:measurement(gend_tr_t2020, gendered_category_membership__gender_identity_reading, theater_ratio, 2020, 0.3).
narrative_ontology:measurement(gend_tr_t2022, gendered_category_membership__gender_identity_reading, theater_ratio, 2022, 0.33).
narrative_ontology:measurement(gend_tr_t2024, gendered_category_membership__gender_identity_reading, theater_ratio, 2024, 0.35).

% Extraction over time
narrative_ontology:measurement(gend_be_t2010, gendered_category_membership__gender_identity_reading, base_extractiveness, 2010, 0.15).
narrative_ontology:measurement(gend_be_t2014, gendered_category_membership__gender_identity_reading, base_extractiveness, 2014, 0.22).
narrative_ontology:measurement(gend_be_t2018, gendered_category_membership__gender_identity_reading, base_extractiveness, 2018, 0.35).
narrative_ontology:measurement(gend_be_t2020, gendered_category_membership__gender_identity_reading, base_extractiveness, 2020, 0.38).
narrative_ontology:measurement(gend_be_t2022, gendered_category_membership__gender_identity_reading, base_extractiveness, 2022, 0.4).
narrative_ontology:measurement(gend_be_t2024, gendered_category_membership__gender_identity_reading, base_extractiveness, 2024, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(gend_su_t2010, gendered_category_membership__gender_identity_reading, suppression_requirement, 2010, 0.25).
narrative_ontology:measurement(gend_su_t2014, gendered_category_membership__gender_identity_reading, suppression_requirement, 2014, 0.35).
narrative_ontology:measurement(gend_su_t2018, gendered_category_membership__gender_identity_reading, suppression_requirement, 2018, 0.48).
narrative_ontology:measurement(gend_su_t2020, gendered_category_membership__gender_identity_reading, suppression_requirement, 2020, 0.52).
narrative_ontology:measurement(gend_su_t2022, gendered_category_membership__gender_identity_reading, suppression_requirement, 2022, 0.55).
narrative_ontology:measurement(gend_su_t2024, gendered_category_membership__gender_identity_reading, suppression_requirement, 2024, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gendered_category_membership__gender_identity_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(gendered_category_membership__gender_identity_reading, 0.08).
narrative_ontology:affects_constraint(gendered_category_membership__gender_identity_reading, gendered_category_membership__biological_sex_reading).
narrative_ontology:affects_constraint(gendered_category_membership__gender_identity_reading, gendered_category_membership__social_role_reading).

% DUAL FORMULATION NOTE:
% This constraint and its siblings form a kernel family: gendered_category_membership decomposes into three structurally distinct readings with different ε values, beneficiary/victim structures, and coordination/extraction profiles. The gender_identity_reading (this story) has moderate ε (0.42) and tangled_rope structure. The biological_sex_reading has low ε (~0.15) and rope/mountain structure. The social_role_reading has moderate ε (~0.35) and scaffold/tangled_rope structure. They are linked via network.affects_constraints in all three files.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(gendered_category_membership__gender_identity_reading, institutional, 0.1).
constraint_indexing:directionality_override(gendered_category_membership__gender_identity_reading, organized, 0.75).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
