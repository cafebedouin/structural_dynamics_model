% ============================================================================
% CONSTRAINT STORY: gendered_category_membership__gender_identity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-14
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
 *   human_readable: Gender Category Membership via Self-Declaration
 *   domain: social_ontology/political_philosophy/bioethics
 *
 * SUMMARY:
 *   This constraint story models the gender_identity_reading of the contested
 *   kernel 'gendered_category_membership': the claim that category membership
 *   in 'woman'/'man' is grounded in subjective gender identity and
 *   self-declaration, such that trans women are women by declaration,
 *   sex-segregated spaces become gender-segregated, and resistance to this
 *   re-categorization is framed as exclusion. The constraint operates through
 *   institutional policy (government ID changes, sports federation rules,
 *   prison placement, shelter admission, school facilities), corporate HR
 *   mandates, and social enforcement (deplatforming, employment loss, social
 *   ostracism for dissent). The claimed_type is tangled_rope because there is
 *   a genuine coordination function (reducing gatekeeping violence against
 *   trans people, simplifying legal recognition) AND asymmetric extraction
 *   (cis women lose sex-based protections and single-sex spaces without
 *   consent or compensation; female athletes lose fair competition; service
 *   providers face compelled speech). Active enforcement is required: the
 *   constraint cannot persist without suppressing the biological_sex_reading
 *   and social_role_reading alternatives, which remain live in law, science,
 *   and public opinion. ε = 0.48 reflects moderate but rising extraction: the
 *   coordination function is real but the extraction layer has grown as the
 *   constraint expanded from legal recognition into material resource
 *   allocation (sports, prisons, shelters, rape crisis centers).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gendered_category_membership__gender_identity_reading, 0.48).
domain_priors:suppression_score(gendered_category_membership__gender_identity_reading, 0.62).
domain_priors:theater_ratio(gendered_category_membership__gender_identity_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gendered_category_membership__gender_identity_reading, extractiveness, 0.48).
narrative_ontology:constraint_metric(gendered_category_membership__gender_identity_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(gendered_category_membership__gender_identity_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gendered_category_membership__gender_identity_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(gendered_category_membership__gender_identity_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gendered_category_membership__gender_identity_reading, tangled_rope).
narrative_ontology:human_readable(gendered_category_membership__gender_identity_reading, "Gender Category Membership via Self-Declaration").
narrative_ontology:topic_domain(gendered_category_membership__gender_identity_reading, "social_ontology/political_philosophy/bioethics").

domain_priors:requires_active_enforcement(gendered_category_membership__gender_identity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gendered_category_membership__gender_identity_reading, '5bb3200a-e899-4429-9b8e-458f78a38141').
narrative_ontology:cs_kernel_codification('5bb3200a-e899-4429-9b8e-458f78a38141', distributed).
narrative_ontology:cs_authority_grounding('5bb3200a-e899-4429-9b8e-458f78a38141', diffuse_epistemic).
narrative_ontology:cs_reading_relation('5bb3200a-e899-4429-9b8e-458f78a38141', gendered_category_membership__biological_sex_reading, forecloses).
narrative_ontology:cs_reading_relation('5bb3200a-e899-4429-9b8e-458f78a38141', gendered_category_membership__social_role_reading, coexists_with).
narrative_ontology:cs_axiom('5bb3200a-e899-4429-9b8e-458f78a38141', foundational, gender_identity_is_self_determined_and_authoritative).
narrative_ontology:cs_axiom_status(gender_identity_is_self_determined_and_authoritative, holdable).
narrative_ontology:cs_axiom_grounding('5bb3200a-e899-4429-9b8e-458f78a38141', gender_identity_is_self_determined_and_authoritative, deontological).
narrative_ontology:cs_axiom('5bb3200a-e899-4429-9b8e-458f78a38141', foundational, trans_women_are_women_without_qualification).
narrative_ontology:cs_axiom_status(trans_women_are_women_without_qualification, holdable).
narrative_ontology:cs_axiom_grounding('5bb3200a-e899-4429-9b8e-458f78a38141', trans_women_are_women_without_qualification, deontological).
narrative_ontology:cs_axiom('5bb3200a-e899-4429-9b8e-458f78a38141', secondary, sex_based_categories_are_exclusionary_and_obsolete).
narrative_ontology:cs_axiom_status(sex_based_categories_are_exclusionary_and_obsolete, holdable).
narrative_ontology:cs_axiom_grounding('5bb3200a-e899-4429-9b8e-458f78a38141', sex_based_categories_are_exclusionary_and_obsolete, instrumental).
narrative_ontology:cs_reference_frame('5bb3200a-e899-4429-9b8e-458f78a38141', medical_gatekeeping_model).
narrative_ontology:cs_drift_state('5bb3200a-e899-4429-9b8e-458f78a38141', contemporary_self_id_expansion, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('5bb3200a-e899-4429-9b8e-458f78a38141', '').
narrative_ontology:cs_kernel_id(gendered_category_membership__gender_identity_reading, gendered_category_membership).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gendered_category_membership__gender_identity_reading, trans_women).
narrative_ontology:constraint_beneficiary(gendered_category_membership__gender_identity_reading, nonbinary_people_seeking_category_access).
narrative_ontology:constraint_beneficiary(gendered_category_membership__gender_identity_reading, gender_affirming_institutions).
narrative_ontology:constraint_victim(gendered_category_membership__gender_identity_reading, cis_women_in_segregated_spaces).
narrative_ontology:constraint_victim(gendered_category_membership__gender_identity_reading, female_athletes_in_competitive_sport).
narrative_ontology:constraint_victim(gendered_category_membership__gender_identity_reading, service_providers_facing_compelled_speech).
narrative_ontology:constraint_vindicates(gendered_category_membership__gender_identity_reading, gender_self_determination).
narrative_ontology:constraint_vindicates(gendered_category_membership__gender_identity_reading, trans_inclusion_as_nondiscrimination).
narrative_ontology:constraint_vindicates(gendered_category_membership__gender_identity_reading, identity_as_authoritative_for_social_category).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Gain legal recognition and access to women's spaces, sports, services through self-declaration. Their gender identity is fused with self-concept — exit from the category would mean detransition, which is existentially costly. They organize politically to defend and expand the constraint. The coordination benefit (freedom from gatekeeping) is real and substantial.
narrative_ontology:constraint_stakeholder(gendered_category_membership__gender_identity_reading, trans_women, beneficiary,
    organized, biographical, identity_locked, global).

% Lose access to single-sex spaces (changing rooms, shelters, prisons, sports categories) redefined as gender-segregated. Cannot exit the 'woman' sex class — it is a material reality, not an identity choice. Feminist resistance is identity-locked for those whose political self-concept is constituted through sex-class analysis. They bear the costs of inclusion (privacy, safety, fair competition) without receiving its benefits.
narrative_ontology:constraint_stakeholder(gendered_category_membership__gender_identity_reading, cis_women_in_segregated_spaces, payer,
    moderate, biographical, identity_locked, national).

% Face male-puberty-advantaged competitors in female categories under self-ID policies. Sport is a biographical horizon — exit means abandoning athletic career. The constraint extracts competitive integrity and records. Some resist publicly and face sanction; others exit quietly. The extraction is concentrated on a small, visible group.
narrative_ontology:constraint_stakeholder(gendered_category_membership__gender_identity_reading, female_athletes_in_competitive_sport, payer,
    moderate, biographical, constrained, global).

% Required to use preferred pronouns, admit trans women to female-only services (rape crisis centers, domestic violence shelters, women's prisons). Face legal penalties, professional discipline, or loss of funding for non-compliance. Can exit by changing profession but at high biographical cost. The constraint extracts expressive freedom and professional autonomy.
narrative_ontology:constraint_stakeholder(gendered_category_membership__gender_identity_reading, service_providers_facing_compelled_speech, payer,
    moderate, biographical, constrained, national).

% Major medical associations (WPATH, APA, AMA), human rights organizations, corporate DEI departments, government agencies. They set and enforce the self-ID standard. Their institutional mission, funding, and legitimacy are validated by the constraint's expansion. They have arbitrage-grade exit — they could pivot to other frameworks — but the constraint serves their institutional interests.
narrative_ontology:constraint_stakeholder(gendered_category_membership__gender_identity_reading, gender_affirming_institutions, beneficiary,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(gendered_category_membership__gender_identity_reading, gender_affirming_institutions, agenda_setter).

% Gain some recognition through self-ID frameworks (X markers on documents, nonbinary categories) but the binary 'woman/man' structure remains dominant. Their category access is partial and unstable — they benefit from the constraint's challenge to biological essentialism but are not its primary beneficiaries. Exit is constrained by the binary architecture of most institutions.
narrative_ontology:constraint_stakeholder(gendered_category_membership__gender_identity_reading, nonbinary_people_seeking_category_access, beneficiary,
    moderate, biographical, constrained, global).

% Observes the full constraint structure across all seats. Sees the coordination function (trans inclusion) and the extraction layer (cis women's lost protections) as simultaneous structural facts. Does not collect or pay; computes the per-seat divergence that the engine formalizes.
narrative_ontology:constraint_stakeholder(gendered_category_membership__gender_identity_reading, analytical_observer, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the genuine coordination problem of trans people being excluded from legal recognition, healthcare, and social participation by gatekeeping requirements (medical diagnosis, surgery, court orders). Self-ID replaces institutional gatekeeping with self-declaration, reducing violence, bureaucratic burden, and medicalization.
% TRANSFER_FUNCTION: Moves category access, legal protections, and material resources (sports rankings, shelter beds, prison placements) from sex-based allocation to identity-based allocation. Cis women lose sex-segregated spaces and fair competition; trans women gain access to women's categories. The transfer is zero-sum in fixed-resource domains (sports, shelters, prisons) and positive-sum in recognition domains (documents, social acknowledgment).
% ABSENT_VOICES: Detransitioners and desisters — people who transitioned and later re-identified with their sex — are structurally excluded from the conversation. Their experience contradicts the 'identity is immutable and authoritative' axiom. Gender-critical feminists in institutional positions (academics, clinicians, journalists) who lose platforms for dissent. Parents of gender-questioning minors who question affirmation-only protocols. These voices would object if present but are kept out by the same suppression machinery the constraint deploys.
% DISAPPEARANCE_RATIONALE: If self-ID category membership vanished overnight: legal documents would revert to sex-based classification; sports federations would reinstate sex-based eligibility; prisons and shelters would return to sex-segregated placement; compelled pronoun mandates would lapse. Trans people would lose legal recognition and access gained; cis women would regain sex-based spaces. The world rearranges substantially — arrangements depend on this constraint.
% FOUNDING_PROBLEM: Trans people faced systematic exclusion: legal recognition required medical gatekeeping (diagnosis, hormones, surgery), exposing them to violence, unemployment, and healthcare denial. The medical model pathologized gender variance. The founding problem was gatekeeping violence and medicalization of identity.
% FOUNDING_PROBLEM_CORROBORATION: Trans advocacy organizations and gender clinics attest the problem remains live — citing ongoing violence, healthcare access gaps, and global jurisdictions without recognition. Gender-critical feminists and some clinicians attest the founding problem is substantially solved in Western jurisdictions (legal self-ID exists in 15+ countries, medical gatekeeping reduced, social acceptance risen) and the constraint now expands into domains (sports, prisons, shelters) where the original gatekeeping problem did not exist. Independent longitudinal studies (e.g., Sweden 1973-2003, UK Cass Review 2024) corroborate that medical transition outcomes are mixed and the affirmative model's evidence base is thin — this corroborates the 'contested' status from outside the beneficiary set.
narrative_ontology:disappearance_verdict(gendered_category_membership__gender_identity_reading, world_rearranges).
narrative_ontology:founding_problem_status(gendered_category_membership__gender_identity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gendered_category_membership__gender_identity_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(gendered_category_membership__gender_identity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(gendered_category_membership__gender_identity_reading, 0.48, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gendered_category_membership__gender_identity_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(gendered_category_membership__gender_identity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(gendered_category_membership__gender_identity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction (0.48) is moderate but rising: trans people gain access to categories/spaces they were excluded from (coordination benefit), while cis women lose sex-segregated spaces and fair competition without compensation (extraction). The asymmetry is structural — the constraint redefines 'woman' to include trans women but does not create new protections for cis women's lost spaces. Suppression (0.62) is high: dissenting women face institutional discipline, social punishment, and legal risk; the biological_sex_reading is suppressed in institutional policy though not in scientific discourse. Theater_ratio (0.28) is moderate: inclusion rhetoric is genuine but a growing share of enforcement activity defends the category boundary against the biological_sex_reading rather than protecting trans people from violence. Accessibility_collapse (0.35) is moderate: alternatives (sex-based categories, third spaces) exist but are institutionally foreclosed. Resistance (0.71) is high: organized feminist resistance, legal challenges, public opinion majorities against self-ID in sports/prisons/shelters.
 *
 * PERSPECTIVAL GAP:
 *   The trans_women seat experiences this as a rope (genuine coordination solving real exclusion). The cis_women_in_segregated_spaces seat experiences it as a snare (extraction without consent, suppression of dissent). The gender_affirming_institutions seat experiences it as a scaffold (transitional justice toward full recognition). The analytical_observer seat sees the structural divergence: same constraint, three different computed types. The engine will compute this from the declared structural data; the claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   trans_women: primary beneficiary (d ~ 0.15) — gain category access, legal recognition, protection from gatekeeping. cis_women_in_segregated_spaces: primary target (d ~ 0.85) — lose sex-based spaces, fair competition, associative freedom; constrained exit (cannot leave sex class); identity_locked for those whose feminism is fused with sex-class analysis. female_athletes: target (d ~ 0.80) — lose competitive integrity; constrained exit (sport is biographical horizon). service_providers_facing_compelled_speech: target (d ~ 0.70) — compelled pronoun usage, facility policies; moderate exit (can change profession). gender_affirming_institutions: beneficiary (d ~ 0.20) — institutional mission validated, funding flows. nonbinary_people_seeking_category_access: partial beneficiary (d ~ 0.30) — some access gained but nonbinary category remains unstable. analytical_observer: analytical (d = 0.50) — sees full structure.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (gatekeeping violence against trans people, medicalization of identity) is live but substantially solved in Western jurisdictions — legal recognition exists, medical gatekeeping reduced. The constraint persists and expands into domains (sports, prisons, shelters) where the original coordination problem does not apply. This is mandatrophy: the mandate (protect trans people from exclusion) has outlived its function in the original domain and now extracts in new domains. The identity_coordination floor (0.08) is conservative — the constraint's coordination cost is real but the extraction layer (0.48) far exceeds it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_framing_underdetermination,
    'Does the kernel ''gendered_category_membership'' properly name the commitment, or does it conflate three distinct constraints (legal recognition, social recognition, material resource allocation) that would each have different ε and different stakeholder structures?',
    'Decompose the kernel into three constraint stories: legal_gender_recognition, social_gender_recognition, sex_segregated_resource_allocation. If each decomposes to a different type with different beneficiary/victim sets, the single kernel framing is a category error.',
    'If the kernel decomposes, the gender_identity_reading is not one reading of one constraint but three readings of three constraints — the committer frame would be misapplied and the reading_relations would need re-authoring per decomposed constraint.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_framing_underdetermination, conceptual, 'Whether the kernel itself is a single commitment or a conflation of multiple constraints').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (policy mandates, institutional capture, legal penalties) or internalized (fear of social sanction, moral injury from being labeled exclusionary, identity-fused inability to articulate dissent)?',
    'Track suppression trajectory after policy reversal in a jurisdiction: if suppression persists after legal mandates are removed, internalized component is significant. Survey cis women in female-only spaces pre/post policy change measuring willingness to articulate boundaries.',
    'If substantially internalized, effective suppression is higher than structural measure suggests — the constraint operates through the target''s own cognition, making exit_options ''identity_locked'' more prevalent than ''constrained''.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism in gender category enforcement').

omega_variable(
    committer_frame_reading_identity,
    'This constraint is one reading (gender_identity_reading) of the contested kernel gendered_category_membership. What would the sibling readings (biological_sex_reading, social_role_reading) change structurally?',
    'Author the sibling constraints as separate JSON files. Compare ε, beneficiary/victim sets, and claimed_type across the three readings. The structural delta between them IS the committer structure.',
    'If sibling readings produce different types (e.g., biological_sex_reading = mountain, social_role_reading = rope, gender_identity_reading = tangled_rope), the kernel is a genuine site of structural contestation. If all three produce the same type with only beneficiary relabeling, the contest is perspectival, not structural.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(committer_frame_reading_identity, conceptual, 'Commitment structure: this reading vs. biological_sex_reading and social_role_reading').

omega_variable(
    extraction_asymmetry_cis_women,
    'Do cis women in sex-segregated spaces experience net extraction (loss of privacy, safety, fair competition, associative freedom) that is structurally asymmetric — i.e., they pay the costs of inclusion without receiving its benefits — or is the cost symmetric (shared adjustment burden)?',
    'Measure material outcomes: competitive records, facility usage patterns, reporting rates for harassment/voyeurism, associative exit rates from single-sex spaces. Compare to pre-policy baselines. If cis women''s outcomes degrade while trans women''s improve without reciprocity, extraction is asymmetric.',
    'Asymmetric extraction with active enforcement and a coordination function (trans inclusion) = tangled_rope. If extraction is symmetric, the constraint may be a rope with transition costs. If no genuine coordination function exists, snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraction_asymmetry_cis_women, empirical, 'Whether cis women bear asymmetric costs in gender-identity-based category membership').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gendered_category_membership__gender_identity_reading, 2010, 2030).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gendered_category_membership__gender_identity_reading_tr_t2010, gendered_category_membership__gender_identity_reading, theater_ratio, 2010, 0.08).
narrative_ontology:measurement(gendered_category_membership__gender_identity_reading_tr_t2014, gendered_category_membership__gender_identity_reading, theater_ratio, 2014, 0.12).
narrative_ontology:measurement(gendered_category_membership__gender_identity_reading_tr_t2018, gendered_category_membership__gender_identity_reading, theater_ratio, 2018, 0.18).
narrative_ontology:measurement(gendered_category_membership__gender_identity_reading_tr_t2022, gendered_category_membership__gender_identity_reading, theater_ratio, 2022, 0.24).
narrative_ontology:measurement(gendered_category_membership__gender_identity_reading_tr_t2026, gendered_category_membership__gender_identity_reading, theater_ratio, 2026, 0.28).
narrative_ontology:measurement(gendered_category_membership__gender_identity_reading_tr_t2030, gendered_category_membership__gender_identity_reading, theater_ratio, 2030, 0.33).

% Extraction over time
narrative_ontology:measurement(gendered_category_membership__gender_identity_reading_be_t2010, gendered_category_membership__gender_identity_reading, base_extractiveness, 2010, 0.18).
narrative_ontology:measurement(gendered_category_membership__gender_identity_reading_be_t2014, gendered_category_membership__gender_identity_reading, base_extractiveness, 2014, 0.25).
narrative_ontology:measurement(gendered_category_membership__gender_identity_reading_be_t2018, gendered_category_membership__gender_identity_reading, base_extractiveness, 2018, 0.38).
narrative_ontology:measurement(gendered_category_membership__gender_identity_reading_be_t2022, gendered_category_membership__gender_identity_reading, base_extractiveness, 2022, 0.44).
narrative_ontology:measurement(gendered_category_membership__gender_identity_reading_be_t2026, gendered_category_membership__gender_identity_reading, base_extractiveness, 2026, 0.48).
narrative_ontology:measurement(gendered_category_membership__gender_identity_reading_be_t2030, gendered_category_membership__gender_identity_reading, base_extractiveness, 2030, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(gendered_category_membership__gender_identity_reading_su_t2010, gendered_category_membership__gender_identity_reading, suppression_requirement, 2010, 0.35).
narrative_ontology:measurement(gendered_category_membership__gender_identity_reading_su_t2014, gendered_category_membership__gender_identity_reading, suppression_requirement, 2014, 0.42).
narrative_ontology:measurement(gendered_category_membership__gender_identity_reading_su_t2018, gendered_category_membership__gender_identity_reading, suppression_requirement, 2018, 0.55).
narrative_ontology:measurement(gendered_category_membership__gender_identity_reading_su_t2022, gendered_category_membership__gender_identity_reading, suppression_requirement, 2022, 0.6).
narrative_ontology:measurement(gendered_category_membership__gender_identity_reading_su_t2026, gendered_category_membership__gender_identity_reading, suppression_requirement, 2026, 0.62).
narrative_ontology:measurement(gendered_category_membership__gender_identity_reading_su_t2030, gendered_category_membership__gender_identity_reading, suppression_requirement, 2030, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gendered_category_membership__gender_identity_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(gendered_category_membership__gender_identity_reading, 0.08).
narrative_ontology:affects_constraint(gendered_category_membership__gender_identity_reading, gendered_category_membership__biological_sex_reading).
narrative_ontology:affects_constraint(gendered_category_membership__gender_identity_reading, gendered_category_membership__social_role_reading).
narrative_ontology:affects_constraint(gendered_category_membership__gender_identity_reading, sex_segregated_sports_eligibility).
narrative_ontology:affects_constraint(gendered_category_membership__gender_identity_reading, sex_segregated_prison_placement).
narrative_ontology:affects_constraint(gendered_category_membership__gender_identity_reading, sex_segregated_shelter_access).
narrative_ontology:affects_constraint(gendered_category_membership__gender_identity_reading, compelled_pronoun_usage_mandates).

% DUAL FORMULATION NOTE:
% This constraint and its two siblings form the gendered_category_membership constraint family. Each reading instantiates a different constraint with different ε, different beneficiary/victim structures, and different types. The kernel label 'gendered_category_membership' conflates three structurally distinct claims. This decomposition follows the BGS pattern: ehrenfest_barrier (mountain) → bgs_spectral_universality (mountain) → bgs_eigenvector_thermalization (tangled_rope). Here: biological_sex_reading may be mountain (if biological sex is treated as natural law), social_role_reading may be rope (coordination via mutual recognition), gender_identity_reading is tangled_rope (coordination + asymmetric extraction).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(gendered_category_membership__gender_identity_reading, organized, 0.15).
constraint_indexing:directionality_override(gendered_category_membership__gender_identity_reading, moderate, 0.85).
constraint_indexing:directionality_override(gendered_category_membership__gender_identity_reading, moderate, 0.8).
constraint_indexing:directionality_override(gendered_category_membership__gender_identity_reading, moderate, 0.7).
constraint_indexing:directionality_override(gendered_category_membership__gender_identity_reading, institutional, 0.2).
constraint_indexing:directionality_override(gendered_category_membership__gender_identity_reading, moderate, 0.3).
constraint_indexing:directionality_override(gendered_category_membership__gender_identity_reading, analytical, 0.5).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
