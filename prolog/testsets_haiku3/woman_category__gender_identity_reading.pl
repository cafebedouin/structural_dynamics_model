% ============================================================================
% CONSTRAINT STORY: woman_category__gender_identity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: woman_category__gender_identity_reading
 *   human_readable: Gender Identity Definition of Woman Category
 *   domain: political_philosophy/law/social_policy/bioethics
 *
 * SUMMARY:
 *   This constraint instantiates ONE READING of the contested 'woman
 *   category' kernel: the gender-identity reading, which holds that 'woman' =
 *   a person who identifies as a woman, regardless of assigned sex at birth.
 *   This reading benefits transgender women and non-binary people identifying
 *   as female (who gain legal recognition) while imposing costs on some
 *   cisgender women and users of sex-segregated spaces (who lose exclusive
 *   institutional access). The constraint is claimed as tangled-rope because
 *   it coordinates legal gender recognition (solving a genuine institutional
 *   coordination problem) while simultaneously extracting from those who
 *   relied on sex-based category exclusivity (asymmetric distribution of
 *   benefits and costs). The extractiveness score reflects the asymmetry:
 *   extraction is moderate to high (0.62) in institutional access and sports
 *   domains where access rights collide; suppression is high (0.58) because
 *   the constraint depends on active institutional enforcement (policy
 *   revision, document rewriting, legal mandate) to override prior practice.
 *   Theater ratio (0.41) reflects that the coordination narrative (we need a
 *   workable definition of woman for legal purposes) is real, but a growing
 *   share of enforcement activity defends the identity-based boundary against
 *   sex-essentialist contestation rather than solving the original
 *   coordination problem.
 *
 * KEY AGENTS:
 *   - transgender_women (powerless, identity-locked): gain legal recognition under the constraint's definition
 *   - cisgender_women (organized, constrained exit): undergo redefinition; lose institutional exclusivity
 *   - female_athletes (moderate power, constrained): face redefined competitive categories
 *   - sex_segregated_space_users (moderate power, constrained): navigate redefined access boundaries
 *   - institutional_administrators (institutional power): enforce policy changes
 *   - sex_essentialist_advocates (organized, mobile exit): contest the constraint's legitimacy
 *   - feminist_coalitions_contesting (organized, mobile exit, EXCLUDED): argue the constraint undermines sex-based organizing
 *   - legal_authority (institutional power): adjudicates the constraint across jurisdictions
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(woman_category__gender_identity_reading, 0.62).
domain_priors:suppression_score(woman_category__gender_identity_reading, 0.58).
domain_priors:theater_ratio(woman_category__gender_identity_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(woman_category__gender_identity_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(woman_category__gender_identity_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(woman_category__gender_identity_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(woman_category__gender_identity_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(woman_category__gender_identity_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(woman_category__gender_identity_reading, tangled_rope).
narrative_ontology:human_readable(woman_category__gender_identity_reading, "Gender Identity Definition of Woman Category").
narrative_ontology:topic_domain(woman_category__gender_identity_reading, "political_philosophy/law/social_policy/bioethics").

domain_priors:requires_active_enforcement(woman_category__gender_identity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(woman_category__gender_identity_reading, '3d9e0394-b21d-4e46-9fb2-eb53ed28ee2a').
narrative_ontology:cs_kernel_codification('3d9e0394-b21d-4e46-9fb2-eb53ed28ee2a', formalized).
narrative_ontology:cs_authority_grounding('3d9e0394-b21d-4e46-9fb2-eb53ed28ee2a', extraction).
narrative_ontology:cs_interpretation_layer_present('3d9e0394-b21d-4e46-9fb2-eb53ed28ee2a').
narrative_ontology:cs_reading_relation('3d9e0394-b21d-4e46-9fb2-eb53ed28ee2a', woman_category__sex_biology_reading, coexists_with).
narrative_ontology:cs_reading_relation('3d9e0394-b21d-4e46-9fb2-eb53ed28ee2a', woman_category__intersex_accommodation_reading, coexists_with).
narrative_ontology:cs_axiom('3d9e0394-b21d-4e46-9fb2-eb53ed28ee2a', foundational, gender_identity_criterion_for_woman_category).
narrative_ontology:cs_axiom_status(gender_identity_criterion_for_woman_category, holdable).
narrative_ontology:cs_axiom_grounding('3d9e0394-b21d-4e46-9fb2-eb53ed28ee2a', gender_identity_criterion_for_woman_category, deontological).
narrative_ontology:cs_axiom('3d9e0394-b21d-4e46-9fb2-eb53ed28ee2a', foundational, sex_based_categorization_as_rights_violation).
narrative_ontology:cs_axiom_status(sex_based_categorization_as_rights_violation, holdable).
narrative_ontology:cs_axiom_grounding('3d9e0394-b21d-4e46-9fb2-eb53ed28ee2a', sex_based_categorization_as_rights_violation, deontological).
narrative_ontology:cs_reference_frame('3d9e0394-b21d-4e46-9fb2-eb53ed28ee2a', gender_identity_legal_recognition_framework).
narrative_ontology:cs_drift_state('3d9e0394-b21d-4e46-9fb2-eb53ed28ee2a', contemporary_sex_essentialist_contestation_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('3d9e0394-b21d-4e46-9fb2-eb53ed28ee2a', '').
narrative_ontology:cs_kernel_id(woman_category__gender_identity_reading, woman_category).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(woman_category__gender_identity_reading, transgender_women).
narrative_ontology:constraint_beneficiary(woman_category__gender_identity_reading, non_binary_people_identifying_female).
narrative_ontology:constraint_victim(woman_category__gender_identity_reading, cisgender_women).
narrative_ontology:constraint_victim(woman_category__gender_identity_reading, sex_segregated_space_users).
narrative_ontology:constraint_victim(woman_category__gender_identity_reading, female_athletes).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(woman_category__gender_identity_reading, cisgender_women).
narrative_ontology:constraint_victim(woman_category__gender_identity_reading, sex_essentialist_advocates).
narrative_ontology:constraint_vindicates(woman_category__gender_identity_reading, gender_identity_primacy_doctrine).
narrative_ontology:constraint_vindicates(woman_category__gender_identity_reading, sex_based_legal_categorization_as_discriminatory).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Gain legal recognition of their gender identity; category membership based on internal identity rather than assigned sex at birth means access to legal documents, social recognition, and spaces organized by gender. Their self-conception as women is validated by the constraint's definition. Exit would require denying their identity or relocating to jurisdictions with different definitions.
narrative_ontology:constraint_stakeholder(woman_category__gender_identity_reading, transgender_women, beneficiary,
    powerless, biographical, identity_locked, national).

% Gain inclusion in 'woman' category when identity aligns with it, or obtain legal recognition outside binary categories. The constraint extends the woman category to include internal identification rather than requiring biological essentialism. Exit from this reading means either denying self-identified gender or accepting a categorical system that does not recognize their experience.
narrative_ontology:constraint_stakeholder(woman_category__gender_identity_reading, non_binary_people_identifying_female, beneficiary,
    powerless, biographical, identity_locked, national).

% Undergo redefinition: their historical monopoly on the 'woman' category expands to include others who internally identify as women. They bear costs in contested domains (sports eligibility, sex-segregated spaces, single-sex services) where access rights conflict with inclusion rights. They retain legal recognition as women but lose exclusive claim to gender-segregated institutional spaces. Many cisgender women support the constraint on principle; others contest it as erasing sex-based protections and women-only organizing spaces.
narrative_ontology:constraint_stakeholder(woman_category__gender_identity_reading, cisgender_women, payer,
    organized, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(woman_category__gender_identity_reading, cisgender_women, beneficiary).

% Access to single-sex spaces (shelters, bathrooms, prisons, changing facilities) is redefined by gender identity rather than sex. Users who relied on sex-based segregation for privacy, safety, or religious observance now navigate mixed-identity spaces. Their options are accepting the new boundary or relocating; legal challenges have moderate success depending on jurisdiction.
narrative_ontology:constraint_stakeholder(woman_category__gender_identity_reading, sex_segregated_space_users, payer,
    moderate, biographical, constrained, local).

% Face redefined athletic categories: women's sports now include transgender women, shifting competitive advantage calculations and record recognition. Individual female athletes' podium positions and record-holding status may be affected. Their options are competing in the redefined category or pursuing non-categorical athletics; exit from women's sports is possible but costly for elite athletes invested in the category's prestige.
narrative_ontology:constraint_stakeholder(woman_category__gender_identity_reading, female_athletes, payer,
    moderate, biographical, constrained, national).

% Enforce the constraint by revising policies: rewrite identity documents to accept self-identification, update bathroom/shelter/prison policies, modify athletic eligibility rules. They operate under legal mandates (in some jurisdictions) or institutional commitments (in others). Administrative burden is high; litigation risk is moderate to high. Their exit is limited by legal exposure and constituency pressure from both sides.
narrative_ontology:constraint_stakeholder(woman_category__gender_identity_reading, institutional_administrators, agenda_setter,
    institutional, generational, constrained, national).

% Argue the constraint erases biological sex as a meaningful category; they bear costs in advocacy legitimacy as the constraint's legal establishment expands. They can relocate to jurisdictions with sex-biology readings, organize alternative institutions, or maintain parallel organizing around biological sex. Their exit is constrained by legal landscape but not identity-locked.
narrative_ontology:constraint_stakeholder(woman_category__gender_identity_reading, sex_essentialist_advocates, payer,
    organized, biographical, mobile, national).

% Would argue the constraint undermines sex-based legal protections and women-only organizing necessary for liberation. They are not at the negotiating table when institutional policies shift; their presence would fundamentally reframe the constraint's justification. Some feminist actors support the constraint; others contest it; the constraint definition excludes the contestation from official adjudication.
narrative_ontology:constraint_stakeholder(woman_category__gender_identity_reading, feminist_coalitions_contesting, excluded,
    organized, biographical, mobile, national).

% Adjudicates the constraint through legislation and case law. Courts and legislatures in different jurisdictions have adopted or rejected the gender-identity reading; in jurisdictions that adopt it, legal authority enforces it through document revision, antidiscrimination law application, and institutional mandate. Their power is substantial but distributed across jurisdictions.
narrative_ontology:constraint_stakeholder(woman_category__gender_identity_reading, legal_authority, agenda_setter,
    institutional, generational, analytical, national).

% Are partially included under the gender-identity reading (if they identify as women, they are categorized as women) but face a different structural problem: the constraint remains binary (man/woman) even as it loosens the biology requirement. Intersex people who do not fit binary gender identity are excluded from explicit accommodation; the constraint's benefit to them is incidental to gender-identity inclusion, not structural.
narrative_ontology:constraint_stakeholder(woman_category__gender_identity_reading, intersex_people, excluded,
    powerless, biographical, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(woman_category__gender_identity_reading, transgender_women).
narrative_ontology:fixing_cost_class(woman_category__gender_identity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Operates a stable, legally codified definition of 'woman' for administrative, legal, and institutional purposes. Gender identity provides a workable boundary that avoids the administrative costs of biological verification (chromosomal testing, anatomical examination, medical certification) and instead relies on self-identification—reducing friction in legal document processing, institutional access, and identity verification. Solves the coordination problem of how institutions (courts, agencies, employers, schools) determine who counts as 'woman' for legal purposes.
% TRANSFER_FUNCTION: Moves legal recognition and institutional access from a sex-biology-defined set (people with female reproductive anatomy and associated characteristics) to a gender-identity-defined set (people who internally identify as women). The beneficiary group expands to include transgender women and gender-nonconforming people who identify as women; cisgender women and sex-segregated-space users bear costs through loss of exclusive category membership and contested access to single-sex services, spaces, and opportunities. In athletics, the transfer is directional: female athletes lose record-holding status and podium positions to transgender women with recent transition histories; transgender women gain competitive access.
% ABSENT_VOICES: Sex-essentialist advocates, biological-sex-focused feminists, and intersex people without binary gender identity are structurally excluded from the adjudication process. Feminist actors who argue the constraint erodes sex-based legal protections and women-only organizing are not present at policy tables when institutional definitions shift. Intersex people whose gender identity is non-binary or ambiguous are invisible in the gender-identity vs. sex-biology dispute, which assumes binary gender categories. Their exclusion makes the constraint appear to comprehensively resolve the woman-category question when in fact it preserves and deepens binary assumptions while shifting the binary's foundation.
% DISAPPEARANCE_RATIONALE: If the gender-identity reading vanished and was replaced by the sex-biology reading, legal recognition of transgender women would be revoked, identity documents would revert to assigned sex at birth, access to sex-segregated institutional spaces would reorganize around biological sex, and transgender people would lose the legal standing they have gained. Millions of people's legal status, institutional access, and social recognition would be restructured. The constraint's presence makes concrete differences to how transgender people navigate legal systems, institutional access, and social participation.
% FOUNDING_PROBLEM: For decades before this reading gained legal status, transgender people and gender-nonconforming people had no legal recognition of their gender identity; they were categorized in legal documents and institutional access by assigned sex at birth, creating cognitive dissonance, institutional friction (deadnaming in official processes, forced use of sex-assigned bathrooms and facilities), and legal inability to live according to their identity. Medical and psychological research established that gender identity is a stable, internally-felt property and that legal non-recognition causes documented harms (depression, anxiety, elevated suicide risk).
% FOUNDING_PROBLEM_CORROBORATION: Medical and psychological evidence (American Psychological Association, World Health Organization, peer-reviewed studies) documents that gender identity is a stable property and that legal non-recognition harms mental health. Transgender advocates and rights organizations attest the problem is live and pressing. Sex-essentialist critics acknowledge that some people experience dysphoria between identity and assigned sex, but contest whether the solution is redefining institutional categories or creating separate recognition pathways (e.g., sex-neutral identities, third categories, or parallel-but-equal systems). Feminist critics argue the problem (transgender identity recognition) is legitimate but distinct from the solution offered (redefining 'woman'), and that the solution undermines sex-based organizing and legal protections that serve cisgender women. No neutral arbiter has adjudicated the contest; different jurisdictions have adopted different readings based on political coalitions rather than empirical or philosophical resolution.
narrative_ontology:disappearance_verdict(woman_category__gender_identity_reading, world_rearranges).
narrative_ontology:founding_problem_status(woman_category__gender_identity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(woman_category__gender_identity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(woman_category__gender_identity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(woman_category__gender_identity_reading, 0.62, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness climbs from 0.38 (early in interval, when the reading is contested and unenforced in most jurisdictions) to 0.62 (as legal adoption spreads and institutional enforcement tightens). Theater ratio rises from 0.25 to 0.41 as institutional compliance becomes more performative: early enforcement focuses on genuine accommodation (medical transition, name/identity documents); later enforcement emphasizes institutional boundary management and contestation suppression. Suppression remains high throughout because the constraint depends on active institutional power to suppress sex-essentialist contestation and override prior biology-based practice. Accessibility collapse (0.48) is moderate: alternatives (sex-biology reading, intersex accommodation reading) remain available in law and advocacy; they have not collapsed entirely. Resistance (0.72) is high because the constraint meets substantial organized opposition from sex-essentialist advocates and feminist critics; the constraint persists not by universal agreement but by institutional mandates in some jurisdictions and social pressure in others. The measurements share one grid (every metric is authored at every time point) so temporal analysis can track the constraint's lifecycle: the plateau in theater and suppression after t=25 suggests the constraint has reached enforced equilibrium in adopting jurisdictions, with resistance continuing at high steady state.
 *
 * PERSPECTIVAL GAP:
 *   The constraint produces fundamentally different perceived types across seats. From the transgender-women beneficiary seat, it appears as a genuine rope (coordination that solves a real recognition problem). From the sex-segregated-space-user seat, it appears as a tangled-rope or snare (coordination narrative covering asymmetric access extraction). From the sex-essentialist-advocate seat, it appears as a snare (pure extraction of institutional power and social legitimacy, with coordination narrative as cover). From the institutional-administrator seat, it appears as a scaffold with enforcement burden (transitional policy under legal mandate). The engine computes each seat's classification from power, exit options, beneficiary/victim status, and institutional context; the wide divergence is the signal that the constraint sits at a deep boundary contest.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality diverges sharply across seats. Transgender women and non-binary beneficiaries have d near 0.0 (beneficiary end): the constraint subsidizes them with recognition and access; their time horizon is biographical; their exit is identity-locked (denying their identity is not a viable alternative). Cisgender women have d near 0.5-0.6 (asymmetric): they retain woman-category membership but lose exclusivity; some perceive net benefit (more inclusive movement), others perceive net cost (erosion of sex-based organizing); their exit is constrained (they cannot simply leave the woman category without denying their sex and gender, which is not realistic). Female athletes have d near 0.7 (target end) in competitive sports domains: they bear concrete, measurable costs (shifted podium positions, reclassified records); they benefit from broader gender inclusion ideologically but pay materially. Sex-segregated-space users have d near 0.65 (target end): they lose exclusivity and must navigate changed spaces; their material costs are real (bathroom redesign, privacy loss, safety recalculation). Sex-essentialist advocates and feminist critics have d near 1.0 (full target): the constraint's establishment actively suppresses their reading and delegitimizes their institutional presence; they bear enforcement costs in the form of social pressure, institutional exclusion, and legal disadvantage in adjudication.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's claimed type is tangled-rope: it coordinates legal gender recognition (genuine coordination function — no viable alternative legal definition that satisfies all parties exists) while extracting institutional access from sex-segregated-space users and some cisgender women (asymmetric extraction). The asymmetry is structural: the beneficiary group (transgender women) was previously legally invisible and has strong identity-lock justifying their inclusion; the payer group (sex-segregated-space users) were previously invisible beneficiaries of exclusivity and now bear contestation costs. Mandatrophy arises if the constraint's mandate (recognize gender identity as the woman category) outlives the founding problem (transgender people lack legal recognition). The founding-problem-status measurement is contested: beneficiaries and their allies argue the problem is live and pressing; sex-essentialist critics argue the problem (lack of gender-identity recognition) is distinct from the solution offered (redefining woman category rather than creating a separate trans-identity category or intersex-inclusive category), and that solving that distinct problem has created new harms (loss of sex-based legal protections, erosion of women-only spaces). This contestation is itself structural and may not resolve, making the mandatrophy verdict permanently contested rather than definitively determined.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sex_vs_gender_identity_commensurability,
    'Are ''sex'' (biological anatomy/chromosomes/reproductive function) and ''gender identity'' (internal sense of gender) measuring the same underlying category or fundamentally incommensurable properties?',
    'Philosophical and empirical analysis: neuroscientific investigation of gender identity''s neural substrates; cross-cultural comparison of gender-category definitions; logical analysis of whether ''woman'' can coherently refer to both a biological state and a subjective psychological state simultaneously.',
    'If incommensurable, the readings cannot coexist in a single definition and one must foreclose the other. If commensurable (e.g., both referring to aspects of a single gender property), multiple definitions can coexist by choosing which property takes precedence. This determines whether the kernel-reading structure is a permanent contest or a resolvable empirical question.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sex_vs_gender_identity_commensurability, conceptual, 'Whether sex and gender identity are the same category, overlapping categories, or incommensurable dimensions.').

omega_variable(
    sex_based_protections_vs_gender_inclusion,
    'Can legal protections grounded in ''sex'' (pregnancy discrimination, sexual harassment, reproductive rights) be maintained while the woman category is defined by gender identity rather than sex?',
    'Legal analysis and empirical case law: jurisdictions that adopt the gender-identity reading and then attempt to apply sex-based protections; examination of whether sex-specific legal rights (pregnancy-related benefits, reproductive autonomy) can be preserved if ''woman'' no longer implies biological sex.',
    'If the protections can be preserved independently, the constraint preserves sex-based rights alongside gender-identity inclusion (tangled-rope classification holds). If the protections erode when the category shifts, the constraint extracts from those who depend on sex-based legal recognition (classification shifts toward snare). This is the structural question that determines whether sex-essentialist critics'' fears are empirically grounded.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sex_based_protections_vs_gender_inclusion, empirical, 'Whether sex-based legal protections can coexist with gender-identity category definitions.').

omega_variable(
    sports_fairness_constraint_independence,
    'Is the extraction observed in female athletics due to the gender-identity definition of ''woman'' itself, or due to incomplete athletic-eligibility criteria (hormone levels, transition duration, performance metrics) that could be refined while keeping the gender-identity definition?',
    'Sports-science research on transgender athlete performance; monitoring of fairness outcomes across different eligibility criteria (current IOC standards, hormone-threshold models, other proposed metrics); comparison of extraction costs under different criteria.',
    'If the extraction is due to incomplete criteria rather than the definition itself, refining criteria could maintain the gender-identity reading while reducing extraction from female athletes. If extraction is intrinsic to the definition (e.g., some residual performance advantage is inseparable from the identity recognition), the constraint''s cost class in sports remains high. This determines whether the tangled-rope classification can be improved through policy refinement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sports_fairness_constraint_independence, empirical, 'Whether sports-domain extraction is due to the gender-identity definition or to incomplete athletic-eligibility criteria.').

omega_variable(
    suppression_mechanism_structural_vs_internalized,
    'Is the measured suppression (0.58) structural (legal/institutional barriers to sex-essentialist advocacy and organizing) or internalized (advocates have internalized the reading''s legitimacy or suppressed their own doubts about it)?',
    'Post-suppression trajectory: if sex-essentialist advocates relocate to non-adopting jurisdictions or form parallel institutions and suppression decreases, the suppression was structural. If suppression persists even in non-enforcing contexts (e.g., self-censorship, identity crisis among former advocates), the suppression has become internalized.',
    'If structural, the suppression is reversible by institutional change; the constraint is enforced by external power. If internalized, the constraint has become self-perpetuating even after institutional enforcement relaxes; exit costs for advocates increase. This affects whether the constraint qualifies as active-enforcement dependent or has transitioned to cultural/identity-level embedding.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_vs_internalized, empirical, 'Whether measured suppression reflects external institutional enforcement or internalized identity constraints.').

omega_variable(
    intersex_categorical_visibility,
    'Does the gender-identity reading adequately address the categorical situation of intersex people who do not fit binary gender identity, or does it leave them in a secondary accommodation position while binary readings (gender-identity vs. sex-biology) dominate the kernel contest?',
    'Empirical audit: examine whether intersex people whose gender identity is non-binary or ambiguous are served by current gender-identity institutional policies, or whether they remain forced into binary choices. Legal and policy analysis of whether non-binary gender-identity options are available with equal institutional recognition.',
    'If intersex people remain structurally excluded, the constraint does not fully resolve the woman-category boundary question; it redistributes visibility without achieving comprehensive inclusion. If they are adequately served, the reading''s coordination scope is broader than simple transgender inclusion.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(intersex_categorical_visibility, empirical, 'Whether the gender-identity reading addresses intersex people''s categorical situation or leaves them partially invisible.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(woman_category__gender_identity_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(woma_tr_t0, woman_category__gender_identity_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement_basis(woma_tr_t0, observed).
narrative_ontology:measurement(woma_tr_t5, woman_category__gender_identity_reading, theater_ratio, 5, 0.3).
narrative_ontology:measurement_basis(woma_tr_t5, observed).
narrative_ontology:measurement(woma_tr_t10, woman_category__gender_identity_reading, theater_ratio, 10, 0.33).
narrative_ontology:measurement_basis(woma_tr_t10, observed).
narrative_ontology:measurement(woma_tr_t15, woman_category__gender_identity_reading, theater_ratio, 15, 0.37).
narrative_ontology:measurement_basis(woma_tr_t15, observed).
narrative_ontology:measurement(woma_tr_t20, woman_category__gender_identity_reading, theater_ratio, 20, 0.4).
narrative_ontology:measurement_basis(woma_tr_t20, observed).
narrative_ontology:measurement(woma_tr_t25, woman_category__gender_identity_reading, theater_ratio, 25, 0.41).
narrative_ontology:measurement_basis(woma_tr_t25, observed).
narrative_ontology:measurement(woma_tr_t30, woman_category__gender_identity_reading, theater_ratio, 30, 0.41).
narrative_ontology:measurement_basis(woma_tr_t30, observed).

% Extraction over time
narrative_ontology:measurement(woma_be_t0, woman_category__gender_identity_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement_basis(woma_be_t0, observed).
narrative_ontology:measurement(woma_be_t5, woman_category__gender_identity_reading, base_extractiveness, 5, 0.45).
narrative_ontology:measurement_basis(woma_be_t5, observed).
narrative_ontology:measurement(woma_be_t10, woman_category__gender_identity_reading, base_extractiveness, 10, 0.52).
narrative_ontology:measurement_basis(woma_be_t10, observed).
narrative_ontology:measurement(woma_be_t15, woman_category__gender_identity_reading, base_extractiveness, 15, 0.58).
narrative_ontology:measurement_basis(woma_be_t15, observed).
narrative_ontology:measurement(woma_be_t20, woman_category__gender_identity_reading, base_extractiveness, 20, 0.61).
narrative_ontology:measurement_basis(woma_be_t20, observed).
narrative_ontology:measurement(woma_be_t25, woman_category__gender_identity_reading, base_extractiveness, 25, 0.62).
narrative_ontology:measurement_basis(woma_be_t25, observed).
narrative_ontology:measurement(woma_be_t30, woman_category__gender_identity_reading, base_extractiveness, 30, 0.62).
narrative_ontology:measurement_basis(woma_be_t30, observed).

% Suppression requirement over time
narrative_ontology:measurement(woma_su_t0, woman_category__gender_identity_reading, suppression_requirement, 0, 0.42).
narrative_ontology:measurement_basis(woma_su_t0, observed).
narrative_ontology:measurement(woma_su_t5, woman_category__gender_identity_reading, suppression_requirement, 5, 0.48).
narrative_ontology:measurement_basis(woma_su_t5, observed).
narrative_ontology:measurement(woma_su_t10, woman_category__gender_identity_reading, suppression_requirement, 10, 0.52).
narrative_ontology:measurement_basis(woma_su_t10, observed).
narrative_ontology:measurement(woma_su_t15, woman_category__gender_identity_reading, suppression_requirement, 15, 0.55).
narrative_ontology:measurement_basis(woma_su_t15, observed).
narrative_ontology:measurement(woma_su_t20, woman_category__gender_identity_reading, suppression_requirement, 20, 0.57).
narrative_ontology:measurement_basis(woma_su_t20, observed).
narrative_ontology:measurement(woma_su_t25, woman_category__gender_identity_reading, suppression_requirement, 25, 0.58).
narrative_ontology:measurement_basis(woma_su_t25, observed).
narrative_ontology:measurement(woma_su_t30, woman_category__gender_identity_reading, suppression_requirement, 30, 0.58).
narrative_ontology:measurement_basis(woma_su_t30, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(woman_category__gender_identity_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(woman_category__gender_identity_reading, 0.12).
narrative_ontology:affects_constraint(woman_category__gender_identity_reading, woman_category__sex_biology_reading).
narrative_ontology:affects_constraint(woman_category__gender_identity_reading, woman_category__intersex_accommodation_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the contested 'woman_category' kernel. The sibling readings (sex_biology_reading and intersex_accommodation_reading) are separate constraint stories with their own ε values, victim sets, and classifications. The gender_identity_reading (this story) exhibits moderate-to-high extractiveness in contested domains (sports, sex-segregated spaces) because it imposes costs on people whose legal standing relied on sex-based exclusivity. The sex-biology reading would exhibit different extractiveness characteristics (lower toward transgender women, higher toward broader inclusion) because it grounds the category in biological properties. These are not measurement artifacts or observable-dependent evaluations of a single constraint—they are structurally distinct claims with different empirical referents, beneficiary structures, and persistence mechanisms. The three stories form a kernel family linked by their shared governance of the 'woman' category definition; the readings coexist as live positions in institutional and social dispute.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(woman_category__gender_identity_reading, powerless, 0.15).
constraint_indexing:directionality_override(woman_category__gender_identity_reading, organized, 0.58).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
