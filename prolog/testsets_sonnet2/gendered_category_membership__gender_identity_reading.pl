% ============================================================================
% CONSTRAINT STORY: gendered_category_membership__gender_identity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
    constraint_indexing:constraint_classification/3,
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
 *   human_readable: Gender Category Membership via Self-Declared Identity
 *   domain: social_ontology/political_philosophy/bioethics
 *
 * SUMMARY:
 *   This story instantiates the gender-identity reading of the contested
 *   'gendered category membership' kernel: category membership (woman, man,
 *   or neither) is grounded in subjective self-declaration, not
 *   birth-assigned biological markers (the sibling biological_sex_reading) or
 *   sustained social performance recognized by others (the sibling
 *   social_role_reading). Under this reading, trans women are included in the
 *   category 'woman' by self-ID, sex-segregated spaces and categories are
 *   reorganized as gender-segregated, and institutional adoption of the
 *   criterion has moderate gatekeeping and dispute-resolution costs that fall
 *   differentially on cis women who use sex-segregated services, female
 *   athletes in contested competitive categories, and advocates for sex-based
 *   legal protections who are institutionally disfavored for contesting the
 *   criterion. This is one reading only; the biological_sex_reading and
 *   social_role_reading are separate constraints with their own ε,
 *   beneficiary/victim structures, and classifications, linked here via
 *   network.affects_constraints.
 *
 * KEY AGENTS:
 *   - trans_women: beneficiary (moderate/identity_locked) — gains category recognition under self-ID
 *   - cis_women_in_sex_segregated_spaces: payer (powerless/trapped) — bears reorganization of space eligibility
 *   - gender_identity_advocacy_organizations: agenda_setter (organized/mobile) — administers and advances the criterion institutionally
 *   - gender_critical_feminists: payer/excluded (moderate/constrained) — bears institutional disfavor for contesting the criterion
 *   - courts_and_legislatures: observer (institutional/analytical) — adjudicates which reading holds legal force
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gendered_category_membership__gender_identity_reading, 0.42).
domain_priors:suppression_score(gendered_category_membership__gender_identity_reading, 0.38).
domain_priors:theater_ratio(gendered_category_membership__gender_identity_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gendered_category_membership__gender_identity_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(gendered_category_membership__gender_identity_reading, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(gendered_category_membership__gender_identity_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gendered_category_membership__gender_identity_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(gendered_category_membership__gender_identity_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gendered_category_membership__gender_identity_reading, tangled_rope).
narrative_ontology:human_readable(gendered_category_membership__gender_identity_reading, "Gender Category Membership via Self-Declared Identity").
narrative_ontology:topic_domain(gendered_category_membership__gender_identity_reading, "social_ontology/political_philosophy/bioethics").

domain_priors:requires_active_enforcement(gendered_category_membership__gender_identity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gendered_category_membership__gender_identity_reading, '841be35a-0ee9-4f65-9956-fb62337ac8b6').
narrative_ontology:cs_kernel_codification('841be35a-0ee9-4f65-9956-fb62337ac8b6', distributed).
narrative_ontology:cs_authority_grounding('841be35a-0ee9-4f65-9956-fb62337ac8b6', distributed).
narrative_ontology:cs_reading_relation('841be35a-0ee9-4f65-9956-fb62337ac8b6', gendered_category_membership__biological_sex_reading, forecloses).
narrative_ontology:cs_reading_relation('841be35a-0ee9-4f65-9956-fb62337ac8b6', gendered_category_membership__social_role_reading, coexists_with).
narrative_ontology:cs_axiom('841be35a-0ee9-4f65-9956-fb62337ac8b6', foundational, subjective_identity_is_sufficient_for_category_membership).
narrative_ontology:cs_axiom_status(subjective_identity_is_sufficient_for_category_membership, holdable).
narrative_ontology:cs_axiom_grounding('841be35a-0ee9-4f65-9956-fb62337ac8b6', subjective_identity_is_sufficient_for_category_membership, deontological).
narrative_ontology:cs_axiom('841be35a-0ee9-4f65-9956-fb62337ac8b6', secondary, biological_markers_are_not_necessary_for_category_membership).
narrative_ontology:cs_axiom_status(biological_markers_are_not_necessary_for_category_membership, holdable).
narrative_ontology:cs_axiom_grounding('841be35a-0ee9-4f65-9956-fb62337ac8b6', biological_markers_are_not_necessary_for_category_membership, conventional).
narrative_ontology:cs_reference_frame('841be35a-0ee9-4f65-9956-fb62337ac8b6', self_declared_identity_as_sufficient_criterion).
narrative_ontology:cs_drift_state('841be35a-0ee9-4f65-9956-fb62337ac8b6', post_institutional_adoption_contestation, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('841be35a-0ee9-4f65-9956-fb62337ac8b6', '').
narrative_ontology:cs_kernel_id(gendered_category_membership__gender_identity_reading, gendered_category_membership).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gendered_category_membership__gender_identity_reading, trans_women).
narrative_ontology:constraint_beneficiary(gendered_category_membership__gender_identity_reading, trans_men).
narrative_ontology:constraint_beneficiary(gendered_category_membership__gender_identity_reading, nonbinary_individuals).
narrative_ontology:constraint_beneficiary(gendered_category_membership__gender_identity_reading, gender_identity_advocacy_organizations).
narrative_ontology:constraint_victim(gendered_category_membership__gender_identity_reading, cis_women_in_sex_segregated_spaces).
narrative_ontology:constraint_victim(gendered_category_membership__gender_identity_reading, female_athletes_in_contested_categories).
narrative_ontology:constraint_victim(gendered_category_membership__gender_identity_reading, gender_critical_feminists).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(gendered_category_membership__gender_identity_reading, employers_and_service_providers).
narrative_ontology:constraint_vindicates(gendered_category_membership__gender_identity_reading, gender_identity_is_the_operative_criterion_for_social_category_membership).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Seek recognition and access to women's spaces, services, and categories on the basis of self-declared gender identity rather than birth-assigned sex. Access to accurate category membership materially affects safety, dignity, and social participation; for this seat exit from the identity is not experienced as available or desirable — it is who they are, not a strategic position.
narrative_ontology:constraint_stakeholder(gendered_category_membership__gender_identity_reading, trans_women, beneficiary,
    moderate, biographical, identity_locked, national).

% Seek recognition in the men's category on the same self-ID basis; less central to the specific contested spaces (single-sex facilities, women's sport) but structurally covered by the same category-membership rule and its legitimacy.
narrative_ontology:constraint_stakeholder(gendered_category_membership__gender_identity_reading, trans_men, beneficiary,
    moderate, biographical, identity_locked, national).

% Seek recognition outside the binary altogether; the self-ID framework is the only available mechanism by which their category claims can be legally or socially registered at all, since biological-sex or social-role readings offer them no category.
narrative_ontology:constraint_stakeholder(gendered_category_membership__gender_identity_reading, nonbinary_individuals, beneficiary,
    powerless, biographical, identity_locked, national).

% Advance self-ID as the operative legal and institutional standard through litigation, policy advocacy, and professional-body guideline capture. Their organizational mission and funding are tied to the reading's institutional adoption; they administer definitions used by employers, health systems, and public bodies.
narrative_ontology:constraint_stakeholder(gendered_category_membership__gender_identity_reading, gender_identity_advocacy_organizations, agenda_setter,
    organized, generational, mobile, national).

% Use shelters, prisons, changing rooms, and other single-sex spaces whose original rationale (privacy, safety from male-pattern violence) was organized around biological sex. Under this reading, admission to those spaces is reorganized around declared gender identity; raising objections is at increased risk of being characterized as exclusionary or bigoted, which suppresses articulation of the cost even where it is felt directly.
narrative_ontology:constraint_stakeholder(gendered_category_membership__gender_identity_reading, cis_women_in_sex_segregated_spaces, payer,
    powerless, biographical, trapped, national).

% Compete in categories whose eligibility criteria under this reading shift from sex-linked physiological measures to declared identity. Where physiological advantages from male puberty persist, they bear competitive costs (lost placements, scholarships, records) that are difficult to contest without being cast as opposing trans inclusion generally.
narrative_ontology:constraint_stakeholder(gendered_category_membership__gender_identity_reading, female_athletes_in_contested_categories, payer,
    moderate, biographical, constrained, national).

% Hold that sex is a material, immutable category relevant to specific protections and argue self-ID displaces sex-based analysis needed for those protections. Under this reading's institutional adoption, their position is frequently treated as illegitimate speech in employment, platform moderation, and professional bodies, restricting their capacity to advocate for the sibling readings.
narrative_ontology:constraint_stakeholder(gendered_category_membership__gender_identity_reading, gender_critical_feminists, payer,
    moderate, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(gendered_category_membership__gender_identity_reading, gender_critical_feminists, excluded).

% Must adopt and enforce self-ID-based category rules in HR policy, facilities access, and service provision to comply with guidance and avoid discrimination liability; bear compliance and dispute-resolution costs regardless of which internal constituency's view they credit.
narrative_ontology:constraint_stakeholder(gendered_category_membership__gender_identity_reading, employers_and_service_providers, agenda_setter,
    institutional, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(gendered_category_membership__gender_identity_reading, employers_and_service_providers, payer).

% Adjudicate disputes between the readings — whether self-declared identity, biological sex, or social role is the operative legal criterion for specific statutory categories (single-sex services exemptions, sports eligibility, prison placement). Their rulings shift which reading holds institutional force in a given domain.
narrative_ontology:constraint_stakeholder(gendered_category_membership__gender_identity_reading, courts_and_legislatures, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(gendered_category_membership__gender_identity_reading, diffuse).
narrative_ontology:fixing_cost_class(gendered_category_membership__gender_identity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single, administrable criterion (self-declared identity) for allocating people to gendered categories in law, sport, and services, avoiding the intrusive verification (medical, chromosomal, or performative) that the sibling readings would otherwise require of every individual.
% TRANSFER_FUNCTION: Moves categorical membership and its attendant access (to spaces, competitive categories, and social recognition) from a sex-linked or role-linked basis toward an identity-declared basis; correspondingly moves the costs of contested edge cases (safety concerns in single-sex spaces, competitive fairness in sport) onto those whose interests were organized around the sex-linked criterion.
% ABSENT_VOICES: Detransitioners and intersex individuals whose category claims do not map cleanly onto either 'stable self-declared identity' or 'biological sex' are rarely centered in either advocacy or critique; survivors of sex-based violence whose service needs were organized around biological-sex segregation are frequently spoken for rather than directly consulted in policy design under this reading.
% DISAPPEARANCE_RATIONALE: Beneficiary seats hold that reverting to a sex- or role-based criterion would strip legal recognition and access currently secured under self-ID and materially harm trans and nonbinary people. Payer seats hold that removing self-ID as the operative criterion would restore verification standards they view as necessary for the specific protections at stake (single-sex spaces, sport). Because the parties dispute which world is the baseline being defended, the disappearance verdict cannot be settled from inside either seat.
% FOUNDING_PROBLEM: Historically, category membership required medical or legal gatekeeping (diagnosis, surgery, or lengthy administrative process) before recognition was granted, which excluded many trans and nonbinary people from any category recognition at all and imposed significant burdens even on those who could pursue it.
% FOUNDING_PROBLEM_CORROBORATION: Trans and nonbinary advocacy organizations and allied legal scholars attest the gatekeeping problem was real and remains partially live wherever medicalized verification persists. Gender-critical feminist organizations and some sex-based-rights legal scholars, positioned outside the beneficiary set, attest that a distinct problem — the loss of a verifiable sex-based criterion for specific protections — has been created by the reading's institutional adoption, and dispute that self-ID resolves the original problem without generating this new one. No source entirely outside all contesting parties has offered an uncontested account of which problem is primary.
narrative_ontology:disappearance_verdict(gendered_category_membership__gender_identity_reading, contested).
narrative_ontology:founding_problem_status(gendered_category_membership__gender_identity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gendered_category_membership__gender_identity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(gendered_category_membership__gender_identity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(gendered_category_membership__gender_identity_reading, 0.42, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness is moderate (0.42 at interval end) rather than high: the coordination function (a single administrable criterion avoiding intrusive verification) is genuine and real, but the reallocation of eligibility in specific high-stakes domains (single-sex services, sport) imposes costs on a specific, identifiable payer set. Suppression (0.38) captures the institutional and reputational cost of contesting the criterion once adopted by professional bodies and employers, not overt legal coercion. Accessibility collapse is moderate (0.35) — alternative readings remain live in public discourse and some jurisdictions, unlike a fully settled arrangement. Resistance is high (0.68) reflecting substantial organized contestation from multiple directions (sex-based-rights advocates, some sport governance bodies, some legislatures) — this is an actively contested kernel reading, not a settled consensus.
 *
 * DIRECTIONALITY LOGIC:
 *   Trans and nonbinary beneficiaries sit near the beneficiary end of directionality: the criterion directly extends their category recognition and access. Their exit_options are coded identity_locked, not mobile — this is not a strategic position they could abandon at low cost; the self-ID criterion's legitimacy is bound up with their own identity claims. Cis women in sex-segregated spaces and female athletes in contested categories sit toward the target end: the reorganization is imposed on their prior arrangement rather than chosen by them, and their exit options are trapped/constrained because the spaces and competitive categories are not optional infrastructure for them. Gender-critical feminists are coded payer with secondary excluded — they bear reputational and institutional costs for contesting the criterion and are frequently not treated as legitimate parties to the debate in institutional settings, which is a distinct cost from the direct access reallocation.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — exclusionary medical gatekeeping preventing any category recognition for trans and nonbinary people — was real and, per corroboration, is only partially resolved (medicalized gatekeeping persists in many jurisdictions). This story does not classify the constraint as pure extraction: the coordination function is genuine, which is why it is authored as tangled_rope rather than snare. But the founding-problem status is contested rather than resolved, and the reading's institutional adoption has generated a distinct, non-trivial cost structure for the payer seats that the founding narrative does not address — classifying this as tangled_rope rather than a clean rope prevents both mislabeling the entire arrangement as pure extraction (erasing the real coordination gain for beneficiaries) and mislabeling it as costless coordination (erasing the real, differential costs to specific payer seats).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    self_id_verification_gate,
    'Should self-declaration alone be sufficient for category membership in all institutional contexts, or should some contexts (e.g., competitive sport, custodial facilities) require additional verification without abandoning the self-ID criterion as the general rule?',
    'Comparative policy analysis across jurisdictions that have adopted graduated verification requirements versus pure self-ID, tracking outcomes for safety incidents, competitive fairness disputes, and access barriers for trans and nonbinary people.',
    'If graduated verification in high-stakes contexts resolves most of the payer-seat costs without materially burdening beneficiary-seat access, the tangled_rope classification''s extraction component would substantially shrink in those specific domains while the general coordination function is preserved elsewhere.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(self_id_verification_gate, conceptual, 'Whether context-specific verification thresholds can decouple the coordination benefit from the extraction cost.').

omega_variable(
    kernel_framing_selection,
    'Is ''gendered category membership'' genuinely best decomposed into three coexisting readings (biological sex, gender identity, social role), or does one reading properly subsume or ground the others (e.g., is social role recognition actually downstream of whichever criterion — sex or identity — legal and social institutions adopt as primary)?',
    'Track whether jurisdictions and institutions that formally adopt the gender_identity_reading as the legal criterion subsequently see social_role_reading outcomes (recognition in daily social interaction) converge with or diverge from the legal criterion; divergence would support the readings'' independence, convergence would support a grounding relationship.',
    'If social_role_reading is downstream of legal/institutional adoption of gender_identity_reading rather than a fully independent axis, the reading_relations edge from this constraint to social_role_reading should be reclassified from coexists_with toward influences with greater confidence, and the family''s structure would need revision.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_framing_selection, conceptual, 'Whether the three-reading decomposition is the correct joint-carving of the kernel or whether one reading structurally grounds another.').

omega_variable(
    cis_women_framing_asymmetry,
    'Does this reading''s institutional adoption structurally position cis women who seek to maintain sex-based space allocation as perpetrators of exclusion, and if so, is that framing a necessary implication of the reading or a contingent rhetorical overlay separable from it?',
    'Analyze whether jurisdictions/institutions that formally adopt self-ID as the legal criterion for category membership can and do simultaneously preserve narrowly-tailored sex-based exemptions (e.g., single-sex domestic violence shelters) without those exemptions being characterized as illegitimate discrimination under the same institutional framework.',
    'If narrowly-tailored exemptions can coexist with the general self-ID criterion without the exemption-holders being institutionally framed as bigoted, the suppression metric for the cis_women_in_sex_segregated_spaces seat would be substantially overstated in jurisdictions with such carve-outs; if exemptions are consistently treated as illegitimate wherever the reading is formally adopted, the current suppression value is accurate or understated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cis_women_framing_asymmetry, empirical, 'Whether the perpetrator-framing of resistant cis women is intrinsic to the reading''s institutional logic or a separable contingent overlay.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gendered_category_membership__gender_identity_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gend_tr_t0, gendered_category_membership__gender_identity_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(gend_tr_t4, gendered_category_membership__gender_identity_reading, theater_ratio, 4, 0.13).
narrative_ontology:measurement(gend_tr_t8, gendered_category_membership__gender_identity_reading, theater_ratio, 8, 0.16).
narrative_ontology:measurement(gend_tr_t12, gendered_category_membership__gender_identity_reading, theater_ratio, 12, 0.18).
narrative_ontology:measurement(gend_tr_t16, gendered_category_membership__gender_identity_reading, theater_ratio, 16, 0.2).
narrative_ontology:measurement(gend_tr_t20, gendered_category_membership__gender_identity_reading, theater_ratio, 20, 0.22).

% Extraction over time
narrative_ontology:measurement(gend_be_t0, gendered_category_membership__gender_identity_reading, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(gend_be_t4, gendered_category_membership__gender_identity_reading, base_extractiveness, 4, 0.24).
narrative_ontology:measurement(gend_be_t8, gendered_category_membership__gender_identity_reading, base_extractiveness, 8, 0.31).
narrative_ontology:measurement(gend_be_t12, gendered_category_membership__gender_identity_reading, base_extractiveness, 12, 0.36).
narrative_ontology:measurement(gend_be_t16, gendered_category_membership__gender_identity_reading, base_extractiveness, 16, 0.4).
narrative_ontology:measurement(gend_be_t20, gendered_category_membership__gender_identity_reading, base_extractiveness, 20, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(gend_su_t0, gendered_category_membership__gender_identity_reading, suppression_requirement, 0, 0.2).
narrative_ontology:measurement(gend_su_t4, gendered_category_membership__gender_identity_reading, suppression_requirement, 4, 0.26).
narrative_ontology:measurement(gend_su_t8, gendered_category_membership__gender_identity_reading, suppression_requirement, 8, 0.31).
narrative_ontology:measurement(gend_su_t12, gendered_category_membership__gender_identity_reading, suppression_requirement, 12, 0.34).
narrative_ontology:measurement(gend_su_t16, gendered_category_membership__gender_identity_reading, suppression_requirement, 16, 0.36).
narrative_ontology:measurement(gend_su_t20, gendered_category_membership__gender_identity_reading, suppression_requirement, 20, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(gendered_category_membership__gender_identity_reading, gendered_category_membership__biological_sex_reading).
narrative_ontology:affects_constraint(gendered_category_membership__gender_identity_reading, gendered_category_membership__social_role_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the gendered_category_membership kernel, each authored as a separate constraint story per the ε-invariance principle. biological_sex_reading grounds membership in immutable birth-assigned markers and would author near-zero ε for the criterion itself (contested only at its edges, e.g. intersex conditions) while authoring high ε for this reading's displacement of sex-based protections from its own vantage. social_role_reading grounds membership in sustained social performance and recognition, producing a different beneficiary/victim structure (e.g., long-term transitioners recognized socially prior to any legal self-ID regime versus those who self-declare without sustained social role change). All three readings share the same underlying kernel contest over who counts as a woman/man/neither, but each is a structurally distinct constraint with its own ε, stakeholders, and classification — they are linked, not merged.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
