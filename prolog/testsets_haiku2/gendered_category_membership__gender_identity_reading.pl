% ============================================================================
% CONSTRAINT STORY: gendered_category_membership__gender_identity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
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
 *   human_readable: Gender Category Membership via Self-Identification
 *   domain: social/ontological/bioethical
 *
 * SUMMARY:
 *   This constraint story instantiates ONE reading of a contested kernel:
 *   gendered_category_membership. The reading positions gender identity
 *   (subjective self-understanding) as the ground of category membership,
 *   making 'woman' a gender category to which trans women have access via
 *   self-declaration. This reading competes with two sibling readings:
 *   biological_sex_reading (category membership grounded in immutable
 *   biological markers) and social_role_reading (category membership grounded
 *   in sustained social performance and recognition). The kernel contest is
 *   NOT about whether trans people deserve recognition and respect—all
 *   readings affirm that—but about WHETHER THE SAME CATEGORY ('woman') is the
 *   mechanism of recognition, and WHAT HAPPENS to sex-based interests
 *   (reproductive health, sexual violence prevention, sports fairness) when
 *   sex categories are replaced by gender categories. This constraint story
 *   describes the institutional structure and extraction dynamics that arise
 *   when one reading is imposed without full accommodation of the sibling
 *   readings' legitimate structural concerns.
 *
 * KEY AGENTS:
 *   - trans_women: identity-locked beneficiaries; gain institutional recognition and access through category redefinition
 *   - cis_women: constrained-exit victims; bear enforcement costs (institutional liability, social stigma, obstruction of sex-based organizing)
 *   - gender_identity_advocates: organized agenda-setters; enforce the reading through legal challenge, DEI policy, and norm-setting
 *   - sex_essentialist_advocates: constrained-exit excluded parties; their counter-reading is pre-judged as discriminatory
 *   - inclusive_institutions: institutional beneficiary-agenda-setters; adopt gender categories and DEI compliance
 *   - policy_arbiters: analytical observers; courts, legislatures, agencies deciding between competing readings
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gendered_category_membership__gender_identity_reading, 0.58).
domain_priors:suppression_score(gendered_category_membership__gender_identity_reading, 0.62).
domain_priors:theater_ratio(gendered_category_membership__gender_identity_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gendered_category_membership__gender_identity_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(gendered_category_membership__gender_identity_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(gendered_category_membership__gender_identity_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gendered_category_membership__gender_identity_reading, accessibility_collapse, 0.51).
narrative_ontology:constraint_metric(gendered_category_membership__gender_identity_reading, resistance, 0.74).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gendered_category_membership__gender_identity_reading, tangled_rope).
narrative_ontology:human_readable(gendered_category_membership__gender_identity_reading, "Gender Category Membership via Self-Identification").
narrative_ontology:topic_domain(gendered_category_membership__gender_identity_reading, "social/ontological/bioethical").

domain_priors:requires_active_enforcement(gendered_category_membership__gender_identity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gendered_category_membership__gender_identity_reading, '9d684b2f-46a9-4b63-b667-69abf2e66d83').
narrative_ontology:cs_kernel_codification('9d684b2f-46a9-4b63-b667-69abf2e66d83', distributed).
narrative_ontology:cs_authority_grounding('9d684b2f-46a9-4b63-b667-69abf2e66d83', distributed).
narrative_ontology:cs_reading_relation('9d684b2f-46a9-4b63-b667-69abf2e66d83', gendered_category_membership__biological_sex_reading, coexists_with).
narrative_ontology:cs_reading_relation('9d684b2f-46a9-4b63-b667-69abf2e66d83', gendered_category_membership__social_role_reading, influences).
narrative_ontology:cs_axiom('9d684b2f-46a9-4b63-b667-69abf2e66d83', foundational, gender_identity_constitutive_of_category_membership).
narrative_ontology:cs_axiom_status(gender_identity_constitutive_of_category_membership, holdable).
narrative_ontology:cs_axiom_grounding('9d684b2f-46a9-4b63-b667-69abf2e66d83', gender_identity_constitutive_of_category_membership, deontological).
narrative_ontology:cs_axiom('9d684b2f-46a9-4b63-b667-69abf2e66d83', foundational, subjective_self_declaration_sufficient_ground).
narrative_ontology:cs_axiom_status(subjective_self_declaration_sufficient_ground, holdable).
narrative_ontology:cs_axiom_grounding('9d684b2f-46a9-4b63-b667-69abf2e66d83', subjective_self_declaration_sufficient_ground, deontological).
narrative_ontology:cs_reference_frame('9d684b2f-46a9-4b63-b667-69abf2e66d83', sex_category_immutability_framework).
narrative_ontology:cs_drift_state('9d684b2f-46a9-4b63-b667-69abf2e66d83', institutional_gender_identity_adoption, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('9d684b2f-46a9-4b63-b667-69abf2e66d83', '').
narrative_ontology:cs_kernel_id(gendered_category_membership__gender_identity_reading, gendered_category_membership).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gendered_category_membership__gender_identity_reading, trans_women).
narrative_ontology:constraint_beneficiary(gendered_category_membership__gender_identity_reading, gender_identity_advocates).
narrative_ontology:constraint_beneficiary(gendered_category_membership__gender_identity_reading, inclusive_institutions).
narrative_ontology:constraint_victim(gendered_category_membership__gender_identity_reading, cis_women).
narrative_ontology:constraint_victim(gendered_category_membership__gender_identity_reading, sex_essentialist_advocates).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Gain formal recognition and institutional access through gender-identity-based category membership: legal documentation, sex-segregated facilities, affinity groups, employment non-discrimination protection. Exit from this identity would require denying their self-understanding; the constraint's recognition is the mechanism of their institutional belonging.
narrative_ontology:constraint_stakeholder(gendered_category_membership__gender_identity_reading, trans_women, beneficiary,
    moderate, biographical, identity_locked, national).

% Bear enforcement costs when they resist category redefinition: institutional liability exposure for rejecting trans women from sex-segregated spaces (locker rooms, shelters, prisons), employment exposure for gender-essentialist speech, social stigma for voicing sex-based concerns. Their ability to organize around sex-based interests (reproductive health, sexual violence prevention rooted in sex-specific threat) faces institutional obstruction as 'exclusionary.'
narrative_ontology:constraint_stakeholder(gendered_category_membership__gender_identity_reading, cis_women, payer,
    organized, biographical, constrained, national).

% Set institutional policy via legal challenge, administrative rule-making, and norm-setting: advocate for X-gender markers, anti-discrimination statutes with gender-identity coverage, institutional inclusion guidelines. Enforce the policy through litigation threats, DEI infrastructure, and social pressure against resisters. Claim the constraint solves recognition injustice; frame resistance as bigotry.
narrative_ontology:constraint_stakeholder(gendered_category_membership__gender_identity_reading, gender_identity_advocates, agenda_setter,
    organized, generational, mobile, national).

% Face institutional marginalization for asserting sex-based category claims: deplatforming, employment risk, academic censure. Their counter-claim—that sex is an immutable biological category relevant to women's interests—is treated as discriminatory rather than as a competing reading of the same kernel. Excluded from official policy-setting forums.
narrative_ontology:constraint_stakeholder(gendered_category_membership__gender_identity_reading, sex_essentialist_advocates, payer,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(gendered_category_membership__gender_identity_reading, sex_essentialist_advocates, excluded).

% Adopt gender-identity-based category membership via policy, reduce administrative sex categories to functional subsets (pregnancy, prostate cancer), rename sex-segregated facilities to gender-segregated ones. Gain institutional alignment with advocacy norms and legal defensibility in jurisdictions where gender-identity protections are codified. Bear cost if sex-segregation serves legitimate function and that function degrades under gender categories.
narrative_ontology:constraint_stakeholder(gendered_category_membership__gender_identity_reading, inclusive_institutions, beneficiary,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(gendered_category_membership__gender_identity_reading, inclusive_institutions, agenda_setter).

% Systematically excluded from institutional category-setting: their framing (that sex categories serve women's reproductive and sexual-violence-prevention interests) is treated as bigoted rather than as an alternative reading of the kernel. They have material interests in preserving sex-based analysis (reproductive health research, violence prevention, sports fairness) but face institutional closure.
narrative_ontology:constraint_stakeholder(gendered_category_membership__gender_identity_reading, biological_sex_advocates, excluded,
    moderate, biographical, trapped, national).

% Legislatures, courts, administrative agencies that must decide whether to codify gender-identity-based categories. Face conflicting testimony from cis women, trans women, advocates, and academic experts. Some jurisdictions have chosen the gender-identity reading; others enforce sex-based categories; most are contested.
narrative_ontology:constraint_stakeholder(gendered_category_membership__gender_identity_reading, policy_arbiters, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(gendered_category_membership__gender_identity_reading, inclusive_institutions).
narrative_ontology:fixing_cost_class(gendered_category_membership__gender_identity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Achieves recognition and institutional belonging for trans individuals by decoupling category membership from immutable sex markers and grounding it in subjective identity self-declaration. Simplifies institutional category administration by collapsing sex-specific concerns (reproductive health, sexual violence prevention, sports fairness) into gender categories that do not track the biological distinctions those concerns turn on.
% TRANSFER_FUNCTION: Moves recognition authority from objective (biological) markers to subjective (identity-based) declaration. Institutional cost and legitimacy are transferred from cis-women's sex-based organizing capacity to inclusive-institutions' DEI compliance and trans women's category inclusion. The constraint transfers institutional attention and resources away from sex-differentiated research, policy, and safety infrastructure toward gender-unified administration.
% ABSENT_VOICES: Sex-essentialist advocates and biological-category defenders are structurally excluded: their core claim—that sex categories serve material women's interests—is pre-judged as exclusionary. They would argue that trans women and cis women have partly divergent interests (reproductive biology, sexual violence rooted in sexed threat), that collapsing sex categories erases those interests, and that self-identification alone cannot determine category membership where material interests diverge. They are kept out of official policy forums and face institutional liability for voicing this claim.
% DISAPPEARANCE_RATIONALE: If gender-identity-based category membership disappeared overnight, institutional practices would revert to sex-based categories (or remain contested). Trans women would lose formal legal recognition and institutional access via this mechanism (though other identity-recognition pathways might remain). Cis women's ability to organize sex-based interests would resume, though institutional inertia might preserve some DEI infrastructure. The world would reorganize, but the direction of reorganization is precisely what the kernel contest addresses.
% FOUNDING_PROBLEM: Transgender individuals face institutional barriers to recognition and belonging: legal documents, institutional forms, and social categories are sex-coded, forcing trans people to choose between authentic identity and institutional access. The founding problem is the mismatch between subjective identity and institutional category.
% FOUNDING_PROBLEM_CORROBORATION: Trans advocates and gender-identity scholars attest the founding problem is live and urgent. Clinical psychology and medicine increasingly recognize gender dysphoria and support social transition. But sex-essentialist advocates and biological researchers attest that the founding problem was solved by social/medical recognition without category redefinition: trans women can be recognized and supported without redefining 'woman' to include them. Policy arbiters and courts remain divided on which framing captures the true founding problem.
narrative_ontology:disappearance_verdict(gendered_category_membership__gender_identity_reading, contested).
narrative_ontology:founding_problem_status(gendered_category_membership__gender_identity_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gendered_category_membership__gender_identity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(gendered_category_membership__gender_identity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(gendered_category_membership__gender_identity_reading, 0.58, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness starts low (0.32 at t0, projected pre-institutional adoption) because the constraint is a novel claim not yet widely institutionalized. It rises to peak (0.63 at t=20, observed) as institutional adoption accelerates and cis women face real enforcement costs (employment risk for sex-essentialist speech, institutional liability for sex-segregated decision-making, social stigma for resistance). The measurement series shows extraction accumulation as the reading becomes institutionalized—a characteristic Goodhart drift where recognition injustice (the genuine founding problem) is solved, but then institutional enforcement extends beyond recognition into category redefinition, imposing costs on those who resist. The slight decline at t=25 (projected) reflects nascent backlash and legislative counter-moves in some jurisdictions, not resolution. Suppression requirement rises in parallel: enforcement machinery (HR audits, diversity training, litigation threats against resisters) intensifies to suppress the cis-women payers' ability to organize sex-based interests. Theater ratio rises because an increasing share of institutional activity labeled 'inclusion' is performative category administration rather than addressing material recognition injustice—institutional forms change, but the underlying interests (reproductive research funding, sexual-violence prevention rooted in sex-specific threat) go unaddressed. Accessibility_collapse is moderate (0.51) because sex-based alternatives remain technically available (grassroots organizing, some jurisdictions enforce sex categories, academic discourse persists) but institutional closure makes them costly and stigmatized. Resistance is high (0.74) precisely because this is not a natural law or a frictionless coordination: millions of cis women and sex-essentialist advocates actively resist the reading, litigation continues, legislative battles proceed. The measurement series is on one shared time grid: every metric is authored at every time point (0, 5, 10, 15, 20, 25), enforcing temporal alignment.
 *
 * PERSPECTIVAL GAP:
 *   The beneficiary seats (trans_women, gender_identity_advocates, inclusive_institutions) should compute as perceiving genuine coordination and justice-enabling (a mountain or rope from their position); the payer seats (cis_women, sex_essentialist_advocates) should compute as perceiving extraction and suppression (a snare or tangled_rope). The engine derives directionality from the structural data (beneficiary vs. victim + exit options + power), so the computed types diverge by seat. From trans_women's identity-locked position, the constraint is identity-enabling—low directionality, high subsidy-like effect. From cis_women's constrained-exit position, the constraint is extractive and suppressive—high directionality, high extraction. The gap is the point: institutional mechanisms often appear as coordination to those they benefit and as extraction to those they burden, and the divergence is STRUCTURAL not perceptual. The engine captures this by computing per-seat classification.
 *
 * DIRECTIONALITY LOGIC:
 *   Trans women are structured as beneficiaries: the constraint grants them recognition and institutional access they lacked before. Their exit options are identity_locked because denying their gender identity would mean abandoning the self-understanding the constraint affirms. Their power is moderate (organized advocacy community, but no institutional control). From their seat: d ≈ 0.2 (beneficiary-tilted), low effective extraction χ. Cis women are structured as victims: they pay enforcement costs (institutional liability for sex-segregated decision-making, employment risk for sex-essentialist speech, obstruction of sex-based organizing). Their exit options are constrained (they cannot opt out of gendered institutional systems; they can organize clandestinely but not openly without risk). Their power is organized but not institutional. From their seat: d ≈ 0.8 (target-tilted), high effective extraction χ. Gender_identity_advocates are the agenda-setters: they enforce the reading through legal and administrative machinery. Their power is organized (advocacy infrastructure, legal resources, DEI networks); their exit options are mobile (they can shift advocacy focus if the reading is displaced). From their seat: they control the constraint's persistence, so d ≈ 0.1 (beneficiary-controlling). Sex-essentialist_advocates are excluded and face constrained exit (institutional closure, social stigma). From their seat: d ≈ 0.9 (high extraction, but from the excluded position—they are not even seated at the table where the extraction is decided). NO directionality overrides are needed here; the structural derivation captures the asymmetry.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy question: HAS THE FOUNDING PROBLEM BECOME DEFUNCT, WHILE THE CONSTRAINT PERSISTS? Founding problem: trans people faced institutional barriers to recognition. That problem is substantially solved by the constraint—institutional forms now recognize gender identity, legal documents can be updated, trans people gain formal belonging. BUT: the constraint now persists not because the founding problem remains (trans recognition is institutionally achieved) but because institutional actors (DEI bureaucrats, advocates, inclusive institutions) have captured the mechanism and extended it beyond recognition into category redefinition. The category redefinition imposes costs on cis women (sex-based organizing obstruction, employment risk) that are NOT necessary to solve the founding problem. A non-mandatrophic version of the constraint would be: 'institutional recognition of gender identity for trans people's legal documents and access, WHILE preserving sex-based categories for sex-specific interests (reproductive health research, sexual violence prevention, sports fairness).' That version solves the founding problem (trans recognition) without imposing the extraction cost on cis women. The measured extraction (0.58 at interval end) and suppression (0.62) suggest mandatrophy is LIVE but not RESOLVED: the constraint persists beyond its founding rationale, extraction has accumulated, enforcement has intensified, but the founding problem is still cited as justification. A mandatrophy-resolved reading would reformulate the constraint to preserve sex categories for material interests while granting gender-identity recognition for institutional access—a technical unbundling that the present constraint does not undertake.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    foundational_axiom_identity_vs_biology,
    'What is the metaphysical and practical ground of sex/gender category membership: subjective identity-feeling, immutable biological markers, or sustained social role? Can one ground serve all legitimate category-uses, or do different uses (medical research, sexual violence prevention, sports fairness, legal recognition, social belonging) require different grounding?',
    'Longitudinal data on outcomes: Do sex-specific medical research questions (reproductive health, sex-hormone-dependent conditions, sexual-violence threat profiles) yield valid results when analyzed under gender categories? Do sports fairness and sexual-violence prevention mechanisms operate identically under gender vs. sex categories? Testimony from researchers, clinicians, athletes, and violence-prevention practitioners.',
    'If different category uses require different grounds, the constraint as instantiated is overreaching—it imposes gender categories on domains (medical research, sexual violence prevention) where sex categories serve material interests. The classification would remain tangled_rope (recognition benefit + imposition cost), but the omega would document that the cost could be reduced by preserving sex categories for domains where they serve legitimate interests. If one ground suffices, the omega resolves against the biological and social-role readings.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(foundational_axiom_identity_vs_biology, conceptual, 'Whether the category ground is universal or function-specific.').

omega_variable(
    suppression_mechanism_structural_vs_internalized,
    'Is the measured suppression of sex-essentialist voices and sex-based organizing structural (institutional closure, HR policies, litigation threats) or internalized (cis women self-censor after internalizing that sex-essentialism is immoral)? Or both, and in what proportion?',
    'Post-policy-change evidence: In jurisdictions that explicitly codify sex-based category recognition alongside gender-identity recognition, do cis women''s sex-based organizing resume? If yes, suppression was primarily structural and removable. If no, suppression persists even after structural barriers fall—evidence of internalization.',
    'If suppression is primarily structural, the constraint''s persistence depends on active enforcement; removing the enforcement (admitting sex categories to policy forums) would dissolve suppression and allow coordination. If suppression is partially internalized, cis women would need counter-messaging and identity reconstruction to exit—a slower, deeper form of constraint. Internalization suggests the constraint''s effective suppression is higher than the structural measure suggests.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_vs_internalized, empirical, 'Whether suppression of sex-based organizing is structural or internalized.').

omega_variable(
    read_divergence_genuine_or_rhetorical,
    'Do the three readings—gender_identity, biological_sex, social_role—represent genuinely incompatible metaphysical claims, or are they rhetorically incompatible positions where a technical solution could preserve all three?',
    'Policy experiment: A jurisdiction that formally recognizes gender identity for institutional access (legal documents, bathrooms, affinity groups) WHILE preserving sex categories for sex-specific interests (reproductive research, sexual-violence prevention, women''s sports competitions, reproductive health clinics). Measure: Do trans people achieve recognition and belonging? Do cis women retain sex-based organizing capacity? Does sex-specific research continue? If all three succeed, the readings were instrumentally separable and the present constraint''s extraction is an artifact of over-extension.',
    'If readings are separable, a reformed constraint would preserve gender-identity recognition while unbundling category redefinition—solving the founding problem (trans recognition) without the cis-women-payer extraction cost. This would likely reclassify the reformed version to rope (coordination without asymmetric extraction). It would document that the present constraint''s tangled_rope classification reflects an over-reach in policy design, not an inherent feature of recognizing trans people.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(read_divergence_genuine_or_rhetorical, empirical, 'Whether the three readings are genuinely incompatible or technically separable.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gendered_category_membership__gender_identity_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gend_tr_t0, gendered_category_membership__gender_identity_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement_basis(gend_tr_t0, projected).
narrative_ontology:measurement(gend_tr_t5, gendered_category_membership__gender_identity_reading, theater_ratio, 5, 0.28).
narrative_ontology:measurement_basis(gend_tr_t5, observed).
narrative_ontology:measurement(gend_tr_t10, gendered_category_membership__gender_identity_reading, theater_ratio, 10, 0.35).
narrative_ontology:measurement_basis(gend_tr_t10, observed).
narrative_ontology:measurement(gend_tr_t15, gendered_category_membership__gender_identity_reading, theater_ratio, 15, 0.41).
narrative_ontology:measurement_basis(gend_tr_t15, observed).
narrative_ontology:measurement(gend_tr_t20, gendered_category_membership__gender_identity_reading, theater_ratio, 20, 0.44).
narrative_ontology:measurement_basis(gend_tr_t20, observed).
narrative_ontology:measurement(gend_tr_t25, gendered_category_membership__gender_identity_reading, theater_ratio, 25, 0.41).
narrative_ontology:measurement_basis(gend_tr_t25, projected).

% Extraction over time
narrative_ontology:measurement(gend_be_t0, gendered_category_membership__gender_identity_reading, base_extractiveness, 0, 0.32).
narrative_ontology:measurement_basis(gend_be_t0, projected).
narrative_ontology:measurement(gend_be_t5, gendered_category_membership__gender_identity_reading, base_extractiveness, 5, 0.42).
narrative_ontology:measurement_basis(gend_be_t5, observed).
narrative_ontology:measurement(gend_be_t10, gendered_category_membership__gender_identity_reading, base_extractiveness, 10, 0.52).
narrative_ontology:measurement_basis(gend_be_t10, observed).
narrative_ontology:measurement(gend_be_t15, gendered_category_membership__gender_identity_reading, base_extractiveness, 15, 0.58).
narrative_ontology:measurement_basis(gend_be_t15, observed).
narrative_ontology:measurement(gend_be_t20, gendered_category_membership__gender_identity_reading, base_extractiveness, 20, 0.63).
narrative_ontology:measurement_basis(gend_be_t20, observed).
narrative_ontology:measurement(gend_be_t25, gendered_category_membership__gender_identity_reading, base_extractiveness, 25, 0.58).
narrative_ontology:measurement_basis(gend_be_t25, projected).

% Suppression requirement over time
narrative_ontology:measurement(gend_su_t0, gendered_category_membership__gender_identity_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement_basis(gend_su_t0, projected).
narrative_ontology:measurement(gend_su_t5, gendered_category_membership__gender_identity_reading, suppression_requirement, 5, 0.48).
narrative_ontology:measurement_basis(gend_su_t5, observed).
narrative_ontology:measurement(gend_su_t10, gendered_category_membership__gender_identity_reading, suppression_requirement, 10, 0.58).
narrative_ontology:measurement_basis(gend_su_t10, observed).
narrative_ontology:measurement(gend_su_t15, gendered_category_membership__gender_identity_reading, suppression_requirement, 15, 0.65).
narrative_ontology:measurement_basis(gend_su_t15, observed).
narrative_ontology:measurement(gend_su_t20, gendered_category_membership__gender_identity_reading, suppression_requirement, 20, 0.68).
narrative_ontology:measurement_basis(gend_su_t20, observed).
narrative_ontology:measurement(gend_su_t25, gendered_category_membership__gender_identity_reading, suppression_requirement, 25, 0.62).
narrative_ontology:measurement_basis(gend_su_t25, projected).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gendered_category_membership__gender_identity_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(gendered_category_membership__gender_identity_reading, 0.12).
narrative_ontology:affects_constraint(gendered_category_membership__gender_identity_reading, gendered_category_membership__biological_sex_reading).
narrative_ontology:affects_constraint(gendered_category_membership__gender_identity_reading, gendered_category_membership__social_role_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the contested kernel 'gendered_category_membership.' All three readings (gender_identity, biological_sex, social_role) share the same kernel but produce different ε values and beneficiary/victim structures. The network links them: this reading (gender_identity) influences the social_role reading (if institutional categories are gender-identity-based, social role becomes less salient) and coexists with the biological_sex reading (neither reading logically forecloses the other; they remain live positions held by different institutional actors and advocacy communities). Each reading is a separate constraint story with its own ε, stakeholders, and classification.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
