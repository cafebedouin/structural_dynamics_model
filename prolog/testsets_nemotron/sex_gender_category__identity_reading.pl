% ============================================================================
% CONSTRAINT STORY: sex_gender_category__identity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sex_gender_category__identity_reading, []).

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
 *   constraint_id: sex_gender_category__identity_reading
 *   human_readable: Category Membership by Subjective Gender Identity (Self-Identification)
 *   domain: social_ontology/legal_classification
 *
 * SUMMARY:
 *   This constraint models the identity reading of the contested kernel
 *   'sex_gender_category': category membership in 'woman' (and related
 *   legal/social categories) is determined solely by subjective gender
 *   identity (self-identification). Under this reading, trans women are
 *   included in the category 'woman' without medical gatekeeping, expanding
 *   the victim set to include trans women experiencing misogyny while cis
 *   women lose exclusive claim to sex-based protections. Boundary enforcement
 *   costs are low (administrative self-declaration replaces medical/legal
 *   gatekeeping) but conflict over space access (shelters, prisons, sports,
 *   changing rooms) is high. The constraint functions as a tangled rope: it
 *   solves a genuine coordination problem (providing clear, accessible
 *   category membership for trans and nonbinary people) while simultaneously
 *   creating asymmetric extraction (cis women lose exclusive protections;
 *   trans women gain inclusion but enter misogyny-exposed positions). Active
 *   enforcement is required to maintain inclusion against biology-based
 *   challenges.
 *
 * KEY AGENTS:
 *   - trans_women: Primary beneficiary (gain category access) and victim (enter misogyny-exposed positions) — identity_locked exit
 *   - cis_women: Primary victim (lose exclusive sex-based protections) — constrained exit
 *   - nonbinary_people: Beneficiary (gain recognition pathway) — constrained exit
 *   - biology_reading_advocates: Excluded (would object but structurally locked out of self-ID frameworks) — trapped exit
 *   - institutions_courts_employers: Agenda setter (administer category rules) — institutional power, arbitrage exit
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sex_gender_category__identity_reading, 0.35).
domain_priors:suppression_score(sex_gender_category__identity_reading, 0.25).
domain_priors:theater_ratio(sex_gender_category__identity_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sex_gender_category__identity_reading, extractiveness, 0.35).
narrative_ontology:constraint_metric(sex_gender_category__identity_reading, suppression_requirement, 0.25).
narrative_ontology:constraint_metric(sex_gender_category__identity_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(sex_gender_category__identity_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(sex_gender_category__identity_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sex_gender_category__identity_reading, tangled_rope).
narrative_ontology:human_readable(sex_gender_category__identity_reading, "Category Membership by Subjective Gender Identity (Self-Identification)").
narrative_ontology:topic_domain(sex_gender_category__identity_reading, "social_ontology/legal_classification").

domain_priors:requires_active_enforcement(sex_gender_category__identity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(sex_gender_category__identity_reading, '8b68f315-7b4f-4fb2-93c9-44efc888be94').
narrative_ontology:cs_kernel_codification('8b68f315-7b4f-4fb2-93c9-44efc888be94', distributed).
narrative_ontology:cs_authority_grounding('8b68f315-7b4f-4fb2-93c9-44efc888be94', distributed).
narrative_ontology:cs_reading_relation('8b68f315-7b4f-4fb2-93c9-44efc888be94', sex_gender_category__biology_reading, forecloses).
narrative_ontology:cs_reading_relation('8b68f315-7b4f-4fb2-93c9-44efc888be94', sex_gender_category__hybrid_reading, coexists_with).
narrative_ontology:cs_axiom('8b68f315-7b4f-4fb2-93c9-44efc888be94', foundational, gender_identity_sufficient_for_category_membership).
narrative_ontology:cs_axiom_status(gender_identity_sufficient_for_category_membership, holdable).
narrative_ontology:cs_axiom_grounding('8b68f315-7b4f-4fb2-93c9-44efc888be94', gender_identity_sufficient_for_category_membership, deontological).
narrative_ontology:cs_axiom('8b68f315-7b4f-4fb2-93c9-44efc888be94', foundational, self_determination_grounds_legal_recognition).
narrative_ontology:cs_axiom_status(self_determination_grounds_legal_recognition, holdable).
narrative_ontology:cs_axiom_grounding('8b68f315-7b4f-4fb2-93c9-44efc888be94', self_determination_grounds_legal_recognition, deontological).
narrative_ontology:cs_reference_frame('8b68f315-7b4f-4fb2-93c9-44efc888be94', medical_gatekeeping_model).
narrative_ontology:cs_drift_state('8b68f315-7b4f-4fb2-93c9-44efc888be94', self_id_legislative_wave, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('8b68f315-7b4f-4fb2-93c9-44efc888be94', '').
narrative_ontology:cs_kernel_id(sex_gender_category__identity_reading, sex_gender_category).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sex_gender_category__identity_reading, trans_women).
narrative_ontology:constraint_beneficiary(sex_gender_category__identity_reading, nonbinary_people).
narrative_ontology:constraint_victim(sex_gender_category__identity_reading, cis_women).
narrative_ontology:constraint_victim(sex_gender_category__identity_reading, trans_women).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(sex_gender_category__identity_reading, trans_men).
narrative_ontology:constraint_vindicates(sex_gender_category__identity_reading, gender_self_determination).
narrative_ontology:constraint_vindicates(sex_gender_category__identity_reading, trans_inclusion_in_women_spaces).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Gain legal recognition and access to women's spaces/services through self-declaration. This inclusion is the coordination function. But they also enter positions exposed to misogyny (harassment, violence, reproductive control) that they would not face as men — a structural extraction that comes with the category. Exit from the 'woman' category means detransition, which is identity-locked (core self-concept fused with gender identity).
narrative_ontology:constraint_stakeholder(sex_gender_category__identity_reading, trans_women, beneficiary,
    moderate, biographical, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(sex_gender_category__identity_reading, trans_women, payer).

% Lose exclusive claim to sex-based protections (shelters, prisons, sports, shortlists, healthcare). The category 'woman' now includes people they regard as male-bodied, which they experience as extraction of their sex-class protections. Exit is constrained: they cannot opt out of the legal category system; they can only advocate for biology-based exemptions or separate categories.
narrative_ontology:constraint_stakeholder(sex_gender_category__identity_reading, cis_women, payer,
    organized, generational, constrained, national).

% Gain recognition pathway through self-ID systems that often include nonbinary markers. The coordination benefit is real (legal recognition without medical gatekeeping). But they may face new vulnerabilities in a binary legal system that still mostly recognizes only 'man' and 'woman'. Exit is constrained — they need the recognition system to function.
narrative_ontology:constraint_stakeholder(sex_gender_category__identity_reading, nonbinary_people, beneficiary,
    moderate, biographical, constrained, national).

% Hold that sex is immutable and category membership should track biology. In self-ID jurisdictions, their framework is legally foreclosed — they cannot implement biology-based policies. They are structurally excluded from the conversation; their objections are treated as illegitimate. Exit is trapped — they must operate under a categorization system they reject as false.
narrative_ontology:constraint_stakeholder(sex_gender_category__identity_reading, biology_reading_advocates, excluded,
    organized, generational, trapped, national).

% Administer the self-ID system: process declarations, adjudicate space-access disputes, implement policies. They benefit from administrative simplicity (self-declaration is cheaper than medical gatekeeping) but bear conflict costs (litigation, policy challenges, public controversy). They have arbitrage exit — they can modify implementation details or seek legislative clarification without losing institutional legitimacy.
narrative_ontology:constraint_stakeholder(sex_gender_category__identity_reading, institutions_courts_employers, agenda_setter,
    institutional, generational, arbitrage, national).

% Gain legal recognition as men through self-declaration. Coordination benefit is real (correct category, dignity). Extraction is lower than for trans women — they gain male privilege rather than entering misogyny-exposed positions. Exit is identity-locked (detransition required to leave 'man' category).
narrative_ontology:constraint_stakeholder(sex_gender_category__identity_reading, trans_men, beneficiary,
    moderate, biographical, identity_locked, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(sex_gender_category__identity_reading, diffuse).
narrative_ontology:fixing_cost_class(sex_gender_category__identity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides an accessible, administratively simple pathway for trans and nonbinary people to have their gender legally recognized and access corresponding spaces/services without medical gatekeeping.
% TRANSFER_FUNCTION: Moves sex-based protections and category access from an exclusive (biology-based) basis to an inclusive (identity-based) basis. Cis women lose exclusive protections; trans women and nonbinary people gain inclusion. The transfer is not monetary but status/protection/access.
% ABSENT_VOICES: Biology-reading feminists and gender-critical advocates are structurally excluded from self-ID policy processes — their framework is treated as illegitimate. Detransitioners (who would testify to the costs of easy entry/exit) are often marginalized in trans advocacy spaces. Parents of gender-questioning youth are sometimes excluded from medical/legal decisions.
% DISAPPEARANCE_RATIONALE: If self-ID vanished overnight, trans people would revert to medical-gatekept or biology-based systems, losing accessible recognition. Cis women would regain exclusive sex-based protections. Institutions would face higher administrative costs for gatekeeping. The legal/social landscape would reorganize around whatever reading replaced it (likely hybrid or biology).
% FOUNDING_PROBLEM: How to provide legal recognition and social inclusion for trans people without requiring medical transition as a prerequisite — the medical gatekeeping model excluded many trans people (cost, access, medical contraindications, nonbinary identities).
% FOUNDING_PROBLEM_CORROBORATION: Trans advocacy organizations and medical bodies (WPATH, major psychiatric associations) attest the problem is live — medical gatekeeping still excludes many. Gender-critical feminists and some legal scholars attest the problem is contested — they argue the founding problem was misdiagnosed and the solution creates new harms. No neutral third party corroborates either side exclusively.
narrative_ontology:disappearance_verdict(sex_gender_category__identity_reading, world_rearranges).
narrative_ontology:founding_problem_status(sex_gender_category__identity_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(sex_gender_category__identity_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(sex_gender_category__identity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(sex_gender_category__identity_reading, 0.35, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sex_gender_category__identity_reading_tests).
:- end_tests(sex_gender_category__identity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.35) is moderate: the constraint transfers sex-based protections from an exclusive to an inclusive basis, which is a real resource transfer but not pure rent extraction. Suppression (0.25) is low-moderate: formal enforcement is cheap (self-declaration), but social/institutional pressure to conform to the reading creates internalized suppression for dissenters. Theater ratio (0.4) is significant: much institutional discourse performs 'inclusion' while material conditions for both trans women and cis women remain contested. Accessibility collapse (0.55) reflects that alternatives (biology-based, hybrid models) are not fully collapsed — they remain live in law, medicine, and social practice. Resistance (0.6) is high: the reading faces sustained challenge from biology_reading and hybrid_reading proponents across legal, political, and cultural domains.
 *
 * PERSPECTIVAL GAP:
 *   From the trans_women seat: the constraint is primarily coordination (access to correct category, dignity, legal recognition) with secondary extraction (misogyny exposure). From the cis_women seat: the constraint is primarily extraction (loss of sex-based protections, forced inclusion of male-bodied people in female spaces) with minimal coordination benefit. From the institutions seat: the constraint is coordination (administrative simplicity, legal clarity) with manageable conflict costs. The engine computes these divergent seat types from the structural data — the authored claim (tangled_rope) does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Trans women are structural beneficiaries (d ~ 0.2) — they gain category access and legal recognition. But they are also victims (d ~ 0.7 for misogyny exposure post-inclusion) — identity_locked exit means they cannot leave the 'woman' category without undoing their transition. Cis women are structural victims (d ~ 0.8) — they lose exclusive protections with constrained exit (cannot opt out of the inclusive category system). Nonbinary people are beneficiaries (d ~ 0.3) with constrained exit. Institutions are agenda setters (d ~ 0.4) — they administer the system but face conflict costs. Biology_reading advocates are excluded (d ~ 0.9) — trapped, their preferred framework is legally foreclosed in self-ID jurisdictions.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (coordination: how to categorize trans people without medical gatekeeping) is live but contested. The identity reading resolves it by making self-ID sufficient, but creates new extraction (cis women's lost protections, trans women's misogyny exposure). The constraint is not a piton — it is actively maintained and expanded, not theatrically preserved. It is not a snare — the coordination function is genuine and the beneficiary group (trans people) is not a cover for extraction. The tangled rope classification captures the real duality: inclusion IS coordination, but it redistributes vulnerability asymmetrically.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity_vs_biology,
    'Is sex/gender category membership a reading of the contested kernel ''sex_gender_category'' (identity_reading vs biology_reading vs hybrid_reading)?',
    'Meta-analysis of how different institutional frameworks (legal, medical, social) instantiate different readings; track which reading governs in each domain and what structural consequences follow.',
    'If multiple readings coexist as live institutional options, the kernel is genuinely contested and each reading should be modeled as a separate constraint. If one reading has been legally foreclosed in a jurisdiction, that reading''s constraint there is inert.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity_vs_biology, conceptual, 'Whether this constraint is one reading of a multi-reading kernel rather than a standalone constraint.').

omega_variable(
    cis_women_as_victim_and_beneficiary,
    'Are cis women simultaneously victims (losing exclusive sex-based protections) and beneficiaries (of expanded gender-inclusive frameworks) under the identity reading?',
    'Empirical study of cis women''s lived experience in jurisdictions with self-ID laws: do they report net loss of sex-based protections, net gain from inclusive frameworks, or both simultaneously?',
    'If cis women are dual-positioned, the constraint''s extraction profile is more complex than simple beneficiary/victim partition. The engine''s per-seat computation would need to reflect this duality.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cis_women_as_victim_and_beneficiary, empirical, 'Whether cis women occupy a structurally dual position under identity-based category membership.').

omega_variable(
    trans_women_as_dual_positioned,
    'Are trans women simultaneously beneficiaries (gaining category access) and victims (experiencing misogyny after inclusion) under the identity reading?',
    'Longitudinal study of trans women''s experiences post-legal transition: track both category access benefits and misogyny exposure to measure net structural position.',
    'Dual positioning of trans women would make the constraint a classic tangled rope — coordination (inclusion) AND asymmetric extraction (misogyny exposure) in the same structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(trans_women_as_dual_positioned, empirical, 'Whether the primary beneficiary group also bears extraction within the same constraint.').

omega_variable(
    enforcement_cost_vs_conflict_intensity,
    'Why are boundary enforcement costs low but space-access conflicts high under self-identification?',
    'Compare administrative costs of self-ID systems vs gatekept systems; separately measure conflict frequency/intensity over single-sex spaces, sports, prisons, shelters.',
    'If enforcement is cheap but conflict is intense, the constraint''s suppression operates through social contest rather than state coercion — a different structural signature than high-enforcement snares.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_cost_vs_conflict_intensity, empirical, 'The dissociation between low formal enforcement cost and high social conflict under self-ID.').

omega_variable(
    suppression_mechanism_identity_reading,
    'Is suppression under the identity reading primarily structural (legal/administrative barriers to biology-based claims) or internalized (social pressure to accept self-ID, fear of ostracism for dissent)?',
    'Post-policy-change suppression trajectory: if suppression persists or shifts form after legal barriers are removed, reclassify as partially internalized. Survey dissenters'' reported costs of non-compliance.',
    'If internalized, the constraint''s effective suppression is higher than structural measures suggest — the target carries suppression with them after formal exit.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_identity_reading, empirical, 'Structural vs internalized suppression mechanism in identity-based category systems.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sex_gender_category__identity_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sex__tr_t0, sex_gender_category__identity_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(sex__tr_t5, sex_gender_category__identity_reading, theater_ratio, 5, 0.18).
narrative_ontology:measurement(sex__tr_t10, sex_gender_category__identity_reading, theater_ratio, 10, 0.28).
narrative_ontology:measurement(sex__tr_t15, sex_gender_category__identity_reading, theater_ratio, 15, 0.35).
narrative_ontology:measurement(sex__tr_t20, sex_gender_category__identity_reading, theater_ratio, 20, 0.4).

% Extraction over time
narrative_ontology:measurement(sex__be_t0, sex_gender_category__identity_reading, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(sex__be_t5, sex_gender_category__identity_reading, base_extractiveness, 5, 0.22).
narrative_ontology:measurement(sex__be_t10, sex_gender_category__identity_reading, base_extractiveness, 10, 0.28).
narrative_ontology:measurement(sex__be_t15, sex_gender_category__identity_reading, base_extractiveness, 15, 0.32).
narrative_ontology:measurement(sex__be_t20, sex_gender_category__identity_reading, base_extractiveness, 20, 0.35).

% Suppression requirement over time
narrative_ontology:measurement(sex__su_t0, sex_gender_category__identity_reading, suppression_requirement, 0, 0.1).
narrative_ontology:measurement(sex__su_t5, sex_gender_category__identity_reading, suppression_requirement, 5, 0.15).
narrative_ontology:measurement(sex__su_t10, sex_gender_category__identity_reading, suppression_requirement, 10, 0.2).
narrative_ontology:measurement(sex__su_t15, sex_gender_category__identity_reading, suppression_requirement, 15, 0.23).
narrative_ontology:measurement(sex__su_t20, sex_gender_category__identity_reading, suppression_requirement, 20, 0.25).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sex_gender_category__identity_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(sex_gender_category__identity_reading, 0.08).
narrative_ontology:affects_constraint(sex_gender_category__identity_reading, sex_gender_category__biology_reading).
narrative_ontology:affects_constraint(sex_gender_category__identity_reading, sex_gender_category__hybrid_reading).
narrative_ontology:affects_constraint(sex_gender_category__identity_reading, single_sex_space_access).
narrative_ontology:affects_constraint(sex_gender_category__identity_reading, sports_category_eligibility).
narrative_ontology:affects_constraint(sex_gender_category__identity_reading, prison_placement_policy).

% DUAL FORMULATION NOTE:
% Part of the sex_gender_category constraint family. The identity_reading treats self-identification as sufficient for category membership; the biology_reading treats reproductive biology as necessary; the hybrid_reading requires both biology and transition. Each reading instantiates a different constraint with different beneficiary/victim structures and different ε values. This decomposition follows the ε-invariance principle: the label 'gender category membership' covers structurally distinct claims with different extraction profiles.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(sex_gender_category__identity_reading, institutional, 0.4).
constraint_indexing:directionality_override(sex_gender_category__identity_reading, powerless, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
