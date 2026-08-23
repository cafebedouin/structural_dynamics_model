% ============================================================================
% CONSTRAINT STORY: woman_category__gender_identity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
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
    constraint_indexing:constraint_classification/3,
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
 *   human_readable: Woman Category: Gender Identity Reading
 *   domain: political_philosophy/law/social_policy/bioethics
 *
 * SUMMARY:
 *   This constraint instantiates the gender-identity reading of the contested
 *   woman_category kernel: the claim that 'woman' is a person who identifies
 *   as a woman, regardless of sex assigned at birth. It is codified in
 *   anti-discrimination law, identity-document policy, and institutional
 *   inclusion guidelines across a growing number of jurisdictions. The
 *   reading coordinates category membership by replacing biological
 *   gatekeeping with self-declaration, but its application to sports
 *   eligibility and sex-segregated spaces generates high-stakes collision
 *   between access rights and exclusion rights. The authored victim
 *   setâpeople who identify as women, including transgender
 *   womenâreflects the structural delta provided for this reading: they
 *   bear the concentrated costs of contested implementation, political
 *   targeting, and safety risks in high-extraction domains. The claim/metric
 *   independence is maintained: the constraint is claimed as tangled_rope
 *   (genuine coordination function plus asymmetric extraction) while the
 *   metrics describe a substantially extractive, actively enforced
 *   arrangement with high resistance.
 *
 * KEY AGENTS:
 *   - institutional_adopters (agenda_setter / institutional / constrained) â governments and courts that codify the gender-identity criterion and administer its enforcement
 *   - gender_inclusion_advocates (beneficiary / organized / mobile) â advocacy networks that collect policy victories and legitimacy from the constraint's operation
 *   - gender_identity_women (payer / powerless / identity_locked) â self-identified women, including transgender women, who bear the costs of collision in sports and segregated spaces
 *   - sex_based_protection_advocates (excluded / organized / constrained) â feminist and legal advocates arguing for sex-based categorical protections, structurally marginalized as discriminatory
 *   - sports_governance_bodies (observer / institutional / analytical) â federations attempting to reconcile inclusion with competitive fairness
 *   - medical_legal_observers (observer / institutional / analytical) â expert bodies providing testimony on gender identity standards
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(woman_category__gender_identity_reading, 0.66).
domain_priors:suppression_score(woman_category__gender_identity_reading, 0.72).
domain_priors:theater_ratio(woman_category__gender_identity_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(woman_category__gender_identity_reading, extractiveness, 0.66).
narrative_ontology:constraint_metric(woman_category__gender_identity_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(woman_category__gender_identity_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(woman_category__gender_identity_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(woman_category__gender_identity_reading, resistance, 0.78).

% --- Constraint claim ---
narrative_ontology:constraint_claim(woman_category__gender_identity_reading, tangled_rope).
narrative_ontology:human_readable(woman_category__gender_identity_reading, "Woman Category: Gender Identity Reading").
narrative_ontology:topic_domain(woman_category__gender_identity_reading, "political_philosophy/law/social_policy/bioethics").

domain_priors:requires_active_enforcement(woman_category__gender_identity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(woman_category__gender_identity_reading, '7d871585-0c63-4d80-bfab-eb72d3b21697').
narrative_ontology:cs_kernel_codification('7d871585-0c63-4d80-bfab-eb72d3b21697', formalized).
narrative_ontology:cs_authority_grounding('7d871585-0c63-4d80-bfab-eb72d3b21697', lineage).
narrative_ontology:cs_interpretation_layer_present('7d871585-0c63-4d80-bfab-eb72d3b21697').
narrative_ontology:cs_reading_relation('7d871585-0c63-4d80-bfab-eb72d3b21697', woman_category__sex_biology_reading, forecloses).
narrative_ontology:cs_reading_relation('7d871585-0c63-4d80-bfab-eb72d3b21697', woman_category__intersex_accommodation_reading, coexists_with).
narrative_ontology:cs_axiom('7d871585-0c63-4d80-bfab-eb72d3b21697', foundational, gender_identity_determines_membership).
narrative_ontology:cs_axiom_status(gender_identity_determines_membership, holdable).
narrative_ontology:cs_axiom_grounding('7d871585-0c63-4d80-bfab-eb72d3b21697', gender_identity_determines_membership, deontological).
narrative_ontology:cs_axiom('7d871585-0c63-4d80-bfab-eb72d3b21697', foundational, sex_based_exclusion_is_discriminatory).
narrative_ontology:cs_axiom_status(sex_based_exclusion_is_discriminatory, holdable).
narrative_ontology:cs_axiom_grounding('7d871585-0c63-4d80-bfab-eb72d3b21697', sex_based_exclusion_is_discriminatory, deontological).
narrative_ontology:cs_reference_frame('7d871585-0c63-4d80-bfab-eb72d3b21697', gender_self_determination_framework).
narrative_ontology:cs_drift_state('7d871585-0c63-4d80-bfab-eb72d3b21697', contemporary_policy_contestation, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('7d871585-0c63-4d80-bfab-eb72d3b21697', '').
narrative_ontology:cs_kernel_id(woman_category__gender_identity_reading, woman_category).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(woman_category__gender_identity_reading, gender_inclusion_advocates).
narrative_ontology:constraint_beneficiary(woman_category__gender_identity_reading, institutional_adopters).
narrative_ontology:constraint_victim(woman_category__gender_identity_reading, gender_identity_women).
narrative_ontology:constraint_vindicates(woman_category__gender_identity_reading, gender_self_determination_doctrine).
narrative_ontology:constraint_vindicates(woman_category__gender_identity_reading, anti_discrimination_gender_identity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Governments, courts, and administrative bodies that codify gender identity as the legal criterion for categorical membership. They gain a administrable rule that avoids biological testing but face litigation, political backlash, and contradictory demands when applying the rule to sports, shelters, and correctional facilities.
narrative_ontology:constraint_stakeholder(woman_category__gender_identity_reading, institutional_adopters, agenda_setter,
    institutional, generational, constrained, national).

% Advocacy organizations, legal nonprofits, and campaign networks that have secured adoption of gender-identity criteria. They collect policy victories, organizational funding, and moral legitimacy from the constraint's continued operation and defend it against sex-based protection claims.
narrative_ontology:constraint_stakeholder(woman_category__gender_identity_reading, gender_inclusion_advocates, beneficiary,
    organized, biographical, mobile, national).

% People who identify as women, including transgender women, who are designated beneficiaries of the category rule but bear its concentrated costs: invasive scrutiny during sports eligibility hearings, political targeting in public discourse, and safety risks in sex-segregated spaces where access rights collide with exclusion rights. Exit from the category is existentially costly because gender identity is constitutive of social personhood for this seat.
narrative_ontology:constraint_stakeholder(woman_category__gender_identity_reading, gender_identity_women, payer,
    powerless, biographical, identity_locked, national).

% Feminist and legal advocates who argue that 'woman' is a sex-based category requiring protections for female biology. They are structurally excluded from the policy framework, their arguments pre-classified as discriminatory rather than treated as contesting the kernel on its own terms.
narrative_ontology:constraint_stakeholder(woman_category__gender_identity_reading, sex_based_protection_advocates, excluded,
    organized, generational, constrained, national).

% International and national sports federations attempting to reconcile gender-identity inclusion with competitive fairness. They observe the collision but lack authority to resolve the underlying kernel dispute, producing shifting testosterone limits and inclusion guidelines that move with political and legal pressure.
narrative_ontology:constraint_stakeholder(woman_category__gender_identity_reading, sports_governance_bodies, observer,
    institutional, generational, analytical, global).

% Medical associations, legal scholars, and human rights monitors who provide expert testimony and standards on gender identity. They observe the implementation and offer interpretive guidance but do not directly collect or pay within the constraint's transfer structure.
narrative_ontology:constraint_stakeholder(woman_category__gender_identity_reading, medical_legal_observers, observer,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Resolves categorical boundary disputes over who counts as a woman by replacing biological gatekeeping with subjective self-identification, reducing administrative verification costs and affirming individual autonomy over categorical membership.
% TRANSFER_FUNCTION: Moves legal recognition, access to sex-segregated spaces, and eligibility for sex-based protections from a biologically assigned system to a self-declaration system; moves political legitimacy and policy-coordination gains to adopting institutions and advocacy networks, while moving the costs of contested implementationâscrutiny, safety risks, and political targetingâto self-identified women, especially in high-stakes domains like sports and shelters.
% ABSENT_VOICES: Advocates for sex-based protections for natal females are structurally excluded from the policy framework, their arguments pre-classified as discriminatory; intersex perspectives that resist binary gender-identity categorization are also largely absent from the dominant policy discourse.
% DISAPPEARANCE_RATIONALE: If the gender-identity criterion vanished overnight, legal sex classification would revert to biological or administrative sex markers, access policies for women's sports and shelters would require alternative eligibility tests, and the existing anti-discrimination infrastructure built around gender identity would require reconceptualization.
% FOUNDING_PROBLEM: How to include transgender women in the legal and social category 'woman' without subjecting them to invasive biological verification, medical gatekeeping, or categorical exclusion.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated by human rights organizations and medical associations from outside the immediate beneficiary advocacy sphere; contested by feminist legal scholars, sport regulators, and some medical ethicists who argue the solution dissolves necessary sex-based protections.
narrative_ontology:disappearance_verdict(woman_category__gender_identity_reading, world_rearranges).
narrative_ontology:founding_problem_status(woman_category__gender_identity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(woman_category__gender_identity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(woman_category__gender_identity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(woman_category__gender_identity_reading, 0.66, 'kimi-k2.6', 'none', direct).

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
 *   Extractiveness (0.66) is moderate-to-high because the constraint transfers category membership and access rights from a biologically assigned system to self-declaration, generating substantial costs in high-stakes domains (sports, shelters, prisons) where collision is sharpest. It is lower in identity-document policy. Suppression (0.72) is high because the constraint's persistence requires active enforcementâanti-discrimination penalties, institutional exclusion of dissent, and social delegitimation of sex-based protection claims. Theater ratio (0.28) reflects a real coordination function in legal recognition alongside growing performative allyship that outpaces institutional capacity. Accessibility collapse (0.60) captures the partial delegitimation of biological-sex-based alternatives without their full disappearance. Resistance (0.78) is high due to organized opposition from sex-based advocacy movements and jurisdictional pushback. The measurement series share a single time grid to prevent misalignment artifacts.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter and beneficiary seats (institutions, advocates) experience the constraint as a rights-affirming coordination mechanism that resolves boundary disputes and reduces gatekeeping costs. The payer seat (gender_identity_women) experiences the same constraint as a source of invasive scrutiny, political targeting, and physical risk in sex-segregated spaces where access rights collide with exclusion rights. The engine computes this divergence from the structural data: low directionality for adopters and advocates, high directionality for identity-locked payers.
 *
 * DIRECTIONALITY LOGIC:
 *   Institutional adopters and inclusion advocates are structural beneficiaries: they gain administrative clarity, policy coherence, and political legitimacy from the constraint's operation (low d, subsidized). Gender-identity women are structural targets: they bear the concentrated extraction of contested implementation, especially in sports and spaces, and their exit is identity-locked because leaving the category is existentially costly (high d, amplified chi). Sex-based protection advocates are excluded from the framework entirely, their opposition pre-classified as discriminatory, giving them no directional influence within the constraint's own logic.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint prevents mislabeling by preserving the coordination function: it genuinely resolves a categorical boundary problem and reduces the indignity of biological gatekeeping. However, the coordination function is inseparable from asymmetric extraction because the same self-declaration mechanism that grants documents also governs sports and shelter access, where the collision costs fall on the payer seat. Without the genuine coordination function, the constraint would be a pure snare; without the asymmetric extraction in high-stakes domains, it would be a rope. The tangled_rope classification captures this hybridity.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    domain_dependent_extraction,
    'Does the constraint''s extractiveness vary structurally between identity-document policy and sports eligibility such that they constitute separate constraints?',
    'Decompose into separate stories if empirical measurement shows non-overlapping epsilon confidence intervals across domains.',
    'If domain-specific, the current story conflates two constraints and should be split per the epsilon-invariance principle.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(domain_dependent_extraction, conceptual, 'Structural ambiguity over whether identity-documents and sports/spaces are the same constraint').

omega_variable(
    victim_beneficiary_ambiguity,
    'Are people who identify as women genuine victims of this constraint, or are they its primary beneficiaries, with the victim role actually belonging to natal women losing sex-based protections?',
    'Analyze directionality from each seat: if self-identified women experience negative effective extraction (subsidized access), they are beneficiaries; if they bear positive extraction (scrutiny, targeting, collision costs), they are victims.',
    'Reverses the beneficiary-victim structure and thus the directionality derivation for the primary affected group.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(victim_beneficiary_ambiguity, conceptual, 'Whether the primary affected group is subsidized or extracted-from').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression of sex-based protection claims structural (legal penalties, institutional exclusion) or internalized (social stigma, self-censorship)?',
    'Track opposition expression pre- and post-legal adoption; if suppression persists after legal threat recedes, it is partially internalized.',
    'Internalized suppression inflates effective extraction beyond the structural measure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural versus internalized suppression mechanism').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(woman_category__gender_identity_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(wcgir_tr_t0, woman_category__gender_identity_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(wcgir_tr_t10, woman_category__gender_identity_reading, theater_ratio, 10, 0.15).
narrative_ontology:measurement(wcgir_tr_t20, woman_category__gender_identity_reading, theater_ratio, 20, 0.2).
narrative_ontology:measurement(wcgir_tr_t30, woman_category__gender_identity_reading, theater_ratio, 30, 0.24).
narrative_ontology:measurement(wcgir_tr_t40, woman_category__gender_identity_reading, theater_ratio, 40, 0.28).

% Extraction over time
narrative_ontology:measurement(wcgir_be_t0, woman_category__gender_identity_reading, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(wcgir_be_t10, woman_category__gender_identity_reading, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(wcgir_be_t20, woman_category__gender_identity_reading, base_extractiveness, 20, 0.55).
narrative_ontology:measurement(wcgir_be_t30, woman_category__gender_identity_reading, base_extractiveness, 30, 0.61).
narrative_ontology:measurement(wcgir_be_t40, woman_category__gender_identity_reading, base_extractiveness, 40, 0.66).

% Suppression requirement over time
narrative_ontology:measurement(wcgir_su_t0, woman_category__gender_identity_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(wcgir_su_t10, woman_category__gender_identity_reading, suppression_requirement, 10, 0.55).
narrative_ontology:measurement(wcgir_su_t20, woman_category__gender_identity_reading, suppression_requirement, 20, 0.63).
narrative_ontology:measurement(wcgir_su_t30, woman_category__gender_identity_reading, suppression_requirement, 30, 0.68).
narrative_ontology:measurement(wcgir_su_t40, woman_category__gender_identity_reading, suppression_requirement, 40, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(woman_category__gender_identity_reading, identity_coordination).
narrative_ontology:affects_constraint(woman_category__gender_identity_reading, sex_biology_reading).
narrative_ontology:affects_constraint(woman_category__gender_identity_reading, intersex_accommodation_reading).

% DUAL FORMULATION NOTE:
% The woman_category kernel decomposes into three structurally distinct readings. The gender_identity_reading treats self-identification as determinative; the sex_biology_reading treats chromosomal/anatomical sex as determinative; the intersex_accommodation_reading treats sex as a non-binary spectrum with accommodation for atypical development. Each reading has a different beneficiary/victim structure and epsilon profile.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
