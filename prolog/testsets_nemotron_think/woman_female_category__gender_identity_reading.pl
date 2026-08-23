% ============================================================================
% CONSTRAINT STORY: woman_female_category__gender_identity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_woman_female_category__gender_identity_reading, []).

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
 *   constraint_id: woman_female_category__gender_identity_reading
 *   human_readable: Gender Identity Self-Identification Mandate for Woman/Female Category
 *   domain: political_philosophy/bioethics/gender_studies/law
 *
 * SUMMARY:
 *   This constraint story models the gender identity reading of the
 *   woman/female category kernel: the claim that category membership is
 *   determined solely by internal self-identification, independent of
 *   biological sex. This reading has been instantiated in law and policy
 *   across multiple jurisdictions (Argentina 2012, Malta 2015, Ireland 2015,
 *   Scotland 2022 blocked, US executive orders 2021-2024). The constraint
 *   operates as a tangled rope: it solves a genuine coordination problem
 *   (trans recognition/dignity) but extracts asymmetrically from cisgender
 *   women and girls in single-sex spaces, requiring active enforcement (legal
 *   mandates, institutional policies, social sanction) to suppress the
 *   sex-based alternative. The measurement series (2010-2024) shows rising
 *   extractiveness as self-ID mandates expanded beyond document changes into
 *   sports, prisons, shelters, and schools; rising theater as 'inclusion'
 *   rhetoric increasingly covers material displacement; and rising
 *   suppression as dissent is disciplined.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(woman_female_category__gender_identity_reading, 0.68).
domain_priors:suppression_score(woman_female_category__gender_identity_reading, 0.62).
domain_priors:theater_ratio(woman_female_category__gender_identity_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(woman_female_category__gender_identity_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(woman_female_category__gender_identity_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(woman_female_category__gender_identity_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(woman_female_category__gender_identity_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(woman_female_category__gender_identity_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(woman_female_category__gender_identity_reading, tangled_rope).
narrative_ontology:human_readable(woman_female_category__gender_identity_reading, "Gender Identity Self-Identification Mandate for Woman/Female Category").
narrative_ontology:topic_domain(woman_female_category__gender_identity_reading, "political_philosophy/bioethics/gender_studies/law").

domain_priors:requires_active_enforcement(woman_female_category__gender_identity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(woman_female_category__gender_identity_reading, 'bc826bb2-a430-4eb0-8654-00c96e1c1fc8').
narrative_ontology:cs_kernel_codification('bc826bb2-a430-4eb0-8654-00c96e1c1fc8', distributed).
narrative_ontology:cs_authority_grounding('bc826bb2-a430-4eb0-8654-00c96e1c1fc8', extraction).
narrative_ontology:cs_interpretation_layer_present('bc826bb2-a430-4eb0-8654-00c96e1c1fc8').
narrative_ontology:cs_reading_relation('bc826bb2-a430-4eb0-8654-00c96e1c1fc8', woman_female_category__sex_biology_reading, forecloses).
narrative_ontology:cs_reading_relation('bc826bb2-a430-4eb0-8654-00c96e1c1fc8', woman_female_category__hybrid_contextual_reading, influences).
narrative_ontology:cs_axiom('bc826bb2-a430-4eb0-8654-00c96e1c1fc8', foundational, gender_identity_determines_category_membership).
narrative_ontology:cs_axiom_status(gender_identity_determines_category_membership, holdable).
narrative_ontology:cs_axiom_grounding('bc826bb2-a430-4eb0-8654-00c96e1c1fc8', gender_identity_determines_category_membership, deontological).
narrative_ontology:cs_axiom('bc826bb2-a430-4eb0-8654-00c96e1c1fc8', secondary, sex_based_classifications_are_invalid_for_gender_categories).
narrative_ontology:cs_axiom_status(sex_based_classifications_are_invalid_for_gender_categories, holdable).
narrative_ontology:cs_axiom_grounding('bc826bb2-a430-4eb0-8654-00c96e1c1fc8', sex_based_classifications_are_invalid_for_gender_categories, deontological).
narrative_ontology:cs_reference_frame('bc826bb2-a430-4eb0-8654-00c96e1c1fc8', self_determination_framework).
narrative_ontology:cs_drift_state('bc826bb2-a430-4eb0-8654-00c96e1c1fc8', contemporary_gender_policy_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('bc826bb2-a430-4eb0-8654-00c96e1c1fc8', '').
narrative_ontology:cs_kernel_id(woman_female_category__gender_identity_reading, woman_female_category).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(woman_female_category__gender_identity_reading, transgender_individuals).
narrative_ontology:constraint_beneficiary(woman_female_category__gender_identity_reading, trans_women).
narrative_ontology:constraint_victim(woman_female_category__gender_identity_reading, cisgender_women).
narrative_ontology:constraint_victim(woman_female_category__gender_identity_reading, female_athletes).
narrative_ontology:constraint_victim(woman_female_category__gender_identity_reading, women_in_single_sex_spaces).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(woman_female_category__gender_identity_reading, trans_women).
narrative_ontology:constraint_victim(woman_female_category__gender_identity_reading, medical_professionals).
narrative_ontology:constraint_vindicates(woman_female_category__gender_identity_reading, gender_self_determination_right).
narrative_ontology:constraint_vindicates(woman_female_category__gender_identity_reading, identity_based_anti_discrimination).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Gain legal recognition and access to gender-segregated spaces, services, and protections matching their gender identity. Their ability to exit the constraint is minimal because their identity is constitutive of their self-concept; the constraint validates their existence. They organize politically but remain demographically small.
narrative_ontology:constraint_stakeholder(woman_female_category__gender_identity_reading, transgender_individuals, beneficiary,
    moderate, biographical, identity_locked, global).

% Specifically gain access to women's spaces (bathrooms, shelters, prisons, sports). Also bear costs: heightened scrutiny, violence risk, and political backlash. Their identity-locked exit means they cannot disentangle from the category even when the constraint generates hostility.
narrative_ontology:constraint_stakeholder(woman_female_category__gender_identity_reading, trans_women, beneficiary,
    moderate, biographical, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(woman_female_category__gender_identity_reading, trans_women, payer).

% Lose sex-based legal protections and single-sex spaces (shelters, prisons, sports, changing rooms). Organized feminist groups resist but face institutional capture and social sanction. Exit is constrained: they cannot leave womanhood, and political exit requires breaking with mainstream institutions.
narrative_ontology:constraint_stakeholder(woman_female_category__gender_identity_reading, cisgender_women, payer,
    organized, generational, constrained, global).

% Face direct competitive displacement in sports categories. Exit is constrained by athletic career investment and lack of alternative competitive structures. Bear concentrated costs while the constraint's benefits are diffuse.
narrative_ontology:constraint_stakeholder(woman_female_category__gender_identity_reading, female_athletes, payer,
    moderate, biographical, constrained, global).

% Prisoners, shelter residents, patients in intimate care settings. Cannot exit the spaces where the constraint operates. Bear the most acute costs (safety, privacy, dignity) with zero leverage. Their powerlessness is structural, not incidental.
narrative_ontology:constraint_stakeholder(woman_female_category__gender_identity_reading, women_in_single_sex_spaces, payer,
    powerless, immediate, trapped, local).

% Enact and enforce self-ID laws, policies, and guidelines. Capture the legitimacy gains of inclusion while offloading implementation costs onto frontline institutions (prisons, schools, sports bodies). Can arbitrage between jurisdictions and shift enforcement intensity.
narrative_ontology:constraint_stakeholder(woman_female_category__gender_identity_reading, institutions_policymakers, agenda_setter,
    institutional, generational, arbitrage, national).

% Argue that sex-based rights are being erased. Excluded from mainstream policy formation, deplatformed, and professionally sanctioned. Their exclusion is structural: the constraint's logic treats their position as hateful rather than contested. Exit from the conversation is constrained by professional and social penalties.
narrative_ontology:constraint_stakeholder(woman_female_category__gender_identity_reading, gender_critical_feminists, excluded,
    moderate, biographical, constrained, global).

% Have lived experience of gender transition and reversal. Their testimony challenges the constraint's foundational axiom. Structurally excluded from both trans advocacy and gender-critical spaces. Bear compound harms: medical, psychological, and epistemic.
narrative_ontology:constraint_stakeholder(woman_female_category__gender_identity_reading, detransitioners, excluded,
    powerless, biographical, trapped, global).

% Implement gender-affirming care protocols mandated by the constraint. Face professional discipline for dissent. Bear liability and ethical conflict costs. Exit is constrained by licensing, institutional employment, and professional identity.
narrative_ontology:constraint_stakeholder(woman_female_category__gender_identity_reading, medical_professionals, observer,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(woman_female_category__gender_identity_reading, medical_professionals, payer).

% Adjudicate conflicts between self-ID rights and sex-based rights. Their rulings shape the constraint's enforcement boundary. Hold analytical exit (can interpret narrowly or broadly) but institutional role compels engagement.
narrative_ontology:constraint_stakeholder(woman_female_category__gender_identity_reading, legal_authorities, agenda_setter,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(woman_female_category__gender_identity_reading, legal_authorities, observer).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the coordination problem of transgender individuals' legal and social recognition: ensures that gender category membership follows self-identification rather than external biological criteria, enabling trans people to access rights, spaces, and protections congruent with their gender identity.
% TRANSFER_FUNCTION: Moves the authority to define 'woman' and 'female' categories from biological criteria (chromosomes, gametes, anatomy) to individual self-declaration. Transfers recognition, access, and legal protection from sex-based frameworks to identity-based frameworks. The transfer is mandatory: institutions must recognize self-ID regardless of biological sex.
% ABSENT_VOICES: Gender-critical feminists, detransitioners, parents of gender-questioning youth, female athletes, and women in prisons/shelters are structurally excluded from policy formation. They would object to the erasure of sex-based categories but are kept out by institutional capture, professional sanctions, and the constraint's framing of dissent as hate.
% DISAPPEARANCE_RATIONALE: If self-ID mandates vanished overnight, legal systems would revert to sex-based classifications. Trans people would lose gender recognition certificates, access to single-sex spaces, and anti-discrimination protections tied to gender identity. Female-only spaces would be re-established on biological basis. Sports, prisons, and shelters would reorganize around sex. The transgender advocacy infrastructure would lose its primary legal lever.
% FOUNDING_PROBLEM: Transgender individuals faced systematic denial of legal recognition, healthcare access, and basic dignity under sex-based classification systems that treated their gender identity as irrelevant or pathological. They could not marry, access appropriate healthcare, use public facilities safely, or obtain identity documents matching their lived gender.
% FOUNDING_PROBLEM_CORROBORATION: International human rights bodies (UN Independent Expert on SOGI, WHO), major medical associations (WPATH, APA, AMA), and courts in multiple jurisdictions attest the founding problem remains live: trans people still face recognition gaps, healthcare barriers, and violence globally. Gender-critical feminists and some bioethicists contest the framing, arguing the problem was overstated or has been solved by existing protections.
narrative_ontology:disappearance_verdict(woman_female_category__gender_identity_reading, world_rearranges).
narrative_ontology:founding_problem_status(woman_female_category__gender_identity_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(woman_female_category__gender_identity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(woman_female_category__gender_identity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(woman_female_category__gender_identity_reading, 0.68, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(woman_female_category__gender_identity_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(woman_female_category__gender_identity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(woman_female_category__gender_identity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness (0.68) reflects that the constraint transfers substantial authority and resources: legal recognition, sports titles, prison placements, shelter access, data categories. The transfer is mandatory and non-reciprocal. Suppression (0.62) is high because the constraint's persistence depends on actively suppressing the sex-based alternative: deplatforming, professional sanctions, hate speech laws, institutional capture. Theater ratio (0.38) is moderate: the coordination function (trans dignity) is real, but a growing share of enforcement activity defends the mandate's reach into contexts where coordination rationale is weak (female sports, rape shelters). Accessibility collapse (0.45) is moderate: sex-based categories remain cognitively available and legally operative in many domains, but the self-ID framework makes them unspeakable in elite institutions. Resistance (0.58) is significant: organized feminism, parent groups, medical professionals, and some faith communities resist, but face severe institutional penalties.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter/beneficiary seat (trans advocates, institutions), the constraint is a rope: genuine coordination solving a recognition problem with minimal coercion. From the payer seats (cisgender women, female athletes, women in prisons), the same constraint computes as snare: extraction enforced by suppressing the sex-based alternative. The engine computes this divergence from the declared structural data — the claimed_type (tangled_rope) acknowledges both functions exist simultaneously.
 *
 * DIRECTIONALITY LOGIC:
 *   Transgender individuals (especially trans women) are structural beneficiaries: they collect recognition, access, and legal protection (d near 0.0). Cisgender women in single-sex spaces are structural targets: they bear displacement, safety risks, and loss of sex-based rights with constrained exit (d near 1.0). Female athletes and women in prisons/shelters are concentrated targets with trapped or constrained exit (d ~ 0.9-1.0). Institutions/policymakers are agenda-setters with arbitrage exit (d ~ 0.2). Gender-critical feminists and detransitioners are excluded: their structural position is outside the constraint's coordination logic, so directionality derivation does not apply cleanly; their suppression is the constraint's enforcement mechanism.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (trans recognition denial) remains live globally, so mandatrophy is not resolved. However, in jurisdictions where self-ID is fully implemented, the constraint shows mandatrophy signatures: theater rising, extraction expanding into contexts with weak coordination rationale (sports, intimate spaces), suppression hardening. The founding problem's status is contested: trans advocates say it's live; gender-critical feminists say it's solved by existing anti-discrimination law. This contestation is the kernel's structural motor.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    committer_kernel_reading_identity,
    'This constraint is one reading (gender_identity_reading) of the contested kernel woman_female_category. What structural elements do the sibling readings (sex_biology_reading, hybrid_contextual_reading) change, and where is the disagreement located?',
    'Decompose the kernel into three constraint stories with distinct ε, beneficiaries, victims, and claimed_type. Compare structural deltas: sex_biology_reading has victim set = trans individuals denied sex-based protections; beneficiary = cisgender women. hybrid_contextual_reading has context-dependent victim/beneficiary sets.',
    'If the kernel is treated as a single constraint, ε becomes observer-relative (violating ε-invariance). Decomposition reveals that gender_identity_reading has high ε on dignity harms to cisgender women in single-sex spaces, while sex_biology_reading has high ε on recognition harms to trans people. The disagreement is located in the category membership criterion itself — the kernel''s core variable.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(committer_kernel_reading_identity, conceptual, 'Committer frame: this story is one reading of a multi-reading kernel; sibling readings instantiate different constraints.').

omega_variable(
    coordination_extraction_boundary,
    'Is the coordination function (trans dignity/recognition) structurally separable from the extraction function (displacement of cisgender women in single-sex spaces), or does the constraint require the extraction to achieve the coordination?',
    'Natural experiment: jurisdictions with self-ID for documents only (Argentina, Malta) vs. self-ID for all single-sex spaces (proposed US Equality Act, Scottish GRR Bill). If trans recognition outcomes are equivalent but extraction on cisgender women differs, the functions are separable.',
    'If separable, the constraint is a tangled rope where extraction is a policy choice, not a coordination necessity. If inseparable, the measured extraction is the price of coordination itself, and the tangled_rope classification is structurally necessary.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_extraction_boundary, empirical, 'Whether the constraint''s coordination and extraction components can be disaggregated by policy design.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression (0.62) structural (legal mandates, institutional policies, professional sanctions) or internalized (self-censorship, fear of social ostracism, identity fusion with the constraint''s moral frame)?',
    'Post-exit suppression trajectory: measure suppression persistence among gender-critical feminists and detransitioners who have exited mainstream institutions. If suppression persists (online harassment, banking de-platforming, family estrangement), reclassify as partially internalized.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests — excluded agents carry the suppression with them after institutional exit, creating a diffusion effect the scalar metric misses.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism in interpersonal/institutional constraints.').

omega_variable(
    identity_lock_nature,
    'What specific identity-fusion mechanism binds transgender individuals to this constraint? Is it professional identity (career in advocacy), relational identity (community belonging), ideological identity (self-concept as ''authentic self''), or institutional identity (legal recognition as existential validation)?',
    'Longitudinal study of trans individuals who detransition or desist: does exit from the constraint correlate with identity restructuring? Compare with cisgender women''s exit options from sex-based categories.',
    'If identity-locked for trans individuals is ideological/relational (exit = identity dissolution), their directionality d remains near 0.0 regardless of extraction. If identity-locked for cisgender women in single-sex spaces is structural (exit = physical impossibility), their d remains near 1.0. The asymmetry drives the tangled_rope classification.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_lock_nature, conceptual, 'Identity-lock mechanism differentiation between beneficiary and victim seats.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(woman_female_category__gender_identity_reading, 0, 14).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(wfc_gir_tr_t0, woman_female_category__gender_identity_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(wfc_gir_tr_t2, woman_female_category__gender_identity_reading, theater_ratio, 2, 0.18).
narrative_ontology:measurement(wfc_gir_tr_t4, woman_female_category__gender_identity_reading, theater_ratio, 4, 0.25).
narrative_ontology:measurement(wfc_gir_tr_t6, woman_female_category__gender_identity_reading, theater_ratio, 6, 0.31).
narrative_ontology:measurement(wfc_gir_tr_t8, woman_female_category__gender_identity_reading, theater_ratio, 8, 0.35).
narrative_ontology:measurement(wfc_gir_tr_t10, woman_female_category__gender_identity_reading, theater_ratio, 10, 0.37).
narrative_ontology:measurement(wfc_gir_tr_t12, woman_female_category__gender_identity_reading, theater_ratio, 12, 0.38).
narrative_ontology:measurement(wfc_gir_tr_t14, woman_female_category__gender_identity_reading, theater_ratio, 14, 0.38).

% Extraction over time
narrative_ontology:measurement(wfc_gir_be_t0, woman_female_category__gender_identity_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(wfc_gir_be_t2, woman_female_category__gender_identity_reading, base_extractiveness, 2, 0.42).
narrative_ontology:measurement(wfc_gir_be_t4, woman_female_category__gender_identity_reading, base_extractiveness, 4, 0.51).
narrative_ontology:measurement(wfc_gir_be_t6, woman_female_category__gender_identity_reading, base_extractiveness, 6, 0.58).
narrative_ontology:measurement(wfc_gir_be_t8, woman_female_category__gender_identity_reading, base_extractiveness, 8, 0.63).
narrative_ontology:measurement(wfc_gir_be_t10, woman_female_category__gender_identity_reading, base_extractiveness, 10, 0.66).
narrative_ontology:measurement(wfc_gir_be_t12, woman_female_category__gender_identity_reading, base_extractiveness, 12, 0.67).
narrative_ontology:measurement(wfc_gir_be_t14, woman_female_category__gender_identity_reading, base_extractiveness, 14, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(wfc_gir_su_t0, woman_female_category__gender_identity_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(wfc_gir_su_t2, woman_female_category__gender_identity_reading, suppression_requirement, 2, 0.45).
narrative_ontology:measurement(wfc_gir_su_t4, woman_female_category__gender_identity_reading, suppression_requirement, 4, 0.52).
narrative_ontology:measurement(wfc_gir_su_t6, woman_female_category__gender_identity_reading, suppression_requirement, 6, 0.58).
narrative_ontology:measurement(wfc_gir_su_t8, woman_female_category__gender_identity_reading, suppression_requirement, 8, 0.6).
narrative_ontology:measurement(wfc_gir_su_t10, woman_female_category__gender_identity_reading, suppression_requirement, 10, 0.61).
narrative_ontology:measurement(wfc_gir_su_t12, woman_female_category__gender_identity_reading, suppression_requirement, 12, 0.62).
narrative_ontology:measurement(wfc_gir_su_t14, woman_female_category__gender_identity_reading, suppression_requirement, 14, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(woman_female_category__gender_identity_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(woman_female_category__gender_identity_reading, 0.08).
narrative_ontology:affects_constraint(woman_female_category__gender_identity_reading, woman_female_category__sex_biology_reading).
narrative_ontology:affects_constraint(woman_female_category__gender_identity_reading, woman_female_category__hybrid_contextual_reading).
narrative_ontology:affects_constraint(woman_female_category__gender_identity_reading, single_sex_space_protections).
narrative_ontology:affects_constraint(woman_female_category__gender_identity_reading, gender_recognition_law).
narrative_ontology:affects_constraint(woman_female_category__gender_identity_reading, sports_category_policy).
narrative_ontology:affects_constraint(woman_female_category__gender_identity_reading, prison_placement_policy).

% DUAL FORMULATION NOTE:
% This constraint (gender_identity_reading) and sex_biology_reading are dual formulations of the woman_female_category kernel: they share the same category label ('woman', 'female') but instantiate mutually exclusive membership criteria. The hybrid_contextual_reading attempts a synthesis but inherits instability from both parents. The ε values differ substantially: gender_identity_reading ε ≈ 0.68 (extraction on cisgender women), sex_biology_reading ε ≈ 0.72 (extraction on trans people), hybrid_contextual_reading ε ≈ 0.45 (contextual extraction).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(woman_female_category__gender_identity_reading, powerless, 0.95).
constraint_indexing:directionality_override(woman_female_category__gender_identity_reading, moderate, 0.75).
constraint_indexing:directionality_override(woman_female_category__gender_identity_reading, organized, 0.65).
constraint_indexing:directionality_override(woman_female_category__gender_identity_reading, institutional, 0.2).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
