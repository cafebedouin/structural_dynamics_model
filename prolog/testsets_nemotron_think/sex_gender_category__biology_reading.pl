% ============================================================================
% CONSTRAINT STORY: sex_gender_category__biology_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sex_gender_category__biology_reading, []).

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
 *   constraint_id: sex_gender_category__biology_reading
 *   human_readable: Sex/Gender Category Membership by Immutable Reproductive Biology
 *   domain: social/legal/identity
 *
 * SUMMARY:
 *   This constraint story instantiates the biology_reading of the
 *   sex_gender_category kernel: category membership is determined by
 *   immutable reproductive biology (chromosomes, anatomy at birth). Under
 *   this reading, trans women are excluded from the 'woman' category; cis
 *   women are the sole victim set for sex-based harms; intersex individuals
 *   are forced into the binary through medical/legal coercion; boundary
 *   enforcement is active and escalating (bathroom bills, sports bans, prison
 *   placement litigation, restrictions on gender marker changes). The
 *   constraint operates as a tangled rope: it provides genuine coordination
 *   (stable categories for law, medicine, sports) but extracts asymmetrically
 *   from trans, intersex, and nonbinary people who bear the costs of boundary
 *   maintenance. The engine will compute per-seat classifications from the
 *   structural data authored here.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sex_gender_category__biology_reading, 0.75).
domain_priors:suppression_score(sex_gender_category__biology_reading, 0.78).
domain_priors:theater_ratio(sex_gender_category__biology_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sex_gender_category__biology_reading, extractiveness, 0.75).
narrative_ontology:constraint_metric(sex_gender_category__biology_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(sex_gender_category__biology_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(sex_gender_category__biology_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(sex_gender_category__biology_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sex_gender_category__biology_reading, tangled_rope).
narrative_ontology:human_readable(sex_gender_category__biology_reading, "Sex/Gender Category Membership by Immutable Reproductive Biology").
narrative_ontology:topic_domain(sex_gender_category__biology_reading, "social/legal/identity").

domain_priors:requires_active_enforcement(sex_gender_category__biology_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(sex_gender_category__biology_reading, '6658c328-e175-4d50-92ed-87b61392659c').
narrative_ontology:cs_kernel_codification('6658c328-e175-4d50-92ed-87b61392659c', fixed_text).
narrative_ontology:cs_authority_grounding('6658c328-e175-4d50-92ed-87b61392659c', lineage).
narrative_ontology:cs_interpretation_layer_present('6658c328-e175-4d50-92ed-87b61392659c').
narrative_ontology:cs_reading_relation('6658c328-e175-4d50-92ed-87b61392659c', sex_gender_category__identity_reading, forecloses).
narrative_ontology:cs_reading_relation('6658c328-e175-4d50-92ed-87b61392659c', sex_gender_category__hybrid_reading, influences).
narrative_ontology:cs_axiom('6658c328-e175-4d50-92ed-87b61392659c', foundational, sex_is_immutable_biological_fact).
narrative_ontology:cs_axiom_status(sex_is_immutable_biological_fact, holdable).
narrative_ontology:cs_axiom_grounding('6658c328-e175-4d50-92ed-87b61392659c', sex_is_immutable_biological_fact, empirically_contingent).
narrative_ontology:cs_axiom('6658c328-e175-4d50-92ed-87b61392659c', foundational, women_category_excludes_male_sexed).
narrative_ontology:cs_axiom_status(women_category_excludes_male_sexed, holdable).
narrative_ontology:cs_axiom_grounding('6658c328-e175-4d50-92ed-87b61392659c', women_category_excludes_male_sexed, deontological).
narrative_ontology:cs_reference_frame('6658c328-e175-4d50-92ed-87b61392659c', classical_sex_binary_ontology).
narrative_ontology:cs_drift_state('6658c328-e175-4d50-92ed-87b61392659c', contemporary_gender_identity_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('6658c328-e175-4d50-92ed-87b61392659c', '').
narrative_ontology:cs_kernel_id(sex_gender_category__biology_reading, sex_gender_category).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sex_gender_category__biology_reading, cis_women).
narrative_ontology:constraint_beneficiary(sex_gender_category__biology_reading, sex_based_rights_advocates).
narrative_ontology:constraint_beneficiary(sex_gender_category__biology_reading, medical_gatekeeping_institutions).
narrative_ontology:constraint_victim(sex_gender_category__biology_reading, trans_women).
narrative_ontology:constraint_victim(sex_gender_category__biology_reading, trans_men).
narrative_ontology:constraint_victim(sex_gender_category__biology_reading, intersex_individuals).
narrative_ontology:constraint_victim(sex_gender_category__biology_reading, nonbinary_individuals).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(sex_gender_category__biology_reading, legal_institutions).
narrative_ontology:constraint_vindicates(sex_gender_category__biology_reading, biological_sex_binary).
narrative_ontology:constraint_vindicates(sex_gender_category__biology_reading, sex_based_protection_necessity).
narrative_ontology:constraint_vindicates(sex_gender_category__biology_reading, immutable_category_boundary).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Receive sex-based protections (Title IX, single-sex spaces, sports categories) grounded in biological definition. Their situation depends on the boundary holding; they face pressure to accept broader definitions but organize to maintain the biology-based line. Exit from this arrangement means losing legally recognized sex-specific provisions.
narrative_ontology:constraint_stakeholder(sex_gender_category__biology_reading, cis_women, beneficiary,
    organized, biographical, constrained, national).

% Excluded from 'woman' category under this reading. Denied access to women's spaces, sports, prisons, and sex-based protections. Face high boundary enforcement: bathroom bills, sports bans, legal challenges to gender marker changes. Exit requires either accepting misclassification or leaving jurisdictions with such laws; identity is fused to the contested category.
narrative_ontology:constraint_stakeholder(sex_gender_category__biology_reading, trans_women, payer,
    moderate, biographical, identity_locked, national).

% Classified as 'female' by birth biology despite male gender. Retain some sex-based protections (pregnancy accommodations) but lose male-privilege assumptions. Forced into women's spaces (prisons, shelters) against gender presentation. Bear costs of both misclassification and boundary enforcement.
narrative_ontology:constraint_stakeholder(sex_gender_category__biology_reading, trans_men, payer,
    moderate, biographical, identity_locked, national).

% Forced into binary categories by medical/legal systems that treat biological variation as disorder. Subject to non-consensual infant surgeries to fit binary. No exit from classification system; their bodies are the evidence against the binary but the constraint treats them as exceptions to be corrected.
narrative_ontology:constraint_stakeholder(sex_gender_category__biology_reading, intersex_individuals, payer,
    powerless, biographical, trapped, national).

% Entirely unrepresented by binary biology-based system. No legal recognition in most jurisdictions. Bear costs of misclassification in every institutional interaction (ID documents, healthcare, prisons, shelters). Exit requires jurisdictions with third-gender markers; most have none.
narrative_ontology:constraint_stakeholder(sex_gender_category__biology_reading, nonbinary_individuals, payer,
    powerless, biographical, trapped, national).

% Control diagnostic criteria, treatment access, and legal gender marker processes. WDS/APA standards gate transition-related care. Benefit from authority over category boundaries. Can shift between biology-reading and hybrid-reading positions as professional consensus evolves.
narrative_ontology:constraint_stakeholder(sex_gender_category__biology_reading, medical_gatekeeping_institutions, agenda_setter,
    institutional, generational, arbitrage, national).

% Enforce binary classification through ID documents, anti-discrimination law, prison placement, sports governance. Gain administrative simplicity and stability from fixed categories. Face litigation pressure from all sides; can modify rules but rarely abandon binary framework.
narrative_ontology:constraint_stakeholder(sex_gender_category__biology_reading, legal_institutions, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(sex_gender_category__biology_reading, legal_institutions, beneficiary).

% Organize politically to maintain biology-based definitions. View trans inclusion as erasure of sex-based rights. Fund litigation, lobby for bathroom bills and sports bans. Their political identity is constituted through defending this boundary.
narrative_ontology:constraint_stakeholder(sex_gender_category__biology_reading, sex_based_rights_advocates, beneficiary,
    organized, biographical, constrained, national).

% Advocate for identity-based or hybrid recognition. Excluded from policy-making tables where biology-reading dominates. Would object to the constraint's victim structure but are structurally kept out of the legislative/judicial processes that enforce it.
narrative_ontology:constraint_stakeholder(sex_gender_category__biology_reading, trans_rights_advocates, excluded,
    organized, biographical, constrained, national).

% Sees full structure: biology-reading as one of three contested readings of the sex/gender category kernel. Observes coordination function (stable categories for law/medicine) and extraction function (trans/intersex/nonbinary people bear costs of boundary maintenance). No stake in outcome.
narrative_ontology:constraint_stakeholder(sex_gender_category__biology_reading, analytical_observer, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides stable, administratively tractable categories for sex-based law (anti-discrimination, Title IX, prison placement, sports eligibility, single-sex spaces). Solves the coordination problem of 'who counts as a woman/man for institutional purposes' by anchoring to putatively objective biological criteria.
% TRANSFER_FUNCTION: Moves categorization authority and access to sex-segregated provisions from trans/intersex/nonbinary people to cis women and gatekeeping institutions. Trans women lose access to women's spaces/sports/protections; trans men lose male classification; intersex people lose bodily autonomy; institutions gain administrative simplicity and definitional control.
% ABSENT_VOICES: Trans youth (too young to organize), intersex infants (subject to surgical assignment without consent), detransitioners (used as evidence by both sides but rarely heard on their own terms), Global South gender-diverse communities (two-spirit, hijra, muxe, etc.) whose categories are erased by Western binary export.
% DISAPPEARANCE_RATIONALE: If biology-reading vanished overnight, legal sex classification would shift to identity-reading or hybrid-reading frameworks. Trans women would gain access to women's spaces/sports; trans men would be classified male; intersex people would gain bodily autonomy protections; nonbinary markers would proliferate. Institutions would face massive administrative transition. Sex-based rights advocates would lose their definitional anchor.
% FOUNDING_PROBLEM: Late 19th/early 20th century sexology and eugenics projects sought to ground social hierarchy in biological fact. The binary sex classification system was formalized to administer population control, marriage law, inheritance, and later anti-discrimination protections — all requiring administratively stable categories.
% FOUNDING_PROBLEM_CORROBORATION: Historians of science (Fausto-Sterling, Roughgarden) and critical legal scholars (Spade, Currah) document the administrative origins of binary sex classification. Medical historians note intersex management protocols were designed to fit bodies into binary, not discover binary in bodies. The biology-reading's own proponents cite 'immutable biology' as self-evident rather than historically constructed.
narrative_ontology:disappearance_verdict(sex_gender_category__biology_reading, world_rearranges).
narrative_ontology:founding_problem_status(sex_gender_category__biology_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(sex_gender_category__biology_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(sex_gender_category__biology_reading, 'none', 1).
narrative_ontology:epsilon_provenance(sex_gender_category__biology_reading, 0.75, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sex_gender_category__biology_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(sex_gender_category__biology_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(sex_gender_category__biology_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.75) is high because the constraint transfers categorization rights and access to sex-segregated provisions from gender-diverse populations to cis women and gatekeeping institutions. Suppression (0.78) is high and rising because persistence depends on active legal/medical enforcement — bathroom bills, sports bans, surgical requirements for gender markers, non-consensual intersex surgeries. Theater ratio (0.38) is moderate: the coordination function (administrative stability, sex-based protections) is real but a growing share of enforcement activity defends the boundary against identity-reading challenges rather than serving the coordination function. Accessibility collapse (0.65) reflects that alternative categorization schemes exist and function elsewhere but are actively suppressed in biology-reading jurisdictions. Resistance (0.72) is high from trans rights movements, medical associations shifting toward informed consent models, and international human rights bodies.
 *
 * PERSPECTIVAL GAP:
 *   From the biology-reading seat (cis women, gatekeepers), the constraint appears as a Mountain or Rope: biology is immutable, categories are natural, coordination is genuine. From the payer seats (trans/intersex/nonbinary people), the same structure operates as a Snare: active enforcement, suppressed alternatives, identifiable victims. The engine computes this divergence from the structural data — the claimed_type (tangled_rope) represents the analytical observer's structural assessment, not any single seat's experience.
 *
 * DIRECTIONALITY LOGIC:
 *   Cis women and sex-based rights advocates are structural beneficiaries (collect sex-based protections, political identity constituted through boundary defense — d near 0.2). Medical and legal gatekeeping institutions are agenda_setters who also benefit from administrative control (d ~0.15). Trans women, trans men, intersex, and nonbinary individuals are payers bearing the costs of exclusion, misclassification, and bodily autonomy violations (d near 0.85-0.95). Trans people's exit_options are identity_locked — their gender identity is fused to the contested category, making exit psychologically and socially prohibitive even where legal exit exists. Intersex and nonbinary people are trapped — no jurisdiction offers full exit from binary classification. The analytical observer sits at d=0.5 by definition.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (administrative stability for population governance) is contested: sex-based rights advocates say it remains live (biological differences require sex-specific law); trans rights advocates say it's dead (administrative stability can be achieved without binary biology). The constraint persists not because the founding problem is solved but because the biology-reading has become a political identity for gender-critical feminists and a governance convenience for institutions. Mandatrophy is unresolved — the arrangement has outlived its original administrative justification but acquired new legitimating narratives.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    biological_binary_empirical_status,
    'Does reproductive biology actually produce a clean, immutable binary at the level of chromosomes, gametes, and anatomy — or is the binary a taxonomic imposition on a bimodal distribution with significant intersex variation?',
    'Systematic review of intersex prevalence estimates, chromosomal variation frequencies, and developmental biology literature on sex differentiation pathways. Comparison of clinical diagnostic criteria across jurisdictions and eras.',
    'If biology is genuinely binary at the relevant level, the constraint''s coordination function has natural-law grounding (lower extractiveness). If biology is bimodal with enforced binariness, the constraint''s ''naturalness'' is constructed and extraction is higher than measured.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(biological_binary_empirical_status, empirical, 'Whether the biological referent actually supports the immutable binary claim.').

omega_variable(
    coordination_necessity_of_rigid_boundary,
    'Do the coordination functions (prison safety, sports fairness, single-sex spaces) actually require a rigid biology-based boundary, or could they be served by tailored policies (risk assessment, hormone thresholds, self-ID with safeguards) that don''t categorically exclude trans/intersex people?',
    'Natural experiments from jurisdictions with self-ID laws (Argentina, Malta, Denmark, parts of US): track prison incidents, sports outcomes, shelter access. Compare outcomes under biology-reading vs identity-reading regimes.',
    'If coordination works without rigid boundary, the biology-reading''s extraction is gratuitous — a snare using coordination as cover. If coordination fails without it, the constraint is a genuine tangled_rope with unavoidable tradeoffs.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coordination_necessity_of_rigid_boundary, empirical, 'Whether the claimed coordination benefits structurally require this specific boundary.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the high suppression measured (0.78) primarily structural (laws, medical gatekeeping, prison placement policies) or does it include a substantial internalized component (trans people internalizing exclusion, cis women internalizing boundary defense as feminist duty)?',
    'Longitudinal studies of trans people in jurisdictions that shift from biology-reading to identity-reading: does suppression experience decrease immediately (structural) or persist (internalized)? Cross-cultural comparison of internalized transphobia levels.',
    'If substantially internalized, effective suppression is higher than structural measure suggests — the constraint travels with the target after legal exit. This would increase χ for identity_locked agents beyond the structural derivation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs internalized suppression mechanism for identity-locked targets.').

omega_variable(
    kernel_reading_framing_underdetermination,
    'Does the biology-reading frame the kernel as ''biological sex classification'' (making the binary a discovery) or as ''legal sex assignment'' (making the binary a policy choice)? The framing determines whether intersex variation is ''noise'' or ''counterevidence''.',
    'Discourse analysis of biology-reading advocates'' language: do they speak of ''discovering'' sex or ''assigning'' sex? Legal history of sex classification statutes: were they enacted as recognition of biological fact or creation of administrative category?',
    'If the kernel is framed as discovery, the constraint claims Mountain-like naturalness (FSM candidate). If framed as assignment, it admits constructedness and the tangled_rope classification is stable. This is a conceptual omega — resolution changes the CS structure, not the metrics.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_framing_underdetermination, conceptual, 'Framing under-determination: discovery vs. assignment of the sex/gender category kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sex_gender_category__biology_reading, 2010, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sex__tr_t2010, sex_gender_category__biology_reading, theater_ratio, 2010, 0.25).
narrative_ontology:measurement(sex__tr_t2013, sex_gender_category__biology_reading, theater_ratio, 2013, 0.28).
narrative_ontology:measurement(sex__tr_t2016, sex_gender_category__biology_reading, theater_ratio, 2016, 0.31).
narrative_ontology:measurement(sex__tr_t2019, sex_gender_category__biology_reading, theater_ratio, 2019, 0.34).
narrative_ontology:measurement(sex__tr_t2022, sex_gender_category__biology_reading, theater_ratio, 2022, 0.36).
narrative_ontology:measurement(sex__tr_t2025, sex_gender_category__biology_reading, theater_ratio, 2025, 0.38).

% Extraction over time
narrative_ontology:measurement(sex__be_t2010, sex_gender_category__biology_reading, base_extractiveness, 2010, 0.55).
narrative_ontology:measurement(sex__be_t2013, sex_gender_category__biology_reading, base_extractiveness, 2013, 0.6).
narrative_ontology:measurement(sex__be_t2016, sex_gender_category__biology_reading, base_extractiveness, 2016, 0.65).
narrative_ontology:measurement(sex__be_t2019, sex_gender_category__biology_reading, base_extractiveness, 2019, 0.7).
narrative_ontology:measurement(sex__be_t2022, sex_gender_category__biology_reading, base_extractiveness, 2022, 0.73).
narrative_ontology:measurement(sex__be_t2025, sex_gender_category__biology_reading, base_extractiveness, 2025, 0.75).

% Suppression requirement over time
narrative_ontology:measurement(sex__su_t2010, sex_gender_category__biology_reading, suppression_requirement, 2010, 0.55).
narrative_ontology:measurement(sex__su_t2013, sex_gender_category__biology_reading, suppression_requirement, 2013, 0.6).
narrative_ontology:measurement(sex__su_t2016, sex_gender_category__biology_reading, suppression_requirement, 2016, 0.68).
narrative_ontology:measurement(sex__su_t2019, sex_gender_category__biology_reading, suppression_requirement, 2019, 0.72).
narrative_ontology:measurement(sex__su_t2022, sex_gender_category__biology_reading, suppression_requirement, 2022, 0.76).
narrative_ontology:measurement(sex__su_t2025, sex_gender_category__biology_reading, suppression_requirement, 2025, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sex_gender_category__biology_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(sex_gender_category__biology_reading, 0.08).
narrative_ontology:affects_constraint(sex_gender_category__biology_reading, sex_gender_category__hybrid_reading).
narrative_ontology:affects_constraint(sex_gender_category__biology_reading, sex_gender_category__identity_reading).
narrative_ontology:affects_constraint(sex_gender_category__biology_reading, legal_sex_classification).
narrative_ontology:affects_constraint(sex_gender_category__biology_reading, sports_eligibility_policy).
narrative_ontology:affects_constraint(sex_gender_category__biology_reading, prison_placement_policy).
narrative_ontology:affects_constraint(sex_gender_category__biology_reading, bathroom_access_law).

% DUAL FORMULATION NOTE:
% This constraint is one member of the sex_gender_category constraint family. The biology_reading claims the kernel is a biological discovery (low ε for coordination); the identity_reading claims the kernel is a social recognition (low ε for different coordination); the hybrid_reading claims a medical-gatekept middle. Their ε values differ substantially because they measure extraction against different referents and beneficiary/victim structures. They are linked via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(sex_gender_category__biology_reading, organized, 0.2).
constraint_indexing:directionality_override(sex_gender_category__biology_reading, powerless, 0.92).
constraint_indexing:directionality_override(sex_gender_category__biology_reading, moderate, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
