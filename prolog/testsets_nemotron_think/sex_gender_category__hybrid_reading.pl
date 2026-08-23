% ============================================================================
% CONSTRAINT STORY: sex_gender_category__hybrid_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sex_gender_category__hybrid_reading, []).

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
 *   constraint_id: sex_gender_category__hybrid_reading
 *   human_readable: Hybrid Medical Gatekeeping Model for Sex/Gender Category Membership
 *   domain: social_ontology/identity_politics/legal_classification
 *
 * SUMMARY:
 *   The hybrid reading of the sex/gender category kernel holds that legal
 *   category membership (woman/female) is determined by a combination of
 *   biological origin and completed medical transition. Trans women are
 *   conditionally included after satisfying medical gatekeeping requirements
 *   (hormone therapy, surgeries, legal documentation changes). This model
 *   concentrates authority in medical institutions (WPATH-aligned clinics,
 *   gender identity services, forensic evaluators) that certify 'sufficient'
 *   transition. Non-transitioning trans women are excluded from the category,
 *   as are trans people denied medical access. The constraint presents itself
 *   as a pragmatic compromise between biology-only and identity-only models,
 *   but operates as a tangled rope: it coordinates legal classification
 *   through an objective-seeming medical standard while extracting
 *   substantial costs from trans people and asymmetrically excluding those
 *   who cannot or do not medicalize.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sex_gender_category__hybrid_reading, 0.68).
domain_priors:suppression_score(sex_gender_category__hybrid_reading, 0.76).
domain_priors:theater_ratio(sex_gender_category__hybrid_reading, 0.32).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sex_gender_category__hybrid_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(sex_gender_category__hybrid_reading, suppression_requirement, 0.76).
narrative_ontology:constraint_metric(sex_gender_category__hybrid_reading, theater_ratio, 0.32).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(sex_gender_category__hybrid_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(sex_gender_category__hybrid_reading, resistance, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sex_gender_category__hybrid_reading, tangled_rope).
narrative_ontology:human_readable(sex_gender_category__hybrid_reading, "Hybrid Medical Gatekeeping Model for Sex/Gender Category Membership").
narrative_ontology:topic_domain(sex_gender_category__hybrid_reading, "social_ontology/identity_politics/legal_classification").

domain_priors:requires_active_enforcement(sex_gender_category__hybrid_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(sex_gender_category__hybrid_reading, '8b8fd783-6572-4b8a-9ca6-b6ebb516d366').
narrative_ontology:cs_kernel_codification('8b8fd783-6572-4b8a-9ca6-b6ebb516d366', formalized).
narrative_ontology:cs_authority_grounding('8b8fd783-6572-4b8a-9ca6-b6ebb516d366', expertise).
narrative_ontology:cs_interpretation_layer_present('8b8fd783-6572-4b8a-9ca6-b6ebb516d366').
narrative_ontology:cs_reading_relation('8b8fd783-6572-4b8a-9ca6-b6ebb516d366', sex_gender_category__biology_reading, coexists_with).
narrative_ontology:cs_reading_relation('8b8fd783-6572-4b8a-9ca6-b6ebb516d366', sex_gender_category__identity_reading, coexists_with).
narrative_ontology:cs_axiom('8b8fd783-6572-4b8a-9ca6-b6ebb516d366', foundational, medical_transition_grants_category_membership).
narrative_ontology:cs_axiom_status(medical_transition_grants_category_membership, holdable).
narrative_ontology:cs_axiom_grounding('8b8fd783-6572-4b8a-9ca6-b6ebb516d366', medical_transition_grants_category_membership, conventional).
narrative_ontology:cs_axiom('8b8fd783-6572-4b8a-9ca6-b6ebb516d366', foundational, biological_origin_retains_categorical_relevance).
narrative_ontology:cs_axiom_status(biological_origin_retains_categorical_relevance, holdable).
narrative_ontology:cs_axiom_grounding('8b8fd783-6572-4b8a-9ca6-b6ebb516d366', biological_origin_retains_categorical_relevance, conventional).
narrative_ontology:cs_axiom('8b8fd783-6572-4b8a-9ca6-b6ebb516d366', secondary, medical_certification_is_necessary_and_sufficient_for_transition_recognition).
narrative_ontology:cs_axiom_status(medical_certification_is_necessary_and_sufficient_for_transition_recognition, holdable).
narrative_ontology:cs_axiom_grounding('8b8fd783-6572-4b8a-9ca6-b6ebb516d366', medical_certification_is_necessary_and_sufficient_for_transition_recognition, empirically_contingent).
narrative_ontology:cs_reference_frame('8b8fd783-6572-4b8a-9ca6-b6ebb516d366', medical_gatekeeping_standard).
narrative_ontology:cs_drift_state('8b8fd783-6572-4b8a-9ca6-b6ebb516d366', contemporary_rights_based_challenge, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('8b8fd783-6572-4b8a-9ca6-b6ebb516d366', '').
narrative_ontology:cs_kernel_id(sex_gender_category__hybrid_reading, sex_gender_category).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sex_gender_category__hybrid_reading, medically_transitioned_trans_women).
narrative_ontology:constraint_beneficiary(sex_gender_category__hybrid_reading, cis_women_seeking_category_stability).
narrative_ontology:constraint_beneficiary(sex_gender_category__hybrid_reading, medical_gatekeeping_institutions).
narrative_ontology:constraint_victim(sex_gender_category__hybrid_reading, non_transitioning_trans_women).
narrative_ontology:constraint_victim(sex_gender_category__hybrid_reading, non_transitioning_trans_men).
narrative_ontology:constraint_victim(sex_gender_category__hybrid_reading, trans_people_denied_medical_access).
narrative_ontology:constraint_victim(sex_gender_category__hybrid_reading, detransitioners_with_ambiguous_status).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(sex_gender_category__hybrid_reading, medically_transitioned_trans_women).
narrative_ontology:constraint_vindicates(sex_gender_category__hybrid_reading, medical_oversight_ensures_authentic_transition).
narrative_ontology:constraint_vindicates(sex_gender_category__hybrid_reading, legal_category_requires_objective_verifiable_criteria).
narrative_ontology:constraint_vindicates(sex_gender_category__hybrid_reading, biological_origin_retains_categorical_significance).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Control the medical certification pathway (hormone therapy, surgeries, legal documentation) that gates category membership. Derive professional authority, institutional funding, and clinical volume from gatekeeping role. Set clinical protocols that determine who qualifies as 'sufficiently transitioned'.
narrative_ontology:constraint_stakeholder(sex_gender_category__hybrid_reading, medical_gatekeeping_institutions, agenda_setter,
    institutional, generational, analytical, national).

% Gain legal recognition as women after completing medical transition requirements. Bear substantial costs (financial, physical, temporal) of medical transition and ongoing compliance. Their inclusion is conditional on maintaining medical/legal documentation; exit from the category is structurally difficult once achieved.
narrative_ontology:constraint_stakeholder(sex_gender_category__hybrid_reading, medically_transitioned_trans_women, beneficiary,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(sex_gender_category__hybrid_reading, medically_transitioned_trans_women, payer).

% Identify as women but cannot or do not pursue medical transition (financial barriers, medical contraindications, personal choice, geographic lack of access). Legally categorized as men despite gender identity. Face exclusion from women's spaces, services, and legal protections. No viable exit from this mis-categorization under the hybrid model.
narrative_ontology:constraint_stakeholder(sex_gender_category__hybrid_reading, non_transitioning_trans_women, payer,
    powerless, biographical, trapped, national).

% Benefit from a legal category with defined boundaries that they see as protecting sex-based rights, spaces, and data collection. View medical gatekeeping as a necessary safeguard against category erosion. Can advocate for alternative models (biology-only) through political organizing; not personally constrained by the gatekeeping.
narrative_ontology:constraint_stakeholder(sex_gender_category__hybrid_reading, cis_women_seeking_category_stability, beneficiary,
    organized, generational, mobile, national).

% Trans people who need medical transition for category recognition but are barred by cost, geography, waitlists, medical gatekeeping denials, or comorbid conditions. Bear the double burden of gender dysphoria and legal mis-categorization. No exit without medical access that the system controls.
narrative_ontology:constraint_stakeholder(sex_gender_category__hybrid_reading, trans_people_denied_medical_access, payer,
    powerless, biographical, trapped, national).

% Legislatures, courts, and administrative agencies that codify and enforce the hybrid standard. Rely on medical certification as an 'objective' administrative criterion to avoid subjective adjudication. Face pressure from both biology-only and identity-only advocacy coalitions.
narrative_ontology:constraint_stakeholder(sex_gender_category__hybrid_reading, legal_administrative_bodies, agenda_setter,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(sex_gender_category__hybrid_reading, legal_administrative_bodies, observer).

% Advocate for biology-only categorization. View the hybrid model as an unstable compromise that concedes too much to gender identity ideology while still excluding some trans people. Their preferred reading (biology_reading) is a sibling constraint in the same kernel.
narrative_ontology:constraint_stakeholder(sex_gender_category__hybrid_reading, gender_critical_feminists, observer,
    organized, generational, analytical, national).

% Advocate for identity-only categorization (self-ID). View the hybrid model as pathologizing transness, imposing unjust medical barriers, and violating bodily autonomy. Their preferred reading (identity_reading) is a sibling constraint in the same kernel.
narrative_ontology:constraint_stakeholder(sex_gender_category__hybrid_reading, trans_rights_activists, observer,
    organized, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a legally administrable standard for sex/gender category membership that balances biological continuity with recognition of social transition, using medical certification as an objective gatekeeping mechanism to resolve classification disputes without case-by-case subjective adjudication.
% TRANSFER_FUNCTION: Moves the costs of medical transition (financial, physical, temporal, and psychological) onto trans individuals seeking category recognition; moves the burden of exclusion onto non-transitioning trans people who are legally mis-categorized; moves administrative certainty and professional authority to medical and legal institutions.
% ABSENT_VOICES: Non-transitioning trans people (especially those without medical access), trans people in jurisdictions without medical transition pathways, intersex people whose categorization is unresolved by this binary medical model, detransitioners whose category membership becomes ambiguous after medical reversal, trans youth whose access depends on parental consent and clinical protocols they cannot control.
% DISAPPEARANCE_RATIONALE: If the hybrid model vanished overnight, legal systems would immediately revert to either biology-only or identity-only standards. Millions would be re-categorized: medically transitioned trans women would lose recognition under biology-only, or gain it unconditionally under identity-only. Anti-discrimination law, sports classification, prison placement, healthcare access, and single-sex service provision would all require emergency restructuring.
% FOUNDING_PROBLEM: Legal systems in the late 20th century needed a workable standard for sex/gender classification that acknowledged trans existence without abandoning biological criteria entirely, following the emergence of medical transition protocols (Harry Benjamin Standards of Care, later WPATH) that offered a seemingly objective clinical pathway.
% FOUNDING_PROBLEM_CORROBORATION: Medical institutions and some legal scholars attest the problem remains live (ongoing need for objective criteria to prevent fraud and protect sex-based provisions). Trans rights organizations and international human rights bodies (Yogyakarta Principles, UN Independent Expert on SOGI) attest the founding problem is superseded by self-determination models; multiple national law reforms (Argentina 2012, Malta 2015, Iceland 2019, Spain 2023) demonstrate identity-based recognition works without the predicted harms, from outside the benefiting parties.
narrative_ontology:disappearance_verdict(sex_gender_category__hybrid_reading, world_rearranges).
narrative_ontology:founding_problem_status(sex_gender_category__hybrid_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(sex_gender_category__hybrid_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(sex_gender_category__hybrid_reading, 'none', 1).
narrative_ontology:epsilon_provenance(sex_gender_category__hybrid_reading, 0.68, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sex_gender_category__hybrid_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(sex_gender_category__hybrid_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(sex_gender_category__hybrid_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68) reflects the high costs imposed on trans people (medical, financial, temporal, bodily autonomy) and the exclusionary harm to non-transitioning trans people. Suppression (0.76) is high because the constraint's persistence depends on active medical-legal enforcement: certification requirements, diagnostic gatekeeping, legal documentation hurdles, and the structural impossibility of category access without institutional permission. Theater ratio (0.32) captures the growing performative gap: medical gatekeeping is increasingly framed as 'patient-centered care' while protocols remain rigid and access-denying. The measurement series tracks ~35 years (1990s-2020s): early period had lower extractiveness (fewer trans people seeking recognition, less developed medical infrastructure), rising as demand grew and protocols hardened; suppression increased as legal challenges forced more formalized gatekeeping; theater rose as institutional rhetoric softened while material barriers persisted.
 *
 * PERSPECTIVAL GAP:
 *   From the medical institution seat, the constraint appears as legitimate professional gatekeeping ensuring clinical appropriateness — a rope-like coordination function. From the non-transitioning trans person seat, it appears as a snare: coercive medicalization as the price of legal existence. From the medically transitioned trans woman seat, it appears as a tangled rope: real coordination (legal recognition achieved) but at extractive cost. The engine computes this divergence from the structural data: same constraint, different effective extraction per seat.
 *
 * DIRECTIONALITY LOGIC:
 *   Medical gatekeeping institutions are structural beneficiaries (d near 0.0): they collect professional authority, clinical volume, and institutional legitimacy from gatekeeping. Medically transitioned trans women are conditional beneficiaries with partial payer role (d ~0.3): they gain category membership but bear extraction costs. Non-transitioning trans people and those denied medical access are full targets (d near 1.0): they bear exclusion costs with no offsetting benefit. Cis women seeking category stability are beneficiaries (d ~0.15) with mobile exit (can advocate for alternative models). Legal/administrative bodies are agenda_setters with analytical exit (d ~0.1). The two observer seats (gender-critical feminists, trans rights activists) represent the sibling readings contesting this kernel.
 *
 * MANDATROPHY ANALYSIS:
 *   The hybrid model was founded to solve a genuine coordination problem: how to legally classify trans people without abandoning biological criteria. That founding problem is now contested — self-ID models demonstrate coordination without medical gatekeeping. The constraint persists despite the founding problem's contested status because medical institutions and legal systems have developed inertia around the gatekeeping infrastructure. This is mandatrophy: the mandate (medical certification as prerequisite) has outlived its coordinating function but persists through institutional capture and the absence of a concentrated beneficiary with both motive and power to dismantle it (medical institutions benefit; cis women's organizations are split; trans rights organizations lack institutional power).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is this constraint properly understood as one reading of the contested sex_gender_category kernel, or as an independent constraint?',
    'Comparative analysis of the three readings'' structural properties: if they share the same referent (legal category membership) but instantiate different ε, beneficiary/victim structures, and authority groundings, they are kernel readings. If they address different referents, they are independent constraints.',
    'If kernel readings, classification must be reading-indexed and networked via affects_constraints; if independent, each stands alone. Misidentification obscures the contest structure and misattributes extraction to the wrong constraint.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Whether the three category models are readings of one kernel or independent constraints.').

omega_variable(
    gatekeeping_necessity,
    'Is medical gatekeeping structurally necessary for the coordination function (legal administrability, fraud prevention), or is it extractive overhead that could be replaced by self-declaration with post-hoc verification?',
    'Natural experiment from jurisdictions that adopted self-ID (Argentina, Malta, Iceland, Spain, New Zealand): compare rates of fraudulent category claims, administrative burden, and rights violations before/after reform. If self-ID systems show no increase in fraud or administrative failure, gatekeeping is not necessary for coordination.',
    'If gatekeeping is unnecessary, the hybrid model''s extractiveness is almost entirely overhead — reclassifying toward snare. If necessary, part of measured extraction is genuine coordination cost.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(gatekeeping_necessity, empirical, 'Whether medical certification is a necessary coordination cost or extractive overhead.').

omega_variable(
    partial_inclusion_boundary,
    'Where exactly does the ''sufficient medical transition'' boundary fall, and how does its indeterminacy function as an extraction mechanism?',
    'Survey of clinical protocols across jurisdictions: variation in required surgeries, hormone duration, psychological assessments, and legal documentation. Measure correlation between boundary strictness and institutional revenue/authority.',
    'If the boundary is strategically indeterminate (tightened when trans visibility increases, relaxed when political pressure mounts), the indeterminacy itself is an extraction tool — the constraint extracts compliance through moving goalposts.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(partial_inclusion_boundary, conceptual, 'Whether the medical sufficiency boundary is a stable coordination standard or a manipulable extraction lever.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (legal barriers, medical access denial) or internalized (trans people self-excluding from category claims due to internalized gatekeeping norms)?',
    'Post-exit suppression trajectory: in jurisdictions that adopted self-ID, measure whether non-transitioning trans people immediately claim category membership or whether internalized gatekeeping norms persist. If suppression persists after structural barriers are removed, reclassify as partially internalized.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests — the target carries the suppression with them after formal exit.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism in interpersonal/institutional constraints.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sex_gender_category__hybrid_reading, 0, 35).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sex__tr_t0, sex_gender_category__hybrid_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement(sex__tr_t5, sex_gender_category__hybrid_reading, theater_ratio, 5, 0.22).
narrative_ontology:measurement(sex__tr_t10, sex_gender_category__hybrid_reading, theater_ratio, 10, 0.26).
narrative_ontology:measurement(sex__tr_t15, sex_gender_category__hybrid_reading, theater_ratio, 15, 0.29).
narrative_ontology:measurement(sex__tr_t20, sex_gender_category__hybrid_reading, theater_ratio, 20, 0.31).
narrative_ontology:measurement(sex__tr_t25, sex_gender_category__hybrid_reading, theater_ratio, 25, 0.32).
narrative_ontology:measurement(sex__tr_t30, sex_gender_category__hybrid_reading, theater_ratio, 30, 0.32).
narrative_ontology:measurement(sex__tr_t35, sex_gender_category__hybrid_reading, theater_ratio, 35, 0.32).

% Extraction over time
narrative_ontology:measurement(sex__be_t0, sex_gender_category__hybrid_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(sex__be_t5, sex_gender_category__hybrid_reading, base_extractiveness, 5, 0.52).
narrative_ontology:measurement(sex__be_t10, sex_gender_category__hybrid_reading, base_extractiveness, 10, 0.58).
narrative_ontology:measurement(sex__be_t15, sex_gender_category__hybrid_reading, base_extractiveness, 15, 0.62).
narrative_ontology:measurement(sex__be_t20, sex_gender_category__hybrid_reading, base_extractiveness, 20, 0.65).
narrative_ontology:measurement(sex__be_t25, sex_gender_category__hybrid_reading, base_extractiveness, 25, 0.67).
narrative_ontology:measurement(sex__be_t30, sex_gender_category__hybrid_reading, base_extractiveness, 30, 0.68).
narrative_ontology:measurement(sex__be_t35, sex_gender_category__hybrid_reading, base_extractiveness, 35, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(sex__su_t0, sex_gender_category__hybrid_reading, suppression_requirement, 0, 0.65).
narrative_ontology:measurement(sex__su_t5, sex_gender_category__hybrid_reading, suppression_requirement, 5, 0.68).
narrative_ontology:measurement(sex__su_t10, sex_gender_category__hybrid_reading, suppression_requirement, 10, 0.71).
narrative_ontology:measurement(sex__su_t15, sex_gender_category__hybrid_reading, suppression_requirement, 15, 0.73).
narrative_ontology:measurement(sex__su_t20, sex_gender_category__hybrid_reading, suppression_requirement, 20, 0.75).
narrative_ontology:measurement(sex__su_t25, sex_gender_category__hybrid_reading, suppression_requirement, 25, 0.76).
narrative_ontology:measurement(sex__su_t30, sex_gender_category__hybrid_reading, suppression_requirement, 30, 0.76).
narrative_ontology:measurement(sex__su_t35, sex_gender_category__hybrid_reading, suppression_requirement, 35, 0.76).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sex_gender_category__hybrid_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(sex_gender_category__hybrid_reading, 0.08).
narrative_ontology:affects_constraint(sex_gender_category__hybrid_reading, sports_classification_policy).
narrative_ontology:affects_constraint(sex_gender_category__hybrid_reading, prison_placement_policy).
narrative_ontology:affects_constraint(sex_gender_category__hybrid_reading, anti_discrimination_law_application).
narrative_ontology:affects_constraint(sex_gender_category__hybrid_reading, single_sex_service_provision).
narrative_ontology:affects_constraint(sex_gender_category__hybrid_reading, healthcare_access_protocols).
narrative_ontology:affects_constraint(sex_gender_category__hybrid_reading, identity_document_standards).

% DUAL FORMULATION NOTE:
% This constraint is one member of the sex_gender_category constraint family (kernel_id: sex_gender_category). The biology_reading (immutable biology) and identity_reading (self-identification) are sibling constraints. All three share the same referent (legal category membership criteria) but instantiate different ε values, beneficiary/victim structures, and authority groundings. The hybrid reading sits structurally between them: it adopts biology_reading's insistence on objective criteria but identity_reading's inclusion of trans people, resolving the tension through medical gatekeeping. This decomposition follows the ε-invariance principle: each reading has a stable ε that does not depend on the observable used to evaluate it.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(sex_gender_category__hybrid_reading, institutional, 0.15).
constraint_indexing:directionality_override(sex_gender_category__hybrid_reading, moderate, 0.35).
constraint_indexing:directionality_override(sex_gender_category__hybrid_reading, powerless, 0.92).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
