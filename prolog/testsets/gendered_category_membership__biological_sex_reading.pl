% ============================================================================
% CONSTRAINT STORY: gendered_category_membership__biological_sex_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_gendered_category_membership__biological_sex_reading, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: gendered_category_membership__biological_sex_reading
 *   human_readable: Biological Sex Marker as Category Boundary for 'Woman'
 *   domain: social_ontology/political_philosophy/bioethics
 *
 * SUMMARY:
 *   This constraint story instantiates ONE READING of a contested kernel
 *   about the definition of the social category 'woman'. The kernel is a
 *   stabilized but contested commitment: 'what makes someone a woman?' The
 *   biological-sex reading answers: immutable biological markers (chromosomal
 *   sex, reproductive anatomy at birth) determine membership. This reading
 *   has been the dominant legal and institutional framing for much of the
 *   20th century but faces sustained contestation from alternative readings
 *   grounded in gender identity and social role. The story analyzes the
 *   biological-sex reading in structural isolation — it is not a synthesis or
 *   an attempt to adjudicate between readings. The sibling readings
 *   (identity-based, social-role-based) are separate constraint stories with
 *   different ε values, different stakeholder structures, and different
 *   persistence mechanisms. This story models the biological-sex reading as a
 *   tangled-rope constraint: it coordinates genuine interests (stable
 *   category enabling policy design, protections for cisgender women) AND
 *   extracts from excluded parties (trans women, non-binary AFAB individuals)
 *   through identity suppression and exclusion from protected spaces and
 *   legal recognition. The claim/metric gap is intentional: the
 *   biological-sex reading is CLAIMED as natural-law foundation (biology
 *   determines category membership) while the authored metrics describe
 *   substantially extractive, actively enforced institutional practice. The
 *   engine measures that divergence through the structural data.
 *
 * KEY AGENTS:
 *   - cisgender_women: primary beneficiary of category protection and access to sex-segregated spaces; secondary payer through boundary-maintenance costs
 *   - trans_women: primary victim; identity-locked excluded from the category regardless of transition history or legal recognition elsewhere
 *   - non_binary_assigned_female_at_birth: structurally invisible; anatomically included but identity-excluded, forced into contradictory position
 *   - sex_segregated_institutional_operators: agenda-setters enforcing the boundary daily; bear resource costs of verification and exclusion procedures
 *   - medical_diagnostic_authorities: definitional authority; control the official characterization of 'biological sex' and its measurement
 *   - legislatures_and_courts: institutional agenda-setters encoding the reading into law; face direct contestation via litigation
 *   - trans_rights_advocates: excluded from definitional authority despite civil-society voice; their exclusion is part of the constraint's enforcement
 *   - feminist_philosophy_establishments: beneficiaries of category stability; intellectual and career structures built on bounded-group framework
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gendered_category_membership__biological_sex_reading, 0.68).
domain_priors:suppression_score(gendered_category_membership__biological_sex_reading, 0.76).
domain_priors:theater_ratio(gendered_category_membership__biological_sex_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gendered_category_membership__biological_sex_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(gendered_category_membership__biological_sex_reading, suppression_requirement, 0.76).
narrative_ontology:constraint_metric(gendered_category_membership__biological_sex_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gendered_category_membership__biological_sex_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(gendered_category_membership__biological_sex_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gendered_category_membership__biological_sex_reading, tangled_rope).
narrative_ontology:human_readable(gendered_category_membership__biological_sex_reading, "Biological Sex Marker as Category Boundary for 'Woman'").
narrative_ontology:topic_domain(gendered_category_membership__biological_sex_reading, "social_ontology/political_philosophy/bioethics").

domain_priors:requires_active_enforcement(gendered_category_membership__biological_sex_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gendered_category_membership__biological_sex_reading, 'f2f51047-bde4-4abc-9cea-a46be1591326').
narrative_ontology:cs_kernel_codification('f2f51047-bde4-4abc-9cea-a46be1591326', fixed_text).
narrative_ontology:cs_authority_grounding('f2f51047-bde4-4abc-9cea-a46be1591326', extraction).
narrative_ontology:cs_interpretation_layer_present('f2f51047-bde4-4abc-9cea-a46be1591326').
narrative_ontology:cs_reading_relation('f2f51047-bde4-4abc-9cea-a46be1591326', gendered_category_membership__identity_reading, forecloses).
narrative_ontology:cs_reading_relation('f2f51047-bde4-4abc-9cea-a46be1591326', gendered_category_membership__social_role_reading, influences).
narrative_ontology:cs_axiom('f2f51047-bde4-4abc-9cea-a46be1591326', foundational, chromosomal_sex_determines_categorical_membership).
narrative_ontology:cs_axiom_status(chromosomal_sex_determines_categorical_membership, holdable).
narrative_ontology:cs_axiom_grounding('f2f51047-bde4-4abc-9cea-a46be1591326', chromosomal_sex_determines_categorical_membership, empirically_contingent).
narrative_ontology:cs_axiom('f2f51047-bde4-4abc-9cea-a46be1591326', foundational, reproductive_anatomy_at_birth_is_identity_marker).
narrative_ontology:cs_axiom_status(reproductive_anatomy_at_birth_is_identity_marker, holdable).
narrative_ontology:cs_axiom_grounding('f2f51047-bde4-4abc-9cea-a46be1591326', reproductive_anatomy_at_birth_is_identity_marker, empirically_contingent).
narrative_ontology:cs_reference_frame('f2f51047-bde4-4abc-9cea-a46be1591326', immutable_biological_sex_categories).
narrative_ontology:cs_drift_state('f2f51047-bde4-4abc-9cea-a46be1591326', contemporary_trans_visibility_and_legal_recognition_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('f2f51047-bde4-4abc-9cea-a46be1591326', '').
narrative_ontology:cs_kernel_id(gendered_category_membership__biological_sex_reading, gendered_category_membership).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gendered_category_membership__biological_sex_reading, cisgender_women).
narrative_ontology:constraint_victim(gendered_category_membership__biological_sex_reading, trans_women).
narrative_ontology:constraint_victim(gendered_category_membership__biological_sex_reading, non_binary_individuals_assigned_female_at_birth).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(gendered_category_membership__biological_sex_reading, feminist_philosophy_establishments).
narrative_ontology:constraint_victim(gendered_category_membership__biological_sex_reading, cisgender_women).
narrative_ontology:constraint_victim(gendered_category_membership__biological_sex_reading, non_binary_assigned_female_at_birth).
narrative_ontology:constraint_vindicates(gendered_category_membership__biological_sex_reading, chromosomal_sex_determines_ontological_category).
narrative_ontology:constraint_vindicates(gendered_category_membership__biological_sex_reading, reproductive_anatomy_at_birth_is_immutable_identity_marker).
narrative_ontology:constraint_vindicates(gendered_category_membership__biological_sex_reading, sex_segregated_spaces_protect_bounded_group_interests).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Claim protected status within the 'woman' category through verified biological markers (chromosomal sex XX, reproductive anatomy at birth). Receive coordinated access to sex-segregated spaces (bathrooms, shelters, sports leagues, domestic violence services, prisons) justified as protecting bounded group interests and privacy. Also bear the cost of continuous boundary-policing: verification procedures, documentation requirements, and responsive litigation defending the category's exclusivity. Some cisgender women experience identity suppression through rigid biologism if their anatomy deviates from stereotypical form, though the constraint's primary protective effect accrues to the majority presentation.
narrative_ontology:constraint_stakeholder(gendered_category_membership__biological_sex_reading, cisgender_women, beneficiary,
    organized, generational, mobile, global).
narrative_ontology:stakeholder_secondary_role(gendered_category_membership__biological_sex_reading, cisgender_women, payer).

% Excluded from the legal and social 'woman' category under this reading, regardless of identity, transition history, or legal recognition in other jurisdictions. Denied access to sex-segregated spaces, services, and protections justified as preserving biological boundaries. The exclusion is framed as ontological fact ('you are not biologically woman') rather than social choice, which forecloses appeal to fairness or consensus. Exit would require either accepting exclusion or abandoning their self-identity; identity_locked captures that the boundary-definition treats their core identity claim as incoherent rather than merely unwelcome.
narrative_ontology:constraint_stakeholder(gendered_category_membership__biological_sex_reading, trans_women, payer,
    powerless, biographical, identity_locked, global).

% Structurally invisible within this reading: assigned female at birth (anatomy-based category inclusion) but rejecting 'woman' identity (ontological boundary-crossing). The constraint offers no coherent position — they are biologically included but identity-excluded, creating a structural bind. Sex-segregated spaces access becomes a forced alignment with an identity they reject, or exclusion on identity grounds despite anatomical qualification.
narrative_ontology:constraint_stakeholder(gendered_category_membership__biological_sex_reading, non_binary_assigned_female_at_birth, payer,
    powerless, biographical, identity_locked, global).

% Implement and enforce the biological-marker boundary in daily practice: verify sex at entry to bathrooms, locker rooms, shelters, detention facilities, and sports competition. Operate the verification machinery (document checks, anatomical inspection in some contexts, exclusion procedures). Have discretion in how strictly to apply the rule but face litigation and institutional pressure to maintain the boundary. The enforcement function is resource-intensive and increasingly contested; the institutional position has shifted from assumption to active defense.
narrative_ontology:constraint_stakeholder(gendered_category_membership__biological_sex_reading, sex_segregated_institutional_operators, agenda_setter,
    institutional, generational, constrained, national).

% Define and certify the biological markers (chromosomal sex karyotype, reproductive anatomy) that instantiate the category. Control the official terminology (DSM-5, ICD-11) and recognition protocols. This reading treats their definitions as natural-law fact; they are actually subject to revision and contestation (intersex conditions, sex chromosome variation, developmental anomalies complicate binary classification). Hold interpretive authority over 'biological sex' itself, though that authority is under pressure from empirical complexity and from alternative readings that locate identity elsewhere.
narrative_ontology:constraint_stakeholder(gendered_category_membership__biological_sex_reading, medical_diagnostic_authorities, agenda_setter,
    institutional, generational, constrained, global).

% Encode the biological-marker reading into law and policy: sex-segregated facility requirements, bathroom access rules, sports eligibility standards, legal sex recognition thresholds. Face direct contestation via litigation challenging the boundary's necessity and justification. Increasingly split across jurisdictions: some enforce the boundary strictly, others recognize legal sex change independent of anatomy, creating parallel enforcement regimes. The constraint's persistence in any given jurisdiction depends on legislative/judicial institutional commitment to the reading.
narrative_ontology:constraint_stakeholder(gendered_category_membership__biological_sex_reading, legislatures_and_courts, agenda_setter,
    institutional, generational, constrained, national).

% Argue for the identity-based or social-role readings as alternatives. Are largely excluded from institutional authority positions that define the category (medicine, law, institutional management), though they hold substantial voice in civil society, academia (gender studies, sociology), and some professional guilds (psychology, social work). Their exclusion from the definitional seat is itself the constraint's enforcement mechanism — the category boundary is defended partly by controlling who speaks authoritatively about what 'woman' means.
narrative_ontology:constraint_stakeholder(gendered_category_membership__biological_sex_reading, trans_rights_advocates_and_allied_scholars, excluded,
    moderate, biographical, mobile, global).

% Occupy the complementary category 'man' under the same biological-marker logic. Benefit from parallel sex-segregation in spaces and services; protected by the same boundary-maintenance logic applied to the adjacent category. Analytically positioned outside the constraint (not victims, not beneficiaries of the woman-category specifically), but the reading's implications extend symmetrically to them.
narrative_ontology:constraint_stakeholder(gendered_category_membership__biological_sex_reading, cisgender_men, observer,
    organized, generational, mobile, global).

% Institutions (academic departments, professional conferences, research networks) that have built intellectual and career structures around 'woman' as a stable, biologically grounded category. The reading vindicates decades of scholarship grounded in a bounded group identity; alternative readings that dissolve or pluralize the category threaten the intellectual coherence and career paths built on the old framework. Benefit from the category's legal and social persistence, though some feminist scholars advocate for the alternative readings.
narrative_ontology:constraint_stakeholder(gendered_category_membership__biological_sex_reading, feminist_philosophy_establishments, beneficiary,
    organized, generational, mobile, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(gendered_category_membership__biological_sex_reading, cisgender_women).
narrative_ontology:fixing_cost_class(gendered_category_membership__biological_sex_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Sustains and protects a social category ('woman') claimed as rooted in immutable biological fact. Enables single-axis policy design: sex-segregated spaces, gender-based legal protections, reproductive rights frameworks, and sports eligibility rules all rely on a stable, testable boundary. Reduces coordination costs for institutions that need to allocate resources, protections, or restrictions along a sex axis — they appeal to biology rather than negotiating contested social values each time.
% TRANSFER_FUNCTION: Moves authority over the category definition from individual identity-claim to institutional medical/legal gatekeeping. Transfers the cost of boundary-policing (verification, exclusion, litigation defense) onto institutional operators and excluded parties. Transfers epistemic authority to medical-diagnostic authorities and lawmakers to define who counts as 'woman' rather than granting individuals that definitional power. Trans women and non-binary AFAB individuals transfer access to sex-segregated protections and spaces; cisgender women gain the benefit of a bounded, legally recognized group but bear the cost of continuous boundary-maintenance.
% ABSENT_VOICES: Trans women and non-binary AFAB individuals are present in the story but structurally excluded from the seat that defines the category. Advocates for alternative readings (identity-based, social-role-based, plural-axes frameworks) are present in civil society but largely excluded from institutional authority positions where the boundary is actually defined and enforced. Medical practitioners who work with intersex conditions and sex-chromosome variation are aware of empirical complexity that complicates the binary, but the constraint suppresses that knowledge by treating biology as simpler than it is — they stay largely silent to avoid delegitimizing the category they are officially tasked to certify.
% DISAPPEARANCE_RATIONALE: If the biological-marker reading as law and institutional practice vanished overnight, sex-segregated policy frameworks would require rapid redesign (what boundary principle replaces biology?), trans women would gain legal category membership in most jurisdictions, sports eligibility and bathroom access would become contested institutional design choices rather than biology-justified facts, and institutional operators would lose the appeal to natural fact that currently forecloses re-negotiation. The category 'woman' itself would not vanish, but its legal meaning and enforcement basis would undergo fundamental reorganization — arrangements that currently depend on biological closure would rearrange.
% FOUNDING_PROBLEM: Biological sex differences have real implications for reproduction, athletic performance variation, and medical treatment (pregnancy-specific care, hormone-responsive conditions). Early feminist and civil rights frameworks needed a stable category to anchor legal protections (right to reproductive autonomy, protection from sexual violence, equal treatment in employment). The biological-marker reading solved that by rooting 'woman' in immutable fact rather than male-defined roles ('woman' = confinement to domesticity) or mere social preference. The reading vindicates the insight that a stable, legally bounded category enables concentrated protections.
% FOUNDING_PROBLEM_CORROBORATION: Cisgender women and medical authorities affirm that reproductive biology has ongoing implications for medicine and policy. Feminist scholars grounded in this reading affirm that category stability enabled effective rights frameworks. Trans-rights advocates and allied scholars attest that the founding problem (need for legal protections) has been solved and is now sustained independently of category-boundary definition — protections for reproductive autonomy, safety, and employment can be granted via alternative framings (identity-recognized groups, explicitly specified rights rather than category-derived rights). Legal jurisdictions that have separated legal sex recognition from anatomy report stable outcomes and effective protections, suggesting the founding problem's urgency has shifted. No corroboration is offered from trans women themselves regarding whether the current boundary definition serves their interests (it does not, by structural position).
narrative_ontology:disappearance_verdict(gendered_category_membership__biological_sex_reading, world_rearranges).
narrative_ontology:founding_problem_status(gendered_category_membership__biological_sex_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gendered_category_membership__biological_sex_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(gendered_category_membership__biological_sex_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gendered_category_membership__biological_sex_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(gendered_category_membership__biological_sex_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(gendered_category_membership__biological_sex_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.68 at interval end) because the constraint moves definitional authority from trans women to medical/legal gatekeepers, denies legal recognition to trans women even where identity-based recognition exists in other jurisdictions, and enforces identity suppression by treating trans women's self-identity as ontologically false rather than socially contested. The trajectory shows rising extractiveness from 1970 to 2026 (0.45 → 0.68): initially the reading was assumed natural-law fact requiring no defense; as alternative readings emerged and trans visibility increased, enforcement machinery intensified (institutional verification, litigation defense, legislative codification), converting the constraint from background assumption to active extraction. Suppression is very high (0.76) because alternatives to the biological-reading are not merely disfavored but legally foreclosed — trans women cannot simply exit the category by declaring themselves women in most jurisdictions; they must accept exclusion or engage in extended legal battle. The constraint suppresses knowledge of empirical complexity (intersex conditions, sex-chromosome variation, reproductive anatomy variation in cisgender women) by treating biology as simpler and more determinate than it is. Theater ratio is moderate (0.41 at interval end, rising from 0.18 at baseline) because the institutional focus has shifted from the original coordination function (stable policy categories) toward boundary-maintenance itself — an increasing share of enforcement activity defends the category exclusivity rather than serves the reproductive and medical-care functions that originally justified the category. Accessibility collapse is moderate-high (0.72): once the biological-reading is understood and institutionalized, alternatives become difficult to access — trans women must either accept the excluded position or relocate to jurisdictions with alternative readings, requiring substantial exit cost. Resistance is high (0.71): the constraint meets sustained resistance from trans-rights advocates, progressive scholars, and institutional reformers who advocate for alternative readings; the rising suppression_requirement over time reflects intensifying defense against this resistance.
 *
 * PERSPECTIVAL GAP:
 *   The beneficiary and victim seats should compute dramatically differently under the engine's per-seat classification rules. From the cisgender-women seat: the constraint is a genuine coordination solution enabling group protection, and any extractiveness reflects necessary boundary-maintenance cost. From the trans-women seat: the constraint is pure extraction disguised as natural fact; the 'coordination' function accrues only to the in-group and depends wholly on their exclusion. Institutional operators should compute as enforcing an increasingly costly regime whose original coordination rationale has been hollowed out by rising theater ratio — the enforcement machinery persists primarily to defend the category boundary itself rather than serve the reproductive, safety, and medical functions that initially justified it. This perspectival divergence is NOT an error in the authoring; it is exactly what the per-seat classification apparatus is designed to measure. The claim (natural-law foundation) diverges from the metrics (high extraction, high suppression, rising theater) — that divergence is the story's measurement: a constraint presented as natural law operates as enforced extraction once institutional variation is accounted for.
 *
 * DIRECTIONALITY LOGIC:
 *   Cisgender women sit near the beneficiary end (d ≈ 0.25-0.35): they benefit from a stable, legally protected category, though they also pay through boundary-maintenance costs. Their power is organized and their exit options are mobile — they could in principle organize around alternative framings, but the current reading vindicates their interests as a stable group. Trans women sit at the full-target end (d ≈ 0.95): the constraint extracts from them through exclusion and identity suppression; their power is powerless and their exit options are identity-locked — they cannot exit the category through exit-option choice, only through accepting exclusion or abandoning their identity claim. The directionality asymmetry between beneficiaries and victims is the core tangled-rope signature: the same constraint that coordinates a bounded group protection simultaneously suppresses and excludes another group. Institutional operators (medical authorities, legislatures, institutional managers) sit at high d (0.75-0.85) in their administrative capacity — they are tasked with enforcing the boundary and bear rising suppression costs as resistance increases. This is not directionality per se (they are not targets being extracted from) but rather institutional position asymmetry: they are agenda-setters whose primary function has shifted from coordination to enforcement, and the enforcement function is becoming increasingly resource-intensive and contested.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (need for stable legal categories enabling focused protections for biological-sex-differentiated needs) had genuine urgency in 1970 when the reading emerged. By 2005-2015, the founding problem's urgency had shifted: reproductive rights, safety protections, and medical care had become decoupled from category-boundary definition in many jurisdictions (legal sex recognition independent of anatomy in some countries; reproductive protections grounded explicitly in reproductive capacity rather than category membership; domestic-violence and sexual-assault protections grounded in vulnerability rather than sex category). The constraint's persistence from 2015-2026 increasingly reflects institutional inertia and extracted rents (medical authorities defending diagnostic authority, institutional operators defending boundary-maintenance budgets) rather than active coordination around the founding problem. The rising theater_ratio (0.18 → 0.41) models this drift: an increasing share of enforcement activity is theatrical (defending the category's boundary for its own sake) rather than functional (solving the founding problem the category originally addressed). This is the classic mandatrophy signature: the founding problem is largely solved or has shifted, but the institutional arrangement persists because beneficiaries have reorganized around defending the category boundary itself rather than solving the original problem. A mandatrophy-resolved verdict is appropriate for the 2020-2026 interval.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    biological_determination_empirical_adequacy,
    'Is chromosomal sex and reproductive anatomy at birth a coherent, measurable, binary category sufficient to ground policy, or is ''biological sex'' empirically more complex (intersex conditions, sex-chromosome variation, reproductive anatomy variation) than the reading assumes?',
    'Empirical medical literature on sex-determined variation, intersex prevalence and variation, and the limits of binary classification; endocrinology and reproductive physiology research on the plurality of biochemical sex expressions.',
    'If biological sex is empirically simpler and more binary than suggested by medical literature, the reading''s authority strengthens. If biological sex is empirically more plural and continuous, the constraint depends on institutional suppression of empirical complexity — which would elevate both extractiveness and theater_ratio and weaken the natural-law framing.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(biological_determination_empirical_adequacy, empirical, 'Whether the constraint''s appeal to ''biological sex'' matches actual biological complexity or requires suppression of known variation.').

omega_variable(
    identity_locked_exit_mechanism,
    'Is the identity_locked exit category accurate for trans women under this reading, or is there a meaningful path to category membership or exemption that the constraint structurally forecloses?',
    'Institutional audit of trans women''s actual options under this reading: legal recognition procedures, space-access mechanisms, challenge and appeals processes. Does any institutional pathway exist for trans women to gain category membership while maintaining trans identity, or does the reading require either acceptance of exclusion or identity abandonment?',
    'If pathways exist that the story misses, directionality for trans women would adjust downward (trapped rather than identity_locked, or constrained rather than trapped). If no pathway exists, identity_locked is correct and the suppression assessment stands. This affects the computed extraction and theater ratio — whether the constraint is closure through definition or closure through active institutional barrier.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(identity_locked_exit_mechanism, empirical, 'Whether trans women have available institutional pathways for category membership or are structurally closed out.').

omega_variable(
    coordination_function_persistence,
    'Is the coordination function (stable category enabling policy design and group protections) still the operative justification for the constraint, or has institutional focus shifted entirely to boundary-maintenance as an end in itself?',
    'Content analysis of institutional rhetoric over time: do policy justifications cite reproductive medicine, safety protections, and equal treatment (original coordination functions) or do they increasingly cite boundary-preservation itself? Examination of institutional budget allocations and litigation costs devoted to boundary-defense versus functional policy outcomes.',
    'If the coordination function remains the primary justification, the constraint is still tangled_rope (genuine coordination plus extraction). If rhetoric and budgets have shifted to pure boundary-maintenance, the constraint should reclassify as snare with a theater_ratio veneer of coordination. This is the core mandatrophy test: is the founding problem still being solved, or is the institutional apparatus now defending the category for its own sake?',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_function_persistence, empirical, 'Whether the constraint''s primary function is coordination or boundary-maintenance theater.').

omega_variable(
    reading_logical_independence,
    'Does the biological-sex reading genuinely coexist with the identity-reading and social-role-reading, or does accepting one reading logically foreclose the others within a single institutional framework?',
    'Examine whether jurisdictions that have adopted the identity-reading (legal sex recognition independent of anatomy) have experienced institutional incoherence, or whether alternative readings can be integrated into a single coherent legal framework operating with different category-boundaries for different policy purposes.',
    'If readings are truly coexistent (different jurisdictions, different policies), then reading_relations should be ''coexists_with''. If accepting one reading logically forecloses others in any single framework, then relation should be ''forecloses''. This affects how the three readings should be modeled as a constraint family — whether they are parallel options or mutually exclusive commitments.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_logical_independence, conceptual, 'Whether the three readings of the kernel are logically coexistent or mutually foreclosing.').

omega_variable(
    trans_women_suppression_mechanism_structural_vs_internalized,
    'Is the measured suppression (0.76) primarily structural (legal barriers, institutional exclusion) or partly internalized (trans women absorbing the constraint''s identity-negation and self-excluding)?',
    'Post-transition institutional analysis: where trans women have legal recognition and institutional access (jurisdictions with identity-based reading), do they maintain suppression beliefs about their own category membership, or does suppression decay with barrier removal? Interview data on trans women''s self-concept trajectory in different institutional contexts.',
    'If suppression is primarily structural, the effective suppression remains high as long as institutional barriers persist. If suppression is partly internalized, then even where institutional barriers are removed (identity-reading jurisdictions), trans women may carry suppression patterns from previous institutional exposure — the constraint''s extraction follows trans women across jurisdictional boundaries. This affects the measurement''s portability: suppression measured in biological-sex-dominant jurisdictions may overestimate or underestimate actual suppression in alternative-reading jurisdictions.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(trans_women_suppression_mechanism_structural_vs_internalized, empirical, 'Whether identity suppression is structural (barrier-dependent) or partly internalized (persisting after barriers are removed).').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gendered_category_membership__biological_sex_reading, 1970, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gend_tr_t1970, gendered_category_membership__biological_sex_reading, theater_ratio, 1970, 0.18).
narrative_ontology:measurement_basis(gend_tr_t1970, projected).
narrative_ontology:measurement(gend_tr_t1990, gendered_category_membership__biological_sex_reading, theater_ratio, 1990, 0.22).
narrative_ontology:measurement_basis(gend_tr_t1990, projected).
narrative_ontology:measurement(gend_tr_t2005, gendered_category_membership__biological_sex_reading, theater_ratio, 2005, 0.28).
narrative_ontology:measurement_basis(gend_tr_t2005, observed).
narrative_ontology:measurement(gend_tr_t2015, gendered_category_membership__biological_sex_reading, theater_ratio, 2015, 0.35).
narrative_ontology:measurement_basis(gend_tr_t2015, observed).
narrative_ontology:measurement(gend_tr_t2020, gendered_category_membership__biological_sex_reading, theater_ratio, 2020, 0.39).
narrative_ontology:measurement_basis(gend_tr_t2020, observed).
narrative_ontology:measurement(gend_tr_t2026, gendered_category_membership__biological_sex_reading, theater_ratio, 2026, 0.41).
narrative_ontology:measurement_basis(gend_tr_t2026, observed).

% Extraction over time
narrative_ontology:measurement(gend_be_t1970, gendered_category_membership__biological_sex_reading, base_extractiveness, 1970, 0.45).
narrative_ontology:measurement_basis(gend_be_t1970, projected).
narrative_ontology:measurement(gend_be_t1990, gendered_category_membership__biological_sex_reading, base_extractiveness, 1990, 0.48).
narrative_ontology:measurement_basis(gend_be_t1990, projected).
narrative_ontology:measurement(gend_be_t2005, gendered_category_membership__biological_sex_reading, base_extractiveness, 2005, 0.56).
narrative_ontology:measurement_basis(gend_be_t2005, observed).
narrative_ontology:measurement(gend_be_t2015, gendered_category_membership__biological_sex_reading, base_extractiveness, 2015, 0.63).
narrative_ontology:measurement_basis(gend_be_t2015, observed).
narrative_ontology:measurement(gend_be_t2020, gendered_category_membership__biological_sex_reading, base_extractiveness, 2020, 0.66).
narrative_ontology:measurement_basis(gend_be_t2020, observed).
narrative_ontology:measurement(gend_be_t2026, gendered_category_membership__biological_sex_reading, base_extractiveness, 2026, 0.68).
narrative_ontology:measurement_basis(gend_be_t2026, observed).

% Suppression requirement over time
narrative_ontology:measurement(gend_su_t1970, gendered_category_membership__biological_sex_reading, suppression_requirement, 1970, 0.32).
narrative_ontology:measurement_basis(gend_su_t1970, projected).
narrative_ontology:measurement(gend_su_t1990, gendered_category_membership__biological_sex_reading, suppression_requirement, 1990, 0.38).
narrative_ontology:measurement_basis(gend_su_t1990, projected).
narrative_ontology:measurement(gend_su_t2005, gendered_category_membership__biological_sex_reading, suppression_requirement, 2005, 0.52).
narrative_ontology:measurement_basis(gend_su_t2005, observed).
narrative_ontology:measurement(gend_su_t2015, gendered_category_membership__biological_sex_reading, suppression_requirement, 2015, 0.68).
narrative_ontology:measurement_basis(gend_su_t2015, observed).
narrative_ontology:measurement(gend_su_t2020, gendered_category_membership__biological_sex_reading, suppression_requirement, 2020, 0.72).
narrative_ontology:measurement_basis(gend_su_t2020, observed).
narrative_ontology:measurement(gend_su_t2026, gendered_category_membership__biological_sex_reading, suppression_requirement, 2026, 0.76).
narrative_ontology:measurement_basis(gend_su_t2026, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gendered_category_membership__biological_sex_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(gendered_category_membership__biological_sex_reading, 0.12).
narrative_ontology:affects_constraint(gendered_category_membership__biological_sex_reading, gendered_category_membership__identity_reading).
narrative_ontology:affects_constraint(gendered_category_membership__biological_sex_reading, gendered_category_membership__social_role_reading).
narrative_ontology:affects_constraint(gendered_category_membership__biological_sex_reading, sex_segregated_space_access_bathroom_policies).
narrative_ontology:affects_constraint(gendered_category_membership__biological_sex_reading, athletic_competition_eligibility_criteria).
narrative_ontology:affects_constraint(gendered_category_membership__biological_sex_reading, reproductive_healthcare_access_boundaries).
narrative_ontology:affects_constraint(gendered_category_membership__biological_sex_reading, legal_sex_recognition_procedures).

% DUAL FORMULATION NOTE:
% This constraint is part of the gendered_category_membership kernel family, which decomposes into three structurally distinct readings with different ε values, stakeholder structures, and persistence mechanisms. The biological-sex reading treats category membership as rooted in chromosomal and anatomical fact — high extractiveness from trans women through identity suppression and institutional exclusion. The identity-based reading treats membership as grounded in self-declared gender identity — substantially lower extractiveness, different victim/beneficiary structure, different institutional supports. The social-role reading treats membership as grounded in sustained social performance and community recognition — different boundary mechanics, different suppression mechanisms. Each reading instantiates a different constraint because measuring 'woman' one way (biology) versus another (identity) changes the beneficiary/victim set, the extraction mechanism, and the persistence path. The three stories are linked via affects_constraints and share commentary about the kernel dispute. Do NOT attempt to collapse them into a single constraint with multiple measurement bases — each reading is ε-invariant within itself; the variance is between readings, not within.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(gendered_category_membership__biological_sex_reading, organized, 0.3).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
