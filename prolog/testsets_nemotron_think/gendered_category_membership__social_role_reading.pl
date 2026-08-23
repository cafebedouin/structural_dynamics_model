% ============================================================================
% CONSTRAINT STORY: gendered_category_membership__social_role_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_gendered_category_membership__social_role_reading, []).

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
 *   constraint_id: gendered_category_membership__social_role_reading
 *   human_readable: Gender Category Membership via Sustained Social Performance and Recognition
 *   domain: social_ontology/political_philosophy/bioethics
 *
 * SUMMARY:
 *   This constraint story instantiates the social_role_reading of the
 *   contested kernel 'gendered_category_membership.' The reading holds that
 *   category membership (e.g., 'woman') is constituted by sustained social
 *   performance — dress, mannerism, vocal patterns, social role enactment —
 *   that elicits recognition from others. Recognition is the gate;
 *   performance is the key. Trans women are conditionally included: those who
 *   'pass' and sustain the performance gain recognition and category
 *   membership; those who do not are excluded. Cis women benefit from a
 *   stable, legible category but face boundary erosion when membership
 *   becomes performative. Gender-nonconforming people are excluded from both
 *   poles. The constraint is a tangled rope: it genuinely coordinates social
 *   interaction (providing a shared, low-friction system for gender
 *   attribution) AND asymmetrically extracts performance labor from those
 *   whose bodies or presentations deviate from the norm, while distributing
 *   gatekeeping across millions of everyday recognizers. The kernel has two
 *   sibling readings: biological_sex_reading (category grounded in immutable
 *   biology) and gender_identity_reading (category grounded in
 *   self-declaration). This reading forecloses the biological reading (cannot
 *   be both performance and immutable biology) and coexists with the identity
 *   reading (different factions hold each; neither logically eliminates the
 *   other within a single framework).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gendered_category_membership__social_role_reading, 0.42).
domain_priors:suppression_score(gendered_category_membership__social_role_reading, 0.58).
domain_priors:theater_ratio(gendered_category_membership__social_role_reading, 0.31).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gendered_category_membership__social_role_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(gendered_category_membership__social_role_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(gendered_category_membership__social_role_reading, theater_ratio, 0.31).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gendered_category_membership__social_role_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(gendered_category_membership__social_role_reading, resistance, 0.52).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gendered_category_membership__social_role_reading, tangled_rope).
narrative_ontology:human_readable(gendered_category_membership__social_role_reading, "Gender Category Membership via Sustained Social Performance and Recognition").
narrative_ontology:topic_domain(gendered_category_membership__social_role_reading, "social_ontology/political_philosophy/bioethics").

domain_priors:requires_active_enforcement(gendered_category_membership__social_role_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gendered_category_membership__social_role_reading, '773ab3b4-32bf-4473-ac94-cdb67c89ce08').
narrative_ontology:cs_kernel_codification('773ab3b4-32bf-4473-ac94-cdb67c89ce08', distributed).
narrative_ontology:cs_authority_grounding('773ab3b4-32bf-4473-ac94-cdb67c89ce08', practice).
narrative_ontology:cs_interpretation_layer_present('773ab3b4-32bf-4473-ac94-cdb67c89ce08').
narrative_ontology:cs_reading_relation('773ab3b4-32bf-4473-ac94-cdb67c89ce08', gendered_category_membership__biological_sex_reading, forecloses).
narrative_ontology:cs_reading_relation('773ab3b4-32bf-4473-ac94-cdb67c89ce08', gendered_category_membership__gender_identity_reading, coexists_with).
narrative_ontology:cs_axiom('773ab3b4-32bf-4473-ac94-cdb67c89ce08', foundational, category_membership_requires_social_recognition).
narrative_ontology:cs_axiom_status(category_membership_requires_social_recognition, holdable).
narrative_ontology:cs_axiom_grounding('773ab3b4-32bf-4473-ac94-cdb67c89ce08', category_membership_requires_social_recognition, conventional).
narrative_ontology:cs_axiom('773ab3b4-32bf-4473-ac94-cdb67c89ce08', foundational, sustained_performance_constitutes_category_membership).
narrative_ontology:cs_axiom_status(sustained_performance_constitutes_category_membership, holdable).
narrative_ontology:cs_axiom_grounding('773ab3b4-32bf-4473-ac94-cdb67c89ce08', sustained_performance_constitutes_category_membership, empirically_contingent).
narrative_ontology:cs_reference_frame('773ab3b4-32bf-4473-ac94-cdb67c89ce08', social_performance_framework).
narrative_ontology:cs_drift_state('773ab3b4-32bf-4473-ac94-cdb67c89ce08', contemporary_gender_politics, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('773ab3b4-32bf-4473-ac94-cdb67c89ce08', '').
narrative_ontology:cs_kernel_id(gendered_category_membership__social_role_reading, gendered_category_membership).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gendered_category_membership__social_role_reading, cis_women_seeking_stable_category).
narrative_ontology:constraint_beneficiary(gendered_category_membership__social_role_reading, institutions_relying_on_legible_gender).
narrative_ontology:constraint_victim(gendered_category_membership__social_role_reading, trans_women_failing_recognition).
narrative_ontology:constraint_victim(gendered_category_membership__social_role_reading, gender_nonconforming_people).
narrative_ontology:constraint_victim(gendered_category_membership__social_role_reading, cis_women_facing_boundary_erosion).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(gendered_category_membership__social_role_reading, trans_women_seeking_recognition).
narrative_ontology:constraint_beneficiary(gendered_category_membership__social_role_reading, medical_legal_gatekeepers).
narrative_ontology:constraint_victim(gendered_category_membership__social_role_reading, trans_women_seeking_recognition).
narrative_ontology:constraint_victim(gendered_category_membership__social_role_reading, cis_women_seeking_stable_category).
narrative_ontology:constraint_vindicates(gendered_category_membership__social_role_reading, social_categories_require_public_legibility).
narrative_ontology:constraint_vindicates(gendered_category_membership__social_role_reading, reciprocal_recognition_sustains_social_ontology).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Invest sustained effort in gender performance (presentation, mannerism, social signaling) to achieve recognition as women. When recognition is granted, they gain category membership and its social affordances; when denied, they face exclusion, misgendering, and violence. Exit from the performance demand is identity-locked: the desire for recognition is fused with self-concept, and the social world offers no recognized alternative path to the category.
narrative_ontology:constraint_stakeholder(gendered_category_membership__social_role_reading, trans_women_seeking_recognition, payer,
    moderate, biographical, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(gendered_category_membership__social_role_reading, trans_women_seeking_recognition, beneficiary).

% Benefit from a socially legible category 'woman' that enables collective political action, sex-based protections, and shared spaces. Bear costs when category boundaries become contested: political cohesion fractures, protections become legally uncertain, and single-sex spaces face challenges. Exit is constrained — they can advocate for boundary maintenance but cannot individually opt out of the category's political stakes.
narrative_ontology:constraint_stakeholder(gendered_category_membership__social_role_reading, cis_women_seeking_stable_category, beneficiary,
    organized, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(gendered_category_membership__social_role_reading, cis_women_seeking_stable_category, payer).

% Face exclusion from both 'man' and 'woman' categories because their presentation does not satisfy the sustained performance demands of either. The constraint extracts compliance through social sanction (harassment, employment discrimination, familial rejection) without offering a recognizable category position. Exit is trapped — no performance satisfies the recognizers, and no alternative category is socially available.
narrative_ontology:constraint_stakeholder(gendered_category_membership__social_role_reading, gender_nonconforming_people, payer,
    powerless, biographical, trapped, local).

% Experience the social role constraint as a threat to the coherence of their political category. When 'woman' becomes defined by performative criteria rather than shared material condition, they lose the stable referent for sex-based rights. They pay the cost of boundary erosion without having chosen the performance framework. Exit is constrained — they can resist redefinition but cannot unilaterally restore the prior category stability.
narrative_ontology:constraint_stakeholder(gendered_category_membership__social_role_reading, cis_women_facing_boundary_erosion, payer,
    moderate, biographical, constrained, national).

% The distributed enforcers of the constraint: coworkers, service providers, bureaucrats, friends, family who grant or withhold recognition in daily interaction. They do not set the norm centrally but enact it situationally — using pronouns, gendered address, access decisions. Their collective behavior constitutes the enforcement machinery. Exit is mobile: any individual recognizer can change their behavior without systemic consequence, but the aggregate pattern persists.
narrative_ontology:constraint_stakeholder(gendered_category_membership__social_role_reading, social_recognizers_everyday, agenda_setter,
    organized, immediate, mobile, local).

% Institutions (clinics, courts, ID-issuing bodies) that formalize the social recognition threshold — requiring 'real-life experience,' therapist letters, or surgical history before updating legal sex. They legitimize the performance standard by codifying it, and they benefit from the authority to adjudicate category membership. Exit is arbitrage: they can reform criteria (as many jurisdictions have) without losing institutional role.
narrative_ontology:constraint_stakeholder(gendered_category_membership__social_role_reading, medical_legal_gatekeepers, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(gendered_category_membership__social_role_reading, medical_legal_gatekeepers, beneficiary).

% Analyze the constraint as a patriarchal imposition: the demand that women perform femininity to be recognized as women. They argue the social role reading naturalizes gendered performance expectations and undermines sex-based politics. Their seat is analytical — they do not bear the performance costs directly but trace the constraint's extraction logic.
narrative_ontology:constraint_stakeholder(gendered_category_membership__social_role_reading, feminist_theorists_gender_critical, observer,
    analytical, generational, analytical, global).

% Analyze the constraint as a site of both oppression and possibility: the performance demand is extractive, but the recognition pathway is also what makes trans women's category membership achievable. They critique the gatekeeping while defending the possibility of social recognition. Analytical seat with distinct framing from gender-critical observers.
narrative_ontology:constraint_stakeholder(gendered_category_membership__social_role_reading, trans_feminist_theorists, observer,
    analytical, generational, analytical, global).

% The constraint's binary structure (man/woman as the only recognized categories) renders them structurally unintelligible. They would object to the premise that category membership requires sustained performance toward one of two poles, but their objection has no purchase in the recognizer network because the constraint itself produces the binary. Excluded from the conversation that defines the categories they must navigate.
narrative_ontology:constraint_stakeholder(gendered_category_membership__social_role_reading, nonbinary_people_excluded_from_binary, excluded,
    powerless, biographical, trapped, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates social interaction by providing a shared, publicly legible basis for attributing category membership 'woman'/'man' — enabling pronouns, gendered address, single-sex spaces, sex-based protections, and statistical tracking without requiring intrusive verification of biology or subjective identity.
% TRANSFER_FUNCTION: Moves social recognition and category-affordances (access to women's spaces, she/her pronouns, sex-based legal protections) from those who do not perform the recognized gender role to those who do. Moves exclusion risk and performance burden onto gender-nonconforming people and trans women who fail the recognition threshold. Moves political coherence costs onto cis women when the category's boundary becomes performative rather than material.
% ABSENT_VOICES: Non-binary people (excluded by the binary structure), cultures with non-binary gender systems (e.g., Two-Spirit, hijra, fa'afafine) whose categories are erased by the Western binary performance framework, historical subjects who lived under different gender taxonomies, and future people who might inhabit post-gender social forms. The constraint's recognizer network only hears voices that speak within the man/woman binary.
% DISAPPEARANCE_RATIONALE: If the social-performance constraint vanished overnight, the coordination function it provides (legible gender attribution in daily life) would not disappear — the demand for gender legibility would persist. But the mechanism would shift: either toward self-declaration (identity reading), biological verification (sex reading), or a new untested system. Single-sex spaces, pronouns, and sex-based law would face immediate practical crisis. The world rearranges because the constraint is the current operating system for gender coordination, not because the coordination need is artificial.
% FOUNDING_PROBLEM: Pre-modern and early modern societies needed a durable, publicly verifiable system for sorting people into 'man' and 'woman' roles — for labor division, inheritance, marriage law, military conscription, and religious ritual — without relying on invasive bodily inspection or private self-report. Sustained social performance (dress, manner, social role enactment) provided a legible, enforceable proxy that could be policed by ordinary people in daily life.
% FOUNDING_PROBLEM_CORROBORATION: Historians of gender (Laqueur, Schiebinger) corroborate that the 'one-sex' to 'two-sex' transition in 18th-century Europe made social performance the primary marker of gender difference as anatomical distinction was downplayed. Sociologists (West & Zimmerman, Butler) document the ongoing 'doing gender' as interactional achievement. The founding problem (legible sorting without invasive inspection) is contested: gender-critical feminists argue the material basis (reproductive sex) was never actually replaced; trans feminists argue the founding problem was patriarchal control, not coordination; legal scholars note that modern ID systems have shifted toward self-declaration, suggesting the founding problem is being solved differently.
narrative_ontology:disappearance_verdict(gendered_category_membership__social_role_reading, world_rearranges).
narrative_ontology:founding_problem_status(gendered_category_membership__social_role_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gendered_category_membership__social_role_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(gendered_category_membership__social_role_reading, 'none', 1).
narrative_ontology:epsilon_provenance(gendered_category_membership__social_role_reading, 0.42, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gendered_category_membership__social_role_reading_tests).
:- end_tests(gendered_category_membership__social_role_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.42) is low-to-moderate: the primary cost is performance labor (time, money, psychological effort) borne by those whose bodies don't match the norm. It is not rent extraction in the classic sense — no central actor collects the performance surplus. Suppression (0.58) is moderate: the constraint is enforced by distributed social sanction (misgendering, exclusion, violence) not state violence, but the sanction is pervasive and inescapable in daily life. Theater ratio (0.31) reflects that much gender performance is genuine self-expression or habit, not pure compliance theater, but a growing fraction is strategic performance for recognition. Accessibility collapse (0.48) is partial: alternatives (non-recognition, non-binary presentation) exist but carry high social cost. Resistance (0.52) is significant: trans activism, gender-critical feminism, and non-binary visibility all contest the constraint from different directions. The measurement series shows declining extractiveness and suppression from 1970-2010 (feminist and gay liberation weakened rigid performance norms) then stabilization 2010-2025 (new visibility politics created fresh performance demands for trans women). All metrics share the same 6 time-point grid.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats (trans women, GNC people, cis women facing erosion) experience the constraint as extraction: performance demanded, recognition conditional, exclusion painful. The agenda-setter seats (everyday recognizers, medical gatekeepers) experience it as coordination: they just follow the social script that makes interaction smooth. The beneficiary seat (cis women seeking stability) experiences it as infrastructure: the category works for them until it doesn't. The engine computes per-seat types from this structural data — the analytical observers see the full tangled rope structure.
 *
 * DIRECTIONALITY LOGIC:
 *   Trans women seeking recognition are identity-locked payers who become conditional beneficiaries if they pass — their directionality is high (near target) when unrecognized, drops toward symmetric when recognized. Cis women seeking stable category are organized beneficiaries who become payers when boundaries erode — directionality shifts from low (beneficiary) to moderate (payer) depending on political moment. Gender-nonconforming people are trapped payers with directionality near 1.0 — no exit, no recognition. Cis women facing boundary erosion are constrained payers (directionality ~0.7). Everyday recognizers are mobile agenda-setters (directionality ~0.3 — they enforce but don't primarily benefit). Medical/legal gatekeepers are institutional agenda-setters with arbitrage exit (directionality ~0.2 — they benefit from adjudicative authority). The two analytical observers are analytical (directionality 0.5 by convention). Nonbinary excluded are trapped excluded (directionality not computed — they are outside the constraint's coordinate system).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (legible gender sorting without invasive inspection) was real and the constraint solved it for centuries. But the problem has mutated: modern societies have less need for rigid gender sorting (labor division changed, marriage law equalized, conscription ended or gender-neutralized), and we have alternative coordination mechanisms (self-declaration, legal gender markers). The constraint persists because the recognition network is self-reinforcing — everyone recognizes because everyone recognizes — not because the founding problem remains live. This is mandatrophy: the mandate (coordinate gender legibly) has outlived its function (rigid binary sorting for patriarchal institutions), but the constraint remains due to distributed inertia and the lack of a coordinated switch to a new system. The status 'contested' reflects that different factions disagree on whether the problem is dead (gender-critical: material sex still matters), transformed (trans feminists: the problem was patriarchy, not coordination), or solved differently (legal trend toward self-ID).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    performance_cost_distribution,
    'How are the performance costs of the social role constraint distributed across trans women, cis women, and gender-nonconforming people, and do they constitute extraction or coordination cost?',
    'Time-use surveys, financial cost accounting (clothing, medical, grooming), and psychological burden measurement across the three groups, compared to a counterfactual ''no gender performance demand'' baseline.',
    'If costs are concentrated on trans women and GNC people while cis women net-benefit, the constraint is extractive (snare/tangled rope). If costs are broadly shared and offset by coordination gains, it leans rope. The current 0.42 ε reflects judgment that costs are real but not monopolistic.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(performance_cost_distribution, empirical, 'Whether performance costs are extractive overhead or necessary coordination friction.').

omega_variable(
    recognition_as_rent_or_coordination,
    'Is the recognition gate a coordination mechanism (solving the ''who counts as a woman'' problem for social interaction) or a rent-collection mechanism (controlling access to category-affordances)?',
    'Natural experiment: jurisdictions that adopted self-declaration (Argentina 2012, Ireland 2015, etc.) — did social coordination collapse? Did category-affordances (prisons, shelters, sports) become unworkable? If coordination holds without the performance gate, the gate is rent.',
    'If recognition gate is dispensable for coordination, the constraint''s coordination function is a cover story and ε should be higher. If coordination fails without it, the 0.42 ε correctly captures necessary friction.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(recognition_as_rent_or_coordination, empirical, 'Whether the recognition threshold is functionally necessary or extractive gatekeeping.').

omega_variable(
    victim_structure_ambiguity,
    'Are trans women and cis women co-victims of the same constraint (both harmed by the performance gate) or are they in structural contention (cis women''s category stability vs trans women''s inclusion)?',
    'Analyze whether policy changes that benefit one group (e.g., self-ID for trans women) necessarily harm the other (cis women''s sex-based protections), or whether institutional redesign can satisfy both. Track coalition formation vs. conflict in legislative battles.',
    'If co-victims, the constraint is a snare extracting from both. If structural contention, it''s a tangled rope with opposed payer seats. The current victim list names both, reflecting ambiguity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(victim_structure_ambiguity, conceptual, 'Whether the victim structure is unified extraction or zero-sum contention.').

omega_variable(
    kernel_reading_foreclosure_biological,
    'Does the social role reading logically foreclose the biological sex reading within any single normative framework, or can a framework hold both (e.g., ''sex is biological, gender is social performance'')?',
    'Examine whether any live political/legal framework successfully maintains both: sex as immutable biological category for some purposes (medicine, sport) and gender as social performance for others (civil recognition, pronouns). If such dual-system frameworks are stable, foreclosure is false; if they inevitably collapse into one reading, foreclosure holds.',
    'If forecloses, the readings are mutually exclusive kernel interpretations — adopting one logically commits to rejecting the other. If coexists_with, they can occupy different institutional domains simultaneously.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_foreclosure_biological, conceptual, 'Logical relationship between social role and biological sex readings of the kernel.').

omega_variable(
    kernel_reading_relation_identity,
    'Does the social role reading foreclose the gender identity reading, or do they coexist as competing but logically compatible positions?',
    'Test whether a single framework can consistently hold: ''category membership requires social recognition'' AND ''category membership is grounded in self-declaration.'' If self-declaration is sufficient for recognition, the performance demand becomes optional — the readings conflict on whether recognition is earned or owed. But different factions can hold different readings without logical contradiction in the broader discourse.',
    'If forecloses, the identity reading''s rise structurally displaces the social role reading. If coexists_with, both remain live in different communities. Current judgment: coexists_with — they compete for institutional adoption but neither logically eliminates the other.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_relation_identity, conceptual, 'Structural relationship between social role and gender identity readings.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression (0.58) structural (distributed social sanction, institutional gatekeeping) or internalized (the target polices their own performance because they have fused their identity with the category)?',
    'Post-exit suppression trajectory: if trans women who achieve recognition still self-monitor performance, or if detransitioners report persistent internalized gender policing, the suppression has an internalized component that persists after structural pressure eases.',
    'If internalized, effective suppression is higher than structural measure suggests — the target carries the constraint with them. This would increase χ for identity-locked payers beyond what structural d captures.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism in gender performance enforcement.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gendered_category_membership__social_role_reading, 1970, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gcm_srr_tr_t1970, gendered_category_membership__social_role_reading, theater_ratio, 1970, 0.22).
narrative_ontology:measurement(gcm_srr_tr_t1985, gendered_category_membership__social_role_reading, theater_ratio, 1985, 0.25).
narrative_ontology:measurement(gcm_srr_tr_t2000, gendered_category_membership__social_role_reading, theater_ratio, 2000, 0.28).
narrative_ontology:measurement(gcm_srr_tr_t2010, gendered_category_membership__social_role_reading, theater_ratio, 2010, 0.3).
narrative_ontology:measurement(gcm_srr_tr_t2020, gendered_category_membership__social_role_reading, theater_ratio, 2020, 0.31).
narrative_ontology:measurement(gcm_srr_tr_t2025, gendered_category_membership__social_role_reading, theater_ratio, 2025, 0.31).

% Extraction over time
narrative_ontology:measurement(gcm_srr_be_t1970, gendered_category_membership__social_role_reading, base_extractiveness, 1970, 0.55).
narrative_ontology:measurement(gcm_srr_be_t1985, gendered_category_membership__social_role_reading, base_extractiveness, 1985, 0.51).
narrative_ontology:measurement(gcm_srr_be_t2000, gendered_category_membership__social_role_reading, base_extractiveness, 2000, 0.46).
narrative_ontology:measurement(gcm_srr_be_t2010, gendered_category_membership__social_role_reading, base_extractiveness, 2010, 0.41).
narrative_ontology:measurement(gcm_srr_be_t2020, gendered_category_membership__social_role_reading, base_extractiveness, 2020, 0.42).
narrative_ontology:measurement(gcm_srr_be_t2025, gendered_category_membership__social_role_reading, base_extractiveness, 2025, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(gcm_srr_su_t1970, gendered_category_membership__social_role_reading, suppression_requirement, 1970, 0.72).
narrative_ontology:measurement(gcm_srr_su_t1985, gendered_category_membership__social_role_reading, suppression_requirement, 1985, 0.66).
narrative_ontology:measurement(gcm_srr_su_t2000, gendered_category_membership__social_role_reading, suppression_requirement, 2000, 0.61).
narrative_ontology:measurement(gcm_srr_su_t2010, gendered_category_membership__social_role_reading, suppression_requirement, 2010, 0.57).
narrative_ontology:measurement(gcm_srr_su_t2020, gendered_category_membership__social_role_reading, suppression_requirement, 2020, 0.58).
narrative_ontology:measurement(gcm_srr_su_t2025, gendered_category_membership__social_role_reading, suppression_requirement, 2025, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gendered_category_membership__social_role_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(gendered_category_membership__social_role_reading, 0.08).
narrative_ontology:affects_constraint(gendered_category_membership__social_role_reading, gendered_category_membership__biological_sex_reading).
narrative_ontology:affects_constraint(gendered_category_membership__social_role_reading, gendered_category_membership__gender_identity_reading).

% DUAL FORMULATION NOTE:
% This constraint is one member of the gendered_category_membership kernel family. The three readings (biological_sex_reading, gender_identity_reading, social_role_reading) decompose the colloquial 'what is a woman/man?' into structurally distinct claims with different ε, different victim/beneficiary structures, and different coordination/extraction profiles. The biological reading has near-zero ε (Mountain candidate); the identity reading has low ε but high suppression for non-declarants; this social role reading has moderate ε and distributed suppression. They are linked here and should be linked reciprocally in the sibling files.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(gendered_category_membership__social_role_reading, moderate, 0.75).
constraint_indexing:directionality_override(gendered_category_membership__social_role_reading, organized, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
