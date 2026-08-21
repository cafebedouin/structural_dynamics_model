% ============================================================================
% CONSTRAINT STORY: gendered_category_membership__social_role_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
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
 *   constraint_id: gendered_category_membership__social_role_reading
 *   human_readable: Gendered Category Membership (Social Role Reading)
 *   domain: social_ontology/political_philosophy/bioethics
 *
 * SUMMARY:
 *   This constraint describes gendered category membership as grounded in
 *   sustained social performance and recognition by others. It is one reading
 *   of the broader 'gendered_category_membership' kernel. This reading
 *   emphasizes the performative aspects of gender and the role of social
 *   gatekeeping in defining who belongs to which category. Trans women are
 *   conditionally included based on their ability to 'pass' and be
 *   recognized, while both trans individuals and cis women who deviate from
 *   norms bear the costs of exclusion or scrutiny. The constraint is claimed
 *   as a Tangled Rope due to its genuine coordination function (social order)
 *   intertwined with asymmetric extraction (performance costs, exclusion).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gendered_category_membership__social_role_reading, 0.45).
domain_priors:suppression_score(gendered_category_membership__social_role_reading, 0.6).
domain_priors:theater_ratio(gendered_category_membership__social_role_reading, 0.7).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gendered_category_membership__social_role_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(gendered_category_membership__social_role_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(gendered_category_membership__social_role_reading, theater_ratio, 0.7).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gendered_category_membership__social_role_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(gendered_category_membership__social_role_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gendered_category_membership__social_role_reading, tangled_rope).
narrative_ontology:human_readable(gendered_category_membership__social_role_reading, "Gendered Category Membership (Social Role Reading)").
narrative_ontology:topic_domain(gendered_category_membership__social_role_reading, "social_ontology/political_philosophy/bioethics").

domain_priors:requires_active_enforcement(gendered_category_membership__social_role_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gendered_category_membership__social_role_reading, '89dc1c96-5585-4434-83e7-e7fb74aae0a0').
narrative_ontology:cs_kernel_codification('89dc1c96-5585-4434-83e7-e7fb74aae0a0', implicit).
narrative_ontology:cs_authority_grounding('89dc1c96-5585-4434-83e7-e7fb74aae0a0', practice).
narrative_ontology:cs_interpretation_layer_present('89dc1c96-5585-4434-83e7-e7fb74aae0a0').
narrative_ontology:cs_reading_relation('89dc1c96-5585-4434-83e7-e7fb74aae0a0', gendered_category_membership__biological_sex_reading, coexists_with).
narrative_ontology:cs_reading_relation('89dc1c96-5585-4434-83e7-e7fb74aae0a0', gendered_category_membership__gender_identity_reading, coexists_with).
narrative_ontology:cs_axiom('89dc1c96-5585-4434-83e7-e7fb74aae0a0', foundational, gender_is_socially_performed_and_recognized).
narrative_ontology:cs_axiom_status(gender_is_socially_performed_and_recognized, holdable).
narrative_ontology:cs_axiom_grounding('89dc1c96-5585-4434-83e7-e7fb74aae0a0', gender_is_socially_performed_and_recognized, conventional).
narrative_ontology:cs_axiom('89dc1c96-5585-4434-83e7-e7fb74aae0a0', secondary, social_cohesion_requires_gendered_roles).
narrative_ontology:cs_axiom_status(social_cohesion_requires_gendered_roles, holdable).
narrative_ontology:cs_axiom_grounding('89dc1c96-5585-4434-83e7-e7fb74aae0a0', social_cohesion_requires_gendered_roles, instrumental).
narrative_ontology:cs_reference_frame('89dc1c96-5585-4434-83e7-e7fb74aae0a0', traditional_gender_role_stability).
narrative_ontology:cs_drift_state('89dc1c96-5585-4434-83e7-e7fb74aae0a0', contemporary_gender_discourse, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('89dc1c96-5585-4434-83e7-e7fb74aae0a0', '').
narrative_ontology:cs_kernel_id(gendered_category_membership__social_role_reading, gendered_category_membership).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gendered_category_membership__social_role_reading, cis_women_who_conform).
narrative_ontology:constraint_beneficiary(gendered_category_membership__social_role_reading, social_norm_enforcers).
narrative_ontology:constraint_victim(gendered_category_membership__social_role_reading, trans_women).
narrative_ontology:constraint_victim(gendered_category_membership__social_role_reading, gender_nonconforming_individuals).
narrative_ontology:constraint_victim(gendered_category_membership__social_role_reading, cis_women_who_deviate).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefit from the stability and perceived coherence of gendered social categories, which define their social roles and expectations. They may also experience pressure to conform to these roles, limiting their individual expression. Their membership is generally unquestioned as long as they perform expected gender roles.
narrative_ontology:constraint_stakeholder(gendered_category_membership__social_role_reading, cis_women_who_conform, beneficiary,
    moderate, biographical, constrained, local).

% Bear the cost of constant performance and seeking recognition to be included in gendered categories. Their membership is conditional and subject to scrutiny, leading to social exclusion, gatekeeping, and the emotional labor of 'passing.' Exit means abandoning their affirmed identity or facing severe social ostracization.
narrative_ontology:constraint_stakeholder(gendered_category_membership__social_role_reading, trans_women, payer,
    powerless, biographical, identity_locked, local).

% Pay the cost of deviating from expected gender roles, regardless of their assigned sex or gender identity. They face social pressure, exclusion, and misrecognition because their performance does not align with the social role reading of gender. Their identity is often at odds with the constraint's demands.
narrative_ontology:constraint_stakeholder(gendered_category_membership__social_role_reading, gender_nonconforming_individuals, payer,
    powerless, biographical, identity_locked, local).

% Experience costs when their social performance deviates from traditional feminine roles, even if they are cisgender. They may face social judgment, exclusion from certain spaces, or questioning of their 'womanhood' by those who adhere strictly to the social role reading. Their exit options are limited by the pervasive nature of gendered expectations.
narrative_ontology:constraint_stakeholder(gendered_category_membership__social_role_reading, cis_women_who_deviate, payer,
    moderate, biographical, constrained, local).

% Are individuals or groups who actively police and reinforce gendered social roles through social pressure, gatekeeping, and informal sanctions. They benefit from the perceived order and stability that these roles provide, and their actions maintain the constraint's persistence. Their role is often diffuse and decentralized.
narrative_ontology:constraint_stakeholder(gendered_category_membership__social_role_reading, social_norm_enforcers, agenda_setter,
    organized, generational, constrained, local).

% Advocate for gender identity as the primary determinant of gendered category membership, directly challenging the social role reading. They are often excluded from spaces where the social role reading is dominant, and their arguments are dismissed as undermining social order. Their 'exit' is to form alternative communities or legal frameworks.
narrative_ontology:constraint_stakeholder(gendered_category_membership__social_role_reading, gender_identity_advocates, excluded,
    organized, generational, mobile, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a framework for social interaction by assigning roles and expectations based on observable gender performance, facilitating communication and social order within a traditional understanding of gender.
% TRANSFER_FUNCTION: Transfers social recognition, access to gendered spaces, and validation from those who conform to gendered social roles to those who successfully perform them. It extracts emotional labor, conformity, and authenticity from those whose performance is scrutinized or who deviate from norms.
% ABSENT_VOICES: Those who prioritize gender identity or biological sex as the sole determinant of gendered category membership are often excluded from the discourse that sustains the social role reading. They would argue that performance is either irrelevant or secondary to intrinsic identity or biological reality.
% DISAPPEARANCE_RATIONALE: If gendered category membership based on social role and recognition vanished, social interactions would lose a key organizing principle. Gendered spaces, expectations, and norms would dissolve, leading to a period of significant social reordering as new forms of categorization or interaction emerged.
% FOUNDING_PROBLEM: To establish clear social roles and expectations, ensuring social cohesion and the orderly transmission of cultural norms related to gender.
% FOUNDING_PROBLEM_CORROBORATION: Proponents of the social role reading (often social conservatives or some feminists) argue the problem is live, citing perceived social chaos or the erosion of women's spaces. Critics (gender identity advocates, some queer theorists) argue the problem is dead, replaced by a need for individual autonomy and recognition, and that the constraint now serves to enforce outdated hierarchies. Corroboration for the 'dead' status comes from sociological studies on gender fluidity and the lived experiences of trans and gender-nonconforming individuals.
narrative_ontology:disappearance_verdict(gendered_category_membership__social_role_reading, world_rearranges).
narrative_ontology:founding_problem_status(gendered_category_membership__social_role_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gendered_category_membership__social_role_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(gendered_category_membership__social_role_reading, 'none', 1).
narrative_ontology:epsilon_provenance(gendered_category_membership__social_role_reading, 0.45, 'gemini-2.5-flash', 'none', direct).

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
 *   Extractiveness is moderate (0.45) because while it provides social order, it imposes significant performance costs and gatekeeping on those who do not naturally fit or choose to deviate. Suppression is moderate (0.6) as social pressure and informal sanctions are pervasive, making exit difficult for those whose identities are tied to these categories. Theater ratio is high (0.7) because much of the 'performance' of gender is now for the sake of maintaining the social construct itself, rather than serving a purely functional purpose, especially as other readings gain prominence. The rising theater ratio over time reflects the increasing performative effort required to maintain this reading in the face of challenges.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of cis women who conform, this constraint might appear as a Rope, providing necessary social structure. From the perspective of trans women or gender non-conforming individuals, it operates as a Snare, demanding constant performance and threatening exclusion. The engine's per-seat classification will capture this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Cis women who conform benefit from the stability of the categories and generally face lower performance costs (low d). Trans women and gender non-conforming individuals are primary targets, bearing high performance costs and facing exclusion (high d, identity_locked exit). Social norm enforcers are agenda-setters, benefiting from the maintenance of social order (low d). Cis women who deviate from norms also bear costs, though often less severe than trans individuals (moderate d).
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate (social order through clear gender roles) is contested. While some argue it still serves a vital function, others see its persistence as primarily serving to maintain traditional power structures and extract conformity. The high theater ratio and contested founding problem status suggest a drift towards a Piton or Snare for many, even if it retains some coordination function for others. The classification as Tangled Rope reflects this hybridity, preventing mislabeling it as pure coordination or pure extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    social_recognition_threshold,
    'What specific criteria and level of social performance are required for ''recognition'' into a gendered category under this reading, and how consistently are they applied?',
    'Detailed ethnographic studies of social gatekeeping practices and qualitative analysis of public discourse surrounding gendered inclusion/exclusion.',
    'If criteria are vague or inconsistently applied, the constraint''s extractiveness and suppression are higher due to arbitrary enforcement. If criteria are clear and consistently applied, it might reduce perceived extraction for those who can meet them, but increase it for those who cannot.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(social_recognition_threshold, empirical, 'Ambiguity in the ''recognition'' component of the social role reading.').

omega_variable(
    internalized_vs_structural_suppression,
    'What proportion of the measured suppression is due to external social pressure (structural) versus internalized norms and self-policing (internalized) for individuals seeking to conform to gendered roles?',
    'Longitudinal studies tracking individuals'' experiences of gender performance and self-perception after changes in external social acceptance or legal frameworks.',
    'If suppression is largely internalized, the effective suppression is higher than structural measures suggest, as individuals carry the constraint with them even in more permissive environments. This would shift the classification towards a more insidious form of Snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(internalized_vs_structural_suppression, empirical, 'Structural vs. internalized suppression mechanism in gender role conformity.').

omega_variable(
    reading_framing_underdetermination,
    'Is the ''social_role_reading'' a distinct and coherent framing of gendered category membership, or is it a hybrid/conflation of elements from the ''biological_sex_reading'' and ''gender_identity_reading''?',
    'Conceptual analysis and philosophical debate clarifying the logical independence of social role as a primary grounding for gender, distinct from biological or subjective identity.',
    'If it''s a hybrid, its classification might be unstable, shifting depending on which underlying element is emphasized. If it''s truly distinct, its classification as Tangled Rope is more robust. This omega addresses the conceptual purity of the reading itself.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_framing_underdetermination, conceptual, 'Conceptual purity of the social_role_reading as a distinct framing of gender.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gendered_category_membership__social_role_reading, 1950, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gend_tr_t1950, gendered_category_membership__social_role_reading, theater_ratio, 1950, 0.5).
narrative_ontology:measurement(gend_tr_t1970, gendered_category_membership__social_role_reading, theater_ratio, 1970, 0.55).
narrative_ontology:measurement(gend_tr_t1990, gendered_category_membership__social_role_reading, theater_ratio, 1990, 0.6).
narrative_ontology:measurement(gend_tr_t2010, gendered_category_membership__social_role_reading, theater_ratio, 2010, 0.65).
narrative_ontology:measurement(gend_tr_t2024, gendered_category_membership__social_role_reading, theater_ratio, 2024, 0.7).

% Extraction over time
narrative_ontology:measurement(gend_be_t1950, gendered_category_membership__social_role_reading, base_extractiveness, 1950, 0.3).
narrative_ontology:measurement(gend_be_t1970, gendered_category_membership__social_role_reading, base_extractiveness, 1970, 0.35).
narrative_ontology:measurement(gend_be_t1990, gendered_category_membership__social_role_reading, base_extractiveness, 1990, 0.4).
narrative_ontology:measurement(gend_be_t2010, gendered_category_membership__social_role_reading, base_extractiveness, 2010, 0.43).
narrative_ontology:measurement(gend_be_t2024, gendered_category_membership__social_role_reading, base_extractiveness, 2024, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(gend_su_t1950, gendered_category_membership__social_role_reading, suppression_requirement, 1950, 0.7).
narrative_ontology:measurement(gend_su_t1970, gendered_category_membership__social_role_reading, suppression_requirement, 1970, 0.65).
narrative_ontology:measurement(gend_su_t1990, gendered_category_membership__social_role_reading, suppression_requirement, 1990, 0.6).
narrative_ontology:measurement(gend_su_t2010, gendered_category_membership__social_role_reading, suppression_requirement, 2010, 0.58).
narrative_ontology:measurement(gend_su_t2024, gendered_category_membership__social_role_reading, suppression_requirement, 2024, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gendered_category_membership__social_role_reading, identity_coordination).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
