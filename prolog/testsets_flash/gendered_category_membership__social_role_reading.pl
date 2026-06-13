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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: gendered_category_membership__social_role_reading
 *   human_readable: Gendered Category Membership (Social Role Reading)
 *   domain: social_ontology/political_philosophy/bioethics
 *
 * SUMMARY:
 *   This constraint describes gendered category membership as grounded in
 *   sustained social performance and recognition by others. It is one reading
 *   of the 'gendered_category_membership' kernel, which is also interpreted
 *   through biological sex and gender identity. Under this social role
 *   reading, individuals are recognized as members of a gender category
 *   (e.g., 'woman') if they consistently perform the social roles associated
 *   with that gender and are recognized as such by others. This creates a
 *   dynamic where trans women may be conditionally included based on
 *   'passing' and social acceptance, while gatekeeping is distributed across
 *   everyday social interactions. The constraint imposes performance costs
 *   and risks of exclusion for those who do not conform, affecting both trans
 *   individuals and cis women who deviate from prescribed roles.
 *
 * KEY AGENTS:
 *   - trans_women: Primary target (powerless/identity_locked) — bears performance costs and exclusion risk.
 *   - cis_women_who_conform: Primary beneficiary (moderate/constrained) — benefits from category stability, but also bears costs of conformity.
 *   - social_institutions: Agenda setter (institutional/mobile) — implicitly or explicitly reinforces gendered social roles.
 *   - gender_non_conforming_individuals: Victim (powerless/identity_locked) — bears costs of non-conformity and exclusion.
 *   - cis_women_who_deviate: Victim (moderate/constrained) — faces social penalties for non-conformity.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gendered_category_membership__social_role_reading, 0.45).
domain_priors:suppression_score(gendered_category_membership__social_role_reading, 0.6).
domain_priors:theater_ratio(gendered_category_membership__social_role_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gendered_category_membership__social_role_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(gendered_category_membership__social_role_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(gendered_category_membership__social_role_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gendered_category_membership__social_role_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(gendered_category_membership__social_role_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gendered_category_membership__social_role_reading, tangled_rope).
narrative_ontology:human_readable(gendered_category_membership__social_role_reading, "Gendered Category Membership (Social Role Reading)").
narrative_ontology:topic_domain(gendered_category_membership__social_role_reading, "social_ontology/political_philosophy/bioethics").

domain_priors:requires_active_enforcement(gendered_category_membership__social_role_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gendered_category_membership__social_role_reading, '9c7ad99b-e06d-4dfc-a609-e451152ce5be').
narrative_ontology:cs_kernel_codification('9c7ad99b-e06d-4dfc-a609-e451152ce5be', implicit).
narrative_ontology:cs_authority_grounding('9c7ad99b-e06d-4dfc-a609-e451152ce5be', practice).
narrative_ontology:cs_interpretation_layer_present('9c7ad99b-e06d-4dfc-a609-e451152ce5be').
narrative_ontology:cs_reading_relation('9c7ad99b-e06d-4dfc-a609-e451152ce5be', gendered_category_membership__biological_sex_reading, coexists_with).
narrative_ontology:cs_reading_relation('9c7ad99b-e06d-4dfc-a609-e451152ce5be', gendered_category_membership__gender_identity_reading, coexists_with).
narrative_ontology:cs_axiom('9c7ad99b-e06d-4dfc-a609-e451152ce5be', foundational, gender_is_socially_performed).
narrative_ontology:cs_axiom_status(gender_is_socially_performed, holdable).
narrative_ontology:cs_axiom_grounding('9c7ad99b-e06d-4dfc-a609-e451152ce5be', gender_is_socially_performed, conventional).
narrative_ontology:cs_axiom('9c7ad99b-e06d-4dfc-a609-e451152ce5be', foundational, recognition_is_constitutive_of_gender).
narrative_ontology:cs_axiom_status(recognition_is_constitutive_of_gender, holdable).
narrative_ontology:cs_axiom_grounding('9c7ad99b-e06d-4dfc-a609-e451152ce5be', recognition_is_constitutive_of_gender, conventional).
narrative_ontology:cs_reference_frame('9c7ad99b-e06d-4dfc-a609-e451152ce5be', stable_gender_roles_and_recognition).
narrative_ontology:cs_drift_state('9c7ad99b-e06d-4dfc-a609-e451152ce5be', contemporary_gender_discourse, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('9c7ad99b-e06d-4dfc-a609-e451152ce5be', '').
narrative_ontology:cs_kernel_id(gendered_category_membership__social_role_reading, gendered_category_membership).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gendered_category_membership__social_role_reading, cis_women_who_conform).
narrative_ontology:constraint_beneficiary(gendered_category_membership__social_role_reading, social_institutions).
narrative_ontology:constraint_victim(gendered_category_membership__social_role_reading, trans_women).
narrative_ontology:constraint_victim(gendered_category_membership__social_role_reading, gender_non_conforming_individuals).
narrative_ontology:constraint_victim(gendered_category_membership__social_role_reading, cis_women_who_deviate).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Seek recognition as women but face conditional inclusion based on 'passing' and social acceptance. They bear significant emotional labor and performance costs, and risk exclusion or misgendering if they do not conform to social expectations of womanhood. Their identity is deeply tied to this recognition.
narrative_ontology:constraint_stakeholder(gendered_category_membership__social_role_reading, trans_women, payer,
    powerless, biographical, identity_locked, global).

% Benefit from the stability and legibility of gendered social roles, which provide a clear framework for their identity and social interactions. However, they also bear the costs of conforming to these roles and may face social penalties if they deviate too much.
narrative_ontology:constraint_stakeholder(gendered_category_membership__social_role_reading, cis_women_who_conform, beneficiary,
    moderate, biographical, constrained, global).

% Implicitly or explicitly reinforce gendered social roles through policies, cultural norms, and media representation. They benefit from the social order and predictability that these roles provide, and actively (though often unconsciously) gatekeep access to gendered spaces and resources based on perceived conformity.
narrative_ontology:constraint_stakeholder(gendered_category_membership__social_role_reading, social_institutions, agenda_setter,
    institutional, generational, mobile, global).

% Do not conform to traditional gender roles and face social sanctions, exclusion, and misrecognition. They bear the costs of defying expectations and often struggle to find a legible place within existing gender categories. Their identity is often in tension with the constraint.
narrative_ontology:constraint_stakeholder(gendered_category_membership__social_role_reading, gender_non_conforming_individuals, payer,
    powerless, biographical, identity_locked, global).

% Are biologically female but do not perform traditional feminine roles (e.g., 'butch' women, women in traditionally masculine professions). They may face social penalties, misrecognition, or questions about their 'true' womanhood, bearing costs for non-conformity despite their biological sex.
narrative_ontology:constraint_stakeholder(gendered_category_membership__social_role_reading, cis_women_who_deviate, payer,
    moderate, biographical, constrained, global).

% Analyze the social construction of gender, the mechanisms of recognition, and the power dynamics embedded in gendered categories. They seek to understand the constraint's operation without being directly subject to its enforcement in their analytical role.
narrative_ontology:constraint_stakeholder(gendered_category_membership__social_role_reading, gender_theorists, observer,
    analytical, generational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(gendered_category_membership__social_role_reading, social_institutions).
narrative_ontology:fixing_cost_class(gendered_category_membership__social_role_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a framework for social interaction and identity formation by establishing legible gender categories and associated roles, enabling individuals to navigate social spaces with shared expectations.
% TRANSFER_FUNCTION: Transfers social recognition, access to gendered spaces/resources, and a sense of belonging to those who successfully perform gendered roles, while extracting emotional labor, conformity, and potential exclusion from those who do not or cannot perform adequately.
% ABSENT_VOICES: Those who advocate for a complete dismantling of gender as a social category are largely excluded from mainstream discourse, as their perspective challenges the foundational premise of the constraint itself. They would argue that the entire system of gendered category membership is inherently extractive and suppressive.
% DISAPPEARANCE_RATIONALE: If the constraint of gendered category membership based on social role and recognition vanished overnight, social interactions, identity formation, and institutional structures would undergo a profound and chaotic reorganization. Gendered spaces, language, and expectations would lose their meaning, leading to a period of intense social renegotiation and uncertainty.
% FOUNDING_PROBLEM: The need for social legibility and order, allowing for predictable interactions and the assignment of social roles and responsibilities based on perceived gender.
% FOUNDING_PROBLEM_CORROBORATION: Anthropological and sociological studies attest to the historical and cross-cultural prevalence of gendered social roles for maintaining social order. While the specific roles evolve, the underlying problem of social legibility through gender persists. Gender theorists, while critical of its effects, acknowledge its functional role in social organization.
narrative_ontology:disappearance_verdict(gendered_category_membership__social_role_reading, world_rearranges).
narrative_ontology:founding_problem_status(gendered_category_membership__social_role_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gendered_category_membership__social_role_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(gendered_category_membership__social_role_reading, 'none', 1).

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
 *   The extractiveness (0.45) reflects the significant, ongoing performance costs and emotional labor required to maintain social recognition, as well as the costs of exclusion for those who fail to conform. Suppression (0.6) is high because the enforcement is diffuse and pervasive, occurring through social sanction, microaggressions, and institutional gatekeeping, making alternatives difficult to access. Theater ratio (0.2) is relatively low, as the social performance is genuinely functional for maintaining the category, though some aspects may be performative. The metrics reflect the dynamic where recognition is conditional and requires active maintenance, creating a system of both coordination (for those who conform) and extraction (from those who must perform or are excluded).
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of cis women who conform, the constraint may appear as a natural, stable social order (closer to Rope/Mountain), providing clear social roles and expectations. From the perspective of trans women or gender non-conforming individuals, it is a highly extractive and suppressive system (closer to Snare) that demands constant performance and offers conditional, precarious membership. Social institutions may view it as a necessary mechanism for social cohesion. The engine's per-seat classification will highlight these divergences.
 *
 * DIRECTIONALITY LOGIC:
 *   Trans women and gender non-conforming individuals are targets (high d) due to the performance demands and risk of exclusion. Cis women who conform are beneficiaries (low d) as they benefit from the stability and recognition of the category, though they also bear costs of conformity. Social institutions are agenda setters (low d) as they implicitly or explicitly reinforce these roles and benefit from the social order they maintain. Cis women who deviate from roles are also victims (high d) as they face social penalties.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification as a Tangled Rope prevents mislabeling it as a pure Rope (which would ignore the significant extraction and suppression) or a pure Snare (which would ignore the genuine, albeit conditional, coordination function of social recognition). It highlights the hybrid nature where social cohesion and recognition are achieved through mechanisms that simultaneously impose substantial costs and enforce conformity, with identifiable beneficiaries and victims. The constraint's mandate is to maintain a legible social order, but its operation has accumulated extractive and suppressive elements.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_ambiguity,
    'Is this constraint a genuine reflection of social reality, or a constructed norm that benefits specific groups by enforcing a particular reading of gendered category membership?',
    'Analysis of historical and cross-cultural variations in gender roles and recognition, and the power dynamics involved in enforcing these roles.',
    'If constructed, the constraint''s extractiveness and suppression are higher than perceived, and its classification shifts towards Snare. This reading (social_role_reading) is one of three competing interpretations of the ''gendered_category_membership'' kernel, alongside ''biological_sex_reading'' and ''gender_identity_reading''.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_ambiguity, conceptual, 'Ambiguity between social construction and natural emergence of gendered categories.').

omega_variable(
    victim_identity_ambiguity,
    'Who is the primary victim of this constraint: trans women (due to exclusion/conditional inclusion) or cis women who deviate from social roles (due to enforcement of conformity)?',
    'Empirical studies on the differential impact of social role enforcement on various gendered populations, including intersectional analysis.',
    'Clarifying the primary victim group would refine the directionality and effective extraction for specific stakeholders, potentially shifting the overall classification if one group''s suffering is significantly more pronounced and systematically extracted from.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(victim_identity_ambiguity, empirical, 'Ambiguity in identifying the primary victim group due to overlapping harms.').

omega_variable(
    performance_cost_vs_benefit,
    'Are the costs of sustained social performance (e.g., emotional labor, self-monitoring) for individuals seeking recognition balanced by the benefits of category membership?',
    'Qualitative and quantitative studies on the lived experiences of individuals navigating gendered social roles, assessing psychological, social, and economic costs and benefits.',
    'If costs consistently outweigh benefits for a significant portion of those performing, the constraint''s extractiveness is higher than currently estimated, indicating a more coercive dynamic.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(performance_cost_vs_benefit, empirical, 'Balance of costs and benefits for individuals performing gendered social roles.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gendered_category_membership__social_role_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gend_tr_t0, gendered_category_membership__social_role_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(gend_tr_t10, gendered_category_membership__social_role_reading, theater_ratio, 10, 0.22).
narrative_ontology:measurement(gend_tr_t20, gendered_category_membership__social_role_reading, theater_ratio, 20, 0.2).

% Extraction over time
narrative_ontology:measurement(gend_be_t0, gendered_category_membership__social_role_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(gend_be_t10, gendered_category_membership__social_role_reading, base_extractiveness, 10, 0.4).
narrative_ontology:measurement(gend_be_t20, gendered_category_membership__social_role_reading, base_extractiveness, 20, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(gend_su_t0, gendered_category_membership__social_role_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(gend_su_t10, gendered_category_membership__social_role_reading, suppression_requirement, 10, 0.55).
narrative_ontology:measurement(gend_su_t20, gendered_category_membership__social_role_reading, suppression_requirement, 20, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gendered_category_membership__social_role_reading, identity_coordination).
narrative_ontology:affects_constraint(gendered_category_membership__social_role_reading, gendered_category_membership__biological_sex_reading).
narrative_ontology:affects_constraint(gendered_category_membership__social_role_reading, gendered_category_membership__gender_identity_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three competing readings of the 'gendered_category_membership' kernel. Each reading defines category membership differently, leading to distinct structural constraints and classifications. This social_role_reading focuses on performance and recognition, influencing and being influenced by the biological_sex_reading and gender_identity_reading.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
