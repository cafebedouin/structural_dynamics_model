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
 *   sustained social performance and recognition by others. It's a 'social
 *   role' reading, where inclusion in a gender category (e.g., 'woman') is
 *   conditional on performing the associated social roles and being
 *   recognized as such by the community. This reading is distinct from those
 *   based purely on biological sex or subjective identity. It functions as a
 *   Tangled Rope: it provides a coordination function for social interaction
 *   but extracts significant costs in terms of conformity and emotional
 *   labor, particularly from trans individuals and gender non-conforming
 *   people. The high theater ratio reflects the performative nature of gender
 *   in this framework.
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
narrative_ontology:constraint_metric(gendered_category_membership__social_role_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gendered_category_membership__social_role_reading, tangled_rope).
narrative_ontology:human_readable(gendered_category_membership__social_role_reading, "Gendered Category Membership (Social Role Reading)").
narrative_ontology:topic_domain(gendered_category_membership__social_role_reading, "social_ontology/political_philosophy/bioethics").

domain_priors:requires_active_enforcement(gendered_category_membership__social_role_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gendered_category_membership__social_role_reading, '143e1164-0952-4291-8431-c8fb443934af').
narrative_ontology:cs_kernel_codification('143e1164-0952-4291-8431-c8fb443934af', implicit).
narrative_ontology:cs_authority_grounding('143e1164-0952-4291-8431-c8fb443934af', practice).
narrative_ontology:cs_interpretation_layer_present('143e1164-0952-4291-8431-c8fb443934af').
narrative_ontology:cs_reading_relation('143e1164-0952-4291-8431-c8fb443934af', gendered_category_membership__biological_sex_reading, coexists_with).
narrative_ontology:cs_reading_relation('143e1164-0952-4291-8431-c8fb443934af', gendered_category_membership__gender_identity_reading, coexists_with).
narrative_ontology:cs_axiom('143e1164-0952-4291-8431-c8fb443934af', foundational, gender_is_socially_performed).
narrative_ontology:cs_axiom_status(gender_is_socially_performed, holdable).
narrative_ontology:cs_axiom_grounding('143e1164-0952-4291-8431-c8fb443934af', gender_is_socially_performed, conventional).
narrative_ontology:cs_axiom('143e1164-0952-4291-8431-c8fb443934af', foundational, social_recognition_confers_membership).
narrative_ontology:cs_axiom_status(social_recognition_confers_membership, holdable).
narrative_ontology:cs_axiom_grounding('143e1164-0952-4291-8431-c8fb443934af', social_recognition_confers_membership, conventional).
narrative_ontology:cs_reference_frame('143e1164-0952-4291-8431-c8fb443934af', traditional_gender_role_performance).
narrative_ontology:cs_drift_state('143e1164-0952-4291-8431-c8fb443934af', contemporary_gender_discourse, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('143e1164-0952-4291-8431-c8fb443934af', '').
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

% Benefit from the clarity and perceived stability of gendered social categories, which define their social roles and expectations. They may also participate in enforcing these norms, gaining social capital. However, they are constrained by the performance requirements of the role.
narrative_ontology:constraint_stakeholder(gendered_category_membership__social_role_reading, cis_women_who_conform, beneficiary,
    moderate, biographical, constrained, local).

% Bear the cost of constant performance and seeking recognition to be included in gendered categories. They face gatekeeping, scrutiny, and potential exclusion if their performance is deemed insufficient or 'inauthentic' by others. Their identity is deeply tied to this recognition.
narrative_ontology:constraint_stakeholder(gendered_category_membership__social_role_reading, trans_women, payer,
    powerless, biographical, identity_locked, local).

% Pay the cost of social friction and exclusion for not conforming to gendered social roles, regardless of their gender identity. They challenge the very premise of performance-based category membership and face resistance for doing so.
narrative_ontology:constraint_stakeholder(gendered_category_membership__social_role_reading, gender_nonconforming_individuals, payer,
    powerless, biographical, identity_locked, local).

% These are individuals or groups who actively police and reinforce gendered social roles through social pressure, shaming, or exclusion. They benefit from maintaining the existing social order and their position within it, but are also bound by the norms they enforce.
narrative_ontology:constraint_stakeholder(gendered_category_membership__social_role_reading, social_norm_enforcers, agenda_setter,
    organized, generational, constrained, regional).

% Experience costs when their behavior or presentation deviates from expected gender roles, even if they identify as cisgender women. They face social judgment and may be denied full membership in certain gendered spaces or activities, highlighting the performance aspect of the constraint.
narrative_ontology:constraint_stakeholder(gendered_category_membership__social_role_reading, cis_women_who_deviate, payer,
    moderate, biographical, constrained, local).

% Analyze and critique the social role reading, advocating for self-identification as the primary basis for gendered category membership. They seek to dismantle the performance requirements and distributed gatekeeping inherent in this constraint.
narrative_ontology:constraint_stakeholder(gendered_category_membership__social_role_reading, gender_identity_advocates, observer,
    organized, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates social interactions by providing a shared, if implicit, framework for understanding and categorizing individuals based on observable gendered performance and presentation, facilitating social cohesion and role assignment.
% TRANSFER_FUNCTION: Transfers social recognition, access to gendered spaces, and validation from those who perform gendered roles acceptably to those who enforce the norms. It extracts emotional labor and conformity from those seeking inclusion.
% ABSENT_VOICES: Individuals who reject the premise of gender as a social performance, or those who are unable to perform gendered roles due to disability or other factors, are largely excluded from shaping the terms of this constraint. Their perspectives are often dismissed as 'unrealistic' or 'disruptive'.
% DISAPPEARANCE_RATIONALE: If gendered category membership based on social role and recognition vanished, social interactions would become highly ambiguous, existing gendered spaces and institutions would lose their organizing principle, and individuals would need to find new ways to signal social identity and belonging. The social world would undergo significant reorganization.
% FOUNDING_PROBLEM: To establish clear social roles and expectations, facilitate social cohesion, and organize society around a binary understanding of gender based on observable performance.
% FOUNDING_PROBLEM_CORROBORATION: Many social conservatives and some radical feminists attest that the problem of social cohesion and clear gender roles is still live, arguing that blurring these lines leads to social confusion and harm. However, gender identity advocates and many sociologists argue that the 'problem' itself is a social construct that perpetuates harm, and that the constraint's function has shifted to maintaining existing power structures.
narrative_ontology:disappearance_verdict(gendered_category_membership__social_role_reading, world_rearranges).
narrative_ontology:founding_problem_status(gendered_category_membership__social_role_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gendered_category_membership__social_role_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
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
 *   Extractiveness is moderate (0.45) due to the ongoing costs of performance and the risk of exclusion. Suppression is high (0.6) because non-conformity is met with social pressure and gatekeeping, making exit from performance difficult for those seeking inclusion. Theater ratio is high (0.7) because the 'performance' aspect is central to the constraint's operation; much social energy is expended in maintaining and policing these roles. Resistance is high (0.75) due to ongoing challenges from trans rights advocates and gender non-conforming individuals. The slight dip in extractiveness and suppression towards the end of the interval reflects increasing social resistance and a partial, but not complete, softening of enforcement.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of cis women who conform, this constraint might appear as a Rope, providing necessary social order. From the perspective of trans women, it is a Snare, demanding constant performance for conditional inclusion. The engine's classification as Tangled Rope reflects the hybrid nature of coordination and extraction, with distributed gatekeeping.
 *
 * DIRECTIONALITY LOGIC:
 *   Cis women who conform benefit from the social order and clarity, but also pay a cost in conformity. Trans women and gender non-conforming individuals are primary payers, bearing the burden of performance and potential exclusion. Social norm enforcers act as agenda-setters, benefiting from maintaining the status quo. Cis women who deviate also experience costs, highlighting the performance aspect. Gender identity advocates are observers, seeking to change the constraint.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    recognition_criteria_ambiguity,
    'What specific criteria constitute ''sufficient'' social performance and recognition for category membership, and how are these criteria adjudicated in practice?',
    'Ethnographic studies of social gatekeeping in gendered spaces, analysis of public discourse on gender roles, and legal challenges to performance-based inclusion.',
    'If criteria are vague and arbitrarily applied, the constraint''s extractiveness and suppression are higher than measured, as targets face unpredictable demands. If criteria are clear and consistently applied, the constraint is more transparent, though still potentially extractive.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(recognition_criteria_ambiguity, empirical, 'Ambiguity in the standards for social performance and recognition.').

omega_variable(
    internalized_suppression_component,
    'To what extent is the suppression experienced by trans women and gender non-conforming individuals internalized (e.g., self-policing, fear of social reprisal) versus purely structural (e.g., explicit exclusion from spaces)?',
    'Qualitative studies on the lived experience of trans and gender non-conforming individuals, focusing on self-censorship and anticipatory conformity. Post-exit suppression trajectory: if suppression persists after explicit barriers are removed, reclassify as partially internalized.',
    'If internalized suppression is a significant component, the effective suppression is higher than the structural measure suggests, as individuals carry the constraint with them even in ostensibly ''safe'' spaces.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(internalized_suppression_component, empirical, 'Structural vs. internalized suppression mechanism for gendered social roles.').

omega_variable(
    coordination_vs_exclusion_function,
    'Is the primary function of this constraint to coordinate social interaction, or to exclude certain individuals from gendered categories and spaces?',
    'Analysis of historical shifts in gender role enforcement and the impact of increased gender diversity on social cohesion. If social cohesion can be maintained with less rigid performance requirements, the exclusionary function is dominant.',
    'If the primary function is exclusion, the constraint is closer to a Snare, with the coordination story serving as cover. If coordination is primary, it remains a Tangled Rope, albeit with high extraction.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coordination_vs_exclusion_function, conceptual, 'Whether the constraint''s primary function is coordination or exclusion.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gendered_category_membership__social_role_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gend_tr_t0, gendered_category_membership__social_role_reading, theater_ratio, 0, 0.5).
narrative_ontology:measurement(gend_tr_t10, gendered_category_membership__social_role_reading, theater_ratio, 10, 0.6).
narrative_ontology:measurement(gend_tr_t20, gendered_category_membership__social_role_reading, theater_ratio, 20, 0.7).
narrative_ontology:measurement(gend_tr_t30, gendered_category_membership__social_role_reading, theater_ratio, 30, 0.75).
narrative_ontology:measurement(gend_tr_t40, gendered_category_membership__social_role_reading, theater_ratio, 40, 0.72).
narrative_ontology:measurement(gend_tr_t50, gendered_category_membership__social_role_reading, theater_ratio, 50, 0.7).

% Extraction over time
narrative_ontology:measurement(gend_be_t0, gendered_category_membership__social_role_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(gend_be_t10, gendered_category_membership__social_role_reading, base_extractiveness, 10, 0.35).
narrative_ontology:measurement(gend_be_t20, gendered_category_membership__social_role_reading, base_extractiveness, 20, 0.4).
narrative_ontology:measurement(gend_be_t30, gendered_category_membership__social_role_reading, base_extractiveness, 30, 0.45).
narrative_ontology:measurement(gend_be_t40, gendered_category_membership__social_role_reading, base_extractiveness, 40, 0.48).
narrative_ontology:measurement(gend_be_t50, gendered_category_membership__social_role_reading, base_extractiveness, 50, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(gend_su_t0, gendered_category_membership__social_role_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(gend_su_t10, gendered_category_membership__social_role_reading, suppression_requirement, 10, 0.55).
narrative_ontology:measurement(gend_su_t20, gendered_category_membership__social_role_reading, suppression_requirement, 20, 0.6).
narrative_ontology:measurement(gend_su_t30, gendered_category_membership__social_role_reading, suppression_requirement, 30, 0.65).
narrative_ontology:measurement(gend_su_t40, gendered_category_membership__social_role_reading, suppression_requirement, 40, 0.62).
narrative_ontology:measurement(gend_su_t50, gendered_category_membership__social_role_reading, suppression_requirement, 50, 0.6).


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
