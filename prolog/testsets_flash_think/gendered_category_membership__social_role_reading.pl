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
 *   human_readable: Gendered Category Membership via Social Role Performance
 *   domain: social_ontology/political_philosophy/bioethics
 *
 * SUMMARY:
 *   This constraint describes how membership in gendered categories (e.g.,
 *   'woman') is determined by sustained social performance and the
 *   recognition of that performance by others. It is one reading of the
 *   broader 'gendered_category_membership' kernel. This reading emphasizes
 *   the social construction and maintenance of gender, where individuals must
 *   actively demonstrate alignment with societal expectations to be
 *   recognized as members of a particular gender category. This process
 *   coordinates social interaction but also imposes performance costs and can
 *   lead to exclusion.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gendered_category_membership__social_role_reading, 0.45).
domain_priors:suppression_score(gendered_category_membership__social_role_reading, 0.6).
domain_priors:theater_ratio(gendered_category_membership__social_role_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gendered_category_membership__social_role_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(gendered_category_membership__social_role_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(gendered_category_membership__social_role_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gendered_category_membership__social_role_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(gendered_category_membership__social_role_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gendered_category_membership__social_role_reading, tangled_rope).
narrative_ontology:human_readable(gendered_category_membership__social_role_reading, "Gendered Category Membership via Social Role Performance").
narrative_ontology:topic_domain(gendered_category_membership__social_role_reading, "social_ontology/political_philosophy/bioethics").

domain_priors:requires_active_enforcement(gendered_category_membership__social_role_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gendered_category_membership__social_role_reading, 'e34dd851-720b-41ad-9007-ffd4a69c5875').
narrative_ontology:cs_kernel_codification('e34dd851-720b-41ad-9007-ffd4a69c5875', implicit).
narrative_ontology:cs_authority_grounding('e34dd851-720b-41ad-9007-ffd4a69c5875', practice).
narrative_ontology:cs_interpretation_layer_present('e34dd851-720b-41ad-9007-ffd4a69c5875').
narrative_ontology:cs_reading_relation('e34dd851-720b-41ad-9007-ffd4a69c5875', gendered_category_membership__biological_sex_reading, forecloses).
narrative_ontology:cs_reading_relation('e34dd851-720b-41ad-9007-ffd4a69c5875', gendered_category_membership__gender_identity_reading, coexists_with).
narrative_ontology:cs_axiom('e34dd851-720b-41ad-9007-ffd4a69c5875', foundational, gender_is_socially_performed).
narrative_ontology:cs_axiom_status(gender_is_socially_performed, holdable).
narrative_ontology:cs_axiom_grounding('e34dd851-720b-41ad-9007-ffd4a69c5875', gender_is_socially_performed, conventional).
narrative_ontology:cs_axiom('e34dd851-720b-41ad-9007-ffd4a69c5875', foundational, social_recognition_validates_membership).
narrative_ontology:cs_axiom_status(social_recognition_validates_membership, holdable).
narrative_ontology:cs_axiom_grounding('e34dd851-720b-41ad-9007-ffd4a69c5875', social_recognition_validates_membership, conventional).
narrative_ontology:cs_reference_frame('e34dd851-720b-41ad-9007-ffd4a69c5875', traditional_gender_roles).
narrative_ontology:cs_drift_state('e34dd851-720b-41ad-9007-ffd4a69c5875', contemporary_social_discourse, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('e34dd851-720b-41ad-9007-ffd4a69c5875', '').
narrative_ontology:cs_kernel_id(gendered_category_membership__social_role_reading, gendered_category_membership).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gendered_category_membership__social_role_reading, cis_women).
narrative_ontology:constraint_beneficiary(gendered_category_membership__social_role_reading, social_institutions).
narrative_ontology:constraint_victim(gendered_category_membership__social_role_reading, trans_women).
narrative_ontology:constraint_victim(gendered_category_membership__social_role_reading, gender_non_conforming_individuals).
narrative_ontology:constraint_victim(gendered_category_membership__social_role_reading, cis_women).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The diffuse collective of individuals and institutions that implicitly or explicitly enforce gendered social norms through recognition, gatekeeping, and social sanction. They benefit from the predictability and order these categories provide.
narrative_ontology:constraint_stakeholder(gendered_category_membership__social_role_reading, social_actors_collectively, agenda_setter,
    institutional, generational, constrained, global).

% Benefit from the stability and shared understanding of 'woman' as a social category, which can provide solidarity and shared spaces. However, they also bear the cost of conforming to gendered social roles and face exclusion if their performance deviates too much from expectations.
narrative_ontology:constraint_stakeholder(gendered_category_membership__social_role_reading, cis_women, beneficiary,
    moderate, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(gendered_category_membership__social_role_reading, cis_women, payer).

% Bear the burden of performing gendered social roles to gain recognition and acceptance into the category 'woman'. They face conditional inclusion, gatekeeping, and the emotional and social costs of non-recognition or misgendering if their performance is deemed insufficient by others.
narrative_ontology:constraint_stakeholder(gendered_category_membership__social_role_reading, trans_women, payer,
    powerless, generational, identity_locked, global).

% Experience social pressure and potential exclusion for not conforming to expected gendered social roles, regardless of their gender identity. They pay the cost of social friction and lack of recognition.
narrative_ontology:constraint_stakeholder(gendered_category_membership__social_role_reading, gender_non_conforming_individuals, payer,
    powerless, biographical, identity_locked, global).

% Benefit from the existence of stable, socially recognized gender categories for organizing roles, spaces, and expectations (e.g., legal systems, healthcare, sports). They rely on these categories for administrative and social coherence.
narrative_ontology:constraint_stakeholder(gendered_category_membership__social_role_reading, social_institutions, beneficiary,
    institutional, civilizational, constrained, global).

% Advocate for gendered category membership based solely on immutable biological markers. Their framework is largely rejected by this social role reading, which prioritizes performance and recognition over biology.
narrative_ontology:constraint_stakeholder(gendered_category_membership__social_role_reading, biological_essentialists, excluded,
    organized, generational, constrained, global).

% Advocate for gendered category membership based solely on subjective identity and self-declaration. While their concerns may overlap, their foundational premise is distinct from this social role reading, which emphasizes external recognition and performance.
narrative_ontology:constraint_stakeholder(gendered_category_membership__social_role_reading, gender_identity_advocates, excluded,
    organized, generational, constrained, global).

% Analyze the mechanisms, impacts, and evolution of gendered social roles and their enforcement. They seek to understand the structural dynamics of this constraint without being directly subject to its performance demands.
narrative_ontology:constraint_stakeholder(gendered_category_membership__social_role_reading, social_theorists, observer,
    analytical, generational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates social interactions by providing shared expectations for gendered roles and behaviors, facilitating social cohesion and predictability within a given cultural context.
% TRANSFER_FUNCTION: Transfers social conformity and performance from individuals to the collective maintenance of gendered categories; transfers social recognition, belonging, and access to gendered spaces to those who perform adequately and are recognized by others.
% ABSENT_VOICES: Individuals who reject gender as a social construct entirely, or those whose performance does not align with expectations and are therefore denied recognition. Also, those who advocate for purely biological or purely identity-based definitions of gender, as their foundational premises are not centered in this reading.
% DISAPPEARANCE_RATIONALE: If the constraint of gendered category membership based on social performance and recognition vanished overnight, social interactions would lose a key organizing principle. Roles, expectations, and access to spaces would undergo significant re-evaluation, leading to widespread social reorganization as new forms of categorization or interaction emerged.
% FOUNDING_PROBLEM: The need for social order, division of labor, and predictable interpersonal interactions based on perceived differences, historically often tied to biological sex and reproductive roles.
% FOUNDING_PROBLEM_CORROBORATION: Proponents argue that shared social roles are still necessary for social cohesion and functional institutions, citing anthropological studies of social organization. Critics, including many social theorists and activists, attest that the founding problem is substantially solved or that the current arrangement creates more harm than benefit, citing sociological studies of gender fluidity and the negative impacts of rigid role expectations. Legislative hearings and public discourse reflect this contestation.
narrative_ontology:disappearance_verdict(gendered_category_membership__social_role_reading, world_rearranges).
narrative_ontology:founding_problem_status(gendered_category_membership__social_role_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gendered_category_membership__social_role_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
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
 *   Extractiveness is moderate (0.45) due to the ongoing effort required for social performance and the costs of conditional inclusion or exclusion. Suppression is moderate (0.6) as it relies on diffuse social pressure, informal gatekeeping, and the threat of non-recognition rather than formal legal enforcement. Theater ratio is moderate (0.4) because while performance is central, it's not purely theatrical; it serves a functional role in social cohesion and identity formation. Over time, extractiveness and suppression have slightly decreased as social norms around gender have become more fluid and contested, while the performative aspect (theater_ratio) has become more salient as the 'naturalness' of gender roles is challenged.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of social actors collectively, this constraint provides necessary social order and predictability. From the perspective of trans women and gender non-conforming individuals, it is a demanding and often exclusionary system that imposes significant burdens for basic recognition. The engine's per-seat classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Social actors collectively (the diffuse enforcers of norms) and cis women (who benefit from category stability) are beneficiaries. Trans women and gender non-conforming individuals are primary targets, bearing the performance costs and risks of exclusion. Cis women also bear costs of conformity. The 'identity_locked' exit option for trans women and gender non-conforming individuals reflects that their self-concept is deeply tied to their gender identity, making 'exit' from seeking recognition within these categories extremely difficult or impossible without profound personal cost.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    social_vs_internalized_suppression,
    'To what extent is the suppression experienced by individuals structural (external social pressure, gatekeeping) versus internalized (self-policing, fear of non-recognition)?',
    'Longitudinal studies tracking individuals'' experiences of gender performance and recognition across different social contexts and over time, particularly after changes in external social acceptance.',
    'If suppression is largely internalized, the effective extractiveness and persistence of the constraint are higher than structural measures suggest, as individuals carry the burden of performance even in less overtly hostile environments. If primarily structural, changes in social norms could more readily reduce its impact.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(social_vs_internalized_suppression, empirical, 'Distinguishing between external and internal mechanisms of social suppression in gender performance.').

omega_variable(
    victim_burden_distribution,
    'How is the burden of performance and risk of exclusion distributed between trans women and cis women under this social role reading?',
    'Comparative sociological studies analyzing the specific forms of gatekeeping, performance demands, and social sanctions faced by each group, and the relative ease or difficulty of achieving recognition.',
    'A clearer understanding of burden distribution would refine the directionality and effective extraction for each victim group, potentially reclassifying one as more severely targeted than the other, or revealing distinct mechanisms of extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(victim_burden_distribution, empirical, 'Clarifying the differential impact of social role performance on various gendered groups.').

omega_variable(
    natural_vs_constructed_social_roles,
    'Is the observed pattern of gendered social performance an emergent property of human social interaction, or is it primarily a constructed norm maintained through cultural and institutional practices?',
    'Cross-cultural anthropological comparisons of gender systems and historical analysis of the evolution of gender roles, alongside psychological studies of social categorization.',
    'If largely emergent, the constraint leans towards a ''mountain'' or ''rope'' (natural coordination); if primarily constructed, it reinforces its ''tangled_rope'' classification, highlighting the role of active maintenance and potential for change.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(natural_vs_constructed_social_roles, conceptual, 'Ambiguity regarding the naturalness versus social construction of gendered social roles.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gendered_category_membership__social_role_reading, 1950, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gend_tr_t1950, gendered_category_membership__social_role_reading, theater_ratio, 1950, 0.2).
narrative_ontology:measurement(gend_tr_t1970, gendered_category_membership__social_role_reading, theater_ratio, 1970, 0.25).
narrative_ontology:measurement(gend_tr_t1990, gendered_category_membership__social_role_reading, theater_ratio, 1990, 0.3).
narrative_ontology:measurement(gend_tr_t2010, gendered_category_membership__social_role_reading, theater_ratio, 2010, 0.35).
narrative_ontology:measurement(gend_tr_t2024, gendered_category_membership__social_role_reading, theater_ratio, 2024, 0.4).

% Extraction over time
narrative_ontology:measurement(gend_be_t1950, gendered_category_membership__social_role_reading, base_extractiveness, 1950, 0.6).
narrative_ontology:measurement(gend_be_t1970, gendered_category_membership__social_role_reading, base_extractiveness, 1970, 0.55).
narrative_ontology:measurement(gend_be_t1990, gendered_category_membership__social_role_reading, base_extractiveness, 1990, 0.5).
narrative_ontology:measurement(gend_be_t2010, gendered_category_membership__social_role_reading, base_extractiveness, 2010, 0.48).
narrative_ontology:measurement(gend_be_t2024, gendered_category_membership__social_role_reading, base_extractiveness, 2024, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(gend_su_t1950, gendered_category_membership__social_role_reading, suppression_requirement, 1950, 0.8).
narrative_ontology:measurement(gend_su_t1970, gendered_category_membership__social_role_reading, suppression_requirement, 1970, 0.75).
narrative_ontology:measurement(gend_su_t1990, gendered_category_membership__social_role_reading, suppression_requirement, 1990, 0.68).
narrative_ontology:measurement(gend_su_t2010, gendered_category_membership__social_role_reading, suppression_requirement, 2010, 0.63).
narrative_ontology:measurement(gend_su_t2024, gendered_category_membership__social_role_reading, suppression_requirement, 2024, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gendered_category_membership__social_role_reading, identity_coordination).
narrative_ontology:affects_constraint(gendered_category_membership__social_role_reading, gendered_spaces_access).
narrative_ontology:affects_constraint(gendered_category_membership__social_role_reading, gendered_labor_roles).
narrative_ontology:affects_constraint(gendered_category_membership__social_role_reading, gendered_language_norms).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'gendered_category_membership' kernel, each representing a distinct structural claim about how gendered categories are constituted. This 'social_role_reading' focuses on performance and recognition, distinct from biological or identity-based definitions.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
