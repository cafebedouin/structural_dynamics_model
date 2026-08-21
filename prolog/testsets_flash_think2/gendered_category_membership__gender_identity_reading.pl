% ============================================================================
% CONSTRAINT STORY: gendered_category_membership__gender_identity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_gendered_category_membership__gender_identity_reading, []).

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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: gendered_category_membership__gender_identity_reading
 *   human_readable: Gendered Category Membership (Gender Identity Reading)
 *   domain: social_ontology/political_philosophy/bioethics
 *
 * SUMMARY:
 *   This constraint defines membership in gendered categories, particularly
 *   'woman', based on an individual's subjective gender identity and
 *   self-declaration. It asserts that self-identification is the primary
 *   determinant, leading to the inclusion of trans women in categories
 *   previously understood as sex-segregated. The constraint is actively
 *   enforced through social norms, institutional policies, and legal
 *   frameworks, often imposing costs on those who resist this definition.
 *
 * KEY AGENTS:
 *   - trans_individuals: Beneficiary (moderate/identity_locked)
 *   - gender_identity_advocates: Agenda_setter (organized/mobile)
 *   - cis_women_resisting_self_id: Payer (moderate/constrained)
 *   - gender_critical_feminists: Payer/Excluded (organized/constrained)
 *   - social_institutions: Agenda_setter (institutional/constrained)
 *   - analytical_observers: Observer (analytical/analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gendered_category_membership__gender_identity_reading, 0.6).
domain_priors:suppression_score(gendered_category_membership__gender_identity_reading, 0.75).
domain_priors:theater_ratio(gendered_category_membership__gender_identity_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gendered_category_membership__gender_identity_reading, extractiveness, 0.6).
narrative_ontology:constraint_metric(gendered_category_membership__gender_identity_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(gendered_category_membership__gender_identity_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gendered_category_membership__gender_identity_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(gendered_category_membership__gender_identity_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gendered_category_membership__gender_identity_reading, tangled_rope).
narrative_ontology:human_readable(gendered_category_membership__gender_identity_reading, "Gendered Category Membership (Gender Identity Reading)").
narrative_ontology:topic_domain(gendered_category_membership__gender_identity_reading, "social_ontology/political_philosophy/bioethics").

domain_priors:requires_active_enforcement(gendered_category_membership__gender_identity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gendered_category_membership__gender_identity_reading, '220c65cd-1449-406f-92fd-2e76072a3fe2').
narrative_ontology:cs_kernel_codification('220c65cd-1449-406f-92fd-2e76072a3fe2', implicit).
narrative_ontology:cs_authority_grounding('220c65cd-1449-406f-92fd-2e76072a3fe2', practice).
narrative_ontology:cs_interpretation_layer_present('220c65cd-1449-406f-92fd-2e76072a3fe2').
narrative_ontology:cs_reading_relation('220c65cd-1449-406f-92fd-2e76072a3fe2', gendered_category_membership__biological_sex_reading, forecloses).
narrative_ontology:cs_reading_relation('220c65cd-1449-406f-92fd-2e76072a3fe2', gendered_category_membership__social_role_reading, coexists_with).
narrative_ontology:cs_axiom('220c65cd-1449-406f-92fd-2e76072a3fe2', foundational, gender_identity_is_self_determined).
narrative_ontology:cs_axiom_status(gender_identity_is_self_determined, holdable).
narrative_ontology:cs_axiom_grounding('220c65cd-1449-406f-92fd-2e76072a3fe2', gender_identity_is_self_determined, deontological).
narrative_ontology:cs_axiom('220c65cd-1449-406f-92fd-2e76072a3fe2', foundational, gender_identity_determines_social_gender).
narrative_ontology:cs_axiom_status(gender_identity_determines_social_gender, holdable).
narrative_ontology:cs_axiom_grounding('220c65cd-1449-406f-92fd-2e76072a3fe2', gender_identity_determines_social_gender, conventional).
narrative_ontology:cs_reference_frame('220c65cd-1449-406f-92fd-2e76072a3fe2', self_declaration_as_primary_gender_marker).
narrative_ontology:cs_drift_state('220c65cd-1449-406f-92fd-2e76072a3fe2', contemporary_social_discourse, gap(stable, minor, true)).
narrative_ontology:cs_created_at('220c65cd-1449-406f-92fd-2e76072a3fe2', '').
narrative_ontology:cs_kernel_id(gendered_category_membership__gender_identity_reading, gendered_category_membership).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gendered_category_membership__gender_identity_reading, trans_individuals).
narrative_ontology:constraint_beneficiary(gendered_category_membership__gender_identity_reading, gender_identity_advocates).
narrative_ontology:constraint_victim(gendered_category_membership__gender_identity_reading, cis_women_resisting_self_id).
narrative_ontology:constraint_victim(gendered_category_membership__gender_identity_reading, gender_critical_feminists).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Gain recognition and inclusion in gendered categories aligned with their self-declared identity. Their ability to participate in society as their affirmed gender is contingent on this constraint's enforcement. Exit from this framework would mean denying their identity.
narrative_ontology:constraint_stakeholder(gendered_category_membership__gender_identity_reading, trans_individuals, beneficiary,
    moderate, biographical, identity_locked, global).

% Actively promote and enforce the understanding of gendered categories based on gender identity. They work to codify these definitions in law and policy, and to shape social norms. They benefit from the expansion of rights and recognition for transgender individuals.
narrative_ontology:constraint_stakeholder(gendered_category_membership__gender_identity_reading, gender_identity_advocates, agenda_setter,
    organized, generational, mobile, global).

% Bear social and reputational costs for questioning or resisting the redefinition of gendered categories based solely on self-declared identity. They perceive a loss of sex-based rights and spaces, and face accusations of transphobia. Their options are to conform, resist and face consequences, or attempt to create alternative spaces.
narrative_ontology:constraint_stakeholder(gendered_category_membership__gender_identity_reading, cis_women_resisting_self_id, payer,
    moderate, biographical, constrained, global).

% Actively articulate and organize around the view that gendered categories should be based on biological sex. They face significant social and institutional pressure, including deplatforming and professional repercussions, for their stance. They are often excluded from mainstream discourse on gender.
narrative_ontology:constraint_stakeholder(gendered_category_membership__gender_identity_reading, gender_critical_feminists, payer,
    organized, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(gendered_category_membership__gender_identity_reading, gender_critical_feminists, excluded).

% Governments, NGOs, and other organizations that adopt policies and practices aligning with gender identity as the primary determinant of gendered category membership. They benefit from appearing inclusive and progressive, but may face internal dissent or legal challenges.
narrative_ontology:constraint_stakeholder(gendered_category_membership__gender_identity_reading, social_institutions, agenda_setter,
    institutional, generational, constrained, national).

% Academics, philosophers, and researchers who analyze the conceptual, social, and ethical implications of defining gendered categories by identity. They aim to understand the structural dynamics without direct participation in the contest.
narrative_ontology:constraint_stakeholder(gendered_category_membership__gender_identity_reading, analytical_observers, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(gendered_category_membership__gender_identity_reading, trans_individuals).
narrative_ontology:fixing_cost_class(gendered_category_membership__gender_identity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To establish a coherent and inclusive definition of gendered categories, particularly 'woman', based on subjective identity, facilitating social recognition and legal protections for transgender individuals.
% TRANSFER_FUNCTION: Transfers social recognition, access to gendered spaces, and normative authority from definitions based on biological sex or social role to definitions based on gender identity. It imposes social and reputational costs on those who resist this redefinition.
% ABSENT_VOICES: Children and future generations, whose understanding of gendered categories is being shaped, and those who are silenced or deplatformed for expressing dissenting views, particularly gender-critical feminists who are often excluded from mainstream discourse.
% DISAPPEARANCE_RATIONALE: If the constraint vanished, the social and legal landscape around gender identity would immediately become highly contested and fragmented. Definitions of 'woman' and access to gendered spaces would revert to more varied interpretations, likely leading to a re-emphasis on biological sex or social role in many contexts, and a significant shift in power dynamics within feminist and LGBTQ+ movements.
% FOUNDING_PROBLEM: To address the historical exclusion and marginalization of transgender individuals from gendered categories and spaces, and to affirm their self-identified gender as authentic and valid.
% FOUNDING_PROBLEM_CORROBORATION: Transgender advocacy organizations and many human rights bodies corroborate the ongoing problem of marginalization. However, gender-critical feminists contest the solution's impact on cis women's rights and spaces, arguing it creates new problems for sex-based rights; legislative debates and academic critiques from outside the benefiting parties support the contested status.
narrative_ontology:disappearance_verdict(gendered_category_membership__gender_identity_reading, world_rearranges).
narrative_ontology:founding_problem_status(gendered_category_membership__gender_identity_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gendered_category_membership__gender_identity_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(gendered_category_membership__gender_identity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(gendered_category_membership__gender_identity_reading, 0.6, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gendered_category_membership__gender_identity_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(gendered_category_membership__gender_identity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(gendered_category_membership__gender_identity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.62 at interval end) due to the social and reputational costs imposed on those who resist the identity-based definition, including accusations of transphobia and exclusion from discourse. Suppression is high (0.75) because the persistence of this definition relies on active social enforcement, deplatforming, and institutional pressure to limit alternatives or dissenting views. Theater ratio is low (0.2) as the enforcement is direct and functional in shaping social and institutional practice, rather than merely performative. Resistance is high (0.7) reflecting ongoing, organized opposition from gender-critical groups.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of trans individuals and gender identity advocates, this constraint functions as a vital coordination mechanism for social recognition and inclusion, enabling individuals to live authentically. From the perspective of cis women resisting self-ID and gender-critical feminists, the same constraint operates as an extractive force, eroding sex-based rights and imposing significant social costs for dissent. The engine's per-seat classification will reflect this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Trans individuals are primary beneficiaries (d near 0.0) as the constraint affirms their identity and grants access to categories. Gender identity advocates also benefit by advancing their agenda (d near 0.1-0.2). Cis women resisting self-ID and gender-critical feminists are targets (d near 0.8-0.9) as they bear the costs of non-compliance and perceived loss of sex-based protections. Social institutions act as agenda-setters, benefiting from perceived progressiveness while navigating internal and external pressures (d near 0.3-0.4).
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    identity_vs_material_reality,
    'To what extent does subjective gender identity override or interact with material biological reality in defining social categories?',
    'Long-term empirical study of outcomes in gender-segregated spaces (e.g., sports, prisons, changing rooms) and philosophical consensus on the nature of social categories and their boundaries.',
    'If material reality is deemed primary for certain contexts, the constraint''s legitimacy erodes in those contexts, and its classification shifts towards pure extraction for those resisting. If identity is fully affirmed as the sole determinant, the constraint is seen as pure coordination.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_vs_material_reality, conceptual, 'The fundamental tension between subjective identity and objective biological sex in category definition.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression (social ostracism, deplatforming, professional repercussions) structural or internalized?',
    'Post-exit suppression trajectory: if resistance persists after the extractive mechanism is removed, reclassify as partially internalized. If the social costs immediately dissipate upon a shift in norms, it is primarily structural.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests — targets carry the suppression with them after exit. If purely structural, removing the external pressure would lead to immediate shifts in behavior and discourse.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism in social discourse.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gendered_category_membership__gender_identity_reading, 2000, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gend_tr_t2000, gendered_category_membership__gender_identity_reading, theater_ratio, 2000, 0.15).
narrative_ontology:measurement(gend_tr_t2005, gendered_category_membership__gender_identity_reading, theater_ratio, 2005, 0.16).
narrative_ontology:measurement(gend_tr_t2010, gendered_category_membership__gender_identity_reading, theater_ratio, 2010, 0.17).
narrative_ontology:measurement(gend_tr_t2015, gendered_category_membership__gender_identity_reading, theater_ratio, 2015, 0.18).
narrative_ontology:measurement(gend_tr_t2020, gendered_category_membership__gender_identity_reading, theater_ratio, 2020, 0.19).
narrative_ontology:measurement(gend_tr_t2025, gendered_category_membership__gender_identity_reading, theater_ratio, 2025, 0.2).

% Extraction over time
narrative_ontology:measurement(gend_be_t2000, gendered_category_membership__gender_identity_reading, base_extractiveness, 2000, 0.45).
narrative_ontology:measurement(gend_be_t2005, gendered_category_membership__gender_identity_reading, base_extractiveness, 2005, 0.5).
narrative_ontology:measurement(gend_be_t2010, gendered_category_membership__gender_identity_reading, base_extractiveness, 2010, 0.55).
narrative_ontology:measurement(gend_be_t2015, gendered_category_membership__gender_identity_reading, base_extractiveness, 2015, 0.58).
narrative_ontology:measurement(gend_be_t2020, gendered_category_membership__gender_identity_reading, base_extractiveness, 2020, 0.6).
narrative_ontology:measurement(gend_be_t2025, gendered_category_membership__gender_identity_reading, base_extractiveness, 2025, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(gend_su_t2000, gendered_category_membership__gender_identity_reading, suppression_requirement, 2000, 0.55).
narrative_ontology:measurement(gend_su_t2005, gendered_category_membership__gender_identity_reading, suppression_requirement, 2005, 0.6).
narrative_ontology:measurement(gend_su_t2010, gendered_category_membership__gender_identity_reading, suppression_requirement, 2010, 0.65).
narrative_ontology:measurement(gend_su_t2015, gendered_category_membership__gender_identity_reading, suppression_requirement, 2015, 0.7).
narrative_ontology:measurement(gend_su_t2020, gendered_category_membership__gender_identity_reading, suppression_requirement, 2020, 0.73).
narrative_ontology:measurement(gend_su_t2025, gendered_category_membership__gender_identity_reading, suppression_requirement, 2025, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gendered_category_membership__gender_identity_reading, identity_coordination).
narrative_ontology:affects_constraint(gendered_category_membership__gender_identity_reading, sex_segregated_spaces_definition).
narrative_ontology:affects_constraint(gendered_category_membership__gender_identity_reading, womens_rights_framework).
narrative_ontology:affects_constraint(gendered_category_membership__gender_identity_reading, gender_identity_legal_recognition).

% DUAL FORMULATION NOTE:
% This constraint is the 'gender_identity_reading' of the 'gendered_category_membership' kernel. It focuses on subjective identity as the primary determinant of gendered categories, distinct from readings based on biological sex or social role.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
