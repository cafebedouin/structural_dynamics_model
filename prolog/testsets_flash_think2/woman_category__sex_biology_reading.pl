% ============================================================================
% CONSTRAINT STORY: woman_category__sex_biology_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_woman_category__sex_biology_reading, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: woman_category__sex_biology_reading
 *   human_readable: Category 'Woman' Defined by Sex Biology
 *   domain: political_philosophy/law/social_policy/bioethics
 *
 * SUMMARY:
 *   This constraint defines 'woman' as an adult human female with XX
 *   chromosomes and typical female reproductive anatomy, grounding the
 *   category in biological sex. It is presented as a natural, immutable fact,
 *   often by advocates for sex-based rights. However, its application in
 *   social policy and law, particularly in contexts like sports, prisons, and
 *   single-sex spaces, leads to significant extraction from those who do not
 *   fit this definition (e.g., transgender women) and requires active
 *   enforcement to maintain against strong resistance. The claimed type is
 *   'mountain' reflecting its proponents' framing, while the metrics reflect
 *   its operational reality in a contested social landscape.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(woman_category__sex_biology_reading, 0.78).
domain_priors:suppression_score(woman_category__sex_biology_reading, 0.85).
domain_priors:theater_ratio(woman_category__sex_biology_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(woman_category__sex_biology_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(woman_category__sex_biology_reading, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(woman_category__sex_biology_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(woman_category__sex_biology_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(woman_category__sex_biology_reading, resistance, 0.8).

% --- Constraint claim ---
narrative_ontology:constraint_claim(woman_category__sex_biology_reading, mountain).
narrative_ontology:human_readable(woman_category__sex_biology_reading, "Category 'Woman' Defined by Sex Biology").
narrative_ontology:topic_domain(woman_category__sex_biology_reading, "political_philosophy/law/social_policy/bioethics").

domain_priors:requires_active_enforcement(woman_category__sex_biology_reading).
domain_priors:emerges_naturally(woman_category__sex_biology_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(woman_category__sex_biology_reading, '706d1ee5-c97f-4f13-930e-e17ce2c168da').
narrative_ontology:cs_kernel_codification('706d1ee5-c97f-4f13-930e-e17ce2c168da', formalized).
narrative_ontology:cs_authority_grounding('706d1ee5-c97f-4f13-930e-e17ce2c168da', expertise).
narrative_ontology:cs_interpretation_layer_present('706d1ee5-c97f-4f13-930e-e17ce2c168da').
narrative_ontology:cs_reading_relation('706d1ee5-c97f-4f13-930e-e17ce2c168da', woman_category__gender_identity_reading, forecloses).
narrative_ontology:cs_reading_relation('706d1ee5-c97f-4f13-930e-e17ce2c168da', woman_category__intersex_accommodation_reading, influences).
narrative_ontology:cs_axiom('706d1ee5-c97f-4f13-930e-e17ce2c168da', foundational, human_sex_is_binary).
narrative_ontology:cs_axiom_status(human_sex_is_binary, holdable).
narrative_ontology:cs_axiom_grounding('706d1ee5-c97f-4f13-930e-e17ce2c168da', human_sex_is_binary, empirically_contingent).
narrative_ontology:cs_axiom('706d1ee5-c97f-4f13-930e-e17ce2c168da', foundational, sex_is_immutable).
narrative_ontology:cs_axiom_status(sex_is_immutable, holdable).
narrative_ontology:cs_axiom_grounding('706d1ee5-c97f-4f13-930e-e17ce2c168da', sex_is_immutable, empirically_contingent).
narrative_ontology:cs_reference_frame('706d1ee5-c97f-4f13-930e-e17ce2c168da', biological_dimorphism_framework).
narrative_ontology:cs_drift_state('706d1ee5-c97f-4f13-930e-e17ce2c168da', contemporary_social_discourse, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('706d1ee5-c97f-4f13-930e-e17ce2c168da', '').
narrative_ontology:cs_kernel_id(woman_category__sex_biology_reading, woman_category).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(woman_category__sex_biology_reading, biological_women).
narrative_ontology:constraint_beneficiary(woman_category__sex_biology_reading, advocates_for_sex_based_rights).
narrative_ontology:constraint_victim(woman_category__sex_biology_reading, transgender_women).
narrative_ontology:constraint_victim(woman_category__sex_biology_reading, some_intersex_people).
narrative_ontology:constraint_victim(woman_category__sex_biology_reading, advocates_for_gender_identity_inclusion).
narrative_ontology:constraint_vindicates(woman_category__sex_biology_reading, sex_dimorphism_in_humans).
narrative_ontology:constraint_vindicates(woman_category__sex_biology_reading, biological_reality_of_sex).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Individuals who fit the definition of adult human female with XX chromosomes and typical female reproductive anatomy. They benefit from sex-segregated spaces, data collection, and policies designed to address sex-specific issues, and their identity is often tied to this definition.
narrative_ontology:constraint_stakeholder(woman_category__sex_biology_reading, biological_women, beneficiary,
    organized, generational, identity_locked, global).

% Individuals who identify as women but do not fit the biological definition. They are excluded from sex-segregated spaces and protections based on this definition, facing social and legal barriers to recognition.
narrative_ontology:constraint_stakeholder(woman_category__sex_biology_reading, transgender_women, payer,
    powerless, biographical, trapped, global).

% Groups and individuals who actively champion the definition of 'woman' based on sex biology, often arguing for the necessity of sex-segregated spaces and policies for the protection and advancement of biological women. They work to codify and enforce this definition in law and policy.
narrative_ontology:constraint_stakeholder(woman_category__sex_biology_reading, advocates_for_sex_based_rights, agenda_setter,
    organized, generational, mobile, global).

% Groups and individuals who advocate for definitions of 'woman' that include transgender women, based on gender identity. They bear the costs of resisting the sex-biology definition and face significant opposition in policy debates.
narrative_ontology:constraint_stakeholder(woman_category__sex_biology_reading, advocates_for_gender_identity_inclusion, payer,
    organized, generational, constrained, global).

% Government officials and legislative bodies responsible for creating and enforcing laws and policies that define categories like 'woman' for various purposes (e.g., sports, healthcare, legal identity). They navigate competing definitions and public pressure.
narrative_ontology:constraint_stakeholder(woman_category__sex_biology_reading, policy_makers, agenda_setter,
    institutional, biographical, constrained, national).

% Individuals with variations in sex characteristics that do not fit typical XX/XY chromosomal or anatomical definitions. They may be ambiguously included or excluded depending on the specific application of the sex-biology definition, leading to uncertainty and marginalization.
narrative_ontology:constraint_stakeholder(woman_category__sex_biology_reading, some_intersex_people, payer,
    powerless, biographical, constrained, global).

% Organizations that set rules for athletic competition, often implementing sex-segregated categories based on biological sex to ensure fair play and safety. They face pressure from both sides of the debate.
narrative_ontology:constraint_stakeholder(woman_category__sex_biology_reading, sports_governing_bodies, agenda_setter,
    institutional, biographical, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(woman_category__sex_biology_reading, biological_women).
narrative_ontology:fixing_cost_class(woman_category__sex_biology_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a clear, biologically defined category for 'woman' to enable sex-segregated spaces, data collection, and policies intended to address sex-specific issues (e.g., violence against women, reproductive health, fair sports competition).
% TRANSFER_FUNCTION: Transfers access, recognition, and protection within sex-segregated categories to those defined by biological sex, while denying or complicating it for those who do not fit this definition. It also transfers social and political capital to advocates of this definition.
% ABSENT_VOICES: Transgender women and their advocates, and some intersex advocates, are often excluded from the policy-making spaces where this definition is codified, or their perspectives are actively dismissed as irrelevant or harmful to the interests of biological women.
% DISAPPEARANCE_RATIONALE: If this definition vanished overnight, sex-segregated spaces and policies would lose their clear biological boundary. This would lead to significant re-evaluation and reorganization of social structures, legal frameworks, and data collection, as the concept of 'woman' would become entirely self-identified or highly fluid, making sex-based provisions difficult to implement.
% FOUNDING_PROBLEM: The need to categorize humans for reproductive roles, social organization, and to address sex-specific vulnerabilities and inequalities, particularly for the protection and advancement of the female sex.
% FOUNDING_PROBLEM_CORROBORATION: Biologists, medical professionals, and some feminist scholars corroborate the ongoing relevance of biological sex for understanding human populations and addressing sex-specific issues. This is often supported by historical legal and social frameworks that have long recognized sex as a primary category.
narrative_ontology:disappearance_verdict(woman_category__sex_biology_reading, world_rearranges).
narrative_ontology:founding_problem_status(woman_category__sex_biology_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(woman_category__sex_biology_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(woman_category__sex_biology_reading, 'none', 1).
narrative_ontology:epsilon_provenance(woman_category__sex_biology_reading, 0.78, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(woman_category__sex_biology_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(woman_category__sex_biology_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(woman_category__sex_biology_reading, ExtMetricName, E),
    domain_priors:suppression_score(woman_category__sex_biology_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(woman_category__sex_biology_reading),
    narrative_ontology:constraint_metric(woman_category__sex_biology_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(woman_category__sex_biology_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(woman_category__sex_biology_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The high extractiveness (0.78) stems from the denial of access and recognition to individuals who identify as women but are excluded by this biological definition. Suppression (0.85) is very high due to the active legal and social enforcement required to maintain this definition in policy, often involving the suppression of alternative definitions and the voices of those excluded. The theater ratio (0.40) is moderate; while the biological basis is genuinely believed by proponents, the public defense and enforcement of this definition in policy often involves performative aspects to justify exclusions and resist challenges. Accessibility collapse is high for those excluded, and resistance is also high from those advocating for broader inclusion.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of biological women and sex-based rights advocates, this constraint is a necessary, natural boundary (a mountain) that protects their interests and ensures fair competition. From the perspective of transgender women and their allies, it is a highly extractive and suppressive snare that denies their identity and rights. The engine's classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Biological women and advocates for sex-based rights are beneficiaries, gaining from the clarity and protections afforded by this definition (low directionality). Transgender women and some intersex people are targets, bearing the costs of exclusion and non-recognition (high directionality). Policy makers and sports governing bodies act as agenda-setters, mediating the application and enforcement of this definition. Advocates for gender identity inclusion are also targets, as their efforts are actively suppressed by this constraint.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_social_construct,
    'Is the definition of ''woman'' based on sex biology a genuine natural law (mountain) or a socially constructed boundary that benefits identifiable agents (snare/tangled_rope)?',
    'Analysis of the historical and cross-cultural variability of sex-based social categories, alongside the identification of specific institutional actors who benefit from its enforcement. If the benefits are concentrated and the enforcement is active, it leans towards a construct.',
    'If resolved as a social construct, the classification would shift from mountain to tangled_rope or snare, reflecting its active maintenance and extractive function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_social_construct, conceptual, 'Ambiguity between biological fact and social application of the ''woman'' category.').

omega_variable(
    intersex_inclusion_ambiguity,
    'How does this definition consistently accommodate the full spectrum of intersex variations, given its focus on ''typical'' XX chromosomes and female anatomy?',
    'Detailed case studies of intersex individuals'' experiences with this definition in policy and law, assessing whether their inclusion/exclusion is consistent and non-arbitrary. If inconsistent, the definition''s internal coherence is challenged.',
    'If the definition proves inconsistent or arbitrary in its application to intersex individuals, its claim to naturalness and universality is weakened, potentially increasing its measured extractiveness and suppression for this group.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(intersex_inclusion_ambiguity, empirical, 'Consistency of intersex inclusion within a typical sex-biology definition.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression of alternative definitions primarily structural (legal/policy barriers) or internalized (social stigma, self-censorship)?',
    'Post-policy-change trajectory: if legal barriers are removed but social stigma and self-censorship persist, it indicates a significant internalized component to suppression.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests, as targets carry the suppression with them even after formal barriers are removed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for alternative definitions of ''woman''.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(woman_category__sex_biology_reading, 2000, 2030).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(woma_tr_t2000, woman_category__sex_biology_reading, theater_ratio, 2000, 0.2).
narrative_ontology:measurement(woma_tr_t2005, woman_category__sex_biology_reading, theater_ratio, 2005, 0.25).
narrative_ontology:measurement(woma_tr_t2010, woman_category__sex_biology_reading, theater_ratio, 2010, 0.3).
narrative_ontology:measurement(woma_tr_t2015, woman_category__sex_biology_reading, theater_ratio, 2015, 0.35).
narrative_ontology:measurement(woma_tr_t2020, woman_category__sex_biology_reading, theater_ratio, 2020, 0.38).
narrative_ontology:measurement(woma_tr_t2025, woman_category__sex_biology_reading, theater_ratio, 2025, 0.39).
narrative_ontology:measurement(woma_tr_t2030, woman_category__sex_biology_reading, theater_ratio, 2030, 0.4).

% Extraction over time
narrative_ontology:measurement(woma_be_t2000, woman_category__sex_biology_reading, base_extractiveness, 2000, 0.6).
narrative_ontology:measurement(woma_be_t2005, woman_category__sex_biology_reading, base_extractiveness, 2005, 0.65).
narrative_ontology:measurement(woma_be_t2010, woman_category__sex_biology_reading, base_extractiveness, 2010, 0.7).
narrative_ontology:measurement(woma_be_t2015, woman_category__sex_biology_reading, base_extractiveness, 2015, 0.74).
narrative_ontology:measurement(woma_be_t2020, woman_category__sex_biology_reading, base_extractiveness, 2020, 0.76).
narrative_ontology:measurement(woma_be_t2025, woman_category__sex_biology_reading, base_extractiveness, 2025, 0.77).
narrative_ontology:measurement(woma_be_t2030, woman_category__sex_biology_reading, base_extractiveness, 2030, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(woma_su_t2000, woman_category__sex_biology_reading, suppression_requirement, 2000, 0.65).
narrative_ontology:measurement(woma_su_t2005, woman_category__sex_biology_reading, suppression_requirement, 2005, 0.7).
narrative_ontology:measurement(woma_su_t2010, woman_category__sex_biology_reading, suppression_requirement, 2010, 0.75).
narrative_ontology:measurement(woma_su_t2015, woman_category__sex_biology_reading, suppression_requirement, 2015, 0.8).
narrative_ontology:measurement(woma_su_t2020, woman_category__sex_biology_reading, suppression_requirement, 2020, 0.83).
narrative_ontology:measurement(woma_su_t2025, woman_category__sex_biology_reading, suppression_requirement, 2025, 0.84).
narrative_ontology:measurement(woma_su_t2030, woman_category__sex_biology_reading, suppression_requirement, 2030, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
