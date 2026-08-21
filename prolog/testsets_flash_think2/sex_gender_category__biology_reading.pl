% ============================================================================
% CONSTRAINT STORY: sex_gender_category__biology_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: sex_gender_category__biology_reading
 *   human_readable: Sex Category Defined by Immutable Reproductive Biology
 *   domain: social_ontology/identity_politics/legal_classification
 *
 * SUMMARY:
 *   This constraint defines sex/gender category membership based on immutable
 *   reproductive biology (chromosomes, anatomy at birth). Proponents claim it
 *   is a natural, immutable truth (Mountain), essential for legal and social
 *   clarity. However, its operation involves significant extraction from
 *   transgender and intersex individuals, who are denied recognition or
 *   forced into binary categories. It also requires active enforcement
 *   against evolving scientific understanding and social challenges. The high
 *   resistance and extractiveness, despite the 'Mountain' claim, are intended
 *   to highlight the divergence between the claimed and actual structural
 *   type.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sex_gender_category__biology_reading, 0.65).
domain_priors:suppression_score(sex_gender_category__biology_reading, 0.75).
domain_priors:theater_ratio(sex_gender_category__biology_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sex_gender_category__biology_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(sex_gender_category__biology_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(sex_gender_category__biology_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(sex_gender_category__biology_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(sex_gender_category__biology_reading, resistance, 0.8).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sex_gender_category__biology_reading, mountain).
narrative_ontology:human_readable(sex_gender_category__biology_reading, "Sex Category Defined by Immutable Reproductive Biology").
narrative_ontology:topic_domain(sex_gender_category__biology_reading, "social_ontology/identity_politics/legal_classification").

domain_priors:requires_active_enforcement(sex_gender_category__biology_reading).
domain_priors:emerges_naturally(sex_gender_category__biology_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(sex_gender_category__biology_reading, '9233aa04-fe42-4ffd-a416-7cda1441b3e3').
narrative_ontology:cs_kernel_codification('9233aa04-fe42-4ffd-a416-7cda1441b3e3', formalized).
narrative_ontology:cs_authority_grounding('9233aa04-fe42-4ffd-a416-7cda1441b3e3', lineage).
narrative_ontology:cs_interpretation_layer_present('9233aa04-fe42-4ffd-a416-7cda1441b3e3').
narrative_ontology:cs_reading_relation('9233aa04-fe42-4ffd-a416-7cda1441b3e3', sex_gender_category__identity_reading, forecloses).
narrative_ontology:cs_reading_relation('9233aa04-fe42-4ffd-a416-7cda1441b3e3', sex_gender_category__hybrid_reading, forecloses).
narrative_ontology:cs_axiom('9233aa04-fe42-4ffd-a416-7cda1441b3e3', foundational, sex_is_binary_and_immutable).
narrative_ontology:cs_axiom_status(sex_is_binary_and_immutable, holdable).
narrative_ontology:cs_axiom_grounding('9233aa04-fe42-4ffd-a416-7cda1441b3e3', sex_is_binary_and_immutable, empirically_contingent).
narrative_ontology:cs_axiom('9233aa04-fe42-4ffd-a416-7cda1441b3e3', foundational, reproductive_capacity_defines_sex_categories).
narrative_ontology:cs_axiom_status(reproductive_capacity_defines_sex_categories, holdable).
narrative_ontology:cs_axiom_grounding('9233aa04-fe42-4ffd-a416-7cda1441b3e3', reproductive_capacity_defines_sex_categories, empirically_contingent).
narrative_ontology:cs_reference_frame('9233aa04-fe42-4ffd-a416-7cda1441b3e3', immutable_sex_binary).
narrative_ontology:cs_drift_state('9233aa04-fe42-4ffd-a416-7cda1441b3e3', contemporary_scientific_and_social_challenge, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('9233aa04-fe42-4ffd-a416-7cda1441b3e3', '').
narrative_ontology:cs_kernel_id(sex_gender_category__biology_reading, sex_gender_category).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sex_gender_category__biology_reading, biological_essentialists).
narrative_ontology:constraint_beneficiary(sex_gender_category__biology_reading, cis_women).
narrative_ontology:constraint_beneficiary(sex_gender_category__biology_reading, some_feminist_groups).
narrative_ontology:constraint_victim(sex_gender_category__biology_reading, transgender_individuals).
narrative_ontology:constraint_victim(sex_gender_category__biology_reading, intersex_individuals).
narrative_ontology:constraint_victim(sex_gender_category__biology_reading, gender_non_conforming_individuals).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(sex_gender_category__biology_reading, cis_women).
narrative_ontology:constraint_vindicates(sex_gender_category__biology_reading, biological_determinism).
narrative_ontology:constraint_vindicates(sex_gender_category__biology_reading, sex_binary_invariance).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Advocate for and enforce the definition of sex based solely on immutable reproductive biology, viewing it as a foundational and unchangeable truth necessary for social order and legal clarity. They actively resist alternative definitions.
narrative_ontology:constraint_stakeholder(sex_gender_category__biology_reading, biological_essentialists, agenda_setter,
    institutional, civilizational, identity_locked, universal).

% Benefit from the clarity of sex-segregated spaces and rights defined by this reading, which they see as protecting their interests. However, they are also constrained by rigid definitions of womanhood that can limit their roles or self-expression.
narrative_ontology:constraint_stakeholder(sex_gender_category__biology_reading, cis_women, beneficiary,
    organized, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(sex_gender_category__biology_reading, cis_women, payer).

% Are denied category membership in their affirmed gender, leading to legal, social, and medical exclusion. They face significant discrimination and barriers to recognition under this framework.
narrative_ontology:constraint_stakeholder(sex_gender_category__biology_reading, transgender_individuals, payer,
    powerless, biographical, trapped, global).

% Are forced into binary sex categories that often do not accurately reflect their biological reality, frequently leading to non-consensual medical interventions and psychological distress. Their existence challenges the core premise of the reading.
narrative_ontology:constraint_stakeholder(sex_gender_category__biology_reading, intersex_individuals, payer,
    powerless, biographical, trapped, global).

% Face social pressure, discrimination, and lack of recognition for not conforming to gender norms derived from the strict biological binary. While not always denied legal sex, their social identity is often challenged.
narrative_ontology:constraint_stakeholder(sex_gender_category__biology_reading, gender_non_conforming_individuals, payer,
    moderate, biographical, constrained, global).

% Advocate for sex-based rights and protections, often aligning with the biological definition of sex to define the category of 'woman' as distinct from 'gender identity'. They see this as essential for addressing systemic sexism.
narrative_ontology:constraint_stakeholder(sex_gender_category__biology_reading, some_feminist_groups, beneficiary,
    organized, generational, constrained, national).

% Actively challenge this reading, advocating for self-identification and inclusion of transgender individuals in their affirmed gender categories. They are often excluded from policy-making and definitional processes by proponents of this reading.
narrative_ontology:constraint_stakeholder(sex_gender_category__biology_reading, transgender_rights_advocates, excluded,
    organized, biographical, constrained, global).

% Observe and document biological variations, but also operate within legal and social frameworks that may enforce this binary reading. They are often caught between evolving scientific understanding and rigid social definitions.
narrative_ontology:constraint_stakeholder(sex_gender_category__biology_reading, medical_professionals, observer,
    institutional, biographical, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(sex_gender_category__biology_reading, diffuse).
narrative_ontology:fixing_cost_class(sex_gender_category__biology_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes clear, immutable categories for sex, which proponents argue is necessary for legal definitions, data collection, and the maintenance of sex-segregated spaces (e.g., sports, bathrooms, prisons) for safety and fairness.
% TRANSFER_FUNCTION: Transfers social and legal recognition, access to certain spaces/rights, and definitional power to those who fit the immutable biological criteria, and away from those who do not. It also transfers the burden of boundary enforcement to institutions and individuals.
% ABSENT_VOICES: Transgender rights advocates, intersex advocates, and gender studies scholars are often excluded from the definitional process, despite being directly impacted. Their perspectives on the fluidity and complexity of sex and gender are systematically marginalized.
% DISAPPEARANCE_RATIONALE: If the definition of sex based solely on immutable reproductive biology vanished overnight, legal systems, social norms, and identity categories would undergo fundamental redefinition. Sex-segregated spaces would need new criteria, data collection methods would change, and the social understanding of identity would shift dramatically.
% FOUNDING_PROBLEM: To provide a clear, objective, and immutable basis for distinguishing between sexes, historically for reproductive roles, legal status, and social organization, particularly in contexts where sex differences were deemed relevant (e.g., marriage, military service, property rights).
% FOUNDING_PROBLEM_CORROBORATION: Proponents (e.g., some biological essentialists, certain feminist groups) argue the problem is still live and essential for protecting sex-based rights and maintaining social order, citing biological sciences and historical legal precedent. Opponents (e.g., transgender rights organizations, intersex advocacy groups, some scientific bodies) dispute its continued relevance and accuracy as a sole definitional basis, arguing it causes harm and is overly simplistic given contemporary scientific understanding.
narrative_ontology:disappearance_verdict(sex_gender_category__biology_reading, world_rearranges).
narrative_ontology:founding_problem_status(sex_gender_category__biology_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(sex_gender_category__biology_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(sex_gender_category__biology_reading, 'none', 1).
narrative_ontology:epsilon_provenance(sex_gender_category__biology_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sex_gender_category__biology_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(sex_gender_category__biology_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(sex_gender_category__biology_reading, ExtMetricName, E),
    domain_priors:suppression_score(sex_gender_category__biology_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(sex_gender_category__biology_reading),
    narrative_ontology:constraint_metric(sex_gender_category__biology_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(sex_gender_category__biology_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(sex_gender_category__biology_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.65) is substantial because this reading denies fundamental identity and access to categories for transgender and intersex individuals, imposing significant social and legal costs. Suppression (0.75) is high due to active legal, medical, and social enforcement mechanisms that maintain the binary and exclude alternatives. Theater ratio (0.40) is moderate, reflecting the performative re-assertion of 'naturalness' in the face of scientific nuance and social change, alongside genuine belief. The high resistance (0.80) from affected groups and advocates indicates that this is not an unchangeable natural law, but a contested social construct.
 *
 * PERSPECTIVAL GAP:
 *   Proponents of this reading perceive it as a self-evident, natural truth (Mountain), with minimal extraction or suppression, necessary for social order. Those targeted by its exclusionary aspects experience it as a highly extractive and suppressive Snare or Tangled Rope, actively enforced to maintain a rigid social hierarchy. The engine's classification will likely reflect this divergence from the claimed type.
 *
 * DIRECTIONALITY LOGIC:
 *   Biological essentialists and some feminist groups are beneficiaries, gaining definitional power and perceived protection of sex-based rights. Transgender, intersex, and gender non-conforming individuals are targets, bearing the costs of exclusion, misgendering, and forced categorization. Cis women are both beneficiaries (for sex-segregated spaces) and payers (constrained by rigid definitions). Rival perspectives (transgender rights advocates) are actively excluded from the definitional process.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_social_construct,
    'Is the definition of sex based on immutable reproductive biology a genuine natural law (Mountain) or a socially constructed constraint that benefits identifiable agents?',
    'Analysis of historical and cross-cultural variations in sex/gender definitions, and the impact of scientific advancements on the understanding of biological sex beyond a simple binary. If definitions vary significantly and are enforced to benefit specific groups, it points to a construct.',
    'If resolved as a social construct, the constraint''s classification would shift from Mountain to a more extractive type (e.g., Snare or Tangled Rope), reflecting its active enforcement and identifiable victims.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(natural_law_vs_social_construct, conceptual, 'Ambiguity regarding the naturalness vs. constructedness of the sex definition.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression primarily structural (legal barriers, medical gatekeeping) or internalized (social pressure leading to self-censorship and identity concealment)?',
    'Post-exit suppression trajectory: if individuals continue to self-censor or conceal their identities even after legal/medical barriers are removed, it indicates a significant internalized component. Longitudinal studies on the psychological impact of non-recognition.',
    'If internalized suppression is a major factor, the constraint''s effective suppression is higher than the structural measure suggests, as the target carries the suppression with them after any formal ''exit'' or legal change.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for gender identity.').

omega_variable(
    biological_definition_ambiguity,
    'What specific biological criteria (chromosomes, gonads, internal/external anatomy, reproductive capacity) are considered immutable and definitive for sex categorization, and how do intersex variations challenge this immutability?',
    'Consensus among scientific bodies on a comprehensive and consistent definition of biological sex that accounts for all human variations, or a legal framework that explicitly addresses intersex conditions without forcing binary assignment.',
    'If biological definitions are found to be more complex or less immutable than claimed, the ''emerges_naturally'' claim would be undermined, further shifting the constraint away from a Mountain classification and highlighting its constructed nature.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(biological_definition_ambiguity, empirical, 'Ambiguity in the precise biological definition of sex and its immutability.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sex_gender_category__biology_reading, 2000, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sex__tr_t2000, sex_gender_category__biology_reading, theater_ratio, 2000, 0.3).
narrative_ontology:measurement(sex__tr_t2005, sex_gender_category__biology_reading, theater_ratio, 2005, 0.32).
narrative_ontology:measurement(sex__tr_t2010, sex_gender_category__biology_reading, theater_ratio, 2010, 0.35).
narrative_ontology:measurement(sex__tr_t2015, sex_gender_category__biology_reading, theater_ratio, 2015, 0.37).
narrative_ontology:measurement(sex__tr_t2020, sex_gender_category__biology_reading, theater_ratio, 2020, 0.39).
narrative_ontology:measurement(sex__tr_t2025, sex_gender_category__biology_reading, theater_ratio, 2025, 0.4).

% Extraction over time
narrative_ontology:measurement(sex__be_t2000, sex_gender_category__biology_reading, base_extractiveness, 2000, 0.55).
narrative_ontology:measurement(sex__be_t2005, sex_gender_category__biology_reading, base_extractiveness, 2005, 0.58).
narrative_ontology:measurement(sex__be_t2010, sex_gender_category__biology_reading, base_extractiveness, 2010, 0.6).
narrative_ontology:measurement(sex__be_t2015, sex_gender_category__biology_reading, base_extractiveness, 2015, 0.62).
narrative_ontology:measurement(sex__be_t2020, sex_gender_category__biology_reading, base_extractiveness, 2020, 0.64).
narrative_ontology:measurement(sex__be_t2025, sex_gender_category__biology_reading, base_extractiveness, 2025, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(sex__su_t2000, sex_gender_category__biology_reading, suppression_requirement, 2000, 0.65).
narrative_ontology:measurement(sex__su_t2005, sex_gender_category__biology_reading, suppression_requirement, 2005, 0.68).
narrative_ontology:measurement(sex__su_t2010, sex_gender_category__biology_reading, suppression_requirement, 2010, 0.7).
narrative_ontology:measurement(sex__su_t2015, sex_gender_category__biology_reading, suppression_requirement, 2015, 0.72).
narrative_ontology:measurement(sex__su_t2020, sex_gender_category__biology_reading, suppression_requirement, 2020, 0.74).
narrative_ontology:measurement(sex__su_t2025, sex_gender_category__biology_reading, suppression_requirement, 2025, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sex_gender_category__biology_reading, identity_coordination).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
