% ============================================================================
% CONSTRAINT STORY: gendered_category_membership__biological_sex_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: gendered_category_membership__biological_sex_reading
 *   human_readable: Gendered Category Membership (Biological Sex Reading)
 *   domain: social_ontology/political_philosophy/bioethics
 *
 * SUMMARY:
 *   This constraint defines gendered category membership based on immutable
 *   biological markers (chromosomes, reproductive anatomy at birth). It is
 *   presented by its proponents as a natural and self-evident truth,
 *   essential for social order and the protection of biological females.
 *   However, its operation leads to the exclusion and suppression of trans
 *   women and non-binary individuals from categories aligned with their
 *   gender identity. The claimed type is 'mountain' reflecting the
 *   proponents' view of its naturalness, but the high extractiveness and
 *   suppression metrics reflect its actual impact on those it excludes.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gendered_category_membership__biological_sex_reading, 0.85).
domain_priors:suppression_score(gendered_category_membership__biological_sex_reading, 0.9).
domain_priors:theater_ratio(gendered_category_membership__biological_sex_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gendered_category_membership__biological_sex_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(gendered_category_membership__biological_sex_reading, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(gendered_category_membership__biological_sex_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gendered_category_membership__biological_sex_reading, accessibility_collapse, 0.95).
narrative_ontology:constraint_metric(gendered_category_membership__biological_sex_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gendered_category_membership__biological_sex_reading, mountain).
narrative_ontology:human_readable(gendered_category_membership__biological_sex_reading, "Gendered Category Membership (Biological Sex Reading)").
narrative_ontology:topic_domain(gendered_category_membership__biological_sex_reading, "social_ontology/political_philosophy/bioethics").

domain_priors:requires_active_enforcement(gendered_category_membership__biological_sex_reading).
domain_priors:emerges_naturally(gendered_category_membership__biological_sex_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gendered_category_membership__biological_sex_reading, '6a0106c9-7c30-43cd-843b-545a0ccda483').
narrative_ontology:cs_kernel_codification('6a0106c9-7c30-43cd-843b-545a0ccda483', formalized).
narrative_ontology:cs_authority_grounding('6a0106c9-7c30-43cd-843b-545a0ccda483', lineage).
narrative_ontology:cs_interpretation_layer_present('6a0106c9-7c30-43cd-843b-545a0ccda483').
narrative_ontology:cs_reading_relation('6a0106c9-7c30-43cd-843b-545a0ccda483', gendered_category_membership__gender_identity_reading, forecloses).
narrative_ontology:cs_reading_relation('6a0106c9-7c30-43cd-843b-545a0ccda483', gendered_category_membership__social_role_reading, coexists_with).
narrative_ontology:cs_axiom('6a0106c9-7c30-43cd-843b-545a0ccda483', foundational, sex_is_binary_and_immutable).
narrative_ontology:cs_axiom_status(sex_is_binary_and_immutable, holdable).
narrative_ontology:cs_axiom_grounding('6a0106c9-7c30-43cd-843b-545a0ccda483', sex_is_binary_and_immutable, empirically_contingent).
narrative_ontology:cs_axiom('6a0106c9-7c30-43cd-843b-545a0ccda483', foundational, sex_determines_gendered_categories).
narrative_ontology:cs_axiom_status(sex_determines_gendered_categories, holdable).
narrative_ontology:cs_axiom_grounding('6a0106c9-7c30-43cd-843b-545a0ccda483', sex_determines_gendered_categories, conventional).
narrative_ontology:cs_reference_frame('6a0106c9-7c30-43cd-843b-545a0ccda483', binary_biological_determinism).
narrative_ontology:cs_drift_state('6a0106c9-7c30-43cd-843b-545a0ccda483', contemporary_identity_discourse, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('6a0106c9-7c30-43cd-843b-545a0ccda483', '').
narrative_ontology:cs_kernel_id(gendered_category_membership__biological_sex_reading, gendered_category_membership).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gendered_category_membership__biological_sex_reading, biological_essentialists).
narrative_ontology:constraint_beneficiary(gendered_category_membership__biological_sex_reading, cis_women).
narrative_ontology:constraint_victim(gendered_category_membership__biological_sex_reading, trans_women).
narrative_ontology:constraint_victim(gendered_category_membership__biological_sex_reading, non_binary_individuals).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(gendered_category_membership__biological_sex_reading, cis_women).
narrative_ontology:constraint_victim(gendered_category_membership__biological_sex_reading, gender_identity_advocates).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Advocate for and enforce the definition of gendered categories based solely on immutable biological markers (chromosomes, reproductive anatomy at birth). They benefit from the perceived clarity, stability, and 'naturalness' of this framework, which aligns with their ideological commitments.
narrative_ontology:constraint_stakeholder(gendered_category_membership__biological_sex_reading, biological_essentialists, agenda_setter,
    institutional, civilizational, identity_locked, universal).

% Benefit from the preservation of sex-segregated spaces and categories, which they perceive as essential for safety, fairness, and identity. They may also bear social costs from defending these boundaries against challenges, and some may feel their category is diluted by alternative definitions.
narrative_ontology:constraint_stakeholder(gendered_category_membership__biological_sex_reading, cis_women, beneficiary,
    powerful, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(gendered_category_membership__biological_sex_reading, cis_women, payer).

% Are systematically excluded from categories they identify with and from sex-segregated spaces aligned with their gender identity. They face significant social, legal, and institutional barriers, experiencing high identity suppression and extraction.
narrative_ontology:constraint_stakeholder(gendered_category_membership__biological_sex_reading, trans_women, payer,
    powerless, biographical, trapped, global).

% Their identities are rendered invisible or illegitimate by a strict binary biological definition of gender. They experience exclusion and invalidation, with their self-identification often denied by the constraint's framework.
narrative_ontology:constraint_stakeholder(gendered_category_membership__biological_sex_reading, non_binary_individuals, payer,
    powerless, biographical, trapped, global).

% Actively resist this constraint, advocating for category membership based on subjective gender identity. They bear the costs of activism, legal challenges, and social backlash in their efforts to dismantle the biological essentialist framework.
narrative_ontology:constraint_stakeholder(gendered_category_membership__biological_sex_reading, gender_identity_advocates, payer,
    organized, generational, constrained, global).

% Analyze the social construction of gender and its relationship to biological sex, often challenging purely biological definitions by highlighting the role of social performance and recognition. They seek to understand, rather than directly enforce or resist, the constraint.
narrative_ontology:constraint_stakeholder(gendered_category_membership__biological_sex_reading, social_role_theorists, observer,
    analytical, generational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a clear, unambiguous, and historically established basis for defining sex-segregated spaces, social roles, and legal categories, simplifying social organization based on perceived biological reality.
% TRANSFER_FUNCTION: Transfers social and institutional recognition, access to specific spaces and resources, and definitional power from individuals whose gender identity does not align with their birth-assigned sex to those whose biological markers align with traditional binary categories.
% ABSENT_VOICES: Intersex individuals, whose biological reality challenges the strict binary, are often marginalized or ignored in this framework. Children, whose gender identity may not yet align with birth sex, are also excluded from self-determination within this rigid definition.
% DISAPPEARANCE_RATIONALE: If category membership grounded in immutable biological markers ceased to be the primary determinant of gendered categories, social structures, legal definitions, and identity frameworks would undergo significant re-evaluation and reorganization. Sex-segregated spaces would need new justifications, and the concept of 'gender' itself would be fundamentally re-defined.
% FOUNDING_PROBLEM: To establish clear, universally recognizable categories for human reproduction, social roles, and the protection of biological females, based on observable and immutable biological differences.
% FOUNDING_PROBLEM_CORROBORATION: Proponents (biological essentialists) assert the problem of clear, stable categories is still live and fundamental. Opponents (gender identity advocates, social role theorists) attest that the founding problem has shifted or was always misframed, and the constraint now primarily serves to suppress identity and enforce a rigid social order, rather than solve a genuine coordination problem. Independent sociological and bioethical analyses from outside the benefiting parties support the shifted-function reading.
narrative_ontology:disappearance_verdict(gendered_category_membership__biological_sex_reading, world_rearranges).
narrative_ontology:founding_problem_status(gendered_category_membership__biological_sex_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gendered_category_membership__biological_sex_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(gendered_category_membership__biological_sex_reading, 'none', 1).
narrative_ontology:epsilon_provenance(gendered_category_membership__biological_sex_reading, 0.85, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gendered_category_membership__biological_sex_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(gendered_category_membership__biological_sex_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(gendered_category_membership__biological_sex_reading, ExtMetricName, E),
    domain_priors:suppression_score(gendered_category_membership__biological_sex_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(gendered_category_membership__biological_sex_reading),
    narrative_ontology:constraint_metric(gendered_category_membership__biological_sex_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(gendered_category_membership__biological_sex_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(gendered_category_membership__biological_sex_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.85) is high because the constraint denies fundamental identity and access to categories for a significant population. Suppression (0.90) is severe, as it relies on active enforcement of biological definitions in legal, medical, and social contexts, with few viable alternatives for those who do not conform. The theater ratio is low (0.10) because the constraint's function is direct and its enforcement is largely literal, not performative. Resistance is high (0.70) due to ongoing social and political challenges from gender identity advocates. The increasing extractiveness and suppression over the interval reflect the hardening of positions and intensification of enforcement as contestation has grown.
 *
 * PERSPECTIVAL GAP:
 *   Proponents of this constraint (biological essentialists, many cis women) perceive it as a natural, unchangeable 'mountain' that provides necessary social coordination and protection. Those targeted by the constraint (trans women, non-binary individuals, gender identity advocates) experience it as a 'snare' or 'tangled rope' that extracts identity, access, and legitimacy through coercive enforcement. The engine's classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Biological essentialists are clear beneficiaries and agenda-setters, as the constraint validates their worldview and grants them definitional power (low d). Cis women are also beneficiaries of category preservation, though some may experience costs (moderate d). Trans women and non-binary individuals are clear targets, experiencing high extraction and suppression (high d). Gender identity advocates are also targets, bearing the costs of resistance (high d).
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_social_construct,
    'Is the grounding of gendered category membership in immutable biological markers a genuine natural law (Mountain) or a socially constructed constraint that benefits identifiable agents (Snare/Tangled Rope)?',
    'Analysis of historical and cross-cultural variations in gender definitions, and the extent to which enforcement mechanisms are required to maintain the ''natural'' boundary.',
    'If primarily a social construct, the constraint''s classification would shift from Mountain to a more extractive type (e.g., Snare or Tangled Rope), highlighting its coercive aspects and the beneficiaries of its enforcement.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(natural_law_vs_social_construct, conceptual, 'Ambiguity between natural law and social construct for gendered categories.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression of non-conforming identities structural (external barriers) or internalized (cognitive patterns that persist after barrier removal)?',
    'Post-exit suppression trajectory: if suppression persists for individuals after moving to contexts where the biological definition is not enforced, it suggests internalized suppression. Longitudinal studies of identity formation in different social contexts.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests, as individuals carry the suppression with them. This would amplify the effective extraction for targets.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for gender identity.').

omega_variable(
    legitimacy_of_exclusion,
    'To what extent is the exclusion of trans women from ''woman'' categories and spaces a legitimate consequence of biological difference, versus an act of discrimination?',
    'Ethical and legal adjudication of competing rights claims, considering principles of bodily autonomy, non-discrimination, and the purpose of sex-segregated spaces. This is a preference-based question.',
    'If deemed discriminatory, the constraint''s legitimacy would be severely undermined, leading to calls for its dismantling or redefinition. If deemed legitimate, the constraint''s persistence would be reinforced, albeit with continued contestation.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(legitimacy_of_exclusion, preference, 'Ethical legitimacy of exclusion based on biological markers.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gendered_category_membership__biological_sex_reading, 2000, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gend_tr_t2000, gendered_category_membership__biological_sex_reading, theater_ratio, 2000, 0.12).
narrative_ontology:measurement(gend_tr_t2005, gendered_category_membership__biological_sex_reading, theater_ratio, 2005, 0.11).
narrative_ontology:measurement(gend_tr_t2010, gendered_category_membership__biological_sex_reading, theater_ratio, 2010, 0.1).
narrative_ontology:measurement(gend_tr_t2015, gendered_category_membership__biological_sex_reading, theater_ratio, 2015, 0.1).
narrative_ontology:measurement(gend_tr_t2020, gendered_category_membership__biological_sex_reading, theater_ratio, 2020, 0.1).
narrative_ontology:measurement(gend_tr_t2025, gendered_category_membership__biological_sex_reading, theater_ratio, 2025, 0.1).

% Extraction over time
narrative_ontology:measurement(gend_be_t2000, gendered_category_membership__biological_sex_reading, base_extractiveness, 2000, 0.7).
narrative_ontology:measurement(gend_be_t2005, gendered_category_membership__biological_sex_reading, base_extractiveness, 2005, 0.75).
narrative_ontology:measurement(gend_be_t2010, gendered_category_membership__biological_sex_reading, base_extractiveness, 2010, 0.8).
narrative_ontology:measurement(gend_be_t2015, gendered_category_membership__biological_sex_reading, base_extractiveness, 2015, 0.83).
narrative_ontology:measurement(gend_be_t2020, gendered_category_membership__biological_sex_reading, base_extractiveness, 2020, 0.84).
narrative_ontology:measurement(gend_be_t2025, gendered_category_membership__biological_sex_reading, base_extractiveness, 2025, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(gend_su_t2000, gendered_category_membership__biological_sex_reading, suppression_requirement, 2000, 0.75).
narrative_ontology:measurement(gend_su_t2005, gendered_category_membership__biological_sex_reading, suppression_requirement, 2005, 0.8).
narrative_ontology:measurement(gend_su_t2010, gendered_category_membership__biological_sex_reading, suppression_requirement, 2010, 0.85).
narrative_ontology:measurement(gend_su_t2015, gendered_category_membership__biological_sex_reading, suppression_requirement, 2015, 0.88).
narrative_ontology:measurement(gend_su_t2020, gendered_category_membership__biological_sex_reading, suppression_requirement, 2020, 0.89).
narrative_ontology:measurement(gend_su_t2025, gendered_category_membership__biological_sex_reading, suppression_requirement, 2025, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
