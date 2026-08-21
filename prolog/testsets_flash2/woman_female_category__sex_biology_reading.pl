% ============================================================================
% CONSTRAINT STORY: woman_female_category__sex_biology_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_woman_female_category__sex_biology_reading, []).

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
 *   constraint_id: woman_female_category__sex_biology_reading
 *   human_readable: Female Category Defined by Sex Biology
 *   domain: political_philosophy/bioethics/gender_studies/law
 *
 * SUMMARY:
 *   This constraint defines 'woman' or 'female' based on chromosomal sex
 *   (XX/XY), reproductive anatomy, and gamete production capacity. It is one
 *   reading of the broader 'woman_female_category' kernel. This reading
 *   emphasizes biological reality for categorization, particularly in
 *   contexts where sex differences are considered material (e.g., physical
 *   safety, sports fairness). It is claimed as a Tangled Rope because it
 *   genuinely coordinates for natal females (providing sex-based protections)
 *   but also extracts from transgender women (excluding them from female
 *   categories) and requires active enforcement to maintain these boundaries.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(woman_female_category__sex_biology_reading, 0.6).
domain_priors:suppression_score(woman_female_category__sex_biology_reading, 0.7).
domain_priors:theater_ratio(woman_female_category__sex_biology_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(woman_female_category__sex_biology_reading, extractiveness, 0.6).
narrative_ontology:constraint_metric(woman_female_category__sex_biology_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(woman_female_category__sex_biology_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(woman_female_category__sex_biology_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(woman_female_category__sex_biology_reading, resistance, 0.8).

% --- Constraint claim ---
narrative_ontology:constraint_claim(woman_female_category__sex_biology_reading, tangled_rope).
narrative_ontology:human_readable(woman_female_category__sex_biology_reading, "Female Category Defined by Sex Biology").
narrative_ontology:topic_domain(woman_female_category__sex_biology_reading, "political_philosophy/bioethics/gender_studies/law").

domain_priors:requires_active_enforcement(woman_female_category__sex_biology_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(woman_female_category__sex_biology_reading, '76e77d3a-b2d5-434b-8333-0ee9ca2e68ed').
narrative_ontology:cs_kernel_codification('76e77d3a-b2d5-434b-8333-0ee9ca2e68ed', formalized).
narrative_ontology:cs_authority_grounding('76e77d3a-b2d5-434b-8333-0ee9ca2e68ed', expertise).
narrative_ontology:cs_interpretation_layer_present('76e77d3a-b2d5-434b-8333-0ee9ca2e68ed').
narrative_ontology:cs_reading_relation('76e77d3a-b2d5-434b-8333-0ee9ca2e68ed', woman_female_category__gender_identity_reading, forecloses).
narrative_ontology:cs_reading_relation('76e77d3a-b2d5-434b-8333-0ee9ca2e68ed', woman_female_category__hybrid_contextual_reading, coexists_with).
narrative_ontology:cs_axiom('76e77d3a-b2d5-434b-8333-0ee9ca2e68ed', foundational, sex_is_binary_and_immutable).
narrative_ontology:cs_axiom_status(sex_is_binary_and_immutable, holdable).
narrative_ontology:cs_axiom_grounding('76e77d3a-b2d5-434b-8333-0ee9ca2e68ed', sex_is_binary_and_immutable, empirically_contingent).
narrative_ontology:cs_axiom('76e77d3a-b2d5-434b-8333-0ee9ca2e68ed', foundational, sex_based_rights_are_necessary).
narrative_ontology:cs_axiom_status(sex_based_rights_are_necessary, holdable).
narrative_ontology:cs_axiom_grounding('76e77d3a-b2d5-434b-8333-0ee9ca2e68ed', sex_based_rights_are_necessary, deontological).
narrative_ontology:cs_reference_frame('76e77d3a-b2d5-434b-8333-0ee9ca2e68ed', biological_sex_as_foundational_category).
narrative_ontology:cs_drift_state('76e77d3a-b2d5-434b-8333-0ee9ca2e68ed', contemporary_gender_discourse, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('76e77d3a-b2d5-434b-8333-0ee9ca2e68ed', '2024-07-30T12:00:00Z').
narrative_ontology:cs_kernel_id(woman_female_category__sex_biology_reading, woman_female_category).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(woman_female_category__sex_biology_reading, natal_females_seeking_sex_based_protections).
narrative_ontology:constraint_victim(woman_female_category__sex_biology_reading, transgender_women_seeking_female_category_inclusion).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Advocate for and benefit from policies that define 'woman' or 'female' based on biological sex, particularly in contexts like sports, prisons, and single-sex spaces, citing concerns about fairness and safety. They experience the constraint as protective.
narrative_ontology:constraint_stakeholder(woman_female_category__sex_biology_reading, natal_females_seeking_sex_based_protections, beneficiary,
    organized, generational, constrained, global).

% Are excluded from categories and spaces defined by biological sex, which they perceive as discriminatory and invalidating of their gender identity. They bear the cost of exclusion and lack of recognition in these contexts.
narrative_ontology:constraint_stakeholder(woman_female_category__sex_biology_reading, transgender_women_seeking_female_category_inclusion, payer,
    powerless, biographical, identity_locked, global).

% Are tasked with defining legal and policy categories for 'woman' or 'female'. They face pressure from various advocacy groups and navigate complex legal and ethical considerations, often leading to contested or inconsistent applications of the definition.
narrative_ontology:constraint_stakeholder(woman_female_category__sex_biology_reading, policy_makers_and_institutions, agenda_setter,
    institutional, generational, constrained, national).

% Provide scientific and medical expertise on sex differentiation, reproductive biology, and developmental processes. Their input is often cited by proponents of the sex-biology reading, but they typically do not set policy directly.
narrative_ontology:constraint_stakeholder(woman_female_category__sex_biology_reading, biological_scientists_and_medical_professionals, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a clear, universally applicable definition for 'female' based on observable biological characteristics, facilitating consistent categorization in contexts where sex-linked traits are deemed relevant (e.g., reproduction, physical dimorphism).
% TRANSFER_FUNCTION: Transfers exclusive access to certain categories and protections (e.g., female-only sports, single-sex spaces) to natal females, while transferring exclusion and lack of recognition in those contexts to transgender women.
% ABSENT_VOICES: Intersex individuals, whose biological sex characteristics may not fit neatly into a binary XX/XY framework, are often marginalized in this debate. Their experiences highlight the limitations of strictly binary biological definitions.
% DISAPPEARANCE_RATIONALE: If the definition of 'female' based on sex biology vanished overnight, it would lead to significant re-evaluation and re-categorization across numerous social, legal, and medical domains. Policies related to sex-segregated spaces, sports, and medical care would need fundamental restructuring, leading to widespread societal rearrangement.
% FOUNDING_PROBLEM: The need for clear, consistent categorization of individuals based on biological sex for purposes of reproduction, population statistics, and sex-linked social roles and protections.
% FOUNDING_PROBLEM_CORROBORATION: Biological and medical sciences consistently attest to the foundational role of sex biology in human reproduction and health. Advocacy groups for sex-based rights corroborate the ongoing need for sex-specific protections. While contested in some social contexts, the underlying biological reality remains a live problem for many domains.
narrative_ontology:disappearance_verdict(woman_female_category__sex_biology_reading, world_rearranges).
narrative_ontology:founding_problem_status(woman_female_category__sex_biology_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(woman_female_category__sex_biology_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(woman_female_category__sex_biology_reading, 'none', 1).
narrative_ontology:epsilon_provenance(woman_female_category__sex_biology_reading, 0.6, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(woman_female_category__sex_biology_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(woman_female_category__sex_biology_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(woman_female_category__sex_biology_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.6) arises from the exclusion of transgender women from categories and spaces they identify with, leading to social and psychological costs. Suppression (0.7) is high due to active enforcement of sex-based definitions in policy and law, often through legal challenges and institutional resistance to gender identity-based inclusion. Resistance (0.8) is also high, reflecting significant activism and legal challenges from transgender rights advocates. The theater ratio is low (0.1) as the constraint's function is largely direct and not performative; its purpose is to maintain a specific biological definition.
 *
 * PERSPECTIVAL GAP:
 *   Natal females who benefit from sex-based protections would experience this as a protective Rope, ensuring their safety and fairness in specific contexts. Transgender women, however, would experience it as a Snare, as it actively excludes them from categories they identify with, imposing significant social and psychological costs. Policy makers experience it as a Tangled Rope, balancing competing claims and facing enforcement challenges.
 *
 * DIRECTIONALITY LOGIC:
 *   Natal females seeking sex-based protections are beneficiaries (low d) as the constraint directly serves their interests in maintaining sex-segregated spaces and categories. Transgender women seeking inclusion are targets (high d) as the constraint directly excludes them. Policy makers are agenda-setters, navigating the enforcement and contestation of this definition.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification prevents mislabeling by recognizing both the genuine coordination function (for natal females) and the asymmetric extraction (from transgender women). It highlights that the constraint is not a pure Snare, as it serves a protective role for one group, but also not a pure Rope, due to the significant costs imposed on another. The ongoing contestation indicates the mandate is live but highly disputed, preventing a Piton classification.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    scope_of_sex_based_relevance,
    'In which specific contexts (e.g., sports, prisons, medical care, social spaces) is biological sex categorization genuinely relevant, and in which is it not?',
    'Empirical studies on sex-linked performance differences, safety outcomes, and social impacts in various contexts, combined with legal and ethical analysis of rights and harms.',
    'If biological sex is found to be irrelevant in certain contexts, the extractiveness and suppression of this reading would be re-evaluated as higher in those contexts, potentially shifting the classification towards a Snare. If found highly relevant, the coordination function would be strengthened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(scope_of_sex_based_relevance, empirical, 'Determines the legitimate scope of applying sex-biology definitions.').

omega_variable(
    identity_vs_biology_primacy,
    'Which framework (gender identity or biological sex) should take normative primacy in defining ''woman'' or ''female'' in a given context?',
    'This is a conceptual and preference-based question, resolvable through ongoing societal debate, legal precedent, and evolving ethical consensus, rather than purely empirical data.',
    'If gender identity gains normative primacy, this reading''s classification would shift towards a Snare due to its exclusionary nature. If biological sex retains primacy, its Tangled Rope classification would be reinforced, with its coordination function emphasized.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_vs_biology_primacy, conceptual, 'The fundamental conceptual conflict between gender identity and biological sex as definitional criteria.').

omega_variable(
    intersex_inclusion_ambiguity,
    'How does this sex-biology reading accommodate or exclude intersex individuals, and what are the implications for their categorization and rights?',
    'Detailed analysis of how intersex variations are handled within existing sex-biology frameworks, and the development of inclusive categorization models that move beyond strict binary definitions.',
    'If intersex individuals are systematically excluded or miscategorized, the extractiveness of this reading would be higher, and its claim to universal biological clarity would be weakened, potentially leading to a re-evaluation of its coordination function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(intersex_inclusion_ambiguity, empirical, 'The challenge of intersex variations to binary sex-biology definitions.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(woman_female_category__sex_biology_reading, 1950, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(woma_tr_t1950, woman_female_category__sex_biology_reading, theater_ratio, 1950, 0.05).
narrative_ontology:measurement(woma_tr_t1970, woman_female_category__sex_biology_reading, theater_ratio, 1970, 0.07).
narrative_ontology:measurement(woma_tr_t1990, woman_female_category__sex_biology_reading, theater_ratio, 1990, 0.09).
narrative_ontology:measurement(woma_tr_t2010, woman_female_category__sex_biology_reading, theater_ratio, 2010, 0.1).
narrative_ontology:measurement(woma_tr_t2024, woman_female_category__sex_biology_reading, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(woma_be_t1950, woman_female_category__sex_biology_reading, base_extractiveness, 1950, 0.5).
narrative_ontology:measurement(woma_be_t1970, woman_female_category__sex_biology_reading, base_extractiveness, 1970, 0.55).
narrative_ontology:measurement(woma_be_t1990, woman_female_category__sex_biology_reading, base_extractiveness, 1990, 0.58).
narrative_ontology:measurement(woma_be_t2010, woman_female_category__sex_biology_reading, base_extractiveness, 2010, 0.6).
narrative_ontology:measurement(woma_be_t2024, woman_female_category__sex_biology_reading, base_extractiveness, 2024, 0.6).

% Suppression requirement over time
narrative_ontology:measurement(woma_su_t1950, woman_female_category__sex_biology_reading, suppression_requirement, 1950, 0.6).
narrative_ontology:measurement(woma_su_t1970, woman_female_category__sex_biology_reading, suppression_requirement, 1970, 0.65).
narrative_ontology:measurement(woma_su_t1990, woman_female_category__sex_biology_reading, suppression_requirement, 1990, 0.68).
narrative_ontology:measurement(woma_su_t2010, woman_female_category__sex_biology_reading, suppression_requirement, 2010, 0.7).
narrative_ontology:measurement(woma_su_t2024, woman_female_category__sex_biology_reading, suppression_requirement, 2024, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(woman_female_category__sex_biology_reading, identity_coordination).
narrative_ontology:affects_constraint(woman_female_category__sex_biology_reading, gender_identity_reading).
narrative_ontology:affects_constraint(woman_female_category__sex_biology_reading, hybrid_contextual_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'woman_female_category' kernel. Its structural properties and metrics differ significantly from the 'gender_identity_reading' and 'hybrid_contextual_reading', necessitating separate constraint stories. This reading influences the others by setting a baseline for biological definitions.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
