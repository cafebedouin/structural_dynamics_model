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
 *   constraint_id: sex_gender_category__biology_reading
 *   human_readable: Sex Category Membership (Biology Reading)
 *   domain: social_ontology/identity_politics/legal_classification
 *
 * SUMMARY:
 *   This constraint defines sex category membership based solely on immutable
 *   reproductive biology (chromosomes, anatomy at birth). It is one reading
 *   of the broader 'sex_gender_category' kernel. This reading structurally
 *   excludes trans women from the category 'woman' and often forces intersex
 *   individuals into a binary, while providing a clear, immutable category
 *   for cis women and those who advocate for sex-based rights. The metrics
 *   reflect the high costs of enforcing these boundaries and the extraction
 *   from those excluded.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sex_gender_category__biology_reading, 0.65).
domain_priors:suppression_score(sex_gender_category__biology_reading, 0.78).
domain_priors:theater_ratio(sex_gender_category__biology_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sex_gender_category__biology_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(sex_gender_category__biology_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(sex_gender_category__biology_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(sex_gender_category__biology_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(sex_gender_category__biology_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sex_gender_category__biology_reading, tangled_rope).
narrative_ontology:human_readable(sex_gender_category__biology_reading, "Sex Category Membership (Biology Reading)").
narrative_ontology:topic_domain(sex_gender_category__biology_reading, "social_ontology/identity_politics/legal_classification").

domain_priors:requires_active_enforcement(sex_gender_category__biology_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(sex_gender_category__biology_reading, '9f61da5a-34ba-4a6e-a005-7ea31f1aa9b3').
narrative_ontology:cs_kernel_codification('9f61da5a-34ba-4a6e-a005-7ea31f1aa9b3', formalized).
narrative_ontology:cs_authority_grounding('9f61da5a-34ba-4a6e-a005-7ea31f1aa9b3', lineage).
narrative_ontology:cs_interpretation_layer_present('9f61da5a-34ba-4a6e-a005-7ea31f1aa9b3').
narrative_ontology:cs_reading_relation('9f61da5a-34ba-4a6e-a005-7ea31f1aa9b3', sex_gender_category__identity_reading, forecloses).
narrative_ontology:cs_reading_relation('9f61da5a-34ba-4a6e-a005-7ea31f1aa9b3', sex_gender_category__hybrid_reading, influences).
narrative_ontology:cs_axiom('9f61da5a-34ba-4a6e-a005-7ea31f1aa9b3', foundational, sex_is_binary_and_immutable).
narrative_ontology:cs_axiom_status(sex_is_binary_and_immutable, holdable).
narrative_ontology:cs_axiom_grounding('9f61da5a-34ba-4a6e-a005-7ea31f1aa9b3', sex_is_binary_and_immutable, empirically_contingent).
narrative_ontology:cs_axiom('9f61da5a-34ba-4a6e-a005-7ea31f1aa9b3', foundational, sex_is_primary_for_social_classification).
narrative_ontology:cs_axiom_status(sex_is_primary_for_social_classification, holdable).
narrative_ontology:cs_axiom_grounding('9f61da5a-34ba-4a6e-a005-7ea31f1aa9b3', sex_is_primary_for_social_classification, conventional).
narrative_ontology:cs_reference_frame('9f61da5a-34ba-4a6e-a005-7ea31f1aa9b3', traditional_biological_binary).
narrative_ontology:cs_drift_state('9f61da5a-34ba-4a6e-a005-7ea31f1aa9b3', contemporary_identity_politics_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('9f61da5a-34ba-4a6e-a005-7ea31f1aa9b3', '').
narrative_ontology:cs_kernel_id(sex_gender_category__biology_reading, sex_gender_category).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sex_gender_category__biology_reading, cis_women_advocates).
narrative_ontology:constraint_beneficiary(sex_gender_category__biology_reading, biological_sex_researchers).
narrative_ontology:constraint_victim(sex_gender_category__biology_reading, trans_women).
narrative_ontology:constraint_victim(sex_gender_category__biology_reading, intersex_individuals).
narrative_ontology:constraint_victim(sex_gender_category__biology_reading, gender_non_conforming_individuals).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Advocate for sex-based rights and spaces, defining 'woman' exclusively by reproductive biology. They benefit from clear, immutable categories for policy and resource allocation, but bear costs of defending these boundaries against other readings.
narrative_ontology:constraint_stakeholder(sex_gender_category__biology_reading, cis_women_advocates, beneficiary,
    organized, generational, constrained, national).

% Their work on sex differences and reproductive biology is directly validated by this reading. They benefit from the clarity and immutability of biological sex as a primary classification, but face pressure from other readings to integrate social and psychological factors.
narrative_ontology:constraint_stakeholder(sex_gender_category__biology_reading, biological_sex_researchers, beneficiary,
    institutional, civilizational, analytical, global).

% Are excluded from categories aligned with their gender identity, facing legal and social barriers to accessing women's spaces and services. They bear significant social and psychological costs, with no easy exit from their identity or the societal structures that enforce this reading.
narrative_ontology:constraint_stakeholder(sex_gender_category__biology_reading, trans_women, payer,
    powerless, biographical, identity_locked, local).

% Are often forced into binary sex categories that do not reflect their biological reality, leading to medical interventions and social marginalization. They bear the cost of having their complex biology simplified and suppressed by this binary framework.
narrative_ontology:constraint_stakeholder(sex_gender_category__biology_reading, intersex_individuals, payer,
    powerless, biographical, trapped, local).

% While not necessarily seeking to change their legal sex, they experience social pressure and misunderstanding when their gender expression does not align with rigid biological sex expectations. They bear the cost of a less flexible social environment.
narrative_ontology:constraint_stakeholder(sex_gender_category__biology_reading, gender_non_conforming_individuals, payer,
    moderate, biographical, constrained, local).

% Are tasked with codifying and enforcing sex-based classifications in law, often defaulting to a biological reading for birth certificates, sports, and single-sex spaces. They bear the administrative and political costs of adjudicating disputes arising from this reading.
narrative_ontology:constraint_stakeholder(sex_gender_category__biology_reading, legal_systems, agenda_setter,
    institutional, generational, constrained, national).

% Observe and critique the impact of this reading on marginalized groups, particularly trans and intersex individuals, advocating for more inclusive legal and social frameworks. They analyze the constraint's operation and its human rights implications.
narrative_ontology:constraint_stakeholder(sex_gender_category__biology_reading, human_rights_advocates, observer,
    organized, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a clear, immutable, and universally applicable basis for sex classification, simplifying legal, social, and scientific categorization related to reproduction and sex-linked characteristics.
% TRANSFER_FUNCTION: Transfers social and legal recognition, access to sex-specific resources, and the definition of 'woman' from trans women and intersex individuals to cis women, based on a biological definition of sex.
% ABSENT_VOICES: The lived experiences and perspectives of trans men, non-binary individuals, and those with diverse gender identities are largely absent from the foundational premises of this reading; they would challenge the immutability and exclusivity of binary biological sex categories.
% DISAPPEARANCE_RATIONALE: If this biological reading of sex categories vanished overnight, legal systems would need to rapidly redefine sex and gender for all purposes (birth certificates, sports, prisons, healthcare), leading to significant social and legal upheaval as new, more fluid or identity-based systems emerged. The concept of 'woman' as a distinct biological class would lose its legal and social primacy.
% FOUNDING_PROBLEM: To establish a clear, objective, and immutable basis for distinguishing between two reproductive classes (male and female) for purposes of reproduction, social roles, and legal rights.
% FOUNDING_PROBLEM_CORROBORATION: Advocates for this reading assert the problem is live, citing the ongoing need for sex-segregated data, spaces, and protections. Human rights advocates and gender studies scholars attest that while biological sex is real, its exclusive use for social and legal categories is a constructed problem, not a natural one, and that the founding problem has been superseded by evolving understandings of gender and identity.
narrative_ontology:disappearance_verdict(sex_gender_category__biology_reading, world_rearranges).
narrative_ontology:founding_problem_status(sex_gender_category__biology_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(sex_gender_category__biology_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(sex_gender_category__biology_reading, 'none', 1).
narrative_ontology:epsilon_provenance(sex_gender_category__biology_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sex_gender_category__biology_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(sex_gender_category__biology_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(sex_gender_category__biology_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.65) is substantial due to the social, legal, and psychological costs imposed on trans and intersex individuals by their exclusion or miscategorization. Suppression (0.78) is high because this reading requires active enforcement of boundaries in legal systems, public discourse, and social spaces, often suppressing alternative understandings of sex and gender. Theater ratio (0.20) is low as the enforcement is largely functional to maintaining the claimed biological distinction, though some performative aspects exist in public debates. Resistance (0.75) is high due to active advocacy from trans rights groups and intersex advocates.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of cis women advocates, this constraint is a necessary 'rope' for coordination around sex-based rights. From the perspective of trans women and intersex individuals, it operates as a 'snare' that extracts recognition and access. The engine's classification will reflect this divergence based on the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Cis women advocates and biological sex researchers are beneficiaries (d near 0.0) as this reading validates their claims and research. Trans women, intersex individuals, and gender non-conforming individuals are targets (d near 1.0) as they bear the direct costs of exclusion and miscategorization. Legal systems act as agenda-setters, enforcing the categories, while human rights advocates observe and critique.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    biological_immutability_vs_intersex_variation,
    'How does the concept of ''immutable reproductive biology'' account for the biological diversity of intersex individuals, and does it force a binary where one does not naturally exist?',
    'Expanded medical and social recognition of intersex variations, leading to legal frameworks that accommodate non-binary biological sex markers.',
    'If intersex variations are fully recognized, the ''immutable binary'' axiom would be challenged, potentially reducing suppression and extractiveness for intersex individuals, and shifting the constraint towards a more nuanced biological understanding or a hybrid model.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(biological_immutability_vs_intersex_variation, empirical, 'Ambiguity in applying a strict biological binary to intersex individuals.').

omega_variable(
    sex_vs_gender_distinction_clarity,
    'Is the distinction between ''sex'' (biological) and ''gender'' (social/identity) sufficiently clear and consistently applied within this reading, or does the biological reading of sex implicitly conflate the two?',
    'Conceptual clarification and consistent terminological usage across legal, social, and scientific discourse, explicitly separating biological sex from social gender roles and identity.',
    'If the distinction is clarified and maintained, the constraint''s scope might narrow to purely biological contexts, reducing its suppressive impact on gender identity. If conflation persists, the constraint''s extractiveness and suppression remain high as it implicitly governs both biological and social aspects of identity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sex_vs_gender_distinction_clarity, conceptual, 'Ambiguity in the conceptual boundary between biological sex and social gender.').

omega_variable(
    enforcement_cost_sustainability,
    'Given rising resistance, are the costs of actively enforcing this biological reading of sex categories sustainable for legal systems and social institutions in the long term?',
    'Longitudinal analysis of legal challenges, administrative burdens, and social unrest related to sex classification, alongside public opinion shifts.',
    'If enforcement costs become prohibitive, legal systems may seek alternative, less contentious classification methods (e.g., identity-based or hybrid models), leading to a reduction in suppression and extractiveness for targets. If costs are deemed sustainable, the current enforcement trajectory continues.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(enforcement_cost_sustainability, empirical, 'Sustainability of enforcement costs for the biological sex category.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sex_gender_category__biology_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sex__tr_t0, sex_gender_category__biology_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(sex__tr_t6, sex_gender_category__biology_reading, theater_ratio, 6, 0.17).
narrative_ontology:measurement(sex__tr_t12, sex_gender_category__biology_reading, theater_ratio, 12, 0.18).
narrative_ontology:measurement(sex__tr_t18, sex_gender_category__biology_reading, theater_ratio, 18, 0.19).
narrative_ontology:measurement(sex__tr_t24, sex_gender_category__biology_reading, theater_ratio, 24, 0.2).
narrative_ontology:measurement(sex__tr_t30, sex_gender_category__biology_reading, theater_ratio, 30, 0.2).

% Extraction over time
narrative_ontology:measurement(sex__be_t0, sex_gender_category__biology_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(sex__be_t6, sex_gender_category__biology_reading, base_extractiveness, 6, 0.58).
narrative_ontology:measurement(sex__be_t12, sex_gender_category__biology_reading, base_extractiveness, 12, 0.61).
narrative_ontology:measurement(sex__be_t18, sex_gender_category__biology_reading, base_extractiveness, 18, 0.63).
narrative_ontology:measurement(sex__be_t24, sex_gender_category__biology_reading, base_extractiveness, 24, 0.64).
narrative_ontology:measurement(sex__be_t30, sex_gender_category__biology_reading, base_extractiveness, 30, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(sex__su_t0, sex_gender_category__biology_reading, suppression_requirement, 0, 0.65).
narrative_ontology:measurement(sex__su_t6, sex_gender_category__biology_reading, suppression_requirement, 6, 0.7).
narrative_ontology:measurement(sex__su_t12, sex_gender_category__biology_reading, suppression_requirement, 12, 0.74).
narrative_ontology:measurement(sex__su_t18, sex_gender_category__biology_reading, suppression_requirement, 18, 0.76).
narrative_ontology:measurement(sex__su_t24, sex_gender_category__biology_reading, suppression_requirement, 24, 0.77).
narrative_ontology:measurement(sex__su_t30, sex_gender_category__biology_reading, suppression_requirement, 30, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sex_gender_category__biology_reading, identity_coordination).
narrative_ontology:affects_constraint(sex_gender_category__biology_reading, sex_gender_category__identity_reading).
narrative_ontology:affects_constraint(sex_gender_category__biology_reading, sex_gender_category__hybrid_reading).

% DUAL FORMULATION NOTE:
% This constraint is the 'biology_reading' of the 'sex_gender_category' kernel. It defines sex category membership by immutable reproductive biology. It is linked to the 'identity_reading' and 'hybrid_reading' as part of a constraint family where different interpretations of the same kernel lead to distinct constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
