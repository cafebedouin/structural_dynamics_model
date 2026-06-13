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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: sex_gender_category__biology_reading
 *   human_readable: Sex Category by Reproductive Biology
 *   domain: social_ontology/identity_politics/legal_classification
 *
 * SUMMARY:
 *   This constraint defines sex categories based strictly on reproductive
 *   biology (chromosomes, anatomy at birth). It is one reading of a contested
 *   kernel, 'sex_gender_category'. This reading excludes transgender women
 *   from the 'woman' category, positions cis women as the sole victim set for
 *   sex-based harms, incurs high boundary enforcement costs, and often forces
 *   intersex individuals into a binary framework. The constraint is claimed
 *   as a 'rope' by its proponents, emphasizing its coordination function for
 *   biological realities, but its metrics reflect significant extraction and
 *   suppression, particularly for transgender and intersex individuals,
 *   leading to a computed 'tangled_rope' classification.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sex_gender_category__biology_reading, 0.65).
domain_priors:suppression_score(sex_gender_category__biology_reading, 0.7).
domain_priors:theater_ratio(sex_gender_category__biology_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sex_gender_category__biology_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(sex_gender_category__biology_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(sex_gender_category__biology_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(sex_gender_category__biology_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(sex_gender_category__biology_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sex_gender_category__biology_reading, tangled_rope).
narrative_ontology:human_readable(sex_gender_category__biology_reading, "Sex Category by Reproductive Biology").
narrative_ontology:topic_domain(sex_gender_category__biology_reading, "social_ontology/identity_politics/legal_classification").

domain_priors:requires_active_enforcement(sex_gender_category__biology_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(sex_gender_category__biology_reading, 'db1e5b26-bbe9-4da1-ad81-e19d39c49c2a').
narrative_ontology:cs_kernel_codification('db1e5b26-bbe9-4da1-ad81-e19d39c49c2a', implicit).
narrative_ontology:cs_authority_grounding('db1e5b26-bbe9-4da1-ad81-e19d39c49c2a', lineage).
narrative_ontology:cs_interpretation_layer_present('db1e5b26-bbe9-4da1-ad81-e19d39c49c2a').
narrative_ontology:cs_reading_relation('db1e5b26-bbe9-4da1-ad81-e19d39c49c2a', sex_gender_category__identity_reading, forecloses).
narrative_ontology:cs_reading_relation('db1e5b26-bbe9-4da1-ad81-e19d39c49c2a', sex_gender_category__hybrid_reading, forecloses).
narrative_ontology:cs_axiom('db1e5b26-bbe9-4da1-ad81-e19d39c49c2a', foundational, sex_is_binary_and_immutable).
narrative_ontology:cs_axiom_status(sex_is_binary_and_immutable, holdable).
narrative_ontology:cs_axiom_grounding('db1e5b26-bbe9-4da1-ad81-e19d39c49c2a', sex_is_binary_and_immutable, empirically_contingent).
narrative_ontology:cs_axiom('db1e5b26-bbe9-4da1-ad81-e19d39c49c2a', foundational, sex_is_assigned_at_birth_by_anatomy).
narrative_ontology:cs_axiom_status(sex_is_assigned_at_birth_by_anatomy, holdable).
narrative_ontology:cs_axiom_grounding('db1e5b26-bbe9-4da1-ad81-e19d39c49c2a', sex_is_assigned_at_birth_by_anatomy, empirically_contingent).
narrative_ontology:cs_reference_frame('db1e5b26-bbe9-4da1-ad81-e19d39c49c2a', traditional_biological_dimorphism).
narrative_ontology:cs_drift_state('db1e5b26-bbe9-4da1-ad81-e19d39c49c2a', contemporary_gender_theory_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('db1e5b26-bbe9-4da1-ad81-e19d39c49c2a', '').
narrative_ontology:cs_kernel_id(sex_gender_category__biology_reading, sex_gender_category).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sex_gender_category__biology_reading, cis_women_advocates).
narrative_ontology:constraint_beneficiary(sex_gender_category__biology_reading, biological_sex_researchers).
narrative_ontology:constraint_victim(sex_gender_category__biology_reading, transgender_women).
narrative_ontology:constraint_victim(sex_gender_category__biology_reading, intersex_individuals).
narrative_ontology:constraint_victim(sex_gender_category__biology_reading, gender_non_conforming_individuals).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefit from the clarity and perceived protection of sex-based categories defined by reproductive biology, arguing it is essential for addressing sex-specific harms and maintaining women's spaces. They actively enforce this definition in policy and discourse.
narrative_ontology:constraint_stakeholder(sex_gender_category__biology_reading, cis_women_advocates, beneficiary,
    organized, generational, constrained, national).

% Are excluded from categories they identify with, leading to social, legal, and physical marginalization. They bear the cost of non-recognition and are often denied access to spaces and protections designated for women under this definition. Their identity is deeply tied to their self-identification, making 'exit' from their identity impossible.
narrative_ontology:constraint_stakeholder(sex_gender_category__biology_reading, transgender_women, payer,
    powerless, biographical, identity_locked, global).

% Are often forced into binary sex categories that do not accurately reflect their biological reality, leading to medical interventions and social pressure. They bear the cost of having their complex biology simplified and often erased by this rigid definition.
narrative_ontology:constraint_stakeholder(sex_gender_category__biology_reading, intersex_individuals, payer,
    powerless, biographical, trapped, global).

% Benefit from a clear, biologically defined framework for sex, which aligns with traditional scientific methodologies and research paradigms. Their work is often cited as foundational for this reading of sex categories.
narrative_ontology:constraint_stakeholder(sex_gender_category__biology_reading, biological_sex_researchers, beneficiary,
    institutional, generational, mobile, global).

% Are tasked with codifying and enforcing sex-based classifications in law, often defaulting to or being pressured to adopt definitions based on reproductive biology for birth certificates, sports, and other areas. They bear the cost of legal challenges and social unrest when these definitions are contested.
narrative_ontology:constraint_stakeholder(sex_gender_category__biology_reading, legal_systems, agenda_setter,
    institutional, civilizational, constrained, national).

% While not necessarily transgender, they experience social pressure and exclusion when their presentation or identity does not align with rigid, biologically-defined sex roles. They pay a social cost for non-conformity within this framework.
narrative_ontology:constraint_stakeholder(sex_gender_category__biology_reading, gender_non_conforming_individuals, payer,
    moderate, biographical, constrained, local).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a clear, binary, and ostensibly immutable basis for legal and social classification, aiming to coordinate social understanding and legal protections around reproductive biology.
% TRANSFER_FUNCTION: Transfers social and legal recognition, access to sex-segregated spaces, and specific protections to individuals whose sex aligns with their reproductive biology at birth, while denying these to others.
% ABSENT_VOICES: Advocates for intersex rights and non-binary recognition are often marginalized or excluded from policy discussions that solidify binary, biology-only sex definitions. They would argue for more inclusive and nuanced understandings of sex and gender.
% DISAPPEARANCE_RATIONALE: If sex categories based solely on reproductive biology vanished overnight, legal systems would need to redefine numerous laws (e.g., marriage, anti-discrimination, sports), social norms around gender would undergo significant upheaval, and the concept of sex-segregated spaces would be fundamentally challenged. The world would rearrange to accommodate more fluid or identity-based understandings.
% FOUNDING_PROBLEM: The need for clear, unambiguous categories to organize human populations for legal, social, and reproductive purposes, historically rooted in observed biological dimorphism.
% FOUNDING_PROBLEM_CORROBORATION: Advocates for this reading (cis_women_advocates, biological_sex_researchers) assert the problem is live and essential for women's rights and scientific clarity. Opposing groups (transgender_women, intersex_individuals) argue that while categorization is necessary, this specific biological definition creates more problems than it solves, and that the 'founding problem' has been reframed to exclude certain populations, as evidenced by human rights reports and medical consensus on intersex care from outside the benefiting parties.
narrative_ontology:disappearance_verdict(sex_gender_category__biology_reading, world_rearranges).
narrative_ontology:founding_problem_status(sex_gender_category__biology_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(sex_gender_category__biology_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(sex_gender_category__biology_reading, 'none', 1).

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
 *   The extractiveness (0.65) is driven by the social and legal costs imposed on those excluded or miscategorized by this definition. Suppression (0.7) is high due to active enforcement in legal systems, public discourse, and social institutions to maintain this rigid boundary. The theater ratio (0.2) is relatively low, as the biological basis is genuinely invoked, but some performativity exists in ignoring biological complexities (e.g., intersex variations) to maintain a strict binary. Resistance (0.75) is high, reflecting the intense social and political contestation this reading faces.
 *
 * PERSPECTIVAL GAP:
 *   Proponents (cis_women_advocates, biological_sex_researchers) perceive this as a necessary, natural, and protective 'rope' for women's rights and scientific clarity. Those excluded (transgender_women, intersex_individuals) experience it as a 'snare' or 'tangled_rope' that extracts recognition, rights, and safety, while enforcing a narrow, often inaccurate, definition of their existence. Legal systems (agenda_setter) navigate this tension, often defaulting to this reading due to historical precedent and political pressure.
 *
 * DIRECTIONALITY LOGIC:
 *   Cis women advocates and biological sex researchers are beneficiaries (d near 0.0) as this reading aligns with their interests and research. Transgender women, intersex individuals, and gender non-conforming individuals are targets (d near 1.0) as they bear the direct costs of exclusion and miscategorization. Legal systems act as agenda-setters, enforcing the constraint, and thus have a more moderate directionality, balancing perceived societal order with increasing challenges.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate to provide clear categories for social organization is still 'live' but its 'status' is 'contested'. The persistence of this specific biological reading, despite its high extractiveness and suppression for certain groups, suggests it functions as a 'tangled_rope' rather than a pure 'rope'. The coordination function (clear categories) is intertwined with asymmetric extraction (exclusion of trans/intersex individuals), preventing mislabeling it as a benign coordination mechanism. The rising suppression and extractiveness over time, coupled with high resistance, indicate an enforcement ratchet rather than a natural, self-sustaining order.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    biological_complexity_vs_binary,
    'To what extent does the biological reality of intersex variations challenge the immutability and binary nature of sex as defined by reproductive biology?',
    'Increased public education on intersex biology and medical consensus on non-binary sex assignment at birth, leading to legal recognition of non-binary sex markers.',
    'If intersex variations are widely acknowledged as fundamental biological realities, the ''immutable binary'' premise of this reading would be severely undermined, potentially shifting the classification towards a ''snare'' for intersex individuals and forcing a re-evaluation of the ''mountain'' aspects of biological sex.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(biological_complexity_vs_binary, empirical, 'Impact of intersex biology on binary sex definitions.').

omega_variable(
    category_purpose_drift,
    'Has the primary purpose of sex-based categories drifted from reproductive organization to a tool for social exclusion and boundary maintenance?',
    'Analysis of legal and policy changes over time, specifically examining whether new sex-based classifications are primarily used to restrict access or to facilitate reproductive health/safety.',
    'If the purpose has drifted to exclusion, the ''coordination'' aspect of this reading would be revealed as cover, pushing its classification more firmly towards a ''snare'' or a highly extractive ''tangled_rope''.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(category_purpose_drift, conceptual, 'Drift in the purpose of sex-based categories.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (legal barriers, institutional policies) or internalized (social stigma, fear of violence) for transgender individuals?',
    'Post-legal reform analysis: if social stigma and fear of violence persist after legal recognition of gender identity, reclassify as partially internalized suppression.',
    'If internalized, the constraint''s effective suppression for transgender individuals is higher than the structural measure suggests, as they carry the suppression with them even after some legal barriers are removed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for transgender individuals.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sex_gender_category__biology_reading, 1950, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sex__tr_t1950, sex_gender_category__biology_reading, theater_ratio, 1950, 0.1).
narrative_ontology:measurement(sex__tr_t1970, sex_gender_category__biology_reading, theater_ratio, 1970, 0.12).
narrative_ontology:measurement(sex__tr_t1990, sex_gender_category__biology_reading, theater_ratio, 1990, 0.15).
narrative_ontology:measurement(sex__tr_t2010, sex_gender_category__biology_reading, theater_ratio, 2010, 0.18).
narrative_ontology:measurement(sex__tr_t2024, sex_gender_category__biology_reading, theater_ratio, 2024, 0.2).

% Extraction over time
narrative_ontology:measurement(sex__be_t1950, sex_gender_category__biology_reading, base_extractiveness, 1950, 0.5).
narrative_ontology:measurement(sex__be_t1970, sex_gender_category__biology_reading, base_extractiveness, 1970, 0.55).
narrative_ontology:measurement(sex__be_t1990, sex_gender_category__biology_reading, base_extractiveness, 1990, 0.6).
narrative_ontology:measurement(sex__be_t2010, sex_gender_category__biology_reading, base_extractiveness, 2010, 0.63).
narrative_ontology:measurement(sex__be_t2024, sex_gender_category__biology_reading, base_extractiveness, 2024, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(sex__su_t1950, sex_gender_category__biology_reading, suppression_requirement, 1950, 0.4).
narrative_ontology:measurement(sex__su_t1970, sex_gender_category__biology_reading, suppression_requirement, 1970, 0.5).
narrative_ontology:measurement(sex__su_t1990, sex_gender_category__biology_reading, suppression_requirement, 1990, 0.6).
narrative_ontology:measurement(sex__su_t2010, sex_gender_category__biology_reading, suppression_requirement, 2010, 0.65).
narrative_ontology:measurement(sex__su_t2024, sex_gender_category__biology_reading, suppression_requirement, 2024, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sex_gender_category__biology_reading, identity_coordination).
narrative_ontology:affects_constraint(sex_gender_category__biology_reading, sex_gender_category__identity_reading).
narrative_ontology:affects_constraint(sex_gender_category__biology_reading, sex_gender_category__hybrid_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'sex_gender_category' kernel. This 'biology_reading' defines sex by immutable reproductive biology. It is linked to the 'identity_reading' (sex by subjective gender identity) and 'hybrid_reading' (sex by biology + social transition) as part of a constraint family, where each reading represents a distinct structural claim.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
