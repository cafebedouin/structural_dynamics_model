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
 *   This constraint represents the 'biology reading' of sex and gender
 *   categories, where membership is determined by immutable reproductive
 *   biology (chromosomes, anatomy at birth). It is one reading of the broader
 *   'sex_gender_category' kernel. This reading structurally excludes trans
 *   women from the 'woman' category, positions cis women as the sole victim
 *   set for sex-based harms, incurs high boundary enforcement costs, and
 *   often forces intersex individuals into a binary framework. The metrics
 *   reflect the substantial extraction and suppression required to maintain
 *   this rigid classification in the face of social and scientific
 *   contestation.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sex_gender_category__biology_reading, 0.65).
domain_priors:suppression_score(sex_gender_category__biology_reading, 0.7).
domain_priors:theater_ratio(sex_gender_category__biology_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sex_gender_category__biology_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(sex_gender_category__biology_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(sex_gender_category__biology_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(sex_gender_category__biology_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(sex_gender_category__biology_reading, resistance, 0.8).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sex_gender_category__biology_reading, tangled_rope).
narrative_ontology:human_readable(sex_gender_category__biology_reading, "Sex Category Membership (Biology Reading)").
narrative_ontology:topic_domain(sex_gender_category__biology_reading, "social_ontology/identity_politics/legal_classification").

domain_priors:requires_active_enforcement(sex_gender_category__biology_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(sex_gender_category__biology_reading, '5b196669-f66e-400c-8f0f-e83dbad0fe13').
narrative_ontology:cs_kernel_codification('5b196669-f66e-400c-8f0f-e83dbad0fe13', formalized).
narrative_ontology:cs_authority_grounding('5b196669-f66e-400c-8f0f-e83dbad0fe13', lineage).
narrative_ontology:cs_interpretation_layer_present('5b196669-f66e-400c-8f0f-e83dbad0fe13').
narrative_ontology:cs_reading_relation('5b196669-f66e-400c-8f0f-e83dbad0fe13', sex_gender_category__identity_reading, forecloses).
narrative_ontology:cs_reading_relation('5b196669-f66e-400c-8f0f-e83dbad0fe13', sex_gender_category__hybrid_reading, influences).
narrative_ontology:cs_axiom('5b196669-f66e-400c-8f0f-e83dbad0fe13', foundational, sex_is_binary_and_immutable).
narrative_ontology:cs_axiom_status(sex_is_binary_and_immutable, holdable).
narrative_ontology:cs_axiom_grounding('5b196669-f66e-400c-8f0f-e83dbad0fe13', sex_is_binary_and_immutable, empirically_contingent).
narrative_ontology:cs_axiom('5b196669-f66e-400c-8f0f-e83dbad0fe13', secondary, sex_determines_gender_roles).
narrative_ontology:cs_axiom_status(sex_determines_gender_roles, holdable).
narrative_ontology:cs_axiom_grounding('5b196669-f66e-400c-8f0f-e83dbad0fe13', sex_determines_gender_roles, conventional).
narrative_ontology:cs_reference_frame('5b196669-f66e-400c-8f0f-e83dbad0fe13', traditional_binary_sex_classification).
narrative_ontology:cs_drift_state('5b196669-f66e-400c-8f0f-e83dbad0fe13', contemporary_identity_politics_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('5b196669-f66e-400c-8f0f-e83dbad0fe13', '').
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

% Advocate for sex-based categories defined by reproductive biology, asserting this is necessary to protect the rights and spaces of cisgender women. They actively enforce these boundaries in policy and discourse, often bearing significant social costs for doing so.
narrative_ontology:constraint_stakeholder(sex_gender_category__biology_reading, cis_women_advocates, agenda_setter,
    organized, generational, constrained, national).

% Are excluded from categories they identify with, leading to denial of access to sex-segregated spaces, legal recognition, and social affirmation. Their identity is fundamentally challenged by this reading, making exit from the 'target' position impossible without abandoning their self-concept.
narrative_ontology:constraint_stakeholder(sex_gender_category__biology_reading, trans_women, payer,
    powerless, biographical, identity_locked, global).

% Are often forced into binary sex categories that do not align with their biological reality, leading to medical interventions and social pressure. Their existence challenges the immutability axiom, but they are often coerced into compliance.
narrative_ontology:constraint_stakeholder(sex_gender_category__biology_reading, intersex_individuals, payer,
    powerless, biographical, identity_locked, global).

% Experience social pressure and exclusion when their gender expression does not align with rigid biological sex categories, even if their biological sex is unambiguous. They bear the cost of social policing of gender norms.
narrative_ontology:constraint_stakeholder(sex_gender_category__biology_reading, gender_non_conforming_individuals, payer,
    moderate, biographical, constrained, local).

% Benefit from the re-emphasis on biological sex as a primary category for analysis, securing funding and academic legitimacy for research focused on sex differences. They provide scientific justification for the constraint.
narrative_ontology:constraint_stakeholder(sex_gender_category__biology_reading, biological_sex_researchers, beneficiary,
    institutional, generational, mobile, global).

% Are tasked with codifying and enforcing sex-based classifications, often struggling to reconcile traditional biological definitions with evolving social understandings and human rights principles. They bear the cost of legal challenges and social unrest.
narrative_ontology:constraint_stakeholder(sex_gender_category__biology_reading, legal_systems, agenda_setter,
    institutional, civilizational, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a clear, binary, and ostensibly immutable basis for social and legal classification, particularly in areas like sports, prisons, and single-sex spaces, aiming to coordinate expectations and resource allocation based on reproductive biology.
% TRANSFER_FUNCTION: Transfers social and legal recognition, access to specific spaces, and the definition of 'woman' from trans women and intersex individuals to cisgender women, based on biological criteria. It also transfers the burden of boundary enforcement to advocates and legal systems.
% ABSENT_VOICES: Advocates for gender self-identification and intersex rights organizations are actively excluded from the framing of this constraint, as their core premises directly challenge its foundational axioms. They would argue for a more inclusive and less coercive classification system.
% DISAPPEARANCE_RATIONALE: If this biological reading of sex categories vanished overnight, legal systems would need to rapidly redefine sex and gender, social norms around single-sex spaces would shift, and the political landscape of identity politics would fundamentally reorganize. The current beneficiaries would lose their exclusive claim to certain categories and protections.
% FOUNDING_PROBLEM: The need for a stable, universally recognizable basis for human classification, particularly for reproductive roles, legal rights, and social organization, historically rooted in observable biological differences.
% FOUNDING_PROBLEM_CORROBORATION: Advocates for this reading assert the problem is live, citing ongoing concerns about women's safety and fairness in sports. Opponents, including trans rights groups and many social scientists, argue the problem has been reframed to exclude marginalized groups, and that the original problem of basic classification is now overdetermined by social constructs, with corroboration from sociological and psychological research.
narrative_ontology:disappearance_verdict(sex_gender_category__biology_reading, world_rearranges).
narrative_ontology:founding_problem_status(sex_gender_category__biology_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(sex_gender_category__biology_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
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
 *   Extractiveness is high (0.65) due to the significant social and legal costs imposed on trans women and intersex individuals by their exclusion or misclassification. Suppression (0.70) is also high, reflecting the active enforcement of these categories through legal challenges, policy debates, and social policing. Resistance is very high (0.80) as this reading is met with strong opposition from trans rights advocates and many academic disciplines. Theater ratio is low (0.10) because the constraint is actively defended and its function, while contested, is not merely performative from the perspective of its proponents.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of cis women advocates, this constraint is a necessary 'rope' for coordination and protection. From the perspective of trans women and intersex individuals, it operates as a 'snare' of exclusion and coercion. The engine's classification will reflect this divergence based on the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Cis women advocates and biological sex researchers are beneficiaries (d near 0.0) as this reading secures their claims and research. Trans women, intersex individuals, and gender non-conforming individuals are targets (d near 1.0) as they bear the direct costs of exclusion and misclassification. Legal systems act as agenda-setters, mediating the enforcement and codification of these categories.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    biological_immutability_ambiguity,
    'Is ''immutable reproductive biology'' a sufficiently clear and universally applicable criterion for sex classification, given the existence of intersex variations and evolving scientific understanding?',
    'Consensus among medical and biological scientific bodies on a precise, universally applicable definition of ''sex'' that accounts for all human variations without forcing binary classification.',
    'If the criterion is found to be ambiguous or not universally applicable, the foundational axiom of this reading is weakened, potentially leading to reclassification towards a more ''tangled_rope'' or ''snare'' type due to its reliance on contested definitions.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(biological_immutability_ambiguity, empirical, 'Ambiguity in the foundational biological definition of sex.').

omega_variable(
    coordination_vs_exclusion_function,
    'Does the primary function of this constraint genuinely coordinate social categories for the benefit of cis women, or is its primary effect the exclusion and harm of trans and intersex individuals?',
    'Analysis of policy outcomes: if policies based on this reading consistently produce disproportionate harm to marginalized groups without demonstrable, unique benefits to cis women that cannot be achieved otherwise, the exclusion function is dominant.',
    'If the exclusion function is dominant, the constraint would be reclassified closer to a ''snare'', as its coordination claims would be revealed as cover for extraction. If genuine, unique coordination benefits are demonstrated, it might remain a ''tangled_rope'' or even ''rope'' for its beneficiaries.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_vs_exclusion_function, conceptual, 'Distinguishing genuine coordination from exclusionary effects.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (legal barriers, policy exclusion) or internalized (social stigma, self-censorship by targets)?',
    'Post-exit suppression trajectory: if suppression persists after legal/policy barriers are removed, reclassify as partially internalized. This would require observing the experiences of trans and intersex individuals in jurisdictions where legal recognition is granted.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests — the target carries the suppression with them after exit, indicating deeper, more pervasive harm.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for trans and intersex individuals.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sex_gender_category__biology_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sex__tr_t0, sex_gender_category__biology_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(sex__tr_t6, sex_gender_category__biology_reading, theater_ratio, 6, 0.1).
narrative_ontology:measurement(sex__tr_t12, sex_gender_category__biology_reading, theater_ratio, 12, 0.1).
narrative_ontology:measurement(sex__tr_t18, sex_gender_category__biology_reading, theater_ratio, 18, 0.1).
narrative_ontology:measurement(sex__tr_t24, sex_gender_category__biology_reading, theater_ratio, 24, 0.1).
narrative_ontology:measurement(sex__tr_t30, sex_gender_category__biology_reading, theater_ratio, 30, 0.1).

% Extraction over time
narrative_ontology:measurement(sex__be_t0, sex_gender_category__biology_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(sex__be_t6, sex_gender_category__biology_reading, base_extractiveness, 6, 0.58).
narrative_ontology:measurement(sex__be_t12, sex_gender_category__biology_reading, base_extractiveness, 12, 0.61).
narrative_ontology:measurement(sex__be_t18, sex_gender_category__biology_reading, base_extractiveness, 18, 0.63).
narrative_ontology:measurement(sex__be_t24, sex_gender_category__biology_reading, base_extractiveness, 24, 0.64).
narrative_ontology:measurement(sex__be_t30, sex_gender_category__biology_reading, base_extractiveness, 30, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(sex__su_t0, sex_gender_category__biology_reading, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(sex__su_t6, sex_gender_category__biology_reading, suppression_requirement, 6, 0.63).
narrative_ontology:measurement(sex__su_t12, sex_gender_category__biology_reading, suppression_requirement, 12, 0.66).
narrative_ontology:measurement(sex__su_t18, sex_gender_category__biology_reading, suppression_requirement, 18, 0.68).
narrative_ontology:measurement(sex__su_t24, sex_gender_category__biology_reading, suppression_requirement, 24, 0.69).
narrative_ontology:measurement(sex__su_t30, sex_gender_category__biology_reading, suppression_requirement, 30, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sex_gender_category__biology_reading, identity_coordination).
narrative_ontology:affects_constraint(sex_gender_category__biology_reading, sex_gender_category__identity_reading).
narrative_ontology:affects_constraint(sex_gender_category__biology_reading, sex_gender_category__hybrid_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'sex_gender_category' kernel. This 'biology_reading' defines categories by immutable reproductive biology. The 'identity_reading' defines by subjective gender identity, and the 'hybrid_reading' by a combination of biology and social transition. All three are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
