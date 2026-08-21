% ============================================================================
% CONSTRAINT STORY: woman_category__intersex_accommodation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_woman_category__intersex_accommodation_reading, []).

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
 *   constraint_id: woman_category__intersex_accommodation_reading
 *   human_readable: Woman Category: Intersex Accommodation Reading
 *   domain: political_philosophy/law/social_policy/bioethics
 *
 * SUMMARY:
 *   This constraint represents a reading of the 'woman' category that
 *   acknowledges biological sex as a non-binary spectrum, explicitly
 *   including typical female biology and intersex variations that do not fit
 *   the male category. It aims to provide a more inclusive and biologically
 *   accurate framework for understanding sex. The claimed type is 'rope'
 *   because it primarily functions as a coordination mechanism for a more
 *   nuanced biological understanding, with relatively low extraction for
 *   most, though it creates friction for those holding strictly binary or
 *   identity-only views. The kernel context is explicitly documented in
 *   omegas and cs_structure.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(woman_category__intersex_accommodation_reading, 0.25).
domain_priors:suppression_score(woman_category__intersex_accommodation_reading, 0.4).
domain_priors:theater_ratio(woman_category__intersex_accommodation_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(woman_category__intersex_accommodation_reading, extractiveness, 0.25).
narrative_ontology:constraint_metric(woman_category__intersex_accommodation_reading, suppression_requirement, 0.4).
narrative_ontology:constraint_metric(woman_category__intersex_accommodation_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(woman_category__intersex_accommodation_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(woman_category__intersex_accommodation_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(woman_category__intersex_accommodation_reading, rope).
narrative_ontology:human_readable(woman_category__intersex_accommodation_reading, "Woman Category: Intersex Accommodation Reading").
narrative_ontology:topic_domain(woman_category__intersex_accommodation_reading, "political_philosophy/law/social_policy/bioethics").

domain_priors:requires_active_enforcement(woman_category__intersex_accommodation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(woman_category__intersex_accommodation_reading, '8f381af8-d781-462d-a8e5-b203526d58d8').
narrative_ontology:cs_kernel_codification('8f381af8-d781-462d-a8e5-b203526d58d8', distributed).
narrative_ontology:cs_authority_grounding('8f381af8-d781-462d-a8e5-b203526d58d8', expertise).
narrative_ontology:cs_interpretation_layer_present('8f381af8-d781-462d-a8e5-b203526d58d8').
narrative_ontology:cs_reading_relation('8f381af8-d781-462d-a8e5-b203526d58d8', woman_category__sex_biology_reading, influences).
narrative_ontology:cs_reading_relation('8f381af8-d781-462d-a8e5-b203526d58d8', woman_category__gender_identity_reading, coexists_with).
narrative_ontology:cs_axiom('8f381af8-d781-462d-a8e5-b203526d58d8', foundational, biological_sex_is_a_spectrum).
narrative_ontology:cs_axiom_status(biological_sex_is_a_spectrum, holdable).
narrative_ontology:cs_axiom_grounding('8f381af8-d781-462d-a8e5-b203526d58d8', biological_sex_is_a_spectrum, empirically_contingent).
narrative_ontology:cs_axiom('8f381af8-d781-462d-a8e5-b203526d58d8', foundational, inclusion_of_intersex_individuals_is_a_moral_imperative).
narrative_ontology:cs_axiom_status(inclusion_of_intersex_individuals_is_a_moral_imperative, holdable).
narrative_ontology:cs_axiom_grounding('8f381af8-d781-462d-a8e5-b203526d58d8', inclusion_of_intersex_individuals_is_a_moral_imperative, deontological).
narrative_ontology:cs_reference_frame('8f381af8-d781-462d-a8e5-b203526d58d8', scientific_biological_diversity).
narrative_ontology:cs_drift_state('8f381af8-d781-462d-a8e5-b203526d58d8', contemporary_social_discourse, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('8f381af8-d781-462d-a8e5-b203526d58d8', '2024-07-30T12:00:00Z').
narrative_ontology:cs_kernel_id(woman_category__intersex_accommodation_reading, woman_category).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(woman_category__intersex_accommodation_reading, intersex_individuals).
narrative_ontology:constraint_beneficiary(woman_category__intersex_accommodation_reading, medical_professionals).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(woman_category__intersex_accommodation_reading, women_with_typical_female_biology).
narrative_ontology:constraint_victim(woman_category__intersex_accommodation_reading, gender_identity_advocates).
narrative_ontology:constraint_victim(woman_category__intersex_accommodation_reading, sex_biology_advocates).
narrative_ontology:constraint_vindicates(woman_category__intersex_accommodation_reading, biological_diversity_principle).
narrative_ontology:constraint_vindicates(woman_category__intersex_accommodation_reading, non_discrimination_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Individuals whose biological sex characteristics (chromosomes, gonads, hormones, anatomy) do not fit typical binary definitions of male or female. This reading provides a framework for their inclusion in the category 'woman' based on their biology, offering recognition and reducing exclusion.
narrative_ontology:constraint_stakeholder(woman_category__intersex_accommodation_reading, intersex_individuals, beneficiary,
    powerless, biographical, identity_locked, global).

% Experts in biology, genetics, and endocrinology who recognize the spectrum of biological sex. They advocate for and implement policies that reflect this understanding, influencing legal and social definitions of sex and gender categories.
narrative_ontology:constraint_stakeholder(woman_category__intersex_accommodation_reading, medical_professionals, agenda_setter,
    institutional, generational, analytical, global).

% Individuals who identify as women and have typical female biological characteristics. This reading expands the definition of 'woman' to be more inclusive, aligning with a broader understanding of biological diversity, which many find affirming.
narrative_ontology:constraint_stakeholder(woman_category__intersex_accommodation_reading, women_with_typical_female_biology, beneficiary,
    organized, biographical, mobile, global).

% Advocates who prioritize gender identity as the primary determinant of 'woman' category membership. While this reading is inclusive of intersex individuals, it may be seen as insufficient or even problematic if it does not fully align with identity-based definitions, creating friction in policy debates.
narrative_ontology:constraint_stakeholder(woman_category__intersex_accommodation_reading, gender_identity_advocates, payer,
    organized, biographical, constrained, global).

% Advocates who define 'woman' strictly by typical binary reproductive biology. This reading challenges their binary framework by introducing a spectrum, leading to resistance and perceived erosion of their preferred definition.
narrative_ontology:constraint_stakeholder(woman_category__intersex_accommodation_reading, sex_biology_advocates, payer,
    organized, biographical, constrained, global).

% Organizations tasked with setting fair competition rules. This reading forces them to grapple with complex biological realities that challenge simple binary sex categories, particularly in cases where intersex variations might confer a performance advantage, leading to difficult policy decisions and potential exclusion of some intersex athletes.
narrative_ontology:constraint_stakeholder(woman_category__intersex_accommodation_reading, elite_sports_organizations, agenda_setter,
    institutional, generational, constrained, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a more biologically accurate and inclusive definition of 'woman' that accommodates the natural spectrum of human sex characteristics, moving beyond a strict binary.
% TRANSFER_FUNCTION: Transfers recognition and inclusion to intersex individuals, shifting the burden of categorization away from a rigid binary and towards a more nuanced biological understanding.
% ABSENT_VOICES: Historically, intersex individuals themselves were often excluded from these definitional debates, with medical and legal authorities making decisions on their behalf. Their voices are now increasingly present but still face challenges in being fully heard and integrated into policy.
% DISAPPEARANCE_RATIONALE: If this understanding vanished, intersex individuals would revert to being categorized solely within a binary framework, leading to increased exclusion, misgendering, and medical interventions aimed at 'normalizing' their bodies. Policies in sports, healthcare, and legal recognition would become less inclusive and more rigid.
% FOUNDING_PROBLEM: The historical and ongoing exclusion and pathologization of intersex individuals due to rigid binary sex definitions in medical, legal, and social contexts.
% FOUNDING_PROBLEM_CORROBORATION: Intersex advocacy organizations, human rights bodies, and a growing consensus within the medical and scientific community corroborate that the problem of binary exclusion for intersex individuals remains live, despite increasing awareness.
narrative_ontology:disappearance_verdict(woman_category__intersex_accommodation_reading, world_rearranges).
narrative_ontology:founding_problem_status(woman_category__intersex_accommodation_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(woman_category__intersex_accommodation_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(woman_category__intersex_accommodation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(woman_category__intersex_accommodation_reading, 0.25, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(woman_category__intersex_accommodation_reading_tests).
:- end_tests(woman_category__intersex_accommodation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.25) because this reading primarily seeks to include rather than exclude, and the population directly affected (intersex individuals) is small. Suppression is moderate (0.4) as it requires active advocacy and education to counter entrenched binary views, but does not typically involve coercive enforcement against individuals. Resistance is also moderate (0.3) from those who prefer strict binary or identity-only definitions. Accessibility collapse is moderate (0.6) as it offers a clearer path to recognition for intersex individuals, but does not fully resolve all categorization challenges.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of intersex individuals and medical professionals, this is a necessary and beneficial clarification of biological reality. From the perspective of strict binary sex advocates, it is a problematic blurring of categories. From gender identity advocates, it may be seen as an incomplete or competing framework. The engine will compute these divergent classifications based on the structural roles and exit options.
 *
 * DIRECTIONALITY LOGIC:
 *   Intersex individuals are primary beneficiaries (d near 0.0) as this reading directly addresses their inclusion. Medical professionals are agenda-setters and beneficiaries (d near 0.15) as their scientific understanding is vindicated. Advocates for strict binary sex or gender identity definitions are payers (d near 0.7) as this reading challenges their preferred frameworks, creating definitional friction. Elite sports organizations are agenda-setters (d near 0.5) as they must adapt policies, which can be complex and costly.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    boundary_with_gender_identity_reading,
    'How does this biological spectrum reading of ''woman'' interact with the ''gender_identity_reading'' in policy and social contexts?',
    'Analysis of legal cases and policy implementations where both readings are simultaneously invoked or contested, particularly regarding access to single-sex spaces or services.',
    'If the readings are found to be in direct conflict in critical policy areas, it could lead to increased suppression or exclusion for one group; if they are found to be complementary, it could lead to more robust, multi-faceted inclusion policies.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(boundary_with_gender_identity_reading, conceptual, 'Ambiguity in how biological spectrum and gender identity definitions of ''woman'' coexist or conflict.').

omega_variable(
    application_in_elite_sports,
    'To what extent does this reading''s accommodation of intersex variations create perceived unfairness or competitive advantage in elite sports categories?',
    'Empirical studies on the physiological advantages conferred by specific intersex variations in sports, combined with ethical and policy debates on balancing inclusion with fair competition.',
    'If significant, unmitigable competitive advantages are consistently found, it could lead to more restrictive policies for intersex athletes in certain categories, increasing extraction for them. If not, it reinforces the inclusive framework.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(application_in_elite_sports, empirical, 'Impact of intersex accommodation on fairness in elite sports.').

omega_variable(
    natural_vs_constructed_spectrum,
    'Is the non-binary spectrum of biological sex an ''emerges_naturally'' phenomenon, or is its recognition and categorization a ''constructed'' social and medical framework?',
    'Philosophical analysis of biological essentialism vs. social constructionism in sex categorization, combined with historical studies of medical and scientific discourse on intersex conditions.',
    'If ''emerges_naturally'' is strongly affirmed, it strengthens the ''mountain'' aspect of this reading. If ''constructed'' is emphasized, it highlights the role of human agency and policy choices, potentially shifting the classification towards a ''rope'' or ''tangled_rope'' depending on the power dynamics involved.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_vs_constructed_spectrum, conceptual, 'Whether the biological sex spectrum is a natural fact or a constructed framework.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(woman_category__intersex_accommodation_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Extraction over time
narrative_ontology:measurement(woma_be_t0, woman_category__intersex_accommodation_reading, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(woma_be_t5, woman_category__intersex_accommodation_reading, base_extractiveness, 5, 0.2).
narrative_ontology:measurement(woma_be_t10, woman_category__intersex_accommodation_reading, base_extractiveness, 10, 0.25).
narrative_ontology:measurement(woma_be_t15, woman_category__intersex_accommodation_reading, base_extractiveness, 15, 0.25).
narrative_ontology:measurement(woma_be_t20, woman_category__intersex_accommodation_reading, base_extractiveness, 20, 0.25).

% Suppression requirement over time
narrative_ontology:measurement(woma_su_t0, woman_category__intersex_accommodation_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(woma_su_t5, woman_category__intersex_accommodation_reading, suppression_requirement, 5, 0.35).
narrative_ontology:measurement(woma_su_t10, woman_category__intersex_accommodation_reading, suppression_requirement, 10, 0.4).
narrative_ontology:measurement(woma_su_t15, woman_category__intersex_accommodation_reading, suppression_requirement, 15, 0.4).
narrative_ontology:measurement(woma_su_t20, woman_category__intersex_accommodation_reading, suppression_requirement, 20, 0.4).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(woman_category__intersex_accommodation_reading, information_standard).
narrative_ontology:affects_constraint(woman_category__intersex_accommodation_reading, sex_biology_reading).
narrative_ontology:affects_constraint(woman_category__intersex_accommodation_reading, gender_identity_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'woman_category' kernel. This reading focuses on biological diversity and intersex inclusion, influencing and being influenced by the sex_biology_reading and gender_identity_reading.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
