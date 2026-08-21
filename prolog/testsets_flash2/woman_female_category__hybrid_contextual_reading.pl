% ============================================================================
% CONSTRAINT STORY: woman_female_category__hybrid_contextual_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_woman_female_category__hybrid_contextual_reading, []).

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
 *   constraint_id: woman_female_category__hybrid_contextual_reading
 *   human_readable: Hybrid Contextual Reading of Woman/Female Category
 *   domain: political_philosophy/bioethics/gender_studies/law
 *
 * SUMMARY:
 *   This constraint represents a 'hybrid contextual' reading of the
 *   categories 'woman' and 'female,' where the criteria for membership vary
 *   depending on the specific context (e.g., biological sex for
 *   medical/sports/safety, gender identity for social/legal recognition). It
 *   is one reading of the broader 'woman_female_category' kernel. The
 *   constraint is claimed as a Tangled Rope because it attempts to coordinate
 *   diverse social needs but results in asymmetric extraction, with both
 *   cisgender and transgender women experiencing costs depending on the
 *   context.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(woman_female_category__hybrid_contextual_reading, 0.45).
domain_priors:suppression_score(woman_female_category__hybrid_contextual_reading, 0.6).
domain_priors:theater_ratio(woman_female_category__hybrid_contextual_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(woman_female_category__hybrid_contextual_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(woman_female_category__hybrid_contextual_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(woman_female_category__hybrid_contextual_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(woman_female_category__hybrid_contextual_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(woman_female_category__hybrid_contextual_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(woman_female_category__hybrid_contextual_reading, tangled_rope).
narrative_ontology:human_readable(woman_female_category__hybrid_contextual_reading, "Hybrid Contextual Reading of Woman/Female Category").
narrative_ontology:topic_domain(woman_female_category__hybrid_contextual_reading, "political_philosophy/bioethics/gender_studies/law").

domain_priors:requires_active_enforcement(woman_female_category__hybrid_contextual_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(woman_female_category__hybrid_contextual_reading, '0f54e52c-ac60-4bb8-aa50-253701478d0e').
narrative_ontology:cs_kernel_codification('0f54e52c-ac60-4bb8-aa50-253701478d0e', formalized).
narrative_ontology:cs_authority_grounding('0f54e52c-ac60-4bb8-aa50-253701478d0e', lineage).
narrative_ontology:cs_interpretation_layer_present('0f54e52c-ac60-4bb8-aa50-253701478d0e').
narrative_ontology:cs_reading_relation('0f54e52c-ac60-4bb8-aa50-253701478d0e', woman_female_category__gender_identity_reading, coexists_with).
narrative_ontology:cs_reading_relation('0f54e52c-ac60-4bb8-aa50-253701478d0e', woman_female_category__sex_biology_reading, coexists_with).
narrative_ontology:cs_axiom('0f54e52c-ac60-4bb8-aa50-253701478d0e', foundational, context_determines_category_criteria).
narrative_ontology:cs_axiom_status(context_determines_category_criteria, holdable).
narrative_ontology:cs_axiom_grounding('0f54e52c-ac60-4bb8-aa50-253701478d0e', context_determines_category_criteria, conventional).
narrative_ontology:cs_axiom('0f54e52c-ac60-4bb8-aa50-253701478d0e', foundational, balancing_competing_claims_is_necessary).
narrative_ontology:cs_axiom_status(balancing_competing_claims_is_necessary, holdable).
narrative_ontology:cs_axiom_grounding('0f54e52c-ac60-4bb8-aa50-253701478d0e', balancing_competing_claims_is_necessary, instrumental).
narrative_ontology:cs_reference_frame('0f54e52c-ac60-4bb8-aa50-253701478d0e', pragmatic_social_ordering).
narrative_ontology:cs_drift_state('0f54e52c-ac60-4bb8-aa50-253701478d0e', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('0f54e52c-ac60-4bb8-aa50-253701478d0e', '').
narrative_ontology:cs_kernel_id(woman_female_category__hybrid_contextual_reading, woman_female_category).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(woman_female_category__hybrid_contextual_reading, institutional_actors_seeking_conflict_minimization).
narrative_ontology:constraint_victim(woman_female_category__hybrid_contextual_reading, transgender_women_in_sex_segregated_spaces).
narrative_ontology:constraint_victim(woman_female_category__hybrid_contextual_reading, cisgender_women_in_gender_identity_spaces).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(woman_female_category__hybrid_contextual_reading, gender_identity_advocates).
narrative_ontology:constraint_beneficiary(woman_female_category__hybrid_contextual_reading, sex_based_rights_advocates).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These actors (e.g., sports federations, medical bodies, legal drafters) attempt to balance competing claims by applying different criteria for 'woman' or 'female' across various contexts. They benefit from reduced public conflict and legal challenges, but bear the cost of maintaining complex, context-dependent rules.
narrative_ontology:constraint_stakeholder(woman_female_category__hybrid_contextual_reading, institutional_actors_seeking_conflict_minimization, agenda_setter,
    institutional, generational, constrained, national).

% Are excluded from sex-segregated spaces (e.g., women's sports, some medical contexts) based on biological sex, despite identifying as women. They experience this as a denial of their identity and a barrier to participation, with limited recourse due to the contextual application of rules.
narrative_ontology:constraint_stakeholder(woman_female_category__hybrid_contextual_reading, transgender_women_in_sex_segregated_spaces, payer,
    powerless, biographical, identity_locked, local).

% Are expected to accept gender identity as the primary criterion in social and legal recognition contexts, which they may perceive as eroding sex-based protections or categories. They bear the cost of adapting to a framework that may not align with their understanding of 'woman', with limited options for opting out of social norms.
narrative_ontology:constraint_stakeholder(woman_female_category__hybrid_contextual_reading, cisgender_women_in_gender_identity_spaces, payer,
    moderate, biographical, constrained, local).

% Benefit from the recognition of gender identity in social and legal contexts, advancing their goal of full inclusion for transgender individuals. They may view the biological sex-based exceptions as necessary compromises or as areas for future advocacy.
narrative_ontology:constraint_stakeholder(woman_female_category__hybrid_contextual_reading, gender_identity_advocates, beneficiary,
    organized, generational, mobile, national).

% Benefit from the retention of biological sex as a criterion in contexts like sports and medicine, which they see as essential for protecting the rights and safety of cisgender women. They may view the gender identity-based recognition in other contexts as a necessary compromise or as an area of concern.
narrative_ontology:constraint_stakeholder(woman_female_category__hybrid_contextual_reading, sex_based_rights_advocates, beneficiary,
    organized, generational, mobile, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Attempts to coordinate social and legal norms by providing a framework for category membership that acknowledges both biological sex and gender identity, aiming to reduce conflict and provide clarity in specific domains.
% TRANSFER_FUNCTION: Transfers social and legal recognition (or exclusion) based on context-dependent criteria, from one group to another, or from one context to another, aiming to distribute the costs and benefits of inclusion/exclusion across different groups and domains.
% ABSENT_VOICES: Individuals who reject any form of contextual compromise, insisting on a single, universal definition of 'woman' or 'female' (either purely biological or purely identity-based), are often marginalized in the policy-making process that seeks hybrid solutions.
% DISAPPEARANCE_RATIONALE: If this hybrid contextual reading vanished, the legal and social landscape would immediately revert to either a purely biological or purely gender identity-based framework, or descend into complete definitional chaos, leading to widespread legal challenges and social upheaval as institutions would lack clear guidance on category membership.
% FOUNDING_PROBLEM: The problem of reconciling competing claims regarding the definition of 'woman' or 'female' in a diverse society, particularly concerning the rights and needs of both cisgender and transgender individuals across various social, legal, medical, and sporting contexts.
% FOUNDING_PROBLEM_CORROBORATION: Institutional actors (e.g., legal bodies, sports organizations) and a significant portion of the public attest that the problem remains live, as evidenced by ongoing debates, legal challenges, and policy adjustments. This is corroborated by media coverage and academic discourse from outside the immediate advocacy groups.
narrative_ontology:disappearance_verdict(woman_female_category__hybrid_contextual_reading, world_rearranges).
narrative_ontology:founding_problem_status(woman_female_category__hybrid_contextual_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(woman_female_category__hybrid_contextual_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(woman_female_category__hybrid_contextual_reading, 'none', 1).
narrative_ontology:epsilon_provenance(woman_female_category__hybrid_contextual_reading, 0.45, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(woman_female_category__hybrid_contextual_reading_tests).
:- end_tests(woman_female_category__hybrid_contextual_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.45) is moderate because while it attempts to balance, it still imposes costs on both groups of women by subordinating their preferred definition in certain contexts. Suppression (0.6) is significant as institutional actors actively enforce these contextual distinctions, often against resistance from those who prefer a universal definition. Theater ratio (0.2) is low, as the distinctions are genuinely applied, though the 'balance' may be performative in some cases. Resistance (0.7) is high due to ongoing advocacy from both sides of the debate.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of institutional actors, this is a necessary and pragmatic coordination mechanism. From the perspective of those whose preferred definition is subordinated in a given context, it is an extractive imposition. The engine's per-seat classification will reflect these divergent experiences.
 *
 * DIRECTIONALITY LOGIC:
 *   Institutional actors seeking conflict minimization are the primary beneficiaries, as this reading provides a framework to manage complex social and legal issues, reducing direct conflict. Both transgender women (in sex-segregated spaces) and cisgender women (in gender identity-based spaces) are victims, as their preferred category definition is subordinated in specific contexts, leading to exclusion or perceived erosion of rights. Advocates for both gender identity and sex-based rights are secondary beneficiaries, as their positions are partially recognized.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    contextual_boundary_stability,
    'Are the boundaries between contexts (medical/sports/safety vs. social/legal) stable and clearly defined, or are they subject to ongoing contestation and drift?',
    'Longitudinal study of legal challenges and policy changes in different jurisdictions; analysis of public discourse and advocacy efforts to shift boundaries.',
    'If boundaries are unstable, the constraint''s effective extractiveness and suppression will fluctuate, leading to greater uncertainty and potential for reclassification towards a more purely extractive or purely coordinative type as one definition gains dominance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(contextual_boundary_stability, empirical, 'Examines the stability of contextual distinctions over time.').

omega_variable(
    legitimacy_of_compromise,
    'Is the ''compromise'' inherent in this hybrid reading genuinely accepted as legitimate by the affected parties, or is it perceived as an imposed solution by institutional actors?',
    'Surveys and qualitative interviews with cisgender and transgender women, as well as advocates, to gauge their perception of fairness and legitimacy of the contextual distinctions.',
    'If perceived as illegitimate, the constraint''s resistance will remain high, and its long-term stability as a ''tangled rope'' is questionable, potentially leading to a breakdown into more purely extractive ''snare'' dynamics as one side attempts to impose its definition universally.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legitimacy_of_compromise, preference, 'Assesses the perceived legitimacy of the contextual compromise.').

omega_variable(
    unintended_consequences_of_contextualization,
    'Does the contextual application of definitions create unintended consequences or new forms of exclusion/discrimination that are not accounted for in the current framework?',
    'Case studies of individuals navigating multiple contexts with conflicting definitions; analysis of intersectional impacts on marginalized groups.',
    'If significant unintended consequences are found, the constraint''s effective extractiveness could be higher than measured, and the ''coordination'' function could be revealed as a cover for new forms of harm, pushing classification towards ''snare''.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(unintended_consequences_of_contextualization, empirical, 'Investigates unforeseen negative impacts of contextual definitions.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(woman_female_category__hybrid_contextual_reading, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(woma_tr_t0, woman_female_category__hybrid_contextual_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(woma_tr_t5, woman_female_category__hybrid_contextual_reading, theater_ratio, 5, 0.18).
narrative_ontology:measurement(woma_tr_t10, woman_female_category__hybrid_contextual_reading, theater_ratio, 10, 0.2).

% Extraction over time
narrative_ontology:measurement(woma_be_t0, woman_female_category__hybrid_contextual_reading, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(woma_be_t5, woman_female_category__hybrid_contextual_reading, base_extractiveness, 5, 0.43).
narrative_ontology:measurement(woma_be_t10, woman_female_category__hybrid_contextual_reading, base_extractiveness, 10, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(woma_su_t0, woman_female_category__hybrid_contextual_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(woma_su_t5, woman_female_category__hybrid_contextual_reading, suppression_requirement, 5, 0.58).
narrative_ontology:measurement(woma_su_t10, woman_female_category__hybrid_contextual_reading, suppression_requirement, 10, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
