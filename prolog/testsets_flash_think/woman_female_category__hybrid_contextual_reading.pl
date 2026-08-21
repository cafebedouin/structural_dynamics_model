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
 *   constraint_id: woman_female_category__hybrid_contextual_reading
 *   human_readable: Hybrid Contextual Categorization of Woman/Female
 *   domain: political_philosophy/bioethics/gender_studies/law
 *
 * SUMMARY:
 *   This constraint instantiates the 'hybrid_contextual_reading' of the
 *   'woman_female_category' kernel, which posits that category membership
 *   varies by context (biological sex for medical/sports/safety, gender
 *   identity for social/legal recognition). Sibling readings include
 *   'sex_biology_reading' and 'gender_identity_reading'. This reading
 *   attempts to manage the tension between these two approaches by applying
 *   different criteria in different domains, aiming for a pragmatic
 *   compromise in public policy and institutional practice.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(woman_female_category__hybrid_contextual_reading, 0.45).
domain_priors:suppression_score(woman_female_category__hybrid_contextual_reading, 0.55).
domain_priors:theater_ratio(woman_female_category__hybrid_contextual_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(woman_female_category__hybrid_contextual_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(woman_female_category__hybrid_contextual_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(woman_female_category__hybrid_contextual_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(woman_female_category__hybrid_contextual_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(woman_female_category__hybrid_contextual_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(woman_female_category__hybrid_contextual_reading, tangled_rope).
narrative_ontology:human_readable(woman_female_category__hybrid_contextual_reading, "Hybrid Contextual Categorization of Woman/Female").
narrative_ontology:topic_domain(woman_female_category__hybrid_contextual_reading, "political_philosophy/bioethics/gender_studies/law").

domain_priors:requires_active_enforcement(woman_female_category__hybrid_contextual_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(woman_female_category__hybrid_contextual_reading, '598ce565-9e96-4fac-a91f-4aba7d7c3ae3').
narrative_ontology:cs_kernel_codification('598ce565-9e96-4fac-a91f-4aba7d7c3ae3', formalized).
narrative_ontology:cs_authority_grounding('598ce565-9e96-4fac-a91f-4aba7d7c3ae3', practice).
narrative_ontology:cs_interpretation_layer_present('598ce565-9e96-4fac-a91f-4aba7d7c3ae3').
narrative_ontology:cs_reading_relation('598ce565-9e96-4fac-a91f-4aba7d7c3ae3', woman_female_category__sex_biology_reading, coexists_with).
narrative_ontology:cs_reading_relation('598ce565-9e96-4fac-a91f-4aba7d7c3ae3', woman_female_category__gender_identity_reading, coexists_with).
narrative_ontology:cs_axiom('598ce565-9e96-4fac-a91f-4aba7d7c3ae3', foundational, contextual_relevance_of_sex_and_gender).
narrative_ontology:cs_axiom_status(contextual_relevance_of_sex_and_gender, holdable).
narrative_ontology:cs_axiom_grounding('598ce565-9e96-4fac-a91f-4aba7d7c3ae3', contextual_relevance_of_sex_and_gender, conventional).
narrative_ontology:cs_axiom('598ce565-9e96-4fac-a91f-4aba7d7c3ae3', foundational, balancing_competing_rights_claims).
narrative_ontology:cs_axiom_status(balancing_competing_rights_claims, holdable).
narrative_ontology:cs_axiom_grounding('598ce565-9e96-4fac-a91f-4aba7d7c3ae3', balancing_competing_rights_claims, deontological).
narrative_ontology:cs_reference_frame('598ce565-9e96-4fac-a91f-4aba7d7c3ae3', pluralistic_social_governance).
narrative_ontology:cs_drift_state('598ce565-9e96-4fac-a91f-4aba7d7c3ae3', contemporary, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('598ce565-9e96-4fac-a91f-4aba7d7c3ae3', '').
narrative_ontology:cs_kernel_id(woman_female_category__hybrid_contextual_reading, woman_female_category).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(woman_female_category__hybrid_contextual_reading, institutional_actors_seeking_conflict_minimization).
narrative_ontology:constraint_beneficiary(woman_female_category__hybrid_contextual_reading, public_policy_makers).
narrative_ontology:constraint_victim(woman_female_category__hybrid_contextual_reading, transgender_individuals_in_sex_segregated_contexts).
narrative_ontology:constraint_victim(woman_female_category__hybrid_contextual_reading, cisgender_women_in_gender_identity_contexts).
narrative_ontology:constraint_victim(woman_female_category__hybrid_contextual_reading, legal_system_navigators).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These actors (e.g., international sports federations, national governments) seek to implement a framework that reduces immediate social and legal conflict by offering different definitions of 'woman' or 'female' depending on the specific context. They benefit from a perceived reduction in political pressure and a claim to balanced policy.
narrative_ontology:constraint_stakeholder(woman_female_category__hybrid_contextual_reading, institutional_actors_seeking_conflict_minimization, agenda_setter,
    institutional, generational, arbitrage, global).

% Responsible for translating the hybrid framework into specific laws and regulations. They benefit from having a 'solution' to a contentious issue, even if complex, which allows them to move forward with policy implementation, reducing direct political heat.
narrative_ontology:constraint_stakeholder(woman_female_category__hybrid_contextual_reading, public_policy_makers, beneficiary,
    institutional, biographical, constrained, national).

% Individuals who identify as women but are categorized by biological sex in contexts like competitive sports or certain medical settings. They bear the cost of having their gender identity subordinated to biological sex in these specific domains, leading to exclusion or differential treatment.
narrative_ontology:constraint_stakeholder(woman_female_category__hybrid_contextual_reading, transgender_individuals_in_sex_segregated_contexts, payer,
    powerless, biographical, constrained, global).

% Individuals who are biologically female and identify as women, but find that sex-based protections or spaces are diluted or redefined by gender identity in social or legal recognition contexts. They bear the cost of their sex-based needs being subordinated to gender identity in these specific domains.
narrative_ontology:constraint_stakeholder(woman_female_category__hybrid_contextual_reading, cisgender_women_in_gender_identity_contexts, payer,
    moderate, biographical, constrained, global).

% Lawyers, judges, and administrators who must interpret and apply the complex, context-dependent rules of the hybrid framework. They bear the cost of increased administrative burden, legal ambiguity, and the potential for inconsistent application across cases.
narrative_ontology:constraint_stakeholder(woman_female_category__hybrid_contextual_reading, legal_system_navigators, payer,
    moderate, biographical, constrained, national).

% Groups advocating for a consistent definition of 'woman' based solely on biological sex across all contexts. They are excluded from the core premise of this hybrid framework, as it inherently compromises their position by incorporating gender identity.
narrative_ontology:constraint_stakeholder(woman_female_category__hybrid_contextual_reading, advocates_for_sex_based_rights, excluded,
    organized, generational, constrained, global).

% Groups advocating for a consistent definition of 'woman' based solely on gender identity across all contexts. They are excluded from the core premise of this hybrid framework, as it inherently compromises their position by incorporating biological sex.
narrative_ontology:constraint_stakeholder(woman_female_category__hybrid_contextual_reading, advocates_for_gender_identity_rights, excluded,
    organized, generational, constrained, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To provide a framework for categorizing individuals as 'woman' or 'female' that attempts to balance competing claims from sex-based and gender-identity-based perspectives across different social, legal, medical, and sporting contexts, thereby managing social friction.
% TRANSFER_FUNCTION: Transfers administrative burden, legal complexity, and social friction to individuals and institutions navigating the shifting definitions, while transferring a degree of conflict-reduction and perceived legitimacy to institutional actors and public policy makers.
% ABSENT_VOICES: Advocates for a purely sex-based definition and advocates for a purely gender-identity-based definition are both structurally excluded from the core framing of this hybrid approach, as it inherently compromises their preferred consistent application. They would object to the lack of a single, coherent definition.
% DISAPPEARANCE_RATIONALE: If this hybrid contextual framework vanished, the underlying conflict between sex-based and gender-identity-based definitions would resurface with full force, leading to a chaotic and uncoordinated re-establishment of categories. This would likely default to either a purely sex-based or purely gender-identity-based system, or a new, equally contested hybrid, as the fundamental tension remains unresolved.
% FOUNDING_PROBLEM: The irreconcilable conflict between demands for sex-based categorization (e.g., for fairness in sports, medical accuracy) and demands for gender-identity-based categorization (e.g., for social inclusion, legal recognition) in defining 'woman' or 'female' in public policy.
% FOUNDING_PROBLEM_CORROBORATION: Legal scholars, social scientists, and human rights organizations from diverse perspectives corroborate the ongoing, live nature of this conflict. While they disagree on the optimal resolution, there is broad consensus that the tension between sex and gender identity in categorization remains a pressing societal challenge, supporting the claim that the founding problem is still active.
narrative_ontology:disappearance_verdict(woman_female_category__hybrid_contextual_reading, world_rearranges).
narrative_ontology:founding_problem_status(woman_female_category__hybrid_contextual_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(woman_female_category__hybrid_contextual_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
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
 *   The extractiveness (0.45) is moderate because while this framework aims for coordination, it inherently creates friction and administrative overhead due to its complexity and shifting definitions. Individuals whose identities or needs are subordinated in specific contexts experience real costs. Suppression (0.55) is moderate because the framework, by attempting to accommodate, still imposes categories and rules that can marginalize those who advocate for a simpler, consistent definition or whose identities don't fit neatly. Resistance (0.60) is high because this compromise satisfies no party fully, leading to ongoing contestation from both sex-based and gender-identity-based advocates. Theater ratio (0.25) is moderate-low, reflecting genuine efforts to implement the framework, but also the performative aspects of navigating the political landscape and justifying complex rules.
 *
 * PERSPECTIVAL GAP:
 *   Institutional actors perceive this framework as a necessary and pragmatic compromise that reduces overall societal conflict. However, from the perspective of the payer seats (transgender individuals, cisgender women, legal navigators), the same structure is experienced as a source of ongoing burden, confusion, and the subordination of their specific needs or identities in certain contexts. The engine's per-seat classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Institutional actors seeking conflict minimization and public policy makers are structural beneficiaries (low directionality) as they gain a framework to manage a contentious issue, reducing direct political pressure. Transgender individuals in sex-segregated contexts, cisgender women in gender-identity contexts, and legal system navigators are targets (high directionality) as they bear the direct costs of navigating and being subjected to the shifting, complex definitions, experiencing subordination of their identity or needs in specific domains.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint addresses a live and highly contested social problem, so it is not subject to mandatrophy. Its persistence is driven by the ongoing need to manage the fundamental tension between sex-based and gender-identity-based categorizations, rather than by inertia or a solved problem.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    conflict_resolution_vs_displacement,
    'Is this hybrid contextual approach genuinely resolving the underlying conflict between sex-based and gender-identity-based categorizations, or is it merely displacing and diffusing it into administrative complexity and individual burden?',
    'Longitudinal studies tracking the levels of social conflict, legal challenges, and individual well-being in jurisdictions implementing this framework versus those with more consistent (sex-based or gender-identity-based) approaches.',
    'If it''s merely displacement, the effective extractiveness and suppression are higher than measured, as the ''coordination'' function is illusory, and the constraint is closer to a Snare. If genuine resolution, the current metrics are accurate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(conflict_resolution_vs_displacement, conceptual, 'Assessing whether the hybrid framework achieves genuine conflict resolution or just shifts the burden.').

omega_variable(
    long_term_individual_impact,
    'What is the long-term psychological, social, and legal impact on individuals who must navigate these shifting, context-dependent categories for their own identity and rights?',
    'Qualitative and quantitative research on individuals'' experiences, including mental health outcomes, sense of belonging, and legal clarity, over extended periods in jurisdictions with hybrid systems.',
    'If the impact is significantly negative (e.g., increased confusion, distress, or legal precarity), the effective extractiveness and suppression for affected individuals are higher than currently estimated, pushing their seat classification towards Snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(long_term_individual_impact, empirical, 'Evaluating the human cost of navigating context-dependent identity categories.').

omega_variable(
    conceptual_coherence_of_hybridity,
    'Does the ''woman/female'' category retain sufficient conceptual coherence when its definition shifts by context, or does this hybridity fundamentally undermine its meaning and utility?',
    'Philosophical and legal analysis of the logical consistency and practical implications of context-dependent definitions, particularly in areas where contexts overlap or are contested.',
    'If conceptual coherence is severely undermined, the framework''s legitimacy erodes, increasing resistance and potentially reclassifying it as a Piton (if its function atrophies) or a Snare (if it becomes purely extractive through confusion).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(conceptual_coherence_of_hybridity, conceptual, 'Examining the logical and practical coherence of a context-dependent definition of ''woman/female''.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(woman_female_category__hybrid_contextual_reading, 2015, 2045).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(woma_tr_t2015, woman_female_category__hybrid_contextual_reading, theater_ratio, 2015, 0.2).
narrative_ontology:measurement(woma_tr_t2020, woman_female_category__hybrid_contextual_reading, theater_ratio, 2020, 0.22).
narrative_ontology:measurement(woma_tr_t2025, woman_female_category__hybrid_contextual_reading, theater_ratio, 2025, 0.25).
narrative_ontology:measurement(woma_tr_t2030, woman_female_category__hybrid_contextual_reading, theater_ratio, 2030, 0.28).
narrative_ontology:measurement(woma_tr_t2035, woman_female_category__hybrid_contextual_reading, theater_ratio, 2035, 0.3).
narrative_ontology:measurement(woma_tr_t2045, woman_female_category__hybrid_contextual_reading, theater_ratio, 2045, 0.32).

% Extraction over time
narrative_ontology:measurement(woma_be_t2015, woman_female_category__hybrid_contextual_reading, base_extractiveness, 2015, 0.38).
narrative_ontology:measurement(woma_be_t2020, woman_female_category__hybrid_contextual_reading, base_extractiveness, 2020, 0.42).
narrative_ontology:measurement(woma_be_t2025, woman_female_category__hybrid_contextual_reading, base_extractiveness, 2025, 0.45).
narrative_ontology:measurement(woma_be_t2030, woman_female_category__hybrid_contextual_reading, base_extractiveness, 2030, 0.48).
narrative_ontology:measurement(woma_be_t2035, woman_female_category__hybrid_contextual_reading, base_extractiveness, 2035, 0.5).
narrative_ontology:measurement(woma_be_t2045, woman_female_category__hybrid_contextual_reading, base_extractiveness, 2045, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(woma_su_t2015, woman_female_category__hybrid_contextual_reading, suppression_requirement, 2015, 0.48).
narrative_ontology:measurement(woma_su_t2020, woman_female_category__hybrid_contextual_reading, suppression_requirement, 2020, 0.52).
narrative_ontology:measurement(woma_su_t2025, woman_female_category__hybrid_contextual_reading, suppression_requirement, 2025, 0.55).
narrative_ontology:measurement(woma_su_t2030, woman_female_category__hybrid_contextual_reading, suppression_requirement, 2030, 0.58).
narrative_ontology:measurement(woma_su_t2035, woman_female_category__hybrid_contextual_reading, suppression_requirement, 2035, 0.6).
narrative_ontology:measurement(woma_su_t2045, woman_female_category__hybrid_contextual_reading, suppression_requirement, 2045, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(woman_female_category__hybrid_contextual_reading, identity_coordination).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
