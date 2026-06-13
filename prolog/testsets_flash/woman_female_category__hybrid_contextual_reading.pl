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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
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
 *   constraint_id: woman_female_category__hybrid_contextual_reading
 *   human_readable: Hybrid Contextual Reading of Woman/Female Category
 *   domain: political_philosophy/bioethics/gender_studies/law
 *
 * SUMMARY:
 *   This constraint represents a 'hybrid contextual' reading of the
 *   categories 'woman' and 'female', where biological sex is prioritized in
 *   contexts like medicine, sports, and safety, while gender identity is
 *   prioritized for social and legal recognition. This reading attempts to
 *   navigate the conflict between the 'sex_biology_reading' and
 *   'gender_identity_reading' by assigning different criteria based on the
 *   perceived salience of biological sex versus social identity in specific
 *   domains. It is a contested approach, often leading to different groups
 *   feeling subordinated depending on the context.
 *
 * KEY AGENTS:
 *   - institutional_actors_seeking_conflict_minimization: Primary beneficiary (institutional/arbitrage) — benefits from reduced direct conflict, but bears costs of ongoing ambiguity.
 *   - transgender_women_in_sex_segregated_spaces: Primary victim (powerless/constrained) — bears exclusion in contexts where sex is prioritized.
 *   - cisgender_women_in_gender_identity_contexts: Primary victim (powerless/constrained) — bears subordination in contexts where gender identity is prioritized.
 *   - legal_scholars_and_ethicists: Analytical observer (analytical/analytical) — analyzes the coherence and ethical implications of the hybrid approach.
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
narrative_ontology:constraint_metric(woman_female_category__hybrid_contextual_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(woman_female_category__hybrid_contextual_reading, tangled_rope).
narrative_ontology:human_readable(woman_female_category__hybrid_contextual_reading, "Hybrid Contextual Reading of Woman/Female Category").
narrative_ontology:topic_domain(woman_female_category__hybrid_contextual_reading, "political_philosophy/bioethics/gender_studies/law").

domain_priors:requires_active_enforcement(woman_female_category__hybrid_contextual_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(woman_female_category__hybrid_contextual_reading, '67256707-24fb-48be-9bc8-2f7867fa7460').
narrative_ontology:cs_kernel_codification('67256707-24fb-48be-9bc8-2f7867fa7460', distributed).
narrative_ontology:cs_authority_grounding('67256707-24fb-48be-9bc8-2f7867fa7460', distributed).
narrative_ontology:cs_reading_relation('67256707-24fb-48be-9bc8-2f7867fa7460', woman_female_category__sex_biology_reading, coexists_with).
narrative_ontology:cs_reading_relation('67256707-24fb-48be-9bc8-2f7867fa7460', woman_female_category__gender_identity_reading, coexists_with).
narrative_ontology:cs_axiom('67256707-24fb-48be-9bc8-2f7867fa7460', foundational, contextual_salience_determines_category).
narrative_ontology:cs_axiom_status(contextual_salience_determines_category, holdable).
narrative_ontology:cs_axiom_grounding('67256707-24fb-48be-9bc8-2f7867fa7460', contextual_salience_determines_category, conventional).
narrative_ontology:cs_axiom('67256707-24fb-48be-9bc8-2f7867fa7460', secondary, conflict_minimization_is_primary_goal).
narrative_ontology:cs_axiom_status(conflict_minimization_is_primary_goal, holdable).
narrative_ontology:cs_axiom_grounding('67256707-24fb-48be-9bc8-2f7867fa7460', conflict_minimization_is_primary_goal, instrumental).
narrative_ontology:cs_reference_frame('67256707-24fb-48be-9bc8-2f7867fa7460', pragmatic_social_coexistence).
narrative_ontology:cs_drift_state('67256707-24fb-48be-9bc8-2f7867fa7460', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('67256707-24fb-48be-9bc8-2f7867fa7460', '').
narrative_ontology:cs_kernel_id(woman_female_category__hybrid_contextual_reading, woman_female_category).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(woman_female_category__hybrid_contextual_reading, institutional_actors_seeking_conflict_minimization).
narrative_ontology:constraint_victim(woman_female_category__hybrid_contextual_reading, transgender_women_in_sex_segregated_spaces).
narrative_ontology:constraint_victim(woman_female_category__hybrid_contextual_reading, cisgender_women_in_gender_identity_contexts).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These actors (e.g., government agencies, sports federations, medical bodies) adopt the hybrid reading to reduce direct legal and social conflict, aiming for a 'workable compromise.' They benefit from avoiding definitive stances that would alienate large constituencies, but bear the cost of ongoing ambiguity and criticism from both sides.
narrative_ontology:constraint_stakeholder(woman_female_category__hybrid_contextual_reading, institutional_actors_seeking_conflict_minimization, beneficiary,
    institutional, generational, arbitrage, national).

% These individuals experience exclusion or conditional inclusion in contexts (e.g., sports, some medical settings, certain safety spaces) where biological sex is prioritized over their gender identity. They bear the cost of not having their identity fully recognized across all domains.
narrative_ontology:constraint_stakeholder(woman_female_category__hybrid_contextual_reading, transgender_women_in_sex_segregated_spaces, payer,
    powerless, biographical, constrained, local).

% These individuals experience their sex-based claims (e.g., for single-sex spaces, data collection) being subordinated in contexts where gender identity is prioritized (e.g., some legal recognition, social spaces). They bear the cost of not having their sex-based category fully recognized across all domains.
narrative_ontology:constraint_stakeholder(woman_female_category__hybrid_contextual_reading, cisgender_women_in_gender_identity_contexts, payer,
    powerless, biographical, constrained, local).

% These experts analyze the coherence, ethical implications, and practical effects of the hybrid contextual reading. They contribute to the discourse but do not directly enforce or suffer from the constraint's operation.
narrative_ontology:constraint_stakeholder(woman_female_category__hybrid_contextual_reading, legal_scholars_and_ethicists, observer,
    analytical, generational, analytical, global).

% These groups advocate for full recognition of gender identity across all contexts, viewing the hybrid approach as a partial and insufficient compromise that still marginalizes transgender individuals in certain domains. They are often excluded from the final decision-making on contextual boundaries.
narrative_ontology:constraint_stakeholder(woman_female_category__hybrid_contextual_reading, advocacy_groups_for_transgender_rights, excluded,
    organized, generational, constrained, national).

% These groups advocate for the primacy of biological sex in all contexts, viewing the hybrid approach as undermining the rights and protections of cisgender women in domains where gender identity is prioritized. They are often excluded from the final decision-making on contextual boundaries.
narrative_ontology:constraint_stakeholder(woman_female_category__hybrid_contextual_reading, advocacy_groups_for_sex_based_rights, excluded,
    organized, generational, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(woman_female_category__hybrid_contextual_reading, institutional_actors_seeking_conflict_minimization).
narrative_ontology:fixing_cost_class(woman_female_category__hybrid_contextual_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To provide a framework for categorizing individuals as 'woman' or 'female' that attempts to accommodate both biological sex and gender identity, by assigning different criteria based on the specific context (e.g., medical, sports, social, legal).
% TRANSFER_FUNCTION: Transfers the burden of navigating conflicting categorical claims from institutional decision-makers to individuals, who must accept different categorizations depending on the context. It also transfers social and legal recognition in some contexts, while withholding it in others.
% ABSENT_VOICES: Advocacy groups from both the 'sex_biology_reading' and 'gender_identity_reading' perspectives are often excluded from the final adjudication of contextual boundaries, as their maximalist positions are seen as undermining the 'compromise' of the hybrid approach. They would argue for a consistent application of their preferred criteria across all contexts.
% DISAPPEARANCE_RATIONALE: If this hybrid contextual reading vanished, institutions would be forced to adopt either a purely sex-based or purely gender-identity-based categorization, or to create entirely new frameworks. This would lead to significant legal challenges, social upheaval, and a complete reorganization of policies related to gender and sex across various domains.
% FOUNDING_PROBLEM: The problem of reconciling conflicting claims regarding the definition of 'woman' and 'female' in an increasingly diverse and rights-conscious society, particularly where biological sex and gender identity diverge, leading to social and legal disputes.
% FOUNDING_PROBLEM_CORROBORATION: Legal scholars, ethicists, and institutional policy-makers (outside of those directly benefiting from the specific compromise) corroborate that the underlying conflict remains live, even if the hybrid reading attempts to manage it. The ongoing debates and legal challenges from both sides attest to the persistence of the problem.
narrative_ontology:disappearance_verdict(woman_female_category__hybrid_contextual_reading, world_rearranges).
narrative_ontology:founding_problem_status(woman_female_category__hybrid_contextual_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(woman_female_category__hybrid_contextual_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(woman_female_category__hybrid_contextual_reading, 'none', 1).

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
 *   The extractiveness (0.45) is moderate because while it attempts to coordinate, it does so by subordinating one group's claims in certain contexts, leading to real costs for those affected. Suppression (0.6) is significant as it requires active enforcement of contextual boundaries and often silences dissenting voices from both sides when their preferred categorization is not applied. Theater ratio (0.2) is low, as the contextual distinctions are genuinely applied, though the underlying justification for the boundaries is often debated. Accessibility collapse (0.4) is moderate, as alternatives (pure sex-based or pure gender-identity-based categorization) are well-understood but actively suppressed in favor of the hybrid approach. Resistance (0.5) is also moderate, as both cisgender and transgender women's advocacy groups resist aspects of this hybrid approach.
 *
 * PERSPECTIVAL GAP:
 *   Institutional actors view this as a pragmatic solution to an intractable conflict, minimizing their own exposure to legal and social backlash. However, both transgender women and cisgender women experience this as a constraint that selectively invalidates their claims depending on the context, leading to feelings of being victimized or having their identity subordinated. The engine will compute different classifications for the institutional beneficiaries (closer to Rope/Scaffold) versus the victim groups (closer to Snare/Tangled Rope).
 *
 * DIRECTIONALITY LOGIC:
 *   Institutional actors seeking conflict minimization are beneficiaries (d=0.0-0.2) as this reading reduces direct, high-stakes conflict for them, even if it creates diffuse costs. Transgender women in sex-segregated spaces are victims (d=0.8-1.0) when their gender identity is not recognized in contexts they deem relevant. Cisgender women in gender identity contexts are also victims (d=0.8-1.0) when their sex-based claims are subordinated in contexts they deem relevant. The directionality shifts for these victim groups depending on the specific context being analyzed, but overall, they bear the costs of the compromise.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is a Tangled Rope because it genuinely attempts to coordinate conflicting claims (providing a framework for different contexts) but does so with significant asymmetric extraction, where both groups of women are victims in different contexts. It requires active enforcement to maintain the contextual boundaries and manage the ongoing resistance. It avoids being a Snare because there is a genuine, albeit imperfect, coordination function in attempting to provide a workable framework for diverse social and legal situations. It avoids being a Rope because of the significant and contested extraction from both victim groups.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    context_boundary_ambiguity,
    'Where are the precise boundaries between ''medical/sports/safety'' contexts and ''social/legal recognition'' contexts, and who adjudicates these boundaries?',
    'Case law establishing precedents for specific contexts, or legislative action defining contextual applicability.',
    'Ambiguity allows for arbitrary application, increasing extraction for whichever group is subordinated in a given context. Clear boundaries would reduce this ambiguity and stabilize the victim set.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(context_boundary_ambiguity, conceptual, 'Ambiguity in contextual boundaries for category application.').

omega_variable(
    natural_vs_constructed_context,
    'Are the distinctions between ''medical/sports/safety'' and ''social/legal'' contexts inherent and natural, or are they socially constructed and thus revisable?',
    'Philosophical analysis of the nature of categories and empirical study of the impact of different categorizations across cultures and legal systems.',
    'If natural, the hybrid reading is a more robust coordination mechanism. If constructed, the specific contextual assignments are open to challenge and re-negotiation, potentially shifting the beneficiary/victim structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_vs_constructed_context, conceptual, 'Whether contextual distinctions are natural or constructed.').

omega_variable(
    reading_as_conflict_minimization,
    'Is this hybrid reading a genuine attempt at equitable coordination, or primarily a mechanism for institutional actors to minimize conflict and avoid taking a definitive stance?',
    'Analysis of institutional decision-making processes, stakeholder engagement, and the distribution of costs/benefits over time. If the primary outcome is reduced institutional friction rather than equitable outcomes for affected groups, it suggests conflict minimization as the driving force.',
    'If primarily conflict minimization, the constraint''s coordination function is weaker, and its extractive component (from both victim groups) is higher, reclassifying it closer to a Snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_as_conflict_minimization, empirical, 'Motivation for the hybrid reading: genuine coordination vs. conflict minimization.').


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
narrative_ontology:measurement(woma_be_t0, woman_female_category__hybrid_contextual_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(woma_be_t5, woman_female_category__hybrid_contextual_reading, base_extractiveness, 5, 0.4).
narrative_ontology:measurement(woma_be_t10, woman_female_category__hybrid_contextual_reading, base_extractiveness, 10, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(woma_su_t0, woman_female_category__hybrid_contextual_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(woma_su_t5, woman_female_category__hybrid_contextual_reading, suppression_requirement, 5, 0.55).
narrative_ontology:measurement(woma_su_t10, woman_female_category__hybrid_contextual_reading, suppression_requirement, 10, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(woman_female_category__hybrid_contextual_reading, identity_coordination).
narrative_ontology:affects_constraint(woman_female_category__hybrid_contextual_reading, woman_female_category__sex_biology_reading).
narrative_ontology:affects_constraint(woman_female_category__hybrid_contextual_reading, woman_female_category__gender_identity_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'woman_female_category' kernel, each representing a distinct structural claim about category membership. This hybrid reading attempts to mediate between the 'sex_biology_reading' and 'gender_identity_reading' by applying different criteria in different contexts.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
