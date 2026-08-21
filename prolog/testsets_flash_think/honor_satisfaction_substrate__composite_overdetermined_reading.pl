% ============================================================================
% CONSTRAINT STORY: honor_satisfaction_substrate__composite_overdetermined_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_honor_satisfaction_substrate__composite_overdetermined_reading, []).

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
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
 *   constraint_id: honor_satisfaction_substrate__composite_overdetermined_reading
 *   human_readable: Honor Satisfaction Substrate (Composite Overdetermined Decline)
 *   domain: historical_sociology/cultural_anthropology/legal_history
 *
 * SUMMARY:
 *   This constraint story analyzes the 'honor satisfaction substrate' as a
 *   social system that underpinned practices like dueling, focusing on its
 *   decline during the 18th and 19th centuries. This reading posits that the
 *   decline was 'overdetermined' by two causally entangled forces: exogenous
 *   suppression (legal prohibitions, institutional barriers) and endogenous
 *   delegitimation (a transformation of the honor code itself, shifting
 *   towards 'cultures of dignity'). The constraint is modeled as a Tangled
 *   Rope, reflecting its dual function of coordinating social status while
 *   extracting high costs, with metrics showing its degradation over time.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(honor_satisfaction_substrate__composite_overdetermined_reading, 0.4).
domain_priors:suppression_score(honor_satisfaction_substrate__composite_overdetermined_reading, 0.6).
domain_priors:theater_ratio(honor_satisfaction_substrate__composite_overdetermined_reading, 0.7).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(honor_satisfaction_substrate__composite_overdetermined_reading, extractiveness, 0.4).
narrative_ontology:constraint_metric(honor_satisfaction_substrate__composite_overdetermined_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(honor_satisfaction_substrate__composite_overdetermined_reading, theater_ratio, 0.7).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(honor_satisfaction_substrate__composite_overdetermined_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(honor_satisfaction_substrate__composite_overdetermined_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(honor_satisfaction_substrate__composite_overdetermined_reading, tangled_rope).
narrative_ontology:human_readable(honor_satisfaction_substrate__composite_overdetermined_reading, "Honor Satisfaction Substrate (Composite Overdetermined Decline)").
narrative_ontology:topic_domain(honor_satisfaction_substrate__composite_overdetermined_reading, "historical_sociology/cultural_anthropology/legal_history").

domain_priors:requires_active_enforcement(honor_satisfaction_substrate__composite_overdetermined_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(honor_satisfaction_substrate__composite_overdetermined_reading, '653e83e1-a5ef-46e9-bf3d-8e47e99a3be2').
narrative_ontology:cs_kernel_codification('653e83e1-a5ef-46e9-bf3d-8e47e99a3be2', implicit).
narrative_ontology:cs_authority_grounding('653e83e1-a5ef-46e9-bf3d-8e47e99a3be2', practice).
narrative_ontology:cs_interpretation_layer_present('653e83e1-a5ef-46e9-bf3d-8e47e99a3be2').
narrative_ontology:cs_reading_relation('653e83e1-a5ef-46e9-bf3d-8e47e99a3be2', honor_satisfaction_substrate__practice_decline_reading, coexists_with).
narrative_ontology:cs_reading_relation('653e83e1-a5ef-46e9-bf3d-8e47e99a3be2', honor_satisfaction_substrate__cultural_contraction_reading, coexists_with).
narrative_ontology:cs_axiom('653e83e1-a5ef-46e9-bf3d-8e47e99a3be2', foundational, honor_demands_satisfaction_by_force).
narrative_ontology:cs_axiom_status(honor_demands_satisfaction_by_force, overridden).
narrative_ontology:cs_axiom_grounding('653e83e1-a5ef-46e9-bf3d-8e47e99a3be2', honor_demands_satisfaction_by_force, conventional).
narrative_ontology:cs_axiom('653e83e1-a5ef-46e9-bf3d-8e47e99a3be2', foundational, legitimacy_is_socially_constructed_and_fragile).
narrative_ontology:cs_axiom_status(legitimacy_is_socially_constructed_and_fragile, holdable).
narrative_ontology:cs_axiom_grounding('653e83e1-a5ef-46e9-bf3d-8e47e99a3be2', legitimacy_is_socially_constructed_and_fragile, empirically_contingent).
narrative_ontology:cs_reference_frame('653e83e1-a5ef-46e9-bf3d-8e47e99a3be2', traditional_honor_code_supremacy).
narrative_ontology:cs_drift_state('653e83e1-a5ef-46e9-bf3d-8e47e99a3be2', late_19th_century_decline, gap(authority_erosion, severe, true)).
narrative_ontology:cs_created_at('653e83e1-a5ef-46e9-bf3d-8e47e99a3be2', '').
narrative_ontology:cs_kernel_id(honor_satisfaction_substrate__composite_overdetermined_reading, honor_satisfaction_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(honor_satisfaction_substrate__composite_overdetermined_reading, aristocratic_elites).
narrative_ontology:constraint_victim(honor_satisfaction_substrate__composite_overdetermined_reading, individuals_of_honor).
narrative_ontology:constraint_victim(honor_satisfaction_substrate__composite_overdetermined_reading, lower_social_strata).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Historically benefited from the honor code as a mechanism for maintaining social status and resolving disputes. As the code declined, they faced pressure to abandon dueling but often sought alternative means of honor satisfaction.
narrative_ontology:constraint_stakeholder(honor_satisfaction_substrate__composite_overdetermined_reading, aristocratic_elites, agenda_setter,
    institutional, generational, constrained, national).

% Were bound by the honor code's demands, including the risk of dueling, to maintain their social standing. Their identity was often fused with adherence to these norms, making exit difficult even as the system delegitimized.
narrative_ontology:constraint_stakeholder(honor_satisfaction_substrate__composite_overdetermined_reading, individuals_of_honor, payer,
    moderate, biographical, identity_locked, local).

% Actively suppressed dueling through legal prohibitions and institutional sanctions, contributing to the exogenous decline of the honor satisfaction substrate. They benefited from the assertion of state monopoly on violence.
narrative_ontology:constraint_stakeholder(honor_satisfaction_substrate__composite_overdetermined_reading, legal_authorities, agenda_setter,
    institutional, generational, analytical, national).

% Advocated for a shift from 'cultures of honor' to 'cultures of dignity,' challenging the internal legitimacy of the honor code and contributing to its endogenous delegitimation. They sought to replace violent dispute resolution with legal and moral frameworks.
narrative_ontology:constraint_stakeholder(honor_satisfaction_substrate__composite_overdetermined_reading, cultural_reformers, observer,
    organized, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(honor_satisfaction_substrate__composite_overdetermined_reading, cultural_reformers, agenda_setter).

% Were largely excluded from the formal mechanisms of the honor code but often suffered its consequences indirectly, such as violence or social instability. Their voices were not part of the debate over its legitimacy.
narrative_ontology:constraint_stakeholder(honor_satisfaction_substrate__composite_overdetermined_reading, lower_social_strata, excluded,
    powerless, immediate, trapped, local).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(honor_satisfaction_substrate__composite_overdetermined_reading, aristocratic_elites).
narrative_ontology:fixing_cost_class(honor_satisfaction_substrate__composite_overdetermined_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provided a structured, albeit violent, means for elites to maintain social status, resolve perceived insults, and enforce social norms within a specific honor-bound society.
% TRANSFER_FUNCTION: Transferred social capital and legitimacy to those who successfully navigated its demands, while extracting personal risk, social conformity, and sometimes life from participants. It also transferred authority over dispute resolution from individuals to the collective honor code.
% ABSENT_VOICES: Women, lower classes, and those who rejected violence on moral or religious grounds were largely excluded from the formal mechanisms of the honor code. They would have argued for alternative, non-violent forms of dispute resolution and a redefinition of social worth.
% DISAPPEARANCE_RATIONALE: The disappearance of the honor satisfaction substrate fundamentally altered elite social interaction, dispute resolution, and self-perception. It paved the way for the rise of legal frameworks and a shift towards individual dignity as a basis for social standing, reorganizing a significant part of social life.
% FOUNDING_PROBLEM: To establish and maintain a clear social hierarchy and a mechanism for resolving disputes among elites, ensuring personal and family honor was upheld in societies where state legal authority was often weak or inaccessible for such matters.
% FOUNDING_PROBLEM_CORROBORATION: Historians and cultural anthropologists widely attest to the historical function of honor codes and their subsequent transformation, drawing on extensive primary sources (e.g., dueling codes, personal correspondence, legal records) and sociological analysis. This corroboration comes from academic research outside the direct beneficiaries of the honor system.
narrative_ontology:disappearance_verdict(honor_satisfaction_substrate__composite_overdetermined_reading, world_rearranges).
narrative_ontology:founding_problem_status(honor_satisfaction_substrate__composite_overdetermined_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(honor_satisfaction_substrate__composite_overdetermined_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(honor_satisfaction_substrate__composite_overdetermined_reading, 'none', 1).
narrative_ontology:epsilon_provenance(honor_satisfaction_substrate__composite_overdetermined_reading, 0.4, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(honor_satisfaction_substrate__composite_overdetermined_reading_tests).
:- end_tests(honor_satisfaction_substrate__composite_overdetermined_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The honor satisfaction substrate, when active, was a Tangled Rope: it coordinated social status and dispute resolution for elites (beneficiaries) but extracted significant personal risk and conformity (victims). Its decline is reflected in decreasing extractiveness and suppression (as its power waned) and increasing theater ratio (as adherence became more performative). Resistance also increased as external forces challenged it and internal beliefs eroded. The 'overdetermined' nature means both external enforcement and internal cultural shifts contributed to its unraveling, making its persistence increasingly difficult.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the aristocratic elites, the decline of the honor code was an erosion of a vital social mechanism. From the perspective of cultural reformers and legal authorities, it was a necessary evolution towards a more civilized society. The engine's per-seat classification would reflect these divergent experiences of the constraint's decline.
 *
 * DIRECTIONALITY LOGIC:
 *   Aristocratic elites, as agenda-setters, initially benefited from the system's ability to maintain their status. Individuals of honor were payers, bound by identity-locked exit options due to social pressure. Legal authorities became agenda-setters for suppression, benefiting from the assertion of state power. Cultural reformers acted as observers and agenda-setters for delegitimation. Lower social strata were excluded, bearing indirect costs without formal participation.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    causal_pathway_independence,
    'To what extent were the exogenous suppression and endogenous delegitimation truly non-independent, and what were the specific causal links between them?',
    'Detailed historical-sociological analysis tracing specific instances where legal changes influenced cultural norms, or where cultural shifts enabled legal enforcement, using counterfactual reasoning.',
    'If the pathways were more independent, the decline might be better understood as two distinct forces acting in parallel. If highly interdependent, it strengthens the ''overdetermined'' composite reading, emphasizing the systemic nature of the decline.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(causal_pathway_independence, empirical, 'Assessing the causal entanglement of external and internal forces in the decline of honor codes.').

omega_variable(
    honor_code_definition_ambiguity,
    'What constitutes ''honor code transformation'' versus its complete replacement by a ''culture of dignity''?',
    'Conceptual analysis of historical texts and social practices to delineate the precise boundaries and overlapping elements of ''honor'' and ''dignity'' in different periods and social strata.',
    'If transformation implies a continuous evolution, the ''tangled_rope'' classification holds. If it''s a complete replacement, the original constraint might be considered ''dead'' earlier, with a new ''rope'' (dignity) emerging.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(honor_code_definition_ambiguity, conceptual, 'Defining the threshold between honor code transformation and replacement.').

omega_variable(
    reading_identification,
    'Is this constraint a genuine ''composite_overdetermined_reading'' of the honor_satisfaction_substrate kernel, or is it better understood as a blend of the ''practice_decline_reading'' and ''cultural_contraction_reading''?',
    'Analysis of the specific historical evidence and theoretical arguments that support the claim of ''non-independent causal pathways'' as a distinct explanatory framework, rather than a mere summation of other factors.',
    'If the composite reading is not structurally distinct, it might be decomposed into two separate, linked constraints, each representing one of the simpler sibling readings. If it holds, it validates the framework''s ability to model complex, multi-causal constraints.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_identification, conceptual, 'Verifying the distinctness of the composite_overdetermined_reading as a kernel interpretation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(honor_satisfaction_substrate__composite_overdetermined_reading, 1700, 1900).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hono_tr_t1700, honor_satisfaction_substrate__composite_overdetermined_reading, theater_ratio, 1700, 0.1).
narrative_ontology:measurement(hono_tr_t1725, honor_satisfaction_substrate__composite_overdetermined_reading, theater_ratio, 1725, 0.15).
narrative_ontology:measurement(hono_tr_t1750, honor_satisfaction_substrate__composite_overdetermined_reading, theater_ratio, 1750, 0.25).
narrative_ontology:measurement(hono_tr_t1775, honor_satisfaction_substrate__composite_overdetermined_reading, theater_ratio, 1775, 0.35).
narrative_ontology:measurement(hono_tr_t1800, honor_satisfaction_substrate__composite_overdetermined_reading, theater_ratio, 1800, 0.45).
narrative_ontology:measurement(hono_tr_t1825, honor_satisfaction_substrate__composite_overdetermined_reading, theater_ratio, 1825, 0.55).
narrative_ontology:measurement(hono_tr_t1850, honor_satisfaction_substrate__composite_overdetermined_reading, theater_ratio, 1850, 0.65).
narrative_ontology:measurement(hono_tr_t1875, honor_satisfaction_substrate__composite_overdetermined_reading, theater_ratio, 1875, 0.68).
narrative_ontology:measurement(hono_tr_t1900, honor_satisfaction_substrate__composite_overdetermined_reading, theater_ratio, 1900, 0.7).

% Extraction over time
narrative_ontology:measurement(hono_be_t1700, honor_satisfaction_substrate__composite_overdetermined_reading, base_extractiveness, 1700, 0.8).
narrative_ontology:measurement(hono_be_t1725, honor_satisfaction_substrate__composite_overdetermined_reading, base_extractiveness, 1725, 0.75).
narrative_ontology:measurement(hono_be_t1750, honor_satisfaction_substrate__composite_overdetermined_reading, base_extractiveness, 1750, 0.68).
narrative_ontology:measurement(hono_be_t1775, honor_satisfaction_substrate__composite_overdetermined_reading, base_extractiveness, 1775, 0.6).
narrative_ontology:measurement(hono_be_t1800, honor_satisfaction_substrate__composite_overdetermined_reading, base_extractiveness, 1800, 0.55).
narrative_ontology:measurement(hono_be_t1825, honor_satisfaction_substrate__composite_overdetermined_reading, base_extractiveness, 1825, 0.5).
narrative_ontology:measurement(hono_be_t1850, honor_satisfaction_substrate__composite_overdetermined_reading, base_extractiveness, 1850, 0.45).
narrative_ontology:measurement(hono_be_t1875, honor_satisfaction_substrate__composite_overdetermined_reading, base_extractiveness, 1875, 0.42).
narrative_ontology:measurement(hono_be_t1900, honor_satisfaction_substrate__composite_overdetermined_reading, base_extractiveness, 1900, 0.4).

% Suppression requirement over time
narrative_ontology:measurement(hono_su_t1700, honor_satisfaction_substrate__composite_overdetermined_reading, suppression_requirement, 1700, 0.5).
narrative_ontology:measurement(hono_su_t1725, honor_satisfaction_substrate__composite_overdetermined_reading, suppression_requirement, 1725, 0.55).
narrative_ontology:measurement(hono_su_t1750, honor_satisfaction_substrate__composite_overdetermined_reading, suppression_requirement, 1750, 0.65).
narrative_ontology:measurement(hono_su_t1775, honor_satisfaction_substrate__composite_overdetermined_reading, suppression_requirement, 1775, 0.75).
narrative_ontology:measurement(hono_su_t1800, honor_satisfaction_substrate__composite_overdetermined_reading, suppression_requirement, 1800, 0.8).
narrative_ontology:measurement(hono_su_t1825, honor_satisfaction_substrate__composite_overdetermined_reading, suppression_requirement, 1825, 0.78).
narrative_ontology:measurement(hono_su_t1850, honor_satisfaction_substrate__composite_overdetermined_reading, suppression_requirement, 1850, 0.7).
narrative_ontology:measurement(hono_su_t1875, honor_satisfaction_substrate__composite_overdetermined_reading, suppression_requirement, 1875, 0.65).
narrative_ontology:measurement(hono_su_t1900, honor_satisfaction_substrate__composite_overdetermined_reading, suppression_requirement, 1900, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(honor_satisfaction_substrate__composite_overdetermined_reading, identity_coordination).
narrative_ontology:affects_constraint(honor_satisfaction_substrate__composite_overdetermined_reading, honor_satisfaction_substrate__practice_decline_reading).
narrative_ontology:affects_constraint(honor_satisfaction_substrate__composite_overdetermined_reading, honor_satisfaction_substrate__cultural_contraction_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'honor_satisfaction_substrate' kernel. This 'composite_overdetermined_reading' posits that the decline of dueling was due to both exogenous suppression and endogenous delegitimation, with entangled causal pathways, distinguishing it from readings that emphasize only one factor.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
