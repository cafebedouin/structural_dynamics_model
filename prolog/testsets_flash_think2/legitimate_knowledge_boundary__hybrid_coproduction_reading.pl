% ============================================================================
% CONSTRAINT STORY: legitimate_knowledge_boundary__hybrid_coproduction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_legitimate_knowledge_boundary__hybrid_coproduction_reading, []).

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
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: legitimate_knowledge_boundary__hybrid_coproduction_reading
 *   human_readable: Hybrid Co-production Knowledge Boundary
 *   domain: epistemology/science_and_technology_studies/political_theory
 *
 * SUMMARY:
 *   This constraint defines legitimate knowledge as requiring the integration
 *   of methodological rigor (typically associated with academic expertise)
 *   and experiential validity (derived from lived experience), achieved
 *   through co-production processes. It is one reading of the broader
 *   'legitimate_knowledge_boundary' kernel, which is contested by readings
 *   emphasizing purely credentialed expertise or purely experiential
 *   pluralism. This reading aims to bridge the gap between these extremes but
 *   imposes significant structural requirements and costs on all
 *   participants.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(legitimate_knowledge_boundary__hybrid_coproduction_reading, 0.6).
domain_priors:suppression_score(legitimate_knowledge_boundary__hybrid_coproduction_reading, 0.7).
domain_priors:theater_ratio(legitimate_knowledge_boundary__hybrid_coproduction_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(legitimate_knowledge_boundary__hybrid_coproduction_reading, extractiveness, 0.6).
narrative_ontology:constraint_metric(legitimate_knowledge_boundary__hybrid_coproduction_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(legitimate_knowledge_boundary__hybrid_coproduction_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(legitimate_knowledge_boundary__hybrid_coproduction_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(legitimate_knowledge_boundary__hybrid_coproduction_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(legitimate_knowledge_boundary__hybrid_coproduction_reading, tangled_rope).
narrative_ontology:human_readable(legitimate_knowledge_boundary__hybrid_coproduction_reading, "Hybrid Co-production Knowledge Boundary").
narrative_ontology:topic_domain(legitimate_knowledge_boundary__hybrid_coproduction_reading, "epistemology/science_and_technology_studies/political_theory").

domain_priors:requires_active_enforcement(legitimate_knowledge_boundary__hybrid_coproduction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(legitimate_knowledge_boundary__hybrid_coproduction_reading, 'f45bb939-3ea2-4c51-be62-152a1d1e2701').
narrative_ontology:cs_kernel_codification('f45bb939-3ea2-4c51-be62-152a1d1e2701', formalized).
narrative_ontology:cs_authority_grounding('f45bb939-3ea2-4c51-be62-152a1d1e2701', practice).
narrative_ontology:cs_interpretation_layer_present('f45bb939-3ea2-4c51-be62-152a1d1e2701').
narrative_ontology:cs_reading_relation('f45bb939-3ea2-4c51-be62-152a1d1e2701', legitimate_knowledge_boundary__credentialed_expertise_reading, influences).
narrative_ontology:cs_reading_relation('f45bb939-3ea2-4c51-be62-152a1d1e2701', legitimate_knowledge_boundary__experiential_pluralism_reading, influences).
narrative_ontology:cs_axiom('f45bb939-3ea2-4c51-be62-152a1d1e2701', foundational, knowledge_is_co_produced).
narrative_ontology:cs_axiom_status(knowledge_is_co_produced, holdable).
narrative_ontology:cs_axiom_grounding('f45bb939-3ea2-4c51-be62-152a1d1e2701', knowledge_is_co_produced, conventional).
narrative_ontology:cs_axiom('f45bb939-3ea2-4c51-be62-152a1d1e2701', foundational, dual_validation_necessary).
narrative_ontology:cs_axiom_status(dual_validation_necessary, holdable).
narrative_ontology:cs_axiom_grounding('f45bb939-3ea2-4c51-be62-152a1d1e2701', dual_validation_necessary, instrumental).
narrative_ontology:cs_reference_frame('f45bb939-3ea2-4c51-be62-152a1d1e2701', integrated_knowledge_paradigm).
narrative_ontology:cs_drift_state('f45bb939-3ea2-4c51-be62-152a1d1e2701', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('f45bb939-3ea2-4c51-be62-152a1d1e2701', '').
narrative_ontology:cs_kernel_id(legitimate_knowledge_boundary__hybrid_coproduction_reading, legitimate_knowledge_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(legitimate_knowledge_boundary__hybrid_coproduction_reading, co_production_advocates).
narrative_ontology:constraint_beneficiary(legitimate_knowledge_boundary__hybrid_coproduction_reading, integrated_knowledge_users).
narrative_ontology:constraint_victim(legitimate_knowledge_boundary__hybrid_coproduction_reading, pure_credentialed_experts).
narrative_ontology:constraint_victim(legitimate_knowledge_boundary__hybrid_coproduction_reading, pure_experiential_advocates).
narrative_ontology:constraint_victim(legitimate_knowledge_boundary__hybrid_coproduction_reading, marginalized_knowledge_forms).
narrative_ontology:constraint_victim(legitimate_knowledge_boundary__hybrid_coproduction_reading, traditional_academic_institutions).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Actively promote and implement co-production methodologies, benefiting from the increased legitimacy and relevance of the knowledge produced. They invest in developing and institutionalizing these processes.
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__hybrid_coproduction_reading, co_production_advocates, agenda_setter,
    institutional, generational, constrained, global).

% Benefit from more robust, contextually relevant, and legitimate knowledge that addresses complex societal problems more effectively than siloed approaches. They seek out and apply this hybrid knowledge.
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__hybrid_coproduction_reading, integrated_knowledge_users, beneficiary,
    moderate, biographical, mobile, global).

% Are required to adapt their research methods, engage in participatory processes, and share epistemic authority with non-academic actors. This incurs costs in time, resources, and a shift in professional identity, as their traditional forms of validation are no longer sufficient.
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__hybrid_coproduction_reading, pure_credentialed_experts, payer,
    powerful, biographical, constrained, global).

% Are required to engage with formal methodological rigor, often needing to formalize or translate their lived experience into frameworks recognizable by academic partners. This can be a demanding process, requiring significant effort and adaptation.
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__hybrid_coproduction_reading, pure_experiential_advocates, payer,
    organized, biographical, constrained, global).

% Knowledge systems that do not easily fit the dual validation criteria (e.g., highly localized, non-codified, or non-Western epistemologies) are often excluded from 'legitimate' status, despite their intrinsic value, because they lack the resources or frameworks to engage in co-production as defined.
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__hybrid_coproduction_reading, marginalized_knowledge_forms, excluded,
    powerless, generational, trapped, local).

% Are pressured to adapt their structures, funding mechanisms, and reward systems to support co-production. This involves significant institutional change, resource reallocation, and a re-evaluation of traditional metrics of success, incurring substantial organizational costs.
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__hybrid_coproduction_reading, traditional_academic_institutions, agenda_setter,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(legitimate_knowledge_boundary__hybrid_coproduction_reading, traditional_academic_institutions, payer).

% Analyze the theoretical underpinnings, practical implementation, and societal impacts of hybrid co-production, assessing its effectiveness, ethical implications, and power dynamics.
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__hybrid_coproduction_reading, epistemology_scholars, observer,
    analytical, biographical, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Integrates diverse forms of knowledge (methodological rigor from academia and experiential validity from lived experience) to produce more robust, legitimate, and actionable insights, particularly for complex societal problems that require transdisciplinary approaches.
% TRANSFER_FUNCTION: Transfers epistemic authority, resources, and legitimacy from purely academic or purely experiential knowledge systems towards hybrid co-production models. This requires investment in new processes, infrastructure, and a re-evaluation of traditional validation mechanisms.
% ABSENT_VOICES: Knowledge systems that are neither purely academic nor easily integrated into co-production frameworks (e.g., highly localized indigenous knowledge systems without formal 'methodological rigor' as defined by Western science, or highly specialized academic fields with no clear 'experiential validity' component) are often marginalized or excluded from the 'legitimate knowledge' boundary.
% DISAPPEARANCE_RATIONALE: If the requirement for hybrid co-production vanished overnight, knowledge production would likely revert to more siloed, less integrated forms. This would lead to a resurgence of the legitimacy gap between academic expertise and lived experience, potentially resulting in less effective or less trusted solutions for complex problems, and a reorganization of funding and institutional priorities.
% FOUNDING_PROBLEM: Traditional knowledge production often suffered from a legitimacy gap, being perceived either as academically rigorous but socially irrelevant, or experientially rich but lacking generalizability or systematic validation. This led to a disconnect between expert knowledge and public trust/applicability.
% FOUNDING_PROBLEM_CORROBORATION: Advocates for participatory research, science communication scholars, and community-based organizations consistently attest to the ongoing need for integrated knowledge. They cite numerous examples of policy failures and public distrust stemming from disconnected expertise, supporting the claim that the founding problem remains active.
narrative_ontology:disappearance_verdict(legitimate_knowledge_boundary__hybrid_coproduction_reading, world_rearranges).
narrative_ontology:founding_problem_status(legitimate_knowledge_boundary__hybrid_coproduction_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(legitimate_knowledge_boundary__hybrid_coproduction_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(legitimate_knowledge_boundary__hybrid_coproduction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(legitimate_knowledge_boundary__hybrid_coproduction_reading, 0.6, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(legitimate_knowledge_boundary__hybrid_coproduction_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(legitimate_knowledge_boundary__hybrid_coproduction_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(legitimate_knowledge_boundary__hybrid_coproduction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The constraint is classified as a Tangled Rope because it genuinely coordinates different forms of knowledge for a common good (solving complex problems) but does so with significant asymmetric extraction and active enforcement. Extractiveness (0.6) is moderate-high because it demands substantial adaptation and resource investment from both academic and experiential knowledge holders, and it excludes knowledge forms that cannot or will not engage in this dual validation. Suppression (0.7) is high because it actively enforces dual validation through funding mechanisms, publication standards, and institutional recognition, effectively suppressing alternatives. Theater ratio (0.2) is low-moderate, acknowledging that while the ideal of co-production is genuine, some implementations may become performative without true integration.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of co-production advocates, this constraint is a necessary Rope, solving a critical coordination problem in knowledge production. However, from the perspective of traditional experts or marginalized knowledge holders, it can feel like a Snare or Tangled Rope, imposing new, often burdensome, requirements and excluding valuable forms of knowledge that do not fit the co-production mold. The engine's computation of per-seat classification will reveal this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Co-production advocates and integrated knowledge users are beneficiaries, as they gain legitimacy and more effective knowledge. Pure credentialed experts, pure experiential advocates, and traditional academic institutions are payers, as they bear the costs of adaptation, resource reallocation, and shifts in epistemic authority. Marginalized knowledge forms are excluded, facing the highest barriers to legitimacy. The dual validation requirement means both 'sides' must adapt, creating a complex flow of costs and benefits.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    genuine_vs_performative_coproduction,
    'Is the integration of methodological rigor and experiential validity through co-production processes genuinely achieved, or is it often a performative exercise to legitimize pre-determined outcomes or maintain existing power structures?',
    'Longitudinal case studies of co-production projects, assessing changes in epistemic authority, resource allocation, and the substantive influence of diverse knowledge forms on outcomes, rather than just process adherence.',
    'If largely performative, the constraint''s effective extractiveness and theater_ratio are higher than measured, functioning more as a Snare that co-opts rather than integrates. If genuine, the coordination function is stronger, supporting a Tangled Rope or even Rope classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(genuine_vs_performative_coproduction, empirical, 'Assesses the authenticity of co-production''s integrative function.').

omega_variable(
    definition_of_rigor_and_validity,
    'How are ''methodological rigor'' and ''experiential validity'' actually defined and balanced in practice within co-production, and who holds the power to set these definitions?',
    'Discourse analysis of co-production guidelines and project evaluations, coupled with ethnographic studies of power dynamics within co-production teams, to identify whose definitions prevail.',
    'If definitions are primarily set by academic institutions, the constraint''s suppression of non-academic knowledge is higher. If definitions are genuinely co-created, the coordination function is more equitable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(definition_of_rigor_and_validity, conceptual, 'Examines the power dynamics in defining key terms of co-production.').

omega_variable(
    power_dynamics_in_coproduction,
    'Does co-production genuinely redistribute epistemic power, or does it merely co-opt marginalized voices into existing power structures, maintaining the dominance of academic institutions?',
    'Analysis of funding flows, publication credits, and decision-making authority within co-production projects, comparing outcomes to stated goals of equitable partnership.',
    'If power remains concentrated, the constraint functions more as a Snare, using the rhetoric of integration to extract legitimacy without genuine redistribution. If power is genuinely shared, it reinforces the Tangled Rope''s coordination aspect.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(power_dynamics_in_coproduction, empirical, 'Investigates the actual redistribution of epistemic power in co-production.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(legitimate_knowledge_boundary__hybrid_coproduction_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(legi_tr_t0, legitimate_knowledge_boundary__hybrid_coproduction_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(legi_tr_t5, legitimate_knowledge_boundary__hybrid_coproduction_reading, theater_ratio, 5, 0.13).
narrative_ontology:measurement(legi_tr_t10, legitimate_knowledge_boundary__hybrid_coproduction_reading, theater_ratio, 10, 0.17).
narrative_ontology:measurement(legi_tr_t15, legitimate_knowledge_boundary__hybrid_coproduction_reading, theater_ratio, 15, 0.19).
narrative_ontology:measurement(legi_tr_t20, legitimate_knowledge_boundary__hybrid_coproduction_reading, theater_ratio, 20, 0.2).

% Extraction over time
narrative_ontology:measurement(legi_be_t0, legitimate_knowledge_boundary__hybrid_coproduction_reading, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(legi_be_t5, legitimate_knowledge_boundary__hybrid_coproduction_reading, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(legi_be_t10, legitimate_knowledge_boundary__hybrid_coproduction_reading, base_extractiveness, 10, 0.55).
narrative_ontology:measurement(legi_be_t15, legitimate_knowledge_boundary__hybrid_coproduction_reading, base_extractiveness, 15, 0.58).
narrative_ontology:measurement(legi_be_t20, legitimate_knowledge_boundary__hybrid_coproduction_reading, base_extractiveness, 20, 0.6).

% Suppression requirement over time
narrative_ontology:measurement(legi_su_t0, legitimate_knowledge_boundary__hybrid_coproduction_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(legi_su_t5, legitimate_knowledge_boundary__hybrid_coproduction_reading, suppression_requirement, 5, 0.58).
narrative_ontology:measurement(legi_su_t10, legitimate_knowledge_boundary__hybrid_coproduction_reading, suppression_requirement, 10, 0.65).
narrative_ontology:measurement(legi_su_t15, legitimate_knowledge_boundary__hybrid_coproduction_reading, suppression_requirement, 15, 0.68).
narrative_ontology:measurement(legi_su_t20, legitimate_knowledge_boundary__hybrid_coproduction_reading, suppression_requirement, 20, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(legitimate_knowledge_boundary__hybrid_coproduction_reading, identity_coordination).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
