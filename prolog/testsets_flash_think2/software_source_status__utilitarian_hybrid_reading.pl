% ============================================================================
% CONSTRAINT STORY: software_source_status__utilitarian_hybrid_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_software_source_status__utilitarian_hybrid_reading, []).

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
 *   constraint_id: software_source_status__utilitarian_hybrid_reading
 *   human_readable: Utilitarian Hybrid Reading of Software Source Status
 *   domain: Software Engineering/Political Economy of Technology/Intellectual Property
 *
 * SUMMARY:
 *   This constraint represents the 'utilitarian hybrid' reading of software
 *   source status, which asserts that software licensing decisions should be
 *   guided by the maximization of aggregate societal welfare. This
 *   perspective acknowledges that both open-source and proprietary models can
 *   serve different contexts optimally, and therefore advocates for a
 *   flexible, evidence-based approach rather than a dogmatic adherence to one
 *   model. It functions as a guiding principle for policy-makers and
 *   developers, aiming to coordinate diverse interests towards a common goal
 *   of societal benefit.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(software_source_status__utilitarian_hybrid_reading, 0.45).
domain_priors:suppression_score(software_source_status__utilitarian_hybrid_reading, 0.3).
domain_priors:theater_ratio(software_source_status__utilitarian_hybrid_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(software_source_status__utilitarian_hybrid_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(software_source_status__utilitarian_hybrid_reading, suppression_requirement, 0.3).
narrative_ontology:constraint_metric(software_source_status__utilitarian_hybrid_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(software_source_status__utilitarian_hybrid_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(software_source_status__utilitarian_hybrid_reading, resistance, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(software_source_status__utilitarian_hybrid_reading, rope).
narrative_ontology:human_readable(software_source_status__utilitarian_hybrid_reading, "Utilitarian Hybrid Reading of Software Source Status").
narrative_ontology:topic_domain(software_source_status__utilitarian_hybrid_reading, "Software Engineering/Political Economy of Technology/Intellectual Property").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(software_source_status__utilitarian_hybrid_reading, '20df42a4-3a63-4cdd-8c13-7d5d2ce3f388').
narrative_ontology:cs_kernel_codification('20df42a4-3a63-4cdd-8c13-7d5d2ce3f388', distributed).
narrative_ontology:cs_authority_grounding('20df42a4-3a63-4cdd-8c13-7d5d2ce3f388', expertise).
narrative_ontology:cs_interpretation_layer_present('20df42a4-3a63-4cdd-8c13-7d5d2ce3f388').
narrative_ontology:cs_reading_relation('20df42a4-3a63-4cdd-8c13-7d5d2ce3f388', software_source_status__freedom_imperative_reading, coexists_with).
narrative_ontology:cs_reading_relation('20df42a4-3a63-4cdd-8c13-7d5d2ce3f388', software_source_status__pragmatic_development_reading, coexists_with).
narrative_ontology:cs_reading_relation('20df42a4-3a63-4cdd-8c13-7d5d2ce3f388', software_source_status__property_rights_reading, coexists_with).
narrative_ontology:cs_axiom('20df42a4-3a63-4cdd-8c13-7d5d2ce3f388', foundational, aggregate_welfare_maximization_is_primary).
narrative_ontology:cs_axiom_status(aggregate_welfare_maximization_is_primary, holdable).
narrative_ontology:cs_axiom_grounding('20df42a4-3a63-4cdd-8c13-7d5d2ce3f388', aggregate_welfare_maximization_is_primary, instrumental).
narrative_ontology:cs_axiom('20df42a4-3a63-4cdd-8c13-7d5d2ce3f388', secondary, context_dependent_licensing_optimality).
narrative_ontology:cs_axiom_status(context_dependent_licensing_optimality, holdable).
narrative_ontology:cs_axiom_grounding('20df42a4-3a63-4cdd-8c13-7d5d2ce3f388', context_dependent_licensing_optimality, empirically_contingent).
narrative_ontology:cs_reference_frame('20df42a4-3a63-4cdd-8c13-7d5d2ce3f388', optimal_societal_welfare).
narrative_ontology:cs_drift_state('20df42a4-3a63-4cdd-8c13-7d5d2ce3f388', contemporary_policy_debates, gap(stable, minor, true)).
narrative_ontology:cs_created_at('20df42a4-3a63-4cdd-8c13-7d5d2ce3f388', '').
narrative_ontology:cs_kernel_id(software_source_status__utilitarian_hybrid_reading, software_source_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(software_source_status__utilitarian_hybrid_reading, software_users).
narrative_ontology:constraint_beneficiary(software_source_status__utilitarian_hybrid_reading, innovators).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(software_source_status__utilitarian_hybrid_reading, software_developers).
narrative_ontology:constraint_victim(software_source_status__utilitarian_hybrid_reading, software_developers).
narrative_ontology:constraint_victim(software_source_status__utilitarian_hybrid_reading, proprietary_software_firms).
narrative_ontology:constraint_vindicates(software_source_status__utilitarian_hybrid_reading, consequentialist_ethics).
narrative_ontology:constraint_vindicates(software_source_status__utilitarian_hybrid_reading, economic_efficiency_theory).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Responsible for crafting and implementing policies that govern software licensing, aiming to maximize aggregate societal welfare. They interpret and apply the utilitarian hybrid principle to specific contexts.
narrative_ontology:constraint_stakeholder(software_source_status__utilitarian_hybrid_reading, policy_makers, agenda_setter,
    institutional, civilizational, analytical, global).

% Benefit from ecosystems optimized for innovation and access, but may bear costs or face restrictions from specific licensing choices made under the utilitarian principle. Their choices are guided by the perceived welfare outcomes.
narrative_ontology:constraint_stakeholder(software_source_status__utilitarian_hybrid_reading, software_developers, beneficiary,
    moderate, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(software_source_status__utilitarian_hybrid_reading, software_developers, payer).

% Are the ultimate beneficiaries of software licensing models that maximize aggregate welfare, gaining access to high-quality, innovative, and affordable software solutions. They experience the direct impact of licensing decisions.
narrative_ontology:constraint_stakeholder(software_source_status__utilitarian_hybrid_reading, software_users, beneficiary,
    organized, biographical, mobile, global).

% Advocate for open-source models, often arguing that they inherently maximize welfare through collaboration and access. They observe and critique policy decisions through this lens, influencing the debate.
narrative_ontology:constraint_stakeholder(software_source_status__utilitarian_hybrid_reading, open_source_advocates, observer,
    organized, generational, constrained, global).

% May bear costs if proprietary models are deemed suboptimal for aggregate welfare in certain contexts, leading to policy restrictions or competitive disadvantages. They seek to justify their models within a welfare framework.
narrative_ontology:constraint_stakeholder(software_source_status__utilitarian_hybrid_reading, proprietary_software_firms, payer,
    powerful, biographical, constrained, global).

% Provide the analytical framework for defining and measuring 'aggregate welfare' and 'optimal contexts.' They refine the utilitarian principle and evaluate its application, influencing policy makers.
narrative_ontology:constraint_stakeholder(software_source_status__utilitarian_hybrid_reading, economists_and_ethicists, observer,
    analytical, generational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates diverse software development and licensing models (open, proprietary, hybrid) to achieve optimal aggregate societal welfare, balancing innovation, access, security, and economic incentives across different contexts.
% TRANSFER_FUNCTION: Guides the allocation of intellectual property rights, development resources, and economic benefits in software creation, aiming to maximize overall societal gain rather than specific private or ideological interests.
% ABSENT_VOICES: Those who advocate for purely ideological positions (e.g., absolute software freedom or absolute intellectual property rights) without considering the empirical trade-offs and aggregate welfare outcomes. Their categorical claims are not directly addressed by this context-dependent optimization principle.
% DISAPPEARANCE_RATIONALE: If this guiding principle vanished, software licensing decisions would likely revert to more ideologically driven or purely self-interested positions, leading to suboptimal outcomes for society as a whole, increased conflict between different licensing models, and less effective policy-making in the technology sector.
% FOUNDING_PROBLEM: The historical and ongoing conflict between different software licensing ideologies (e.g., free software vs. proprietary software) leading to suboptimal outcomes, missed opportunities for societal benefit, and a lack of a coherent framework for policy decisions.
% FOUNDING_PROBLEM_CORROBORATION: Ongoing policy debates, economic studies on software ecosystems, and legal challenges regarding intellectual property rights in software, corroborated by independent academic research in economics, ethics, and technology policy, as well as government reports on digital economies.
narrative_ontology:disappearance_verdict(software_source_status__utilitarian_hybrid_reading, world_rearranges).
narrative_ontology:founding_problem_status(software_source_status__utilitarian_hybrid_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(software_source_status__utilitarian_hybrid_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(software_source_status__utilitarian_hybrid_reading, 'none', 1).
narrative_ontology:epsilon_provenance(software_source_status__utilitarian_hybrid_reading, 0.45, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(software_source_status__utilitarian_hybrid_reading_tests).
:- end_tests(software_source_status__utilitarian_hybrid_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness is moderate (0.45) because optimizing for aggregate welfare inherently involves trade-offs, where some parties might bear costs for the greater good, but it's not designed for pure extraction. Suppression is low (0.30) as this reading doesn't inherently suppress alternatives but evaluates them based on outcomes; any 'suppression' would be the consequence of deeming certain approaches suboptimal. Theater ratio is low (0.10) because the principle is genuinely about achieving a functional outcome (welfare maximization) rather than maintaining a facade. The claimed type is 'rope' because it serves as a coordination mechanism for a complex, multi-faceted problem, aiming for mutual benefit (aggregate welfare) through a flexible framework.
 *
 * PERSPECTIVAL GAP:
 *   While the principle aims for aggregate welfare, different stakeholders will inevitably have different interpretations of what constitutes 'welfare' and how it should be measured. For instance, open-source advocates might see open models as inherently welfare-maximizing, while proprietary firms might emphasize the welfare benefits of private investment and innovation. The engine's per-seat classification would reflect these differing experiences of the same guiding principle.
 *
 * DIRECTIONALITY LOGIC:
 *   Policy makers and economists/ethicists act as agenda-setters and analytical observers, guiding the application of the principle. Software users and innovators are the primary beneficiaries, as the principle aims to optimize outcomes for them. Software developers and proprietary software firms may act as payers in specific contexts where their preferred licensing models are deemed suboptimal for aggregate welfare, requiring them to adapt or bear costs. There are no categorical victims, as the principle's intent is to avoid systemic harm.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    aggregate_welfare_measurability,
    'Is ''aggregate welfare'' objectively measurable and universally agreed upon, or is it subject to political contestation and differing value judgments?',
    'Development of robust, multi-dimensional welfare metrics that achieve broad consensus across diverse stakeholders, or explicit acknowledgment of irreducible normative disagreements.',
    'If objectively measurable, the principle functions as a strong coordination mechanism. If highly contested, its application may become a ''tangled_rope'' or ''snare'' where powerful actors impose their definition of welfare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(aggregate_welfare_measurability, conceptual, 'Ambiguity in defining and measuring aggregate welfare.').

omega_variable(
    individual_rights_vs_aggregate_welfare,
    'How does this reading balance individual creator rights (e.g., intellectual property) against the pursuit of aggregate societal welfare, especially when they conflict?',
    'Establishment of clear ethical guidelines or legal precedents that define the boundaries and priorities between individual rights and collective benefit in software licensing.',
    'If individual rights are consistently overridden, the constraint could become more extractive for creators. If individual rights are prioritized, aggregate welfare might not be fully maximized.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(individual_rights_vs_aggregate_welfare, preference, 'Trade-off between individual rights and collective welfare.').

omega_variable(
    context_specific_optimality_evidence,
    'What empirical evidence definitively establishes the specific contexts where proprietary vs. open-source models truly maximize aggregate welfare?',
    'Longitudinal economic and sociological studies comparing outcomes of different licensing models across various software domains (e.g., infrastructure, consumer applications, specialized tools).',
    'Strong empirical evidence would solidify the ''rope'' classification by demonstrating genuine coordination towards an optimal outcome. Weak or conflicting evidence could lead to the principle being used as a ''tangled_rope'' to justify pre-existing preferences.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(context_specific_optimality_evidence, empirical, 'Empirical basis for context-dependent licensing optimality.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(software_source_status__utilitarian_hybrid_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(soft_tr_t0, software_source_status__utilitarian_hybrid_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(soft_tr_t10, software_source_status__utilitarian_hybrid_reading, theater_ratio, 10, 0.1).
narrative_ontology:measurement(soft_tr_t20, software_source_status__utilitarian_hybrid_reading, theater_ratio, 20, 0.1).
narrative_ontology:measurement(soft_tr_t30, software_source_status__utilitarian_hybrid_reading, theater_ratio, 30, 0.1).

% Extraction over time
narrative_ontology:measurement(soft_be_t0, software_source_status__utilitarian_hybrid_reading, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(soft_be_t10, software_source_status__utilitarian_hybrid_reading, base_extractiveness, 10, 0.43).
narrative_ontology:measurement(soft_be_t20, software_source_status__utilitarian_hybrid_reading, base_extractiveness, 20, 0.45).
narrative_ontology:measurement(soft_be_t30, software_source_status__utilitarian_hybrid_reading, base_extractiveness, 30, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(soft_su_t0, software_source_status__utilitarian_hybrid_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(soft_su_t10, software_source_status__utilitarian_hybrid_reading, suppression_requirement, 10, 0.28).
narrative_ontology:measurement(soft_su_t20, software_source_status__utilitarian_hybrid_reading, suppression_requirement, 20, 0.3).
narrative_ontology:measurement(soft_su_t30, software_source_status__utilitarian_hybrid_reading, suppression_requirement, 30, 0.3).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(software_source_status__utilitarian_hybrid_reading, resource_allocation).

% DUAL FORMULATION NOTE:
% This constraint is one of four readings of the 'software_source_status' kernel, focusing on aggregate welfare maximization. It is linked to the 'freedom_imperative_reading', 'pragmatic_development_reading', and 'property_rights_reading' as part of a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
