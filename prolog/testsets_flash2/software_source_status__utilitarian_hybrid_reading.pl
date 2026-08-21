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
 *   domain: software_engineering/political_economy_of_technology/intellectual_property
 *
 * SUMMARY:
 *   This constraint represents the 'utilitarian hybrid' reading of software
 *   source status, where licensing decisions are driven by maximizing
 *   aggregate welfare. It posits that neither purely open-source nor purely
 *   proprietary models are universally optimal; instead, the best approach is
 *   context-dependent. This reading aims for a flexible, optimized ecosystem,
 *   accepting mixed models where appropriate. It is one reading of the
 *   'software_source_status' kernel, alongside 'freedom_imperative_reading',
 *   'pragmatic_development_reading', and 'property_rights_reading'.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(software_source_status__utilitarian_hybrid_reading, 0.3).
domain_priors:suppression_score(software_source_status__utilitarian_hybrid_reading, 0.2).
domain_priors:theater_ratio(software_source_status__utilitarian_hybrid_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(software_source_status__utilitarian_hybrid_reading, extractiveness, 0.3).
narrative_ontology:constraint_metric(software_source_status__utilitarian_hybrid_reading, suppression_requirement, 0.2).
narrative_ontology:constraint_metric(software_source_status__utilitarian_hybrid_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(software_source_status__utilitarian_hybrid_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(software_source_status__utilitarian_hybrid_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(software_source_status__utilitarian_hybrid_reading, rope).
narrative_ontology:human_readable(software_source_status__utilitarian_hybrid_reading, "Utilitarian Hybrid Reading of Software Source Status").
narrative_ontology:topic_domain(software_source_status__utilitarian_hybrid_reading, "software_engineering/political_economy_of_technology/intellectual_property").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(software_source_status__utilitarian_hybrid_reading, '8e717c9d-6267-431f-8644-db7af3a43ef6').
narrative_ontology:cs_kernel_codification('8e717c9d-6267-431f-8644-db7af3a43ef6', distributed).
narrative_ontology:cs_authority_grounding('8e717c9d-6267-431f-8644-db7af3a43ef6', expertise).
narrative_ontology:cs_interpretation_layer_present('8e717c9d-6267-431f-8644-db7af3a43ef6').
narrative_ontology:cs_reading_relation('8e717c9d-6267-431f-8644-db7af3a43ef6', software_source_status__freedom_imperative_reading, coexists_with).
narrative_ontology:cs_reading_relation('8e717c9d-6267-431f-8644-db7af3a43ef6', software_source_status__pragmatic_development_reading, coexists_with).
narrative_ontology:cs_reading_relation('8e717c9d-6267-431f-8644-db7af3a43ef6', software_source_status__property_rights_reading, coexists_with).
narrative_ontology:cs_axiom('8e717c9d-6267-431f-8644-db7af3a43ef6', foundational, aggregate_welfare_maximization_is_primary).
narrative_ontology:cs_axiom_status(aggregate_welfare_maximization_is_primary, holdable).
narrative_ontology:cs_axiom_grounding('8e717c9d-6267-431f-8644-db7af3a43ef6', aggregate_welfare_maximization_is_primary, instrumental).
narrative_ontology:cs_axiom('8e717c9d-6267-431f-8644-db7af3a43ef6', foundational, licensing_models_are_context_dependent_tools).
narrative_ontology:cs_axiom_status(licensing_models_are_context_dependent_tools, holdable).
narrative_ontology:cs_axiom_grounding('8e717c9d-6267-431f-8644-db7af3a43ef6', licensing_models_are_context_dependent_tools, empirically_contingent).
narrative_ontology:cs_reference_frame('8e717c9d-6267-431f-8644-db7af3a43ef6', optimized_mixed_ecosystem).
narrative_ontology:cs_drift_state('8e717c9d-6267-431f-8644-db7af3a43ef6', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('8e717c9d-6267-431f-8644-db7af3a43ef6', '').
narrative_ontology:cs_kernel_id(software_source_status__utilitarian_hybrid_reading, software_source_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(software_source_status__utilitarian_hybrid_reading, software_users).
narrative_ontology:constraint_beneficiary(software_source_status__utilitarian_hybrid_reading, software_developers).
narrative_ontology:constraint_beneficiary(software_source_status__utilitarian_hybrid_reading, economic_sectors).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(software_source_status__utilitarian_hybrid_reading, software_developers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefit from access to diverse software solutions, both open and proprietary, chosen for fitness-for-purpose and aggregate value. Their welfare is the primary metric for licensing decisions.
narrative_ontology:constraint_stakeholder(software_source_status__utilitarian_hybrid_reading, software_users, beneficiary,
    organized, biographical, mobile, global).

% Benefit from the flexibility to choose licensing models (open or proprietary) that best serve the welfare-maximizing goal for specific projects. They bear the cost of adapting to different licensing regimes but gain from optimized outcomes.
narrative_ontology:constraint_stakeholder(software_source_status__utilitarian_hybrid_reading, software_developers, beneficiary,
    moderate, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(software_source_status__utilitarian_hybrid_reading, software_developers, payer).

% Benefit from the efficient allocation of software resources across the economy, where licensing choices are made to maximize productivity and innovation, rather than adhering to rigid ideological positions.
narrative_ontology:constraint_stakeholder(software_source_status__utilitarian_hybrid_reading, economic_sectors, beneficiary,
    institutional, generational, mobile, national).

% Would argue for the categorical superiority of open source on ethical grounds, but their deontological arguments are subordinated to aggregate welfare calculations in this reading. They are 'excluded' from setting the primary decision criterion.
narrative_ontology:constraint_stakeholder(software_source_status__utilitarian_hybrid_reading, open_source_advocates, excluded,
    organized, generational, identity_locked, global).

% Would argue for strong property rights as the primary basis for software licensing, but their claims are evaluated against aggregate welfare, not as an inherent right. Their profit motive is secondary to societal benefit.
narrative_ontology:constraint_stakeholder(software_source_status__utilitarian_hybrid_reading, proprietary_software_firms, excluded,
    powerful, biographical, constrained, global).

% Are tasked with designing intellectual property frameworks that promote innovation and economic growth, aligning with the utilitarian goal of maximizing aggregate welfare through flexible licensing policies.
narrative_ontology:constraint_stakeholder(software_source_status__utilitarian_hybrid_reading, policy_makers, agenda_setter,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates software development and deployment by providing a flexible framework where licensing decisions (open vs. proprietary) are made based on maximizing overall societal benefit, rather than rigid ideological or property-based rules.
% TRANSFER_FUNCTION: Optimizes the flow of innovation and economic value across different software models, ensuring that the most beneficial licensing approach is adopted for each context, leading to a diffuse, aggregate welfare gain.
% ABSENT_VOICES: Categorical advocates for either pure open source (freedom imperative) or pure proprietary (property rights) are absent from the core decision-making, as their positions are seen as suboptimal for aggregate welfare maximization. They would argue for their respective absolute principles.
% DISAPPEARANCE_RATIONALE: If this utilitarian framework vanished, licensing decisions would likely revert to more rigid, ideologically driven or purely self-interested models, leading to suboptimal outcomes for overall welfare, increased friction, and potentially less innovation as the 'best fit' for context is lost.
% FOUNDING_PROBLEM: The initial conflict between absolute software freedom and absolute property rights led to suboptimal software ecosystems, hindering innovation and user benefit by forcing one-size-fits-all licensing.
% FOUNDING_PROBLEM_CORROBORATION: Economists and technology policy analysts, independent of specific software camps, corroborate that rigid licensing frameworks continue to create inefficiencies and that a flexible, welfare-maximizing approach is still needed to address evolving technological and market conditions.
narrative_ontology:disappearance_verdict(software_source_status__utilitarian_hybrid_reading, world_rearranges).
narrative_ontology:founding_problem_status(software_source_status__utilitarian_hybrid_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(software_source_status__utilitarian_hybrid_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(software_source_status__utilitarian_hybrid_reading, 'none', 1).
narrative_ontology:epsilon_provenance(software_source_status__utilitarian_hybrid_reading, 0.3, 'gemini-2.5-flash', 'none', direct).

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
 *   The extractiveness (0.3) and suppression (0.2) are relatively low because this reading aims to optimize for overall benefit, minimizing unnecessary friction or rent-seeking. Any 'extraction' is seen as a necessary cost for achieving greater aggregate welfare. The 'rope' classification reflects its function as a coordination mechanism for diverse licensing models. Theater ratio is low (0.1) as the focus is on genuine, measurable welfare outcomes.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of this reading, the constraint is a beneficial coordination mechanism. However, from the perspective of the 'freedom_imperative_reading' or 'property_rights_reading' siblings, this 'hybrid' approach might be seen as a compromise that extracts from their core principles, even if it yields aggregate welfare. This reading prioritizes outcomes over deontological or rights-based principles.
 *
 * DIRECTIONALITY LOGIC:
 *   Software users, developers, and economic sectors are all beneficiaries, as the framework aims to optimize outcomes for all. Policy makers act as agenda-setters, guiding the system towards welfare maximization. Advocates for absolute open source or property rights are 'excluded' in the sense that their categorical claims are not the primary decision criteria, though their input may inform the welfare calculation.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading inherently resists mandatrophy by continuously re-evaluating licensing choices against the live goal of maximizing aggregate welfare. If a particular licensing model ceases to be welfare-maximizing, the framework demands its adjustment, preventing the constraint from persisting beyond its utility. The 'live' status of the founding problem reinforces this adaptive nature.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    aggregate_welfare_measurement_ambiguity,
    'How is ''aggregate welfare'' precisely defined and measured in the context of software licensing, and whose welfare is prioritized in cases of conflict?',
    'Development of standardized, independently verifiable metrics for software-related welfare, including economic impact, innovation rates, accessibility, and user satisfaction, with transparent weighting mechanisms.',
    'Ambiguity in welfare measurement could allow powerful actors to define ''welfare'' in a way that benefits them, effectively turning this ''rope'' into a ''tangled_rope'' or ''snare'' by masking extraction under a utilitarian justification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(aggregate_welfare_measurement_ambiguity, conceptual, 'Uncertainty in defining and measuring the ''aggregate welfare'' that this reading seeks to maximize.').

omega_variable(
    context_dependency_arbitrage,
    'How are ''different contexts'' for licensing decisions objectively delineated to prevent strategic misclassification by actors seeking to maximize private gain over aggregate welfare?',
    'Establishment of clear, auditable criteria for determining when open-source vs. proprietary models are genuinely welfare-maximizing for specific software types or applications, enforced by independent regulatory bodies.',
    'Without clear delineation, actors could claim ''context-dependency'' to justify proprietary models where open source would be more welfare-maximizing, leading to rent extraction and a reclassification towards ''tangled_rope''.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(context_dependency_arbitrage, empirical, 'Risk of strategic exploitation of ''context-dependency'' in licensing decisions.').

omega_variable(
    kernel_reading_identity,
    'Is this constraint a genuine ''utilitarian hybrid'' reading, or is it a ''pragmatic development'' reading that uses utilitarian language as a cover?',
    'Analysis of policy outcomes: if policies consistently prioritize measurable aggregate welfare even when it conflicts with development methodology preferences, it supports the utilitarian reading. If development methodology consistently overrides welfare, it''s a pragmatic reading.',
    'If it''s a pragmatic reading in disguise, the underlying values are different, potentially leading to different classifications for specific policy implementations. The ''pragmatic development'' reading might tolerate more extraction if it believes it leads to ''better'' software, even if not strictly welfare-maximizing.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Distinguishing genuine utilitarianism from pragmatic development framed in utilitarian terms.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(software_source_status__utilitarian_hybrid_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(soft_tr_t0, software_source_status__utilitarian_hybrid_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement(soft_tr_t5, software_source_status__utilitarian_hybrid_reading, theater_ratio, 5, 0.09).
narrative_ontology:measurement(soft_tr_t10, software_source_status__utilitarian_hybrid_reading, theater_ratio, 10, 0.09).
narrative_ontology:measurement(soft_tr_t15, software_source_status__utilitarian_hybrid_reading, theater_ratio, 15, 0.1).
narrative_ontology:measurement(soft_tr_t20, software_source_status__utilitarian_hybrid_reading, theater_ratio, 20, 0.1).

% Extraction over time
narrative_ontology:measurement(soft_be_t0, software_source_status__utilitarian_hybrid_reading, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(soft_be_t5, software_source_status__utilitarian_hybrid_reading, base_extractiveness, 5, 0.27).
narrative_ontology:measurement(soft_be_t10, software_source_status__utilitarian_hybrid_reading, base_extractiveness, 10, 0.28).
narrative_ontology:measurement(soft_be_t15, software_source_status__utilitarian_hybrid_reading, base_extractiveness, 15, 0.29).
narrative_ontology:measurement(soft_be_t20, software_source_status__utilitarian_hybrid_reading, base_extractiveness, 20, 0.3).

% Suppression requirement over time
narrative_ontology:measurement(soft_su_t0, software_source_status__utilitarian_hybrid_reading, suppression_requirement, 0, 0.15).
narrative_ontology:measurement(soft_su_t5, software_source_status__utilitarian_hybrid_reading, suppression_requirement, 5, 0.17).
narrative_ontology:measurement(soft_su_t10, software_source_status__utilitarian_hybrid_reading, suppression_requirement, 10, 0.18).
narrative_ontology:measurement(soft_su_t15, software_source_status__utilitarian_hybrid_reading, suppression_requirement, 15, 0.19).
narrative_ontology:measurement(soft_su_t20, software_source_status__utilitarian_hybrid_reading, suppression_requirement, 20, 0.2).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(software_source_status__utilitarian_hybrid_reading, resource_allocation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
