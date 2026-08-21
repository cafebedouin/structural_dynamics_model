% ============================================================================
% CONSTRAINT STORY: marriage_commitment_legitimacy__hybrid_pragmatic_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_marriage_commitment_legitimacy__hybrid_pragmatic_reading, []).

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
 *   constraint_id: marriage_commitment_legitimacy__hybrid_pragmatic_reading
 *   human_readable: Hybrid Pragmatic Reading of Marriage Commitment Legitimacy
 *   domain: religious_institutional_history/political_theology/commitment_systems
 *
 * SUMMARY:
 *   This constraint represents the 'hybrid pragmatic reading' of the marriage
 *   commitment legitimacy kernel. It describes an institutional strategy
 *   where prophetic authority is used to manage an exogenous crisis (e.g.,
 *   federal legal changes) by adapting institutional practice while
 *   preserving core theological commitments through interpretive flexibility
 *   and scope ambiguity. The institutional leadership acts as a beneficiary,
 *   gaining stability and flexibility, while rank-and-file members are
 *   victims, bearing the cost of interpretive uncertainty and potential
 *   cognitive dissonance.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(marriage_commitment_legitimacy__hybrid_pragmatic_reading, 0.6).
domain_priors:suppression_score(marriage_commitment_legitimacy__hybrid_pragmatic_reading, 0.5).
domain_priors:theater_ratio(marriage_commitment_legitimacy__hybrid_pragmatic_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(marriage_commitment_legitimacy__hybrid_pragmatic_reading, extractiveness, 0.6).
narrative_ontology:constraint_metric(marriage_commitment_legitimacy__hybrid_pragmatic_reading, suppression_requirement, 0.5).
narrative_ontology:constraint_metric(marriage_commitment_legitimacy__hybrid_pragmatic_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(marriage_commitment_legitimacy__hybrid_pragmatic_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(marriage_commitment_legitimacy__hybrid_pragmatic_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(marriage_commitment_legitimacy__hybrid_pragmatic_reading, tangled_rope).
narrative_ontology:human_readable(marriage_commitment_legitimacy__hybrid_pragmatic_reading, "Hybrid Pragmatic Reading of Marriage Commitment Legitimacy").
narrative_ontology:topic_domain(marriage_commitment_legitimacy__hybrid_pragmatic_reading, "religious_institutional_history/political_theology/commitment_systems").

domain_priors:requires_active_enforcement(marriage_commitment_legitimacy__hybrid_pragmatic_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(marriage_commitment_legitimacy__hybrid_pragmatic_reading, '6e90d111-d8bc-49e6-9900-b6fc13c1bab1').
narrative_ontology:cs_kernel_codification('6e90d111-d8bc-49e6-9900-b6fc13c1bab1', formalized).
narrative_ontology:cs_authority_grounding('6e90d111-d8bc-49e6-9900-b6fc13c1bab1', lineage).
narrative_ontology:cs_interpretation_layer_present('6e90d111-d8bc-49e6-9900-b6fc13c1bab1').
narrative_ontology:cs_reading_relation('6e90d111-d8bc-49e6-9900-b6fc13c1bab1', marriage_commitment_legitimacy__exogenous_override_reading, coexists_with).
narrative_ontology:cs_reading_relation('6e90d111-d8bc-49e6-9900-b6fc13c1bab1', marriage_commitment_legitimacy__endogenous_reinterpretation_reading, coexists_with).
narrative_ontology:cs_axiom('6e90d111-d8bc-49e6-9900-b6fc13c1bab1', foundational, institutional_survival_imperative).
narrative_ontology:cs_axiom_status(institutional_survival_imperative, holdable).
narrative_ontology:cs_axiom_grounding('6e90d111-d8bc-49e6-9900-b6fc13c1bab1', institutional_survival_imperative, instrumental).
narrative_ontology:cs_axiom('6e90d111-d8bc-49e6-9900-b6fc13c1bab1', foundational, prophetic_authority_as_adaptive_mechanism).
narrative_ontology:cs_axiom_status(prophetic_authority_as_adaptive_mechanism, holdable).
narrative_ontology:cs_axiom_grounding('6e90d111-d8bc-49e6-9900-b6fc13c1bab1', prophetic_authority_as_adaptive_mechanism, conventional).
narrative_ontology:cs_reference_frame('6e90d111-d8bc-49e6-9900-b6fc13c1bab1', theological_doctrinal_consistency).
narrative_ontology:cs_drift_state('6e90d111-d8bc-49e6-9900-b6fc13c1bab1', post_manifesto_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('6e90d111-d8bc-49e6-9900-b6fc13c1bab1', '').
narrative_ontology:cs_kernel_id(marriage_commitment_legitimacy__hybrid_pragmatic_reading, marriage_commitment_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(marriage_commitment_legitimacy__hybrid_pragmatic_reading, institutional_leadership).
narrative_ontology:constraint_victim(marriage_commitment_legitimacy__hybrid_pragmatic_reading, rank_and_file_members).
narrative_ontology:constraint_vindicates(marriage_commitment_legitimacy__hybrid_pragmatic_reading, institutional_adaptability_doctrine).
narrative_ontology:constraint_vindicates(marriage_commitment_legitimacy__hybrid_pragmatic_reading, prophetic_guidance_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Deploys prophetic authority to navigate external legal pressures while preserving core theological commitments. Benefits from maintaining institutional unity, legal compliance, and doctrinal flexibility, which allows for strategic adaptation without outright capitulation or schism.
narrative_ontology:constraint_stakeholder(marriage_commitment_legitimacy__hybrid_pragmatic_reading, institutional_leadership, agenda_setter,
    institutional, generational, arbitrage, global).

% Bear the costs of interpretive uncertainty and legitimacy ambiguity. They are expected to align their understanding and practice with the evolving institutional position, often experiencing cognitive dissonance or a sense of loss regarding prior doctrinal clarity. Exit means leaving a deeply embedded community.
narrative_ontology:constraint_stakeholder(marriage_commitment_legitimacy__hybrid_pragmatic_reading, rank_and_file_members, payer,
    moderate, biographical, constrained, global).

% Exerts legal and social pressure on the institution regarding marriage definitions. While not directly part of the internal theological constraint, its actions are the exogenous crisis that the hybrid pragmatic reading seeks to manage.
narrative_ontology:constraint_stakeholder(marriage_commitment_legitimacy__hybrid_pragmatic_reading, federal_government, observer,
    institutional, generational, analytical, national).

% Analyze the doctrinal implications, historical precedents, and theological coherence of the institutional adaptation. Their work can either reinforce or challenge the legitimacy of the hybrid pragmatic reading, but they do not directly set or enforce the constraint.
narrative_ontology:constraint_stakeholder(marriage_commitment_legitimacy__hybrid_pragmatic_reading, theological_scholars, observer,
    organized, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Manages the institutional response to external legal changes regarding marriage, ensuring federal compliance while maintaining theological integrity and institutional unity among its members.
% TRANSFER_FUNCTION: Transfers interpretive flexibility and doctrinal certainty from rank-and-file members to institutional leadership, in exchange for institutional stability and continued operation within a changing legal and social landscape.
% ABSENT_VOICES: Hardline doctrinal conservatives who believe any adaptation compromises core tenets, and progressive members who believe the adaptation is insufficient or too slow. Both are marginalized by the hybrid framing, which seeks a middle ground that satisfies neither extreme but preserves the institution.
% DISAPPEARANCE_RATIONALE: Without this adaptive framework, the institution would face severe internal schism due to irreconcilable doctrinal positions or external legal challenges threatening its tax-exempt status and operational capacity, potentially leading to its dissolution or radical transformation.
% FOUNDING_PROBLEM: The conflict between evolving federal legal standards for marriage and the institution's long-standing theological doctrine, threatening its legal status, internal cohesion, and public legitimacy.
% FOUNDING_PROBLEM_CORROBORATION: Institutional historians and legal analysts (outside the direct leadership) corroborate the historical pressure and the ongoing need for such adaptive strategies to navigate the legal and social landscape. Public records of legal challenges and internal debates also attest to the problem's persistence.
narrative_ontology:disappearance_verdict(marriage_commitment_legitimacy__hybrid_pragmatic_reading, world_rearranges).
narrative_ontology:founding_problem_status(marriage_commitment_legitimacy__hybrid_pragmatic_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(marriage_commitment_legitimacy__hybrid_pragmatic_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(marriage_commitment_legitimacy__hybrid_pragmatic_reading, 'none', 1).
narrative_ontology:epsilon_provenance(marriage_commitment_legitimacy__hybrid_pragmatic_reading, 0.6, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(marriage_commitment_legitimacy__hybrid_pragmatic_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(marriage_commitment_legitimacy__hybrid_pragmatic_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(marriage_commitment_legitimacy__hybrid_pragmatic_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.6) as the leadership gains significant control over interpretation and institutional direction, but it's not pure extraction as genuine coordination (institutional survival) is also present. Suppression (0.5) reflects the active management of internal dissent and the expectation of member compliance with evolving interpretations. Theater ratio (0.4) indicates a notable performative aspect, where 'prophetic authority' is deployed strategically to legitimize adaptation, sometimes obscuring the underlying pragmatic calculus. The metrics show a slight increase in extractiveness and theater over time, suggesting a hardening of the adaptive strategy.
 *
 * PERSPECTIVAL GAP:
 *   From the institutional leadership's perspective, this is a necessary and divinely guided adaptation (a Rope or Scaffold). From the perspective of some rank-and-file members, it may feel like an imposed reinterpretation that extracts their prior certainty (a Snare or Tangled Rope). The engine's computation will capture this divergence based on the declared structural roles and exit options.
 *
 * DIRECTIONALITY LOGIC:
 *   Institutional leadership is the primary beneficiary (low d) as they gain the flexibility to navigate crises and preserve the institution. Rank-and-file members are targets (high d) as they must adapt their understanding and practice, bearing the costs of ambiguity. The federal government is an external force, and theological scholars are observers, neither directly benefiting nor being targeted by this internal institutional constraint.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    genuineness_of_prophetic_authority,
    'Is the deployment of prophetic authority a genuine theological response or primarily an instrumental tool for institutional survival and public relations?',
    'Longitudinal study of internal institutional discourse, comparison with historical precedents of prophetic guidance in non-crisis contexts, and analysis of the consistency of theological justifications over time.',
    'If primarily instrumental, the theater_ratio and extractiveness would be higher, pushing the classification closer to a Snare. If genuinely theological, the coordination function is stronger, supporting a Tangled Rope or even a Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(genuineness_of_prophetic_authority, conceptual, 'Ambiguity regarding the true nature of prophetic authority''s deployment.').

omega_variable(
    scope_ambiguity_impact,
    'How effectively does ''scope ambiguity'' truly preserve core theological commitments, and what are the long-term costs to doctrinal coherence for rank-and-file members?',
    'Qualitative sociological studies of member belief and practice, and theological analysis of the logical consistency of the adapted doctrine over generations.',
    'If scope ambiguity leads to significant long-term doctrinal incoherence, the suppression and extractiveness on members are higher than currently measured, potentially shifting the classification towards a Snare. If it successfully preserves coherence, the coordination function is stronger.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(scope_ambiguity_impact, empirical, 'The effectiveness and long-term consequences of using scope ambiguity as an adaptive strategy.').

omega_variable(
    reading_contest_resolution,
    'Which of the competing readings (hybrid pragmatic, exogenous override, endogenous reinterpretation) will ultimately gain dominance within the broader institutional and public discourse?',
    'Analysis of future institutional pronouncements, shifts in member demographics and belief, and external academic/media reception of the different framings.',
    'If the ''exogenous override'' reading gains dominance, the constraint''s extractiveness would be re-evaluated as higher (pure coercion). If ''endogenous reinterpretation'' dominates, the coordination function would be seen as more legitimate, potentially lowering perceived extraction. The ''hybrid pragmatic'' reading''s persistence depends on its continued perceived utility.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_contest_resolution, preference, 'The ultimate resolution of the contest between different readings of the marriage commitment legitimacy kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(marriage_commitment_legitimacy__hybrid_pragmatic_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(marr_tr_t0, marriage_commitment_legitimacy__hybrid_pragmatic_reading, theater_ratio, 0, 0.3).
narrative_ontology:measurement(marr_tr_t5, marriage_commitment_legitimacy__hybrid_pragmatic_reading, theater_ratio, 5, 0.35).
narrative_ontology:measurement(marr_tr_t10, marriage_commitment_legitimacy__hybrid_pragmatic_reading, theater_ratio, 10, 0.4).
narrative_ontology:measurement(marr_tr_t15, marriage_commitment_legitimacy__hybrid_pragmatic_reading, theater_ratio, 15, 0.42).
narrative_ontology:measurement(marr_tr_t20, marriage_commitment_legitimacy__hybrid_pragmatic_reading, theater_ratio, 20, 0.4).

% Extraction over time
narrative_ontology:measurement(marr_be_t0, marriage_commitment_legitimacy__hybrid_pragmatic_reading, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(marr_be_t5, marriage_commitment_legitimacy__hybrid_pragmatic_reading, base_extractiveness, 5, 0.55).
narrative_ontology:measurement(marr_be_t10, marriage_commitment_legitimacy__hybrid_pragmatic_reading, base_extractiveness, 10, 0.6).
narrative_ontology:measurement(marr_be_t15, marriage_commitment_legitimacy__hybrid_pragmatic_reading, base_extractiveness, 15, 0.62).
narrative_ontology:measurement(marr_be_t20, marriage_commitment_legitimacy__hybrid_pragmatic_reading, base_extractiveness, 20, 0.6).

% Suppression requirement over time
narrative_ontology:measurement(marr_su_t0, marriage_commitment_legitimacy__hybrid_pragmatic_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(marr_su_t5, marriage_commitment_legitimacy__hybrid_pragmatic_reading, suppression_requirement, 5, 0.48).
narrative_ontology:measurement(marr_su_t10, marriage_commitment_legitimacy__hybrid_pragmatic_reading, suppression_requirement, 10, 0.5).
narrative_ontology:measurement(marr_su_t15, marriage_commitment_legitimacy__hybrid_pragmatic_reading, suppression_requirement, 15, 0.52).
narrative_ontology:measurement(marr_su_t20, marriage_commitment_legitimacy__hybrid_pragmatic_reading, suppression_requirement, 20, 0.5).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(marriage_commitment_legitimacy__hybrid_pragmatic_reading, identity_coordination).
narrative_ontology:affects_constraint(marriage_commitment_legitimacy__hybrid_pragmatic_reading, marriage_commitment_legitimacy__exogenous_override_reading).
narrative_ontology:affects_constraint(marriage_commitment_legitimacy__hybrid_pragmatic_reading, marriage_commitment_legitimacy__endogenous_reinterpretation_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'marriage_commitment_legitimacy' kernel, each representing a distinct structural interpretation of the institutional response to external pressures on marriage doctrine. They are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
