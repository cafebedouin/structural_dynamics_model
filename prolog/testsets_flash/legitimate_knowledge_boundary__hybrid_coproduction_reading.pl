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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: legitimate_knowledge_boundary__hybrid_coproduction_reading
 *   human_readable: Hybrid Co-production Standard for Legitimate Knowledge
 *   domain: epistemology/science_and_technology_studies/political_theory
 *
 * SUMMARY:
 *   This constraint defines legitimate knowledge as requiring the integration
 *   of methodological rigor and experiential validity through co-production
 *   processes. It is a reading of the broader 'legitimate_knowledge_boundary'
 *   kernel, which is contested by readings prioritizing credentialed
 *   expertise or pure experiential pluralism. This hybrid approach aims to
 *   bridge divides but introduces its own complexities and enforcement costs,
 *   making it a Tangled Rope.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(legitimate_knowledge_boundary__hybrid_coproduction_reading, 0.45).
domain_priors:suppression_score(legitimate_knowledge_boundary__hybrid_coproduction_reading, 0.35).
domain_priors:theater_ratio(legitimate_knowledge_boundary__hybrid_coproduction_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(legitimate_knowledge_boundary__hybrid_coproduction_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(legitimate_knowledge_boundary__hybrid_coproduction_reading, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(legitimate_knowledge_boundary__hybrid_coproduction_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(legitimate_knowledge_boundary__hybrid_coproduction_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(legitimate_knowledge_boundary__hybrid_coproduction_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(legitimate_knowledge_boundary__hybrid_coproduction_reading, tangled_rope).
narrative_ontology:human_readable(legitimate_knowledge_boundary__hybrid_coproduction_reading, "Hybrid Co-production Standard for Legitimate Knowledge").
narrative_ontology:topic_domain(legitimate_knowledge_boundary__hybrid_coproduction_reading, "epistemology/science_and_technology_studies/political_theory").

domain_priors:requires_active_enforcement(legitimate_knowledge_boundary__hybrid_coproduction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(legitimate_knowledge_boundary__hybrid_coproduction_reading, 'f9e88949-dd49-417c-b6d3-d45b712b57aa').
narrative_ontology:cs_kernel_codification('f9e88949-dd49-417c-b6d3-d45b712b57aa', formalized).
narrative_ontology:cs_authority_grounding('f9e88949-dd49-417c-b6d3-d45b712b57aa', practice).
narrative_ontology:cs_interpretation_layer_present('f9e88949-dd49-417c-b6d3-d45b712b57aa').
narrative_ontology:cs_reading_relation('f9e88949-dd49-417c-b6d3-d45b712b57aa', legitimate_knowledge_boundary__credentialed_expertise_reading, influences).
narrative_ontology:cs_reading_relation('f9e88949-dd49-417c-b6d3-d45b712b57aa', legitimate_knowledge_boundary__experiential_pluralism_reading, influences).
narrative_ontology:cs_axiom('f9e88949-dd49-417c-b6d3-d45b712b57aa', foundational, epistemic_pluralism_is_necessary).
narrative_ontology:cs_axiom_status(epistemic_pluralism_is_necessary, holdable).
narrative_ontology:cs_axiom_grounding('f9e88949-dd49-417c-b6d3-d45b712b57aa', epistemic_pluralism_is_necessary, deontological).
narrative_ontology:cs_axiom('f9e88949-dd49-417c-b6d3-d45b712b57aa', foundational, integrated_knowledge_is_more_robust).
narrative_ontology:cs_axiom_status(integrated_knowledge_is_more_robust, holdable).
narrative_ontology:cs_axiom_grounding('f9e88949-dd49-417c-b6d3-d45b712b57aa', integrated_knowledge_is_more_robust, empirically_contingent).
narrative_ontology:cs_reference_frame('f9e88949-dd49-417c-b6d3-d45b712b57aa', integrated_knowledge_for_societal_challenges).
narrative_ontology:cs_drift_state('f9e88949-dd49-417c-b6d3-d45b712b57aa', contemporary, gap(practice_drift, minor, true)).
narrative_ontology:cs_created_at('f9e88949-dd49-417c-b6d3-d45b712b57aa', '').
narrative_ontology:cs_kernel_id(legitimate_knowledge_boundary__hybrid_coproduction_reading, legitimate_knowledge_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(legitimate_knowledge_boundary__hybrid_coproduction_reading, coproduction_facilitators).
narrative_ontology:constraint_beneficiary(legitimate_knowledge_boundary__hybrid_coproduction_reading, interdisciplinary_researchers).
narrative_ontology:constraint_beneficiary(legitimate_knowledge_boundary__hybrid_coproduction_reading, marginalized_communities).
narrative_ontology:constraint_victim(legitimate_knowledge_boundary__hybrid_coproduction_reading, traditional_academic_institutions).
narrative_ontology:constraint_victim(legitimate_knowledge_boundary__hybrid_coproduction_reading, pure_experiential_advocates).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Organizations and individuals who design, fund, and manage co-production processes. They benefit from the legitimacy conferred by this standard, but also bear the costs of its complex implementation.
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__hybrid_coproduction_reading, coproduction_facilitators, agenda_setter,
    organized, biographical, constrained, national).

% Academics whose work naturally bridges methodological rigor and experiential knowledge. This standard legitimizes their approach and opens new funding and publication avenues.
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__hybrid_coproduction_reading, interdisciplinary_researchers, beneficiary,
    moderate, biographical, mobile, global).

% Groups whose experiential knowledge is recognized as valid and integrated into research, leading to more relevant and equitable outcomes. They gain voice and influence in knowledge production, but are still dependent on the co-production framework.
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__hybrid_coproduction_reading, marginalized_communities, beneficiary,
    powerless, generational, constrained, local).

% Universities and research bodies that must adapt their structures, funding models, and tenure processes to accommodate co-production. They bear the costs of institutional change and may resist the dilution of traditional credentialed authority.
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__hybrid_coproduction_reading, traditional_academic_institutions, payer,
    institutional, generational, constrained, national).

% Groups who prioritize lived experience as the primary source of knowledge and view methodological rigor as potentially colonial or exclusionary. They are forced to engage with methodological standards they may distrust, bearing the cost of compromise.
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__hybrid_coproduction_reading, pure_experiential_advocates, payer,
    organized, biographical, constrained, local).

% Experts in traditional academic fields who may find co-production outputs difficult to evaluate using conventional metrics, leading to their marginalization from the validation process or their resistance to it.
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__hybrid_coproduction_reading, credentialed_peer_reviewers, excluded,
    powerful, biographical, constrained, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the integration of diverse forms of knowledge (scientific, indigenous, local, experiential) to produce more robust and socially relevant insights, bridging the gap between academic rigor and practical validity.
% TRANSFER_FUNCTION: Transfers epistemic authority and resources from purely credentialed experts to include experiential knowledge holders, while also transferring methodological requirements to experiential advocates. It also transfers funding and legitimacy to co-production infrastructure.
% ABSENT_VOICES: Purely credentialed peer reviewers who would argue for the primacy of traditional methodological standards, and radical experiential pluralists who would reject any imposition of academic rigor. Both are often sidelined in the hybrid co-production discourse.
% DISAPPEARANCE_RATIONALE: If this standard vanished, the nascent infrastructure for co-production would likely collapse, leading to a re-entrenchment of either purely credentialed expertise or fragmented experiential knowledge, with less cross-pollination and mutual legitimation. Funding for interdisciplinary, community-engaged research would diminish.
% FOUNDING_PROBLEM: The problem of knowledge legitimacy in complex societal challenges, where purely academic knowledge often lacks practical relevance or public trust, and purely experiential knowledge lacks systematic rigor or generalizability.
% FOUNDING_PROBLEM_CORROBORATION: Scholars in Science and Technology Studies, public policy analysts, and community organizers from outside the direct co-production industry corroborate the ongoing need for integrated knowledge approaches to address wicked problems like climate change, public health, and social inequality. Reports from international bodies also attest to this need.
narrative_ontology:disappearance_verdict(legitimate_knowledge_boundary__hybrid_coproduction_reading, world_rearranges).
narrative_ontology:founding_problem_status(legitimate_knowledge_boundary__hybrid_coproduction_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(legitimate_knowledge_boundary__hybrid_coproduction_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(legitimate_knowledge_boundary__hybrid_coproduction_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(legitimate_knowledge_boundary__hybrid_coproduction_reading_tests).
:- end_tests(legitimate_knowledge_boundary__hybrid_coproduction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The constraint is classified as a Tangled Rope because it genuinely coordinates the integration of diverse knowledge forms (beneficiaries: marginalized communities, interdisciplinary researchers) but also involves asymmetric extraction and requires active enforcement. Traditional academic institutions and pure experiential advocates bear costs by having to adapt to a dual validation standard. Extractiveness (0.45) is moderate, reflecting the overhead and compromises required. Suppression (0.35) is present as both methodological and experiential standards must be enforced, and alternatives (purely academic or purely experiential knowledge) are de-legitimized. Theater ratio (0.20) is low, as the co-production processes are largely functional, though some performative aspects exist in navigating institutional resistance.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of co-production facilitators and marginalized communities, this constraint is a beneficial coordination mechanism. From the perspective of traditional academic institutions and pure experiential advocates, it imposes new burdens and compromises existing epistemic frameworks. The engine's per-seat classification will reflect these divergent experiences.
 *
 * DIRECTIONALITY LOGIC:
 *   Co-production facilitators and interdisciplinary researchers are beneficiaries, gaining legitimacy and resources. Marginalized communities are also beneficiaries, as their knowledge is valued. Traditional academic institutions and pure experiential advocates are payers, as they must adapt and compromise. Credentialed peer reviewers are excluded, as their traditional validation methods are not sufficient for this hybrid standard.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    coproduction_authenticity,
    'To what extent are co-production processes genuinely integrative, versus merely performative inclusion of marginalized voices to legitimize pre-determined research agendas?',
    'Longitudinal studies tracking power dynamics and decision-making authority within co-production projects, assessing whether community priorities genuinely shape research questions and methodologies.',
    'If largely performative, the constraint''s extractiveness from marginalized communities is higher than measured, and its coordination function is weaker, pushing it closer to a Snare. If genuinely integrative, its Rope-like qualities are stronger.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coproduction_authenticity, empirical, 'Assessing the genuine integration vs. performative inclusion in co-production.').

omega_variable(
    institutional_resistance_cost,
    'What is the true cost of institutional adaptation for traditional academic bodies to implement co-production, and is this cost disproportionately borne by specific departments or individuals?',
    'Detailed financial audits and qualitative studies of academic institutions, tracking resource allocation, faculty workload, and career progression for those engaged in co-production.',
    'If costs are high and concentrated, the constraint''s extractiveness from traditional institutions is higher, and its sustainability is threatened by internal resistance. If costs are diffuse and manageable, the transition is smoother.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_resistance_cost, empirical, 'Cost of institutional adaptation to co-production standards.').

omega_variable(
    epistemic_framing_ambiguity,
    'Is the ''integration'' of methodological rigor and experiential validity a true synthesis, or does one form of knowledge implicitly retain primacy, leading to a subtle re-subordination?',
    'Discourse analysis of co-produced outputs and meta-analysis of co-production frameworks to identify implicit hierarchies or ''translation'' requirements that privilege one epistemic form over another.',
    'If one form retains primacy, the constraint''s claimed coordination function is partially theatrical, and its extractiveness from the subordinated knowledge form is higher, pushing it closer to a Tangled Rope or Snare.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(epistemic_framing_ambiguity, conceptual, 'Whether integration achieves true synthesis or implicit hierarchy.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(legitimate_knowledge_boundary__hybrid_coproduction_reading, 2000, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(legi_tr_t2000, legitimate_knowledge_boundary__hybrid_coproduction_reading, theater_ratio, 2000, 0.1).
narrative_ontology:measurement(legi_tr_t2008, legitimate_knowledge_boundary__hybrid_coproduction_reading, theater_ratio, 2008, 0.15).
narrative_ontology:measurement(legi_tr_t2016, legitimate_knowledge_boundary__hybrid_coproduction_reading, theater_ratio, 2016, 0.18).
narrative_ontology:measurement(legi_tr_t2024, legitimate_knowledge_boundary__hybrid_coproduction_reading, theater_ratio, 2024, 0.2).

% Extraction over time
narrative_ontology:measurement(legi_be_t2000, legitimate_knowledge_boundary__hybrid_coproduction_reading, base_extractiveness, 2000, 0.3).
narrative_ontology:measurement(legi_be_t2008, legitimate_knowledge_boundary__hybrid_coproduction_reading, base_extractiveness, 2008, 0.38).
narrative_ontology:measurement(legi_be_t2016, legitimate_knowledge_boundary__hybrid_coproduction_reading, base_extractiveness, 2016, 0.42).
narrative_ontology:measurement(legi_be_t2024, legitimate_knowledge_boundary__hybrid_coproduction_reading, base_extractiveness, 2024, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(legi_su_t2000, legitimate_knowledge_boundary__hybrid_coproduction_reading, suppression_requirement, 2000, 0.25).
narrative_ontology:measurement(legi_su_t2008, legitimate_knowledge_boundary__hybrid_coproduction_reading, suppression_requirement, 2008, 0.3).
narrative_ontology:measurement(legi_su_t2016, legitimate_knowledge_boundary__hybrid_coproduction_reading, suppression_requirement, 2016, 0.33).
narrative_ontology:measurement(legi_su_t2024, legitimate_knowledge_boundary__hybrid_coproduction_reading, suppression_requirement, 2024, 0.35).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(legitimate_knowledge_boundary__hybrid_coproduction_reading, identity_coordination).
narrative_ontology:affects_constraint(legitimate_knowledge_boundary__hybrid_coproduction_reading, legitimate_knowledge_boundary__credentialed_expertise_reading).
narrative_ontology:affects_constraint(legitimate_knowledge_boundary__hybrid_coproduction_reading, legitimate_knowledge_boundary__experiential_pluralism_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'legitimate_knowledge_boundary' kernel. This 'hybrid_coproduction_reading' attempts to integrate elements from the 'credentialed_expertise_reading' and the 'experiential_pluralism_reading', creating a new standard for knowledge legitimacy.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
