% ============================================================================
% CONSTRAINT STORY: ai_alignment_priority__integrated_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ai_alignment_priority__integrated_reading, []).

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
 *   constraint_id: ai_alignment_priority__integrated_reading
 *   human_readable: Integrated AI Alignment: Catastrophic and Present Harms
 *   domain: ai_governance/technology_ethics/risk_assessment
 *
 * SUMMARY:
 *   This constraint instantiates the 'integrated_reading' of the
 *   'ai_alignment_priority' kernel. This reading posits that AI alignment
 *   must address both catastrophic existential risks and immediate,
 *   present-day harms (e.g., bias, discrimination, labor displacement) as
 *   complementary, rather than competing, priorities. Sibling readings
 *   include the 'existential_risk_reading' (prioritizing long-term safety)
 *   and the 'nearterm_harms_reading' (prioritizing immediate justice). The
 *   constraint functions as a framework to coordinate diverse efforts, but
 *   its enforcement requires active suppression of single-focus approaches,
 *   leading to moderate extraction from those who prefer a simpler
 *   prioritization.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_alignment_priority__integrated_reading, 0.55).
domain_priors:suppression_score(ai_alignment_priority__integrated_reading, 0.6).
domain_priors:theater_ratio(ai_alignment_priority__integrated_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_alignment_priority__integrated_reading, extractiveness, 0.55).
narrative_ontology:constraint_metric(ai_alignment_priority__integrated_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(ai_alignment_priority__integrated_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ai_alignment_priority__integrated_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(ai_alignment_priority__integrated_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_alignment_priority__integrated_reading, tangled_rope).
narrative_ontology:human_readable(ai_alignment_priority__integrated_reading, "Integrated AI Alignment: Catastrophic and Present Harms").
narrative_ontology:topic_domain(ai_alignment_priority__integrated_reading, "ai_governance/technology_ethics/risk_assessment").

domain_priors:requires_active_enforcement(ai_alignment_priority__integrated_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ai_alignment_priority__integrated_reading, 'dbbbd61c-ecc3-4c64-a15b-2ab49e579a76').
narrative_ontology:cs_kernel_codification('dbbbd61c-ecc3-4c64-a15b-2ab49e579a76', formalized).
narrative_ontology:cs_authority_grounding('dbbbd61c-ecc3-4c64-a15b-2ab49e579a76', expertise).
narrative_ontology:cs_interpretation_layer_present('dbbbd61c-ecc3-4c64-a15b-2ab49e579a76').
narrative_ontology:cs_reading_relation('dbbbd61c-ecc3-4c64-a15b-2ab49e579a76', ai_alignment_priority__existential_risk_reading, influences).
narrative_ontology:cs_reading_relation('dbbbd61c-ecc3-4c64-a15b-2ab49e579a76', ai_alignment_priority__nearterm_harms_reading, influences).
narrative_ontology:cs_axiom('dbbbd61c-ecc3-4c64-a15b-2ab49e579a76', foundational, dual_imperative_of_safety_and_justice).
narrative_ontology:cs_axiom_status(dual_imperative_of_safety_and_justice, holdable).
narrative_ontology:cs_axiom_grounding('dbbbd61c-ecc3-4c64-a15b-2ab49e579a76', dual_imperative_of_safety_and_justice, deontological).
narrative_ontology:cs_axiom('dbbbd61c-ecc3-4c64-a15b-2ab49e579a76', foundational, interconnectedness_of_risk_horizons).
narrative_ontology:cs_axiom_status(interconnectedness_of_risk_horizons, holdable).
narrative_ontology:cs_axiom_grounding('dbbbd61c-ecc3-4c64-a15b-2ab49e579a76', interconnectedness_of_risk_horizons, empirically_contingent).
narrative_ontology:cs_reference_frame('dbbbd61c-ecc3-4c64-a15b-2ab49e579a76', dual_imperative_framework).
narrative_ontology:cs_drift_state('dbbbd61c-ecc3-4c64-a15b-2ab49e579a76', contemporary_ai_governance_debate, gap(stable, minor, true)).
narrative_ontology:cs_created_at('dbbbd61c-ecc3-4c64-a15b-2ab49e579a76', '').
narrative_ontology:cs_kernel_id(ai_alignment_priority__integrated_reading, ai_alignment_priority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_alignment_priority__integrated_reading, integrated_alignment_advocates).
narrative_ontology:constraint_beneficiary(ai_alignment_priority__integrated_reading, future_generations).
narrative_ontology:constraint_beneficiary(ai_alignment_priority__integrated_reading, marginalized_communities).
narrative_ontology:constraint_victim(ai_alignment_priority__integrated_reading, single_focus_longtermists).
narrative_ontology:constraint_victim(ai_alignment_priority__integrated_reading, single_focus_neartermists).
narrative_ontology:constraint_victim(ai_alignment_priority__integrated_reading, ai_developers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Actively promotes and develops frameworks that integrate both long-term catastrophic risks and immediate societal harms in AI alignment. They benefit from the broader acceptance and resource allocation towards this comprehensive approach.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__integrated_reading, integrated_alignment_advocates, agenda_setter,
    organized, generational, constrained, global).

% Primarily concerned with existential risks from advanced AI. They bear the cost of diverting resources and attention to present harms, which they may perceive as less critical or distracting from their core mission. Their exit is constrained by the growing consensus for broader approaches.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__integrated_reading, single_focus_longtermists, payer,
    powerful, civilizational, constrained, global).

% Primarily concerned with immediate harms like bias, discrimination, and labor displacement. They bear the cost of diverting resources and attention to speculative future risks, which they may perceive as a distraction from urgent present needs. Their exit is constrained by the growing recognition of future risks.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__integrated_reading, single_focus_neartermists, payer,
    powerful, biographical, constrained, global).

% Responsible for implementing alignment principles. They face increased compliance burdens and design constraints due to the dual focus on both catastrophic and present harms, requiring more complex and resource-intensive development processes.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__integrated_reading, ai_developers, payer,
    institutional, immediate, constrained, global).

% Benefit from efforts to mitigate catastrophic risks that could threaten their existence or well-being. Their interests are represented by advocates in the present.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__integrated_reading, future_generations, beneficiary,
    powerless, civilizational, trapped, universal).

% Benefit from efforts to mitigate present harms like algorithmic bias and discrimination, which disproportionately affect them. Their advocacy helps ensure their concerns are integrated into alignment efforts.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__integrated_reading, marginalized_communities, beneficiary,
    organized, generational, constrained, global).

% Observe and evaluate the effectiveness of integrated alignment strategies, considering potential regulatory interventions. They are influenced by the arguments and evidence presented by all stakeholder groups.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__integrated_reading, policy_makers, observer,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To unify disparate AI alignment efforts under a comprehensive framework that addresses both long-term catastrophic risks and immediate societal harms, preventing a zero-sum competition for resources and attention.
% TRANSFER_FUNCTION: Transfers resources, research focus, and compliance burdens from single-priority approaches to a dual-priority framework. It also transfers attention and protective measures to both future populations and currently marginalized groups.
% ABSENT_VOICES: Those who believe AI alignment is an an unsolvable problem or a distraction from other societal issues; those who believe AI development should be unconstrained by ethical or safety considerations.
% DISAPPEARANCE_RATIONALE: If this integrated approach vanished, the AI alignment discourse would likely fracture into competing, zero-sum camps, leading to unaddressed risks on one or both fronts, and potentially less effective governance overall. Resources would be misallocated, and a coherent strategy would be impossible.
% FOUNDING_PROBLEM: The fragmentation of AI safety efforts into competing camps (existential risk vs. present harms), leading to an inability to form a coherent, broadly supported governance strategy for AI.
% FOUNDING_PROBLEM_CORROBORATION: Academic papers, policy reports, and public statements from diverse AI ethics and safety organizations (e.g., Partnership on AI, AI Now Institute, Future of Humanity Institute) corroborate the ongoing challenge of integrating these perspectives, even if they don't all endorse this specific integrated reading.
narrative_ontology:disappearance_verdict(ai_alignment_priority__integrated_reading, world_rearranges).
narrative_ontology:founding_problem_status(ai_alignment_priority__integrated_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ai_alignment_priority__integrated_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(ai_alignment_priority__integrated_reading, 'none', 1).
narrative_ontology:epsilon_provenance(ai_alignment_priority__integrated_reading, 0.55, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ai_alignment_priority__integrated_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(ai_alignment_priority__integrated_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(ai_alignment_priority__integrated_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.55) reflects the cost of maintaining a dual focus, requiring resources and attention from all parties, including those who would prefer to concentrate on a single priority. Suppression (0.60) is necessary to prevent the discourse and resource allocation from fracturing into competing, zero-sum battles. The theater ratio is low (0.15) because the integrated approach is a genuine attempt to address complex problems, with concrete research and policy implications. Resistance (0.55) is present from both single-focus camps who feel their priorities are diluted.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of integrated alignment advocates, this constraint is a necessary coordination mechanism for effective AI governance. However, from the perspective of single-focus groups, it represents an extractive force that dilutes their efforts and imposes additional costs without clear benefits to their primary concerns. The engine's per-seat classification will reflect these divergent experiences.
 *
 * DIRECTIONALITY LOGIC:
 *   Integrated alignment advocates, future generations, and marginalized communities are beneficiaries, as their interests are directly served by this comprehensive approach. Single-focus longtermists and neartermists, along with AI developers, are payers, as they must adapt their priorities and bear increased compliance burdens. Policy makers act as observers, evaluating the efficacy of this integrated framework.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    implicit_prioritization_bias,
    'Does the ''integrated'' approach, in practice, implicitly favor one type of harm (catastrophic vs. present) over the other in resource allocation or policy implementation?',
    'Longitudinal analysis of funding allocations, research output, and regulatory enforcement actions across both harm categories within organizations adopting the integrated framework.',
    'If a consistent bias is found, the constraint''s effective extractiveness and suppression would be higher for the disfavored group, potentially reclassifying it closer to a Snare for that seat.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(implicit_prioritization_bias, empirical, 'Whether the integrated framework maintains genuine balance or develops an implicit bias.').

omega_variable(
    rhetorical_vs_structural_integration,
    'Is the ''complementary'' framing a genuine structural integration of priorities, or primarily a rhetorical strategy to manage stakeholder conflict without true operational change?',
    'Qualitative analysis of internal decision-making processes and interviews with key stakeholders to assess the extent to which the ''complementary'' principle genuinely guides trade-offs and resource allocation, rather than merely being invoked publicly.',
    'If primarily rhetorical, the theater_ratio would be significantly higher, and the constraint might compute closer to a Piton or Snare, as its stated function would diverge from its actual operation.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(rhetorical_vs_structural_integration, conceptual, 'Assessing the authenticity of the ''complementary'' framing in practice.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_alignment_priority__integrated_reading, 2018, 2028).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ai_a_tr_t2018, ai_alignment_priority__integrated_reading, theater_ratio, 2018, 0.1).
narrative_ontology:measurement(ai_a_tr_t2020, ai_alignment_priority__integrated_reading, theater_ratio, 2020, 0.11).
narrative_ontology:measurement(ai_a_tr_t2022, ai_alignment_priority__integrated_reading, theater_ratio, 2022, 0.12).
narrative_ontology:measurement(ai_a_tr_t2024, ai_alignment_priority__integrated_reading, theater_ratio, 2024, 0.13).
narrative_ontology:measurement(ai_a_tr_t2026, ai_alignment_priority__integrated_reading, theater_ratio, 2026, 0.14).
narrative_ontology:measurement(ai_a_tr_t2028, ai_alignment_priority__integrated_reading, theater_ratio, 2028, 0.15).

% Extraction over time
narrative_ontology:measurement(ai_a_be_t2018, ai_alignment_priority__integrated_reading, base_extractiveness, 2018, 0.5).
narrative_ontology:measurement(ai_a_be_t2020, ai_alignment_priority__integrated_reading, base_extractiveness, 2020, 0.51).
narrative_ontology:measurement(ai_a_be_t2022, ai_alignment_priority__integrated_reading, base_extractiveness, 2022, 0.52).
narrative_ontology:measurement(ai_a_be_t2024, ai_alignment_priority__integrated_reading, base_extractiveness, 2024, 0.53).
narrative_ontology:measurement(ai_a_be_t2026, ai_alignment_priority__integrated_reading, base_extractiveness, 2026, 0.54).
narrative_ontology:measurement(ai_a_be_t2028, ai_alignment_priority__integrated_reading, base_extractiveness, 2028, 0.55).

% Suppression requirement over time
narrative_ontology:measurement(ai_a_su_t2018, ai_alignment_priority__integrated_reading, suppression_requirement, 2018, 0.55).
narrative_ontology:measurement(ai_a_su_t2020, ai_alignment_priority__integrated_reading, suppression_requirement, 2020, 0.56).
narrative_ontology:measurement(ai_a_su_t2022, ai_alignment_priority__integrated_reading, suppression_requirement, 2022, 0.57).
narrative_ontology:measurement(ai_a_su_t2024, ai_alignment_priority__integrated_reading, suppression_requirement, 2024, 0.58).
narrative_ontology:measurement(ai_a_su_t2026, ai_alignment_priority__integrated_reading, suppression_requirement, 2026, 0.59).
narrative_ontology:measurement(ai_a_su_t2028, ai_alignment_priority__integrated_reading, suppression_requirement, 2028, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ai_alignment_priority__integrated_reading, enforcement_mechanism).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'ai_alignment_priority' kernel, alongside 'ai_alignment_priority__existential_risk_reading' and 'ai_alignment_priority__nearterm_harms_reading'.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
