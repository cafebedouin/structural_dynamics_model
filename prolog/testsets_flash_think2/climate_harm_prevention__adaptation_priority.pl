% ============================================================================
% CONSTRAINT STORY: climate_harm_prevention__adaptation_priority
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_climate_harm_prevention__adaptation_priority, []).

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
 *   constraint_id: climate_harm_prevention__adaptation_priority
 *   human_readable: Climate Adaptation Priority Framework
 *   domain: climate_policy/political_economy/intergenerational_ethics
 *
 * SUMMARY:
 *   This constraint describes a dominant approach to climate policy that
 *   prioritizes building resilience and adapting to climate change impacts in
 *   the near term, often accepting a higher global warming trajectory due to
 *   the perceived political and economic infeasibility of rapid, deep
 *   emissions mitigation. It frames adaptation as the pragmatic and
 *   achievable response, while implicitly deferring significant climate costs
 *   and risks to future generations and regions with limited adaptive
 *   capacity. This constraint is one reading of the broader
 *   'climate_harm_prevention' kernel.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_harm_prevention__adaptation_priority, 0.78).
domain_priors:suppression_score(climate_harm_prevention__adaptation_priority, 0.85).
domain_priors:theater_ratio(climate_harm_prevention__adaptation_priority, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_harm_prevention__adaptation_priority, extractiveness, 0.78).
narrative_ontology:constraint_metric(climate_harm_prevention__adaptation_priority, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(climate_harm_prevention__adaptation_priority, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(climate_harm_prevention__adaptation_priority, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(climate_harm_prevention__adaptation_priority, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_harm_prevention__adaptation_priority, tangled_rope).
narrative_ontology:human_readable(climate_harm_prevention__adaptation_priority, "Climate Adaptation Priority Framework").
narrative_ontology:topic_domain(climate_harm_prevention__adaptation_priority, "climate_policy/political_economy/intergenerational_ethics").

domain_priors:requires_active_enforcement(climate_harm_prevention__adaptation_priority).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_harm_prevention__adaptation_priority, '1366e3d0-6a49-4bcf-9de9-42cd08a8c3cb').
narrative_ontology:cs_kernel_codification('1366e3d0-6a49-4bcf-9de9-42cd08a8c3cb', formalized).
narrative_ontology:cs_authority_grounding('1366e3d0-6a49-4bcf-9de9-42cd08a8c3cb', practice).
narrative_ontology:cs_interpretation_layer_present('1366e3d0-6a49-4bcf-9de9-42cd08a8c3cb').
narrative_ontology:cs_reading_relation('1366e3d0-6a49-4bcf-9de9-42cd08a8c3cb', climate_harm_prevention__degrowth_reading, forecloses).
narrative_ontology:cs_reading_relation('1366e3d0-6a49-4bcf-9de9-42cd08a8c3cb', climate_harm_prevention__mitigation_priority, influences).
narrative_ontology:cs_axiom('1366e3d0-6a49-4bcf-9de9-42cd08a8c3cb', foundational, present_generations_primary_duty).
narrative_ontology:cs_axiom_status(present_generations_primary_duty, holdable).
narrative_ontology:cs_axiom_grounding('1366e3d0-6a49-4bcf-9de9-42cd08a8c3cb', present_generations_primary_duty, deontological).
narrative_ontology:cs_axiom('1366e3d0-6a49-4bcf-9de9-42cd08a8c3cb', foundational, mitigation_politically_infeasible).
narrative_ontology:cs_axiom_status(mitigation_politically_infeasible, holdable).
narrative_ontology:cs_axiom_grounding('1366e3d0-6a49-4bcf-9de9-42cd08a8c3cb', mitigation_politically_infeasible, empirically_contingent).
narrative_ontology:cs_reference_frame('1366e3d0-6a49-4bcf-9de9-42cd08a8c3cb', pragmatic_climate_governance).
narrative_ontology:cs_drift_state('1366e3d0-6a49-4bcf-9de9-42cd08a8c3cb', contemporary_climate_crisis, gap(stable, minor, true)).
narrative_ontology:cs_created_at('1366e3d0-6a49-4bcf-9de9-42cd08a8c3cb', '').
narrative_ontology:cs_kernel_id(climate_harm_prevention__adaptation_priority, climate_harm_prevention).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_harm_prevention__adaptation_priority, present_vulnerable_populations).
narrative_ontology:constraint_beneficiary(climate_harm_prevention__adaptation_priority, adaptation_industry).
narrative_ontology:constraint_victim(climate_harm_prevention__adaptation_priority, future_generations).
narrative_ontology:constraint_victim(climate_harm_prevention__adaptation_priority, low_adaptation_capacity_regions).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Directly benefit from near-term resilience investments, which protect them from immediate climate impacts. Their options are limited to adapting in place or migrating, making direct resilience support critical.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__adaptation_priority, present_vulnerable_populations, beneficiary,
    powerless, immediate, constrained, local).

% Receives significant funding and political support for developing and implementing adaptation technologies and infrastructure. Their economic interests align with prioritizing adaptation over mitigation.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__adaptation_priority, adaptation_industry, beneficiary,
    powerful, biographical, mobile, global).

% Bear the long-term costs of a higher warming trajectory, including more severe and frequent climate disasters, ecosystem collapse, and resource scarcity, due to deferred mitigation efforts. They have no voice in current policy decisions.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__adaptation_priority, future_generations, payer,
    powerless, civilizational, trapped, universal).

% Experience disproportionate harm from climate change as their capacity for adaptation is limited by economic, social, and political factors. They bear residual climate costs that adaptation efforts cannot fully address.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__adaptation_priority, low_adaptation_capacity_regions, payer,
    powerless, generational, trapped, regional).

% Argue for aggressive emissions reductions to prevent future harm. Their policy proposals are often deemed politically or economically infeasible within this framework, limiting their influence on dominant climate policy.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__adaptation_priority, mitigation_advocates, excluded,
    organized, biographical, constrained, global).

% Advocate for planned economic contraction in high-income countries as a necessary condition for effective climate response. Their proposals are considered outside the mainstream political and economic discourse of this framework.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__adaptation_priority, degrowth_proponents, excluded,
    moderate, generational, constrained, global).

% Formulate and implement climate policies that prioritize adaptation, citing political and economic constraints on deep mitigation. They manage the allocation of resources and justify the chosen trajectory to their constituents.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__adaptation_priority, policy_makers, agenda_setter,
    institutional, biographical, mobile, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(climate_harm_prevention__adaptation_priority, adaptation_industry).
narrative_ontology:fixing_cost_class(climate_harm_prevention__adaptation_priority, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To coordinate national and international efforts and investments towards building resilience against the unavoidable impacts of climate change, given perceived limitations on rapid mitigation.
% TRANSFER_FUNCTION: Transfers political capital, financial resources, and policy focus from long-term, systemic emissions reduction efforts to immediate, localized adaptation and resilience projects. It also implicitly transfers the burden of residual climate harm to future generations and regions with limited adaptive capacity.
% ABSENT_VOICES: Future generations, non-human species, and advocates for radical systemic change (e.g., degrowth) are largely excluded from the dominant policy discourse that shapes this framework. Their interests are either unrepresented or dismissed as politically unfeasible.
% DISAPPEARANCE_RATIONALE: If this framework vanished overnight, climate policy would likely undergo a significant reorientation, potentially shifting towards more aggressive mitigation strategies or even degrowth proposals. Resources and political will would be reallocated, leading to a different global climate trajectory and distribution of burdens.
% FOUNDING_PROBLEM: How to address the immediate and growing impacts of climate change while navigating the perceived political and economic infeasibility of rapid, deep decarbonization within existing societal structures.
% FOUNDING_PROBLEM_CORROBORATION: Policy makers and the adaptation industry consistently attest that the political and economic barriers to aggressive mitigation remain live and necessitate an adaptation-first approach. However, climate scientists and some economists, while acknowledging challenges, often contest the *degree* of this infeasibility, arguing that more ambitious mitigation is both possible and necessary, thus corroborating the problem's existence but contesting its framing and proposed solution.
narrative_ontology:disappearance_verdict(climate_harm_prevention__adaptation_priority, world_rearranges).
narrative_ontology:founding_problem_status(climate_harm_prevention__adaptation_priority, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(climate_harm_prevention__adaptation_priority, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(climate_harm_prevention__adaptation_priority, 'none', 1).
narrative_ontology:epsilon_provenance(climate_harm_prevention__adaptation_priority, 0.78, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(climate_harm_prevention__adaptation_priority_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(climate_harm_prevention__adaptation_priority, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(climate_harm_prevention__adaptation_priority_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.78) is high because the framework's acceptance of higher warming trajectories directly translates into greater burdens for future generations and vulnerable regions. Suppression (0.85) is also high, as alternative approaches like aggressive mitigation or degrowth are actively sidelined or deemed 'infeasible' within the dominant policy discourse. The theater ratio (0.4) reflects that while genuine adaptation efforts occur, a portion of the rhetoric and policy activity serves to justify the avoidance of more challenging mitigation, rather than purely functional adaptation. Accessibility collapse is high (0.75) because the political and economic systems are structured to make alternatives seem unviable. Resistance is moderate (0.6) from excluded groups, but not strong enough to shift the dominant paradigm.
 *
 * PERSPECTIVAL GAP:
 *   Policy makers and the adaptation industry perceive this framework as a necessary and pragmatic response to an intractable problem, emphasizing its coordination function in building resilience. In contrast, future generations and mitigation advocates would likely experience it as a highly extractive and suppressive constraint, prioritizing short-term interests over long-term planetary health and intergenerational equity. The engine's classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Present vulnerable populations are beneficiaries of immediate resilience efforts (low d). The adaptation industry is a clear beneficiary, profiting from the shift in investment (low d). Future generations and low-adaptation-capacity regions are the primary targets, bearing the deferred and residual costs of climate change (high d). Mitigation advocates and degrowth proponents are excluded, their alternatives suppressed (high d for their policy positions). Policy makers act as agenda-setters, shaping the discourse and resource allocation (moderate d, as they also bear some political risk).
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    political_economic_infeasibility_ambiguity,
    'Is deep and rapid climate mitigation truly politically and economically infeasible, or is this framing a strategic choice to avoid difficult structural changes?',
    'Comparative analysis of policy outcomes in jurisdictions that have pursued more aggressive mitigation, or a shift in political will/technological breakthroughs that alter the perceived feasibility landscape.',
    'If mitigation is found to be more feasible than claimed, the constraint''s suppression and extractiveness would be re-evaluated as higher, indicating a more deliberate choice to extract from future generations rather than an unavoidable necessity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(political_economic_infeasibility_ambiguity, empirical, 'Ambiguity regarding the true feasibility of aggressive climate mitigation.').

omega_variable(
    intergenerational_equity_framing,
    'Is prioritizing near-term resilience over long-term harm prevention an ethically justifiable allocation of burdens across generations?',
    'Development of a globally accepted intergenerational justice framework, or a shift in societal values regarding the rights and duties owed to future generations.',
    'If deemed unjust, the constraint''s classification would shift more strongly towards a Snare, highlighting the ethical extraction from future generations. If deemed just, the coordination function would be emphasized.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(intergenerational_equity_framing, preference, 'Ethical justification of intergenerational burden sharing in climate policy.').

omega_variable(
    adaptation_limits_ambiguity,
    'At what level of warming does adaptation become insufficient or impossible for vulnerable populations and ecosystems?',
    'Further climate science research, empirical observation of adaptation limits in various regions, and integrated assessment modeling of high-warming scenarios.',
    'If adaptation limits are found to be lower than currently assumed, the extractiveness from vulnerable populations and future generations would be significantly higher, as the ''solution'' offered by this framework would prove inadequate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(adaptation_limits_ambiguity, empirical, 'Uncertainty about the ultimate effectiveness and limits of climate adaptation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_harm_prevention__adaptation_priority, 2000, 2050).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clim_tr_t2000, climate_harm_prevention__adaptation_priority, theater_ratio, 2000, 0.3).
narrative_ontology:measurement(clim_tr_t2010, climate_harm_prevention__adaptation_priority, theater_ratio, 2010, 0.35).
narrative_ontology:measurement(clim_tr_t2020, climate_harm_prevention__adaptation_priority, theater_ratio, 2020, 0.4).
narrative_ontology:measurement(clim_tr_t2030, climate_harm_prevention__adaptation_priority, theater_ratio, 2030, 0.4).
narrative_ontology:measurement(clim_tr_t2040, climate_harm_prevention__adaptation_priority, theater_ratio, 2040, 0.4).
narrative_ontology:measurement(clim_tr_t2050, climate_harm_prevention__adaptation_priority, theater_ratio, 2050, 0.4).

% Extraction over time
narrative_ontology:measurement(clim_be_t2000, climate_harm_prevention__adaptation_priority, base_extractiveness, 2000, 0.65).
narrative_ontology:measurement(clim_be_t2010, climate_harm_prevention__adaptation_priority, base_extractiveness, 2010, 0.7).
narrative_ontology:measurement(clim_be_t2020, climate_harm_prevention__adaptation_priority, base_extractiveness, 2020, 0.75).
narrative_ontology:measurement(clim_be_t2030, climate_harm_prevention__adaptation_priority, base_extractiveness, 2030, 0.77).
narrative_ontology:measurement(clim_be_t2040, climate_harm_prevention__adaptation_priority, base_extractiveness, 2040, 0.78).
narrative_ontology:measurement(clim_be_t2050, climate_harm_prevention__adaptation_priority, base_extractiveness, 2050, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(clim_su_t2000, climate_harm_prevention__adaptation_priority, suppression_requirement, 2000, 0.7).
narrative_ontology:measurement(clim_su_t2010, climate_harm_prevention__adaptation_priority, suppression_requirement, 2010, 0.75).
narrative_ontology:measurement(clim_su_t2020, climate_harm_prevention__adaptation_priority, suppression_requirement, 2020, 0.8).
narrative_ontology:measurement(clim_su_t2030, climate_harm_prevention__adaptation_priority, suppression_requirement, 2030, 0.83).
narrative_ontology:measurement(clim_su_t2040, climate_harm_prevention__adaptation_priority, suppression_requirement, 2040, 0.85).
narrative_ontology:measurement(clim_su_t2050, climate_harm_prevention__adaptation_priority, suppression_requirement, 2050, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_harm_prevention__adaptation_priority, resource_allocation).
narrative_ontology:affects_constraint(climate_harm_prevention__adaptation_priority, climate_harm_prevention__mitigation_priority).
narrative_ontology:affects_constraint(climate_harm_prevention__adaptation_priority, climate_harm_prevention__degrowth_reading).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
