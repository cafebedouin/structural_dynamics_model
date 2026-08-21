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
 *   human_readable: Climate Adaptation Priority (Reading of Climate Harm Prevention)
 *   domain: climate_policy/political_economy/intergenerational_ethics
 *
 * SUMMARY:
 *   This constraint describes a climate policy framework that prioritizes
 *   adaptation and resilience building in the near term, implicitly accepting
 *   a higher global warming trajectory due to the perceived political and
 *   economic infeasibility of aggressive mitigation. It is a specific reading
 *   of the broader 'climate harm prevention' kernel, distinct from readings
 *   that prioritize mitigation or degrowth. The constraint functions as a
 *   Tangled Rope, providing immediate coordination for adaptation while
 *   extracting from future generations and vulnerable regions through
 *   deferred mitigation.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_harm_prevention__adaptation_priority, 0.65).
domain_priors:suppression_score(climate_harm_prevention__adaptation_priority, 0.7).
domain_priors:theater_ratio(climate_harm_prevention__adaptation_priority, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_harm_prevention__adaptation_priority, extractiveness, 0.65).
narrative_ontology:constraint_metric(climate_harm_prevention__adaptation_priority, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(climate_harm_prevention__adaptation_priority, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(climate_harm_prevention__adaptation_priority, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(climate_harm_prevention__adaptation_priority, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_harm_prevention__adaptation_priority, tangled_rope).
narrative_ontology:human_readable(climate_harm_prevention__adaptation_priority, "Climate Adaptation Priority (Reading of Climate Harm Prevention)").
narrative_ontology:topic_domain(climate_harm_prevention__adaptation_priority, "climate_policy/political_economy/intergenerational_ethics").

domain_priors:requires_active_enforcement(climate_harm_prevention__adaptation_priority).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_harm_prevention__adaptation_priority, '4b12613e-2855-424c-9140-661e99d284ff').
narrative_ontology:cs_kernel_codification('4b12613e-2855-424c-9140-661e99d284ff', distributed).
narrative_ontology:cs_authority_grounding('4b12613e-2855-424c-9140-661e99d284ff', extraction).
narrative_ontology:cs_interpretation_layer_present('4b12613e-2855-424c-9140-661e99d284ff').
narrative_ontology:cs_reading_relation('4b12613e-2855-424c-9140-661e99d284ff', climate_harm_prevention__mitigation_priority, influences).
narrative_ontology:cs_reading_relation('4b12613e-2855-424c-9140-661e99d284ff', climate_harm_prevention__degrowth_reading, influences).
narrative_ontology:cs_axiom('4b12613e-2855-424c-9140-661e99d284ff', foundational, present_generations_have_priority_claim_on_resources).
narrative_ontology:cs_axiom_status(present_generations_have_priority_claim_on_resources, holdable).
narrative_ontology:cs_axiom_grounding('4b12613e-2855-424c-9140-661e99d284ff', present_generations_have_priority_claim_on_resources, deontological).
narrative_ontology:cs_axiom('4b12613e-2855-424c-9140-661e99d284ff', foundational, economic_growth_is_non_negotiable_in_near_term).
narrative_ontology:cs_axiom_status(economic_growth_is_non_negotiable_in_near_term, holdable).
narrative_ontology:cs_axiom_grounding('4b12613e-2855-424c-9140-661e99d284ff', economic_growth_is_non_negotiable_in_near_term, conventional).
narrative_ontology:cs_reference_frame('4b12613e-2855-424c-9140-661e99d284ff', pragmatic_climate_governance).
narrative_ontology:cs_drift_state('4b12613e-2855-424c-9140-661e99d284ff', contemporary_climate_crisis, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('4b12613e-2855-424c-9140-661e99d284ff', '').
narrative_ontology:cs_kernel_id(climate_harm_prevention__adaptation_priority, climate_harm_prevention).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_harm_prevention__adaptation_priority, present_vulnerable_populations).
narrative_ontology:constraint_beneficiary(climate_harm_prevention__adaptation_priority, global_north_economies).
narrative_ontology:constraint_victim(climate_harm_prevention__adaptation_priority, future_generations).
narrative_ontology:constraint_victim(climate_harm_prevention__adaptation_priority, low_adaptation_capacity_regions).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Receive immediate benefits from resilience investments (e.g., sea walls, early warning systems), improving their near-term safety and livelihoods. Their vulnerability makes them dependent on these interventions, even if they come at a long-term cost.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__adaptation_priority, present_vulnerable_populations, beneficiary,
    moderate, immediate, trapped, local).

% Benefit from avoiding costly and politically difficult mitigation efforts, allowing continued economic growth and consumption patterns. They front-load adaptation costs, which are often less disruptive to current economic structures than deep decarbonization.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__adaptation_priority, global_north_economies, beneficiary,
    institutional, biographical, mobile, global).

% Bear the long-term costs of higher warming trajectories, including increased frequency and intensity of extreme weather, resource scarcity, and ecosystem collapse. They have no voice in current policy decisions.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__adaptation_priority, future_generations, payer,
    powerless, generational, trapped, universal).

% Experience disproportionate harm from climate change due to limited resources and infrastructure for adaptation. They receive some resilience funding but face residual and escalating impacts that outstrip their capacity to cope.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__adaptation_priority, low_adaptation_capacity_regions, payer,
    powerless, biographical, trapped, regional).

% Provide projections of warming trajectories and impacts, often highlighting the long-term risks of insufficient mitigation. Their scientific findings inform policy but do not directly dictate it.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__adaptation_priority, climate_scientists, observer,
    analytical, generational, analytical, global).

% Argue for aggressive emissions reductions as the primary response to climate change, viewing adaptation as a secondary or insufficient measure. Their proposals are often deemed politically or economically unfeasible within the dominant policy discourse.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__adaptation_priority, mitigation_advocates, excluded,
    moderate, generational, constrained, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates immediate responses to climate impacts, directing resources to build resilience in vulnerable areas and manage unavoidable climate risks, thereby preventing near-term humanitarian crises and economic disruptions.
% TRANSFER_FUNCTION: Transfers resources and attention from long-term, systemic mitigation efforts to near-term, localized adaptation projects. It also transfers the burden of future climate harms from present generations to future ones, and from high-emitting nations to low-adaptation-capacity regions.
% ABSENT_VOICES: Future generations are structurally absent from the policy-making process, unable to advocate for their interests in a lower warming trajectory. Mitigation advocates are often marginalized in policy discussions that prioritize 'pragmatic' adaptation over 'radical' systemic change.
% DISAPPEARANCE_RATIONALE: If this prioritization vanished, the immediate focus on adaptation would dissolve. Resources would likely shift towards more aggressive mitigation strategies, leading to significant economic restructuring in the Global North and potentially leaving present vulnerable populations without immediate resilience support, causing near-term crises.
% FOUNDING_PROBLEM: The immediate and visible impacts of climate change (e.g., extreme weather events, sea-level rise) demand urgent action, while deep decarbonization is perceived as politically and economically unfeasible in the short to medium term.
% FOUNDING_PROBLEM_CORROBORATION: Governments and international development agencies attest to the live problem of immediate climate impacts and the political difficulty of rapid mitigation. Climate scientists corroborate the increasing frequency of extreme events, while economists and political scientists highlight the barriers to rapid, large-scale mitigation within current political-economic systems.
narrative_ontology:disappearance_verdict(climate_harm_prevention__adaptation_priority, world_rearranges).
narrative_ontology:founding_problem_status(climate_harm_prevention__adaptation_priority, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(climate_harm_prevention__adaptation_priority, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(climate_harm_prevention__adaptation_priority, 'none', 1).
narrative_ontology:epsilon_provenance(climate_harm_prevention__adaptation_priority, 0.65, 'gemini-2.5-flash', 'none', direct).

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
 *   Extractiveness is high (0.65) because the policy choice imposes significant uncompensated costs on future generations and low-adaptation-capacity regions. Suppression (0.70) is also high, as the political and economic structures that deem mitigation 'infeasible' actively suppress alternative policy pathways. Theater ratio (0.20) is moderate; while genuine adaptation efforts occur, a portion of the discourse and funding serves to deflect from more fundamental mitigation responsibilities. The increasing trend in extractiveness and suppression reflects the growing divergence between the stated goal of 'climate harm prevention' and the actual outcomes of adaptation-first policies.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of present vulnerable populations, this constraint offers crucial, immediate support. From the perspective of future generations, it represents a profound intergenerational injustice. The 'adaptation priority' framing coordinates immediate action but obscures the long-term extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   Present vulnerable populations are beneficiaries of immediate adaptation funding, though they remain victims of residual climate impacts. Global North economies are beneficiaries as they avoid disruptive mitigation. Future generations and low-adaptation-capacity regions are clear victims, bearing the escalating costs of a warmer world. Mitigation advocates are excluded, their proposals deemed impractical. This structural asymmetry drives the high extractiveness.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate to 'prevent climate harm' is increasingly atrophied in its mitigation component. While adaptation is a live problem, the acceptance of higher warming trajectories means the original mandate of comprehensive harm prevention is only partially met, with the 'infeasibility' argument serving to justify the shift in burden. This prevents mislabeling it as pure coordination by highlighting the unaddressed and growing harm.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    political_economic_infeasibility_ambiguity,
    'Is the perceived ''political/economic infeasibility'' of aggressive mitigation an objective constraint, or a constructed narrative that serves the interests of current economic structures?',
    'Comparative analysis of policy outcomes in jurisdictions that have successfully implemented aggressive mitigation, or detailed economic modeling of alternative transition pathways that challenge current assumptions.',
    'If constructed, the suppression metric is higher than currently assessed, as it relies on ideological rather than structural barriers. This would reclassify the constraint closer to a Snare, as the coordination story (adaptation) would be more clearly cover for extraction (avoiding mitigation costs).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(political_economic_infeasibility_ambiguity, conceptual, 'Ambiguity regarding the true nature of mitigation ''infeasibility''.').

omega_variable(
    intergenerational_equity_framing,
    'To what extent does the ''adaptation priority'' framework explicitly account for intergenerational equity, and how would a stronger ethical weighting of future generations'' rights alter the policy choices?',
    'Policy analysis incorporating intergenerational cost-benefit analysis with a low discount rate, or legal challenges based on the rights of future generations.',
    'A stronger intergenerational equity framing would likely shift policy towards more aggressive mitigation, reducing the extractiveness from future generations and potentially reclassifying the constraint towards a Rope or Scaffold if the transition is managed equitably.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(intergenerational_equity_framing, preference, 'The role of intergenerational ethics in shaping climate policy priorities.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_harm_prevention__adaptation_priority, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clim_tr_t0, climate_harm_prevention__adaptation_priority, theater_ratio, 0, 0.1).
narrative_ontology:measurement(clim_tr_t10, climate_harm_prevention__adaptation_priority, theater_ratio, 10, 0.15).
narrative_ontology:measurement(clim_tr_t20, climate_harm_prevention__adaptation_priority, theater_ratio, 20, 0.2).
narrative_ontology:measurement(clim_tr_t30, climate_harm_prevention__adaptation_priority, theater_ratio, 30, 0.25).
narrative_ontology:measurement(clim_tr_t40, climate_harm_prevention__adaptation_priority, theater_ratio, 40, 0.28).
narrative_ontology:measurement(clim_tr_t50, climate_harm_prevention__adaptation_priority, theater_ratio, 50, 0.3).

% Extraction over time
narrative_ontology:measurement(clim_be_t0, climate_harm_prevention__adaptation_priority, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(clim_be_t10, climate_harm_prevention__adaptation_priority, base_extractiveness, 10, 0.55).
narrative_ontology:measurement(clim_be_t20, climate_harm_prevention__adaptation_priority, base_extractiveness, 20, 0.6).
narrative_ontology:measurement(clim_be_t30, climate_harm_prevention__adaptation_priority, base_extractiveness, 30, 0.65).
narrative_ontology:measurement(clim_be_t40, climate_harm_prevention__adaptation_priority, base_extractiveness, 40, 0.68).
narrative_ontology:measurement(clim_be_t50, climate_harm_prevention__adaptation_priority, base_extractiveness, 50, 0.7).

% Suppression requirement over time
narrative_ontology:measurement(clim_su_t0, climate_harm_prevention__adaptation_priority, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(clim_su_t10, climate_harm_prevention__adaptation_priority, suppression_requirement, 10, 0.63).
narrative_ontology:measurement(clim_su_t20, climate_harm_prevention__adaptation_priority, suppression_requirement, 20, 0.66).
narrative_ontology:measurement(clim_su_t30, climate_harm_prevention__adaptation_priority, suppression_requirement, 30, 0.7).
narrative_ontology:measurement(clim_su_t40, climate_harm_prevention__adaptation_priority, suppression_requirement, 40, 0.72).
narrative_ontology:measurement(clim_su_t50, climate_harm_prevention__adaptation_priority, suppression_requirement, 50, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_harm_prevention__adaptation_priority, resource_allocation).
narrative_ontology:affects_constraint(climate_harm_prevention__adaptation_priority, climate_harm_prevention__mitigation_priority).
narrative_ontology:affects_constraint(climate_harm_prevention__adaptation_priority, climate_harm_prevention__degrowth_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'climate_harm_prevention' kernel. It prioritizes adaptation, influencing (and being influenced by) the mitigation-first and degrowth readings through resource allocation and policy discourse.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
