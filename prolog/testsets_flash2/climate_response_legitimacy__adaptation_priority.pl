% ============================================================================
% CONSTRAINT STORY: climate_response_legitimacy__adaptation_priority
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_climate_response_legitimacy__adaptation_priority, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: climate_response_legitimacy__adaptation_priority
 *   human_readable: Climate Response: Adaptation Priority Reading
 *   domain: climate_policy/political_economy/intergenerational_ethics
 *
 * SUMMARY:
 *   This constraint represents the 'adaptation priority' reading of
 *   legitimate climate response, where the focus is on protecting vulnerable
 *   populations from the impacts of an accepted warming trajectory through
 *   resilience infrastructure and adaptive capacity. This approach implicitly
 *   allows wealthy nations to maintain their development models, deferring
 *   aggressive mitigation and shifting the burden of climate change onto
 *   vulnerable regions and future generations. The constraint is claimed as a
 *   'tangled_rope' because it provides a genuine coordination function
 *   (adaptation funding) but is coupled with asymmetric extraction (continued
 *   emissions, deferred costs).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_response_legitimacy__adaptation_priority, 0.68).
domain_priors:suppression_score(climate_response_legitimacy__adaptation_priority, 0.75).
domain_priors:theater_ratio(climate_response_legitimacy__adaptation_priority, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_response_legitimacy__adaptation_priority, extractiveness, 0.68).
narrative_ontology:constraint_metric(climate_response_legitimacy__adaptation_priority, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(climate_response_legitimacy__adaptation_priority, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(climate_response_legitimacy__adaptation_priority, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(climate_response_legitimacy__adaptation_priority, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_response_legitimacy__adaptation_priority, tangled_rope).
narrative_ontology:human_readable(climate_response_legitimacy__adaptation_priority, "Climate Response: Adaptation Priority Reading").
narrative_ontology:topic_domain(climate_response_legitimacy__adaptation_priority, "climate_policy/political_economy/intergenerational_ethics").

domain_priors:requires_active_enforcement(climate_response_legitimacy__adaptation_priority).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_response_legitimacy__adaptation_priority, '4bcbe260-7a84-4762-8b2f-c9a7388b0399').
narrative_ontology:cs_kernel_codification('4bcbe260-7a84-4762-8b2f-c9a7388b0399', distributed).
narrative_ontology:cs_authority_grounding('4bcbe260-7a84-4762-8b2f-c9a7388b0399', extraction).
narrative_ontology:cs_interpretation_layer_present('4bcbe260-7a84-4762-8b2f-c9a7388b0399').
narrative_ontology:cs_reading_relation('4bcbe260-7a84-4762-8b2f-c9a7388b0399', climate_response_legitimacy__mitigation_priority, influences).
narrative_ontology:cs_reading_relation('4bcbe260-7a84-4762-8b2f-c9a7388b0399', climate_response_legitimacy__degrowth_transformation, influences).
narrative_ontology:cs_axiom('4bcbe260-7a84-4762-8b2f-c9a7388b0399', foundational, economic_growth_is_non_negotiable).
narrative_ontology:cs_axiom_status(economic_growth_is_non_negotiable, holdable).
narrative_ontology:cs_axiom_grounding('4bcbe260-7a84-4762-8b2f-c9a7388b0399', economic_growth_is_non_negotiable, conventional).
narrative_ontology:cs_axiom('4bcbe260-7a84-4762-8b2f-c9a7388b0399', foundational, adaptation_is_primary_moral_duty).
narrative_ontology:cs_axiom_status(adaptation_is_primary_moral_duty, holdable).
narrative_ontology:cs_axiom_grounding('4bcbe260-7a84-4762-8b2f-c9a7388b0399', adaptation_is_primary_moral_duty, deontological).
narrative_ontology:cs_reference_frame('4bcbe260-7a84-4762-8b2f-c9a7388b0399', pragmatic_climate_realism).
narrative_ontology:cs_drift_state('4bcbe260-7a84-4762-8b2f-c9a7388b0399', contemporary_climate_crisis, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('4bcbe260-7a84-4762-8b2f-c9a7388b0399', '').
narrative_ontology:cs_kernel_id(climate_response_legitimacy__adaptation_priority, climate_response_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_response_legitimacy__adaptation_priority, wealthy_nations).
narrative_ontology:constraint_beneficiary(climate_response_legitimacy__adaptation_priority, fossil_fuel_industries).
narrative_ontology:constraint_victim(climate_response_legitimacy__adaptation_priority, vulnerable_populations_global_south).
narrative_ontology:constraint_victim(climate_response_legitimacy__adaptation_priority, future_generations).
narrative_ontology:constraint_vindicates(climate_response_legitimacy__adaptation_priority, economic_growth_imperative).
narrative_ontology:constraint_vindicates(climate_response_legitimacy__adaptation_priority, national_sovereignty_over_resources).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Advocate for and fund adaptation measures in vulnerable regions, while largely maintaining their own economic growth models and consumption patterns. They benefit from deferring costly mitigation efforts and preserving current economic structures.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__adaptation_priority, wealthy_nations, agenda_setter,
    institutional, generational, mobile, global).

% Bear the immediate and escalating costs of climate impacts, despite receiving adaptation funding. The funding is often insufficient, conditional, and does not address the root causes of their vulnerability or the ongoing emissions from wealthy nations. They are trapped by geography and limited resources.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__adaptation_priority, vulnerable_populations_global_south, payer,
    powerless, immediate, trapped, regional).

% Will inherit a world with higher warming trajectories and compounded impacts due to deferred mitigation. They bear the long-term costs of current adaptation-focused policies that do not sufficiently reduce emissions.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__adaptation_priority, future_generations, payer,
    powerless, civilizational, trapped, universal).

% Benefit from a policy framework that prioritizes adaptation over aggressive mitigation, allowing for continued extraction and combustion of fossil fuels. They actively lobby against policies that would constrain their operations.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__adaptation_priority, fossil_fuel_industries, beneficiary,
    organized, biographical, arbitrage, global).

% Provide data and models on warming trajectories and impacts, often highlighting the inadequacy of adaptation alone without significant mitigation. Their role is to inform, but their findings are often selectively used or downplayed by political actors.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__adaptation_priority, climate_scientists, observer,
    analytical, generational, analytical, global).

% Administer and distribute adaptation funding, often balancing the priorities of donor nations with the urgent needs on the ground. They are constrained by political mandates and funding levels set by wealthy nations.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__adaptation_priority, international_development_agencies, agenda_setter,
    institutional, biographical, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(climate_response_legitimacy__adaptation_priority, wealthy_nations).
narrative_ontology:fixing_cost_class(climate_response_legitimacy__adaptation_priority, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates international efforts to build resilience and adaptive capacity in regions most affected by climate change, preventing immediate humanitarian crises and managing unavoidable impacts.
% TRANSFER_FUNCTION: Transfers financial resources and technical expertise from wealthy nations to vulnerable regions for adaptation projects, while implicitly transferring the burden of emissions reduction to future generations and the costs of unmitigated warming to vulnerable populations.
% ABSENT_VOICES: Indigenous communities and ecological systems, whose intrinsic value and long-term well-being are often marginalized in policy discussions focused on economic and human infrastructure. They would advocate for deeper systemic change and recognition of non-human rights.
% DISAPPEARANCE_RATIONALE: If this adaptation-priority framework vanished, the immediate consequence would be a catastrophic increase in climate-related disasters and humanitarian crises in vulnerable regions, as existing (albeit insufficient) support structures would collapse. The global political economy of climate action would be forced to re-evaluate its foundational assumptions.
% FOUNDING_PROBLEM: The recognition that some level of climate change is unavoidable due to past emissions, necessitating measures to protect human lives and livelihoods from immediate and escalating impacts.
% FOUNDING_PROBLEM_CORROBORATION: The scientific consensus on unavoidable warming and the increasing frequency and intensity of extreme weather events, corroborated by reports from the IPCC, UN agencies, and local communities experiencing direct impacts, attests to the live nature of the problem. However, the adequacy of the 'adaptation priority' as a solution is contested by many.
narrative_ontology:disappearance_verdict(climate_response_legitimacy__adaptation_priority, world_rearranges).
narrative_ontology:founding_problem_status(climate_response_legitimacy__adaptation_priority, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(climate_response_legitimacy__adaptation_priority, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(climate_response_legitimacy__adaptation_priority, 'none', 1).
narrative_ontology:epsilon_provenance(climate_response_legitimacy__adaptation_priority, 0.68, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(climate_response_legitimacy__adaptation_priority_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(climate_response_legitimacy__adaptation_priority, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(climate_response_legitimacy__adaptation_priority_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.68) because the adaptation deficit for vulnerable regions is substantial ($350B gap), and the costs of unmitigated warming are borne disproportionately by those least responsible. Suppression (0.75) is high due to the structural power imbalance that allows wealthy nations to dictate the terms of climate response, effectively suppressing demands for more radical mitigation. Theater ratio (0.40) is moderate and rising, reflecting the increasing performative aspect of adaptation funding as a substitute for genuine emissions reduction. The rising trend in extractiveness and suppression over time indicates an intensifying burden on victims as warming progresses.
 *
 * PERSPECTIVAL GAP:
 *   Wealthy nations and fossil fuel industries perceive this as a pragmatic and responsible approach, balancing economic stability with climate action. Vulnerable populations and future generations experience it as a form of climate injustice, where their well-being is sacrificed for the continued prosperity of others. The engine's per-seat classification will reflect this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Wealthy nations and fossil fuel industries are beneficiaries (low d) as they maintain their economic models and defer costly transitions. Vulnerable populations and future generations are clear victims (high d) as they bear the brunt of impacts and deferred costs. International development agencies are agenda-setters, mediating the flow of resources but constrained by the priorities of donor nations.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification prevents mislabeling the adaptation-priority framework as a pure 'rope' (genuine coordination) by highlighting its extractive components. The rising extractiveness and suppression, coupled with the increasing theater ratio, suggest a drift towards a 'snare' if the coordination function becomes entirely performative and the extraction intensifies without genuine adaptation outcomes. The 'tangled_rope' classification captures the hybrid nature of both coordination and extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    adaptation_funding_sufficiency,
    'Is the current and projected level of adaptation funding sufficient to protect vulnerable populations from the accepted warming trajectory?',
    'Independent assessment of adaptation needs vs. funding flows, tracking of climate-related loss and damage, and evaluation of adaptation project effectiveness.',
    'If funding is found to be consistently insufficient, the ''coordination'' aspect of the constraint is further undermined, pushing it closer to a pure ''snare'' by exposing the adaptation narrative as cover for extraction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(adaptation_funding_sufficiency, empirical, 'Whether adaptation funding meets actual needs.').

omega_variable(
    intergenerational_equity_framing,
    'Is the deferral of aggressive mitigation an acceptable intergenerational trade-off, or does it constitute an unjust burden on future generations?',
    'Ethical and philosophical analysis of intergenerational justice, coupled with economic modeling of long-term climate damages and adaptation costs.',
    'If deemed an unjust burden, the ''legitimacy'' claim of this climate response reading is fundamentally challenged, reclassifying it as a ''snare'' from the perspective of future generations, regardless of current adaptation efforts.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(intergenerational_equity_framing, conceptual, 'Ethical justification of intergenerational burden sharing.').

omega_variable(
    structural_vs_internalized_suppression,
    'To what extent is the suppression of demands for aggressive mitigation structural (e.g., lobbying power, economic inertia) versus internalized (e.g., belief in technological solutions, climate fatalism)?',
    'Analysis of political economy of climate policy, media discourse analysis, and surveys of public and elite opinion on climate action. If suppression persists after structural barriers are reduced, it suggests internalized components.',
    'If internalized suppression is significant, the effective suppression is higher than the structural measure suggests, as targets carry the suppression with them. This would amplify the ''snare'' characteristics of the constraint.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(structural_vs_internalized_suppression, empirical, 'Structural vs. internalized suppression mechanism for climate action.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_response_legitimacy__adaptation_priority, 2000, 2050).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clim_tr_t2000, climate_response_legitimacy__adaptation_priority, theater_ratio, 2000, 0.25).
narrative_ontology:measurement(clim_tr_t2010, climate_response_legitimacy__adaptation_priority, theater_ratio, 2010, 0.3).
narrative_ontology:measurement(clim_tr_t2020, climate_response_legitimacy__adaptation_priority, theater_ratio, 2020, 0.35).
narrative_ontology:measurement(clim_tr_t2030, climate_response_legitimacy__adaptation_priority, theater_ratio, 2030, 0.4).
narrative_ontology:measurement_basis(clim_tr_t2030, projected).
narrative_ontology:measurement(clim_tr_t2040, climate_response_legitimacy__adaptation_priority, theater_ratio, 2040, 0.45).
narrative_ontology:measurement_basis(clim_tr_t2040, projected).
narrative_ontology:measurement(clim_tr_t2050, climate_response_legitimacy__adaptation_priority, theater_ratio, 2050, 0.5).
narrative_ontology:measurement_basis(clim_tr_t2050, projected).

% Extraction over time
narrative_ontology:measurement(clim_be_t2000, climate_response_legitimacy__adaptation_priority, base_extractiveness, 2000, 0.55).
narrative_ontology:measurement(clim_be_t2010, climate_response_legitimacy__adaptation_priority, base_extractiveness, 2010, 0.6).
narrative_ontology:measurement(clim_be_t2020, climate_response_legitimacy__adaptation_priority, base_extractiveness, 2020, 0.65).
narrative_ontology:measurement(clim_be_t2030, climate_response_legitimacy__adaptation_priority, base_extractiveness, 2030, 0.68).
narrative_ontology:measurement_basis(clim_be_t2030, projected).
narrative_ontology:measurement(clim_be_t2040, climate_response_legitimacy__adaptation_priority, base_extractiveness, 2040, 0.7).
narrative_ontology:measurement_basis(clim_be_t2040, projected).
narrative_ontology:measurement(clim_be_t2050, climate_response_legitimacy__adaptation_priority, base_extractiveness, 2050, 0.72).
narrative_ontology:measurement_basis(clim_be_t2050, projected).

% Suppression requirement over time
narrative_ontology:measurement(clim_su_t2000, climate_response_legitimacy__adaptation_priority, suppression_requirement, 2000, 0.6).
narrative_ontology:measurement(clim_su_t2010, climate_response_legitimacy__adaptation_priority, suppression_requirement, 2010, 0.65).
narrative_ontology:measurement(clim_su_t2020, climate_response_legitimacy__adaptation_priority, suppression_requirement, 2020, 0.7).
narrative_ontology:measurement(clim_su_t2030, climate_response_legitimacy__adaptation_priority, suppression_requirement, 2030, 0.75).
narrative_ontology:measurement_basis(clim_su_t2030, projected).
narrative_ontology:measurement(clim_su_t2040, climate_response_legitimacy__adaptation_priority, suppression_requirement, 2040, 0.78).
narrative_ontology:measurement_basis(clim_su_t2040, projected).
narrative_ontology:measurement(clim_su_t2050, climate_response_legitimacy__adaptation_priority, suppression_requirement, 2050, 0.8).
narrative_ontology:measurement_basis(clim_su_t2050, projected).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_response_legitimacy__adaptation_priority, resource_allocation).
narrative_ontology:boltzmann_floor_override(climate_response_legitimacy__adaptation_priority, 0.15).
narrative_ontology:affects_constraint(climate_response_legitimacy__adaptation_priority, climate_response_legitimacy__mitigation_priority).
narrative_ontology:affects_constraint(climate_response_legitimacy__adaptation_priority, climate_response_legitimacy__degrowth_transformation).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'climate_response_legitimacy' kernel. Its focus on adaptation influences the political and resource landscape for both mitigation and degrowth approaches, often by diverting attention and resources.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
