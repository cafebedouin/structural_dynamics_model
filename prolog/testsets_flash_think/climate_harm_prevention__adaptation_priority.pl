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
 *   human_readable: Climate Response: Adaptation Priority
 *   domain: climate_policy/political_economy/intergenerational_ethics
 *
 * SUMMARY:
 *   This constraint represents the 'adaptation priority' reading of the
 *   broader 'climate harm prevention' kernel. It frames climate response as
 *   primarily focused on building resilience to current and near-term climate
 *   impacts, accepting that aggressive mitigation is politically and
 *   economically infeasible. This approach implicitly accepts a higher
 *   warming trajectory, shifting the burden of unmitigated climate change
 *   onto future generations and regions with limited adaptive capacity. The
 *   constraint coordinates significant resources towards adaptation efforts
 *   but does so by suppressing alternative mitigation-focused strategies.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_harm_prevention__adaptation_priority, 0.8).
domain_priors:suppression_score(climate_harm_prevention__adaptation_priority, 0.7).
domain_priors:theater_ratio(climate_harm_prevention__adaptation_priority, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_harm_prevention__adaptation_priority, extractiveness, 0.8).
narrative_ontology:constraint_metric(climate_harm_prevention__adaptation_priority, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(climate_harm_prevention__adaptation_priority, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(climate_harm_prevention__adaptation_priority, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(climate_harm_prevention__adaptation_priority, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_harm_prevention__adaptation_priority, tangled_rope).
narrative_ontology:human_readable(climate_harm_prevention__adaptation_priority, "Climate Response: Adaptation Priority").
narrative_ontology:topic_domain(climate_harm_prevention__adaptation_priority, "climate_policy/political_economy/intergenerational_ethics").

domain_priors:requires_active_enforcement(climate_harm_prevention__adaptation_priority).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_harm_prevention__adaptation_priority, '2a373d8e-19d1-4e2e-a034-839170f1dc40').
narrative_ontology:cs_kernel_codification('2a373d8e-19d1-4e2e-a034-839170f1dc40', formalized).
narrative_ontology:cs_authority_grounding('2a373d8e-19d1-4e2e-a034-839170f1dc40', practice).
narrative_ontology:cs_interpretation_layer_present('2a373d8e-19d1-4e2e-a034-839170f1dc40').
narrative_ontology:cs_reading_relation('2a373d8e-19d1-4e2e-a034-839170f1dc40', climate_harm_prevention__mitigation_priority, coexists_with).
narrative_ontology:cs_reading_relation('2a373d8e-19d1-4e2e-a034-839170f1dc40', climate_harm_prevention__degrowth_reading, coexists_with).
narrative_ontology:cs_axiom('2a373d8e-19d1-4e2e-a034-839170f1dc40', foundational, near_term_human_welfare_priority).
narrative_ontology:cs_axiom_status(near_term_human_welfare_priority, holdable).
narrative_ontology:cs_axiom_grounding('2a373d8e-19d1-4e2e-a034-839170f1dc40', near_term_human_welfare_priority, deontological).
narrative_ontology:cs_axiom('2a373d8e-19d1-4e2e-a034-839170f1dc40', foundational, mitigation_political_economic_infeasibility).
narrative_ontology:cs_axiom_status(mitigation_political_economic_infeasibility, holdable).
narrative_ontology:cs_axiom_grounding('2a373d8e-19d1-4e2e-a034-839170f1dc40', mitigation_political_economic_infeasibility, empirically_contingent).
narrative_ontology:cs_reference_frame('2a373d8e-19d1-4e2e-a034-839170f1dc40', present_day_political_economic_realities).
narrative_ontology:cs_drift_state('2a373d8e-19d1-4e2e-a034-839170f1dc40', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('2a373d8e-19d1-4e2e-a034-839170f1dc40', '').
narrative_ontology:cs_kernel_id(climate_harm_prevention__adaptation_priority, climate_harm_prevention).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_harm_prevention__adaptation_priority, present_vulnerable_populations).
narrative_ontology:constraint_beneficiary(climate_harm_prevention__adaptation_priority, resilience_building_industries).
narrative_ontology:constraint_victim(climate_harm_prevention__adaptation_priority, future_generations).
narrative_ontology:constraint_victim(climate_harm_prevention__adaptation_priority, low_adaptation_capacity_regions).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Receive immediate support and resources for climate resilience, such as flood defenses, early warning systems, and drought-resistant agriculture. While benefiting from these efforts, they remain exposed to residual climate impacts from unmitigated warming.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__adaptation_priority, present_vulnerable_populations, beneficiary,
    powerless, immediate, constrained, local).

% Profit from contracts, investments, and policy incentives related to adaptation infrastructure, climate-resilient technologies, and disaster response services. They are key drivers and beneficiaries of the adaptation-first approach.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__adaptation_priority, resilience_building_industries, beneficiary,
    organized, biographical, arbitrage, global).

% Implement policies, allocate budgets for adaptation, and frame the political and economic feasibility of climate action. They prioritize adaptation as a pragmatic response to immediate pressures, often citing the difficulty of achieving deep mitigation.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__adaptation_priority, national_governments, agenda_setter,
    institutional, biographical, constrained, national).

% Bear the long-term, unmitigated costs of a higher warming trajectory, including increased frequency and intensity of extreme weather events, resource scarcity, ecosystem collapse, and potential societal instability. They have no voice in current policy decisions.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__adaptation_priority, future_generations, payer,
    powerless, generational, trapped, universal).

% Often located in the Global South, these regions lack sufficient resources and infrastructure to adapt effectively to climate change. They disproportionately suffer the impacts of accepted warming, despite contributing least to its causes.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__adaptation_priority, low_adaptation_capacity_regions, payer,
    powerless, generational, trapped, regional).

% Argue for aggressive emissions reductions and systemic changes to prevent future harm, but their proposals are often deemed politically or economically infeasible by the dominant policy frame, leading to their marginalization in policy debates.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__adaptation_priority, mitigation_advocates, excluded,
    organized, biographical, constrained, global).

% Propose planned economic contraction in the Global North as a necessary condition for effective mitigation, but their ideas are largely dismissed as unrealistic or radical within mainstream policy discourse.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__adaptation_priority, degrowth_advocates, excluded,
    organized, biographical, constrained, global).

% Provide data, models, and projections on climate impacts and mitigation pathways. While their scientific findings are acknowledged, their warnings about long-term risks and the urgency of mitigation are often deprioritized in favor of adaptation.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__adaptation_priority, climate_scientists, observer,
    analytical, biographical, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates national and international efforts to build resilience against the impacts of climate change, allocating resources to protect vulnerable populations and infrastructure from immediate threats.
% TRANSFER_FUNCTION: Transfers resources (funding, technology, expertise) to present vulnerable populations and resilience industries, while implicitly transferring the unmitigated costs of a higher warming trajectory to future generations and low-adaptation-capacity regions.
% ABSENT_VOICES: Future generations and low-adaptation-capacity regions are structurally absent from the policy-making process, bearing costs without representation. Mitigation and degrowth advocates are actively excluded from the dominant policy discourse that prioritizes adaptation.
% DISAPPEARANCE_RATIONALE: If the policy framework prioritizing adaptation vanished, there would be a significant re-evaluation of climate strategy. Resources currently allocated to resilience would be re-directed, potentially towards more aggressive mitigation, and the political framing of 'infeasibility' would lose its grounding, leading to a reorganization of climate action and international burden-sharing debates.
% FOUNDING_PROBLEM: The immediate and visible impacts of climate change (e.g., extreme weather events, sea-level rise) posed an urgent threat to human lives, infrastructure, and economic stability, while deep emissions cuts were perceived as politically and economically challenging.
% FOUNDING_PROBLEM_CORROBORATION: The urgency of climate impacts is widely corroborated by scientific consensus and observed events. The political/economic infeasibility of aggressive mitigation is attested by national governments and industry, though contested by mitigation and degrowth advocates, and implicitly challenged by scientific assessments of necessary emissions reductions.
narrative_ontology:disappearance_verdict(climate_harm_prevention__adaptation_priority, world_rearranges).
narrative_ontology:founding_problem_status(climate_harm_prevention__adaptation_priority, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(climate_harm_prevention__adaptation_priority, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(climate_harm_prevention__adaptation_priority, 'none', 1).
narrative_ontology:epsilon_provenance(climate_harm_prevention__adaptation_priority, 0.8, 'gemini-2.5-flash', 'none', direct).

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
 *   The high extractiveness (0.80) reflects the substantial unmitigated costs borne by future generations and vulnerable regions due to the accepted higher warming trajectory. Suppression (0.70) is significant because the 'infeasibility' narrative actively marginalizes and suppresses calls for more aggressive mitigation or systemic change. Theater ratio (0.20) is relatively low, as resilience building involves genuine, tangible efforts, but the framing of mitigation's infeasibility can have performative elements. The claimed type is Tangled Rope because it genuinely coordinates adaptation efforts for some (present vulnerable populations, resilience industries) but simultaneously extracts from others (future generations, low-adaptation-capacity regions) through the same policy structure, requiring active enforcement of resource allocation and policy choices.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of present vulnerable populations and resilience industries, this constraint appears as a necessary and beneficial coordination mechanism. However, from the perspective of future generations and low-adaptation-capacity regions, it functions as a highly extractive mechanism that shifts burdens and perpetuates harm. Climate scientists, while acknowledging the need for adaptation, often highlight the long-term risks of insufficient mitigation, creating a tension with the dominant policy frame.
 *
 * DIRECTIONALITY LOGIC:
 *   Present vulnerable populations and resilience-building industries are beneficiaries, receiving direct resources and profits from adaptation efforts. Future generations and low-adaptation-capacity regions are the primary targets, bearing the long-term, unmitigated costs. National governments act as agenda-setters, directing resources and framing the policy discourse. Mitigation and degrowth advocates are excluded, their alternatives suppressed by the dominant narrative.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is this constraint accurately representing the ''adaptation_priority'' reading of the ''climate_harm_prevention'' kernel?',
    'Comparison with other instantiated readings of the same kernel and expert review of the core tenets of each climate response philosophy.',
    'If misidentified, the analysis of inter-reading relations and the overall coherence of the climate harm prevention kernel would be compromised.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Confirms the specific reading being instantiated from the climate harm prevention kernel.').

omega_variable(
    mitigation_infeasibility_objectivity,
    'Is the ''political/economic infeasibility'' of aggressive mitigation an objective, irreducible constraint, or a constructed narrative that serves to protect existing economic and political structures?',
    'Comparative policy analysis across different political systems, economic modeling of alternative mitigation pathways, and historical case studies of rapid societal transitions in response to crises. If alternative pathways are shown to be feasible, the ''infeasibility'' claim is weakened.',
    'If the infeasibility is largely constructed, the suppression metric for this constraint would be higher, and its classification would lean more strongly towards Snare, as the coordination story would be revealed as a cover for extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mitigation_infeasibility_objectivity, empirical, 'Assesses the true nature of the claimed barriers to mitigation.').

omega_variable(
    intergenerational_equity_justification,
    'Is the implicit transfer of unmitigated climate costs to future generations and low-adaptation-capacity regions ethically justifiable, given the benefits accrued by present populations and industries?',
    'Ethical deliberation, intergenerational equity frameworks, and public discourse that explicitly weighs the moral implications of current climate policy choices. This is a normative question without a purely empirical resolution.',
    'If deemed unjustifiable, the extractiveness of the constraint would be viewed as morally illegitimate, potentially increasing resistance and calls for systemic change, even if the structural classification remains Tangled Rope.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(intergenerational_equity_justification, preference, 'Examines the ethical basis of burden-sharing in climate policy.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_harm_prevention__adaptation_priority, 2000, 2050).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clim_tr_t2000, climate_harm_prevention__adaptation_priority, theater_ratio, 2000, 0.1).
narrative_ontology:measurement(clim_tr_t2010, climate_harm_prevention__adaptation_priority, theater_ratio, 2010, 0.15).
narrative_ontology:measurement(clim_tr_t2020, climate_harm_prevention__adaptation_priority, theater_ratio, 2020, 0.2).
narrative_ontology:measurement(clim_tr_t2030, climate_harm_prevention__adaptation_priority, theater_ratio, 2030, 0.25).
narrative_ontology:measurement(clim_tr_t2040, climate_harm_prevention__adaptation_priority, theater_ratio, 2040, 0.28).
narrative_ontology:measurement(clim_tr_t2050, climate_harm_prevention__adaptation_priority, theater_ratio, 2050, 0.3).

% Extraction over time
narrative_ontology:measurement(clim_be_t2000, climate_harm_prevention__adaptation_priority, base_extractiveness, 2000, 0.65).
narrative_ontology:measurement(clim_be_t2010, climate_harm_prevention__adaptation_priority, base_extractiveness, 2010, 0.7).
narrative_ontology:measurement(clim_be_t2020, climate_harm_prevention__adaptation_priority, base_extractiveness, 2020, 0.75).
narrative_ontology:measurement(clim_be_t2030, climate_harm_prevention__adaptation_priority, base_extractiveness, 2030, 0.8).
narrative_ontology:measurement(clim_be_t2040, climate_harm_prevention__adaptation_priority, base_extractiveness, 2040, 0.83).
narrative_ontology:measurement(clim_be_t2050, climate_harm_prevention__adaptation_priority, base_extractiveness, 2050, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(clim_su_t2000, climate_harm_prevention__adaptation_priority, suppression_requirement, 2000, 0.55).
narrative_ontology:measurement(clim_su_t2010, climate_harm_prevention__adaptation_priority, suppression_requirement, 2010, 0.6).
narrative_ontology:measurement(clim_su_t2020, climate_harm_prevention__adaptation_priority, suppression_requirement, 2020, 0.65).
narrative_ontology:measurement(clim_su_t2030, climate_harm_prevention__adaptation_priority, suppression_requirement, 2030, 0.7).
narrative_ontology:measurement(clim_su_t2040, climate_harm_prevention__adaptation_priority, suppression_requirement, 2040, 0.73).
narrative_ontology:measurement(clim_su_t2050, climate_harm_prevention__adaptation_priority, suppression_requirement, 2050, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_harm_prevention__adaptation_priority, resource_allocation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
