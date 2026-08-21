% ============================================================================
% CONSTRAINT STORY: climate_harm_prevention__mitigation_priority
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_climate_harm_prevention__mitigation_priority, []).

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
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: climate_harm_prevention__mitigation_priority
 *   human_readable: Climate Harm Prevention: Mitigation Priority within Growth Framework
 *   domain: climate_policy/political_economy/intergenerational_ethics
 *
 * SUMMARY:
 *   This constraint represents the dominant policy paradigm for addressing
 *   climate change: prioritizing emissions reduction through technological
 *   innovation and market mechanisms, while assuming continued economic
 *   growth. It is one reading of the broader 'climate_harm_prevention'
 *   kernel, distinguishing itself from readings that prioritize adaptation or
 *   advocate for degrowth. The framework aims to coordinate global action to
 *   prevent future harm, but its implementation involves significant costs
 *   for carbon-intensive sectors and actively suppresses alternative
 *   approaches that challenge the growth paradigm.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_harm_prevention__mitigation_priority, 0.68).
domain_priors:suppression_score(climate_harm_prevention__mitigation_priority, 0.75).
domain_priors:theater_ratio(climate_harm_prevention__mitigation_priority, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_harm_prevention__mitigation_priority, extractiveness, 0.68).
narrative_ontology:constraint_metric(climate_harm_prevention__mitigation_priority, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(climate_harm_prevention__mitigation_priority, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(climate_harm_prevention__mitigation_priority, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(climate_harm_prevention__mitigation_priority, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_harm_prevention__mitigation_priority, tangled_rope).
narrative_ontology:human_readable(climate_harm_prevention__mitigation_priority, "Climate Harm Prevention: Mitigation Priority within Growth Framework").
narrative_ontology:topic_domain(climate_harm_prevention__mitigation_priority, "climate_policy/political_economy/intergenerational_ethics").

domain_priors:requires_active_enforcement(climate_harm_prevention__mitigation_priority).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_harm_prevention__mitigation_priority, '126140b1-1514-4f66-a8a1-3e3898bd8e54').
narrative_ontology:cs_kernel_codification('126140b1-1514-4f66-a8a1-3e3898bd8e54', formalized).
narrative_ontology:cs_authority_grounding('126140b1-1514-4f66-a8a1-3e3898bd8e54', lineage).
narrative_ontology:cs_interpretation_layer_present('126140b1-1514-4f66-a8a1-3e3898bd8e54').
narrative_ontology:cs_reading_relation('126140b1-1514-4f66-a8a1-3e3898bd8e54', climate_harm_prevention__adaptation_priority, coexists_with).
narrative_ontology:cs_reading_relation('126140b1-1514-4f66-a8a1-3e3898bd8e54', climate_harm_prevention__degrowth_reading, forecloses).
narrative_ontology:cs_axiom('126140b1-1514-4f66-a8a1-3e3898bd8e54', foundational, technological_decoupling_possible).
narrative_ontology:cs_axiom_status(technological_decoupling_possible, holdable).
narrative_ontology:cs_axiom_grounding('126140b1-1514-4f66-a8a1-3e3898bd8e54', technological_decoupling_possible, empirically_contingent).
narrative_ontology:cs_axiom('126140b1-1514-4f66-a8a1-3e3898bd8e54', foundational, intergenerational_equity_primary).
narrative_ontology:cs_axiom_status(intergenerational_equity_primary, holdable).
narrative_ontology:cs_axiom_grounding('126140b1-1514-4f66-a8a1-3e3898bd8e54', intergenerational_equity_primary, deontological).
narrative_ontology:cs_reference_frame('126140b1-1514-4f66-a8a1-3e3898bd8e54', sustainable_development_paradigm).
narrative_ontology:cs_drift_state('126140b1-1514-4f66-a8a1-3e3898bd8e54', contemporary_climate_crisis, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('126140b1-1514-4f66-a8a1-3e3898bd8e54', '').
narrative_ontology:cs_kernel_id(climate_harm_prevention__mitigation_priority, climate_harm_prevention).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_harm_prevention__mitigation_priority, future_generations).
narrative_ontology:constraint_beneficiary(climate_harm_prevention__mitigation_priority, renewable_energy_sector).
narrative_ontology:constraint_victim(climate_harm_prevention__mitigation_priority, fossil_fuel_industries).
narrative_ontology:constraint_victim(climate_harm_prevention__mitigation_priority, carbon_intensive_sectors).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(climate_harm_prevention__mitigation_priority, present_carbon_intensive_sectors).
narrative_ontology:constraint_victim(climate_harm_prevention__mitigation_priority, adaptation_advocates).
narrative_ontology:constraint_vindicates(climate_harm_prevention__mitigation_priority, sustainable_development_goals).
narrative_ontology:constraint_vindicates(climate_harm_prevention__mitigation_priority, intergenerational_equity_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The primary beneficiaries of successful mitigation efforts, as they avoid the most severe impacts of climate change. They have no direct voice or agency in current policy decisions.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__mitigation_priority, future_generations, beneficiary,
    powerless, civilizational, trapped, universal).

% Bear the direct costs of emissions reductions, including investments in new technologies, carbon pricing, and potential stranded assets. They resist rapid transition due to economic disruption and job losses.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__mitigation_priority, present_carbon_intensive_sectors, payer,
    organized, immediate, constrained, global).

% Benefits from policies that prioritize mitigation, as it drives demand for their products and services. They advocate for stronger climate policies and technological transition.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__mitigation_priority, renewable_energy_sector, beneficiary,
    powerful, biographical, mobile, global).

% Provide the foundational data and projections for climate policy. They observe the effectiveness of mitigation efforts and the ongoing climate crisis, often advocating for more ambitious action.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__mitigation_priority, climate_scientists, observer,
    analytical, generational, analytical, global).

% Responsible for designing and implementing policies to achieve emissions reductions. They balance scientific advice, economic interests, and political feasibility within the growth framework.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__mitigation_priority, policy_makers, agenda_setter,
    institutional, biographical, constrained, national).

% Argue that mitigation within a growth framework is insufficient or impossible, advocating for planned economic contraction. Their proposals are largely excluded from mainstream policy discourse.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__mitigation_priority, degrowth_advocates, excluded,
    organized, generational, constrained, global).

% Focus on building resilience to current and unavoidable climate impacts. They often see resources diverted to mitigation efforts, arguing for a rebalancing of priorities, and are sometimes excluded from mitigation-focused policy tables.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__mitigation_priority, adaptation_advocates, payer,
    organized, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(climate_harm_prevention__mitigation_priority, adaptation_advocates, excluded).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(climate_harm_prevention__mitigation_priority, diffuse).
narrative_ontology:fixing_cost_class(climate_harm_prevention__mitigation_priority, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To coordinate global efforts to reduce greenhouse gas emissions, stabilize the climate system, and prevent catastrophic future harm, primarily through technological transition within an economic growth paradigm.
% TRANSFER_FUNCTION: Transfers investment and regulatory burden from future generations (who would bear climate costs) to the present generation, particularly carbon-intensive industries, to fund technological transition and decarbonization.
% ABSENT_VOICES: Degrowth advocates, who argue the growth framework itself is the problem, are largely excluded. Those who prioritize immediate adaptation over long-term mitigation also find their voices marginalized in this framework.
% DISAPPEARANCE_RATIONALE: If this policy framework vanished overnight, global emissions would likely accelerate, leading to more severe climate impacts. The economic and social structures built around decarbonization efforts would collapse or reorient, and the moral imperative for intergenerational equity would lose its primary policy expression.
% FOUNDING_PROBLEM: The existential threat of anthropogenic climate change and its disproportionate impact on future generations, coupled with the perceived need for a viable economic pathway to address it without sacrificing economic development.
% FOUNDING_PROBLEM_CORROBORATION: The Intergovernmental Panel on Climate Change (IPCC) reports, national scientific academies, and international bodies (e.g., UN, World Bank) consistently corroborate the live status of the climate crisis and the necessity of mitigation efforts. This corroboration comes from outside the direct beneficiaries of the growth framework.
narrative_ontology:disappearance_verdict(climate_harm_prevention__mitigation_priority, world_rearranges).
narrative_ontology:founding_problem_status(climate_harm_prevention__mitigation_priority, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(climate_harm_prevention__mitigation_priority, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(climate_harm_prevention__mitigation_priority, 'none', 1).
narrative_ontology:epsilon_provenance(climate_harm_prevention__mitigation_priority, 0.68, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(climate_harm_prevention__mitigation_priority_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(climate_harm_prevention__mitigation_priority, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(climate_harm_prevention__mitigation_priority_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high because the transition costs are substantial and often borne by specific sectors, while the benefits (avoided future harm) are diffuse. Suppression is high due to the active marginalization of alternative climate strategies (like degrowth) that challenge the underlying economic framework. Theater ratio is moderate and rising, reflecting the gap between ambitious policy commitments and actual, often insufficient, emissions reductions, leading to performative actions that do not fully address the problem. Resistance is high from those who bear the costs or advocate for different approaches. The claimed type 'tangled_rope' reflects the genuine coordination function (preventing climate catastrophe) intertwined with asymmetric extraction and active enforcement to maintain the chosen pathway.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of future generations and the renewable energy sector, this constraint is a necessary 'rope' for survival and growth. From the perspective of fossil fuel industries, it's a 'snare' imposing existential costs. Degrowth advocates see it as a 'tangled rope' that coordinates a false solution while extracting from the planet and future generations by perpetuating unsustainable growth. The engine's per-seat classification will capture these divergences.
 *
 * DIRECTIONALITY LOGIC:
 *   Future generations are full beneficiaries, as the constraint aims to secure their well-being. The renewable energy sector also benefits from the policy shift. Fossil fuel and carbon-intensive industries are primary targets, bearing the costs of transition. Policy makers act as agenda-setters, balancing competing interests. Degrowth and adaptation advocates are largely excluded or bear indirect costs, pushing their directionality towards the target end.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    technological_decoupling_feasibility,
    'Is technological innovation alone sufficient to achieve deep decarbonization targets while maintaining economic growth, or does it require fundamental shifts in consumption and production patterns?',
    'Empirical observation of global emissions trajectories relative to GDP growth over the next decade, and the rate of deployment and effectiveness of carbon capture and removal technologies.',
    'If decoupling proves insufficient, the ''mitigation_priority'' reading''s core premise is undermined, potentially shifting its classification towards a ''snare'' (if it extracts without solving the problem) or increasing pressure for ''degrowth_reading'' alternatives.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(technological_decoupling_feasibility, empirical, 'Uncertainty regarding the empirical feasibility of growth-compatible decarbonization.').

omega_variable(
    suppression_of_alternatives_legitimacy,
    'Is the suppression of ''degrowth'' and radical ''adaptation'' narratives a legitimate coordination function (focusing resources on a viable path) or an extractive mechanism (protecting incumbent economic interests)?',
    'Analysis of policy discourse and media framing: if suppression relies on misrepresentation or active silencing rather than reasoned debate, it indicates an extractive function. Also, the observed efficacy of the ''mitigation_priority'' approach: if it consistently fails to meet targets, the suppression of alternatives becomes less legitimate.',
    'If suppression is primarily extractive, the ''mitigation_priority'' constraint''s overall extractiveness and suppression scores would be re-evaluated upwards, potentially pushing it closer to a ''snare'' classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_of_alternatives_legitimacy, conceptual, 'Ambiguity in whether suppressing alternative climate responses is legitimate coordination or extraction.').

omega_variable(
    intergenerational_equity_burden_distribution,
    'Does the current distribution of mitigation burdens truly align with intergenerational equity, or does it disproportionately burden vulnerable populations in the present for the benefit of future, potentially wealthier, generations?',
    'Detailed socio-economic analysis of the distributional impacts of carbon pricing, green investments, and technological transitions across different income groups and regions, both within and between generations.',
    'If the burden is found to be inequitably distributed in the present, the ''intergenerational_equity_primary'' axiom''s practical application is challenged, potentially leading to increased resistance and calls for policy reform, or reclassifying the constraint as more extractive for certain present-day groups.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(intergenerational_equity_burden_distribution, empirical, 'Uncertainty about the equitable distribution of mitigation costs across present and future generations.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_harm_prevention__mitigation_priority, 1990, 2050).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clim_tr_t1990, climate_harm_prevention__mitigation_priority, theater_ratio, 1990, 0.2).
narrative_ontology:measurement(clim_tr_t2000, climate_harm_prevention__mitigation_priority, theater_ratio, 2000, 0.28).
narrative_ontology:measurement(clim_tr_t2010, climate_harm_prevention__mitigation_priority, theater_ratio, 2010, 0.35).
narrative_ontology:measurement(clim_tr_t2020, climate_harm_prevention__mitigation_priority, theater_ratio, 2020, 0.4).
narrative_ontology:measurement(clim_tr_t2030, climate_harm_prevention__mitigation_priority, theater_ratio, 2030, 0.45).
narrative_ontology:measurement(clim_tr_t2040, climate_harm_prevention__mitigation_priority, theater_ratio, 2040, 0.48).
narrative_ontology:measurement(clim_tr_t2050, climate_harm_prevention__mitigation_priority, theater_ratio, 2050, 0.5).

% Extraction over time
narrative_ontology:measurement(clim_be_t1990, climate_harm_prevention__mitigation_priority, base_extractiveness, 1990, 0.45).
narrative_ontology:measurement(clim_be_t2000, climate_harm_prevention__mitigation_priority, base_extractiveness, 2000, 0.52).
narrative_ontology:measurement(clim_be_t2010, climate_harm_prevention__mitigation_priority, base_extractiveness, 2010, 0.58).
narrative_ontology:measurement(clim_be_t2020, climate_harm_prevention__mitigation_priority, base_extractiveness, 2020, 0.63).
narrative_ontology:measurement(clim_be_t2030, climate_harm_prevention__mitigation_priority, base_extractiveness, 2030, 0.68).
narrative_ontology:measurement(clim_be_t2040, climate_harm_prevention__mitigation_priority, base_extractiveness, 2040, 0.7).
narrative_ontology:measurement(clim_be_t2050, climate_harm_prevention__mitigation_priority, base_extractiveness, 2050, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(clim_su_t1990, climate_harm_prevention__mitigation_priority, suppression_requirement, 1990, 0.5).
narrative_ontology:measurement(clim_su_t2000, climate_harm_prevention__mitigation_priority, suppression_requirement, 2000, 0.58).
narrative_ontology:measurement(clim_su_t2010, climate_harm_prevention__mitigation_priority, suppression_requirement, 2010, 0.65).
narrative_ontology:measurement(clim_su_t2020, climate_harm_prevention__mitigation_priority, suppression_requirement, 2020, 0.7).
narrative_ontology:measurement(clim_su_t2030, climate_harm_prevention__mitigation_priority, suppression_requirement, 2030, 0.75).
narrative_ontology:measurement(clim_su_t2040, climate_harm_prevention__mitigation_priority, suppression_requirement, 2040, 0.78).
narrative_ontology:measurement(clim_su_t2050, climate_harm_prevention__mitigation_priority, suppression_requirement, 2050, 0.8).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_harm_prevention__mitigation_priority, global_infrastructure).
narrative_ontology:affects_constraint(climate_harm_prevention__mitigation_priority, climate_harm_prevention__adaptation_priority).
narrative_ontology:affects_constraint(climate_harm_prevention__mitigation_priority, climate_harm_prevention__degrowth_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'climate_harm_prevention' kernel. It focuses on emissions reduction within a growth framework, influencing and being influenced by alternative readings that prioritize adaptation or advocate for degrowth.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
