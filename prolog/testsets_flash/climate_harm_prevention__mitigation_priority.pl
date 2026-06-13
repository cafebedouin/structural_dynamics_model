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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:measurement_basis/2,
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
 *   constraint_id: climate_harm_prevention__mitigation_priority
 *   human_readable: Mitigation Priority for Climate Harm Prevention
 *   domain: climate_policy/political_economy/intergenerational_ethics
 *
 * SUMMARY:
 *   This constraint represents the dominant policy paradigm for climate
 *   change: prioritizing emissions reduction (mitigation) through
 *   technological innovation within an economic growth framework. It frames
 *   climate action as a long-term investment for future generations, with
 *   present-day carbon-intensive sectors bearing the primary costs of
 *   transition. The constraint is a reading of the broader
 *   'climate_harm_prevention' kernel, distinguishing itself from readings
 *   that prioritize adaptation or degrowth.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_harm_prevention__mitigation_priority, 0.6).
domain_priors:suppression_score(climate_harm_prevention__mitigation_priority, 0.4).
domain_priors:theater_ratio(climate_harm_prevention__mitigation_priority, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_harm_prevention__mitigation_priority, extractiveness, 0.6).
narrative_ontology:constraint_metric(climate_harm_prevention__mitigation_priority, suppression_requirement, 0.4).
narrative_ontology:constraint_metric(climate_harm_prevention__mitigation_priority, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(climate_harm_prevention__mitigation_priority, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(climate_harm_prevention__mitigation_priority, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_harm_prevention__mitigation_priority, tangled_rope).
narrative_ontology:human_readable(climate_harm_prevention__mitigation_priority, "Mitigation Priority for Climate Harm Prevention").
narrative_ontology:topic_domain(climate_harm_prevention__mitigation_priority, "climate_policy/political_economy/intergenerational_ethics").

domain_priors:requires_active_enforcement(climate_harm_prevention__mitigation_priority).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_harm_prevention__mitigation_priority, 'aeb3d78d-aa1f-432f-a4e3-ec70fdfd59ff').
narrative_ontology:cs_kernel_codification('aeb3d78d-aa1f-432f-a4e3-ec70fdfd59ff', formalized).
narrative_ontology:cs_authority_grounding('aeb3d78d-aa1f-432f-a4e3-ec70fdfd59ff', expertise).
narrative_ontology:cs_interpretation_layer_present('aeb3d78d-aa1f-432f-a4e3-ec70fdfd59ff').
narrative_ontology:cs_reading_relation('aeb3d78d-aa1f-432f-a4e3-ec70fdfd59ff', climate_harm_prevention__adaptation_priority, coexists_with).
narrative_ontology:cs_reading_relation('aeb3d78d-aa1f-432f-a4e3-ec70fdfd59ff', climate_harm_prevention__degrowth_reading, coexists_with).
narrative_ontology:cs_axiom('aeb3d78d-aa1f-432f-a4e3-ec70fdfd59ff', foundational, technological_progress_solves_climate).
narrative_ontology:cs_axiom_status(technological_progress_solves_climate, holdable).
narrative_ontology:cs_axiom_grounding('aeb3d78d-aa1f-432f-a4e3-ec70fdfd59ff', technological_progress_solves_climate, empirically_contingent).
narrative_ontology:cs_axiom('aeb3d78d-aa1f-432f-a4e3-ec70fdfd59ff', foundational, intergenerational_equity_demands_mitigation).
narrative_ontology:cs_axiom_status(intergenerational_equity_demands_mitigation, holdable).
narrative_ontology:cs_axiom_grounding('aeb3d78d-aa1f-432f-a4e3-ec70fdfd59ff', intergenerational_equity_demands_mitigation, deontological).
narrative_ontology:cs_reference_frame('aeb3d78d-aa1f-432f-a4e3-ec70fdfd59ff', sustainable_development_paradigm).
narrative_ontology:cs_drift_state('aeb3d78d-aa1f-432f-a4e3-ec70fdfd59ff', contemporary, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('aeb3d78d-aa1f-432f-a4e3-ec70fdfd59ff', '').
narrative_ontology:cs_kernel_id(climate_harm_prevention__mitigation_priority, climate_harm_prevention).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_harm_prevention__mitigation_priority, future_generations).
narrative_ontology:constraint_beneficiary(climate_harm_prevention__mitigation_priority, renewable_energy_sector).
narrative_ontology:constraint_victim(climate_harm_prevention__mitigation_priority, fossil_fuel_industries).
narrative_ontology:constraint_victim(climate_harm_prevention__mitigation_priority, carbon_intensive_sectors).
narrative_ontology:constraint_victim(climate_harm_prevention__mitigation_priority, present_consumers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The primary beneficiaries of successful emissions reduction, as they will experience less severe climate impacts. They have no direct voice in current policy decisions.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__mitigation_priority, future_generations, beneficiary,
    powerless, generational, trapped, universal).

% Bear significant costs from policies prioritizing emissions reduction, including stranded assets, carbon taxes, and regulatory burdens. They actively resist these policies.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__mitigation_priority, fossil_fuel_industries, payer,
    powerful, immediate, constrained, global).

% Industries like heavy manufacturing, agriculture, and transportation face mandates for decarbonization, requiring costly technological transitions and operational changes.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__mitigation_priority, carbon_intensive_sectors, payer,
    organized, biographical, constrained, national).

% Benefits from policies that incentivize technological transition away from fossil fuels, receiving subsidies, investment, and market expansion.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__mitigation_priority, renewable_energy_sector, beneficiary,
    moderate, biographical, arbitrage, global).

% Bear costs through higher energy prices, carbon taxes, and changes in consumption patterns, though they also benefit from improved air quality and long-term climate stability.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__mitigation_priority, present_consumers, payer,
    moderate, immediate, constrained, national).

% Provide the scientific basis for understanding climate change and the urgency of emissions reduction. Their role is to inform policy, not to set it.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__mitigation_priority, climate_scientists, observer,
    analytical, generational, analytical, global).

% Responsible for designing and implementing policies that prioritize emissions reduction, balancing economic impacts with long-term environmental goals. They navigate political resistance from affected industries.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__mitigation_priority, policy_makers, agenda_setter,
    institutional, generational, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates global and national efforts to reduce greenhouse gas emissions, aligning diverse actors (governments, industries, consumers) towards a common goal of preventing catastrophic climate change.
% TRANSFER_FUNCTION: Transfers economic costs (e.g., investment in new technologies, carbon pricing) from future generations (who would bear climate damages) to present generations and carbon-intensive industries, in exchange for a more stable climate future.
% ABSENT_VOICES: The most directly affected future generations are absent from current policy debates. Their interests are represented by advocates, but they have no direct political power. Indigenous communities, often disproportionately affected by climate change and resource extraction, are also frequently marginalized.
% DISAPPEARANCE_RATIONALE: If the priority on emissions reduction vanished, global climate policy would fragment, investments in fossil fuels would surge, and the planet would rapidly commit to much higher warming trajectories, fundamentally altering future ecosystems and human societies.
% FOUNDING_PROBLEM: The existential threat of anthropogenic climate change, driven by greenhouse gas emissions, leading to irreversible environmental degradation and societal disruption.
% FOUNDING_PROBLEM_CORROBORATION: The scientific consensus, as articulated by the IPCC and national academies of science, consistently corroborates the live status of the climate crisis. International agreements and national climate laws also reflect this consensus, providing corroboration from outside the immediate beneficiaries of mitigation policies.
narrative_ontology:disappearance_verdict(climate_harm_prevention__mitigation_priority, world_rearranges).
narrative_ontology:founding_problem_status(climate_harm_prevention__mitigation_priority, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(climate_harm_prevention__mitigation_priority, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(climate_harm_prevention__mitigation_priority, 'none', 1).

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
 *   The constraint is classified as a Tangled Rope because it genuinely coordinates global action towards a collective good (preventing climate harm) but also involves significant asymmetric extraction from specific industries and, to a lesser extent, present consumers. Extraction is moderate (0.6) due to the substantial costs imposed on fossil fuel and carbon-intensive sectors. Suppression (0.4) reflects the political and regulatory pressure required to overcome resistance from these sectors, but it's not absolute due to their lobbying power. Theater ratio (0.2) is relatively low, indicating that while there's some 'greenwashing,' the core efforts are directed towards actual emissions reduction. Resistance (0.7) is high, primarily from industries facing transition costs.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of future generations, this constraint is a necessary Rope, ensuring their survival. From the perspective of fossil fuel industries, it's a Snare, extracting their profits and threatening their existence. Policy makers experience it as a Tangled Rope, navigating the coordination challenge amidst significant political and economic resistance.
 *
 * DIRECTIONALITY LOGIC:
 *   Future generations and the renewable energy sector are clear beneficiaries (low d). Fossil fuel and carbon-intensive industries are primary targets (high d) due to the direct costs imposed. Present consumers are also targets, though with more constrained exit options and some indirect benefits. Policy makers act as agenda-setters, balancing these competing interests.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate (preventing climate harm) is very much live. The challenge is not mandatrophy but the political economy of transition. The classification as Tangled Rope prevents mislabeling it as a pure Snare (ignoring the coordination function) or a pure Rope (ignoring the extraction and suppression).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    growth_decoupling_feasibility,
    'Is economic growth truly compatible with the necessary pace and scale of decarbonization, or does the ''growth framework'' implicitly limit effective mitigation?',
    'Empirical observation of global emissions trajectories relative to GDP growth over the next 10-20 years, particularly in developed economies. Analysis of whether technological solutions alone can achieve targets without systemic economic shifts.',
    'If decoupling proves infeasible, the ''mitigation_priority'' reading might be reclassified towards a Snare for future generations (as it fails to deliver promised harm prevention) or shift towards the ''degrowth_reading'' as a more viable path.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(growth_decoupling_feasibility, empirical, 'The feasibility of achieving climate goals within a growth-oriented economic paradigm.').

omega_variable(
    intergenerational_equity_burden,
    'Is the distribution of transition costs between present and future generations, and across different present-day sectors, truly equitable, or does it disproportionately burden vulnerable groups?',
    'Socio-economic impact assessments of climate policies, disaggregated by income, geography, and social group. Analysis of ''just transition'' mechanisms and their effectiveness.',
    'If the burden is found to be highly inequitable, the ''extractiveness'' metric might be re-evaluated upwards for specific victim groups, potentially shifting their seat classification towards a Snare, even if the overall constraint remains a Tangled Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(intergenerational_equity_burden, preference, 'Equity of cost distribution in climate mitigation policies.').

omega_variable(
    mitigation_vs_adaptation_tradeoff,
    'At what point does the ''mitigation_priority'' reading become untenable, requiring a shift towards ''adaptation_priority'' due to insufficient progress or unavoidable warming?',
    'Monitoring of global temperature targets and climate tipping points. Assessment of the economic and social costs of further mitigation versus the costs of adaptation to unavoidable impacts.',
    'If mitigation targets are missed and warming accelerates, the ''mitigation_priority'' reading''s effectiveness diminishes, potentially leading to a re-evaluation of its claimed benefits and a shift in policy focus towards adaptation, aligning more with the ''adaptation_priority'' sibling reading.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(mitigation_vs_adaptation_tradeoff, empirical, 'The dynamic balance point between mitigation and adaptation strategies.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_harm_prevention__mitigation_priority, 1990, 2050).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clim_tr_t1990, climate_harm_prevention__mitigation_priority, theater_ratio, 1990, 0.1).
narrative_ontology:measurement(clim_tr_t2000, climate_harm_prevention__mitigation_priority, theater_ratio, 2000, 0.15).
narrative_ontology:measurement(clim_tr_t2010, climate_harm_prevention__mitigation_priority, theater_ratio, 2010, 0.2).
narrative_ontology:measurement(clim_tr_t2020, climate_harm_prevention__mitigation_priority, theater_ratio, 2020, 0.25).
narrative_ontology:measurement(clim_tr_t2030, climate_harm_prevention__mitigation_priority, theater_ratio, 2030, 0.3).
narrative_ontology:measurement_basis(clim_tr_t2030, projected).
narrative_ontology:measurement(clim_tr_t2040, climate_harm_prevention__mitigation_priority, theater_ratio, 2040, 0.35).
narrative_ontology:measurement_basis(clim_tr_t2040, projected).
narrative_ontology:measurement(clim_tr_t2050, climate_harm_prevention__mitigation_priority, theater_ratio, 2050, 0.4).
narrative_ontology:measurement_basis(clim_tr_t2050, projected).

% Extraction over time
narrative_ontology:measurement(clim_be_t1990, climate_harm_prevention__mitigation_priority, base_extractiveness, 1990, 0.4).
narrative_ontology:measurement(clim_be_t2000, climate_harm_prevention__mitigation_priority, base_extractiveness, 2000, 0.48).
narrative_ontology:measurement(clim_be_t2010, climate_harm_prevention__mitigation_priority, base_extractiveness, 2010, 0.55).
narrative_ontology:measurement(clim_be_t2020, climate_harm_prevention__mitigation_priority, base_extractiveness, 2020, 0.6).
narrative_ontology:measurement(clim_be_t2030, climate_harm_prevention__mitigation_priority, base_extractiveness, 2030, 0.65).
narrative_ontology:measurement_basis(clim_be_t2030, projected).
narrative_ontology:measurement(clim_be_t2040, climate_harm_prevention__mitigation_priority, base_extractiveness, 2040, 0.68).
narrative_ontology:measurement_basis(clim_be_t2040, projected).
narrative_ontology:measurement(clim_be_t2050, climate_harm_prevention__mitigation_priority, base_extractiveness, 2050, 0.7).
narrative_ontology:measurement_basis(clim_be_t2050, projected).

% Suppression requirement over time
narrative_ontology:measurement(clim_su_t1990, climate_harm_prevention__mitigation_priority, suppression_requirement, 1990, 0.2).
narrative_ontology:measurement(clim_su_t2000, climate_harm_prevention__mitigation_priority, suppression_requirement, 2000, 0.25).
narrative_ontology:measurement(clim_su_t2010, climate_harm_prevention__mitigation_priority, suppression_requirement, 2010, 0.3).
narrative_ontology:measurement(clim_su_t2020, climate_harm_prevention__mitigation_priority, suppression_requirement, 2020, 0.4).
narrative_ontology:measurement(clim_su_t2030, climate_harm_prevention__mitigation_priority, suppression_requirement, 2030, 0.45).
narrative_ontology:measurement_basis(clim_su_t2030, projected).
narrative_ontology:measurement(clim_su_t2040, climate_harm_prevention__mitigation_priority, suppression_requirement, 2040, 0.5).
narrative_ontology:measurement_basis(clim_su_t2040, projected).
narrative_ontology:measurement(clim_su_t2050, climate_harm_prevention__mitigation_priority, suppression_requirement, 2050, 0.55).
narrative_ontology:measurement_basis(clim_su_t2050, projected).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_harm_prevention__mitigation_priority, global_infrastructure).
narrative_ontology:affects_constraint(climate_harm_prevention__mitigation_priority, adaptation_priority).
narrative_ontology:affects_constraint(climate_harm_prevention__mitigation_priority, degrowth_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'climate_harm_prevention' kernel. Its focus on mitigation and growth-compatible decarbonization influences, and is influenced by, alternative readings that prioritize adaptation or degrowth.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
