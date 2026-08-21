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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: climate_harm_prevention__mitigation_priority
 *   human_readable: Climate Harm Prevention: Mitigation Priority Reading
 *   domain: climate_policy/political_economy/intergenerational_ethics
 *
 * SUMMARY:
 *   This constraint represents the 'mitigation priority' reading of climate
 *   harm prevention, which emphasizes reducing emissions through
 *   technological transition within an economic growth framework. It posits
 *   that future generations are the primary beneficiaries, while present-day
 *   carbon-intensive sectors and, to some extent, low-income households bear
 *   the transition costs. The constraint is classified as a Tangled Rope due
 *   to its genuine coordination function (preventing climate catastrophe)
 *   coupled with significant, actively enforced extraction from specific
 *   groups. The metrics reflect the increasing costs and suppression required
 *   to maintain this transition.
 *
 * KEY AGENTS:
 *   - future_generations: Primary beneficiaries (powerless/trapped)
 *   - carbon_intensive_industries: Primary payers (powerful/constrained)
 *   - fossil_fuel_dependent_economies: Payer (institutional/constrained)
 *   - renewable_energy_sector: Beneficiary (organized/mobile)
 *   - environmental_advocacy_groups: Agenda-setter (organized/mobile)
 *   - low_income_households: Payer (powerless/trapped)
 *   - international_climate_negotiators: Agenda-setter (institutional/constrained)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_harm_prevention__mitigation_priority, 0.65).
domain_priors:suppression_score(climate_harm_prevention__mitigation_priority, 0.7).
domain_priors:theater_ratio(climate_harm_prevention__mitigation_priority, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_harm_prevention__mitigation_priority, extractiveness, 0.65).
narrative_ontology:constraint_metric(climate_harm_prevention__mitigation_priority, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(climate_harm_prevention__mitigation_priority, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(climate_harm_prevention__mitigation_priority, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(climate_harm_prevention__mitigation_priority, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_harm_prevention__mitigation_priority, tangled_rope).
narrative_ontology:human_readable(climate_harm_prevention__mitigation_priority, "Climate Harm Prevention: Mitigation Priority Reading").
narrative_ontology:topic_domain(climate_harm_prevention__mitigation_priority, "climate_policy/political_economy/intergenerational_ethics").

domain_priors:requires_active_enforcement(climate_harm_prevention__mitigation_priority).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_harm_prevention__mitigation_priority, '65e58ae0-7732-480b-a331-bbedc13623f8').
narrative_ontology:cs_kernel_codification('65e58ae0-7732-480b-a331-bbedc13623f8', formalized).
narrative_ontology:cs_authority_grounding('65e58ae0-7732-480b-a331-bbedc13623f8', expertise).
narrative_ontology:cs_interpretation_layer_present('65e58ae0-7732-480b-a331-bbedc13623f8').
narrative_ontology:cs_reading_relation('65e58ae0-7732-480b-a331-bbedc13623f8', climate_harm_prevention__adaptation_priority, coexists_with).
narrative_ontology:cs_reading_relation('65e58ae0-7732-480b-a331-bbedc13623f8', climate_harm_prevention__degrowth_reading, coexists_with).
narrative_ontology:cs_axiom('65e58ae0-7732-480b-a331-bbedc13623f8', foundational, emissions_reduction_is_primary_lever).
narrative_ontology:cs_axiom_status(emissions_reduction_is_primary_lever, holdable).
narrative_ontology:cs_axiom_grounding('65e58ae0-7732-480b-a331-bbedc13623f8', emissions_reduction_is_primary_lever, empirically_contingent).
narrative_ontology:cs_axiom('65e58ae0-7732-480b-a331-bbedc13623f8', foundational, decarbonization_compatible_with_growth).
narrative_ontology:cs_axiom_status(decarbonization_compatible_with_growth, holdable).
narrative_ontology:cs_axiom_grounding('65e58ae0-7732-480b-a331-bbedc13623f8', decarbonization_compatible_with_growth, empirically_contingent).
narrative_ontology:cs_reference_frame('65e58ae0-7732-480b-a331-bbedc13623f8', scientific_consensus_on_mitigation).
narrative_ontology:cs_drift_state('65e58ae0-7732-480b-a331-bbedc13623f8', contemporary_political_economy, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('65e58ae0-7732-480b-a331-bbedc13623f8', '').
narrative_ontology:cs_kernel_id(climate_harm_prevention__mitigation_priority, climate_harm_prevention).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_harm_prevention__mitigation_priority, future_generations).
narrative_ontology:constraint_beneficiary(climate_harm_prevention__mitigation_priority, renewable_energy_sector).
narrative_ontology:constraint_victim(climate_harm_prevention__mitigation_priority, carbon_intensive_industries).
narrative_ontology:constraint_victim(climate_harm_prevention__mitigation_priority, fossil_fuel_dependent_economies).
narrative_ontology:constraint_victim(climate_harm_prevention__mitigation_priority, low_income_households).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Primary beneficiaries of successful mitigation efforts, as they avoid the most severe impacts of climate change. They have no direct voice in current policy decisions.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__mitigation_priority, future_generations, beneficiary,
    powerless, generational, trapped, universal).

% Bear significant costs from emissions regulations, carbon pricing, and mandates for technological transition. They face stranded assets and competitive disadvantages if unable to adapt or externalize costs.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__mitigation_priority, carbon_intensive_industries, payer,
    powerful, immediate, constrained, global).

% National economies heavily reliant on fossil fuel production or consumption face economic disruption, job losses, and fiscal challenges during a rapid transition away from carbon. Their political systems are often resistant to change.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__mitigation_priority, fossil_fuel_dependent_economies, payer,
    institutional, biographical, constrained, national).

% Benefits from policies that incentivize emissions reduction, such as subsidies, tax credits, and mandates for renewable energy deployment. They see increased investment and market growth.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__mitigation_priority, renewable_energy_sector, beneficiary,
    organized, biographical, mobile, global).

% Actively lobby for stronger emissions reduction targets and policies, shaping the public discourse and political agenda around mitigation. They represent the interests of future generations and ecosystems.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__mitigation_priority, environmental_advocacy_groups, agenda_setter,
    organized, generational, mobile, global).

% May bear disproportionate costs of energy transition through higher energy prices, regressive carbon taxes, or job displacement in transitioning industries, without adequate compensatory mechanisms.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__mitigation_priority, low_income_households, payer,
    powerless, immediate, trapped, local).

% Work to establish global emissions reduction targets and frameworks, balancing national interests with the collective goal of climate stability. Their agreements set the broad parameters for national policies.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__mitigation_priority, international_climate_negotiators, agenda_setter,
    institutional, generational, constrained, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates global and national efforts to reduce greenhouse gas emissions, aligning diverse actors towards a common goal of preventing catastrophic future climate change through technological and economic transition.
% TRANSFER_FUNCTION: Transfers economic costs and regulatory burdens from future generations (who would bear climate impacts) to present-day carbon-intensive sectors and, indirectly, to consumers and taxpayers, in exchange for a more stable climate future.
% ABSENT_VOICES: The voices of future generations are structurally absent, represented by proxies (environmental groups, scientists). Indigenous communities, often disproportionately affected by both climate change and some mitigation projects, are frequently marginalized in policy formulation.
% DISAPPEARANCE_RATIONALE: If the priority on mitigation vanished, global emissions would likely accelerate, leading to more severe and rapid climate impacts. Investment would shift away from renewables, and carbon-intensive industries would face fewer constraints, fundamentally altering the global economy and environment.
% FOUNDING_PROBLEM: The scientific consensus on anthropogenic climate change and its potential for severe, irreversible harm to future generations and ecosystems.
% FOUNDING_PROBLEM_CORROBORATION: The Intergovernmental Panel on Climate Change (IPCC) reports, national science academies, and a vast body of peer-reviewed scientific literature from outside the directly benefiting renewable energy sector or environmental advocacy groups consistently corroborate the live status of the climate change problem and the need for mitigation.
narrative_ontology:disappearance_verdict(climate_harm_prevention__mitigation_priority, world_rearranges).
narrative_ontology:founding_problem_status(climate_harm_prevention__mitigation_priority, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(climate_harm_prevention__mitigation_priority, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(climate_harm_prevention__mitigation_priority, 'none', 1).
narrative_ontology:epsilon_provenance(climate_harm_prevention__mitigation_priority, 0.65, 'gemini-2.5-flash', 'none', direct).

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
 *   Extractiveness is high (0.65) because the transition demands significant re-allocation of capital and labor, imposing substantial costs on established industries and potentially vulnerable populations. Suppression (0.70) is also high, reflecting the active political and regulatory enforcement needed to overcome resistance from powerful incumbent interests and to drive rapid technological change. Theater ratio (0.20) is moderate, as there are genuine efforts towards mitigation, but also performative elements like 'greenwashing' or insufficient policy implementation. The projected decrease in theater ratio reflects an anticipated hardening of policy and a shift from rhetoric to action as climate impacts intensify. Accessibility collapse (0.40) is moderate, as alternatives to carbon-intensive development are increasingly available but still require significant investment and political will. Resistance (0.75) is high, driven by the substantial economic and social disruption inherent in the transition.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of future generations and environmental advocates, this constraint is a necessary, albeit challenging, coordination mechanism. From the perspective of carbon-intensive industries and fossil-fuel-dependent economies, it is a highly extractive and suppressive force threatening their existence. Low-income households experience it as a burden without clear immediate benefits. The engine's per-seat classification will reflect these divergent experiences.
 *
 * DIRECTIONALITY LOGIC:
 *   Future generations are full beneficiaries (d=0.0) as they avoid harm. The renewable energy sector is a beneficiary (d low) due to market growth. Carbon-intensive industries and fossil-fuel-dependent economies are targets (d high) due to imposed costs and regulations. Low-income households are also targets (d high) due to potential regressive impacts. Environmental advocacy groups and international climate negotiators act as agenda-setters, pushing for the constraint's enforcement, aligning with the beneficiary side.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is a Tangled Rope, not a Snare, because it addresses a genuine, universally acknowledged coordination problem (climate change) and offers a pathway to a collective good (a stable climate). However, it is 'tangled' because the chosen solution (growth-compatible technological transition) involves significant, asymmetric extraction from specific present-day actors. It avoids being mislabeled as a pure Snare because the coordination function is real and widely accepted by the scientific community and a broad coalition of stakeholders, even if the distribution of costs is contested. Mandatrophy is not yet resolved, as the transition is ongoing and the founding problem remains live.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    growth_decoupling_feasibility,
    'Is it empirically feasible to achieve the necessary emissions reductions within a global economic growth framework, or does it require a degrowth paradigm?',
    'Empirical observation of global emissions trajectories relative to GDP growth over the next 10-20 years, and the efficacy of green technologies in achieving absolute decoupling.',
    'If decoupling proves infeasible, this reading''s foundational premise (growth-compatible decarbonization) would be challenged, potentially shifting the classification towards a Snare (if the growth imperative is seen as a cover for extraction) or strengthening the degrowth_reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(growth_decoupling_feasibility, empirical, 'Uncertainty about whether economic growth and emissions reduction can genuinely be decoupled.').

omega_variable(
    intergenerational_equity_burden,
    'Is the distribution of transition costs between present and future generations, and within the present generation, equitable, or does it disproportionately burden vulnerable groups?',
    'Socio-economic impact assessments of climate policies, disaggregated by income, geography, and sector, coupled with ethical analysis of intergenerational justice frameworks.',
    'If the burden is found to be highly inequitable, the ''coordination'' aspect of this Tangled Rope could be undermined, pushing it closer to a Snare, as the benefits accrue to a diffuse future while costs are concentrated in a vulnerable present.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(intergenerational_equity_burden, preference, 'Ethical ambiguity regarding the fairness of cost distribution in climate mitigation.').

omega_variable(
    reading_framing_ambiguity,
    'Is this constraint a genuine ''mitigation priority'' reading, or is it a ''delayed action'' reading that prioritizes maintaining the status quo under the guise of future mitigation?',
    'Analysis of policy implementation vs. rhetoric: if actual emissions reductions consistently fall short of stated targets, and investments in fossil fuels continue, it suggests a ''delayed action'' framing.',
    'If reclassified as ''delayed action'', the extractiveness and theater_ratio would likely be higher, and the claimed_type might shift towards a Snare or Piton, as the coordination function becomes a cover for inaction.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_framing_ambiguity, conceptual, 'Ambiguity between genuine mitigation priority and rhetorical delay.').


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
narrative_ontology:measurement(clim_tr_t2030, climate_harm_prevention__mitigation_priority, theater_ratio, 2030, 0.2).
narrative_ontology:measurement_basis(clim_tr_t2030, projected).
narrative_ontology:measurement(clim_tr_t2040, climate_harm_prevention__mitigation_priority, theater_ratio, 2040, 0.15).
narrative_ontology:measurement_basis(clim_tr_t2040, projected).
narrative_ontology:measurement(clim_tr_t2050, climate_harm_prevention__mitigation_priority, theater_ratio, 2050, 0.1).
narrative_ontology:measurement_basis(clim_tr_t2050, projected).

% Extraction over time
narrative_ontology:measurement(clim_be_t1990, climate_harm_prevention__mitigation_priority, base_extractiveness, 1990, 0.4).
narrative_ontology:measurement(clim_be_t2000, climate_harm_prevention__mitigation_priority, base_extractiveness, 2000, 0.48).
narrative_ontology:measurement(clim_be_t2010, climate_harm_prevention__mitigation_priority, base_extractiveness, 2010, 0.55).
narrative_ontology:measurement(clim_be_t2020, climate_harm_prevention__mitigation_priority, base_extractiveness, 2020, 0.62).
narrative_ontology:measurement(clim_be_t2030, climate_harm_prevention__mitigation_priority, base_extractiveness, 2030, 0.65).
narrative_ontology:measurement_basis(clim_be_t2030, projected).
narrative_ontology:measurement(clim_be_t2040, climate_harm_prevention__mitigation_priority, base_extractiveness, 2040, 0.68).
narrative_ontology:measurement_basis(clim_be_t2040, projected).
narrative_ontology:measurement(clim_be_t2050, climate_harm_prevention__mitigation_priority, base_extractiveness, 2050, 0.7).
narrative_ontology:measurement_basis(clim_be_t2050, projected).

% Suppression requirement over time
narrative_ontology:measurement(clim_su_t1990, climate_harm_prevention__mitigation_priority, suppression_requirement, 1990, 0.3).
narrative_ontology:measurement(clim_su_t2000, climate_harm_prevention__mitigation_priority, suppression_requirement, 2000, 0.45).
narrative_ontology:measurement(clim_su_t2010, climate_harm_prevention__mitigation_priority, suppression_requirement, 2010, 0.55).
narrative_ontology:measurement(clim_su_t2020, climate_harm_prevention__mitigation_priority, suppression_requirement, 2020, 0.65).
narrative_ontology:measurement(clim_su_t2030, climate_harm_prevention__mitigation_priority, suppression_requirement, 2030, 0.7).
narrative_ontology:measurement_basis(clim_su_t2030, projected).
narrative_ontology:measurement(clim_su_t2040, climate_harm_prevention__mitigation_priority, suppression_requirement, 2040, 0.72).
narrative_ontology:measurement_basis(clim_su_t2040, projected).
narrative_ontology:measurement(clim_su_t2050, climate_harm_prevention__mitigation_priority, suppression_requirement, 2050, 0.75).
narrative_ontology:measurement_basis(clim_su_t2050, projected).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_harm_prevention__mitigation_priority, global_infrastructure).
narrative_ontology:affects_constraint(climate_harm_prevention__mitigation_priority, climate_harm_prevention__adaptation_priority).
narrative_ontology:affects_constraint(climate_harm_prevention__mitigation_priority, climate_harm_prevention__degrowth_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading ('mitigation_priority') of the 'climate_harm_prevention' kernel. It is linked to sibling readings 'adaptation_priority' and 'degrowth_reading', which offer alternative approaches to climate response.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
