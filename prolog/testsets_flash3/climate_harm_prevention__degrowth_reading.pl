% ============================================================================
% CONSTRAINT STORY: climate_harm_prevention__degrowth_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_climate_harm_prevention__degrowth_reading, []).

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
 *   constraint_id: climate_harm_prevention__degrowth_reading
 *   human_readable: Degrowth Imperative for Climate Harm Prevention
 *   domain: climate_policy/political_economy/intergenerational_ethics
 *
 * SUMMARY:
 *   This constraint represents the 'degrowth' reading of legitimate climate
 *   response, asserting that planned economic contraction in the Global North
 *   is a necessary condition for preventing climate harm, as mitigation
 *   within a growth framework is deemed physically and politically
 *   impossible. It identifies Global South populations and future generations
 *   as primary beneficiaries, while Global North consumers and extractive
 *   industries bear the costs of reduced consumption and economic activity.
 *   The constraint is framed as a Snare from the perspective of those whose
 *   consumption and economic models are targeted, due to its high
 *   extractiveness and the suppression required to implement such a radical
 *   shift.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_harm_prevention__degrowth_reading, 0.9).
domain_priors:suppression_score(climate_harm_prevention__degrowth_reading, 0.7).
domain_priors:theater_ratio(climate_harm_prevention__degrowth_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_harm_prevention__degrowth_reading, extractiveness, 0.9).
narrative_ontology:constraint_metric(climate_harm_prevention__degrowth_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(climate_harm_prevention__degrowth_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(climate_harm_prevention__degrowth_reading, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(climate_harm_prevention__degrowth_reading, resistance, 0.95).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_harm_prevention__degrowth_reading, snare).
narrative_ontology:human_readable(climate_harm_prevention__degrowth_reading, "Degrowth Imperative for Climate Harm Prevention").
narrative_ontology:topic_domain(climate_harm_prevention__degrowth_reading, "climate_policy/political_economy/intergenerational_ethics").

domain_priors:requires_active_enforcement(climate_harm_prevention__degrowth_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_harm_prevention__degrowth_reading, '852cb026-0e8e-44d7-b4b8-7292282fe356').
narrative_ontology:cs_kernel_codification('852cb026-0e8e-44d7-b4b8-7292282fe356', distributed).
narrative_ontology:cs_authority_grounding('852cb026-0e8e-44d7-b4b8-7292282fe356', diffuse_epistemic).
narrative_ontology:cs_reading_relation('852cb026-0e8e-44d7-b4b8-7292282fe356', climate_harm_prevention__mitigation_priority, forecloses).
narrative_ontology:cs_reading_relation('852cb026-0e8e-44d7-b4b8-7292282fe356', climate_harm_prevention__adaptation_priority, coexists_with).
narrative_ontology:cs_axiom('852cb026-0e8e-44d7-b4b8-7292282fe356', foundational, infinite_growth_on_finite_planet_impossible).
narrative_ontology:cs_axiom_status(infinite_growth_on_finite_planet_impossible, holdable).
narrative_ontology:cs_axiom_grounding('852cb026-0e8e-44d7-b4b8-7292282fe356', infinite_growth_on_finite_planet_impossible, empirically_contingent).
narrative_ontology:cs_axiom('852cb026-0e8e-44d7-b4b8-7292282fe356', foundational, ecological_justice_requires_global_north_contraction).
narrative_ontology:cs_axiom_status(ecological_justice_requires_global_north_contraction, holdable).
narrative_ontology:cs_axiom_grounding('852cb026-0e8e-44d7-b4b8-7292282fe356', ecological_justice_requires_global_north_contraction, deontological).
narrative_ontology:cs_reference_frame('852cb026-0e8e-44d7-b4b8-7292282fe356', planetary_boundaries_framework).
narrative_ontology:cs_drift_state('852cb026-0e8e-44d7-b4b8-7292282fe356', contemporary_policy_discourse, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('852cb026-0e8e-44d7-b4b8-7292282fe356', '').
narrative_ontology:cs_kernel_id(climate_harm_prevention__degrowth_reading, climate_harm_prevention).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_harm_prevention__degrowth_reading, global_south_populations).
narrative_ontology:constraint_beneficiary(climate_harm_prevention__degrowth_reading, future_generations).
narrative_ontology:constraint_victim(climate_harm_prevention__degrowth_reading, global_north_consumers).
narrative_ontology:constraint_victim(climate_harm_prevention__degrowth_reading, extractive_industries).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Would bear the direct costs of planned economic contraction, including reduced consumption, altered lifestyles, and potential job displacement in carbon-intensive sectors. Their current consumption patterns are seen as a primary driver of the problem.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__degrowth_reading, global_north_consumers, payer,
    powerful, immediate, constrained, global).

% Would benefit from reduced climate impacts, improved ecological stability, and a more equitable distribution of global resources. They currently bear a disproportionate share of climate harms despite low historical emissions.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__degrowth_reading, global_south_populations, beneficiary,
    powerless, generational, trapped, global).

% Are the ultimate beneficiaries of a stable climate system and a sustainable planetary boundary, avoiding catastrophic warming scenarios that current policies risk.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__degrowth_reading, future_generations, beneficiary,
    powerless, civilizational, trapped, universal).

% Would face severe contraction or obsolescence under a degrowth framework, as their business models are predicated on continuous resource extraction and economic expansion. They actively resist policies that threaten growth.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__degrowth_reading, extractive_industries, payer,
    institutional, biographical, constrained, global).

% Propose and articulate the necessity of planned economic contraction, advocating for policies that prioritize ecological sustainability and social equity over GDP growth. They seek to reframe the climate debate.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__degrowth_reading, degrowth_advocates, agenda_setter,
    organized, generational, mobile, global).

% Currently operate within a growth-oriented paradigm, finding degrowth politically unfeasible due to perceived economic and social costs. They are excluded from the degrowth framing of legitimate climate response.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__degrowth_reading, mainstream_policy_makers, excluded,
    institutional, immediate, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates global economic activity and resource use to remain within planetary boundaries, ensuring long-term ecological stability and intergenerational equity by reducing consumption in high-income nations.
% TRANSFER_FUNCTION: Transfers ecological space, resource availability, and climate stability from current Global North consumption to Global South populations and future generations, by requiring a reduction in material throughput and economic activity in the Global North.
% ABSENT_VOICES: The voices of future generations are structurally absent, as are the full ecological systems that bear the brunt of current growth. Their interests are represented by advocates but cannot speak for themselves.
% DISAPPEARANCE_RATIONALE: If the degrowth imperative vanished, the default trajectory of continuous economic growth and associated climate harms would persist, leading to escalating ecological crises and exacerbating existing inequalities. The global economic and ecological system would continue on its current path, which this reading deems unsustainable.
% FOUNDING_PROBLEM: The foundational problem is the inherent conflict between infinite economic growth on a finite planet, leading to ecological overshoot, climate breakdown, and severe intergenerational and global inequity.
% FOUNDING_PROBLEM_CORROBORATION: Ecological economists, climate scientists, and indigenous communities corroborate the live status of the founding problem, citing planetary boundary transgressions, accelerating climate impacts, and the historical injustice of colonial resource extraction. This corroboration comes from outside the immediate beneficiaries of degrowth.
narrative_ontology:disappearance_verdict(climate_harm_prevention__degrowth_reading, world_rearranges).
narrative_ontology:founding_problem_status(climate_harm_prevention__degrowth_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(climate_harm_prevention__degrowth_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(climate_harm_prevention__degrowth_reading, 'none', 1).
narrative_ontology:epsilon_provenance(climate_harm_prevention__degrowth_reading, 0.9, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(climate_harm_prevention__degrowth_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(climate_harm_prevention__degrowth_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(climate_harm_prevention__degrowth_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness is very high (0.9) because it demands a fundamental restructuring of economic systems and a significant reduction in material consumption in the Global North. Suppression is high (0.7) due to the immense political and social resistance such a policy would face, requiring active enforcement to overcome entrenched interests and consumer habits. Resistance is also very high (0.95) reflecting the current political infeasibility and strong opposition to degrowth proposals. Theater ratio is low (0.1) as this reading is a direct, unvarnished call for systemic change, with little room for performative gestures masking other functions.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of Global North consumers and extractive industries, this constraint would be perceived as a highly extractive Snare, demanding significant sacrifices and suppressing their current way of life. For Global South populations and future generations, it would be seen as a necessary Rope or even a Mountain, ensuring their survival and well-being by aligning human activity with ecological limits.
 *
 * DIRECTIONALITY LOGIC:
 *   Global South populations and future generations are full beneficiaries (d=0.0) as the constraint directly addresses their existential threats and historical injustices. Global North consumers and extractive industries are full targets (d=1.0) as they are required to contract their economic activity and consumption. Degrowth advocates act as agenda-setters, pushing for the implementation of this constraint.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is not subject to mandatrophy in the traditional sense, as it addresses a 'live' and escalating problem (climate breakdown). The challenge is not that its mandate has atrophied, but that its proposed solution is highly contested and resisted. The classification as a Snare from the perspective of those who bear the costs highlights the extractive nature of the proposed solution for certain groups, preventing it from being mislabeled as a purely coordinative Rope or a natural Mountain, which would obscure the significant transfers and suppressions involved.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    political_feasibility_of_degrowth,
    'Is planned economic contraction in the Global North politically feasible within existing democratic structures, or would it require authoritarian enforcement?',
    'Empirical observation of policy implementation attempts and public response in democratic nations. Analysis of historical precedents for large-scale, planned economic restructuring.',
    'If politically infeasible without authoritarianism, the ''suppression'' metric might be understated, and the constraint''s ''claimed_type'' as a Snare would be further reinforced by the necessity of coercive state power. If feasible through democratic means, the ''suppression'' might be lower, suggesting a more consensual (though still costly) transition.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(political_feasibility_of_degrowth, empirical, 'Uncertainty regarding the political viability and enforcement mechanisms of a degrowth agenda.').

omega_variable(
    economic_contraction_impact_on_global_south,
    'Would planned economic contraction in the Global North inadvertently harm Global South economies that are currently dependent on trade and investment from the North?',
    'Detailed economic modeling of global supply chains and trade relationships under various degrowth scenarios. Analysis of historical instances of Northern economic downturns and their impact on the South.',
    'If degrowth in the North significantly harms the South, the ''beneficiary'' status of Global South populations would be complicated, potentially shifting their ''directionality'' towards a more neutral or even payer position, altering the overall classification of the constraint''s impact.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(economic_contraction_impact_on_global_south, empirical, 'Potential unintended negative economic consequences of Northern degrowth for the Global South.').

omega_variable(
    growth_decoupling_potential,
    'Is it physically and politically impossible to achieve sufficient climate mitigation within a growth framework, or could technological and policy innovations enable ''green growth'' to decouple emissions from economic activity?',
    'Long-term empirical data on the absolute decoupling of resource use and emissions from GDP in various economic sectors and nations. Assessment of the political will and capacity to implement necessary technological transitions at scale.',
    'If significant decoupling is proven possible, the foundational premise of the degrowth reading would be challenged, potentially shifting its ''claimed_type'' towards a more ''contested'' status or even ''overridden'' if green growth proves viable. This would also influence the ''reading_relations'' with ''mitigation_priority''.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(growth_decoupling_potential, empirical, 'The core empirical claim that growth and climate mitigation are fundamentally incompatible.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_harm_prevention__degrowth_reading, 2020, 2050).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clim_tr_t2020, climate_harm_prevention__degrowth_reading, theater_ratio, 2020, 0.1).
narrative_ontology:measurement(clim_tr_t2025, climate_harm_prevention__degrowth_reading, theater_ratio, 2025, 0.1).
narrative_ontology:measurement(clim_tr_t2030, climate_harm_prevention__degrowth_reading, theater_ratio, 2030, 0.1).
narrative_ontology:measurement(clim_tr_t2035, climate_harm_prevention__degrowth_reading, theater_ratio, 2035, 0.1).
narrative_ontology:measurement(clim_tr_t2040, climate_harm_prevention__degrowth_reading, theater_ratio, 2040, 0.1).
narrative_ontology:measurement(clim_tr_t2045, climate_harm_prevention__degrowth_reading, theater_ratio, 2045, 0.1).
narrative_ontology:measurement(clim_tr_t2050, climate_harm_prevention__degrowth_reading, theater_ratio, 2050, 0.1).

% Extraction over time
narrative_ontology:measurement(clim_be_t2020, climate_harm_prevention__degrowth_reading, base_extractiveness, 2020, 0.85).
narrative_ontology:measurement(clim_be_t2025, climate_harm_prevention__degrowth_reading, base_extractiveness, 2025, 0.87).
narrative_ontology:measurement(clim_be_t2030, climate_harm_prevention__degrowth_reading, base_extractiveness, 2030, 0.88).
narrative_ontology:measurement(clim_be_t2035, climate_harm_prevention__degrowth_reading, base_extractiveness, 2035, 0.89).
narrative_ontology:measurement(clim_be_t2040, climate_harm_prevention__degrowth_reading, base_extractiveness, 2040, 0.9).
narrative_ontology:measurement(clim_be_t2045, climate_harm_prevention__degrowth_reading, base_extractiveness, 2045, 0.9).
narrative_ontology:measurement(clim_be_t2050, climate_harm_prevention__degrowth_reading, base_extractiveness, 2050, 0.9).

% Suppression requirement over time
narrative_ontology:measurement(clim_su_t2020, climate_harm_prevention__degrowth_reading, suppression_requirement, 2020, 0.6).
narrative_ontology:measurement(clim_su_t2025, climate_harm_prevention__degrowth_reading, suppression_requirement, 2025, 0.63).
narrative_ontology:measurement(clim_su_t2030, climate_harm_prevention__degrowth_reading, suppression_requirement, 2030, 0.66).
narrative_ontology:measurement(clim_su_t2035, climate_harm_prevention__degrowth_reading, suppression_requirement, 2035, 0.68).
narrative_ontology:measurement(clim_su_t2040, climate_harm_prevention__degrowth_reading, suppression_requirement, 2040, 0.7).
narrative_ontology:measurement(clim_su_t2045, climate_harm_prevention__degrowth_reading, suppression_requirement, 2045, 0.7).
narrative_ontology:measurement(clim_su_t2050, climate_harm_prevention__degrowth_reading, suppression_requirement, 2050, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_harm_prevention__degrowth_reading, resource_allocation).
narrative_ontology:affects_constraint(climate_harm_prevention__degrowth_reading, climate_harm_prevention__mitigation_priority).
narrative_ontology:affects_constraint(climate_harm_prevention__degrowth_reading, climate_harm_prevention__adaptation_priority).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'climate_harm_prevention' kernel, focusing on degrowth. It is structurally distinct from the 'mitigation_priority' and 'adaptation_priority' readings, which offer alternative approaches to the same core problem.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
