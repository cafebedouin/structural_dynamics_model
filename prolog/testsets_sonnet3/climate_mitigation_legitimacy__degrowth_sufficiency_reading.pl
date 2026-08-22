% ============================================================================
% CONSTRAINT STORY: climate_mitigation_legitimacy__degrowth_sufficiency_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_climate_mitigation_legitimacy__degrowth_sufficiency_reading, []).

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
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
 *   constraint_id: climate_mitigation_legitimacy__degrowth_sufficiency_reading
 *   human_readable: Degrowth Sufficiency Reading of Climate Mitigation Legitimacy
 *   domain: energy_policy/climate_mitigation/technology_governance
 *
 * SUMMARY:
 *   This story instantiates one specific reading within the contested
 *   climate_mitigation_legitimacy kernel: the degrowth_sufficiency reading
 *   holds that decarbonization's legitimate pathway runs through demand
 *   reduction, and that large-scale generation expansion — whether nuclear or
 *   renewable — is unnecessary and potentially counterproductive if
 *   sufficiency measures succeed. Under this reading, both nuclear developers
 *   and utility-scale renewable developers fall into the victim set, because
 *   the reading's core premise treats new large-scale generation build-out
 *   itself as a growth-dependent artifact to be minimized, not merely a
 *   technology choice between clean options. This is structurally distinct
 *   from the sibling readings: baseload_necessity treats nuclear as the
 *   answer and renewables as insufficient; renewable_primacy treats
 *   renewables as the answer and nuclear as unnecessary; portfolio_pragmatism
 *   treats both as complementary. Only the sufficiency reading treats the
 *   entire generation-expansion project — regardless of technology — as the
 *   thing to be minimized. The ε values, beneficiary structure, and victim
 *   sets are authored for THIS reading alone and are not averaged against or
 *   hedged toward the sibling readings.
 *
 * KEY AGENTS:
 *   - sufficiency_policy_advocates: agenda_setter (organized/analytical) — set the intellectual legitimacy framework
 *   - existing_grid_asset_holders: beneficiary (institutional/arbitrage) — incumbent assets protected from new competition
 *   - nuclear_developers: payer (powerful/constrained) — growth-dependent capital projects delegitimized
 *   - utility_scale_renewable_developers: payer (powerful/constrained) — equally captured as growth-dependent under this specific reading
 *   - energy_poor_households_in_growing_economies: payer (powerless/trapped) — subsistence demand growth aggregated into restrainable 'demand'
 *   - portfolio_pragmatists: excluded (organized/analytical) — technology-neutral synthesis not admitted within this reading's premises
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_mitigation_legitimacy__degrowth_sufficiency_reading, 0.58).
domain_priors:suppression_score(climate_mitigation_legitimacy__degrowth_sufficiency_reading, 0.42).
domain_priors:theater_ratio(climate_mitigation_legitimacy__degrowth_sufficiency_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__degrowth_sufficiency_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__degrowth_sufficiency_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__degrowth_sufficiency_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__degrowth_sufficiency_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__degrowth_sufficiency_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_mitigation_legitimacy__degrowth_sufficiency_reading, tangled_rope).
narrative_ontology:human_readable(climate_mitigation_legitimacy__degrowth_sufficiency_reading, "Degrowth Sufficiency Reading of Climate Mitigation Legitimacy").
narrative_ontology:topic_domain(climate_mitigation_legitimacy__degrowth_sufficiency_reading, "energy_policy/climate_mitigation/technology_governance").

domain_priors:requires_active_enforcement(climate_mitigation_legitimacy__degrowth_sufficiency_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_mitigation_legitimacy__degrowth_sufficiency_reading, '10ceea0c-f9dc-45cd-9a16-91529ff5d2dc').
narrative_ontology:cs_kernel_codification('10ceea0c-f9dc-45cd-9a16-91529ff5d2dc', distributed).
narrative_ontology:cs_authority_grounding('10ceea0c-f9dc-45cd-9a16-91529ff5d2dc', distributed).
narrative_ontology:cs_reading_relation('10ceea0c-f9dc-45cd-9a16-91529ff5d2dc', climate_mitigation_legitimacy__baseload_necessity_reading, forecloses).
narrative_ontology:cs_reading_relation('10ceea0c-f9dc-45cd-9a16-91529ff5d2dc', climate_mitigation_legitimacy__renewable_primacy_reading, forecloses).
narrative_ontology:cs_reading_relation('10ceea0c-f9dc-45cd-9a16-91529ff5d2dc', climate_mitigation_legitimacy__portfolio_pragmatism_reading, forecloses).
narrative_ontology:cs_axiom('10ceea0c-f9dc-45cd-9a16-91529ff5d2dc', foundational, generation_expansion_presumptively_illegitimate).
narrative_ontology:cs_axiom_status(generation_expansion_presumptively_illegitimate, holdable).
narrative_ontology:cs_axiom_grounding('10ceea0c-f9dc-45cd-9a16-91529ff5d2dc', generation_expansion_presumptively_illegitimate, conventional).
narrative_ontology:cs_axiom('10ceea0c-f9dc-45cd-9a16-91529ff5d2dc', foundational, demand_reduction_sufficient_for_full_decarbonization).
narrative_ontology:cs_axiom_status(demand_reduction_sufficient_for_full_decarbonization, holdable).
narrative_ontology:cs_axiom_grounding('10ceea0c-f9dc-45cd-9a16-91529ff5d2dc', demand_reduction_sufficient_for_full_decarbonization, empirically_contingent).
narrative_ontology:cs_reference_frame('10ceea0c-f9dc-45cd-9a16-91529ff5d2dc', planetary_boundary_throughput_constraint).
narrative_ontology:cs_drift_state('10ceea0c-f9dc-45cd-9a16-91529ff5d2dc', post_ipcc_ar6_scenario_proliferation, gap(revival_pressure, substantial, false)).
narrative_ontology:cs_created_at('10ceea0c-f9dc-45cd-9a16-91529ff5d2dc', '').
narrative_ontology:cs_kernel_id(climate_mitigation_legitimacy__degrowth_sufficiency_reading, climate_mitigation_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_mitigation_legitimacy__degrowth_sufficiency_reading, sufficiency_policy_advocates).
narrative_ontology:constraint_beneficiary(climate_mitigation_legitimacy__degrowth_sufficiency_reading, existing_grid_asset_holders).
narrative_ontology:constraint_beneficiary(climate_mitigation_legitimacy__degrowth_sufficiency_reading, demand_reduction_consultancies).
narrative_ontology:constraint_victim(climate_mitigation_legitimacy__degrowth_sufficiency_reading, nuclear_developers).
narrative_ontology:constraint_victim(climate_mitigation_legitimacy__degrowth_sufficiency_reading, utility_scale_renewable_developers).
narrative_ontology:constraint_victim(climate_mitigation_legitimacy__degrowth_sufficiency_reading, grid_dependent_industrial_sectors).
narrative_ontology:constraint_victim(climate_mitigation_legitimacy__degrowth_sufficiency_reading, energy_poor_households_in_growing_economies).
narrative_ontology:constraint_vindicates(climate_mitigation_legitimacy__degrowth_sufficiency_reading, sufficiency_first_climate_doctrine).
narrative_ontology:constraint_vindicates(climate_mitigation_legitimacy__degrowth_sufficiency_reading, growth_decoupling_skepticism).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Research institutes, NGO coalitions, and academic degrowth economists who set the intellectual and policy agenda that demand reduction — not supply expansion — is the legitimate decarbonization pathway. They author the modeling scenarios (IPCC 'low energy demand' pathways, sufficiency corridors) that legitimize capping new generation build-out and shape funding priorities toward consumption reduction programs.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__degrowth_sufficiency_reading, sufficiency_policy_advocates, agenda_setter,
    organized, generational, analytical, global).

% Utilities and asset owners whose existing (often depreciated) generation and grid infrastructure retains value precisely because a sufficiency framing suppresses competing new capital deployment. They benefit from a policy environment that treats new large-scale build as presumptively unnecessary, protecting incumbent asset returns without requiring them to compete against fresh capacity.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__degrowth_sufficiency_reading, existing_grid_asset_holders, beneficiary,
    institutional, biographical, arbitrage, national).

% Energy efficiency auditors, behavioral-change program administrators, and demand-management contractors whose revenue depends on sufficiency being the dominant policy paradigm. They win the contracts that flow when governments prioritize demand-side programs over new generation procurement.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__degrowth_sufficiency_reading, demand_reduction_consultancies, beneficiary,
    organized, biographical, mobile, national).

% Firms and state agencies proposing new nuclear capacity are cast as pursuing growth-dependent, capital-intensive infrastructure that the sufficiency framing treats as unnecessary or even counterproductive. Financing, licensing, and public support erode when the reigning legitimacy narrative holds that demand reduction obviates the need for new dispatchable capacity; they cannot easily relocate the underlying claim that reactors are needed.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__degrowth_sufficiency_reading, nuclear_developers, payer,
    powerful, generational, constrained, global).

% Wind and solar developers planning gigawatt-scale build-out are equally captured as victims under this specific reading — the sufficiency argument does not exempt renewables from the growth-dependency critique. Financing rounds and siting approvals face the same legitimacy headwind: if demand falls enough, the case for large new renewable farms weakens, undercutting project pipelines regardless of the technology's climate credentials elsewhere.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__degrowth_sufficiency_reading, utility_scale_renewable_developers, payer,
    powerful, biographical, constrained, global).

% Manufacturing, data-center, and electrification-dependent industries (EV charging buildout, heat-pump conversion, green hydrogen) need more electricity, not less, to decarbonize their own processes. A sufficiency-dominant policy regime that caps generation growth directly constrains their ability to secure supply, and they have little practical ability to relocate operations or self-generate at the needed scale.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__degrowth_sufficiency_reading, grid_dependent_industrial_sectors, payer,
    moderate, biographical, trapped, national).

% Households in low- and middle-income countries whose per-capita consumption must rise substantially to meet basic needs (refrigeration, cooling, electrified cooking) are treated as part of aggregate 'demand' to be restrained under a global sufficiency framing calibrated largely on wealthy-country overconsumption. They bear the cost of a legitimacy narrative that does not distinguish their still-rising subsistence demand from discretionary consumption elsewhere, and have no practical channel to contest the global modeling assumptions that constrain financing for generation serving their regions.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__degrowth_sufficiency_reading, energy_poor_households_in_growing_economies, payer,
    powerless, biographical, trapped, global).

% Engineers, grid operators, and technology-neutral policy analysts who would argue that demand reduction and substantial new generation are not mutually exclusive, and that treating generation expansion as inherently illegitimate forecloses options prematurely. Their technical modeling is present in adjacent literature but is not admitted as authoritative within this reading's own legitimacy framework, which treats supply-side skepticism as the settled premise.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__degrowth_sufficiency_reading, portfolio_pragmatists, excluded,
    organized, generational, analytical, global).

% Academic and IEA/IPCC-adjacent modelers who compare low-energy-demand scenarios against high-electrification scenarios, documenting the assumptions embedded in each without being institutionally committed to either. They can trace how sensitive the sufficiency conclusion is to assumed decoupling rates and behavioral-change feasibility.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__degrowth_sufficiency_reading, independent_energy_modelers, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(climate_mitigation_legitimacy__degrowth_sufficiency_reading, diffuse).
narrative_ontology:fixing_cost_class(climate_mitigation_legitimacy__degrowth_sufficiency_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a genuine and real problem: if aggregate energy demand can be substantially reduced through efficiency, behavioral change, and structural economic reorganization, the embodied-carbon, land-use, and mineral-extraction costs of building enormous new generation fleets (nuclear or renewable) are avoided — a real physical and ecological coordination gain if demand reduction is achievable at the scale and speed claimed.
% TRANSFER_FUNCTION: Moves legitimacy, financing access, and regulatory priority away from generation-expansion projects (nuclear and utility-scale renewable alike) and toward demand-management programs and incumbent asset retention, while imposing the cost of constrained supply growth on industrial electrification and on populations whose energy consumption is still rising from a low base.
% ABSENT_VOICES: Grid engineers modeling reliability under high-demand-growth electrification scenarios, and representatives of energy-poor populations in the Global South whose subsistence-level consumption growth is aggregated into 'demand' figures the sufficiency framing treats as reducible — both are largely absent from the sufficiency-modeling literature's authorship, which is concentrated in wealthy-country academic and NGO institutions.
% DISAPPEARANCE_RATIONALE: If the sufficiency-legitimacy framing disappeared overnight, financing and permitting for both nuclear and utility-scale renewable projects would face substantially less ideological headwind, capital would flow toward generation expansion at a pace closer to portfolio-pragmatist or renewable-primacy projections, and demand-reduction program funding would likely contract as it lost its status as the primary legitimate pathway.
% FOUNDING_PROBLEM: Early degrowth and sufficiency economics arose to address the observation that GDP growth and material/energy throughput had not been convincingly decoupled at the pace or scale required for planetary boundary compliance, and that supply-side decarbonization alone (swapping fossil generation for equivalent-scale clean generation) risked reproducing the ecological footprint of the growth-dependent system it replaced.
% FOUNDING_PROBLEM_CORROBORATION: Ecological economists outside the degrowth advocacy network (e.g. mainstream decoupling-literature reviewers) attest that absolute decoupling has occurred in some wealthy economies at a pace that undercuts the strongest sufficiency claims, while grid reliability engineers and industrial electrification analysts — parties with no stake in the sufficiency framing's legitimacy — attest that projected electrification demand (EVs, heat pumps, data centers, green hydrogen) makes flat or declining aggregate demand implausible in most modeled decarbonization pathways. Sufficiency advocates themselves are the primary source asserting the founding problem remains fully live at global scale.
narrative_ontology:disappearance_verdict(climate_mitigation_legitimacy__degrowth_sufficiency_reading, world_rearranges).
narrative_ontology:founding_problem_status(climate_mitigation_legitimacy__degrowth_sufficiency_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(climate_mitigation_legitimacy__degrowth_sufficiency_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(climate_mitigation_legitimacy__degrowth_sufficiency_reading, 'none', 1).
narrative_ontology:epsilon_provenance(climate_mitigation_legitimacy__degrowth_sufficiency_reading, 0.58, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(climate_mitigation_legitimacy__degrowth_sufficiency_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(climate_mitigation_legitimacy__degrowth_sufficiency_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(climate_mitigation_legitimacy__degrowth_sufficiency_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58) is moderate-high and rising over the interval: the sufficiency framing has moved from a marginal academic position toward influencing real financing and permitting decisions, and as it gains institutional traction the cost imposed on generation-expansion projects (both nuclear and renewable) grows correspondingly. Suppression (0.42) is meaningfully lower than extractiveness because the sufficiency reading operates primarily through legitimacy and financing-access channels rather than direct coercive prohibition — developers can still pursue projects, just against a harder headwind of public and investor skepticism. Accessibility collapse (0.35) stays moderate because the portfolio-pragmatist and renewable-primacy alternatives remain visibly live in adjacent discourse; this reading has not achieved anything like mountain-level closure of alternatives. Resistance (0.71) is high because powerful, well-resourced actors (nuclear and renewable developers alike) actively contest the sufficiency framing in financing, media, and policy arenas.
 *
 * DIRECTIONALITY LOGIC:
 *   Sufficiency policy advocates and existing grid asset holders sit near the beneficiary end: the former gain intellectual and institutional authority, the latter gain protection from being competed against by new capacity. Demand reduction consultancies are a secondary beneficiary group whose revenue model depends on the sufficiency paradigm's continued dominance. Nuclear and renewable developers sit near the target end despite occupying very different positions in the broader energy debate — this is the reading's distinctive structural signature: it does not discriminate between clean generation technologies, it discriminates against generation expansion as such. Energy-poor households in growing economies sit at the extreme target end with the least power and the least exit: their rising subsistence consumption is aggregated into global demand figures they have no voice in constructing.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — whether growth and energy/material throughput can be decoupled fast enough for planetary-boundary compliance — remains genuinely contested rather than settled in either direction, which is why founding_problem_status is authored as contested rather than dead. This prevents the classification from either dismissing sufficiency framing as pure rent-seeking by incumbents (it is not: the underlying ecological question is real) or accepting it as costless coordination (it is not: it imposes real, asymmetric costs on generation-expansion actors and on energy-poor populations whose needs the framing was not calibrated against). The tangled_rope classification holds both truths simultaneously: genuine coordination function (avoiding unnecessary embodied-carbon and resource-extraction costs of overbuilding) plus asymmetric extraction (concentrated cost falling on specific developer classes and diffuse populations) requiring active enforcement (financing gatekeeping, permitting friction, narrative dominance in policy circles) to persist.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    decoupling_feasibility_ambiguity,
    'Can global energy demand be reduced at the pace and scale the sufficiency reading requires without economic or welfare costs that fall disproportionately on lower-income populations and regions still below subsistence consumption levels?',
    'Longitudinal cross-country data on absolute decoupling rates achieved under sufficiency-oriented policy versus supply-expansion policy, disaggregated by starting income level and controlling for measurement-boundary effects (consumption- vs. production-based accounting).',
    'If decoupling at the required pace is empirically infeasible without welfare losses concentrated on the powerless, the sufficiency reading''s coordination claim weakens substantially and its extraction on generation developers and energy-poor households looks less like avoided harm and more like a cost shifted without a corresponding benefit. If feasible, the coordination function is vindicated and the extraction is more clearly the necessary cost of a real gain.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(decoupling_feasibility_ambiguity, empirical, 'Whether aggregate demand reduction is achievable at the scale claimed without disproportionate cost to low-consumption populations.').

omega_variable(
    kernel_reading_boundary_location,
    'Where exactly does the disagreement between this reading and its siblings (baseload_necessity, renewable_primacy, portfolio_pragmatism) actually live — in empirical forecasts of achievable demand reduction, or in a prior normative commitment about whether growth-oriented infrastructure expansion is legitimate regardless of its empirical payoff?',
    'Structured elicitation distinguishing sufficiency advocates'' empirical demand-reduction forecasts from their normative priors about infrastructure growth; check whether advocates who accept high electrification-demand forecasts still oppose generation expansion on other grounds.',
    'If the disagreement is primarily empirical (a forecast dispute), the readings could in principle converge on new data and this constraint''s classification could shift toward rope as evidence resolves. If primarily normative (a prior commitment independent of forecasts), the readings are foreclosing rather than merely coexisting on this axis, and no amount of demand-forecast data would resolve the contest.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_boundary_location, conceptual, 'Whether the kernel contest is empirical (resolvable by demand data) or normative (a prior commitment about growth legitimacy).').

omega_variable(
    global_south_demand_aggregation_omega,
    'Is the aggregate global demand-reduction target, as modeled by sufficiency advocates concentrated in wealthy-country institutions, appropriately disaggregated to exempt subsistence-level consumption growth in low-income regions, or does the aggregate framing structurally impose wealthy-country consumption norms on populations with a legitimately different consumption trajectory?',
    'Audit of sufficiency-scenario modeling literature (e.g. IPCC low-energy-demand pathways) for explicit regional disaggregation and equity-weighting of demand targets versus uniform or per-capita-convergence assumptions.',
    'If the modeling literature already disaggregates and exempts subsistence growth, the victim classification of energy-poor households in growing economies should be softened or removed. If it does not, the victim classification is structurally well-founded and the extraction on that group is real rather than an artifact of this story''s framing.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(global_south_demand_aggregation_omega, empirical, 'Whether sufficiency-scenario demand targets are disaggregated to exempt subsistence-level consumption growth in low-income regions.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_mitigation_legitimacy__degrowth_sufficiency_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clim_tr_t0, climate_mitigation_legitimacy__degrowth_sufficiency_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement(clim_tr_t4, climate_mitigation_legitimacy__degrowth_sufficiency_reading, theater_ratio, 4, 0.2).
narrative_ontology:measurement(clim_tr_t8, climate_mitigation_legitimacy__degrowth_sufficiency_reading, theater_ratio, 8, 0.23).
narrative_ontology:measurement(clim_tr_t12, climate_mitigation_legitimacy__degrowth_sufficiency_reading, theater_ratio, 12, 0.25).
narrative_ontology:measurement(clim_tr_t16, climate_mitigation_legitimacy__degrowth_sufficiency_reading, theater_ratio, 16, 0.27).
narrative_ontology:measurement(clim_tr_t20, climate_mitigation_legitimacy__degrowth_sufficiency_reading, theater_ratio, 20, 0.28).

% Extraction over time
narrative_ontology:measurement(clim_be_t0, climate_mitigation_legitimacy__degrowth_sufficiency_reading, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(clim_be_t4, climate_mitigation_legitimacy__degrowth_sufficiency_reading, base_extractiveness, 4, 0.38).
narrative_ontology:measurement(clim_be_t8, climate_mitigation_legitimacy__degrowth_sufficiency_reading, base_extractiveness, 8, 0.45).
narrative_ontology:measurement(clim_be_t12, climate_mitigation_legitimacy__degrowth_sufficiency_reading, base_extractiveness, 12, 0.51).
narrative_ontology:measurement(clim_be_t16, climate_mitigation_legitimacy__degrowth_sufficiency_reading, base_extractiveness, 16, 0.55).
narrative_ontology:measurement(clim_be_t20, climate_mitigation_legitimacy__degrowth_sufficiency_reading, base_extractiveness, 20, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(clim_su_t0, climate_mitigation_legitimacy__degrowth_sufficiency_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(clim_su_t4, climate_mitigation_legitimacy__degrowth_sufficiency_reading, suppression_requirement, 4, 0.29).
narrative_ontology:measurement(clim_su_t8, climate_mitigation_legitimacy__degrowth_sufficiency_reading, suppression_requirement, 8, 0.33).
narrative_ontology:measurement(clim_su_t12, climate_mitigation_legitimacy__degrowth_sufficiency_reading, suppression_requirement, 12, 0.37).
narrative_ontology:measurement(clim_su_t16, climate_mitigation_legitimacy__degrowth_sufficiency_reading, suppression_requirement, 16, 0.4).
narrative_ontology:measurement(clim_su_t20, climate_mitigation_legitimacy__degrowth_sufficiency_reading, suppression_requirement, 20, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_mitigation_legitimacy__degrowth_sufficiency_reading, resource_allocation).
narrative_ontology:affects_constraint(climate_mitigation_legitimacy__degrowth_sufficiency_reading, climate_mitigation_legitimacy__baseload_necessity_reading).
narrative_ontology:affects_constraint(climate_mitigation_legitimacy__degrowth_sufficiency_reading, climate_mitigation_legitimacy__renewable_primacy_reading).
narrative_ontology:affects_constraint(climate_mitigation_legitimacy__degrowth_sufficiency_reading, climate_mitigation_legitimacy__portfolio_pragmatism_reading).

% DUAL FORMULATION NOTE:
% This story is one of four sibling readings of the climate_mitigation_legitimacy kernel, each authored as a separate ε-invariant constraint per the decomposition principle. The degrowth_sufficiency_reading is structurally distinctive among the four in placing BOTH nuclear and renewable developers in its victim set (the other three readings each favor one generation technology over another, or favor a portfolio of both over neither). Contamination/legitimacy pressure flows bidirectionally: as sufficiency framing gains institutional ground it directly reduces financing and legitimacy available to all three generation-expansion readings; conversely, empirical successes claimed by baseload_necessity or renewable_primacy readings (successful large-scale build-out at falling cost) exert downward pressure on the sufficiency reading's founding-problem status by demonstrating decoupling or clean-supply feasibility at scale.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
