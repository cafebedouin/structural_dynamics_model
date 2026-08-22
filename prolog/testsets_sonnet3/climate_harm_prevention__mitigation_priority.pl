% ============================================================================
% CONSTRAINT STORY: climate_harm_prevention__mitigation_priority
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: climate_harm_prevention__mitigation_priority
 *   human_readable: Mitigation-Priority Reading of Climate Harm Prevention (Growth-Compatible Decarbonization)
 *   domain: climate_policy/political_economy/intergenerational_ethics
 *
 * SUMMARY:
 *   This story instantiates the mitigation-priority reading of the contested
 *   climate-harm-prevention kernel: legitimate climate response is defined as
 *   emissions reduction pursued through technology substitution (renewables,
 *   EVs, carbon capture, efficiency) inside a continued-growth macroeconomic
 *   framework, justified primarily by harm prevented for future generations.
 *   This is a distinct constraint from the adaptation-priority reading (which
 *   accepts a higher warming trajectory and prioritizes near-term resilience)
 *   and the degrowth reading (which holds growth-compatible decarbonization
 *   to be physically impossible and demands planned contraction). Each
 *   reading has its own beneficiary/victim structure and its own epsilon;
 *   they are linked as siblings of one kernel, not merged into one story.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_harm_prevention__mitigation_priority, 0.58).
domain_priors:suppression_score(climate_harm_prevention__mitigation_priority, 0.47).
domain_priors:theater_ratio(climate_harm_prevention__mitigation_priority, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_harm_prevention__mitigation_priority, extractiveness, 0.58).
narrative_ontology:constraint_metric(climate_harm_prevention__mitigation_priority, suppression_requirement, 0.47).
narrative_ontology:constraint_metric(climate_harm_prevention__mitigation_priority, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(climate_harm_prevention__mitigation_priority, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(climate_harm_prevention__mitigation_priority, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_harm_prevention__mitigation_priority, tangled_rope).
narrative_ontology:human_readable(climate_harm_prevention__mitigation_priority, "Mitigation-Priority Reading of Climate Harm Prevention (Growth-Compatible Decarbonization)").
narrative_ontology:topic_domain(climate_harm_prevention__mitigation_priority, "climate_policy/political_economy/intergenerational_ethics").

domain_priors:requires_active_enforcement(climate_harm_prevention__mitigation_priority).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_harm_prevention__mitigation_priority, '6d21bf54-3de0-4a3a-a736-94362ba56cc9').
narrative_ontology:cs_kernel_codification('6d21bf54-3de0-4a3a-a736-94362ba56cc9', distributed).
narrative_ontology:cs_authority_grounding('6d21bf54-3de0-4a3a-a736-94362ba56cc9', distributed).
narrative_ontology:cs_reading_relation('6d21bf54-3de0-4a3a-a736-94362ba56cc9', climate_harm_prevention__adaptation_priority, coexists_with).
narrative_ontology:cs_reading_relation('6d21bf54-3de0-4a3a-a736-94362ba56cc9', climate_harm_prevention__degrowth_reading, influences).
narrative_ontology:cs_axiom('6d21bf54-3de0-4a3a-a736-94362ba56cc9', foundational, technological_substitution_can_decouple_growth_from_emissions).
narrative_ontology:cs_axiom_status(technological_substitution_can_decouple_growth_from_emissions, holdable).
narrative_ontology:cs_axiom_grounding('6d21bf54-3de0-4a3a-a736-94362ba56cc9', technological_substitution_can_decouple_growth_from_emissions, empirically_contingent).
narrative_ontology:cs_axiom('6d21bf54-3de0-4a3a-a736-94362ba56cc9', foundational, future_harm_prevention_justifies_present_transition_cost_regardless_of_present_distribution).
narrative_ontology:cs_axiom_status(future_harm_prevention_justifies_present_transition_cost_regardless_of_present_distribution, holdable).
narrative_ontology:cs_axiom_grounding('6d21bf54-3de0-4a3a-a736-94362ba56cc9', future_harm_prevention_justifies_present_transition_cost_regardless_of_present_distribution, instrumental).
narrative_ontology:cs_reference_frame('6d21bf54-3de0-4a3a-a736-94362ba56cc9', unfccc_growth_compatible_consensus).
narrative_ontology:cs_drift_state('6d21bf54-3de0-4a3a-a736-94362ba56cc9', post_paris_agreement_gap_reports, gap(axiom_overriding, substantial, true)).
narrative_ontology:cs_created_at('6d21bf54-3de0-4a3a-a736-94362ba56cc9', '').
narrative_ontology:cs_kernel_id(climate_harm_prevention__mitigation_priority, climate_harm_prevention).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_harm_prevention__mitigation_priority, future_generations).
narrative_ontology:constraint_beneficiary(climate_harm_prevention__mitigation_priority, green_technology_sector).
narrative_ontology:constraint_beneficiary(climate_harm_prevention__mitigation_priority, growth_compatible_policy_elites).
narrative_ontology:constraint_victim(climate_harm_prevention__mitigation_priority, fossil_fuel_dependent_workers).
narrative_ontology:constraint_victim(climate_harm_prevention__mitigation_priority, carbon_intensive_manufacturing_regions).
narrative_ontology:constraint_victim(climate_harm_prevention__mitigation_priority, global_south_frontline_communities).
narrative_ontology:constraint_vindicates(climate_harm_prevention__mitigation_priority, decarbonization_is_compatible_with_continued_growth).
narrative_ontology:constraint_vindicates(climate_harm_prevention__mitigation_priority, technological_substitution_can_meet_emissions_targets_in_time).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Cannot participate in current policy negotiation but are the named justification for the entire mitigation-priority framework. They inherit whatever emissions trajectory current institutions choose; the reading treats avoided future harm to this group as the primary legitimating good, though they have no seat at any table setting the pace of transition.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__mitigation_priority, future_generations, beneficiary,
    powerless, civilizational, trapped, global).
narrative_ontology:stakeholder_non_agent(climate_harm_prevention__mitigation_priority, future_generations).

% Captures subsidies, tax credits, procurement mandates, and carbon-pricing revenue redirected toward renewables, EVs, and carbon capture. Benefits directly and immediately from the mitigation-priority framing being adopted as the legitimate response, independent of whether the pace chosen actually prevents the harm it is justified by.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__mitigation_priority, green_technology_sector, beneficiary,
    organized, biographical, arbitrage, global).

% International bodies, national ministries, and multilateral finance institutions that write and enforce mitigation targets, carbon markets, and green industrial policy. They set the pace and mechanism of transition, choosing market-based and technology-substitution instruments over consumption limits or planned contraction, and defend this choice as the only politically viable and economically legitimate path.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__mitigation_priority, growth_compatible_policy_elites, agenda_setter,
    institutional, generational, mobile, global).

% Coal miners, oil and gas workers, and communities whose local economies are built around extractive industry face job loss, depressed property values, and social disruption as mitigation policy phases out their sector. Retraining and transition-fund promises are frequently underfunded or delayed; relocation is costly and disruptive to family and community ties.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__mitigation_priority, fossil_fuel_dependent_workers, payer,
    powerless, biographical, trapped, regional).

% Steel, cement, and chemical manufacturing hubs bear the direct cost of carbon pricing and compliance mandates, face competitiveness pressure from producers in jurisdictions with laxer rules, and absorb capital-stranding risk as facilities are retired ahead of their economic life. Some capacity exists to lobby for compensation or exemptions, but the underlying cost burden is structural.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__mitigation_priority, carbon_intensive_manufacturing_regions, payer,
    moderate, biographical, constrained, national).

% Communities already experiencing climate harm bear both the ongoing damages the mitigation-priority pace fails to prevent quickly enough, and the downstream costs of mineral extraction (lithium, cobalt, nickel) required for the green-technology buildout that mitigation-priority relies on. They have limited representation in the institutions that set global mitigation targets and technology-transition financing terms.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__mitigation_priority, global_south_frontline_communities, payer,
    powerless, biographical, trapped, global).
narrative_ontology:stakeholder_secondary_role(climate_harm_prevention__mitigation_priority, global_south_frontline_communities, excluded).

% Argue that growth-compatible decarbonization cannot physically deliver emissions cuts fast enough, and that continued material throughput growth undermines the mitigation goal itself. Their position is treated as politically unserious within mainstream climate-policy institutions and rarely enters formal negotiating text, despite substantial academic and civil-society support.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__mitigation_priority, degrowth_advocates, excluded,
    moderate, generational, constrained, global).

% Some vulnerable-state governments argue mitigation-priority framing diverts finance and attention from urgent resilience-building they need now, given the emissions trajectory is already largely locked in. Their resilience-first framing competes for the same limited international climate finance mitigation-priority instruments are designed to allocate toward abatement.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__mitigation_priority, adaptation_focused_governments, excluded,
    moderate, biographical, constrained, national).

% Assess whether current mitigation trajectories under growth-compatible policy are consistent with stated temperature targets, publishing gap reports and pathway analyses that inform, but do not control, which reading of legitimate climate response prevails in policy.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__mitigation_priority, climate_scientists_and_iea_analysts, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a global transition of energy, transport, and industrial systems away from fossil fuels through price signals, subsidies, and technology standards, allowing many independent firms and states to redirect investment toward decarbonization without requiring centralized rationing of consumption or output.
% TRANSFER_FUNCTION: Moves capital and political attention toward green-technology investment and toward the future harm avoided for generations not yet born, while moving transition costs — stranded assets, job displacement, extraction burdens for battery minerals — onto present-day carbon-intensive workers, manufacturing regions, and Global South communities.
% ABSENT_VOICES: Degrowth advocates argue the growth-compatibility premise cannot deliver sufficient and timely emissions cuts; adaptation-focused governments argue mitigation-priority finance crowds out urgently needed resilience spending. Both positions are represented in academic literature and some diplomatic forums but are structurally marginal in the institutions (UNFCCC negotiating text, national NDCs, multilateral development bank lending criteria) that operationalize mitigation-priority as the default legitimate response.
% DISAPPEARANCE_RATIONALE: If mitigation-priority framing disappeared overnight, green-technology subsidy regimes, carbon markets, and NDC-based diplomatic architecture would lose their legitimating rationale and likely be renegotiated toward either adaptation-first finance or contraction-oriented policy — a real rearrangement for policy elites and the green-tech sector. But carbon-intensive workers and frontline communities dispute whether the current framing meaningfully changes their material trajectory versus a counterfactual without it, since emissions have continued rising under the mitigation-priority regime for decades.
% FOUNDING_PROBLEM: Scientific consensus by the late 1980s/1990s established that continued unabated greenhouse gas emissions would cause severe, escalating harm to human and ecological systems; a legitimating framework was needed to justify collective and costly present-day action to prevent harm that would otherwise fall on people not yet able to consent to or object to today's choices.
% FOUNDING_PROBLEM_CORROBORATION: IPCC assessment reports and independent climate science (not produced or controlled by green-technology firms or growth-compatible policy institutions) continue to corroborate that the underlying harm-prevention problem remains live and that current mitigation trajectories are insufficient to meet stated temperature targets — a status attested from outside the beneficiary set, even as it also documents that the growth-compatible mitigation pace chosen has not resolved the founding problem at the required speed.
narrative_ontology:disappearance_verdict(climate_harm_prevention__mitigation_priority, contested).
narrative_ontology:founding_problem_status(climate_harm_prevention__mitigation_priority, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(climate_harm_prevention__mitigation_priority, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(climate_harm_prevention__mitigation_priority, 'none', 1).
narrative_ontology:epsilon_provenance(climate_harm_prevention__mitigation_priority, 0.58, 'claude-sonnet-5', 'none', direct).

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
 *   Extraction is authored at a moderate-high 0.58: the coordination function (redirecting global capital toward decarbonization technology) is real, but the growth-compatibility premise systematically shifts transition costs onto fossil-fuel-dependent workers, carbon-intensive manufacturing regions, and Global South mineral-extraction-adjacent communities, while green-technology firms and policy elites who design the instruments capture concentrated near-term benefit. Theater ratio rises over the interval (0.25 to 0.42) reflecting the growing gap between NDC pledges/net-zero announcements and measured emissions trajectories — a Goodhart-style substitution of pledge theater for delivered abatement. Suppression rises modestly (0.25 to 0.47) as carbon-pricing and disclosure enforcement regimes have hardened, alongside growing marginalization of adaptation-first and degrowth positions within formal negotiating architecture.
 *
 * PERSPECTIVAL GAP:
 *   Growth-compatible policy elites and green-technology firms experience this constraint as legitimate, effective coordination toward a shared future good. Fossil-fuel-dependent workers and carbon-intensive regions experience the same structure as an imposed cost shifted onto them by institutions they do not control, justified by a future-harm rationale that offers them little present compensation. Global South frontline communities experience a double bind: harmed by both insufficient pace and by the extraction demands of the very technologies meant to help.
 *
 * DIRECTIONALITY LOGIC:
 *   Future generations are declared beneficiaries but are a non-agent entity here (no present standing to bargain) — the story lists them for completeness while excluding them from directionality math per the agent:false convention. Green-technology sector and growth-compatible policy elites are the concrete present-day beneficiaries who actually capture flows (subsidies, procurement, agenda control). Fossil-fuel workers, carbon-intensive regions, and Global South frontline communities are victims bearing concentrated transition costs and, in the frontline case, both the harms mitigation-priority is meant to prevent (arriving too slowly) and new extraction harms from the green buildout itself.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (preventing severe climate harm) remains scientifically live per outside corroboration (IPCC), which blocks a simple mandatrophy verdict — this is not a dead mandate persisting by inertia. But the specific INSTRUMENT choice (growth-compatible, technology-substitution mitigation) is a policy commitment layered on top of the live problem, and its persistence is increasingly defended by parties (green-tech capital, growth-compatible elites) who benefit from that specific instrument choice regardless of whether it is delivering harm prevention at the required pace. This is the tangled-rope signature: real coordination function, real founding problem, but also asymmetric extraction sustained by active enforcement (carbon market rules, subsidy allocation, NDC diplomatic pressure) that a pure rope would not require.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    growth_compatibility_empirical_status,
    'Is growth-compatible decarbonization physically capable of delivering emissions reductions fast enough to prevent the harm the mitigation-priority reading claims to prevent, or does continued material/energy throughput growth structurally undermine the pace required?',
    'Longitudinal comparison of decoupling rates (GDP growth vs. absolute emissions) against required IPCC pathway trajectories; if absolute decoupling at sufficient pace is never observed at scale, this would corroborate the degrowth reading''s core empirical claim against mitigation-priority''s founding assumption.',
    'If growth-compatible decarbonization is empirically shown insufficient, mitigation-priority''s claimed coordination function collapses into cover for continued extraction under a harm-prevention label, sharpening its classification toward pure extraction; if sufficient decoupling is demonstrated, the tangled-rope reading''s coordination component strengthens relative to its extraction component.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(growth_compatibility_empirical_status, empirical, 'Whether growth-compatible decarbonization can physically meet required emissions pathways.').

omega_variable(
    instrument_choice_capture_ambiguity,
    'Is the specific choice of market-based, technology-substitution instruments (over consumption limits, rationing, or planned contraction) a good-faith judgment about political feasibility, or a structural capture by parties who benefit from that specific instrument set regardless of its efficacy?',
    'Trace policy formation processes and lobbying expenditure by green-technology and growth-compatible institutional actors relative to instrument design choices in major climate legislation and international agreements; compare against counterfactual instrument sets proposed by excluded voices (degrowth, adaptation-first).',
    'Evidence of capture-driven instrument selection would sharpen the tangled-rope classification toward snare at the instrument level even while the underlying harm-prevention coordination function remains real; evidence of good-faith feasibility judgment under genuine political constraint would support the tangled-rope reading as currently authored.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(instrument_choice_capture_ambiguity, conceptual, 'Whether growth-compatible instrument choice reflects capture or genuine feasibility constraint.').

omega_variable(
    future_generations_representation_gap,
    'Can a constraint legitimately claim future generations as its primary beneficiary and justificatory basis when that group has zero representation in the institutions setting the pace and instrument choice of the response?',
    'Compare mitigation-priority''s actual delivered emissions trajectory against what full-weight future-generations representation (e.g., via long-termist discount-rate mandates in policy) would imply; assess whether present-beneficiary capture (green-tech, policy elites) systematically diverges from what a future-generations-optimizing pace would look like.',
    'A wide divergence would suggest ''future generations'' functions rhetorically to legitimate present extraction rather than substantively constraining policy — reinforcing the false-summit-adjacent reading that a beneficiary group with no bargaining power cannot check the arrangement''s actual direction.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(future_generations_representation_gap, conceptual, 'Whether the declared primary beneficiary (future generations) has any structural check on present instrument choice.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_harm_prevention__mitigation_priority, 1992, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clim_tr_t1992, climate_harm_prevention__mitigation_priority, theater_ratio, 1992, 0.25).
narrative_ontology:measurement(clim_tr_t1997, climate_harm_prevention__mitigation_priority, theater_ratio, 1997, 0.3).
narrative_ontology:measurement(clim_tr_t2005, climate_harm_prevention__mitigation_priority, theater_ratio, 2005, 0.34).
narrative_ontology:measurement(clim_tr_t2015, climate_harm_prevention__mitigation_priority, theater_ratio, 2015, 0.38).
narrative_ontology:measurement(clim_tr_t2020, climate_harm_prevention__mitigation_priority, theater_ratio, 2020, 0.4).
narrative_ontology:measurement(clim_tr_t2024, climate_harm_prevention__mitigation_priority, theater_ratio, 2024, 0.42).

% Extraction over time
narrative_ontology:measurement(clim_be_t1992, climate_harm_prevention__mitigation_priority, base_extractiveness, 1992, 0.32).
narrative_ontology:measurement(clim_be_t1997, climate_harm_prevention__mitigation_priority, base_extractiveness, 1997, 0.36).
narrative_ontology:measurement(clim_be_t2005, climate_harm_prevention__mitigation_priority, base_extractiveness, 2005, 0.42).
narrative_ontology:measurement(clim_be_t2015, climate_harm_prevention__mitigation_priority, base_extractiveness, 2015, 0.5).
narrative_ontology:measurement(clim_be_t2020, climate_harm_prevention__mitigation_priority, base_extractiveness, 2020, 0.55).
narrative_ontology:measurement(clim_be_t2024, climate_harm_prevention__mitigation_priority, base_extractiveness, 2024, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(clim_su_t1992, climate_harm_prevention__mitigation_priority, suppression_requirement, 1992, 0.25).
narrative_ontology:measurement(clim_su_t1997, climate_harm_prevention__mitigation_priority, suppression_requirement, 1997, 0.3).
narrative_ontology:measurement(clim_su_t2005, climate_harm_prevention__mitigation_priority, suppression_requirement, 2005, 0.35).
narrative_ontology:measurement(clim_su_t2015, climate_harm_prevention__mitigation_priority, suppression_requirement, 2015, 0.4).
narrative_ontology:measurement(clim_su_t2020, climate_harm_prevention__mitigation_priority, suppression_requirement, 2020, 0.44).
narrative_ontology:measurement(clim_su_t2024, climate_harm_prevention__mitigation_priority, suppression_requirement, 2024, 0.47).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_harm_prevention__mitigation_priority, resource_allocation).
narrative_ontology:boltzmann_floor_override(climate_harm_prevention__mitigation_priority, 0.12).
narrative_ontology:affects_constraint(climate_harm_prevention__mitigation_priority, climate_harm_prevention__adaptation_priority).
narrative_ontology:affects_constraint(climate_harm_prevention__mitigation_priority, climate_harm_prevention__degrowth_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the climate_harm_prevention kernel, decomposed per the epsilon-invariance principle because measuring 'legitimate climate response' under different instrument/beneficiary assumptions yields structurally distinct extraction profiles. mitigation_priority (this story, epsilon=0.58, tangled_rope) assumes growth-compatible technology substitution with future generations as declared beneficiary and present carbon-intensive sectors as payers. adaptation_priority accepts higher warming and prioritizes near-term resilience finance, with a different and likely lower epsilon reflecting more direct, less contested coordination. degrowth_reading holds growth-compatible decarbonization physically impossible and mandates Global North contraction, with beneficiary/victim sets inverted relative to this story (Global North consumption classes become payers). All three share the same founding problem (anthropogenic climate harm) but diverge on instrument legitimacy, beneficiary structure, and epsilon. Network edges are declared bidirectionally in spirit: mitigation-priority's dominance in international finance architecture structurally starves adaptation-priority and degrowth-reading proposals of resources and legitimacy, which is why this story's edges point outward to both siblings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
