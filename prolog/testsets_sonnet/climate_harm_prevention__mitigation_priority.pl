% ============================================================================
% CONSTRAINT STORY: climate_harm_prevention__mitigation_priority
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
 *   constraint_id: climate_harm_prevention__mitigation_priority
 *   human_readable: Mitigation-Priority Reading of Climate Harm Prevention (Growth-Compatible Decarbonization)
 *   domain: climate_policy/political_economy/intergenerational_ethics
 *
 * SUMMARY:
 *   This story instantiates the mitigation-priority reading of the climate
 *   harm prevention kernel: the position that legitimate climate response
 *   means reducing emissions fast enough to prevent future harm, achieved
 *   through technological substitution (renewables, EVs, carbon capture)
 *   within an intact growth framework. This is the dominant framing of the
 *   UNFCCC/Paris Agreement architecture and most national net-zero
 *   legislation. It is distinct from the adaptation-priority reading (which
 *   treats mitigation as infeasible and prioritizes near-term resilience) and
 *   the degrowth reading (which treats growth-compatible decarbonization as
 *   physically impossible and calls for planned contraction). Each reading
 *   has a different beneficiary/victim structure and a different epsilon;
 *   they are not measured on a single sliding scale but authored as separate
 *   constraints linked through the shared kernel.
 *
 * KEY AGENTS:
 *   - future_generations: primary intended beneficiary (powerless/trapped) — receives avoided harm but has no present voice
 *   - green_technology_firms: concentrated present beneficiary (organized/arbitrage) — captures the addressable market the mandate creates
 *   - multilateral_climate_institutions: agenda_setter (institutional/analytical) — designs, administers, and defends the mitigation-priority framework as the legitimate response
 *   - fossil_fuel_dependent_workers: primary payer (powerless/trapped) — bears concentrated transition cost
 *   - carbon_intensive_export_economies: institutional payer (moderate/constrained) — bears constrained development trajectory
 *   - degrowth_advocates and adaptation_focused_vulnerable_states: excluded voices whose rival readings of the same kernel are marginalized by mitigation-priority's institutional dominance
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_harm_prevention__mitigation_priority, 0.52).
domain_priors:suppression_score(climate_harm_prevention__mitigation_priority, 0.44).
domain_priors:theater_ratio(climate_harm_prevention__mitigation_priority, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_harm_prevention__mitigation_priority, extractiveness, 0.52).
narrative_ontology:constraint_metric(climate_harm_prevention__mitigation_priority, suppression_requirement, 0.44).
narrative_ontology:constraint_metric(climate_harm_prevention__mitigation_priority, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(climate_harm_prevention__mitigation_priority, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(climate_harm_prevention__mitigation_priority, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_harm_prevention__mitigation_priority, tangled_rope).
narrative_ontology:human_readable(climate_harm_prevention__mitigation_priority, "Mitigation-Priority Reading of Climate Harm Prevention (Growth-Compatible Decarbonization)").
narrative_ontology:topic_domain(climate_harm_prevention__mitigation_priority, "climate_policy/political_economy/intergenerational_ethics").

domain_priors:requires_active_enforcement(climate_harm_prevention__mitigation_priority).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_harm_prevention__mitigation_priority, '7e50c41c-4085-4d68-bc27-8c7eba7227b2').
narrative_ontology:cs_kernel_codification('7e50c41c-4085-4d68-bc27-8c7eba7227b2', distributed).
narrative_ontology:cs_authority_grounding('7e50c41c-4085-4d68-bc27-8c7eba7227b2', distributed).
narrative_ontology:cs_reading_relation('7e50c41c-4085-4d68-bc27-8c7eba7227b2', climate_harm_prevention__adaptation_priority, coexists_with).
narrative_ontology:cs_reading_relation('7e50c41c-4085-4d68-bc27-8c7eba7227b2', climate_harm_prevention__degrowth_reading, influences).
narrative_ontology:cs_axiom('7e50c41c-4085-4d68-bc27-8c7eba7227b2', foundational, technological_substitution_can_decouple_growth_from_emissions).
narrative_ontology:cs_axiom_status(technological_substitution_can_decouple_growth_from_emissions, holdable).
narrative_ontology:cs_axiom_grounding('7e50c41c-4085-4d68-bc27-8c7eba7227b2', technological_substitution_can_decouple_growth_from_emissions, empirically_contingent).
narrative_ontology:cs_axiom('7e50c41c-4085-4d68-bc27-8c7eba7227b2', foundational, future_harm_prevention_takes_priority_over_present_distributional_cost).
narrative_ontology:cs_axiom_status(future_harm_prevention_takes_priority_over_present_distributional_cost, holdable).
narrative_ontology:cs_axiom_grounding('7e50c41c-4085-4d68-bc27-8c7eba7227b2', future_harm_prevention_takes_priority_over_present_distributional_cost, deontological).
narrative_ontology:cs_reference_frame('7e50c41c-4085-4d68-bc27-8c7eba7227b2', rio_precautionary_common_but_differentiated_responsibility).
narrative_ontology:cs_drift_state('7e50c41c-4085-4d68-bc27-8c7eba7227b2', post_paris_ratchet_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('7e50c41c-4085-4d68-bc27-8c7eba7227b2', '').
narrative_ontology:cs_kernel_id(climate_harm_prevention__mitigation_priority, climate_harm_prevention).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_harm_prevention__mitigation_priority, future_generations).
narrative_ontology:constraint_beneficiary(climate_harm_prevention__mitigation_priority, green_technology_firms).
narrative_ontology:constraint_beneficiary(climate_harm_prevention__mitigation_priority, renewable_energy_investors).
narrative_ontology:constraint_beneficiary(climate_harm_prevention__mitigation_priority, multilateral_climate_institutions).
narrative_ontology:constraint_victim(climate_harm_prevention__mitigation_priority, fossil_fuel_dependent_workers).
narrative_ontology:constraint_victim(climate_harm_prevention__mitigation_priority, carbon_intensive_export_economies).
narrative_ontology:constraint_victim(climate_harm_prevention__mitigation_priority, energy_cost_burdened_households).
narrative_ontology:constraint_vindicates(climate_harm_prevention__mitigation_priority, growth_compatible_decarbonization_feasibility).
narrative_ontology:constraint_vindicates(climate_harm_prevention__mitigation_priority, technological_substitution_sufficiency).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Cannot participate in present policy negotiation but are the named recipients of avoided climate harm. Their claimed interest (a stable climate) is invoked to justify present costs, but they have no seat, no vote, and no mechanism to enforce that the transition actually delivers the promised avoided harm.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__mitigation_priority, future_generations, beneficiary,
    powerless, civilizational, trapped, global).

% Manufacture solar panels, batteries, EVs, and carbon-capture equipment. Subsidies, mandates, and carbon pricing that enforce the transition create their addressable market directly. They can relocate production and lobby across jurisdictions to capture the most favorable subsidy regimes, giving them mobility the workers being displaced do not have.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__mitigation_priority, green_technology_firms, beneficiary,
    organized, generational, arbitrage, global).

% Capital flows toward assets whose value depends on continued regulatory and price support for decarbonization. They benefit from policy certainty and can exit into other asset classes if the framework weakens, unlike workers or regions locked into physical infrastructure.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__mitigation_priority, renewable_energy_investors, beneficiary,
    powerful, biographical, arbitrage, global).

% Set emissions targets, administer carbon markets, and certify national commitments under the mitigation-priority framing. They design and defend the growth-compatible transition pathway, adjudicate compliance, and control the legitimacy narrative that determines which climate responses count as serious.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__mitigation_priority, multilateral_climate_institutions, agenda_setter,
    institutional, generational, analytical, global).

% Employed in coal, oil, gas extraction and adjacent industries slated for phase-out. Retraining programs are underfunded relative to the speed of mandated transition; relocation is costly and disruptive to family and community ties. They bear concentrated, immediate costs for a benefit (avoided future harm) that accrues to people not yet born.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__mitigation_priority, fossil_fuel_dependent_workers, payer,
    powerless, biographical, trapped, regional).

% National economies whose exports (coal, steel, cement, petrochemicals) face carbon border adjustments and market access restrictions under the mitigation framework. They must either subsidize their own decarbonization at high fiscal cost or lose export competitiveness; their development trajectory is constrained by rules written primarily by wealthier economies that industrialized without equivalent restriction.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__mitigation_priority, carbon_intensive_export_economies, payer,
    moderate, generational, constrained, national).

% Face higher electricity and fuel prices as carbon pricing and grid transition costs pass through to retail rates. Cannot easily substitute away from energy consumption and have no meaningful voice in setting the pace of transition, despite bearing its most immediate financial pressure.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__mitigation_priority, energy_cost_burdened_households, payer,
    powerless, immediate, trapped, national).

% Argue that growth-compatible decarbonization is a physical impossibility given resource and energy throughput constraints, and that planned contraction in wealthy economies is the only honest response. Their position is treated as politically unserious within mitigation-priority institutions and is largely excluded from mainstream policy negotiation rooms.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__mitigation_priority, degrowth_advocates, excluded,
    moderate, civilizational, constrained, global).

% Low-lying and climate-exposed states argue mitigation is already too slow to prevent significant harm to them within their lifetimes and want resources redirected toward resilience and loss-and-damage funding now. Their near-term survival claims compete with, and are often subordinated to, the mitigation-priority framework's long-horizon logic.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__mitigation_priority, adaptation_focused_vulnerable_states, excluded,
    powerless, immediate, trapped, regional).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(climate_harm_prevention__mitigation_priority, diffuse).
narrative_ontology:fixing_cost_class(climate_harm_prevention__mitigation_priority, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a global emissions-reduction effort by aligning national policy, private investment, and technology deployment around a shared target trajectory, solving a genuine collective-action problem: no single actor's mitigation matters without coordinated global reduction.
% TRANSFER_FUNCTION: Moves transition costs (stranded assets, retraining burdens, higher energy prices, constrained industrial development) from present carbon-intensive workers, regions, and economies toward present green-technology capital and toward future generations who receive the avoided-harm benefit without bearing present cost.
% ABSENT_VOICES: Fossil-fuel-dependent workers and carbon-intensive export economies have limited voice in setting the pace of transition relative to the diplomatic and financial weight of wealthy economies and green-capital interests. Degrowth advocates and near-term-vulnerable states are structurally outside the room where mitigation-priority framing is negotiated and enforced.
% DISAPPEARANCE_RATIONALE: Green-technology firms and multilateral institutions would argue the world rearranges catastrophically — investment flows, national targets, and technology deployment pathways collapse without the mitigation-priority framework's enforcement mechanisms. Fossil-fuel-dependent workers and carbon-intensive economies might argue their immediate situation would improve or at least stabilize, while degrowth advocates and near-term-vulnerable states would argue the underlying climate problem persists regardless of which framework nominally governs it — the disagreement itself reflects the kernel contest.
% FOUNDING_PROBLEM: Rising atmospheric greenhouse gas concentrations threaten catastrophic, irreversible harm to ecosystems and human societies; the mitigation-priority reading was built to solve this by reducing emissions fast enough to avoid the worst outcomes, while preserving economic growth as the mechanism for generating the technology and capital needed to do so.
% FOUNDING_PROBLEM_CORROBORATION: Independent physical climate science (IPCC assessment reports, drawing on measurement records outside any single national or corporate interest) corroborates that the underlying problem — accumulating radiative forcing from greenhouse gases — remains live and worsening. However, whether growth-compatible technological substitution is a sufficient response, versus requiring degrowth or accepting an adaptation-first posture, is contested even among climate scientists and economists who are not beneficiaries of any single reading.
narrative_ontology:disappearance_verdict(climate_harm_prevention__mitigation_priority, contested).
narrative_ontology:founding_problem_status(climate_harm_prevention__mitigation_priority, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(climate_harm_prevention__mitigation_priority, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(climate_harm_prevention__mitigation_priority, 'none', 1).
narrative_ontology:epsilon_provenance(climate_harm_prevention__mitigation_priority, 0.52, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness (0.52) and suppression (0.44) are authored at moderate-substantial levels: the mitigation-priority framework has a genuine coordination function (aligning global emissions reduction is a real collective-action problem) but also demonstrably transfers concentrated present costs onto specific workers and economies while the primary claimed beneficiary (future generations) is not a party who can verify or enforce delivery. Theater ratio rises over the measured interval (0.20 to 0.40) reflecting the growing gap between headline net-zero pledges and measured emissions trajectories — a substantial and growing share of the framework's visible activity (summits, pledges, offset markets) has drifted toward symbolic compliance relative to physical decarbonization. Suppression requirement also rises (0.22 to 0.44) as carbon border adjustments, disclosure mandates, and compliance architecture have hardened over three decades from voluntary commitments (Rio 1992) toward binding-adjacent mechanisms (Paris ratchet, CBAM). All three tracked metrics share the same six-point time grid.
 *
 * DIRECTIONALITY LOGIC:
 *   Future generations and green-technology/investment capital sit near the beneficiary end: future generations receive the claimed avoided-harm benefit without paying transition costs, and present green capital captures concentrated market benefit from the enforcement architecture. Fossil-fuel-dependent workers, carbon-intensive export economies, and energy-cost-burdened households sit near the target end: they bear concentrated, immediate, and geographically specific costs with limited exit (trapped or constrained) for a benefit whose delivery they cannot verify and whose timeline exceeds their own planning horizon. Multilateral institutions are the agenda-setter seat — they do not directly extract rents but they administer and defend the framework, and their legitimacy is bound up with the mitigation-priority reading's continued institutional dominance over its sibling readings.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (rising GHG concentrations threatening catastrophic harm) is independently corroborated as live by physical climate science outside any beneficiary group. What is contested is not whether the problem exists but whether THIS reading's proposed mechanism (growth-compatible technological substitution) is adequate to it, or whether the mandate has partially drifted from emissions reduction toward maintaining the growth-compatibility commitment itself as an end. The rising theater_ratio series is evidence for a mild Goodhart-style substitution: pledged targets and summit outputs increasingly stand in for measured atmospheric outcomes. This does not mean the coordination function is fake — it means the classification should track the tangled_rope structure (real coordination + real asymmetric extraction) rather than collapsing to either a pure rope (all coordination, no victims) or pure snare (all extraction, no genuine function) reading.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    growth_compatibility_feasibility,
    'Is technological substitution within an intact growth framework physically and politically sufficient to achieve the emissions reductions required to avoid catastrophic warming, or does the growth-compatibility premise itself constrain the pace of decarbonization below what is required?',
    'Longitudinal comparison of decoupling rates (GDP growth vs. absolute emissions) against required IPCC reduction pathways; if absolute decoupling at required rates is empirically demonstrated at scale, the premise is vindicated, if not, the degrowth reading''s critique is strengthened.',
    'If growth-compatible decarbonization proves structurally insufficient, this reading''s claimed coordination function (preventing future harm) is undermined even as its cost-transfer structure persists, sharpening the case that this constraint is more extractive than coordinating.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(growth_compatibility_feasibility, empirical, 'Whether growth-compatible mitigation can physically deliver the harm prevention it claims to.').

omega_variable(
    intergenerational_beneficiary_verification,
    'Can present institutions meaningfully verify that costs imposed on present carbon-intensive workers and economies actually translate into avoided harm for future generations, given the long causal lag and absence of any feedback mechanism from the future?',
    'No direct empirical resolution is possible in principle (future generations cannot report back); proxy resolution via climate model validation against realized trajectories over multi-decade windows.',
    'If the causal link between present transition costs and future harm reduction is weak or highly uncertain, the framework''s claimed coordination function is harder to distinguish from a transfer justified by an unfalsifiable future beneficiary.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(intergenerational_beneficiary_verification, conceptual, 'Irreducible uncertainty in verifying benefit delivery to a beneficiary class that cannot participate in the present.').

omega_variable(
    reading_dominance_vs_merit,
    'Does mitigation-priority''s institutional dominance over the adaptation-priority and degrowth readings reflect genuine analytical superiority, or does it reflect the fact that mitigation-priority is the reading most compatible with the interests of currently powerful actors (green capital, wealthy-economy governments, multilateral institutions)?',
    'Comparative analysis of whose interests are structurally served by each reading''s institutional dominance, cross-checked against independent physical-outcome data on which reading''s predictions best track observed climate and economic trajectories.',
    'If institutional dominance tracks power rather than merit, the mitigation-priority reading''s legitimacy claim (that it is the objectively correct response) is weakened relative to its structural role in preserving growth-compatible arrangements for currently powerful actors.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_dominance_vs_merit, conceptual, 'Whether this reading''s dominance among the three kernel readings tracks analytical merit or structural power.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_harm_prevention__mitigation_priority, 1990, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clim_tr_t1990, climate_harm_prevention__mitigation_priority, theater_ratio, 1990, 0.2).
narrative_ontology:measurement(clim_tr_t1997, climate_harm_prevention__mitigation_priority, theater_ratio, 1997, 0.25).
narrative_ontology:measurement(clim_tr_t2005, climate_harm_prevention__mitigation_priority, theater_ratio, 2005, 0.3).
narrative_ontology:measurement(clim_tr_t2012, climate_harm_prevention__mitigation_priority, theater_ratio, 2012, 0.34).
narrative_ontology:measurement(clim_tr_t2018, climate_harm_prevention__mitigation_priority, theater_ratio, 2018, 0.38).
narrative_ontology:measurement(clim_tr_t2024, climate_harm_prevention__mitigation_priority, theater_ratio, 2024, 0.4).

% Extraction over time
narrative_ontology:measurement(clim_be_t1990, climate_harm_prevention__mitigation_priority, base_extractiveness, 1990, 0.28).
narrative_ontology:measurement(clim_be_t1997, climate_harm_prevention__mitigation_priority, base_extractiveness, 1997, 0.33).
narrative_ontology:measurement(clim_be_t2005, climate_harm_prevention__mitigation_priority, base_extractiveness, 2005, 0.38).
narrative_ontology:measurement(clim_be_t2012, climate_harm_prevention__mitigation_priority, base_extractiveness, 2012, 0.44).
narrative_ontology:measurement(clim_be_t2018, climate_harm_prevention__mitigation_priority, base_extractiveness, 2018, 0.49).
narrative_ontology:measurement(clim_be_t2024, climate_harm_prevention__mitigation_priority, base_extractiveness, 2024, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(clim_su_t1990, climate_harm_prevention__mitigation_priority, suppression_requirement, 1990, 0.22).
narrative_ontology:measurement(clim_su_t1997, climate_harm_prevention__mitigation_priority, suppression_requirement, 1997, 0.28).
narrative_ontology:measurement(clim_su_t2005, climate_harm_prevention__mitigation_priority, suppression_requirement, 2005, 0.33).
narrative_ontology:measurement(clim_su_t2012, climate_harm_prevention__mitigation_priority, suppression_requirement, 2012, 0.37).
narrative_ontology:measurement(clim_su_t2018, climate_harm_prevention__mitigation_priority, suppression_requirement, 2018, 0.41).
narrative_ontology:measurement(clim_su_t2024, climate_harm_prevention__mitigation_priority, suppression_requirement, 2024, 0.44).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_harm_prevention__mitigation_priority, resource_allocation).
narrative_ontology:boltzmann_floor_override(climate_harm_prevention__mitigation_priority, 0.12).
narrative_ontology:affects_constraint(climate_harm_prevention__mitigation_priority, climate_harm_prevention__adaptation_priority).
narrative_ontology:affects_constraint(climate_harm_prevention__mitigation_priority, climate_harm_prevention__degrowth_reading).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the climate_harm_prevention kernel. Each reading is authored as a separate constraint with its own epsilon, beneficiary/victim structure, and classification, per the epsilon-invariance principle. mitigation_priority (this story) assumes growth-compatible decarbonization and names future generations plus green capital as primary beneficiaries; adaptation_priority accepts higher warming and near-term resilience over emissions reduction; degrowth_reading treats growth-compatible mitigation as impossible and calls for planned Global North contraction. All three share the founding problem (rising GHG concentrations) but diverge on feasibility premises, time horizon, and who bears cost versus who benefits. Network edges here are structural (shared kernel, competing legitimacy claims, resource competition for policy attention and finance) rather than causal dependency.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
