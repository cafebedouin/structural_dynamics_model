% ============================================================================
% CONSTRAINT STORY: climate_mitigation_legitimacy__portfolio_pragmatism_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_climate_mitigation_legitimacy__portfolio_pragmatism_reading, []).

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
 *   constraint_id: climate_mitigation_legitimacy__portfolio_pragmatism_reading
 *   human_readable: Technology-Neutral Portfolio Requirement in Climate Mitigation Governance
 *   domain: economic/political/environmental
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the climate_mitigation_legitimacy
 *   kernel: the portfolio_pragmatism_reading, which holds that optimal
 *   decarbonization requires a technology-neutral portfolio including both
 *   nuclear and renewables, with neither privileged a priori and the optimal
 *   mix varying by region. The constraint under examination is the
 *   governance-and-discourse regime that operationalizes this reading:
 *   scenario frameworks, national plan templates, funding conditionality, and
 *   the legitimacy norms that mark single-technology advocacy as outside
 *   respectable planning. The epsilon referent is this standing
 *   portfolio-governance arrangement itself, assessed as it actually
 *   operates, not the arrangement any sibling reading would install. KEY
 *   AGENTS (by structural relationship): - ipcc_iea_scenario_bodies: Agenda
 *   setter (institutional/constrained) - maintains the pathway architecture;
 *   - national_energy_ministries: Agenda setter with secondary benefit
 *   (institutional/constrained) - write plans under the frame; -
 *   nuclear_industry_and_supply_chain: Primary beneficiary
 *   (organized/identity_locked) - collects protected market share and
 *   guarantees; - diversified_integrated_utilities: Beneficiary
 *   (institutional/constrained) - mixed-fleet strategy vindicated; -
 *   energy_modeling_consultancies: Beneficiary (moderate/arbitrage) - sells
 *   the frame's machinery; - fossil_bridge_interests: Opportunist beneficiary
 *   (powerful/arbitrage) - rides neutrality language for delay; -
 *   renewable_developers: Payer with secondary benefit (organized/mobile) -
 *   capital diluted, seat guaranteed; - single_pathway_climate_advocates:
 *   Payer (moderate/identity_locked) - pays in discourse legitimacy; -
 *   electricity_ratepayers: Payer (powerless/trapped) - bears above-frontier
 *   cost incidence; - global_south_energy_access_advocates: Excluded voice
 *   (powerless/trapped); - energy_systems_analysts: Analytical observer
 *   (analytical/analytical).
 *
 * KEY AGENTS:
 *   - - ipcc_iea_scenario_bodies: Agenda setter (institutional/constrained) — maintains the scenario architecture defining pathway legitimacy
 *   - - national_energy_ministries: Agenda setter, secondary beneficiary (institutional/constrained) — write national plans under portfolio norms
 *   - - nuclear_industry_and_supply_chain: Primary beneficiary (organized/identity_locked) — collects protected market share, loan guarantees, and capacity payments legitimized by portfolio-diversity logic
 *   - - diversified_integrated_utilities: Beneficiary (institutional/constrained) — mixed-fleet asset strategy vindicated; regulated returns continue on legacy plants
 *   - - energy_modeling_consultancies: Beneficiary (moderate/arbitrage) — sells the integrated-assessment machinery that operationalizes the frame
 *   - - fossil_bridge_interests: Opportunist beneficiary (powerful/arbitrage) — invokes neutrality language to extend gas and defer phase-out mandates
 *   - - renewable_developers: Payer with secondary benefit (organized/mobile) — capital and connection queues diluted, but the frame guarantees renewables a permanent seat
 *   - - single_pathway_climate_advocates: Payer (moderate/identity_locked) — pays in discourse legitimacy; positions labeled ideological
 *   - - electricity_ratepayers: Payer (powerless/trapped) — bears tariff incidence of above-frontier mixes
 *   - - global_south_energy_access_advocates: Excluded voice (powerless/trapped) — access finance crowded out of the debate
 *   - - energy_systems_analysts: Analytical observer (analytical/analytical) — tests portfolio prescriptions against cost and reliability frontiers
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_mitigation_legitimacy__portfolio_pragmatism_reading, 0.52).
domain_priors:suppression_score(climate_mitigation_legitimacy__portfolio_pragmatism_reading, 0.42).
domain_priors:theater_ratio(climate_mitigation_legitimacy__portfolio_pragmatism_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__portfolio_pragmatism_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__portfolio_pragmatism_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__portfolio_pragmatism_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__portfolio_pragmatism_reading, accessibility_collapse, 0.38).
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__portfolio_pragmatism_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_mitigation_legitimacy__portfolio_pragmatism_reading, tangled_rope).
narrative_ontology:human_readable(climate_mitigation_legitimacy__portfolio_pragmatism_reading, "Technology-Neutral Portfolio Requirement in Climate Mitigation Governance").
narrative_ontology:topic_domain(climate_mitigation_legitimacy__portfolio_pragmatism_reading, "economic/political/environmental").

domain_priors:requires_active_enforcement(climate_mitigation_legitimacy__portfolio_pragmatism_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_mitigation_legitimacy__portfolio_pragmatism_reading, '20ca81a1-2c97-487c-98cb-74fbef9d1fe4').
narrative_ontology:cs_kernel_codification('20ca81a1-2c97-487c-98cb-74fbef9d1fe4', formalized).
narrative_ontology:cs_authority_grounding('20ca81a1-2c97-487c-98cb-74fbef9d1fe4', expertise).
narrative_ontology:cs_interpretation_layer_present('20ca81a1-2c97-487c-98cb-74fbef9d1fe4').
narrative_ontology:cs_reading_relation('20ca81a1-2c97-487c-98cb-74fbef9d1fe4', climate_mitigation_legitimacy__baseload_necessity_reading, influences).
narrative_ontology:cs_reading_relation('20ca81a1-2c97-487c-98cb-74fbef9d1fe4', climate_mitigation_legitimacy__renewable_primacy_reading, coexists_with).
narrative_ontology:cs_reading_relation('20ca81a1-2c97-487c-98cb-74fbef9d1fe4', climate_mitigation_legitimacy__degrowth_sufficiency_reading, coexists_with).
narrative_ontology:cs_axiom('20ca81a1-2c97-487c-98cb-74fbef9d1fe4', foundational, no_a_priori_technology_privilege).
narrative_ontology:cs_axiom_status(no_a_priori_technology_privilege, holdable).
narrative_ontology:cs_axiom_grounding('20ca81a1-2c97-487c-98cb-74fbef9d1fe4', no_a_priori_technology_privilege, instrumental).
narrative_ontology:cs_axiom('20ca81a1-2c97-487c-98cb-74fbef9d1fe4', foundational, regional_mix_variation_principle).
narrative_ontology:cs_axiom_status(regional_mix_variation_principle, holdable).
narrative_ontology:cs_axiom_grounding('20ca81a1-2c97-487c-98cb-74fbef9d1fe4', regional_mix_variation_principle, empirically_contingent).
narrative_ontology:cs_axiom('20ca81a1-2c97-487c-98cb-74fbef9d1fe4', secondary, deep_uncertainty_capital_hedging).
narrative_ontology:cs_axiom_status(deep_uncertainty_capital_hedging, holdable).
narrative_ontology:cs_axiom_grounding('20ca81a1-2c97-487c-98cb-74fbef9d1fe4', deep_uncertainty_capital_hedging, instrumental).
narrative_ontology:cs_reference_frame('20ca81a1-2c97-487c-98cb-74fbef9d1fe4', technology_neutral_regional_portfolio_norm).
narrative_ontology:cs_drift_state('20ca81a1-2c97-487c-98cb-74fbef9d1fe4', post_2015_cost_revolution_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('20ca81a1-2c97-487c-98cb-74fbef9d1fe4', '').
narrative_ontology:cs_kernel_id(climate_mitigation_legitimacy__portfolio_pragmatism_reading, climate_mitigation_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_mitigation_legitimacy__portfolio_pragmatism_reading, nuclear_industry_and_supply_chain).
narrative_ontology:constraint_beneficiary(climate_mitigation_legitimacy__portfolio_pragmatism_reading, diversified_integrated_utilities).
narrative_ontology:constraint_beneficiary(climate_mitigation_legitimacy__portfolio_pragmatism_reading, energy_modeling_consultancies).
narrative_ontology:constraint_beneficiary(climate_mitigation_legitimacy__portfolio_pragmatism_reading, fossil_bridge_interests).
narrative_ontology:constraint_victim(climate_mitigation_legitimacy__portfolio_pragmatism_reading, renewable_developers).
narrative_ontology:constraint_victim(climate_mitigation_legitimacy__portfolio_pragmatism_reading, single_pathway_climate_advocates).
narrative_ontology:constraint_victim(climate_mitigation_legitimacy__portfolio_pragmatism_reading, electricity_ratepayers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(climate_mitigation_legitimacy__portfolio_pragmatism_reading, national_energy_ministries).
narrative_ontology:constraint_beneficiary(climate_mitigation_legitimacy__portfolio_pragmatism_reading, renewable_developers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintain the scenario architectures and pathway taxonomies that define what counts as a legitimate decarbonization strategy. Their authority rests on perceived neutrality among technologies, so abandoning the portfolio frame would undercut the legitimacy basis they themselves depend on.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__portfolio_pragmatism_reading, ipcc_iea_scenario_bodies, agenda_setter,
    institutional, generational, constrained, global).

% Write integrated national energy and climate plans under portfolio norms. The frame gives them technocratic insulation from single-technology lobbies, and they absorb blame for cost premiums the frame produces in their jurisdictions.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__portfolio_pragmatism_reading, national_energy_ministries, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(climate_mitigation_legitimacy__portfolio_pragmatism_reading, national_energy_ministries, beneficiary).

% Receives protected market share, loan guarantees, capacity payments, and research funding justified by portfolio-diversity logic rather than standalone merchant economics. Reactor-vendor workforces and host communities have fused their professional and civic identities with continued portfolio inclusion; exit would mean writing off supply chains and skills pipelines built around the frame.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__portfolio_pragmatism_reading, nuclear_industry_and_supply_chain, beneficiary,
    organized, generational, identity_locked, global).

% Operate mixed generation fleets spanning nuclear, gas, and renewables. The portfolio frame vindicates retaining legacy assets alongside new builds and slows stranding; regulated returns continue flowing on whichever technology the approved plan includes. A full renewables-only pivot would strand their own capital, so exit is structurally expensive.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__portfolio_pragmatism_reading, diversified_integrated_utilities, beneficiary,
    institutional, generational, constrained, continental).

% Sell the integrated-assessment and capacity-expansion modeling that operationalizes portfolio optimization for governments and investors. Fee streams depend on the frame remaining the legitimate planning mode, but their tooling is portable to whichever frame wins.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__portfolio_pragmatism_reading, energy_modeling_consultancies, beneficiary,
    moderate, biographical, arbitrage, global).

% Invoke technology neutrality to extend gas as bridge capacity and resist binding phase-out mandates, capturing delay value while the neutrality language remains authoritative. They carry long-run exposure to the same transition the frame manages, which tempers but does not remove their short-run gain.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__portfolio_pragmatism_reading, fossil_bridge_interests, beneficiary,
    powerful, biographical, arbitrage, global).

% Compete for capital, grid-connection queues, and subsidy envelopes that portfolio rules split with nuclear; in some jurisdictions portfolio logic caps renewable buildout below assessed resource potential. At the same time the frame guarantees renewables a permanent central seat they might lose under baseload-necessity governance. A booming global project pipeline gives them unusually good exit relative to other payers.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__portfolio_pragmatism_reading, renewable_developers, payer,
    organized, biographical, mobile, global).
narrative_ontology:stakeholder_secondary_role(climate_mitigation_legitimacy__portfolio_pragmatism_reading, renewable_developers, beneficiary).

% Campaign for exclusive-renewable or demand-side pathways. The portfolio frame labels their positions ideological or premature and keeps them out of scenario-authorship seats. Their advocacy identity is fused with a single-technology vision, so rhetorical exit remains costly regardless of the evidence either way.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__portfolio_pragmatism_reading, single_pathway_climate_advocates, payer,
    moderate, civilizational, identity_locked, global).

% Bear the tariff and tax incidence wherever the approved mix costs more than the local least-cost frontier, as with nuclear cost overruns socialized through rates. They cannot opt out of the grid financing the chosen mix, and their numbers are too diffuse for cheap coalition formation, though bill-strike campaigns remain a latent channel.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__portfolio_pragmatism_reading, electricity_ratepayers, payer,
    powerless, immediate, trapped, regional).

% Would press the case that portfolio debates among OECD-scale centralized technologies crowd out finance and attention for distributed access, mini-grids, and clean cooking. They hold no seats in the scenario processes and national plan consultations where the frame is maintained.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__portfolio_pragmatism_reading, global_south_energy_access_advocates, excluded,
    powerless, generational, trapped, global).

% Publish capacity-expansion, reliability, and cost-frontier studies that test whether portfolio prescriptions track what the physics and the price curves actually support. From this seat both the genuine hedging value and the rent streams riding on the frame are visible at once.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__portfolio_pragmatism_reading, energy_systems_analysts, observer,
    analytical, biographical, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(climate_mitigation_legitimacy__portfolio_pragmatism_reading, nuclear_industry_and_supply_chain).
narrative_ontology:fixing_cost_class(climate_mitigation_legitimacy__portfolio_pragmatism_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates trillion-dollar generation-capital deployment under deep technological uncertainty: portfolio rules let planners hedge against any single technology failing on cost, construction time, or reliability, and match mixes to regional resource endowments instead of adjudicating a winner in advance.
% TRANSFER_FUNCTION: Moves capital-allocation share and policy legitimacy toward nuclear (protected market share, loan guarantees, capacity payments) and toward the integrative-planning consultancy sector; moves discourse authority away from single-technology advocates; in some jurisdictions moves ratepayer funds into mixes priced above the local least-cost frontier.
% ABSENT_VOICES: Global South energy-access advocates and energy-poor households are absent from scenario authorship and national plan consultation; future generations bearing any delay-induced residual emissions are unrepresented. Their objection, that portfolio pluralism among rich-country technologies rations capital away from access and speed, never enters the frame's own proceedings.
% DISAPPEARANCE_RATIONALE: Scenario architectures, national plan templates, subsidy conditionality, and advocacy coalitions all organize around portfolio neutrality. Overnight removal would force immediate re-adjudication of technology choice, reopen nuclear subsidy fights, strand the modeling and consulting industry built on the frame, and collapse the broad mitigation coalition the frame was built to hold together.
% FOUNDING_PROBLEM: Early-2000s climate governance repeatedly failed at prescriptive technology mandates: carbon prices were rejected at the ballot box, nuclear expansion was politically blocked, and renewables were dismissed as marginal. Scenario builders constructed a technology-neutral portfolio frame to depoliticize technology choice and hold a broad mitigation coalition together despite those failures.
% FOUNDING_PROBLEM_CORROBORATION: Political-science histories of IPCC scenario design and IEA modeling practice corroborate the coalition-broadening origin from outside the benefiting parties. Renewable-primacy and degrowth advocates, also outside the beneficiary set, attest that the frame now functions partly as delay cover, so the origin is corroborated while the current status is disputed across non-beneficiary seats.
narrative_ontology:disappearance_verdict(climate_mitigation_legitimacy__portfolio_pragmatism_reading, world_rearranges).
narrative_ontology:founding_problem_status(climate_mitigation_legitimacy__portfolio_pragmatism_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(climate_mitigation_legitimacy__portfolio_pragmatism_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(climate_mitigation_legitimacy__portfolio_pragmatism_reading, 'none', 1).
narrative_ontology:epsilon_provenance(climate_mitigation_legitimacy__portfolio_pragmatism_reading, 0.52, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(climate_mitigation_legitimacy__portfolio_pragmatism_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(climate_mitigation_legitimacy__portfolio_pragmatism_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(climate_mitigation_legitimacy__portfolio_pragmatism_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Claim and metrics are authored independently. The claimed type is tangled_rope because the structure exhibits both halves: a genuine coordination function (hedging trillion-dollar capital deployment against deep technological uncertainty, matching mixes to regional endowments, maintaining optionality) AND asymmetric extraction through the same structure (nuclear legitimacy rents detached from merchant economics, fossil delay riders on neutrality language, discourse gatekeeping against single-path positions). Metrics describe operation: extractiveness 0.52 reflects moderate forced capital diversification plus identifiable rent streams; suppression 0.42 is discourse-level gatekeeping (reviewer norms, funding conditionality, legitimacy labeling) rather than legal coercion, and is authored as a raw structural property the engine scales only for extractiveness; theater_ratio 0.28 reflects mostly-real modeling activity with a growing performative component in neutrality rhetoric; accessibility_collapse 0.38 because alternative framings persist and remain loudly argued rather than collapsing; resistance 0.62 because renewable-primacy, degrowth, and baseload constituencies all actively contest the frame's authority claims. The temporal series run on one shared grid (points 0, 6, 12, 18, 24, 30, mapping approximately 1995-2025): base_extractiveness rises as nuclear rents and delay riders accumulated on the frame; theater_ratio rises as neutrality language grew more performative relative to its analytic content; suppression_requirement rises because enforcing discourse neutrality required intensifying gatekeeping as single-path movements gained scientific and popular momentum after 2015. All points are observed history, not projection.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seats (scenario bodies, ministries) experience the frame as neutral methodology that protects process integrity and insulates planning from lobbying; the payer seats experience the same structure as gatekeeping that prices their alternatives out of legitimacy before the evidence is heard; the beneficiary seats experience it as earned pluralism that merely guarantees everyone a hearing. Same-level divergence is sharpest between renewable_developers and single_pathway_climate_advocates, both nominally pro-renewables actors at comparable power: the developers' mobile exit and revenue exposure make the frame a negotiable cost, while the advocates' identity-locked position makes it an existential discursive threat. The engine computes these divergent per-seat classifications from the structural data; the authored claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries derive low directionality: the nuclear industry and mixed-fleet utilities sit near the subsidized end, consultancies collect fees from the frame's operation, and fossil bridge interests extract delay value. Victims derive high directionality: ratepayers are trapped and bear concentrated cost incidence, single-path advocates pay in legitimacy, and renewable developers pay in capital dilution. No directionality_overrides are declared: the structural derivation from beneficiary/victim declarations plus exit options captures the relationships, including the dual-positioned renewable developers, whose secondary beneficiary role is declared on the stakeholder surface rather than forced through an override. The one candidate override, differentiating fossil_bridge_interests from other beneficiaries, was judged unnecessary because their arbitrage-grade exit already modulates their derived position appropriately.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (holding a mitigation coalition together after prescriptive mandates failed politically) is partially live: mandates still fail, so the frame's original warrant has not simply expired. Status is therefore contested rather than dead, and the R5 mismatch consumer reads contested-status against the world_rearranges disappearance verdict without firing the zombie flag. Mandatrophy analysis here prevents two mislabels: calling the frame a pure snare would erase the real hedging function that keeps capital flowing under genuine uncertainty, and calling it a pure rope would launder the nuclear rent streams and fossil delay riders that ride on the same neutrality language. Tangled_rope keeps both halves visible and lets the omegas (cost convergence, storage threshold, delay attribution) determine which half dominates as the evidence matures.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contestation,
    'This constraint is one reading of the climate_mitigation_legitimacy kernel: does the portfolio-pragmatism instantiation, rather than a sibling reading (baseload necessity, renewable primacy, degrowth sufficiency), correctly describe what legitimate mitigation requires?',
    'Resolution of the underlying technology contests (cost trajectories, reliability performance, demand elasticity) plus discourse analysis of which reading captures scenario-authority seats over time.',
    'Adopting baseload necessity raises epsilon for renewable exclusion; adopting renewable primacy converts the nuclear-inclusion component into pure rent; adopting degrowth sufficiency dissolves the expansion premise the portfolio frame administers.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'Committer structure: one of four live readings of the climate-mitigation-legitimacy kernel; disagreement located in whether any technology class deserves a priori standing in legitimate mitigation.').

omega_variable(
    nuclear_cost_convergence,
    'Will new nuclear costs, including small-modular-reactor learning curves, converge toward competitiveness such that portfolio inclusion reflects efficiency rather than legitimacy rent?',
    'Track overnight capital costs, construction durations, and SMR first-of-a-kind results against renewable-plus-storage system costs over the coming decade.',
    'Convergence supports the coordination reading of nuclear inclusion; continued divergence confirms the rent component and pushes the computed classification toward captured-snare territory.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(nuclear_cost_convergence, empirical, 'Whether the nuclear leg of the portfolio is efficiency or subsidy-sheltered rent.').

omega_variable(
    neutrality_rhetoric_delay_attribution,
    'How much fossil-generation delay is causally attributable to technology-neutrality rhetoric versus independent regulatory and economic factors?',
    'Comparative policy analysis across jurisdictions differing in neutrality-language adoption, controlling for gas prices, permitting regimes, and pre-existing coal fleets.',
    'High attribution raises epsilon and strengthens the extraction side of the tangled-rope verdict; low attribution leaves the frame closer to a plain rope.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(neutrality_rhetoric_delay_attribution, empirical, 'Causal weight of neutrality language in extending fossil asset lives.').

omega_variable(
    storage_dominance_threshold,
    'At what storage and grid-flexibility cost does renewables-plus-storage dominate the cost-reliability frontier in every major region, dissolving the both-technologies-needed premise?',
    'System-level capacity-expansion studies with sensitivity bands on long-duration storage costs and transmission buildout rates.',
    'Below the threshold the nuclear-inclusion component becomes vestigial rent and the frame drifts toward piton or snare; above it the hedging function stays live and the coordination half of the frame holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(storage_dominance_threshold, empirical, 'The cost frontier at which portfolio pluralism loses its empirical warrant.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_mitigation_legitimacy__portfolio_pragmatism_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clim_tr_t0, climate_mitigation_legitimacy__portfolio_pragmatism_reading, theater_ratio, 0, 0.14).
narrative_ontology:measurement(clim_tr_t6, climate_mitigation_legitimacy__portfolio_pragmatism_reading, theater_ratio, 6, 0.17).
narrative_ontology:measurement(clim_tr_t12, climate_mitigation_legitimacy__portfolio_pragmatism_reading, theater_ratio, 12, 0.2).
narrative_ontology:measurement(clim_tr_t18, climate_mitigation_legitimacy__portfolio_pragmatism_reading, theater_ratio, 18, 0.23).
narrative_ontology:measurement(clim_tr_t24, climate_mitigation_legitimacy__portfolio_pragmatism_reading, theater_ratio, 24, 0.26).
narrative_ontology:measurement(clim_tr_t30, climate_mitigation_legitimacy__portfolio_pragmatism_reading, theater_ratio, 30, 0.28).

% Extraction over time
narrative_ontology:measurement(clim_be_t0, climate_mitigation_legitimacy__portfolio_pragmatism_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(clim_be_t6, climate_mitigation_legitimacy__portfolio_pragmatism_reading, base_extractiveness, 6, 0.39).
narrative_ontology:measurement(clim_be_t12, climate_mitigation_legitimacy__portfolio_pragmatism_reading, base_extractiveness, 12, 0.43).
narrative_ontology:measurement(clim_be_t18, climate_mitigation_legitimacy__portfolio_pragmatism_reading, base_extractiveness, 18, 0.47).
narrative_ontology:measurement(clim_be_t24, climate_mitigation_legitimacy__portfolio_pragmatism_reading, base_extractiveness, 24, 0.5).
narrative_ontology:measurement(clim_be_t30, climate_mitigation_legitimacy__portfolio_pragmatism_reading, base_extractiveness, 30, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(clim_su_t0, climate_mitigation_legitimacy__portfolio_pragmatism_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(clim_su_t6, climate_mitigation_legitimacy__portfolio_pragmatism_reading, suppression_requirement, 6, 0.33).
narrative_ontology:measurement(clim_su_t12, climate_mitigation_legitimacy__portfolio_pragmatism_reading, suppression_requirement, 12, 0.36).
narrative_ontology:measurement(clim_su_t18, climate_mitigation_legitimacy__portfolio_pragmatism_reading, suppression_requirement, 18, 0.39).
narrative_ontology:measurement(clim_su_t24, climate_mitigation_legitimacy__portfolio_pragmatism_reading, suppression_requirement, 24, 0.42).
narrative_ontology:measurement(clim_su_t30, climate_mitigation_legitimacy__portfolio_pragmatism_reading, suppression_requirement, 30, 0.44).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_mitigation_legitimacy__portfolio_pragmatism_reading, resource_allocation).
narrative_ontology:affects_constraint(climate_mitigation_legitimacy__portfolio_pragmatism_reading, climate_mitigation_legitimacy__baseload_necessity_reading).
narrative_ontology:affects_constraint(climate_mitigation_legitimacy__portfolio_pragmatism_reading, climate_mitigation_legitimacy__renewable_primacy_reading).
narrative_ontology:affects_constraint(climate_mitigation_legitimacy__portfolio_pragmatism_reading, climate_mitigation_legitimacy__degrowth_sufficiency_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'what legitimate climate mitigation requires' decomposes into four structurally distinct readings of the climate_mitigation_legigimacy kernel, each with its own epsilon, beneficiary/victim structure, and classification. This file instantiates the portfolio_pragmatism_reading only. The upstream readings (baseload necessity, renewable primacy) are cited as evidence within portfolio discourse, and this reading structurally demotes baseload necessity from a-priori requirement to selectable option; degrowth sufficiency contests the expansion premise the portfolio frame presupposes. Cross-links run through network.affects_constraints in all four files.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
