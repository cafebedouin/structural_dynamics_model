% ============================================================================
% CONSTRAINT STORY: climate_mitigation_imperative__systems_transition_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_climate_mitigation_imperative__systems_transition_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: climate_mitigation_imperative__systems_transition_reading
 *   human_readable: Climate Mitigation as Energy-System Democratization Imperative (Systems Transition Reading)
 *   domain: energy_policy/climate_mitigation/technology_governance
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the contested 'climate mitigation
 *   imperative' kernel: the systems-transition reading, which holds that
 *   decarbonization is inseparable from a governance transformation of energy
 *   provision toward decentralized, democratically-controlled infrastructure.
 *   On this reading, nuclear power — regardless of its carbon intensity — is
 *   structurally disqualified as a mitigation pathway because its capital
 *   structure, licensing regime, and technical operation require centralized,
 *   expert-controlled, capital-intensive institutions that reproduce the same
 *   extractive ownership patterns mitigation is meant to dismantle. This is
 *   NOT a claim about nuclear's carbon footprint or deployment speed (those
 *   live in the sibling readings, portfolio_optimization_reading and
 *   opportunity_cost_reading, as separate constraint stories with their own
 *   epsilon values). Here, ep is authored for the systems-transition
 *   reading's own account of the standing arrangement: a global energy system
 *   still substantially organized around centralized generation and utility
 *   monopoly, which this reading treats as itself extractive and in need of
 *   dismantling as part of mitigation. The three readings are siblings
 *   sharing a kernel, not three measurements of one constraint — each has a
 *   stable, non-averaged epsilon.
 *
 * KEY AGENTS:
 *   - distributed_renewable_developers: Primary beneficiary (organized/mobile) — captures market share and legitimacy from the governance framing
 *   - community_energy_cooperatives: Beneficiary (moderate/constrained) — gains policy support and financing preference under this reading
 *   - incumbent_nuclear_utilities: Primary target (institutional/constrained) — reclassified as extractive incumbents regardless of carbon performance
 *   - nuclear_engineering_workforce: Secondary target (moderate/trapped) — career and livelihood tied to a technology this reading structurally disfavors
 *   - grid_dependent_low_income_ratepayers_in_centralized_systems: Diffuse victim (powerless/trapped) — bears transition costs and reliability risk without seat at the framing table
 *   - energy_intensive_industrial_regions_reliant_on_baseload: Secondary victim (organized/constrained) — regional economies structured around large centralized plants
 *   - climate_policy_analysts: Analytical observer — sees the kernel-level contest among readings and its distributive consequences
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_mitigation_imperative__systems_transition_reading, 0.68).
domain_priors:suppression_score(climate_mitigation_imperative__systems_transition_reading, 0.58).
domain_priors:theater_ratio(climate_mitigation_imperative__systems_transition_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_mitigation_imperative__systems_transition_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(climate_mitigation_imperative__systems_transition_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(climate_mitigation_imperative__systems_transition_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(climate_mitigation_imperative__systems_transition_reading, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(climate_mitigation_imperative__systems_transition_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_mitigation_imperative__systems_transition_reading, tangled_rope).
narrative_ontology:human_readable(climate_mitigation_imperative__systems_transition_reading, "Climate Mitigation as Energy-System Democratization Imperative (Systems Transition Reading)").
narrative_ontology:topic_domain(climate_mitigation_imperative__systems_transition_reading, "energy_policy/climate_mitigation/technology_governance").

domain_priors:requires_active_enforcement(climate_mitigation_imperative__systems_transition_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_mitigation_imperative__systems_transition_reading, 'bc5bf0a3-dc84-4053-8cf0-de3a5294285d').
narrative_ontology:cs_kernel_codification('bc5bf0a3-dc84-4053-8cf0-de3a5294285d', distributed).
narrative_ontology:cs_authority_grounding('bc5bf0a3-dc84-4053-8cf0-de3a5294285d', distributed).
narrative_ontology:cs_reading_relation('bc5bf0a3-dc84-4053-8cf0-de3a5294285d', climate_mitigation_imperative__portfolio_optimization_reading, coexists_with).
narrative_ontology:cs_reading_relation('bc5bf0a3-dc84-4053-8cf0-de3a5294285d', climate_mitigation_imperative__opportunity_cost_reading, influences).
narrative_ontology:cs_axiom('bc5bf0a3-dc84-4053-8cf0-de3a5294285d', foundational, governance_structure_is_intrinsic_to_mitigation).
narrative_ontology:cs_axiom_status(governance_structure_is_intrinsic_to_mitigation, holdable).
narrative_ontology:cs_axiom_grounding('bc5bf0a3-dc84-4053-8cf0-de3a5294285d', governance_structure_is_intrinsic_to_mitigation, deontological).
narrative_ontology:cs_axiom('bc5bf0a3-dc84-4053-8cf0-de3a5294285d', secondary, centralization_causally_produces_extraction).
narrative_ontology:cs_axiom_status(centralization_causally_produces_extraction, holdable).
narrative_ontology:cs_axiom_grounding('bc5bf0a3-dc84-4053-8cf0-de3a5294285d', centralization_causally_produces_extraction, empirically_contingent).
narrative_ontology:cs_reference_frame('bc5bf0a3-dc84-4053-8cf0-de3a5294285d', predecarbonization_centralized_utility_monopoly).
narrative_ontology:cs_drift_state('bc5bf0a3-dc84-4053-8cf0-de3a5294285d', post_taxonomy_fight_era, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('bc5bf0a3-dc84-4053-8cf0-de3a5294285d', '').
narrative_ontology:cs_kernel_id(climate_mitigation_imperative__systems_transition_reading, climate_mitigation_imperative).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_mitigation_imperative__systems_transition_reading, distributed_renewable_developers).
narrative_ontology:constraint_beneficiary(climate_mitigation_imperative__systems_transition_reading, community_energy_cooperatives).
narrative_ontology:constraint_beneficiary(climate_mitigation_imperative__systems_transition_reading, grid_democratization_advocacy_organizations).
narrative_ontology:constraint_beneficiary(climate_mitigation_imperative__systems_transition_reading, local_municipal_utilities).
narrative_ontology:constraint_victim(climate_mitigation_imperative__systems_transition_reading, incumbent_nuclear_utilities).
narrative_ontology:constraint_victim(climate_mitigation_imperative__systems_transition_reading, nuclear_engineering_workforce).
narrative_ontology:constraint_victim(climate_mitigation_imperative__systems_transition_reading, grid_dependent_low_income_ratepayers_in_centralized_systems).
narrative_ontology:constraint_victim(climate_mitigation_imperative__systems_transition_reading, energy_intensive_industrial_regions_reliant_on_baseload).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Build and finance rooftop solar, community wind, and microgrid projects. Gain preferential access to green bonds, renewable portfolio standard credits, and cooperative financing structures that this reading's policy apparatus routes toward decentralized generation and away from nuclear. Can relocate capital across jurisdictions that adopt the framing most favorably.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__systems_transition_reading, distributed_renewable_developers, beneficiary,
    organized, generational, mobile, national).

% Member-owned entities generating and governing local power supply. Gain legitimacy, technical assistance, and policy priority under the democratization framing. Exit options are limited by the capital intensity of standing up cooperative infrastructure, but they are structurally favored rather than targeted by the constraint's operation.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__systems_transition_reading, community_energy_cooperatives, beneficiary,
    moderate, generational, constrained, local).

% Shape climate finance taxonomies, renewable portfolio standard design, and international climate negotiation positions to encode decentralization and governance-democratization as mitigation criteria. Draft model legislation excluding or disadvantaging nuclear in green finance classifications. Their institutional standing and funding grow as the framing gains adoption.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__systems_transition_reading, grid_democratization_advocacy_organizations, agenda_setter,
    organized, civilizational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(climate_mitigation_imperative__systems_transition_reading, grid_democratization_advocacy_organizations, beneficiary).

% Publicly-owned utilities that gain political and financial support for municipalization and distributed-generation buildout under the democratization framing, positioning them favorably against investor-owned centralized utility incumbents.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__systems_transition_reading, local_municipal_utilities, beneficiary,
    moderate, biographical, constrained, regional).

% Operate large centralized nuclear plants with multi-decade licensing and capital cycles. Under this reading, they are excluded from green taxonomies and mitigation finance mechanisms regardless of their carbon performance, because their governance structure is itself classified as the harm. Cannot pivot business models within the plant lifecycle; face stranded-asset risk if policy foreclosure hardens.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__systems_transition_reading, incumbent_nuclear_utilities, payer,
    institutional, generational, constrained, national).

% Highly specialized technical workforce with skills largely non-transferable to distributed renewable sectors on comparable terms. Careers and regional economies are exposed to the policy foreclosure this reading's advocacy produces, with limited say in how the mitigation mandate is defined.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__systems_transition_reading, nuclear_engineering_workforce, payer,
    moderate, biographical, trapped, national).

% Depend on existing centralized grid infrastructure for affordable, reliable power and have no practical ability to opt into cooperative or distributed alternatives due to housing tenure, income, or geography. Bear reliability risk and potential rate increases during a transition whose governance criteria they did not help set.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__systems_transition_reading, grid_dependent_low_income_ratepayers_in_centralized_systems, payer,
    powerless, immediate, trapped, regional).

% Regional economies (heavy manufacturing, aluminum smelting, industrial clusters) built around access to large-scale, high-capacity-factor centralized power. Face disruption if mitigation finance and policy withdraw support from the baseload sources their industrial base depends on, without a clear decentralized substitute at comparable reliability.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__systems_transition_reading, energy_intensive_industrial_regions_reliant_on_baseload, payer,
    organized, generational, constrained, regional).

% Study the kernel-level contest among mitigation readings, tracing how the systems-transition framing versus the portfolio-optimization and opportunity-cost framings produce materially different winners, losers, and finance flows despite all being offered as accounts of the same climate mandate.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__systems_transition_reading, climate_policy_analysts, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(climate_mitigation_imperative__systems_transition_reading, diffuse).
narrative_ontology:fixing_cost_class(climate_mitigation_imperative__systems_transition_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates capital, policy support, and technical assistance toward distributed, community-governed energy generation, solving a real problem: utility monopoly unaccountability and community exclusion from energy infrastructure decisions.
% TRANSFER_FUNCTION: Moves climate finance eligibility, regulatory priority, and political legitimacy away from centralized generation (especially nuclear) and toward distributed renewable and cooperative ownership structures, regardless of the comparative carbon performance of the excluded technology.
% ABSENT_VOICES: Nuclear engineering workforce and industrial baseload-dependent regions are rarely present in the advocacy and policy-design venues (renewable portfolio standard rulemakings, green taxonomy committees) where this reading's criteria are set; they surface mainly in litigation and legislative pushback after the fact, not in the framing conversation itself.
% DISAPPEARANCE_RATIONALE: Advocacy organizations and cooperative beneficiaries would say the world rearranges catastrophically toward re-entrenched utility monopoly power if the democratization criterion vanished from mitigation policy. Nuclear utilities and industrial baseload regions would say the world is already rearranged AGAINST them by this criterion's presence, and its disappearance would simply restore technology-neutral carbon accounting. The disagreement is genuinely about which arrangement is the baseline, which is itself evidence this is a live kernel contest rather than a settled fact.
% FOUNDING_PROBLEM: Utility monopolies and state nuclear complexes historically excluded communities from decisions about their own energy infrastructure, concentrating both economic rents and technical authority in centralized institutions; the systems-transition reading was built to ensure that decarbonization did not simply re-entrench that concentration under a green label.
% FOUNDING_PROBLEM_CORROBORATION: Grid democratization advocacy organizations and cooperative beneficiaries attest the founding problem (unaccountable centralized energy governance) remains live and worsening under corporate-financed renewable buildout. Independent energy-justice researchers outside the advocacy coalition partially corroborate this for investor-owned distributed generation specifically, but note that publicly-owned nuclear utilities in several jurisdictions (municipal and state-owned reactors) do not fit the extraction pattern the reading generalizes from — suggesting the founding problem may be real but the nuclear-specific victim assignment overgeneralizes from a subset of centralized-utility cases to the technology as a whole.
narrative_ontology:disappearance_verdict(climate_mitigation_imperative__systems_transition_reading, contested).
narrative_ontology:founding_problem_status(climate_mitigation_imperative__systems_transition_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(climate_mitigation_imperative__systems_transition_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(climate_mitigation_imperative__systems_transition_reading, 'none', 1).
narrative_ontology:epsilon_provenance(climate_mitigation_imperative__systems_transition_reading, 0.68, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(climate_mitigation_imperative__systems_transition_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(climate_mitigation_imperative__systems_transition_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(climate_mitigation_imperative__systems_transition_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68 at t=24) reflects the reading's own account: an energy system whose centralized, expert-gatekept architecture is asserted to systematically transfer control and rents from communities to utility monopolies and state nuclear complexes, worsening as the reading's advocacy coalition hardens its position against nuclear inclusion in mitigation finance. Suppression (0.58) is authored moderate-high because the reading's institutional foothold (renewable portfolio standards excluding nuclear, green bond taxonomies, cooperative financing preferences) increasingly forecloses nuclear as an eligible mitigation pathway in jurisdictions that adopt it, which is a real suppression of an alternative even if nuclear proponents contest the causal story. Resistance (0.72) is high because incumbent nuclear utilities, their workforce, and industrial baseload-dependent regions actively contest the framing in court, in legislatures, and in international climate finance bodies (e.g., taxonomy fights over classifying nuclear as 'green'). Accessibility_collapse is moderate (0.42), not high, because nuclear as an alternative pathway has NOT collapsed globally — it persists and is even expanding in some jurisdictions (France, China, emerging SMR programs) precisely because this reading has not achieved universal capture.
 *
 * PERSPECTIVAL GAP:
 *   From the distributed-renewable-developer and cooperative seat, this arrangement is a genuine coordination achievement: it enables communities to own and govern their own energy infrastructure, displacing a genuinely extractive centralized utility model. From the incumbent nuclear utility and workforce seat, the same arrangement is exclusionary extraction dressed in democratic language — a policy apparatus that denies capital and legitimacy to a low-carbon technology on governance-aesthetic grounds unrelated to its emissions performance, while transferring political and financial support to a competing capital fraction. The engine should compute these seats differently from the same structural data: the agenda-setting beneficiary coalition experiences coordination; the foreclosed incumbent experiences extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   Distributed renewable developers and community cooperatives are declared beneficiaries: they gain market access, financing preference, and legitimacy from the governance framing, and their exit options are mobile-to-organized (they can scale within the favored framework). Incumbent nuclear utilities and their workforce are declared victims: the constraint's operation directly forecloses their sector's eligibility for climate finance and policy support regardless of technical merit, and their exit options are constrained-to-trapped (sunk capital, licensing timelines, specialized labor with few adjacent sectors). Low-income ratepayers in still-centralized systems are a diffuse, powerless victim class bearing transition costs (stranded asset costs, reliability risk during transition) without having chosen the governance framing that assigns those costs to them.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — decarbonizing global energy systems fast enough to avoid catastrophic warming — remains unambiguously live; no one disputes that. The mandatrophy risk here is narrower and more subtle: has the GOVERNANCE-TRANSFORMATION component of this reading's mandate become an end pursued independent of its original justification (that decentralization reduces both emissions AND extraction)? If distributed generation financing increasingly resembles the extractive patterns (investor-owned, PPA-locked, land-lease-dependent) it was meant to displace, the reading's own coordination claim would be hollowing out even as its policy apparatus (renewable portfolio standards, green taxonomies excluding nuclear) hardens — a classic scaffold-to-tangled-rope drift where the stated transitional justification persists as active enforcement machinery after the substance it claimed to deliver has partially decoupled from the mechanism enforcing it. This is exactly why an omega on beneficiary capture is included above rather than assumed away.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_disambiguation,
    'Is the climate mitigation imperative best read as a governance-transformation mandate (this reading), a portfolio-optimization mandate treating all low-carbon sources as substitutable, or a cost-per-tonne-abated mandate indifferent to ownership structure?',
    'This is not empirically resolvable within one framework — it depends on whether one treats decarbonization speed/cost as the terminal value or treats the governance structure of energy provision as co-terminal with decarbonization. The three readings are authored as three separate constraint stories (portfolio_optimization_reading, opportunity_cost_reading, systems_transition_reading) linked via network.affects_constraints.',
    'Under this reading, nuclear is a victim-class incumbent whose centralization is itself the harm being mitigated, independent of its carbon intensity. Under the sibling readings, nuclear is either a necessary beneficiary (portfolio) or a harmful cost-inefficiency (opportunity cost) — but neither sibling treats governance structure as intrinsic to the mitigation target the way this reading does.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_disambiguation, conceptual, 'Which reading of the climate mitigation kernel is operative determines nuclear''s structural position entirely.').

omega_variable(
    decentralization_extraction_causal_link,
    'Does centralized energy infrastructure (nuclear, large hydro, transmission-dependent grids) causally produce extraction and democratic deficit, or is the correlation between centralization and extractive ownership contingent on regulatory choices independent of plant architecture?',
    'Comparative institutional analysis of publicly-owned centralized nuclear fleets (e.g., municipal or state-owned reactors) versus privately-owned distributed renewable portfolios with extractive financing (e.g., private equity-owned solar farms) would test whether extraction tracks ownership model or physical centralization.',
    'If extraction tracks ownership rather than plant architecture, this reading''s core axiom (centralization causes extraction) is significantly weakened and the constraint''s victim-class assignment of nuclear becomes a category error; if centralization is causally load-bearing, the reading''s structural claims hold.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(decentralization_extraction_causal_link, empirical, 'Whether physical grid centralization or ownership structure is the actual extraction mechanism.').

omega_variable(
    beneficiary_capture_of_democratization_frame,
    'Do distributed renewable developers and cooperative advocacy organizations benefit from this reading because it genuinely serves energy democracy, or because the democratization frame provides cover for a competing capital interest (distributed generation finance, land-lease solar developers) seeking market share against incumbent nuclear operators?',
    'Trace financing structures of the named beneficiary groups: are they genuinely community-owned cooperatives, or investor-owned distributed generation firms using democratic-control rhetoric while extracting from ratepayers via power purchase agreements?',
    'If beneficiaries are substantially investor-owned distributed generation firms rather than genuine cooperatives, this reading itself contains a false-summit dynamic — a governance claim providing cover for a different capital fraction''s extraction, which would reclassify parts of the beneficiary set as victims.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_capture_of_democratization_frame, empirical, 'Whether the named beneficiaries of the systems-transition reading are genuine democratic actors or capital interests using democratization rhetoric.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_mitigation_imperative__systems_transition_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clim_tr_t0, climate_mitigation_imperative__systems_transition_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(clim_tr_t4, climate_mitigation_imperative__systems_transition_reading, theater_ratio, 4, 0.22).
narrative_ontology:measurement(clim_tr_t8, climate_mitigation_imperative__systems_transition_reading, theater_ratio, 8, 0.24).
narrative_ontology:measurement(clim_tr_t12, climate_mitigation_imperative__systems_transition_reading, theater_ratio, 12, 0.26).
narrative_ontology:measurement(clim_tr_t16, climate_mitigation_imperative__systems_transition_reading, theater_ratio, 16, 0.27).
narrative_ontology:measurement(clim_tr_t20, climate_mitigation_imperative__systems_transition_reading, theater_ratio, 20, 0.29).
narrative_ontology:measurement(clim_tr_t24, climate_mitigation_imperative__systems_transition_reading, theater_ratio, 24, 0.3).

% Extraction over time
narrative_ontology:measurement(clim_be_t0, climate_mitigation_imperative__systems_transition_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(clim_be_t4, climate_mitigation_imperative__systems_transition_reading, base_extractiveness, 4, 0.48).
narrative_ontology:measurement(clim_be_t8, climate_mitigation_imperative__systems_transition_reading, base_extractiveness, 8, 0.53).
narrative_ontology:measurement(clim_be_t12, climate_mitigation_imperative__systems_transition_reading, base_extractiveness, 12, 0.58).
narrative_ontology:measurement(clim_be_t16, climate_mitigation_imperative__systems_transition_reading, base_extractiveness, 16, 0.62).
narrative_ontology:measurement(clim_be_t20, climate_mitigation_imperative__systems_transition_reading, base_extractiveness, 20, 0.65).
narrative_ontology:measurement(clim_be_t24, climate_mitigation_imperative__systems_transition_reading, base_extractiveness, 24, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(clim_su_t0, climate_mitigation_imperative__systems_transition_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(clim_su_t4, climate_mitigation_imperative__systems_transition_reading, suppression_requirement, 4, 0.4).
narrative_ontology:measurement(clim_su_t8, climate_mitigation_imperative__systems_transition_reading, suppression_requirement, 8, 0.45).
narrative_ontology:measurement(clim_su_t12, climate_mitigation_imperative__systems_transition_reading, suppression_requirement, 12, 0.49).
narrative_ontology:measurement(clim_su_t16, climate_mitigation_imperative__systems_transition_reading, suppression_requirement, 16, 0.53).
narrative_ontology:measurement(clim_su_t20, climate_mitigation_imperative__systems_transition_reading, suppression_requirement, 20, 0.56).
narrative_ontology:measurement(clim_su_t24, climate_mitigation_imperative__systems_transition_reading, suppression_requirement, 24, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_mitigation_imperative__systems_transition_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(climate_mitigation_imperative__systems_transition_reading, 0.12).
narrative_ontology:affects_constraint(climate_mitigation_imperative__systems_transition_reading, climate_mitigation_imperative__portfolio_optimization_reading).
narrative_ontology:affects_constraint(climate_mitigation_imperative__systems_transition_reading, climate_mitigation_imperative__opportunity_cost_reading).

% DUAL FORMULATION NOTE:
% This story is one of three sibling readings of the climate_mitigation_imperative kernel: systems_transition_reading (this file, tangled_rope, epsilon=0.68), portfolio_optimization_reading (nuclear as beneficiary/necessary baseload, likely rope or tangled_rope with lower epsilon), and opportunity_cost_reading (nuclear as cost-inefficiency, likely snare-leaning toward nuclear specifically, higher epsilon against nuclear but different victim set). The readings share the same kernel text (the abstract mitigation imperative) but diverge on whether governance structure, portfolio composition, or cost-per-tonne is the operative criterion — each reading assigns nuclear a different structural role (victim here, beneficiary in portfolio_optimization_reading, also-victim-but-differently in opportunity_cost_reading). They are linked, not merged, per the ep-invariance principle: forcing one epsilon across all three would erase the real structural disagreement the three readings represent.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(climate_mitigation_imperative__systems_transition_reading, institutional, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
