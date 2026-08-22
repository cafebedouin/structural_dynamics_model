% ============================================================================
% CONSTRAINT STORY: technology_legitimacy_kernel__reliability_primacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_technology_legitimacy_kernel__reliability_primacy_reading, []).

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
 *   constraint_id: technology_legitimacy_kernel__reliability_primacy_reading
 *   human_readable: Reliability-Primacy Technology Legitimacy Gate (Dispatchable Baseload Reading)
 *   domain: energy policy/climate mitigation/technology governance
 *
 * SUMMARY:
 *   This story instantiates one reading of a contested legitimacy kernel in
 *   climate-technology governance: the claim that a mitigation technology is
 *   legitimate if and only if it provides dispatchable, baseload-capable
 *   generation for grid stability. The standing arrangement under contest —
 *   and the sole referent of epsilon here — is the reliability-primacy gate
 *   as it actually operates: capacity-accreditation regimes,
 *   clean-energy-standard eligibility rules, and subsidy criteria that
 *   condition climate-policy standing on firm, dispatchable output. Under
 *   this reading nuclear power enters the beneficiary set outright, hydro
 *   qualifies cleanly, gas collects the largest share of reliability-linked
 *   revenue, storage developers gain a mandated market, variable-output
 *   renewables carry a qualification cost, and ratepayers absorb the
 *   premiums. The arrangement has a genuine coordination core (grids really
 *   do need adequacy and reserves) and an enforceable asymmetric incidence
 *   (the firms that already possess the qualifying property collect; those
 *   that do not pay to acquire it or are excluded). Per the
 *   epsilon-invariance principle, the sibling readings of the same kernel —
 *   velocity_primacy_reading and precautionary_reading — are separate
 *   constraint files with their own epsilon values and stakeholder sets;
 *   nothing about their classifications is averaged into this one. KEY AGENTS
 *   (by structural relationship): - resource_adequacy_regulators: Agenda
 *   setter (institutional/constrained) — writes accreditation and eligibility
 *   rules - nuclear_power_industry: Primary legitimacy beneficiary
 *   (institutional/identity_locked) — qualifies by definition -
 *   gas_generation_industry: Primary revenue beneficiary
 *   (institutional/constrained) — collects most reliability payments -
 *   hydropower_operators: Secondary beneficiary (powerful/constrained) -
 *   storage_technology_developers: Contingent beneficiary (organized/mobile)
 *   — sells the qualification - intermittent_renewable_developers: Primary
 *   payer among producers (organized/constrained) - electricity_ratepayers:
 *   Diffuse payer (powerless/trapped) — bears premiums with minimal voice -
 *   climate_velocity_advocates, precautionary_waste_critics: Excluded seats —
 *   hold sibling readings, absent from enforcement venues -
 *   energy_systems_modelers: Analytical observer — sees the full structure,
 *   bears nothing
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(technology_legitimacy_kernel__reliability_primacy_reading, 0.65).
domain_priors:suppression_score(technology_legitimacy_kernel__reliability_primacy_reading, 0.58).
domain_priors:theater_ratio(technology_legitimacy_kernel__reliability_primacy_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(technology_legitimacy_kernel__reliability_primacy_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(technology_legitimacy_kernel__reliability_primacy_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(technology_legitimacy_kernel__reliability_primacy_reading, theater_ratio, 0.35).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(technology_legitimacy_kernel__reliability_primacy_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(technology_legitimacy_kernel__reliability_primacy_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(technology_legitimacy_kernel__reliability_primacy_reading, tangled_rope).
narrative_ontology:human_readable(technology_legitimacy_kernel__reliability_primacy_reading, "Reliability-Primacy Technology Legitimacy Gate (Dispatchable Baseload Reading)").
narrative_ontology:topic_domain(technology_legitimacy_kernel__reliability_primacy_reading, "energy policy/climate mitigation/technology governance").

domain_priors:requires_active_enforcement(technology_legitimacy_kernel__reliability_primacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(technology_legitimacy_kernel__reliability_primacy_reading, 'f522f3ba-14a8-4bed-98ba-dd0bb8b32299').
narrative_ontology:cs_kernel_codification('f522f3ba-14a8-4bed-98ba-dd0bb8b32299', formalized).
narrative_ontology:cs_authority_grounding('f522f3ba-14a8-4bed-98ba-dd0bb8b32299', expertise).
narrative_ontology:cs_interpretation_layer_present('f522f3ba-14a8-4bed-98ba-dd0bb8b32299').
narrative_ontology:cs_reading_relation('f522f3ba-14a8-4bed-98ba-dd0bb8b32299', technology_legitimacy_kernel__velocity_primacy_reading, coexists_with).
narrative_ontology:cs_reading_relation('f522f3ba-14a8-4bed-98ba-dd0bb8b32299', technology_legitimacy_kernel__precautionary_reading, coexists_with).
narrative_ontology:cs_axiom('f522f3ba-14a8-4bed-98ba-dd0bb8b32299', foundational, grid_stability_requires_dispatchable_baseload).
narrative_ontology:cs_axiom_status(grid_stability_requires_dispatchable_baseload, holdable).
narrative_ontology:cs_axiom_grounding('f522f3ba-14a8-4bed-98ba-dd0bb8b32299', grid_stability_requires_dispatchable_baseload, empirically_contingent).
narrative_ontology:cs_axiom('f522f3ba-14a8-4bed-98ba-dd0bb8b32299', foundational, mitigation_legitimacy_tracks_grid_service_contribution).
narrative_ontology:cs_axiom_status(mitigation_legitimacy_tracks_grid_service_contribution, holdable).
narrative_ontology:cs_axiom_grounding('f522f3ba-14a8-4bed-98ba-dd0bb8b32299', mitigation_legitimacy_tracks_grid_service_contribution, instrumental).
narrative_ontology:cs_reference_frame('f522f3ba-14a8-4bed-98ba-dd0bb8b32299', dispatchable_baseload_grid_norm).
narrative_ontology:cs_drift_state('f522f3ba-14a8-4bed-98ba-dd0bb8b32299', contemporary_high_renewables_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('f522f3ba-14a8-4bed-98ba-dd0bb8b32299', '').
narrative_ontology:cs_kernel_id(technology_legitimacy_kernel__reliability_primacy_reading, technology_legitimacy_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(technology_legitimacy_kernel__reliability_primacy_reading, nuclear_power_industry).
narrative_ontology:constraint_beneficiary(technology_legitimacy_kernel__reliability_primacy_reading, gas_generation_industry).
narrative_ontology:constraint_beneficiary(technology_legitimacy_kernel__reliability_primacy_reading, hydropower_operators).
narrative_ontology:constraint_beneficiary(technology_legitimacy_kernel__reliability_primacy_reading, storage_technology_developers).
narrative_ontology:constraint_victim(technology_legitimacy_kernel__reliability_primacy_reading, intermittent_renewable_developers).
narrative_ontology:constraint_victim(technology_legitimacy_kernel__reliability_primacy_reading, electricity_ratepayers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(technology_legitimacy_kernel__reliability_primacy_reading, electricity_ratepayers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Operates and builds reactors whose output is dispatchable and runs at high capacity factors. Under this criterion its product qualifies as legitimate climate mitigation by definition, and the industry's public case — clean firm power — is built almost entirely on that qualification. Its trade associations, vendor pipelines, and workforce programs are organized around the reliability argument; if the qualifying property were reframed around deployment speed or waste boundedness, the industry's central claim would need rebuilding rather than adjusting.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__reliability_primacy_reading, nuclear_power_industry, beneficiary,
    institutional, generational, identity_locked, global).

% Owns the dispatchable thermal fleet that currently collects the majority of capacity-market and reserve payments worldwide. The criterion protects that revenue by making firm capacity a precondition of climate-policy standing. Its exposure sits on the fuel side — carbon limits could strand assets — so it hedges through CCS pilots and hydrogen-ready retrofits rather than exiting the arrangement.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__reliability_primacy_reading, gas_generation_industry, beneficiary,
    institutional, biographical, constrained, global).

% Runs reservoir fleets that deliver dispatchable energy and storage services where geography permits. Qualifies cleanly under the criterion and collects firm-capacity premiums in markets that adopt it. Assets are site-fixed; expansion is capped by remaining river sites and ecological licensing.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__reliability_primacy_reading, hydropower_operators, beneficiary,
    powerful, generational, constrained, regional).

% Sells the battery and long-duration systems that variable-output projects must procure to pass the criterion's dispatchability test. Every tightening of the qualification bar enlarges its addressable market. Its commercial position depends on the criterion persisting even as its own products erode the criterion's technical necessity.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__reliability_primacy_reading, storage_technology_developers, beneficiary,
    organized, biographical, mobile, global).

% Builds wind and solar whose output follows weather. To qualify as legitimate under the criterion they must buy storage or firming capacity, raising project costs, or accept exclusion from clean-energy procurements and reduced capacity accreditation. They cannot relocate out of the policy environments where the criterion governs; they contest accreditation rules continuously and invest in hybridization to satisfy the test.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__reliability_primacy_reading, intermittent_renewable_developers, payer,
    organized, biographical, constrained, global).

% Pay the bills that fund reliability premiums: capacity payments to firm plants, storage mandates passed through in rates, and the fixed costs of maintaining redundant dispatchable capacity. They also receive the reliability those payments purchase, though they experience the arrangement chiefly as line items. They cannot leave the grid, and they hold few seats in the resource-adequacy dockets where the criteria are set.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__reliability_primacy_reading, electricity_ratepayers, payer,
    powerless, immediate, trapped, national).
narrative_ontology:stakeholder_secondary_role(technology_legitimacy_kernel__reliability_primacy_reading, electricity_ratepayers, beneficiary).

% System operators, utility commissions, and energy ministries that define capacity accreditation, resource-adequacy margins, and clean-energy-standard eligibility. They operationalize the criterion in tariff schedules and procurement rules. They answer politically for blackouts in a way they never answer for slow decarbonization, and their discretion is bounded by statute and by the engineering staffs they depend on.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__reliability_primacy_reading, resource_adequacy_regulators, agenda_setter,
    institutional, generational, constrained, continental).

% Climate scientists and campaign organizations holding that legitimacy should track deployment speed against the remaining carbon budget. They publish feasibility studies of high-renewables grids and lobby legislatures, but they rarely hold seats in the resource-adequacy and accreditation proceedings where the reliability criterion is actually enforced.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__reliability_primacy_reading, climate_velocity_advocates, excluded,
    organized, biographical, constrained, global).

% Environmental justice organizations and anti-nuclear campaigns holding that legitimacy should track bounded, reversible worst-case failures and legacy costs within a generation. They contest reactor relicensing and waste siting, but the venues where the reliability criterion operates — capacity markets, adequacy dockets, procurement rules — are not forums where their evidence enters.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__reliability_primacy_reading, precautionary_waste_critics, excluded,
    organized, generational, constrained, regional).

% Academic and consulting teams running capacity-expansion and production-cost simulations of future grids under varying technology mixes. Their results feed every faction's argument; they hold no procurement authority and bear none of the arrangement's costs.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__reliability_primacy_reading, energy_systems_modelers, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(technology_legitimacy_kernel__reliability_primacy_reading, gas_generation_industry).
narrative_ontology:fixing_cost_class(technology_legitimacy_kernel__reliability_primacy_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Electricity systems must balance supply and demand continuously at stable frequency through plant outages and weather extremes. Resource-adequacy planning, capacity accreditation, and reserve requirements solve that physical coordination problem once, centrally, instead of leaving each consumer to hedge blackout risk alone.
% TRANSFER_FUNCTION: Moves policy legitimacy, subsidy eligibility, capacity-market revenue, and procurement preference toward dispatchable, firm-generation technologies; moves qualification costs (storage and firming purchases) onto variable-output developers and reliability premiums onto ratepayers.
% ABSENT_VOICES: Holders of the velocity reading — climate scientists oriented to the carbon-budget timeline — and holders of the precautionary reading — communities bearing legacy-waste and failure-mode risks — are structurally absent from the accreditation and adequacy proceedings where the criterion is enforced. Ratepayer advocates appear but are thinly resourced against utility and generator intervenors.
% DISAPPEARANCE_RATIONALE: If the criterion vanished overnight, capacity-accreditation rules, clean-energy-standard definitions, and subsidy eligibility would reorganize around whichever legitimacy test each jurisdiction adopted next; nuclear's policy standing would shift sharply, gas capacity revenues would be renegotiated, and variable-renewable procurement would expand where speed became the qualifying property.
% FOUNDING_PROBLEM: Early decarbonization policy needed a defensible way to decide which technologies deserved public support, and grid institutions facing proposals for high-renewables systems answered with the oldest test they had: a technology counts if it can be relied upon to keep the lights on. The criterion fused professional reliability practice to climate-mitigation legitimacy.
% FOUNDING_PROBLEM_CORROBORATION: Independent reliability assessments (NERC Long-Term Reliability Assessments, ENTSO-E adequacy reviews) and blackout post-mortems corroborate, from outside the benefiting parties, that resource adequacy is a live and recurring problem. No external party attests that the iff-legitimacy formulation is the correct response to it — velocity-oriented analysts explicitly dispute that framing — so corroboration covers the founding problem, not the criterion built on it.
narrative_ontology:disappearance_verdict(technology_legitimacy_kernel__reliability_primacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(technology_legitimacy_kernel__reliability_primacy_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(technology_legitimacy_kernel__reliability_primacy_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(technology_legitimacy_kernel__reliability_primacy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(technology_legitimacy_kernel__reliability_primacy_reading, 0.65, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(technology_legitimacy_kernel__reliability_primacy_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(technology_legitimacy_kernel__reliability_primacy_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(technology_legitimacy_kernel__reliability_primacy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is 0.65 because the gate converts a real engineering requirement into a distributive instrument: capacity payments, subsidy eligibility, and procurement access flow disproportionately to incumbents of the qualifying property, while qualification costs (storage mandates, firming purchases) and reliability premiums land on variable-output developers and captive ratepayers. Suppression is 0.58 and is a raw structural property, unscaled by power or scope: enforcement operates through accreditation downgrades and procurement exclusion inside governed venues rather than through blanket prohibition — wind and solar still deploy, but on worse terms wherever the criterion binds. Theater_ratio is 0.35: adequacy planning and reserve engineering are functional to the core, but a growing share of reliability rhetoric in policy debate exceeds its engineering content, invoked where the operative question is subsidy allocation. Accessibility_collapse is 0.48: within an adopting institution the alternatives collapse almost completely once the criterion is accepted (non-qualifying technologies are ineligible by definition), yet the wider policy space retains live alternatives — several jurisdictions reject the criterion outright and renewables deploy regardless. Resistance is 0.62: sustained contest from renewables industries, velocity-oriented scientists, and legacy-cost campaigners. The three measurement series run on one shared time grid (t=0,4,8,12,16,20) so every metric is authored at every examined point; the trajectories show extraction accumulating and enforcement hardening together as accreditation reform tightened capacity credit for variable resources. Claimed type and metrics are authored independently: tangled_rope is claimed from the structure (genuine coordination function plus enforced asymmetric extraction), not tuned to any predicted verdict.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently from the same text. From the agenda-setter seat the criterion is prudent engineering: regulators answer politically for blackouts, never for slow decarbonization, so reliability-first selection is the defensible default. From the nuclear seat it is overdue recognition of a property its product uniquely possesses. From the ratepayer seat it is an opaque set of bill lines funding capacity it cannot inspect. From the excluded velocity seat it is a timeline hazard dressed as engineering prudence. Nothing in the arrangement contradicts any of these experiences simultaneously — the divergence is structural, driven by position, exit, and horizon, and the engine computes it from the authored data rather than from this claim.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries derive low directionality: nuclear (identity_locked — its self-concept is constituted by the qualifying property), gas (collects the largest payment stream), hydro (site-fixed premium collector), and storage developers (whose market exists because the gate exists). Victims derive high directionality: ratepayers are trapped with no arbitrage exit and bear diffuse premiums, and variable-output developers are constrained — they can deploy but only by paying the qualification cost or accepting degraded accreditation. The regulator seat administers rather than collects, sitting nearer the middle with enforcement obligations on one side and incumbent pressure on the other. Storage developers occupy an unstable position worth flagging: the derivation reads them as beneficiaries, correctly for current cash flows, but their product erodes the criterion's necessity — the omega on storage-cost dissolution tracks this instability.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — keeping lights on through outages and extremes — remains live, so this is not a mandatrophy case and the classification guards against mislabeling in both directions. A pure-extraction reading would erase the genuine coordination core: adequacy shortfalls kill people, and firm capacity provides a real service no accounting trick removes. A pure-coordination reading would erase the asymmetric incidence: the same rules that purchase reliability channel legitimacy and revenue to whoever already holds the qualifying property, and impose acquisition costs on those who do not. Holding both facts simultaneously is what the tangled_rope claim asserts; the temporal series (rising extraction alongside hardening enforcement) shows the extraction share growing within a still-functional arrangement rather than a mandate that has died.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_underdetermination,
    'This constraint is one reading of the technology_legitimacy_kernel; would instantiating a sibling reading (velocity_primacy_reading or precautionary_reading) restructure the beneficiary and victim sets so thoroughly that the classification itself changes?',
    'Author and compile the sibling stories and compare computed per-seat classifications across the family; the delta in beneficiary/victim composition and effective extraction is the answer.',
    'Under the velocity reading nuclear migrates toward the paying side (build-time disqualification) and storage developers rise; under the precautionary reading nuclear pays through legacy-waste liability. The reliability reading''s specific shape — nuclear beneficiary, ratepayer victim — holds only within this reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_underdetermination, conceptual, 'Committer structure: which reading of the legitimacy kernel this story instantiates and what siblings would change.').

omega_variable(
    baseload_necessity_contest,
    'Does modern grid physics actually require baseload-class plants, or do flexibility, storage, transmission, and demand response suffice to maintain stability at high variable-renewable penetration?',
    'Operating record of high-penetration systems (South Australia, Denmark, Iberia, ERCO-T style interconnections) combined with production-cost modeling of credible 2035 mixes; watch for unserved-energy events attributable to insufficient firm capacity versus to market or transmission failures.',
    'If flexibility-plus-storage suffices, the criterion''s coordination core shrinks and its extraction share grows — pushing the arrangement toward pure extraction; if firm capacity remains physically necessary, part of the measured extraction is the price of the coordination itself.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(baseload_necessity_contest, empirical, 'Whether the vindicated baseload doctrine is empirically true or an incumbent-preserving overstatement.').

omega_variable(
    reliability_cost_attribution,
    'Are the reliability premiums borne by ratepayers attributable to the legitimacy criterion as operated, or to underlying physical reliability needs that any governance regime would impose?',
    'Counterfactual costing across jurisdictions with differing accreditation regimes, holding reliability outcomes constant; decompose capacity-payment differences into service-value and gate-rent components.',
    'If most of the premium is gate rent, the victim set deepens and effective extraction rises; if most is service value, the arrangement is closer to priced coordination than the scalar suggests.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reliability_cost_attribution, empirical, 'Attribution of ratepayer costs between genuine reliability service and criterion-induced rent.').

omega_variable(
    storage_cost_dissolution,
    'Will falling storage costs make the criterion''s qualification bar cheap enough to satisfy that the gate stops binding — dissolving its extractive force without formal repeal?',
    'Track storage cost curves against accreditation and firming requirements per megawatt of variable capacity; identify the price threshold at which qualification ceases to be a material project cost.',
    'If dissolution occurs, the arrangement decays from enforced gate toward harmless formality — a drift the temporal series should eventually register as falling suppression and rising theater; if accreditation bars ratchet upward in step with storage costs, the gate is self-hardening instead.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(storage_cost_dissolution, empirical, 'Whether technological change erodes the criterion''s bite faster than enforcement can rebuild it.').

omega_variable(
    authority_framing_underdetermination,
    'Is the authority behind this criterion best framed as engineering expertise applying a technical standard, or as an incumbent-defense instrument layered above a technical claim — with the engineering content doing justificatory rather than causal work?',
    'Venue and authorship analysis of accreditation rule changes: who drafts them, whose cost models they cite, and whether the qualifying bar moves with measured reliability outcomes or with incumbent portfolio composition.',
    'Under the expertise framing the arrangement is a credentialed body interpreting a real standard; under the incumbent-defense framing the authority structure extracts benefit from preventing revision of the kernel, and the classification shifts toward extraction-grounded commitment-system dynamics.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(authority_framing_underdetermination, conceptual, 'Two coherent framings of the same authority structure yield different commitment-system classifications.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(technology_legitimacy_kernel__reliability_primacy_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tech_tr_t0, technology_legitimacy_kernel__reliability_primacy_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement(tech_tr_t4, technology_legitimacy_kernel__reliability_primacy_reading, theater_ratio, 4, 0.25).
narrative_ontology:measurement(tech_tr_t8, technology_legitimacy_kernel__reliability_primacy_reading, theater_ratio, 8, 0.28).
narrative_ontology:measurement(tech_tr_t12, technology_legitimacy_kernel__reliability_primacy_reading, theater_ratio, 12, 0.31).
narrative_ontology:measurement(tech_tr_t16, technology_legitimacy_kernel__reliability_primacy_reading, theater_ratio, 16, 0.33).
narrative_ontology:measurement(tech_tr_t20, technology_legitimacy_kernel__reliability_primacy_reading, theater_ratio, 20, 0.35).

% Extraction over time
narrative_ontology:measurement(tech_be_t0, technology_legitimacy_kernel__reliability_primacy_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(tech_be_t4, technology_legitimacy_kernel__reliability_primacy_reading, base_extractiveness, 4, 0.52).
narrative_ontology:measurement(tech_be_t8, technology_legitimacy_kernel__reliability_primacy_reading, base_extractiveness, 8, 0.56).
narrative_ontology:measurement(tech_be_t12, technology_legitimacy_kernel__reliability_primacy_reading, base_extractiveness, 12, 0.6).
narrative_ontology:measurement(tech_be_t16, technology_legitimacy_kernel__reliability_primacy_reading, base_extractiveness, 16, 0.63).
narrative_ontology:measurement(tech_be_t20, technology_legitimacy_kernel__reliability_primacy_reading, base_extractiveness, 20, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(tech_su_t0, technology_legitimacy_kernel__reliability_primacy_reading, suppression_requirement, 0, 0.44).
narrative_ontology:measurement(tech_su_t4, technology_legitimacy_kernel__reliability_primacy_reading, suppression_requirement, 4, 0.47).
narrative_ontology:measurement(tech_su_t8, technology_legitimacy_kernel__reliability_primacy_reading, suppression_requirement, 8, 0.5).
narrative_ontology:measurement(tech_su_t12, technology_legitimacy_kernel__reliability_primacy_reading, suppression_requirement, 12, 0.53).
narrative_ontology:measurement(tech_su_t16, technology_legitimacy_kernel__reliability_primacy_reading, suppression_requirement, 16, 0.56).
narrative_ontology:measurement(tech_su_t20, technology_legitimacy_kernel__reliability_primacy_reading, suppression_requirement, 20, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(technology_legitimacy_kernel__reliability_primacy_reading, resource_allocation).
narrative_ontology:affects_constraint(technology_legitimacy_kernel__reliability_primacy_reading, velocity_primacy_reading).
narrative_ontology:affects_constraint(technology_legitimacy_kernel__reliability_primacy_reading, precautionary_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'legitimate climate technology' decomposes, per the epsilon-invariance principle, into three structurally distinct constraint stories — one per reading of the technology_legitimacy_kernel. This file carries the reliability-primacy reading (epsilon 0.65; nuclear and the firm fleet in the beneficiary set, ratepayers and variable-output developers paying). velocity_primacy_reading and precautionary_reading carry their own epsilon values and inverted or shifted stakeholder sets. They are linked as a constraint family because each reading's proponents cite the others' blind spots as evidence for their own criterion: reliability advocates point to blackout risk under velocity-run systems; velocity advocates point to build-time incompatibility with the carbon budget; precautionary advocates point to legacy costs both others ignore. The reliability reading is upstream in institutional lineage — adequacy planning predates climate policy — which gives it structural influence over the operating environment of the newer readings without logically eliminating either.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
