% ============================================================================
% CONSTRAINT STORY: acceptable_risk_energy__option_value_preserving
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_acceptable_risk_energy__option_value_preserving, []).

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
    narrative_ontology:measurement_basis/2,
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
 *   constraint_id: acceptable_risk_energy__option_value_preserving
 *   human_readable: Option-Value-Preserving Acceptable-Risk Regime (Multi-Pathway Energy Maintenance)
 *   domain: economic/political/decision-theoretic
 *
 * SUMMARY:
 *   A policy regime holds more than one electricity-generation pathway
 *   (notably nuclear and fossil) deliberately viable against deep uncertainty
 *   about climate response, technology cost, and long-run demand, on the
 *   argument that premature, irreversible closure of any pathway destroys
 *   decision flexibility worth more than the carrying cost. The regime is
 *   administered through licensing, subsidy, and grid-adequacy machinery that
 *   must actively block both the anti-nuclear closure agenda and the rapid
 *   fossil-phaseout agenda. KEY AGENTS (by structural relationship): -
 *   energy_regulators: Agenda setter (institutional/constrained) -
 *   administers the retention machinery and collects discretion. -
 *   fossil_fuel_producers: Primary beneficiary (powerful/arbitrage) - delayed
 *   exit preserves revenue and avoids stranding. - nuclear_operators:
 *   Beneficiary (institutional/constrained) - license continuity and avoided
 *   write-downs, though sometimes compelled to keep operating. -
 *   climate_vulnerable_populations: Primary target (powerless/trapped) - bear
 *   the emissions externality of the maintained fossil leg. -
 *   host_communities_for_nuclear_waste: Target (moderate/trapped) - carry
 *   indefinite stewardship burdens. - ratepayers: Target with incidental
 *   benefit (moderate/constrained) - fund redundancy, receive reliability. -
 *   anti_nuclear_closure_movements and rapid_phaseout_coalitions: Suppressed
 *   extremes (organized/mobile). - future_generations: Beneficiary
 *   (powerless/trapped, civilizational horizon) - inherit the preserved
 *   option set. - integrated_assessment_modelers: Analytical observer - sees
 *   the full structure. This file instantiates ONE reading of the
 *   acceptable_risk_energy kernel; the sibling readings are separate
 *   constraints, not alternatives folded into this one. The claim/metric gap
 *   is deliberate: the reading CLAIMS tangled_rope (genuine coordination plus
 *   real asymmetric cost) while the metrics are authored independently as
 *   descriptively true; the engine computes per-seat classifications from the
 *   structural data.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(acceptable_risk_energy__option_value_preserving, 0.52).
domain_priors:suppression_score(acceptable_risk_energy__option_value_preserving, 0.45).
domain_priors:theater_ratio(acceptable_risk_energy__option_value_preserving, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(acceptable_risk_energy__option_value_preserving, extractiveness, 0.52).
narrative_ontology:constraint_metric(acceptable_risk_energy__option_value_preserving, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(acceptable_risk_energy__option_value_preserving, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(acceptable_risk_energy__option_value_preserving, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(acceptable_risk_energy__option_value_preserving, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(acceptable_risk_energy__option_value_preserving, tangled_rope).
narrative_ontology:human_readable(acceptable_risk_energy__option_value_preserving, "Option-Value-Preserving Acceptable-Risk Regime (Multi-Pathway Energy Maintenance)").
narrative_ontology:topic_domain(acceptable_risk_energy__option_value_preserving, "economic/political/decision-theoretic").

domain_priors:requires_active_enforcement(acceptable_risk_energy__option_value_preserving).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(acceptable_risk_energy__option_value_preserving, '806db380-5889-4cce-bc69-d6a7500710e7').
narrative_ontology:cs_kernel_codification('806db380-5889-4cce-bc69-d6a7500710e7', distributed).
narrative_ontology:cs_authority_grounding('806db380-5889-4cce-bc69-d6a7500710e7', distributed).
narrative_ontology:cs_reading_relation('806db380-5889-4cce-bc69-d6a7500710e7', acceptable_risk_energy__catastrophic_tail_dominant, coexists_with).
narrative_ontology:cs_reading_relation('806db380-5889-4cce-bc69-d6a7500710e7', acceptable_risk_energy__expected_value_dominant, influences).
narrative_ontology:cs_axiom('806db380-5889-4cce-bc69-d6a7500710e7', foundational, deep_uncertainty_defeats_single_metric_optimization).
narrative_ontology:cs_axiom_status(deep_uncertainty_defeats_single_metric_optimization, holdable).
narrative_ontology:cs_axiom_grounding('806db380-5889-4cce-bc69-d6a7500710e7', deep_uncertainty_defeats_single_metric_optimization, empirically_contingent).
narrative_ontology:cs_axiom('806db380-5889-4cce-bc69-d6a7500710e7', secondary, pathway_premature_closure_destroys_irreversible_option_value).
narrative_ontology:cs_axiom_status(pathway_premature_closure_destroys_irreversible_option_value, holdable).
narrative_ontology:cs_axiom_grounding('806db380-5889-4cce-bc69-d6a7500710e7', pathway_premature_closure_destroys_irreversible_option_value, instrumental).
narrative_ontology:cs_reference_frame('806db380-5889-4cce-bc69-d6a7500710e7', deliberate_multi_pathway_option_portfolio).
narrative_ontology:cs_drift_state('806db380-5889-4cce-bc69-d6a7500710e7', contemporary_energy_security_crisis_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('806db380-5889-4cce-bc69-d6a7500710e7', '').
narrative_ontology:cs_kernel_id(acceptable_risk_energy__option_value_preserving, acceptable_risk_energy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(acceptable_risk_energy__option_value_preserving, nuclear_operators).
narrative_ontology:constraint_beneficiary(acceptable_risk_energy__option_value_preserving, fossil_fuel_producers).
narrative_ontology:constraint_beneficiary(acceptable_risk_energy__option_value_preserving, future_generations).
narrative_ontology:constraint_victim(acceptable_risk_energy__option_value_preserving, climate_vulnerable_populations).
narrative_ontology:constraint_victim(acceptable_risk_energy__option_value_preserving, host_communities_for_nuclear_waste).
narrative_ontology:constraint_victim(acceptable_risk_energy__option_value_preserving, ratepayers).
narrative_ontology:constraint_victim(acceptable_risk_energy__option_value_preserving, anti_nuclear_closure_movements).
narrative_ontology:constraint_victim(acceptable_risk_energy__option_value_preserving, rapid_phaseout_coalitions).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(acceptable_risk_energy__option_value_preserving, ratepayers).
narrative_ontology:constraint_vindicates(acceptable_risk_energy__option_value_preserving, real_options_valuation).
narrative_ontology:constraint_vindicates(acceptable_risk_energy__option_value_preserving, robust_decision_making).
narrative_ontology:constraint_vindicates(acceptable_risk_energy__option_value_preserving, deep_uncertainty_portfolio_management).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Set and administer the licensing, subsidy, and decommissioning rules that determine which generation pathways may close and which must be kept available. Justify each retention decision against scenario portfolios rather than point forecasts. Their own payoff is procedural: retained discretion is valuable to them, and each irreversible closure removes it. Exit for them means changing mandate, not leaving the system.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__option_value_preserving, energy_regulators, agenda_setter,
    institutional, generational, constrained, national).

% Operate reactor fleets whose economics depend on license renewals, life extensions, and occasional rescue from scheduled shutdowns. Some units they would retire on commercial grounds continue operating under policy direction. What flows to them is capital-asset protection and workforce continuity; what flows from them is continued waste generation and accident exposure they do not fully internalize. Exit would mean writing down plants and dispersing trained staff.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__option_value_preserving, nuclear_operators, beneficiary,
    institutional, biographical, constrained, national).

% Sell fuel into a generation fleet whose replacement schedule the policy regime repeatedly extends. Every additional year of maintained viability converts directly into revenue and delays stranding of reserves and plant. Their capital is internationally mobile, so adverse policy in one jurisdiction can be arbitraged by shifting investment elsewhere. They fund advocacy for reliability and prudence framings of retention.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__option_value_preserving, fossil_fuel_producers, beneficiary,
    powerful, biographical, arbitrage, global).

% Live with flood, heat, and storm exposure amplified by each additional ton emitted by the maintained fossil fleet. They did not choose the portfolio that emits on their behalf and have no practical exit from the atmosphere. Their costs arrive on decadal horizons while the decisions protecting the emitting capacity are reviewed on annual ones.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__option_value_preserving, climate_vulnerable_populations, payer,
    powerless, generational, trapped, global).

% Host interim storage and candidate repository sites, carrying stewardship obligations that outlast the operating careers of everyone who decided the siting. Consent processes reach them after pathway-level commitments are effectively fixed. Individuals can move away, but the burden stays with the site and new communities inherit it.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__option_value_preserving, host_communities_for_nuclear_waste, payer,
    moderate, generational, trapped, local).

% Pay tariffs and taxes that fund standby capacity, strategic reserves, and subsidy programs keeping less-economic plants available. They receive reliability and price smoothing in return, and they also absorb the bill shocks that follow fuel-price swings in the retained fossil leg. Individual exit through rooftop generation and efficiency is possible but partial and self-selected.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__option_value_preserving, ratepayers, payer,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(acceptable_risk_energy__option_value_preserving, ratepayers, beneficiary).

% Campaign for dated closure schedules, license denials, and non-renewal of operating permits. The policy regime answers them with review procedures, safety-case requirements, and grid-adequacy findings that postpone or reverse their wins. They can redirect effort between jurisdictions and campaigns, which keeps them in the fight but rarely lets them finish it.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__option_value_preserving, anti_nuclear_closure_movements, payer,
    organized, biographical, mobile, continental).

% Press for emergency-pace elimination of fossil combustion on climate-integrity grounds. The regime converts their demands into targets, consultations, and offset mechanisms that stretch timelines. They are mobile across venues, and their partial wins, such as coal closures on some grids, are absorbed as portfolio adjustments rather than precedents.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__option_value_preserving, rapid_phaseout_coalitions, payer,
    organized, biographical, mobile, global).

% Inherit whatever option set survives today's decisions: working plants or stranded ruins, spent fuel in managed storage or abandoned sites, and a climate shaped by the emissions the portfolio released. They hold the largest stake in preserved flexibility and the smallest ability to influence it; academies, commissioners, and ombudspersons speak in their name as proxies.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__option_value_preserving, future_generations, beneficiary,
    powerless, civilizational, trapped, global).

% Build the scenario ensembles and option-value models the regime cites. They observe the full structure: which pathways are retained, at what measured cost, and how closely retention behavior tracks the declared flexibility rationales. Their stake is reputational and methodological rather than material.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__option_value_preserving, integrated_assessment_modelers, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(acceptable_risk_energy__option_value_preserving, fossil_fuel_producers).
narrative_ontology:fixing_cost_class(acceptable_risk_energy__option_value_preserving, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Preserves system-level option value that no individual actor can capture alone: maintaining licensed plants, skilled workforces, fuel-cycle segments, and supply chains across more than one generation pathway, so the system can shift as evidence about climate response, technology cost, and demand arrives. Without coordinated retention, each operator rationally strands its own capacity and the portfolio collapses to the cheapest present pathway.
% TRANSFER_FUNCTION: Moves regulatory protection, subsidy, and continued license life from the present polity's budget and attention to incumbent pathway operators; moves continued emission rights to fossil generators at the expense of climate-exposed populations; moves policy discretion to administrators at the expense of closure and phaseout advocates whose preferred endpoints are postponed indefinitely.
% ABSENT_VOICES: Climate-exposed populations outside the deciding jurisdiction bear emissions costs with no seat in the licensing proceedings of the emitting polity. Future generations hold the largest interest in preserved options and none in the room except through proxy institutions. Waste-siting candidates enter consultation after route-level commitments are fixed. Each absence flatters the unanimity of the retention consensus.
% DISAPPEARANCE_RATIONALE: Overnight removal would unleash both suppressed extremes simultaneously: closure mandates and emergency phaseout deadlines would propagate through legislatures already holding drafted bills, uneconomic capacity would strand within one review cycle, and the portfolio would collapse toward whichever coalition moved first. The option set this regime exists to hold open would not survive the transition.
% FOUNDING_PROBLEM: After the 1970s oil shocks demonstrated that energy systems optimized to a single pathway fail catastrophically when that pathway's assumptions break, and as climate science made fossil dependence look similarly brittle on longer horizons, policymakers faced investments whose consequences unfold over fifty-plus years under parameters no one could confidently forecast. The arrangement was built to stop any single generation from irrevocably committing the system to one answer.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated outside the benefiting parties: national academy reviews and the peer-reviewed integrated-assessment literature independently document persistent deep spread in climate sensitivity, technology-cost, and demand projections; scenario exercises run by research consortia with no revenue at stake in any pathway reproduce the same parameter spread. No attesting source collects from pathway maintenance.
narrative_ontology:disappearance_verdict(acceptable_risk_energy__option_value_preserving, world_rearranges).
narrative_ontology:founding_problem_status(acceptable_risk_energy__option_value_preserving, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(acceptable_risk_energy__option_value_preserving, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(acceptable_risk_energy__option_value_preserving, 'none', 1).
narrative_ontology:epsilon_provenance(acceptable_risk_energy__option_value_preserving, 0.52, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(acceptable_risk_energy__option_value_preserving_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(acceptable_risk_energy__option_value_preserving, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(acceptable_risk_energy__option_value_preserving_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.52 at interval end): real transfers occur - the fossil leg's climate externality lands on populations with no exit, redundancy costs land on ratepayers, and both advocacy extremes bear postponed endpoints - but a substantial share of the carrying cost functions as insurance premium under uncertainty the corroborating literature confirms is deep. Suppression (0.45) is a raw structural property, unscaled by power or scope: the regime blocks closure and phaseout mandates procedurally (review cycles, safety cases, adequacy findings) rather than coercively, and it suppresses BOTH extremes symmetrically in kind though not in consequence. Theater (0.25) is low-moderate: diversification is substantially real, but a growing share of flexibility language covers default continuation, especially on the fossil leg. Accessibility_collapse (0.35) is low because alternatives demonstrably persist - single-pathway policies have been enacted in real jurisdictions - so the regime taxes and delays alternatives rather than erasing them. Resistance (0.6) is high because both suppressed extremes litigate, campaign, and win locally. The measurement series run on one shared time grid (all three metrics at all eleven points). The series show a CYCLICAL pattern: calm periods let incumbent retention entrench and extraction accumulate; shocks (fuel crises, major accidents - visible as the suppression_requirement spike at t=18, the post-accident surge of closure pressure the machinery had to work hardest to resist) briefly expose option value and re-legitimate the portfolio, after which enforcement effort partially decays but settles above the prior baseline. The oscillation is partly functional (shocks genuinely reveal option value) and partly extraction-enabling (each calm period lets the fossil leg's retention drift from option to default). A latent coalition exists that the regime's structure discourages: the two suppressed extremes could jointly attack incumbent subsidies from opposite directions, but each fears the other's endpoint more than the regime's delay.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats and the agenda-setter seat should compute differently. From the regulator's position the arrangement is responsible hedging it personally benefits from in discretion; from the climate-vulnerable and waste-hosting seats it is imposed continuation with costs they never chose and cannot exit; from the producers' position it is prudence that happens to pay them; from the suppressed movements' position it is a procedural maze that converts their majorities into postponements. The engine computes this divergence from the structural data - the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Fossil producers sit nearest the beneficiary end: they receive the transfer directly and their arbitrage-grade exit amplifies the subsidy side of the computation. Climate-vulnerable populations sit nearest the target end: trapped, no exit from the atmosphere, costs imposed without consent. Waste-hosting communities are high-d targets: localized, multigenerational, trapped to the site. Ratepayers sit mid-high: constrained, paying diffuse redundancy costs against an incidental reliability benefit (hence the secondary beneficiary role). The two advocacy movements are targets of the regime's suppression; their mobility dampens their effective-target position relative to trapped seats but does not move them near symmetry. Future generations derive near the beneficiary end on the option-inheritance flow, with a flagged tension: they also inherit the emissions damage, and this reading prices their option inheritance above their climate inheritance - that pricing choice is exactly what the sibling readings contest (see omega opportunity_cost_victim_attribution and the kernel omega). Regulators sit near symmetric: they collect discretion and bear accountability exposure. No directionality_overrides are authored: the beneficiary/victim declarations plus exit options capture every seat's relationship, and the one subtle case (nuclear operators' ambivalence - protected but sometimes compelled) is carried by their constrained exit and mixed flows rather than by a blunt power-atom override that would misfire across the story's other institutional seats.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification guards against two opposite mislabels. Reading the regime as pure extraction (snare) would see only the fossil delay's rents and miss the genuine coordination function: no single operator or consumer can preserve system-level optionality alone, and the corroborating uncertainty literature is independent of the benefiting parties. Reading it as pure coordination (rope) would see only the prudence framing and miss the asymmetric imposition: the climate externality and the waste burden fall on seats that did not agree to them, and the suppressed extremes pay through the same structure that coordinates the rest. Tangled rope holds both halves. On mandatrophy proper: the founding problem (irreversible commitment under deep uncertainty) is still live, so no resolved mandatrophy is declared. The watch-item is the fossil leg: if retained capacity stops functioning as a maintained option and becomes default continuation dressed in flexibility language, that leg atrophies toward performance while the nuclear leg retains function - the theater_ratio series is the instrument that would register it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contestation,
    'This story instantiates one reading (option_value_preserving) of the kernel acceptable_risk_energy. How would the sibling readings restructure the constraint''s beneficiary/victim sets and classification?',
    'Comparative compilation and classification of the sibling stories (acceptable_risk_energy__catastrophic_tail_dominant, acceptable_risk_energy__expected_value_dominant) over the same standing arrangement.',
    'Under catastrophic_tail_dominant the fossil leg loses defensibility - tail-exposed populations join the victim set and epsilon rises sharply. Under expected_value_dominant the fossil leg closes on mortality-per-TWh arithmetic while nuclear protection strengthens. This reading''s moderate dual-suppression profile exists only under the option frame.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'Committer structure: one of three rival readings of the acceptable-risk kernel; sibling readings are separate constraints, not alternatives inside this one.').

omega_variable(
    genuine_optionality_vs_inertial_retention,
    'Is the retained fossil capacity functioning as a preserved option (dispatchable, convertible, maintained against a stated trigger) or as default continuation that would persist regardless of any flexibility rationale?',
    'Dispatch and maintenance records audited against declared reactivation triggers; counterfactual retirement modeling of what capacity owners would do absent the retention regime.',
    'If inertial, the fossil leg''s theater_ratio is understated and that leg drifts piton-ward while the nuclear leg retains function; if genuine, the coordination function is confirmed and the measured extraction reads largely as insurance premium.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(genuine_optionality_vs_inertial_retention, empirical, 'Whether the fossil leg of the portfolio is a real option or rhetorical cover for default continuation.').

omega_variable(
    depth_of_uncertainty_claim,
    'Is the uncertainty motivating pathway preservation genuinely deep (parameters not probabilizable) or merely wide-but-probabilizable?',
    'Convergence analysis across independent expert elicitations and integrated-assessment ensembles tracked over time: shrinking, calibrating distributions would indicate probabilizability.',
    'If uncertainty proves probabilizable, the expected_value_dominant sibling regains footing and this regime''s justification shifts from coordination to preference - the extraction re-reads as rent and classification drifts snare-ward.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(depth_of_uncertainty_claim, empirical, 'The epistemic premise the whole option-value justification rests on.').

omega_variable(
    suppressed_extreme_victim_status,
    'Are the suppressed advocacy movements victims of the arrangement (bearing imposed costs) or merely outbid participants in an ordinary policy contest?',
    'Examine whether the regime''s blocking mechanisms exceed ordinary majoritarian procedure - supermajority locks, licensing structures insulated from electoral reversal, adequacy findings with binding force.',
    'If ordinary contest, suppression reads low and the transfer reads as routine politics; if structurally insulated from reversal, the movements'' postponed-endpoint costs strengthen the asymmetric-extraction component of the classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppressed_extreme_victim_status, conceptual, 'Boundary of the victim set: imposed cost versus lost vote.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(acceptable_risk_energy__option_value_preserving, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(are_ovp_tr_t0, acceptable_risk_energy__option_value_preserving, theater_ratio, 0, 0.12).
narrative_ontology:measurement_basis(are_ovp_tr_t0, observed).
narrative_ontology:measurement(are_ovp_tr_t3, acceptable_risk_energy__option_value_preserving, theater_ratio, 3, 0.14).
narrative_ontology:measurement_basis(are_ovp_tr_t3, observed).
narrative_ontology:measurement(are_ovp_tr_t6, acceptable_risk_energy__option_value_preserving, theater_ratio, 6, 0.13).
narrative_ontology:measurement_basis(are_ovp_tr_t6, observed).
narrative_ontology:measurement(are_ovp_tr_t9, acceptable_risk_energy__option_value_preserving, theater_ratio, 9, 0.16).
narrative_ontology:measurement_basis(are_ovp_tr_t9, observed).
narrative_ontology:measurement(are_ovp_tr_t12, acceptable_risk_energy__option_value_preserving, theater_ratio, 12, 0.19).
narrative_ontology:measurement_basis(are_ovp_tr_t12, observed).
narrative_ontology:measurement(are_ovp_tr_t15, acceptable_risk_energy__option_value_preserving, theater_ratio, 15, 0.22).
narrative_ontology:measurement_basis(are_ovp_tr_t15, observed).
narrative_ontology:measurement(are_ovp_tr_t18, acceptable_risk_energy__option_value_preserving, theater_ratio, 18, 0.24).
narrative_ontology:measurement_basis(are_ovp_tr_t18, observed).
narrative_ontology:measurement(are_ovp_tr_t21, acceptable_risk_energy__option_value_preserving, theater_ratio, 21, 0.27).
narrative_ontology:measurement_basis(are_ovp_tr_t21, observed).
narrative_ontology:measurement(are_ovp_tr_t24, acceptable_risk_energy__option_value_preserving, theater_ratio, 24, 0.29).
narrative_ontology:measurement_basis(are_ovp_tr_t24, observed).
narrative_ontology:measurement(are_ovp_tr_t27, acceptable_risk_energy__option_value_preserving, theater_ratio, 27, 0.27).
narrative_ontology:measurement_basis(are_ovp_tr_t27, observed).
narrative_ontology:measurement(are_ovp_tr_t30, acceptable_risk_energy__option_value_preserving, theater_ratio, 30, 0.25).
narrative_ontology:measurement_basis(are_ovp_tr_t30, observed).

% Extraction over time
narrative_ontology:measurement(are_ovp_be_t0, acceptable_risk_energy__option_value_preserving, base_extractiveness, 0, 0.42).
narrative_ontology:measurement_basis(are_ovp_be_t0, observed).
narrative_ontology:measurement(are_ovp_be_t3, acceptable_risk_energy__option_value_preserving, base_extractiveness, 3, 0.44).
narrative_ontology:measurement_basis(are_ovp_be_t3, observed).
narrative_ontology:measurement(are_ovp_be_t6, acceptable_risk_energy__option_value_preserving, base_extractiveness, 6, 0.43).
narrative_ontology:measurement_basis(are_ovp_be_t6, observed).
narrative_ontology:measurement(are_ovp_be_t9, acceptable_risk_energy__option_value_preserving, base_extractiveness, 9, 0.46).
narrative_ontology:measurement_basis(are_ovp_be_t9, observed).
narrative_ontology:measurement(are_ovp_be_t12, acceptable_risk_energy__option_value_preserving, base_extractiveness, 12, 0.48).
narrative_ontology:measurement_basis(are_ovp_be_t12, observed).
narrative_ontology:measurement(are_ovp_be_t15, acceptable_risk_energy__option_value_preserving, base_extractiveness, 15, 0.47).
narrative_ontology:measurement_basis(are_ovp_be_t15, observed).
narrative_ontology:measurement(are_ovp_be_t18, acceptable_risk_energy__option_value_preserving, base_extractiveness, 18, 0.5).
narrative_ontology:measurement_basis(are_ovp_be_t18, observed).
narrative_ontology:measurement(are_ovp_be_t21, acceptable_risk_energy__option_value_preserving, base_extractiveness, 21, 0.52).
narrative_ontology:measurement_basis(are_ovp_be_t21, observed).
narrative_ontology:measurement(are_ovp_be_t24, acceptable_risk_energy__option_value_preserving, base_extractiveness, 24, 0.54).
narrative_ontology:measurement_basis(are_ovp_be_t24, observed).
narrative_ontology:measurement(are_ovp_be_t27, acceptable_risk_energy__option_value_preserving, base_extractiveness, 27, 0.53).
narrative_ontology:measurement_basis(are_ovp_be_t27, observed).
narrative_ontology:measurement(are_ovp_be_t30, acceptable_risk_energy__option_value_preserving, base_extractiveness, 30, 0.52).
narrative_ontology:measurement_basis(are_ovp_be_t30, observed).

% Suppression requirement over time
narrative_ontology:measurement(are_ovp_su_t0, acceptable_risk_energy__option_value_preserving, suppression_requirement, 0, 0.3).
narrative_ontology:measurement_basis(are_ovp_su_t0, observed).
narrative_ontology:measurement(are_ovp_su_t3, acceptable_risk_energy__option_value_preserving, suppression_requirement, 3, 0.32).
narrative_ontology:measurement_basis(are_ovp_su_t3, observed).
narrative_ontology:measurement(are_ovp_su_t6, acceptable_risk_energy__option_value_preserving, suppression_requirement, 6, 0.31).
narrative_ontology:measurement_basis(are_ovp_su_t6, observed).
narrative_ontology:measurement(are_ovp_su_t9, acceptable_risk_energy__option_value_preserving, suppression_requirement, 9, 0.34).
narrative_ontology:measurement_basis(are_ovp_su_t9, observed).
narrative_ontology:measurement(are_ovp_su_t12, acceptable_risk_energy__option_value_preserving, suppression_requirement, 12, 0.36).
narrative_ontology:measurement_basis(are_ovp_su_t12, observed).
narrative_ontology:measurement(are_ovp_su_t15, acceptable_risk_energy__option_value_preserving, suppression_requirement, 15, 0.38).
narrative_ontology:measurement_basis(are_ovp_su_t15, observed).
narrative_ontology:measurement(are_ovp_su_t18, acceptable_risk_energy__option_value_preserving, suppression_requirement, 18, 0.52).
narrative_ontology:measurement_basis(are_ovp_su_t18, observed).
narrative_ontology:measurement(are_ovp_su_t21, acceptable_risk_energy__option_value_preserving, suppression_requirement, 21, 0.48).
narrative_ontology:measurement_basis(are_ovp_su_t21, observed).
narrative_ontology:measurement(are_ovp_su_t24, acceptable_risk_energy__option_value_preserving, suppression_requirement, 24, 0.44).
narrative_ontology:measurement_basis(are_ovp_su_t24, observed).
narrative_ontology:measurement(are_ovp_su_t27, acceptable_risk_energy__option_value_preserving, suppression_requirement, 27, 0.41).
narrative_ontology:measurement_basis(are_ovp_su_t27, observed).
narrative_ontology:measurement(are_ovp_su_t30, acceptable_risk_energy__option_value_preserving, suppression_requirement, 30, 0.45).
narrative_ontology:measurement_basis(are_ovp_su_t30, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(acceptable_risk_energy__option_value_preserving, resource_allocation).
narrative_ontology:affects_constraint(acceptable_risk_energy__option_value_preserving, acceptable_risk_energy__catastrophic_tail_dominant).
narrative_ontology:affects_constraint(acceptable_risk_energy__option_value_preserving, acceptable_risk_energy__expected_value_dominant).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition of the colloquial label 'acceptable risk in energy policy'. The label conflates three structurally distinct decision criteria, each authored as its own story with its own epsilon, beneficiary/victim structure, and classification, linked through network.affects_constraints. Structural gradient: the option_value_preserving reading INFLUENCES the traction of expected_value_dominant (its deep-uncertainty premise erodes the legitimacy of single-metric cost-benefit ranking) while COEXISTING with catastrophic_tail_dominant (precaution and flexibility are jointly holdable in one framework, and option preservation is often justified BY tail-risk aversion).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
