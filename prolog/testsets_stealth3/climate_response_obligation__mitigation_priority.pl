% ============================================================================
% CONSTRAINT STORY: climate_response_obligation__mitigation_priority
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_climate_response_obligation__mitigation_priority, []).

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
 *   constraint_id: climate_response_obligation__mitigation_priority
 *   human_readable: Mitigation-Priority Reading of the Climate Response Obligation
 *   domain: political/economic/environmental
 *
 * SUMMARY:
 *   This story instantiates ONE reading — mitigation_priority — of the
 *   contested kernel climate_response_obligation. The standing arrangement
 *   under contest is the Paris-era mitigation regime: national pledges,
 *   net-zero statutes, carbon pricing, phase-out mandates, and the review
 *   machinery that disciplines them. Its function is to solve the atmospheric
 *   commons problem; its costs fall on identifiable present actors (fossil
 *   capital via stranded assets, carbon-intensive workers via plant closures,
 *   Global North households via energy prices), while its good is delivered
 *   to parties who cannot reciprocate, observe, or enforce — future
 *   generations — plus present climate-vulnerable populations. The sibling
 *   readings (adaptation_priority, degrowth_reading) are separate constraints
 *   with separate files; they are not described or averaged here. The
 *   claim/metric gap is deliberate: the constraint is CLAIMED as tangled_rope
 *   because it possesses both a genuine coordination function and asymmetric
 *   extraction under active enforcement, while the metrics are authored
 *   independently as descriptive facts about the regime's actual operation.
 *   ε's referent is the standing mitigation arrangement assessed by this
 *   reading's own lights — the reading affirms the extraction as owed, which
 *   is a judgment about legitimacy, not a denial that extraction occurs.
 *
 * KEY AGENTS:
 *   - - future_generations: Primary beneficiary (powerless/trapped) — receives the minimized-warming climate; cannot participate, contract, or enforce
 *   - - climate_vulnerable_populations: Secondary beneficiary (organized/trapped) — most-exposed present-day winners from successful mitigation
 *   - - fossil_capital: Primary payer (powerful/arbitrage) — bears stranded-asset losses; retains divest, pivot, relocate, and lobby exits
 *   - - carbon_intensive_sector_workers: Payer (organized/constrained) — bear localized transition costs with skill and geography lock-in
 *   - - global_north_households: Payer with secondary beneficiary position (moderate/constrained) — carry energy-price and consumption costs; benefit via descendants' climate
 *   - - renewable_energy_industries: Beneficiary (institutional/mobile) — capture concentrated present gains from mandates and subsidies
 *   - - global_south_developing_states: Beneficiary with secondary payer position (organized/constrained) — protected by mitigation yet development-space-constrained absent finance
 *   - - unfccc_cop_process: Agenda setter (institutional/constrained) — administers the pledge-review machinery under consensus rules
 *   - - ipcc_scientific_assessment_body: Analytical observer (institutional/analytical) — authors the carbon-budget science every seat cites
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_response_obligation__mitigation_priority, 0.72).
domain_priors:suppression_score(climate_response_obligation__mitigation_priority, 0.55).
domain_priors:theater_ratio(climate_response_obligation__mitigation_priority, 0.47).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_response_obligation__mitigation_priority, extractiveness, 0.72).
narrative_ontology:constraint_metric(climate_response_obligation__mitigation_priority, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(climate_response_obligation__mitigation_priority, theater_ratio, 0.47).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(climate_response_obligation__mitigation_priority, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(climate_response_obligation__mitigation_priority, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_response_obligation__mitigation_priority, tangled_rope).
narrative_ontology:human_readable(climate_response_obligation__mitigation_priority, "Mitigation-Priority Reading of the Climate Response Obligation").
narrative_ontology:topic_domain(climate_response_obligation__mitigation_priority, "political/economic/environmental").

domain_priors:requires_active_enforcement(climate_response_obligation__mitigation_priority).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_response_obligation__mitigation_priority, 'cc46cee8-8018-4019-9db6-a358afdfa6ef').
narrative_ontology:cs_kernel_codification('cc46cee8-8018-4019-9db6-a358afdfa6ef', fixed_text).
narrative_ontology:cs_authority_grounding('cc46cee8-8018-4019-9db6-a358afdfa6ef', distributed).
narrative_ontology:cs_reading_relation('cc46cee8-8018-4019-9db6-a358afdfa6ef', climate_response_obligation__adaptation_priority, forecloses).
narrative_ontology:cs_reading_relation('cc46cee8-8018-4019-9db6-a358afdfa6ef', climate_response_obligation__degrowth_reading, coexists_with).
narrative_ontology:cs_axiom('cc46cee8-8018-4019-9db6-a358afdfa6ef', foundational, intergenerational_justice_requires_minimizing_warming).
narrative_ontology:cs_axiom_status(intergenerational_justice_requires_minimizing_warming, holdable).
narrative_ontology:cs_axiom_grounding('cc46cee8-8018-4019-9db6-a358afdfa6ef', intergenerational_justice_requires_minimizing_warming, deontological).
narrative_ontology:cs_axiom('cc46cee8-8018-4019-9db6-a358afdfa6ef', foundational, early_mitigation_is_morally_prior_to_adaptation_allocation).
narrative_ontology:cs_axiom_status(early_mitigation_is_morally_prior_to_adaptation_allocation, holdable).
narrative_ontology:cs_axiom_grounding('cc46cee8-8018-4019-9db6-a358afdfa6ef', early_mitigation_is_morally_prior_to_adaptation_allocation, instrumental).
narrative_ontology:cs_axiom('cc46cee8-8018-4019-9db6-a358afdfa6ef', secondary, historical_emissions_ground_disproportionate_north_burden).
narrative_ontology:cs_axiom_status(historical_emissions_ground_disproportionate_north_burden, holdable).
narrative_ontology:cs_axiom_grounding('cc46cee8-8018-4019-9db6-a358afdfa6ef', historical_emissions_ground_disproportionate_north_burden, conventional).
narrative_ontology:cs_reference_frame('cc46cee8-8018-4019-9db6-a358afdfa6ef', prevention_dominant_minimization).
narrative_ontology:cs_drift_state('cc46cee8-8018-4019-9db6-a358afdfa6ef', first_global_stocktake_aftermath, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('cc46cee8-8018-4019-9db6-a358afdfa6ef', '').
narrative_ontology:cs_kernel_id(climate_response_obligation__mitigation_priority, climate_response_obligation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_response_obligation__mitigation_priority, future_generations).
narrative_ontology:constraint_beneficiary(climate_response_obligation__mitigation_priority, climate_vulnerable_populations).
narrative_ontology:constraint_beneficiary(climate_response_obligation__mitigation_priority, renewable_energy_industries).
narrative_ontology:constraint_victim(climate_response_obligation__mitigation_priority, fossil_capital).
narrative_ontology:constraint_victim(climate_response_obligation__mitigation_priority, carbon_intensive_sector_workers).
narrative_ontology:constraint_victim(climate_response_obligation__mitigation_priority, global_north_households).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(climate_response_obligation__mitigation_priority, global_north_households).
narrative_ontology:constraint_beneficiary(climate_response_obligation__mitigation_priority, global_south_developing_states).
narrative_ontology:constraint_victim(climate_response_obligation__mitigation_priority, global_south_developing_states).
narrative_ontology:constraint_vindicates(climate_response_obligation__mitigation_priority, intergenerational_justice_doctrine).
narrative_ontology:constraint_vindicates(climate_response_obligation__mitigation_priority, precautionary_principle).
narrative_ontology:constraint_vindicates(climate_response_obligation__mitigation_priority, carbon_budget_science).
narrative_ontology:constraint_vindicates(climate_response_obligation__mitigation_priority, common_but_differentiated_responsibility).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% People who will be alive in the second half of this century and after. They inherit whatever concentration of greenhouse gases the present leaves in the atmosphere, along with the warming already locked in. They cannot vote, contract, litigate in their own name, or withhold cooperation; every present decision about how fast to cut emissions is made entirely by others on their behalf. What reaches them depends wholly on whether present institutions keep the promises made in their name.
narrative_ontology:constraint_stakeholder(climate_response_obligation__mitigation_priority, future_generations, beneficiary,
    powerless, civilizational, trapped, global).

% Communities in low-lying coastal zones, arid regions, and small island states whose exposure to floods, heat, and storm damage is highest. Successful emission cuts protect them sooner and more completely than anyone else alive today. They organize through coalitions such as the Alliance of Small Island States to press for deeper cuts, but they cannot move away from a warming planet, and their own emissions are small.
narrative_ontology:constraint_stakeholder(climate_response_obligation__mitigation_priority, climate_vulnerable_populations, beneficiary,
    organized, generational, trapped, global).

% Manufacturers, developers, and utilities building wind, solar, storage, transmission, and electrified transport. Mandates, subsidies, and carbon prices channel investment and revenue toward them; their order books and valuations depend directly on the pace and credibility of decarbonization rules. They can and do relocate production and lobbying across jurisdictions to wherever support is strongest.
narrative_ontology:constraint_stakeholder(climate_response_obligation__mitigation_priority, renewable_energy_industries, beneficiary,
    institutional, biographical, mobile, global).

% Owners of coal, oil, and gas reserves, pipelines, refineries, and related infrastructure. Phase-out schedules and carbon prices convert part of their booked reserves and equipment into write-downs. They retain real exits: divesting holdings, pivoting into low-carbon lines, relocating headquarters, and financing political opposition. A large share choose the exit of fighting rather than absorbing the loss.
narrative_ontology:constraint_stakeholder(climate_response_obligation__mitigation_priority, fossil_capital, payer,
    powerful, biographical, arbitrage, global).

% Coal miners, oil and gas field workers, combustion-engine plant employees, and adjacent trades. Their wages, pensions, and town economies are tied to facilities scheduled for closure. Retraining programs exist but fit poorly with age, location, and mortgage realities; moving or switching trades carries steep personal costs, so most stay until the plant closes.
narrative_ontology:constraint_stakeholder(climate_response_obligation__mitigation_priority, carbon_intensive_sector_workers, payer,
    organized, immediate, constrained, regional).

% Households in wealthy economies facing higher electricity and fuel prices, renovation mandates, and restrictions on high-emission consumption. They also hold the largest per-capita historical responsibility and the deepest capacity to pay. Benefits reach them indirectly and late — cleaner air now, and the climate their children inherit — while the costs arrive on monthly bills.
narrative_ontology:constraint_stakeholder(climate_response_obligation__mitigation_priority, global_north_households, payer,
    moderate, biographical, constrained, continental).
narrative_ontology:stakeholder_secondary_role(climate_response_obligation__mitigation_priority, global_north_households, beneficiary).

% Governments of lower-income countries that industrialized late and emit little per capita. Deep global cuts shield them from damages they are least equipped to absorb, but carbon budgets also compress the fossil-fueled development path earlier economies used, unless finance and technology transfer arrives. They bargain as a bloc for finance in exchange for earlier peaking dates.
narrative_ontology:constraint_stakeholder(climate_response_obligation__mitigation_priority, global_south_developing_states, beneficiary,
    organized, generational, constrained, continental).
narrative_ontology:stakeholder_secondary_role(climate_response_obligation__mitigation_priority, global_south_developing_states, payer).

% The treaty secretariat, rotating presidencies, and national climate ministries that run the annual conferences, review national pledges, and maintain the transparency framework. They administer the machinery but command no independent enforcement force; their leverage is reputational and procedural, plus whatever domestic courts and markets do with the commitments. Consensus rules give every party a veto over strengthening.
narrative_ontology:constraint_stakeholder(climate_response_obligation__mitigation_priority, unfccc_cop_process, agenda_setter,
    institutional, generational, constrained, global).

% The assessment panels and the research community behind them. They produce the carbon budgets, remaining-emission estimates, and impact projections that every other seat cites when arguing for or against particular allocations. They take no position on how burdens should be split and hold no enforcement role.
narrative_ontology:constraint_stakeholder(climate_response_obligation__mitigation_priority, ipcc_scientific_assessment_body, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(climate_response_obligation__mitigation_priority, renewable_energy_industries).
narrative_ontology:fixing_cost_class(climate_response_obligation__mitigation_priority, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the atmospheric commons problem: a ton of CO2 warms the planet identically wherever it is emitted, so only coordinated, near-universal, front-loaded emission reductions keep cumulative emissions inside any given temperature budget. The arrangement coordinates the timing, depth, and distribution of cuts across all emitters, and supplies the inventory, review, and stocktake machinery that makes mutual restraint verifiable.
% TRANSFER_FUNCTION: Moves present value — fossil revenues, carbon-intensive employment, cheap energy consumption, and asset values — from current emitters (concentrated in fossil-capital balance sheets and Global North economies) into an unproduced public good, a minimized-warming climate, delivered mainly to future generations and climate-vulnerable populations; secondarily it redirects capital flows toward low-carbon industries.
% ABSENT_VOICES: The primary beneficiary class — future generations — holds no seat in any forum where the obligation is negotiated; their interests enter only through proxy advocates (youth litigants, ombudsperson proposals, a few state delegations). Nonhuman systems affected by warming are likewise unrepresented. Meanwhile the strongest present losers, fossil capital, are loudly present, which systematically tilts negotiated ambition downward.
% DISAPPEARANCE_RATIONALE: If the mitigation obligation vanished overnight, energy investment would revert to the cheapest available sources: coal and oil expansion resumes, renewable pipelines cancel, adaptation spending rises to meet accelerating damages, and the intergenerational distribution of climate harm shifts wholesale onto the unborn. The courts, carbon markets, and disclosure regimes built on the obligation would lose their normative anchor simultaneously.
% FOUNDING_PROBLEM: Industrial energy systems impose cumulative, delayed harms whose costs fall overwhelmingly on people who cannot vote, contract, or retaliate — a double failure of market pricing and political representation. The arrangement was built to make present actors internalize harms to absent future parties before the atmosphere's buffering capacity is spent.
% FOUNDING_PROBLEM_CORROBORATION: IPCC assessment cycles — a scientific body outside the benefiting parties — attest the problem is live and worsening; central banks and insurance actuaries price the exposure independently; constitutional and human-rights courts (Urgenda, Neubauer, KlimaSeniorinnen) corroborate the intergenerational-duty framing from outside the beneficiary set. No corroboration can come from future generations themselves, who cannot attest — the duty's holders are silent by construction, which is itself structural signal.
narrative_ontology:disappearance_verdict(climate_response_obligation__mitigation_priority, world_rearranges).
narrative_ontology:founding_problem_status(climate_response_obligation__mitigation_priority, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(climate_response_obligation__mitigation_priority, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(climate_response_obligation__mitigation_priority, 'none', 1).
narrative_ontology:epsilon_provenance(climate_response_obligation__mitigation_priority, 0.72, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(climate_response_obligation__mitigation_priority_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(climate_response_obligation__mitigation_priority, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(climate_response_obligation__mitigation_priority_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is high (0.72 at interval end) because transition costs are large, front-loaded, and borne by identifiable actors while the delivered good is deferred and diffuse — the classic shape of intergenerational transfer. Suppression (0.55) is authored as a raw structural property, unscaled by power or scope: treaty-level enforcement lacks hard sanctions, but domestic instruments (constitutional climate litigation, carbon pricing, border adjustments, disclosure mandates) actively close business-as-usual exits, and the series shows enforcement machinery maturing over the interval. Theater (0.47) reflects the pledge-offset-accounting layer: a near-half share of declared activity is performative (offsets of dubious additionality, targets without implementing policy), rising as pledge volume outpaced delivery after 2009. Accessibility collapse (0.55) is partial: once carbon-budget physics is granted, alternatives to deep mitigation thin sharply, but political alternatives (adaptation-first, sufficiency) remain live options, so collapse stops well short of natural-law completeness. Resistance (0.70) is sustained: lobbying, delay discourse, cost-of-living backlash, and withdrawal cycles. The measurement series runs on one shared seven-point grid (1992–2025) so every tracked metric is authored at every examined time point; the 2009 dip in suppression_requirement marks the post-Copenhagen enforcement retreat, not noise. Victim-side coalition potential is real but asymmetric: worker-household just-transition alliances are the emergent counterweight, while fossil capital's arbitrage exits prevent durable victim solidarity with them.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute differently. From fossil capital's position the arrangement approaches pure coerced loss — every flow is outbound and the good is invisible — so its computed type skews harsher than the story-level claim. From the future-generations seat (held vicariously by proxies) the same structure reads as minimal decency purchased at tolerable cost. Carbon-intensive workers sit between: real loss, real (if ill-fitting) transition support. The agenda-setter seat experiences the arrangement as fragile consensus management it cannot strengthen without unanimity. The engine derives these per-seat classifications from the authored power, exit, and role data; the story-level claim does not adjudicate among them.
 *
 * DIRECTIONALITY LOGIC:
 *   Future generations are declared beneficiaries with powerless/trapped position: d sits near the full-beneficiary end — the constraint subsidizes them entirely. Climate-vulnerable populations and global_south_developing_states derive low d from beneficiary declaration, with the latter pulled slightly upward by its secondary payer position. Renewable_energy_industries derives low d from beneficiary status amplified by mobile exit — effectively subsidized by the regime. On the target side, carbon_intensive_sector_workers (victim, constrained exit) sit near the full-target end; global_north_households (victim with secondary beneficiary position) sit moderately high; fossil_capital is the interesting case — victim declaration raises d, but arbitrage-grade exit damps it below the trapped-target maximum, which is exactly right: they bear heavy extraction yet retain enough exit capacity to fight rather than absorb. No directionality overrides were needed; the beneficiary/victim declarations plus exit atoms produce the correct qualitative map.
 *
 * MANDATROPHY ANALYSIS:
 *   No mandatrophy: the founding problem (present actors imposing uncompensated harms on absent future parties) is fully live — emissions remain far above any budget-consistent path, so founding_problem_status is live and the status x disappearance_verdict pair (live x world_rearranges) raises no zombie flag. The tangled_rope classification guards both mislabeling directions: reading the arrangement as pure coordination ignores the named victims (stranded assets, closed plants, higher household bills); reading it as pure extraction ignores the genuine commons function no alternative institution currently performs. The forward risk is piton drift rather than mandatrophy: if the pledge-delivery gap keeps widening, theater_ratio crosses 0.5 and the regime persists as performance while the founding problem worsens — the temporal series is authored to make that trajectory detectable.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_structural_delta,
    'How would this constraint''s beneficiary/victim structure and epsilon reconfigure under the sibling readings of the climate_response_obligation kernel?',
    'Author and classify the sibling stories (climate_response_obligation__adaptation_priority, climate_response_obligation__degrowth_reading) separately, then diff the structural partitions across the family.',
    'Under adaptation_priority, future generations drop out as primary beneficiaries and transition spending re-enters as waste rather than owed transfer; under degrowth_reading the victim set widens to all throughput-intensive consumption and the Global North disproportion narrows. Each sibling is a different constraint with its own epsilon, not a re-measurement of this one.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_structural_delta, conceptual, 'Committer structure: this story is the mitigation_priority reading; siblings instantiate structurally different constraints from the same kernel.').

omega_variable(
    absent_party_enforceability,
    'Can an obligation whose primary beneficiaries cannot observe, enforce, or even witness compliance remain binding on present majorities, or must it decay into present-interest bargaining?',
    'Track pledge-versus-delivery ratios across jurisdictions as advocacy attention shifts; compare jurisdictions granting future-generations legal standing (youth climate suits, ombudsperson offices) against those without.',
    'If compliance tracks advocacy attention rather than codified duty, the arrangement drifts toward theatrical maintenance and eventual piton classification; durable standing mechanisms would support tangled_rope stability.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(absent_party_enforceability, empirical, 'Whether duties to structurally silent parties can stabilize without conversion into present-interest bargains.').

omega_variable(
    intergenerational_discount_rate_contest,
    'What social discount rate applied to future welfare is ethically defensible — the single parameter that scales the entire extraction profile of this arrangement?',
    'Not resolvable by data alone: revealed-preference analysis of adopted social-cost-of-carbon values constrains the practical range, but the residual is an explicit ethical choice each polity must own.',
    'Near-zero discounting justifies nearly any present cost, and the measured extraction reads as owed; discount rates above roughly 3 percent collapse the mitigation-priority case toward the adaptation_priority allocation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(intergenerational_discount_rate_contest, preference, 'The ethical parameter governing how heavily present costs weigh against future benefits.').

omega_variable(
    stranded_asset_loss_incidence,
    'Who ultimately absorbs stranded-asset losses — shareholders, pension beneficiaries, workers, or taxpayers via stabilization bailouts?',
    'Trace realized write-downs and rescue interventions across jurisdictions as phase-out schedules bite.',
    'If losses socialize, fossil_capital''s effective extraction falls and taxpayers enter the victim set, shifting the seat map without changing the constraint''s identity or coordination function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(stranded_asset_loss_incidence, empirical, 'Final incidence of the transition''s largest concentrated cost.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_response_obligation__mitigation_priority, 1992, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mitigation_priority_tr_t1992, climate_response_obligation__mitigation_priority, theater_ratio, 1992, 0.18).
narrative_ontology:measurement(mitigation_priority_tr_t1997, climate_response_obligation__mitigation_priority, theater_ratio, 1997, 0.22).
narrative_ontology:measurement(mitigation_priority_tr_t2005, climate_response_obligation__mitigation_priority, theater_ratio, 2005, 0.26).
narrative_ontology:measurement(mitigation_priority_tr_t2009, climate_response_obligation__mitigation_priority, theater_ratio, 2009, 0.34).
narrative_ontology:measurement(mitigation_priority_tr_t2015, climate_response_obligation__mitigation_priority, theater_ratio, 2015, 0.36).
narrative_ontology:measurement(mitigation_priority_tr_t2020, climate_response_obligation__mitigation_priority, theater_ratio, 2020, 0.42).
narrative_ontology:measurement(mitigation_priority_tr_t2025, climate_response_obligation__mitigation_priority, theater_ratio, 2025, 0.47).

% Extraction over time
narrative_ontology:measurement(mitigation_priority_be_t1992, climate_response_obligation__mitigation_priority, base_extractiveness, 1992, 0.3).
narrative_ontology:measurement(mitigation_priority_be_t1997, climate_response_obligation__mitigation_priority, base_extractiveness, 1997, 0.4).
narrative_ontology:measurement(mitigation_priority_be_t2005, climate_response_obligation__mitigation_priority, base_extractiveness, 2005, 0.44).
narrative_ontology:measurement(mitigation_priority_be_t2009, climate_response_obligation__mitigation_priority, base_extractiveness, 2009, 0.46).
narrative_ontology:measurement(mitigation_priority_be_t2015, climate_response_obligation__mitigation_priority, base_extractiveness, 2015, 0.6).
narrative_ontology:measurement(mitigation_priority_be_t2020, climate_response_obligation__mitigation_priority, base_extractiveness, 2020, 0.66).
narrative_ontology:measurement(mitigation_priority_be_t2025, climate_response_obligation__mitigation_priority, base_extractiveness, 2025, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(mitigation_priority_su_t1992, climate_response_obligation__mitigation_priority, suppression_requirement, 1992, 0.22).
narrative_ontology:measurement(mitigation_priority_su_t1997, climate_response_obligation__mitigation_priority, suppression_requirement, 1997, 0.3).
narrative_ontology:measurement(mitigation_priority_su_t2005, climate_response_obligation__mitigation_priority, suppression_requirement, 2005, 0.34).
narrative_ontology:measurement(mitigation_priority_su_t2009, climate_response_obligation__mitigation_priority, suppression_requirement, 2009, 0.3).
narrative_ontology:measurement(mitigation_priority_su_t2015, climate_response_obligation__mitigation_priority, suppression_requirement, 2015, 0.44).
narrative_ontology:measurement(mitigation_priority_su_t2020, climate_response_obligation__mitigation_priority, suppression_requirement, 2020, 0.5).
narrative_ontology:measurement(mitigation_priority_su_t2025, climate_response_obligation__mitigation_priority, suppression_requirement, 2025, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_response_obligation__mitigation_priority, resource_allocation).
narrative_ontology:affects_constraint(climate_response_obligation__mitigation_priority, climate_response_obligation__adaptation_priority).
narrative_ontology:affects_constraint(climate_response_obligation__mitigation_priority, climate_response_obligation__degrowth_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'climate action' decomposes, per the epsilon-invariance principle, into at least three structurally distinct response obligations sharing one kernel (the UNFCCC/Paris commitment to respond). This story instantiates the mitigation_priority reading only: its epsilon is authored for the standing mitigation regime as this reading assesses it — heavy present extraction affirmed as owed. The adaptation_priority sibling authors a different constraint (present exposed populations as beneficiaries; prevention spending re-read as waste); the degrowth_reading sibling widens the victim set to all throughput-intensive consumption. The physical carbon budget itself is a separate mountain-class regularity that all three readings cite as evidence; it is not part of this story. Family members link via affects_constraints; the upstream scientific claim functions as shared evidence for downstream readings whose epsilon values differ widely.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
