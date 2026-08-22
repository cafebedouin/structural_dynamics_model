% ============================================================================
% CONSTRAINT STORY: climate_response_obligation__mitigation_priority
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
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
 *   constraint_id: climate_response_obligation__mitigation_priority
 *   human_readable: Mitigation-Priority Climate Response Obligation (Rapid Decarbonization)
 *   domain: political/economic/ethical — climate governance and intergenerational justice
 *
 * SUMMARY:
 *   The standing arrangement under contest is the global climate-mitigation
 *   commitment structure as it actually operates: the treaty architecture,
 *   national net-zero pledges, carbon pricing regimes, renewable subsidy
 *   complexes, and the enforcement machinery forming around them. This file
 *   instantiates ONE reading of the climate_response_obligation kernel —
 *   mitigation_priority — which holds that the obligation is discharged by
 *   rapid decarbonization and that intergenerational justice requires
 *   minimizing warming. The reading's structural signature: future
 *   generations are the primary beneficiaries, the current generation bears
 *   transition costs, the Global North bears a disproportionate mitigation
 *   burden on historical-emissions grounds, and fossil capital enters the
 *   victim set through stranded assets. The claim/metric gap is deliberate:
 *   the reading CLAIMS a justice-grounded coordination duty while the
 *   authored metrics describe substantially extractive, increasingly
 *   theatrical, actively enforced operation — the engine measures that
 *   divergence; do not reconcile the claim to the metrics. Sibling readings
 *   (adaptation_priority, degrowth_reading) are separate constraints in
 *   separate files; nothing about them is averaged into this one.
 *
 * KEY AGENTS:
 *   - - future_generations: Primary intended beneficiary (powerless/trapped) — receives avoided warming; absent from every table
 *   - - renewable_energy_sectors: Concentrated present-day collector (organized/mobile) — contracted rents from mandates, credits, and carbon markets
 *   - - fossil_capital_asset_holders: Primary payer (powerful/constrained) — booked reserves converted to stranded capital
 *   - - carbon_intensive_workers: Concentrated payer (moderate/trapped) — payroll and community losses on externally set schedules
 *   - - global_north_households: Diffuse payer (moderate/constrained) — visible near-term costs, posthumous returns
 *   - - climate_vulnerable_regions: Secondary beneficiary (organized/trapped) — finance flows and avoided damages, least responsible for the stock
 *   - - unfccc_cop_process: Agenda-setter (institutional/constrained) — administers the architecture under consensus rules
 *   - - youth_climate_movements: Excluded claimant (organized/trapped) — mobilizes outside formal seats
 *   - - ipcc_assessment_body: Analytical observer (institutional/analytical) — defines the numbers every faction must cite
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_response_obligation__mitigation_priority, 0.62).
domain_priors:suppression_score(climate_response_obligation__mitigation_priority, 0.58).
domain_priors:theater_ratio(climate_response_obligation__mitigation_priority, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_response_obligation__mitigation_priority, extractiveness, 0.62).
narrative_ontology:constraint_metric(climate_response_obligation__mitigation_priority, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(climate_response_obligation__mitigation_priority, theater_ratio, 0.55).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(climate_response_obligation__mitigation_priority, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(climate_response_obligation__mitigation_priority, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_response_obligation__mitigation_priority, tangled_rope).
narrative_ontology:human_readable(climate_response_obligation__mitigation_priority, "Mitigation-Priority Climate Response Obligation (Rapid Decarbonization)").
narrative_ontology:topic_domain(climate_response_obligation__mitigation_priority, "political/economic/ethical — climate governance and intergenerational justice").

domain_priors:requires_active_enforcement(climate_response_obligation__mitigation_priority).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_response_obligation__mitigation_priority, '872778e5-022c-4a47-87b3-1a7e78b2282d').
narrative_ontology:cs_kernel_codification('872778e5-022c-4a47-87b3-1a7e78b2282d', formalized).
narrative_ontology:cs_authority_grounding('872778e5-022c-4a47-87b3-1a7e78b2282d', expertise).
narrative_ontology:cs_interpretation_layer_present('872778e5-022c-4a47-87b3-1a7e78b2282d').
narrative_ontology:cs_reading_relation('872778e5-022c-4a47-87b3-1a7e78b2282d', climate_response_obligation__adaptation_priority, influences).
narrative_ontology:cs_reading_relation('872778e5-022c-4a47-87b3-1a7e78b2282d', climate_response_obligation__degrowth_reading, coexists_with).
narrative_ontology:cs_axiom('872778e5-022c-4a47-87b3-1a7e78b2282d', foundational, warming_minimization_owed_to_future_persons).
narrative_ontology:cs_axiom_status(warming_minimization_owed_to_future_persons, holdable).
narrative_ontology:cs_axiom_grounding('872778e5-022c-4a47-87b3-1a7e78b2282d', warming_minimization_owed_to_future_persons, deontological).
narrative_ontology:cs_axiom('872778e5-022c-4a47-87b3-1a7e78b2282d', foundational, historical_emitters_owe_disproportionate_mitigation).
narrative_ontology:cs_axiom_status(historical_emitters_owe_disproportionate_mitigation, holdable).
narrative_ontology:cs_axiom_grounding('872778e5-022c-4a47-87b3-1a7e78b2282d', historical_emitters_owe_disproportionate_mitigation, deontological).
narrative_ontology:cs_axiom('872778e5-022c-4a47-87b3-1a7e78b2282d', secondary, rapid_decarbonization_prevents_threshold_crossing).
narrative_ontology:cs_axiom_status(rapid_decarbonization_prevents_threshold_crossing, holdable).
narrative_ontology:cs_axiom_grounding('872778e5-022c-4a47-87b3-1a7e78b2282d', rapid_decarbonization_prevents_threshold_crossing, empirically_contingent).
narrative_ontology:cs_reference_frame('872778e5-022c-4a47-87b3-1a7e78b2282d', intergenerational_carbon_stewardship).
narrative_ontology:cs_drift_state('872778e5-022c-4a47-87b3-1a7e78b2282d', contemporary_pledge_delivery_gap, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('872778e5-022c-4a47-87b3-1a7e78b2282d', '').
narrative_ontology:cs_kernel_id(climate_response_obligation__mitigation_priority, climate_response_obligation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_response_obligation__mitigation_priority, future_generations).
narrative_ontology:constraint_beneficiary(climate_response_obligation__mitigation_priority, renewable_energy_sectors).
narrative_ontology:constraint_beneficiary(climate_response_obligation__mitigation_priority, climate_vulnerable_regions).
narrative_ontology:constraint_victim(climate_response_obligation__mitigation_priority, fossil_capital_asset_holders).
narrative_ontology:constraint_victim(climate_response_obligation__mitigation_priority, carbon_intensive_workers).
narrative_ontology:constraint_victim(climate_response_obligation__mitigation_priority, global_north_households).
narrative_ontology:constraint_vindicates(climate_response_obligation__mitigation_priority, intergenerational_justice_doctrine).
narrative_ontology:constraint_vindicates(climate_response_obligation__mitigation_priority, precautionary_principle).
narrative_ontology:constraint_vindicates(climate_response_obligation__mitigation_priority, common_but_differentiated_responsibilities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% People not yet born, and children too young to vote, who will inhabit whatever climate today's decisions produce. They receive the arrangement's intended return — a lower-warming world — but hold no vote, no negotiating seat, and in most jurisdictions no standing to sue; youth organizations and sympathetic courts speak for them only indirectly. Nothing they can do alters the terms they are offered.
narrative_ontology:constraint_stakeholder(climate_response_obligation__mitigation_priority, future_generations, beneficiary,
    powerless, civilizational, trapped, global).
narrative_ontology:stakeholder_secondary_role(climate_response_obligation__mitigation_priority, future_generations, excluded).

% Manufacturers, project developers, and financiers of wind, solar, storage, transmission, and hydrogen. Mandates, tax credits, guaranteed-price contracts, and carbon markets create and protect their customer base, and contracted revenues arrive on schedule regardless of whether aggregate emissions fall on schedule. Capital can be redeployed to other sectors if policy reverses, and the sector lobbies actively to extend and expand its preferential access.
narrative_ontology:constraint_stakeholder(climate_response_obligation__mitigation_priority, renewable_energy_sectors, beneficiary,
    organized, biographical, mobile, global).

% Low-lying island states, drought-exposed agricultural zones, and Arctic settlements. They receive adaptation finance lines and the largest share of avoided-damage benefit from successful mitigation, having contributed little to cumulative emissions. They cannot relocate out of the climate system and cannot unilaterally alter the treaty architecture their survival planning assumes; their leverage is moral coalition rather than market power.
narrative_ontology:constraint_stakeholder(climate_response_obligation__mitigation_priority, climate_vulnerable_regions, beneficiary,
    organized, generational, trapped, global).

% Owners of reserves, pipelines, refineries, and thermal generation whose balance sheets assume decades of continued combustion. Policy timelines convert a growing fraction of booked reserves into assets that cannot be burned profitably, and the underlying resources cannot be moved or repurposed. Available responses are political (delay, dilute, seek compensation), financial (accelerate recovery before stranding), or partial diversification into the favored technologies.
narrative_ontology:constraint_stakeholder(climate_response_obligation__mitigation_priority, fossil_capital_asset_holders, payer,
    powerful, biographical, constrained, global).

% Coal miners, oil and gas crews, refinery operators, engine-plant workers, and the towns built around them. Payrolls end on schedules set in distant capitals; skills are site- and sector-specific; housing equity and community identity are bound to the facility. Retraining leads into thinner local labor markets, and moving means leaving family networks and depreciated homes.
narrative_ontology:constraint_stakeholder(climate_response_obligation__mitigation_priority, carbon_intensive_workers, payer,
    moderate, immediate, trapped, regional).

% Taxpayers and energy consumers in high-income democracies who fund subsidy programs, absorb carbon-priced fuel and electricity, and carry the borrowing costs of transition investment. The visible costs arrive within household budget horizons while the advertised returns accrue largely after the payers' lifetimes. Political exit means voting against the governing coalition; personal exit from the energy system is effectively unavailable.
narrative_ontology:constraint_stakeholder(climate_response_obligation__mitigation_priority, global_north_households, payer,
    moderate, biographical, constrained, national).

% The treaty secretariats, rotating presidencies, and negotiating rounds that administer the response architecture: convening parties, setting agendas, verifying pledges, brokering finance packages. Their authority and continuity depend on the process continuing; consensus rules mean no party can be forced out, and the process cannot impose terms any major emitter refuses.
narrative_ontology:constraint_stakeholder(climate_response_obligation__mitigation_priority, unfccc_cop_process, agenda_setter,
    institutional, generational, constrained, global).

% Mass-mobilization and litigation networks of school-age and young-adult activists pressing for faster, deeper emission cuts. They demonstrate outside negotiating halls and win occasional court orders but hold no formal decision rights; they will live entirely inside the century whose temperature the current negotiators are setting.
narrative_ontology:constraint_stakeholder(climate_response_obligation__mitigation_priority, youth_climate_movements, excluded,
    organized, generational, trapped, global).

% The volunteer scientist network and technical staff whose assessment cycles define remaining carbon budgets, warming projections, and threshold estimates. Governments approve the summaries line by line; the body takes no position among competing response strategies, but its numbers are the common currency every strategy must cite.
narrative_ontology:constraint_stakeholder(climate_response_obligation__mitigation_priority, ipcc_assessment_body, observer,
    institutional, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(climate_response_obligation__mitigation_priority, renewable_energy_sectors).
narrative_ontology:fixing_cost_class(climate_response_obligation__mitigation_priority, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the atmospheric commons problem: no single actor can stabilize the climate alone, so the arrangement aligns national targets, investment signals, and technology deployment on a shared emissions pathway, and supplies the verification machinery that lets parties trust one another's cuts.
% TRANSFER_FUNCTION: Moves present purchasing power — tax revenue, private capital, and booked asset value — away from fossil asset holders, carbon-intensive employers, and high-income households, toward renewable-sector firms as contracted revenues, toward exposed regions as finance flows, and toward future generations as avoided warming.
% ABSENT_VOICES: Future generations — the arrangement's primary intended beneficiaries — are absent by construction: no vote, no seat, no standing. Fossil-dependent regions are present only through governments that frequently negotiate against their own workers' interests, and youth movements stand outside the hall with moral but not procedural leverage.
% DISAPPEARANCE_RATIONALE: Overnight removal would release energy investment back toward the cheapest available supply — overwhelmingly fossil in most grids — void the subsidized renewable order book and carbon-market contracts, unwind border-adjustment and disclosure machinery, and shift warming trajectories upward within a decade. The intergenerational transfer the arrangement performs would simply stop.
% FOUNDING_PROBLEM: Greenhouse gases are an intertemporal externality: emitters capture the benefit of combustion now while the costs land later on people who did not emit and were not represented. The arrangement was built to bind present actors to costs whose beneficiaries cannot yet exist as parties.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the green-sector beneficiary set: IPCC assessment cycles quantify the unpaid externality; insurance and reinsurance loss series document damages already arriving; the NGFS central-bank network treats the exposure as material to financial stability; and constitutional courts in several jurisdictions have accepted minors' standing to claim the harm. Fossil-industry submissions dispute the magnitude, not the existence, of the problem — dispute is contest, not absence of attestation.
narrative_ontology:disappearance_verdict(climate_response_obligation__mitigation_priority, world_rearranges).
narrative_ontology:founding_problem_status(climate_response_obligation__mitigation_priority, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(climate_response_obligation__mitigation_priority, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(climate_response_obligation__mitigation_priority, 'none', 1).
narrative_ontology:epsilon_provenance(climate_response_obligation__mitigation_priority, 0.62, 'stealth/ox-alpha', 'none', direct).

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
 *   Extractiveness is 0.62 at interval end: the arrangement moves very large value from identified, concentrated payers (asset holders, workers, households) toward diffuse and posthumous beneficiaries while a measurable share of receipts lands as contracted rents in organized present-day sectors — asymmetric enough to be real, short of snare-grade because the transfer tracks a genuine physical target. Suppression is 0.58: enforcement is regulatory-economic (mandates, capital denial, border adjustment, disclosure law) rather than violent, but dissenting sectors and regions face material coercion, and the enforcement build-out is tracked in the suppression_requirement series. Theater_ratio is 0.55: the pledge, offset, and disclosure layer has grown faster than delivery, a classic Goodhart surface. Accessibility_collapse is 0.45: alternatives (adaptation-first, degrowth, fossil continuation) remain politically live, so understanding the constraint does not close the option space. Resistance is 0.70: fuel-price protests, federal policy whiplash, petrostate obstruction, and finance-quarrel walkouts are routine. The three temporal series run on ONE shared eight-point grid (t=0..28, roughly Kyoto adoption to the mid-2020s), all monotonic rises; no cyclical pattern is claimed, so no intermittent-reinforcement mechanism is alleged.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats and the beneficiary/agenda seats compute differently from identical structural data. From fossil capital's position the arrangement is uncompensated expropriation on a schedule set by others; from carbon-intensive workers' position it is a payroll termination notice written in someone else's currency; from the COP seat it is the only workable answer to a commons problem; from renewable sectors it is a protected market; from future generations (via proxies) it is a bare-minimum duty already delivered too slowly. The engine computes this per-seat divergence from power, exit, and directional position — the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive low directionality: future_generations sits nearest the full-beneficiary end (powerless, trapped, entire benefit), climate_vulnerable_regions near it (trapped, organized-but-outnumbered), renewable_energy_sectors low despite mobility because its receipts are contractual and policy-contingent. Victim declarations drive high directionality: carbon_intensive_workers highest in effective terms (trapped, moderate power, concentrated loss), global_north_households high with diffuse incidence, fossil_capital_asset_holders high but tempered by power and partial arbitrage through delayed stranding. The engine scales effective extraction by directionality and scope — global scope raises verification difficulty and thus amplifies extraction modestly — while suppression enters unscaled as a raw structural property.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification guards both symmetrical errors. Calling this a pure rope would erase the documented asymmetric transfers — stranded assets, regional job destruction, regressive energy-price incidence — that require active enforcement to hold; calling it a snare would erase the genuine atmospheric-commons coordination function that no alternative arrangement currently performs at scale. The founding problem remains live (the emissions gap persists; thresholds approach), so no mandatrophy resolution is declared. The mismatch consumer should watch founding_problem_status=live against the rising theater series: if delivery permanently detaches from pledge while the machinery keeps expanding, the zombie-flag path opens and the arrangement drifts toward theatrically maintained inertia carrying an extractive load.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    response_kernel_reading_indexicality,
    'This story instantiates only the mitigation_priority reading of the climate_response_obligation kernel; what structurally changes under the sibling readings?',
    'Classify the sibling files (adaptation_priority, degrowth_reading) independently and compare per-seat classifications; the divergence locates the disagreement in beneficiary identity and burden allocation rather than in the obligation''s existence.',
    'Under adaptation_priority the primary beneficiaries become currently exposed populations, enforcement intensity drops, and fossil capital exits the victim set; under degrowth_reading the victim set widens to high-throughput consumption generally and the coordination target shifts from energy substitution to volume contraction. Cross-reading comparison, not revision of this file, resolves it.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(response_kernel_reading_indexicality, conceptual, 'Committer structure: this constraint is one of three readings of the response-obligation kernel.').

omega_variable(
    obligation_duty_vs_constructed_interest,
    'Is the mitigation obligation a science-grounded duty owed to future persons, or a constructed political commitment whose operative effect is to channel rents to organized green sectors?',
    'Compare delivery trajectories against receipt concentration: if warming outcomes track the physics while receipts stay plural and competitive, duty-grounding dominates; if receipts concentrate in a stable incumbent set while outcomes lag pledges, constructed-interest dominates.',
    'Duty-grounding supports the tangled-rope reading with extraction as the transitional price of coordination; constructed-interest dominance pushes the classification toward snare with the coordination story as cover.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(obligation_duty_vs_constructed_interest, conceptual, 'Whether the obligation''s warrant is intergenerational duty or green-sector rent protection.').

omega_variable(
    posthumous_benefit_receipt_verifiability,
    'Can a transfer whose recipient does not yet exist be verified as received, or is the future-generations benefit unfalsifiable cover for present redistribution?',
    'Instrumental proxies: observed warming trajectory against counterfactual no-policy baselines, and avoided-damage attribution studies; these verify the transfer''s reality without needing the recipient''s testimony.',
    'Verifiable receipt keeps future_generations a genuine beneficiary seat; persistent failure against counterfactual baselines demotes the seat to rhetorical and reweights the classification toward extraction-only.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(posthumous_benefit_receipt_verifiability, empirical, 'Whether the intergenerational transfer is verifiable or rhetorically asserted.').

omega_variable(
    historical_burden_weighting,
    'On what defensible weighting does the Global North owe a disproportionate mitigation burden — cumulative historical emissions, current per-capita emissions, capability, or some blend — and does any weighting survive political contest?',
    'No dataset settles it; resolution comes from negotiated formulae (finance quantification, operationalized differentiation) surviving successive negotiating rounds without collapse.',
    'A stable formula legitimizes the elevated extraction assigned to global_north_households; permanent contest destabilizes the burden-sharing premise and pushes the arrangement toward voluntary, under-enforced coordination.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(historical_burden_weighting, preference, 'The fairness premise underneath the differentiated-burden structure.').

omega_variable(
    stranded_asset_compensation_status,
    'Does stranding fossil assets constitute compensable taking or legitimate exposure to ordinary policy risk?',
    'Investor-state arbitration outcomes, domestic compensation statutes, and the emerging jurisprudence on climate-policy risk allocation.',
    'Compensable-taking rulings raise the arrangement''s fiscal extraction (public funds flowing to former asset holders) and soften the fossil-capital payer seat; risk-allocation rulings hold the seat''s losses uncompensated and sharpen the asymmetry.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(stranded_asset_compensation_status, preference, 'Whether the fossil-capital victim position carries a compensation claim.').

omega_variable(
    pledge_theater_transitional_vs_terminal,
    'Is the rising theater_ratio a transitional artifact of pledges preceding delivery in every infrastructure buildout, or terminal Goodhart drift in which the pledge layer replaces delivery?',
    'Decade-scale delivery-versus-pledge accounting: ratcheting national commitments converting into measured emission declines distinguishes transition from drift.',
    'Transitional theater leaves the coordination function intact beneath the noise; terminal drift with flat delivery would push the arrangement toward theatrically maintained inertia despite its extractive load.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(pledge_theater_transitional_vs_terminal, empirical, 'Whether the performative pledge layer is scaffolding for delivery or its replacement.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_response_obligation__mitigation_priority, 0, 28).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clim_tr_t0, climate_response_obligation__mitigation_priority, theater_ratio, 0, 0.22).
narrative_ontology:measurement_basis(clim_tr_t0, observed).
narrative_ontology:measurement(clim_tr_t4, climate_response_obligation__mitigation_priority, theater_ratio, 4, 0.26).
narrative_ontology:measurement_basis(clim_tr_t4, observed).
narrative_ontology:measurement(clim_tr_t8, climate_response_obligation__mitigation_priority, theater_ratio, 8, 0.31).
narrative_ontology:measurement_basis(clim_tr_t8, observed).
narrative_ontology:measurement(clim_tr_t12, climate_response_obligation__mitigation_priority, theater_ratio, 12, 0.38).
narrative_ontology:measurement_basis(clim_tr_t12, observed).
narrative_ontology:measurement(clim_tr_t16, climate_response_obligation__mitigation_priority, theater_ratio, 16, 0.44).
narrative_ontology:measurement_basis(clim_tr_t16, observed).
narrative_ontology:measurement(clim_tr_t20, climate_response_obligation__mitigation_priority, theater_ratio, 20, 0.49).
narrative_ontology:measurement_basis(clim_tr_t20, observed).
narrative_ontology:measurement(clim_tr_t24, climate_response_obligation__mitigation_priority, theater_ratio, 24, 0.53).
narrative_ontology:measurement_basis(clim_tr_t24, observed).
narrative_ontology:measurement(clim_tr_t28, climate_response_obligation__mitigation_priority, theater_ratio, 28, 0.55).
narrative_ontology:measurement_basis(clim_tr_t28, observed).

% Extraction over time
narrative_ontology:measurement(clim_be_t0, climate_response_obligation__mitigation_priority, base_extractiveness, 0, 0.34).
narrative_ontology:measurement_basis(clim_be_t0, observed).
narrative_ontology:measurement(clim_be_t4, climate_response_obligation__mitigation_priority, base_extractiveness, 4, 0.38).
narrative_ontology:measurement_basis(clim_be_t4, observed).
narrative_ontology:measurement(clim_be_t8, climate_response_obligation__mitigation_priority, base_extractiveness, 8, 0.42).
narrative_ontology:measurement_basis(clim_be_t8, observed).
narrative_ontology:measurement(clim_be_t12, climate_response_obligation__mitigation_priority, base_extractiveness, 12, 0.47).
narrative_ontology:measurement_basis(clim_be_t12, observed).
narrative_ontology:measurement(clim_be_t16, climate_response_obligation__mitigation_priority, base_extractiveness, 16, 0.52).
narrative_ontology:measurement_basis(clim_be_t16, observed).
narrative_ontology:measurement(clim_be_t20, climate_response_obligation__mitigation_priority, base_extractiveness, 20, 0.56).
narrative_ontology:measurement_basis(clim_be_t20, observed).
narrative_ontology:measurement(clim_be_t24, climate_response_obligation__mitigation_priority, base_extractiveness, 24, 0.6).
narrative_ontology:measurement_basis(clim_be_t24, observed).
narrative_ontology:measurement(clim_be_t28, climate_response_obligation__mitigation_priority, base_extractiveness, 28, 0.62).
narrative_ontology:measurement_basis(clim_be_t28, observed).

% Suppression requirement over time
narrative_ontology:measurement(clim_su_t0, climate_response_obligation__mitigation_priority, suppression_requirement, 0, 0.28).
narrative_ontology:measurement_basis(clim_su_t0, observed).
narrative_ontology:measurement(clim_su_t4, climate_response_obligation__mitigation_priority, suppression_requirement, 4, 0.33).
narrative_ontology:measurement_basis(clim_su_t4, observed).
narrative_ontology:measurement(clim_su_t8, climate_response_obligation__mitigation_priority, suppression_requirement, 8, 0.38).
narrative_ontology:measurement_basis(clim_su_t8, observed).
narrative_ontology:measurement(clim_su_t12, climate_response_obligation__mitigation_priority, suppression_requirement, 12, 0.44).
narrative_ontology:measurement_basis(clim_su_t12, observed).
narrative_ontology:measurement(clim_su_t16, climate_response_obligation__mitigation_priority, suppression_requirement, 16, 0.5).
narrative_ontology:measurement_basis(clim_su_t16, observed).
narrative_ontology:measurement(clim_su_t20, climate_response_obligation__mitigation_priority, suppression_requirement, 20, 0.54).
narrative_ontology:measurement_basis(clim_su_t20, observed).
narrative_ontology:measurement(clim_su_t24, climate_response_obligation__mitigation_priority, suppression_requirement, 24, 0.57).
narrative_ontology:measurement_basis(clim_su_t24, observed).
narrative_ontology:measurement(clim_su_t28, climate_response_obligation__mitigation_priority, suppression_requirement, 28, 0.58).
narrative_ontology:measurement_basis(clim_su_t28, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_response_obligation__mitigation_priority, resource_allocation).
narrative_ontology:affects_constraint(climate_response_obligation__mitigation_priority, climate_response_obligation__adaptation_priority).
narrative_ontology:affects_constraint(climate_response_obligation__mitigation_priority, climate_response_obligation__degrowth_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'climate response obligation' decomposes into three structurally distinct constraints, one per reading of the shared kernel. This file instantiates mitigation_priority (rapid decarbonization; future generations as primary beneficiaries; fossil capital entering the victim set via stranded assets). Its epsilon (0.62) is intrinsic to THIS reading's constraint and is not comparable to sibling epsilon values as measurements of one thing: adaptation_priority's arrangement extracts along resilience-spending lines with currently exposed populations as beneficiaries, and degrowth_reading's extracts along throughput lines with high-consumption lifestyles in the victim set. Downstream/upstream: mitigation codified temperature targets that now condition adaptation's damage-exposure premises (influences edge); degrowth remains a parallel live diagnosis held by other factions (coexists edge).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
