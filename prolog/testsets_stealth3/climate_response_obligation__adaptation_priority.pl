% ============================================================================
% CONSTRAINT STORY: climate_response_obligation__adaptation_priority
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_climate_response_obligation__adaptation_priority, []).

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
 *   constraint_id: climate_response_obligation__adaptation_priority
 *   human_readable: Adaptation-Priority Climate Settlement (2-3°C Inevitability Reading)
 *   domain: climate policy/political economy/intergenerational ethics
 *
 * SUMMARY:
 *   A political settlement — embodied in national adaptation plans, budget
 *   allocations, insurance retreat, and international negotiating positions —
 *   treats 2-3°C of warming as the fixed planning baseline and directs
 *   climate resources to resilience investment rather than prevention. Sea
 *   walls, hardened grids, heat-action plans, drought-tolerant agriculture,
 *   and managed-retreat programs are funded; prevention-scale carbon pricing,
 *   fossil-supply restrictions, and rapid transition mandates are declined as
 *   unrealistic. The settlement is administered by wealthy-nation
 *   governments, financed by present taxpayers, and its most concentrated
 *   material effect is the preservation of fossil asset values that a
 *   prevention-priority regime would strand. Its accepted warming lands on
 *   parties with no seat at the allocation — future generations and Global
 *   South populations — while its adaptation benefits concentrate in wealthy
 *   regions. Claim and metrics are independent authored facts: the
 *   arrangement is CLAIMED as tangled_rope because it couples a genuine
 *   coordination function (resilience against real physical risk) with
 *   asymmetric extraction (unpriced harm transferred to the unrepresented,
 *   asset values preserved); the metrics describe substantially extractive,
 *   actively enforced operation, and the engine computes per-seat types from
 *   the structural data.
 *
 * KEY AGENTS:
 *   - fossil_capital_owners: primary beneficiary (powerful/arbitrage) — asset values preserved by the settlement's refusal to strand them
 *   - current_generation_wealthy_taxpayers: primary beneficiary (moderate/constrained) — avoids present transition costs, receives adaptation benefits
 *   - wealthy_nation_governments: agenda setter (institutional/constrained, secondarily beneficiary) — administers the baseline, the budgets, and the framing
 *   - wealthy_nation_adaptation_industries: secondary beneficiary (organized/mobile) — collects the adaptation contract revenue
 *   - future_generations: primary target (powerless/trapped) — inherits the accepted warming without representation
 *   - global_south_populations: primary target (moderate/constrained) — bears the sharpest impacts with the least adaptation finance
 *   - frontline_coastal_communities: target (powerless/trapped) — first-impact bearers offered managed retreat
 *   - climate_advocacy_movements: excluded voice (organized/mobile) — contests the inevitability framing from outside the budget process
 *   - climate_science_community: analytical observer (institutional/analytical) — attests the warming level is policy-dependent
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_response_obligation__adaptation_priority, 0.7).
domain_priors:suppression_score(climate_response_obligation__adaptation_priority, 0.62).
domain_priors:theater_ratio(climate_response_obligation__adaptation_priority, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_response_obligation__adaptation_priority, extractiveness, 0.7).
narrative_ontology:constraint_metric(climate_response_obligation__adaptation_priority, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(climate_response_obligation__adaptation_priority, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(climate_response_obligation__adaptation_priority, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(climate_response_obligation__adaptation_priority, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_response_obligation__adaptation_priority, tangled_rope).
narrative_ontology:human_readable(climate_response_obligation__adaptation_priority, "Adaptation-Priority Climate Settlement (2-3°C Inevitability Reading)").
narrative_ontology:topic_domain(climate_response_obligation__adaptation_priority, "climate policy/political economy/intergenerational ethics").

domain_priors:requires_active_enforcement(climate_response_obligation__adaptation_priority).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_response_obligation__adaptation_priority, 'a598f6c5-fc5e-40a1-8eb8-cb9cfa6390d7').
narrative_ontology:cs_kernel_codification('a598f6c5-fc5e-40a1-8eb8-cb9cfa6390d7', distributed).
narrative_ontology:cs_authority_grounding('a598f6c5-fc5e-40a1-8eb8-cb9cfa6390d7', distributed).
narrative_ontology:cs_reading_relation('a598f6c5-fc5e-40a1-8eb8-cb9cfa6390d7', climate_response_obligation__mitigation_priority, coexists_with).
narrative_ontology:cs_reading_relation('a598f6c5-fc5e-40a1-8eb8-cb9cfa6390d7', climate_response_obligation__degrowth_reading, forecloses).
narrative_ontology:cs_axiom('a598f6c5-fc5e-40a1-8eb8-cb9cfa6390d7', foundational, warming_locked_in_beyond_affordable_prevention).
narrative_ontology:cs_axiom_status(warming_locked_in_beyond_affordable_prevention, holdable).
narrative_ontology:cs_axiom_grounding('a598f6c5-fc5e-40a1-8eb8-cb9cfa6390d7', warming_locked_in_beyond_affordable_prevention, empirically_contingent).
narrative_ontology:cs_axiom('a598f6c5-fc5e-40a1-8eb8-cb9cfa6390d7', foundational, present_resilience_over_prevention_cost).
narrative_ontology:cs_axiom_status(present_resilience_over_prevention_cost, holdable).
narrative_ontology:cs_axiom_grounding('a598f6c5-fc5e-40a1-8eb8-cb9cfa6390d7', present_resilience_over_prevention_cost, instrumental).
narrative_ontology:cs_axiom('a598f6c5-fc5e-40a1-8eb8-cb9cfa6390d7', secondary, future_climate_harm_market_discounting_permissible).
narrative_ontology:cs_axiom_status(future_climate_harm_market_discounting_permissible, holdable).
narrative_ontology:cs_axiom_grounding('a598f6c5-fc5e-40a1-8eb8-cb9cfa6390d7', future_climate_harm_market_discounting_permissible, conventional).
narrative_ontology:cs_reference_frame('a598f6c5-fc5e-40a1-8eb8-cb9cfa6390d7', realist_welfare_protection_settlement).
narrative_ontology:cs_drift_state('a598f6c5-fc5e-40a1-8eb8-cb9cfa6390d7', contemporary_clean_energy_cost_decline_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('a598f6c5-fc5e-40a1-8eb8-cb9cfa6390d7', '2026-08-05T12:00:00Z').
narrative_ontology:cs_kernel_id(climate_response_obligation__adaptation_priority, climate_response_obligation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_response_obligation__adaptation_priority, current_generation_wealthy_taxpayers).
narrative_ontology:constraint_beneficiary(climate_response_obligation__adaptation_priority, fossil_capital_owners).
narrative_ontology:constraint_beneficiary(climate_response_obligation__adaptation_priority, wealthy_nation_adaptation_industries).
narrative_ontology:constraint_victim(climate_response_obligation__adaptation_priority, future_generations).
narrative_ontology:constraint_victim(climate_response_obligation__adaptation_priority, global_south_populations).
narrative_ontology:constraint_victim(climate_response_obligation__adaptation_priority, frontline_coastal_communities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(climate_response_obligation__adaptation_priority, wealthy_nation_governments).
narrative_ontology:constraint_vindicates(climate_response_obligation__adaptation_priority, warming_inevitability_doctrine).
narrative_ontology:constraint_vindicates(climate_response_obligation__adaptation_priority, market_discount_rate_orthodoxy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Live in wealthy democracies whose governments fund resilience out of general revenue instead of imposing rapid transition costs. They avoid the carbon prices, energy disruption, and restructuring bills that prevention-first policy would present now, and their homes, insurers, ports, and grids receive adaptation spending. They also pay adaptation taxes and will live with the warming their governments accepted — a cost that arrives later, diffusely, and is easy to discount. Leaving would mean emigration or political exit, both expensive.
narrative_ontology:constraint_stakeholder(climate_response_obligation__adaptation_priority, current_generation_wealthy_taxpayers, beneficiary,
    moderate, biographical, constrained, national).

% Hold reserves, pipelines, refineries, and plants whose book value depends on continued fossil throughput. A prevention-first regime would strand a large share of those assets; the adaptation settlement keeps that scenario off the table, so valuations hold and returns continue. Capital can move across jurisdictions and into new holdings if any single regulator tightens. Their political spending and litigation are among the mechanisms that keep prevention mandates from reaching the agenda.
narrative_ontology:constraint_stakeholder(climate_response_obligation__adaptation_priority, fossil_capital_owners, beneficiary,
    powerful, biographical, arbitrage, global).

% Engineering, construction, insurance-adjacent, and climate-services firms that win the contracts for sea walls, hardened grids, cooled cities, flood defense, and resilient agriculture. Adaptation appropriations are their revenue line, so they lobby for larger resilience budgets and hold no comparable stake in prevention spending. They can follow contracts across borders.
narrative_ontology:constraint_stakeholder(climate_response_obligation__adaptation_priority, wealthy_nation_adaptation_industries, beneficiary,
    organized, biographical, mobile, global).

% Set the budgets that allocate between resilience and prevention, write the national adaptation plans, and hold the international position that treats 2-3°C as the planning baseline. The settlement suits their electoral horizon: its avoided costs are present while its accepted costs are deferred, diffuse, and borne largely by non-voters. They administer the enforcement — publishing inevitability-framed assessments, funding resilience, and declining prevention mandates as unrealistic.
narrative_ontology:constraint_stakeholder(climate_response_obligation__adaptation_priority, wealthy_nation_governments, agenda_setter,
    institutional, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(climate_response_obligation__adaptation_priority, wealthy_nation_governments, beneficiary).

% Will inhabit the 2-3°C world the settlement accepted, bearing intensified heat, higher seas, storm damage, crop stress, and ecological loss, without any seat in the legislatures or markets where the allocation was made. Their present leverage runs only through advocacy proxies, courts, and the consciences of current voters. There is no exit: they inherit the outcome wherever they are born.
narrative_ontology:constraint_stakeholder(climate_response_obligation__adaptation_priority, future_generations, payer,
    powerless, civilizational, trapped, global).

% Live mostly in low-latitude regions where the accepted warming produces the sharpest agricultural, heat-mortality, and sea-level harms, while global adaptation finance reaches them in small fractions. They bear impacts produced overwhelmingly by wealthy-nation emissions and preserved by the settlement's refusal to prevent. Organized through negotiating coalitions and the loss-and-damage process, they can win pledges but not the prevention the settlement declines; migration toward wealthier regions is blocked by border regimes.
narrative_ontology:constraint_stakeholder(climate_response_obligation__adaptation_priority, global_south_populations, payer,
    moderate, biographical, constrained, global).

% Occupy deltas, small islands, and low-lying coasts where the accepted warming converts directly into inundation, salinization, and storm loss. Resilience investment reaches them last and least; the plan offered to many is managed retreat. Their land, fisheries, and burial grounds are the collateral of the arrangement. Moving means abandoning livelihood and community against compensation that rarely arrives in full.
narrative_ontology:constraint_stakeholder(climate_response_obligation__adaptation_priority, frontline_coastal_communities, payer,
    powerless, biographical, trapped, regional).

% Youth movements, environmental organizations, and litigation networks that argue the inevitability framing is a choice dressed as fate and that prevention remains available at falling cost. They stand outside the budget process that fixes the baseline; their access runs through protest, courts, and shareholder action, which the settlement's institutions treat as noise around a settled plan.
narrative_ontology:constraint_stakeholder(climate_response_obligation__adaptation_priority, climate_advocacy_movements, excluded,
    organized, biographical, mobile, global).

% Produces the assessments the settlement cites — and complicates them: the same literature shows the warming level is policy-dependent, that each year of resilience-first delay raises the later adaptation bill, and that hard adaptation limits exist above which protection fails regardless of spending. It attests from outside the beneficiary seats and holds no allocation power.
narrative_ontology:constraint_stakeholder(climate_response_obligation__adaptation_priority, climate_science_community, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(climate_response_obligation__adaptation_priority, fossil_capital_owners).
narrative_ontology:fixing_cost_class(climate_response_obligation__adaptation_priority, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves two real coordination problems at once: it organizes resilience investment (defenses, hardened infrastructure, adjusted agriculture, early-warning systems) against warming the settlement treats as coming regardless, and it organizes a political truce — a shared baseline that lets budgeting proceed without re-litigating the mitigation fight every fiscal year.
% TRANSFER_FUNCTION: Moves fiscal resources from present taxpayers in wealthy nations into adaptation projects concentrated in wealthy regions; moves unpriced climate harm from the present generation of fossil-fuel users to future generations and to Global South populations; and preserves the book value of fossil assets that a prevention-priority regime would strand.
% ABSENT_VOICES: Future generations are absent by construction — no seat, no proxy with standing. Global South populations and small island states are present in negotiations but structurally outvoted and under-financed. Climate advocacy movements and prevention-oriented economists are excluded from the budget frame that treats the warming baseline as fixed; they would object that the baseline is a policy output, not an input.
% DISAPPEARANCE_RATIONALE: If the settlement vanished overnight, the 2-3°C planning baseline would lose its administrative force: prevention mandates would return to the legislative agenda, fossil asset valuations would reprice against stranding risk, adaptation budgets would be contested against prevention spending, and international finance negotiations would reopen around liability for harm already locked in. The arrangement is load-bearing for fossil balance sheets and for wealthy-nation fiscal comfort — its disappearance rearranges both.
% FOUNDING_PROBLEM: The settlement was assembled to solve the problem that prevention looked politically impossible and economically disruptive: rapid decarbonization threatened concentrated, immediate losses to voters and donors, so the arrangement was built to deliver a climate response that protects present welfare against warming treated as unavoidable.
% FOUNDING_PROBLEM_CORROBORATION: Proponents (finance ministries, adaptation agencies, fossil-sector commentators) attest the feasibility constraint is still live. Corroboration from outside the beneficiary set cuts the other way: the IPCC and national science academies attest that the warming trajectory is policy-dependent — the inevitability premise is an output of choices like this one, not an independent fact — while Global South negotiating blocs and climate-litigation courts attest that the arrangement externalizes its costs onto parties who never accepted them. No source outside the beneficiary set attests that prevention is unavailable; the availability claim is attested only by the seats the settlement protects.
narrative_ontology:disappearance_verdict(climate_response_obligation__adaptation_priority, world_rearranges).
narrative_ontology:founding_problem_status(climate_response_obligation__adaptation_priority, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(climate_response_obligation__adaptation_priority, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(climate_response_obligation__adaptation_priority, 'none', 1).
narrative_ontology:epsilon_provenance(climate_response_obligation__adaptation_priority, 0.7, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(climate_response_obligation__adaptation_priority_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(climate_response_obligation__adaptation_priority, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(climate_response_obligation__adaptation_priority_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction (0.70) is high because the settlement's central operation is the transfer of unpriced harm to seats with zero market and political weight: the warming it accepts is produced by continued fossil throughput the settlement protects, and the harm lands on future generations and Global South populations who neither priced it nor consented to it. Suppression (0.62) is real but non-coercive in form: enforcement runs through framing (inevitability as administrative baseline), lobbying, budget discipline, and the treatment of prevention mandates as unrealistic — alternatives are starved, not banned, so they remain visible (accessibility_collapse 0.42, low for a substantially extractive constraint) while being inadmissible inside the allocation process. Resistance (0.60) is substantial and rising — litigation wins, youth movements, Global South bloc pressure — which is why the suppression_requirement series rises across the interval: the settlement needed more active defense as its premise came under challenge, not less. Theater (0.38): resilience spending is real, but a growing share of activity is strategy documents and announcements that outpace delivered protection, and the inevitability framing itself performs fate in order to foreclose choice. All three metric series run on one shared seven-point grid (t=0..24) so no metric's end-state value is substituted into earlier periods. The receipt surface names fossil_capital_owners as gain_flow because the settlement's largest concentrated, demonstrable gain pool is the preserved valuation of fossil assets; taxpayer and industry receipts are real but diffuse and appropriation-contingent. fixing_cost is authored 'prohibitive' because the seat that could fix the settlement — the wealthy-nation governments — would bear concentrated, immediate electoral and fiscal costs that exceed, from that seat, the deferred and diffuse benefit; the cost class is seat-relative and does not demote the capture named in gain_flow.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats and the agenda-setter/beneficiary seats should compute different types from the same structure. From the wealthy-nation government seat, the settlement is prudent administration: a physical given managed at least cost, with adaptation delivering visible protection to voters. From the current-taxpayer seat it is a bargain trading deferred, diffuse costs for present comfort. From the future-generation and Global South seats the identical structure operates as harm transfer to the unrepresented — a regime that spends real money protecting the seats that chose it while externalizing the accepted warming onto seats that could not refuse. Same-power divergence matters: global_south_populations and current_generation_wealthy_taxpayers both hold 'moderate' power, but adaptation-finance allocation gives the wealthy-nation seat resilience investment and the Global South seat managed-retreat plans — the constraint, not global standing, differentiates their exit options. Inter-institutionally, wealthy-nation and Global South governments are nominally the same kind of actor with opposite exposure and finance access. Coalition capacity differentiates the powerless seats: frontline_coastal_communities can align with Global South negotiating blocs and litigation networks, while future_generations hold no coalition capacity at all — their only present leverage is fiduciary litigation brought by current minors and advocacy proxies. The engine computes per-seat classifications from the structural data; the authored claim does not adjudicate the divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations map to the low-d end: fossil_capital_owners sit nearest d=0 — the settlement's distinctive material effect is preserving their asset values, and their arbitrage-grade exit pushes them further toward the beneficiary pole. current_generation_wealthy_taxpayers are low-d but not zero: they also pay adaptation taxes and live with residual warming, damping their subsidy. wealthy_nation_adaptation_industries collect contract revenue but hold only mobile, appropriation-contingent benefit. Victim declarations map to the high-d end: future_generations sit nearest d=1 — trapped, unrepresented, pure cost-bearers with civilizational exposure. global_south_populations are near the target pole with slight damping from adaptation-finance receipts they receive but do not control. frontline_coastal_communities are near d=1 with regional scope and no exit. The wealthy_nation_governments seat is the agenda-setter and derives its position from avoided present costs — structurally near the beneficiary end despite administering rather than collecting. No directionality overrides are authored: the derivation from role, power, and exit data produces the right d for every seat, and the override mechanism is keyed by power atom, which cannot separate the two 'moderate' seats that need opposite directions. Suppression is authored as a raw structural property and is not scaled by power or scope; extractiveness is scaled — the global scope of the harm transfer and the trapped exit of its targets amplify effective extraction for the victim seats.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled_rope classification prevents two mislabelings. A pure snare reading would erase the genuine coordination: physical warming risk is real under any allocation, and the settlement does deliver sea defenses, hardened grids, and early-warning systems that solve a real collective-action problem for the seats it protects. A pure rope reading would erase the asymmetry: the same budget line that builds the sea wall also settles the baseline that protects fossil asset values and defers the accepted warming onto parties with no seat. The hybrid is exact — coordination function and extraction ride one structure and require its active enforcement (framing, lobbying, budget discipline) to hold. No mandatrophy resolution is declared: the founding problem (present-cost avoidance under assumed inevitability) is contested rather than dead, and the arrangement's functions are still actively performed — the R5 mismatch check (status=contested × verdict=world_rearranges) stays unflagged, and theater_ratio (0.38) remains below the Goodhart drift threshold even though it rises across the interval.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    inevitability_endogeneity,
    'Is the 2-3°C warming premise an independent physical fact, or is it partly produced by the adaptation-priority choice itself?',
    'Attribution comparison of warming trajectories under adaptation-first versus prevention-first policy counterfactuals, updated against observed emissions and adopted policy each cycle.',
    'If substantially endogenous, the reading''s foundational axiom is self-fulfilling — the settlement manufactures the inevitability it cites — and the extraction profile deepens, since the harm transferred to future generations is an output of the arrangement rather than an input to it.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(inevitability_endogeneity, empirical, 'Whether the inevitability premise is exogenous physics or a policy output.').

omega_variable(
    discount_rate_ethics,
    'Does the settlement''s implicit discounting of future climate harm reflect a defensible ethical position or a convention that enables transfer of harm to the unrepresented?',
    'Intergenerational-ethics analysis and sensitivity testing of policy rankings across discount-rate assumptions; legislative and judicial treatment of future-persons standing.',
    'Near-zero discounting reclassifies most of the adaptation-over-prevention arithmetic as extraction from future seats; market-rate discounting sustains the welfare-protection framing.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(discount_rate_ethics, conceptual, 'Ethical status of the discount rate underwriting the priority ordering.').

omega_variable(
    adaptation_delivery_gap,
    'Can adaptation investment actually deliver the protection the settlement promises at 2-3°C, or does the resilience promise exceed what adaptation can physically and fiscally deliver?',
    'Audit of announced versus delivered adaptation projects; assessment of hard adaptation limits (wet-bulb temperature exposure, sea-level commitment, agricultural thresholds) against the accepted warming range.',
    'If adaptation cannot deliver at 2-3°C, the settlement''s coordination function fails on its own terms and the arrangement collapses toward pure harm transfer with theatrical resilience.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(adaptation_delivery_gap, empirical, 'Whether the resilience promise is physically deliverable at the accepted warming level.').

omega_variable(
    kernel_reading_underdetermination,
    'This constraint is one reading (adaptation_priority) of the kernel climate_response_obligation; what fixed the reading choice, and what would the sibling readings change structurally?',
    'The choice was fixed by the manifest''s structural delta: current generation as primary beneficiary, future generations and Global South as victims, fossil capital protected, adaptation investment concentrated in wealthy regions. A sibling reading (mitigation_priority) would move fossil capital and present consumers into the victim set and future generations toward the beneficiary set, inverting the directionality profile; degrowth_reading would add present throughput consumers as targets and dissolve the fossil-protection function entirely.',
    'Victim and beneficiary sets, epsilon, and per-seat classifications are reading-indexed; adopting a sibling reading produces a different constraint with a different classification from the same kernel.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_underdetermination, conceptual, 'Committer structure: reading-indexed classification of the climate_response_obligation kernel.').

omega_variable(
    suppression_mechanism_composition,
    'Is the settlement''s suppression of prevention alternatives primarily structural (fossil capital''s political power, budget lock-in, administrative baseline) or internalized (public belief that warming is simply fate)?',
    'Track whether prevention policy re-expands in jurisdictions where structural blockers are removed (litigation wins, subsidy shifts, coalition changes) — if prevention stays politically dormant where the blockers fell, part of the suppression is internalized.',
    'If internalized, effective suppression exceeds the structural measure and outlasts the enforcement machinery; the settlement persists even if fossil political power collapses.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_composition, empirical, 'Structural versus internalized composition of the settlement''s suppressive force.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_response_obligation__adaptation_priority, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(adaptation_priority_tr_t0, climate_response_obligation__adaptation_priority, theater_ratio, 0, 0.22).
narrative_ontology:measurement(adaptation_priority_tr_t4, climate_response_obligation__adaptation_priority, theater_ratio, 4, 0.25).
narrative_ontology:measurement(adaptation_priority_tr_t8, climate_response_obligation__adaptation_priority, theater_ratio, 8, 0.28).
narrative_ontology:measurement(adaptation_priority_tr_t12, climate_response_obligation__adaptation_priority, theater_ratio, 12, 0.3).
narrative_ontology:measurement(adaptation_priority_tr_t16, climate_response_obligation__adaptation_priority, theater_ratio, 16, 0.33).
narrative_ontology:measurement(adaptation_priority_tr_t20, climate_response_obligation__adaptation_priority, theater_ratio, 20, 0.36).
narrative_ontology:measurement(adaptation_priority_tr_t24, climate_response_obligation__adaptation_priority, theater_ratio, 24, 0.38).

% Extraction over time
narrative_ontology:measurement(adaptation_priority_be_t0, climate_response_obligation__adaptation_priority, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(adaptation_priority_be_t4, climate_response_obligation__adaptation_priority, base_extractiveness, 4, 0.58).
narrative_ontology:measurement(adaptation_priority_be_t8, climate_response_obligation__adaptation_priority, base_extractiveness, 8, 0.61).
narrative_ontology:measurement(adaptation_priority_be_t12, climate_response_obligation__adaptation_priority, base_extractiveness, 12, 0.64).
narrative_ontology:measurement(adaptation_priority_be_t16, climate_response_obligation__adaptation_priority, base_extractiveness, 16, 0.66).
narrative_ontology:measurement(adaptation_priority_be_t20, climate_response_obligation__adaptation_priority, base_extractiveness, 20, 0.68).
narrative_ontology:measurement(adaptation_priority_be_t24, climate_response_obligation__adaptation_priority, base_extractiveness, 24, 0.7).

% Suppression requirement over time
narrative_ontology:measurement(adaptation_priority_su_t0, climate_response_obligation__adaptation_priority, suppression_requirement, 0, 0.48).
narrative_ontology:measurement(adaptation_priority_su_t4, climate_response_obligation__adaptation_priority, suppression_requirement, 4, 0.51).
narrative_ontology:measurement(adaptation_priority_su_t8, climate_response_obligation__adaptation_priority, suppression_requirement, 8, 0.53).
narrative_ontology:measurement(adaptation_priority_su_t12, climate_response_obligation__adaptation_priority, suppression_requirement, 12, 0.56).
narrative_ontology:measurement(adaptation_priority_su_t16, climate_response_obligation__adaptation_priority, suppression_requirement, 16, 0.58).
narrative_ontology:measurement(adaptation_priority_su_t20, climate_response_obligation__adaptation_priority, suppression_requirement, 20, 0.6).
narrative_ontology:measurement(adaptation_priority_su_t24, climate_response_obligation__adaptation_priority, suppression_requirement, 24, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_response_obligation__adaptation_priority, resource_allocation).
narrative_ontology:affects_constraint(climate_response_obligation__adaptation_priority, climate_response_obligation__mitigation_priority).
narrative_ontology:affects_constraint(climate_response_obligation__adaptation_priority, climate_response_obligation__degrowth_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'climate policy' covers structurally distinct commitments that this corpus decomposes into sibling readings of one kernel (climate_response_obligation): this adaptation_priority story (accept 2-3°C, resilience investment, fossil assets preserved), the mitigation_priority story (prevention as intergenerational duty — fossil capital and present consumers enter the victim set), and the degrowth_reading story (throughput contraction — the victim set shifts to present throughput consumers and the coordination function becomes sufficiency provisioning). The epsilon values differ because the readings allocate the same physical warming to different payer seats; they are one constraint family, linked here, not one constraint with a measurement parameter. This reading structurally pressures both siblings: its budget allocations crowd out mitigation spending and its inevitability framing erodes the legitimacy conditions under which the sibling readings are argued.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
