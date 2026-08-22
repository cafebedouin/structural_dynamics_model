% ============================================================================
% CONSTRAINT STORY: climate_response_imperative__mitigation_priority_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_climate_response_imperative__mitigation_priority_reading, []).

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
 *   constraint_id: climate_response_imperative__mitigation_priority_reading
 *   human_readable: Climate Response Imperative — Mitigation-Priority Reading (Innovation and Markets First, Adaptation Residual)
 *   domain: environmental/political-economic/intergenerational
 *
 * SUMMARY:
 *   The mitigation-priority reading organizes climate response around
 *   emissions reduction pursued through technological innovation and market
 *   mechanisms — carbon pricing, offset markets, subsidized clean-tech
 *   deployment — with adaptation assigned whatever attention and finance
 *   remain. The arrangement solves a real coordination problem (comparable
 *   global abatement accounting and price-guided effort allocation) while
 *   systematically deferring costs onto parties with no exit and no vote:
 *   future generations inherit compounded warming and unbuilt defenses;
 *   exposed regions receive adaptation as a residual budget line. Reliance on
 *   carbon removal technologies that exist mostly as projections converts
 *   present-day under-delivery into a promissory asset. This story is ONE
 *   READING of the climate_response_imperative kernel (see
 *   commentary.kernel_context and the omega variables): the ε referent is the
 *   mitigation-priority arrangement itself as this reading's structural data
 *   describes it — not the adaptation-priority or degrowth arrangements,
 *   which are separate constraint stories linked through the network. The
 *   claim/metric relationship is deliberately unreconciled: the frame
 *   presents itself as straightforward coordination; the authored metrics
 *   describe coordination carrying substantial asymmetric extraction.
 *
 * KEY AGENTS:
 *   - global_north_innovation_sectors: Primary beneficiary (powerful/arbitrage) — captures subsidy, credit, and mandate-driven investment flows
 *   - carbon_market_intermediaries: Secondary beneficiary (organized/arbitrage) — collects fees on every transacted tonne
 *   - fossil_incumbent_emitters: Dual-positioned beneficiary/payer (powerful/arbitrage) — pays compliance costs, collects the larger gain of deferral
 *   - future_generations: Primary target (powerless/trapped, universal scope) — inherits deferred mitigation and unbuilt adaptation
 *   - frontline_vulnerable_regions: Primary target (organized/trapped) — bears residualized adaptation costs today
 *   - cop_presidency_and_secretariat: Agenda setter (institutional/constrained) — controls what reaches decision text
 *   - multilateral_climate_funds: Allocation administrator (institutional/constrained) — rations the residual adaptation finance
 *   - climate_justice_movements: Excluded claimant (organized/constrained) — parity and liability demands sit outside the frame
 *   - ipcc_assessment_body: Analytical observer (institutional/analytical) — documents the adaptation gap and CDR limits
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_response_imperative__mitigation_priority_reading, 0.58).
domain_priors:suppression_score(climate_response_imperative__mitigation_priority_reading, 0.6).
domain_priors:theater_ratio(climate_response_imperative__mitigation_priority_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_response_imperative__mitigation_priority_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(climate_response_imperative__mitigation_priority_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(climate_response_imperative__mitigation_priority_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(climate_response_imperative__mitigation_priority_reading, accessibility_collapse, 0.52).
narrative_ontology:constraint_metric(climate_response_imperative__mitigation_priority_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_response_imperative__mitigation_priority_reading, tangled_rope).
narrative_ontology:human_readable(climate_response_imperative__mitigation_priority_reading, "Climate Response Imperative — Mitigation-Priority Reading (Innovation and Markets First, Adaptation Residual)").
narrative_ontology:topic_domain(climate_response_imperative__mitigation_priority_reading, "environmental/political-economic/intergenerational").

domain_priors:requires_active_enforcement(climate_response_imperative__mitigation_priority_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_response_imperative__mitigation_priority_reading, '73f5747b-ba37-4326-91ff-27fe7bafa549').
narrative_ontology:cs_kernel_codification('73f5747b-ba37-4326-91ff-27fe7bafa549', formalized).
narrative_ontology:cs_authority_grounding('73f5747b-ba37-4326-91ff-27fe7bafa549', lineage).
narrative_ontology:cs_interpretation_layer_present('73f5747b-ba37-4326-91ff-27fe7bafa549').
narrative_ontology:cs_reading_relation('73f5747b-ba37-4326-91ff-27fe7bafa549', climate_response_imperative__adaptation_priority_reading, coexists_with).
narrative_ontology:cs_reading_relation('73f5747b-ba37-4326-91ff-27fe7bafa549', climate_response_imperative__degrowth_reading, forecloses).
narrative_ontology:cs_axiom('73f5747b-ba37-4326-91ff-27fe7bafa549', foundational, growth_compatible_decarbonization_sufficiency).
narrative_ontology:cs_axiom_status(growth_compatible_decarbonization_sufficiency, holdable).
narrative_ontology:cs_axiom_grounding('73f5747b-ba37-4326-91ff-27fe7bafa549', growth_compatible_decarbonization_sufficiency, empirically_contingent).
narrative_ontology:cs_axiom('73f5747b-ba37-4326-91ff-27fe7bafa549', foundational, adaptation_as_residual_claim_on_surplus).
narrative_ontology:cs_axiom_status(adaptation_as_residual_claim_on_surplus, holdable).
narrative_ontology:cs_axiom_grounding('73f5747b-ba37-4326-91ff-27fe7bafa549', adaptation_as_residual_claim_on_surplus, instrumental).
narrative_ontology:cs_axiom('73f5747b-ba37-4326-91ff-27fe7bafa549', secondary, carbon_price_signal_allocative_efficiency).
narrative_ontology:cs_axiom_status(carbon_price_signal_allocative_efficiency, holdable).
narrative_ontology:cs_axiom_grounding('73f5747b-ba37-4326-91ff-27fe7bafa549', carbon_price_signal_allocative_efficiency, instrumental).
narrative_ontology:cs_reference_frame('73f5747b-ba37-4326-91ff-27fe7bafa549', growth_compatible_technological_transition).
narrative_ontology:cs_drift_state('73f5747b-ba37-4326-91ff-27fe7bafa549', first_global_stocktake_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('73f5747b-ba37-4326-91ff-27fe7bafa549', '').
narrative_ontology:cs_kernel_id(climate_response_imperative__mitigation_priority_reading, climate_response_imperative).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_response_imperative__mitigation_priority_reading, global_north_innovation_sectors).
narrative_ontology:constraint_beneficiary(climate_response_imperative__mitigation_priority_reading, carbon_market_intermediaries).
narrative_ontology:constraint_beneficiary(climate_response_imperative__mitigation_priority_reading, fossil_incumbent_emitters).
narrative_ontology:constraint_victim(climate_response_imperative__mitigation_priority_reading, future_generations).
narrative_ontology:constraint_victim(climate_response_imperative__mitigation_priority_reading, frontline_vulnerable_regions).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(climate_response_imperative__mitigation_priority_reading, fossil_incumbent_emitters).
narrative_ontology:constraint_vindicates(climate_response_imperative__mitigation_priority_reading, green_growth_compatibility_thesis).
narrative_ontology:constraint_vindicates(climate_response_imperative__mitigation_priority_reading, carbon_pricing_efficiency_hypothesis).
narrative_ontology:constraint_vindicates(climate_response_imperative__mitigation_priority_reading, technological_optimism_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Manufactures renewable generation, electric vehicles, heat pumps, and develops carbon removal ventures. Captures production tax credits, deployment subsidies, concessional climate finance, and a growing share of institutional investment steered by net-zero portfolio mandates. Revenue scales with total mitigation spending whether or not aggregate targets are met, and capital can be redirected across sectors and jurisdictions if any single support regime weakens.
narrative_ontology:constraint_stakeholder(climate_response_imperative__mitigation_priority_reading, global_north_innovation_sectors, beneficiary,
    powerful, biographical, arbitrage, global).

% Operates offset registries, verification bodies, trading desks, and brokerage services that collect registration fees, issuance commissions, and bid-ask spreads on every transacted tonne. Income is a function of transaction volume and rule complexity; when one standard or jurisdiction tightens its rules, activity migrates to another.
narrative_ontology:constraint_stakeholder(climate_response_imperative__mitigation_priority_reading, carbon_market_intermediaries, beneficiary,
    organized, biographical, arbitrage, global).

% Pays compliance costs — carbon prices, allowance purchases, offset retirements — under the arrangement, yet draws its larger gain from the same frame: because climate response is constituted as a market purchase rather than a supply-side constraint, extraction licenses remain valid through a transition measured in decades. Costs can be passed to consumers, production relocated to unpriced jurisdictions, and depleted assets converted into offset-generation projects.
narrative_ontology:constraint_stakeholder(climate_response_imperative__mitigation_priority_reading, fossil_incumbent_emitters, beneficiary,
    powerful, biographical, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(climate_response_imperative__mitigation_priority_reading, fossil_incumbent_emitters, payer).

% Inherits both the compounded warming from ambition deferred to later, cheaper technological fixes and the coastal, drought, and heat defenses that were never built while adaptation waited on residual budget lines. Holds no seat in any negotiation; appears only through ombudsperson litigation and proxy advocacy conducted by present-day actors with their own agendas. Exit is not merely difficult — it is undefined.
narrative_ontology:constraint_stakeholder(climate_response_imperative__mitigation_priority_reading, future_generations, payer,
    powerless, civilizational, trapped, universal).

% Small island states, deltaic and dryland populations whose adaptation needs are funded after mitigation allocations are met, leaving chronic gaps between assessed needs and delivered finance. Organizes effectively as negotiating blocs but holds little fiscal leverage; territory cannot be relocated, and adaptation-finance dependence deepens the entrapment with each approved project.
narrative_ontology:constraint_stakeholder(climate_response_imperative__mitigation_priority_reading, frontline_vulnerable_regions, payer,
    organized, biographical, trapped, global).

% Controls which items reach decision text at annual conferences, staffs the transparency and MRV machinery that operationalizes mitigation-first accounting, and manages the consensus rule that gives every major emitter a veto over reordering priorities. Its administrative continuity depends on the existing frame; proposing adaptation parity or liability regimes as co-equal pillars would require rebuilding the process it runs.
narrative_ontology:constraint_stakeholder(climate_response_imperative__mitigation_priority_reading, cop_presidency_and_secretariat, agenda_setter,
    institutional, generational, constrained, global).

% Rations adaptation finance that arrives as the residue of mitigation-weighted replenishments. Approval pipelines, co-financing requirements, and results frameworks steer recipient governments toward market-aligned projects (renewables auctions, crediting schemes) over public-works resilience that generates no tradable asset.
narrative_ontology:constraint_stakeholder(climate_response_imperative__mitigation_priority_reading, multilateral_climate_funds, agenda_setter,
    institutional, generational, constrained, global).

% Demands adaptation parity, loss-and-damage liability, and structural transformation of Northern consumption. Present at the conference margins as accredited observers and street pressure, with procedural voice but no decision rights; their core claims sit outside the operative frame that defines what counts as a climate measure at all.
narrative_ontology:constraint_stakeholder(climate_response_imperative__mitigation_priority_reading, climate_justice_movements, excluded,
    organized, biographical, constrained, global).

% Produces the assessment cycles that document the widening adaptation gap, the mitigation implementation shortfall, and the scalability limits of carbon removal. Its summaries furnish the evidentiary record that every other seat litigates over; it collects nothing and pays nothing under the arrangement.
narrative_ontology:constraint_stakeholder(climate_response_imperative__mitigation_priority_reading, ipcc_assessment_body, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(climate_response_imperative__mitigation_priority_reading, global_north_innovation_sectors).
narrative_ontology:fixing_cost_class(climate_response_imperative__mitigation_priority_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the global collective-action problem of decarbonization by creating a common metric (the traded tonne of CO2e), price signals that route abatement to cheapest locations, and technology deployment pipelines that coordinate national pledges into comparable, auditable frameworks.
% TRANSFER_FUNCTION: Moves public subsidy, carbon revenue, and investment capital toward Global North innovation sectors and offset intermediaries; moves the costs of climate response onto future generations (deferred ambition, unbuilt defenses) and onto exposed regions today (residualized adaptation finance, uncompensated loss and damage).
% ABSENT_VOICES: Future generations are absent by construction and appear only through proxies. Frontline communities attend as petitioners whose procedural weight is a fraction of their financial stake. Adaptation-first and degrowth advocates are present in the room but their premises are classified as outside the frame — they object to the question's wording, not just its answer, and the agenda-setting machinery never grants that objection decision status.
% DISAPPEARANCE_RATIONALE: If the mitigation-priority frame vanished overnight, compliance carbon markets and their intermediary fee streams would dissolve, net-zero portfolio mandates would lose their accounting object, the subsidy architecture feeding innovation sectors would be renegotiated around different pillars, and adaptation finance would rebalance from residual to co-equal — the entire governance economy built on the tonne-as-unit would reorganize.
% FOUNDING_PROBLEM: How can a global emissions response be organized that does not require halting Northern economic growth — a politically feasible path that converts climate response into investable market activity and technological procurement rather than structural sacrifice.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: IPCC assessment reports and the UNEP Adaptation Gap Report (analytical seats) document both the reality of the coordination achievement and the widening adaptation gap that the residual treatment produces; AOSIS and G77 negotiating statements attest the cost-shifting from the payer seats. The frame's own account — that feasibility required this design — is attested principally by its beneficiaries and administrators; no independent source confirms that the growth-compatibility premise was ever empirically secured rather than politically convenient.
narrative_ontology:disappearance_verdict(climate_response_imperative__mitigation_priority_reading, world_rearranges).
narrative_ontology:founding_problem_status(climate_response_imperative__mitigation_priority_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(climate_response_imperative__mitigation_priority_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(climate_response_imperative__mitigation_priority_reading, 'none', 1).
narrative_ontology:epsilon_provenance(climate_response_imperative__mitigation_priority_reading, 0.58, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(climate_response_imperative__mitigation_priority_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(climate_response_imperative__mitigation_priority_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(climate_response_imperative__mitigation_priority_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.58: the transfer machinery is real and large, but roughly half its volume routes to genuine abatement activity, keeping ε below snare territory while well above rope overhead. Suppression is 0.60 and reflects agenda suppression rather than physical coercion — consensus vetoes, agenda control, and the definitional power that classifies adaptation-first proposals as outside the frame. Theater ratio 0.48: offset quality scandals, pledge-without-plan net-zero commitments, and CDR projections booked as achieved tonnage inflate the performative share, though real deployment proceeds alongside. Accessibility collapse 0.52: within mainstream policy discourse the frame crowds out rivals, but adaptation-first and degrowth positions persist institutionally at the margins, so alternatives are suppressed rather than unthinkable. Resistance 0.60: vulnerable-country blocs, climate justice movements, and periodic finance-parity fights impose real friction. Suppression is authored as a raw structural property and is NOT scaled by power or scope in the engine's arithmetic — only extractiveness is scaled, by directionality and spatial scope; the universal scope of the future-generations seat therefore amplifies effective extraction on that seat specifically. The measurement series run on one shared time grid (t=0..30 at stride 5) with every tracked metric authored at every point; trajectories are monotonic rather than cyclical — annual COP pledge spikes wash out into the underlying trend, so no intermittent-reinforcement dynamic is claimed.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute divergent types from identical structural data. From the innovation-sector and intermediary seats the arrangement is the coordination success it built: common metrics, functioning price signals, deployable capital — a rope with overhead. From the future-generations seat — powerless, trapped, universal scope — the same structure computes as near-full-target extraction with no exit whatsoever. The frontline-regions seat computes heavy extraction moderated by its organized bloc power. The agenda-setter seat sits intermediate: it experiences the frame as administrable reality while bearing the consensus constraint that prevents it from reordering priorities. The fossil-incumbent seat is the sharpest divergence case: it pays compliance costs (payer signal) while collecting the larger gain of deferral (beneficiary signal), and no single-seat classification captures both halves.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive innovation sectors and intermediaries toward the beneficiary end (low d, damped or inverted effective extraction). Future generations derive maximum target directionality: victim declaration, powerless power atom, trapped exit, universal scope — the engine's amplifiers all align. Frontline vulnerable regions derive high d from victim status and trapped exit, moderated somewhat by organized power. The fossil incumbent is the case where the automatic derivation is weakest: its beneficiary listing pulls d low, its payment flows push d up, and its net position depends on the unresolved delay-effect question (see omega market_frame_delay_effect). No directionality_override is authored because the override mechanism keys on power atoms, and an override at the 'powerful' level would distort the innovation sectors' cleanly beneficiary-derived d along with the incumbent's mixed one; the ambiguity is routed to the omega instead, which is where it belongs until resolved.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled_rope claim guards against both symmetric misreadings. Reading the arrangement as pure rope launders the extraction: the deferred-adaptation structure is not coordination overhead but its principal output for the seats that cannot refuse it. Reading it as pure snare erases the genuine coordination achievement — common accounting, price-guided abatement, real deployment — that any replacement must preserve. The mandatrophy-relevant risk is forward-looking: founding_problem_status is contested and disappearance_verdict is world_rearranges, so the mismatch consumer should watch for the dead-problem signature — if decarbonization is achieved or collectively abandoned while the market apparatus persists as self-perpetuating administration, the arrangement completes the drift from tangled_rope toward piton, with theater_ratio already trending toward the 0.5 threshold that marks proxy-goal displacement (Goodhart drift from tonnes-abated toward tonnes-traded).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indexicality,
    'This constraint is one reading of the climate_response_imperative kernel — how would the adaptation-priority or degrowth reading restructure the victim set, beneficiary set, and ε for the same imperative?',
    'Author the sibling stories and compare computed per-seat classifications across the family; the divergence locates the disagreement in the priority-ordering premise (this reading vs adaptation-priority) and the growth-compatibility premise (this reading vs degrowth).',
    'Under the degrowth reading, Global North consumers become obligated payers, innovation sectors exit the beneficiary set, and ε rises; under the adaptation-priority reading, today''s frontline regions become primary beneficiaries and part of the future-generation extraction commutes into present-day provision. Classification of THIS story is unchanged either way — the siblings are different constraints.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_indexicality, conceptual, 'Committer structure: reading-indexed classification of a contested kernel; sibling readings instantiate different constraints.').

omega_variable(
    cdr_scalability_gap,
    'Will carbon dioxide removal deploy at the gigatonne scale and cost profile the mitigation-priority arrangement assumes when booking future removals against present emissions?',
    'Deployment and cost-curve data through 2035: realized DAC/BECCS capacity versus announced pipelines, verified removal volumes, and delivered cost per tonne.',
    'If CDR fails to scale, deferred abatement converts into permanent additional warming borne by the powerless/trapped future-generations seat — effective extraction on that seat rises sharply and the arrangement shifts toward snare. If it scales, part of the measured extraction is repriced as option value paid for flexibility.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cdr_scalability_gap, empirical, 'Whether the arrangement''s promissory reliance on unproven removal technology is a hedge or a deferral mechanism.').

omega_variable(
    adaptation_residualization_extent,
    'Is adaptation actually financed as a residual, or do adaptation flows approach needs-parity once all channels are counted comprehensively?',
    'Cross-check OECD Rio-marker finance accounting and multilateral fund disbursements against UNEP Adaptation Gap estimates of needs versus delivered flows, controlling for double-counting of development finance relabeled as adaptation.',
    'Confirmed residualization sustains the victim declarations and high effective extraction on trapped seats; demonstrated parity would weaken the frontline victim declaration and pull the arrangement''s computed type toward rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(adaptation_residualization_extent, empirical, 'Whether the ''adaptation as residual'' structure is descriptive of actual finance flows or rhetorical.').

omega_variable(
    intergenerational_discount_rate,
    'What social discount rate governs how much harm to future generations the arrangement registers as a cost at all?',
    'Not resolvable by data alone: compare revealed discount rates in policy appraisal (cost-benefit guidance, central bank climate stress tests) against the zero-or-near-zero rates argued in welfare ethics; the gap is the contested quantity.',
    'At conventional positive rates, the frame''s extraction from the future-generations seat registers as modest and the tangled_rope reading stands; at near-zero rates the same flows register as severe extraction and the computed classification migrates toward snare. The structural data is fixed; the ethical parameter moves the verdict.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(intergenerational_discount_rate, preference, 'Normative parameter determining how much of the deferred-cost structure counts as extraction.').

omega_variable(
    market_frame_delay_effect,
    'Does constituting climate response as a market purchase extend fossil fuel operations beyond the counterfactual in which supply-side structural constraints applied?',
    'Comparative policy analysis: licensing, production, and reserve-utilization trajectories in jurisdictions imposing supply-side constraints versus carbon-price-only regimes, matched for resource endowment and demand.',
    'If the delay effect is real, the fossil incumbent''s net position sits closer to the beneficiary end than its payment flows suggest, and part of the arrangement''s persistence is deliberate deferral rather than transition management; if not, incumbents are ordinary payers and the beneficiary set narrows to innovation sectors and intermediaries, lowering measured asymmetry.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(market_frame_delay_effect, empirical, 'Whether the market framing functions as an incumbent-delay mechanism, resolving the dual-positioned seat''s directionality.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_response_imperative__mitigation_priority_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clim_tr_t0, climate_response_imperative__mitigation_priority_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(clim_tr_t5, climate_response_imperative__mitigation_priority_reading, theater_ratio, 5, 0.28).
narrative_ontology:measurement(clim_tr_t10, climate_response_imperative__mitigation_priority_reading, theater_ratio, 10, 0.32).
narrative_ontology:measurement(clim_tr_t15, climate_response_imperative__mitigation_priority_reading, theater_ratio, 15, 0.36).
narrative_ontology:measurement(clim_tr_t20, climate_response_imperative__mitigation_priority_reading, theater_ratio, 20, 0.41).
narrative_ontology:measurement(clim_tr_t25, climate_response_imperative__mitigation_priority_reading, theater_ratio, 25, 0.45).
narrative_ontology:measurement(clim_tr_t30, climate_response_imperative__mitigation_priority_reading, theater_ratio, 30, 0.48).

% Extraction over time
narrative_ontology:measurement(clim_be_t0, climate_response_imperative__mitigation_priority_reading, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(clim_be_t5, climate_response_imperative__mitigation_priority_reading, base_extractiveness, 5, 0.44).
narrative_ontology:measurement(clim_be_t10, climate_response_imperative__mitigation_priority_reading, base_extractiveness, 10, 0.47).
narrative_ontology:measurement(clim_be_t15, climate_response_imperative__mitigation_priority_reading, base_extractiveness, 15, 0.5).
narrative_ontology:measurement(clim_be_t20, climate_response_imperative__mitigation_priority_reading, base_extractiveness, 20, 0.53).
narrative_ontology:measurement(clim_be_t25, climate_response_imperative__mitigation_priority_reading, base_extractiveness, 25, 0.56).
narrative_ontology:measurement(clim_be_t30, climate_response_imperative__mitigation_priority_reading, base_extractiveness, 30, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(clim_su_t0, climate_response_imperative__mitigation_priority_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(clim_su_t5, climate_response_imperative__mitigation_priority_reading, suppression_requirement, 5, 0.4).
narrative_ontology:measurement(clim_su_t10, climate_response_imperative__mitigation_priority_reading, suppression_requirement, 10, 0.46).
narrative_ontology:measurement(clim_su_t15, climate_response_imperative__mitigation_priority_reading, suppression_requirement, 15, 0.51).
narrative_ontology:measurement(clim_su_t20, climate_response_imperative__mitigation_priority_reading, suppression_requirement, 20, 0.55).
narrative_ontology:measurement(clim_su_t25, climate_response_imperative__mitigation_priority_reading, suppression_requirement, 25, 0.58).
narrative_ontology:measurement(clim_su_t30, climate_response_imperative__mitigation_priority_reading, suppression_requirement, 30, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_response_imperative__mitigation_priority_reading, resource_allocation).
narrative_ontology:affects_constraint(climate_response_imperative__mitigation_priority_reading, climate_response_imperative__adaptation_priority_reading).
narrative_ontology:affects_constraint(climate_response_imperative__mitigation_priority_reading, climate_response_imperative__degrowth_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the natural-language label 'climate response' decomposes under the ε-invariance principle into three structurally distinct readings of the climate_response_imperative kernel. This story (mitigation_priority_reading) carries ε ≈ 0.58 with victims {future_generations, frontline_vulnerable_regions} and beneficiaries drawn from innovation/market actors; the adaptation_priority_reading inverts the priority ordering and moves frontline regions toward the beneficiary set; the degrowth_reading adds Northern consumers as obligated payers and removes growth-compatibility from the premise set. The upstream/downstream structure runs from this reading outward: as the currently dominant institutional instantiation, it sets the finance-scarcity conditions under which the adaptation-priority reading operates and the growth-dependence conditions the degrowth reading contests. Each file links the others via network.affects_constraints; contamination propagation across the family traces how degradation of the mitigation frame's credibility (offset scandals, stocktake shortfalls) transfers pressure to the sibling readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
