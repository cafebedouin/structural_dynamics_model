% ============================================================================
% CONSTRAINT STORY: climate_response_action__mitigation_priority
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_climate_response_action__mitigation_priority, []).

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
 *   constraint_id: climate_response_action__mitigation_priority
 *   human_readable: Mitigation-Priority Climate Regime: Below-2C via Growth-Compatible Decarbonization
 *   domain: environmental/political economy/intergenerational ethics
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the climate_response_action
 *   kernel: mitigation_priority, the arrangement in which responding to
 *   climate change means holding warming below 2C chiefly through emissions
 *   reductions, enabled by technological innovation and carbon markets, with
 *   GDP growth explicitly maintained. The epsilon referent is the standing
 *   arrangement under contest - the Paris-Architecture regime of NDC cycles,
 *   carbon pricing and offset markets, green industrial policy, and the
 *   growth-compatibility premise - assessed by this reading's own lights; the
 *   sibling readings (adaptation_priority, degrowth_transformation) are
 *   separate constraints in separate files and are not averaged into this
 *   one. Structurally the arrangement solves a genuine commons problem (a
 *   single shared atmosphere, classic free-rider structure) while
 *   concentrating its costs asymmetrically: transition costs land on present
 *   high-emitting sectors and their workers, adaptation and residual-damage
 *   costs are deferred to vulnerable low-emission regions and to people not
 *   yet born, and the innovation, standards, and market-making rents accrue
 *   disproportionately to economies that already possess technological
 *   capacity. The claim/metric independence rule is observed: claimed_type is
 *   authored as tangled_rope from what I believe is structurally true (real
 *   coordination function plus real asymmetric extraction under active
 *   enforcement), and the metrics are authored as descriptive facts about
 *   operation, without tuning either toward the other or toward a predicted
 *   engine output.
 *
 * KEY AGENTS:
 *   - - innovation_leading_economies: Agenda-setter and principal beneficiary (institutional/arbitrage) - writes the regime's rulebook, anchors the growth-compatibility premise, clears the carbon instruments, exports the transition's hardware
 *   - - green_technology_industries: Beneficiary (powerful/mobile) - sells decarbonization into demand the regime manufactures, holds the patent rents
 *   - - carbon_market_intermediaries: Beneficiary (organized/arbitrage) - collects fees on volume and complexity, indifferent to tons abated
 *   - - climate_diplomacy_establishment: Beneficiary (institutional/identity_locked) - careers and funding fused to the frame's continuation
 *   - - fossil_fuel_dependent_workers: Payer (organized/trapped) - concentrated transition costs, late and small compensation
 *   - - global_south_vulnerable_populations: Payer (powerless/trapped) - inherits the deferred adaptation bill
 *   - - small_island_states: Payer (organized/trapped) - existential exposure under the residual warming a 2C ceiling permits
 *   - - emerging_industrial_economies: Payer and secondary beneficiary (powerful/constrained) - pressed to peak early while capturing green-manufacturing demand
 *   - - future_generations: Payer (powerless/trapped) - no seat, no vote, no exit; represented only by discount rates
 *   - - degrowth_adaptation_advocates: Excluded (organized/constrained) - holders of the sibling readings, kept to side events
 *   - - climate_equity_analysts: Observer (analytical/analytical) - measures pledge-versus-delivery and distributional incidence, commands no votes
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_response_action__mitigation_priority, 0.62).
domain_priors:suppression_score(climate_response_action__mitigation_priority, 0.48).
domain_priors:theater_ratio(climate_response_action__mitigation_priority, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_response_action__mitigation_priority, extractiveness, 0.62).
narrative_ontology:constraint_metric(climate_response_action__mitigation_priority, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(climate_response_action__mitigation_priority, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(climate_response_action__mitigation_priority, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(climate_response_action__mitigation_priority, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_response_action__mitigation_priority, tangled_rope).
narrative_ontology:human_readable(climate_response_action__mitigation_priority, "Mitigation-Priority Climate Regime: Below-2C via Growth-Compatible Decarbonization").
narrative_ontology:topic_domain(climate_response_action__mitigation_priority, "environmental/political economy/intergenerational ethics").

domain_priors:requires_active_enforcement(climate_response_action__mitigation_priority).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_response_action__mitigation_priority, '92df79d4-50dd-4358-b1b4-cbfbeeff8a57').
narrative_ontology:cs_kernel_codification('92df79d4-50dd-4358-b1b4-cbfbeeff8a57', fixed_text).
narrative_ontology:cs_authority_grounding('92df79d4-50dd-4358-b1b4-cbfbeeff8a57', lineage).
narrative_ontology:cs_interpretation_layer_present('92df79d4-50dd-4358-b1b4-cbfbeeff8a57').
narrative_ontology:cs_reading_relation('92df79d4-50dd-4358-b1b4-cbfbeeff8a57', climate_response_action__adaptation_priority, influences).
narrative_ontology:cs_reading_relation('92df79d4-50dd-4358-b1b4-cbfbeeff8a57', climate_response_action__degrowth_transformation, forecloses).
narrative_ontology:cs_axiom('92df79d4-50dd-4358-b1b4-cbfbeeff8a57', foundational, growth_compatible_mitigation_sufficiency).
narrative_ontology:cs_axiom_status(growth_compatible_mitigation_sufficiency, holdable).
narrative_ontology:cs_axiom_grounding('92df79d4-50dd-4358-b1b4-cbfbeeff8a57', growth_compatible_mitigation_sufficiency, empirically_contingent).
narrative_ontology:cs_axiom('92df79d4-50dd-4358-b1b4-cbfbeeff8a57', foundational, emissions_reduction_primacy).
narrative_ontology:cs_axiom_status(emissions_reduction_primacy, holdable).
narrative_ontology:cs_axiom_grounding('92df79d4-50dd-4358-b1b4-cbfbeeff8a57', emissions_reduction_primacy, instrumental).
narrative_ontology:cs_reference_frame('92df79d4-50dd-4358-b1b4-cbfbeeff8a57', growth_compatible_two_degree_guardrail).
narrative_ontology:cs_drift_state('92df79d4-50dd-4358-b1b4-cbfbeeff8a57', post_paris_implementation_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('92df79d4-50dd-4358-b1b4-cbfbeeff8a57', '').
narrative_ontology:cs_kernel_id(climate_response_action__mitigation_priority, climate_response_action).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_response_action__mitigation_priority, innovation_leading_economies).
narrative_ontology:constraint_beneficiary(climate_response_action__mitigation_priority, green_technology_industries).
narrative_ontology:constraint_beneficiary(climate_response_action__mitigation_priority, carbon_market_intermediaries).
narrative_ontology:constraint_beneficiary(climate_response_action__mitigation_priority, climate_diplomacy_establishment).
narrative_ontology:constraint_victim(climate_response_action__mitigation_priority, fossil_fuel_dependent_workers).
narrative_ontology:constraint_victim(climate_response_action__mitigation_priority, global_south_vulnerable_populations).
narrative_ontology:constraint_victim(climate_response_action__mitigation_priority, small_island_states).
narrative_ontology:constraint_victim(climate_response_action__mitigation_priority, future_generations).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(climate_response_action__mitigation_priority, emerging_industrial_economies).
narrative_ontology:constraint_victim(climate_response_action__mitigation_priority, emerging_industrial_economies).
narrative_ontology:constraint_vindicates(climate_response_action__mitigation_priority, absolute_decoupling_feasibility).
narrative_ontology:constraint_vindicates(climate_response_action__mitigation_priority, carbon_markets_cost_effectiveness_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Draft and renegotiate the regime's architecture - NDC guidance, Article 6 market rules, carbon border adjustments - anchor the premise that decarbonization is compatible with growth, host the financial centers that clear carbon instruments, and capture first-mover rents as exporters of renewable technology and standards. Their exposure to the constraint's costs is hedged by diversified economies and, ultimately, by their capacity to spend defensively on adaptation if the guardrail slips.
narrative_ontology:constraint_stakeholder(climate_response_action__mitigation_priority, innovation_leading_economies, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(climate_response_action__mitigation_priority, innovation_leading_economies, beneficiary).

% Sell the hardware and software of decarbonization - turbines, batteries, electrolyzers, monitoring platforms - into demand created by mandates, subsidies, and carbon prices; hold patent portfolios that license the transition; relocate production across jurisdictions routinely when incentives shift.
narrative_ontology:constraint_stakeholder(climate_response_action__mitigation_priority, green_technology_industries, beneficiary,
    powerful, biographical, mobile, global).

% Registries, verifiers, brokers, and fund managers collecting fees on issuance, verification, and trading of allowances and offsets; their income scales with transaction volume and methodological complexity rather than with tons actually abated, giving them a structural interest in elaborate rules over simple ones.
narrative_ontology:constraint_stakeholder(climate_response_action__mitigation_priority, carbon_market_intermediaries, beneficiary,
    organized, immediate, arbitrage, global).

% Negotiators, secretariat staff, accredited observers, and the conference circuit whose professional standing, funding lines, and career ladders exist inside the mitigation frame; their identities are fused with the regime's continuation, so the sibling readings threaten livelihoods as well as positions, and frame-challenging evidence is processed through interpretive bodies they staff.
narrative_ontology:constraint_stakeholder(climate_response_action__mitigation_priority, climate_diplomacy_establishment, beneficiary,
    institutional, generational, identity_locked, global).

% Coal miners, oil-patch crews, and combustion-industry workers whose regions concentrate the transition's costs; promised just-transition funds arrive late and small relative to lost wages, housing equity, and community viability; relocating means abandoning seniority, property, and kin networks, so most stay and absorb the decline.
narrative_ontology:constraint_stakeholder(climate_response_action__mitigation_priority, fossil_fuel_dependent_workers, payer,
    organized, biographical, trapped, regional).

% Low-emission populations in flood-, heat-, and drought-exposed regions who bear the adaptation bill the regime defers; adaptation finance arrives as loans and as fractions of promised amounts; their formal seats in negotiations are outnumbered and outspent, and migration - the de facto exit - is priced in lives.
narrative_ontology:constraint_stakeholder(climate_response_action__mitigation_priority, global_south_vulnerable_populations, payer,
    powerless, generational, trapped, regional).

% Coalition members with negligible cumulative emissions facing existential sea-level exposure under the residual warming that a 2C ceiling permits; their leverage is bloc voting and moral suasion inside a process whose pace is set by larger powers, and no relocation option preserves the nation as such.
narrative_ontology:constraint_stakeholder(climate_response_action__mitigation_priority, small_island_states, payer,
    organized, civilizational, trapped, global).

% Large fast-industrializing emitters pressed to peak and cut earlier than their development sequencing prefers, while simultaneously capturing the largest shares of renewable-manufacturing demand; bound into export markets and supply chains they cannot abandon, they absorb reduction pressure and green-industry rents at the same time.
narrative_ontology:constraint_stakeholder(climate_response_action__mitigation_priority, emerging_industrial_economies, payer,
    powerful, generational, constrained, continental).
narrative_ontology:stakeholder_secondary_role(climate_response_action__mitigation_priority, emerging_industrial_economies, beneficiary).

% People not yet born who inherit whatever residual warming, locked-in infrastructure, and depleted carbon budgets present choices leave; they are represented only through discount rates and scattered guardianship experiments; they hold no seat, cast no vote, and have no exit from the planet they inherit.
narrative_ontology:constraint_stakeholder(climate_response_action__mitigation_priority, future_generations, payer,
    powerless, civilizational, trapped, global).

% Holders of the sibling readings - resilience-first planners and sufficiency economists - confined to side events, academic journals, and municipal pilots; they would reorder the regime's priorities around protection and sufficiency but lack agenda access inside the negotiation track where the frame is reproduced.
narrative_ontology:constraint_stakeholder(climate_response_action__mitigation_priority, degrowth_adaptation_advocates, excluded,
    organized, generational, constrained, global).

% Assessment bodies and independent trackers - emissions-gap analysts, climate-finance auditors, equity researchers - who measure pledge-versus-delivery and the distributional incidence of costs and rents; they see the full structure and publish it, but command no votes inside the process they measure.
narrative_ontology:constraint_stakeholder(climate_response_action__mitigation_priority, climate_equity_analysts, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(climate_response_action__mitigation_priority, innovation_leading_economies).
narrative_ontology:fixing_cost_class(climate_response_action__mitigation_priority, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the genuine global collective-action problem of a shared atmospheric commons: coordinates national emission-reduction efforts against a common temperature guardrail, standardizes accounting so effort is comparable (MRV), channels technology diffusion through markets, and gives every major emitter a reason to sign by defining climate action as growth-compatible.
% TRANSFER_FUNCTION: Moves transition costs onto present high-emitting sectors and their workers via carbon prices, mandates, and stranded assets; moves adaptation and residual-damage costs onto future generations and low-emission vulnerable regions via deferral and discounting; moves subsidy flows, patent rents, and market-making fees toward innovation-capacity firms, nations, and intermediaries; moves offset revenue from buyer jurisdictions to project sites and verifiers.
% ABSENT_VOICES: Future generations are absent from every table and are voiced only by discount rates; Global South vulnerable populations are formally present but effectively outvoted, with finance promises chronically unmet; holders of the adaptation and degrowth readings are structurally excluded to side events; the non-human systems bearing the residual harm have no proxy with agenda access.
% DISAPPEARANCE_RATIONALE: If the arrangement vanished overnight, the carbon markets, NDC cycles, green subsidy complexes, net-zero legislation, and the entire COP apparatus would dissolve or repurpose; energy investment would reallocate around whichever reading captured the vacated agenda; the sibling readings would move from the margins to the center of climate governance, and the distribution of costs across present workers, vulnerable regions, and future generations would be redrawn rather than left as is.
% FOUNDING_PROBLEM: After the Kyoto-era top-down targets failed to bind major emitters, the founding problem was to construct a durable global regime that would hold warming below dangerous thresholds while accommodating sovereign development priorities - answered by defining climate action as growth-compatible mitigation so that every major emitter had a reason to join.
% FOUNDING_PROBLEM_CORROBORATION: IPCC assessment rounds and UNEP Emissions Gap reports - bodies outside the beneficiary set - corroborate that the danger-threshold problem is live and that delivered pledges fall short of the 2C path; G77-plus-China statements and AOSIS submissions corroborate that the growth-compatible delivery mechanism is contested by those bearing its deferred costs; no source outside the innovation-economy beneficiary set attests that the founding bargain is succeeding as designed.
narrative_ontology:disappearance_verdict(climate_response_action__mitigation_priority, world_rearranges).
narrative_ontology:founding_problem_status(climate_response_action__mitigation_priority, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(climate_response_action__mitigation_priority, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(climate_response_action__mitigation_priority, 'none', 1).
narrative_ontology:epsilon_provenance(climate_response_action__mitigation_priority, 0.62, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(climate_response_action__mitigation_priority_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(climate_response_action__mitigation_priority, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(climate_response_action__mitigation_priority_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is substantial (0.62 at interval end) but not pure: the atmosphere-coordination function is genuine, so part of the cost participants bear is the price of solving a real collective-action problem, while the remainder is asymmetric transfer - deferral of adaptation costs, discounting of residual harm, and rent capture by innovation-capacity actors. Suppression (0.48) is authored as a raw structural property and is NOT scaled by power or scope - only extractiveness is scaled, by directionality and spatial scope in the engine's computation; the regime's suppressive force operates through accounting rules, market-access conditions, and border adjustment mechanisms rather than physical coercion, and it marginalizes rival readings institutionally rather than banning them. Theater ratio (0.45) reflects the growing share of activity that is performative: net-zero pledges without delivery plans, offset methodologies with contested additionality, and a ratchet mechanism whose ambition cycles substitute for emission cuts - though real decarbonization in power and transport keeps the ratio below majority. Accessibility_collapse (0.5) is moderate: once a nation accepts the frame, alternatives within it collapse (carbon-market lock-in, stranded-asset politics), but the sibling readings remain live outside it, which is precisely why they appear as the excluded stakeholder seat. Resistance (0.55) is real and bidirectional: fossil-dependent regions resist the costs, Global South blocs resist the deferral, and youth movements resist the insufficiency. The three measurement series run on ONE shared time grid (2015-2025, six points, every metric authored at every point) so the engine samples no substituted end-state values; the trajectories show extraction accumulation and pledge-theater growth alongside a maturing enforcement apparatus (Article 6 operationalization, carbon border adjustment), modeled as a rising suppression_requirement series because enforcement-capacity change is part of this story's record.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute differently and the divergence is the finding. From the agenda-setter seat (innovation_leading_economies), the arrangement is a regime it built, staffs, and profits from - coordination it genuinely believes in, experienced as leadership. From the trapped payer seats, the same structure operates as imposed cost: fossil_fuel_dependent_workers experience concentrated loss with rhetorical compensation, global_south_vulnerable_populations experience a deferred bill arriving as loans, and future_generations experience the whole arrangement as a decision made about them without them. The dual-positioned emerging_industrial_economies seat should compute intermediate - pressed and enriched at once. The engine derives these per-seat classifications from the structural data; nothing in the authored claim adjudicates between them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive low directionality (subsidy-side) for innovation_leading_economies, green_technology_industries, carbon_market_intermediaries, and climate_diplomacy_establishment; victim declarations drive high directionality (target-side) for fossil_fuel_dependent_workers, global_south_vulnerable_populations, small_island_states, and future_generations. Exit modulation sharpens the targets: trapped and identity_locked payers sit nearer the full-target end than mobile ones, while the arbitrage-grade exits of the agenda-setter and intermediaries sit nearest the beneficiary end. No directionality overrides are authored: the derivation chain from beneficiary/victim declarations plus power and exit produces the correct relationships for every seat, including the dual-positioned emerging_industrial_economies, whose payer-primary/secondary-beneficiary declaration places them near symmetric. Spatial scope amplifies effective extraction for the global-scope trapped seats (verification of distant harm is hardest exactly where the deferred costs land).
 *
 * MANDATROPHY ANALYSIS:
 *   The classification guards against mislabeling in both directions. Reading the arrangement as pure snare would erase the genuine coordination achievement - a near-universal treaty architecture solving a real commons problem that no ad hoc coalition was solving; reading it as pure rope would erase the documented asymmetry - unmet finance promises, discount-rate-encoded transfers to the unborn, and rents flowing to those who wrote the rules. The tangled_rope claim holds both truths in one structure. On obsolescence: the founding problem (binding major emitters to a safe guardrail while accommodating development) is NOT dead - warming continues and the danger threshold is live - so the arrangement is not a piton maintained by inertia; its persistence tracks a persistent problem. But the founding BARGAIN (that growth-compatible mitigation suffices) is contested by evidence the regime itself publishes, which is why founding_problem_status is authored contested rather than live: the mismatch consumer should find no dead-mandate zombie flag here, but should find a live dispute about whether the mandate's chosen instrument can ever discharge it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contestation,
    'This constraint is one reading (mitigation_priority) of the climate_response_action kernel; what would the sibling readings (adaptation_priority, degrowth_transformation) change structurally if instantiated instead?',
    'Comparative classification across the three reading-stories of the kernel: identical seat structures authored under each reading, with per-seat classifications computed and diffed.',
    'If either sibling computes as less extractive for the same seats (global_south_vulnerable_populations, future_generations), the mitigation frame''s dominance is revealed as a distributional choice among possible responses rather than a necessity, and the regime''s legitimacy claims weaken accordingly.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'Committer structure: which kernel, which reading, what siblings would alter.').

omega_variable(
    absolute_decoupling_feasibility,
    'Is absolute decoupling of GDP from territorial emissions at the required global rate (roughly 7 percent per year sustained) an empirically demonstrated regularity or an assumed promissory premise?',
    'National-level panel analysis of sustained absolute decoupling episodes at required rates, controlling for offshored consumption emissions.',
    'If undemonstrated, the growth-compatibility premise functions as a deferral device: present actors bank the promise and the arrangement''s effective extraction on future_generations rises toward snare territory; if demonstrated, part of the measured extraction is the honest price of the coordination itself.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(absolute_decoupling_feasibility, empirical, 'Whether the reading''s load-bearing empirical premise is established or assumed.').

omega_variable(
    carbon_removal_scale_assumption,
    'Do below-2C pathways depend on carbon dioxide removal at scales never operated, and does that dependence transfer present abatement costs onto future generations?',
    'Engineering deployment trajectories for direct air capture and bioenergy-with-capture against the removal volumes embedded in integrated assessment pathways.',
    'If removal fails to scale, residual warming exceeds the 2C guardrail and the deferred burden lands on future_generations and global_south_vulnerable_populations with interest; the arrangement drifts from tangled_rope toward snare in per-seat computation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(carbon_removal_scale_assumption, empirical, 'Feasibility of the removal volumes the target implicitly borrows against.').

omega_variable(
    enforcement_vs_convergence,
    'Is observed decarbonization driven by the regime''s enforcement machinery (accounting rules, border adjustments, market access conditions) or by cost convergence in renewables that would proceed regardless?',
    'Difference-in-differences across jurisdictions with varying regime exposure but similar technology cost curves.',
    'If convergence dominates, the suppression requirement falls and the arrangement''s rope character strengthens; if enforcement dominates, the constraint''s persistence depends on actively maintained coercion and its extractive share rises.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_vs_convergence, empirical, 'Source of compliance: enforced obligation or converging economic interest.').

omega_variable(
    discount_rate_value_choice,
    'How heavily are future generations'' interests weighted in the social cost of carbon and in national cost-benefit appraisal, given that the discount rate is a value choice rather than a discovered parameter?',
    'Deliberative determination of the ethical discount rate, or institutional adoption of near-zero pure-time-preference rates as some jurisdictions have legislated.',
    'High discount rates legitimize the deferral structure and mask extraction on future_generations; near-zero rates invert the arrangement''s justification arithmetic and would reclassify much of its current operation as uncompensated imposition.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(discount_rate_value_choice, preference, 'The value-laden weighting that determines how visible the intergenerational transfer is.').

omega_variable(
    adaptation_deferral_causality,
    'Is chronic adaptation underfinance a consequence of the mitigation-priority framing specifically, or of aggregate climate-finance scarcity that any reading would face?',
    'Budget-share analysis across donor portfolios before and after mitigation-framed instruments (offsets, Article 6, green bonds) displaced grant-based adaptation finance.',
    'If the framing causes the diversion, the deferral is attributable to this reading''s structure and counts as its extraction; if scarcity causes it, the deferral is a common constraint and this reading bears less of the responsibility the structural delta assigns it.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(adaptation_deferral_causality, empirical, 'Attribution of the adaptation finance gap between framing effect and budget constraint.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_response_action__mitigation_priority, 2015, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clim_tr_t2015, climate_response_action__mitigation_priority, theater_ratio, 2015, 0.25).
narrative_ontology:measurement_basis(clim_tr_t2015, observed).
narrative_ontology:measurement(clim_tr_t2017, climate_response_action__mitigation_priority, theater_ratio, 2017, 0.29).
narrative_ontology:measurement_basis(clim_tr_t2017, observed).
narrative_ontology:measurement(clim_tr_t2019, climate_response_action__mitigation_priority, theater_ratio, 2019, 0.33).
narrative_ontology:measurement_basis(clim_tr_t2019, observed).
narrative_ontology:measurement(clim_tr_t2021, climate_response_action__mitigation_priority, theater_ratio, 2021, 0.37).
narrative_ontology:measurement_basis(clim_tr_t2021, observed).
narrative_ontology:measurement(clim_tr_t2023, climate_response_action__mitigation_priority, theater_ratio, 2023, 0.41).
narrative_ontology:measurement_basis(clim_tr_t2023, observed).
narrative_ontology:measurement(clim_tr_t2025, climate_response_action__mitigation_priority, theater_ratio, 2025, 0.45).
narrative_ontology:measurement_basis(clim_tr_t2025, observed).

% Extraction over time
narrative_ontology:measurement(clim_be_t2015, climate_response_action__mitigation_priority, base_extractiveness, 2015, 0.5).
narrative_ontology:measurement_basis(clim_be_t2015, observed).
narrative_ontology:measurement(clim_be_t2017, climate_response_action__mitigation_priority, base_extractiveness, 2017, 0.53).
narrative_ontology:measurement_basis(clim_be_t2017, observed).
narrative_ontology:measurement(clim_be_t2019, climate_response_action__mitigation_priority, base_extractiveness, 2019, 0.56).
narrative_ontology:measurement_basis(clim_be_t2019, observed).
narrative_ontology:measurement(clim_be_t2021, climate_response_action__mitigation_priority, base_extractiveness, 2021, 0.58).
narrative_ontology:measurement_basis(clim_be_t2021, observed).
narrative_ontology:measurement(clim_be_t2023, climate_response_action__mitigation_priority, base_extractiveness, 2023, 0.6).
narrative_ontology:measurement_basis(clim_be_t2023, observed).
narrative_ontology:measurement(clim_be_t2025, climate_response_action__mitigation_priority, base_extractiveness, 2025, 0.62).
narrative_ontology:measurement_basis(clim_be_t2025, observed).

% Suppression requirement over time
narrative_ontology:measurement(clim_su_t2015, climate_response_action__mitigation_priority, suppression_requirement, 2015, 0.36).
narrative_ontology:measurement_basis(clim_su_t2015, observed).
narrative_ontology:measurement(clim_su_t2017, climate_response_action__mitigation_priority, suppression_requirement, 2017, 0.39).
narrative_ontology:measurement_basis(clim_su_t2017, observed).
narrative_ontology:measurement(clim_su_t2019, climate_response_action__mitigation_priority, suppression_requirement, 2019, 0.42).
narrative_ontology:measurement_basis(clim_su_t2019, observed).
narrative_ontology:measurement(clim_su_t2021, climate_response_action__mitigation_priority, suppression_requirement, 2021, 0.44).
narrative_ontology:measurement_basis(clim_su_t2021, observed).
narrative_ontology:measurement(clim_su_t2023, climate_response_action__mitigation_priority, suppression_requirement, 2023, 0.46).
narrative_ontology:measurement_basis(clim_su_t2023, observed).
narrative_ontology:measurement(clim_su_t2025, climate_response_action__mitigation_priority, suppression_requirement, 2025, 0.48).
narrative_ontology:measurement_basis(clim_su_t2025, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_response_action__mitigation_priority, resource_allocation).
narrative_ontology:affects_constraint(climate_response_action__mitigation_priority, climate_response_action__adaptation_priority).
narrative_ontology:affects_constraint(climate_response_action__mitigation_priority, climate_response_action__degrowth_transformation).

% DUAL FORMULATION NOTE:
% Constraint family: the natural-language label 'climate response' decomposes into three structurally distinct readings of one kernel, each with its own stable epsilon, beneficiary/victim structure, and classification - per the epsilon-invariance principle. This story (mitigation_priority) authors epsilon for the standing mitigation-first arrangement assessed by its own lights; adaptation_priority authors a different constraint with a different victim set (present-day exposed populations as primary rather than deferred payers) and degrowth_transformation another (throughput-dependent populations as payers, growth itself as the contested variable). The upstream reading (mitigation_priority) influences the downstream adaptation reading by absorbing the finance share adaptation competes for, and stands in logical contradiction with the degrowth reading on the growth premise. Each member links the others via network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
