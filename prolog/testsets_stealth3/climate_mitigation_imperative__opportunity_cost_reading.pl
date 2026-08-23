% ============================================================================
% CONSTRAINT STORY: climate_mitigation_imperative__opportunity_cost_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_climate_mitigation_imperative__opportunity_cost_reading, []).

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
 *   constraint_id: climate_mitigation_imperative__opportunity_cost_reading
 *   human_readable: Climate Mitigation Imperative - Opportunity-Cost Reading (Speed-First Allocation Discipline)
 *   domain: energy policy/climate mitigation/technology governance
 *
 * SUMMARY:
 *   This file instantiates ONE reading - the opportunity_cost_reading - of
 *   the contested kernel climate_mitigation_imperative. The kernel is the
 *   standing commitment that greenhouse-gas mitigation must proceed; the
 *   readings disagree on the allocation discipline it implies. This reading
 *   holds that mitigation is a triage problem under a binding carbon budget
 *   and scarce capital: resources must rank strictly by carbon avoided per
 *   dollar per year, and technologies whose deployment timelines exceed the
 *   decisive decade - chiefly nuclear fission - are net-harmful because they
 *   consume capital, attention, and political bandwidth that faster options
 *   would convert into greater cumulative abatement. The constraint authored
 *   here is that ranking discipline as it actually operates in climate
 *   finance, taxonomy governance, lender conditionality, and advocacy
 *   discourse. Per the epsilon-referent rule for kernel readings, epsilon's
 *   referent is this standing speed-first allocation arrangement as it
 *   operates - NOT the fully-renewables world this reading endorses, and NOT
 *   the sibling readings' referents. Sibling files:
 *   portfolio_optimization_reading (authors epsilon for the underinvestment
 *   in firm low-carbon capacity; nuclear appears as necessary pillar, not
 *   victim) and systems_transition_reading (authors epsilon for centralized
 *   corporate energy control; a different referent entirely). Both are linked
 *   via network.affects_constraints. Claim and metrics are independent
 *   authored facts: claimed_type records my structural read (tangled_rope -
 *   genuine coordination function plus asymmetric incidence requiring active
 *   enforcement), while the reading's own normative assertions live in
 *   cs_structure.axioms, not in the claim field.
 *
 * KEY AGENTS:
 *   - - nuclear_energy_sector: primary target (organized/trapped) - bears the rule's extraction: financing denial, retirement pressure, workforce attrition
 *   - - renewable_deployment_industry: primary beneficiary (institutional/mobile) - collects the capital and policy favor the rule channels
 *   - - climate_advocacy_networks: secondary beneficiary (organized/identity_locked) - collects relevance and funding from the frame; cannot exit without self-repudiation
 *   - - natural_gas_interests: interim incidental beneficiary and long-run payer (institutional/arbitrage) - captures backfill volumes where nuclear retires first
 *   - - electricity_ratepayers: diffuse payer with partial beneficiary position (moderate/trapped) - absorbs firmness-gap system costs and volatility
 *   - - green_finance_standard_setters: agenda setter (institutional/constrained) - administers the taxonomies that operationalize the rule
 *   - - developing_grid_planners: excluded voice (moderate/constrained) - bound by lender conditionality, absent from the forums setting allocation criteria
 *   - - energy_system_analysts: analytical observer - sees both the genuine savings and the unpriced system costs
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_mitigation_imperative__opportunity_cost_reading, 0.62).
domain_priors:suppression_score(climate_mitigation_imperative__opportunity_cost_reading, 0.55).
domain_priors:theater_ratio(climate_mitigation_imperative__opportunity_cost_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_mitigation_imperative__opportunity_cost_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(climate_mitigation_imperative__opportunity_cost_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(climate_mitigation_imperative__opportunity_cost_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(climate_mitigation_imperative__opportunity_cost_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(climate_mitigation_imperative__opportunity_cost_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_mitigation_imperative__opportunity_cost_reading, tangled_rope).
narrative_ontology:human_readable(climate_mitigation_imperative__opportunity_cost_reading, "Climate Mitigation Imperative - Opportunity-Cost Reading (Speed-First Allocation Discipline)").
narrative_ontology:topic_domain(climate_mitigation_imperative__opportunity_cost_reading, "energy policy/climate mitigation/technology governance").

domain_priors:requires_active_enforcement(climate_mitigation_imperative__opportunity_cost_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_mitigation_imperative__opportunity_cost_reading, '9652ca90-aba1-4f9e-9ee0-c6918427aa50').
narrative_ontology:cs_kernel_codification('9652ca90-aba1-4f9e-9ee0-c6918427aa50', distributed).
narrative_ontology:cs_authority_grounding('9652ca90-aba1-4f9e-9ee0-c6918427aa50', extraction).
narrative_ontology:cs_interpretation_layer_present('9652ca90-aba1-4f9e-9ee0-c6918427aa50').
narrative_ontology:cs_reading_relation('9652ca90-aba1-4f9e-9ee0-c6918427aa50', climate_mitigation_imperative__portfolio_optimization_reading, influences).
narrative_ontology:cs_reading_relation('9652ca90-aba1-4f9e-9ee0-c6918427aa50', climate_mitigation_imperative__systems_transition_reading, coexists_with).
narrative_ontology:cs_axiom('9652ca90-aba1-4f9e-9ee0-c6918427aa50', foundational, speed_weighted_abatement_supremacy).
narrative_ontology:cs_axiom_status(speed_weighted_abatement_supremacy, holdable).
narrative_ontology:cs_axiom_grounding('9652ca90-aba1-4f9e-9ee0-c6918427aa50', speed_weighted_abatement_supremacy, empirically_contingent).
narrative_ontology:cs_axiom('9652ca90-aba1-4f9e-9ee0-c6918427aa50', secondary, timeline_exceeding_options_net_harmful).
narrative_ontology:cs_axiom_status(timeline_exceeding_options_net_harmful, holdable).
narrative_ontology:cs_axiom_grounding('9652ca90-aba1-4f9e-9ee0-c6918427aa50', timeline_exceeding_options_net_harmful, empirically_contingent).
narrative_ontology:cs_reference_frame('9652ca90-aba1-4f9e-9ee0-c6918427aa50', binding_carbon_budget_triage_frame).
narrative_ontology:cs_drift_state('9652ca90-aba1-4f9e-9ee0-c6918427aa50', post_energy_security_shock, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('9652ca90-aba1-4f9e-9ee0-c6918427aa50', '').
narrative_ontology:cs_kernel_id(climate_mitigation_imperative__opportunity_cost_reading, climate_mitigation_imperative).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_mitigation_imperative__opportunity_cost_reading, renewable_deployment_industry).
narrative_ontology:constraint_beneficiary(climate_mitigation_imperative__opportunity_cost_reading, climate_advocacy_networks).
narrative_ontology:constraint_beneficiary(climate_mitigation_imperative__opportunity_cost_reading, natural_gas_interests).
narrative_ontology:constraint_victim(climate_mitigation_imperative__opportunity_cost_reading, nuclear_energy_sector).
narrative_ontology:constraint_victim(climate_mitigation_imperative__opportunity_cost_reading, electricity_ratepayers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(climate_mitigation_imperative__opportunity_cost_reading, electricity_ratepayers).
narrative_ontology:constraint_victim(climate_mitigation_imperative__opportunity_cost_reading, natural_gas_interests).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Develops, finances, and manufactures utility-scale solar, wind, and storage. Receives the bulk of mitigation capital channeled by speed-ranked procurement rules, subsidy envelopes, and green-taxonomy eligibility. Exit is easy: capital redeploys across markets and jurisdictions, and the industry's product line is precisely what the ranking favors.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__opportunity_cost_reading, renewable_deployment_industry, beneficiary,
    institutional, biographical, mobile, global).

% NGOs, foundations, and campaign coalitions whose theory of change and fundraising are fused to the fastest-abatement frame. Staff taxonomy consultations, shape development-bank conditionality, and supply the public case that slow options waste the decisive decade. Pivoting to a portfolio or technology-neutral frame would repudiate decades of accumulated positioning and sever donor relationships, so exit from the frame is not practically available to them even where intellectually available.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__opportunity_cost_reading, climate_advocacy_networks, beneficiary,
    organized, biographical, identity_locked, global).

% Sells into the interim window wherever nuclear retires ahead of clean firm build-out, capturing backfill volume and extended asset life. Simultaneously the long-run target of the same decarbonization drive the rule serves: an interim winner attached to a terminal threat, able to arbitrage globally through LNG markets while its domestic franchise erodes.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__opportunity_cost_reading, natural_gas_interests, beneficiary,
    institutional, biographical, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(climate_mitigation_imperative__opportunity_cost_reading, natural_gas_interests, payer).

% Operators of existing fleets, new-build developers, and the specialized supply chain. Under speed-ranked criteria they face financing exclusion, unfavorable taxonomy treatment, retirement pressure on working zero-carbon assets, and steady workforce attrition as pipelines thin. Assets are sunk, regulatorily locked, and skill-specific; there is nowhere for a reactor franchise to go.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__opportunity_cost_reading, nuclear_energy_sector, payer,
    organized, generational, trapped, global).

% Pay retail rates shaped by the system build-out the rule directs: absorbing firmness-gap costs, storage and transmission overbuild, and price volatility from weather-dependent supply. They also receive cleaner air and, where renewables mature, eventual bill declines. They cannot exit the grid, and their exposure is mediated through regulators they individually do not control.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__opportunity_cost_reading, electricity_ratepayers, payer,
    moderate, immediate, trapped, national).
narrative_ontology:stakeholder_secondary_role(climate_mitigation_imperative__opportunity_cost_reading, electricity_ratepayers, beneficiary).

% Administer sustainable-finance taxonomies, disclosure regimes, and eligibility screens that operationalize which technologies count as legitimate mitigation spending. Revision requires multi-stakeholder processes and political cover, so the criteria move slowly even when underlying evidence shifts; the seat experiences the rule as procedure while allocating its losses.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__opportunity_cost_reading, green_finance_standard_setters, agenda_setter,
    institutional, generational, constrained, continental).

% Plan industrializing grids under lender conditionality steered by the speed-first frame, advised to build variable-renewable-heavy systems without firm low-carbon anchors. Their objections about firmness needs, import dependence, and land constraints rarely reach the forums where allocation criteria are set; their leverage is limited by dependence on development finance.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__opportunity_cost_reading, developing_grid_planners, excluded,
    moderate, generational, constrained, global).

% Produce the firm-capacity, integration-cost, and learning-rate literature that both camps cite. Positioned to see the full structure at once: the rule's genuine savings in redirected capital and fallen abatement costs, alongside its unpriced system costs and realized backfill emissions.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__opportunity_cost_reading, energy_system_analysts, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(climate_mitigation_imperative__opportunity_cost_reading, renewable_deployment_industry).
narrative_ontology:fixing_cost_class(climate_mitigation_imperative__opportunity_cost_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the capital-scarcity-under-deadline problem: with a binding carbon budget and limited political appetite to fund everything, it supplies a single ranking (carbon avoided per dollar per year) that tells treasuries, development banks, and donors what to build first, and strips legitimacy from spending that slows near-term abatement.
% TRANSFER_FUNCTION: Moves investment capital, subsidy envelopes, policy attention, and regulatory eligibility from slow, capital-intensive firm low-carbon technologies (chiefly nuclear) toward fast-deploying variable renewables and their supply chains; secondarily, wherever nuclear retires before clean firm capacity arrives, it moves electricity-market volume to incumbent fossil generation during the interim window.
% ABSENT_VOICES: Developing-economy grid planners bound by lender conditionality (their firmness objections never reach allocation forums), host regions of retiring nuclear plants (workforce and tax-base losses), energy-poor households exposed to reliability externalities they did not choose, and future generations who inherit both the carbon trajectory and the foregone firm-clean option. All sit outside the foundation-taxonomy-procurement circuit where the rule is maintained.
% DISAPPEARANCE_RATIONALE: If the ranking discipline vanished overnight, mitigation capital would redistribute toward portfolio-weighted allocations: nuclear life-extensions and new-build programs regain financing access, lender conditionality drops speed-only screens, advocacy coalitions reorganize around technology-neutral frames, and jurisdictions mid-phase-out revisit retirement schedules. Renewable deployment continues but its share of marginal dollars falls. The arrangement's absence is immediately visible in allocation decisions, so the world rearranges.
% FOUNDING_PROBLEM: Post-Kyoto climate policy faced scarce political capital against a closing carbon budget: the cheapest, fastest abatement (efficiency, early renewables) was deployable immediately, while nuclear was expensive, slow to build, and politically damaged by Chernobyl and later Fukushima. The rule was built to answer the question of what to fund first with limited dollars and less time, and to deny legitimacy to proposals that would spend the decade's capital on technologies unable to deliver within it.
% FOUNDING_PROBLEM_CORROBORATION: The scarcity-and-urgency half of the founding problem is corroborated from outside the benefiting set by the climate-science community and the carbon-budget literature generally. The nuclear-disqualification corollary, however, is attested almost entirely by the rule's own beneficiaries (renewable industry analyses, advocacy-commissioned cost comparisons); independent firm-capacity literature and the post-2022 security-driven policy reversals attest the opposite. Stated plainly: the urgency premise has outside corroboration; the nuclear corollary currently does not.
narrative_ontology:disappearance_verdict(climate_mitigation_imperative__opportunity_cost_reading, world_rearranges).
narrative_ontology:founding_problem_status(climate_mitigation_imperative__opportunity_cost_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(climate_mitigation_imperative__opportunity_cost_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(climate_mitigation_imperative__opportunity_cost_reading, 'none', 1).
narrative_ontology:epsilon_provenance(climate_mitigation_imperative__opportunity_cost_reading, 0.62, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(climate_mitigation_imperative__opportunity_cost_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(climate_mitigation_imperative__opportunity_cost_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(climate_mitigation_imperative__opportunity_cost_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is 0.62 because the rule's operation concentrates real losses on identifiable parties - financing denial and premature retirement in the nuclear sector, ratepayer exposure to firmness-gap costs, and realized emission increases where fossil generation backfills retired nuclear - while its coordination output (rapid renewable cost decline and mass deployment) is genuine. Suppression (0.55) is structural rather than carceral: taxonomy exclusion, lender conditionality, and procurement screens close the nuclear pathway's access to capital and legitimacy without banning argument; a smaller internalized component persists as professional convention among planners who treat nuclear as presumptively unacceptable. Alternative framings survive in academia and several states, so suppression is high but not total. Accessibility_collapse is 0.45: once the rule is understood, the portfolio and systems alternatives remain visible and actively argued - the sibling readings are live - which is precisely why enforcement must stay active. Resistance is 0.60: nuclear-capable states, post-2022 energy-security turns, SMR entrants, and firm-power-hungry data-center demand push back continuously. Theater_ratio is 0.38: physical deployment is real, but a growing share of regime activity is pledge inflation, accounting performance, and LCOE presentations that substitute for firm-capacity planning. Suppression is authored as a raw structural property and is NOT scaled by power or scope; only extractiveness passes through the engine's directionality and scope scaling. All three temporal series share one grid (0, 5, 10, 15, 20, 25, 30) so no metric row borrows an end-state value at earlier times. The suppression_requirement series is authored because the story specifically traces enforcement-capacity intensification (taxonomy battles, counter-campaign mobilization against nuclear rehabilitation), not merely extraction drift.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute differently. From the renewable-developer and advocacy seats the rule is the discipline that made mitigation financeable - genuine coordination they operate inside. From the nuclear seat the same structure is a financing blockade retiring working zero-carbon assets. From the gas seat it is a transient windfall attached to a terminal threat. Ratepayers straddle: cheaper marginal electrons, costlier firm capacity. The agenda-setter seat experiences the rule as procedure, insulated from the losses it allocates. The engine computes these per-seat types from the power and exit structure; the authored claim adjudicates none of them. Identity-lock note: climate_advocacy_networks carry identity_locked exit - the fusion is ideological-professional (decades of anti-nuclear campaigning and speed-first fundraising make a portfolio pivot self-repudiating and donor-destroying); were the identity frame to break, that seat's directionality would shift sharply toward symmetric.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations map to low d: renewable_deployment_industry (the rule subsidizes it directly, d near the beneficiary end), climate_advocacy_networks (collects relevance and funding though not material throughput, still low d). Victim declarations map to high d: nuclear_energy_sector (full target, trapped, near the full-target end), electricity_ratepayers (bear system costs but receive climate and air-quality benefits, derived d mid-high, moderated by the secondary beneficiary position). natural_gas_interests derives LOW d from its beneficiary role, but its true structural position is mid-range: interim gainer, terminal target of the same decarbonization drive. No directionality_overrides entry is authored because overrides key on power atoms and the only other institutional seat (green_finance_standard_setters) occupies a genuinely different position; a single institutional-atom override would corrupt both. The residual gas misestimate is accepted as a known derivation limit and flagged here rather than papered over with an override.
 *
 * MANDATROPHY ANALYSIS:
 *   Tangling is load-bearing. Labeling the rule a rope would erase its asymmetric incidence - the destroyed nuclear franchises and realized backfill emissions are not coordination costs anyone consented to. Labeling it a snare would erase its real achievement - speed-ranked allocation drove the largest peacetime redirection of energy investment on record and collapsed abatement costs; the coordination function is not cover. The tangled_rope claim preserves both facts and routes the question of whether the extraction is separable from the coordination to the omega set (firm_capacity_value_dispute, backfill_emissions_ratchet) instead of pre-deciding it. Mandatrophy posture: founding_problem_status is contested, not dead - the scarcity the rule answered persists even as its nuclear-disqualification corollary erodes - so the status-by-verdict pairing (contested crossed with world_rearranges) raises no zombie flag. The drift to watch is axiom_overriding in cs_structure: if the empirical challenge completes, this reading risks converting into a piton of former urgency politics maintained rhetorically after its operative content has gone.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_position,
    'This file instantiates only the opportunity_cost_reading of the kernel climate_mitigation_imperative. What would the sibling readings change structurally if instantiated instead?',
    'Compare against the portfolio_optimization_reading and systems_transition_reading files: nuclear moves from victim to necessary pillar (portfolio) or from victim to centralization perpetuator (systems); the beneficiary set and the objective function change with each reading.',
    'Cross-reading comparison is the unit of kernel analysis; merging readings into one story would destroy epsilon-invariance and fabricate a constraint that none of the parties actually holds.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_position, conceptual, 'Committer-frame routing: one reading of a three-reading kernel; sibling deltas documented.').

omega_variable(
    firm_capacity_value_dispute,
    'Does nuclear''s firm, dispatchable, high-density output carry system-level value (capacity credit, integration-cost avoidance, land sparing) large enough to reverse the opportunity-cost verdict once full-system accounting replaces levelized-cost screening?',
    'Whole-system capacity-expansion modeling with firm-low-carbon sensitivity runs, plus revealed preference from jurisdictions procuring firm clean power after 2022.',
    'If firm value reverses the verdict, the rule''s nuclear exclusion is miscalibrated extraction riding on real urgency and classification drifts toward the snare boundary; if not, the verdict stands and the measured costs are the price of speed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(firm_capacity_value_dispute, empirical, 'Whether the reading''s core empirical premise survives whole-system accounting.').

omega_variable(
    backfill_emissions_ratchet,
    'Did speed-ranked retirement of existing nuclear raise cumulative emissions in jurisdictions where fossil generation filled the interim gap before renewables and storage caught up?',
    'Grid-level dispatch data decomposing emissions trajectories against counterfactual fleet-retention runs for the major phase-out jurisdictions.',
    'A positive finding converts part of the rule''s coordination ledger into realized harm and supports tangled_rope-to-snare drift monitoring; a null finding supports a rope-leaning computation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(backfill_emissions_ratchet, empirical, 'Whether the rule''s operation produced realized emission increases via fossil backfill.').

omega_variable(
    urgency_vs_coalition_genealogy,
    'Is the speed-first rule an emergent response to genuine carbon-budget scarcity, or a constructed coalition artifact serving renewable-industry and advocacy interests that adopted urgency language?',
    'Trace funding flows and agenda-setting moments (foundation strategy documents, taxonomy consultation records) against independent scarcity analysis; test whether the rule''s stringency tracks scarcity indicators or coalition fortunes.',
    'A constructed-artifact finding shifts classification toward snare-flavored capture with the urgency framing as cover; an emergent-response finding stabilizes tangled_rope with coordination primacy.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(urgency_vs_coalition_genealogy, empirical, 'Genealogy of the rule: scarcity response versus coalition construction.').

omega_variable(
    authority_grounding_framing_omega,
    'Is the enforcement complex best framed as extraction-grounded authority (an advocacy-finance gatekeeping complex that collects relevance and funding from preventing kernel revision), or as distributed authority (no designated adjudicator among competing modeler, NGO, and finance factions)?',
    'Examine who actually blocks revision attempts: if identifiable gatekeepers with career and funding stakes in the frame repeatedly defeat revision proposals (taxonomy votes, donor reprisals), extraction framing holds; if revision failures are scattered and leaderless, distributed framing holds.',
    'Signals guiding the current choice: taxonomy bodies and foundation program officers exercise consequential gatekeeping with material stakes in the frame, supporting the extraction declaration. If the distributed alternative were adopted, the commitment-system pattern match weakens toward open contestation and the drift computations lose their designated-authority anchor.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(authority_grounding_framing_omega, conceptual, 'CS-framing under-determination: extraction-grounded versus distributed authority for this reading''s enforcement complex.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_mitigation_imperative__opportunity_cost_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cmi_opportunity_cost_tr_t0, climate_mitigation_imperative__opportunity_cost_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(cmi_opportunity_cost_tr_t5, climate_mitigation_imperative__opportunity_cost_reading, theater_ratio, 5, 0.24).
narrative_ontology:measurement(cmi_opportunity_cost_tr_t10, climate_mitigation_imperative__opportunity_cost_reading, theater_ratio, 10, 0.28).
narrative_ontology:measurement(cmi_opportunity_cost_tr_t15, climate_mitigation_imperative__opportunity_cost_reading, theater_ratio, 15, 0.31).
narrative_ontology:measurement(cmi_opportunity_cost_tr_t20, climate_mitigation_imperative__opportunity_cost_reading, theater_ratio, 20, 0.34).
narrative_ontology:measurement(cmi_opportunity_cost_tr_t25, climate_mitigation_imperative__opportunity_cost_reading, theater_ratio, 25, 0.36).
narrative_ontology:measurement(cmi_opportunity_cost_tr_t30, climate_mitigation_imperative__opportunity_cost_reading, theater_ratio, 30, 0.38).

% Extraction over time
narrative_ontology:measurement(cmi_opportunity_cost_be_t0, climate_mitigation_imperative__opportunity_cost_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(cmi_opportunity_cost_be_t5, climate_mitigation_imperative__opportunity_cost_reading, base_extractiveness, 5, 0.4).
narrative_ontology:measurement(cmi_opportunity_cost_be_t10, climate_mitigation_imperative__opportunity_cost_reading, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(cmi_opportunity_cost_be_t15, climate_mitigation_imperative__opportunity_cost_reading, base_extractiveness, 15, 0.54).
narrative_ontology:measurement(cmi_opportunity_cost_be_t20, climate_mitigation_imperative__opportunity_cost_reading, base_extractiveness, 20, 0.58).
narrative_ontology:measurement(cmi_opportunity_cost_be_t25, climate_mitigation_imperative__opportunity_cost_reading, base_extractiveness, 25, 0.61).
narrative_ontology:measurement(cmi_opportunity_cost_be_t30, climate_mitigation_imperative__opportunity_cost_reading, base_extractiveness, 30, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(cmi_opportunity_cost_su_t0, climate_mitigation_imperative__opportunity_cost_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(cmi_opportunity_cost_su_t5, climate_mitigation_imperative__opportunity_cost_reading, suppression_requirement, 5, 0.35).
narrative_ontology:measurement(cmi_opportunity_cost_su_t10, climate_mitigation_imperative__opportunity_cost_reading, suppression_requirement, 10, 0.42).
narrative_ontology:measurement(cmi_opportunity_cost_su_t15, climate_mitigation_imperative__opportunity_cost_reading, suppression_requirement, 15, 0.48).
narrative_ontology:measurement(cmi_opportunity_cost_su_t20, climate_mitigation_imperative__opportunity_cost_reading, suppression_requirement, 20, 0.52).
narrative_ontology:measurement(cmi_opportunity_cost_su_t25, climate_mitigation_imperative__opportunity_cost_reading, suppression_requirement, 25, 0.54).
narrative_ontology:measurement(cmi_opportunity_cost_su_t30, climate_mitigation_imperative__opportunity_cost_reading, suppression_requirement, 30, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_mitigation_imperative__opportunity_cost_reading, resource_allocation).
narrative_ontology:affects_constraint(climate_mitigation_imperative__opportunity_cost_reading, portfolio_optimization_reading).
narrative_ontology:affects_constraint(climate_mitigation_imperative__opportunity_cost_reading, systems_transition_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the natural-language concept 'the climate mitigation imperative' decomposes into three structurally distinct constraints, one per reading of the kernel. This file (opportunity_cost_reading) is the middle node in empirical contention: its upstream premise (binding budgets, scarce capital) is well-corroborated, while its downstream corollary (nuclear is net-harmful) is the contested element challenged by the portfolio sibling's firm-capacity evidence and pressured institutionally by the post-2022 security turn. The siblings are separate files with separate epsilon values and victim sets; linking them via affects_constraints enables contamination analysis (evidence that degrades this reading's corollary propagates to the portfolio reading's vindication and vice versa).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
