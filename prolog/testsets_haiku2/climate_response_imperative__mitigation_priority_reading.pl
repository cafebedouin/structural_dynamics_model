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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
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
 *   human_readable: Climate Mitigation-Priority Imperative: Technological Innovation + Market Mechanisms
 *   domain: climate_policy/political_economy/intergenerational_justice
 *
 * SUMMARY:
 *   The mitigation-priority reading instantiates a specific operative
 *   constraint within the contested climate response kernel: climate action
 *   is framed as solvable primarily through emissions reduction via
 *   technological innovation (renewables, carbon capture, nuclear, energy
 *   efficiency) and market mechanisms (carbon pricing, green bonds,
 *   technology transfer). Adaptation to unavoidable climate impacts is
 *   treated as a residual expense and lower priority. This reading benefits
 *   Global North technology sectors, financial intermediaries, and
 *   intellectual property holders; it extracts from climate-vulnerable
 *   regions (deferred adaptation investment, locked into waiting for
 *   technology solutions), future generations (who inherit the residual
 *   adaptation burden if mitigation fails), and communities whose near-term
 *   resilience needs are starved of funding. The constraint's operation is
 *   actively enforced through multilateral development bank conditionality,
 *   research funding that emphasizes decarbonization over adaptation,
 *   international climate finance architecture, and the exclusion of
 *   alternative readings from mainstream policy forums.
 *
 * KEY AGENTS:
 *   - Global North innovation sectors: institutional power, beneficiary, agenda-setter role, control R&D priorities
 *   - Carbon finance intermediaries: institutional power, beneficiary, capture fee flows from green bonds and carbon markets
 *   - Climate-vulnerable regions: moderate power at best, payer role, trapped exit (geographic exposure), immediate time horizon but deferred adaptation funding
 *   - Future generations: powerless, payer role, trapped exit, civilizational time horizon, excluded from policy voice
 *   - Multilateral development banks: institutional power, agenda-setter, enforce the framework through loan conditionality
 *   - Climate scientists: institutional power, agenda-setter, produce technical basis for feasibility claims about mitigation-primary paths
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_response_imperative__mitigation_priority_reading, 0.68).
domain_priors:suppression_score(climate_response_imperative__mitigation_priority_reading, 0.72).
domain_priors:theater_ratio(climate_response_imperative__mitigation_priority_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_response_imperative__mitigation_priority_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(climate_response_imperative__mitigation_priority_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(climate_response_imperative__mitigation_priority_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(climate_response_imperative__mitigation_priority_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(climate_response_imperative__mitigation_priority_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_response_imperative__mitigation_priority_reading, tangled_rope).
narrative_ontology:human_readable(climate_response_imperative__mitigation_priority_reading, "Climate Mitigation-Priority Imperative: Technological Innovation + Market Mechanisms").
narrative_ontology:topic_domain(climate_response_imperative__mitigation_priority_reading, "climate_policy/political_economy/intergenerational_justice").

domain_priors:requires_active_enforcement(climate_response_imperative__mitigation_priority_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_response_imperative__mitigation_priority_reading, '36c0c84d-77fe-40a8-a4e1-44211407e136').
narrative_ontology:cs_kernel_codification('36c0c84d-77fe-40a8-a4e1-44211407e136', distributed).
narrative_ontology:cs_authority_grounding('36c0c84d-77fe-40a8-a4e1-44211407e136', extraction).
narrative_ontology:cs_interpretation_layer_present('36c0c84d-77fe-40a8-a4e1-44211407e136').
narrative_ontology:cs_reading_relation('36c0c84d-77fe-40a8-a4e1-44211407e136', climate_response_imperative__adaptation_priority_reading, influences).
narrative_ontology:cs_reading_relation('36c0c84d-77fe-40a8-a4e1-44211407e136', climate_response_imperative__degrowth_reading, coexists_with).
narrative_ontology:cs_axiom('36c0c84d-77fe-40a8-a4e1-44211407e136', foundational, technological_innovation_sufficiency).
narrative_ontology:cs_axiom_status(technological_innovation_sufficiency, holdable).
narrative_ontology:cs_axiom_grounding('36c0c84d-77fe-40a8-a4e1-44211407e136', technological_innovation_sufficiency, empirically_contingent).
narrative_ontology:cs_axiom('36c0c84d-77fe-40a8-a4e1-44211407e136', foundational, market_price_carbon_adequacy).
narrative_ontology:cs_axiom_status(market_price_carbon_adequacy, holdable).
narrative_ontology:cs_axiom_grounding('36c0c84d-77fe-40a8-a4e1-44211407e136', market_price_carbon_adequacy, instrumental).
narrative_ontology:cs_axiom('36c0c84d-77fe-40a8-a4e1-44211407e136', secondary, adaptation_residual_acceptable).
narrative_ontology:cs_axiom_status(adaptation_residual_acceptable, holdable).
narrative_ontology:cs_axiom_grounding('36c0c84d-77fe-40a8-a4e1-44211407e136', adaptation_residual_acceptable, empirically_contingent).
narrative_ontology:cs_reference_frame('36c0c84d-77fe-40a8-a4e1-44211407e136', technology_market_climate_solutions_hegemony).
narrative_ontology:cs_drift_state('36c0c84d-77fe-40a8-a4e1-44211407e136', contemporary_adaptation_deficit_visibility, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('36c0c84d-77fe-40a8-a4e1-44211407e136', '').
narrative_ontology:cs_kernel_id(climate_response_imperative__mitigation_priority_reading, climate_response_imperative).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_response_imperative__mitigation_priority_reading, global_north_innovation_sectors).
narrative_ontology:constraint_beneficiary(climate_response_imperative__mitigation_priority_reading, carbon_finance_intermediaries).
narrative_ontology:constraint_beneficiary(climate_response_imperative__mitigation_priority_reading, intellectual_property_holders).
narrative_ontology:constraint_victim(climate_response_imperative__mitigation_priority_reading, climate_vulnerable_regions).
narrative_ontology:constraint_victim(climate_response_imperative__mitigation_priority_reading, future_generations).
narrative_ontology:constraint_victim(climate_response_imperative__mitigation_priority_reading, near_term_adaptation_capacities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(climate_response_imperative__mitigation_priority_reading, global_south_low_carbon_development).
narrative_ontology:constraint_victim(climate_response_imperative__mitigation_priority_reading, global_south_low_carbon_development).
narrative_ontology:constraint_victim(climate_response_imperative__mitigation_priority_reading, global_north_workers_in_fossil_sectors).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Advanced technology companies, renewable energy manufacturers, carbon capture vendors, and financial engineers in high-income nations benefit from the framing that climate response is solvable through innovation and markets. Patent protections, licensing revenue, and green finance flows concentrate in these sectors. They set the policy agenda through technical advisory roles, funding of climate research emphasizing technological solutions, and dominance of international climate governance forums.
narrative_ontology:constraint_stakeholder(climate_response_imperative__mitigation_priority_reading, global_north_innovation_sectors, beneficiary,
    institutional, biographical, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(climate_response_imperative__mitigation_priority_reading, global_north_innovation_sectors, agenda_setter).

% Banks, asset managers, and climate funds profit from carbon markets, green bonds, and technology transfer finance mechanisms. They benefit from the market-based framing because it generates tradeable assets and financial intermediation opportunities. Their fee collection depends on the constraint's persistence as the primary climate governance mode.
narrative_ontology:constraint_stakeholder(climate_response_imperative__mitigation_priority_reading, carbon_finance_intermediaries, beneficiary,
    institutional, biographical, arbitrage, global).

% Patent holders and technology licensors in emissions-reduction technology benefit from market mechanisms that respect IP protections and from the assumption that technology transfer will occur via licensing rather than open knowledge. They extract rents from the constraint's framework that property rights must be preserved to incentivize innovation.
narrative_ontology:constraint_stakeholder(climate_response_imperative__mitigation_priority_reading, intellectual_property_holders, beneficiary,
    institutional, generational, arbitrage, global).

% Low-lying island nations, sub-Saharan Africa, South Asia, and other regions facing immediate climate impacts pay through deferred adaptation resources. Under the mitigation-priority framework, adaptation is treated as a residual expense to be addressed after emissions are reduced—a timeline that may span decades. They bear the costs of infrastructure loss, agricultural displacement, and water stress in the near term while the innovation-driven mitigation strategy develops. Their exit options are blocked by geographic exposure and lack of capital to relocate or self-insure.
narrative_ontology:constraint_stakeholder(climate_response_imperative__mitigation_priority_reading, climate_vulnerable_regions, payer,
    moderate, immediate, trapped, regional).

% Unborn and currently young populations inherit both the climate impacts unmitigated in the present and the residual adaptation deficits. The mitigation-priority framework distributes the cost of present inaction backward in time—betting that technology and market mechanisms will successfully scale in the future to reduce atmospheric CO₂. If the bet fails (unproven CDR technologies do not scale, markets fail to price carbon correctly, innovation stalls), future generations absorb the adaptation burden. Their voice is entirely absent from present policy-making.
narrative_ontology:constraint_stakeholder(climate_response_imperative__mitigation_priority_reading, future_generations, payer,
    powerless, civilizational, trapped, global).

% Communities in developing nations that could implement resilience measures—water systems, crop breeding, coastal defense, early warning—face chronic underfunding because adaptation budgets are starved relative to mitigation finance. Donors and multilateral institutions prioritize emissions reduction projects, leaving adaptation funding far below assessed needs. These actors must choose between accepting climate risk or diverting limited development resources from poverty reduction.
narrative_ontology:constraint_stakeholder(climate_response_imperative__mitigation_priority_reading, near_term_adaptation_capacities, payer,
    organized, biographical, constrained, regional).

% Developing economies can access green finance and technology transfer under the mitigation-priority framework if their development paths emphasize low-carbon alternatives. They benefit from renewable energy investment and technology flows. However, the high cost of clean technology, IP protection barriers, and the constraint that development must be decoupled from emissions means they must absorb adaptation risk while waiting for technology costs to fall—a cost-benefit tradeoff not available to countries that industrialized via cheap fossil fuels.
narrative_ontology:constraint_stakeholder(climate_response_imperative__mitigation_priority_reading, global_south_low_carbon_development, beneficiary,
    moderate, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(climate_response_imperative__mitigation_priority_reading, global_south_low_carbon_development, payer).

% Research institutions and climate modelers whose funding and authority depend on establishing the feasibility of low-carbon pathways and technological solutions. Their ability to credibly project a 1.5°C scenario with available technology and market mechanisms determines policy plausibility. They set research priorities, define what counts as climate ambition, and produce the technical basis for mitigation-centric policy. Career advancement and institutional funding ride on demonstrating a technologically and economically viable mitigation path.
narrative_ontology:constraint_stakeholder(climate_response_imperative__mitigation_priority_reading, climate_scientists_modeling_communities, agenda_setter,
    institutional, generational, mobile, global).

% World Bank, regional development banks, and IMF operationalize the mitigation-priority reading through their lending conditionality, climate finance windows, and infrastructure financing criteria. They enforce the framework through loan agreements that mandate emissions reduction pathways, carbon pricing, and technology adoption as conditions for development finance. They control what counts as eligible investment in developing nations.
narrative_ontology:constraint_stakeholder(climate_response_imperative__mitigation_priority_reading, multilateral_development_banks, agenda_setter,
    institutional, generational, mobile, global).

% Coal, oil, and gas companies are excluded from the mitigation-priority framework's beneficiary set but remain structurally powerful. They would argue for adaptation-as-primary or for continued fossil fuel use with carbon capture, but are increasingly shut out of climate policy forums. Their exclusion is enforced through investor divestment pressure, regulatory phase-outs, and reputational shunning, yet they retain geopolitical leverage through energy supply control.
narrative_ontology:constraint_stakeholder(climate_response_imperative__mitigation_priority_reading, incumbent_fossil_fuel_interests, excluded,
    institutional, biographical, constrained, global).

% Communities whose climate knowledge, land management, and adaptive capacity are critical to both mitigation and adaptation are largely excluded from global policy design. Their voices appear in climate governance documents but do not determine priorities or resource allocation. Under the mitigation-priority frame, their traditional adaptation knowledge is treated as supplementary rather than central, and their land rights are at risk if carbon offsetting or large-scale renewable projects claim their territories.
narrative_ontology:constraint_stakeholder(climate_response_imperative__mitigation_priority_reading, indigenous_and_local_communities, excluded,
    powerless, generational, trapped, local).

% Workers in coal mining, oil refining, and gas infrastructure in wealthy nations face displacement as the mitigation-priority agenda accelerates phase-outs. They are partially included via 'just transition' rhetoric but often excluded from genuine policy negotiation. The constraint's operation increases their adaptation burden (job loss, retraining costs) while the primary benefit of mitigation accrues to technology sectors and financial intermediaries.
narrative_ontology:constraint_stakeholder(climate_response_imperative__mitigation_priority_reading, global_north_workers_in_fossil_sectors, excluded,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(climate_response_imperative__mitigation_priority_reading, global_north_workers_in_fossil_sectors, payer).

% Independent climate and ecological researchers who assess the feasibility of mitigation-primary pathways, the scalability of carbon dioxide removal, and the adequacy of adaptation funding under the current allocation. They produce diagnostic evidence on whether the constraint's operative logic (technology + markets = sufficient response) is empirically sound.
narrative_ontology:constraint_stakeholder(climate_response_imperative__mitigation_priority_reading, observer_ecological_science, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(climate_response_imperative__mitigation_priority_reading, global_north_innovation_sectors).
narrative_ontology:fixing_cost_class(climate_response_imperative__mitigation_priority_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a unified global commitment that climate response is solvable primarily through emissions reduction via technology innovation and market pricing, coordinating investment flows, research priorities, and national pledges around decarbonization pathways that preserve property rights, technological competition, and growth trajectories.
% TRANSFER_FUNCTION: Transfers wealth from climate-vulnerable regions and future generations (who bear deferred adaptation costs and higher residual climate impacts) to Global North innovation sectors, IP holders, and financial intermediaries (who profit from green technology licensing, carbon markets, and green finance). Also transfers authority from vulnerable-region voices to technology-expert and finance-sector voices in climate governance.
% ABSENT_VOICES: Climate-vulnerable regions have observer seats but not decision power. Indigenous and local knowledge holders are acknowledged but not determinative. Future generations cannot speak. Workers in fossil fuel transitions are nominally included in 'just transition' discourse but excluded from agenda-setting. Ecological scientists who project adaptation inadequacy under current funding are marginalized relative to modelers demonstrating technological feasibility.
% DISAPPEARANCE_RATIONALE: If the mitigation-priority imperative and its market mechanisms were dissolved overnight, global climate finance would redistribute toward adaptation and resilience in vulnerable regions; research priorities would rebalance toward local adaptive capacity and ecological restoration; development finance conditionality would no longer require decarbonization as a prerequisite; and vulnerable nations would shift resources from waiting for technology solutions to immediate protective infrastructure. The world would rearrange toward a polyglot response mixing adaptation, mitigation, and degrowth pressures simultaneously.
% FOUNDING_PROBLEM: Climate change poses an existential risk to global civilization. In the 1990s–2000s, the problem was framed as solvable through technological innovation and market mechanisms (Kyoto Protocol, carbon markets) because these tools respected the sovereignty of wealthy nations, preserved property rights, and promised to solve the crisis without requiring structural economic change or redistribution. The founding problem was both ecological (rising CO₂) and political (how to coordinate global action without economic disruption to incumbent powers).
% FOUNDING_PROBLEM_CORROBORATION: Climate scientists attest the ecological founding problem (rising CO₂, climate tipping points) remains live. Technology proponents and financial intermediaries attest the market-mechanism framing still works if scaled up. Adaptation-priority advocates, degrowth scholars, and vulnerable-region delegates attest the founding problem is PARTIALLY solved (emissions have decoupled from growth in some wealthy nations) but the POLITICAL founding problem—how to coordinate response without disrupting the North's economic order—has SHIFTED: that framing now OBSTRUCTS adequate adaptation and perpetuates injustice, becoming itself an obstacle to solving the ecological crisis. Indigenous knowledge holders and ecological scientists attest that the technological optimism underestimating system complexity and overestimating the feasibility of carbon dioxide removal at required scales.
narrative_ontology:disappearance_verdict(climate_response_imperative__mitigation_priority_reading, world_rearranges).
narrative_ontology:founding_problem_status(climate_response_imperative__mitigation_priority_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(climate_response_imperative__mitigation_priority_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(climate_response_imperative__mitigation_priority_reading, 'none', 1).
narrative_ontology:epsilon_provenance(climate_response_imperative__mitigation_priority_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Base extractiveness is high and rising (0.35→0.75 over 1990–2050) because the constraint directs climate finance toward innovation and green bonds, away from adaptation and near-term resilience in vulnerable regions. The constraint's beneficiaries (tech sectors, finance) capture the wealth flows while the payers (vulnerable regions, future generations) bear deferred costs. Suppression is substantial (0.45→0.81) and rising because the mitigation-priority framing is actively defended through research funding allocation, exclusion of adaptation-primary voices from policy forums, and conditionality imposed by development banks on developing-nation borrowing. Theater rises faster (0.25→0.58) than suppression does, signaling that performative climate action (net-zero pledges without binding emissions reductions, carbon offset projects of dubious additionality, green bonds that finance expansion of extractive industries) grows as the gap between the framing and ecological outcomes widens. The measurement series model a 60-year interval (industrial climate governance era, 1990–2050) with historical data to 2025 and projections thereafter assuming the mitigation-priority reading holds despite increasing challenge.
 *
 * PERSPECTIVAL GAP:
 *   From the beneficiary seat (Global North innovation sector, multilateral bank economist), the constraint is genuine coordination: it solved the political problem of getting wealthy nations to commit to climate action without requiring structural economic change, and it mobilized trillions in green finance that would not have emerged from purely national action. From the payer seats (vulnerable regions, future generations, near-term adaptation needs), the same constraint is extraction dressed as cooperation: it locks in a timeline for emissions reduction that may fail, defers their most pressing needs (adaptive infrastructure NOW), and transfers the risk of technological failure backward to those with no voice in the decision. The engine computes this divergence from the structural data. Vulnerable-region delegates in climate forums report that their adaptation priorities are consistently deprioritized in favor of mitigation projects; the gap is real and structural, not a perception error.
 *
 * DIRECTIONALITY LOGIC:
 *   Global North sectors derive high benefit (subsidy through green finance flows, IP protection, market share) and low cost (they can afford the innovation strategy, which aligns with their competitive advantages) → d near 0.0 (beneficiary). Climate-vulnerable regions and future generations derive low benefit (deferred promises of reduced warming, contingent on technological success) and high cost (adaptation deficits, climate impacts, adaptation burden) → d near 1.0 (target). Multilateral banks as agenda-setters occupy d near 0.1–0.2 (they administer the system and collect administrative rents, but face pressure from vulnerable-region constituencies to rebalance); their secondary role as partially captured by Northern interests and Northern development models pushes them toward beneficiary status despite nominally neutral governance. Adaptation-funding communities, fossil fuel workers, and indigenous peoples are payers or excluded—all shifted toward high d. No directionality override needed; the structural data derive correctly.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem—that climate change requires urgent global action without economic disruption to wealthy nations—has PARTIALLY evolved. The ecological problem (rising CO₂) remains live. But the POLITICAL founding problem that the mitigation-priority framing was built to solve (how to coordinate without redistribution) has become an OBSTACLE rather than a solution. The vulnerable-region payees now attest that the founding problem's solution is failing because it defers their survival needs. The constraint persists not because it still solves the original political problem (wealthy nations are increasingly divided on climate commitment anyway, and the framing no longer prevents disruption—energy transitions create job losses, geopolitical power shifts, and degrowth pressures regardless), but because the beneficiaries (tech sectors, finance) now have a structural interest in its maintenance. The R5 mismatch is: founding_problem_status='contested' and disappearance_verdict='world_rearranges'. This signals a zombie constraint: the original justification is lost or in dispute, but the arrangement persists because powerful actors profit from it. The constraint exhibits incipient mandatrophy.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    carbon_dioxide_removal_feasibility,
    'Can carbon dioxide removal (CDR) technologies scale to 10+ Gt CO₂/year by 2050 at economically sustainable costs, and what is the probability that mitigation-primary pathways depend critically on unproven technology?',
    'Empirical tracking of CDR deployment costs, technological breakthroughs in direct air capture and enhanced weathering, and reconciliation with integrated assessment model (IAM) assumptions. Real-time comparison of projected vs. realized CDR cost curves.',
    'If CDR remains expensive and slow to scale, the mitigation-priority reading''s mathematical foundation collapses; adaptation-priority and degrowth readings become unavoidable. If CDR scales as projected, the reading gains vindication and the victim set shrinks. This is THE empirical linchpin for the constraint''s persistence.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(carbon_dioxide_removal_feasibility, empirical, 'Feasibility and scalability of unproven carbon dioxide removal technology that the mitigation-primary pathway depends on.').

omega_variable(
    green_growth_decoupling_sufficiency,
    'Is relative decoupling of emissions from GDP growth in wealthy nations sufficient to meet Paris Agreement targets without absolute consumption reduction, or do current decoupling rates require structural economic change to supplement?',
    'Continued tracking of emissions-to-GDP ratios, carbon intensity improvements, and climate modeling showing whether observed decoupling rates can deliver adequate global emissions reductions if universalized to developing nations.',
    'If relative decoupling is sufficient, the market-mechanism reading holds and growth can continue. If absolute reduction is required, the degrowth reading becomes unavoidable as a structural condition; the mitigation-priority reading becomes cover for inaction. This determines whether the constraint''s framing is empirically sustainable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(green_growth_decoupling_sufficiency, empirical, 'Whether growth-compatible emissions reduction is sufficient or if structural economic transformation is required.').

omega_variable(
    adaptation_funding_adequacy_under_priority_deferral,
    'What is the actual cost of adequate climate adaptation in vulnerable regions over the next 25 years, and can that cost be met from the residual adaptation budget that remains after mitigation finance is allocated under the priority framework?',
    'Independent assessment of adaptation needs vs. funded adaptation projects. Empirical tracking of whether near-term climate impacts (floods, droughts, disease, displacement) are mitigated to acceptable levels under the current allocation.',
    'If the adaptation gap is catastrophic and unavoidable under the current allocation, the victim set expands and the constraint becomes unjust even on the mitigation-priority''s own coordination logic (it fails to solve the founding problem if it creates unmanageable adaptation deficits). This reframes the constraint from coordination to pure extraction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(adaptation_funding_adequacy_under_priority_deferral, empirical, 'Whether deferred adaptation budgets are adequate to prevent catastrophic impacts in vulnerable regions.').

omega_variable(
    reading_kernel_boundary_contention,
    'Is the mitigation-priority reading a coherent alternative framing of the same climate response kernel, or is it a redefinition of the kernel itself to exclude questions about economic structure and intergenerational equity that sibling readings treat as foundational?',
    'Conceptual analysis of what the three readings take the ''climate response'' problem to be: is it identical across readings (same problem, different solutions) or does each reading constitute a different problem? Examine how each reading''s beneficiaries and victims are chosen—do they arise from the problem framing, or does the framing arise from beneficiary/victim pre-selection?',
    'If the readings are incommensurable (different kernels dressed as the same), the claim that they are ''coexisting readings'' is false; the constraint is not one reading of a shared kernel but a redefinition that forecloses others. This affects whether the reading''s cs_structure.reading_relations are correctly modeled as coexists_with or should be forecloses.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_kernel_boundary_contention, conceptual, 'Whether the three readings contest a shared kernel or instead constitute competing kernel definitions.').

omega_variable(
    suppression_mechanism_structural_vs_internalized,
    'Is the suppression of adaptation-priority and degrowth voices in climate governance structurally enforced (development bank conditionality, research funding allocation, voting rules in climate conferences) or internalized (developing-nation leaders have adopted the mitigation-priority reading as their own legitimate framework)?',
    'Comparative analysis: do vulnerable-region advocates WANT the adaptation-primary reading to be dominant but are prevented by structural barriers, or have they internalized the mitigation-priority framing as legitimate? Examine post-exit trajectories: if structural suppression were removed (open research funding, no development bank conditions, voice equality in climate forums), would adaptation-primary advocacy surge or remain muted?',
    'If suppression is structural, the measured suppression coefficient (0.45→0.81) is an accurate measure of enforcement intensity. If partially internalized, the internal portion travels with the payees even after structural barriers are removed—the constraint''s true suppression cost is higher than the structural measure alone suggests. This affects whether the constraint is classifiable as snare (structural + internalized) vs. tangled_rope (structural only).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_vs_internalized, empirical, 'Whether suppression of alternative climate readings is structural (external barriers) or internalized (adopted frameworks).').

omega_variable(
    reading_relation_coexists_vs_forecloses_mitigation_degrowth,
    'Does the mitigation-priority reading structurally foreclose the degrowth reading within a single governance or institutional framework, or do the two coexist as incompatible framings held by different parties?',
    'Logical analysis: can an institution simultaneously commit to both ''emissions reduction via market growth'' (mitigation-priority) and ''structural economic contraction in the Global North'' (degrowth)? Or are they logically incompatible? Historical/empirical: do any multilateral institutions, national governments, or research communities hold both readings simultaneously, or is allegiance always to one or the other?',
    'If forecloses: the relationship_relations axiom should read forecloses, not coexists_with. If coexists: different parties hold different readings and neither rules out the other in isolation. This omega documents the uncertainty about whether the axiom relation is correctly modeled.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_relation_coexists_vs_forecloses_mitigation_degrowth, conceptual, 'Whether mitigation-priority and degrowth readings are logically incompatible or coexist across different holders.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_response_imperative__mitigation_priority_reading, 1990, 2050).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clim_tr_t1990, climate_response_imperative__mitigation_priority_reading, theater_ratio, 1990, 0.25).
narrative_ontology:measurement(clim_tr_t2005, climate_response_imperative__mitigation_priority_reading, theater_ratio, 2005, 0.32).
narrative_ontology:measurement(clim_tr_t2015, climate_response_imperative__mitigation_priority_reading, theater_ratio, 2015, 0.41).
narrative_ontology:measurement(clim_tr_t2025, climate_response_imperative__mitigation_priority_reading, theater_ratio, 2025, 0.48).
narrative_ontology:measurement(clim_tr_t2040, climate_response_imperative__mitigation_priority_reading, theater_ratio, 2040, 0.54).
narrative_ontology:measurement(clim_tr_t2050, climate_response_imperative__mitigation_priority_reading, theater_ratio, 2050, 0.58).

% Extraction over time
narrative_ontology:measurement(clim_be_t1990, climate_response_imperative__mitigation_priority_reading, base_extractiveness, 1990, 0.35).
narrative_ontology:measurement(clim_be_t2005, climate_response_imperative__mitigation_priority_reading, base_extractiveness, 2005, 0.48).
narrative_ontology:measurement(clim_be_t2015, climate_response_imperative__mitigation_priority_reading, base_extractiveness, 2015, 0.62).
narrative_ontology:measurement(clim_be_t2025, climate_response_imperative__mitigation_priority_reading, base_extractiveness, 2025, 0.68).
narrative_ontology:measurement(clim_be_t2040, climate_response_imperative__mitigation_priority_reading, base_extractiveness, 2040, 0.72).
narrative_ontology:measurement(clim_be_t2050, climate_response_imperative__mitigation_priority_reading, base_extractiveness, 2050, 0.75).

% Suppression requirement over time
narrative_ontology:measurement(clim_su_t1990, climate_response_imperative__mitigation_priority_reading, suppression_requirement, 1990, 0.45).
narrative_ontology:measurement(clim_su_t2005, climate_response_imperative__mitigation_priority_reading, suppression_requirement, 2005, 0.58).
narrative_ontology:measurement(clim_su_t2015, climate_response_imperative__mitigation_priority_reading, suppression_requirement, 2015, 0.67).
narrative_ontology:measurement(clim_su_t2025, climate_response_imperative__mitigation_priority_reading, suppression_requirement, 2025, 0.72).
narrative_ontology:measurement(clim_su_t2040, climate_response_imperative__mitigation_priority_reading, suppression_requirement, 2040, 0.77).
narrative_ontology:measurement(clim_su_t2050, climate_response_imperative__mitigation_priority_reading, suppression_requirement, 2050, 0.81).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_response_imperative__mitigation_priority_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(climate_response_imperative__mitigation_priority_reading, 0.18).
narrative_ontology:affects_constraint(climate_response_imperative__mitigation_priority_reading, climate_response_imperative__adaptation_priority_reading).
narrative_ontology:affects_constraint(climate_response_imperative__mitigation_priority_reading, climate_response_imperative__degrowth_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the climate_response_imperative kernel. The three readings decompose the contested question 'what should the primary global climate response prioritize?' into three structurally distinct constraints with different beneficiary/victim sets, different ε values, and different operative logics. They are linked via the kernel and via network.affects_constraints. The decomposition follows the ε-invariance principle: a single term ('climate response') masks three structurally incommensurable claims, each with its own ε. The mitigation-priority reading treats technological innovation and market mechanisms as primary (high ε because it defers adaptation and extracts from vulnerable regions); the adaptation-priority reading treats resilience and damage reduction as primary (different ε, different victim set); the degrowth reading treats structural economic transformation as necessary (yet different ε and beneficiary set). Each story instantiates its reading cleanly; the kernel contest is modeled via cs_structure.reading_relations and omegas.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
