% ============================================================================
% CONSTRAINT STORY: climate_mitigation_imperative__opportunity_cost_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_climate_mitigation_opportunity_cost, []).

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
 *   human_readable: Climate Mitigation Opportunity Cost Constraint (Opportunity-Cost Reading)
 *   domain: energy_policy/climate_mitigation/technology_governance
 *
 * SUMMARY:
 *   This constraint embodies ONE READING of a contested kernel: the climate
 *   mitigation imperative. This specific reading frames mitigation as a race
 *   against cumulative carbon budgets, making deployment speed per dollar the
 *   dominant efficiency metric. Under this frame, nuclear power enters the
 *   VICTIM set because its capital intensity and 10–15 year timelines mean
 *   each dollar spent on nuclear is a dollar NOT deployed to faster
 *   alternatives (solar, wind, battery storage) that could abate carbon more
 *   quickly. Fast-deployment renewables are beneficiaries because the metric
 *   structurally prioritizes their investment. The reading is a real force in
 *   climate policy: it dominates World Bank climate finance criteria, many
 *   national climate plans, and renewable-advocacy organizations' framing.
 *   However, it coexists with two competing readings: portfolio-optimization
 *   (which places nuclear in the beneficiary set for reliability/diversity)
 *   and systems-transition (which frames nuclear as extractively
 *   centralized). The three readings share the same foundational kernel—that
 *   mitigation is urgent and necessary—but diverge sharply on what 'efficient
 *   mitigation' means and thus on which technologies are costs vs. benefits.
 *
 * KEY AGENTS:
 *   - Fast-deployment renewables sector: Organized beneficiary controlling the metric's operationalization through climate finance mechanisms
 *   - Nuclear industry: Institutional victim, bears allocation priority deprioritization and regulatory friction
 *   - Capital gatekeepers: Institutional agenda-setter; operationalizes the metric through climate finance, multilateral development bank policies
 *   - Decarbonization speed maximizers: Analytical beneficiary; produces legitimating analyses that justify metric adoption
 *   - Baseload reliability advocates: Conflicted; benefit from low-carbon adoption but excluded from metric design
 *   - Long-term industrial planners: Excluded; their 30+ year horizons and security concerns fall outside the metric's frame
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_mitigation_imperative__opportunity_cost_reading, 0.68).
domain_priors:suppression_score(climate_mitigation_imperative__opportunity_cost_reading, 0.54).
domain_priors:theater_ratio(climate_mitigation_imperative__opportunity_cost_reading, 0.31).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_mitigation_imperative__opportunity_cost_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(climate_mitigation_imperative__opportunity_cost_reading, suppression_requirement, 0.54).
narrative_ontology:constraint_metric(climate_mitigation_imperative__opportunity_cost_reading, theater_ratio, 0.31).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(climate_mitigation_imperative__opportunity_cost_reading, accessibility_collapse, 0.41).
narrative_ontology:constraint_metric(climate_mitigation_imperative__opportunity_cost_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_mitigation_imperative__opportunity_cost_reading, tangled_rope).
narrative_ontology:human_readable(climate_mitigation_imperative__opportunity_cost_reading, "Climate Mitigation Opportunity Cost Constraint (Opportunity-Cost Reading)").
narrative_ontology:topic_domain(climate_mitigation_imperative__opportunity_cost_reading, "energy_policy/climate_mitigation/technology_governance").

domain_priors:requires_active_enforcement(climate_mitigation_imperative__opportunity_cost_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_mitigation_imperative__opportunity_cost_reading, '0794aca6-6b3d-4466-8f36-8f160eb575e3').
narrative_ontology:cs_kernel_codification('0794aca6-6b3d-4466-8f36-8f160eb575e3', distributed).
narrative_ontology:cs_authority_grounding('0794aca6-6b3d-4466-8f36-8f160eb575e3', expertise).
narrative_ontology:cs_interpretation_layer_present('0794aca6-6b3d-4466-8f36-8f160eb575e3').
narrative_ontology:cs_reading_relation('0794aca6-6b3d-4466-8f36-8f160eb575e3', climate_mitigation_imperative__portfolio_optimization_reading, coexists_with).
narrative_ontology:cs_reading_relation('0794aca6-6b3d-4466-8f36-8f160eb575e3', climate_mitigation_imperative__systems_transition_reading, coexists_with).
narrative_ontology:cs_axiom('0794aca6-6b3d-4466-8f36-8f160eb575e3', foundational, deployment_speed_is_primary_efficiency).
narrative_ontology:cs_axiom_status(deployment_speed_is_primary_efficiency, holdable).
narrative_ontology:cs_axiom_grounding('0794aca6-6b3d-4466-8f36-8f160eb575e3', deployment_speed_is_primary_efficiency, empirically_contingent).
narrative_ontology:cs_axiom('0794aca6-6b3d-4466-8f36-8f160eb575e3', secondary, urgency_forecloses_portfolio_optionality).
narrative_ontology:cs_axiom_status(urgency_forecloses_portfolio_optionality, holdable).
narrative_ontology:cs_axiom_grounding('0794aca6-6b3d-4466-8f36-8f160eb575e3', urgency_forecloses_portfolio_optionality, empirically_contingent).
narrative_ontology:cs_reference_frame('0794aca6-6b3d-4466-8f36-8f160eb575e3', carbon_speed_optimization_framework).
narrative_ontology:cs_drift_state('0794aca6-6b3d-4466-8f36-8f160eb575e3', contemporary_2024_2026, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('0794aca6-6b3d-4466-8f36-8f160eb575e3', '').
narrative_ontology:cs_kernel_id(climate_mitigation_imperative__opportunity_cost_reading, climate_mitigation_imperative).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_mitigation_imperative__opportunity_cost_reading, fast_deployment_renewables_sector).
narrative_ontology:constraint_beneficiary(climate_mitigation_imperative__opportunity_cost_reading, decarbonization_speed_maximizers).
narrative_ontology:constraint_beneficiary(climate_mitigation_imperative__opportunity_cost_reading, distributed_energy_advocates).
narrative_ontology:constraint_victim(climate_mitigation_imperative__opportunity_cost_reading, nuclear_industry).
narrative_ontology:constraint_victim(climate_mitigation_imperative__opportunity_cost_reading, capital_intensive_baseload_technologies).
narrative_ontology:constraint_victim(climate_mitigation_imperative__opportunity_cost_reading, long_timeline_infrastructure_programs).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(climate_mitigation_imperative__opportunity_cost_reading, baseload_reliability_advocates).
narrative_ontology:constraint_victim(climate_mitigation_imperative__opportunity_cost_reading, baseload_reliability_advocates).
narrative_ontology:constraint_victim(climate_mitigation_imperative__opportunity_cost_reading, grid_integration_cost_bearers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Solar panel manufacturers, wind turbine makers, battery storage companies, and project developers. They produce technologies deployable in 2–4 years with manufacturing capacity that scales rapidly. Under this reading, capital and policy flow toward their sector because they score highest on the carbon-per-dollar-per-year metric. They defend the metric actively through industry organizations and climate advocacy, framing it as the only rational efficiency measure given urgency constraints. Their market position strengthens as the metric becomes global policy standard.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__opportunity_cost_reading, fast_deployment_renewables_sector, beneficiary,
    organized, biographical, mobile, global).
narrative_ontology:stakeholder_secondary_role(climate_mitigation_imperative__opportunity_cost_reading, fast_deployment_renewables_sector, agenda_setter).

% Nuclear reactor manufacturers (Westinghouse, EDF, Russia State Atomic), utilities operating existing fleets, fuel suppliers, waste management entities. They face deprioritization in climate finance, regulatory barriers framed around speed metrics, and growing perception that nuclear is 'too slow.' New reactor projects are starved of capital while renewables are prioritized, even when the nuclear project would eventually deliver more low-carbon energy. Their exit option is constrained because they cannot quickly pivot to fast-deployment technologies; their capital stock and expertise are locked into long-timeline systems. The constraint directly harms their sector's expansion prospects.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__opportunity_cost_reading, nuclear_industry, payer,
    institutional, generational, constrained, global).

% Climate scientists and energy economists (IPCC contributors, university research groups, think-tank analysts) who frame mitigation as a race against cumulative carbon. They produce legitimating analyses arguing that every year of delay increases damage, making speed of carbon deployment the only rational optimization target. Their analytical authority shapes policy; their framing is embedded in climate science consensus narratives. They benefit from adoption of speed-centric metrics because it validates their urgency framing.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__opportunity_cost_reading, decarbonization_speed_maximizers, beneficiary,
    analytical, civilizational, analytical, global).

% World Bank, International Finance Corporation, bilateral development banks, Global Climate Fund. They operationalize the opportunity-cost reading by adopting carbon-per-dollar-per-year as the primary allocation criterion in climate finance. They enforce the metric through grant programs, concessional lending criteria, and investment frameworks. Their enforcement is what makes the metric binding: nuclear projects that would receive private capital are excluded from climate finance, which is the limiting factor for many developing-world nuclear programs.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__opportunity_cost_reading, multilateral_climate_finance_gatekeepers, agenda_setter,
    institutional, generational, trapped, global).

% Grid operators, traditional utilities, electricity system planners who argue that reliable, dispatchable power is essential for grid stability. They benefit from any low-carbon adoption and from nuclear's high capacity factor (avoiding curtailment). But they are payers because the opportunity-cost metric deprioritizes dispatchable generation in favor of renewables, requiring them to invest heavily in storage and frequency-management services that the metric treats as external costs. They argue the metric's system boundary is artificially narrow.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__opportunity_cost_reading, baseload_reliability_advocates, payer,
    organized, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(climate_mitigation_imperative__opportunity_cost_reading, baseload_reliability_advocates, beneficiary).

% Grid operators and system planners who manage integration of high renewable penetration. They face escalating costs for frequency regulation, ramping, voltage support, and storage. The opportunity-cost metric does not price these costs into the efficiency calculation; they are treated as separable from carbon metrics. The payer is the grid operator or ratepayer who bears these integration costs. Their exit is constrained because they cannot choose the technology mix; it is set by capital allocation upstream.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__opportunity_cost_reading, grid_integration_cost_bearers, payer,
    moderate, biographical, constrained, regional).

% Government industrial policy makers, national energy security planners, manufacturing base developers. They are excluded from the opportunity-cost metric's decision frame because their optimization targets (domestic technology sovereignty, 30–50 year industrial capacity, supply chain resilience) do not fit the per-dollar-per-year metric. If seated at the table, they would argue for technology diversity and domestic manufacturing, which the metric structurally disadvantages by prioritizing speed-to-deployment over industrial base development.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__opportunity_cost_reading, long_term_industrial_planners, excluded,
    institutional, civilizational, constrained, national).

% Defense and security policy analysts who view energy supply resilience, uranium security, and rare-earth supply chains as geopolitical goods. They are excluded because security considerations are not priced into the carbon metric. Their inclusion would reframe technology assessment to include supply-chain concentration (rare earths in renewables; uranium in nuclear) as a risk dimension, which could alter the victim/beneficiary structure by highlighting developing-world supply-chain dependency.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__opportunity_cost_reading, geopolitical_energy_security_analysts, excluded,
    analytical, generational, analytical, global).

% Electrification advocates, universal energy access organizations, development NGOs. They are excluded because the opportunity-cost metric deprioritizes dispatchable baseload power, which developing nations argue is necessary for reliable rural electrification. Their inclusion would shift the frame toward total-system reliability and access speed, not marginal carbon efficiency, which could reposition nuclear as a necessary component for energy security.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__opportunity_cost_reading, developing_world_electricity_access_advocates, excluded,
    moderate, biographical, constrained, regional).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(climate_mitigation_imperative__opportunity_cost_reading, fast_deployment_renewables_sector).
narrative_ontology:fixing_cost_class(climate_mitigation_imperative__opportunity_cost_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the capital-scarcity problem in climate mitigation by establishing a single efficiency metric (carbon per dollar per year) that directs limited investment flows toward technologies that abate emissions fastest. Eliminates decision paralysis from competing technology claims by ranking all mitigation options on one scale.
% TRANSFER_FUNCTION: Transfers policy priority, climate finance capital, investment screening preference, and regulatory streamlining FROM nuclear and long-timeline technologies TO fast-deployment renewables and distributed energy infrastructure. Money, permitting velocity, and favorable lending criteria flow preferentially to the beneficiary sector.
% ABSENT_VOICES: Long-term industrial planners and geopolitical security analysts who prioritize supply-chain resilience and manufacturing sovereignty are structurally outside the decision frame. Grid stability engineers who absorb integration costs are excluded from metric design. They would argue for portfolio approaches that internalize security and integration costs into the efficiency calculation, which would reposition nuclear as necessary and renewables as incomplete without dispatchable complement.
% DISAPPEARANCE_RATIONALE: If the opportunity-cost reading and its metric disappeared, regulatory priority would revert to portfolio optimization or systems-transition framings. Nuclear projects currently blocked or delayed would receive financing and permitting support. Grid operators would design systems around mixed portfolios rather than pure renewable+storage. Capital allocation mechanisms would include resilience, supply-chain, and baseload factors alongside speed. The global energy transition would proceed with a different technology mix and slower (but more diversified) deployment profile.
% FOUNDING_PROBLEM: Climate change demands mitigation on the timescale of the remaining carbon budget, estimated at ~400 Gt CO2 for 1.5°C limit. At current rates this budget is exhausted in ~8 years. Capital for low-carbon energy is limited. Every dollar spent on slow-deployment technologies is a dollar unavailable for faster alternatives. The founding problem is: how to maximize carbon abated per dollar per year to stay within the shrinking budget?
% FOUNDING_PROBLEM_CORROBORATION: IPCC AR6 confirms the speed constraint and budget exhaustion timeline. Bloomberg NEF and International Energy Agency analyses outside the renewables sector acknowledge that deployment speed affects cumulative carbon. However, they dispute whether the opportunity-cost metric is the right solution — many sources argue portfolio diversity is necessary. Speed constraint is corroborated; the metric's sufficiency is contested by grid engineers, security analysts, and industrial planners who are NOT beneficiaries of the reading.
narrative_ontology:disappearance_verdict(climate_mitigation_imperative__opportunity_cost_reading, world_rearranges).
narrative_ontology:founding_problem_status(climate_mitigation_imperative__opportunity_cost_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(climate_mitigation_imperative__opportunity_cost_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(climate_mitigation_imperative__opportunity_cost_reading, 'none', 1).
narrative_ontology:epsilon_provenance(climate_mitigation_imperative__opportunity_cost_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   EXTRACTION AT 0.68 (interval end): The metric structure extracts from nuclear and long-timeline technologies by redefining 'efficiency' in a way that makes their core characteristics (high reliability per unit, decades-long operational life, domestic industrial capacity) invisible or negative. A nuclear plant that runs at 92% capacity factor for 60 years produces vastly more carbon-free energy than its up-front carbon footprint amortizes to—but the opportunity-cost metric truncates the calculation at deployment speed, so the long tail of production is discounted. This is extraction because the metric is chosen, enforced, and benefits specific actors. SUPPRESSION AT 0.54: Moderate because the enforcement is regulatory and financial (climate finance criteria, investment screening), not direct physical coercion. Nuclear projects are not banned; they are deprioritized and starved of capital. Suppression rises over the interval as more climate finance mechanisms adopt the metric. THEATER AT 0.31: The carbon-per-dollar-per-year metric is genuine—deployment speed IS relevant to cumulative emissions. But as the series shows, theater rises because the framing increasingly performs the function of defending renewable-sector interests against competing technologies, not purely optimizing mitigation. The metric is real; its selective application is performative.
 *
 * PERSPECTIVAL GAP:
 *   SEATS WILL COMPUTE DIFFERENTLY: The renewables-sector beneficiary seats compute the constraint as coordination—a genuine solution to capital scarcity that directs resources efficiently. The nuclear-industry payer seats compute it as extraction—a metric deliberately designed to disadvantage their technology independent of actual climate impact. The capital-gatekeeper seats (institutional power) compute it as coordination they enforce for legitimate reasons. Grid-stability engineers (moderate power) compute it as extraction because it externalizes integration costs they must absorb. The engine's per-seat computation will expose this divergence; it is not reconciled by the authored claim. THE AUTHORED CLAIM IS TANGLED ROPE (coordination + asymmetric extraction + active enforcement) because the metric IS genuine coordination (solves real capital-scarcity problems) AND simultaneously extracts from particular constituencies through selective metric design.
 *
 * DIRECTIONALITY LOGIC:
 *   Renewables sector: d ≈ 0.15–0.20 (beneficiary + mobile exit + organized power = low directionality toward extraction). Nuclear industry: d ≈ 0.78–0.85 (victim + constrained exit + institutional power = high directionality toward target). Capital gatekeepers: d ≈ 0.55–0.60 (neither beneficiary nor victim; they administer the constraint; directionality reflects their neutral administrative stance + institutional power modulation). Baseload advocates: d ≈ 0.65–0.72 (conflicted—benefit from low-carbon adoption, but excluded from metric design and absorb integration costs; treated as payers in structure). Grid engineers: d ≈ 0.72–0.80 (moderate power but trapped by system design requirements; trapped exit + costs externalizes = high target directionality). The directionality divergence between beneficiary and payer seats is LARGE (0.20 vs 0.80), which the engine will flag as coordination+extraction asymmetry.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint sits in a critical mandatrophy zone: the founding problem (carbon urgency, capital scarcity) is LIVE and growing more acute. But the mandate—'optimize carbon per dollar per year'—is increasingly contested as the actual constraint's function. As deployment scales and grid integration becomes the binding constraint (not capital), the metric is exposed as optimizing for a problem (capital scarcity in 2015–2020) that is partially solved (by 2024, renewable capex is cost-competitive and deployment is gated by grid interconnection, supply chains, and permitting, not capital). The constraint persists not because it solves the founding problem anymore, but because the renewable sector and climate-finance institutions have built career and institutional interest in it. Theater ratio climbing (0.18 to 0.31) is the signal: more enforcement energy is spent defending the metric choice than optimizing actual carbon reduction. A hard mandatrophy call would require observing whether, when the constraint is relaxed (portfolio approaches adopted), total carbon reduction accelerates, stagnates, or shifts to different mixes. Current evidence is mixed because the constraint coexists with other framings.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    metric_boundary_sufficiency,
    'Is carbon-per-dollar-per-year a sufficient efficiency metric for climate mitigation, or does it artificially truncate system-level optimization by excluding integration costs, grid stability, supply-chain resilience, and long-term operational capacity factors?',
    'Compare total system cost and carbon outcomes across scenarios: (A) pure metric-driven optimization (no nuclear, maximize renewables+storage), (B) portfolio optimization including nuclear for baseload, (C) systems-transition decentralized approach. If scenario B or C delivers lower total system cost or faster actual carbon reduction per unit of installed capacity, the metric boundary is insufficient.',
    'If the metric is insufficient, the constraint shifts from tangled_rope (legitimate coordination plus extraction) toward snare (metric is cover for incumbent-protection). If sufficient, it remains tangled_rope with justified extraction from nuclear and long-timeline technologies.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(metric_boundary_sufficiency, empirical, 'Whether opportunity-cost metric captures all efficiency dimensions or artificially narrows the frame to advantage fast-deployment technologies.').

omega_variable(
    reading_foreclosure_potential,
    'Can the portfolio_optimization_reading and the opportunity_cost_reading coexist indefinitely within a single climate policy framework, or does adoption of opportunity-cost framing eventually foreclose portfolio approaches?',
    'Track regulatory evolution: if climate finance mechanisms begin including ''diversity'' or ''resilience'' requirements that admit nuclear alongside renewables despite lower per-dollar-per-year scores, coexistence is stabilizing. If nuclear deployment remains blocked despite pressure from portfolio advocates, foreclosure is occurring.',
    'If coexistence is stable, the readings genuinely coexist_with each other (competitive but not mutually exclusive). If one forecloses the other, the reading_relations must shift to forecloses.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_foreclosure_potential, empirical, 'Whether competing readings can persist in parallel or whether one eventually dominates the decision frame.').

omega_variable(
    capital_constraint_evolution,
    'Is capital availability actually the binding constraint on climate mitigation deployment, or has the constraint shifted to grid interconnection, supply chains, labor, or permitting as of 2024–2026?',
    'Survey project developers: ask what delays renewable projects (capital availability vs. interconnection queue vs. supply chain vs. permitting). If interconnection and supply-chain delays exceed capital delays, the founding problem has evolved but the metric has not.',
    'If capital is no longer binding, the founding_problem is partially dead (though not fully, given developing-nation capital scarcity). The constraint persists but increasingly for institutional-interest reasons, not problem-solving reasons—a mandatrophy signal. Theater ratio would be expected to climb as enforcement protects an outdated metric.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(capital_constraint_evolution, empirical, 'Whether the founding problem (capital scarcity in mitigation deployment) is still live or has been partially superseded.').

omega_variable(
    excluded_voices_counterfactual,
    'If long-term industrial planners, geopolitical security analysts, and grid stability engineers were seated in the metric-design process, would they demand inclusion of supply-chain resilience, manufacturing base preservation, and system integration costs—and would those inclusions materially change which technologies receive capital?',
    'Design a multi-stakeholder metric that includes speed, reliability, resilience, and security dimensions, then simulate capital allocation under that metric. Compare to actual allocation under opportunity-cost frame.',
    'If the counterfactual metric produces substantially different technology rankings (nuclear higher, some renewables lower), the exclusion of these voices is revealed as extraction: beneficiaries excluded competitors through metric design, not superior performance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(excluded_voices_counterfactual, conceptual, 'Whether the metric''s frame is the only defensible one or reflects deliberate exclusion of alternative efficiency dimensions.').

omega_variable(
    nuclear_victim_classification_stability,
    'Is nuclear correctly classified as a victim (payer) of the constraint, or does it belong in a mixed victim/beneficiary position because it still receives some policy support and financing outside the climate-metric frame?',
    'Audit capital flows: measure total public and private capital directed to nuclear vs. renewables as a share of each sector''s expansion targets. If nuclear receives comparable capital despite lower metric scores, the victim classification overstates extraction.',
    'If nuclear is a partial beneficiary despite metric deprioritization, the extraction is lower than authored (directionality is more moderate), and the constraint is less asymmetric.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(nuclear_victim_classification_stability, empirical, 'Whether nuclear genuinely bears extraction or partially benefits through non-metric channels.').

omega_variable(
    kernel_reading_identity,
    'This constraint is ONE READING of the climate_mitigation_imperative kernel. The reading instantiates a specific normative claim: deployment speed per dollar is the primary efficiency measure. Do portfolio_optimization and systems_transition readings use the SAME kernel (shared commitment to mitigation) or are they different kernels entirely?',
    'Check whether portfolio and systems-transition advocates acknowledge the same founding urgency (climate emergency, carbon budgets) or dispute the urgency itself. If they accept the urgency but dispute the efficiency criterion, they share the kernel; if they dispute urgency, they are different kernels.',
    'If shared kernel: the three readings coexist_with as competitive frames within a single commitment. If different kernels: each reading is a separate constraint story, not a reading-family member. The network structure changes.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Whether sibling readings share the same foundational kernel or represent distinct commitments.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_mitigation_imperative__opportunity_cost_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clim_tr_t0, climate_mitigation_imperative__opportunity_cost_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement(clim_tr_t3, climate_mitigation_imperative__opportunity_cost_reading, theater_ratio, 3, 0.21).
narrative_ontology:measurement(clim_tr_t7, climate_mitigation_imperative__opportunity_cost_reading, theater_ratio, 7, 0.25).
narrative_ontology:measurement(clim_tr_t12, climate_mitigation_imperative__opportunity_cost_reading, theater_ratio, 12, 0.28).
narrative_ontology:measurement(clim_tr_t18, climate_mitigation_imperative__opportunity_cost_reading, theater_ratio, 18, 0.3).
narrative_ontology:measurement(clim_tr_t25, climate_mitigation_imperative__opportunity_cost_reading, theater_ratio, 25, 0.31).

% Extraction over time
narrative_ontology:measurement(clim_be_t0, climate_mitigation_imperative__opportunity_cost_reading, base_extractiveness, 0, 0.52).
narrative_ontology:measurement(clim_be_t3, climate_mitigation_imperative__opportunity_cost_reading, base_extractiveness, 3, 0.56).
narrative_ontology:measurement(clim_be_t7, climate_mitigation_imperative__opportunity_cost_reading, base_extractiveness, 7, 0.61).
narrative_ontology:measurement(clim_be_t12, climate_mitigation_imperative__opportunity_cost_reading, base_extractiveness, 12, 0.65).
narrative_ontology:measurement(clim_be_t18, climate_mitigation_imperative__opportunity_cost_reading, base_extractiveness, 18, 0.67).
narrative_ontology:measurement(clim_be_t25, climate_mitigation_imperative__opportunity_cost_reading, base_extractiveness, 25, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(clim_su_t0, climate_mitigation_imperative__opportunity_cost_reading, suppression_requirement, 0, 0.38).
narrative_ontology:measurement(clim_su_t3, climate_mitigation_imperative__opportunity_cost_reading, suppression_requirement, 3, 0.42).
narrative_ontology:measurement(clim_su_t7, climate_mitigation_imperative__opportunity_cost_reading, suppression_requirement, 7, 0.47).
narrative_ontology:measurement(clim_su_t12, climate_mitigation_imperative__opportunity_cost_reading, suppression_requirement, 12, 0.51).
narrative_ontology:measurement(clim_su_t18, climate_mitigation_imperative__opportunity_cost_reading, suppression_requirement, 18, 0.53).
narrative_ontology:measurement(clim_su_t25, climate_mitigation_imperative__opportunity_cost_reading, suppression_requirement, 25, 0.54).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_mitigation_imperative__opportunity_cost_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(climate_mitigation_imperative__opportunity_cost_reading, 0.12).
narrative_ontology:affects_constraint(climate_mitigation_imperative__opportunity_cost_reading, climate_mitigation_imperative__portfolio_optimization_reading).
narrative_ontology:affects_constraint(climate_mitigation_imperative__opportunity_cost_reading, climate_mitigation_imperative__systems_transition_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the climate_mitigation_imperative kernel. The sibling readings are portfolio_optimization_reading (nuclear as necessary baseload) and systems_transition_reading (nuclear as extractive centralization). All three share the foundational kernel (mitigation is urgent) but differ on the efficiency criterion and thus on technology classification. The three readings are linked via network.affects_constraints and should be analyzed as a constraint family. The network direction (opportunity_cost affects the siblings) reflects that adoption of opportunity-cost framing creates structural pressure on portfolio approaches (nuclear deprioritization makes portfolio arguments harder) but does not logically foreclose them.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(climate_mitigation_imperative__opportunity_cost_reading, moderate, 0.75).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
