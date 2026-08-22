% ============================================================================
% CONSTRAINT STORY: climate_mitigation_legitimacy__baseload_necessity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_climate_mitigation_legitimacy__baseload_necessity_reading, []).

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
 *   constraint_id: climate_mitigation_legitimacy__baseload_necessity_reading
 *   human_readable: Baseload Necessity Doctrine in Climate Mitigation
 *   domain: energy/climate/policy
 *
 * SUMMARY:
 *   The 'baseload necessity' reading holds that reliable decarbonization
 *   requires dispatchable firm power that renewables cannot provide at scale.
 *   This is one of four readings of the contested 'climate mitigation
 *   legitimacy' kernel. The reading instantiates a constraint that
 *   coordinates genuine technical coordination (grids require firm capacity
 *   management) while extracting policy priority, capital allocation, and
 *   legitimacy from renewable alternatives. Nuclear and incumbent utilities
 *   benefit from the doctrine by securing long-term policy support and
 *   infrastructure investment; renewable sectors, distributed-generation
 *   advocates, and developing nations with capital constraints bear the costs
 *   through policy subordination. The constraint is actively enforced through
 *   grid codes, R&D funding allocation, and technical standards that
 *   subordinate variable renewables to firm-capacity requirements. The
 *   measurement series shows base_extractiveness rising from 0.48 to 0.68
 *   over 25 years as the doctrine shifted from technical hypothesis to policy
 *   orthodoxy; theater_ratio rising from 0.22 to 0.41 indicates growing
 *   proportion of enforcement activity defending the doctrine itself
 *   (excluding research, licensing alternative pathways) rather than solving
 *   grid coordination problems.
 *
 * KEY AGENTS:
 *   - Nuclear industry: prime beneficiary, shapes doctrine through technical reports and policy engagement
 *   - Incumbent utilities: benefit from centralization; coordinate with grid operators to subordinate renewables
 *   - Financial institutions: hold nuclear debt; reduce refinancing risk by maintaining policy support
 *   - Long-term climate modelers: produce scenarios parameterized on baseload necessity; professional standing tied to doctrine
 *   - Renewable energy sector: pay through policy subordination and constrained investment signals
 *   - Distributed generation advocates: excluded and subordinated by grid codes requiring firm-power compliance
 *   - Energy democracy movements: identity-locked target; centralization forecloses community ownership models
 *   - Developing nations: trapped; nuclear-finance obligations lock them into debt-financed centralized infrastructure
 *   - Grid operators: co-author enforcement (grid codes); benefit operationally from centralized, controllable generation
 *   - Climate policy authorities: observe; decisions either entrench or dislodge the doctrine
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_mitigation_legitimacy__baseload_necessity_reading, 0.68).
domain_priors:suppression_score(climate_mitigation_legitimacy__baseload_necessity_reading, 0.72).
domain_priors:theater_ratio(climate_mitigation_legitimacy__baseload_necessity_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__baseload_necessity_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__baseload_necessity_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__baseload_necessity_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__baseload_necessity_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__baseload_necessity_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_mitigation_legitimacy__baseload_necessity_reading, tangled_rope).
narrative_ontology:human_readable(climate_mitigation_legitimacy__baseload_necessity_reading, "Baseload Necessity Doctrine in Climate Mitigation").
narrative_ontology:topic_domain(climate_mitigation_legitimacy__baseload_necessity_reading, "energy/climate/policy").

domain_priors:requires_active_enforcement(climate_mitigation_legitimacy__baseload_necessity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_mitigation_legitimacy__baseload_necessity_reading, 'd5e8d6e8-c8c3-4abc-abf9-e061f37768d3').
narrative_ontology:cs_kernel_codification('d5e8d6e8-c8c3-4abc-abf9-e061f37768d3', formalized).
narrative_ontology:cs_authority_grounding('d5e8d6e8-c8c3-4abc-abf9-e061f37768d3', extraction).
narrative_ontology:cs_interpretation_layer_present('d5e8d6e8-c8c3-4abc-abf9-e061f37768d3').
narrative_ontology:cs_reading_relation('d5e8d6e8-c8c3-4abc-abf9-e061f37768d3', climate_mitigation_legitimacy__renewable_primacy_reading, coexists_with).
narrative_ontology:cs_reading_relation('d5e8d6e8-c8c3-4abc-abf9-e061f37768d3', climate_mitigation_legitimacy__portfolio_pragmatism_reading, influences).
narrative_ontology:cs_reading_relation('d5e8d6e8-c8c3-4abc-abf9-e061f37768d3', climate_mitigation_legitimacy__degrowth_sufficiency_reading, coexists_with).
narrative_ontology:cs_axiom('d5e8d6e8-c8c3-4abc-abf9-e061f37768d3', foundational, firm_dispatchable_power_necessary_at_scale).
narrative_ontology:cs_axiom_status(firm_dispatchable_power_necessary_at_scale, holdable).
narrative_ontology:cs_axiom_grounding('d5e8d6e8-c8c3-4abc-abf9-e061f37768d3', firm_dispatchable_power_necessary_at_scale, empirically_contingent).
narrative_ontology:cs_axiom('d5e8d6e8-c8c3-4abc-abf9-e061f37768d3', foundational, renewable_storage_insufficient_for_reliable_decarbonization).
narrative_ontology:cs_axiom_status(renewable_storage_insufficient_for_reliable_decarbonization, holdable).
narrative_ontology:cs_axiom_grounding('d5e8d6e8-c8c3-4abc-abf9-e061f37768d3', renewable_storage_insufficient_for_reliable_decarbonization, empirically_contingent).
narrative_ontology:cs_reference_frame('d5e8d6e8-c8c3-4abc-abf9-e061f37768d3', physics_limited_renewables_framework).
narrative_ontology:cs_drift_state('d5e8d6e8-c8c3-4abc-abf9-e061f37768d3', post_storage_cost_collapse, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('d5e8d6e8-c8c3-4abc-abf9-e061f37768d3', '').
narrative_ontology:cs_kernel_id(climate_mitigation_legitimacy__baseload_necessity_reading, climate_mitigation_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_mitigation_legitimacy__baseload_necessity_reading, nuclear_industry).
narrative_ontology:constraint_beneficiary(climate_mitigation_legitimacy__baseload_necessity_reading, incumbent_utilities).
narrative_ontology:constraint_beneficiary(climate_mitigation_legitimacy__baseload_necessity_reading, financial_institutions_holding_nuclear_debt).
narrative_ontology:constraint_beneficiary(climate_mitigation_legitimacy__baseload_necessity_reading, long_term_climate_modelers).
narrative_ontology:constraint_victim(climate_mitigation_legitimacy__baseload_necessity_reading, renewable_energy_sector).
narrative_ontology:constraint_victim(climate_mitigation_legitimacy__baseload_necessity_reading, distributed_generation_advocates).
narrative_ontology:constraint_victim(climate_mitigation_legitimacy__baseload_necessity_reading, energy_democracy_movements).
narrative_ontology:constraint_victim(climate_mitigation_legitimacy__baseload_necessity_reading, developing_nations_with_limited_capital).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(climate_mitigation_legitimacy__baseload_necessity_reading, grid_operators).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Constructs and operates large-scale nuclear plants. The baseload-necessity doctrine legitimates continued investment in multi-billion-dollar reactor projects with 60+ year capital recovery timelines. Actively promotes the doctrine through industry associations, technical reports, and policy engagement. Collects long-term revenue streams from government contracts and energy sales that would be threatened if renewable-only pathways gained policy acceptance.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__baseload_necessity_reading, nuclear_industry, beneficiary,
    institutional, civilizational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(climate_mitigation_legitimacy__baseload_necessity_reading, nuclear_industry, agenda_setter).

% Own centralized generation assets (coal plants being retired, nuclear plants, hydro) and distribution infrastructure. The baseload doctrine supports continued investment in large, centralized generation requiring extensive transmission infrastructure. Their business model depends on electricity flowing from few large generators to many distributed consumers; distributed renewable generation with local storage threatens this centralization rent.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__baseload_necessity_reading, incumbent_utilities, beneficiary,
    institutional, generational, constrained, national).

% Hold tens of billions in debt on existing nuclear plants and in-construction projects. The doctrine's acceptance reduces refinancing risk by maintaining policy support for baseload infrastructure. A shift to renewable primacy could trigger plant abandonment, asset write-downs, and debt restructuring. They influence policy through lending conditions and credit ratings.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__baseload_necessity_reading, financial_institutions_holding_nuclear_debt, beneficiary,
    institutional, biographical, mobile, global).

% Produce long-range decarbonization scenarios (IPCC, IEA net-zero pathways) where nuclear capacity expands substantially to meet firm baseload demand. Their modeling parameterizes the baseload-necessity assumption; scenarios built on the doctrine show nuclear as indispensable. Their professional standing and funding depend on the legitimacy of their baseline assumptions.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__baseload_necessity_reading, long_term_climate_modelers, beneficiary,
    analytical, civilizational, analytical, global).

% Manufactures and deploys solar, wind, storage, and grid-balancing technologies. The baseload-necessity doctrine claims their technologies cannot meet peak-load and seasonal firm-capacity requirements, making them structurally subordinate to nuclear. This reduces policy incentives for storage R&D, grid flexibility investment, and deployment at the scale needed for renewable-only pathways. They pay through policy subordination and constrained investment signals.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__baseload_necessity_reading, renewable_energy_sector, payer,
    powerful, generational, constrained, global).

% Promote local, community-scale renewable generation and microgrids as alternatives to centralized plants. The baseload doctrine frames distributed, intermittent generation as inherently unreliable and inadequate, legitimating regulatory barriers to local generation interconnection and forcing projects to meet centralized grid-stability criteria. Their options are constrained by grid codes, interconnection standards, and utility control of transmission.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__baseload_necessity_reading, distributed_generation_advocates, payer,
    moderate, biographical, constrained, local).

% Advocate for public ownership, community control, and equitable distribution of energy production. The baseload doctrine legitimates capital-intensive centralized infrastructure (nuclear plants cost $15–20 billion+, require 10+ year construction, need government backing) that forecloses community ownership models. They bear the costs of continued centralization (energy poverty, lack of local control, fossil-fuel-dependent imports) and cannot exit without dissolving their identity as advocates for democratic energy.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__baseload_necessity_reading, energy_democracy_movements, payer,
    powerless, biographical, identity_locked, local).

% Have limited capital for decarbonization investment but abundant renewable resources (solar, wind, hydro potential). The baseload doctrine channels development finance toward nuclear projects that require external capital, technical expertise, and long-term operational commitment. This locks their economies into debt-financed centralized infrastructure instead of enabling locally-resourced renewable deployment. They cannot exit without abandoning decarbonization or contracting with external nuclear-finance partners.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__baseload_necessity_reading, developing_nations_with_limited_capital, payer,
    powerless, generational, trapped, national).

% Operate transmission and distribution networks and manage real-time frequency/voltage. The baseload doctrine simplifies their operational mandate: large, firm generators (nuclear, coal, hydro) provide predictable power flow; their task is dispatch and load-balancing. A renewable-heavy grid requires active demand-response, storage orchestration, and dynamic frequency support — operationally more complex. The doctrine legitimates their preference for centralized, controllable generation; they co-author grid codes that subordinate renewable generation to dispatchability requirements.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__baseload_necessity_reading, grid_operators, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(climate_mitigation_legitimacy__baseload_necessity_reading, grid_operators, beneficiary).

% Study storage, grid integration, demand flexibility, and synthetic fuels to address intermittency. The baseload doctrine frames their research as peripheral to the 'core' decarbonization problem. Their work competes for funding and policy attention against assumed-necessary nuclear projects. If the doctrine were overturned, their research would move from 'optimization problem' (how to supplement baseload with renewables) to 'central problem' (how to run a renewable-heavy grid), radically shifting resource allocation.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__baseload_necessity_reading, renewable_technology_researchers, excluded,
    analytical, biographical, constrained, global).

% Emphasize that the timescale of decarbonization (years to decade) is much faster than nuclear deployment timescale (10–20 years per plant). They argue that baseload-focused strategies will fail to meet near-term targets. The doctrine excludes their voice from policy tables by asserting long-term necessity (baseload matters for 2050 decarbonization) and deferring urgency (nuclear plants built now will operate for 60+ years). They are analytically present but policy-excluded.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__baseload_necessity_reading, climate_scientists_focused_on_rapid_transition, excluded,
    analytical, civilizational, constrained, global).

% Set carbon budgets, allocate R&D funding, license infrastructure projects, and enforce grid standards. They mediate between the constraint's beneficiaries and victims. They hear testimony from all sides but operate within framing assumptions (e.g., that baseload is necessary) that shape which policies are considered 'realistic.' Their decisions either entrench or dislodge the doctrine.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__baseload_necessity_reading, climate_policy_authorities, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(climate_mitigation_legitimacy__baseload_necessity_reading, nuclear_industry).
narrative_ontology:fixing_cost_class(climate_mitigation_legitimacy__baseload_necessity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the technical coordination problem of matching generation to demand in a decarbonized grid: any grid must balance instantaneous load and maintain frequency/voltage stability. The doctrine offers one solution — large, firm generators (nuclear, hydro, biomass) handle the base demand reliably, renewables handle the variable demand on top. This is one valid technical architecture among several feasible ones.
% TRANSFER_FUNCTION: Moves capital commitments, deployment priority, and policy legitimacy from renewable-technology sectors to nuclear industry and incumbent utilities. Allocates R&D funding toward long-timescale nuclear projects and away from storage, demand-flexibility, and distributed generation. Redirects electricity revenue streams from distributed, locally-owned systems toward centralized, institutional generators.
% ABSENT_VOICES: Renewable technology researchers, climate scientists emphasizing rapid transition, energy democracy advocates, and developing nations with capital constraints are excluded or marginalized from decarbonization-pathway planning. They would argue that baseload necessity is contingent on system design choices, not a physical law, and that faster, locally-resourced, more equitable pathways exist. Their exclusion means decarbonization-pathway debates proceed without voices that would challenge the centralization assumption.
% DISAPPEARANCE_RATIONALE: If the baseload-necessity doctrine evaporated overnight, policy would shift toward renewable-first deployment, storage investment, demand-flexibility, and distributed generation. Grid operators would reframe around managing high renewable penetration rather than securing firm baseload. Capital would flow to battery and storage R&D instead of reactor construction. Incumbent utilities and nuclear operators would face forced restructuring; developing nations could scale solar/wind without nuclear-finance obligations. The world would NOT be unchanged (centralized infrastructure investments would be cancelled, R&D redirected, ownership models shifted), but contention exists about whether this rearrangement would accelerate or hinder decarbonization (a contested empirical claim about system dynamics).
% FOUNDING_PROBLEM: Early (1980s–2000s) decarbonization analysis showed that decarbonizing grids with variable demand required either massive over-capacity in renewables with waste, or reliable firm power to handle base-load demand when wind/solar were unavailable. Nuclear and hydro were the known firm technologies. The doctrine crystallized that firm power was necessary; the debate shifted to what KIND of firm power.
% FOUNDING_PROBLEM_CORROBORATION: The nuclear industry and incumbent utilities attest the founding problem remains live: renewables are still variable, grids still require firm capacity, storage is still not deployable at scale. Renewable-sector technologists and energy democracy advocates attest the founding problem has been substantially reframed: storage costs have collapsed (80% reduction in lithium-ion costs since 2010), grid-integration studies show high renewable penetration feasible with active demand-response, and the real constraint is no longer 'what is firm power' but 'what timescale and ownership model can we commit to.' Independent analyses from IRENA, MIT Energy Initiative, and NREL conclude that the founding problem is partly solved and the remainder is partly technical (addressable with deployed tech) and partly policy/economics (deployment speed depends on capital allocation, not physics).
narrative_ontology:disappearance_verdict(climate_mitigation_legitimacy__baseload_necessity_reading, contested).
narrative_ontology:founding_problem_status(climate_mitigation_legitimacy__baseload_necessity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(climate_mitigation_legitimacy__baseload_necessity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(climate_mitigation_legitimacy__baseload_necessity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(climate_mitigation_legitimacy__baseload_necessity_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(climate_mitigation_legitimacy__baseload_necessity_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(climate_mitigation_legitimacy__baseload_necessity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(climate_mitigation_legitimacy__baseload_necessity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness reaches 0.68 because the doctrine channels policy priority and capital from a viable alternative (renewable-heavy systems) toward a capital-intensive, incumbent-controlled pathway. Suppression is higher (0.72) because the constraint's persistence depends on actively subordinating renewable research, excluding distributed-generation pathways, and maintaining grid codes that penalize variable renewables. This active subordination is the suppression mechanism. Theater_ratio (0.41 at interval end) reflects that roughly 40% of enforcement activity now goes to rhetorical defense of the necessity claim rather than operational grid coordination — enforcement capacity is increasingly spent on defending the doctrine against mounting empirical challenge (storage cost collapse, grid-integration studies) rather than on the technical problem it supposedly solves. The trajectory is flatline after year 25, indicating the doctrine has reached policy saturation: it is now entrenched in regulatory frameworks, and further extractiveness gains require different mechanisms (new infrastructure lock-in, financing structures) rather than doctrine-strengthening. Accessibility_collapse (0.62) reflects that alternative decarbonization pathways are not impossible but are severely disadvantaged: policy, capital, and technical standards require renewable-heavy pathways to meet firm-power criteria designed for centralized generation, which is technically feasible but adds cost and complexity. Resistance (0.71) is high because renewable sectors, climate scientists, and developing nations mount substantial research and policy counter-arguments; they have not overcome the doctrine but they are not passive.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seats (nuclear industry, incumbent utilities, grid operators) compute the constraint as genuine coordination-enabling infrastructure: they see a technical necessity (grids need firm power) that they provide, and extract legitimate compensation for that service. The victim seats (renewable sectors, distributed advocates, developing nations) compute the constraint as pure extraction: they see a policy choice to subordinate renewable pathways, not a technical necessity, and they bear costs through policy subordination while the real technical problem (firm capacity) can be solved with deployed storage and demand flexibility. Climate policy authorities sit in observation — they hear both frames. The engine computes per-seat classifications from the structural data: the beneficiary seats will compute rope or tangled-rope (real coordination, extraction layered on top); the victim seats will compute snare or tangled-rope (extraction masquerading as coordination). The authored claim (tangled_rope) reflects the author's reading that both frames are structurally real: there IS a coordination problem (grids require firm capacity), and there IS extraction (the doctrine subordinates renewable alternatives that could address that problem). The metrics are authored independently of the claim — the author believes the coordination function is real but only 40–60% of the enforcement energy (per theater_ratio) goes to that coordination; the rest goes to defending the doctrine against empirical challenge.
 *
 * DIRECTIONALITY LOGIC:
 *   Nuclear industry: d ≈ 0.1 (full beneficiary). They collect long-term revenue, control the policy narrative, and face minimal constraints on their exit — they can always shift to other large energy projects. Incumbent utilities: d ≈ 0.15 (beneficiary, slightly more constrained than nuclear because utilities face regulatory requirements to serve all customers, but they control the infrastructure and distribution). Financial institutions: d ≈ 0.2 (beneficiary but more constrained — their exit is easier in principle, but debt refinancing lock-in makes exiting difficult; they benefit from reduced refinancing risk). Long-term climate modelers: d ≈ 0.3 (ambiguous; they benefit from being on the 'consensus' side of a major policy question, but they face pressure if their assumptions diverge from empirical evidence; moderately beneficial because their assumptions are treated as settled). Renewable energy sector: d ≈ 0.7 (target). They pay through policy subordination, constrained investment, and technical standards they must comply with. Their exit is highly constrained (they cannot exit renewable technology) but mobile (they can shift deployment to other nations with pro-renewable policies). Distributed generation advocates: d ≈ 0.75 (target, more severely trapped). Their exit is identity-locked; they cannot exit without abandoning their core commitment to distributed, democratic energy. Grid operators: d ≈ 0.4 (symmetric to slightly beneficial). They benefit operationally from the doctrine (simpler dispatch with centralized, firm generation), but they also bear costs (they must manage higher renewable penetration as it forces its way in; they face increasing complexity as the doctrine's technical assumptions break down). Developing nations: d ≈ 0.85 (severely targeted). They are trapped (cannot exit decarbonization or energy development) and their exit to renewable-only pathways requires overcoming policy and finance barriers. Energy democracy movements: d ≈ 0.8 (target, identity-locked). Overrides are not needed — the derivation from beneficiary/victim data and exit options produces these directionalities.
 *
 * MANDATROPHY ANALYSIS:
 *   The baseload-necessity doctrine exhibits mandatrophy: the founding problem (grids require firm capacity management) has been substantially addressed by technological development (storage costs down 80% over 15 years; grid-integration studies showing 70–90% renewable penetration feasible), but the arrangement persists due to incumbent-utility and nuclear-industry policy power. The mandate to 'provide firm power because no alternatives exist' has died, but the arrangement persists because those who benefit from it have political capacity to maintain it. Evidence: (1) renewable-sector technologists produce evidence that the problem is solvable at scale with deployed technologies; (2) developing nations' solar deployment accelerates despite policy barriers, showing revealed preference for renewable-primary pathways; (3) grid operators publish studies on high-renewable-penetration operation; (4) climate scientists increasingly emphasize rapid deployment timescale (years) over baseload timescale (decades). The doctrine persists not because the founding problem is live but because the beneficiaries have institutional power to keep it entrenched. The theater_ratio rise from 0.22 to 0.41 is the clearest mandatrophy signal: enforcement activity increasingly goes to defending the doctrine itself rather than to solving the technical problem it supposedly addresses.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    storage_cost_trajectory_uncertainty,
    'Will battery storage costs continue to decline at historical rates (15–20% annually), or will they plateau as engineering margins tighten?',
    'Longitudinal cost tracking of lithium-ion, alternative chemistries, and grid-scale storage; engineering analysis of remaining cost-reduction pathways.',
    'If costs continue declining and deployment accelerates, the baseload-necessity claim weakens substantially — firm power can be provided by renewable plus storage at comparable or lower cost than nuclear. If costs plateau, baseload necessity strengthens. A 50% reduction in storage costs would move many grid scenarios from ''nuclear necessary'' to ''renewable-storage sufficient,'' potentially reclassifying the constraint from tangled-rope to snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(storage_cost_trajectory_uncertainty, empirical, 'Cost trajectory determines whether renewable-plus-storage is economically competitive with nuclear baseload.').

omega_variable(
    firm_power_physical_necessity_vs_policy_choice,
    'Is the requirement for firm power a physical necessity imposed by grid physics, or a policy choice encoded in grid codes and operational assumptions?',
    'Analysis of grids with high renewable penetration (Denmark, Ireland, Chile); controlled experiments in operational research; jurisdictional variation in grid-code requirements for renewable-generation interconnection.',
    'If firm power is physical necessity, the baseload doctrine is natural law (mountain from all seats). If it is policy choice, the doctrine is constructed extraction (tangled rope or snare). The evidence suggests elements of both — frequency stability is real physics, but the stringency of firm-power requirements is policy-tunable. A resolution clarifying the policy component would justify tighter scrutiny of whether grid codes are optimized for technical robustness or for incumbent-utility protection.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(firm_power_physical_necessity_vs_policy_choice, empirical, 'Whether firm-power requirement is physical law or policy construct.').

omega_variable(
    deployment_timescale_decarbonization_urgency_mismatch,
    'Given the urgency of near-term decarbonization (targets for 2030–2040) and the 10–20 year construction timescale for nuclear plants, does baseload strategy enable or delay decarbonization?',
    'Scenario analysis comparing deployment pathways: nuclear-first (hits baseload targets post-2035 but reaches near-term targets slowly), renewable-fast (hits near-term targets through rapid solar/wind deployment, risks intermittency managing longer-term decarbonization), hybrid (slow nuclear plus fast renewable plus storage). Empirical comparison of actual deployment rates across jurisdictions.',
    'If nuclear-first delays decarbonization relative to renewable-fast pathways, the baseload doctrine is misaligned with climate urgency. If nuclear-fast is feasible through policy acceleration and nuclear is operationally necessary for long-term stability, the doctrine is justified. This omega is conceptually and empirically complex: the answer depends on both physics (what is operationally necessary) and policy (what deployment rate is achievable). A resolution would clarify whether the baseload doctrine optimizes for climate impact or for incumbent-asset utilization.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(deployment_timescale_decarbonization_urgency_mismatch, conceptual, 'Whether baseload strategy enables or delays decarbonization given near-term urgency.').

omega_variable(
    reading_boundary_firm_power_vs_decentralization,
    'Is this reading''s core claim ''firm power is necessary'' or ''centralized firm power is necessary''? Are these logically separable, or does the reading foreclose decentralized firm power (distributed nuclear, distributed storage, peer-to-peer microgrids)?',
    'Textual analysis of baseload-necessity claims in policy documents; interviews with doctrine proponents; operational analysis of decentralized firm-power architectures.',
    'If the reading only asserts ''firm power is necessary'' and allows it to be decentralized, it coexists with renewable-plus-distributed-storage pathways (coexists_with relation). If the reading asserts ''centralized firm power is necessary'' to defend incumbent utilities'' business model, it forecloses decentralized pathways (influences or forecloses relation, per structure). This determines whether the reading is a genuine technical claim or a bundled policy preference for centralization.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_boundary_firm_power_vs_decentralization, conceptual, 'Whether the doctrine asserts firm-power necessity generally or centralized-infrastructure necessity specifically.').

omega_variable(
    suppression_mechanism_structural_vs_internalized,
    'Is the subordination of renewable pathways structural (external barriers: grid codes, financing constraints, licensing timelines) or internalized (the renewable sector has accepted baseload necessity as technically sound and constrained its own ambitions)?',
    'Survey of renewable-sector technologists on belief in baseload necessity vs. perceived external barriers; analysis of renewable-sector R&D priorities; natural experiment from jurisdictions with pro-renewable grid codes and open-access financing to see whether renewable deployment accelerates or encounters new barriers.',
    'If suppression is primarily structural, removing grid barriers and financing constraints would unlock alternative pathways. If suppression is primarily internalized (the sector believes the doctrine), removing barriers would not change outcomes. If both, a mixed response — some deployment acceleration but continued hesitation from internalized doubt. The distinction matters for policy intervention: structural suppression requires regulatory reform; internalized suppression requires evidence campaigns and R&D investment in alternatives.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_vs_internalized, empirical, 'Whether renewable-sector constraint is external barrier or internalized belief in baseload necessity.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_mitigation_legitimacy__baseload_necessity_reading, 0, 35).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clim_tr_t0, climate_mitigation_legitimacy__baseload_necessity_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement(clim_tr_t5, climate_mitigation_legitimacy__baseload_necessity_reading, theater_ratio, 5, 0.26).
narrative_ontology:measurement(clim_tr_t10, climate_mitigation_legitimacy__baseload_necessity_reading, theater_ratio, 10, 0.31).
narrative_ontology:measurement(clim_tr_t15, climate_mitigation_legitimacy__baseload_necessity_reading, theater_ratio, 15, 0.36).
narrative_ontology:measurement(clim_tr_t20, climate_mitigation_legitimacy__baseload_necessity_reading, theater_ratio, 20, 0.39).
narrative_ontology:measurement(clim_tr_t25, climate_mitigation_legitimacy__baseload_necessity_reading, theater_ratio, 25, 0.4).
narrative_ontology:measurement(clim_tr_t30, climate_mitigation_legitimacy__baseload_necessity_reading, theater_ratio, 30, 0.41).
narrative_ontology:measurement(clim_tr_t35, climate_mitigation_legitimacy__baseload_necessity_reading, theater_ratio, 35, 0.41).

% Extraction over time
narrative_ontology:measurement(clim_be_t0, climate_mitigation_legitimacy__baseload_necessity_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(clim_be_t5, climate_mitigation_legitimacy__baseload_necessity_reading, base_extractiveness, 5, 0.53).
narrative_ontology:measurement(clim_be_t10, climate_mitigation_legitimacy__baseload_necessity_reading, base_extractiveness, 10, 0.59).
narrative_ontology:measurement(clim_be_t15, climate_mitigation_legitimacy__baseload_necessity_reading, base_extractiveness, 15, 0.64).
narrative_ontology:measurement(clim_be_t20, climate_mitigation_legitimacy__baseload_necessity_reading, base_extractiveness, 20, 0.66).
narrative_ontology:measurement(clim_be_t25, climate_mitigation_legitimacy__baseload_necessity_reading, base_extractiveness, 25, 0.67).
narrative_ontology:measurement(clim_be_t30, climate_mitigation_legitimacy__baseload_necessity_reading, base_extractiveness, 30, 0.68).
narrative_ontology:measurement(clim_be_t35, climate_mitigation_legitimacy__baseload_necessity_reading, base_extractiveness, 35, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(clim_su_t0, climate_mitigation_legitimacy__baseload_necessity_reading, suppression_requirement, 0, 0.58).
narrative_ontology:measurement(clim_su_t5, climate_mitigation_legitimacy__baseload_necessity_reading, suppression_requirement, 5, 0.62).
narrative_ontology:measurement(clim_su_t10, climate_mitigation_legitimacy__baseload_necessity_reading, suppression_requirement, 10, 0.66).
narrative_ontology:measurement(clim_su_t15, climate_mitigation_legitimacy__baseload_necessity_reading, suppression_requirement, 15, 0.69).
narrative_ontology:measurement(clim_su_t20, climate_mitigation_legitimacy__baseload_necessity_reading, suppression_requirement, 20, 0.7).
narrative_ontology:measurement(clim_su_t25, climate_mitigation_legitimacy__baseload_necessity_reading, suppression_requirement, 25, 0.71).
narrative_ontology:measurement(clim_su_t30, climate_mitigation_legitimacy__baseload_necessity_reading, suppression_requirement, 30, 0.72).
narrative_ontology:measurement(clim_su_t35, climate_mitigation_legitimacy__baseload_necessity_reading, suppression_requirement, 35, 0.72).

% Leveled coercion grid (OQ-93): 32/32 authored points at t0=0, tn=35
narrative_ontology:measurement(clim_grid_01, climate_mitigation_legitimacy__baseload_necessity_reading, accessibility_collapse(class), 0, 0.58).
narrative_ontology:measurement(clim_grid_02, climate_mitigation_legitimacy__baseload_necessity_reading, accessibility_collapse(class), 35, 0.65).
narrative_ontology:measurement(clim_grid_03, climate_mitigation_legitimacy__baseload_necessity_reading, accessibility_collapse(individual), 0, 0.45).
narrative_ontology:measurement(clim_grid_04, climate_mitigation_legitimacy__baseload_necessity_reading, accessibility_collapse(individual), 35, 0.58).
narrative_ontology:measurement(clim_grid_05, climate_mitigation_legitimacy__baseload_necessity_reading, accessibility_collapse(organizational), 0, 0.62).
narrative_ontology:measurement(clim_grid_06, climate_mitigation_legitimacy__baseload_necessity_reading, accessibility_collapse(organizational), 35, 0.68).
narrative_ontology:measurement(clim_grid_07, climate_mitigation_legitimacy__baseload_necessity_reading, accessibility_collapse(structural), 0, 0.72).
narrative_ontology:measurement(clim_grid_08, climate_mitigation_legitimacy__baseload_necessity_reading, accessibility_collapse(structural), 35, 0.73).
narrative_ontology:measurement(clim_grid_09, climate_mitigation_legitimacy__baseload_necessity_reading, resistance(class), 0, 0.62).
narrative_ontology:measurement(clim_grid_10, climate_mitigation_legitimacy__baseload_necessity_reading, resistance(class), 35, 0.72).
narrative_ontology:measurement(clim_grid_11, climate_mitigation_legitimacy__baseload_necessity_reading, resistance(individual), 0, 0.42).
narrative_ontology:measurement(clim_grid_12, climate_mitigation_legitimacy__baseload_necessity_reading, resistance(individual), 35, 0.55).
narrative_ontology:measurement(clim_grid_13, climate_mitigation_legitimacy__baseload_necessity_reading, resistance(organizational), 0, 0.68).
narrative_ontology:measurement(clim_grid_14, climate_mitigation_legitimacy__baseload_necessity_reading, resistance(organizational), 35, 0.75).
narrative_ontology:measurement(clim_grid_15, climate_mitigation_legitimacy__baseload_necessity_reading, resistance(structural), 0, 0.58).
narrative_ontology:measurement(clim_grid_16, climate_mitigation_legitimacy__baseload_necessity_reading, resistance(structural), 35, 0.65).
narrative_ontology:measurement(clim_grid_17, climate_mitigation_legitimacy__baseload_necessity_reading, stakes_inflation(class), 0, 0.48).
narrative_ontology:measurement(clim_grid_18, climate_mitigation_legitimacy__baseload_necessity_reading, stakes_inflation(class), 35, 0.58).
narrative_ontology:measurement(clim_grid_19, climate_mitigation_legitimacy__baseload_necessity_reading, stakes_inflation(individual), 0, 0.35).
narrative_ontology:measurement(clim_grid_20, climate_mitigation_legitimacy__baseload_necessity_reading, stakes_inflation(individual), 35, 0.48).
narrative_ontology:measurement(clim_grid_21, climate_mitigation_legitimacy__baseload_necessity_reading, stakes_inflation(organizational), 0, 0.52).
narrative_ontology:measurement(clim_grid_22, climate_mitigation_legitimacy__baseload_necessity_reading, stakes_inflation(organizational), 35, 0.62).
narrative_ontology:measurement(clim_grid_23, climate_mitigation_legitimacy__baseload_necessity_reading, stakes_inflation(structural), 0, 0.68).
narrative_ontology:measurement(clim_grid_24, climate_mitigation_legitimacy__baseload_necessity_reading, stakes_inflation(structural), 35, 0.72).
narrative_ontology:measurement(clim_grid_25, climate_mitigation_legitimacy__baseload_necessity_reading, suppression(class), 0, 0.56).
narrative_ontology:measurement(clim_grid_26, climate_mitigation_legitimacy__baseload_necessity_reading, suppression(class), 35, 0.72).
narrative_ontology:measurement(clim_grid_27, climate_mitigation_legitimacy__baseload_necessity_reading, suppression(individual), 0, 0.45).
narrative_ontology:measurement(clim_grid_28, climate_mitigation_legitimacy__baseload_necessity_reading, suppression(individual), 35, 0.58).
narrative_ontology:measurement(clim_grid_29, climate_mitigation_legitimacy__baseload_necessity_reading, suppression(organizational), 0, 0.62).
narrative_ontology:measurement(clim_grid_30, climate_mitigation_legitimacy__baseload_necessity_reading, suppression(organizational), 35, 0.75).
narrative_ontology:measurement(clim_grid_31, climate_mitigation_legitimacy__baseload_necessity_reading, suppression(structural), 0, 0.68).
narrative_ontology:measurement(clim_grid_32, climate_mitigation_legitimacy__baseload_necessity_reading, suppression(structural), 35, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_mitigation_legitimacy__baseload_necessity_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(climate_mitigation_legitimacy__baseload_necessity_reading, 0.18).
narrative_ontology:affects_constraint(climate_mitigation_legitimacy__baseload_necessity_reading, climate_mitigation_legitimacy__renewable_primacy_reading).
narrative_ontology:affects_constraint(climate_mitigation_legitimacy__baseload_necessity_reading, climate_mitigation_legitimacy__portfolio_pragmatism_reading).
narrative_ontology:affects_constraint(climate_mitigation_legitimacy__baseload_necessity_reading, climate_mitigation_legitimacy__degrowth_sufficiency_reading).

% DUAL FORMULATION NOTE:
% The 'climate_mitigation_legitimacy' kernel decomposes into four structurally distinct constraint readings, each with different beneficiary sets, policy implications, and ε values. This story ('baseload_necessity_reading') asserts firm power necessity; siblings ('renewable_primacy_reading', 'portfolio_pragmatism_reading', 'degrowth_sufficiency_reading') assert alternative pathways are sufficient. Each is a separate constraint with its own ε referent (the standing decarbonization arrangement under that reading's lights), its own victims and beneficiaries, and its own classification by the engine. The readings coexist in policy discourse and influence one another through shared policy-resource constraints (capital allocation to nuclear vs. renewable R&D, grid-code stringency affecting deployment feasibility), hence the network coupling. Each reading's ε is independent and stable within that reading's framing; the engine computes per-seat classification for each reading separately.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(climate_mitigation_legitimacy__baseload_necessity_reading, analytical, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
