% ============================================================================
% CONSTRAINT STORY: climate_mitigation_imperative__portfolio_optimization_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_climate_mitigation_imperative__portfolio_optimization_reading, []).

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
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: climate_mitigation_imperative__portfolio_optimization_reading
 *   human_readable: Climate Mitigation Portfolio Optimization (Nuclear Baseload Reading)
 *   domain: energy_policy/climate_mitigation/technology_governance
 *
 * SUMMARY:
 *   This constraint embodies ONE READING of the climate mitigation
 *   imperative: that decarbonization requires maximizing all low-carbon
 *   sources, with nuclear providing necessary reliable baseload. Under this
 *   reading, nuclear enters the beneficiary set (receives policy support),
 *   fossil fuel producers are the primary victims (face accelerated
 *   stranded-asset risk and phase-out), and carbon intensity becomes the sole
 *   ordering criterion across energy technologies. This is not the only
 *   defensible reading of the founding problem (climate change requires rapid
 *   decarbonization); it is a contestable specification of what 'maximal
 *   decarbonization' means operationally. The sibling readings
 *   (opportunity_cost_reading: fastest deployment per dollar;
 *   systems_transition_reading: democratized decentralized systems) offer
 *   structurally different constraints from the same climate problem.
 *
 * KEY AGENTS:
 *   - Nuclear industry (beneficiary, organized power, arbitrage exit)
 *   - Climate policy advisors / decarbonization advocates (beneficiary, moderate power, constrained exit)
 *   - Fossil fuel producers (victim, powerful but compressed by policy, constrained exit)
 *   - Coal-mining regions and workers (victim, powerless, trapped)
 *   - Natural gas infrastructure investors (victim, powerful, constrained exit)
 *   - Renewable energy sector (dual: benefits from carbon criterion, constrained by baseload priority)
 *   - Grid operators (agenda setter, enforces portfolio priority)
 *   - Systems transition advocates (excluded, moderate power, mobile)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_mitigation_imperative__portfolio_optimization_reading, 0.58).
domain_priors:suppression_score(climate_mitigation_imperative__portfolio_optimization_reading, 0.42).
domain_priors:theater_ratio(climate_mitigation_imperative__portfolio_optimization_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_mitigation_imperative__portfolio_optimization_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(climate_mitigation_imperative__portfolio_optimization_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(climate_mitigation_imperative__portfolio_optimization_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(climate_mitigation_imperative__portfolio_optimization_reading, accessibility_collapse, 0.71).
narrative_ontology:constraint_metric(climate_mitigation_imperative__portfolio_optimization_reading, resistance, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_mitigation_imperative__portfolio_optimization_reading, tangled_rope).
narrative_ontology:human_readable(climate_mitigation_imperative__portfolio_optimization_reading, "Climate Mitigation Portfolio Optimization (Nuclear Baseload Reading)").
narrative_ontology:topic_domain(climate_mitigation_imperative__portfolio_optimization_reading, "energy_policy/climate_mitigation/technology_governance").

domain_priors:requires_active_enforcement(climate_mitigation_imperative__portfolio_optimization_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_mitigation_imperative__portfolio_optimization_reading, 'e18001fb-1897-4017-80a7-59956413326e').
narrative_ontology:cs_kernel_codification('e18001fb-1897-4017-80a7-59956413326e', formalized).
narrative_ontology:cs_authority_grounding('e18001fb-1897-4017-80a7-59956413326e', expertise).
narrative_ontology:cs_interpretation_layer_present('e18001fb-1897-4017-80a7-59956413326e').
narrative_ontology:cs_reading_relation('e18001fb-1897-4017-80a7-59956413326e', climate_mitigation_imperative__opportunity_cost_reading, coexists_with).
narrative_ontology:cs_reading_relation('e18001fb-1897-4017-80a7-59956413326e', climate_mitigation_imperative__systems_transition_reading, coexists_with).
narrative_ontology:cs_axiom('e18001fb-1897-4017-80a7-59956413326e', foundational, technology_neutrality_carbon_criterion).
narrative_ontology:cs_axiom_status(technology_neutrality_carbon_criterion, holdable).
narrative_ontology:cs_axiom_grounding('e18001fb-1897-4017-80a7-59956413326e', technology_neutrality_carbon_criterion, instrumental).
narrative_ontology:cs_axiom('e18001fb-1897-4017-80a7-59956413326e', foundational, baseload_carbon_displacement_optimization).
narrative_ontology:cs_axiom_status(baseload_carbon_displacement_optimization, holdable).
narrative_ontology:cs_axiom_grounding('e18001fb-1897-4017-80a7-59956413326e', baseload_carbon_displacement_optimization, empirically_contingent).
narrative_ontology:cs_reference_frame('e18001fb-1897-4017-80a7-59956413326e', carbon_intensity_decarbonization_framework).
narrative_ontology:cs_drift_state('e18001fb-1897-4017-80a7-59956413326e', post_2015_paris_agreement_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('e18001fb-1897-4017-80a7-59956413326e', '2026-06-11T14:32:00Z').
narrative_ontology:cs_kernel_id(climate_mitigation_imperative__portfolio_optimization_reading, climate_mitigation_imperative).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_mitigation_imperative__portfolio_optimization_reading, nuclear_industry).
narrative_ontology:constraint_beneficiary(climate_mitigation_imperative__portfolio_optimization_reading, decarbonization_advocates).
narrative_ontology:constraint_victim(climate_mitigation_imperative__portfolio_optimization_reading, fossil_fuel_producers).
narrative_ontology:constraint_victim(climate_mitigation_imperative__portfolio_optimization_reading, coal_mining_regions).
narrative_ontology:constraint_victim(climate_mitigation_imperative__portfolio_optimization_reading, natural_gas_infrastructure_investors).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(climate_mitigation_imperative__portfolio_optimization_reading, renewable_energy_advocates).
narrative_ontology:constraint_victim(climate_mitigation_imperative__portfolio_optimization_reading, renewable_energy_advocates).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Nuclear operators and suppliers receive policy support, preferential grid interconnection terms, extended operating licenses, and access to capital subsidies justified by the portfolio optimization imperative. The constraint's carbon-intensity framing — regardless of actual deployment speed — legitimates their position as 'essential decarbonization infrastructure.' Their exit is arbitrage: they can shift investment geography if policies change, but the constraint keeps them in the narrative center of climate mitigation.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__portfolio_optimization_reading, nuclear_industry, beneficiary,
    organized, generational, arbitrage, global).

% Climate scientists, environmental groups, and policy advisors who endorse the portfolio-optimization reading gain institutional legitimacy and policy access. The constraint enables their role as technical advisors guiding energy mix decisions. They claim no direct financial benefit but derive status, publication venues, policy influence, and sustained funding for their research programs. Their exit is constrained: questioning the portfolio framing risks losing access and credibility within the institutional climate-policy apparatus.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__portfolio_optimization_reading, decarbonization_advocates, beneficiary,
    moderate, biographical, constrained, global).

% Coal, oil, and gas extractors face policy-driven stranded-asset risk, carbon-pricing mechanisms, and accelerated retirement timelines justified by the portfolio optimization framing. Even if their infrastructure could provide low-cost (if carbon-intensive) power, the constraint's carbon-intensity criterion pre-empts that option. They cannot exit the energy market itself but face compressed timelines and rising compliance costs. Their exit options are asset conversion (if feasible), geographic relocation to unregulated jurisdictions, or diversification into renewables — all constrained by the policy trajectory.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__portfolio_optimization_reading, fossil_fuel_producers, payer,
    powerful, generational, constrained, global).

% Communities dependent on coal extraction and thermal power generation face economic collapse as the constraint's carbon-intensity focus drives retirement of coal plants and mine closures. Their economic infrastructure is locked to a single commodity; the portfolio optimization framing offers no alternative pathway that preserves local control or employment. They experience the constraint as an imposed externality, not as a chosen commitment to decarbonization. Politically weak and geographically fixed, they are trapped.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__portfolio_optimization_reading, coal_mining_regions, payer,
    powerless, biographical, trapped, local).

% Gas pipelines, LNG terminals, and peaking plant operators face policy uncertainty. While natural gas is lower-carbon than coal, it is still carbon-intensive and the portfolio optimization reading treats it as a transitional fuel with a shrinking policy window. Investors in gas infrastructure face stranded-asset risk; they cannot easily redeploy capital into nuclear (different expertise and timescales) or renewables (different regulatory regime). Their exit is constrained by the infrastructure lock-in and the reading's technological trajectory.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__portfolio_optimization_reading, natural_gas_infrastructure_investors, payer,
    powerful, generational, constrained, national).

% Solar and wind developers benefit from the portfolio framing as carbon-intensity criterion. However, the constraint's emphasis on 'reliable baseload' positions renewable sources as secondary (intermittent, requiring storage or backup), which can reduce their policy priority relative to nuclear. They gain from decarbonization imperative but lose relative status in the technology hierarchy. Their exit is mobile: they can advocate for alternative readings (systems_transition_reading or opportunity_cost_reading) without losing sector position if the reading shifts.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__portfolio_optimization_reading, renewable_energy_advocates, beneficiary,
    moderate, biographical, mobile, global).
narrative_ontology:stakeholder_secondary_role(climate_mitigation_imperative__portfolio_optimization_reading, renewable_energy_advocates, payer).

% System operators enforce the constraint through dispatch rules, interconnection standards, and grid-balancing protocols that prioritize baseload carbon sources (nuclear) while treating renewables as variable. They administer the operationalization of portfolio optimization. Their authority rests on technical feasibility and reliability mandates; the constraint is legitimate to them if it can be operationalized. Their exit is constrained: they cannot simply choose a different constraint without losing technical mandate.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__portfolio_optimization_reading, grid_operators, agenda_setter,
    institutional, generational, constrained, national).

% The atmospheric CO₂ budget and radiative forcing response do not depend on policy readings. This is an analytical seat documenting that the underlying climate imperative is real, independent of how different readings operationalize it. The physics constrains what 'mitigation' means but does not adjudicate between technology portfolios.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__portfolio_optimization_reading, climate_physics, observer,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(climate_mitigation_imperative__portfolio_optimization_reading, climate_physics).

% Energy democracy and decentralization theorists are systematically excluded from the portfolio optimization reading's policy apparatus. They argue the constraint embeds extractive centralization (nuclear requires massive capital, state coordination, and concentrated ownership) and that true mitigation requires democratization and distributed generation. They are kept out not by explicit exclusion but by the reading's prior closure of what counts as 'necessary baseload' — their alternative is pre-emptively illegitimate.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__portfolio_optimization_reading, systems_transition_advocates, excluded,
    moderate, generational, mobile, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(climate_mitigation_imperative__portfolio_optimization_reading, nuclear_industry).
narrative_ontology:fixing_cost_class(climate_mitigation_imperative__portfolio_optimization_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Allocates limited climate-mitigation capital and policy attention across competing low-carbon technologies. The constraint solves the collective problem of decarbonization speed: rather than dispersing effort across many technologies of varying maturity and cost-effectiveness, the portfolio reading coordinates toward a mix designed to maximize reliable carbon displacement at scale.
% TRANSFER_FUNCTION: Transfers policy priority, capital subsidy, grid-dispatch preference, and carbon-pricing benefits FROM fossil fuel sectors TO nuclear operators and renewable developers. The distribution is asymmetric: nuclear operators and their supply chains receive concentrated support; fossil fuel workers and communities bear concentrated costs; renewable developers receive mixed benefits (carbon criterion favors them; baseload priority attenuates them); climate advocates receive status and influence.
% ABSENT_VOICES: Systems transition theorists, energy democracy advocates, workers in fossil fuel sectors (particularly coal-mining communities), and investors in natural gas infrastructure are excluded from the core policy conversation. Their objections to the reading (that it embeds centralization, ignores transition equity, and prioritizes technological lock-in over democratic transformation) are treated as non-technical rather than as substantive policy alternatives. Grid workers and distributed renewable operators are also under-represented — their operational knowledge would challenge the baseload assumption.
% DISAPPEARANCE_RATIONALE: If the portfolio optimization reading disappeared and the carbon-intensity criterion were abandoned in favor of an alternative reading (opportunity-cost or systems-transition), the entire energy infrastructure policy landscape would reorganize: nuclear deployment timelines would be reconsidered, renewable investment would accelerate without baseload qualification, fossil fuel phase-out would change pace, and capital allocation would shift radically. The constraint is not natural law; it is a policy choice whose removal would permit alternative energy futures.
% FOUNDING_PROBLEM: Climate change requires decarbonization at global scale and speed. Early mitigation modeling showed that no single technology could decarbonize the entire energy system; diverse low-carbon sources would be necessary. The portfolio optimization reading emerged as a response: carbon intensity as the sole criterion, with the corollary that all low-carbon sources (including nuclear) should be maximized to achieve fastest displacement of fossil fuels.
% FOUNDING_PROBLEM_CORROBORATION: Climate scientists attest that the founding problem (need for rapid decarbonization) is live and growing. However, energy economists and systems analysts outside the portfolio-optimization reading dispute whether the founding problem is adequately solved by this particular reading. The International Energy Agency's Net Zero roadmaps endorse portfolio optimization (supporting the reading); critiques from the Post-Carbon Institute and distributed-energy researchers argue the reading mis-specifies the problem and locks in sub-optimal solutions. Academic literature shows genuine disagreement: meta-analyses of decarbonization pathways show some models support portfolio optimization while others show faster decarbonization via renewables-first or demand-reduction strategies. The founding problem is real; the reading's claim to solve it optimally is contested.
narrative_ontology:disappearance_verdict(climate_mitigation_imperative__portfolio_optimization_reading, world_rearranges).
narrative_ontology:founding_problem_status(climate_mitigation_imperative__portfolio_optimization_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(climate_mitigation_imperative__portfolio_optimization_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(climate_mitigation_imperative__portfolio_optimization_reading, 'none', 1).
narrative_ontology:epsilon_provenance(climate_mitigation_imperative__portfolio_optimization_reading, 0.58, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(climate_mitigation_imperative__portfolio_optimization_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(climate_mitigation_imperative__portfolio_optimization_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(climate_mitigation_imperative__portfolio_optimization_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises from 0.38 to 0.58 over the interval (observed through t=16, then projected) because the policy momentum of carbon-intensity prioritization creates increasing support for nuclear while accelerating fossil fuel write-downs. The extraction is real but not absolute: there is genuine coordination around decarbonization (the founding problem is legitimate), which prevents this from classifying as pure snare. However, the extraction becomes clearer as time passes — coal regions face real job loss, natural gas investors face stranded assets, and the constraint's carbon criterion becomes increasingly weaponized against alternatives that do not fit the portfolio frame. Theater ratio is moderate (0.28 at endpoint): some enforcement activity is genuine (ensuring low-carbon power reaches the grid), but a growing fraction is defending the specific portfolio mix rather than optimizing carbon displacement per unit cost or per unit time. The suppression requirement is lower (0.42 at endpoint) than high-extraction snares because the constraint rides on a legitimate coordination problem; the climate crisis itself provides much of the legitimacy without requiring active suppression of alternatives — instead, alternatives are pre-excluded by the reading's prior framing of what counts as 'necessary.'
 *
 * PERSPECTIVAL GAP:
 *   The nuclear industry and climate policy advisors experience the constraint as coordination (solving the collective decarbonization problem). The fossil fuel sector experiences it as extraction (policy-driven asset destruction). Grid operators experience it as a reliability mandate that creates operational constraints. Coal regions experience it as external imposition. From the nuclear seat the constraint is legitimate technology neutrality applied to carbon intensity. From the fossil-fuel seat it is naked policy bias toward a specific technology that happens to be theirs. From the grid-operator seat it is a balancing act (must maintain reliability while deploying the mix). The engine computes this divergence from the structural data — the stakeholder situations are authentically different.
 *
 * DIRECTIONALITY LOGIC:
 *   See above: directionality varies by agent power, exit options, and structural relationship. No overrides needed; the derivation chain (beneficiary/victim + power + exit → d) produces accurate relative values.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is CLAIMED as tangled_rope: genuine coordination (decarbonization problem is real) combined with asymmetric extraction (fossil fuel sectors are targeted, nuclear is elevated). The authorization for calling it tangled rather than snare comes from the founding problem being live and non-contested at the physics level — climate change is real and requires action. The contested part is not the founding problem but whether THIS READING's solution is optimal or fair. The constraint requires active enforcement (grid rules, interconnection standards, carbon-pricing mechanisms) to suppress fossil fuel alternatives. Mandatrophy risk: if the founding problem (climate change) were somehow solved or ceased to be credible, the extraction would be fully visible and the constraint would collapse toward snare. However, the climate problem is not solved and is increasingly urgent, which sustains the coordination framing. Sibling readings (opportunity_cost, systems_transition) offer alternative operationalizations of the same founding problem, which is why they are siblings, not competitors on different problems.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    baseload_necessity,
    'Is reliable baseload power structurally necessary for decarbonization, or can a combination of renewables plus storage and demand management achieve equivalent reliability at lower cost and faster deployment?',
    'Operational data from jurisdictions using high renewable penetration (Denmark, Costa Rica, Uruguay); grid-simulation studies comparing baseload-required vs. demand-flexible scenarios; post-2030 empirical outcomes from renewable-heavy systems.',
    'If baseload proves unnecessary, nuclear''s claimed role as ''essential'' becomes policy preference rather than technical requirement, moving the constraint toward pure extraction. If baseload is structurally necessary, the portfolio reading is validated. This is the empirical crux of the reading''s legitimacy.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(baseload_necessity, empirical, 'Whether baseload is a technical requirement or a policy choice.').

omega_variable(
    opportunity_cost_displacement,
    'For a given decarbonization budget, does capital deployed to nuclear achieve faster or slower total CO₂ displacement than the same capital deployed to renewables, storage, and efficiency?',
    'Prospective analysis: compare deployment scenarios (nuclear-heavy vs. renewable-heavy vs. balanced) on timeline and carbon-intensity curves through 2050. Retrospective analysis: compare national pathways and their carbon reduction per unit capital. International Energy Agency pathway scenarios provide some evidence; meta-analysis of IPCC models shows disagreement.',
    'If renewables displace carbon faster per dollar, the portfolio framing becomes a question of risk diversification (valid coordination) vs. false optimization (extraction defending slower technology). This omega routes to whether the constraint solves the founding problem optimally or just claims to.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(opportunity_cost_displacement, empirical, 'Whether portfolio optimization achieves fastest decarbonization per unit capital, or whether opportunity cost is being paid.').

omega_variable(
    systems_transition_foreclosure,
    'Does the portfolio-optimization reading logically foreclose the systems-transition reading, or do they coexist as different operationalizations of the same founding problem?',
    'Conceptual analysis of the axioms: if portfolio-optimization requires ''technology neutrality'' (all low-carbon sources treated as equivalent on carbon intensity) and systems-transition requires ''decentralization'' (rejecting centralized nuclear), are these logically incompatible in one framework? Or do they describe different priorities that different parties can hold?',
    'If the readings foreclose each other, the constraint is part of a fundamental policy choice with no middle ground. If they coexist, both are live policy options and the contest is over priority weighting, not logical necessity. This affects whether the sibling reading in the network is ''competitor'' or ''alternative specification.''',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(systems_transition_foreclosure, conceptual, 'Whether the portfolio and systems-transition readings are logically incompatible or coexist as different operative choices.').

omega_variable(
    suppression_mechanism_internalized,
    'Is the exclusion of systems-transition voices from the policy apparatus structural (institutional gatekeeping) or internalized (the excluded parties have internalized the portfolio framing and believe it is objectively necessary)?',
    'Post-policy-shift trajectories: if policy shifted toward systems-transition framing, would the previously excluded voices re-engage, or have they been so thoroughly incorporated into the portfolio framework that they continue advocating it? Comparison with other policy domains where excluded voices later drove reframes.',
    'If suppression is structural, the constraint''s effective suppression is accurately measured and the exclusion is a policy choice. If internalized, the excluded parties carry the suppression with them even if the policy changes — the constraint has colonized their cognition. This affects the long-term stability and reversibility of the constraint.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_mechanism_internalized, empirical, 'Whether policy exclusion is structural gatekeeping or internalized constraint adoption.').

omega_variable(
    coal_region_path_dependency,
    'Could coal-producing regions successfully transition to renewable manufacturing and deployment (solar/wind factories, grid infrastructure), or does the portfolio-optimization framing trap them by assuming no alternative economic base?',
    'Case studies of coal-region transition success (Germany''s Ruhr Valley, examples of industrial repurposing); investment analysis of renewable manufacturing capacity in coal regions; labor-force retraining data.',
    'If alternative economic bases are feasible, the constraint''s victimization of coal regions is partly policy failure (no just-transition plan) rather than inevitable. If alternatives are infeasible, the constraint enforces real economic collapse in those regions. This affects whether the mandatrophy of coal regions is remediable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coal_region_path_dependency, empirical, 'Whether coal-region victims could transition to alternative economic bases under the constraint.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_mitigation_imperative__portfolio_optimization_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clim_tr_t0, climate_mitigation_imperative__portfolio_optimization_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement(clim_tr_t8, climate_mitigation_imperative__portfolio_optimization_reading, theater_ratio, 8, 0.21).
narrative_ontology:measurement(clim_tr_t16, climate_mitigation_imperative__portfolio_optimization_reading, theater_ratio, 16, 0.25).
narrative_ontology:measurement(clim_tr_t24, climate_mitigation_imperative__portfolio_optimization_reading, theater_ratio, 24, 0.27).
narrative_ontology:measurement(clim_tr_t32, climate_mitigation_imperative__portfolio_optimization_reading, theater_ratio, 32, 0.28).
narrative_ontology:measurement(clim_tr_t40, climate_mitigation_imperative__portfolio_optimization_reading, theater_ratio, 40, 0.28).

% Extraction over time
narrative_ontology:measurement(clim_be_t0, climate_mitigation_imperative__portfolio_optimization_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(clim_be_t8, climate_mitigation_imperative__portfolio_optimization_reading, base_extractiveness, 8, 0.44).
narrative_ontology:measurement(clim_be_t16, climate_mitigation_imperative__portfolio_optimization_reading, base_extractiveness, 16, 0.51).
narrative_ontology:measurement(clim_be_t24, climate_mitigation_imperative__portfolio_optimization_reading, base_extractiveness, 24, 0.55).
narrative_ontology:measurement(clim_be_t32, climate_mitigation_imperative__portfolio_optimization_reading, base_extractiveness, 32, 0.58).
narrative_ontology:measurement(clim_be_t40, climate_mitigation_imperative__portfolio_optimization_reading, base_extractiveness, 40, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(clim_su_t0, climate_mitigation_imperative__portfolio_optimization_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(clim_su_t8, climate_mitigation_imperative__portfolio_optimization_reading, suppression_requirement, 8, 0.38).
narrative_ontology:measurement(clim_su_t16, climate_mitigation_imperative__portfolio_optimization_reading, suppression_requirement, 16, 0.4).
narrative_ontology:measurement(clim_su_t24, climate_mitigation_imperative__portfolio_optimization_reading, suppression_requirement, 24, 0.41).
narrative_ontology:measurement(clim_su_t32, climate_mitigation_imperative__portfolio_optimization_reading, suppression_requirement, 32, 0.42).
narrative_ontology:measurement(clim_su_t40, climate_mitigation_imperative__portfolio_optimization_reading, suppression_requirement, 40, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_mitigation_imperative__portfolio_optimization_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(climate_mitigation_imperative__portfolio_optimization_reading, 0.18).
narrative_ontology:affects_constraint(climate_mitigation_imperative__portfolio_optimization_reading, climate_mitigation_imperative__opportunity_cost_reading).
narrative_ontology:affects_constraint(climate_mitigation_imperative__portfolio_optimization_reading, climate_mitigation_imperative__systems_transition_reading).
narrative_ontology:affects_constraint(climate_mitigation_imperative__portfolio_optimization_reading, nuclear_subsidy_regime).
narrative_ontology:affects_constraint(climate_mitigation_imperative__portfolio_optimization_reading, fossil_fuel_phase_out_obligation).

% DUAL FORMULATION NOTE:
% This constraint is one reading of a three-way kernel decomposition around climate mitigation. The portfolio_optimization_reading specifies mitigation as 'maximize all low-carbon sources, nuclear provides baseload.' Sibling readings (opportunity_cost_reading: fastest deployment per dollar; systems_transition_reading: decentralized democratic systems) offer alternative operationalizations of the same founding problem. All three readings share the carbon-intensity criterion but differ on what 'maximal mitigation' means operationally. The readings do NOT replace each other; they coexist as live policy alternatives held by different institutional constituencies. The portfolio reading dominates mainstream climate policy (IPCC, national government adoption), while the opportunity-cost reading is held by some energy economists and the systems-transition reading by energy-democracy advocates. The network links show upstream pressure: portfolio → opportunity-cost (if opportunity-cost trajectory proves faster, portfolio loses empirical justification); portfolio → systems-transition (if decentralization becomes politically viable, systems-transition undermines nuclear's role). Each story carries its own ε and beneficiary/victim structure to preserve reading-independence.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(climate_mitigation_imperative__portfolio_optimization_reading, organized, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
