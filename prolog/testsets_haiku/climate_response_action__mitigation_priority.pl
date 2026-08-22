% ============================================================================
% CONSTRAINT STORY: climate_response_action__mitigation_priority
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
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
 *   constraint_id: climate_response_action__mitigation_priority
 *   human_readable: Climate Mitigation via Emissions Reduction + Carbon Markets + Growth Maintenance
 *   domain: climate_policy/political_economy/intergenerational_ethics
 *
 * SUMMARY:
 *   This constraint instantiates ONE reading of the contested
 *   climate-response kernel: mitigation priority — the dominant global policy
 *   framing — which holds that limiting warming to 2°C (or 1.5°C) through
 *   emissions reductions, powered by technological innovation and carbon
 *   markets, within GDP-growth frameworks, is the primary climate response.
 *   This reading concentrates near-term mitigation costs on high-emissions
 *   sectors and wealthy nations; defers adaptation costs and climate exposure
 *   to vulnerable populations and future generations; assumes carbon-removal
 *   technologies will mature to handle residual emissions; and benefits
 *   nations with capital for innovation. The competing readings
 *   (adaptation_priority, degrowth_transformation) are structurally different
 *   constraints with different ε values and victim/beneficiary sets; they are
 *   NOT alternative views of the same constraint but different constraints
 *   solving different problems. This story author the mitigation-priority
 *   reading as a single, ε-invariant constraint without describing the
 *   alternatives inside it.
 *
 * KEY AGENTS:
 *   - High-income innovation economies (USA, EU, Japan, South Korea): primary beneficiaries of mitigation framing; have capital and infrastructure for clean-tech deployment; set global agenda via UNFCCC and multilateral development banks.
 *   - Carbon-market intermediaries (financial firms, offset brokers, carbon exchanges): extract rents from the gap between verified carbon-reduction cost and traded-credit price; lobby for market expansion.
 *   - Fossil-fuel transition winners (renewable-energy firms, battery/EV manufacturers, grid-modernization companies): benefit from subsidy regimes and carbon-pricing incentives tied to mitigation targets.
 *   - High-emissions-dependent workers (coal, oil, gas, heavy manufacturing): face front-loaded unemployment and deskilling; retraining programs are inadequate.
 *   - Climate-vulnerable populations (small island states, arid regions, flood-prone areas): bear climate impacts now while mitigation takes decades; lack capital for adaptation; deferred investment in resilience.
 *   - Future generations: bear the cost of mitigation failure (carbon removal, residual warming impacts) and adaptation deficits; cannot negotiate.
 *   - Low-income nations: constrained to expensive renewable pathways while climate-vulnerable; carbon offsets create perverse incentives; excluded from innovation capture.
 *   - Carbon-removal researchers: benefit from funding that assumes mitigation will require negative-emissions technology; shape technological-feasibility narratives.
 *   - Regulatory authorities (UNFCCC, IEA, central banks): gain institutional authority and budgets as mitigation becomes central governance function; legitimacy rests on assumptions about decoupling growth from emissions.
 *   - Excluded: adaptation-priority constituencies and degrowth advocates who would restructure the constraint if seated.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_response_action__mitigation_priority, 0.68).
domain_priors:suppression_score(climate_response_action__mitigation_priority, 0.62).
domain_priors:theater_ratio(climate_response_action__mitigation_priority, 0.44).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_response_action__mitigation_priority, extractiveness, 0.68).
narrative_ontology:constraint_metric(climate_response_action__mitigation_priority, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(climate_response_action__mitigation_priority, theater_ratio, 0.44).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(climate_response_action__mitigation_priority, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(climate_response_action__mitigation_priority, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_response_action__mitigation_priority, tangled_rope).
narrative_ontology:human_readable(climate_response_action__mitigation_priority, "Climate Mitigation via Emissions Reduction + Carbon Markets + Growth Maintenance").
narrative_ontology:topic_domain(climate_response_action__mitigation_priority, "climate_policy/political_economy/intergenerational_ethics").

domain_priors:requires_active_enforcement(climate_response_action__mitigation_priority).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_response_action__mitigation_priority, '47471fbd-fa44-4f57-abeb-b392bedf511c').
narrative_ontology:cs_kernel_codification('47471fbd-fa44-4f57-abeb-b392bedf511c', formalized).
narrative_ontology:cs_authority_grounding('47471fbd-fa44-4f57-abeb-b392bedf511c', extraction).
narrative_ontology:cs_interpretation_layer_present('47471fbd-fa44-4f57-abeb-b392bedf511c').
narrative_ontology:cs_reading_relation('47471fbd-fa44-4f57-abeb-b392bedf511c', climate_response_action__adaptation_priority, influences).
narrative_ontology:cs_reading_relation('47471fbd-fa44-4f57-abeb-b392bedf511c', climate_response_action__degrowth_transformation, influences).
narrative_ontology:cs_axiom('47471fbd-fa44-4f57-abeb-b392bedf511c', foundational, technological_decoupling_growth_compatible).
narrative_ontology:cs_axiom_status(technological_decoupling_growth_compatible, holdable).
narrative_ontology:cs_axiom_grounding('47471fbd-fa44-4f57-abeb-b392bedf511c', technological_decoupling_growth_compatible, empirically_contingent).
narrative_ontology:cs_axiom('47471fbd-fa44-4f57-abeb-b392bedf511c', foundational, carbon_markets_efficient_allocation).
narrative_ontology:cs_axiom_status(carbon_markets_efficient_allocation, holdable).
narrative_ontology:cs_axiom_grounding('47471fbd-fa44-4f57-abeb-b392bedf511c', carbon_markets_efficient_allocation, instrumental).
narrative_ontology:cs_reference_frame('47471fbd-fa44-4f57-abeb-b392bedf511c', paris_agreement_1_5c_2c_pathway).
narrative_ontology:cs_drift_state('47471fbd-fa44-4f57-abeb-b392bedf511c', contemporary_2024_post_cop28, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('47471fbd-fa44-4f57-abeb-b392bedf511c', '').
narrative_ontology:cs_kernel_id(climate_response_action__mitigation_priority, climate_response_action).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_response_action__mitigation_priority, high_income_innovation_economies).
narrative_ontology:constraint_beneficiary(climate_response_action__mitigation_priority, carbon_market_intermediaries).
narrative_ontology:constraint_beneficiary(climate_response_action__mitigation_priority, fossil_fuel_transition_winners).
narrative_ontology:constraint_victim(climate_response_action__mitigation_priority, climate_vulnerable_populations).
narrative_ontology:constraint_victim(climate_response_action__mitigation_priority, future_generations).
narrative_ontology:constraint_victim(climate_response_action__mitigation_priority, high_emissions_dependent_workers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(climate_response_action__mitigation_priority, low_income_nations).
narrative_ontology:constraint_beneficiary(climate_response_action__mitigation_priority, carbon_removal_researchers).
narrative_ontology:constraint_victim(climate_response_action__mitigation_priority, low_income_nations).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Nations with capital for R&D, existing low-carbon infrastructure, and financial markets. Benefit from carbon markets (sell credits), technology licensing, and first-mover advantage in clean-energy sectors. Their emissions reductions are technically feasible within GDP-growth frameworks. Export carbon-reduction obligations to lower-income nations via carbon offsets while maintaining consumption.
narrative_ontology:constraint_stakeholder(climate_response_action__mitigation_priority, high_income_innovation_economies, beneficiary,
    institutional, generational, arbitrage, global).

% Financial institutions, offset brokers, carbon traders. Extract value from the gap between verification cost and traded-credit price. Profit from fungibility of credits across geographies and time periods. Their continued existence depends on the carbon-market mechanism; they lobby to maintain and expand carbon-trading infrastructure.
narrative_ontology:constraint_stakeholder(climate_response_action__mitigation_priority, carbon_market_intermediaries, beneficiary,
    organized, biographical, arbitrage, global).

% Renewable-energy companies, battery manufacturers, electric-vehicle makers, grid-modernization firms. Benefit from mitigation-focused policy (subsidies, mandates, carbon pricing). Accumulate capital and political influence as mitigation investments expand. Lobby for technology-intensive emissions reduction.
narrative_ontology:constraint_stakeholder(climate_response_action__mitigation_priority, fossil_fuel_transition_winners, beneficiary,
    organized, biographical, mobile, global).

% Workers in coal, oil, gas, cement, and heavy manufacturing sectors. Face unemployment or deskilling when carbon-intensive industries contract. Retraining programs funded under mitigation frameworks are often inadequate or geographically mismatched. Their communities bear the front-loaded costs of decarbonization.
narrative_ontology:constraint_stakeholder(climate_response_action__mitigation_priority, high_emissions_dependent_workers, payer,
    moderate, biographical, constrained, regional).

% Low-lying island nations, arid regions, tropical areas facing floods and heat extremes. Mitigation-priority framing defers investment in adaptive infrastructure (seawalls, drought-resistant agriculture, climate-resilient housing) in favor of global emissions reduction. They bear climate impacts now while waiting for mitigation to take effect (20–50 years lag). Exit is migration, which is politically and economically constrained.
narrative_ontology:constraint_stakeholder(climate_response_action__mitigation_priority, climate_vulnerable_populations, payer,
    powerless, biographical, trapped, local).

% Those born post-2050. Bear whatever climate impacts mitigation fails to prevent, plus any adaptation deficits and stranded-carbon-removal costs (negative-emissions technology). The mitigation pathway assumes their labor will bear the cost of carbon removal if emissions-reduction targets miss. Cannot negotiate, exit, or object to terms.
narrative_ontology:constraint_stakeholder(climate_response_action__mitigation_priority, future_generations, payer,
    powerless, civilizational, trapped, global).

% Developing nations with high population growth, energy-poverty, and development aspirations. Mitigation framing asks them to adopt expensive renewables and efficiency while remaining climate-vulnerable. Carbon-offset sales can generate revenue but create perverse incentives (forest protection over land rights). They benefit from clean-energy access but pay through foregone industrial development pathways.
narrative_ontology:constraint_stakeholder(climate_response_action__mitigation_priority, low_income_nations, payer,
    moderate, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(climate_response_action__mitigation_priority, low_income_nations, beneficiary).

% Scientists and engineers working on direct-air capture, enhanced weathering, ocean alkalinization, bioenergy with carbon capture. Benefit from research funding that assumes carbon removal will be necessary. Their field's expansion and legitimacy depends on mitigation-failure scenarios becoming real. Shape policy narratives around technological feasibility.
narrative_ontology:constraint_stakeholder(climate_response_action__mitigation_priority, carbon_removal_researchers, beneficiary,
    organized, biographical, mobile, global).
narrative_ontology:stakeholder_secondary_role(climate_response_action__mitigation_priority, carbon_removal_researchers, agenda_setter).

% National and international climate bodies (UNFCCC, IEA, central banks). Enforce or coordinate emissions targets, carbon-market rules, and technology mandates. Gain institutional authority and budgets as mitigation becomes a central governance domain. Their legitimacy rests on the assumption that emissions can be reduced within growth frameworks.
narrative_ontology:constraint_stakeholder(climate_response_action__mitigation_priority, regulatory_authorities, agenda_setter,
    institutional, generational, analytical, national).

% Communities and nations advocating for climate-justice framing and immediate adaptation investment. Structurally excluded from setting global climate agenda: mitigation dominates climate finance and international agreements. Would prioritize resilience and equity over global emissions targets if seated.
narrative_ontology:constraint_stakeholder(climate_response_action__mitigation_priority, adaptation_priority_constituencies, excluded,
    moderate, biographical, constrained, local).

% Movements and scholars arguing climate response requires rejecting GDP growth, restructuring consumption, and redistributing resources. Excluded from mainstream policy design: carbon markets and green growth assume technological decoupling of emissions from growth. Would reshape the constraint entirely if admitted to agenda-setting.
narrative_ontology:constraint_stakeholder(climate_response_action__mitigation_priority, degrowth_transformation_advocates, excluded,
    moderate, generational, constrained, global).

% Independent climate scientists, economists, and policy analysts examining the mitigation pathway's actual efficacy. Can testify to empirical gaps (carbon-removal feasibility, emissions-reductions track record, adaptation-cost underestimation) but do not set policy.
narrative_ontology:constraint_stakeholder(climate_response_action__mitigation_priority, observer_analytical, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(climate_response_action__mitigation_priority, high_income_innovation_economies).
narrative_ontology:fixing_cost_class(climate_response_action__mitigation_priority, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the collective-action problem of controlling atmospheric CO₂: no single nation benefits from unilateral reduction (free-rider pressure); coordinating on global emissions targets aligns incentives (or attempts to). Carbon markets and technology transfer are coordination mechanisms enabling nations to reduce emissions at lowest cost by trading and sharing innovation.
% TRANSFER_FUNCTION: Moves mitigation costs (renewable investment, industrial restructuring, worker retraining) to high-emissions sectors and high-income economies in the short term. Moves carbon-credit revenue to nations with forest/land offsets. Defers adaptation costs and climate-impact exposure to vulnerable regions and future generations. Transfers assumption that carbon removal will work (cost of bet on negative-emissions technology) to post-2050 populations.
% ABSENT_VOICES: Climate-vulnerable populations in lowest-income nations are largely excluded from mitigation governance; they would demand adaptation funding and loss-and-damage compensation but are voices in UNFCCC negotiation only, never primary agenda-setters. Degrowth advocates and Global South climate-justice movements are structurally excluded from mainstream climate policy; they would reshape the entire constraint but are marginalized. Future generations and non-human communities cannot negotiate.
% DISAPPEARANCE_RATIONALE: If the mitigation-priority constraint (global emissions targets + carbon markets + growth maintenance) collapsed, climate finance and R&D would reorient toward adaptation and resilience; carbon-market institutions would dissolve; renewable-energy deployment would lose subsidies; industrial transition would stall; the constraint structures a $trillions-per-year allocation of capital. Its disappearance would reallocate resources from (costly, uncertain, long-lag) mitigation toward (adaptive, near-term, localized) resilience.
% FOUNDING_PROBLEM: Industrial civilization's energy system runs on carbon fuels, driving atmospheric CO₂ to dangerous levels. Single nations cannot solve this unilaterally; emissions from one nation's neighbor affect all; no pricing mechanism internalizes the climate cost. The founding problem: coordinate global behavior to limit cumulative emissions.
% FOUNDING_PROBLEM_CORROBORATION: Independent climate scientists (IPCC, national academies) attest the founding problem is live: cumulative emissions drive temperature rise, which drives climate impacts. The empirical premise (emissions → temperature → impacts) is corroborated outside the benefiting parties. However, whether the mitigation-priority solution (reduce emissions within growth; use markets and technology) adequately addresses the founding problem is contested: adaptation advocates note that even aggressive mitigation leaves 1.5–2°C warming and require immediate resilience investment; degrowth analysts argue that technological decoupling is empirically insufficient and the founding problem's real solution requires structural economic change, not carbon markets.
narrative_ontology:disappearance_verdict(climate_response_action__mitigation_priority, world_rearranges).
narrative_ontology:founding_problem_status(climate_response_action__mitigation_priority, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(climate_response_action__mitigation_priority, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(climate_response_action__mitigation_priority, 'none', 1).
narrative_ontology:epsilon_provenance(climate_response_action__mitigation_priority, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness (0.68) is high because the constraint concentrates costs on identifiable victims (vulnerable populations, high-emissions workers, future generations) while benefits accrue to institutional beneficiaries (high-income economies, carbon-market intermediaries, clean-tech winners) who set the agenda. The measured extractiveness rises over time (0.48 → 0.68 by end of interval) as: (1) carbon markets mature and extract wider rent margins; (2) renewable-energy deployment accelerates, concentrating economic surplus in tech-owning nations; (3) adaptation deficits accumulate in vulnerable regions, making the deferred-costs strategy increasingly visible as extractive. Suppression (0.62) reflects enforcement of mitigation priorities over adaptation and degrowth alternatives: vulnerable-nation appeals for adaptation finance are suppressed by global finance rules favoring mitigation; indigenous and climate-justice movements are excluded from agenda-setting; workers in carbon-intensive sectors face labor-market suppression as industries contract. Theater (0.44) arises because increasing share of mitigation activity becomes performative: net-zero commitments with weak enforcement, carbon offsets that don't deliver real reductions, green-growth claims disconnected from consumption growth. Accessibility collapse (0.58 structural) reflects that once mitigation-priority framing locked global institutions (UNFCCC, Paris Accord, central bank climate mandates), alternatives became politically and institutionally inaccessible: adaptation funding competes with mitigation budgets; degrowth is excluded from mainstream policy; the constraint's own architecture (carbon markets, technology mandates) preempts other pathways. Resistance (0.71) is substantial because affected populations (climate-vulnerable, high-emissions workers, degrowth advocates) mount real opposition — indigenous land defenses against offset schemes, labor action against factory closures, climate-justice movements demanding reparations — but this resistance is met with suppression and marginalization.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat (high-income nations, regulatory authorities) perceives the constraint as a coordination solution to a shared problem: global emissions require coordinated global reduction; markets and technology enable cost-minimization; growth is decoupled from emissions through efficiency and renewable substitution. The cost is distributed (all nations contribute) and the benefit is universal (lower climate impacts). From the payer seats (vulnerable populations, high-emissions workers, future generations), the same constraint appears as asymmetric extraction: the costs are front-loaded and concentrated (job losses now, climate impacts now, foregone development pathways); the benefits are deferred, uncertain, and unequally distributed (wealthy nations capture clean-tech rents; carbon removal is speculative). The engine computes this divergence from the authored structural data — beneficiary/victim declarations, power atoms, exit options — without forcing reconciliation. The claim (tangled_rope: real coordination + enforced extraction) reflects the structural fact that mitigation DOES solve a genuine collective-action problem (uncoordinated emissions are worse than coordinated reduction) AND DOES operate asymmetrically, extracting from those least able to afford it.
 *
 * DIRECTIONALITY LOGIC:
 *   High-income innovation economies sit near d=0 (full beneficiary): they set the agenda, capture clean-tech benefits, trade carbon credits, and maintain growth. Their power is institutional, their exit is arbitrage (they can adopt alternative frameworks). Carbon-market intermediaries and clean-tech winners sit near d=0.2–0.3: they collect rents but depend on the constraint's continuation and face regulatory risk. High-emissions workers sit near d=0.75–0.85 (target): they bear job losses, constrained exit (retraining inadequate, geographic immobility), powerless positioning (individual moderate power), and identifiable costs. Climate-vulnerable populations sit at d=0.9+ (full target): powerless (no collective institutional power), identity-locked (cannot leave their territories), bearing front-loaded impacts with deferred mitigation benefit, and unable to negotiate terms. Future generations: d=1.0 (full target, analytical): they have no seat at negotiation, cannot exit the accumulated carbon, and bear all residual costs. Low-income nations: d=0.65–0.75 (target): constrained exit (must adopt renewable frameworks via development conditionality), moderate institutional power (coalition voice in UNFCCC but outvoted), bearing climate impacts now, accruing few clean-tech benefits. The constraint's structure makes directionality divergence: from wealthy-nation seats, the arrangement is genuine coordination (solving collective action, enabling innovation). From vulnerable-population seats, the same structure operates as enforced extraction (costs now, benefits deferred, with no guarantee mitigation will prevent their impacts).
 *
 * MANDATROPHY ANALYSIS:
 *   Mandatrophy resolution: The founding problem (coordinate global emissions reduction) is still live — atmospheric CO₂ continues to rise, impacts accumulate. The constraint (mitigation via markets, technology, growth maintenance) persists as the dominant framework despite mounting evidence that: (1) technological decoupling is insufficient (global emissions continue rising despite renewable deployment); (2) carbon offsets do not deliver equivalent reductions (systematic measurement failure); (3) adaptation deficits are accumulating faster than mitigation benefits accrue; (4) carbon-removal technology remains unproven at scale. The constraint persists because beneficiaries (high-income economies, carbon intermediaries, clean-tech firms) have institutional power to maintain it, and because the alternative framings (adaptation priority, degrowth) would require redistributing costs and benefits in ways that threaten beneficiary interests. The theater ratio (0.44, rising to 0.46 by mid-interval) reflects increasing performative activity: net-zero commitments with weak enforcement; carbon-credit creation that doesn't reflect real reductions; green-growth narratives disconnected from consumption growth. The constraint is NOT a piton (it has concentrated beneficiaries who actively defend it, not just inertial persistence); it is a tangled rope operating increasingly as a snare — the coordination story is real, but the extraction is becoming the constraint's primary function as alternatives are suppressed.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    technological_decoupling_feasibility,
    'Can global emissions be reduced to net-zero by 2050–2070 while maintaining GDP growth, via renewable energy and carbon removal technologies?',
    'Track empirical progress: (1) renewable electricity cost curves and deployment rates vs. 2°C pathway; (2) carbon-removal technology maturation (DAC, BECCS, enhanced weathering) and scalability; (3) emissions decoupling in high-income economies — whether emissions actually fall in absolute terms (not just intensity). Global Inventory of Carbon Removal Solutions (GICR) data and independent IPCC assessments.',
    'If decoupling is achievable, the mitigation-priority reading''s core assumption holds, and the constraint''s classification as tangled_rope (real coordination + extraction) stands. If decoupling is not achievable at the scales required, mitigation-priority becomes a false-summit constraint (presented as natural necessity, actually defending growth interests), and the adaptation-priority and degrowth readings would have stronger structural claim.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(technological_decoupling_feasibility, empirical, 'Whether emissions decoupling from growth is technologically and economically feasible at scale.').

omega_variable(
    carbon_removal_cost_and_timeline,
    'Will mature carbon-removal technology be available, affordable, and deployable at the ~Gt/year scale needed to reach net-zero by 2070, or will residual emissions accumulate faster than removal capacity grows?',
    'Direct-air-capture and bioenergy-with-carbon-capture deployment data; cost learning curves; scalability bottlenecks (energy, land, minerals). By 2030–2035, commercial DAC must demonstrate <$100–150/tonne CO₂ at meaningful scale (~Mt/year). By 2040, must project clear path to Gt-scale. If neither occurs, the constraint''s promise that mitigation will prevent dangerous warming fails.',
    'If carbon removal works, future generations'' costs are reduced but shifted to them (they do the removal work). If it fails, future generations bear both unmitigated warming AND inadequate adaptation (because resources were allocated to failed removal rather than resilience). This uncertainty is currently borne by those not at the table — a core extraction mechanism.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(carbon_removal_cost_and_timeline, empirical, 'Feasibility and cost trajectory of negative-emissions technology.').

omega_variable(
    adaptation_deficit_accumulation,
    'Is the deferral of adaptation investment creating irreversible adaptation deficits in vulnerable regions — water systems, agricultural infrastructure, coastal protection — such that even successful mitigation cannot prevent catastrophic impacts?',
    'Monitor climate-vulnerability indices and adaptive capacity in low-income nations: (1) infrastructure investment in climate-resilient systems relative to climate exposure (gap widening = deficit accumulation); (2) loss-and-damage costs in vulnerable regions vs. adaptation finance available (ratio of uncompensated loss); (3) migration pressure and climate-refugee flows as early indicator of adaptation failure. Corroboration from IPCC synthesis reports on differential impacts by region and income.',
    'If adaptation deficits are accumulating, the mitigation-priority reading''s benefit claim (''lower global warming = lower vulnerability'') is misleading for vulnerable populations — they are worse off than adaptation-priority pathway would leave them, because mitigation benefits take decades while adaptation deficits compound immediately.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(adaptation_deficit_accumulation, empirical, 'Whether deferral of adaptation investment is creating irreversible deficits in vulnerable regions.').

omega_variable(
    intergenerational_cost_shifting,
    'Is the mitigation-priority framework systematically shifting climate and removal costs to future generations (through deferred adaptation, carbon-removal debt, and residual-warming exposure) in ways that violate principles of intergenerational equity?',
    'Normative assessment against established intergenerational justice frameworks (Shue, Gardiner, Caney, Stern): does the allocation of mitigation costs (now) vs. benefits (long-term) vs. removal obligations (future) constitute unjust burden-shifting? Empirical complement: compare welfare trajectories of mid-century vs. end-of-century generations under mitigation-priority pathway vs. counterfactual earlier-action or adaptation-priority pathways.',
    'If cost-shifting is substantial and systematic, the constraint''s claim to solve a collective problem is undermined — it solves the problem for current decision-makers at the expense of those excluded from negotiation. The constraint would reclassify from tangled_rope toward snare (extraction from powerless future generations who bear removal obligations).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(intergenerational_cost_shifting, preference, 'Whether mitigation-priority framework shifts climate costs unjustly to future generations.').

omega_variable(
    carbon_market_additionality_failure,
    'Do carbon offset credits actually represent additional emissions reductions, or are they largely phantom reductions (forest preservation that would have occurred anyway, emissions reductions already required by regulation, leakage that shifts emissions to other regions)?',
    'Large-scale audits of carbon-credit projects: (1) ex-post comparison of credited emissions reductions vs. observed emissions in credited regions (post-project vs. baseline); (2) analysis of leakage (did protection in one forest shift logging to another?); (3) review of baseline-setting practices (were baselines artificially inflated to generate credits for minimal action?). Gold Standard, Verra, and academic analyses.',
    'If credits are substantially phantom (estimates range 30–70% depending on credit type), the carbon-market mechanism does not actually reduce global emissions; instead, it generates rents for credit sellers while global emissions continue rising. This would be the mechanism by which the constraint becomes extractive — beneficiaries collect from a system that does not deliver its claimed coordination function.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(carbon_market_additionality_failure, empirical, 'Whether carbon offset credits represent real, additional emissions reductions or phantom reductions.').

omega_variable(
    kernel_choice_under_contention,
    'The mitigation-priority reading assumes the climate problem is solvable within growth frameworks via technology and markets. The adaptation-priority and degrowth readings assume growth-as-usual (or rapid growth in low-income nations) will prevent adequate mitigation and adaptation. Which reading''s diagnostic claim about what is actually necessary is corroborated by evidence, vs. which is defended primarily by beneficiary-party interests?',
    'Meta-analysis: (1) track which reading''s empirical predictions are holding (e.g., does renewable deployment match mitigation-pathway requirements? does carbon removal appear?); (2) audit whose research is funded and by whom (clean-tech companies fund mitigation-optimism research; climate-justice groups fund adaptation/degrowth analyses); (3) examine policy venues — are adaptation and degrowth excluded from UNFCCC/World Bank agendas for technical reasons or institutional reasons?',
    'If mitigation-priority reading is corroborated, it is a genuine tangled_rope — real coordination with real extraction. If adaptation-priority or degrowth reading is corroborated and excluded from agenda, mitigation-priority becomes a false-summit constraint — presented as natural/necessary, defended by beneficiary interests, suppressing alternatives that would actually solve the founding problem.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_choice_under_contention, conceptual, 'Which climate-response reading''s core diagnosis is empirically supported vs. defended primarily by beneficiary interests.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_response_action__mitigation_priority, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clim_tr_t0, climate_response_action__mitigation_priority, theater_ratio, 0, 0.28).
narrative_ontology:measurement(clim_tr_t8, climate_response_action__mitigation_priority, theater_ratio, 8, 0.32).
narrative_ontology:measurement(clim_tr_t16, climate_response_action__mitigation_priority, theater_ratio, 16, 0.37).
narrative_ontology:measurement(clim_tr_t24, climate_response_action__mitigation_priority, theater_ratio, 24, 0.41).
narrative_ontology:measurement(clim_tr_t32, climate_response_action__mitigation_priority, theater_ratio, 32, 0.44).
narrative_ontology:measurement(clim_tr_t40, climate_response_action__mitigation_priority, theater_ratio, 40, 0.46).
narrative_ontology:measurement(clim_tr_t50, climate_response_action__mitigation_priority, theater_ratio, 50, 0.44).

% Extraction over time
narrative_ontology:measurement(clim_be_t0, climate_response_action__mitigation_priority, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(clim_be_t8, climate_response_action__mitigation_priority, base_extractiveness, 8, 0.54).
narrative_ontology:measurement(clim_be_t16, climate_response_action__mitigation_priority, base_extractiveness, 16, 0.6).
narrative_ontology:measurement(clim_be_t24, climate_response_action__mitigation_priority, base_extractiveness, 24, 0.65).
narrative_ontology:measurement(clim_be_t32, climate_response_action__mitigation_priority, base_extractiveness, 32, 0.68).
narrative_ontology:measurement(clim_be_t40, climate_response_action__mitigation_priority, base_extractiveness, 40, 0.71).
narrative_ontology:measurement(clim_be_t50, climate_response_action__mitigation_priority, base_extractiveness, 50, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(clim_su_t0, climate_response_action__mitigation_priority, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(clim_su_t8, climate_response_action__mitigation_priority, suppression_requirement, 8, 0.5).
narrative_ontology:measurement(clim_su_t16, climate_response_action__mitigation_priority, suppression_requirement, 16, 0.56).
narrative_ontology:measurement(clim_su_t24, climate_response_action__mitigation_priority, suppression_requirement, 24, 0.6).
narrative_ontology:measurement(clim_su_t32, climate_response_action__mitigation_priority, suppression_requirement, 32, 0.63).
narrative_ontology:measurement(clim_su_t40, climate_response_action__mitigation_priority, suppression_requirement, 40, 0.65).
narrative_ontology:measurement(clim_su_t50, climate_response_action__mitigation_priority, suppression_requirement, 50, 0.62).

% Leveled coercion grid (OQ-93): 32/32 authored points at t0=0, tn=50
narrative_ontology:measurement(clim_grid_01, climate_response_action__mitigation_priority, accessibility_collapse(class), 0, 0.48).
narrative_ontology:measurement(clim_grid_02, climate_response_action__mitigation_priority, accessibility_collapse(class), 50, 0.68).
narrative_ontology:measurement(clim_grid_03, climate_response_action__mitigation_priority, accessibility_collapse(individual), 0, 0.35).
narrative_ontology:measurement(clim_grid_04, climate_response_action__mitigation_priority, accessibility_collapse(individual), 50, 0.62).
narrative_ontology:measurement(clim_grid_05, climate_response_action__mitigation_priority, accessibility_collapse(organizational), 0, 0.58).
narrative_ontology:measurement(clim_grid_06, climate_response_action__mitigation_priority, accessibility_collapse(organizational), 50, 0.72).
narrative_ontology:measurement(clim_grid_07, climate_response_action__mitigation_priority, accessibility_collapse(structural), 0, 0.55).
narrative_ontology:measurement(clim_grid_08, climate_response_action__mitigation_priority, accessibility_collapse(structural), 50, 0.72).
narrative_ontology:measurement(clim_grid_09, climate_response_action__mitigation_priority, resistance(class), 0, 0.72).
narrative_ontology:measurement(clim_grid_10, climate_response_action__mitigation_priority, resistance(class), 50, 0.7).
narrative_ontology:measurement(clim_grid_11, climate_response_action__mitigation_priority, resistance(individual), 0, 0.68).
narrative_ontology:measurement(clim_grid_12, climate_response_action__mitigation_priority, resistance(individual), 50, 0.62).
narrative_ontology:measurement(clim_grid_13, climate_response_action__mitigation_priority, resistance(organizational), 0, 0.75).
narrative_ontology:measurement(clim_grid_14, climate_response_action__mitigation_priority, resistance(organizational), 50, 0.68).
narrative_ontology:measurement(clim_grid_15, climate_response_action__mitigation_priority, resistance(structural), 0, 0.7).
narrative_ontology:measurement(clim_grid_16, climate_response_action__mitigation_priority, resistance(structural), 50, 0.71).
narrative_ontology:measurement(clim_grid_17, climate_response_action__mitigation_priority, stakes_inflation(class), 0, 0.5).
narrative_ontology:measurement(clim_grid_18, climate_response_action__mitigation_priority, stakes_inflation(class), 50, 0.76).
narrative_ontology:measurement(clim_grid_19, climate_response_action__mitigation_priority, stakes_inflation(individual), 0, 0.42).
narrative_ontology:measurement(clim_grid_20, climate_response_action__mitigation_priority, stakes_inflation(individual), 50, 0.78).
narrative_ontology:measurement(clim_grid_21, climate_response_action__mitigation_priority, stakes_inflation(organizational), 0, 0.55).
narrative_ontology:measurement(clim_grid_22, climate_response_action__mitigation_priority, stakes_inflation(organizational), 50, 0.81).
narrative_ontology:measurement(clim_grid_23, climate_response_action__mitigation_priority, stakes_inflation(structural), 0, 0.45).
narrative_ontology:measurement(clim_grid_24, climate_response_action__mitigation_priority, stakes_inflation(structural), 50, 0.72).
narrative_ontology:measurement(clim_grid_25, climate_response_action__mitigation_priority, suppression(class), 0, 0.45).
narrative_ontology:measurement(clim_grid_26, climate_response_action__mitigation_priority, suppression(class), 50, 0.65).
narrative_ontology:measurement(clim_grid_27, climate_response_action__mitigation_priority, suppression(individual), 0, 0.38).
narrative_ontology:measurement(clim_grid_28, climate_response_action__mitigation_priority, suppression(individual), 50, 0.58).
narrative_ontology:measurement(clim_grid_29, climate_response_action__mitigation_priority, suppression(organizational), 0, 0.52).
narrative_ontology:measurement(clim_grid_30, climate_response_action__mitigation_priority, suppression(organizational), 50, 0.68).
narrative_ontology:measurement(clim_grid_31, climate_response_action__mitigation_priority, suppression(structural), 0, 0.42).
narrative_ontology:measurement(clim_grid_32, climate_response_action__mitigation_priority, suppression(structural), 50, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_response_action__mitigation_priority, resource_allocation).
narrative_ontology:boltzmann_floor_override(climate_response_action__mitigation_priority, 0.18).
narrative_ontology:affects_constraint(climate_response_action__mitigation_priority, climate_response_action__adaptation_priority).
narrative_ontology:affects_constraint(climate_response_action__mitigation_priority, climate_response_action__degrowth_transformation).

% DUAL FORMULATION NOTE:
% The climate-response kernel decomposes into three structurally distinct constraints: MITIGATION_PRIORITY (this story, emissions reductions + technology + growth), ADAPTATION_PRIORITY (resilience + immediate investment + accepting residual warming), DEGROWTH_TRANSFORMATION (structural economic change away from growth). These are NOT alternative interpretations of one constraint; they are different constraints with different ε values, different victim/beneficiary structures, different power distributions, and incompatible institutional logics. Mitigation-priority assumes technological decoupling and global carbon markets can solve the problem; adaptation-priority assumes mitigation will fail and invests in resilience; degrowth assumes both fail without structural change. Each reading has its own axioms, reference frames, and drift states. All three are live positions in global climate governance, but mitigation has institutional dominance (UNFCCC, World Bank, IEA) while adaptation and degrowth are marginalized or excluded. The stories are linked via network.affects_constraints: mitigation-priority influences the other two (by capturing climate finance and agenda-setting, it constrains adaptation and degrowth pathways).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(climate_response_action__mitigation_priority, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
