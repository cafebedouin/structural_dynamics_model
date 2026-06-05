% ============================================================================
% CONSTRAINT STORY: climate_response_legitimacy__adaptation_priority
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_climate_response_legitimacy__adaptation_priority, []).

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
    constraint_indexing:directionality_override/3,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: climate_response_legitimacy__adaptation_priority
 *   human_readable: Climate Response Legitimacy: Adaptation Priority Reading
 *   domain: climate_policy/political_economy/intergenerational_ethics
 *
 * SUMMARY:
 *   The adaptation-priority reading of legitimate climate response
 *   institutionalizes a structural choice: accepting warming trajectories
 *   above 1.5°C (implicitly 2.0°C or higher) while prioritizing
 *   infrastructure and resilience investments to protect vulnerable
 *   populations from climate impacts. This reading is one of three contested
 *   positions within the climate_response_legitimacy kernel. Sibling readings
 *   include mitigation_priority (aggressive emissions reduction through
 *   technology and carbon pricing) and degrowth_transformation (structural
 *   economic transformation in wealthy nations). The adaptation-priority
 *   reading is institutionally mainstream — it structures the UNFCCC's
 *   loss-and-damage mechanisms, green climate fund, and national adaptation
 *   plan frameworks — but it embeds an asymmetric extraction structure:
 *   wealthy nations (primary emitters) preserve high-carbon development
 *   trajectories while deferring the bulk of emissions reductions, while
 *   low-income regions (minimal historical emitters) face immediate
 *   adaptation deficits and compounding costs. The constraint exhibits
 *   tangled-rope structure: genuine coordination function exists (adaptation
 *   infrastructure benefits all regions) alongside systematic extraction
 *   (wealthy nations' development model is subsidized by vulnerable
 *   populations' adaptation burden and future generations' deferred climate
 *   costs). The measurement trajectory shows extractiveness and suppression
 *   rising over 30 years as adaptation costs accumulate faster than promised
 *   finance flows, while theater ratio rises as institutional commitment
 *   increases but actual resource transfer lags. The constraint becomes
 *   increasingly incoherent as the warming trajectory steepens — at high
 *   enough warming, adaptation becomes physically infeasible, revealing the
 *   adaptation-priority frame as a legitimacy cover rather than a functional
 *   response.
 *
 * KEY AGENTS:
 *   - Low-income regions & vulnerable populations: Primary victims (powerless/trapped) — face $350B annual adaptation deficit, zero historical responsibility for emissions, structurally immobile
 *   - Wealthy industrialized nations: Primary beneficiaries (institutional/arbitrage) — preserve high-carbon consumption patterns, defer emissions reduction costs, control adaptation finance conditionality
 *   - Incumbent energy sectors: Beneficiaries (institutional/arbitrage) — adaptation-priority defers transition away from fossil fuels, maintains market share during gradual decarbonization window
 *   - Middle-income nations: Secondary victim (moderate/constrained) — face adaptation costs and climate impacts while servicing external debt; constrained by resource limits and conditionality
 *   - UNFCCC institutional apparatus: Institutional maintainer (institutional/constrained) — adaptation frameworks trapped by consensus requirements, national sovereignty norms, and funding inadequacy; sees own mechanisms as degraded
 *   - Climate justice coalitions (AOSIS, LDC Group, indigenous organizations): Organized victims (organized/mobile) — perceive adaptation-priority as temporary scaffold pending loss-and-damage reparations; retain organizing capacity and exit vision
 *   - Future generations: Deferred victims (analytical/trapped) — not present to negotiate; bear compounding costs of deferred mitigation decisions; adaptation costs compound nonlinearly as warming accelerates
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_response_legitimacy__adaptation_priority, 0.58).
domain_priors:suppression_score(climate_response_legitimacy__adaptation_priority, 0.62).
domain_priors:theater_ratio(climate_response_legitimacy__adaptation_priority, 0.51).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_response_legitimacy__adaptation_priority, extractiveness, 0.58).
narrative_ontology:constraint_metric(climate_response_legitimacy__adaptation_priority, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(climate_response_legitimacy__adaptation_priority, theater_ratio, 0.51).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_response_legitimacy__adaptation_priority, tangled_rope).
narrative_ontology:human_readable(climate_response_legitimacy__adaptation_priority, "Climate Response Legitimacy: Adaptation Priority Reading").
narrative_ontology:topic_domain(climate_response_legitimacy__adaptation_priority, "climate_policy/political_economy/intergenerational_ethics").

domain_priors:requires_active_enforcement(climate_response_legitimacy__adaptation_priority).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_response_legitimacy__adaptation_priority, 'd4e84262-9180-4990-9e8b-c251045c2f3d').
narrative_ontology:cs_kernel_codification('d4e84262-9180-4990-9e8b-c251045c2f3d', formalized).
narrative_ontology:cs_authority_grounding('d4e84262-9180-4990-9e8b-c251045c2f3d', extraction).
narrative_ontology:cs_interpretation_layer_present('d4e84262-9180-4990-9e8b-c251045c2f3d').
narrative_ontology:cs_reading_relation('d4e84262-9180-4990-9e8b-c251045c2f3d', climate_response_legitimacy__mitigation_priority, coexists_with).
narrative_ontology:cs_reading_relation('d4e84262-9180-4990-9e8b-c251045c2f3d', climate_response_legitimacy__degrowth_transformation, influences).
narrative_ontology:cs_axiom('d4e84262-9180-4990-9e8b-c251045c2f3d', foundational, emissions_pathway_negotiable).
narrative_ontology:cs_axiom_status(emissions_pathway_negotiable, holdable).
narrative_ontology:cs_axiom_grounding('d4e84262-9180-4990-9e8b-c251045c2f3d', emissions_pathway_negotiable, instrumental).
narrative_ontology:cs_axiom('d4e84262-9180-4990-9e8b-c251045c2f3d', foundational, adaptation_as_sufficient_response).
narrative_ontology:cs_axiom_status(adaptation_as_sufficient_response, holdable).
narrative_ontology:cs_axiom_grounding('d4e84262-9180-4990-9e8b-c251045c2f3d', adaptation_as_sufficient_response, empirically_contingent).
narrative_ontology:cs_reference_frame('d4e84262-9180-4990-9e8b-c251045c2f3d', anthropocentric_welfare_preservation).
narrative_ontology:cs_drift_state('d4e84262-9180-4990-9e8b-c251045c2f3d', contemporary_high_impact_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('d4e84262-9180-4990-9e8b-c251045c2f3d', '').
narrative_ontology:cs_kernel_id(climate_response_legitimacy__adaptation_priority, climate_response_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_response_legitimacy__adaptation_priority, wealthy_industrialized_nations).
narrative_ontology:constraint_beneficiary(climate_response_legitimacy__adaptation_priority, incumbent_energy_sectors).
narrative_ontology:constraint_beneficiary(climate_response_legitimacy__adaptation_priority, current_consumers_fossil_fuels).
narrative_ontology:constraint_victim(climate_response_legitimacy__adaptation_priority, low_income_regions).
narrative_ontology:constraint_victim(climate_response_legitimacy__adaptation_priority, climate_vulnerable_populations).
narrative_ontology:constraint_victim(climate_response_legitimacy__adaptation_priority, future_generations).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: LOW-INCOME REGIONS / VULNERABLE POPULATIONS (SNARE) — Structurally trapped in a warming world they did not cause. Face immediate impacts (sea-level rise, drought, flooding) while bearing adaptation costs ($350B annual gap) they cannot afford. Extraction flows directly to them via mandatory climate adaptation without corresponding emissions reductions in wealthy nations. No exit — geographic immobility, resource constraints, and political powerlessness prevent relocation or capacity-building. Maximum experienced extraction.
constraint_indexing:constraint_classification(climate_response_legitimacy__adaptation_priority, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: MIDDLE-INCOME NATIONS (TANGLED ROPE) — Constrained by debt servicing, infrastructure costs, and climate impacts; adaptation-priority framework promises some support via green climate fund and bilateral aid but delivery is slow and conditional. Genuine coordination function exists (adaptation infrastructure benefits all regional actors); but asymmetric extraction operates through conditionality, loan structures, and technology licensing costs. Benefit from adaptation finance but bear disproportionate upfront costs. Moderate agency through regional organization and borrowing capacity, but mobility remains limited.
constraint_indexing:constraint_classification(climate_response_legitimacy__adaptation_priority, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: WEALTHY INDUSTRIALIZED NATIONS / INCUMBENT ENERGY SECTORS (ROPE) — Primary beneficiaries. Adaptation-priority framework preserves their development model and high-carbon consumption patterns while deferring decarbonization costs to the future. They experience the constraint as pure coordination: funding adaptation in other regions maintains geopolitical stability and opens markets for climate technology. Net extraction runs toward them. Arbitrage options abundant — can exit fossil fuel dependence gradually, invest in adaptation technology, or shift climate burden to future decades via adaptation financing. Sees adaptation-priority as legitimate because it maintains their interests while appearing responsive.
constraint_indexing:constraint_classification(climate_response_legitimacy__adaptation_priority, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: CLIMATE JUSTICE COALITIONS / LOSS & DAMAGE ADVOCATES (SCAFFOLD) — Organized agents (AOSIS, LDC Group, indigenous organizations) see adaptation-priority as a temporary holding framework pending deeper structural transformation. Sunset logic: adaptation-priority is structurally unstable because it defers the core problem (emissions reduction) while costs compound. Coalition perceives clear exit path through loss-and-damage funding, climate reparations, and technology transfer agreements. Extraction from their perspective is moderate because they retain organizing capacity and see the constraint as negotiable. Sunset emerges when adaptation costs become unsustainable even for wealthy nations (~2040-2050 cost projections).
constraint_indexing:constraint_classification(climate_response_legitimacy__adaptation_priority, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: UNFCCC INSTITUTIONAL APPARATUS (PITON) — The UNFCCC's adaptation-priority positioning is largely performative at scale. Adaptation funds are underfunded relative to need ($350B gap vs. $1T+ requirement); loss-and-damage mechanisms exist on paper but lack enforcement; National Adaptation Plans are authored but sparsely implemented. The framework persists through institutional inertia (COP cyclical renewal, pledge systems, consensus requirements for enforcement) despite low functional impact on actual adaptation capacity. Theater_ratio reflects that much adaptation governance is reporting and planning ritual rather than systemic resource mobilization. The UNFCCC maintains legitimacy through the appearance of action while structural barriers (wealthy nation veto, national sovereignty, funding inadequacy) prevent real redistribution. Institutional actors trapped by dependency on consensus — reform requires agreement from the beneficiaries.
constraint_indexing:constraint_classification(climate_response_legitimacy__adaptation_priority, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / THERMODYNAMIC CONSTRAINT VIEW (MOUNTAIN) — From a civilizational physics perspective, the energy transition required to prevent 2°C warming is a material and thermodynamic constraint: the global energy system is embedded in planetary heat capacity and carbon cycle dynamics that cannot be negotiated away. Adaptation-priority reading naturalizes what should be a forced transition. Warming trajectory above 1.5°C generates adaptation requirements that exceed human capacity to implement at the required speed and scale, making them effectively immutable. However, this perspective risks false summitry — it treats as thermodynamic law what is actually a political choice (wealthy nations rejecting emissions reductions). The adaptation-first framing is not a natural law response to physics but a contingent institutional arrangement that benefits those who caused the problem.
constraint_indexing:constraint_classification(climate_response_legitimacy__adaptation_priority, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(climate_response_legitimacy__adaptation_priority_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(climate_response_legitimacy__adaptation_priority, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(climate_response_legitimacy__adaptation_priority, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(climate_response_legitimacy__adaptation_priority, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(climate_response_legitimacy__adaptation_priority, TR),
    TR >= 0.70.

:- end_tests(climate_response_legitimacy__adaptation_priority_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. The adaptation-priority reading delivers real adaptation finance and coordination benefits to vulnerable regions (lowering extraction), but the asymmetry in historical responsibility and future burden is substantial. Wealthy nations extract benefit by deferring their own emissions reduction costs while requiring vulnerable nations to absorb impacts immediately. The value reflects both the genuine coordination (adaptation does help) and the systematic extraction (burden distribution is asymmetric relative to historical contribution and current capacity). Extractiveness rises over time (0.35 → 0.71) as compounding climate damage increases adaptation costs faster than promised finance flows. Suppression (0.62): Moderate-high. Barriers to exit include geographic immobility (vulnerable populations cannot easily relocate), economic dependency (low-income regions depend on external finance), political powerlessness (SIDS and LDCs lack veto capacity in UNFCCC), and informational asymmetry (adaptation finance conditionality and technology licensing restrict alternatives). Suppression rises over time as climate impacts increase urgency and reduce negotiating power of vulnerable nations. Theater ratio (0.51): Moderate. The adaptation-priority framework includes genuine planning and coordination mechanisms (NDCs, NAPs, GCF projects) but also substantial performative elements — UNFCCC reporting rituals that don't translate to implementation, pledges that exceed actual disbursement, and institutional processes that maintain appearance of action despite structural funding inadequacy. Theater is lower than piton-range because adaptation infrastructure genuinely gets built (functional element exists), but higher than pure-rope because reporting mechanisms exceed implementation mechanisms.
 *
 * PERSPECTIVAL GAP:
 *   This constraint shows maximal perspectival divergence. The wealthy-nation institutional perspective (Perspective 3, Rope) experiences adaptation-priority as pure coordination — funding other regions' adaptation maintains stability and opens climate-tech markets. The low-income victim perspective (Perspective 1, Snare) experiences the identical framework as pure extraction — they face all adaptation costs and climate impacts while wealthy nations defer their own decarbonization. The justice coalition perspective (Perspective 4, Scaffold) sees a temporary holding framework with a sunset (loss-and-damage reparations and emissions reductions will eventually be forced). The UNFCCC apparatus (Perspective 5, Piton) experiences its own mechanisms as degraded theater — the framework persists through inertia despite structural underfunding and low implementation rates. The mountain perspective (Perspective 6) naturalizes the constraint as thermodynamic inevitability (adaptation is necessary because mitigation won't prevent warming), but this risks false summitry — it treats as a law of physics what is actually a political choice (wealthy nations rejecting aggressive mitigation). The perspectival gaps reveal the underlying asymmetry: each agent's classification is internally coherent, but they are mutually contradictory because they describe opposing extraction flows.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) for each perspective derives from the agent's structural position relative to this constraint: beneficiary status, exit capacity, and power level. Wealthy nations are beneficiaries with arbitrage exits (low d → low χ, but effective extraction still high because they benefit from deferral). Low-income populations are victims with trapped exits (high d → high χ). Middle-income actors are mixed victims with constrained exits (moderate d → moderate χ). Organized coalitions are victims with mobile/constrained exits (higher d than trapped but lower than powerless, reflecting retained agency). The UNFCCC is constrained institutional actor (moderate d reflecting both beneficiary role in legitimacy provision and victim role in being trapped by consensus). The computational chain is: (1) identify beneficiary/victim status from constraint structure (wealthy benefit, low-income bear costs); (2) assess exit options from position-specific constraints (low-income trapped by geography and dependency, wealthy mobile via technology and capital); (3) apply power atom (powerless/moderate/institutional); (4) engine derives d from these factors; (5) sigmoid f(d) produces experienced extractiveness. The perspectival gap emerges because the same structural object (warming + adaptation investment) produces opposite directionality values: wealthy-nation beneficiaries see low d and experience the constraint as beneficial coordination, while trapped populations see high d and experience maximum extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION: This constraint resolves the mandatrophy by showing that the adaptation-priority reading is internally coherent as a tangled rope BUT it institutionalizes a choice that generates different pressures for the three sibling readings. Adaptation-priority is coherent if and only if two assumptions hold: (1) emissions_pathway_negotiable — wealthy nations can choose the warming level they accept; (2) adaptation_as_sufficient_response — vulnerable populations can adapt in place at any warming level. These are NOT natural law constraints — they are contingent institutional choices. Mandatrophy resolution requires stating explicitly: adaptation-priority is the wealthy nations' reading of legitimacy because it preserves their interests while appearing responsive. Mitigation-priority is the technological optimist's reading because it promises decarbonization without sacrifice. Degrowth-transformation is the structuralist's reading because it identifies growth-dependence as the root incompatibility. None of these is THE answer; each is a reading of what legitimacy requires, and each embeds different victim/beneficiary structures. The framework's job is to make the choice explicit — to show that accepting adaptation-priority means accepting a 2.5°C+ warming trajectory and its compounding consequences for the powerless, while preserving the development model of the powerful. That clarity is the mandatrophy resolution.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    adaptation_sufficiency_threshold,
    'At what warming trajectory do adaptation costs exceed the economic capacity of middle and low-income regions to implement, rendering further adaptation infeasible?',
    'Climate damage function analysis (IPCC AR6 Chapter 4); empirical tracking of regional adaptation cost trajectories; identification of tipping points where adaptation infrastructure fails faster than it can be rebuilt (e.g., flood defenses outpaced by sea-level rise, crop diversity lost before cultivars can adapt)',
    'If threshold < 2.5°C: adaptation-priority frame collapses into incoherence — accepted warming exceeds adaptation feasibility. If threshold > 3.0°C: adaptation-priority remains nominally viable but costs compound and extraction becomes absolute (adaptation finance insufficient by orders of magnitude). If no clear threshold: adaptation capacity is unbounded (technical optimism) vs. bounded but hard-to-specify (precautionary).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(adaptation_sufficiency_threshold, empirical, 'Adaptation feasibility threshold as function of warming trajectory').

omega_variable(
    wealth_redistribution_mechanism_effectiveness,
    'Does adaptation finance from wealthy to vulnerable nations actually transfer to the vulnerable populations who need it, or does it leak through governance corruption, government capture, and contractor markup?',
    'Audit trail analysis of green climate fund disbursements; tracking of adaptation projects from fund approval to on-ground implementation; comparison of allocated vs. received funds by vulnerable communities; cost-per-beneficiary analysis for adaptation infrastructure',
    'If transfer efficiency > 70%: beneficiary extraction is moderate and adaptation-priority has real coordination function. If transfer efficiency < 40%: most finance benefits elites and construction contractors in middle-income nations; adaptation-priority is primarily extraction mechanism for wealthy nations (legitimacy shell). Affects whether victims truly benefit or merely appear to in reporting structures.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(wealth_redistribution_mechanism_effectiveness, empirical, 'Adaptation finance transfer efficiency to intended beneficiaries').

omega_variable(
    emissions_deferral_cost_compounding,
    'How much does the cost of climate impacts compound per decade of deferred emissions reduction, and at what point do deferred mitigation costs exceed near-term emissions reduction costs (crossing the inversion threshold)?',
    'Integrated assessment model (IAM) comparison: cost of 1.5°C pathway (immediate aggressive mitigation) vs. 2.5°C pathway (adaptation-first deferral) vs. 3.0°C+ trajectory; empirical tracking of damage cost acceleration as warming increases (nonlinear damage functions)',
    'If crossing occurs before 2050: adaptation-priority becomes economically incoherent within wealthy nations themselves — deferred costs exceed current mitigation costs, making the ''cheaper'' approach provably wrong. If crossing occurs after 2100: deferral remains a viable inter-generational bargain (wealthy live well now, costs hit future). Affects whether the constraint is structurally extractive or merely risk-allocating.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(emissions_deferral_cost_compounding, empirical, 'Compounding trajectory of deferred emissions reduction costs').

omega_variable(
    reader_frame_legitimacy_commitment,
    'This constraint is ONE reading of the climate_response_legitimacy kernel. What distinguishes the adaptation_priority reading''s normative commitment from mitigation_priority and degrowth_transformation readings?',
    'Examine the foundational axioms declared in cs_structure: adaptation_priority commits to accepting warming trajectory and protecting populations in situ (axiom: emissions_pathway_negotiable). Mitigation_priority commits to decarbonization without growth constraints (axiom: growth_decoupling_achievable). Degrowth_transformation commits to structural economic change (axiom: growth_incompatible_with_stabilization). Each reading instantiates a different kernel claim; omegas route the contest to explicit reasoning rather than averaging.',
    'If axiom_commitment can be held sincerely by reasonable actors: all three readings coexist (institutional, not logical foreclosure). If one axiom forecloses another: the readings exclude each other and only one can be legitimately held within a single framework. See cs_structure for relation topology.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reader_frame_legitimacy_commitment, conceptual, 'Foundational axiom distinguishing this reading from sibling readings of the climate response legitimacy kernel').

omega_variable(
    island_sovereignty_exit_capacity,
    'For island nations facing imminent territorial loss (AOSIS members, Tuvalu, Maldives), does adaptation-priority framework provide genuine mobility or does it trap populations in a slowly-sinking sovereignty?',
    'Track availability of climate migration pathways: bilateral relocation agreements, climate refugee status recognition, citizenship transfer offers, financial compensation for loss of territory. Compare promised adaptation finance to relocation costs per capita.',
    'If relocation pathways are available and funded: low-income populations have exit option (mobile rather than trapped); extractiveness decreases. If pathways are blocked or inadequately funded: populations are trapped despite nominally accepting adaptation (reveals the snare mechanism). Affects directionality for AOSIS members — are they identity_locked in sovereignty commitment despite physical displacement becoming inevitable?',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(island_sovereignty_exit_capacity, empirical, 'Exit capacity for island nations under adaptation-priority framework').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_response_legitimacy__adaptation_priority, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clim_adapt_tr_t0, climate_response_legitimacy__adaptation_priority, theater_ratio, 0, 0.38).
narrative_ontology:measurement(clim_adapt_tr_t15, climate_response_legitimacy__adaptation_priority, theater_ratio, 15, 0.51).

% Extraction over time
narrative_ontology:measurement(clim_adapt_be_t0, climate_response_legitimacy__adaptation_priority, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(clim_adapt_be_t10, climate_response_legitimacy__adaptation_priority, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(clim_adapt_be_t20, climate_response_legitimacy__adaptation_priority, base_extractiveness, 20, 0.62).
narrative_ontology:measurement(clim_adapt_be_t30, climate_response_legitimacy__adaptation_priority, base_extractiveness, 30, 0.71).

% Suppression requirement over time
narrative_ontology:measurement(clim_adapt_su_t0, climate_response_legitimacy__adaptation_priority, suppression_requirement, 0, 0.42).
narrative_ontology:measurement(clim_adapt_su_t10, climate_response_legitimacy__adaptation_priority, suppression_requirement, 10, 0.55).
narrative_ontology:measurement(clim_adapt_su_t20, climate_response_legitimacy__adaptation_priority, suppression_requirement, 20, 0.68).
narrative_ontology:measurement(clim_adapt_su_t30, climate_response_legitimacy__adaptation_priority, suppression_requirement, 30, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_response_legitimacy__adaptation_priority, resource_allocation).
narrative_ontology:affects_constraint(climate_response_legitimacy__adaptation_priority, climate_response_legitimacy__mitigation_priority).
narrative_ontology:affects_constraint(climate_response_legitimacy__adaptation_priority, climate_response_legitimacy__degrowth_transformation).
narrative_ontology:affects_constraint(climate_response_legitimacy__adaptation_priority, loss_and_damage_financial_mechanism).
narrative_ontology:affects_constraint(climate_response_legitimacy__adaptation_priority, green_climate_fund_disbursement_bottleneck).

% DUAL FORMULATION NOTE:
% This constraint is part of the climate_response_legitimacy kernel family. The three readings (adaptation_priority, mitigation_priority, degrowth_transformation) are structurally distinct constraints with different beneficiary/victim structures, not variations of a single constraint. They are linked via network relationships documenting which sibling each influences. Each reading has its own ε value reflecting its own structural coherence and extractiveness. Adaptation_priority (ε=0.58, tangled rope) institutionalizes deferral of emissions reductions while requiring immediate adaptation investment from vulnerable regions. Mitigation_priority (structurally distinct ε, stored separately) emphasizes decarbonization without growth constraints. Degrowth_transformation (structurally distinct ε, stored separately) requires foundational economic restructuring. The readings coexist institutionally as competing policy frameworks held by different political coalitions, not as variations of a single true response.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(climate_response_legitimacy__adaptation_priority, institutional, 0.22).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
