% ============================================================================
% CONSTRAINT STORY: sotu_1974_nixon_energy_independence_infrastructure
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sotu_1974_nixon_energy_independence_infrastructure, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: sotu_1974_nixon_energy_independence_infrastructure
 *   human_readable: Energy Independence Infrastructure Development (1974 Nixon Initiative)
 *   domain: infrastructure/energy/geopolitics
 *
 * SUMMARY:
 *   President Nixon's 1974 State of the Union commitment to energy
 *   independence established a structural constraint requiring massive
 *   domestic infrastructure development to reduce reliance on foreign energy
 *   supplies. This constraint exhibits simultaneous coordination and
 *   extraction functions: the genuine problem being solved is geopolitical
 *   vulnerability to OPEC commodity leverage and Cold War energy security
 *   dependence; the extraction mechanism is the asymmetric distribution of
 *   infrastructure costs (environmental degradation, land displacement,
 *   transition risks) onto non-beneficiary populations. The constraint
 *   demonstrates how national security framing can authorize extraction by
 *   naturalizing it as unavoidable coordination. Over the subsequent 52-year
 *   interval (1974-2026), the extractiveness has increased (0.28 → 0.58) as
 *   sunk capital in fossil infrastructure accumulated, while theater ratio
 *   has risen (0.35 → 0.55) as the original geopolitical justification
 *   weakened and renewable alternatives matured. The constraint now persists
 *   substantially through institutional inertia — energy independence remains
 *   national policy despite the strategic context that justified it having
 *   substantially transformed.
 *
 * KEY AGENTS:
 *   - Domestic Energy Producers: Primary beneficiary (institutional/arbitrage) — direct capital investment, operational authority, sustained demand guarantee
 *   - Environmental Stewardship and Land Communities: Primary victim (powerless/trapped) — mandatory infrastructure development, mining operations, extraction authorization, no exit options
 *   - Energy-Dependent Industry: Secondary beneficiary (moderate/constrained) — benefits from supply security and price stability; also bears transition costs and infrastructure risk
 *   - National Security Apparatus: Institutional beneficiary (institutional/constrained) — achieves geopolitical resilience; also constrained by long-term technology obsolescence and public investment requirements
 *   - Fossil Fuel Transition Movement: Organized critic (organized/mobile) — recognizes constraint as degraded institutional mechanism; has mobile exit options through alternative energy advocacy
 *   - Energy Policy Reform Coalition: Organized alternative provider (organized/mobile) — building renewable/distributed pathways with visible sunset logic; can transition infrastructure authorization toward sustainability
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sotu_1974_nixon_energy_independence_infrastructure, 0.52).
domain_priors:suppression_score(sotu_1974_nixon_energy_independence_infrastructure, 0.65).
domain_priors:theater_ratio(sotu_1974_nixon_energy_independence_infrastructure, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sotu_1974_nixon_energy_independence_infrastructure, extractiveness, 0.52).
narrative_ontology:constraint_metric(sotu_1974_nixon_energy_independence_infrastructure, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(sotu_1974_nixon_energy_independence_infrastructure, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sotu_1974_nixon_energy_independence_infrastructure, tangled_rope).
narrative_ontology:human_readable(sotu_1974_nixon_energy_independence_infrastructure, "Energy Independence Infrastructure Development (1974 Nixon Initiative)").
narrative_ontology:topic_domain(sotu_1974_nixon_energy_independence_infrastructure, "infrastructure/energy/geopolitics").

domain_priors:requires_active_enforcement(sotu_1974_nixon_energy_independence_infrastructure).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sotu_1974_nixon_energy_independence_infrastructure, domestic_energy_producers).
narrative_ontology:constraint_beneficiary(sotu_1974_nixon_energy_independence_infrastructure, energy_dependent_industry).
narrative_ontology:constraint_beneficiary(sotu_1974_nixon_energy_independence_infrastructure, national_security_apparatus).
narrative_ontology:constraint_victim(sotu_1974_nixon_energy_independence_infrastructure, environmental_stewardship).
narrative_ontology:constraint_victim(sotu_1974_nixon_energy_independence_infrastructure, competing_land_use_communities).
narrative_ontology:constraint_victim(sotu_1974_nixon_energy_independence_infrastructure, fossil_fuel_transition_movements).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ENVIRONMENTAL STEWARDSHIP / LAND COMMUNITIES (SNARE) — Communities facing mandatory infrastructure development, mining operations, and extraction authorization on their lands cannot exit. Bears full extraction cost: habitat loss, water contamination, health externalities, disrupted land use. No alternatives within national energy independence framework. Maximum suppression — federal permitting overrides local opposition; national security framing forecloses negotiation. State power + trapped exit = pure extraction from this perspective.
constraint_indexing:constraint_classification(sotu_1974_nixon_energy_independence_infrastructure, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: ENERGY-DEPENDENT INDUSTRY (TANGLED ROPE) — Manufacturing, transportation, heating sectors benefit from supply security and price stability coordination achieved through domestic capacity development. Also constrained by infrastructure costs, transition risks, and supply volatility during buildout. Mixed position: genuine coordination benefit (industrial capacity maintained through stable energy supply) alongside asymmetric extraction (labor, capital, and transition risks concentrated on specific regions and sectors). Active enforcement required to maintain supply chains. Moderate power enables some negotiation; constrained exit reflects high cost of energy system change.
constraint_indexing:constraint_classification(sotu_1974_nixon_energy_independence_infrastructure, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: DOMESTIC ENERGY PRODUCERS (ROPE) — Direct beneficiary of infrastructure development mandate and capital investment. Experiences constraint as pure coordination: federal policy creates market certainty, capital flows, and operational authority. Can arbitrage between energy commodity markets and policy support. Primary beneficiary position with institutional power and arbitrage exit capacity = rope classification. Extraction flows toward this agent; they perceive the system as enabling rather than constraining.
constraint_indexing:constraint_classification(sotu_1974_nixon_energy_independence_infrastructure, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: NATIONAL SECURITY APPARATUS (TANGLED ROPE) — Institutional beneficiary: energy independence reduces foreign leverage and geopolitical vulnerability. Also bears coordination costs: infrastructure development requires sustained public investment, environmental liability management, and political capital expenditure. Constrained by long-term energy transition imperatives and technology obsolescence risks. Enforcement is active and authoritarian (federal override of local opposition, permitting acceleration). This perspective exhibits genuine coordination function (national resilience) alongside extraction (concentrated costs on non-strategic populations). Institutional power with constrained exit (cannot easily abandon energy infrastructure commitments) = tangled rope at generational horizon.
constraint_indexing:constraint_classification(sotu_1974_nixon_energy_independence_infrastructure, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: FOSSIL FUEL TRANSITION MOVEMENT (PITON) — Organized agents (environmental groups, alternative energy advocates, climate scientists) recognize the infrastructure constraint as a degraded institutional mechanism: the 1974 energy independence strategy was designed for a geopolitical context (OPEC embargo, Cold War) that has substantially transformed. The constraint persists through institutional inertia despite changing strategic reality (renewable technology maturity, climate imperatives, distributed energy feasibility). Theater ratio reflects this: much of the ongoing 'energy independence' discourse remains performative (patriotic energy nationalism, technological optimism) despite underlying strategic assumptions becoming obsolete. Organized agents have mobile exit options (can advocate alternative pathways, support technology transitions). The constraint maintains itself through narrative performance rather than functional necessity — piton signature.
constraint_indexing:constraint_classification(sotu_1974_nixon_energy_independence_infrastructure, piton,
    context(agent_power(organized),
            time_horizon(civilizational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: ENERGY POLICY REFORM COALITION (SCAFFOLD) — Organized agents (state renewable energy boards, municipal utilities, clean energy startups, progressive policy advocates) see the infrastructure constraint as a temporary coordination failure with visible sunset mechanism. Distributed generation, grid modernization, and renewable portfolio standards create alternative energy security pathways that bypass centralized infrastructure extraction. Low effective extraction because this coalition has agency, sees exit paths, and can transition infrastructure authorization toward sustainability criteria. Sunset logic: as renewable capacity and grid storage mature, the rationale for large-scale fossil fuel infrastructure authorization diminishes — estimated 15-25 year horizon for norms transition in energy policy. Constraint maintains coercive form through inertia but functional necessity declines.
constraint_indexing:constraint_classification(sotu_1974_nixon_energy_independence_infrastructure, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational/universal perspective, some degree of energy infrastructure coordination is inherent to industrial economies: the technical requirement to convert resources into distributed energy is a structural feature of modern society. This perspective risks naturalizing the 1974 policy framework as an inevitable law of economics and physics. However, the structural data contradicts this — the constraint's extraction asymmetry, active enforcement requirements, and environmental externalities reveal it as a contingent institutional arrangement, not a natural law. False summit risk: the 'inherent to industrialism' framing naturalizes what is actually a specific policy choice about who bears transition costs.
constraint_indexing:constraint_classification(sotu_1974_nixon_energy_independence_infrastructure, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sotu_1974_nixon_energy_independence_infrastructure_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(sotu_1974_nixon_energy_independence_infrastructure, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(sotu_1974_nixon_energy_independence_infrastructure, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(sotu_1974_nixon_energy_independence_infrastructure, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(sotu_1974_nixon_energy_independence_infrastructure, TR),
    TR >= 0.70.

:- end_tests(sotu_1974_nixon_energy_independence_infrastructure_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high, rising. Initial extractiveness was lower (0.28 in 1974) when geopolitical urgency and technological uncertainty justified infrastructure development as genuine coordination. As renewable alternatives matured and geopolitical context shifted, the same infrastructure continued through path dependence and capital sunk cost, increasing the proportion of extraction relative to legitimate coordination. The trajectory shows extraction accumulation over time — the core mechanism (who bears costs) remained asymmetric, but the justification weakened. Suppression (0.65): High, reflecting multiple enforcement mechanisms: federal permitting authority overrides local opposition, national security framing forecloses negotiation, capital requirements concentrate decision-making in federal and corporate institutions, alternative energy pathways face regulatory barriers and incumbent opposition. These barriers are not absolute (hence not a mountain-level 0.85+) but sufficient to prevent meaningful exit for affected communities. Theater ratio (0.48 initial, 0.55 final): Moderate and rising. The 1974 constraint exhibited genuine functional necessity (energy security under OPEC embargo) — theater was lower. As geopolitical imperatives weakened and renewable capacity grew, more of the constraint's continued enforcement became performative: 'energy independence' rhetoric persists despite underlying strategic assumptions becoming obsolete. The rise in theater ratio indicates growing gap between stated justification and functional necessity.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates maximum perspectival heterogeneity. Environmental communities trapped in infrastructure development zones perceive pure extraction (snare) — no coordination benefit reaches them, only extraction costs. Domestic energy producers perceive pure coordination (rope) — the infrastructure development creates market certainty and capital flow. Energy-dependent industries perceive mixed coordination-extraction (tangled rope) — real supply security benefit alongside transition costs. National security apparatus perceives mixed benefit and constraint (tangled rope) — achieves geopolitical resilience but faces long-term obsolescence. Transition movement perceives degraded institutional mechanism (piton) — the constraint persists through inertia despite weakening strategic rationale. Reform coalition perceives temporary coordination failure with exit pathway (scaffold) — distributed renewable systems offer alternative security model with visible sunset. The analytical observer risks naturalizing the whole arrangement (mountain) — 'industrial societies must have infrastructure' — but the structural data reveals this as false summit: the extraction is contingent on specific policy choices about who bears costs, not inherent to industrialism itself. The perspectival gap demonstrates that the constraint's classification depends entirely on structural position; there is no neutral 'correct' answer.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values are derived from each agent's structural position relative to extraction flow. Domestic energy producers are direct beneficiaries with institutional power and arbitrage exit options — they experience negative effective extraction (benefit flow). Environmental stewardship faces maximum targeting (d ≈ 0.95) due to powerless position + trapped exit + victim status — high f(d) produces maximum experienced extraction. Energy-dependent industry occupies intermediate position: they benefit from supply coordination but bear transition costs; moderate power + constrained exit + mixed beneficiary/victim status produces d ≈ 0.55. National security apparatus benefits from infrastructure but is constrained by long-term obsolescence; institutional power + constrained exit + beneficiary status produces d ≈ 0.35. The piton perspective (transition movement) has mobile exit options and organized power — even though they bear some costs, their ability to advocate alternatives and transition away reduces effective d. The scaffold perspective (reform coalition) has the lowest d among critics because they have highest agency (organized power, mobile exit, clear alternative pathway). This directionality gradient explains the perspectival gap: powerless trapped communities experience snare (maximum χ); organized mobile agents experience piton or scaffold (low χ); beneficiaries experience rope (negative χ).
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLVED through perspectival heterogeneity: The constraint is simultaneously rope (for beneficiaries), tangled rope (for dependent industries and security apparatus), snare (for trapped communities), piton (for transition movement), scaffold (for reform coalition), and false-summit mountain (for naive analytical observer). The mandate to choose a single type dissolves when the analytic framework recognizes that different agents, with different structural positions, genuinely experience different constraint types. The 1974 energy independence infrastructure is rope from the perspective of those who benefit from market certainty and capital investment. It is snare from the perspective of communities facing mandatory development with no exit. It is tangled rope from the perspective of those who benefit but also bear coordination costs. It is piton from the perspective of those recognizing its functional obsolescence. It is scaffold from the perspective of those building alternative pathways. No single classification is 'wrong' — they are all structurally correct readings of the same constraint from different positions. The mandate (choose one type) is resolved by recognizing the presheaf of perspectives as the actual answer. The constraint IS the structure of divergent experience — the indexical tuple encodes what each agent actually perceives, and the different types they produce are features, not bugs, of the framework.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    energy_security_definition_boundary,
    'What constitutes genuine energy security — reduction in foreign commodity dependence, price stability, supply continuity, resilience to supply disruption, or decentralized generation capacity?',
    'Historical analysis of 1973-1974 OPEC embargo impacts vs subsequent energy policy outcomes; comparative analysis of different security frameworks (centralized fossil vs distributed renewable) under disruption scenarios',
    'If security = foreign commodity reduction: centralized fossil infrastructure is functional solution. If security = supply resilience: distributed renewable systems with storage provide better structural resilience. Different security framings change the classification of the constraint — from tangled rope (centralization with coordination benefit) to snare (if centralization creates new dependencies without genuine security gain).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(energy_security_definition_boundary, conceptual, 'Definition of energy security determines constraint''s coordination vs extraction ratio').

omega_variable(
    externality_internalization_lag,
    'Do environmental and health externalities from fossil fuel infrastructure constitute part of the extraction cost, or are they external to the constraint''s economic model?',
    'Life-cycle assessment of infrastructure development costs including environmental remediation; longitudinal health impact studies of communities hosting energy infrastructure; cost-benefit analysis incorporating externality capitalization',
    'If externalities are structural costs: suppression and extraction values rise substantially (0.65 → 0.80+). Constraint moves from tangled rope toward snare from all non-beneficiary perspectives. If externalities are treated as separate problem: current suppression/extraction values hold, but violate the structural principle that extraction includes all asymmetric costs borne by non-beneficiaries.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(externality_internalization_lag, empirical, 'Whether environmental externalities are internal to constraint''s extraction accounting').

omega_variable(
    geopolitical_context_obsolescence,
    'How much of the 1974 energy independence framework''s continued enforcement is driven by persistent geopolitical vulnerability vs institutional inertia and sunk capital commitments?',
    'Comparison of stated energy security imperatives (OPEC leverage, Cold War fuel security) vs current geopolitical risk profile; analysis of renewable/distributed capacity growth trajectories and cost curves; policy discourse analysis showing shifting rationales over 1974-2026 period',
    'If primarily geopolitical: tangled rope classification holds — genuine coordination need justifies extraction costs. If primarily inertial: piton classification strengthens — constraint is performative theater maintaining institutional legitimacy of infrastructure already built. Structural evidence points toward increasing inertia component, suggesting movement toward piton over generational horizon.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(geopolitical_context_obsolescence, empirical, 'Degree to which energy independence framework reflects current vs obsolete geopolitical imperatives').

omega_variable(
    coordination_function_substitutability,
    'Can renewable/distributed energy systems provide equivalent coordination benefits (supply certainty, price stability, industrial capacity maintenance) as centralized fossil infrastructure, and at what timeline?',
    'Grid stability analysis under high renewable penetration; battery storage and demand management capacity projections; comparative cost analysis of renewable vs fossil infrastructure over 20-50 year horizons; pilot programs demonstrating distributed system coordination',
    'If renewables can substitute coordination: scaffold perspective is structural — genuine exit path exists on 15-25 year timeline. Constraint moves toward piton as functional necessity declines. If renewables cannot yet substitute: tangled rope remains primary classification; energy independence extraction continues until alternatives mature. Current evidence suggests partial substitutability approaching (2026); full substitution projected 2040-2050.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_function_substitutability, empirical, 'Whether renewable systems can functionally replace fossil infrastructure coordination benefits').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sotu_1974_nixon_energy_independence_infrastructure, 0, 52).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sotu1974_tr_t0, sotu_1974_nixon_energy_independence_infrastructure, theater_ratio, 0, 0.35).
narrative_ontology:measurement(sotu1974_tr_t13, sotu_1974_nixon_energy_independence_infrastructure, theater_ratio, 13, 0.42).
narrative_ontology:measurement(sotu1974_tr_t26, sotu_1974_nixon_energy_independence_infrastructure, theater_ratio, 26, 0.48).
narrative_ontology:measurement(sotu1974_tr_t52, sotu_1974_nixon_energy_independence_infrastructure, theater_ratio, 52, 0.55).

% Extraction over time
narrative_ontology:measurement(sotu1974_be_t0, sotu_1974_nixon_energy_independence_infrastructure, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(sotu1974_be_t13, sotu_1974_nixon_energy_independence_infrastructure, base_extractiveness, 13, 0.4).
narrative_ontology:measurement(sotu1974_be_t26, sotu_1974_nixon_energy_independence_infrastructure, base_extractiveness, 26, 0.52).
narrative_ontology:measurement(sotu1974_be_t52, sotu_1974_nixon_energy_independence_infrastructure, base_extractiveness, 52, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sotu_1974_nixon_energy_independence_infrastructure, global_infrastructure).
narrative_ontology:affects_constraint(sotu_1974_nixon_energy_independence_infrastructure, domestic_oil_production_capacity).
narrative_ontology:affects_constraint(sotu_1974_nixon_energy_independence_infrastructure, natural_gas_distribution_infrastructure).
narrative_ontology:affects_constraint(sotu_1974_nixon_energy_independence_infrastructure, nuclear_power_expansion).
narrative_ontology:affects_constraint(sotu_1974_nixon_energy_independence_infrastructure, clean_energy_transition_barrier).
narrative_ontology:affects_constraint(sotu_1974_nixon_energy_independence_infrastructure, geopolitical_resource_dependence).

% DUAL FORMULATION NOTE:
% Energy independence infrastructure is upstream of specific extraction technologies (oil drilling, natural gas, nuclear) but represents a distinct structural constraint. The infrastructure development constraint has ε ≈ 0.52 (mixed coordination-extraction at national scale); individual technology constraints have varying ε values reflecting technological maturity, environmental impact, and competitive dynamics. The shared beneficiaries (domestic producers, security apparatus) and victims (environmental communities, transition stakeholders) link the constraint family. Decomposition follows domain lines: infrastructure authorization (this story) vs. specific technology implementation (downstream stories).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(sotu_1974_nixon_energy_independence_infrastructure, institutional, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
