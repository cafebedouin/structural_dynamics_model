% ============================================================================
% CONSTRAINT STORY: alpine_infrastructure_adaptation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_alpine_infrastructure_adaptation, []).

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
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: alpine_infrastructure_adaptation
 *   human_readable: Alpine Infrastructure Adaptation Under Climate Change
 *   domain: environmental_policy/infrastructure_planning
 *
 * SUMMARY:
 *   Alpine infrastructure adaptation under climate change creates a
 *   structural constraint that exhibits the characteristics of tangled rope
 *   coordination-extraction hybridity. Alpine regions face accelerating
 *   climate impacts (glacier loss, altered precipitation patterns, increased
 *   avalanche risk) that require coordinated, cross-border infrastructure
 *   investment and water resource management. Genuine coordination problems
 *   exist: water is shared across mountain valleys and national borders,
 *   climate risks affect multiple communities simultaneously, and technical
 *   solutions (improved monitoring, shared reservoirs, integrated
 *   forecasting) require basin-wide cooperation. Simultaneously, the
 *   constraint exhibits substantial asymmetric extraction: adaptation costs
 *   are concentrated in alpine communities (infrastructure investment,
 *   agricultural disruption, population displacement risk) while benefits
 *   flow primarily to lowland urban centers and hydroelectric operators
 *   (water security, power generation, economic growth). This dual structure
 *   — real coordination function alongside real extraction mechanism —
 *   defines the tangled rope. The constraint's theater ratio (0.48) is
 *   moderate because actual infrastructure work is occurring (dams being
 *   built, water allocation systems being deployed, climate monitoring
 *   improving) but substantial theater surrounds international coordination
 *   efforts (Alpine Convention committees, transnational assessments,
 *   summits) that lack enforcement power and functional decision-making
 *   authority. The extractiveness trajectory (0.35 → 0.58 over 20 years)
 *   reflects accumulating costs as climate impacts intensify and adaptation
 *   burden becomes clearer.
 *
 * KEY AGENTS:
 *   - Subsistence Alpine Farmers: Primary victims (powerless/trapped) — bear costs of crop failure, water scarcity, and infrastructure disruption with no exit; no representation in regional water allocation decisions
 *   - Alpine Community Coalition: Organized agents (organized/constrained) — regional advocacy networks, local governments, environmental organizations; can mobilize politically but face structural dependence on infrastructure and fragmentation across borders
 *   - Hydroelectric Power Operator: Primary beneficiary (institutional/arbitrage) — captures value from coordinated water management; has full exit options (investment alternatives, geographic mobility, policy negotiation capacity)
 *   - National Water Authority: Powerful institutional actor (powerful/mobile) — controls basin-wide allocation decisions; sees both genuine coordination problem and opportunity to exercise asymmetric power
 *   - International Alpine Convention: Institutional framework (institutional/arbitrage) — formally coordinates across nations but lacks enforcement mechanisms; persists through ceremonial function (piton perspective)
 *   - Climate Adaptation Investment Initiative: Organized temporary agent (organized/constrained) — mobilizes funding and technical support with explicit sunset; enables transition from international coordination to national funding responsibility
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing institutional choices (prioritizing lowland water security) as immutable physical constraints
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(alpine_infrastructure_adaptation, 0.58).
domain_priors:suppression_score(alpine_infrastructure_adaptation, 0.52).
domain_priors:theater_ratio(alpine_infrastructure_adaptation, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(alpine_infrastructure_adaptation, extractiveness, 0.58).
narrative_ontology:constraint_metric(alpine_infrastructure_adaptation, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(alpine_infrastructure_adaptation, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(alpine_infrastructure_adaptation, tangled_rope).
narrative_ontology:human_readable(alpine_infrastructure_adaptation, "Alpine Infrastructure Adaptation Under Climate Change").
narrative_ontology:topic_domain(alpine_infrastructure_adaptation, "environmental_policy/infrastructure_planning").

domain_priors:requires_active_enforcement(alpine_infrastructure_adaptation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(alpine_infrastructure_adaptation, hydroelectric_power_operators).
narrative_ontology:constraint_beneficiary(alpine_infrastructure_adaptation, low_elevation_urban_centers).
narrative_ontology:constraint_beneficiary(alpine_infrastructure_adaptation, wealthy_mountain_communities).
narrative_ontology:constraint_victim(alpine_infrastructure_adaptation, subsistence_alpine_farmers).
narrative_ontology:constraint_victim(alpine_infrastructure_adaptation, mountain_ecosystem_stability).
narrative_ontology:constraint_victim(alpine_infrastructure_adaptation, future_water_availability).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ALPINE FARMER (SNARE) — Subsistence farmers in alpine regions face accelerating climate volatility (increased precipitation variability, altered growing seasons, glacier loss affecting water supply) with no exit options. Cannot relocate without abandoning ancestral land and livelihood identity. Trapped within a regional constraint that offers no coordination benefit and maximum extraction: costs of adaptation are externalized to this agent (water scarcity, crop failure risk, infrastructure degradation) while infrastructure investment prioritizes hydroelectric capacity and lowland urban water security. No genuine coordination function exists for this agent's participation.
constraint_indexing:constraint_classification(alpine_infrastructure_adaptation, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: ALPINE COMMUNITY COALITION (TANGLED ROPE) — Regional and national alpine communities can organize through advocacy networks, environmental groups, and local governance structures. Face constraint as mixed: genuine coordination is needed (shared water resources, avalanche risk management, infrastructure resilience across borders) alongside asymmetric extraction (funding for infrastructure adaptation is contingent, adaptation costs are locally borne while benefits flow to lowland consumers). Exit is constrained by economic dependency on regional infrastructure and political fragmentation across alpine borders. High suppression: limited funding alternatives, political pressure to maintain hydroelectric capacity, and knowledge asymmetry regarding climate impacts.
constraint_indexing:constraint_classification(alpine_infrastructure_adaptation, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: HYDROELECTRIC POWER OPERATOR (ROPE) — Benefits from infrastructure adaptation: coordinating water management across alpine regions increases reservoir capacity, predictability, and power generation efficiency. Experiences the constraint as pure coordination: hydroelectric systems require basin-wide cooperation, dam maintenance networks, and integrated water allocation mechanisms. Exit is easy (arbitrage): can shift investment to other energy sources, negotiate supply agreements, or divest from alpine operations if coordination costs rise. Net beneficiary with full agency — experiences low effective extraction despite high base extractiveness because their structural position is one of power and mobility.
constraint_indexing:constraint_classification(alpine_infrastructure_adaptation, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: NATIONAL WATER AUTHORITY (TANGLED ROPE) — Coordinates basin-wide water allocation and infrastructure planning; genuine coordination function (allocating scarce water across competing demands, maintaining dam safety, managing seasonal flows). Also extracts value through centralized planning authority: controls investment priorities, determines allocation formulas, sets regional pricing. Mobile (can shift policy framework, negotiate with other nations, invest in alternatives) but moderately constrained by physical infrastructure lock-in and international treaties. Mixed experience: sees both real coordination problem and exercise of asymmetric power.
constraint_indexing:constraint_classification(alpine_infrastructure_adaptation, tangled_rope,
    context(agent_power(powerful),
            time_horizon(immediate),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: INTERNATIONAL ALPINE CONVENTION (PITON) — Established framework (Alpine Convention, international water treaties, EU Water Framework Directive) attempts to coordinate alpine resource management across multiple nations. Theater ratio is high: formal compliance protocols, transboundary committees, environmental impact assessments exist but lack enforcement mechanisms or funding to drive actual infrastructure transformation. Primary function (coordinating international cooperation) has substantially atrophied — most real decisions are made through bilateral state negotiations and hydroelectric companies' market power. Framework persists through institutional inertia and ceremonial status rather than functional necessity. Theater is maintained through reporting cycles and international summits while actual adaptation remains fragmented.
constraint_indexing:constraint_classification(alpine_infrastructure_adaptation, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: CLIMATE ADAPTATION INVESTMENT INITIATIVE (SCAFFOLD) — Temporary coordinated funding and technical support programs (Alpine Space Programme, climate adaptation funds, transnational research networks) provide resources and governance frameworks for adaptation with explicit sunset clauses: funding tied to near-term targets (2025-2035), sunsetting as national governments are expected to internalize adaptation costs. Low effective extraction because the constraint is framed as temporary and transitional; suppression is moderate (high-cost to participate but exit occurs after sunset date). Coordination function is real: mobilizes expertise, pools capital, standardizes best practices. As funding expires and national frameworks mature, the constraint's extraction mechanism loses force.
constraint_indexing:constraint_classification(alpine_infrastructure_adaptation, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (MOUNTAIN) — From a civilizational/universal perspective, alpine infrastructure adaptation reflects an immutable physical constraint: mountains have limited water storage capacity, seasonal melt patterns are determined by climatology and terrain, and the fundamental constraint (finite freshwater in alpine regions serving growing lowland demand) is a natural limit, not a contingent institutional arrangement. However, the structural data contradicts the mountain classification: who bears extraction costs, who benefits, and how alternative institutional arrangements could redistribute the burden are all socially determined. The 'natural limit' framing naturalizes what is actually an institutional choice: treating lowland urban water security as non-negotiable while externalizing costs to alpine communities is policy, not physics.
constraint_indexing:constraint_classification(alpine_infrastructure_adaptation, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(alpine_infrastructure_adaptation_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(alpine_infrastructure_adaptation, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(alpine_infrastructure_adaptation, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(alpine_infrastructure_adaptation, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(alpine_infrastructure_adaptation, TR),
    TR >= 0.70.

:- end_tests(alpine_infrastructure_adaptation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High but not extreme. Alpine communities bear concentrated costs (infrastructure investment, adaptation burden, livelihood disruption) while lowland beneficiaries receive concentrated benefits (water security, hydroelectric revenue, economic growth). The extractiveness value reflects sustained asymmetry in cost-benefit distribution across 20-year interval, with costs accelerating as climate impacts intensify. Suppression (0.52): Moderate-high. Alpine communities face significant barriers to resisting extraction: limited political power relative to lowland populations (voting demographics), economic dependency on regional infrastructure and national water systems, knowledge asymmetry regarding climate impacts and adaptation options, and fragmentation across multiple national jurisdictions. Suppression has not reached snare levels (≥0.60) because some agency exists: regional coalitions can organize, international frameworks provide access to adaptation funding, and media attention to climate justice can create political pressure. Theater ratio (0.48): Moderate. Substantial functional work occurs (dam construction, water allocation system redesign, climate monitoring infrastructure) but significant theater surrounds international coordination: Alpine Convention assessments, transnational committees, and climate summits generate extensive reporting and commitment-making with limited actual enforcement or resource redirection. Theater has remained relatively stable because the tension between functional work and procedural theater is structural, not time-dependent.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates maximal perspectival disagreement on classification type despite identical base properties. The alpine farmer sees extraction with no coordination benefit (snare) — the constraint offers them nothing except burden. The hydroelectric operator sees pure coordination (rope) — solving the collective problem of water management directly benefits them. The alpine coalition sees mixed coordination and extraction (tangled rope) — genuine problems to solve, but asymmetric power in solving them. The national water authority sees pure coordination (tangled rope, weighted toward rope aspects) — they manage basin-wide cooperation while maintaining policy authority. The international framework sees its own degraded function (piton) — ceremonial coordination persisting despite limited real decision-making authority. The climate adaptation initiative sees temporary support structure (scaffold) — real resources mobilized but with explicit sunset, creating transition pressure. The civilizational analyst risks seeing immutable constraint (mountain) — treating finite alpine water as natural limit rather than institutional choice. This perspectival spread reflects genuine structural ambiguity: is the constraint primarily about physical scarcity (mountain interpretation) or about institutional power asymmetry (snare/tangled rope interpretation)? The empirical evidence points toward institutional: alternative allocation rules, different investment priorities, or transnational cost-sharing could substantially reduce extraction without changing the underlying water availability.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values are derived from each agent's power level, exit options, and structural relationship to extraction flow. Subsistence farmers (powerless/trapped) experience maximum d (d ≈ 0.95): no exit options, victimized by allocation asymmetry, bearing concentrated costs. Alpine coalition (organized/constrained) experiences moderate-high d (d ≈ 0.60): can mobilize politically but constrained by economic dependency and fragmentation; victims and beneficiaries mixed. Hydroelectric operator (institutional/arbitrage) experiences low d (d ≈ 0.15): full exit options, receives benefits, no victimization. National water authority (powerful/mobile) experiences low-moderate d (d ≈ 0.40): constrained by existing infrastructure lock-in but has substantial policy discretion. International framework (institutional/arbitrage) experiences low d: ceremonial role, no actual victimization, can adjust commitment level. Climate initiative (organized/constrained) experiences moderate d (d ≈ 0.55): mobilizes resources but constrained by sunset clause and political dependence on national governments. The f(d) sigmoid translates these d values into experienced extractiveness multipliers: trapped agents experience amplified χ; mobile beneficiaries experience suppressed χ. Scope modifier σ(S) amplifies χ for national and global scope (1.0, 1.2) reflecting verification difficulty at larger scales.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandate of alpine infrastructure adaptation is to coordinate climate-resilient water resource management across regions and borders. The mandatrophy emerges because the same structural constraint can be framed as pure coordination (rope: all regions share water risk, cooperation benefits all) or asymmetric extraction (snare: lowland beneficiaries impose costs on alpine communities, mountain regions subsidize lowland security). The tangled rope classification resolves the mandatrophy by declaring that BOTH framings are structurally accurate: genuine coordination problems exist alongside genuine asymmetric extraction. The constraint cannot be solved as rope alone (ignoring the power asymmetry) nor as snare alone (denying the real coordination problem). The resolution mechanism is redistributive: alpine communities must receive compensation (technology transfer, adaptation funding, cost-sharing on infrastructure) proportional to their contribution and risk. Without this redistribution, the constraint devolves toward snare (powerless agents subsidize lowland beneficiaries indefinitely). With it, the constraint stabilizes as tangled rope with suppression declining as exit options improve through capacity-building and economic diversification. The piton perspective signals a real danger: international frameworks may become purely ceremonial theater (Alpine Convention as ritual coordination without enforcement) while actual decisions are made through bilateral state power. The scaffold perspective signals a real hope: time-limited adaptation investment can build sufficient capacity and institutional change (national funding responsibility, regional governance maturity) that alpine communities are no longer trapped.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    glacier_melt_timeline_ambiguity,
    'Do glacial melt projections represent an irreversible constraint (mountain) or a manageable adaptation challenge (rope/tangled rope)?',
    'Comparison of regional glacier loss projections across climate models; assessment of whether documented melt rates are consistent with model uncertainty or represent threshold crossing. Identification of adaptation capacity thresholds (water storage, infrastructure investment) relative to melt timeline.',
    'If irreversible: constraint approaches mountain character; extraction becomes rooted in scarcity rather than institutional choice. If manageable: constraint remains tangled rope; current extraction patterns reflect political power asymmetry rather than natural limit.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(glacier_melt_timeline_ambiguity, empirical, 'Whether glacier loss represents immutable physical constraint or manageable adaptation challenge').

omega_variable(
    transnational_coordination_capacity,
    'Can alpine nations achieve coordination on water management, or are national interests sufficiently misaligned to prevent collective adaptation?',
    'Historical analysis of transnational water agreement compliance; measurement of actual vs. negotiated water allocation; assessment of investment coordination across borders. Identification of commons dilemmas (prisoner''s dilemma patterns in water allocation or dam construction decisions).',
    'If coordination possible: tangled rope classification is accurate and transition to rope-dominant regime is viable. If coordination fails: constraint evolves toward snare (powerless agents bear costs of collective action failure) or piton (coordination frameworks become pure theater).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(transnational_coordination_capacity, empirical, 'Capacity for transnational coordination on alpine water management').

omega_variable(
    cost_allocation_legitimacy,
    'Should adaptation costs be borne by beneficiary regions (lowland water consumers) or by alpine communities where infrastructure is located?',
    'Comparative analysis of cost allocation in successful transnational adaptation (Rhine basin, Danube Commission); examination of willingness-to-pay studies for lowland water security; assessment of political pressure from alpine communities vs. lowland voting majorities.',
    'If costs should flow to beneficiaries: current extraction pattern is illegitimate; constraint requires redistributive policy overhaul. If costs should be shared proportionally: suppression and extraction metrics remain valid but require different legitimacy framing.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cost_allocation_legitimacy, preference, 'Legitimate allocation of adaptation costs across beneficiary vs. affected regions').

omega_variable(
    infrastructure_lock_in_reversibility,
    'Are existing hydroelectric installations (dams, reservoirs, diversion systems) irreversibly locked into lowland-focused water allocation, or can infrastructure be retrofitted to prioritize alpine community resilience?',
    'Engineering assessment of infrastructure modification costs relative to operational value; identification of infrastructure with greatest lock-in (irreversible once built, costly to modify). Measurement of political willingness to support retrofitting or retirement of existing infrastructure.',
    'If lock-in is near-total: constraint approaches mountain character; current extraction patterns are structurally embedded. If retrofitting is feasible: extraction is policy choice; tangled rope character suggests alternative institutional arrangements are possible.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(infrastructure_lock_in_reversibility, empirical, 'Reversibility of infrastructure lock-in into lowland-focused water allocation').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(alpine_infrastructure_adaptation, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(alpine_tr_t0, alpine_infrastructure_adaptation, theater_ratio, 0, 0.4).
narrative_ontology:measurement(alpine_tr_t10, alpine_infrastructure_adaptation, theater_ratio, 10, 0.45).
narrative_ontology:measurement(alpine_tr_t20, alpine_infrastructure_adaptation, theater_ratio, 20, 0.48).

% Extraction over time
narrative_ontology:measurement(alpine_be_t0, alpine_infrastructure_adaptation, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(alpine_be_t10, alpine_infrastructure_adaptation, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(alpine_be_t20, alpine_infrastructure_adaptation, base_extractiveness, 20, 0.58).
narrative_ontology:measurement(alpine_be_t5, alpine_infrastructure_adaptation, base_extractiveness, 5, 0.41).
narrative_ontology:measurement(alpine_be_t15, alpine_infrastructure_adaptation, base_extractiveness, 15, 0.54).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(alpine_infrastructure_adaptation, resource_allocation).
narrative_ontology:boltzmann_floor_override(alpine_infrastructure_adaptation, 0.18).
narrative_ontology:affects_constraint(alpine_infrastructure_adaptation, hydroelectric_energy_transition).
narrative_ontology:affects_constraint(alpine_infrastructure_adaptation, transnational_water_governance).
narrative_ontology:affects_constraint(alpine_infrastructure_adaptation, alpine_ecosystem_degradation).

% DUAL FORMULATION NOTE:
% Alpine infrastructure adaptation decomposes into three structurally distinct constraint stories: (1) Resource allocation coordination (this story) — managing shared water across competing demands, (2) Energy transition coupling — shifting away from hydroelectric dependence requires alternative power sources and storage, (3) Ecosystem degradation — glacier loss and hydrological changes that drive the adaptation need. Each has different ε and classification; they are linked because addressing one directly influences the others. This story (resource allocation) is upstream of energy transition (hydroelectric retirement decisions depend on water coordination framework) and affects ecosystem degradation (infrastructure decisions determine whether melt patterns are captured for power or allowed to support alpine ecology).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(alpine_infrastructure_adaptation, institutional, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
