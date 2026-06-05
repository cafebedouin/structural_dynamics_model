% ============================================================================
% CONSTRAINT STORY: aquifer_dependency_fragility
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_aquifer_dependency_fragility, []).

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
 *   constraint_id: aquifer_dependency_fragility
 *   human_readable: Aquifer Dependency Fragility: Asymmetric Extraction Through Hydrological Coordination
 *   domain: environmental/resource_management/water_security
 *
 * SUMMARY:
 *   Aquifer dependency creates a nested coordination-extraction problem:
 *   multiple user classes (agricultural, industrial, municipal, subsistence
 *   rural) share a finite groundwater resource governed by allocation rules
 *   that were designed for a different hydrological regime. The constraint
 *   exhibits the full Deferential Realism spectrum because it combines
 *   genuine coordination benefits (preventing unmanaged extraction chaos)
 *   with asymmetric extraction (allocation rules concentrate consumption
 *   among high-volume users while concentrating depletion costs among
 *   low-volume dependent communities). The same institutional apparatus that
 *   enables coordination also mechanisms extraction. The theater ratio (0.52)
 *   reflects that formal water management maintains legitimacy through
 *   scientific and legal ritual (hydrological surveys, allocation tribunals,
 *   environmental impact studies) while actual allocation decisions are
 *   increasingly decoupled from sustainability metrics. Over the 30-year
 *   interval, extractiveness has increased from 0.35 to 0.58 as extraction
 *   rates have moved further above recharge rates, and theater has increased
 *   as management institutions have expanded ceremonial activity (stakeholder
 *   consultations, sustainability certifications, environmental monitoring)
 *   without proportional change to extraction volumes.
 *
 * KEY AGENTS:
 *   - Subsistence Rural Communities: Primary victim (powerless/trapped) — geographically bound to single aquifer, no relocation option, no political voice in allocation decisions
 *   - High-Volume Agricultural Extractors: Primary beneficiary (powerful/arbitrage) — capture 70-80% of extraction; have alternatives (crop switching, alternative regions) but political incentives prevent switching
 *   - Industrial Water Users: Secondary beneficiary (powerful/arbitrage) — concentrated volume extraction; mobility through relocatable manufacturing
 *   - Water Authority / Government: Institutional beneficiary (institutional/arbitrage) — operates allocation framework; benefits from legitimacy and tax base stability
 *   - Environmental Advocacy Coalition: Organized victim (organized/constrained) — sees extraction but has limited enforcement power; organized enough to contest but not to override established allocation rules
 *   - Future Generations: Distributed victim (powerless/trapped) — not yet present; cannot participate in allocation decisions; bear full cost of depletion
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risk of naturalizing policy choice (current extraction rates) as physical limit (aquifer capacity)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(aquifer_dependency_fragility, 0.58).
domain_priors:suppression_score(aquifer_dependency_fragility, 0.68).
domain_priors:theater_ratio(aquifer_dependency_fragility, 0.52).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(aquifer_dependency_fragility, extractiveness, 0.58).
narrative_ontology:constraint_metric(aquifer_dependency_fragility, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(aquifer_dependency_fragility, theater_ratio, 0.52).

% --- Constraint claim ---
narrative_ontology:constraint_claim(aquifer_dependency_fragility, tangled_rope).
narrative_ontology:human_readable(aquifer_dependency_fragility, "Aquifer Dependency Fragility: Asymmetric Extraction Through Hydrological Coordination").
narrative_ontology:topic_domain(aquifer_dependency_fragility, "environmental/resource_management/water_security").

domain_priors:requires_active_enforcement(aquifer_dependency_fragility).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(aquifer_dependency_fragility, high_volume_agricultural_extractors).
narrative_ontology:constraint_beneficiary(aquifer_dependency_fragility, industrial_water_users).
narrative_ontology:constraint_beneficiary(aquifer_dependency_fragility, municipalities_with_junior_rights).
narrative_ontology:constraint_victim(aquifer_dependency_fragility, subsistence_rural_communities).
narrative_ontology:constraint_victim(aquifer_dependency_fragility, future_generations).
narrative_ontology:constraint_victim(aquifer_dependency_fragility, aquifer_ecosystem_integrity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SUBSISTENCE RURAL COMMUNITY (SNARE) — Trapped by geographic dependence on a single aquifer with no alternative water source. Cannot relocate without losing land, livelihood, and cultural continuity. Bears full extraction cost as industrial and agricultural users deplete the shared resource. Zero exit options; maximum experienced extraction.
constraint_indexing:constraint_classification(aquifer_dependency_fragility, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: REGIONAL AGRICULTURAL SECTOR (TANGLED ROPE) — Genuinely coordinates water allocation through irrigation infrastructure and seasonal scheduling. Simultaneously extracts by consuming 70-80% of aquifer discharge while bearing only proportional cost-sharing. Constrained by relocation costs and market dependencies, but not trapped. Mixed benefit-cost structure: coordination enables profitable farming; extraction enables unsustainable volume.
constraint_indexing:constraint_classification(aquifer_dependency_fragility, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: WATER AUTHORITY (ROPE) — Operates allocation framework that coordinates multiple user classes. Experiences the constraint as solving a collective action problem: allocating scarce water to maximize utility. Arbitrage options through inter-basin transfers, desalination contracts, or regulatory exemptions. Net beneficiary of the coordination function; sees extraction primarily as necessary cost-sharing mechanism.
constraint_indexing:constraint_classification(aquifer_dependency_fragility, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: ENVIRONMENTAL ADVOCACY COALITION (TANGLED ROPE) — Organized agents (NGOs, scientific bodies) see genuine coordination function (aquifer management prevents race-to-the-bottom extraction chaos) alongside clear asymmetric extraction (carrying costs that others externalise). Constrained by policy influence limits and institutional resistance, but organized enough to negotiate modification of allocation rules. Perceives both benefits (preventing total collapse) and extraction (unsustainable rates embedded in rules).
constraint_indexing:constraint_classification(aquifer_dependency_fragility, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 5: COLONIAL-ERA WATER RIGHTS DOCTRINE (PITON) — The legal framework allocating aquifer extraction is substantially performative: original-right doctrine based on historical usage patterns no longer reflects hydrological capacity or need distribution. The rules persist through institutional inertia despite degraded function (allocation authority cannot enforce sustainability without violating established rights). Theater ratio reflects maintenance of legitimacy through legal ritual rather than adaptive governance. The framework has become disconnected from its original purpose.
constraint_indexing:constraint_classification(aquifer_dependency_fragility, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / PHYSICAL LIMIT VIEW (MOUNTAIN) — From a long timescale perspective, aquifer depletion is an immutable physical constraint: extraction rates exceeding recharge rates are mathematically incompatible with perpetual supply. The constraint appears as natural law — hydrological balance is not negotiable. However, the base properties reveal this as false naturalization: the binding mechanism is institutional (extraction rates set by allocation rules, not physical necessity), not physical (recharge rate is not immutable, only current extraction pattern is). The mountain classification marks this as naturalization of a policy choice.
constraint_indexing:constraint_classification(aquifer_dependency_fragility, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(aquifer_dependency_fragility_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(aquifer_dependency_fragility, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(aquifer_dependency_fragility, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(aquifer_dependency_fragility, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(aquifer_dependency_fragility, TR),
    TR >= 0.70.

:- end_tests(aquifer_dependency_fragility_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderately high. The constraint exhibits clear asymmetric extraction: high-volume users consume 70-80% of aquifer discharge while bearing only proportional cost-sharing; subsistence users consume 5-10% but bear 40%+ of adaptation costs as water tables fall and well depths increase. The measurement is not total-system extraction (all allocation is within the aquifer's existing use pattern) but extraction differential — some agents capture disproportionate benefit relative to cost burden. The value reflects that extraction is significant but not complete predation (some coordination benefit exists, some cost-sharing occurs). Over the interval, extractiveness has risen as recharge-to-extraction ratio has declined, making the asymmetry sharper. Suppression (0.68): High. Subsistence rural communities face severe barriers to exit: no geographic mobility without abandoning land and livelihood; no alternative water source; limited political power to negotiate allocation changes; no capital for adaptation (deeper wells, water-efficient crops). Industrial and agricultural users have higher suppression via market structures (commodity prices, competition dynamics) that incentivize unsustainable extraction. Theater ratio (0.52): Moderate. Water management maintains legitimacy through scientific and legal apparatus (hydrological assessments, environmental reviews, allocation procedures) that have become increasingly performative as the gap between formal sustainability targets and actual extraction rates has widened. The constraint is not pure theater (genuine hydrological coordination occurs) but theater has increased over the interval as institutions have shifted from supply-expansion logic to demand-management rhetoric without shifting actual extraction approval patterns.
 *
 * PERSPECTIVAL GAP:
 *   This constraint shows maximal perspectival disagreement. The water authority sees rope — solving a collective action problem of shared resource allocation without mutual harm (institutional perspective, arbitrage exit, immediate time horizon). The agricultural sector sees rope or tangled_rope depending on whether they perceive themselves as extractive — most perceive coordination, some perceive asymmetry (powerful/constrained, biographical horizon). The subsistence community sees snare — they are trapped and experience pure extraction with no coordination benefit for themselves (powerless/trapped, biographical horizon). The environmental coalition sees tangled_rope — genuine coordination value (prevents total depletion chaos) but embedded in asymmetric extraction rules (organized/constrained, generational horizon). The piton perspective (institutional/arbitrage, civilizational horizon) observes that the legal and administrative apparatus maintaining allocation has become decoupled from its original function — it legitimates extraction rather than optimizing allocation. The mountain perspective (analytical, civilizational) risks naturalizing institutional choices (extraction rates set by allocation rules) as physical limits (aquifer capacity), obscuring that the constraint is primarily political rather than geophysical.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is derived from beneficiary/victim status plus exit options for each agent. Subsistence communities: victim status + trapped exit → d = 0.95 → maximum experienced extraction (f(d) ≈ 1.42). Agricultural extractors: beneficiary status + arbitrage exit → d = 0.10 → minimal extraction experienced (f(d) ≈ -0.02); they see the constraint as enabling their activity. Water authority: beneficiary status (legitimacy, revenue stability) + arbitrage exit (can shift allocation rules or subsidize alternatives) → d = 0.05 → negative extraction (coordination benefit). Environmental coalition: mixed position (victims of extraction, beneficiaries of coordination, organized with constrained exit) → d = 0.50 → moderate extraction (f(d) ≈ 0.65). The perspectival gap in directionality (0.05 to 0.95) reflects the asymmetric distribution of extraction: some agents experience subsidization, others experience predation, all from the same institutional structure.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by showing that tangled_rope is the correct type when viewed from the system level: the allocation apparatus genuinely coordinates water use (preventing chaos and enabling multiple uses) while also embedding asymmetric extraction (concentration of benefits among high-volume users, concentration of costs among low-volume dependent users and future generations). The error to avoid is classifying this as pure rope (coordination-only) because the coordination function is real and non-trivial. The error to avoid on the other side is classifying as pure snare because extraction exists — the constraint is not pure predation; it solves a real collective action problem. The tangled_rope classification captures both: active enforcement (allocation rules must be maintained and monitored), genuine coordination (prevents depletion chaos), and asymmetric extraction (benefits are concentrated, costs are dispersed). The piton perspective correctly identifies that the apparatus has become increasingly performative (theater ratio 0.52) but this does not downgrade to snare — the performance itself serves a function (maintaining belief in the allocation system's legitimacy), even as actual allocation becomes less responsive to sustainability targets. The mandatrophy is resolved by distinguishing between the coordination function (real, valuable) and the extraction asymmetry (real, costly), both of which are structural features of the constraint.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    recharge_rate_measurement_ambiguity,
    'Is the aquifer recharge rate measured as local precipitation infiltration only, or does it include inter-aquifer flow and paleowater reserves?',
    'Isotopic analysis of groundwater age and origin; hydrological modeling with explicit source tracking; independent validation against piezometric data',
    'If recharge excludes paleowater: extraction is already 300-500% of sustainable rate (snare classification confirmed universally). If recharge includes accessible reserves: extraction may be sustainable for 50-100 years at current rates (snare becomes piton — degraded system with functioning time limit). Classification uncertainty: ±0.15 on extractiveness.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(recharge_rate_measurement_ambiguity, empirical, 'Ambiguity in aquifer recharge rate measurement including or excluding paleowater').

omega_variable(
    agricultural_exit_feasibility,
    'Can the agricultural sector genuinely exit high-volume extraction through crop substitution, deficit irrigation, or relocation, or are these exit paths economically and politically infeasible?',
    'Cost analysis of irrigation technology shifts (drip vs flood), profitability comparison of high-water vs low-water crops in regional market, feasibility study of relocation to irrigated regions with senior water rights, political economy analysis of subsidy structures enabling current extraction',
    'If exit is feasible: agricultural power atom should upgrade from ''powerful'' to ''organized'' (can negotiate from strength). Exit options downgrade from ''arbitrage'' to ''constrained'' or ''mobile'' (exit is possible but costly). Directionality d increases from 0.48 toward 0.65. If exit is infeasible: current classification stands (agricultural sector trapped within low-margin high-water commodity economy).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(agricultural_exit_feasibility, empirical, 'Whether agricultural sector has feasible exit through crop substitution or relocation').

omega_variable(
    subsistence_community_coalition_capacity,
    'Can subsistence rural communities organize into a coalition with sufficient power to negotiate water-sharing agreements, or are organizational barriers (geographic dispersion, literacy, capital) insurmountable?',
    'Case study of existing water user associations in same region; analysis of coalition formation costs vs community asset base; historical precedent of successful community water governance in similar contexts',
    'If coalition is feasible: powerless agents upgrade to ''organized'' power atom (Dynamic Coalition extension). Classification shifts from snare to tangled_rope for this perspective. Experienced extraction (chi) declines significantly as organized agents have negotiation capacity. If infeasible: snare classification confirmed; powerless agents remain structurally locked.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(subsistence_community_coalition_capacity, empirical, 'Whether subsistence communities can achieve sufficient organizational capacity for coalition power').

omega_variable(
    allocation_rule_flexibility,
    'How much adjustment to water allocation rules can the political system tolerate without cascading coalition collapse among current beneficiaries?',
    'Political sensitivity analysis: model of allocation rule parameter space and which parameters are politically defended vs negotiable; stakeholder interview data on red-line positions; historical record of allocation disputes and resolution',
    'If flexibility is high (> 30% reallocation feasible): scaffold perspective gains credibility (sunset mechanism possible through phased rule change). If flexibility is low (< 10% reallocation possible): tangled_rope classification becomes the ceiling — even with political will, structural constraint persists. High flexibility suggests extraction is political choice; low flexibility suggests extraction is embedded in institutional architecture.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(allocation_rule_flexibility, conceptual, 'Political feasibility of adjusting water allocation rules').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(aquifer_dependency_fragility, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(aquif_tr_t0, aquifer_dependency_fragility, theater_ratio, 0, 0.38).
narrative_ontology:measurement(aquif_tr_t15, aquifer_dependency_fragility, theater_ratio, 15, 0.45).
narrative_ontology:measurement(aquif_tr_t30, aquifer_dependency_fragility, theater_ratio, 30, 0.52).

% Extraction over time
narrative_ontology:measurement(aquif_be_t0, aquifer_dependency_fragility, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(aquif_be_t15, aquifer_dependency_fragility, base_extractiveness, 15, 0.48).
narrative_ontology:measurement(aquif_be_t30, aquifer_dependency_fragility, base_extractiveness, 30, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(aquifer_dependency_fragility, resource_allocation).
narrative_ontology:boltzmann_floor_override(aquifer_dependency_fragility, 0.18).
narrative_ontology:affects_constraint(aquifer_dependency_fragility, groundwater_depletion_rate_accumulation).
narrative_ontology:affects_constraint(aquifer_dependency_fragility, agricultural_commodity_pricing_subsidy_lock).
narrative_ontology:affects_constraint(aquifer_dependency_fragility, rural_outmigration_demographic_decline).

% DUAL FORMULATION NOTE:
% The aquifer dependency constraint is composed of multiple structurally distinct sub-constraints with different epsilon values. The hydrological recharge-extraction coordination (managing seasonal flow) has lower extractiveness (~0.20, Rope). The long-term depletion extraction (depleting non-renewable reserves) has higher extractiveness (~0.72, Snare). The allocation rule asymmetry (concentrating benefits/costs) has moderate extractiveness (~0.58, Tangled Rope). This story represents the allocation asymmetry; decomposed stories would address recharge coordination (upstream) and depletion dynamics (downstream).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(aquifer_dependency_fragility, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
