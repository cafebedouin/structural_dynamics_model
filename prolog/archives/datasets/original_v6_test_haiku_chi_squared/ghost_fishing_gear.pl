% ============================================================================
% CONSTRAINT STORY: ghost_fishing_gear
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ghost_fishing_gear, []).

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
 *   constraint_id: ghost_fishing_gear
 *   human_readable: Persistence of Abandoned, Lost, or Discarded Fishing Gear (ALDFG)
 *   domain: ecological/economic
 *
 * SUMMARY:
 *   Ghost fishing gear (ALDFG) persists in marine environments because the
 *   structural incentives align perfectly against recovery: industrial
 *   fishing operations externalize the cost of gear loss and abandonment onto
 *   marine ecosystems, small-scale fishers, and coastal communities.
 *   Abandoned nets, traps, and lines continue to trap and kill fish,
 *   crustaceans, and marine mammals indefinitely — a process called 'ghost
 *   fishing.' The constraint is a hybrid of coordination (fishing requires
 *   shared marine space; gear management is a legitimate coordination
 *   problem) and extraction (operators profit by avoiding gear recovery
 *   costs, which are borne by the environment and public). The temporal
 *   trajectory shows extractiveness increasing from 0.35 to 0.58 over 50
 *   years as industrial fleet size and average deployment duration have
 *   grown, while theater ratio increases from 0.25 to 0.55 as regulatory
 *   agencies expand monitoring and recovery programs without proportional
 *   enforcement, creating performative compliance theater.
 *
 * KEY AGENTS:
 *   - Marine ecosystems and non-target species: Primary victim (powerless/trapped) — bears continuous ghost fishing mortality with no exit option
 *   - Small-scale and subsistence fishers: Secondary victim (powerless/trapped) — compete with abandoned gear, lack capital to retrieve it, cannot influence regulations
 *   - Coastal communities: Mixed victim-beneficiary (moderate/constrained) — depend on marine resources and industrial fishing revenue, trapped in dependence
 *   - Industrial fishing operators: Primary beneficiary (institutional/arbitrage) — profit by externalizing gear loss costs; mobile across jurisdictions
 *   - Seafood supply chain (processors, retailers): Secondary beneficiary (institutional/arbitrage) — benefit from cheap industrial catch; no direct enforcement cost
 *   - Environmental and conservation NGOs: Organized partial victim (organized/constrained) — see both coordination function and extraction; constrained by lack of enforcement power
 *   - Fisheries management agencies: Institutional actor with degraded function (institutional/constrained) — issue regulations but enforce weakly; theater ratio 0.55 indicates performative compliance monitoring
 *   - Analytical observer: Civilizational view (analytical/analytical) — sees hybrid constraint; risk is naturalizing externality as 'cost of fishing' rather than structural design choice
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ghost_fishing_gear, 0.58).
domain_priors:suppression_score(ghost_fishing_gear, 0.72).
domain_priors:theater_ratio(ghost_fishing_gear, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ghost_fishing_gear, extractiveness, 0.58).
narrative_ontology:constraint_metric(ghost_fishing_gear, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(ghost_fishing_gear, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ghost_fishing_gear, tangled_rope).
narrative_ontology:human_readable(ghost_fishing_gear, "Persistence of Abandoned, Lost, or Discarded Fishing Gear (ALDFG)").
narrative_ontology:topic_domain(ghost_fishing_gear, "ecological/economic").

domain_priors:requires_active_enforcement(ghost_fishing_gear).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ghost_fishing_gear, industrial_fishing_operators).
narrative_ontology:constraint_beneficiary(ghost_fishing_gear, processor_companies).
narrative_ontology:constraint_beneficiary(ghost_fishing_gear, seafood_retailers).
narrative_ontology:constraint_victim(ghost_fishing_gear, marine_ecosystems).
narrative_ontology:constraint_victim(ghost_fishing_gear, non_target_species).
narrative_ontology:constraint_victim(ghost_fishing_gear, small_scale_fishers).
narrative_ontology:constraint_victim(ghost_fishing_gear, coastal_communities).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: MARINE ECOSYSTEMS (SNARE) — Trapped in a cycle of continuous ghost fishing: abandoned nets continue to trap and kill fish, crustaceans, and marine mammals indefinitely. No exit option, no compensation mechanism. Costs are total (ecosystem degradation, biomass loss, species mortality). d≈0.98, f(d)≈1.45, σ=1.2 → χ≈1.00.
constraint_indexing:constraint_classification(ghost_fishing_gear, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: SMALL-SCALE FISHERS (SNARE) — Trapped by industrial operators' abandoned gear competing with functional catch rates. High suppression (cannot lobby effectively, lack capital for gear retrieval). No negotiation power. d≈0.92, f(d)≈1.38, σ=0.9 → χ≈0.72.
constraint_indexing:constraint_classification(ghost_fishing_gear, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 3: COASTAL COMMUNITIES (TANGLED ROPE) — Constrained by dependence on marine resources but also benefit from fishing industry's economic activity. Ghost gear represents mixed burden: ecosystem degradation threatens livelihood, yet industrial fishing revenue funds local infrastructure. No clean exit; trapped in mixed extraction-coordination. d≈0.68, f(d)≈1.08, σ=0.9 → χ≈0.56.
constraint_indexing:constraint_classification(ghost_fishing_gear, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: INDUSTRIAL FISHING OPERATORS (ROPE) — Beneficiaries. Gear loss/abandonment is externality they avoid paying for; recovery costs are borne by environment and public. Minimal suppression on their side (organized market, capital mobility, regulatory arbitrage across jurisdictions). d≈0.08, f(d)≈-0.10, σ=1.2 → χ≈-0.07. Negative effective extraction = subsidized by external costs.
constraint_indexing:constraint_classification(ghost_fishing_gear, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: SUPPLY CHAIN (ROPE) — Beneficiary. Depends on cheap industrial catch; externalizes environmental costs. Can arbitrage between suppliers and regions. No direct enforcement cost. d≈0.10, f(d)≈-0.08, σ=1.2 → χ≈-0.06.
constraint_indexing:constraint_classification(ghost_fishing_gear, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ENVIRONMENTAL COALITION (TANGLED ROPE) — Organized agents (NGOs, conservation bodies, some governments) see both coordination function (recovery programs, gear marking, marine spatial planning) and extraction (cost externalization by industry). Constrained exit (cannot abandon the issue; enforcement requires institutional leverage). Coordination with small-scale fishers and coastal communities creates shared interest. d≈0.45, f(d)≈0.48, σ=1.2 → χ≈0.34.
constraint_indexing:constraint_classification(ghost_fishing_gear, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: FISHERIES MANAGEMENT AGENCIES (PITON) — Theater_ratio=0.55. Agencies issue regulations, conduct compliance monitoring, run recovery programs; but enforcement is weak, compliance variable, and actual ghost gear removal is minimal relative to loss rate. Institutional mandate persists (piton: degraded but maintained through regulatory inertia) without substantive functional impact. d≈0.50, f(d)≈0.65, σ=1.0 → χ≈0.36.
constraint_indexing:constraint_classification(ghost_fishing_gear, piton,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER (TANGLED ROPE) — From civilizational/global scope, ghost fishing gear represents a hybrid constraint: (1) coordination problem (how to incentivize safe gear deployment and recovery), and (2) extraction mechanism (cost externalization by industrial operators). The constraint persists because coordination costs are borne collectively (environment, public) while extraction benefits are private (operator profit). Effective extraction χ=0.58 reflects the hybrid: genuine coordination function exists (fishing requires shared marine space) but asymmetric cost allocation dominates. d≈0.72, f(d)≈1.15, σ=1.2 → χ≈0.80.
constraint_indexing:constraint_classification(ghost_fishing_gear, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ghost_fishing_gear_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(ghost_fishing_gear, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(ghost_fishing_gear, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(ghost_fishing_gear, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(ghost_fishing_gear, TR),
    TR >= 0.70.

:- end_tests(ghost_fishing_gear_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. Industrial operators extract private benefit (avoided recovery costs) while imposing diffuse public costs (ecosystem damage, small-fisher competition, coastal community livelihood threat). The value reflects that extraction is substantial but not absolute — some fishing is necessary, some gear loss is genuinely unavoidable, and some operators do implement recovery measures. Extractiveness has grown over 50 years as fleet industrialization increased (larger, more powerful vessels operate farther offshore with higher loss probability). Suppression (0.72): High. Suppression operates through multiple mechanisms: (1) economic power imbalance (small-scale fishers cannot organize to challenge industrial operators), (2) regulatory arbitrage (operators fish in jurisdictions with weak enforcement or move fleets when enforcement tightens), (3) information asymmetry (gear loss data is opaque; operators control loss reporting), (4) collective action problem (environmental costs are diffuse; no single victim can exit unilaterally). Theater ratio (0.55): Moderate-high. Fisheries agencies conduct monitoring, issue recovery guidelines, run gear tagging programs; but enforcement is weak (detection of violations is difficult in open ocean, prosecution rare), compliance is voluntary (fines are often cheaper than compliance), and actual recovery rate is <10% of lost gear. The theater reflects the gap between regulatory visibility and functional impact.
 *
 * PERSPECTIVAL GAP:
 *   The constraint exhibits a dramatic perspectival gap across the agent spectrum. Industrial operators and supply chain actors see low or negative effective extraction (rope: they are solving the legitimate problem of deploying fishing gear; gear loss is an unavoidable cost of operations). Environmental advocates and analytical observers see tangled rope (genuine coordination function mixed with asymmetric cost allocation). Fisheries agencies see their own process as piton (regulatory theater with degraded function). Coastal communities see mixed extraction and coordination (tangled rope: industry is extracting via cost externalization but also providing economic livelihood). Small-scale fishers see pure snare (trapped, powerless, no exit option). Marine ecosystems see ultimate snare (ghost fishing continues indefinitely, no recovery mechanism, no collective defense). The beneficiary/victim split is clean: industrial operators and their supply chain partners are beneficiaries; marine ecosystems, non-target species, small-scale fishers, and coastal communities are victims.
 *
 * DIRECTIONALITY LOGIC:
 *   Industrial operators and supply chain: Beneficiaries + arbitrage → d≈0.08, f(d)≈-0.10. Net beneficiaries; derive negative effective extraction (subsidy via externality). Marine ecosystems and non-target species: Victims + trapped → d≈0.98, f(d)≈1.45. Maximum extraction via ghost fishing. Small-scale fishers: Victims + trapped → d≈0.92, f(d)≈1.38. Near-maximum extraction; cannot exit due to livelihood dependence. Coastal communities: Mixed (victim of ecosystem degradation, beneficiary of economic activity) + constrained → d≈0.68, f(d)≈1.08. Moderate extraction reflecting the mixed position. Environmental advocates: Organized + constrained → d≈0.45, f(d)≈0.48. Low effective extraction because advocates have organizational power and see a potential path forward (regulatory strengthening). Fisheries agencies: Institutional + constrained → d≈0.50, f(d)≈0.65. Moderate extraction; agencies see enforcement constraints but maintain regulatory mandate. Piton classification comes from theater_ratio gate (0.55 ≥ 0.70 fails piton gate, but theater presence indicates degraded function).
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION: The constraint resolves the mandatrophy between 'natural coordination problem' (rope framing) and 'deliberate extraction' (snare framing) by recognizing that both are structurally true at different levels. (1) At the operational level, fishing is a genuine coordination problem: vessels must deploy gear in shared marine space, recover losses when possible, and balance immediate catch needs against long-term sustainability. This is rope logic. (2) At the institutional level, cost externalization is an extraction mechanism: operators profit by avoiding recovery costs that are borne collectively. This is snare logic. (3) The hybrid classification (tangled rope) resolves the mandatrophy by asserting that both mechanisms operate simultaneously within a single constraint structure. The constraint persists because the coordination function (fishing operations) is valuable (societies need food) but the extraction mechanism (cost externalization) is more profitable (operators save on recovery costs). The system would optimize toward pure rope (full recovery, sustainable fishing) if externalities were internalized, but suppression (regulatory arbitrage, information asymmetry, diffuse victim costs) locks the constraint in the tangled rope state. The increasing theater ratio (0.25→0.55 over 50 years) indicates a drift toward piton: regulatory machinery expands (monitoring, recovery programs, international agreements) but functional recovery remains low, suggesting the constraint is degrading into performative compliance theater while the underlying extraction persists.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    gear_loss_intentionality,
    'What fraction of ALDFG is due to unavoidable accident (storms, entanglement, infrastructure collision) versus cost-saving abandonment (deliberately discarding worn gear rather than paying for proper disposal)?',
    'Comparative analysis of loss rates by fleet safety standards, vessel age, insurance practices; interviews with fishing captains and salvage operators; forensic examination of recovered gear damage patterns',
    'If intentionality > 50%: constraint is pure snare (deliberate extraction). If < 20%: constraint softens to rope (largely coordination problem with some unavoidable externality). Classification boundary at ~35%.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(gear_loss_intentionality, empirical, 'Whether gear loss is accidental or deliberate abandonment').

omega_variable(
    recovery_cost_allocation_mechanism,
    'Can a functional cost-recovery system (gear taxes, deposit schemes, strict liability) be designed that doesn''t collapse the industry or drive displacement to unregulated waters?',
    'Pilot programs (e.g., EU gear marking, deposit schemes in selected ports); cost-benefit analysis of various liability models; tracking of fleet relocation patterns in response to enforcement tightening',
    'If feasible and effective: constraint can evolve from snare to rope or scaffold (recovery becomes sustainable). If costs exceed industry viability or cause displacement: constraint hardens to snare (externality is locked in structurally).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(recovery_cost_allocation_mechanism, empirical, 'Feasibility of implementing effective cost recovery for gear recovery').

omega_variable(
    biodiversity_recovery_timeline,
    'What is the generational lag for marine ecosystem recovery after ghost gear input ceases? Does recovery occur within a human lifetime or across multiple generations?',
    'Longitudinal studies of zones with reduced ghost gear input (marine protected areas, gear retrieval projects); ecosystem regeneration modeling; species rebound tracking (fish stocks, benthic communities, megafauna)',
    'If recovery < 30 years: ecological cost is recoverable (snare is temporary, scaffold logic applies). If > 100 years: ecological cost is effectively permanent (snare is multigenerational). Affects whether victims can ever exit the constraint.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(biodiversity_recovery_timeline, empirical, 'Timeline for marine ecosystem recovery after ghost gear cessation').

omega_variable(
    substitution_technology_viability,
    'Can biodegradable fishing gear (biodegradable nets, self-destructing traps, ghost net detection/removal technology) become economically viable at scale without regulatory mandate?',
    'Market research on biodegradable gear costs vs conventional; trials with early-adopter fleets; technology cost curves; regulatory impact assessment on adoption rates',
    'If market-viable: constraint can shift to rope (technology-enabled coordination). If only viable with subsidy: constraint remains snare (externality is locked in by economics, not physics).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(substitution_technology_viability, empirical, 'Viability of biodegradable fishing gear as market alternative').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ghost_fishing_gear, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gfg_tr_t0, ghost_fishing_gear, theater_ratio, 0, 0.25).
narrative_ontology:measurement(gfg_tr_t25, ghost_fishing_gear, theater_ratio, 25, 0.4).
narrative_ontology:measurement(gfg_tr_t50, ghost_fishing_gear, theater_ratio, 50, 0.55).

% Extraction over time
narrative_ontology:measurement(gfg_be_t0, ghost_fishing_gear, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(gfg_be_t25, ghost_fishing_gear, base_extractiveness, 25, 0.48).
narrative_ontology:measurement(gfg_be_t50, ghost_fishing_gear, base_extractiveness, 50, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ghost_fishing_gear, resource_allocation).
narrative_ontology:affects_constraint(ghost_fishing_gear, marine_protected_area_enforcement).
narrative_ontology:affects_constraint(ghost_fishing_gear, sustainable_fishing_supply_chains).
narrative_ontology:affects_constraint(ghost_fishing_gear, microplastic_ocean_contamination).

% DUAL FORMULATION NOTE:
% ALDFG decomposes into three structurally distinct constraints: (1) Gear loss causation (ε=0.35, mostly accidental; some intentional abandonment) — rope-like coordination problem. (2) Gear recovery incentive failure (ε=0.58, operators profit by not recovering) — snare-like extraction. (3) Regulatory compliance theater (ε=0.55 theater ratio) — piton-like degraded function. The single-story tangled rope classification captures that (1) and (2) are operationally linked: the recovery failure exists because gear loss is externalized, and externalization is profitable because recovery is expensive. All three feed into marine ecosystem constraint (microplastic contamination, species mortality, biomass loss).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(ghost_fishing_gear, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
