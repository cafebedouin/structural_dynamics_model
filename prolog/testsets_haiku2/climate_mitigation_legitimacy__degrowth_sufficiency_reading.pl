% ============================================================================
% CONSTRAINT STORY: climate_mitigation_legitimacy__degrowth_sufficiency_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_climate_mitigation_legitimacy__degrowth_sufficiency_reading, []).

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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:measurement_basis/2,
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
 *   constraint_id: climate_mitigation_legitimacy__degrowth_sufficiency_reading
 *   human_readable: Decarbonization-via-Degrowth Legitimacy Claim
 *   domain: energy_policy/climate_mitigation/technology_governance
 *
 * SUMMARY:
 *   This constraint instantiates the degrowth-sufficiency reading of the
 *   contested climate-mitigation-legitimacy kernel. The reading asserts that
 *   climate stabilization's defining requirement is absolute demand
 *   reduction, rendering large-scale generation expansion (whether fossil,
 *   nuclear, or renewable) illegitimate as a climate strategy because it
 *   enables continued high-energy growth. This reading conflicts structurally
 *   with the baseload-necessity and renewable-primacy readings, which
 *   position generation switching as primary, and differs from
 *   portfolio-pragmatism, which treats technology-agnostic expansion as
 *   viable. The constraint's extraction emerges from the broad delegitimacy
 *   applied to incumbent energy systems (all technologies), workers dependent
 *   on expansion pathways, and energy-access expansion in the global south.
 *   Suppression increases sharply in the early interval (t=0 to t=15) as the
 *   reading gains institutional adoption in climate-policy spaces, then
 *   plateaus as regulatory regimes crystallize around it. Theater ratio rises
 *   steadily: demand-reduction advocacy becomes increasingly performative as
 *   political actors adopt the frame while scaling renewable expansion
 *   simultaneously, creating a gap between stated sufficiency and actual
 *   energy-system operation.
 *
 * KEY AGENTS:
 *   - Climate stabilization advocates (agenda-setters): set the frame that demand reduction is necessary, making large-scale generation expansion illegitimate
 *   - Fossil fuel operators (payers): face phase-out, expected and aligned with all readings
 *   - Nuclear generation capital (payers): face delegitimation under this reading because expansion is framed as growth-enabling; this differs from other readings
 *   - Renewable equipment manufacturers (payer/beneficiary duality): benefit from renewable legitimacy but face extraction when framed as expansion vendors rather than demand-reduction enablers
 *   - Incumbent energy-demand sectors (payers, identity-locked): industries like data centers and heavy manufacturing face delegitimation as structurally incompatible with climate stabilization
 *   - Global south development actors (excluded): face foreclosure of energy-access expansion; the frame is imposed by high-consumption-history agents
 *   - Energy workers (excluded, trapped payers): face job loss from both fossil phase-out and the rejection of renewable-expansion employment pathways
 *   - Energy system planners (observers): assess feasibility; often find demand reduction necessary but not sufficient, creating epistemic tension with the reading
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_mitigation_legitimacy__degrowth_sufficiency_reading, 0.68).
domain_priors:suppression_score(climate_mitigation_legitimacy__degrowth_sufficiency_reading, 0.71).
domain_priors:theater_ratio(climate_mitigation_legitimacy__degrowth_sufficiency_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__degrowth_sufficiency_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__degrowth_sufficiency_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__degrowth_sufficiency_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__degrowth_sufficiency_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__degrowth_sufficiency_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_mitigation_legitimacy__degrowth_sufficiency_reading, tangled_rope).
narrative_ontology:human_readable(climate_mitigation_legitimacy__degrowth_sufficiency_reading, "Decarbonization-via-Degrowth Legitimacy Claim").
narrative_ontology:topic_domain(climate_mitigation_legitimacy__degrowth_sufficiency_reading, "energy_policy/climate_mitigation/technology_governance").

domain_priors:requires_active_enforcement(climate_mitigation_legitimacy__degrowth_sufficiency_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_mitigation_legitimacy__degrowth_sufficiency_reading, '9b4548a2-4114-4ad9-9e8f-c9fe9f21c243').
narrative_ontology:cs_kernel_codification('9b4548a2-4114-4ad9-9e8f-c9fe9f21c243', formalized).
narrative_ontology:cs_authority_grounding('9b4548a2-4114-4ad9-9e8f-c9fe9f21c243', distributed).
narrative_ontology:cs_reading_relation('9b4548a2-4114-4ad9-9e8f-c9fe9f21c243', climate_mitigation_legitimacy__baseload_necessity_reading, coexists_with).
narrative_ontology:cs_reading_relation('9b4548a2-4114-4ad9-9e8f-c9fe9f21c243', climate_mitigation_legitimacy__renewable_primacy_reading, coexists_with).
narrative_ontology:cs_reading_relation('9b4548a2-4114-4ad9-9e8f-c9fe9f21c243', climate_mitigation_legitimacy__portfolio_pragmatism_reading, coexists_with).
narrative_ontology:cs_axiom('9b4548a2-4114-4ad9-9e8f-c9fe9f21c243', foundational, demand_reduction_necessary_decarbonization).
narrative_ontology:cs_axiom_status(demand_reduction_necessary_decarbonization, holdable).
narrative_ontology:cs_axiom_grounding('9b4548a2-4114-4ad9-9e8f-c9fe9f21c243', demand_reduction_necessary_decarbonization, empirically_contingent).
narrative_ontology:cs_axiom('9b4548a2-4114-4ad9-9e8f-c9fe9f21c243', foundational, generation_expansion_enables_false_decarbonization).
narrative_ontology:cs_axiom_status(generation_expansion_enables_false_decarbonization, holdable).
narrative_ontology:cs_axiom_grounding('9b4548a2-4114-4ad9-9e8f-c9fe9f21c243', generation_expansion_enables_false_decarbonization, empirically_contingent).
narrative_ontology:cs_reference_frame('9b4548a2-4114-4ad9-9e8f-c9fe9f21c243', energy_demand_decoupling_framework).
narrative_ontology:cs_drift_state('9b4548a2-4114-4ad9-9e8f-c9fe9f21c243', contemporary_net_zero_policy_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('9b4548a2-4114-4ad9-9e8f-c9fe9f21c243', '').
narrative_ontology:cs_kernel_id(climate_mitigation_legitimacy__degrowth_sufficiency_reading, climate_mitigation_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_mitigation_legitimacy__degrowth_sufficiency_reading, climate_stabilization_advocates).
narrative_ontology:constraint_beneficiary(climate_mitigation_legitimacy__degrowth_sufficiency_reading, renewable_energy_deployment_advocates).
narrative_ontology:constraint_beneficiary(climate_mitigation_legitimacy__degrowth_sufficiency_reading, degrowth_theorists).
narrative_ontology:constraint_victim(climate_mitigation_legitimacy__degrowth_sufficiency_reading, fossil_fuel_energy_operators).
narrative_ontology:constraint_victim(climate_mitigation_legitimacy__degrowth_sufficiency_reading, nuclear_generation_capital).
narrative_ontology:constraint_victim(climate_mitigation_legitimacy__degrowth_sufficiency_reading, renewable_equipment_manufacturers).
narrative_ontology:constraint_victim(climate_mitigation_legitimacy__degrowth_sufficiency_reading, incumbent_energy_demand_sectors).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(climate_mitigation_legitimacy__degrowth_sufficiency_reading, renewable_equipment_manufacturers).
narrative_ontology:constraint_beneficiary(climate_mitigation_legitimacy__degrowth_sufficiency_reading, grid_operators_and_utilities).
narrative_ontology:constraint_victim(climate_mitigation_legitimacy__degrowth_sufficiency_reading, grid_operators_and_utilities).
narrative_ontology:constraint_victim(climate_mitigation_legitimacy__degrowth_sufficiency_reading, workers_in_extraction_and_generation).
narrative_ontology:constraint_vindicates(climate_mitigation_legitimacy__degrowth_sufficiency_reading, climate_emergency_nonlinear_tipping).
narrative_ontology:constraint_vindicates(climate_mitigation_legitimacy__degrowth_sufficiency_reading, demand_reduction_technical_feasibility).
narrative_ontology:constraint_vindicates(climate_mitigation_legitimacy__degrowth_sufficiency_reading, steady_state_economic_viability).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Holds that decarbonization's core requirement is absolute demand reduction (through efficiency, consumption norms, and behavioral change), making large-scale generation expansion (nuclear, renewable, or either) structurally counterproductive because it permits continued high-energy-intensity development. Sets the frame that legitimacy flows from degrowth alignment, not generation adequacy. Operates through policy advocacy, research institutions, and NGO networks. Cannot exit the climate advocacy posture without institutional death.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__degrowth_sufficiency_reading, climate_stabilization_advocates, agenda_setter,
    organized, civilizational, constrained, global).

% Face mandated phase-out under this reading's legitimacy frame. Their exit costs are existential (stranded assets, workforce obsolescence, regulatory exclusion). They contest the degrowth premise by arguing demand will persist and only generation switching matters, not demand reduction.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__degrowth_sufficiency_reading, fossil_fuel_energy_operators, payer,
    institutional, biographical, constrained, global).

% Nuclear operators and capital-equipment manufacturers face delegitimation under this reading: large-scale nuclear deployment is framed as growth-enabling and thus incompatible with degrowth. Their situation differs from fossil operators in that they would accept a decarbonized frame—but only if it privileges generation expansion as necessary. Under the degrowth reading, their expansion plans are recategorized as the problem rather than the solution.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__degrowth_sufficiency_reading, nuclear_generation_capital, payer,
    institutional, generational, constrained, global).

% Occupy a dual position: the degrowth reading legitimizes renewable deployment as carbon-free energy source, BUT only when coupled to demand reduction, not growth-serving expansion. A solar manufacturer extracting rents from a 3x generation expansion while energy demand remains constant would be read as complicit in false decarbonization. Their escape route is repositioning as demand-reduction enablers (efficiency equipment, grid-demand management) rather than generation-expansion vendors.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__degrowth_sufficiency_reading, renewable_equipment_manufacturers, payer,
    institutional, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(climate_mitigation_legitimacy__degrowth_sufficiency_reading, renewable_equipment_manufacturers, beneficiary).

% High-energy-intensity industries (data centers, heavy manufacturing, aviation, fertilizer production) face the core extraction under this reading: their operational models are delegitimized as inherently incompatible with climate stabilization. They cannot exit this constraint without operationally transforming their businesses, which many regard as equivalent to cease-to-exist. Their identity as industrial actors is fused to energy-intensive production.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__degrowth_sufficiency_reading, incumbent_energy_demand_sectors, payer,
    powerful, biographical, identity_locked, global).

% Benefit from the legitimacy frame's elevation of demand-side management and grid intelligence as the central coordination problem. They can position as neutral efficiency coordinators rather than growth servants. But they also face cost: if demand reduction removes peak-load pressures, their capital deployment models and revenue streams shift, requiring regulatory redesign.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__degrowth_sufficiency_reading, grid_operators_and_utilities, beneficiary,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(climate_mitigation_legitimacy__degrowth_sufficiency_reading, grid_operators_and_utilities, payer).

% Face job loss and community economic collapse under both fossil phase-out AND under the degrowth frame's rejection of large-scale renewable expansion (which would otherwise offer employment transition pathways). The degrowth reading provides no new employment model for them; they are structurally payers who cannot renegotiate the constraint. Unions and regional authorities voice their absence from the climate-mitigation conversation.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__degrowth_sufficiency_reading, workers_in_extraction_and_generation, payer,
    powerless, biographical, trapped, regional).
narrative_ontology:stakeholder_secondary_role(climate_mitigation_legitimacy__degrowth_sufficiency_reading, workers_in_extraction_and_generation, excluded).

% The degrowth reading privileges low energy demand as the climate-legitimate posture, which forecloses energy-access expansion in developing economies that have historically lower per-capita consumption but rising demand from poverty reduction and electrification. They are excluded from setting the legitimate path forward; the reading is imposed by high-consumption-history agents in wealthy nations framing their own demand reduction as the model.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__degrowth_sufficiency_reading, global_south_development_actors, excluded,
    powerless, generational, trapped, global).

% Assess decarbonization feasibility under different assumptions. They observe that the degrowth reading constrains the solution space by making generation-side flexibility a second-order variable. Their technical models often show demand reduction + renewable expansion + flexible generation as co-optimizing, whereas the reading privileges demand reduction alone. They provide evidence that shapes whether other stakeholders are convinced.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__degrowth_sufficiency_reading, energy_system_planners_and_engineers, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(climate_mitigation_legitimacy__degrowth_sufficiency_reading, climate_stabilization_advocates).
narrative_ontology:fixing_cost_class(climate_mitigation_legitimacy__degrowth_sufficiency_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Orients climate-mitigation legitimacy toward demand-reduction-centered strategy, coordinating research, policy advocacy, and institutional positioning around the claim that decarbonization cannot succeed while energy demand remains growth-coupled or energy-intensity unchanged.
% TRANSFER_FUNCTION: Transfers delegitimacy from fossil operators (expected, desired) to nuclear operators and large-scale renewable-deployment capital (unexpected within other climate-mitigation readings) and to high-energy-intensity incumbent industries, by framing their operations as incompatible with climate stabilization. Collects legitimacy for demand-reduction research, sufficiency-economics advocacy, and grid-management innovation.
% ABSENT_VOICES: Workers dependent on fossil, nuclear, and renewable-energy capital deployment; global-development actors and energy-access advocates in low-consumption regions; energy-system engineers and economists who find demand reduction necessary but insufficient; communities dependent on energy-intensive manufacturing and agricultural modernization.
% DISAPPEARANCE_RATIONALE: If this degrowth-sufficiency frame vanished, energy policy would immediately revert to generation-switching as the primary mitigation lever (baseload-necessary or renewable-primary frames), which would restore capital deployment pathways for all generation technologies and incumbent-sector energy models. The global-south development actors would gain standing in climate conversations; workers could transition to renewable expansion jobs. But climate advocates argue that absence of the degrowth frame permits false decarbonization—high-generation-capacity renewable deployment enabling continued growth, which fails to stabilize atmospheric CO2 on the required timescale.
% FOUNDING_PROBLEM: The biophysical climate system exhibits nonlinear tipping points that make decarbonization timelines extremely aggressive (net-zero by 2050 or sooner); energy-demand growth extends the technology switching timeline beyond the tipping-point window, making demand reduction structurally necessary, not optional, if emissions are to stabilize before runaway feedback loops.
% FOUNDING_PROBLEM_CORROBORATION: Climate scientists and IPCC assessments attest that demand reduction features in all 1.5C scenarios (corroboration from outside the degrowth-advocate community). Energy modelers and some climate economists note that demand reduction appears in scenarios but is often implemented via technological efficiency and behavior change rather than absolute consumption reduction (contested corroboration—the problem statement is accepted, the degrowth interpretation is not universal). Fossil-fuel operators, nuclear advocates, and renewable-equipment manufacturers contest both the problem urgency and the sufficiency claim, arguing that generation switching at scale can achieve net-zero compatible with continued energy availability.
narrative_ontology:disappearance_verdict(climate_mitigation_legitimacy__degrowth_sufficiency_reading, contested).
narrative_ontology:founding_problem_status(climate_mitigation_legitimacy__degrowth_sufficiency_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(climate_mitigation_legitimacy__degrowth_sufficiency_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(climate_mitigation_legitimacy__degrowth_sufficiency_reading, 'none', 1).
narrative_ontology:epsilon_provenance(climate_mitigation_legitimacy__degrowth_sufficiency_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(climate_mitigation_legitimacy__degrowth_sufficiency_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(climate_mitigation_legitimacy__degrowth_sufficiency_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(climate_mitigation_legitimacy__degrowth_sufficiency_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises from 0.48 to a plateau at 0.68 because the reading's institutional adoption accelerates until regulatory regimes align with it (t=0 to t=20), then stabilizes as the frame becomes institutionalized. The rise reflects increasing breadth of delegitimacy applied to expansion-dependent actors. Suppression follows a similar trajectory: enforcement of demand-reduction frames (building codes, carbon budgets, energy-intensity targets, renewable subsidies paired with nuclear restrictions) rises as the reading consolidates. Theater ratio climbs from 0.25 to 0.42 and plateaus because the gap between stated sufficiency (demand reduction is necessary and sufficient for decarbonization) and actual practice (renewable expansion continues, energy demand in practice remains coupled to growth in most sectors, degrowth targets are unmet) widens as the frame becomes official policy. One shared time grid is used; all measurements are authored at each time point, enabling temporal analysis of the decoupling between rhetoric and operation.
 *
 * PERSPECTIVAL GAP:
 *   The baseload-necessity reading would compute an inverted directionality for nuclear capital (d near beneficiary end, full legitimacy) and a victim role for renewable manufacturers (who would be framed as perpetuating intermittency, a problem to be solved). The renewable-primacy reading would compute fossil and nuclear as full targets, renewables and demand-flexibility as beneficiaries. Portfolio-pragmatism would compute all generation technologies as beneficiaries and treat expansion as legitimate. The engine computes each seat's type from the structural data of this reading alone; the divergence across readings reveals how legitimacy framing migrates extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   The constraint exhibits strong directionality asymmetry across stakeholder seats. Climate-stabilization advocates occupy the d≈0.0 beneficiary end: they collect legitimacy, agenda-setting authority, and policy priority without bearing the extraction costs. Fossil operators sit near d=1.0 as full targets: they pay through existential asset devaluation and regulatory exclusion. Nuclear capital and renewable manufacturers occupy a contested middle: they appear as beneficiaries in other readings (nuclear in baseload-necessity; renewables in renewable-primacy) but face partial extraction here because their expansion is delegitimized as growth-serving. Incumbent energy-demand sectors approach d=1.0 because the reading attacks their operational model as fundamentally incompatible with climate legitimacy. The duality in renewable-equipment manufacturers' stakeholder role reflects this constraint-reading dependency: in renewable-primacy they would be full beneficiaries; here they are split.
 *
 * MANDATROPHY ANALYSIS:
 *   The degrowth-sufficiency reading addresses the mandatrophy question: does the climate-mitigation mandate (stabilize atmospheric CO2 to limit warming to 1.5–2.0°C) require demand reduction to succeed, or only technology switching? The reading answers affirmatively, embedding demand reduction into the legitimacy criterion itself. But the core mandatrophy risk is visible in the theater-ratio plateau: the reading's stated mandate (decarbonization via degrowth) becomes uncoupled from actual operation (continued energy-demand growth with renewable expansion) because the political economy of degrowth lacks enforcement mechanisms outside wealthy-nation policy spaces. Demand reduction appears in regulatory targets and carbon budgets but remains unmet in practice, making the theatrical ratio the binding constraint: the constraint persists not because demand is actually reduced, but because the frame legitimizes demand-reduction advocacy while leaving energy-system operation largely unchanged. If the theater ratio continues rising while extractiveness plateaus, the constraint is becoming a pure performance mechanism—the founding mandate (demand-coupled decarbonization) is dead, but the constraint persists through institutional inertia.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    demand_reduction_feasibility,
    'Is demand reduction (absolute, not efficiency-coupled to growth) technically and politically feasible at the scale and speed required by 1.5°C scenarios, or is it a boundary condition that operates at the margin while generation switching remains primary?',
    'Empirical tracking of energy-demand trajectories in high-income economies adopting the degrowth frame versus control jurisdictions; assessment of whether demand reduction tracks policy or continues growth-coupled despite regulatory framing.',
    'If demand reduction proves infeasible at scale, the degrowth-sufficiency reading becomes unsustainable as a legitimacy mechanism; expansion-based readings (baseload-necessity, portfolio-pragmatism) would regain legitimacy. If feasible, the reading''s structural position is reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(demand_reduction_feasibility, empirical, 'Whether demand reduction is a feasible primary lever or a marginal contribution to decarbonization.').

omega_variable(
    global_south_development_legitimacy,
    'Does the degrowth-sufficiency reading apply equally to energy-access expansion in low-income regions, or does it permit differentiated development trajectories (high-income degrowth, low-income growth-compatible decarbonization)?',
    'Reading-community discourse: do advocates of degrowth-sufficiency universalize the frame or carve exceptions for development actors? Do global-south actors accept the frame or contest it as neo-colonial constraint?',
    'If universalized, the reading forecloses energy-access expansion globally and intensifies extraction on global-south actors; the reading becomes radically redistributive but faces political collapse. If differentiated, the reading applies primarily to high-income incumbent systems, narrowing its scope and reducing its extraction footprint in the global south.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(global_south_development_legitimacy, conceptual, 'Whether degrowth-sufficiency is a universal climate mandate or a high-income-specific redistributive reading.').

omega_variable(
    expansion_vs_grid_transformation,
    'Can grid transformation (storage, flexibility, demand-side management, smart distribution) achieve decarbonization with current or modestly reduced demand, or does demand reduction become necessary because grid transformation alone is insufficient?',
    'Long-term energy modeling comparing generation-switching-only scenarios against demand-reduction-coupled scenarios across different grid architectures and storage assumptions; empirical data from high-renewable-penetration grids (Denmark, Costa Rica, South Australia) on feasibility without demand reduction.',
    'If grid transformation proves sufficient, demand reduction becomes an optional efficiency lever rather than a structural necessity, and the degrowth-sufficiency reading loses its core legitimacy claim; renewable-primacy and portfolio-pragmatism regain ground. If transformation is insufficient, demand reduction becomes integral, and the degrowth reading''s structural position is confirmed.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(expansion_vs_grid_transformation, empirical, 'Whether decarbonization without demand reduction is technically feasible with advanced grid systems.').

omega_variable(
    renewable_expansion_growth_coupling,
    'When renewable energy is deployed at scale, does it enable continued high-energy-intensity growth, or does it fundamentally alter energy-service delivery toward lower absolute demand?',
    'Empirical observation: regions with high renewable deployment (Denmark, Germany, Costa Rica, Uruguay) have tracked energy demand and energy intensity; do they show demand reduction or demand stabilization with renewable substitution?',
    'If renewable expansion enables continued growth (observed in most cases), the degrowth reading''s claim that generation switching is insufficient becomes empirically supported. If renewable expansion correlates with absolute demand reduction, the reading''s position is weakened and renewable-primacy regains legitimacy as a sufficient frame.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(renewable_expansion_growth_coupling, empirical, 'Whether renewable deployment achieves decarbonization via demand reduction or via growth-coupled substitution.').

omega_variable(
    worker_transition_pathways_absent_from_reading,
    'Does the degrowth-sufficiency reading embed a viable employment and income-security model for workers dependent on fossil extraction and large-scale energy deployment, or is the reading essentially a transition mechanism that displaces workers without compensating them?',
    'Policy development and implementation data: do jurisdictions adopting degrowth frames couple them with guaranteed employment transition, wage floors, and community investment, or do they impose demand reduction without these mechanisms?',
    'If pathways are absent or inadequate, the reading becomes unsustainable politically; worker resistance and exclusion from the frame will grow, reducing suppression effectiveness. If robust pathways are embedded, the reading''s legitimacy broadens and extraction can be absorbed through institutional redistribution.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(worker_transition_pathways_absent_from_reading, preference, 'Whether the degrowth-sufficiency reading includes just-transition mechanisms for displaced energy workers or operates as pure extraction.').

omega_variable(
    reading_forecast_accuracy_divergence,
    'This reading is one of four competing readings of the climate-mitigation-legitimacy kernel. Which reading''s forecast—about decarbonization feasibility, required technology deployment, and timeline adequacy—will prove most accurate as climate outcomes and energy-system trajectories accumulate data?',
    'Retrospective comparison at t=2035–2040: actual global emissions trajectories, renewable deployment rates, nuclear deployment rates, energy-demand trajectories, and climate outcomes against projections made by each reading''s adherent community in 2025–2026.',
    'The reading whose forecasts prove most accurate gains legitimacy and institutional momentum; alternative readings lose credibility. This is the meta-level selection mechanism by which one reading eventually dominates or persists as minority dissent.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_forecast_accuracy_divergence, empirical, 'Which of the four kernel readings has the most accurate forecast about decarbonization feasibility and energy-system operation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_mitigation_legitimacy__degrowth_sufficiency_reading, 0, 35).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clim_tr_t0, climate_mitigation_legitimacy__degrowth_sufficiency_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement_basis(clim_tr_t0, observed).
narrative_ontology:measurement(clim_tr_t5, climate_mitigation_legitimacy__degrowth_sufficiency_reading, theater_ratio, 5, 0.3).
narrative_ontology:measurement_basis(clim_tr_t5, observed).
narrative_ontology:measurement(clim_tr_t10, climate_mitigation_legitimacy__degrowth_sufficiency_reading, theater_ratio, 10, 0.36).
narrative_ontology:measurement_basis(clim_tr_t10, observed).
narrative_ontology:measurement(clim_tr_t15, climate_mitigation_legitimacy__degrowth_sufficiency_reading, theater_ratio, 15, 0.4).
narrative_ontology:measurement_basis(clim_tr_t15, observed).
narrative_ontology:measurement(clim_tr_t20, climate_mitigation_legitimacy__degrowth_sufficiency_reading, theater_ratio, 20, 0.42).
narrative_ontology:measurement_basis(clim_tr_t20, observed).
narrative_ontology:measurement(clim_tr_t25, climate_mitigation_legitimacy__degrowth_sufficiency_reading, theater_ratio, 25, 0.42).
narrative_ontology:measurement_basis(clim_tr_t25, projected).
narrative_ontology:measurement(clim_tr_t30, climate_mitigation_legitimacy__degrowth_sufficiency_reading, theater_ratio, 30, 0.42).
narrative_ontology:measurement_basis(clim_tr_t30, projected).
narrative_ontology:measurement(clim_tr_t35, climate_mitigation_legitimacy__degrowth_sufficiency_reading, theater_ratio, 35, 0.42).
narrative_ontology:measurement_basis(clim_tr_t35, projected).

% Extraction over time
narrative_ontology:measurement(clim_be_t0, climate_mitigation_legitimacy__degrowth_sufficiency_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement_basis(clim_be_t0, observed).
narrative_ontology:measurement(clim_be_t5, climate_mitigation_legitimacy__degrowth_sufficiency_reading, base_extractiveness, 5, 0.54).
narrative_ontology:measurement_basis(clim_be_t5, observed).
narrative_ontology:measurement(clim_be_t10, climate_mitigation_legitimacy__degrowth_sufficiency_reading, base_extractiveness, 10, 0.61).
narrative_ontology:measurement_basis(clim_be_t10, observed).
narrative_ontology:measurement(clim_be_t15, climate_mitigation_legitimacy__degrowth_sufficiency_reading, base_extractiveness, 15, 0.65).
narrative_ontology:measurement_basis(clim_be_t15, observed).
narrative_ontology:measurement(clim_be_t20, climate_mitigation_legitimacy__degrowth_sufficiency_reading, base_extractiveness, 20, 0.67).
narrative_ontology:measurement_basis(clim_be_t20, observed).
narrative_ontology:measurement(clim_be_t25, climate_mitigation_legitimacy__degrowth_sufficiency_reading, base_extractiveness, 25, 0.68).
narrative_ontology:measurement_basis(clim_be_t25, projected).
narrative_ontology:measurement(clim_be_t30, climate_mitigation_legitimacy__degrowth_sufficiency_reading, base_extractiveness, 30, 0.68).
narrative_ontology:measurement_basis(clim_be_t30, projected).
narrative_ontology:measurement(clim_be_t35, climate_mitigation_legitimacy__degrowth_sufficiency_reading, base_extractiveness, 35, 0.68).
narrative_ontology:measurement_basis(clim_be_t35, projected).

% Suppression requirement over time
narrative_ontology:measurement(clim_su_t0, climate_mitigation_legitimacy__degrowth_sufficiency_reading, suppression_requirement, 0, 0.48).
narrative_ontology:measurement_basis(clim_su_t0, observed).
narrative_ontology:measurement(clim_su_t5, climate_mitigation_legitimacy__degrowth_sufficiency_reading, suppression_requirement, 5, 0.56).
narrative_ontology:measurement_basis(clim_su_t5, observed).
narrative_ontology:measurement(clim_su_t10, climate_mitigation_legitimacy__degrowth_sufficiency_reading, suppression_requirement, 10, 0.64).
narrative_ontology:measurement_basis(clim_su_t10, observed).
narrative_ontology:measurement(clim_su_t15, climate_mitigation_legitimacy__degrowth_sufficiency_reading, suppression_requirement, 15, 0.68).
narrative_ontology:measurement_basis(clim_su_t15, observed).
narrative_ontology:measurement(clim_su_t20, climate_mitigation_legitimacy__degrowth_sufficiency_reading, suppression_requirement, 20, 0.7).
narrative_ontology:measurement_basis(clim_su_t20, observed).
narrative_ontology:measurement(clim_su_t25, climate_mitigation_legitimacy__degrowth_sufficiency_reading, suppression_requirement, 25, 0.71).
narrative_ontology:measurement_basis(clim_su_t25, projected).
narrative_ontology:measurement(clim_su_t30, climate_mitigation_legitimacy__degrowth_sufficiency_reading, suppression_requirement, 30, 0.71).
narrative_ontology:measurement_basis(clim_su_t30, projected).
narrative_ontology:measurement(clim_su_t35, climate_mitigation_legitimacy__degrowth_sufficiency_reading, suppression_requirement, 35, 0.71).
narrative_ontology:measurement_basis(clim_su_t35, projected).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_mitigation_legitimacy__degrowth_sufficiency_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(climate_mitigation_legitimacy__degrowth_sufficiency_reading, 0.12).
narrative_ontology:affects_constraint(climate_mitigation_legitimacy__degrowth_sufficiency_reading, climate_mitigation_legitimacy__baseload_necessity_reading).
narrative_ontology:affects_constraint(climate_mitigation_legitimacy__degrowth_sufficiency_reading, climate_mitigation_legitimacy__renewable_primacy_reading).
narrative_ontology:affects_constraint(climate_mitigation_legitimacy__degrowth_sufficiency_reading, climate_mitigation_legitimacy__portfolio_pragmatism_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of four readings of the climate_mitigation_legitimacy kernel. The kernel is the contested claim that orients how decarbonization legitimacy is assigned across energy technologies and expansion strategies. Each reading has a different ε (extractiveness), a different victim set, and a different theater ratio. The baseload-necessity and renewable-primacy readings treat generation expansion as necessary; this reading (degrowth-sufficiency) treats expansion as illegitimate. Portfolio-pragmatism treats expansion as technology-agnostic and viable. All four readings coexist as live positions held by different institutional actors in energy policy and climate governance. They are linked through network.affects_constraints to enable contamination and coupling analysis across the kernel family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(climate_mitigation_legitimacy__degrowth_sufficiency_reading, institutional, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
