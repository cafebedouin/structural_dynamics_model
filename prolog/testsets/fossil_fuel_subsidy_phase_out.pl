% ============================================================================
% CONSTRAINT STORY: fossil_fuel_subsidy_phase_out
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_fossil_fuel_subsidy_phase_out, []).

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
 *   constraint_id: fossil_fuel_subsidy_phase_out
 *   human_readable: Fossil Fuel Subsidy Phase-Out Constraint
 *   domain: economic_policy/energy_transition
 *
 * SUMMARY:
 *   The fossil fuel subsidy phase-out constraint represents a global energy
 *   transition mechanism designed to redirect capital flows from hydrocarbon
 *   extraction toward renewable energy infrastructure. This constraint
 *   exhibits classic tangled rope structure: genuine coordination function
 *   (preventing competitive subsidy races, stabilizing energy markets,
 *   enabling capital reallocation) coexists with asymmetric extraction
 *   (concentrated costs on workers and energy-poor populations, concentrated
 *   benefits among renewable investors and high-income consumers). The
 *   constraint is enforced through international agreements (Paris Agreement,
 *   G20 commitments), national legislation, and market mechanisms, yet
 *   effectiveness is contested: global fossil fuel subsidies increased from
 *   $412 billion (2010) to $7 trillion when including externalities (2022,
 *   IMF data). The theater_ratio trend shows declining performativity over
 *   the interval as actual phase-out commitments materialize in wealthy
 *   nations, though subsidy fungibility into developing markets suggests
 *   geographic redistribution rather than total elimination. The
 *   extractiveness increase reflects accumulating costs on trapped
 *   populations (coal workers, energy-poor) as renewable transition
 *   accelerates without proportional just transition implementation.
 *
 * KEY AGENTS:
 *   - Coal miners and fossil fuel workers: Primary victims (powerless/trapped) — bear direct employment and regional economic loss with uncertain transition pathways
 *   - Energy-poor consumers: Primary victims (moderate/constrained) — face energy price increases during phase-out transition; benefit long-term from lower renewable costs
 *   - Renewable energy producers: Primary beneficiaries (institutional/arbitrage) — benefit from subsidy reallocation and cost-competitive technology deployment
 *   - Oil & gas corporations: Mixed agent (powerful/mobile) — face extraction through reduced subsidies but also benefit from coordinated global phase-out preventing subsidy races
 *   - Climate-vulnerable populations: Secondary beneficiaries (powerless/trapped) — benefit from climate stabilization but not compensated during transition; climate benefits are diffuse and delayed
 *   - Fossil fuel-dependent nations: Secondary agent (organized/constrained) — face budget pressure from subsidy removal; also face climate risk from continued subsidization
 *   - Climate Coalition: Organized agent (organized/constrained) — orchestrates coordination through international agreements; has sunset logic through renewable dominance targets
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fossil_fuel_subsidy_phase_out, 0.58).
domain_priors:suppression_score(fossil_fuel_subsidy_phase_out, 0.68).
domain_priors:theater_ratio(fossil_fuel_subsidy_phase_out, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fossil_fuel_subsidy_phase_out, extractiveness, 0.58).
narrative_ontology:constraint_metric(fossil_fuel_subsidy_phase_out, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(fossil_fuel_subsidy_phase_out, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(fossil_fuel_subsidy_phase_out, tangled_rope).
narrative_ontology:human_readable(fossil_fuel_subsidy_phase_out, "Fossil Fuel Subsidy Phase-Out Constraint").
narrative_ontology:topic_domain(fossil_fuel_subsidy_phase_out, "economic_policy/energy_transition").

domain_priors:requires_active_enforcement(fossil_fuel_subsidy_phase_out).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(fossil_fuel_subsidy_phase_out, renewable_energy_producers).
narrative_ontology:constraint_beneficiary(fossil_fuel_subsidy_phase_out, climate_vulnerable_populations).
narrative_ontology:constraint_beneficiary(fossil_fuel_subsidy_phase_out, future_generations).
narrative_ontology:constraint_victim(fossil_fuel_subsidy_phase_out, fossil_fuel_extractors).
narrative_ontology:constraint_victim(fossil_fuel_subsidy_phase_out, fossil_fuel_workers).
narrative_ontology:constraint_victim(fossil_fuel_subsidy_phase_out, energy_poor_consumers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: COAL MINER (SNARE) — Trapped in a resource-dependent region with no alternative employment. Subsidy phase-out directly eliminates livelihood with no genuine transition pathway. Cannot exit without geographic relocation or skill retraining with uncertain outcomes. Maximum experienced extraction.
constraint_indexing:constraint_classification(fossil_fuel_subsidy_phase_out, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: ENERGY-POOR CONSUMER (TANGLED ROPE) — Bears disproportionate costs of subsidy phase-out through energy price increases. However, also benefits from coordinated transition that builds renewable infrastructure and reduces long-term energy costs. Significant extraction during transition window, but potential long-term benefit creates coordination component.
constraint_indexing:constraint_classification(fossil_fuel_subsidy_phase_out, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: RENEWABLE ENERGY PRODUCER (ROPE) — Experiences subsidy phase-out as coordination mechanism. Removal of fossil fuel advantages enables cost-competitive deployment. Has arbitrage options (global market access, technology arbitrage). Net beneficiary — extraction runs toward this agent.
constraint_indexing:constraint_classification(fossil_fuel_subsidy_phase_out, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: CLIMATE COALITION (SCAFFOLD) — International climate agreements (Paris Agreement, G20 commitments) frame subsidy phase-out as temporary coordination scaffolding with explicit sunset: goals include 'ending inefficient subsidies' by defined dates. Organized agents (UNEP, climate ministries) see exit path through renewable dominance. Theater_ratio is moderate — significant performative commitment without immediate enforcement.
constraint_indexing:constraint_classification(fossil_fuel_subsidy_phase_out, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: OIL & GAS CORPORATION (TANGLED ROPE) — Faces genuine coordination problem: subsidy phase-out requires coordinated global action to prevent competitive disadvantage. Also bears extraction through reduced rent extraction and forced operational adaptation. Has mobility (capital reallocation, geographic arbitrage across jurisdictions with persistent subsidies). Mixed coordination benefit (global price stability) and extraction cost (reduced margins).
constraint_indexing:constraint_classification(fossil_fuel_subsidy_phase_out, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: SUBSIDY BUREAUCRACY (PITON) — Historical accumulation of energy subsidies embedded in tax codes, price controls, and regulatory preferences now operates largely on institutional inertia. Formal commitments to phase out coexist with continued subsidy expansion (IMF data: global subsidies increased 2000-2022). Theater_ratio reflects performative announcements without functional reform. Degraded through conflict between stated goals and institutional incentive structure.
constraint_indexing:constraint_classification(fossil_fuel_subsidy_phase_out, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From civilizational timescale, some subsidy lag appears inevitable: political economy logic suggests governments will always subsidize energy for electoral reasons. This perspective naturalizes subsidy persistence as a law of political economy. However, structural data reveals this as false summit — subsidy phase-out is achievable through coordination (counterexample: Germany's energy transition), suggesting contingent institutional arrangements rather than natural law.
constraint_indexing:constraint_classification(fossil_fuel_subsidy_phase_out, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(fossil_fuel_subsidy_phase_out_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(fossil_fuel_subsidy_phase_out, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(fossil_fuel_subsidy_phase_out, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(fossil_fuel_subsidy_phase_out, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(fossil_fuel_subsidy_phase_out, TR),
    TR >= 0.70.

:- end_tests(fossil_fuel_subsidy_phase_out_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The constraint creates measurable rent extraction from fossil fuel producers (subsidy removal reduces their implicit income) and significant cost imposition on workers and energy-poor (through employment loss and price increases). However, extraction is not as severe as a pure snare would be (0.66+) because the constraint includes genuine coordination function and transition mechanisms reduce total economic loss. The trend shows increasing extractiveness (0.35→0.58) as transition accelerates and costs concentrate on less-organized populations. Suppression (0.68): High. Multiple barriers prevent exit: workers cannot easily relocate or retrain; energy-poor cannot reduce consumption without welfare loss; fossil fuel corporations cannot exit without capital write-down. However, suppression is not total (0.80+) because some jurisdictions permit subsidy persistence and corporate mobility across jurisdictions partially alleviates constraint. Theater ratio (0.55): Moderate. International commitments have real enforcement mechanisms (financial commitments, regulatory implementation) reducing pure theater, but significant gap persists between announced phase-out targets and actual subsidy trends (global subsidies increased despite phase-out commitments). Theater decline from 0.62→0.55 reflects increasing enforcement credibility, though subsidy fungibility suggests continued performative commitments in some jurisdictions.
 *
 * PERSPECTIVAL GAP:
 *   Perspectival gap is maximum across this constraint. Renewable energy producers experience coordination (Rope) — subsidy reallocation solves market distortion. Coal workers experience pure extraction (Snare) — no coordination benefit, only cost. Energy-poor consumers see mixed extraction and long-term coordination (Tangled Rope) — costs now, benefits later if distributed fairly. Fossil fuel corporations see both extraction (subsidy loss) and coordination benefit (stable global energy pricing prevents subsidy races) — mixed Tangled Rope experience. The analytical observer risks falsely naturalizing subsidy persistence as political-economic law (Mountain), but structural data shows contingent institutional choice (counterexample: Germany's 40% renewable electricity achieved through deliberate phase-out). The constraint identity itself is contested: beneficiaries frame it as natural climate requirement; victims frame it as industrial policy favoring renewables; corporations frame it as necessary coordination to prevent subsidy wars.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) values span the full range, producing the widest perspectival gap in the corpus. Coal miners (powerless/trapped victim) derive d ≈ 0.95, producing maximum f(d) ≈ 1.42 effective extractiveness. Energy-poor consumers (moderate/constrained victim) derive d ≈ 0.70, producing f(d) ≈ 1.00 — experienced extraction equals nominal base extraction. Renewable producers (institutional/arbitrage beneficiary) derive d ≈ 0.10, producing f(d) ≈ -0.01 — experienced extraction is negative (they benefit). Oil & gas corporations (powerful/mobile victim+beneficiary) derive d ≈ 0.55, producing f(d) ≈ 0.75 — mixed extraction reflecting both costs (subsidy removal) and benefits (global coordination preventing subsidy races). The directionality spread reflects genuine structural differences in who bears and who benefits from the constraint.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by clearly separating coordination function from extraction mechanism. The coordination function is real: subsidy phase-out solves the prisoners' dilemma in energy policy (countries want to eliminate distortionary subsidies but fear capital flight if acting alone). This is genuine rope-type coordination benefit. The extraction is equally real: costs concentrate on workers and energy-poor during transition, while benefits concentrate among renewable investors. Neither function subsumes the other — the constraint REQUIRES both enforcement (making it harder to exit) and coordination (making collective action beneficial). This is the defining characteristic of Tangled Rope. The mandatrophy is resolved by explicitly declaring: (1) What is the coordination problem being solved? (Answer: subsidy race prevention and capital reallocation.) (2) Who is the asymmetric extraction targeting? (Answer: fossil fuel producers, workers, energy-poor.) (3) Is the extraction necessary to solve the coordination problem? (Partial: yes for redirecting capital, but not for the magnitude of worker displacement, suggesting excessive extraction beyond coordination requirement.)
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    just_transition_credibility,
    'Are announced ''just transition'' programs genuine alternative pathways or performative theater masking extraction?',
    'Empirical comparison: retraining program completion rates, employment outcomes for former fossil fuel workers, wage replacement in alternative sectors vs. historical employment',
    'If credible (>70% employment retention): suppression falls to 0.40-0.50, multiple perspectives shift to Scaffold. If theater (>40% failed transitions): suppression stays high, snare perspective confirmed for workers.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(just_transition_credibility, empirical, 'Credibility of just transition programs as genuine vs performative').

omega_variable(
    subsidy_fungibility,
    'Do fossil fuel subsidies eliminated in wealthy countries simply migrate to developing nations via capital flows, or does global phase-out reduce total subsidy rent?',
    'Capital flow analysis; subsidy growth rates in jurisdictions vs. countries with phase-out; time-series correlation between phase-out commitment and global subsidy trends',
    'If migration (high probability): global extraction redistribution rather than elimination; classification shifts from snare->rope to snare->tangled_rope. If reduction: constraint achieves stated coordination function; scaffold sunset becomes credible.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(subsidy_fungibility, empirical, 'Geographic redistribution vs. total reduction of subsidy rents').

omega_variable(
    coordination_benefit_allocation,
    'Do benefits of subsidy phase-out (lower long-term energy costs, climate stabilization) accrue to those bearing costs (workers, energy-poor), or do they concentrate among renewable energy investors and high-income consumers?',
    'Benefit incidence analysis; tracking of renewable investment returns vs. energy price impacts by income decile; climate risk exposure reduction by wealth category',
    'If concentrated (likely): extraction component dominates coordination component; most perspectives shift toward snare/tangled_rope. If distributed (unlikely): genuine coordination function; multiple perspectives shift to rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_benefit_allocation, empirical, 'Distribution of coordination benefits vs. costs across income groups').

omega_variable(
    coal_to_renewables_substitutability,
    'Are coal-dependent regional economies genuinely substitutable by renewable energy infrastructure, or is the substitution incomplete (geospatial mismatch, skill-base mismatch)?',
    'Regional employment studies post-phase-out; renewable deployment patterns vs. former coal regions; wage differential between replacement sectors and original fossil fuel employment',
    'If substitutable: energy-poor and worker perspectives shift toward scaffold. If incomplete: trapped exit options confirmed; snare classification held; suppression remains high.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(coal_to_renewables_substitutability, empirical, 'Whether regional coal-to-renewables substitution is achievable').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fossil_fuel_subsidy_phase_out, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ffs_tr_t0, fossil_fuel_subsidy_phase_out, theater_ratio, 0, 0.62).
narrative_ontology:measurement(ffs_tr_t10, fossil_fuel_subsidy_phase_out, theater_ratio, 10, 0.58).
narrative_ontology:measurement(ffs_tr_t20, fossil_fuel_subsidy_phase_out, theater_ratio, 20, 0.55).
narrative_ontology:measurement(ffs_tr_t30, fossil_fuel_subsidy_phase_out, theater_ratio, 30, 0.48).

% Extraction over time
narrative_ontology:measurement(ffs_be_t0, fossil_fuel_subsidy_phase_out, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(ffs_be_t10, fossil_fuel_subsidy_phase_out, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(ffs_be_t20, fossil_fuel_subsidy_phase_out, base_extractiveness, 20, 0.58).
narrative_ontology:measurement(ffs_be_t30, fossil_fuel_subsidy_phase_out, base_extractiveness, 30, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(fossil_fuel_subsidy_phase_out, resource_allocation).
narrative_ontology:boltzmann_floor_override(fossil_fuel_subsidy_phase_out, 0.18).
narrative_ontology:affects_constraint(fossil_fuel_subsidy_phase_out, renewable_energy_deployment_barriers).
narrative_ontology:affects_constraint(fossil_fuel_subsidy_phase_out, just_transition_infrastructure).
narrative_ontology:affects_constraint(fossil_fuel_subsidy_phase_out, climate_stabilization_imperative).

% DUAL FORMULATION NOTE:
% Fossil fuel subsidy phase-out decomposes into multiple structural constraints with distinct ε values: (1) Capital reallocation coordination (ε≈0.25, Rope) — pure market mechanism redirecting investment flows; (2) Worker transition extraction (ε≈0.68, Snare) — concentration of employment costs without adequate compensation; (3) Energy price stabilization (ε≈0.40, Tangled Rope) — genuine coordination of energy markets with asymmetric price impacts. This story integrates all three, which is analytically justified because they are causally coupled (subsidy phase-out triggers all three mechanisms simultaneously). However, if analysis requires isolating the pure coordination benefit from the distributional extraction, decomposition into separate stories is warranted.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(fossil_fuel_subsidy_phase_out, institutional, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
