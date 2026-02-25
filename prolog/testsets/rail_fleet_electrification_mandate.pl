% ============================================================================
% CONSTRAINT STORY: rail_fleet_electrification_mandate
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-29
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_rail_fleet_electrification_mandate, []).

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
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: rail_fleet_electrification_mandate
 *   human_readable: Mandated Electrification of National Rail Fleet
 *   domain: technological/economic/environmental
 *
 * SUMMARY:
 *   This constraint models the mandated, large-scale transition of a national
 *   railway from diesel to electric locomotives, framed as an environmental
 *   and modernization initiative. While it serves a genuine coordination
 *   purpose—aligning a major industry with national decarbonization goals—it
 *   also imposes severe, uncompensated costs on the incumbent diesel-focused
 *   workforce and supply chain. This creates a structural conflict where a
 *   societal benefit is achieved through targeted extraction from a
 *   politically and economically weaker group.
 *
 * KEY AGENTS:
 *   - State Regulators & National Rail Operator: Primary beneficiary (institutional/arbitrage) - achieve climate targets and operational efficiencies.
 *   - Electric Locomotive Manufacturers: Primary beneficiary (organized/mobile) - gain a protected market for new technology.
 *   - Diesel Locomotive Workforce: Primary victim (powerless/trapped) - skills are devalued, leading to job insecurity and displacement.
 *   - Diesel Industry Incumbents: Secondary victim (organized/constrained) - lose market share and existing capital assets are stranded.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(rail_fleet_electrification_mandate, 0.55).
domain_priors:suppression_score(rail_fleet_electrification_mandate, 0.68).
domain_priors:theater_ratio(rail_fleet_electrification_mandate, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(rail_fleet_electrification_mandate, extractiveness, 0.55).
narrative_ontology:constraint_metric(rail_fleet_electrification_mandate, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(rail_fleet_electrification_mandate, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(rail_fleet_electrification_mandate, tangled_rope).
narrative_ontology:human_readable(rail_fleet_electrification_mandate, "Mandated Electrification of National Rail Fleet").
narrative_ontology:topic_domain(rail_fleet_electrification_mandate, "technological/economic/environmental").

domain_priors:requires_active_enforcement(rail_fleet_electrification_mandate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(rail_fleet_electrification_mandate, electric_locomotive_manufacturers).
narrative_ontology:constraint_beneficiary(rail_fleet_electrification_mandate, national_rail_operators).
narrative_ontology:constraint_beneficiary(rail_fleet_electrification_mandate, state_regulators).
narrative_ontology:constraint_beneficiary(rail_fleet_electrification_mandate, electricity_grid_operators).
narrative_ontology:constraint_victim(rail_fleet_electrification_mandate, diesel_locomotive_workforce).
narrative_ontology:constraint_victim(rail_fleet_electrification_mandate, diesel_engine_manufacturers).
narrative_ontology:constraint_victim(rail_fleet_electrification_mandate, fossil_fuel_suppliers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE DISPLACED WORKER (SNARE) — A skilled mechanic whose expertise in diesel engines is rendered obsolete by the mandated transition. They face unemployment or costly retraining with no guarantee of equivalent work. From this view, the policy is a pure extractive act that destroys their livelihood for a societal goal from which they may not benefit. d≈0.95, f(d)≈1.42, σ=0.9 → χ≈0.70.
constraint_indexing:constraint_classification(rail_fleet_electrification_mandate, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: THE REGULATOR (ROPE) — An agency tasked with meeting national or international emissions targets. They see the mandate as a pure coordination tool to align industrial and public interests towards a common good (cleaner air, climate stability). The costs borne by the legacy industry are viewed as necessary externalities for progress. d≈0.05, f(d)≈-0.12, σ=1.0 → χ≈-0.07.
constraint_indexing:constraint_classification(rail_fleet_electrification_mandate, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE) — This observer recognizes both the genuine coordination function (decarbonization) and the asymmetric extraction from the legacy diesel sector. The constraint simultaneously solves a collective action problem while imposing uncompensated costs on a specific, politically weaker group. This dual nature is the hallmark of a Tangled Rope. d≈0.73, f(d)≈1.15, σ=1.2 → χ≈0.76.
constraint_indexing:constraint_classification(rail_fleet_electrification_mandate, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 4: THE NEW MANUFACTURER (ROPE) — The company building the new electric locomotives. They are a primary beneficiary, seeing the mandate as a coordination mechanism that creates a guaranteed market for their product. Their mobility allows them to sell to any country adopting these standards. d≈0.15 (beneficiary, mobile) f(d)≈-0.01, σ=1.1 → χ≈-0.006.
constraint_indexing:constraint_classification(rail_fleet_electrification_mandate, rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(continental))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(rail_fleet_electrification_mandate_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(rail_fleet_electrification_mandate, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(rail_fleet_electrification_mandate, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(rail_fleet_electrification_mandate, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(rail_fleet_electrification_mandate_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (ε=0.55) reflects the significant economic value (jobs, capital, expertise) lost by the displaced diesel sector. Suppression (0.68) is high because regulatory mandates and network effects make it nearly impossible for the legacy technology to persist or compete once the transition is underway. The theater ratio (0.35) is moderate, acknowledging the real functional benefits of electrification while accounting for the 'green' marketing used to obscure the extractive consequences for losers.
 *
 * PERSPECTIVAL GAP:
 *   The gap is stark. For the state regulator, the mandate is a clean and efficient Rope solving a collective action problem. For the diesel mechanic whose career is erased, it is an inescapable Snare. The manufacturer of the new technology also sees a Rope, one that conveniently creates a captive market. The analytical observer, weighing both the coordination benefits and the asymmetric costs, must classify it as a Tangled Rope.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries are institutional and organized actors who shape the rules of the transition, giving them low directionality ('d') values and perceiving the constraint as a subsidy (negative χ). Victims are the powerless or trapped workforce and industries subject to these rules, resulting in high 'd' values and experiencing severe extraction (high positive χ). The system extracts value from the latter to deliver policy wins and profits to the former.
 *
 * MANDATROPHY ANALYSIS:
 *   This story is a clear case for Mandatrophy resolution. A naive analysis might label this transition a pure Rope, focusing only on the positive environmental outcome. This would be a mandatrophy error, ignoring the severe extraction imposed on the displaced workers. The framework, by requiring perspectives from the victims and calculating the high effective extraction they face, correctly identifies the full structure as a Tangled Rope, preventing the misclassification of targeted industrial policy as a purely benevolent coordination act.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    just_transition_feasibility,
    'Could the transition have been structured to fully compensate the displaced workforce and industries, effectively turning the constraint into a pure Rope?',
    'Comparative analysis of industrial transition policies, contrasting outcomes in regions with and without comprehensive ''Just Transition'' funds, retraining programs, and early retirement packages.',
    'If full compensation is feasible and implemented, the base extractiveness (ε) would approach zero, reclassifying the constraint to Rope. If it''s structurally impossible, the Tangled Rope classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(just_transition_feasibility, preference, 'Whether a ''Just Transition'' could eliminate the extractive component.').

omega_variable(
    net_lifecycle_impact,
    'Does the full lifecycle environmental cost of the new electric fleet (including mining for battery minerals, grid upgrades, and manufacturing) offer a significant net benefit over maintaining and upgrading the existing diesel fleet?',
    'Comprehensive, independent lifecycle assessment (LCA) studies comparing the two technological paths, including energy return on investment (EROI) and carbon accounting.',
    'If the net benefit is marginal or negative, the constraint''s coordination function is weakened, revealing it to be more of an industrial policy Snare benefiting specific manufacturers rather than a genuine environmental Rope.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(net_lifecycle_impact, empirical, 'Whether the new technology''s full lifecycle cost is a true improvement.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(rail_fleet_electrification_mandate, 2020, 2040).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(rail_tr_t2020, rail_fleet_electrification_mandate, theater_ratio, 2020, 0.4).
narrative_ontology:measurement(rail_tr_t2030, rail_fleet_electrification_mandate, theater_ratio, 2030, 0.35).
narrative_ontology:measurement(rail_tr_t2040, rail_fleet_electrification_mandate, theater_ratio, 2040, 0.35).

% Extraction over time
narrative_ontology:measurement(rail_be_t2020, rail_fleet_electrification_mandate, base_extractiveness, 2020, 0.45).
narrative_ontology:measurement(rail_be_t2030, rail_fleet_electrification_mandate, base_extractiveness, 2030, 0.5).
narrative_ontology:measurement(rail_be_t2040, rail_fleet_electrification_mandate, base_extractiveness, 2040, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(rail_fleet_electrification_mandate, global_infrastructure).
narrative_ontology:affects_constraint(rail_fleet_electrification_mandate, fossil_fuel_phase_out).
narrative_ontology:affects_constraint(rail_fleet_electrification_mandate, critical_mineral_supply_chains).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
