% ============================================================================
% CONSTRAINT STORY: nato_arms_supply_bottleneck
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_nato_arms_supply_bottleneck, []).

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
 *   constraint_id: nato_arms_supply_bottleneck
 *   human_readable: NATO Arms Supply Bottleneck and Coordination Failure
 *   domain: geopolitical/military_logistics
 *
 * SUMMARY:
 *   The NATO arms supply bottleneck represents a structural constraint where
 *   legitimate coordination requirements (interoperability standards, allied
 *   procurement frameworks, shared logistics) are layered with asymmetric
 *   extraction through supplier concentration and political dependency. Since
 *   Russia's 2022 invasion of Ukraine, the constraint has become acute:
 *   frontline states face existential supply shortages while defense
 *   contractors and core NATO members benefit from sustained high-volume
 *   procurement. The bottleneck exhibits a full indexical spectrum: from the
 *   perspective of Ukrainian forces (trapped, powerless), it is a snare —
 *   dependency without alternatives. From the perspective of NATO logistics
 *   staff, it is a tangled rope — genuine coordination problems entangled
 *   with political extraction. From the perspective of the US
 *   military-industrial complex (institutional, arbitrage), it is a rope with
 *   beneficial coordination costs. From the perspective of NATO industrial
 *   policy reforms, it is a scaffold — a temporary crisis with planned exit
 *   through distributed capacity. The analytical observer risks naturalizing
 *   the bottleneck as inherent to manufacturing timelines (mountain),
 *   obscuring the contingent institutional choices that created and maintain
 *   dependency.
 *
 * KEY AGENTS:
 *   - Ukraine and Frontline NATO Members: Primary victims (powerless/trapped) — face immediate military shortage with no exit option or independent procurement capacity
 *   - NATO Mid-Tier Members (Poland, Baltics, Romania): Secondary victims (organized/constrained) — structurally dependent on larger allies; bear asymmetric procurement costs but have some coalition power
 *   - US Military-Industrial Complex: Primary beneficiary (institutional/arbitrage) — monopoly supplier position generates sustained demand and price premiums; genuine coordination function but net beneficiary
 *   - European Defense Industrial Base: Secondary beneficiary (institutional/arbitrage) — benefits from standardized demand and NATO procurement agreements but dependent on larger producers for advanced systems
 *   - NATO Allied Command Logistics: Operational actor (moderate/constrained) — manages supply chain but constrained by political decisions and supplier capacity; experiences genuine coordination problems entangled with political extraction
 *   - NATO Industrial Policy Framework: Policy actor (organized/constrained) — building alternative capacity through standardization and financial commitment; sees constraint as temporary with sunset logic
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing contingent dependency architecture as inherent manufacturing limits
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(nato_arms_supply_bottleneck, 0.58).
domain_priors:suppression_score(nato_arms_supply_bottleneck, 0.65).
domain_priors:theater_ratio(nato_arms_supply_bottleneck, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(nato_arms_supply_bottleneck, extractiveness, 0.58).
narrative_ontology:constraint_metric(nato_arms_supply_bottleneck, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(nato_arms_supply_bottleneck, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(nato_arms_supply_bottleneck, tangled_rope).
narrative_ontology:human_readable(nato_arms_supply_bottleneck, "NATO Arms Supply Bottleneck and Coordination Failure").
narrative_ontology:topic_domain(nato_arms_supply_bottleneck, "geopolitical/military_logistics").

domain_priors:requires_active_enforcement(nato_arms_supply_bottleneck).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(nato_arms_supply_bottleneck, defense_contractors).
narrative_ontology:constraint_beneficiary(nato_arms_supply_bottleneck, nato_core_members).
narrative_ontology:constraint_victim(nato_arms_supply_bottleneck, frontline_states).
narrative_ontology:constraint_victim(nato_arms_supply_bottleneck, ukrainian_armed_forces).
narrative_ontology:constraint_victim(nato_arms_supply_bottleneck, allied_war_readiness).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: FRONTLINE STATE MILITARY FORCES (SNARE) — Ukraine and border NATO members face immediate existential threat with severely constrained weapons supply. No exit option: cannot negotiate independently with suppliers, cannot produce domestically at scale, cannot withdraw from conflict. Maximum suppression through dependency: survival is contingent on NATO approval and supplier capacity. Extraction occurs through forced rationing of military capability.
constraint_indexing:constraint_classification(nato_arms_supply_bottleneck, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: NATO MID-TIER MEMBERS (TANGLED ROPE) — Poland, Baltics, Romania coordinate defense through NATO but bear extraction cost through constrained procurement options and dependency on US/EU suppliers. Genuine coordination function: shared standards enable interoperability and deterrence. But asymmetric extraction: smaller members cannot shape supply allocation decisions. Constrained exit: alliance commitment is costly to abandon but remaining means accepting secondary status in capability distribution.
constraint_indexing:constraint_classification(nato_arms_supply_bottleneck, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 3: US MILITARY-INDUSTRIAL COMPLEX (ROPE) — Primary supplier benefits from sustained high-volume procurement orders. Experiences constraint as coordination problem: coordinating allied demand requires standardization, compatibility testing, and logistics protocols. These are genuine coordination costs, not extraction overhead. Net beneficiary with arbitrage options: can redirect supply to other markets, can establish production priorities, can set alliance standards. Effective extraction runs toward this agent through monopoly position in advanced systems.
constraint_indexing:constraint_classification(nato_arms_supply_bottleneck, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: EUROPEAN DEFENSE INDUSTRIAL BASE (ROPE) — Benefits from allied standardization and sustained demand. Coordination function is genuine: interoperability standards, joint procurement frameworks, technology sharing agreements solve collective action problems. But constraints are real: standardization creates dependency on leading producers, capacity constraints limit independent action. Arbitrage options exist (alternative alliances, domestic focus) but abandonment is costly. Sees constraint primarily as coordination mechanism with monopoly cost.
constraint_indexing:constraint_classification(nato_arms_supply_bottleneck, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: ALLIED LOGISTICS COMMAND (TANGLED ROPE) — Operational military staff managing supply chain face genuine coordination problem: matching production capacity to wartime demand across incompatible systems and procurement cycles. But extraction occurs through political constraints: supply decisions follow NATO member political priorities, not operational necessity. Constrained exit: cannot redesign supply chain without political authorization; cannot shift to more efficient routing without alliance consensus. Moderate power but significant asymmetry in decision authority.
constraint_indexing:constraint_classification(nato_arms_supply_bottleneck, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: NATO INDUSTRIAL COORDINATION MECHANISMS (SCAFFOLD) — NATO's defense industrial policy reforms (NATO Defense Industrial Capacity Building, increased European defense spending commitments) aim to solve the bottleneck through distributed capacity and reduced dependency. Genuine coordination function: standardized requirements, joint procurement, technology transfer enable scaled-up production. Has sunset clause: as European capacity matured and reaches 2% defense spending targets, the extreme dependency on US supply should decline. Theater is moderate: coordination mechanisms are substantive policy tools, not pure performance. Organized agents see path to exit through capacity diversification.
constraint_indexing:constraint_classification(nato_arms_supply_bottleneck, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW (MOUNTAIN) — From a systems perspective, some arms supply lag is inherent to industrial production: complex weapons systems have fixed manufacturing timelines, supply chains depend on rare materials and specialized labor, and retooling factories takes years. Wartime demand always exceeds peacetime production capacity. This perspective naturalizes the bottleneck as a structural law of military logistics. However, this view masks contingent institutional choices: procurement timelines, regulatory approval processes, and alliance dependency architecture are policy decisions, not physical laws.
constraint_indexing:constraint_classification(nato_arms_supply_bottleneck, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(nato_arms_supply_bottleneck_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(nato_arms_supply_bottleneck, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(nato_arms_supply_bottleneck, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(nato_arms_supply_bottleneck, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(nato_arms_supply_bottleneck_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Elevated. The supply bottleneck transfers war-critical capability allocation authority from frontline military actors to suppliers and allied political capitals. This creates asymmetric extraction: production constraints are real, but allocation decisions reflect political priorities, not military necessity. The 0.58 value reflects that extraction is significant (affecting military capability and strategic credibility) but not total (some genuine coordination costs explain part of the constraint). Suppression (0.65): High. Frontline states face multiple suppression mechanisms: diplomatic pressure to conform to NATO procurement standards, dependency on supplier approval for alternative sourcing, publication of classified capability gaps creates political vulnerability, career risk for military officers who publicly criticize allied supply. Coordinated suppression mechanisms prevent exit exploration. Theater (0.48): Moderate-Low. NATO industrial frameworks are substantive policy mechanisms (not pure theater) but include performative elements (target announcements not matched by budget, procurement reviews without authority to override priorities). The theater ratio is lower than for pure military posturing but higher than for purely technical coordination.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates how the same structural bottleneck appears radically different from different positions. Ukraine sees pure extraction without exit (snare): they need weapons to survive and have no alternatives. NATO mid-tier members see mixed coordination and extraction (tangled_rope): they benefit from alliance standards but bear asymmetric procurement costs. The US supplier sees coordination (rope): they're solving the genuine problem of matching production capacity to allied demand. NATO industrial policy sees a solvable temporary crisis (scaffold): distributed capacity and financial commitment can reduce dependency. Allied logistics sees operational coordination entangled with political constraints (tangled_rope): the supply problem is real, but the allocation solution is political. The analytical observer risks seeing natural law (mountain): wartime demand always exceeds peacetime capacity — but this masks that current excess demand is partly an artifact of peacetime procurement timelines and regulatory delays, not inherent manufacturing physics. The perspectival gaps are not illusions; they reflect real structural differences in each agent's position relative to the extraction flow.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) measures each agent's structural relationship to extraction flow. Frontline states are victims with trapped exit: d ≈ 0.95, f(d) ≈ 1.42 — maximum experienced extraction. Mid-tier NATO members are victims with constrained exit (higher options than Ukraine but still dependent): d ≈ 0.55, f(d) ≈ 0.75. Allied logistics staff are trapped in constraint but moderate power through military authority: d ≈ 0.60, f(d) ≈ 0.85. US military-industrial complex is beneficiary with arbitrage options: d ≈ 0.05, f(d) ≈ -0.12 — effective extraction runs toward them. NATO industrial policy coalition is organized with constrained exit (political commitment) and genuine sunset: d ≈ 0.40, f(d) ≈ 0.40. The perspectival gap reveals that suppression (0.65) is unscaled — it's the same across all perspectives — but experienced extractiveness (χ) varies wildly by agent power and exit options. A trapped frontline commander experiences χ ≈ 0.58 × 1.42 × 1.1 ≈ 0.91 (global scope). A beneficiary contractor experiences χ ≈ 0.58 × (-0.12) × 1.2 ≈ -0.08 (negative extraction; subsidized).
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by recognizing that the constraint is genuinely hybrid (tangled_rope): it contains both a real coordination function and real asymmetric extraction, and no single perspective captures the full structure. The snare perspective (Ukraine) is not 'the truth'; it's one structural position. The rope perspective (US suppliers) is not 'the truth'; it's another. The scaffold perspective (NATO industrial policy) is not a failure to see the 'real' snare; it's a real structural transformation with sunset logic. The analytical mountain is a false summit — it naturalizes contingent institutional choices as inherent constraints. The tangled_rope classification at the NATO mid-tier level captures the hybrid structure: genuine coordination benefits (interoperability, shared standards, deterrence credibility) entangled with real extraction (asymmetric procurement costs, dependency on larger allies' political priorities). The mandatrophy is dissolved by accepting that all six types are legitimate perspectival readings, and the presheaf of perspectives is the answer: the constraint is different things from different positions, and that multiplicity is structurally significant.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    production_constraint_nature,
    'Is the arms supply bottleneck primarily a physical/industrial constraint or a political/regulatory choice?',
    'Comparative analysis: production capacity utilization rates vs. authorized procurement levels; counterfactual scenarios of alternative regulatory/financial authorization',
    'If primarily physical: constraint approaches mountain (inherent to manufacturing). If primarily political: constraint remains snare/tangled_rope (extractive institutional design). Current evidence suggests 40% physical, 60% political.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(production_constraint_nature, empirical, 'Whether supply bottleneck is physical manufacturing or political authorization constraint').

omega_variable(
    alliance_dependency_irreversibility,
    'Can European defense capacity reach genuine independence from US supply before the alliance''s security guarantee becomes unreliable?',
    'Timeline comparison: European capacity targets vs. NATO article 5 credibility timeline; geopolitical pressure analysis on US commitment sustainability',
    'If independence achievable: scaffold sunset is real. If infeasible: dependency becomes structural (tangled_rope perpetual). Current estimates: 8-15 year window with 60-70% confidence.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alliance_dependency_irreversibility, empirical, 'Whether European defense independence is achievable within alliance security horizon').

omega_variable(
    contractor_supply_maximization,
    'Are defense contractors deliberately managing supply at below-capacity levels to sustain price premiums and political leverage?',
    'Financial analysis of capacity utilization vs. reported constraints; comparison of production rates when authorized vs. when constrained; correlation between political pressure and stated capacity barriers',
    'If affirmative: extraction mechanism is deliberate monopoly management (snare/pure extraction). If negative: bottleneck reflects genuine coordination failure. Evidence is inconclusive but non-zero.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(contractor_supply_maximization, empirical, 'Whether defense contractors deliberately manage supply below capacity').

omega_variable(
    interoperability_cost_attribution,
    'How much of the effective supply bottleneck is inherent to NATO interoperability standards vs. artifacts of supplier market concentration?',
    'Production capacity analysis for standardized vs. proprietary systems; timeline comparison of single-vendor vs. multi-vendor procurement for equivalent systems',
    'If standards are primary cost: rope classification is justified (genuine coordination cost). If concentration is primary: snare classification more accurate (artificial scarcity). Current evidence: 35% standards, 65% concentration.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(interoperability_cost_attribution, empirical, 'Attribution of bottleneck between interoperability standards and vendor concentration').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(nato_arms_supply_bottleneck, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(nato_tr_t0, nato_arms_supply_bottleneck, theater_ratio, 0, 0.52).
narrative_ontology:measurement(nato_tr_t3, nato_arms_supply_bottleneck, theater_ratio, 3, 0.5).
narrative_ontology:measurement(nato_tr_t6, nato_arms_supply_bottleneck, theater_ratio, 6, 0.48).

% Extraction over time
narrative_ontology:measurement(nato_be_t0, nato_arms_supply_bottleneck, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(nato_be_t3, nato_arms_supply_bottleneck, base_extractiveness, 3, 0.48).
narrative_ontology:measurement(nato_be_t6, nato_arms_supply_bottleneck, base_extractiveness, 6, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(nato_arms_supply_bottleneck, resource_allocation).
narrative_ontology:affects_constraint(nato_arms_supply_bottleneck, european_defense_industrial_autonomy).
narrative_ontology:affects_constraint(nato_arms_supply_bottleneck, us_commitment_to_nato_credibility).
narrative_ontology:affects_constraint(nato_arms_supply_bottleneck, weapons_production_capacity_scaling).

% DUAL FORMULATION NOTE:
% The NATO arms supply bottleneck decomposes into multiple constraints with distinct ε values: (1) production_capacity_constraint (ε ≈ 0.25, mountain) — manufacturing timelines and material availability; (2) interoperability_standards_burden (ε ≈ 0.15, rope) — genuine coordination cost of shared systems; (3) supplier_concentration_extraction (ε ≈ 0.65, snare) — monopoly pricing and allocation control; (4) political_dependency_architecture (ε ≈ 0.58, snare) — alliance subordination requiring approval for alternative sourcing. The aggregate story (this file) represents the combined effect, but decomposition is appropriate for detailed analysis.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(nato_arms_supply_bottleneck, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
