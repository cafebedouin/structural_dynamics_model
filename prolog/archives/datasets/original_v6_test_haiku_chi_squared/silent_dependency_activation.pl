% ============================================================================
% CONSTRAINT STORY: silent_dependency_activation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_silent_dependency_activation, []).

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
 *   constraint_id: silent_dependency_activation
 *   human_readable: The Invisible Supply Chain Trap
 *   domain: technological/economic
 *
 * SUMMARY:
 *   The invisible supply chain trap is a structural constraint that emerges
 *   when complex technological systems depend on hidden, low-level components
 *   controlled by a single actor or small cartel. The dependency remains
 *   dormant and invisible — obscured by cost accounting that externalizes it,
 *   by complexity that makes it hard to audit, by institutional design that
 *   separates procurement from risk assessment — until a change in market
 *   conditions (supply shock, geopolitical event, demand surge) or policy
 *   intervention suddenly activates it as a critical bottleneck. At that
 *   moment, dependent systems face a binary choice: pay extractive rents to
 *   the controller, or accept catastrophic operational failure during the
 *   years required to redesign around the dependency. The constraint creates
 *   asymmetric extraction: the dependency controller benefits from both the
 *   invisibility (avoiding cost internalization during dormancy) and the
 *   activation crisis (capturing rents when alternatives become too
 *   expensive). End consumers and dependent systems bear the full cost: they
 *   absorbed the invisibility tax through hidden cost transfer, and when
 *   activation occurs, they face either extraction or failure.
 *
 * KEY AGENTS:
 *   - Dependency Monopolist: Primary beneficiary (powerful/arbitrage) — controls low-level component; benefits from invisibility and from activation crisis
 *   - Downstream Dependent Systems: Primary victim (powerless/trapped) — becomes locked-in once dependency activates; cannot exit without years of redesign
 *   - End Consumers: Secondary victim (moderate/constrained) — bears price shocks and supply disruption when dependency activates; had no visibility to make informed choices
 *   - Cost-Externalizing Producers: Secondary beneficiary (institutional/constrained) — extract value by hiding dependency costs in product margins; maintain invisibility through non-disclosure
 *   - Regulatory Apparatus: Organized responder (organized/constrained) — tasked with supply chain resilience but operates under severe information asymmetry
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing dependency opacity as inherent to complex systems rather than recognizing it as a result of economic incentives
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(silent_dependency_activation, 0.58).
domain_priors:suppression_score(silent_dependency_activation, 0.68).
domain_priors:theater_ratio(silent_dependency_activation, 0.64).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(silent_dependency_activation, extractiveness, 0.58).
narrative_ontology:constraint_metric(silent_dependency_activation, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(silent_dependency_activation, theater_ratio, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(silent_dependency_activation, snare).
narrative_ontology:human_readable(silent_dependency_activation, "The Invisible Supply Chain Trap").
narrative_ontology:topic_domain(silent_dependency_activation, "technological/economic").

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(silent_dependency_activation, dependency_monopolist).
narrative_ontology:constraint_beneficiary(silent_dependency_activation, cost_externalizing_producers).
narrative_ontology:constraint_victim(silent_dependency_activation, downstream_dependent_systems).
narrative_ontology:constraint_victim(silent_dependency_activation, end_consumers).
narrative_ontology:constraint_victim(silent_dependency_activation, supply_chain_transparency_advocates).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DEPENDENT SYSTEM (SNARE) — Once activated, cannot exit without catastrophic operational failure. Before activation, the dependency is invisible; after activation, exit is impossible without redesign costing years and billions. d≈0.98, f(d)≈1.48, σ=1.2 → χ≈1.05. Pure extraction with no coordination benefit.
constraint_indexing:constraint_classification(silent_dependency_activation, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: DEPENDENCY MONOPOLIST (ROPE) — Controls the low-level component and experiences the constraint as coordination: providing the component solves a real collective problem (standardization, reliability). d≈0.08, f(d)≈-0.10, σ=1.2 → χ≈-0.07. Net beneficiary; sees constraint as enabling their value capture.
constraint_indexing:constraint_classification(silent_dependency_activation, rope,
    context(agent_power(powerful),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: REGULATORY APPARATUS (TANGLED_ROPE) — Responsible for consumer/system safety and supply chain resilience, but operates under information asymmetry about hidden dependencies. Coordination function: setting standards and transparency requirements. Extraction mechanism: enforcement burden and delayed crisis response. d≈0.52, f(d)≈0.68, σ=1.0 → χ≈0.39.
constraint_indexing:constraint_classification(silent_dependency_activation, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: END CONSUMER (SNARE) — Invisible dependency means no informed choice. When activated, faces supply disruption, price shocks, or system failure. Cannot exit without accepting alternative technologies or forgoing essential services. d≈0.85, f(d)≈1.12, σ=1.2 → χ≈0.76.
constraint_indexing:constraint_classification(silent_dependency_activation, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: COST-EXTERNALIZING PRODUCER (PITON) — Actively benefits from invisibility of dependencies because visibility would trigger cost internalization (material sourcing transparency, supply chain audits, redundancy investment). Theater ratio high because performance metrics (efficiency, cost) hide real fragility. d≈0.15, f(d)≈-0.02, σ=1.2 → χ≈-0.01. Maintains the silent dependency through deliberate non-disclosure and institutional inertia.
constraint_indexing:constraint_classification(silent_dependency_activation, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a systems engineering view, some degree of dependency opacity is inevitable in complex systems: no single actor can have perfect information about all sub-components. This perspective sees silent dependency activation as an immutable property of scale and complexity. BUT structural data (ε=0.58, suppression=0.68) reveals this as a false summit: the invisibility is actively maintained through economic incentives and regulatory gaps, not inherent to complexity.
constraint_indexing:constraint_classification(silent_dependency_activation, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(silent_dependency_activation_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(silent_dependency_activation, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(silent_dependency_activation, TypeOther, context(agent_power(powerful), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(silent_dependency_activation, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(silent_dependency_activation, TR),
    TR >= 0.70.

:- end_tests(silent_dependency_activation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness (0.58): High. The constraint extracts value through multiple mechanisms: (1) the monopolist captures economic rent during dormancy via cost externalization; (2) during activation, the monopolist can extract crisis rents from trapped systems; (3) dependent systems lose optionality and competitive advantage; (4) end consumers lose choice. Suppression (0.68): High. Multiple mechanisms suppress alternatives and exit options: knowledge suppression (dependencies are deliberately hidden), technical suppression (redesigning around a dependency takes years), economic suppression (switching costs are astronomical), regulatory suppression (no requirement to disclose or maintain substitutes). Theater ratio (0.64): Moderate-high. Performance metrics during dormancy (cost efficiency, reliability, on-time delivery) hide the real fragility of the system. The monopolist's supply chain risk management is largely theatrical—designed to convince investors and regulators of resilience while actually depending on the hidden component remaining cheap and available. When activation occurs, the theater collapses.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates the full spectrum from the dependent system's view (pure snare: trapped, no exit, maximum extraction) to the monopolist's view (pure rope: coordination mechanism that enables their value creation). The regulatory apparatus occupies a tangled middle: responsible for resilience but structurally prevented from having visibility by the information asymmetry. The cost-externalizing producer sees the constraint as a piton — a degraded institution (non-disclosure) maintained through deliberate inertia because visibility would destroy their margin structure. The end consumer sees a snare (no visibility, no choice, bears cost when activated). The false summit is the analytical observer's natural law reading: 'complexity inherently creates hidden dependencies.' In reality, many hidden dependencies are actively hidden through economic incentives.
 *
 * DIRECTIONALITY LOGIC:
 *   Dependency Monopolist: Beneficiary + arbitrage → d≈0.08. Net beneficiary; drives the constraint. Downstream dependent systems: Victim + trapped → d≈0.98. Maximum extraction; cannot exit once activated. End consumers: Victim + constrained → d≈0.85. High extraction; invisibility prevents informed choice, and activation prevents exit. Regulatory apparatus: Mixed (victim of information asymmetry + constrained) → d≈0.52. Responsible but structurally prevented from having visibility. Cost-externalizing producers: Beneficiary-like (benefit from invisibility) but constrained (threatened by activation) → d≈0.15. Override not needed; the piton classification captures the degradation.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by recognizing that the snare classification (the constraint is pure extraction) is correct at the moment of activation but misses the temporal structure of the trap: the constraint operates in two phases. Phase 1 (dormancy): extraction is hidden, suppression is high (invisibility maintained through incentives), but the dependent system has theoretical alternatives (they could have invested in redundancy or substitutes during dormancy). Phase 2 (activation): extraction is explicit, suppression is total (exit is now impossible), and the snare becomes irreversible. The regulatory apparatus should be focused on preventing Phase 2 from occurring, which requires breaking Phase 1's suppression (requiring visibility, redundancy standards, substitute qualification). The fact that Phase 1 invisibility is maintained through deliberate incentives (not inherent complexity) is the key to mandatrophy resolution: this is not a law of nature but a contingent institutional arrangement. The snare classification stands, but with the understanding that it is remediable during dormancy and catastrophic during activation.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    activation_threshold_determinism,
    'What market or policy conditions deterministically activate a previously-silent dependency as a bottleneck?',
    'Historical case analysis of dependency activations (rare earth export controls, semiconductor fab concentration, battery supply chain disruption); identification of threshold variables (supply concentration ratio, geopolitical event magnitude, demand growth rate)',
    'If deterministic and forecastable: regulatory early warning becomes possible, reducing extraction. If random or dependent on opaque political decisions: dependency remains permanently activated once triggered.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(activation_threshold_determinism, empirical, 'Determinism of silent dependency activation thresholds').

omega_variable(
    visibility_cost_asymmetry,
    'Do the costs of supply chain transparency disproportionately burden dependent systems vs. beneficiary monopolists?',
    'Cost-benefit analysis of transparency requirements (supplier audits, redundancy investment, component re-qualification) for dependent vs. monopolist actors; comparison of compliance burden across firm size and market power',
    'If asymmetric: transparency becomes de facto extraction tax (Tangled Rope dominant). If symmetric: transparency is pure coordination (Rope dominant). Affects regulatory feasibility of breaking silent dependency.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(visibility_cost_asymmetry, empirical, 'Cost asymmetry in supply chain transparency requirements').

omega_variable(
    substitute_availability_reality,
    'For major silent dependencies, are economically viable substitutes actually available or only theoretically possible?',
    'Engineering feasibility and cost parity analysis for substitute technologies; timeline to qualification and deployment; comparison with monopolist R&D investment rates',
    'If viable substitutes exist: exit is technically possible (restores ''mobile'' status to trapped agents). If only theoretical: exit remains impossible despite theoretical alternatives, confirming snare classification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(substitute_availability_reality, empirical, 'Actual vs. theoretical availability of substitute technologies').

omega_variable(
    producer_incentive_to_maintain_invisibility,
    'Is the invisibility of dependencies a deliberate strategy by monopolist actors or a byproduct of incentive misalignment?',
    'Analysis of disclosure practices before vs. after regulatory pressure; comparison of firms with strong vs. weak incentives to maintain secrecy; examination of internal communications and supply chain strategy documents',
    'If deliberate: the constraint is fully a Snare (intentional extraction). If byproduct: constraint is more Tangled Rope (coordination failure rather than predation). Affects remediation strategy.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(producer_incentive_to_maintain_invisibility, conceptual, 'Whether invisibility is deliberate strategy or systemic byproduct').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(silent_dependency_activation, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sda_tr_t0, silent_dependency_activation, theater_ratio, 0, 0.4).
narrative_ontology:measurement(sda_tr_t5, silent_dependency_activation, theater_ratio, 5, 0.52).
narrative_ontology:measurement(sda_tr_t10, silent_dependency_activation, theater_ratio, 10, 0.64).

% Extraction over time
narrative_ontology:measurement(sda_be_t0, silent_dependency_activation, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(sda_be_t5, silent_dependency_activation, base_extractiveness, 5, 0.35).
narrative_ontology:measurement(sda_be_t10, silent_dependency_activation, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(silent_dependency_activation, global_infrastructure).
narrative_ontology:affects_constraint(silent_dependency_activation, rare_earth_supply_concentration).
narrative_ontology:affects_constraint(silent_dependency_activation, semiconductor_fab_centralization).
narrative_ontology:affects_constraint(silent_dependency_activation, battery_supply_chain_choke_points).

% DUAL FORMULATION NOTE:
% The silent dependency activation is a meta-constraint that operates across multiple specific supply chain domains (rare earths, semiconductors, batteries). Each specific domain has its own ε value reflecting empirical supply concentration. This story models the structural pattern common to all: invisibility during dormancy, catastrophic activation, and asymmetric extraction. The upstream constraints model specific cases; this story models the generic activation mechanism.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(silent_dependency_activation, institutional, 0.18).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
