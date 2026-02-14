% ============================================================================
% CONSTRAINT STORY: fiber_optic_chip_tech
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-05-21
% ============================================================================

:- module(constraint_fiber_optic_chip_tech, []).

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
    domain_priors:emerges_naturally/1.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: fiber_optic_chip_tech
 *   human_readable: Strategic Control over Fiber-Optic Chip Technology
 *   domain: technological
 *
 * SUMMARY:
 *   A Chinese research team has developed a novel method for creating
 *   high-performance electro-optic modulators (a key component for 6G and AI)
 *   from a single lithium niobate crystal fiber. This technology bypasses
 *   the complex, capital-intensive silicon-based fabrication methods
 *   (e.g., EUV lithography) currently dominated by Western nations. The
 *   constraint is the strategic control and dependency this new, potentially
 *   more efficient, technology creates in the global technology ecosystem.
 *
 * KEY AGENTS (by structural relationship):
 *   - Western Semiconductor Bloc: Primary target (institutional/constrained) — Their technological leverage through complex fabrication is eroded, facing a new competitor that bypasses their control points.
 *   - End Consumers: Downstream targets (powerless/trapped) — Bear the indirect costs of geopolitical friction, such as higher prices or fragmented standards.
 *   - Chinese Tech Ecosystem: Primary beneficiary (institutional/arbitrage) — Gains technological sovereignty, a key advantage in the 6G race, and a counter-lever against technology sanctions.
 *   - Global Technology Integrators: Secondary actors (organized/mobile) — Companies like Apple, Google, Samsung must now navigate a bifurcated supply chain with competing standards and geopolitical pressures.
 *   - Analytical Observer: Analytical observer — Sees the full structure as a coordination mechanism with deeply embedded geopolitical extraction.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fiber_optic_chip_tech, 0.55).
domain_priors:suppression_score(fiber_optic_chip_tech, 0.65).   % Structural property (raw, unscaled).
domain_priors:theater_ratio(fiber_optic_chip_tech, 0.10).       % Piton detection (>= 0.70)

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fiber_optic_chip_tech, extractiveness, 0.55).
narrative_ontology:constraint_metric(fiber_optic_chip_tech, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(fiber_optic_chip_tech, theater_ratio, 0.10).

% --- NL Profile Metrics (required for mountain constraints) ---
% N/A for this constraint.

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(fiber_optic_chip_tech, tangled_rope).

% --- Binary flags ---
domain_priors:requires_active_enforcement(fiber_optic_chip_tech). % Enforcement via patents, industrial policy, and export controls.

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% These feed the directionality derivation chain.
% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(fiber_optic_chip_tech, chinese_tech_ecosystem).
%
% Who bears disproportionate cost?
narrative_ontology:constraint_victim(fiber_optic_chip_tech, western_semiconductor_bloc).
narrative_ontology:constraint_victim(fiber_optic_chip_tech, end_consumers).
%
% Gate requirements:
%   Tangled Rope: beneficiary + victim + requires_active_enforcement (all three satisfied)

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × f(d) × σ(S)
   where f(d) is the sigmoid directionality function:
     f(d) = -0.20 + 1.70 / (1 + e^(-6*(d - 0.50)))
   The engine derives d from beneficiary/victim membership + exit_options.
   Scope modifiers: local=0.8, regional=0.9, national=1.0,
                    continental=1.1, global=1.2, universal=1.0.
   CONTEXT ARITY: All context() terms must have exactly 4 arguments.
   ========================================================================== */

% PERSPECTIVE 1: THE PRIMARY TARGET (THE ESTABLISHED WESTERN BLOC)
% They are institutional victims with constrained exit. Their previous
% technological monopoly is being bypassed, creating a strategic trap.
% Engine derives a high d, leading to high χ and a Snare classification.
constraint_indexing:constraint_classification(fiber_optic_chip_tech, snare,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 2: THE PRIMARY BENEFICIARY (THE CHINESE TECH ECOSYSTEM)
% They are institutional beneficiaries with arbitrage exit; they created the
% new option. Engine derives a very low d, leading to negative χ. For them,
% this is a pure coordination and subsidy mechanism.
constraint_indexing:constraint_classification(fiber_optic_chip_tech, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER
% Sees both the genuine coordination function (enabling 6G) and the
% asymmetric geopolitical extraction. This balanced view, combining the
% function and the cost, correctly identifies the structure as a Tangled Rope.
constraint_indexing:constraint_classification(fiber_optic_chip_tech, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 4: GLOBAL TECHNOLOGY INTEGRATORS
% These powerful firms (e.g., device manufacturers) have mobile exit but are
% caught between the two blocs. They experience the constraint as a Tangled Rope:
% they can benefit from the new tech's performance/cost (coordination) but
% must navigate the attached geopolitical costs and supply chain risks (extraction).
constraint_indexing:constraint_classification(fiber_optic_chip_tech, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: END CONSUMERS (POWERLESS TARGET)
% Individual users of technology are powerless and trapped within the resulting
% ecosystem. They bear the downstream costs of geopolitical competition (e.g.,
% price hikes, fragmented standards, data sovereignty risks) without any
% ability to influence the outcome. For them, the system is a Snare.
constraint_indexing:constraint_classification(fiber_optic_chip_tech, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(fiber_optic_chip_tech_tests).

test(perspectival_gap_inter_institutional) :-
    % Verify the core perspectival gap between the two institutional actors.
    constraint_indexing:constraint_classification(fiber_optic_chip_tech, snare, context(agent_power(institutional), _, exit_options(constrained), _)),
    constraint_indexing:constraint_classification(fiber_optic_chip_tech, rope, context(agent_power(institutional), _, exit_options(arbitrage), _)),
    % This confirms that even with the same power atom, different exit options
    % driven by structural relationships (victim vs. beneficiary) yield different classifications.
    true.

test(analytical_view_is_tangled_rope) :-
    % The analytical view must match the declared constraint_claim.
    narrative_ontology:constraint_claim(fiber_optic_chip_tech, ClaimType),
    constraint_indexing:constraint_classification(fiber_optic_chip_tech, ClaimType, context(agent_power(analytical), _, _, _)).

test(tangled_rope_gate_validation) :-
    % Verify all three conditions for a Tangled Rope are met.
    narrative_ontology:constraint_beneficiary(fiber_optic_chip_tech, _),
    narrative_ontology:constraint_victim(fiber_optic_chip_tech, _),
    domain_priors:requires_active_enforcement(fiber_optic_chip_tech).

:- end_tests(fiber_optic_chip_tech_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   - Base Extractiveness (ε=0.55): Set high to reflect the immense geopolitical and economic leverage conferred by controlling a foundational next-generation technology. This isn't just about profit; it's about setting standards and creating dependency.
 *   - Suppression Score (0.65): The new method is reportedly simpler and cheaper. If it scales, it will structurally suppress the more complex and expensive silicon-based alternatives, not through force, but through superior economics and performance.
 *   - Theater Ratio (0.10): This is a functional technology with demonstrated results in a lab setting, not a "Potemkin" project. The claims are grounded in verifiable physics.
 *
 * PERSPECTIVAL GAP:
 *   The gap is stark and driven by inter-institutional conflict.
 *   - For the Chinese Tech Ecosystem (Beneficiary), the technology is a Rope: a liberating tool of coordination that enables innovation and breaks free from external constraints. The negative effective extraction (χ) reflects the subsidy it provides to their strategic goals.
 *   - For the Western Semiconductor Bloc (Victim), the technology is a Snare: a trap that nullifies their primary geopolitical lever (control over advanced fabrication) and forces them into a reactive position. The high effective extraction (χ) reflects the strategic advantage being drained from them.
 *   - For End Consumers (Powerless Victim), it is also a Snare, imposing the indirect costs of technological fragmentation and geopolitical rivalry.
 *
 * INTER-INSTITUTIONAL DYNAMICS:
 *   This story is a prime example of inter-institutional dynamics. Both primary agents are `institutional` actors. The system correctly differentiates them not by power level, but by their structural relationship (beneficiary vs. victim) and resulting exit options (`arbitrage` vs. `constrained`). This creates the Rope/Snare dichotomy that the analytical `tangled_rope` classification resolves. No directionality overrides are needed because the derivation from structural data works as intended.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification prevents a simplistic "innovation is always good" or "geopolitical competition is always bad" analysis. By classifying the constraint as a Tangled Rope from the analytical perspective, the system acknowledges the dual nature of the technology: it is both a genuine coordination mechanism that could advance global communications (the Rope aspect) and a powerful tool of asymmetric extraction and geopolitical leverage (the Snare aspect). It avoids mislabeling a strategic weapon as pure coordination.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_fiber_optic_chip_tech,
    'Can the "sugar figurine" fabrication process scale to mass production with high yield and reliability outside of a laboratory environment?',
    'Longitudinal data on production yields, device failure rates, and deployment costs from major telecommunication infrastructure projects over a 5-year period.',
    'If YES, it confirms the Tangled Rope classification and accelerates a global shift in the technology supply chain. If NO, the constraint degrades into a Piton: a celebrated but non-viable technology maintained for prestige.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(fiber_optic_chip_tech, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% This is a high-extraction constraint (ε=0.55 > 0.46), so temporal
% measurements are required to model its evolution from research to
% strategic asset.

% Theater ratio over time (stable and low):
narrative_ontology:measurement(foc_tr_t0, fiber_optic_chip_tech, theater_ratio, 0, 0.05).
narrative_ontology:measurement(foc_tr_t5, fiber_optic_chip_tech, theater_ratio, 5, 0.08).
narrative_ontology:measurement(foc_tr_t10, fiber_optic_chip_tech, theater_ratio, 10, 0.10).

% Extraction over time (shows accumulation of strategic value):
narrative_ontology:measurement(foc_ex_t0, fiber_optic_chip_tech, base_extractiveness, 0, 0.20).
narrative_ontology:measurement(foc_ex_t5, fiber_optic_chip_tech, base_extractiveness, 5, 0.45).
narrative_ontology:measurement(foc_ex_t10, fiber_optic_chip_tech, base_extractiveness, 10, 0.55).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type: This technology is a foundational component for next-gen
% communications and computing.
narrative_ontology:coordination_type(fiber_optic_chip_tech, global_infrastructure).

% Network relationships: This technology directly impacts the effectiveness of
% semiconductor sanctions and the race to define 6G standards.
narrative_ontology:affects_constraint(fiber_optic_chip_tech, sanctions_on_semiconductor_tech).
narrative_ontology:affects_constraint(fiber_optic_chip_tech, global_6g_standardization).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% No overrides are necessary for this constraint. The standard derivation
% chain, using the declared beneficiary/victim groups and the distinct
% exit_options (arbitrage vs. constrained) for the institutional actors,
% correctly computes the divergent directionality values that produce the
% Rope vs. Snare perspectival gap.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */