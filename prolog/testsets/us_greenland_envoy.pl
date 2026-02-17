% ============================================================================
% CONSTRAINT STORY: us_greenland_envoy
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-05-21
% ============================================================================

:- module(constraint_us_greenland_envoy, []).

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
    domain_priors:emerges_naturally/1,
    narrative_ontology:human_readable/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: us_greenland_envoy
 *   human_readable: US Special Envoy for Greenlandic Affairs
 *   domain: geopolitical/economic
 *
 * SUMMARY:
 *   Following a public expression of interest in purchasing Greenland, a US
 *   administration appoints a "special envoy for Greenlandic affairs".
 *   Ostensibly for "deepening partnership and economic development," the
 *   move is widely seen as a mechanism to secure preferential access to
 *   Greenland's vast rare earth mineral deposits and strategic location,
 *   while countering Chinese and Russian influence in the Arctic. The
 *   constraint is the diplomatic and economic pressure exerted through
 *   this special channel.
 *
 * KEY AGENTS (by structural relationship):
 *   - Greenlandic Government & People: Primary target (powerless/trapped) — possesses resources sought, faces intense pressure.
 *   - US Administration & Strategic Industries: Primary beneficiary (institutional/arbitrage) — seeks resources and geopolitical advantage.
 *   - Danish Government: Constrained institutional actor (institutional/constrained) — caught between its obligations to Greenland and its alliance with the US.
 *   - Analytical Observer: External analyst — sees the coordination narrative and the underlying extractive function.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(us_greenland_envoy, 0.65).
domain_priors:suppression_score(us_greenland_envoy, 0.75).   % Structural property (raw, unscaled).
domain_priors:theater_ratio(us_greenland_envoy, 0.40).       % Piton detection (>= 0.70)

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(us_greenland_envoy, extractiveness, 0.65).
narrative_ontology:constraint_metric(us_greenland_envoy, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(us_greenland_envoy, theater_ratio, 0.40).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(us_greenland_envoy, tangled_rope).
narrative_ontology:human_readable(us_greenland_envoy, "US Special Envoy for Greenlandic Affairs").

% --- Binary flags ---
domain_priors:requires_active_enforcement(us_greenland_envoy). % Required for Tangled Rope

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% These feed the directionality derivation chain: the engine computes
% d (directionality) from agent membership in these groups + exit_options.
%
% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(us_greenland_envoy, us_strategic_interests).
%
% Who bears disproportionate cost?
narrative_ontology:constraint_victim(us_greenland_envoy, greenlandic_sovereignty).
narrative_ontology:constraint_victim(us_greenland_envoy, danish_sovereignty).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × f(d) × σ(S)
   where f(d) is the sigmoid directionality function:
     f(d) = -0.20 + 1.70 / (1 + e^(-6*(d - 0.50)))
   The engine derives d from beneficiary/victim membership + exit_options.
   Scope modifiers: local=0.8, regional=0.9, national=1.0,
                    continental=1.1, global=1.2, universal=1.0.
   CONTEXT ARITY: All context() terms must have exactly 4 arguments.
   Linter Rule 23 rejects files with context arity ≠ 4.
   ========================================================================== */

% PERSPECTIVE 1: GREENLAND (THE PRIMARY TARGET)
% As the target of intense geopolitical pressure with limited ability to resist,
% Greenland experiences the envoy as a highly coercive mechanism.
% Engine derives d from: victim + trapped exit → d ≈ 0.95 → f(d) ≈ 1.42 → high χ.
% χ ≈ 0.65 * 1.42 * 1.0 (national) = 0.923. This is well above the Snare threshold (χ ≥ 0.66).
constraint_indexing:constraint_classification(us_greenland_envoy, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: US ADMINISTRATION (THE PRIMARY BENEFICIARY)
% The US sees this as a low-cost, high-leverage diplomatic tool. It's pure coordination
% from their perspective, facilitating access and influence.
% Engine derives d from: beneficiary + arbitrage exit → d ≈ 0.05 → f(d) ≈ -0.12 → negative χ.
% χ ≈ 0.65 * -0.12 * 1.0 (national) = -0.078. This is firmly a Rope.
constraint_indexing:constraint_classification(us_greenland_envoy, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER
% The analyst sees both the coordination narrative (economic development) and the
% severe underlying extraction (resource access). This dual nature is the
% definition of a Tangled Rope.
% Engine derives d ≈ 0.72 → f(d) ≈ 1.15 for analytical perspective.
% χ ≈ 0.65 * 1.15 * 1.2 (global) = 0.897. This fits Tangled Rope (0.40 ≤ χ ≤ 0.90).
constraint_indexing:constraint_classification(us_greenland_envoy, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% --- INTER-INSTITUTIONAL PERSPECTIVE ---

% PERSPECTIVE 4: DANISH GOVERNMENT (CONSTRAINED INSTITUTION)
% Denmark is a powerful state but is trapped in a difficult position between its
% major security ally (US) and its autonomous territory (Greenland). It cannot
% easily exit its NATO obligations or its responsibilities to Greenland.
% Engine derives d from: victim + constrained exit → d ≈ 0.90 → f(d) ≈ 1.35 → high χ.
% χ ≈ 0.65 * 1.35 * 1.0 (national) = 0.8775. From Denmark's perspective, this is a
% coercive Snare that compromises its sovereignty and diplomatic freedom.
constraint_indexing:constraint_classification(us_greenland_envoy, snare,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(us_greenland_envoy_tests).

test(perspectival_gap_target_beneficiary) :-
    constraint_indexing:constraint_classification(us_greenland_envoy, snare, context(agent_power(powerless), _, exit_options(trapped), _)),
    constraint_indexing:constraint_classification(us_greenland_envoy, rope, context(agent_power(institutional), _, exit_options(arbitrage), _)),
    % This test verifies the core Rope/Snare gap.
    true.

test(perspectival_gap_inter_institutional) :-
    % Verify the gap between the two institutional actors.
    constraint_indexing:constraint_classification(us_greenland_envoy, TypeBeneficiary, context(agent_power(institutional), _, exit_options(arbitrage), _)),
    constraint_indexing:constraint_classification(us_greenland_envoy, TypeConstrained, context(agent_power(institutional), _, exit_options(constrained), _)),
    TypeBeneficiary = rope,
    TypeConstrained = snare,
    TypeBeneficiary \= TypeConstrained.

test(analytical_classification_is_tangled_rope) :-
    constraint_indexing:constraint_classification(us_greenland_envoy, tangled_rope, context(agent_power(analytical), _, _, _)).

test(tangled_rope_gate_requirements_met) :-
    % Verify that all three conditions for a Tangled Rope are present.
    narrative_ontology:constraint_beneficiary(us_greenland_envoy, _),
    narrative_ontology:constraint_victim(us_greenland_envoy, _),
    domain_priors:requires_active_enforcement(us_greenland_envoy).

:- end_tests(us_greenland_envoy_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   - Base Extractiveness (ε=0.65): High. The value of Greenland's rare earth minerals and strategic military position is orders of magnitude greater than the US investment in diplomatic envoys and promised aid. The potential for asymmetric value transfer is immense.
 *   - Suppression Score (S=0.75): High. The creation of a "special" high-level diplomatic channel effectively crowds out and suppresses Greenland's ability to form alternative partnerships, particularly with geopolitical rivals like China. It creates a coercive preferential relationship.
 *   - Theater Ratio (T=0.40): Moderate. While there is significant diplomatic theater about "partnership," the underlying function of securing access is very real and actively pursued. The theater serves to legitimate the extraction, not replace the function.
 *
 * PERSPECTIVAL GAP:
 *   The gap is extreme. The US (beneficiary) sees a low-cost, effective coordination mechanism (Rope) to achieve strategic goals. Greenland (target) and Denmark (constrained institution) see a highly coercive diplomatic maneuver that threatens their sovereignty and autonomy (Snare). This gap arises directly from the directionality of the constraint: it funnels value and strategic advantage from Greenland/Denmark to the US.
 *
 * DIRECTIONALITY LOGIC:
 *   - Beneficiary: `us_strategic_interests`. This includes the military-industrial complex and tech sectors reliant on rare earth minerals. They gain resources and geopolitical dominance.
 *   - Victims: `greenlandic_sovereignty` and `danish_sovereignty`. They bear the cost through loss of autonomy, pressure to accept unfavorable deals, and limitations on their foreign policy options. The declarations correctly map this asymmetric relationship.
 *
 * INTER-INSTITUTIONAL DYNAMICS:
 *   This is a classic inter-institutional case. Both the US and Denmark are `institutional` actors, but their relationship to the constraint is radically different due to their `exit_options`. The US has `arbitrage` (it can pursue other resources or use other diplomatic tools). Denmark has a `constrained` exit; it cannot easily abandon its NATO alliance with the US nor its constitutional responsibilities to Greenland. This lack of exit optionality places it in a victim position, causing it to perceive the constraint as a Snare, just as the less powerful Greenland does.
 *
 * MANDATROPHY ANALYSIS:
 *   This case demonstrates the power of the framework in preventing mandatrophy. A naive analysis might see the envoy's official mandate ("economic development") and classify this as a Rope or Scaffold. However, the high base extractiveness (ε) and suppression (S), combined with the perspectival analysis, reveals the true structure: a Tangled Rope. The system correctly identifies that the coordination function is a veneer for a primarily extractive purpose, preventing the mislabeling of coercion as cooperation.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    omega_us_greenland_envoy,
    'Is the "economic development" component genuine coordination or pure theater?',
    'Observing the terms of resulting investment deals: do they favor local Greenlandic enterprise and retain profits domestically, or do they primarily benefit US corporations with minimal local return?',
    'If genuine (significant local benefit), base extractiveness (ε) would decrease. If purely theatrical (extractive terms), ε would be even higher than 0.65.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(us_greenland_envoy, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% This is a high-extraction constraint (ε=0.65 > 0.46), so temporal data is required.
% The trajectory models the constraint intensifying over time: starting as diplomatic
% theater and evolving into overt pressure for resource extraction.

% Theater ratio over time (starts high, then decreases as the real function emerges):
narrative_ontology:measurement(us_greenland_envoy_tr_t0, us_greenland_envoy, theater_ratio, 0, 0.70).
narrative_ontology:measurement(us_greenland_envoy_tr_t5, us_greenland_envoy, theater_ratio, 5, 0.55).
narrative_ontology:measurement(us_greenland_envoy_tr_t10, us_greenland_envoy, theater_ratio, 10, 0.40).

% Extraction over time (starts lower, increases as demands become concrete):
narrative_ontology:measurement(us_greenland_envoy_ex_t0, us_greenland_envoy, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(us_greenland_envoy_ex_t5, us_greenland_envoy, base_extractiveness, 5, 0.58).
narrative_ontology:measurement(us_greenland_envoy_ex_t10, us_greenland_envoy, base_extractiveness, 10, 0.65).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type: The constraint's stated purpose is to coordinate investment,
% making it a resource allocation mechanism.
narrative_ontology:coordination_type(us_greenland_envoy, resource_allocation).

% Network relationships: This policy is a direct response to global resource
% competition and dependency on specific supply chains.
narrative_ontology:affects_constraint(global_rare_earth_dependency, us_greenland_envoy).
narrative_ontology:affects_constraint(us_greenland_envoy, china_arctic_presence).


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% No overrides are needed for this story. The structural derivation chain
% (beneficiary/victim declarations + exit options) accurately models the
% directionality for all key agents, including the distinct experiences
% of the two institutional actors (US and Denmark).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */