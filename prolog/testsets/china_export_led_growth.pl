% ============================================================================
% CONSTRAINT STORY: china_export_led_growth
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-05-21
% ============================================================================

:- module(constraint_china_export_led_growth, []).

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
 *   constraint_id: china_export_led_growth
 *   human_readable: China's State-Directed Export-Led Growth Model
 *   domain: economic/political
 *
 * SUMMARY:
 *   This constraint represents China's state-directed industrial policy, which
 *   prioritizes massive investment in manufacturing and export-led growth.
 *   This model suppresses domestic consumption to fuel production, creating
 *   large trade surpluses by exporting goods (e.g., EVs, solar panels) at
 *   prices that foreign competitors, operating without similar state backing,
 *   cannot match.
 *
 * KEY AGENTS (by structural relationship):
 *   - Foreign Manufacturers: Primary target (powerless/trapped) — faces subsidized competition, leading to market share loss and factory closures.
 *   - Chinese Domestic Consumers: Secondary target (powerless/trapped) — their consumption power is structurally suppressed to maximize savings and investment.
 *   - Chinese State & Exporters: Primary beneficiary (institutional/arbitrage) — benefits from economic growth, global market dominance, and geopolitical leverage.
 *   - US/EU Governments: Secondary target (institutional/constrained) — experiences industrial base erosion but is constrained by WTO rules and political costs of retaliation (e.g., tariffs).
 *   - Analytical Observer: Sees the full structure of coordination and extraction.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(china_export_led_growth, 0.48).
domain_priors:suppression_score(china_export_led_growth, 0.65).   % Structural property (raw, unscaled).
domain_priors:theater_ratio(china_export_led_growth, 0.20).       % Piton detection (>= 0.70)

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(china_export_led_growth, extractiveness, 0.48).
narrative_ontology:constraint_metric(china_export_led_growth, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(china_export_led_growth, theater_ratio, 0.20).

% --- NL Profile Metrics (required for mountain constraints) ---
% N/A for this constraint.

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(china_export_led_growth, tangled_rope).

% --- Binary flags ---
domain_priors:requires_active_enforcement(china_export_led_growth). % Required for Tangled Rope

% --- Emergence flag (required for mountain constraints) ---
% N/A for this constraint.

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% These feed the directionality derivation chain: the engine computes
% d (directionality) from agent membership in these groups + exit_options.
%
% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(china_export_led_growth, chinese_state_and_exporters).
%
% Who bears disproportionate cost?
narrative_ontology:constraint_victim(china_export_led_growth, foreign_manufacturers).
narrative_ontology:constraint_victim(china_export_led_growth, chinese_domestic_consumers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × f(d) × σ(S)
   where f(d) is the sigmoid directionality function:
     f(d) = -0.20 + 1.70 / (1 + e^(-6*(d - 0.50)))
   The engine derives d from beneficiary/victim membership + exit_options.
   Scope modifiers: local=0.8, regional=0.9, national=1.0,
                    continental=1.1, global=1.2, universal=1.0.
   CONTEXT ARITY: All context() terms must have exactly 4 arguments.
   Do not add measurement_basis, beneficiary/victim, or other metadata.
   Linter Rule 23 rejects files with context arity ≠ 4.
   ========================================================================== */

% PERSPECTIVE 1: THE PRIMARY TARGET (FOREIGN MANUFACTURER)
% Agent who bears the most extraction. Engine derives d from:
%   victim membership + trapped exit → d ≈ 0.95 → f(d) ≈ 1.42 → high χ
constraint_indexing:constraint_classification(china_export_led_growth, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: THE PRIMARY BENEFICIARY (CHINESE STATE)
% Agent who benefits most. Engine derives d from:
%   beneficiary membership + arbitrage exit → d ≈ 0.05 → f(d) ≈ -0.12 → low/negative χ
constraint_indexing:constraint_classification(china_export_led_growth, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER
% Default analytical context. With beneficiary, victim, and enforcement, the
% high ε and suppression scores lead to a Tangled Rope classification.
constraint_indexing:constraint_classification(china_export_led_growth, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% --- INTER-INSTITUTIONAL PERSPECTIVES (declare when applicable) ---
% The constraint operates between institutional actors with different
% structural relationships. The engine differentiates them via their
% victim/beneficiary status and exit options, producing different d values.

% PERSPECTIVE 4A: US/EU GOVERNMENTS (Institutional Target)
% As a victim with constrained exit options (tariffs are costly and politically
% difficult), this institutional actor experiences significant extraction.
% The derived d will be high for an institutional actor.
constraint_indexing:constraint_classification(china_export_led_growth, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 4B: CHINESE GOVERNMENT (Institutional Beneficiary)
% As the architect and beneficiary with arbitrage exit (full policy control),
% this actor experiences the constraint as pure coordination. This perspective
% is identical to Perspective 2 but is included here for direct comparison.
constraint_indexing:constraint_classification(china_export_led_growth, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(china_export_led_growth_tests).

test(perspectival_gap_target_beneficiary) :-
    % Verify perspectival gap between powerless target and institutional beneficiary.
    constraint_indexing:constraint_classification(china_export_led_growth, snare, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(china_export_led_growth, rope, context(agent_power(institutional), _, exit_options(arbitrage), _)).

test(perspectival_gap_inter_institutional) :-
    % Verify the gap between the two institutional actors.
    constraint_indexing:constraint_classification(china_export_led_growth, TypeTarget, context(agent_power(institutional), _, exit_options(constrained), _)),
    constraint_indexing:constraint_classification(china_export_led_growth, TypeBeneficiary, context(agent_power(institutional), _, exit_options(arbitrage), _)),
    (TypeTarget = tangled_rope ; TypeTarget = snare),
    TypeBeneficiary = rope,
    TypeTarget \= TypeBeneficiary.

test(tangled_rope_conditions_met) :-
    % Verify the analytical claim meets the Tangled Rope structural requirements.
    narrative_ontology:constraint_claim(china_export_led_growth, tangled_rope),
    narrative_ontology:constraint_beneficiary(china_export_led_growth, _),
    narrative_ontology:constraint_victim(china_export_led_growth, _),
    domain_priors:requires_active_enforcement(china_export_led_growth).

:- end_tests(china_export_led_growth_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   - Base Extractiveness (ε=0.48): Represents a significant, structural transfer of economic value from foreign industrial bases and suppressed Chinese domestic consumption towards China's export sector. It's not 1:1 theft, but a systemic distortion of capital allocation, hence the high but not extreme value.
 *   - Suppression Score (S=0.65): High. The model's success depends on suppressing alternatives: foreign firms cannot compete with state-backed pricing, and domestic policy limits the growth of a consumption-based economy. WTO rules and supply chain dependencies also suppress effective international responses.
 *   - Theater Ratio (T=0.20): Low. While there is official rhetoric about "win-win cooperation" and "high-quality development," the underlying economic mechanism is highly functional and non-performative.
 *
 * PERSPECTIVAL GAP:
 *   The gap is profound. For a foreign auto manufacturer, this is a Snare; their business is directly threatened by a flood of subsidized goods they cannot compete with. For the Chinese state, it's a Rope; a highly effective tool for coordinating national resources to achieve strategic goals of technological leadership and economic growth. This disagreement is not a matter of opinion but of structural position.
 *
 * DIRECTIONALITY LOGIC:
 *   - Beneficiary: `chinese_state_and_exporters`. They gain market share, profits, and geopolitical power. Their `arbitrage` exit option reflects their control over the policy levers.
 *   - Victims: `foreign_manufacturers` and `chinese_domestic_consumers`. They bear the direct and indirect costs. Foreign firms lose revenue; domestic consumers have their potential quality of life traded for national industrial capacity. Their `trapped` exit status reflects their inability to escape these effects.
 *
 * INTER-INSTITUTIONAL DYNAMICS:
 *   This is a key feature of the constraint. Both the US/EU governments and the Chinese government are `institutional` actors, but their experiences diverge based on their structural relationship to the constraint.
 *   - China: As the beneficiary with `arbitrage` exit, it experiences the constraint as a Rope.
 *   - US/EU: As a victim with `constrained` exit (retaliation is possible but costly), it experiences the constraint as a Tangled Rope. The system is extracting from their industrial base, but they are not entirely powerless, creating a hybrid classification. This measurable gap in effective extraction (χ) between two institutional peers is the central insight of the inter-institutional analysis.
 *
 * MANDATROPHY ANALYSIS:
 *   This framework correctly identifies the dual nature of the policy as a Tangled Rope. A simpler analysis might label it purely as a Snare (ignoring its powerful internal coordination function for China) or a Rope (ignoring the massive negative externalities imposed on others). The Tangled Rope classification captures both aspects, preventing mischaracterization and providing a more accurate model of the system's dynamics.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    omega_china_export_led_growth,
    'Is the suppression of domestic consumption a deliberate policy choice to fuel exports, or an emergent, unintended consequence of an investment-heavy growth model?',
    'Analysis of internal CCP economic planning documents and comparison with private statements from policymakers.',
    'If deliberate, the constraint is more extractive (closer to a pure Snare). If emergent, it is a more complex systemic imbalance (a classic Tangled Rope).',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(china_export_led_growth, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% This model has intensified over the last decade as China moved into higher-value
% industries. Temporal data tracks this accumulation of extractive potential.
% Interval: 2015 (t=0) to 2025 (t=10).

% Theater ratio over time (slight increase as international scrutiny grew):
narrative_ontology:measurement(china_export_led_growth_tr_t0, china_export_led_growth, theater_ratio, 0, 0.10).
narrative_ontology:measurement(china_export_led_growth_tr_t5, china_export_led_growth, theater_ratio, 5, 0.15).
narrative_ontology:measurement(china_export_led_growth_tr_t10, china_export_led_growth, theater_ratio, 10, 0.20).

% Extraction over time (intensified as China dominated more advanced sectors):
narrative_ontology:measurement(china_export_led_growth_ex_t0, china_export_led_growth, base_extractiveness, 0, 0.40).
narrative_ontology:measurement(china_export_led_growth_ex_t5, china_export_led_growth, base_extractiveness, 5, 0.45).
narrative_ontology:measurement(china_export_led_growth_ex_t10, china_export_led_growth, base_extractiveness, 10, 0.48).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% This policy is a textbook case of state-level resource allocation.
narrative_ontology:coordination_type(china_export_led_growth, resource_allocation).

% Network relationships (structural influence edges)
% This policy directly impacts competition in specific global markets.
narrative_ontology:affects_constraint(china_export_led_growth, global_ev_market_competition).
narrative_ontology:affects_constraint(china_export_led_growth, global_solar_panel_supply).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% No overrides are necessary for this constraint. The structural derivation
% chain, using the declared beneficiary/victim groups combined with the
% different exit_options for the institutional actors (constrained vs.
% arbitrage), accurately captures the directionality dynamics.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */