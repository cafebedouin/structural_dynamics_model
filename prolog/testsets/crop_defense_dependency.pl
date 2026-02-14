% ============================================================================
% CONSTRAINT STORY: crop_defense_dependency
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-05-21
% ============================================================================

:- module(constraint_crop_defense_dependency, []).

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
    constraint_indexing:directionality_override/3.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: crop_defense_dependency
 *   human_readable: "Dependency on External Pesticides due to Crop Biological Limitations"
 *   domain: technological
 *
 * SUMMARY:
 *   Most staple food crops (e.g., corn, wheat, rice) have been bred for yield at the
 *   expense of their natural defense mechanisms. This biological limitation creates a
 *   structural dependency on externally applied synthetic pesticides to ensure crop
 *   survival and productivity. This system, while enabling high-yield agriculture,
 *   creates significant economic extraction and environmental externalities.
 *
 * KEY AGENTS (by structural relationship):
 *   - smallholder_farmers: Primary target (powerless/trapped) — bear the direct cost of pesticides and are locked into the system.
 *   - agrochemical_corporations: Primary beneficiary (institutional/arbitrage) — profit from the sale of patented pesticides and seeds.
 *   - synthetic_biology_researchers: Analytical observer (analytical/analytical) — seeking to disrupt the constraint by engineering self-defending crops.
 *   - large_agribusiness: Secondary beneficiary (organized/mobile) — benefit from predictable yields but also seek to minimize input costs.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(crop_defense_dependency, 0.60).
domain_priors:suppression_score(crop_defense_dependency, 0.75).   % Structural property (raw, unscaled).
domain_priors:theater_ratio(crop_defense_dependency, 0.15).       % Piton detection (>= 0.70)

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(crop_defense_dependency, extractiveness, 0.60).
narrative_ontology:constraint_metric(crop_defense_dependency, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(crop_defense_dependency, theater_ratio, 0.15).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(crop_defense_dependency, tangled_rope).

% --- Binary flags ---
domain_priors:requires_active_enforcement(crop_defense_dependency). % Required for Tangled Rope

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% These feed the directionality derivation chain: the engine computes
% d (directionality) from agent membership in these groups + exit_options.
%
% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(crop_defense_dependency, agrochemical_corporations).
%
% Who bears disproportionate cost?
narrative_ontology:constraint_victim(crop_defense_dependency, smallholder_farmers).
%
% Gate requirements:
%   Tangled Rope: beneficiary + victim + requires_active_enforcement (all three)

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

% PERSPECTIVE 1: THE PRIMARY TARGET (SNARE)
% The smallholder farmer is trapped by high input costs and lack of alternatives.
% Engine derives d from: victim membership + trapped exit → d ≈ 0.95 → f(d) ≈ 1.42 → high χ
% χ = 0.60 * 1.42 * 1.0 (national scope) = 0.852. This is well into Snare territory (χ ≥ 0.66).
constraint_indexing:constraint_classification(crop_defense_dependency, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE PRIMARY BENEFICIARY (ROPE)
% The agrochemical corporation sees a stable, functional market it serves.
% Engine derives d from: beneficiary membership + arbitrage exit → d ≈ 0.05 → f(d) ≈ -0.12 → low/negative χ
% χ = 0.60 * (-0.12) * 1.2 (global scope) = -0.0864. This is a classic Rope classification.
constraint_indexing:constraint_classification(crop_defense_dependency, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% The researcher or systems analyst sees both the coordination function and the asymmetric extraction.
% Engine derives d ≈ 0.73 → f(d) ≈ 1.15 for analytical perspective.
% χ = 0.60 * 1.15 * 1.2 (global scope) = 0.828. This χ falls within the Tangled Rope range (0.40 ≤ χ ≤ 0.90),
% and the other conditions (ε ≥ 0.30, suppression ≥ 0.40) are met.
constraint_indexing:constraint_classification(crop_defense_dependency, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(crop_defense_dependency_tests).

test(perspectival_gap_snare_vs_rope, [nondet]) :-
    % Verify the core perspectival gap between target (snare) and beneficiary (rope).
    constraint_indexing:constraint_classification(crop_defense_dependency, snare, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(crop_defense_dependency, rope, context(agent_power(institutional), _, _, _)).

test(analytical_view_is_tangled_rope, [nondet]) :-
    % Verify the analytical classification is Tangled Rope.
    constraint_indexing:constraint_classification(crop_defense_dependency, tangled_rope, context(agent_power(analytical), _, _, _)).

test(tangled_rope_gate_conditions_met) :-
    % Verify that the structural conditions for a Tangled Rope are met.
    domain_priors:base_extractiveness(crop_defense_dependency, E), E >= 0.30,
    domain_priors:suppression_score(crop_defense_dependency, S), S >= 0.40,
    narrative_ontology:constraint_beneficiary(crop_defense_dependency, _),
    narrative_ontology:constraint_victim(crop_defense_dependency, _),
    domain_priors:requires_active_enforcement(crop_defense_dependency).

:- end_tests(crop_defense_dependency_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   - Base Extractiveness (ε=0.60): High, reflecting the significant profit margins on patented pesticides and the portion of farm revenue they consume.
 *   - Suppression Score (0.75): High, due to market consolidation in the agrochemical sector, strong IP protection (patents), and high regulatory barriers for novel alternatives. This creates a powerful moat that suppresses competition. Suppression is a raw structural property and is not scaled by context.
 *   - Theater Ratio (0.15): Low, because the pesticides are functionally effective at preventing crop loss. The system delivers on its primary promise, even if it has severe extractive side effects.
 *
 * PERSPECTIVAL GAP:
 *   The gap between the farmer (Snare) and the corporation (Rope) is profound.
 *   - The farmer experiences a coercive relationship. They must buy the product to avoid ruin, and the price is set by a powerful oligopoly. Their exit options are nil, hence a Snare.
 *   - The corporation views itself as providing an essential tool that solves a massive coordination problem (feeding the world) and stabilizes the food supply. From its perspective, it's a value-adding service, hence a Rope.
 *
 * DIRECTIONALITY LOGIC:
 *   The constraint's structure directs value upwards.
 *   - Beneficiaries: `agrochemical_corporations` who capture profits from the dependency. This informs the engine to derive a low directionality `d` for them, resulting in a low or negative effective extraction `χ`.
 *   - Victims: `smallholder_farmers` who bear the direct financial costs, and society at large which bears the environmental and health externalities. This informs the engine to derive a high `d` for them, resulting in a high `χ`.
 *
 * MANDATROPHY ANALYSIS:
 *   This is a classic Tangled Rope, and failing to recognize it as such leads to policy errors. Viewing it as a pure Snare (the farmer's view) misses the genuine coordination function that pesticides provide in modern agriculture, making it hard to develop viable alternatives. Viewing it as a pure Rope (the corporation's view) whitewashes the immense coercive extraction and structural power imbalance. The Tangled Rope classification correctly identifies both aspects are present and interdependent, which is essential for designing interventions (like the synthetic biology research in the article) that can replace the coordination function without recreating the extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% The primary uncertainty is whether the technological solution will break the dependency or just replace it.
omega_variable(
    omega_crop_defense_dependency,
    'Will the genetic "cassettes" for crop self-defense be deployed as open-source public goods or as a new generation of patented, controlled technologies?',
    'Tracking patent filings, licensing models, and the market structure of agricultural synthetic biology over the next 10-15 years.',
    'If deployed as public goods, the constraint could shift towards a Rope. If deployed as patented tech, it could become a new, more deeply embedded Snare or Tangled Rope.',
    confidence_without_resolution(low)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(crop_defense_dependency, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% The dependency on pesticides has intensified since the mid-20th century due to
% market consolidation and stronger IP regimes. Base extraction has increased.
% Required because base_extractiveness (0.60) > 0.46.

% Theater ratio over time (stable and low)
narrative_ontology:measurement(crop_defense_dependency_tr_t0, crop_defense_dependency, theater_ratio, 0, 0.15).
narrative_ontology:measurement(crop_defense_dependency_tr_t5, crop_defense_dependency, theater_ratio, 5, 0.15).
narrative_ontology:measurement(crop_defense_dependency_tr_t10, crop_defense_dependency, theater_ratio, 10, 0.15).

% Extraction over time (increasing due to market consolidation)
narrative_ontology:measurement(crop_defense_dependency_ex_t0, crop_defense_dependency, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(crop_defense_dependency_ex_t5, crop_defense_dependency, base_extractiveness, 5, 0.55).
narrative_ontology:measurement(crop_defense_dependency_ex_t10, crop_defense_dependency, base_extractiveness, 10, 0.60).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type: This system allocates a key technological resource.
narrative_ontology:coordination_type(crop_defense_dependency, resource_allocation).

% Network relationships: This dependency is structurally linked to the IP regime for seeds.
narrative_ontology:affects_constraint(crop_defense_dependency, seed_ip_regime).


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% No overrides needed. The structural derivation from beneficiary/victim
% declarations and exit options accurately models the power dynamics.


/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */