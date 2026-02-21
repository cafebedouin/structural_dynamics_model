% ============================================================================
% CONSTRAINT STORY: eu_russian_asset_freeze_2025
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-05-21
% ============================================================================

:- module(constraint_eu_russian_asset_freeze_2025, []).

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
    narrative_ontology:omega_variable/3.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: eu_russian_asset_freeze_2025
 *   human_readable: Indefinite freeze of Russian state assets by the EU
 *   domain: geopolitical/economic
 *
 * SUMMARY:
 *   Following the 2022 invasion of Ukraine, the European Union imposed sanctions
 *   that included freezing central bank and state-owned assets of the Russian
 *   Federation. As of late 2025, this freeze on over €210 billion of assets
 *   has been made indefinite, functioning as a powerful tool of economic and
 *   political leverage. The constraint is the legal and financial framework
 *   that immobilizes these assets, preventing their use by the Russian state.
 *
 * KEY AGENTS (by structural relationship):
 *   - Russian Federation State Entities: Primary target (institutional/trapped) — bears the full extraction of the asset immobilization.
 *   - European Union Bloc: Primary beneficiary (institutional/arbitrage) — uses the constraint as a foreign policy tool and coordinates member state action.
 *   - Ukrainian State: Secondary beneficiary (organized/constrained) — benefits from the pressure applied to its adversary, but lacks direct control over the constraint.
 *   - Powerless Russian actors: Secondary targets (powerless/trapped) — private citizens or businesses with assets inadvertently frozen.
 *   - Analytical Observer: Sees the dual nature of the constraint as both a coordination mechanism for EU policy and an extractive tool against Russia.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(eu_russian_asset_freeze_2025, 0.85).
domain_priors:suppression_score(eu_russian_asset_freeze_2025, 0.90).   % Structural property (raw, unscaled).
domain_priors:theater_ratio(eu_russian_asset_freeze_2025, 0.10).       % Piton detection (>= 0.70)

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(eu_russian_asset_freeze_2025, extractiveness, 0.85).
narrative_ontology:constraint_metric(eu_russian_asset_freeze_2025, suppression_requirement, 0.90).
narrative_ontology:constraint_metric(eu_russian_asset_freeze_2025, theater_ratio, 0.10).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(eu_russian_asset_freeze_2025, tangled_rope).

% --- Binary flags ---
domain_priors:requires_active_enforcement(eu_russian_asset_freeze_2025). % Required for Tangled Rope

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% These feed the directionality derivation chain: the engine computes
% d (directionality) from agent membership in these groups + exit_options.
%
% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(eu_russian_asset_freeze_2025, european_union_bloc).
narrative_ontology:constraint_beneficiary(eu_russian_asset_freeze_2025, ukrainian_state).
%
% Who bears disproportionate cost?
narrative_ontology:constraint_victim(eu_russian_asset_freeze_2025, russian_federation_state_entities).

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

% PERSPECTIVE 1: THE PRIMARY TARGET (RUSSIA)
% As the victim with trapped exit options, the derived directionality `d` is
% very high (~0.95), leading to a high effective extraction (χ) that qualifies
% the constraint as a Snare. The coordination benefit is irrelevant to them.
constraint_indexing:constraint_classification(eu_russian_asset_freeze_2025, snare,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(continental))).

% PERSPECTIVE 2: THE PRIMARY BENEFICIARY (EU)
% As the architect and primary beneficiary with arbitrage exit (they can change
% the policy), `d` is very low (~0.05), making effective extraction (χ) negative.
% From this perspective, it's a pure coordination tool (Rope) for foreign policy.
constraint_indexing:constraint_classification(eu_russian_asset_freeze_2025, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(continental))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER
% The analytical observer sees both the genuine coordination function (aligning
% 27 EU member states) and the severe asymmetric extraction. The presence of
% beneficiary, victim, and active enforcement flags, combined with high ε and
% suppression, classifies this as a canonical Tangled Rope.
constraint_indexing:constraint_classification(eu_russian_asset_freeze_2025, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 4: INTER-INSTITUTIONAL (UKRAINE)
% As a beneficiary, but one with constrained exit and less power than the EU,
% Ukraine perceives the constraint as a supportive coordination mechanism (Rope).
% We use a directionality override to capture the nuance that they benefit but
% have little control over the mechanism.
constraint_indexing:constraint_classification(eu_russian_asset_freeze_2025, rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 5: THE POWERLESS INDIVIDUAL
% A private Russian citizen or small business owner with assets inadvertently
% frozen. From their perspective (powerless, trapped), the constraint is an
% absolute, inescapable Snare with no discernible coordination benefit.
constraint_indexing:constraint_classification(eu_russian_asset_freeze_2025, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(eu_russian_asset_freeze_2025_tests).

test(perspectival_gap_target_vs_beneficiary) :-
    % Verify the core perspectival gap between Russia and the EU.
    constraint_indexing:constraint_classification(eu_russian_asset_freeze_2025, snare, context(agent_power(institutional), _, exit_options(trapped), _)),
    constraint_indexing:constraint_classification(eu_russian_asset_freeze_2025, rope, context(agent_power(institutional), _, exit_options(arbitrage), _)).

test(analytical_view_is_tangled_rope) :-
    % Verify the analytical observer correctly identifies the hybrid nature.
    constraint_indexing:constraint_classification(eu_russian_asset_freeze_2025, tangled_rope, context(agent_power(analytical), _, _, _)).

test(tangled_rope_structural_check) :-
    % A constraint can only be a Tangled Rope if it has all three structural markers.
    narrative_ontology:constraint_beneficiary(eu_russian_asset_freeze_2025, _),
    narrative_ontology:constraint_victim(eu_russian_asset_freeze_2025, _),
    domain_priors:requires_active_enforcement(eu_russian_asset_freeze_2025).

test(powerless_perspective_is_snare) :-
    % Verify the powerless agent correctly perceives the constraint as a snare.
    constraint_indexing:constraint_classification(eu_russian_asset_freeze_2025, snare, context(agent_power(powerless), _, _, _)).

:- end_tests(eu_russian_asset_freeze_2025_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   - Base Extractiveness (ε=0.85): High, representing the near-total loss of control over €210bn in assets. The value is not destroyed, but its utility is extracted from the target.
 *   - Suppression (0.90): Extremely high. The legal and financial power of the EU bloc makes circumvention or legal challenge nearly impossible for the target. Alternatives are effectively suppressed.
 *   - Theater (0.10): Low. This is a functional economic constraint with direct, material consequences, not a symbolic or performative gesture.
 *
 * PERSPECTIVAL GAP:
 *   The gap is profound. For the Russian Federation, this is a pure Snare—a coercive seizure of assets with no redeeming coordination benefit. For the EU, it is a textbook Rope—a mechanism to coordinate the foreign policy of 27 sovereign nations toward a common goal, with the extractive element being the intended function, not a bug. The analytical perspective must classify it as a Tangled Rope to capture this duality, acknowledging both the coordination function and the asymmetric extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   The declarations are straightforward: the `european_union_bloc` and `ukrainian_state` are the beneficiaries of the pressure this constraint applies. The `russian_federation_state_entities` are the unambiguous victims, bearing the cost. This structural reality directly feeds the directionality derivation `d`, which in turn drives the wildly different perspectival calculations of effective extraction `χ`.
 *
 * MANDATROPHY ANALYSIS: [RESOLVED MANDATROPHY]
 *   This case is a prime example of where Mandatrophy is avoided. A naive analysis might label this simply as "sanctions" (implying a Rope) or "asset theft" (implying a Snare). The Deferential Realism framework, by requiring indexed perspectives, forces the analyst to see both faces of the coin. The `tangled_rope` classification correctly identifies that this is a system that *simultaneously* solves a coordination problem for one group while imposing a highly extractive reality on another.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% /5 form: narrative detail for story context
omega_variable(
    omega_eu_russian_asset_freeze_2025,
    'Will the frozen assets eventually be confiscated and transferred to Ukraine for reconstruction, or will they remain frozen as a bargaining chip for a future peace settlement?',
    'A formal legal decision by the EU Council or a judgment from the European Court of Justice clarifying the ultimate disposition of the assets.',
    'If confiscated (True), the extraction becomes permanent, solidifying the Snare classification for the target. If used as a bargaining chip (False), it retains a coercive but potentially reversible quality, reinforcing the Tangled Rope nature of geopolitical leverage.',
    confidence_without_resolution(low)
).

% /3 form: typed classification for reporting engine (REQUIRED)
narrative_ontology:omega_variable(omega_eu_russian_asset_freeze_2025, conceptual, 'Ultimate disposition of frozen assets (confiscation vs bargaining chip).').

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(eu_russian_asset_freeze_2025, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Temporal data for this high-extraction constraint (ε > 0.46).
% The timeline models the period from the initial 2022 sanctions to the
% 2025 "indefinite" legal solidification.
%
% Theater ratio over time (low and stable):
narrative_ontology:measurement(eu_russian_asset_freeze_2025_tr_t0, eu_russian_asset_freeze_2025, theater_ratio, 0, 0.15).
narrative_ontology:measurement(eu_russian_asset_freeze_2025_tr_t5, eu_russian_asset_freeze_2025, theater_ratio, 5, 0.12).
narrative_ontology:measurement(eu_russian_asset_freeze_2025_tr_t10, eu_russian_asset_freeze_2025, theater_ratio, 10, 0.10).

% Extraction over time (starts high and slightly increases as legal loopholes are closed):
narrative_ontology:measurement(eu_russian_asset_freeze_2025_ex_t0, eu_russian_asset_freeze_2025, base_extractiveness, 0, 0.80).
narrative_ontology:measurement(eu_russian_asset_freeze_2025_ex_t5, eu_russian_asset_freeze_2025, base_extractiveness, 5, 0.82).
narrative_ontology:measurement(eu_russian_asset_freeze_2025_ex_t10, eu_russian_asset_freeze_2025, base_extractiveness, 10, 0.85).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type: This constraint is fundamentally a mechanism to coordinate
% and enforce a unified policy across multiple independent states.
narrative_ontology:coordination_type(eu_russian_asset_freeze_2025, enforcement_mechanism).

% Network relationships: The status of these assets directly impacts potential
% funding mechanisms for Ukrainian reconstruction.
narrative_ontology:affects_constraint(eu_russian_asset_freeze_2025, ukraine_reconstruction_funding_2026).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% The default derivation for Ukraine (organized, constrained, beneficiary) might
% be ambiguous. We use an override to explicitly model their position as a
% beneficiary with limited agency, distinct from the EU's role as architect.
% A d-value of 0.20 places them clearly on the beneficiary side (d < 0.5) but
% acknowledges they are not the primary drivers of the policy.
constraint_indexing:directionality_override(eu_russian_asset_freeze_2025, organized, 0.20).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */