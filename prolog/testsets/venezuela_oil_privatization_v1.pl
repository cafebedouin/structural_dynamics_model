% ============================================================================
% CONSTRAINT STORY: venezuela_oil_privatization_v1
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-05-22
% ============================================================================

:- module(constraint_venezuela_oil_privatization_v1, []).

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
 *   constraint_id: venezuela_oil_privatization_v1
 *   human_readable: "Shadow Privatization of Venezuela's Oil Sector"
 *   domain: geopolitical/economic
 *
 * SUMMARY:
 *   In response to crippling U.S. sanctions and internal mismanagement, the
 *   Venezuelan government under President Maduro has initiated a covert,
 *   extra-legal privatization of the state oil company, PDVSA. National
 *   assets are being transferred to small, often unknown, private local and
 *   foreign firms without public oversight or a legal framework. This allows
 *   oil production to continue, providing the state with revenue, but does so
 *   through opaque deals that extract long-term national wealth for the
 *   benefit of connected elites and private operators.
 *
 * KEY AGENTS (by structural relationship):
 *   - Venezuelan Public & PDVSA Workers: Primary target (powerless/trapped) — bears the cost of lost national patrimony and precarious labor conditions.
 *   - New Private Operators: Primary beneficiary (powerful/arbitrage) — gain control of valuable assets at low cost with minimal oversight.
 *   - Maduro Government Officials: Secondary beneficiary (institutional/constrained) — maintain power and revenue streams by offloading operational risk, but are constrained by sanctions.
 *   - Analytical Observer: Sees the full structure of coordination coupled with extraction.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(venezuela_oil_privatization_v1, 0.75).
domain_priors:suppression_score(venezuela_oil_privatization_v1, 0.80).   % Structural property (raw, unscaled).
domain_priors:theater_ratio(venezuela_oil_privatization_v1, 0.40).       % Piton detection (>= 0.70)

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(venezuela_oil_privatization_v1, extractiveness, 0.75).
narrative_ontology:constraint_metric(venezuela_oil_privatization_v1, suppression_requirement, 0.80).
narrative_ontology:constraint_metric(venezuela_oil_privatization_v1, theater_ratio, 0.40).

% --- NL Profile Metrics (required for mountain constraints) ---
% N/A for this constraint.

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(venezuela_oil_privatization_v1, tangled_rope).

% --- Binary flags ---
domain_priors:requires_active_enforcement(venezuela_oil_privatization_v1). % Required for Tangled Rope

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% These feed the directionality derivation chain: the engine computes
% d (directionality) from agent membership in these groups + exit_options.

% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(venezuela_oil_privatization_v1, new_private_operators).
narrative_ontology:constraint_beneficiary(venezuela_oil_privatization_v1, maduro_government_officials).

% Who bears disproportionate cost?
narrative_ontology:constraint_victim(venezuela_oil_privatization_v1, venezuelan_public).
narrative_ontology:constraint_victim(venezuela_oil_privatization_v1, pdvsa_workers).

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

% PERSPECTIVE 1: THE VENEZUELAN PUBLIC (PRIMARY TARGET)
% As trapped victims, the engine derives a high d (≈0.95), leading to a high
% f(d) (≈1.42). This amplifies the already high base extraction (ε=0.75),
% resulting in a clear Snare classification. They experience the loss of
% national wealth with no recourse.
constraint_indexing:constraint_classification(venezuela_oil_privatization_v1, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: NEW PRIVATE OPERATORS (PRIMARY BENEFICIARY)
% As beneficiaries with arbitrage exit options, the engine derives a very
% low d (≈0.05), leading to a negative f(d) (≈-0.12). This makes the effective
% extraction (χ) negative. They perceive a highly efficient coordination
% mechanism (Rope) that unlocks immense value for them.
constraint_indexing:constraint_classification(venezuela_oil_privatization_v1, rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: THE MADURO GOVERNMENT (INTER-INSTITUTIONAL BENEFICIARY)
% As institutional beneficiaries, but with constrained exit due to sanctions,
% their derived d is low but higher than the private operators'. For them, this is a
% necessary Rope that solves the critical coordination problem of generating
% state revenue under extreme pressure, justifying the extractive component.
constraint_indexing:constraint_classification(venezuela_oil_privatization_v1, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: THE ANALYTICAL OBSERVER
% This perspective accounts for the full structure: the genuine coordination
% function (getting oil out when the state can't) AND the severe asymmetric
% extraction. It correctly identifies the constraint as a Tangled Rope.
constraint_indexing:constraint_classification(venezuela_oil_privatization_v1, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(venezuela_oil_privatization_v1_tests).

test(perspectival_gap_is_snare_vs_rope, [nondet]) :-
    % Verify the core perspectival gap between the trapped victim and the arbitrage beneficiary.
    constraint_indexing:constraint_classification(venezuela_oil_privatization_v1, snare, context(agent_power(powerless), _, exit_options(trapped), _)),
    constraint_indexing:constraint_classification(venezuela_oil_privatization_v1, rope, context(agent_power(powerful), _, exit_options(arbitrage), _)).

test(analytical_view_is_tangled_rope, [nondet]) :-
    % The ground truth classification from the analytical view must be Tangled Rope.
    constraint_indexing:constraint_classification(venezuela_oil_privatization_v1, tangled_rope, context(agent_power(analytical), _, _, _)).

test(tangled_rope_structural_gates_pass) :-
    % Verify that all three structural requirements for a Tangled Rope are present.
    narrative_ontology:constraint_beneficiary(venezuela_oil_privatization_v1, _),
    narrative_ontology:constraint_victim(venezuela_oil_privatization_v1, _),
    domain_priors:requires_active_enforcement(venezuela_oil_privatization_v1).

:- end_tests(venezuela_oil_privatization_v1_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   - Base Extractiveness (ε=0.75): Extremely high. The constraint facilitates the transfer of a nation's primary source of wealth to private hands with little to no public accountability or transparent return of value. This represents a massive extraction from the public commons.
 *   - Suppression Score (0.80): High. The process is extra-legal and opaque, actively suppressing legal and transparent alternatives. Public debate, legislative oversight, and worker organization are all effectively bypassed or repressed.
 *   - The combination of a real coordination need (keeping oil flowing under sanctions) and extreme extraction makes this a canonical Tangled Rope.
 *
 * PERSPECTIVAL GAP:
 *   The gap is extreme, spanning from Snare to Rope.
 *   - For the Venezuelan Public (Snare): They are trapped victims. They bear the full long-term cost of lost national assets and see none of the coordination benefits, only the extraction. The high `d` derived from their `trapped` status makes the effective extraction overwhelming.
 *   - For the New Private Operators (Rope): They are beneficiaries with arbitrage exit. They see a pure coordination opportunity to apply capital and expertise to unlock assets that were inaccessible. For them, the `d` is so low that effective extraction is negative; the system subsidizes them.
 *
 * DIRECTIONALITY LOGIC:
 *   - Beneficiaries: The `new_private_operators` and `maduro_government_officials` directly benefit. The former gain assets and profits; the latter maintain political power and revenue. This structural relationship drives their low `d` values.
 *   - Victims: The `venezuelan_public` and `pdvsa_workers` bear the cost. The public loses long-term national wealth, and workers face job insecurity under new management. This drives their high `d` value. The directionality engine correctly models this asymmetry.
 *
 * MANDATROPHY ANALYSIS: [RESOLVED MANDATROPHY]
 *   This case demonstrates the power of the Tangled Rope classification. A simpler model might label this a pure Snare, missing the crucial point that it *does* solve a genuine coordination problem for the Maduro government (survival under sanctions). Conversely, a naive analysis might see it as a "necessary" Rope, ignoring the massive, unaccountable extraction. The Tangled Rope classification correctly identifies that both functions are present and coupled, preventing mislabeling and capturing the true, tragic nature of the policy.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    omega_vopv1,
    'What is the ultimate destination and use of the revenue generated by these private operations?',
    'A full, independent, and public audit of PDVSA and state accounts, which is currently impossible.',
    'If revenue primarily funds state functions (food imports, infrastructure), the coordination component is stronger. If it is primarily captured by elites, the constraint is functionally closer to a pure Snare.',
    confidence_without_resolution(low)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(venezuela_oil_privatization_v1, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% This models the degradation of Venezuela's state-run oil sector into the
% current shadow privatization model, tracking the rise of extraction and theater.
% As a high-extraction constraint (ε > 0.46), temporal data is required.

% Theater ratio over time (metric_substitution):
narrative_ontology:measurement(vopv1_tr_t0, venezuela_oil_privatization_v1, theater_ratio, 0, 0.10).
narrative_ontology:measurement(vopv1_tr_t5, venezuela_oil_privatization_v1, theater_ratio, 5, 0.30).
narrative_ontology:measurement(vopv1_tr_t10, venezuela_oil_privatization_v1, theater_ratio, 10, 0.40).

% Extraction over time (extraction_accumulation):
narrative_ontology:measurement(vopv1_ex_t0, venezuela_oil_privatization_v1, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(vopv1_ex_t5, venezuela_oil_privatization_v1, base_extractiveness, 5, 0.60).
narrative_ontology:measurement(vopv1_ex_t10, venezuela_oil_privatization_v1, base_extractiveness, 10, 0.75).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% This constraint is fundamentally about allocating capital and operational
% rights over oil reserves.
narrative_ontology:coordination_type(venezuela_oil_privatization_v1, resource_allocation).

% Network relationships (structural influence edges)
% The shadow privatization is a direct, adaptive response to the pressure
% exerted by the U.S. sanctions regime.
narrative_ontology:affects_constraint(us_sanctions_venezuela, venezuela_oil_privatization_v1).


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% No overrides are necessary for this constraint. The standard derivation chain,
% using the declared beneficiary/victim groups and exit options, accurately
% models the directionality of extraction and benefit for all key agents.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */