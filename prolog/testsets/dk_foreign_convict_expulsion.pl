% ============================================================================
% CONSTRAINT STORY: dk_foreign_convict_expulsion
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-05-21
% ============================================================================

:- module(constraint_dk_foreign_convict_expulsion, []).

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
 *   constraint_id: dk_foreign_convict_expulsion
 *   human_readable: Denmark's Foreign Convict Expulsion Law
 *   domain: political/social
 *
 * SUMMARY:
 *   A Danish government policy mandating the expulsion of any foreign national
 *   sentenced to at least one year in prison. The law is framed as a measure
 *   to protect the country and its citizens by removing criminal elements,
 *   but it imposes extreme costs on the individuals affected, who lose their
 *   residency, social connections, and accumulated life capital in Denmark.
 *
 * KEY AGENTS (by structural relationship):
 *   - Convicted Foreign Nationals: Primary target (powerless/trapped) — bear the full extractive cost of expulsion.
 *   - Danish State / Nationalist Bloc: Primary beneficiary (institutional/arbitrage) — gains political capital and fulfills a populist mandate for security.
 *   - General Danish Public: Secondary beneficiary (organized/mobile) — promised increased safety and social order.
 *   - Analytical Observer: Sees the full structure, including both the coordination claims and the extractive reality.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dk_foreign_convict_expulsion, 0.75).
domain_priors:suppression_score(dk_foreign_convict_expulsion, 0.95).   % Structural property (raw, unscaled).
domain_priors:theater_ratio(dk_foreign_convict_expulsion, 0.40).       % Piton detection (>= 0.70)

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dk_foreign_convict_expulsion, extractiveness, 0.75).
narrative_ontology:constraint_metric(dk_foreign_convict_expulsion, suppression_requirement, 0.95).
narrative_ontology:constraint_metric(dk_foreign_convict_expulsion, theater_ratio, 0.40).

% --- NL Profile Metrics (required for mountain constraints) ---
% N/A for this constraint.

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(dk_foreign_convict_expulsion, tangled_rope).
narrative_ontology:human_readable(dk_foreign_convict_expulsion, "Denmark's Foreign Convict Expulsion Law").

% --- Binary flags ---
domain_priors:requires_active_enforcement(dk_foreign_convict_expulsion). % Required for Tangled Rope

% --- Emergence flag (required for mountain constraints) ---
% N/A for this constraint.

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% These feed the directionality derivation chain: the engine computes
% d (directionality) from agent membership in these groups + exit_options.

% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(dk_foreign_convict_expulsion, danish_state_nationalist_bloc).
narrative_ontology:constraint_beneficiary(dk_foreign_convict_expulsion, general_danish_public).
%
% Who bears disproportionate cost?
narrative_ontology:constraint_victim(dk_foreign_convict_expulsion, convicted_foreign_nationals).

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

% PERSPECTIVE 1: THE PRIMARY TARGET (SNARE)
% The convicted foreign national experiences this as pure, inescapable coercion.
% Engine derives d from: victim membership + trapped exit → d ≈ 0.95 → f(d) ≈ 1.42 → high χ.
% χ = 0.75 * 1.42 * 1.0 (national scope) ≈ 1.065.
% High ε (0.75), high suppression (0.95), and high χ (1.065) make this a clear Snare.
constraint_indexing:constraint_classification(dk_foreign_convict_expulsion, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE PRIMARY BENEFICIARY (ROPE)
% The Danish state views this as a tool for social management and security.
% Engine derives d from: beneficiary membership + arbitrage exit → d ≈ 0.05 → f(d) ≈ -0.12 → negative χ.
% χ = 0.75 * -0.12 * 1.0 (national scope) ≈ -0.09.
% Negative effective extraction means this is a subsidy from this perspective,
% classifying it as a Rope—a mechanism for achieving a coordination goal.
constraint_indexing:constraint_classification(dk_foreign_convict_expulsion, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% The analytical view sees both the coordination function and the asymmetric extraction.
% Engine derives d ≈ 0.73 → f(d) ≈ 1.15. Global scope amplifies χ.
% χ = 0.75 * 1.15 * 1.2 (global scope) ≈ 1.035.
% Because it possesses a genuine coordination function (beneficiary is declared),
% asymmetric extraction (victim is declared), and requires enforcement, the structure
% is a Tangled Rope, despite the very high extraction.
constraint_indexing:constraint_classification(dk_foreign_convict_expulsion, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(dk_foreign_convict_expulsion_tests).

test(perspectival_gap_is_snare_vs_rope) :-
    constraint_indexing:constraint_classification(dk_foreign_convict_expulsion, snare, context(agent_power(powerless), _, exit_options(trapped), _)),
    constraint_indexing:constraint_classification(dk_foreign_convict_expulsion, rope, context(agent_power(institutional), _, exit_options(arbitrage), _)).

test(analytical_view_is_tangled_rope) :-
    constraint_indexing:constraint_classification(dk_foreign_convict_expulsion, tangled_rope, context(agent_power(analytical), _, _, _)).

test(tangled_rope_structural_gates_pass) :-
    % A constraint is a Tangled Rope only if it has a beneficiary (coordination),
    % a victim (asymmetric extraction), and requires active enforcement.
    narrative_ontology:constraint_beneficiary(dk_foreign_convict_expulsion, _),
    narrative_ontology:constraint_victim(dk_foreign_convict_expulsion, _),
    domain_priors:requires_active_enforcement(dk_foreign_convict_expulsion).

:- end_tests(dk_foreign_convict_expulsion_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   - Base Extractiveness (ε=0.75): Extremely high, representing the complete
 *     removal of a person's life, social network, and economic standing within
 *     a country. This is a maximal form of social extraction short of execution.
 *   - Suppression (0.95): For the target, the constraint is absolute. Once the
 *     legal threshold is met, there are no alternatives to avoid expulsion.
 *   - Theater (0.40): The policy is functional, but its framing ("protect our
 *     country, not criminals") is highly theatrical and aimed at a domestic
 *     political audience, justifying its coercive nature.
 *
 * PERSPECTIVAL GAP:
 *   The gap is extreme. For the `convicted_foreign_national` (powerless, trapped),
 *   the policy is an inescapable Snare that destroys their life. For the
 *   `danish_state` (institutional, arbitrage), it is a Rope—a simple, effective
 *   coordination tool for enforcing social norms and delivering on political promises
 *   of security. The state does not experience the extraction; it orchestrates it.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality is unambiguous. The constraint is explicitly designed to
 *   benefit the `danish_state_nationalist_bloc` and the `general_danish_public`
 *   by extracting `convicted_foreign_nationals`. The beneficiary/victim
 *   declarations directly map to this clear structural relationship. The system's
 *   ability to derive directionality from these declarations is key to producing
 *   the correct perspectival classifications.
 *
 * MANDATROPHY ANALYSIS: [RESOLVED MANDATROPHY]
 *   This classification correctly avoids two potential errors. A naive "pro-state"
 *   analysis might label this a Rope, ignoring the severe, asymmetric extraction.
 *   A naive "pro-victim" analysis might label it a pure Snare, ignoring the genuine
 *   (from the state's perspective) coordination function it serves. The analytical
 *   classification of Tangled Rope correctly captures this dual nature: it is a
 *   mechanism of social ordering that is achieved through highly coercive and
 *   asymmetric extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    omega_dk_expulsion,
    'Does the expulsion policy produce a net increase in public safety, or is its primary function political theater and nationalist signaling?',
    'Comparative criminological studies analyzing crime rates before and after the policy, controlling for other variables, versus polling data on voter sentiment and political capital gained by its proponents.',
    'If True (it increases safety), the coordination function is robust, solidifying the Tangled Rope classification. If False (it is primarily theater), the constraint is closer to a pure Snare, with the coordination claim being a pretense.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(dk_foreign_convict_expulsion, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% This constraint has high base extraction (0.75 > 0.46), so temporal data is
% required. We model a scenario where the policy was introduced with a higher
% conviction threshold and less theatrical rhetoric, then intensified over time.

% Theater ratio over time (triggers metric_substitution detection):
narrative_ontology:measurement(dk_expulsion_tr_t0, dk_foreign_convict_expulsion, theater_ratio, 0, 0.20).
narrative_ontology:measurement(dk_expulsion_tr_t5, dk_foreign_convict_expulsion, theater_ratio, 5, 0.30).
narrative_ontology:measurement(dk_expulsion_tr_t10, dk_foreign_convict_expulsion, theater_ratio, 10, 0.40).

% Extraction over time (triggers extraction_accumulation detection):
narrative_ontology:measurement(dk_expulsion_ex_t0, dk_foreign_convict_expulsion, base_extractiveness, 0, 0.60).
narrative_ontology:measurement(dk_expulsion_ex_t5, dk_foreign_convict_expulsion, base_extractiveness, 5, 0.70).
narrative_ontology:measurement(dk_expulsion_ex_t10, dk_foreign_convict_expulsion, base_extractiveness, 10, 0.75).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% This constraint's coordination function is clearly about enforcing state-defined norms.
narrative_ontology:coordination_type(dk_foreign_convict_expulsion, enforcement_mechanism).

% This policy structurally affects and is affected by broader legal frameworks
% governing human rights and citizenship within the European context.
narrative_ontology:affects_constraint(dk_foreign_convict_expulsion, eu_human_rights_charter).
narrative_ontology:affects_constraint(dk_foreign_convict_expulsion, eu_freedom_of_movement).
narrative_ontology:affects_constraint(dk_citizenship_pathways, dk_foreign_convict_expulsion).


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% No overrides are necessary for this constraint. The automatic derivation chain,
% using the explicit beneficiary/victim declarations and the agents' exit options,
% accurately computes the directionality (d) for each perspective.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */