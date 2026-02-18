% ============================================================================
% CONSTRAINT STORY: us_legal_standard_reasonable_doubt
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-05-21
% ============================================================================

:- module(constraint_us_legal_standard_reasonable_doubt, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: us_legal_standard_reasonable_doubt
 *   human_readable: The US Legal Standard of "Guilt Beyond a Reasonable Doubt" in Political Prosecutions
 *   domain: political/legal
 *
 * SUMMARY:
 *   This constraint models the legal principle of "guilty beyond a reasonable
 *   doubt" as applied to a high-profile political figure (Donald Trump)
 *   following the events of January 6th, 2021. The principle is designed as a
 *   safeguard for the accused (a coordination function for justice), but in a
 *   hyper-polarized environment, its application is perceived by opponents as
 *   a politically motivated extractive tool. This creates a significant
 *   perspectival gap between those upholding the legal system and those
 *   targeted by it.
 *
 * KEY AGENTS (by structural relationship):
 *   - The Targeted Political Movement: Primary target (organized/constrained) — bears the direct legal and political costs of prosecution under this standard.
 *   - Supporters of the Movement: Secondary victims (powerless/trapped) — experience the legal action as an attack on their political identity and worldview.
 *   - The State/Judiciary: Primary beneficiary (institutional/arbitrage) — uses the standard to enforce laws, maintain order, and legitimize its actions.
 *   - Analytical Observer: (analytical/analytical) — sees both the coordination function and the extractive application.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(us_legal_standard_reasonable_doubt, 0.48).
domain_priors:suppression_score(us_legal_standard_reasonable_doubt, 0.85).   % Structural property (raw, unscaled). High due to state monopoly on adjudication.
domain_priors:theater_ratio(us_legal_standard_reasonable_doubt, 0.20).       % Piton detection (< 0.70). The legal process is functional, despite political theater.

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(us_legal_standard_reasonable_doubt, extractiveness, 0.48).
narrative_ontology:constraint_metric(us_legal_standard_reasonable_doubt, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(us_legal_standard_reasonable_doubt, theater_ratio, 0.20).

% --- NL Profile Metrics (required for mountain constraints) ---
% N/A. This is a human-constructed legal principle.

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(us_legal_standard_reasonable_doubt, tangled_rope).
narrative_ontology:topic_domain(us_legal_standard_reasonable_doubt, "political/legal").
narrative_ontology:human_readable(us_legal_standard_reasonable_doubt, "The US Legal Standard of \"Guilt Beyond a Reasonable Doubt\" in Political Prosecutions").

% --- Binary flags ---
domain_priors:requires_active_enforcement(us_legal_standard_reasonable_doubt). % Required for Tangled Rope

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% These feed the directionality derivation chain.
% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(us_legal_standard_reasonable_doubt, the_state_judiciary).
%
% Who bears disproportionate cost?
narrative_ontology:constraint_victim(us_legal_standard_reasonable_doubt, targeted_political_movement).

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

% PERSPECTIVE 1: THE POWERLESS SUPPORTER (SNARE)
% Experiences the legal system as a coercive, illegitimate force targeting
% their political identity. Victim membership + trapped exit → d ≈ 0.95 → high χ.
constraint_indexing:constraint_classification(us_legal_standard_reasonable_doubt, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE PRIMARY BENEFICIARY / THE STATE (ROPE)
% The judiciary and state actors see the standard as a fundamental coordination
% mechanism for ensuring fair trials and maintaining legal order.
% Beneficiary membership + arbitrage exit → d ≈ 0.05 → low/negative χ.
constraint_indexing:constraint_classification(us_legal_standard_reasonable_doubt, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: THE ORGANIZED TARGET (TANGLED ROPE)
% The political movement's leadership recognizes the legal standard's legitimacy
% in principle (coordination) but experiences its specific application as a
% direct political attack (extraction).
constraint_indexing:constraint_classification(us_legal_standard_reasonable_doubt, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% Default analytical context. Recognizes the dual nature of the constraint:
% a genuine coordination principle (Rope) that is also a vehicle for
% asymmetric extraction in a political conflict. This is the definition of
% a Tangled Rope.
constraint_indexing:constraint_classification(us_legal_standard_reasonable_doubt, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).


/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(us_legal_standard_reasonable_doubt_tests).

test(perspectival_gap_target_beneficiary) :-
    % Verify the core perspectival gap between the powerless victim and the institutional beneficiary.
    constraint_indexing:constraint_classification(us_legal_standard_reasonable_doubt, snare, context(agent_power(powerless), _, exit_options(trapped), _)),
    constraint_indexing:constraint_classification(us_legal_standard_reasonable_doubt, rope, context(agent_power(institutional), _, exit_options(arbitrage), _)),
    true.

test(analytical_view_is_tangled_rope) :-
    % The analytical observer must see the hybrid nature of the constraint.
    constraint_indexing:constraint_classification(us_legal_standard_reasonable_doubt, tangled_rope, context(agent_power(analytical), _, _, _)).

test(tangled_rope_structural_gates_pass) :-
    % Verify that all three conditions for Tangled Rope are met.
    narrative_ontology:constraint_beneficiary(us_legal_standard_reasonable_doubt, _),
    narrative_ontology:constraint_victim(us_legal_standard_reasonable_doubt, _),
    domain_priors:requires_active_enforcement(us_legal_standard_reasonable_doubt).

:- end_tests(us_legal_standard_reasonable_doubt_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   - Base Extractiveness (ε=0.48): This value is chosen to reflect the hybrid
 *     nature of the constraint. The legal standard itself has a low-extraction
 *     coordination purpose (protecting the innocent). However, its application
 *     in a political context to remove a rival has a very high extractive
 *     potential (loss of liberty, power, and status). 0.48 places it firmly
 *     in the Tangled Rope category.
 *   - Suppression (0.85): The state's legal system holds a near-total
 *     monopoly on adjudication and enforcement. There are no viable
 *     alternatives to engaging with it, making suppression extremely high.
 *
 * PERSPECTIVAL GAP:
 *   The gap is profound. For the state (institutional), the standard is a
 *   foundational Rope ensuring stability and justice. For the powerless
 *   supporter (powerless/trapped), it is a Snare, a weapon used by a hostile
 *   regime to eliminate their political champions. The analytical view and the
 *   organized target see the truth: it is both. It's a Tangled Rope, a legitimate
 *   coordination device that simultaneously functions as an extractive tool.
 *
 * DIRECTIONALITY LOGIC:
 *   - Beneficiary: `the_state_judiciary`. They are the architects and users
 *     of the system. The standard reinforces their power and legitimacy.
 *     Combined with `arbitrage` exit, this yields a low `d` and a Rope classification.
 *   - Victim: `targeted_political_movement`. They bear the direct costs.
 *     Combined with `trapped` or `constrained` exit, this yields a high `d`,
 *     leading to Snare or Tangled Rope classifications.
 *     The declarations directly map to the structural conflict described in the source article.
 *
 * MANDATROPHY ANALYSIS:
 *   This framework correctly identifies the dual-function nature of the
 *   constraint. A simpler model might be forced to label it as either a pure
 *   Rope (ignoring the political extraction) or a pure Snare (ignoring the
 *   genuine legal principle). The Tangled Rope classification, derived from
 *   the analytical perspective, captures the essential tension: a tool of
 *   justice is also being used as a weapon of political conflict. This avoids
 *   mislabeling and clarifies the source of social disagreement.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_us_legal_standard_reasonable_doubt,
    'Is the prosecution a good-faith application of law to perceived crimes, or a politically motivated weaponization of the legal system?',
    'Access to un-redacted internal communications and strategy documents from the Department of Justice, which is empirically inaccessible.',
    'If good-faith, the system is functioning as a Rope that feels like a Snare to its target. If politically motivated, it is a canonical Tangled Rope, where the coordination function provides cover for extraction.',
    confidence_without_resolution(low)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(us_legal_standard_reasonable_doubt, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% While not strictly required (ε < 0.50), the high-profile and dynamic nature
% of this constraint warrants temporal tracking. We model a gradual increase in
% both perceived extraction and political theater as the legal process becomes
% more entangled with electoral politics.

% Theater ratio over time:
narrative_ontology:measurement(us_legal_standard_reasonable_doubt_tr_t0, us_legal_standard_reasonable_doubt, theater_ratio, 0, 0.10).
narrative_ontology:measurement(us_legal_standard_reasonable_doubt_tr_t5, us_legal_standard_reasonable_doubt, theater_ratio, 5, 0.15).
narrative_ontology:measurement(us_legal_standard_reasonable_doubt_tr_t10, us_legal_standard_reasonable_doubt, theater_ratio, 10, 0.20).

% Extraction over time:
narrative_ontology:measurement(us_legal_standard_reasonable_doubt_ex_t0, us_legal_standard_reasonable_doubt, base_extractiveness, 0, 0.40).
narrative_ontology:measurement(us_legal_standard_reasonable_doubt_ex_t5, us_legal_standard_reasonable_doubt, base_extractiveness, 5, 0.45).
narrative_ontology:measurement(us_legal_standard_reasonable_doubt_ex_t10, us_legal_standard_reasonable_doubt, base_extractiveness, 10, 0.48).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
narrative_ontology:coordination_type(us_legal_standard_reasonable_doubt, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% No overrides are necessary for this constraint. The standard derivation
% chain based on beneficiary/victim declarations and exit options accurately
% models the structural relationships and generates the correct perspectival gaps.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */