% ============================================================================
% CONSTRAINT STORY: lavender_ai_targeting
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-06-19
% ============================================================================

:- module(constraint_lavender_ai_targeting, []).

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
 *   constraint_id: lavender_ai_targeting
 *   human_readable: "Lavender" AI-Powered Targeting System in Gaza
 *   domain: technological/military
 *
 * SUMMARY:
 *   "Lavender" is an AI system used by the Israeli military (IDF) to generate
 *   lists of suspected junior Hamas and Palestinian Islamic Jihad (PIJ)
 *   operatives for assassination. The system dramatically accelerates target
 *   generation but operates with a reported 10% error rate and is coupled
 *   with policies permitting high civilian casualties (15-20 civilians per
 *   junior militant). This creates a high-speed, high-extraction targeting
 *   mechanism where human oversight is minimal (20 seconds per target),
 *   transforming military decision-making into a mass data-processing task.
 *
 * KEY AGENTS (by structural relationship):
 *   - Junior Militants & Nearby Civilians: Primary targets (powerless/trapped) — bear lethal extraction, with no recourse or appeal against the AI's classification.
 *   - IDF Command Structure: Primary beneficiary (institutional/arbitrage) — gains unprecedented speed and scale in targeting, offloading cognitive and moral labor.
 *   - Human Intelligence Officers: Constrained institutional actors (institutional/constrained) — tasked with "rubber-stamping" AI decisions under immense time pressure, serving more as a procedural check than a substantive one.
 *   - Analytical Observer: External analyst (analytical/analytical) — sees the dual nature of the system as both a coordination tool for one side and an extractive snare for the other.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
% Extraction is the removal of life and autonomy, the highest form of extraction.
% A 10% error rate and permissive collateral damage rules justify a very high ε.
domain_priors:base_extractiveness(lavender_ai_targeting, 0.75).

% Suppression is extremely high. Targets cannot opt-out, appeal, or escape. The
% system suppresses the alternative of slow, deliberate, human-vetted targeting.
domain_priors:suppression_score(lavender_ai_targeting, 0.90).   % Structural property (raw, unscaled).

% Theater is low. The system is brutally functional. The human "check" is
% procedural but not performative in a way that obscures the core function.
domain_priors:theater_ratio(lavender_ai_targeting, 0.10).       % Piton detection (>= 0.70)

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(lavender_ai_targeting, extractiveness, 0.75).
narrative_ontology:constraint_metric(lavender_ai_targeting, suppression_requirement, 0.90).
narrative_ontology:constraint_metric(lavender_ai_targeting, theater_ratio, 0.10).

% --- NL Profile Metrics (required for mountain constraints) ---
% Not a mountain constraint.

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(lavender_ai_targeting, tangled_rope).
narrative_ontology:human_readable(lavender_ai_targeting, "\"Lavender\" AI-Powered Targeting System in Gaza").

% --- Binary flags ---
% This system is actively enforced by military action. Required for Tangled Rope.
domain_priors:requires_active_enforcement(lavender_ai_targeting).

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(lavender_ai_targeting, idf_command_structure).

% Who bears disproportionate cost?
narrative_ontology:constraint_victim(lavender_ai_targeting, designated_gaza_militants).
narrative_ontology:constraint_victim(lavender_ai_targeting, proximate_gaza_civilians).

% Gate requirements:
%   Tangled Rope: beneficiary + victim + requires_active_enforcement (all three are present)

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
% Designated militants and nearby civilians are trapped within the system's logic.
% High ε and suppression, combined with victim status and trapped exit, create
% a near-perfect Snare. d -> 1.0, f(d) -> 1.42, χ is very high.
constraint_indexing:constraint_classification(lavender_ai_targeting, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: THE PRIMARY BENEFICIARY (ROPE)
% For the IDF command, this is a powerful coordination tool. It solves the
% problem of generating targets at scale. Beneficiary status and arbitrage exit
% (they can choose to use it, modify it, or not) drive d -> 0.0, f(d) -> -0.12,
% resulting in negative effective extraction (a subsidy).
constraint_indexing:constraint_classification(lavender_ai_targeting, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% The observer sees both the coordination function and the asymmetric extraction.
% It has a real coordination purpose (beneficiary exists) and a brutal extractive
% one (victims exist), with active enforcement. This is the definition of a
% Tangled Rope.
constraint_indexing:constraint_classification(lavender_ai_targeting, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% --- INTER-INSTITUTIONAL PERSPECTIVE ---

% PERSPECTIVE 4: THE CONSTRAINED OPERATOR (ROPE)
% The human intelligence officer is part of the beneficiary institution but
% experiences the system differently. Their exit options are constrained
% (defy orders, leave the unit), not arbitrage. The system coordinates their
% workflow but removes their agency. From this view, it's still a Rope, but a
% more coercive one than for command. The derived 'd' will be higher than for
% the beneficiary with arbitrage, but still low enough to be a Rope.
constraint_indexing:constraint_classification(lavender_ai_targeting, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).


/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(lavender_ai_targeting_tests).

test(perspectival_gap_target_beneficiary) :-
    constraint_indexing:constraint_classification(lavender_ai_targeting, snare, context(agent_power(powerless), _, exit_options(trapped), _)),
    constraint_indexing:constraint_classification(lavender_ai_targeting, rope, context(agent_power(institutional), _, exit_options(arbitrage), _)),
    true.

test(analytical_view_is_tangled_rope) :-
    constraint_indexing:constraint_classification(lavender_ai_targeting, tangled_rope, context(agent_power(analytical), _, _, _)).

test(inter_institutional_view_is_rope) :-
    constraint_indexing:constraint_classification(lavender_ai_targeting, rope, context(agent_power(institutional), _, exit_options(constrained), _)).

test(threshold_validation_high_extraction) :-
    narrative_ontology:constraint_metric(lavender_ai_targeting, extractiveness, E),
    E >= 0.46,
    narrative_ontology:constraint_metric(lavender_ai_targeting, suppression_requirement, S),
    S >= 0.60.

:- end_tests(lavender_ai_targeting_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness (ε=0.75) is set extremely high because the system's
 *   output is the literal extraction of human life, with a significant and
 *   accepted error rate. Suppression (0.90) is also maximal, as targets have
 *   no means to contest or escape the AI's designation. The theater ratio is
 *   low (0.10) because the human "rubber stamp" is a procedural formality
 *   that does not obscure the system's primary, lethal function.
 *
 * PERSPECTIVAL GAP:
 *   The gap is stark. For the IDF command (beneficiary), Lavender is a Rope—an
 *   incredibly efficient tool for coordinating military resources (airstrikes)
 *   to solve a complex problem (target identification). For the designated
 *   targets (victims), it is a Snare—an inescapable, arbitrary, and lethal trap.
 *   This is the classic signature of a Tangled Rope: what one agent sees as
 *   pure coordination, another experiences as pure extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   - Beneficiary: The `idf_command_structure` benefits from a massive increase in
 *     operational tempo and a reduction in the manpower required for targeting.
 *     This group has `arbitrage` exit options.
 *   - Victims: `designated_gaza_militants` and `proximate_gaza_civilians` bear
 *     the ultimate cost. Their membership in the victim group and `trapped`
 *     status drives their directionality `d` towards 1.0.
 *   - The structural data (beneficiary, victim, enforcement) correctly model the
 *     asymmetry, allowing the engine to derive the different classifications.
 *
 * INTER-INSTITUTIONAL DYNAMICS:
 *   The human intelligence officer's perspective is crucial. They are part of
 *   the beneficiary institution but have `constrained` exit options, not
 *   `arbitrage`. The system dictates their workflow, reducing their role to
 *   a 20-second check. This constraint on their professional judgment and agency
 *   is a key feature of the system's design. While it still functions as a Rope
 *   for them (coordinating their actions), it highlights an internal tension and
 *   coercion even within the beneficiary institution.
 *
 * MANDATROPHY ANALYSIS: [RESOLVED MANDATROPHY]
 *   Classifying this system as a Tangled Rope is essential. A naive analysis might
 *   call it a pure Snare, ignoring its powerful coordination function for the user.
 *   Recognizing the Rope component is key to understanding why such systems are
 *   built and deployed: they offer a genuine, compelling benefit to the institution
 *   that wields them. The `tangled_rope` classification captures this duality,
 *   preventing mischaracterization and forcing an analysis of how coordination for one
 *   group is directly coupled to lethal extraction from another.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_lavender_ai_targeting,
    'What is the true, independently verified false-positive rate of the Lavender system?',
    'Auditing of the system''s training data, algorithms, and a statistical analysis of all individuals it designated as targets versus ground truth.',
    'If the rate is near the claimed 10%, the current ε=0.75 holds. If the rate is significantly higher (e.g., 30-50%), ε would approach 0.90-0.95, making the Snare aspect even more dominant. If it were near zero, the argument for it being a pure coordination tool would be stronger, though the collateral damage policy would still imply high extraction.',
    confidence_without_resolution(low)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(lavender_ai_targeting, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Required for high-extraction constraints (base_extractiveness > 0.46).
% Models the system's deployment and intensification over a conflict period.
% At T=0, the system is in development/limited use (lower ε). By T=10, it is
% fully deployed for mass targeting (high ε). Theater remains low throughout.

% Theater ratio over time (stable and low):
narrative_ontology:measurement(lavender_tr_t0, lavender_ai_targeting, theater_ratio, 0, 0.10).
narrative_ontology:measurement(lavender_tr_t5, lavender_ai_targeting, theater_ratio, 5, 0.10).
narrative_ontology:measurement(lavender_tr_t10, lavender_ai_targeting, theater_ratio, 10, 0.10).

% Extraction over time (ramps up with deployment):
narrative_ontology:measurement(lavender_ex_t0, lavender_ai_targeting, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(lavender_ex_t5, lavender_ai_targeting, base_extractiveness, 5, 0.60).
narrative_ontology:measurement(lavender_ex_t10, lavender_ai_targeting, base_extractiveness, 10, 0.75).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type: The system allocates kinetic force (munitions) to targets.
narrative_ontology:coordination_type(lavender_ai_targeting, resource_allocation).

% Network relationships: This system is both influenced by and influences other
% constraints in the domain.
narrative_ontology:affects_constraint(israeli_military_doctrine_dahiya, lavender_ai_targeting). % Doctrine of disproportionate force enables permissive rules.
narrative_ontology:affects_constraint(lavender_ai_targeting, humanitarian_aid_access_gaza). % Widespread targeting degrades infrastructure and safety needed for aid.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% No overrides are needed for this constraint. The automatic derivation from
% beneficiary/victim status and the different exit_options (arbitrage vs.
% constrained vs. trapped) accurately captures the directionality for all key agents.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */