% ============================================================================
% CONSTRAINT STORY: finnish_ubi_experiment
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-05-21
% ============================================================================

:- module(constraint_finnish_ubi_experiment, []).

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
 *   constraint_id: finnish_ubi_experiment
 *   human_readable: Finnish Basic Income Experiment (2017-2018)
 *   domain: economic/social
 *
 * SUMMARY:
 *   Between 2017 and 2018, Finland conducted a nationwide basic income
 *   experiment. 2,000 unemployed individuals were given a guaranteed,
 *   unconditional monthly payment of €560. The experiment aimed to test
 *   whether a simplified social security model could reduce bureaucracy and
 *   incentivize employment by removing welfare traps. This constraint represents
 *   the structure, rules, and temporary nature of this policy intervention.
 *
 * KEY AGENTS (by structural relationship):
 *   - Unemployed Participants: Primary target (powerless/trapped) — subject to the experiment's rules, with no ability to opt-out.
 *   - Kela & Finnish Policymakers: Primary beneficiary (institutional/arbitrage) — gained valuable data, tested a policy hypothesis, and controlled the experiment's lifecycle.
 *   - Social Scientists: Analytical observer — sees the full structure, including its temporary nature and coordination goals.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(finnish_ubi_experiment, 0.20).
domain_priors:suppression_score(finnish_ubi_experiment, 0.35).   % Structural property (raw, unscaled). Participants could not opt-out.
domain_priors:theater_ratio(finnish_ubi_experiment, 0.15).       % A genuine experiment, not political theater.

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(finnish_ubi_experiment, extractiveness, 0.20).
narrative_ontology:constraint_metric(finnish_ubi_experiment, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(finnish_ubi_experiment, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
% N/A for this constraint type.

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(finnish_ubi_experiment, scaffold).
narrative_ontology:human_readable(finnish_ubi_experiment, "Finnish Basic Income Experiment (2017-2018)").
narrative_ontology:topic_domain(finnish_ubi_experiment, "economic/social").

% --- Binary flags ---
narrative_ontology:has_sunset_clause(finnish_ubi_experiment).      % Mandatory if Scaffold: it was explicitly a two-year experiment.
domain_priors:requires_active_enforcement(finnish_ubi_experiment). % Kela actively administered payments and rules.

% --- Emergence flag (required for mountain constraints) ---
% N/A. This is a human-designed constraint.

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(finnish_ubi_experiment, kela_policymakers).
%
% Who bears disproportionate cost?
narrative_ontology:constraint_victim(finnish_ubi_experiment, unemployed_participants).
% Note: "Victim" here refers to being the subject of an experiment with limited
% agency and an uncertain outcome, not necessarily net financial harm.

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

% PERSPECTIVE 1: THE PRIMARY TARGET (UNEMPLOYED PARTICIPANT)
% Experiences an unconditional payment (coordination good) but is trapped within
% an experimental structure. Low extraction means it's not a Snare.
% Derived d ≈ 0.95 → f(d) ≈ 1.42. χ = 0.20 * 1.42 * 1.0 (national) = 0.284.
% With χ ≤ 0.35 and ε ≤ 0.45, this classifies as a Rope.
constraint_indexing:constraint_classification(finnish_ubi_experiment, rope,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE PRIMARY BENEFICIARY (KELA & POLICYMAKERS)
% For the state and its agencies, the experiment is a tool for data collection
% and policy testing. It's a pure coordination mechanism.
% Derived d ≈ 0.05 → f(d) ≈ -0.12. χ = 0.20 * -0.12 * 1.0 = -0.024.
% Negative χ is a clear Rope.
constraint_indexing:constraint_classification(finnish_ubi_experiment, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER
% This perspective sees the complete structure: a temporary, low-extraction
% coordination mechanism with an explicit sunset clause. This is the definition
% of a Scaffold.
% Analytical d ≈ 0.72 → f(d) ≈ 1.15. χ = 0.20 * 1.15 * 1.2 (global) = 0.276.
% With has_sunset_clause, χ ≤ 0.30, and theater ≤ 0.70, this is a Scaffold.
constraint_indexing:constraint_classification(finnish_ubi_experiment, scaffold,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(finnish_ubi_experiment_tests).

test(perspectival_gap_participant_vs_analytical) :-
    constraint_indexing:constraint_classification(finnish_ubi_experiment, rope,
        context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(finnish_ubi_experiment, scaffold,
        context(agent_power(analytical), _, _, _)),
    format('... Perspectival gap validated (Rope vs Scaffold)').

test(scaffold_properties_adherence) :-
    narrative_ontology:has_sunset_clause(finnish_ubi_experiment),
    domain_priors:base_extractiveness(finnish_ubi_experiment, E), E < 0.30,
    domain_priors:theater_ratio(finnish_ubi_experiment, T), T < 0.70.

test(beneficiary_is_rope) :-
    constraint_indexing:constraint_classification(finnish_ubi_experiment, rope,
        context(agent_power(institutional), _, _, _)).

:- end_tests(finnish_ubi_experiment_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The base extractiveness (ε=0.20) is set low to reflect the experiment's
 *   goal of simplifying welfare and reducing overhead, though it includes
 *   the costs of setup and monitoring. Suppression (0.35) reflects the
 *   mandatory participation for the selected group. The key feature is the
 *   explicit two-year duration, making `has_sunset_clause` true and pointing
 *   strongly toward a Scaffold classification.
 *
 * PERSPECTIVAL GAP:
 *   The gap is between Rope and Scaffold. For participants and policymakers,
 *   the immediate experience is that of a coordination mechanism (a Rope)
 *   — one that provides unconditional income, the other that provides data.
 *   Only the analytical observer, with a wider time horizon, correctly
 *   classifies the entire structure as a Scaffold: a temporary support
 *   system designed to achieve a specific goal (testing a policy) before being
 *   dismantled. The participants don't see the sunset clause as a feature,
 *   but as the endpoint of a benefit.
 *
 * DIRECTIONALITY LOGIC:
 *   - Beneficiary: `kela_policymakers` benefit from the data generated and the
 *     political option value of testing a novel policy. Their exit option
 *     (arbitrage) is their power to terminate the experiment or choose
 *     alternative policies, giving them a low directionality (d).
 *   - Victim: `unemployed_participants` are victims of the experimental
 *     structure itself. They are subjected to a policy they did not choose
 *     and cannot exit, and which was ultimately discontinued, forcing them
 *     into a new, more coercive system. This "trapped" status gives them a
 *     high directionality (d), though the low base extraction (ε) prevents
 *     the classification from becoming a Snare.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification correctly identifies the experiment's nature as a
 *   temporary support structure (Scaffold), avoiding two potential errors.
 *   It is not a pure Rope, because its temporary nature is a defining
 *   structural feature. It is not a Snare, because its extractive component
 *   is very low and its primary function is coordination and information
 *   gathering, not rent-seeking. The framework correctly separates the
 *   intent and temporary structure from the lived experience of the participants.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    omega_finnish_ubi,
    'Would the observed minor positive effects on well-being and negligible effects on employment hold, grow, or reverse if the experiment were made permanent and universal?',
    'A longer-term (5-10 year) and larger-scale (cross-regional) basic income trial, or a full policy implementation.',
    'If effects grow, the policy is a viable Rope. If they reverse (e.g., due to labor market distortions), it is a failed Scaffold or a potential Piton if maintained for political reasons.',
    confidence_without_resolution(low)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(finnish_ubi_experiment, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% As a short, planned two-year experiment, the constraint's core metrics did
% not drift significantly during its lifecycle. The values are modeled as flat.
% This section is included for completeness, though not strictly required
% for a low-extraction constraint.
narrative_ontology:measurement(finnish_ubi_tr_t0, finnish_ubi_experiment, theater_ratio, 0, 0.15).
narrative_ontology:measurement(finnish_ubi_tr_t5, finnish_ubi_experiment, theater_ratio, 5, 0.15).
narrative_ontology:measurement(finnish_ubi_tr_t10, finnish_ubi_experiment, theater_ratio, 10, 0.15).

narrative_ontology:measurement(finnish_ubi_ex_t0, finnish_ubi_experiment, base_extractiveness, 0, 0.20).
narrative_ontology:measurement(finnish_ubi_ex_t5, finnish_ubi_experiment, base_extractiveness, 5, 0.20).
narrative_ontology:measurement(finnish_ubi_ex_t10, finnish_ubi_experiment, base_extractiveness, 10, 0.20).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
narrative_ontology:coordination_type(finnish_ubi_experiment, resource_allocation).

% Network relationships (structural influence edges)
% The failure to extend the UBI experiment directly led to the political
% decision to implement a more coercive "activation model" for the unemployed.
narrative_ontology:affects_constraint(finnish_ubi_experiment, finnish_activation_model).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% No overrides are necessary for this constraint. The structural derivation from
% beneficiary/victim declarations and exit options accurately models the
% power dynamics of the experiment.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */