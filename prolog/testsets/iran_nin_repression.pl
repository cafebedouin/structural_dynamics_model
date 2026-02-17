% ============================================================================
% CONSTRAINT STORY: constraint_iran_nin_repression
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-05-21
% ============================================================================

:- module(constraint_iran_nin_repression, []).

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
 *   constraint_id: iran_nin_repression
 *   human_readable: Iran's National Information Network (State-Controlled Intranet)
 *   domain: technological/political
 *
 * SUMMARY:
 *   Iran's National Information Network (NIN), or "halal internet," is a
 *   state-controlled intranet designed to function independently of the global
 *   internet. While officially framed as a tool for cybersecurity and cultural
 *   protection, its primary function is to enable the state to monitor citizens,
 *   censor information, and selectively sever access to global networks,
 *   particularly during periods of political unrest, thereby suppressing dissent.
 *
 * KEY AGENTS (by structural relationship):
 *   - Iranian citizens: Primary target (powerless/trapped) — bear the costs of censorship, surveillance, and loss of economic/social opportunity.
 *   - The Iranian regime: Primary beneficiary (institutional/arbitrage) — benefits from enhanced control, surveillance capabilities, and the ability to disrupt opposition organizing.
 *   - International observers: Analytical observer — sees the full structure of repression.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(iran_nin_repression, 0.85).
domain_priors:suppression_score(iran_nin_repression, 0.95).   % Structural property (raw, unscaled).
domain_priors:theater_ratio(iran_nin_repression, 0.15).       % Piton detection (>= 0.70)

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(iran_nin_repression, extractiveness, 0.85).
narrative_ontology:constraint_metric(iran_nin_repression, suppression_requirement, 0.95).
narrative_ontology:constraint_metric(iran_nin_repression, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
% N/A for this constraint.

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(iran_nin_repression, snare).
narrative_ontology:human_readable(iran_nin_repression, "Iran's National Information Network (State-Controlled Intranet)").

% --- Binary flags ---
domain_priors:requires_active_enforcement(iran_nin_repression).

% --- Emergence flag (required for mountain constraints) ---
% N/A for this constraint.

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% These feed the directionality derivation chain: the engine computes
% d (directionality) from agent membership in these groups + exit_options.
%
% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(iran_nin_repression, iranian_regime).
%
% Who bears disproportionate cost?
narrative_ontology:constraint_victim(iran_nin_repression, iranian_citizens).

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
% Iranian citizens are trapped within the digital borders created by the state.
% Engine derives d from: victim membership + trapped exit → d ≈ 0.95 → f(d) ≈ 1.42 → high χ
% χ ≈ 0.85 * 1.42 * 1.0 (national scope) ≈ 1.20. Well above Snare threshold (χ ≥ 0.66).
% NOTE: The "Dynamic Coalition" extension suggests that if a critical mass
% of citizens organize, their power could shift to 'organized', but the NIN is
% explicitly designed to prevent this shift from occurring.
constraint_indexing:constraint_classification(iran_nin_repression, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE PRIMARY BENEFICIARY (ROPE)
% The Iranian regime sees this as a perfect tool for state control and stability.
% Engine derives d from: beneficiary membership + arbitrage exit → d ≈ 0.05 → f(d) ≈ -0.12 → negative χ
% χ ≈ 0.85 * -0.12 * 1.0 (national scope) ≈ -0.10. Classifies as Rope.
constraint_indexing:constraint_classification(iran_nin_repression, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (SNARE)
% The observer sees the high base extraction and suppression, classifying it based on its core function.
% Engine derives d ≈ 0.72 → f(d) ≈ 1.15 for analytical perspective.
% χ ≈ 0.85 * 1.15 * 1.2 (global scope) ≈ 1.17. Clearly a Snare.
constraint_indexing:constraint_classification(iran_nin_repression, snare,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).


/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(iran_nin_repression_tests).

test(perspectival_gap_is_snare_vs_rope) :-
    % Verify the core perspectival gap between the trapped citizen and the regime.
    constraint_indexing:constraint_classification(iran_nin_repression, snare, context(agent_power(powerless), _, exit_options(trapped), _)),
    constraint_indexing:constraint_classification(iran_nin_repression, rope, context(agent_power(institutional), _, exit_options(arbitrage), _)).

test(analytical_view_is_snare) :-
    % The ground truth classification from the system's perspective must be Snare.
    constraint_indexing:constraint_classification(iran_nin_repression, snare, context(agent_power(analytical), _, _, _)).

test(threshold_validation_snare) :-
    % Verify the base metrics meet the Snare definition.
    domain_priors:base_extractiveness(iran_nin_repression, E), E >= 0.46,
    domain_priors:suppression_score(iran_nin_repression, S), S >= 0.60.

:- end_tests(iran_nin_repression_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   - Base Extractiveness (ε=0.85): Extremely high. This value represents the
 *     severe cost imposed on Iranian citizens, measured in lost freedom of
 *     speech, access to information, privacy, and economic opportunity due to
 *     isolation from the global digital economy.
 *   - Suppression Score (S=0.95): Near-total. The NIN's explicit purpose is to
 *     suppress the primary alternative—the global internet. This is achieved
 *     through technical blocking, legal prohibitions on circumvention tools (VPNs),
 *     and coercive state power.
 *   - Theater Ratio (T=0.15): Low. The official justifications (cybersecurity,
 *     protecting culture) are a thin pretext for the system's primary, non-
 *     performative function of surveillance and control.
 *
 * PERSPECTIVAL GAP:
 *   The gap is stark: Snare vs. Rope. For a citizen (powerless, trapped), the NIN
 *   is an inescapable mechanism of control that extracts their fundamental rights.
 *   For the regime (institutional, arbitrage), it is a pure coordination tool (a Rope)
 *   for managing state security and suppressing political threats, with zero
 *   perceived extraction from their perspective. This difference is driven entirely
 *   by their structural relationship to the constraint, which the directionality
 *   formalism captures.
 *
 * DIRECTIONALITY LOGIC:
 *   - Beneficiary: `iranian_regime`. They gain total information control, pre-emptive
 *     disruption of protests, and surveillance capabilities. Their `arbitrage` exit
 *     status reflects their ability to selectively access the global internet while
 *     denying it to others, driving their directionality `d` close to 0.
 *   - Victim: `iranian_citizens`. They bear all the costs. Their `trapped` exit status
 *     reflects their inability to opt out of this digital infrastructure without
 *     facing significant risk or leaving the country, driving their `d` close to 1.
 *   The engine correctly derives the huge gap in `d` values from these declarations,
 *   which in turn produces the Snare/Rope classification gap.
 *
 * MANDATROPHY ANALYSIS:
 *   [RESOLVED MANDATROPHY] This classification correctly identifies the NIN as a tool of pure extraction (Snare)
 *   from a systemic perspective, while also explaining why its creators and beneficiaries
 *   can rationalize it as a necessary coordination mechanism (Rope). It avoids the
 *   mandatrophy error of taking the regime's "coordination" claim at face value by
 *   centering the analysis on the extremely high, asymmetrically borne costs (ε) and
 *   the suppression of alternatives (S).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    omega_iran_nin_repression,
    'Is the NIN a stable tool of long-term control, or will it accelerate regime fragility by fueling dissent and innovation in circumvention technology?',
    'Observation of the system''s effectiveness and the population''s response during the next major nationwide protest cycle.',
    'If stable -> confirms Snare efficacy. If it fails or backfires -> indicates a potential lifecycle shift toward a brittle Piton as circumvention becomes widespread.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(iran_nin_repression, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% This models the intensification of the NIN from a concept with some plausible
% deniability into a full-fledged tool of repression over a 10-year interval.
% Extraction accumulates as the system becomes more technologically capable
% and ruthlessly enforced.

% Theater ratio over time (declines as repressive function becomes primary):
narrative_ontology:measurement(iran_nin_repression_tr_t0, iran_nin_repression, theater_ratio, 0, 0.30).
narrative_ontology:measurement(iran_nin_repression_tr_t5, iran_nin_repression, theater_ratio, 5, 0.20).
narrative_ontology:measurement(iran_nin_repression_tr_t10, iran_nin_repression, theater_ratio, 10, 0.15).

% Extraction over time (increases as infrastructure is built and control tightens):
narrative_ontology:measurement(iran_nin_repression_ex_t0, iran_nin_repression, base_extractiveness, 0, 0.40).
narrative_ontology:measurement(iran_nin_repression_ex_t5, iran_nin_repression, base_extractiveness, 5, 0.65).
narrative_ontology:measurement(iran_nin_repression_ex_t10, iran_nin_repression, base_extractiveness, 10, 0.85).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type: The system's "coordination" function is purely for the
% beneficiary (the state) to coordinate its control over the target (the population).
narrative_ontology:coordination_type(iran_nin_repression, enforcement_mechanism).

% Network relationships: The NIN is a tool used by the regime to manage its
% political legitimacy crisis by suppressing dissent that highlights it.
narrative_ontology:affects_constraint(iran_political_legitimacy_crisis, iran_nin_repression).


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% No overrides are necessary for this constraint. The structural derivation
% from beneficiary/victim declarations and exit options accurately models
% the power dynamics at play.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */