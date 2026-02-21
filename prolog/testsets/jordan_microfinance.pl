% ============================================================================
% CONSTRAINT STORY: jordan_microfinance
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-05-21
% ============================================================================

:- module(constraint_jordan_microfinance, []).

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
 *   constraint_id: jordan_microfinance
 *   human_readable: Ideological Gating of Microfinance in Jordan
 *   domain: economic
 *
 * SUMMARY:
 *   A microfinance program in Jordan, operated by Finca and backed by USAID,
 *   requires impoverished loan applicants to attend mandatory educational
 *   sessions. These sessions promote neoliberal ideologies through the works of
 *   Ayn Rand, Stephen Covey, and Thomas Friedman as a precondition for
 *   receiving capital. The constraint gates access to a financial resource
 *   with a coercive behavioral and ideological requirement, framed as
 *   empowerment or teaching "smartness."
 *
 * KEY AGENTS (by structural relationship):
 *   - low_income_borrowers: Primary target (powerless/trapped) — bear the
 *     cost of mandatory ideological training to access needed capital.
 *   - microfinance_institutions: Primary beneficiary (institutional/arbitrage)
 *     — benefit from perceived higher repayment rates, ideological alignment
 *     of their client base, and institutional prestige for their "innovative" model.
 *   - Analytical Observer: Sees the full dual-function structure of the
 *     constraint (coordination and extraction).
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jordan_microfinance, 0.60).
domain_priors:suppression_score(jordan_microfinance, 0.75).   % Structural property (raw, unscaled).
domain_priors:theater_ratio(jordan_microfinance, 0.40).       % Piton detection (>= 0.70)

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jordan_microfinance, extractiveness, 0.60).
narrative_ontology:constraint_metric(jordan_microfinance, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(jordan_microfinance, theater_ratio, 0.40).

% --- NL Profile Metrics (required for mountain constraints) ---
% N/A for this constraint.

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(jordan_microfinance, tangled_rope).

% --- Binary flags ---
domain_priors:requires_active_enforcement(jordan_microfinance). % Required for Tangled Rope

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% These feed the directionality derivation chain: the engine computes
% d (directionality) from agent membership in these groups + exit_options.
%
% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(jordan_microfinance, microfinance_institutions).
%
% Who bears disproportionate cost?
narrative_ontology:constraint_victim(jordan_microfinance, low_income_borrowers).
%
% Gate requirements:
%   Tangled Rope: beneficiary + victim + requires_active_enforcement (all three are met)
%   Snare:        victim required (met)

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
% The low-income borrower who needs the loan. They experience the mandatory
% training as a coercive, extractive hurdle.
% Engine derives d from: victim membership + trapped exit → d ≈ 0.95 → f(d) ≈ 1.42.
% χ = 0.60 * 1.42 * 0.8 (local scope) = 0.68.
% With ε=0.60, suppression=0.75, and χ=0.68, this meets the Snare thresholds.
constraint_indexing:constraint_classification(jordan_microfinance, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: THE PRIMARY BENEFICIARY (ROPE)
% The microfinance institution (e.g., Finca) and its backers (e.g., USAID).
% They see the program as an innovative coordination mechanism to improve client
% success and ensure repayment.
% Engine derives d from: beneficiary membership + arbitrage exit → d ≈ 0.05 → f(d) ≈ -0.12.
% χ = 0.60 * -0.12 * 1.0 (national scope) = -0.072.
% With negative effective extraction, this classifies as a Rope.
constraint_indexing:constraint_classification(jordan_microfinance, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% A sociologist or economist observing the system's full structure. They
% recognize both the valid coordination function (providing capital) and the
% asymmetric extraction (coercive ideological training).
% Engine derives canonical d ≈ 0.72 → f(d) ≈ 1.15.
% χ = 0.60 * 1.15 * 1.2 (global scope) = 0.828.
% With ε=0.60, suppression=0.75, χ=0.83, and all three Tangled Rope gate
% conditions met, this is a clear Tangled Rope.
constraint_indexing:constraint_classification(jordan_microfinance, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).


/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jordan_microfinance_tests).

test(perspectival_gap) :-
    % Verify the core perspectival gap between target (Snare) and beneficiary (Rope).
    constraint_indexing:constraint_classification(jordan_microfinance, snare, context(agent_power(powerless), _, exit_options(trapped), _)),
    constraint_indexing:constraint_classification(jordan_microfinance, rope, context(agent_power(institutional), _, exit_options(arbitrage), _)),
    constraint_indexing:constraint_classification(jordan_microfinance, tangled_rope, context(agent_power(analytical), _, _, _)).

test(tangled_rope_gates_pass) :-
    % A constraint is only a Tangled Rope if it has a coordination function
    % (beneficiary), asymmetric extraction (victim), and active enforcement.
    narrative_ontology:constraint_beneficiary(jordan_microfinance, _),
    narrative_ontology:constraint_victim(jordan_microfinance, _),
    domain_priors:requires_active_enforcement(jordan_microfinance).

test(threshold_validation) :-
    % Verify the base metrics are in the high-extraction range for Snare/Tangled Rope.
    narrative_ontology:constraint_metric(jordan_microfinance, extractiveness, E), E >= 0.46,
    narrative_ontology:constraint_metric(jordan_microfinance, suppression_requirement, S), S >= 0.40.

:- end_tests(jordan_microfinance_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness (ε=0.60) is high, representing the significant non-monetary
 *   cost of time, cognitive load, and ideological pressure imposed on borrowers.
 *   Suppression (0.75) reflects the lack of alternative capital sources for this
 *   demographic, making the conditions non-negotiable. The theater ratio (0.40)
 *   captures the "empowerment" and "smartness" narrative used to frame the
 *   coercive element. The constraint is a Tangled Rope because it has an
 *   undeniable coordination function (providing loans) layered with an equally
 *   undeniable extractive one (mandatory ideological compliance).
 *
 * PERSPECTIVAL GAP:
 *   The gap is profound. For the institution (beneficiary), the ideological
 *   training is a feature, a risk-mitigation and empowerment tool that makes
 *   their coordination (lending) more effective. They see a Rope. For the
 *   borrower (target), the training is a coercive cost they must pay to
 *   access a lifeline. They experience a Snare that extracts compliance. The
 *   analytical view reconciles these by identifying the hybrid Tangled Rope
 *   structure.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is derived directly from the structural roles. The `low_income_borrowers`
 *   are declared victims; combined with their `trapped` exit status, this
 *   derives a high directionality (d ≈ 0.95), maximizing the effective
 *   extraction (χ) they experience. The `microfinance_institutions` are declared
 *   beneficiaries; with `arbitrage` exit options, this derives a low/negative
 *   directionality (d ≈ 0.05), making the constraint appear as pure coordination (Rope).
 *
 * MANDATROPHY ANALYSIS:
 *   This classification avoids two common errors. It prevents the system from
 *   being naively classified as a pure Rope, ignoring the coercive extraction of
 *   compliance. It also prevents it from being classified as a pure Snare by
 *   the analytical observer, acknowledging that a genuine coordination service
 *   (access to capital) is being provided. The Tangled Rope classification
 *   correctly identifies that the problem is not the loan itself, but the
 *   extractive conditions tied to it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_jordan_microfinance,
    'Is the ideological training primarily a cynical tool for social control and risk management, or a genuinely (if misguidedly) paternalistic attempt at empowerment?',
    'Internal communications and strategy documents from the microfinance institution and its backers (e.g., USAID).',
    'If cynical, the base extractiveness could be even higher (closer to a pure Snare). If genuinely paternalistic, the theater ratio would be higher, reflecting sincere belief in the performative aspect.',
    confidence_without_resolution(low)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(jordan_microfinance, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% This is a high-extraction constraint (ε=0.60 > 0.46), so temporal data is required.
% This models a scenario where the program began with a more modest
% educational component that intensified into a more rigid ideological one
% over time, increasing both extraction and theater.

% Theater ratio over time (triggers metric_substitution detection):
narrative_ontology:measurement(jordan_microfinance_tr_t0, jordan_microfinance, theater_ratio, 0, 0.20).
narrative_ontology:measurement(jordan_microfinance_tr_t5, jordan_microfinance, theater_ratio, 5, 0.30).
narrative_ontology:measurement(jordan_microfinance_tr_t10, jordan_microfinance, theater_ratio, 10, 0.40).

% Extraction over time (triggers extraction_accumulation detection):
narrative_ontology:measurement(jordan_microfinance_ex_t0, jordan_microfinance, base_extractiveness, 0, 0.40).
narrative_ontology:measurement(jordan_microfinance_ex_t5, jordan_microfinance, base_extractiveness, 5, 0.50).
narrative_ontology:measurement(jordan_microfinance_ex_t10, jordan_microfinance, base_extractiveness, 10, 0.60).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% This constraint's coordination function is to allocate capital.
narrative_ontology:coordination_type(jordan_microfinance, resource_allocation).

% Network relationships (structural influence edges)
% The existence and perceived success of this model can influence other
% development aid programs, making this type of conditionality more common.
narrative_ontology:affects_constraint(jordan_microfinance, development_aid_conditionality).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% No overrides are needed for this constraint. The automatic derivation from
% beneficiary/victim declarations and exit options accurately models the
% structural relationships and directionality of the constraint.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */