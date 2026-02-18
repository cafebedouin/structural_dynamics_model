% ============================================================================
% CONSTRAINT STORY: iss_ped_ban
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-05-20
% ============================================================================

:- module(constraint_iss_ped_ban, []).

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
 *   constraint_id: iss_ped_ban
 *   human_readable: NASA's historical ban on personal electronic devices on the ISS
 *   domain: technological
 *
 * SUMMARY:
 *   For over two decades, NASA prohibited astronauts from bringing personal
 *   electronic devices (PEDs) like smartphones to the International Space
 *   Station (ISS). The ban was justified on grounds of cybersecurity, potential
 *   interference with station equipment, and fire safety (lithium-ion batteries).
 *   This story models the ban itself as a constraint, which was recently lifted
 *   (c. 2024), indicating a change in the underlying risk assessment.
 *
 * KEY AGENTS (by structural relationship):
 *   - ISS Astronauts: Primary target (moderate/trapped) — bore the costs of isolation and lack of personal communication tools.
 *   - NASA Security & Operations: Primary beneficiary (institutional/arbitrage) — benefited from a simplified threat model and reduced operational complexity.
 *   - Analytical Observer: Analytical observer (analytical/analytical) — sees the full structure as a trade-off between security and human factors.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(iss_ped_ban, 0.55). % Extracts personal autonomy, privacy, and ease of communication.
domain_priors:suppression_score(iss_ped_ban, 0.85).   % Structural property (raw, unscaled). Alternatives were completely forbidden.
domain_priors:theater_ratio(iss_ped_ban, 0.40).       % Piton detection (>= 0.70). Not a Piton, but justification weakened over time.

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(iss_ped_ban, extractiveness, 0.55).
narrative_ontology:constraint_metric(iss_ped_ban, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(iss_ped_ban, theater_ratio, 0.40).

% --- NL Profile Metrics (required for mountain constraints) ---
% N/A for this constraint.

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(iss_ped_ban, tangled_rope).
narrative_ontology:human_readable(iss_ped_ban, "NASA's historical ban on personal electronic devices on the ISS").
narrative_ontology:topic_domain(iss_ped_ban, "technological").

% --- Binary flags ---
domain_priors:requires_active_enforcement(iss_ped_ban). % Cargo was screened; rules were enforced.

% --- Emergence flag (required for mountain constraints) ---
% N/A for this constraint.

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% These feed the directionality derivation chain: the engine computes
% d (directionality) from agent membership in these groups + exit_options.

% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(iss_ped_ban, nasa_security_and_ops).
%
% Who bears disproportionate cost?
narrative_ontology:constraint_victim(iss_ped_ban, iss_astronauts).
%
% Gate requirements:
%   Tangled Rope: beneficiary + victim + requires_active_enforcement (all three met)
%   Scaffold:     beneficiary + (has_sunset_clause OR no enforcement)
%   Snare:        victim required; beneficiary optional

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

% PERSPECTIVE 1: THE PRIMARY TARGET (ASTRONAUTS)
% Agent who bears the most extraction. Engine derives d from:
%   victim membership + trapped exit → d ≈ 0.95 → f(d) ≈ 1.42 → high χ
% With ε=0.55 and local scope (σ=0.8), χ ≈ 0.55 * 1.42 * 0.8 ≈ 0.62.
% This is just below the snare threshold (χ ≥ 0.66), making it a very
% coercive Tangled Rope. They benefit from station security, but the
% cost is extremely high.
constraint_indexing:constraint_classification(iss_ped_ban, tangled_rope,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: THE PRIMARY BENEFICIARY (NASA SECURITY & OPS)
% Agent who benefits most. Engine derives d from:
%   beneficiary membership + arbitrage exit → d ≈ 0.05 → f(d) ≈ -0.12 → negative χ
% From this perspective, the ban is a pure coordination Rope that simplifies
% security and reduces risk with no perceived extractive cost.
constraint_indexing:constraint_classification(iss_ped_ban, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER
% Default analytical context (civilizational/analytical/global).
% Correctly identifies the dual nature of coordination and extraction.
constraint_indexing:constraint_classification(iss_ped_ban, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).


/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(iss_ped_ban_tests).

test(perspectival_gap_is_tangled_rope_vs_rope, [nondet]) :-
    % Verify the core perspectival gap: target sees Tangled Rope, beneficiary sees Rope.
    constraint_indexing:constraint_classification(iss_ped_ban, tangled_rope,
        context(agent_power(powerless), _, exit_options(trapped), _)),
    constraint_indexing:constraint_classification(iss_ped_ban, rope,
        context(agent_power(institutional), _, exit_options(arbitrage), _)).

test(analytical_view_is_tangled_rope, [nondet]) :-
    % The system's official claim should match the analytical perspective.
    narrative_ontology:constraint_claim(iss_ped_ban, ClaimedType),
    constraint_indexing:constraint_classification(iss_ped_ban, AnalyticalType,
        context(agent_power(analytical), _, _, _)),
    ClaimedType == AnalyticalType,
    ClaimedType == tangled_rope.

test(tangled_rope_gate_requirements_met) :-
    % Verify that all three structural conditions for a Tangled Rope are met.
    narrative_ontology:constraint_beneficiary(iss_ped_ban, _),
    narrative_ontology:constraint_victim(iss_ped_ban, _),
    domain_priors:requires_active_enforcement(iss_ped_ban).

:- end_tests(iss_ped_ban_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   - Base Extractiveness (ε=0.55): The ban imposed significant non-monetary
 *     costs on astronauts, affecting mental health, family connection, and
 *     personal autonomy for extended missions. This is a high level of
 *     extraction from a human-factors perspective.
 *   - Suppression (0.85): The rule was absolute. No workarounds were permitted.
 *     Alternatives were completely suppressed by institutional authority.
 *   - The combination of a legitimate coordination function (security) and high,
 *     asymmetric extraction makes this a canonical Tangled Rope.
 *
 * PERSPECTIVAL GAP:
 *   The gap is stark. For NASA Security and Operations, the ban was a pure Rope.
 *   It solved a complex coordination problem (securing a multi-billion dollar
 *   asset with human lives at stake) with a simple, enforceable rule. From this
 *   view, the cost is zero and the benefit is high.
 *   For an astronaut, the rule was a coercive Tangled Rope bordering on a Snare.
 *   While they also benefited from the station's security, the cost was borne
 *   entirely by them, extracting a fundamental part of modern life for what
 *   increasingly seemed like institutional convenience.
 *
 * DIRECTIONALITY LOGIC:
 *   - Beneficiary: `nasa_security_and_ops` benefits from a drastically simplified
 *     threat landscape. They set the rules and have arbitrage exit.
 *   - Victim: `iss_astronauts` bear the costs. They are physically trapped in the
 *     environment where the constraint applies, giving them `trapped` exit status.
 *   This structural relationship is the primary driver of the perspectival gap.
 *   The engine correctly derives a low `d` for the institution and a high `d` for
 *   the astronauts, leading to the Rope vs. Tangled Rope classifications.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification correctly avoids two errors. It doesn't label the ban
 *   a pure Snare, because that would ignore the very real and legitimate
 *   security coordination function it served, especially in its early days.
 *   Conversely, it avoids labeling it a simple Rope, which would erase the
 *   significant extraction imposed on astronauts. The Tangled Rope
 *   classification captures this dual nature perfectly: a tool of coordination
 *   achieved through coercive, asymmetric means. The lifting of the ban suggests
 *   a transition to a new, less-extractive Rope (e.g., using secured devices).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_iss_ped_ban,
    'Was the cybersecurity risk justifying the total ban genuine and proportional, or was it an artifact of institutional risk aversion disproportionate to the actual threat?',
    'Declassification of internal NASA/partner agency risk assessments from 2000-2020.',
    'If the risk was genuinely high, the Rope component is stronger. If it was inflated, the Snare/extractive component is stronger, and the theater ratio was higher throughout its lifecycle.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(iss_ped_ban, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Required for high-extraction constraints (base_extractiveness > 0.46).
% This models the ban from its inception (~2000) to its end (~2024).

% Theater ratio over time (triggers metric_substitution detection):
% The ban's justification became weaker as technology for securing devices
% improved, making the rule more theatrical/inertial over time.
narrative_ontology:measurement(iss_ped_ban_tr_t0, iss_ped_ban, theater_ratio, 0, 0.10).
narrative_ontology:measurement(iss_ped_ban_tr_t5, iss_ped_ban, theater_ratio, 5, 0.25).
narrative_ontology:measurement(iss_ped_ban_tr_t10, iss_ped_ban, theater_ratio, 10, 0.40).

% Extraction over time (triggers extraction_accumulation detection):
% The extractive cost to astronauts remained consistently high throughout the
% ban's existence.
narrative_ontology:measurement(iss_ped_ban_ex_t0, iss_ped_ban, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(iss_ped_ban_ex_t5, iss_ped_ban, base_extractiveness, 5, 0.55).
narrative_ontology:measurement(iss_ped_ban_ex_t10, iss_ped_ban, base_extractiveness, 10, 0.55).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% The ban was a security protocol.
narrative_ontology:coordination_type(iss_ped_ban, enforcement_mechanism).

% Network relationships (structural influence edges)
% The ban on personal devices directly impacts crew morale, which is its
% own complex constraint system.
narrative_ontology:affects_constraint(iss_ped_ban, iss_crew_morale).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% No overrides are necessary for this constraint. The automatic derivation
% from beneficiary/victim declarations and exit options accurately captures
% the structural dynamics between NASA Operations and the astronauts.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */