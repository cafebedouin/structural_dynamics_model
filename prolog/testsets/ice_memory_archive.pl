% ============================================================================
% CONSTRAINT STORY: ice_memory_archive
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-05-21
% ============================================================================

:- module(constraint_ice_memory_archive, []).

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
 *   constraint_id: ice_memory_archive
 *   human_readable: The imperative to create a global ice core archive before glaciers melt.
 *   domain: technological/environmental
 *
 * SUMMARY:
 *   Due to accelerating climate change, glaciers around the world are melting,
 *   threatening the invaluable climate and environmental data trapped within their
 *   ice. The Ice Memory Foundation is leading a global project to drill ice
 *   cores from these threatened glaciers and transport them to a permanent,
 *   secure archive in Antarctica. The constraint is the irreversible loss of this
 *   data, which creates a time-limited window for action. This story models the
 *   project itself as a response to that threat.
 *
 * KEY AGENTS (by structural relationship):
 *   - Future Researchers (powerless/trapped): Primary conceptual "victim" of inaction. If the project fails, their ability to study past climate is permanently constrained.
 *   - Climate Science Community (institutional/arbitrage): Primary beneficiary and architect. They gain access to a preserved library of climate data.
 *   - Funding Nations/Organizations (organized/constrained): Provide resources for the project.
 *   - Analytical Observer (analytical/analytical): Sees the project as a temporary, coordinated support structure.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
% Extraction is the opportunity cost of resources (funding, personnel, logistics)
% dedicated to this project instead of other scientific or social goals. It is
% low, as the project is a public good.
domain_priors:base_extractiveness(ice_memory_archive, 0.10).

% Suppression is extremely high. Once a glacier melts, the specific layered
% data it contains is gone forever. There is no alternative way to recover it.
domain_priors:suppression_score(ice_memory_archive, 0.95).

% The project is highly functional (drilling, logistics, construction). The ratio
% of performative to functional activity is very low.
domain_priors:theater_ratio(ice_memory_archive, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ice_memory_archive, extractiveness, 0.10).
narrative_ontology:constraint_metric(ice_memory_archive, suppression_requirement, 0.95).
narrative_ontology:constraint_metric(ice_memory_archive, theater_ratio, 0.05).

% --- NL Profile Metrics ---
% Not a mountain from the analytical perspective.

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(ice_memory_archive, scaffold).

% --- Binary flags ---
% The project has a clear "sunset": once the archive is built and the
% threatened ice cores are secured, the emergency extraction phase is over.
% The archive then becomes a different, more permanent institution (a Rope).
narrative_ontology:has_sunset_clause(ice_memory_archive).

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% These feed the directionality derivation chain.
narrative_ontology:constraint_beneficiary(ice_memory_archive, climate_science_community).
narrative_ontology:constraint_beneficiary(ice_memory_archive, future_generations).

% The "victim" is a conceptual group representing the cost of inaction. Declaring
% them is crucial for deriving the correct directionality for the powerless/trapped
% perspective, which sees the permanent loss of data.
narrative_ontology:constraint_victim(ice_memory_archive, future_researchers_without_data).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × f(d) × σ(S)
   where f(d) = -0.20 + 1.70 / (1 + e^(-6*(d - 0.50)))
   ========================================================================== */

% PERSPECTIVE 1: FUTURE RESEARCHERS (THE "VICTIM" OF INACTION)
% From the perspective of a future scientist for whom the data is irrevocably
% lost (i.e., if this project were to fail), the constraint is the absolute
% limit on their knowledge. It is an unchangeable fact of their reality.
% ε is low, but suppression is near-total (0.95). This combination, from a
% trapped position, is experienced as a Mountain.
% Engine derives d from victim status + trapped exit → d ≈ 0.95 → f(d) ≈ 1.42.
% χ = 0.10 * 1.42 * 1.2 (global) = 0.17. While χ is low, the near-zero
% degrees of freedom (suppression=0.95) and the irreversibility make this
% classify as Mountain for this index.
constraint_indexing:constraint_classification(ice_memory_archive, mountain,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: THE CLIMATE SCIENCE COMMUNITY (THE BENEFICIARY)
% For the scientists and institutions organizing the project, it is a pure
% coordination challenge to achieve a shared goal.
% Engine derives d from beneficiary status + arbitrage exit → d ≈ 0.05 → f(d) ≈ -0.12.
% χ = 0.10 * -0.12 * 1.2 (global) = -0.0144.
% Negative extraction signifies a subsidy; this is a clear Rope.
constraint_indexing:constraint_classification(ice_memory_archive, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER
% The observer sees the full structure: a temporary, coordinated support system
% to bridge a critical gap, with a clear sunset clause. This is the definition
% of a Scaffold.
% Engine derives d ≈ 0.72 → f(d) ≈ 1.15 for analytical perspective.
% χ = 0.10 * 1.15 * 1.2 (global) = 0.138.
% With χ ≤ 0.30, theater ≤ 0.70, and a sunset clause, this is a canonical Scaffold.
constraint_indexing:constraint_classification(ice_memory_archive, scaffold,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ice_memory_archive_tests).

test(perspectival_gap_mountain_vs_rope) :-
    % Verify the core perspectival gap between the powerless and institutional views.
    constraint_indexing:constraint_classification(ice_memory_archive, mountain, context(agent_power(powerless), _, trapped, _)),
    constraint_indexing:constraint_classification(ice_memory_archive, rope, context(agent_power(institutional), _, arbitrage, _)),
    format('Passed: Powerless sees Mountain, Institutional sees Rope.~n').

test(analytical_scaffold_classification) :-
    % Verify the analytical classification is Scaffold.
    constraint_indexing:constraint_classification(ice_memory_archive, scaffold, context(agent_power(analytical), _, _, _)),
    narrative_ontology:has_sunset_clause(ice_memory_archive).

test(low_extraction_and_high_suppression) :-
    domain_priors:base_extractiveness(ice_memory_archive, E),
    domain_priors:suppression_score(ice_memory_archive, S),
    E =< 0.25,
    S >= 0.80.

:- end_tests(ice_memory_archive_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   - Base Extractiveness (ε=0.10): The project is a public good, but requires significant resource allocation (funding, logistics, scientific expertise) that could have been used elsewhere. This value represents the low opportunity cost.
 *   - Suppression (0.95): This score reflects the core driver of the project: the laws of thermodynamics. Once the glaciers melt, the data is permanently and irrecoverably lost. There are no alternative ways to access this specific historical record.
 *   - Theater (0.05): The project is defined by tangible, functional actions like drilling, transportation, and construction, making its theatrical component minimal.
 *
 * PERSPECTIVAL GAP:
 *   The gap is profound and illustrates the power of the indexical system.
 *   - The Beneficiary (climate scientists) sees a pure coordination problem with a shared benefit: a Rope.
 *   - The conceptual Victim (future researchers without data) experiences the consequence of failure as an unchangeable physical limit on their knowledge: a Mountain.
 *   - The Analytical observer sees the project for its structural role: a temporary support system to prevent a permanent loss, i.e., a Scaffold.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is derived from the clear roles. The `climate_science_community` and `future_generations` are explicit beneficiaries. The `future_researchers_without_data` group, though conceptual, perfectly represents the target of the underlying threat (data loss), justifying their role as `victim` for the derivation of `d`. This allows the engine to correctly compute the high `d` for the powerless/trapped perspective, capturing their experience of irreversibility.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification correctly identifies the project as a coordination effort (Scaffold/Rope) rather than a form of extraction (Snare). It avoids mislabeling the high suppression (which comes from physics) as a sign of coercive extraction. The system distinguishes between suppression from natural law and suppression from human enforcement. The analytical classification as a Scaffold highlights its temporary, supportive nature, preventing it from being mistaken for a permanent Rope or an inert Piton.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    omega_ice_memory_archive,
    'Is the primary long-term function of the Antarctic archive purely scientific, or could it become a geopolitical asset, altering its extractive potential?',
    'Analysis of governance charters for the archive, access protocols for non-funding nations, and geopolitical statements over the next 50 years.',
    'If purely scientific, it will transition from Scaffold to Rope. If it becomes a geopolitical asset, it could drift towards a Tangled Rope, where access is traded for political leverage.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ice_memory_archive, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Although extraction is low, we model a slight increase as the project
% institutionalizes, moves from initial grants to requiring permanent operational funding.
% Theater remains low throughout. This represents a healthy lifecycle.

% Theater ratio over time:
narrative_ontology:measurement(ice_memory_archive_tr_t0, ice_memory_archive, theater_ratio, 0, 0.05).
narrative_ontology:measurement(ice_memory_archive_tr_t5, ice_memory_archive, theater_ratio, 5, 0.05).
narrative_ontology:measurement(ice_memory_archive_tr_t10, ice_memory_archive, theater_ratio, 10, 0.05).

% Extraction over time:
narrative_ontology:measurement(ice_memory_archive_ex_t0, ice_memory_archive, base_extractiveness, 0, 0.08).
narrative_ontology:measurement(ice_memory_archive_ex_t5, ice_memory_archive, base_extractiveness, 5, 0.09).
narrative_ontology:measurement(ice_memory_archive_ex_t10, ice_memory_archive, base_extractiveness, 10, 0.10).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% The project's function is to create a permanent piece of shared scientific
% infrastructure.
narrative_ontology:coordination_type(ice_memory_archive, global_infrastructure).

% This constraint is a direct consequence of a larger, more fundamental constraint.
% The failure mode of 'anthropogenic_climate_change' is the trigger for the
% existence of 'ice_memory_archive'.
narrative_ontology:affects_constraint(anthropogenic_climate_change, ice_memory_archive).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% No overrides are necessary. The structural derivation from beneficiary/victim
% declarations and exit options accurately models the perspectives.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */