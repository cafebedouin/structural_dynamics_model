% ============================================================================
% CONSTRAINT STORY: shannon_source_coding
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-15
% ============================================================================

:- module(constraint_shannon_source_coding, []).

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
 *   constraint_id: shannon_source_coding
 *   human_readable: Shannon's Source Coding Theorem (Achievable Compression Limit)
 *   domain: technological
 *
 * SUMMARY:
 *   Shannon's Source Coding Theorem establishes the theoretical limit for lossless data compression. It states that a data source can be compressed to its entropy rate, but no further, without losing information. This constraint is a fundamental law of information theory, impacting the efficiency of all digital data storage and transmission.
 *
 * KEY AGENTS (by structural relationship):
 *   - Data Originators: Any agent creating data (powerless/trapped) — must adhere to entropy limits.
 *   - Algorithm Designers: Agents creating compression algorithms (powerful/arbitrage) — leverage the theorem to create economic value.
 *   - Network Providers: Infrastructure operators (institutional/arbitrage) — their business models depend on the efficiencies enabled by approaching this limit.
 *   - Analytical Observers: Information theorists, computer scientists (analytical/analytical) — understand the theorem's mathematical basis and implications.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(shannon_source_coding, 0.15).
domain_priors:suppression_score(shannon_source_coding, 0.05).   % Structural property (raw, unscaled).
domain_priors:theater_ratio(shannon_source_coding, 0.01).       % Piton detection (>= 0.70)

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(shannon_source_coding, extractiveness, 0.15).
narrative_ontology:constraint_metric(shannon_source_coding, suppression_requirement, 0.05).
narrative_ontology:constraint_metric(shannon_source_coding, theater_ratio, 0.01).

% --- NL Profile Metrics (required for mountain constraints) ---
% These feed the natural_law_signature certification chain in
% structural_signatures.pl. Without these, the NL signature defaults to 0.5
% and fails certification.
narrative_ontology:constraint_metric(shannon_source_coding, accessibility_collapse, 0.95).
narrative_ontology:constraint_metric(shannon_source_coding, resistance, 0.02).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(shannon_source_coding, mountain).

% --- Binary flags ---
% narrative_ontology:has_sunset_clause(shannon_source_coding).
% domain_priors:requires_active_enforcement(shannon_source_coding).

% --- Emergence flag (required for mountain constraints) ---
% Required for the mountain metric gate: without this, the classify_from_metrics
% mountain clause will not fire.
domain_priors:emerges_naturally(shannon_source_coding).

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% No enrichment needed for natural law (Mountain) constraints. The concepts of
% beneficiary and victim do not apply as the constraint is a feature of
% reality, not a socially constructed mechanism of extraction.

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

% PERSPECTIVE 1: DATA ORIGINATORS (POWERLESS)
% Any agent creating data is fundamentally constrained by this theorem.
% Exit is impossible, hence 'trapped'.
constraint_indexing:constraint_classification(shannon_source_coding, mountain,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(universal))).

% PERSPECTIVE 2: ALGORITHM DESIGNERS (POWERFUL)
% Those who understand and apply the theorem can create value.
constraint_indexing:constraint_classification(shannon_source_coding, mountain,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: NETWORK PROVIDERS (INSTITUTIONAL)
% Large-scale infrastructure operators whose business models rely on the
% efficiencies predicted by the theorem.
constraint_indexing:constraint_classification(shannon_source_coding, mountain,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: THE ANALYTICAL OBSERVER
% The formal, mathematical perspective.
constraint_indexing:constraint_classification(shannon_source_coding, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(shannon_source_coding_tests).

test(perspectival_alignment) :-
    % Verify perspectival alignment: all perspectives should classify as mountain.
    constraint_indexing:constraint_classification(shannon_source_coding, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(shannon_source_coding, TypeInstitutional, context(agent_power(institutional), _, _, _)),
    constraint_indexing:constraint_classification(shannon_source_coding, TypeObserver, context(agent_power(analytical), _, _, _)),

    TypePowerless = mountain,
    TypeInstitutional = mountain,
    TypeObserver = mountain.

test(threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(shannon_source_coding, ExtMetricName, E),
    E =< 0.25. % Mountain threshold.

test(nl_profile_validation) :-
    narrative_ontology:constraint_metric(shannon_source_coding, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(shannon_source_coding, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(shannon_source_coding_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The scores reflect that Shannon's source coding theorem is a fundamental, mathematical limit on compression (ε=0.15, suppression=0.05). It is not a human-enforced rule but a property of information itself. The extraction is low because the "cost" it imposes is simply the irreducible complexity of the data. The theater ratio is near zero as there is no performative aspect to a mathematical theorem. The NL profile metrics (high accessibility_collapse, low resistance) and the emerges_naturally flag certify its status as a natural law.
 *
 * PERSPECTIVAL GAP:
 *   There is no perspectival gap. As a natural law of information theory, the constraint is perceived as a Mountain by all agents, regardless of their power, resources, or goals. The classification is invariant across all indices, which is the hallmark of a Mountain constraint.
 *
 * DIRECTIONALITY LOGIC:
 *   For a Mountain constraint, beneficiary/victim declarations are not applicable as the constraint is not a mechanism for social extraction. The different perspectives (powerless, powerful, institutional) merely demonstrate that the law applies universally, constraining all agents equally in proportion to their interaction with information. No agent can choose to ignore it, and no agent can leverage it to extract from another *through the mechanism of the law itself*.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as a Mountain is crucial. It correctly identifies the theorem as a feature of reality, not an artificial Snare or a coordination-based Rope. An attempt to frame this limit as an oppressive, extractive mechanism would be a category error, confusing a law of nature with a law of man. The framework's strict metric requirements for Mountain classification prevent this mislabeling.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_shannon_source_coding,
    'Could a novel physical regime (e.g., quantum information, non-ergodic systems) reveal a different, less restrictive compression limit?',
    'Empirical demonstration of sustained, lossless compression beyond the Shannon entropy limit in a non-classical system.',
    'If true: Shannon limit is a classical limit, not universal. If false: Shannon limit is fundamental across all known physical systems.',
    confidence_without_resolution(low)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(shannon_source_coding, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Temporal data is not required as base_extractiveness (0.15) is below the
% 0.46 threshold. The theorem's properties are static and do not drift over time.
% The following are included to show the principle of non-drift.
%
% Theater ratio over time (stable):
narrative_ontology:measurement(shannon_source_coding_tr_t0, shannon_source_coding, theater_ratio, 0, 0.01).
narrative_ontology:measurement(shannon_source_coding_tr_t5, shannon_source_coding, theater_ratio, 5, 0.01).
narrative_ontology:measurement(shannon_source_coding_tr_t10, shannon_source_coding, theater_ratio, 10, 0.01).

% Extraction over time (stable):
narrative_ontology:measurement(shannon_source_coding_ex_t0, shannon_source_coding, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(shannon_source_coding_ex_t5, shannon_source_coding, base_extractiveness, 5, 0.15).
narrative_ontology:measurement(shannon_source_coding_ex_t10, shannon_source_coding, base_extractiveness, 10, 0.15).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% The theorem itself is a standard against which all compression schemes are measured.
narrative_ontology:coordination_type(shannon_source_coding, information_standard).

% Network relationships (structural influence edges)
% This theorem is a foundational prerequisite for many other technological constraints.
narrative_ontology:affects_constraint(shannon_source_coding, shannon_channel_capacity).
narrative_ontology:affects_constraint(shannon_source_coding, digital_communication_protocols).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% Not applicable for a Mountain constraint where directionality is irrelevant.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */