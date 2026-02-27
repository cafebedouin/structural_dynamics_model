% ============================================================================
% CONSTRAINT STORY: cap_theorem
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_cap_theorem, []).

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
    constraint_indexing:constraint_classification/3,
    domain_priors:emerges_naturally/1,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: cap_theorem
 *   human_readable: CAP Theorem (Brewer's Theorem)
 *   domain: technological/distributed_systems
 *
 * SUMMARY:
 *   The CAP theorem (formalized by Eric Brewer, proven by Lynch and Gilbert)
 *   establishes that any distributed data storage system can guarantee at
 *   most two of three properties: Consistency (every read returns the most
 *   recent write), Availability (every request receives a response), and
 *   Partition Tolerance (the system continues operating during network
 *   partitions). This constraint is invariant across all implementations,
 *   market conditions, engineering skill levels, and economic incentives. It
 *   is not a friction cost that can be engineered away, not a regulation that
 *   can be reformed, not a coordination problem that can be solved by
 *   coalition. It is a logical consequence of the structure of distributed
 *   information systems operating under adversarial network conditions. All
 *   major database systems (PostgreSQL, MongoDB, Cassandra, Dynamo, Spanner)
 *   recognize and design around this constraint by explicitly choosing their
 *   position in the CA-CP-AP space.
 *
 * KEY AGENTS:
 *   - Distributed Systems Theory: Analytical observer — derives the theorem from first principles; sees pure mathematical constraint
 *   - Systems Architects: Powerful/analytical — design production systems; face the constraint as an unavoidable tradeoff; cannot escape through engineering
 *   - Database Vendors: Institutional/arbitrage — implement systems that choose two of three; compete on which two they prioritize
 *   - Application Developers: Moderate/constrained — must select databases that match their consistency/availability requirements; face the tradeoff indirectly through vendor choices
 *   - Network Infrastructure: Powerless/trapped — cannot prevent partitions; subjected to the constraint's reality
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(cap_theorem, 0.12).
domain_priors:suppression_score(cap_theorem, 0.03).
domain_priors:theater_ratio(cap_theorem, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(cap_theorem, extractiveness, 0.12).
narrative_ontology:constraint_metric(cap_theorem, suppression_requirement, 0.03).
narrative_ontology:constraint_metric(cap_theorem, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(cap_theorem, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(cap_theorem, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(cap_theorem, mountain).
narrative_ontology:human_readable(cap_theorem, "CAP Theorem (Brewer's Theorem)").
narrative_ontology:topic_domain(cap_theorem, "technological/distributed_systems").

domain_priors:emerges_naturally(cap_theorem).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THEORETICAL CONSENSUS (MOUNTAIN) — The CAP theorem is a proven mathematical constraint on distributed systems. From a formal logic perspective, any distributed database must sacrifice one of three properties. This is invariant across all implementations, architectures, and design choices. Zero degrees of freedom. The constraint emerges from the incompleteness of information propagation across partitioned networks.
constraint_indexing:constraint_classification(cap_theorem, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 2: SYSTEMS ARCHITECT (MOUNTAIN) — Even with unlimited resources and engineering skill, the CAP constraint cannot be circumvented. The architect cannot buy their way out, cannot design around it, cannot implement a system that violates the theorem. The constraint is a ceiling on design possibility, not a friction cost. Recognized by every major database vendor as an immutable architecture tradeoff.
constraint_indexing:constraint_classification(cap_theorem, mountain,
    context(agent_power(powerful),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: ENGINEERING ORGANIZATION (MOUNTAIN) — Distributed systems teams across decades and continents have arrived independently at the same conclusion: all production systems choose two of three. No organizational coalition, standards body, or market competition has produced a system that violates CAP. The constraint persists as a structural reality independent of economic incentives or collective action.
constraint_indexing:constraint_classification(cap_theorem, mountain,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 4: DATABASE DEVELOPER (MOUNTAIN) — Individual engineers implementing distributed data stores encounter the CAP constraint as an unavoidable design forcing function. Choosing which two of three to provide is not optional; it is the primary decision. The constraint appears as a natural law of system behavior, not as an external rule imposed on them.
constraint_indexing:constraint_classification(cap_theorem, mountain,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(cap_theorem_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(cap_theorem, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(cap_theorem, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(cap_theorem, ExtMetricName, E),
    domain_priors:suppression_score(cap_theorem, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(cap_theorem),
    narrative_ontology:constraint_metric(cap_theorem, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(cap_theorem, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(cap_theorem_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.12): Minimal. The constraint imposes no extraction from any party — it does not transfer resources or opportunities from one agent to another. It is a purely structural limit, not a distribution mechanism. The value is near-zero, reflecting only the minor friction of having to make an explicit choice among three options. Suppression (0.03): Negligible. The constraint suppresses no alternatives — engineers are free to choose any two of the three; there is no coercion or reduction of choices. The small value reflects only the fact that one choice (the third guarantee) must be forgone. Theater ratio (0.15): Very low. The constraint exhibits no performative content. Its function is purely technical: it partitions design possibility space. There is no gap between the declared function (mathematical proof) and actual behavior. The small value reflects minor pedagogical theater in how the theorem is taught (simplified examples vs full formal proof).
 *
 * PERSPECTIVAL GAP:
 *   All four perspectives classify as Mountain because the CAP constraint is invariant across all observation points. No perspectival gap exists — the theoretical consensus, the architect, the organization, and the developer all encounter the same immutable constraint. This uniformity is diagnostic: it indicates a genuine natural law of distributed systems. Unlike the verification bottleneck exemplar (which showed all six types from different perspectives of a contingent institutional arrangement), the CAP constraint exhibits no perspectival variance. This is the signature of a true mountain.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is not applicable to mountain constraints. There is no extraction flow, no beneficiary/victim structure, and no power asymmetry to model. The constraint operates identically regardless of the observer's power level, exit options, or time horizon. Every agent experiences the same logical ceiling on design possibility.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    partition_tolerance_necessity,
    'Is partition tolerance truly optional, or do all practical distributed systems require it because network partitions are empirically inevitable?',
    'Analysis of production systems claiming CA (no partition tolerance): examine failure modes when network partitions occur; compare stated guarantees to actual behavior under adverse conditions',
    'If partition tolerance is empirically unavoidable: effective CAP reduces to CP vs AP choice in practice. The theorem remains mathematical but the practical space is narrower.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(partition_tolerance_necessity, empirical, 'Whether partition tolerance is truly optional or empirically unavoidable').

omega_variable(
    consistency_definition_boundary,
    'Does the theorem''s definition of ''consistency'' (linearizability) capture all forms of data correctness that systems require, or do weaker consistency models (eventual, causal) represent a different constraint class?',
    'Formal analysis of consistency hierarchy; examination of systems that claim to exceed CAP via weaker consistency models; determine if these systems have simply redefined ''consistency'' rather than violated the theorem',
    'If weaker models are a different constraint: CAP remains invariant for its formal definition but does not address practical system design. If they violate CAP: theorem requires revision.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(consistency_definition_boundary, conceptual, 'Whether consistency definitions beyond linearizability constitute theorem violations').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(cap_theorem, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cap_tr_t0, cap_theorem, theater_ratio, 0, 0.1).
narrative_ontology:measurement(cap_tr_t20, cap_theorem, theater_ratio, 20, 0.15).
narrative_ontology:measurement(cap_tr_t40, cap_theorem, theater_ratio, 40, 0.15).

% Extraction over time
narrative_ontology:measurement(cap_be_t0, cap_theorem, base_extractiveness, 0, 0.1).
narrative_ontology:measurement(cap_be_t20, cap_theorem, base_extractiveness, 20, 0.12).
narrative_ontology:measurement(cap_be_t40, cap_theorem, base_extractiveness, 40, 0.12).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(cap_theorem, eventual_consistency_semantics).
narrative_ontology:affects_constraint(cap_theorem, quorum_consensus_protocols).

% DUAL FORMULATION NOTE:
% The CAP theorem has two decompositions in the literature: (1) Lynch-Gilbert formal proof (2003), which rigorously establishes the incompatibility of strong consistency with partition tolerance; (2) Brewer's original conjecture (2000), which stated the tradeoff informally. Both converge on the same constraint structure. Network links reflect constraints that are downstream of or conditional on CAP: eventual consistency models represent design choices acknowledging the AP or CP selection; consensus protocols (Raft, Paxos) are implementations that explicitly choose the CP position.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
