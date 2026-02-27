% ============================================================================
% CONSTRAINT STORY: fermat_proof_barrier
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-02-29
% ============================================================================

:- module(constraint_fermat_proof_barrier, []).

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
 *   constraint_id: fermat_proof_barrier
 *   human_readable: The Difficulty of Proving Fermat's Last Theorem
 *   domain: mathematical
 *
 * SUMMARY:
 *   Fermat's Last Theorem, the claim that no three positive integers a, b, and c can satisfy the equation a^n + b^n = c^n for any integer value of n greater than 2, posed a significant barrier to mathematicians for over 350 years. The constraint represents the inherent difficulty and intellectual labor required to overcome this mathematical challenge. This wasn't simply a computational problem; it demanded novel conceptual frameworks.
 *
 * KEY AGENTS (by structural relationship):
 *   - Mathematicians attempting to prove the theorem: Observer (moderate/constrained) — bears the intellectual labor of discovery.
 *   - The broader mathematical community: Observer (institutional/analytical) — benefits from the collective effort and discoveries.
 *   - Analytical observer: Observer (analytical/analytical) — Sees the full structure and consequences of the theorem.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fermat_proof_barrier, 0.20).
domain_priors:suppression_score(fermat_proof_barrier, 0.05).   % Structural property (raw, unscaled). Must be <= 0.05 for Mountain.
domain_priors:theater_ratio(fermat_proof_barrier, 0.05).       % Piton detection (>= 0.70)

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fermat_proof_barrier, extractiveness, 0.20).
narrative_ontology:constraint_metric(fermat_proof_barrier, suppression_requirement, 0.05).
narrative_ontology:constraint_metric(fermat_proof_barrier, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
% These feed the natural_law_signature certification chain in
% structural_signatures.pl. Without these, the NL signature defaults to 0.5
% and fails certification.
narrative_ontology:constraint_metric(fermat_proof_barrier, accessibility_collapse, 0.90).
narrative_ontology:constraint_metric(fermat_proof_barrier, resistance, 0.10).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(fermat_proof_barrier, mountain).
narrative_ontology:human_readable(fermat_proof_barrier, "The Difficulty of Proving Fermat's Last Theorem").
narrative_ontology:topic_domain(fermat_proof_barrier, "mathematical").

% --- Binary flags ---
% narrative_ontology:has_sunset_clause(fermat_proof_barrier).      % Mandatory if Scaffold
% domain_priors:requires_active_enforcement(fermat_proof_barrier). % Required for Tangled Rope

% --- Emergence flag (required for mountain constraints) ---
% Required for the mountain metric gate: without this, the
% classify_from_metrics mountain clause will not fire.
domain_priors:emerges_naturally(fermat_proof_barrier).

% --- Structural relationships (No enrichment needed for Mountain constraints) ---
% As a Mountain constraint (a feature of mathematical reality), the concepts of
% 'beneficiary' and 'victim' do not apply in a structural sense. The constraint
% is uniform for all observers. The 'cost' borne by mathematicians is the
% intellectual labor required for discovery, not an asymmetric extraction imposed
% by another group.
% narrative_ontology:constraint_beneficiary(fermat_proof_barrier, mathematical_community).
% narrative_ontology:constraint_victim(fermat_proof_barrier, mathematicians).

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

% PERSPECTIVE 1: THE WORKING MATHEMATICIAN
% For a natural law constraint (mountain-only), the classification is the same
% from all perspectives. We include multiple perspectives to demonstrate this
% invariance.
constraint_indexing:constraint_classification(fermat_proof_barrier, mountain,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(universal))).

% PERSPECTIVE 2: THE MATHEMATICAL COMMUNITY
% The institutional perspective also sees an unchangeable feature of
% mathematical reality.
constraint_indexing:constraint_classification(fermat_proof_barrier, mountain,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER
% The default analytical context confirms the Mountain classification, which
% is used by the bridge to derive the constraint_claim.
constraint_indexing:constraint_classification(fermat_proof_barrier, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(fermat_proof_barrier_tests).

test(perspectival_agreement) :-
    % Verify perspectival agreement (all perspectives classify as mountain).
    constraint_indexing:constraint_classification(fermat_proof_barrier, mountain, context(agent_power(moderate), _, _, _)),
    constraint_indexing:constraint_classification(fermat_proof_barrier, mountain, context(agent_power(institutional), _, _, _)),
    constraint_indexing:constraint_classification(fermat_proof_barrier, mountain, context(agent_power(analytical), _, _, _)).


test(threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    config:param(suppression_metric_name, SuppMetricName),
    narrative_ontology:constraint_metric(fermat_proof_barrier, ExtMetricName, E),
    narrative_ontology:constraint_metric(fermat_proof_barrier, SuppMetricName, S),
    E =< 0.25, % Mountain extractiveness threshold
    S =< 0.05. % Mountain suppression threshold

:- end_tests(fermat_proof_barrier_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The base extractiveness is set to 0.20 because solving the problem requires significant intellectual effort, time, and resources. However, it's not pure extraction, as the process often yields valuable byproducts (new mathematical techniques, insights). Suppression is very low (0.05) because there are no active attempts to prevent mathematicians from working on the problem; its inherent difficulty naturally limits the number of successful attempts. The theater ratio is low (0.05) because the focus is genuinely on solving the mathematical problem, not on performative aspects. The constraint is a Mountain because it is a feature of mathematical reality, not a socially constructed rule.

 * PERSPECTIVAL GAP:
 *   There is no perspectival gap. As a mathematical truth, the constraint is a Mountain from all perspectives. The moderate perspective acknowledges the struggle of discovery, while the institutional and analytical perspectives recognize the profound implications and the unwavering nature of the mathematical truth.
 *
 * DIRECTIONALITY LOGIC:
 *   As a Mountain constraint (a feature of mathematical reality), the concepts of 'beneficiary' and 'victim' do not apply in a structural sense. The constraint is uniform for all observers. The 'cost' borne by mathematicians is the intellectual labor required for discovery, not an asymmetric extraction imposed by another group. The 'benefit' to the community is the expansion of collective knowledge. For these reasons, beneficiary/victim declarations are omitted as they are not structurally meaningful for a natural law.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as a Mountain prevents mislabeling the intellectual labor of discovery as a form of social extraction. The theorem's difficulty arises from its inherent mathematical structure, not from any artificial extraction mechanism. It's not a rope, as there's no designed coordination; it's a fixed challenge that emerges from the axioms of arithmetic.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_fermat,
    'Is the known proof''s complexity an unavoidable consequence of the theorem''s structure, or could a simpler proof exist?',
    'Further mathematical research and potential discovery of alternative proofs.',
    'If simpler proof exists: Reduced perceived difficulty and wider accessibility. If not: Reinforces the inherent complexity of the theorem and its connection to advanced mathematical concepts.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(fermat_proof_barrier, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Temporal data is not required for low-extraction constraints (ε <= 0.46),
% but is included here to demonstrate the stability of this mathematical constant.
%
% Theater ratio over time (stable):
narrative_ontology:measurement(fermat_proof_barrier_tr_t0, fermat_proof_barrier, theater_ratio, 0, 0.05).
narrative_ontology:measurement(fermat_proof_barrier_tr_t5, fermat_proof_barrier, theater_ratio, 5, 0.05).
narrative_ontology:measurement(fermat_proof_barrier_tr_t10, fermat_proof_barrier, theater_ratio, 10, 0.05).

% Extraction over time (stable):
narrative_ontology:measurement(fermat_proof_barrier_ex_t0, fermat_proof_barrier, base_extractiveness, 0, 0.20).
narrative_ontology:measurement(fermat_proof_barrier_ex_t5, fermat_proof_barrier, base_extractiveness, 5, 0.20).
narrative_ontology:measurement(fermat_proof_barrier_ex_t10, fermat_proof_barrier, base_extractiveness, 10, 0.20).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% Not applicable for a Mountain constraint.
% narrative_ontology:coordination_type(fermat_proof_barrier, information_standard).

% Boltzmann floor override (only if domain knowledge justifies)
% Not applicable for a Mountain constraint.
% narrative_ontology:boltzmann_floor_override(fermat_proof_barrier, 0.20).

% Network relationships (structural influence edges)
% The proof of Fermat's Last Theorem relies on the Taniyama-Shimura conjecture.
narrative_ontology:affects_constraint(taniyama_shimura_conjecture, fermat_proof_barrier).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% Not applicable for a Mountain constraint where directionality is uniform.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */