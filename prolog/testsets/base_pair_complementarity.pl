% ============================================================================
% CONSTRAINT STORY: base_pair_complementarity
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-15
% ============================================================================

:- module(constraint_base_pair_complementarity, []).

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
    domain_priors:emerges_naturally/1,
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
    narrative_ontology:omega_variable/3.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: base_pair_complementarity
 *   human_readable: Specific Base-Pair Pairing in DNA
 *   domain: biological/chemical
 *
 * SUMMARY:
 *   The structure of DNA is constrained by the specific pairing of purine and
 *   pyrimidine bases: Adenine with Thymine (A-T) and Guanine with Cytosine (G-C).
 *   This constraint, dictated by hydrogen bond geometry and steric hindrance,
 *   is a fundamental, unchangeable feature of terrestrial biology that enables
 *   the stable storage and replication of genetic information. It is a natural
 *   law of biochemistry.
 *
 * KEY AGENTS (by structural relationship):
 *   - Nucleotide Base: Primary target (powerless/trapped) — must conform to the
 *     hydrogen-bond geometry.
 *   - DNA Polymerase: Institutional actor (institutional/arbitrage) — "reads"
 *     and enforces the pairing during replication, treating it as a fixed rule.
 *   - Biochemist: Analytical observer — identifies the constraint as a
 *     fundamental law of molecular biology.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(base_pair_complementarity, 0.20).
domain_priors:suppression_score(base_pair_complementarity, 0.05).   % Structural property (raw, unscaled).
domain_priors:theater_ratio(base_pair_complementarity, 0.0).       % Piton detection (>= 0.70)

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(base_pair_complementarity, extractiveness, 0.20).
narrative_ontology:constraint_metric(base_pair_complementarity, suppression_requirement, 0.05).
narrative_ontology:constraint_metric(base_pair_complementarity, theater_ratio, 0.0).

% --- NL Profile Metrics (required for mountain constraints) ---
% These feed the natural_law_signature certification chain in
% structural_signatures.pl.
narrative_ontology:constraint_metric(base_pair_complementarity, accessibility_collapse, 1.0).
narrative_ontology:constraint_metric(base_pair_complementarity, resistance, 0.0).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(base_pair_complementarity, mountain).

% --- Binary flags ---
% No sunset clause. Not a scaffold.
% No active human enforcement required. Not a tangled rope.

% --- Emergence flag (required for mountain constraints) ---
% This constraint emerges from fundamental chemical and physical laws.
domain_priors:emerges_naturally(base_pair_complementarity).

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% Not required for Mountain constraints. The concepts of beneficiary and
% victim do not apply to natural laws in the same way they apply to
% socially constructed constraints. No enrichment needed.

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

% This is a uniform-type constraint (Mountain-only). The classification is
% invariant across all perspectives because it is a natural law.

% PERSPECTIVE 1: THE NUCLEOTIDE BASE (MOUNTAIN)
% For an individual base, the pairing rule is an unchangeable law of its
% existence, dictated by molecular geometry.
constraint_indexing:constraint_classification(base_pair_complementarity, mountain,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: THE CELLULAR MACHINERY (MOUNTAIN)
% For enzymes like DNA polymerase, the pairing rule is a fixed fact of the
% environment, a law to be followed for successful replication.
constraint_indexing:constraint_classification(base_pair_complementarity, mountain,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (MOUNTAIN)
% A biochemist recognizes the constraint as a fundamental, non-negotiable
% principle of molecular biology.
constraint_indexing:constraint_classification(base_pair_complementarity, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(base_pair_complementarity_tests).

test(perspectival_invariance) :-
    % Verify that the classification is Mountain from multiple key perspectives.
    constraint_indexing:constraint_classification(base_pair_complementarity, mountain, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(base_pair_complementarity, mountain, context(agent_power(institutional), _, _, _)),
    constraint_indexing:constraint_classification(base_pair_complementarity, mountain, context(agent_power(analytical), _, _, _)).

test(mountain_threshold_validation) :-
    % Verify metrics are within Mountain thresholds.
    narrative_ontology:constraint_metric(base_pair_complementarity, extractiveness, E),
    narrative_ontology:constraint_metric(base_pair_complementarity, suppression_requirement, S),
    E =< 0.25,
    S =< 0.05.

test(natural_law_profile_validation) :-
    % Verify the NL profile metrics are present and correct for a Mountain.
    narrative_ontology:constraint_metric(base_pair_complementarity, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(base_pair_complementarity, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(base_pair_complementarity_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   - Base Extractiveness (ε=0.20): The constraint "extracts" thermodynamic
 *     stability and informational fidelity from the chemical system. This is a
 *     metaphorical use of extraction, representing the cost of imposing order,
 *     and the value is low, consistent with a Mountain.
 *   - Suppression Score (S=0.05): The score is very low. Suppression in the
 *     Deferential Realism framework refers to the coercive foreclosure of
 *     VIABLE alternatives. Here, alternatives like A-G pairing are physically
 *     and chemically impossible due to steric hindrance. Since there are no
 *     viable alternatives to suppress, the suppression score is minimal. A
 *     previous version incorrectly set this to 1.0, confusing physical
 *     impossibility with maximum coercion.
 *   - NL Profile: Accessibility Collapse is 1.0 because no alternative pairing
 *     is conceivable within the double helix geometry. Resistance is 0.0 as
 *     resisting a chemical law is incoherent.
 *
 * PERSPECTIVAL GAP:
 *   There is no perspectival gap. As a natural law, the constraint is a
 *   Mountain from all perspectives. A nucleotide, a cellular enzyme, and a
 *   human scientist all experience it as an unchangeable fact of reality.
 *
 * DIRECTIONALITY LOGIC:
 *   As a Mountain constraint, beneficiary/victim declarations are not
 *   structurally required. The constraint does not have a "direction" in the
 *   social sense; it is a symmetric, background condition of the system.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as a Mountain is robust. The low extraction and
 *   suppression scores, combined with the `emerges_naturally` flag and the
 *   complete NL profile, prevent misclassification. The system correctly
 *   identifies this as a feature of reality, not a designed system of
 *   coordination or extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% Omega variables — open questions the framework cannot yet resolve
%
% /5 form: narrative detail for story context
omega_variable(
    omega_base_pair_complementarity,
    "Can bases exist in alternative tautomeric states that allow 'illegal' pairings, causing mutations?",
    "Biochemical analysis of point mutation rates and quantum chemical modeling of tautomer stability.",
    "If common, the 'perfect' Mountain has stochastic flaws, making it a source of genetic 'noise' (extraction from fidelity). If rare, the Mountain classification holds.",
    confidence_without_resolution(high)
).

% /3 form: typed classification for reporting engine (REQUIRED)
narrative_ontology:omega_variable(omega_base_pair_complementarity, empirical, "Whether tautomeric shifts allow alternative base pairings, introducing errors.").

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(base_pair_complementarity, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Not applicable. Base extractiveness is below the 0.46 threshold for
% mandatory temporal tracking. As a natural law, its properties are static.

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Not applicable for this fundamental constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% Not applicable. As a Mountain, directionality is not a relevant concept.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */