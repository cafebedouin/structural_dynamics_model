% ============================================================================
% CONSTRAINT STORY: hydra_game
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_hydra_game, []).

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
 *   constraint_id: hydra_game
 *   human_readable: The Hydra Game (Kirby-Paris Theorem)
 *   domain: mathematical_logic/proof_theory
 *
 * SUMMARY:
 *   The Hydra Game (Kirby-Paris Theorem) is a mathematical constraint that
 *   emerges from the structure of formal proof theory itself. The game is
 *   played on a finite rooted tree where a player selects a leaf, removes it,
 *   and the tree regenerates according to fixed rules. The constraint is that
 *   the game always terminates in finite time, but this termination cannot be
 *   proven within Peano Arithmetic (PA), despite being provable in stronger
 *   systems like Zermelo-Fraenkel set theory (ZFC) with ordinal strength
 *   analysis. The Kirby-Paris theorem (1982) establishes this unprovability
 *   rigorously. The constraint is natural law: no agent, no matter how
 *   resourced or positioned, can construct a PA proof of Hydra termination.
 *   The unprovability is a consequence of Gödel's Second Incompleteness
 *   Theorem applied to the specific structure of the Hydra game. This makes
 *   it a pure mountain constraint — invariant across all observational
 *   perspectives, all time horizons, all exit options, and all spatial
 *   scopes.
 *
 * KEY AGENTS:
 *   - Mathematical Community: Institutional (arbitrage) — collectively establishes and certifies the existence of the unprovability constraint; has access to ZFC proofs but cannot weaken them to PA
 *   - Peano Arithmetic System: Formal system (analytical) — the reference formal system within which unprovability is defined; PA itself cannot transcend its own proof-theoretic strength
 *   - Proof Theorist: Individual researcher (powerful/mobile) — attempts to prove or disprove Hydra termination within PA; encounters the structural limit regardless of resources or mobility
 *   - Computational Implementation: Technical actor (organized/constrained) — can execute the Hydra game algorithm and observe empirical termination; constrained by the fact that empirical observation cannot substitute for formal proof within PA
 *   - Analytical Observer: Meta-perspective (analytical/analytical) — recognizes the constraint as a structural property of proof theory itself, not a contingent limitation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hydra_game, 0.08).
domain_priors:suppression_score(hydra_game, 0.02).
domain_priors:theater_ratio(hydra_game, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hydra_game, extractiveness, 0.08).
narrative_ontology:constraint_metric(hydra_game, suppression_requirement, 0.02).
narrative_ontology:constraint_metric(hydra_game, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(hydra_game, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(hydra_game, resistance, 0.04).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hydra_game, mountain).
narrative_ontology:human_readable(hydra_game, "The Hydra Game (Kirby-Paris Theorem)").
narrative_ontology:topic_domain(hydra_game, "mathematical_logic/proof_theory").

domain_priors:emerges_naturally(hydra_game).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PURE MATHEMATICAL STRUCTURE (MOUNTAIN) — The Hydra Game is a finite, well-defined rooted tree with a deterministic termination property proven by Kirby-Paris. The constraint is the unprovability of termination within Peano Arithmetic — a structural limit imposed by Gödel's Second Incompleteness Theorem. No agent can escape this; no exit options exist. ε=0.08, suppression=0.02. The constraint is natural law.
constraint_indexing:constraint_classification(hydra_game, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 2: THE PROOF THEORIST (MOUNTAIN) — Even maximally resourced agents (well-funded research groups, leading universities, interdisciplinary teams) cannot prove Hydra termination within PA. The unprovability is structural, not due to resource scarcity. ε=0.08, suppression=0.02. Powerful agents with arbitrage exit options discover that the constraint is immovable.
constraint_indexing:constraint_classification(hydra_game, mountain,
    context(agent_power(powerful),
            time_horizon(civilizational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 3: FORMAL SYSTEMS INSTITUTION (MOUNTAIN) — Institutions (proof theory research programs, mathematics departments) cannot mandate a PA proof of Hydra termination through enforcement or resource allocation. The constraint is logical, not institutional. ε=0.08, suppression=0.02. Even institutional power encounters the structural limit.
constraint_indexing:constraint_classification(hydra_game, mountain,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: THE GRADUATE STUDENT (MOUNTAIN) — A graduate student attempting to prove Hydra termination within PA will eventually encounter the Kirby-Paris theorem, which mathematically establishes that the proof is impossible within that formal system. The constraint is equally inescapable from a constrained position. ε=0.08, suppression=0.02.
constraint_indexing:constraint_classification(hydra_game, mountain,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hydra_game_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(hydra_game, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(hydra_game, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(hydra_game, ExtMetricName, E),
    domain_priors:suppression_score(hydra_game, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(hydra_game),
    narrative_ontology:constraint_metric(hydra_game, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(hydra_game, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(hydra_game_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.08): Minimal. The constraint does not extract value from one agent to benefit another. No redistribution occurs. The unprovability is a structural limit on ALL agents equally — neither beneficiaries nor victims exist in the economic sense. The low extractiveness reflects that the Hydra game is a pure structural constraint, not a coordination or coercion mechanism. Suppression (0.02): Negligible. There are no alternatives being suppressed. The unprovability is not maintained by coercion or denial of exit options; it simply is a mathematical fact. PA's inability to prove Hydra termination is not due to active suppression but due to the formal system's mathematical depth. Theater ratio (0.15): Very low. The constraint is non-performative. Proofs that Hydra terminates are constructive and necessarily involve ordinal analysis; there is no theatrical dimension to the unprovability. The slight non-zero value (0.15 vs 0.0) accounts for the fact that the presentation of the constraint in pedagogy and research communication has some expository framing, but the core mathematical content is pure.
 *
 * PERSPECTIVAL GAP:
 *   INVARIANT ACROSS ALL PERSPECTIVES. This constraint classifies as Mountain from every index tuple. The powerful proof theorist with arbitrage exit options encounters the same unprovability as the constrained graduate student. The immediate-horizon institutional view (formal systems department seeking quick results) meets the same structural limit as the civilizational-horizon analytical observer. The local mathematician and the universal mathematician both confront the fact that PA cannot prove Hydra termination. This invariance is the hallmark of a true mountain constraint: the classification does not depend on power, time horizon, exit options, or scope. All six agent perspectives produce Mountain. This is NOT a weak design (too uniform) — it is evidence of structural robustness. The constraint is a natural law.
 *
 * DIRECTIONALITY LOGIC:
 *   Mountain constraints do not have beneficiaries or victims. No agent benefits; no agent is extracted from. The constraint is a structural limit on the proof-theoretic power of Peano Arithmetic. All agents experience it as an objective fact, not as a social arrangement. The directionality framework does not apply to mountains because there is no asymmetric relationship to exploit. The ordinal assignment function that proves Hydra termination in ZFC (typically ordinal ε₀ or higher) is a mathematical object, not a power relation.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    formal_system_adequacy,
    'Is Peano Arithmetic the ''correct'' reference formal system for evaluating Hydra unprovability, or does the choice of formal system (ZFC, Type Theory, Infinitary Logic) change the structural constraint?',
    'Proof-theoretic analysis of Hydra termination across different formal systems; comparison of ordinal strength required vs ordinal strength of each system',
    'If all natural formal systems require ordinal strength beyond their capacity: Hydra unprovability is a universal structural limit (mountain confirmed). If some weak systems contain PA proofs of termination: constraint is system-relative, not universal.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(formal_system_adequacy, empirical, 'Whether Hydra unprovability is universal or system-dependent').

omega_variable(
    ordinal_assignment_uniqueness,
    'Is the ordinal assignment function that proves Hydra termination in ZFC structurally unique, or are multiple incomparable ordinal-based proofs possible?',
    'Proof-theoretic comparison of known Hydra termination proofs; analysis of which ordinals suffice and whether different ordinals encode the same structural termination argument',
    'If ordinal assignment is essentially unique: suggests deep structural necessity (mountain confirmed). If multiple incomparable proofs exist: suggests contingency in proof methodology.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ordinal_assignment_uniqueness, empirical, 'Whether ordinal termination proofs are structurally unique').

omega_variable(
    computational_tractability_bound,
    'Is there a finite computational bound on the Hydra game length for trees up to a given size, even if the proof is unprovable in PA?',
    'Explicit computation of Hydra game termination times for small trees; comparison with theoretical ordinal bounds; assessment of whether empirical termination times falsify the theoretical proof',
    'If computational bounds exist and are tighter than theory predicts: empirical mathematics reveals constraint that formal theory cannot formalize (epistemically interesting but not a different mountain). If computational results perfectly match theory: constraint is fully captured by the formal structure.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(computational_tractability_bound, empirical, 'Whether computational bounds on Hydra game length are tractable').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hydra_game, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hydra_tr_t0, hydra_game, theater_ratio, 0, 0.1).
narrative_ontology:measurement(hydra_tr_t50, hydra_game, theater_ratio, 50, 0.15).
narrative_ontology:measurement(hydra_tr_t100, hydra_game, theater_ratio, 100, 0.15).

% Extraction over time
narrative_ontology:measurement(hydra_be_t0, hydra_game, base_extractiveness, 0, 0.05).
narrative_ontology:measurement(hydra_be_t50, hydra_game, base_extractiveness, 50, 0.08).
narrative_ontology:measurement(hydra_be_t100, hydra_game, base_extractiveness, 100, 0.08).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hydra_game, information_standard).
narrative_ontology:affects_constraint(hydra_game, godel_incompleteness_theorem).
narrative_ontology:affects_constraint(hydra_game, peano_arithmetic_completeness_limits).
narrative_ontology:affects_constraint(hydra_game, ordinal_proof_strength).

% DUAL FORMULATION NOTE:
% The Hydra Game constraint family includes three related structures: (1) the Hydra Game itself (finite tree manipulation with termination proof), (2) the unprovability within PA (proof-theoretic constraint), and (3) the ordinal strength required for proof in ZFC (hierarchical constraint on formal systems). These are not separate constraints but a single constraint viewed at different levels of formalization. The Kirby-Paris theorem unifies all three perspectives. The network links reflect the logical dependencies: Gödel's incompleteness theorems make the unprovability possible; ordinal strength hierarchies enable the ZFC proof. All three constraints share the same ε value (≈0.08) and are all mountains.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
