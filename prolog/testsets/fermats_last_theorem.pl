% ============================================================================
% CONSTRAINT STORY: fermats_last_theorem
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [RESOLVED MATHEMATICALLY]
% ============================================================================

:- module(constraint_fermats_last_theorem, []).

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
 *   constraint_id: fermats_last_theorem
 *   human_readable: The Difficulty of Proving Fermat's Last Theorem
 *   domain: mathematical/number_theory
 *
 * SUMMARY:
 *   Fermat's Last Theorem presents a paradigmatic case of a mathematical
 *   constraint that is genuinely irreducible. For 358 years, the statement
 *   'no three positive integers a, b, c can satisfy a^n + b^n = c^n for
 *   integer n > 2' resisted proof despite efforts by the world's greatest
 *   mathematicians. The constraint is not social, institutional, or
 *   extractive—it is structural to the mathematical knowledge system itself.
 *   The difficulty of proving FLT emerges from the logical architecture of
 *   number theory: the proof requires the Taniyama-Shimura conjecture
 *   (connecting elliptic curves to modular forms), Frey's observation linking
 *   FLT to elliptic curve properties, Ribet's proof that a counterexample
 *   would violate modularity, and finally Wiles's 100-page proof of the
 *   Taniyama-Shimura conjecture. Each prerequisite required decades of
 *   foundational work. The constraint classifies as Mountain from all
 *   perspectives because no observer—individual mathematician, mathematical
 *   community, institution, or outside agent—could bypass the logical
 *   requirement. The extractiveness remains stable and low (ε ≈ 0.12) across
 *   the entire interval because the constraint's structure does not change:
 *   it is an immutable feature of mathematical knowledge.
 *
 * KEY AGENTS:
 *   - Individual Mathematicians (powerless/analytical): Euler, Legendre, Lamé, Kummer, Fermat himself—each constrained by the logical prerequisites they could not yet access. No individual could exit this constraint.
 *   - Mathematical Community (organized/constrained): Collected efforts of number theory specialists across continents and centuries. Despite coordination, the community was collectively powerless against the logical barrier.
 *   - Mathematical Institutions (institutional/analytical): Universities, academies, societies provided infrastructure but could not overcome the proof's structural requirements.
 *   - Analytical Observer (analytical/analytical): Sees the constraint as a feature of mathematical knowledge itself—not a social phenomenon but a logical one.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fermats_last_theorem, 0.12).
domain_priors:suppression_score(fermats_last_theorem, 0.03).
domain_priors:theater_ratio(fermats_last_theorem, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fermats_last_theorem, extractiveness, 0.12).
narrative_ontology:constraint_metric(fermats_last_theorem, suppression_requirement, 0.03).
narrative_ontology:constraint_metric(fermats_last_theorem, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(fermats_last_theorem, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(fermats_last_theorem, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(fermats_last_theorem, mountain).
narrative_ontology:human_readable(fermats_last_theorem, "The Difficulty of Proving Fermat's Last Theorem").
narrative_ontology:topic_domain(fermats_last_theorem, "mathematical/number_theory").

domain_priors:emerges_naturally(fermats_last_theorem).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CONTEMPORARY MATHEMATICIAN (MOUNTAIN) — A working mathematician in any era from 1637–1995 faces an immutable logical barrier: the theorem's proof requires tools not yet invented or understood. The constraint is not social or institutional but structural to mathematical knowledge itself. No amount of effort by individual mathematicians could overcome the barrier without centuries of advancement in algebraic number theory, elliptic curves, and modular forms. The powerless agent experiences this as a ceiling: genuine mathematical limits, not suppression or extraction.
constraint_indexing:constraint_classification(fermats_last_theorem, mountain,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 2: MATHEMATICAL COMMUNITY (MOUNTAIN) — Even coordinated international effort in number theory cannot bypass logical necessity. The constraint persists because the underlying mathematical structure requires proof techniques that depend on prior theorems (Taniyama-Shimura conjecture, Frey curves, Ribet's theorem) that took decades to establish. The community has no 'arbitrage' option: they cannot choose an easier path. This is collective powerlessness against a structural mathematical limit.
constraint_indexing:constraint_classification(fermats_last_theorem, mountain,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 3: ANALYTICAL OBSERVER (MOUNTAIN) — From a civilizational and universal perspective, Fermat's Last Theorem is a statement about the logical structure of number theory itself. The constraint is not that 'proving FLT is hard' but that 'proving FLT requires a coherent mathematical framework.' This constraint emerges from the axiomatic nature of mathematics: given Peano arithmetic and the axioms of set theory, certain propositions require certain proof structures. The difficulty is not an artifact of institutional delay or suppression—it is an irreducible feature of mathematical knowledge.
constraint_indexing:constraint_classification(fermats_last_theorem, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 4: MATHEMATICAL INSTITUTIONS (MOUNTAIN) — Universities and mathematical societies cannot force a proof of FLT through institutional investment alone. Funding for number theory research, hiring of specialists, and collaborative workshops all contribute to the eventual proof, but the constraint—the logical requirement for specific foundational results—remains independent of institutional structure. The institutions can facilitate the approach to the proof but cannot circumvent the proof's inherent structure.
constraint_indexing:constraint_classification(fermats_last_theorem, mountain,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(fermats_last_theorem_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(fermats_last_theorem, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(fermats_last_theorem, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(fermats_last_theorem, ExtMetricName, E),
    domain_priors:suppression_score(fermats_last_theorem, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(fermats_last_theorem),
    narrative_ontology:constraint_metric(fermats_last_theorem, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(fermats_last_theorem, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(fermats_last_theorem_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.12): Very low, reflecting that the constraint imposes no asymmetric extraction—no agent benefits from other agents' inability to prove the theorem. The difficulty is symmetric: it applies equally to all mathematicians regardless of power or position. The slight non-zero value (rather than 0.00) accounts for the minor benefit captured by Wiles in establishing the proof first, which provides priority and professional recognition. However, this benefit is vastly smaller than the public good of the proof itself (mathematics advances for all). Suppression (0.03): Near zero. There is no active suppression mechanism—no institutional barrier, no information hiding, no cartel preventing access. The constraint emerges purely from logical necessity, not from deliberate coercion. Accessibility collapse (0.92): Very high. The theorem is stated simply and can be understood by secondary-school mathematics students, but accessing the proof requires mastery of algebraic number theory, elliptic curve theory, and modular forms—concepts at the frontier of mathematical knowledge. The gap between statement and proof is maximal. Resistance (0.08): Very low. Once the proof was discovered, it faced no resistance—the mathematical community immediately accepted it. There is no countervailing force or alternative framework that rejects the proof.
 *
 * PERSPECTIVAL GAP:
 *   Unlike typical constraints that show dramatic perspectival gaps, FLT classifies as Mountain from all perspectives because the logical barrier is truly invariant across observation positions. A powerless mathematician, the organized community, institutions, and the analytical observer all face the same immutable constraint: the proof requires specific prerequisite theorems. The perspectival gap is not a gap—it is consensus. All perspectives converge on Mountain. This uniformity is itself diagnostic: it confirms that the constraint is structural (mathematical) rather than social (institutional). If the constraint were actually institutional—if it derived from gatekeeping, suppression, or extraction—then perspectives would diverge: the beneficiary would see Rope while the victim would see Snare. The fact that no such divergence occurs validates the Mountain classification.
 *
 * MANDATROPHY ANALYSIS:
 *   FLT resolves the mandatrophy trivially: all perspectives produce Mountain, all extractiveness and suppression metrics are below their thresholds, all omegas remain unresolved at low confidence, and the constraint emerges naturally from logical necessity. There is no risk of mislabeling FLT as pure extraction (Snare) disguised as a natural law—the structural data is unambiguous. No beneficiaries, no victims, no asymmetric extraction, no theater. The mandatrophy resolution is the ABSENCE of doubt: when all perspectives agree on Mountain and all metrics support it, the constraint is genuinely a natural law, not a social phenomenon masquerading as one.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    proof_necessity_vs_contingency,
    'Is the 350-year gap between conjecture and proof a necessary feature of mathematical knowledge, or could faster proof techniques have been discovered earlier?',
    'Analysis of the proof dependency graph: Taniyama-Shimura → Frey curves → Ribet''s theorem → Wiles''s proof. Examine whether any component required centuries of prerequisite development or whether faster routes existed but were not explored.',
    'If necessary: the constraint is purely mathematical (Mountain, ε ≤ 0.25). If contingent: the constraint includes institutional path-dependence (Tangled Rope, ε > 0.30). The distinction determines whether FLT exemplifies irreducible mathematical difficulty or path-dependent research allocation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(proof_necessity_vs_contingency, conceptual, 'Whether the 350-year gap reflects mathematical necessity or contingent research paths').

omega_variable(
    computational_verification_vs_proof,
    'Does computational verification of FLT for all exponents up to 4×10^18 constitute an alternative form of justification, or is a closed-form proof structurally necessary for mathematical certainty?',
    'Philosophical analysis of proof standards in mathematics; examination of how other conjectures (Goldbach, twin primes) are justified via computation vs proof.',
    'If computation suffices: the constraint is not purely mathematical but institutional-epistemological (Rope or Scaffold, ε ≤ 0.45). If proof is necessary: the constraint is purely mathematical (Mountain, ε ≤ 0.25).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(computational_verification_vs_proof, conceptual, 'Whether proof is necessary beyond computational verification').

omega_variable(
    wiles_proof_accessibility,
    'Is Wiles''s proof the unique proof of FLT, or do alternative proofs exist that might have been discovered via different research paths?',
    'Ongoing mathematical research for simplifications or alternative approaches; analysis of proof structure to identify whether modularity and elliptic curves are necessary or just sufficient.',
    'If unique: the constraint structure is more rigid (Mountain). If alternatives exist: the constraint includes elements of discovery path (Tangled Rope with lower ε).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(wiles_proof_accessibility, empirical, 'Whether Wiles''s proof is the only possible proof structure').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fermats_last_theorem, 1637, 1995).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fermat_tr_t0, fermats_last_theorem, theater_ratio, 0, 0.08).
narrative_ontology:measurement(fermat_tr_t175, fermats_last_theorem, theater_ratio, 175, 0.12).
narrative_ontology:measurement(fermat_tr_t350, fermats_last_theorem, theater_ratio, 350, 0.15).

% Extraction over time
narrative_ontology:measurement(fermat_be_t0, fermats_last_theorem, base_extractiveness, 0, 0.1).
narrative_ontology:measurement(fermat_be_t175, fermats_last_theorem, base_extractiveness, 175, 0.11).
narrative_ontology:measurement(fermat_be_t350, fermats_last_theorem, base_extractiveness, 350, 0.12).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(fermats_last_theorem, information_standard).
narrative_ontology:affects_constraint(fermats_last_theorem, bgs_spectral_universality).
narrative_ontology:affects_constraint(fermats_last_theorem, halting_problem_undecidability).

% DUAL FORMULATION NOTE:
% FLT is a canonical example of a mathematical constraint that does not decompose into multiple ε values across observables. The theorem is either true or false; its proof either exists or does not. There is no measurement-basis dependence. The constraint family includes other irreducible mathematical theorems (Gödel's Incompleteness, Halting Problem) that exhibit similar Mountain classifications. These are linked not by causal dependence but by their shared property: emergence from logical necessity.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
