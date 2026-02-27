% ============================================================================
% CONSTRAINT STORY: hilberts_hotel_infinity
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_hilberts_hotel_infinity, []).

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
 *   constraint_id: hilberts_hotel_infinity
 *   human_readable: Hilbert's Paradox of the Grand Hotel
 *   domain: mathematical/logical
 *
 * SUMMARY:
 *   Hilbert's Hotel is a logical constraint embedded in the structure of
 *   countable infinity. It states: no matter how many guests are accommodated
 *   in a countably infinite hotel, any countable arrival can be
 *   re-accommodated by shifting existing guests. This is not a paradox in
 *   logic but a revelation of how infinite cardinality behaves under
 *   bijection. The constraint has zero degrees of freedom — it emerges
 *   necessarily from the definition of countable infinity via Dedekind's
 *   characterization. No boarding manager can evade it; no alternative
 *   axiomatics within standard mathematics can avoid it. The apparent
 *   'problem' (how to fit new guests) has a forced solution (re-index all
 *   guests) that works at infinite scale but violates finite intuition. The
 *   constraint's classification as Mountain is invariant across all
 *   observables: whether expressed via ordinal induction, Cantor pairing, or
 *   algorithmic re-accommodation, the bijective closure of countable infinity
 *   produces the same structural result.
 *
 * KEY AGENTS:
 *   - Mathematical Formalism: The constraint is inherent in the formal definition of countable infinity; no agent bears or benefits from it
 *   - Intuitive Cognition: Finite minds encounter the constraint as paradox until formal training disambiguates it
 *   - Set Theory Foundation: The axioms of ZFC (or equivalent) generate the constraint necessarily
 *   - Pedagogical System: Mathematics education must teach that intuitions fail at infinity; the constraint is immutable but understanding can improve
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hilberts_hotel_infinity, 0.08).
domain_priors:suppression_score(hilberts_hotel_infinity, 0.02).
domain_priors:theater_ratio(hilberts_hotel_infinity, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hilberts_hotel_infinity, extractiveness, 0.08).
narrative_ontology:constraint_metric(hilberts_hotel_infinity, suppression_requirement, 0.02).
narrative_ontology:constraint_metric(hilberts_hotel_infinity, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(hilberts_hotel_infinity, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(hilberts_hotel_infinity, resistance, 0.03).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hilberts_hotel_infinity, mountain).
narrative_ontology:human_readable(hilberts_hotel_infinity, "Hilbert's Paradox of the Grand Hotel").
narrative_ontology:topic_domain(hilberts_hotel_infinity, "mathematical/logical").

domain_priors:emerges_naturally(hilberts_hotel_infinity).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: FORMAL MATHEMATICAL STRUCTURE (MOUNTAIN) — The constraint is the logical structure of countable infinity itself. For any countably infinite set N and any countable subset S, there exists a bijection that exhausts S within N without remainder. This is invariant across all observables and representations. Zero degrees of freedom.
constraint_indexing:constraint_classification(hilberts_hotel_infinity, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 2: SET-THEORETIC FOUNDATION (MOUNTAIN) — Cantor's definition of countable infinity via the Dedekind-infinite property creates an irreducible constraint: no finite procedure can enumerate the set in time; no finite resource constraint can prevent re-accommodation. The constraint emerges necessarily from the axioms of ZFC set theory.
constraint_indexing:constraint_classification(hilberts_hotel_infinity, mountain,
    context(agent_power(analytical),
            time_horizon(generational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 3: INTUITIVE OBSERVER (MOUNTAIN) — The 'paradox' dissolves when the constraint is recognized: our intuitions about finite collections do not transfer to infinite ones. This is not a failure of logic but a boundary of finite cognition. The constraint (countable infinity's bijective closure) is immutable. What changes is understanding, not the constraint's structure.
constraint_indexing:constraint_classification(hilberts_hotel_infinity, mountain,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 4: MATHEMATICAL EDUCATION SYSTEM (MOUNTAIN) — The constraint functions pedagogically: no finite model can make countable infinity intuitive; the gap between finite and infinite cannot be bridged by intuition alone. Students encounter an irreducible conceptual barrier that formal proof must overcome. This is not a flaw in pedagogy but a structural feature of how infinite mathematics enters finite minds.
constraint_indexing:constraint_classification(hilberts_hotel_infinity, mountain,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hilberts_hotel_infinity_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(hilberts_hotel_infinity, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(hilberts_hotel_infinity, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(hilberts_hotel_infinity, ExtMetricName, E),
    domain_priors:suppression_score(hilberts_hotel_infinity, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(hilberts_hotel_infinity),
    narrative_ontology:constraint_metric(hilberts_hotel_infinity, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(hilberts_hotel_infinity, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(hilberts_hotel_infinity_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.08): Minimal. The constraint imposes no asymmetric extraction — no agent gains at another's expense. The re-accommodation is a neutral operation on the formal structure. The value reflects only that any logical constraint imposes some minimal cognitive cost to understand it. Suppression (0.02): Nearly absent. The constraint suppresses no alternatives — it is the only possible outcome given Dedekind infinity. Finite alternatives are simply impossible given the definitions. Theater ratio (0.15): Low. The formal proof requires no performative scaffolding. Pedagogically, the thought experiment uses theatrical language ('hotel,' 'guests,' 'manager') to communicate a formal result, but the underlying mathematics is transparent. As formalism, theater ratio is near-zero; the small value reflects the pedagogical staging.
 *
 * PERSPECTIVAL GAP:
 *   All perspectives converge on Mountain classification. The constraint is invariant across observables: the formal mathematician, the pedagogical educator, the set-theorist, and the intuition-confronting student all encounter the same irreducible structural property. There is no perspectival gap because the constraint has no degrees of freedom for observation to modulate. This uniformity is itself the diagnostic signature of a true Mountain — not because perspectives happen to agree, but because the constraint's structure admits no perpective-dependent variation.
 *
 * DIRECTIONALITY LOGIC:
 *   No beneficiary/victim structure exists. The constraint operates on formal structures (countable infinite sets) that have no agents. There is no extraction relationship, no power asymmetry, no directionality. The directionality derivation chain does not apply to this constraint — it is an example of a pure mathematical boundary, where the six-type classification system reduces to a single type (Mountain) across all contexts. This is one of the rare cases where the DR framework reveals that a concept has no agent-relative degrees of freedom.
 *
 * MANDATROPHY ANALYSIS:
 *   No mandatrophy resolution needed. Hilbert's Hotel is a Mountain from all perspectives, which satisfies the logical requirement that natural laws exhibit zero degrees of freedom. The thought experiment's apparent paradox is fully resolved by recognizing the constraint's structural necessity — it is not possible to classify this as Snare (extraction exists), Rope (coordination exists), or any hybrid type because no agents are present and no asymmetry exists. The constraint is pure mathematical boundary: as immutable as the speed of light, as indifferent to perspective as the Halting Problem. The pedagogical difficulty (intuitive paradox) does not change the constraint's type; it merely reflects that finite cognition must learn to work within an infinite boundary.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    intuition_gap_nature,
    'Is the intuitive paradox a property of Hilbert''s Hotel (cognitive illusion) or a revealed feature of infinite set structure (deep insight)?',
    'Compare intuitive difficulty of Hilbert''s Hotel with other bijective constructions (e.g., Cantor pairing function, ordinal arithmetic). If all carry similar cognitive resistance, the gap is cognitive; if Hilbert''s is uniquely difficult, the hotel structure may reveal something special.',
    'If cognitive illusion: constraint is purely structural (mountain). If revealed feature: constraint may have pedagogical or conceptual substructure worth studying separately.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(intuition_gap_nature, conceptual, 'Whether the intuitive paradox reflects cognition or deep structure').

omega_variable(
    cardinality_choice_dependence,
    'Does the re-accommodation property depend on the Axiom of Choice, making it contingent rather than universal?',
    'Examine whether the Dedekind-infinite property (core of Hilbert''s constraint) requires Choice. In ZF without Choice, do all infinite sets admit the bijection that exhausts new arrivals?',
    'If Choice-dependent: constraint is weaker in constructive mathematics; mountain classification is relative to ZFC. If Choice-independent: constraint is truly universal; mountain status is absolute.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cardinality_choice_dependence, empirical, 'Choice-dependence of countable infinity bijection').

omega_variable(
    infinity_type_universality,
    'Does the constraint apply equally to all infinite cardinalities (ℵ₀, 2^ℵ₀, etc.) or does it break down at uncountable infinities?',
    'Test whether Hilbert''s re-accommodation works for uncountably many new guests. The constraint depends on the arrival set being countable relative to the hotel cardinality.',
    'If universal: constraint is a property of cardinality hierarchy, not specific to countable infinity. If cardinality-specific: constraint is narrower; enables decomposition into countable vs uncountable stories.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(infinity_type_universality, empirical, 'Universality across infinite cardinalities').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hilberts_hotel_infinity, 0, 1000).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hilbert_tr_t0, hilberts_hotel_infinity, theater_ratio, 0, 0.1).
narrative_ontology:measurement(hilbert_tr_t500, hilberts_hotel_infinity, theater_ratio, 500, 0.15).
narrative_ontology:measurement(hilbert_tr_t1000, hilberts_hotel_infinity, theater_ratio, 1000, 0.15).

% Extraction over time
narrative_ontology:measurement(hilbert_be_t0, hilberts_hotel_infinity, base_extractiveness, 0, 0.08).
narrative_ontology:measurement(hilbert_be_t500, hilberts_hotel_infinity, base_extractiveness, 500, 0.08).
narrative_ontology:measurement(hilbert_be_t1000, hilberts_hotel_infinity, base_extractiveness, 1000, 0.08).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hilberts_hotel_infinity, information_standard).
narrative_ontology:affects_constraint(hilberts_hotel_infinity, cantor_cardinality_hierarchy).
narrative_ontology:affects_constraint(hilberts_hotel_infinity, dedekind_infinite_property).

% DUAL FORMULATION NOTE:
% Hilbert's Hotel is the intuitive gateway to countable infinity's bijective closure property. The constraint decomposes into: (1) the Dedekind-infinite property (ε ≈ 0.05, Mountain — the formal definition), (2) the Cantor cardinality hierarchy (ε ≈ 0.10, Mountain — the broader context of infinite sets), and (3) Hilbert's pedagogical thought experiment (ε ≈ 0.08, this story — the communication vehicle). All three stories are Mountains; Hilbert's is the most conceptually accessible entry point.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
