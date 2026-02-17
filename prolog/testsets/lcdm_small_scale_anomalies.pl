% ============================================================================
% CONSTRAINT STORY: lcdm_small_scale_anomalies
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-05-21
% ============================================================================

:- module(constraint_lcdm_small_scale_anomalies, []).

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
    narrative_ontology:human_readable/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: lcdm_small_scale_anomalies
 *   human_readable: Lambda-CDM Cosmological Model (Small-Scale Structure)
 *   domain: scientific/cosmological
 *
 * SUMMARY:
 *   The Lambda-Cold Dark Matter (ΛCDM) model is the standard paradigm of
 *   cosmology. While extraordinarily successful at describing the universe's
 *   large-scale structure, it faces growing anomalies on smaller, galactic
 *   scales. This constraint story models the paradigm as it applies to these
 *   contested observables (e.g., galaxy formation timelines, satellite
 *   galaxy distributions, Hubble tension), particularly in light of new JWST
 *   data suggesting unexpectedly early star formation. The model acts as a
 *   powerful coordination tool but also suppresses alternative theories and
 *   channels research focus, creating asymmetric outcomes for scientists.
 *
 * KEY AGENTS (by structural relationship):
 *   - Mainstream Cosmologists: Primary beneficiary (institutional/arbitrage) — benefit from the model's coordination function, funding streams, and predictive framework.
 *   - Alternative Model Theorists: Primary target (organized/constrained) — bear the cost of suppression, as their work is marginalized by the dominant paradigm.
 *   - Funding Agencies/Journals: Enforcement actors (institutional) - maintain the paradigm through peer review and resource allocation.
 *   - Analytical Observer: A historian or philosopher of science who sees both the coordination and extraction functions.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(lcdm_small_scale_anomalies, 0.42). % The model's unexplained variance from accumulated anomalies.
domain_priors:suppression_score(lcdm_small_scale_anomalies, 0.65).   % Structural property (raw, unscaled). High due to institutional inertia.
domain_priors:theater_ratio(lcdm_small_scale_anomalies, 0.15).       % Piton detection (< 0.70). The paradigm is still highly functional.

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(lcdm_small_scale_anomalies, extractiveness, 0.42).
narrative_ontology:constraint_metric(lcdm_small_scale_anomalies, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(lcdm_small_scale_anomalies, theater_ratio, 0.15).

% --- NL Profile Metrics ---
% Not a mountain; these are not applicable.

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(lcdm_small_scale_anomalies, tangled_rope).
narrative_ontology:human_readable(lcdm_small_scale_anomalies, "Lambda-CDM Cosmological Model (Small-Scale Structure)").

% --- Binary flags ---
domain_priors:requires_active_enforcement(lcdm_small_scale_anomalies). % Via peer review, funding panels, conference programming.

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% These feed the directionality derivation chain.
narrative_ontology:constraint_beneficiary(lcdm_small_scale_anomalies, mainstream_cosmologists).
narrative_ontology:constraint_victim(lcdm_small_scale_anomalies, alternative_model_theorists).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × f(d) × σ(S)
   where f(d) is the sigmoid directionality function.
   The engine derives d from beneficiary/victim membership + exit_options.
   Scope for cosmology is universal, σ(S) = 1.0.
   CONTEXT ARITY: All context() terms must have exactly 4 arguments.
   ========================================================================== */

% PERSPECTIVE 1: THE ALTERNATIVE THEORIST (TARGET)
% As a victim with constrained exit options, the model appears as a Tangled
% Rope. It has a genuine coordination function they are excluded from, and a
% high coercive cost (suppression of their research).
% Engine derives d from: victim + constrained exit -> high d -> high f(d) -> high χ.
constraint_indexing:constraint_classification(lcdm_small_scale_anomalies, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(universal))).

% PERSPECTIVE 2: THE MAINSTREAM COSMOLOGIST (BENEFICIARY)
% As a primary beneficiary with high agency, the model is a pure coordination
% device (Rope), enabling collaborative progress. The extractive component is
% invisible or seen as necessary quality control.
% Engine derives d from: beneficiary + arbitrage exit -> d ≈ 0.05 -> f(d) ≈ -0.12 -> negative χ.
constraint_indexing:constraint_classification(lcdm_small_scale_anomalies, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(universal))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER
% The historian of science sees both the powerful coordination function and the
% asymmetric extraction. The high ε and high suppression, combined with active
% enforcement and beneficiary/victim groups, meet the criteria for a Tangled Rope.
% Engine derives d ≈ 0.72 -> f(d) ≈ 1.15 for analytical perspective.
constraint_indexing:constraint_classification(lcdm_small_scale_anomalies, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 4: A GRADUATE STUDENT (POWERLESS/TRAPPED)
% For a student whose early research contradicts the paradigm, the constraint
% can feel like a Snare, threatening their career prospects. While structurally
% a Tangled Rope, the localized, high-stakes perception shifts.
% Engine derives d from: victim + trapped exit -> d ≈ 0.95 -> f(d) ≈ 1.42 -> high χ.
% χ = 0.42 * 1.42 * 1.0 = 0.596. This is still below the Snare threshold of 0.66,
% classifying as a severe Tangled Rope.
constraint_indexing:constraint_classification(lcdm_small_scale_anomalies, tangled_rope,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).


/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(lcdm_small_scale_anomalies_tests).

test(perspectival_gap_beneficiary_vs_observer) :-
    constraint_indexing:constraint_classification(lcdm_small_scale_anomalies, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    constraint_indexing:constraint_classification(lcdm_small_scale_anomalies, TypeObserver, context(agent_power(analytical), _, _, _)),
    assertion(TypeBeneficiary == rope),
    assertion(TypeObserver == tangled_rope),
    TypeBeneficiary \= TypeObserver.

test(tangled_rope_structural_gates_pass) :-
    narrative_ontology:constraint_beneficiary(lcdm_small_scale_anomalies, _),
    narrative_ontology:constraint_victim(lcdm_small_scale_anomalies, _),
    domain_priors:requires_active_enforcement(lcdm_small_scale_anomalies).

test(tangled_rope_metric_thresholds_pass) :-
    domain_priors:base_extractiveness(lcdm_small_scale_anomalies, E),
    domain_priors:suppression_score(lcdm_small_scale_anomalies, S),
    assertion(E >= 0.30),
    assertion(S >= 0.40).

:- end_tests(lcdm_small_scale_anomalies_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   - Base Extractiveness (ε=0.42): This value is chosen to reflect that while
 *     ΛCDM is hugely successful, the accumulated weight of anomalies on small
 *     scales (Hubble tension, satellite problems, core-cusp problem, and now
 *     early JWST galaxies) represents a significant portion of unexplained
 *     observational reality. It's not a minor issue, hence ε > 0.4.
 *   - Suppression (S=0.65): Scientific paradigms are inherently conservative.
 *     The institutional mechanisms of peer review, grant allocation, and faculty
 *     hiring create a strong filtering effect against non-standard models. This
 *     score reflects a high but not absolute barrier to alternative ideas.
 *
 * PERSPECTIVAL GAP:
 *   The gap is profound. For a mainstream researcher (beneficiary), ΛCDM is a
 *   massively productive Rope, providing the tools and shared assumptions for
 *   progress. Its failures are seen as "puzzles to be solved." For an alternative
 *   theorist (victim), the same structure is a Tangled Rope; its "puzzles" are
 *   their evidence, but the model's institutional weight actively suppresses
 *   their ability to compete, creating extraction in the form of lost careers,
 *   funding, and recognition. The analytical observer sees both functions
 *   simultaneously, hence the Tangled Rope classification.
 *
 * DIRECTIONALITY LOGIC:
 *   - Beneficiary: `mainstream_cosmologists` directly benefit from the coordination
 *     function. Their `arbitrage` exit option reflects their ability to pivot
 *     within the paradigm to fruitful research areas. This derives a low `d`.
 *   - Victim: `alternative_model_theorists` bear the costs of suppression.
 *     Their `constrained` exit reflects the high career cost of leaving the
 *     mainstream; they can't easily switch paradigms. This derives a high `d`.
 *     The declarations map directly to the power dynamics of Kuhnian paradigm science.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification correctly avoids two errors. It doesn't label ΛCDM a
 *   Snare, which would ignore its immense, genuine coordination function.
 *   Nor does it label it a pure Rope, which would ignore the very real
 *   coercive and extractive effects on minority scientific views. The Tangled
 *   Rope classification captures the essential duality of a powerful but
 *   contested scientific paradigm.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    omega_lcdm_paradigm,
    'Are the small-scale anomalies minor puzzles for ΛCDM, or are they falsifications pointing toward a paradigm shift?',
    'Sustained, high-significance data from next-generation telescopes (JWST, Roman, Euclid) that either resolves the anomalies within ΛCDM or confirms their irreconcilability.',
    'If resolved, this constraint story degrades to a Rope (ε falls). If irreconcilable, it signals the paradigm is failing and may become a Piton over time as a new model emerges.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(lcdm_small_scale_anomalies, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Required as base_extractiveness (0.42) > 0.4.
% Models the period from the solidification of ΛCDM (late 90s, T=0)
% to the era of precision cosmology and JWST findings (T=10).

% Theater ratio over time (slight increase as defense becomes more performative):
narrative_ontology:measurement(lcdm_tr_t0, lcdm_small_scale_anomalies, theater_ratio, 0, 0.05).
narrative_ontology:measurement(lcdm_tr_t5, lcdm_small_scale_anomalies, theater_ratio, 5, 0.10).
narrative_ontology:measurement(lcdm_tr_t10, lcdm_small_scale_anomalies, theater_ratio, 10, 0.15).

% Extraction over time (anomalies accumulate, increasing ε):
narrative_ontology:measurement(lcdm_ex_t0, lcdm_small_scale_anomalies, base_extractiveness, 0, 0.20).
narrative_ontology:measurement(lcdm_ex_t5, lcdm_small_scale_anomalies, base_extractiveness, 5, 0.30).
narrative_ontology:measurement(lcdm_ex_t10, lcdm_small_scale_anomalies, base_extractiveness, 10, 0.42).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type: ΛCDM provides the common language and assumptions for the field.
narrative_ontology:coordination_type(lcdm_small_scale_anomalies, information_standard).

% --- Network Decomposition (Constraint Families) ---
% The colloquial label "ΛCDM Model" is decomposed into two constraints
% because its explanatory power (and thus its base extractiveness ε) is
% radically different depending on the observable.

% DUAL FORMULATION NOTE:
% This constraint is one of 2 stories decomposed from "the ΛCDM model".
% Decomposed because ε differs across observables (ε-invariance principle).
% The success of the large-scale model provides the institutional weight that
% makes this small-scale version a Tangled Rope rather than just a weak theory.
% Related stories:
%   - lcdm_large_scale_structure (ε=0.05, Mountain)

narrative_ontology:affects_constraint(lcdm_large_scale_structure, lcdm_small_scale_anomalies).


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% No overrides are necessary for this constraint. The standard derivation
% from beneficiary/victim declarations and exit options accurately captures
% the power dynamics of the scientific community.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */