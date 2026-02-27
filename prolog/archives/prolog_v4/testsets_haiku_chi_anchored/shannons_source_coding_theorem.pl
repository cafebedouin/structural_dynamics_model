% ============================================================================
% CONSTRAINT STORY: shannons_source_coding_theorem
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_shannons_source_coding_theorem, []).

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
 *   constraint_id: shannons_source_coding_theorem
 *   human_readable: Shannon's Source Coding Theorem (Achievable Compression Limit)
 *   domain: technological/information_theory
 *
 * SUMMARY:
 *   Shannon's Source Coding Theorem (1948) is a mathematical statement of
 *   natural law: the entropy of an information source H(X) defines the
 *   minimum average number of bits required to losslessly encode symbols from
 *   that source. No compression algorithm, no matter how sophisticated, can
 *   achieve a lower average rate than H(X) bits per symbol for arbitrarily
 *   long sequences drawn from a fixed distribution. The theorem is among the
 *   most robust constraints in mathematics and physics — it has withstood 75+
 *   years of research, empirical testing across all physical domains
 *   (electrical, optical, acoustic, biological), and attempted extensions
 *   without modification. It is not a policy, not an institutional
 *   arrangement, not a contingent fact about current technology — it is a
 *   structural limit that emerges from the definition of information itself.
 *   This makes it a canonical mountain constraint: unchangeable, universally
 *   applicable, with zero degrees of freedom for all observer positions.
 *
 * KEY AGENTS:
 *   - Mathematical Formalism: Pure constraint carrier (analytical/analytical) — the theorem as stated in information theory textbooks
 *   - Hardware Engineers: Implementers (powerful/mobile) — design systems subject to the compression limit; cannot escape the bound through engineering
 *   - Data Streams: Passive bearers (powerless/trapped) — any finite sequence has a fixed, minimum compressible size equal to its entropy
 *   - Information Theory Community: Validators (organized/constrained) — 75 years of research confirming the theorem's universality and its resistance to exceptions
 *   - Quantum Information Theorists: Extension explorers (powerful/analytical) — investigating whether quantum substrates extend or modify the classical bound
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(shannons_source_coding_theorem, 0.08).
domain_priors:suppression_score(shannons_source_coding_theorem, 0.02).
domain_priors:theater_ratio(shannons_source_coding_theorem, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(shannons_source_coding_theorem, extractiveness, 0.08).
narrative_ontology:constraint_metric(shannons_source_coding_theorem, suppression_requirement, 0.02).
narrative_ontology:constraint_metric(shannons_source_coding_theorem, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(shannons_source_coding_theorem, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(shannons_source_coding_theorem, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(shannons_source_coding_theorem, mountain).
narrative_ontology:human_readable(shannons_source_coding_theorem, "Shannon's Source Coding Theorem (Achievable Compression Limit)").
narrative_ontology:topic_domain(shannons_source_coding_theorem, "technological/information_theory").

domain_priors:emerges_naturally(shannons_source_coding_theorem).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: MATHEMATICAL FORMALISM (MOUNTAIN) — Shannon's theorem is a mathematical truth about the relationship between entropy and compression. No escape; no alternatives. The compression limit H(X) bits per symbol is invariant across all compression schemes and all physical instantiations. d≈0.72, f(d)≈1.15, σ=1.0 → χ≈0.09. The theorem holds regardless of whether observed or implemented.
constraint_indexing:constraint_classification(shannons_source_coding_theorem, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 2: HARDWARE ENGINEER (MOUNTAIN) — For any physical storage or transmission system, the theorem imposes a hard constraint on compression efficiency. No codec design, no algorithm, no hardware acceleration can beat the entropy bound. d≈0.48, f(d)≈0.60, σ=1.2 → χ≈0.06. Mobile exit options (design other systems, use other technologies) do not change the mathematical reality that THIS system has a fixed compression ceiling.
constraint_indexing:constraint_classification(shannons_source_coding_theorem, mountain,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 3: THE DATA STREAM (MOUNTAIN) — From the perspective of any finite sequence of symbols, the Shannon limit is inexorable. The entropy of the source determines how much it can be compressed. No amount of organizational effort or financial investment can compress a high-entropy source below its Shannon bound. d≈1.0, f(d)≈1.42, σ=0.8 → χ≈0.09. The constraint is equally binding for all data streams.
constraint_indexing:constraint_classification(shannons_source_coding_theorem, mountain,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 4: INFORMATION THEORY COMMUNITY (MOUNTAIN) — Decades of research have confirmed the theorem's universality. No alternative theoretical framework has been discovered that permits lossless compression below the entropy bound. The community's collective search for exceptions has defined the limits precisely. d≈0.40, f(d)≈0.40, σ=1.2 → χ≈0.03. The theorem has become more ironclad, not less, as knowledge has accumulated.
constraint_indexing:constraint_classification(shannons_source_coding_theorem, mountain,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(shannons_source_coding_theorem_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(shannons_source_coding_theorem, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(shannons_source_coding_theorem, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(shannons_source_coding_theorem, ExtMetricName, E),
    domain_priors:suppression_score(shannons_source_coding_theorem, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(shannons_source_coding_theorem),
    narrative_ontology:constraint_metric(shannons_source_coding_theorem, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(shannons_source_coding_theorem, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(shannons_source_coding_theorem_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness (0.08): Extremely low. The constraint does not extract in any meaningful sense — it sets a lower bound that all compression schemes approach asymptotically. No agent is enriched at another's expense. The entropy bound is a floor, not a mechanism for redistribution. Theater ratio (0.15): Minimal. The theorem requires virtually no performative activity to enforce itself. No auditing, no verification rituals, no institutional theater. The bound is self-enforcing through mathematical logic. Suppression (0.02): Negligible. While the theorem establishes an absolute limit, it suppresses nothing — all feasible compression rates remain available. Agents are free to compress at any rate above H(X); the theorem only says they cannot go below it. Accessibility collapse (0.92): Very high. The constraint is maximally irreducible — it cannot be bypassed by any alternative methodology, technology, or interpretation. Mathematical substitution is impossible. Resistance (0.05): Very low. No force is required to maintain the constraint; it is self-enforcing through logical necessity. The theorem requires no institutional support, no enforcement apparatus, no coercive mechanism.
 *
 * PERSPECTIVAL GAP:
 *   All four perspectives classify as Mountain, with only minor variations in derived χ values due to scope modifiers. The perspectival gap is minimal because the constraint is genuinely universal across observer positions. A hardware engineer, a mathematician, a data stream, and the research community all encounter the same irreducible limit. The theorem's universality across perspectives is itself the diagnostic marker that distinguishes a true mountain from a contingent institutional constraint. Unlike the verification bottleneck (which appears as Rope to the beneficiary and Snare to the victim), Shannon's theorem appears as Mountain to all agents. This invariance is not a weakness of the framework but confirmation of the framework's diagnostic capability.
 *
 * DIRECTIONALITY LOGIC:
 *   Directional values are uniform across all perspectives because there is no extraction mechanism. All d values derive from analytical observation of a mathematical fact, not from asymmetric structural positioning. The theorem does not create winners and losers — it creates a uniform constraint that applies to all compression schemes. The variations in d (0.40 to 1.0) reflect only the observer's structural position (institutional to powerless) but do not affect the classification outcome (all mountain). This is a diagnostic feature: when directionality varies but classification does not, the constraint is a true natural law.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    quantum_information_extension,
    'Does quantum information theory permit compression below the classical Shannon limit for quantum data?',
    'Analysis of Schumacher compression; comparison of quantum and classical entropy bounds for mixed states; experimental realization of quantum compression protocols',
    'If quantum < classical: Shannon''s theorem is not universal across information substrates (reveals domain-specificity). If quantum ≥ classical: universality is confirmed (mountain stability).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(quantum_information_extension, empirical, 'Whether quantum compression can exceed classical Shannon bound').

omega_variable(
    context_dependent_compression,
    'Can semantic or task-specific compression schemes achieve lower average rates than Shannon''s entropy bound by exploiting task structure rather than statistical structure?',
    'Formal analysis of task-specific compression (e.g., ''compress only enough to perform action X''); comparison of task-aware vs distribution-aware lower bounds; empirical testing against synthetic and real data',
    'If true: Shannon bound is substrate-specific (applies to distribution-only view). If false: bound is truly universal (mountain is universal).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(context_dependent_compression, conceptual, 'Whether task-dependent compression can beat Shannon bound').

omega_variable(
    nonergodic_source_behavior,
    'For nonergodic information sources (whose statistics are time-dependent or state-dependent in ways not captured by entropy), does the Shannon bound still hold or does it require extension?',
    'Theoretical analysis of nonergodic source coding; empirical testing on sources with drift, regime switching, or hidden state; extension theorems for variable-entropy sources',
    'If bound holds: mountain is robust to source assumptions. If bound requires extension: mountain splits into specialized versions (different constraints for ergodic vs nonergodic).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(nonergodic_source_behavior, empirical, 'Whether Shannon bound extends to nonergodic sources').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(shannons_source_coding_theorem, 0, 70).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(shannon_tr_t0, shannons_source_coding_theorem, theater_ratio, 0, 0.12).
narrative_ontology:measurement(shannon_tr_t35, shannons_source_coding_theorem, theater_ratio, 35, 0.14).
narrative_ontology:measurement(shannon_tr_t70, shannons_source_coding_theorem, theater_ratio, 70, 0.18).

% Extraction over time
narrative_ontology:measurement(shannon_be_t0, shannons_source_coding_theorem, base_extractiveness, 0, 0.06).
narrative_ontology:measurement(shannon_be_t35, shannons_source_coding_theorem, base_extractiveness, 35, 0.07).
narrative_ontology:measurement(shannon_be_t70, shannons_source_coding_theorem, base_extractiveness, 70, 0.08).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(shannons_source_coding_theorem, information_standard).
narrative_ontology:affects_constraint(shannons_source_coding_theorem, huffman_coding_optimality).
narrative_ontology:affects_constraint(shannons_source_coding_theorem, arithmetic_coding_efficiency).
narrative_ontology:affects_constraint(shannons_source_coding_theorem, lz_compression_asymptotic_rate).

% DUAL FORMULATION NOTE:
% Shannon's Source Coding Theorem is the foundational constraint for all lossless compression algorithms. Huffman coding, arithmetic coding, and LZ-family algorithms are all implementations that asymptotically approach but never exceed the Shannon bound. These are separate constraints capturing specific algorithmic achievements; Shannon's theorem is the universal natural law that governs all of them.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
