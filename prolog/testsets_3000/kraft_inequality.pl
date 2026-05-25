% ============================================================================
% CONSTRAINT STORY: kraft_inequality
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_kraft_inequality, []).

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
 *   constraint_id: kraft_inequality
 *   human_readable: Kraft Inequality: Fundamental Bound on Prefix-Free Code Length
 *   domain: information_theory/mathematical_logic
 *
 * SUMMARY:
 *   The Kraft inequality is a foundational theorem in information theory
 *   stating that for any prefix-free code (a code in which no codeword is a
 *   prefix of another) on a D-ary alphabet, the sum of D raised to the
 *   negative power of each codeword length must not exceed 1: Σ D^(-l_i) ≤ 1.
 *   This bound is a mathematical necessity — not a physical law that could be
 *   violated by cleverness or resources, but a logical consequence of the
 *   pigeonhole principle applied to prefix-free structure. There is no
 *   beneficiary and no victim. The constraint has zero degrees of freedom for
 *   all agents across all observables. It represents genuine natural law in
 *   the Deferential Realism sense: accessibility collapse (any attempt to
 *   build a shorter prefix-free code fails universally) is maximal;
 *   resistance to the constraint (finding a workaround, a loophole, an
 *   alternative principle) is near-zero; emergence is fully natural from the
 *   definition of prefix-free codes and finitary arithmetic.
 *
 * KEY AGENTS:
 *   - Code Designers: All agents attempting to construct optimal codes. Structurally trapped by mathematical necessity, not institutional design. No asymmetry of power or information.
 *   - Information Theorists: The analytical community that proved and extended the bound. They are observers and clarifiers, not beneficiaries or extractors.
 *   - Potential Alternatives (Nonexistent): Any coding scheme that claims to beat the Kraft bound is not a different perspective on the same constraint — it is a different constraint altogether (non-prefix-free codes, quantum codes with redefined metrics, etc.) and should be analyzed separately.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(kraft_inequality, 0.12).
domain_priors:suppression_score(kraft_inequality, 0.02).
domain_priors:theater_ratio(kraft_inequality, 0.08).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(kraft_inequality, extractiveness, 0.12).
narrative_ontology:constraint_metric(kraft_inequality, suppression_requirement, 0.02).
narrative_ontology:constraint_metric(kraft_inequality, theater_ratio, 0.08).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(kraft_inequality, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(kraft_inequality, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(kraft_inequality, mountain).
narrative_ontology:human_readable(kraft_inequality, "Kraft Inequality: Fundamental Bound on Prefix-Free Code Length").
narrative_ontology:topic_domain(kraft_inequality, "information_theory/mathematical_logic").

domain_priors:emerges_naturally(kraft_inequality).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CODE DESIGNER FACING CONSTRAINT — Any attempt to construct a prefix-free binary code with shorter average length than the Kraft bound permits faces immediate logical impossibility. The designer has no degrees of freedom — the mathematics is immutable regardless of ingenuity or resources.
constraint_indexing:constraint_classification(kraft_inequality, mountain,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(trapped),
            spatial_scope(universal))).

% PERSPECTIVE 2: INSTITUTIONAL OPTIMIZER — Even institutions with maximal power (computational capacity, research resources, funding) cannot circumvent the Kraft bound by moving to alternative coordinate systems or changing the experimental setup. The constraint is coordinate-invariant and substrate-independent.
constraint_indexing:constraint_classification(kraft_inequality, mountain,
    context(agent_power(powerful),
            time_horizon(civilizational),
            exit_options(mobile),
            spatial_scope(universal))).

% PERSPECTIVE 3: ANALYTICAL OBSERVER / NATURAL LAW — From the civilizational perspective at universal scope, the Kraft inequality is a mathematical necessity that follows from the definition of prefix-free codes and the pigeonhole principle. It is not a regularity of nature that could change, but a logical structure of information itself. Emerges naturally from first principles; no beneficiary, no victim, no extraction mechanism.
constraint_indexing:constraint_classification(kraft_inequality, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(kraft_inequality_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(kraft_inequality, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(kraft_inequality, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(kraft_inequality, ExtMetricName, E),
    domain_priors:suppression_score(kraft_inequality, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(kraft_inequality),
    narrative_ontology:constraint_metric(kraft_inequality, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(kraft_inequality, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(kraft_inequality_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.12): Minimal. The 0.12 value reflects that even true mountains have infinitesimal residual extractiveness from the measurement and pedagogical apparatus — the cost of stating the theorem, proving it, teaching it. The constraint itself extracts nothing; the social structures around it may extract labor and attention. Suppression (0.02): Minimal. There is no suppression mechanism — the bound cannot be enforced because it is simply true. Alternatives do not exist to suppress; the space of possible codes is exhaustively constrained by logic, not by force. Theater ratio (0.08): Minimal. Kraft inequality proofs are non-theatrical — they state definitions, apply pigeonhole logic, and deliver the conclusion. No performative element; the ritual of proof mirrors the logical necessity. The measurements show perfect flatness across time — the constraint has not changed and cannot change. A mathematical truth is invariant under temporal translation.
 *
 * PERSPECTIVAL GAP:
 *   This is a uniform-type constraint (mountain-only). All perspectives produce mountain classification because the constraint is genuinely invariant across all observables and measurement methodologies. A code designer facing the Kraft bound and an institutional optimizer with maximal resources both face the same immutable bound. The analytical observer's perspective at civilizational scope simply formalizes what all agents discover empirically — the bound is a law of information structure, not an institutional arrangement.
 *
 * DIRECTIONALITY LOGIC:
 *   The Kraft inequality has no directionality because there is no flow of extraction, no beneficiary, no victim. Directionality d is derived from beneficiary/victim declarations and exit options; Kraft inequality has neither. All agents (powerless, powerful, institutional, analytical) experience the same constraint equally. This is the defining feature of a mountain: the constraint's binding mechanism is logical/mathematical, not structural/social. The d-value is undefined and meaningless for this constraint — the chi formula collapses because there is no extraction to scale.
 *
 * MANDATROPHY ANALYSIS:
 *   No mandatrophy to resolve. The constraint classifies identically (mountain) from all perspectives and all observables. There is no tension between coordination and extraction readings — neither applies. The Kraft inequality is neither a coordination mechanism (it does not solve a collective action problem) nor an extraction mechanism (it does not transfer value from one agent to another). It is a boundary of what is logically possible. This constraint serves as a diagnostic exemplar of true natural law in the DR system — the absence of perspectival gap is the signature of mathematical necessity.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    nonbinary_alphabet_generalization,
    'Does the Kraft bound apply equally to non-binary alphabets, or does it represent a specifically binary structural constraint?',
    'Mathematical generalization: Kraft-McMillan inequality extends the bound to D-ary alphabets as sum(D^(-l_i)) ≤ 1. Verification that the bound holds universally across all finite alphabets.',
    'If bound is universal: constraint is truly natural law (mountain) independent of encoding choice. If bound is binary-specific: there exists alternative encoding regime that escapes the constraint (downgrade to scaffold or rope).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(nonbinary_alphabet_generalization, empirical, 'Whether Kraft bound generalizes to all finite alphabets or is binary-specific').

omega_variable(
    quantum_coding_exception,
    'Do quantum coding schemes (quantum information theory) constitute a genuine exception to the Kraft inequality, or do they operate in a different measurement space that preserves the bound under reinterpretation?',
    'Formal proof that quantum codes satisfy their own Kraft-type bound with respect to density matrices and quantum entropy. Analysis of whether apparent violations are due to measurement space changes rather than true logical escape.',
    'If true exception: Kraft inequality is not universal — quantum systems inhabit a different constraint landscape (downgrade to rope or scaffold). If reinterpretation preserves the bound: Kraft is preserved even in quantum domain, confirming mountain status.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(quantum_coding_exception, conceptual, 'Whether quantum information theory escapes or reinterprets the Kraft bound').

omega_variable(
    unbounded_alphabet_limit,
    'In the limit of unbounded or infinite alphabets, does the Kraft inequality become vacuous, or does it imply new structural constraints on infinite information systems?',
    'Mathematical analysis of Kraft bound in infinite-alphabet regime. Examine whether constraints on infinite codes emerge as limit behavior or whether the bound degenerates.',
    'If vacuous: Kraft bound is a finite-alphabet phenomenon, suggesting contingency rather than universality (downgrade). If non-vacuous: universality confirmed across scale transitions.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(unbounded_alphabet_limit, conceptual, 'Whether Kraft inequality preserves meaning in infinite-alphabet limit').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(kraft_inequality, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(kraft_tr_t0, kraft_inequality, theater_ratio, 0, 0.08).
narrative_ontology:measurement(kraft_tr_t50, kraft_inequality, theater_ratio, 50, 0.08).
narrative_ontology:measurement(kraft_tr_t100, kraft_inequality, theater_ratio, 100, 0.08).

% Extraction over time
narrative_ontology:measurement(kraft_be_t0, kraft_inequality, base_extractiveness, 0, 0.12).
narrative_ontology:measurement(kraft_be_t50, kraft_inequality, base_extractiveness, 50, 0.12).
narrative_ontology:measurement(kraft_be_t100, kraft_inequality, base_extractiveness, 100, 0.12).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(kraft_inequality, information_standard).
narrative_ontology:affects_constraint(kraft_inequality, huffman_coding_optimality).
narrative_ontology:affects_constraint(kraft_inequality, source_coding_theorem).
narrative_ontology:affects_constraint(kraft_inequality, channel_coding_reliability).

% DUAL FORMULATION NOTE:
% Kraft inequality is upstream of all practical coding theorems. Huffman coding achieves the bound as a constructive proof that the bound is tight. Source coding theorem (Shannon's theorem) guarantees codes exist at the entropy rate approaching the Kraft bound asymptotically. These downstream constraints have different epsilon values reflecting degrees of freedom in construction; the Kraft inequality itself permits no degrees of freedom.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
