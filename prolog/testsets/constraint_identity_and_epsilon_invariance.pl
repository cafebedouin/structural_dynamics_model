% ============================================================================
% CONSTRAINT STORY: constraint_identity_and_epsilon_invariance
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_constraint_identity_and_epsilon_invariance, []).

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
 *   constraint_id: constraint_identity_and_epsilon_invariance
 *   human_readable: Constraint Identity and Epsilon Invariance: The Mathematical Binding of Constraint Definition to Measurable Extraction
 *   domain: foundational_theory/formal_systems
 *
 * SUMMARY:
 *   Constraint Identity and Epsilon Invariance is a foundational principle of
 *   the Deferential Realism indexical classification system. It states that ε
 *   (base extractiveness) is an intrinsic property of a constraint, not
 *   dependent on the choice of observable or measurement method used to
 *   evaluate the constraint. When an analyst observes that measuring a claim
 *   via one observable yields ε ≈ 0.08 but measuring via a different
 *   observable yields ε ≈ 0.42, the principle mandates that the analyst is
 *   observing two structurally distinct constraints, not one constraint
 *   measured imprecisely. This principle is not a convention — it follows
 *   mathematically from the χ formula. If ε changes when the observable
 *   changes, then ε is not intrinsic to the constraint. Because the
 *   constraint's classification depends on both ε and the indexical tuple
 *   (P,T,E,S), allowing ε to vary by observable choice would make
 *   classification observer-relative and context-dependent in the worst
 *   sense: the same claim could be simultaneously mountain and snare
 *   depending on how you measure it, collapsing the system's coherence. The
 *   principle forces clarity: when a natural-language concept (like 'the BGS
 *   conjecture' or 'market efficiency') covers multiple structurally distinct
 *   claims with different ε values, the analyst must write separate
 *   constraint stories for each claim and link them with network
 *   relationships. This decomposition is not a limitation — it is a strength
 *   that disambiguates colloquial labels into structurally precise claims.
 *
 * KEY AGENTS:
 *   - The Analyst Using the Framework: Constraint-relative (powerless/constrained) — cannot measure the same constraint in two different ways and obtain different ε values without violating the principle; must decompose if observables yield inconsistent ε
 *   - The Framework Itself: Institutional (institutional/arbitrage) — preserves coherence by enforcing ε-invariance; any attempt to relax this principle undermines the entire classification system
 *   - The Natural-Language Concept: Abstract entity (powerless/trapped) — has no agency; its identity is determined by the decomposition, not by authorial intent
 *   - The Causal Mechanism: Physical/logical reality (analytical/analytical) — determines whether two observables track the same extraction mechanism or different ones; grounds the distinction between measurement imprecision and constraint decomposition
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(constraint_identity_and_epsilon_invariance, 0.08).
domain_priors:suppression_score(constraint_identity_and_epsilon_invariance, 0.02).
domain_priors:theater_ratio(constraint_identity_and_epsilon_invariance, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(constraint_identity_and_epsilon_invariance, extractiveness, 0.08).
narrative_ontology:constraint_metric(constraint_identity_and_epsilon_invariance, suppression_requirement, 0.02).
narrative_ontology:constraint_metric(constraint_identity_and_epsilon_invariance, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(constraint_identity_and_epsilon_invariance, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(constraint_identity_and_epsilon_invariance, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(constraint_identity_and_epsilon_invariance, mountain).
narrative_ontology:human_readable(constraint_identity_and_epsilon_invariance, "Constraint Identity and Epsilon Invariance: The Mathematical Binding of Constraint Definition to Measurable Extraction").
narrative_ontology:topic_domain(constraint_identity_and_epsilon_invariance, "foundational_theory/formal_systems").

domain_priors:emerges_naturally(constraint_identity_and_epsilon_invariance).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ANALYST VIEWING THROUGH OBSERVABLE LENS (MOUNTAIN) — Even from the position of an agent trying to escape the constraint via alternative measurements, the underlying structural principle is immutable. Changing the observable and obtaining different ε values is not an escape — it is evidence that you have defined a different constraint. The binding is absolute: ε is intrinsic to the constraint, not to the observer. This agent cannot escape by choosing a measurement.
constraint_indexing:constraint_classification(constraint_identity_and_epsilon_invariance, mountain,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(trapped),
            spatial_scope(universal))).

% PERSPECTIVE 2: ANALYTICAL OBSERVER AT CIVILIZATIONAL SCOPE (MOUNTAIN) — From the global analytical position, the ε-invariance principle is a mathematical necessity, not a empirical claim. If ε and (P,T,E,S) are fixed, χ is determined via the formula χ = ε × f(d) × σ(S), and classification outcome is determined by the χ gates and type-specific thresholds. Observable selection cannot alter this without changing ε, which means changing the constraint. The constraint identity is preserved across all reference frames because the mathematics is invariant.
constraint_indexing:constraint_classification(constraint_identity_and_epsilon_invariance, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 3: INSTITUTIONAL FRAMEWORK (MOUNTAIN) — The institutions that maintain the Deferential Realism framework (research bodies, theory development communities) are bound by the ε-invariance principle because it is constitutive of the system's coherence. Any attempt to allow ε to vary with observable selection undermines the entire classification structure — it introduces a free parameter that can be gamed to justify desired classifications. The constraint that ε must be intrinsic is the foundation preserving institutional integrity.
constraint_indexing:constraint_classification(constraint_identity_and_epsilon_invariance, mountain,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(universal))).

% PERSPECTIVE 4: RESEARCH PROGRAM GENERATIONAL VIEW (MOUNTAIN) — Across a research generation, the principle that constraints decompose when observables yield different ε values has become a defining feature of rigorous constraint analysis. The BGS exemplar demonstrated that this decomposition is not a limitation but a strength — it forces clarity about what is actually being claimed. This principle cannot be abandoned without collapsing the entire analytical program.
constraint_indexing:constraint_classification(constraint_identity_and_epsilon_invariance, mountain,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: INDIVIDUAL THEORIST AT BIOGRAPHICAL SCOPE (MOUNTAIN) — An individual analyst working within the DR framework faces immovable constraints: they cannot assign two different ε values to the same constraint without violating the logical structure of the system itself. The constraint is not imposed externally but is intrinsic to the tool they are using. They are constrained by the requirement for coherence, not by external enforcement.
constraint_indexing:constraint_classification(constraint_identity_and_epsilon_invariance, mountain,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(constraint_identity_and_epsilon_invariance_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(constraint_identity_and_epsilon_invariance, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(constraint_identity_and_epsilon_invariance, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(constraint_identity_and_epsilon_invariance, ExtMetricName, E),
    domain_priors:suppression_score(constraint_identity_and_epsilon_invariance, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(constraint_identity_and_epsilon_invariance),
    narrative_ontology:constraint_metric(constraint_identity_and_epsilon_invariance, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(constraint_identity_and_epsilon_invariance, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(constraint_identity_and_epsilon_invariance_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.08): Minimal. The constraint is a mathematical necessity, not an empirical extraction. It does not extract resources from any agent except clarity from analysts who wish to evade the principle. Suppression (0.02): Minimal. The constraint does not suppress alternatives — it requires that when alternatives (different observables) reveal different ε values, the analyst acknowledge they are describing different constraints. Theater ratio (0.15): Very low. The principle makes no performative claims; it is purely structural. There is no ritual or maintenance cost — the principle is simply true or false. Mountain classification: Justified by accessibility_collapse (0.92) and resistance (0.08). The principle is accessible to rigorous analysis and faces minimal challenge from empirical anomalies because it is logically grounded in the χ formula itself, not in empirical claims about how the world works. The principle cannot be violated without making the framework incoherent.
 *
 * PERSPECTIVAL GAP:
 *   There is no perspectival gap. All five perspectives classify the constraint identically as a mountain. This uniform classification is itself diagnostic: it indicates a true natural law, not a contingent institutional arrangement. The principle produces the same classification from the powerless analyst's perspective (trapped, no alternative measurements are available that would satisfy the framework's internal consistency requirements) and from the analytical observer's civilizational view (the principle is logically necessary). This uniformity is the hallmark of a genuine constraint.
 *
 * DIRECTIONALITY LOGIC:
 *   This constraint operates inversely to typical extraction patterns. Rather than flowing from powerful to powerless, the constraint flows from the framework to all users. Beneficiaries (the framework, the analytical program) gain coherence. Victims (analysts who wish to use context-dependent ε values to justify predetermined classifications) lose that freedom. The directionality is not extraction-based but coherence-based — the constraint protects the integrity of the classification system itself, not the interests of any agent. The constraint is perfectly symmetric: it binds equally to all users regardless of their power level or position. This symmetry is why it appears as a mountain from all perspectives — there is no exit option, no escape, no position from which the constraint is experienced differently.
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLVES TRIVIALLY: This constraint has no mandatrophy because it permits only one consistent interpretation. Unlike the verification bottleneck or other empirical constraints where the same structural data can yield multiple plausible classifications depending on perspective, the ε-invariance principle admits no ambiguity. If ε changes with observable, you have defined a different constraint. The framework enforces this disambiguation automatically through the mechanism that any attempt to violate ε-invariance produces incoherent classification outcomes (the same claim classified as both mountain and snare from the same perspective, with the same (P,T,E,S) tuple). The mandatrophy is not resolved through philosophical debate or empirical evidence — it is resolved by the internal logic of the system itself. Any analyst who attempts to maintain ε as context-dependent while using the framework will rapidly discover that their classifications become self-contradictory, forcing them to either (a) accept ε-invariance and decompose, or (b) abandon the framework. There is no stable middle ground.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    epsilon_measurement_ambiguity_vs_multiple_constraints,
    'When an analyst observes that measuring a claim via observable A yields ε ≈ 0.08 but measuring via observable B yields ε ≈ 0.42, are they measuring the same constraint with imperfect precision or revealing two distinct constraints?',
    'Structural analysis of the observables: do they track the same extraction mechanism, or are they detecting different extraction pathways? Causal dependency analysis: would eliminating one claim eliminate both observables, or would they persist independently?',
    'If same constraint: ε-invariance is violated and the framework requires revision. If distinct constraints: ε-invariance is preserved, and the analyst must write two stories. Current analysis assumes the latter.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(epsilon_measurement_ambiguity_vs_multiple_constraints, conceptual, 'Whether high ε variance indicates measurement imprecision or constraint decomposition').

omega_variable(
    natural_language_concept_entanglement,
    'How does the analyst determine whether a natural-language concept (e.g., ''market efficiency,'' ''quantum measurement,'' ''freedom of speech'') represents one constraint or multiple constraints that the colloquial label has conflated?',
    'Decomposition test: attempt to assign a single (P,T,E,S) tuple that produces consistent classification from all perspectives. If some perspectives require contradictory tuples for the same ''constraint,'' decompose. If decomposition yields logically independent claims with different ε values, the decomposition is justified.',
    'Failure to decompose produces false unified constraint stories with inconsistent internal logic. Over-decomposition produces fragmented corpus with missing network relationships. The ε-invariance principle is the guardrail that prevents both errors.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_language_concept_entanglement, empirical, 'Criteria for disambiguating natural language concepts into structurally distinct constraints').

omega_variable(
    circular_definition_risk,
    'Does the principle ''if ε changes, you have a different constraint'' become circular — defining constraint identity by ε invariance while using ε to determine constraint identity?',
    'Independent constraint identity criteria: does the constraint have an invariant causal core (the extraction mechanism, the coordination function, the natural law being expressed) that persists across all observables? If yes, ε-invariance follows from causal structure, not the reverse.',
    'If circular: the framework is coherent but lacks independent grounding — it is a formal system without empirical traction. If not circular: the framework is both formally consistent and empirically anchored to real causal structures.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(circular_definition_risk, conceptual, 'Whether ε-invariance grounds constraint identity or constraint identity grounds ε-invariance').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(constraint_identity_and_epsilon_invariance, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ciei_tr_t0, constraint_identity_and_epsilon_invariance, theater_ratio, 0, 0.15).
narrative_ontology:measurement(ciei_tr_t20, constraint_identity_and_epsilon_invariance, theater_ratio, 20, 0.15).
narrative_ontology:measurement(ciei_tr_t40, constraint_identity_and_epsilon_invariance, theater_ratio, 40, 0.15).

% Extraction over time
narrative_ontology:measurement(ciei_be_t0, constraint_identity_and_epsilon_invariance, base_extractiveness, 0, 0.08).
narrative_ontology:measurement(ciei_be_t20, constraint_identity_and_epsilon_invariance, base_extractiveness, 20, 0.08).
narrative_ontology:measurement(ciei_be_t40, constraint_identity_and_epsilon_invariance, base_extractiveness, 40, 0.08).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(constraint_identity_and_epsilon_invariance, information_standard).
narrative_ontology:affects_constraint(constraint_identity_and_epsilon_invariance, bgs_spectral_universality).
narrative_ontology:affects_constraint(constraint_identity_and_epsilon_invariance, bgs_eigenvector_thermalization).
narrative_ontology:affects_constraint(constraint_identity_and_epsilon_invariance, verification_bottleneck).

% DUAL FORMULATION NOTE:
% This constraint is upstream of all constraint family decompositions. It establishes the principle that distinguishes decomposition (multiple structurally distinct constraints with different ε values) from disambiguation (refined measurement of a single constraint). The BGS constraint family exemplifies this principle: spectral universality (ε=0.08, mountain) and eigenvector thermalization (ε=0.42, tangled_rope) are not two measurements of the same claim but two distinct claims that the natural-language label 'BGS conjecture' conflates.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
