% ============================================================================
% CONSTRAINT STORY: semantic_drift_velocity
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2025-01-02
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_semantic_drift_velocity, []).

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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    domain_priors:emerges_naturally/1,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: semantic_drift_velocity
 *   human_readable: Semantic Drift Velocity in Organizational Communication
 *   domain: sociolinguistics/organizational_communication/coordination_systems
 *
 * SUMMARY:
 *   Semantic drift velocity describes the structural gap between the rate at
 *   which operational vocabularies evolve through practice and the rate at
 *   which administrative vocabularies can stabilize meaning through
 *   formalization. In any organization where frontline work involves
 *   innovation, adaptation, or context-specific problem-solving,
 *   practitioners generate new terms, repurpose existing terms, and refine
 *   distinctions faster than institutional processes (standards committees,
 *   training programs, documentation systems) can canonize them. This creates
 *   a persistent semantic gap: operational reports use terms that
 *   administrative systems do not yet recognize, and administrative
 *   directives use terms that operational contexts have already moved beyond.
 *   The constraint exhibits mountain classification from all perspectives
 *   because the velocity differential is not a coordination failure or
 *   extractive mechanism — it is an information-theoretic property of how
 *   distributed practice generates knowledge. You cannot standardize a term
 *   until it has stabilized in use, but use continues evolving during
 *   standardization. The gap is irreducible.
 *
 * KEY AGENTS:
 *   - Frontline Operator: Experiences semantic drift as immediate operational reality (powerless/trapped) — new terms emerge from practice itself
 *   - Standards Committee: Institutional actor attempting formalization (institutional/arbitrage) — cannot eliminate lag despite resources and authority
 *   - Professional Association: Organized cross-organizational coordination (organized/mobile) — can reduce mistranslation frequency but not eliminate velocity differential
 *   - Middle Manager: Translation intermediary (moderate/constrained) — bears coordination cost of bridging semantic gap
 *   - Analytical Observer: Recognizes structural inevitability (analytical/analytical) — velocity differential is information-theoretic property of distributed coordination
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(semantic_drift_velocity, 0.12).
domain_priors:suppression_score(semantic_drift_velocity, 0.03).
domain_priors:theater_ratio(semantic_drift_velocity, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(semantic_drift_velocity, extractiveness, 0.12).
narrative_ontology:constraint_metric(semantic_drift_velocity, suppression_requirement, 0.03).
narrative_ontology:constraint_metric(semantic_drift_velocity, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(semantic_drift_velocity, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(semantic_drift_velocity, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(semantic_drift_velocity, mountain).
narrative_ontology:human_readable(semantic_drift_velocity, "Semantic Drift Velocity in Organizational Communication").
narrative_ontology:topic_domain(semantic_drift_velocity, "sociolinguistics/organizational_communication/coordination_systems").

domain_priors:emerges_naturally(semantic_drift_velocity).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: FRONTLINE OPERATOR (MOUNTAIN) — Experiences semantic drift as an unchangeable feature of operational reality. New terms emerge from practice faster than any institutional process can standardize them. The operator cannot slow the rate of lexical innovation — it emerges from the work itself.
constraint_indexing:constraint_classification(semantic_drift_velocity, mountain,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: STANDARDS COMMITTEE (MOUNTAIN) — Even with institutional resources and authority, cannot eliminate the semantic gap. Standardization efforts lag innovation by structural necessity: you cannot standardize a term until it has stabilized in practice, but practice continues evolving during standardization. The committee experiences this as an immutable coordination limit.
constraint_indexing:constraint_classification(semantic_drift_velocity, mountain,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: PROFESSIONAL ASSOCIATION (MOUNTAIN) — Organized agents with cross-organizational reach still cannot eliminate semantic drift. They can reduce mistranslation frequency through training and glossaries, but the underlying velocity differential — operational language evolves faster than administrative language — persists across all coordination interventions. The gap is a structural feature of distributed practice.
constraint_indexing:constraint_classification(semantic_drift_velocity, mountain,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 4: ANALYTICAL OBSERVER (MOUNTAIN) — Semantic drift velocity is a consequence of the relationship between practice and formalization in any sufficiently complex coordination system. Operational vocabularies evolve through use; administrative vocabularies evolve through deliberation. The velocity differential is not a policy choice or institutional failure — it is an information-theoretic property of distributed coordination under innovation. No observer position changes this classification.
constraint_indexing:constraint_classification(semantic_drift_velocity, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 5: MIDDLE MANAGER (MOUNTAIN) — Experiences the semantic gap as a translation burden but recognizes it as structurally unavoidable. Can invest in glossaries, cross-training, and communication protocols to reduce mistranslation incidents, but cannot eliminate the gap itself. The constraint is perceived as a natural limit of organizational scale and operational complexity.
constraint_indexing:constraint_classification(semantic_drift_velocity, mountain,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(semantic_drift_velocity_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(semantic_drift_velocity, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(semantic_drift_velocity, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(semantic_drift_velocity, ExtMetricName, E),
    domain_priors:suppression_score(semantic_drift_velocity, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(semantic_drift_velocity),
    narrative_ontology:constraint_metric(semantic_drift_velocity, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(semantic_drift_velocity, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(semantic_drift_velocity_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.12): Very low. The semantic gap imposes coordination costs (translation time, mistranslation incidents, training overhead) but these are distributed across all participants and are not asymmetrically extracted by any agent. The constraint does not concentrate benefits on one group while imposing costs on another — everyone bears translation burden proportional to their communication load. The small non-zero value reflects real coordination overhead, but this is inherent cost, not extraction. Suppression (0.03): Minimal. No agent is prevented from coining new terms, adopting operational vocabulary, or participating in standardization. The constraint does not suppress alternatives — it IS the alternative space (the full range of possible terms and meanings). Resistance (0.08): Very low. Attempts to eliminate semantic drift (mandatory standardization, vocabulary freezes, centralized term approval) fail because they conflict with operational necessity. Practice-driven innovation is more resistant to institutional control than institutional control is to practice-driven innovation. Accessibility collapse (0.92): Very high. The constraint is accessible to all observers through direct experience — anyone who has written an operational report using terms not yet in the official glossary, or received an administrative directive using terms that no longer match field usage, has encountered this constraint. Theater ratio (0.15): Very low. The coordination costs are real and functional — translation time is actual work, mistranslation incidents have operational consequences, training overhead is genuine knowledge transfer. There is minimal performative content.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits uniform mountain classification across all perspectives, which is the diagnostic signature of a natural law. The powerless frontline operator, the institutional standards committee, the organized professional association, the moderate middle manager, and the analytical observer all perceive the semantic drift velocity as an unchangeable structural feature. The gap between operational and administrative vocabularies persists regardless of resources, authority, coordination mechanisms, or time horizon. This invariance across perspectives confirms that the constraint is not a contingent institutional arrangement that could be reclassified from a different structural position — it is an information-theoretic limit on how fast distributed practice can be formalized. The constraint passes the mountain invariance test: no change in observer position changes the classification.
 *
 * DIRECTIONALITY LOGIC:
 *   No beneficiary or victim groups are declared because the constraint does not extract asymmetrically. All agents bear coordination costs proportional to their communication load. The standards committee is not a beneficiary (they bear the Sisyphean burden of perpetual catch-up). The frontline operator is not a victim (they benefit from the flexibility to innovate vocabulary). The middle manager bears translation costs but also benefits from the operational adaptability that generates the drift. The constraint is symmetric in its impact — it is a coordination limit, not an extraction mechanism. Directionality values default to canonical power-atom mappings, but effective extraction remains very low across all perspectives because base extraction is very low and no structural asymmetry amplifies it.
 *
 * MANDATROPHY ANALYSIS:
 *   MOUNTAIN EXEMPLAR: This constraint demonstrates that low extractiveness does not automatically imply rope classification. The semantic drift velocity has very low extraction (0.12) because the coordination costs are distributed and symmetric, but it is mountain rather than rope because the constraint is immutable. Rope requires that the constraint be a chosen coordination mechanism that could be replaced with alternatives. Semantic drift velocity is not chosen — it emerges necessarily from the relationship between practice and formalization. The mandatrophy resolution is: low extraction + immutability = mountain (natural coordination limit), not rope (chosen coordination mechanism). The constraint also demonstrates the importance of the NL profile metrics (accessibility_collapse, resistance, emerges_naturally). Without these, a constraint with ε=0.12 and suppression=0.03 might be misclassified as rope based on thresholds alone. The NL profile confirms that this is a structural limit, not a coordination choice.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(semantic_drift_velocity, 0, 0).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(semantic_drift_velocity, information_standard).

% DUAL FORMULATION NOTE:
% Semantic drift velocity is a foundational constraint in organizational communication. It does not decompose into multiple stories because the observable (term introduction rate, mistranslation frequency, translation time) produces a stable epsilon value regardless of measurement methodology. The constraint is the velocity differential itself, not any specific vocabulary domain or standardization process.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
