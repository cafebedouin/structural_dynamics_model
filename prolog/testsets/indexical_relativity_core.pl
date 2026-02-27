% ============================================================================
% CONSTRAINT STORY: indexical_relativity_core
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_indexical_relativity_core, []).

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
 *   constraint_id: indexical_relativity_core
 *   human_readable: The Law of Indexical Relativity
 *   domain: epistemological
 *
 * SUMMARY:
 *   Indexical Relativity is the foundational epistemological constraint
 *   underlying the entire Deferential Realism framework. It asserts that no
 *   classification of a social or physical limitation — whether as a
 *   constraint, a resource, a law of nature, or a contingent institution —
 *   can be made without reference to an observer's structural position. The
 *   constraint is not that different observers disagree (a mere fact about
 *   human disagreement), but that disagreement is mathematically necessary
 *   given different values of the four indexical axes (Power, Time, Exit,
 *   Scope). This is a law of classification logic itself, not a limitation of
 *   observation. It applies universally across all domains: mathematical,
 *   physical, social, institutional, and epistemological. The constraint
 *   exhibits zero perspectival variance — all observers, from any position,
 *   agree that indexical relativity holds. This agreement-about-disagreement
 *   is the signature of a mountain constraint: the invariant is not what
 *   different observers conclude, but the structure that makes their
 *   conclusions context-dependent.
 *
 * KEY AGENTS:
 *   - Analytical Observer: Discovers and formalizes the law (analytical/analytical) — sees indexical relativity as a logical tautology
 *   - Institutional Knowledge Keeper: Learns indexical relativity as a hard limit on classification schemes (institutional/analytical) — cannot produce context-free systems
 *   - Policy-Maker Attempting Universality: Discovers indexical relativity when implementing universal rules (powerful/analytical) — cannot legislate away perspectival gaps
 *   - Constrained Agent Experiencing Classification Gaps: Discovers that their perspective is classified as parochial by authority (powerless/trapped) — the law manifests as structural irrelevance of their viewpoint
 *   - The Deferential Realism Framework: Embeds indexical relativity as its foundational principle (institutional/analytical) — makes context-dependence explicit through the (P,T,E,S) tuple
 *   - Physical Science Tradition: Parallel discovery in special relativity (analytical/civilizational) — abandonment of frame-independence as a logical necessity
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(indexical_relativity_core, 0.08).
domain_priors:suppression_score(indexical_relativity_core, 0.02).
domain_priors:theater_ratio(indexical_relativity_core, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(indexical_relativity_core, extractiveness, 0.08).
narrative_ontology:constraint_metric(indexical_relativity_core, suppression_requirement, 0.02).
narrative_ontology:constraint_metric(indexical_relativity_core, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(indexical_relativity_core, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(indexical_relativity_core, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(indexical_relativity_core, mountain).
narrative_ontology:human_readable(indexical_relativity_core, "The Law of Indexical Relativity").
narrative_ontology:topic_domain(indexical_relativity_core, "epistemological").

domain_priors:emerges_naturally(indexical_relativity_core).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: LOGICAL INVARIANT (MOUNTAIN) — From a universal analytical view, indexical relativity is a tautology of classification theory itself. Any classification (constraint type, agent power, exit option, scope) is relative to an observer position. This is not a contingent feature of how we measure constraints — it is a necessary consequence of the definition of classification. Classification without an indexical context is meaningless. Zero degrees of freedom: all observers universally agree that classification requires a context tuple.
constraint_indexing:constraint_classification(indexical_relativity_core, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 2: INSTITUTIONAL KNOWLEDGE KEEPER (MOUNTAIN) — Institutions that systematize constraint classification (academia, regulatory bodies, frameworks like Deferential Realism) discover indexical relativity as a hard limit on their classification schemes. They cannot produce a context-free classification that holds for all observers and times. This limit is not a failure of their system — it is a discovery about the structure of classification itself. The constraint persists regardless of institutional effort to escape it.
constraint_indexing:constraint_classification(indexical_relativity_core, mountain,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 3: POLICY-MAKER (MOUNTAIN) — When a powerful agent (regulator, legislator, institutional authority) attempts to implement a 'universal' classification rule across a jurisdiction, they immediately encounter indexical relativity: what appears as a constraint from the perspective of the affected population may appear as a coordination mechanism from the bureaucrat's perspective. The policy-maker cannot legislate away this perspectival gap — it is inherent to the structure of the constraint itself, not a feature of implementation quality.
constraint_indexing:constraint_classification(indexical_relativity_core, mountain,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(analytical),
            spatial_scope(national))).

% PERSPECTIVE 4: CONSTRAINED AGENT (MOUNTAIN) — An agent experiencing a constraint as extractive (snare) discovers that a powerful actor claims it is merely coordination (rope) or even natural law (mountain). The powerless agent's experience of indexical relativity is that their classification is irrelevant to the official account. This gap is not resolvable through more evidence or better argumentation — it is a structural feature of how different observer positions generate different classifications. The constraint (indexical relativity itself) is that the agent cannot escape the fact of their parochial viewpoint.
constraint_indexing:constraint_classification(indexical_relativity_core, mountain,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 5: META-OBSERVER / DR FRAMEWORK (MOUNTAIN) — The Deferential Realism framework itself is built on the recognition that indexical relativity is a logical law. The (P,T,E,S) tuple is the formal encoding of this law: no classification is valid without specifying power, time, exit, and scope. The framework does not try to escape indexical relativity — it makes relativity the core of the classification system. This is the mountain interpretation made explicit: classification without context is not a weaker form of classification, it is incoherent.
constraint_indexing:constraint_classification(indexical_relativity_core, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 6: PHYSICAL SCIENCE ANALOG (MOUNTAIN) — Indexical relativity parallels Einstein's relativity: there is no frame-independent fact about simultaneity, length, or time in special relativity. Similarly, there is no observer-independent fact about whether a constraint is extractive or coordinative. Both are structural properties of the relationship between the observer and the observed system. Physical relativity required abandoning frame-independence; epistemological relativity requires abandoning context-independence. This is a mathematical and logical necessity, not a limitation of measurement technology or institutional design.
constraint_indexing:constraint_classification(indexical_relativity_core, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(indexical_relativity_core_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(indexical_relativity_core, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(indexical_relativity_core, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(indexical_relativity_core, ExtMetricName, E),
    domain_priors:suppression_score(indexical_relativity_core, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(indexical_relativity_core),
    narrative_ontology:constraint_metric(indexical_relativity_core, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(indexical_relativity_core, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(indexical_relativity_core_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.08): Minimal. Indexical relativity does not extract from any agent in the conventional sense — no agent is coercively constrained by the law itself. However, the law's necessity means that any agent attempting to implement a context-free classification system will encounter failure. The 'extraction' is subtle: the impossibility of escape from context-dependence. Suppression (0.02): Negligible. The law is not hidden or suppressed — it is discovered through formal analysis and embedded in the DR framework explicitly. Once recognized, it cannot be suppressed because denying indexical relativity produces logical incoherence. Theater ratio (0.05): Near-zero. No performative content — the law either holds or it does not. All observations confirm it; no counterexample has been found. The minimal non-zero value reflects that the law must be taught and explained (linguistic performance), not that the law itself is theatrical.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits ZERO perspectival gap, which is the defining characteristic of a mountain constraint. All six perspectives, from powerless to analytical, from immediate to civilizational, from trapped to arbitrage, agree that indexical relativity is an immutable law. The perspectival gap is not in disagreement about whether the law holds, but in how the law's necessity is experienced. The analytical observer sees it as a logical tautology; the powerless agent discovers it when their perspective is dismissed as parochial; the policy-maker discovers it when universal rules fragment across contexts. The law itself is invariant — only its experiential manifestation varies by position.
 *
 * DIRECTIONALITY LOGIC:
 *   Indexical relativity has no beneficiaries or victims in the conventional sense, because the law does not extract from agents — it describes the structure of classification itself. No agent can gain or lose by indexical relativity being true or false; the law is not contingent on any agent's choice. The directionality value is undefined (or equivalently, symmetric across all positions) because the constraint is not about differential extraction but about the necessity of context. Every agent, regardless of power or exit options, must acknowledge that their classification is context-dependent. There are no structural beneficiaries and no victims — only the universal constraint that context-dependence is logically necessary.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint does not face mandatrophy because it does not involve a coordination function that could be confused with extraction. The law is about the structure of classification, not about resource allocation or coercive mechanisms. There is no risk of mislabeling it as a snare (pure extraction) or a rope (pure coordination) because those categories apply to constraints between agents, and indexical relativity is a meta-constraint about how all such constraints are classified. The law's truth does not depend on institutional enforcement or suppression — it follows from the definition of classification itself. Mandatrophy is resolved by recognizing that indexical relativity is not a contingent institution requiring enforcement, but a logical necessity requiring only recognition and formalization.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    metalinguistic_scope,
    'Does indexical relativity apply to itself? Is the claim ''indexical relativity is a mountain'' itself context-dependent?',
    'Formal logical analysis of self-referential classification; comparison with Gödel''s incompleteness and Tarski''s undefinability results',
    'If yes (self-reflexive): the law is about itself, creating a coherent loop. If no (excepted): indexical relativity has a privileged status that requires justification. The distinction determines whether the framework is self-consistent or contains a foundational exception.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(metalinguistic_scope, conceptual, 'Whether indexical relativity is itself subject to indexical relativity').

omega_variable(
    observer_privileging,
    'Can an observer position (analytical, civilizational, universal scope) be more ''correct'' than others, or are all positions equally valid?',
    'Epistemological analysis of observer parity; examination of whether some positions have epistemic advantages (e.g., civilizational scope captures more invariants); historical cases of perspectival agreement convergence',
    'If some positions are privileged: indexical relativity is asymmetric, and the framework should optimize toward analytical perspectives. If symmetric: all perspectives are equally valid, and disagreement is inherent to the system.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(observer_privileging, conceptual, 'Whether some observer positions have epistemic privilege').

omega_variable(
    empirical_constraint_identity,
    'When do two constraints described from different perspectives represent the same underlying phenomenon vs. two distinct constraints?',
    'Examination of the ε-invariance principle; cases where the same constraint gets two different ε values when measured via different observables (triggering decomposition into separate stories)',
    'If observer determines constraint identity: constraints are not pre-theoretical objects but emerge from measurement choice. If constraints have identity independent of measurement: some constraints should yield the same ε across observables (mountain signature).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(empirical_constraint_identity, empirical, 'Whether constraint identity is observer-dependent or intrinsic').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(indexical_relativity_core, 0, 1000).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(idxrel_tr_t0, indexical_relativity_core, theater_ratio, 0, 0.02).
narrative_ontology:measurement(idxrel_tr_t500, indexical_relativity_core, theater_ratio, 500, 0.04).
narrative_ontology:measurement(idxrel_tr_t1000, indexical_relativity_core, theater_ratio, 1000, 0.05).

% Extraction over time
narrative_ontology:measurement(idxrel_be_t0, indexical_relativity_core, base_extractiveness, 0, 0.07).
narrative_ontology:measurement(idxrel_be_t500, indexical_relativity_core, base_extractiveness, 500, 0.08).
narrative_ontology:measurement(idxrel_be_t1000, indexical_relativity_core, base_extractiveness, 1000, 0.08).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(indexical_relativity_core, information_standard).
narrative_ontology:affects_constraint(indexical_relativity_core, mountain_false_summit_detection).
narrative_ontology:affects_constraint(indexical_relativity_core, constraint_identity_and_epsilon_invariance).
narrative_ontology:affects_constraint(indexical_relativity_core, perspectival_gap_measurement).

% DUAL FORMULATION NOTE:
% Indexical Relativity is the meta-constraint that determines how all other constraints are classified. It is upstream of every constraint story in the corpus because every classification (mountain, rope, tangled_rope, snare, scaffold, piton) depends on specifying an observer's (P,T,E,S) position. The constraint affects all others by establishing that context-dependence is not a limitation but a necessity.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
