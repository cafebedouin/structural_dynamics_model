% ============================================================================
% CONSTRAINT STORY: gilgamesh_mortality_limit
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_gilgamesh_mortality_limit, []).

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
 *   constraint_id: gilgamesh_mortality_limit
 *   human_readable: The Allotment of Mortality
 *   domain: philosophical/religious
 *
 * SUMMARY:
 *   In the Epic of Gilgamesh, King Gilgamesh learns that mortality is the
 *   ultimate allotment imposed by the gods, the irreversible limit that
 *   defines the condition of being human. After his friend Enkidu dies,
 *   Gilgamesh seeks Utnapishtim, the survivor of the flood who has been
 *   granted immortality, hoping to escape death. Utnapishtim's story confirms
 *   the immutability of the decree: the gods have allotted mortality to
 *   humans and immortality only to themselves. No negotiation, innovation, or
 *   heroic action can override this constraint. The Epic portrays mortality
 *   not as extractive oppression but as a foundational law — as inescapable
 *   and impersonal as gravity. This constraint models mortality as the
 *   ultimate physical and metaphysical limit: the irreducible boundary
 *   between existence and non-existence that no agent can negotiate or
 *   transcend through collective action. Mortality is not suppressed by
 *   institutions; it is enforced by the structure of thermodynamics, cellular
 *   senescence, and entropy itself. The very low extractiveness (ε=0.08) and
 *   suppression (0.02) reflect that the constraint operates through natural
 *   law, not through coercive institutional mechanisms. Theater is minimal
 *   because the constraint requires no performative maintenance — death
 *   enforces itself.
 *
 * KEY AGENTS:
 *   - Gilgamesh (and all humans): Primary subject (powerless/trapped) — bears the constraint universally; no exit option available
 *   - The Gods (cosmic order): Institutional actor (institutional/analytical) — impose the allotment as the foundation of divine hierarchy and cosmic order
 *   - Enkidu: Victim (powerless/trapped) — exemplifies the constraint through death; his mortality forces Gilgamesh's existential crisis
 *   - Utnapishtim: Exceptional case (institutional/arbitrage) — granted immortality by the gods, confirming that the allotment is imposed by fiat, not by natural law alone (omega variable)
 *   - The Analytical Observer: Civilizational view (analytical/analytical) — assesses whether the mythological 'allotment' is descriptive natural law or prescriptive religious narrative
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gilgamesh_mortality_limit, 0.08).
domain_priors:suppression_score(gilgamesh_mortality_limit, 0.02).
domain_priors:theater_ratio(gilgamesh_mortality_limit, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gilgamesh_mortality_limit, extractiveness, 0.08).
narrative_ontology:constraint_metric(gilgamesh_mortality_limit, suppression_requirement, 0.02).
narrative_ontology:constraint_metric(gilgamesh_mortality_limit, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gilgamesh_mortality_limit, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(gilgamesh_mortality_limit, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gilgamesh_mortality_limit, mountain).
narrative_ontology:human_readable(gilgamesh_mortality_limit, "The Allotment of Mortality").
narrative_ontology:topic_domain(gilgamesh_mortality_limit, "philosophical/religious").

domain_priors:emerges_naturally(gilgamesh_mortality_limit).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE MORTAL AGENT (MOUNTAIN) — No exit from the biological limit of death. The constraint is experienced as an irreducible condition of existence. d≈1.0, f(d)≈1.42, σ=1.0 → χ≈0.11. The high directionality reflects that the agent is the substrate of the constraint, not a negotiable party.
constraint_indexing:constraint_classification(gilgamesh_mortality_limit, mountain,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(universal))).

% PERSPECTIVE 2: RELIGIOUS INSTITUTIONAL OBSERVER (MOUNTAIN) — From the perspective of religious and philosophical traditions (Sumerian, Hindu, Christian, Islamic), mortality is classified as a fundamental condition imposed by divine order or cosmic law. The constraint appears as immutable across all observational contexts and timeframes. d≈0.0, f(d)≈-0.12, σ=1.0 → χ≈-0.01. Institutional actors frame mortality as a necessary limit that preserves cosmic order, not as an extractive mechanism.
constraint_indexing:constraint_classification(gilgamesh_mortality_limit, mountain,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 3: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From the standpoint of biology, thermodynamics, and physics, mortality emerges from entropy, metabolic limits, cellular senescence, and the second law of thermodynamics. No organism escapes these fundamental laws. ε=0.08 reflects that the constraint is purely a natural law with zero degrees of freedom. d≈0.72, f(d)≈1.15, σ=1.0 → χ≈0.09. The analytical frame confirms the mountain classification across all dimensions.
constraint_indexing:constraint_classification(gilgamesh_mortality_limit, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gilgamesh_mortality_limit_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(gilgamesh_mortality_limit, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(gilgamesh_mortality_limit, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(gilgamesh_mortality_limit, ExtMetricName, E),
    domain_priors:suppression_score(gilgamesh_mortality_limit, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(gilgamesh_mortality_limit),
    narrative_ontology:constraint_metric(gilgamesh_mortality_limit, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(gilgamesh_mortality_limit, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(gilgamesh_mortality_limit_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.08): Minimal. The constraint operates through thermodynamic limits and entropy, not through resource extraction or institutional rent-seeking. No agent profits from mortality in the way they profit from artificial scarcity or monopoly. The 'extraction' is purely the exhaustion of life — the conversion of potential into actuality. Suppression (0.02): Minimal. There are no alternatives to suppress; the constraint does not need to eliminate competitors because there are none. Mortality is enforced by physics, not by silencing dissent. Theater ratio (0.15): Very low. The constraint requires no performative maintenance. Death needs no ritual confirmation or institutional theater — it happens regardless. Religious and philosophical narratives about mortality are commentary, not the enforcement mechanism. Accessibility collapse (0.92): Very high. Every living organism will encounter the limit of mortality; it cannot be hidden or delayed indefinitely. No observer can access a world where the constraint fails to operate. Resistance (0.08): Very low. No known intervention permanently defeats death, though some (hibernation, cryopreservation, medical life extension) can delay it. The resistance reflects the marginal success of medical and biological interventions against the underlying constraint.
 *
 * PERSPECTIVAL GAP:
 *   Unlike most constraints, the gap here is minimal. All observers — the powerless agent who dies, the institutional observer who frames mortality as divine order, and the analytical observer who analyzes it as entropy — agree on the classification: Mountain. There is no perspectival ambiguity because the constraint is truly universal. Gilgamesh, Utnapishtim, and the Sumerian theologian would all agree: mortality is non-negotiable. The gap that does emerge is between the mythological framing (the gods' decree) and the physical framing (thermodynamic necessity). This gap raises the omega variable: Is the 'allotment' a description of natural law or a religious narrative that naturalizes the constraint? The mythological version emphasizes that the gods chose this limit, implying that a different cosmic order could have been arranged. The physical version emphasizes that entropy is not a choice — it is a consequence of the structure of reality. Both agree on the mountain classification; they differ on whether the underlying cause is intentional or structural.
 *
 * DIRECTIONALITY LOGIC:
 *   Mortal agent: Powerless + trapped → d≈1.0, f(d)≈1.42. The agent IS the substrate of the constraint; they are not external to it. The maximum d reflects that the entire body of the mortal agent is the locus of the mortality constraint. Institutional observer: Institutional + analytical → d≈0.0, f(d)≈-0.12. The gods (or cosmic order) are the beneficiary of the hierarchy established by mortality — immortality distinguishes the divine from the mortal. But this relationship is not extractive in the economic sense; it is foundational. The negative chi reflects that the institutional observer does not experience the constraint as something imposed on them, but as something they (or the order they represent) maintain. Analytical observer: Analytical + analytical → d≈0.72, f(d)≈1.15. The observer occupies an external epistemic position, measuring the constraint from a position of knowledge rather than experience. The moderate d reflects that analysis itself is not directly subject to mortality (knowledge persists; organisms do not), yet the analyst is ultimately mortal as well. No override needed — the derivation captures the relationship.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    biological_necessity_vs_contingency,
    'Is mortality biologically necessary (unavoidable from first principles of chemistry and thermodynamics) or merely contingent (possibly bypassed by future technology)?',
    'Theoretical advances in aging biology, longevity research, and molecular repair mechanisms; empirical evidence of maximum lifespan extension in model organisms; assessment of whether theoretical thermodynamic limits have been correctly identified',
    'If biologically necessary: mountain classification holds from all perspectives. If contingent: the constraint degrades to Tangled Rope or Scaffold (humans may escape it through technology, suggesting it is extractive institutional framing, not natural law).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(biological_necessity_vs_contingency, empirical, 'Whether mortality is biologically necessary or technologically contingent').

omega_variable(
    mythological_vs_phenomenological_status,
    'Does Gilgamesh''s mythological framing (the gods'' allotment, Anu''s decree) constitute descriptive natural law or prescriptive institutional imposition disguised as nature?',
    'Textual and historical analysis of Sumerian religious cosmology; comparison with other mythological mortality narratives; assessment of whether the ''decree'' reflects observed limits or serves ideological functions (social control, legitimation of hierarchy)',
    'If descriptive: mountain holds. If prescriptive: the constraint is actually a Snare or Tangled Rope (religious authorities using natural-law framing to suppress the search for life-extension or resurrection). The narrative itself becomes the extractive mechanism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mythological_vs_phenomenological_status, conceptual, 'Whether mythological framing reflects natural law or ideological imposition').

omega_variable(
    irreversibility_of_entropic_commitment,
    'Once an organism commits to irreversible entropic pathways (reproduction, metabolism, aging), can information-theoretic resurrection or radical life extension remain available in principle?',
    'Advances in reversible computing, quantum information preservation, and molecular archaeology; theoretical work on whether the second law permits unbounded biological persistence',
    'If truly irreversible: mountain holds universally. If reversible (information never fully lost): the constraint becomes Rope (coordination on which organisms persist) or Scaffold (temporary until reversal technology matures).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(irreversibility_of_entropic_commitment, empirical, 'Whether entropic commitment irreversibly enforces mortality').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gilgamesh_mortality_limit, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gm_tr_t0, gilgamesh_mortality_limit, theater_ratio, 0, 0.15).
narrative_ontology:measurement(gm_tr_t50, gilgamesh_mortality_limit, theater_ratio, 50, 0.15).
narrative_ontology:measurement(gm_tr_t100, gilgamesh_mortality_limit, theater_ratio, 100, 0.15).

% Extraction over time
narrative_ontology:measurement(gm_be_t0, gilgamesh_mortality_limit, base_extractiveness, 0, 0.08).
narrative_ontology:measurement(gm_be_t50, gilgamesh_mortality_limit, base_extractiveness, 50, 0.08).
narrative_ontology:measurement(gm_be_t100, gilgamesh_mortality_limit, base_extractiveness, 100, 0.08).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gilgamesh_mortality_limit, global_infrastructure).
narrative_ontology:affects_constraint(gilgamesh_mortality_limit, human_finitude_narrative).
narrative_ontology:affects_constraint(gilgamesh_mortality_limit, entropy_second_law).

% DUAL FORMULATION NOTE:
% The Gilgamesh mortality constraint is upstream of multiple constraint families in philosophy and physics. It connects the mythological-religious formulation (death as divine allotment) to the biological formulation (senescence and entropy) and the metaphysical formulation (finitude as the human condition). These are separate stories with different ε values and perspectival gaps, but they all trace to the same underlying irreversible limit. The mythological version (Gilgamesh narrative) emphasizes the institutional framing and choice; the biological version emphasizes mechanism; the metaphysical version emphasizes the existential consequence. All three must be included in a complete family analysis.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
