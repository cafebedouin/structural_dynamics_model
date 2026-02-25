% ============================================================================
% CONSTRAINT STORY: arrows_impossibility_theorem
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-28
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_arrows_impossibility_theorem, []).

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
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: arrows_impossibility_theorem
 *   human_readable: Arrow's Impossibility Theorem (as a political justification)
 *   domain: political/economic
 *
 * SUMMARY:
 *   This constraint story models the political USE of Arrow's Impossibility
 *   Theorem, not the mathematical theorem itself. The theorem, a formal proof
 *   about the limits of ranked-voting systems, is a mathematical Mountain
 *   (ε≈0). However, its application in political discourse functions as a
 *   social constraint to justify flawed status-quo voting systems and
 *   suppress reform efforts. By framing any imperfection in an alternative
 *   system as a fatal flaw predicted by an inexorable mathematical law,
 *   political incumbents create a powerful rhetorical barrier to change.
 *
 * KEY AGENTS:
 *   - Political Incumbents: Primary beneficiaries (institutional/arbitrage) — Use the theorem to defend the existing electoral system that advantages them.
 *   - Electoral Reform Advocates: Primary victims (powerless/trapped) — Their proposals are dismissed as mathematically naive, extracting their political agency.
 *   - Cynical Pundits: Enforcers (powerful/mobile) — Wield the theorem as a theatrical tool to signal authority and shut down debate.
 *   - Disenfranchised Voters: Secondary victims (powerless/trapped) — Bear the costs of the sub-optimal voting system.
 *   - Analytical Observer: Sees the full structure of coordination and extraction.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(arrows_impossibility_theorem, 0.55).
domain_priors:suppression_score(arrows_impossibility_theorem, 0.75).
domain_priors:theater_ratio(arrows_impossibility_theorem, 0.75).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(arrows_impossibility_theorem, extractiveness, 0.55).
narrative_ontology:constraint_metric(arrows_impossibility_theorem, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(arrows_impossibility_theorem, theater_ratio, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(arrows_impossibility_theorem, tangled_rope).
narrative_ontology:human_readable(arrows_impossibility_theorem, "Arrow's Impossibility Theorem (as a political justification)").
narrative_ontology:topic_domain(arrows_impossibility_theorem, "political/economic").

domain_priors:requires_active_enforcement(arrows_impossibility_theorem).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(arrows_impossibility_theorem, political_incumbents).
narrative_ontology:constraint_beneficiary(arrows_impossibility_theorem, status_quo_partisans).
narrative_ontology:constraint_victim(arrows_impossibility_theorem, electoral_reform_advocates).
narrative_ontology:constraint_victim(arrows_impossibility_theorem, disenfranchised_voters).
narrative_ontology:constraint_victim(arrows_impossibility_theorem, third_party_movements).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ELECTORAL REFORM ADVOCATE (SNARE) — Trapped within a political system they seek to change. The theorem is wielded as an unassailable justification for inaction, creating a snare that extracts political agency and suppresses alternatives. d≈0.95, f(d)≈1.42, σ=1.0 → χ≈0.78.
constraint_indexing:constraint_classification(arrows_impossibility_theorem, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: STATUS QUO INCUMBENT (ROPE) — Benefits from the current flawed system. The theorem is a pure coordination tool to maintain consensus, prevent disruptive reforms, and ensure stability (of their own power). d≈0.05, f(d)≈-0.12, σ=1.0 → χ≈-0.07. Negative extraction signifies a net subsidy.
constraint_indexing:constraint_classification(arrows_impossibility_theorem, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: THE CYNICAL PUNDIT (PITON) — The theorem's mathematical substance is inert in the political debate; its function is purely theatrical. Invoking it signals intellectual authority to shut down arguments, a performative act that persists due to institutional inertia in political discourse. theater_ratio=0.75 satisfies the piton gate (≥0.70).
constraint_indexing:constraint_classification(arrows_impossibility_theorem, piton,
    context(agent_power(powerful),
            time_horizon(immediate),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 4: ANALYTICAL OBSERVER (TANGLED ROPE) — Sees both the genuine (if minimal) coordination function of preventing endless cycling over voting systems, and the significant asymmetric extraction where incumbents benefit at the expense of reformers. The high suppression and extraction are undeniable. d≈0.72, f(d)≈1.15, σ=1.2 → χ≈0.76.
constraint_indexing:constraint_classification(arrows_impossibility_theorem, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 5: THE MATHEMATICAL PURIST (MOUNTAIN) — This perspective ignores the political application and sees only the theorem itself: an unchangeable, fixed limit of social choice theory. This is a 'false summit'; the engine will reject this classification because the base properties (ε=0.55, suppression=0.75) are inconsistent with a Mountain. This demonstrates the naturalization of a social constraint.
constraint_indexing:constraint_classification(arrows_impossibility_theorem, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(arrows_impossibility_theorem_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(arrows_impossibility_theorem, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(arrows_impossibility_theorem, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(arrows_impossibility_theorem, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(arrows_impossibility_theorem, TR),
    TR >= 0.70.

:- end_tests(arrows_impossibility_theorem_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.55): Represents the significant extraction of political agency and possibility from reform movements and the general populace. Suppression (0.75): The argument from 'mathematical impossibility' is an extremely effective tool for shutting down debate and delegitimizing alternative proposals. Theater Ratio (0.75): Invoking a complex, Nobel-winning mathematical theorem that few understand is a classic example of performative intellectualism in politics, where the gesture of authority is more important than the technical substance.
 *
 * PERSPECTIVAL GAP:
 *   The gap is profound. Incumbents see a Rope that coordinates everyone around a stable (and beneficial) system. Reformers experience a Snare that uses abstract logic to trap them in a broken reality. Pundits use it as a Piton, a rhetorical club that has lost its original analytical function but still works to win arguments. The analytical observer sees the Tangled Rope: a mix of a legitimate (if misapplied) coordination principle and a clear extractive agenda. The 'Mathematical Purist' perspective illustrates the category error of conflating a law of logic with a law of politics, a 'false summit' the framework is designed to detect.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (incumbents) have arbitrage exit from the debate's substance and benefit from the outcome, leading to a low 'd' value and a Rope classification. Victims (reformers) are trapped in the system and bear the full cost of inaction, leading to a high 'd' value and a Snare classification. The analytical observer's position derives a 'd' value that correctly identifies the mixed nature of the constraint as a Tangled Rope.
 *
 * MANDATROPHY ANALYSIS:
 *   This case resolves the mandatrophy by strictly separating the mathematical object from its political deployment. To label the political situation a 'Mountain' because its justification is rooted in a mathematical theorem is a category error that naturalizes a contingent political choice. The framework correctly identifies the political use-case as a high-extraction, high-suppression social constraint (Tangled Rope/Snare), while acknowledging that a different constraint story (`arrows_theorem_mathematical`) would correctly classify the theorem itself as a Mountain. This decomposition is central to the ε-invariance principle.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    public_comprehension_impact,
    'To what extent does the public''s lack of understanding of the theorem''s technical details enable its use as a tool of suppression?',
    'Comparative analysis of reform debates in populations with high vs. low mathematical literacy; polling on the theorem''s perceived authority vs. actual understanding.',
    'If low comprehension is the primary enabler, the constraint is a Snare based on information asymmetry. If elites use it to persuade other elites regardless of public opinion, it''s a Tangled Rope of elite consensus.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(public_comprehension_impact, empirical, 'Impact of public''s technical understanding on the constraint''s power').

omega_variable(
    alternative_system_viability,
    'Are there viable voting systems (e.g., ranked-choice, approval, score voting) that, while not ''perfect'' by Arrow''s criteria, are demonstrably superior to the status quo?',
    'Empirical studies of jurisdictions using these alternative systems, measuring voter satisfaction, representation fidelity, and spoiler effects.',
    'If viable, superior alternatives are proven to exist, the ''impossibility'' justification becomes a pure Snare. If all alternatives have significant, comparable trade-offs, the justification retains a Tangled Rope character.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(alternative_system_viability, empirical, 'Whether viable, superior voting systems exist in practice').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(arrows_impossibility_theorem, 1951, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(arro_tr_t0, arrows_impossibility_theorem, theater_ratio, 0, 0.3).
narrative_ontology:measurement(arro_tr_t39, arrows_impossibility_theorem, theater_ratio, 39, 0.6).
narrative_ontology:measurement(arro_tr_t73, arrows_impossibility_theorem, theater_ratio, 73, 0.75).

% Extraction over time
narrative_ontology:measurement(arro_be_t0, arrows_impossibility_theorem, base_extractiveness, 0, 0.2).
narrative_ontology:measurement(arro_be_t39, arrows_impossibility_theorem, base_extractiveness, 39, 0.4).
narrative_ontology:measurement(arro_be_t73, arrows_impossibility_theorem, base_extractiveness, 73, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(arrows_impossibility_theorem, enforcement_mechanism).
narrative_ontology:affects_constraint(arrows_impossibility_theorem, first_past_the_post_voting).
narrative_ontology:affects_constraint(arrows_impossibility_theorem, two_party_system_dominance).

% DUAL FORMULATION NOTE:
% This story models the political *use* of Arrow's Theorem, a social constraint with ε=0.55. The underlying mathematical theorem is a separate constraint, `arrows_theorem_mathematical`, which is a Mountain with ε≈0.0. The political constraint weaponizes the authority of the mathematical one.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
