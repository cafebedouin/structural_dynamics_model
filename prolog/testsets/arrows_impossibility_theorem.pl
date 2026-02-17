% ============================================================================
% CONSTRAINT STORY: arrows_impossibility_theorem
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-15
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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:human_readable/2.

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
 *   This constraint story models the POLITICAL USE of Arrow's Impossibility
 *   Theorem to justify and enforce the selection of specific, flawed voting
 *   systems. The theorem proves that no rank-order voting system can
 *   simultaneously satisfy a set of "fairness" criteria. While the mathematical
 *   theorem itself is a Mountain (a logical limit), its application in political
 *   discourse becomes a Tangled Rope: it provides a coordination function for
 *   system designers while simultaneously extracting political hope and
 *   suppressing alternatives, benefiting incumbents.
 *
 * KEY AGENTS (by structural relationship):
 *   - Democratic Reformers: Primary target (moderate/constrained) — bears the
 *     extraction of lost political energy and suppressed alternatives.
 *   - Constitutional Designers & Incumbents: Primary beneficiary (institutional/arbitrage) —
 *     benefits from the stability and predictability of a chosen system, using
 *     the theorem to deflect criticism.
 *   - Individual Voters: Secondary target (powerless/trapped) — experiences the
 *     paradoxes of the chosen system as an unchangeable reality.
 *   - Analytical Observer: Sees the full structure as a Tangled Rope.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
% These metrics reflect the *political application* of the theorem, not the
% mathematical proof itself. The extraction comes from foreclosed political
% possibility and wasted reformist energy.
domain_priors:base_extractiveness(arrows_impossibility_theorem, 0.60).
domain_priors:suppression_score(arrows_impossibility_theorem, 0.40).   % Structural property (raw, unscaled).
domain_priors:theater_ratio(arrows_impossibility_theorem, 0.20).       % Piton detection (>= 0.70)

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(arrows_impossibility_theorem, extractiveness, 0.60).
narrative_ontology:constraint_metric(arrows_impossibility_theorem, suppression_requirement, 0.40).
narrative_ontology:constraint_metric(arrows_impossibility_theorem, theater_ratio, 0.20).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(arrows_impossibility_theorem, tangled_rope).
narrative_ontology:human_readable(arrows_impossibility_theorem, "Arrow's Impossibility Theorem (as a political justification)").

% --- Binary flags ---
% The political enforcement of "no perfect system exists" requires active
% reinforcement in academic and political discourse to suppress alternatives.
domain_priors:requires_active_enforcement(arrows_impossibility_theorem). % Required for Tangled Rope

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% These feed the directionality derivation chain.
%
% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(arrows_impossibility_theorem, constitutional_designers).
narrative_ontology:constraint_beneficiary(arrows_impossibility_theorem, stable_incumbents).
%
% Who bears disproportionate cost?
narrative_ontology:constraint_victim(arrows_impossibility_theorem, democratic_reformers).
narrative_ontology:constraint_victim(arrows_impossibility_theorem, individual_voters).
%
% Gate requirements:
%   Tangled Rope: beneficiary + victim + requires_active_enforcement (all three) -> MET

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × f(d) × σ(S)
   ========================================================================== */

% PERSPECTIVE 1: THE DEMOCRATIC REFORMER (SNARE)
% Agent who bears the most extraction. The theorem is used to strangle hope
% for a "perfect" system, extracting political energy and foreclosing options.
constraint_indexing:constraint_classification(arrows_impossibility_theorem, snare,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 2: THE CONSTITUTIONAL DESIGNER (ROPE)
% Agent who benefits most. For them, the theorem is a coordination tool.
% It provides a justification for choosing a "good enough" system and
% creating stability, deflecting calls for radical change.
constraint_indexing:constraint_classification(arrows_impossibility_theorem, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% Sees both the coordination function (for designers) and the asymmetric
% extraction (from reformers), classifying it as a Tangled Rope.
constraint_indexing:constraint_classification(arrows_impossibility_theorem, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 4: THE INDIVIDUAL VOTER (SNARE)
% Experiences the system's flaws as an inescapable reality. While it feels
% like a Mountain (a law of nature), its high base extractiveness (in terms
% of democratic legitimacy) makes it a Snare from their trapped perspective.
constraint_indexing:constraint_classification(arrows_impossibility_theorem, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).


/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(arrows_impossibility_theorem_tests).

test(perspectival_gap_reformer_vs_designer) :-
    % Verify the gap between the victim (Reformer -> Snare) and beneficiary (Designer -> Rope).
    constraint_indexing:constraint_classification(arrows_impossibility_theorem, TypeTarget,
        context(agent_power(moderate), _, _, _)),
    constraint_indexing:constraint_classification(arrows_impossibility_theorem, TypeBeneficiary,
        context(agent_power(institutional), _, _, _)),
    TypeTarget == snare,
    TypeBeneficiary == rope.

test(analytical_view_is_tangled_rope) :-
    % The ground truth claim must match the analytical perspective.
    narrative_ontology:constraint_claim(arrows_impossibility_theorem, ClaimedType),
    constraint_indexing:constraint_classification(arrows_impossibility_theorem, AnalyticalType,
        context(agent_power(analytical), _, _, _)),
    ClaimedType == tangled_rope,
    AnalyticalType == tangled_rope.

test(tangled_rope_structural_gates_pass) :-
    % Verify all three structural requirements for a Tangled Rope are met.
    narrative_ontology:constraint_beneficiary(arrows_impossibility_theorem, _),
    narrative_ontology:constraint_victim(arrows_impossibility_theorem, _),
    domain_priors:requires_active_enforcement(arrows_impossibility_theorem).

:- end_tests(arrows_impossibility_theorem_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The original file had a MOUNTAIN_METRIC_CONFLICT: it claimed to be a
 *   Mountain while having high extraction (0.6) and suppression (0.4). This
 *   regeneration resolves the conflict by re-framing the story around the
 *   *political application* of the theorem, which is a Tangled Rope, not the
 *   mathematical proof itself, which is a Mountain. The high metrics are now
 *   justified, as they measure the cost of foreclosed political possibilities
 *   and the suppression of alternative (e.g., cardinal) voting systems.
 *
 * PERSPECTIVAL GAP:
 *   The gap is stark. For institutional designers and incumbents, the theorem
 *   is a Rope that coordinates stable governance by providing a powerful
 *   argument ("no system is perfect") to maintain the status quo. For reformers
 *   and voters, this same argument is a Snare that extracts political hope,
 *   wastes energy on impossible goals, and makes them feel trapped by logic.
 *
 * DIRECTIONALITY LOGIC:
 *   - Beneficiaries: 'constitutional_designers' and 'stable_incumbents' use the
 *     theorem to create and maintain predictable systems. Their 'arbitrage' exit
 *     option reflects their ability to choose which fairness axiom to violate.
 *   - Victims: 'democratic_reformers' and 'individual_voters' bear the cost.
 *     Reformers are 'constrained' because the theorem is a barrier to their
 *     goals. Voters are 'trapped' within the resulting flawed system.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification correctly identifies a "false natural law." The
 *   constraint is presented as a Mountain ("it's just math"), but its function
 *   is that of a Tangled Rope (coordination + extraction). By assigning high
 *   extraction and suppression metrics, the system correctly flags that this
 *   is not a neutral law of nature but an actively enforced political choice
 *   with winners and losers.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_arrows_cardinal_utility,
    'Does the "impossibility" hold for non-rank-order systems like Cardinal (Score/Approval) Voting?',
    'Formal analysis of whether cardinal utility systems successfully evade the fairness paradoxes identified by Arrow.',
    'If Yes: The constraint is a Snare specific to ordinal systems, and its enforcement is pure extraction. If No: The constraint is a much harder Mountain of collective choice.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(arrows_impossibility_theorem, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Temporal data reflects the growing use of the theorem to enforce political
% stasis as the complexity of democratic systems has increased.
%
% Theater ratio over time: Stays low, as the constraint's power is its logical, not performative, force.
narrative_ontology:measurement(arrows_tr_t0, arrows_impossibility_theorem, theater_ratio, 0, 0.05).
narrative_ontology:measurement(arrows_tr_t5, arrows_impossibility_theorem, theater_ratio, 5, 0.12).
narrative_ontology:measurement(arrows_tr_t10, arrows_impossibility_theorem, theater_ratio, 10, 0.20).

% Extraction over time: Rises as the theorem becomes more widely known and cited,
% increasing its power to foreclose alternatives and extract political will.
narrative_ontology:measurement(arrows_ex_t0, arrows_impossibility_theorem, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(arrows_ex_t5, arrows_impossibility_theorem, base_extractiveness, 5, 0.52).
narrative_ontology:measurement(arrows_ex_t10, arrows_impossibility_theorem, base_extractiveness, 10, 0.60).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type: The theorem is used to enforce a particular standard of
% what is considered a "possible" or "legitimate" voting system.
narrative_ontology:coordination_type(arrows_impossibility_theorem, enforcement_mechanism).

% --- Network Decomposition (Constraint Families) ---
%
% DUAL FORMULATION NOTE:
% This constraint is one of 2 stories decomposed from "Arrow's Theorem".
% Decomposed because ε differs across observables (ε-invariance principle).
% This story models the POLITICAL USE of the theorem.
% Related stories:
%   - arrows_theorem_mathematical_limit (ε=0.05, Mountain)
%
narrative_ontology:affects_constraint(arrows_theorem_mathematical_limit, arrows_impossibility_theorem).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% No overrides needed. The structural derivation from beneficiary/victim
% declarations and exit options accurately models the directionality.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */