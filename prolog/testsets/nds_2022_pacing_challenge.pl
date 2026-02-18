% ============================================================================
% CONSTRAINT STORY: nds_2022_pacing_challenge
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-05-21
% ============================================================================

:- module(constraint_nds_2022_pacing_challenge, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: nds_2022_pacing_challenge
 *   human_readable: US National Defense Strategy 2022: Pacing Challenge Doctrine
 *   domain: geopolitical
 *
 * SUMMARY:
 *   The 2022 US National Defense Strategy (NDS) establishes a geopolitical
 *   framework centered on "integrated deterrence" against China (the "pacing
 *   challenge") and Russia (an "acute threat"). This doctrine coordinates US
 *   military posture, directs massive resource flows to the defense sector,
 *   and pressures allies into strategic alignment. It functions as both a
 *   coordination mechanism for national security and an extractive system
 *   transferring wealth from the tax base to the defense industry.
 *
 * KEY AGENTS (by structural relationship):
 *   - US Taxpayers: Primary target (powerless/trapped) — bear the financial cost of the strategy.
 *   - US Defense Industrial Base: Primary beneficiary (institutional/arbitrage) — receives vast, long-term funding contracts.
 *   - US Allied Nations: Secondary beneficiary/target (organized/constrained) — benefit from security but are pressured to align policy and spending.
 *   - Designated Rival Nations (China, Russia): Explicit institutional targets (powerful/constrained) — the strategy is designed to limit their geopolitical options.
 *   - Analytical Observer: Sees the full dual-function structure.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(nds_2022_pacing_challenge, 0.48).
domain_priors:suppression_score(nds_2022_pacing_challenge, 0.75).   % Structural property (raw, unscaled).
domain_priors:theater_ratio(nds_2022_pacing_challenge, 0.20).       % Piton detection (>= 0.70)

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(nds_2022_pacing_challenge, extractiveness, 0.48).
narrative_ontology:constraint_metric(nds_2022_pacing_challenge, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(nds_2022_pacing_challenge, theater_ratio, 0.20).

% --- NL Profile Metrics (required for mountain constraints) ---
% N/A. This is a human-constructed policy, not a natural law.

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(nds_2022_pacing_challenge, tangled_rope).
narrative_ontology:human_readable(nds_2022_pacing_challenge, "US National Defense Strategy 2022: Pacing Challenge Doctrine").
narrative_ontology:topic_domain(nds_2022_pacing_challenge, "geopolitical").

% --- Binary flags ---
domain_priors:requires_active_enforcement(nds_2022_pacing_challenge). % Required for Tangled Rope

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% These feed the directionality derivation chain: the engine computes
% d (directionality) from agent membership in these groups + exit_options.

% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(nds_2022_pacing_challenge, us_defense_industrial_base).
narrative_ontology:constraint_beneficiary(nds_2022_pacing_challenge, us_dod_leadership).
narrative_ontology:constraint_beneficiary(nds_2022_pacing_challenge, us_allied_nations).

% Who bears disproportionate cost?
narrative_ontology:constraint_victim(nds_2022_pacing_challenge, us_taxpayers).
narrative_ontology:constraint_victim(nds_2022_pacing_challenge, designated_rival_nations).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × f(d) × σ(S)
   where f(d) is the sigmoid directionality function:
     f(d) = -0.20 + 1.70 / (1 + e^(-6*(d - 0.50)))
   The engine derives d from beneficiary/victim membership + exit_options.
   Scope modifiers: local=0.8, regional=0.9, national=1.0,
                    continental=1.1, global=1.2, universal=1.0.
   CONTEXT ARITY: All context() terms must have exactly 4 arguments.
   ========================================================================== */

% PERSPECTIVE 1: THE PRIMARY TARGET (US TAXPAYER)
% Bears the full financial burden with no exit. High ε, high suppression, and a
% derived d≈0.95 (victim + trapped) lead to a very high effective extraction χ,
% classifying the constraint as a Snare.
constraint_indexing:constraint_classification(nds_2022_pacing_challenge, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE PRIMARY BENEFICIARY (DEFENSE INDUSTRIAL BASE)
% Receives massive, directed funding. As a beneficiary with arbitrage exit
% options (e.g., foreign military sales), the derived d≈0.05 yields a negative
% χ. From this view, the NDS is a pure Rope subsidizing their existence.
constraint_indexing:constraint_classification(nds_2022_pacing_challenge, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER
% Sees both the genuine coordination function (deterrence) and the vast,
% asymmetric extraction. The metrics (ε=0.48, suppression=0.75) and the
% presence of beneficiaries, victims, and active enforcement satisfy all
% conditions for a Tangled Rope.
constraint_indexing:constraint_classification(nds_2022_pacing_challenge, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% --- INTER-INSTITUTIONAL PERSPECTIVES ---

% PERSPECTIVE 4: ALLIED NATIONS
% Allies are in a complex position: they benefit from the US security guarantee
% but are simultaneously pressured to increase defense spending and align
% their foreign policy. Their exit is constrained. This hybrid role is best
% captured as a Tangled Rope. We use a directionality override to reflect this
% tension, which simple beneficiary/victim declaration might miss.
constraint_indexing:constraint_classification(nds_2022_pacing_challenge, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 5: DESIGNATED RIVAL NATIONS
% As the explicit targets of the strategy, they experience it as a coercive
% mechanism designed to constrain their actions. From their powerful but
% constrained position, it is an externally imposed Snare.
constraint_indexing:constraint_classification(nds_2022_pacing_challenge, snare,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).


/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(nds_2022_pacing_challenge_tests).

test(perspectival_gap_target_beneficiary) :-
    constraint_indexing:constraint_classification(nds_2022_pacing_challenge, snare, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(nds_2022_pacing_challenge, rope, context(agent_power(institutional), _, _, _)),
    format('Passed: Taxpayer (Snare) vs. Industry (Rope) gap confirmed.~n').

test(analytical_classification_is_tangled_rope) :-
    constraint_indexing:constraint_classification(nds_2022_pacing_challenge, tangled_rope, context(agent_power(analytical), _, _, _)),
    format('Passed: Analytical classification correctly identifies Tangled Rope.~n').

test(tangled_rope_structural_gates_pass) :-
    narrative_ontology:constraint_beneficiary(nds_2022_pacing_challenge, _),
    narrative_ontology:constraint_victim(nds_2022_pacing_challenge, _),
    domain_priors:requires_active_enforcement(nds_2022_pacing_challenge),
    format('Passed: All three structural requirements for Tangled Rope are met.~n').

:- end_tests(nds_2022_pacing_challenge_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   - Base Extractiveness (ε=0.48): This value is high, reflecting the enormous
 *     transfer of wealth (>$800B/year) from the public to the defense sector.
 *     However, it is kept below the pure Snare threshold (ε>0.5) to acknowledge
 *     the NDS's non-trivial coordination function in international relations
 *     and military planning.
 *   - Suppression Score (0.75): Extremely high. Taxpayers cannot opt out.
 *     Allies face immense diplomatic and security costs for non-alignment.
 *     Even the beneficiaries (defense firms) have few alternative customers
 *     on this scale, locking them into the system.
 *
 * PERSPECTIVAL GAP:
 *   The gap is profound. The US Taxpayer experiences the NDS as a Snare: a
 *   non-negotiable, massive financial drain. The defense industry sees it as a
 *   Rope: a beneficial coordination system that guarantees revenue and stability.
 *   The Analytical observer identifies it as a Tangled Rope, recognizing that
 *   the system uses immense, asymmetric extraction as the very mechanism by
 *   which it achieves its coordination goals.
 *
 * DIRECTIONALITY LOGIC:
 *   - Beneficiaries: The `us_defense_industrial_base` benefits directly from
 *     contracts. `us_dod_leadership` benefits from budget allocation and a
 *     clear strategic mission. `us_allied_nations` are listed as beneficiaries
 *     due to the security umbrella, but this is complicated by coercive pressure.
 *   - Victims: `us_taxpayers` are the primary financial victims.
 *     `designated_rival_nations` are the primary geopolitical victims, as the
 *     constraint is explicitly designed to limit their power.
 *
 * INTER-INSTITUTIONAL DYNAMICS:
 *   The relationship with Allied Nations is a textbook case of inter-institutional
 *   complexity. They are not pure beneficiaries; they are also targets of
 *   coercive coordination. Their `organized` power and `constrained` exit
 *   options differentiate them from the `institutional`/`arbitrage` position
 *   of the defense industry. The directionality override for their perspective
 *   (d=0.6) reflects that they perceive more cost/coercion than pure benefit.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification avoids two common errors. It rejects a cynical "pure
 *   Snare" view that ignores the real geopolitical coordination function. It
 *   also rejects a naive "pure Rope" view that ignores the staggering and
 *   asymmetric costs. The Tangled Rope classification correctly identifies
 *   the structure as a system where coordination and extraction are functionally inseparable.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_nds_2022,
    'Is the primary function of the NDS geopolitical coordination (deterring China), or is it a domestic mechanism for sustaining the defense industrial base, with geopolitics as the justification?',
    'Historical analysis of a counterfactual (e.g., a non-peer competitor threat) or declassification of records showing the weighting of industrial base concerns vs. strategic imperatives in budget decisions.',
    'If primarily coordination, Tangled Rope is correct. If primarily industrial policy, it would be a Snare with a high theater_ratio, where the geopolitical narrative is the theatrical cover.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing, interval represents 2022-2032.
narrative_ontology:interval(nds_2022_pacing_challenge, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Required for high-extraction constraints (base_extractiveness > 0.46).
% Models a slight increase in both extraction and performative theater over
% the strategy's lifecycle as initial goals are met or modified.

% Theater ratio over time (triggers metric_substitution detection):
narrative_ontology:measurement(nds22_tr_t0, nds_2022_pacing_challenge, theater_ratio, 0, 0.15).
narrative_ontology:measurement(nds22_tr_t5, nds_2022_pacing_challenge, theater_ratio, 5, 0.18).
narrative_ontology:measurement(nds22_tr_t10, nds_2022_pacing_challenge, theater_ratio, 10, 0.20).

% Extraction over time (triggers extraction_accumulation detection):
narrative_ontology:measurement(nds22_ex_t0, nds_2022_pacing_challenge, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(nds22_ex_t5, nds_2022_pacing_challenge, base_extractiveness, 5, 0.47).
narrative_ontology:measurement(nds22_ex_t10, nds_2022_pacing_challenge, base_extractiveness, 10, 0.48).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type: primarily a resource allocation mechanism, directing
% national wealth towards specific geopolitical and industrial ends.
narrative_ontology:coordination_type(nds_2022_pacing_challenge, resource_allocation).

% Network relationships: The NDS has far-reaching effects on other policy domains.
narrative_ontology:affects_constraint(nds_2022_pacing_challenge, semiconductor_supply_chain_resilience).
narrative_ontology:affects_constraint(nds_2022_pacing_challenge, federal_budget_deficits).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% An override is used for Allied Nations. The automatic derivation based on them
% being a 'beneficiary' would yield a low d-value (d≈0.15-0.25). This fails to
% capture the significant coercive pressure they are under. The override sets
% d=0.6, reflecting a perspective that feels more of the cost and constraint
% than the benefit, placing them firmly in the Tangled Rope classification.
constraint_indexing:directionality_override(nds_2022_pacing_challenge, organized, 0.60).


/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */