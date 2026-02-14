% ============================================================================
% CONSTRAINT STORY: taiwan_ids_program
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-05-18
% ============================================================================

:- module(constraint_taiwan_ids_program, []).

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
    domain_priors:emerges_naturally/1.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: taiwan_ids_program
 *   human_readable: Taiwan's Indigenous Defense Submarine (IDS) Program
 *   domain: geopolitical/technological
 *
 * SUMMARY:
 *   Faced with sustained geopolitical pressure from the People's Republic of
 *   China (PRC) that effectively blockades the international sale of submarines
 *   to Taiwan, the Taiwanese state initiated the Indigenous Defense Submarine
 *   (IDS) program. This multi-billion dollar project coordinates domestic industry
 *   and foreign technical assistance to build a fleet of modern submarines.
 *   The constraint is the program itself: a high-cost, high-risk, but necessary
 *   coordination mechanism born from the suppression of alternatives.
 *
 * KEY AGENTS (by structural relationship):
 *   - Taiwanese Taxpayers: Primary target (powerless/trapped) — bear the immense financial cost of the program.
 *   - Taiwanese State & Military (MND): Primary beneficiary (institutional/constrained) — gains a critical defense capability, but is locked into this path due to external pressure.
 *   - Taiwanese Defense Industry (CSBC Corp): Secondary beneficiary (organized/mobile) — receives massive contracts, investment, and technology transfer.
 *   - People's Republic of China (PRC): Inter-institutional actor (institutional/arbitrage) — created the underlying condition (the blockade) and is the strategic target of the program's output.
 *   - Analytical Observer: Sees the full structure of necessary coordination and asymmetric extraction.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(taiwan_ids_program, 0.55).
domain_priors:suppression_score(taiwan_ids_program, 0.75).   % Structural property (raw, unscaled). High, as the program exists because alternatives are suppressed.
domain_priors:theater_ratio(taiwan_ids_program, 0.15).       % Piton detection (>= 0.70). Low, this is a highly functional project.

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(taiwan_ids_program, extractiveness, 0.55).
narrative_ontology:constraint_metric(taiwan_ids_program, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(taiwan_ids_program, theater_ratio, 0.15).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(taiwan_ids_program, tangled_rope).

% --- Binary flags ---
domain_priors:requires_active_enforcement(taiwan_ids_program). % Required for Tangled Rope. The program is enforced via state budgets and contracts.

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% These feed the directionality derivation chain.
%
% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(taiwan_ids_program, taiwanese_state_and_military).
narrative_ontology:constraint_beneficiary(taiwan_ids_program, taiwanese_defense_industry).
%
% Who bears disproportionate cost?
narrative_ontology:constraint_victim(taiwan_ids_program, taiwanese_taxpayers).
narrative_ontology:constraint_victim(taiwan_ids_program, prc_strategic_position). % The program's success imposes a cost on the PRC's strategic goals.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × f(d) × σ(S)
   where f(d) is the sigmoid directionality function.
   The engine derives d from beneficiary/victim membership + exit_options.
   Scope modifiers: local=0.8, regional=0.9, national=1.0, etc.
   CONTEXT ARITY: All context() terms must have exactly 4 arguments.
   ========================================================================== */

% PERSPECTIVE 1: THE TAIWANESE TAXPAYER (SNARE)
% Bears the full financial extraction without direct benefit. Engine derives
% d from victim status + trapped exit -> d ≈ 0.95 -> f(d) ≈ 1.42 -> very high χ.
constraint_indexing:constraint_classification(taiwan_ids_program, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE TAIWANESE DEFENSE INDUSTRY (ROPE)
% Receives massive capital and technology flows. Engine derives d from
% beneficiary status + mobile exit -> d ≈ 0.15 -> f(d) ≈ -0.01 -> low/negative χ.
constraint_indexing:constraint_classification(taiwan_ids_program, rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% Sees both the necessary coordination function and the high asymmetric extraction.
% Engine derives d ≈ 0.72 -> f(d) ≈ 1.15. The combination of high ε and high f(d)
% leads to a high χ, but the clear coordination function prevents a Snare classification.
constraint_indexing:constraint_classification(taiwan_ids_program, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% --- INTER-INSTITUTIONAL PERSPECTIVES ---

% PERSPECTIVE 4A: TAIWANESE STATE & MILITARY (TANGLED ROPE)
% A beneficiary, but with constrained options. They see the program as necessary
% coordination but are acutely aware of the immense cost and risk (extraction).
% Beneficiary + constrained exit -> d is higher than for an arbitrage beneficiary.
constraint_indexing:constraint_classification(taiwan_ids_program, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 4B: PEOPLE'S REPUBLIC OF CHINA (TANGLED ROPE)
% The PRC is a victim of the program's strategic success, but has arbitrage exit
% as they control the underlying pressure. They see it as a costly, problematic
% (for them) coordination effort by Taiwan, acknowledging its function but also
% its internal financial strain, which they may view as a secondary benefit.
constraint_indexing:constraint_classification(taiwan_ids_program, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(regional))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(taiwan_ids_program_tests).

test(perspectival_gap_taxpayer_vs_industry, [nondet]) :-
    % Verify the massive gap between the funder (taxpayer) and industrial beneficiary.
    constraint_indexing:constraint_classification(taiwan_ids_program, snare, context(agent_power(powerless), _, exit_options(trapped), _)),
    constraint_indexing:constraint_classification(taiwan_ids_program, rope, context(agent_power(organized), _, exit_options(mobile), _)).

test(analytical_classification_is_tangled_rope) :-
    constraint_indexing:constraint_classification(taiwan_ids_program, tangled_rope, context(agent_power(analytical), _, _, _)).

test(tangled_rope_gate_requirements_met) :-
    % A Tangled Rope requires a beneficiary, a victim, and active enforcement.
    narrative_ontology:constraint_beneficiary(taiwan_ids_program, _),
    narrative_ontology:constraint_victim(taiwan_ids_program, _),
    domain_priors:requires_active_enforcement(taiwan_ids_program).

:- end_tests(taiwan_ids_program_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   - Base Extractiveness (ε=0.55): The program costs billions, a significant diversion of public funds compared to a hypothetical scenario of buying cheaper, proven foreign submarines. This cost represents the extraction.
 *   - Suppression (0.75): The entire program's existence is predicated on the high suppression of alternatives by the PRC's diplomatic and military pressure. Taiwan cannot simply buy subs on the open market.
 *   - Theater (0.15): The program is highly functional, aimed at producing a credible military asset. The successful sea trial of the "Narwhal" demonstrates its low theatrical ratio.
 *   These metrics, combined with the clear presence of both a coordination function and asymmetric extraction, make Tangled Rope the correct analytical classification.
 *
 * PERSPECTIVAL GAP:
 *   The gap is extreme. For the Taiwanese taxpayer, it's a Snare: a massive, unavoidable financial drain with an abstract, long-term security benefit. For the domestic defense industry (CSBC), it's a pure Rope: a perfectly coordinated injection of capital, jobs, and technology. The Taiwanese state itself experiences it as a Tangled Rope, seeing both the necessary coordination and the painful extractive cost.
 *
 * DIRECTIONALITY LOGIC:
 *   - Beneficiaries: The Taiwanese state (gains security) and its defense industry (gains revenue/capability).
 *   - Victims: The Taiwanese taxpayers (foot the bill) and, strategically, the PRC (whose A2/AD strategy is weakened by the program's success).
 *   These structural roles directly inform the directionality `d` calculated by the engine. A taxpayer is a victim with `trapped` exit (high `d`), while the defense industry is a beneficiary with `mobile` exit (low `d`), creating the massive gap in perceived classification.
 *
 * INTER-INSTITUTIONAL DYNAMICS:
 *   The constraint is experienced differently by the two primary state actors. The Taiwanese state is a beneficiary, but with `constrained` exit options, forcing it to accept the high extraction. The PRC is a victim of the program's function, but has `arbitrage` exit over the underlying pressure, giving it strategic flexibility. This demonstrates how institutional actors can have opposing relationships to the same constraint.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification correctly avoids two errors. It is not a pure Snare, because it serves a genuine, critical coordination function (national defense). It is not a pure Rope, because the cost is immense and borne asymmetrically. The Tangled Rope classification captures this dual nature: a necessary act of coordination that is simultaneously highly extractive due to the suppressed alternatives.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    omega_taiwan_ids_program,
    'Will the IDS fleet provide a credible asymmetric deterrent that alters the PRC''s strategic calculus, or will it be a financially ruinous project with minimal strategic impact?',
    'Observation of fleet deployment, operational readiness, and PRC strategic response over the next 10-15 years.',
    'If deterrent is credible, the high extraction is justified (Tangled Rope). If not, the program was a costly Piton or Snare from the start.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(taiwan_ids_program, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Required for high-extraction constraints (base_extractiveness > 0.46).
% The program's costs became more concrete over time. Theater remained low.
%
% Theater ratio over time:
narrative_ontology:measurement(taiwan_ids_program_tr_t0, taiwan_ids_program, theater_ratio, 0, 0.10).
narrative_ontology:measurement(taiwan_ids_program_tr_t5, taiwan_ids_program, theater_ratio, 5, 0.12).
narrative_ontology:measurement(taiwan_ids_program_tr_t10, taiwan_ids_program, theater_ratio, 10, 0.15).

% Extraction over time:
narrative_ontology:measurement(taiwan_ids_program_ex_t0, taiwan_ids_program, base_extractiveness, 0, 0.50).
narrative_ontology:measurement(taiwan_ids_program_ex_t5, taiwan_ids_program, base_extractiveness, 5, 0.53).
narrative_ontology:measurement(taiwan_ids_program_ex_t10, taiwan_ids_program, base_extractiveness, 10, 0.55).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% This is a massive state-led resource allocation project.
narrative_ontology:coordination_type(taiwan_ids_program, resource_allocation).

% Network relationships (structural influence edges)
% The IDS program is a direct response to, and exists within the context of,
% the PRC's broader Anti-Access/Area Denial (A2/AD) strategy.
narrative_ontology:affects_constraint(china_a2ad_strategy, taiwan_ids_program).


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% No overrides are needed for this story. The standard derivation from
% beneficiary/victim declarations and exit options accurately captures the
% structural relationships and perspectival gaps. The difference between a
% 'constrained' beneficiary (Taiwan MND) and a 'mobile' beneficiary (CSBC)
% is handled correctly by the core derivation engine.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */