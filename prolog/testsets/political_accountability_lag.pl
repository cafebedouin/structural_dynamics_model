% ============================================================================
% CONSTRAINT STORY: political_accountability_lag
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_political_accountability_lag, []).

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
 *   constraint_id: political_accountability_lag
 *   human_readable: Political Accountability Lag in Democratic Systems
 *   domain: political_economy/governance
 *
 * SUMMARY:
 *   Political accountability lag describes the structural gap between when
 *   elected officials take policy action and when voters can observe
 *   consequences and execute electoral correction. This constraint operates
 *   in all democratic systems but varies significantly in magnitude across
 *   institutional designs. The lag creates a temporal arbitrage opportunity:
 *   incumbents benefit from credit-claiming for inherited benefits while
 *   deferring blame for policy failures beyond the current electoral cycle.
 *   The constraint exhibits a sharp perspectival divergence reflecting
 *   competing structural positions. Incumbent political leadership
 *   experiences it as neutral coordination (electoral cycles enable
 *   synchronized action). Voters trapped in non-competitive districts
 *   experience it as pure extraction (no exit, no intermediate
 *   accountability). Organized reform movements see it as a solvable problem
 *   (alternative institutional designs reduce the lag). The constraint's
 *   extractiveness has increased over recent decades as political
 *   polarization has reduced swing-voter agency, and information technologies
 *   have made real-time accountability mechanically feasible, widening the
 *   gap between what is possible and what institutional design permits.
 *
 * KEY AGENTS:
 *   - Incumbent Political Leadership: Primary beneficiary (institutional/arbitrage) — captures agenda-setting advantage and can arbitrage between inherited and self-generated outcomes
 *   - Electorate (Powerless Subset): Primary victim (powerless/trapped) — voters in non-swing districts or entrenched political contexts with zero electoral leverage; face maximal suppression and extraction
 *   - Electorate (Swing Voters): Secondary victim (moderate/constrained) — mobile voters in competitive districts; participate in genuine coordination but face asymmetric information
 *   - Bureaucratic Institutions: Secondary beneficiary (institutional/constrained) — benefit from accountability gaps that reduce political interference during implementation windows; face pressure during accountability moments
 *   - Institutional Reform Movement: Organized agents (organized/constrained) — transparency advocates, electoral reformers, democratic accountability movements building alternative institutional pathways
 *   - Constitutional Electoral Framework: Structural actor (institutional/arbitrage) — 4-6 year cycles, separation of powers, fixed legislative calendars maintained through constitutional inertia
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing contingent electoral design as inherent to democratic governance
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(political_accountability_lag, 0.58).
domain_priors:suppression_score(political_accountability_lag, 0.62).
domain_priors:theater_ratio(political_accountability_lag, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(political_accountability_lag, extractiveness, 0.58).
narrative_ontology:constraint_metric(political_accountability_lag, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(political_accountability_lag, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(political_accountability_lag, tangled_rope).
narrative_ontology:human_readable(political_accountability_lag, "Political Accountability Lag in Democratic Systems").
narrative_ontology:topic_domain(political_accountability_lag, "political_economy/governance").

domain_priors:requires_active_enforcement(political_accountability_lag).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(political_accountability_lag, incumbent_political_actors).
narrative_ontology:constraint_beneficiary(political_accountability_lag, bureaucratic_institutions).
narrative_ontology:constraint_victim(political_accountability_lag, electorate_temporal_leverage).
narrative_ontology:constraint_victim(political_accountability_lag, policy_correction_velocity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DISENFRANCHISED VOTER (SNARE) — Voters in non-swing districts, jurisdictions with entrenched machines, or polarized contexts face maximal extraction with minimal exit. Cannot credibly threaten to leave; electoral choice is between predetermined outcomes. Bears full cost of policy failure with 4-6 year delay before any correction mechanism activates. No intermediate accountability; no exit option except geographic relocation.
constraint_indexing:constraint_classification(political_accountability_lag, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: SWING VOTER (TANGLED ROPE) — Participates in genuine electoral coordination (the mechanism that selects leaders) but faces asymmetric information lag: cannot observe policy effects until long after electoral commitment. Benefits from competitive elections (coordination function) but bears extraction through delayed accountability. Can exit via political disengagement but at cost of surrendering influence.
constraint_indexing:constraint_classification(political_accountability_lag, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: INCUMBENT POLITICAL LEADERSHIP (ROPE) — Experiences the constraint as pure coordination: electoral timetables, campaign cycles, and legislative calendars enable synchronized political action. The lag between action and accountability is a neutral institutional framework. Benefits from first-mover advantage and agenda control. Can arbitrage: claim credit for prior administration's benefits, defer blame for inherited problems.
constraint_indexing:constraint_classification(political_accountability_lag, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: INSTITUTIONAL REFORM MOVEMENT (SCAFFOLD) — Organized actors (transparency advocates, ranked-choice-voting coalitions, real-time accountability mechanisms) see accountability lag as a solvable coordination failure. Exit mechanism exists via institutional design: more frequent elections, citizen assemblies, continuous performance monitoring, instant-runoff voting reduce the lag. Temporal sunset: as democratic norms mature toward more granular accountability, traditional 4-6 year cycle loses extraction power.
constraint_indexing:constraint_classification(political_accountability_lag, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: CONSTITUTIONAL ELECTORAL FRAMEWORK (PITON) — Fixed election cycles (e.g., 4-year presidential terms, fixed legislative sessions) are treated as immutable constitutional structures, but their functional role has atrophied. Modern communications technology makes real-time accountability mechanically possible; the fixed cycle persists through institutional inertia rather than necessity. Theater ratio reflects that constitutional 'checks and balances' are substantially performative — checks occur only during narrow windows and are often ineffectual. The mechanism endures because constitutional change is hard, not because it optimally coordinates accountability.
constraint_indexing:constraint_classification(political_accountability_lag, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, some lag between action and accountability is inherent to governance: information propagation, causal measurement, and collective decision-making take time. No political system can eliminate the gap between policy implementation and observable effects. However, the structural data contradicts the mountain classification — modern democracies with real-time transparency, continuous polling, and immediate feedback mechanisms demonstrate that the lag is not a natural law but a contingent institutional design. The false summit reveals naturalizing rhetorical moves.
constraint_indexing:constraint_classification(political_accountability_lag, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(political_accountability_lag_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(political_accountability_lag, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(political_accountability_lag, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(political_accountability_lag, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(political_accountability_lag, TR),
    TR >= 0.70.

:- end_tests(political_accountability_lag_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high, reflecting the substantial but not maximal extraction from delayed accountability. The value increased from 0.35 (when information technology made real-time feedback less available) to 0.58 as modern communication technology revealed the lag as a design choice rather than a necessity. Suppression (0.62): Moderate-high. Significant barriers to intermediate accountability include constitutional constraints on executive removal, supermajority requirements for legislative reversal, and psychological sunk-cost effects that trap voters in failing policies. But suppression is not total — swing voters have demonstrable exit (they can vote out incumbents), and organized movements have been able to implement some accountability mechanisms (ballot initiatives, recall provisions in some jurisdictions). Theater ratio (0.68): Moderate-high. Electoral rituals — debates, campaign messaging, promise-making — are substantially performative. The actual mechanisms of accountability are often ineffectual or captured: legislative investigations are partisan theater, budget reconciliation is opaque, and blame-shifting is systematized. Real-time feedback mechanisms (polling, transparency reporting, town halls) would have lower theater because they skip the ritual validation layer.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates how the same structural phenomenon — the 4-6 year gap between policy action and electoral accountability — is experienced as coordination (incumbent leadership), extraction (trapped voters), mixed extraction-coordination (swing voters), temporary failure with solvable sunset (reform movements), and degraded ritual (constitutional framework). The gap is not perspectival illusion; it is a real structural feature that different agents experience differently based on their power, exit options, and position in the extraction flow. The false summit at the analytical/civilizational level reveals the naturalization move: 'All democracies have accountability lag' does not mean the lag is inherent to democracy — it means that incumbent-serving institutional designs are universal. The existence of alternative designs (recall provisions, continuous polling, real-time transparency) demonstrates the lag's contingency.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality derives from the agent's structural position relative to the accountability flow. Incumbent leadership benefits from the lag (low d) — they can claim credit early and defer blame late. Powerless voters are targeted by the lag (high d) — they cannot credibly threaten exit and bear costs without recourse. Swing voters are partly beneficiary (they coordinate electoral outcomes) and partly victim (information lag constrains their knowledge) — moderate d. Bureaucratic institutions benefit from implementation windows free of political interference but face intense scrutiny during accountability moments — moderate d reflecting the temporal pattern. The Scaffold perspective (organized reformers) has lower experienced extraction than the Snare perspective (trapped voters) because the reformers have agency, organizational capacity, and viable exit paths (institutional design change). The piton classification for the constitutional framework reflects that the mechanism persists through constitutional inertia rather than functional necessity — the theater ratio is high (electoral processes are performative) even though suppression is genuine (constitutional structures are hard to change).
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by showing that accountability lag is correctly classified as Tangled Rope from the system-wide analytical perspective: (1) genuine coordination function — elections do select leaders, and some electoral cycle is required to aggregate voter preferences; (2) asymmetric extraction — the lag disproportionately benefits incumbents and harms voters who cannot observe policy effects; (3) active enforcement — constitutional constraints maintain the lag despite technological feasibility of reducing it. The mountain perspective (analytical/civilizational) is a false summit that naturalizes institutional design as natural law. The trap perspective (powerless/trapped) is genuinely experienced as Snare because the victim has no exit. The beneficiary perspective (incumbent/arbitrage) is genuinely experienced as Rope because the benefit is experienced as neutral coordination. All perspectives are structurally correct given their positions; the system-wide classification is Tangled Rope because it captures both the coordination function (elections are real) and the asymmetric extraction (lag benefits incumbents at voter expense).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    information_vs_causal_lag,
    'How much of the accountability lag is information delay (voters don''t know policy outcomes) vs. causal delay (policy outcomes take time to materialize)?',
    'Comparison of accountability response times for (a) policies with immediate measurable effects (unemployment insurance, stimulus checks) vs. (b) policies with delayed effects (education investment, infrastructure); measurement of voter knowledge gaps via surveys before and after information interventions',
    'If predominantly information lag: transparency and real-time reporting can reduce extractiveness substantially (Scaffold perspective is correct). If predominantly causal lag: institutional design changes cannot compress the accountability timeline (Mountain perspective has validity).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(information_vs_causal_lag, empirical, 'Information lag vs. causal lag in accountability response').

omega_variable(
    electoral_cycle_optimality,
    'Are fixed electoral cycles (4-6 years) optimal for accountability, or do they primarily serve incumbent re-election incentives?',
    'Comparative institutional analysis: democracies with variable election timing vs. fixed cycles; measurement of policy volatility and incumbent advantage across different electoral schedules; temporal correlation between electoral cycle length and rent-extraction rates',
    'If optimal: Piton classification is wrong (cycle is functional, not degraded). If primarily incumbent-serving: Piton is correct, and Scaffold''s exit via more frequent accountability is viable.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(electoral_cycle_optimality, empirical, 'Whether fixed electoral cycles optimize accountability or primarily serve incumbents').

omega_variable(
    delegation_incompleteness,
    'Can accountability be compressed without delegating authority away from elected representatives? Or does reducing the lag fundamentally change the delegation relationship?',
    'Analysis of real-time accountability mechanisms (citizen assemblies, instant-recall provisions, continuous polling) and their impact on representative authority; measurement of legislative autonomy erosion as feedback frequency increases',
    'If delegated authority is preserved: Scaffold can reduce extraction without institutional collapse. If delegation is fundamentally altered: faster accountability requires redistributing power from representatives to constituencies (systemic change).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(delegation_incompleteness, conceptual, 'Whether accountability acceleration preserves or erodes representative delegation').

omega_variable(
    voter_rationality_and_attribution,
    'Can voters attribute policy outcomes to correct causes across the accountability lag, or does the lag enable systematic misattribution?',
    'Experimental manipulation of lag length and causal complexity; measurement of voter misattribution rates as lag increases; analysis of how incumbents strategically deploy credit-claiming and blame-shifting rhetoric during gaps',
    'If attribution is robust: voter feedback is relatively accurate even with lag (tangled rope extraction is not severe). If lag enables systematic misattribution: extraction mechanism is amplified (snare perspective is correct).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(voter_rationality_and_attribution, empirical, 'Voter causal attribution accuracy across accountability lags').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(political_accountability_lag, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(acctlag_tr_t0, political_accountability_lag, theater_ratio, 0, 0.5).
narrative_ontology:measurement(acctlag_tr_t2, political_accountability_lag, theater_ratio, 2, 0.62).
narrative_ontology:measurement(acctlag_tr_t4, political_accountability_lag, theater_ratio, 4, 0.68).
narrative_ontology:measurement(acctlag_tr_t6, political_accountability_lag, theater_ratio, 6, 0.64).

% Extraction over time
narrative_ontology:measurement(acctlag_be_t0, political_accountability_lag, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(acctlag_be_t2, political_accountability_lag, base_extractiveness, 2, 0.45).
narrative_ontology:measurement(acctlag_be_t4, political_accountability_lag, base_extractiveness, 4, 0.58).
narrative_ontology:measurement(acctlag_be_t6, political_accountability_lag, base_extractiveness, 6, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(political_accountability_lag, enforcement_mechanism).
narrative_ontology:affects_constraint(political_accountability_lag, regulatory_capture_lag).
narrative_ontology:affects_constraint(political_accountability_lag, fiscal_policy_business_cycle).
narrative_ontology:affects_constraint(political_accountability_lag, judicial_review_timeline).

% DUAL FORMULATION NOTE:
% Political accountability lag is a system-level constraint but can be decomposed into structurally distinct mechanisms: (1) information lag (voters don't know outcomes) — lower ε, solvable via transparency; (2) causal lag (outcomes take time to materialize) — higher ε, constrained by physics; (3) institutional lag (electoral cycles are fixed) — moderate ε, solvable via institutional design. This story models the integrated constraint; domain-specific applications may require decomposition.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
