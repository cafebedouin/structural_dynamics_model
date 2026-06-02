% ============================================================================
% CONSTRAINT STORY: committee_tyranny_of_minority
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_committee_tyranny_of_minority, []).

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
    constraint_indexing:directionality_override/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: committee_tyranny_of_minority
 *   human_readable: Committee Tyranny of Minority: Extractive Coordination in Collective Decision-Making
 *   domain: governance/organizational_decision_making
 *
 * SUMMARY:
 *   Committee tyranny of minority occurs when formal decision-making
 *   structures (committees, boards, councils, assemblies) grant veto or
 *   supermajority-blocking power to minority coalitions, enabling asymmetric
 *   extraction: the minority extracts concessions or prevents
 *   majority-preferred outcomes while the majority cannot reciprocate. This
 *   constraint is characterized by a performative gap — committees present
 *   themselves as deliberative bodies aggregating diverse preferences, but
 *   the minority veto mechanism ensures outcomes reflect organized
 *   obstruction rather than aggregate preference. The constraint exhibits
 *   genuine coordination function (structured deliberation, preference
 *   aggregation infrastructure) alongside extractive asymmetry (minority can
 *   block majority-preferred outcomes without cost to themselves). Theater
 *   ratio has increased over time (from 0.38 to 0.61) as organizational
 *   complexity has grown, pushing more decisions through committee processes
 *   while decision quality and responsiveness have declined. The constraint
 *   is pervasive across institutional contexts: legislative committees,
 *   corporate boards, university governance, labor unions, professional
 *   societies, and nonprofit organizations all exhibit this pattern when
 *   minority coalition power exceeds their preference intensity.
 *
 * KEY AGENTS:
 *   - Majority Preference Holders: Primary victims (powerless/trapped) — lack exit options from committee structure; suppressed through procedural rules requiring supermajority or unanimous support
 *   - Minority Coalition Members: Primary beneficiaries (organized/arbitrage) — coordinate internally to block majority preferences; have exit/threat options; experience the constraint as pure coordination mechanism
 *   - Unaligned Committee Members: Secondary victims (moderate/constrained) — face pressure to join coalitions; experience mixed coordination and extraction
 *   - Organizational Leadership: Constrained institutional actor — benefits from committee coordination but constrained by minority veto power; cannot implement strategic decisions without minority concurrence
 *   - Formal Democratic Procedure: Institutional ritual (institutional/arbitrage) — maintains itself through theater; primary coordination function has degraded as organizational complexity increased
 *   - Structural Reform Movement: External powerful actor (powerful/mobile) — sees minority tyranny as temporary problem with sunset; building alternative decision-making pathways
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(committee_tyranny_of_minority, 0.52).
domain_priors:suppression_score(committee_tyranny_of_minority, 0.68).
domain_priors:theater_ratio(committee_tyranny_of_minority, 0.61).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(committee_tyranny_of_minority, extractiveness, 0.52).
narrative_ontology:constraint_metric(committee_tyranny_of_minority, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(committee_tyranny_of_minority, theater_ratio, 0.61).

% --- Constraint claim ---
narrative_ontology:constraint_claim(committee_tyranny_of_minority, tangled_rope).
narrative_ontology:human_readable(committee_tyranny_of_minority, "Committee Tyranny of Minority: Extractive Coordination in Collective Decision-Making").
narrative_ontology:topic_domain(committee_tyranny_of_minority, "governance/organizational_decision_making").

domain_priors:requires_active_enforcement(committee_tyranny_of_minority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(committee_tyranny_of_minority, minority_coalition_members).
narrative_ontology:constraint_beneficiary(committee_tyranny_of_minority, agenda_gatekeepers).
narrative_ontology:constraint_victim(committee_tyranny_of_minority, majority_preference_holders).
narrative_ontology:constraint_victim(committee_tyranny_of_minority, organizational_consensus_building).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: MAJORITY PREFERENCE HOLDER (SNARE) — Trapped within the committee structure. Cannot exit without abandoning organizational participation. Suppressed through procedural rules (filibuster, unanimous consent requirements, veto power, endless amendments) that require exhausting supermajority support to overcome minority obstruction. Experiences pure extraction: blocked from achieving preferred outcomes while bearing opportunity costs of continued deliberation.
constraint_indexing:constraint_classification(committee_tyranny_of_minority, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: MINORITY COALITION (ROPE) — Organized actors who coordinate internally to block or extract concessions from the majority. Experience the constraint as pure coordination: their ability to function as a bloc solves the collective action problem of aligning minority preferences. They have arbitrage options — can threaten exit or non-cooperation, can offer support to external actors. Low experienced extraction because they control the extraction mechanism.
constraint_indexing:constraint_classification(committee_tyranny_of_minority, rope,
    context(agent_power(organized),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(local))).

% PERSPECTIVE 3: UNALIGNED COMMITTEE MEMBER (TANGLED ROPE) — Individual with neither majority nor minority bloc affiliation. Experiences genuine coordination: the committee process aggregates preferences and requires deliberation. But also experiences asymmetric extraction: unaligned members face pressure to join coalitions or accept suboptimal outcomes. Can exit committee but faces career and organizational costs (constrained). Mixed function — real coordination but embedded extraction.
constraint_indexing:constraint_classification(committee_tyranny_of_minority, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(local))).

% PERSPECTIVE 4: ORGANIZATIONAL LEADERSHIP (TANGLED ROPE) — Institutional actors who benefit from committee structure as a coordination mechanism for distributed decision-making. But constrained by minority veto power — leadership cannot implement strategic decisions without minority concurrence or elaborate workarounds. Genuine coordination function (distributes deliberation cost) alongside asymmetric extraction (minority can extract concessions by blocking). Cannot fully exit the committee structure without abandoning institutional legitimacy.
constraint_indexing:constraint_classification(committee_tyranny_of_minority, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 5: FORMAL DEMOCRATIC PROCEDURE RITUAL (PITON) — The committee structure maintains itself through procedural theater: the ritual of deliberation, floor time, amendment cycles, and voting preserves the appearance of democratic legitimacy even as the minority veto mechanism ensures substantive outcomes reflect organized obstruction rather than aggregate preference. Theater ratio is high (0.61) — committees spend substantial time on procedures that perform democracy rather than enable it. The primary function (aggregating preferences via structured deliberation) has degraded as organizational complexity increased; the ritual persists through inertia.
constraint_indexing:constraint_classification(committee_tyranny_of_minority, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a logical/mathematical perspective, this perspective claims that collective decision-making under restricted access (committees, small deliberative bodies) faces inherent contradictions: Condorcet's paradox (preference cycling), Arrow's impossibility theorem, and other voting paradoxes establish that no procedure simultaneously satisfies basic rationality axioms and aggregates diverse preferences. From this view, minority tyranny is not a contingent institutional feature but a necessary consequence of applying consistent aggregation rules. However, the structural data (high suppression, enforced minority coordination, artifactual theater) suggests the mountain is false: the tyranny is contingent institutional design, not logical necessity.
constraint_indexing:constraint_classification(committee_tyranny_of_minority, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 7: STRUCTURAL REFORM MOVEMENT (SCAFFOLD) — Powerful external actors (institutional reformers, parliamentary procedures overhaul movements, organizational governance redesigners) see minority tyranny as a temporary procedural problem with a sunset: alternative decision-making mechanisms (ranked choice voting, proportional representation, consensus-building protocols, sortition/citizen assemblies) are building parallel pathways that reduce minority veto power. Low extraction because these reformers have mobile exit options and see a time-bounded problem. Suppression declines as alternative procedures mature.
constraint_indexing:constraint_classification(committee_tyranny_of_minority, scaffold,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(committee_tyranny_of_minority_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(committee_tyranny_of_minority, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(committee_tyranny_of_minority, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(committee_tyranny_of_minority, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(committee_tyranny_of_minority, TR),
    TR >= 0.70.

:- end_tests(committee_tyranny_of_minority_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The minority coalition systematically blocks majority preferences, extracting concessions via threat of indefinite obstruction. The baseline extraction is not as severe as pure snare (which would require ε ≥ 0.66) because: (1) the minority coalition faces some costs to persistent obstruction (reputational, coalition cohesion), (2) the majority can occasionally overcome blocking through procedural exhaustion or external pressure, and (3) some decisions do proceed despite minority opposition. Suppression (0.68): High. The majority faces significant barriers to achieving preferred outcomes: procedural rules requiring supermajority or unanimous consent, extended deliberation cycles allowing repeated amendment and re-voting, threat of indefinite delay, institutional culture normalizing minority accommodation. However, suppression is not total (0.95+) because exit options exist (though constrained) and some majorities can mobilize sufficient political pressure to overcome blocking. Theater ratio (0.61): Moderate-high. Committees spend substantial time on procedural performance: floor time allocation, amendment protocols, voting rituals, deliberation cycles that perform democracy without enabling it. But the constraint has genuine coordination content (aggregating preferences, distributing deliberation cost) — theater is not near-total (≥0.90). The ratio has increased over time as organizational complexity has grown: larger committees with greater diversity of preference have longer deliberation cycles and higher theatrical content.
 *
 * PERSPECTIVAL GAP:
 *   The snare perspective (majority holder) and rope perspective (minority coalition) are genuinely inversed: the same constraint structure appears as pure extraction to one agent and pure coordination to another. This is not a measurement artifact but a structural feature of asymmetric veto power. The tangled_rope perspective (leadership) is intermediate: they benefit from committee coordination but are suppressed by minority veto. The piton perspective reveals that procedural theater has increased (0.38 → 0.61) as the constraint has aged — the original coordination function (distributed deliberation, preference aggregation) persists in form but is increasingly hollowed out. The scaffold perspective projects that alternative procedures (sortition, ranked choice, consensus protocols) will reduce minority veto power by shifting the aggregation mechanism — but this is conditional on implementation feasibility (omega variable). The mountain perspective (voting paradox naturalization) represents a false summit: Arrow's impossibility theorem applies to the aggregation rule space, not to the historical fact that this specific committee structure exhibits minority veto extraction. Many decision structures do not exhibit this tyranny (autocracy, randomized allocation, rotating coalitions) — the tyranny is contingent on the choice of procedure.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) derives from each agent's structural relationship to the extraction flow. The majority preference holder (victim status + trapped exit) derives high d (~0.95) → high f(d) → experiences maximum effective extraction. The minority coalition member (beneficiary status + arbitrage exit) derives low d (~0.10) → negative f(d) → experiences negative effective extraction (they are the beneficiaries). The unaligned member (mixed status + constrained exit) derives moderate d (~0.60) → moderate f(d) → moderate extraction. The organizational leadership (beneficiary from coordination, victim from veto constraint, constrained exit) derives moderate d (~0.55) → moderate f(d). The procedural ritual (institutional beneficiary + arbitrage) derives low d (~0.15) → negative f(d). The reformer (powerful external + mobile) derives moderate d (~0.50) → positive f(d) but low absolute magnitude because they are not embedded in the constraint. The chi formula χ = ε × f(d) × σ(S) produces: majority holders experiencing χ ≈ 0.52 × 1.42 × 0.80 ≈ 0.59 (snare threshold); minority members experiencing χ ≈ 0.52 × (-0.12) × 0.80 ≈ -0.05 (rope/beneficiary); leadership experiencing χ ≈ 0.52 × 0.75 × 0.8 ≈ 0.31 (moderate extraction, consistent with tangled_rope).
 *
 * MANDATROPHY ANALYSIS:
 *   DIAGNOSTIC EXEMPLAR: This constraint resolves mandatrophy by illustrating how institutional procedure choices create extractive classification outcomes that appear to be logical necessities. The mountain perspective (voting paradoxes force minorities to block) naturalizes a contingent procedure choice (supermajority/veto rules) as a logical law. The snare, rope, and tangled_rope perspectives (minority extraction, minority coordination, mixed) are all correct structural descriptions of the same institutional arrangement — they differ because the agents occupy different structural positions relative to the blocking power. The piton perspective (procedural theater) correctly identifies that the original coordination value has degraded as the constraint has aged — committees still deliberate, but deliberation increasingly produces theater rather than responsive outcomes. The scaffold perspective (sunset through alternative procedures) is conditional on whether those alternatives can be implemented — if they can, the minority tyranny is not a law of decision-making but a policy choice. The analytical observer should not assume the mountain represents logical necessity — instead recognize that the mountain is a common cover story for institutional choices that benefit minority coalitions. The true mandatrophy resolution is: procedure choices create classification outcomes; some procedures (supermajority, veto) create snare/piton; other procedures (median voter, random selection, consensus) would produce different classifications from the same agents. The tyranny is chosen, not necessary.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    minority_definition_threshold,
    'What percentage or coalition size constitutes ''minority'' for purposes of extractive veto power vs. legitimate blocking coalition?',
    'Empirical analysis of historical blocking coalitions; comparison of outcomes under different majority thresholds (50%, 55%, 60%, 75%); measurement of minority size vs. extraction magnitude',
    'If threshold < 30%: many legitimate preferences misclassified as extraction. If threshold > 50%: blocking power extends to near-majorities, diffusing extraction signal.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(minority_definition_threshold, empirical, 'Threshold for defining extractive minority vs. legitimate blocking coalition').

omega_variable(
    deliberation_function_vs_theater,
    'Does committee deliberation produce meaningfully better decisions than top-down allocation, or is it primarily theater masking the minority veto mechanism?',
    'Outcome analysis: compare committee decisions to hypothetical top-down allocations; measure information utilization (do committee discussions incorporate available expertise?) vs. procedural time (how much time spent on process vs. substance?); longitudinal tracking of decision quality metrics',
    'If deliberation is functional: snare classification may be incorrect — the constraint contains genuine coordination value. If primarily theater: snare and piton perspectives are validated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(deliberation_function_vs_theater, empirical, 'Whether committee deliberation produces substantive value vs. procedural theater').

omega_variable(
    exit_option_feasibility,
    'Can majority preference holders realistically exit the committee structure without organizational costs, or is exit suppressed by institutional culture?',
    'Case studies of exit attempts; measurement of career penalties and organizational retaliation for committees members who advocate non-participation; comparison of exit costs across organizational types (voluntary vs. mandatory participation)',
    'If exit is feasible: majority holders are constrained rather than trapped — reclassify from snare to tangled_rope. If exit is heavily penalized: trap classification confirmed.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(exit_option_feasibility, empirical, 'Whether majority can exit committee without organizational penalties').

omega_variable(
    minority_coalition_stability,
    'Is the blocking minority a stable coalition with durable preferences, or do minority positions rotate across issues?',
    'Issue-by-issue tracking of committee voting patterns; identification of stable blocking coalitions vs. shifting issue-based alliances; measurement of minority bloc cohesion across decision domains',
    'If stable minority: extraction mechanism is consistent. If rotating minorities: tyranny may be symmetric across all members at different times — reclassify as tangled_rope for all.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(minority_coalition_stability, empirical, 'Whether blocking minority is stable or rotates across issues').

omega_variable(
    alternative_aggregation_feasibility,
    'Are alternative decision-making procedures (sortition, liquid democracy, ranked choice, consensus-building) actually implementable as successors to committee veto systems, or do they have equally severe failure modes?',
    'Implementation case studies from organizations that have adopted alternatives; measurement of extractiveness under new procedures; identification of failure modes in alternative systems',
    'If alternatives prove workable: scaffold perspective is validated and sunset timeline is realistic. If alternatives show equivalent or worse extraction: scaffold is aspirational and the constraint may be structural to decision-making itself.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_aggregation_feasibility, empirical, 'Whether alternative aggregation procedures provide genuine exit from minority veto').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(committee_tyranny_of_minority, 0, 9).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ctom_tr_t0, committee_tyranny_of_minority, theater_ratio, 0, 0.38).
narrative_ontology:measurement(ctom_tr_t3, committee_tyranny_of_minority, theater_ratio, 3, 0.48).
narrative_ontology:measurement(ctom_tr_t6, committee_tyranny_of_minority, theater_ratio, 6, 0.61).
narrative_ontology:measurement(ctom_tr_t9, committee_tyranny_of_minority, theater_ratio, 9, 0.61).

% Extraction over time
narrative_ontology:measurement(ctom_be_t0, committee_tyranny_of_minority, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(ctom_be_t3, committee_tyranny_of_minority, base_extractiveness, 3, 0.42).
narrative_ontology:measurement(ctom_be_t6, committee_tyranny_of_minority, base_extractiveness, 6, 0.52).
narrative_ontology:measurement(ctom_be_t9, committee_tyranny_of_minority, base_extractiveness, 9, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(committee_tyranny_of_minority, enforcement_mechanism).
narrative_ontology:affects_constraint(committee_tyranny_of_minority, legislative_gridlock).
narrative_ontology:affects_constraint(committee_tyranny_of_minority, corporate_board_veto_dynamics).
narrative_ontology:affects_constraint(committee_tyranny_of_minority, professional_credentialing_gatekeeping).

% DUAL FORMULATION NOTE:
% Committee tyranny of minority is structurally distinct from but causally related to legislative gridlock (which operates at state level with constitutional veto mechanisms) and corporate board dynamics (which operate at organizational level with shareholder voting rules). Each constraint has its own extractiveness value reflecting domain-specific blocking mechanisms and exit options. The network relationship indicates that committee tyranny shares the minority-veto extraction mechanism across these domains.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(committee_tyranny_of_minority, institutional, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
