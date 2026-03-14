% ============================================================================
% CONSTRAINT STORY: coalition_formation_bargaining
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_coalition_formation_bargaining, []).

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
 *   constraint_id: coalition_formation_bargaining
 *   human_readable: Coalition Formation Bargaining Constraint
 *   domain: political_economy/collective_action
 *
 * SUMMARY:
 *   Coalition formation in multi-party systems creates a structured
 *   bargaining problem where actors must coordinate to form winning
 *   coalitions while simultaneously extracting maximum gains from that
 *   coordination process. The constraint is the tension between the genuine
 *   coordination requirement (no single actor can govern alone) and the
 *   rent-seeking behavior that emerges in coalition assembly. This creates a
 *   hybrid mechanism: coalitions provide real coordination benefit to their
 *   members (they gain power and policy influence they would lack in
 *   opposition) while simultaneously concentrating extraction in the hands of
 *   pivotal or brokering actors who control the bargaining process. The same
 *   institutional structure that solves the collective action problem enables
 *   asymmetric distribution of its benefits. Theater ratio remains low (0.35)
 *   because coalition negotiations are substantive — the real distributional
 *   conflict is clearly visible and not masked by performative ritual.
 *   However, theater has increased over the measurement interval (0.28→0.35)
 *   as formal coalition documents have become more elaborate while informal
 *   side agreements determine actual power flow, indicating incipient piton
 *   dynamics.
 *
 * KEY AGENTS:
 *   - Pivotal Median Actor: Primary beneficiary (institutional/arbitrage) — controls decisive vote in coalition formation; extracts coordination rent through superior bargaining position
 *   - Coalition Broker / Leadership Council: Primary beneficiary (organized/constrained) — institutional actors who solve multi-party coordination problem and extract disproportionate gains through agenda control and selective enforcement
 *   - Excluded Minority Faction: Primary victim (powerless/trapped) — lacks resources or credibility to form alternative coalition; forced to accept unfavorable coalition terms or face isolation and worse outcomes
 *   - Rank-and-File Members: Secondary victim (moderate/constrained) — experience mixed coordination benefit and extraction; party discipline enforces inequality within coalition
 *   - Institutional Coalition Architecture: Degrading institutional mechanism (institutional/arbitrage) — formal rules increasingly performative; actual power determined by informal bargaining
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(coalition_formation_bargaining, 0.52).
domain_priors:suppression_score(coalition_formation_bargaining, 0.48).
domain_priors:theater_ratio(coalition_formation_bargaining, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(coalition_formation_bargaining, extractiveness, 0.52).
narrative_ontology:constraint_metric(coalition_formation_bargaining, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(coalition_formation_bargaining, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(coalition_formation_bargaining, tangled_rope).
narrative_ontology:human_readable(coalition_formation_bargaining, "Coalition Formation Bargaining Constraint").
narrative_ontology:topic_domain(coalition_formation_bargaining, "political_economy/collective_action").

domain_priors:requires_active_enforcement(coalition_formation_bargaining).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(coalition_formation_bargaining, pivotal_median_actor).
narrative_ontology:constraint_beneficiary(coalition_formation_bargaining, coalition_broker).
narrative_ontology:constraint_victim(coalition_formation_bargaining, excluded_minority_factions).
narrative_ontology:constraint_victim(coalition_formation_bargaining, rank_and_file_members).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: EXCLUDED MINORITY FACTION (SNARE) — Structurally trapped in the coalition formation process. Cannot credibly threaten exit; lacks resources to form alternative coalition. Bears full weight of bargaining extraction — forced to accept unfavorable terms or face coalition dissolution and worse outcomes. No genuine alternatives; maximum suppression.
constraint_indexing:constraint_classification(coalition_formation_bargaining, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: RANK-AND-FILE MEMBERS (TANGLED ROPE) — Experience genuine coordination benefit (access to power-sharing, policy influence they would lack alone) alongside asymmetric extraction (leadership captures disproportionate gains, imposes party discipline). Can exit at significant cost (loss of access, reputational damage, career penalties in party structure). Mixed mechanism: coordination solves their collective action problem but extraction persists within the coalition.
constraint_indexing:constraint_classification(coalition_formation_bargaining, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: PIVOTAL MEDIAN ACTOR (ROPE) — Experiences coalition formation as pure coordination. This actor's support is arithmetically necessary; can arbitrage between competing coalitions. Extracts gains through bargaining but these are genuine coordination rent — necessary to incentivize participation from a critical actor. Theater minimal; function clear.
constraint_indexing:constraint_classification(coalition_formation_bargaining, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: COALITION BROKER / LEADERSHIP COUNCIL (TANGLED ROPE) — Organized institutional actors who coordinate the coalition AND extract rents through control of agenda, resource distribution, and enforcement. Genuine coordination function (solving multi-party bargaining problem) coexists with asymmetric extraction (disproportionate power concentration, veto authority, selective enforcement of coalition terms). Active enforcement required to maintain both the coordination and the extraction.
constraint_indexing:constraint_classification(coalition_formation_bargaining, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: INSTITUTIONAL COALITION ARCHITECTURE (PITON) — Coalition rules, protocols, and norms have become largely performative. Formal coalition agreements exist (theater) but actual power flow is determined by extra-institutional bargaining. The written coalition document persists through institutional inertia and provides legitimacy theater despite low functional role. Actors navigate around formal rules through informal side agreements.
constraint_indexing:constraint_classification(coalition_formation_bargaining, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW (MOUNTAIN) — From a universal game-theoretic perspective, coalition formation involves irreducible mathematical constraints: any coalition must satisfy feasibility conditions (winning threshold), and any internal division must satisfy stability conditions (no sub-coalition prefers defection). These constraints are inherent to multi-party bargaining and cannot be removed by institutional design. However, the structural data contradicts the mountain classification — the engine will flag this as a false summit, revealing naturalization of contingent institutional choices (coalition rules, veto structures, enforcement mechanisms) as inherent mathematical necessities.
constraint_indexing:constraint_classification(coalition_formation_bargaining, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(coalition_formation_bargaining_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(coalition_formation_bargaining, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(coalition_formation_bargaining, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(coalition_formation_bargaining, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(coalition_formation_bargaining, TR),
    TR >= 0.70.

:- end_tests(coalition_formation_bargaining_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. Coalition formation does extract value from some parties (excluded factions, rank-and-file), but the extraction is not total or uniform. Pivotal actors and brokers extract substantial gains, but these partly reflect genuine coordination rent — the cost of incentivizing critical actors to participate. Excluded factions bear concentrated extraction (inability to access coalition benefits) but the overall system extractiveness is tempered by the fact that included members do gain real policy and power benefits. Suppression (0.48): Moderate. Multiple suppression mechanisms operate: excluded factions cannot credibly threaten competing coalitions (structural), rank-and-file face party discipline and exit costs (institutional), and information about alternative possibilities is often controlled by brokers (epistemic). However, suppression is not total — actors retain some bargaining agency, and coalition terms can be renegotiated when circumstances change. Theater ratio (0.35): Low-moderate. Coalition bargaining is relatively substantive — the distributional conflict is clearly visible. However, formal coalition documents (theater) have increased while informal bargaining determines actual allocation, creating incipient piton dynamics. The constraint requires active enforcement: coalition terms must be maintained through continuous renegotiation, selective incentives for compliance, and sanctions for defection.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits a seven-way perspectival gap. The pivotal actor sees coordination (Rope). The brokers see mixed function (Tangled Rope). Rank-and-file see mixed function (Tangled Rope). Excluded factions see pure extraction (Snare). The institutional form appears degraded (Piton). The civilizational analytical observer risks naturalizing contingent institutional choices as mathematical necessity (Mountain). The gap is irreducible: each perspective captures a real structural feature. The constraint cannot be accurately described by a single type — the presheaf of perspectives IS the complete picture.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) for each perspective is computed from the agent's structural position. The pivotal median actor: beneficiary status + arbitrage exit → d ≈ 0.15 → f(d) ≈ -0.01, producing negative/minimal experienced extraction (benefits from constraint). The coalition broker: beneficiary status + constrained exit (cannot abandon brokerage role) + organized power → d ≈ 0.35 → f(d) ≈ 0.35, producing moderate-high experienced extraction despite being beneficiary (they extract but are also bound to the system). The rank-and-file: mixed victim/beneficiary + constrained exit → d ≈ 0.55 → f(d) ≈ 0.75, producing moderate-high experienced extraction. The excluded faction: victim status + trapped exit → d ≈ 0.92 → f(d) ≈ 1.38, producing maximum experienced extraction. Scope modifier σ(S) = 1.0 for national scope. Effective extraction χ = 0.52 × f(d) × 1.0 varies by perspective from -0.01 to 1.38, explaining the perspectival gap. The constraint's effective extractiveness is high for excluded factions and moderate for included members, but averaged across the system, the mixed benefit-cost profile produces moderate base extractiveness.
 *
 * MANDATROPHY ANALYSIS:
 *   Coalition formation bargaining resolves potential mandatrophy by distinguishing coordination (legitimate coalition assembly) from extraction (disproportionate distribution of coalition benefits). The constraint is genuinely a hybrid: the coordination component is real (excluded actors would be worse off without any coalition system) but extraction is also real (pivotal and brokering actors capture disproportionate gains). The Tangled Rope classification is appropriate for the beneficiary and moderate perspectives because both the coordination and extraction mechanisms are observable and necessary to explain the constraint's persistence. The Snare classification for excluded factions is not a mischaracterization — from their perspective, there is no coordination benefit, only extraction. The Rope classification for pivotal actors correctly identifies genuine coordination rent. The Piton classification at civilizational scale correctly identifies that formal coalition structures are increasingly performative while informal bargaining determines outcomes. The Mountain classification at analytical scale is a false summit — it naturalizes institutional choices (coalition rules, broker authority, veto structures) as mathematical necessity. The mandatrophy is resolved not by choosing one type but by recognizing that all six types are legitimate perspectival readings reflecting different structural positions relative to the same constraint mechanism.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    extraction_vs_pivotal_rent,
    'Is the compensation paid to pivotal actors genuine coordination rent or extractive overhead?',
    'Counterfactual analysis: compare pivotal actor compensation to the difference between coalition value with and without that actor; test whether other actors benefit from coalition despite paying the pivotal actor',
    'If genuine rent: classification remains Tangled Rope with mixed function. If pure extraction: reclassify as Snare for excluded factions.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraction_vs_pivotal_rent, empirical, 'Whether pivotal actor compensation represents coordination rent or extraction').

omega_variable(
    threshold_credibility,
    'Do excluded factions genuinely lack capacity to form competing coalitions or is their exclusion a function of institutional barriers?',
    'Historical analysis of coalition formation attempts; measurement of resource asymmetry; comparison to counterfactual institutional designs with lower barriers',
    'If genuine capacity lacks: trapped classification accurate. If barriers are institutional: reclassify trapped→constrained; extraction partially institutional rather than structural.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(threshold_credibility, empirical, 'Whether exclusion reflects genuine incapacity or institutional barriers').

omega_variable(
    informal_enforcement_mechanism,
    'How much of the coalition extraction is actively enforced through visible sanctions vs passively maintained through exit costs and uncertainty?',
    'Ethnographic analysis of coalition discipline mechanisms; measurement of sanction frequency and severity; comparison of enforcement patterns across different coalition types',
    'If heavily active: requires_active_enforcement justified, Tangled Rope confirmed. If passive: theater_ratio may be understated; suppression may be overestimated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(informal_enforcement_mechanism, empirical, 'Degree of active vs passive enforcement of coalition discipline').

omega_variable(
    information_asymmetry_binding,
    'To what extent does coalition extraction persist because excluded factions lack information about alternative coalition possibilities vs structural unavailability of alternatives?',
    'Comparison of coalition outcomes when information is symmetric vs asymmetric; analysis of counterfactual coalition structures if all actors had complete information',
    'If information-dependent: suppression value overstated; with better information, trapped→constrained or constrained→mobile. If structural: classification robust.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(information_asymmetry_binding, empirical, 'Whether extraction depends on information asymmetry or structural constraints').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(coalition_formation_bargaining, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cfb_tr_t0, coalition_formation_bargaining, theater_ratio, 0, 0.28).
narrative_ontology:measurement(cfb_tr_t3, coalition_formation_bargaining, theater_ratio, 3, 0.32).
narrative_ontology:measurement(cfb_tr_t6, coalition_formation_bargaining, theater_ratio, 6, 0.35).

% Extraction over time
narrative_ontology:measurement(cfb_be_t0, coalition_formation_bargaining, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(cfb_be_t3, coalition_formation_bargaining, base_extractiveness, 3, 0.45).
narrative_ontology:measurement(cfb_be_t6, coalition_formation_bargaining, base_extractiveness, 6, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(coalition_formation_bargaining, resource_allocation).
narrative_ontology:affects_constraint(coalition_formation_bargaining, veto_player_power).
narrative_ontology:affects_constraint(coalition_formation_bargaining, coalition_stability_mechanisms).
narrative_ontology:affects_constraint(coalition_formation_bargaining, party_discipline_enforcement).

% DUAL FORMULATION NOTE:
% Coalition formation bargaining is a family constraint decomposable into three distinct structural problems: the game-theoretic coalition assembly problem (mathematical constraint), the institutional rules governing coalition formation (policy choice), and the informal bargaining process determining actual distribution (political economy). Each has different ε and classification. This story models the hybrid (institutional + political economy) constraint. The pure mathematical constraint would constitute a separate mountain-type story; the formal rules would constitute a separate piton-type story.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(coalition_formation_bargaining, institutional, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
