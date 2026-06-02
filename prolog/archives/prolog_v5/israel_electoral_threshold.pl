% ============================================================================
% CONSTRAINT STORY: israel_electoral_threshold
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_israel_electoral_threshold, []).

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
 *   constraint_id: israel_electoral_threshold
 *   human_readable: The 3.25% Knesset Electoral Threshold
 *   domain: political/electoral_systems
 *
 * SUMMARY:
 *   Israel's 3.25% electoral threshold for Knesset representation creates a
 *   binary outcome: a party either gains seats or gains none, with no middle
 *   ground. Since the threshold was raised from 2% in 2014, it has become a
 *   flashpoint for questions about representation, coalition stability, and
 *   whether it serves genuine governance needs or functions as a tool for
 *   entrenching establishment parties. The threshold exhibits both
 *   coordination and extraction functions: it can simplify coalition
 *   arithmetic (rope-like), but it also suppresses minority and new-party
 *   representation (snare-like). The constraint's extractiveness has risen
 *   from 0.42 to 0.58 over the 2014-2024 interval, driven by demographic
 *   changes (growth of Arab-sector parties near the threshold, ideological
 *   fragmentation within the center), rising political polarization, and
 *   increased awareness of the threshold's gatekeeping effect. Theater ratio
 *   remains low (the mechanism is simple and transparent), indicating this is
 *   not a degraded institutional performance but rather a deliberately
 *   maintained structural barrier.
 *
 * KEY AGENTS:
 *   - Small Parties (2-4% support): Primary victims (powerless/trapped) — cannot gain representation; have no exit except consolidation that destroys autonomy
 *   - Minority Communities (Arab citizens, Russian immigrants): Primary victims (powerless/trapped) — dispersed support below threshold; consolidated representation impossible without losing distinct voice
 *   - Establishment Coalition (15%+ parties): Primary beneficiaries (institutional/arbitrage) — benefit from seat magnification and simplified coalition arithmetic; have strong exit options and set threshold policy
 *   - Mid-Tier Kingmaker Parties (4-8%): Secondary beneficiaries (organized/constrained) — benefit from magnified seats and coalition leverage; face coercion through coalition blackmail
 *   - Electoral Reform Coalition: Organized agents (organized/constrained) — advocate for threshold reduction; have agency through legal mechanisms but face institutional resistance
 *   - Knesset Elections Committee: Administrative actor (institutional/arbitrage) — enforces threshold through routine; maintains it despite functional atrophy relative to original justification
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing a contingent policy choice (the specific 3.25% value) as an inevitable property of democratic systems
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(israel_electoral_threshold, 0.58).
domain_priors:suppression_score(israel_electoral_threshold, 0.72).
domain_priors:theater_ratio(israel_electoral_threshold, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(israel_electoral_threshold, extractiveness, 0.58).
narrative_ontology:constraint_metric(israel_electoral_threshold, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(israel_electoral_threshold, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(israel_electoral_threshold, snare).
narrative_ontology:human_readable(israel_electoral_threshold, "The 3.25% Knesset Electoral Threshold").
narrative_ontology:topic_domain(israel_electoral_threshold, "political/electoral_systems").

domain_priors:requires_active_enforcement(israel_electoral_threshold).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(israel_electoral_threshold, establishment_parties).
narrative_ontology:constraint_beneficiary(israel_electoral_threshold, coalition_brokers).
narrative_ontology:constraint_victim(israel_electoral_threshold, small_parties).
narrative_ontology:constraint_victim(israel_electoral_threshold, minority_representation).
narrative_ontology:constraint_victim(israel_electoral_threshold, political_newcomers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SMALL PARTY FACING THRESHOLD (SNARE) — A political movement with 2.8% support cannot gain representation regardless of voter preference. The party has no exit: voters cannot switch parties without consolidation, and consolidation destroys the movement's independence. The threshold extracts the party's electoral legitimacy and redistributes seats to larger parties. Zero agency; full suppression.
constraint_indexing:constraint_classification(israel_electoral_threshold, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: MINORITY CONSTITUENCY WITH FRAGMENTED SUPPORT (SNARE) — Communities with dispersed political preferences (Arab citizens, recent immigrants, ideological minorities) may aggregate above 3.25% collectively but cannot coordinate below the threshold without losing distinct representation. If fragmented support totals 8%, but it distributes as 2.5% + 2.2% + 2.8% across three parties, all three fail the threshold and that 8% of votes yields zero seats. Trapped: no exit without consolidation that destroys autonomy.
constraint_indexing:constraint_classification(israel_electoral_threshold, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 3: MID-TIER PARTY ABOVE THRESHOLD (TANGLED ROPE) — A party with 6% support clears the threshold easily but is constrained by coalition dynamics — the threshold concentrates bargaining power in the hands of kingmaker parties that can tip coalitions. The mid-tier party benefits from the threshold (it guarantees representation and magnifies seat share due to redistribution of sub-threshold votes) but faces extraction through coalition blackmail (partner demands disproportionate concessions). Mixed coordination (guaranteed representation) and extraction (coalition coercion).
constraint_indexing:constraint_classification(israel_electoral_threshold, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: LARGE ESTABLISHMENT PARTY (ROPE) — A party with 15%+ support experiences the threshold as a coordination mechanism that simplifies coalition formation. The large party benefits from seat magnification (sub-threshold votes redistributed to it), has strong exit options (can form coalitions without kingmakers), and faces minimal suppression. The threshold serves its coordination interests by eliminating noise from ultra-small parties and creating bargaining simplicity.
constraint_indexing:constraint_classification(israel_electoral_threshold, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: ELECTORAL REFORM MOVEMENT (SCAFFOLD) — Civil society and proportional representation advocates (e.g., Israel Democracy Institute, electoral reform NGOs) see the threshold as a temporary governance expedient with a sunset clause embedded in the political system itself. They experience suppression (the threshold is enforced) but have agency through legal and legislative mechanisms to change it. Their perspective suggests the threshold is performing a temporary function (stabilizing coalition arithmetic) that will eventually be replaced by more proportional or mixed-member systems as consensus builds. High suppression but not maximal — organized agents with exit paths (legislative lobbying, constitutional change).
constraint_indexing:constraint_classification(israel_electoral_threshold, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: ELECTORAL ADMINISTRATION (PITON) — The Knesset Elections Committee and state machinery maintain the threshold through administrative routine and statutory obligation. They experience it as a degraded institutional mechanism — the threshold is justified on grounds that once worked (simplifying coalition-building when party systems were more stable) but now persists primarily through institutional inertia. The administration has arbitrage options (legislative change), but the threshold persists because replacement mechanisms haven't been fully developed and stakeholders haven't reached consensus. Theater ratio is low (the mechanism is straightforward), but function has atrophied relative to its original justification.
constraint_indexing:constraint_classification(israel_electoral_threshold, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, some threshold is a mathematical necessity in any finite legislative system: if you have 120 seats and need to apportion votes proportionally, you must exclude parties below some minimum percentage to avoid fractional seats and infinite fragmentation. A threshold of 0% would require infinite seat divisions; a threshold of 50% would require super-majority governance. The 3.25% threshold might appear as an inevitable feature of representative democracy. However, the structural data contradicts this naturalization — alternative systems (proportional with higher fragmentation tolerance, mixed-member, cumulative voting) demonstrate that the specific 3.25% value is contingent policy choice, not immutable law.
constraint_indexing:constraint_classification(israel_electoral_threshold, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(israel_electoral_threshold_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(israel_electoral_threshold, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(israel_electoral_threshold, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(israel_electoral_threshold, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(israel_electoral_threshold, TR),
    TR >= 0.70.

:- end_tests(israel_electoral_threshold_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. The threshold extracts representation from sub-threshold voters (5-8% of the electorate in recent elections have votes that yield zero seats) and redistributes it to larger parties. This is not total extraction because the threshold also performs a genuine coordination function — it prevents extreme fragmentation and simplifies coalition-building. The rise from 0.42 (2014) to 0.58 (2024) reflects increased political fragmentation and the emergence of parties systematically near but below the threshold, making the extraction effect more visible. Suppression (0.72): High. The threshold is enforced through statutory law with no exceptions. Parties have no legal recourse and cannot compete below the threshold. Voters who prefer sub-threshold parties cannot express that preference without 'wasting' their vote. However, suppression is not absolute — parties can organize legislative campaigns to change the threshold (recent legislative attempts), and voters can organize coalitions to surpass it. Theater ratio (0.35): Low. The mechanism is transparent and straightforward — vote counts are public, the arithmetic is simple, and the results are determinate. The threshold is not justified through elaborate performative claims but through explicit statistical and coalition-stability arguments. Claimed type: Snare, based on the primary experience of small-party victims facing trap-like constraints with high suppression and extraction.
 *
 * PERSPECTIVAL GAP:
 *   The establishment coalition (Rope perspective) experiences the threshold as a coordination mechanism that stabilizes coalition arithmetic and prevents excessive fragmentation. From their position, the threshold solves a genuine problem: without it, every election would produce 20+ parties, making coalition-building impossible. The small party (Snare perspective) experiences the same threshold as a barrier to representation: 2.8% of the vote yields zero seats while 3.3% yields 4 seats — the same party performance produces different outcomes based on electoral geometry. These are not different subjective evaluations of the same thing; they are genuinely different structural realities. The establishment party benefits from the threshold (its votes are magnified when sub-threshold votes are redistributed). The small party bears the full cost. The analytical observer risks saying 'all electoral systems need some threshold, so this is a natural law' — a false summit that naturalizes the specific 3.25% value as inevitable when alternative thresholds (2%, 5%, 1%) would produce different outcomes.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality (d) derives from the agent's structural position relative to extraction flow. Small parties with trapped exit options face maximum d (≈0.95) — they cannot escape the threshold without sacrificing party independence, yielding high f(d) ≈1.42 and high experienced extraction chi. Establishment parties with arbitrage exit options face low d (≈0.15) — they can form coalitions without depending on the threshold and can change it through legislation, yielding low f(d) ≈-0.01 and negative experienced extraction chi. Mid-tier kingmakers face moderate d (≈0.65) — they benefit from the threshold's seat magnification but are constrained by coalition blackmail, yielding moderate f(d) ≈1.00. Analytical observers face high d (≈0.73) at the civilizational scope, reflecting their detached position but global scope, yielding f(d) ≈1.15. The directionality chain drives the perspectival gap: small parties see snare; establishment sees rope; analytical observer risks seeing mountain (inevitable), but structural data contradicts this.
 *
 * MANDATROPHY ANALYSIS:
 *   The threshold resolves mandatrophy by clarifying that it serves BOTH coordination AND extraction simultaneously. The coordination function is genuine: lower thresholds do produce more fragmentation and more complex coalition-building (empirically documented in comparative systems). The extraction function is also genuine: the threshold suppresses representation of minority movements with real political support. The mandatrophy is not 'is this Rope or Snare?' but 'how much of each?' Extractiveness 0.58 indicates the extraction component is substantial but not total. The snare classification follows because, from the powerless agent's perspective, suppression (0.72) dominates — they have no exit and cannot escape the mechanism. From the establishment perspective, the rope classification follows because they benefit from the coordination function and have arbitrage exit options. The threshold is a genuine Tangled Rope at the aggregate level (mixed beneficiaries and victims, active enforcement) but appears as pure Snare from the small-party perspective (no mixed benefits, only extraction) and pure Rope from the establishment perspective (no extraction experienced, only coordination benefit).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    coalescence_vs_fragmentation_tradeoff,
    'What is the empirically optimal threshold value that balances coalition manageability against proportional representation of genuine political movements?',
    'Comparative electoral systems analysis; simulation of Israeli elections under alternative thresholds (2%, 3.25%, 5%); correlation of threshold level with coalition stability, legislative deadlock frequency, and voter satisfaction across democracies',
    'If optimal < 3.25%: current threshold is extractive rent-seeking by establishment. If optimal > 3.25%: threshold is justified coordination mechanism; victims are political artifacts without genuine support. If optimal ≈ 3.25%: threshold is well-calibrated but may have drifted due to demographic/political change.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coalescence_vs_fragmentation_tradeoff, empirical, 'Optimal threshold value balancing coalition stability vs representation').

omega_variable(
    subconsciousness_of_threshold_effect,
    'Do Israeli voters strategically abandon sub-threshold parties out of rational vote-maximization, or do they have genuine preference for those parties that the threshold suppresses?',
    'Exit polls and preference surveys asking voters whether they would have voted for sub-threshold parties absent the threshold; comparison of support levels before vs after threshold changes; behavioral economics analysis of strategic voting behavior',
    'If votes are strategic reactions to the threshold: extractiveness is high (genuine representation is suppressed). If votes are genuine preferences: parties below threshold are genuinely marginal; extraction is lower and victims are self-selected. If mixed: extractiveness depends on proportion of suppressed genuine support.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(subconsciousness_of_threshold_effect, empirical, 'Whether sub-threshold abandonment is strategic or reflects genuine marginal support').

omega_variable(
    coalition_dysfunction_counterfactual,
    'Would Israeli coalition formation be significantly more dysfunctional under a lower threshold (e.g., 2% or 1%), or would governance improve by including more authentic political voices?',
    'Historical analysis of coalition formation timelines and stability in years when fragmentation increased (e.g., post-2015); comparison with other democracies using lower thresholds (Netherlands 0.67%, Germany 5%, Australia 4%); simulation of coalition games under alternative thresholds',
    'If dysfunction increases sharply: threshold is justified coordination mechanism (classification shifts toward Rope from Establishment perspective). If governance improves: threshold is extractive rent-seeking (classification solidifies as Snare from Small Party perspective).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(coalition_dysfunction_counterfactual, empirical, 'Coalition dysfunction counterfactual under lower thresholds').

omega_variable(
    historical_intent_vs_current_effect,
    'Was the 3.25% threshold (raised from 2% in 2014) established to address a genuine coordination problem, or was it primarily a vehicle for eliminating political competitors to the ruling coalition?',
    'Analysis of legislative record from 2013-2014 threshold increase debate; expert interviews with Knesset members and electoral law scholars; comparison of timing with political party landscape changes and coalition composition shifts',
    'If genuinely coordination-motivated: threshold is Rope or Tangled Rope. If extraction-motivated: threshold is Snare. If mixed: the mandatrophy is real — the same statute performs both functions simultaneously.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(historical_intent_vs_current_effect, conceptual, 'Historical intent behind the 2014 threshold increase').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(israel_electoral_threshold, 2014, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(israel_threshold_tr_t0, israel_electoral_threshold, theater_ratio, 0, 0.28).
narrative_ontology:measurement(israel_threshold_tr_t5, israel_electoral_threshold, theater_ratio, 5, 0.31).
narrative_ontology:measurement(israel_threshold_tr_t10, israel_electoral_threshold, theater_ratio, 10, 0.35).

% Extraction over time
narrative_ontology:measurement(israel_threshold_be_t0, israel_electoral_threshold, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(israel_threshold_be_t5, israel_electoral_threshold, base_extractiveness, 5, 0.52).
narrative_ontology:measurement(israel_threshold_be_t10, israel_electoral_threshold, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(israel_electoral_threshold, enforcement_mechanism).
narrative_ontology:affects_constraint(israel_electoral_threshold, coalition_formation_bargaining).
narrative_ontology:affects_constraint(israel_electoral_threshold, minority_representation_deficit).
narrative_ontology:affects_constraint(israel_electoral_threshold, electoral_system_fragmentation).

% DUAL FORMULATION NOTE:
% The 3.25% threshold is downstream of the structural need for coalition stability in multi-party systems but represents a distinct constraint on representation. The upstream constraint (coalition_formation_bargaining) has its own extractiveness reflecting the complexity of forming viable coalitions under proportional systems; the threshold constraint has its own extractiveness (0.58) reflecting the specific suppression of sub-threshold votes. These are linked by network causality: the threshold exists to solve coalition problems, but by solving those problems through vote suppression rather than incentive-compatible mechanisms, it creates its own extraction effects.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(israel_electoral_threshold, institutional, 0.12).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
