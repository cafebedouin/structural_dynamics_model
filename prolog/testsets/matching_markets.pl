% ============================================================================
% CONSTRAINT STORY: matching_markets
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_matching_markets, []).

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
 *   constraint_id: matching_markets
 *   human_readable: Matching Market Congestion Externality
 *   domain: economic/market_design
 *
 * SUMMARY:
 *   Matching markets (ride-sharing, online dating, job boards, freelance
 *   platforms) create a structural externality: increased participation
 *   improves the matching pool initially, but beyond an optimal density,
 *   congestion reduces individual match probability and quality for all
 *   participants, particularly those entering later. This constraint exhibits
 *   the full range of DR classifications from different structural positions.
 *   The platform operator experiences coordination benefits and arbitrage
 *   options. Early participants capture information asymmetry advantage. Late
 *   entrants face suppressed matching probability with limited exit options.
 *   Regulators can impose transparency requirements that create sunset
 *   dynamics. The academic matching theory provides institutional
 *   justification through algorithmic stability proofs that become
 *   increasingly theatrical in congested regimes. The analytical observer
 *   risks naturalizing congestion as an inherent matching problem, when it is
 *   actually contingent on platform architecture (matching frequency,
 *   information disclosure, queue design). The extractiveness trajectory
 *   shows congestion accumulating over time: initially minimal (0.15) in
 *   sparse markets, growing to moderate (0.38) as density increases. The
 *   theater ratio remains low (0.35) because the matching function retains
 *   genuine coordination value even in congested states — unlike purely
 *   performative constraints, algorithmic matching continues to produce real
 *   matches.
 *
 * KEY AGENTS:
 *   - Platform Operator: Institutional beneficiary (institutional/arbitrage) — captures network externalities, transaction volume, and competitive moat as participation scales
 *   - Early Participants: Primary beneficiary (moderate/mobile) — enjoy information asymmetry advantage and higher match rates before congestion dominates
 *   - Late Entrants: Primary victim (powerless/trapped) — face degraded matching probability, suppressed choice quality, reduced outside options; trapped by employment/relationship market constraints
 *   - Market Efficiency: Diffuse victim (analytical/analytical) — aggregate welfare decreases as congestion externality reduces total matching surplus
 *   - Regulatory Authority: Organized actor (organized/constrained) — can impose disclosure requirements (transparency, queue data, algorithm auditability) to enable informed exit decisions
 *   - Matching Theory Community: Institutional actor (institutional/arbitrage) — perpetuates algorithmic stability frameworks that address theoretical stability but not empirical congestion outcomes
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(matching_markets, 0.38).
domain_priors:suppression_score(matching_markets, 0.42).
domain_priors:theater_ratio(matching_markets, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(matching_markets, extractiveness, 0.38).
narrative_ontology:constraint_metric(matching_markets, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(matching_markets, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(matching_markets, tangled_rope).
narrative_ontology:human_readable(matching_markets, "Matching Market Congestion Externality").
narrative_ontology:topic_domain(matching_markets, "economic/market_design").

domain_priors:requires_active_enforcement(matching_markets).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(matching_markets, platform_operator).
narrative_ontology:constraint_beneficiary(matching_markets, early_participants).
narrative_ontology:constraint_victim(matching_markets, late_entrants).
narrative_ontology:constraint_victim(matching_markets, market_efficiency).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: LATE-ARRIVING GIG WORKER (SNARE) — Enters a congested market with deteriorating match quality. Cannot exit without accepting employment elsewhere or withdrawing from the gig economy. Faces suppressed matching probability and reduced wage options. Trapped in a degraded equilibrium created by prior entrants. Maximum extraction from structural position.
constraint_indexing:constraint_classification(matching_markets, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: PLATFORM OPERATOR (ROPE) — Benefits from network externalities and scaling: more participants generate more transaction volume and data. Experiences congestion as a coordination problem manageable through algorithmic matching improvements. Has arbitrage options (geographic expansion, service line expansion, algorithmic tuning). Net beneficiary from the constraint structure.
constraint_indexing:constraint_classification(matching_markets, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: EARLY-STAGE PARTICIPANT (TANGLED ROPE) — Captured mixed benefits and costs. Early arrival grants matching advantage (coordination benefit), but congestion accumulation eventually degrades their outcomes (extraction cost). Can switch platforms or exit (mobile), but only at switching cost. Experiences the constraint as both enabling and constraining.
constraint_indexing:constraint_classification(matching_markets, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(regional))).

% PERSPECTIVE 4: REGULATORY AUTHORITY (SCAFFOLD) — Can impose disclosure requirements (match rates, queue times, algorithm transparency) that enable participants to make exit decisions. Temporary intervention architecture: as information transparency improves and alternative platforms compete, the congestion penalty becomes less extractive because participants can actively optimize their participation. Sunset clause embedded in information-driven market correction.
constraint_indexing:constraint_classification(matching_markets, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: ACADEMIC MATCHING THEORY (PITON) — Stable matching algorithms (Gale-Shapley) are applied as if they solve the matching problem, but in congested markets with incomplete information and dynamic entry, algorithmic stability becomes largely theatrical. The theory persists as the institutional justification for market design despite low functional verification in high-congestion regimes. Theater ratio high because algorithm validation focuses on theoretical stability properties, not empirical matching outcomes.
constraint_indexing:constraint_classification(matching_markets, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, information asymmetry in congested matching markets is inherent to the problem structure: participants cannot observe all alternative matches simultaneously, creating an irreducible friction that generates congestion. However, the structural data contradicts the mountain classification — the engine will identify this as a false summit, revealing that congestion externality is contingent on platform architecture choices (matching frequency, information disclosure, queue transparency) rather than an immutable law.
constraint_indexing:constraint_classification(matching_markets, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(matching_markets_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(matching_markets, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(matching_markets, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(matching_markets, TR),
    TR >= 0.70.

:- end_tests(matching_markets_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate-high. The constraint exhibits genuine extraction — late entrants' matching probability is suppressed by prior entrants' presence, creating an asymmetric cost distribution. However, extractiveness is not maximal (0.46+) because the constraint generates real coordination value (matching is still occurring) and because participants retain some options (geographic mobility, service switching, market exit). The trajectory from 0.15 to 0.38 reflects accumulating congestion: sparse markets have minimal externality; dense markets exhibit significant externality. Suppression (0.42): Moderate. Late entrants face substantial barriers to exit (employment necessity, relationship market constraints), but suppression is not extreme because alternative platforms exist (albeit at switching cost) and geographic arbitrage is sometimes available. Suppression increases as platform network effects concentrate market share. Theater ratio (0.35): Low. The constraint retains genuine functional value — algorithmic matching continues to produce real matches even in congestion. Theater increases only when matching metrics become performative (reporting match rates without accounting for acceptance/completion rates, or matching queue times without transparency). The low initial theater reflects that the constraint is primarily a coordination problem; theater would increase if platforms began using algorithmic opacity to hide congestion dynamics.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates stark perspectival divergence. The platform operator sees a coordination mechanism with scaling benefits (Rope) — more participants improve matching opportunities for all. Early participants see a mixed coordination-extraction system (Tangled Rope) — they benefited from early entry but increasingly experience congestion costs. Late entrants see pure extraction (Snare) — suppressed matching probability with no exit. The regulatory observer sees a temporary problem with a transparency-driven sunset (Scaffold) — disclosure of congestion metrics enables informed entry decisions and supports platform competition. The matching theory community sees an algorithmic solution (Piton) — stable matching proofs persist despite low explanatory power for congestion outcomes, justified by institutional inertia. The civilizational observer risks naturalizing congestion as inherent to matching (Mountain false summit) — but the structural data reveals that congestion severity is contingent on platform architecture: matching frequency, information disclosure, queue transparency, and algorithmic transparency all modulate congestion externality.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality varies sharply by structural position. Late entrants (powerless/trapped) have d ≈ 0.95 — they bear maximum extraction with no exit, yielding high f(d) ≈ 1.42. Early participants (moderate/mobile) have d ≈ 0.45 — they have benefited from the coordination function but face accumulating extraction, with ability to switch platforms; d ≈ 0.45 yields f(d) ≈ 0.60. Platform operators (institutional/arbitrage) have d ≈ 0.05 — they are net beneficiaries with full exit optionality (geographic expansion, service innovation); d ≈ 0.05 yields f(d) ≈ -0.12. The scope modifier σ(S) applies: national scope (σ=1.0) for regional job/dating markets; global scope (σ=1.2) for international ride-sharing and freelance platforms, amplifying effective extraction where platforms operate at global scale. The chi formula produces: late_entrant_chi ≈ 0.38 × 1.42 × 1.0 ≈ 0.54 (severe extraction); early_participant_chi ≈ 0.38 × 0.60 × 1.0 ≈ 0.23 (moderate); platform_operator_chi ≈ 0.38 × (-0.12) × 1.0 ≈ -0.05 (net benefit).
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint demonstrates mandatrophy resolution through inter-institutional perspectival decomposition. The confusion is between (1) the coordination function of matching (genuine, persistent, creates real value), and (2) the extraction externality of congestion (contingent on architecture, not inherent to matching). Snare classification for late entrants is correct: they experience suppressed matching with trapped exit. Rope classification for platform operator is correct: they experience coordination benefits. Tangled Rope for early participants is correct: they experience both coordination benefits and extraction externality. The mandatrophy is resolved by recognizing that all four classifications are simultaneously true — they are not contradictory readings of the same structural position, but rather consistent readings of different structural positions within the same market. The false summit (mountain view) is correctly identified by the engine: congestion appears inherent only from the analytical observer who has not situated themselves in any agent's actual constraints. The regulatory scaffold perspective is not aspirational — it is the actual structural path: as information transparency improves (through regulatory disclosure, third-party auditing, platform competition), participant decision-making becomes more informed, late entrants can better assess entry timing, and platform operators face pressure to optimize for match quality rather than volume, reducing the congestion externality.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    congestion_threshold_dynamics,
    'At what participation density does matching probability collapse from coordination benefit to extraction externality?',
    'Empirical measurement of match rates across participation densities in multiple platforms; identification of inflection points where additional participation decreases individual match probability',
    'If threshold is low and sharp: congestion externality is severe and early-arriving participants capture disproportionate value (Snare confirmed for latecomers). If threshold is high and gradual: externality is diffuse and many participants benefit from scale (Rope from more perspectives).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(congestion_threshold_dynamics, empirical, 'Participation density at which congestion externality becomes dominant').

omega_variable(
    algorithmic_mitigation_effectiveness,
    'Can matching algorithms (machine learning, preference prediction, dynamic matching frequencies) substantially reduce congestion externality without introducing new extraction mechanisms?',
    'Comparative analysis of match rates pre/post algorithmic improvement; identification of whether algorithmic opacity creates new asymmetries or enforcement costs',
    'If effective and transparent: congestion becomes a solvable coordination problem (Rope classification strengthens). If effective but opaque: congestion is replaced by algorithmic extraction (Snare from algorithmic opacity perspective). If ineffective: externality is structural (Mountain false summit).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(algorithmic_mitigation_effectiveness, empirical, 'Whether algorithmic improvements can resolve congestion without new extraction').

omega_variable(
    platform_incentive_alignment,
    'Do platform operators have economic incentives to reduce congestion externality or to maintain it (congestion increases transaction volume and data collection)?',
    'Analysis of platform behavior: pricing policies, queue management, algorithmic prioritization of volume vs match quality; comparison of platforms with different ownership structures (cooperative vs for-profit)',
    'If operators benefit from congestion: constraint is actively enforced extraction (Snare/Tangled Rope). If operators bear costs of congestion: constraint is an unintended coordination problem (Rope). If mixed: directionality differs by platform business model.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(platform_incentive_alignment, empirical, 'Whether platform economic incentives align with reducing congestion').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(matching_markets, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mmce_tr_t0, matching_markets, theater_ratio, 0, 0.2).
narrative_ontology:measurement(mmce_tr_t3, matching_markets, theater_ratio, 3, 0.28).
narrative_ontology:measurement(mmce_tr_t6, matching_markets, theater_ratio, 6, 0.35).

% Extraction over time
narrative_ontology:measurement(mmce_be_t0, matching_markets, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(mmce_be_t3, matching_markets, base_extractiveness, 3, 0.28).
narrative_ontology:measurement(mmce_be_t6, matching_markets, base_extractiveness, 6, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(matching_markets, resource_allocation).
narrative_ontology:affects_constraint(matching_markets, platform_search_cost_asymmetry).
narrative_ontology:affects_constraint(matching_markets, information_disclosure_gaming).

% DUAL FORMULATION NOTE:
% The matching market congestion externality decomposes into two structurally distinct claims: (1) the coordination function of matching (low extractiveness, persistent), and (2) the congestion-driven externality on late entrants (moderate-high extractiveness, contingent on platform architecture). This story models the hybrid system. Downstream constraints address search cost asymmetries (how platforms distribute matching costs) and information disclosure gaming (how platforms present congestion metrics to participants).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(matching_markets, organized, 0.48).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
