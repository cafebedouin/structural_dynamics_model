% ============================================================================
% CONSTRAINT STORY: innovators_dilemma
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_innovators_dilemma, []).

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
 *   constraint_id: innovators_dilemma
 *   human_readable: The Innovator's Dilemma
 *   domain: economic/technological
 *
 * SUMMARY:
 *   The Innovator's Dilemma describes a structural constraint where incumbent
 *   firms, by rationally maximizing shareholder value through investment in
 *   high-margin products and responsiveness to their most profitable customer
 *   segments, create an organizational inability to respond competitively to
 *   disruptive technologies that begin in low-margin, underserved markets.
 *   This is not a failure of management competence or strategy — it is the
 *   rational output of profit-maximizing firms operating under standard
 *   capital allocation rules, customer feedback mechanisms, and quarterly
 *   earnings pressures. The constraint exhibits a perspectival range from
 *   pure coordination (incumbent's experience of listening to customers) to
 *   pure extraction (disruptor's experience of being locked out) to
 *   contingent temporary problems (venture capital's response) to degraded
 *   institutional patterns (vertical integration inertia) to false
 *   naturalizations (efficiency frame). The base extractiveness (0.52)
 *   reflects that the incumbent's rational behavior does extract from
 *   disruptors and low-margin segments, but it is not overwhelming
 *   suppression — disruptors can and do escape by acquiring capital, building
 *   scale, or moving into adjacent markets. The theater ratio (0.35) is low
 *   because the constraint operates through material incentive structures
 *   (quarterly earnings, customer profitability, capital allocation
 *   processes) rather than performative ritual. The constraint is primarily
 *   structural and functional, not theatrical.
 *
 * KEY AGENTS:
 *   - Incumbent Firm Management: Primary beneficiary (institutional/arbitrage) — maximizes shareholder value through rational capital allocation
 *   - High-Margin Customer Segment: Secondary beneficiary (moderate/arbitrage) — receives preferential innovation investment and responsiveness
 *   - Disruptive Innovators: Primary victim (powerless/trapped) — locked out of incumbent's distribution, capital, and R&D resources; forced to bootstrap in low-margin niches
 *   - Low-Margin Market Entrants: Secondary victim (moderate/constrained) — constrained by incumbent's scale advantages and network effects; benefit from any innovation that reaches them
 *   - Future Competitive Position: Structural victim (analytical/analytical) — incumbent's rationality today creates organizational rigidity that prevents response to tomorrow's competitive threats
 *   - Venture Capital Ecosystem: Organized mediator (organized/mobile) — funds disruptors precisely because incumbents under-allocate; sees constraint as temporary coordination problem with sunset
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(innovators_dilemma, 0.52).
domain_priors:suppression_score(innovators_dilemma, 0.48).
domain_priors:theater_ratio(innovators_dilemma, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(innovators_dilemma, extractiveness, 0.52).
narrative_ontology:constraint_metric(innovators_dilemma, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(innovators_dilemma, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(innovators_dilemma, tangled_rope).
narrative_ontology:human_readable(innovators_dilemma, "The Innovator's Dilemma").
narrative_ontology:topic_domain(innovators_dilemma, "economic/technological").

domain_priors:requires_active_enforcement(innovators_dilemma).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(innovators_dilemma, incumbent_firm_shareholders).
narrative_ontology:constraint_beneficiary(innovators_dilemma, high_margin_customer_segment).
narrative_ontology:constraint_victim(innovators_dilemma, disruptive_innovators).
narrative_ontology:constraint_victim(innovators_dilemma, low_margin_market_entrants).
narrative_ontology:constraint_victim(innovators_dilemma, future_competitive_position).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DISRUPTIVE INNOVATOR (SNARE) — Small entrant with minimal resources, trapped in low-margin niche. Incumbent's rational R&D allocation starves alternative pathways. No exit: must either succeed against structural disadvantage or fail. Faces suppression through incumbent's scale advantages, distribution networks, and customer lock-in. Maximum extraction experienced.
constraint_indexing:constraint_classification(innovators_dilemma, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: LOW-MARGIN CUSTOMER BASE (TANGLED ROPE) — Constraints are both coordinating (access to improving products) and extracting (locked into low-margin alternatives, forced to accept slower innovation in their segment). Trapped by resource scarcity but also benefit from any innovation that reaches them. Constrained exit: could theoretically switch but switching costs are high.
constraint_indexing:constraint_classification(innovators_dilemma, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: INCUMBENT FIRM MANAGEMENT (ROPE) — Rational coordination mechanism: listening to best customers, investing in high-margin products maximizes short-term shareholder value. Management experiences the constraint as sound business logic. High exit optionality: can reallocate capital, acquire disruptors, or pivot. Net beneficiary — extraction runs toward them.
constraint_indexing:constraint_classification(innovators_dilemma, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: VENTURE CAPITAL ECOSYSTEM (SCAFFOLD) — Structured to fund disruptive entrants precisely because incumbents under-allocate to low-margin markets. VC capital is a temporary coordination solution with built-in sunset: successful disruptors scale up and eventually become the new incumbent (facing the same dilemma), or they fail and capital redeploys. Low extraction from VC perspective because they have organizational exit and see structural purpose. Theater is low — capital allocation is functional, not performative.
constraint_indexing:constraint_classification(innovators_dilemma, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: VERTICAL INTEGRATION (PITON) — Incumbent firms are often bound into supply chain relationships, manufacturing commitments, and organizational structures optimized for high-margin product lines. These commitments persist through institutional inertia long after the strategic rationale has changed. Theater_ratio reflects that much 'commitment to customer' is ritual organizational behavior — stakeholder boards, customer advisory committees, market research ceremonies — that preserves the constraint through performance rather than functional necessity. The constraint decays as these relationships are unbundled.
constraint_indexing:constraint_classification(innovators_dilemma, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / EFFICIENCY VIEW (MOUNTAIN) — From a civilizational/universal analytical frame, the dilemma appears as a logical inevitability: firms maximize expected value given information and incentives, and rational maximization of present cash flow creates organizational rigidity toward future possibilities. This perspective sees the constraint as an immutable feature of capital allocation logic. However, the base properties reveal this is a false summit: the dilemma is not a law of economics but a contingent organizational and incentive structure that can be decomposed and redesigned (dual innovation processes, separate business units, venture arms).
constraint_indexing:constraint_classification(innovators_dilemma, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(innovators_dilemma_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(innovators_dilemma, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(innovators_dilemma, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(innovators_dilemma, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(innovators_dilemma, TR),
    TR >= 0.70.

:- end_tests(innovators_dilemma_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The incumbent's rational allocation of capital and attention to high-margin customers directly starves alternative pathways and forces disruptors into low-margin traps. However, this is not the maximum extraction (0.70+) because disruptors have partial escape routes: venture capital funding, international markets, adjacent segments, and eventual acquisition. The extraction is real but not total suppression. It increases over time (0.35 → 0.52) as incumbent's market dominance and customer lock-in deepen, making escape increasingly costly. Suppression (0.48): Moderate. Incumbents suppress disruptive pathways through scale advantages, distribution networks, and customer switching costs. However, suppression is not overwhelming — some disruptors do escape, venture capital can fund end-runs around incumbent advantages, and global markets provide geographic arbitrage. Theater ratio (0.35): Low. The constraint operates through functional material incentives (quarterly earnings, customer profitability metrics, capital allocation spreadsheets) rather than performative ritual. Management genuinely believes in listening to customers; the organizational processes that implement this belief are functional, not theatrical. As firms age and vertical integration deepens, theater does increase slightly (0.25 → 0.35) as legacy commitments require performative justification.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates fundamental disagreement about whether the dilemma is coordination or extraction. The incumbent sees rational coordination (Rope): they are solving the legitimate problem of allocating capital efficiently and responding to customer needs. The disruptive innovator sees pure extraction (Snare): they are locked out by incumbent's scale and have no exit. The VC ecosystem sees a temporary coordination failure (Scaffold): they can solve it by funding disruptors, but this solution has a built-in sunset as successful disruptors become new incumbents facing the same dilemma. The low-margin market segment sees mixed coordination and extraction (Tangled Rope): the incumbent's neglect of their segment is partially extractive (forcing them into inferior products) and partially coordinating (some innovation eventually reaches them). The efficiency observer risks seeing a natural law (Mountain): rational profit maximization necessarily produces this outcome — but the base properties reveal this is a false summit because the constraint can be decomposed through dual innovation processes, separate business units, and modified incentives.
 *
 * DIRECTIONALITY LOGIC:
 *   Each agent's experienced extractiveness (chi) derives from their structural position relative to capital and customer attention flows. Incumbent management benefits from the constraint (d ≈ 0.1, low extraction) because they are rationally maximizing their objective function. High-margin customers benefit (d ≈ 0.2) because they receive preferential investment. Disruptive innovators experience maximum extraction (d ≈ 0.95) because they are locked out of incumbent's resources and face capital scarcity in low-margin markets. The VC ecosystem has moderate extraction (d ≈ 0.5) because they solve part of the problem but know their solution is temporary — successful disruptors will eventually become incumbents. The constraint's directional flow is from disruptors toward incumbents and their best customers: capital allocation, customer attention, distribution channels, and R&D resources all concentrate upmarket. Low-margin entrants bear the cost. Future competitive position (analytical perspective) experiences high extraction (d ≈ 0.85) because present rationality creates future organizational rigidity.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy here is the false distinction between 'this is coordination' (incumbent view) and 'this is pure extraction' (disruptor view). The constraint is genuinely a tangled rope: it has a real coordination function (capital allocation and customer focus are legitimate business disciplines that solve real problems) AND genuine asymmetric extraction (low-margin segments are starved of capital and innovation). The mandatrophy is resolved by recognizing that both perspectives are structurally accurate: the incumbent IS solving a coordination problem (capital scarcity is real), AND they ARE extracting from disruptors (low-margin market access is real). The constraint is not 'really' coordination disguised as extraction, or vice versa. It is a hybrid that genuinely combines both functions. The evidence for tangled rope classification is the dual presence of beneficiary groups (high-margin customers, incumbent shareholders) deriving real coordination benefits, victim groups (disruptors, low-margin entrants) bearing real extraction costs, and active enforcement (capital allocation processes, customer advisory boards, quarterly earnings pressures that maintain the flow). The constraint cannot be classified as pure rope (no asymmetric extraction gate passes) or pure snare (there is genuine coordination function present) — it is legitimately both.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    organizational_capability_vs_incentive,
    'Is the innovator''s dilemma fundamentally about organizational capability (ability to execute low-margin strategies) or about incentive misalignment (unwillingness to cannibalize high-margin business)?',
    'Empirical analysis of incumbent firms that have successfully deployed dual innovation processes (e.g., IBM''s PC division, Microsoft''s cloud transition, incumbents with venture arms). Distinguish firms that failed due to capability gaps vs those that could have succeeded with different incentive structures.',
    'If incentive-driven: the constraint is a tangled rope that can be restructured (separate P&L, modified compensation, sunset clauses on legacy products). If capability-driven: the constraint remains a snare for incumbents regardless of incentives.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(organizational_capability_vs_incentive, empirical, 'Whether dilemma is driven by organizational capability or incentive structure').

omega_variable(
    disruptor_margin_trajectory,
    'Do disruptive technologies inevitably migrate from low-margin to high-margin product tiers, or can they remain low-margin without losing viability?',
    'Long-term margin analysis of canonical disruptors (digital photography, smartphones, cloud computing, electric vehicles) across their lifecycle. Identify whether margin compression is structural or contingent on competitive dynamics.',
    'If inevitable: disruptors will eventually compete for incumbent''s customer base, validating the dilemma''s prediction. If contingent: low-margin niches can stabilize separately, reducing extraction on downstream entrants.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(disruptor_margin_trajectory, empirical, 'Whether disruptor margin trajectory is inevitable or contingent').

omega_variable(
    structural_decomposability,
    'Can the dilemma be structurally decomposed through organizational design (separate business units, distinct governance, modified incentives, venture arms) or is it an immutable consequence of rational profit maximization?',
    'Meta-analysis of incumbent response strategies to disruption: do firms with dual innovation processes, separate venture units, or modified compensation show different disruptor response rates? Longitudinal tracking of organizational structures that have successfully navigated disruption.',
    'If decomposable: the constraint is a tangled rope with redesign opportunities, not a snare. If immutable: the constraint borders on mountain (natural law of capital allocation), but the base properties still contradict — the suppression is institutional, not natural.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(structural_decomposability, conceptual, 'Whether the dilemma can be decomposed through organizational redesign').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(innovators_dilemma, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(innov_tr_t0, innovators_dilemma, theater_ratio, 0, 0.25).
narrative_ontology:measurement(innov_tr_t5, innovators_dilemma, theater_ratio, 5, 0.3).
narrative_ontology:measurement(innov_tr_t10, innovators_dilemma, theater_ratio, 10, 0.35).

% Extraction over time
narrative_ontology:measurement(innov_be_t0, innovators_dilemma, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(innov_be_t5, innovators_dilemma, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(innov_be_t10, innovators_dilemma, base_extractiveness, 10, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(innovators_dilemma, resource_allocation).
narrative_ontology:affects_constraint(innovators_dilemma, market_selection_efficiency).
narrative_ontology:affects_constraint(innovators_dilemma, venture_capital_concentration).
narrative_ontology:affects_constraint(innovators_dilemma, organizational_capability_lock).

% DUAL FORMULATION NOTE:
% The Innovator's Dilemma could decompose into two structurally distinct constraints: (1) incumbent_capital_allocation (resource allocation rational from quarterly perspective), (2) disruptor_market_access (entry barriers to low-margin segments). However, the constraint operates as a unified system where incumbent rationality directly causes disruptor constraint — the single ε=0.52 story captures this unity better than separate stories. Network links show how capital concentration and organizational capability lock-in propagate the constraint's effects downstream.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(innovators_dilemma, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
