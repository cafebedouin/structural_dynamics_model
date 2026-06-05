% ============================================================================
% CONSTRAINT STORY: rent_seeking_equilibrium
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_rent_seeking_equilibrium, []).

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
 *   constraint_id: rent_seeking_equilibrium
 *   human_readable: The Toll-Bridge Stagnation
 *   domain: economic/political
 *
 * SUMMARY:
 *   The rent-seeking equilibrium represents a structural trap where incumbent
 *   wealth holders invest more resources in capturing existing wealth through
 *   political, legal, and regulatory influence than in creating new value.
 *   This constraint creates a toll bridge: all new economic activity must
 *   negotiate passage with entrenched actors, who extract rents in exchange
 *   for access. The constraint exhibits characteristics of both pure
 *   extraction (snare) and performative institutional maintenance (piton).
 *   From the incumbent perspective, rent-seeking appears as a coordination
 *   mechanism (rope) — trade associations, lobbying coalitions, and
 *   regulatory bodies represent solutions to the collective action problem of
 *   competition. From the productive economy and entry-level entrepreneurs,
 *   the same apparatus is a pure extraction mechanism with suppressed
 *   alternatives (snare). The constraint's theater ratio (0.65) reflects that
 *   regulatory and legal frameworks maintain a public-interest facade —
 *   consumer protection, competition enforcement, market safety — while their
 *   primary function in equilibrium is to entrench existing wealth. Over the
 *   30-year interval examined, base extractiveness has nearly doubled (0.32 →
 *   0.58), and theater has increased from 0.40 to 0.65, indicating that
 *   rent-seeking has become more sophisticated, more expensive to maintain,
 *   and more performative over time. The constraint is mandatrophy-resolved:
 *   the distinction between coordination (incumbent stability) and extraction
 *   (prevented innovation) is clear from the structural data and
 *   beneficiary/victim declarations.
 *
 * KEY AGENTS:
 *   - Incumbent Wealth Holders: Primary beneficiary (institutional/arbitrage) — capture regulatory rents, litigation advantages, and political influence without deploying capital in productive innovation
 *   - Entry-Level Entrepreneurs: Primary victim (powerless/trapped) — face regulatory compliance costs, legal attack risks, and inability to access capital or markets controlled by incumbents
 *   - Productive Economy: Secondary victim (moderate/constrained) — capital and labor trapped in existing arrangements; higher input costs, regulatory overhead, and opportunity cost of capital diverted to rent-seeking
 *   - Wage Workers: Secondary victim (moderate/constrained) — trapped in incumbent-dominated firms; limited job mobility; wage growth suppressed relative to productivity growth
 *   - Innovation Sector: Secondary victim (powerful/mobile) — can partially exit through offshore relocation, but faces policy pressure and loss of domestic market access
 *   - Reform-Minded Policymakers: Mixed position (organized/constrained) — see extraction problem but constrained by incumbent political power and campaign finance dependencies
 *   - Regulatory Apparatus: Institutional maintenance (institutional/arbitrage) — persists as theater; enforcement becomes selective; primary function (correcting market failures) atrophied
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(rent_seeking_equilibrium, 0.58).
domain_priors:suppression_score(rent_seeking_equilibrium, 0.68).
domain_priors:theater_ratio(rent_seeking_equilibrium, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(rent_seeking_equilibrium, extractiveness, 0.58).
narrative_ontology:constraint_metric(rent_seeking_equilibrium, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(rent_seeking_equilibrium, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(rent_seeking_equilibrium, snare).
narrative_ontology:human_readable(rent_seeking_equilibrium, "The Toll-Bridge Stagnation").
narrative_ontology:topic_domain(rent_seeking_equilibrium, "economic/political").

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(rent_seeking_equilibrium, incumbent_wealth_holders).
narrative_ontology:constraint_victim(rent_seeking_equilibrium, productive_economy).
narrative_ontology:constraint_victim(rent_seeking_equilibrium, entry_level_entrepreneurs).
narrative_ontology:constraint_victim(rent_seeking_equilibrium, wage_workers).
narrative_ontology:constraint_victim(rent_seeking_equilibrium, innovation_sector).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ENTRY-LEVEL ENTREPRENEUR (SNARE) — Cannot compete with entrenched actors' access to regulatory capture, litigation resources, and political influence. Trapped in a market where the rules are written by incumbents. High barriers to entry (regulatory compliance, legal challenges) funded by incumbents' extracted rents. d≈0.92, f(d)≈1.38, σ=1.0 → χ≈0.80.
constraint_indexing:constraint_classification(rent_seeking_equilibrium, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: PRODUCTIVE ECONOMY (SNARE) — Capital and labor trapped in existing arrangements; cannot easily relocate to non-rent-seeking equilibria. Extraction manifests as: higher input costs (monopolistic intermediaries), regulatory compliance overhead, and opportunity cost of capital diverted to rent-seeking rather than innovation. d≈0.85, f(d)≈1.18, σ=1.0 → χ≈0.68.
constraint_indexing:constraint_classification(rent_seeking_equilibrium, snare,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: INCUMBENT WEALTH HOLDERS (ROPE) — Experience the constraint as a coordination mechanism: lobbying groups, trade associations, and regulatory capture represent coordination to defend market position. From their perspective, the rent-seeking apparatus is a solution to the collective action problem of competition. d≈0.08, f(d)≈-0.11, σ=1.0 → χ≈-0.06. Net beneficiary.
constraint_indexing:constraint_classification(rent_seeking_equilibrium, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: REFORM-MINDED POLICYMAKERS (TANGLED ROPE) — See both coordination function (incumbent stability prevents systemic instability) and asymmetric extraction (wealth captured at the expense of growth). Constrained by political economy: reducing incumbents' extraction invites their political opposition and withdrawal of campaign support. Both benefit from the system's predictability and harmed by its inefficiency. d≈0.62, f(d)≈0.82, σ=1.0 → χ≈0.47.
constraint_indexing:constraint_classification(rent_seeking_equilibrium, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: REGULATORY APPARATUS (PITON) — The regulatory and legal infrastructure persists as a theater of consumer protection, competition enforcement, and market safety, but its primary function (innovation and efficiency) has atrophied. Regulatory bodies capture by incumbents; enforcement becomes selective. theater_ratio=0.65 indicates significant performative activity (regulatory reviews, antitrust hearings) without functional correction of rent-seeking. d≈0.10, f(d)≈-0.09, σ=1.0 → χ≈-0.06.
constraint_indexing:constraint_classification(rent_seeking_equilibrium, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / ECONOMIC LAW VIEW (MOUNTAIN) — From a civilizational/universal perspective, rent-seeking is an immutable feature of property rights and markets: wherever wealth exists, actors have incentives to defend and expand it through political means. This perspective sees the toll-bridge stagnation as a natural outcome of human nature and institutional structure, not a contingent arrangement. However, the structural data (ε=0.58, suppression=0.68, theater=0.65) contradicts the mountain classification — the engine will detect a false summit, revealing that rent-seeking equilibrium is contingent on specific institutional design, not inevitable.
constraint_indexing:constraint_classification(rent_seeking_equilibrium, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(rent_seeking_equilibrium_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(rent_seeking_equilibrium, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(rent_seeking_equilibrium, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(rent_seeking_equilibrium, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(rent_seeking_equilibrium, TR),
    TR >= 0.70.

:- end_tests(rent_seeking_equilibrium_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High. Incumbent wealth holders systematically capture regulatory rents, litigation resources, and political influence, and deploy these to prevent market entry and innovation. The measurement trajectory (0.32 → 0.58 over 30 years) indicates that rent-seeking apparatus has become more sophisticated and expensive. Base extraction is not higher (e.g., 0.75) because some incumbents do maintain genuine innovation capacity, and the constraint has not achieved total market closure — partial exits and niche markets exist. Suppression (0.68): High. Entry barriers are substantial: regulatory compliance costs (averaging 10-25% of startup capital), litigation risk from incumbent patent/trade secret attacks, and restricted access to capital markets (incumbents control bank boards and venture networks). Exits exist but are heavily constrained. Theater Ratio (0.65): Moderate-high. Regulatory bodies maintain public legitimacy through consumer protection reviews, antitrust investigations, and market safety enforcement, but effectiveness in preventing rent-seeking is low. The performative component has increased over time as rent-seeking has become more subtle and legally sophisticated. Claimed Type: Snare. The structural data meets snare thresholds: ε=0.58 (≥0.46), suppression=0.68 (≥0.60), χ will exceed 0.66 across multiple perspectives.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates sharp perspectival divergence. Incumbent wealth holders experience coordination (rope) — they coordinate to manage competition and secure returns. Entry-level entrepreneurs experience pure extraction (snare) — they face one-way barriers and no escape. The productive economy experiences mixed effects (tangled rope) — some benefit from incumbent stability and scale, others suffer from foreclosure. Reform policymakers see both coordination (preventing instability) and extraction (preventing innovation) simultaneously, making them trapped between conflicting institutional pressures. The regulatory apparatus sees itself as functional (mountain) — protecting consumers and competition — but behaves as theater (piton) — maintaining legitimacy while enabling capture. The analytical observer risks naturalizing rent-seeking as inevitable, missing the contingent institutional designs that enable it. The perspectival gap is not merely observational disagreement; it reflects structural asymmetries in power, exit options, and benefit flows.
 *
 * DIRECTIONALITY LOGIC:
 *   Incumbent wealth holders: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.11. Net beneficiary; experience negative effective extraction (they extract from others). Entry-level entrepreneurs: Victim + trapped → d≈0.92, f(d)≈1.38. Maximum extraction; no exit options within the constraint system. Productive economy: Victim + constrained → d≈0.85, f(d)≈1.18. High extraction; some exit options (capital relocation, merger into incumbents) but costly and incomplete. Reform policymakers: Mixed (beneficiary of stability + victim of inefficiency) + constrained → d≈0.62, f(d)≈0.82. Moderate-high extraction; constrained by political economy (cannot reduce incumbent rents without losing political support). Regulatory apparatus: Institutional + arbitrage → d≈0.10, f(d)≈-0.09. Piton classification derives from theater gate (≥0.70 theater), not from high d. Innovation sector: Victim + mobile → d≈0.55, f(d)≈0.75. Moderate extraction; can partially exit through relocation but faces policy pressure.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLVED: The constraint clearly separates coordination (incumbent stability, predictable rule-making) from extraction (blocked entry, suppressed innovation). The confusion arises from the incumbent perspective: they genuinely coordinate to solve their collective action problem (competing with each other while defending against external threats). But this coordination function produces asymmetric extraction from everyone outside the incumbent coalition. The mandatrophy is resolved by recognizing that coordination for SOME agents is extraction for OTHERS. The snare classification is justified because: (1) the primary function from the system's perspective is extraction (preventing innovation and entry), not coordination; (2) the coordination that does exist (incumbent cartels, regulatory capture) is purely defensive, not productive; (3) beneficiaries are concentrated and explicit (incumbent wealth holders), while victims are dispersed and implicit (foregone innovation, prevented entry). The piton observation (theater_ratio=0.65) indicates that regulatory institutions maintain legitimacy through performative activity (antitrust reviews, consumer protection) without functional correction of the underlying rent-seeking structure. The measurement trajectory shows that theater has increased as rent-seeking has become more sophisticated — the apparatus requires more elaborate justification as its extractive function becomes more visible. This is the classic piton pattern: function atrophied (regulatory agencies can no longer effectively police rent-seeking), but institutional persistence maintained through performance.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    productive_versus_extractive_boundary,
    'What proportion of incumbent wealth comes from genuine innovation vs. regulatory/legal capture?',
    'Historical patent analysis; comparison of R&D spending in concentrated vs. competitive industries; measurement of welfare gains from incumbents'' products vs. costs of market foreclosure',
    'If >70% productive: snare classification overstates extraction severity. If <30% productive: snare classification is understated; constraint is more severe than ε=0.58 indicates.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(productive_versus_extractive_boundary, empirical, 'Boundary between productive returns and extractive rents').

omega_variable(
    regulatory_capture_extent,
    'How much of regulatory/legal complexity represents genuine public interest protection vs. incumbent-serving barriers to entry?',
    'Analysis of regulatory changes when incumbents gain/lose political power; comparison of compliance costs across industries with varying incumbent concentration; audit of regulatory benefit-cost analyses',
    'If capture is high (>60%): suppression should exceed 0.68, and the constraint moves toward severe snare. If capture is low (<40%): suppression should decrease, and constraint may be tangled rope rather than snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_capture_extent, empirical, 'Extent of regulatory capture by incumbents').

omega_variable(
    exit_option_materiality,
    'Can mid-sized firms actually exit the rent-seeking equilibrium by relocating, decoupling from regulatory dependencies, or finding niche markets outside incumbent control?',
    'Empirical study of firm exit rates, relocation costs, and success rates for businesses attempting to avoid rent-seeking dependencies; measurement of geographic variation in rent-seeking intensity',
    'If exit is feasible: many agents classified as ''trapped'' should be ''constrained'' or ''mobile'', lowering d values and reducing χ. If exit is illusory: trap classification is confirmed, and suppression may be understated.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(exit_option_materiality, empirical, 'Whether exit from rent-seeking equilibrium is materially feasible').

omega_variable(
    efficiency_cost_magnitude,
    'What is the aggregate economic cost of capital diverted from productive innovation to rent-seeking activities?',
    'Estimation of total lobbying, litigation, and regulatory compliance spending; comparison with aggregate R&D spending; measurement of growth foregone relative to low-rent-seeking equilibrium benchmarks',
    'If costs are <5% of GDP: extraction is moderate, snare classification marginal. If costs are >15% of GDP: extraction is severe, and constraint may warrant reclassification to higher suppression.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(efficiency_cost_magnitude, empirical, 'Aggregate efficiency cost of rent-seeking diversion').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(rent_seeking_equilibrium, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(rent_tr_t0, rent_seeking_equilibrium, theater_ratio, 0, 0.4).
narrative_ontology:measurement(rent_tr_t15, rent_seeking_equilibrium, theater_ratio, 15, 0.52).
narrative_ontology:measurement(rent_tr_t30, rent_seeking_equilibrium, theater_ratio, 30, 0.65).

% Extraction over time
narrative_ontology:measurement(rent_be_t0, rent_seeking_equilibrium, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(rent_be_t15, rent_seeking_equilibrium, base_extractiveness, 15, 0.45).
narrative_ontology:measurement(rent_be_t30, rent_seeking_equilibrium, base_extractiveness, 30, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(rent_seeking_equilibrium, enforcement_mechanism).
narrative_ontology:affects_constraint(rent_seeking_equilibrium, regulatory_complexity_accumulation).
narrative_ontology:affects_constraint(rent_seeking_equilibrium, innovation_capital_concentration).
narrative_ontology:affects_constraint(rent_seeking_equilibrium, market_access_gatekeeping).

% DUAL FORMULATION NOTE:
% Rent-seeking equilibrium is downstream of specific rent sources (regulatory barriers, intellectual property enforcement, capital access restrictions). This story models the aggregate equilibrium state. Upstream constraints capture individual rent-extraction mechanisms (e.g., patent thickets, occupational licensing, market entry regulations). The network edges reflect structural coupling: reducing one rent source shifts extraction pressure to others unless the underlying institutional design changes.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(rent_seeking_equilibrium, moderate, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
