% ============================================================================
% CONSTRAINT STORY: financialization_drag
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_financialization_drag, []).

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
 *   constraint_id: financialization_drag
 *   human_readable: The Financialization Gravity Well
 *   domain: economic/technological
 *
 * SUMMARY:
 *   The financialization gravity well describes a structural shift in
 *   resource allocation mechanisms: capital, regulatory favor, and skilled
 *   labor progressively concentrate in financial engineering rather than
 *   productive sector investment. Over 1980-2020, the financial sector's
 *   share of GDP grew from ~2.5% to ~8%; financial sector compensation
 *   captured an increasing fraction of total wages; and manufacturing as a
 *   share of US productive capacity declined from ~25% to ~11%. This
 *   constraint exhibits high extractiveness (0.62) and suppression (0.68)
 *   because the mechanism operates through incentive alignment rather than
 *   explicit coercion. Capital allocators rationally pursue financial returns
 *   when they exceed productive returns; workers cannot credibly relocate
 *   into sectors that are themselves capital-starved; and regulatory
 *   oversight is partly captured by the industry it oversees. The theater
 *   ratio (0.58) reflects that financial regulation creates procedural
 *   legitimacy (stress tests, capital adequacy) while the underlying gravity
 *   well persists: the incentive structure that drives capital toward
 *   financial arbitrage remains unaltered. Unlike a pure snare with single
 *   victim, financialization victimizes multiple structural agents
 *   simultaneously — manufacturing workers, small firms, technological
 *   innovation capacity, and long-term economic resilience. The mega-corp
 *   perspective (tangled rope) reveals that not all powerful actors are
 *   beneficiaries: large multinational corporations benefit from
 *   financialization's capital access but depend on the real productive
 *   capacity that financialization starves.
 *
 * KEY AGENTS:
 *   - Manufacturing Workers: Primary victim (powerless/trapped) — wages stagnate as capital diverts to financial returns
 *   - Small-to-Medium Enterprises: Primary victim (moderate/constrained) — cannot compete for capital; face rising cost of credit
 *   - Financial Sector Capital Allocators: Primary beneficiary (institutional/arbitrage) — capture returns from price discovery, risk management, leverage
 *   - High-Frequency Trading Platforms: Primary beneficiary (institutional/arbitrage) — extract value from speed-based arbitrage; fuel financialization acceleration
 *   - Mega-Corp Multi-Nationals: Secondary/ambivalent (powerful/mobile) — benefit from capital access but constrained by dependence on real supply chains
 *   - Central Bank Regulators: Theater maintainers (institutional/constrained) — manage stability appearance; constrained by political economy of financial capture
 *   - Technological Innovation Capacity: Victim (abstract/trapped) — capital diverted from R&D; institutional knowledge lost as sectors shrink
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(financialization_drag, 0.62).
domain_priors:suppression_score(financialization_drag, 0.68).
domain_priors:theater_ratio(financialization_drag, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(financialization_drag, extractiveness, 0.62).
narrative_ontology:constraint_metric(financialization_drag, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(financialization_drag, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(financialization_drag, snare).
narrative_ontology:human_readable(financialization_drag, "The Financialization Gravity Well").
narrative_ontology:topic_domain(financialization_drag, "economic/technological").

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(financialization_drag, financial_sector_capital_allocators).
narrative_ontology:constraint_beneficiary(financialization_drag, high_frequency_trading_platforms).
narrative_ontology:constraint_beneficiary(financialization_drag, derivative_issuers).
narrative_ontology:constraint_victim(financialization_drag, productive_manufacturing_sector).
narrative_ontology:constraint_victim(financialization_drag, real_wage_workers).
narrative_ontology:constraint_victim(financialization_drag, small_to_medium_enterprises).
narrative_ontology:constraint_victim(financialization_drag, technological_innovation_capacity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: MANUFACTURING WORKER (SNARE) — Trapped in declining productive sectors as capital flows to financial arbitrage. No exit option without complete relocation and skill retraining. Wages stagnant despite productivity gains captured by financial sector. d≈0.92, f(d)≈1.40, σ=1.2 → χ≈0.74.
constraint_indexing:constraint_classification(financialization_drag, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: SMALL TO MEDIUM ENTERPRISE (SNARE) — Constrained by inability to compete with financial engineering returns on capital. Banks prioritize lending to financial firms over productive investment. Cost of capital rises; access deteriorates. d≈0.88, f(d)≈1.32, σ=1.0 → χ≈0.62.
constraint_indexing:constraint_classification(financialization_drag, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: FINANCIAL SECTOR CAPITAL ALLOCATOR (ROPE) — Experiences financialization as pure coordination: efficient price discovery, risk management through derivatives, optimal capital reallocation. Arbitrage opportunities between markets are the legitimate function of the constraint. d≈0.08, f(d)≈-0.11, σ=1.2 → χ≈-0.06. Net beneficiary.
constraint_indexing:constraint_classification(financialization_drag, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: MEGA-CORP MULTI-NATIONAL (TANGLED ROPE) — Powerful actors benefit from financialization (access to capital markets, leverage for acquisitions, financial engineering for shareholder returns) but also constrained by needing productive capacity and skilled labor. Mobile exit (can relocate operations, offshore profits) but also dependent on real supply chains. d≈0.45, f(d)≈0.50, σ=1.2 → χ≈0.31. Hybrid experience: coordination for capital access + extraction from labor/supply chains.
constraint_indexing:constraint_classification(financialization_drag, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: CENTRAL BANK REGULATOR (PITON) — Maintains financial stability theater: stress tests, capital adequacy ratios, macroprudential oversight. Performative regulation that preserves the appearance of control while the fundamental gravity well persists. Theater_ratio=0.58 borderline; regulations maintain institutional legitimacy without addressing structural incentive misalignment. d≈0.50, f(d)≈0.65, σ=1.0 → χ≈0.37.
constraint_indexing:constraint_classification(financialization_drag, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (TANGLED ROPE) — From civilizational distance, financialization solves a genuine coordination problem (efficient capital allocation) while creating systematic extraction (capital diverted from productive investment toward financial returns). The gravity well is neither a natural law nor pure illusion: it is a structural consequence of incentive alignment in financial markets. Regulation attempts coordination but encounters enforcement costs that enable persistent extraction. d≈0.68, f(d)≈1.05, σ=1.2 → χ≈0.66.
constraint_indexing:constraint_classification(financialization_drag, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(financialization_drag_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(financialization_drag, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(financialization_drag, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(financialization_drag, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(financialization_drag, TR),
    TR >= 0.70.

:- end_tests(financialization_drag_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62): High. The constraint extracts value from productive sectors and redirects it to financial returns. The trajectory (0.28→0.62 over 40 years) shows systematic acceleration as financial engineering techniques proliferate and capital concentration increases. This is not a fixed property but an accumulating mechanism: each cycle of financialization enables more sophisticated financial instruments, which attract more capital, which increases the extraction gradient. Suppression (0.68): High-moderate. The mechanism operates through incentive alignment, not explicit coercion, but suppression comes from: (1) regulatory asymmetry (financial sector lobbies effectively; productive sectors lobby weakly), (2) information asymmetry (financial complexity hides extraction mechanisms from public view), (3) institutional lock-in (once capital and expertise concentrate in finance, exit costs become prohibitive). Theater ratio (0.58): Moderate-high. Financial regulation creates substantial performative activity (stress tests, capital adequacy ratios, resolution authority structures) that maintains institutional legitimacy while the underlying gravity well persists. The theater increased over time as regulations were layered in response to crises but without addressing the fundamental incentive structure. Claimed type (Snare) is justified by: χ ≥ 0.66 threshold (0.62 × 1.05 × 1.2 ≈ 0.78 from analytical perspective; 0.62 × 1.40 × 1.2 ≈ 1.04 from manufacturing worker perspective). High suppression (0.68 ≥ 0.60 gate). Multiple victims with no effective exit.
 *
 * PERSPECTIVAL GAP:
 *   The manufacturing worker and small firm perspectives (snare) diverge sharply from the financial sector allocator perspective (rope). The beneficiary genuinely experiences the constraint as pure coordination: efficient price discovery, optimal capital allocation, legitimate risk management. The victim experiences systematic exclusion and wage stagnation with no exit option. The mega-corp (tangled rope) occupies an intermediate position: it benefits from access to financial capital (rope function) but is also constrained by dependency on the real productive capacity that the gravity well is starving (snare-like constraint). The central bank regulator (piton) maintains the procedural legitimacy of oversight while the fundamental extraction mechanism persists. The analytical observer (tangled rope) recognizes both functions: the gravity well solves a coordination problem (capital to high-return opportunities) while simultaneously creating systematic extraction (capital diverted from productive investment). This perspectival gap is not resolvable by changing how we measure the constraint — it reflects genuinely different structural positions within the same system.
 *
 * DIRECTIONALITY LOGIC:
 *   Financial sector allocators: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.11. Net beneficiary; negative effective extraction. Manufacturing workers: Victim + trapped → d≈0.92, f(d)≈1.40. Maximum extraction pressure. Small firms: Victim + constrained → d≈0.88, f(d)≈1.32. High extraction; some limited exit via relocation/sector transition but with severe costs. Mega-corp multinational: Both beneficiary (capital access) and victim (labor scarcity, supply chain pressure) + mobile → d≈0.45, f(d)≈0.50. Mixed experience. Central bank regulator: Constrained actor experiencing coordination function + theater maintenance → d≈0.50, f(d)≈0.65. Moderate effective extraction from institutional perspective. Analytical observer: Sees both coordination and extraction simultaneously → d≈0.68, f(d)≈1.05. Confirms tangled rope classification.
 *
 * MANDATROPHY ANALYSIS:
 *   EXTRACTIVENESS > 0.70 THRESHOLD: Mandatrophy is resolved by recognizing the dual function. The financialization gravity well is NOT pure extraction misrepresented as coordination. It IS coordination (capital to high-return opportunities) that generates systematic extraction as a byproduct. The mandatrophy would emerge if we claimed the constraint was either (a) pure rope (only coordination, no extraction) or (b) pure snare (only extraction, no coordination function). The tangled rope classification resolves this by insisting that both functions are real and structural: (1) Coordination function: Financial markets do allocate capital to high-productivity uses and manage risk efficiently within their scope. (2) Extraction function: This efficient allocation to financial returns creates systematic under-investment in productive sectors, worker wage stagnation, and innovation capacity loss. The constraint cannot be justified purely as coordination, nor can it be condemned purely as extraction. The mandatrophy resolution comes from measuring both: beneficiaries genuinely benefit from coordination, victims genuinely suffer from extraction, and the two are inseparable consequences of the same mechanism. Policy responses must address the extraction without destroying the coordination function — this is the genuine dilemma, not a question of labeling.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    productive_vs_financial_returns_threshold,
    'At what rate differential do productive investments become rationally subordinate to financial engineering from a capital allocator''s perspective?',
    'Historical comparison of real productive returns (manufacturing ROI, R&D yield) vs financial returns (equity arbitrage, derivatives yield) across decades; identification of inflection point where financial returns exceed productive returns',
    'If productive returns structurally exceed financial returns: financialization is regulatory failure (snare from all perspectives). If financial returns structurally exceed productive returns: financialization is rational coordination (rope from powerful perspectives justified).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(productive_vs_financial_returns_threshold, empirical, 'Rate differential between productive and financial investment returns').

omega_variable(
    skilled_labor_substitution_reversibility,
    'Once capital and human expertise flee productive sectors into finance, can they be re-allocated back to manufacturing without degrading financial system stability?',
    'Analysis of skill transferability, institutional lock-in, regulatory dependencies; historical examination of de-financialization attempts (post-2008 regulatory efforts); measurement of friction costs for labor/capital reallocation',
    'If reversible with moderate friction: workers have constrained (not trapped) exit. If irreversible due to institutional lock-in: workers are trapped; classification shifts firmly to snare from all victim perspectives.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(skilled_labor_substitution_reversibility, empirical, 'Reversibility of productive sector divestment').

omega_variable(
    innovation_velocity_causality,
    'Does the financialization gravity well causally reduce innovation velocity in productive sectors, or do declining sectors lose financial support because they exhibit declining innovation independently?',
    'Causal analysis controlling for sectoral innovation: time-series correlation between capital flight and R&D productivity; identification of sectors with high innovation that retain capital despite low immediate returns; analysis of financing patterns pre/post financialization shift (1980-2000 transition)',
    'If causally destructive: financialization is extractive (snare). If correlation without causation: financialization is coordination responding to existing sector decline (rope-compatible explanation).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(innovation_velocity_causality, empirical, 'Causal relationship between financialization and innovation decline').

omega_variable(
    regulatory_capture_completeness,
    'To what degree have financial sector regulators been captured by the finance industry versus maintaining genuine independence in macroprudential policy?',
    'Analysis of revolving door (regulators → finance; finance → regulators); institutional funding sources for regulatory agencies; regulatory failure rate on major financial crises; comparative analysis of regulatory stringency across jurisdictions with different capture levels',
    'If fully captured: regulation is theater (piton validated). If substantially independent: regulation creates genuine friction (tangled rope from regulator perspective).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_capture_completeness, empirical, 'Degree of regulatory capture in financial sector oversight').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(financialization_drag, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fin_drag_tr_t0, financialization_drag, theater_ratio, 0, 0.35).
narrative_ontology:measurement(fin_drag_tr_t20, financialization_drag, theater_ratio, 20, 0.48).
narrative_ontology:measurement(fin_drag_tr_t40, financialization_drag, theater_ratio, 40, 0.58).

% Extraction over time
narrative_ontology:measurement(fin_drag_be_t0, financialization_drag, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(fin_drag_be_t20, financialization_drag, base_extractiveness, 20, 0.45).
narrative_ontology:measurement(fin_drag_be_t40, financialization_drag, base_extractiveness, 40, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(financialization_drag, resource_allocation).
narrative_ontology:affects_constraint(financialization_drag, manufacturing_decline_lock_in).
narrative_ontology:affects_constraint(financialization_drag, wage_stagnation_structural).
narrative_ontology:affects_constraint(financialization_drag, research_funding_scarcity).

% DUAL FORMULATION NOTE:
% Financialization gravity well is upstream of multiple sectoral decline constraints. The wage stagnation and manufacturing decline are downstream consequences of the capital reallocation mechanism. These constraints are structurally linked: addressing financialization requires simultaneous management of all three to avoid regulatory whack-a-mole (reducing financial returns without addressing productive sector competitiveness simply accelerates structural decline).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(financialization_drag, institutional, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
