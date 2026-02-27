% ============================================================================
% CONSTRAINT STORY: cloudflare_dual_class_asymmetry
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_cloudflare_dual_class_asymmetry, []).

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
 *   constraint_id: cloudflare_dual_class_asymmetry
 *   human_readable: Cloudflare Dual-Class Voting Control Asymmetry
 *   domain: economic/corporate_governance
 *
 * SUMMARY:
 *   Cloudflare's dual-class voting structure (Class B 10 votes per share for
 *   founders Matthew Prince and Lee Holloway; Class A 1 vote per share for
 *   public shareholders) creates a structural tension between founder
 *   incentive alignment and shareholder governance rights. The constraint
 *   exhibits multiple classification types depending on the observer's
 *   structural position. Class A shareholders see a snare: they purchase
 *   equity expecting standard voting rights but receive 1/10th voting power
 *   per share, with no exit except costly share sale. Founders see pure
 *   coordination: the structure solves the alignment problem and enables
 *   long-term vision without board interference. Institutional investors see
 *   tangled rope: the structure provides governance stability (coordination
 *   benefit) but also grants founders unilateral veto power on
 *   shareholder-friendly policies (extraction cost). Regulators see a piton:
 *   SEC disclosure requirements create the appearance of accountability, but
 *   the disclosure ritual does not actually constrain founder voting.
 *   Governance reformers see a scaffold: market evolution and activist
 *   pressure will eventually force sunsetting through shareholder proposals
 *   or founder mortality. The analytical observer risks naturalizing this as
 *   an immutable property of tech startups — but dual-class is a contingent
 *   legal choice, not a natural law.
 *
 * KEY AGENTS:
 *   - Matthew Prince & Lee Holloway (Founders): Primary beneficiaries (institutional/arbitrage) — retain voting control and founder veto power despite dilution of ownership
 *   - Class A Shareholders (Retail & Institutional): Primary victims (powerless/mobile and powerful/constrained) — experience voting asymmetry and limited governance input
 *   - Market Price Discovery: Secondary victim (moderate/constrained) — prices in governance risk via 5-15% dual-class discount
 *   - Proxy Advisors & Governance Activists: Organized agents (organized/mobile) — propose sunset clauses and governance reforms through shareholder proposals
 *   - SEC & Delaware Corporate Law: Institutional actors (institutional/constrained) — maintain disclosure requirements but allow dual-class structure
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks treating contingent design as natural law
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(cloudflare_dual_class_asymmetry, 0.52).
domain_priors:suppression_score(cloudflare_dual_class_asymmetry, 0.68).
domain_priors:theater_ratio(cloudflare_dual_class_asymmetry, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(cloudflare_dual_class_asymmetry, extractiveness, 0.52).
narrative_ontology:constraint_metric(cloudflare_dual_class_asymmetry, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(cloudflare_dual_class_asymmetry, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(cloudflare_dual_class_asymmetry, tangled_rope).
narrative_ontology:human_readable(cloudflare_dual_class_asymmetry, "Cloudflare Dual-Class Voting Control Asymmetry").
narrative_ontology:topic_domain(cloudflare_dual_class_asymmetry, "economic/corporate_governance").

domain_priors:requires_active_enforcement(cloudflare_dual_class_asymmetry).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(cloudflare_dual_class_asymmetry, matthew_prince_lee_holloway_founders).
narrative_ontology:constraint_victim(cloudflare_dual_class_asymmetry, class_a_shareholders).
narrative_ontology:constraint_victim(cloudflare_dual_class_asymmetry, market_price_discovery).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CLASS A SHAREHOLDER (SNARE) — Purchases shares expecting market standard: one share = one vote. Trapped in a structure where founder control is legally entrenched. Mobile exit exists (sell shares) but at significant cost: (1) disclosure that your vote has been diluted, (2) opportunity cost of missing upside if platform succeeds, (3) liquidity constraints in concentrated positions. Suppression is structural: the dual-class structure is disclosed in IPO materials but its extraction mechanism (founder veto power on all governance decisions) is not cognitively salient to most retail investors until governance disputes emerge. High experienced extraction χ due to trapped voting power and mobile-but-costly exit.
constraint_indexing:constraint_classification(cloudflare_dual_class_asymmetry, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 2: INSTITUTIONAL ASSET MANAGERS (TANGLED ROPE) — Large funds (Vanguard, BlackRock, State Street) that hold Cloudflare shares experience both coordination and extraction. Coordination: founders' incentive alignment (skin in the game) theoretically reduces agency costs — founders cannot be removed, so they invest long-term. Extraction: founders can unilaterally block acquisitions, dividend policy, capital structure changes, or shareholder-friendly governance reforms. Constrained exit: divesting $500M position moves the market and signals negative sentiment, inviting activist pressure on their own boards. Institutional investors accept some extraction in exchange for founder stability, but the ratio deteriorates if founders pursue value-destructive policies.
constraint_indexing:constraint_classification(cloudflare_dual_class_asymmetry, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 3: CLOUDFLARE FOUNDERS (ROPE) — Dual-class structure enables pure coordination from the founder perspective: they solve the alignment problem (founder vision survives investor pressure), achieve transaction efficiency (10:1 voting leverage), and capture optionality (can pursue high-risk product pivots without board constraints). Arbitrage exit is theoretically available (sell shares, retain voting control temporarily) but is not primary motivation — founders retain >90% voting power even post-IPO. This perspective experiences the structure as a coordination mechanism with minimal coercive overhead. Low experienced extraction from the founder view because they are the beneficiaries.
constraint_indexing:constraint_classification(cloudflare_dual_class_asymmetry, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: GOVERNANCE REFORM COALITION (SCAFFOLD) — Organized agents (ISS, Glass Lewis, Interfaith Center on Corporate Responsibility, academic governance researchers) view dual-class as a temporary coordination solution that will sunset through market pressure. Mechanism: as founders age, mortality risk increases; dual-class logic decays when founders are no longer active decision-makers. Alternative: activist investors repeatedly propose sunset clauses (share-dilution trigger, time-based expiration, founder tenure limits). Mobile exit is organizational (can divest positions, shift voting recommendations) but suppression is active (founders use majority control to reject governance reforms). Theater ratio is moderate (proxy contests and shareholder proposals are performative — outcomes are predetermined by founder voting power, but the ritual of annual meetings creates the appearance of contested governance). This perspective sees the structure as temporary because market evolution (new founders' mortality, institutional shareholder power concentration) will eventually force sunsetting.
constraint_indexing:constraint_classification(cloudflare_dual_class_asymmetry, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: SECURITIES REGULATORS (PITON) — SEC's position: dual-class voting is legal under Delaware corporate law and SEC disclosure rules. The constraint persists through regulatory inertia. Theater is high: SEC requires disclosure of voting disparities (Regulation S-K Item 403 amendments, 2015), creating the appearance of market-based accountability. Functional reality: disclosure enables informed decision-making for sophisticated investors, but does not provide remedy for retail investors who underestimate voting leverage. Constrainted exit reflects regulatory capture incentives: SEC cannot easily ban dual-class (would face political opposition from founder-led tech companies) and doesn't, instead relying on disclosure theater. The regulatory framework acknowledges the structure but treats it as managed through transparency rather than eliminated through restriction. High theater ratio because SEC compliance (annual proxy filing, Item 403C disclosure) is performed but does not actually constrain founder voting.
constraint_indexing:constraint_classification(cloudflare_dual_class_asymmetry, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: MARKET PRICE DISCOVERY (TANGLED ROPE) — The equity market collectively prices Cloudflare's shares based on: (1) technical performance, competitive position, growth trajectory, and (2) governance risk (founder lock-in, limited shareholder input on strategic decisions). The dual-class structure creates a persistent governance discount — empirical studies show dual-class firms trade at 5-15% discount to equivalent single-class firms due to perceived agency costs. Coordination function: founders have skin in the game, reducing agency waste. Extraction: the discount represents transferred wealth from public shareholders to founder-controlled equity pool. Constrained exit: market participants cannot coordinate to reject the structure (it is already embedded in the equity); they can only price it in. The constraint is active (founders actively use majority voting) and persistent (no sunset mechanism) yet accepted (market liquidity remains). Theater ratio is moderate: equity pricing is mechanically efficient (prices incorporate available information) but the governance risk component is difficult to quantify, creating slack for founder-favorable outcomes.
constraint_indexing:constraint_classification(cloudflare_dual_class_asymmetry, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From the civilizational perspective, some voting asymmetry is a structural inevitability in joint-stock companies where ownership and control are separated. The fundamental problem: dispersed shareholders have free-rider incentives in governance participation; founders have concentrated incentives in long-term vision. Dual-class solves this by giving founders voting leverage to overcome dispersed-shareholder passivity. This perspective sees the structure as an immutable property of how founder-led companies scale. However, this analysis naturalizes what is actually a contingent institutional design choice. Delaware corporate law permits dual-class, but does not require it; other jurisdictions regulate it differently (Germany mandates one-share-one-vote in some contexts; Sweden allows but requires sunset clauses); market competitors (Google pre-IPO, Amazon pre-IPO) chose different structures. The mountain classification is a false summit — it mistakes a legal accommodation for a natural law.
constraint_indexing:constraint_classification(cloudflare_dual_class_asymmetry, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(cloudflare_dual_class_asymmetry_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(cloudflare_dual_class_asymmetry, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(cloudflare_dual_class_asymmetry, TypeOther, context(agent_power(powerful), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(cloudflare_dual_class_asymmetry, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(cloudflare_dual_class_asymmetry, TR),
    TR >= 0.70.

:- end_tests(cloudflare_dual_class_asymmetry_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The dual-class structure transfers governance control value from dispersed public shareholders to concentrated founder shareholders. The extraction is not maximal (founders benefit from company success and have skin in the game) but is structurally significant (founders can unilaterally block acquisitions, dividend policy, or governance reforms). The trajectory shows increasing extractiveness over the first 6 years post-IPO as founders' voting leverage becomes more salient in contested governance situations. Suppression (0.68): High. Multiple suppression mechanisms: (1) disclosure is technical and underestimated by retail investors (information suppression), (2) voting structure is locked in and cannot be changed without founder consent (structural suppression), (3) exit is costly (economic suppression — share sale signals negative sentiment and forgoes upside). Theater ratio (0.55): Moderate. SEC disclosure requirements (Item 403C amendments) create the appearance of regulatory oversight. Proxy contests and shareholder proposals occur annually (theatrical governance ritual), but outcomes are predetermined by founder voting majority. However, the theater is not maximal (60+) because founders occasionally engage substantively with shareholder concerns, creating ambiguity about whether the constraint is purely extractive or partly responsive.
 *
 * PERSPECTIVAL GAP:
 *   The largest gap is between the founder perspective (Rope: coordination, minimal coercion) and the Class A shareholder perspective (Snare: extraction, trapped voting). Both observe the same 10:1 voting ratio, but experience it entirely differently. Founders see it as solving the alignment problem; shareholders see it as capturing their voting power. A secondary gap appears between the institutional investor perspective (Tangled Rope: mixed costs and benefits) and the market price discovery perspective (Tangled Rope: governance discount). Both see tangled rope but at different levels of aggregation — the institutional investor experiences voting constraints on individual governance decisions; the market experiences persistent governance discount that is capitalized into share price. The governance reform coalition (Scaffold) and the founder perspective (Rope) both see the structure as stable, but differ in permanence: founders see it as permanent solution; reformers see it as temporary until sunset mechanisms activate. The regulatory perspective (Piton) reflects institutional capture: SEC could ban dual-class but does not, instead maintaining high disclosure theater.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality (d) is derived from agent power, exit options, and structural benefit/cost. Founders benefit from the structure and have arbitrage exit (low d → negative χ). Class A shareholders suffer from the structure and have mobile-but-costly exit (high d → high χ). Institutional investors experience mixed benefits (stability) and costs (veto power) with constrained exit (medium d → moderate χ). Regulators are constrained but maintain appearance of control through disclosure (medium-high d). Governance activists have mobile exit but see the structure as temporary (low d via scaffold framing). The piton perspective shows that regulators' d is driven by inertia: they do not actively suppress founder voting (would face political opposition from tech founders) so they accept high theater ratio as substitute. The mountain perspective risks d ≈ 0.5 (neutral) by naturalizing the structure, but this is a false summit — the structural data shows asymmetric benefit (founders) and cost (public shareholders) that contradicts symmetry.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by recognizing that dual-class voting is a genuine tangled rope: it provides a real coordination benefit (founder incentive alignment, protection from short-term activist pressure) while extracting real governance costs (shareholder disenfranchisement, founder veto on value-creating decisions). The snare perspective is valid from the Class A shareholder view, but the snare classification is not the authoritative verdict — it is a perspectival reading. From the founder perspective, it is pure rope. The analytical observer risks naturalizing the structure as an immutable feature of tech company scaling (mountain classification), but this is a false summit revealed by examining the base extraction metrics: extractiveness (0.52) exceeds the mountain threshold (≤0.25) and suppression (0.68) exceeds the mountain threshold (≤0.05). The structure is not natural law; it is a contested institutional arrangement. The true mandatrophy resolution is that all classifications (snare, rope, tangled_rope, piton, scaffold) are valid from their respective perspectives, and the perspectival gap itself is the analytically important feature — it reveals that the structure distributes benefits (to founders) and costs (to public shareholders) asymmetrically, which is the definition of extraction. However, because founders' share in company success (skin in the game), the extraction is not maximal (not a pure snare from the analytical view), qualifying it as tangled rope with coordination benefits.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    founder_mortality_mechanism,
    'Does founder mortality or voluntary departure automatically trigger voting power redistribution, or do heirs and successors retain dual-class control?',
    'Historical cases (Google founders, Facebook Zuckerberg dynasty); examination of Cloudflare governance documents for succession clauses; empirical tracking of voting structure following founder transitions',
    'If automatic redistribution: scaffold perspective is correct and sunset is structural. If inherited: dual-class becomes permanent multi-generational extraction (snare status increases).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(founder_mortality_mechanism, empirical, 'Whether founder mortality triggers voting redistribution or inheritance').

omega_variable(
    market_discount_attribution,
    'Is the empirically observed 5-15% dual-class governance discount a true measure of agency cost extraction, or a misattribution of other risk factors (founder concentration, strategic risk)?',
    'Cross-sectional regression: dual-class dummy variable controlling for founder wealth concentration, cash burn rate, product diversification, competitive position; longitudinal tracking of Cloudflare share price vs peers with equivalent fundamentals but different voting structures',
    'If true agency cost: extraction is real and measurable (supports snare/tangled_rope). If misattribution: discount reflects rational pricing of founder-concentration risk (supports rope/scaffold).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(market_discount_attribution, empirical, 'Whether governance discount reflects true agency costs or founder-concentration risk').

omega_variable(
    founder_veto_frequency,
    'How often do founders use voting control to block shareholder proposals or board-recommended policies that would pass under one-share-one-vote?',
    'Proxy statement analysis: frequency of defeated shareholder proposals, board recommendations overridden by founder voting, strategic decisions made unilaterally by founders vs board consensus',
    'If high frequency: extraction is active (snare). If low frequency: coordination benefit dominates and founders rarely need to override (rope).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(founder_veto_frequency, empirical, 'Frequency of founder exercise of veto power against shareholder interests').

omega_variable(
    retail_investor_cognition,
    'Do retail investors understand at the time of purchase that their voting power is 1/10th of equivalent founder stake?',
    'Survey of retail investor knowledge at IPO; analysis of retail investor forums (Reddit, Twitter); correlation between investor education and subsequent sell-off decisions',
    'If low cognition: suppression is high (information disclosure is insufficient to enable informed decision); extraction is structurally snare. If high cognition: suppression is moderate (disclosure works) and investors consciously accept trade-off.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(retail_investor_cognition, empirical, 'Retail investor awareness of voting power asymmetry at purchase').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(cloudflare_dual_class_asymmetry, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cfdual_tr_t0, cloudflare_dual_class_asymmetry, theater_ratio, 0, 0.4).
narrative_ontology:measurement(cfdual_tr_t3, cloudflare_dual_class_asymmetry, theater_ratio, 3, 0.5).
narrative_ontology:measurement(cfdual_tr_t6, cloudflare_dual_class_asymmetry, theater_ratio, 6, 0.55).

% Extraction over time
narrative_ontology:measurement(cfdual_be_t0, cloudflare_dual_class_asymmetry, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(cfdual_be_t3, cloudflare_dual_class_asymmetry, base_extractiveness, 3, 0.48).
narrative_ontology:measurement(cfdual_be_t6, cloudflare_dual_class_asymmetry, base_extractiveness, 6, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(cloudflare_dual_class_asymmetry, enforcement_mechanism).
narrative_ontology:affects_constraint(cloudflare_dual_class_asymmetry, shareholder_activism_constraint).
narrative_ontology:affects_constraint(cloudflare_dual_class_asymmetry, founder_controlled_platform_governance).

% DUAL FORMULATION NOTE:
% Cloudflare's dual-class voting is structurally linked to the broader shareholder activism constraint (how activist investors attempt to reshape corporate policy despite super-majority founder control) and to founder-controlled platform governance constraints (how private platforms like Facebook/Meta use dual-class structures to maintain content moderation control). The upstream constraint is the legal permissibility of dual-class voting under Delaware law; the downstream constraints are specific governance disputes where dual-class voting prevents shareholder-backed reforms.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(cloudflare_dual_class_asymmetry, institutional, 0.38).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
