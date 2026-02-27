% ============================================================================
% CONSTRAINT STORY: cfius_hiefo_emcore_divestment
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_cfius_hiefo_emcore_divestment, []).

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
 *   constraint_id: cfius_hiefo_emcore_divestment
 *   human_readable: CFIUS authority to force divestment of strategic assets
 *   domain: geopolitical/economic
 *
 * SUMMARY:
 *   CFIUS authority to retroactively force divestment of strategic assets
 *   represents a tangled coordination-extraction hybrid that has evolved over
 *   50 years from narrow defense-industrial focus to increasingly broad
 *   geopolitical gatekeeping. The constraint operates between foreign
 *   acquirers (victims), the U.S. national security apparatus (primary
 *   beneficiary), domestic strategic industries (mixed beneficiary/victim),
 *   and allied foreign governments (constrained beneficiary). The core
 *   tension is structural: preventing hostile nations from controlling
 *   critical U.S. assets is a genuine coordination problem requiring
 *   unilateral state authority, but that same authority can be exploited for
 *   competitive advantage against allies and for selective extraction from
 *   foreign investors. The extractiveness measurement has risen from 0.35
 *   (1975, narrow defense focus) to 0.58 (2026, expanded 'national security'
 *   definition). Theater ratio rising from 0.52 to 0.68 reflects increasing
 *   gap between formal CFIUS review process (transparent, criteria-based
 *   rhetoric) and actual decision-making (discretionary,
 *   geopolitically-calibrated enforcement). The constraint is Tangled Rope,
 *   not Mountain, because CFIUS authority is contingent statutory law (not a
 *   law of nature), its scope has expanded through executive interpretation
 *   (not fixed by physics), and alternative governance structures exist (not
 *   logically inevitable).
 *
 * KEY AGENTS:
 *   - Foreign Acquirers (China, Russia, allies): Victims (powerless/trapped) — face retroactive unwind authority with minimal recourse after capital deployment
 *   - U.S. National Security Apparatus (DoD, CFIUS committee): Primary beneficiary (institutional/arbitrage) — controls unilateral authority to approve or block transactions; selectively enforces based on geopolitical alignment
 *   - Domestic Strategic Industries (semiconductor, defense-adjacent tech): Mixed beneficiary/victim (powerful/constrained) — benefit from foreign ownership prevention but bear cost of reduced capital inflow and exit liquidity
 *   - Allied Foreign Governments (Japan, South Korea, Canada, EU): Constrained beneficiary (organized/constrained) — benefit from CFIUS enforcement against adversaries but face asymmetric restrictions on their own strategic investments
 *   - International Trade Regime (WTO, multilateral agreements): Performative actor (institutional/arbitrage) — formally presumed non-discrimination in capital flows; functionally degraded by selective CFIUS enforcement
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing contingent statutory authority as inevitable law of geopolitics
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(cfius_hiefo_emcore_divestment, 0.58).
domain_priors:suppression_score(cfius_hiefo_emcore_divestment, 0.72).
domain_priors:theater_ratio(cfius_hiefo_emcore_divestment, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(cfius_hiefo_emcore_divestment, extractiveness, 0.58).
narrative_ontology:constraint_metric(cfius_hiefo_emcore_divestment, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(cfius_hiefo_emcore_divestment, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(cfius_hiefo_emcore_divestment, tangled_rope).
narrative_ontology:human_readable(cfius_hiefo_emcore_divestment, "CFIUS authority to force divestment of strategic assets").
narrative_ontology:topic_domain(cfius_hiefo_emcore_divestment, "geopolitical/economic").

domain_priors:requires_active_enforcement(cfius_hiefo_emcore_divestment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(cfius_hiefo_emcore_divestment, us_national_security_apparatus).
narrative_ontology:constraint_beneficiary(cfius_hiefo_emcore_divestment, incumbent_domestic_competitors).
narrative_ontology:constraint_victim(cfius_hiefo_emcore_divestment, foreign_acquirers).
narrative_ontology:constraint_victim(cfius_hiefo_emcore_divestment, market_certainty).
narrative_ontology:constraint_victim(cfius_hiefo_emcore_divestment, cross_border_capital_allocation).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: FOREIGN ACQUIRER POST-CLOSING (SNARE) — After acquiring a U.S. strategic asset (semiconductors, defense-adjacent technology), the foreign entity faces retroactive unwind authority with no meaningful exit. CFIUS can force divestment years after closing with minimal recourse. The acquirer bears full extractive cost: sunk capital, reputational damage, operational disruption. Suppression is near-total — no alternative to compliance once CFIUS acts.
constraint_indexing:constraint_classification(cfius_hiefo_emcore_divestment, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: DOMESTIC STRATEGIC INDUSTRY (TANGLED ROPE) — Benefits from CFIUS enforcement: prevents foreign ownership of critical suppliers, maintains domestic control of supply chains, reduces foreign leverage over U.S. strategic capacity. But also bears costs: chilling foreign investment reduces capital inflow, constrains exit options for domestic firms seeking foreign liquidity, creates regulatory uncertainty that increases cost of capital. Mixed extraction and coordination — benefits from protection but constrained by reduced liquidity.
constraint_indexing:constraint_classification(cfius_hiefo_emcore_divestment, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: U.S. NATIONAL SECURITY APPARATUS (ROPE) — CFIUS authority solves a genuine coordination problem: preventing strategic asset concentration in potentially hostile hands. The apparatus experiences the constraint as enabling — it creates the authority structure needed to maintain national security governance. No extraction against this agent; net beneficiary. The arbitrage option (can selectively enforce or grant exemptions) amplifies the coordination benefit.
constraint_indexing:constraint_classification(cfius_hiefo_emcore_divestment, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: ALLIED FOREIGN GOVERNMENTS (TANGLED ROPE) — Allied nations (Japan, South Korea, Canada, EU) benefit from CFIUS enforcement against hostile actors (China, Russia) but face growing restriction on their own strategic investments. Experienced as mixed: coordination against common adversaries, but asymmetric extraction as their firms face increasing scrutiny. Exit options constrained by political alliance requirements and retaliatory fears. Active enforcement gate satisfied: CFIUS requires regular reporting and political justification.
constraint_indexing:constraint_classification(cfius_hiefo_emcore_divestment, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: FORMAL INTERNATIONAL TRADE REGIME (PITON) — The WTO and multilateral trade agreements presume non-discriminatory capital flows. CFIUS enforcement creates performative compliance with trade rules while substantively restricting capital flows. The formal regime persists through institutional inertia (invocations of national security exception, grandfather clauses) despite functional degradation of its core principle (capital mobility). Theater ratio (0.68) reflects gap between stated non-discrimination and actual selective enforcement based on geopolitical calculation.
constraint_indexing:constraint_classification(cfius_hiefo_emcore_divestment, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, the tension between national security and free capital flows is inherent to state sovereignty: no nation can credibly commit to irreversible capital transfer to potential adversaries. The constraint appears as a natural law of geopolitics — the structural inevitability that states must retain some unilateral power over strategic asset ownership. However, the structural data contradicts the mountain classification: CFIUS is a contingent 1975 statutory authority, its scope has expanded through executive interpretation, and alternative governance structures exist (fixed thresholds, transparent criteria, binding arbitration). The analytical observer risks naturalizing a strategic choice as inevitable.
constraint_indexing:constraint_classification(cfius_hiefo_emcore_divestment, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(cfius_hiefo_emcore_divestment_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(cfius_hiefo_emcore_divestment, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(cfius_hiefo_emcore_divestment, TypeOther, context(agent_power(powerful), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(cfius_hiefo_emcore_divestment, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(cfius_hiefo_emcore_divestment, TR),
    TR >= 0.70.

:- end_tests(cfius_hiefo_emcore_divestment_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high, rising over interval. Base extractiveness reflects that CFIUS authority captures significant value for the U.S. security apparatus and domestic incumbents at the cost of foreign acquirers: reduced capital inflow, capital losses from forced divestitures, chilled future investment. The rise from 0.35 to 0.58 reflects scope expansion — 'national security' has broadened from defense contractors to cloud computing, AI, rare earths — capturing more transactions and higher extraction intensity. Suppression (0.72): High. Foreign acquirers face near-total suppression of alternatives once CFIUS determines divestment is necessary: no meaningful legal recourse, no bilateral trade remedies, no arbitration mechanism, limited transparency in decision criteria. Domestic industries face softer suppression: they can advocate for broader CFIUS scope or exemptions, but face political constraints and foreign retaliation risks. Theater ratio (0.68): Moderate-high. The CFIUS process presents formal criteria (defense-industrial relevance, critical infrastructure, supply chain resilience) but actual decisions are geopolitically calibrated and discretionary. The theater has increased as the formal criteria have become more performative — nearly any technology can be framed as 'national security' relevant. The gap between stated criteria and actual enforcement reflects strategic interest alignment over rule-following.
 *
 * PERSPECTIVAL GAP:
 *   The foreign acquirer sees pure extraction (Snare) — they bear unilateral cost with no exit, no recourse, retroactive enforcement. The domestic strategic industry sees mixed benefits and constraints (Tangled Rope) — they gain supply chain security but lose capital inflow and liquidity options. Allied foreign governments see the same mixed picture but organized (Tangled Rope) — they benefit from CFIUS enforcement against adversaries but face asymmetric restrictions on their own strategic bets. The U.S. security apparatus sees pure coordination (Rope) — CFIUS solves the legitimate problem of preventing hostile asset concentration. The formal international trade regime sees its core principle (non-discriminatory capital flows) degraded to theater (Piton) — the formal multilateral agreements presume capital mobility, but CFIUS selective enforcement undermines it. The analytical observer risks seeing a natural law (Mountain) — that states must retain unilateral control over strategic assets — but this naturalizes a contingent 1975 statutory choice that could be reformed with transparent thresholds and binding arbitration.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality d is determined by structural position: Who benefits? Who bears costs? What are the exit options? Foreign acquirers are victims (high d toward 1.0) with trapped exit — they deployed capital, cannot recover it, face retroactive enforcement with no meaningful recourse. The engine computes high f(d) from d ≈ 0.92 (victim + trapped), amplifying experienced extraction. The U.S. security apparatus are beneficiaries (low d toward 0.0) with arbitrage exit — they gain unilateral authority and can selectively enforce; the engine computes negative f(d) from d ≈ 0.08, making their χ negative (coordination benefit). Domestic strategic industries occupy middle ground (d ≈ 0.55-0.60): they benefit from foreign ownership prevention (lowering d) but face constrained exit options and reduced capital inflow (raising d). Allied governments face similar middle positioning but with organized power, shifting their d somewhat lower (≈0.45) due to their ability to coordinate countermeasures. The scope modifier σ(national) = 1.0 applies — CFIUS operates at national scale. The constraint's extractiveness is scaled by these directionality values, producing different experienced χ for each perspective: victims experience high χ (extraction), beneficiaries experience low/negative χ (coordination), mixed actors experience moderate χ (balanced pressure).
 *
 * MANDATROPHY ANALYSIS:
 *   CFIUS authority exhibits mandatrophy: it conflates a genuine coordination function (preventing hostile acquisition of strategic assets) with an extraction mechanism (selective enforcement favoring geopolitical allies and domestic incumbents). The classification resolves mandatrophy by recognizing both functions in the Tangled Rope type: beneficiaries = [u.s._national_security_apparatus, incumbent_domestic_competitors] (coordination), victims = [foreign_acquirers, market_certainty, cross_border_capital_allocation] (extraction), requires_active_enforcement = true (distinguishing from pure Rope). The perspectival gap reveals that coordination is asymmetric: it coordinates against adversaries but extracts from allies. The rise in extractiveness (0.35 → 0.58) and theater ratio (0.52 → 0.68) indicates rent-seeking layering onto coordination: as CFIUS scope has expanded beyond core defense-industrial assets into competitive technology domains, the coordination logic has weakened and the extraction logic has strengthened. Alternative governance (fixed thresholds, transparent criteria, binding arbitration for allies) could preserve coordination while reducing extraction. The fact that such alternatives are politically infeasible (omega_3) suggests that current discretion serves extractive interests beyond pure coordination. Mandatrophy is NOT resolved (mandatrophy_resolved: false) because the tension between coordination and extraction remains unresolved — the CFIUS structure admits both readings, and the policy discourse emphasizes coordination while the enforcement pattern emphasizes extraction against non-adversaries.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    national_security_definition_scope,
    'What constitutes ''national security'' for CFIUS purposes? How broadly does the term extend from defense-industrial base to general economic competition?',
    'Analysis of CFIUS cases: tracking expansion of ''national security'' framing from semiconductor foundries (clearly defense-relevant) to cloud computing, AI, rare earth processing, biotech. Measurement of case definitions over time.',
    'If narrowly defined: CFIUS is coordination mechanism (Rope). If broadly defined to include competitive advantage: CFIUS is extraction mechanism (Snare/Tangled Rope). Current trajectory toward broad definition empirically shifts classification toward Snare.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(national_security_definition_scope, empirical, 'Whether ''national security'' includes general economic competition').

omega_variable(
    retroactive_unwinding_frequency,
    'How frequently does CFIUS actually invoke retroactive divestment authority post-closing? Is it a credible threat or rarely-used backstop?',
    'Historical count of post-closing divestment orders vs. total completed transactions subject to CFIUS review. Timeline analysis: average delay between closing and divestment order if it occurs.',
    'If frequency < 1%: threat value exceeds actual extraction (classification shifts toward theater-heavy Piton). If frequency > 5%: credible enforcement (classification confirmed as Snare/Tangled Rope). Current rate unclear from public data.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(retroactive_unwinding_frequency, empirical, 'Actual frequency of retroactive CFIUS divestment orders').

omega_variable(
    alternative_governance_viability,
    'Would alternative structures (fixed thresholds, transparent criteria, binding arbitration) eliminate the extraction mechanism while preserving national security coordination?',
    'Comparative analysis of CFIUS vs. EU foreign investment review mechanisms vs. proposed CFIUS reform frameworks. Modeling of coordination sufficiency under constrained vs. discretionary authority.',
    'If viable: current CFIUS design represents extractive discretion rather than necessary coordination (Snare classification confirmed). If not viable: discretion is required for security (Tangled Rope/Mountain classification confirmed).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(alternative_governance_viability, conceptual, 'Whether alternative transparent governance structures could preserve security').

omega_variable(
    geopolitical_alignment_bias,
    'Does CFIUS enforcement correlate with geopolitical alignment? Are allied nation acquisitions treated differently than adversary acquisitions?',
    'Statistical analysis of CFIUS approval rates by acquirer nation origin. Controlled comparison of case details (asset type, strategic relevance) across approved vs. rejected transactions by country alignment.',
    'If significant bias: CFIUS is selective extraction mechanism favoring aligned powers (Tangled Rope classification toward upper range χ). If no bias: CFIUS is neutral security mechanism (Rope/Mountain). Preliminary data suggests substantial bias.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(geopolitical_alignment_bias, empirical, 'Correlation between geopolitical alignment and CFIUS enforcement').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(cfius_hiefo_emcore_divestment, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cfius_tr_t0, cfius_hiefo_emcore_divestment, theater_ratio, 0, 0.52).
narrative_ontology:measurement(cfius_tr_t15, cfius_hiefo_emcore_divestment, theater_ratio, 15, 0.61).
narrative_ontology:measurement(cfius_tr_t30, cfius_hiefo_emcore_divestment, theater_ratio, 30, 0.68).

% Extraction over time
narrative_ontology:measurement(cfius_be_t0, cfius_hiefo_emcore_divestment, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(cfius_be_t15, cfius_hiefo_emcore_divestment, base_extractiveness, 15, 0.48).
narrative_ontology:measurement(cfius_be_t30, cfius_hiefo_emcore_divestment, base_extractiveness, 30, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(cfius_hiefo_emcore_divestment, enforcement_mechanism).
narrative_ontology:affects_constraint(cfius_hiefo_emcore_divestment, semiconductor_supply_chain_concentration).
narrative_ontology:affects_constraint(cfius_hiefo_emcore_divestment, allied_capital_mobility_restriction).
narrative_ontology:affects_constraint(cfius_hiefo_emcore_divestment, rare_earth_dependency).

% DUAL FORMULATION NOTE:
% CFIUS authority operates at the intersection of three distinct structural claims: (1) preventing hostile acquisition of strategic assets (coordination, Mountain), (2) managing allied competition for strategic assets (Tangled Rope), (3) selective enforcement favoring geopolitical alignment over neutral criteria (Snare). These decompose into separate stories with different ε values and different upstream/downstream relationships. The current story focuses on claim (2) — the mixed extraction-coordination hybrid. Upstream constraints establish the necessity of strategic asset protection; downstream constraints model how CFIUS decisions ripple through specific supply chains and geopolitical relationships.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(cfius_hiefo_emcore_divestment, organized, 0.48).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
