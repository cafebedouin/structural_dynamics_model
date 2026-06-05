% ============================================================================
% CONSTRAINT STORY: strange_attractors
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_strange_attractors, []).

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
 *   constraint_id: strange_attractors
 *   human_readable: Systemic Risk Amplification via Strange Attractor Dynamics
 *   domain: economic/systemic_finance
 *
 * SUMMARY:
 *   Systemic risk in modern financial markets exhibits strange attractor
 *   dynamics: interconnected feedback loops (leverage pro-cyclicality,
 *   mark-to-market volatility cascades, liquidity evaporation, collateral
 *   chains) concentrate risk into self-reinforcing pathways from which
 *   individual agents cannot escape without triggering the very cascade they
 *   fear. The constraint is a snare because it extracts from retail
 *   investors, mid-sized institutions, and even large banks through forced
 *   participation in an increasingly unstable equilibrium. The central bank
 *   is trapped in a rescue loop: cannot exit without cascade, but rescue
 *   perpetuates moral hazard and deepens the attractor. The regulatory system
 *   is a piton—macro-prudential oversight and stress-testing are performative
 *   theater that masks the structural problem. From a chaos-theory
 *   perspective, the attractor appears as a natural law (mountain), but the
 *   underlying architecture (leverage norms, interconnectedness design,
 *   regulatory architecture) is contingent and policy-driven, making it a
 *   false summit. The constraint has deepened since 2008: extractiveness has
 *   risen from ~0.35 to 0.58, theater has risen from 0.42 to 0.58 (regulatory
 *   protocols proliferate without structural reform), and suppression remains
 *   high at 0.62 (opacity in derivatives markets, complexity of
 *   interconnectedness, moral hazard disincentives for disclosure).
 *
 * KEY AGENTS:
 *   - Retail Investors: Primary victims (powerless/trapped) — forced participation in attractor basin with no exit or visibility; catastrophic losses during cascade events
 *   - Mid-Sized Financial Institutions: Secondary victims (moderate/constrained) — interconnected via repo/derivatives; constrained exit due to exposure opacity; contagion risk is structural
 *   - Systemically Important Banks: Architect-victims (powerful/arbitrage paradox) — designed the attractor but remain trapped by too-interconnected-to-fail; hedging creates moral hazard
 *   - Central Bank: Institutional rescuer (institutional/constrained) — forced intervention perpetuates moral hazard; rescue is extracted value transferred to monetary system
 *   - Emerging Market Economies: Organized contagion victims (organized/mobile) — trapped by dollar funding but have partial exits through capital controls; defensive coordination possible
 *   - Regulatory System: Institutional piton (institutional/arbitrage) — performative oversight; theater increases while structural reform stalls; benefits the regulated, not systemic stability
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing attractor as inherent to complex systems rather than contingent institutional design
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(strange_attractors, 0.58).
domain_priors:suppression_score(strange_attractors, 0.62).
domain_priors:theater_ratio(strange_attractors, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(strange_attractors, extractiveness, 0.58).
narrative_ontology:constraint_metric(strange_attractors, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(strange_attractors, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(strange_attractors, snare).
narrative_ontology:human_readable(strange_attractors, "Systemic Risk Amplification via Strange Attractor Dynamics").
narrative_ontology:topic_domain(strange_attractors, "economic/systemic_finance").

% --- Structural relationships ---
narrative_ontology:constraint_victim(strange_attractors, retail_investors).
narrative_ontology:constraint_victim(strange_attractors, systemically_exposed_institutions).
narrative_ontology:constraint_victim(strange_attractors, emerging_market_economies).
narrative_ontology:constraint_victim(strange_attractors, financial_system_stability).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: RETAIL INVESTORS (SNARE) — Trapped within the attractor basin. Cannot exit without realizing catastrophic losses; must hold during cascade events because selling amplifies losses. No visibility into feedback loop cascade or when next shock arrives. d≈0.95, f(d)≈1.42, σ=1.2 → χ≈0.97.
constraint_indexing:constraint_classification(strange_attractors, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: MID-SIZED FINANCIAL INSTITUTIONS (SNARE) — Constrained exit. Interconnected via repo markets, derivatives, and interbank lending. Exposure is opacity-protected by accounting norms and regulatory forbearance. Contagion risk is structural, not measurable from inside. d≈0.82, f(d)≈1.25, σ=1.0 → χ≈0.73.
constraint_indexing:constraint_classification(strange_attractors, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: SYSTEMICALLY IMPORTANT BANKS (SNARE/ARCHITECT) — Powerful agents with arbitrage exit who nevertheless remain vulnerable to the attractor they helped structure. High leverage, pro-cyclical risk models, and too-interconnected-to-fail status mean they cannot credibly exit. Hedging creates moral hazard. d≈0.65, f(d)≈1.00, σ=1.2 → χ≈0.70. Paradox: architect is also trapped.
constraint_indexing:constraint_classification(strange_attractors, snare,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: CENTRAL BANK (SNARE/RESCUE) — Constrained by systemic stability duty. Cannot exit intervention without cascade, but intervention perpetuates moral hazard and deepens the attractor. Rescue operations are themselves extracted value — transferred risk to monetary system. d≈0.70, f(d)≈1.08, σ=1.2 → χ≈0.77. Theater_ratio escalates as policy theater (forward guidance, QE, emergency protocols) substitutes for structural reform.
constraint_indexing:constraint_classification(strange_attractors, snare,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: EMERGING MARKET ECONOMIES (TANGLED_ROPE) — Experience the attractor as a snare (trapped by dollar funding, capital flight dynamics), BUT they have exits through capital controls, reserve management, and regional clearing unions. Some coordination benefit from defensive integration. d≈0.55, f(d)≈0.75, σ=1.1 → χ≈0.48. Hybrid: pure extraction from attractor + some coordination through defensive alliances.
constraint_indexing:constraint_classification(strange_attractors, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: REGULATORY SYSTEM (PITON) — Macro-prudential oversight, stress testing, and capital adequacy rules are largely performative. Regulatory models assume independence that doesn't exist; feedback loops are ignored; systemic correlations are hidden by model risk. Theater_ratio≈0.62 (high). Regulatory architecture is maintained through inertia despite documented failures (2008, 2020 volatility spikes). Benefits the regulated (institutions get arbitrage), not society. d≈0.02, f(d)≈-0.11, σ=1.2 → χ≈-0.07.
constraint_indexing:constraint_classification(strange_attractors, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / CHAOS THEORY (MOUNTAIN?) — From civilizational timescale, strange attractors are inherent to complex dynamical systems with feedback; risk concentration is a property of phase-space geometry, not policy choice. Suggests the attractor is an irreducible natural law. However, structural data (ε=0.58, suppression=0.62) contradicts mountain classification — the engine detects this as a false summit, revealing that attractor structure is contingent on leverage norms, interconnectedness design, and regulatory architecture.
constraint_indexing:constraint_classification(strange_attractors, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(strange_attractors_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(strange_attractors, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(strange_attractors, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(strange_attractors, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(strange_attractors, TR),
    TR >= 0.70.

:- end_tests(strange_attractors_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High and rising. The constraint extracts through forced participation in an increasingly unstable equilibrium. Retail investors experience direct extraction (volatility, cascade losses). Institutions experience extraction through interconnectedness (contagion, forced liquidations). The central bank experiences extraction through moral hazard (resources deployed to rescues that perpetuate the underlying problem). The rise from 0.35 to 0.58 over 20 years reflects accumulating leverage, rising interconnectedness, and declining regulatory effectiveness. Suppression (0.62): High. Feedback loops are opaque (derivatives complexity, repo market opacity, interconnectedness hidden in regulatory reports). Moral hazard discourages disclosure (institutions hide leverage from each other). Models assume independence that doesn't exist (risk correlations during stress are systematically underestimated). No institutional actor has incentive to map the true attractor basin. Theater ratio (0.58): Moderate-high and rising. Macro-prudential oversight, stress tests, and capital adequacy rules proliferate without structural change. Central bank communication (forward guidance, emergency protocols) creates appearance of control. Regulatory theater masks the structural problem—leverage norms and interconnectedness design are unchanged despite documented failures.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates the snare's structural power: most agents rationally perceive it as pure extraction (snare), yet the system's architects (large banks, regulators) also cannot escape (architect paradox). The emerging markets see a tangled rope because they have partial exits through defensive integration. The regulatory system sees itself as piton (degraded but functional), not snare, because it benefits from the arrangement and has arbitrage exit. The analytical observer risks seeing the attractor as a mountain (inherent to complex systems), naturalizing what is actually a contingent institutional structure. The perspectival gap reveals that no single agent, not even the most powerful, controls the attractor—it is a structure that entraps everyone at different extraction rates.
 *
 * DIRECTIONALITY LOGIC:
 *   Retail investors: Victim + trapped → d≈0.95, f(d)≈1.42. Maximum extraction. Mid-sized institutions: Victim + constrained → d≈0.82, f(d)≈1.25. High extraction. Large banks: Beneficiary + arbitrage paradox → d≈0.65, f(d)≈1.00 (caught in their own trap despite nominal power). Central bank: Victim + constrained → d≈0.70, f(d)≈1.08 (trapped by duty to maintain stability). Emerging markets: Victim + mobile (organized) → d≈0.55, f(d)≈0.75 (partial exits through defensive coordination). Regulatory system: Beneficiary + arbitrage → d≈0.02, f(d)≈-0.11 (low effective extraction; serves the regulated). Analytical observer: Analytical → d≈0.72, f(d)≈1.15 (false summit detector flags the natural law framing as mislabeled).
 *
 * MANDATROPHY ANALYSIS:
 *   CRITICAL MANDATROPHY POINT: The strange attractor constraint nearly triggers mandatrophy because it could plausibly be framed as a pure information coordination problem (rope) rather than extraction (snare). The narrative would be: 'The system just needs better information, reduced leverage limits, and clearer interconnectedness disclosure—this is a coordination failure, not malice.' This framing is INCORRECT and must be rejected. The constraint is a snare because (1) agents rationally cannot exit without catastrophic loss, (2) suppression is structural—opaque derivatives, hidden leverage, model risk, and moral hazard disincentives for disclosure are built into the incentive structure, not accidents, (3) the extraction is not a byproduct of coordination but the primary function—large financial firms profit from volatility and leverage, and (4) rescue operations systematically reinforce moral hazard rather than resolve the underlying problem. The mandatrophy is resolved by recognizing that the constraint is EXTRACTED VALUE (rent extraction through forced participation in instability) not COORDINATION FAILURE. Disclosure reforms and leverage caps are necessary but insufficient because the underlying incentive structure (compensation tied to leverage, profits from volatility, too-big-to-fail protection) remains unchanged. True resolution requires structural reform (reshape compensation, break up systemic interconnectedness, change regulatory incentives), not just better information.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    attractor_basin_boundary,
    'Where is the true boundary of the current strange attractor basin? What leverage ratio, interconnectedness threshold, or shock magnitude defines the phase transition?',
    'Agent-based modeling of feedback loop cascade; empirical measurement of leverage-to-liquidity correlations; historical shock analysis to identify phase transition points',
    'If boundary is known and measurable: constraint moves from snare to tangled_rope (knowable risks enable hedging). If boundary is emergent/unknowable: snare classification holds; tail risks remain invisible.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(attractor_basin_boundary, empirical, 'Where is the true boundary of the attractor basin?').

omega_variable(
    feedback_loop_dominance_hierarchy,
    'Which feedback loops (leverage pro-cyclicality, mark-to-market volatility spiral, liquidity evaporation, collateral chains, fire sales) dominate the attractor dynamics? Can some be decoupled without destabilizing the whole?',
    'Stress-test the system removing each loop class; measure cascade magnitude with and without each feedback; identify which loops are load-bearing vs redundant',
    'If one loop dominates and is decoupled-able: targeted intervention can shrink attractor basin (scaffold perspective). If all loops are coupled: systemic reform required (snare deepens).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(feedback_loop_dominance_hierarchy, empirical, 'Which feedback loops dominate the attractor and are they decoupled-able?').

omega_variable(
    moral_hazard_feedback_closure,
    'Does central bank rescue intervention actually close the moral hazard feedback loop (institutions reduce leverage and interconnectedness) or does it reinforce it (institutions increase leverage, betting on rescue)?',
    'Longitudinal measurement of leverage, interconnectedness, and risk concentration pre- and post-rescue episodes; comparison with counterfactual scenarios (hard reshaping vs soft intervention)',
    'If rescue closes loop: intervention is essential containment (snare perspective justified). If rescue reinforces loop: intervention accelerates attractor deepening; cascade becomes inevitable (snare worsens).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(moral_hazard_feedback_closure, empirical, 'Does rescue intervention close or reinforce moral hazard feedback?').

omega_variable(
    alternative_attractor_stability,
    'Is a lower-leverage, lower-interconnectedness financial system dynamically stable, or does it drift back toward the high-risk attractor through institutional pressure and incentive structures?',
    'Historical comparison of regulatory regimes (post-Glass-Steagall vs post-Dodd-Frank); modeling of institutional drift under different interest rate and fee structures; measurement of whether reforms stick',
    'If alternative is stable: regulatory reform can transition the system away from snare (structural exit possible). If system drifts back: attractor is institutional, not just mathematical (snare is quasi-permanent).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_attractor_stability, conceptual, 'Is a low-risk equilibrium stable or does the system drift back to high-risk attractor?').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(strange_attractors, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sysrisk_tr_t0, strange_attractors, theater_ratio, 0, 0.42).
narrative_ontology:measurement(sysrisk_tr_t10, strange_attractors, theater_ratio, 10, 0.5).
narrative_ontology:measurement(sysrisk_tr_t20, strange_attractors, theater_ratio, 20, 0.58).

% Extraction over time
narrative_ontology:measurement(sysrisk_be_t0, strange_attractors, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(sysrisk_be_t10, strange_attractors, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(sysrisk_be_t20, strange_attractors, base_extractiveness, 20, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(strange_attractors, resource_allocation).
narrative_ontology:affects_constraint(strange_attractors, sovereign_debt_cascade).
narrative_ontology:affects_constraint(strange_attractors, repo_market_fragility).
narrative_ontology:affects_constraint(strange_attractors, derivatives_opacity_extraction).
narrative_ontology:affects_constraint(strange_attractors, central_bank_moral_hazard).

% DUAL FORMULATION NOTE:
% The strange attractor constraint is upstream of multiple financial instability constraints. Sovereign debt cascades, repo market dynamics, derivatives opacity, and central bank rescue loops are all structurally dependent on the attractor's existence and depth. The attractor's ε=0.58 represents the coordination failure at system level; downstream constraints have their own ε values reflecting domain-specific extractiveness. All downstream constraints are vulnerable to attractor intensification.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(strange_attractors, powerful, 0.65).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
