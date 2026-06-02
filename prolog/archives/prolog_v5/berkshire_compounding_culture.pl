% ============================================================================
% CONSTRAINT STORY: berkshire_compounding_culture
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_berkshire_compounding_culture, []).

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
 *   constraint_id: berkshire_compounding_culture
 *   human_readable: The Berkshire Hathaway Culture of Compounding
 *   domain: economic/corporate_governance
 *
 * SUMMARY:
 *   The Berkshire Hathaway culture of compounding is a corporate governance
 *   constraint that mandates reinvestment of earnings rather than
 *   distribution as dividends. Established by Warren Buffett's control
 *   structure and perpetuated through corporate policy and shareholder
 *   culture, it constrains capital allocation decisions across the entire
 *   enterprise. The constraint exhibits a perspectival chasm: long-term
 *   patient shareholders see genuine coordination (rope) and wealth creation.
 *   Dividend-seeking investors see extraction (snare). Management sees
 *   coordination that also preserves their capital allocation authority.
 *   Institutional governance advocates see a degrading structure (piton)
 *   sustained by founder mythology rather than functional efficiency. The
 *   philosophical framing naturalizes what is actually a contingent choice
 *   about capital structure, tax treatment, and voting control. The
 *   extractiveness has increased over the 50-year interval as the original
 *   compounding thesis (reinvested returns exceed external cost of capital)
 *   has faced headwinds from sheer capital mass and return compression, yet
 *   the cultural mandate persists unchanged.
 *
 * KEY AGENTS:
 *   - Warren Buffett and successor management: Primary beneficiary (institutional/arbitrage) — control capital allocation; experience constraint as pure coordination; captured voting control over decades
 *   - Patient long-term shareholders (Berkshire devotees): Secondary beneficiary (moderate/mobile) — benefit from compounding but constrained by reinvestment mandate; can exit via share sales
 *   - Dividend-seeking investors (retirees, income-focused funds): Primary victim (powerless/trapped) — locked into no-dividend structure; can exit only at tax/transaction cost; bear opportunity cost vs. dividend alternatives
 *   - Institutional investors (governance activists, index funds): Organized pressure (organized/constrained) — building coalition for dividend policy review; see constraint as dysfunctional given Berkshire's capital mass
 *   - Financial media and analyst community: Performative validators (powerful/arbitrage) — reinforce compounding mythology; sustain theater ratio through repetition of efficiency narrative
 *   - Analytical observer: Civilizational view (analytical/analytical) — risks naturalizing voting control and tax arbitrage as inevitable mathematical consequences of compounding
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(berkshire_compounding_culture, 0.38).
domain_priors:suppression_score(berkshire_compounding_culture, 0.42).
domain_priors:theater_ratio(berkshire_compounding_culture, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(berkshire_compounding_culture, extractiveness, 0.38).
narrative_ontology:constraint_metric(berkshire_compounding_culture, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(berkshire_compounding_culture, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(berkshire_compounding_culture, tangled_rope).
narrative_ontology:human_readable(berkshire_compounding_culture, "The Berkshire Hathaway Culture of Compounding").
narrative_ontology:topic_domain(berkshire_compounding_culture, "economic/corporate_governance").

domain_priors:requires_active_enforcement(berkshire_compounding_culture).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(berkshire_compounding_culture, long_term_shareholders).
narrative_ontology:constraint_beneficiary(berkshire_compounding_culture, berkshire_management).
narrative_ontology:constraint_victim(berkshire_compounding_culture, dividend_seeking_investors).
narrative_ontology:constraint_victim(berkshire_compounding_culture, short_term_capital_allocation).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DIVIDEND-SEEKING INVESTOR (SNARE) — Trapped by the no-dividend structure; cannot extract income without selling shares (incurring tax/fees). Bears full cost of the reinvestment mandate. Exit exists only at liquidity cost. Maximum experienced extraction.
constraint_indexing:constraint_classification(berkshire_compounding_culture, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: PATIENT LONG-TERM SHAREHOLDER (TANGLED ROPE) — Mobile exit option (can sell shares), but benefits from compounding. Extraction present (reinvestment mandate vs. dividend alternative) but offset by genuine coordination gain (long-term wealth accumulation). Moderate extraction with asymmetric distribution of upside.
constraint_indexing:constraint_classification(berkshire_compounding_culture, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 3: BERKSHIRE MANAGEMENT (ROPE) — Buffett/Todd (via voting control and incentive alignment) experience the constraint as pure coordination. Reinvestment maximizes their control duration and capital deployment optionality. Primary beneficiary. Extraction flows toward this agent through capital allocation authority.
constraint_indexing:constraint_classification(berkshire_compounding_culture, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: SUCCESSOR GOVERNANCE COALITION (SCAFFOLD) — Organized pressure from institutional investors, governance advocates, and estate planning concerns is generating alternative mechanisms (dividend increases, share buybacks, succession planning). Sunset clause is implicit: once Buffett/Todd era ends, the reinvestment mandate will face structural renegotiation. Coordination function (reinvestment) persists but with declining suppression of alternatives.
constraint_indexing:constraint_classification(berkshire_compounding_culture, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: BUFFETT CULT OF PERSONALITY (PITON) — The philosophical justification for compounding (patient capital, long-term value creation) has become increasingly theatrical. Modern Berkshire operations (insurance float utilization, dealmaking, activist positioning) are only loosely connected to the original compounding narrative. The constraint is sustained partly through founder-worship and legitimacy theater rather than functional necessity. Theater ratio reflects the divergence between the ideological framing and operational reality.
constraint_indexing:constraint_classification(berkshire_compounding_culture, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a universal financial theory perspective, the no-dividend policy appears as an immutable mathematical consequence of compounding: if reinvested returns exceed external capital costs, reinvestment strictly dominates distribution. This perspective risks naturalizing what is actually a choice embedded in corporate governance (share class structure, voting control, tax arbitrage). Engine false summit detector will flag this as naturalization of contingent institutional design.
constraint_indexing:constraint_classification(berkshire_compounding_culture, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(berkshire_compounding_culture_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(berkshire_compounding_culture, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(berkshire_compounding_culture, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(berkshire_compounding_culture, TR),
    TR >= 0.70.

:- end_tests(berkshire_compounding_culture_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The core extraction is real but bounded. Dividend-seeking investors are forced to forgo distributions they could access from dividend-paying equities or bonds. The opportunity cost is measurable but varies by shareholder profile. For patient shareholders, the extraction is offset by genuine compounding gain. For management, there is no extraction — they experience pure coordination. The 0.38 value reflects that the constraint redistributes capital allocation authority toward long-term reinvestment and management discretion, but the underlying returns have historically justified this in absolute terms (though increasingly contested). Suppression (0.42): Moderate. Exit options exist (sell shares), but carry tax and transaction costs. The share class structure (Class A voting control, Class B/C tracking) reduces shareholder voice. Alternative governance structures (dividend policies, share buybacks) are available but suppressed by voting control. Suppression has been stable because Buffett's track record provided sufficient legitimacy to prevent shareholder rebellion; post-Buffett, suppression will likely rise as governance activists press alternatives. Theater ratio (0.35): Moderate-low. The original compounding thesis (patient capital, long-term value creation) is substantively grounded in decades of outperformance. However, the theater has increased as operational reality has diverged from the narrative. Berkshire's modern business (insurance float utilization, derivatives positioning, activist dealmaking) is loosely connected to the philosophical framing of compounding. The constraint is justified increasingly by legitimacy theater rather than functional necessity — hence the 0.35 (rising from 0.15) reflects Goodhart drift.
 *
 * PERSPECTIVAL GAP:
 *   This constraint generates a maximum perspectival divergence. The patient shareholder and management see the constraint as beneficial coordination (Rope/Rope). The dividend-seeking investor experiences it as coercive extraction (Snare). The governance activist sees it as dysfunctional inertia sustained by founder mythology (Piton). The successor governance coalition sees it as a temporary structure with an implicit sunset (Scaffold). The natural law perspective risks misclassifying a governance choice as mathematical inevitability (false Mountain). The gap between 'compounding is optimal mathematics' and 'compounding is an entrenched voting control mechanism' is the core analytic divide.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is determined by each agent's position relative to capital allocation authority. Buffett/management have arbitrage exit (can reallocate capital freely) and benefit from reinvestment policy (maintains their discretionary authority and long-term control duration). Their d ≈ 0.05 (beneficiary + arbitrage) produces negative f(d), resulting in rope. Patient shareholders have mobile exit (can sell shares) and benefit from compounding realized in share price appreciation. Their d ≈ 0.35 (mixed beneficiary/victim, mobile exit) produces moderate f(d) and moderate χ, resulting in tangled_rope. Dividend-seeking investors have trapped exit (can exit only at cost) and bear the extraction (forgone dividend distributions). Their d ≈ 0.90 (victim + trapped) produces high f(d) and high χ, resulting in snare. Organized governance activists have constrained exit (limited ability to influence policy without coalition) but access to alternative governance models. Their d ≈ 0.45 (moderate victim with constrained exit, growing agency) produces moderate f(d), and the scaffold classification derives from the sunset clause (expectation of policy change post-Buffett) rather than from low χ alone.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy in Berkshire's compounding culture is the question: 'Is this coordination that should be mandated, or extraction disguised as coordination?' The resolution is perspectival. From management's view: pure coordination (rope). From the patient shareholder's view: mixed (tangled_rope) — genuine gain offset by constrained alternatives. From the dividend-seeker's view: pure extraction (snare). The piton classification reveals that the philosophical justification has decayed — the narrative of compounding efficiency persists through founder mythology and media repetition rather than through continual demonstration of returns-above-cost-of-capital. The false mountain classification at the analytical level shows the risk of naturalizing what is actually a governance choice. The constraint resolves the mandatrophy by showing that the classification depends critically on the shareholder's time horizon and capital needs. The universal analytical view that risks naturalizing the no-dividend mandate as optimal mathematics is the false summit — the engine flags this as naturalization of contingent voting control and tax arbitrage.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    compounding_efficiency_threshold,
    'At what accumulated capital base does the marginal return to reinvestment fall below the cost of capital, making dividend distribution optimal?',
    'Real-world empirical analysis of Berkshire''s actual returns on incremental capital deployment over successive decades. Comparison with benchmark equity returns.',
    'If threshold crossed: the compounding mandate becomes wealth-destructive rather than wealth-generative, converting rope to snare for long-term shareholders. If threshold not crossed: the rope classification holds indefinitely.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(compounding_efficiency_threshold, empirical, 'Threshold at which reinvestment returns fall below cost of capital').

omega_variable(
    succession_regime_stability,
    'Will successor management maintain the no-dividend compounding culture under different governance structures and investor bases?',
    'Post-Buffett policy decisions on distributions; governance activism by institutional investors; comparative analysis of peer conglomerates under different management.',
    'If maintained: scaffold perspective is false (no real sunset). If abandoned: scaffold classification confirmed, and the constraint transitions from tangled_rope to rope/snare depending on successor''s rationale.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(succession_regime_stability, empirical, 'Will successor management maintain compounding mandate').

omega_variable(
    tax_arbitrage_sustainability,
    'How much of the compounding mandate''s efficiency advantage derives from favorable tax treatment of retained earnings vs. distributed dividends? Is this arbitrage legislative-regime dependent?',
    'Tax law analysis; comparison of Berkshire''s effective tax rate on retained vs. distributed capital; sensitivity to corporate tax regime changes.',
    'If tax arbitrage dominant: the constraint is contingent on fiscal policy and vulnerable to legislative change. If returns-based: constraint is more structurally stable. Affects both extractiveness assessment and mandatrophy resolution.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(tax_arbitrage_sustainability, empirical, 'Degree to which tax arbitrage sustains compounding advantage').

omega_variable(
    control_entrenchment_function,
    'To what extent does the no-dividend, share-buyback structure serve to entrench management control beyond the efficient capital allocation rationale?',
    'Governance analysis of voting share concentration; comparison with dividend-paying conglomerates of similar complexity; historical timing of policy changes relative to control threats.',
    'If control entrenchment is primary function: extractiveness rises significantly, converting rope to tangled_rope or snare. If capital efficiency is primary: extraction assessment holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(control_entrenchment_function, empirical, 'Degree to which policy entrenchs management control').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(berkshire_compounding_culture, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bh_compound_tr_t0, berkshire_compounding_culture, theater_ratio, 0, 0.15).
narrative_ontology:measurement(bh_compound_tr_t25, berkshire_compounding_culture, theater_ratio, 25, 0.28).
narrative_ontology:measurement(bh_compound_tr_t50, berkshire_compounding_culture, theater_ratio, 50, 0.35).

% Extraction over time
narrative_ontology:measurement(bh_compound_be_t0, berkshire_compounding_culture, base_extractiveness, 0, 0.12).
narrative_ontology:measurement(bh_compound_be_t25, berkshire_compounding_culture, base_extractiveness, 25, 0.32).
narrative_ontology:measurement(bh_compound_be_t50, berkshire_compounding_culture, base_extractiveness, 50, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(berkshire_compounding_culture, resource_allocation).
narrative_ontology:affects_constraint(berkshire_compounding_culture, long_term_capital_markets_structure).
narrative_ontology:affects_constraint(berkshire_compounding_culture, corporate_governance_voting_control).
narrative_ontology:affects_constraint(berkshire_compounding_culture, dividend_policy_optionality).

% DUAL FORMULATION NOTE:
% The Berkshire compounding mandate is downstream of broader capital markets structures (tax treatment of dividends, voting control mechanisms, institutional ownership norms) but represents a distinct organizational constraint. Its extractiveness reflects both the genuine efficiency gains from long-term reinvestment AND the governance entrenchment that perpetuates the policy beyond empirical justification.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(berkshire_compounding_culture, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
