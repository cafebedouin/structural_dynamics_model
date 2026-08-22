% ============================================================================
% CONSTRAINT STORY: structural_adjustment_conditionalities__creditor_coordination_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_structural_adjustment_conditionalities__creditor_coordination_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_axiom_grounding/3,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:cs_created_at/2,
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: structural_adjustment_conditionalities__creditor_coordination_reading
 *   human_readable: Structural Adjustment Conditionalities (Creditor Coordination Reading)
 *   domain: economic/political/international
 *
 * SUMMARY:
 *   Under this reading, structural adjustment conditionalities attached to
 *   IMF and World Bank lending are presented as a necessary coordination
 *   mechanism. The core problem they solve: debtor states need market access
 *   but creditors cannot verify creditworthiness without external monitoring.
 *   Conditionalities provide the verification — the state commits to
 *   IMF-monitored fiscal targets, which restore market confidence, which
 *   enables borrowing at sustainable rates. This reading frames the burden of
 *   adjustment (public sector cuts, subsidy elimination, privatization) as
 *   the necessary price of coordination, not as extractive punishment. The
 *   beneficiaries are international capital markets (which receive assurance
 *   of repayment) and future taxpayers (who inherit lower debt). The victims
 *   are public sector workers and subsidy-dependent populations whose income
 *   the adjustment eliminates. The claim/metric gap is deliberate: the
 *   constraint is CLAIMED as a Rope (genuine coordination function) while the
 *   authored metrics describe substantial suppression (0.71) and rising
 *   theater (0.44 by 2024) — the engine measures whether the coordination
 *   reading's internal structure holds or whether the suppression and theater
 *   indicate that coordination is the cover story for a different structure.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(structural_adjustment_conditionalities__creditor_coordination_reading, 0.62).
domain_priors:suppression_score(structural_adjustment_conditionalities__creditor_coordination_reading, 0.71).
domain_priors:theater_ratio(structural_adjustment_conditionalities__creditor_coordination_reading, 0.44).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(structural_adjustment_conditionalities__creditor_coordination_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(structural_adjustment_conditionalities__creditor_coordination_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(structural_adjustment_conditionalities__creditor_coordination_reading, theater_ratio, 0.44).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(structural_adjustment_conditionalities__creditor_coordination_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(structural_adjustment_conditionalities__creditor_coordination_reading, resistance, 0.73).

% --- Constraint claim ---
narrative_ontology:constraint_claim(structural_adjustment_conditionalities__creditor_coordination_reading, rope).
narrative_ontology:human_readable(structural_adjustment_conditionalities__creditor_coordination_reading, "Structural Adjustment Conditionalities (Creditor Coordination Reading)").
narrative_ontology:topic_domain(structural_adjustment_conditionalities__creditor_coordination_reading, "economic/political/international").

domain_priors:requires_active_enforcement(structural_adjustment_conditionalities__creditor_coordination_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(structural_adjustment_conditionalities__creditor_coordination_reading, '8c7dbacf-2fe9-4336-8ecb-ad8c0c77719b').
narrative_ontology:cs_kernel_codification('8c7dbacf-2fe9-4336-8ecb-ad8c0c77719b', formalized).
narrative_ontology:cs_authority_grounding('8c7dbacf-2fe9-4336-8ecb-ad8c0c77719b', extraction).
narrative_ontology:cs_interpretation_layer_present('8c7dbacf-2fe9-4336-8ecb-ad8c0c77719b').
narrative_ontology:cs_reading_relation('8c7dbacf-2fe9-4336-8ecb-ad8c0c77719b', structural_adjustment_conditionalities__debtor_extraction_reading, coexists_with).
narrative_ontology:cs_reading_relation('8c7dbacf-2fe9-4336-8ecb-ad8c0c77719b', structural_adjustment_conditionalities__hybrid_selectivity_reading, influences).
narrative_ontology:cs_axiom('8c7dbacf-2fe9-4336-8ecb-ad8c0c77719b', foundational, market_confidence_requires_credible_fiscal_commitment).
narrative_ontology:cs_axiom_status(market_confidence_requires_credible_fiscal_commitment, holdable).
narrative_ontology:cs_axiom_grounding('8c7dbacf-2fe9-4336-8ecb-ad8c0c77719b', market_confidence_requires_credible_fiscal_commitment, empirically_contingent).
narrative_ontology:cs_axiom('8c7dbacf-2fe9-4336-8ecb-ad8c0c77719b', secondary, conditionalities_are_optimal_credibility_mechanism).
narrative_ontology:cs_axiom_status(conditionalities_are_optimal_credibility_mechanism, holdable).
narrative_ontology:cs_axiom_grounding('8c7dbacf-2fe9-4336-8ecb-ad8c0c77719b', conditionalities_are_optimal_credibility_mechanism, instrumental).
narrative_ontology:cs_reference_frame('8c7dbacf-2fe9-4336-8ecb-ad8c0c77719b', market_access_through_verified_discipline).
narrative_ontology:cs_drift_state('8c7dbacf-2fe9-4336-8ecb-ad8c0c77719b', contemporary_post_2008, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('8c7dbacf-2fe9-4336-8ecb-ad8c0c77719b', '').
narrative_ontology:cs_kernel_id(structural_adjustment_conditionalities__creditor_coordination_reading, structural_adjustment_conditionalities).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(structural_adjustment_conditionalities__creditor_coordination_reading, international_capital_markets).
narrative_ontology:constraint_beneficiary(structural_adjustment_conditionalities__creditor_coordination_reading, future_taxpayers).
narrative_ontology:constraint_victim(structural_adjustment_conditionalities__creditor_coordination_reading, public_sector_workers).
narrative_ontology:constraint_victim(structural_adjustment_conditionalities__creditor_coordination_reading, subsidy_dependent_populations).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(structural_adjustment_conditionalities__creditor_coordination_reading, debtor_government).
narrative_ontology:constraint_beneficiary(structural_adjustment_conditionalities__creditor_coordination_reading, domestic_capitalists).
narrative_ontology:constraint_victim(structural_adjustment_conditionalities__creditor_coordination_reading, debtor_government).
narrative_ontology:constraint_victim(structural_adjustment_conditionalities__creditor_coordination_reading, domestic_capitalists).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Designs and enforces conditionality frameworks tied to lending. Sets the terms that debtor governments must meet: fiscal deficit limits, privatization schedules, subsidy elimination, labor market liberalization. Justifies these as prerequisites for market confidence and sustainable debt repayment. Controls the release of funds and publicly certifies compliance, thereby shaping market perception and creditworthiness ratings.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__creditor_coordination_reading, imf_world_bank, agenda_setter,
    institutional, generational, arbitrage, global).

% Capital markets benefit from conditionalities by receiving assurance that debtor states will prioritize debt service over domestic spending. Conditionalities reduce the perceived risk of default and allow markets to price debt at lower spreads. Market confidence is maintained through visible enforcement — markets read compliance certification as signal of state fiscal discipline.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__creditor_coordination_reading, international_capital_markets, beneficiary,
    institutional, biographical, arbitrage, global).

% Faces a coordination problem: it needs external financing to avoid immediate crisis, but markets will not lend without assurance of repayment. Conditionalities solve this by providing a credible commitment device — the government surrenders discretion over spending, which credibly signals it will prioritize debt service. The government benefits from restored market access and lower borrowing costs. It bears the cost of the policy changes themselves.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__creditor_coordination_reading, debtor_government, payer,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(structural_adjustment_conditionalities__creditor_coordination_reading, debtor_government, beneficiary).

% Experience wage freezes, hiring freezes, benefit cuts, and pension reforms as direct conditionality consequences. Public sector employment is often the largest available formal employment for working-class populations; conditionalities reduce its size and remuneration. Their exit options are limited to informal work, emigration (where possible), or political resistance. Conditionalities target inefficient public sectors, but the efficiency gain is extracted from worker compensation.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__creditor_coordination_reading, public_sector_workers, payer,
    powerless, biographical, constrained, national).

% Depend on food, fuel, transport, or medicine subsidies maintained by the state. Conditionalities require subsidy elimination to reduce fiscal deficits. These populations cannot absorb price shocks; they lack savings, cannot switch to alternatives, and have no exit. The transition burden falls hardest on those with zero ability to absorb it. Conditionalities are justified as removing market distortions, but the distortion they remove benefits this population.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__creditor_coordination_reading, subsidy_dependent_populations, payer,
    powerless, immediate, trapped, national).

% Benefit from conditionalities because lower debt service burdens reduce the fiscal obligations they will inherit. A government forced to run primary surpluses and reduce debt-to-GDP ratios leaves a less leveraged state for the next generation. They cannot negotiate or exit; they receive the benefit or burden passively.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__creditor_coordination_reading, future_taxpayers, beneficiary,
    powerless, generational, trapped, national).

% Emerging creditors (China, regional development banks, Gulf sovereigns) offer lending without conditionalities, but on terms that may include collateral, resource concessions, or geopolitical alignment. They are excluded from the formal coordination mechanism because the IMF/World Bank conditionality system pre-sets the terms. Their non-interference preserves the coordination framework.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__creditor_coordination_reading, alternative_creditors, excluded,
    institutional, biographical, constrained, global).

% Benefit from privatization of state enterprises (conditionality requirement) and from labor market liberalization, acquiring assets at distressed prices and accessing cheaper labor. They pay through increased tax compliance requirements and reduced subsidy capture that had previously benefited selected sectors. Their exit is arbitrage into other markets or alternative investment jurisdictions.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__creditor_coordination_reading, domestic_capitalists, beneficiary,
    powerful, biographical, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(structural_adjustment_conditionalities__creditor_coordination_reading, domestic_capitalists, payer).

% Analyze whether conditionalities achieve their stated goals (market confidence, sustainable debt repayment) and whether the policy mix is causally optimal or ideologically driven. They see the constraint from outside all participating seats and can measure outcomes against predictions.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__creditor_coordination_reading, development_economists, observer,
    analytical, biographical, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(structural_adjustment_conditionalities__creditor_coordination_reading, international_capital_markets).
narrative_ontology:fixing_cost_class(structural_adjustment_conditionalities__creditor_coordination_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the market-access paradox: a debtor state cannot access capital markets without credible commitment to fiscal discipline, but markets cannot observe discipline without surrendering control to external verification. Conditionalities provide the mechanism: the state commits to IMF-monitored targets that prove discipline, which restores market confidence, which allows borrowing at lower cost. The coordination solves a genuine information and commitment problem.
% TRANSFER_FUNCTION: Moves fiscal adjustment burden (wage cuts, subsidy removal, tax increases, public asset sales) from future debt service onto current workers and vulnerable populations. The real transfer is from populations whose income derives from public sector employment or subsidies to (1) international capital markets (via lower risk premiums), (2) future taxpayers (via reduced debt), and (3) domestic capitalists acquiring privatized assets. The mechanism is the policy conditionalities attached to the lending.
% ABSENT_VOICES: Workers in sectors targeted for privatization (utilities, telecoms, airlines) whose jobs and terms depend on public ownership. Populations consuming subsidized goods who would be excluded from negotiation over subsidy removal. Communities whose public services would be scaled back. Political voices opposing market liberalization. These groups are not in the conversation; their objections to the conditionality terms are not represented in the loan negotiation.
% DISAPPEARANCE_RATIONALE: If structural adjustment conditionalities disappeared, debtor states would have access to external financing without submitting to IMF discipline — they would retain domestic fiscal autonomy. Public sector employment would likely stabilize or grow, subsidy structures would persist longer, and privatization would slow. International capital markets would price emerging-market debt at higher spreads due to increased default risk. Debtor states' macroeconomic policies would reorient toward political constituencies rather than creditor preferences. The constraint's disappearance would fundamentally reshape state capacity and spending distribution.
% FOUNDING_PROBLEM: The 1970s–1980s debt crisis: many developing states borrowed heavily in dollars, encountered sudden interest rate shocks and commodity price collapses, and faced immediate illiquidity. They lacked the market access to roll over debt or attract new lending without assurance to creditors that they would repay. Creditors needed a commitment mechanism to believe the debtor state would prioritize debt service over spending.
% FOUNDING_PROBLEM_CORROBORATION: The IMF and World Bank attest the problem persists: emerging markets still face sudden capital flight, creditor preferences for fiscal discipline remain constant, and market spreads still respond to fiscal signals. Independent economists (Stiglitz, Easterly, Rodrik) attest the founding problem is substantially solved in many cases but conditionalities persist and have expanded beyond the original crisis prevention rationale into permanent governance frameworks. Academic and policy debate contests whether the problem-set driving the founding conditions still justifies the original policy mix.
narrative_ontology:disappearance_verdict(structural_adjustment_conditionalities__creditor_coordination_reading, world_rearranges).
narrative_ontology:founding_problem_status(structural_adjustment_conditionalities__creditor_coordination_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(structural_adjustment_conditionalities__creditor_coordination_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(structural_adjustment_conditionalities__creditor_coordination_reading, 'none', 1).
narrative_ontology:epsilon_provenance(structural_adjustment_conditionalities__creditor_coordination_reading, 0.62, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(structural_adjustment_conditionalities__creditor_coordination_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(structural_adjustment_conditionalities__creditor_coordination_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(structural_adjustment_conditionalities__creditor_coordination_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is 0.62 at interval end because the beneficiary structure (international capital, future taxpayers) is real but the burden distribution is asymmetric — the benefits accrue to abstract future actors and global capital, while costs are concentrated on present workers. The trajectory rises slightly from 1980 (0.52) through 2000 (0.62) and plateaus thereafter, reflecting the institutionalization of conditionality frameworks: initially crisis-driven and genuinely bounded by immediate needs, by 2000 conditionalities had become permanent and ideologically entrenched, increasingly distant from the original debt crisis. Suppression is high (0.71) and stable because the constraint's enforcement depends on creditor control of lending access — debtor states cannot refuse without losing the financing they need. Rising theater (0.25 to 0.44) reflects a documented drift: the original conditionalities focused on macroeconomic stabilization (interest rate policy, reserve management); by 2010–2024 they had expanded into social policy conditionalities (health, education spending targets) that are harder to justify as market-necessary and more clearly ideological. The measurement series tracks this institutional drift on one shared time grid.
 *
 * PERSPECTIVAL GAP:
 *   The IMF/World Bank seat (agenda-setter) and the capital market seat (beneficiary) experience this as coordination: the rules exist because markets demand credibility, and the rules work (markets lend). The debtor-government seat sits at the hinge: it genuinely needs the financing, so compliance is voluntary in the sense that it chose not to default — but exit is illusory (default means immediate economic collapse). The public-sector-worker seat and subsidy-dependent seat (payers) experience pure extraction: they did not choose the policy, did not benefit from the lending, and cannot exit. From the payer seats the constraint appears as suppressed coercion; from the agenda-setter and beneficiary seats it appears as necessary discipline. The engine should compute this divergence from the structural data (power atoms, exit options) — the authored claim (Rope) and the authored metrics (high suppression, rising theater) do not pre-adjudicate which seat's perception the engine will validate.
 *
 * DIRECTIONALITY LOGIC:
 *   International capital markets sit at the beneficiary end (d near 0.0): they receive the assurance and lower risk premiums without bearing adjustment costs. Future taxpayers sit near beneficiary (d ~0.2) despite being powerless: they benefit passively from reduced debt but cannot negotiate. The debtor government is dual-positioned: as beneficiary (it gets financing it cannot access otherwise), but with highly constrained exit — if it rejects conditionalities it loses the financing and faces immediate crisis, so acceptance is extractive despite the stated benefit. Estimate d ~0.65 for debtor government: benefits in principle, but constrained exit makes the benefit coercive. Public-sector workers and subsidy-dependent populations sit at the target end (d near 1.0): they bear the adjustment costs (wage cuts, subsidy removal) without negotiating, their exit is trapped or identity-locked (public sector employment is often inherited, regional, or tied to political patronage), and they collect nothing from the arrangement. The spatial scope (global for markets and conditionalities, national for implementation) makes verification harder and amplifies effective extraction for the powerless payers.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (creditor confidence in repayment) was genuinely live in 1980–1990 (genuine debt crisis, real risk of cascade defaults). By 2010 the problem had substantially shifted: most countries had reestablished market access, private capital flows dwarfed official flows, and the binding constraint on borrowing was no longer credibility but pricing. Conditionalities persisted and expanded despite the founding problem's attenuation, suggesting institutional inertia. Theater ratio rise (0.25 to 0.44) is diagnostic: the constraint increasingly defends itself rather than solving the original problem. This reading avoids the mandatrophy failure by acknowledging the shift (founding_problem_status: live, but contested) — the disagreement is whether the persistent problem justifies the persistent constraint or whether the constraint has become Piton-like (administratively maintained for reasons unrelated to function). The Rope classification depends on the coordination reading holding — if the subsequent readings' analyses of suppression and theater are correct, the computation should surface the type divergence, triggering the claim/metric gap that the corpus measures.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    coordination_vs_coercion_boundary,
    'Is the suppression measured in this constraint (0.71) a structural feature of the coordination mechanism itself, or evidence that the suppression is doing the actual work and coordination is the stated justification?',
    'Counterfactual comparison: observational studies of cases where debtor states negotiated conditional frameworks with lenders who had less enforcement power (Paris Club rescheduling, bilateral creditors). Did outcomes differ substantially? Did market confidence require the high suppression, or did the suppression permit lower-cost policy adjustment for creditors?',
    'High suppression is consistent with both Rope and Snare: in a Rope it is the price of commitment credibility; in a Snare it is the mechanism by which extraction persists. The directionality divergence (beneficiary vs. payer) will resolve the boundary if computed faithfully from the stakeholder data.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(coordination_vs_coercion_boundary, empirical, 'Whether suppression is instrumental to coordination or evidence of coercive extraction.').

omega_variable(
    theater_ratio_expansion_driver,
    'Why did theater_ratio rise from 0.25 (1980) to 0.44 (2024)? Is this drift evidence that the original coordination function has atrophied and conditionalities now persist through institutional inertia (Piton diagnosis), or is it evidence that the coordination function has expanded into social policy domains (health, education, governance) that are now as important as macro stabilization?',
    'Content analysis of conditionality frameworks across decades: what percentage of conditions in 1990 addressed macroeconomic stabilization vs. structural reform? What percentage in 2024? Paired with outcome data: do the social-policy conditions correlate with improved social outcomes (supporting expansion thesis) or are they theater for ideological preference (supporting Piton thesis)?',
    'Expansion thesis (legitimate, supported by Rope reading): conditionalities evolved to address deeper structural problems, and the increased proportion of social conditions reflects deepening understanding of what drives sustainable development. Piton thesis (institutional inertia, supported by sibling readings): the original problem was solved by 2000, but the institution persisted and filled its time with increasingly tangential conditions to maintain its relevance. The ambiguity is irreducible without outcome data.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(theater_ratio_expansion_driver, empirical, 'Whether theater rise reflects expanded legitimate function or institutional inertia.').

omega_variable(
    kernel_reading_contestation,
    'This constraint instantiates ONE reading of the structural_adjustment_conditionalities kernel. The sibling reading ''debtor_extraction_reading'' instantiates the same kernel as a Snare. Both readings share the observable arrangement (policy conditions, lending terms, stated goals) but differ in how they attribute beneficiary structure and suppression function. What would constitute evidence that one reading is correct and the other is false?',
    'Outcome analysis: track countries that underwent structural adjustment over 1980–2024 and measure whether the predicted outcomes of each reading materialized. Rope reading predicts: restored market confidence (testable: compare spreads before/after), sustainable debt reduction (testable: debt-to-GDP trends), and long-term growth recovery. Snare reading predicts: persistent debt despite adjustment (testable: debt-to-GDP ratios), market confidence failing to translate to lower-cost borrowing (testable: spreads vs. compliance), and extraction flowing to creditors via higher risk premiums despite stated coordination. Each reading makes falsifiable predictions; outcomes should diverge.',
    'This is the foundational omega for kernel readings: neither reading is self-justifying. The corpus exists to measure which reading''s structural analysis holds. If the evidence supports the Rope reading, conditionalities should compute as Rope from the engine. If the evidence supports the Snare reading, they should compute as Snare despite the authored claim.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'What evidence would falsify this reading relative to its siblings?').

omega_variable(
    suppression_structure_vs_internalization,
    'The suppression measure (0.71) reflects the constraining of debtor-state policy choices via creditor control of lending access. Is this suppression structural (external — the state cannot access lending without compliance) or internalized (the state has adopted the creditor''s policy framework as its own legitimate framework)?',
    'Political discourse analysis: Do debtor-state policy makers defend conditionality policies as imposed constraints they resent, or as adopted policies they endorse? Survey data on policy-maker and affected-population attitudes. Comparative analysis: do states that escape creditor control (via alternative financing, commodity booms, debt reduction) maintain the same policies, or do they revert? If reversion is swift, suppression was structural; if policies persist, some internalization occurred.',
    'If suppression is purely structural, the constraint''s persistence is contingent on creditor power remaining high. If suppression is internalized, the constraint persists even when structural power decreases because the target population has adopted the framework as legitimate. Internalized suppression is harder to escape and is a diagnosis separate from the formal classification (but affects the post-exit trajectory).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_structure_vs_internalization, empirical, 'Whether the measured suppression is structural imposition or internalized framework adoption.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(structural_adjustment_conditionalities__creditor_coordination_reading, 1980, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(stru_tr_t1980, structural_adjustment_conditionalities__creditor_coordination_reading, theater_ratio, 1980, 0.25).
narrative_ontology:measurement(stru_tr_t1990, structural_adjustment_conditionalities__creditor_coordination_reading, theater_ratio, 1990, 0.32).
narrative_ontology:measurement(stru_tr_t2000, structural_adjustment_conditionalities__creditor_coordination_reading, theater_ratio, 2000, 0.38).
narrative_ontology:measurement(stru_tr_t2010, structural_adjustment_conditionalities__creditor_coordination_reading, theater_ratio, 2010, 0.42).
narrative_ontology:measurement(stru_tr_t2020, structural_adjustment_conditionalities__creditor_coordination_reading, theater_ratio, 2020, 0.44).
narrative_ontology:measurement(stru_tr_t2024, structural_adjustment_conditionalities__creditor_coordination_reading, theater_ratio, 2024, 0.44).

% Extraction over time
narrative_ontology:measurement(stru_be_t1980, structural_adjustment_conditionalities__creditor_coordination_reading, base_extractiveness, 1980, 0.52).
narrative_ontology:measurement(stru_be_t1990, structural_adjustment_conditionalities__creditor_coordination_reading, base_extractiveness, 1990, 0.58).
narrative_ontology:measurement(stru_be_t2000, structural_adjustment_conditionalities__creditor_coordination_reading, base_extractiveness, 2000, 0.62).
narrative_ontology:measurement(stru_be_t2010, structural_adjustment_conditionalities__creditor_coordination_reading, base_extractiveness, 2010, 0.64).
narrative_ontology:measurement(stru_be_t2020, structural_adjustment_conditionalities__creditor_coordination_reading, base_extractiveness, 2020, 0.62).
narrative_ontology:measurement(stru_be_t2024, structural_adjustment_conditionalities__creditor_coordination_reading, base_extractiveness, 2024, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(stru_su_t1980, structural_adjustment_conditionalities__creditor_coordination_reading, suppression_requirement, 1980, 0.58).
narrative_ontology:measurement(stru_su_t1990, structural_adjustment_conditionalities__creditor_coordination_reading, suppression_requirement, 1990, 0.65).
narrative_ontology:measurement(stru_su_t2000, structural_adjustment_conditionalities__creditor_coordination_reading, suppression_requirement, 2000, 0.7).
narrative_ontology:measurement(stru_su_t2010, structural_adjustment_conditionalities__creditor_coordination_reading, suppression_requirement, 2010, 0.71).
narrative_ontology:measurement(stru_su_t2020, structural_adjustment_conditionalities__creditor_coordination_reading, suppression_requirement, 2020, 0.71).
narrative_ontology:measurement(stru_su_t2024, structural_adjustment_conditionalities__creditor_coordination_reading, suppression_requirement, 2024, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(structural_adjustment_conditionalities__creditor_coordination_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(structural_adjustment_conditionalities__creditor_coordination_reading, 0.18).
narrative_ontology:affects_constraint(structural_adjustment_conditionalities__creditor_coordination_reading, structural_adjustment_conditionalities__debtor_extraction_reading).
narrative_ontology:affects_constraint(structural_adjustment_conditionalities__creditor_coordination_reading, structural_adjustment_conditionalities__hybrid_selectivity_reading).

% DUAL FORMULATION NOTE:
% structural_adjustment_conditionalities is a contested kernel with three structural readings: this reading (creditor_coordination_reading) frames conditionalities as solving a genuine coordination problem (market-confidence commitment) at the cost of distributional asymmetry; debtor_extraction_reading frames them as primarily extractive instruments dressed as coordination; hybrid_selectivity_reading frames them as genuinely coordinating for powerless debtors but selectively waived for geopolitically strategic ones. All three readings instantiate the same institutional arrangement and share observable data. The readings diverge in their attribution of suppression (necessary commitment mechanism vs. coercive control vs. hidden coercion) and beneficiary structure (international capital + future taxpayers vs. creditors only vs. creditors + geopolitical allies). Each reading computes a different constraint type from the shared stakeholder and structural data; the corpus's role is to measure which reading's structural analysis the empirical outcomes support. All three must be present as sibling files; pairwise network edges link them.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(structural_adjustment_conditionalities__creditor_coordination_reading, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
