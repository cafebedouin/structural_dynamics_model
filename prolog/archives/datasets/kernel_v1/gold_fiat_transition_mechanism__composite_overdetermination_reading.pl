% ============================================================================
% CONSTRAINT STORY: gold_fiat_transition_mechanism__composite_overdetermination_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_gold_fiat_transition_mechanism__composite_overdetermination_reading, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: gold_fiat_transition_mechanism__composite_overdetermination_reading
 *   human_readable: Gold-to-Fiat Transition (Composite Overdetermination Reading)
 *   domain: monetary_economics/political_economy/economic_history
 *
 * SUMMARY:
 *   The gold-to-fiat transition in the early 1970s is commonly narrated as a
 *   single causal event: Nixon's 1971 decision to end gold convertibility.
 *   The composite overdetermination reading challenges this singularity. The
 *   transition was not a unified causal rupture but the convergence of four
 *   independent structural changes, each of which was pushing toward fiat
 *   money independently: (1) telecommunications technology enabling instant
 *   capital flows across borders, making capital controls increasingly porous
 *   and the gold reserve auditable in real-time; (2) the collapse of Bretton
 *   Woods pegs as autonomous facts of international finance, driven by
 *   fundamental incompatibility between fixed exchange rates and independent
 *   monetary policies (Triffin dilemma); (3) labor bargaining power shifts in
 *   the 1960s creating wage-push inflation that contradicted the gold
 *   standard's requirement for stable price levels; (4) legal tender
 *   enforcement maturation in central banking doctrine and operational
 *   infrastructure, allowing fiat legitimacy to rest on institutional
 *   credibility rather than precious-metal backing. No single one of these
 *   was sufficient alone. But their convergence made the gold standard
 *   structurally untenable. Nixon Shock was the political recognition moment,
 *   not the causal locus. The constraint operates on multiple victim and
 *   beneficiary groups differently: gold-holding nations and fixed-income
 *   creditors faced extraction through the regime change; the US fiscal
 *   authority and constituencies benefiting from seigniorage and capital
 *   mobility captured gains; Bretton Woods-dependent nations experienced
 *   mixed effects (loss of coordination benefits, gain in policy autonomy);
 *   and organized central banking specialists successfully managed the
 *   transition toward floating-rate norms. The theater ratio increased during
 *   the transition (1960–1975) as the ritual of gold-backed legitimacy
 *   persisted even as its structural foundation eroded, then declined
 *   post-1975 as floating-rate norms matured and the performative content of
 *   gold reserves became explicit.
 *
 * KEY AGENTS:
 *   - US Fiscal Authority & Capital Mobility Constituency (institutional/arbitrage) — net beneficiary; captures seigniorage rights and escapes gold peg fiscal constraint
 *   - Gold Reserve Holders (powerless/trapped) — primary victims; trapped by policy change; maximum experienced extraction
 *   - Bretton Woods Dependent Nations (moderate/constrained) — mixed effects; lose coordination benefit but gain policy autonomy
 *   - Fixed-Income Creditors (powerless/trapped) — secondary victims; inflation erodes real claims; no exit from currency depreciation
 *   - Floating Exchange Rate Regime Coalition (organized/mobile) — transitional actors seeing scaffold; central banks, Treasury specialists managing regime change
 *   - Labor Movements (organized/identity_locked) — ambiguous position; wage-push power during transition period, but trapped within national jurisdictions; identity fused with national monetary sovereignty
 *   - Gold Standard Mythology Custodians (institutional/arbitrage) — maintain piton; continue to invoke gold as legitimacy anchor despite functional obsolescence
 *   - Analytical Observer (analytical/analytical) — risks naturalizing contingent institutional choice as monetary physics
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gold_fiat_transition_mechanism__composite_overdetermination_reading, 0.38).
domain_priors:suppression_score(gold_fiat_transition_mechanism__composite_overdetermination_reading, 0.42).
domain_priors:theater_ratio(gold_fiat_transition_mechanism__composite_overdetermination_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gold_fiat_transition_mechanism__composite_overdetermination_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(gold_fiat_transition_mechanism__composite_overdetermination_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(gold_fiat_transition_mechanism__composite_overdetermination_reading, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gold_fiat_transition_mechanism__composite_overdetermination_reading, tangled_rope).
narrative_ontology:human_readable(gold_fiat_transition_mechanism__composite_overdetermination_reading, "Gold-to-Fiat Transition (Composite Overdetermination Reading)").
narrative_ontology:topic_domain(gold_fiat_transition_mechanism__composite_overdetermination_reading, "monetary_economics/political_economy/economic_history").

domain_priors:requires_active_enforcement(gold_fiat_transition_mechanism__composite_overdetermination_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gold_fiat_transition_mechanism__composite_overdetermination_reading, 'ec3b5fc6-f5c9-4c00-9157-f6f2c585e3be').
narrative_ontology:cs_kernel_codification('ec3b5fc6-f5c9-4c00-9157-f6f2c585e3be', distributed).
narrative_ontology:cs_authority_grounding('ec3b5fc6-f5c9-4c00-9157-f6f2c585e3be', extraction).
narrative_ontology:cs_interpretation_layer_present('ec3b5fc6-f5c9-4c00-9157-f6f2c585e3be').
narrative_ontology:cs_reading_relation('ec3b5fc6-f5c9-4c00-9157-f6f2c585e3be', gold_fiat_transition_mechanism__automatic_constraint_reading, coexists_with).
narrative_ontology:cs_reading_relation('ec3b5fc6-f5c9-4c00-9157-f6f2c585e3be', gold_fiat_transition_mechanism__creditor_discipline_reading, coexists_with).
narrative_ontology:cs_axiom('ec3b5fc6-f5c9-4c00-9157-f6f2c585e3be', foundational, multiple_independent_structural_mechanisms_sufficient).
narrative_ontology:cs_axiom_status(multiple_independent_structural_mechanisms_sufficient, holdable).
narrative_ontology:cs_axiom_grounding('ec3b5fc6-f5c9-4c00-9157-f6f2c585e3be', multiple_independent_structural_mechanisms_sufficient, empirically_contingent).
narrative_ontology:cs_axiom('ec3b5fc6-f5c9-4c00-9157-f6f2c585e3be', foundational, nixons_shock_as_recognition_not_causation).
narrative_ontology:cs_axiom_status(nixons_shock_as_recognition_not_causation, holdable).
narrative_ontology:cs_axiom_grounding('ec3b5fc6-f5c9-4c00-9157-f6f2c585e3be', nixons_shock_as_recognition_not_causation, empirically_contingent).
narrative_ontology:cs_reference_frame('ec3b5fc6-f5c9-4c00-9157-f6f2c585e3be', unified_kernel_theory).
narrative_ontology:cs_drift_state('ec3b5fc6-f5c9-4c00-9157-f6f2c585e3be', contemporary_monetary_economics, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('ec3b5fc6-f5c9-4c00-9157-f6f2c585e3be', '2026-02-26T14:32:00Z').
narrative_ontology:cs_kernel_id(gold_fiat_transition_mechanism__composite_overdetermination_reading, gold_fiat_transition_mechanism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gold_fiat_transition_mechanism__composite_overdetermination_reading, us_fiscal_authority).
narrative_ontology:constraint_beneficiary(gold_fiat_transition_mechanism__composite_overdetermination_reading, capital_mobility_constituency).
narrative_ontology:constraint_beneficiary(gold_fiat_transition_mechanism__composite_overdetermination_reading, inflation_indexed_debtors).
narrative_ontology:constraint_victim(gold_fiat_transition_mechanism__composite_overdetermination_reading, gold_reserve_holders).
narrative_ontology:constraint_victim(gold_fiat_transition_mechanism__composite_overdetermination_reading, fixed_income_creditors).
narrative_ontology:constraint_victim(gold_fiat_transition_mechanism__composite_overdetermination_reading, bretton_woods_dependent_nations).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: GOLD RESERVE HOLDERS (SNARE) — Trapped by the escalating extraction: as US fiscal deficits mounted, gold reserves faced seizure or hair-cut through policy change. No exit option once reserves were in US custody. The regime change itself was an extraction mechanism — trapped assets, trapped position, maximum experienced chi.
constraint_indexing:constraint_classification(gold_fiat_transition_mechanism__composite_overdetermination_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: BRETTON WOODS DEPENDENT NATIONS (TANGLED ROPE) — Constrained exit (capital controls, trade integration, currency peg dependencies) but also benefited from Bretton Woods coordination (stable exchange rates, preferential access to capital). The transition imposed costs but also broke Bretton Woods constraints on their own fiscal autonomy. Mixed experience — extraction + some liberation.
constraint_indexing:constraint_classification(gold_fiat_transition_mechanism__composite_overdetermination_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: US FISCAL AUTHORITY & CAPITAL MOBILITY CONSTITUENCY (ROPE) — Net beneficiary (arbitrage). The regime change removed the gold peg constraint on US fiscal expansion and enabled seigniorage capture. Telecommunications technology and capital mobility reduced the cost of fiat enforcement. This agent experiences the constraint as coordination: the transition solved the coordination problem of how to allocate seigniorage rights in a post-Bretton Woods world.
constraint_indexing:constraint_classification(gold_fiat_transition_mechanism__composite_overdetermination_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: FLOATING EXCHANGE RATE REGIME COALITION (SCAFFOLD) — Organized agents (central banks, Treasury specialists, IMF architects) saw the transition as temporary friction toward a better equilibrium. The scaffold sunset was real: capital mobility norms and floating rate adoption gradually replaced the rigid Bretton Woods structure. Low extraction because this constituency had agency and saw an exit path (toward mature floating-rate regimes).
constraint_indexing:constraint_classification(gold_fiat_transition_mechanism__composite_overdetermination_reading, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: GOLD STANDARD MYTHOLOGY (PITON) — The intellectual tradition of gold-standard legitimacy persists through theater and inertia despite functional degradation. Central banks still hold gold reserves long after the peg was severed; monetary economists still reference gold as 'historical anchor' despite it performing no structural monetary function. Theater ratio high: the ritual persists without functional verification.
constraint_indexing:constraint_classification(gold_fiat_transition_mechanism__composite_overdetermination_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a long-historical/universal perspective, the gold standard was always incompatible with industrial-scale capital flows and modern fiscal state scope. The transition is portrayed as inevitable, like entropy increasing — a law of monetary physics rather than a contingent institutional arrangement. This reading risks naturalizing what the composite overdetermination reading reveals as a genuine contingency (multiple independent mechanisms had to converge; causality is not unified).
constraint_indexing:constraint_classification(gold_fiat_transition_mechanism__composite_overdetermination_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gold_fiat_transition_mechanism__composite_overdetermination_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(gold_fiat_transition_mechanism__composite_overdetermination_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(gold_fiat_transition_mechanism__composite_overdetermination_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(gold_fiat_transition_mechanism__composite_overdetermination_reading, TR),
    TR >= 0.70.

:- end_tests(gold_fiat_transition_mechanism__composite_overdetermination_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The transition involved real extraction (seizure of gold reserves' optionality, erosion of creditor claims, forced conversion to depreciating currency), but the magnitude is muted compared to pure extraction mechanisms (snares) because multiple offsetting effects occurred. Some beneficiaries (the US) captured large gains, but other victims (Bretton Woods dependents) gained policy autonomy. The composite reading's core insight — that extractiveness came from regime change, not from a single unified mechanism — justifies the moderate scoring. A higher score (0.5+) would suggest a single coherent extraction mechanism, which the overdetermination thesis denies. Suppression (0.42): Moderate. Capital controls, legal tender enforcement, and institutional inertia all constrained exit options for gold-holding nations and fixed-income creditors. But suppression was not maximally coercive — Bretton Woods was institutionalized negotiation, not physical seizure. The suppression requirement increased over the transition period (1960–1971) as pressure mounted, then declined post-1975 as floating regimes normalized and capital controls relaxed. Theater ratio (0.58): Moderate-high. Throughout the late Bretton Woods period (1960–1971), the gold peg ritual persisted despite mounting evidence of stress (gold drain, reserve speculation, forward market pressure). The theater reached peak around 1970 (officials performing confidence in the peg while knowing it was indefensible) and declined post-1975 as floating norms replaced the performative Bretton Woods fiction. The composite reading's staging of theater reflects the regime change moment as one where ritual and reality maximally diverged.
 *
 * PERSPECTIVAL GAP:
 *   The perspectives demonstrate maximum divergence across all six types. The gold-holding nations experience snare (trapped, no exit). The US beneficiary experiences rope (coordination solved). The floating-rate coalition experiences scaffold (temporary friction toward better equilibrium). The gold-standard mythology experiences piton (ritual persisting through inertia). Bretton Woods dependents experience tangled rope (mixed costs and benefits). The analytical observer risks mountain (naturalizing as monetary physics). This full hexarctic spread — all six types represented — is rare and diagnostically significant. It indicates that the constraint's structure is genuinely complex: no single causal mechanism suffices to explain the transition. The perspectival gaps reveal that the different readings of the 'gold-fiat transition kernel' are not just different interpretations of one event — they are describing genuinely different structural positions vis-à-vis the transition.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) values are derived from beneficiary/victim status and exit options. The US fiscal authority (beneficiary + arbitrage) derives d ≈ 0.10 (full beneficiary position). Gold-holding nations (victim + trapped) derive d ≈ 0.92 (full target). Bretton Woods dependents (mixed victim-beneficiary + constrained) derive d ≈ 0.55 (symmetric, some agency). The engine computes f(d) from these values and applies scope modulation (σ(S)=1.1 for continental, 1.2 for global) to produce effective extractiveness chi per each perspective. The powerless trapped perspective experiences maximum chi; the institutional arbitrage perspective experiences negative chi (subsidization). The composite reading's moderate epsilon reflects that no single directionality value dominates — multiple agents experience structurally distinct relationships to the constraint.
 *
 * MANDATROPHY ANALYSIS:
 *   The composite overdetermination reading resolves mandatrophy by denying the premise: there is no unified extraction mechanism to misidentify as coordination (or vice versa). Instead, the transition involved multiple structural changes — some coordinating (establishing floating-rate norms), some extractive (seizing gold reserves' optionality), some liberating (breaking capital controls), some constraining (forcing legal tender acceptance). The mandatrophy does not arise because we cannot distinguish type; it arises because the kernel itself is not singular. The automatic_constraint_reading and creditor_discipline_reading each propose a unified causal mechanism (automatic balance-of-payments adjustment, or creditor-enforced fiscal discipline). The composite reading denies both: these are post-hoc framings of what was genuinely overdetermined convergence. The mandatrophy is resolved not by choosing among the six types but by recognizing that the six-type spectrum appears precisely because no unified causal kernel exists.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_singularity_vs_overdetermination,
    'Was the gold-fiat transition a single unified causal event (one kernel, multiple readings of it) or genuinely overdetermined (multiple independent structural changes, no unified kernel)?',
    'Counterfactual historical analysis: remove one structural change (e.g., keep telecommunications absent but remove Bretton Woods pegs) and ask whether transition still occurred. If multiple independent changes were each sufficient, the transition is overdetermined, not kernel-singular.',
    'If overdetermined: the automatic_constraint_reading and creditor_discipline_reading misattribute causality to a non-existent unified event. The composite reading is correct — there is no single kernel, only convergence. If singular: one of the sibling readings has correctly identified the true causal kernel; the composite reading is a false decomposition.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_singularity_vs_overdetermination, conceptual, 'Whether transition is unified causal event or overdetermined convergence').

omega_variable(
    telecommunications_necessity_threshold,
    'What threshold of capital mobility (enabled by telecomm technology) was necessary for the gold standard to become unworkable? Was it a hard constraint or gradual?',
    'Archival analysis of capital flows before/after specific telecom infrastructure milestones (transatlantic cable upgrade 1956, international funds transfer protocols 1970s, SWIFT 1973). Correlation between telecom capacity expansion and pressure on gold reserves.',
    'If threshold was hard and specific: telecom change was a genuine structural enabler, not just a correlate. If gradual: gold standard could have persisted longer with different political choices (shifts causal weight toward creditor discipline vs tech determinism).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(telecommunications_necessity_threshold, empirical, 'Threshold of capital mobility technology necessary for gold standard incompatibility').

omega_variable(
    labor_bargaining_power_distributional_effect,
    'Did labor bargaining power shifts (mid-1960s wage pressure, wildcat strikes, decline of real wage growth post-1973) arise from labor''s autonomous organizing or from monetary authorities'' policy choices responding to gold drain?',
    'Comparison of labor militancy timing across countries with different monetary regimes (gold-standard-committed UK vs flexible-currency US); isolation of wage-push inflation as exogenous shock vs endogenous response to policy.',
    'If autonomous: labor power is an independent structural change, supporting composite reading. If policy-response: labor dynamics are downstream of monetary regime choice, suggesting simpler causal model (automatic_constraint_reading has deeper truth).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(labor_bargaining_power_distributional_effect, empirical, 'Whether labor bargaining shifts were autonomous or policy-responsive').

omega_variable(
    legal_tender_enforcement_maturation_timing,
    'When did legal tender enforcement infrastructure (currency monopoly enforcement, capital controls as binding constraint, fiat legitimacy narratives) reach maturity sufficient to sustain fiat money without gold backing? Was it pre-1971 or post-1975?',
    'Archival tracking of legal tender law revisions, currency monopoly enforcement mechanisms, taxation-acceptance protocols, and central bank operational independence doctrines. Identify discrete moments of infrastructure completion.',
    'If pre-1971: enforcement readiness preceded Nixon Shock, suggesting Shock was political choice, not structural necessity (creditor_discipline reading more plausible). If post-1975: enforcement infrastructure was built during transition, suggesting it was a structural requirement that took years to mature (composite reading confirmed — multiple systems had to mature in parallel).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legal_tender_enforcement_maturation_timing, empirical, 'Timeline of legal tender enforcement infrastructure maturation').

omega_variable(
    bretton_woods_collapse_counterfactual,
    'If US policymakers had chosen to defend the gold peg (higher seigniorage taxes, capital controls, fiscal contraction) rather than exit, could the Bretton Woods system have persisted into the 1980s?',
    'Simulation of counterfactual policy paths using economic models calibrated to 1960s parameters. Identify whether resource constraints or political economy prevented peg defense.',
    'If peg could have been defended: the transition was a choice, not a necessity. The different readings reflect different political commitments, not different causal mechanisms. If peg was undefendable: composite overdetermination reading is confirmed — structural forces converged making transition inevitable.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(bretton_woods_collapse_counterfactual, conceptual, 'Counterfactual defensibility of gold peg given 1960s constraints').

omega_variable(
    reading_as_kernel_revision_test,
    'Does the composite overdetermination reading itself reveal that the gold-fiat transition lacks a true kernel — that it is a presheaf of overdetermined factors rather than a singular commit that admits multiple readings?',
    'Axiom test: can the composite reading coexist with the automatic_constraint_reading under a single unified commitment framework? If not, the kernel itself is contested — different camps disagree on what the kernel IS, not just how to read it.',
    'If coexistence fails: this is not a kernel-and-readings structure. It is a genuine kernel contest. The committer frame itself must be revised. The constraint schema would need to declare ''no unified kernel'' as a status, not as a reading of one.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_as_kernel_revision_test, conceptual, 'Whether composite reading dissolves the kernel singularity assumption').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gold_fiat_transition_mechanism__composite_overdetermination_reading, 1960, 1980).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gftr_tr_t1960, gold_fiat_transition_mechanism__composite_overdetermination_reading, theater_ratio, 1960, 0.35).
narrative_ontology:measurement(gftr_tr_t1965, gold_fiat_transition_mechanism__composite_overdetermination_reading, theater_ratio, 1965, 0.44).
narrative_ontology:measurement(gftr_tr_t1970, gold_fiat_transition_mechanism__composite_overdetermination_reading, theater_ratio, 1970, 0.52).
narrative_ontology:measurement(gftr_tr_t1971, gold_fiat_transition_mechanism__composite_overdetermination_reading, theater_ratio, 1971, 0.58).
narrative_ontology:measurement(gftr_tr_t1975, gold_fiat_transition_mechanism__composite_overdetermination_reading, theater_ratio, 1975, 0.56).
narrative_ontology:measurement(gftr_tr_t1980, gold_fiat_transition_mechanism__composite_overdetermination_reading, theater_ratio, 1980, 0.48).

% Extraction over time
narrative_ontology:measurement(gftr_be_t1960, gold_fiat_transition_mechanism__composite_overdetermination_reading, base_extractiveness, 1960, 0.18).
narrative_ontology:measurement(gftr_be_t1965, gold_fiat_transition_mechanism__composite_overdetermination_reading, base_extractiveness, 1965, 0.28).
narrative_ontology:measurement(gftr_be_t1970, gold_fiat_transition_mechanism__composite_overdetermination_reading, base_extractiveness, 1970, 0.35).
narrative_ontology:measurement(gftr_be_t1971, gold_fiat_transition_mechanism__composite_overdetermination_reading, base_extractiveness, 1971, 0.42).
narrative_ontology:measurement(gftr_be_t1975, gold_fiat_transition_mechanism__composite_overdetermination_reading, base_extractiveness, 1975, 0.38).
narrative_ontology:measurement(gftr_be_t1980, gold_fiat_transition_mechanism__composite_overdetermination_reading, base_extractiveness, 1980, 0.32).

% Suppression requirement over time
narrative_ontology:measurement(gftr_su_t1960, gold_fiat_transition_mechanism__composite_overdetermination_reading, suppression_requirement, 1960, 0.25).
narrative_ontology:measurement(gftr_su_t1965, gold_fiat_transition_mechanism__composite_overdetermination_reading, suppression_requirement, 1965, 0.35).
narrative_ontology:measurement(gftr_su_t1970, gold_fiat_transition_mechanism__composite_overdetermination_reading, suppression_requirement, 1970, 0.42).
narrative_ontology:measurement(gftr_su_t1971, gold_fiat_transition_mechanism__composite_overdetermination_reading, suppression_requirement, 1971, 0.48).
narrative_ontology:measurement(gftr_su_t1975, gold_fiat_transition_mechanism__composite_overdetermination_reading, suppression_requirement, 1975, 0.42).
narrative_ontology:measurement(gftr_su_t1980, gold_fiat_transition_mechanism__composite_overdetermination_reading, suppression_requirement, 1980, 0.35).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gold_fiat_transition_mechanism__composite_overdetermination_reading, resource_allocation).
narrative_ontology:affects_constraint(gold_fiat_transition_mechanism__composite_overdetermination_reading, bretton_woods_regime_stability__automatic_constraint_reading).
narrative_ontology:affects_constraint(gold_fiat_transition_mechanism__composite_overdetermination_reading, creditor_discipline_monetary_policy__creditor_discipline_reading).
narrative_ontology:affects_constraint(gold_fiat_transition_mechanism__composite_overdetermination_reading, seigniorage_capture_fiscal_authority).
narrative_ontology:affects_constraint(gold_fiat_transition_mechanism__composite_overdetermination_reading, capital_mobility_constraint_erosion).

% DUAL FORMULATION NOTE:
% The gold-fiat transition is modeled as three separate constraint stories reflecting genuine kernel contest, not observer-dependent perspectives. The automatic_constraint_reading and creditor_discipline_reading are independent files with different claimed_types and omegas. The composite_overdetermination_reading argues that no unified kernel exists — the transition was overdetermined by multiple independent structural changes. Network links indicate that each reading influences but does not foreclose the others. If the composite reading's core omega (kernel_singularity_vs_overdetermination) resolves toward overdetermination, the sibling readings become analytically sound descriptions of contributing mechanisms but epistemically incorrect attributions of primary causality.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(gold_fiat_transition_mechanism__composite_overdetermination_reading, organized, 0.48).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
