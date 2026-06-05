% ============================================================================
% CONSTRAINT STORY: transition_causality__overdetermined_collapse_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_transition_causality__overdetermined_collapse_reading, []).

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
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
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
 *   constraint_id: transition_causality__overdetermined_collapse_reading
 *   human_readable: Bretton Woods Collapse: Overdetermined Structural Inevitability Reading
 *   domain: international_finance/monetary_economics/political_economy
 *
 * SUMMARY:
 *   The Bretton Woods system collapsed in August 1971 when President Nixon
 *   announced the suspension of gold convertibility. This constraint story
 *   instantiates ONE READING of the contested kernel 'transition_causality':
 *   the OVERDETERMINED COLLAPSE READING, which argues the transition was
 *   structurally inevitable due to multiple reinforcing contradictions
 *   converging on the same terminal state. This reading opposes two sibling
 *   readings: the CONTINGENT_CHOICE_READING (the transition was a policy
 *   decision that could have been avoided with different choices) and the
 *   HYBRID_TRIGGER_READING (structural contradictions accumulated but
 *   required contingent trigger events to actualize collapse). The
 *   overdetermined reading treats the Bretton Woods system as subject to the
 *   Triffin Dilemma (a mountain constraint) plus at least three additional
 *   independent structural contradictions: U.S. monetary-fiscal trilemma
 *   (Vietnam War spending + domestic inflation vs. gold standard commitment),
 *   gold reserve depletion (U.S. gold supply insufficient to meet redemption
 *   claims since mid-1960s), and Third World capital flight under fixed
 *   rates. The reading's core claim: NO policy choice available to U.S. or
 *   allied leadership could have prevented collapse given the accumulated
 *   contradictions. The system was overdetermined to fail.
 *
 * KEY AGENTS:
 *   - U.S. Federal Reserve and Treasury: Institutional actor (institutional/arbitrage) — faces impossible trilemma between fixed peg, independent monetary policy, and capital mobility. Arbitrage available (abandon the system) but high political cost.
 *   - Bretton Woods institutional faith: Victim (powerless/trapped) — abstract institutional commitment to gold standard and fixed rates; no internal mechanism for self-revision once contradictions became visible.
 *   - Developing economies: Victims (powerless/trapped) — locked into fixed-rate regime with capital flight penalties and policy constraints imposed by IMF conditionality. No exit mechanism except regime collapse.
 *   - Peripheral reserve currency states (Britain, France): Victims (moderate/constrained) — face balance-of-payments crises; constrained by defending peg (domestic deflation) or losing reserve status (policy constraint). High-cost exit options (devaluation, capital controls).
 *   - The Triffin Dilemma itself: Structural contradiction (analytical/analytical) — the logical irresolubility of a national currency serving as international reserve. Treated as a natural law in this reading.
 *   - Analytical observer: (analytical/analytical) — sees overdetermined collapse where multiple causal pathways converge on terminal state; counterfactual viability near-zero.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(transition_causality__overdetermined_collapse_reading, 0.62).
domain_priors:suppression_score(transition_causality__overdetermined_collapse_reading, 0.68).
domain_priors:theater_ratio(transition_causality__overdetermined_collapse_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(transition_causality__overdetermined_collapse_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(transition_causality__overdetermined_collapse_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(transition_causality__overdetermined_collapse_reading, theater_ratio, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(transition_causality__overdetermined_collapse_reading, snare).
narrative_ontology:human_readable(transition_causality__overdetermined_collapse_reading, "Bretton Woods Collapse: Overdetermined Structural Inevitability Reading").
narrative_ontology:topic_domain(transition_causality__overdetermined_collapse_reading, "international_finance/monetary_economics/political_economy").

domain_priors:requires_active_enforcement(transition_causality__overdetermined_collapse_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(transition_causality__overdetermined_collapse_reading, '12afea33-a3f5-43f1-86b0-d2badaca757f').
narrative_ontology:cs_kernel_codification('12afea33-a3f5-43f1-86b0-d2badaca757f', distributed).
narrative_ontology:cs_authority_grounding('12afea33-a3f5-43f1-86b0-d2badaca757f', distributed).
narrative_ontology:cs_reading_relation('12afea33-a3f5-43f1-86b0-d2badaca757f', transition_causality__contingent_choice_reading, forecloses).
narrative_ontology:cs_reading_relation('12afea33-a3f5-43f1-86b0-d2badaca757f', transition_causality__hybrid_trigger_reading, forecloses).
narrative_ontology:cs_axiom('12afea33-a3f5-43f1-86b0-d2badaca757f', foundational, structural_contradictions_suffice_for_collapse).
narrative_ontology:cs_axiom_status(structural_contradictions_suffice_for_collapse, holdable).
narrative_ontology:cs_axiom_grounding('12afea33-a3f5-43f1-86b0-d2badaca757f', structural_contradictions_suffice_for_collapse, empirically_contingent).
narrative_ontology:cs_axiom('12afea33-a3f5-43f1-86b0-d2badaca757f', foundational, counterfactual_viability_near_zero).
narrative_ontology:cs_axiom_status(counterfactual_viability_near_zero, holdable).
narrative_ontology:cs_axiom_grounding('12afea33-a3f5-43f1-86b0-d2badaca757f', counterfactual_viability_near_zero, empirically_contingent).
narrative_ontology:cs_reference_frame('12afea33-a3f5-43f1-86b0-d2badaca757f', bretton_woods_institutional_stability).
narrative_ontology:cs_drift_state('12afea33-a3f5-43f1-86b0-d2badaca757f', id_1971_collapse, gap(codification_collapse, severe, true)).
narrative_ontology:cs_created_at('12afea33-a3f5-43f1-86b0-d2badaca757f', '2026-02-26T14:32:18Z').
narrative_ontology:cs_kernel_id(transition_causality__overdetermined_collapse_reading, transition_causality).

% --- Structural relationships ---
narrative_ontology:constraint_victim(transition_causality__overdetermined_collapse_reading, fixed_rate_regime_participants).
narrative_ontology:constraint_victim(transition_causality__overdetermined_collapse_reading, developing_economies).
narrative_ontology:constraint_victim(transition_causality__overdetermined_collapse_reading, gold_standard_believers).
narrative_ontology:constraint_victim(transition_causality__overdetermined_collapse_reading, bretton_woods_institutional_faith).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DEVELOPING ECONOMIES (SNARE) — Locked into fixed-rate regime with no exit mechanism. Capital flight, foreign exchange depletion, and domestic inflation create a structural trap. Alternatives (floating rates, capital controls) are suppressed by IMF conditionality and institutional pressure. Maximum experienced extraction through seigniorage drain and policy constraint.
constraint_indexing:constraint_classification(transition_causality__overdetermined_collapse_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: PERIPHERAL RESERVE CURRENCY STATES (SNARE) — Constrained by balance-of-payment crises and reserve depletion. The Bretton Woods system forces choices between defending the peg (via domestic deflation and unemployment) and losing reserve status. Both paths extract from domestic populations. Exit options are high-cost (currency devaluation, capital controls) and politically dangerous.
constraint_indexing:constraint_classification(transition_causality__overdetermined_collapse_reading, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 3: TRIFFIN DILEMMA AS NATURAL LAW (MOUNTAIN) — The structural contradiction between the dollar's role as both a national currency (subject to inflation) and international reserve (requiring stability) is logically irresoluble within the fixed-rate framework. This reading treats the dilemma as a natural law: no amount of political will or coordination can square this circle. The only question is timing.
constraint_indexing:constraint_classification(transition_causality__overdetermined_collapse_reading, mountain,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: U.S. MONETARY-FISCAL CONTRADICTION (SNARE) — The U.S. faces an impossible trilemma: maintain fixed peg to gold, conduct independent monetary policy, and allow capital mobility. Vietnam War spending, Great Society programs, and corporate profit cycles force the choice: abandon gold backing (exit the system) or accept domestic deflation/unemployment. The system extracts from the U.S. by forcing this impossible choice.
constraint_indexing:constraint_classification(transition_causality__overdetermined_collapse_reading, snare,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: ANALYTICAL OBSERVER / OVERDETERMINED INEVITABILITY (MOUNTAIN) — From a civilizational horizon, the Bretton Woods collapse is overdetermined: multiple independent structural contradictions converge on the same terminal state (collapse). Triffin Dilemma, U.S.-European trade imbalance, Third World capital flight, gold depletion, monetary-fiscal trilemma — any ONE of these suffices to force collapse. All FOUR together make counterfactual viability near-zero. No policy choice, no contingent event, no coalition could have sustained the system given this level of contradiction.
constraint_indexing:constraint_classification(transition_causality__overdetermined_collapse_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 6: GOLD CONVERTIBILITY AS RITUAL (PITON) — The performative element: gold convertibility is treated as guaranteeing the dollar's value, yet the U.S. gold supply has been insufficient to meet redemption claims since the mid-1960s. The ritual persists through institutional inertia — central bankers publicly honor the gold standard while privately acknowledging it is unsustainable. Theater ratio reflects this performative maintenance of a failed mechanism.
constraint_indexing:constraint_classification(transition_causality__overdetermined_collapse_reading, piton,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(transition_causality__overdetermined_collapse_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(transition_causality__overdetermined_collapse_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(transition_causality__overdetermined_collapse_reading, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(transition_causality__overdetermined_collapse_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(transition_causality__overdetermined_collapse_reading, TR),
    TR >= 0.70.

:- end_tests(transition_causality__overdetermined_collapse_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62): High. The fixed-rate regime extracts from developing economies through balance-of-payments discipline and capital flight penalties; from peripheral reserve-currency states through deflationary domestic policy required to defend pegs; from the U.S. through the monetary-fiscal trilemma (must choose between inflation and unemployment). The extraction increases over the interval (0.35 → 0.62) as contradictions accumulate and policy space narrows. Suppression (0.68): High. Alternatives to fixed rates (floating rates, capital controls, more flexible adjustable pegs) are institutionally suppressed by IMF conditionality, Bretton Woods charter constraints, and ideological commitment to 'sound money.' Developing economies cannot employ capital controls without threatening IMF standing. Peripheral powers cannot devalue without loss of status. The suppression is enforced through institutional mechanisms (IMF conditionality, peer pressure among central bankers) rather than overt coercion. Theater ratio (0.45): Moderate. The ritual of gold convertibility persists despite being mathematically insolvent since 1966, maintained through central bank coordination and public silence about the insolvency. The ritual is performative but not primary — the constraint's primary mechanism is the structural contradiction (Triffin Dilemma), not theatrical maintenance. Lower theater ratio reflects that the system's failure is driven by mathematics and macroeconomic dynamics, not just institutional inertia.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap between readings is located in whether the contradictions alone determine collapse or require contingent trigger events. Within the OVERDETERMINED reading specifically, the gap is between the institutional actors (U.S., central banks) who experience high policy constraint (snare/constrained) and the analytical observer who sees the entire system as subject to mathematical inevitability (mountain). The institutional actors perceive their choices as constrained but real; the analytical observer sees their choices as illusory — all paths lead to collapse. The piton perspective (gold convertibility ritual) reveals that institutional actors knew the system was insolvent but maintained the performance for coordination reasons. The mountain perspective argues the performance was irrelevant — the mathematics was already determining the outcome.
 *
 * DIRECTIONALITY LOGIC:
 *   This reading declares NO beneficiaries. The Bretton Woods system is modeled as a pure extraction mechanism (Snare) with victims but no beneficiaries. This is analytically distinct from sibling readings that might identify beneficiaries (manufacturing exporters, U.S. financial interests) — those would produce different directionality. In the OVERDETERMINED reading, the system's terminal failure is so comprehensive that even apparent beneficiaries are trapped within it: the U.S. benefits from seigniorage in the short term but faces monetary-fiscal trilemma by the medium term. Developing economies never benefit — they are constrained throughout. The structure is: trapped agents with no exit (d=0.95, high f(d)) dominate the agent set. Institutional perspectives can exercise arbitrage at high political cost (U.S. can abandon the system but loses reserve privilege; peripheral powers can devalue but lose status) — these approach d=0.50-0.60 (moderate experienced extraction). The Triffin Dilemma perspective (mountain) has d=0.72 (analytical agent observing universal constraint), producing analytical perspective extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading resolves mandatrophy by treating the Bretton Woods system as a Snare (pure extraction with no coordination function) plus a Mountain (Triffin Dilemma as logical constraint). The distinction: the Snare is the institutional extraction mechanism (policy constraint on developing economies, balance-of-payments discipline, deflationary pressure on peripheral powers). The Mountain is the logical contradiction (reserve-currency dilemma) that makes the Snare unsustainable long-term. The mandatrophy is resolved by showing that apparent coordination (gold standard as coordination mechanism, fixed rates enabling trade) is actually extraction: the coordination is imposed, not emergent, and it concentrates costs (capital flight, unemployment, policy constraint) on powerless agents (developing economies) while distributing nominal benefits (privilege, seigniorage) to institutional actors. The mountain constraint ensures this extraction becomes increasingly untenable. By 1971, the system could only extract by pure suppression (forcing continued pegging despite obvious insolvency), at which point the institution collapsed.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    counterfactual_viability_threshold,
    'How many independent causal pathways to collapse must converge before the transition becomes structurally inevitable rather than contingent?',
    'Counterfactual analysis: remove each causal pathway (Triffin Dilemma, U.S. fiscal deficit, gold depletion, Third World capital flight, sterling-dollar competition) individually and assess whether the system could have survived. If removal of ANY SINGLE pathway would have prevented collapse, the overdetermined reading is false (system is contingent on that pathway). If removal of ALL FOUR is required to prevent collapse, overdetermined reading is supported.',
    'If system contingent on single pathway: reclassify as hybrid_trigger_reading (structural contradictions + contingent trigger). If truly overdetermined: mountain classification supported; all counterfactuals near-zero.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(counterfactual_viability_threshold, empirical, 'Structural threshold for overdetermined vs contingent collapse').

omega_variable(
    policy_choice_constraint_space,
    'Given the constraints active in 1965-1971, what was the actual policy choice space available to U.S. leadership?',
    'Institutional analysis of Federal Reserve meeting minutes, Treasury deliberations, and international coordination attempts. Identify which policy combinations were ruled out by institutional law (e.g., gold convertibility mandate), which by political feasibility (e.g., domestic inflation sensitivity), which by international coordination failure (e.g., no unified response to dollar outflows).',
    'If choice space was > 3 qualitatively different paths: contingent_choice_reading gains support (different choices were materially available). If choice space < 2 paths (all lead to collapse, differ only in timing): overdetermined reading supported.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(policy_choice_constraint_space, empirical, 'Size and structure of policy choice space under actual constraints').

omega_variable(
    gold_reserve_depletion_inevitability,
    'Was the U.S. gold reserve depletion inevitable given the balance-of-payments deficit, or could strategic reserve management have prevented the collapse?',
    'Financial modeling: simulate U.S. gold reserves under different capital control, trade policy, and SDR allocation scenarios. Assess whether any combination of policies could have stabilized gold reserves while maintaining fixed-rate convertibility and the other Bretton Woods constraints (capital mobility, independent monetary policy).',
    'If strategic management could have stabilized reserves: one pathway to collapse is contingent, reducing overdetermination. If all management scenarios fail given the trilemma constraint: overdetermination of that pathway confirmed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(gold_reserve_depletion_inevitability, empirical, 'Whether gold depletion was inevitable or contingent on policy choices').

omega_variable(
    kernel_contest_location,
    'Where exactly does the reading-disagreement locate within the institutional and causal structure?',
    'Analytic decomposition: contingent_choice_reading locates the freedom point in political decision-making (policymakers could have chosen differently); hybrid_trigger_reading locates it in the causal chain (contradictions accumulated but required contingent trigger to actualize); overdetermined_collapse_reading locates it nowhere (contradictions alone determine outcome). The three readings differ on whether structural contradictions are sufficient for collapse or require additional contingent inputs.',
    'This omega documents the kernel contest itself. Resolving the empirical omegas above (counterfactual viability, policy choice space, gold depletion) will determine which reading survives. Overdetermined reading is foreclosed if even one high-confidence pathway is revealed as contingent on policy choice.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_contest_location, conceptual, 'Where the reading contest locates freedom and necessity in the causal structure').

omega_variable(
    smithsonian_interlude_counterevidence,
    'Did the Smithsonian Agreement (December 1971) and subsequent pegged-but-adjustable rates from 1971-1973 provide evidence that the fixed-rate system could have been reformed rather than abandoned?',
    'Historical analysis of Smithsonian Agreement implementation and failure. Assess whether Smithsonian''s wider band (±2.25% vs ±1%), higher gold price ($38→$38.02), and coordinated revaluation constituted a structural reform that could have sustained fixed rates, or merely a performative delaying action that confirmed the underlying contradictions were unresolvable.',
    'If Smithsonian represented genuine structural reform: overdetermined reading faces counterevidence (system was contingently improvable rather than necessarily doomed). If Smithsonian was theater (contradictions persisted, collapse resumed within 18 months): overdetermined reading confirmed.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(smithsonian_interlude_counterevidence, empirical, 'Whether Smithsonian Agreement provides evidence against overdetermined collapse thesis').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(transition_causality__overdetermined_collapse_reading, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(trans_over_tr_t0, transition_causality__overdetermined_collapse_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement(trans_over_tr_t6, transition_causality__overdetermined_collapse_reading, theater_ratio, 6, 0.42).
narrative_ontology:measurement(trans_over_tr_t10, transition_causality__overdetermined_collapse_reading, theater_ratio, 10, 0.45).

% Extraction over time
narrative_ontology:measurement(trans_over_be_t0, transition_causality__overdetermined_collapse_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(trans_over_be_t3, transition_causality__overdetermined_collapse_reading, base_extractiveness, 3, 0.48).
narrative_ontology:measurement(trans_over_be_t6, transition_causality__overdetermined_collapse_reading, base_extractiveness, 6, 0.58).
narrative_ontology:measurement(trans_over_be_t10, transition_causality__overdetermined_collapse_reading, base_extractiveness, 10, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(trans_over_su_t0, transition_causality__overdetermined_collapse_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(trans_over_su_t3, transition_causality__overdetermined_collapse_reading, suppression_requirement, 3, 0.58).
narrative_ontology:measurement(trans_over_su_t6, transition_causality__overdetermined_collapse_reading, suppression_requirement, 6, 0.65).
narrative_ontology:measurement(trans_over_su_t10, transition_causality__overdetermined_collapse_reading, suppression_requirement, 10, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(transition_causality__overdetermined_collapse_reading, resource_allocation).
narrative_ontology:affects_constraint(transition_causality__overdetermined_collapse_reading, triffin_dilemma_mountain).
narrative_ontology:affects_constraint(transition_causality__overdetermined_collapse_reading, gold_reserve_depletion).
narrative_ontology:affects_constraint(transition_causality__overdetermined_collapse_reading, monetary_fiscal_trilemma).
narrative_ontology:affects_constraint(transition_causality__overdetermined_collapse_reading, third_world_capital_flight).

% DUAL FORMULATION NOTE:
% The overdetermined reading treats the Bretton Woods collapse as overdetermined by four converging structural pathways: Triffin Dilemma (mountain), U.S. monetary-fiscal trilemma (snare), gold reserve depletion (snare), Third World capital flight (snare). Sibling readings would decompose the collapse differently, potentially identifying a single dominant pathway or arguing that contingent trigger events were necessary. Each pathway should have its own constraint story with its own epsilon value; the network edges model how overdetermination emerges from the convergence.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
