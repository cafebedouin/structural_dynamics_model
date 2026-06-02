% ============================================================================
% CONSTRAINT STORY: gold_fiat_transition_mechanism__creditor_discipline_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_gold_fiat_transition_mechanism__creditor_discipline_reading, []).

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
 *   constraint_id: gold_fiat_transition_mechanism__creditor_discipline_reading
 *   human_readable: Gold-to-Fiat Transition: Creditor Discipline Elimination
 *   domain: monetary_economics/political_economy/history_of_economic_thought
 *
 * SUMMARY:
 *   This constraint story instantiates the CREDITOR DISCIPLINE READING of the
 *   contested kernel 'gold_fiat_transition_mechanism'. The reading focuses on
 *   the elimination of the creditor nations' structural veto power — the
 *   ability to demand gold redemption if a debtor nation (especially the US)
 *   ran persistent balance-of-payments deficits. Under the gold standard,
 *   this redemption threat forced fiscal discipline: debtor nations could not
 *   indefinitely deficit-spend. The 1944 Bretton Woods compromise established
 *   fixed exchange rates pegged to gold via the dollar; the 1971 closure of
 *   the gold window severed this link. From the creditor discipline reading,
 *   the transition was an extraction mechanism: the US reserve-currency
 *   issuer captured the ability to run deficits without triggering the gold
 *   discipline that had constrained all prior debtors. Creditor nations lost
 *   their only exit mechanism — the threat of redemption.
 *   Non-reserve-currency debtors remained subject to balance-of-payments
 *   discipline through capital flight and currency devaluation, now
 *   asymmetric. The constraint manifests as a snare for creditors (trapped
 *   with no exit, experiencing extraction through loss of leverage) and for
 *   non-aligned debtors (remaining disciplined while the reserve issuer
 *   escapes). This reading coexists with two siblings: the AUTOMATIC
 *   CONSTRAINT READING (which interprets gold discipline as a legitimate,
 *   self-enforcing coordination mechanism necessary for any reserve system)
 *   and the COMPOSITE OVERDETERMINATION READING (which holds that the
 *   transition created multiple simultaneous constraints, not a single shift
 *   from discipline to immunity). All three readings are live positions in
 *   contemporary monetary economics and international political economy
 *   scholarship.
 *
 * KEY AGENTS:
 *   - Reserve-Currency Issuer (US): Primary beneficiary (institutional/arbitrage) — gained ability to run deficits and deficit-spend without triggering gold redemption discipline. Benefits from seigniorage, expanded fiscal space, geopolitical leverage.
 *   - Creditor Nations (especially Switzerland, Germany, Japan post-war): Primary victims (institutional/trapped) — lost structural veto power (gold redemption threat). Their claims on the US were devalued through currency depreciation and inflation. No exit: holding reserve currencies is essential for trade and capital storage, but without redemption rights.
 *   - Non-Reserve-Currency Debtors (Global South, non-aligned): Secondary victims (moderate/constrained) — remained subject to balance-of-payments discipline through capital flight and currency depreciation, even as the reserve issuer escaped discipline. Asymmetric extraction.
 *   - Institutionally Aligned Debtors (UK, Canada, etc.): Mixed position (powerful/mobile) — benefited from expanded liquidity and export demand created by US deficit-spending, but remained subject to unilateral veto power (sanctions, capital controls) by the reserve issuer.
 *   - IMF / Bretton Woods Institutions: Institutional enforcers (institutional/constrained) — maintained performative discipline role after gold window closed, but with degraded structural leverage. Theater-intensive.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gold_fiat_transition_mechanism__creditor_discipline_reading, 0.68).
domain_priors:suppression_score(gold_fiat_transition_mechanism__creditor_discipline_reading, 0.62).
domain_priors:theater_ratio(gold_fiat_transition_mechanism__creditor_discipline_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gold_fiat_transition_mechanism__creditor_discipline_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(gold_fiat_transition_mechanism__creditor_discipline_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(gold_fiat_transition_mechanism__creditor_discipline_reading, theater_ratio, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gold_fiat_transition_mechanism__creditor_discipline_reading, snare).
narrative_ontology:human_readable(gold_fiat_transition_mechanism__creditor_discipline_reading, "Gold-to-Fiat Transition: Creditor Discipline Elimination").
narrative_ontology:topic_domain(gold_fiat_transition_mechanism__creditor_discipline_reading, "monetary_economics/political_economy/history_of_economic_thought").

domain_priors:requires_active_enforcement(gold_fiat_transition_mechanism__creditor_discipline_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gold_fiat_transition_mechanism__creditor_discipline_reading, '7a2f8c1d-4e9b-42b3-8f67-2c5a9b1d3e4f').
narrative_ontology:cs_kernel_codification('7a2f8c1d-4e9b-42b3-8f67-2c5a9b1d3e4f', formalized).
narrative_ontology:cs_authority_grounding('7a2f8c1d-4e9b-42b3-8f67-2c5a9b1d3e4f', extraction).
narrative_ontology:cs_interpretation_layer_present('7a2f8c1d-4e9b-42b3-8f67-2c5a9b1d3e4f').
narrative_ontology:cs_reading_relation('7a2f8c1d-4e9b-42b3-8f67-2c5a9b1d3e4f', gold_fiat_transition_mechanism__automatic_constraint_reading, coexists_with).
narrative_ontology:cs_reading_relation('7a2f8c1d-4e9b-42b3-8f67-2c5a9b1d3e4f', gold_fiat_transition_mechanism__composite_overdetermination_reading, influences).
narrative_ontology:cs_axiom('7a2f8c1d-4e9b-42b3-8f67-2c5a9b1d3e4f', foundational, creditor_veto_power_was_extractive).
narrative_ontology:cs_axiom_status(creditor_veto_power_was_extractive, holdable).
narrative_ontology:cs_axiom_grounding('7a2f8c1d-4e9b-42b3-8f67-2c5a9b1d3e4f', creditor_veto_power_was_extractive, deontological).
narrative_ontology:cs_axiom('7a2f8c1d-4e9b-42b3-8f67-2c5a9b1d3e4f', foundational, reserve_issuer_gained_unilateral_deficit_capacity).
narrative_ontology:cs_axiom_status(reserve_issuer_gained_unilateral_deficit_capacity, holdable).
narrative_ontology:cs_axiom_grounding('7a2f8c1d-4e9b-42b3-8f67-2c5a9b1d3e4f', reserve_issuer_gained_unilateral_deficit_capacity, empirically_contingent).
narrative_ontology:cs_reference_frame('7a2f8c1d-4e9b-42b3-8f67-2c5a9b1d3e4f', gold_standard_universal_discipline).
narrative_ontology:cs_drift_state('7a2f8c1d-4e9b-42b3-8f67-2c5a9b1d3e4f', fiat_system_stabilized_post_1980, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('7a2f8c1d-4e9b-42b3-8f67-2c5a9b1d3e4f', '2026-02-26T14:23:18Z').
narrative_ontology:cs_kernel_id(gold_fiat_transition_mechanism__creditor_discipline_reading, gold_fiat_transition_mechanism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gold_fiat_transition_mechanism__creditor_discipline_reading, reserve_currency_issuer).
narrative_ontology:constraint_beneficiary(gold_fiat_transition_mechanism__creditor_discipline_reading, debtor_nations_us_aligned).
narrative_ontology:constraint_victim(gold_fiat_transition_mechanism__creditor_discipline_reading, creditor_nations).
narrative_ontology:constraint_victim(gold_fiat_transition_mechanism__creditor_discipline_reading, gold_standard_discipline).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CREDITOR NATIONS (SNARE) — The transition eliminated their only structural escape mechanism: the credible threat to demand gold redemption if a debtor nation ran persistent deficits. This threat was the creditor's veto power. Once the link to gold was severed, creditor nations lost the mechanism to enforce discipline on debtors. They remain trapped in a system where debtor nations can accumulate deficits indefinitely, devaluing their claims. No exit: holding dollars becomes a structural dependency with no redemption right. Pure extraction — the creditor cannot organize collective refusal because each creditor benefits individually from dollar liquidity, even as their collective interest is violated.
constraint_indexing:constraint_classification(gold_fiat_transition_mechanism__creditor_discipline_reading, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: RESERVE CURRENCY ISSUER / DEBTOR NATIONS (ROPE) — The US as issuer of the reserve currency experiences the transition as pure coordination benefit. The constraint is the mechanism by which the US can run deficits without triggering the gold redemption discipline that would have forced fiscal adjustment. Experienced as a coordination solution: nations need a medium of exchange; dollars provide this; the quid pro quo is that the US can deficit-spend. The beneficiary sees this as legitimate exchange — the dollar's role confers benefits that offset its use as fiscal discipline elimination. Net beneficiary position: arbitrage exit (can always acquire gold or alternative reserves). Low experienced extraction because the benefit aligns with the mechanism.
constraint_indexing:constraint_classification(gold_fiat_transition_mechanism__creditor_discipline_reading, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: NON-RESERVE-CURRENCY DEBTOR NATIONS (SNARE) — While the transition eliminated the discipline for reserve-currency debtors (especially the US), non-reserve debtors remain subject to external discipline: capital flight, balance-of-payments crises, IMF conditionality. These nations could run deficits briefly but face hard constraints from currency depreciation and liquidity withdrawal. They are trapped in a discipline mechanism that the reserve issuer has escaped — extraction asymmetry. High suppression (capital outflow threat, currency devaluation), high extractiveness (the mechanism that disciplined them is no longer universal).
constraint_indexing:constraint_classification(gold_fiat_transition_mechanism__creditor_discipline_reading, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 4: INSTITUTIONALLY ALIGNED DEBTORS (TANGLED ROPE) — Debtor nations aligned with the US reserve-issuer bloc experience a mixed constraint. They benefit from the expanded fiscal space created by the transition (the reserve issuer's deficit-spending capacity increases demand for their exports, provides liquidity, enables their own borrowing). But they also remain subject to discipline mechanisms (sanctions, capital restrictions, trade exclusion) that the reserve issuer can deploy unilaterally. Genuine coordination function (trade, investment, military alliance) alongside asymmetric extraction (the reserve issuer maintains veto power through sanctions, not through gold discipline). Mobile exit options because geopolitical alignment is contingent, though costly to exit.
constraint_indexing:constraint_classification(gold_fiat_transition_mechanism__creditor_discipline_reading, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: ANALYTICAL OBSERVER / NATURAL LAW (MOUNTAIN) — From a civilizational perspective, the gold constraint is treated as a natural law: any currency system requires some discipline mechanism; the gold link provided automatic discipline; removing it is inherently destabilizing and impossible to sustain without replacement mechanisms. The observer sees the transition as impossible — debtors cannot indefinitely run deficits without currency collapse. This perspective naturalizes what this reading treats as a deliberately constructed extraction mechanism. The structural data (clear beneficiaries in reserve-issuing debtors, clear victims in creditor nations and non-reserve debtors) will trigger false-summit detection in the engine.
constraint_indexing:constraint_classification(gold_fiat_transition_mechanism__creditor_discipline_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 6: BRETTON WOODS INSTITUTIONAL LEGACY (PITON) — The IMF, World Bank, and associated conditionality regimes represent an attempted reconstruction of the gold-standard discipline after the US closed the gold window in 1971. These institutions perform the ritual of fiscal oversight and conditionality enforcement while lacking the structural leverage that the gold link provided. Theater ratio is high: IMF programs are elaborately detailed, intensively monitored, and rhetorically presented as binding discipline. Actual enforcement power is degraded compared to the gold-standard mechanism — nations can default, capital controls can be imposed, political will can override compliance. The institutional apparatus persists through inertia and because some creditor nations benefit from the appearance of discipline, even if enforcement is weaker than the original mechanism.
constraint_indexing:constraint_classification(gold_fiat_transition_mechanism__creditor_discipline_reading, piton,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gold_fiat_transition_mechanism__creditor_discipline_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(gold_fiat_transition_mechanism__creditor_discipline_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(gold_fiat_transition_mechanism__creditor_discipline_reading, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(gold_fiat_transition_mechanism__creditor_discipline_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(gold_fiat_transition_mechanism__creditor_discipline_reading, TR),
    TR >= 0.70.

:- end_tests(gold_fiat_transition_mechanism__creditor_discipline_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. The transition created a durable asymmetry: the US reserve-currency issuer can accumulate deficits without triggering the redemption discipline that constrains other debtors. This is extractive because it concentrates the benefit (unlimited deficit-spending capacity) on one actor while distributing the cost (currency depreciation, inflation, loss of purchasing power) across creditor nations and the global system. The extractiveness increased over time as the US learned to exploit the new system (t0=0.25 under gold, t1=0.52 at transition, t2=0.68 once stabilized). Suppression (0.62): Moderate-high. The mechanism requires suppression of alternative arrangements: creditor nations cannot credibly demand a return to gold (capital controls, capital flight threats); non-reserve debtors cannot escape discipline (currency markets enforce it). Theater ratio (0.45): Moderate. The gold standard mechanism was relatively automatic and low-theater — the discipline was enforced by the physical constraint of the gold reserve. The Bretton Woods compromise introduced some theater (fixed-but-adjustable exchange rates, institutional oversight). The post-1971 fiat system introduced higher theater: IMF programs, conditionality regimes, and policy prescriptions replaced the automatic mechanism. The theater ratio increase from 0.15 (gold auto-mechanism) to 0.45 (fiat ritual) documents the degradation of enforcement mechanism from structural to performative.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap reveals a fundamental disagreement about the mechanism's function. The reserve-currency issuer (rope perspective) sees legitimate coordination — nations need a medium of exchange; the dollar provides this; the quid pro quo is that the US can deficit-spend without triggering the gold discipline. This is a standard beneficiary frame: coordination benefit justifies the extraction. Creditor nations (snare perspective) see pure extraction — they have lost their only structural escape mechanism without gaining offsetting benefits. Non-aligned debtors (snare perspective) see asymmetric extraction — the reserve issuer escaped discipline while they remain constrained. The analytical observer (mountain perspective) risks naturalizing the constraint as a law of finance: any reserve system requires some discipline; fiat systems must have alternative mechanisms. This naturalization masks the distributive shift — from universal discipline under gold to asymmetric discipline under fiat. The piton perspective (Bretton Woods institutional legacy) sees a degraded ritual: the IMF performs detailed oversight without the structural leverage that the gold link provided. The performance persists because some creditor nations benefit from the appearance of discipline.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) is derived from beneficiary/victim declarations and exit options. Reserve-currency issuer: beneficiary + arbitrage exit (can accumulate gold, abandon dollar system if necessary) → d ≈ 0.05 → low experienced extraction. Creditor nations: victims + trapped (must hold reserve currencies for trade; no exit to gold redemption) → d ≈ 0.95 → maximum experienced extraction. Non-reserve debtors: victims + constrained (face capital flight threat but could theoretically impose controls) → d ≈ 0.85 → high experienced extraction. Institutionally aligned debtors: mixed (beneficiary through trade benefit, victim through veto power) + mobile exit (geopolitical alignment contingent) → d ≈ 0.55 → moderate experienced extraction. The formula χ = ε × f(d) × σ(S) amplifies the snare classification: at global scope σ(S) = 1.2, and for trapped agents f(d) ≈ 1.42, producing χ ≈ 0.68 × 1.42 × 1.2 ≈ 1.15 for the creditor perspective — extractiveness well above the snare floor of 0.66.
 *
 * MANDATROPHY ANALYSIS:
 *   COMMITTER FRAME APPLICATION: This reading resolves potential mandatrophy by explicitly situating itself within a contested kernel. The gold-to-fiat transition can be read three ways: (1) creditor discipline reading (this constraint): elimination of creditor veto power, beneficiary is reserve issuer, extraction is real. (2) automatic constraint reading (sibling): gold discipline was legitimate coordination mechanism; fiat requires alternative (Bretton Woods institutions); no extraction, just mechanism change. (3) composite overdetermination reading (sibling): the transition created multiple constraints simultaneously — discipline constraint, seigniorage constraint, geopolitical constraint — no single reading captures it. All three are defensible from the academic literature. This reading does not claim to be the only truth; it claims to be a coherent, perspectival reading of a contested kernel. The snare classification holds if creditor discipline was extractive; the rope classification holds if it was coordinating. The perspectival gap between the beneficiary's rope and the creditor's snare is not a failure of the framework — it is the framework working correctly, revealing that the transition's legitimacy depends on normative frame adoption.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reading_assumption_one_reserve_issuer_immunity,
    'Did the gold-to-fiat transition create structural immunity from balance-of-payments discipline specifically for the reserve-currency issuer, or did it create a different (but still binding) discipline mechanism?',
    'Historical analysis of US deficit-spending trajectories pre- and post-1971; comparison of discipline mechanisms imposed on reserve issuer vs non-reserve debtors; identification of whether Bretton Woods conditionality constitutes genuine constraint or performative oversight',
    'If reserve issuer immunity is real and structural: this reading''s snare classification holds — extraction is genuine. If new discipline mechanisms (currency market, capital flight threat, political cost) bind the reserve issuer equally: reclassify to tangled_rope or rope (coordination with constraints on all participants).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_assumption_one_reserve_issuer_immunity, empirical, 'Whether reserve-currency issuer achieved structural immunity from discipline').

omega_variable(
    reading_assumption_two_extracted_from_whom,
    'Is the primary extraction from creditor nations (who lost veto power), from non-reserve debtors (who remain disciplined), or from the global monetary system itself (inflation, systemic instability)?',
    'Wealth flow analysis: did creditor nations lose reserves/purchasing power after 1971? Did non-reserve debtors face tighter constraints post-1971? Did global inflation accelerate post-transition? Distributional analysis of seigniorage gains.',
    'If creditor nations are primary victims: snare reading is correct — extraction concentrates on those with no exit. If non-reserve debtors are primary victims: this is a different snare (asymmetric discipline). If the global system itself is victim (inflation, instability): reclassify to tangled_rope with distributed extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_assumption_two_extracted_from_whom, empirical, 'Primary target of extraction in the gold-to-fiat transition').

omega_variable(
    reading_assumption_three_committer_frame_gold_discipline_normative_status,
    'Was the gold-standard discipline mechanism a legitimate coordination cost (coordination function in rope-reading) or an illegitimate extraction (pure snare as this reading claims)?',
    'This is a preference omega, resolvable only by normative frame adoption. Different kernels (readings) hold different positions on whether gold discipline was coordinating or extracting. This reading assumes it was extractive; the ''automatic_constraint_reading'' sibling assumes it was coordinating; the ''composite_overdetermination_reading'' assumes both roles simultaneously.',
    'This omega documents the kernel-level disagreement. The snare reading holds that creditors used gold discipline to extract rents; sibling readings hold that gold discipline provided necessary discipline function. No amount of empirical data resolves this without adopting a normative frame about what constitutes ''legitimate'' discipline vs ''extraction''.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_assumption_three_committer_frame_gold_discipline_normative_status, conceptual, 'Normative frame: was gold discipline legitimate coordination or extractive mechanism?').

omega_variable(
    reading_assumption_four_sibling_relation_choice,
    'Do the sibling readings (automatic_constraint_reading and composite_overdetermination_reading) genuinely coexist as live positions, or does the empirical evidence foreclose one of them?',
    'Scholarly consensus analysis: do leading economic historians and policy scholars maintain all three readings as defensible, or has evidence accumulated that eliminates one or more? Identification of which reading dominates among practitioners at different time horizons.',
    'If all three coexist among serious practitioners: relation is ''coexists_with''. If evidence or logic forecloses one reading: update reading_relations to ''forecloses'' for the appropriate pairs.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_assumption_four_sibling_relation_choice, empirical, 'Whether sibling readings coexist or are foreclosed by accumulated evidence').

omega_variable(
    reading_assumption_five_extraction_persistence,
    'Is the extraction mechanism (reserve-issuer immunity from balance-of-payments discipline) stabilizing over time, degrading, or being replaced by alternative mechanisms?',
    'Long-term trajectory analysis: US deficit-to-GDP ratios, currency reserve composition, capital flight dynamics, emergence of alternative reserve currencies (SDR, digital currencies, regional alternatives). Measurement of whether the extraction mechanism is self-reinforcing or eroding.',
    'If stabilizing: this is a durable snare with high suppression. If degrading: the constraint is moving toward piton (ritual enforcement without structural bite). If replacement mechanisms emerging: this reading''s snare may be transitioning to a new constraint (digital currency discipline, regional reserve systems).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_assumption_five_extraction_persistence, empirical, 'Long-term trajectory of reserve-currency discipline extraction mechanism').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gold_fiat_transition_mechanism__creditor_discipline_reading, 0, 2).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gftm_cred_theater_t0_gold_auto_mechanism, gold_fiat_transition_mechanism__creditor_discipline_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(gftm_cred_theater_t1_bretton_woods_reform, gold_fiat_transition_mechanism__creditor_discipline_reading, theater_ratio, 1, 0.35).
narrative_ontology:measurement(gftm_cred_theater_t2_imf_ritual, gold_fiat_transition_mechanism__creditor_discipline_reading, theater_ratio, 2, 0.45).

% Extraction over time
narrative_ontology:measurement(gftm_cred_extract_t0_gold_standard_baseline, gold_fiat_transition_mechanism__creditor_discipline_reading, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(gftm_cred_extract_t1_bretton_woods_end, gold_fiat_transition_mechanism__creditor_discipline_reading, base_extractiveness, 1, 0.52).
narrative_ontology:measurement(gftm_cred_extract_t2_fiat_stabilized, gold_fiat_transition_mechanism__creditor_discipline_reading, base_extractiveness, 2, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(gftm_cred_supp_t0_gold_window_closure_threat, gold_fiat_transition_mechanism__creditor_discipline_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(gftm_cred_supp_t1_capital_flight_regime, gold_fiat_transition_mechanism__creditor_discipline_reading, suppression_requirement, 1, 0.62).
narrative_ontology:measurement(gftm_cred_supp_t2_imf_conditionality, gold_fiat_transition_mechanism__creditor_discipline_reading, suppression_requirement, 2, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gold_fiat_transition_mechanism__creditor_discipline_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(gold_fiat_transition_mechanism__creditor_discipline_reading, 0.12).
narrative_ontology:affects_constraint(gold_fiat_transition_mechanism__creditor_discipline_reading, bretton_woods_conditionality_regime).
narrative_ontology:affects_constraint(gold_fiat_transition_mechanism__creditor_discipline_reading, reserve_currency_seigniorage_extraction).
narrative_ontology:affects_constraint(gold_fiat_transition_mechanism__creditor_discipline_reading, capital_flight_discipline_mechanism).
narrative_ontology:affects_constraint(gold_fiat_transition_mechanism__creditor_discipline_reading, imf_structural_adjustment_snare).

% DUAL FORMULATION NOTE:
% The gold-to-fiat transition constraint family includes three sibling constraints with different epsilon values and structural readings. The creditor_discipline_reading (this constraint, ε=0.68) focuses on the elimination of the creditor veto mechanism. The automatic_constraint_reading sibling (downstream, ε lower, rope-classified) assumes gold discipline was coordinating and examines the replacement coordination mechanisms. The composite_overdetermination_reading (independent) treats the transition as creating multiple simultaneous constraints (discipline, seigniorage, geopolitical power). All three are linked via network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
