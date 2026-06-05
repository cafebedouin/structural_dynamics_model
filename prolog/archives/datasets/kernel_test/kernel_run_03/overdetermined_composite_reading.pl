% ============================================================================
% CONSTRAINT STORY: overdetermined_composite_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_overdetermined_composite_reading, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: overdetermined_composite_reading
 *   human_readable: Overdetermined Composite Collapse of Bretton Woods (1960-1971)
 *   domain: monetary_economics/political_economy/international_finance
 *
 * SUMMARY:
 *   The collapse of the Bretton Woods gold-standard system (1971) is
 *   typically explained through a single dominant cause: Triffin's dilemma
 *   (the impossibility of a reserve currency simultaneously maintaining
 *   parity, autonomy, and solvency). However, the overdetermined_composite
 *   reading argues that the collapse resulted from multiple structurally
 *   independent causal streams — Triffin logic, Vietnam War fiscal deficits
 *   inflating the money supply, technological capital mobility enabling
 *   currency arbitrage, and the Keynesian policy consensus rejecting
 *   deflation as a policy tool — each of which alone would have generated
 *   unsustainable pressure on the system, but whose combination made the
 *   collapse inevitable and structurally irreversible by the late 1960s. This
 *   reading is distinct from single-cause (Triffin inevitability) and
 *   policy-intervention (punctuated swap) siblings. It instantiates a
 *   tangled_rope constraint because genuine coordination functions (fixed
 *   exchange rates enabled post-war recovery and trade expansion) coexisted
 *   with asymmetric extraction (US seigniorage, capital flight to safety,
 *   imported inflation for allied economies). The overdetermination itself is
 *   the constraint: no single policy lever could address all causal streams
 *   simultaneously without reverting to unacceptable alternatives (deflation,
 *   capital controls, gold standard abandonment). The constraint's
 *   extractiveness rose from 0.25 (early 1960s: system functional,
 *   beneficiaries consolidating advantages) to 0.58 (1970-71: system
 *   dysfunctional, suppression and extraction visible to all agents). Theater
 *   ratio remained moderate (0.32-0.48) because while institutions performed
 *   ritual (IMF swap agreements, SDR issuance), the underlying structural
 *   contradictions were intellectually transparent — this was not a
 *   false-summit case where natural law framing obscured extraction.
 *
 * KEY AGENTS:
 *   - US Fiscal Authority: Primary beneficiary (institutional/arbitrage) — seigniorage privileges, deficit spending capacity, deflationary flexibility
 *   - Capital Mobile Institutions: Secondary beneficiary (organized/mobile) — arbitrage opportunities, Eurodollar creation, exit capacity
 *   - Non-Reserve Currency Economies: Primary victim (powerless/trapped) — forced austerity, capital flight, revaluation dilemma with no exit
 *   - Monetary Discipline Constraint: Structural victim (abstract/trapped) — the gold standard's anti-inflationary anchor loses force as system destabilizes
 *   - Allied Developed Economies: Mixed agent (moderate/constrained) — benefit from dollar anchor and US security but export inflation and face seigniorage transfer
 *   - IMF / Bretton Woods Institutions: Coordinator (institutional/arbitrage) — attempt to manage adjustment through technical reforms (SDRs, swaps) but operate outside the fundamental causal streams
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing the collapse as inevitable when it reflects overdetermined policy constraints, not immutable law
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(overdetermined_composite_reading, 0.58).
domain_priors:suppression_score(overdetermined_composite_reading, 0.65).
domain_priors:theater_ratio(overdetermined_composite_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(overdetermined_composite_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(overdetermined_composite_reading, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(overdetermined_composite_reading, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(overdetermined_composite_reading, tangled_rope).
narrative_ontology:human_readable(overdetermined_composite_reading, "Overdetermined Composite Collapse of Bretton Woods (1960-1971)").
narrative_ontology:topic_domain(overdetermined_composite_reading, "monetary_economics/political_economy/international_finance").

domain_priors:requires_active_enforcement(overdetermined_composite_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(overdetermined_composite_reading, 'c0d804bc-9674-4b61-8291-5fd2233b5c8a').
narrative_ontology:cs_created_at('c0d804bc-9674-4b61-8291-5fd2233b5c8a', '').
narrative_ontology:cs_kernel_codification('c0d804bc-9674-4b61-8291-5fd2233b5c8a', fixed_text).
narrative_ontology:cs_authority_grounding('c0d804bc-9674-4b61-8291-5fd2233b5c8a', extraction).
narrative_ontology:cs_interpretation_layer_present('c0d804bc-9674-4b61-8291-5fd2233b5c8a').
narrative_ontology:cs_kernel_id(overdetermined_composite_reading, monetary_anchor_principle).
narrative_ontology:cs_reading_relation('c0d804bc-9674-4b61-8291-5fd2233b5c8a', triffin_inevitability_reading, influences).
narrative_ontology:cs_reading_relation('c0d804bc-9674-4b61-8291-5fd2233b5c8a', punctuated_swap_reading, coexists_with).
narrative_ontology:cs_axiom('c0d804bc-9674-4b61-8291-5fd2233b5c8a', foundational, overdetermination_irreversibility).
narrative_ontology:cs_axiom_status(overdetermination_irreversibility, holdable).
narrative_ontology:cs_axiom_grounding('c0d804bc-9674-4b61-8291-5fd2233b5c8a', overdetermination_irreversibility, empirically_contingent).
narrative_ontology:cs_axiom('c0d804bc-9674-4b61-8291-5fd2233b5c8a', foundational, beneficiary_extraction_through_constraint_maintenance).
narrative_ontology:cs_axiom_status(beneficiary_extraction_through_constraint_maintenance, holdable).
narrative_ontology:cs_axiom_grounding('c0d804bc-9674-4b61-8291-5fd2233b5c8a', beneficiary_extraction_through_constraint_maintenance, deontological).
narrative_ontology:cs_reference_frame('c0d804bc-9674-4b61-8291-5fd2233b5c8a', dollar_gold_parity_regime).
narrative_ontology:cs_drift_state('c0d804bc-9674-4b61-8291-5fd2233b5c8a', post_1968_london_gold_pool_collapse, gap(authority_erosion, substantial, false)).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(overdetermined_composite_reading, us_fiscal_authority).
narrative_ontology:constraint_beneficiary(overdetermined_composite_reading, capital_mobile_institutions).
narrative_ontology:constraint_victim(overdetermined_composite_reading, monetary_discipline_constraint).
narrative_ontology:constraint_victim(overdetermined_composite_reading, non_reserve_currency_states).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: MONETARY DISCIPLINE AS VICTIM (SNARE) — The gold standard constraint on domestic monetary expansion cannot exit the system. Non-US central banks face impossible trilemma: maintain fixed parity, preserve monetary autonomy, or allow capital flows. Choose any two; the third collapses. By late 1960s, the constraint extracts maximum cost (forced austerity, capital flight, forced revaluation or default). Powerless because the victim is an abstract institutional principle, not an organized actor.
constraint_indexing:constraint_classification(overdetermined_composite_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: NON-RESERVE CURRENCY ECONOMIES (SNARE) — Smaller trading nations face forced choice: peg to dollar at overvalued rate (sacrificing competitiveness) or break parity (destroying savings, capital flight). The overdetermined collapse removes both paths simultaneously. No exit: cannot maintain the peg (external reserves drain), cannot devalue (triggers competitive devaluation cascade), cannot abandon the system (it's the only game). Trapped by structural dependency on reserve currency anchor.
constraint_indexing:constraint_classification(overdetermined_composite_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 3: US MONETARY AUTHORITY (TANGLED ROPE) — The US experiences genuine coordination function (Bretton Woods enables post-war trade, capital reconstruction) AND asymmetric extraction (seigniorage privilege, deficit spending latitude). Constrained because commitment to gold parity is costly (must maintain large gold reserve, credibility threshold rising) but profitable (dollar as international medium). The constraint benefits them systemically but constrains their autonomy operationally. Both coordination and asymmetric extraction are real.
constraint_indexing:constraint_classification(overdetermined_composite_reading, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 4: CAPITAL MOBILE INSTITUTIONS (ROPE) — International banks, multinational corporations, and capital arbitrageurs see Bretton Woods as pure coordination mechanism: fixed parities enable pricing, forward contracting, and capital allocation without exchange risk. As system degrades (gold reserve drain, inflation, capital controls tightening), they exercise exit through offshore markets, Eurodollar creation, capital flight. Mobile exit + benefit = rope classification. They experience the system as unilateral coordination, not extraction.
constraint_indexing:constraint_classification(overdetermined_composite_reading, rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: ALLIED DEVELOPED ECONOMIES (TANGLED ROPE) — Benefit from dollar stability and US security umbrella (coordination function: enabled post-war recovery, trade expansion, capital investment). Also extract costs: dollar inflation imported via fixed parity, seigniorage transfers to US, constrained monetary autonomy. Constrained exit because breaking parity risks US security commitment, but also generating slow capital erosion (inflation, real exchange rate overvaluation). Mixed coordination and extraction — both structural and asymmetric.
constraint_indexing:constraint_classification(overdetermined_composite_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: IMF AND BRETTON WOODS INSTITUTIONS (PITON) — The IMF was designed to manage adjustments (liquidity provision, orderly revaluation). By late 1960s, the institution is performing theater: issuing Special Drawing Rights (SDRs) to supplement reserves without addressing fundamental contradictions, negotiating swap agreements as temporary patches, claiming coordination function while the actual system mechanics bypass them (Eurodollar market, capital controls, bilateral negotiations). Theater-ratio high (0.65+) because the institution's tools cannot address the overdetermined collapse — structural causal streams are not resolving through the institution's intended mechanisms.
constraint_indexing:constraint_classification(overdetermined_composite_reading, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / INEVITABILITY VIEW (MOUNTAIN) — From a civilizational view, the collapse appears inevitable: given Triffin dilemma (reserve currency state cannot simultaneously maintain parity, autonomy, and solvency), given capital mobility at large scale, given differential inflation rates under Bretton Woods nominal anchor, the system's collapse is structurally determined. The observer might classify this as an immutable feature of the international monetary geometry — you cannot sustain fixed rates + free capital flows + independent monetary policy simultaneously. However, this perspective risks naturalizing what is actually a contingent choice about which constraints to prioritize.
constraint_indexing:constraint_classification(overdetermined_composite_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 8: POLICY REFORM COALITION (SCAFFOLD) — International monetary reformers (central bankers, Keynesians, IMF economists) see the overdetermined collapse not as inevitable mountain but as a temporary coordination failure resolvable through staged reform: expand liquidity through SDRs, widen gold points, negotiate revaluation bands, transition to adjustable peg system. The scaffold perspective sees extraction as temporary and solvable through policy design. Has sunset logic: if reforms can be sequenced (parity widening → SDR expansion → revaluation → float), the system can be rescued. Theater is lower (0.35-0.45) because reformers are genuinely trying to solve the coordination problem, not performing theater.
constraint_indexing:constraint_classification(overdetermined_composite_reading, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(overdetermined_composite_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(overdetermined_composite_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(overdetermined_composite_reading, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(overdetermined_composite_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(overdetermined_composite_reading, TR),
    TR >= 0.70.

:- end_tests(overdetermined_composite_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High. The system generates substantial asymmetric benefits for beneficiaries (US fiscal authority, capital-mobile institutions) while imposing costs on non-reserve-currency economies and the monetary discipline principle itself. The extractiveness is not maximal because genuine coordination functions existed (post-war trade, capital recovery) and because the system's failure was predictable and visible to all parties by the late 1960s — not a hidden extraction mechanism. Suppression (0.65): High. Trapped agents (non-reserve economies) face suppression from multiple directions: Triffin logic (cannot maintain parity + autonomy + capital flows), capital mobility (funds flow out under fixed overvalued parity), policy consensus (Keynesian opposition to deflation as adjustment mechanism), structural deficits (external reserves cannot cover US spending). Exit via devaluation is suppressed (triggers competitive cascades), exit via capital controls is suppressed (contradicts IMF Articles), exit via default is suppressed (destroys savings and creditworthiness). Theater ratio (0.48): Moderate. The system maintained genuine functional elements (trade finance, capital allocation under fixed rates) while policy institutions (IMF, central banks) performed coordination theater (SDRs, swap lines) that could not address fundamental contradictions. The theater is not high because the underlying logic was transparent — economists understood Triffin by 1960; the collapse was not surprising to informed observers, only to policymakers who hoped technical fixes would work.
 *
 * PERSPECTIVAL GAP:
 *   This constraint shows maximum perspectival differentiation. The US beneficiary sees rope (coordination) — the system solves the legitimate problem of enabling post-war trade and capital recovery without the transaction costs of constant renegotiation. Capital-mobile institutions see pure rope (arbitrage opportunities and pricing certainty). Allied developed economies see tangled_rope (benefit from security and trade but bear inflation and seigniorage costs). Non-reserve economies see snare (trapped by trilemma with no exit path). The IMF sees piton (performing coordination theater while the real system mechanics bypass institutional mechanisms). The analytical observer risks seeing mountain (inevitable structural collapse) when the reality is tangled_rope with multiple causal streams that political choices could have addressed if beneficiaries had accepted different distributional outcomes. The perspectival gap reveals that the constraint's type depends not on physics but on power position: beneficiaries see coordination, victims see extraction, analysts risk naturalizing what is contingent.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) values are derived from beneficiary/victim declarations and exit options. US fiscal authority (beneficiary + arbitrage exit) has d ≈ 0.05-0.10, experiencing f(d) ≈ -0.10 to -0.02 (negative chi, net benefit). Non-reserve economies (victims + trapped) have d ≈ 0.92-0.98, experiencing f(d) ≈ 1.30-1.42 (maximum effective extraction). Capital-mobile institutions (beneficiaries + mobile) have d ≈ 0.15-0.25, experiencing f(d) ≈ 0.05-0.15 (low but positive chi). Allied developed economies (mixed + constrained) have d ≈ 0.55-0.65, experiencing f(d) ≈ 0.75-1.05 (moderate extraction from tangled rope). The analytical observer (analytical/analytical) has d ≈ 0.72, experiencing f(d) ≈ 1.15 (standard analytical perspective moderate chi). Scope modifier σ(S) is global (1.2) for international finance constraints, amplifying effective extractiveness across all perspectives. These directionality relationships are not overridden because the structural data (beneficiaries benefiting from seigniorage, victims trapped by trilemma logic) accurately reflects the real causal flow. The tangled_rope classification depends on the coexistence of genuine coordination function (fixed rates enabled post-war recovery) with asymmetric extraction (seigniorage, inflation transfer, suppressed alternatives).
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by recognizing the genuine coordination function (post-war recovery, trade expansion, capital allocation efficiency) coexisting with genuine asymmetric extraction (seigniorage, inflation transfer, suppressed alternatives for non-beneficiaries). Tangled_rope is the correct classification because BOTH are structurally real, not because of observer position bias. The constraint requires active enforcement (Bretton Woods Articles of Agreement, IMF surveillance, central bank coordination agreements) to maintain the parity system against Triffin pressure — enforcement is visible and acknowledged by all parties. The constraint benefits an identifiable set of agents (US fiscal authority, capital-mobile institutions) and harms another identifiable set (non-reserve-currency economies, the monetary discipline constraint itself). No single observer perspective sees the constraint as pure coordination or pure extraction because the structural reality is hybrid. The analytical observer's mountain perspective instantiates the false summit problem: Triffin's logic is sometimes presented as an immutable mathematical law (trilemma: cannot simultaneously achieve three goals with two instruments), but the trilemma is not a law of nature — it reflects a specific policy choice to prioritize fixed rates and free capital flows while using Keynesian policies that resist deflation. Abandon any of these three choices, and the trilemma dissolves. The appearance of inevitability comes from treating the choices as constraints rather than recognizing them as policy decisions benefiting powerful agents.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    causality_compression_ambiguity,
    'Is the late-1960s collapse driven by one sufficient cause (Triffin dilemma alone) or multiple jointly necessary causes (Triffin + deficits + capital mobility + Keynesian policy consensus)?',
    'Counterfactual historical analysis: could the system have survived without Vietnam deficits but with Triffin logic? Could it have survived without capital mobility despite Triffin + deficits? Each causal stream disabled independently to test sufficiency.',
    'If one sufficient cause: the constraint is a simpler mountain (structural inevitability). If jointly necessary: the constraint is a tangled_rope with multiple upstream causality (policy choices matter, alternatives existed). Classification shifts from mountain to tangled_rope depending on answer.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(causality_compression_ambiguity, conceptual, 'Whether collapse is overdetermined (multiply sufficient) or contingently causal (joint necessity)').

omega_variable(
    policy_deliberation_counterfactual,
    'Could US policymakers in 1960-1968 have chosen a different path (gold-standard-exit preparation, gradual revaluation negotiation, capital control reinforcement) that would have extended or prevented the collapse?',
    'Deep archival analysis of decision-making: what alternatives were considered and explicitly rejected? Were the paths technically feasible but politically unacceptable? What would have been the distributional consequences of each path (who would have borne costs)?',
    'If policy alternatives existed and were deliberately rejected: the overdetermination reflects political choice, not structural inevitability — extractiveness classification shifts upward (more tangled_rope character, less mountain character). If paths existed but were politically infeasible: the overdetermination reflects structural political constraints, not pure economics.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(policy_deliberation_counterfactual, empirical, 'Whether policy alternatives existed and were deliberately rejected or politically infeasible').

omega_variable(
    beneficiary_intentionality_ambiguity,
    'Did US fiscal authority and capital mobile institutions deliberately engineer the Bretton Woods collapse to extract seigniorage and escape monetary discipline, or was the collapse an unintended consequence of multiple independently rational policies?',
    'Evidence from policy statements, internal memos, academic writings: were policymakers aware of Triffin logic? Did they model the collapse scenario? Were capital-mobility-enabling policies chosen despite known destabilizing effects?',
    'If deliberate engineering: the constraint is more extractive (snare-class from some perspectives), the beneficiaries are agents, the suppression includes institutional deception. If unintended consequence: the constraint is more tragic (tangled_rope), the beneficiaries are benefiting from emergent properties, not designed extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_intentionality_ambiguity, empirical, 'Whether the collapse was deliberately engineered or an unintended consequence of independent policies').

omega_variable(
    reading_vs_sibling_foreclosure,
    'Does the overdetermined_composite reading (multiple causal streams) foreclose the triffin_inevitability reading (single sufficient cause) and the punctuated_swap reading (policy intervention can defer collapse)?',
    'Logical analysis: if the system is overdetermined (multiply sufficient causes), is a single-cause reading logically incoherent? If policies can address all causal streams (capital controls, fiscal discipline, reserve system redesign), is the overdetermination actually constraining?',
    'If overdetermined logic forecloses single-cause reading: reading_relations = forecloses. If overdetermined and single-cause readings coexist as different analyst positions: reading_relations = coexists_with. If overdetermined reading shows that policy intervention cannot address all streams simultaneously: reading_relations = influences (constrains the policy intervention reading).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_vs_sibling_foreclosure, conceptual, 'Logical and structural relationships between this reading and sibling readings').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(overdetermined_composite_reading, 1960, 1971).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(odcomp_theater_1960, overdetermined_composite_reading, theater_ratio, 0, 0.32).
narrative_ontology:measurement(odcomp_theater_1965, overdetermined_composite_reading, theater_ratio, 5, 0.4).
narrative_ontology:measurement(odcomp_theater_1971, overdetermined_composite_reading, theater_ratio, 11, 0.48).

% Extraction over time
narrative_ontology:measurement(odcomp_extractiveness_1960, overdetermined_composite_reading, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(odcomp_extractiveness_1965, overdetermined_composite_reading, base_extractiveness, 5, 0.42).
narrative_ontology:measurement(odcomp_extractiveness_1971, overdetermined_composite_reading, base_extractiveness, 11, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(overdetermined_composite_reading, global_infrastructure).
narrative_ontology:affects_constraint(overdetermined_composite_reading, dollar_standard_succession).
narrative_ontology:affects_constraint(overdetermined_composite_reading, petrodollar_substitution_mechanism).
narrative_ontology:affects_constraint(overdetermined_composite_reading, capital_control_regime_shift).

% DUAL FORMULATION NOTE:
% The Bretton Woods collapse decomposes into multiple constraint stories reflecting different causal streams: (1) triffin_dilemma_logic (base extractiveness ≈0.08, mountain) — the mathematical structural feature; (2) vietnam_war_fiscal_cascade (ε≈0.35, tangled_rope) — the deficit-driven monetary expansion; (3) eurodollar_capital_mobility (ε≈0.45, rope) — the technological enabler of arbitrage; (4) overdetermined_composite_reading (ε≈0.58, tangled_rope) — the joint structural determination by all streams. The overdetermined story is downstream of the individual component stories and shows how their interaction creates irreversibility. The successor constraints (dollar standard without gold, petrodollar system, capital control regimes) are all affected by the specific mechanism of collapse that overdetermination reveals.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
