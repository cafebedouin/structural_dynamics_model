% ============================================================================
% CONSTRAINT STORY: hybrid_trigger_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_hybrid_trigger_reading, []).

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
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: hybrid_trigger_reading
 *   human_readable: Bretton Woods Collapse: Structural Contradiction + Contingent Trigger
 *   domain: monetary_economics/international_finance/political_economy
 *
 * SUMMARY:
 *   The Bretton Woods monetary system instantiates a structural contradiction
 *   (Triffin Dilemma: impossible to simultaneously maintain gold
 *   convertibility, domestic price stability, and expanding international
 *   liquidity) that persisted for 27 years before collapse. This reading of
 *   the transition_causality kernel holds that structural contradictions
 *   alone were insufficient to determine when or how the system would fail —
 *   contingent trigger events (U.S. fiscal shock from Vietnam War escalation,
 *   coordinated French gold redemption demands, breakdown of the London Gold
 *   Pool) were necessary actualizers. The hybrid reading splits the
 *   difference from deterministic siblings: not overdetermined (structural
 *   math alone determining collapse) and not purely contingent (random choice
 *   among many possible futures). Rather, the structural contradiction
 *   created a narrowing window of possible equilibria, and specific trigger
 *   events actualized collapse within that window. Different triggers,
 *   different timing, different starting shocks would have produced different
 *   collapse sequences or even temporary stabilization via institutional
 *   redesign (SDR reform, gold price adjustment, reserve basket mechanism).
 *   The extractiveness trajectory (0.22 → 0.72) reflects how contingent
 *   triggers convert slow-burn structural tension into acute extraction
 *   crisis.
 *
 * KEY AGENTS:
 *   - United States (Monetary Hegemony): Primary beneficiary (institutional/arbitrage) — captures seigniorage benefit from dollar reserve status; runs fiscal deficits externalized to subordinate states
 *   - Subordinate Currency States (France, Germany, UK, others): Primary victims (powerless/trapped) — forced to accumulate dollars and accept inflation exported from U.S. monetary expansion; trapped by Cold War alliance and dollar-denominated debt
 *   - Coalition of Deficit States: Organized agents (organized/constrained) — France under de Gaulle and allied states attempting coordinated gold redemption and reserve reform; constrained by Cold War dynamics and fear of triggering system collapse
 *   - Bretton Woods Institutional Apparatus (IMF, World Bank): Institutional maintainers (institutional/arbitrage) — perform ritual compliance with gold standard fiction while system operates as de facto dollar standard; benefit from stability theater
 *   - Speculative Capital (Private Markets): Moderate agents (moderate/constrained) — exploit arbitrage opportunities in gold price controls and forward markets; constrained by regulatory limits and confidence expectations
 *   - Vietnam War Fiscal Shock: Contingent trigger (analytical/analytical) — escalation beginning 1965 accelerated U.S. fiscal deficits and monetary expansion; made structural contradiction acute; required by this reading as necessary actualizer of collapse
 *   - French Gold Coordination: Contingent trigger (organized/constrained) — de Gaulle's call for gold-backed reserve system and coordinated redemption runs; activated latent vulnerabilities in gold stock; made structural contradiction visible
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hybrid_trigger_reading, 0.58).
domain_priors:suppression_score(hybrid_trigger_reading, 0.65).
domain_priors:theater_ratio(hybrid_trigger_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hybrid_trigger_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(hybrid_trigger_reading, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(hybrid_trigger_reading, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hybrid_trigger_reading, tangled_rope).
narrative_ontology:human_readable(hybrid_trigger_reading, "Bretton Woods Collapse: Structural Contradiction + Contingent Trigger").
narrative_ontology:topic_domain(hybrid_trigger_reading, "monetary_economics/international_finance/political_economy").

domain_priors:requires_active_enforcement(hybrid_trigger_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hybrid_trigger_reading, 'e508b48f-2737-4af2-ad53-bc6ed9f0e288').
narrative_ontology:cs_created_at('e508b48f-2737-4af2-ad53-bc6ed9f0e288', '').
narrative_ontology:cs_kernel_codification('e508b48f-2737-4af2-ad53-bc6ed9f0e288', formalized).
narrative_ontology:cs_authority_grounding('e508b48f-2737-4af2-ad53-bc6ed9f0e288', extraction).
narrative_ontology:cs_interpretation_layer_present('e508b48f-2737-4af2-ad53-bc6ed9f0e288').
narrative_ontology:cs_kernel_id(hybrid_trigger_reading, transition_causality).
narrative_ontology:cs_reading_relation('e508b48f-2737-4af2-ad53-bc6ed9f0e288', contingent_choice_reading, coexists_with).
narrative_ontology:cs_reading_relation('e508b48f-2737-4af2-ad53-bc6ed9f0e288', overdetermined_collapse_reading, influences).
narrative_ontology:cs_axiom('e508b48f-2737-4af2-ad53-bc6ed9f0e288', foundational, contradictions_require_contingent_actualizers).
narrative_ontology:cs_axiom_status(contradictions_require_contingent_actualizers, holdable).
narrative_ontology:cs_axiom('e508b48f-2737-4af2-ad53-bc6ed9f0e288', secondary, trigger_timing_affects_institutional_form).
narrative_ontology:cs_axiom_status(trigger_timing_affects_institutional_form, holdable).
narrative_ontology:cs_reference_frame('e508b48f-2737-4af2-ad53-bc6ed9f0e288', gold_backed_convertibility_framework).
narrative_ontology:cs_drift_state('e508b48f-2737-4af2-ad53-bc6ed9f0e288', vietnam_era_1965, gap(authority_erosion, substantial, false)).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hybrid_trigger_reading, united_states_monetary_authority).
narrative_ontology:constraint_beneficiary(hybrid_trigger_reading, dollar_reserve_currency_holders).
narrative_ontology:constraint_victim(hybrid_trigger_reading, subordinate_currency_states).
narrative_ontology:constraint_victim(hybrid_trigger_reading, gold_standard_discipline_collective).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SUBORDINATE CURRENCY STATE (SNARE) — Trapped within dollar standard; cannot exit gold-exchange system without sovereign risk. Bears full cost of U.S. monetary expansion (inflation exported, reserves eroded) while constrained by Bretton Woods commitment and dollar-denominated debt. No exit option beyond collective action (foreclosed by prisoner's dilemma). Maximum extraction from perspective of trapped agent.
constraint_indexing:constraint_classification(hybrid_trigger_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: COALITION OF DEFICIT STATES (TANGLED ROPE) — France under de Gaulle, allied deficit countries. Organized action possible (gold redemption runs, call for gold-backed reserve reform) but constrained by Cold War alliance structure and fear of system collapse. Benefits from stability coordination while bearing extraction cost of monetary subordination. Medium exit cost — can coordinate demand for gold redemption but faces severe diplomatic pressure.
constraint_indexing:constraint_classification(hybrid_trigger_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 3: UNITED STATES MONETARY HEGEMONY (ROPE) — Immediate horizon: Bretton Woods enables seigniorage and monetary flexibility unavailable to gold-standard nations. U.S. dollar as international medium of exchange solves coordination problem (universal numeraire for trade). Net beneficiary with full arbitrage capacity (can exit by abandoning convertibility and absorbing reserve-currency tax). Extraction runs toward this agent.
constraint_indexing:constraint_classification(hybrid_trigger_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: BRETTON WOODS INSTITUTIONAL APPARATUS (PITON) — IMF, World Bank, fixed-parity machinery. Theater ratio (0.48) reflects: genuine coordination function (trade financing, exchange stability) overlaid with performative commitment to gold standard that U.S. treats as optional. The apparatus maintains ritual pretense of gold backing while U.S. conducts fiat monetary policy. Institutional inertia persists because the alternative — acknowledgment that system is fundamentally dollar-based, not gold-backed — requires renegotiation no one wants to initiate. System degrades over interval as subordinate states lose confidence but continue performing compliance.
constraint_indexing:constraint_classification(hybrid_trigger_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: SPECULATIVE CAPITAL (TANGLED ROPE) — Private investors and speculators. Constrained by forward market regulations and expectation management but benefit from arbitrage opportunities (gold price controls enable below-market accumulation until confidence breaks). Mixed extraction: coordination function (price stability incentivizes trade) with asymmetric benefit capture when trigger event collapses peg.
constraint_indexing:constraint_classification(hybrid_trigger_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From civilizational horizon, Triffin Dilemma is a structural mathematical constraint: a reserve currency nation cannot simultaneously maintain gold convertibility, domestic price stability, and international liquidity expansion. The mathematics is invariant. Collapse appears inevitable from this frame. However, the trigger-contingency analysis reveals this as false summit: structural contradiction alone did not collapse the system; specific contingent events (Vietnam escalation, French gold runs) were required actualizers. Without triggers, the system could have persisted in degraded form indefinitely.
constraint_indexing:constraint_classification(hybrid_trigger_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hybrid_trigger_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(hybrid_trigger_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(hybrid_trigger_reading, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(hybrid_trigger_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(hybrid_trigger_reading, TR),
    TR >= 0.70.

:- end_tests(hybrid_trigger_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58, not 0.22 or 0.72): The hybrid reading claims medium extractiveness because the constraint contains both coordination function and asymmetric extraction in non-separable form. The Triffin Dilemma is a genuine coordination problem: subordinate states benefit from fixed-parity trade certainty and dollar numeraire function. But the U.S. solution to the dilemma — unilateral monetary expansion financed by foreign dollar accumulation — is asymmetrically extractive. The measured value (0.58) reflects the mixed regime: real coordination benefits with embedded extraction asymmetry. The trajectory (0.22 → 0.72) shows how triggers convert structural tension into acute extraction. Suppression (0.65): High but not total. Subordinate states face significant barriers to exit (Cold War alliance dependencies, dollar-denominated debt, lack of alternative reserve mechanisms) but not absolute barriers. Some states (France) coordinate demand for reform; others (Germany) accommodate subordination for political reasons. Triggers activate latent organizing capacity. Theater ratio (0.48): Moderate. Bretton Woods machinery performs real coordination functions (trade financing, exchange stability) but overlays these with performative commitment to gold standard that U.S. treats as optional. Theater is lower than pure piton because the coordination function is genuine and non-performative; it is higher than pure rope because the gold-standard fiction requires ritual maintenance. Final theater (0.48) reflects that post-collapse, the theatrical element (pretense of gold backing) was stripped away, revealing the real coordination function underneath.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates perspectival plurality around causality. From the U.S. perspective, the system is rope — solving international coordination problem; beneficiary position obscures extraction function. From subordinate state perspective, the system is snare — trapped by structural vulnerability to U.S. monetary expansion with no escape. From the analytical perspective, structural determination (mountain) appears inevitable until triggers reveal contingency. From the coalition perspective, the constraint is tangled rope with constrained exit options — coordination benefits are real, but extraction costs and the possibility of alternative designs (SDRs, gold price adjustment) create space for organized resistance. The piton perspective shows how institutional inertia maintains the ritual of gold convertibility even as economic function degrades. The perspectival gap between the U.S. beneficiary (rope) and the trapped subordinate (snare) is maximal — the same constraint is experienced as coordination from one position and extraction from another.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality (d) is derived from the agent's structural relationship to the Triffin Dilemma and the triggers. The U.S. monetary authority has d ≈ 0.10 (primary beneficiary with full arbitrage exit capacity) → low χ → rope classification. Subordinate states trapped in dollar accumulation have d ≈ 0.90 (full targets with minimal exit options) → high χ → snare classification. Organized coalition has d ≈ 0.65 (partial victim capacity to organize and demand reform, but constrained by alliance structure) → medium χ → tangled rope. The piton perspective uses d ≈ 0.72 (analytical/institutional, not directly beneficiary or victim, but maintaining a degraded system). The mountain perspective's d ≈ 0.73 (analytical observer) produces the false summit that the engine will detect — the structural contradiction appears as natural law from civilizational horizon, but the contingent trigger requirement reveals it as constructed institutional arrangement. Triggers shift the directionality landscape: Vietnam escalation raises U.S. d (becomes partial target of its own fiscal expansion); French gold runs lower subordinate state d (from 0.95 trapped to 0.80 by organizing exit threat); institutional apparatus d rises (becomes target of its own performance failure).
 *
 * MANDATROPHY ANALYSIS:
 *   This reading resolves mandatrophy by distinguishing the structural contradiction (which is real and ineliminable) from the trigger events (which are contingent and history-dependent). The Triffin Dilemma cannot be solved within the fixed-parity gold-standard frame — this constraint is genuine and mathematical. But the collapse of Bretton Woods in 1971 was not determined solely by the dilemma; it required specific trigger events. The reading models this as tangled rope (not snare) because the contradiction alone could have been managed indefinitely if the triggers had not occurred or had occurred differently. With different fiscal policy (no Vietnam War escalation), different institutional evolution (earlier SDR reform), or different trigger sequencing (gold pool breakdown before coordinated French runs), the system could have evolved into a modified form or persisted in degraded-but-functional state (piton) rather than collapsing. The medium extractiveness (0.58) reflects this manageability. Higher extractiveness (snare, 0.72) would imply the collapse was overdetermined by structural math alone; lower extractiveness (rope, 0.35) would imply the extraction was incidental to genuine coordination. The tangled rope classification at 0.58 says: yes, there is asymmetric extraction (U.S. seigniorage, inflation export), but it was structurally manageable until triggers converted tension into acute crisis.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    trigger_necessity_vs_structural_determination,
    'Were structural contradictions (Triffin Dilemma) sufficient to determine collapse timing and mechanism, or were contingent trigger events (Vietnam War fiscal shock, de Gaulle''s gold redemption calls, London Gold Pool breakdown) necessary actualizers that could have occurred differently or not at all?',
    'Counterfactual historical analysis: (A) without Vietnam escalation, would U.S. fiscal position have held longer and enabled peg maintenance into the 1980s? (B) without French gold coordination, would other deficit states have accepted monetary subordination? (C) what trigger events were historically contingent vs structurally inevitable given the contradiction?',
    'If triggers are necessary: the reading is hybrid (structural contradiction + contingent actualization), ε ≈ 0.58, tangled rope classification. If triggers are epiphenomenal: the reading collapses into overdetermined_collapse_reading, ε → 0.75, snare classification. The difference is whether the collapse was predetermined by structural math or required specific historical contingencies.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(trigger_necessity_vs_structural_determination, conceptual, 'Whether collapse was structurally determined or contingently triggered').

omega_variable(
    counterfactual_stabilization_mechanisms,
    'What alternative institutional designs or policy choices could have stabilized Bretton Woods despite the Triffin Dilemma — e.g., special drawing rights (SDRs) as true reserve asset, gold price adjustment without peg collapse, or alternative reserve basket mechanisms?',
    'Historical policy deliberation analysis: Were these mechanisms proposed and rejected? What prevented their adoption? If institutional design, not structural math, determined the choice, the constraint is more tangled_rope (hybrid) than snare (pure extraction). If mechanisms were technically infeasible or politically unthinkable, structural determination dominates.',
    'If stabilization mechanisms were viable but rejected: extraction and enforcement (U.S. refusal to share seigniorage) dominate; system is snare. If mechanisms were genuinely infeasible: structural contradiction dominates; system is tangled_rope. The ε value is robust either way, but classification type is sensitive.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(counterfactual_stabilization_mechanisms, conceptual, 'Whether structural math or policy choice determined collapse pathway').

omega_variable(
    seigniorage_extraction_quantification,
    'What fraction of U.S. monetary expansion 1965-1971 was seigniorage extraction (monetary expansion beyond domestic needs, financed by foreign dollar accumulation) versus legitimate international liquidity provision?',
    'Monetary aggregates analysis: (A) domestic money supply growth vs inflation-adjusted GDP growth; (B) foreign official dollar holdings accumulation vs U.S. trade deficit; (C) gold outflows relative to seigniorage benefit estimation. Ratio determines whether extraction is primary (snare) or secondary benefit (tangled rope).',
    'If seigniorage >60% of expansion: extraction is primary mechanism, snare classification. If seigniorage <40%: coordination/liquidity function is primary, tangled rope confirmed. The base extractiveness value (0.58) is calibrated to mixed regime (45-50% seigniorage); this omega resolves which direction dominates.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(seigniorage_extraction_quantification, empirical, 'Seigniorage extraction magnitude relative to liquidity provision').

omega_variable(
    hybrid_kernel_reading_identity,
    'Is this constraint best understood as a single hybrid causality (slow-burn structural contradiction + necessary contingent triggers), or should it decompose into separate constraint stories: one for the Triffin Dilemma (structural, ε≈0.08, mountain) and one for the trigger events (contingent, ε≈0.65, snare)?',
    'ε-invariance check: Do the structural contradiction and the trigger event have the same measurement basis and extractiveness profile, or are they empirically and structurally distinct? If distinct, decompose into family of constraints.',
    'If unified constraint: this story stands as-is, tangled_rope, ε=0.58. If decomposed: create two stories linked by network.affects_constraints, each with own ε. The unified reading is mandated by the kernel_context (this is ONE reading of transition_causality kernel). The question is whether the reading''s own internal structure requires decomposition.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(hybrid_kernel_reading_identity, conceptual, 'Whether hybrid causality is one constraint or family of constraints').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hybrid_trigger_reading, 0, 27).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hybrid_theater_1944, hybrid_trigger_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(hybrid_theater_1952, hybrid_trigger_reading, theater_ratio, 8, 0.35).
narrative_ontology:measurement(hybrid_theater_1960, hybrid_trigger_reading, theater_ratio, 16, 0.42).
narrative_ontology:measurement(hybrid_theater_1965, hybrid_trigger_reading, theater_ratio, 21, 0.55).
narrative_ontology:measurement(hybrid_theater_1971, hybrid_trigger_reading, theater_ratio, 27, 0.48).

% Extraction over time
narrative_ontology:measurement(hybrid_extract_1944, hybrid_trigger_reading, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(hybrid_extract_1952, hybrid_trigger_reading, base_extractiveness, 8, 0.42).
narrative_ontology:measurement(hybrid_extract_1960, hybrid_trigger_reading, base_extractiveness, 16, 0.55).
narrative_ontology:measurement(hybrid_extract_1965_vietnam, hybrid_trigger_reading, base_extractiveness, 21, 0.68).
narrative_ontology:measurement(hybrid_extract_1971_collapse, hybrid_trigger_reading, base_extractiveness, 27, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hybrid_trigger_reading, resource_allocation).
narrative_ontology:affects_constraint(hybrid_trigger_reading, triffin_dilemma_structural_necessity).
narrative_ontology:affects_constraint(hybrid_trigger_reading, vietnam_fiscal_shock_trigger).
narrative_ontology:affects_constraint(hybrid_trigger_reading, french_gold_coordination_threshold).

% DUAL FORMULATION NOTE:
% The hybrid_trigger_reading is part of a constraint family decomposed by the ε-invariance principle. The Triffin Dilemma as pure structural constraint (ε≈0.08, mountain) is a separate story instantiating a different reading (overdetermined_collapse_reading). The Vietnam War fiscal shock as pure trigger event (ε≈0.65, snare from subordinate state perspective) is a separate story. The French gold coordination campaign as organized response to subordinate position (ε≈0.52, tangled rope) is a separate story. The hybrid_trigger_reading links these three constraints via network.affects_constraints to show how they co-constitute the collapse causality. The family structure instantiates the kernel_context: different readings emphasize different constraint combinations and causal weights.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(hybrid_trigger_reading, institutional, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
