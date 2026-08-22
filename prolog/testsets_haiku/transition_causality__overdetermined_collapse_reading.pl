% ============================================================================
% CONSTRAINT STORY: transition_causality__overdetermined_collapse_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
    narrative_ontology:measurement_basis/2,
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
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: transition_causality__overdetermined_collapse_reading
 *   human_readable: Bretton Woods Fixed-Rate Regime Collapse (Overdetermined Structural Inevitability Reading)
 *   domain: economic/political/international
 *
 * SUMMARY:
 *   This constraint story instantiates the overdetermined-collapse reading of
 *   the Bretton Woods transition: a structurally inevitable outcome driven by
 *   the mathematical geometry of the Triffin Dilemma, not by policy choices
 *   or contingent triggers. The fixed-rate system faced an irreducible
 *   contradiction — the reserve-currency center must both expand monetary
 *   base (to provide liquidity) and defend gold backing (to maintain
 *   credibility), but these two mandates cannot coexist indefinitely. This
 *   reading treats the system's collapse not as an accident or a preventable
 *   mistake, but as a structural necessity. The reading does NOT claim the
 *   system was bad or that the founders were wrong — only that the
 *   mathematical constraints of the arrangement made perpetual operation
 *   impossible. The claim that this was 'inevitable' is itself contested (the
 *   kernel conflict); the other readings hold that contingent choices or
 *   trigger events could have altered the outcome.
 *
 * KEY AGENTS:
 *   - bretton_woods_central_architect: The hegemon administering the system; faces the core contradiction directly
 *   - reserve_currency_center: Benefits from privilege while constrained by discipline; beneficiary and payer simultaneously
 *   - gold_holding_states: Trapped in collective-action problem; individual rationality triggers systemic collapse
 *   - peripheral_economies: Victims of austerity and capital controls imposed by the regime; absorb adjustment shock when peg breaks
 *   - private_capital_markets: Excluded from governance but sense the contradiction first; their arbitrage accelerates the collapse
 *   - analytical_observer: Examines the constraint's mathematics; measures counterfactual viability
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(transition_causality__overdetermined_collapse_reading, 0.87).
domain_priors:suppression_score(transition_causality__overdetermined_collapse_reading, 0.72).
domain_priors:theater_ratio(transition_causality__overdetermined_collapse_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(transition_causality__overdetermined_collapse_reading, extractiveness, 0.87).
narrative_ontology:constraint_metric(transition_causality__overdetermined_collapse_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(transition_causality__overdetermined_collapse_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(transition_causality__overdetermined_collapse_reading, accessibility_collapse, 0.91).
narrative_ontology:constraint_metric(transition_causality__overdetermined_collapse_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(transition_causality__overdetermined_collapse_reading, mountain).
narrative_ontology:human_readable(transition_causality__overdetermined_collapse_reading, "Bretton Woods Fixed-Rate Regime Collapse (Overdetermined Structural Inevitability Reading)").
narrative_ontology:topic_domain(transition_causality__overdetermined_collapse_reading, "economic/political/international").

domain_priors:emerges_naturally(transition_causality__overdetermined_collapse_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(transition_causality__overdetermined_collapse_reading, 'd6949a9a-8c6b-4015-a0a7-00f9e22118b9').
narrative_ontology:cs_kernel_codification('d6949a9a-8c6b-4015-a0a7-00f9e22118b9', fixed_text).
narrative_ontology:cs_authority_grounding('d6949a9a-8c6b-4015-a0a7-00f9e22118b9', lineage).
narrative_ontology:cs_interpretation_layer_present('d6949a9a-8c6b-4015-a0a7-00f9e22118b9').
narrative_ontology:cs_reading_relation('d6949a9a-8c6b-4015-a0a7-00f9e22118b9', transition_causality__contingent_choice_reading, forecloses).
narrative_ontology:cs_reading_relation('d6949a9a-8c6b-4015-a0a7-00f9e22118b9', transition_causality__hybrid_trigger_reading, forecloses).
narrative_ontology:cs_axiom('d6949a9a-8c6b-4015-a0a7-00f9e22118b9', foundational, triffin_dilemma_mathematically_unsolvable).
narrative_ontology:cs_axiom_status(triffin_dilemma_mathematically_unsolvable, holdable).
narrative_ontology:cs_axiom_grounding('d6949a9a-8c6b-4015-a0a7-00f9e22118b9', triffin_dilemma_mathematically_unsolvable, empirically_contingent).
narrative_ontology:cs_axiom('d6949a9a-8c6b-4015-a0a7-00f9e22118b9', secondary, no_policy_sequence_reconciles_reserve_expansion_and_gold_backing).
narrative_ontology:cs_axiom_status(no_policy_sequence_reconciles_reserve_expansion_and_gold_backing, holdable).
narrative_ontology:cs_axiom_grounding('d6949a9a-8c6b-4015-a0a7-00f9e22118b9', no_policy_sequence_reconciles_reserve_expansion_and_gold_backing, empirically_contingent).
narrative_ontology:cs_reference_frame('d6949a9a-8c6b-4015-a0a7-00f9e22118b9', gold_backed_fixed_parity_discipline).
narrative_ontology:cs_drift_state('d6949a9a-8c6b-4015-a0a7-00f9e22118b9', post_1968_two_tier_gold_market, gap(codification_collapse, severe, false)).
narrative_ontology:cs_created_at('d6949a9a-8c6b-4015-a0a7-00f9e22118b9', '2026-06-12T14:32:00Z').
narrative_ontology:cs_kernel_id(transition_causality__overdetermined_collapse_reading, transition_causality).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(transition_causality__overdetermined_collapse_reading, reserve_currency_center).
narrative_ontology:constraint_beneficiary(transition_causality__overdetermined_collapse_reading, capital_control_enforcing_states).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(transition_causality__overdetermined_collapse_reading, reserve_currency_center).
narrative_ontology:constraint_victim(transition_causality__overdetermined_collapse_reading, deficit_financing_states).
narrative_ontology:constraint_victim(transition_causality__overdetermined_collapse_reading, gold_holding_states).
narrative_ontology:constraint_victim(transition_causality__overdetermined_collapse_reading, peripheral_and_developing_economies).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The hegemon (US) that designed and administered the fixed-rate system, holding reserve-currency privilege and enforcement capacity. Faced structural pressure from the dilemma's own geometry: maintaining gold backing while expanding monetary base to fund imperial commitments required breaking the system's core rule. Exit was not available — the system's collapse is the exit.
narrative_ontology:constraint_stakeholder(transition_causality__overdetermined_collapse_reading, bretton_woods_central_architect, agenda_setter,
    institutional, generational, trapped, global).

% Collects seigniorage and policy autonomy from reserve-currency status while also bearing the discipline of the gold standard peg. The beneficiary position (cheap capital access, international prestige) and the payer position (forced to defend parity, constrained monetary policy) are inseparable in the Triffin structure.
narrative_ontology:constraint_stakeholder(transition_causality__overdetermined_collapse_reading, reserve_currency_center, beneficiary,
    institutional, generational, trapped, global).
narrative_ontology:stakeholder_secondary_role(transition_causality__overdetermined_collapse_reading, reserve_currency_center, payer).

% States dependent on imported capital and dollar-denominated borrowing, constrained by fixed-rate discipline. Carry the cost of defending fixed parties, import inflation from excess-dollar-creation elsewhere, and have limited autonomy to inflate away debt or pursue independent monetary policy. The system extracts capacity for policy experimentation.
narrative_ontology:constraint_stakeholder(transition_causality__overdetermined_collapse_reading, deficit_financing_states, payer,
    powerful, biographical, constrained, national).

% Accumulate dollars as reserves but face declining gold backing. Trapped in a collective-action problem: individually rational to convert dollars to gold (triggering the runs that collapsed the system), but collectively that conversion exhausts the peg and forces revaluation. Identity-locked because monetary reserve status is institutional identity.
narrative_ontology:constraint_stakeholder(transition_causality__overdetermined_collapse_reading, gold_holding_states, payer,
    organized, generational, identity_locked, global).

% Face the constraint of pegged exchange rates they did not design, cannot influence, and must defend through capital controls and policy austerity. When the peg breaks, they absorb the adjustment shock (sudden devaluation, capital flight, inability to borrow). They are the ultimate victims of the system's inevitability — they had no role in its design and no exit option from its collapse.
narrative_ontology:constraint_stakeholder(transition_causality__overdetermined_collapse_reading, peripheral_and_developing_economies, payer,
    powerless, biographical, trapped, global).

% Technical administrators of reserve management and peg defense. Witness the dilemma in real time: defending the peg requires defending gold reserves, which requires defending monetary autonomy, which requires constraining credit expansion. The mathematics of the constraint becomes visible to them first, yet they lack the authority to break the system (that authority lies with heads of state and the hegemon).
narrative_ontology:constraint_stakeholder(transition_causality__overdetermined_collapse_reading, central_banks_operating_under_peg, observer,
    organized, biographical, constrained, national).

% Sit outside the official system but sense structural contradiction before policy-makers admit it. Initiate the runs on gold that actualize the structural contradiction; their early arbitrage signals (capital flight, gold demand) are early-warning systems of inevitable collapse. They would advocate for floating rates and open capital flows; they are excluded from the formal decision-making that defends the peg.
narrative_ontology:constraint_stakeholder(transition_causality__overdetermined_collapse_reading, private_capital_markets, excluded,
    organized, immediate, arbitrage, global).

% Examines the constraint's logic and measures the counterfactual viability of the fixed-rate system under the reading's own lights. Observes that Triffin Dilemma is mathematical, not political — no policy sequence could have sustained the peg indefinitely given the structural contradictions.
narrative_ontology:constraint_stakeholder(transition_causality__overdetermined_collapse_reading, analytical_observer, observer,
    analytical, generational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(transition_causality__overdetermined_collapse_reading, reserve_currency_center).
narrative_ontology:fixing_cost_class(transition_causality__overdetermined_collapse_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a fixed-exchange-rate anchor and international unit of account: enables trade planning, capital flows, and comparative advantage realization by eliminating currency volatility and speculation.
% TRANSFER_FUNCTION: Transfers policy autonomy from peripheral states to the center; transfers seigniorage from gold-holding states to the reserve-currency center; transfers inflation risk globally while concentrating inflation-creation capacity in the center.
% ABSENT_VOICES: Private capital markets and speculators who would exploit the structural contradiction are structurally excluded from the system's governance but not from observing its mathematics. Alternative theories of monetary order (commodity-backed decentralized systems, managed floating) are closed out of policy discourse by the hegemonic position of the Bretton Woods framework.
% DISAPPEARANCE_RATIONALE: If the fixed-rate regime had not existed, international capital flows would have followed market-determined floating rates from the outset, avoiding the accumulation of unsustainable dollar claims and the structural contradictions of the peg. The world rearranges because the regime's collapse was a response to its internal contradictions, not a cause external to it.
% FOUNDING_PROBLEM: Post-WWII chaos: multiple currencies in free fall, trade paralyzed by volatility, no agreed unit of account for international settlement. A fixed-rate system anchored to gold promised stability and prevented competitive devaluation.
% FOUNDING_PROBLEM_CORROBORATION: Economists and economic historians outside the Bretton Woods administration (Triffin, Keynes' successors, 1960s monetary reformers) attested by the 1960s that the founding problem — currency chaos and competitive devaluation — had been replaced by a different problem: the structural impossibility of maintaining fixed rates while expanding monetary base. The original problem Bretton Woods solved was real; its solution created a new structural problem that could not be escaped.
narrative_ontology:disappearance_verdict(transition_causality__overdetermined_collapse_reading, world_rearranges).
narrative_ontology:founding_problem_status(transition_causality__overdetermined_collapse_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(transition_causality__overdetermined_collapse_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(transition_causality__overdetermined_collapse_reading, 'none', 1).
narrative_ontology:epsilon_provenance(transition_causality__overdetermined_collapse_reading, 0.87, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(transition_causality__overdetermined_collapse_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(transition_causality__overdetermined_collapse_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(transition_causality__overdetermined_collapse_reading, ExtMetricName, E),
    domain_priors:suppression_score(transition_causality__overdetermined_collapse_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(transition_causality__overdetermined_collapse_reading),
    narrative_ontology:constraint_metric(transition_causality__overdetermined_collapse_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(transition_causality__overdetermined_collapse_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(transition_causality__overdetermined_collapse_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises monotonically from 0.35 (early system, real coordination function evident) to 0.87 (late system, coordination function attenuated, pure maintenance of peg dominating). This reading interprets the measurement as extraction because, under this reading, the system's real coordination benefit (stable unit of account, reduced currency volatility) was exhausted by the mid-1960s, while the costs of defending the peg mounted. Theater ratio rises slowly (0.08 to 0.28) because the coordination function, though diminished, never fully disappeared — central banks continued defending parity as if the founding problem required it, even as the actual constraint became purely mathematical. Suppression rises sharply and plateaus (0.42 to 0.73) because defending the peg required increasing capital controls and austerity, but the maximum suppression effort could not overcome the Triffin math. Accessibility collapse is high throughout (0.78 to 0.91) because floating rates and capital liberation were not available as policy choices — the system's own logic locked them out. Resistance rises late (0.15 to 0.62) as the system's contradictions became visible to market participants and some policymakers, but resistance came too late to alter the structural trajectory. The coercion grid shows that suppression pressure was highest on peripheral economies and gold-holding states, while structural-level contradiction pressure affected the center itself — the asymmetry models the overdetermined reading's claim that the center was as trapped as the periphery, though for different reasons.
 *
 * PERSPECTIVAL GAP:
 *   From the hegemon's seat, the system could have been defended indefinitely if other states had accepted perpetual dollar accumulation and foregone gold conversion — the collapse appears as a failure of international cooperation, not structural necessity. From the gold-holding states' seat, the system was a trap: individually rational to convert dollars to gold (signaling doubt), collectively rational to hold dollars (preserving the peg), but the trap had no cooperative equilibrium. From peripheral economies' seat, the system was pure extraction — a fixed parity they could not influence, defended through austerity they did not choose, collapsing in devaluation they could not prevent. The analytical seat observes that all three perspectives are locally rational responses to a globally unsolvable contradiction. The engine computes this divergence as difference in directionality: the hegemon sits near-symmetric (benefits and constrained equally), gold-holders sit at high target (identity-locked, forced to hold depreciating assets), periphery sits at full target (trapped, payers, powerless). This reading claims the mathematical constraint makes all three positions equally inevitable — none could have chosen differently.
 *
 * DIRECTIONALITY LOGIC:
 *   The reserve-currency center is authored as simultaneously beneficiary (collects seigniorage, policy prestige) and payer (bound by peg discipline, forced into impossible choice between gold defense and monetary expansion). This is NOT a contradiction — it models the Triffin Dilemma's core: the beneficiary position AND the payer position are inseparable consequences of the same structural arrangement. Gold-holding states are targets (identity_locked because reserve status is institutional identity; trapped because converting dollars to gold triggers the collapse each individual state fears). Peripheral economies are targets (powerless, constrained, payers of austerity costs). The center is itself trapped because breaking the system is the only exit, and that exit IS the collapse. Under this reading, there is no seat with a viable exit path — the entire structure converges on collapse as the unique attractor.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading interprets the Bretton Woods system as having resolved its founding problem (post-war currency chaos, competitive devaluation) by 1950–1956, leaving only the maintenance of the peg and the extraction it enabled. By the 1960s, the founding problem was dead (stable international currency system achieved), but the system persisted and extracted because no actor had the authority to dissolve it unilaterally. The hegemon could not abandon gold backing without triggering the very runs it sought to prevent. Peripheral states could not exit without capital controls. Gold-holders could not exit without cascading the collapse. The system persisted in performance mode (defending parity for its own sake) rather than function mode (solving currency instability) — classic mandatrophy. Yet this reading treats even the mandatrophy as structurally inevitable: the mathematical constraint prevented any alternative end-state. The constraint was classified as mountain (not snare or piton) because under this reading, the collapse was not contingent on anyone's continued extraction effort — it would have occurred even if every actor acted in perfect cooperation to preserve the system, because the mathematics permitted no stable fixed-rate regime.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    structural_inevitability_vs_counterfactual_policy_space,
    'Could a different policy sequence (higher US deficit discipline, earlier reserve diversification, managed float instead of peg defense) have sustained the fixed-rate system indefinitely?',
    'Formal modeling of the Triffin constraint: if models show no policy sequence satisfies both reserve-center expansion and gold-backing credibility indefinitely, inevitability claim is supported; if models show feasible alternatives, contingency reading gains ground.',
    'If inevitability holds, the collapse was necessary; if alternatives exist, the reading should reclassify to hybrid_trigger or contingent_choice. Directionality would shift: if contingent, the hegemon becomes a volitional payer (not trapped); if inevitable, all seats remain trapped.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(structural_inevitability_vs_counterfactual_policy_space, empirical, 'Whether structural mathematics or policy choices determined the system''s collapse path.').

omega_variable(
    reserve_center_agency_under_triffin_constraint,
    'Did the hegemon face a genuine choice between defending the peg and expanding monetary base, or was one of these paths already foreclosed by structural pressures (wars, domestic politics, capital flows)?',
    'Archival analysis of policymaker deliberations (IMF records, US Treasury meeting minutes, central-bank correspondence); counterfactual modeling of constraint relaxation (what if the hegemon had chosen austerity instead of deficit spending).',
    'If the hegemon faced a genuine choice, the reading should shift toward contingent_choice; if structural pressures made one path inevitable, the overdetermined reading strengthens. Beneficiary/payer directionality of reserve_currency_center depends on whether expansion was volitional or forced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reserve_center_agency_under_triffin_constraint, empirical, 'The degree of agency the hegemon retained given structural constraints.').

omega_variable(
    kernel_boundary_transition_causality_vs_regime_design,
    'Is the contest over the Bretton Woods collapse (inevitability vs. contingency) a different kernel than the contest over whether the system should have been designed differently (better alternatives available at t0 1944)?',
    'Semantic and logical analysis: the transition-causality kernel addresses WHETHER collapse was unavoidable given the system as designed; the design-alternatives kernel would address WHETHER a better design existed. These are distinct — one can claim the actual design was inevitable-to-collapse while also claiming better designs were feasible.',
    'If the kernels are distinct, this story should clarify it is about transition causality (inevitable given design), not about design quality (whether alternatives were theoretically possible). Scope of the reading would narrow: from ''the fixed-rate system could never work'' to ''the system as actually designed could not persist indefinitely.''',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_boundary_transition_causality_vs_regime_design, conceptual, 'The boundary between the transition-causality kernel and the regime-design kernel.').

omega_variable(
    mountain_classification_under_contested_inevitability,
    'Can a constraint be classified as mountain (natural/inevitable) when its inevitability is the subject of live academic and policy dispute, and beneficiaries exist for maintaining it?',
    'Epistemological analysis: mountains are constraints whose persistence follows from irreducible physical/logical limits, not from anyone''s extraction effort. If the Triffin Dilemma is mathematical (not institutional), it qualifies even under dispute; if its inevitability depends on institutional choices (not pure math), it does not.',
    'If the constraint is properly mountain, FSM (false-summit detection) fires because beneficiaries are declared; the engine reclassifies unless the mountain claim is robust. If the constraint is actually snare or tangled_rope, the false-summit reclassification is correct. This omega documents the ambiguity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mountain_classification_under_contested_inevitability, conceptual, 'Whether structural mathematical inevitability is sufficient for mountain classification when the structure is institutionally embedded and contested.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(transition_causality__overdetermined_collapse_reading, 1944, 1973).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tran_tr_t1944, transition_causality__overdetermined_collapse_reading, theater_ratio, 1944, 0.08).
narrative_ontology:measurement_basis(tran_tr_t1944, observed).
narrative_ontology:measurement(tran_tr_t1956, transition_causality__overdetermined_collapse_reading, theater_ratio, 1956, 0.12).
narrative_ontology:measurement_basis(tran_tr_t1956, observed).
narrative_ontology:measurement(tran_tr_t1960, transition_causality__overdetermined_collapse_reading, theater_ratio, 1960, 0.16).
narrative_ontology:measurement_basis(tran_tr_t1960, observed).
narrative_ontology:measurement(tran_tr_t1965, transition_causality__overdetermined_collapse_reading, theater_ratio, 1965, 0.21).
narrative_ontology:measurement_basis(tran_tr_t1965, observed).
narrative_ontology:measurement(tran_tr_t1968, transition_causality__overdetermined_collapse_reading, theater_ratio, 1968, 0.26).
narrative_ontology:measurement_basis(tran_tr_t1968, observed).
narrative_ontology:measurement(tran_tr_t1971, transition_causality__overdetermined_collapse_reading, theater_ratio, 1971, 0.27).
narrative_ontology:measurement_basis(tran_tr_t1971, observed).
narrative_ontology:measurement(tran_tr_t1973, transition_causality__overdetermined_collapse_reading, theater_ratio, 1973, 0.28).
narrative_ontology:measurement_basis(tran_tr_t1973, observed).

% Extraction over time
narrative_ontology:measurement(tran_be_t1944, transition_causality__overdetermined_collapse_reading, base_extractiveness, 1944, 0.35).
narrative_ontology:measurement_basis(tran_be_t1944, observed).
narrative_ontology:measurement(tran_be_t1956, transition_causality__overdetermined_collapse_reading, base_extractiveness, 1956, 0.52).
narrative_ontology:measurement_basis(tran_be_t1956, observed).
narrative_ontology:measurement(tran_be_t1960, transition_causality__overdetermined_collapse_reading, base_extractiveness, 1960, 0.64).
narrative_ontology:measurement_basis(tran_be_t1960, observed).
narrative_ontology:measurement(tran_be_t1965, transition_causality__overdetermined_collapse_reading, base_extractiveness, 1965, 0.75).
narrative_ontology:measurement_basis(tran_be_t1965, observed).
narrative_ontology:measurement(tran_be_t1968, transition_causality__overdetermined_collapse_reading, base_extractiveness, 1968, 0.82).
narrative_ontology:measurement_basis(tran_be_t1968, observed).
narrative_ontology:measurement(tran_be_t1971, transition_causality__overdetermined_collapse_reading, base_extractiveness, 1971, 0.88).
narrative_ontology:measurement_basis(tran_be_t1971, observed).
narrative_ontology:measurement(tran_be_t1973, transition_causality__overdetermined_collapse_reading, base_extractiveness, 1973, 0.87).
narrative_ontology:measurement_basis(tran_be_t1973, observed).

% Suppression requirement over time
narrative_ontology:measurement(tran_su_t1944, transition_causality__overdetermined_collapse_reading, suppression_requirement, 1944, 0.42).
narrative_ontology:measurement_basis(tran_su_t1944, observed).
narrative_ontology:measurement(tran_su_t1956, transition_causality__overdetermined_collapse_reading, suppression_requirement, 1956, 0.54).
narrative_ontology:measurement_basis(tran_su_t1956, observed).
narrative_ontology:measurement(tran_su_t1960, transition_causality__overdetermined_collapse_reading, suppression_requirement, 1960, 0.62).
narrative_ontology:measurement_basis(tran_su_t1960, observed).
narrative_ontology:measurement(tran_su_t1965, transition_causality__overdetermined_collapse_reading, suppression_requirement, 1965, 0.68).
narrative_ontology:measurement_basis(tran_su_t1965, observed).
narrative_ontology:measurement(tran_su_t1968, transition_causality__overdetermined_collapse_reading, suppression_requirement, 1968, 0.71).
narrative_ontology:measurement_basis(tran_su_t1968, observed).
narrative_ontology:measurement(tran_su_t1971, transition_causality__overdetermined_collapse_reading, suppression_requirement, 1971, 0.73).
narrative_ontology:measurement_basis(tran_su_t1971, observed).
narrative_ontology:measurement(tran_su_t1973, transition_causality__overdetermined_collapse_reading, suppression_requirement, 1973, 0.72).
narrative_ontology:measurement_basis(tran_su_t1973, observed).

% Leveled coercion grid (OQ-93): 32/32 authored points at t0=1944, tn=1973
narrative_ontology:measurement(tran_grid_01, transition_causality__overdetermined_collapse_reading, accessibility_collapse(class), 1944, 0.65).
narrative_ontology:measurement(tran_grid_02, transition_causality__overdetermined_collapse_reading, accessibility_collapse(class), 1973, 0.92).
narrative_ontology:measurement(tran_grid_03, transition_causality__overdetermined_collapse_reading, accessibility_collapse(individual), 1944, 0.58).
narrative_ontology:measurement(tran_grid_04, transition_causality__overdetermined_collapse_reading, accessibility_collapse(individual), 1973, 0.89).
narrative_ontology:measurement(tran_grid_05, transition_causality__overdetermined_collapse_reading, accessibility_collapse(organizational), 1944, 0.72).
narrative_ontology:measurement(tran_grid_06, transition_causality__overdetermined_collapse_reading, accessibility_collapse(organizational), 1973, 0.88).
narrative_ontology:measurement(tran_grid_07, transition_causality__overdetermined_collapse_reading, accessibility_collapse(structural), 1944, 0.78).
narrative_ontology:measurement(tran_grid_08, transition_causality__overdetermined_collapse_reading, accessibility_collapse(structural), 1973, 0.91).
narrative_ontology:measurement(tran_grid_09, transition_causality__overdetermined_collapse_reading, resistance(class), 1944, 0.18).
narrative_ontology:measurement(tran_grid_10, transition_causality__overdetermined_collapse_reading, resistance(class), 1973, 0.54).
narrative_ontology:measurement(tran_grid_11, transition_causality__overdetermined_collapse_reading, resistance(individual), 1944, 0.12).
narrative_ontology:measurement(tran_grid_12, transition_causality__overdetermined_collapse_reading, resistance(individual), 1973, 0.48).
narrative_ontology:measurement(tran_grid_13, transition_causality__overdetermined_collapse_reading, resistance(organizational), 1944, 0.22).
narrative_ontology:measurement(tran_grid_14, transition_causality__overdetermined_collapse_reading, resistance(organizational), 1973, 0.58).
narrative_ontology:measurement(tran_grid_15, transition_causality__overdetermined_collapse_reading, resistance(structural), 1944, 0.15).
narrative_ontology:measurement(tran_grid_16, transition_causality__overdetermined_collapse_reading, resistance(structural), 1973, 0.62).
narrative_ontology:measurement(tran_grid_17, transition_causality__overdetermined_collapse_reading, stakes_inflation(class), 1944, 0.18).
narrative_ontology:measurement(tran_grid_18, transition_causality__overdetermined_collapse_reading, stakes_inflation(class), 1973, 0.71).
narrative_ontology:measurement(tran_grid_19, transition_causality__overdetermined_collapse_reading, stakes_inflation(individual), 1944, 0.12).
narrative_ontology:measurement(tran_grid_20, transition_causality__overdetermined_collapse_reading, stakes_inflation(individual), 1973, 0.68).
narrative_ontology:measurement(tran_grid_21, transition_causality__overdetermined_collapse_reading, stakes_inflation(organizational), 1944, 0.28).
narrative_ontology:measurement(tran_grid_22, transition_causality__overdetermined_collapse_reading, stakes_inflation(organizational), 1973, 0.79).
narrative_ontology:measurement(tran_grid_23, transition_causality__overdetermined_collapse_reading, stakes_inflation(structural), 1944, 0.32).
narrative_ontology:measurement(tran_grid_24, transition_causality__overdetermined_collapse_reading, stakes_inflation(structural), 1973, 0.84).
narrative_ontology:measurement(tran_grid_25, transition_causality__overdetermined_collapse_reading, suppression(class), 1944, 0.51).
narrative_ontology:measurement(tran_grid_26, transition_causality__overdetermined_collapse_reading, suppression(class), 1973, 0.68).
narrative_ontology:measurement(tran_grid_27, transition_causality__overdetermined_collapse_reading, suppression(individual), 1944, 0.62).
narrative_ontology:measurement(tran_grid_28, transition_causality__overdetermined_collapse_reading, suppression(individual), 1973, 0.71).
narrative_ontology:measurement(tran_grid_29, transition_causality__overdetermined_collapse_reading, suppression(organizational), 1944, 0.44).
narrative_ontology:measurement(tran_grid_30, transition_causality__overdetermined_collapse_reading, suppression(organizational), 1973, 0.75).
narrative_ontology:measurement(tran_grid_31, transition_causality__overdetermined_collapse_reading, suppression(structural), 1944, 0.38).
narrative_ontology:measurement(tran_grid_32, transition_causality__overdetermined_collapse_reading, suppression(structural), 1973, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(transition_causality__overdetermined_collapse_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(transition_causality__overdetermined_collapse_reading, 0.18).
narrative_ontology:affects_constraint(transition_causality__overdetermined_collapse_reading, transition_causality__contingent_choice_reading).
narrative_ontology:affects_constraint(transition_causality__overdetermined_collapse_reading, transition_causality__hybrid_trigger_reading).
narrative_ontology:affects_constraint(transition_causality__overdetermined_collapse_reading, triffin_dilemma_structural_geometry).
narrative_ontology:affects_constraint(transition_causality__overdetermined_collapse_reading, reserve_currency_privilege_extraction).

% DUAL FORMULATION NOTE:
% The transition_causality kernel decomposes into three constraint stories corresponding to three competing readings: overdetermined_collapse_reading (this file, claims structural inevitability), contingent_choice_reading (claims policy alternatives existed), and hybrid_trigger_reading (claims structural contradictions required contingent triggers). Each reading instantiates a different constraint with a different epsilon, beneficiary structure, and strategic directionality. They share the same kernel (Bretton Woods transition) but diverge on the causal mechanism. The network links document this family relationship; the three stories are linked via affects_constraints and should be read as a triad, not as independent assessments.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(transition_causality__overdetermined_collapse_reading, institutional, 0.52).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
