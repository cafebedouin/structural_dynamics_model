% ============================================================================
% CONSTRAINT STORY: monetary_anchor_principle__overdetermined_composite_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_monetary_anchor_principle__overdetermined_composite_reading, []).

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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
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
 *   constraint_id: monetary_anchor_principle__overdetermined_composite_reading
 *   human_readable: Monetary Anchor Principle â Overdetermined Composite Reading
 *   domain: monetary economics / political economy / international finance
 *
 * SUMMARY:
 *   This constraint story instantiates the overdetermined composite reading
 *   of the monetary_anchor_principle kernel. It models the Bretton Woods
 *   gold-exchange standard (1944â1971) not as pure coordination nor as a
 *   discrete policy choice, but as a tangled rope: a genuine international
 *   monetary coordination function (fixed parities, liquidity provision) that
 *   structurally enabled asymmetric extraction favoring US fiscal capacity at
 *   the expense of monetary discipline. The collapse is read as inevitable by
 *   the late 1960s due to the convergence of the Triffin dilemma, Vietnam War
 *   deficits, Keynesian policy consensus, and technological capital mobility
 *   â upstream mountain-like pressures feeding an entangled policy space.
 *   This reading coexists with the punctuated_swap reading (discrete 1971
 *   choice) and influences the triffin_inevitability reading by subsuming the
 *   Triffin dilemma as necessary but insufficient.
 *
 * KEY AGENTS:
 *   - us_fiscal_state (agenda_setter/institutional/arbitrage): Administers the gold-dollar peg, captures seigniorage and fiscal space
 *   - foreign_central_banks (payer/organized/constrained): Bear reserve erosion and inflation tax
 *   - monetary_hawks (payer/moderate/constrained): Lose price stability anchor to fiscal expansion
 *   - multilateral_traders (beneficiary/powerful/constrained): Gain coordination benefit from fixed rates
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(monetary_anchor_principle__overdetermined_composite_reading, 0.85).
domain_priors:suppression_score(monetary_anchor_principle__overdetermined_composite_reading, 0.7).
domain_priors:theater_ratio(monetary_anchor_principle__overdetermined_composite_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(monetary_anchor_principle__overdetermined_composite_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(monetary_anchor_principle__overdetermined_composite_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(monetary_anchor_principle__overdetermined_composite_reading, theater_ratio, 0.58).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(monetary_anchor_principle__overdetermined_composite_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(monetary_anchor_principle__overdetermined_composite_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(monetary_anchor_principle__overdetermined_composite_reading, tangled_rope).
narrative_ontology:human_readable(monetary_anchor_principle__overdetermined_composite_reading, "Monetary Anchor Principle â Overdetermined Composite Reading").
narrative_ontology:topic_domain(monetary_anchor_principle__overdetermined_composite_reading, "monetary economics / political economy / international finance").

domain_priors:requires_active_enforcement(monetary_anchor_principle__overdetermined_composite_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(monetary_anchor_principle__overdetermined_composite_reading, '5b7bcfa4-efab-4ff6-a922-a5a2b812636a').
narrative_ontology:cs_kernel_codification('5b7bcfa4-efab-4ff6-a922-a5a2b812636a', formalized).
narrative_ontology:cs_authority_grounding('5b7bcfa4-efab-4ff6-a922-a5a2b812636a', lineage).
narrative_ontology:cs_interpretation_layer_present('5b7bcfa4-efab-4ff6-a922-a5a2b812636a').
narrative_ontology:cs_reading_relation('5b7bcfa4-efab-4ff6-a922-a5a2b812636a', monetary_anchor_principle__punctuated_swap_reading, coexists_with).
narrative_ontology:cs_reading_relation('5b7bcfa4-efab-4ff6-a922-a5a2b812636a', monetary_anchor_principle__triffin_inevitability_reading, influences).
narrative_ontology:cs_axiom('5b7bcfa4-efab-4ff6-a922-a5a2b812636a', foundational, regime_change_is_structurally_overdetermined).
narrative_ontology:cs_axiom_status(regime_change_is_structurally_overdetermined, holdable).
narrative_ontology:cs_axiom_grounding('5b7bcfa4-efab-4ff6-a922-a5a2b812636a', regime_change_is_structurally_overdetermined, empirically_contingent).
narrative_ontology:cs_axiom('5b7bcfa4-efab-4ff6-a922-a5a2b812636a', foundational, fiscal_liquidity_imperative_overrides_gold_constraint).
narrative_ontology:cs_axiom_status(fiscal_liquidity_imperative_overrides_gold_constraint, holdable).
narrative_ontology:cs_axiom_grounding('5b7bcfa4-efab-4ff6-a922-a5a2b812636a', fiscal_liquidity_imperative_overrides_gold_constraint, empirically_contingent).
narrative_ontology:cs_reference_frame('5b7bcfa4-efab-4ff6-a922-a5a2b812636a', gold_parity_fixed_exchange_anchor).
narrative_ontology:cs_drift_state('5b7bcfa4-efab-4ff6-a922-a5a2b812636a', late_sixties_crisis_era, gap(axiom_overriding, severe, false)).
narrative_ontology:cs_created_at('5b7bcfa4-efab-4ff6-a922-a5a2b812636a', '').
narrative_ontology:cs_kernel_id(monetary_anchor_principle__overdetermined_composite_reading, monetary_anchor_principle).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(monetary_anchor_principle__overdetermined_composite_reading, us_fiscal_state).
narrative_ontology:constraint_beneficiary(monetary_anchor_principle__overdetermined_composite_reading, multilateral_traders).
narrative_ontology:constraint_victim(monetary_anchor_principle__overdetermined_composite_reading, foreign_central_banks).
narrative_ontology:constraint_victim(monetary_anchor_principle__overdetermined_composite_reading, monetary_hawks).
narrative_ontology:constraint_vindicates(monetary_anchor_principle__overdetermined_composite_reading, triffin_dilemma_hypothesis).
narrative_ontology:constraint_vindicates(monetary_anchor_principle__overdetermined_composite_reading, reserve_currency_asymmetry_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers the Bretton Woods gold-exchange standard, sets the dollar-gold parity, and finances escalating fiscal deficits for the Vietnam War and Great Society programs. It captures seigniorage and fiscal space because foreign central banks are structurally induced to hold dollars, effectively financing US spending without immediate devaluation or domestic austerity.
narrative_ontology:constraint_stakeholder(monetary_anchor_principle__overdetermined_composite_reading, us_fiscal_state, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(monetary_anchor_principle__overdetermined_composite_reading, us_fiscal_state, beneficiary).

% Accumulate dollar reserves to manage fixed exchange rates under the Bretton Woods parity grid. They bear the erosion of reserve value as US gold coverage declines. Mass conversion to gold is technically available but politically and economically self-destructive, because exercising it would collapse the dollar and destroy the value of their remaining dollar assets.
narrative_ontology:constraint_stakeholder(monetary_anchor_principle__overdetermined_composite_reading, foreign_central_banks, payer,
    organized, biographical, constrained, global).

% Domestic and international advocates of price stability and hard money. They experience the erosion of the gold anchor as fiscal expansion overrides parity discipline. They lack institutional veto points over deficit spending and suffer the inflation tax and redistribution of wealth to deficit-financed state programs.
narrative_ontology:constraint_stakeholder(monetary_anchor_principle__overdetermined_composite_reading, monetary_hawks, payer,
    moderate, biographical, constrained, national).

% International commercial banks, trading firms, and export sectors that rely on fixed exchange rates to reduce currency risk and transaction costs. They benefit from the coordination function of the monetary anchor, even as the same structure is leveraged to finance fiscal deficits that ultimately destabilize the parity.
narrative_ontology:constraint_stakeholder(monetary_anchor_principle__overdetermined_composite_reading, multilateral_traders, beneficiary,
    powerful, biographical, constrained, global).

% Later academic and policy analysts who interpret the collapse as an overdetermined structural convergence rather than a discrete policy choice. They observe the entanglement of the Triffin dilemma, Vietnam deficits, Keynesian consensus, and capital mobility without being subject to the constraint's incentives.
narrative_ontology:constraint_stakeholder(monetary_anchor_principle__overdetermined_composite_reading, structural_analysts, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(monetary_anchor_principle__overdetermined_composite_reading, us_fiscal_state).
narrative_ontology:fixing_cost_class(monetary_anchor_principle__overdetermined_composite_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a stable international monetary order with fixed exchange rates and a common reserve asset, reducing currency risk and transaction costs for cross-border trade and investment.
% TRANSFER_FUNCTION: Transfers purchasing power and inflation risk from foreign dollar holders and domestic monetary discipline constituencies to the US fiscal state, enabling persistent deficit financing without immediate devaluation or austerity.
% ABSENT_VOICES: Floating-rate advocates, bancor and SDR proponents, and peripheral developing countries subject to IMF conditionality were structurally marginalized in the design and reform negotiations. They would have advocated for alternative reserve mechanisms or symmetric adjustment obligations.
% DISAPPEARANCE_RATIONALE: If the gold-dollar peg and its enforcement architecture had disappeared overnight in 1960, the international monetary order would have reorganized: exchange rates would have adjusted, US fiscal deficits would have faced harder budget constraints earlier, and global liquidity would have shifted to alternative reserves or floating. The 1971 collapse confirms that the arrangements depended on the constraint's active maintenance.
% FOUNDING_PROBLEM: The interwar period demonstrated the dangers of floating exchange rates, competitive devaluations, and beggar-thy-neighbor policies. The monetary anchor principle was built to restore credible parities, stable exchange expectations, and a reliable international payment mechanism.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem is corroborated by interwar historiography and the Bretton Woods conference records. Its 'dead' status is attested by Robert Triffin (from outside the US fiscal beneficiary seat) and by the French government's gold conversion challenges in the 1960s, which explicitly argued that the arrangement had outlived its stability function and become an engine of asymmetric extraction.
narrative_ontology:disappearance_verdict(monetary_anchor_principle__overdetermined_composite_reading, world_rearranges).
narrative_ontology:founding_problem_status(monetary_anchor_principle__overdetermined_composite_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(monetary_anchor_principle__overdetermined_composite_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(monetary_anchor_principle__overdetermined_composite_reading, 'none', 1).
narrative_ontology:epsilon_provenance(monetary_anchor_principle__overdetermined_composite_reading, 0.85, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(monetary_anchor_principle__overdetermined_composite_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(monetary_anchor_principle__overdetermined_composite_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(monetary_anchor_principle__overdetermined_composite_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.85) because the reserve currency asymmetry allowed sustained deficit financing decoupled from adjustment. Suppression (0.70) reflects active enforcement: capital controls, IMF surveillance, gold-pool interventions, and diplomatic pressure to maintain the parity fiction. Theater ratio (0.58) captures the performative maintenance of the gold window while actual gold coverage collapsed. Accessibility collapse (0.65) registers that alternatives (SDRs, bancor, early floating) were institutionally suppressed or ideologically excluded. Resistance (0.60) includes French gold conversions and domestic hawkish opposition. The metric trajectory shows extraction rising monotonically as structural pressures converged, suppression hardening through the 1960s, and theater peaking just before the 1971 closure. The claim is tangled_rope: genuine coordination for trade, asymmetric extraction for fiscal capacity.
 *
 * PERSPECTIVAL GAP:
 *   The us_fiscal_state seat experiences the constraint as a necessary and legitimate coordinating architecture it administers; the foreign_central_banks and monetary_hawks seats experience the same structure as an increasingly extractive asymmetry where they finance US deficits. The engine will compute high effective extraction for the payer seats and low or negative extraction for the beneficiary seats. The multilateral_traders seat occupies a middle zone: genuine coordination benefit, but indirect cost via systemic instability.
 *
 * DIRECTIONALITY LOGIC:
 *   us_fiscal_state is structurally a beneficiary (low d): it collects seigniorage and fiscal space from the reserve currency role. multilateral_traders are beneficiaries of coordination (low d). foreign_central_banks and monetary_hawks are targets (high d): they bear the inflation tax and reserve erosion. The exit asymmetry is extreme â the US can alter the rules (arbitrage), while foreign holders face constrained exit because exercising it destroys their own asset values.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem â preventing interwar monetary chaos â was genuine and largely solved by the 1950s. By the 1960s, the constraint persisted beyond its solving function, morphing into a vehicle for fiscal extraction. The R5 interview marks founding_problem_status as dead, while the disappearance_verdict is world_rearranges, flagging the classic mandatrophy pattern: a coordination scaffold that became a tangled rope and finally collapsed under its own extraction load. The classification as tangled_rope rather than snare preserves the genuine coordination function that multilateral_traders depended on; classifying it as rope would ignore the asymmetric extraction, while classifying it as piton would miss the active, high-extraction operation before collapse.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    fiscal_extraction_vs_trade_coordination,
    'Is the monetary anchor principle separable into a pure coordination function for trade and a distinct extraction function for fiscal capacity, or are the two structurally fused?',
    'Historical counterfactual analysis of alternative reserve regimes (e.g., Keynes''s bancor, an SDR-centered system) to determine whether the US fiscal privilege was an avoidable design flaw or inherent to the anchor architecture.',
    'If separable, the constraint is more cleanly tangled rope; if fused, it trends toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fiscal_extraction_vs_trade_coordination, conceptual, 'Coordination-extraction separability of the gold-exchange standard').

omega_variable(
    overdetermination_contingency_boundary,
    'Does the historical record support structural overdetermination (multiple necessary causes converging), or does the collapse reduce to a small number of contingent policy decisions that could have been otherwise?',
    'Archival and econometric analysis of decision nodes (1965 Vietnam escalation, 1967 sterling devaluation contagion, 1968 gold rush) to assess elasticity of outcomes.',
    'If contingent, the high extractiveness metric should be reattributed to agency rather than structure, lowering accessibility_collapse and shifting agenda_setter directionality.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(overdetermination_contingency_boundary, empirical, 'Structural inevitability versus contingent policy choice in regime collapse').

omega_variable(
    reading_sibling_relation_underdetermination,
    'Does the overdetermined composite reading subsume the triffin_inevitability reading as a component, or do they represent competing causal ontologies that assign different structural primacy to liquidity dilemmas versus fiscal policy?',
    'Comparative historiographical synthesis assessing whether Triffin-alone explanations survive the inclusion of Vietnam deficit and capital mobility variables.',
    'Determines whether the reading_relations entry should be coexists_with or influences, and whether the constraint family network topology is hierarchical or flat.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_sibling_relation_underdetermination, conceptual, 'Committer underdetermination between overdetermined and Triffin readings').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(monetary_anchor_principle__overdetermined_composite_reading, 0, 27).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mone_tr_t0, monetary_anchor_principle__overdetermined_composite_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(mone_tr_t5, monetary_anchor_principle__overdetermined_composite_reading, theater_ratio, 5, 0.15).
narrative_ontology:measurement(mone_tr_t9, monetary_anchor_principle__overdetermined_composite_reading, theater_ratio, 9, 0.22).
narrative_ontology:measurement(mone_tr_t14, monetary_anchor_principle__overdetermined_composite_reading, theater_ratio, 14, 0.35).
narrative_ontology:measurement(mone_tr_t18, monetary_anchor_principle__overdetermined_composite_reading, theater_ratio, 18, 0.48).
narrative_ontology:measurement(mone_tr_t23, monetary_anchor_principle__overdetermined_composite_reading, theater_ratio, 23, 0.62).
narrative_ontology:measurement(mone_tr_t27, monetary_anchor_principle__overdetermined_composite_reading, theater_ratio, 27, 0.58).

% Extraction over time
narrative_ontology:measurement(mone_be_t0, monetary_anchor_principle__overdetermined_composite_reading, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(mone_be_t5, monetary_anchor_principle__overdetermined_composite_reading, base_extractiveness, 5, 0.3).
narrative_ontology:measurement(mone_be_t9, monetary_anchor_principle__overdetermined_composite_reading, base_extractiveness, 9, 0.4).
narrative_ontology:measurement(mone_be_t14, monetary_anchor_principle__overdetermined_composite_reading, base_extractiveness, 14, 0.55).
narrative_ontology:measurement(mone_be_t18, monetary_anchor_principle__overdetermined_composite_reading, base_extractiveness, 18, 0.68).
narrative_ontology:measurement(mone_be_t23, monetary_anchor_principle__overdetermined_composite_reading, base_extractiveness, 23, 0.78).
narrative_ontology:measurement(mone_be_t27, monetary_anchor_principle__overdetermined_composite_reading, base_extractiveness, 27, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(mone_su_t0, monetary_anchor_principle__overdetermined_composite_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(mone_su_t5, monetary_anchor_principle__overdetermined_composite_reading, suppression_requirement, 5, 0.38).
narrative_ontology:measurement(mone_su_t9, monetary_anchor_principle__overdetermined_composite_reading, suppression_requirement, 9, 0.45).
narrative_ontology:measurement(mone_su_t14, monetary_anchor_principle__overdetermined_composite_reading, suppression_requirement, 14, 0.58).
narrative_ontology:measurement(mone_su_t18, monetary_anchor_principle__overdetermined_composite_reading, suppression_requirement, 18, 0.7).
narrative_ontology:measurement(mone_su_t23, monetary_anchor_principle__overdetermined_composite_reading, suppression_requirement, 23, 0.78).
narrative_ontology:measurement(mone_su_t27, monetary_anchor_principle__overdetermined_composite_reading, suppression_requirement, 27, 0.4).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(monetary_anchor_principle__overdetermined_composite_reading, global_infrastructure).
narrative_ontology:affects_constraint(monetary_anchor_principle__overdetermined_composite_reading, punctuated_swap_reading).
narrative_ontology:affects_constraint(monetary_anchor_principle__overdetermined_composite_reading, triffin_inevitability_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the monetary_anchor_principle kernel, decomposed from colloquial 'Bretton Woods collapse' narratives into structurally distinct claims. The overdetermined composite reading treats the collapse as inevitable structural convergence; sibling readings treat it as discrete choice or single-cause dilemma.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
