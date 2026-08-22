% ============================================================================
% CONSTRAINT STORY: gold_fiat_transition_mechanism__composite_overdetermination_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-20
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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: gold_fiat_transition_mechanism__composite_overdetermination_reading
 *   human_readable: Composite Overdetermination of Gold-to-Fiat Transition
 *   domain: monetary_economics/political_economy/history_of_economic_thought
 *
 * SUMMARY:
 *   The gold-to-fiat transition is conventionally narrated as a singular
 *   event (Nixon closing the gold window, 1971) or a singular structural
 *   shift (removal of automatic constraint, loss of creditor discipline).
 *   This reading argues the transition was a convergence of at least four
 *   independent structural changes: (1) telecommunications technology
 *   enabling instant cross-border capital flows (Telex, SWIFT, Eurodollar
 *   market infrastructure), (2) the mechanical collapse of Bretton Woods pegs
 *   due to U.S. balance-of-payments deficits and Triffin dilemma, (3) labor
 *   bargaining power shifts driven by globalization, automation, and
 *   political realignment, and (4) legal tender enforcement maturation making
 *   fiat operationally viable at scale. The Nixon Shock was a symbolic marker
 *   that crystallized the convergence, not its causal node. This reading
 *   challenges the kernel's singularity — there was no 'the transition' as a
 *   unified mechanism, only a distributed convergence that different actors
 *   experienced as different constraints.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gold_fiat_transition_mechanism__composite_overdetermination_reading, 0.45).
domain_priors:suppression_score(gold_fiat_transition_mechanism__composite_overdetermination_reading, 0.35).
domain_priors:theater_ratio(gold_fiat_transition_mechanism__composite_overdetermination_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gold_fiat_transition_mechanism__composite_overdetermination_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(gold_fiat_transition_mechanism__composite_overdetermination_reading, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(gold_fiat_transition_mechanism__composite_overdetermination_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gold_fiat_transition_mechanism__composite_overdetermination_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(gold_fiat_transition_mechanism__composite_overdetermination_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gold_fiat_transition_mechanism__composite_overdetermination_reading, tangled_rope).
narrative_ontology:human_readable(gold_fiat_transition_mechanism__composite_overdetermination_reading, "Composite Overdetermination of Gold-to-Fiat Transition").
narrative_ontology:topic_domain(gold_fiat_transition_mechanism__composite_overdetermination_reading, "monetary_economics/political_economy/history_of_economic_thought").

domain_priors:requires_active_enforcement(gold_fiat_transition_mechanism__composite_overdetermination_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gold_fiat_transition_mechanism__composite_overdetermination_reading, '7d49fd1e-3f21-470a-a7f7-e27bfeb52193').
narrative_ontology:cs_kernel_codification('7d49fd1e-3f21-470a-a7f7-e27bfeb52193', distributed).
narrative_ontology:cs_authority_grounding('7d49fd1e-3f21-470a-a7f7-e27bfeb52193', practice).
narrative_ontology:cs_interpretation_layer_present('7d49fd1e-3f21-470a-a7f7-e27bfeb52193').
narrative_ontology:cs_reading_relation('7d49fd1e-3f21-470a-a7f7-e27bfeb52193', gold_fiat_transition_mechanism__automatic_constraint_reading, coexists_with).
narrative_ontology:cs_reading_relation('7d49fd1e-3f21-470a-a7f7-e27bfeb52193', gold_fiat_transition_mechanism__creditor_discipline_reading, coexists_with).
narrative_ontology:cs_axiom('7d49fd1e-3f21-470a-a7f7-e27bfeb52193', foundational, transition_is_convergence_not_swap).
narrative_ontology:cs_axiom_status(transition_is_convergence_not_swap, holdable).
narrative_ontology:cs_axiom_grounding('7d49fd1e-3f21-470a-a7f7-e27bfeb52193', transition_is_convergence_not_swap, empirically_contingent).
narrative_ontology:cs_axiom('7d49fd1e-3f21-470a-a7f7-e27bfeb52193', foundational, nixon_shock_is_symbolic_marker).
narrative_ontology:cs_axiom_status(nixon_shock_is_symbolic_marker, holdable).
narrative_ontology:cs_axiom_grounding('7d49fd1e-3f21-470a-a7f7-e27bfeb52193', nixon_shock_is_symbolic_marker, empirically_contingent).
narrative_ontology:cs_axiom('7d49fd1e-3f21-470a-a7f7-e27bfeb52193', secondary, distributional_effects_are_component_specific).
narrative_ontology:cs_axiom_status(distributional_effects_are_component_specific, holdable).
narrative_ontology:cs_axiom_grounding('7d49fd1e-3f21-470a-a7f7-e27bfeb52193', distributional_effects_are_component_specific, empirically_contingent).
narrative_ontology:cs_reference_frame('7d49fd1e-3f21-470a-a7f7-e27bfeb52193', bretton_woods_compromise_stability).
narrative_ontology:cs_drift_state('7d49fd1e-3f21-470a-a7f7-e27bfeb52193', post_volcker_disinflation, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('7d49fd1e-3f21-470a-a7f7-e27bfeb52193', '2026-08-20T14:30:00Z').
narrative_ontology:cs_kernel_id(gold_fiat_transition_mechanism__composite_overdetermination_reading, gold_fiat_transition_mechanism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gold_fiat_transition_mechanism__composite_overdetermination_reading, international_banks).
narrative_ontology:constraint_beneficiary(gold_fiat_transition_mechanism__composite_overdetermination_reading, multinational_corporations).
narrative_ontology:constraint_beneficiary(gold_fiat_transition_mechanism__composite_overdetermination_reading, reserve_currency_issuer).
narrative_ontology:constraint_beneficiary(gold_fiat_transition_mechanism__composite_overdetermination_reading, financial_technologists).
narrative_ontology:constraint_victim(gold_fiat_transition_mechanism__composite_overdetermination_reading, labor_unions).
narrative_ontology:constraint_victim(gold_fiat_transition_mechanism__composite_overdetermination_reading, developing_nation_governments).
narrative_ontology:constraint_victim(gold_fiat_transition_mechanism__composite_overdetermination_reading, fixed_income_savers).
narrative_ontology:constraint_victim(gold_fiat_transition_mechanism__composite_overdetermination_reading, gold_standard_advocates).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Gained ability to move capital instantly across borders via new telecommunications infrastructure, enabling Eurodollar markets and offshore lending. The convergence of telecom and regulatory change created their profit window — they did not cause the transition but were positioned to exploit its distributed mechanics.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__composite_overdetermination_reading, international_banks, beneficiary,
    institutional, generational, arbitrage, global).

% Benefited from exchange rate flexibility that allowed global supply chain optimization and tax arbitrage. The collapse of fixed pegs was a structural windfall for firms with cross-border operations, distinct from the banking sector's gains.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__composite_overdetermination_reading, multinational_corporations, beneficiary,
    powerful, biographical, mobile, global).

% The U.S. Treasury and Federal Reserve administered the Bretton Woods system's dissolution and gained seigniorage advantages from reserve currency status. However, their agency was constrained by the convergence — they managed the symbolic moment (Nixon Shock) but did not engineer the underlying telecom, labor, and legal-tender shifts.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__composite_overdetermination_reading, reserve_currency_issuer, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(gold_fiat_transition_mechanism__composite_overdetermination_reading, reserve_currency_issuer, beneficiary).

% Builders of the telecom and settlement infrastructure (SWIFT, Telex, early electronic funds transfer) that made instant capital flows physically possible. Their innovations were a necessary condition for the transition but developed independently of monetary policy choices.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__composite_overdetermination_reading, financial_technologists, beneficiary,
    organized, biographical, mobile, global).

% Lost the gold standard's implicit wage anchor and faced capital mobility that undermined bargaining power. The shift to fiat enabled inflationary policies that eroded real wages, while capital flight threats disciplined wage demands. Their exit from this constraint was blocked by national legal frameworks.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__composite_overdetermination_reading, labor_unions, payer,
    organized, biographical, constrained, national).

% Faced volatile capital flows and loss of policy autonomy after pegs collapsed. The composite transition removed the gold standard's discipline on reserve currencies but imposed new disciplines via IMF conditionality and currency crises. They had no structural power to shape the convergence.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__composite_overdetermination_reading, developing_nation_governments, payer,
    moderate, generational, trapped, regional).

% Holders of bonds and savings denominated in currencies that lost gold backing. Inflation in the 1970s transferred wealth from creditors to debtors. Their exit options were limited to real assets (gold, property) which were inaccessible or illegal for many.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__composite_overdetermination_reading, fixed_income_savers, payer,
    powerless, biographical, constrained, national).

% Economists and policymakers who argued the transition was a mistake and gold provided necessary discipline. Their structural exclusion from the new monetary architecture was not by force but by the convergence rendering their framework operationally obsolete — the constraint they defended dissolved from beneath them.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__composite_overdetermination_reading, gold_standard_advocates, excluded,
    moderate, generational, identity_locked, global).

% Analyze the transition from outside the distributional conflict. Their work maps the convergence of telecom, geopolitical, labor, and legal changes without being subject to the constraint's extraction. They constitute the analytical seat for this reading.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__composite_overdetermination_reading, monetary_historians, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The convergence solved a genuine coordination problem: enabling global trade and capital allocation at speeds impossible under gold-standard settlement. Telecom + legal tender maturation created a payment system that could scale with industrial production.
% TRANSFER_FUNCTION: Different structural changes transferred wealth differently: telecom/telecom infrastructure transferred gains to banks and technologists; peg collapse transferred seigniorage to reserve issuer; labor power shifts transferred income from wages to capital; legal tender maturation transferred discipline from creditors to debtors. No single transfer function — the constraint is the convergence itself.
% ABSENT_VOICES: Developing nation populations (not just governments) who bore currency crisis costs without representation; future generations facing fiat system's long-term stability questions; gold-standard advocates excluded from policy debate after convergence made their framework inoperable.
% DISAPPEARANCE_RATIONALE: If the composite understanding disappeared, the dominant narrative would revert to a singular causal story (automatic constraint removal or creditor discipline loss), reshaping policy debates about monetary reform, central bank independence, and global financial architecture. The constraint's analytical frame structures how we diagnose current monetary problems.
% FOUNDING_PROBLEM: The post-WWII global monetary system needed to reconcile fixed exchange rates with national policy autonomy, growing trade volumes, and U.S. balance-of-payments deficits. The Bretton Woods compromise worked temporarily but generated structural contradictions across multiple dimensions simultaneously.
% FOUNDING_PROBLEM_CORROBORATION: Eichengreen (1996) 'Globalizing Capital' documents the multi-causal breakdown; Obstfeld & Taylor (2004) 'Global Capital Markets' show telecom and capital flow data independent of policy choices; Tooze (2018) 'Crashed' traces labor power shifts as autonomous driver. No beneficiary group claims the convergence was designed for them — all attest to emergent outcomes.
narrative_ontology:disappearance_verdict(gold_fiat_transition_mechanism__composite_overdetermination_reading, world_rearranges).
narrative_ontology:founding_problem_status(gold_fiat_transition_mechanism__composite_overdetermination_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gold_fiat_transition_mechanism__composite_overdetermination_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(gold_fiat_transition_mechanism__composite_overdetermination_reading, 'none', 1).
narrative_ontology:epsilon_provenance(gold_fiat_transition_mechanism__composite_overdetermination_reading, 0.45, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gold_fiat_transition_mechanism__composite_overdetermination_reading_tests).
:- end_tests(gold_fiat_transition_mechanism__composite_overdetermination_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Moderate extractiveness (0.45) reflects that the convergence created winners and losers without a single extractive mechanism — extraction is distributed across the component changes. Suppression (0.35) is moderate because the constraint's persistence didn't require active enforcement of a single rule; the convergence itself was structurally self-reinforcing once telecom and legal tender matured. Theater ratio (0.25) is low: the transition was not performative but a genuine structural reconfiguration. Accessibility collapse (0.40) is partial: alternatives (gold clauses, fixed pegs, commodity standards) remained legally possible but economically nonviable. Resistance (0.55) is significant: labor, developing nations, and gold advocates contested the outcomes, but the distributed nature of the convergence meant no single target for resistance.
 *
 * PERSPECTIVAL GAP:
 *   The analytical seat (monetary historians) sees the full convergence structure. Beneficiary seats see their specific gain mechanism as natural or earned. Victim seats experience their specific loss as imposed. The agenda setter (reserve issuer) experiences the constraint as something it administered but did not fully control. The engine computes per-seat classifications from these structural positions — the same convergence reads as rope (coordination gain) from the technologist seat, tangled_rope (mixed coordination/extraction) from the bank seat, and snare (pure extraction) from the labor seat.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (international banks, MNCs, reserve issuer, financial technologists) gained from different components of the convergence — their d values cluster near the beneficiary end but for different structural reasons. Victims (labor, developing nations, fixed-income savers, gold advocates) lost from different components — their d values cluster near the target end. The reserve currency issuer is dual-positioned (agenda_setter + beneficiary) because it managed the symbolic moment but was also constrained by the convergence. Gold standard advocates are identity_locked: their professional and ideological identity fused with the defeated framework, making exit from the debate structurally impossible even as the constraint they defended dissolved.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (reconciling fixed rates with autonomy and trade growth) is dead — the convergence resolved it by eliminating the fixed-rate requirement itself. But the analytical frame of 'the transition' persists as a mandatrophy: a singular narrative that obscures the distributed causality and prevents clear diagnosis of current monetary problems (e.g., whether crypto, CBDCs, or currency boards are 'reversing the transition' or addressing different structural conditions). The composite reading resolves mandatrophy by refusing the singular frame.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    convergence_vs_causality,
    'Is the convergence of telecom, peg collapse, labor shifts, and legal tender maturation a genuine structural overdetermination (multiple independent sufficient causes), or does one component causally dominate the others?',
    'Counterfactual historical analysis: if telecom had advanced but Bretton Woods held, would fiat have emerged? If Bretton Woods collapsed but telecom remained primitive? If labor power hadn''t shifted? Empirical work on Eurodollar market timing vs. Nixon Shock vs. Volcker shock sequencing.',
    'If genuine overdetermination, the kernel''s singularity is false and all three readings are partial. If one component dominates, that reading captures the primary mechanism and the others are secondary.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(convergence_vs_causality, conceptual, 'Whether the composite reading''s core premise (distributed convergence, no singular mechanism) holds against causal hierarchy analysis.').

omega_variable(
    kernel_singularity_false_unity,
    'Does the label ''gold-to-fiat transition'' refer to a single constraint with one ε, or a family of constraints with different ε values for different structural changes?',
    'Decompose the kernel into component constraints (telecom-enabled capital mobility, peg system collapse, labor discipline shift, legal tender maturation) and measure ε for each. If ε values differ substantially, the kernel is a false unity.',
    'If the kernel decomposes into multiple constraints with different ε, the automatic_constraint_reading and creditor_discipline_reading are not alternative readings of one constraint — they are readings of different constraints in a family. This reading''s claim (kernel is false unity) is validated.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_singularity_false_unity, empirical, 'Whether the kernel itself satisfies ε-invariance or is a colloquial label conflating structurally distinct constraints.').

omega_variable(
    nixon_shock_causal_weight,
    'What was the Nixon Shock''s actual causal weight in the convergence — symbolic marker, accelerant, or necessary condition?',
    'Compare pre-1971 trends (Eurodollar growth, gold pool failures, labor share peak) with post-1971 trajectories. If trends were already established and continued unchanged, the Shock was symbolic. If trajectories shifted discontinuously, it was accelerant or necessary condition.',
    'If symbolic, the automatic_constraint_reading''s focus on 1971 as the moment of constraint removal is misdated. If accelerant, the Shock matters but the convergence preceded it. This determines whether the kernel''s temporal framing (1971 as transition point) is analytically valid.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(nixon_shock_causal_weight, empirical, 'Causal status of the Nixon Shock within the convergence.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gold_fiat_transition_mechanism__composite_overdetermination_reading, 1960, 1985).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gftmc_tr_t1960, gold_fiat_transition_mechanism__composite_overdetermination_reading, theater_ratio, 1960, 0.1).
narrative_ontology:measurement(gftmc_tr_t1965, gold_fiat_transition_mechanism__composite_overdetermination_reading, theater_ratio, 1965, 0.12).
narrative_ontology:measurement(gftmc_tr_t1970, gold_fiat_transition_mechanism__composite_overdetermination_reading, theater_ratio, 1970, 0.18).
narrative_ontology:measurement(gftmc_tr_t1971, gold_fiat_transition_mechanism__composite_overdetermination_reading, theater_ratio, 1971, 0.22).
narrative_ontology:measurement(gftmc_tr_t1973, gold_fiat_transition_mechanism__composite_overdetermination_reading, theater_ratio, 1973, 0.25).
narrative_ontology:measurement(gftmc_tr_t1975, gold_fiat_transition_mechanism__composite_overdetermination_reading, theater_ratio, 1975, 0.25).
narrative_ontology:measurement(gftmc_tr_t1980, gold_fiat_transition_mechanism__composite_overdetermination_reading, theater_ratio, 1980, 0.24).
narrative_ontology:measurement(gftmc_tr_t1985, gold_fiat_transition_mechanism__composite_overdetermination_reading, theater_ratio, 1985, 0.25).

% Extraction over time
narrative_ontology:measurement(gftmc_be_t1960, gold_fiat_transition_mechanism__composite_overdetermination_reading, base_extractiveness, 1960, 0.15).
narrative_ontology:measurement(gftmc_be_t1965, gold_fiat_transition_mechanism__composite_overdetermination_reading, base_extractiveness, 1965, 0.2).
narrative_ontology:measurement(gftmc_be_t1970, gold_fiat_transition_mechanism__composite_overdetermination_reading, base_extractiveness, 1970, 0.3).
narrative_ontology:measurement(gftmc_be_t1971, gold_fiat_transition_mechanism__composite_overdetermination_reading, base_extractiveness, 1971, 0.35).
narrative_ontology:measurement(gftmc_be_t1973, gold_fiat_transition_mechanism__composite_overdetermination_reading, base_extractiveness, 1973, 0.42).
narrative_ontology:measurement(gftmc_be_t1975, gold_fiat_transition_mechanism__composite_overdetermination_reading, base_extractiveness, 1975, 0.45).
narrative_ontology:measurement(gftmc_be_t1980, gold_fiat_transition_mechanism__composite_overdetermination_reading, base_extractiveness, 1980, 0.48).
narrative_ontology:measurement(gftmc_be_t1985, gold_fiat_transition_mechanism__composite_overdetermination_reading, base_extractiveness, 1985, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(gftmc_su_t1960, gold_fiat_transition_mechanism__composite_overdetermination_reading, suppression_requirement, 1960, 0.2).
narrative_ontology:measurement(gftmc_su_t1965, gold_fiat_transition_mechanism__composite_overdetermination_reading, suppression_requirement, 1965, 0.25).
narrative_ontology:measurement(gftmc_su_t1970, gold_fiat_transition_mechanism__composite_overdetermination_reading, suppression_requirement, 1970, 0.3).
narrative_ontology:measurement(gftmc_su_t1971, gold_fiat_transition_mechanism__composite_overdetermination_reading, suppression_requirement, 1971, 0.35).
narrative_ontology:measurement(gftmc_su_t1973, gold_fiat_transition_mechanism__composite_overdetermination_reading, suppression_requirement, 1973, 0.38).
narrative_ontology:measurement(gftmc_su_t1975, gold_fiat_transition_mechanism__composite_overdetermination_reading, suppression_requirement, 1975, 0.35).
narrative_ontology:measurement(gftmc_su_t1980, gold_fiat_transition_mechanism__composite_overdetermination_reading, suppression_requirement, 1980, 0.35).
narrative_ontology:measurement(gftmc_su_t1985, gold_fiat_transition_mechanism__composite_overdetermination_reading, suppression_requirement, 1985, 0.35).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gold_fiat_transition_mechanism__composite_overdetermination_reading, global_infrastructure).
narrative_ontology:boltzmann_floor_override(gold_fiat_transition_mechanism__composite_overdetermination_reading, 0.18).
narrative_ontology:affects_constraint(gold_fiat_transition_mechanism__composite_overdetermination_reading, gold_fiat_transition_mechanism__automatic_constraint_reading).
narrative_ontology:affects_constraint(gold_fiat_transition_mechanism__composite_overdetermination_reading, gold_fiat_transition_mechanism__creditor_discipline_reading).
narrative_ontology:affects_constraint(gold_fiat_transition_mechanism__composite_overdetermination_reading, eurodollar_market_emergence).
narrative_ontology:affects_constraint(gold_fiat_transition_mechanism__composite_overdetermination_reading, bretton_woods_peg_collapse).
narrative_ontology:affects_constraint(gold_fiat_transition_mechanism__composite_overdetermination_reading, labor_share_decline_1970s).
narrative_ontology:affects_constraint(gold_fiat_transition_mechanism__composite_overdetermination_reading, legal_tender_enforcement_modernization).

% DUAL FORMULATION NOTE:
% This reading decomposes the gold_fiat_transition_mechanism kernel into a constraint family. The automatic_constraint_reading isolates the 'material-to-institutional constraint' strand (ε ≈ 0.25, Mountain→Rope). The creditor_discipline_reading isolates the 'creditor veto loss' strand (ε ≈ 0.55, Tangled Rope). This reading (composite_overdetermination) treats the kernel as a false unity — the convergence itself is the constraint (ε ≈ 0.45, Tangled Rope). All three stories link via affects_constraints. The ε values differ because the referents differ: each reading measures extraction against a different structural mechanism.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(gold_fiat_transition_mechanism__composite_overdetermination_reading, institutional, 0.3).
constraint_indexing:directionality_override(gold_fiat_transition_mechanism__composite_overdetermination_reading, organized, 0.65).
constraint_indexing:directionality_override(gold_fiat_transition_mechanism__composite_overdetermination_reading, powerless, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
