% ============================================================================
% CONSTRAINT STORY: gold_fiat_transition_mechanism__composite_overdetermination_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: gold_fiat_transition_mechanism__composite_overdetermination_reading
 *   human_readable: Gold-Fiat Transition Mechanism (Composite Overdetermination Reading)
 *   domain: monetary economics / political economy / history of economic thought
 *
 * SUMMARY:
 *   This constraint instantiates the composite_overdetermination_reading of
 *   the gold_fiat_transition_mechanism kernel. The transition from
 *   gold-pegged to fiat monetary governance is conventionally read as a
 *   single causal swapâeither the elimination of an automatic physical
 *   constraint or the removal of creditor discipline. This reading rejects
 *   kernel singularity: the transition was the convergence of multiple
 *   independent structural vectors (telecommunications enabling instant
 *   capital mobility, Bretton Woods peg collapse under speculative pressure,
 *   labor bargaining power realignments, legal tender enforcement maturation)
 *   with the Nixon Shock serving as symbolic ratification rather than causal
 *   node. Because no single vector dominated, there is no single beneficiary
 *   or unified extraction mechanism; distributional effects vary by
 *   structural channel.
 *
 * KEY AGENTS:
 *   - Central banks: Primary agenda-setter (institutional/constrained exit) â operate the fiat infrastructure and cannot exit it.
 *   - Reserve currency issuers: Primary beneficiary (powerful/constrained exit) â capture seigniorage and policy autonomy.
 *   - Global financial intermediaries: Secondary beneficiary (powerful/arbitrage exit) â profit from floating-rate architecture and instant capital flows.
 *   - Non-reserve currency economies: Primary payer (moderate/constrained exit) â bear exchange-rate volatility and imported inflation.
 *   - Fixed-income creditors: Secondary payer (moderate/constrained exit) â suffer inflation-tax erosion of nominal claims.
 *   - Gold-standard advocates: Excluded voice (moderate/constrained exit) â structurally barred from policy design.
 *   - Monetary historians: Analytical observer (analytical exit) â trace the multicausal convergence.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gold_fiat_transition_mechanism__composite_overdetermination_reading, 0.55).
domain_priors:suppression_score(gold_fiat_transition_mechanism__composite_overdetermination_reading, 0.62).
domain_priors:theater_ratio(gold_fiat_transition_mechanism__composite_overdetermination_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gold_fiat_transition_mechanism__composite_overdetermination_reading, extractiveness, 0.55).
narrative_ontology:constraint_metric(gold_fiat_transition_mechanism__composite_overdetermination_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(gold_fiat_transition_mechanism__composite_overdetermination_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gold_fiat_transition_mechanism__composite_overdetermination_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(gold_fiat_transition_mechanism__composite_overdetermination_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gold_fiat_transition_mechanism__composite_overdetermination_reading, tangled_rope).
narrative_ontology:human_readable(gold_fiat_transition_mechanism__composite_overdetermination_reading, "Gold-Fiat Transition Mechanism (Composite Overdetermination Reading)").
narrative_ontology:topic_domain(gold_fiat_transition_mechanism__composite_overdetermination_reading, "monetary economics / political economy / history of economic thought").

domain_priors:requires_active_enforcement(gold_fiat_transition_mechanism__composite_overdetermination_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gold_fiat_transition_mechanism__composite_overdetermination_reading, '156555bf-2ace-4060-94d0-f5f459978698').
narrative_ontology:cs_kernel_codification('156555bf-2ace-4060-94d0-f5f459978698', distributed).
narrative_ontology:cs_authority_grounding('156555bf-2ace-4060-94d0-f5f459978698', distributed).
narrative_ontology:cs_reading_relation('156555bf-2ace-4060-94d0-f5f459978698', gold_fiat_transition_mechanism__automatic_constraint_reading, influences).
narrative_ontology:cs_reading_relation('156555bf-2ace-4060-94d0-f5f459978698', gold_fiat_transition_mechanism__creditor_discipline_reading, influences).
narrative_ontology:cs_axiom('156555bf-2ace-4060-94d0-f5f459978698', foundational, transition_is_overdetermined_not_unified).
narrative_ontology:cs_axiom_status(transition_is_overdetermined_not_unified, holdable).
narrative_ontology:cs_axiom_grounding('156555bf-2ace-4060-94d0-f5f459978698', transition_is_overdetermined_not_unified, empirically_contingent).
narrative_ontology:cs_axiom('156555bf-2ace-4060-94d0-f5f459978698', foundational, monetary_architecture_emerges_from_structural_convergence).
narrative_ontology:cs_axiom_status(monetary_architecture_emerges_from_structural_convergence, holdable).
narrative_ontology:cs_axiom_grounding('156555bf-2ace-4060-94d0-f5f459978698', monetary_architecture_emerges_from_structural_convergence, empirically_contingent).
narrative_ontology:cs_reference_frame('156555bf-2ace-4060-94d0-f5f459978698', multicausal_structural_convergence).
narrative_ontology:cs_drift_state('156555bf-2ace-4060-94d0-f5f459978698', post_nixon_consolidation_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('156555bf-2ace-4060-94d0-f5f459978698', '').
narrative_ontology:cs_kernel_id(gold_fiat_transition_mechanism__composite_overdetermination_reading, gold_fiat_transition_mechanism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gold_fiat_transition_mechanism__composite_overdetermination_reading, central_banks).
narrative_ontology:constraint_beneficiary(gold_fiat_transition_mechanism__composite_overdetermination_reading, reserve_currency_issuers).
narrative_ontology:constraint_beneficiary(gold_fiat_transition_mechanism__composite_overdetermination_reading, global_financial_intermediaries).
narrative_ontology:constraint_victim(gold_fiat_transition_mechanism__composite_overdetermination_reading, non_reserve_currency_economies).
narrative_ontology:constraint_victim(gold_fiat_transition_mechanism__composite_overdetermination_reading, fixed_income_creditors).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administer fiat monetary operations, legal tender frameworks, lender-of-last-resort facilities, and exchange-rate regimes. Their institutional authority and policy toolkit depend on the composite fiat architecture; they cannot exit the constraint because they are its primary operators and symbolic anchors.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__composite_overdetermination_reading, central_banks, agenda_setter,
    institutional, generational, constrained, global).

% Issue the dominant reserve currencies used for global trade, reserves, and debt denomination. Capture seigniorage and macroeconomic policy autonomy from the fiat architecture, while remaining constrained by the same global capital-flow dynamics and balance-of-payments imperatives the system generates.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__composite_overdetermination_reading, reserve_currency_issuers, beneficiary,
    powerful, generational, constrained, global).

% Operate payment, clearing, derivatives, and capital-allocation infrastructure enabled by instant telecommunications and unpegged floating exchange rates. They benefit from transaction volumes, yield spreads, and volatility-management products that the post-gold architecture sustains.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__composite_overdetermination_reading, global_financial_intermediaries, beneficiary,
    powerful, biographical, arbitrage, global).

% Must hold reserves in foreign fiat, face exchange-rate volatility, imported inflation, and periodic balance-of-payments crises. They pay for the macroeconomic flexibility of reserve issuers through reduced monetary sovereignty and asymmetric adjustment burdens.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__composite_overdetermination_reading, non_reserve_currency_economies, payer,
    moderate, generational, constrained, national).

% Hold nominal claims denominated in fiat currencyâbonds, deposits, pensions. Their real returns are eroded by inflation and negative real rates that the fiat system enables for macro-stabilization or fiscal financing. Exit to inflation-resistant assets is available but costly and incomplete for contractual savers.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__composite_overdetermination_reading, fixed_income_creditors, payer,
    moderate, biographical, constrained, national).

% Advocate for commodity-backed monetary regimes and automatic monetary constraints. Their position is structurally excluded from mainstream policy architecture because legal tender enforcement, deposit insurance frameworks, and central bank statutes lock in fiat governance.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__composite_overdetermination_reading, gold_standard_advocates, excluded,
    moderate, civilizational, constrained, global).

% Analyze the transition as a composite convergence of independent structural vectorsâtelecommunications, Bretton Woods collapse, labor power shifts, legal tender maturationârather than a singular policy choice. They observe that Nixon Shock functioned as symbolic ratification, not causal origin.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__composite_overdetermination_reading, monetary_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(gold_fiat_transition_mechanism__composite_overdetermination_reading, diffuse).
narrative_ontology:fixing_cost_class(gold_fiat_transition_mechanism__composite_overdetermination_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates global macroeconomic stabilization, cross-border capital allocation, sovereign fiscal flexibility, and payment finality by replacing a rigid commodity anchor with a managed fiat architecture that can adjust to asymmetric shocks.
% TRANSFER_FUNCTION: Moves purchasing power and policy autonomy from nominal-claim holders and peripheral economies to sovereign debtors, reserve-currency issuers, and financial intermediaries through inflation, seigniorage, exchange-rate asymmetry, and macro-policy externalities.
% ABSENT_VOICES: Gold-standard advocates and commodity-money theorists are structurally excluded from treasury and central bank policy design; peripheral economy representatives are under-weighted in reserve-currency policy councils where global liquidity conditions are set.
% DISAPPEARANCE_RATIONALE: If the composite fiat architecture vanished overnight, global payment systems would freeze, sovereign debt servicing would lack a denominating unit, and macroeconomic policy tools would collapse. The world would rearrange around a new monetary anchor, fragmented currency blocs, or a spontaneous re-commoditization of money.
% FOUNDING_PROBLEM: The interwar and early post-war need for macroeconomic policy flexibility incompatible with rigid gold-peg discipline, combined with the technical impossibility of maintaining Bretton Woods parity under expanding trade, telecommunications-enabled capital mobility, and divergent national inflation preferences.
% FOUNDING_PROBLEM_CORROBORATION: Monetary historians outside the central banking mainstreamâe.g., scholarship in the Eichengreen and Helleiner lineagesâattest the multicausal convergence. Gold-standard advocates and some macroeconomic stability hawks dispute that flexibility was a genuine problem requiring this solution. Central banks assert a unified crisis-and-response narrative that composite historians contest.
narrative_ontology:disappearance_verdict(gold_fiat_transition_mechanism__composite_overdetermination_reading, world_rearranges).
narrative_ontology:founding_problem_status(gold_fiat_transition_mechanism__composite_overdetermination_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gold_fiat_transition_mechanism__composite_overdetermination_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(gold_fiat_transition_mechanism__composite_overdetermination_reading, 'none', 1).
narrative_ontology:epsilon_provenance(gold_fiat_transition_mechanism__composite_overdetermination_reading, 0.55, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gold_fiat_transition_mechanism__composite_overdetermination_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(gold_fiat_transition_mechanism__composite_overdetermination_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(gold_fiat_transition_mechanism__composite_overdetermination_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.55) because the fiat architecture generates real seigniorage and inflation-tax extraction, but the composite reading distributes these across multiple channels rather than concentrating them in a single seat. Suppression (0.62) reflects active legal tender enforcement, regulatory exclusion of competing commodity monies, and capital-control maintenance required to sustain national fiat jurisdictions. Theater ratio (0.28) captures the symbolic performance of central bank independence and inflation-targeting credibility, which partially masks fiscal dominance and distributional transfers. Accessibility collapse (0.65) because viable commodity-money alternatives are institutionally barred for ordinary economic actors, though black-market and offshore substitutes persist at the margin. Resistance (0.45) from gold-standard advocates, peripheral economies, and populist movements challenging monetaryorthodoxy. The measurement series share a single time grid so every metric is authored at every examined point.
 *
 * PERSPECTIVAL GAP:
 *   Reserve-currency issuers and global financial intermediaries experience the constraint as coordinating infrastructure that enables macro-policy flexibility and profitable intermediation; from these seats the arrangement looks like necessary rope. Non-reserve economies and fixed-income creditors experience the same architecture as an extractive asymmetry that exports adjustment costs and erodes nominal wealth; from these seats it reads as tangled rope trending toward snare. The engine computes this divergence from beneficiary/victim declarations, power differentials, and exit asymmetries.
 *
 * DIRECTIONALITY LOGIC:
 *   Central banks, reserve-currency issuers, and global financial intermediaries are declared beneficiaries, situating them toward the low-d beneficiary end of the spectrum. Central banks have constrained exit because they are institutionally fused to the machinery; financiers have arbitrage-grade exit within the system, which further dampens their effective extraction. Non-reserve economies and fixed-income creditors are declared victims with constrained exit, pushing them toward the high-d target end. Gold-standard advocates are excluded and carry no directional flow because they are outside the constraint's operation rather than governed by it.
 *
 * MANDATROPHY ANALYSIS:
 *   The composite reading prevents mislabeling the transition as a single designed extraction (snare) by documenting independent structural origins across technology, law, labor relations, and international regime collapse. If the founding problemsâBretton Woods rigidity, pre-digital settlement frictionâare dead but the arrangement persists, that persistence could signal piton-like inertia. However, the metrics resist this: moderate but rising extraction, active enforcement, and identifiable beneficiary capture keep the classification in tangled_rope rather than rope or piton. The absence of a single beneficiary does not eliminate extraction; it diffuses it across multiple structural channels.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_singularity_ambiguity,
    'Is the gold-to-fiat transition best modeled as a singular constraint with unified causality, or as a convergence of independent structural shifts with no single governing mechanism?',
    'Comparative historiography and counterfactual analysis assessing whether telecommunications, labor power shifts, and legal tender maturation could have produced the observed regime independently of Bretton Woods collapse or Nixon-era policy choices.',
    'If singular, the automatic_constraint and creditor_discipline readings gain validity as separate constraints; if irreducibly composite, the epsilon-invariance principle may require decomposing the kernel into multiple independent constraints rather than one overdetermined story.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_singularity_ambiguity, conceptual, 'Whether the transition mechanism is ontologically singular or plural.').

omega_variable(
    labor_bargaining_directionality,
    'Did the labor bargaining power shift under fiat regimes benefit workers through full-employment macro policy, or extract from them via inflation-tax erosion of nominal wages?',
    'Sectoral and temporal disaggregation of wage-share data, real wage trajectories, and inflation exposure across core and peripheral economies.',
    'Would flip directionality for organized labor from beneficiary to victim or vice versa, altering the computed seat classification for labor-aligned stakeholders.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(labor_bargaining_directionality, empirical, 'Directional ambiguity of labor''s structural position under fiat.').

omega_variable(
    seigniorage_concentration,
    'Does the extractive surplus of the fiat architecture concentrate in reserve-currency issuers, or diffuse across financial intermediaries and fiscal authorities in proportion to market power?',
    'Flow-of-funds analysis tracing seigniorage, inflation-tax incidence, and rent extraction from floating-exchange-rate volatility.',
    'Would determine whether gain_flow names a specific capturer or remains diffuse, with downstream effects on snare versus tangled_rope certification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(seigniorage_concentration, empirical, 'Concentration versus diffusion of extraction surplus.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gold_fiat_transition_mechanism__composite_overdetermination_reading, 0, 64).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gold_tr_t0, gold_fiat_transition_mechanism__composite_overdetermination_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(gold_tr_t10, gold_fiat_transition_mechanism__composite_overdetermination_reading, theater_ratio, 10, 0.12).
narrative_ontology:measurement(gold_tr_t20, gold_fiat_transition_mechanism__composite_overdetermination_reading, theater_ratio, 20, 0.15).
narrative_ontology:measurement(gold_tr_t30, gold_fiat_transition_mechanism__composite_overdetermination_reading, theater_ratio, 30, 0.18).
narrative_ontology:measurement(gold_tr_t40, gold_fiat_transition_mechanism__composite_overdetermination_reading, theater_ratio, 40, 0.22).
narrative_ontology:measurement(gold_tr_t50, gold_fiat_transition_mechanism__composite_overdetermination_reading, theater_ratio, 50, 0.25).
narrative_ontology:measurement(gold_tr_t60, gold_fiat_transition_mechanism__composite_overdetermination_reading, theater_ratio, 60, 0.28).

% Extraction over time
narrative_ontology:measurement(gold_be_t0, gold_fiat_transition_mechanism__composite_overdetermination_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(gold_be_t10, gold_fiat_transition_mechanism__composite_overdetermination_reading, base_extractiveness, 10, 0.35).
narrative_ontology:measurement(gold_be_t20, gold_fiat_transition_mechanism__composite_overdetermination_reading, base_extractiveness, 20, 0.4).
narrative_ontology:measurement(gold_be_t30, gold_fiat_transition_mechanism__composite_overdetermination_reading, base_extractiveness, 30, 0.45).
narrative_ontology:measurement(gold_be_t40, gold_fiat_transition_mechanism__composite_overdetermination_reading, base_extractiveness, 40, 0.5).
narrative_ontology:measurement(gold_be_t50, gold_fiat_transition_mechanism__composite_overdetermination_reading, base_extractiveness, 50, 0.53).
narrative_ontology:measurement(gold_be_t60, gold_fiat_transition_mechanism__composite_overdetermination_reading, base_extractiveness, 60, 0.55).

% Suppression requirement over time
narrative_ontology:measurement(gold_su_t0, gold_fiat_transition_mechanism__composite_overdetermination_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(gold_su_t10, gold_fiat_transition_mechanism__composite_overdetermination_reading, suppression_requirement, 10, 0.4).
narrative_ontology:measurement(gold_su_t20, gold_fiat_transition_mechanism__composite_overdetermination_reading, suppression_requirement, 20, 0.45).
narrative_ontology:measurement(gold_su_t30, gold_fiat_transition_mechanism__composite_overdetermination_reading, suppression_requirement, 30, 0.5).
narrative_ontology:measurement(gold_su_t40, gold_fiat_transition_mechanism__composite_overdetermination_reading, suppression_requirement, 40, 0.55).
narrative_ontology:measurement(gold_su_t50, gold_fiat_transition_mechanism__composite_overdetermination_reading, suppression_requirement, 50, 0.6).
narrative_ontology:measurement(gold_su_t60, gold_fiat_transition_mechanism__composite_overdetermination_reading, suppression_requirement, 60, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gold_fiat_transition_mechanism__composite_overdetermination_reading, global_infrastructure).

% DUAL FORMULATION NOTE:
% This reading is one of three in the gold_fiat_transition_mechanism kernel family. The sibling readingsâautomatic_constraint_reading and creditor_discipline_readingâinstantiate separate constraints with distinct epsilon values and causal attributions. Consult them for the full decomposition of the kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
