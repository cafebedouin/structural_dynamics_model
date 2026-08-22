% ============================================================================
% CONSTRAINT STORY: gold_fiat_transition_mechanism__automatic_constraint_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_gold_fiat_transition_mechanism__automatic_constraint_reading, []).

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
    constraint_indexing:constraint_classification/3,
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
 *   constraint_id: gold_fiat_transition_mechanism__automatic_constraint_reading
 *   human_readable: Automatic-to-Discretionary Monetary Constraint Transition (Automatic Constraint Reading)
 *   domain: monetary_economics/political_economy
 *
 * SUMMARY:
 *   The transition from commodity-backed to discretionary fiat monetary
 *   authority, viewed through the automatic_constraint_reading: the
 *   elimination of gold-reserve limits on money creation replaced an
 *   automatic physical constraint with institutional discretionary power.
 *   Monetary authorities gained discretion; creditor classes lost automatic
 *   debasement protection. The constraint weakened in absolute termsâno
 *   longer mechanically bound by physical reservesâbut remained
 *   institutional, extractive, and actively enforced.
 *
 * KEY AGENTS:
 *   - monetary_authorities: Primary agenda-setter and beneficiary (institutional/global/analytical) â administers discretionary fiat framework, gained policy autonomy from gold constraint elimination
 *   - fiscal_authorities: Secondary beneficiary (institutional/national/constrained) â captures seigniorage and expanded debt capacity
 *   - creditor_class: Primary target and payer (powerful/global/constrained) â lost automatic gold-redemption protection, bears discretionary inflation risk
 *   - fixed_income_households: Secondary payer (moderate/national/constrained) â bears inflation tax without automatic floor
 *   - hard_money_advocates: Excluded voice (moderate/global/constrained) â argues for automatic constraint restoration, structurally absent from policy councils
 *   - academic_monetary_economists: Analytical observer (institutional/global/analytical) â provides interpretive justification for discretionary framework
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gold_fiat_transition_mechanism__automatic_constraint_reading, 0.82).
domain_priors:suppression_score(gold_fiat_transition_mechanism__automatic_constraint_reading, 0.76).
domain_priors:theater_ratio(gold_fiat_transition_mechanism__automatic_constraint_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gold_fiat_transition_mechanism__automatic_constraint_reading, extractiveness, 0.82).
narrative_ontology:constraint_metric(gold_fiat_transition_mechanism__automatic_constraint_reading, suppression_requirement, 0.76).
narrative_ontology:constraint_metric(gold_fiat_transition_mechanism__automatic_constraint_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gold_fiat_transition_mechanism__automatic_constraint_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(gold_fiat_transition_mechanism__automatic_constraint_reading, resistance, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gold_fiat_transition_mechanism__automatic_constraint_reading, tangled_rope).
narrative_ontology:human_readable(gold_fiat_transition_mechanism__automatic_constraint_reading, "Automatic-to-Discretionary Monetary Constraint Transition (Automatic Constraint Reading)").
narrative_ontology:topic_domain(gold_fiat_transition_mechanism__automatic_constraint_reading, "monetary_economics/political_economy").

domain_priors:requires_active_enforcement(gold_fiat_transition_mechanism__automatic_constraint_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gold_fiat_transition_mechanism__automatic_constraint_reading, 'fbcad55a-bb82-448a-bd97-e23d6cfd5521').
narrative_ontology:cs_kernel_codification('fbcad55a-bb82-448a-bd97-e23d6cfd5521', formalized).
narrative_ontology:cs_authority_grounding('fbcad55a-bb82-448a-bd97-e23d6cfd5521', extraction).
narrative_ontology:cs_interpretation_layer_present('fbcad55a-bb82-448a-bd97-e23d6cfd5521').
narrative_ontology:cs_reading_relation('fbcad55a-bb82-448a-bd97-e23d6cfd5521', gold_fiat_transition_mechanism__creditor_discipline_reading, coexists_with).
narrative_ontology:cs_reading_relation('fbcad55a-bb82-448a-bd97-e23d6cfd5521', gold_fiat_transition_mechanism__composite_overdetermination_reading, influences).
narrative_ontology:cs_axiom('fbcad55a-bb82-448a-bd97-e23d6cfd5521', foundational, automatic_money_constraint_normatively_required).
narrative_ontology:cs_axiom_status(automatic_money_constraint_normatively_required, holdable).
narrative_ontology:cs_axiom_grounding('fbcad55a-bb82-448a-bd97-e23d6cfd5521', automatic_money_constraint_normatively_required, deontological).
narrative_ontology:cs_axiom('fbcad55a-bb82-448a-bd97-e23d6cfd5521', foundational, discretionary_authority_extractive_toward_creditors).
narrative_ontology:cs_axiom_status(discretionary_authority_extractive_toward_creditors, holdable).
narrative_ontology:cs_axiom_grounding('fbcad55a-bb82-448a-bd97-e23d6cfd5521', discretionary_authority_extractive_toward_creditors, empirically_contingent).
narrative_ontology:cs_reference_frame('fbcad55a-bb82-448a-bd97-e23d6cfd5521', automatic_gold_convertibility_regime).
narrative_ontology:cs_drift_state('fbcad55a-bb82-448a-bd97-e23d6cfd5521', contemporary_fiat_era, gap(codification_collapse, severe, true)).
narrative_ontology:cs_created_at('fbcad55a-bb82-448a-bd97-e23d6cfd5521', '').
narrative_ontology:cs_kernel_id(gold_fiat_transition_mechanism__automatic_constraint_reading, gold_fiat_transition_mechanism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gold_fiat_transition_mechanism__automatic_constraint_reading, monetary_authorities).
narrative_ontology:constraint_beneficiary(gold_fiat_transition_mechanism__automatic_constraint_reading, fiscal_authorities).
narrative_ontology:constraint_victim(gold_fiat_transition_mechanism__automatic_constraint_reading, creditor_class).
narrative_ontology:constraint_victim(gold_fiat_transition_mechanism__automatic_constraint_reading, fixed_income_households).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administer the discretionary fiat monetary framework, setting interest rates and money supply without mechanical gold-reserve constraint. Gained substantial policy discretion and institutional autonomy from the elimination of automatic convertibility. Cannot exit the system because they constitute its authoritative center.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__automatic_constraint_reading, monetary_authorities, agenda_setter,
    institutional, generational, analytical, global).
narrative_ontology:stakeholder_secondary_role(gold_fiat_transition_mechanism__automatic_constraint_reading, monetary_authorities, beneficiary).

% Benefit from expanded sovereign debt capacity and seigniorage revenue made possible by discretionary money creation unconstrained by physical reserves. Bound by the same fiat framework but positioned to capture its fiscal upside through deficit monetization.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__automatic_constraint_reading, fiscal_authorities, beneficiary,
    institutional, generational, constrained, national).

% Holds sovereign and private debt instruments denominated in fiat currency. Lost the automatic nominal-protection mechanism that gold convertibility provided; now exposed to discretionary inflation and debasement. Can hedge into real assets but cannot escape legal-tender denomination of existing contractual claims.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__automatic_constraint_reading, creditor_class, payer,
    powerful, biographical, constrained, global).

% Hold savings, pensions, and insurance products in fiat-denominated form. Bear inflation risk and negative real rates without the automatic floor that commodity backing once provided. Lack access to institutional-grade inflation-hedge instruments.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__automatic_constraint_reading, fixed_income_households, payer,
    moderate, biographical, constrained, national).

% Argue for restoration of commodity-backed automatic monetary constraints. Structurally excluded from central bank governance, academic macroeconomics hiring, and monetary policy councils despite representing creditor-protection interests.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__automatic_constraint_reading, hard_money_advocates, excluded,
    moderate, civilizational, constrained, global).

% Produce the macroeconomic models and policy frameworks that justify and operationalize discretionary central banking. Provide the interpretive layer that absorbs drift from the abandoned gold constraint without advocating for its restoration.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__automatic_constraint_reading, academic_monetary_economists, observer,
    institutional, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides discretionary capacity for monetary policy response to liquidity crises, business cycles, and sovereign financing needs without mechanical binding to physical gold reserve ratios.
% TRANSFER_FUNCTION: Moves purchasing power risk from automatic gold-redeemability to discretionary institutional management; transfers seigniorage and inflation-tax capacity to monetary and fiscal authorities, while transferring debasement exposure to creditors and fixed-income holders.
% ABSENT_VOICES: Hard-money advocates and commodity-money economists who would argue for restoration of automatic reserve constraints are structurally excluded from central bank policy committees and mainstream macroeconomic discourse.
% DISAPPEARANCE_RATIONALE: If the discretionary authority vanished without replacement, the modern monetary order would collapse; if the automatic gold constraint reappeared, money creation would be mechanically bounded by physical reserves, sovereign debt capacity would contract, and creditor claims would regain automatic nominal protection.
% FOUNDING_PROBLEM: The gold-exchange standard imposed procyclical liquidity constraints that prevented central banks from expanding money supply during crises, deepening depressions and constraining war finance.
% FOUNDING_PROBLEM_CORROBORATION: Monetary authorities and macroeconomic policymakers attest the founding problem remains live. Hard-money economists and creditor-class representatives outside the benefiting parties argue the founding problem was soluble through narrower adjustments rather than elimination of the automatic constraint; historical banking-crisis scholarship offers mixed corroboration.
narrative_ontology:disappearance_verdict(gold_fiat_transition_mechanism__automatic_constraint_reading, world_rearranges).
narrative_ontology:founding_problem_status(gold_fiat_transition_mechanism__automatic_constraint_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gold_fiat_transition_mechanism__automatic_constraint_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(gold_fiat_transition_mechanism__automatic_constraint_reading, 'none', 1).
narrative_ontology:epsilon_provenance(gold_fiat_transition_mechanism__automatic_constraint_reading, 0.82, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gold_fiat_transition_mechanism__automatic_constraint_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(gold_fiat_transition_mechanism__automatic_constraint_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(gold_fiat_transition_mechanism__automatic_constraint_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.82) is high because the elimination of automatic gold convertibility transferred purchasing-power risk to creditors and fixed-income holders without their consent, while concentrating seigniorage and discretion in monetary and fiscal authorities. Suppression (0.76) is high because the arrangement depends on legal tender laws, prohibitions on private currency, and capital controls that prevent exit to commodity money. Theater ratio (0.40) reflects the growing performative component of central bankingâforward guidance, press conferences, complex forecasting modelsâthat supplements core monetary function. Accessibility collapse (0.65) captures the legal and frictional barriers to exiting fiat into gold or alternatives despite theoretical availability. Resistance (0.48) reflects persistent but politically marginalized opposition from hard-money advocates and creditor-class interests.
 *
 * PERSPECTIVAL GAP:
 *   The monetary_authorities seat experiences the constraint as legitimate coordination machinery it operates; the creditor_class seat experiences the same structure as expropriation of automatic contractual protection. The engine computes this divergence from identical structural facts through directionality: the same constraint yields negative effective extraction (subsidy) for the agenda-setter and strongly positive extraction for the constrained payer.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (monetary_authorities, fiscal_authorities) sit near the low-d beneficiary pole because the constraint subsidizes their discretion, revenue, and debt capacity. Victims (creditor_class, fixed_income_households) sit near the high-d target pole because the constraint extracts purchasing power from their nominal claims. Exit differentiation drives the spread: monetary authorities have analytical exit (they are the system), while creditors and households are constrained by legal tender and contract denomination.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problemâprocyclical liquidity contractions under goldâwas genuinely severe, which prevents classification as a pure snare. However, the solution (discretionary fiat authority) outlived the acute crisis that produced it and now operates as a standing extraction mechanism. The coordination function (crisis liquidity, lender of last resort) is real but asymmetrically distributed: the benefiting parties (authorities, sovereigns) control the machinery, while the paying parties (creditors, savers) have no comparable veto. Tangled rope captures this hybrid structure better than rope (which would ignore the creditor extraction) or snare (which would deny the genuine coordination function).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    automatic_vs_composite_causality,
    'Was the transition from gold to fiat a single causal swap of automatic constraint for discretionary authority, or a convergence of multiple independent technological and political changes?',
    'Deep historiography disaggregating the causal contribution of Nixon Shock from telecommunications-enabled capital mobility, labor bargaining shifts, and legal tender maturation.',
    'If composite, the automatic constraint reading overstates the intentionality and unity of the transition; if single-swap, the composite reading understates the centrality of deliberate constraint elimination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(automatic_vs_composite_causality, conceptual, 'Whether the transition was a unified constraint replacement or overdetermined convergence').

omega_variable(
    discretionary_coordination_or_extraction,
    'Does discretionary central banking provide a genuine coordination function that offsets its asymmetric extraction from creditors, or is the coordination story a legitimizing cover for systematic wealth transfer?',
    'Cross-regime comparison of crisis-frequency and creditor-wealth trajectories under automatic versus discretionary monetary constraints, controlling for non-monetary variables.',
    'If coordination is genuine and offsetting, classification shifts toward rope or tangled_rope with lower net extraction; if cover, classification shifts toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(discretionary_coordination_or_extraction, empirical, 'Whether discretionary monetary authority''s coordination function is genuine or cover').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gold_fiat_transition_mechanism__automatic_constraint_reading, 0, 53).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gold_fiat_auto_tr_t0, gold_fiat_transition_mechanism__automatic_constraint_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(gold_fiat_auto_tr_t10, gold_fiat_transition_mechanism__automatic_constraint_reading, theater_ratio, 10, 0.25).
narrative_ontology:measurement(gold_fiat_auto_tr_t20, gold_fiat_transition_mechanism__automatic_constraint_reading, theater_ratio, 20, 0.3).
narrative_ontology:measurement(gold_fiat_auto_tr_t30, gold_fiat_transition_mechanism__automatic_constraint_reading, theater_ratio, 30, 0.33).
narrative_ontology:measurement(gold_fiat_auto_tr_t40, gold_fiat_transition_mechanism__automatic_constraint_reading, theater_ratio, 40, 0.37).
narrative_ontology:measurement(gold_fiat_auto_tr_t53, gold_fiat_transition_mechanism__automatic_constraint_reading, theater_ratio, 53, 0.4).

% Extraction over time
narrative_ontology:measurement(gold_fiat_auto_be_t0, gold_fiat_transition_mechanism__automatic_constraint_reading, base_extractiveness, 0, 0.6).
narrative_ontology:measurement(gold_fiat_auto_be_t10, gold_fiat_transition_mechanism__automatic_constraint_reading, base_extractiveness, 10, 0.7).
narrative_ontology:measurement(gold_fiat_auto_be_t20, gold_fiat_transition_mechanism__automatic_constraint_reading, base_extractiveness, 20, 0.68).
narrative_ontology:measurement(gold_fiat_auto_be_t30, gold_fiat_transition_mechanism__automatic_constraint_reading, base_extractiveness, 30, 0.74).
narrative_ontology:measurement(gold_fiat_auto_be_t40, gold_fiat_transition_mechanism__automatic_constraint_reading, base_extractiveness, 40, 0.79).
narrative_ontology:measurement(gold_fiat_auto_be_t53, gold_fiat_transition_mechanism__automatic_constraint_reading, base_extractiveness, 53, 0.82).

% Suppression requirement over time
narrative_ontology:measurement(gold_fiat_auto_su_t0, gold_fiat_transition_mechanism__automatic_constraint_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(gold_fiat_auto_su_t10, gold_fiat_transition_mechanism__automatic_constraint_reading, suppression_requirement, 10, 0.58).
narrative_ontology:measurement(gold_fiat_auto_su_t20, gold_fiat_transition_mechanism__automatic_constraint_reading, suppression_requirement, 20, 0.64).
narrative_ontology:measurement(gold_fiat_auto_su_t30, gold_fiat_transition_mechanism__automatic_constraint_reading, suppression_requirement, 30, 0.7).
narrative_ontology:measurement(gold_fiat_auto_su_t40, gold_fiat_transition_mechanism__automatic_constraint_reading, suppression_requirement, 40, 0.74).
narrative_ontology:measurement(gold_fiat_auto_su_t53, gold_fiat_transition_mechanism__automatic_constraint_reading, suppression_requirement, 53, 0.76).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(gold_fiat_transition_mechanism__automatic_constraint_reading, gold_fiat_transition_mechanism__creditor_discipline_reading).
narrative_ontology:affects_constraint(gold_fiat_transition_mechanism__automatic_constraint_reading, gold_fiat_transition_mechanism__composite_overdetermination_reading).

% DUAL FORMULATION NOTE:
% The gold_fiat_transition_mechanism kernel decomposes into three structurally distinct readings: automatic_constraint_reading (single swap of automatic for discretionary), creditor_discipline_reading (geopolitical shift from creditor to debtor power), and composite_overdetermination_reading (overdetermined convergence of multiple structural changes). Each reading carries a different epsilon and stakeholder geometry; they are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
