% ============================================================================
% CONSTRAINT STORY: structural_adjustment_conditionalities__creditor_coordination_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_structural_adjustment_conditionalities__creditor_coordination_reading, []).

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
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: structural_adjustment_conditionalities__creditor_coordination_reading
 *   human_readable: Structural Adjustment Conditionalities â Creditor Coordination Reading
 *   domain: international_political_economy/development_finance
 *
 * SUMMARY:
 *   This constraint story instantiates the creditor_coordination_reading of
 *   the contested kernel 'structural_adjustment_conditionalities'. Under this
 *   reading, loan conditionalities imposed by multilateral creditors are
 *   necessary coordination mechanisms that solve a sovereign
 *   time-inconsistency problem. The reading treats inefficient state sectors
 *   as the residual losers of a welfare-improving reform, future taxpayers as
 *   diffuse beneficiaries of fiscal sustainability, and international capital
 *   as the coordinated party that requires credible commitment devices. The
 *   claim is rope; the authored metrics are descriptively independent and
 *   capture low-moderate extraction, low suppression, and modest theater. The
 *   kernel is contested by a debtor_extraction_reading (pure extraction
 *   framing) and a hybrid_selectivity_reading (selective enforcement
 *   framing).
 *
 * KEY AGENTS:
 *   - multilateral_creditor_institutions: Agenda setter (institutional/arbitrage/global) â designs and enforces conditionality frameworks
 *   - future_taxpayers_debtor_states: Diffuse beneficiary (powerless/trapped/national) â may gain from fiscal sustainability but have no seat at the table
 *   - sovereign_debt_investors: Concentrated beneficiary (powerful/arbitrage/global) â receives risk reduction through pooled monitoring
 *   - inefficient_state_enterprises: Structural payer (organized/constrained/national) â bears the cost of subsidy removal and privatization
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(structural_adjustment_conditionalities__creditor_coordination_reading, 0.32).
domain_priors:suppression_score(structural_adjustment_conditionalities__creditor_coordination_reading, 0.28).
domain_priors:theater_ratio(structural_adjustment_conditionalities__creditor_coordination_reading, 0.18).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(structural_adjustment_conditionalities__creditor_coordination_reading, extractiveness, 0.32).
narrative_ontology:constraint_metric(structural_adjustment_conditionalities__creditor_coordination_reading, suppression_requirement, 0.28).
narrative_ontology:constraint_metric(structural_adjustment_conditionalities__creditor_coordination_reading, theater_ratio, 0.18).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(structural_adjustment_conditionalities__creditor_coordination_reading, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(structural_adjustment_conditionalities__creditor_coordination_reading, resistance, 0.38).

% --- Constraint claim ---
narrative_ontology:constraint_claim(structural_adjustment_conditionalities__creditor_coordination_reading, rope).
narrative_ontology:human_readable(structural_adjustment_conditionalities__creditor_coordination_reading, "Structural Adjustment Conditionalities â Creditor Coordination Reading").
narrative_ontology:topic_domain(structural_adjustment_conditionalities__creditor_coordination_reading, "international_political_economy/development_finance").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(structural_adjustment_conditionalities__creditor_coordination_reading, '8e16104d-9cba-48ad-b003-60891e901a5f').
narrative_ontology:cs_kernel_codification('8e16104d-9cba-48ad-b003-60891e901a5f', formalized).
narrative_ontology:cs_authority_grounding('8e16104d-9cba-48ad-b003-60891e901a5f', expertise).
narrative_ontology:cs_interpretation_layer_present('8e16104d-9cba-48ad-b003-60891e901a5f').
narrative_ontology:cs_reading_relation('8e16104d-9cba-48ad-b003-60891e901a5f', structural_adjustment_conditionalities__debtor_extraction_reading, coexists_with).
narrative_ontology:cs_reading_relation('8e16104d-9cba-48ad-b003-60891e901a5f', structural_adjustment_conditionalities__hybrid_selectivity_reading, influences).
narrative_ontology:cs_axiom('8e16104d-9cba-48ad-b003-60891e901a5f', foundational, external_anchor_necessary_for_sovereign_commitment).
narrative_ontology:cs_axiom_status(external_anchor_necessary_for_sovereign_commitment, holdable).
narrative_ontology:cs_axiom_grounding('8e16104d-9cba-48ad-b003-60891e901a5f', external_anchor_necessary_for_sovereign_commitment, empirically_contingent).
narrative_ontology:cs_axiom('8e16104d-9cba-48ad-b003-60891e901a5f', foundational, conditionality_generates_net_welfare_gain).
narrative_ontology:cs_axiom_status(conditionality_generates_net_welfare_gain, holdable).
narrative_ontology:cs_axiom_grounding('8e16104d-9cba-48ad-b003-60891e901a5f', conditionality_generates_net_welfare_gain, instrumental).
narrative_ontology:cs_reference_frame('8e16104d-9cba-48ad-b003-60891e901a5f', credible_commitment_regime).
narrative_ontology:cs_drift_state('8e16104d-9cba-48ad-b003-60891e901a5f', post_washington_consensus_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('8e16104d-9cba-48ad-b003-60891e901a5f', '').
narrative_ontology:cs_kernel_id(structural_adjustment_conditionalities__creditor_coordination_reading, structural_adjustment_conditionalities).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(structural_adjustment_conditionalities__creditor_coordination_reading, future_taxpayers_debtor_states).
narrative_ontology:constraint_beneficiary(structural_adjustment_conditionalities__creditor_coordination_reading, sovereign_debt_investors).
narrative_ontology:constraint_victim(structural_adjustment_conditionalities__creditor_coordination_reading, inefficient_state_enterprises).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Design and enforce conditionality frameworks attached to balance-of-payments support and development lending. They set fiscal and structural benchmarks, monitor compliance, and disburse tranches conditionally. They justify the arrangement as preventing moral hazard and ensuring repayment capacity.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__creditor_coordination_reading, multilateral_creditor_institutions, agenda_setter,
    institutional, generational, arbitrage, global).

% Bear the long-term fiscal burden of sovereign debt if the state defaults or runs unsustainable deficits. From their position, conditionalities are a distant mechanism that may reduce future tax extraction or inflation by enforcing fiscal discipline today, though they have no voice in negotiating the terms.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__creditor_coordination_reading, future_taxpayers_debtor_states, beneficiary,
    powerless, generational, trapped, national).

% Hold sovereign debt instruments and require credible signals of repayment capacity. Conditionality provides a coordination mechanism that pools monitoring and enforcement, reducing due diligence costs and default risk across emerging-market portfolios.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__creditor_coordination_reading, sovereign_debt_investors, beneficiary,
    powerful, biographical, arbitrage, global).

% Receive subsidies, protective tariffs, and preferential credit under pre-adjustment regimes. Conditionality mandates privatization, subsidy removal, or liquidation, directly eliminating their protected position and transferring resources to market-priced competitors.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__creditor_coordination_reading, inefficient_state_enterprises, payer,
    organized, immediate, constrained, national).

% Evaluate the empirical record of structural adjustment programs, debating whether conditionality improves fiscal outcomes and growth or generates unnecessary contraction. Their research informs but does not set the policy framework.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__creditor_coordination_reading, development_economists, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves a time-inconsistency problem in sovereign debt: governments facing electoral or political pressures cannot credibly commit to fiscal discipline without an external enforcement mechanism. Conditionality pools monitoring capacity and aligns debtor policies with repayment capacity, preventing collective losses from default.
% TRANSFER_FUNCTION: Moves fiscal resources away from subsidized state enterprises and deficit-financed public consumption toward debt service and market-clearing prices; moves risk-adjusted returns to international creditors by lowering default probability across the portfolio.
% ABSENT_VOICES: Domestic populations in debtor states subject to austerity, especially users of public services and employees of state enterprises, are not seated at the conditionality negotiation table; their interests are represented only indirectly through debtor governments. Anti-debt social movements and heterodox economists arguing for default or alternative development models are structurally excluded from creditor institution deliberations.
% DISAPPEARANCE_RATIONALE: If conditionalities vanished overnight, debtor governments would face higher risk premiums, capital flows would reprice sharply as coordination and monitoring collapsed, and inefficient state sectors would likely be resuscitated by political pressure; the sovereign debt architecture would reorganize around alternative commitment devices or higher default rates.
% FOUNDING_PROBLEM: Sovereign debt markets suffer from moral hazard and time inconsistency: governments borrow excessively, subsidize inefficient sectors for political gain, and default or inflate away debt, imposing costs on creditors and future taxpayers.
% FOUNDING_PROBLEM_CORROBORATION: Creditor institutions and mainstream macroeconomists attest the problem remains live. Heterodox economists and debtor-side civil society organizations contest that the problem was ever primarily one of commitment rather than unequal exchange and global structural inequality; no outside party attests unanimously. Independent empirical literature is mixed, with some studies finding conditionality reduces default risk and others finding it imposes net costs without corresponding growth benefits.
narrative_ontology:disappearance_verdict(structural_adjustment_conditionalities__creditor_coordination_reading, world_rearranges).
narrative_ontology:founding_problem_status(structural_adjustment_conditionalities__creditor_coordination_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(structural_adjustment_conditionalities__creditor_coordination_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(structural_adjustment_conditionalities__creditor_coordination_reading, 'none', 1).
narrative_ontology:epsilon_provenance(structural_adjustment_conditionalities__creditor_coordination_reading, 0.32, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(structural_adjustment_conditionalities__creditor_coordination_reading_tests).
:- end_tests(structural_adjustment_conditionalities__creditor_coordination_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.32 because the constraint does transfer real resources away from protected state enterprises, but the magnitude is moderated by the genuine coordination function (solving commitment problems). Suppression is low (0.28) because enforcement relies on contractual tranche conditionality and voluntary program participation rather than raw coercion; alternatives to the program exist (non-concessional borrowing, default), though they are costly. Theater ratio is low (0.18) because monitoring and disbursement are primarily functional, with limited performative overlay. Accessibility collapse is moderate (0.42): once a program is entered, the path dependency of disbursement makes exit difficult, but alternative financing and policy space are not fully closed. Resistance is moderate (0.38): inefficient sectors and some debtor governments resist, but opposition is institutionalized within negotiation frameworks rather than systemic rupture.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat (multilateral creditors) experiences the constraint as a technical coordination device it maintains for global welfare; the payer seat (inefficient state enterprises) experiences the same structure as abrupt dismantlement of their protected position. The beneficiary seats split: sovereign debt investors experience a risk-management tool, while future taxpayers experience a distant, unaccountable mechanism that may or may not reduce their fiscal burden. The engine computes these divergences from the structural data; the authored rope claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries are declared as future_taxpayers_debtor_states and sovereign_debt_investors. Future taxpayers are diffuse, powerless, and trapped nationally, giving them low directionality (they are subsidized by the constraint's fiscal discipline). Sovereign debt investors are powerful, globally mobile, and arbitrage-capable, also giving them low directionality (the constraint subsidizes their risk exposure). The victim, inefficient_state_enterprises, is organized but constrained nationally and immediately threatened by subsidy removal, giving it high directionality as the target of the structural transfer. The multilateral creditors set the agenda but do not personally collect the transferred resources; their directionality is intermediate.
 *
 * MANDATROPHY ANALYSIS:
 *   The rope classification prevents mislabeling the constraint as pure extraction (snare) by requiring that coordination function, beneficiaries, and low suppression all be present. The presence of declared victims (inefficient_state_enterprises) does not automatically convert the constraint to tangled_rope or snare because the schema treats claim and metrics as independently authored facts. If the engine computes a more extractive type from the structural data, that divergence is the intended signal of the corpus. Conversely, if a pure extraction reading were claimed on this same structural data, the mandatrophy mismatch would flag a likely false extraction claim or failure to recognize coordination benefits.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'Is this constraint a genuine coordination mechanism for fiscal sustainability, or an extractive instrument whose coordination function is cover for surplus transfer to creditors?',
    'Comparative cross-debtor analysis measuring whether conditionalities correlate with improved fiscal outcomes net of contractionary effects, and whether gains accrue disproportionately to creditor nations versus debtor populations.',
    'If the coordination function is empirically unsupported or the gains accrue asymmetrically to creditors, the constraint reclassifies toward tangled_rope or snare; if supported, it remains rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'The fundamental ambiguity between coordination and extraction framings of structural adjustment conditionalities.').

omega_variable(
    empirical_efficacy_of_conditionality,
    'Do structural adjustment conditionalities generate net welfare improvements for debtor states, or do they impose contractionary costs that exceed coordination benefits?',
    'Meta-analysis of fiscal and growth outcomes in SAP-adopting versus non-adopting comparable economies, controlling for selection effects and external shocks.',
    'If conditionalities show no net welfare gain, the coordination claim loses empirical support and the rope classification weakens; if gains are demonstrated, the coordination reading is reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(empirical_efficacy_of_conditionality, empirical, 'Whether the creditor coordination reading''s empirical premises hold.').

omega_variable(
    practice_drift_vs_reference_frame,
    'Has the shift from structural adjustment conditionalities to PRSPs and flexible lending acknowledged the failure of the original coordination model, or merely repackaged it?',
    'Textual and policy analysis comparing enforcement mechanisms, policy content, and outcome conditionalities across SAP, PRSP, and contemporary lending instruments.',
    'If the practice drift represents abandonment of the core axiom, the reading''s reference frame is obsolete; if repackaging, the constraint persists under a new label.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(practice_drift_vs_reference_frame, empirical, 'Gap between the creditor coordination reference frame and actual institutional practice.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(structural_adjustment_conditionalities__creditor_coordination_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(stru_tr_t0, structural_adjustment_conditionalities__creditor_coordination_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(stru_tr_t8, structural_adjustment_conditionalities__creditor_coordination_reading, theater_ratio, 8, 0.12).
narrative_ontology:measurement(stru_tr_t16, structural_adjustment_conditionalities__creditor_coordination_reading, theater_ratio, 16, 0.14).
narrative_ontology:measurement(stru_tr_t24, structural_adjustment_conditionalities__creditor_coordination_reading, theater_ratio, 24, 0.16).
narrative_ontology:measurement(stru_tr_t32, structural_adjustment_conditionalities__creditor_coordination_reading, theater_ratio, 32, 0.18).
narrative_ontology:measurement(stru_tr_t40, structural_adjustment_conditionalities__creditor_coordination_reading, theater_ratio, 40, 0.2).

% Extraction over time
narrative_ontology:measurement(stru_be_t0, structural_adjustment_conditionalities__creditor_coordination_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(stru_be_t8, structural_adjustment_conditionalities__creditor_coordination_reading, base_extractiveness, 8, 0.31).
narrative_ontology:measurement(stru_be_t16, structural_adjustment_conditionalities__creditor_coordination_reading, base_extractiveness, 16, 0.33).
narrative_ontology:measurement(stru_be_t24, structural_adjustment_conditionalities__creditor_coordination_reading, base_extractiveness, 24, 0.32).
narrative_ontology:measurement(stru_be_t32, structural_adjustment_conditionalities__creditor_coordination_reading, base_extractiveness, 32, 0.3).
narrative_ontology:measurement(stru_be_t40, structural_adjustment_conditionalities__creditor_coordination_reading, base_extractiveness, 40, 0.28).

% Suppression requirement over time
narrative_ontology:measurement(stru_su_t0, structural_adjustment_conditionalities__creditor_coordination_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(stru_su_t8, structural_adjustment_conditionalities__creditor_coordination_reading, suppression_requirement, 8, 0.26).
narrative_ontology:measurement(stru_su_t16, structural_adjustment_conditionalities__creditor_coordination_reading, suppression_requirement, 16, 0.27).
narrative_ontology:measurement(stru_su_t24, structural_adjustment_conditionalities__creditor_coordination_reading, suppression_requirement, 24, 0.25).
narrative_ontology:measurement(stru_su_t32, structural_adjustment_conditionalities__creditor_coordination_reading, suppression_requirement, 32, 0.23).
narrative_ontology:measurement(stru_su_t40, structural_adjustment_conditionalities__creditor_coordination_reading, suppression_requirement, 40, 0.22).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(structural_adjustment_conditionalities__creditor_coordination_reading, resource_allocation).
narrative_ontology:affects_constraint(structural_adjustment_conditionalities__creditor_coordination_reading, debtor_extraction_reading).
narrative_ontology:affects_constraint(structural_adjustment_conditionalities__creditor_coordination_reading, hybrid_selectivity_reading).

% DUAL FORMULATION NOTE:
% The natural-language label 'structural adjustment conditionalities' decomposes into three kernel readings because the same institutional practice supports both a coordination story (this file), an extraction story (debtor_extraction_reading), and a selective-enforcement story (hybrid_selectivity_reading). Each reading carries a distinct Îµ and stakeholder geometry; they are linked as a constraint family sharing a regulatory domain and institutional coupling.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
