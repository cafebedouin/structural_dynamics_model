% ============================================================================
% CONSTRAINT STORY: gold_fiat_transition_mechanism__composite_overdetermination_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
 *   human_readable: Gold-Fiat Transition Mechanism (Composite Overdetermination Reading)
 *   domain: monetary_economics/political_economy/history_of_economic_thought
 *
 * SUMMARY:
 *   This constraint instantiates the composite_overdetermination_reading of
 *   the gold_fiat_transition_mechanism kernel. Rather than treating the 1971
 *   Nixon Shock as a causal node where gold discipline was swapped for fiat
 *   discretion, this reading holds that the transition was the convergence of
 *   multiple independent structural changes: telecommunications enabling
 *   instantaneous capital flows, the maturation of legal tender enforcement,
 *   secular shifts in labor bargaining power, and the inherent instabilities
 *   of the Bretton Woods pegged regime. The Nixon Shock was a symbolic marker
 *   that historiography misread as a unified transition. The colloquial label
 *   'the end of Bretton Woods' conflates structurally distinct mechanisms;
 *   this constraint disambiguates the composite arrangement from singular
 *   causal narratives.
 *
 * KEY AGENTS:
 *   - Reserve currency issuers (agenda_setter/beneficiary): Administer the global reserve system and capture seigniorage.
 *   - Financial intermediaries (beneficiary): Capture value from velocity and volatility of deregulated capital flows.
 *   - Debtor governments (beneficiary): Gain policy autonomy from eliminated gold-discipline.
 *   - Central banks (agenda_setter): Operate the fiat infrastructure through discretionary practice.
 *   - Creditor nations (payer): Lost redemption discipline and bear inflation risk.
 *   - Fixed income savers (payer): Bear the inflation tax with limited exit.
 *   - Peripheral economies (payer): Subject to volatile capital flows and dollar hegemony.
 *   - Monetary scholars (observer): Document the composite convergence evidence.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gold_fiat_transition_mechanism__composite_overdetermination_reading, 0.5).
domain_priors:suppression_score(gold_fiat_transition_mechanism__composite_overdetermination_reading, 0.58).
domain_priors:theater_ratio(gold_fiat_transition_mechanism__composite_overdetermination_reading, 0.32).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gold_fiat_transition_mechanism__composite_overdetermination_reading, extractiveness, 0.5).
narrative_ontology:constraint_metric(gold_fiat_transition_mechanism__composite_overdetermination_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(gold_fiat_transition_mechanism__composite_overdetermination_reading, theater_ratio, 0.32).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gold_fiat_transition_mechanism__composite_overdetermination_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(gold_fiat_transition_mechanism__composite_overdetermination_reading, resistance, 0.42).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gold_fiat_transition_mechanism__composite_overdetermination_reading, tangled_rope).
narrative_ontology:human_readable(gold_fiat_transition_mechanism__composite_overdetermination_reading, "Gold-Fiat Transition Mechanism (Composite Overdetermination Reading)").
narrative_ontology:topic_domain(gold_fiat_transition_mechanism__composite_overdetermination_reading, "monetary_economics/political_economy/history_of_economic_thought").

domain_priors:requires_active_enforcement(gold_fiat_transition_mechanism__composite_overdetermination_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gold_fiat_transition_mechanism__composite_overdetermination_reading, '18a2098f-adb5-42fc-a2cf-ac983f1d2a16').
narrative_ontology:cs_kernel_codification('18a2098f-adb5-42fc-a2cf-ac983f1d2a16', implicit).
narrative_ontology:cs_authority_grounding('18a2098f-adb5-42fc-a2cf-ac983f1d2a16', practice).
narrative_ontology:cs_interpretation_layer_present('18a2098f-adb5-42fc-a2cf-ac983f1d2a16').
narrative_ontology:cs_reading_relation('18a2098f-adb5-42fc-a2cf-ac983f1d2a16', gold_fiat_transition_mechanism__automatic_constraint_reading, influences).
narrative_ontology:cs_reading_relation('18a2098f-adb5-42fc-a2cf-ac983f1d2a16', gold_fiat_transition_mechanism__creditor_discipline_reading, influences).
narrative_ontology:cs_axiom('18a2098f-adb5-42fc-a2cf-ac983f1d2a16', foundational, monetary_transition_as_overdetermined).
narrative_ontology:cs_axiom_status(monetary_transition_as_overdetermined, holdable).
narrative_ontology:cs_axiom_grounding('18a2098f-adb5-42fc-a2cf-ac983f1d2a16', monetary_transition_as_overdetermined, empirically_contingent).
narrative_ontology:cs_axiom('18a2098f-adb5-42fc-a2cf-ac983f1d2a16', foundational, causal_singularity_is_misattribution).
narrative_ontology:cs_axiom_status(causal_singularity_is_misattribution, holdable).
narrative_ontology:cs_axiom_grounding('18a2098f-adb5-42fc-a2cf-ac983f1d2a16', causal_singularity_is_misattribution, empirically_contingent).
narrative_ontology:cs_reference_frame('18a2098f-adb5-42fc-a2cf-ac983f1d2a16', convergent_structural_order).
narrative_ontology:cs_drift_state('18a2098f-adb5-42fc-a2cf-ac983f1d2a16', post_2008_centralization, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('18a2098f-adb5-42fc-a2cf-ac983f1d2a16', '').
narrative_ontology:cs_kernel_id(gold_fiat_transition_mechanism__composite_overdetermination_reading, gold_fiat_transition_mechanism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gold_fiat_transition_mechanism__composite_overdetermination_reading, reserve_currency_issuers).
narrative_ontology:constraint_beneficiary(gold_fiat_transition_mechanism__composite_overdetermination_reading, financial_intermediaries).
narrative_ontology:constraint_beneficiary(gold_fiat_transition_mechanism__composite_overdetermination_reading, debtor_governments).
narrative_ontology:constraint_victim(gold_fiat_transition_mechanism__composite_overdetermination_reading, creditor_nations).
narrative_ontology:constraint_victim(gold_fiat_transition_mechanism__composite_overdetermination_reading, fixed_income_savers).
narrative_ontology:constraint_victim(gold_fiat_transition_mechanism__composite_overdetermination_reading, peripheral_economies).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Issue the global reserve currency and benefit from seigniorage, macroeconomic policy flexibility, and the ability to run persistent deficits without external creditor veto. Their monetary dominance is reinforced by legal tender enforcement and network effects, making unilateral exit from the fiat framework self-defeating.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__composite_overdetermination_reading, reserve_currency_issuers, beneficiary,
    institutional, generational, constrained, global).

% Profit from instant capital flows, floating-exchange-rate trading, and intermediation fees enabled by telecommunications infrastructure and deregulated forex markets. They can restructure exposures across jurisdictions and instruments, capturing value from volatility and velocity.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__composite_overdetermination_reading, financial_intermediaries, beneficiary,
    powerful, biographical, arbitrage, global).

% Gain fiscal flexibility and domestic policy space after the elimination of gold-convertibility discipline. They can monetize debt, run countercyclical deficits, and prioritize employment over balance-of-payments constraints, though they remain subject to capital-market sentiment.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__composite_overdetermination_reading, debtor_governments, beneficiary,
    organized, generational, constrained, national).

% Administer the fiat monetary infrastructure through policy discretion, lender-of-last-resort operations, and legal tender enforcement. They coordinate liquidity provision through BIS networks and currency swap lines, deriving authority from operational practice rather than commodity backing.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__composite_overdetermination_reading, central_banks, agenda_setter,
    institutional, generational, constrained, global).

% Lost the gold-redemption mechanism that enforced balance-of-payments discipline on debtors. Hold reserve assets exposed to inflation risk and devaluation. Can diversify reserve portfolios but cannot unilaterally restore the pre-1971 disciplinary structure.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__composite_overdetermination_reading, creditor_nations, payer,
    powerful, generational, constrained, global).

% Bear the inflation tax and financial repression inherent in persistent fiat expansion. Protective instruments are often inaccessible, regulated, or themselves denominated in the depreciating currency, collapsing meaningful exit.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__composite_overdetermination_reading, fixed_income_savers, payer,
    powerless, biographical, constrained, national).

% Subject to volatile capital flows and sudden stops enabled by real-time telecommunications and deregulated forex. Lack effective monetary autonomy due to dollar hegemony and face boom-bust cycles driven by reserve-currency monetary policy spillovers.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__composite_overdetermination_reading, peripheral_economies, payer,
    moderate, generational, constrained, regional).

% Analyze the transition through competing historiographical frameworks. The composite reading emerges from archival and econometric evidence showing multiple independent structural shifts converging in the 1968â1973 window, challenging singular causal narratives.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__composite_overdetermination_reading, monetary_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(gold_fiat_transition_mechanism__composite_overdetermination_reading, diffuse).
narrative_ontology:fixing_cost_class(gold_fiat_transition_mechanism__composite_overdetermination_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates global monetary stability through convergent institutional mechanisms: telecommunications enable real-time settlement, legal tender laws enforce currency acceptance, floating exchange rates absorb asymmetric shocks, and central bank networks provide liquidity backstops.
% TRANSFER_FUNCTION: Transfers purchasing power from fixed-income savers and peripheral economies to reserve-currency issuers and financial intermediaries via inflation tax, seigniorage, and volatility asymmetries; transfers balance-of-payments discipline risk from debtor governments to creditor nations.
% ABSENT_VOICES: Hard-money theorists, gold-standard advocates, and peripheral-economy central bankers are structurally underrepresented in BIS and Fed-centered liquidity forums; their exclusion naturalizes the fiat arrangement as the only viable monetary architecture.
% DISAPPEARANCE_RATIONALE: If the composite fiat arrangement vanished overnight, global trade settlement would freeze, capital flows would seize, debtor governments would face immediate solvency crises, and the convergent infrastructure (SWIFT, FX markets, legal tender regimes) would have to be rebuilt or replaced by an alternative monetary architecture.
% FOUNDING_PROBLEM: The Bretton Woods gold-exchange standard faced Triffin dilemma pressures, dollar overhang, and incompatibility with domestic full-employment mandates amid rising capital mobility.
% FOUNDING_PROBLEM_CORROBORATION: Monetary historians and IMF archival research corroborate the Triffin dilemma and balance-of-payments crises. However, no external party outside the beneficiary set attests to a singular founding problem; the composite reading holds that the singular narrative is itself a post-hoc rationalization masking irreducibly multiple pressures.
narrative_ontology:disappearance_verdict(gold_fiat_transition_mechanism__composite_overdetermination_reading, world_rearranges).
narrative_ontology:founding_problem_status(gold_fiat_transition_mechanism__composite_overdetermination_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gold_fiat_transition_mechanism__composite_overdetermination_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(gold_fiat_transition_mechanism__composite_overdetermination_reading, 'none', 1).
narrative_ontology:epsilon_provenance(gold_fiat_transition_mechanism__composite_overdetermination_reading, 0.5, 'kimi-k2.6', 'none', direct).

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
 *   Extractiveness is moderate (0.50) because the fiat arrangement extracts diffusely across multiple channels rather than concentrating in a single seat; suppression is moderate-high (0.58) because legal tender enforcement, tax denomination requirements, and network externalities actively suppress monetary alternatives; theater is moderate (0.32) because the Nixon Shock functions as performative causal attribution that obscures the underlying structural convergence. Accessibility collapse is high (0.72) because once an economy is integrated into the dollar-based settlement system, reverting to commodity money or parallel currency becomes structurally nearly impossible. Resistance is moderate (0.42) because creditor nations and hard-money advocates have mounted sustained but unsuccessful opposition. The measurement grid is shared: all three metrics are authored at every time point from 1960 to 2020.
 *
 * PERSPECTIVAL GAP:
 *   Reserve currency issuers and financial intermediaries experience the constraint as a coordination framework that enables global macro flexibility and market depth. Fixed income savers and peripheral economies experience the same arrangement as an extractive structure that erodes purchasing power and imposes external volatility. The engine computes this divergence from the structural data: low directionality for beneficiaries with arbitrage or institutional power, high directionality for trapped payers with identity-locked or constrained exit.
 *
 * DIRECTIONALITY LOGIC:
 *   Reserve currency issuers and financial intermediaries sit near the beneficiary end: the constraint subsidizes their policy space and intermediation rents. Debtor governments also sit near the beneficiary end, though with more constrained exit. Fixed income savers sit nearest the full-target end: they are identity-locked into domestic currency denominations and lack arbitrage-grade exit. Peripheral economies are high-target due to constrained monetary sovereignty. Creditor nations are moderate-target: they have reserve diversification options but cannot unilaterally restore the old constraint. Central banks sit near symmetric but slightly toward beneficiary due to their operational control.
 *
 * MANDATROPHY ANALYSIS:
 *   The original Bretton Woods mandateâfixed parities enabling trade reconciliation without competitive devaluationâis dead. However, the arrangement has not atrophied into a piton because new coordination functions (crisis liquidity provision, macro flexibility, tax-based demand maintenance) have partially replaced the founding function. The constraint persists as a tangled rope: genuine coordination coexists with asymmetric extraction. The partial mandatrophy is what prevents classification as either pure scaffold or pure snare.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_singularity_ambiguity,
    'Is the gold-to-fiat transition structurally a single constraint swap, or an irreducibly composite convergence of independent structural changes?',
    'Archival econometric analysis measuring the marginal contribution of telecommunications, labor bargaining, legal tender enforcement, and balance-of-payments pressures to the collapse of Bretton Woods, independent of the Nixon Shock narrative.',
    'If composite convergence is verified, singular readings are reclassified as epistemic misattributions rather than competing constraints; if a unified mechanism dominates, the composite reading dissolves into narrative confusion.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_singularity_ambiguity, empirical, 'Whether the transition kernel is singular or composite.').

omega_variable(
    fiat_alternative_accessibility,
    'Does the fiat system''s persistence reflect accessibility collapse of monetary alternatives, or genuine coordination superiority?',
    'Natural experiment from currency substitution episodes, dollarization, and crypto adoption measuring exit costs and network effects.',
    'If alternatives are structurally suppressed, extraction is higher than coordination-framed metrics suggest; if coordination benefits dominate, the moderate epsilon reading is vindicated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fiat_alternative_accessibility, empirical, 'Whether fiat persistence is suppression or coordination.').

omega_variable(
    distributional_effects_decomposition,
    'Can the distributional effects of the transition be decomposed by causal channel, or do they interact non-separably?',
    'Counterfactual macroeconomic modeling isolating each structural channel''s distributional impact.',
    'If separable, the no-single-beneficiary claim is structurally grounded; if non-separable, the composite reading may mask a unified extraction mechanism.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(distributional_effects_decomposition, conceptual, 'Decomposability of distributional effects by structural channel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gold_fiat_transition_mechanism__composite_overdetermination_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gftm_comp_tr_t0, gold_fiat_transition_mechanism__composite_overdetermination_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(gftm_comp_tr_t10, gold_fiat_transition_mechanism__composite_overdetermination_reading, theater_ratio, 10, 0.35).
narrative_ontology:measurement(gftm_comp_tr_t20, gold_fiat_transition_mechanism__composite_overdetermination_reading, theater_ratio, 20, 0.28).
narrative_ontology:measurement(gftm_comp_tr_t30, gold_fiat_transition_mechanism__composite_overdetermination_reading, theater_ratio, 30, 0.25).
narrative_ontology:measurement(gftm_comp_tr_t40, gold_fiat_transition_mechanism__composite_overdetermination_reading, theater_ratio, 40, 0.3).
narrative_ontology:measurement(gftm_comp_tr_t50, gold_fiat_transition_mechanism__composite_overdetermination_reading, theater_ratio, 50, 0.33).
narrative_ontology:measurement(gftm_comp_tr_t60, gold_fiat_transition_mechanism__composite_overdetermination_reading, theater_ratio, 60, 0.36).

% Extraction over time
narrative_ontology:measurement(gftm_comp_be_t0, gold_fiat_transition_mechanism__composite_overdetermination_reading, base_extractiveness, 0, 0.2).
narrative_ontology:measurement(gftm_comp_be_t10, gold_fiat_transition_mechanism__composite_overdetermination_reading, base_extractiveness, 10, 0.38).
narrative_ontology:measurement(gftm_comp_be_t20, gold_fiat_transition_mechanism__composite_overdetermination_reading, base_extractiveness, 20, 0.45).
narrative_ontology:measurement(gftm_comp_be_t30, gold_fiat_transition_mechanism__composite_overdetermination_reading, base_extractiveness, 30, 0.42).
narrative_ontology:measurement(gftm_comp_be_t40, gold_fiat_transition_mechanism__composite_overdetermination_reading, base_extractiveness, 40, 0.46).
narrative_ontology:measurement(gftm_comp_be_t50, gold_fiat_transition_mechanism__composite_overdetermination_reading, base_extractiveness, 50, 0.49).
narrative_ontology:measurement(gftm_comp_be_t60, gold_fiat_transition_mechanism__composite_overdetermination_reading, base_extractiveness, 60, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(gftm_comp_su_t0, gold_fiat_transition_mechanism__composite_overdetermination_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(gftm_comp_su_t10, gold_fiat_transition_mechanism__composite_overdetermination_reading, suppression_requirement, 10, 0.45).
narrative_ontology:measurement(gftm_comp_su_t20, gold_fiat_transition_mechanism__composite_overdetermination_reading, suppression_requirement, 20, 0.55).
narrative_ontology:measurement(gftm_comp_su_t30, gold_fiat_transition_mechanism__composite_overdetermination_reading, suppression_requirement, 30, 0.52).
narrative_ontology:measurement(gftm_comp_su_t40, gold_fiat_transition_mechanism__composite_overdetermination_reading, suppression_requirement, 40, 0.56).
narrative_ontology:measurement(gftm_comp_su_t50, gold_fiat_transition_mechanism__composite_overdetermination_reading, suppression_requirement, 50, 0.6).
narrative_ontology:measurement(gftm_comp_su_t60, gold_fiat_transition_mechanism__composite_overdetermination_reading, suppression_requirement, 60, 0.63).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(gold_fiat_transition_mechanism__composite_overdetermination_reading, automatic_constraint_reading).
narrative_ontology:affects_constraint(gold_fiat_transition_mechanism__composite_overdetermination_reading, creditor_discipline_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the gold_fiat_transition_mechanism kernel. The three readings (automatic_constraint, composite_overdetermination, creditor_discipline) decompose the colloquial 'Nixon Shock' transition into structurally distinct claims with different epsilon values and stakeholder structures. They form a constraint family linked by network edges.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
