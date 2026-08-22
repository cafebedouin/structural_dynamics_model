% ============================================================================
% CONSTRAINT STORY: ecb_mandate_article_127__climate_incorporation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-04
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ecb_mandate_article_127__climate_incorporation, []).

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
 *   constraint_id: ecb_mandate_article_127__climate_incorporation
 *   human_readable: ECB Mandate Climate Risk Integration (Article 127/11 TFEU Reading)
 *   domain: monetary_policy/constitutional_law/eu_governance
 *
 * SUMMARY:
 *   This constraint instantiates the climate_incorporation reading of the ECB
 *   mandate kernel (Article 127 TFEU). Under this reading, Article 11 TFEU's
 *   environmental integration clause operationally binds the ECB to
 *   incorporate climate risk into its asset purchases and collateral
 *   frameworks. The arrangement coordinates monetary policy with EU climate
 *   goals and systemic risk management while asymmetrically extracting from
 *   carbon-intensive issuers via collateral haircuts and portfolio tilting.
 *   The ECB actively enforces the constraint through its operational
 *   calibrations, and the structural asymmetry between climate-aligned
 *   beneficiaries and fossil fuel targets generates divergent seat
 *   perceptions.
 *
 * KEY AGENTS:
 *   - ECB Governing Council: Agenda-setter (institutional/constrained) â calibrates climate integration into monetary operations under its treaty interpretive authority.
 *   - Green bond issuers: Primary beneficiary (moderate/mobile) â receive preferential collateral and purchase treatment, lowering financing costs.
 *   - Fossil fuel sector: Primary target (powerful/constrained) â bears collateral haircuts and portfolio exclusion, raising refinancing costs.
 *   - Orthodox monetary economists: Analytical observer (analytical) â contest the legal and economic basis for climate operationalization.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ecb_mandate_article_127__climate_incorporation, 0.64).
domain_priors:suppression_score(ecb_mandate_article_127__climate_incorporation, 0.61).
domain_priors:theater_ratio(ecb_mandate_article_127__climate_incorporation, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ecb_mandate_article_127__climate_incorporation, extractiveness, 0.64).
narrative_ontology:constraint_metric(ecb_mandate_article_127__climate_incorporation, suppression_requirement, 0.61).
narrative_ontology:constraint_metric(ecb_mandate_article_127__climate_incorporation, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ecb_mandate_article_127__climate_incorporation, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(ecb_mandate_article_127__climate_incorporation, resistance, 0.52).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ecb_mandate_article_127__climate_incorporation, tangled_rope).
narrative_ontology:human_readable(ecb_mandate_article_127__climate_incorporation, "ECB Mandate Climate Risk Integration (Article 127/11 TFEU Reading)").
narrative_ontology:topic_domain(ecb_mandate_article_127__climate_incorporation, "monetary_policy/constitutional_law/eu_governance").

domain_priors:requires_active_enforcement(ecb_mandate_article_127__climate_incorporation).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ecb_mandate_article_127__climate_incorporation, 'aa60a8c5-bd00-47b0-94e8-194062be3908').
narrative_ontology:cs_kernel_codification('aa60a8c5-bd00-47b0-94e8-194062be3908', formalized).
narrative_ontology:cs_authority_grounding('aa60a8c5-bd00-47b0-94e8-194062be3908', lineage).
narrative_ontology:cs_interpretation_layer_present('aa60a8c5-bd00-47b0-94e8-194062be3908').
narrative_ontology:cs_reading_relation('aa60a8c5-bd00-47b0-94e8-194062be3908', ecb_mandate_article_127__orthodox_price_stability, coexists_with).
narrative_ontology:cs_reading_relation('aa60a8c5-bd00-47b0-94e8-194062be3908', ecb_mandate_article_127__expansive_secondary_objectives, influences).
narrative_ontology:cs_axiom('aa60a8c5-bd00-47b0-94e8-194062be3908', foundational, article_11_tfeu_operational_binding).
narrative_ontology:cs_axiom_status(article_11_tfeu_operational_binding, holdable).
narrative_ontology:cs_axiom_grounding('aa60a8c5-bd00-47b0-94e8-194062be3908', article_11_tfeu_operational_binding, conventional).
narrative_ontology:cs_reference_frame('aa60a8c5-bd00-47b0-94e8-194062be3908', climate_risk_inclusive_mandate).
narrative_ontology:cs_drift_state('aa60a8c5-bd00-47b0-94e8-194062be3908', post_2021_strategy_review, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('aa60a8c5-bd00-47b0-94e8-194062be3908', '').
narrative_ontology:cs_kernel_id(ecb_mandate_article_127__climate_incorporation, ecb_mandate_article_127).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ecb_mandate_article_127__climate_incorporation, green_bond_issuers).
narrative_ontology:constraint_victim(ecb_mandate_article_127__climate_incorporation, fossil_fuel_sector).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets monetary policy operational frameworks including collateral eligibility and asset purchase portfolios. Integrates climate risk metrics into these frameworks under its reading of Article 127 TFEU as informed by Article 11 TFEU. Cannot exit the treaty framework but exercises interpretive discretion in calibration.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__climate_incorporation, ecb_governing_council, agenda_setter,
    institutional, generational, constrained, continental).

% Receive preferential treatment in ECB collateral frameworks and corporate asset purchases, lowering their financing costs relative to conventional issuers. Can access multiple funding markets but benefit specifically from ECB-driven demand in eurozone bond markets.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__climate_incorporation, green_bond_issuers, beneficiary,
    moderate, biographical, mobile, continental).

% Faces escalating collateral haircuts and exclusion from ECB purchase programs, which raises refinancing costs and compresses market access within the eurozone banking system. Deeply embedded in legacy collateral chains and eurozone bank lending relationships, making rapid exit costly.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__climate_incorporation, fossil_fuel_sector, payer,
    powerful, biographical, constrained, global).

% Contest the operationalization of climate objectives within monetary policy, arguing that price stability is the sole primary mandate and that climate integration lacks proven materiality to inflation outcomes. Provide analytical resistance through legal opinions and academic critique.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__climate_incorporation, orthodox_monetary_economists, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Integrates environmental policy considerations into central bank operations to align monetary policy with the EU treaty environmental integration clause and to manage systemic climate-related financial risks in eurozone collateral and portfolios.
% TRANSFER_FUNCTION: Moves relative financing cost advantage from carbon-intensive issuers to climate-aligned issuers via differentiated collateral haircuts, eligibility criteria, and asset purchase portfolio tilting.
% ABSENT_VOICES: Fossil fuel-dependent member state governments, non-eurozone carbon-intensive industries, and legal scholars who reject Art 11 TFEU as directly binding on monetary policy are underrepresented in ECB strategy calibration.
% DISAPPEARANCE_RATIONALE: If the climate integration mandate vanished overnight, ECB portfolios and collateral frameworks would revert to market-neutral or credit-risk-only weighting, green bond spreads would widen, fossil fuel sector refinancing costs in the eurozone would fall, and the ECB's institutional stance would revert toward the orthodox price stability framework â rearranging both market pricing and EU policy alignment.
% FOUNDING_PROBLEM: Classical monetary policy frameworks treated environmental externalities as outside the central bank mandate, leaving systemic climate financial risks unpriced in eurozone collateral and asset portfolios.
% FOUNDING_PROBLEM_CORROBORATION: Climate-aligned policymakers and ECB legal staff cite Art 11 TFEU as mandating integration. Orthodox monetary economists and some national central bankers contest that climate risk integration is the proper founding problem, arguing the mandate is price stability alone; no external non-beneficiary attests the climate risk framing as the original problem the ECB was built to solve.
narrative_ontology:disappearance_verdict(ecb_mandate_article_127__climate_incorporation, world_rearranges).
narrative_ontology:founding_problem_status(ecb_mandate_article_127__climate_incorporation, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ecb_mandate_article_127__climate_incorporation, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(ecb_mandate_article_127__climate_incorporation, 'none', 1).
narrative_ontology:epsilon_provenance(ecb_mandate_article_127__climate_incorporation, 0.64, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ecb_mandate_article_127__climate_incorporation_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(ecb_mandate_article_127__climate_incorporation, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(ecb_mandate_article_127__climate_incorporation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness (0.64 at interval end) reflects the significant cost shift imposed on carbon-intensive issuers through differential collateral and purchase treatment. Suppression (0.61) captures the active portfolio tilting mechanism that structurally disadvantages fossil fuel sector access to ECB liquidity and purchase demand; it is a raw structural property and is not scaled by context. Theater ratio (0.28) is moderate because the financial risk rationale is partially substantive, though disclosure and signaling requirements introduce performative overhead. Accessibility collapse (0.48) is moderate: global capital markets provide partial alternatives, but eurozone banking system dependence on ECB collateral and purchase programs makes exit costly. Resistance (0.52) reflects persistent legal and political contestation from orthodox economists and industry actors. The measurement series tracks the ramp-up from pre-strategy review (2016) to full operationalization (2024) on a single shared time grid.
 *
 * PERSPECTIVAL GAP:
 *   The green bond issuers and ECB agenda-setter experience the constraint as prudent risk management and treaty compliance, while the fossil fuel sector experiences it as politically motivated extraction through monetary channels. The orthodox observer seat sees an illegitimate expansion of mandate. These divergences are structurally driven by directional position: beneficiaries receive subsidized access while targets bear haircut costs.
 *
 * DIRECTIONALITY LOGIC:
 *   The ECB Governing Council sits near the beneficiary end (d low) because the integration expands its institutional relevance and operational discretion within the treaty framework. Green bond issuers are clear beneficiaries (d low) via preferential purchase and collateral treatment. Fossil fuel sector is the structural target (d high) because the same frameworks impose explicit haircuts and exclusion. Exit options amplify this: green issuers are mobile across funding markets, whereas fossil fuel firms are constrained by legacy eurozone banking and collateral embedding.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem â systemic climate financial risks left unpriced by classical monetary policy â is contested. If the problem is genuinely live and the ECB's climate integration materially reduces systemic risk, the constraint retains substantial coordination function. If the problem is overstated or the framework has shifted toward industrial policy preference, the coordination function atrophies and extraction dominates. The temporal series shows monotonically rising extractiveness and suppression requirement from 2016 to 2024, consistent with enforcement maturation but also with potential mandatrophy as industrial policy layering accumulates on the original risk-management justification. The divergence between the claimed coordination function and the rising extraction metric is the signal the engine is meant to measure.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    art_11_tfeu_binding_force,
    'Does Article 11 TFEU create a legally binding operational obligation on the ECB, or is it a general integration principle without direct justiciable force in monetary policy?',
    'Authoritative ECJ ruling on the legality of ECB climate-related asset purchase and collateral decisions, or formal treaty interpretation by the EU Council.',
    'If Article 11 is non-binding, the constraint''s enforcement rests on ECB political discretion rather than treaty obligation, shifting classification toward rope or scaffold; if binding, the tangled rope extraction is treaty-entrenched.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(art_11_tfeu_binding_force, conceptual, 'Legal ambiguity over Art 11 TFEU binding force on ECB monetary policy').

omega_variable(
    climate_risk_vs_industrial_policy,
    'Does the ECB''s climate integration price material financial risk, or does it impose industrial policy preferences through monetary tools?',
    'Empirical comparison of ECB climate haircuts and tilting decisions against private-sector climate risk pricing and credit default data.',
    'If the latter, the coordination story is cover for extraction and effective extractiveness is higher than the risk-management framing suggests; if the former, the coordination function is genuine.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(climate_risk_vs_industrial_policy, empirical, 'Whether ECB climate integration tracks risk or policy preference').

omega_variable(
    portfolio_tilting_suppression_nature,
    'Is portfolio tilting a novel suppression mechanism that structurally disadvantages carbon-intensive sectors, or a neutral prudential risk-management practice?',
    'Counterfactual analysis of fossil fuel sector financing costs in a market-neutral ECB portfolio regime versus the current tilted regime.',
    'Determines whether the suppression metric reflects enforceable extraction or legitimate risk adjustment, with consequences for directionality calibration.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(portfolio_tilting_suppression_nature, conceptual, 'Ambiguity over whether portfolio tilting is suppression or prudence').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ecb_mandate_article_127__climate_incorporation, 0, 8).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ecb_climate_tr_t0, ecb_mandate_article_127__climate_incorporation, theater_ratio, 0, 0.1).
narrative_ontology:measurement(ecb_climate_tr_t2, ecb_mandate_article_127__climate_incorporation, theater_ratio, 2, 0.15).
narrative_ontology:measurement(ecb_climate_tr_t4, ecb_mandate_article_127__climate_incorporation, theater_ratio, 4, 0.2).
narrative_ontology:measurement(ecb_climate_tr_t6, ecb_mandate_article_127__climate_incorporation, theater_ratio, 6, 0.25).
narrative_ontology:measurement(ecb_climate_tr_t8, ecb_mandate_article_127__climate_incorporation, theater_ratio, 8, 0.28).

% Extraction over time
narrative_ontology:measurement(ecb_climate_be_t0, ecb_mandate_article_127__climate_incorporation, base_extractiveness, 0, 0.2).
narrative_ontology:measurement(ecb_climate_be_t2, ecb_mandate_article_127__climate_incorporation, base_extractiveness, 2, 0.3).
narrative_ontology:measurement(ecb_climate_be_t4, ecb_mandate_article_127__climate_incorporation, base_extractiveness, 4, 0.42).
narrative_ontology:measurement(ecb_climate_be_t6, ecb_mandate_article_127__climate_incorporation, base_extractiveness, 6, 0.55).
narrative_ontology:measurement(ecb_climate_be_t8, ecb_mandate_article_127__climate_incorporation, base_extractiveness, 8, 0.64).

% Suppression requirement over time
narrative_ontology:measurement(ecb_climate_su_t0, ecb_mandate_article_127__climate_incorporation, suppression_requirement, 0, 0.15).
narrative_ontology:measurement(ecb_climate_su_t2, ecb_mandate_article_127__climate_incorporation, suppression_requirement, 2, 0.25).
narrative_ontology:measurement(ecb_climate_su_t4, ecb_mandate_article_127__climate_incorporation, suppression_requirement, 4, 0.38).
narrative_ontology:measurement(ecb_climate_su_t6, ecb_mandate_article_127__climate_incorporation, suppression_requirement, 6, 0.5).
narrative_ontology:measurement(ecb_climate_su_t8, ecb_mandate_article_127__climate_incorporation, suppression_requirement, 8, 0.61).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ecb_mandate_article_127__climate_incorporation, enforcement_mechanism).
narrative_ontology:affects_constraint(ecb_mandate_article_127__climate_incorporation, ecb_mandate_article_127__orthodox_price_stability).
narrative_ontology:affects_constraint(ecb_mandate_article_127__climate_incorporation, ecb_mandate_article_127__expansive_secondary_objectives).

% DUAL FORMULATION NOTE:
% This story is one member of the ecb_mandate_article_127 constraint family. It shares the treaty kernel with orthodox_price_stability and expansive_secondary_objectives but differs in epsilon, beneficiary/victim structure, and the operational status of secondary objectives. Decomposition follows the epsilon-invariance principle: the climate integration claim has a structurally distinct extraction profile from the orthodox price stability claim.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
