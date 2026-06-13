% ============================================================================
% CONSTRAINT STORY: ecb_mandate_article_127__climate_incorporation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: ecb_mandate_article_127__climate_incorporation
 *   human_readable: ECB Article 127 Climate Risk Integration Mandate
 *   domain: monetary_policy/constitutional_law/eu_institutional_governance
 *
 * SUMMARY:
 *   The ECB's Article 127 mandate requires monetary policy support for EU
 *   objectives 'without prejudice' to price stability. Three sharply distinct
 *   readings of this kernel compete: (1) the ORTHODOX reading treats climate
 *   as unauthorized mandate creep and price stability as exclusive focus; (2)
 *   the EXPANSIVE reading permits discretionary balancing of secondary
 *   objectives when price stability permits; (3) THIS READING
 *   (climate_incorporation) treats Article 11 TFEU environmental integration
 *   as MANDATORY, operationalized through collateral haircuts, portfolio
 *   tilting, and risk-adjusted eligibility. This story instantiates the third
 *   reading only. The climate incorporation reading involves substantial
 *   extraction from fossil fuel sectors and small banks with legacy
 *   portfolios, bundled with genuine coordination on climate transition risk.
 *   The founding problem is contested — orthodox readers deny the mandate
 *   foundation, while climate-integration readers cite Article 11 as binding
 *   constraint on ECB action.
 *
 * KEY AGENTS:
 *   - ECB Governing Council: Interprets and implements Article 127 climate integration; controls collateral haircut schedules and portfolio tilt parameters
 *   - Fossil fuel sector: Structural target of extraction via collateral devaluation and financing-cost increases; constrained exit within euro zone
 *   - Climate-aligned investors & renewable energy sector: Structural beneficiaries of preferential collateral treatment and ECB buying power redirection
 *   - Small banks with legacy industrial portfolios: Trapped payers facing unplanned collateral devaluation without transition support
 *   - Orthodox defenders: Excluded from agenda-setting; mount legal and academic resistance
 *   - German constitutional court: Holds veto power via ultra vires review but defers to EU institutional hierarchy
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ecb_mandate_article_127__climate_incorporation, 0.68).
domain_priors:suppression_score(ecb_mandate_article_127__climate_incorporation, 0.72).
domain_priors:theater_ratio(ecb_mandate_article_127__climate_incorporation, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ecb_mandate_article_127__climate_incorporation, extractiveness, 0.68).
narrative_ontology:constraint_metric(ecb_mandate_article_127__climate_incorporation, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(ecb_mandate_article_127__climate_incorporation, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ecb_mandate_article_127__climate_incorporation, accessibility_collapse, 0.61).
narrative_ontology:constraint_metric(ecb_mandate_article_127__climate_incorporation, resistance, 0.74).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ecb_mandate_article_127__climate_incorporation, tangled_rope).
narrative_ontology:human_readable(ecb_mandate_article_127__climate_incorporation, "ECB Article 127 Climate Risk Integration Mandate").
narrative_ontology:topic_domain(ecb_mandate_article_127__climate_incorporation, "monetary_policy/constitutional_law/eu_institutional_governance").

domain_priors:requires_active_enforcement(ecb_mandate_article_127__climate_incorporation).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ecb_mandate_article_127__climate_incorporation, '061c12a9-2927-43c0-88ee-5c0cf2072baf').
narrative_ontology:cs_kernel_codification('061c12a9-2927-43c0-88ee-5c0cf2072baf', fixed_text).
narrative_ontology:cs_authority_grounding('061c12a9-2927-43c0-88ee-5c0cf2072baf', extraction).
narrative_ontology:cs_interpretation_layer_present('061c12a9-2927-43c0-88ee-5c0cf2072baf').
narrative_ontology:cs_reading_relation('061c12a9-2927-43c0-88ee-5c0cf2072baf', ecb_mandate_article_127__orthodox_price_stability, forecloses).
narrative_ontology:cs_reading_relation('061c12a9-2927-43c0-88ee-5c0cf2072baf', ecb_mandate_article_127__expansive_secondary_objectives, influences).
narrative_ontology:cs_axiom('061c12a9-2927-43c0-88ee-5c0cf2072baf', foundational, article_11_environmental_integration_mandatory).
narrative_ontology:cs_axiom_status(article_11_environmental_integration_mandatory, holdable).
narrative_ontology:cs_axiom_grounding('061c12a9-2927-43c0-88ee-5c0cf2072baf', article_11_environmental_integration_mandatory, conventional).
narrative_ontology:cs_axiom('061c12a9-2927-43c0-88ee-5c0cf2072baf', secondary, climate_transition_risk_is_financial_stability_risk).
narrative_ontology:cs_axiom_status(climate_transition_risk_is_financial_stability_risk, holdable).
narrative_ontology:cs_axiom_grounding('061c12a9-2927-43c0-88ee-5c0cf2072baf', climate_transition_risk_is_financial_stability_risk, empirically_contingent).
narrative_ontology:cs_reference_frame('061c12a9-2927-43c0-88ee-5c0cf2072baf', price_stability_exclusive_mandate).
narrative_ontology:cs_drift_state('061c12a9-2927-43c0-88ee-5c0cf2072baf', contemporary_eu_climate_governance_integration, gap(authority_erosion, substantial, true)).
narrative_ontology:cs_created_at('061c12a9-2927-43c0-88ee-5c0cf2072baf', '').
narrative_ontology:cs_kernel_id(ecb_mandate_article_127__climate_incorporation, ecb_mandate_article_127).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ecb_mandate_article_127__climate_incorporation, climate_transition_aligned_investors).
narrative_ontology:constraint_beneficiary(ecb_mandate_article_127__climate_incorporation, renewable_energy_sector).
narrative_ontology:constraint_beneficiary(ecb_mandate_article_127__climate_incorporation, eu_climate_policy_agenda).
narrative_ontology:constraint_victim(ecb_mandate_article_127__climate_incorporation, fossil_fuel_producers).
narrative_ontology:constraint_victim(ecb_mandate_article_127__climate_incorporation, carbon_intensive_collateral_holders).
narrative_ontology:constraint_victim(ecb_mandate_article_127__climate_incorporation, small_banks_with_legacy_portfolios).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(ecb_mandate_article_127__climate_incorporation, fossil_fuel_sector).
narrative_ontology:constraint_victim(ecb_mandate_article_127__climate_incorporation, small_banks_legacy_portfolio_holders).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interprets Article 127 TFEU and implements climate risk integration into collateral frameworks and asset purchases. Argues the mandate obligates climate consideration under Article 11 environmental integration; holds discretion over implementation severity. Controls the eligibility criteria, haircut schedules, and portfolio tilt parameters that operationalize the constraint.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__climate_incorporation, ecb_governing_council, agenda_setter,
    institutional, generational, analytical, continental).

% Faces collateral haircuts and reduced ECB eligibility, raising financing costs and limiting the securities they can pledge. Cannot exit euro monetary zone; litigation has largely failed. Must absorb higher funding costs or accelerate divestment/transition. The haircuts are presented as 'risk adjustment' but proportionately exceed traditional credit risk metrics.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__climate_incorporation, fossil_fuel_sector, payer,
    powerful, generational, constrained, continental).

% Benefit from collateral preferencing and reduced haircuts on green bonds and transition-aligned securities. The constraint redirects ECB buying power toward their holdings, raising values and lowering refinancing costs. They operate with exit optionality (can relocate capital or hedge exposures).
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__climate_incorporation, climate_transition_aligned_investors, beneficiary,
    powerful, generational, mobile, global).

% Benefits from preferential collateral treatment and ECB portfolio tilt toward green financing. Lower financing costs, expanded access to ECB liquidity windows, and policy support for sector expansion. Coordinates through industry associations and can shift financing sources if ECB support weakens.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__climate_incorporation, renewable_energy_sector, beneficiary,
    organized, generational, mobile, continental).

% Hold significant legacy industrial and carbon-intensive collateral from long-standing regional borrowers. Face haircut escalation without equivalent transition support. Cannot rapidly rebalance; refinancing options are limited to other euro-zone central banks with similar policies. Caught between collateral devaluation and borrower relationship disruption.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__climate_incorporation, small_banks_legacy_portfolio_holders, payer,
    moderate, biographical, trapped, national).
narrative_ontology:stakeholder_secondary_role(ecb_mandate_article_127__climate_incorporation, small_banks_legacy_portfolio_holders, excluded).

% The European Green Deal and climate neutrality target (2050) are vindicated by ECB collateral policy. Central bank operational alignment with legislative climate mandates reduces friction and amplifies policy reach. The constraint operationalizes Article 11 TFEU environmental integration across monetary operations.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__climate_incorporation, eu_climate_policy_agenda, beneficiary,
    institutional, generational, analytical, continental).
narrative_ontology:stakeholder_non_agent(ecb_mandate_article_127__climate_incorporation, eu_climate_policy_agenda).

% Argue the mandate permits only price stability focus; climate integration is unauthorized goal creep. Cannot reverse ECB decisions through alternative governance channels (no supranational climate authority); litigation in CJEU has produced mixed results. Excluded from agenda-setting but mount organized legal and academic resistance.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__climate_incorporation, orthodox_price_stability_defenders, excluded,
    institutional, generational, constrained, continental).

% Reviews ECB monetary policy decisions under German law; has previously constrained ECB action (OMT, PSPP cases). Can declare ECB climate integration ultra vires if it finds insufficient mandate foundation or disproportionate fiscal effects. Holds veto power but acts only in response to referrals and faces political pressure to defer to EU institutional hierarchy.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__climate_incorporation, german_constitutional_court, observer,
    institutional, biographical, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(ecb_mandate_article_127__climate_incorporation, ecb_governing_council).
narrative_ontology:fixing_cost_class(ecb_mandate_article_127__climate_incorporation, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Aligns ECB monetary operations with EU climate policy targets (Article 11 TFEU environmental integration), solving the coordination problem of central bank independence pulling away from European legislative climate commitments. Integrates systemic financial risk (climate transition risk) into collateral frameworks, which is a genuine risk-management coordination function.
% TRANSFER_FUNCTION: Redistributes financing costs and collateral eligibility between fossil fuel sectors (bearing haircuts, reduced access) and climate-aligned sectors (receiving preferential treatment, lower haircuts). Transfers policy authority from purely price-stability grounds to include climate risk considerations, shifting the mandate's operational center.
% ABSENT_VOICES: Stranded-asset holders in developing economies; transition workers in coal and oil regions without retraining support; central banks of fossil-fuel-dependent states (non-EU) excluded from ECB collateral policy scope but affected by collateral cascades. Academic economists arguing Article 127 forbids climate operational weight are sidelined in regulatory spaces.
% DISAPPEARANCE_RATIONALE: If climate haircuts and portfolio tilt vanished, fossil fuel financing costs would drop sharply, green bond preferencing would evaporate, and renewable sector refinancing would depend on commercial terms. EU climate policy would lose operational central bank support and face funding pressure. Fossil fuel assets would re-enter ECB collateral pools at pre-climate-integration valuations.
% FOUNDING_PROBLEM: Article 11 TFEU mandates environmental integration into EU policy-making; ECB operations were exempted from this integration. Climate transition risk emerged as a systemic financial stability threat (stranded assets, abrupt repricing). The founding problem: how can the ECB honor Article 11 while maintaining its independence and price-stability mandate?
% FOUNDING_PROBLEM_CORROBORATION: ECB staff analysis and external climate finance research (NGFS, academic economists outside ECB) corroborate that climate transition risk is real and poses financial stability concerns. However, the orthodox reading contests whether Article 11 integration is MANDATORY or merely PERMISSIVE. The ECB's own interpretation treats it as mandatory; the founding problem is contested by institutional actors (German constitutional court, some CJEU judges, orthodox monetary economists) who deny the mandate foundation.
narrative_ontology:disappearance_verdict(ecb_mandate_article_127__climate_incorporation, world_rearranges).
narrative_ontology:founding_problem_status(ecb_mandate_article_127__climate_incorporation, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ecb_mandate_article_127__climate_incorporation, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(ecb_mandate_article_127__climate_incorporation, 'none', 1).

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
 *   Extractiveness rises from 0.12 (2015, minimal climate integration) to 0.68 (2030, mature implementation) following ECB policy escalation. Theater_ratio rises from 0.05 to 0.48, indicating growing decoupling between stated risk-management rationale and actual portfolio tilt magnitude — by 2024, ECB's green bond purchases exceed climate risk metrics alone would justify, signaling policy objectives beyond narrow financial stability. Suppression_requirement tracks extractiveness closely (0.15→0.72), reflecting growing enforcement cost as fossil fuel sector litigation and German constitutional court challenges accumulate. The constraint requires active enforcement because collateral haircuts and portfolio tilt face legal contestation and would collapse without ECB credible commitment to maintain them. Beneficiary beneficiaries (climate sectors) are genuine but non-captive; they have exit optionality (can access commercial financing). Victims (fossil fuel, small banks) are trapped — they cannot escape euro zone monetary policy or rapidly rebalance collateral portfolios. This asymmetry is the core tangled_rope structure: coordination on climate transition risk serves a real function (managing systemic financial risk), but the implementation extracts disproportionately from those least able to transition, creating what the victims experience as regulatory exaction.
 *
 * PERSPECTIVAL GAP:
 *   From the ECB Governing Council's analytical seat, the constraint solves a coordination problem: Article 11 mandates environmental integration, climate risk is financial risk, collateral frameworks must price it. From the fossil fuel sector's seat, the same operations appear as unauthorized mandate expansion using financial regulation to execute climate policy that the monetary mandate does not permit. From the small bank's seat, collateral haircuts are exogenous regulatory shocks imposed without transition support. The engine computes per-seat directionality from these structural differences: ECB has analytical exit (can reinterpret the mandate), beneficiaries have mobile exit (can access alternative capital), victims have trapped or identity_locked exit (cannot escape euro zone or rapidly rebalance 30-year-maturity collateral). This produces divergent d values: ECB near 0.5 (symmetric, analytical framing), climate sectors near 0.1 (full beneficiaries), fossil fuel near 0.9 (full targets).
 *
 * DIRECTIONALITY LOGIC:
 *   The climate_incorporation reading binds the ECB's interpretation. Under this reading, Article 127 + Article 11 TFEU creates a mandatory climate integration clause. The ECB's directionality sits at d~0.5 (agenda-setter, analytical seat — can reinterpret but chooses to implement aggressively, so partial commitment). Fossil fuel sector at d~0.85 (full target: collateral haircuts, financing-cost increases, no exit within zone). Climate-aligned sectors at d~0.15 (full beneficiaries: preferential haircuts, ECB buying power). Small banks at d~0.75 (near-target: unplanned collateral devaluation, trapped within zone). The directionality overrides are not needed — the structural derivation captures the asymmetry. However, a potential override consideration: German constitutional court sits at d~0.5 (observer, analytical), but its veto power over ECB actions is suppressed by deference to EU institutional hierarchy. Commentary notes the suppression; the override would be inappropriate because it is not a misclassification of the court's structural position (analytical, analytical exit) but rather a question of institutional power ordering (EU > national), which is structural fact, not derived directionality.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem is LIVE but CONTESTED. Article 11 environmental integration is written into TFEU; climate transition risk is empirically real and systemic. But the orthodox reading contests whether the ECB mandate permits mandatory climate integration or only permits it (discretionary, conditional on price stability). The constraint avoids mandatrophy (constraint persisting after founding problem vanishes) because the problem is truly live: if climate transition risk ceased to pose financial stability concerns, or if Article 11 TFEU were amended to exclude ECB, the constraint would genuinely dissolve. However, the constraint is THEATER-RISING (theater_ratio 0.05→0.48): the ECB's stated justification is financial risk management, but the actual haircut schedules and portfolio weights increasingly diverge from a purely risk-based model, tracking instead climate policy targets. This rising theater signals mandate-function tension: the ECB is operationalizing climate policy using financial regulation language, which is the characteristic double-speak of a constraint whose founding problem (climate risk) genuinely exists but whose actual enforcement increasingly serves a secondary objective (EU climate policy alignment) that the mandate more ambiguously authorizes.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    article_127_mandate_scope,
    'Does Article 127 TFEU mandate climate integration into ECB operations, merely permit it, or forbid it as unauthorized mandate creep beyond price stability?',
    'CJEU constitutional review of ECB collateral framework decisions; systematic analysis of ECB''s own mandate interpretation documents (ECB Opinions, governing council meeting minutes); comparison with CJEU precedent on Article 11 integration requirements for other EU institutions.',
    'If CJEU rules mandatory: the climate incorporation reading is vindicated, suppression requirements may decline (legal basis clarified), and extraction becomes explicitly authorized institutional action. If CJEU rules permissive or forbidden: the constraint''s legal basis crumbles, beneficiaries lose policy support, fossil fuel sectors gain grounds for collateral-framework reversal.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(article_127_mandate_scope, conceptual, 'Whether Article 127 mandates, permits, or forbids climate integration in ECB operations.').

omega_variable(
    climate_risk_quantification_divergence,
    'What is the true financial stability cost of climate transition risk in ECB-eligible collateral, and how does it compare to the haircut schedules the ECB has implemented?',
    'Independent climate risk stress-testing (Bank for International Settlements, academic financial economists); comparison of ECB haircuts to equivalent climate-risk-adjusted credit spreads in commercial markets; disclosure of ECB''s internal climate risk models.',
    'If ECB haircuts EXCEED climate risk metrics: the constraint is extractive relative to its stated risk-management purpose (theater_ratio rises, mandatrophy risk increases). If haircuts ALIGN with metrics: the constraint is defensible as pure risk adjustment. If haircuts UNDER-weight climate risk: the climate sectors are not receiving as much implicit subsidy as beneficiary analysis suggests.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(climate_risk_quantification_divergence, empirical, 'Gap between ECB''s stated climate risk metrics and actual collateral haircut implementation.').

omega_variable(
    mandate_vs_mandate_conflict,
    'Is the ECB''s operational implementation of climate integration in conflict with the independence requirement of the ESCB, or does Article 11 environmental integration override independence concerns in this domain?',
    'CJEU ruling on institutional independence boundaries; analysis of whether climate integration limits policy discretion inappropriately (i.e., constrains the ECB''s ability to conduct price-stability policy) or enhances it (i.e., adds a necessary risk dimension).',
    'If independence-limiting: ECB can argue for narrower climate integration scope. If independence-neutral or enhancing: independence doctrine provides no defense against climate integration expansion.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mandate_vs_mandate_conflict, conceptual, 'Whether Article 11 environmental integration and ECB independence are structurally compatible.').

omega_variable(
    fossil_sector_exit_impossibility,
    'Can fossil fuel firms and small banks holding legacy industrial collateral realistically transition out of euro-zone dependence, or is their exit genuinely impossible (trapped directionality)?',
    'Empirical analysis of refinancing options for stranded-asset holders: can they access non-euro financing, relocate operations, or credibly diversify away from euro-zone collateral markets? What is the actual cost of exit relative to absorption of collateral haircuts?',
    'If exit is impossible: the victims remain trapped, suppression mechanisms work through economic strangulation. If exit is costly but possible: the constraint''s extraction operates via cost-elevation rather than absolute suppression; directionality may shift from 0.9 toward 0.7. If exit is cheap: the constraint''s extractive power is limited by exit threat credibility.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(fossil_sector_exit_impossibility, empirical, 'Whether fossil sector and small-bank exit from ECB collateral dependence is genuinely impossible or merely costly.').

omega_variable(
    kernel_reading_committer,
    'Which reading of the Article 127 kernel is the ''correct'' interpretation of the treaty text, or is correctness underdetermined by the text itself?',
    'Structural linguistic analysis of Article 127 and Article 11 semantics; historical legislative intent documents (TFEU travaux préparatoires); CJEU interpretive precedent on environmental integration clauses in other EU institutions.',
    'If climate_incorporation reading is demonstrably correct per treaty text: the constraint gains legal certainty and suppression mechanisms weaken (no longer contested). If the text underdetermines the reading: the constraint''s authority depends on ECB institutional credibility and political acceptance, not on mandate clarity.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_committer, conceptual, 'Whether Article 127 + Article 11 semantically mandates or permits climate integration, or whether the text is genuinely underdetermined.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ecb_mandate_article_127__climate_incorporation, 2015, 2030).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ecb_climate_127_tr_t2015, ecb_mandate_article_127__climate_incorporation, theater_ratio, 2015, 0.05).
narrative_ontology:measurement_basis(ecb_climate_127_tr_t2015, observed).
narrative_ontology:measurement(ecb_climate_127_tr_t2018, ecb_mandate_article_127__climate_incorporation, theater_ratio, 2018, 0.12).
narrative_ontology:measurement_basis(ecb_climate_127_tr_t2018, observed).
narrative_ontology:measurement(ecb_climate_127_tr_t2021, ecb_mandate_article_127__climate_incorporation, theater_ratio, 2021, 0.35).
narrative_ontology:measurement_basis(ecb_climate_127_tr_t2021, observed).
narrative_ontology:measurement(ecb_climate_127_tr_t2024, ecb_mandate_article_127__climate_incorporation, theater_ratio, 2024, 0.47).
narrative_ontology:measurement_basis(ecb_climate_127_tr_t2024, observed).
narrative_ontology:measurement(ecb_climate_127_tr_t2027, ecb_mandate_article_127__climate_incorporation, theater_ratio, 2027, 0.51).
narrative_ontology:measurement_basis(ecb_climate_127_tr_t2027, projected).
narrative_ontology:measurement(ecb_climate_127_tr_t2030, ecb_mandate_article_127__climate_incorporation, theater_ratio, 2030, 0.48).
narrative_ontology:measurement_basis(ecb_climate_127_tr_t2030, projected).

% Extraction over time
narrative_ontology:measurement(ecb_climate_127_be_t2015, ecb_mandate_article_127__climate_incorporation, base_extractiveness, 2015, 0.12).
narrative_ontology:measurement_basis(ecb_climate_127_be_t2015, observed).
narrative_ontology:measurement(ecb_climate_127_be_t2018, ecb_mandate_article_127__climate_incorporation, base_extractiveness, 2018, 0.28).
narrative_ontology:measurement_basis(ecb_climate_127_be_t2018, observed).
narrative_ontology:measurement(ecb_climate_127_be_t2021, ecb_mandate_article_127__climate_incorporation, base_extractiveness, 2021, 0.52).
narrative_ontology:measurement_basis(ecb_climate_127_be_t2021, observed).
narrative_ontology:measurement(ecb_climate_127_be_t2024, ecb_mandate_article_127__climate_incorporation, base_extractiveness, 2024, 0.65).
narrative_ontology:measurement_basis(ecb_climate_127_be_t2024, observed).
narrative_ontology:measurement(ecb_climate_127_be_t2027, ecb_mandate_article_127__climate_incorporation, base_extractiveness, 2027, 0.7).
narrative_ontology:measurement_basis(ecb_climate_127_be_t2027, projected).
narrative_ontology:measurement(ecb_climate_127_be_t2030, ecb_mandate_article_127__climate_incorporation, base_extractiveness, 2030, 0.68).
narrative_ontology:measurement_basis(ecb_climate_127_be_t2030, projected).

% Suppression requirement over time
narrative_ontology:measurement(ecb_climate_127_su_t2015, ecb_mandate_article_127__climate_incorporation, suppression_requirement, 2015, 0.15).
narrative_ontology:measurement_basis(ecb_climate_127_su_t2015, observed).
narrative_ontology:measurement(ecb_climate_127_su_t2018, ecb_mandate_article_127__climate_incorporation, suppression_requirement, 2018, 0.38).
narrative_ontology:measurement_basis(ecb_climate_127_su_t2018, observed).
narrative_ontology:measurement(ecb_climate_127_su_t2021, ecb_mandate_article_127__climate_incorporation, suppression_requirement, 2021, 0.62).
narrative_ontology:measurement_basis(ecb_climate_127_su_t2021, observed).
narrative_ontology:measurement(ecb_climate_127_su_t2024, ecb_mandate_article_127__climate_incorporation, suppression_requirement, 2024, 0.71).
narrative_ontology:measurement_basis(ecb_climate_127_su_t2024, observed).
narrative_ontology:measurement(ecb_climate_127_su_t2027, ecb_mandate_article_127__climate_incorporation, suppression_requirement, 2027, 0.75).
narrative_ontology:measurement_basis(ecb_climate_127_su_t2027, projected).
narrative_ontology:measurement(ecb_climate_127_su_t2030, ecb_mandate_article_127__climate_incorporation, suppression_requirement, 2030, 0.72).
narrative_ontology:measurement_basis(ecb_climate_127_su_t2030, projected).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ecb_mandate_article_127__climate_incorporation, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(ecb_mandate_article_127__climate_incorporation, 0.12).
narrative_ontology:affects_constraint(ecb_mandate_article_127__climate_incorporation, ecb_mandate_article_127__orthodox_price_stability).
narrative_ontology:affects_constraint(ecb_mandate_article_127__climate_incorporation, ecb_mandate_article_127__expansive_secondary_objectives).
narrative_ontology:affects_constraint(ecb_mandate_article_127__climate_incorporation, eu_green_deal_financing_gap).
narrative_ontology:affects_constraint(ecb_mandate_article_127__climate_incorporation, fossil_fuel_stranded_assets_eu).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the Article 127 TFEU kernel. The orthodox_price_stability reading claims climate integration is unauthorized mandate creep. The expansive_secondary_objectives reading claims climate is permissible discretionary balancing. This climate_incorporation reading claims climate integration is mandatory via Article 11 TFEU. The three readings have distinct beneficiary/victim structures, distinct ε values, and distinct institutional authority foundations. They are NOT the same constraint viewed from different angles; they are genuinely different constraints instantiating different legal interpretations of the same treaty text. All three are linked via network.affects_constraints to enable kernel-level analysis of how interpretive choices reshape constraint structures.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
