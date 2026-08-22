% ============================================================================
% CONSTRAINT STORY: ecb_mandate_article_127__climate_incorporation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: ecb_mandate_article_127__climate_incorporation
 *   human_readable: ECB Mandate Article 127 TFEU Climate Risk Integration
 *   domain: monetary_policy/constitutional_law/environmental
 *
 * SUMMARY:
 *   The ECB's mandate under Article 127 TFEU contains two operational
 *   objectives: price stability (primary) and support for EU policies,
 *   including environmental policy via Article 11 TFEU integration clause.
 *   This constraint story instantiates the climate-incorporation reading: the
 *   mandate requires the ECB to treat climate transition risk as material to
 *   financial stability and collateral assessment, aligning monetary
 *   operations with EU climate commitments. The climate-incorporation reading
 *   contends that Article 11 TFEU environmental integration is not a
 *   secondary ornament but an active operational requirement. This stands in
 *   tension with the orthodox reading (price-stability exclusivity) and the
 *   expansive reading (broader discretionary balancing). The claim/metric gap
 *   is intentional: the constraint is CLAIMED as tangled_rope (genuine
 *   coordination on climate risk + asymmetric extraction from
 *   fossil-intensive holders), while the authored metrics show substantial
 *   extraction (0.62) and moderate suppression (0.58) through portfolio
 *   tilting mechanisms. This reading does not adjudicate the kernel contest;
 *   it models one coherent interpretation of the mandate that the engine
 *   measures alongside the others.
 *
 * KEY AGENTS:
 *   - ECB Governing Council — institutional agenda-setter; interprets and implements climate incorporation; controls haircut and counterparty assessment rules
 *   - Fossil fuel companies — powerful but constrained payers; bear elevated collateral haircuts and funding costs; cannot exit eurozone operations without capital loss
 *   - Climate transition finance sector — organized beneficiaries; gain preferential collateral treatment and market signaling of legitimacy
 *   - Commercial banks — institutionally dual-positioned; incur compliance costs but diverge by asset composition (green lenders benefit, carbon-heavy lenders face pressure)
 *   - EU member states — institutional beneficiaries; gain operational support for climate finance and Article 11 TFEU environmental integration
 *   - Orthodox ECB economists and German Bundesbank — excluded dissenters; minoritized on governing council; argue mandate subordination
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ecb_mandate_article_127__climate_incorporation, 0.62).
domain_priors:suppression_score(ecb_mandate_article_127__climate_incorporation, 0.58).
domain_priors:theater_ratio(ecb_mandate_article_127__climate_incorporation, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ecb_mandate_article_127__climate_incorporation, extractiveness, 0.62).
narrative_ontology:constraint_metric(ecb_mandate_article_127__climate_incorporation, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(ecb_mandate_article_127__climate_incorporation, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ecb_mandate_article_127__climate_incorporation, accessibility_collapse, 0.51).
narrative_ontology:constraint_metric(ecb_mandate_article_127__climate_incorporation, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ecb_mandate_article_127__climate_incorporation, tangled_rope).
narrative_ontology:human_readable(ecb_mandate_article_127__climate_incorporation, "ECB Mandate Article 127 TFEU Climate Risk Integration").
narrative_ontology:topic_domain(ecb_mandate_article_127__climate_incorporation, "monetary_policy/constitutional_law/environmental").

domain_priors:requires_active_enforcement(ecb_mandate_article_127__climate_incorporation).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ecb_mandate_article_127__climate_incorporation, '8a1a230c-9b1f-4194-a9a1-d6a8afff40bd').
narrative_ontology:cs_kernel_codification('8a1a230c-9b1f-4194-a9a1-d6a8afff40bd', fixed_text).
narrative_ontology:cs_authority_grounding('8a1a230c-9b1f-4194-a9a1-d6a8afff40bd', extraction).
narrative_ontology:cs_interpretation_layer_present('8a1a230c-9b1f-4194-a9a1-d6a8afff40bd').
narrative_ontology:cs_reading_relation('8a1a230c-9b1f-4194-a9a1-d6a8afff40bd', ecb_mandate_article_127__orthodox_price_stability, influences).
narrative_ontology:cs_reading_relation('8a1a230c-9b1f-4194-a9a1-d6a8afff40bd', ecb_mandate_article_127__expansive_secondary_objectives, coexists_with).
narrative_ontology:cs_axiom('8a1a230c-9b1f-4194-a9a1-d6a8afff40bd', foundational, climate_risk_materiality_to_financial_stability).
narrative_ontology:cs_axiom_status(climate_risk_materiality_to_financial_stability, holdable).
narrative_ontology:cs_axiom_grounding('8a1a230c-9b1f-4194-a9a1-d6a8afff40bd', climate_risk_materiality_to_financial_stability, empirically_contingent).
narrative_ontology:cs_axiom('8a1a230c-9b1f-4194-a9a1-d6a8afff40bd', foundational, article_11_tfeu_environmental_integration_operationality).
narrative_ontology:cs_axiom_status(article_11_tfeu_environmental_integration_operationality, holdable).
narrative_ontology:cs_axiom_grounding('8a1a230c-9b1f-4194-a9a1-d6a8afff40bd', article_11_tfeu_environmental_integration_operationality, deontological).
narrative_ontology:cs_reference_frame('8a1a230c-9b1f-4194-a9a1-d6a8afff40bd', price_stability_subordinate_environmental_alignment).
narrative_ontology:cs_drift_state('8a1a230c-9b1f-4194-a9a1-d6a8afff40bd', contemporary_climate_crisis_acceleration, gap(axiom_overriding, substantial, true)).
narrative_ontology:cs_created_at('8a1a230c-9b1f-4194-a9a1-d6a8afff40bd', '').
narrative_ontology:cs_kernel_id(ecb_mandate_article_127__climate_incorporation, ecb_mandate_article_127).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ecb_mandate_article_127__climate_incorporation, climate_transition_finance_sector).
narrative_ontology:constraint_beneficiary(ecb_mandate_article_127__climate_incorporation, green_technology_firms).
narrative_ontology:constraint_beneficiary(ecb_mandate_article_127__climate_incorporation, eu_member_states_climate_commitments).
narrative_ontology:constraint_victim(ecb_mandate_article_127__climate_incorporation, fossil_fuel_companies).
narrative_ontology:constraint_victim(ecb_mandate_article_127__climate_incorporation, carbon_intensive_collateral_holders).
narrative_ontology:constraint_victim(ecb_mandate_article_127__climate_incorporation, emerging_market_commodity_exporters).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(ecb_mandate_article_127__climate_incorporation, commercial_banks).
narrative_ontology:constraint_victim(ecb_mandate_article_127__climate_incorporation, commercial_banks).
narrative_ontology:constraint_victim(ecb_mandate_article_127__climate_incorporation, german_bundesbank).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interprets and operationalizes the dual mandate: price stability and support for EU environmental policy via Article 11 TFEU integration. Sets haircuts on fossil-fuel-intensive collateral, adjusts portfolio allocation criteria, and implements climate risk stress tests on counterparties. Justified as risk management and treaty compliance; contested by orthodox factions as mandate overreach.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__climate_incorporation, ecb_governing_council, agenda_setter,
    institutional, generational, analytical, continental).

% Face elevated collateral haircuts, reduced repo eligibility, and higher funding costs when pledging carbon-intensive assets to ECB operations. Their securities that were AAA-equivalent for collateral purposes become subject to climate transition risk penalties. Exit via relocation outside eurozone is costly and capital-intensive; refinancing alternatives are constrained by ECB's market influence.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__climate_incorporation, fossil_fuel_companies, payer,
    powerful, biographical, trapped, continental).

% Green bond issuers, sustainable finance fund managers, and climate tech companies gain preferential collateral treatment, lower haircuts on transition-aligned assets, and market confidence signals from ECB's framework. Their securities become more attractive to ECB counterparties. The constraint's operation vindicates climate finance as systemic and legitimate.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__climate_incorporation, climate_transition_finance_sector, beneficiary,
    organized, generational, mobile, continental).

% Incur compliance costs integrating climate risk into internal collateral frameworks and stress-testing procedures. They also benefit: those with green lending portfolios gain competitive advantage in repo access; those concentrated in fossil fuels face funding pressure that redirects lending away from carbon-intensive sectors. Their net position diverges by asset composition.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__climate_incorporation, commercial_banks, payer,
    institutional, biographical, constrained, continental).
narrative_ontology:stakeholder_secondary_role(ecb_mandate_article_127__climate_incorporation, commercial_banks, beneficiary).

% Member states pursuing climate targets under EU Climate Law and Article 11 TFEU gain operational support from ECB; their green transition financing becomes cheaper and easier as the central bank signals climate risk as material. The constraint operationalizes the treaty-mandated environmental integration across monetary policy.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__climate_incorporation, eu_member_states_climate_commitments, beneficiary,
    institutional, generational, mobile, continental).

% Face reduced willingness of eurozone banks to hold their commodity-linked debt and collateral as ECB climate frameworks expand; the carbon footprint of their export economies becomes a risk factor in ECB counterparty assessments. Their borrowing costs rise as eurozone financing becomes less accessible; their exit is to non-eurozone funding sources.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__climate_incorporation, emerging_market_commodity_exporters, payer,
    powerful, biographical, mobile, global).

% Argue that climate risk integration exceeds the ECB's price-stability mandate; they are not at the table when portfolio and collateral criteria are set but have published formal dissents and minority opinions. Their exclusion is structural: they lack voting power on the governing council and their framing (mandate subordination) is not recognized as legitimate input to the operational framework.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__climate_incorporation, orthodox_ecb_economists, excluded,
    organized, biographical, constrained, continental).

% Exercises democratic accountability through monetary dialogue; questions ECB on the constitutional basis for climate incorporation and whether Article 127 truly permits dual operational weight. Cannot directly override ECB decisions but can condition political support for the institution on mandate clarity and transparency.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__climate_incorporation, european_parliament, observer,
    institutional, generational, constrained, continental).

% Has dissented from climate incorporation on grounds that it exceeds price-stability mandate; as a governing council member on a single vote among 25, its position is minoritized. Cannot exit without exiting the eurozone governance structure itself. Faces reputational and institutional pressure to align with ECB decisions once adopted.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__climate_incorporation, german_bundesbank, excluded,
    institutional, generational, trapped, continental).
narrative_ontology:stakeholder_secondary_role(ecb_mandate_article_127__climate_incorporation, german_bundesbank, payer).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(ecb_mandate_article_127__climate_incorporation, climate_transition_finance_sector).
narrative_ontology:fixing_cost_class(ecb_mandate_article_127__climate_incorporation, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Integrates climate transition risk into eurozone monetary operations and collateral frameworks: solves for the reality that fossil-fuel-intensive assets carry tail risks (stranded asset risk, carbon pricing exposure) that affect repayment probability and collateral stability. Aligns ECB risk management with EU climate commitments under Article 11 TFEU, closing the gap between financial stability mandate and environmental policy objectives.
% TRANSFER_FUNCTION: Moves funding cost burdens away from transition-aligned firms and toward carbon-intensive firms by differentiating collateral haircuts and counterparty assessment; transfers legitimacy and market confidence to climate finance sector by treating climate risk as material and systemic; transfers political support from orthodox monetary economists to climate-centered policy constituencies.
% ABSENT_VOICES: Fossil fuel companies subject to haircuts cannot vote on ECB decisions; emerging market commodity exporters have no seat at the governing council; German Bundesbank and other orthodox dissenters are minoritized and functionally excluded from shaping the framework once majority votes. These parties argue for price-stability-only mandate but are not heard at the point of rule-setting.
% DISAPPEARANCE_RATIONALE: If climate incorporation vanished overnight, collateral haircuts would normalize, fossil-fuel-linked securities would regain repo eligibility, green bond premiums would compress, and eurozone refinancing would reallocate back toward carbon-intensive firms. The EU's climate finance strategy would lose the ECB's operational support, forcing member states to finance green transitions through fiscal policy alone at higher cost. Eurozone monetary operations would decouple from EU environmental law, creating institutional contradiction.
% FOUNDING_PROBLEM: Eurozone monetary framework operated without accounting for climate transition risk; securities pledged as collateral treated carbon intensity as irrelevant to repayment probability; ECB asset purchases showed no alignment with EU climate commitments under Article 11 TFEU, creating policy coordination failure and tail risk exposure.
% FOUNDING_PROBLEM_CORROBORATION: ECB climate risk assessment reports and European Commission environmental policy documents attest the problem was real: financial stability implications of climate transition and ECB's treaty obligation under Article 11 to support EU environmental policy. Orthodox economists and German Bundesbank attest the problem is either non-existent (climate risk is immaterial to repayment) or orthogonal to monetary mandate. Independent financial stability analysts and climate economists outside ECB attest material risk; central bank regulatory capture concerns and mandate-scope literature support the contested status.
narrative_ontology:disappearance_verdict(ecb_mandate_article_127__climate_incorporation, world_rearranges).
narrative_ontology:founding_problem_status(ecb_mandate_article_127__climate_incorporation, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ecb_mandate_article_127__climate_incorporation, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(ecb_mandate_article_127__climate_incorporation, 'none', 1).
narrative_ontology:epsilon_provenance(ecb_mandate_article_127__climate_incorporation, 0.62, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness reaches 0.62 by interval end because the constraint transfers funding costs across sectors based on carbon intensity, not on marginal credit risk alone. The transfer is extractive (not pure coordination cost) because the climate haircuts are calibrated to redirect capital, not to measure default probability accurately—the fossil fuel sector bears costs disproportionate to actual heightened risk, while transition-aligned sectors receive benefits disproportionate to risk reduction. Suppression is moderate-high (0.58) because the constraint operates through portfolio tilting and counterparty pressure, not through explicit coercion, yet orthodoxly-minded actors cannot voice dissent at the point of decision. Theater is moderate (0.42): climate risk assessment is real, but a growing share of the framework's operational logic defends preferential treatment for transition finance, which is performance of the commitment rather than direct risk mitigation. The temporal trajectory shows extraction and suppression rising steeply in years 0-15 (implementation phase: collateral frameworks tightened, haircuts differentiated, compliance costs imposed) then plateauing by year 20 (institutional adaptation complete, resistance stable). The plateau indicates the constraint has locked in and the marginal suppression needed to maintain it stabilizes—this is the signature of a tangled rope that has solidified.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter (ECB Governing Council majority) perceives the constraint as genuine coordination: climate risk is material, collateral stability improves, and the constraint operationalizes treaty obligations. Fossil fuel payers perceive extraction: haircuts are disconnected from marginal credit risk, and the constraint redirects capital on political grounds. Transition finance beneficiaries perceive subsidized coordination: the constraint legitimizes their sector and reduces their funding costs. The engine should compute these divergences: the ECB seat computes toward tangled_rope (coordination + enforcement), the payer seats compute toward snare (pure extraction + suppression), the beneficiary seats compute toward rope (coordination without extraction against them). The perspectival gap is structurally real and central to why the constraint is contested.
 *
 * DIRECTIONALITY LOGIC:
 *   The beneficiary/victim declarations map as follows: beneficiaries (climate_transition_finance_sector, eu_member_states_climate_commitments) sit at low-to-moderate d because they gain without bearing collateral costs; victims (fossil_fuel_companies, carbon_intensive_collateral_holders, emerging_market_commodity_exporters) sit at high d because they bear explicit haircuts and constricted funding. The constraint's enforcement (portfolio tilting, counterparty pressure) is not external military force but institutional leverage through central banking operations—the suppression is structural, not coercive violence, but it operates on trapped actors who cannot exit without massive cost. This is the signature of institutional extraction through leverage on economic dependency.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (climate transition risk was not accounted for in eurozone monetary operations) appears live (climate risk is measurable and persistent), but the constraint's solution conflates two distinct problems: (1) Does the ECB's mandate permit climate risk accounting? (2) Should the ECB preferentially favor transition-aligned finance over fossil finance? The first problem is technical-legal (solved by Article 11 TFEU environmental integration); the second is political-distributional (solved by portfolio tilting, which redistributes capital). If the founding problem is merely (1), then the constraint has partly solved it, and the residual extraction (0.62) is political design, not response to the original problem. If the founding problem is also (2), then the constraint solves the distributional problem by design—making extraction the feature, not a side effect. The constraint is NOT mandatrophy-resolved because the distributional problem is live and contested, and the orthodox reading denies the problem exists at all. Classification as tangled_rope (not snare, not rope) depends on accepting that both the coordination and extraction components are structurally real—neither is cover for the other. The measurement trajectory supports this: extraction and suppression rise together, suggesting they are coupled features of the implementation, not divergent paths.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    climate_risk_materiality_dispute,
    'Is climate transition risk material to credit and collateral risk assessments, or is it a long-term externality orthogonal to repayment probability?',
    'Empirical: multivariate credit default analysis controlling for climate transition exposure; stress-test models that isolate climate risk''s contribution to default correlation.',
    'If climate risk is material, the constraint is genuine risk management and coordination; if immaterial, the constraint is political redistribution disguised as risk assessment. Classification would shift from tangled_rope to snare if climate risk is shown immaterial.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(climate_risk_materiality_dispute, empirical, 'Materiality of climate transition risk to collateral stability.').

omega_variable(
    mandate_scope_kernel_ambiguity,
    'Does Article 127 TFEU permit operational weight on secondary objectives (including environmental policy via Article 11), or does it require exclusive focus on price stability?',
    'Textual: Treaty interpretation via established canons and ECJ precedent. Institutional: European Parliament clarification via democratic dialogue or formalized mandate review.',
    'If Article 127 permits climate incorporation as an operational secondary objective, the constraint is structural coordination meeting a treaty requirement. If it requires price-stability exclusivity, the constraint is mandate overreach and should be classified as snare (pure redistribution under false cover of coordination). Classification depends entirely on which reading of the kernel is correct.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(mandate_scope_kernel_ambiguity, conceptual, 'Kernel ambiguity: does ECB mandate permit operational climate incorporation or require price-stability exclusivity?').

omega_variable(
    suppression_internalization_trajectory,
    'Is the measured suppression (0.58) structural (collateral haircuts, counterparty pressure) or internalized (fossil fuel companies treat climate risk as legitimate, accept reduced access as fair)?',
    'Post-exit analysis: firms that relocate outside eurozone; measurement of continued market access constraints post-relocation. Behavioral study: whether firms'' subjective assessment of constraint fairness changes after rule implementation.',
    'If primarily structural, the suppression dissolves with exit and the constraint''s hold on extrazone actors weakens. If primarily internalized, suppression persists across markets and the constraint''s extraction extends beyond direct ECB reach. Affects classification of piton risk: if suppression persists post-exit, the constraint may maintain extraction longer than the direct coercive machinery.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_internalization_trajectory, empirical, 'Whether suppression is structural-institutional or internalized-normative.').

omega_variable(
    orthodox_dissent_minoritization,
    'Is the minoritization of orthodox monetary economists (trapped votes, no council representation) structural suppression of an alternative reading, or legitimate democratic disagreement within a collective decision procedure?',
    'Procedural: analysis of voting records, dissent statements, and institutional mechanisms for voice. Comparative: study of similar institutional minorities in other central banks and international bodies.',
    'If suppression, the constraint carries a meta-level extraction cost: dissenting expertise is institutionally silenced, which amplifies the risk of mandate overreach. If legitimate, the minority dissent is a normal part of governance and the constraint''s democratic legitimacy is intact. Affects the reading''s relation to orthodox_price_stability: is coexistence possible (both readings live at different seats), or does minoritization indicate foreclosure (the orthodox reading is being suppressed)?',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(orthodox_dissent_minoritization, conceptual, 'Whether minoritized orthodoxy represents structural suppression or legitimate institutional disagreement.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ecb_mandate_article_127__climate_incorporation, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ecb__tr_t0, ecb_mandate_article_127__climate_incorporation, theater_ratio, 0, 0.2).
narrative_ontology:measurement(ecb__tr_t5, ecb_mandate_article_127__climate_incorporation, theater_ratio, 5, 0.28).
narrative_ontology:measurement(ecb__tr_t10, ecb_mandate_article_127__climate_incorporation, theater_ratio, 10, 0.35).
narrative_ontology:measurement(ecb__tr_t15, ecb_mandate_article_127__climate_incorporation, theater_ratio, 15, 0.4).
narrative_ontology:measurement(ecb__tr_t20, ecb_mandate_article_127__climate_incorporation, theater_ratio, 20, 0.42).
narrative_ontology:measurement(ecb__tr_t25, ecb_mandate_article_127__climate_incorporation, theater_ratio, 25, 0.42).

% Extraction over time
narrative_ontology:measurement(ecb__be_t0, ecb_mandate_article_127__climate_incorporation, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(ecb__be_t5, ecb_mandate_article_127__climate_incorporation, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(ecb__be_t10, ecb_mandate_article_127__climate_incorporation, base_extractiveness, 10, 0.55).
narrative_ontology:measurement(ecb__be_t15, ecb_mandate_article_127__climate_incorporation, base_extractiveness, 15, 0.6).
narrative_ontology:measurement(ecb__be_t20, ecb_mandate_article_127__climate_incorporation, base_extractiveness, 20, 0.62).
narrative_ontology:measurement(ecb__be_t25, ecb_mandate_article_127__climate_incorporation, base_extractiveness, 25, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(ecb__su_t0, ecb_mandate_article_127__climate_incorporation, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(ecb__su_t5, ecb_mandate_article_127__climate_incorporation, suppression_requirement, 5, 0.42).
narrative_ontology:measurement(ecb__su_t10, ecb_mandate_article_127__climate_incorporation, suppression_requirement, 10, 0.5).
narrative_ontology:measurement(ecb__su_t15, ecb_mandate_article_127__climate_incorporation, suppression_requirement, 15, 0.56).
narrative_ontology:measurement(ecb__su_t20, ecb_mandate_article_127__climate_incorporation, suppression_requirement, 20, 0.58).
narrative_ontology:measurement(ecb__su_t25, ecb_mandate_article_127__climate_incorporation, suppression_requirement, 25, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ecb_mandate_article_127__climate_incorporation, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(ecb_mandate_article_127__climate_incorporation, 0.18).
narrative_ontology:affects_constraint(ecb_mandate_article_127__climate_incorporation, ecb_mandate_article_127__orthodox_price_stability).
narrative_ontology:affects_constraint(ecb_mandate_article_127__climate_incorporation, ecb_mandate_article_127__expansive_secondary_objectives).
narrative_ontology:affects_constraint(ecb_mandate_article_127__climate_incorporation, eu_climate_taxonomy_regulation).
narrative_ontology:affects_constraint(ecb_mandate_article_127__climate_incorporation, basel_iii_climate_risk_framework).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the contested ECB Article 127 TFEU kernel. The climate_incorporation reading interprets Article 11 TFEU environmental integration as an operational requirement, not discretionary. Sibling readings (orthodox_price_stability, expansive_secondary_objectives) interpret the same Treaty text with different emphases. All three readings share the same referent (Article 127 TFEU mandate scope) but author different ε values because they instantiate different constraint structures. Network: climate_incorporation influences orthodox_price_stability (by establishing climate risk as a legitimate operational category, it pressures orthodoxy to either absorb climate concerns or explicitly reject them); coexists_with expansive_secondary_objectives (both readings permit secondary-objective weight, differing on whether climate incorporation is required or merely permitted). The three stories together model the kernel's contested structure; each story is independently ε-invariant.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(ecb_mandate_article_127__climate_incorporation, organized, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
