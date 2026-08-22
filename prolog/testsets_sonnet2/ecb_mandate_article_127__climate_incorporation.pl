% ============================================================================
% CONSTRAINT STORY: ecb_mandate_article_127__climate_incorporation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
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
 *   human_readable: ECB Climate-Incorporation Reading of the Article 127 Mandate
 *   domain: monetary_policy/constitutional_law/eu_institutional_governance
 *
 * SUMMARY:
 *   Since roughly 2022 the ECB has progressively tilted its corporate bond
 *   purchase programs and adjusted collateral haircut schedules to account
 *   for issuers' climate transition risk, justified as required by the
 *   combination of the Treaty's environmental integration clause (Article 11
 *   TFEU) and the ECB's own reading of its Article 127 mandate's 'without
 *   prejudice' language. Green-aligned issuers and member states see reduced
 *   financing costs; carbon-intensive issuers and member states see increased
 *   costs through the same collateral and purchase machinery — the
 *   coordination function (pricing systemic transition risk before disorderly
 *   repricing) and the extraction function (redistributing financing cost by
 *   sector) run through the identical operational levers.
 *
 * KEY AGENTS:
 *   - ecb_governing_council: agenda_setter (institutional/analytical) — administers collateral and purchase-tilting rules under this reading
 *   - renewable_energy_issuers: beneficiary (moderate/mobile) — lower financing cost from favorable tilt
 *   - fossil_fuel_sector_borrowers: payer (powerful/constrained) — higher haircuts, reduced purchase eligibility
 *   - carbon_intensive_industrial_issuers: payer (moderate/trapped) — least able to substitute financing sources
 *   - eu_climate_policy_coalition: beneficiary (organized/analytical) — gains enforcement lever without legislative fight
 *   - orthodox_price_stability_advocates: excluded (organized/constrained) — outvoted once climate reading operationalized
 *   - court_of_justice_of_the_eu: observer (institutional/analytical) — adjudicates ultra vires challenges
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ecb_mandate_article_127__climate_incorporation, 0.58).
domain_priors:suppression_score(ecb_mandate_article_127__climate_incorporation, 0.61).
domain_priors:theater_ratio(ecb_mandate_article_127__climate_incorporation, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ecb_mandate_article_127__climate_incorporation, extractiveness, 0.58).
narrative_ontology:constraint_metric(ecb_mandate_article_127__climate_incorporation, suppression_requirement, 0.61).
narrative_ontology:constraint_metric(ecb_mandate_article_127__climate_incorporation, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ecb_mandate_article_127__climate_incorporation, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(ecb_mandate_article_127__climate_incorporation, resistance, 0.57).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ecb_mandate_article_127__climate_incorporation, tangled_rope).
narrative_ontology:human_readable(ecb_mandate_article_127__climate_incorporation, "ECB Climate-Incorporation Reading of the Article 127 Mandate").
narrative_ontology:topic_domain(ecb_mandate_article_127__climate_incorporation, "monetary_policy/constitutional_law/eu_institutional_governance").

domain_priors:requires_active_enforcement(ecb_mandate_article_127__climate_incorporation).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ecb_mandate_article_127__climate_incorporation, 'b7340809-b9a1-457a-8159-833a1e97e403').
narrative_ontology:cs_kernel_codification('b7340809-b9a1-457a-8159-833a1e97e403', fixed_text).
narrative_ontology:cs_authority_grounding('b7340809-b9a1-457a-8159-833a1e97e403', extraction).
narrative_ontology:cs_interpretation_layer_present('b7340809-b9a1-457a-8159-833a1e97e403').
narrative_ontology:cs_reading_relation('b7340809-b9a1-457a-8159-833a1e97e403', ecb_mandate_article_127__orthodox_price_stability, forecloses).
narrative_ontology:cs_reading_relation('b7340809-b9a1-457a-8159-833a1e97e403', ecb_mandate_article_127__expansive_secondary_objectives, influences).
narrative_ontology:cs_axiom('b7340809-b9a1-457a-8159-833a1e97e403', foundational, article_11_creates_operative_climate_duty).
narrative_ontology:cs_axiom_status(article_11_creates_operative_climate_duty, holdable).
narrative_ontology:cs_axiom_grounding('b7340809-b9a1-457a-8159-833a1e97e403', article_11_creates_operative_climate_duty, conventional).
narrative_ontology:cs_axiom('b7340809-b9a1-457a-8159-833a1e97e403', secondary, systemic_carbon_risk_requires_operational_pricing).
narrative_ontology:cs_axiom_status(systemic_carbon_risk_requires_operational_pricing, holdable).
narrative_ontology:cs_axiom_grounding('b7340809-b9a1-457a-8159-833a1e97e403', systemic_carbon_risk_requires_operational_pricing, empirically_contingent).
narrative_ontology:cs_reference_frame('b7340809-b9a1-457a-8159-833a1e97e403', independent_price_stability_primacy).
narrative_ontology:cs_drift_state('b7340809-b9a1-457a-8159-833a1e97e403', post_2021_strategy_review, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('b7340809-b9a1-457a-8159-833a1e97e403', '').
narrative_ontology:cs_kernel_id(ecb_mandate_article_127__climate_incorporation, ecb_mandate_article_127).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ecb_mandate_article_127__climate_incorporation, renewable_energy_issuers).
narrative_ontology:constraint_beneficiary(ecb_mandate_article_127__climate_incorporation, green_bond_market_participants).
narrative_ontology:constraint_beneficiary(ecb_mandate_article_127__climate_incorporation, eu_climate_policy_coalition).
narrative_ontology:constraint_beneficiary(ecb_mandate_article_127__climate_incorporation, climate_aligned_member_states).
narrative_ontology:constraint_victim(ecb_mandate_article_127__climate_incorporation, fossil_fuel_sector_borrowers).
narrative_ontology:constraint_victim(ecb_mandate_article_127__climate_incorporation, carbon_intensive_industrial_issuers).
narrative_ontology:constraint_victim(ecb_mandate_article_127__climate_incorporation, high_carbon_member_state_treasuries).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets and administers the collateral haircut schedules, asset purchase tilting rules, and disclosure requirements that operationalize climate risk integration. Justifies this as a reading of Article 127(1)'s 'without prejudice' clause combined with the Article 11 TFEU environmental integration duty binding all Union institutions. Faces no direct exit from its own mandate but bears reputational and legal risk if courts find the reading ultra vires.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__climate_incorporation, ecb_governing_council, agenda_setter,
    institutional, generational, analytical, continental).

% Receive preferential collateral treatment and disproportionate representation in the ECB's corporate bond purchase tilting, lowering their cost of capital relative to carbon-intensive peers. Can also access capital markets outside the Eurosystem if the tilt reverses, but currently benefit from the constraint's operation without bearing its enforcement costs.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__climate_incorporation, renewable_energy_issuers, beneficiary,
    moderate, biographical, mobile, continental).

% Face higher collateral haircuts on pledged assets and reduced eligibility in ECB purchase programs, raising their effective cost of Eurosystem-linked financing. Large incumbents can partially substitute non-euro financing or bank credit, but euro-area headquartered issuers with concentrated financing needs are structurally exposed to the haircut schedule with no equivalent internal appeal mechanism.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__climate_incorporation, fossil_fuel_sector_borrowers, payer,
    powerful, biographical, constrained, continental).

% Mid-sized manufacturers and utilities without the balance sheets to diversify financing sources absorb the haircut penalty directly into borrowing costs. They lack the market access of major fossil fuel majors and cannot easily relocate financing outside the euro system given supply-chain and currency exposure.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__climate_incorporation, carbon_intensive_industrial_issuers, payer,
    moderate, biographical, trapped, national).

% Member states with carbon-intensive industrial bases and fossil-dependent tax revenue see sovereign and quasi-sovereign issuance treated less favorably under climate-tilted collateral frameworks, indirectly raising their financing costs relative to greener peers, while having no formal vote inside the Governing Council's operational rule-making.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__climate_incorporation, high_carbon_member_state_treasuries, payer,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(ecb_mandate_article_127__climate_incorporation, high_carbon_member_state_treasuries, excluded).

% European Commission climate directorates, the European Parliament's environment committee, and allied civil society organizations gain a powerful enforcement lever for EU decarbonization targets that operates through monetary policy rather than fiscal legislation requiring unanimous or qualified-majority political agreement. They did not have to win a legislative fight to get this leverage.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__climate_incorporation, eu_climate_policy_coalition, beneficiary,
    organized, generational, analytical, continental).

% States with green-heavy industrial bases and sovereign green bond programs see their issuance treated favorably, effectively subsidizing their transition financing through the collective monetary institution rather than their own fiscal capacity.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__climate_incorporation, climate_aligned_member_states, beneficiary,
    institutional, generational, analytical, national).

% Central bank economists and member-state central bank governors who read Article 127 as requiring exclusive focus on price stability argue climate tilting introduces discretionary industrial policy into an independent monetary authority, eroding the depoliticization rationale for central bank independence. They participate in Governing Council debate but do not control the operational rule-making once the climate reading prevails institutionally.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__climate_incorporation, orthodox_price_stability_advocates, excluded,
    organized, biographical, constrained, continental).

% Adjudicates ultra vires challenges to ECB operational decisions, weighing whether climate risk integration falls within Article 127(1)'s 'without prejudice to price stability' language and whether Article 11 TFEU creates an operative duty on the ECB or only a horizontal interpretive principle for other Union legislation.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__climate_incorporation, court_of_justice_of_the_eu, observer,
    institutional, generational, analytical, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(ecb_mandate_article_127__climate_incorporation, diffuse).
narrative_ontology:fixing_cost_class(ecb_mandate_article_127__climate_incorporation, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the Eurosystem's balance sheet with the Union's binding decarbonization trajectory, internalizing climate transition risk into asset valuation and collateral eligibility so that systemic financial risk from stranded carbon assets is priced before a disorderly repricing event forces it.
% TRANSFER_FUNCTION: Moves financing-cost advantage from carbon-intensive borrowers to green-aligned borrowers by adjusting collateral haircuts and purchase-program composition, using the Eurosystem's balance sheet as the transfer mechanism rather than fiscal transfer or legislated carbon pricing.
% ABSENT_VOICES: Workers and regions dependent on carbon-intensive industry have no direct seat in Governing Council deliberations and are represented, if at all, only through member-state political channels once financing costs have already shifted. Orthodox price-stability advocates are present in debate but structurally outvoted once the climate reading is operationalized.
% DISAPPEARANCE_RATIONALE: If the climate-incorporation reading were withdrawn, the immediate operational effect would be a reversion to sector-neutral collateral and purchase rules, which fossil-linked issuers would experience as relief and green issuers as a lost advantage. Whether the 'world rearranges' depends on the sibling-reading question of whether Article 11 TFEU imposes a binding operative duty at all — climate advocates say the underlying legal duty persists regardless of ECB practice; orthodox advocates say nothing was ever mandated and the world reverts to the pre-tilt baseline.
% FOUNDING_PROBLEM: Two distinct founding problems are fused in this reading: (1) the original Article 127 mandate was built to solve currency-union credibility and inflation-anchoring after decades of national currency instability; (2) the climate risk integration layer was built to address systemic financial risk from unpriced carbon-transition exposure sitting on Eurosystem collateral and balance sheets, and to discharge the Article 11 TFEU integration duty.
% FOUNDING_PROBLEM_CORROBORATION: The Network for Greening the Financial System (an international central-bank body, not an ECB internal advocate) corroborates that carbon-transition risk is a genuine, underpriced financial stability exposure. Independent legal scholars outside both the ECB and the Commission are split on whether Article 11 TFEU creates an operative duty on an independent monetary authority or merely a horizontal principle binding legislative acts; several German constitutional law scholars and the Bundesbank's own economists — outside the beneficiary coalition — have publicly disputed that the founding problem for THIS reading (a binding TFEU duty on ECB operations) is live rather than constructed.
narrative_ontology:disappearance_verdict(ecb_mandate_article_127__climate_incorporation, contested).
narrative_ontology:founding_problem_status(ecb_mandate_article_127__climate_incorporation, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ecb_mandate_article_127__climate_incorporation, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(ecb_mandate_article_127__climate_incorporation, 'none', 1).
narrative_ontology:epsilon_provenance(ecb_mandate_article_127__climate_incorporation, 0.58, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness rises from 0.31 to 0.58 over the interval as collateral haircut schedules and purchase tilting have hardened from pilot disclosure requirements into binding operational rules — this is genuine rent redistribution by sector, not merely coordination cost, because the same mechanism that prices systemic risk also systematically raises financing costs for one identifiable sector relative to another. Suppression rises in parallel (0.40 to 0.61) as the mechanism shifts from voluntary disclosure encouragement to hard-coded haircut formulas with no internal appeal route for affected issuers — this is the 'novel suppression mechanism via portfolio tilting' named in the structural delta: alternatives are not banned outright, but the Eurosystem's balance sheet is large enough relative to euro-area capital markets that the tilt meaningfully forecloses financing paths for carbon-intensive issuers without formally excluding them. Theater ratio stays comparatively low (0.28) because the collateral and purchase mechanisms are operationally real, not primarily symbolic — the disclosure regime that preceded hard tilting was more theatrical (fits the earlier, lower measurement points) than the current binding haircut schedule.
 *
 * PERSPECTIVAL GAP:
 *   From the ecb_governing_council seat, this reading is coordination: pricing a real, underpriced systemic risk before a disorderly market correction forces it, in discharge of a binding treaty duty. From the fossil_fuel_sector_borrowers and carbon_intensive_industrial_issuers seats, the identical haircut schedule is extraction: a financing-cost penalty imposed by an unelected technocratic body with no legislative mandate specific to their sector, justified by a treaty clause (Article 11) whose operative force on ECB conduct is itself contested. The engine's per-seat computation should register this divergence directly from the declared power/exit/scope data rather than from any adjudication of whose reading of Article 127 is correct.
 *
 * DIRECTIONALITY LOGIC:
 *   Renewable energy issuers and the eu_climate_policy_coalition sit near the beneficiary end of directionality: they collect financing advantage or policy leverage without bearing enforcement cost. Fossil fuel sector borrowers and carbon-intensive industrial issuers sit near the full-target end: the same mechanism that is coordination from the Governing Council's seat is extraction from theirs, and their exit options are constrained-to-trapped depending on balance-sheet size and market access. High-carbon member state treasuries are payers but also excluded from the operational rule-making entirely, despite bearing sovereign financing cost consequences — this is the coordination/extraction seam that makes tangled_rope rather than snare the structurally correct claim: a genuine systemic-risk-pricing function coexists with asymmetric sectoral extraction running through the identical levers.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem for the underlying Article 127 mandate (currency-union credibility) remains unambiguously live. The founding problem specific to THIS reading — that Article 11 TFEU creates a binding operative duty requiring climate integration into ECB operations — is contested outside the benefiding coalition, which is exactly the R5 corroboration finding above. Classifying this as tangled_rope rather than snare or mountain preserves the possibility that the coordination function (systemic risk pricing) is real even if the treaty-duty framing is eventually rejected by the CJEU; classifying it as mountain would falsely naturalize a contested legal interpretation as an inevitable constraint, which is precisely the false-summit risk this reading must avoid by NOT claiming emerges_naturally.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    article_11_operative_duty_ambiguity,
    'Does Article 11 TFEU''s environmental integration clause create a binding operative legal duty on ECB monetary policy operations, or is it a horizontal interpretive principle that binds only Union legislative acts and leaves ECB operational discretion under Article 127(1) unconstrained?',
    'A CJEU ruling on an ultra vires challenge to ECB collateral or purchase-tilting decisions would resolve whether Article 11 has direct operative force on an independent monetary authority''s day-to-day instrument choices, as distinct from binding only legislative rulemaking.',
    'If Article 11 is found non-operative on ECB conduct, this reading''s entire structural claim collapses into policy choice dressed as treaty compliance, converting the constraint''s classification toward snare (extraction without genuine legal coordination mandate) rather than tangled_rope. If found operative, the coordination function is legally anchored and the tangled_rope classification is the more defensible reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(article_11_operative_duty_ambiguity, conceptual, 'Whether Article 11 TFEU binds ECB operational conduct or only Union legislation.').

omega_variable(
    systemic_risk_vs_industrial_policy,
    'Is the climate collateral tilt a genuine systemic financial risk correction (pricing an underpriced exposure) or discretionary industrial policy implemented through monetary instruments because it could not pass through ordinary EU legislative channels?',
    'Compare the magnitude and calibration of haircut adjustments against independent stress-test estimates of actual carbon-transition exposure on Eurosystem collateral; a tilt that tracks measured risk exposure supports the systemic-risk reading, while a tilt that exceeds measured exposure or correlates with political salience supports the industrial-policy reading.',
    'Determines whether the beneficiary/victim asymmetry is a side effect of legitimate risk pricing (tangled_rope, coordination genuinely present) or the primary function with risk-pricing as cover (snare, coordination story only).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(systemic_risk_vs_industrial_policy, empirical, 'Whether the tilt magnitude tracks measured transition risk or exceeds it.').

omega_variable(
    reading_selection_context,
    'Given that all three sibling readings (climate_incorporation, expansive_secondary_objectives, orthodox_price_stability) are textually available within Article 127''s ambiguous ''without prejudice'' clause, what governed the ECB''s selection of the climate-incorporation reading over the other two at this historical moment?',
    'Institutional history of Governing Council deliberations, personnel composition, and external political pressure (Commission Green Deal timeline, COP commitments) surrounding the 2021-2022 strategy review would show whether the reading was adopted for principled legal reasons or as the operationally available path to a politically desired outcome.',
    'If the reading was selected primarily for political convenience rather than legal necessity, it strengthens the case that this is a constructed reading serving identifiable beneficiaries (FSM-adjacent concern) rather than a discovered treaty obligation — though this constraint is not authored as a mountain, so FSM does not directly apply; the concern instead sharpens the coordination/extraction seam analysis.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_selection_context, conceptual, 'What drove selection of this reading among the three textually available options.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ecb_mandate_article_127__climate_incorporation, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ecb__tr_t0, ecb_mandate_article_127__climate_incorporation, theater_ratio, 0, 0.14).
narrative_ontology:measurement(ecb__tr_t4, ecb_mandate_article_127__climate_incorporation, theater_ratio, 4, 0.17).
narrative_ontology:measurement(ecb__tr_t8, ecb_mandate_article_127__climate_incorporation, theater_ratio, 8, 0.2).
narrative_ontology:measurement(ecb__tr_t12, ecb_mandate_article_127__climate_incorporation, theater_ratio, 12, 0.22).
narrative_ontology:measurement(ecb__tr_t16, ecb_mandate_article_127__climate_incorporation, theater_ratio, 16, 0.25).
narrative_ontology:measurement(ecb__tr_t20, ecb_mandate_article_127__climate_incorporation, theater_ratio, 20, 0.27).
narrative_ontology:measurement(ecb__tr_t24, ecb_mandate_article_127__climate_incorporation, theater_ratio, 24, 0.28).

% Extraction over time
narrative_ontology:measurement(ecb__be_t0, ecb_mandate_article_127__climate_incorporation, base_extractiveness, 0, 0.31).
narrative_ontology:measurement(ecb__be_t4, ecb_mandate_article_127__climate_incorporation, base_extractiveness, 4, 0.38).
narrative_ontology:measurement(ecb__be_t8, ecb_mandate_article_127__climate_incorporation, base_extractiveness, 8, 0.44).
narrative_ontology:measurement(ecb__be_t12, ecb_mandate_article_127__climate_incorporation, base_extractiveness, 12, 0.49).
narrative_ontology:measurement(ecb__be_t16, ecb_mandate_article_127__climate_incorporation, base_extractiveness, 16, 0.53).
narrative_ontology:measurement(ecb__be_t20, ecb_mandate_article_127__climate_incorporation, base_extractiveness, 20, 0.56).
narrative_ontology:measurement(ecb__be_t24, ecb_mandate_article_127__climate_incorporation, base_extractiveness, 24, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(ecb__su_t0, ecb_mandate_article_127__climate_incorporation, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(ecb__su_t4, ecb_mandate_article_127__climate_incorporation, suppression_requirement, 4, 0.46).
narrative_ontology:measurement(ecb__su_t8, ecb_mandate_article_127__climate_incorporation, suppression_requirement, 8, 0.51).
narrative_ontology:measurement(ecb__su_t12, ecb_mandate_article_127__climate_incorporation, suppression_requirement, 12, 0.55).
narrative_ontology:measurement(ecb__su_t16, ecb_mandate_article_127__climate_incorporation, suppression_requirement, 16, 0.58).
narrative_ontology:measurement(ecb__su_t20, ecb_mandate_article_127__climate_incorporation, suppression_requirement, 20, 0.6).
narrative_ontology:measurement(ecb__su_t24, ecb_mandate_article_127__climate_incorporation, suppression_requirement, 24, 0.61).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ecb_mandate_article_127__climate_incorporation, enforcement_mechanism).
narrative_ontology:affects_constraint(ecb_mandate_article_127__climate_incorporation, orthodox_price_stability).
narrative_ontology:affects_constraint(ecb_mandate_article_127__climate_incorporation, expansive_secondary_objectives).

% DUAL FORMULATION NOTE:
% This story is one of three constraints reading the same ecb_mandate_article_127 kernel. orthodox_price_stability claims Article 127 requires exclusive inflation focus with secondary objectives non-operational (near-mountain framing of central bank independence, minimal beneficiary/victim structure). expansive_secondary_objectives claims discretionary balancing of employment/growth when price stability is unthreatened, without the specific mandatory Article 11 climate claim (rope-leaning, broader discretionary coordination). This reading (climate_incorporation) is the most structurally extractive of the three because it names concrete sectoral beneficiaries and victims through a specific, currently-contested legal mechanism (Article 11 TFEU operative duty) rather than general policy discretion. The three do not share an ε value — each is a distinct constraint with its own metrics, linked here for contamination-propagation and family-tracing purposes only.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
