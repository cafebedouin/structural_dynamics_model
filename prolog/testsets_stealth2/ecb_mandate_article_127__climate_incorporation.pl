% ============================================================================
% CONSTRAINT STORY: ecb_mandate_article_127__climate_incorporation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-12
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:measurement_basis/2,
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
 *   human_readable: ECB Climate Incorporation Regime: Purchase Tilts and Collateral Haircuts under the Article 11 TFEU Integration Duty
 *   domain: economic/constitutional/institutional
 *
 * SUMMARY:
 *   The arrangement under contest is the Eurosystem's operational integration
 *   of climate considerations into monetary policy: corporate bond purchases
 *   concentrated toward lower-emission issuers along disclosed
 *   decarbonization paths, collateral eligibility and haircut schedules
 *   differentiated by climate performance, climate-related disclosure
 *   requirements for counterparties, and the legal ground offered by the
 *   treaty duty to integrate environmental protection into the implementation
 *   of all Union policies. The arrangement phases in from 2021 onward and is
 *   administered entirely by the Governing Council's own decision procedures.
 *   Its gains concentrate on climate-aligned issuers and the green finance
 *   complex; its costs concentrate on fossil fuel producers and
 *   carbon-intensive manufacturers, who face penalty haircuts and purchase
 *   exclusion through the banking channel rather than any prohibition on
 *   transacting. The claim/metric gap is deliberate: the constraint is
 *   CLAIMED as tangled_rope (genuine risk-repricing and treaty-compliance
 *   function carrying a concentrated sectoral cost) while the authored
 *   metrics describe moderately extractive, actively enforced operation — the
 *   engine measures the divergence; nothing here reconciles claim to metrics.
 *
 * KEY AGENTS:
 *   - ecb_governing_council: Agenda setter (institutional/arbitrage) — administers collateral schedules and purchase tilts by internal decision
 *   - fossil_fuel_producers: Primary target (organized/trapped) — bears penalty haircuts and purchase exclusion; cannot reposition without dissolving the business
 *   - carbon_intensive_manufacturers: Secondary target (organized/constrained) — down-weighted paper, slow capital-cycle adjustment
 *   - climate_aligned_issuers: Primary beneficiary (organized/mobile) — receives concentrated purchase demand and favorable collateral terms
 *   - green_finance_industry: Beneficiary (institutional/mobile) — collects fees on the legitimized green complex
 *   - european_commission: Institutional beneficiary (institutional/constrained) — gains monetary-side reinforcement of its climate agenda
 *   - commercial_banks: Dual-positioned intermediary (institutional/mobile) — gains on green collateral, loses on carbon-heavy client books
 *   - euro_area_households: Diffuse beneficiary-payer (powerless/constrained) — long-horizon climate and price-stability returns, marginal transmission costs, no direct voice
 *   - monetary_orthodoxy_advocates: Excluded objector (organized/constrained) — presses objections through litigation and member-state politics only
 *   - european_court_of_justice: Analytical observer (institutional/analytical) — fixes the legal boundary the schedules operate within
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ecb_mandate_article_127__climate_incorporation, 0.48).
domain_priors:suppression_score(ecb_mandate_article_127__climate_incorporation, 0.56).
domain_priors:theater_ratio(ecb_mandate_article_127__climate_incorporation, 0.33).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ecb_mandate_article_127__climate_incorporation, extractiveness, 0.48).
narrative_ontology:constraint_metric(ecb_mandate_article_127__climate_incorporation, suppression_requirement, 0.56).
narrative_ontology:constraint_metric(ecb_mandate_article_127__climate_incorporation, theater_ratio, 0.33).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ecb_mandate_article_127__climate_incorporation, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(ecb_mandate_article_127__climate_incorporation, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ecb_mandate_article_127__climate_incorporation, tangled_rope).
narrative_ontology:human_readable(ecb_mandate_article_127__climate_incorporation, "ECB Climate Incorporation Regime: Purchase Tilts and Collateral Haircuts under the Article 11 TFEU Integration Duty").
narrative_ontology:topic_domain(ecb_mandate_article_127__climate_incorporation, "economic/constitutional/institutional").

domain_priors:requires_active_enforcement(ecb_mandate_article_127__climate_incorporation).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ecb_mandate_article_127__climate_incorporation, 'c51f6135-ece6-4cbb-8051-6f77fcbea966').
narrative_ontology:cs_kernel_codification('c51f6135-ece6-4cbb-8051-6f77fcbea966', fixed_text).
narrative_ontology:cs_authority_grounding('c51f6135-ece6-4cbb-8051-6f77fcbea966', lineage).
narrative_ontology:cs_interpretation_layer_present('c51f6135-ece6-4cbb-8051-6f77fcbea966').
narrative_ontology:cs_reading_relation('c51f6135-ece6-4cbb-8051-6f77fcbea966', ecb_mandate_article_127__orthodox_price_stability, forecloses).
narrative_ontology:cs_reading_relation('c51f6135-ece6-4cbb-8051-6f77fcbea966', ecb_mandate_article_127__expansive_secondary_objectives, coexists_with).
narrative_ontology:cs_axiom('c51f6135-ece6-4cbb-8051-6f77fcbea966', foundational, environmental_integration_binding_on_all_union_institutions).
narrative_ontology:cs_axiom_status(environmental_integration_binding_on_all_union_institutions, holdable).
narrative_ontology:cs_axiom_grounding('c51f6135-ece6-4cbb-8051-6f77fcbea966', environmental_integration_binding_on_all_union_institutions, conventional).
narrative_ontology:cs_axiom('c51f6135-ece6-4cbb-8051-6f77fcbea966', foundational, climate_transition_risk_is_monetary_policy_relevant_risk).
narrative_ontology:cs_axiom_status(climate_transition_risk_is_monetary_policy_relevant_risk, holdable).
narrative_ontology:cs_axiom_grounding('c51f6135-ece6-4cbb-8051-6f77fcbea966', climate_transition_risk_is_monetary_policy_relevant_risk, empirically_contingent).
narrative_ontology:cs_axiom('c51f6135-ece6-4cbb-8051-6f77fcbea966', secondary, market_neutrality_not_treaty_required).
narrative_ontology:cs_axiom_status(market_neutrality_not_treaty_required, holdable).
narrative_ontology:cs_axiom_grounding('c51f6135-ece6-4cbb-8051-6f77fcbea966', market_neutrality_not_treaty_required, conventional).
narrative_ontology:cs_reference_frame('c51f6135-ece6-4cbb-8051-6f77fcbea966', treaty_environmental_integration_operationalized).
narrative_ontology:cs_drift_state('c51f6135-ece6-4cbb-8051-6f77fcbea966', contemporary_post_apex_court_review, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('c51f6135-ece6-4cbb-8051-6f77fcbea966', '').
narrative_ontology:cs_kernel_id(ecb_mandate_article_127__climate_incorporation, ecb_mandate_article_127).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ecb_mandate_article_127__climate_incorporation, climate_aligned_issuers).
narrative_ontology:constraint_beneficiary(ecb_mandate_article_127__climate_incorporation, green_finance_industry).
narrative_ontology:constraint_beneficiary(ecb_mandate_article_127__climate_incorporation, european_commission).
narrative_ontology:constraint_victim(ecb_mandate_article_127__climate_incorporation, fossil_fuel_producers).
narrative_ontology:constraint_victim(ecb_mandate_article_127__climate_incorporation, carbon_intensive_manufacturers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(ecb_mandate_article_127__climate_incorporation, commercial_banks).
narrative_ontology:constraint_beneficiary(ecb_mandate_article_127__climate_incorporation, euro_area_households).
narrative_ontology:constraint_victim(ecb_mandate_article_127__climate_incorporation, commercial_banks).
narrative_ontology:constraint_victim(ecb_mandate_article_127__climate_incorporation, euro_area_households).
narrative_ontology:constraint_vindicates(ecb_mandate_article_127__climate_incorporation, article_11_tfeu_environmental_integration_clause).
narrative_ontology:constraint_vindicates(ecb_mandate_article_127__climate_incorporation, climate_financial_risk_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets the Eurosystem's collateral eligibility rules and haircut schedules, designs the corporate bond purchase tilts, and publishes the climate action plan that sequences them. Recalibrating or unwinding any element requires only an internal Governing Council decision, exercised within boundaries fixed by the treaties and reviewed by the courts. Collects no fee; its return is balance-sheet risk reduction and coherence with the wider Union framework it is bound to support.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__climate_incorporation, ecb_governing_council, agenda_setter,
    institutional, generational, arbitrage, continental).

% Oil and gas producers, refiners, and coal-fired generators whose bonds are excluded from purchase programs and carry penalty haircuts when posted as collateral by banks, raising their funding costs through the banking channel. Repositioning means abandoning the core business; private placements and non-euro funding exist but at visibly higher cost and smaller scale.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__climate_incorporation, fossil_fuel_producers, payer,
    organized, biographical, trapped, global).

% Steel, cement, chemicals, and aviation firms whose paper is down-weighted in purchase programs and haircut schedules. Process switching runs on capital-cycle timescales longer than the framework's revision cycle, so their cost disadvantage persists across planning horizons. They continue to borrow through syndicated loan markets priced off collateral eligible at the Eurosystem.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__climate_incorporation, carbon_intensive_manufacturers, payer,
    organized, biographical, constrained, continental).

% Renewable developers, grid operators, and green-bond issuers whose securities attract concentrated purchases and favorable collateral treatment, lowering their relative financing cost as demand concentrates on their paper. Most could fund privately in any scenario, so the advantage is incremental to otherwise viable businesses.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__climate_incorporation, climate_aligned_issuers, beneficiary,
    organized, biographical, mobile, continental).

% Asset managers, index providers, and verification firms whose green and ESG products gain benchmark status and asset flows as official-sector mandates legitimize climate-tilted investing. Fee income scales with the expanded green complex; revenues are diversified across mandates, so no single firm rises or falls with the schedules.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__climate_incorporation, green_finance_industry, beneficiary,
    institutional, biographical, mobile, global).

% Author of the Green Deal legislative package and custodian of the Union's climate targets. The treaties obligate every institution to integrate environmental protection into its work; alignment from the Eurosystem supplies monetary-side reinforcement the Commission cannot direct. It operates inside the same treaty architecture whose obligations it invokes.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__climate_incorporation, european_commission, beneficiary,
    institutional, generational, constrained, continental).

% Daily counterparties posting collateral to the Eurosystem and lending to the corporate sectors the schedules treat differently. Favorable terms on green collateral improve their liquidity positions; heavy exposure to carbon-intensive clients worsens theirs. Portfolio rebalancing across quarters keeps the exposure manageable, but the direction of adjustment is set by the schedules.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__climate_incorporation, commercial_banks, beneficiary,
    institutional, biographical, mobile, continental).
narrative_ontology:stakeholder_secondary_role(ecb_mandate_article_127__climate_incorporation, commercial_banks, payer).

% Deposit holders and pension savers whose money reaches the Eurosystem through bank collateral chains. They share the long-horizon returns of climate stabilization and of stable prices, and bear the marginal transmission costs — deposit spreads and pass-through of financing costs into goods prices — without any direct voice in the Council's decisions.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__climate_incorporation, euro_area_households, beneficiary,
    powerless, generational, constrained, continental).
narrative_ontology:stakeholder_secondary_role(ecb_mandate_article_127__climate_incorporation, euro_area_households, payer).

% Academic economists, think tanks, and some national central banking traditions who maintain that the primary objective exhausts the Eurosystem's operational obligations. They press their case through litigation, parliamentary questioning, and member-state politics rather than any seat in the deliberations that set the schedules.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__climate_incorporation, monetary_orthodoxy_advocates, excluded,
    organized, biographical, constrained, continental).

% Adjudicated the challenge brought against the climate-related elements of the monetary policy operations and fixed the legal boundary of what the treaties tolerate. Takes no position on calibration; its rulings define the outer limits within which the Governing Council revises the schedules.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__climate_incorporation, european_court_of_justice, observer,
    institutional, generational, analytical, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(ecb_mandate_article_127__climate_incorporation, climate_aligned_issuers).
narrative_ontology:fixing_cost_class(ecb_mandate_article_127__climate_incorporation, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Reprices climate-related financial risk across euro-area funding markets in a single coordinated move rather than issuer by issuer, reduces the transition-risk exposure accumulated on the Eurosystem balance sheet under market-neutral benchmarks, and discharges the treaty duty to integrate environmental protection into the implementation of Union policies.
% TRANSFER_FUNCTION: Moves financing-cost advantage from carbon-intensive issuers to climate-aligned issuers via purchase concentration and collateral haircuts; moves balance-sheet transition risk from the Eurosystem to private holders; moves institutional legitimacy and enforcement weight to the Union's climate policy agenda.
% ABSENT_VOICES: Critics who hold that the primary objective exhausts the Eurosystem's operational obligations speak through litigation and member-state politics rather than any seat in Governing Council deliberations. Carbon-intensive industries were consulted in impact assessments but hold no vote on the schedules. Depositors and pension savers affected at the transmission margin have no direct representation anywhere in the process.
% DISAPPEARANCE_RATIONALE: Collateral haircuts would flatten and purchase programs would revert to market-neutral benchmarks; carbon-intensive issuers' funding spreads would compress within weeks; green issuance premia would narrow; the Union's climate-financing architecture would lose its monetary anchor, and the Commission would face the integration duty unsupported on the monetary side.
% FOUNDING_PROBLEM: Two problems jointly: transition risk accumulating unpriced on the Eurosystem balance sheet because market-neutral benchmarks held climate-exposed paper as if riskless, and the unimplemented treaty duty to integrate environmental protection into all Union policy implementation, of which monetary policy is a part.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: the Network for Greening the Financial System's published risk assessments from central banks beyond the Eurosystem, the peer-reviewed climate-finance literature on transition-risk mispricing, supervisory stress-test results, and the apex court's willingness to adjudicate the schedules' legality rather than dismiss the risk premise outright. The sectors bearing the costs dispute the premise's policy force while their own litigation confirms the arrangements' operative reality; no corroborating attestation comes from any beneficiary seat.
narrative_ontology:disappearance_verdict(ecb_mandate_article_127__climate_incorporation, world_rearranges).
narrative_ontology:founding_problem_status(ecb_mandate_article_127__climate_incorporation, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ecb_mandate_article_127__climate_incorporation, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(ecb_mandate_article_127__climate_incorporation, 'none', 1).
narrative_ontology:epsilon_provenance(ecb_mandate_article_127__climate_incorporation, 0.48, 'stealth/ox-alpha', 'none', direct).

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
 *   Extraction is authored at 0.48: the financing-cost penalties are real, concentrated on two identifiable sectors, and phased in deliberately (visible in the rising series from 2021), but they sit well below snare territory because the mechanism reprices a risk class that independent analysis treats as genuine and discharges a duty the treaties impose on every institution. Suppression is 0.56 and operates through a novel channel: not prohibition but continuous repricing — a haircut schedule and a tilt methodology enforced on every collateral posting and every purchase tranche, with exit available only at penalty (private placement, non-euro funding) and, for producers whose asset base is the carbon itself, effectively unavailable on business timescales. Theater is 0.33: disclosure exercises, stress tests, and research publication carry a signaling component that outruns their operational bite, while the haircut and tilt mechanics bind hard money. Accessibility collapse is 0.40 — the foreclosed alternative is only the market-neutral status quo of Eurosystem operations; private funding markets and jurisdictional alternatives remain open, so understanding the arrangement does not close the option set. Resistance is 0.62: apex-court litigation, parliamentary and member-state objection, and sustained industry campaigning, which the rising suppression series partly answers. All three series run on one shared eight-point annual grid; the scalar base_properties values are the interval-end states. Enforcement capacity was built up over the interval (from no climate framework to full tilt-plus-haircut machinery), which is why suppression_requirement is tracked rather than left static.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats and the agenda-setter seat compute differently from the same schedules. From the Governing Council's position the arrangement is prudential housekeeping it was always obliged to perform; from the fossil producer's position it is a penalty levied on its existence through the liquidity system it cannot avoid; from the aligned issuer's position it is overdue correction of a mispricing that disadvantaged it; from the household seat it is nearly invisible except as marginal spread. The engine computes these divergent classifications from the structural data — power, exit, and role — and the divergence is the finding, not something the authored claim adjudicates.
 *
 * DIRECTIONALITY LOGIC:
 *   Fossil fuel producers combine payer role, trapped exit, and a business model identical to the penalized attribute, placing them nearest the full-target end of the directionality range. Carbon-intensive manufacturers are payers with constrained exit — high target, slightly damped by partial process-switching. Climate-aligned issuers and the green finance industry are beneficiaries with mobile exit, sitting near the beneficiary end; the Commission is a beneficiary bound into the architecture it gains from. Commercial banks are genuinely dual-positioned (favorable terms received, adverse client exposure borne) and derive a mid-range value. Households sit near symmetric: diffuse long-horizon benefit, diffuse marginal cost. No directionality_overrides are authored: the derivation chain separates even same-power atoms correctly here, because the organized-power fossil producers and the organized-power aligned issuers differ in both declared role and exit options, which is precisely the input the structural derivation reads.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification guards against two opposite mislabels. Calling this a snare would erase the genuine collective-action content — transition risk was demonstrably mispriced under market-neutral benchmarks, and the treaty integration duty is real enacted law, not cover invented after the fact. Calling it a rope would launder the concentrated sectoral incidence as neutral risk management, when the same schedule that protects the balance sheet reliably taxes two named industries. Tangled_rope keeps both facts load-bearing. On the genealogy interview: the founding problem (unpriced transition risk plus the unimplemented integration duty) is still live, corroborated from outside the benefiting parties, and the disappearance verdict is world_rearranges — so the status-times-verdict mismatch consumer finds no dead-mandate flag, and no piton drift is asserted. The theater series is watched, not tuned: if the real-economy effect omega resolves toward negligible, the performative share grows and inertial drift becomes the live hypothesis.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_assignment,
    'Which reading of the Article 127 kernel governs the Eurosystem''s operations — this one, in which the Article 11 TFEU integration duty operationally binds asset purchases and collateral policy, or a sibling in which it merely permits discretionary weight or never reaches operations?',
    'Doctrinal consolidation at the Court of Justice across successive legal challenges, or explicit treaty amendment reallocating the integration duty''s force for monetary policy.',
    'Sibling adoption dissolves this constraint''s beneficiary/victim structure entirely: the exclusive-focus sibling unwinds the tilts and haircuts and deletes the sectoral incidence, while the permissive-discretion sibling converts mandatory integration into episodic discretion, making the beneficiary set contingent on each exercise of balance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_assignment, conceptual, 'Kernel-level contest over the operational force assigned to the environmental integration clause.').

omega_variable(
    risk_pricing_vs_policy_premium,
    'Do the climate-differentiated haircuts track independently measurable transition-risk differentials, or do they embed a policy premium beyond what private markets charge the same issuers?',
    'Compare Eurosystem haircut schedules against CDS spreads and unsecured funding differentials for identical issuers over the same window.',
    'If the schedule exceeds the market risk differential, the excess functions as regulatory penalty and subsidy rather than risk pricing, sharpening the extraction component of the arrangement; if it tracks the differential, the incidence is ordinary risk-based pricing and the coordination reading strengthens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(risk_pricing_vs_policy_premium, empirical, 'Whether haircut differentiation prices risk or layers a policy premium on top of it.').

omega_variable(
    real_economy_emission_effect,
    'Does the purchase tilt alter real-economy emission trajectories, or does it mainly reallocate portfolio composition among already-priced securities?',
    'Difference-in-differences on issuance volumes, spreads, and capital expenditure for treated versus untreated issuers around the 2022 tilt introduction and the 2024 haircut revision.',
    'A negligible real-economy effect raises the performative share of the arrangement and points toward inertial drift once legal risk subsides; a measurable effect substantiates the coordination function and stabilizes the hybrid classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(real_economy_emission_effect, empirical, 'Whether the tilting mechanism changes what gets built or only who holds the paper.').

omega_variable(
    ultimate_cost_incidence,
    'Who ultimately bears the financing-cost penalties — firm owners, sector employees, or energy consumers through pass-through?',
    'Incidence modeling tracing haircut-driven funding costs into output prices and labor markets in the affected sectors.',
    'Consumer-bearing incidence would widen the paying population beyond the named sectors and dilute the concentration that drives the current classification; owner-bearing incidence keeps the payer set narrow and the asymmetry sharp.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(ultimate_cost_incidence, empirical, 'Downstream distribution of the collateral and purchase penalties.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ecb_mandate_article_127__climate_incorporation, 2019, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ecb__tr_t2019, ecb_mandate_article_127__climate_incorporation, theater_ratio, 2019, 0.1).
narrative_ontology:measurement_basis(ecb__tr_t2019, observed).
narrative_ontology:measurement(ecb__tr_t2020, ecb_mandate_article_127__climate_incorporation, theater_ratio, 2020, 0.18).
narrative_ontology:measurement_basis(ecb__tr_t2020, observed).
narrative_ontology:measurement(ecb__tr_t2021, ecb_mandate_article_127__climate_incorporation, theater_ratio, 2021, 0.22).
narrative_ontology:measurement_basis(ecb__tr_t2021, observed).
narrative_ontology:measurement(ecb__tr_t2022, ecb_mandate_article_127__climate_incorporation, theater_ratio, 2022, 0.25).
narrative_ontology:measurement_basis(ecb__tr_t2022, observed).
narrative_ontology:measurement(ecb__tr_t2023, ecb_mandate_article_127__climate_incorporation, theater_ratio, 2023, 0.28).
narrative_ontology:measurement_basis(ecb__tr_t2023, observed).
narrative_ontology:measurement(ecb__tr_t2024, ecb_mandate_article_127__climate_incorporation, theater_ratio, 2024, 0.3).
narrative_ontology:measurement_basis(ecb__tr_t2024, observed).
narrative_ontology:measurement(ecb__tr_t2025, ecb_mandate_article_127__climate_incorporation, theater_ratio, 2025, 0.32).
narrative_ontology:measurement_basis(ecb__tr_t2025, observed).
narrative_ontology:measurement(ecb__tr_t2026, ecb_mandate_article_127__climate_incorporation, theater_ratio, 2026, 0.33).
narrative_ontology:measurement_basis(ecb__tr_t2026, observed).

% Extraction over time
narrative_ontology:measurement(ecb__be_t2019, ecb_mandate_article_127__climate_incorporation, base_extractiveness, 2019, 0.05).
narrative_ontology:measurement_basis(ecb__be_t2019, observed).
narrative_ontology:measurement(ecb__be_t2020, ecb_mandate_article_127__climate_incorporation, base_extractiveness, 2020, 0.08).
narrative_ontology:measurement_basis(ecb__be_t2020, observed).
narrative_ontology:measurement(ecb__be_t2021, ecb_mandate_article_127__climate_incorporation, base_extractiveness, 2021, 0.15).
narrative_ontology:measurement_basis(ecb__be_t2021, observed).
narrative_ontology:measurement(ecb__be_t2022, ecb_mandate_article_127__climate_incorporation, base_extractiveness, 2022, 0.28).
narrative_ontology:measurement_basis(ecb__be_t2022, observed).
narrative_ontology:measurement(ecb__be_t2023, ecb_mandate_article_127__climate_incorporation, base_extractiveness, 2023, 0.38).
narrative_ontology:measurement_basis(ecb__be_t2023, observed).
narrative_ontology:measurement(ecb__be_t2024, ecb_mandate_article_127__climate_incorporation, base_extractiveness, 2024, 0.45).
narrative_ontology:measurement_basis(ecb__be_t2024, observed).
narrative_ontology:measurement(ecb__be_t2025, ecb_mandate_article_127__climate_incorporation, base_extractiveness, 2025, 0.47).
narrative_ontology:measurement_basis(ecb__be_t2025, observed).
narrative_ontology:measurement(ecb__be_t2026, ecb_mandate_article_127__climate_incorporation, base_extractiveness, 2026, 0.48).
narrative_ontology:measurement_basis(ecb__be_t2026, observed).

% Suppression requirement over time
narrative_ontology:measurement(ecb__su_t2019, ecb_mandate_article_127__climate_incorporation, suppression_requirement, 2019, 0.05).
narrative_ontology:measurement_basis(ecb__su_t2019, observed).
narrative_ontology:measurement(ecb__su_t2020, ecb_mandate_article_127__climate_incorporation, suppression_requirement, 2020, 0.1).
narrative_ontology:measurement_basis(ecb__su_t2020, observed).
narrative_ontology:measurement(ecb__su_t2021, ecb_mandate_article_127__climate_incorporation, suppression_requirement, 2021, 0.2).
narrative_ontology:measurement_basis(ecb__su_t2021, observed).
narrative_ontology:measurement(ecb__su_t2022, ecb_mandate_article_127__climate_incorporation, suppression_requirement, 2022, 0.33).
narrative_ontology:measurement_basis(ecb__su_t2022, observed).
narrative_ontology:measurement(ecb__su_t2023, ecb_mandate_article_127__climate_incorporation, suppression_requirement, 2023, 0.44).
narrative_ontology:measurement_basis(ecb__su_t2023, observed).
narrative_ontology:measurement(ecb__su_t2024, ecb_mandate_article_127__climate_incorporation, suppression_requirement, 2024, 0.52).
narrative_ontology:measurement_basis(ecb__su_t2024, observed).
narrative_ontology:measurement(ecb__su_t2025, ecb_mandate_article_127__climate_incorporation, suppression_requirement, 2025, 0.55).
narrative_ontology:measurement_basis(ecb__su_t2025, observed).
narrative_ontology:measurement(ecb__su_t2026, ecb_mandate_article_127__climate_incorporation, suppression_requirement, 2026, 0.56).
narrative_ontology:measurement_basis(ecb__su_t2026, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ecb_mandate_article_127__climate_incorporation, resource_allocation).
narrative_ontology:affects_constraint(ecb_mandate_article_127__climate_incorporation, ecb_mandate_article_127__orthodox_price_stability).
narrative_ontology:affects_constraint(ecb_mandate_article_127__climate_incorporation, ecb_mandate_article_127__expansive_secondary_objectives).

% DUAL FORMULATION NOTE:
% The colloquial label 'ECB climate mandate' decomposes into three structurally distinct readings of one kernel (Articles 127 and 11 TFEU together). This file authors the climate_incorporation reading: a standing tilt-and-haircut arrangement with named sectoral winners and losers, epsilon reflecting the concentrated incidence assessed by this reading's own lights. The orthodox_price_stability sibling authors an exclusive-focus arrangement with no sectoral beneficiary structure and near-zero epsilon; the expansive_secondary_objectives sibling authors a discretionary-balancing arrangement whose beneficiary set varies with each exercise of discretion. Each is a separate constraint with its own epsilon, its own stakeholders, and its own classification; they are linked here via affects_constraints, with the upstream doctrinal question (which reading the treaties compel) flowing into the downstream operational arrangements.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
