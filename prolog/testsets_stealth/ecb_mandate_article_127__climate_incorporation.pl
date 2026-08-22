% ============================================================================
% CONSTRAINT STORY: ecb_mandate_article_127__climate_incorporation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-06
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
 *   human_readable: ECB Mandate — Climate Incorporation Reading (Article 127 TFEU x Article 11 Environmental Integration)
 *   domain: monetary_policy/eu_constitutional_law
 *
 * SUMMARY:
 *   Under the climate-incorporation reading, the ECB's operational frameworks
 *   are treated as treaty-bound to integrate climate risk: corporate bond
 *   purchases are tilted toward lower-emission issuers, collateral haircuts
 *   penalize carbon-intensive paper, and counterparties face phased
 *   climate-disclosure requirements, all justified by Article 11 TFEU's
 *   environmental-integration clause alongside balance-sheet risk management.
 *   The standing arrangement under contest — and the referent of epsilon — is
 *   this operating overlay as actually administered, assessed by this
 *   reading's own lights; the reading's endorsed alternative (some other
 *   configuration of Eurosystem operations) is not the referent. The
 *   claim/metric gap is deliberate: the arrangement is CLAIMED as
 *   tangled_rope (genuine risk-pricing coordination plus asymmetric
 *   incidence) while the metrics are authored independently from its observed
 *   operation — the engine computes per-seat types from the structural data.
 *
 * KEY AGENTS:
 *   - ecb_governing_council: agenda setter (institutional/arbitrage) — administers haircut schedules, purchase tilts, and disclosure requirements
 *   - low_carbon_bond_issuers: primary beneficiary (organized/constrained) — collect the funding-cost advantage
 *   - green_asset_managers: secondary beneficiary (organized/mobile) — hold assets supported by Eurosystem demand
 *   - eu_climate_policy_institutions: secondary beneficiary (institutional/mobile) — obtain monetary reinforcement of Green Deal transmission
 *   - fossil_energy_producers: primary target (powerful/constrained) — bear haircut and eligibility costs
 *   - carbon_intensive_industrial_borrowers: secondary target (moderate/trapped) — absorb widened funding spreads
 *   - commercial_bank_counterparties: dual-positioned conduit (organized/constrained) — bear compliance costs, retain liquidity access
 *   - euro_area_households: absent voice (powerless/trapped) — bear diffuse ultimate costs with no seat
 *   - cjeu_mandate_adjudicators: analytical observer (institutional/analytical) — adjudicate competence after the fact
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ecb_mandate_article_127__climate_incorporation, 0.58).
domain_priors:suppression_score(ecb_mandate_article_127__climate_incorporation, 0.42).
domain_priors:theater_ratio(ecb_mandate_article_127__climate_incorporation, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ecb_mandate_article_127__climate_incorporation, extractiveness, 0.58).
narrative_ontology:constraint_metric(ecb_mandate_article_127__climate_incorporation, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(ecb_mandate_article_127__climate_incorporation, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ecb_mandate_article_127__climate_incorporation, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(ecb_mandate_article_127__climate_incorporation, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ecb_mandate_article_127__climate_incorporation, tangled_rope).
narrative_ontology:human_readable(ecb_mandate_article_127__climate_incorporation, "ECB Mandate — Climate Incorporation Reading (Article 127 TFEU x Article 11 Environmental Integration)").
narrative_ontology:topic_domain(ecb_mandate_article_127__climate_incorporation, "monetary_policy/eu_constitutional_law").

domain_priors:requires_active_enforcement(ecb_mandate_article_127__climate_incorporation).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ecb_mandate_article_127__climate_incorporation, '5baf9c62-b39f-4610-a9b4-3b34941c1e8a').
narrative_ontology:cs_kernel_codification('5baf9c62-b39f-4610-a9b4-3b34941c1e8a', fixed_text).
narrative_ontology:cs_authority_grounding('5baf9c62-b39f-4610-a9b4-3b34941c1e8a', lineage).
narrative_ontology:cs_interpretation_layer_present('5baf9c62-b39f-4610-a9b4-3b34941c1e8a').
narrative_ontology:cs_reading_relation('5baf9c62-b39f-4610-a9b4-3b34941c1e8a', ecb_mandate_article_127__orthodox_price_stability, forecloses).
narrative_ontology:cs_reading_relation('5baf9c62-b39f-4610-a9b4-3b34941c1e8a', ecb_mandate_article_127__expansive_secondary_objectives, influences).
narrative_ontology:cs_axiom('5baf9c62-b39f-4610-a9b4-3b34941c1e8a', foundational, article_11_integration_is_operationally_binding).
narrative_ontology:cs_axiom_status(article_11_integration_is_operationally_binding, holdable).
narrative_ontology:cs_axiom_grounding('5baf9c62-b39f-4610-a9b4-3b34941c1e8a', article_11_integration_is_operationally_binding, conventional).
narrative_ontology:cs_axiom('5baf9c62-b39f-4610-a9b4-3b34941c1e8a', foundational, climate_transition_risk_is_price_stability_relevant).
narrative_ontology:cs_axiom_status(climate_transition_risk_is_price_stability_relevant, holdable).
narrative_ontology:cs_axiom_grounding('5baf9c62-b39f-4610-a9b4-3b34941c1e8a', climate_transition_risk_is_price_stability_relevant, empirically_contingent).
narrative_ontology:cs_reference_frame('5baf9c62-b39f-4610-a9b4-3b34941c1e8a', environmental_integration_binding_baseline).
narrative_ontology:cs_drift_state('5baf9c62-b39f-4610-a9b4-3b34941c1e8a', contemporary_climate_action_plan_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('5baf9c62-b39f-4610-a9b4-3b34941c1e8a', '').
narrative_ontology:cs_kernel_id(ecb_mandate_article_127__climate_incorporation, ecb_mandate_article_127).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ecb_mandate_article_127__climate_incorporation, low_carbon_bond_issuers).
narrative_ontology:constraint_beneficiary(ecb_mandate_article_127__climate_incorporation, green_asset_managers).
narrative_ontology:constraint_beneficiary(ecb_mandate_article_127__climate_incorporation, eu_climate_policy_institutions).
narrative_ontology:constraint_victim(ecb_mandate_article_127__climate_incorporation, fossil_energy_producers).
narrative_ontology:constraint_victim(ecb_mandate_article_127__climate_incorporation, carbon_intensive_industrial_borrowers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(ecb_mandate_article_127__climate_incorporation, commercial_bank_counterparties).
narrative_ontology:constraint_victim(ecb_mandate_article_127__climate_incorporation, commercial_bank_counterparties).
narrative_ontology:constraint_vindicates(ecb_mandate_article_127__climate_incorporation, article_11_tfeu_environmental_integration).
narrative_ontology:constraint_vindicates(ecb_mandate_article_127__climate_incorporation, climate_financial_risk_materiality).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets the annual climate-related parameter schedule for Eurosystem operations: haircut calibrations for corporate collateral, decarbonization paths for bond purchase portfolios, and counterparty climate-disclosure requirements. Publishes the legal reasoning tying these parameters to Article 11 TFEU and to balance-sheet risk management. Adjusts or suspends parameters by internal vote; no external actor can compel a parameter change.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__climate_incorporation, ecb_governing_council, agenda_setter,
    institutional, generational, arbitrage, continental).

% Issue bonds that qualify for purchase under tilted eligibility rules and post collateral carrying reduced haircuts. Their funding costs sit below comparable carbon-intensive issuers whenever the framework operates. Their position depends on continued parameter settings; a framework reversal would remove the funding advantage.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__climate_incorporation, low_carbon_bond_issuers, beneficiary,
    organized, biographical, constrained, continental).

% Run funds concentrated in climate-aligned assets whose relative valuations are supported by Eurosystem purchase demand. Operate across jurisdictions and can shift mandates or domicile products elsewhere if euro-area parameters change.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__climate_incorporation, green_asset_managers, beneficiary,
    organized, biographical, mobile, global).

% Draft and enforce EU climate legislation, including the Green Deal package and emissions trading revisions. Obtain monetary-side reinforcement of policy transmission without budgetary outlay; their instruments interact with the collateral and purchase parameters, but neither body controls the other.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__climate_incorporation, eu_climate_policy_institutions, beneficiary,
    institutional, generational, mobile, continental).

% Large energy companies whose bonds carry elevated haircuts and sharply reduced purchase eligibility under the framework. Funding against their paper through Eurosystem facilities costs more than before the climate parameters were introduced, and refinancing increasingly runs through costlier private channels. Decarbonization timelines span decades while parameters revise annually; relocating issuance outside the euro system would forfeit their core investor base.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__climate_incorporation, fossil_energy_producers, payer,
    powerful, biographical, constrained, continental).

% Steel, cement, and chemical producers dependent on bank credit lines that fund themselves partly through Eurosystem collateral operations. They lack the balance-sheet flexibility to restructure plant and fuel mix quickly and absorb the widened funding spread as margin compression.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__climate_incorporation, carbon_intensive_industrial_borrowers, payer,
    moderate, biographical, trapped, continental).

% Post collateral and borrow in Eurosystem facilities. Must reweight pledged portfolios toward lower-carbon assets and meet phased disclosure requirements to preserve full access to liquidity operations. Pass part of the added cost through to loan clients while retaining privileged liquidity access unavailable outside the system.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__climate_incorporation, commercial_bank_counterparties, payer,
    organized, biographical, constrained, continental).
narrative_ontology:stakeholder_secondary_role(ecb_mandate_article_127__climate_incorporation, commercial_bank_counterparties, beneficiary).

% Bear the eventual costs of both inflation deviations and transition financing through prices, wages, and taxes. Have no procedural seat in parameter setting; representation runs indirectly through political principals who do not control the Governing Council.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__climate_incorporation, euro_area_households, excluded,
    powerless, generational, trapped, continental).

% The Court of Justice of the European Union adjudicates competence challenges to climate-related monetary measures. It reviews whether parameter choices remain within the treaty's enumerated objectives; its rulings bind the framework but arrive years after the measures under review.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__climate_incorporation, cjeu_mandate_adjudicators, observer,
    institutional, generational, analytical, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(ecb_mandate_article_127__climate_incorporation, low_carbon_bond_issuers).
narrative_ontology:fixing_cost_class(ecb_mandate_article_127__climate_incorporation, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a shared standard for how climate-related financial risk enters the Eurosystem balance sheet: collateral valuation, purchase eligibility, and counterparty disclosure are priced and screened once, centrally, rather than left to divergent national practices, and Eurosystem operations are aligned with the Union's binding environmental-policy commitments.
% TRANSFER_FUNCTION: Moves liquidity terms and balance-sheet capacity from carbon-intensive collateral posters and issuers toward climate-aligned ones: cheaper funding and purchase support flow to green assets, while higher haircuts and exclusion costs fall on fossil-linked paper.
% ABSENT_VOICES: Euro-area households bear the ultimate costs of both inflation deviations and transition financing but hold no seat in parameter design; carbon-intensive regions and workforces carry the real-economy adjustment costs decided in financial-regulatory fora; non-euro issuers into euro markets are subject to eligibility effects without voice. All three stand outside the Governing Council's operational consultation loop, represented only indirectly through political principals.
% DISAPPEARANCE_RATIONALE: If the climate-integration overlay vanished overnight, collateral haircuts would flatten toward pure credit-risk calibration, purchase tilts would unwind, green funding premia would compress, and fossil-sector funding costs would drop immediately — financing conditions across the euro area would reorganize around the reverted parameters within one refinancing cycle.
% FOUNDING_PROBLEM: Climate-related transition risk was unpriced in central bank collateral frameworks and asset purchases, leaving the Eurosystem balance sheet exposed to disorderly repricing while Union treaty law (Article 11 TFEU) required environmental protection to be integrated into all Union policies — a duty the Eurosystem's operational frameworks had not operationalized.
% FOUNDING_PROBLEM_CORROBORATION: Attested from outside the benefiting parties by the Network for Greening the Financial System's multi-central-bank risk assessments, peer central banks' parallel climate-risk programs, academic climate-finance literature on transition-risk mispricing, and EIOPA insurance-sector risk reports. Orthodox monetary economists dispute the mandate reading itself while largely conceding the underlying risk-materiality problem, which corroborates problem liveness independently of this reading's contested legal premise.
narrative_ontology:disappearance_verdict(ecb_mandate_article_127__climate_incorporation, world_rearranges).
narrative_ontology:founding_problem_status(ecb_mandate_article_127__climate_incorporation, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ecb_mandate_article_127__climate_incorporation, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(ecb_mandate_article_127__climate_incorporation, 'none', 1).
narrative_ontology:epsilon_provenance(ecb_mandate_article_127__climate_incorporation, 0.58, 'stealth/ox-alpha', 'none', direct).

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
 *   Extractiveness is moderate-high (0.58) because the incidence is real and asymmetric — carbon-intensive issuers face measurably costlier Eurosystem funding — but bounded: the mechanism reprices access rather than seizing assets, and magnitudes remain a fraction of total funding costs. Suppression (0.42) is lower than extractiveness because the mechanism steers through terms rather than prohibition: nothing forbids fossil-linked issuance, and private-market alternatives exist, though Eurosystem liquidity access is leverage no large euro-area borrower can ignore. Suppression is authored as a raw structural property and is deliberately NOT scaled by power or scope — only extractiveness is scaled downstream. Theater (0.30) reflects genuine financial effects diluted by symbolic elements: initial tilt factors of basis-point scale, long disclosure phase-ins, and communication that markets the framework as transformative beyond its current calibration. Accessibility_collapse (0.45) is partial — decarbonization, private issuance, and green relabeling remain open exits — and resistance (0.55) is substantive: competence litigation, political backlash, industry lobbying, and orthodox-economic critique all press on the framework. The measurement series run on one shared time grid (T=0..10, every 2) so every tracked metric is authored at every examined time point; trajectories are monotonic (framework tightening, no oscillation), with endpoint values projected forward from the observed trend and marked as such.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently by construction. From the fossil_energy_producers seat the framework operates as punitive repricing imposed by an unelected body on lawful activity; from the low_carbon_bond_issuers seat the identical parameters are risk correction that finally prices a mispriced externality; from the ecb_governing_council seat they are mandate execution and prudent balance-sheet management; from euro_area_households the whole apparatus is a remote technocracy trading off their inflation and energy costs without their voice. The engine derives these divergent classifications from power, exit, and directional position — the authored claim does not adjudicate among them. Note on coalition: the two payer groups share exposure but differ sharply in power and exit (powerful/constrained versus moderate/trapped), so joint resistance is possible in principle yet fragmented by divergent sector interests and adjustment timelines.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations map to low directionality: low_carbon_bond_issuers and green_asset_managers sit near the subsidized end (the parameters lower their relative funding costs), and eu_climate_policy_institutions benefit incidentally through reinforced policy transmission without bearing framework costs. Victim declarations map to high directionality: fossil_energy_producers and carbon_intensive_industrial_borrowers bear the transfer through the same haircut and eligibility schedules that subsidize the green side, with trapped or constrained exits pushing them toward the full-target end. commercial_bank_counterparties sit near symmetric — they pay compliance costs and retain privileged access, so costs and benefits roughly offset. euro_area_households are structurally target-side (diffuse cost-bearers, no exit) but powerless, so their high effective extraction carries no enforcement weight. The agenda_setter collects no direct rent; its stake is remit and institutional precedent, which the derivation treats as weakly beneficiary-side.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — unpriced transition risk plus an unoperationalized Article 11 duty — remains live, so no mandatrophy declaration is warranted and none is keyed to any metric. The tangled_rope classification does the protective work in both directions: a pure-rope reading (the Governing Council's own framing, risk management only) would erase the identifiable payers and the asymmetric incidence riding on the same schedules; a pure-snare reading (parts of the industry's framing, confiscatory green repricing) would erase the genuine coordination function — centralized climate-risk pricing and treaty alignment — that persists even under full symmetry assumptions. The classification holds both facts simultaneously and lets the temporal record show which component is growing: extractiveness and suppression rise together with enforcement capacity, while theater creeps up more slowly, indicating the extraction component is currently compounding faster than the performative one.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    mandate_reading_underdetermination,
    'This constraint is one reading of kernel ecb_mandate_article_127 (reading: climate_incorporation). Would either sibling reading instantiate a structurally different constraint with a different epsilon?',
    'A CJEU ruling on the operational status of Article 127(2) and Article 11 TFEU, or an explicit intergovernmental treaty clarification of the mandate''s hierarchy.',
    'Under orthodox_price_stability the entire beneficiary/victim set dissolves — no climate-targeted incidence exists and the arrangement reduces to ordinary collateral policy with negligible epsilon. Under expansive_secondary_objectives the climate-specific incidence generalizes into politically cycled discretionary balancing. Both classification and epsilon move accordingly; the current story is valid only within this reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(mandate_reading_underdetermination, conceptual, 'Kernel-level ambiguity: which reading of Article 127 governs determines whether this constraint exists at all.').

omega_variable(
    haircut_calibration_basis,
    'Are the climate-adjusted haircuts calibrated to measured transition-risk differentials, or do they embed policy preference beyond what risk data supports?',
    'Independent comparison of haircut deltas against realized default and loss differentials and against rating-agency transition-risk models for the same issuer classes.',
    'Haircuts exceeding risk-justified levels enlarge the extraction component and push the arrangement snare-ward; haircuts tracking risk data strengthen the coordination reading and stabilize the tangled_rope classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(haircut_calibration_basis, empirical, 'Whether the penalty schedule prices risk or preference.').

omega_variable(
    tilt_real_economy_transmission,
    'Does portfolio tilting shift real capital allocation toward new low-carbon investment, or does it mainly relabel existing portfolios and refinance already-green projects?',
    'Trace marginal funding from tilted purchases and favorable haircuts to new green capital expenditure versus refinancing of pre-existing projects.',
    'Predominant relabeling raises theater_ratio over time and drives piton-ward drift (performative maintenance of a fading function); genuine real-economy transmission supports the tangled_rope classification with a growing coordination share.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(tilt_real_economy_transmission, empirical, 'Whether the steering mechanism moves real resources or performs movement.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ecb_mandate_article_127__climate_incorporation, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ecb__tr_t0, ecb_mandate_article_127__climate_incorporation, theater_ratio, 0, 0.18).
narrative_ontology:measurement_basis(ecb__tr_t0, observed).
narrative_ontology:measurement(ecb__tr_t2, ecb_mandate_article_127__climate_incorporation, theater_ratio, 2, 0.2).
narrative_ontology:measurement_basis(ecb__tr_t2, observed).
narrative_ontology:measurement(ecb__tr_t4, ecb_mandate_article_127__climate_incorporation, theater_ratio, 4, 0.22).
narrative_ontology:measurement_basis(ecb__tr_t4, observed).
narrative_ontology:measurement(ecb__tr_t6, ecb_mandate_article_127__climate_incorporation, theater_ratio, 6, 0.25).
narrative_ontology:measurement_basis(ecb__tr_t6, observed).
narrative_ontology:measurement(ecb__tr_t8, ecb_mandate_article_127__climate_incorporation, theater_ratio, 8, 0.27).
narrative_ontology:measurement_basis(ecb__tr_t8, observed).
narrative_ontology:measurement(ecb__tr_t10, ecb_mandate_article_127__climate_incorporation, theater_ratio, 10, 0.3).
narrative_ontology:measurement_basis(ecb__tr_t10, projected).

% Extraction over time
narrative_ontology:measurement(ecb__be_t0, ecb_mandate_article_127__climate_incorporation, base_extractiveness, 0, 0.34).
narrative_ontology:measurement_basis(ecb__be_t0, observed).
narrative_ontology:measurement(ecb__be_t2, ecb_mandate_article_127__climate_incorporation, base_extractiveness, 2, 0.39).
narrative_ontology:measurement_basis(ecb__be_t2, observed).
narrative_ontology:measurement(ecb__be_t4, ecb_mandate_article_127__climate_incorporation, base_extractiveness, 4, 0.44).
narrative_ontology:measurement_basis(ecb__be_t4, observed).
narrative_ontology:measurement(ecb__be_t6, ecb_mandate_article_127__climate_incorporation, base_extractiveness, 6, 0.49).
narrative_ontology:measurement_basis(ecb__be_t6, observed).
narrative_ontology:measurement(ecb__be_t8, ecb_mandate_article_127__climate_incorporation, base_extractiveness, 8, 0.54).
narrative_ontology:measurement_basis(ecb__be_t8, observed).
narrative_ontology:measurement(ecb__be_t10, ecb_mandate_article_127__climate_incorporation, base_extractiveness, 10, 0.58).
narrative_ontology:measurement_basis(ecb__be_t10, projected).

% Suppression requirement over time
narrative_ontology:measurement(ecb__su_t0, ecb_mandate_article_127__climate_incorporation, suppression_requirement, 0, 0.25).
narrative_ontology:measurement_basis(ecb__su_t0, observed).
narrative_ontology:measurement(ecb__su_t2, ecb_mandate_article_127__climate_incorporation, suppression_requirement, 2, 0.29).
narrative_ontology:measurement_basis(ecb__su_t2, observed).
narrative_ontology:measurement(ecb__su_t4, ecb_mandate_article_127__climate_incorporation, suppression_requirement, 4, 0.33).
narrative_ontology:measurement_basis(ecb__su_t4, observed).
narrative_ontology:measurement(ecb__su_t6, ecb_mandate_article_127__climate_incorporation, suppression_requirement, 6, 0.36).
narrative_ontology:measurement_basis(ecb__su_t6, observed).
narrative_ontology:measurement(ecb__su_t8, ecb_mandate_article_127__climate_incorporation, suppression_requirement, 8, 0.39).
narrative_ontology:measurement_basis(ecb__su_t8, observed).
narrative_ontology:measurement(ecb__su_t10, ecb_mandate_article_127__climate_incorporation, suppression_requirement, 10, 0.42).
narrative_ontology:measurement_basis(ecb__su_t10, projected).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ecb_mandate_article_127__climate_incorporation, resource_allocation).
narrative_ontology:affects_constraint(ecb_mandate_article_127__climate_incorporation, ecb_mandate_article_127__orthodox_price_stability).
narrative_ontology:affects_constraint(ecb_mandate_article_127__climate_incorporation, ecb_mandate_article_127__expansive_secondary_objectives).
narrative_ontology:affects_constraint(ecb_mandate_article_127__climate_incorporation, eu_emissions_trading_system).

% DUAL FORMULATION NOTE:
% Article 127 TFEU's two-clause structure (primary price-stability objective; 'without prejudice' support for Union policies, including Article 11 environmental integration) is a single fixed-text kernel decomposed into three readings, each instantiating a distinct constraint with distinct epsilon and distinct beneficiary/victim structure: orthodox_price_stability (no operational secondaries; negligible targeted extraction), expansive_secondary_objectives (discretionary balancing; variable, politically cycled incidence), and climate_incorporation (this file: required integration; stable asymmetric incidence on carbon-intensive collateral). The orthodox reading is upstream — it historically grounded the framework this reading modifies — and each story links the others via affects_constraints. The colloquial label 'the ECB mandate' conflates these; the decomposition follows the epsilon-invariance rule: one reading, one constraint, one epsilon.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
