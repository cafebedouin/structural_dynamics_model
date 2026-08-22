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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
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
 *   human_readable: ECB Mandate Read Through Article 11 TFEU Climate Integration Duty
 *   domain: monetary_policy/constitutional_law/eu_institutional_governance
 *
 * SUMMARY:
 *   This story instantiates the climate_incorporation reading of the Article
 *   127 ECB mandate kernel: the view that the treaty's environmental
 *   integration clause (Article 11 TFEU) operationally binds the ECB to
 *   incorporate climate risk into its collateral frameworks and asset
 *   purchase programs, alongside its price-stability core function. Under
 *   this reading, the ECB's portfolio tilting toward green-labeled assets and
 *   away from carbon-intensive collateral is not mission creep but treaty
 *   compliance intersecting with a genuine prudential concern (stranded-asset
 *   risk to collateral value). The sibling readings —
 *   orthodox_price_stability (exclusive 2% inflation focus, climate
 *   considerations non-operational) and expansive_secondary_objectives
 *   (discretionary balancing under the 'without prejudice' clause, not
 *   specifically climate-anchored) — are separate constraints with their own
 *   ε values, not alternative measurements of this one. Under
 *   orthodox_price_stability, climate tilting would appear as unauthorized
 *   mandate expansion with much higher illegitimacy and possibly a snare
 *   classification from the fossil-issuer seat; under climate_incorporation,
 *   the same operational facts are read as a coordinated implementation of
 *   dual treaty obligations, producing a tangled_rope: real coordination
 *   (systemic risk internalization, treaty compliance) plus real extraction
 *   (differential financing costs imposed on carbon-intensive issuers who did
 *   not consent to being repriced through monetary operations).
 *
 * KEY AGENTS:
 *   - ecb_governing_council: agenda-setter administering collateral and purchase criteria, deriving authority from Article 11 TFEU as read into Article 127
 *   - climate_transition_industries and green_bond_issuers: beneficiaries capturing preferential financing terms
 *   - fossil_fuel_issuers and carbon_intensive_manufacturers: targets bearing higher collateral haircuts and funding costs
 *   - member_states_with_high_carbon_sovereign_debt: sovereign-level payers trapped within monetary union
 *   - eu_courts_and_legal_scholars: analytical observers who could authoritatively resolve or foreclose this reading
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ecb_mandate_article_127__climate_incorporation, 0.52).
domain_priors:suppression_score(ecb_mandate_article_127__climate_incorporation, 0.58).
domain_priors:theater_ratio(ecb_mandate_article_127__climate_incorporation, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ecb_mandate_article_127__climate_incorporation, extractiveness, 0.52).
narrative_ontology:constraint_metric(ecb_mandate_article_127__climate_incorporation, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(ecb_mandate_article_127__climate_incorporation, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ecb_mandate_article_127__climate_incorporation, accessibility_collapse, 0.38).
narrative_ontology:constraint_metric(ecb_mandate_article_127__climate_incorporation, resistance, 0.61).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ecb_mandate_article_127__climate_incorporation, tangled_rope).
narrative_ontology:human_readable(ecb_mandate_article_127__climate_incorporation, "ECB Mandate Read Through Article 11 TFEU Climate Integration Duty").
narrative_ontology:topic_domain(ecb_mandate_article_127__climate_incorporation, "monetary_policy/constitutional_law/eu_institutional_governance").

domain_priors:requires_active_enforcement(ecb_mandate_article_127__climate_incorporation).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ecb_mandate_article_127__climate_incorporation, '508e717f-a31a-4690-9f10-a79615db2a3d').
narrative_ontology:cs_kernel_codification('508e717f-a31a-4690-9f10-a79615db2a3d', fixed_text).
narrative_ontology:cs_authority_grounding('508e717f-a31a-4690-9f10-a79615db2a3d', lineage).
narrative_ontology:cs_interpretation_layer_present('508e717f-a31a-4690-9f10-a79615db2a3d').
narrative_ontology:cs_reading_relation('508e717f-a31a-4690-9f10-a79615db2a3d', ecb_mandate_article_127__orthodox_price_stability, forecloses).
narrative_ontology:cs_reading_relation('508e717f-a31a-4690-9f10-a79615db2a3d', ecb_mandate_article_127__expansive_secondary_objectives, influences).
narrative_ontology:cs_axiom('508e717f-a31a-4690-9f10-a79615db2a3d', foundational, article_11_binds_ecb_operations_directly).
narrative_ontology:cs_axiom_status(article_11_binds_ecb_operations_directly, holdable).
narrative_ontology:cs_axiom_grounding('508e717f-a31a-4690-9f10-a79615db2a3d', article_11_binds_ecb_operations_directly, conventional).
narrative_ontology:cs_axiom('508e717f-a31a-4690-9f10-a79615db2a3d', secondary, climate_risk_is_price_stability_relevant).
narrative_ontology:cs_axiom_status(climate_risk_is_price_stability_relevant, holdable).
narrative_ontology:cs_axiom_grounding('508e717f-a31a-4690-9f10-a79615db2a3d', climate_risk_is_price_stability_relevant, empirically_contingent).
narrative_ontology:cs_reference_frame('508e717f-a31a-4690-9f10-a79615db2a3d', maastricht_price_stability_primacy).
narrative_ontology:cs_drift_state('508e717f-a31a-4690-9f10-a79615db2a3d', post_2022_climate_agenda_mainstreaming, gap(axiom_overriding, substantial, true)).
narrative_ontology:cs_created_at('508e717f-a31a-4690-9f10-a79615db2a3d', '').
narrative_ontology:cs_kernel_id(ecb_mandate_article_127__climate_incorporation, ecb_mandate_article_127).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ecb_mandate_article_127__climate_incorporation, climate_transition_industries).
narrative_ontology:constraint_beneficiary(ecb_mandate_article_127__climate_incorporation, green_bond_issuers).
narrative_ontology:constraint_beneficiary(ecb_mandate_article_127__climate_incorporation, eu_climate_policy_apparatus).
narrative_ontology:constraint_victim(ecb_mandate_article_127__climate_incorporation, fossil_fuel_issuers).
narrative_ontology:constraint_victim(ecb_mandate_article_127__climate_incorporation, carbon_intensive_manufacturers).
narrative_ontology:constraint_victim(ecb_mandate_article_127__climate_incorporation, member_states_with_high_carbon_sovereign_debt).
narrative_ontology:constraint_vindicates(ecb_mandate_article_127__climate_incorporation, environmental_integration_principle_article_11_tfeu).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Designs and administers the collateral framework and asset purchase eligibility criteria, deciding how much climate risk weighting and portfolio tilting to apply. Justifies the tilt as both prudential (stranded-asset risk to collateral value) and treaty-mandated (Article 11 environmental integration duty binding on all EU institutional action, including the ECB's). Faces no direct financial cost from the tilt and controls its calibration.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__climate_incorporation, ecb_governing_council, agenda_setter,
    institutional, generational, arbitrage, continental).

% Renewable energy, green infrastructure, and low-carbon manufacturing firms see their bonds favored in eligibility criteria and haircut schedules, lowering their cost of capital relative to carbon-intensive competitors. They did not create the tilt but capture its financing benefit.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__climate_incorporation, climate_transition_industries, beneficiary,
    organized, generational, arbitrage, continental).

% Sovereigns and corporates issuing labeled green debt receive preferential collateral treatment and purchase-program eligibility, reducing issuance spreads. Their gain is a direct transfer effect of the same criteria that penalize carbon-intensive issuers.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__climate_incorporation, green_bond_issuers, beneficiary,
    moderate, biographical, mobile, continental).

% The European Commission and Council climate directorates gain a powerful enforcement lever for Green Deal targets without needing new legislation — the ECB's balance sheet does policy work Parliament has not separately authorized in monetary terms. Article 11 TFEU is their textual anchor for pressing the ECB to internalize climate objectives.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__climate_incorporation, eu_climate_policy_apparatus, beneficiary,
    institutional, civilizational, analytical, continental).
narrative_ontology:stakeholder_secondary_role(ecb_mandate_article_127__climate_incorporation, eu_climate_policy_apparatus, agenda_setter).

% Oil, gas, and coal-linked corporates face higher collateral haircuts and reduced or excluded eligibility for ECB purchase programs, raising their funding costs directly through the central bank's own balance sheet operations. They cannot exit the eurozone credit system without abandoning euro-denominated financing altogether; litigation over mandate scope is their main recourse.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__climate_incorporation, fossil_fuel_issuers, payer,
    powerful, biographical, constrained, continental).

% Steel, cement, and chemical firms with heavy legacy emissions profiles see collateral value and funding access degrade as climate risk weighting tightens, even where they are pursuing compliant transition plans on realistic timelines. They lack the organized lobbying reach of large energy majors.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__climate_incorporation, carbon_intensive_manufacturers, payer,
    moderate, biographical, constrained, national).

% Coal-dependent or carbon-intensive member states see their sovereign debt receive less favorable treatment under climate-weighted purchase criteria, raising borrowing costs relative to greener peers, while remaining bound within monetary union with no exit from the shared collateral framework.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__climate_incorporation, member_states_with_high_carbon_sovereign_debt, payer,
    powerful, generational, trapped, national).

% Implement Eurosystem collateral rules domestically and absorb political pressure from national fossil-linked industries and finance ministries, while having limited independent authority to deviate from Governing Council-set criteria.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__climate_incorporation, national_central_banks, observer,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(ecb_mandate_article_127__climate_incorporation, national_central_banks, agenda_setter).

% Economists and former central bankers who read Article 127(1) as exclusively about price stability argue climate tilting exceeds mandate and risks politicizing monetary policy and eroding independence. Their objections surface in academic and legal commentary but have not altered Governing Council practice.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__climate_incorporation, price_stability_purists, excluded,
    organized, generational, constrained, continental).

% The Court of Justice of the EU and constitutional scholars evaluate whether Article 11's environmental integration duty operationally binds the ECB's conduct of monetary policy, or whether it is aspirational and non-justiciable relative to Article 127's primary objective. Their eventual rulings could validate, narrow, or invalidate the climate-incorporation reading.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__climate_incorporation, eu_courts_and_legal_scholars, observer,
    institutional, civilizational, analytical, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(ecb_mandate_article_127__climate_incorporation, diffuse).
narrative_ontology:fixing_cost_class(ecb_mandate_article_127__climate_incorporation, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the Eurosystem's balance sheet operations with the EU's collective climate transition commitments, internalizing a systemic risk (stranded carbon assets degrading collateral value) that individual credit assessments were slow to price, while aligning with a textual EU treaty obligation that applies across institutions.
% TRANSFER_FUNCTION: Moves financing-cost advantage from carbon-intensive issuers and carbon-exposed member states to green-labeled issuers and transition-aligned industries, via differential collateral haircuts and asset-purchase eligibility criteria administered through the ECB's own operational discretion.
% ABSENT_VOICES: Workers and regions economically dependent on carbon-intensive industry (coal regions, legacy manufacturing towns) are not seated in either the Governing Council's technical deliberations or the climate-policy apparatus's advocacy; their transition costs are externalized to fiscal policy debates the ECB does not participate in. Price-stability purists are heard in commentary but have not altered practice.
% DISAPPEARANCE_RATIONALE: If climate-weighted collateral and purchase criteria were withdrawn overnight, fossil fuel and carbon-intensive issuers would see funding costs fall relative to green issuers, high-carbon sovereign spreads would compress, and the EU climate apparatus would lose a significant non-legislative enforcement channel for Green Deal financing objectives — a real reallocation of capital costs would reverse.
% FOUNDING_PROBLEM: Two intertwined problems: (1) climate change poses a systemic financial-stability risk that traditional collateral valuation was mispricing (stranded-asset risk), and (2) Article 11 TFEU requires environmental protection requirements to be integrated into the definition and implementation of all Union policies, arguably including ECB conduct, creating treaty pressure independent of any monetary-stability rationale.
% FOUNDING_PROBLEM_CORROBORATION: The ECB's own climate-related risk reports and the European Commission's Green Deal communications attest the stranded-asset risk and treaty-integration problems are live. Independent legal scholars and several former Bundesbank and Bank of Finland officials, outside the ECB's own advocacy, dispute whether Article 11 creates an operationally binding duty on the ECB specifically versus a general interpretive principle for EU legislative acts — this dispute has not been resolved by the CJEU.
narrative_ontology:disappearance_verdict(ecb_mandate_article_127__climate_incorporation, world_rearranges).
narrative_ontology:founding_problem_status(ecb_mandate_article_127__climate_incorporation, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ecb_mandate_article_127__climate_incorporation, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(ecb_mandate_article_127__climate_incorporation, 'none', 1).
narrative_ontology:epsilon_provenance(ecb_mandate_article_127__climate_incorporation, 0.52, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness (0.52) reflects a genuine, growing transfer from carbon-intensive issuers to green-favored issuers through collateral and purchase mechanics, not merely differential market pricing — this differential is administered by the ECB's own discretionary criteria. It rose over the interval (0.22 to 0.52) as climate risk integration moved from disclosure requirements to binding eligibility and haircut mechanics. Suppression (0.58) captures the structural lock-in: carbon-intensive issuers and high-carbon sovereigns cannot exit the eurozone collateral system, and the tilting mechanism is enforced uniformly by the Eurosystem regardless of issuer objection. Theater ratio is modest (0.28) because the risk being priced (stranded-asset exposure) is empirically real, not merely performative, though some of the tilting's political function (satisfying the EU climate apparatus's demand for ECB alignment) exceeds what pure collateral-risk management alone would justify. Accessibility collapse is moderate (0.38): alternative financing channels (private markets, non-euro debt) exist for large issuers but are foreclosing for smaller carbon-intensive manufacturers and constrained for member states. Resistance is substantial (0.61) — price-stability purists, industry lobbies, and some member states actively contest the mandate reading in legal and political fora.
 *
 * DIRECTIONALITY LOGIC:
 *   Climate transition industries and green bond issuers sit near the beneficiary end: the tilting mechanism directly subsidizes their cost of capital without their having built or administered the mechanism. The ECB Governing Council and the EU climate policy apparatus are dual beneficiary/agenda-setter seats — the Council administers the criteria and bears no direct cost, while the climate apparatus gains a policy-enforcement channel it did not need to legislate. Fossil fuel issuers, carbon-intensive manufacturers, and high-carbon member states sit near the target end: the same criteria that subsidize green issuers directly raise their funding costs, and their exit options range from constrained (manufacturers, able to shift some financing to non-euro or private channels at cost) to trapped (member states, bound within monetary union with no exit from shared collateral rules).
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled_rope classification under this reading prevents two mislabeling errors. First, it prevents flattening the arrangement into a pure snare, which would ignore the real coordination function: stranded-asset risk is a genuine threat to collateral integrity that traditional risk models underweighted, and Article 11 is a real treaty text, not a fabricated pretext. Second, it prevents flattening the arrangement into a pure rope, which would ignore that identifiable parties (fossil issuers, carbon-intensive manufacturers, high-carbon sovereigns) bear concentrated, non-consensual costs through a mechanism they cannot exit and did not design. The founding_problem_status is authored as 'contested' rather than resolved in either direction — this is deliberate: whether Article 11 creates an operationally binding duty on the ECB specifically remains legally unsettled, and the mandatrophy question (has climate incorporation become an entrenched policy lever independent of its original prudential justification) cannot be resolved by this story alone; it depends on the sibling readings and eventual CJEU adjudication.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    article_11_operational_bindingness,
    'Does Article 11 TFEU''s environmental integration clause create an operationally binding duty on the ECB''s conduct of monetary policy specifically, or is it a general interpretive principle addressed to EU legislative and executive acts that does not extend to independent central bank operations?',
    'A definitive CJEU ruling on the scope of Article 11 as applied to ECB collateral and purchase-program decisions; absent that, accumulated legal scholarship consensus or a preliminary reference case.',
    'If operationally binding, climate incorporation is treaty-compelled and the coordination function is legally mandatory, strengthening the tangled_rope reading against snare reclassification. If merely interpretive/aspirational, the climate tilting rests on ECB discretionary choice alone, weakening the treaty-compliance leg of the coordination claim and shifting the balance toward the orthodox_price_stability reading''s characterization of the same facts as ultra vires extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(article_11_operational_bindingness, conceptual, 'Whether Article 11 TFEU binds the ECB''s monetary operations or only EU legislative acts generally.').

omega_variable(
    stranded_asset_risk_magnitude,
    'How large is the actual stranded-asset risk to ECB collateral value from carbon-intensive issuers, independent of the treaty-compliance rationale?',
    'Independent quantitative climate-risk stress testing of Eurosystem collateral pools, compared against the magnitude of financing-cost differential actually imposed on carbon-intensive issuers.',
    'If the prudential risk is small relative to the financing-cost differential imposed, the arrangement leans more extractive than coordinating (supporting a heavier weighting toward snare); if the risk is large and well-matched to the differential, the coordination function is substantiated and the tangled_rope balance holds.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(stranded_asset_risk_magnitude, empirical, 'Whether the prudential risk justification is proportionate to the extraction it authorizes.').

omega_variable(
    cross_reading_framing_choice,
    'Is the ECB''s climate-weighted collateral practice better understood as a distinct constraint (this reading) or as a single fact pattern whose classification depends entirely on which of the three kernel readings an observer adopts?',
    'Track whether ECB internal communications, Commission statements, and judicial reasoning converge on treating Article 11 as doing independent work (supporting decomposition into separate readings) versus treating the climate tilting as fully explained by ordinary discretionary balancing (collapsing this reading into expansive_secondary_objectives).',
    'If convergence occurs toward the discretionary-balancing account, this reading''s distinctiveness (and its higher extraction/suppression profile relative to plain discretionary balancing) becomes harder to sustain as a separate constraint from expansive_secondary_objectives; if Article 11 is treated as doing independent legal work, the three-way kernel decomposition remains structurally justified.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cross_reading_framing_choice, conceptual, 'Whether the climate_incorporation reading is a genuinely distinct constraint or collapses into the discretionary-balancing reading under sufficient scrutiny.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ecb_mandate_article_127__climate_incorporation, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ecb__tr_t0, ecb_mandate_article_127__climate_incorporation, theater_ratio, 0, 0.15).
narrative_ontology:measurement(ecb__tr_t4, ecb_mandate_article_127__climate_incorporation, theater_ratio, 4, 0.18).
narrative_ontology:measurement(ecb__tr_t8, ecb_mandate_article_127__climate_incorporation, theater_ratio, 8, 0.2).
narrative_ontology:measurement(ecb__tr_t12, ecb_mandate_article_127__climate_incorporation, theater_ratio, 12, 0.23).
narrative_ontology:measurement(ecb__tr_t16, ecb_mandate_article_127__climate_incorporation, theater_ratio, 16, 0.25).
narrative_ontology:measurement(ecb__tr_t20, ecb_mandate_article_127__climate_incorporation, theater_ratio, 20, 0.27).
narrative_ontology:measurement(ecb__tr_t24, ecb_mandate_article_127__climate_incorporation, theater_ratio, 24, 0.28).

% Extraction over time
narrative_ontology:measurement(ecb__be_t0, ecb_mandate_article_127__climate_incorporation, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(ecb__be_t4, ecb_mandate_article_127__climate_incorporation, base_extractiveness, 4, 0.31).
narrative_ontology:measurement(ecb__be_t8, ecb_mandate_article_127__climate_incorporation, base_extractiveness, 8, 0.38).
narrative_ontology:measurement(ecb__be_t12, ecb_mandate_article_127__climate_incorporation, base_extractiveness, 12, 0.44).
narrative_ontology:measurement(ecb__be_t16, ecb_mandate_article_127__climate_incorporation, base_extractiveness, 16, 0.48).
narrative_ontology:measurement(ecb__be_t20, ecb_mandate_article_127__climate_incorporation, base_extractiveness, 20, 0.5).
narrative_ontology:measurement(ecb__be_t24, ecb_mandate_article_127__climate_incorporation, base_extractiveness, 24, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(ecb__su_t0, ecb_mandate_article_127__climate_incorporation, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(ecb__su_t4, ecb_mandate_article_127__climate_incorporation, suppression_requirement, 4, 0.36).
narrative_ontology:measurement(ecb__su_t8, ecb_mandate_article_127__climate_incorporation, suppression_requirement, 8, 0.42).
narrative_ontology:measurement(ecb__su_t12, ecb_mandate_article_127__climate_incorporation, suppression_requirement, 12, 0.48).
narrative_ontology:measurement(ecb__su_t16, ecb_mandate_article_127__climate_incorporation, suppression_requirement, 16, 0.53).
narrative_ontology:measurement(ecb__su_t20, ecb_mandate_article_127__climate_incorporation, suppression_requirement, 20, 0.56).
narrative_ontology:measurement(ecb__su_t24, ecb_mandate_article_127__climate_incorporation, suppression_requirement, 24, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ecb_mandate_article_127__climate_incorporation, resource_allocation).
narrative_ontology:boltzmann_floor_override(ecb_mandate_article_127__climate_incorporation, 0.12).
narrative_ontology:affects_constraint(ecb_mandate_article_127__climate_incorporation, ecb_mandate_article_127__orthodox_price_stability).
narrative_ontology:affects_constraint(ecb_mandate_article_127__climate_incorporation, ecb_mandate_article_127__expansive_secondary_objectives).

% DUAL FORMULATION NOTE:
% This story is one of three sibling constraints decomposing the natural-language 'ECB mandate under Article 127' claim, per the ε-invariance principle. orthodox_price_stability treats climate/secondary considerations as non-operational (low ε, near-mountain from the ECB's own textualist seat); expansive_secondary_objectives treats climate tilting as ordinary discretionary balancing under the 'without prejudice' clause without specific Article 11 anchoring (moderate ε, rope-leaning); climate_incorporation (this story) treats Article 11 TFEU as doing independent binding work, producing a higher, treaty-compulsion-flavored ε and a tangled_rope classification. All three share the same underlying operational facts (collateral haircuts, purchase eligibility criteria) but assign different legal/structural readings to them, yielding genuinely different ε values, beneficiary/victim sets, and classifications — not the same constraint measured three ways.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
