% ============================================================================
% CONSTRAINT STORY: ecb_mandate_article_127__climate_incorporation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
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
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: ecb_mandate_article_127__climate_incorporation
 *   human_readable: ECB Climate Risk Integration Mandate under Article 127 TFEU
 *   domain: monetary_policy/constitutional_law/eu_institutional_governance
 *
 * SUMMARY:
 *   The ECB's integration of climate risk into monetary policy operations
 *   represents a contested reading of the Article 127 TFEU mandate. The
 *   climate_incorporation reading holds that the Treaty's environmental
 *   integration clause (Article 11 TFEU) and the systemic risk nature of
 *   climate change require operational integration into asset purchases
 *   (CSPP, PEPP) and collateral frameworks. This reading creates
 *   beneficiaries in climate transition sectors and the EU green bond market,
 *   while extracting from fossil fuel and carbon-intensive industries through
 *   collateral haircuts and purchase exclusions. The constraint operates as a
 *   tangled rope: genuine coordination function (financial stability, Treaty
 *   compliance) combined with asymmetric extraction (sectoral redistribution
 *   via central bank balance sheet). Active enforcement required through
 *   continuous framework calibration, disclosure requirements, and portfolio
 *   tilting.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ecb_mandate_article_127__climate_incorporation, 0.48).
domain_priors:suppression_score(ecb_mandate_article_127__climate_incorporation, 0.62).
domain_priors:theater_ratio(ecb_mandate_article_127__climate_incorporation, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ecb_mandate_article_127__climate_incorporation, extractiveness, 0.48).
narrative_ontology:constraint_metric(ecb_mandate_article_127__climate_incorporation, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(ecb_mandate_article_127__climate_incorporation, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ecb_mandate_article_127__climate_incorporation, accessibility_collapse, 0.52).
narrative_ontology:constraint_metric(ecb_mandate_article_127__climate_incorporation, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ecb_mandate_article_127__climate_incorporation, tangled_rope).
narrative_ontology:human_readable(ecb_mandate_article_127__climate_incorporation, "ECB Climate Risk Integration Mandate under Article 127 TFEU").
narrative_ontology:topic_domain(ecb_mandate_article_127__climate_incorporation, "monetary_policy/constitutional_law/eu_institutional_governance").

domain_priors:requires_active_enforcement(ecb_mandate_article_127__climate_incorporation).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ecb_mandate_article_127__climate_incorporation, '0a0b70c7-8db7-4fad-be72-90797721fb4b').
narrative_ontology:cs_kernel_codification('0a0b70c7-8db7-4fad-be72-90797721fb4b', formalized).
narrative_ontology:cs_authority_grounding('0a0b70c7-8db7-4fad-be72-90797721fb4b', lineage).
narrative_ontology:cs_interpretation_layer_present('0a0b70c7-8db7-4fad-be72-90797721fb4b').
narrative_ontology:cs_reading_relation('0a0b70c7-8db7-4fad-be72-90797721fb4b', ecb_mandate_article_127__orthodox_price_stability, forecloses).
narrative_ontology:cs_reading_relation('0a0b70c7-8db7-4fad-be72-90797721fb4b', ecb_mandate_article_127__expansive_secondary_objectives, coexists_with).
narrative_ontology:cs_axiom('0a0b70c7-8db7-4fad-be72-90797721fb4b', foundational, climate_change_systemic_financial_risk_mandatory_integration).
narrative_ontology:cs_axiom_status(climate_change_systemic_financial_risk_mandatory_integration, holdable).
narrative_ontology:cs_axiom_grounding('0a0b70c7-8db7-4fad-be72-90797721fb4b', climate_change_systemic_financial_risk_mandatory_integration, empirically_contingent).
narrative_ontology:cs_axiom('0a0b70c7-8db7-4fad-be72-90797721fb4b', foundational, article_11_tfeu_binds_monetary_policy_operations).
narrative_ontology:cs_axiom_status(article_11_tfeu_binds_monetary_policy_operations, holdable).
narrative_ontology:cs_axiom_grounding('0a0b70c7-8db7-4fad-be72-90797721fb4b', article_11_tfeu_binds_monetary_policy_operations, conventional).
narrative_ontology:cs_reference_frame('0a0b70c7-8db7-4fad-be72-90797721fb4b', price_stability_exclusive_mandate).
narrative_ontology:cs_drift_state('0a0b70c7-8db7-4fad-be72-90797721fb4b', post_paris_agreement_nfgs_formation, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('0a0b70c7-8db7-4fad-be72-90797721fb4b', '').
narrative_ontology:cs_kernel_id(ecb_mandate_article_127__climate_incorporation, ecb_mandate_article_127).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ecb_mandate_article_127__climate_incorporation, climate_transition_sectors).
narrative_ontology:constraint_beneficiary(ecb_mandate_article_127__climate_incorporation, eu_green_bond_market).
narrative_ontology:constraint_beneficiary(ecb_mandate_article_127__climate_incorporation, financial_stability_authorities).
narrative_ontology:constraint_victim(ecb_mandate_article_127__climate_incorporation, fossil_fuel_energy_sector).
narrative_ontology:constraint_victim(ecb_mandate_article_127__climate_incorporation, carbon_intensive_industry).
narrative_ontology:constraint_victim(ecb_mandate_article_127__climate_incorporation, traditional_banking_collateral_models).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(ecb_mandate_article_127__climate_incorporation, national_central_banks).
narrative_ontology:constraint_beneficiary(ecb_mandate_article_127__climate_incorporation, climate_vulnerable_member_states).
narrative_ontology:constraint_vindicates(ecb_mandate_article_127__climate_incorporation, article_11_tfeu_environmental_integration).
narrative_ontology:constraint_vindicates(ecb_mandate_article_127__climate_incorporation, ecb_mandate_evolutionary_interpretation).
narrative_ontology:constraint_vindicates(ecb_mandate_article_127__climate_incorporation, climate_change_systemic_financial_risk).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets monetary policy and collateral frameworks for the Eurosystem. Interprets Article 127 TFEU as requiring climate risk integration into asset purchases and collateral eligibility. Administers the Corporate Sector Purchase Programme (CSPP) and collateral frameworks with climate tilting. Authority derives from Treaty mandate and EU legislative framework.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__climate_incorporation, ecb_governing_council, agenda_setter,
    institutional, generational, analytical, continental).

% Implement ECB monetary policy operations at national level. Execute asset purchases and collateral management under ECB guidelines. Benefit from institutional coherence and expanded mandate legitimacy. Constrained by Eurosystem discipline and national political accountability.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__climate_incorporation, national_central_banks, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(ecb_mandate_article_127__climate_incorporation, national_central_banks, beneficiary).

% Renewable energy, energy efficiency, clean transport, and green technology companies. Receive preferential access to ECB asset purchases and lower collateral haircuts. Gain cheaper financing and implicit central bank backing for transition investments. Can access alternative green finance channels if ECB support shifts.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__climate_incorporation, climate_transition_sectors, beneficiary,
    organized, biographical, mobile, continental).

% European green bond issuers and investors. Benefit from ECB purchase programmes targeting green bonds and climate-related disclosure standards. Market depth and pricing improve from central bank demand. Can access private market alternatives if ECB purchases reduce.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__climate_incorporation, eu_green_bond_market, beneficiary,
    organized, biographical, mobile, continental).

% ESRB, national macroprudential authorities, and banking supervisors. Gain regulatory alignment between monetary policy and climate risk management. Climate stress testing and disclosure requirements reinforced by ECB collateral framework. Institutional mandate coherence strengthened.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__climate_incorporation, financial_stability_authorities, beneficiary,
    institutional, generational, analytical, continental).

% Oil, gas, coal, and conventional power generation companies. Face higher collateral haircuts, exclusion from CSPP eligibility, and rising financing costs from portfolio tilting. Assets risk stranding from both policy and market forces. Limited exit: core business model incompatible with transition, diversification slow and capital-intensive.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__climate_incorporation, fossil_fuel_energy_sector, payer,
    powerful, biographical, constrained, continental).

% Steel, cement, chemicals, aviation, and heavy manufacturing. Face collateral discrimination and higher financing costs. Transition technologies exist but require massive capital expenditure with uncertain returns. Exit constrained by asset specificity, competitive pressure, and shareholder expectations.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__climate_incorporation, carbon_intensive_industry, payer,
    organized, biographical, constrained, continental).

% Banks with loan portfolios heavy in carbon-intensive sectors. Collateral values decline as ECB haircuts increase for brown assets. Balance sheet impact transmitted to lending capacity. Must reallocate credit toward green assets while managing legacy exposures. Constrained by regulatory capital requirements and depositor base.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__climate_incorporation, traditional_banking_collateral_models, payer,
    organized, biographical, constrained, continental).

% Adjudicates legal challenges to ECB climate mandate interpretation. Reviews whether Article 127 TFEU and Article 11 TFEU environmental integration clause authorize operational climate integration. Precedent from PSPP and OMT cases establishes judicial deference to monetary policy discretion within Treaty limits.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__climate_incorporation, european_court_of_justice, observer,
    institutional, generational, analytical, continental).

% Democratic oversight of ECB through monetary dialogue and appointment hearings. Generally supportive of climate mandate integration. Can influence mandate interpretation through political pressure and legislative initiatives (e.g., EU Taxonomy, CSRD). Accountability to electorates creates pressure for green transition.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__climate_incorporation, european_parliament, observer,
    institutional, biographical, analytical, continental).
narrative_ontology:stakeholder_secondary_role(ecb_mandate_article_127__climate_incorporation, european_parliament, agenda_setter).

% German Bundesverfassungsgericht and peers. Review ECB acts for ultra vires and proportionality under national constitutional identity. PSPP ruling established precedent for substantive review. Climate mandate integration may trigger new challenges on mandate creep and democratic legitimacy.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__climate_incorporation, national_constitutional_courts, observer,
    institutional, generational, analytical, national).

% Southern and Eastern EU members disproportionately exposed to physical climate risks. Benefit from ECB climate action reducing systemic risk and supporting transition finance. Limited fiscal capacity for own transition; depend on EU-level monetary and fiscal coordination. Exit constrained by euro membership and institutional lock-in.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__climate_incorporation, climate_vulnerable_member_states, beneficiary,
    moderate, generational, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Aligns monetary policy operations with EU climate policy objectives under Article 11 TFEU, integrating climate risk into financial stability framework and directing capital toward transition through central bank balance sheet operations.
% TRANSFER_FUNCTION: Moves preferential financing access and lower collateral requirements from carbon-intensive sectors to climate transition sectors via ECB asset purchase programmes and collateral framework tilting; transfers risk from transition beneficiaries to fossil fuel and carbon-intensive payers through portfolio reallocation.
% ABSENT_VOICES: Households and SMEs in carbon-intensive regions facing transition costs without direct representation in ECB governance; developing country exporters of fossil fuels affected by EU financial regulation extraterritoriality; future generations bearing climate risk not represented in current institutional mandate.
% DISAPPEARANCE_RATIONALE: If climate integration vanished overnight, ECB would revert to carbon-neutral collateral and purchase frameworks; fossil fuel sectors would regain preferential central bank access; green bond premium would compress; financial stability authorities would lose regulatory alignment; EU climate policy would lose monetary policy pillar; transition financing costs would rise significantly.
% FOUNDING_PROBLEM: Post-Paris Agreement recognition that climate change constitutes systemic financial risk requiring central bank response; EU Treaty obligation under Article 11 TFEU to integrate environmental protection into all policies; ECB mandate evolution from narrow price stability to broader sustainability within Treaty framework.
% FOUNDING_PROBLEM_CORROBORATION: IPCC and NGFS reports corroborate climate systemic risk; European Court of Auditors and academic studies confirm financial stability rationale; fossil fuel industry and some national courts contest the operational scope; corroboration comes from outside beneficiary set via NGFS (central bank network) and independent financial stability research.
narrative_ontology:disappearance_verdict(ecb_mandate_article_127__climate_incorporation, world_rearranges).
narrative_ontology:founding_problem_status(ecb_mandate_article_127__climate_incorporation, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ecb_mandate_article_127__climate_incorporation, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(ecb_mandate_article_127__climate_incorporation, 'none', 1).
narrative_ontology:epsilon_provenance(ecb_mandate_article_127__climate_incorporation, 0.48, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Extractiveness (0.48) reflects significant but not total redistribution: climate tilting affects marginal allocation of ECB balance sheet, not entire financial system. Suppression (0.62) captures the structural exclusion of brown assets from central bank eligibility and the difficulty of exiting the framework for carbon-intensive sectors. Theater ratio (0.28) acknowledges genuine financial stability rationale alongside growing performative elements in disclosure and reporting. Accessibility collapse (0.52) and resistance (0.45) reflect contested legitimacy: alternatives exist (carbon-neutral frameworks) but Treaty interpretation creates path dependency. Measurement series shows accelerating extraction and suppression from 2015 Paris Agreement through 2030 strategic horizon.
 *
 * PERSPECTIVAL GAP:
 *   From agenda_setter seat (ECB), constraint appears as necessary coordination: Treaty compliance, financial stability, mandate fulfillment. From payer seats (fossil fuel, carbon-intensive), constraint appears as extraction: regulatory discrimination, stranded asset risk, competitive disadvantage imposed by monetary authority. From beneficiary seats (green sectors), constraint appears as legitimate support: leveling playing field, correcting market failure. The engine computes this divergence from power/exit/role structure — the claimed_type (tangled_rope) acknowledges both coordination and extraction as structurally real.
 *
 * DIRECTIONALITY LOGIC:
 *   ECB Governing Council and NCBs as agenda_setters sit near beneficiary end (d~0.2): they gain mandate coherence, institutional relevance, and expanded policy tools. Climate transition sectors and green bond market as beneficiaries sit at d~0.15: receive concrete financial advantages with mobile exit. Fossil fuel sector and carbon-intensive industry as payers sit at d~0.85: bear concentrated costs with constrained exit due to asset specificity. Traditional banking collateral models at d~0.7: absorb transmission costs with constrained exit from regulatory framework. Observers (ECJ, EP, constitutional courts) at d~0.5: analytical position with institutional stakes. Climate vulnerable states at d~0.3: net beneficiaries but institutionally constrained.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandate's original price stability focus has not atrophied — climate integration is framed as essential to price stability over medium term (physical/transition risks to inflation). However, the secondary objectives clause ('without prejudice to price stability') is being read as operational authorization rather than subordinate constraint. This creates mandatrophy tension: is climate integration serving price stability (live founding problem) or has it become independent objective (dead founding problem, mandate creep)? Corroboration from NGFS and financial stability research supports live status; constitutional court challenges suggest contested status.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    mandate_interpretation_boundary,
    'Does Article 127 TFEU read with Article 11 TFEU require operational climate integration, or merely permit it as discretionary consideration?',
    'ECJ ruling on legal challenge to ECB climate collateral framework or asset purchase criteria; Treaty amendment clarifying mandate scope.',
    'If required, extraction is Treaty-mandated coordination cost; if permitted, extraction is discretionary policy choice — affects legitimacy and classification from payer seats.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mandate_interpretation_boundary, conceptual, 'Whether climate integration is legally compelled or discretionary under Treaty framework').

omega_variable(
    financial_stability_causality,
    'Does climate risk integration into monetary policy operations measurably improve financial stability outcomes, or is the causality unproven?',
    'Counterfactual analysis of financial crises with/without climate tilting; NGFS scenario comparison; academic research on central bank climate policy effectiveness.',
    'If causal, coordination function is empirically grounded; if unproven, extraction lacks coordination justification — shifts classification toward snare from payer perspective.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(financial_stability_causality, empirical, 'Empirical validation of the coordination function''s effectiveness').

omega_variable(
    transition_finance_additionality,
    'Does ECB climate tilting create additional green finance, or merely displace private capital that would have flowed anyway?',
    'Micro-econometric analysis of CSPP green bond purchases vs. counterfactual private demand; ECB purchase impact studies; market microstructure research.',
    'If additional, coordination function creates net benefit; if displacement, extraction transfers rents without net transition acceleration — affects beneficiary seat classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(transition_finance_additionality, empirical, 'Whether central bank climate operations are additional or displacing private finance').

omega_variable(
    kernel_reading_relations,
    'How do the three readings of the ECB Article 127 kernel structurally relate: does climate_incorporation foreclose orthodox_price_stability, coexist with expansive_secondary_objectives, or influence both?',
    'Institutional practice: can ECB simultaneously hold orthodox and climate readings? Legal doctrine: does accepting climate integration logically entail rejecting exclusive price stability focus? Political economy: do climate and expansive readings form coalition against orthodoxy?',
    'Determines cs_structure.reading_relations: forecloses vs coexists_with vs influences. Affects whether kernel constitutes a genuine commitment system with contested but coherent readings, or a fragmenting authority structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_relations, conceptual, 'Structural relationships among sibling readings of the ECB mandate kernel').

omega_variable(
    suppression_mechanism_novelty,
    'Is portfolio tilting a novel suppression mechanism distinct from traditional monetary policy coercion, or merely an extension of existing collateral policy?',
    'Compare suppression intensity and exit blocking of climate haircuts vs. traditional credit quality haircuts; assess whether climate criteria create new exclusion categories or recalibrate existing ones.',
    'If novel, suppression metric captures new structural dynamic; if extension, suppression is continuation of existing framework — affects temporal measurement interpretation and omega resolution.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_novelty, conceptual, 'Whether climate collateral framework constitutes novel suppression mechanism').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ecb_mandate_article_127__climate_incorporation, 2015, 2030).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ecb__tr_t2015, ecb_mandate_article_127__climate_incorporation, theater_ratio, 2015, 0.05).
narrative_ontology:measurement(ecb__tr_t2018, ecb_mandate_article_127__climate_incorporation, theater_ratio, 2018, 0.08).
narrative_ontology:measurement(ecb__tr_t2021, ecb_mandate_article_127__climate_incorporation, theater_ratio, 2021, 0.15).
narrative_ontology:measurement(ecb__tr_t2024, ecb_mandate_article_127__climate_incorporation, theater_ratio, 2024, 0.22).
narrative_ontology:measurement(ecb__tr_t2027, ecb_mandate_article_127__climate_incorporation, theater_ratio, 2027, 0.3).
narrative_ontology:measurement(ecb__tr_t2030, ecb_mandate_article_127__climate_incorporation, theater_ratio, 2030, 0.38).

% Extraction over time
narrative_ontology:measurement(ecb__be_t2015, ecb_mandate_article_127__climate_incorporation, base_extractiveness, 2015, 0.05).
narrative_ontology:measurement(ecb__be_t2018, ecb_mandate_article_127__climate_incorporation, base_extractiveness, 2018, 0.12).
narrative_ontology:measurement(ecb__be_t2021, ecb_mandate_article_127__climate_incorporation, base_extractiveness, 2021, 0.28).
narrative_ontology:measurement(ecb__be_t2024, ecb_mandate_article_127__climate_incorporation, base_extractiveness, 2024, 0.42).
narrative_ontology:measurement(ecb__be_t2027, ecb_mandate_article_127__climate_incorporation, base_extractiveness, 2027, 0.55).
narrative_ontology:measurement(ecb__be_t2030, ecb_mandate_article_127__climate_incorporation, base_extractiveness, 2030, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(ecb__su_t2015, ecb_mandate_article_127__climate_incorporation, suppression_requirement, 2015, 0.1).
narrative_ontology:measurement(ecb__su_t2018, ecb_mandate_article_127__climate_incorporation, suppression_requirement, 2018, 0.25).
narrative_ontology:measurement(ecb__su_t2021, ecb_mandate_article_127__climate_incorporation, suppression_requirement, 2021, 0.45).
narrative_ontology:measurement(ecb__su_t2024, ecb_mandate_article_127__climate_incorporation, suppression_requirement, 2024, 0.58).
narrative_ontology:measurement(ecb__su_t2027, ecb_mandate_article_127__climate_incorporation, suppression_requirement, 2027, 0.68).
narrative_ontology:measurement(ecb__su_t2030, ecb_mandate_article_127__climate_incorporation, suppression_requirement, 2030, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ecb_mandate_article_127__climate_incorporation, resource_allocation).
narrative_ontology:boltzmann_floor_override(ecb_mandate_article_127__climate_incorporation, 0.18).
narrative_ontology:affects_constraint(ecb_mandate_article_127__climate_incorporation, eu_taxonomy_regulation).
narrative_ontology:affects_constraint(ecb_mandate_article_127__climate_incorporation, corporate_sustainability_reporting_directive).
narrative_ontology:affects_constraint(ecb_mandate_article_127__climate_incorporation, ecb_banking_supervision_climate_stress_tests).
narrative_ontology:affects_constraint(ecb_mandate_article_127__climate_incorporation, european_green_bond_standard).
narrative_ontology:affects_constraint(ecb_mandate_article_127__climate_incorporation, eu_ets_carbon_pricing).

% DUAL FORMULATION NOTE:
% Part of ecb_mandate_article_127 kernel family with orthodox_price_stability and expansive_secondary_objectives readings. This reading operationalizes Article 11 TFEU environmental integration clause; orthodox reading treats Article 11 as non-operational for monetary policy; expansive reading treats secondary objectives as discretionary balancing. All three share kernel but instantiate different constraints with distinct ε and beneficiary/victim structures.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(ecb_mandate_article_127__climate_incorporation, institutional, 0.15).
constraint_indexing:directionality_override(ecb_mandate_article_127__climate_incorporation, powerful, 0.85).
constraint_indexing:directionality_override(ecb_mandate_article_127__climate_incorporation, organized, 0.75).
constraint_indexing:directionality_override(ecb_mandate_article_127__climate_incorporation, moderate, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
