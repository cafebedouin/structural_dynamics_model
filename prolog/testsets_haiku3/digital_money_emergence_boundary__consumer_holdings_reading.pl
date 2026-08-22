% ============================================================================
% CONSTRAINT STORY: digital_money_emergence_boundary__consumer_holdings_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_digital_money_emergence_boundary__consumer_holdings_reading, []).

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
 *   constraint_id: digital_money_emergence_boundary__consumer_holdings_reading
 *   human_readable: Digital Money Consumer Holdings Boundary (EMD/E-Money Reading)
 *   domain: monetary/financial_governance/technology
 *
 * SUMMARY:
 *   The consumer-holdings reading of digital money emergence places the
 *   boundary at the moment when individuals could directly hold and transact
 *   with digital instruments outside traditional banking infrastructure —
 *   marked by 1990s e-purses, formalized by the 2000 EMD Directive in Europe,
 *   and reinforced by regulatory frameworks worldwide. This reading
 *   instantiates one specific constraint: the claim that 'money' is defined
 *   by individual custody and transactability. Regulatory bodies benefit from
 *   measurement clarity; fintech issuers gain legitimacy and market access;
 *   traditional banks lose intermediation volume; informal value transfer
 *   networks lose legal standing. The constraint organizes monetary policy
 *   measurement (M4/M5 distinction) and fintech oversight.
 *
 * KEY AGENTS:
 *   - Regulatory bodies (ECB, national central banks, financial authorities) — agenda setters; benefit from definitional clarity and monetary control
 *   - Fintech issuers (PayPal, Revolut, digital wallet operators) — beneficiaries + payers; gain institutional legitimacy but bear compliance costs
 *   - Consumers — beneficiaries + diffuse payers; gain payment speed and service access, bear counterparty risk
 *   - Traditional banking sector — payers; lose payment intermediation rents through regulatory arbitrage
 *   - Informal value transfer networks — payers; face formalization pressure and exclusion from legitimacy
 *   - Unbanked populations — beneficiaries + payers; gain potential financial inclusion but bear platform dependence
 *   - Technology standards bodies — excluded; would contest regulatory definition of 'money' but have no seat in monetary policy
 *   - Financial analysts — observers; document divergence between regulatory categories and market behavior
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(digital_money_emergence_boundary__consumer_holdings_reading, 0.61).
domain_priors:suppression_score(digital_money_emergence_boundary__consumer_holdings_reading, 0.48).
domain_priors:theater_ratio(digital_money_emergence_boundary__consumer_holdings_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(digital_money_emergence_boundary__consumer_holdings_reading, extractiveness, 0.61).
narrative_ontology:constraint_metric(digital_money_emergence_boundary__consumer_holdings_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(digital_money_emergence_boundary__consumer_holdings_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(digital_money_emergence_boundary__consumer_holdings_reading, accessibility_collapse, 0.71).
narrative_ontology:constraint_metric(digital_money_emergence_boundary__consumer_holdings_reading, resistance, 0.54).

% --- Constraint claim ---
narrative_ontology:constraint_claim(digital_money_emergence_boundary__consumer_holdings_reading, tangled_rope).
narrative_ontology:human_readable(digital_money_emergence_boundary__consumer_holdings_reading, "Digital Money Consumer Holdings Boundary (EMD/E-Money Reading)").
narrative_ontology:topic_domain(digital_money_emergence_boundary__consumer_holdings_reading, "monetary/financial_governance/technology").

domain_priors:requires_active_enforcement(digital_money_emergence_boundary__consumer_holdings_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(digital_money_emergence_boundary__consumer_holdings_reading, '50068f06-d2c8-4e89-9960-3d42cb2646ec').
narrative_ontology:cs_kernel_codification('50068f06-d2c8-4e89-9960-3d42cb2646ec', formalized).
narrative_ontology:cs_authority_grounding('50068f06-d2c8-4e89-9960-3d42cb2646ec', extraction).
narrative_ontology:cs_interpretation_layer_present('50068f06-d2c8-4e89-9960-3d42cb2646ec').
narrative_ontology:cs_reading_relation('50068f06-d2c8-4e89-9960-3d42cb2646ec', digital_money_emergence_boundary__conceptualization_reading, forecloses).
narrative_ontology:cs_reading_relation('50068f06-d2c8-4e89-9960-3d42cb2646ec', digital_money_emergence_boundary__infrastructure_reading, influences).
narrative_ontology:cs_axiom('50068f06-d2c8-4e89-9960-3d42cb2646ec', foundational, individual_custody_is_definitional).
narrative_ontology:cs_axiom_status(individual_custody_is_definitional, holdable).
narrative_ontology:cs_axiom_grounding('50068f06-d2c8-4e89-9960-3d42cb2646ec', individual_custody_is_definitional, conventional).
narrative_ontology:cs_axiom('50068f06-d2c8-4e89-9960-3d42cb2646ec', foundational, regulatory_classification_determines_monetary_status).
narrative_ontology:cs_axiom_status(regulatory_classification_determines_monetary_status, holdable).
narrative_ontology:cs_axiom_grounding('50068f06-d2c8-4e89-9960-3d42cb2646ec', regulatory_classification_determines_monetary_status, deontological).
narrative_ontology:cs_axiom('50068f06-d2c8-4e89-9960-3d42cb2646ec', secondary, informal_networks_are_non_monetary).
narrative_ontology:cs_axiom_status(informal_networks_are_non_monetary, holdable).
narrative_ontology:cs_axiom_grounding('50068f06-d2c8-4e89-9960-3d42cb2646ec', informal_networks_are_non_monetary, conventional).
narrative_ontology:cs_reference_frame('50068f06-d2c8-4e89-9960-3d42cb2646ec', consumer_custody_as_money_criterion).
narrative_ontology:cs_drift_state('50068f06-d2c8-4e89-9960-3d42cb2646ec', contemporary_post_pandemic_digital_acceleration, gap(codification_collapse, substantial, false)).
narrative_ontology:cs_created_at('50068f06-d2c8-4e89-9960-3d42cb2646ec', '2026-06-11T14:32:00Z').
narrative_ontology:cs_kernel_id(digital_money_emergence_boundary__consumer_holdings_reading, digital_money_emergence_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(digital_money_emergence_boundary__consumer_holdings_reading, regulatory_bodies).
narrative_ontology:constraint_beneficiary(digital_money_emergence_boundary__consumer_holdings_reading, fintech_issuers).
narrative_ontology:constraint_victim(digital_money_emergence_boundary__consumer_holdings_reading, traditional_banking_sector).
narrative_ontology:constraint_victim(digital_money_emergence_boundary__consumer_holdings_reading, informal_value_transfer_networks).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(digital_money_emergence_boundary__consumer_holdings_reading, consumers).
narrative_ontology:constraint_beneficiary(digital_money_emergence_boundary__consumer_holdings_reading, central_banks).
narrative_ontology:constraint_beneficiary(digital_money_emergence_boundary__consumer_holdings_reading, unbanked_populations).
narrative_ontology:constraint_victim(digital_money_emergence_boundary__consumer_holdings_reading, fintech_issuers).
narrative_ontology:constraint_victim(digital_money_emergence_boundary__consumer_holdings_reading, unbanked_populations).
narrative_ontology:constraint_vindicates(digital_money_emergence_boundary__consumer_holdings_reading, money_requires_individual_custody).
narrative_ontology:constraint_vindicates(digital_money_emergence_boundary__consumer_holdings_reading, monetary_aggregates_require_refinement).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Central banks and financial regulators (ECB, national authorities) define legal categories for electronic money, set reserve requirements, conduct compliance monitoring, and determine which instruments qualify as 'money' for monetary aggregates. They benefit from clarity in monetary classification and control over money supply measurement. Their agenda-setting power derives from the authority to license, regulate, and define monetary instruments.
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__consumer_holdings_reading, regulatory_bodies, agenda_setter,
    institutional, generational, analytical, national).

% E-money providers (PayPal, Revolut, Square Cash, digital wallet operators) gain access to consumer funding pools and payment infrastructure legitimacy through regulatory classification as money-like instruments. They benefit from the boundary definition by gaining institutional credibility, but bear compliance costs and capital reserve requirements. Their position enables market entry but constrains operational freedom through regulation.
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__consumer_holdings_reading, fintech_issuers, beneficiary,
    powerful, biographical, mobile, global).
narrative_ontology:stakeholder_secondary_role(digital_money_emergence_boundary__consumer_holdings_reading, fintech_issuers, payer).

% Individual account holders in e-money systems gain instantaneous peer-to-peer transfer capability, multi-currency flexibility, and access to financial services without traditional bank intermediation. They benefit from service innovation and speed. They also bear counterparty risk (issuer insolvency) and regulatory uncertainty as the boundary definition remains contested and enforcement evolves.
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__consumer_holdings_reading, consumers, beneficiary,
    organized, biographical, mobile, global).

% Commercial and savings banks face regulatory arbitrage pressure: e-money instruments can perform payment settlement functions without triggering the same capital reserve requirements, deposit insurance obligations, or regulatory overhead as traditional deposits. They lose intermediation rents as e-money absorbs payment flows, but are constrained from exiting by existing customer relationships and cannot exit the regulatory framework itself.
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__consumer_holdings_reading, traditional_banking_sector, payer,
    powerful, generational, constrained, global).

% Informal remittance networks, underground banking systems (hawala, hundi), and cash-based value transfer bear the cost of formalization pressure: as e-money becomes the regulatory definition of 'money' and the boundary moves toward consumer-held digital instruments, informal networks face exclusion from legitimacy, legal liability, and competitive displacement. They are structurally unable to exit the constraint or adopt its framework without ceasing to exist.
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__consumer_holdings_reading, informal_value_transfer_networks, payer,
    powerless, immediate, trapped, global).

% Central banks benefit from monetary clarity: the consumer-holdings boundary enables precise measurement of money supply aggregates (M4/M5 distinction), which improves policy transmission. They also gain surveillance capability over financial flows as e-money becomes a classified, regulated instrument. Their benefit is institutional control and measurement clarity.
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__consumer_holdings_reading, central_banks, beneficiary,
    institutional, generational, analytical, national).

% Populations without access to traditional banking (2+ billion globally) gain potential entry to financial services through e-money platforms with lower barriers than bank accounts. They also bear platform dependence (locked into proprietary systems), data extraction, and regulatory uncertainty as the boundary definition and consumer protection rules remain uneven across jurisdictions and issuers.
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__consumer_holdings_reading, unbanked_populations, beneficiary,
    powerless, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(digital_money_emergence_boundary__consumer_holdings_reading, unbanked_populations, payer).

% Technical standards organizations (ISO, IEEE, blockchain consortia) that could propose alternative digital money frameworks are structurally excluded from the boundary definition — which is set by financial regulators, not technical communities. They would argue that the constraint imposes regulatory definitions on technical systems, limiting innovation in how 'money' could be structured, but they have no seat in monetary policy.
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__consumer_holdings_reading, technology_standards_bodies, excluded,
    organized, biographical, constrained, global).

% Academic economists, financial researchers, and policy analysts measure the boundary's impact and track whether the constraint's definitional framework captures the actual behavior of digital instruments. They observe and document divergence between regulatory categories and market reality but do not enforce the boundary.
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__consumer_holdings_reading, financial_analysts, observer,
    analytical, biographical, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(digital_money_emergence_boundary__consumer_holdings_reading, regulatory_bodies).
narrative_ontology:fixing_cost_class(digital_money_emergence_boundary__consumer_holdings_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a unified, measurable category (e-money) that distinguishes consumer-held digital instruments from bank deposits, enabling central banks to track and control monetary aggregates accurately and financial regulators to apply consistent oversight to issuers and their reserve requirements.
% TRANSFER_FUNCTION: Transfers definitional authority and regulatory rents from traditional banking (deposit-taking) to fintech issuers and regulatory bodies. Fintech issuers gain access to consumer funding pools and payment infrastructure legitimacy; banks lose payment intermediation volume; informal networks lose legitimacy. Regulatory bodies gain surveillance and measurement control over previously opaque payment flows.
% ABSENT_VOICES: Technology standards communities are structurally excluded — they would argue that defining 'money' via regulatory category rather than technical function constrains innovation in how digital value transfer could be structured. Informal value transfer operators would object (hawala, underground banking) but are systematically excluded from regulatory conversations. Unbanked populations would contest whether the boundary serves their financial inclusion or merely extracts their data.
% DISAPPEARANCE_RATIONALE: If the consumer-holdings boundary disappeared — if regulatory bodies ceased to distinguish e-money as a separate monetary category — the monetary aggregates system would need reconstruction, fintech issuers would lose regulatory clarity (with cascading impacts on capital availability and consumer trust), informal networks would gain competitive legitimacy, and central banks' ability to measure money supply would degrade. The constraint organizes a substantial portion of modern payment infrastructure; its disappearance would force institutional reorganization.
% FOUNDING_PROBLEM: From the 1990s forward, e-purses and later EMD systems allowed individuals to hold value in digital form outside bank accounts, creating payment instruments that did not fit existing monetary categories (M1, M2, M3). Regulators faced a measurement problem: were these instruments 'money'? Should they count in monetary aggregates? Who should oversee them? The boundary was needed to classify instruments and restore measurement coherence.
% FOUNDING_PROBLEM_CORROBORATION: Central banks and regulators affirm the founding problem remains live: central bank digital currencies (CBDCs) under development across jurisdictions assume the boundary is legitimate and codify it further. Technology firms and academic economists contest whether the problem was solved correctly: they note that the consumer-holdings boundary omits settlement-layer innovations (blockchain, distributed ledgers) that may constitute 'money' differently. Empirical testimony from fintech firms shows they navigate the boundary but argue it is increasingly arbitrary as payment technology converges.
narrative_ontology:disappearance_verdict(digital_money_emergence_boundary__consumer_holdings_reading, world_rearranges).
narrative_ontology:founding_problem_status(digital_money_emergence_boundary__consumer_holdings_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(digital_money_emergence_boundary__consumer_holdings_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(digital_money_emergence_boundary__consumer_holdings_reading, 'none', 1).
narrative_ontology:epsilon_provenance(digital_money_emergence_boundary__consumer_holdings_reading, 0.61, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(digital_money_emergence_boundary__consumer_holdings_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(digital_money_emergence_boundary__consumer_holdings_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(digital_money_emergence_boundary__consumer_holdings_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises from 0.18 (pre-1990, boundary undefined) to 0.61 (2025, boundary established and enforced). The trajectory reflects increasing regulatory capture of the definition: as e-money markets grew and informal networks faced compliance pressure, the constraint's extractive character became visible. The gainer (fintech issuers + regulators) extract definitional authority; the losers (banks, informal networks) bear the cost. Theater is modest (0.22): the coordination function is real — measurement clarity matters for monetary policy — but enforcement increasingly serves boundary maintenance rather than coordination. Suppression is moderate (0.48): the constraint is maintained through licensing requirements, compliance monitoring, and capital rules, not through overt coercion. The measurement grid spans the interval with one shared time axis (every metric at every point): the constraint emerged gradually as e-money adoption spread, not suddenly at a single event.
 *
 * PERSPECTIVAL GAP:
 *   From the regulatory bodies' seat, this is genuine coordination: measurement clarity and standardized oversight solve a real problem (monetary transparency). From fintech issuers' seat, it is a coordination function that extracts rents through compliance costs and capital requirements they must pass to consumers. From traditional banks' seat, it is pure extraction — their deposits are regulated more heavily, their capital requirements higher, for performing the same payment and custody functions. From informal networks' seat, it is coercive formalization. The engine computes these divergences from the structural data: same constraint, different d values at different seats. The authored claim (tangled_rope) reflects the presence of both genuine coordination (measurement) and asymmetric extraction (differential regulation).
 *
 * DIRECTIONALITY LOGIC:
 *   Regulatory bodies are structural beneficiaries (near d=0.0): they set the definition, enforce it, gain measurement control, bear no individual costs. Fintech issuers are near-symmetric (d≈0.5): they benefit from legitimacy and market access but bear compliance costs and capital rules. Consumers are diffuse beneficiaries (d≈0.3): they benefit from payment speed but bear counterparty risk and platform lock-in. Banks are targets (d≈0.75): they bear high compliance costs, capital requirements, and lost intermediation volume. Informal networks are full targets (d≈1.0): they bear complete exclusion from legitimacy and face legal liability. The constraint's enforcement (licensing, capital rules, compliance monitoring) targets those with the least mobility (informal networks, banks bound to jurisdictions) and benefits those with the most (international fintech platforms, regulatory bodies).
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint is claimed as tangled_rope because it serves both coordination (monetary measurement) and extraction (differential regulation favoring fintech, disfavoring banks and informal networks). The question is whether the extracted rent is necessary cost of the coordination or excess overhead. The measurement series shows extractiveness rising as e-money adoption grows: in 1990, the boundary was theoretically articulated but not enforced, so extraction was low. By 2025, the boundary is codified in regulation and affects billions in payment flows, so extraction is higher. The founded coordination function (measurement clarity) remains constant; the extractive overhead grows. Theater ratio is stable-low (0.05→0.22): the real measurement function is not being displaced by performative activity, which suggests the constraint is not sliding into piton status. The stability of theater — and the persistence of real coordination function — supports the tangled_rope classification over snare. A snare would show higher theater (increasing performative maintenance) and decoupling of extraction from any coordination benefit. This constraint's extraction grows WITH adoption of the coordination function itself, which is structurally tangled-rope behavior.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    boundary_contingency_on_institutional_legitimacy,
    'Is the consumer-holdings boundary a natural dividing line in digital money, or is it an institutional construction whose salience depends on regulatory choice to enforce it?',
    'Comparative analysis of jurisdictions with different boundary definitions (some counting only CBDC-equivalent instruments as ''e-money,'' others including all private e-money): if boundaries with different definitions produce equivalent monetary measurement and control, the boundary is contingent; if only the consumer-holdings boundary yields stable aggregates, it is natural.',
    'If contingent, the boundary is a coordinative choice (multiple valid options exist) and the extraction is rent-seeking around one choice. If natural, the extraction is the price of using the correct measurement framework. Classification would shift from tangled_rope toward rope (coordination-dominant) if natural, or toward snare (extraction-dominant) if purely contingent.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(boundary_contingency_on_institutional_legitimacy, empirical, 'Whether the consumer-holdings boundary is a natural feature of digital money or an institutional construction.').

omega_variable(
    informal_networks_structural_incompatibility,
    'Can informal value transfer networks (hawala, hundi, underground banking) coexist with the consumer-holdings boundary, or are they structurally incompatible and destined for exclusion?',
    'Historical cases of jurisdictions that attempted to integrate informal networks into regulated frameworks: did integration preserve the informal networks'' function, or did formalization destroy what made them valuable?',
    'If coexistence is possible, the constraint could be modified to include informal networks without losing coordination benefits. If incompatible, the constraint''s extraction of informal networks is a permanent structural feature and the classification remains snare for that seat. The piton question (is the boundary maintained theatrically, or does it serve a real function?) hinges partly on whether exclusion is necessary.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(informal_networks_structural_incompatibility, empirical, 'Structural compatibility between regulatory boundary and pre-existing informal money systems.').

omega_variable(
    sibling_reading_observational_divergence,
    'Do the sibling readings (conceptualization_reading, infrastructure_reading) produce the same classification under the metrics that characterize the consumer-holdings reading, or do they instantiate genuinely different constraint structures?',
    'Authoring the sibling readings as separate constraint stories and comparing their ε values, beneficiary/victim sets, and extraction profiles. If ε values converge, the readings are measuring the same constraint from different temporal perspectives. If they diverge, the readings instantiate genuinely different constraints.',
    'If omegas are identical across readings, the kernel is a single constraint viewed from multiple standpoints (the boundary between readings is observational, not structural). If the sibling readings carry different ε and different beneficiary sets, the kernel is constitutively contested — each reading is a distinct constraint, and the ''emergence'' question is unanswerable without committing to one reading''s frame. The network structure (affects_constraints edges) depends on this resolution.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_reading_observational_divergence, conceptual, 'Whether the sibling readings are different frames on one constraint or genuinely distinct constraints.').

omega_variable(
    monetary_aggregate_necessity,
    'Is the M4/M5 distinction (separating consumer-held e-money from bank deposits) necessary for monetary policy transmission, or is it an ornamental refinement that regulators maintain for administrative convenience?',
    'Empirical analysis of whether policy outcomes (inflation targeting, interest-rate effectiveness) differ when using M4 vs. M5 aggregates. If outcomes are identical, the distinction is administrative convenience. If outcomes differ materially, the distinction is functionally necessary.',
    'If ornamental, the constraint is primarily extractive (rents for fintech issuers, costs for banks, without genuine coordination benefit); reclassify toward snare. If necessary, the constraint is legitimately tangled_rope (coordination is real, extraction is the price). The resolution affects long-term policy: should the boundary be simplified or deepened?',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(monetary_aggregate_necessity, empirical, 'Whether the consumer-holdings boundary is necessary for monetary control or administrative convenience.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(digital_money_emergence_boundary__consumer_holdings_reading, 1990, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(digi_tr_t1990, digital_money_emergence_boundary__consumer_holdings_reading, theater_ratio, 1990, 0.05).
narrative_ontology:measurement_basis(digi_tr_t1990, projected).
narrative_ontology:measurement(digi_tr_t2000, digital_money_emergence_boundary__consumer_holdings_reading, theater_ratio, 2000, 0.12).
narrative_ontology:measurement_basis(digi_tr_t2000, observed).
narrative_ontology:measurement(digi_tr_t2008, digital_money_emergence_boundary__consumer_holdings_reading, theater_ratio, 2008, 0.16).
narrative_ontology:measurement_basis(digi_tr_t2008, observed).
narrative_ontology:measurement(digi_tr_t2015, digital_money_emergence_boundary__consumer_holdings_reading, theater_ratio, 2015, 0.2).
narrative_ontology:measurement_basis(digi_tr_t2015, observed).
narrative_ontology:measurement(digi_tr_t2020, digital_money_emergence_boundary__consumer_holdings_reading, theater_ratio, 2020, 0.21).
narrative_ontology:measurement_basis(digi_tr_t2020, observed).
narrative_ontology:measurement(digi_tr_t2025, digital_money_emergence_boundary__consumer_holdings_reading, theater_ratio, 2025, 0.22).
narrative_ontology:measurement_basis(digi_tr_t2025, observed).

% Extraction over time
narrative_ontology:measurement(digi_be_t1990, digital_money_emergence_boundary__consumer_holdings_reading, base_extractiveness, 1990, 0.18).
narrative_ontology:measurement_basis(digi_be_t1990, projected).
narrative_ontology:measurement(digi_be_t2000, digital_money_emergence_boundary__consumer_holdings_reading, base_extractiveness, 2000, 0.38).
narrative_ontology:measurement_basis(digi_be_t2000, observed).
narrative_ontology:measurement(digi_be_t2008, digital_money_emergence_boundary__consumer_holdings_reading, base_extractiveness, 2008, 0.48).
narrative_ontology:measurement_basis(digi_be_t2008, observed).
narrative_ontology:measurement(digi_be_t2015, digital_money_emergence_boundary__consumer_holdings_reading, base_extractiveness, 2015, 0.55).
narrative_ontology:measurement_basis(digi_be_t2015, observed).
narrative_ontology:measurement(digi_be_t2020, digital_money_emergence_boundary__consumer_holdings_reading, base_extractiveness, 2020, 0.59).
narrative_ontology:measurement_basis(digi_be_t2020, observed).
narrative_ontology:measurement(digi_be_t2025, digital_money_emergence_boundary__consumer_holdings_reading, base_extractiveness, 2025, 0.61).
narrative_ontology:measurement_basis(digi_be_t2025, observed).

% Suppression requirement over time
narrative_ontology:measurement(digi_su_t1990, digital_money_emergence_boundary__consumer_holdings_reading, suppression_requirement, 1990, 0.22).
narrative_ontology:measurement_basis(digi_su_t1990, projected).
narrative_ontology:measurement(digi_su_t2000, digital_money_emergence_boundary__consumer_holdings_reading, suppression_requirement, 2000, 0.35).
narrative_ontology:measurement_basis(digi_su_t2000, observed).
narrative_ontology:measurement(digi_su_t2008, digital_money_emergence_boundary__consumer_holdings_reading, suppression_requirement, 2008, 0.41).
narrative_ontology:measurement_basis(digi_su_t2008, observed).
narrative_ontology:measurement(digi_su_t2015, digital_money_emergence_boundary__consumer_holdings_reading, suppression_requirement, 2015, 0.46).
narrative_ontology:measurement_basis(digi_su_t2015, observed).
narrative_ontology:measurement(digi_su_t2020, digital_money_emergence_boundary__consumer_holdings_reading, suppression_requirement, 2020, 0.47).
narrative_ontology:measurement_basis(digi_su_t2020, observed).
narrative_ontology:measurement(digi_su_t2025, digital_money_emergence_boundary__consumer_holdings_reading, suppression_requirement, 2025, 0.48).
narrative_ontology:measurement_basis(digi_su_t2025, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(digital_money_emergence_boundary__consumer_holdings_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(digital_money_emergence_boundary__consumer_holdings_reading, 0.18).
narrative_ontology:affects_constraint(digital_money_emergence_boundary__consumer_holdings_reading, digital_money_emergence_boundary__conceptualization_reading).
narrative_ontology:affects_constraint(digital_money_emergence_boundary__consumer_holdings_reading, digital_money_emergence_boundary__infrastructure_reading).
narrative_ontology:affects_constraint(digital_money_emergence_boundary__consumer_holdings_reading, central_bank_digital_currency_domestic_circulation).
narrative_ontology:affects_constraint(digital_money_emergence_boundary__consumer_holdings_reading, fintech_regulatory_arbitrage).

% DUAL FORMULATION NOTE:
% This story is one of three readings of the digital_money_emergence_boundary kernel. The sibling readings (conceptualization_reading, infrastructure_reading) place the boundary at earlier technological/theoretical milestones and carry different beneficiary/victim structures. All three are linked via network.affects_constraints because the adopted boundary determines which innovations count as 'money' and which regulatory frameworks apply. The consumer-holdings reading (this story) is the most recent and most enforceable boundary; it influences the other readings by establishing the dominant regulatory frame.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(digital_money_emergence_boundary__consumer_holdings_reading, powerful, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
