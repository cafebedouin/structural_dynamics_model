% ============================================================================
% CONSTRAINT STORY: digital_money_emergence_boundary__consumer_holdings_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
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
 *   constraint_id: digital_money_emergence_boundary__consumer_holdings_reading
 *   human_readable: Consumer-Holdings Boundary for Digital Money Emergence
 *   domain: monetary_economics/financial_history/technology_governance
 *
 * SUMMARY:
 *   This constraint story captures the 'consumer-holdings' reading of when
 *   digital money emerged: the boundary drawn by the 2000 Electronic Money
 *   Directive (EMD) and subsequent EMI licensing regime, which defined
 *   'electronic money' as monetary value stored electronically and issued on
 *   receipt of funds for making payment transactions, distinct from bank
 *   deposits. The reading claims digital money exists only when individuals
 *   can hold it directly outside traditional bank accounts — making the M4/M5
 *   distinction necessary. The constraint is the regulatory boundary itself,
 *   not the technology. It coordinates by creating a legal category for
 *   non-bank stored-value issuance, and extracts by gating market access
 *   through EMI licenses, imposing compliance costs, and generating
 *   supervisory revenue. The claim/metric gap is deliberate: the constraint
 *   is CLAIMED as tangled_rope (genuine coordination + asymmetric extraction)
 *   while the authored metrics describe a regime whose extractive component
 *   has grown steadily as the EMI perimeter expanded to cover fintech
 *   wallets, prepaid cards, and embedded finance.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(digital_money_emergence_boundary__consumer_holdings_reading, 0.68).
domain_priors:suppression_score(digital_money_emergence_boundary__consumer_holdings_reading, 0.58).
domain_priors:theater_ratio(digital_money_emergence_boundary__consumer_holdings_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(digital_money_emergence_boundary__consumer_holdings_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(digital_money_emergence_boundary__consumer_holdings_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(digital_money_emergence_boundary__consumer_holdings_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(digital_money_emergence_boundary__consumer_holdings_reading, accessibility_collapse, 0.52).
narrative_ontology:constraint_metric(digital_money_emergence_boundary__consumer_holdings_reading, resistance, 0.54).

% --- Constraint claim ---
narrative_ontology:constraint_claim(digital_money_emergence_boundary__consumer_holdings_reading, tangled_rope).
narrative_ontology:human_readable(digital_money_emergence_boundary__consumer_holdings_reading, "Consumer-Holdings Boundary for Digital Money Emergence").
narrative_ontology:topic_domain(digital_money_emergence_boundary__consumer_holdings_reading, "monetary_economics/financial_history/technology_governance").

domain_priors:requires_active_enforcement(digital_money_emergence_boundary__consumer_holdings_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(digital_money_emergence_boundary__consumer_holdings_reading, 'bf9f89f7-a138-4821-822a-24ece77d665b').
narrative_ontology:cs_kernel_codification('bf9f89f7-a138-4821-822a-24ece77d665b', formalized).
narrative_ontology:cs_authority_grounding('bf9f89f7-a138-4821-822a-24ece77d665b', extraction).
narrative_ontology:cs_interpretation_layer_present('bf9f89f7-a138-4821-822a-24ece77d665b').
narrative_ontology:cs_reading_relation('bf9f89f7-a138-4821-822a-24ece77d665b', digital_money_emergence_boundary__conceptualization_reading, coexists_with).
narrative_ontology:cs_reading_relation('bf9f89f7-a138-4821-822a-24ece77d665b', digital_money_emergence_boundary__infrastructure_reading, influences).
narrative_ontology:cs_axiom('bf9f89f7-a138-4821-822a-24ece77d665b', foundational, money_requires_direct_holdability).
narrative_ontology:cs_axiom_status(money_requires_direct_holdability, holdable).
narrative_ontology:cs_axiom_grounding('bf9f89f7-a138-4821-822a-24ece77d665b', money_requires_direct_holdability, conventional).
narrative_ontology:cs_axiom('bf9f89f7-a138-4821-822a-24ece77d665b', foundational, e_money_distinct_from_bank_deposits).
narrative_ontology:cs_axiom_status(e_money_distinct_from_bank_deposits, holdable).
narrative_ontology:cs_axiom_grounding('bf9f89f7-a138-4821-822a-24ece77d665b', e_money_distinct_from_bank_deposits, conventional).
narrative_ontology:cs_reference_frame('bf9f89f7-a138-4821-822a-24ece77d665b', emd_2000_regulatory_perimeter).
narrative_ontology:cs_drift_state('bf9f89f7-a138-4821-822a-24ece77d665b', contemporary_embedded_finance_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('bf9f89f7-a138-4821-822a-24ece77d665b', '2026-08-03T12:00:00Z').
narrative_ontology:cs_kernel_id(digital_money_emergence_boundary__consumer_holdings_reading, digital_money_emergence_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(digital_money_emergence_boundary__consumer_holdings_reading, regulatory_bodies_emi_ecb).
narrative_ontology:constraint_beneficiary(digital_money_emergence_boundary__consumer_holdings_reading, fintech_firms_e_money_issuers).
narrative_ontology:constraint_victim(digital_money_emergence_boundary__consumer_holdings_reading, traditional_banks).
narrative_ontology:constraint_victim(digital_money_emergence_boundary__consumer_holdings_reading, consumers).
narrative_ontology:constraint_victim(digital_money_emergence_boundary__consumer_holdings_reading, alternative_definition_advocates).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(digital_money_emergence_boundary__consumer_holdings_reading, consumers).
narrative_ontology:constraint_vindicates(digital_money_emergence_boundary__consumer_holdings_reading, electronic_money_directive_framework).
narrative_ontology:constraint_vindicates(digital_money_emergence_boundary__consumer_holdings_reading, m4_m5_monetary_aggregate_distinction).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Define the legal category of 'electronic money' through the Electronic Money Directive (2000) and subsequent EMI licensing regime. They set the boundary conditions for what counts as e-money versus bank deposits, enforce compliance, and collect supervisory fees. Their authority derives from the power to grant or withhold EMI licenses, which gates market entry for fintech firms.
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__consumer_holdings_reading, regulatory_bodies_emi_ecb, agenda_setter,
    institutional, generational, analytical, global).

% Issue e-money products (prepaid cards, digital wallets, stored-value accounts) under the EMI license regime. They benefit from a clear regulatory category that legitimizes their business models and creates a moat against unlicensed competitors. They pay compliance costs but gain market access and consumer trust from the regulatory badge. Exit means surrendering the license or relocating to jurisdictions with lighter regimes.
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__consumer_holdings_reading, fintech_firms_e_money_issuers, beneficiary,
    organized, biographical, mobile, global).

% Face deposit competition from e-money issuers who operate with lighter capital requirements and no deposit insurance obligations. The M4/M5 distinction makes this competition visible in monetary statistics. They bear compliance costs for their own banking licenses while arguing for 'level playing field' regulation. Exit from the constraint is impossible — they are the reference category against which e-money is defined.
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__consumer_holdings_reading, traditional_banks, payer,
    institutional, generational, constrained, global).

% Gain access to convenient digital payment instruments (wallets, prepaid cards, neobank accounts) with regulatory protections (safeguarding, redemption rights). They also bear the costs: fees on e-money products, complexity of navigating multiple regulatory regimes, and the risk that safeguarding fails (as seen in Wirecard). Their choice set is bounded by what licensed EMIs offer; unlicensed alternatives are legally suppressed.
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__consumer_holdings_reading, consumers, payer,
    organized, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(digital_money_emergence_boundary__consumer_holdings_reading, consumers, beneficiary).

% Analyze the emergence boundary as a constructed legal category rather than a discovered monetary fact. They document how the 1990s e-purse trials and 2000 EMD created the 'e-money' category ex ante, then measured adoption ex post. They have no stake in the regulatory regime but their work shapes the intellectual framework within which regulators and industry operate.
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__consumer_holdings_reading, academic_scholars_monetary_historians, observer,
    analytical, civilizational, analytical, universal).

% Build payment systems that fall outside the EMI/bank dichotomy — community currencies, mutual credit systems, crypto-native payment rails, informal value transfer networks. They are structurally excluded because the consumer-holdings reading defines 'digital money' exclusively through the EMI license gate. They would argue for a broader definition of digital money based on functional use rather than regulatory status, but have no seat at the EMD drafting table.
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__consumer_holdings_reading, alternative_payment_innovators, excluded,
    moderate, biographical, trapped, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Creates a legally defined category 'electronic money' distinct from bank deposits, enabling: (1) consumer protection through safeguarding and redemption rules, (2) monetary statistics (M4/M5) that separate e-money liabilities from bank deposits, (3) a licensing gateway (EMI regime) that authorizes non-bank issuers to offer stored-value products, and (4) a regulatory perimeter that makes the fintech business model legible to supervisors.
% TRANSFER_FUNCTION: Moves regulatory authority from an undefined space (1990s e-purses operated in a grey zone) to the EMI licensing regime; moves compliance costs (capital requirements, safeguarding, reporting) onto e-money issuers; moves competitive pressure onto traditional banks who lose deposit share to e-money wallets; moves supervisory revenue (license fees, ongoing levies) to regulatory bodies.
% ABSENT_VOICES: Cash-dependent communities, informal economy participants, decentralized currency advocates, and mutual credit system operators who transact digitally but outside the EMI framework. They are absent because the consumer-holdings reading equates 'holding digital money' with 'holding an EMI-licensed claim' — their digital instruments (community tokens, crypto stablecoins used as cash, hawala digital records) are rendered invisible by the regulatory boundary.
% DISAPPEARANCE_RATIONALE: If the consumer-holdings boundary vanished overnight: the EMI licensing regime would collapse (no legal basis for 'e-money' distinct from deposits), fintech firms would lose their regulatory moat and face either banking license requirements or outright prohibition, M4/M5 monetary aggregates would become incoherent (no e-money sub-category), and consumers would lose safeguarding protections on stored-value products. The entire fintech stored-value ecosystem would reorganize — either into full banking or into unregulated shadow payment systems.
% FOUNDING_PROBLEM: Regulatory vacuum in the 1990s: e-purse trials (Mondex, Visa Cash, Proton) and early stored-value products operated without clear legal status. Consumers had no safeguarding protections; issuers had no authorization framework; central banks could not measure e-money in monetary aggregates; supervisors had no perimeter for non-bank stored-value issuance. The 2000 Electronic Money Directive was built to solve this vacuum.
% FOUNDING_PROBLEM_CORROBORATION: ECB and national regulators attest the problem remains live (new products like stablecoins, embedded finance wallets require updated EMI framework). Fintech industry associations attest the founding problem is substantially solved — the EMI regime works and should be extended, not replaced. Monetary historians (e.g., Lastra, Goodhart) corroborate from outside the beneficiary set that the 'regulatory vacuum' narrative was constructed ex post to justify a category that also served industrial policy goals (EU fintech competitiveness).
narrative_ontology:disappearance_verdict(digital_money_emergence_boundary__consumer_holdings_reading, world_rearranges).
narrative_ontology:founding_problem_status(digital_money_emergence_boundary__consumer_holdings_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(digital_money_emergence_boundary__consumer_holdings_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(digital_money_emergence_boundary__consumer_holdings_reading, 'none', 1).
narrative_ontology:epsilon_provenance(digital_money_emergence_boundary__consumer_holdings_reading, 0.68, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Extractiveness (0.68) reflects the regime's dual nature: the coordination function (consumer protection, monetary statistics) is real but the extraction component (licensing rents, compliance moats, supervisory fees) has grown as the EMI category expanded beyond its 2000 scope. Suppression (0.58) is moderate — alternative definitions (infrastructure-based, conceptual) are not banned but are legally marginalized; unlicensed digital value transfer is actively suppressed. Theater ratio (0.38) captures the growing gap between the regime's stated consumer-protection purpose and its de facto function as a fintech industrial policy tool. Accessibility collapse (0.52) and resistance (0.54) are moderate — the boundary is contestable (stablecoins, CBDCs, crypto challenge it) but the EMI regime has successfully expanded to absorb many challengers.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat (regulators) experiences this as genuine coordination: they built a framework that solved a real regulatory vacuum. The payer seats (banks, consumers) experience it as asymmetric extraction: banks face unfair competition; consumers pay for a regulatory perimeter that also serves industrial policy. The excluded seat (alternative innovators) experiences it as suppression: their functional digital money is rendered illegal by definitional fiat. The engine computes this divergence from the structural data — the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Regulatory bodies (EMI/ECB) are structural beneficiaries — they define the category, collect fees, and expand supervisory perimeter (d near 0.0). Fintech EMI issuers are beneficiaries — they gain market access and legitimacy from the license (d ~0.15). Traditional banks are payers — they face deposit competition with asymmetric regulation (d ~0.85). Consumers are dual-role — genuine coordination benefit (safeguarding, convenience) but also pay fees and bear complexity (d ~0.5). Alternative innovators are excluded — their exit is trapped (d ~0.95). Academic observers are analytical (d=0.5 by definition).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (1990s regulatory vacuum) is contested: regulators say it's live (new products need framework), industry says it's solved (extend the regime), historians say it was constructed. The constraint persists not because the vacuum remains but because the EMI regime created its own constituency (licensed firms, supervisory bodies, compliant infrastructure) that would lose from boundary dissolution. This is classic mandatrophy — the arrangement outlives its founding justification but persists through institutional inertia and beneficiary capture.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is this constraint one reading of a contested kernel (digital_money_emergence_boundary) rather than a standalone constraint?',
    'Cross-reference with sibling constraint stories (conceptualization_reading, infrastructure_reading) to verify they share the same kernel_id but instantiate different ε, beneficiaries, and structural relationships.',
    'If confirmed, classification must be reading-indexed — the engine computes per-reading types, not a single kernel type. The constraint family linked via network.affects_constraints enables contamination analysis across readings.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Commitment to the kernel-reading frame for this constraint story.').

omega_variable(
    boundary_construction_vs_discovery,
    'Is the consumer-holdings boundary a discovered monetary fact (money naturally becomes digital when holdable) or a constructed regulatory category (the EMD created ''e-money'' ex ante)?',
    'Historical analysis of EMD drafting: did the directive codify an existing market reality or create a new legal category that shaped subsequent market development? Compare pre-2000 e-purse adoption (minimal) vs post-2000 EMI licensing growth.',
    'If constructed, the constraint is a scaffold/tangled_rope with extractive regulatory capture; if discovered, it trends toward rope/mountain. The ε-invariance principle requires this reading to declare its own ε (0.68) regardless of the answer — but the omega documents the irreducible ambiguity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(boundary_construction_vs_discovery, conceptual, 'Whether the emergence boundary is ontological or regulatory.').

omega_variable(
    sibling_reading_structural_delta,
    'How do the three readings structurally differ in their ε, beneficiary sets, and constraint types?',
    'Author the sibling constraint stories and compare: conceptualization_reading (ε~0.1, beneficiaries: cryptographers/academics, type: rope), infrastructure_reading (ε~0.3, beneficiaries: banks/infrastructure operators, type: rope), consumer_holdings_reading (ε~0.68, beneficiaries: regulators/fintech, type: tangled_rope). The delta is the measured extraction gradient across the kernel.',
    'If the ε gradient is real, the kernel is a family of constraints with a measurable extraction gradient — later readings are more extractive. This validates the ε-invariance decomposition. If the gradient collapses on re-measurement, the decomposition was artifact of authoring bias.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_structural_delta, empirical, 'Structural delta between the three emergence-boundary readings.').

omega_variable(
    m4_m5_distinction_necessity,
    'Is the M4/M5 monetary aggregate distinction (separating e-money from deposits) analytically necessary or a regulatory artifact that serves the EMI regime?',
    'Counterfactual: if e-money were measured within M3 (as bank deposits), would monetary policy transmission be impaired? Compare Euro Area M4/M5 tracking since 2000 with jurisdictions that don''t separate e-money.',
    'If analytically necessary, the coordination function is genuine and extraction is the price of measurement clarity. If artifact, the distinction is extraction-enabling theater — the M4/M5 split makes EMI liabilities visible as a distinct aggregate, justifying the regime''s existence.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(m4_m5_distinction_necessity, empirical, 'Whether the monetary statistics justification is genuine or constructed.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(digital_money_emergence_boundary__consumer_holdings_reading, 0, 34).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dm_holding_tr_t0, digital_money_emergence_boundary__consumer_holdings_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(dm_holding_tr_t6, digital_money_emergence_boundary__consumer_holdings_reading, theater_ratio, 6, 0.18).
narrative_ontology:measurement(dm_holding_tr_t12, digital_money_emergence_boundary__consumer_holdings_reading, theater_ratio, 12, 0.24).
narrative_ontology:measurement(dm_holding_tr_t18, digital_money_emergence_boundary__consumer_holdings_reading, theater_ratio, 18, 0.3).
narrative_ontology:measurement(dm_holding_tr_t24, digital_money_emergence_boundary__consumer_holdings_reading, theater_ratio, 24, 0.34).
narrative_ontology:measurement(dm_holding_tr_t30, digital_money_emergence_boundary__consumer_holdings_reading, theater_ratio, 30, 0.37).
narrative_ontology:measurement(dm_holding_tr_t34, digital_money_emergence_boundary__consumer_holdings_reading, theater_ratio, 34, 0.38).

% Extraction over time
narrative_ontology:measurement(dm_holding_be_t0, digital_money_emergence_boundary__consumer_holdings_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(dm_holding_be_t6, digital_money_emergence_boundary__consumer_holdings_reading, base_extractiveness, 6, 0.42).
narrative_ontology:measurement(dm_holding_be_t12, digital_money_emergence_boundary__consumer_holdings_reading, base_extractiveness, 12, 0.51).
narrative_ontology:measurement(dm_holding_be_t18, digital_money_emergence_boundary__consumer_holdings_reading, base_extractiveness, 18, 0.58).
narrative_ontology:measurement(dm_holding_be_t24, digital_money_emergence_boundary__consumer_holdings_reading, base_extractiveness, 24, 0.63).
narrative_ontology:measurement(dm_holding_be_t30, digital_money_emergence_boundary__consumer_holdings_reading, base_extractiveness, 30, 0.66).
narrative_ontology:measurement(dm_holding_be_t34, digital_money_emergence_boundary__consumer_holdings_reading, base_extractiveness, 34, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(dm_holding_su_t0, digital_money_emergence_boundary__consumer_holdings_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(dm_holding_su_t6, digital_money_emergence_boundary__consumer_holdings_reading, suppression_requirement, 6, 0.33).
narrative_ontology:measurement(dm_holding_su_t12, digital_money_emergence_boundary__consumer_holdings_reading, suppression_requirement, 12, 0.41).
narrative_ontology:measurement(dm_holding_su_t18, digital_money_emergence_boundary__consumer_holdings_reading, suppression_requirement, 18, 0.48).
narrative_ontology:measurement(dm_holding_su_t24, digital_money_emergence_boundary__consumer_holdings_reading, suppression_requirement, 24, 0.53).
narrative_ontology:measurement(dm_holding_su_t30, digital_money_emergence_boundary__consumer_holdings_reading, suppression_requirement, 30, 0.56).
narrative_ontology:measurement(dm_holding_su_t34, digital_money_emergence_boundary__consumer_holdings_reading, suppression_requirement, 34, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(digital_money_emergence_boundary__consumer_holdings_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(digital_money_emergence_boundary__consumer_holdings_reading, 0.15).
narrative_ontology:affects_constraint(digital_money_emergence_boundary__consumer_holdings_reading, monetary_aggregate_measurement_m4_m5).
narrative_ontology:affects_constraint(digital_money_emergence_boundary__consumer_holdings_reading, fintech_licensing_regime_emi).
narrative_ontology:affects_constraint(digital_money_emergence_boundary__consumer_holdings_reading, digital_money_emergence_boundary__conceptualization_reading).
narrative_ontology:affects_constraint(digital_money_emergence_boundary__consumer_holdings_reading, digital_money_emergence_boundary__infrastructure_reading).

% DUAL FORMULATION NOTE:
% This constraint is one member of the digital_money_emergence_boundary constraint family (3 readings). The consumer_holdings_reading draws the latest boundary (2000 EMD), creating the M4/M5 distinction and EMI licensing regime. The conceptualization_reading draws the earliest boundary (1985 Chaum) with negligible extraction. The infrastructure_reading draws an intermediate boundary (1970s rails) with moderate extraction. The extraction gradient across the family (0.1 → 0.3 → 0.68) measures how the 'emergence' narrative has been leveraged to build progressively more extractive regulatory perimeters.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(digital_money_emergence_boundary__consumer_holdings_reading, institutional, 0.1).
constraint_indexing:directionality_override(digital_money_emergence_boundary__consumer_holdings_reading, organized, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
