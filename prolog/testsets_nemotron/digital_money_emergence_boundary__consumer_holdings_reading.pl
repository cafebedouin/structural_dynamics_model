% ============================================================================
% CONSTRAINT STORY: digital_money_emergence_boundary__consumer_holdings_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
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
 *   human_readable: Digital Money Emergence: Consumer Holdings Boundary (1990s E-Purses, 2000 EMD)
 *   domain: monetary_economics/financial_history/technology_governance
 *
 * SUMMARY:
 *   The consumer-holdings reading of the digital money emergence boundary
 *   places the origin of digital money at the moment individuals could
 *   directly hold and transact with digital instruments — 1990s e-purse
 *   trials (Mondex, Proton, Visa Cash) and the 2000 Electronic Money
 *   Directive that gave them legal form. This reading draws the boundary at
 *   consumer-facing stored value, distinct from bank deposits, creating the
 *   M4/M5 monetary aggregate separation. The constraint is a tangled rope: it
 *   genuinely coordinates trust in non-bank digital liabilities
 *   (beneficiaries: regulators, fintech issuers) while extracting from those
 *   with the least exit power (unbanked consumers, cash-dependent
 *   populations, small merchants). The beneficiary structure is asymmetric —
 *   regulators and issuers gain authority and revenue; the excluded pay for
 *   access.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(digital_money_emergence_boundary__consumer_holdings_reading, 0.42).
domain_priors:suppression_score(digital_money_emergence_boundary__consumer_holdings_reading, 0.31).
domain_priors:theater_ratio(digital_money_emergence_boundary__consumer_holdings_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(digital_money_emergence_boundary__consumer_holdings_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(digital_money_emergence_boundary__consumer_holdings_reading, suppression_requirement, 0.31).
narrative_ontology:constraint_metric(digital_money_emergence_boundary__consumer_holdings_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(digital_money_emergence_boundary__consumer_holdings_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(digital_money_emergence_boundary__consumer_holdings_reading, resistance, 0.38).

% --- Constraint claim ---
narrative_ontology:constraint_claim(digital_money_emergence_boundary__consumer_holdings_reading, tangled_rope).
narrative_ontology:human_readable(digital_money_emergence_boundary__consumer_holdings_reading, "Digital Money Emergence: Consumer Holdings Boundary (1990s E-Purses, 2000 EMD)").
narrative_ontology:topic_domain(digital_money_emergence_boundary__consumer_holdings_reading, "monetary_economics/financial_history/technology_governance").

domain_priors:requires_active_enforcement(digital_money_emergence_boundary__consumer_holdings_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(digital_money_emergence_boundary__consumer_holdings_reading, '6c1a57ef-db30-4cb2-9052-4ece5ab48f83').
narrative_ontology:cs_kernel_codification('6c1a57ef-db30-4cb2-9052-4ece5ab48f83', formalized).
narrative_ontology:cs_authority_grounding('6c1a57ef-db30-4cb2-9052-4ece5ab48f83', lineage).
narrative_ontology:cs_interpretation_layer_present('6c1a57ef-db30-4cb2-9052-4ece5ab48f83').
narrative_ontology:cs_reading_relation('6c1a57ef-db30-4cb2-9052-4ece5ab48f83', digital_money_emergence_boundary__conceptualization_reading, coexists_with).
narrative_ontology:cs_reading_relation('6c1a57ef-db30-4cb2-9052-4ece5ab48f83', digital_money_emergence_boundary__infrastructure_reading, influences).
narrative_ontology:cs_axiom('6c1a57ef-db30-4cb2-9052-4ece5ab48f83', foundational, money_requires_direct_consumer_holding).
narrative_ontology:cs_axiom_status(money_requires_direct_consumer_holding, holdable).
narrative_ontology:cs_axiom_grounding('6c1a57ef-db30-4cb2-9052-4ece5ab48f83', money_requires_direct_consumer_holding, conventional).
narrative_ontology:cs_axiom('6c1a57ef-db30-4cb2-9052-4ece5ab48f83', foundational, emoney_liability_distinct_from_deposit).
narrative_ontology:cs_axiom_status(emoney_liability_distinct_from_deposit, holdable).
narrative_ontology:cs_axiom_grounding('6c1a57ef-db30-4cb2-9052-4ece5ab48f83', emoney_liability_distinct_from_deposit, conventional).
narrative_ontology:cs_axiom('6c1a57ef-db30-4cb2-9052-4ece5ab48f83', secondary, m4_m5_separation_improves_monetary_analysis).
narrative_ontology:cs_axiom_status(m4_m5_separation_improves_monetary_analysis, holdable).
narrative_ontology:cs_axiom_grounding('6c1a57ef-db30-4cb2-9052-4ece5ab48f83', m4_m5_separation_improves_monetary_analysis, empirically_contingent).
narrative_ontology:cs_reference_frame('6c1a57ef-db30-4cb2-9052-4ece5ab48f83', emd_2000_regulatory_framework).
narrative_ontology:cs_drift_state('6c1a57ef-db30-4cb2-9052-4ece5ab48f83', post_stablecoin_psd2_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('6c1a57ef-db30-4cb2-9052-4ece5ab48f83', '2026-08-15T14:32:17Z').
narrative_ontology:cs_kernel_id(digital_money_emergence_boundary__consumer_holdings_reading, digital_money_emergence_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(digital_money_emergence_boundary__consumer_holdings_reading, emi_regulators).
narrative_ontology:constraint_beneficiary(digital_money_emergence_boundary__consumer_holdings_reading, ecb_supervisors).
narrative_ontology:constraint_beneficiary(digital_money_emergence_boundary__consumer_holdings_reading, fintech_issuers).
narrative_ontology:constraint_beneficiary(digital_money_emergence_boundary__consumer_holdings_reading, payment_network_operators).
narrative_ontology:constraint_victim(digital_money_emergence_boundary__consumer_holdings_reading, unbanked_consumers).
narrative_ontology:constraint_victim(digital_money_emergence_boundary__consumer_holdings_reading, cash_dependent_populations).
narrative_ontology:constraint_victim(digital_money_emergence_boundary__consumer_holdings_reading, small_merchants).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% European electronic money institution regulators who authored the 2000 EMD and subsequent PSD2/EMR frameworks. They define the legal category of e-money, set issuance rules, and supervise compliance. Their regulatory authority creates the market structure fintech firms operate within. They can exit by revising the framework but have no structural incentive to do so.
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__consumer_holdings_reading, emi_regulators, agenda_setter,
    institutional, generational, arbitrage, continental).

% European Central Bank supervisors who oversee monetary aggregates and payment system stability. The M4/M5 distinction gives them analytical tools to separate bank deposits from e-money liabilities. They benefit from clearer monetary transmission visibility but also enforce the boundary through supervisory expectations.
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__consumer_holdings_reading, ecb_supervisors, agenda_setter,
    institutional, generational, arbitrage, continental).
narrative_ontology:stakeholder_secondary_role(digital_money_emergence_boundary__consumer_holdings_reading, ecb_supervisors, beneficiary).

% E-money institutions and fintech firms (e.g., PayPal EU, Wise, Revolut pre-banking license) that issue e-money products. They gain a regulatory passport to operate across the EEA without a full banking license, capturing revenue from stored value and transaction fees. Their exit is mobile — they can pivot to banking licenses or other jurisdictions — but the e-money license is a lower-barrier entry point they benefit from.
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__consumer_holdings_reading, fintech_issuers, beneficiary,
    organized, biographical, mobile, continental).

% Card networks (Visa, Mastercard) and scheme operators that process e-money transactions. They collect interchange and scheme fees on e-money flows. The regulatory boundary legitimizes their role in the e-money ecosystem. Exit is constrained by network effects and scheme rules.
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__consumer_holdings_reading, payment_network_operators, beneficiary,
    organized, biographical, constrained, continental).

% Individuals without access to traditional bank accounts who rely on e-money products for digital payments. They pay fees for loading, holding, and transacting with e-money that banked consumers avoid. Their exit options are trapped — cash exclusion and lack of banking access force dependence on the very products that extract from them.
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__consumer_holdings_reading, unbanked_consumers, payer,
    powerless, immediate, trapped, local).

% Populations (elderly, rural, informal economy workers) who remain cash-dependent as digital payments become the norm. The e-money boundary accelerates cash infrastructure withdrawal. They bear the cost of digital exclusion but have no voice in the regulatory frameworks that define the boundary. Exit is trapped — they cannot adopt what they cannot access.
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__consumer_holdings_reading, cash_dependent_populations, payer,
    powerless, immediate, trapped, local).
narrative_ontology:stakeholder_secondary_role(digital_money_emergence_boundary__consumer_holdings_reading, cash_dependent_populations, excluded).

% Merchants who must accept e-money payments to remain competitive, paying scheme fees and acquiring costs they cannot easily pass through. The regulatory boundary mandates acceptance infrastructure. Exit is constrained — refusing digital payments loses customers, but fee structures are set by networks and issuers.
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__consumer_holdings_reading, small_merchants, payer,
    moderate, biographical, constrained, regional).

% Central bank researchers and academic monetary economists who track M4/M5 aggregates and debate the boundary's analytical validity. They observe the constraint's operation but neither collect from nor pay into it. Their exit is analytical — they can revise their models without material consequence.
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__consumer_holdings_reading, monetary_analysts, observer,
    analytical, generational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a legally recognized category for stored-value digital instruments that are not bank deposits, enabling non-bank firms to issue payment products and consumers to hold digital value without a banking relationship. Solves the coordination problem of trust in private digital liabilities by subjecting issuers to prudential safeguards (safeguarding funds, capital requirements) and giving holders a clear legal claim.
% TRANSFER_FUNCTION: Moves seigniorage-like revenue (float on stored value, transaction fees, interchange) from e-money holders and merchants to issuers and payment networks. Moves regulatory compliance costs onto issuers. Moves analytical clarity (separation of money-like liabilities) to supervisors. The M4/M5 distinction transfers classification authority to regulators.
% ABSENT_VOICES: Unbanked consumers and cash-dependent populations are structurally excluded from the regulatory process that defined the e-money boundary. They would object to fee structures that extract from the least resourced and to the acceleration of cash displacement, but they have no seat at EMI/ECB working groups. Consumer advocacy NGOs are occasionally consulted but hold no veto.
% DISAPPEARANCE_RATIONALE: If the consumer-holdings boundary vanished overnight, the legal distinction between e-money and bank deposits would collapse. Fintech issuers would lose their regulatory passport (reverting to banking licenses or unregulated status), supervisors would lose M4/M5 analytical separation, unbanked consumers would lose the specific protections (safeguarding, redemption rights) the boundary created, and the 2000 EMD/PSD2 regulatory architecture would require fundamental reconstruction.
% FOUNDING_PROBLEM: The 1990s e-purse pilots (Mondex, Visa Cash, Proton) and early internet payments created a regulatory vacuum: private digital liabilities that looked like money but fell outside banking regulation. The founding problem was how to accommodate innovation in digital stored value without compromising monetary stability, consumer protection, or the integrity of the banking monopoly on deposit-taking.
% FOUNDING_PROBLEM_CORROBORATION: EMI regulators and ECB supervisors attest the problem remains live (ongoing stablecoin/crypto challenges, need for consistent framework). Fintech issuers argue the founding problem is substantially solved for traditional e-money but the framework is now overextended to novel instruments. Academic monetary economists (Bindseil, Panetta, Adrian) and consumer advocates (BEUC, Finance Watch) corroborate from outside the beneficiary set that the boundary has shifted from enabling innovation to entrenching a specific industry structure.
narrative_ontology:disappearance_verdict(digital_money_emergence_boundary__consumer_holdings_reading, world_rearranges).
narrative_ontology:founding_problem_status(digital_money_emergence_boundary__consumer_holdings_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(digital_money_emergence_boundary__consumer_holdings_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(digital_money_emergence_boundary__consumer_holdings_reading, 'none', 1).
narrative_ontology:epsilon_provenance(digital_money_emergence_boundary__consumer_holdings_reading, 0.42, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(digital_money_emergence_boundary__consumer_holdings_reading_tests).
:- end_tests(digital_money_emergence_boundary__consumer_holdings_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.42) reflects that the boundary enables a revenue model (float, fees, interchange) that disproportionately burdens low-income users while the coordination function (trust in e-money) is real but partial — safeguarding rules work but don't eliminate cost asymmetries. Suppression (0.31) is moderate: the constraint doesn't forcibly prevent cash use but creates structural pressure through infrastructure withdrawal and merchant acceptance norms. Theater (0.28) is present: consumer protection rhetoric accompanies fee structures that extract from the protected. Accessibility collapse (0.45) and resistance (0.38) reflect that alternatives (cash, bank accounts) persist but are eroding. Measurements share a single time grid (1990-2024) across all three metrics.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter seat (regulators), the boundary is a coordination achievement: it brought order to a chaotic innovation space, created consumer protections, and enabled fintech competition. From the payer seats (unbanked, cash-dependent, small merchants), the same structure operates as a fee-extraction mechanism that locks them into costly digital rails while cash infrastructure atrophies. The engine computes this divergence from the structural data; the claimed_type (tangled_rope) asserts the hybrid nature without resolving the seat disagreement.
 *
 * DIRECTIONALITY LOGIC:
 *   EMI regulators and ECB supervisors are structural beneficiaries (d near 0.1-0.2): they gain analytical tools and regulatory authority from the boundary. Fintech issuers and payment networks are beneficiaries (d ~0.2-0.3): they collect revenue enabled by the regulatory passport. Unbanked consumers and cash-dependent populations are targets (d ~0.8-0.9): they pay fees for products they have no alternative to, with trapped exit. Small merchants are payers (d ~0.6-0.7): constrained exit, bear costs they cannot fully pass through. Monetary analysts are analytical observers (d=0.5). The boundary's extraction is amplified for trapped agents by the engine's directionality computation.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (regulating private digital liabilities without banking them) was live in 2000. By 2024, the original e-money instruments are marginal, but the boundary has been extended to cover prepaid cards, digital wallets, and stablecoin-adjacent products. The mandate has not atrophied — it has expanded. However, the coordination function (enabling non-bank digital money) now serves an industry structure that extracts from the vulnerable. The constraint is not a piton (it's actively maintained and expanded) but a tangled rope where the coordination story legitimizes an extractive edge.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_boundary_location,
    'Is the consumer-holdings boundary the structurally correct location for the digital money emergence constraint, or do the sibling readings (conceptualization, infrastructure) capture the coordination function more fundamentally?',
    'Counterfactual analysis: if the 2000 EMD had never passed but e-purse technology diffused anyway, would the coordination problem have been solved differently? If Chaum''s 1985 formalization had been implemented as a central bank digital currency in 1990, would the consumer-holdings reading still be the relevant boundary?',
    'If the sibling readings capture the deeper coordination function, this reading''s claimed_type (tangled_rope) may misattribute extractiveness to a boundary that is derivative rather than fundamental. The engine''s classification would then reflect a contingent regulatory choice, not the kernel''s structural logic.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_boundary_location, conceptual, 'Whether this reading''s boundary is fundamental or derivative relative to the kernel.').

omega_variable(
    extraction_coordination_separability,
    'Can the coordination function (trust in non-bank digital liabilities) be separated from the extractive edge (fee structures burdening the unbanked), or are they structurally fused in the e-money regulatory form?',
    'Natural experiment from jurisdictions with different e-money fee caps or public e-money options (e.g., Brazil''s Pix, Kenya''s M-Pesa regulatory model). If coordination persists without the extractive fee structure, they are separable.',
    'If separable, the constraint''s extraction is a policy choice, not a structural necessity — the tangled_rope classification would reflect regulatory capture rather than irreducible hybridity. If fused, the tangled_rope is the honest description of the constraint''s nature.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(extraction_coordination_separability, empirical, 'Whether the constraint''s coordination and extraction components can be disaggregated.').

omega_variable(
    m4_m5_analytical_necessity,
    'Does the M4/M5 monetary aggregate distinction (separating bank deposits from e-money) serve a genuine analytical function for monetary policy, or is it a regulatory artifact that reifies the boundary this reading draws?',
    'ECB internal debate records on whether M4/M5 separation changed any policy decision vs. pre-2000 aggregates. Academic literature on monetary aggregate informativeness post-EMD.',
    'If M4/M5 is analytically vacuous, the boundary''s coordination function for supervisors (a declared beneficiary) is theatrical — the beneficiary declaration would be a vindicated proposition masquerading as a beneficiary, reducing the constraint''s coordination legitimacy.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(m4_m5_analytical_necessity, empirical, 'Whether the monetary aggregate distinction this reading creates has operational significance.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(digital_money_emergence_boundary__consumer_holdings_reading, 1990, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dmeb_chr_tr_t1990, digital_money_emergence_boundary__consumer_holdings_reading, theater_ratio, 1990, 0.08).
narrative_ontology:measurement(dmeb_chr_tr_t1996, digital_money_emergence_boundary__consumer_holdings_reading, theater_ratio, 1996, 0.12).
narrative_ontology:measurement(dmeb_chr_tr_t2000, digital_money_emergence_boundary__consumer_holdings_reading, theater_ratio, 2000, 0.15).
narrative_ontology:measurement(dmeb_chr_tr_t2007, digital_money_emergence_boundary__consumer_holdings_reading, theater_ratio, 2007, 0.21).
narrative_ontology:measurement(dmeb_chr_tr_t2015, digital_money_emergence_boundary__consumer_holdings_reading, theater_ratio, 2015, 0.25).
narrative_ontology:measurement(dmeb_chr_tr_t2024, digital_money_emergence_boundary__consumer_holdings_reading, theater_ratio, 2024, 0.28).

% Extraction over time
narrative_ontology:measurement(dmeb_chr_be_t1990, digital_money_emergence_boundary__consumer_holdings_reading, base_extractiveness, 1990, 0.12).
narrative_ontology:measurement(dmeb_chr_be_t1996, digital_money_emergence_boundary__consumer_holdings_reading, base_extractiveness, 1996, 0.18).
narrative_ontology:measurement(dmeb_chr_be_t2000, digital_money_emergence_boundary__consumer_holdings_reading, base_extractiveness, 2000, 0.25).
narrative_ontology:measurement(dmeb_chr_be_t2007, digital_money_emergence_boundary__consumer_holdings_reading, base_extractiveness, 2007, 0.32).
narrative_ontology:measurement(dmeb_chr_be_t2015, digital_money_emergence_boundary__consumer_holdings_reading, base_extractiveness, 2015, 0.38).
narrative_ontology:measurement(dmeb_chr_be_t2024, digital_money_emergence_boundary__consumer_holdings_reading, base_extractiveness, 2024, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(dmeb_chr_su_t1990, digital_money_emergence_boundary__consumer_holdings_reading, suppression_requirement, 1990, 0.15).
narrative_ontology:measurement(dmeb_chr_su_t1996, digital_money_emergence_boundary__consumer_holdings_reading, suppression_requirement, 1996, 0.2).
narrative_ontology:measurement(dmeb_chr_su_t2000, digital_money_emergence_boundary__consumer_holdings_reading, suppression_requirement, 2000, 0.25).
narrative_ontology:measurement(dmeb_chr_su_t2007, digital_money_emergence_boundary__consumer_holdings_reading, suppression_requirement, 2007, 0.28).
narrative_ontology:measurement(dmeb_chr_su_t2015, digital_money_emergence_boundary__consumer_holdings_reading, suppression_requirement, 2015, 0.3).
narrative_ontology:measurement(dmeb_chr_su_t2024, digital_money_emergence_boundary__consumer_holdings_reading, suppression_requirement, 2024, 0.31).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(digital_money_emergence_boundary__consumer_holdings_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(digital_money_emergence_boundary__consumer_holdings_reading, 0.12).
narrative_ontology:affects_constraint(digital_money_emergence_boundary__consumer_holdings_reading, digital_money_emergence_boundary__conceptualization_reading).
narrative_ontology:affects_constraint(digital_money_emergence_boundary__consumer_holdings_reading, digital_money_emergence_boundary__infrastructure_reading).
narrative_ontology:affects_constraint(digital_money_emergence_boundary__consumer_holdings_reading, stablecoin_regulatory_boundary).
narrative_ontology:affects_constraint(digital_money_emergence_boundary__consumer_holdings_reading, cbdc_design_authority).
narrative_ontology:affects_constraint(digital_money_emergence_boundary__consumer_holdings_reading, payment_services_directive_scope).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the digital_money_emergence_boundary kernel. The conceptualization_reading locates the boundary at theoretical formalization (1960s/1985); the infrastructure_reading at transfer rails (1967/1972/1977); this reading at consumer-held instruments (1990s/2000). Each reading has distinct beneficiaries, victims, and ε values. They are linked as a constraint family via affects_constraints. The ε-invariance principle requires separate stories because the extraction profile changes with the boundary location — the consumer-holdings reading has the highest extractiveness (0.42) because it is the boundary that enables fee extraction from end-users.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(digital_money_emergence_boundary__consumer_holdings_reading, institutional, 0.15).
constraint_indexing:directionality_override(digital_money_emergence_boundary__consumer_holdings_reading, powerless, 0.85).
constraint_indexing:directionality_override(digital_money_emergence_boundary__consumer_holdings_reading, moderate, 0.65).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
