% ============================================================================
% CONSTRAINT STORY: digital_money_emergence_boundary__consumer_holdings_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
 *   human_readable: Consumer-Holdings Boundary for Digital Money Emergence (E-Money Directive Reading)
 *   domain: monetary_economics/financial_history/technology_governance
 *
 * SUMMARY:
 *   This story instantiates the LATEST of three contested boundaries for when
 *   'digital money' emerged: the point at which consumers could directly hold
 *   and transact with digital instruments outside traditional bank accounts,
 *   crystallized in the 1990s e-purse experiments (Mondex, Proton) and
 *   formalized by the EU's 2000 Electronic Money Directive (EMD). This
 *   reading treats emergence as a regulatory-categorization event, not a
 *   technological or conceptual one — money 'emerges' when law recognizes a
 *   new holding relationship and creates the M4/M5 statistical split to track
 *   it. This is deliberately NOT the infrastructure reading (1967 ATMs / 1972
 *   ACH / 1977 SWIFT — electronic transfer capability) or the
 *   conceptualization reading (1960s telecom advances / 1985 Chaum
 *   formalization — theoretical thinkability). Those are separate constraints
 *   with separate ε values and separate beneficiary structures; they are
 *   linked here only through the shared kernel, not merged into this story's
 *   classification.
 *
 * KEY AGENTS:
 *   - e_money_institution_regulators: Primary agenda-setter (institutional/analytical) — defines the legal category and its enforcement
 *   - ecb_monetary_authority: Primary beneficiary (institutional/analytical) — gains statistical and policy visibility over a newly bounded aggregate
 *   - fintech_emoney_issuers: Secondary beneficiary (organized/mobile) — gains a licensed market niche created by the boundary
 *   - unlicensed_prepaid_instrument_issuers: Primary target (moderate/trapped) — bears compliance cost or market exit
 *   - cross_border_emoney_consumers: Diffuse payer (powerless/constrained) — bears protection uncertainty at jurisdictional edges
 *   - cash_dependent_populations: Excluded (powerless/trapped) — structurally outside the category the boundary protects
 *   - monetary_historians: Analytical observer — compares this boundary against sibling boundaries
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(digital_money_emergence_boundary__consumer_holdings_reading, 0.42).
domain_priors:suppression_score(digital_money_emergence_boundary__consumer_holdings_reading, 0.38).
domain_priors:theater_ratio(digital_money_emergence_boundary__consumer_holdings_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(digital_money_emergence_boundary__consumer_holdings_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(digital_money_emergence_boundary__consumer_holdings_reading, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(digital_money_emergence_boundary__consumer_holdings_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(digital_money_emergence_boundary__consumer_holdings_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(digital_money_emergence_boundary__consumer_holdings_reading, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(digital_money_emergence_boundary__consumer_holdings_reading, tangled_rope).
narrative_ontology:human_readable(digital_money_emergence_boundary__consumer_holdings_reading, "Consumer-Holdings Boundary for Digital Money Emergence (E-Money Directive Reading)").
narrative_ontology:topic_domain(digital_money_emergence_boundary__consumer_holdings_reading, "monetary_economics/financial_history/technology_governance").

domain_priors:requires_active_enforcement(digital_money_emergence_boundary__consumer_holdings_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(digital_money_emergence_boundary__consumer_holdings_reading, '0e97d097-ed8e-4bde-89be-ca12a8150171').
narrative_ontology:cs_kernel_codification('0e97d097-ed8e-4bde-89be-ca12a8150171', formalized).
narrative_ontology:cs_authority_grounding('0e97d097-ed8e-4bde-89be-ca12a8150171', extraction).
narrative_ontology:cs_interpretation_layer_present('0e97d097-ed8e-4bde-89be-ca12a8150171').
narrative_ontology:cs_reading_relation('0e97d097-ed8e-4bde-89be-ca12a8150171', digital_money_emergence_boundary__conceptualization_reading, coexists_with).
narrative_ontology:cs_reading_relation('0e97d097-ed8e-4bde-89be-ca12a8150171', digital_money_emergence_boundary__infrastructure_reading, influences).
narrative_ontology:cs_axiom('0e97d097-ed8e-4bde-89be-ca12a8150171', foundational, money_requires_direct_consumer_holding_outside_banks).
narrative_ontology:cs_axiom_status(money_requires_direct_consumer_holding_outside_banks, holdable).
narrative_ontology:cs_axiom_grounding('0e97d097-ed8e-4bde-89be-ca12a8150171', money_requires_direct_consumer_holding_outside_banks, conventional).
narrative_ontology:cs_axiom('0e97d097-ed8e-4bde-89be-ca12a8150171', secondary, regulatory_categorization_constitutes_monetary_reality).
narrative_ontology:cs_axiom_status(regulatory_categorization_constitutes_monetary_reality, holdable).
narrative_ontology:cs_axiom_grounding('0e97d097-ed8e-4bde-89be-ca12a8150171', regulatory_categorization_constitutes_monetary_reality, conventional).
narrative_ontology:cs_reference_frame('0e97d097-ed8e-4bde-89be-ca12a8150171', emd_2000_consumer_holdings_definition).
narrative_ontology:cs_drift_state('0e97d097-ed8e-4bde-89be-ca12a8150171', post_stablecoin_mobile_money_expansion, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('0e97d097-ed8e-4bde-89be-ca12a8150171', '').
narrative_ontology:cs_kernel_id(digital_money_emergence_boundary__consumer_holdings_reading, digital_money_emergence_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(digital_money_emergence_boundary__consumer_holdings_reading, e_money_institution_regulators).
narrative_ontology:constraint_beneficiary(digital_money_emergence_boundary__consumer_holdings_reading, ecb_monetary_authority).
narrative_ontology:constraint_beneficiary(digital_money_emergence_boundary__consumer_holdings_reading, fintech_emoney_issuers).
narrative_ontology:constraint_victim(digital_money_emergence_boundary__consumer_holdings_reading, unlicensed_prepaid_instrument_issuers).
narrative_ontology:constraint_victim(digital_money_emergence_boundary__consumer_holdings_reading, cross_border_emoney_consumers).
narrative_ontology:constraint_victim(digital_money_emergence_boundary__consumer_holdings_reading, cash_dependent_populations).
narrative_ontology:constraint_vindicates(digital_money_emergence_boundary__consumer_holdings_reading, m4_m5_monetary_aggregate_distinction).
narrative_ontology:constraint_vindicates(digital_money_emergence_boundary__consumer_holdings_reading, regulatory_categorization_of_money_forms).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Draft and enforce the legal definition of 'electronic money' (the 2000 EMD and successor directives), setting capital requirements, licensing regimes, and redemption rules that determine which digital instruments count as money at all. Their categorization work becomes the basis for M4/M5 statistical separation and for who may lawfully issue consumer-held digital value.
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__consumer_holdings_reading, e_money_institution_regulators, agenda_setter,
    institutional, generational, analytical, continental).

% Uses the consumer-holdings boundary to decide what belongs in monetary aggregates and financial stability oversight. The boundary lets the ECB treat e-money balances as a distinct, monitorable category separate from bank deposits, reinforcing its authority over what counts as 'money' for policy purposes.
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__consumer_holdings_reading, ecb_monetary_authority, beneficiary,
    institutional, civilizational, analytical, continental).
narrative_ontology:stakeholder_secondary_role(digital_money_emergence_boundary__consumer_holdings_reading, ecb_monetary_authority, agenda_setter).

% Obtain licenses under the e-money framework to issue prepaid cards, digital wallets, and mobile money products directly to consumers, competing with banks without needing a full banking license. The boundary's existence is what creates their regulatory niche and market opportunity.
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__consumer_holdings_reading, fintech_emoney_issuers, beneficiary,
    organized, biographical, mobile, continental).

% Operated closed-loop or informal prepaid schemes before or outside the licensing regime; the consumer-holdings boundary criminalizes or forces costly relicensing of their instruments once regulators declare that consumer-held digital value constitutes regulated e-money. Many were unable to absorb capital and safeguarding requirements and exited the market.
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__consumer_holdings_reading, unlicensed_prepaid_instrument_issuers, payer,
    moderate, biographical, trapped, national).

% Hold e-money balances across jurisdictions with inconsistent redemption guarantees and consumer protection depending on where the issuer is licensed; the consumer-holdings definitional boundary determines whether their balance is protected as regulated e-money or left as an unprotected contractual claim, and they have no direct say in where that line is drawn.
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__consumer_holdings_reading, cross_border_emoney_consumers, payer,
    powerless, biographical, constrained, continental).

% Remain outside the entire e-money framework because they lack the identification, banking access, or smartphone infrastructure to hold digital balances at all. The consumer-holdings boundary, by making 'holding digital value' the test for monetary participation, structurally excludes them from the category the regulation is meant to protect.
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__consumer_holdings_reading, cash_dependent_populations, excluded,
    powerless, biographical, trapped, regional).

% Study when 'digital money' properly emerged, comparing this consumer-holdings boundary against the earlier infrastructure and conceptualization boundaries, and noting that each boundary serves the interests of the institutions that draw it.
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__consumer_holdings_reading, monetary_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(digital_money_emergence_boundary__consumer_holdings_reading, ecb_monetary_authority).
narrative_ontology:fixing_cost_class(digital_money_emergence_boundary__consumer_holdings_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a common legal and statistical definition of e-money so that regulators, issuers, and consumers share a stable understanding of which digital instruments carry redemption guarantees, prudential requirements, and monetary-policy visibility — solving the genuine problem of distinguishing regulated stored value from unregulated IOUs or bank deposits.
% TRANSFER_FUNCTION: Moves regulatory legitimacy and market access toward licensed e-money institutions and the authorities that define the category, while moving compliance costs onto smaller/informal issuers and moving protection uncertainty onto consumers whose balances fall near the boundary's edge.
% ABSENT_VOICES: Cash-dependent and unbanked populations have no seat in defining what counts as 'holding' digital money, despite being the group for whom the boundary's threshold effectively decides inclusion or exclusion from the formal monetary system. Informal prepaid issuers in the Global South, whose products predate or fall outside EU-style licensing, are also not consulted when the boundary is exported as a template.
% DISAPPEARANCE_RATIONALE: Regulators and the ECB would say the world rearranges sharply — without the consumer-holdings boundary, e-money issuers would operate without capital/safeguarding rules and monetary statistics would blur bank deposits with prepaid balances. Fintech issuers and monetary historians note the underlying technology and consumer practice would persist unchanged; only the legal category and its compliance apparatus would vanish, meaning the 'boundary' is a governance artifact layered on infrastructure and practice that already existed by the 1990s.
% FOUNDING_PROBLEM: Regulators needed to determine whether stored monetary value that consumers could hold and spend directly on digital devices (smart cards, mobile wallets) constituted 'money' requiring bank-style prudential regulation, or an unregulated commercial instrument — the 1990s proliferation of e-purses (Mondex, Proton) created products that did not fit existing banking or payment-services law.
% FOUNDING_PROBLEM_CORROBORATION: The European Commission's own EMD review documents and academic monetary economists outside the regulatory bodies (e.g. critiques from BIS working papers and independent central-bank researchers) attest the definitional problem was real in 2000 but argue the boundary has since been used opportunistically to expand regulatory jurisdiction over new instruments (mobile money, stablecoins) well past the original e-purse problem it was built to solve.
narrative_ontology:disappearance_verdict(digital_money_emergence_boundary__consumer_holdings_reading, contested).
narrative_ontology:founding_problem_status(digital_money_emergence_boundary__consumer_holdings_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(digital_money_emergence_boundary__consumer_holdings_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(digital_money_emergence_boundary__consumer_holdings_reading, 'none', 1).
narrative_ontology:epsilon_provenance(digital_money_emergence_boundary__consumer_holdings_reading, 0.42, 'claude-sonnet-5', 'none', direct).

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
 *   Extraction is moderate (0.42 at 2024) because the coordination function is real — a stable legal definition of e-money genuinely reduces ambiguity for consumers and prevents outright fraud in stored-value products — but the definitional power itself is a rent: regulators and the ECB gain jurisdictional reach and statistical authority, and licensed fintech issuers gain a protected market segment, while informal issuers and boundary-adjacent consumers bear the cost of being defined in or out. Suppression (0.38) reflects the licensing and enforcement apparatus needed to keep unlicensed prepaid schemes from operating, which has hardened over the interval as e-money volumes grew (1990: nascent e-purse pilots era, low suppression; 2024: mature licensing regime with active enforcement against unlicensed issuers). Theater ratio is modest (0.28) — most of the regulatory activity is functional categorization and prudential oversight, not pure performance, though an increasing share is definitional boundary-policing (deciding whether mobile money, stablecoins, or new instruments fall inside the 2000-era category) rather than protecting the original e-purse consumer.
 *
 * DIRECTIONALITY LOGIC:
 *   Regulators and the ECB sit near the full-beneficiary end: they created the category, administer it, and gain analytical/monetary authority from its existence without bearing its compliance costs. Fintech e-money issuers are moderate beneficiaries — organized, mobile exit options, they chose to enter the regulated category because it grants them market access banks would otherwise monopolize. Unlicensed prepaid issuers are structural targets — trapped by capital requirements they often cannot meet, their prior business model is retroactively defined as either regulated (with a mandatory transition or exit) or illegal. Cross-border consumers are diffuse payers: powerless individually, they bear the risk of inconsistent redemption protection at the boundary's jurisdictional edges. Cash-dependent populations are excluded entirely rather than exploited directly — the boundary's harm to them is exclusion from a protective regime, not extraction through it.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (distinguishing regulated stored value from ad hoc commercial instruments in the 1990s e-purse era) was genuinely live in 2000. Whether it remains live in 2024 is contested: critics argue the same definitional apparatus is now used to extend regulatory reach over mobile money and stablecoins that bear little resemblance to Mondex-style e-purses, turning a narrow consumer-protection fix into a general-purpose jurisdictional claim over 'what counts as money.' This is the seat divergence the classification should surface: from the regulator's seat, the boundary is still solving its original problem (rope-like); from the unlicensed issuer's seat, the same boundary is an enforced barrier to market entry dressed in consumer-protection language (tangled-rope-to-snare drift). The tangled_rope classification holds both readings simultaneously rather than forcing a premature verdict.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    consumer_holdings_boundary_naturalness,
    'Is the consumer-holdings boundary a discovery of when digital money ''really'' emerged, or a constructed regulatory category that serves the institutions empowered to draw it?',
    'Compare whether the M4/M5 statistical distinction tracks any pre-existing economically meaningful discontinuity in consumer behavior, or whether it was created post hoc to justify a licensing regime that regulators and incumbent issuers had independent reasons to want.',
    'If the boundary tracks a real behavioral/economic discontinuity, the tangled_rope classification''s coordination component is well-grounded. If the boundary is primarily a jurisdictional convenience, the extraction component dominates and the classification should drift toward snare over time.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(consumer_holdings_boundary_naturalness, conceptual, 'Whether the 2000 EMD boundary reflects economic reality or regulatory convenience.').

omega_variable(
    kernel_reading_disagreement_location,
    'Where exactly do the three sibling readings (conceptualization, infrastructure, consumer_holdings) disagree — is it about a factual sequence of events, or about what ''money'' fundamentally requires (holdability vs. transferability vs. thinkability)?',
    'Examine whether proponents of each reading would change their view given the same historical facts, or whether they hold different definitions of ''money'' such that no amount of additional history would converge them.',
    'If the disagreement is definitional rather than factual, no single ''true'' emergence date exists and all three readings should persist as coexisting constraints rather than one superseding the others.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_disagreement_location, conceptual, 'Whether the kernel contest is empirical or definitional in nature.').

omega_variable(
    boundary_creep_into_new_instruments,
    'Has the consumer-holdings regulatory category, built for 1990s e-purses, been extended to stablecoins and mobile money in ways that exceed its original coordination function?',
    'Track EU/EBA guidance and enforcement actions post-2015 to see whether new instrument types are being folded into e-money licensing primarily to protect consumers or primarily to extend regulatory jurisdiction and incumbent issuer protection.',
    'If boundary creep is substantial, the founding_problem_status should be read as ''dead'' relative to new instruments even though ''live'' for the original e-purse case — supporting a mandatrophy reading for the extended scope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(boundary_creep_into_new_instruments, empirical, 'Whether the e-money category''s scope has expanded beyond its founding problem.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(digital_money_emergence_boundary__consumer_holdings_reading, 1990, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(digi_tr_t1990, digital_money_emergence_boundary__consumer_holdings_reading, theater_ratio, 1990, 0.1).
narrative_ontology:measurement(digi_tr_t1996, digital_money_emergence_boundary__consumer_holdings_reading, theater_ratio, 1996, 0.13).
narrative_ontology:measurement(digi_tr_t2000, digital_money_emergence_boundary__consumer_holdings_reading, theater_ratio, 2000, 0.16).
narrative_ontology:measurement(digi_tr_t2009, digital_money_emergence_boundary__consumer_holdings_reading, theater_ratio, 2009, 0.2).
narrative_ontology:measurement(digi_tr_t2015, digital_money_emergence_boundary__consumer_holdings_reading, theater_ratio, 2015, 0.24).
narrative_ontology:measurement(digi_tr_t2024, digital_money_emergence_boundary__consumer_holdings_reading, theater_ratio, 2024, 0.28).

% Extraction over time
narrative_ontology:measurement(digi_be_t1990, digital_money_emergence_boundary__consumer_holdings_reading, base_extractiveness, 1990, 0.18).
narrative_ontology:measurement(digi_be_t1996, digital_money_emergence_boundary__consumer_holdings_reading, base_extractiveness, 1996, 0.22).
narrative_ontology:measurement(digi_be_t2000, digital_money_emergence_boundary__consumer_holdings_reading, base_extractiveness, 2000, 0.28).
narrative_ontology:measurement(digi_be_t2009, digital_money_emergence_boundary__consumer_holdings_reading, base_extractiveness, 2009, 0.33).
narrative_ontology:measurement(digi_be_t2015, digital_money_emergence_boundary__consumer_holdings_reading, base_extractiveness, 2015, 0.38).
narrative_ontology:measurement(digi_be_t2024, digital_money_emergence_boundary__consumer_holdings_reading, base_extractiveness, 2024, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(digi_su_t1990, digital_money_emergence_boundary__consumer_holdings_reading, suppression_requirement, 1990, 0.15).
narrative_ontology:measurement(digi_su_t1996, digital_money_emergence_boundary__consumer_holdings_reading, suppression_requirement, 1996, 0.2).
narrative_ontology:measurement(digi_su_t2000, digital_money_emergence_boundary__consumer_holdings_reading, suppression_requirement, 2000, 0.3).
narrative_ontology:measurement(digi_su_t2009, digital_money_emergence_boundary__consumer_holdings_reading, suppression_requirement, 2009, 0.34).
narrative_ontology:measurement(digi_su_t2015, digital_money_emergence_boundary__consumer_holdings_reading, suppression_requirement, 2015, 0.36).
narrative_ontology:measurement(digi_su_t2024, digital_money_emergence_boundary__consumer_holdings_reading, suppression_requirement, 2024, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(digital_money_emergence_boundary__consumer_holdings_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(digital_money_emergence_boundary__consumer_holdings_reading, 0.12).
narrative_ontology:affects_constraint(digital_money_emergence_boundary__consumer_holdings_reading, digital_money_emergence_boundary__conceptualization_reading).
narrative_ontology:affects_constraint(digital_money_emergence_boundary__consumer_holdings_reading, digital_money_emergence_boundary__infrastructure_reading).

% DUAL FORMULATION NOTE:
% This story is one of three siblings decomposing the colloquial label 'when did digital money emerge' per the ε-invariance principle. The infrastructure_reading (1967 ATMs/1972 ACH/1977 SWIFT) has the lowest ε — pure interbank/wholesale coordination with negligible consumer-facing extraction; it likely classifies as rope or mountain-adjacent. The conceptualization_reading (1960s telecom/1985 Chaum) is a near-mountain claim about theoretical possibility with essentially no identifiable beneficiary/victim structure at the time of the claim itself. This consumer_holdings_reading has the highest ε of the three because it is the only reading built around an active, enforced legal category (the EMD) with concentrated institutional beneficiaries and identifiable payers. The infrastructure reading is upstream of this one: interbank electronic transfer capability was a precondition for e-purses and EMI licensing to be technically meaningful at all.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
