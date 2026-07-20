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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
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
 *   human_readable: Consumer Holdings Reading of Digital Money Emergence Boundary
 *   domain: monetary_economics/financial_history/technology_governance
 *
 * SUMMARY:
 *   This constraint instantiates the consumer_holdings_reading of the
 *   contested digital_money_emergence_boundary kernel. Under this reading,
 *   digital money is defined as emerging only when consumers could directly
 *   hold and transact with digital instruments outside traditional bank
 *   accounts, crystallized in the 1990s e-purse experiments and the 2000
 *   Electronic Money Directive. The reading creates a legal and statistical
 *   boundary (M4/M5) that separates bank deposits from e-money, empowering
 *   regulatory bodies and fintech issuers while imposing costs on traditional
 *   banks and consumers. It is claimed as coordination (legal clarity for
 *   innovation) but operates with substantial extraction (regulatory
 *   expansion, market segmentation, institutional turf).
 *
 * KEY AGENTS:
 *   - Regulatory bodies (EMI/ECB): agenda-setter and beneficiary â define the M4/M5 boundary and capture expanded supervisory mandate.
 *   - Fintech issuers: beneficiary â operate inside the e-money license category created by the constraint.
 *   - Traditional banks: payer â lose deposit monopoly and face regulatory arbitrage.
 *   - Consumers: payer â bear fragmentation and compliance costs of the segmented system.
 *   - Infrastructure providers: excluded â their prior electronic transfer systems are narratively marginalized.
 *   - Monetary economists: observer â debate whether the boundary tracks economic reality.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(digital_money_emergence_boundary__consumer_holdings_reading, 0.65).
domain_priors:suppression_score(digital_money_emergence_boundary__consumer_holdings_reading, 0.6).
domain_priors:theater_ratio(digital_money_emergence_boundary__consumer_holdings_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(digital_money_emergence_boundary__consumer_holdings_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(digital_money_emergence_boundary__consumer_holdings_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(digital_money_emergence_boundary__consumer_holdings_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(digital_money_emergence_boundary__consumer_holdings_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(digital_money_emergence_boundary__consumer_holdings_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(digital_money_emergence_boundary__consumer_holdings_reading, tangled_rope).
narrative_ontology:human_readable(digital_money_emergence_boundary__consumer_holdings_reading, "Consumer Holdings Reading of Digital Money Emergence Boundary").
narrative_ontology:topic_domain(digital_money_emergence_boundary__consumer_holdings_reading, "monetary_economics/financial_history/technology_governance").

domain_priors:requires_active_enforcement(digital_money_emergence_boundary__consumer_holdings_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(digital_money_emergence_boundary__consumer_holdings_reading, 'c259df0d-83c2-4951-a91e-7298f2a5afc8').
narrative_ontology:cs_kernel_codification('c259df0d-83c2-4951-a91e-7298f2a5afc8', formalized).
narrative_ontology:cs_authority_grounding('c259df0d-83c2-4951-a91e-7298f2a5afc8', lineage).
narrative_ontology:cs_interpretation_layer_present('c259df0d-83c2-4951-a91e-7298f2a5afc8').
narrative_ontology:cs_reading_relation('c259df0d-83c2-4951-a91e-7298f2a5afc8', digital_money_emergence_boundary__conceptualization_reading, coexists_with).
narrative_ontology:cs_reading_relation('c259df0d-83c2-4951-a91e-7298f2a5afc8', digital_money_emergence_boundary__infrastructure_reading, influences).
narrative_ontology:cs_axiom('c259df0d-83c2-4951-a91e-7298f2a5afc8', foundational, consumer_custody_as_monetary_boundary).
narrative_ontology:cs_axiom_status(consumer_custody_as_monetary_boundary, holdable).
narrative_ontology:cs_axiom_grounding('c259df0d-83c2-4951-a91e-7298f2a5afc8', consumer_custody_as_monetary_boundary, conventional).
narrative_ontology:cs_axiom('c259df0d-83c2-4951-a91e-7298f2a5afc8', foundational, electronic_money_distinct_from_deposit_money).
narrative_ontology:cs_axiom_status(electronic_money_distinct_from_deposit_money, holdable).
narrative_ontology:cs_axiom_grounding('c259df0d-83c2-4951-a91e-7298f2a5afc8', electronic_money_distinct_from_deposit_money, conventional).
narrative_ontology:cs_reference_frame('c259df0d-83c2-4951-a91e-7298f2a5afc8', consumer_direct_custody_reference).
narrative_ontology:cs_drift_state('c259df0d-83c2-4951-a91e-7298f2a5afc8', post_crypto_stablecoin_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('c259df0d-83c2-4951-a91e-7298f2a5afc8', '').
narrative_ontology:cs_kernel_id(digital_money_emergence_boundary__consumer_holdings_reading, digital_money_emergence_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(digital_money_emergence_boundary__consumer_holdings_reading, regulatory_bodies).
narrative_ontology:constraint_beneficiary(digital_money_emergence_boundary__consumer_holdings_reading, fintech_issuers).
narrative_ontology:constraint_victim(digital_money_emergence_boundary__consumer_holdings_reading, traditional_banks).
narrative_ontology:constraint_victim(digital_money_emergence_boundary__consumer_holdings_reading, consumers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Define and maintain the legal categories of electronic money and the M4/M5 statistical boundary through directives such as the Electronic Money Directive and ECB statistical manuals. Supervise Electronic Money Institutions and determine what counts as digital money in official statistics, benefiting from expanded mandate and institutional relevance.
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__consumer_holdings_reading, regulatory_bodies, agenda_setter,
    institutional, generational, constrained, continental).
narrative_ontology:stakeholder_secondary_role(digital_money_emergence_boundary__consumer_holdings_reading, regulatory_bodies, beneficiary).

% Issue e-money products such as prepaid cards and digital wallets under licenses created by the regulatory boundary. Their business model depends on the legal distinction between e-money and bank deposits, allowing them to offer payment and storage services without full banking regulation.
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__consumer_holdings_reading, fintech_issuers, beneficiary,
    organized, biographical, constrained, continental).

% Lose the monopoly on digital value storage and face competition from e-money issuers operating under lighter regulatory regimes. Must absorb deposit-base fragmentation and regulatory arbitrage costs, or adapt by creating their own e-money products.
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__consumer_holdings_reading, traditional_banks, payer,
    powerful, biographical, mobile, continental).

% Hold digital monetary instruments outside traditional bank accounts. Bear fragmentation costs across non-interoperable schemes, variable consumer protection levels, and compliance overhead such as identity verification, without capturing the regulatory rent created by the boundary.
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__consumer_holdings_reading, consumers, payer,
    powerless, biographical, constrained, continental).

% Operate ACH networks, SWIFT, and bank transfer infrastructures that enabled electronic value movement before e-purses existed. Their claim that digital money emerged with electronic transfer infrastructure is structurally marginalized by the consumer-holdings regulatory narrative.
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__consumer_holdings_reading, infrastructure_providers, excluded,
    powerful, biographical, constrained, continental).

% Analyze whether the M4/M5 distinction and the consumer-holdings boundary track economic reality or serve institutional interests, debating whether bank deposits already constituted digital money before the regulatory boundary was drawn.
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__consumer_holdings_reading, monetary_economists, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Creates a legal and statistical framework that distinguishes bank deposits from electronic money, enabling non-bank institutions to issue digital payment instruments under clear regulatory supervision and consumer-protection rules.
% TRANSFER_FUNCTION: Moves regulatory authority and market opportunity from traditional deposit-taking banks to licensed e-money issuers and the supervisory bodies that define the categories; moves compliance costs and fragmentation friction to consumers.
% ABSENT_VOICES: Infrastructure providers and proponents of the infrastructure reading argue that digital money existed with ATMs and ACH in the 1970s; conceptualization proponents argue it began with theoretical possibility in the 1960s. Both are excluded from the ECB and EMI policy table where the consumer-holdings boundary is enforced.
% DISAPPEARANCE_RATIONALE: If the consumer-holdings boundary and M4/M5 distinction vanished, e-money issuers would lose their regulatory niche and either become banks or unregulated payment apps; the ECB and EMI would lose a major pillar of digital money oversight; traditional banks would regain clarity around deposit monopoly; consumer holdings would likely be reabsorbed into deposit insurance frameworks or left entirely unregulated.
% FOUNDING_PROBLEM: The lack of a legal category for non-bank digital value storage and transfer in the 1990s, creating uncertainty for prepaid card and electronic purse schemes and leaving gaps in consumer protection and supervisory oversight.
% FOUNDING_PROBLEM_CORROBORATION: Academic financial historians and early e-money pilot evaluations attest to the 1990s regulatory gap from outside the ECB and EMI beneficiary seat; competition authorities and traditional bank coalitions attest that the gap has been filled and the arrangement now serves market segmentation rather than the original consumer-protection purpose.
narrative_ontology:disappearance_verdict(digital_money_emergence_boundary__consumer_holdings_reading, world_rearranges).
narrative_ontology:founding_problem_status(digital_money_emergence_boundary__consumer_holdings_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(digital_money_emergence_boundary__consumer_holdings_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(digital_money_emergence_boundary__consumer_holdings_reading, 'none', 1).
narrative_ontology:epsilon_provenance(digital_money_emergence_boundary__consumer_holdings_reading, 0.65, 'kimi-k2.6', 'none', direct).

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
 *   Extractiveness rises from 0.20 to 0.65 over the interval as the EMD framework matures and fintech capture deepens. Suppression rises to 0.60 because the constraint's persistence requires actively marginalizing the infrastructure reading in policy discourse and enforcing the M4/M5 distinction against empirical blurring. Theater reaches 0.45 because a growing share of regulatory activity maintains the boundary for institutional turf reasons rather than live consumer-protection needs. The measurement grid is shared across all three tracked metrics to prevent temporal misalignment artifacts.
 *
 * PERSPECTIVAL GAP:
 *   The regulatory seat experiences the constraint as necessary coordination providing legal certainty; the fintech seat experiences it as enabling infrastructure. The traditional bank and consumer seats experience the same structure as enforced extraction that segments markets and fragments protections. The engine computes this divergence from the structural data rather than adjudicating it.
 *
 * DIRECTIONALITY LOGIC:
 *   Regulatory bodies and fintech issuers are declared beneficiaries, placing their directionality near the subsidy end; traditional banks and consumers are declared victims, placing their directionality near the target end. The beneficiary-victim asymmetry is what makes this a tangled rope rather than a rope or a snare.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as tangled rope prevents mislabeling the arrangement as pure extraction (it does solve a genuine 1990s legal-gap coordination problem) and prevents mislabeling it as pure coordination (the boundary now serves regulatory expansion and fintech privilege beyond the original consumer-protection mandate). The contested founding_problem_status signals that the mandate's obsolescence is disputed, which the engine reads as a live diagnostic rather than a settled verdict.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    regulatory_construct_vs_natural_category,
    'Is the M4/M5 distinction and the consumer-holdings boundary a natural economic category or a regulatory construct that privileges fintech issuers and supervisory bodies?',
    'Comparative jurisdictional analysis of monetary aggregation frameworks and historical study of central bank statistical definitions.',
    'If purely constructed, the constraint leans toward snare; if tracking a real structural economic difference, tangled_rope classification is stable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_construct_vs_natural_category, conceptual, 'Whether the monetary boundary is natural or regulatory').

omega_variable(
    emd_obsolescence,
    'Has the Electronic Money Directive''s founding problem (1990s legal uncertainty for e-purses) been rendered obsolete by subsequent payment innovations, leaving the constraint as inertial market segmentation?',
    'Empirical analysis of whether current EMI frameworks address live risks or preserve 2000s categories for institutional benefit.',
    'If obsolescent, drift toward piton increases; if still solving live coordination problems, tangled_rope persists.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(emd_obsolescence, empirical, 'Whether the founding problem is dead or live').

omega_variable(
    kernel_reading_contest,
    'Does the consumer-holdings boundary represent the definitive emergence of digital money, or is it one contested reading among siblings that would redistribute agency and victimhood?',
    'Comparison with sibling constraints (conceptualization_reading, infrastructure_reading) and cross-jurisdictional analysis of monetary boundaries.',
    'If the reading is contingent, its epsilon and type are reading-relative rather than kernel-absolute.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Contested kernel reading ambiguity for digital money boundary').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(digital_money_emergence_boundary__consumer_holdings_reading, 0, 34).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(digi_tr_t0, digital_money_emergence_boundary__consumer_holdings_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(digi_tr_t6, digital_money_emergence_boundary__consumer_holdings_reading, theater_ratio, 6, 0.15).
narrative_ontology:measurement(digi_tr_t12, digital_money_emergence_boundary__consumer_holdings_reading, theater_ratio, 12, 0.25).
narrative_ontology:measurement(digi_tr_t18, digital_money_emergence_boundary__consumer_holdings_reading, theater_ratio, 18, 0.3).
narrative_ontology:measurement(digi_tr_t24, digital_money_emergence_boundary__consumer_holdings_reading, theater_ratio, 24, 0.35).
narrative_ontology:measurement(digi_tr_t30, digital_money_emergence_boundary__consumer_holdings_reading, theater_ratio, 30, 0.4).
narrative_ontology:measurement(digi_tr_t34, digital_money_emergence_boundary__consumer_holdings_reading, theater_ratio, 34, 0.45).

% Extraction over time
narrative_ontology:measurement(digi_be_t0, digital_money_emergence_boundary__consumer_holdings_reading, base_extractiveness, 0, 0.2).
narrative_ontology:measurement(digi_be_t6, digital_money_emergence_boundary__consumer_holdings_reading, base_extractiveness, 6, 0.3).
narrative_ontology:measurement(digi_be_t12, digital_money_emergence_boundary__consumer_holdings_reading, base_extractiveness, 12, 0.45).
narrative_ontology:measurement(digi_be_t18, digital_money_emergence_boundary__consumer_holdings_reading, base_extractiveness, 18, 0.52).
narrative_ontology:measurement(digi_be_t24, digital_money_emergence_boundary__consumer_holdings_reading, base_extractiveness, 24, 0.58).
narrative_ontology:measurement(digi_be_t30, digital_money_emergence_boundary__consumer_holdings_reading, base_extractiveness, 30, 0.62).
narrative_ontology:measurement(digi_be_t34, digital_money_emergence_boundary__consumer_holdings_reading, base_extractiveness, 34, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(digi_su_t0, digital_money_emergence_boundary__consumer_holdings_reading, suppression_requirement, 0, 0.2).
narrative_ontology:measurement(digi_su_t6, digital_money_emergence_boundary__consumer_holdings_reading, suppression_requirement, 6, 0.3).
narrative_ontology:measurement(digi_su_t12, digital_money_emergence_boundary__consumer_holdings_reading, suppression_requirement, 12, 0.4).
narrative_ontology:measurement(digi_su_t18, digital_money_emergence_boundary__consumer_holdings_reading, suppression_requirement, 18, 0.48).
narrative_ontology:measurement(digi_su_t24, digital_money_emergence_boundary__consumer_holdings_reading, suppression_requirement, 24, 0.55).
narrative_ontology:measurement(digi_su_t30, digital_money_emergence_boundary__consumer_holdings_reading, suppression_requirement, 30, 0.58).
narrative_ontology:measurement(digi_su_t34, digital_money_emergence_boundary__consumer_holdings_reading, suppression_requirement, 34, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(digital_money_emergence_boundary__consumer_holdings_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(digital_money_emergence_boundary__consumer_holdings_reading, digital_money_emergence_boundary__infrastructure_reading).

% DUAL FORMULATION NOTE:
% The kernel 'digital_money_emergence_boundary' decomposes into three readings because the label 'digital money' conflates theoretically distinct claims: theoretical conceivability (conceptualization_reading), infrastructure-enabled transfer (infrastructure_reading), and direct consumer custody (consumer_holdings_reading). Each reading has different epsilon, beneficiary structure, and temporal boundary. This file instantiates the consumer_holdings_reading.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
