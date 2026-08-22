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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   human_readable: Consumer Holdings Boundary for Digital Money Emergence
 *   domain: monetary_economics/financial_history/technology_governance
 *
 * SUMMARY:
 *   This constraint story models the consumer-holdings reading of the
 *   digital_money_emergence_boundary kernel: the legal and statistical claim
 *   that digital money properly emerges when consumers can directly hold and
 *   transact with digital instruments outside traditional bank accounts
 *   (1990s e-purses, 2000 EU EMD). The constraint is the institutionalized
 *   boundary itself â enforced through ECB monetary aggregates (M4/M5
 *   separation) and EMI licensing â which coordinates a category for
 *   non-bank digital value while asymmetrically concentrating regulatory
 *   authority and market rents.
 *
 * KEY AGENTS:
 *   - regulatory_bodies (ECB/EMI): Agenda-setter (institutional/analytical) â defines monetary categories and licensing frameworks
 *   - fintech_issuers: Beneficiary (organized/constrained) â operate within the licensed e-money category
 *   - traditional_depository_institutions: Payer (powerful/constrained) â face competitive and analytical costs from the bank/non-bank boundary
 *   - consumers: Payer (powerless/constrained) â bear fragmentation and scheme compliance costs
 *   - monetary_historians: Observer (analytical/analytical) â track competing emergence narratives
 *   - legacy_infrastructure_operators: Excluded (powerful/constrained) â infrastructure enabling electronic transfer is reclassified as pre-digital
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(digital_money_emergence_boundary__consumer_holdings_reading, 0.63).
domain_priors:suppression_score(digital_money_emergence_boundary__consumer_holdings_reading, 0.65).
domain_priors:theater_ratio(digital_money_emergence_boundary__consumer_holdings_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(digital_money_emergence_boundary__consumer_holdings_reading, extractiveness, 0.63).
narrative_ontology:constraint_metric(digital_money_emergence_boundary__consumer_holdings_reading, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(digital_money_emergence_boundary__consumer_holdings_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(digital_money_emergence_boundary__consumer_holdings_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(digital_money_emergence_boundary__consumer_holdings_reading, resistance, 0.42).

% --- Constraint claim ---
narrative_ontology:constraint_claim(digital_money_emergence_boundary__consumer_holdings_reading, tangled_rope).
narrative_ontology:human_readable(digital_money_emergence_boundary__consumer_holdings_reading, "Consumer Holdings Boundary for Digital Money Emergence").
narrative_ontology:topic_domain(digital_money_emergence_boundary__consumer_holdings_reading, "monetary_economics/financial_history/technology_governance").

domain_priors:requires_active_enforcement(digital_money_emergence_boundary__consumer_holdings_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(digital_money_emergence_boundary__consumer_holdings_reading, '3e6f4758-2d4f-4aa3-958d-1c6554b7b5a6').
narrative_ontology:cs_kernel_codification('3e6f4758-2d4f-4aa3-958d-1c6554b7b5a6', formalized).
narrative_ontology:cs_authority_grounding('3e6f4758-2d4f-4aa3-958d-1c6554b7b5a6', lineage).
narrative_ontology:cs_interpretation_layer_present('3e6f4758-2d4f-4aa3-958d-1c6554b7b5a6').
narrative_ontology:cs_reading_relation('3e6f4758-2d4f-4aa3-958d-1c6554b7b5a6', digital_money_emergence_boundary__conceptualization_reading, coexists_with).
narrative_ontology:cs_reading_relation('3e6f4758-2d4f-4aa3-958d-1c6554b7b5a6', digital_money_emergence_boundary__infrastructure_reading, influences).
narrative_ontology:cs_axiom('3e6f4758-2d4f-4aa3-958d-1c6554b7b5a6', foundational, consumer_holding_as_monetary_threshold).
narrative_ontology:cs_axiom_status(consumer_holding_as_monetary_threshold, holdable).
narrative_ontology:cs_axiom_grounding('3e6f4758-2d4f-4aa3-958d-1c6554b7b5a6', consumer_holding_as_monetary_threshold, conventional).
narrative_ontology:cs_axiom('3e6f4758-2d4f-4aa3-958d-1c6554b7b5a6', foundational, legal_categorization_authority_over_monetary_ontology).
narrative_ontology:cs_axiom_status(legal_categorization_authority_over_monetary_ontology, holdable).
narrative_ontology:cs_axiom_grounding('3e6f4758-2d4f-4aa3-958d-1c6554b7b5a6', legal_categorization_authority_over_monetary_ontology, conventional).
narrative_ontology:cs_reference_frame('3e6f4758-2d4f-4aa3-958d-1c6554b7b5a6', legal_consumer_holding_framework).
narrative_ontology:cs_drift_state('3e6f4758-2d4f-4aa3-958d-1c6554b7b5a6', contemporary_instant_payment_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('3e6f4758-2d4f-4aa3-958d-1c6554b7b5a6', '').
narrative_ontology:cs_kernel_id(digital_money_emergence_boundary__consumer_holdings_reading, digital_money_emergence_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(digital_money_emergence_boundary__consumer_holdings_reading, regulatory_bodies).
narrative_ontology:constraint_beneficiary(digital_money_emergence_boundary__consumer_holdings_reading, fintech_issuers).
narrative_ontology:constraint_victim(digital_money_emergence_boundary__consumer_holdings_reading, traditional_depository_institutions).
narrative_ontology:constraint_victim(digital_money_emergence_boundary__consumer_holdings_reading, consumers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Defines and maintains the legal boundary between bank deposits and e-money through the EMD and ECB monetary aggregates (M4/M5 distinction). Sets licensing requirements for e-money issuers and controls the statistical ontology of digital money in EU policy.
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__consumer_holdings_reading, regulatory_bodies, agenda_setter,
    institutional, generational, analytical, continental).

% Issue e-money products under EMI licenses, benefiting from legal recognition as non-bank money issuers. Must comply with float maintenance and supervisory reporting but gain a protected market category distinct from deposit-taking banks.
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__consumer_holdings_reading, fintech_issuers, beneficiary,
    organized, biographical, constrained, continental).

% Face competitive and analytical costs from the consumer-holdings boundary, which legitimates non-bank money-like instruments and forces continuous distinction between deposits and e-money on their balance sheets and in regulatory reporting.
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__consumer_holdings_reading, traditional_depository_institutions, payer,
    powerful, generational, constrained, continental).

% Can hold digital value outside bank accounts under the boundary, but face scheme fragmentation, interoperability gaps, KYC compliance burdens, and weaker prudential protection than bank deposits across competing e-money platforms.
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__consumer_holdings_reading, consumers, payer,
    powerless, biographical, constrained, continental).

% Study competing emergence narratives for digital money without being bound to the regulatory definitions enforced by ECB/EMI frameworks.
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__consumer_holdings_reading, monetary_historians, observer,
    analytical, generational, analytical, global).

% Operate ACH, SWIFT, and ATM networks that enabled electronic transfer before consumer e-money. Their infrastructure is reclassified as bank-intermediated or pre-digital under the consumer-holdings boundary, excluding them from official digital-money categories.
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__consumer_holdings_reading, legacy_infrastructure_operators, excluded,
    powerful, biographical, constrained, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Creates a legally enforceable category for non-bank digital value instruments, enabling statistical measurement, consumer protection, and supervisory oversight distinct from traditional bank deposits.
% TRANSFER_FUNCTION: Moves regulatory authority over monetary definitions from traditional banking statutes to the EMI/ECB framework, and transfers market legitimacy from depository institutions to licensed e-money issuers, by establishing consumer direct-holding as the threshold criterion.
% ABSENT_VOICES: Monetary historians and economists who privilege infrastructure-first or conceptualization-first emergence narratives are marginalized in regulatory discourse; legacy electronic transfer operators are excluded from the digital-money category despite having built the preconditions.
% DISAPPEARANCE_RATIONALE: If the consumer-holdings boundary vanished, the M4/M5 statistical distinction would collapse, EMI-licensed firms would face reclassification as payment institutions or banks, ECB monetary aggregates would require reconstruction, and the competitive frontier between bank deposits and e-money would reorganize around different regulatory criteria.
% FOUNDING_PROBLEM: The rise of stored-value cards and early e-purses in the 1990s created unregulated money-like liabilities outside banking law, threatening monetary statistical coherence and consumer protection gaps.
% FOUNDING_PROBLEM_CORROBORATION: ECB and EU Commission attest the problem required legal categorization; banking historians corroborate that pre-EMD innovation occurred in a supervisory grey area; consumer advocates and competition economists from outside the beneficiary set note that the EMD solution created new market distortions and fragmentation costs.
narrative_ontology:disappearance_verdict(digital_money_emergence_boundary__consumer_holdings_reading, world_rearranges).
narrative_ontology:founding_problem_status(digital_money_emergence_boundary__consumer_holdings_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(digital_money_emergence_boundary__consumer_holdings_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(digital_money_emergence_boundary__consumer_holdings_reading, 'none', 1).
narrative_ontology:epsilon_provenance(digital_money_emergence_boundary__consumer_holdings_reading, 0.63, 'kimi-k2.6', 'none', direct).

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
 *   Extractiveness (0.63 at interval end) is moderate-high because the consumer-holdings boundary creates regulatory rents through licensing barriers and definitional authority. Suppression (0.65) reflects the active exclusion of alternative monetary ontologies (infrastructure-first, conceptualization-first) from official ECB statistics and legal curricula. Theater_ratio (0.45) captures the increasing share of activity devoted to maintaining the M4/M5 distinction as fintech practice drifts from the EMD reference frame. Accessibility_collapse (0.68) is high because once the EMI framework is adopted, alternative statistical classifications become nearly invisible in EU policy discourse. Resistance (0.42) is moderate: traditional banks and alternative monetary theorists contest the boundary, but the regulatory agenda-setting power of ECB/EMI limits effective challenge.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat (regulatory_bodies) experiences the constraint as necessary coordination â without clear categories, monetary supervision would fail. The payer seats (traditional banks, consumers) experience the same structure as imposed categorization that fragments the payment landscape and erodes deposit franchises. The engine computes this divergence from structural data: identical constraint, opposite directionality.
 *
 * DIRECTIONALITY LOGIC:
 *   Regulatory bodies and fintech issuers are declared beneficiaries, deriving low directionality. Regulatory bodies capture definitional authority (the power to draw the M4/M5 line); fintech issuers capture licensed market access. Traditional depository institutions and consumers are declared victims/payers, deriving high directionality. Banks pay through competitive erosion of their deposit monopoly; consumers pay through scheme fragmentation, KYC burden, and reduced prudential protection relative to bank deposits.
 *
 * MANDATROPHY ANALYSIS:
 *   The consumer-holdings reading prevents mislabeling by requiring both coordination and extraction to be present. A pure coordination reading (Rope) would ignore the asymmetric licensing rents and definitional power captured by regulators. A pure extraction reading (Snare) would ignore the genuine problem of unregulated non-bank digital value that the EMD solved. The Tangled Rope classification captures that the same legal framework simultaneously solves a coordination problem and asymmetrically distributes authority and rents.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    consumer_holdings_vs_infrastructure_boundary,
    'Does the consumer-holdings boundary foreclose the infrastructure reading in a single regulatory framework, or can both emergence narratives coexist within institutional monetary history?',
    'Comparative analysis of central bank legal histories and academic curricula to determine whether institutions teach both emergence narratives or privilege the consumer-holdings boundary exclusively.',
    'If foreclosed, the constraint functions as a stronger exclusion mechanism against alternative monetary ontologies; if coexisting, the extraction is softer and the directionality for excluded voices lower.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(consumer_holdings_vs_infrastructure_boundary, conceptual, 'Structural relation between consumer-holdings and infrastructure readings').

omega_variable(
    monetary_aggregate_naturalness,
    'Is the M4/M5 separation a natural analytical boundary in monetary economics, or a constructed legal distinction that serves European regulatory jurisdiction?',
    'Cross-jurisdictional comparison: do non-EU monetary authorities (Federal Reserve, Bank of Japan) maintain equivalent aggregate distinctions for non-bank digital money?',
    'If unique to EU legal tradition, the constraint is more constructed and extractive; if globally adopted, it functions more like a genuine coordination standard.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(monetary_aggregate_naturalness, empirical, 'Whether M4/M5 distinction is natural or constructed').

omega_variable(
    e_money_framework_net_welfare,
    'Does the EMI/e-money framework produce net consumer benefit through innovation and access, or net cost through fragmentation and reduced prudential protection?',
    'Comparative consumer financial protection data, e-money scheme failure rates, and switching-cost studies across EU member states.',
    'If net cost to consumers, directionality for the consumer seat moves toward full target; if net benefit, toward symmetric or even subsidized.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(e_money_framework_net_welfare, empirical, 'Net welfare effect of e-money framework on consumers').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(digital_money_emergence_boundary__consumer_holdings_reading, 1990, 2023).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dm_eb_chr_tr_t1990, digital_money_emergence_boundary__consumer_holdings_reading, theater_ratio, 1990, 0.1).
narrative_ontology:measurement(dm_eb_chr_tr_t1995, digital_money_emergence_boundary__consumer_holdings_reading, theater_ratio, 1995, 0.18).
narrative_ontology:measurement(dm_eb_chr_tr_t2000, digital_money_emergence_boundary__consumer_holdings_reading, theater_ratio, 2000, 0.28).
narrative_ontology:measurement(dm_eb_chr_tr_t2005, digital_money_emergence_boundary__consumer_holdings_reading, theater_ratio, 2005, 0.32).
narrative_ontology:measurement(dm_eb_chr_tr_t2009, digital_money_emergence_boundary__consumer_holdings_reading, theater_ratio, 2009, 0.35).
narrative_ontology:measurement(dm_eb_chr_tr_t2015, digital_money_emergence_boundary__consumer_holdings_reading, theater_ratio, 2015, 0.38).
narrative_ontology:measurement(dm_eb_chr_tr_t2020, digital_money_emergence_boundary__consumer_holdings_reading, theater_ratio, 2020, 0.42).
narrative_ontology:measurement(dm_eb_chr_tr_t2023, digital_money_emergence_boundary__consumer_holdings_reading, theater_ratio, 2023, 0.45).

% Extraction over time
narrative_ontology:measurement(dm_eb_chr_be_t1990, digital_money_emergence_boundary__consumer_holdings_reading, base_extractiveness, 1990, 0.22).
narrative_ontology:measurement(dm_eb_chr_be_t1995, digital_money_emergence_boundary__consumer_holdings_reading, base_extractiveness, 1995, 0.3).
narrative_ontology:measurement(dm_eb_chr_be_t2000, digital_money_emergence_boundary__consumer_holdings_reading, base_extractiveness, 2000, 0.45).
narrative_ontology:measurement(dm_eb_chr_be_t2005, digital_money_emergence_boundary__consumer_holdings_reading, base_extractiveness, 2005, 0.5).
narrative_ontology:measurement(dm_eb_chr_be_t2009, digital_money_emergence_boundary__consumer_holdings_reading, base_extractiveness, 2009, 0.52).
narrative_ontology:measurement(dm_eb_chr_be_t2015, digital_money_emergence_boundary__consumer_holdings_reading, base_extractiveness, 2015, 0.55).
narrative_ontology:measurement(dm_eb_chr_be_t2020, digital_money_emergence_boundary__consumer_holdings_reading, base_extractiveness, 2020, 0.6).
narrative_ontology:measurement(dm_eb_chr_be_t2023, digital_money_emergence_boundary__consumer_holdings_reading, base_extractiveness, 2023, 0.63).

% Suppression requirement over time
narrative_ontology:measurement(dm_eb_chr_su_t1990, digital_money_emergence_boundary__consumer_holdings_reading, suppression_requirement, 1990, 0.15).
narrative_ontology:measurement(dm_eb_chr_su_t1995, digital_money_emergence_boundary__consumer_holdings_reading, suppression_requirement, 1995, 0.28).
narrative_ontology:measurement(dm_eb_chr_su_t2000, digital_money_emergence_boundary__consumer_holdings_reading, suppression_requirement, 2000, 0.48).
narrative_ontology:measurement(dm_eb_chr_su_t2005, digital_money_emergence_boundary__consumer_holdings_reading, suppression_requirement, 2005, 0.52).
narrative_ontology:measurement(dm_eb_chr_su_t2009, digital_money_emergence_boundary__consumer_holdings_reading, suppression_requirement, 2009, 0.55).
narrative_ontology:measurement(dm_eb_chr_su_t2015, digital_money_emergence_boundary__consumer_holdings_reading, suppression_requirement, 2015, 0.58).
narrative_ontology:measurement(dm_eb_chr_su_t2020, digital_money_emergence_boundary__consumer_holdings_reading, suppression_requirement, 2020, 0.62).
narrative_ontology:measurement(dm_eb_chr_su_t2023, digital_money_emergence_boundary__consumer_holdings_reading, suppression_requirement, 2023, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(digital_money_emergence_boundary__consumer_holdings_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(digital_money_emergence_boundary__consumer_holdings_reading, conceptualization_reading).
narrative_ontology:affects_constraint(digital_money_emergence_boundary__consumer_holdings_reading, infrastructure_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the digital_money_emergence_boundary kernel, instantiating the consumer-holdings boundary (1990s/EMD) as distinct from the conceptualization reading (1960s/1985 Chaum) and infrastructure reading (1967 ATMs/1972 ACH/1977 SWIFT). Each reading carries a different epsilon and stakeholder structure.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
