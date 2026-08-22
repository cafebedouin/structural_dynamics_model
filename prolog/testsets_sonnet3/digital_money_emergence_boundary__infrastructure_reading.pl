% ============================================================================
% CONSTRAINT STORY: digital_money_emergence_boundary__infrastructure_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_digital_money_emergence_boundary__infrastructure_reading, []).

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
 *   constraint_id: digital_money_emergence_boundary__infrastructure_reading
 *   human_readable: Infrastructure Boundary Reading of Digital Money Emergence (ATM/ACH/SWIFT rails)
 *   domain: monetary_economics/financial_infrastructure
 *
 * SUMMARY:
 *   This story instantiates the infrastructure_reading of the
 *   digital_money_emergence_boundary kernel: digital money is dated to when
 *   banks could move deposit claims electronically between each other's
 *   ledgers — 1967 networked ATMs, 1972 ACH founding, 1977 SWIFT launch —
 *   regardless of whether any consumer could directly hold a digital
 *   instrument. This is a middle boundary between the
 *   conceptualization_reading (which dates emergence to theoretical
 *   thinkability, e.g. Chaum 1985) and the consumer_holdings_reading (which
 *   requires direct consumer digital holdings, e.g. 1990s e-purses, 2000
 *   EMD). Under this reading, M4/M5 monetary-aggregate boundaries begin to
 *   blur because electronic bank deposits become functionally
 *   indistinguishable from a new settlement category, even though no retail
 *   consumer yet holds a digital token. The beneficiary structure under this
 *   reading is distinctive: it is banking infrastructure providers (SWIFT,
 *   ACH operators, correspondent banks) who control the rails, not toolmakers
 *   of consumer-facing digital cash and not cryptographers who first proved
 *   digital money's theoretical possibility.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(digital_money_emergence_boundary__infrastructure_reading, 0.58).
domain_priors:suppression_score(digital_money_emergence_boundary__infrastructure_reading, 0.52).
domain_priors:theater_ratio(digital_money_emergence_boundary__infrastructure_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(digital_money_emergence_boundary__infrastructure_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(digital_money_emergence_boundary__infrastructure_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(digital_money_emergence_boundary__infrastructure_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(digital_money_emergence_boundary__infrastructure_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(digital_money_emergence_boundary__infrastructure_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(digital_money_emergence_boundary__infrastructure_reading, tangled_rope).
narrative_ontology:human_readable(digital_money_emergence_boundary__infrastructure_reading, "Infrastructure Boundary Reading of Digital Money Emergence (ATM/ACH/SWIFT rails)").
narrative_ontology:topic_domain(digital_money_emergence_boundary__infrastructure_reading, "monetary_economics/financial_infrastructure").

domain_priors:requires_active_enforcement(digital_money_emergence_boundary__infrastructure_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(digital_money_emergence_boundary__infrastructure_reading, '52403170-02c7-4c9d-af07-fe46ad8e6fea').
narrative_ontology:cs_kernel_codification('52403170-02c7-4c9d-af07-fe46ad8e6fea', distributed).
narrative_ontology:cs_authority_grounding('52403170-02c7-4c9d-af07-fe46ad8e6fea', distributed).
narrative_ontology:cs_reading_relation('52403170-02c7-4c9d-af07-fe46ad8e6fea', digital_money_emergence_boundary__conceptualization_reading, coexists_with).
narrative_ontology:cs_reading_relation('52403170-02c7-4c9d-af07-fe46ad8e6fea', digital_money_emergence_boundary__consumer_holdings_reading, influences).
narrative_ontology:cs_axiom('52403170-02c7-4c9d-af07-fe46ad8e6fea', foundational, interbank_transfer_capability_constitutes_monetary_form).
narrative_ontology:cs_axiom_status(interbank_transfer_capability_constitutes_monetary_form, holdable).
narrative_ontology:cs_axiom_grounding('52403170-02c7-4c9d-af07-fe46ad8e6fea', interbank_transfer_capability_constitutes_monetary_form, conventional).
narrative_ontology:cs_axiom('52403170-02c7-4c9d-af07-fe46ad8e6fea', secondary, consumer_access_not_required_for_emergence).
narrative_ontology:cs_axiom_status(consumer_access_not_required_for_emergence, holdable).
narrative_ontology:cs_axiom_grounding('52403170-02c7-4c9d-af07-fe46ad8e6fea', consumer_access_not_required_for_emergence, conventional).
narrative_ontology:cs_reference_frame('52403170-02c7-4c9d-af07-fe46ad8e6fea', bank_deposit_ledger_primacy).
narrative_ontology:cs_drift_state('52403170-02c7-4c9d-af07-fe46ad8e6fea', post_ach_swift_maturation, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('52403170-02c7-4c9d-af07-fe46ad8e6fea', '').
narrative_ontology:cs_kernel_id(digital_money_emergence_boundary__infrastructure_reading, digital_money_emergence_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(digital_money_emergence_boundary__infrastructure_reading, swift_cooperative_members).
narrative_ontology:constraint_beneficiary(digital_money_emergence_boundary__infrastructure_reading, ach_operators).
narrative_ontology:constraint_beneficiary(digital_money_emergence_boundary__infrastructure_reading, correspondent_banking_network).
narrative_ontology:constraint_beneficiary(digital_money_emergence_boundary__infrastructure_reading, central_bank_settlement_authorities).
narrative_ontology:constraint_victim(digital_money_emergence_boundary__infrastructure_reading, unbanked_and_underbanked_households).
narrative_ontology:constraint_victim(digital_money_emergence_boundary__infrastructure_reading, non_member_regional_banks).
narrative_ontology:constraint_victim(digital_money_emergence_boundary__infrastructure_reading, cross_border_remittance_senders).
narrative_ontology:constraint_victim(digital_money_emergence_boundary__infrastructure_reading, retail_depositors_bearing_float_costs).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Owns and governs the messaging standard that lets banks instruct each other to move money electronically across borders. Sets participation rules, message formats, and compliance gates. Collects membership fees and message fees, and because the network effect locks in participants, effectively decides who counts as connected to the electronic money system at all.
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__infrastructure_reading, swift_cooperative_members, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(digital_money_emergence_boundary__infrastructure_reading, swift_cooperative_members, beneficiary).

% Operate the domestic batch-clearing rails that convert paper-check logic into electronic entries between bank ledgers. Set settlement windows, reserve requirements for member banks, and per-transaction fees. Their infrastructure decision (batch timing, cutoffs) determines when a depositor's money is 'real' electronically versus merely promised.
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__infrastructure_reading, ach_operators, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(digital_money_emergence_boundary__infrastructure_reading, ach_operators, beneficiary).

% Large money-center banks act as intermediaries for smaller banks that cannot afford direct SWIFT/ACH membership, taking a spread on every routed transfer. They benefit twice: once as network members, once as gatekeepers charging correspondent fees to those without direct access.
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__infrastructure_reading, correspondent_banking_network, beneficiary,
    institutional, generational, arbitrage, global).

% Provide the ultimate settlement finality (reserve accounts) that makes electronic interbank transfers authoritative rather than merely bookkeeping entries between private parties. Benefit from the infrastructure's existence by inheriting a new tier of monetary aggregate (M-something) to define and control, but do not directly extract fees.
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__infrastructure_reading, central_bank_settlement_authorities, beneficiary,
    institutional, civilizational, analytical, national).
narrative_ontology:stakeholder_secondary_role(digital_money_emergence_boundary__infrastructure_reading, central_bank_settlement_authorities, observer).

% Cannot access ACH or SWIFT rails directly because access requires a bank account and often minimum balances; they pay disproportionately for check-cashing, money orders, and remittance services that ride on top of the same rails at a markup, without receiving any of the electronic-transfer efficiency the rails were built to deliver.
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__infrastructure_reading, unbanked_and_underbanked_households, payer,
    powerless, biographical, trapped, national).

% Smaller banks lack the volume or capital to justify direct SWIFT/ACH membership and must route through correspondent relationships, paying spreads and losing settlement speed. Their customers experience slower, costlier electronic transfers than customers of large member banks, even though the underlying infrastructure exists.
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__infrastructure_reading, non_member_regional_banks, payer,
    moderate, biographical, constrained, regional).

% Migrant workers sending money home depend on chains of correspondent banks layered on SWIFT messaging, each taking a cut and adding delay. The infrastructure that supposedly makes money 'digital and instant' for institutions imposes multi-day settlement and double-digit percentage fees on this population.
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__infrastructure_reading, cross_border_remittance_senders, payer,
    powerless, immediate, trapped, global).

% Ordinary depositors whose ACH-cleared paychecks or transfers are subject to bank-imposed holds; the bank earns interest on the float during the hold period even though the underlying ACH batch settles same-day or next-day. The gap between technical settlement speed and depositor-facing availability is pure institutional extraction riding on the infrastructure's real capability.
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__infrastructure_reading, retail_depositors_bearing_float_costs, payer,
    powerless, immediate, constrained, national).

% Study when 'money' became digital as a matter of infrastructural capability rather than public accessibility or theoretical possibility. Their periodization choice (infrastructure vs. conceptualization vs. consumer-holding) determines which institutions get credited as the originators of digital money and which get erased from the narrative.
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__infrastructure_reading, monetary_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(digital_money_emergence_boundary__infrastructure_reading, swift_cooperative_members).
narrative_ontology:fixing_cost_class(digital_money_emergence_boundary__infrastructure_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Interbank electronic messaging and clearing (SWIFT, ACH, networked ATMs) solves a genuine coordination problem: without a shared standard and settlement protocol, banks cannot reliably move deposit claims between each other's ledgers, and money transfer would remain bound to physical instrument exchange or slow paper correspondence.
% TRANSFER_FUNCTION: Moves settlement speed, transaction fees, and float income from those without direct rail access (the unbanked, remittance senders, smaller banks, ordinary depositors subject to holds) to those who own or hold membership in the rails (SWIFT members, ACH operators, correspondent banks) and to central banks who gain a new governable monetary category.
% ABSENT_VOICES: Unbanked households and remittance senders have no seat in SWIFT or ACH governance; their fee structures are set entirely by member institutions and correspondent banks who compete for institutional volume, not for their access. Their objection — that infrastructure existing for banks is not the same as money existing for people — is the exact substance of the sibling consumer_holdings_reading.
% DISAPPEARANCE_RATIONALE: If SWIFT messaging and ACH clearing vanished, interbank transfers would revert to bilateral correspondent arrangements, telex, or physical settlement, collapsing transaction volume and reintroducing multi-day-to-multi-week settlement times industry-wide; the entire architecture of modern deposit-based money movement depends on this infrastructure layer.
% FOUNDING_PROBLEM: Banks needed a faster, cheaper, more reliable way to instruct each other to debit and credit accounts than physical check clearing, telex cables, or bilateral paper correspondence — a problem acute by the late 1960s as cross-border and high-volume domestic commerce outgrew paper settlement capacity.
% FOUNDING_PROBLEM_CORROBORATION: SWIFT and ACH operators attest the founding problem (unreliable, slow interbank settlement) remains live given continuing transaction-volume growth. Independent monetary historians and remittance-sector researchers (e.g. World Bank remittance-price monitoring) corroborate that the technical settlement problem was substantially solved decades ago, and that current fee and delay structures for non-member populations reflect rent extraction on top of solved infrastructure rather than the original coordination problem itself.
narrative_ontology:disappearance_verdict(digital_money_emergence_boundary__infrastructure_reading, world_rearranges).
narrative_ontology:founding_problem_status(digital_money_emergence_boundary__infrastructure_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(digital_money_emergence_boundary__infrastructure_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(digital_money_emergence_boundary__infrastructure_reading, 'none', 1).
narrative_ontology:epsilon_provenance(digital_money_emergence_boundary__infrastructure_reading, 0.58, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(digital_money_emergence_boundary__infrastructure_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(digital_money_emergence_boundary__infrastructure_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(digital_money_emergence_boundary__infrastructure_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at a moderate-high and rising level (0.28 to 0.58) because the coordination function is genuine (interbank settlement without shared electronic rails is genuinely worse) but a widening extraction layer has accreted on top of it: correspondent-bank spreads, remittance fees, and depositor float capture all ride the same rails without delivering the rails' actual technical capability to the population paying for them. Suppression is authored similarly rising (0.30 to 0.52): access to the rails is gated by membership economics (SWIFT/ACH participation costs) that exclude smaller institutions and unbanked populations, and this gating has hardened over time as network effects deepened. Theater ratio stays comparatively low (0.05 to 0.22) because the rails perform their claimed function — they really do move money electronically — the extraction is not disguised nonfunction, it is a real function with an attached toll booth.
 *
 * PERSPECTIVAL GAP:
 *   From a SWIFT/ACH member seat, this is pure infrastructure coordination — a shared standard everyone needed and everyone benefits from. From an unbanked household or remittance-sender seat, the same infrastructure is experienced as a wall: the technical capability exists but access is gated by bank-account requirements and correspondent markups, so 'digital money' emerged for institutions decades before it functionally emerged for them — which is exactly the boundary dispute the consumer_holdings_reading exists to capture as a separate constraint.
 *
 * DIRECTIONALITY LOGIC:
 *   SWIFT members, ACH operators, and correspondent banks sit near the full-beneficiary end: they set participation terms, collect fees, and hold arbitrage-grade exit (they can restructure their own fee schedules or membership tiers at will). Central bank settlement authorities benefit indirectly (a new governable aggregate) without directly extracting fees, so they sit closer to symmetric. Unbanked households, remittance senders, and float-bearing depositors sit near the full-target end: trapped or constrained exit, no seat in rail governance, and costs imposed precisely because they lack the access the rails were built to enable for institutions.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — unreliable interbank settlement — was substantially solved by the 1980s; the infrastructure now does far more (real-time gross settlement is technically available in most member networks) than the fee and delay structures imposed on non-member populations reflect. Classifying this as tangled_rope rather than snare preserves the genuine coordination function (member banks really do need shared settlement rails) while still registering the asymmetric extraction (non-members and consumers bear costs the technology itself no longer requires) — a pure snare framing would erase the real coordination achievement of 1967-1977; a pure rope framing would erase the documented fee/delay asymmetry imposed on excluded populations.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    infrastructure_vs_conceptualization_boundary_location,
    'Does digital money begin when the technical capability for interbank electronic transfer exists (this reading), or when the concept becomes theoretically well-defined and formalizable (the conceptualization_reading)?',
    'Historical and philosophical analysis of whether ''digital money'' is a capability-claim (something a system can do) or a concept-claim (something that can be coherently theorized) — these are different kinds of claims and may not have a fact-of-the-matter resolution, only a convention choice made explicit per reading.',
    'Choosing the infrastructure boundary locates the beneficiary set among rail operators (SWIFT, ACH); choosing the conceptualization boundary would relocate beneficiaries toward cryptographic researchers and telecom standard-setters, with a very different, likely much lower, extraction profile since no fee-collecting institution controls a ''concept.''',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(infrastructure_vs_conceptualization_boundary_location, conceptual, 'Whether the emergence boundary is best located at technical capability versus theoretical formalization.').

omega_variable(
    infrastructure_vs_consumer_holdings_boundary_location,
    'Does digital money begin when banks can move it electronically even though consumers cannot directly hold it (this reading), or only when consumers themselves can hold and transact digital instruments (the consumer_holdings_reading)?',
    'Track M4/M5 aggregate definitions and regulatory treatment of electronic bank deposits versus later e-money instruments; identify whether regulators, at the time, treated 1970s electronic bank deposits as a genuinely new monetary category or merely as electronic bookkeeping of pre-existing deposit money.',
    'If regulators/economists at the time treated electronic interbank transfer as merely accelerating existing deposit money rather than creating a new monetary form, this reading''s emergence-claim weakens and the consumer_holdings_reading becomes the more defensible boundary — shifting victim/beneficiary analysis toward 1990s-2000s e-money exclusion rather than 1970s banking-access exclusion.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(infrastructure_vs_consumer_holdings_boundary_location, conceptual, 'Whether bank-side electronic capability alone constitutes monetary emergence absent consumer-side digital holding.').

omega_variable(
    m4_m5_collapse_causal_status,
    'Did 1970s electronic interbank settlement infrastructure genuinely cause the blurring of M4/M5 monetary aggregate boundaries, or is that blurring better explained by later financial innovation (money market funds, eurodollar markets) that happened to coincide with rail deployment?',
    'Economic-history analysis correlating the timing and magnitude of M4/M5 boundary revisions against ACH/SWIFT adoption curves versus against money-market-fund and eurodollar-market growth curves.',
    'If the aggregate blurring is primarily attributable to financial-instrument innovation rather than settlement-rail infrastructure, this reading''s causal claim (infrastructure drove the monetary-category shift) weakens, though the beneficiary/victim structure around rail access would remain independently defensible.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(m4_m5_collapse_causal_status, empirical, 'Whether infrastructure deployment or parallel financial innovation drove monetary aggregate boundary changes.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(digital_money_emergence_boundary__infrastructure_reading, 1967, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(digi_tr_t1967, digital_money_emergence_boundary__infrastructure_reading, theater_ratio, 1967, 0.05).
narrative_ontology:measurement(digi_tr_t1977, digital_money_emergence_boundary__infrastructure_reading, theater_ratio, 1977, 0.08).
narrative_ontology:measurement(digi_tr_t1990, digital_money_emergence_boundary__infrastructure_reading, theater_ratio, 1990, 0.12).
narrative_ontology:measurement(digi_tr_t2001, digital_money_emergence_boundary__infrastructure_reading, theater_ratio, 2001, 0.16).
narrative_ontology:measurement(digi_tr_t2012, digital_money_emergence_boundary__infrastructure_reading, theater_ratio, 2012, 0.19).
narrative_ontology:measurement(digi_tr_t2025, digital_money_emergence_boundary__infrastructure_reading, theater_ratio, 2025, 0.22).

% Extraction over time
narrative_ontology:measurement(digi_be_t1967, digital_money_emergence_boundary__infrastructure_reading, base_extractiveness, 1967, 0.28).
narrative_ontology:measurement(digi_be_t1977, digital_money_emergence_boundary__infrastructure_reading, base_extractiveness, 1977, 0.34).
narrative_ontology:measurement(digi_be_t1990, digital_money_emergence_boundary__infrastructure_reading, base_extractiveness, 1990, 0.42).
narrative_ontology:measurement(digi_be_t2001, digital_money_emergence_boundary__infrastructure_reading, base_extractiveness, 2001, 0.48).
narrative_ontology:measurement(digi_be_t2012, digital_money_emergence_boundary__infrastructure_reading, base_extractiveness, 2012, 0.53).
narrative_ontology:measurement(digi_be_t2025, digital_money_emergence_boundary__infrastructure_reading, base_extractiveness, 2025, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(digi_su_t1967, digital_money_emergence_boundary__infrastructure_reading, suppression_requirement, 1967, 0.3).
narrative_ontology:measurement(digi_su_t1977, digital_money_emergence_boundary__infrastructure_reading, suppression_requirement, 1977, 0.38).
narrative_ontology:measurement(digi_su_t1990, digital_money_emergence_boundary__infrastructure_reading, suppression_requirement, 1990, 0.42).
narrative_ontology:measurement(digi_su_t2001, digital_money_emergence_boundary__infrastructure_reading, suppression_requirement, 2001, 0.46).
narrative_ontology:measurement(digi_su_t2012, digital_money_emergence_boundary__infrastructure_reading, suppression_requirement, 2012, 0.49).
narrative_ontology:measurement(digi_su_t2025, digital_money_emergence_boundary__infrastructure_reading, suppression_requirement, 2025, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(digital_money_emergence_boundary__infrastructure_reading, global_infrastructure).
narrative_ontology:boltzmann_floor_override(digital_money_emergence_boundary__infrastructure_reading, 0.2).
narrative_ontology:affects_constraint(digital_money_emergence_boundary__infrastructure_reading, digital_money_emergence_boundary__conceptualization_reading).
narrative_ontology:affects_constraint(digital_money_emergence_boundary__infrastructure_reading, digital_money_emergence_boundary__consumer_holdings_reading).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the digital_money_emergence_boundary kernel. conceptualization_reading dates emergence to theoretical thinkability; infrastructure_reading (this file) dates it to interbank electronic transfer capability; consumer_holdings_reading dates it to direct consumer digital instrument holding. Each carries its own ε, beneficiary/victim structure, and claimed_type — they are not the same constraint measured three ways. The three are temporally sequential (roughly 1960s-1985 conceptualization, 1967-1977 infrastructure, 1990s-2000 consumer holdings) which creates a plausible but non-necessary causal ordering: infrastructure existing may have been a precondition for consumer-holdings instruments, but conceptualization did not require infrastructure to exist first (Chaum's 1985 work post-dates SWIFT but the underlying cryptographic concept is logically infrastructure-independent).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
