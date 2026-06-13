% ============================================================================
% CONSTRAINT STORY: digital_money_emergence_boundary__infrastructure_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: digital_money_emergence_boundary__infrastructure_reading
 *   human_readable: Digital Money Emergence via Infrastructure Boundary (1967-1977 infrastructure reading)
 *   domain: economic/technological/financial_history
 *
 * SUMMARY:
 *   This constraint embodies one reading of the contested kernel 'digital
 *   money emergence': it asserts that digital money emerged when
 *   infrastructure enabled electronic transfer between banks, not when
 *   consumers could directly hold digital instruments or when theorists first
 *   formalized the concept. Under this reading, digital money exists and is
 *   constituted by SWIFT, ACH, and ATM networks that enable banks to move
 *   funds electronically. The reading benefits those who control and operate
 *   this infrastructure (SWIFT, ACH operators, large banks), who collect fees
 *   on every electronic transfer and whose role as money custodians is locked
 *   in by the technical definition. The claim/metric gap is deliberate and
 *   structural: the constraint is CLAIMED as tangled rope (coordination
 *   function + enforced asymmetry) while the authored metrics show rising
 *   extractiveness over 50+ years, suggesting the coordination function may
 *   have atrophied and pure rent collection persists. The engine measures
 *   this divergence.
 *
 * KEY AGENTS:
 *   - banking_infrastructure_operators: Institutional beneficiaries, control the technical standards and gates (SWIFT, ACH, ATM networks); set and collect fees on all transactions.
 *   - major_commercial_banks: Institutional beneficiaries, gain operational efficiency from electronic transfer; also pay fees but recoup through deposit interest arbitrage and transaction volume.
 *   - retail_consumers: Powerless payers, locked in to bank custody; cannot directly transact in digital money under this reading and pay indirectly through fees and reduced interest.
 *   - central_banks: Observers, face a technical problem — electronic bank deposits blur the M3/M4 boundary, destabilizing monetary statistics.
 *   - alternative_system_proponents: Excluded, barred from the definitional conversation by institutional lock-in of the infrastructure reading.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(digital_money_emergence_boundary__infrastructure_reading, 0.68).
domain_priors:suppression_score(digital_money_emergence_boundary__infrastructure_reading, 0.71).
domain_priors:theater_ratio(digital_money_emergence_boundary__infrastructure_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(digital_money_emergence_boundary__infrastructure_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(digital_money_emergence_boundary__infrastructure_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(digital_money_emergence_boundary__infrastructure_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(digital_money_emergence_boundary__infrastructure_reading, accessibility_collapse, 0.63).
narrative_ontology:constraint_metric(digital_money_emergence_boundary__infrastructure_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(digital_money_emergence_boundary__infrastructure_reading, tangled_rope).
narrative_ontology:human_readable(digital_money_emergence_boundary__infrastructure_reading, "Digital Money Emergence via Infrastructure Boundary (1967-1977 infrastructure reading)").
narrative_ontology:topic_domain(digital_money_emergence_boundary__infrastructure_reading, "economic/technological/financial_history").

domain_priors:requires_active_enforcement(digital_money_emergence_boundary__infrastructure_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(digital_money_emergence_boundary__infrastructure_reading, '46370fd6-690d-48d0-aae7-de94573769d2').
narrative_ontology:cs_kernel_codification('46370fd6-690d-48d0-aae7-de94573769d2', distributed).
narrative_ontology:cs_authority_grounding('46370fd6-690d-48d0-aae7-de94573769d2', extraction).
narrative_ontology:cs_interpretation_layer_present('46370fd6-690d-48d0-aae7-de94573769d2').
narrative_ontology:cs_reading_relation('46370fd6-690d-48d0-aae7-de94573769d2', digital_money_emergence_boundary__conceptualization_reading, coexists_with).
narrative_ontology:cs_reading_relation('46370fd6-690d-48d0-aae7-de94573769d2', digital_money_emergence_boundary__consumer_holdings_reading, influences).
narrative_ontology:cs_axiom('46370fd6-690d-48d0-aae7-de94573769d2', foundational, electronic_bank_transfer_constitutes_emergence).
narrative_ontology:cs_axiom_status(electronic_bank_transfer_constitutes_emergence, holdable).
narrative_ontology:cs_axiom_grounding('46370fd6-690d-48d0-aae7-de94573769d2', electronic_bank_transfer_constitutes_emergence, deontological).
narrative_ontology:cs_axiom('46370fd6-690d-48d0-aae7-de94573769d2', foundational, infrastructure_control_determines_money_definition).
narrative_ontology:cs_axiom_status(infrastructure_control_determines_money_definition, holdable).
narrative_ontology:cs_axiom_grounding('46370fd6-690d-48d0-aae7-de94573769d2', infrastructure_control_determines_money_definition, instrumental).
narrative_ontology:cs_reference_frame('46370fd6-690d-48d0-aae7-de94573769d2', bank_to_bank_electronic_settlement_baseline).
narrative_ontology:cs_drift_state('46370fd6-690d-48d0-aae7-de94573769d2', contemporary_fintech_and_cbdc_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('46370fd6-690d-48d0-aae7-de94573769d2', '').
narrative_ontology:cs_kernel_id(digital_money_emergence_boundary__infrastructure_reading, digital_money_emergence_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(digital_money_emergence_boundary__infrastructure_reading, banking_infrastructure_operators).
narrative_ontology:constraint_victim(digital_money_emergence_boundary__infrastructure_reading, retail_consumers).
narrative_ontology:constraint_victim(digital_money_emergence_boundary__infrastructure_reading, independent_financial_institutions).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(digital_money_emergence_boundary__infrastructure_reading, major_commercial_banks).
narrative_ontology:constraint_victim(digital_money_emergence_boundary__infrastructure_reading, major_commercial_banks).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% SWIFT, ACH operators, ATM network consortia, and other payment-rail operators set technical standards, control access, and collect transaction fees. They have high power (institutional monopoly), low exit options (can diversify but not fundamentally exit infrastructure provision), and long time horizons (generational lock-in). They define what counts as digital money because they control the physical-layer infrastructure.
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__infrastructure_reading, banking_infrastructure_operators, agenda_setter,
    institutional, generational, arbitrage, global).

% Gain settlement speed, operational efficiency, and access to electronic liquidity markets from the infrastructure. They also pay per-transaction fees and must comply with standards set by operators. They are beneficiaries of the coordination (faster settlement reduces their risks) and payers (fees reduce margins). Their constrained exit means they cannot withdraw from electronic systems without ceasing to be modern commercial banks.
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__infrastructure_reading, major_commercial_banks, beneficiary,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(digital_money_emergence_boundary__infrastructure_reading, major_commercial_banks, payer).

% Have no direct access to electronic money under this reading — money remains locked in bank accounts, accessed via physical cash (ATMs) or checks. They experience the infrastructure as a rate-setting mechanism that banks use to reduce deposit interest rates and charge ATM and transfer fees. They are trapped because modern commerce requires banking, which requires participation in electronic systems they do not control and cannot directly use.
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__infrastructure_reading, retail_consumers, payer,
    powerless, biographical, trapped, global).

% Credit unions, community banks, and non-bank lenders must connect to SWIFT, ACH, and other networks to offer electronic services. They face higher per-transaction costs than large banks (economies of scale favor incumbents) and limited ability to negotiate on standards. Constrained exit: they could refuse connection but would become non-viable financial institutions.
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__infrastructure_reading, independent_financial_institutions, payer,
    moderate, biographical, constrained, national).

% Monitor payment systems for stability and conduct monetary policy. The infrastructure reading creates a technical problem: electronic bank deposits blur the boundary between M3 (near-money) and M4 (money), destabilizing monetary aggregates and inflation measurement. They are analytical observers rather than direct participants, but the infrastructure's existence constrains their measurement tools.
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__infrastructure_reading, central_banks, observer,
    institutional, generational, analytical, national).

% Technologists, regulators, and academics who advocate for direct consumer access to digital money (blockchain settlement, central bank digital currency, open payment networks). They are excluded from the definitional conversation by institutional lock-in — the infrastructure reading is enshrined in regulatory definitions, industry standards, and central bank policy. Their proposals for alternative boundaries are treated as radical departures rather than equally valid readings of the same kernel.
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__infrastructure_reading, alternative_digital_system_advocates, excluded,
    moderate, biographical, trapped, global).

% Debate what counts as money and analyze the effects of different boundary definitions on monetary statistics, inflation measurement, and policy transmission. They see the emergence question as an empirical and conceptual issue, not a political or institutional one. They are analytical observers without direct stake but with epistemic influence over policy.
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__infrastructure_reading, monetary_economists_and_theorists, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(digital_money_emergence_boundary__infrastructure_reading, banking_infrastructure_operators).
narrative_ontology:fixing_cost_class(digital_money_emergence_boundary__infrastructure_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Electronic transfer infrastructure solved genuine coordination problems: instant inter-bank settlement reduces counterparty risk, enables large-scale cross-border commerce, allows central banks to conduct monetary policy through bank reserves, and provides security assurances that individual banks cannot credibly offer. The infrastructure reading asserts that THIS SOLUTION, not consumer holding or theoretical formalization, constitutes the emergence of digital money.
% TRANSFER_FUNCTION: Moves control over money movement from diverse decentralized actors (individual accountholders, local banks, physical couriers) to centralized infrastructure operators who set technical standards, collect transaction fees, and mediate every electronic transfer. Also transfers de facto definitional power: the reading that becomes institutional standard defines what counts as money for policy, regulation, and accounting purposes.
% ABSENT_VOICES: Those who argue for direct consumer access to digital money (consumer_holdings_reading advocates) and those who argue for theoretical/formal definitions (conceptualization_reading advocates) are excluded from the definitional conversation because the infrastructure reading is locked in by institutional adoption, regulatory incorporation, and technical standardization. Once SWIFT and ACH became mandatory standards, the infrastructure-centric definition became the baseline that alternatives must argue against rather than a live option in a kernel-negotiation.
% DISAPPEARANCE_RATIONALE: If electronic transfer infrastructure (SWIFT, ACH, ATM networks) vanished overnight, modern commerce would collapse. Settlement of transactions would revert to physical currency and paper clearing, which can only support local-scale trade. The infrastructure reading ties the EXISTENCE of digital money to these specific technical systems; destruction of the systems would, under this reading, erase digital money itself and force an economic contraction.
% FOUNDING_PROBLEM: Physical currency and paper-check clearing could not scale to the volume and speed required by mid-20th-century commerce. Banks needed to settle transactions between institutions in real time without waiting for physical transfer of funds or paper documents. Electronic transfer directly solved this technical problem.
% FOUNDING_PROBLEM_CORROBORATION: Central banks and settlement-system operators attest that the founding problem was real: the 1960s-1970s saw explosive growth in transactions that physical clearing could not handle. Historians of payment systems confirm the scaling crisis was genuine. Academic monetary economists outside the banking industry (e.g., from academic monetary policy research programs) confirm that electronic infrastructure solved a real coordination problem. Dissent is over whether solving the coordination problem constitutes EMERGING digital money (infrastructure_reading) or merely upgrading the technical infrastructure while money itself remained defined by other properties (consumer_holdings_reading or conceptualization_reading argue).
narrative_ontology:disappearance_verdict(digital_money_emergence_boundary__infrastructure_reading, world_rearranges).
narrative_ontology:founding_problem_status(digital_money_emergence_boundary__infrastructure_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(digital_money_emergence_boundary__infrastructure_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(digital_money_emergence_boundary__infrastructure_reading, 'none', 1).

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
 *   Extractiveness starts low (0.25 in 1967) because the infrastructure is new and many actors are still experimenting with adoption; as adoption becomes mandatory (locked-in standards), extractiveness rises to 0.68 by 2020. Suppression is correspondingly high (0.71 at end) because participation in the electronic system is mandatory for any institution wanting to participate in modern commerce — no exit without abandoning the banking system entirely. Theater rises gradually (0.18 to 0.42) as the infrastructure's stated purpose (secure, efficient settlement) becomes less salient relative to its actual function (fee collection and control over money movement). The measurement grid is shared across all three metrics and runs from 1967 (ATM emergence) through 2020 (modern era), with dense sampling in 1977 (SWIFT adoption) and 1990 (ACH national scale) where institutional locking accelerated.
 *
 * PERSPECTIVAL GAP:
 *   The infrastructure-operator seat and the retail-consumer seat should compute radically differently. From the operator's position, electronic transfer infrastructure is essential coordination they built and maintain at cost — a rope. From the powerless consumer's position, it is a locked-in extract mechanism they cannot exit — a snare. The directed acyclic graph of institutional control (operators → large banks → central banks → consumers) means the beneficiary seats experience low directionality (low χ, net gain) while the payer seats experience high directionality (high χ, net loss). This divergence is the measurement the per-seat classification system exists to take.
 *
 * DIRECTIONALITY LOGIC:
 *   Banking infrastructure operators have d ≈ 0.0-0.1 (full beneficiaries): they set prices, control access, collect fees directly. Major commercial banks have d ≈ 0.3-0.4 (symmetric-to-beneficiary): they pay fees but recoup interest and gain speed; their exit options are constrained but their power is institutional so the constraint is negotiated at their level. Retail consumers have d ≈ 0.9 (near full targets): they are trapped (cannot exit without abandoning banking), powerless (no voice in standard-setting), and pay through all channels (fees, interest, lost alternatives). Central banks are analytical (d does not apply). This structural gradient means the same constraint looks like beneficial coordination to the top and like pure extraction to the bottom.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled_rope classification requires coordination function + enforced asymmetry. The coordination function is real in 1967-1977: electronic transfer genuinely solves a scaling problem that physical clearing cannot. But by 2020, the founding problem (scaling electronic commerce) is solved so thoroughly that the infrastructure has become utility-grade and commodified — yet fees remain high, standards remain closed, and control remains concentrated. The rising theater_ratio (0.18→0.42) suggests the coordination story is increasingly theatrical cover for pure extraction. This is not mandatrophy in the strict sense (constraint persists as pure performance) — it is deferred mandatrophy: the constraint could become a piton if the stated coordination justification is abandoned and only rent collection remains. The measurement series tracks the degradation of the coordination story over time.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    coordination_vs_extraction_boundary,
    'At what point does electronic transfer infrastructure transition from solving a genuine coordination problem to pure rent collection on a solved problem?',
    'Empirical study of infrastructure costs (marginal cost per transaction) vs. fees charged; regulatory mandates testing whether fee reduction causes system degradation; comparison with commodity settlement services (power grids, telecom backbone) that operate at near-marginal-cost.',
    'If fees are far above marginal cost and system quality is stable, the constraint is pure snare not tangled_rope — the coordination story is cover. If fees track cost, tangled_rope holds. The measurement series suggests fees have decoupled from cost, favoring snare reclassification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_vs_extraction_boundary, empirical, 'Whether the infrastructure has become a solved commodity or remains a contested coordination function.').

omega_variable(
    reading_mutual_exclusivity,
    'Are the three readings of the digital-money-emergence kernel mutually exclusive (only one can be true), mutually compatible (all three can be true simultaneously), or do they describe different aspects of a unified phenomenon?',
    'Philosophical analysis of what ''emergence'' means in monetary systems; historical reconstruction of what counts as money in different institutional contexts; empirical testing of whether the three boundaries correspond to distinct changes in economic behavior or are linguistic artifacts of competing framings.',
    'If mutually exclusive, then the kernel is genuinely contested and one reading will win (lock-in). If compatible, then the three constraints describe different layers of a complex phenomenon and should all be authored as separate stories linked via network.affects_constraints (current intention). If linguistic artifacts, then the distinction is not real and the three constraints collapse into one with multiple framings (does not match the structure here).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_mutual_exclusivity, conceptual, 'Whether the kernel-contest is about objective reality or competing frames.').

omega_variable(
    suppression_mechanism_internalization,
    'Is the high suppression (0.71) in modern digital money systems structural (technical barriers to exit, regulatory lock-in) or internalized (institutions believe electronic bank-to-bank transfer IS money and no alternative is conceivable)?',
    'Study the discourse and learning process: do institutions defend electronic transfer as necessary because they cannot imagine alternatives, or because they have examined alternatives and rejected them? Test with proposals for alternative infrastructure (blockchain settlement, central bank digital currency, decentralized networks) — do institutions resist because they believe alternatives cannot work, or because they benefit from current arrangements?',
    'If structural, suppression persists only as long as the technical barriers exist; alternatives could be adopted if barriers fell. If internalized, institutions would resist alternatives even if barriers fell; the constraint persists through belief and institutional identity. The measurement series suggests rising suppression alongside rising theater_ratio, implying internalization is growing (institutions increasingly defend the constraint as natural/necessary rather than beneficial/chosen).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalization, empirical, 'Whether modern digital money suppression is maintained by external barriers or internal conviction.').

omega_variable(
    multiple_readings_kernel_contest,
    'Does this constraint''s instantiation as one reading of a contested kernel change the computed classification compared to a single-reading authoring of the same content?',
    'The engine computes per-seat type from structural data (power, exit, beneficiary/victim). The kernel framing (asserting that this is one reading, not the only possible account) should not change the structural data, so computed types should be identical. If types differ, the kernel framing itself is doing classification work, which means the framing is not neutral recording but normative positioning. Run the same constraint through the engine both with and without the kernel context and compare outputs.',
    'If types differ, the kernel framing is not mere genealogy but an active classification mechanism — confessing to the reading strategy would alter the measurement system, which is a violation of independence (OQ-83 R2). If types are identical, the kernel framing is honest genealogy with no direct effect on classification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(multiple_readings_kernel_contest, empirical, 'Whether acknowledging the kernel-contest affects computed classification.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(digital_money_emergence_boundary__infrastructure_reading, 1967, 2020).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(digi_tr_t1967, digital_money_emergence_boundary__infrastructure_reading, theater_ratio, 1967, 0.18).
narrative_ontology:measurement_basis(digi_tr_t1967, projected).
narrative_ontology:measurement(digi_tr_t1977, digital_money_emergence_boundary__infrastructure_reading, theater_ratio, 1977, 0.25).
narrative_ontology:measurement_basis(digi_tr_t1977, observed).
narrative_ontology:measurement(digi_tr_t1990, digital_money_emergence_boundary__infrastructure_reading, theater_ratio, 1990, 0.32).
narrative_ontology:measurement_basis(digi_tr_t1990, observed).
narrative_ontology:measurement(digi_tr_t2005, digital_money_emergence_boundary__infrastructure_reading, theater_ratio, 2005, 0.38).
narrative_ontology:measurement_basis(digi_tr_t2005, observed).
narrative_ontology:measurement(digi_tr_t2015, digital_money_emergence_boundary__infrastructure_reading, theater_ratio, 2015, 0.41).
narrative_ontology:measurement_basis(digi_tr_t2015, observed).
narrative_ontology:measurement(digi_tr_t2020, digital_money_emergence_boundary__infrastructure_reading, theater_ratio, 2020, 0.42).
narrative_ontology:measurement_basis(digi_tr_t2020, observed).

% Extraction over time
narrative_ontology:measurement(digi_be_t1967, digital_money_emergence_boundary__infrastructure_reading, base_extractiveness, 1967, 0.25).
narrative_ontology:measurement_basis(digi_be_t1967, projected).
narrative_ontology:measurement(digi_be_t1977, digital_money_emergence_boundary__infrastructure_reading, base_extractiveness, 1977, 0.42).
narrative_ontology:measurement_basis(digi_be_t1977, observed).
narrative_ontology:measurement(digi_be_t1990, digital_money_emergence_boundary__infrastructure_reading, base_extractiveness, 1990, 0.54).
narrative_ontology:measurement_basis(digi_be_t1990, observed).
narrative_ontology:measurement(digi_be_t2005, digital_money_emergence_boundary__infrastructure_reading, base_extractiveness, 2005, 0.61).
narrative_ontology:measurement_basis(digi_be_t2005, observed).
narrative_ontology:measurement(digi_be_t2015, digital_money_emergence_boundary__infrastructure_reading, base_extractiveness, 2015, 0.66).
narrative_ontology:measurement_basis(digi_be_t2015, observed).
narrative_ontology:measurement(digi_be_t2020, digital_money_emergence_boundary__infrastructure_reading, base_extractiveness, 2020, 0.68).
narrative_ontology:measurement_basis(digi_be_t2020, observed).

% Suppression requirement over time
narrative_ontology:measurement(digi_su_t1967, digital_money_emergence_boundary__infrastructure_reading, suppression_requirement, 1967, 0.35).
narrative_ontology:measurement_basis(digi_su_t1967, projected).
narrative_ontology:measurement(digi_su_t1977, digital_money_emergence_boundary__infrastructure_reading, suppression_requirement, 1977, 0.48).
narrative_ontology:measurement_basis(digi_su_t1977, observed).
narrative_ontology:measurement(digi_su_t1990, digital_money_emergence_boundary__infrastructure_reading, suppression_requirement, 1990, 0.58).
narrative_ontology:measurement_basis(digi_su_t1990, observed).
narrative_ontology:measurement(digi_su_t2005, digital_money_emergence_boundary__infrastructure_reading, suppression_requirement, 2005, 0.66).
narrative_ontology:measurement_basis(digi_su_t2005, observed).
narrative_ontology:measurement(digi_su_t2015, digital_money_emergence_boundary__infrastructure_reading, suppression_requirement, 2015, 0.69).
narrative_ontology:measurement_basis(digi_su_t2015, observed).
narrative_ontology:measurement(digi_su_t2020, digital_money_emergence_boundary__infrastructure_reading, suppression_requirement, 2020, 0.71).
narrative_ontology:measurement_basis(digi_su_t2020, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(digital_money_emergence_boundary__infrastructure_reading, global_infrastructure).
narrative_ontology:boltzmann_floor_override(digital_money_emergence_boundary__infrastructure_reading, 0.18).
narrative_ontology:affects_constraint(digital_money_emergence_boundary__infrastructure_reading, digital_money_emergence_boundary__conceptualization_reading).
narrative_ontology:affects_constraint(digital_money_emergence_boundary__infrastructure_reading, digital_money_emergence_boundary__consumer_holdings_reading).

% DUAL FORMULATION NOTE:
% This constraint is the INFRASTRUCTURE READING of the contested kernel 'digital_money_emergence_boundary'. The kernel has three structurally distinct readings with different ε values, beneficiary structures, and empirical status. All three readings coexist in contemporary monetary policy and academic discourse but produce incompatible boundaries for what counts as digital money. The infrastructure_reading claims digital money emerged when electronic bank-to-bank transfer became possible (1967-1977); it benefits those who control the rails (SWIFT, ACH operators). The conceptualization_reading claims it emerged when theorists formalized the concept (1960s-1985); it privileges intellectual/academic authority. The consumer_holdings_reading claims it emerged when consumers could directly hold digital instruments (1990s-2000s); it privileges retail access and direct participation. These three constraints are SEPARATE STORIES, each with its own ε, beneficiary/victim structure, and epistemic status. The readings coexist because they appeal to different institutional constituencies and because no single decisive evidence has settled which boundary is authoritative. Authoring them as three separate constraints with network.affects_constraints links enables the corpus to model the kernel-contest as an ongoing institutional and epistemic dispute rather than a settled fact.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(digital_money_emergence_boundary__infrastructure_reading, powerless, 0.92).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
