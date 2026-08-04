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
 *   constraint_id: digital_money_emergence_boundary__infrastructure_reading
 *   human_readable: Infrastructure Boundary of Digital Money Emergence (Bank Electronic Transfer Rails)
 *   domain: monetary_economics/financial_infrastructure
 *
 * SUMMARY:
 *   Between 1967 and 1977, three infrastructure milestones — ATMs giving
 *   depositors electronic account access, ACH enabling domestic batch
 *   clearing of deposits, and SWIFT standardizing cross-border interbank
 *   messaging — converted bank deposits from paper-ledger claims into
 *   electronically transferable balances. Banks, not consumers, were the
 *   primary actors who gained new transfer capability; the consumer
 *   experience of 'holding digital money' lagged by decades. The rails that
 *   emerged became durable chokepoints: SWIFT and ACH operators, and the
 *   correspondent banks that broker access for non-members, sit at the center
 *   of a coordination function (fast, standardized settlement) that has
 *   increasingly generated extraction (fees, spreads, de-risking
 *   externalities) layered on top of it.
 *
 * KEY AGENTS:
 *   - swift_cooperative: rail operator / agenda_setter (institutional/arbitrage) — sets messaging standards and membership terms
 *   - ach_operators: domestic rail operator / agenda_setter (institutional/arbitrage) — sets batch clearing rules and fees
 *   - correspondent_banking_network: intermediary beneficiary (powerful/constrained) — brokers rail access for non-members at a spread
 *   - central_bank_settlement_authorities: ultimate settlement guarantor / beneficiary (institutional/analytical) — anchors finality to licensed banks
 *   - retail_depositors and cross_border_remittance_senders: payers (powerless/trapped) — bear fees and float without rail governance voice
 *   - monetary_historians_and_economists: analytical observer — dates the boundary by infrastructure capability, not concept or consumer access
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(digital_money_emergence_boundary__infrastructure_reading, 0.58).
domain_priors:suppression_score(digital_money_emergence_boundary__infrastructure_reading, 0.42).
domain_priors:theater_ratio(digital_money_emergence_boundary__infrastructure_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(digital_money_emergence_boundary__infrastructure_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(digital_money_emergence_boundary__infrastructure_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(digital_money_emergence_boundary__infrastructure_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(digital_money_emergence_boundary__infrastructure_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(digital_money_emergence_boundary__infrastructure_reading, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(digital_money_emergence_boundary__infrastructure_reading, tangled_rope).
narrative_ontology:human_readable(digital_money_emergence_boundary__infrastructure_reading, "Infrastructure Boundary of Digital Money Emergence (Bank Electronic Transfer Rails)").
narrative_ontology:topic_domain(digital_money_emergence_boundary__infrastructure_reading, "monetary_economics/financial_infrastructure").

domain_priors:requires_active_enforcement(digital_money_emergence_boundary__infrastructure_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(digital_money_emergence_boundary__infrastructure_reading, 'cfffe001-8058-4487-a91f-2967ffe74b7c').
narrative_ontology:cs_kernel_codification('cfffe001-8058-4487-a91f-2967ffe74b7c', distributed).
narrative_ontology:cs_authority_grounding('cfffe001-8058-4487-a91f-2967ffe74b7c', practice).
narrative_ontology:cs_interpretation_layer_present('cfffe001-8058-4487-a91f-2967ffe74b7c').
narrative_ontology:cs_reading_relation('cfffe001-8058-4487-a91f-2967ffe74b7c', digital_money_emergence_boundary__conceptualization_reading, influences).
narrative_ontology:cs_reading_relation('cfffe001-8058-4487-a91f-2967ffe74b7c', digital_money_emergence_boundary__consumer_holdings_reading, influences).
narrative_ontology:cs_axiom('cfffe001-8058-4487-a91f-2967ffe74b7c', foundational, transferability_by_banks_constitutes_money_not_holder_access).
narrative_ontology:cs_axiom_status(transferability_by_banks_constitutes_money_not_holder_access, holdable).
narrative_ontology:cs_axiom_grounding('cfffe001-8058-4487-a91f-2967ffe74b7c', transferability_by_banks_constitutes_money_not_holder_access, conventional).
narrative_ontology:cs_axiom('cfffe001-8058-4487-a91f-2967ffe74b7c', secondary, settlement_finality_requires_licensed_intermediary_control).
narrative_ontology:cs_axiom_status(settlement_finality_requires_licensed_intermediary_control, holdable).
narrative_ontology:cs_axiom_grounding('cfffe001-8058-4487-a91f-2967ffe74b7c', settlement_finality_requires_licensed_intermediary_control, instrumental).
narrative_ontology:cs_reference_frame('cfffe001-8058-4487-a91f-2967ffe74b7c', bank_mediated_settlement_primacy).
narrative_ontology:cs_drift_state('cfffe001-8058-4487-a91f-2967ffe74b7c', post_cbdc_pilot_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('cfffe001-8058-4487-a91f-2967ffe74b7c', '').
narrative_ontology:cs_kernel_id(digital_money_emergence_boundary__infrastructure_reading, digital_money_emergence_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(digital_money_emergence_boundary__infrastructure_reading, swift_cooperative).
narrative_ontology:constraint_beneficiary(digital_money_emergence_boundary__infrastructure_reading, ach_operators).
narrative_ontology:constraint_beneficiary(digital_money_emergence_boundary__infrastructure_reading, correspondent_banking_network).
narrative_ontology:constraint_beneficiary(digital_money_emergence_boundary__infrastructure_reading, central_bank_settlement_authorities).
narrative_ontology:constraint_victim(digital_money_emergence_boundary__infrastructure_reading, retail_depositors).
narrative_ontology:constraint_victim(digital_money_emergence_boundary__infrastructure_reading, cross_border_remittance_senders).
narrative_ontology:constraint_victim(digital_money_emergence_boundary__infrastructure_reading, non_member_financial_institutions).
narrative_ontology:constraint_victim(digital_money_emergence_boundary__infrastructure_reading, developing_economy_correspondent_clients).
narrative_ontology:constraint_vindicates(digital_money_emergence_boundary__infrastructure_reading, money_is_defined_by_transferability_not_holder_access).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Operates the messaging standard that most cross-border interbank transfers rely on since 1977; sets membership rules, message formats, and compliance requirements that member banks must meet to participate. Collects fees per message and shapes what counts as a valid electronic transfer instruction.
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__infrastructure_reading, swift_cooperative, agenda_setter,
    institutional, civilizational, arbitrage, global).

% Run the domestic batch clearing systems (established 1972 in the US) that settle direct deposits, bill payments, and interbank transfers; charge participating banks per-transaction or membership fees and define settlement windows and eligibility rules that determine which institutions can move money electronically at all.
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__infrastructure_reading, ach_operators, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(digital_money_emergence_boundary__infrastructure_reading, ach_operators, beneficiary).

% Large money-center banks that hold nostro/vostro accounts for smaller banks, effectively brokering access to the SWIFT/ACH rails for institutions that cannot join directly. They earn spreads and fees on every transfer they intermediate and their gatekeeping position depends on the rails remaining bank-controlled rather than open.
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__infrastructure_reading, correspondent_banking_network, beneficiary,
    powerful, generational, constrained, global).

% Provide the ultimate settlement finality underlying ACH and correspondent transfers through reserve accounts; benefit from the infrastructure boundary because it keeps 'real' money creation and settlement authority anchored to licensed banks rather than to whoever can hold a digital instrument.
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__infrastructure_reading, central_bank_settlement_authorities, beneficiary,
    institutional, civilizational, analytical, national).
narrative_ontology:stakeholder_secondary_role(digital_money_emergence_boundary__infrastructure_reading, central_bank_settlement_authorities, agenda_setter).

% Hold bank deposits that are electronically transferable by the bank on their behalf but cannot access the interbank rails directly; they pay indirectly through fees, float, and the bank's spread on every transfer, without a say in message standards or settlement timing that determines when their 'digital money' actually moves.
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__infrastructure_reading, retail_depositors, payer,
    powerless, biographical, trapped, national).

% Send money across the correspondent banking chain and absorb multiple layers of fees and FX spread at each intermediary hop; the infrastructure boundary's dependence on chained correspondent relationships (rather than direct settlement) is the specific structural feature that generates their cost, and they have no path around it without an alternative rail.
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__infrastructure_reading, cross_border_remittance_senders, payer,
    powerless, biographical, trapped, global).

% Smaller banks, cooperatives, and fintechs in jurisdictions without direct SWIFT or ACH membership access must route through correspondent intermediaries; they would prefer direct settlement access but are excluded by capital, compliance, and membership requirements set by the incumbent rail operators.
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__infrastructure_reading, non_member_financial_institutions, excluded,
    moderate, generational, constrained, regional).

% Banks and their customers in economies dependent on correspondent banking for cross-border settlement bear the cost of de-risking (correspondent banks withdrawing relationships to reduce compliance exposure), which the infrastructure boundary treats as an externality of the rail-control structure rather than a cost the rail operators internalize.
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__infrastructure_reading, developing_economy_correspondent_clients, payer,
    powerless, biographical, trapped, regional).

% Analyze when digital money's boundary should be dated, whether by conceptual possibility, infrastructure capability, or consumer accessibility; note that the infrastructure reading is the one central bankers and BIS statisticians most often adopt when tracking M-aggregate collapse, because it tracks what banks can settle, not what theorists could imagine or consumers could hold.
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__infrastructure_reading, monetary_historians_and_economists, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(digital_money_emergence_boundary__infrastructure_reading, swift_cooperative).
narrative_ontology:fixing_cost_class(digital_money_emergence_boundary__infrastructure_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Standardized electronic messaging (SWIFT) and batch clearing (ACH) solve a genuine multilateral coordination problem: without a shared format and settlement cadence, every bank pair would need bespoke bilateral transfer arrangements, making national and cross-border payments far slower and more error-prone.
% TRANSFER_FUNCTION: Moves fee income and information-control rents from every transaction routed through the rails toward the rail operators (SWIFT, ACH operators) and the correspondent banks that broker access for non-members, while transaction costs, FX spread, and de-risking exposure are moved onto depositors, remitters, and excluded institutions who have no alternative settlement path.
% ABSENT_VOICES: Non-member financial institutions and their retail customers in de-risked corridors would object to membership and correspondent-access terms if given a seat, but rail governance historically sits with founding member banks and central banks, not with excluded institutions or the end customers who bear the pass-through costs.
% DISAPPEARANCE_RATIONALE: If SWIFT messaging and ACH batch clearing vanished overnight, interbank transfers would revert to slower, bilateral, largely manual settlement; cross-border payments would fragment into ad hoc correspondent arrangements without common standards, and the electronic character of 'bank money' that underlies most M2/M3 measurement would itself become uncertain — this is precisely why the infrastructure boundary is treated as consequential rather than incidental.
% FOUNDING_PROBLEM: Banks needed a reliable, standardized way to instruct each other electronically to move funds — replacing paper-based telex, mail, and manual reconciliation that were slow, error-prone, and could not scale with the growth of international trade and domestic consumer banking volume in the 1960s-70s.
% FOUNDING_PROBLEM_CORROBORATION: Central bank payment-systems reports and BIS committee analyses (outside SWIFT's and ACH operators' own governance) corroborate that the original coordination problem (speed, standardization, reconciliation) was real and substantially solved by the 1980s; the same outside analyses increasingly describe current SWIFT/correspondent fee structures and de-risking externalities as extraction layered atop a coordination function that could now be served more cheaply by alternative settlement rails (real-time gross settlement systems, CBDCs), a claim SWIFT and correspondent banks themselves dispute.
narrative_ontology:disappearance_verdict(digital_money_emergence_boundary__infrastructure_reading, world_rearranges).
narrative_ontology:founding_problem_status(digital_money_emergence_boundary__infrastructure_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(digital_money_emergence_boundary__infrastructure_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'f1436bd4937f864097dabaad92b27bd9b6eec212', '2026-08-03',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
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
 *   Extractiveness (0.58) sits at a moderate-high level: the coordination function (standardized, reliable electronic settlement) is real and substantial, but four decades of fee layering, correspondent-chain spreads, and de-risking externalities have accumulated on top of it — the temporal series shows extraction rising from 0.22 at the 1967 ATM introduction to 0.58 by 2024 as the rails matured from novel infrastructure into entrenched, fee-generating chokepoints. Suppression (0.42) reflects real but partial barriers: membership requirements, compliance costs, and correspondent dependency constrain exit for smaller institutions and end users, but the barriers are not absolute — alternative rails (RTGS systems, some CBDC pilots) exist even if underused. Theater ratio is modest (0.28) because most of the apparatus does real settlement work; it is not primarily performative, though a growing share of compliance overhead (KYC/AML layers) functions partly as institutional self-protection rather than pure settlement function.
 *
 * PERSPECTIVAL GAP:
 *   From the rail operator's seat, the infrastructure boundary is a coordination triumph: proof that standardized rails let money move at the speed of instruction rather than the speed of paper. From the trapped depositor or remittance sender's seat, the same boundary is where their money became legible and controllable by intermediaries who take a cut at every hop they cannot avoid. The engine's per-seat computation should register this divergence directly from the declared power/exit asymmetry, not from any claim adjudicating which seat is 'right.'
 *
 * DIRECTIONALITY LOGIC:
 *   SWIFT and ACH operators sit closest to the beneficiary end: they set terms, collect fees, and their exit options are effectively arbitrage-grade (they can restructure pricing or governance with little external check). Correspondent banks and central banks are secondary beneficiaries — they profit from or depend on the rail-control structure without operating it directly. Retail depositors, remittance senders, and developing-economy correspondent clients sit at the target end: trapped exit, no rail governance voice, and the costs of de-risking and correspondent spreads land on them without recourse. Non-member institutions are excluded rather than coordinated — their absence from rail membership is the specific mechanism that sustains correspondent banks' brokering rents.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — the absence of standardized, fast electronic interbank settlement — was substantially solved by the 1980s; the coordination function has not fully disappeared (global payments still need standardized messaging), which is why founding_problem_status is authored as contested rather than dead. The classification as tangled_rope (not snare) preserves this: real coordination persists alongside real, identifiable victims and required active enforcement (membership rules, compliance gatekeeping) — collapsing this into pure extraction would erase the genuine settlement problem the rails still solve; collapsing it into pure coordination would erase the four decades of accumulating extraction the measurement series documents.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    infrastructure_vs_conceptualization_dating,
    'Is the infrastructure reading (banks gain transfer capability, 1967-1977) or the conceptualization reading (digital money becomes theoretically thinkable, 1960s telecom/1985 Chaum) the more structurally correct place to locate digital money''s emergence?',
    'There is no empirical resolution — this is a framing choice about which capability (technical/theoretical possibility vs. operational bank capability) constitutes the defining threshold. Different institutional literatures (BIS/central bank statisticians favor infrastructure; cryptography/monetary theory literatures favor conceptualization) will continue to diverge.',
    'If the conceptualization reading is adopted instead, the beneficiary set shifts from rail operators toward theorists and early cryptographic-protocol designers, and the extraction profile drops sharply (an idea existing collects no rents) — the two readings produce very different ε values, which is exactly why they are authored as separate constraint stories rather than one story with a measurement parameter.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(infrastructure_vs_conceptualization_dating, conceptual, 'Framing choice between the infrastructure-capability boundary and the theoretical-possibility boundary for dating digital money''s emergence.').

omega_variable(
    infrastructure_vs_consumer_holdings_dating,
    'Is the infrastructure reading (banks can transfer electronically even though consumers cannot directly hold digital instruments) or the consumer_holdings reading (consumers can directly hold and transact digital instruments, 1990s e-purses/2000 EMD) the more structurally correct boundary?',
    'Resolvable partly empirically (when did M-aggregate composition statistics actually shift in response to each milestone) but partly conceptually (whether ''money'' requires direct holder access or only bank-mediated transferability) — a mixed omega.',
    'Under the consumer_holdings reading, the beneficiary set shifts toward e-money issuers and payment processors, and the victim set shifts toward merchants and unbanked populations excluded from consumer digital instruments; the infrastructure reading''s beneficiaries (rail operators) and victims (trapped depositors) would not carry over cleanly, confirming these are genuinely distinct constraints, not the same constraint measured differently.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(infrastructure_vs_consumer_holdings_dating, conceptual, 'Framing choice between bank-mediated transferability and direct consumer holding as the defining threshold.').

omega_variable(
    correspondent_deriskng_externality_measurement,
    'How much of the extraction borne by developing_economy_correspondent_clients is attributable to the infrastructure boundary''s correspondent-chain structure specifically, versus general banking regulation (AML/KYC) that would exist under any settlement architecture?',
    'Comparative analysis of de-risking incidence in corridors served by direct RTGS/CBDC settlement versus corridors still dependent on multi-hop correspondent banking, controlling for regulatory regime.',
    'If de-risking is primarily a correspondent-chain artifact, it strengthens the tangled_rope classification (identifiable structural victims of this specific rail architecture); if primarily a general regulatory artifact, the victim attribution to this constraint specifically would need softening.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(correspondent_deriskng_externality_measurement, empirical, 'Whether de-risking externalities are attributable to correspondent-chain architecture or general banking regulation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(digital_money_emergence_boundary__infrastructure_reading, 1967, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(digi_tr_t1967, digital_money_emergence_boundary__infrastructure_reading, theater_ratio, 1967, 0.08).
narrative_ontology:measurement(digi_tr_t1977, digital_money_emergence_boundary__infrastructure_reading, theater_ratio, 1977, 0.1).
narrative_ontology:measurement(digi_tr_t1990, digital_money_emergence_boundary__infrastructure_reading, theater_ratio, 1990, 0.14).
narrative_ontology:measurement(digi_tr_t2001, digital_money_emergence_boundary__infrastructure_reading, theater_ratio, 2001, 0.18).
narrative_ontology:measurement(digi_tr_t2010, digital_money_emergence_boundary__infrastructure_reading, theater_ratio, 2010, 0.22).
narrative_ontology:measurement(digi_tr_t2018, digital_money_emergence_boundary__infrastructure_reading, theater_ratio, 2018, 0.25).
narrative_ontology:measurement(digi_tr_t2024, digital_money_emergence_boundary__infrastructure_reading, theater_ratio, 2024, 0.28).

% Extraction over time
narrative_ontology:measurement(digi_be_t1967, digital_money_emergence_boundary__infrastructure_reading, base_extractiveness, 1967, 0.22).
narrative_ontology:measurement(digi_be_t1977, digital_money_emergence_boundary__infrastructure_reading, base_extractiveness, 1977, 0.3).
narrative_ontology:measurement(digi_be_t1990, digital_money_emergence_boundary__infrastructure_reading, base_extractiveness, 1990, 0.4).
narrative_ontology:measurement(digi_be_t2001, digital_money_emergence_boundary__infrastructure_reading, base_extractiveness, 2001, 0.48).
narrative_ontology:measurement(digi_be_t2010, digital_money_emergence_boundary__infrastructure_reading, base_extractiveness, 2010, 0.53).
narrative_ontology:measurement(digi_be_t2018, digital_money_emergence_boundary__infrastructure_reading, base_extractiveness, 2018, 0.56).
narrative_ontology:measurement(digi_be_t2024, digital_money_emergence_boundary__infrastructure_reading, base_extractiveness, 2024, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(digi_su_t1967, digital_money_emergence_boundary__infrastructure_reading, suppression_requirement, 1967, 0.2).
narrative_ontology:measurement(digi_su_t1977, digital_money_emergence_boundary__infrastructure_reading, suppression_requirement, 1977, 0.28).
narrative_ontology:measurement(digi_su_t1990, digital_money_emergence_boundary__infrastructure_reading, suppression_requirement, 1990, 0.32).
narrative_ontology:measurement(digi_su_t2001, digital_money_emergence_boundary__infrastructure_reading, suppression_requirement, 2001, 0.36).
narrative_ontology:measurement(digi_su_t2010, digital_money_emergence_boundary__infrastructure_reading, suppression_requirement, 2010, 0.4).
narrative_ontology:measurement(digi_su_t2018, digital_money_emergence_boundary__infrastructure_reading, suppression_requirement, 2018, 0.42).
narrative_ontology:measurement(digi_su_t2024, digital_money_emergence_boundary__infrastructure_reading, suppression_requirement, 2024, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(digital_money_emergence_boundary__infrastructure_reading, global_infrastructure).
narrative_ontology:boltzmann_floor_override(digital_money_emergence_boundary__infrastructure_reading, 0.18).
narrative_ontology:affects_constraint(digital_money_emergence_boundary__infrastructure_reading, digital_money_emergence_boundary__conceptualization_reading).
narrative_ontology:affects_constraint(digital_money_emergence_boundary__infrastructure_reading, digital_money_emergence_boundary__consumer_holdings_reading).

% DUAL FORMULATION NOTE:
% This story is the infrastructure_reading member of the digital_money_emergence_boundary kernel family. The conceptualization_reading (theoretical possibility, negligible extraction, closer to mountain/rope) upstream-influences this reading by establishing that electronic transfer was conceivable before banks built the rails to do it. This reading in turn influences the consumer_holdings_reading downstream: once banks could move deposits electronically, the pressure to extend similar capability to consumer-held instruments (e-purses, EMD) built directly on the infrastructure this reading describes. All three stories share the digital_money_emergence_boundary kernel_id but instantiate structurally distinct constraints with different ε values, beneficiary sets, and victim sets — per the ε-invariance principle, they are linked via this network field rather than merged into one story.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
