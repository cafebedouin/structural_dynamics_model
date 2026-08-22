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
 *   human_readable: Infrastructure Boundary of Digital Money Emergence (Bank-Rail Reading)
 *   domain: monetary_economics/financial_history/technology_governance
 *
 * SUMMARY:
 *   This reading locates the emergence of digital money at the point where
 *   bank infrastructure could move deposit claims electronically — 1967 ATM
 *   deployment, 1972 ACH launch, 1977 SWIFT founding — regardless of whether
 *   any consumer could directly hold or manipulate a digital instrument. On
 *   this reading money 'became digital' when the ledger entries moved between
 *   banks over wires rather than when someone could theorize the possibility
 *   (conceptualization_reading) or when a consumer could carry an e-purse
 *   (consumer_holdings_reading). The boundary sits at the interbank layer,
 *   which is precisely why the beneficiary set is the rail operators (SWIFT,
 *   ACH) and the correspondent banks that sit astride the rails, not end
 *   users. The M4/M5 monetary aggregate blur begins exactly here, because
 *   once bank deposits move electronically at scale, the line between 'money'
 *   and 'near-money instruments moved by electronic claim' becomes an
 *   administrative choice made by the same institutions that operate the
 *   rails.
 *
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
narrative_ontology:human_readable(digital_money_emergence_boundary__infrastructure_reading, "Infrastructure Boundary of Digital Money Emergence (Bank-Rail Reading)").
narrative_ontology:topic_domain(digital_money_emergence_boundary__infrastructure_reading, "monetary_economics/financial_history/technology_governance").

domain_priors:requires_active_enforcement(digital_money_emergence_boundary__infrastructure_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(digital_money_emergence_boundary__infrastructure_reading, 'e6db5adf-2c76-49ce-9393-0da5b99c99df').
narrative_ontology:cs_kernel_codification('e6db5adf-2c76-49ce-9393-0da5b99c99df', distributed).
narrative_ontology:cs_authority_grounding('e6db5adf-2c76-49ce-9393-0da5b99c99df', practice).
narrative_ontology:cs_interpretation_layer_present('e6db5adf-2c76-49ce-9393-0da5b99c99df').
narrative_ontology:cs_reading_relation('e6db5adf-2c76-49ce-9393-0da5b99c99df', digital_money_emergence_boundary__conceptualization_reading, coexists_with).
narrative_ontology:cs_reading_relation('e6db5adf-2c76-49ce-9393-0da5b99c99df', digital_money_emergence_boundary__consumer_holdings_reading, influences).
narrative_ontology:cs_axiom('e6db5adf-2c76-49ce-9393-0da5b99c99df', foundational, electronic_interbank_transfer_constitutes_emergence).
narrative_ontology:cs_axiom_status(electronic_interbank_transfer_constitutes_emergence, holdable).
narrative_ontology:cs_axiom_grounding('e6db5adf-2c76-49ce-9393-0da5b99c99df', electronic_interbank_transfer_constitutes_emergence, conventional).
narrative_ontology:cs_axiom('e6db5adf-2c76-49ce-9393-0da5b99c99df', secondary, consumer_access_not_required_for_digital_money_existence).
narrative_ontology:cs_axiom_status(consumer_access_not_required_for_digital_money_existence, holdable).
narrative_ontology:cs_axiom_grounding('e6db5adf-2c76-49ce-9393-0da5b99c99df', consumer_access_not_required_for_digital_money_existence, conventional).
narrative_ontology:cs_reference_frame('e6db5adf-2c76-49ce-9393-0da5b99c99df', interbank_electronic_settlement_capability).
narrative_ontology:cs_drift_state('e6db5adf-2c76-49ce-9393-0da5b99c99df', post_correspondent_fee_scrutiny_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('e6db5adf-2c76-49ce-9393-0da5b99c99df', '').
narrative_ontology:cs_kernel_id(digital_money_emergence_boundary__infrastructure_reading, digital_money_emergence_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(digital_money_emergence_boundary__infrastructure_reading, swift_cooperative).
narrative_ontology:constraint_beneficiary(digital_money_emergence_boundary__infrastructure_reading, ach_network_operators).
narrative_ontology:constraint_beneficiary(digital_money_emergence_boundary__infrastructure_reading, correspondent_banking_network).
narrative_ontology:constraint_beneficiary(digital_money_emergence_boundary__infrastructure_reading, central_bank_settlement_authorities).
narrative_ontology:constraint_victim(digital_money_emergence_boundary__infrastructure_reading, retail_depositors).
narrative_ontology:constraint_victim(digital_money_emergence_boundary__infrastructure_reading, cross_border_remittance_senders).
narrative_ontology:constraint_victim(digital_money_emergence_boundary__infrastructure_reading, non_bank_payment_innovators).
narrative_ontology:constraint_vindicates(digital_money_emergence_boundary__infrastructure_reading, bank_mediated_transfer_constitutes_money_creation_event).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Operates the messaging standard that lets banks instruct each other to move electronic deposit claims across borders. Sets the technical protocol, membership rules, and correspondent relationships that define what counts as a valid interbank transfer. Collects membership and message fees from every participating bank, and its standard becomes the de facto definition of 'moved money' for the entire correspondent system.
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__infrastructure_reading, swift_cooperative, agenda_setter,
    institutional, generational, arbitrage, global).

% Administer the domestic batch-clearing rails (from the 1972 ACH launch onward) that let commercial banks settle electronic deposit transfers overnight instead of by paper check. Set the settlement windows, reserve requirements, and reversal rules. Extract transaction and infrastructure fees while defining, for regulators and depositors alike, what an 'electronic transfer' legally is.
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__infrastructure_reading, ach_network_operators, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(digital_money_emergence_boundary__infrastructure_reading, ach_network_operators, beneficiary).

% Commercial and money-center banks that hold accounts for one another and use the SWIFT/ACH rails to move client deposit claims. They gain gatekeeping power over who can transact internationally at all, and earn spread and fee income on every relayed transfer, while carrying almost none of the compliance burden that falls on end users.
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__infrastructure_reading, correspondent_banking_network, beneficiary,
    powerful, generational, constrained, global).

% Provide the ultimate settlement finality (reserve accounts) underneath the ACH and SWIFT layers, and use the boundary between 'bank money moved electronically' and 'money in any other form' to draw M4/M5 aggregate lines that guide monetary policy. Benefit from a definitional boundary that keeps electronic bank liabilities inside the regulated core they already supervise.
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__infrastructure_reading, central_bank_settlement_authorities, beneficiary,
    institutional, civilizational, analytical, national).
narrative_ontology:stakeholder_secondary_role(digital_money_emergence_boundary__infrastructure_reading, central_bank_settlement_authorities, agenda_setter).

% Hold checking and savings balances that are moved electronically on their behalf by ACH and correspondent transfers, but cannot access, verify, or bypass the underlying rails. Pay indirectly through fees, float, and delayed settlement, and have no standing to dispute whether their money 'moved' before the bank says it did — the infrastructure boundary is set entirely above their heads.
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__infrastructure_reading, retail_depositors, payer,
    powerless, biographical, trapped, national).

% Migrant workers and small businesses sending funds internationally pay compounding correspondent-chain fees and multi-day delays created by the very rails (SWIFT messaging, correspondent hops) that this reading treats as the moment money becomes digital. They bear the highest per-transaction extraction of any group governed by this boundary and have essentially no alternative rail with comparable reach.
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__infrastructure_reading, cross_border_remittance_senders, payer,
    powerless, immediate, trapped, global).

% Fintech and non-bank payment firms that want direct access to clearing and settlement rails are structurally excluded from SWIFT/ACH membership or admitted only through sponsor-bank arrangements that preserve incumbent gatekeeping. They would argue that 'infrastructure-enabled transfer' should not require bank intermediation at all, but this reading's own definition of the emergence boundary keeps them outside the core.
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__infrastructure_reading, non_bank_payment_innovators, excluded,
    moderate, biographical, constrained, national).

% Study when digital money 'began' as a matter of infrastructural capability rather than consumer experience or theoretical possibility. They document the 1967 ATM rollout, 1972 ACH launch, and 1977 SWIFT founding as the infrastructure reading's evidentiary anchors, and note the M4/M5 aggregate blur this reading produces without holding a stake in which reading prevails.
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__infrastructure_reading, monetary_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(digital_money_emergence_boundary__infrastructure_reading, swift_cooperative).
narrative_ontology:fixing_cost_class(digital_money_emergence_boundary__infrastructure_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Electronic clearing and messaging infrastructure (ATMs, ACH, SWIFT) solves a genuine coordination problem: it lets banks settle claims against one another and against customers without physically moving paper instruments, at a speed and volume paper clearing could never match.
% TRANSFER_FUNCTION: Moves fee income, float income, and definitional authority over 'what counts as money movement' from depositors and remittance senders toward the institutions that operate and interconnect via the rails — banks, ACH operators, and the SWIFT cooperative.
% ABSENT_VOICES: Non-bank payment innovators and ordinary depositors would object that infrastructure control should not double as the definitional boundary of money's digital emergence, since it entrenches whoever already operates the rails; neither group has a seat in the standards bodies (SWIFT board, ACH network governance) that set the boundary.
% DISAPPEARANCE_RATIONALE: If the ACH/SWIFT infrastructure boundary were abolished as the defining moment of digital money's emergence, monetary statistics (M4/M5), regulatory capital treatment of electronic deposits, and correspondent banking fee structures would all require re-derivation from a different anchor point — the entire apparatus of interbank settlement law is built on treating electronic bank transfer as the emergence event.
% FOUNDING_PROBLEM: Paper-based check clearing and physical currency movement could not scale to the volume and geographic reach of mid-20th-century commerce and cross-border finance; banks needed a faster, auditable way to move deposit claims among themselves.
% FOUNDING_PROBLEM_CORROBORATION: Central bank settlement authorities and SWIFT/ACH operators attest the clearing-speed problem remains live and the infrastructure is still the necessary backbone. Independent monetary historians and fintech-sector antitrust filings attest that the original clearing-speed problem was substantially solved by the 1980s, and that continued restriction of rail access now functions primarily as incumbent gatekeeping rather than solving an unsolved coordination problem.
narrative_ontology:disappearance_verdict(digital_money_emergence_boundary__infrastructure_reading, world_rearranges).
narrative_ontology:founding_problem_status(digital_money_emergence_boundary__infrastructure_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(digital_money_emergence_boundary__infrastructure_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
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
 *   Extraction (0.58 by 2025, rising from 0.22 in 1967) reflects that the coordination function (fast, auditable interbank settlement) is real but has increasingly been layered with fee extraction — correspondent banking chains, cross-border remittance markups, and membership gatekeeping — that exceeds the marginal cost of moving a ledger entry. Suppression (0.42, rising from 0.20) captures the entrenchment of rail access as a bank-only privilege: non-bank entrants are structurally kept out or forced through sponsor arrangements. Theater ratio stays moderate-low (0.28) because the underlying clearing function remains genuinely operative — this is not primarily a performative constraint, though an increasing share of correspondent-chain complexity now serves fee capture rather than settlement necessity. All three series share the single time grid (1967-2025) so no metric injects an end-state value at an earlier point.
 *
 * DIRECTIONALITY LOGIC:
 *   SWIFT and ACH operators sit at the low-d beneficiary end: they set the protocol, collect on every message and transaction, and face effectively arbitrage-grade exit (no comparable rail can bypass them). Correspondent banks and central bank settlement authorities cluster similarly, benefiting from gatekeeping and definitional authority respectively. Retail depositors and cross-border remittance senders sit at the high-d target end: trapped exit, no visibility into rail operation, and the highest per-transaction cost burden despite having no role in setting the boundary. Non-bank payment innovators are excluded rather than coordinated — their absence from rail governance is the mechanism that preserves incumbent extraction, not a byproduct of it.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — paper clearing could not scale — was substantially solved by the 1980s once ACH and SWIFT reached mature operational volume. The infrastructure has not sunsetted; instead its governance and fee structures have hardened into a tangled rope: coordination (fast settlement) persists genuinely, but active enforcement (membership gatekeeping, correspondent-chain markups) increasingly serves rent extraction on top of a solved problem. Classifying this reading as tangled_rope rather than pure rope or pure snare prevents mislabeling: the coordination function is not fake (unlike a pure snare), but the persistence of gatekeeping well past the original bottleneck is not innocent coordination either.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    infrastructure_vs_conceptualization_priority,
    'Does money''s digital emergence properly date to when banks COULD move it electronically (infrastructure_reading) or to when the concept of digital money was first formalized as theoretically distinct from physical instruments (conceptualization_reading)?',
    'There is no empirical resolution — this is a framing choice about what ''emergence'' means (operational capability vs. conceptual possibility). Historians of monetary technology and legal scholars drawing regulatory boundaries would resolve it differently depending on whether the question is ''when could it be done'' or ''when was it understood as a category.''',
    'Adopting conceptualization_reading instead would shift the beneficiary set away from rail operators toward academic/cryptographic communities and would push the emergence date earlier (1960s telecom theory, 1985 Chaum) with a very different, much lower extraction profile, since no rent-collecting infrastructure is implicated in pure conceptualization.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(infrastructure_vs_conceptualization_priority, conceptual, 'Whether emergence is dated by infrastructural capability or by conceptual formalization — a framing choice, not a fact to be discovered.').

omega_variable(
    m4_m5_aggregate_boundary_ambiguity,
    'Is the blurring of M4/M5 monetary aggregates once bank deposits move electronically a natural consequence of technological capability, or a constructed administrative choice that benefits central banks and rail operators by keeping electronic bank liabilities inside the regulated core?',
    'Comparative analysis of jurisdictions that drew the electronic-deposit aggregate boundary differently, or historical central bank internal deliberations on where to place the M4/M5 line, would show whether the boundary tracks a genuine measurement necessity or a governance-preserving choice.',
    'If the boundary is substantially discretionary rather than technologically forced, the beneficiary declaration for central_bank_settlement_authorities is strengthened and the constraint''s coordination claim (that this is simply where ''money'' naturally starts) weakens further toward tangled_rope or even snare at the aggregate-definition layer specifically.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(m4_m5_aggregate_boundary_ambiguity, conceptual, 'Whether the M4/M5 aggregate blur here is a discovered fact or a constructed governance choice.').

omega_variable(
    correspondent_fee_necessity,
    'How much of the cross-border remittance fee burden reflects genuine settlement risk and compliance cost versus rent extraction enabled by correspondent-chain gatekeeping?',
    'Cost-accounting disclosure from correspondent banks compared against actual settlement risk and compliance expense, or natural experiments from jurisdictions adopting real-time gross settlement bypasses (e.g., some regional instant-payment schemes) that reduce correspondent hops.',
    'A high extraction share would support classifying the remittance-facing portion of this constraint closer to snare for that specific victim group; a low share would support the tangled_rope reading''s claim that most of the cost is genuine coordination expense.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(correspondent_fee_necessity, empirical, 'Whether correspondent banking fees on remittances reflect real settlement cost or extracted rent.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(digital_money_emergence_boundary__infrastructure_reading, 1967, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(digi_tr_t1967, digital_money_emergence_boundary__infrastructure_reading, theater_ratio, 1967, 0.1).
narrative_ontology:measurement(digi_tr_t1977, digital_money_emergence_boundary__infrastructure_reading, theater_ratio, 1977, 0.13).
narrative_ontology:measurement(digi_tr_t1990, digital_money_emergence_boundary__infrastructure_reading, theater_ratio, 1990, 0.17).
narrative_ontology:measurement(digi_tr_t2005, digital_money_emergence_boundary__infrastructure_reading, theater_ratio, 2005, 0.21).
narrative_ontology:measurement(digi_tr_t2015, digital_money_emergence_boundary__infrastructure_reading, theater_ratio, 2015, 0.25).
narrative_ontology:measurement(digi_tr_t2025, digital_money_emergence_boundary__infrastructure_reading, theater_ratio, 2025, 0.28).

% Extraction over time
narrative_ontology:measurement(digi_be_t1967, digital_money_emergence_boundary__infrastructure_reading, base_extractiveness, 1967, 0.22).
narrative_ontology:measurement(digi_be_t1977, digital_money_emergence_boundary__infrastructure_reading, base_extractiveness, 1977, 0.3).
narrative_ontology:measurement(digi_be_t1990, digital_money_emergence_boundary__infrastructure_reading, base_extractiveness, 1990, 0.4).
narrative_ontology:measurement(digi_be_t2005, digital_money_emergence_boundary__infrastructure_reading, base_extractiveness, 2005, 0.49).
narrative_ontology:measurement(digi_be_t2015, digital_money_emergence_boundary__infrastructure_reading, base_extractiveness, 2015, 0.54).
narrative_ontology:measurement(digi_be_t2025, digital_money_emergence_boundary__infrastructure_reading, base_extractiveness, 2025, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(digi_su_t1967, digital_money_emergence_boundary__infrastructure_reading, suppression_requirement, 1967, 0.2).
narrative_ontology:measurement(digi_su_t1977, digital_money_emergence_boundary__infrastructure_reading, suppression_requirement, 1977, 0.26).
narrative_ontology:measurement(digi_su_t1990, digital_money_emergence_boundary__infrastructure_reading, suppression_requirement, 1990, 0.31).
narrative_ontology:measurement(digi_su_t2005, digital_money_emergence_boundary__infrastructure_reading, suppression_requirement, 2005, 0.36).
narrative_ontology:measurement(digi_su_t2015, digital_money_emergence_boundary__infrastructure_reading, suppression_requirement, 2015, 0.39).
narrative_ontology:measurement(digi_su_t2025, digital_money_emergence_boundary__infrastructure_reading, suppression_requirement, 2025, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(digital_money_emergence_boundary__infrastructure_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(digital_money_emergence_boundary__infrastructure_reading, 0.12).
narrative_ontology:affects_constraint(digital_money_emergence_boundary__infrastructure_reading, digital_money_emergence_boundary__conceptualization_reading).
narrative_ontology:affects_constraint(digital_money_emergence_boundary__infrastructure_reading, digital_money_emergence_boundary__consumer_holdings_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the digital_money_emergence_boundary kernel, decomposed per the ε-invariance principle because the three readings produce structurally distinct ε values, beneficiary sets, and victim sets rather than one measurement-dependent value. infrastructure_reading (this file) dates emergence to interbank rail capability (1967-1977) with rail operators and correspondent banks as beneficiaries, ε=0.58. conceptualization_reading dates emergence to theoretical formalization (1960s-1985) with a much lower extraction profile since no rent-collecting infrastructure is implicated. consumer_holdings_reading dates emergence to direct consumer digital instrument holding (1990s e-purses, 2000 EMD) with a different beneficiary set (e-money issuers, payment card networks) and different victim exposure (consumers bearing float and fee costs directly rather than through bank intermediation). All three link to each other via affects_constraints; each carries its own claimed_type and metrics.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(digital_money_emergence_boundary__infrastructure_reading, institutional, 0.12).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
