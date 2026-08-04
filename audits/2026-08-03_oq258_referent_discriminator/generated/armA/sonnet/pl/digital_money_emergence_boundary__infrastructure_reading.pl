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
 *   human_readable: Infrastructure Boundary Reading: Digital Money Emerges When Bank Rails Move It Electronically
 *   domain: monetary_economics/financial_history/technology_governance
 *
 * SUMMARY:
 *   This story instantiates the INFRASTRUCTURE reading of the contested
 *   digital-money-emergence kernel: digital money is held to have emerged
 *   when banking infrastructure — ATMs (1967), ACH (1972), SWIFT (1977) —
 *   made electronic transfer of bank deposits possible, regardless of whether
 *   consumers could directly hold or transact with a digital instrument
 *   themselves, and regardless of whether the underlying concept had been
 *   formally theorized. This is a middle boundary: later than the
 *   conceptualization reading (which locates emergence in 1960s telecom
 *   theory and Chaum's 1985 formalization) and earlier than the
 *   consumer-holdings reading (which requires 1990s e-purses / 2000 EMD
 *   before digital money counts as existing). The beneficiary structure under
 *   THIS reading is distinctive: SWIFT and ACH operators and the
 *   correspondent banks with first-mover rail access, not cryptographers or
 *   consumer-facing e-money issuers. The M4/M5 monetary-aggregate boundary
 *   begins to blur specifically under this reading, because once bank
 *   deposits move electronically between institutions, the question of
 *   whether an electronically-moved deposit claim is 'money' or merely a
 *   faster claim-on-money becomes structurally ambiguous — and that ambiguity
 *   is resolved, in practice, by whoever controls the rails.
 *
 * KEY AGENTS:
 *   - swift_cooperative: agenda-setter and beneficiary, sets the cross-border messaging standard and collects on volume
 *   - ach_network_operators: agenda-setter and beneficiary, set domestic clearing rules and collect processing fees
 *   - correspondent_banks: beneficiary via first-mover rail access advantage
 *   - non_member_financial_institutions: payer, routes through correspondents at a markup
 *   - unbanked_populations: excluded, has no seat and no account through which the infrastructure's 'emergence' reaches them
 *   - cross_border_remittance_senders: payer, bears the highest per-dollar cost of the very rails credited with creating digital money
 *   - monetary_historians: analytical observer across all three kernel readings
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(digital_money_emergence_boundary__infrastructure_reading, 0.42).
domain_priors:suppression_score(digital_money_emergence_boundary__infrastructure_reading, 0.38).
domain_priors:theater_ratio(digital_money_emergence_boundary__infrastructure_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(digital_money_emergence_boundary__infrastructure_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(digital_money_emergence_boundary__infrastructure_reading, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(digital_money_emergence_boundary__infrastructure_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(digital_money_emergence_boundary__infrastructure_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(digital_money_emergence_boundary__infrastructure_reading, resistance, 0.32).

% --- Constraint claim ---
narrative_ontology:constraint_claim(digital_money_emergence_boundary__infrastructure_reading, tangled_rope).
narrative_ontology:human_readable(digital_money_emergence_boundary__infrastructure_reading, "Infrastructure Boundary Reading: Digital Money Emerges When Bank Rails Move It Electronically").
narrative_ontology:topic_domain(digital_money_emergence_boundary__infrastructure_reading, "monetary_economics/financial_history/technology_governance").

domain_priors:requires_active_enforcement(digital_money_emergence_boundary__infrastructure_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(digital_money_emergence_boundary__infrastructure_reading, '76a2c9a8-bafe-4998-8366-44b562f03888').
narrative_ontology:cs_kernel_codification('76a2c9a8-bafe-4998-8366-44b562f03888', distributed).
narrative_ontology:cs_authority_grounding('76a2c9a8-bafe-4998-8366-44b562f03888', distributed).
narrative_ontology:cs_reading_relation('76a2c9a8-bafe-4998-8366-44b562f03888', digital_money_emergence_boundary__conceptualization_reading, coexists_with).
narrative_ontology:cs_reading_relation('76a2c9a8-bafe-4998-8366-44b562f03888', digital_money_emergence_boundary__consumer_holdings_reading, influences).
narrative_ontology:cs_axiom('76a2c9a8-bafe-4998-8366-44b562f03888', foundational, bank_electronic_movability_sufficient_for_money_status).
narrative_ontology:cs_axiom_status(bank_electronic_movability_sufficient_for_money_status, holdable).
narrative_ontology:cs_axiom_grounding('76a2c9a8-bafe-4998-8366-44b562f03888', bank_electronic_movability_sufficient_for_money_status, conventional).
narrative_ontology:cs_axiom('76a2c9a8-bafe-4998-8366-44b562f03888', secondary, consumer_direct_holding_not_required_for_emergence).
narrative_ontology:cs_axiom_status(consumer_direct_holding_not_required_for_emergence, holdable).
narrative_ontology:cs_axiom_grounding('76a2c9a8-bafe-4998-8366-44b562f03888', consumer_direct_holding_not_required_for_emergence, conventional).
narrative_ontology:cs_reference_frame('76a2c9a8-bafe-4998-8366-44b562f03888', paper_instrument_settlement_baseline).
narrative_ontology:cs_drift_state('76a2c9a8-bafe-4998-8366-44b562f03888', post_swift_maturity_1990s, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('76a2c9a8-bafe-4998-8366-44b562f03888', '').
narrative_ontology:cs_kernel_id(digital_money_emergence_boundary__infrastructure_reading, digital_money_emergence_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(digital_money_emergence_boundary__infrastructure_reading, swift_cooperative).
narrative_ontology:constraint_beneficiary(digital_money_emergence_boundary__infrastructure_reading, ach_network_operators).
narrative_ontology:constraint_beneficiary(digital_money_emergence_boundary__infrastructure_reading, correspondent_banks).
narrative_ontology:constraint_victim(digital_money_emergence_boundary__infrastructure_reading, unbanked_populations).
narrative_ontology:constraint_victim(digital_money_emergence_boundary__infrastructure_reading, non_member_financial_institutions).
narrative_ontology:constraint_victim(digital_money_emergence_boundary__infrastructure_reading, cross_border_remittance_senders).
narrative_ontology:constraint_vindicates(digital_money_emergence_boundary__infrastructure_reading, electronic_settlement_finality_doctrine).
narrative_ontology:constraint_vindicates(digital_money_emergence_boundary__infrastructure_reading, bank_deposit_dematerialization_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Operates the messaging standard that member banks use to instruct cross-border transfers, and sets the technical and membership rules governing who can participate. Collects fees on message volume and captures the standard-setting position that makes its rails the default reference point for what counts as a completed interbank transfer.
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__infrastructure_reading, swift_cooperative, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(digital_money_emergence_boundary__infrastructure_reading, swift_cooperative, beneficiary).

% Run the batch-clearing infrastructure that moved domestic bank deposits from paper-instrument settlement to same-day and next-day electronic settlement starting 1972. Set the technical rules for participation and collect processing fees from member banks, positioning themselves as the arbiter of when a domestic deposit transfer 'happened.'
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__infrastructure_reading, ach_network_operators, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(digital_money_emergence_boundary__infrastructure_reading, ach_network_operators, beneficiary).

% Large banks with existing rail access captured first-mover advantage: they could offer clients same-day electronic settlement while smaller institutions could not. Their deposit ledgers became the de facto record of 'digital money' long before any consumer touched a device, and they benefit from the ambiguity over whether M4/M5 boundary-crossing electronic deposits are money or merely a claim on money.
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__infrastructure_reading, correspondent_banks, beneficiary,
    powerful, generational, constrained, global).

% Smaller banks, credit unions, and non-bank payment providers without direct SWIFT or ACH membership must route through correspondent relationships, paying markup and losing settlement-speed advantage. Their transfers still count as 'money moved' under this reading, but only by paying rent to institutions that hold the rails.
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__infrastructure_reading, non_member_financial_institutions, payer,
    moderate, biographical, constrained, national).

% Have no account through which to access ACH or SWIFT-mediated transfer at all. Under the infrastructure reading, digital money exists in their economy the moment banks can move deposits electronically — but this existence claim is made entirely without reference to whether they can touch, hold, or benefit from that money. They are structurally absent from the boundary-setting conversation.
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__infrastructure_reading, unbanked_populations, excluded,
    powerless, biographical, trapped, global).

% Migrant workers and diaspora communities sending money home pay disproportionate fees precisely because the emergence of electronic bank-to-bank transfer (SWIFT correspondent chains) created a rail system priced for institutional volume, not small remittances. They bear the highest per-dollar cost of the infrastructure this reading credits with 'creating' digital money.
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__infrastructure_reading, cross_border_remittance_senders, payer,
    powerless, immediate, trapped, global).

% Debate where to place the emergence boundary for digital money — infrastructure capability, theoretical conceptualization, or consumer-holdable instruments — and note that each boundary choice assigns credit (and rents) to a different set of institutional actors.
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
% COORDINATION_FUNCTION: Bank-to-bank electronic settlement (ATM networks 1967, ACH 1972, SWIFT 1977) solved the genuine problem of costly, slow, error-prone paper-based interbank and cross-border settlement, allowing deposit balances to move at the speed of a message rather than the speed of a courier.
% TRANSFER_FUNCTION: Moves settlement finality and transaction-cost savings from paper-clearing intermediaries to rail operators and the correspondent banks with first access; moves disproportionate per-transaction cost onto non-member institutions and small remittance senders who must pay to access rails they do not control.
% ABSENT_VOICES: Unbanked populations and small remittance senders have no seat in defining the emergence boundary; the boundary is set entirely by institutions that already hold rail access, so the definition of 'when digital money emerged' silently encodes whose access counted as the trigger.
% DISAPPEARANCE_RATIONALE: If the ACH/SWIFT electronic-transfer infrastructure vanished overnight, interbank and cross-border settlement would revert to physical instrument clearing or bilateral wire arrangements; deposit balances would no longer move at electronic speed, correspondent banks would lose their settlement-speed advantage, and the entire edifice of same-day domestic and next-day international settlement would collapse — a substantial, not cosmetic, rearrangement.
% FOUNDING_PROBLEM: Interbank and cross-border payment settlement in the 1960s-70s relied on physical instruments (checks, paper telex confirmations) that were slow, expensive, error-prone, and exposed institutions to settlement risk during the float period.
% FOUNDING_PROBLEM_CORROBORATION: Central bank payment-system historians and BIS settlement-risk reports from outside SWIFT/ACH corroborate that the original settlement-speed and error-reduction problem was substantially solved by the 1980s; SWIFT and ACH operators themselves continue to frame their infrastructure as solving an ongoing (rather than historically completed) problem, which is the self-interested account this story treats with skepticism.
narrative_ontology:disappearance_verdict(digital_money_emergence_boundary__infrastructure_reading, world_rearranges).
narrative_ontology:founding_problem_status(digital_money_emergence_boundary__infrastructure_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(digital_money_emergence_boundary__infrastructure_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-04',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(digital_money_emergence_boundary__infrastructure_reading, 'none', 1).
narrative_ontology:epsilon_provenance(digital_money_emergence_boundary__infrastructure_reading, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(digital_money_emergence_boundary__infrastructure_reading_tests).
:- end_tests(digital_money_emergence_boundary__infrastructure_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.42 at interval end) and rises gradually as the rail network matures from ATM cash-access (1967, low extraction, largely genuine convenience coordination) through ACH batch clearing (1972, moderate extraction as fee structures solidify) to SWIFT's global correspondent messaging standard (1977 onward, where message-fee and correspondent-markup extraction becomes structurally embedded). Suppression is lower than extractiveness throughout (peaking at 0.38) because exclusion from the rails is largely a function of not-yet-having-access rather than active coercive barrier-erection — though this is precisely the coordination/extraction hybrid that motivates the tangled_rope claim: the rails solve a real settlement-speed problem (rope) AND their access rules generate a durable rent for whoever holds a rail seat (extraction), sustained by ongoing membership-gating enforcement.
 *
 * DIRECTIONALITY LOGIC:
 *   SWIFT and ACH operators sit at the full-beneficiary end: institutional power, arbitrage-grade exit (they can renegotiate terms with no comparable counter-leverage), global/national scope. Correspondent banks with early access sit near beneficiary but somewhat less capturing than the rail operators themselves. Non-member institutions and remittance senders sit toward the target end: constrained-to-trapped exit, no seat in rule-setting, and they pay the markup that the rail's gatekeeping generates. Unbanked populations are excluded entirely rather than positioned as payers in the conventional sense — the infrastructure reading's emergence claim is made about a system they structurally cannot enter, which is itself a form of extraction-by-omission worth flagging even though no direct payment flows from them.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (paper-clearing settlement risk and cost) was substantially solved by the 1980s according to independent central-bank and BIS historical accounts, yet SWIFT and ACH continue to frame their infrastructure as addressing a live, ongoing problem — a contested founding-problem status that is the diagnostic signature of a tangled rope rather than either a pure rope (problem fully solved, function should sunset) or a pure snare (no genuine coordination ever existed). The coordination function was real and remains partially real (settlement speed genuinely matters), but the persistence of rail-access rents well past the point of pure necessity is the extraction layered on top.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    infrastructure_vs_conceptual_priority,
    'Does the historical fact that electronic transfer infrastructure (ATMs, ACH, SWIFT) preceded widespread formal theorization of digital money (Chaum 1985) establish that infrastructure capability is causally or definitionally prior to the concept, or did informal conceptual groundwork (telecommunications-era thinking about electronic value transfer) already exist before 1967 and merely lack formalization?',
    'Archival review of internal bank and central-bank planning documents from the 1960s to establish whether ATM/ACH design presupposed an articulated concept of ''electronic money'' as distinct from ''electronic instruction to move money.''',
    'If informal conceptual groundwork predates 1967, the infrastructure reading''s claim to be a genuinely distinct emergence boundary (rather than a downstream implementation of a concept that already existed) weakens, and the conceptualization reading''s priority claim strengthens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(infrastructure_vs_conceptual_priority, conceptual, 'Whether infrastructure capability or prior conceptual groundwork should be treated as the true trigger for emergence.').

omega_variable(
    rail_access_natural_vs_constructed_gatekeeping,
    'Is the correspondent-bank/rail-operator advantage a natural consequence of first-mover technical investment (a legitimate coordination reward) or a constructed gatekeeping structure maintained through membership rules that could, in principle, be opened without loss of settlement function?',
    'Comparative study of jurisdictions or eras where central banks mandated open access to real-time settlement rails (e.g. instant payment schemes with mandated participation) and measured whether settlement quality degraded or rents simply redistributed.',
    'If open access preserves settlement quality while redistributing rents, the extraction component of this tangled_rope is separable from its coordination component, supporting stronger regulatory intervention; if not, the current gatekeeping is closer to necessary coordination cost.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(rail_access_natural_vs_constructed_gatekeeping, empirical, 'Whether rail-access gatekeeping is separable extraction or bundled coordination necessity.').

omega_variable(
    kernel_framing_boundary_choice,
    'The kernel''s three readings (conceptualization, infrastructure, consumer_holdings) could also be framed as a single continuous process rather than three discrete boundary claims — does treating them as three distinct emergence points (each with its own beneficiary structure) more accurately capture the historical record than treating ''digital money emergence'' as a single gradual, multi-decade diffusion with no sharp boundary at all?',
    'None fully resolves this — it is a framing choice about whether monetary history admits discrete boundary events or only gradual diffusion; historians of technology diffusion (e.g. Rogers'' diffusion-of-innovation framework) could be consulted for whether analogous technologies show sharp or gradual emergence patterns.',
    'If gradual diffusion is the more accurate frame, then all three kernel readings (including this one) are somewhat artificial discretizations imposed for analytical tractability, and the beneficiary structure attributed to each boundary is partly an artifact of where the analyst chose to draw the line rather than a fact about history.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_framing_boundary_choice, conceptual, 'Whether the three-reading kernel decomposition itself reflects genuine discrete emergence points or imposes false discreteness on a continuous process.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(digital_money_emergence_boundary__infrastructure_reading, 1967, 2000).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(digi_tr_t1967, digital_money_emergence_boundary__infrastructure_reading, theater_ratio, 1967, 0.1).
narrative_ontology:measurement(digi_tr_t1972, digital_money_emergence_boundary__infrastructure_reading, theater_ratio, 1972, 0.14).
narrative_ontology:measurement(digi_tr_t1977, digital_money_emergence_boundary__infrastructure_reading, theater_ratio, 1977, 0.18).
narrative_ontology:measurement(digi_tr_t1985, digital_money_emergence_boundary__infrastructure_reading, theater_ratio, 1985, 0.22).
narrative_ontology:measurement(digi_tr_t1993, digital_money_emergence_boundary__infrastructure_reading, theater_ratio, 1993, 0.25).
narrative_ontology:measurement(digi_tr_t2000, digital_money_emergence_boundary__infrastructure_reading, theater_ratio, 2000, 0.28).

% Extraction over time
narrative_ontology:measurement(digi_be_t1967, digital_money_emergence_boundary__infrastructure_reading, base_extractiveness, 1967, 0.15).
narrative_ontology:measurement(digi_be_t1972, digital_money_emergence_boundary__infrastructure_reading, base_extractiveness, 1972, 0.22).
narrative_ontology:measurement(digi_be_t1977, digital_money_emergence_boundary__infrastructure_reading, base_extractiveness, 1977, 0.3).
narrative_ontology:measurement(digi_be_t1985, digital_money_emergence_boundary__infrastructure_reading, base_extractiveness, 1985, 0.36).
narrative_ontology:measurement(digi_be_t1993, digital_money_emergence_boundary__infrastructure_reading, base_extractiveness, 1993, 0.4).
narrative_ontology:measurement(digi_be_t2000, digital_money_emergence_boundary__infrastructure_reading, base_extractiveness, 2000, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(digi_su_t1967, digital_money_emergence_boundary__infrastructure_reading, suppression_requirement, 1967, 0.2).
narrative_ontology:measurement(digi_su_t1972, digital_money_emergence_boundary__infrastructure_reading, suppression_requirement, 1972, 0.26).
narrative_ontology:measurement(digi_su_t1977, digital_money_emergence_boundary__infrastructure_reading, suppression_requirement, 1977, 0.31).
narrative_ontology:measurement(digi_su_t1985, digital_money_emergence_boundary__infrastructure_reading, suppression_requirement, 1985, 0.34).
narrative_ontology:measurement(digi_su_t1993, digital_money_emergence_boundary__infrastructure_reading, suppression_requirement, 1993, 0.36).
narrative_ontology:measurement(digi_su_t2000, digital_money_emergence_boundary__infrastructure_reading, suppression_requirement, 2000, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(digital_money_emergence_boundary__infrastructure_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(digital_money_emergence_boundary__infrastructure_reading, 0.12).
narrative_ontology:affects_constraint(digital_money_emergence_boundary__infrastructure_reading, digital_money_emergence_boundary__conceptualization_reading).
narrative_ontology:affects_constraint(digital_money_emergence_boundary__infrastructure_reading, digital_money_emergence_boundary__consumer_holdings_reading).

% DUAL FORMULATION NOTE:
% This constraint is the middle reading of a three-way kernel decomposition of 'when digital money emerged.' The conceptualization_reading (upstream, earlier boundary, lower extraction, academic/cryptographic beneficiary set) and the consumer_holdings_reading (downstream, later boundary, e-money-issuer beneficiary set) are separate constraint files with their own ε values and stakeholder structures. Per the ε-invariance principle, these are not the same constraint measured three ways — they are three structurally distinct claims sharing a common colloquial label ('the emergence of digital money'), each instantiating a different beneficiary structure and a different extraction profile. Network edges here point downstream toward consumer_holdings (infrastructure access preconditions later consumer-facing digital instruments) and laterally toward conceptualization (the infrastructure reading's rail operators later draw on formalized cryptographic concepts for security and settlement-finality claims).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
