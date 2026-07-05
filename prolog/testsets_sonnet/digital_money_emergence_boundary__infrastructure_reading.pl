% ============================================================================
% CONSTRAINT STORY: digital_money_emergence_boundary__infrastructure_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   human_readable: Digital Money Emergence Boundary — Infrastructure (Interbank Rail) Reading
 *   domain: monetary_economics/financial_history/technology_governance
 *
 * SUMMARY:
 *   Between 1967 and 2000, a sequence of banking infrastructure milestones
 *   (ATMs, ACH, SWIFT) gave banks the capability to move money electronically
 *   between institutions, well before any ordinary consumer could hold a
 *   digital monetary instrument outside a bank account. Under this reading,
 *   'digital money' emerged at the rail layer: banks, not consumers, are the
 *   units whose relationship to money changed. That rail layer created a
 *   genuine coordination good (faster, more reliable settlement than
 *   paper/telex) but also created a gatekeeping structure — correspondent
 *   banking, membership fees, access asymmetries — that persists and extracts
 *   from parties who never had a seat in defining the rails.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(digital_money_emergence_boundary__infrastructure_reading, 0.52).
domain_priors:suppression_score(digital_money_emergence_boundary__infrastructure_reading, 0.4).
domain_priors:theater_ratio(digital_money_emergence_boundary__infrastructure_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(digital_money_emergence_boundary__infrastructure_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(digital_money_emergence_boundary__infrastructure_reading, suppression_requirement, 0.4).
narrative_ontology:constraint_metric(digital_money_emergence_boundary__infrastructure_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(digital_money_emergence_boundary__infrastructure_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(digital_money_emergence_boundary__infrastructure_reading, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(digital_money_emergence_boundary__infrastructure_reading, tangled_rope).
narrative_ontology:human_readable(digital_money_emergence_boundary__infrastructure_reading, "Digital Money Emergence Boundary — Infrastructure (Interbank Rail) Reading").
narrative_ontology:topic_domain(digital_money_emergence_boundary__infrastructure_reading, "monetary_economics/financial_history/technology_governance").

domain_priors:requires_active_enforcement(digital_money_emergence_boundary__infrastructure_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(digital_money_emergence_boundary__infrastructure_reading, '3c27ee3e-1886-43ad-b6fc-b97b01e80244').
narrative_ontology:cs_kernel_codification('3c27ee3e-1886-43ad-b6fc-b97b01e80244', distributed).
narrative_ontology:cs_authority_grounding('3c27ee3e-1886-43ad-b6fc-b97b01e80244', practice).
narrative_ontology:cs_reading_relation('3c27ee3e-1886-43ad-b6fc-b97b01e80244', digital_money_emergence_boundary__conceptualization_reading, coexists_with).
narrative_ontology:cs_reading_relation('3c27ee3e-1886-43ad-b6fc-b97b01e80244', digital_money_emergence_boundary__consumer_holdings_reading, influences).
narrative_ontology:cs_axiom('3c27ee3e-1886-43ad-b6fc-b97b01e80244', foundational, electronic_transfer_capability_constitutes_money).
narrative_ontology:cs_axiom_status(electronic_transfer_capability_constitutes_money, holdable).
narrative_ontology:cs_axiom_grounding('3c27ee3e-1886-43ad-b6fc-b97b01e80244', electronic_transfer_capability_constitutes_money, conventional).
narrative_ontology:cs_axiom('3c27ee3e-1886-43ad-b6fc-b97b01e80244', secondary, institutional_capability_precedes_individual_access).
narrative_ontology:cs_axiom_status(institutional_capability_precedes_individual_access, holdable).
narrative_ontology:cs_axiom_grounding('3c27ee3e-1886-43ad-b6fc-b97b01e80244', institutional_capability_precedes_individual_access, empirically_contingent).
narrative_ontology:cs_reference_frame('3c27ee3e-1886-43ad-b6fc-b97b01e80244', paper_and_telex_settlement_baseline).
narrative_ontology:cs_drift_state('3c27ee3e-1886-43ad-b6fc-b97b01e80244', post_swift_maturity_1990s, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('3c27ee3e-1886-43ad-b6fc-b97b01e80244', '').
narrative_ontology:cs_kernel_id(digital_money_emergence_boundary__infrastructure_reading, digital_money_emergence_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(digital_money_emergence_boundary__infrastructure_reading, swift_cooperative).
narrative_ontology:constraint_beneficiary(digital_money_emergence_boundary__infrastructure_reading, ach_network_operators).
narrative_ontology:constraint_beneficiary(digital_money_emergence_boundary__infrastructure_reading, correspondent_banks).
narrative_ontology:constraint_victim(digital_money_emergence_boundary__infrastructure_reading, cross_border_remittance_senders).
narrative_ontology:constraint_victim(digital_money_emergence_boundary__infrastructure_reading, non_member_regional_banks).
narrative_ontology:constraint_victim(digital_money_emergence_boundary__infrastructure_reading, unbanked_and_underbanked_populations).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Operates the messaging standard that lets banks instruct each other to move money electronically. Sets message formats, membership requirements, and fee structures that every participating bank must accept to reach the network. Because SWIFT is the de facto rail for interbank transfer, it defines where the 'money can move electronically' boundary actually sits in practice.
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__infrastructure_reading, swift_cooperative, agenda_setter,
    institutional, generational, arbitrage, global).

% Run the domestic batch-clearing systems (est. 1972) that let commercial banks settle transfers electronically instead of via paper checks. They collect per-transaction and membership fees and set settlement windows and cutoff rules that all participating depository institutions must operate within.
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__infrastructure_reading, ach_network_operators, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(digital_money_emergence_boundary__infrastructure_reading, ach_network_operators, beneficiary).

% Large banks holding accounts for smaller banks gain a structural toll position: they charge correspondent fees and hold float on transfers routed through them because smaller institutions cannot access SWIFT/ACH rails directly without an intermediary.
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__infrastructure_reading, correspondent_banks, beneficiary,
    powerful, generational, constrained, global).

% Too small or too peripheral to hold direct SWIFT or ACH membership, they must route transfers through correspondent banks, paying fees and absorbing settlement delay for access to what the infrastructure reading treats as the moment money became electronically transferable — a moment they experience as gated, not universal.
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__infrastructure_reading, non_member_regional_banks, payer,
    moderate, biographical, constrained, regional).

% Individuals sending money across borders rely on chains of correspondent relationships built on the SWIFT rail; they pay compounding fees and FX spreads at each hop and have no direct access to the underlying infrastructure that defines whether their money 'moved electronically.'
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__infrastructure_reading, cross_border_remittance_senders, payer,
    powerless, immediate, trapped, global).

% Have no bank account and therefore no access point to ACH or SWIFT at all. Under the infrastructure reading, digital money's emergence is entirely about interbank capability — a boundary drawn in a way that structurally cannot include people outside the banking system, though they would object that 'digital money exists' should mean something for them too.
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__infrastructure_reading, unbanked_and_underbanked_populations, excluded,
    powerless, biographical, trapped, global).

% Track when electronic bank-to-bank transfer began blurring the M4/M5 monetary aggregate boundaries, treating the infrastructure milestones (1967 ATMs, 1972 ACH, 1977 SWIFT) as the empirical marker of when 'money' as a measured quantity started including electronically-moved bank liabilities.
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__infrastructure_reading, central_banks_and_bis, observer,
    institutional, civilizational, analytical, global).

% Debate whether the infrastructure-capability boundary, rather than the conceptualization boundary or the consumer-holdings boundary, is the correct place to date digital money's emergence. Their analysis is used to corroborate or dispute the banking-rail claim.
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__infrastructure_reading, monetary_historians, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Standardizing interbank messaging (SWIFT) and domestic batch clearing (ACH) solves a genuine coordination problem: thousands of banks need a common, reliable protocol to instruct and settle transfers without bilateral paper-based reconciliation for every pair.
% TRANSFER_FUNCTION: Moves settlement float, correspondent fees, and membership/access fees from smaller banks, remittance senders, and effectively excluded unbanked populations toward the rail operators (SWIFT, ACH operators) and the large correspondent banks that sit as gatekeepers between the rails and everyone without direct access.
% ABSENT_VOICES: Unbanked and underbanked populations have no seat at all in the infrastructure story — the boundary is drawn entirely in terms of bank-to-bank capability, so their absence from 'digital money' as officially dated is structural rather than an oversight anyone corrects. Cross-border remittance senders are present as payers but have no voice in rail governance.
% DISAPPEARANCE_RATIONALE: If the interbank electronic transfer infrastructure vanished, banks would revert to paper-based check clearing and telex-era manual correspondent messaging; settlement times would balloon from days to weeks, correspondent banking's toll position would evaporate, and the monetary aggregates (M1-M4) would need to be redefined around a much narrower electronic base.
% FOUNDING_PROBLEM: By the late 1960s, growing transaction volumes made paper-based check clearing and manual telex transfer messaging too slow and error-prone to support the scale of interbank and cross-border payment activity; banks needed a faster, standardized, auditable way to move money electronically between institutions.
% FOUNDING_PROBLEM_CORROBORATION: Central banks and the BIS corroborate that the original clearing-speed and reconciliation problem was real and substantially solved by the 1980s. Monetary historians and consumer advocacy analyses (from outside SWIFT/ACH governance) argue the infrastructure now also functions as a toll-collecting layer whose correspondent-fee structure persists well past the point the original speed/reconciliation problem was solved, particularly for cross-border remittance.
narrative_ontology:disappearance_verdict(digital_money_emergence_boundary__infrastructure_reading, world_rearranges).
narrative_ontology:founding_problem_status(digital_money_emergence_boundary__infrastructure_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(digital_money_emergence_boundary__infrastructure_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(digital_money_emergence_boundary__infrastructure_reading, 'none', 1).
narrative_ontology:epsilon_provenance(digital_money_emergence_boundary__infrastructure_reading, 0.52, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness is authored as moderate (0.52 by 2000) because the coordination function (faster, auditable settlement) is real and substantial, but a persistent toll layer (correspondent fees, FX spreads on remittance chains) rides on top of it and has grown as cross-border volume grew. Suppression is moderate (0.40): access to the rails is gated by membership and correspondent relationships rather than by brute coercion, but non-member banks and remittance senders have no practical alternative route. Theater ratio is low-to-moderate (0.22): most of what SWIFT/ACH do is functionally necessary message-passing and clearing, not performance, though some compliance and reporting layers have grown for their own sake over time. Accessibility collapse (0.60) reflects that once SWIFT/ACH became the dominant rail, alternative bilateral arrangements largely disappeared for practical purposes. Resistance (0.35) is moderate: banks occasionally push for alternative rails (e.g. regional payment systems, correspondent banking reform efforts) but resistance has not dislodged the incumbent infrastructure.
 *
 * PERSPECTIVAL GAP:
 *   From the rail operators' seat, SWIFT/ACH read as coordination infrastructure solving a real interbank settlement problem — a rope. From a remittance sender's seat, the same infrastructure reads as a fee-extracting chain of intermediaries with no direct access and no alternative route — closer to a snare experienced through several intermediary layers. The engine's per-seat computation should reflect this divergence structurally, not because either seat is wrong about their own experience.
 *
 * DIRECTIONALITY LOGIC:
 *   SWIFT and ACH operators sit at the pure beneficiary end: they administer the rails, set the rules, and collect fees, with no meaningful alternative rail competing at their scale (d near 0). Correspondent banks are secondary beneficiaries: they occupy a toll position created by the access gap between direct rail membership and everyone else. Non-member regional banks and remittance senders sit toward the target end: they pay fees and absorb delay to access a capability others control outright. Unbanked populations are excluded rather than merely extracted-from — the infrastructure reading's boundary definition structurally cannot include them, which is a different (and arguably worse) status than being a taxed participant.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (slow, error-prone paper/telex settlement) was substantially solved by the 1980s — SWIFT and ACH demonstrably fixed the speed and reconciliation problem they were built for. But correspondent-fee and access-gating structures have persisted and, per the temporal measurements, mildly intensified past the point the original problem was solved, especially in cross-border remittance corridors. This is not classified as fully resolved mandatrophy because the coordination function (interbank settlement infrastructure) remains genuinely live and necessary; the extraction is a layer added onto a still-functioning coordination core, which is exactly the tangled_rope signature rather than a pure legacy snare.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_boundary_choice,
    'Among the three readings of the digital_money_emergence_boundary kernel (conceptualization, infrastructure, consumer_holdings), is the infrastructure boundary the economically meaningful one, or is it an artifact of which historical milestones happen to be well-documented (ATM/ACH/SWIFT dates are institutionally recorded; consumer-side digital cash experiments of the same era are not)?',
    'Comparative monetary-aggregate analysis: track when M1-M4 measurement conventions actually shifted to treat electronically-transferable bank deposits as a distinct category, versus when central banks began citing infrastructure milestones as justification after the fact.',
    'If M4/M5 aggregate conventions shifted concurrently with the 1972/1977 infrastructure dates, the infrastructure reading has strong empirical support as the operative boundary; if aggregate conventions shifted independently (earlier via conceptualization, or later via consumer adoption), the infrastructure reading is better understood as a retrospective institutional narrative privileging the rail operators'' history.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_boundary_choice, conceptual, 'Whether the infrastructure-capability boundary is the economically real emergence point or a documentation artifact favoring bank-rail history.').

omega_variable(
    correspondent_fee_necessity,
    'Is the correspondent banking fee/float structure a necessary cost of coordinating settlement across thousands of non-member institutions, or is it extractive rent enabled by SWIFT/ACH''s membership gatekeeping?',
    'Compare fee structures and settlement times in jurisdictions with broader direct-access rail membership (e.g., real-time gross settlement systems with lower membership barriers) against correspondent-dependent corridors.',
    'If broader-access systems achieve comparable settlement reliability at lower cost, the correspondent toll layer is substantially extractive rent rather than necessary coordination cost, raising the effective extraction figure for non-member banks and remittance senders.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(correspondent_fee_necessity, empirical, 'Whether correspondent banking fees reflect genuine coordination cost or gatekeeping rent.').

omega_variable(
    unbanked_exclusion_as_definitional_artifact,
    'Is the exclusion of unbanked populations from the infrastructure reading''s boundary a neutral consequence of the reading''s scope (it is about bank-to-bank capability, not individual access) or does the choice of this boundary itself perform an exclusion by defining ''digital money''s emergence'' in terms that structurally cannot include the unbanked?',
    'Examine whether alternative readings (consumer_holdings_reading) that center individual access produce materially different beneficiary/victim structures — if they do, the boundary choice is doing normative work, not just descriptive work.',
    'If boundary choice materially redistributes who counts as included, selecting the infrastructure reading as ''the'' emergence date (rather than one of three) has distributive consequences for how monetary history and financial inclusion policy get framed.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(unbanked_exclusion_as_definitional_artifact, conceptual, 'Whether the infrastructure reading''s exclusion of the unbanked is a scope artifact or a normatively loaded framing choice.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(digital_money_emergence_boundary__infrastructure_reading, 1967, 2000).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(digi_tr_t1967, digital_money_emergence_boundary__infrastructure_reading, theater_ratio, 1967, 0.1).
narrative_ontology:measurement_basis(digi_tr_t1967, observed).
narrative_ontology:measurement(digi_tr_t1972, digital_money_emergence_boundary__infrastructure_reading, theater_ratio, 1972, 0.12).
narrative_ontology:measurement_basis(digi_tr_t1972, observed).
narrative_ontology:measurement(digi_tr_t1977, digital_money_emergence_boundary__infrastructure_reading, theater_ratio, 1977, 0.15).
narrative_ontology:measurement_basis(digi_tr_t1977, observed).
narrative_ontology:measurement(digi_tr_t1985, digital_money_emergence_boundary__infrastructure_reading, theater_ratio, 1985, 0.18).
narrative_ontology:measurement_basis(digi_tr_t1985, observed).
narrative_ontology:measurement(digi_tr_t1993, digital_money_emergence_boundary__infrastructure_reading, theater_ratio, 1993, 0.2).
narrative_ontology:measurement_basis(digi_tr_t1993, observed).
narrative_ontology:measurement(digi_tr_t2000, digital_money_emergence_boundary__infrastructure_reading, theater_ratio, 2000, 0.22).
narrative_ontology:measurement_basis(digi_tr_t2000, observed).

% Extraction over time
narrative_ontology:measurement(digi_be_t1967, digital_money_emergence_boundary__infrastructure_reading, base_extractiveness, 1967, 0.28).
narrative_ontology:measurement_basis(digi_be_t1967, observed).
narrative_ontology:measurement(digi_be_t1972, digital_money_emergence_boundary__infrastructure_reading, base_extractiveness, 1972, 0.33).
narrative_ontology:measurement_basis(digi_be_t1972, observed).
narrative_ontology:measurement(digi_be_t1977, digital_money_emergence_boundary__infrastructure_reading, base_extractiveness, 1977, 0.4).
narrative_ontology:measurement_basis(digi_be_t1977, observed).
narrative_ontology:measurement(digi_be_t1985, digital_money_emergence_boundary__infrastructure_reading, base_extractiveness, 1985, 0.46).
narrative_ontology:measurement_basis(digi_be_t1985, observed).
narrative_ontology:measurement(digi_be_t1993, digital_money_emergence_boundary__infrastructure_reading, base_extractiveness, 1993, 0.49).
narrative_ontology:measurement_basis(digi_be_t1993, observed).
narrative_ontology:measurement(digi_be_t2000, digital_money_emergence_boundary__infrastructure_reading, base_extractiveness, 2000, 0.52).
narrative_ontology:measurement_basis(digi_be_t2000, observed).

% Suppression requirement over time
narrative_ontology:measurement(digi_su_t1967, digital_money_emergence_boundary__infrastructure_reading, suppression_requirement, 1967, 0.2).
narrative_ontology:measurement_basis(digi_su_t1967, observed).
narrative_ontology:measurement(digi_su_t1972, digital_money_emergence_boundary__infrastructure_reading, suppression_requirement, 1972, 0.26).
narrative_ontology:measurement_basis(digi_su_t1972, observed).
narrative_ontology:measurement(digi_su_t1977, digital_money_emergence_boundary__infrastructure_reading, suppression_requirement, 1977, 0.32).
narrative_ontology:measurement_basis(digi_su_t1977, observed).
narrative_ontology:measurement(digi_su_t1985, digital_money_emergence_boundary__infrastructure_reading, suppression_requirement, 1985, 0.35).
narrative_ontology:measurement_basis(digi_su_t1985, observed).
narrative_ontology:measurement(digi_su_t1993, digital_money_emergence_boundary__infrastructure_reading, suppression_requirement, 1993, 0.38).
narrative_ontology:measurement_basis(digi_su_t1993, observed).
narrative_ontology:measurement(digi_su_t2000, digital_money_emergence_boundary__infrastructure_reading, suppression_requirement, 2000, 0.4).
narrative_ontology:measurement_basis(digi_su_t2000, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(digital_money_emergence_boundary__infrastructure_reading, global_infrastructure).
narrative_ontology:boltzmann_floor_override(digital_money_emergence_boundary__infrastructure_reading, 0.2).
narrative_ontology:affects_constraint(digital_money_emergence_boundary__infrastructure_reading, conceptualization_reading).
narrative_ontology:affects_constraint(digital_money_emergence_boundary__infrastructure_reading, consumer_holdings_reading).

% DUAL FORMULATION NOTE:
% This story is the infrastructure_reading member of the digital_money_emergence_boundary kernel family (3 readings: conceptualization_reading, infrastructure_reading, consumer_holdings_reading). Each reading has its own epsilon and beneficiary structure and is authored as a separate constraint file per the ε-invariance principle; they are linked here for contamination/network analysis, not averaged. The conceptualization_reading is upstream in time (theoretical possibility precedes institutional build-out) and the consumer_holdings_reading is downstream (individual access follows institutional rail capability), so influence plausibly flows infrastructure_reading -> consumer_holdings_reading as well as conceptualization_reading -> infrastructure_reading, though this file only asserts the edges, not a causal ordering claim beyond structural influence.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
