% ============================================================================
% CONSTRAINT STORY: digital_money_emergence_boundary__infrastructure_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
 *   human_readable: Digital Money Emergence via Infrastructure Boundary (Infrastructure Reading)
 *   domain: monetary_economics/financial_history/technology_governance
 *
 * SUMMARY:
 *   This constraint embodies one reading of when digital money emerged: the
 *   infrastructure-boundaries reading. It asserts that digital money came
 *   into existence when technical infrastructure made bank-to-bank electronic
 *   transfer possible (1967 ATMs, 1972 ACH, 1977 SWIFT), even though
 *   consumers could not directly hold digital instruments until much later.
 *   This reading vindicates banking infrastructure operators (who control the
 *   definition through technical gatekeeping) and excludes alternative money
 *   forms that cannot access the rails. The constraint is CLAIMED as
 *   tangled_rope because it coordinates inter-bank settlement (genuine
 *   coordination problem) while simultaneously extracting definitional
 *   authority from other actors and transferring it to infrastructure
 *   operators. The measurement series tracks how the constraint's
 *   extractiveness, suppression, and theater ratio intensified over the
 *   thirty-year interval as the infrastructure became the de facto monetary
 *   definition standard.
 *
 * KEY AGENTS:
 *   - Banking infrastructure operators (ACH, SWIFT): institutional agenda-setters; control technical standards and fee structure; define what counts as 'digital money' by controlling which transfers are settled.
 *   - Incumbent commercial banks: institutional beneficiaries and payers; gain exclusive access to electronic transfer; pay fees but capture monopoly rents on deposit definition.
 *   - Non-bank payment innovators: moderate-power excluded actors; develop alternative payment systems that cannot access the rails; their innovations are deemed 'not money' by definition.
 *   - Consumer-choice payment architects: powerless and identity-locked excluded actors; envision consumer agency in digital money; locked out of boundary-setting by infrastructure gatekeeping.
 *   - Central banks: institutional observers; could redefine 'money' independent of infrastructure but ratify the private operators' boundary instead.
 *   - Government payment systems: institutional observers; operate separately but accept private infrastructure as the canonical definition.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(digital_money_emergence_boundary__infrastructure_reading, 0.68).
domain_priors:suppression_score(digital_money_emergence_boundary__infrastructure_reading, 0.72).
domain_priors:theater_ratio(digital_money_emergence_boundary__infrastructure_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(digital_money_emergence_boundary__infrastructure_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(digital_money_emergence_boundary__infrastructure_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(digital_money_emergence_boundary__infrastructure_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(digital_money_emergence_boundary__infrastructure_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(digital_money_emergence_boundary__infrastructure_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(digital_money_emergence_boundary__infrastructure_reading, tangled_rope).
narrative_ontology:human_readable(digital_money_emergence_boundary__infrastructure_reading, "Digital Money Emergence via Infrastructure Boundary (Infrastructure Reading)").
narrative_ontology:topic_domain(digital_money_emergence_boundary__infrastructure_reading, "monetary_economics/financial_history/technology_governance").

domain_priors:requires_active_enforcement(digital_money_emergence_boundary__infrastructure_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(digital_money_emergence_boundary__infrastructure_reading, '2dd8f07b-2ae9-4f83-ba3a-6823998f62f6').
narrative_ontology:cs_kernel_codification('2dd8f07b-2ae9-4f83-ba3a-6823998f62f6', distributed).
narrative_ontology:cs_authority_grounding('2dd8f07b-2ae9-4f83-ba3a-6823998f62f6', extraction).
narrative_ontology:cs_reading_relation('2dd8f07b-2ae9-4f83-ba3a-6823998f62f6', digital_money_emergence_boundary__conceptualization_reading, coexists_with).
narrative_ontology:cs_reading_relation('2dd8f07b-2ae9-4f83-ba3a-6823998f62f6', digital_money_emergence_boundary__consumer_holdings_reading, coexists_with).
narrative_ontology:cs_axiom('2dd8f07b-2ae9-4f83-ba3a-6823998f62f6', foundational, infrastructure_as_definition_authority).
narrative_ontology:cs_axiom_status(infrastructure_as_definition_authority, holdable).
narrative_ontology:cs_axiom_grounding('2dd8f07b-2ae9-4f83-ba3a-6823998f62f6', infrastructure_as_definition_authority, instrumental).
narrative_ontology:cs_axiom('2dd8f07b-2ae9-4f83-ba3a-6823998f62f6', secondary, money_is_what_settles_on_the_rails).
narrative_ontology:cs_axiom_status(money_is_what_settles_on_the_rails, holdable).
narrative_ontology:cs_axiom_grounding('2dd8f07b-2ae9-4f83-ba3a-6823998f62f6', money_is_what_settles_on_the_rails, conventional).
narrative_ontology:cs_reference_frame('2dd8f07b-2ae9-4f83-ba3a-6823998f62f6', bank_to_bank_settlement_standard).
narrative_ontology:cs_drift_state('2dd8f07b-2ae9-4f83-ba3a-6823998f62f6', contemporary_post_1997, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('2dd8f07b-2ae9-4f83-ba3a-6823998f62f6', '').
narrative_ontology:cs_kernel_id(digital_money_emergence_boundary__infrastructure_reading, digital_money_emergence_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(digital_money_emergence_boundary__infrastructure_reading, banking_infrastructure_operators).
narrative_ontology:constraint_beneficiary(digital_money_emergence_boundary__infrastructure_reading, incumbent_commercial_banks).
narrative_ontology:constraint_victim(digital_money_emergence_boundary__infrastructure_reading, non_bank_payment_innovators).
narrative_ontology:constraint_victim(digital_money_emergence_boundary__infrastructure_reading, consumer_payment_choice_architects).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(digital_money_emergence_boundary__infrastructure_reading, incumbent_commercial_banks).
narrative_ontology:constraint_vindicates(digital_money_emergence_boundary__infrastructure_reading, banking_system_technological_determinism).
narrative_ontology:constraint_vindicates(digital_money_emergence_boundary__infrastructure_reading, infrastructure_as_definition_authority).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Design, build, and operate the clearing and settlement rails (ACH, SWIFT, CHIPS) that move bank-to-bank funds electronically. Set technical standards for what constitutes 'electronic money' and which transfers qualify as settled. Collect fees on every transaction moving through their infrastructure. Define the boundary between money (what their systems handle) and non-money (what they exclude). Their decisions about protocol, compatibility, and participation rules become the de facto definition of digital money in the system.
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__infrastructure_reading, banking_infrastructure_operators, agenda_setter,
    institutional, generational, arbitrage, global).

% Gain exclusive access to electronic transfer infrastructure and the ability to define what constitutes 'money' within their balance sheets. They pay infrastructure fees but gain monopoly rents on deposit definition and settlement. Their customer deposit liabilities become the canonical form of 'digital money' once electronic transfer is possible, foreclosing alternative money forms that cannot access the rails.
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__infrastructure_reading, incumbent_commercial_banks, beneficiary,
    institutional, generational, mobile, global).
narrative_ontology:stakeholder_secondary_role(digital_money_emergence_boundary__infrastructure_reading, incumbent_commercial_banks, payer).

% Observe the infrastructure boundary being drawn by private operators and ratify it through regulatory acceptance. Define 'money supply' by reference to what moves through the rails their member banks control. Cede the boundary-drawing authority to technical infrastructure providers rather than maintaining it as a monetary policy instrument. Could intervene to redefine what counts as money, but instead accept the infrastructure operators' definition.
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__infrastructure_reading, central_banks, observer,
    institutional, generational, analytical, national).

% Develop alternative electronic payment systems (early e-cash schemes, digital settlement networks, community currencies) that could constitute 'digital money' by function. Cannot access the ACH/SWIFT infrastructure without becoming a bank-like entity and surrendering operational independence. Excluded by the infrastructure operators' technical and regulatory gatekeeping. Their innovations are deemed 'not money' because they operate outside the defined rails, creating a circular definition: money is what the infrastructure handles; what the infrastructure excludes cannot be money.
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__infrastructure_reading, non_bank_payment_innovators, excluded,
    moderate, biographical, constrained, global).

% Researchers, regulators, and technologists who envision consumer agency in choosing payment instruments and owning digital value directly. During the infrastructure-boundary period (1967-1990s), they are excluded from the definition-setting conversation. The infrastructure reading locks them out: 'digital money' is defined as what moves through bank-operated rails, not what consumers can hold or direct. Their preferred boundary (consumer-empowered digital holdings) is pre-closed by the infrastructure operators' definitional gatekeeping.
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__infrastructure_reading, consumer_payment_choice_architects, payer,
    powerless, biographical, identity_locked, global).

% Operate separately from the private bank infrastructure (treasury systems, government settlement networks). Observe that 'digital money' is defined by private operators' infrastructure choices rather than by sovereign monetary authority. Have technical capacity to define alternative boundaries but delegate to private infrastructure operators for efficiency, losing control of the definition in the process.
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__infrastructure_reading, government_payment_systems, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(digital_money_emergence_boundary__infrastructure_reading, banking_infrastructure_operators).
narrative_ontology:fixing_cost_class(digital_money_emergence_boundary__infrastructure_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Enables inter-bank settlement and clearing at electronic speed: eliminates the physical movement of paper checks; reduces clearing time from days to hours; creates a common protocol for transfer so any bank can move funds to any other bank without bilateral negotiation. Before the infrastructure, inter-bank settlement was slow, expensive, and required courier networks; the electronic rails made it cost-efficient at scale.
% TRANSFER_FUNCTION: Moves value (and the definition of what counts as value) from payment innovators to banking infrastructure operators. Non-bank actors who might have created alternative money forms cannot access the rails; infrastructure operators collect fees on the coordination function and control which transfers are 'settled' and which are not. Banks gain the authority to define deposits as the canonical form of digital money.
% ABSENT_VOICES: Non-bank payment technologists, consumer-sovereignty advocates, alternative-currency designers, and central banks acting as independent monetary authorities are excluded from the boundary-setting conversation. They would argue for open settlement infrastructure, plural payment forms, and consumer agency in money definition; the infrastructure operators' gatekeeping keeps them structurally absent from the technical standards committees that decide what 'digital money' means.
% DISAPPEARANCE_RATIONALE: If this infrastructure boundary disappeared overnight and the power to define 'digital money' reverted to central banks or became open-source and decentralized, alternative money forms would proliferate within months — e-cash schemes, community currencies, non-bank digital settlement networks would all become technically and definitionally possible. The financial system would reorganize around multiple competing payment standards rather than the single infrastructure-gated definition.
% FOUNDING_PROBLEM: Inter-bank settlement was slow and labor-intensive: checks moved by courier, clearing took days, costs were high for moving large sums quickly, coordination was bilateral and bilateral-negotiated. Banks needed a faster, cheaper, standardized way to settle with each other at scale.
% FOUNDING_PROBLEM_CORROBORATION: Banking historians and infrastructure engineers attest the founding problem was real: pre-electronic settlement was demonstrably slow. Central banks and payment system operators attest the founding problem persists (although less acutely) — efficiency remains a driver of infrastructure investment. Payment innovators outside banking contest whether the founding problem justifies the infrastructure operators' monopoly on boundary-setting; they argue the problem was 'fast inter-bank settlement' (solved) not 'only banks can define money' (a separate assertion the operators layered on top).
narrative_ontology:disappearance_verdict(digital_money_emergence_boundary__infrastructure_reading, world_rearranges).
narrative_ontology:founding_problem_status(digital_money_emergence_boundary__infrastructure_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(digital_money_emergence_boundary__infrastructure_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-04',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(digital_money_emergence_boundary__infrastructure_reading, 'none', 1).
narrative_ontology:epsilon_provenance(digital_money_emergence_boundary__infrastructure_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness rises from 0.32 to 0.68 over the interval because the coordination problem (fast inter-bank settlement) is solved early (~1972 with ACH), but the infrastructure operators maintain and extend their definitional gatekeeping beyond the original problem. By 1997, the constraint persists almost entirely on the basis of excluding alternative money forms, not on solving settlement speed. Suppression tracks upward (0.38 to 0.72) because the infrastructure operators actively enforce the boundary: they deny non-bank actors access to the rails, exclude alternative settlement standards from regulatory recognition, and define 'money supply' in ways that erase non-infrastructure-based payment forms. Theater rises from 0.18 to 0.42: early infrastructure work is genuinely functional (clearing needed to happen faster), but by 1997, a growing share of the infrastructure operators' work is ceremonial boundary maintenance—publishing standards that exclude non-banks, lobbying for regulatory definitions that equate money with what moves through their rails, maintaining interoperability barriers that appear technical but serve definitional gatekeeping. The suppression_requirement series models the active enforcement cost: as alternative payment technologies proliferate (e-cash, digital currencies, community networks), the infrastructure operators must work harder (build legal barriers, exclude from settlement, monopolize regulatory definition) to maintain the boundary. This is genuine suppression, not mere indifference.
 *
 * PERSPECTIVAL GAP:
 *   The infrastructure operators' seat sees this as rope (a genuine coordination problem they solved and now maintain). The beneficiary banks' seat sees coordination benefit offset by paid-in fees. The innovator and consumer-choice seats see this as snare (excluded from a definition-setting process that was never their to begin with, locked out by technical and regulatory barriers). The central bank seat sees the boundary as technically necessary but increasingly extractive. The engine computes these divergences from the structural data: the infrastructure operators have arbitrage-level exit (they can redefine standards at will), while non-bank innovators have constrained or trapped exit (they cannot access the rails without becoming banks). This directionality asymmetry alone produces the extraction reading from the excluded seats' perspective.
 *
 * DIRECTIONALITY LOGIC:
 *   Infrastructure operators (d ≈ 0.05 to 0.15): full beneficiaries. They collect fees, control standards, face no real exit pressure because the entire financial system is locked into their rails. The constraint subsidizes them. Incumbent banks (d ≈ 0.25 to 0.40): beneficiaries with paid-in costs. They benefit from exclusive access to the definition-setting process but pay infrastructure fees. Exit is mobile—they could theoretically build alternative infrastructure, but coordination costs make that infeasible, creating a quasi-trapped position. Non-bank innovators (d ≈ 0.75 to 0.88): targets. They are excluded by design; their alternative payment forms are definitionally ruled out; their only 'exit' is to become banks, which means abandoning their innovation model. This is near-total directionality toward extraction. Consumer-choice architects (d ≈ 0.82 to 0.92): identity-locked targets. Their entire professional identity is bound up in the belief that consumers should control digital value, but the infrastructure reading forecloses that possibility by definition. Exiting means abandoning their field.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's founding problem (slow inter-bank settlement) was structurally real in 1967 and was solved by ACH and SWIFT by 1977. By 1987, settlement speed was no longer the active coordination problem—the infrastructure existed and worked. Yet the constraint persists and intensifies, with suppression and theater both rising. This is mandatrophy: the original mandate (solve settlement) outlived its function, but the infrastructure operators extended the constraint into definitional gatekeeping, which is a different coordination problem (controlling what counts as money). The measurement series captures this: extractiveness and suppression rise AFTER the founding problem is solved. Calling this 'tangled_rope' (not 'piton') reflects that the infrastructure operators genuinely maintain the rails and justify them by the now-attenuated settlement-speed coordination; the theater ratio (0.42 by 1997) is high enough to flag the performative element but not so high as to suggest pure inertia. The constraint walks the line: it is a rope that has become extractive, not yet a piton that is purely theatrical, but trending toward piton status as the founding problem fades and the definitional gatekeeping becomes the constraint's real function.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_authority_contest,
    'Who has authority to define ''digital money''—infrastructure operators (by technical gatekeeping), central banks (by monetary policy), economists (by conceptual modeling), or consumers (by functional use)?',
    'Regulatory intervention or legislative definition: a central bank or government explicitly asserting a definition independent of infrastructure operators'' technical choices, or accepting one of the sibling readings as canonical.',
    'If infrastructure operators retain authority, this reading''s extraction classification holds and the constraint remains tangled_rope with rising mandatrophy. If central banks or consumers gain authority, the ε boundary shifts to one of the sibling readings, and this reading becomes historically obsolete (a reading of past definitions rather than current ones).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_authority_contest, conceptual, 'Whether technical infrastructure gatekeeping can legitimately define monetary categories.').

omega_variable(
    infrastructure_necessity_vs_control,
    'Is the suppression and gatekeeping (the high suppression and theater values) necessary to operate the infrastructure, or is it unnecessary control layered on top of coordination?',
    'Natural experiment from open-access or decentralized settlement infrastructure (Bitcoin blockchain, distributed ledgers, open banking mandates): if settlement speed and reliability are maintained WITHOUT exclusionary gatekeeping, the suppression is shown to be control, not necessity.',
    'If suppression is necessary, the constraint''s classification might drop to ''rope'' (high coordination function, suppression is coordination cost, not extraction). If suppression is unnecessary control, the constraint is confirmed as ''snare'' from the excluded seats'' perspective, and this ''tangled_rope'' claim is shown to be beneficiary-biased theater.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(infrastructure_necessity_vs_control, empirical, 'Whether infrastructure-operator gatekeeping is functionally necessary or extractive control.').

omega_variable(
    measurement_grid_cyclicality,
    'Do the extractiveness and suppression measurements reflect a cyclical pattern (pressure → innovation → pressure escalation) or a monotonic drift?',
    'Fine-grained historical analysis of regulatory cycles: moments when innovators challenge the boundary (1985 Chaum, 1990s e-cash, 2008 Bitcoin) and infrastructure operators respond with regulatory tightening or standard-setting.',
    'If cyclical, the oscillation itself may be the extraction mechanism (intermittent reinforcement of the boundary). If monotonic, the constraint is a straightforward rent-seeking intensification over the interval.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(measurement_grid_cyclicality, empirical, 'Whether the constraint''s enforcement follows a cycle or steady drift.').

omega_variable(
    alternative_reading_mutual_exclusivity,
    'Are the three sibling readings truly mutually exclusive (different kernels), or do they describe phases of the same emergence process (conceptual thinking → infrastructure building → consumer adoption)?',
    'Philosophical and historical analysis: can a party hold all three readings simultaneously, or does adopting one necessarily foreclose the others?',
    'If mutually exclusive, this omega dissolves and the three constraints remain independent. If they are phases rather than alternatives, they form a constraint family with a different network structure (sequential dependencies rather than lateral coexistence), and the mandatrophy analysis shifts from this reading to the family as a whole.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(alternative_reading_mutual_exclusivity, conceptual, 'Whether the sibling readings are alternative readings or sequential stages of emergence.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(digital_money_emergence_boundary__infrastructure_reading, 1967, 1997).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(digi_tr_t1967, digital_money_emergence_boundary__infrastructure_reading, theater_ratio, 1967, 0.18).
narrative_ontology:measurement_basis(digi_tr_t1967, observed).
narrative_ontology:measurement(digi_tr_t1977, digital_money_emergence_boundary__infrastructure_reading, theater_ratio, 1977, 0.28).
narrative_ontology:measurement_basis(digi_tr_t1977, observed).
narrative_ontology:measurement(digi_tr_t1987, digital_money_emergence_boundary__infrastructure_reading, theater_ratio, 1987, 0.38).
narrative_ontology:measurement_basis(digi_tr_t1987, observed).
narrative_ontology:measurement(digi_tr_t1997, digital_money_emergence_boundary__infrastructure_reading, theater_ratio, 1997, 0.42).
narrative_ontology:measurement_basis(digi_tr_t1997, observed).

% Extraction over time
narrative_ontology:measurement(digi_be_t1967, digital_money_emergence_boundary__infrastructure_reading, base_extractiveness, 1967, 0.32).
narrative_ontology:measurement_basis(digi_be_t1967, observed).
narrative_ontology:measurement(digi_be_t1977, digital_money_emergence_boundary__infrastructure_reading, base_extractiveness, 1977, 0.48).
narrative_ontology:measurement_basis(digi_be_t1977, observed).
narrative_ontology:measurement(digi_be_t1987, digital_money_emergence_boundary__infrastructure_reading, base_extractiveness, 1987, 0.62).
narrative_ontology:measurement_basis(digi_be_t1987, observed).
narrative_ontology:measurement(digi_be_t1997, digital_money_emergence_boundary__infrastructure_reading, base_extractiveness, 1997, 0.68).
narrative_ontology:measurement_basis(digi_be_t1997, observed).

% Suppression requirement over time
narrative_ontology:measurement(digi_su_t1967, digital_money_emergence_boundary__infrastructure_reading, suppression_requirement, 1967, 0.38).
narrative_ontology:measurement_basis(digi_su_t1967, observed).
narrative_ontology:measurement(digi_su_t1977, digital_money_emergence_boundary__infrastructure_reading, suppression_requirement, 1977, 0.55).
narrative_ontology:measurement_basis(digi_su_t1977, observed).
narrative_ontology:measurement(digi_su_t1987, digital_money_emergence_boundary__infrastructure_reading, suppression_requirement, 1987, 0.67).
narrative_ontology:measurement_basis(digi_su_t1987, observed).
narrative_ontology:measurement(digi_su_t1997, digital_money_emergence_boundary__infrastructure_reading, suppression_requirement, 1997, 0.72).
narrative_ontology:measurement_basis(digi_su_t1997, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(digital_money_emergence_boundary__infrastructure_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(digital_money_emergence_boundary__infrastructure_reading, 0.18).
narrative_ontology:affects_constraint(digital_money_emergence_boundary__infrastructure_reading, digital_money_emergence_boundary__conceptualization_reading).
narrative_ontology:affects_constraint(digital_money_emergence_boundary__infrastructure_reading, digital_money_emergence_boundary__consumer_holdings_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the kernel digital_money_emergence_boundary. The three sibling readings (infrastructure_reading, conceptualization_reading, consumer_holdings_reading) decompose the single unresolved question 'when did digital money emerge?' into three structurally distinct constraints with different beneficiaries, victims, and ε values. The infrastructure_reading (this constraint) vindicates technical infrastructure operators; the conceptualization_reading vindicates economists and formal theorists; the consumer_holdings_reading vindicates consumer agency. Each reading asserts a different boundary and a different definition of what counts as 'digital money.' They coexist as live alternative positions held by different institutional actors (banks, economists, regulators, consumer advocates) but neither forecloses the others—all three readings remain simultaneously defended in public discourse.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(digital_money_emergence_boundary__infrastructure_reading, powerless, 0.87).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
