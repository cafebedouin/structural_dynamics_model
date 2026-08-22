% ============================================================================
% CONSTRAINT STORY: digital_money_emergence_boundary__infrastructure_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-04
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
 *   human_readable: Digital Money Emergence — Infrastructure Boundary (1967-1977)
 *   domain: monetary_economics/financial_history/technology_governance
 *
 * SUMMARY:
 *   This constraint story captures the 'infrastructure reading' of the
 *   contested kernel 'digital_money_emergence_boundary'. The reading places
 *   the emergence of digital money at the moment banking infrastructure
 *   enabled electronic transfer — 1967 (ATMs), 1972 (ACH), 1977 (SWIFT) —
 *   arguing that money exists when banks can move it electronically between
 *   themselves, even if consumers cannot yet hold digital instruments
 *   directly. The constraint is the standing arrangement of interbank
 *   electronic settlement infrastructure that emerged in this period.
 *   Beneficiaries are the infrastructure operators (SWIFT, ACH operators, ATM
 *   network operators, correspondent banks) who control the rails. The
 *   constraint functions as coordination infrastructure solving settlement
 *   finality, but carries nascent extraction potential as operators gain
 *   pricing power over payment rails. This reading coexists with the
 *   conceptualization reading (theoretical thinkability, 1960s/1985) and the
 *   consumer holdings reading (consumer accessibility, 1990s/2000) — three
 *   structurally distinct boundaries that produce different ε values and
 *   beneficiary structures.
 *
 * KEY AGENTS:
 *   - swift_operator: Primary beneficiary (institutional/arbitrage) — operates the messaging/settlement rail, collects fees
 *   - ach_operators: Primary beneficiary (institutional/arbitrage) — operates domestic batch settlement, collects per-transaction fees
 *   - atm_network_operators: Primary beneficiary (institutional/arbitrage) — operates shared cash-access infrastructure, collects interchange
 *   - correspondent_banks: Primary beneficiary (powerful/arbitrage) — provide cross-border settlement access, earn correspondent fees
 *   - commercial_banks: Payer/beneficiary dual (powerful/constrained) — pay infrastructure fees but gain settlement efficiency
 *   - central_banks: Agenda setter (institutional/analytical) — oversee settlement systems, set access rules, mandate participation
 *   - consumers: Excluded at this boundary (moderate/trapped) — cannot directly access electronic money; effects transmit via bank pricing
 *   - monetary_economists: Observer (analytical/analytical) — classify monetary aggregates, debate boundary placement
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(digital_money_emergence_boundary__infrastructure_reading, 0.18).
domain_priors:suppression_score(digital_money_emergence_boundary__infrastructure_reading, 0.22).
domain_priors:theater_ratio(digital_money_emergence_boundary__infrastructure_reading, 0.12).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(digital_money_emergence_boundary__infrastructure_reading, extractiveness, 0.18).
narrative_ontology:constraint_metric(digital_money_emergence_boundary__infrastructure_reading, suppression_requirement, 0.22).
narrative_ontology:constraint_metric(digital_money_emergence_boundary__infrastructure_reading, theater_ratio, 0.12).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(digital_money_emergence_boundary__infrastructure_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(digital_money_emergence_boundary__infrastructure_reading, resistance, 0.28).

% --- Constraint claim ---
narrative_ontology:constraint_claim(digital_money_emergence_boundary__infrastructure_reading, rope).
narrative_ontology:human_readable(digital_money_emergence_boundary__infrastructure_reading, "Digital Money Emergence — Infrastructure Boundary (1967-1977)").
narrative_ontology:topic_domain(digital_money_emergence_boundary__infrastructure_reading, "monetary_economics/financial_history/technology_governance").

domain_priors:requires_active_enforcement(digital_money_emergence_boundary__infrastructure_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(digital_money_emergence_boundary__infrastructure_reading, '55768ba6-bd4d-46c1-acdf-51881f523345').
narrative_ontology:cs_kernel_codification('55768ba6-bd4d-46c1-acdf-51881f523345', formalized).
narrative_ontology:cs_authority_grounding('55768ba6-bd4d-46c1-acdf-51881f523345', lineage).
narrative_ontology:cs_interpretation_layer_present('55768ba6-bd4d-46c1-acdf-51881f523345').
narrative_ontology:cs_reading_relation('55768ba6-bd4d-46c1-acdf-51881f523345', digital_money_emergence_boundary__conceptualization_reading, coexists_with).
narrative_ontology:cs_reading_relation('55768ba6-bd4d-46c1-acdf-51881f523345', digital_money_emergence_boundary__consumer_holdings_reading, influences).
narrative_ontology:cs_axiom('55768ba6-bd4d-46c1-acdf-51881f523345', foundational, money_exists_when_banks_can_move_it_electronically).
narrative_ontology:cs_axiom_status(money_exists_when_banks_can_move_it_electronically, holdable).
narrative_ontology:cs_axiom_grounding('55768ba6-bd4d-46c1-acdf-51881f523345', money_exists_when_banks_can_move_it_electronically, conventional).
narrative_ontology:cs_axiom('55768ba6-bd4d-46c1-acdf-51881f523345', foundational, interbank_settlement_infrastructure_is_necessary_condition_for_digital_money).
narrative_ontology:cs_axiom_status(interbank_settlement_infrastructure_is_necessary_condition_for_digital_money, holdable).
narrative_ontology:cs_axiom_grounding('55768ba6-bd4d-46c1-acdf-51881f523345', interbank_settlement_infrastructure_is_necessary_condition_for_digital_money, empirically_contingent).
narrative_ontology:cs_reference_frame('55768ba6-bd4d-46c1-acdf-51881f523345', interbank_electronic_settlement_emergence_1967_1977).
narrative_ontology:cs_drift_state('55768ba6-bd4d-46c1-acdf-51881f523345', contemporary_instant_payment_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('55768ba6-bd4d-46c1-acdf-51881f523345', '2026-08-04T14:22:00Z').
narrative_ontology:cs_kernel_id(digital_money_emergence_boundary__infrastructure_reading, digital_money_emergence_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(digital_money_emergence_boundary__infrastructure_reading, swift_operator).
narrative_ontology:constraint_beneficiary(digital_money_emergence_boundary__infrastructure_reading, ach_operators).
narrative_ontology:constraint_beneficiary(digital_money_emergence_boundary__infrastructure_reading, atm_network_operators).
narrative_ontology:constraint_beneficiary(digital_money_emergence_boundary__infrastructure_reading, correspondent_banks).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(digital_money_emergence_boundary__infrastructure_reading, commercial_banks).
narrative_ontology:constraint_victim(digital_money_emergence_boundary__infrastructure_reading, commercial_banks).
narrative_ontology:constraint_vindicates(digital_money_emergence_boundary__infrastructure_reading, monetary_settlement_finality_requires_centralized_infrastructure).
narrative_ontology:constraint_vindicates(digital_money_emergence_boundary__infrastructure_reading, electronic_bank_transfers_are_functionally_equivalent_to_cash_for_interbank_settlement).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Operates the SWIFT messaging network (founded 1977) that standardizes cross-border payment instructions. Collects per-message fees from member banks. Has arbitrage-grade exit: the technology is licensable, the network could be sold, and alternative messaging standards exist (though none have achieved comparable adoption). The constraint's persistence directly funds SWIFT's operations and development.
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__infrastructure_reading, swift_operator, beneficiary,
    institutional, generational, arbitrage, global).

% Operate Automated Clearing House networks for domestic batch settlement (US ACH from 1972, other national systems similar). Collect per-transaction fees from participating financial institutions. Have arbitrage-grade exit: ACH operators are typically owned by member banks or central banks and could restructure fee models; alternative settlement rails (RTGS, instant payments) provide competitive pressure.
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__infrastructure_reading, ach_operators, beneficiary,
    institutional, generational, arbitrage, national).

% Operate shared ATM networks (from 1967) enabling cross-bank cash access. Collect interchange fees from card-issuing banks. Have arbitrage-grade exit: networks can be sold, merged, or restructured; card schemes (Visa/Mastercard) provide alternative rails. The 1967 ATM deployment marks the earliest infrastructure boundary in this reading.
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__infrastructure_reading, atm_network_operators, beneficiary,
    institutional, generational, arbitrage, national).

% Provide cross-border settlement services to smaller banks lacking direct SWIFT/central bank access. Earn correspondent fees and hold nostro/vostro balances. Have arbitrage-grade exit: can choose which corridors to serve, can shift to direct SWIFT membership or RTGS access where available. Benefit from the infrastructure constraint by controlling access chokepoints.
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__infrastructure_reading, correspondent_banks, beneficiary,
    powerful, generational, arbitrage, global).

% Must connect to SWIFT, ACH, and ATM networks to offer competitive payment services. Pay infrastructure fees (SWIFT per-message, ACH per-transaction, ATM interchange) but gain settlement efficiency, risk reduction, and customer retention. Exit is constrained: leaving a network means losing reach; multi-homing is costly. Over time, fees have risen above marginal cost as network effects hardened — the payer role strengthens relative to beneficiary role.
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__infrastructure_reading, commercial_banks, payer,
    powerful, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(digital_money_emergence_boundary__infrastructure_reading, commercial_banks, beneficiary).

% Oversee payment system stability, set access criteria for settlement infrastructure, mandate participation in systemically important systems (e.g., Fedwire, TARGET2). Do not pay infrastructure fees directly but bear supervisory costs. Have analytical exit: they observe and regulate the constraint but are not subject to its extraction. Their policy choices (e.g., mandating open access, licensing instant payment rails) shape the constraint's evolution.
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__infrastructure_reading, central_banks, agenda_setter,
    institutional, generational, analytical, national).

% Cannot directly access interbank electronic transfer infrastructure at this boundary (1967-1977). Experience the constraint only indirectly through bank pricing (account fees, transfer costs, ATM surcharges). Have no voice in infrastructure governance. Their trapped position at this boundary is why the consumer_holdings_reading places the emergence boundary later — when consumers gain direct access (e-purses, EMD, later CBDC/stablecoins).
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__infrastructure_reading, consumers, excluded,
    moderate, biographical, trapped, national).

% Classify monetary aggregates (M1-M5), debate where 'digital money' begins, advise central banks on measurement. Their boundary placement (infrastructure vs. conceptualization vs. consumer holdings) shapes regulatory treatment and academic consensus. They neither pay nor collect from the infrastructure constraint; they analyze it.
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__infrastructure_reading, monetary_economists, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves interbank settlement finality and efficiency: replaces physical cash transport and bilateral correspondent chains with electronic messaging and net settlement, enabling high-volume, low-risk, near-instant bank-to-bank transfers.
% TRANSFER_FUNCTION: Moves per-transaction/message fees from commercial banks (and ultimately their customers) to infrastructure operators (SWIFT, ACH operators, ATM networks, correspondent banks) as the price of accessing the settlement rail.
% ABSENT_VOICES: Consumers and non-bank payment innovators are structurally excluded at this boundary. Consumers would argue for lower retail fees and direct access; non-bank innovators (fintechs, e-money issuers) would argue for direct infrastructure access. Both are absent because the infrastructure was designed by and for banks. Their exclusion is what the consumer_holdings_reading and later regulatory reforms (PSD2, open banking) address.
% DISAPPEARANCE_RATIONALE: If interbank electronic transfer infrastructure vanished overnight, banks would revert to physical cash transport, bilateral correspondent settlements, and paper-based clearing — settlement would slow from hours to days, operational risk would spike, cross-border payments would degrade severely, and the monetary system would reorganize around physical settlement constraints. The M4/M5 aggregate distinction would collapse back toward M2/M3 definitions.
% FOUNDING_PROBLEM: Interbank settlement in the 1960s relied on physical cash transport, paper checks, and bilateral correspondent relationships — slow, risky, capacity-constrained, and unable to support growing transaction volumes from economic expansion and early globalization.
% FOUNDING_PROBLEM_CORROBORATION: Central bank payment system oversight reports (BIS CPMI, Fed Payments System Risk policy) attest that settlement finality and efficiency remain live policy objectives. Infrastructure operators attest the problem is live to justify continued investment. No independent party claims the founding problem is dead — the infrastructure has evolved (RTGS, instant payments) but the core coordination function persists.
narrative_ontology:disappearance_verdict(digital_money_emergence_boundary__infrastructure_reading, world_rearranges).
narrative_ontology:founding_problem_status(digital_money_emergence_boundary__infrastructure_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(digital_money_emergence_boundary__infrastructure_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(digital_money_emergence_boundary__infrastructure_reading, 'none', 1).
narrative_ontology:epsilon_provenance(digital_money_emergence_boundary__infrastructure_reading, 0.18, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Base extractiveness (0.18) reflects infrastructure fees relative to settlement value — low because the coordination function (finality, speed, risk reduction) dominates. Suppression (0.22) reflects mandatory participation for banks needing settlement access — not coercive toward end users at this boundary. Theater ratio (0.12) is low; the infrastructure performs its stated function. Accessibility collapse (0.35) is moderate: once electronic interbank transfer exists, alternative settlement (physical cash transport, correspondent chains) becomes impractical for high-volume banks but remains technically possible. Resistance (0.28) is low-moderate: banks adopted voluntarily for efficiency; the constraint's persistence is maintained by network effects, not active suppression. The measurement grid (0-30, 6 points per metric) shows gradual extractiveness increase as infrastructure operators gained pricing power, theater creep as 'security/compliance' features were layered on, and suppression hardening as network effects locked in participation.
 *
 * PERSPECTIVAL GAP:
 *   From the infrastructure operator seat (SWIFT, ACH), the constraint is pure rope: they built and maintain coordination infrastructure, collect cost-recovery fees, and face competitive pressure from alternatives. From the commercial bank seat, it is rope with extractive tilt: they must participate to operate, fees exceed marginal cost over time, but exit is constrained by network effects. From the consumer seat (excluded at this boundary), the constraint is invisible — they experience only downstream pricing. From the central bank seat, it is scaffold-like: they oversee the infrastructure as transitional toward RTGS and later retail digital money. The engine computes per-seat classifications from these structural positions.
 *
 * DIRECTIONALITY LOGIC:
 *   Infrastructure operators (SWIFT, ACH, ATM networks) are structural beneficiaries — they control the rails, collect fees, and have arbitrage-grade exit (could sell/license technology). Commercial banks are payers with constrained exit — they must connect to settle, but can choose among networks (SWIFT vs. alternatives, multiple ACHs). Correspondent banks are beneficiaries with powerful/arbitrage position — they gatekeep cross-border access. Central banks are agenda setters with analytical exit — they set rules but don't pay fees. Consumers are excluded at this boundary (trapped) — they have no direct relationship to interbank infrastructure. The directionality derivation from beneficiary/payer declarations + power + exit produces d values that the engine scales into effective extraction per seat.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's founding problem (settlement finality and efficiency for interbank transfers) remains live — the infrastructure continues to solve it. No mandatrophy: the arrangement has not outlived its function. However, the 'middle boundary' framing means this reading captures only the bank-to-bank layer; the consumer-facing digital money constraint (consumer_holdings_reading) is a separate, later constraint with its own mandatrophy dynamics. This reading does not claim the infrastructure is the final form of digital money — only the emergence boundary for the bank-layer.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_boundary_dispute,
    'Is the digital money emergence boundary correctly placed at infrastructure enablement (1967-1977) rather than at theoretical conceptualization (1960s/1985) or consumer accessibility (1990s/2000)?',
    'Historical analysis of monetary aggregate redefinitions (M4/M5 collapse timeline) and central bank policy documents distinguishing bank-reserve electronic money from consumer-facing digital instruments. Regulatory treatment of electronic deposits vs. e-money licenses.',
    'If boundary is infrastructure, extraction beneficiaries are infrastructure operators from 1967 onward; if conceptualization, the constraint is earlier and more abstract with different beneficiary structure; if consumer holdings, the constraint emerges later with different power dynamics. Classification shifts across rope/tangled_rope/snare depending on when the constraint ''starts'' and who benefits.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_boundary_dispute, conceptual, 'Structural ambiguity in kernel boundary placement across three competing readings').

omega_variable(
    infrastructure_coordination_vs_extraction,
    'Does the interbank electronic transfer infrastructure function primarily as coordination (solving settlement finality) or does it embed extraction (rent-seeking on payment rails)?',
    'Cost-structure analysis of SWIFT/ACH/ATM network operations vs. fees charged; comparison with marginal cost of electronic settlement; historical profit data from infrastructure operators; regulatory findings on interchange/access fees.',
    'If pure coordination, claimed_type rope holds with low extractiveness. If extraction embedded, effective extraction rises for payer seats (commercial banks, ultimately consumers) and classification may shift toward tangled_rope. The 0.18 base extractiveness assumes predominantly coordination function.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(infrastructure_coordination_vs_extraction, empirical, 'Whether infrastructure operation is genuinely coordination or contains hidden extraction').

omega_variable(
    m4_m5_collapse_causality,
    'Did electronic bank deposits genuinely blur monetary aggregates (M4/M5 collapse) starting in the 1970s, or was the collapse a later statistical artifact of measurement changes?',
    'Central bank monetary aggregate publications 1970-1990; academic literature on monetary aggregation problems; BIS working papers on electronic money measurement.',
    'If M4/M5 collapse began at infrastructure enablement, the constraint marks a genuine structural shift in money definition. If later, the ''middle boundary'' claim is retrospective rationalization.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(m4_m5_collapse_causality, empirical, 'Causal claim about monetary aggregate redefinition timing').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(digital_money_emergence_boundary__infrastructure_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(digital_money_infrastructure_tr_t0, digital_money_emergence_boundary__infrastructure_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(digital_money_infrastructure_tr_t5, digital_money_emergence_boundary__infrastructure_reading, theater_ratio, 5, 0.07).
narrative_ontology:measurement(digital_money_infrastructure_tr_t10, digital_money_emergence_boundary__infrastructure_reading, theater_ratio, 10, 0.08).
narrative_ontology:measurement(digital_money_infrastructure_tr_t15, digital_money_emergence_boundary__infrastructure_reading, theater_ratio, 15, 0.1).
narrative_ontology:measurement(digital_money_infrastructure_tr_t20, digital_money_emergence_boundary__infrastructure_reading, theater_ratio, 20, 0.11).
narrative_ontology:measurement(digital_money_infrastructure_tr_t25, digital_money_emergence_boundary__infrastructure_reading, theater_ratio, 25, 0.12).
narrative_ontology:measurement(digital_money_infrastructure_tr_t30, digital_money_emergence_boundary__infrastructure_reading, theater_ratio, 30, 0.12).

% Extraction over time
narrative_ontology:measurement(digital_money_infrastructure_be_t0, digital_money_emergence_boundary__infrastructure_reading, base_extractiveness, 0, 0.08).
narrative_ontology:measurement(digital_money_infrastructure_be_t5, digital_money_emergence_boundary__infrastructure_reading, base_extractiveness, 5, 0.1).
narrative_ontology:measurement(digital_money_infrastructure_be_t10, digital_money_emergence_boundary__infrastructure_reading, base_extractiveness, 10, 0.12).
narrative_ontology:measurement(digital_money_infrastructure_be_t15, digital_money_emergence_boundary__infrastructure_reading, base_extractiveness, 15, 0.14).
narrative_ontology:measurement(digital_money_infrastructure_be_t20, digital_money_emergence_boundary__infrastructure_reading, base_extractiveness, 20, 0.16).
narrative_ontology:measurement(digital_money_infrastructure_be_t25, digital_money_emergence_boundary__infrastructure_reading, base_extractiveness, 25, 0.18).
narrative_ontology:measurement(digital_money_infrastructure_be_t30, digital_money_emergence_boundary__infrastructure_reading, base_extractiveness, 30, 0.18).

% Suppression requirement over time
narrative_ontology:measurement(digital_money_infrastructure_su_t0, digital_money_emergence_boundary__infrastructure_reading, suppression_requirement, 0, 0.15).
narrative_ontology:measurement(digital_money_infrastructure_su_t5, digital_money_emergence_boundary__infrastructure_reading, suppression_requirement, 5, 0.18).
narrative_ontology:measurement(digital_money_infrastructure_su_t10, digital_money_emergence_boundary__infrastructure_reading, suppression_requirement, 10, 0.2).
narrative_ontology:measurement(digital_money_infrastructure_su_t15, digital_money_emergence_boundary__infrastructure_reading, suppression_requirement, 15, 0.21).
narrative_ontology:measurement(digital_money_infrastructure_su_t20, digital_money_emergence_boundary__infrastructure_reading, suppression_requirement, 20, 0.22).
narrative_ontology:measurement(digital_money_infrastructure_su_t25, digital_money_emergence_boundary__infrastructure_reading, suppression_requirement, 25, 0.22).
narrative_ontology:measurement(digital_money_infrastructure_su_t30, digital_money_emergence_boundary__infrastructure_reading, suppression_requirement, 30, 0.22).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(digital_money_emergence_boundary__infrastructure_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(digital_money_emergence_boundary__infrastructure_reading, 0.15).
narrative_ontology:affects_constraint(digital_money_emergence_boundary__infrastructure_reading, digital_money_emergence_boundary__consumer_holdings_reading).
narrative_ontology:affects_constraint(digital_money_emergence_boundary__infrastructure_reading, digital_money_emergence_boundary__conceptualization_reading).
narrative_ontology:affects_constraint(digital_money_emergence_boundary__infrastructure_reading, rtgs_settlement_infrastructure).
narrative_ontology:affects_constraint(digital_money_emergence_boundary__infrastructure_reading, electronic_money_directive_2000).

% DUAL FORMULATION NOTE:
% Part of digital_money_emergence_boundary constraint family. This reading (infrastructure) is upstream of consumer_holdings_reading: electronic interbank infrastructure is a necessary (but not sufficient) condition for consumer-facing digital money. The conceptualization_reading is parallel upstream — theoretical framing enabled infrastructure investment. All three readings mark distinct emergence boundaries with different ε and beneficiary structures.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(digital_money_emergence_boundary__infrastructure_reading, institutional, 0.15).
constraint_indexing:directionality_override(digital_money_emergence_boundary__infrastructure_reading, powerful, 0.45).
constraint_indexing:directionality_override(digital_money_emergence_boundary__infrastructure_reading, moderate, 0.65).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
