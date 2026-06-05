% ============================================================================
% CONSTRAINT STORY: digital_money_emergence_boundary__infrastructure_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_digital_money_infrastructure, []).

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
    constraint_indexing:directionality_override/3,
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
 *   human_readable: Digital Money Emergence (Infrastructure Reading): Banking Transfer Rails Control
 *   domain: monetary_economics/financial_infrastructure/technology_governance
 *
 * SUMMARY:
 *   The infrastructure reading of digital money emergence places the boundary
 *   at 1967-1977, when banking infrastructure providers deployed systems
 *   (ATMs in 1967, ACH in 1972, SWIFT in 1973-1977) that enabled electronic
 *   transfer of deposits between banks without physical cash movement. This
 *   is one of three readings of the same kernel: 'when did digital money
 *   emerge?' The infrastructure reading claims that the boundary lies at the
 *   layer of institutional interbank infrastructure, not at theoretical
 *   formalization (conceptualization reading) or consumer-accessible
 *   instruments (consumer_holdings_reading). Under this reading, money
 *   becomes 'digital' when banks can move it electronically, even if
 *   consumers still hold it as non-tangible bank balances rather than as
 *   independent digital instruments they control directly. This places the
 *   boundary precisely at the point where banking infrastructure operators
 *   (SWIFT, ACH, Federal Reserve) gain control of the transfer rails and
 *   where the traditional M1/M2 categories (physical cash, demand deposits)
 *   begin to blur into new categories (electronic deposits, interbank
 *   transfers). The infrastructure reading benefits banking institutions and
 *   infrastructure operators, who experience the constraint as a genuine
 *   solution to the coordination problem of clearing global financial flows.
 *   However, from the perspective of peripheral actors (smaller banks,
 *   depositors, alternative financial systems), the same infrastructure
 *   reading naturalizes what is actually an extractive mechanism: dependency
 *   on centralized rails controlled by a banking cartel.
 *
 * KEY AGENTS:
 *   - Banking Infrastructure Operators (SWIFT, ACH, Federal Reserve): Primary beneficiary (institutional/arbitrage) — design and control the rails that define the boundary; capture fees and data from all electronic transfers
 *   - Large Institutional Banks: Primary beneficiary (institutional/arbitrage) — founding members and major users of SWIFT/ACH; benefit from the coordination function and from the gatekeeping that excludes alternative transfer systems
 *   - Peripheral Banks: Primary victim (powerless/trapped) — must participate in the rails controlled by larger institutions; bear the suppression of infrastructure gatekeeping and cannot arbitrage to alternatives
 *   - Depositors (transitional era): Secondary victim (moderate/constrained) — gain speed of transfer but lose privacy; electronic infrastructure enables real-time financial surveillance and account control
 *   - Central Banks: Mixed (organized/constrained) — benefit from the infrastructure for monetary policy transmission but constrained by the power large banks gain through the rails
 *   - Alternative Financial Systems (credit unions, cooperatives, decentralized networks): Structural victims (constrained/mobile) — excluded from the infrastructure reading's definition of 'real' digital money; their alternative systems are not recognized as part of the money supply under this boundary
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(digital_money_emergence_boundary__infrastructure_reading, 0.52).
domain_priors:suppression_score(digital_money_emergence_boundary__infrastructure_reading, 0.45).
domain_priors:theater_ratio(digital_money_emergence_boundary__infrastructure_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(digital_money_emergence_boundary__infrastructure_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(digital_money_emergence_boundary__infrastructure_reading, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(digital_money_emergence_boundary__infrastructure_reading, theater_ratio, 0.38).

% --- Constraint claim ---
narrative_ontology:constraint_claim(digital_money_emergence_boundary__infrastructure_reading, tangled_rope).
narrative_ontology:human_readable(digital_money_emergence_boundary__infrastructure_reading, "Digital Money Emergence (Infrastructure Reading): Banking Transfer Rails Control").
narrative_ontology:topic_domain(digital_money_emergence_boundary__infrastructure_reading, "monetary_economics/financial_infrastructure/technology_governance").

domain_priors:requires_active_enforcement(digital_money_emergence_boundary__infrastructure_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(digital_money_emergence_boundary__infrastructure_reading, 'd7502e49-63c4-430a-922c-91292a831153').
narrative_ontology:cs_kernel_codification('d7502e49-63c4-430a-922c-91292a831153', distributed).
narrative_ontology:cs_authority_grounding('d7502e49-63c4-430a-922c-91292a831153', extraction).
narrative_ontology:cs_interpretation_layer_present('d7502e49-63c4-430a-922c-91292a831153').
narrative_ontology:cs_reading_relation('d7502e49-63c4-430a-922c-91292a831153', digital_money_emergence_boundary__conceptualization_reading, coexists_with).
narrative_ontology:cs_reading_relation('d7502e49-63c4-430a-922c-91292a831153', digital_money_emergence_boundary__consumer_holdings_reading, influences).
narrative_ontology:cs_axiom('d7502e49-63c4-430a-922c-91292a831153', foundational, infrastructure_substrate_defines_money).
narrative_ontology:cs_axiom_status(infrastructure_substrate_defines_money, holdable).
narrative_ontology:cs_axiom_grounding('d7502e49-63c4-430a-922c-91292a831153', infrastructure_substrate_defines_money, conventional).
narrative_ontology:cs_axiom('d7502e49-63c4-430a-922c-91292a831153', secondary, interbank_settlement_is_primary_money_function).
narrative_ontology:cs_axiom_status(interbank_settlement_is_primary_money_function, holdable).
narrative_ontology:cs_axiom_grounding('d7502e49-63c4-430a-922c-91292a831153', interbank_settlement_is_primary_money_function, instrumental).
narrative_ontology:cs_reference_frame('d7502e49-63c4-430a-922c-91292a831153', institutional_electronic_settlement_capability).
narrative_ontology:cs_drift_state('d7502e49-63c4-430a-922c-91292a831153', contemporary_instant_payment_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('d7502e49-63c4-430a-922c-91292a831153', '2026-02-26T00:00:00Z').
narrative_ontology:cs_kernel_id(digital_money_emergence_boundary__infrastructure_reading, digital_money_emergence_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(digital_money_emergence_boundary__infrastructure_reading, banking_infrastructure_operators).
narrative_ontology:constraint_beneficiary(digital_money_emergence_boundary__infrastructure_reading, large_institutional_banks).
narrative_ontology:constraint_victim(digital_money_emergence_boundary__infrastructure_reading, peripheral_financial_actors).
narrative_ontology:constraint_victim(digital_money_emergence_boundary__infrastructure_reading, monetary_system_transparency).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PERIPHERAL BANK (SNARE) — Trapped within infrastructure dependency. Must route all electronic transfers through SWIFT, ACH, or equivalent rails controlled by large institutional banks and infrastructure operators. Cannot exit or arbitrage; bears the full suppression of infrastructure gatekeeping. The infrastructure reading naturalizes this dependency as 'digital money requires institutional rails' — but from this position, the rails are an extractive mechanism, not a coordination solution.
constraint_indexing:constraint_classification(digital_money_emergence_boundary__infrastructure_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: TRANSITIONAL DEPOSITOR (TANGLED ROPE) — Benefits from the emergence of digital account transfers (can move money faster than physical check clearing). Also bears extraction: the bank now knows deposit location and timing in real-time; electronic transfer infrastructure enables financial surveillance and account freezing that physical cash prevented. Mixed: genuine coordination (faster settlement) overlaid with asymmetric extraction (visibility and control).
constraint_indexing:constraint_classification(digital_money_emergence_boundary__infrastructure_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: LARGE INSTITUTIONAL BANK (ROPE) — Primary beneficiary. The infrastructure reading of digital money arrival (1967-1977) places the boundary exactly where large institutional banks gain control of the transfer rails. SWIFT (1973) and ACH (1972) were created by banking consortia. The constraint is experienced as pure coordination: a genuine solution to the problem of clearing billions in daily transactions across jurisdictions. Net beneficiary — the rails that define 'digital money' under this reading are the rails these banks control and profit from.
constraint_indexing:constraint_classification(digital_money_emergence_boundary__infrastructure_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: INFRASTRUCTURE OPERATOR (ROPE) — Core beneficiary. SWIFT and ACH operators experience the boundary (1967-1977 infrastructure emergence) as a pure coordination mechanism they are designing. The constraint from their perspective is: 'we are solving the technical problem of moving money electronically.' The extraction (fees, data monopoly, financial gatekeeping) is not visible from within this perspective — the operator sees the rails as a genuine coordination solution, not as a mechanism for capturing rents.
constraint_indexing:constraint_classification(digital_money_emergence_boundary__infrastructure_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: CENTRAL BANK AUTHORITY (TANGLED ROPE) — Mixed relationship. Benefits from the infrastructure reading's boundary placement: digital money defined at the institutional/infrastructure layer means central banks can conduct monetary policy through deposit rates, reserve requirements, and interbank settlement without touching consumer cash (until digital currency emerges). Also bears extraction pressure: large institutional banks gain power to freeze accounts, move capital instantly, and evade controls via SWIFT routing. Central banks are constrained by the infrastructure they do not control.
constraint_indexing:constraint_classification(digital_money_emergence_boundary__infrastructure_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / PITON VIEW (PITON) — From a civilizational scale, the infrastructure reading's boundary (1967-1977 emergence) appears contingent and performative. The categorization 'digital money begins when banks can move it electronically' is a choice of definition, not an inevitable boundary. The actual emergence of digital money (from a non-committed perspective) has no single boundary — it is a continuous shift from check-clearing to wire transfer to real-time settlement to CBDC consideration. The theatrical element: declaring a specific date range (1967-1977) when 'money became digital' is a narrative choice that serves the interests of the infrastructure operators and the banks that control the rails.
constraint_indexing:constraint_classification(digital_money_emergence_boundary__infrastructure_reading, piton,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(digital_money_emergence_boundary__infrastructure_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(digital_money_emergence_boundary__infrastructure_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(digital_money_emergence_boundary__infrastructure_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(digital_money_emergence_boundary__infrastructure_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(digital_money_emergence_boundary__infrastructure_reading, TR),
    TR >= 0.70.

:- end_tests(digital_money_emergence_boundary__infrastructure_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The infrastructure reading benefits banking operators and large institutions through control of the transfer rails. This benefit is not extracted through high suppression (agents have access to the rails) but through high asymmetry: peripheral actors depend on the rails but do not control them, and the infrastructure operators capture rents through fees and data monopoly. The extractiveness is lower than a pure snare because the infrastructure genuinely solves a coordination problem (clearing global flows), and participants gain some benefit. But it is higher than a pure rope because the beneficiaries (banking operators and large banks) controlled the design and deployment of the infrastructure, and the boundary (1967-1977 infrastructure emergence) reflects their interests, not a neutral discovery. Suppression (0.45): Moderate. The infrastructure reading is presented as a technical inevitability ('digital money requires institutional rails'), which suppresses alternative boundary placements and alternative technical architectures. However, suppression is not total — the consumer_holdings_reading and conceptualization_reading are still live options (different parties hold them). Theater ratio (0.38): Moderate-low. The infrastructure reading is not primarily performative — the technical infrastructure (ATMs, ACH, SWIFT) genuinely works and serves a real function. However, the claim that THIS boundary (1967-1977 infrastructure emergence) is the 'real' emergence of digital money contains a performative element: it is a definitional choice that serves the interests of the beneficiaries. The boundary could have been placed elsewhere (at consumer accessibility, at theoretical formalization, at decentralized protocols), and the choice to place it at infrastructure reflects institutional power, not technical necessity.
 *
 * PERSPECTIVAL GAP:
 *   The infrastructure reading demonstrates a sharp perspectival gap between beneficiaries and victims. The large institutional banks and infrastructure operators (SWIFT, ACH) see the constraint as pure coordination: 'we are solving the genuine problem of moving money electronically across banks and jurisdictions.' They experience rope. The peripheral banks and alternative financial systems see the same constraint as gatekeeping: 'we are excluded from the rails that define digital money, and we must pay fees and fees to participate in a system we did not design.' They experience snare. The central bank sees mixed effects: the infrastructure enables monetary policy transmission (rope benefit) but also empowers large banks to evade controls through instant SWIFT transfers (snare cost). The depositor sees genuine benefit (faster settlement) layered with extraction (financial surveillance and account control). The analytical observer at civilizational scale sees the boundary itself as contingent: the infrastructure reading's claim that 'digital money emerged when banks could move it electronically' is a choice, not a discovery. Three different readings produce three different boundaries separated by 20-40 years, which reveals that 'when digital money emerged' is not a fact but a narrative choice that reflects the committer's interests.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) maps the agent's structural position relative to the constraint. Banking infrastructure operators have d ≈ 0.10 (beneficiaries with full arbitrage) — they designed the constraint and capture rent. Large institutional banks have d ≈ 0.15 (beneficiaries with strong arbitrage) — founding members of SWIFT/ACH, they benefit from both the coordination function and the gatekeeping. Peripheral banks have d ≈ 0.70 (victims with constrained exit) — they must use the rails but do not control them and cannot exit without losing access to the financial system. Depositors have d ≈ 0.55 (mixed victims and beneficiaries with constrained exit) — they gain speed but lose privacy, and they cannot choose to opt out without losing access to the modern financial system. The infrastructure reading's boundary placement naturally produces these directionality gaps because the boundary is defined precisely at the point where the infrastructure operators and large banks gain control.
 *
 * MANDATROPHY ANALYSIS:
 *   The infrastructure reading resolves mandatrophy by acknowledging that its claimed_type (tangled_rope) reflects its position as ONE reading among multiple valid readings of the kernel. The reading provides genuine coordination (electronic settlement is a real solution to a real problem of clearing global flows) AND asymmetric extraction (the infrastructure operators and large banks control the rails and capture rents from fees and data monopoly). The mandatrophy is resolved by recognizing that the three readings are not competing claims about objective truth, but competing institutional commitments: infrastructure operators commit to the infrastructure reading; theorists commit to the conceptualization reading; consumer advocates commit to the consumer_holdings reading. The classification (tangled_rope) correctly captures that the infrastructure reading embodies both coordination and extraction, and the perspectival gap correctly shows that different agents experience the same constraint differently depending on their structural position.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_contest_identity,
    'Is THIS reading''s boundary (infrastructure emergence at 1967-1977) a discovery of when money genuinely became digital, or a definitional choice that benefits banking infrastructure operators?',
    'Compare the infrastructure reading''s boundary with sibling readings'' boundaries: conceptualization_reading places emergence at 1960s theoretical formalization; consumer_holdings_reading places emergence at 1990s-2000s consumer-accessible digital instruments. The gap between boundaries reveals that ''when digital money emerged'' is NOT a fact about the world but a choice about which layer (infrastructure, theory, consumer access) counts as the ''real'' boundary.',
    'If this is genuine discovery: the infrastructure reading''s classification stands. If this is definitional choice: the reading''s claimed_type should shift from tangled_rope (coordination+extraction) to snare or piton (institutionalized gatekeeping), because the entire boundary becomes performative.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_contest_identity, conceptual, 'Whether the infrastructure boundary is discovered or constructed').

omega_variable(
    beneficiary_capture_scope,
    'Do banking infrastructure operators (SWIFT, ACH) experience this constraint as genuine coordination (rope perspective) or as gatekeeping mechanism (snare perspective with rent-extraction)? The two readings produce opposite chi values.',
    'Historical analysis of SWIFT/ACH design decisions: (1) Were alternatives (decentralized ledgers, non-banking intermediaries, open protocols) actively suppressed or did they simply not exist? (2) Do fee structures reflect marginal costs or monopoly rents? (3) Do access restrictions on SWIFT/ACH membership serve coordination efficiency or banking cartel protection? (4) Post-decentralization: which actors promote or block adoption of alternative rails (blockchain, CISA, instant payments)?',
    'If coordination: operator perspective is genuinely rope, beneficiary is real. If gatekeeping: operator perspective is aspirational rope (they experience themselves as solving a problem) but actual classification is snare, because the extraction mechanism (fee monopoly, access control, real-time financial surveillance) is what they do, not a side effect.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_capture_scope, empirical, 'Whether infrastructure operators provide coordination or gatekeeping').

omega_variable(
    alternative_protocol_feasibility,
    'In 1967-1977, were alternative technical architectures for electronic money transfer feasible? If yes, the infrastructure reading''s boundary is contingent (a choice favoring centralized banking). If no, the boundary is structurally necessary.',
    'Counterfactual engineering analysis: given 1960s compute, telecommunications, and cryptographic capability, could a decentralized ledger or non-banking intermediary have provided electronic settlement? Compare with (1) actual distributed systems capability of the era (Arpanet, Ethernet), (2) cryptographic literature (pre-Chaum: Merkle trees existed; pre-RSA: DES was available), (3) organizational capacity (stock exchange clearinghouses were not banking monopolies).',
    'If alternatives were feasible but suppressed: the infrastructure reading''s boundary reflects banking cartel power, not technical necessity. Extractiveness should rise to snare range. If no alternatives existed: the boundary is structurally necessary, and the rope classification is justified.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_protocol_feasibility, empirical, 'Feasibility of alternative electronic transfer architectures in 1960s-1970s').

omega_variable(
    reading_committer_frame,
    'Is the infrastructure reading a defense of banking institutional design, or a neutral historical observation about when digital money emerged?',
    'Textual and institutional analysis: (1) Who benefits from adopting the infrastructure reading''s boundary? (2) Which contemporary policy debates (CBDC design, stablecoin regulation, banking system architecture) would be settled by accepting this reading? (3) What does acceptance of this reading preclude (e.g., it makes consumer-accessible digital currency seem like a separate phenomenon, not a logical continuation of digital money)? (4) Authorial intent: does the reading emerge from within banking/finance institutions or from external observers?',
    'If the reading serves banking institutional interests: its classification may be correct, but the committer''s interest in the boundary should be explicitly documented. The reading would coexist_with rather than foreclose the consumer_holdings_reading. If the reading is neutral observation: the reading''s boundaries should align across independent sources (historical records, technical archives, regulatory documents). Misalignment suggests committer capture.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_committer_frame, preference, 'Whether the infrastructure reading reflects institutional bias toward banking control').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(digital_money_emergence_boundary__infrastructure_reading, 1967, 1990).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(digmon_infra_tr_t1967, digital_money_emergence_boundary__infrastructure_reading, theater_ratio, 1967, 0.25).
narrative_ontology:measurement(digmon_infra_tr_t1977, digital_money_emergence_boundary__infrastructure_reading, theater_ratio, 1977, 0.38).
narrative_ontology:measurement(digmon_infra_tr_t1990, digital_money_emergence_boundary__infrastructure_reading, theater_ratio, 1990, 0.42).

% Extraction over time
narrative_ontology:measurement(digmon_infra_be_t1967, digital_money_emergence_boundary__infrastructure_reading, base_extractiveness, 1967, 0.15).
narrative_ontology:measurement(digmon_infra_be_t1972, digital_money_emergence_boundary__infrastructure_reading, base_extractiveness, 1972, 0.35).
narrative_ontology:measurement(digmon_infra_be_t1977, digital_money_emergence_boundary__infrastructure_reading, base_extractiveness, 1977, 0.52).
narrative_ontology:measurement(digmon_infra_be_t1990, digital_money_emergence_boundary__infrastructure_reading, base_extractiveness, 1990, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(digmon_infra_su_t1967, digital_money_emergence_boundary__infrastructure_reading, suppression_requirement, 1967, 0.2).
narrative_ontology:measurement(digmon_infra_su_t1972, digital_money_emergence_boundary__infrastructure_reading, suppression_requirement, 1972, 0.4).
narrative_ontology:measurement(digmon_infra_su_t1977, digital_money_emergence_boundary__infrastructure_reading, suppression_requirement, 1977, 0.45).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(digital_money_emergence_boundary__infrastructure_reading, global_infrastructure).
narrative_ontology:affects_constraint(digital_money_emergence_boundary__infrastructure_reading, digital_money_emergence_boundary__conceptualization_reading).
narrative_ontology:affects_constraint(digital_money_emergence_boundary__infrastructure_reading, digital_money_emergence_boundary__consumer_holdings_reading).
narrative_ontology:affects_constraint(digital_money_emergence_boundary__infrastructure_reading, swift_interbank_settlement_gatekeeping).
narrative_ontology:affects_constraint(digital_money_emergence_boundary__infrastructure_reading, ach_clearing_monopoly).

% DUAL FORMULATION NOTE:
% The digital_money_emergence_boundary kernel is decomposed into three constraint stories corresponding to three distinct readings: infrastructure_reading (this file), conceptualization_reading (1960s theoretical emergence), consumer_holdings_reading (1990s-2000s consumer access). Each reading instantiates a different ε value and produces different beneficiary/victim structures. They are not alternative measurements of the same constraint, but distinct constraints with different classification types. The infrastructure_reading affects (and is affected by) the sibling readings in a network where each reading's boundary placement creates structural pressure on the others. See dual_formulation_note in sibling files for the complete network topology.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(digital_money_emergence_boundary__infrastructure_reading, institutional, 0.1).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
