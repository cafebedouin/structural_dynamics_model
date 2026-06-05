% ============================================================================
% CONSTRAINT STORY: swift_legacy_piton
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_swift_legacy_piton, []).

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
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: swift_legacy_piton
 *   human_readable: The SWIFT Financial Messaging Inertia
 *   domain: technological/economic/infrastructure
 *
 * SUMMARY:
 *   The SWIFT financial messaging system exemplifies a Piton constraint: a
 *   formerly functional coordination mechanism that has become technically
 *   obsolete but remains mandatory through institutional inertia. Founded in
 *   1973, SWIFT solved the critical coordination problem of standardizing
 *   cross-border interbank messaging when no alternative existed. Forty years
 *   later, superior technologies have emerged (real-time gross settlement,
 *   blockchain, central bank digital currencies, proprietary settlement
 *   corridors), yet SWIFT remains the de facto global standard for
 *   correspondent banking and high-value transfers. The system persists not
 *   because it is technically optimal but because the switching costs are
 *   asymmetrically distributed: dominant clearing banks can negotiate
 *   workarounds; smaller banks cannot. The theater ratio (0.78) reflects that
 *   SWIFT operations are now 78% procedural overhead: message format
 *   validations, settlement delay accommodations, compliance rituals
 *   necessitated only by SWIFT's technical limitations. The constraint
 *   exemplifies how institutional structures maintain themselves through
 *   regulatory mandate and coordination lock-in rather than genuine
 *   coordination benefits. As central bank digital currencies and real-time
 *   payment networks mature, SWIFT's functional role is being cannibalized —
 *   the question is not whether SWIFT will eventually fail, but how long
 *   institutional inertia will sustain it after its technical purpose has
 *   been superseded.
 *
 * KEY AGENTS:
 *   - SWIFT Organization: Primary beneficiary (institutional/arbitrage) — maintains revenue stream and operational control despite technical obsolescence; sees constraint as legitimate coordination service
 *   - Global Banking Consortium: Primary victim/beneficiary hybrid (institutional/constrained) — locked into SWIFT by coordination lock-in despite availability of superior alternatives; bears theater costs
 *   - Dominant Clearing Banks: Secondary beneficiary (powerful/arbitrage) — can negotiate custom settlement protocols and prioritized access; benefit from SWIFT without bearing full theater costs
 *   - Smaller Regional Banks & Fintech: Secondary victim (powerless/trapped) — face full theater overhead and cannot exit; locked into SWIFT fee structure and messaging format
 *   - Regulatory Framework (Central Banks): Institutional actor (institutional/arbitrage) — maintain SWIFT mandate through regulation; benefit from appearance of unified global control
 *   - Emerging CBDC & RTGS Systems: Organized agents (organized/mobile) — building alternative settlement corridors with real-time processing and lower theater; represent structural sunset path
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(swift_legacy_piton, 0.18).
domain_priors:suppression_score(swift_legacy_piton, 0.35).
domain_priors:theater_ratio(swift_legacy_piton, 0.78).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(swift_legacy_piton, extractiveness, 0.18).
narrative_ontology:constraint_metric(swift_legacy_piton, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(swift_legacy_piton, theater_ratio, 0.78).

% --- Constraint claim ---
narrative_ontology:constraint_claim(swift_legacy_piton, piton).
narrative_ontology:human_readable(swift_legacy_piton, "The SWIFT Financial Messaging Inertia").
narrative_ontology:topic_domain(swift_legacy_piton, "technological/economic/infrastructure").

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(swift_legacy_piton, swift_organization).
narrative_ontology:constraint_beneficiary(swift_legacy_piton, incumbent_banking_system).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: GLOBAL BANKING CONSORTIUM (PITON) — Banks remain locked into SWIFT because exit costs exceed perceived benefits despite superior alternatives (blockchain, real-time payment networks). The constraint persists through institutional inertia: SWIFT's network effects are real but diminishing; the messaging protocol itself is technically degraded relative to modern standards (batch processing, settlement delays, message validation overhead). theater_ratio=0.78 reflects that SWIFT operations are 78% procedural theater: compliance checks, format validations, inter-bank reconciliation rituals that are necessary only because SWIFT cannot be replaced wholesale. d≈0.50, f(d)≈0.65, σ=1.2 → χ≈0.14.
constraint_indexing:constraint_classification(swift_legacy_piton, piton,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 2: SWIFT ORGANIZATION (ROPE) — SWIFT sees the constraint as pure coordination: the messaging standard solves the genuine collective action problem of global interbank communication. From SWIFT's vantage, maintaining the standard IS providing coordination services — fee revenue and operational control are returns on that service, not extraction. d≈0.05, f(d)≈-0.12, σ=1.2 → χ≈-0.03. Negative effective extraction = net beneficiary through coordination.
constraint_indexing:constraint_classification(swift_legacy_piton, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: DOMINANT CLEARING BANKS (ROPE) — Systemically important banks (JPMorgan, Deutsche Bank, BNY Mellon as SWIFT hub operators) have arbitrage exit options: they can negotiate custom messaging protocols, prioritized settlement, or build proprietary rails. Their structural position within SWIFT gives them beneficial directionality. d≈0.10, f(d)≈-0.01, σ=1.2 → χ≈-0.00. Near-zero effective extraction; they benefit from SWIFT's coordination.
constraint_indexing:constraint_classification(swift_legacy_piton, rope,
    context(agent_power(powerful),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: SMALLER REGIONAL BANKS & FINTECH (SNARE) — Smaller banks and fintech firms face the full weight of the constraint without arbitrage exits. They cannot build proprietary settlement rails; they cannot negotiate custom protocols. They are locked into SWIFT's messaging format, operational procedures, and fee structure. The theater (procedural overhead, compliance rituals, format validations) falls entirely on them. d≈0.90, f(d)≈1.35, σ=1.2 → χ≈0.29. Effective extraction approaching snare threshold, though raw ε=0.18 keeps this below pure snare.
constraint_indexing:constraint_classification(swift_legacy_piton, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 5: EMERGING PAYMENT CORRIDORS (SCAFFOLD) — Real-time gross settlement (RTGS) systems, central bank digital currencies (CBDCs), and blockchain-based settlement layers (Ripple, Stellar) represent alternative coordination mechanisms with sunset logic. These systems are gradually replacing SWIFT's coordination function: instant settlement, lower theater (no batch processing), transparent fee structures. They have mobile exit options (jurisdictions can adopt CBDC-to-CBDC transfers, bypass SWIFT entirely). d≈0.35, f(d)≈0.31, σ=1.2 → χ≈0.07. Low effective extraction because the exit path is real and increasingly viable.
constraint_indexing:constraint_classification(swift_legacy_piton, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: REGULATORY FRAMEWORK (PITON) — Central banks and financial regulators maintain SWIFT as a structurally degraded but symbolically irreplaceable coordination system. Regulators cannot decommission SWIFT without a wholesale redesign of cross-border settlement — the institutional burden is too high. Regulations mandate SWIFT compatibility (messaging format, settlement procedures, compliance reporting). The theater here is 100% regulatory: rules require SWIFT use even when superior alternatives exist technically. theater_ratio=0.78 understates regulatory theater; from the regulator's view, it approaches 0.95. d≈0.05, f(d)≈-0.12, σ=1.2 → χ≈-0.01. Negative extraction because regulators benefit from the illusion of unified global control.
constraint_indexing:constraint_classification(swift_legacy_piton, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / FALSE NATURAL LAW (MOUNTAIN REJECTED) — Some analysts argue that SWIFT is a natural law: global financial systems require interoperable messaging standards, and coordination standards always have switching costs. But the structural data (ε=0.18, suppression=0.35, theater=0.78) contradicts a mountain classification. This is NOT an irreducible constraint. Superior alternatives (CBDC, RTGS, blockchain rails) are technically feasible and already operational in many jurisdictions. The perceived immutability is institutional inertia (piton), not natural law. The mountain perspective is a false summit, revealing how institutions naturalize their own technical debt.
constraint_indexing:constraint_classification(swift_legacy_piton, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(swift_legacy_piton_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(swift_legacy_piton, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(swift_legacy_piton, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(swift_legacy_piton, TR),
    TR >= 0.70.

:- end_tests(swift_legacy_piton_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.18): Low. SWIFT's raw extractive capacity is modest because the constraint lacks the suppression and asymmetry of pure snares. SWIFT is maintained through coordination lock-in, not coercion. Banks continue using SWIFT partly because alternatives are fragmented (no single superior standard yet dominates) and partly because regulatory mandate persists. The constraint extracts through procedural overhead and fee revenue, not through blocking alternatives. Suppression (0.35): Moderate. Significant barriers to exit include: (1) regulatory mandate requiring SWIFT compatibility, (2) network effects (all major banks use SWIFT, so opting out is costly), (3) operational sunk costs in integration, and (4) regulatory uncertainty around non-SWIFT settlement. But suppression is not total — jurisdictions (EU, Singapore) are already implementing RTGS alternatives, and CBDCs are being deployed. Theater ratio (0.78): High and rising. SWIFT's procedural overhead has increased over 30 years as the system has accumulated compliance layers, settlement delay accommodations, and validation rituals. Batch processing (T+2 settlement standard) requires extensive reconciliation theater. Message validation requires human review due to format inflexibility. These procedures were necessary when SWIFT was technically optimal; they are now pure theater maintaining a degraded system. Rising theater ratio (0.55 → 0.78 over 30 years) indicates classic piton degradation: the system loses function while its procedural apparatus becomes increasingly elaborate.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates the piton structure across perspectives. The SWIFT organization sees coordination (Rope) — they are performing a coordination service. The analytical observer risks seeing natural law (Mountain) — global settlement requires interoperable standards. But the smaller banks see extraction with no exit (Snare) — they are forced to bear theater costs without alternatives. The emerging CBDC corridors see a temporary problem with a real sunset (Scaffold) — new infrastructure is being built to replace SWIFT. The regulatory framework sees institutional necessity (Piton from another angle) — decommissioning SWIFT requires massive regulatory redesign. The perspectival gap reveals that SWIFT's continued existence is not coordination failure (which would suggest Rope or Tangled Rope) but institutional inertia masquerading as coordination necessity. The constraint is maintained not because it solves a genuine collective action problem that cannot be solved otherwise, but because the institutional machinery around it has become self-perpetuating.
 *
 * DIRECTIONALITY LOGIC:
 *   SWIFT Organization: Beneficiary + arbitrage → d≈0.05, f(d)≈-0.12. Net beneficiary through fee revenue and operational control. Dominant clearing banks: Beneficiary + arbitrage → d≈0.10, f(d)≈-0.01. Secondary beneficiaries with negotiating power. Global banking consortium: Mixed + constrained → d≈0.50, f(d)≈0.65. Symmetric position — benefit from coordination network effects but bear theater costs. Smaller banks & fintech: Victim + trapped → d≈0.90, f(d)≈1.35. Maximum extraction from perspective of powerless agent; d reflects inability to exit despite superior alternatives existing. Regulatory framework: Beneficiary + arbitrage → d≈0.05, f(d)≈-0.12. Benefits from illusion of unified control; can impose mandate without bearing theater costs. CBDC corridors: Mobile agent → d≈0.35, f(d)≈0.31. Low extraction because exit path is real; they have mobile options and see a functional sunset path.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy here is resolved by recognizing that SWIFT is NOT a natural law but a piton — a degraded institutional constraint maintained by procedural theater and regulatory mandate rather than genuine coordination necessity. The false natural law framing ('global settlement requires interoperable standards, therefore SWIFT is inevitable') naturalizes what is actually a contingent institutional arrangement. Superior alternatives (CBDC, RTGS, blockchain rails) prove that the coordination problem is solvable without SWIFT. The piton classification correctly identifies the constraint as inertial: the system persists despite technical obsolescence because (1) switching costs are high, (2) regulatory mandate persists, (3) coordination lock-in creates network effects, and (4) institutional stakeholders benefit from the status quo. The theater ratio (0.78) rising over time is the diagnostic signal of piton degradation: procedures accumulate even as functional utility declines. The sunset mechanism (CBDC & RTGS adoption) is structurally real but faces regulatory and institutional resistance that slows transition. The constraint is resolvable not through optimization but through institutional replacement — when the majority of settlement volume shifts to CBDC corridors, SWIFT becomes optional, and the piton classification transitions from 'maintained through inertia' to 'historical artifact.'
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    cbdc_adoption_timeline,
    'What adoption rate and timeline for CBDC-to-CBDC transfers would constitute the functional sunset of SWIFT''s cross-border settlement monopoly?',
    'Measurement of CBDC adoption across G20+ central banks; tracking of transaction volume shifting from SWIFT to CBDC rails; corridor-by-corridor analysis of SWIFT dependency decline',
    'If adoption >50% within 10 years: scaffold perspective confirmed, piton classification transitions to explicit sunset machinery. If adoption <20% within 20 years: SWIFT piton persists due to coordination lock-in despite superior alternatives.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cbdc_adoption_timeline, empirical, 'CBDC adoption rate threshold for SWIFT functional obsolescence').

omega_variable(
    regulatory_mandate_flexibility,
    'Can regulatory frameworks be modified to permit settlement outside SWIFT without creating financial stability risks?',
    'Regulatory impact analysis of CBDC-based settlement; jurisdictional case studies (EU, UK, Singapore) implementing RTGS alternatives; stability metrics during pilot periods',
    'If flexibly modifiable: regulatory piton is institutional choice, not structural necessity. Exit becomes ''mobile'' for banks, classification shifts from snare/piton toward rope for smaller banks. If regulatory mandate is rigid: piton persists indefinitely, exit remains ''constrained'' or ''trapped.''',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(regulatory_mandate_flexibility, empirical, 'Whether regulatory frameworks can permit non-SWIFT settlement').

omega_variable(
    switching_cost_disaggregation,
    'What portion of SWIFT exit costs are technical (system integration) vs. institutional (regulatory compliance, coordination externalities)?',
    'Cost-benefit analysis of migrating a representative mid-size bank to CBDC settlement; decomposition of IT costs, training costs, regulatory approval costs, and network adoption lag costs',
    'If technical costs >80%: problem is solvable by migration infrastructure (federated gateways, protocol converters). If institutional costs >50%: piton is entrenchment, not coordination lock-in — requires policy intervention beyond technology.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(switching_cost_disaggregation, empirical, 'Disaggregation of technical vs. institutional SWIFT switching costs').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(swift_legacy_piton, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(swift_tr_t0, swift_legacy_piton, theater_ratio, 0, 0.55).
narrative_ontology:measurement(swift_tr_t15, swift_legacy_piton, theater_ratio, 15, 0.68).
narrative_ontology:measurement(swift_tr_t30, swift_legacy_piton, theater_ratio, 30, 0.78).

% Extraction over time
narrative_ontology:measurement(swift_be_t0, swift_legacy_piton, base_extractiveness, 0, 0.12).
narrative_ontology:measurement(swift_be_t15, swift_legacy_piton, base_extractiveness, 15, 0.15).
narrative_ontology:measurement(swift_be_t30, swift_legacy_piton, base_extractiveness, 30, 0.18).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(swift_legacy_piton, information_standard).
narrative_ontology:affects_constraint(swift_legacy_piton, correspondent_banking_fragility).
narrative_ontology:affects_constraint(swift_legacy_piton, settlement_finality_uncertainty).
narrative_ontology:affects_constraint(swift_legacy_piton, regulatory_arbitrage_corridor).

% DUAL FORMULATION NOTE:
% SWIFT Legacy Piton is downstream of the correspondent banking system (constraint_id: correspondent_banking_fragility). The verification bottleneck in cross-border payment settlement creates asymmetric information that SWIFT partially resolves but at the cost of operational theater. As real-time settlement alternatives emerge, the upstream correspondent banking constraint is also decomposing into (1) residual settlement risk for CBDC-incompatible jurisdictions and (2) regulatory fragmentation across CBDC regimes. SWIFT maintains the appearance of unified coordination while the functional system decentralizes.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
