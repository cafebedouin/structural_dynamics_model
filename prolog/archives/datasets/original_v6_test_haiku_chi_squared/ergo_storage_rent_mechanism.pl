% ============================================================================
% CONSTRAINT STORY: ergo_storage_rent_mechanism
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ergo_storage_rent_mechanism, []).

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
    constraint_indexing:directionality_override/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: ergo_storage_rent_mechanism
 *   human_readable: Ergo Storage Rent (Demurrage) Mechanism
 *   domain: economic/technological
 *
 * SUMMARY:
 *   Ergo's storage rent mechanism (demurrage) represents a sophisticated
 *   attempt to solve a deep coordination problem in blockchain design: how to
 *   prevent unbounded state growth while maintaining a globally verifiable
 *   ledger. Every unspent transaction output (UTXO) on Ergo's chain carries a
 *   small annual rent (~0.14 ERG per byte per year), payable when the output
 *   is spent or automatically deducted if inactive. This creates a continuous
 *   incentive for users to either actively manage their holdings or
 *   consciously abandon them, preventing the accumulation of "dust" that
 *   would eventually render full-node operation infeasible. The mechanism
 *   operates at the intersection of protocol design, economic incentive, and
 *   user behavior, exhibiting all six DR types from different vantage points.
 *   From the perspective of dormant UTXO holders, it functions as a snare:
 *   funds erode automatically with no realistic exit except to move them
 *   (incurring transaction costs) or lose them. From the perspective of
 *   network validators and node operators, it functions as a pure
 *   coordination mechanism (rope): storage rent directly solves a commons
 *   tragedy by reducing the disk footprint everyone must maintain. From a
 *   protocol design perspective, it is an elegant solution to a mathematical
 *   inevitability — without decay, blockchain state becomes unbounded. Yet
 *   from a user-experience perspective, it creates friction that custodial
 *   services and wallet automation partially abstract away, making the
 *   constraint performative for many users. The mechanism's extractiveness
 *   has increased over time (0.28 → 0.52) as the ecosystem matured and users
 *   understood its implications, while its theater ratio has decreased (0.55
 *   → 0.35) as automated rent-payment solutions normalized the friction.
 *
 * KEY AGENTS:
 *   - Dormant UTXO Holders: Primary victims (powerless/trapped) — experience automatic erosion of holdings; no exit mechanism short of transaction cost or abandonment
 *   - Active Network Validators: Primary beneficiaries (institutional/arbitrage) — directly benefit from reduced state size and lower hardware requirements
 *   - Long-Term Cold Storage Users: Secondary victims (moderate/constrained) — must periodically move funds to service rent or accept gradual depletion
 *   - Protocol Designers: Secondary beneficiaries (powerful/arbitrage) — achieve elegant solution to state growth coordination problem
 *   - Wallet Providers and DeFi Protocols: Organized responders (organized/mobile) — implementing automated rent-payment and educational mechanisms
 *   - Custodial Services: Institutional mediators (institutional/arbitrage) — absorb rent liability, abstracting constraint from users
 *   - Analytical Observer: Civilization-scale view (analytical/analytical) — risks naturalizing a design choice as physical inevitability
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ergo_storage_rent_mechanism, 0.52).
domain_priors:suppression_score(ergo_storage_rent_mechanism, 0.48).
domain_priors:theater_ratio(ergo_storage_rent_mechanism, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ergo_storage_rent_mechanism, extractiveness, 0.52).
narrative_ontology:constraint_metric(ergo_storage_rent_mechanism, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(ergo_storage_rent_mechanism, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ergo_storage_rent_mechanism, tangled_rope).
narrative_ontology:human_readable(ergo_storage_rent_mechanism, "Ergo Storage Rent (Demurrage) Mechanism").
narrative_ontology:topic_domain(ergo_storage_rent_mechanism, "economic/technological").

domain_priors:requires_active_enforcement(ergo_storage_rent_mechanism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ergo_storage_rent_mechanism, active_network_validators).
narrative_ontology:constraint_beneficiary(ergo_storage_rent_mechanism, resource_efficient_ecosystem).
narrative_ontology:constraint_victim(ergo_storage_rent_mechanism, dormant_utxo_holders).
narrative_ontology:constraint_victim(ergo_storage_rent_mechanism, long_term_cold_storage_users).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DORMANT UTXO HOLDER (SNARE) — Individual users holding ERG in unspent outputs without activity face automatic erosion of holdings via storage rent. No exit mechanism short of moving funds (incurring transaction costs) or abandoning the constraint entirely. d≈0.92, f(d)≈1.38, σ=1.2 → χ≈0.73.
constraint_indexing:constraint_classification(ergo_storage_rent_mechanism, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: SMALL-SCALE COLD STORAGE USER (TANGLED ROPE) — Must periodically move funds to pay rent or face gradual depletion. Benefits from the network's resource optimization (lower full-node costs benefit the ecosystem they use); constrained by friction of required transaction maintenance. d≈0.68, f(d)≈1.04, σ=0.9 → χ≈0.50.
constraint_indexing:constraint_classification(ergo_storage_rent_mechanism, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: ACTIVE NETWORK NODE OPERATOR (ROPE) — Storage rent directly reduces the on-chain state size they must maintain, lowering hardware requirements and syncing times. They experience the constraint as a coordination mechanism solving the tragedy of the commons (disk space). d≈0.08, f(d)≈-0.10, σ=1.2 → χ≈-0.06.
constraint_indexing:constraint_classification(ergo_storage_rent_mechanism, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: PROTOCOL DESIGNER / LONG-TERM ECOSYSTEM (ROPE) — Storage rent is an elegant solution to a recognized coordination failure: without demurrage, blockchains accumulate unbounded state, eventually becoming unverifiable. The mechanism incentivizes network health over time horizons. d≈0.10, f(d)≈-0.08, σ=1.2 → χ≈-0.05.
constraint_indexing:constraint_classification(ergo_storage_rent_mechanism, rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: USER-SIDE ORGANIZATIONAL RESPONSE (SCAFFOLD) — Wallet providers and DeFi protocols are implementing automated rent-payment mechanisms and educational campaigns. This is a temporary coordination layer (sunset: as rent management becomes native to all wallets and becomes invisible to users, the organizational scaffolding dissolves). d≈0.35, f(d)≈0.32, σ=1.2 → χ≈0.13.
constraint_indexing:constraint_classification(ergo_storage_rent_mechanism, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: TRADITIONAL ECONOMIC INCENTIVE LOGIC (PITON) — Storage rent relies on the assumption that users will actively manage holdings or abandon them. In practice, for many users, the rent mechanism is invisible (mediated by wallet automation, managed by custodians, or simply accepted as a cost of participation). The performative element: rent is charged in theory; in practice, automation and custodial services abstract it away. theater_ratio≈0.35 is moderate but the piton classification comes from the assumption-dependence: the mechanism works only if users respond to price signals, but many users don't see the signal. d≈0.12, f(d)≈-0.06, σ=1.2 → χ≈-0.03.
constraint_indexing:constraint_classification(ergo_storage_rent_mechanism, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW (MOUNTAIN CANDIDATE) — From a universal perspective, unbounded state growth is a mathematical inevitability without some decay mechanism. Storage rent as a solution is as fundamental as conservation laws. However, the structural data (ε=0.52, suppression=0.48) contradicts true mountain classification. The 'inevitability' naturalizes what is actually a design choice among many possible solutions (sharding, pruning, state expiration without rent, etc.). The engine's false summit detector will flag this.
constraint_indexing:constraint_classification(ergo_storage_rent_mechanism, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ergo_storage_rent_mechanism_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(ergo_storage_rent_mechanism, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(ergo_storage_rent_mechanism, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(ergo_storage_rent_mechanism, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(ergo_storage_rent_mechanism, TR),
    TR >= 0.70.

:- end_tests(ergo_storage_rent_mechanism_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high, increased over time. Initial extractiveness was lower (0.28) when the rent rate was understood as theoretical; as users realized actual fund depletion, extractiveness increased to 0.42. Current level (0.52) reflects genuine extraction for non-custodial cold storage users balanced against the legitimacy of the coordination function (state cleanup is a real problem). Suppression (0.48): Moderate. Dormant UTXO holders face automatic erosion but are not coercively prevented from moving funds; the friction is real but not total. Exit options exist (spend, move, accept loss, use custodial service) but all carry costs. Theater ratio (0.35): Low-moderate. Storage rent is a direct economic mechanism without performative elements in the protocol itself; the theater arises at the user layer where automation and custodial services mediate the rent payment, making it invisible to many users. The ratio is decreasing (0.55 → 0.35) as automation matures.
 *
 * PERSPECTIVAL GAP:
 *   The core perspectival gap is between those who experience rent as an extractive burden (dormant holders, cold storage users) and those who experience it as essential coordination (validators, protocol designers). Dormant UTXO holders see a snare: automatic wealth erosion with no benefit. Active validators see a rope: their full-node costs decrease directly. The protocol designer sees this as a fundamental solution to an unbounded growth problem — a mountain, a natural law of blockchain design. Yet the analytical observer must resist this naturalization: storage rent is a design choice among many (state expiration, sharding, pruning). The false summit test applies. For custodial users, the constraint is increasingly performative (piton-like): rent is charged in protocol, but wallet automation and custodial services mediate it so thoroughly that individual users rarely experience friction. This creates a two-tier system: non-custodial users experience extraction; custodial users experience invisibility.
 *
 * DIRECTIONALITY LOGIC:
 *   Dormant UTXO holders: Victim + trapped → d≈0.92, f(d)≈1.38. Nearly maximal extraction — no realistic exit. Active validators: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.10. Net beneficiaries with active management. Cold storage users: Victim + constrained → d≈0.68, f(d)≈1.04. Significant extraction but can periodically move funds to service rent. Protocol designers: Beneficiary + arbitrage → d≈0.10, f(d)≈-0.08. They achieve their coordination goal; the mechanism works as intended from their perspective. User-side organizations (wallet providers, DeFi protocols): Organized + mobile → d≈0.35, f(d)≈0.32. They have agency and can create technical solutions (automation, batching) that reduce friction. Custodial services: Institutional + arbitrage → d≈0.12, f(d)≈-0.06. They benefit by absorbing liability and charging users fees; their structural position is similar to protocol designers. Traditional incentive logic (piton): Institutional + arbitrage → d≈0.12, f(d)≈-0.06. The logic works in theory but is increasingly mediated by abstraction layers.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by demonstrating that storage rent is genuinely a hybrid (Tangled Rope), not a false positive for either pure extraction or pure coordination. The tangled rope classification is justified: (1) Coordination function is real and substantial: storage rent solves an authentic commons tragedy (unbounded state growth). Validators genuinely experience coordination benefit. (2) Asymmetric extraction is real and substantial: dormant UTXO holders experience extraction with no coordination benefit; the wealth transfer is direct and non-consensual. (3) Active enforcement is real: the rent is enforced by protocol rules, not by voluntary participation or emergent norms. All three tangled rope gates are satisfied. The mandatrophy does not resolve by finding 'the real type' but by recognizing that the constraint successfully combines both functions: it achieves state cleanup (coordination) by imposing costs on dormant holders (extraction). The confusion arises when observers try to frame it as purely one or the other. Protocol designers naturally emphasize the coordination function (we are solving a real problem). Dormant users naturally experience the extraction (my wealth is eroding). Both are structurally correct. The constraint's long-term stability depends on maintaining this balance: if extractiveness grows toward 0.70+ (reclassifying toward Snare), the mechanism becomes delegitimized as pure rent-seeking; if coordination function fails (state bloat returns), the mechanism loses its justification. Current levels (ε=0.52, suppression=0.48) indicate a viable equilibrium where both functions remain salient.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    rent_rate_adequacy,
    'Is the storage rent rate (currently ~0.14 ERG per byte per year) adequate to incentivize state cleanup without creating unacceptable friction for legitimate users?',
    'Empirical measurement: (a) growth rate of UTXO set over time, (b) fraction of UTXOs becoming inactive vs being actively managed, (c) user churn rates correlated with rent burden, (d) comparison to actual hardware cost of state storage',
    'If rate is too low: state bloat continues (constraint fails). If rate is too high: legitimate users are driven to custodial solutions or other chains (extraction becomes excessive). Optimal rate depends on hardware cost dynamics and user base composition.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(rent_rate_adequacy, empirical, 'Whether storage rent rate balances state cleanup against user friction').

omega_variable(
    custodial_absorption,
    'What fraction of ERG holdings are mediated by custodial services, and does custodial absorption of rent liability fundamentally alter the constraint''s character?',
    'On-chain analysis of address behavior (automated patterns of rent-paying wallets vs manual); survey and documentation of major custodian policies on rent; measurement of non-custodial vs custodial UTXO distributions',
    'If custodial absorption is > 70%: most users never experience the rent constraint directly (becomes invisible, piton-like). The constraint is effectively a tax on non-custodial users and long-term developers. If < 30%: users broadly experience friction and coordination response is real.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(custodial_absorption, empirical, 'Degree to which custodial services absorb rent liability and abstract it from users').

omega_variable(
    state_expiration_alternative,
    'Could state expiration (automatic deletion of old UTXOs without rent extraction) achieve the same state-cleanup coordination goal with lower extraction?',
    'Comparative analysis of Ergo''s rent mechanism vs Tezos''s state expiration vs Cardano''s UTxO model; empirical measurement of state growth rates and user experience friction in each system; theoretical analysis of incentive structures',
    'If expiration is feasible with lower extraction: current design is a choice favoring extractive mechanism over coordination mechanism, reclassifying from Tangled Rope toward Snare. If expiration creates worse problems (loss of funds, security issues): rent is revealed as the least-extractive solution.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(state_expiration_alternative, conceptual, 'Whether state expiration could replace rent as a coordination mechanism').

omega_variable(
    network_value_distribution,
    'Who ultimately benefits from the state cleanup that storage rent enables? Is the benefit captured by validators, the ecosystem, or distributed broadly?',
    'Economic analysis: measure reduced hardware costs for full nodes; track adoption and security metrics as state growth is controlled; identify which actors'' incentives are aligned with rent collection vs state reduction',
    'If benefits accrue primarily to validators: constraint is closer to pure extraction (Snare). If benefits are network-wide (lower full-node costs, improved accessibility, security): constraint is genuinely a coordination mechanism (Rope/Tangled Rope justified).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(network_value_distribution, empirical, 'Distribution of benefits from state cleanup across network stakeholders').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ergo_storage_rent_mechanism, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(esr_tr_t0, ergo_storage_rent_mechanism, theater_ratio, 0, 0.55).
narrative_ontology:measurement(esr_tr_t3, ergo_storage_rent_mechanism, theater_ratio, 3, 0.42).
narrative_ontology:measurement(esr_tr_t6, ergo_storage_rent_mechanism, theater_ratio, 6, 0.35).

% Extraction over time
narrative_ontology:measurement(esr_be_t0, ergo_storage_rent_mechanism, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(esr_be_t3, ergo_storage_rent_mechanism, base_extractiveness, 3, 0.42).
narrative_ontology:measurement(esr_be_t6, ergo_storage_rent_mechanism, base_extractiveness, 6, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ergo_storage_rent_mechanism, resource_allocation).
narrative_ontology:boltzmann_floor_override(ergo_storage_rent_mechanism, 0.35).
narrative_ontology:affects_constraint(ergo_storage_rent_mechanism, utxo_model_scalability).
narrative_ontology:affects_constraint(ergo_storage_rent_mechanism, blockchain_state_growth).

% DUAL FORMULATION NOTE:
% Storage rent is downstream of the fundamental state growth constraint (unbounded accumulation of UTXOs) and upstream of custodial service design choices. The state growth constraint (ε≈0.15, Mountain) describes the mathematical inevitability; storage rent (ε=0.52, Tangled Rope) describes a policy solution; custodial service designs (separate constraint story) describe how the rent mechanism is mediated away from users.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(ergo_storage_rent_mechanism, institutional, 0.12).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
