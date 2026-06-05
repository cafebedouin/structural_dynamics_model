% ============================================================================
% CONSTRAINT STORY: decentralization_sovereignty_commitment
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_decentralization_sovereignty_commitment, []).

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
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: decentralization_sovereignty_commitment
 *   human_readable: Decentralization-Sovereignty Commitment in Cryptocurrency Systems
 *   domain: monetary_theory/technology_governance/political_economy
 *
 * SUMMARY:
 *   The cryptocurrency ecosystem, and Bitcoin specifically, articulates a
 *   commitment to decentralization and monetary sovereignty as alternatives
 *   to state-managed fiat systems. This commitment functions simultaneously
 *   as a coordination mechanism (solving the censorship-resistance problem
 *   for value transfer across borders), as an extraction mechanism
 *   (concentrating early-adopter wealth and governance power), and as theater
 *   (maintaining a decentralization narrative that operational reality
 *   contradicts). The constraint exhibits the full spectrum of DR
 *   classification types depending on observer position and temporal horizon.
 *   Early adopters and protocol developers see rope — the decentralization
 *   commitment is a genuine coordination solution that benefits network
 *   formation. Retail participants see snare — trapped by narrative
 *   commitment and sunk capital, bearing volatility and regulatory risk.
 *   Merchants and cross-border users see tangled rope — genuine coordination
 *   for remittances alongside extractive volatility. Regulatory authorities
 *   see tangled rope — constrained by the coordination problem of capital
 *   controls while bearing enforcement costs. Exchanges and custodians
 *   maintain piton positioning — theater decentralization commitment while
 *   operating as centralized intermediaries. The analytical observer at
 *   civilizational timescale risks seeing mountain — treating cryptographic
 *   decentralization as an immutable law of mathematics — but structural
 *   analysis reveals false summit: governance concentration and custodial
 *   dependence show that actual power is distributed differently than the
 *   commitment claims. The theater ratio has risen from 0.35 to 0.68 over the
 *   interval, indicating that the gap between decentralization narrative and
 *   operational centralization has widened as the system scaled.
 *
 * KEY AGENTS:
 *   - Early Adopters and Core Developers: Primary beneficiary (institutional/arbitrage) — capture first-mover advantage, governance token concentration, and narrative authority
 *   - Protocol Governance Token Holders: Beneficiary (institutional/arbitrage) — extract value through voting power and speculative appreciation
 *   - Retail Participants: Primary victim (powerless/trapped) — locked in by narrative commitment and sunk capital; experience maximum extraction through volatility and regulatory risk
 *   - Merchants and Cross-Border Users: Secondary victim (moderate/constrained) — benefit from censorship resistance but bear costs of price instability and regulatory arbitrage
 *   - Regulatory Authorities: Mixed (institutional/constrained) — experience externalized compliance burden while being constrained by genuine coordination problem (capital controls bypass)
 *   - Exchange and Custodian Infrastructure: Institutional actors (institutional/arbitrage) — maintain performative decentralization while operating as centralized intermediaries
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing governance concentration as immutable mathematical property
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(decentralization_sovereignty_commitment, 0.58).
domain_priors:suppression_score(decentralization_sovereignty_commitment, 0.65).
domain_priors:theater_ratio(decentralization_sovereignty_commitment, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(decentralization_sovereignty_commitment, extractiveness, 0.58).
narrative_ontology:constraint_metric(decentralization_sovereignty_commitment, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(decentralization_sovereignty_commitment, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(decentralization_sovereignty_commitment, tangled_rope).
narrative_ontology:human_readable(decentralization_sovereignty_commitment, "Decentralization-Sovereignty Commitment in Cryptocurrency Systems").
narrative_ontology:topic_domain(decentralization_sovereignty_commitment, "monetary_theory/technology_governance/political_economy").

domain_priors:requires_active_enforcement(decentralization_sovereignty_commitment).

% --- Commitment system structure ---
narrative_ontology:cs_kernel_codification(decentralization_sovereignty_commitment, distributed).
narrative_ontology:cs_authority_grounding(decentralization_sovereignty_commitment, distributed).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(decentralization_sovereignty_commitment, early_adopters_and_core_developers).
narrative_ontology:constraint_beneficiary(decentralization_sovereignty_commitment, protocol_governance_tokens_holders).
narrative_ontology:constraint_victim(decentralization_sovereignty_commitment, retail_participants).
narrative_ontology:constraint_victim(decentralization_sovereignty_commitment, regulatory_compliance_burden).
narrative_ontology:constraint_victim(decentralization_sovereignty_commitment, monetary_policy_flexibility).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: RETAIL PARTICIPANT (SNARE) — Trapped by narrative commitment to decentralization ideology while bearing asymmetric risk. Enters during bull markets with belief in sovereignty promise; locked in by sunk capital, identity fusion with the project, and liquidity constraints. Cannot exit at cost-neutral terms once locked. Suppression maintained through community pressure against 'selling out' and psychological sunk-cost binding. Maximum extraction experienced.
constraint_indexing:constraint_classification(decentralization_sovereignty_commitment, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: MERCHANT / CROSS-BORDER USER (TANGLED ROPE) — Constrained by regulatory barriers to fiat currency movement and legitimate use cases (remittances, sanctions evasion, capital controls bypass). The constraint genuinely coordinates a problem (moving value across borders without state mediation) while extracting through volatility, custodial fees, and regulatory arbitrage. Benefits from censorship resistance; bears costs of price instability and compliance uncertainty. Moderate power through organized merchant networks.
constraint_indexing:constraint_classification(decentralization_sovereignty_commitment, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: EARLY ADOPTER / PROTOCOL DEVELOPER (ROPE) — Primary beneficiary. Net beneficiary of first-mover advantage, governance token concentration, and narrative monopoly. Experiences the constraint as pure coordination: articulating the decentralization commitment attracts capital, enables network effects, and justifies governance roles. Arbitrage position preserved through continuous reinvestment in narrative maintenance. Low experienced extraction — this is their coordination mechanism.
constraint_indexing:constraint_classification(decentralization_sovereignty_commitment, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: REGULATORY AUTHORITY (TANGLED ROPE) — Constrained by genuine coordination problem (asset flows crossing borders, monetary policy transmission, financial stability). The commitment to decentralization simultaneously solves and creates problems: it enables capital controls bypass but creates regulatory arbitrage and prevents monetary policy effectiveness. Enforces regulation at high cost (surveillance, sanctions, staking rules); extraction runs both directions. Moderate institutional power but high operational burden.
constraint_indexing:constraint_classification(decentralization_sovereignty_commitment, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: EXCHANGE / CUSTODIAN (PITON) — Maintains performative decentralization commitment while operating as centralized intermediaries. The architectural claim (decentralized, censorship-resistant) conflicts with operational reality (KYC, AML, custody). Theater ratio is high: marketing emphasizes decentralization while actual transaction flow is thoroughly intermediated. Piton because the theatrical commitment has degraded from operational substance — it persists through narrative inertia despite clear centralization.
constraint_indexing:constraint_classification(decentralization_sovereignty_commitment, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, cryptographic decentralization is presented as an immutable law of mathematics: distributed ledgers cannot be censored by any single authority because consensus requires majority agreement. The commitment appears to emerge naturally from the protocol's mathematical properties. However, this naturalizes a contingent institutional choice: the protocol's governance (developer team, mining/staking pools, token holder votes) concentrates actual power despite distributed verification. The mountain classification reveals a false summit: the decentralization 'law' conceals governance concentration.
constraint_indexing:constraint_classification(decentralization_sovereignty_commitment, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(decentralization_sovereignty_commitment_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(decentralization_sovereignty_commitment, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(decentralization_sovereignty_commitment, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(decentralization_sovereignty_commitment, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(decentralization_sovereignty_commitment, TR),
    TR >= 0.70.

:- end_tests(decentralization_sovereignty_commitment_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The constraint extracts value through multiple mechanisms: (1) temporal arbitrage — early adopters benefit from price appreciation as late arrivals enter; (2) governance extraction — token holders extract through protocol parameter changes benefiting large holders; (3) volatility extraction — custodians and exchanges extract through bid-ask spreads and custody fees; (4) narrative extraction — core developers and promoters extract through authority and capital access. The baseline (0.58) reflects that extraction is significant but not totalizing — genuine coordination function (cross-border value transfer, censorship resistance) exists alongside extraction, preventing pure snare classification. The rising trajectory (0.32→0.58) indicates extraction mechanisms have intensified as the system scaled: early promise of distributed verification has faced challenges from mining pool concentration, token holder power concentration, and custodial intermediation. Suppression (0.65): High. Powerful suppression mechanisms maintain the constraint despite extractive properties: (1) identity fusion — retail participants' identity is fused with decentralization ideology, making exit cognitively difficult even when economically rational; (2) regulatory barriers — capital controls and financial crime laws create forced entry for certain use cases; (3) coordination lock-in — network effects (Metcalfe's law) create switching costs; (4) community pressure — community stigmatizes 'selling out' or questioning decentralization narrative. Theater ratio (0.68): High and rising. Significant gap between decentralization narrative and operational centralization: (1) exchanges maintain 80%+ of transaction volume despite protocol designed to enable peer-to-peer transfer; (2) mining pools concentrate 60-70% of hash rate despite protocol assuming distributed verification; (3) governance votes are dominated by large token holders and core developers despite community narrative of 'one token, one vote' democracy; (4) protocol changes are effectively decided by developer team consensus despite claims of decentralized governance. Rising theater (0.35→0.68) reflects widening gap as scale exposed hidden centralization.
 *
 * PERSPECTIVAL GAP:
 *   This constraint produces maximal perspectival divergence across the observer space. The same institutional structure (decentralized ledger, cryptographic proof-of-work/stake consensus, open governance) is experienced as pure coordination (rope) by beneficiaries, as pure extraction (snare) by powerless retail participants, as mixed hybrid (tangled rope) by merchants and regulators, as performative ritual (piton) by custodial infrastructure, and as immutable natural law (mountain) by civilizational-scope analytical observers. No two perspectives produce the same classification. This divergence is diagnostic of a constraint that operates through narrative capture and identity fusion: agents' classification of the same structure depends entirely on whether they have internalized the decentralization commitment and whether they benefit from the system's power asymmetries. The analytical observer's mountain classification is a false summit — it naturalizes the governance concentration and custodial dependence as 'inherent to distributed systems' rather than contingent institutional choices.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality of each agent is determined by their structural position relative to the extraction flow and their exit options. Early adopters and developers: Beneficiary status + arbitrage exit → d ≈ 0.05, low f(d) ≈ -0.10, negative or minimal effective extraction (rope). Retail participants: Victim status + trapped exit (identity-locked subcomponent) → d ≈ 0.95, high f(d) ≈ 1.42, maximum effective extraction (snare). Merchants: Victim status (bear volatility costs) + beneficiary status (solve censorship resistance problem) + constrained exit → d ≈ 0.60, moderate f(d) ≈ 0.75, moderate effective extraction (tangled rope). Regulators: Victim status (externalized compliance burden) + constrained exit (cannot choose not to regulate) + partial beneficiary (jurisdiction preservation) → d ≈ 0.65, f(d) ≈ 1.00, moderate effective extraction (tangled rope). Exchanges: Beneficiary status (extract intermediation rents) + arbitrage exit (can migrate to other assets) → d ≈ 0.15, low f(d) ≈ -0.01, near-zero chi masked by piton theater classification. The span of d values (0.05 to 0.95) across perspectives is unusually wide, reflecting fundamental structural asymmetry in who benefits and who bears costs.
 *
 * MANDATROPHY ANALYSIS:
 *   The decentralization-sovereignty commitment exhibits acute mandatrophy that has NOT been resolved. The extracted extraction (0.58) is high enough to trigger tangled rope candidacy, and the presence of genuine coordination function (cross-border value transfer) alongside asymmetric extraction confirms tangled rope classification. However, the puzzle is that the system is marketed and perceived by many agents as pure coordination (rope) when structural analysis reveals significant extraction (snare for retail victims). The mandatrophy resolution would require demonstrating that (1) the coordination function is genuine and non-recoverable without the extraction mechanism, AND (2) the extraction is minimized given the coordination requirement. Current evidence suggests the opposite: (1) coordination function (censorship-resistant transfer) could be achieved with lower extraction through different governance designs (less speculative incentives, wider distribution of early tokens, transparent authority structures), AND (2) extraction has intensified as the system scaled despite narrative of increasing decentralization. The mandatrophy is unresolved because the early-adopter beneficiaries have strong incentives to maintain the tangled rope classification (preserving extraction) while claiming rope classification (denying extraction). The analytical observer's false summit classification (mountain) naturalizes this by treating power concentration as inevitable rather than contingent. Until the system either (a) genuinely decentralizes governance to match narrative, or (b) explicitly acknowledges and bounds extraction levels, mandatrophy remains active.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_vs_three_constraints,
    'Is the ''decentralization-sovereignty commitment'' one contested kernel with three readings (sound-money, speculative-asset, decentralization-ideology), or three structurally distinct constraints sharing a technological substrate?',
    'Test coherence of readings under stress: if all three readings collapse simultaneously when one core property is revealed false (e.g., if censorship resistance is broken), they share a kernel. If they persist independently (e.g., speculative-asset reading survives even if decentralization is compromised), they are distinct constraints.',
    'If kernel: the constraint story should decompose into three explicit kernel readings via cs_structure, each with its own omega documenting the contested interpretation. If distinct: decompose into three separate constraint stories with distinct epsilon values and network links.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_vs_three_constraints, conceptual, 'Whether this is one contested kernel or three constraints sharing substrate').

omega_variable(
    decentralization_preservation_vs_capture,
    'Can decentralization-sovereignty commitment persist long-term without concentration in governance tokens, mining pools, or core developer teams?',
    'Longitudinal analysis of actual token distribution, mining pool consolidation, and protocol governance vote participation over 5+ year horizons. Identify whether power laws in token distribution are inherent to blockchain systems or artifacts of adoption dynamics.',
    'If inherent concentration: decentralization is theater masking de facto oligarchy (snare from powerless perspective confirmed). If contingent: better governance design could preserve commitment (tangled rope remains hybrid).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(decentralization_preservation_vs_capture, empirical, 'Whether decentralization is sustainable or inherently concentrating').

omega_variable(
    sovereignty_claim_validity,
    'Does owning cryptocurrency constitute meaningful monetary sovereignty, or is ''sovereignty'' theater covering exposure to protocol governance decisions and custodial platforms?',
    'Test under scenarios of protocol hard fork, exchange bankruptcy, regulatory seizure, and validator collusion. Measure actual agent control over their assets in each scenario vs rhetoric of ''not your keys, not your coins''.',
    'If valid sovereignty: the coordination function is genuine (rope aspects predominate). If theater: sovereignty promise masks dependence on intermediaries (piton aspects predominate).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sovereignty_claim_validity, empirical, 'Whether monetary sovereignty is real or performative').

omega_variable(
    retail_identity_fusion,
    'To what extent is a retail holder''s attachment to a specific cryptocurrency rooted in belief in the decentralization commitment vs. sunk capital and speculation?',
    'Behavioral studies comparing narrative commitment vs financial incentive: do holders maintain positions after disproof of specific decentralization claims? Do they exit during price crashes despite ideological commitment?',
    'If commitment is genuine (identity fusion): exit_options should be identity_locked rather than trapped, changing classification. If capital-driven (sunk-cost psychology): trapped classification is correct.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(retail_identity_fusion, empirical, 'Whether retail attachment is identity-based or capital-based').

omega_variable(
    consensus_mechanism_extractiveness,
    'Do proof-of-work and proof-of-stake consensus mechanisms represent genuine coordination overhead or are they extractive rents captured by miners/validators?',
    'Compare consensus costs to actual transaction security requirements; measure whether cost is minimized or inflated to justify extraction. Test via simulated consensus mechanisms with lower overhead.',
    'If coordination overhead: extractiveness is lower (rope aspects stronger). If extractive rent: base_extractiveness should be 0.65+ (snare aspects stronger).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(consensus_mechanism_extractiveness, empirical, 'Whether consensus mechanisms coordinate or extract').

omega_variable(
    regulatory_arbitrage_externality,
    'Does cryptocurrency''s cross-border frictionlessness represent genuine coordination benefit (capital controls bypass) or externalize costs (financial crime, sanctions evasion, monetary policy failure) to regulatory authorities?',
    'Cost-benefit analysis: measure regulatory authority''s burden (compliance monitoring, sanctions enforcement, financial stability risk) vs merchant/user benefit (remittance cost reduction, capital access). Identify who bears the externality.',
    'If externalized: victims list is correct (regulatory_compliance_burden), suppression is justified. If internalized: victims should be reframed, suppression reduced.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(regulatory_arbitrage_externality, empirical, 'Whether cross-border frictionlessness is net-benefit or cost-externalization').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(decentralization_sovereignty_commitment, 0, 8).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(decsov_tr_t0, decentralization_sovereignty_commitment, theater_ratio, 0, 0.35).
narrative_ontology:measurement(decsov_tr_t2, decentralization_sovereignty_commitment, theater_ratio, 2, 0.48).
narrative_ontology:measurement(decsov_tr_t5, decentralization_sovereignty_commitment, theater_ratio, 5, 0.62).
narrative_ontology:measurement(decsov_tr_t8, decentralization_sovereignty_commitment, theater_ratio, 8, 0.68).

% Extraction over time
narrative_ontology:measurement(decsov_be_t0, decentralization_sovereignty_commitment, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(decsov_be_t2, decentralization_sovereignty_commitment, base_extractiveness, 2, 0.42).
narrative_ontology:measurement(decsov_be_t5, decentralization_sovereignty_commitment, base_extractiveness, 5, 0.54).
narrative_ontology:measurement(decsov_be_t8, decentralization_sovereignty_commitment, base_extractiveness, 8, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(decentralization_sovereignty_commitment, resource_allocation).
narrative_ontology:boltzmann_floor_override(decentralization_sovereignty_commitment, 0.2).
narrative_ontology:affects_constraint(decentralization_sovereignty_commitment, regulatory_arbitrage_capital_controls).
narrative_ontology:affects_constraint(decentralization_sovereignty_commitment, fiat_currency_seigniorage_extraction).
narrative_ontology:affects_constraint(decentralization_sovereignty_commitment, token_holder_governance_power).
narrative_ontology:affects_constraint(decentralization_sovereignty_commitment, mining_pool_centralization).

% DUAL FORMULATION NOTE:
% The decentralization-sovereignty commitment decomposes into three structurally distinct constraints sharing a technological kernel: (1) sound_money_constraint — fixed supply as inflation hedge (epsilon ~0.15, rope) links upstream to this constraint's beneficiary narrative; (2) speculative_asset_constraint — price appreciation vehicle (epsilon ~0.68, snare for retail victims) links as sibling constraint with different extraction mechanism; (3) this constraint — governance decentralization and censorship resistance (epsilon 0.58, tangled rope) represents the dominant narrative framing. All three readings are activated by the same blockchain kernel but produce different classifications because they emphasize different structural elements. This story instantiates the decentralization-ideology reading exclusively; sibling stories instantiate sound-money and speculative readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(decentralization_sovereignty_commitment, moderate, 0.62).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
