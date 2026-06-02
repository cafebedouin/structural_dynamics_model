% ============================================================================
% CONSTRAINT STORY: cryptocurrency_user_asset_security
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_cryptocurrency_user_asset_security, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: cryptocurrency_user_asset_security
 *   human_readable: Cryptocurrency User Asset Security and Key Management Constraint
 *   domain: financial/technology/digital_asset_custody
 *
 * SUMMARY:
 *   Cryptocurrency asset security presents a fundamental structural tension
 *   between decentralization's irreversibility guarantee and users' need for
 *   protection against theft, loss, and human error. The constraint emerges
 *   from the architectural requirement that blockchain transactions be
 *   irreversible (necessary for decentralization and consensus finality)
 *   colliding with the reality that users — especially retail participants —
 *   lack the technical sophistication to manage cryptographic keys safely.
 *   This creates a structural extraction opportunity: platforms and custody
 *   providers capture value by offering convenience and (limited) security
 *   while users bear the full cost of key loss or theft. The constraint
 *   exhibits all six types from different perspectives. Retail users
 *   experience it as a pure snare: irreversible asset loss with no recovery
 *   path. Institutional actors experience it as manageable coordination: they
 *   can hire security expertise and arrange insurance. Regulators experience
 *   it as theater: KYC and reserves attestation create the appearance of
 *   security assurance without preventing the primary harm vectors (private
 *   key theft, user error). Platforms experience it as hybrid
 *   coordination-extraction: they provide genuine trading liquidity and
 *   security infrastructure (coordination) while simultaneously extracting
 *   through platform lock-in and asymmetric custody risk (extraction). The
 *   constraint's theater ratio (0.65) reflects that regulatory security
 *   theater (KYC, compliance attestation) is increasing faster than actual
 *   user security protection.
 *
 * KEY AGENTS:
 *   - Retail Cryptocurrency Users: Primary victim (powerless/trapped) — bear full cost of irreversible asset loss; no recovery mechanisms; no insurance
 *   - Security-Conscious Users: Secondary agent (moderate/constrained) — invest in hardware wallets and security practices; benefit from coordination but bear responsibility for key management
 *   - Exchange Platforms: Primary beneficiary (institutional/arbitrage) — extract transaction fees, trading volume, and data value; provide coordination through consolidated security and liquidity
 *   - Institutional Custody Providers: Secondary beneficiary (organized/mobile) — offer insurable custody with exit options; competitive market discipline
 *   - Security Software Vendors: Beneficiary (institutional/arbitrage) — extract value through hardware wallet sales, key management tools, and security services
 *   - Regulatory Framework: Secondary actor (institutional/constrained) — applies traditional financial regulations; creates theater without addressing primary harms
 *   - Analytical Observer: Universal view (analytical/analytical) — risks naturalizing irreversibility as an immutable property rather than recognizing it as an engineered tradeoff
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(cryptocurrency_user_asset_security, 0.58).
domain_priors:suppression_score(cryptocurrency_user_asset_security, 0.72).
domain_priors:theater_ratio(cryptocurrency_user_asset_security, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(cryptocurrency_user_asset_security, extractiveness, 0.58).
narrative_ontology:constraint_metric(cryptocurrency_user_asset_security, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(cryptocurrency_user_asset_security, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(cryptocurrency_user_asset_security, tangled_rope).
narrative_ontology:human_readable(cryptocurrency_user_asset_security, "Cryptocurrency User Asset Security and Key Management Constraint").
narrative_ontology:topic_domain(cryptocurrency_user_asset_security, "financial/technology/digital_asset_custody").

domain_priors:requires_active_enforcement(cryptocurrency_user_asset_security).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(cryptocurrency_user_asset_security, exchange_platforms).
narrative_ontology:constraint_beneficiary(cryptocurrency_user_asset_security, custody_service_providers).
narrative_ontology:constraint_beneficiary(cryptocurrency_user_asset_security, security_software_vendors).
narrative_ontology:constraint_victim(cryptocurrency_user_asset_security, retail_cryptocurrency_users).
narrative_ontology:constraint_victim(cryptocurrency_user_asset_security, asset_recovery_capability).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: RETAIL USER (SNARE) — Users face irreversible asset loss from stolen keys or lost recovery phrases with zero recovery mechanism. No institutional safety nets, no insurance, no customer protection. Trapped by the irreversibility of blockchain transactions and the technical complexity of self-custody. Extraction flows entirely toward users — they bear all loss risk while platforms extract transaction fees and data value.
constraint_indexing:constraint_classification(cryptocurrency_user_asset_security, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: SECURITY-CONSCIOUS USER (TANGLED ROPE) — Users who invest in hardware wallets and security practices benefit from reduced theft risk (coordination function) while still bearing the full cost of key loss or human error. Constrained by the requirement to manage security themselves; also benefit from the security ecosystem's coordination around standards. Mixed experience — genuine coordination value but asymmetric extraction of the control burden.
constraint_indexing:constraint_classification(cryptocurrency_user_asset_security, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: INSTITUTIONAL CUSTODY (ROPE) — Large institutional investors can organize alternatives: multi-signature custody, insurance arrangements, regulatory oversight. Mobile exit options — can choose between custodians or migrate to blockchain systems with built-in security features. Experience the constraint as pure coordination: delegated security with insurable risk. Low extraction because institutional actors have agency.
constraint_indexing:constraint_classification(cryptocurrency_user_asset_security, rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 4: EXCHANGE PLATFORM (TANGLED ROPE) — Platforms benefit from user deposits (access to transaction fees, trading volume, and investment in infrastructure). They provide genuine coordination (consolidated security, user interface, trading liquidity) while simultaneously extracting through platform-specific lock-in, custody risk concentration, and asymmetric fee structures. Active enforcement of user dependency through terms of service and technical architecture.
constraint_indexing:constraint_classification(cryptocurrency_user_asset_security, tangled_rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: REGULATORY FRAMEWORK (PITON) — Securities regulations, KYC/AML requirements, and custody standards were designed for traditional finance assets. Applied to cryptocurrency, they create theater: compliance activities (KYC, reserves attestation, wallet whitelisting) that perform security assurance without directly preventing the key loss or theft that actually harms users. Regulatory regime persists through institutional inertia despite low functional protection for the primary harm vector.
constraint_indexing:constraint_classification(cryptocurrency_user_asset_security, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / IMMUTABILITY VIEW (MOUNTAIN) — From a cryptographic/physical perspective, blockchain's core feature is irreversible transaction finality — no authority can reverse a confirmed transaction. This immutability is foundational to decentralization but makes user error or key theft permanently destructive. This perspective risks naturalizing the architecture as immutable law rather than recognizing it as an engineered tradeoff. The mountain classification here is a false summit: irreversibility is a choice of system design, not a law of nature.
constraint_indexing:constraint_classification(cryptocurrency_user_asset_security, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(cryptocurrency_user_asset_security_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(cryptocurrency_user_asset_security, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(cryptocurrency_user_asset_security, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(cryptocurrency_user_asset_security, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(cryptocurrency_user_asset_security, TR),
    TR >= 0.70.

:- end_tests(cryptocurrency_user_asset_security_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high, increasing over time. The base extraction reflects the asymmetry between platform/vendor benefits (transaction fees, trading volume, data, security tool sales) and user costs (irreversible loss exposure, key management burden, limited recovery options). The trajectory shows increasing extractiveness as platforms consolidate market share and users accumulate assets on centralized exchanges — the constraint's extraction mechanism becomes more valuable as the trapped user base grows. At time 0 (early cryptocurrency era), users were more aware of self-custody risks and held smaller balances; by time 8 (current period), retail participation has grown without corresponding security literacy, increasing the extraction gradient. Suppression (0.72): High and stable. Multiple barriers prevent users from exiting: technical complexity of self-custody, platform lock-in through trading liquidity and user interface, regulatory uncertainty around decentralized custodians, lack of practical recovery mechanisms for lost keys, and asymmetric information (users unaware of custody risks until loss occurs). Theater ratio (0.65): Moderate-high and increasing. Regulatory security theater (KYC verification, platform reserves attestation, wallet whitelisting) creates the appearance of security assurance without addressing the primary harm vector — private key loss and theft. As regulation increases, theater rises while actual user protection remains low. The theater serves regulators (demonstrating oversight) and platforms (demonstrating security consciousness) more than users.
 *
 * PERSPECTIVAL GAP:
 *   This constraint shows maximum perspectival divergence between trapped and mobile agents. The retail user (powerless/trapped/global) sees a snare — permanent asset loss with no appeal. The platform (institutional/arbitrage/global) sees a rope — efficient coordination of trading and custody. The regulatory framework (institutional/constrained/national) sees theater — compliance activities that satisfy audit trails without preventing losses. Institutional custody providers (organized/mobile/global) see manageable tangled rope because they can exit platform lock-in and have insurance options. The falsest summit is the cryptographic immutability mountain: irreversibility is often presented as inherent to blockchain when it is actually a design choice. Systems like Ethereum smart contracts already implement reversibility through upgradeable contracts and multi-sig controls; some layer-2 systems provide transaction rollback windows. The mountain classification falsely naturalizes a contingent architecture choice.
 *
 * DIRECTIONALITY LOGIC:
 *   The extraction gradient concentrates on powerless agents (retail users) because they occupy the weakest position in exit capacity. A retail user with 10,000 USD on an exchange faces trapped circumstances: accessing the asset requires holding custody through the platform (lock-in), moving it to self-custody requires technical expertise (barrier), and losing the key results in permanent loss (irreversibility). The user's d value is near 1.0 (full victim). An institutional actor with the same balance at the same platform has d near 0.2: they can hire security expertise, arrange custody insurance, or reallocate to alternatives. The platforms and vendors have d near 0.0 to negative: they benefit from every transaction and from the general ecosystem growth driven by user adoption. The exchange platform's institutional actor status is critical — it provides the arbitrage option (exit to competitors or decentralized trading) that gives it low d despite beneficiary status. The constraint's extraction mechanism relies on the gap: platforms can offer convenience at the cost of irreversible loss risk because users lack the exit capacity to refuse.
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLUTION OF MANDATROPHY: This constraint resolves by recognizing that extraction and coordination are genuinely both present in the same structure. The platforms provide real coordination (consolidated security, trading liquidity, user interface, regulatory compliance) — users genuinely benefit from access to these services. Simultaneously, the platforms extract through lock-in (users cannot easily move assets to competitors), through asymmetric custody risk (platforms control key management but don't fully bear asset loss), and through opacity (users are often unaware of their actual custody exposure). The tangled rope classification is the correct synthesis: the constraint cannot be reduced to pure coordination (rope) because the extraction is real and asymmetric; cannot be reduced to pure extraction (snare) because users genuinely benefit from the coordination function. The theater ratio indicates that regulatory attempts to resolve mandatrophy through security theater (KYC, reserves attestation) are increasing — creating the appearance of safety without addressing the core mechanism. True resolution would require either (a) engineering mechanisms for key recovery that don't compromise decentralization (omega variable 3), (b) insurable custody that socializes losses (omega variable 2), or (c) user adoption of distributed custody alternatives that reduce platform dependence (omega variable 4). The current trajectory shows theater increasing (ratio 0.48 → 0.65) while extraction also increases (0.42 → 0.58), indicating that mandatrophy is being masked rather than resolved.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    self_custody_vs_delegated_tradeoff,
    'Is the extraction mechanism inherent to decentralization (self-custody irreversibility) or contingent to current custody architecture (platform lock-in + risk concentration)?',
    'Comparative analysis of security outcomes across custody models: self-custody loss rates vs institutional custody breach costs vs hybrid multi-signature arrangements. Measurement of actual asset recovery rates by custody type.',
    'If inherent: security constraint is a mountain-like tradeoff unavoidable in decentralized systems. If contingent: current architecture unnecessarily concentrates risk and extraction, and alternative designs (threshold signatures, social recovery, time-locked reversibility) could reduce extraction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(self_custody_vs_delegated_tradeoff, empirical, 'Whether extraction is inherent to decentralization or contingent to custody design').

omega_variable(
    insurance_feasibility_ceiling,
    'Can insurance or bonding mechanisms scale to cover cryptocurrency asset theft/loss for retail users, or are loss rates inherently uninsurable at current scale?',
    'Actuarial analysis: historical loss rates by category (private key theft, user error, platform breach, network-level attacks); comparison to insurable loss categories in traditional finance; assessment of risk concentration in small custodian pool.',
    'If feasible: insurance could transform the snare into a rope (coordinated risk transfer). If uninsurable: the security constraint remains extractive because the risk cannot be socialized or transferred, concentrating on users.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(insurance_feasibility_ceiling, empirical, 'Feasibility of insurance for cryptocurrency asset loss').

omega_variable(
    key_recovery_mechanism_design,
    'Can social recovery, time-locked reversibility, or other recovery mechanisms be implemented without compromising the security properties users rely on?',
    'Security analysis of proposed recovery mechanisms against canonical attack vectors (private key theft, account compromise, transaction malleability). Measurement of security parameter degradation in hybrid recovery designs.',
    'If recoverable: recovery mechanisms could reduce trapped-user extraction by enabling legitimate reversal of user error/theft while maintaining security against consensus-level attacks. If unrecoverable: architecture remains locked into permanent irreversibility.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(key_recovery_mechanism_design, empirical, 'Design feasibility of reversible transaction mechanisms').

omega_variable(
    platform_custody_concentration_necessity,
    'Is the concentration of retail user assets on centralized platforms a necessary coordination efficiency, or a lock-in mechanism that could be replaced by distributed custody alternatives?',
    'Adoption metrics of non-custodial solutions (hardware wallets, self-sovereign identity systems, decentralized exchanges); user retention correlation with custody options; transaction cost comparison between platform trading and decentralized alternatives.',
    'If necessary: platform custody is a rope-like coordination tradeoff. If lock-in: it''s an extractive snare mechanism, and adoption of distributed custody alternatives would reduce extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(platform_custody_concentration_necessity, empirical, 'Whether platform custody concentration is necessary or contingent').

omega_variable(
    technical_literacy_barrier_malleability,
    'Is the high technical barrier to secure self-custody an immutable property of public key cryptography, or a contingent feature of current UI/UX design that could be lowered through better tools?',
    'Comparative study of security outcomes in systems with different UI/UX complexity: bare key management vs hardware wallets vs smart contract wallets with social recovery. Measurement of user error rates and loss attribution by system complexity.',
    'If immutable: users will always face high cognitive load managing keys, justifying platform delegation. If contingent: better tools could enable self-custody security at lower cognitive cost, reducing platform lock-in.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(technical_literacy_barrier_malleability, empirical, 'Malleability of technical barriers to self-custody security').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(cryptocurrency_user_asset_security, 0, 8).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(crypto_sec_tr_t0, cryptocurrency_user_asset_security, theater_ratio, 0, 0.48).
narrative_ontology:measurement(crypto_sec_tr_t4, cryptocurrency_user_asset_security, theater_ratio, 4, 0.58).
narrative_ontology:measurement(crypto_sec_tr_t8, cryptocurrency_user_asset_security, theater_ratio, 8, 0.65).

% Extraction over time
narrative_ontology:measurement(crypto_sec_be_t0, cryptocurrency_user_asset_security, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(crypto_sec_be_t4, cryptocurrency_user_asset_security, base_extractiveness, 4, 0.51).
narrative_ontology:measurement(crypto_sec_be_t8, cryptocurrency_user_asset_security, base_extractiveness, 8, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(cryptocurrency_user_asset_security, resource_allocation).
narrative_ontology:affects_constraint(cryptocurrency_user_asset_security, cryptocurrency_exchange_systemic_risk).
narrative_ontology:affects_constraint(cryptocurrency_user_asset_security, decentralized_finance_smart_contract_security).
narrative_ontology:affects_constraint(cryptocurrency_user_asset_security, regulatory_custody_standards_divergence).

% DUAL FORMULATION NOTE:
% User asset security decomposes into distinct constraints: (1) private key management and self-custody irreversibility (ε≈0.65, snare/tangled rope), (2) platform custody and counterparty risk (ε≈0.48, tangled rope), (3) regulatory custody standards (ε≈0.35, piton). Each has different base extraction metrics but they share the underlying tension between irreversibility guarantees and user protection requirements. This story focuses on the integrated user experience constraint; sister stories address platform systemic risk and regulatory arbitrage separately.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(cryptocurrency_user_asset_security, institutional, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
