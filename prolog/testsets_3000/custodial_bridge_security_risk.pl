% ============================================================================
% CONSTRAINT STORY: custodial_bridge_security_risk
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_custodial_bridge_security_risk, []).

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
 *   constraint_id: custodial_bridge_security_risk
 *   human_readable: Custodial Bridge Security Risk in Cross-Chain Asset Transfer
 *   domain: blockchain/cryptocurrency/systemic_risk
 *
 * SUMMARY:
 *   Custodial bridge security risk emerges from the structural need to
 *   transfer assets across blockchain architectures that cannot natively
 *   interoperate. The constraint creates an extraction mechanism where bridge
 *   operators and institutional custodians capture asymmetric upside
 *   (transaction fees, operational control) while distributing downside risk
 *   (exploit exposure, contagion risk) to retail users, liquidity providers,
 *   and the broader ecosystem. The constraint exhibits characteristics of all
 *   major types depending on observer position: pure extraction (snare) from
 *   the retail user perspective; coordination failure with security theater
 *   (piton) from the institutional perspective; mixed coordination and
 *   tail-risk extraction (tangled_rope) from liquidity providers; and an
 *   emerging sunset pathway (scaffold) through decentralized alternatives.
 *   Extractiveness has increased over the interval (0.45 to 0.68) as bridges
 *   have accumulated larger value locks and exploit risk has materialized
 *   empirically (Nomad $190M, Ronin $625M, Poly Network $611M). Theater ratio
 *   (0.58) reflects that security audits and insurance products provide
 *   performative assurance but do not address fundamental architectural
 *   constraints.
 *
 * KEY AGENTS:
 *   - Retail Users: Primary victims (powerless/trapped) — depend on bridges for cross-chain liquidity but cannot audit or enforce security. Bear exploit risk asymmetrically.
 *   - Liquidity Providers: Secondary victims (moderate/constrained) — receive yield compensation but tail-risk exposure from bridge exploits exceeds compensation magnitude. Some exit options (withdrawal) but with slippage cost.
 *   - Bridge Operators: Primary beneficiaries (institutional/arbitrage) — capture transaction fees and maintain operational control. Can exit or upgrade infrastructure at will.
 *   - Institutional Custodians: Beneficiaries (institutional/arbitrage) — control asset movement and benefit from user dependency and fee capture.
 *   - Ecosystem Security: Systemic victim (powerless/trapped) — bridge exploits create contagion and confidence loss. No exit, no audit, no control.
 *   - Security Auditors and Insurers: Theater maintainers (institutional/arbitrage) — create performative assurance that users and regulators demand but that doesn't reduce actual exploit probability.
 *   - Decentralized Bridge Projects: Organized challengers (organized/constrained) — building alternative architectures with distributed verification. Have exit pathway but face technical and liquidity hurdles.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(custodial_bridge_security_risk, 0.68).
domain_priors:suppression_score(custodial_bridge_security_risk, 0.72).
domain_priors:theater_ratio(custodial_bridge_security_risk, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(custodial_bridge_security_risk, extractiveness, 0.68).
narrative_ontology:constraint_metric(custodial_bridge_security_risk, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(custodial_bridge_security_risk, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(custodial_bridge_security_risk, snare).
narrative_ontology:human_readable(custodial_bridge_security_risk, "Custodial Bridge Security Risk in Cross-Chain Asset Transfer").
narrative_ontology:topic_domain(custodial_bridge_security_risk, "blockchain/cryptocurrency/systemic_risk").

domain_priors:requires_active_enforcement(custodial_bridge_security_risk).
% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(custodial_bridge_security_risk, bridge_operators).
narrative_ontology:constraint_beneficiary(custodial_bridge_security_risk, institutional_custodians).
narrative_ontology:constraint_victim(custodial_bridge_security_risk, retail_users).
narrative_ontology:constraint_victim(custodial_bridge_security_risk, liquidity_providers).
narrative_ontology:constraint_victim(custodial_bridge_security_risk, ecosystem_security).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: RETAIL USER (SNARE) — User needs cross-chain liquidity to access DeFi opportunities but faces centralized custody and security risks they cannot audit or escape. No alternative paths with equivalent liquidity. Suppression is total: users accept the risk or forfeit access. Maximum extraction — the user's assets are temporarily held by bridge operators whose security practices are opaque and uninsurable.
constraint_indexing:constraint_classification(custodial_bridge_security_risk, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: LIQUIDITY PROVIDER (TANGLED ROPE) — Provides liquidity that enables the bridge to function (coordination benefit) but bears tail-risk exposure to bridge exploits. Can exit by withdrawing liquidity, but faces slippage penalties and lost fee income. Receives yield compensation (coordination benefit) but insufficient to cover variance of catastrophic loss. Suppression moderately high: concentrated risk on a small group of sophisticated actors who have some agency but constrained exit options.
constraint_indexing:constraint_classification(custodial_bridge_security_risk, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 3: BRIDGE OPERATOR (ROPE) — Solves a genuine coordination problem: enabling asset transfer across incompatible chain architectures. Captures transaction fees and operational control. Can exit or upgrade infrastructure at will. Suppression is low — operators face business risk but not existential constraint. Security risk is borne asymmetrically downward to users and liquidity providers.
constraint_indexing:constraint_classification(custodial_bridge_security_risk, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: ECOSYSTEM SECURITY (SNARE) — Bridge exploits create cascading failures across ecosystems (contagion risk, loss of confidence in custody, regulatory response). The ecosystem cannot audit or enforce bridge security. Trapped in dependency on third-party custodial arrangements. No exit option: users must transact across chains, and no bridge means ecosystem fragmentation. Pure extraction with no coordination benefit at the systemic level.
constraint_indexing:constraint_classification(custodial_bridge_security_risk, snare,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 5: SECURITY THEATER (PITON) — Crypto insurance products and security audits create performative assurance that doesn't fundamentally reduce bridge exploit risk. Insurance claims are often disputed or unpaid after exploits. Security audits find only audited code paths. Theater ratio is moderate-high (0.58) because the security mechanisms (audits, insurance) provide psychological reassurance but don't address the fundamental structural risk: custodial bridges require trust in operator infrastructure and protocol design. The theater persists because alternatives (fully decentralized bridges, atomic swaps) are incomplete or unavailable.
constraint_indexing:constraint_classification(custodial_bridge_security_risk, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: DECENTRALIZED BRIDGE PROTOCOL (SCAFFOLD) — Emerging cross-chain messaging protocols (IBC, LayerZero) with distributed validator sets aim to reduce custodial risk through decentralization. Lower effective extraction because organized actors see an exit pathway. Suppression declining as alternatives mature. Has sunset clause: as decentralized bridges achieve sufficient liquidity and functionality, the custodial bridge's extraction mechanism loses force. Estimated sunset: 5-10 years as alternative architectures mature.
constraint_indexing:constraint_classification(custodial_bridge_security_risk, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (MOUNTAIN) — From a civilizational view, custodial cross-chain transfer might appear immutable: blockchain isolation is a fundamental technical constraint, and custody requires trust. However, the base properties (extractiveness 0.68, suppression 0.72) reveal this as naturalization of a contingent institutional choice (custodial vs. non-custodial architecture). The engine flags this as a false summit: the technical constraint (chain isolation) is real, but the custodial solution is one of several architectures, not inevitable.
constraint_indexing:constraint_classification(custodial_bridge_security_risk, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(custodial_bridge_security_risk_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(custodial_bridge_security_risk, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(custodial_bridge_security_risk, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(custodial_bridge_security_risk, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(custodial_bridge_security_risk, TR),
    TR >= 0.70.

:- end_tests(custodial_bridge_security_risk_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. The bridge operator captures transaction fees (users have no alternative at comparable liquidity), benefits from operational control, and distributes exploit risk downward. Users must accept the risk or forfeit cross-chain access. The extractiveness trajectory (0.45 to 0.68 over interval) reflects that as total value locked in bridges has increased, exploit probability has materialized empirically, making the extraction mechanism more visible. Suppression (0.72): High. Users face informational barriers (can't audit bridge code or operator infrastructure), resource barriers (no alternative bridges with comparable liquidity), and exit barriers (withdrawal costs slippage and lost opportunity). Retail users are individually powerless and cannot coordinate. Liquidity providers face penalty for withdrawal (impermanent loss, slippage). Theater ratio (0.58): Moderate-high. Security audits provide assurance but don't detect all vulnerability classes (recent exploits occurred in audited code). Insurance products are marketed aggressively but actual claims are often disputed or paid at haircut. The theater is more developed than it was (0.35 to 0.58 trajectory) but still does not address the fundamental architectural constraint that bridging requires custody or distributed validator trust.
 *
 * PERSPECTIVAL GAP:
 *   The retail user sees pure extraction (snare) because their alternatives are limited and they bear full exploit risk without compensation. The liquidity provider sees mixed coordination and extraction (tangled_rope) because they do receive fee income but face tail-risk exposure that ordinary market pricing doesn't reflect. The bridge operator sees pure coordination (rope) — they are solving the real problem of cross-chain asset transfer and capture appropriate revenue. The ecosystem sees pure extraction (snare) at the systemic level because bridge exploits create contagion and loss of confidence with no corresponding systemic benefit. The analytical observer at civilizational scale risks seeing custody as inevitable (mountain) but the structural data reveals this as naturalization: decentralized alternatives exist, and the custodial choice is one of several architectural paths. The piton perspective reveals that security theater (audits, insurance) serves to maintain the custodial bridge ecosystem despite its risk profile by creating psychological assurance that substitutes for actual risk reduction.
 *
 * DIRECTIONALITY LOGIC:
 *   Retail users: Powerless + trapped → high d (0.85-0.95) → maximum experienced extraction chi. They face material barriers to exit (no alternative bridges) and informational barriers (can't evaluate bridge security). Bridge operators: Institutional + arbitrage → low d (0.05-0.15) → negative/minimal extraction chi. They have full exit options and benefit from the constraint. Liquidity providers: Moderate + constrained → medium d (0.55-0.65) → moderate chi. They have some exit options but face significant penalty. The asymmetry in directionality values reflects the structural extraction: the beneficiary experiences minimal extraction (even negative), while victims experience high extraction. Ecosystem security cannot be assigned a standard power atom because it is a collective good, not an agent — it is modeled as a snare victim group with powerless/trapped directionality.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLVED: The constraint exhibits extractive asymmetry (beneficiaries: bridge operators, institutional custodians; victims: retail users, liquidity providers, ecosystem security) that is not compensated by coordination benefit at the victim level. The retail user perspective is pure snare (no coordination benefit — they need to transact and have no alternative). The liquidity provider perspective is tangled rope (some coordination benefit in fee income, but insufficient to compensate for tail-risk exposure). The ecosystem perspective is pure snare (pure extraction with cascading contagion risk). The bridge operator perspective is rope (genuine coordination benefit — enables cross-chain transfer). This is not ambiguous: the constraint extracts asymmetrically. The snare classification is validated by the victim perspectives and the high suppression (0.72). The scaffold and piton perspectives confirm that decentralized alternatives exist (sunset) and security theater exists (performative assurance), both of which are features of high-extraction constraints seeking to maintain themselves against alternatives.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    exploit_probability_threshold,
    'At what annual exploit probability does the bridge transition from systemic risk to systemic failure?',
    'Empirical tracking of bridge exploits, near-misses, and vulnerability disclosures; correlation with liquidity withdrawal rates and ecosystem contagion magnitude',
    'If threshold < 1%: current custodial bridges exceed systemic risk tolerance. If threshold > 5%: current losses are within ''acceptable'' tail risk (naturalizing extraction). Determines whether the snare classification holds or whether suppression should be downgraded.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(exploit_probability_threshold, empirical, 'Exploit probability threshold for systemic failure').

omega_variable(
    insurance_mechanism_adequacy,
    'Do insurance products and security audits materially reduce bridge exploit risk or merely provide unenforceable assurance?',
    'Post-exploit analysis of insurance claims, audit follow-up on discovered vulnerabilities, correlation between audit coverage and actual exploit vectors',
    'If materially reducing: suppression should be downgraded, theater_ratio lowered. If purely performative: the piton classification is validated, and security theater is the constraint''s primary function (not risk reduction).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(insurance_mechanism_adequacy, empirical, 'Whether insurance and audits reduce actual exploit risk').

omega_variable(
    decentralized_bridge_viability,
    'Can decentralized bridge architectures (IBC, LayerZero, distributed validators) achieve sufficient liquidity and functionality to replace custodial bridges?',
    'Comparative analysis of decentralized bridge total value locked, transaction volume, user satisfaction, and technical failure rates vs. custodial bridges; timeline to feature parity',
    'If viable: scaffold perspective is structural, and the custodial bridge''s sunset is real. If not viable: the decentralized alternative remains aspirational, and suppression remains high because no real exit path exists.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(decentralized_bridge_viability, empirical, 'Viability of decentralized bridge alternatives').

omega_variable(
    custody_vs_non_custody_false_binary,
    'Is the custody/non-custody distinction meaningful, or do all bridges require some form of trust assumption?',
    'Technical analysis of alternative architectures: identify trust assumptions in each (validator sets, token holders, protocol designers). Classify by trust distribution rather than custody label.',
    'If false binary: the constraint is not ''custody'' but ''verification cost'' — whoever bears verification burden bears extraction. Suppression mechanism is universal, not specific to custodial design. Classification may shift from snare to tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(custody_vs_non_custody_false_binary, conceptual, 'Whether custody vs. non-custody is a meaningful distinction').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(custodial_bridge_security_risk, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(custbridge_tr_t0, custodial_bridge_security_risk, theater_ratio, 0, 0.35).
narrative_ontology:measurement(custbridge_tr_t2, custodial_bridge_security_risk, theater_ratio, 2, 0.42).
narrative_ontology:measurement(custbridge_tr_t4, custodial_bridge_security_risk, theater_ratio, 4, 0.5).
narrative_ontology:measurement(custbridge_tr_t6, custodial_bridge_security_risk, theater_ratio, 6, 0.58).

% Extraction over time
narrative_ontology:measurement(custbridge_be_t0, custodial_bridge_security_risk, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(custbridge_be_t2, custodial_bridge_security_risk, base_extractiveness, 2, 0.52).
narrative_ontology:measurement(custbridge_be_t4, custodial_bridge_security_risk, base_extractiveness, 4, 0.6).
narrative_ontology:measurement(custbridge_be_t6, custodial_bridge_security_risk, base_extractiveness, 6, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(custodial_bridge_security_risk, resource_allocation).
narrative_ontology:affects_constraint(custodial_bridge_security_risk, cross_chain_atomic_swap_technical_barrier).
narrative_ontology:affects_constraint(custodial_bridge_security_risk, decentralized_validator_collusion_risk).
narrative_ontology:affects_constraint(custodial_bridge_security_risk, regulatory_custody_oversight_gap).

% DUAL FORMULATION NOTE:
% The custodial bridge security risk decomposes into architectural constraint (cross-chain isolation — unavoidable technical barrier) and institutional choice (custodial vs. decentralized verification — contingent social choice). The architectural constraint is upstream (true technical boundary); the institutional choice is downstream (multiple solutions available). This story focuses on the institutional extraction mechanism. The upstream constraint is modeled separately in cross_chain_atomic_swap_technical_barrier (lower extractiveness, classification as mountain or rope depending on whether solutions exist).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(custodial_bridge_security_risk, institutional, 0.12).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
