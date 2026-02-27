% ============================================================================
% CONSTRAINT STORY: digital_identity_tether
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_digital_identity_tether, []).

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
 *   constraint_id: digital_identity_tether
 *   human_readable: The Centralized Identity Anchor
 *   domain: technological/digital_infrastructure
 *
 * SUMMARY:
 *   The Centralized Identity Anchor defines a structural constraint in
 *   digital infrastructure where users cannot decouple their reputation,
 *   social graph, and authentication credentials from a single provider
 *   without losing access to all accumulated identity capital. This creates
 *   an asymmetric extraction relationship: the identity provider controls the
 *   terms of access, the cost of switching, and the rules governing
 *   reputation modification. The constraint exhibits genuine coordination
 *   benefits (identity verification, fraud prevention, transaction
 *   settlement) alongside coercive extraction mechanisms (vendor lock-in,
 *   data monopoly, surveillance capacity). The tension between these two
 *   functions—real coordination problem solved + extraction rent captured
 *   through that solution—makes this a canonical Tangled Rope case. The
 *   constraint is degrading in extractiveness over time as regulatory
 *   mandates (EU Digital Identity Regulation, W3C decentralized identifiers)
 *   and cryptographic advances (zero-knowledge proofs, distributed ledger
 *   attestations) build alternative pathways. The theater_ratio indicates
 *   that both legacy authentication mechanisms (email, phone verification)
 *   and emerging regulatory compliance theater (pseudo-interoperability APIs,
 *   fake federation) are increasing as both the original coordination
 *   function and the extraction mechanism face pressure from standardization
 *   efforts.
 *
 * KEY AGENTS:
 *   - Digital Subject (User): Primary victim (powerless/trapped) — loses access to reputation and social graph if they leave; bears full extraction cost from lock-in
 *   - Identity Provider: Primary beneficiary (institutional/arbitrage) — captures transaction fees, surveillance data, and merchant integration revenue; controls switching costs
 *   - Merchant Ecosystem: Secondary beneficiary (institutional/arbitrage) — benefits from centralized authentication but could arbitrage to alternative providers; low extraction exposure
 *   - Open Identity Ecosystem: Primary victim (moderate/constrained) — fragmented by vendor lock-in; bears costs of redundant identity verification and standardization delays
 *   - Regulatory Reform Movement: Organized actor (organized/constrained) — building decentralized identity mandate infrastructure with sunset timeline (8-15 years)
 *   - Legacy Authentication Industry: Institutional actor maintaining degraded mechanisms through inertia and regulatory compliance theater
 *   - Analytical Observer: Civilizational perspective risking naturalization of contingent architectural choices as immutable trust problems
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(digital_identity_tether, 0.58).
domain_priors:suppression_score(digital_identity_tether, 0.68).
domain_priors:theater_ratio(digital_identity_tether, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(digital_identity_tether, extractiveness, 0.58).
narrative_ontology:constraint_metric(digital_identity_tether, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(digital_identity_tether, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(digital_identity_tether, tangled_rope).
narrative_ontology:human_readable(digital_identity_tether, "The Centralized Identity Anchor").
narrative_ontology:topic_domain(digital_identity_tether, "technological/digital_infrastructure").

domain_priors:requires_active_enforcement(digital_identity_tether).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(digital_identity_tether, identity_provider).
narrative_ontology:constraint_beneficiary(digital_identity_tether, merchant_ecosystem).
narrative_ontology:constraint_victim(digital_identity_tether, digital_subject).
narrative_ontology:constraint_victim(digital_identity_tether, open_identity_ecosystem).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DIGITAL SUBJECT (SNARE) — The user cannot exit without losing access to social graph, reputation history, authentication credentials, and digital property linked to their anchor identity. Switching providers means abandoning accumulated identity capital. Exit is structurally blocked: no interoperability standard exists, and reputation does not transfer. Maximum experienced extraction from the user's position.
constraint_indexing:constraint_classification(digital_identity_tether, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: OPEN IDENTITY ECOSYSTEM (TANGLED ROPE) — Federation and interoperability advocates benefit from identity infrastructure (they can build on it, integrate with it) but bear extraction costs: proprietary lock-in fragments the ecosystem, raises integration barriers, and forces redundant identity verification across platforms. Some exit path exists (building alternative standards, federation protocols) but implementation is constrained by network effects and adoption barriers.
constraint_indexing:constraint_classification(digital_identity_tether, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 3: IDENTITY PROVIDER (ROPE) — Experiences the constraint as pure coordination: managing identity verification, reputation aggregation, and authentication is a genuine coordination problem. The provider benefits from being the sole resolver of identity disputes and derives revenue from transaction settlement and merchant integration. Has complete arbitrage exit: can migrate infrastructure, change business model, or exit market. Extraction flows toward this agent.
constraint_indexing:constraint_classification(digital_identity_tether, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: MERCHANT ECOSYSTEM (ROPE) — Benefits from centralized user authentication and reputation data; accepts the tether as a coordination solution to the identity verification problem. Merchants can arbitrage by integrating with multiple identity anchors or building proprietary authentication. The constraint enables their business model (frictionless identity verification) without imposing severe extraction.
constraint_indexing:constraint_classification(digital_identity_tether, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: REGULATORY REFORM MOVEMENT (SCAFFOLD) — EU Digital Identity Regulation, W3C decentralized identifiers, and verifiable credentials initiatives are building a sunset mechanism: portable identity, decentralized reputation, and interoperable authentication pathways. These organized actors see the tether as a temporary coordination gap that can be replaced by federated standards. Theater is low because the regulatory pathway is explicit and measurable (DID adoption, attestation formats, cross-border verification protocols). Exit timeline: 8-15 years for standards maturation and regulatory enforcement.
constraint_indexing:constraint_classification(digital_identity_tether, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: LEGACY AUTHENTICATION INDUSTRY (PITON) — Traditional identity verification (credit reports, phone numbers, government-issued IDs, email addresses) persists as backup authentication despite better alternatives existing (biometric verification, distributed ledgers, attestation chains). The theater ratio is high: merchants and providers maintain multi-factor authentication rituals that are largely performative—they protect against account takeover but not against the fundamental vendor lock-in. Piton classification reflects the institutional inertia keeping these mechanisms alive past their functional utility.
constraint_indexing:constraint_classification(digital_identity_tether, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, digital identity verification requires some form of trusted anchor to bootstrap belief in claimed attributes. The tether appears as a natural consequence of the identity verification problem: someone must verify that you are who you claim to be, and that someone must be trusted. However, this naturalizes what is actually contingent: decentralized reputation systems, distributed ledger attestations, and zero-knowledge proofs demonstrate that trust can be architected without a single anchor. This perspective masks a false summit.
constraint_indexing:constraint_classification(digital_identity_tether, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(digital_identity_tether_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(digital_identity_tether, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(digital_identity_tether, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(digital_identity_tether, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(digital_identity_tether, TR),
    TR >= 0.70.

:- end_tests(digital_identity_tether_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness (0.58): High-moderate. The identity provider captures significant value through transaction fees, merchant integration licensing, data monetization, and switching-cost barriers. Users lose reputation portability and must rebuild social graphs on new platforms. However, extractiveness is not at maximum (0.75+) because: (1) merchants have partial arbitrage capacity—they can integrate with multiple identity anchors or build proprietary authentication; (2) users retain some exit path through multi-platform account creation; (3) regulatory mandates are beginning to fragment the lock-in. The rising trajectory (0.42→0.58 over 14 time units) reflects increased consolidation among identity providers and tightening of ecosystem lock-in as network effects mature. Suppression (0.68): High. Structural barriers to exit include: (1) no interoperability standard for reputation transfer; (2) network effects that penalize switching (social graph is siloed); (3) merchant dependency on centralized identity resolution; (4) regulatory capture by incumbent providers; (5) user inertia and lock-in costs. However, suppression is not maximal (≥0.80) because regulatory mandates are explicitly reducing it—GDPR data portability, DMA interoperability requirements, and W3C standards create formal exit pathways. Theater ratio (0.55): Moderate. The identity provider performs genuine coordination (verification, reputation aggregation) but also deploys theater: multi-factor authentication rituals, compliance certifications, and regulatory messaging about security serve both real assurance and brand legitimacy. The rise from 0.35→0.55 reflects increasing regulatory compliance theater (fake federation APIs, pseudo-interoperability claims) as the regulatory threat increases.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exemplifies how indexical classification reveals structural disagreement about the same phenomenon. The digital subject and the identity provider are both observing a centralized system, but they experience opposite classifications: Snare (extracted) vs Rope (coordination). This is not a measurement ambiguity—it is a real structural asymmetry. The subject's exit options are materially different from the provider's exit options; their power levels are structurally different. The analytical observer risks falsely naturalizing this asymmetry as an immutable identity verification requirement (Mountain), when it is actually a contingent architectural choice that decentralized identity systems prove is decomposable. The scaffold perspective's sunset clause is crucial: regulatory mandates (EU DIA, W3C, DMA compliance) are actively building the ramp down—this is not aspirational, it is a measured policy timeline with enforcement mechanisms. The piton perspective reveals that legacy authentication mechanisms remain not because they are superior but because regulatory compliance theater keeps them alive—this is degradation, not resilience.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality derives from each agent's structural position in the extraction flow. Users (d ≈ 0.95, trapped + victim) experience maximum extraction—they cannot leave and bear full lock-in cost. The identity provider (d ≈ 0.05, institutional + beneficiary + arbitrage) experiences negative extraction (subsidy)—they benefit from being the coordination anchor. Merchants (d ≈ 0.30, institutional + mixed) experience constrained extraction—they benefit from centralized identity but face some coordination friction and switching costs. Open identity ecosystem (d ≈ 0.65, moderate + victim + constrained) experiences significant extraction—they are constrained by vendor lock-in but retain some agency through standardization efforts. The regulatory reform movement (d ≈ 0.50, organized + mixed) experiences symmetric costs and benefits—they bear enforcement costs but gain legitimacy and market access through the mandate. The sigmoid f(d) amplifies extraction perception for trapped victims and dampens it for institutional beneficiaries with arbitrage options. The scope modifier σ(S) applies global scope (σ=1.2) because identity lock-in is worldwide and platform-independent—network effects scale globally.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION: This constraint avoids the mandatrophy trap by precisely distinguishing the genuine coordination function (identity verification, fraud prevention, transaction settlement) from the extractive mechanism (vendor lock-in, reputation monopoly, switching-cost barriers). The tangled_rope classification is structurally justified: beneficiaries exist (identity provider, merchants), victims exist (users, open identity ecosystem), and enforcement is active (network effects, proprietary APIs, data silos). The scaffold perspective resolves the 'is this permanent extraction or temporary coordination problem?' question empirically: regulatory mandates are explicitly building alternative architectures (decentralized identity, portable reputation, federated standards). The mountain perspective is correctly identified as a false summit—trust in digital identity does not require a single anchor; cryptographic primitives (zero-knowledge proofs, distributed attestations) prove decomposability. The constraint avoids false permanence by measuring the sunset clause timeline (8-15 years) against the current enforceability of regulatory mandates. This is not speculation—it is a measurable policy timeline with legal enforcement power.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    decentralized_reputation_feasibility,
    'Can decentralized reputation systems (blockchain-based attestations, zero-knowledge proofs) achieve equivalent or superior fraud detection to centralized identity anchors without creating new attack surfaces?',
    'Comparative cryptanalysis of decentralized vs centralized identity verification; empirical measurement of fraud rates in pilot DID systems vs traditional anchors; identification of sybil-attack resistance and collusion-proofness gaps',
    'If decentralized systems match centralized security: scaffold perspective confirmed and tether becomes clearly extractive (moves toward pure Snare). If decentralized systems have material weaknesses: tether may be legitimate coordination (moves toward pure Rope).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(decentralized_reputation_feasibility, empirical, 'Whether decentralized reputation systems can match centralized security').

omega_variable(
    network_effect_switching_threshold,
    'At what market concentration threshold does identity provider lock-in become irreversible, even with regulatory mandate for interoperability?',
    'Economic modeling of network effect saturation; historical analysis of prior switching-cost escapes (migration from AOL, Compuserve, Lotus Notes, MySpace); measurement of actual user switching rates under regulatory interoperability requirements (GDPR data portability, DMA digital markets act)',
    'If threshold < 2 providers with 80%+ market share: tether is structurally irreversible absent government intervention (Snare). If threshold > 5 providers: market competition can erode lock-in (Rope or early Scaffold).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(network_effect_switching_threshold, empirical, 'Lock-in threshold under regulatory interoperability mandates').

omega_variable(
    regulatory_enforcement_timing,
    'Will regulatory decentralization mandates (EU DIA, Digital Markets Act compliance) actually enforce the sunset clause, or will they degrade into performative compliance theater?',
    'Measurement of actual DID adoption by merchants and users 5-10 years post-mandate; audit of identity provider compliance with interoperability requirements; tracking of regulatory enforcement actions vs compliance theater (fake federation, pseudo-APIs)',
    'If enforcement is real: scaffold perspective is structurally sound, and tether transitions to piton-degradation phase. If enforcement fails: regulatory theater becomes new suppression mechanism (snare classification confirmed from powerless perspective).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_enforcement_timing, empirical, 'Whether regulatory mandates enforce actual decentralization or theater').

omega_variable(
    sybil_resistance_cost_gap,
    'What is the cost differential between sybil-resistant decentralized identity verification and centralized anchor verification? Is the gap closing?',
    'Computational cost analysis of zero-knowledge proof generation vs centralized verification; measurement of attestation overhead in pilot DID systems; tracking of cryptographic efficiency improvements year-over-year',
    'If gap < 2x: decentralization is economically viable and scaffold timeline accelerates. If gap > 10x: performance drag will keep merchants tied to centralized anchors despite regulatory pressure.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sybil_resistance_cost_gap, empirical, 'Cost efficiency gap between decentralized and centralized verification').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(digital_identity_tether, 0, 14).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(didt_tr_t0, digital_identity_tether, theater_ratio, 0, 0.35).
narrative_ontology:measurement(didt_tr_t7, digital_identity_tether, theater_ratio, 7, 0.48).
narrative_ontology:measurement(didt_tr_t14, digital_identity_tether, theater_ratio, 14, 0.55).

% Extraction over time
narrative_ontology:measurement(didt_be_t0, digital_identity_tether, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(didt_be_t7, digital_identity_tether, base_extractiveness, 7, 0.51).
narrative_ontology:measurement(didt_be_t14, digital_identity_tether, base_extractiveness, 14, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(digital_identity_tether, information_standard).
narrative_ontology:affects_constraint(digital_identity_tether, merchant_credit_assessment).
narrative_ontology:affects_constraint(digital_identity_tether, cross_platform_social_portability).
narrative_ontology:affects_constraint(digital_identity_tether, biometric_authentication_monopoly).

% DUAL FORMULATION NOTE:
% The centralized identity anchor decomposes into three structurally distinct constraints: (1) Identity verification (base extraction ε≈0.25, Mountain—cryptographic problem) (2) Reputation portability (ε≈0.55, Tangled Rope—architectural choice) (3) Merchant integration (ε≈0.40, Rope—coordination mechanism). These stories are linked: the reputation portability constraint is downstream of the identity verification problem (which creates the anchor) and affects merchant integration feasibility. Each has its own metrics and perspectives; decomposition prevents false naturalization of architectural choices as immutable technical limits.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(digital_identity_tether, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
