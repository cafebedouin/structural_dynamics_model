% ============================================================================
% CONSTRAINT STORY: middlebox_interception
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_middlebox_interception, []).

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
 *   constraint_id: middlebox_interception
 *   human_readable: Middlebox Interception in Network Infrastructure
 *   domain: network_security/infrastructure
 *
 * SUMMARY:
 *   Middlebox interception represents a structural tension between network
 *   management needs and user privacy. Middleboxes — devices that intercept,
 *   inspect, and modify network traffic — are deployed across internet
 *   infrastructure for multiple purposes: content caching, DDoS mitigation,
 *   traffic shaping, and law enforcement access. This creates a constraint
 *   that exhibits hybrid coordination-extraction dynamics. From network
 *   operators' perspective, middleboxes enable critical management functions.
 *   From end users' perspective, interception is a privacy violation with no
 *   meaningful exit. From the protocol community's perspective, interception
 *   creates design constraints that slow innovation. The constraint has
 *   intensified over the 15-year interval as interception capabilities have
 *   expanded and protocol-level encryption has driven deeper inspection
 *   practices (MITM attacks, DPI). Simultaneously, privacy-engineering
 *   initiatives (encrypted SNI, DNS-over-HTTPS, protocol ossification
 *   resistance) are building structural exits, suggesting the constraint may
 *   become technically obsolete even if regulatory mandates persist.
 *
 * KEY AGENTS:
 *   - End Users: Primary victims (powerless/trapped) — no exit from networks containing middleboxes; traffic intercepted regardless of consent
 *   - Network Operators: Primary beneficiaries (institutional/arbitrage) — deploy middleboxes for network management; experience as coordination mechanism
 *   - Security Vendors: Secondary beneficiaries (institutional/constrained) — sell inspection tools and security services; constrained by encryption evolution
 *   - Protocol Community: Secondary victim (moderate/constrained) — protocol evolution requires middlebox compatibility; design space restricted
 *   - Privacy Engineering Coalition: Organized agents (organized/mobile) — IETF, browser vendors, VPN providers building encrypted alternatives with sunset timeline
 *   - Regulatory Compliance System: Institutional actor (institutional/arbitrage) — lawful interception mandates (CALEA) drive institutional middlebox deployment; increasingly performative
 *   - Analytical Observer: Cross-position view (analytical/analytical) — reveals hybrid coordination-extraction structure and nascent technical paths to resolution
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(middlebox_interception, 0.58).
domain_priors:suppression_score(middlebox_interception, 0.68).
domain_priors:theater_ratio(middlebox_interception, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(middlebox_interception, extractiveness, 0.58).
narrative_ontology:constraint_metric(middlebox_interception, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(middlebox_interception, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(middlebox_interception, tangled_rope).
narrative_ontology:human_readable(middlebox_interception, "Middlebox Interception in Network Infrastructure").
narrative_ontology:topic_domain(middlebox_interception, "network_security/infrastructure").

domain_priors:requires_active_enforcement(middlebox_interception).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(middlebox_interception, network_operators).
narrative_ontology:constraint_beneficiary(middlebox_interception, security_vendors).
narrative_ontology:constraint_beneficiary(middlebox_interception, content_providers).
narrative_ontology:constraint_victim(middlebox_interception, end_users).
narrative_ontology:constraint_victim(middlebox_interception, network_privacy).
narrative_ontology:constraint_victim(middlebox_interception, protocol_integrity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: END USER (SNARE) — Trapped within networks where middleboxes intercept traffic. Cannot exit without abandoning connectivity. Interception occurs regardless of consent or knowledge. Suppression is high: encrypted payloads are accessed through MITM SSL/TLS attacks, DPI inspection, or forced certificate installation. The end user bears extraction costs (privacy violation, data harvesting) with minimal coordination benefit.
constraint_indexing:constraint_classification(middlebox_interception, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: PROTOCOL COMMUNITY (TANGLED ROPE) — Constrained by the need to maintain interoperability with deployed middleboxes. Protocol evolution (e.g., HTTP/2, QUIC, TLS 1.3) requires careful design to avoid breaking NAT traversal, firewalls, and inspection tools. The constraint provides genuine coordination value (network reliability, DDoS mitigation) alongside asymmetric extraction (design space restriction, performance penalties). Active enforcement through backward compatibility requirements.
constraint_indexing:constraint_classification(middlebox_interception, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: NETWORK OPERATOR (ROPE) — Benefits from middleboxes as coordination mechanism for network management, DDoS mitigation, content caching, and traffic shaping. Experiences the constraint as enabling their function rather than extractive. Exit is arbitrage-level: they can choose placement, depth of inspection, and enforcement intensity. Net beneficiary.
constraint_indexing:constraint_classification(middlebox_interception, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: SECURITY VENDOR (TANGLED ROPE) — Constrained by regulatory requirements (lawful interception, traffic filtering mandates) and market expectations to provide deep packet inspection. Benefits from market demand for security tools. But constrained by encryption evolution and regulatory uncertainty. Coordinating role (security provision) combined with extraction (access to traffic data for commercial use). Active enforcement through regulatory compliance architecture.
constraint_indexing:constraint_classification(middlebox_interception, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: PRIVACY ENGINEERING COALITION (SCAFFOLD) — Organized agents (IETF Working Groups, browser vendors, VPN providers, DNS-over-HTTPS) see middlebox interception as a temporary problem with a structural sunset. Encrypted SNI (ESNI/ECH), encrypted DNS, onion routing, and protocol ossification resistance are engineering pathways that gradually reduce middlebox efficacy. Sunset mechanism: as encryption becomes mandatory and protocols are explicitly designed to block inspection, the middlebox constraint loses leverage. Estimated timeline: 10-15 years for mature deployment of privacy-preserving protocols.
constraint_indexing:constraint_classification(middlebox_interception, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: REGULATORY COMPLIANCE SYSTEM (PITON) — Lawful interception mandates (CALEA, equivalents in EU/Asia) established middlebox inspection as institutional requirement. But the primary function (law enforcement access to communications) has become increasingly theatrical: sophisticated actors evade inspection, encrypted protocols render mandates unenforceable, and compliance infrastructure consumes resources with declining effectiveness. The constraint persists through regulatory inertia and sunk institutional cost rather than functional necessity. Theater ratio reflects that the compliance theater continues despite diminishing returns.
constraint_indexing:constraint_classification(middlebox_interception, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (TANGLED ROPE) — From a cross-position analytical view, middlebox interception exhibits genuine coordination functions (NAT traversal, caching, DDoS mitigation) alongside systematic extraction of privacy and data access rights from end users. The constraint is maintained through institutional layering: network operators deploy for efficiency, security vendors for market demand, regulators for law enforcement, but the layering creates suppression that prevents individual actors from withdrawing. Effective extraction (0.58) reflects this hybrid structure — not maximally extractive because coordination value is real, but high suppression makes exit costly.
constraint_indexing:constraint_classification(middlebox_interception, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(middlebox_interception_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(middlebox_interception, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(middlebox_interception, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(middlebox_interception, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(middlebox_interception, TR),
    TR >= 0.70.

:- end_tests(middlebox_interception_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. Middlebox interception extracts privacy and traffic data from end users (high extraction from powerless perspective) while providing genuine coordination value to network operators (low extraction from institutional perspective). The rising trend (0.35 → 0.58 over 15 years) reflects intensified inspection practices as encryption became standard — operators compensated for loss of cleartext access by deploying deeper inspection (MITM SSL attacks, DPI, certificate injection). Suppression (0.68): High. Suppression operates through multiple mechanisms: technical (encrypted traffic is harder to analyze but still targetable), legal (interception is mandated by regulators, making exit impossible), institutional (network topology concentrates middleboxes at chokepoints), and epistemic (users are often unaware of interception). But suppression is not absolute — encrypted protocols and VPNs provide partial escapes. Theater ratio (0.55): Moderate. Regulatory compliance infrastructure (lawful interception) is increasingly theatrical — sophisticated actors evade inspection, encrypted protocols render mandates unenforceable, but compliance theater persists due to institutional inertia. Network management functions (caching, DDoS mitigation) are less theatrical — they provide real coordination value, though some of that value could be achieved without payload interception.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap between the network operator (Rope) and end user (Snare) represents a fundamental disagreement about the constraint's nature. The operator genuinely uses middleboxes for coordination and experiences them as enabling. The end user genuinely cannot exit and experiences them as suppressive. Neither perspective is 'wrong' — they are measuring from different structural positions. The analytical observer (Tangled Rope) reveals that both are partially correct: the constraint contains real coordination functions (rope elements) layered with systematic extraction (snare elements). The private engineering coalition's scaffold view is prospective — they see a technical sunset path that neither the operator nor the end user currently perceives. The regulatory system's piton classification is diagnostic: lawful interception was installed as rope or tangled_rope (genuine law enforcement coordination need) but has degraded into piton (theater persisting despite minimal functional output). This degradation is captured in the rising theater_ratio (0.42 → 0.55) while extractiveness also rose — the constraint became both more theatrical and more extractive, the signature of institutional inertia.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality computation reveals structural asymmetry. Beneficiaries (network operators, security vendors, regulators) have exit options ranging from arbitrage to constrained, producing low or moderate d values and low/negative effective extraction. Victims (end users, privacy, protocol design space) have trapped or constrained exit, producing high d values and high effective extraction. The protocol community occupies an intermediate position — they benefit from the coordination (reliable networks) but bear extraction costs (design restrictions). Victims are listed as abstract: 'network_privacy' and 'protocol_integrity' are not human agents but structural goods that are degraded by interception. This is analytically correct but creates a classification subtlety: the piton perspective's 'regulatory compliance system' actor has arbitrage exit (can choose enforcement intensity) and sees the constraint as performative, not extractive. This is captured in the piton classification: theater_ratio is high (0.55), indicating functional degradation.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATE ANALYSIS: The middlebox constraint resolves mandatrophy by decomposing coordination from extraction. The genuine coordination functions (NAT traversal, caching, DDoS mitigation, even lawful interception) are real and defensible. But the constraint is structured to prevent these functions from being unbundled: users cannot access networks with NAT traversal but no interception, cannot get DDoS protection without payload inspection, cannot benefit from caching without accepting MITM attacks. The unbundling is technically possible (transparent proxies can provide NAT services, DDoS detection can work on encrypted traffic, caching can operate on encrypted content metadata) but institutionally prevented. The mandatrophy is that the constraint's defenders must defend the entire bundled package (coordination + extraction) as necessary, when the coordination could be achieved with lower extraction. The scaffold view (privacy coalition) resolves this by building unbundled alternatives outside the constraint. The piton view (regulatory system) resolves it by admitting that extraction (law enforcement access) has become theatrical rather than functional. No single perspective resolves the mandatrophy — the resolution comes from the presheaf structure: some perspectives see genuine coordination (rope), others see genuine extraction (snare), and the analytical view sees that both are present but could be decoupled.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    encryption_obsolescence_timeline,
    'At what point does protocol-level encryption (TLS, QUIC, ECH) become sufficiently universal that middlebox interception becomes technically obsolete rather than suppressed?',
    'Traffic analysis: measurement of encrypted vs cleartext fraction of internet traffic; correlation with protocol adoption timelines; analysis of middlebox effectiveness against mature encrypted protocols',
    'If timeline < 10 years: scaffold perspective is optimistic but structural. If timeline > 20 years: middlebox constraint becomes semi-permanent, reclassifying toward snare. If timeline undefined (encryption never reaches universality): constraint persists indefinitely.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(encryption_obsolescence_timeline, empirical, 'Obsolescence timeline for middlebox interception via protocol-level encryption').

omega_variable(
    regulatory_enforcement_efficacy,
    'Does lawful interception infrastructure (CALEA, EU equivalents) actually achieve stated law enforcement objectives, or is it primarily performative compliance?',
    'Judicial access analysis: comparison of court-ordered interception requests to actual successful prosecutions; measurement of investigation success rates with/without interception; analysis of evasion prevalence among investigated actors',
    'If efficacy is high: regulatory compliance system''s piton classification is incorrect — it should be rope or tangled_rope. If efficacy is low: piton classification confirmed — theater persists despite minimal functional output.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(regulatory_enforcement_efficacy, empirical, 'Whether lawful interception mandates achieve functional law enforcement objectives').

omega_variable(
    coordination_benefit_decoupling,
    'Can network coordination functions (DDoS mitigation, caching, NAT traversal) be provided without payload interception, or is deep inspection technically necessary for these functions?',
    'Technical analysis: comparison of network resilience with transparent proxies vs. deep inspection middleboxes; measurement of DDoS mitigation efficacy without payload analysis; protocol design alternatives that maintain coordination without interception',
    'If decoupling is possible: middlebox constraint is pure extraction with coordination theater. If decoupling fails: constraint is genuine tangled_rope with irreducible extraction. Classification shifts toward snare in first case, remains tangled_rope in second.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(coordination_benefit_decoupling, empirical, 'Technical necessity of payload interception for network coordination functions').

omega_variable(
    identity_lock_regulatory_capture,
    'Are network operators identity-locked to middlebox deployment (unable to perceive alternatives) or constrained by regulatory/market forces (perceive alternatives but face barriers)?',
    'Interview analysis: operator perception of middlebox necessity; exploration of alternative architectures; assessment of whether operators have considered constraint-free network designs; analysis of regulatory vs. economic vs. technical barriers to exit',
    'If identity-locked: operators cannot see decoupling alternatives even if technically possible — constraint becomes harder to resolve. If constrained: operators perceive exits but face barriers — policy/regulation changes might enable transition. Classification unchanged but mechanism differs fundamentally.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_regulatory_capture, conceptual, 'Whether network operators are identity-locked to middlebox deployment or externally constrained').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(middlebox_interception, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mbox_tr_t0, middlebox_interception, theater_ratio, 0, 0.42).
narrative_ontology:measurement(mbox_tr_t7, middlebox_interception, theater_ratio, 7, 0.48).
narrative_ontology:measurement(mbox_tr_t15, middlebox_interception, theater_ratio, 15, 0.55).
narrative_ontology:measurement(mbox_tr_t5, middlebox_interception, theater_ratio, 5, 0.46).

% Extraction over time
narrative_ontology:measurement(mbox_be_t0, middlebox_interception, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(mbox_be_t7, middlebox_interception, base_extractiveness, 7, 0.52).
narrative_ontology:measurement(mbox_be_t15, middlebox_interception, base_extractiveness, 15, 0.58).
narrative_ontology:measurement(mbox_be_t5, middlebox_interception, base_extractiveness, 5, 0.45).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(middlebox_interception, global_infrastructure).
narrative_ontology:boltzmann_floor_override(middlebox_interception, 0.18).
narrative_ontology:affects_constraint(middlebox_interception, end_to_end_encryption_adoption).
narrative_ontology:affects_constraint(middlebox_interception, regulatory_lawful_interception).

% DUAL FORMULATION NOTE:
% Middlebox interception decomposes into two structurally distinct constraints: (1) technical middlebox deployment (network operator coordination need) and (2) regulatory interception mandates (law enforcement extraction need). The technical coordination function (0.35 extractiveness) is separable from the regulatory extraction function (0.58 measured total). Future stories should decompose these into separate constraint families linked by network.affects_constraints. Current story treats them as unified because institutional practice bundles them.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(middlebox_interception, institutional, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
