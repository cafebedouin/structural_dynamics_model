% ============================================================================
% CONSTRAINT STORY: tls_version_fragmentation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_tls_version_fragmentation, []).

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
 *   constraint_id: tls_version_fragmentation
 *   human_readable: TLS Version Fragmentation and Protocol Ossification
 *   domain: cybersecurity/internet_infrastructure
 *
 * SUMMARY:
 *   TLS version fragmentation refers to the structural inability to deprecate
 *   legacy versions (TLS 1.0, 1.1, 1.2) despite security vulnerabilities and
 *   the availability of superior alternatives (TLS 1.3). This constraint
 *   emerges from a genuine coordination problem — ensuring secure
 *   communication across heterogeneous client/server populations — that has
 *   transformed into an extraction mechanism where legacy infrastructure
 *   operators avoid upgrade costs while modern deployments bear the overhead
 *   of maintaining compatibility. The constraint exhibits all six
 *   classification types depending on perspective, making it a diagnostic
 *   exemplar for how network ossification emerges from rational individual
 *   incentives. The theater ratio (0.58) reflects that much of the
 *   fragmentation maintenance is performative: security fixes are applied to
 *   TLS 1.2, but the primary function (secure communication) is better served
 *   by TLS 1.3. The increasing theater over time indicates the ritual is
 *   becoming more detached from function as TLS 1.3 adoption accelerates.
 *
 * KEY AGENTS:
 *   - Security Standards Body (IETF): Victim (powerless/trapped) — cannot advance security standards due to mandatory backwards compatibility
 *   - Enterprise IT Operators: Mixed (moderate/constrained) — benefit from TLS coordination but constrained by legacy deployed base they don't fully control
 *   - Legacy Infrastructure Owners: Primary beneficiary (institutional/arbitrage) — capture value by avoiding upgrade costs; experience the constraint as pure coordination benefit
 *   - Browser Vendors (Chrome, Firefox, Safari, Edge): Organized actors (organized/constrained) — building exit pathway through coordinated deprecation; have partial agency but constrained by network effects
 *   - TLS 1.2 Maintenance Committees: Institutional actor (institutional/arbitrage) — maintain degraded standard through inertia; see their own process as performative
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing a contingent social choice as an immutable law of distributed systems
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(tls_version_fragmentation, 0.52).
domain_priors:suppression_score(tls_version_fragmentation, 0.48).
domain_priors:theater_ratio(tls_version_fragmentation, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(tls_version_fragmentation, extractiveness, 0.52).
narrative_ontology:constraint_metric(tls_version_fragmentation, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(tls_version_fragmentation, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(tls_version_fragmentation, tangled_rope).
narrative_ontology:human_readable(tls_version_fragmentation, "TLS Version Fragmentation and Protocol Ossification").
narrative_ontology:topic_domain(tls_version_fragmentation, "cybersecurity/internet_infrastructure").

domain_priors:requires_active_enforcement(tls_version_fragmentation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(tls_version_fragmentation, legacy_client_operators).
narrative_ontology:constraint_beneficiary(tls_version_fragmentation, deployed_server_infrastructure).
narrative_ontology:constraint_victim(tls_version_fragmentation, security_standards_advancement).
narrative_ontology:constraint_victim(tls_version_fragmentation, interoperability_innovation).
narrative_ontology:constraint_victim(tls_version_fragmentation, new_client_deployments).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SECURITY STANDARDS ADVANCEMENT (SNARE) — Cannot exit the fragmentation trap. New security improvements (post-quantum cryptography, modern cipher suites) are blocked by the need to maintain compatibility with legacy TLS 1.0/1.1 clients. The standards body bears full cost of delayed modernization with no ability to coordinate a forced upgrade. Zero degrees of freedom: backwards compatibility is mandatory, forward progress is not.
constraint_indexing:constraint_classification(tls_version_fragmentation, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: ENTERPRISE IT OPERATOR (TANGLED ROPE) — Constrained by installed base of legacy systems; upgrading all clients is expensive and carries downtime risk. Benefits from coordinated standards (TLS enables secure communication across heterogeneous networks). Mixed position: genuine coordination function (TLS solves a real problem) plus asymmetric extraction (must maintain compatibility with unpatched legacy systems owned by other organizations).
constraint_indexing:constraint_classification(tls_version_fragmentation, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: LEGACY INFRASTRUCTURE OWNER (ROPE) — Benefits from the fragmentation constraint: their unpatched TLS 1.0/1.1 systems continue to function despite security vulnerabilities. The constraint actively subsidizes their non-compliance. They experience TLS fragmentation as pure coordination benefit — their outdated clients remain functional without costly replacement. Arbitrage option: can delay modernization indefinitely while others bear upgrade costs.
constraint_indexing:constraint_classification(tls_version_fragmentation, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: BROWSER VENDOR COALITION (SCAFFOLD) — Organized actors (Chrome, Firefox, Safari, Edge) are actively sunsetting old TLS versions through client-side deprecation. See the fragmentation as a temporary coordination failure with a clear exit path: progressive client rollout removes support for TLS 1.0/1.1, forcing server-side modernization. Theater low because vendors openly communicate deprecation timelines. Sunset mechanism: when critical mass of clients drop old TLS support, servers must upgrade or lose connectivity. Estimated sunset: 5-10 years for global deployment.
constraint_indexing:constraint_classification(tls_version_fragmentation, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: TLS 1.2 MAINTENANCE COMMITTEE (PITON) — TLS 1.2 (released 2008) remains the institutional standard deployed at massive scale despite TLS 1.3 (2018) offering superior security and performance. The maintenance committee invests effort in bugfixes and clarifications to TLS 1.2, but the primary function has atrophied — modern deployments should be on 1.3. The constraint persists through institutional inertia: organizations have optimized around TLS 1.2, regulatory compliance is built around it, and wholesale replacement carries perceived risk despite 1.3 being superior. High theater ratio reflects performative maintenance of a degraded standard.
constraint_indexing:constraint_classification(tls_version_fragmentation, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a universal perspective, backwards compatibility in network protocols is an immutable constraint: any protocol change risks breaking existing implementations, so compatibility layers must be maintained indefinitely. This perspective sees fragmentation as a law of distributed systems rather than a contingent institutional choice. However, the structural data contradicts this natural law framing — the constraint is fundamentally a coordination failure and social choice about upgrade incentives, not a physical law. The engine's false summit detector will identify this as naturalization of a contingent institutional arrangement.
constraint_indexing:constraint_classification(tls_version_fragmentation, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(tls_version_fragmentation_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(tls_version_fragmentation, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(tls_version_fragmentation, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(tls_version_fragmentation, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(tls_version_fragmentation, TR),
    TR >= 0.70.

:- end_tests(tls_version_fragmentation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high and rising. The constraint began as a genuine coordination mechanism (base 0.35) but has accumulated extractive overhead as it has aged. Modern deployments must maintain compatibility with TLS 1.0/1.1 despite having no legitimate security function — they are museum pieces kept alive by backwards compatibility requirements. The increasing trajectory (0.35 → 0.52 over 15 years) reflects ossification: each new feature added to maintain 1.2/1.3 compatibility costs more, while the legacy versions become proportionally more extractive. Suppression (0.48): Moderate. Barriers to deprecation exist (deployed systems can't upgrade remotely, regulatory compliance is built around TLS 1.2, perceived risk in forced upgrades) but are not total. Browser vendors demonstrably have agency to deprecate versions; the suppression is sustained by coordination failure and organizational risk aversion rather than absolute technical barriers. Theater ratio (0.58, rising): Significant performative content. The TLS maintenance committees spend effort on TLS 1.2 bugfixes, but these are largely safety theater — the ecosystem should be on TLS 1.3. The rising trajectory reflects increasing mismatch between what the constraint ostensibly does (enable secure communication across versions) and what it actually does (subsidize non-compliance).
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates maximal perspectival divergence. The legacy infrastructure owner sees pure coordination benefit (Rope) — their TLS 1.0 server continues to work without replacement cost. The browser vendor coalition sees a temporary problem being solved by active deprecation (Scaffold) — they have agency and a sunset mechanism. The TLS committee sees its own degraded ritual (Piton) — bugfixes on an aging standard. The enterprise operator sees mixed coordination and extraction (Tangled Rope) — the system both enables and constrains their operations. The security standards body sees pure extraction (Snare) — forced to maintain compatibility with insecure legacy versions that lock out security innovations. The analytical observer risks seeing an immutable natural law (Mountain) — backwards compatibility is inherent to distributed systems — but the structural data reveals this as a false summit: the fragmentation is a social choice about upgrade incentives and risk aversion, not a physical law.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality is determined by whether the agent benefits from or bears costs of the fragmentation. Legacy infrastructure owners benefit from mandatory backwards compatibility (d ≈ 0.15, beneficiary + arbitrage exit → negative f(d) → they experience low extraction, actually subsidization). Enterprise IT operators bear mixed costs and benefits (d ≈ 0.50, constrained exit means moderate experienced extraction). The security standards body bears all costs with no exit (d ≈ 0.95, trapped victim → high f(d) → maximum experienced extraction). Browser vendors have partial agency to force migration (d ≈ 0.55, organized but constrained → moderate extraction). The analytical observer risks collapsing onto d ≈ 0.72 (canonical analytical), treating fragmentation as an observer-independent property rather than a contingent institutional choice.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint resolves the mandatrophy by demonstrating that fragmentation is neither pure coordination (that would be Rope classification from all perspectives) nor pure extraction (that would be Snare from all perspectives). It is genuinely hybrid: the coordination function (enabling heterogeneous client/server communication) is real and necessary, but the extraction mechanism (subsidizing non-compliance) is also real and growing. The scaffold perspective shows that the constraint can be sunset through coordinated client-side deprecation, revealing that the 'inevitable' fragmentation is actually contingent on institutional choices (browser vendors coordinating vs. not coordinating). The piton perspective shows that maintenance effort persists despite functional degradation, revealing theatrical elements. No single type captures the full structure — the presheaf of perspectives (snare + rope + scaffold + piton + mountain) together reveal the constraint's true character: an aging coordination mechanism becoming extractive as it ossifies.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    critical_mass_threshold,
    'What percentage of global TLS traffic must shift to TLS 1.3+ to create irreversible pressure on remaining TLS 1.0/1.1 servers?',
    'Measurement of global TLS version distribution over time; correlation with server deprecation events; analysis of when holdout servers begin failing at scale',
    'If threshold < 40%: browsers/CDNs can force migration unilaterally. If threshold > 70%: long tail of legacy deployments persist indefinitely, keeping fragmentation active. If threshold = critical mass point, scaffold sunset becomes rope or snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(critical_mass_threshold, empirical, 'Critical mass threshold for forcing legacy server upgrades').

omega_variable(
    ossification_mechanism,
    'Is TLS version fragmentation a temporary coordination problem or evidence of network protocol ossification — an irreversible hardening of deployed infrastructure?',
    'Historical comparison with IPv4/IPv6 transition timelines; analysis of whether TLS 1.3 adoption trajectory matches or diverges from predicted replacement rate; post-mortem analysis of other protocol transitions (HTTP/2, QUIC adoption)',
    'If temporary: scaffold sunset is real. If ossification: fragmentation becomes structural piton or snare indefinitely. Classification would shift from tangled_rope to snare as agent_power redistributes toward powerless.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(ossification_mechanism, empirical, 'Whether fragmentation is temporary coordination failure or irreversible protocol ossification').

omega_variable(
    client_vendor_coordination,
    'Can browser vendors maintain coordinated TLS version deprecation without collusion concerns, or do antitrust constraints prevent the coalition that would enforce the scaffold sunset?',
    'Legal analysis of browser vendor coordination; analysis of past failures (e.g., HTTP/2 adoption delays); measurement of deprecation timeline consistency across vendors',
    'If coordinated: scaffold sunset mechanism works as designed. If fragmented by antitrust: each vendor deprecates at different rates, replication groups fragment, and the constraint becomes harder to enforce. Scaffold classification becomes rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(client_vendor_coordination, conceptual, 'Whether browser vendor coalition can coordinate TLS deprecation without legal constraints').

omega_variable(
    embedded_system_exceptions,
    'Will IoT and embedded systems (routers, cameras, industrial controllers) running outdated TLS indefinitely create a structural exception that prevents full protocol migration?',
    'Inventory of deployed embedded systems with fixed TLS versions; analysis of whether mission-critical embedded systems can be remotely updated; measurement of embedded system TLS version distribution',
    'If exceptions are <5% of global traffic: can be isolated. If >20%: creates permanent fragmentation floor. Classification would shift: the snare would become a mountain (irreducible embedded legacy constraint).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(embedded_system_exceptions, empirical, 'Whether embedded systems create structural exception to TLS migration').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(tls_version_fragmentation, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tlsfrag_tr_t0, tls_version_fragmentation, theater_ratio, 0, 0.42).
narrative_ontology:measurement(tlsfrag_tr_t5, tls_version_fragmentation, theater_ratio, 5, 0.51).
narrative_ontology:measurement(tlsfrag_tr_t10, tls_version_fragmentation, theater_ratio, 10, 0.58).
narrative_ontology:measurement(tlsfrag_tr_t15, tls_version_fragmentation, theater_ratio, 15, 0.62).

% Extraction over time
narrative_ontology:measurement(tlsfrag_be_t0, tls_version_fragmentation, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(tlsfrag_be_t5, tls_version_fragmentation, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(tlsfrag_be_t10, tls_version_fragmentation, base_extractiveness, 10, 0.52).
narrative_ontology:measurement(tlsfrag_be_t15, tls_version_fragmentation, base_extractiveness, 15, 0.54).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(tls_version_fragmentation, global_infrastructure).
narrative_ontology:affects_constraint(tls_version_fragmentation, http2_adoption_barriers).
narrative_ontology:affects_constraint(tls_version_fragmentation, certificate_authority_coupling).
narrative_ontology:affects_constraint(tls_version_fragmentation, internet_routing_path_dependencies).

% DUAL FORMULATION NOTE:
% TLS version fragmentation can be decomposed into two structurally distinct constraints: (1) the genuine coordination problem of heterogeneous client/server populations (ε ≈ 0.15, Rope), and (2) the institutional decision to maintain backwards compatibility indefinitely (ε ≈ 0.52, Tangled Rope). This story represents the combined effect. See affected_constraints for related network dependencies.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(tls_version_fragmentation, institutional, 0.18).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
