% ============================================================================
% CONSTRAINT STORY: ai_security_attack_surface
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ai_security_attack_surface, []).

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
 *   constraint_id: ai_security_attack_surface
 *   human_readable: AI Security Attack Surface Coordination vs. Extraction
 *   domain: artificial_intelligence/cybersecurity/governance
 *
 * SUMMARY:
 *   AI systems present an expanding attack surface that creates a structural
 *   tension between security researchers' obligation to disclose
 *   vulnerabilities, developers' need time to patch, users' right to know,
 *   and attackers' incentive to exploit before patches deploy. The constraint
 *   exhibits hybrid coordination-extraction dynamics: genuine coordination
 *   function (vulnerability management, responsible disclosure frameworks,
 *   security auditing) layered with systematic extraction (information
 *   asymmetry that traps researchers and users, capability advantage that
 *   accrues to early-moving developers, zero-day exploit markets that profit
 *   from delayed patches). The theater ratio (0.64) reflects that responsible
 *   disclosure, CVE numbering, security scoring, and vulnerability bounties
 *   are partly performative — they create appearance of systematic security
 *   management while the underlying problem (unbounded attack surface growth)
 *   remains largely unmanaged. The extractiveness (0.58) indicates moderate
 *   but growing extraction driven by information asymmetry and incentive
 *   misalignment: researchers face dilemma costs (disclose → enable attack,
 *   delay → enable indefinite exploitation, stay silent → complicity); users
 *   face trapped ignorance; developers capture first-mover advantage in
 *   patching; attackers exploit windows between disclosure and patch
 *   deployment.
 *
 * KEY AGENTS:
 *   - Security Researchers: Primary victims (powerless/trapped) — face disclosure dilemma with no exit; bear reputation risk either way
 *   - End Users: Primary victims (powerless/trapped) — unaware of attack surface, cannot assess risk, cannot switch systems
 *   - AI Developers (Frontier Labs): Primary beneficiaries (institutional/arbitrage) — capture competitive advantage during disclosure windows, benefit from security frameworks that preserve proprietary control
 *   - Attackers & Threat Actors: Secondary beneficiary (powerful/mobile) — profit from vulnerability windows; arbitrage between disclosure timeline and patch deployment
 *   - Regulatory Bodies: Coordination agents (moderate/constrained) — attempt to coordinate multi-stakeholder incentives; face capture risk and limited enforcement capacity
 *   - Responsible Disclosure Framework: Institutional theater (institutional/arbitrage) — performs security management while masking structural problems
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing contingent incentive structures as immutable properties of AI systems
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_security_attack_surface, 0.58).
domain_priors:suppression_score(ai_security_attack_surface, 0.68).
domain_priors:theater_ratio(ai_security_attack_surface, 0.64).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_security_attack_surface, extractiveness, 0.58).
narrative_ontology:constraint_metric(ai_security_attack_surface, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(ai_security_attack_surface, theater_ratio, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_security_attack_surface, tangled_rope).
narrative_ontology:human_readable(ai_security_attack_surface, "AI Security Attack Surface Coordination vs. Extraction").
narrative_ontology:topic_domain(ai_security_attack_surface, "artificial_intelligence/cybersecurity/governance").

domain_priors:requires_active_enforcement(ai_security_attack_surface).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_security_attack_surface, ai_developers).
narrative_ontology:constraint_beneficiary(ai_security_attack_surface, capability_frontier_labs).
narrative_ontology:constraint_victim(ai_security_attack_surface, security_researchers).
narrative_ontology:constraint_victim(ai_security_attack_surface, downstream_users).
narrative_ontology:constraint_victim(ai_security_attack_surface, public_safety).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SECURITY RESEARCHER (SNARE) — Caught between pressure to report vulnerabilities (which enables exploitation if disclosure is premature) and pressure to not disclose (which enables indefinite exploitation). No viable exit: responsible disclosure takes months/years while attackers iterate; full disclosure causes immediate harm; staying silent means complicity. Bears full extraction cost with minimal coordination benefit.
constraint_indexing:constraint_classification(ai_security_attack_surface, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: END USER (SNARE) — No knowledge of AI system attack surface. Cannot assess risk, cannot patch, cannot exit or switch. Trapped in extraction via unequal information. Suppression is nearly total — users have no access to security posture data and no recourse if systems are compromised through known vulnerabilities.
constraint_indexing:constraint_classification(ai_security_attack_surface, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 3: REGULATORY BODY (TANGLED ROPE) — Must coordinate multi-stakeholder security governance (researchers, developers, users) but cannot fully enforce compliance. Faces extraction via regulatory capture (industry lobbying against disclosure requirements) and coordination overhead (managing conflicting incentives). High suppression of regulatory capacity itself.
constraint_indexing:constraint_classification(ai_security_attack_surface, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: AI DEVELOPER (ROPE) — Primary beneficiary. Experiences constraint as pure coordination: responsible disclosure frameworks, security auditing, vulnerability bounties all serve to strengthen security while preserving competitive advantage and limiting liability. Can arbitrage between public safety commitments and internal security practices. Net benefit from coordination mechanisms.
constraint_indexing:constraint_classification(ai_security_attack_surface, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: ATTACKER (TANGLED ROPE) — Coordinates with other threat actors through vulnerability marketplaces and exploit trading; also extracts value from security researchers' forced silences and users' lack of awareness. High mobility enables exit through jurisdictional arbitrage. Experiences constraint as hybrid: coordination with peer attackers + extraction from victims.
constraint_indexing:constraint_classification(ai_security_attack_surface, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: RESPONSIBLE DISCLOSURE (PITON) — The responsible disclosure framework persists through institutional inertia despite degraded function. Researchers perform security research, labs conduct audits, organizations publish CVEs — but the theater masks a structural problem: the gap between disclosure timeline and patch deployment remains vast, making the ritual performative. Theater ratio high because much disclosure activity (CVE numbering, CVSS scoring) is process theater rather than effective vulnerability elimination.
constraint_indexing:constraint_classification(ai_security_attack_surface, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, the constraint appears to reflect an immutable property of complex systems: any sufficiently complex artifact (software, AI model, network) has unbounded attack surface that cannot be fully secured or enumerated. The problem appears to be inherent to the domain rather than a contingent institutional arrangement. However, the structural data reveals this as a false summit — the extraction is driven by information asymmetry and incentive misalignment, not by physics or logic.
constraint_indexing:constraint_classification(ai_security_attack_surface, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ai_security_attack_surface_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(ai_security_attack_surface, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(ai_security_attack_surface, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(ai_security_attack_surface, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(ai_security_attack_surface, TR),
    TR >= 0.70.

:- end_tests(ai_security_attack_surface_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high and increasing. The constraint extracts from security researchers (forced silences, reputational cost), end users (unequal information, inability to assess risk), and the general public (accumulated unpatched vulnerabilities). The extraction is not total because: (1) developers genuinely benefit from security research and have incentive to fix vulnerabilities; (2) responsible disclosure frameworks do reduce average disclosure-to-patch timelines compared to full secrecy; (3) open-source alternatives create exit options for some users. However, extraction dominates because the gap between discovery and deployment remains large (median 30-90 days for critical vulnerabilities), attackers actively exploit during windows, and information asymmetry systematically advantages those who understand the attack surface (developers, attackers) over those who don't (users, policymakers). Suppression (0.68): High. Barriers to exit or resistance include: (1) proprietary systems make security assessment impossible for external parties; (2) disclosure brings reputational risk (attackers immediately exploit); (3) silence brings complicity and safety risk; (4) regulatory barriers limit user options and enforce NDAs; (5) market concentration means switching costs are prohibitive. Theater ratio (0.64): Moderate-high and increasing. The responsible disclosure framework, CVE numbering, CVSS scoring, security auditing, and vulnerability bounties all perform systematic security management while masking the structural problem: attack surface continues to grow faster than patch deployment can manage. The theater has increased over the interval because: (1) AI capability growth has outpaced safety investment, expanding attack surface; (2) responsible disclosure frameworks have become formalized and ritualized without fundamental efficiency improvements; (3) zero-day markets have grown, incentivizing disclosure suppression; (4) media attention to AI security creates demand for visible coordination mechanisms, driving performative activity.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates maximal perspectival divergence. Security researchers see a permanent snare (disclosure dilemma with no good option); end users see another snare (trapped ignorance); developers see pure coordination (responsible disclosure as enabling security improvement); regulators see tangled hybrid (must coordinate conflicting incentives while their capacity is suppressed); attackers see a hybrid (coordination with other attackers + extraction from victims during windows); the institutional responsible disclosure theater sees itself as degraded ritual (piton); the analytical observer risks seeing immutable natural law (unbounded attack surface is inherent to complexity). The gap reveals that 'AI security' labels a bundle of structurally distinct problems: genuine coordination challenges (making disclosure timely), extraction mechanisms (information asymmetry), theatrical performance (CVE scoring), and possibly immutable limits (if attack surface growth is truly unbounded). The constraint's claimed type (tangled_rope) reflects that the coordination function (vulnerability management) and extraction mechanisms (information capture, disclosure timing) are inseparable — you cannot remove the extraction without losing the coordination benefit, and you cannot strengthen coordination without enabling deeper extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   The beneficiary group (ai_developers, capability_frontier_labs) is structurally positioned to arbitrage between public safety commitments and internal security practices. They benefit from both the coordination framework (which enables systematic security improvement and reputational benefit) and from the extraction mechanism (information asymmetry and disclosure timing windows that give them first-mover advantage in patching). Victims (security_researchers, downstream_users, public_safety) bear the cost of information gaps, suppression of disclosure, and the time lag between vulnerability discovery and patch availability. The directive flow is clear: researchers and users have high d (→ high extraction), developers have low d (→ benefit), and the constraint preserves this asymmetry through enforcement (legal NDAs, liability shields, proprietary claims).
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY PARTIALLY RESOLVED: The constraint is classified as tangled_rope because the coordination function (systematic vulnerability management) is genuine and valuable, AND the extraction mechanism (information asymmetry + disclosure timing + market advantage) is also genuine and asymmetric. This is not a case of misconstruing extraction as coordination or vice versa. However, the constraint contains at least one false summit risk: the analytical observer's natural law view (unbounded attack surface is immutable) is likely a naturalization of contingent incentive structures. The question of whether attack surface growth is structurally inevitable or policy-fixable is unresolved and appears in the omegas. The theater ratio (0.64) indicates that the responsible disclosure framework is performing legitimacy rather than fully solving the coordination problem, which is appropriate for a tangled_rope — the framework must maintain apparent legitimacy to preserve the coordination fiction while actually redistributing extraction. If the theater ratio rises above 0.75, the classification should downgrade to piton (the coordination function has atrophied).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    disclosure_timing_paradox,
    'What disclosure timeline (responsible disclosure window) is short enough to prevent exploitation but long enough for developers to patch across heterogeneous deployment environments?',
    'Empirical analysis of historical CVE timeline data: correlation between disclosure-to-patch timeframe and actual exploitation rates; tracking of zero-day vs. N-day exploitation patterns',
    'If optimal window < 30 days: most patches deployed late; disclosure mechanism drives rather than prevents attacks. If optimal window > 180 days: window long enough to enable market-driven vulnerabilities and encourages suppression. If no window exists: the constraint is a snare, not tangled_rope.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(disclosure_timing_paradox, empirical, 'Optimal disclosure window paradox').

omega_variable(
    ai_capability_vs_safety_coupling,
    'Does increasing AI capability directly increase attack surface (more parameters, more complexity, more attack vectors) or does safety investment reduce surface faster than capability growth?',
    'Longitudinal analysis of model safety audits: comparison of attack surface size vs. model parameter count and capability benchmarks; cost curves for security research vs. capability scaling',
    'If capability growth > safety improvement: attack surface expansion is inevitable and constraint becomes structural (mountain or snare). If safety keeps pace: constraint is coordination problem (rope/tangled_rope with sunset possibility via investment).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ai_capability_vs_safety_coupling, empirical, 'Whether AI safety can scale with capability growth').

omega_variable(
    information_asymmetry_structural,
    'Is the information gap between developers and users/researchers structural (inherent to proprietary AI systems) or contingent (policy choice to keep security data proprietary)?',
    'Case studies comparing proprietary vs. open-source AI systems; analysis of security posture transparency in open-source models; policy experiments with mandatory transparency requirements',
    'If structural: users cannot exit trap; constraint is permanent snare. If contingent: transparency policy could convert to rope/scaffold; constraint is policy-fixable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(information_asymmetry_structural, conceptual, 'Whether information asymmetry is structural or policy-contingent').

omega_variable(
    collective_action_failure,
    'Can independent security researchers, developers, and users coordinate to establish shared vulnerability disclosure standards without regulatory enforcement?',
    'Analysis of existing coordination attempts (industy standards, frameworks, consortia); measurement of adoption rates and enforcement mechanisms; comparison with scenarios where regulatory enforcement is present',
    'If coordination fails: constraint is snare requiring regulatory intervention. If coordination succeeds: constraint becomes rope with minimal enforcement overhead.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(collective_action_failure, empirical, 'Feasibility of voluntary coordination on disclosure standards').

omega_variable(
    exploit_market_dynamics,
    'Does the existence of zero-day exploit markets fundamentally restructure the security game from disclosure coordination to asymmetric extraction, making responsible disclosure a false solution?',
    'Economic analysis of zero-day pricing, exploit trading patterns, and attacker incentive structure; correlation between disclosure framework adoption and zero-day market activity',
    'If markets dominate: responsible disclosure is theater masking permanent extraction. If markets are marginal: responsible disclosure addresses real coordination problem.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(exploit_market_dynamics, empirical, 'Zero-day market impact on disclosure mechanisms').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_security_attack_surface, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(aisurf_tr_t0, ai_security_attack_surface, theater_ratio, 0, 0.48).
narrative_ontology:measurement(aisurf_tr_t3, ai_security_attack_surface, theater_ratio, 3, 0.56).
narrative_ontology:measurement(aisurf_tr_t6, ai_security_attack_surface, theater_ratio, 6, 0.64).

% Extraction over time
narrative_ontology:measurement(aisurf_be_t0, ai_security_attack_surface, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(aisurf_be_t3, ai_security_attack_surface, base_extractiveness, 3, 0.5).
narrative_ontology:measurement(aisurf_be_t6, ai_security_attack_surface, base_extractiveness, 6, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ai_security_attack_surface, enforcement_mechanism).
narrative_ontology:affects_constraint(ai_security_attack_surface, ai_model_interpretability_limits).
narrative_ontology:affects_constraint(ai_security_attack_surface, ai_red_teaming_asymmetry).
narrative_ontology:affects_constraint(ai_security_attack_surface, zero_day_exploit_markets).

% DUAL FORMULATION NOTE:
% AI security attack surface is downstream of fundamental AI capability scaling and interpretability limits. The surface itself is constrained by the balance between disclosure frameworks and patch deployment timelines. Upstream constraints have their own extractiveness values reflecting the inevitability of attack surface; this story captures the coordination and extraction dynamics around managing the surface given that it exists.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(ai_security_attack_surface, institutional, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
