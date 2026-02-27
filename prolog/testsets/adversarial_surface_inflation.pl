% ============================================================================
% CONSTRAINT STORY: adversarial_surface_inflation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_adversarial_surface_inflation, []).

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
 *   constraint_id: adversarial_surface_inflation
 *   human_readable: The Infinite Vulnerability Horizon
 *   domain: technological/cybernetic/security
 *
 * SUMMARY:
 *   The Infinite Vulnerability Horizon describes a structural constraint
 *   where the increasing complexity and interconnectedness of digital systems
 *   creates a non-linear expansion of possible attack vectors that outpaces
 *   defense capabilities. Users, operators, and defenders find themselves in
 *   an asymptotic arms race where complete enumeration of the attack surface
 *   becomes computationally intractable, while security vendors benefit from
 *   the persistent market for vulnerability management. The constraint
 *   exhibits tangled-rope properties: it provides genuine coordination
 *   benefits (threat intelligence, standardized frameworks, collective
 *   defensive posture) while simultaneously extracting from those who bear
 *   the asymmetric cost of the expanding horizon. Theater ratio (0.64)
 *   reflects that significant defensive effort (penetration testing,
 *   compliance auditing, vulnerability scanning) is performative — it
 *   demonstrates security awareness and reduces auditable risk but does not
 *   proportionally reduce actual breach probability as complexity grows. The
 *   rise of zero-trust architecture represents a genuine structural
 *   transition with sunset clause: if widely adopted, it reframes the problem
 *   away from surface enumeration toward continuous verification, reducing
 *   theater and extraction both.
 *
 * KEY AGENTS:
 *   - System Users and Critical Infrastructure Operators: Primary victims (powerless/trapped) — bear extraction cost of infinite horizon, cannot opt out of digital connectivity
 *   - Security Operations Teams: Secondary victims (moderate/constrained) — constrained by resource limits, forced to choose between coverage depth and breadth, benefit from vendor tools while being exploited by surface expansion
 *   - Security Vendors and Defensive Contractors: Primary beneficiaries (institutional/arbitrage) — capture persistent market value from vulnerability discovery, threat intelligence licensing, managed services, and compliance frameworks
 *   - Zero-Trust Architecture Coalition: Organized agents (organized/constrained) — NIST, cloud providers, enterprise architects building alternative paradigm with measurable sunset path
 *   - Perimeter Defense Doctrine Institutions: Institutional inertia actors (institutional/arbitrage) — compliance frameworks, regulatory bodies, legacy infrastructure operators maintain castle-and-moat paradigm despite acknowledged limits
 *   - Analytical Observer: Sees potential mountain (computational limits) but risks naturalizing contingent architectural choices
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(adversarial_surface_inflation, 0.52).
domain_priors:suppression_score(adversarial_surface_inflation, 0.68).
domain_priors:theater_ratio(adversarial_surface_inflation, 0.64).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(adversarial_surface_inflation, extractiveness, 0.52).
narrative_ontology:constraint_metric(adversarial_surface_inflation, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(adversarial_surface_inflation, theater_ratio, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(adversarial_surface_inflation, tangled_rope).
narrative_ontology:human_readable(adversarial_surface_inflation, "The Infinite Vulnerability Horizon").
narrative_ontology:topic_domain(adversarial_surface_inflation, "technological/cybernetic/security").

domain_priors:requires_active_enforcement(adversarial_surface_inflation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(adversarial_surface_inflation, security_vendors).
narrative_ontology:constraint_beneficiary(adversarial_surface_inflation, defensive_contractors).
narrative_ontology:constraint_victim(adversarial_surface_inflation, system_users).
narrative_ontology:constraint_victim(adversarial_surface_inflation, critical_infrastructure_operators).
narrative_ontology:constraint_victim(adversarial_surface_inflation, collective_cybersecurity_resilience).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: END-USER / CRITICAL INFRASTRUCTURE OPERATOR (SNARE) — Trapped within digital ecosystems with no meaningful exit option. Bearing full cost of vulnerability expansion: software patches never complete the attack surface, supply chain interdependencies create hidden exposures, firmware updates introduce new vectors. Maximum extraction experience — cannot opt out of digital infrastructure, faces asymmetric information about vulnerabilities.
constraint_indexing:constraint_classification(adversarial_surface_inflation, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: SECURITY OPERATIONS TEAM (TANGLED ROPE) — Constrained by budget limitations, staffing scarcity, and the geometric explosion of monitoring requirements. Experiences mixed coordination and extraction: benefits from standardized threat intelligence and vendor tools (coordination function), but forced into reactive posture by the infinite horizon problem. Active enforcement required — teams must choose between coverage depth and coverage breadth, neither achievable.
constraint_indexing:constraint_classification(adversarial_surface_inflation, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: SECURITY VENDOR ECOSYSTEM (ROPE) — Primary beneficiary. Experiences constraint as coordination opportunity: the expanding attack surface creates persistent demand for vulnerability scanning, penetration testing, compliance frameworks, and managed security services. No exit costs; arbitrage benefits accumulate. Vendor revenue cycles align with vulnerability discovery rates — inflation of threat surface directly inflates addressable market.
constraint_indexing:constraint_classification(adversarial_surface_inflation, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: ZERO-TRUST ARCHITECTURE COALITION (SCAFFOLD) — Organized agents (NIST, cloud providers, enterprise architecture standards bodies) proposing architectural sunset: micro-segmentation, continuous verification, and principle-of-least-privilege networks designed to decouple security enforcement from surface enumeration. Theater ratio declining as operational evidence accumulates. Exit path credible but requires multi-year architectural transitions — has sunset clause as perimeter-security paradigm yields to zero-trust model.
constraint_indexing:constraint_classification(adversarial_surface_inflation, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: PERIMETER DEFENSE DOCTRINE (PITON) — Traditional network security (firewalls, intrusion detection, DMZs, castle-and-moat architecture) is substantially performative: assumes enumerable threats and defensible boundaries, but modern interconnected systems have neither. The doctrine persists through institutional inertia, compliance checkbox requirements, and vendor investment despite acknowledged failure modes. Theater ratio (0.64) reflects the gap between the ritual of perimeter hardening and actual threat reduction in distributed architectures.
constraint_indexing:constraint_classification(adversarial_surface_inflation, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / COMPUTATIONAL LIMITS VIEW (MOUNTAIN) — From a civilizational perspective, the vulnerability horizon may be inherent to computation itself: the halting problem, Gödel's incompleteness, and Rice's theorem establish fundamental limits on static verification. No system can prove its own security properties without exceeding its own logical framework. This perspective naturalizes the vulnerability explosion as a law of computation. However, the structural data reveals this as false summit: the extraction and suppression metrics (ε=0.52, σ=0.68) contradict the mountain threshold. The 'incompleteness' framing masks contingent choices about architecture, disclosure, and supply-chain interdependence.
constraint_indexing:constraint_classification(adversarial_surface_inflation, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(adversarial_surface_inflation_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(adversarial_surface_inflation, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(adversarial_surface_inflation, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(adversarial_surface_inflation, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(adversarial_surface_inflation, TR),
    TR >= 0.70.

:- end_tests(adversarial_surface_inflation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The base extraction reflects that the expanding attack surface creates disproportionate cost burdens on defenders (nonlinear monitoring requirements, supply-chain auditing, continuous patch management) while concentrating gains in the vendor ecosystem. The extraction is not maximal because legitimate coordination benefits exist: threat intelligence sharing, standard frameworks, and collective learning reduce individual burden below what isolated defense would cost. However, extraction exceeds coordination because vendors benefit from surface growth while defenders bear asymmetric costs. Suppression (0.68): High. Significant barriers to exit include: (1) no practical alternative to digital interconnectedness, (2) supply-chain dependencies create invisible attack surfaces, (3) firmware/hardware cannot be fully updated or audited, (4) zero-day asymmetry between attacker advantages and defender timing constraints, (5) disclosure incentive asymmetries suppress architectural redesign discussion. Theater ratio (0.64): Moderate-high. Significant portion of defensive effort is performative: vulnerability scanning identifies fixable instances but does not eliminate class-level vulnerabilities; penetration testing demonstrates access paths but assumes attackers will use those paths; compliance frameworks create audit evidence but correlate weakly with actual breach rates; perimeter hardening appears effective until it is circumvented via supply-chain or insider vectors. Theater has increased from 0.48 to 0.64 over the interval as defensive complexity outpaced effectiveness.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates maximum perspectival divergence. The vendor sees Rope: pure coordination gains from threat intelligence and defensive tool adoption, no extraction experienced, high arbitrage value. The end-user sees Snare: complete extraction, no exit, maximum suppression. The defender sees Tangled Rope: mixed coordination (threat sharing, standards) and extraction (budget pressure, scope creep). The zero-trust coalition sees Scaffold: temporary problem with a real exit path via architectural transition. The perimeter defense establishment sees Piton: their own doctrine acknowledged as degraded but maintained for compliance/inertia reasons. The analytical observer risks seeing Mountain: treating computational limits as immutable law — but the metrics contradict this. The perspectival gap is driven by exit options (vendors have arbitrage, users have none) and beneficiary/victim status (vendors benefit, users bear costs), not by disagreement about the underlying technical facts.
 *
 * DIRECTIONALITY LOGIC:
 *   Each agent's directionality (d) reflects their structural position in the extraction flow. Security vendors have d≈0.05 (beneficiary + arbitrage exit = low d, negative f(d)) — they benefit and can arbitrage freely between vendor platforms and customer segments. End-users have d≈0.95 (victim + trapped exit = high d, f(d)≈1.42) — they bear extraction costs with no exit option. Security ops teams have d≈0.60 (victim + constrained exit = moderate d, f(d)≈0.90) — they experience extraction but retain some agency through tool selection and architecture recommendations. The zero-trust coalition has d≈0.50 (both victim and beneficiary, but with mobile/organized exit = symmetric, f(d)≈0.65) — they have capacity to transition away from perimeter paradigm. The mountain perspective has d≈0.72 (analytical observer, derived canonically from analytical power atom) but is revealed as false summit by the structural data.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION: The constraint is tangled_rope, not pure_rope, because it combines genuine coordination (threat intelligence, standardized defenses, collective learning) with asymmetric extraction (vendors gain persistent revenue from surface expansion, while end-users bear non-linear cost increase). The coordination function is real and necessary — isolated defense is catastrophically expensive; threat sharing and standardization reduce collective cost. But the extraction layer is also real: vendor business models profit from vulnerability discovery rates, and supply-chain interdependencies make complete enumeration impossible, creating permanent market for vulnerability management services. The mandatrophy would arise if one tried to classify this as pure Rope (coordination only, no extraction) — that would ignore the vendor capture of margins and the user asymmetry. The alternative error would be classifying as pure Snare (extraction only, no coordination) — that would ignore that threat intelligence and standards genuinely reduce marginal defensive cost. Tangled Rope captures both: coordination benefits + asymmetric extraction + active enforcement required = true hybrid. The scaffold perspective (zero-trust) offers a genuine sunset: if widespread adoption shifts from perimeter-enumeration to continuous-verification, the surface-counting game loses relevance, and the vendor model must adapt to service maintenance rather than vulnerability discovery.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    surface_discovery_rate,
    'Is the apparent exponential growth in vulnerability count a function of expanding actual attack surface or of improving discovery/disclosure mechanisms?',
    'Comparative analysis: CVE/CVSS distributions over time; correlation between disclosure rates and detection capability improvements; firmware security auditing depth vs discovered vulnerabilities',
    'If discovery-driven: much apparent expansion is observational artifact — underlying surface may be growing sublinearly. Reframes constraint as piton (degraded monitoring theater). If surface-driven: non-linear system complexity is structurally producing new vectors faster than enumerable defense. Constrains constraint as snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(surface_discovery_rate, empirical, 'Whether vulnerability growth reflects real surface expansion or improved discovery').

omega_variable(
    microarchitecture_transitive_closure,
    'Do supply-chain security dependencies form a transitive closure that makes any system vulnerable to any upstream component''s compromise?',
    'Dependency graph analysis: formal computation of transitive vulnerability propagation paths; case studies of real supply-chain attacks (SolarWinds, 3CX, Codecov); feasibility assessment of complete supply-chain decoupling',
    'If true transitive closure: perimeter defense is structurally impossible — surface is infinite by necessity, not by choice. Constraint becomes mathematical mountain (ε→0.05). If bounded by containment: selective decoupling strategies enable surface management. Maintains tangled-rope or scaffold classification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(microarchitecture_transitive_closure, empirical, 'Whether supply-chain dependencies create transitive vulnerability').

omega_variable(
    zero_trust_operational_maturity,
    'Can zero-trust architectures actually reduce the *effective* attack surface (not just conceptually reframe it) compared to perimeter defense, accounting for implementation overhead and new zero-trust-specific vulnerabilities?',
    'Longitudinal case studies: organizations transitioning to zero-trust; comparison of incident rates, mean-time-to-detect, attack complexity, and remediation cost before/after deployment; measurement of ''security theater'' ratio in zero-trust operations vs traditional perimeter',
    'If zero-trust operationally superior: scaffold perspective confirmed — sunset is real and measurable. If parity with overhead: zero-trust is a lateral move, not a sunset — constraint persists under different name. If net negative: zero-trust introduces new vulnerabilities that offset gains — scaffold is aspirational.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(zero_trust_operational_maturity, empirical, 'Whether zero-trust reduces effective attack surface vs perimeter defense').

omega_variable(
    disclosure_incentive_asymmetry,
    'Does the vendor incentive structure (revenue from vulnerability discovery and remediation) drive suppression of disclosure of systemic architectural flaws that would require full redesign?',
    'Historical analysis: delay patterns in disclosure of class-level vulnerabilities vs instance-level CVEs; vendor communication patterns when facing redesign-scale vs patch-scale flaws; comparison of disclosed vs undisclosed vulnerability categories by estimated remediation cost',
    'If asymmetry confirmed: extraction component is behavioral (vendor incentive suppression) not structural (computational limits). Opens policy intervention opportunities. Clarifies mandatrophy: coordination in threat intelligence vs extraction in incentive structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(disclosure_incentive_asymmetry, empirical, 'Whether vendor incentives suppress disclosure of systemic flaws').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(adversarial_surface_inflation, 0, 14).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(adv_surf_tr_t0, adversarial_surface_inflation, theater_ratio, 0, 0.48).
narrative_ontology:measurement(adv_surf_tr_t7, adversarial_surface_inflation, theater_ratio, 7, 0.58).
narrative_ontology:measurement(adv_surf_tr_t14, adversarial_surface_inflation, theater_ratio, 14, 0.64).

% Extraction over time
narrative_ontology:measurement(adv_surf_be_t0, adversarial_surface_inflation, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(adv_surf_be_t7, adversarial_surface_inflation, base_extractiveness, 7, 0.4).
narrative_ontology:measurement(adv_surf_be_t14, adversarial_surface_inflation, base_extractiveness, 14, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(adversarial_surface_inflation, information_standard).
narrative_ontology:affects_constraint(adversarial_surface_inflation, supply_chain_microarchitectural_vulnerability).
narrative_ontology:affects_constraint(adversarial_surface_inflation, firmware_update_risk_accumulation).
narrative_ontology:affects_constraint(adversarial_surface_inflation, zero_day_asymmetry).

% DUAL FORMULATION NOTE:
% The Infinite Vulnerability Horizon is a higher-level constraint that emerges from the interaction of microarchitectural complexity (supply-chain interdependencies), firmware updateability (risk of patch-induced vulnerabilities), and attacker/defender timing asymmetries (zero-day discovery and exploitation). The upstream constraints represent specific technical failure modes; this constraint captures the systemic growth phenomenon across all modes combined. Each upstream constraint contributes to surface inflation nonlinearly.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
