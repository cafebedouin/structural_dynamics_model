% ============================================================================
% CONSTRAINT STORY: physical_airgap_authenticity
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2025-02-14
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_physical_airgap_authenticity, []).

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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    domain_priors:emerges_naturally/1,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: physical_airgap_authenticity
 *   human_readable: Physical Air-Gap Authenticity Constraint
 *   domain: infrastructure_security/disaster_recovery/digital_sovereignty
 *
 * SUMMARY:
 *   The physical air-gap authenticity constraint distinguishes between
 *   genuine physical isolation (network hardware physically absent, device in
 *   Faraday-shielded safe) and virtual air-gapping (logical segmentation on
 *   connected hardware). This is a candidate mountain constraint — the
 *   physical impossibility of transmitting information across a gap with no
 *   physical medium — but with identifiable beneficiaries (compliance
 *   frameworks, hardware vendors, security auditors) that trigger false
 *   summit detection. The constraint's extractiveness (0.08) is very low,
 *   reflecting that the physical limitation is genuine and the beneficiaries'
 *   advantage derives from solving a real coordination problem (verifiable
 *   isolation) rather than from constructed scarcity. The theater ratio
 *   (0.15) is low, indicating that physical air-gap protocols have minimal
 *   performative content — the verification mechanism (physical inspection of
 *   hardware, electromagnetic emanation testing, access control logs)
 *   directly tests the constraint's enforcement. The constraint exhibits
 *   rising extractiveness over the interval (0.05 → 0.08) as the market for
 *   air-gap hardware matures and vendors capture more value, but the increase
 *   is modest and does not indicate rent-seeking layered onto coordination.
 *
 * KEY AGENTS:
 *   - Isolated System Under Attack: Experiences the constraint as absolute physical law (powerless/trapped) — no network interface means no remote access
 *   - Security Architect: Designs systems around the constraint as immutable boundary (moderate/constrained) — virtual segmentation can be bypassed, physical gaps cannot
 *   - Nation-State Adversary: Faces the constraint as hard limit on remote access capabilities (institutional/arbitrage) — can compromise endpoints or protocols but not the gap itself
 *   - Information Theorist: Analytical observer (analytical/analytical) — sees the constraint as consequence of locality and no-cloning theorem
 *   - Compliance Framework Operator: Beneficiary (institutional/arbitrage) — the constraint provides auditable boundary that satisfies regulatory requirements
 *   - Air-Gap Hardware Vendor: Beneficiary (institutional/arbitrage) — the constraint creates legitimate market for high-assurance isolation hardware
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(physical_airgap_authenticity, 0.08).
domain_priors:suppression_score(physical_airgap_authenticity, 0.03).
domain_priors:theater_ratio(physical_airgap_authenticity, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(physical_airgap_authenticity, extractiveness, 0.08).
narrative_ontology:constraint_metric(physical_airgap_authenticity, suppression_requirement, 0.03).
narrative_ontology:constraint_metric(physical_airgap_authenticity, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(physical_airgap_authenticity, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(physical_airgap_authenticity, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(physical_airgap_authenticity, mountain).
narrative_ontology:human_readable(physical_airgap_authenticity, "Physical Air-Gap Authenticity Constraint").
narrative_ontology:topic_domain(physical_airgap_authenticity, "infrastructure_security/disaster_recovery/digital_sovereignty").

domain_priors:emerges_naturally(physical_airgap_authenticity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(physical_airgap_authenticity, security_auditors).
narrative_ontology:constraint_beneficiary(physical_airgap_authenticity, compliance_frameworks).
narrative_ontology:constraint_beneficiary(physical_airgap_authenticity, airgap_hardware_vendors).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ISOLATED SYSTEM (MOUNTAIN) — A physically air-gapped system experiences the constraint as absolute: if the network interface is physically absent and the device is in a Faraday-shielded safe, electromagnetic side-channel exfiltration is physically impossible at immediate timescales. The constraint is immutable within the threat model — no amount of software sophistication can bridge a physical gap that doesn't exist.
constraint_indexing:constraint_classification(physical_airgap_authenticity, mountain,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: SECURITY ARCHITECT (MOUNTAIN) — Designing high-assurance systems, the architect experiences physical air-gapping as a hard constraint: virtual segmentation (VLANs, software firewalls, administrative policies) can always be bypassed by sufficiently sophisticated attackers, but a SATA drive rotated through a safe with no network hardware present cannot be remotely compromised. The constraint is a physical law of information theory — bits cannot traverse a gap that has no physical medium.
constraint_indexing:constraint_classification(physical_airgap_authenticity, mountain,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: NATION-STATE ADVERSARY (MOUNTAIN) — Even state-level actors with access to supply chain interdiction, electromagnetic surveillance, and insider recruitment face the physical air-gap as an immutable barrier at the point of enforcement. A device with no network interface, no wireless radios, and no electromagnetic leakage (proper shielding) cannot be remotely accessed. The adversary can compromise the rotation protocol (human factors), the safe (physical access), or the endpoints (before/after air-gap), but cannot bypass the gap itself while it is enforced.
constraint_indexing:constraint_classification(physical_airgap_authenticity, mountain,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: ANALYTICAL OBSERVER (MOUNTAIN) — From the analytical perspective, the physical air-gap constraint is a direct consequence of the no-cloning theorem and the locality principle in physics: information cannot be transmitted without a physical carrier, and physical carriers cannot traverse a gap instantaneously. Virtual air-gaps (logical segmentation on connected hardware) are not air-gaps in the physical sense — they are policy constraints that can be violated by software. The distinction is categorical, not gradual. This is a genuine natural law, not a naturalized institutional arrangement.
constraint_indexing:constraint_classification(physical_airgap_authenticity, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 5: COMPLIANCE FRAMEWORK (ROPE) — Organizations operating under security compliance regimes (NIST 800-53, IEC 62443, national sovereignty frameworks) experience the physical air-gap as a coordination mechanism: the constraint provides a verifiable, auditable boundary that satisfies regulatory requirements and enables certification. The compliance operator benefits from the constraint's clarity — 'network interface physically absent' is easier to audit than 'network access administratively prohibited.' Low extraction, genuine coordination function.
constraint_indexing:constraint_classification(physical_airgap_authenticity, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: HARDWARE VENDOR (ROPE) — Manufacturers of air-gap transfer devices (data diodes, one-way optical links, SATA rotation systems) experience the constraint as a coordination mechanism that creates a legitimate market for high-assurance hardware. The constraint solves a real problem (verifiable isolation) and the vendor's products are the coordination infrastructure. Beneficiary status derives from market position, but the underlying constraint is physical, not constructed.
constraint_indexing:constraint_classification(physical_airgap_authenticity, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(physical_airgap_authenticity_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(physical_airgap_authenticity, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(physical_airgap_authenticity, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(physical_airgap_authenticity, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(physical_airgap_authenticity, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(physical_airgap_authenticity, ExtMetricName, E),
    domain_priors:suppression_score(physical_airgap_authenticity, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(physical_airgap_authenticity),
    narrative_ontology:constraint_metric(physical_airgap_authenticity, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(physical_airgap_authenticity, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(physical_airgap_authenticity_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.08): Very low. The physical air-gap constraint is a genuine physical limitation — information cannot traverse a gap with no physical medium. The beneficiaries (compliance frameworks, hardware vendors) capture value by solving the coordination problem of verifiable isolation, not by constructing artificial scarcity. The modest extractiveness reflects: (1) hardware vendors' markup on specialized air-gap devices (data diodes, SATA rotation systems) above commodity hardware costs, (2) compliance frameworks' certification fees, and (3) security auditors' premium for air-gap verification services. But these are coordination costs, not extractive rents — the underlying constraint is physical. Suppression (0.03): Very low. Alternatives to physical air-gapping exist (virtual segmentation, network policies, software firewalls) and are widely deployed. The constraint does not suppress alternatives — it defines a specific assurance level that alternatives cannot match. Organizations can choose virtual air-gapping for lower-assurance use cases; the physical constraint applies only when the threat model requires it. Theater ratio (0.15): Low. Physical air-gap verification has minimal performative content. Auditors physically inspect hardware (network interfaces absent), test electromagnetic emanations (Faraday cage effectiveness), and review access logs (safe rotation protocol compliance). These are direct measurements of the constraint's enforcement, not theatrical substitutes. The modest theater reflects documentation overhead (compliance paperwork) and the gap between designed protocols (perfect rotation discipline) and operational reality (human factors, protocol violations).
 *
 * PERSPECTIVAL GAP:
 *   All perspectives classify as mountain except the compliance framework and hardware vendor, which classify as rope. This is the diagnostic signature of a genuine natural law with coordination infrastructure built on top. The isolated system, security architect, nation-state adversary, and information theorist all experience the constraint as physically immutable — information cannot traverse a gap with no physical medium. The compliance framework and hardware vendor experience the constraint as a coordination mechanism — it solves the problem of verifiable isolation and creates a market for high-assurance hardware. The gap is not a contradiction — the physical constraint is real (mountain from most perspectives), and the coordination infrastructure is also real (rope from beneficiary perspectives). The false summit detector will flag this constraint because beneficiaries are declared, but the omega variables document that the beneficiaries' advantage derives from solving a genuine coordination problem on top of a genuine physical constraint, not from naturalizing a constructed arrangement.
 *
 * DIRECTIONALITY LOGIC:
 *   The constraint has three beneficiary groups and no victims, which is unusual for a mountain and triggers false summit detection. However, the beneficiaries' advantage derives from the constraint's genuine physical properties, not from naturalization of a constructed arrangement. Compliance frameworks benefit because the physical air-gap provides an auditable boundary that satisfies regulatory requirements — the constraint solves a real coordination problem (how to verify isolation). Hardware vendors benefit because the constraint creates a legitimate market for high-assurance devices — the products solve a real technical problem (verifiable one-way data transfer, tamper-evident rotation systems). Security auditors benefit because the constraint is easier to verify than virtual alternatives — 'network interface physically absent' is a binary observable, while 'network access administratively prohibited' requires trust in software enforcement. All three beneficiary groups have arbitrage exit options — they can operate in other security domains if the air-gap market contracts. The directionality values are low (d ≈ 0.05-0.15 for beneficiaries) because the extraction flow runs toward them, but the flow is small because the underlying constraint is physical, not constructed. The isolated system, security architect, and nation-state adversary are not victims — they experience the constraint as an immutable physical law that they must work around, not as extraction imposed by the beneficiaries.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by demonstrating that a genuine mountain can have beneficiaries without being a false summit. The physical air-gap is an authentic natural law — the locality principle and no-cloning theorem are not institutional arrangements. But the constraint also enables coordination (compliance frameworks, hardware markets) that creates beneficiaries. The mandatrophy resolution: the mountain classification applies to the physical constraint itself (information cannot traverse a gap with no medium), while the rope classification applies to the coordination infrastructure built on top (auditable boundaries, verifiable isolation hardware). The constraint is not mislabeled — it is genuinely both. The false summit detector will flag it for review, and the omega variables will document that the beneficiaries' advantage is not evidence of naturalization but evidence of coordination layered on physics. The extractiveness is very low (0.08) because the coordination costs are real and the physical constraint is genuine. If extractiveness were higher (e.g., 0.25+), that would indicate rent-seeking layered onto the coordination, and the mountain classification would be suspect. But at 0.08, the constraint is a mountain with coordination infrastructure, not a false summit.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    electromagnetic_emanation_threshold,
    'At what distance and with what equipment does electromagnetic emanation from a physically isolated device become a practical side channel?',
    'Empirical testing of TEMPEST attack ranges under varying shielding conditions; measurement of signal-to-noise ratios for different device types and emanation frequencies',
    'If practical range > 10 meters with commodity equipment: physical air-gap is weaker than claimed, and Faraday shielding becomes mandatory rather than optional. If practical range < 1 meter with specialized equipment: physical air-gap is robust even without shielding for most threat models.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(electromagnetic_emanation_threshold, empirical, 'Practical range and equipment requirements for electromagnetic side-channel attacks').

omega_variable(
    human_protocol_compliance_rate,
    'What is the empirical compliance rate for physical air-gap rotation protocols in operational environments?',
    'Audit data from high-security facilities; incident reports of protocol violations; comparison of designed vs actual rotation procedures',
    'If compliance rate < 80%: the constraint''s effectiveness is dominated by human factors rather than physical properties, and the mountain classification applies only to the idealized protocol, not the operational reality. If compliance rate > 95%: the physical constraint is the dominant factor.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(human_protocol_compliance_rate, empirical, 'Operational compliance rate for air-gap rotation protocols').

omega_variable(
    supply_chain_interdiction_prevalence,
    'How frequently are physical air-gap systems compromised via supply chain interdiction (hardware implants installed before deployment)?',
    'Classified intelligence assessments; public incident disclosures; hardware teardown audits of high-security deployments',
    'If interdiction is common: the air-gap constraint is bypassed before it is enforced, and the mountain classification applies only to the post-deployment state, not the end-to-end security posture. If interdiction is rare: the physical constraint is the primary security boundary.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(supply_chain_interdiction_prevalence, empirical, 'Prevalence of supply chain compromise of air-gapped systems').

omega_variable(
    virtual_airgap_sufficiency_threshold,
    'Under what threat models is virtual air-gapping (logical segmentation on connected hardware) functionally equivalent to physical air-gapping?',
    'Comparative analysis of breach rates for physically vs virtually air-gapped systems; formal verification of segmentation enforcement mechanisms; adversary capability modeling',
    'If virtual air-gaps are sufficient for most threat models: the physical constraint is over-specified, and the market for physical air-gap hardware is partly extractive (selling unnecessary assurance). If virtual air-gaps are insufficient even for moderate threat models: the physical constraint is correctly specified.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(virtual_airgap_sufficiency_threshold, conceptual, 'Threat model conditions under which virtual air-gapping is sufficient').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(physical_airgap_authenticity, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(airgap_tr_t0, physical_airgap_authenticity, theater_ratio, 0, 0.1).
narrative_ontology:measurement(airgap_tr_t5, physical_airgap_authenticity, theater_ratio, 5, 0.12).
narrative_ontology:measurement(airgap_tr_t10, physical_airgap_authenticity, theater_ratio, 10, 0.15).

% Extraction over time
narrative_ontology:measurement(airgap_be_t0, physical_airgap_authenticity, base_extractiveness, 0, 0.05).
narrative_ontology:measurement(airgap_be_t5, physical_airgap_authenticity, base_extractiveness, 5, 0.06).
narrative_ontology:measurement(airgap_be_t10, physical_airgap_authenticity, base_extractiveness, 10, 0.08).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(physical_airgap_authenticity, information_standard).

% DUAL FORMULATION NOTE:
% This constraint is a single structural claim with a single epsilon value. Virtual air-gapping (logical segmentation) is not a different measurement of the same constraint — it is a different constraint entirely, with much higher extractiveness (administrative policies can be bypassed) and different failure modes. If virtual air-gapping were modeled, it would be a separate constraint story (likely tangled_rope or snare depending on the threat model) linked via network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
