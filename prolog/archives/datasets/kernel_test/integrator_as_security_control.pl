% ============================================================================
% CONSTRAINT STORY: integrator_as_security_control
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2025-01-02
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_integrator_as_security_control, []).

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
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: integrator_as_security_control
 *   human_readable: Integrator Positioned as Security Control in Sovereign AI Infrastructure
 *   domain: technology_governance/legal_infrastructure/sovereign_ai
 *
 * SUMMARY:
 *   The integrator-as-security-control pattern in sovereign AI infrastructure
 *   creates a structural dependency that undermines operational autonomy
 *   while claiming to enhance security. The integrator (Polaris) never holds
 *   encryption keys — a design choice that appears to preserve client control
 *   — but the client's Recovery Time Objective (RTO) depends entirely on
 *   integrator response time. This creates a lock-in mechanism masked by
 *   security framing: questioning the integrator relationship appears to
 *   question security itself. The constraint is downstream of
 *   privilege_preservation_architecture, inheriting extraction from the
 *   upstream requirement to maintain legal privilege through architectural
 *   separation. The theater_ratio (0.65) reflects that the security-control
 *   framing is substantially performative: the integrator's non-custody of
 *   keys provides compliance signaling value but does not eliminate
 *   operational control. The integrator retains control over hardware
 *   provisioning, recovery timelines, and operational continuity — the
 *   elements that matter for RTO. The constraint's extractiveness (0.58) and
 *   suppression (0.68) have increased over the 6-year interval as the
 *   integrator's position has become more entrenched and alternative
 *   architectures have become more costly to implement.
 *
 * KEY AGENTS:
 *   - Client Operational Autonomy: Primary victim (powerless/trapped) — structurally locked into integrator dependency by RTO requirement; cannot exit without catastrophic operational failure
 *   - Sovereign AI Independence: Secondary victim (moderate/constrained) — faces high exit costs and political barriers; the security-control framing makes exit appear to compromise security
 *   - Integrator Polaris: Primary beneficiary (institutional/arbitrage) — captures recurring revenue and operational control while claiming to provide security value; can exit or renegotiate at will
 *   - Regulatory Oversight Coalition: Organized observer (organized/mobile) — sees both coordination (audit trails, key custody documentation) and extraction (vendor lock-in, RTO dependency); can mandate alternative architectures
 *   - Alternative Integrator Consortium: Competing providers (powerful/mobile) — face barriers to entry from Polaris's established audit infrastructure; see the constraint as both industry standard and extraction mechanism
 *   - Analytical Observer: Civilizational view (analytical/analytical) — identifies the integrator-as-security-control pattern as structural extraction that undermines sovereign autonomy
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(integrator_as_security_control, 0.58).
domain_priors:suppression_score(integrator_as_security_control, 0.68).
domain_priors:theater_ratio(integrator_as_security_control, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(integrator_as_security_control, extractiveness, 0.58).
narrative_ontology:constraint_metric(integrator_as_security_control, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(integrator_as_security_control, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(integrator_as_security_control, snare).
narrative_ontology:human_readable(integrator_as_security_control, "Integrator Positioned as Security Control in Sovereign AI Infrastructure").
narrative_ontology:topic_domain(integrator_as_security_control, "technology_governance/legal_infrastructure/sovereign_ai").

domain_priors:requires_active_enforcement(integrator_as_security_control).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(integrator_as_security_control, integrator_polaris).
narrative_ontology:constraint_victim(integrator_as_security_control, client_operational_autonomy).
narrative_ontology:constraint_victim(integrator_as_security_control, sovereign_ai_independence).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CLIENT OPERATIONAL AUTONOMY (SNARE) — The client organization cannot exit the integrator dependency without catastrophic RTO failure. Despite never holding encryption keys, the integrator controls recovery timelines, hardware provisioning, and operational continuity. The security-control framing masks extraction: what appears as a security boundary is actually a dependency lock. Maximum experienced extraction — the client is structurally trapped by the RTO requirement.
constraint_indexing:constraint_classification(integrator_as_security_control, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: SOVEREIGN AI INDEPENDENCE (SNARE) — The sovereign AI initiative faces high exit costs: switching integrators requires re-architecting the entire recovery pathway, retraining operations staff, and accepting extended RTO during transition. The integrator's position as 'security control' creates a legitimacy barrier to exit — questioning the arrangement appears to question security itself. Constrained rather than trapped because exit is technically possible, but the costs are prohibitive and the framing makes exit politically difficult.
constraint_indexing:constraint_classification(integrator_as_security_control, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: INTEGRATOR POLARIS (ROPE) — The integrator experiences the arrangement as pure coordination: they provide genuine security value by maintaining hardware-agnostic recovery capability and audit trails. The RTO dependency is a natural consequence of their expertise, not extraction. They can exit or renegotiate at will (arbitrage exit options) and see the relationship as mutually beneficial. Net beneficiary — the constraint subsidizes their position.
constraint_indexing:constraint_classification(integrator_as_security_control, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(continental))).

% PERSPECTIVE 4: REGULATORY OVERSIGHT COALITION (TANGLED ROPE) — Organized regulators and auditors see both coordination (the integrator provides verifiable audit trails and key custody documentation) and extraction (the RTO dependency creates vendor lock-in that undermines sovereign autonomy). They have mobile exit options — they can mandate alternative architectures or require multi-integrator redundancy. The constraint is a hybrid: genuine security coordination layered with asymmetric extraction.
constraint_indexing:constraint_classification(integrator_as_security_control, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(continental))).

% PERSPECTIVE 5: ALTERNATIVE INTEGRATOR CONSORTIUM (TANGLED ROPE) — Competing integrators see the arrangement as both coordination (establishing industry standards for security controls) and extraction (Polaris's first-mover advantage creates barriers to entry). They have mobile exit options — they can build alternative offerings — but face significant coordination costs to match Polaris's established audit infrastructure. Mixed experience: the constraint both enables and constrains their market entry.
constraint_indexing:constraint_classification(integrator_as_security_control, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (SNARE) — From a civilizational perspective, the integrator-as-security-control pattern is a structural extraction mechanism that undermines sovereign AI autonomy. The RTO dependency is not a natural consequence of security architecture but a contingent design choice that concentrates control. The analytical observer sees high extractiveness, high suppression, and significant theater (the security-control framing obscures the dependency lock). This is a snare at the analytical level — the constraint extracts from sovereign autonomy with minimal coordination benefit.
constraint_indexing:constraint_classification(integrator_as_security_control, snare,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(integrator_as_security_control_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(integrator_as_security_control, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(integrator_as_security_control, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(integrator_as_security_control, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(integrator_as_security_control_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The integrator captures recurring revenue and operational control through the RTO dependency. The extraction is not maximal because the integrator does provide genuine audit and recovery services, but the dependency lock extracts more value than the coordination function justifies. The value has increased from 0.42 to 0.58 over the interval as the integrator's position has become more entrenched. Suppression (0.68): High. Significant barriers to exit include: (1) re-architecting the entire recovery pathway, (2) retraining operations staff, (3) accepting extended RTO during transition, (4) political cost of appearing to compromise security by questioning the integrator relationship. The suppression has increased from 0.55 to 0.68 as the integrator's position has become normalized and alternative architectures have become more costly. Theater ratio (0.65): Moderate-high. The security-control framing is substantially performative. The integrator's non-custody of encryption keys provides compliance signaling value but does not eliminate operational control — the integrator retains control over the elements that matter for RTO (hardware provisioning, recovery timelines, operational continuity). The theater has increased from 0.45 to 0.65 as the gap between the security framing and the operational reality has widened.
 *
 * PERSPECTIVAL GAP:
 *   The integrator sees pure coordination (Rope) — they provide genuine security value and the RTO dependency is a natural consequence of their expertise. The client sees pure extraction (Snare) — they are structurally trapped by the RTO requirement and the security-control framing masks the dependency lock. The regulatory coalition sees a hybrid (Tangled Rope) — genuine security coordination layered with asymmetric extraction. The analytical observer sees a snare at the civilizational level — the constraint extracts from sovereign autonomy with minimal coordination benefit. The perspectival gap is diagnostic: the beneficiary's rope is their genuine experience, but the victim's snare is the structural reality. The constraint resolves the mandatrophy by showing that both perspectives are legitimate readings of the same structural data — the question is not 'which type is correct?' but 'which perspective are you measuring from?'
 *
 * DIRECTIONALITY LOGIC:
 *   The client organization (powerless/trapped) is the primary victim — they bear the full cost of the RTO dependency and cannot exit. Directionality is derived as high d (victim + trapped exit) → high f(d) → high chi. The sovereign AI initiative (moderate/constrained) is a secondary victim with high but surmountable exit costs — directionality is derived as moderate-high d (victim + constrained exit) → moderate-high f(d) → moderate-high chi. The integrator (institutional/arbitrage) is the primary beneficiary — they capture value from the dependency lock. Directionality is derived as low d (beneficiary + arbitrage exit) → negative f(d) → negative chi (net subsidy). The regulatory coalition (organized/mobile) sees mixed extraction — they have agency to mandate alternatives but face coordination costs. Directionality is derived as moderate d (mixed + mobile exit) → moderate f(d) → moderate chi. The alternative integrator consortium (powerful/mobile) faces barriers to entry but has agency to build alternatives — directionality is derived as moderate d (mixed + mobile exit) → moderate f(d) → moderate chi. The analytical observer (analytical/analytical) uses the canonical fallback d for analytical contexts, producing high chi that reflects the constraint's structural extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint demonstrates mandatrophy resolution through perspectival decomposition. The integrator's rope classification is not false — they genuinely experience the relationship as coordination. The client's snare classification is not false — they genuinely experience structural entrapment. The analytical observer's snare classification reflects the constraint's net effect on sovereign autonomy. The mandatrophy is resolved by recognizing that all three classifications are valid from their respective perspectives. The constraint is simultaneously coordination (from the beneficiary's view), extraction (from the victim's view), and a hybrid (from the organized observer's view). The presheaf over the observation site IS the answer — there is no single 'correct' type.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    rto_dependency_necessity,
    'Is the RTO dependency on integrator response time a necessary consequence of hardware-agnostic recovery architecture, or could alternative designs achieve comparable security with lower dependency?',
    'Technical analysis of recovery architectures; comparison of RTO metrics across different integrator models; identification of design choices that create vs. eliminate response-time dependency',
    'If necessary: the constraint is coordination with unavoidable extraction (Tangled Rope from more perspectives). If contingent: the constraint is extraction masked as security (Snare from more perspectives).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(rto_dependency_necessity, empirical, 'Whether RTO dependency is architecturally necessary or a design choice').

omega_variable(
    key_custody_theater,
    'Does the integrator''s non-custody of encryption keys provide genuine security value, or is it primarily theatrical — a compliance signal that obscures operational control?',
    'Security audit of actual breach scenarios; analysis of what operational control the integrator retains despite non-custody; comparison of security outcomes in custody vs. non-custody models',
    'If genuine: the security-control framing is accurate (lower theater_ratio). If theatrical: the framing is a cover story for dependency lock (higher theater_ratio, stronger snare classification).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(key_custody_theater, empirical, 'Whether key non-custody provides genuine security or is primarily theatrical').

omega_variable(
    multi_integrator_feasibility,
    'Is multi-integrator redundancy technically feasible for sovereign AI infrastructure, or does the coordination overhead make it prohibitively expensive?',
    'Cost-benefit analysis of multi-integrator architectures; case studies of organizations that have implemented redundancy; identification of coordination costs and failure modes',
    'If feasible: the single-integrator lock is a choice, not a necessity (stronger snare classification, higher suppression). If prohibitive: the lock is a structural feature of the domain (lower suppression, more coordination justification).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(multi_integrator_feasibility, empirical, 'Whether multi-integrator redundancy is feasible for sovereign AI').

omega_variable(
    upstream_privilege_coupling,
    'Does the integrator-as-security-control pattern inherit extraction from the upstream privilege_preservation_architecture constraint, or is it an independent extraction mechanism?',
    'Network analysis of how privilege preservation requirements shape integrator positioning; identification of whether the security-control framing would exist without the upstream constraint',
    'If inherited: the extraction is contamination from upstream (network coupling). If independent: the integrator relationship is a separate extraction layer (additive extraction).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(upstream_privilege_coupling, conceptual, 'Whether extraction is inherited from upstream privilege_preservation_architecture').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(integrator_as_security_control, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(iasc_theater_t0, integrator_as_security_control, theater_ratio, 0, 0.45).
narrative_ontology:measurement(iasc_theater_t3, integrator_as_security_control, theater_ratio, 3, 0.55).
narrative_ontology:measurement(iasc_theater_t6, integrator_as_security_control, theater_ratio, 6, 0.65).

% Extraction over time
narrative_ontology:measurement(iasc_extract_t0, integrator_as_security_control, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(iasc_extract_t3, integrator_as_security_control, base_extractiveness, 3, 0.5).
narrative_ontology:measurement(iasc_extract_t6, integrator_as_security_control, base_extractiveness, 6, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(iasc_suppress_t0, integrator_as_security_control, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(iasc_suppress_t3, integrator_as_security_control, suppression_requirement, 3, 0.62).
narrative_ontology:measurement(iasc_suppress_t6, integrator_as_security_control, suppression_requirement, 6, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(integrator_as_security_control, enforcement_mechanism).

% DUAL FORMULATION NOTE:
% This constraint is downstream of privilege_preservation_architecture. The upstream constraint creates the requirement for architectural separation to maintain legal privilege; this constraint implements that separation through integrator positioning. The extraction in this constraint is partly inherited from upstream (the privilege requirement creates the dependency structure) and partly independent (the integrator's specific positioning as security control adds an additional extraction layer).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
