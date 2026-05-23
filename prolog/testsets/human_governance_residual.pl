% ============================================================================
% CONSTRAINT STORY: human_governance_residual
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2025-01-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_human_governance_residual, []).

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
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: human_governance_residual
 *   human_readable: Human Governance Residual in Technical Control Systems
 *   domain: legal_technology/information_security/professional_responsibility
 *
 * SUMMARY:
 *   The human governance residual describes the structural gap between
 *   technical access controls and actual enforcement of separation of duties
 *   in information systems. While architectural safeguards (role-based access
 *   control, multi-factor authentication, audit logging) reduce the scope of
 *   human discretion, they cannot eliminate it: credentialed administrators
 *   retain bypass authority for operational reasons (emergency access, system
 *   maintenance, incident response). This residual discretion creates a
 *   coordination-extraction hybrid. The coordination function is real —
 *   emergency access is operationally necessary when technical controls fail
 *   or block legitimate urgent actions. The extraction is also real —
 *   concentrated privilege with diffuse accountability creates moral hazard,
 *   and the documentation apparatus (policy attestations, access request
 *   forms, quarterly reviews) is largely performative theater that regulators
 *   accept as evidence despite its limited constraining power. The constraint
 *   exhibits all six DR types from different perspectives, with the
 *   analytical observer recognizing it as a genuine tangled rope rather than
 *   a false summit: zero-trust architectures demonstrate that the residual is
 *   not a natural law (technical controls CAN eliminate bypass authority
 *   through cryptographic attestation and hardware roots of trust), but the
 *   residual also serves a real coordination function that pure extraction
 *   mechanisms lack.
 *
 * KEY AGENTS:
 *   - Credentialed Administrators: Primary beneficiary (institutional/arbitrage) — capture operational flexibility and career premium from privileged access; can exit to similar roles if constraints tighten
 *   - Compliance Framework Integrity: Primary victim (powerless/trapped) — abstract collective good that cannot exit or organize; bears full cost when bypass authority undermines audit trail reliability
 *   - Audit Trail Reliability: Secondary victim (powerless/trapped) — evidentiary value collapses when administrators can perform unauditable actions
 *   - Internal Audit Function: Mixed position (moderate/constrained) — experiences both extraction (cannot enforce controls against senior administrators) and coordination (governance residual justifies ongoing audit headcount)
 *   - Zero-Trust Architecture Coalition: Organized agents (organized/mobile) — NIST, cloud providers, security vendors building cryptographic mechanisms that eliminate rather than document bypass; sees sunset pathway
 *   - Separation of Duties Documentation System: Institutional actor (institutional/constrained) — maintains performative ritual that regulators accept; sees own process as degraded (piton perspective)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(human_governance_residual, 0.58).
domain_priors:suppression_score(human_governance_residual, 0.62).
domain_priors:theater_ratio(human_governance_residual, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(human_governance_residual, extractiveness, 0.58).
narrative_ontology:constraint_metric(human_governance_residual, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(human_governance_residual, theater_ratio, 0.68).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(human_governance_residual, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(human_governance_residual, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(human_governance_residual, tangled_rope).
narrative_ontology:human_readable(human_governance_residual, "Human Governance Residual in Technical Control Systems").
narrative_ontology:topic_domain(human_governance_residual, "legal_technology/information_security/professional_responsibility").

domain_priors:requires_active_enforcement(human_governance_residual).

% --- Commitment system structure ---
narrative_ontology:cs_kernel_codification(human_governance_residual, formalized).
narrative_ontology:cs_authority_grounding(human_governance_residual, extraction).
narrative_ontology:cs_interpretation_layer_present(human_governance_residual).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(human_governance_residual, credentialed_administrators).
narrative_ontology:constraint_victim(human_governance_residual, compliance_framework_integrity).
narrative_ontology:constraint_victim(human_governance_residual, audit_trail_reliability).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: COMPLIANCE FRAMEWORK INTEGRITY (SNARE) — The abstract collective good of reliable audit trails and enforceable separation of duties cannot exit the system or organize resistance. Bears full cost of administrator bypass: when privileged users circumvent technical controls, the compliance framework's evidentiary value collapses. Maximum experienced extraction — the framework exists to constrain precisely the actors who can disable it.
constraint_indexing:constraint_classification(human_governance_residual, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: INTERNAL AUDIT FUNCTION (TANGLED ROPE) — Constrained by organizational hierarchy and resource limits, but benefits from the governance residual through job security: the persistent gap between technical controls and human discretion justifies ongoing audit headcount. Experiences both extraction (cannot enforce controls against senior administrators) and coordination (the residual creates legitimate work). Mixed structural position.
constraint_indexing:constraint_classification(human_governance_residual, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: CREDENTIALED ADMINISTRATOR (ROPE) — Benefits from bypass authority as operational flexibility. Experiences the governance residual as coordination: emergency access procedures enable legitimate incident response. Net beneficiary — extraction runs toward this agent through concentrated privilege and diffuse accountability. Can exit to similar roles at other organizations if constraints tighten.
constraint_indexing:constraint_classification(human_governance_residual, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: ZERO-TRUST ARCHITECTURE COALITION (SCAFFOLD) — Organized technical community (NIST, cloud providers, security vendors) building cryptographic attestation and hardware root-of-trust mechanisms that eliminate rather than document human bypass. Sees the governance residual as a temporary coordination problem with a sunset: as zero-trust architectures mature, the technical controls become non-bypassable by design. Estimated sunset: 15-25 years for widespread adoption in regulated industries.
constraint_indexing:constraint_classification(human_governance_residual, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: SEPARATION OF DUTIES DOCUMENTATION (PITON) — The formal documentation apparatus (policy manuals, access request forms, quarterly attestations) is largely performative. Administrators with root access can bypass separation of duties regardless of what the documentation claims. The ritual persists because regulators accept documentation as evidence, not because it constrains behavior. Theater ratio reflects this gap between documented controls and actual enforcement.
constraint_indexing:constraint_classification(human_governance_residual, piton,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (TANGLED ROPE) — From a civilizational perspective, the governance residual exhibits both genuine coordination (emergency access is operationally necessary) and asymmetric extraction (concentrated privilege with diffuse accountability creates moral hazard). The constraint is not a natural law — zero-trust architectures demonstrate that technical controls can eliminate rather than merely document bypass authority — but neither is it pure extraction. The residual serves a real coordination function (operational flexibility) while enabling extractive behavior (unauditable administrator actions).
constraint_indexing:constraint_classification(human_governance_residual, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(human_governance_residual_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(human_governance_residual, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(human_governance_residual, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(human_governance_residual, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(human_governance_residual, TR),
    TR >= 0.70.

:- end_tests(human_governance_residual_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. Credentialed administrators capture operational flexibility and career premium from bypass authority, while compliance framework integrity and audit trail reliability bear the cost of undermined evidentiary value. The extraction is not maximal (not a pure snare) because the coordination function is real — emergency access does enable legitimate incident response. But the extraction is substantial: the concentration of privilege with diffuse accountability creates moral hazard, and the documentation theater allows extractive behavior (policy circumvention, unauditable actions) to persist behind a compliance facade. Suppression (0.62): Moderate-high. Significant barriers to challenging administrator bypass include organizational hierarchy (administrators often report to executives who benefit from operational flexibility), resource asymmetry (audit functions lack technical capacity to verify controls), regulatory acceptance of documentation as evidence (removes external pressure for actual enforcement), and career risk for whistleblowers. But suppression is not total — some organizations do implement technical controls that limit bypass, and zero-trust architectures are reducing the governance residual in early-adopter environments. Theater ratio (0.68): High. Separation of duties documentation (policy manuals, access request forms, quarterly attestations) is substantially performative. Administrators with root access can bypass documented controls regardless of what the paperwork claims. The ritual persists because regulators accept documentation as evidence of compliance, not because it constrains behavior. Theater has increased over the interval as regulatory complexity has outpaced actual enforcement capacity.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates the full range of DR classification from a single set of base properties. Credentialed administrators see coordination (Rope) — emergency access procedures enable legitimate operational flexibility. The zero-trust architecture coalition sees a temporary problem with a sunset (Scaffold) — cryptographic attestation and hardware roots of trust are building pathways that eliminate rather than document bypass. The separation of duties documentation system sees its own degraded ritual (Piton) — the paperwork persists through regulatory acceptance, not through actual constraining power. Internal audit function sees mixed coordination and extraction (Tangled Rope) — the governance residual both justifies their work and prevents them from enforcing controls. Compliance framework integrity sees pure extraction (Snare) — bypass authority undermines evidentiary value with no self-correction mechanism. The analytical observer sees a genuine tangled rope rather than a false summit — the residual serves a real coordination function (emergency access is operationally necessary) while enabling extractive behavior (concentrated privilege with diffuse accountability creates moral hazard). Zero-trust architectures prove the residual is not a natural law, but the coordination function is also not mere cover story.
 *
 * DIRECTIONALITY LOGIC:
 *   Credentialed administrators are declared beneficiaries with arbitrage exit options — they capture operational flexibility from bypass authority and can move to similar privileged roles at other organizations if governance constraints tighten. This produces low directionality (d ≈ 0.15) and low or negative effective extraction (they experience the constraint as coordination). Compliance framework integrity and audit trail reliability are declared victims with trapped exit options — abstract collective goods that cannot exit the system or organize resistance. This produces high directionality (d ≈ 0.95) and maximum effective extraction (they bear the full cost of undermined evidentiary value). Internal audit function is a victim with constrained exit options — experiences extraction (cannot enforce controls against senior administrators) but also benefits from the governance residual through job security (the persistent gap justifies ongoing audit headcount). This produces moderate directionality (d ≈ 0.55) and moderate effective extraction (mixed structural position). The separation of duties documentation system is an institutional actor with constrained exit options — maintains the performative ritual but recognizes its own degradation (piton perspective). The zero-trust architecture coalition is organized agents with mobile exit options — building alternative technical pathways that eliminate the residual (scaffold perspective with sunset logic).
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLVED: This constraint is a genuine tangled rope, not a snare masquerading as coordination or a rope masquerading as extraction. The coordination function is real and measurable: emergency access procedures do enable legitimate incident response, and organizations without bypass authority face operational brittleness when technical controls fail. The extraction is also real and measurable: concentrated privilege with diffuse accountability creates moral hazard, and the documentation theater allows policy circumvention and unauditable actions to persist behind a compliance facade. The mandatrophy is resolved by recognizing that BOTH functions coexist in the same structural arrangement. The governance residual is not 'really' coordination (the scaffold and rope perspectives are incomplete) and not 'really' extraction (the snare perspective is incomplete). It is a hybrid where genuine operational necessity (emergency access) and extractive moral hazard (unauditable bypass) are structurally inseparable given current technical and regulatory constraints. Zero-trust architectures represent a pathway to decompose the hybrid — cryptographic attestation can preserve emergency access (coordination) while eliminating unauditable bypass (extraction) — which confirms that the tangled rope classification is correct: the constraint exhibits both functions, and they can in principle be separated through architectural innovation.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    emergency_access_necessity,
    'What proportion of administrator bypass events represent genuine operational emergencies versus convenience or policy circumvention?',
    'Longitudinal analysis of documented bypass justifications correlated with incident severity, time-to-resolution, and post-incident review outcomes. Comparison with zero-trust pilot deployments where bypass is architecturally prevented.',
    'If >80% genuine emergencies: governance residual is primarily coordination (Rope from more perspectives). If <40% genuine emergencies: governance residual is primarily extraction (Snare from more perspectives).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(emergency_access_necessity, empirical, 'Proportion of bypass events representing genuine operational necessity').

omega_variable(
    zero_trust_adoption_timeline,
    'Will zero-trust architectures achieve sufficient maturity and adoption to eliminate the governance residual within the scaffold sunset window, or will legacy system dependencies and migration costs preserve the residual indefinitely?',
    'Tracking adoption rates in regulated industries; cost-benefit analysis of migration vs. continued documentation theater; regulatory acceptance of cryptographic attestation as substitute for human oversight.',
    'If adopted within 15-25 years: scaffold perspective confirmed, governance residual has genuine sunset. If adoption stalls: scaffold perspective is aspirational, residual becomes permanent piton.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(zero_trust_adoption_timeline, empirical, 'Whether zero-trust adoption will eliminate governance residual within projected timeline').

omega_variable(
    regulatory_documentation_sufficiency,
    'Do regulators accept separation of duties documentation as evidence of compliance because they believe it constrains behavior, or because they lack resources to verify actual technical enforcement?',
    'Analysis of regulatory examination procedures; interviews with examiners; comparison of enforcement actions in organizations with strong vs. weak technical controls but equivalent documentation.',
    'If regulators verify enforcement: documentation theater is transitional (piton degrading toward obsolescence). If regulators accept documentation uncritically: theater is structural feature of regulatory equilibrium (piton is stable).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_documentation_sufficiency, conceptual, 'Whether regulatory acceptance of documentation reflects belief in efficacy or resource constraints').

omega_variable(
    administrator_identity_lock,
    'Are credentialed administrators identity-locked into their privileged role through professional identity fusion, or are they structurally mobile with arbitrage exit options?',
    'Career path analysis: do administrators move between organizations when governance constraints tighten, or do they remain despite increased oversight? Salary premium analysis for roles with bypass authority vs. equivalent technical roles without privilege.',
    'If identity-locked: administrators experience higher effective extraction than beneficiary status suggests (professional identity depends on privileged access). If mobile with arbitrage: administrators are genuine beneficiaries with low experienced extraction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(administrator_identity_lock, empirical, 'Whether administrators are identity-locked or structurally mobile').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(human_governance_residual, 0, 8).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hgr_tr_t0, human_governance_residual, theater_ratio, 0, 0.52).
narrative_ontology:measurement(hgr_tr_t4, human_governance_residual, theater_ratio, 4, 0.61).
narrative_ontology:measurement(hgr_tr_t8, human_governance_residual, theater_ratio, 8, 0.68).

% Extraction over time
narrative_ontology:measurement(hgr_be_t0, human_governance_residual, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(hgr_be_t4, human_governance_residual, base_extractiveness, 4, 0.53).
narrative_ontology:measurement(hgr_be_t8, human_governance_residual, base_extractiveness, 8, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(human_governance_residual, enforcement_mechanism).

% DUAL FORMULATION NOTE:
% The human governance residual is downstream of privilege_waiver_threshold (mountain — the legal/logical limits on when privilege can be waived) and trust_boundary_architecture (rope — the technical coordination mechanisms that define system boundaries). The upstream constraints have their own extractiveness values reflecting their structural properties; the governance residual has its own extractiveness reflecting the career asymmetry and accountability diffusion that arise when technical controls reduce but cannot eliminate human discretion to bypass architectural safeguards.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
