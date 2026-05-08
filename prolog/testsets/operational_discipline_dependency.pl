% ============================================================================
% CONSTRAINT STORY: operational_discipline_dependency
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2025-01-10
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_operational_discipline_dependency, []).

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
 *   constraint_id: operational_discipline_dependency
 *   human_readable: Operational Discipline Dependency in Infrastructure Security
 *   domain: infrastructure_security/disaster_recovery/digital_sovereignty
 *
 * SUMMARY:
 *   The operational discipline dependency in infrastructure security creates
 *   a structural trap where organizational resilience depends entirely on
 *   sustained human adherence to physical procedures — rotating backup media,
 *   unlocking vaults on schedule, executing recovery drills — rather than on
 *   automated enforcement or fail-safe design. This constraint exhibits
 *   multiple DR types from different perspectives, revealing how the same
 *   dependency structure appears as legitimate coordination (to
 *   administrators and vendors), extractive theater (to compliance frameworks
 *   and operational staff), or a temporary problem being solved (to
 *   automation advocates). The constraint's theater ratio (0.65) reflects
 *   that compliance with rotation procedures has become substantially
 *   decoupled from actual recovery capability: organizations achieve perfect
 *   audit scores while recovery drills fail. The extractiveness (0.68)
 *   captures the asymmetric accountability — operational staff bear career
 *   risk for procedure failures they cannot fully control, while
 *   administrators capture job security and knowledge monopoly rents. Both
 *   metrics have increased over the interval as system complexity has
 *   outpaced human capacity for perfect adherence.
 *
 * KEY AGENTS:
 *   - Operational Staff: Primary victim (powerless/trapped) — bears accountability for perfect adherence to procedures; has no authority to redesign the system; cannot exit the responsibility
 *   - Organizational Security Posture: Primary victim (powerless/trapped) — abstract collective good with no advocate; bears full cost of procedure failures (data loss, recovery failure, sovereignty compromise)
 *   - Infrastructure Administrators: Primary beneficiary (institutional/arbitrage) — captures job security, knowledge monopoly, and control over critical procedures; can exit to other organizations
 *   - Security Vendors: Secondary beneficiary (institutional/arbitrage) — sells vault systems, rotation schedules, compliance software; the dependency on human discipline generates recurring revenue
 *   - Security Auditors: Mixed position (moderate/constrained) — benefits from verification ecosystem but bears cost of theater (checking boxes on procedures that may not reflect actual security state)
 *   - DevOps Automation Coalition: Organized agents (organized/mobile) — building alternative pathways (infrastructure-as-code, automated failover) with sunset logic
 *   - Compliance Frameworks: Institutional actor (institutional/constrained) — maintains performative requirements; sees own process as degraded (piton perspective)
 *   - Analytical Observer: Universal view (analytical/analytical) — sees both genuine coordination function and asymmetric extraction
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(operational_discipline_dependency, 0.68).
domain_priors:suppression_score(operational_discipline_dependency, 0.72).
domain_priors:theater_ratio(operational_discipline_dependency, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(operational_discipline_dependency, extractiveness, 0.68).
narrative_ontology:constraint_metric(operational_discipline_dependency, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(operational_discipline_dependency, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(operational_discipline_dependency, snare).
narrative_ontology:human_readable(operational_discipline_dependency, "Operational Discipline Dependency in Infrastructure Security").
narrative_ontology:topic_domain(operational_discipline_dependency, "infrastructure_security/disaster_recovery/digital_sovereignty").

domain_priors:requires_active_enforcement(operational_discipline_dependency).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(operational_discipline_dependency, infrastructure_administrators).
narrative_ontology:constraint_beneficiary(operational_discipline_dependency, security_vendors).
narrative_ontology:constraint_victim(operational_discipline_dependency, organizational_security_posture).
narrative_ontology:constraint_victim(operational_discipline_dependency, operational_staff).
narrative_ontology:constraint_victim(operational_discipline_dependency, recovery_capability).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: OPERATIONAL STAFF (SNARE) — Trapped in a system where security depends on perfect human adherence to physical procedures (rotating backup media, unlocking vaults on schedule, executing recovery drills). Cannot exit the responsibility; bears career risk if procedures fail; has no authority to automate or redesign the system. Experiences maximum extraction — all accountability, no agency.
constraint_indexing:constraint_classification(operational_discipline_dependency, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: ORGANIZATIONAL SECURITY POSTURE (SNARE) — The abstract collective good of organizational resilience is trapped in dependency on sustained human discipline. Cannot exit the vulnerability; has no advocate; bears full cost of procedure failures (data loss, recovery failure, sovereignty compromise). Maximum extraction from an entity with no agency.
constraint_indexing:constraint_classification(operational_discipline_dependency, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 3: SECURITY AUDITOR (TANGLED ROPE) — Constrained by compliance frameworks that mandate physical rotation and vault procedures. Benefits from the verification ecosystem (audit trails, compliance reports) but also bears cost of theater — checking boxes on procedures that may not reflect actual security state. Mixed coordination and extraction.
constraint_indexing:constraint_classification(operational_discipline_dependency, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 4: INFRASTRUCTURE ADMINISTRATOR (ROPE) — Benefits from the constraint through job security, specialized knowledge monopoly, and control over critical procedures. Experiences the system as coordination — the procedures exist to solve the legitimate problem of disaster recovery. Can exit to other organizations; captures career value from being the keeper of the vault keys.
constraint_indexing:constraint_classification(operational_discipline_dependency, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: SECURITY VENDOR (ROPE) — Benefits from selling physical vault systems, rotation schedules, compliance software, and audit services. The constraint creates a market for products that enforce human discipline. Experiences low extraction — the dependency on operational discipline is a feature, not a bug, because it generates recurring revenue from compliance tooling.
constraint_indexing:constraint_classification(operational_discipline_dependency, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: DEVOPS AUTOMATION COALITION (SCAFFOLD) — Organized agents (infrastructure-as-code, immutable infrastructure, automated failover) see the operational discipline dependency as a temporary problem with a sunset. Automated backup rotation, cryptographic key management systems, and continuous recovery testing are building alternative pathways that remove human discipline as a single point of failure. Estimated sunset: 5-10 years for mature automation to replace manual procedures in critical infrastructure.
constraint_indexing:constraint_classification(operational_discipline_dependency, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 7: COMPLIANCE FRAMEWORK (PITON) — Regulatory requirements for physical media rotation and vault access persist through institutional inertia despite evidence that manual procedures introduce more risk than they mitigate. The compliance ritual is largely performative — organizations check boxes on rotation schedules while actual recovery capability degrades. Theater ratio is high because the procedures measure adherence to process, not actual recovery effectiveness.
constraint_indexing:constraint_classification(operational_discipline_dependency, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER (TANGLED ROPE) — From a universal perspective, some dependency on human discipline is inherent to any system (automation has failure modes too), but the current configuration extracts asymmetrically. The constraint coordinates disaster recovery (genuine function) while simultaneously extracting from operational staff (accountability without authority) and organizational security (dependency on sustained perfection). The analytical classification is tangled_rope because both coordination and extraction are structurally real.
constraint_indexing:constraint_classification(operational_discipline_dependency, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(operational_discipline_dependency_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(operational_discipline_dependency, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(operational_discipline_dependency, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(operational_discipline_dependency, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(operational_discipline_dependency, TR),
    TR >= 0.70.

:- end_tests(operational_discipline_dependency_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. Infrastructure administrators capture career value and knowledge monopoly rents while operational staff bear asymmetric accountability (responsibility without authority). Security vendors benefit from recurring compliance revenue. The organizational security posture bears the cost of dependency on sustained human perfection. The extraction is not total (some genuine coordination exists — disaster recovery is a real problem), but it is severe and asymmetric. Suppression (0.72): High. Operational staff cannot exit the responsibility; compliance frameworks mandate the procedures; knowledge monopolies create lock-in; career risk of procedure failure is severe; automation alternatives face regulatory barriers. Suppression is not total (the DevOps coalition is building exits), but it is substantial. Theater ratio (0.65): High. Compliance with rotation procedures has become substantially decoupled from actual recovery capability. Organizations achieve perfect audit scores (all boxes checked, all schedules documented) while recovery drills fail (media is rotated but corrupted, vaults are unlocked but keys are lost, procedures are documented but not executable). The theater has increased over the interval as system complexity has outpaced the procedures designed for simpler infrastructure.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates how the same dependency structure produces radically different experiences based on structural position. Infrastructure administrators see coordination (Rope) — the procedures solve the legitimate problem of disaster recovery, and they are the experts who maintain the system. Security vendors see coordination (Rope) — the market for compliance tooling is a natural response to regulatory requirements. Operational staff see pure extraction (Snare) — they bear all accountability with no authority to redesign the system. The organizational security posture sees pure extraction (Snare) — it is trapped in dependency on sustained human perfection. Security auditors see mixed coordination and extraction (Tangled Rope) — the verification ecosystem has value, but much of it is theater. The DevOps automation coalition sees a temporary problem with a sunset (Scaffold) — automation is building alternative pathways. The compliance framework sees its own degraded ritual (Piton) — the procedures persist through inertia despite evidence of ineffectiveness. The analytical observer sees both genuine coordination and asymmetric extraction (Tangled Rope) — disaster recovery is a real problem, but the current solution extracts from operational staff and organizational security while benefiting administrators and vendors.
 *
 * DIRECTIONALITY LOGIC:
 *   Infrastructure administrators are beneficiaries with arbitrage exit options — they benefit from job security and knowledge monopoly, and they can move to other organizations. This produces low d and low or negative chi (they experience the constraint as coordination). Security vendors are also beneficiaries with arbitrage — the dependency on human discipline is a feature that generates recurring revenue. Operational staff are victims with trapped exit — they bear accountability without authority and cannot exit the responsibility. This produces high d and high chi (they experience maximum extraction). The organizational security posture is a victim with trapped exit — an abstract collective good with no advocate. Security auditors are in a mixed position — they benefit from the verification ecosystem but are constrained by compliance frameworks that mandate theater. The DevOps automation coalition has mobile exit options and sees a sunset path. The compliance framework is constrained (institutional inertia) but not trapped. The analytical observer uses the analytical exit option and sees the full structure.
 *
 * MANDATROPHY ANALYSIS:
 *   PENDING RESOLUTION: This constraint has extractiveness > 0.70 from the perspective of operational staff (powerless/trapped), requiring mandatrophy resolution. The key question: Is the operational discipline dependency a necessary coordination cost (disaster recovery genuinely requires human judgment and physical procedures) or extractive theater (automation could provide better security with lower human dependency)? The omega variables identify the empirical tests: comparative failure mode analysis (automation vs manual), correlation between compliance and recovery effectiveness, and knowledge monopoly impact on outcomes. If automation failure modes are more severe than manual procedure failure modes, the dependency is legitimate coordination. If compliance is decoupled from recovery capability and knowledge monopolies correlate with failed recoveries, the dependency is extractive theater. The mandatrophy will be resolved when sufficient incident data exists to answer these questions. Current hypothesis: the constraint is a degrading Tangled Rope — it had genuine coordination function when infrastructure was simpler, but increasing system complexity has shifted it toward extraction as human capacity for perfect adherence has been exceeded.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    automation_failure_mode_severity,
    'Are the failure modes of automated backup systems more or less severe than the failure modes of human-dependent manual procedures?',
    'Comparative incident analysis: recovery failures in automated vs manual systems; measurement of mean time to recovery; analysis of failure attribution (human error vs automation bug vs external attack)',
    'If automation failure modes are more severe: the operational discipline dependency is a legitimate risk mitigation (Rope from more perspectives). If manual procedure failure modes are more severe: the dependency is extractive theater (Snare from more perspectives).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(automation_failure_mode_severity, empirical, 'Comparative severity of automation vs manual procedure failure modes').

omega_variable(
    compliance_theater_threshold,
    'At what point does compliance with rotation procedures become decoupled from actual recovery capability?',
    'Correlation analysis between audit compliance scores and actual recovery drill success rates; identification of organizations with perfect compliance but failed recoveries',
    'If correlation is strong: compliance frameworks are measuring real security (lower theater ratio). If correlation is weak: compliance is performative (higher theater ratio, stronger piton classification).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(compliance_theater_threshold, empirical, 'Correlation between compliance adherence and recovery effectiveness').

omega_variable(
    knowledge_monopoly_extraction,
    'Does the specialized knowledge required for vault procedures create extractive rent-seeking by infrastructure administrators, or is it a legitimate coordination cost?',
    'Analysis of administrator turnover impact on recovery capability; measurement of knowledge transfer effectiveness; comparison of recovery success rates in organizations with vs without knowledge monopolies',
    'If knowledge monopoly correlates with better recovery outcomes: the administrator''s position is coordination (Rope). If knowledge monopoly correlates with vendor lock-in and failed recoveries: the position is extractive (Snare from administrator perspective becomes questionable).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(knowledge_monopoly_extraction, empirical, 'Whether administrator knowledge monopoly is coordination or extraction').

omega_variable(
    digital_sovereignty_tradeoff,
    'Does reliance on manual procedures enhance digital sovereignty (independence from cloud vendors) or undermine it (dependency on irreplaceable human knowledge)?',
    'Case studies of sovereignty failures: organizations that lost recovery capability due to administrator departure vs organizations that lost sovereignty due to cloud vendor dependency; comparative analysis of recovery capability persistence over time',
    'If manual procedures enhance sovereignty: the constraint has a genuine coordination function (Tangled Rope or Rope from sovereignty perspective). If manual procedures create human single points of failure: the sovereignty claim is theater masking a different dependency (Snare).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(digital_sovereignty_tradeoff, conceptual, 'Whether manual procedures enhance or undermine digital sovereignty').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(operational_discipline_dependency, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(opdis_tr_t0, operational_discipline_dependency, theater_ratio, 0, 0.4).
narrative_ontology:measurement(opdis_tr_t3, operational_discipline_dependency, theater_ratio, 3, 0.5).
narrative_ontology:measurement(opdis_tr_t6, operational_discipline_dependency, theater_ratio, 6, 0.58).
narrative_ontology:measurement(opdis_tr_t10, operational_discipline_dependency, theater_ratio, 10, 0.65).

% Extraction over time
narrative_ontology:measurement(opdis_be_t0, operational_discipline_dependency, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(opdis_be_t3, operational_discipline_dependency, base_extractiveness, 3, 0.52).
narrative_ontology:measurement(opdis_be_t6, operational_discipline_dependency, base_extractiveness, 6, 0.6).
narrative_ontology:measurement(opdis_be_t10, operational_discipline_dependency, base_extractiveness, 10, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(operational_discipline_dependency, enforcement_mechanism).
narrative_ontology:affects_constraint(operational_discipline_dependency, cloud_vendor_dependency).
narrative_ontology:affects_constraint(operational_discipline_dependency, knowledge_transfer_failure).
narrative_ontology:affects_constraint(operational_discipline_dependency, compliance_framework_ossification).

% DUAL FORMULATION NOTE:
% The operational discipline dependency is upstream of specific failure modes (cloud vendor lock-in, knowledge transfer failure, compliance ossification) but represents a distinct structural constraint. The downstream constraints have their own extractiveness values reflecting the specific mechanisms; the operational discipline dependency has its own extractiveness reflecting the asymmetric accountability and knowledge monopoly structure.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
