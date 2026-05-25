% ============================================================================
% CONSTRAINT STORY: sovereignty_rto_tradeoff
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2025-01-02
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sovereignty_rto_tradeoff, []).

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
    constraint_indexing:directionality_override/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: sovereignty_rto_tradeoff
 *   human_readable: Sovereignty-RTO Tradeoff in Critical Infrastructure
 *   domain: infrastructure_security/disaster_recovery/digital_sovereignty
 *
 * SUMMARY:
 *   The sovereignty-RTO tradeoff emerges when organizations subject to data
 *   sovereignty mandates must choose between absolute infrastructure control
 *   (zero cloud dependency) and aggressive recovery time objectives. Cloud
 *   providers offer sub-second automated failover through globally
 *   distributed infrastructure and mature orchestration tooling. Sovereign
 *   infrastructure — on-premises or domestic-only hosting — typically
 *   requires manual intervention during disaster recovery, extending RTO from
 *   seconds to minutes or hours. This constraint exhibits tangled rope
 *   structure: genuine coordination function (geopolitical risk mitigation,
 *   regulatory compliance, data sovereignty) coexists with extraction
 *   mechanism (regulatory capture by domestic vendors, artificial complexity
 *   maintaining manual processes, service degradation for end users). The
 *   tradeoff is not inherent to sovereignty — open-source cloud-native
 *   tooling is closing the RTO gap — but current institutional arrangements
 *   and vendor incentives maintain the delta. Theater ratio (0.38) reflects
 *   that some sovereignty compliance is performative (checkbox audits, vendor
 *   certifications) while core technical constraints (data residency, access
 *   control) are structural. Extractiveness has increased over the interval
 *   (0.35 → 0.48) as sovereignty mandates have expanded faster than
 *   open-source disaster recovery automation has matured, widening the RTO
 *   gap.
 *
 * KEY AGENTS:
 *   - Sovereignty Mandate Organizations: Primary beneficiary (institutional/arbitrage) — regulatory agencies, national security bodies that define and enforce sovereignty requirements; capture procurement decisions
 *   - Domestic Infrastructure Vendors: Secondary beneficiary (institutional/mobile) — benefit from regulatory moat against foreign cloud providers; reduced competitive pressure on RTO performance
 *   - Service Availability Requirements: Primary victim (powerless/trapped) — abstract collective good representing user expectation of high availability; cannot exit or organize; bears full cost of extended outages
 *   - Operational Staff: Secondary victim (moderate/constrained) — face cognitive load of manual recovery protocols and career risk of failure; benefit from job security and expertise value
 *   - End Users During Outages: Tertiary victim (powerless/trapped) — experience service degradation during recovery windows; no alternative during sovereignty-mandated lock-in
 *   - Open Source Infrastructure Movement: Organized agents (organized/mobile) — building cloud-native sovereignty-compatible tooling with sunset logic; Kubernetes HA, Ceph, OpenStack
 *   - Regional Sovereignty Coalition: Organized institutional actors (organized/mobile) — EU digital sovereignty initiatives, multi-nation partnerships; see coordination function but recognize extraction
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sovereignty_rto_tradeoff, 0.48).
domain_priors:suppression_score(sovereignty_rto_tradeoff, 0.52).
domain_priors:theater_ratio(sovereignty_rto_tradeoff, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sovereignty_rto_tradeoff, extractiveness, 0.48).
narrative_ontology:constraint_metric(sovereignty_rto_tradeoff, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(sovereignty_rto_tradeoff, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(sovereignty_rto_tradeoff, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(sovereignty_rto_tradeoff, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sovereignty_rto_tradeoff, tangled_rope).
narrative_ontology:human_readable(sovereignty_rto_tradeoff, "Sovereignty-RTO Tradeoff in Critical Infrastructure").
narrative_ontology:topic_domain(sovereignty_rto_tradeoff, "infrastructure_security/disaster_recovery/digital_sovereignty").

domain_priors:requires_active_enforcement(sovereignty_rto_tradeoff).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sovereignty_rto_tradeoff, sovereignty_mandate_organizations).
narrative_ontology:constraint_beneficiary(sovereignty_rto_tradeoff, domestic_infrastructure_vendors).
narrative_ontology:constraint_beneficiary(sovereignty_rto_tradeoff, compliance_audit_teams).
narrative_ontology:constraint_victim(sovereignty_rto_tradeoff, service_availability_requirements).
narrative_ontology:constraint_victim(sovereignty_rto_tradeoff, operational_staff).
narrative_ontology:constraint_victim(sovereignty_rto_tradeoff, end_users_during_outages).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: END USERS DURING OUTAGES (SNARE) — Trapped by service dependency with no alternative during recovery windows. Bear full cost of extended downtime (minutes to hours vs sub-second cloud failover). Cannot exit to alternative providers during sovereignty-mandated infrastructure lock-in. Maximum experienced extraction.
constraint_indexing:constraint_classification(sovereignty_rto_tradeoff, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: OPERATIONAL STAFF (TANGLED ROPE) — Constrained by manual intervention requirements and career risk of failure during recovery. Benefit from job security and specialized expertise value, but bear cognitive load of maintaining complex manual protocols. Genuine coordination function (disaster recovery capability) exists alongside extraction (unnecessary complexity from rejecting automated cloud tooling).
constraint_indexing:constraint_classification(sovereignty_rto_tradeoff, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: SOVEREIGNTY MANDATE ORGANIZATIONS (ROPE) — Primary beneficiaries. Experience constraint as coordination: protecting critical infrastructure from foreign dependency. Can arbitrage between compliance frameworks and international partnerships. Net beneficiary — extraction runs toward this agent through regulatory capture of infrastructure procurement.
constraint_indexing:constraint_classification(sovereignty_rto_tradeoff, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: REGIONAL SOVEREIGNTY COALITION (TANGLED ROPE) — Organized actors (EU digital sovereignty initiatives, multi-nation infrastructure partnerships) see genuine coordination function (reducing geopolitical dependency) but also recognize extraction mechanism (vendor lock-in to domestic providers, reduced innovation). Mobile across regulatory frameworks but constrained by political commitments.
constraint_indexing:constraint_classification(sovereignty_rto_tradeoff, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(continental))).

% PERSPECTIVE 5: OPEN SOURCE INFRASTRUCTURE MOVEMENT (SCAFFOLD) — Sees the tradeoff as temporary coordination problem with sunset logic. Open-source cloud-native tools (Kubernetes, OpenStack, Ceph) enable sovereignty without sacrificing automation. As these mature, the false dichotomy between control and RTO dissolves. Estimated sunset: 5-10 years for open-source disaster recovery automation to match proprietary cloud capabilities.
constraint_indexing:constraint_classification(sovereignty_rto_tradeoff, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (TANGLED ROPE) — Recognizes both genuine coordination function (geopolitical risk mitigation, data sovereignty) and extraction mechanism (regulatory capture by domestic vendors, artificial complexity maintaining manual intervention requirements). The tradeoff is not inherent — it reflects current tooling maturity and institutional path dependencies, not fundamental technical limits.
constraint_indexing:constraint_classification(sovereignty_rto_tradeoff, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sovereignty_rto_tradeoff_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(sovereignty_rto_tradeoff, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(sovereignty_rto_tradeoff, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(sovereignty_rto_tradeoff, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(sovereignty_rto_tradeoff_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.48): Moderate-high. The RTO delta imposes real costs on service availability and operational complexity. Sovereignty mandates create regulatory moat for domestic vendors, reducing competitive pressure on disaster recovery performance. However, extraction is not maximal — genuine coordination function exists (data sovereignty, geopolitical risk mitigation), and open-source alternatives are emerging. The value reflects that roughly half the RTO cost is extractive overhead (vendor capture, institutional resistance to automation) and half is legitimate coordination cost (current technical maturity gap). Suppression (0.52): Moderate. Significant barriers to alternative approaches include regulatory lock-in (cannot use foreign cloud), procurement constraints (domestic vendor preferences), and institutional path dependencies (manual protocols embedded in compliance frameworks). But suppression is not total — open-source tooling provides exit path, and some organizations achieve low RTO with sovereign infrastructure through heavy investment. Theater ratio (0.38): Moderate-low. Some sovereignty compliance is performative (vendor certifications, checkbox audits, data residency claims without technical enforcement), but core constraints are structural (actual data location, access control, encryption key management). Theater has increased as sovereignty mandates have expanded into domains where technical enforcement is weaker.
 *
 * PERSPECTIVAL GAP:
 *   The sovereignty mandate organizations experience this constraint as coordination (Rope) — they are solving the legitimate problem of geopolitical dependency and data sovereignty. The open-source infrastructure movement sees a temporary problem with a sunset (Scaffold) — cloud-native tooling is closing the RTO gap, dissolving the false dichotomy between control and automation. Operational staff see mixed coordination and extraction (Tangled Rope) — genuine disaster recovery capability exists, but manual intervention requirements impose unnecessary complexity that benefits vendor lock-in. End users see pure extraction (Snare) — extended outages with no alternative during sovereignty-mandated infrastructure lock-in. The analytical observer recognizes both functions (Tangled Rope) — genuine coordination (geopolitical risk mitigation) coexists with extraction (regulatory capture, artificial complexity). The gap reveals that 'sovereignty requires RTO sacrifice' is a contingent claim, not a natural law — the constraint's structure depends on current tooling maturity and institutional arrangements, both of which are changing.
 *
 * DIRECTIONALITY LOGIC:
 *   Sovereignty mandate organizations are primary beneficiaries — they define requirements that create regulatory moat for domestic vendors and expand their institutional authority. Declared as beneficiaries with institutional power and arbitrage exit options, yielding low d and low/negative effective extraction. Domestic infrastructure vendors are secondary beneficiaries — they capture market share through regulatory protection. Service availability requirements are the primary victim — an abstract collective good (user expectation of high availability) that cannot exit or organize, bearing full cost of RTO degradation. Declared as victim with powerless/trapped context, yielding high d and high effective extraction. Operational staff are secondary victims — they bear cognitive load and career risk but also benefit from job security and expertise value. Declared as victims with moderate power and constrained exit, yielding moderate d and moderate extraction. End users during outages are tertiary victims — trapped by service dependency with no alternative during recovery windows. The perspectival gap emerges from these structural positions: sovereignty mandate organizations see coordination (protecting critical infrastructure), operational staff see mixed coordination and extraction (genuine disaster recovery capability with unnecessary complexity), end users see pure extraction (service degradation with no benefit to them).
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by demonstrating that the sovereignty-RTO tradeoff is neither pure coordination (Rope) nor pure extraction (Snare), but a hybrid (Tangled Rope) where both functions coexist. The coordination function is genuine: data sovereignty and geopolitical risk mitigation are real security concerns, and infrastructure control provides real protection against foreign government leverage. The extraction mechanism is also genuine: sovereignty mandates create regulatory moat for domestic vendors, reducing competitive pressure on RTO performance; manual intervention requirements maintain operational complexity that benefits incumbent expertise; compliance theater (checkbox audits, vendor certifications) substitutes for technical enforcement. The mandatrophy question 'Is this sovereignty or extraction?' has the answer 'Both, and the ratio depends on the observer's structural position.' The open-source movement's scaffold perspective reveals that the tradeoff is not inherent — technical solutions exist that preserve sovereignty without sacrificing RTO — but current institutional arrangements maintain the extraction mechanism. The analytical classification as Tangled Rope reflects this structural ambiguity: the constraint cannot be cleanly separated into coordination and extraction components because they are institutionally entangled.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    rto_floor_ambiguity,
    'What is the minimum achievable RTO for fully sovereign infrastructure with current open-source tooling?',
    'Empirical testing of open-source disaster recovery stacks (Kubernetes HA, Ceph replication, OpenStack multi-region) under realistic failure scenarios; comparison with proprietary cloud RTO benchmarks',
    'If open-source RTO approaches cloud RTO (sub-10-second): tradeoff is artificial, extraction mechanism confirmed. If gap remains order-of-magnitude (minutes vs seconds): some coordination cost is genuine technical debt.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(rto_floor_ambiguity, empirical, 'Minimum RTO achievable with sovereign open-source infrastructure').

omega_variable(
    sovereignty_threat_model_validity,
    'Does foreign cloud dependency constitute a genuine national security risk proportional to the RTO cost imposed?',
    'Historical analysis of cloud provider compliance with foreign government data requests; game-theoretic modeling of geopolitical leverage scenarios; comparison with other dependency vectors (hardware supply chains, undersea cables)',
    'If threat model is valid: coordination function is genuine, higher RTO is justified cost. If threat model is exaggerated: sovereignty mandate is regulatory capture, RTO cost is pure extraction.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sovereignty_threat_model_validity, preference, 'Validity of sovereignty threat model justifying RTO tradeoff').

omega_variable(
    manual_intervention_necessity,
    'Are manual intervention steps in recovery protocols technically necessary for sovereignty, or artifacts of institutional resistance to automation?',
    'Protocol decomposition: identify which manual steps verify sovereignty constraints vs which are legacy procedures. Test automated alternatives that preserve sovereignty properties.',
    'If manual steps are necessary: suppression is structural. If manual steps are vestigial: suppression is institutional theater, piton signature emerges.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(manual_intervention_necessity, empirical, 'Technical necessity of manual intervention for sovereignty').

omega_variable(
    vendor_capture_extent,
    'To what extent do sovereignty mandates function as regulatory capture for domestic infrastructure vendors vs genuine security policy?',
    'Analysis of procurement patterns: correlation between sovereignty requirements and domestic vendor market share; comparison of technical specifications with vendor capabilities; lobbying expenditure tracking',
    'If high capture: beneficiary extraction is primary function, coordination is cover story. If low capture: coordination is genuine, extraction is side effect.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(vendor_capture_extent, empirical, 'Extent of domestic vendor regulatory capture').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sovereignty_rto_tradeoff, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sov_rto_theater_initial, sovereignty_rto_tradeoff, theater_ratio, 0, 0.25).
narrative_ontology:measurement(sov_rto_theater_mid, sovereignty_rto_tradeoff, theater_ratio, 3, 0.32).
narrative_ontology:measurement(sov_rto_theater_final, sovereignty_rto_tradeoff, theater_ratio, 6, 0.38).

% Extraction over time
narrative_ontology:measurement(sov_rto_extract_initial, sovereignty_rto_tradeoff, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(sov_rto_extract_mid, sovereignty_rto_tradeoff, base_extractiveness, 3, 0.42).
narrative_ontology:measurement(sov_rto_extract_final, sovereignty_rto_tradeoff, base_extractiveness, 6, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sovereignty_rto_tradeoff, enforcement_mechanism).

% DUAL FORMULATION NOTE:
% This constraint is downstream of physical_airgap_authenticity (mountain — physical isolation as security primitive) and operational_discipline_dependency (snare — manual protocol complexity). The sovereignty-RTO tradeoff inherits structural properties from both: the mountain upstream constraint establishes that some isolation is technically necessary (genuine coordination floor), while the snare upstream constraint reveals that manual intervention requirements exceed technical necessity (extraction mechanism). The three constraints form a family where the RTO tradeoff is the observable manifestation of deeper structural tensions.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(sovereignty_rto_tradeoff, institutional, 0.12).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
