% ============================================================================
% CONSTRAINT STORY: paradoxical_decompositions
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_paradoxical_decompositions, []).

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
 *   constraint_id: paradoxical_decompositions
 *   human_readable: Data Replication Paradox
 *   domain: technological/cybersecurity/distributed_systems
 *
 * SUMMARY:
 *   The Data Replication Paradox represents a structural tension in
 *   distributed systems design: the mechanisms that improve availability
 *   (data replication across zones and vendors) create exponential growth in
 *   the attack surface and credential exposure pathways. This constraint
 *   exhibits six distinct classifications depending on the observer's
 *   structural position and time horizon. The availability operations group
 *   sees replication as pure coordination solving a legitimate collective
 *   action problem (regional outages, latency). The security operations team
 *   sees mixed coordination and extraction — they benefit from reduced
 *   downtime but absorb monitoring and key management overhead. The abstract
 *   data security perimeter bears full extraction cost without agency: each
 *   additional replica demands protection, yet protection mechanisms grow
 *   beyond linear complexity. The legacy backup ritual persists as a
 *   performative piton — continuous replication has functionally replaced
 *   tape-based recovery, yet scheduled snapshots and air-gapped archives
 *   remain mandatory through compliance inertia. The zero-trust architecture
 *   coalition sees a temporary problem with a structural sunset:
 *   cryptographic identity-bound replication and per-request verification can
 *   decouple availability gains from credential exposure risk. The
 *   civilizational analytical observer risks framing the
 *   availability-security trade-off as an immutable law of information
 *   systems, naturalizing what may be a contingent artifact of symmetric key
 *   distribution and centralized credential management.
 *
 * KEY AGENTS:
 *   - System Availability Operators: Primary beneficiary (institutional/arbitrage) — capture SLA compliance, reduce downtime, improve customer experience through geo-distribution
 *   - Data Security Perimeter: Primary victim (powerless/trapped) — abstract collective good that cannot exit; bears exponential attack surface growth without agency
 *   - Security Operations Team: Secondary victim (moderate/constrained) — face resource and compliance constraints; benefit from availability but absorb monitoring and key rotation overhead
 *   - Credential Distribution Infrastructure: Structural victim (powerless/trapped) — each replica demands credential copy; exposure risk grows with replica count
 *   - Zero-Trust Architecture Coalition: Organized agents (organized/constrained) — cryptographic identity, distributed key management, per-request verification as sunset pathway
 *   - Legacy Backup Systems: Institutional actor (institutional/arbitrage) — maintains performative tape and snapshot rituals; sees own mechanisms as degraded but persists through regulatory requirement
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(paradoxical_decompositions, 0.52).
domain_priors:suppression_score(paradoxical_decompositions, 0.65).
domain_priors:theater_ratio(paradoxical_decompositions, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(paradoxical_decompositions, extractiveness, 0.52).
narrative_ontology:constraint_metric(paradoxical_decompositions, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(paradoxical_decompositions, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(paradoxical_decompositions, tangled_rope).
narrative_ontology:human_readable(paradoxical_decompositions, "Data Replication Paradox").
narrative_ontology:topic_domain(paradoxical_decompositions, "technological/cybersecurity/distributed_systems").

domain_priors:requires_active_enforcement(paradoxical_decompositions).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(paradoxical_decompositions, system_availability_operators).
narrative_ontology:constraint_beneficiary(paradoxical_decompositions, fault_tolerance_architects).
narrative_ontology:constraint_victim(paradoxical_decompositions, data_security_perimeter).
narrative_ontology:constraint_victim(paradoxical_decompositions, credential_exposure_risk).
narrative_ontology:constraint_victim(paradoxical_decompositions, breach_surface_integrity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DATA SECURITY PERIMETER (SNARE) — The abstract security boundary cannot exit the replication paradox. Each additional replica demands protection, yet protection mechanisms grow exponentially in complexity. The perimeter bears full extraction cost without agency: more replicas mean more attack surfaces, more credential distribution points, more backup authentication tokens scattered across infrastructure. No negotiation, no opt-out. Pure constraint experience.
constraint_indexing:constraint_classification(paradoxical_decompositions, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: SECURITY OPERATIONS TEAM (TANGLED ROPE) — Constrained by budget limits, staffing caps, and regulatory compliance obligations. Benefits from replication (reduced downtime incidents, improved customer SLA compliance) but also bears extraction (monitoring overhead, key rotation complexity, incident response scope expansion). Mixed experience: genuine coordination function (availability is real) coupled with asymmetric cost distribution (security team absorbs the overhead).
constraint_indexing:constraint_classification(paradoxical_decompositions, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: AVAILABILITY OPERATIONS GROUP (ROPE) — Primary beneficiary. Sees replication as pure coordination: distributing data across zones solves legitimate collective action problems (zone failures, latency, regional disaster recovery). Experiences the constraint as coordination overhead rather than extraction. Low suppression from this view — they have agency in replica placement strategy, data tiering, and can arbitrage across vendor ecosystems.
constraint_indexing:constraint_classification(paradoxical_decompositions, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: ZERO-TRUST ARCHITECTURE COALITION (SCAFFOLD) — Organized agents (NIST cybersecurity framework, identity-based access control vendors, zero-trust architecture proponents) see the paradox as a temporary coordination failure with a structural sunset. Zero-trust design principles (cryptographic identity, per-request verification, microsegmentation) create pathways to decouple replication from credential exposure. As zero-trust adoption matures over 5-10 years, the paradox weakens: cryptographic identity replicas replace credential replicas. Sunset: adoption of hardware security modules, distributed key management, and identity-bound encryption.
constraint_indexing:constraint_classification(paradoxical_decompositions, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: LEGACY BACKUP RITUAL (PITON) — Traditional tape-based and incremental backup procedures were designed when replication was scarce and recovery was slow. Full-disk backups, scheduled snapshots, and air-gapped archives remain mandatory despite modern continuous replication. The rituals persist through compliance inertia and institutional conservatism. Backup verification is largely performative: teams confirm tapes are generated and stored, but actual recovery-from-backup drills are rare. Theater ratio high because the functional backup mechanism (continuous replication + transaction logs) has made the ritualized backup procedure redundant — yet it persists because regulations require it and no one has formally decommissioned it.
constraint_indexing:constraint_classification(paradoxical_decompositions, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational/universal perspective, the trade-off between availability and security is presented as an immutable law of information systems: you cannot maximize both simultaneously without infinite resources. The replication-security paradox is framed as inherent to distributed systems theory (CAP theorem, Byzantine fault tolerance limits). However, this perspective naturalizes what is actually a contingent artifact of current architectural assumptions: symmetric key distribution, centralized key management, and coarse-grained access control. Cryptographic identity and zero-trust designs suggest the trade-off is not immutable.
constraint_indexing:constraint_classification(paradoxical_decompositions, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(paradoxical_decompositions_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(paradoxical_decompositions, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(paradoxical_decompositions, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(paradoxical_decompositions, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(paradoxical_decompositions, TR),
    TR >= 0.70.

:- end_tests(paradoxical_decompositions_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The paradox creates asymmetric costs between availability benefits (concentrated in operational efficiency) and security burdens (distributed across all credential holders). Early in the interval (t=0), extractiveness was moderate (0.28) — simple replication across 2-3 zones was manageable. As organizations scale to 10+ replicas across continents and vendors (t=10), extractiveness has grown to 0.52 because credential distribution overhead becomes nonlinear: managing 10 copies of database passwords, API tokens, and encryption keys requires exponentially more monitoring, rotation complexity, and breach surface. Suppression (0.65): High. Security teams have limited options for opting out of replication (customer SLAs mandate it), limited ability to reduce replica count (business continuity requirements), and limited ability to simplify credential management (regulatory compliance mandates strong authentication across all access paths). Theater ratio (0.58): Moderate-high. Legacy backup procedures (tape snapshots, offline archives) are highly performative — teams verify backup generation but rarely execute actual recovery drills. Continuous replication has made tape recovery largely redundant, yet the ritual persists because SOC2 and HIPAA explicitly require offline backup verification. As zero-trust adoption increases, theater declines because cryptographic identity verification becomes the actual control mechanism rather than a supplement to backups.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates radical perspectival divergence. The availability operations group (institutional/arbitrage) sees pure coordination — replication solves legitimate zone-failure problems with minimal overhead from their vantage point. The security operations team (moderate/constrained) sees tangled rope — they benefit from availability but absorb exponential security costs. The data security perimeter (powerless/trapped) sees snare — each additional replica is pure extraction with no benefit or exit option. The zero-trust coalition (organized/constrained) sees a temporary problem with a sunset — zero-trust architecture creates structural exit paths. The legacy backup ritual (institutional/arbitrage, but nostalgia-driven) sees its own degradation — modern replication has made tape recovery functionally obsolete, yet the piton persists. The civilizational observer sees a natural law — availability-security trade-offs are supposedly immutable — but this framing naturalizes contingent credential distribution architectures. The perspectival gap reveals that 'the paradox' is not a single constraint but a manifestation of different agents experiencing the same infrastructure through different structural lenses.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (system_availability_operators) have arbitrage exit options: they can adjust replica placement strategy, tier data by criticality, and migrate across vendor ecosystems. They derive low effective extraction (d ≈ 0.15) because replication is genuinely beneficial to them. Victims (data_security_perimeter, credential_exposure_risk, breach_surface_integrity) are trapped — they cannot opt out of protecting replicas once they exist, and credential exposure grows with replica count. They derive high effective extraction (d ≈ 0.85-0.95) because they bear costs without agency. The security operations team is constrained (not arbitrage, not trapped) — they have some agency in designing security architecture but face regulatory and budgetary constraints that limit their options. They derive moderate extraction (d ≈ 0.60-0.65). The zero-trust coalition is organized with constrained exits — they can build new architectures but face adoption barriers and legacy system dependencies. They derive moderate extraction (d ≈ 0.50-0.55), but this is expected to decline as zero-trust matures.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy (coordination vs extraction confusion) by showing that BOTH readings are correct from their respective observational positions. The availability operations group genuinely experiences coordination — they are solving a real collective action problem (zone failures). The security operations team genuinely experiences extraction — they absorb overhead asymmetrically. The mandatrophy is resolved by recognizing that the constraint has a genuine coordination function (improving availability) coupled with asymmetric extraction (security burden distribution). This is the definition of tangled_rope: both components are present and both are required for classification. The false summit risk is at the civilizational level — framing the availability-security trade-off as immutable law when it is actually contingent on symmetric key distribution and centralized credential management. Zero-trust architectures demonstrate that cryptographic identity decouples the trade-off, suggesting the paradox is not a law of physics but a law of legacy architecture. The analytics observer's mountain classification fails the accessibility_collapse gate because the 'constraint' is actually a contingent institutional arrangement that newer architectures have already partially escaped.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    cryptographic_identity_maturity,
    'Will cryptographic identity-bound replication (certificate-per-replica, per-request authentication) reach sufficient maturity and adoption to decouple replication from credential exposure risk?',
    'Tracking adoption rates of hardware security modules, distributed key management systems, and identity-bound encryption in production environments; measurement of incident rates correlating replica count to breach scope in zero-trust vs legacy architectures',
    'If yes: scaffold sunset is structural reality, extractiveness drops below 0.30. If no: paradox remains permanent architectural constraint, snare classification dominates.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(cryptographic_identity_maturity, empirical, 'Whether cryptographic identity can decouple replication from credential exposure').

omega_variable(
    regulatory_backwards_compatibility,
    'Will compliance frameworks (SOC2, HIPAA, PCI-DSS) update to recognize zero-trust architectures as acceptable alternatives to air-gapped backups and scheduled tape recovery, reducing legacy ritual overhead?',
    'Monitoring regulatory guidance updates, auditor acceptance of zero-trust backup verification as compliant, de-certification of legacy tape requirements in industry standards',
    'If yes: piton classification confirmed and theater_ratio begins declining. If no: legacy backup ritual persists as performative overhead indefinitely.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_backwards_compatibility, preference, 'Whether regulators will accept zero-trust as backup alternative').

omega_variable(
    distributed_keyring_attack_surface,
    'Does distributing cryptographic keys across multiple replicas (necessary for decentralized identity) create new attack vectors that offset the reduction in credential exposure from colocated symmetric keys?',
    'Comparative attack surface analysis: counting distinct compromise paths in distributed key scenarios vs centralized key management; empirical breach data comparing incident timelines in each architecture',
    'If keys distribute safely: zero-trust pathway is real, extraction can be decoupled. If new vectors emerge: the paradox resurfaces in different form (key distribution paradox), tangled_rope or snare persists.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(distributed_keyring_attack_surface, empirical, 'Whether distributed keys create new attack vectors').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(paradoxical_decompositions, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(drp_tr_t0, paradoxical_decompositions, theater_ratio, 0, 0.42).
narrative_ontology:measurement(drp_tr_t5, paradoxical_decompositions, theater_ratio, 5, 0.5).
narrative_ontology:measurement(drp_tr_t10, paradoxical_decompositions, theater_ratio, 10, 0.58).

% Extraction over time
narrative_ontology:measurement(drp_be_t0, paradoxical_decompositions, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(drp_be_t5, paradoxical_decompositions, base_extractiveness, 5, 0.4).
narrative_ontology:measurement(drp_be_t10, paradoxical_decompositions, base_extractiveness, 10, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(paradoxical_decompositions, resource_allocation).
narrative_ontology:affects_constraint(paradoxical_decompositions, credential_rotation_overhead).
narrative_ontology:affects_constraint(paradoxical_decompositions, zone_failure_recovery_coupling).
narrative_ontology:affects_constraint(paradoxical_decompositions, cryptographic_key_management_scaling).

% DUAL FORMULATION NOTE:
% The Data Replication Paradox decomposes into three structurally distinct constraints: (1) credential_rotation_overhead — ε ≈ 0.35, the pure operational burden of managing credentials across replicas; (2) zone_failure_recovery_coupling — ε ≈ 0.15, the coordination problem of recovering from regional outages; (3) cryptographic_key_management_scaling — ε ≈ 0.58, the architectural tension between decentralized replication and centralized key distribution. This constraint story addresses the aggregate phenomenon; the network links reveal how zero-trust architectures target the key_management component while accepting the coordination benefits of replication.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(paradoxical_decompositions, organized, 0.48).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
