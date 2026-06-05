% ============================================================================
% CONSTRAINT STORY: paradoxical_decompositions
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
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
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: paradoxical_decompositions
 *   human_readable: Data Replication Paradox: Security vs. Availability Trade-Off
 *   domain: technological/cybersecurity/distributed_systems
 *
 * SUMMARY:
 *   The Data Replication Paradox emerges at the intersection of two competing
 *   institutional demands: regulatory and customer requirements for high data
 *   availability (99.99%+ uptime SLAs) and security frameworks that treat
 *   data exposure as a primary threat. Traditional replication architectures
 *   (multi-master, read replicas, distributed caches) solve the availability
 *   problem by creating multiple authoritative or semi-authoritative copies
 *   of sensitive data. Each copy requires authentication credentials,
 *   introduces a new synchronization dependency, and creates a potential
 *   incident path if any replica is compromised. The paradox is that the
 *   solution to one problem (availability failure) exacerbates another
 *   (security failure) — increasing the system's resilience to hardware
 *   failure simultaneously increases its vulnerability to credential
 *   compromise, supply chain attacks, and insider threats. This constraint is
 *   neither purely coordination (the rope perspective) nor purely extraction
 *   (the snare perspective), but a genuine hybrid where both views are
 *   structurally correct from different agent positions. The operations team
 *   genuinely benefits from replication (failover, disaster recovery) while
 *   genuinely suffering from expanded attack surface (more credentials to
 *   rotate, more sync mechanisms to monitor). The security perimeter has no
 *   exit: a system without replication cannot meet modern availability
 *   requirements, yet each replica deepens the perimeter's burden. The
 *   constraint's theater_ratio (0.58) reflects that much of the replication
 *   infrastructure is performative risk management — the ritual of 'mirroring
 *   everywhere' provides psychological assurance while deepening actual
 *   exposure. Cryptographic approaches (zero-trust, threshold signatures,
 *   Byzantine consensus) are building alternative pathways that decouple
 *   availability from credential exposure, but these are still maturing and
 *   not universally deployed.
 *
 * KEY AGENTS:
 *   - System Availability Advocates: Institutional beneficiary (institutional/arbitrage) — SLA enforcement, regulatory uptime mandates, customer expectations. Drive replication as availability solution.
 *   - Security Perimeter / Incident Response: Primary victim (powerless/trapped) — each replica expands attack surface and credential management burden; cannot opt out.
 *   - Operations Team: Secondary victim (moderate/constrained) — must implement and maintain both replication (availability) and credential hygiene (security); resource-constrained.
 *   - Data Integrity Assurance: Abstract victim (powerless/trapped) — synchronization inconsistencies, cascading breach scenarios where compromise on one replica affects all copies.
 *   - Cryptographic Authentication Coalition: Organized agents (organized/constrained) — zero-trust vendors, BFT framework developers, threshold cryptography researchers building alternative pathways.
 *   - Legacy Replication Architecture: Institutional inertia (institutional/arbitrage) — traditional master-replica patterns persist despite degradation; piton classification.
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing the paradox as inherent to distributed systems rather than contingent on architectural choices.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(paradoxical_decompositions, 0.52).
domain_priors:suppression_score(paradoxical_decompositions, 0.68).
domain_priors:theater_ratio(paradoxical_decompositions, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(paradoxical_decompositions, extractiveness, 0.52).
narrative_ontology:constraint_metric(paradoxical_decompositions, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(paradoxical_decompositions, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(paradoxical_decompositions, tangled_rope).
narrative_ontology:human_readable(paradoxical_decompositions, "Data Replication Paradox: Security vs. Availability Trade-Off").
narrative_ontology:topic_domain(paradoxical_decompositions, "technological/cybersecurity/distributed_systems").

domain_priors:requires_active_enforcement(paradoxical_decompositions).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(paradoxical_decompositions, system_availability_advocates).
narrative_ontology:constraint_beneficiary(paradoxical_decompositions, operational_continuity_stakeholders).
narrative_ontology:constraint_victim(paradoxical_decompositions, security_perimeter).
narrative_ontology:constraint_victim(paradoxical_decompositions, data_integrity_assurance).
narrative_ontology:constraint_victim(paradoxical_decompositions, breach_impact_surface).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SECURITY PERIMETER (SNARE) — Each additional replica introduces new attack vectors, credential stores, and synchronization dependencies. The security perimeter cannot exit this constraint: modern systems require high availability, and high availability architectures mandate replication. d≈0.92, f(d)≈1.38, σ=1.2 → χ≈0.87. The perimeter bears the full cost of expanded attack surface with no choice in the matter.
constraint_indexing:constraint_classification(paradoxical_decompositions, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: OPERATIONS TEAM (TANGLED ROPE) — Must maintain both availability (benefits from replication) and security (harmed by replication). Constrained by resource limits and expertise gaps. The constraint provides coordination benefit (failover, disaster recovery) but extracts cost (credential management overhead, distributed monitoring burden). d≈0.68, f(d)≈1.05, σ=0.9 → χ≈0.52.
constraint_indexing:constraint_classification(paradoxical_decompositions, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: DATA AVAILABILITY MANDATE / SLA ENFORCEMENT (ROPE) — Institutional requirement (SLAs, regulatory uptime requirements, customer expectations) mandates high availability. The mandate benefits from replication as a solution mechanism. Sees the constraint as pure coordination: replication IS the availability solution. d≈0.08, f(d)≈-0.10, σ=1.2 → χ≈-0.06. Net beneficiary; negative effective extraction.
constraint_indexing:constraint_classification(paradoxical_decompositions, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: CRYPTOGRAPHIC AUTHENTICATION COALITION (SCAFFOLD) — Zero-trust architectures, hardware security modules, and distributed trust frameworks (e.g., BFT consensus, threshold cryptography) are building alternative verification pathways that reduce the replication-security paradox. These organized agents see the paradox as solvable via cryptographic substitution for trust-based replication. d≈0.35, f(d)≈0.28, σ=1.2 → χ≈0.17. Low extraction; sunset clause implicit in technology maturation.
constraint_indexing:constraint_classification(paradoxical_decompositions, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: LEGACY REPLICATION ARCHITECTURE (PITON) — Traditional master-replica and multi-master architectures persist through institutional inertia despite degradation. New systems (immutable ledgers, event sourcing, CRDT-based coordination) offer better security properties, but legacy infrastructure remains deployed. theater_ratio=0.58 captures the performative aspect: replication as security theater — the ritual of 'mirroring data everywhere' provides psychological assurance of availability while deepening exposure. d≈0.10, f(d)≈-0.08, σ=1.2 → χ≈-0.05.
constraint_indexing:constraint_classification(paradoxical_decompositions, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / THEORETICAL LIMIT VIEW (MOUNTAIN) — From information-theoretic perspective, there is an irreducible trade-off between the number of system copies (availability) and the number of potential compromise points (security). Byzantine Fault Tolerance (BFT) theorems establish that protecting N+1 components requires more security resources than protecting N. The paradox appears as a natural law: more copies = more surface area. However, the structural data (ε=0.52, suppression=0.68, theater=0.58) contradicts pure mountain classification — cryptographic solutions (threshold schemes, zero-trust) can decouple replication from credential exposure, suggesting the paradox is contingent, not natural.
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
 *   Extractiveness (0.52): Moderate-high. The replication paradox creates asymmetric benefits and costs. The availability mandate benefits substantially from replication (low-cost solution to high SLA requirements). The security perimeter bears the cost (expanded attack surface, credential management complexity, monitoring burden). The extraction is not extreme because some agents (operations teams) benefit from both aspects and can implement mitigations (encryption at rest, credential rotation, network segmentation). The trajectory shows extractiveness increasing from 0.28 to 0.52 over 20 time periods as systems scale and complexity compounds — each additional replica increases the base extraction cost. Suppression (0.68): High. Significant barriers to exit include regulatory requirements (GDPR, PCI-DSS, HIPAA mandates for data availability and auditability), customer SLA contracts (99.99%+ uptime), and competitive pressure (organizations that cannot provide availability lose market share). Operational alternatives (event sourcing, immutable logs, zero-trust architectures) exist but require significant re-engineering and expertise gaps. Theater ratio (0.58): Moderate-high. Replication architectures contain substantial performative elements: the ritual of 'data is everywhere' provides psychological assurance of both availability and backup/disaster recovery, but actual protection is contingent on implementation details (are replicas independently secured? are credentials managed separately? is synchronization monitored?). The theater ratio increased over the interval as cloud infrastructure commoditized replication, making it easier to implement replication without understanding the security implications. Modern security frameworks increasingly recognize that 'more copies = safer' is false, driving re-evaluation of the performance/security balance.
 *
 * PERSPECTIVAL GAP:
 *   This constraint shows classic perspectival disagreement between the primary beneficiary and primary victim. The availability mandate sees replication as pure coordination (Rope) — a scalable solution to a collective action problem (everyone wants their data available). The security perimeter sees extraction (Snare) — each replica deepens exposure with no alternative. Operations teams experience both (Tangled Rope) — genuine coordination benefit (failover, disaster recovery) mixed with genuine extraction cost (credential proliferation, monitoring complexity). The scaffold perspective (Cryptographic Authentication Coalition) sees the paradox as temporary and solvable — architectural alternatives are emerging that decouple availability from credential exposure, with a sunset path as zero-trust and BFT systems mature. The piton perspective (Legacy Replication Architecture) sees the constraint as degraded — the traditional replication pattern persists through institutional inertia despite better alternatives emerging, and the security theater (the assurance that 'replication = safety') has become decoupled from actual security properties. The analytical observer risks naturalizing the paradox as a fundamental theorem of distributed systems (Mountain) — more copies must mean more surface area — but cryptographic solutions suggest the trade-off is contingent on architectural choices, not inherent.
 *
 * DIRECTIONALITY LOGIC:
 *   System Availability Mandate: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.10. Net beneficiary; replication is the solution they require. Security Perimeter: Victim + trapped → d≈0.92, f(d)≈1.38. Maximum extraction — no exit available; all replication requirements flow from institutional decisions made by other agents. Operations Team: Victim + constrained (moderate power) → d≈0.68, f(d)≈1.05. Significant extraction because they bear implementation burden; constrained exit because re-architecting requires capital and expertise. Cryptographic Coalition: Organized + constrained → d≈0.35, f(d)≈0.28. Low effective extraction; organized agents see a path forward and can build alternatives. Legacy Replication Pattern: Institutional + arbitrage → d≈0.10, f(d)≈-0.08. Piton classification comes from theater gate (≥0.70 not met, but 0.58 is still significant), not from directionality — the pattern is neither pure beneficiary nor victim, but an inertial institutional structure.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by recognizing that replication can be BOTH a coordination solution (Rope for availability) AND an extraction mechanism (Snare for security) depending on whether the architectural choice is well-managed. The availability mandate genuinely needs replication to meet SLAs — that's coordination. But the replication requirement ALSO genuinely increases attack surface — that's extraction. The constraint is not 'which type is correct?' but 'what is the actual cost structure, and who bears it?' The tangled rope classification captures this: the constraint provides coordination function (availability), requires active enforcement (credential management, monitoring), and exhibits asymmetric extraction (security perimeter bears costs that availability advocates do not). The mandatrophy is resolved when we recognize that the paradox is not inherent (mountain) but contingent (tangled rope with a scaffold path). Alternative architectures (zero-trust, immutable event logs, Byzantine consensus) can decouple availability from credential exposure, suggesting the paradox is solvable, not fundamental. The classification should remain tangled rope (not degrade to mountain) as long as the cryptographic alternatives are not universally adopted — the constraint continues to extract from the security perimeter even though a path forward exists.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    cryptographic_substitution_feasibility,
    'Can distributed cryptographic trust (threshold signatures, Byzantine Fault Tolerance, zero-trust networks) fully decouple replication-for-availability from credential exposure, or are there irreducible trade-offs that persist even with perfect cryptography?',
    'Comparison of BFT-based systems (Tendermint, Polkadot, libp2p-based networks) vs. traditional replication: measure attack surface (number of private keys needed), credential management complexity, and latency cost. Identify whether security improvements come from better architecture or merely hidden in different cost (computation, latency).',
    'If cryptographic substitution is sufficient: the paradox resolves to a resource trade-off (computation cost replaces credential exposure), and the scaffold perspective becomes dominant — sunset path is clear. If not: the paradox is more fundamental, and snare/tangled rope classifications persist.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cryptographic_substitution_feasibility, empirical, 'Whether cryptographic trust can eliminate replication-security coupling').

omega_variable(
    regulatory_availability_mandate_interpretation,
    'Do regulatory requirements (GDPR right-to-retrieve, healthcare backup mandates, financial transaction audit logs) mandate physical data replication, or merely mandate the OUTCOME of high availability?',
    'Regulatory text analysis paired with implementer interviews. Test whether immutable event logs with cryptographic commitment chains satisfy regulatory intent without traditional replication.',
    'If regulations mandate outcome only: availability can be achieved through architecturally different means (event sourcing, ledger-based approaches), reducing the design coupling. If regulations mandate specific mechanisms: the rope perspective (availability as pure coordination) is overstated, and the tangled rope assessment is correct.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(regulatory_availability_mandate_interpretation, conceptual, 'Whether regulations mandate replication specifically or availability generically').

omega_variable(
    implicit_threat_model_mismatch,
    'Do traditional replication architectures assume a threat model (external attackers, not insider threats) that is increasingly invalid in cloud/multi-tenant environments where the replication infrastructure itself may be compromised?',
    'Historical breach analysis: how many incidents involved compromised replica infrastructure, database credentials on multiple nodes, or supply chain attacks on sync mechanisms. Compare incident rates in replicated vs. non-replicated systems controlling for system size.',
    'If replication architectures embed outdated threat model: the paradox is partly institutional (outdated security assumptions) rather than fundamental. The piton perspective becomes stronger — the replication pattern persists because it matches legacy threat models, not current reality.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(implicit_threat_model_mismatch, empirical, 'Whether replication architectures encode outdated threat models').

omega_variable(
    operational_friction_extraction,
    'Is the ''extraction'' cost of replication (credential management, monitoring, orchestration overhead) primarily a legitimate security burden, or does it create artificial dependency on specialized tools and consulting services that extract value from the paradox?',
    'Tool cost analysis: identify whether orchestration frameworks (Kubernetes, Terraform, replication managers) reduce replication overhead or primarily monetize it. Compare operational burden (team size, training cost) for architectures with and without replication, controlling for feature parity.',
    'If tools genuinely reduce overhead: extraction is minimal, and the tangled rope classification is accurate. If tools primarily monetize the complexity: the snare perspective becomes stronger — the paradox creates captive markets for ''solutions'' that lock in dependency.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(operational_friction_extraction, empirical, 'Whether replication tools solve or monetize the security-availability paradox').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(paradoxical_decompositions, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(drp_tr_t0, paradoxical_decompositions, theater_ratio, 0, 0.35).
narrative_ontology:measurement(drp_tr_t10, paradoxical_decompositions, theater_ratio, 10, 0.47).
narrative_ontology:measurement(drp_tr_t20, paradoxical_decompositions, theater_ratio, 20, 0.58).

% Extraction over time
narrative_ontology:measurement(drp_be_t0, paradoxical_decompositions, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(drp_be_t10, paradoxical_decompositions, base_extractiveness, 10, 0.4).
narrative_ontology:measurement(drp_be_t20, paradoxical_decompositions, base_extractiveness, 20, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(paradoxical_decompositions, resource_allocation).
narrative_ontology:affects_constraint(paradoxical_decompositions, credential_management_complexity).
narrative_ontology:affects_constraint(paradoxical_decompositions, breach_surface_amplification).
narrative_ontology:affects_constraint(paradoxical_decompositions, synchronization_consistency_cost).

% DUAL FORMULATION NOTE:
% The Data Replication Paradox decomposes into multiple structural constraints: credential_management_complexity (ε≈0.45, tangled rope) covers the access control burden; breach_surface_amplification (ε≈0.58, snare) covers the attack surface expansion; synchronization_consistency_cost (ε≈0.38, tangled rope) covers the coordination overhead. The parent paradox story captures the emergent property that arises from their interaction — each individual constraint is manageable, but their combination creates the paradox. This story (data_replication_paradox, ε=0.52) is downstream of all three, showing how they reinforce each other.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
