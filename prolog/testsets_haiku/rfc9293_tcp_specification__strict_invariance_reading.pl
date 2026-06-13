% ============================================================================
% CONSTRAINT STORY: rfc9293_tcp_specification__strict_invariance_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_rfc9293_tcp_strict_invariance, []).

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
    narrative_ontology:suppression_profile/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_axiom_grounding/3,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:cs_created_at/2,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: rfc9293_tcp_specification__strict_invariance_reading
 *   human_readable: RFC 9293 Strict TCP State Machine Invariance
 *   domain: network/protocol/standards
 *
 * SUMMARY:
 *   RFC 9293 (STD 7) specifies the Transmission Control Protocol — a global
 *   interoperability mechanism that allows any TCP implementation to exchange
 *   data reliably with any other, anywhere in the world. The STRICT
 *   INVARIANCE READING interprets RFC 9293 as mandating that implementations
 *   replicate a specific state machine exactly: same flag transitions, same
 *   timeout handling, same sequence-number semantics. Deviations — whether
 *   for performance optimization or to accommodate middleboxes — are
 *   specification violations. This reading is one of three live
 *   interpretations of the same RFC. The OPTIMIZATION LATITUDE READING argues
 *   RFC 9293 specifies the byte-stream behavior, not the internal state
 *   machine, and permits implementations to optimize internally while
 *   preserving semantics. The MIDDLEBOX REALISM READING argues the
 *   specification describes endpoint ideals, but real TCP is path-dependent
 *   and shaped by deployed middleboxes, so specification authority must
 *   accommodate network reality. This JSON instantiates ONLY the strict
 *   invariance reading as a clean, ε-invariant constraint with its own
 *   beneficiary structure, extracted cost, and foundational commitments.
 *
 * KEY AGENTS:
 *   - Internet endpoint implementations (TCP stacks in OS kernels, language runtimes, embedded systems): benefit from a canonical specification they can implement once and achieve global interoperability
 *   - Protocol verification community (formal methods researchers, RFC editors, compliance testing): benefit from a fixed, unambiguous specification they can model and test exhaustively
 *   - Middlebox operators (stateful firewalls, load balancers, proxies, NAT boxes): EXCLUDED by strict invariance — their deployed systems modify TCP flags and state in ways the specification forbids
 *   - Performance-optimization implementers (high-performance TCP stacks using semantic-preserving shortcuts): EXCLUDED by strict invariance — their optimizations are classified as specification violations even if they preserve the byte-stream guarantee
 *   - IETF standards community (TCPM working group, RFC authors, steering committee): AGENDA SETTER — interprets and enforces what RFC 9293 means; under strict invariance reading, they have chosen exact replication as the standard
 *   - Applications and protocols layered on TCP (HTTP, SSH, DNS, QUIC, others): BENEFICIARY — depend on TCP's predictability and invariance globally
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(rfc9293_tcp_specification__strict_invariance_reading, 0.12).
domain_priors:suppression_score(rfc9293_tcp_specification__strict_invariance_reading, 0.08).
domain_priors:theater_ratio(rfc9293_tcp_specification__strict_invariance_reading, 0.18).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(rfc9293_tcp_specification__strict_invariance_reading, extractiveness, 0.12).
narrative_ontology:constraint_metric(rfc9293_tcp_specification__strict_invariance_reading, suppression_requirement, 0.08).
narrative_ontology:constraint_metric(rfc9293_tcp_specification__strict_invariance_reading, theater_ratio, 0.18).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(rfc9293_tcp_specification__strict_invariance_reading, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(rfc9293_tcp_specification__strict_invariance_reading, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(rfc9293_tcp_specification__strict_invariance_reading, rope).
narrative_ontology:human_readable(rfc9293_tcp_specification__strict_invariance_reading, "RFC 9293 Strict TCP State Machine Invariance").
narrative_ontology:topic_domain(rfc9293_tcp_specification__strict_invariance_reading, "network/protocol/standards").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(rfc9293_tcp_specification__strict_invariance_reading, '6b42b603-ab1e-46a2-94da-a84945a86634').
narrative_ontology:cs_kernel_codification('6b42b603-ab1e-46a2-94da-a84945a86634', fixed_text).
narrative_ontology:cs_authority_grounding('6b42b603-ab1e-46a2-94da-a84945a86634', lineage).
narrative_ontology:cs_interpretation_layer_present('6b42b603-ab1e-46a2-94da-a84945a86634').
narrative_ontology:cs_reading_relation('6b42b603-ab1e-46a2-94da-a84945a86634', rfc9293_tcp_specification__optimization_latitude_reading, coexists_with).
narrative_ontology:cs_reading_relation('6b42b603-ab1e-46a2-94da-a84945a86634', rfc9293_tcp_specification__middlebox_realism_reading, coexists_with).
narrative_ontology:cs_axiom('6b42b603-ab1e-46a2-94da-a84945a86634', foundational, specification_as_invariant_state_machine).
narrative_ontology:cs_axiom_status(specification_as_invariant_state_machine, holdable).
narrative_ontology:cs_axiom_grounding('6b42b603-ab1e-46a2-94da-a84945a86634', specification_as_invariant_state_machine, conventional).
narrative_ontology:cs_axiom('6b42b603-ab1e-46a2-94da-a84945a86634', foundational, global_interoperability_requires_perfect_specification_adherence).
narrative_ontology:cs_axiom_status(global_interoperability_requires_perfect_specification_adherence, holdable).
narrative_ontology:cs_axiom_grounding('6b42b603-ab1e-46a2-94da-a84945a86634', global_interoperability_requires_perfect_specification_adherence, empirically_contingent).
narrative_ontology:cs_reference_frame('6b42b603-ab1e-46a2-94da-a84945a86634', tcp_endpoint_equivalence_via_invariant_state_machine).
narrative_ontology:cs_drift_state('6b42b603-ab1e-46a2-94da-a84945a86634', contemporary_quic_era_2020s, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('6b42b603-ab1e-46a2-94da-a84945a86634', '').
narrative_ontology:cs_kernel_id(rfc9293_tcp_specification__strict_invariance_reading, rfc9293_tcp_specification).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(rfc9293_tcp_specification__strict_invariance_reading, internet_endpoint_implementations).
narrative_ontology:constraint_beneficiary(rfc9293_tcp_specification__strict_invariance_reading, protocol_verification_community).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(rfc9293_tcp_specification__strict_invariance_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(rfc9293_tcp_specification__strict_invariance_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(rfc9293_tcp_specification__strict_invariance_reading_tests).
:- end_tests(rfc9293_tcp_specification__strict_invariance_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The strict invariance reading is CLAIMED as pure Rope: the coordination benefit of a single, canonical state machine far exceeds the extraction cost. Extractiveness is low (0.12) because the cost imposed on implementers is not rent collection — it is the cost of maintaining specification fidelity, which is necessary for the coordination benefit itself. Suppression is minimal (0.08) because the constraint is sustained by voluntary adoption and specification authority, not by coercion. Implementations choose to comply because the interoperability benefit is worth more than the optimization cost. Accessibility_collapse is high (0.92) because once you understand RFC 9293, the alternative (a fragmented, non-interoperable TCP landscape) is clearly worse, so the constraint feels inevitable. Resistance is moderate (0.35) because performance-optimization implementers and middlebox operators do push back — they argue for latitude and sometimes deviate — but the IETF's specification authority is strong enough to enforce compliance in most of the deployed base. Theater_ratio is modest (0.18) because specification maintenance is mostly real work (RFC errata processing, clarification RFCs, compliance test suites), though some of the enforcement activity is performative (asserting authority when compliance is already high). The measurement series tracks a slight increase in extractiveness and theater_ratio over 40 years: as QUIC and other alternatives emerge, the strict invariance reading faces more pressure to justify itself, so some of the specification work becomes rhetorical defense rather than pure coordination maintenance. But the constraint remains Rope because the core coordination function — global interoperability — is still the dominant force.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of IETF standards authors and endpoint implementations, RFC 9293 is a stable, canonical foundation they have built three decades of internet reliability on. Deviations are spec violations. From the perspective of middlebox operators and performance optimizers, RFC 9293 is a strait-jacket that forces them to choose between compliance and efficiency — they see themselves as improving the internet's real performance while paying a compliance cost for doing so. The engine computes per-seat classification: the IETF and core endpoint implementers should see this as Rope (pure coordination); middlebox operators and optimization implementers should see the SAME CONSTRAINT as Snare (enforced extraction of compliance cost with no offsetting benefit for them). The structural divergence comes from the excluded set's different exit options and directionality: they are trapped or constrained (middleboxes cannot simply stop operating; optimizers cannot avoid TCP); the constraint imposes costs on them without compensating benefit; the constraint's persistence depends on active suppression of their alternative readings. The engine derives this from the authored structure and computes the divergence automatically.
 *
 * DIRECTIONALITY LOGIC:
 *   IETF/endpoint implementers: Beneficiary role, directionality ~0.1–0.2 (near beneficiary end). They collect the coordination benefit; the cost of specification fidelity is the price they pay, but it is proportional to the benefit. Exit is constrained but not trapped — if they left TCP, they would need another protocol, but alternatives exist (QUIC for some use cases). Middlebox operators: Excluded role, directionality ~0.75–0.85 (near target end). They are trapped (physical infrastructure, contracts require TCP modification for network policies). The constraint imposes costs (compliance audits, spec violations when they optimize, pressure from endpoint implementations that refuse to negotiate). They receive no offsetting benefit — the interoperability works fine without their participation, and they are explicitly told their modifications are unwanted. Performance optimizers: Excluded role, similar to middleboxes — directionality ~0.75–0.8. The strict invariance reading targets them: it denies the argument that semantic preservation is sufficient; it requires exact state-machine replication. The cost is engineer-hours spent on specification compliance instead of optimization. The benefit is... the same interoperability that they could claim if they optimized anyway. No directionality override is needed; the derived d from the structural data (trapped/constrained exit, excluded role, no offsetting benefit) lands them at the target end naturally. Applications: Beneficiary role, directionality ~0.3–0.4. They benefit from TCP's stability (low cost to integration testing when TCP semantics are guaranteed stable globally). They also pay a small cost (they cannot use TCP optimization tricks that the specification forbids, even if those tricks would speed up their application). Net: modest beneficiary.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem is implementation divergence in the 1980s–1990s. The problem status is CONTESTED: IETF attests it is still live (implementations still try to deviate, middleboxes still create surprises), but deployment evidence suggests the problem is substantially solved at least for endpoints that follow the specification — the core deployed TCP implementations (Linux, macOS, Windows, BSD) are remarkably aligned. The disappearance verdict is WORLD_REARRANGES: if strict invariance disappeared, implementations would optimize and the interoperability guarantee would erode. But the measurement data and the founding_problem_corroboration show a subtle mandatrophy: the constraint is maintained (RFC 9293 is read and cited), but the enforcement force is declining because the alternative reading (optimization latitude) is gaining credibility, and QUIC is fragmenting the coordination problem into two sub-problems (TCP for legacy interop, QUIC for new protocols). The theater_ratio rising from 0.12 to 0.18 over the interval reflects this: the IETF still asserts strict invariance, but increasingly the assertion is defended in reasoning about why QUIC is separate, why optimization proposals are rejected, why middleboxes are bad — that is rhetorical work, not coordination work. The constraint is not yet dead (it is still enforced, still widely complied with), but it is aging. It is not a Piton (the cost-to-fix is cheap — just issue a new RFC expanding the definition of compliant — but the payer set is diffuse; no single beneficiary would bear the cost of maintaining it if it crumbled). It remains Rope because the founding problem (global interoperability) is live, and strict invariance is still the solution that actually works. But the mandatrophy signal is present: an omega documents the question of whether strict invariance can survive QUIC's emergence.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    strict_invariance_vs_behavioral_semantics,
    'Does RFC 9293 mandate exact state-machine replication (strict invariance reading), or does it permit implementation latitude so long as the observable behavior (reliable ordered byte stream) is preserved (optimization latitude reading)?',
    'Formal comparison of RFC 9293 text against actual deployed TCP implementations (Linux, Windows, macOS, BSD stacks); measurement of whether implementations that achieve identical byte-stream semantics but use different internal state-transition orders comply or violate the specification.',
    'If strict invariance is the true reading, optimization-based deviations are specification violations and should be corrected. If semantic preservation is the true reading, the constraint''s extractiveness and the cost of compliance are lower than authored — implementations can optimize and remain compliant. The classification would remain Rope either way (coordination benefit dominates), but the extraction cost to implementers shifts.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(strict_invariance_vs_behavioral_semantics, conceptual, 'Whether compliance means exact state-machine replication or semantic behavioral equivalence.').

omega_variable(
    middlebox_specification_authority,
    'Is RFC 9293 a specification of endpoint behavior (which middleboxes are free to violate), or is it a global interoperability mandate that middleboxes must respect by staying out of TCP''s internal state machine?',
    'Network telemetry measuring the prevalence and impact of middlebox TCP modification on deployed systems; comparison with RFC 3234 (Middleboxes: Taxonomy and Implications for the Use of End-to-End Security) and RFC 9293''s explicit directives about middlebox constraints; study of whether a strict-interpretation Internet (middleboxes forbidden from modifying TCP state) produces better interoperability than a realist interpretation (middleboxes are inescapable, specification accommodates them).',
    'If middlebox prohibition is the true interpretation, the constraint includes an enforcement obligation against a powerful excluded set (middlebox operators), which raises suppression requirements and may shift the type toward Tangled Rope (coordination + active suppression of alternatives). If middleboxes are permitted with guidance, the constraint is purely descriptive (endpoint guidance) and extractiveness is even lower.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(middlebox_specification_authority, empirical, 'Whether RFC 9293 is an endpoint-only specification or a system-wide mandate that binds middleboxes.').

omega_variable(
    strict_reading_viability,
    'Can the strict invariance reading be maintained as the authoritative interpretation when empirical practice (middleboxes, optimization implementations, TCP Fast Open, TCP Hybla, and other variants) has diverged so far that strict compliance would require global network restructuring?',
    'Historical analysis of RFC errata, TCP working group discussions, and compliance testing frameworks over the interval 1993–2026; measurement of whether the IETF has consistently enforced strict invariance or has quietly accommodated deviation by issuing new RFCs that expand the definition of ''compliant behavior'' to include deployed variants.',
    'If the strict reading is not being maintained in practice, the actual specification (as read by the deployment community) is closer to the optimization-latitude reading. The constraint''s claimed type would be correct, but the extracted cost would be lower — the ''enforcement'' would be mostly theatrical (specification authority is asserted but not enforced). Theater_ratio would rise toward 0.5–0.6, and the constraint might reclassify as Piton (performance of authority with minimal functional enforcement).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(strict_reading_viability, empirical, 'Whether strict invariance is maintained as the living standard or has been softened by practice and successive RFCs.').

omega_variable(
    specification_authority_grounding,
    'Does the IETF''s authority to enforce RFC 9293 ground itself in the specification being a discovered natural law (the only way to achieve reliable communication), or does it ground itself in the specification''s role as a coordination device that could be replaced if a better coordination device emerged?',
    'Comparison with QUIC (RFC 9000), which abandons TCP''s strict invariance in favor of a more flexible, encrypted state machine while achieving the same coordination goal. Study of whether IETF considers QUIC a replacement (suggesting the strict invariance is not natural law, just one solution) or a complementary protocol (suggesting TCP''s strict invariance persists as the authoritative form for certain use cases).',
    'If the specification''s authority is grounded in natural law (only way to achieve reliable communication), the constraint should be reclassified as Mountain and extractiveness should approach 0. If it is grounded in coordination device role (one solution among possible alternatives), the Rope classification holds and extractiveness correctly reflects the cost of coordinating on this particular solution instead of another.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(specification_authority_grounding, conceptual, 'Whether RFC 9293 is a discovered necessity or a constructed coordination device.').

omega_variable(
    reading_contention_site,
    'Where exactly does the strict invariance reading diverge from the optimization-latitude and middlebox-realism readings in RFC 9293''s text? Is there a passage in the RFC that unambiguously settles this, or do all three readings claim the same source text supports their interpretation?',
    'Line-by-line comparison of RFC 9293 (Section 3.2, state transitions; Section 3.10, implementation notes) against the three reading positions. Identification of passages that are read differently by each interpretation; examination of whether the divergence is in the RFC''s content or in the reading community''s choice of emphasis.',
    'If a single passage clearly mandates strict invariance, the strict reading is grounded in the text, and the contention is about whether to follow the text or deviate in practice. If all three readings claim equal textual support, the contention is fundamentally interpretive — the kernel (RFC 9293) is ambiguous, and the three readings are incommensurable. In the latter case, the authority grounding shifts from ''lineage of text interpretation'' to ''power struggle between reading communities.''',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_contention_site, conceptual, 'Whether the strict invariance reading is grounded in unambiguous text or in contested interpretation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(rfc9293_tcp_specification__strict_invariance_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(rfc9_tr_t0, rfc9293_tcp_specification__strict_invariance_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(rfc9_tr_t5, rfc9293_tcp_specification__strict_invariance_reading, theater_ratio, 5, 0.13).
narrative_ontology:measurement(rfc9_tr_t10, rfc9293_tcp_specification__strict_invariance_reading, theater_ratio, 10, 0.145).
narrative_ontology:measurement(rfc9_tr_t15, rfc9293_tcp_specification__strict_invariance_reading, theater_ratio, 15, 0.16).
narrative_ontology:measurement(rfc9_tr_t20, rfc9293_tcp_specification__strict_invariance_reading, theater_ratio, 20, 0.17).
narrative_ontology:measurement(rfc9_tr_t25, rfc9293_tcp_specification__strict_invariance_reading, theater_ratio, 25, 0.18).
narrative_ontology:measurement(rfc9_tr_t30, rfc9293_tcp_specification__strict_invariance_reading, theater_ratio, 30, 0.18).
narrative_ontology:measurement(rfc9_tr_t40, rfc9293_tcp_specification__strict_invariance_reading, theater_ratio, 40, 0.18).

% Extraction over time
narrative_ontology:measurement(rfc9_be_t0, rfc9293_tcp_specification__strict_invariance_reading, base_extractiveness, 0, 0.08).
narrative_ontology:measurement(rfc9_be_t5, rfc9293_tcp_specification__strict_invariance_reading, base_extractiveness, 5, 0.09).
narrative_ontology:measurement(rfc9_be_t10, rfc9293_tcp_specification__strict_invariance_reading, base_extractiveness, 10, 0.1).
narrative_ontology:measurement(rfc9_be_t15, rfc9293_tcp_specification__strict_invariance_reading, base_extractiveness, 15, 0.11).
narrative_ontology:measurement(rfc9_be_t20, rfc9293_tcp_specification__strict_invariance_reading, base_extractiveness, 20, 0.115).
narrative_ontology:measurement(rfc9_be_t25, rfc9293_tcp_specification__strict_invariance_reading, base_extractiveness, 25, 0.12).
narrative_ontology:measurement(rfc9_be_t30, rfc9293_tcp_specification__strict_invariance_reading, base_extractiveness, 30, 0.12).
narrative_ontology:measurement(rfc9_be_t40, rfc9293_tcp_specification__strict_invariance_reading, base_extractiveness, 40, 0.12).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(rfc9293_tcp_specification__strict_invariance_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(rfc9293_tcp_specification__strict_invariance_reading, global_infrastructure).
narrative_ontology:affects_constraint(rfc9293_tcp_specification__strict_invariance_reading, rfc9293_tcp_specification__optimization_latitude_reading).
narrative_ontology:affects_constraint(rfc9293_tcp_specification__strict_invariance_reading, rfc9293_tcp_specification__middlebox_realism_reading).

% DUAL FORMULATION NOTE:
% RFC 9293 is a contested kernel instantiated in three structurally distinct constraint stories. The strict_invariance_reading (this file) authorizes exact state-machine replication and classifies deviations as violations. The optimization_latitude_reading authorizes semantic-preserving deviation and classifies optimization shortcuts as compliant. The middlebox_realism_reading subordinates specification authority to empirical network behavior. All three stories have the same epsilon value range but different founding authority grounds and different victim/excluded sets. Strict invariance has the lowest extractiveness (0.12) because the coordination benefit dominates; optimization latitude has low-to-moderate extractiveness (0.15–0.22) because the extraction is the cost of negotiating semantic equivalence; middlebox realism has moderate extractiveness (0.25–0.35) because the constraint becomes a description of actual deployed behavior, which includes extractive middlebox activity. Links run both directions: strict invariance influences (constrains) the latitude reading by asserting a stricter standard; middlebox realism influences strict invariance by presenting deployed alternatives; optimization latitude influences both by claiming equivalence without replication.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
