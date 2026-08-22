% ============================================================================
% CONSTRAINT STORY: rfc9293_tcp_specification__optimization_latitude_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_rfc9293_tcp_specification__optimization_latitude_reading, []).

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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:suppression_profile/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: rfc9293_tcp_specification__optimization_latitude_reading
 *   human_readable: RFC 9293 as Semantic Contract with Implementation Latitude
 *   domain: network_protocol_engineering/internet_standards
 *
 * SUMMARY:
 *   This story instantiates the optimization_latitude_reading of the RFC 9293
 *   kernel: the specification is read as defining a behavioral/semantic
 *   contract (reliable, ordered, byte-stream delivery with well-defined
 *   connection lifecycle) while deliberately leaving implementation strategy
 *   — congestion control, retransmission timing, buffer management —
 *   unspecified. On this reading the constraint functions as a Rope: it
 *   solves a genuine coordination problem (universal interoperability across
 *   independently developed stacks) with very low extraction, because no
 *   party captures rents from the latitude itself; the latitude is the
 *   mechanism by which cloud operators, hardware vendors, and researchers all
 *   gain, while end users benefit from the resulting performance improvements
 *   without needing to understand or consent to the specific means. This is a
 *   DIFFERENT constraint from the strict_invariance_reading (which would
 *   treat any implementation divergence as an interoperability threat
 *   requiring remediation) and from the middlebox_realism_reading (which
 *   treats deployed middlebox behavior, not the RFC text, as the actual
 *   operative authority). Each reading is its own file with its own epsilon;
 *   this file's epsilon is low and does not average against the siblings'
 *   higher-extraction readings.
 *
 * KEY AGENTS:
 *   - ietf_tcpm_working_group: agenda-setter, drafts and maintains the semantic/implementation boundary
 *   - cloud_operators: primary beneficiary, exploits latitude at scale via custom congestion control
 *   - network_equipment_vendors: beneficiary, differentiates on implementation quality
 *   - protocol_researchers: beneficiary, latitude is the research object itself
 *   - end_users: diffuse beneficiary, receives performance gains with no visibility into means
 *   - small_independent_implementers: payer, bears burden of chasing a moving de facto performance baseline
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(rfc9293_tcp_specification__optimization_latitude_reading, 0.08).
domain_priors:suppression_score(rfc9293_tcp_specification__optimization_latitude_reading, 0.12).
domain_priors:theater_ratio(rfc9293_tcp_specification__optimization_latitude_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(rfc9293_tcp_specification__optimization_latitude_reading, extractiveness, 0.08).
narrative_ontology:constraint_metric(rfc9293_tcp_specification__optimization_latitude_reading, suppression_requirement, 0.12).
narrative_ontology:constraint_metric(rfc9293_tcp_specification__optimization_latitude_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(rfc9293_tcp_specification__optimization_latitude_reading, accessibility_collapse, 0.25).
narrative_ontology:constraint_metric(rfc9293_tcp_specification__optimization_latitude_reading, resistance, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(rfc9293_tcp_specification__optimization_latitude_reading, rope).
narrative_ontology:human_readable(rfc9293_tcp_specification__optimization_latitude_reading, "RFC 9293 as Semantic Contract with Implementation Latitude").
narrative_ontology:topic_domain(rfc9293_tcp_specification__optimization_latitude_reading, "network_protocol_engineering/internet_standards").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(rfc9293_tcp_specification__optimization_latitude_reading, 'aad30e7a-6389-42dc-8862-9756bc8f0bed').
narrative_ontology:cs_kernel_codification('aad30e7a-6389-42dc-8862-9756bc8f0bed', formalized).
narrative_ontology:cs_authority_grounding('aad30e7a-6389-42dc-8862-9756bc8f0bed', expertise).
narrative_ontology:cs_interpretation_layer_present('aad30e7a-6389-42dc-8862-9756bc8f0bed').
narrative_ontology:cs_reading_relation('aad30e7a-6389-42dc-8862-9756bc8f0bed', rfc9293_tcp_specification__strict_invariance_reading, coexists_with).
narrative_ontology:cs_reading_relation('aad30e7a-6389-42dc-8862-9756bc8f0bed', rfc9293_tcp_specification__middlebox_realism_reading, influences).
narrative_ontology:cs_axiom('aad30e7a-6389-42dc-8862-9756bc8f0bed', foundational, behavioral_outcome_specification_suffices_for_interoperability).
narrative_ontology:cs_axiom_status(behavioral_outcome_specification_suffices_for_interoperability, holdable).
narrative_ontology:cs_axiom_grounding('aad30e7a-6389-42dc-8862-9756bc8f0bed', behavioral_outcome_specification_suffices_for_interoperability, empirically_contingent).
narrative_ontology:cs_axiom('aad30e7a-6389-42dc-8862-9756bc8f0bed', secondary, implementation_diversity_is_a_feature_not_a_defect).
narrative_ontology:cs_axiom_status(implementation_diversity_is_a_feature_not_a_defect, holdable).
narrative_ontology:cs_axiom_grounding('aad30e7a-6389-42dc-8862-9756bc8f0bed', implementation_diversity_is_a_feature_not_a_defect, instrumental).
narrative_ontology:cs_reference_frame('aad30e7a-6389-42dc-8862-9756bc8f0bed', semantic_contract_with_open_implementation_space).
narrative_ontology:cs_drift_state('aad30e7a-6389-42dc-8862-9756bc8f0bed', post_bbr_dctcp_deployment_era, gap(stable, minor, true)).
narrative_ontology:cs_created_at('aad30e7a-6389-42dc-8862-9756bc8f0bed', '').
narrative_ontology:cs_kernel_id(rfc9293_tcp_specification__optimization_latitude_reading, rfc9293_tcp_specification).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(rfc9293_tcp_specification__optimization_latitude_reading, network_equipment_vendors).
narrative_ontology:constraint_beneficiary(rfc9293_tcp_specification__optimization_latitude_reading, cloud_operators).
narrative_ontology:constraint_beneficiary(rfc9293_tcp_specification__optimization_latitude_reading, protocol_researchers).
narrative_ontology:constraint_beneficiary(rfc9293_tcp_specification__optimization_latitude_reading, end_users).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(rfc9293_tcp_specification__optimization_latitude_reading, small_independent_implementers).
narrative_ontology:constraint_vindicates(rfc9293_tcp_specification__optimization_latitude_reading, semantic_contract_sufficiency_doctrine).
narrative_ontology:constraint_vindicates(rfc9293_tcp_specification__optimization_latitude_reading, implementation_diversity_compatible_with_interoperability).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintains and revises the RFC text, drawing the boundary between what is mandated (state machine outcomes, byte-stream reliability, retransmission semantics) and what is left open (congestion control algorithm, buffer management, timer tuning). Does not enforce compliance directly; relies on interoperability testing and rough consensus to keep the boundary legible.
narrative_ontology:constraint_stakeholder(rfc9293_tcp_specification__optimization_latitude_reading, ietf_tcpm_working_group, agenda_setter,
    institutional, civilizational, analytical, global).

% Deploy custom congestion control (BBR and successors) inside their own data centers and edge networks, capturing large throughput and latency gains while remaining interoperable with any RFC-9293-compliant peer. Their exit option is genuinely mobile: they can swap congestion control algorithms unilaterally without renegotiating the standard, because the spec never mandated one.
narrative_ontology:constraint_stakeholder(rfc9293_tcp_specification__optimization_latitude_reading, cloud_operators, beneficiary,
    organized, biographical, arbitrage, global).

% Differentiate router and NIC offload hardware on performance characteristics within the semantic envelope the RFC guarantees. Can compete on implementation quality (buffer strategies, selective-ACK handling, delayed-ACK tuning) rather than being forced into a single reference implementation.
narrative_ontology:constraint_stakeholder(rfc9293_tcp_specification__optimization_latitude_reading, network_equipment_vendors, beneficiary,
    powerful, generational, mobile, global).

% Design and publish new congestion control and loss-recovery algorithms (DCTCP, CUBIC variants, ECN-based schemes) that live entirely inside the optimization latitude the RFC leaves open. Career currency depends on the existence of unspecified implementation space to innovate within; a fully invariant state machine would eliminate their research object.
narrative_ontology:constraint_stakeholder(rfc9293_tcp_specification__optimization_latitude_reading, protocol_researchers, beneficiary,
    moderate, generational, mobile, global).

% Experience the practical result of the latitude — faster page loads, better video streaming, lower latency — without any awareness of or say in which congestion control variant is running underneath. They cannot select or influence the implementation choice, but they are not extracted from by it; the variance in implementation is what delivers the improved outcome they receive.
narrative_ontology:constraint_stakeholder(rfc9293_tcp_specification__optimization_latitude_reading, end_users, beneficiary,
    powerless, immediate, trapped, global).

% Small teams writing embedded or IoT TCP stacks must implement enough of the optional performance machinery (window scaling, SACK, timestamps) to interoperate acceptably with modern peers, even though these are technically optional extensions layered onto the mandatory core. The latitude that benefits large operators imposes a real, if modest, implementation burden on resource-constrained implementers who must chase a moving performance baseline to avoid being treated as degraded peers.
narrative_ontology:constraint_stakeholder(rfc9293_tcp_specification__optimization_latitude_reading, small_independent_implementers, payer,
    powerless, biographical, constrained, national).

% Run conformance and interoperability test suites (e.g., university and vendor interop events) that validate whether latitude-taking implementations still honor the mandatory semantic contract. They do not set the boundary but continuously measure whether it is holding.
narrative_ontology:constraint_stakeholder(rfc9293_tcp_specification__optimization_latitude_reading, interoperability_test_bodies, observer,
    institutional, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the genuine multi-decade problem of letting thousands of independently-developed TCP stacks interoperate reliably while still allowing continuous performance innovation: by specifying only the observable behavioral contract (in-order reliable byte delivery, connection lifecycle, flow control signaling) and leaving algorithmic means (congestion control, retransmission timing, buffer strategy) unspecified, the standard lets implementations compete and evolve without breaking the network.
% TRANSFER_FUNCTION: Under this reading, the arrangement does not primarily move rents from one party to another; it distributes the freedom to optimize. What is 'transferred' is degrees of freedom — from a hypothetical single mandated implementation to individual implementers — with modest compliance burden shifted onto small implementers who must track de facto performance extensions to remain interoperable in practice.
% ABSENT_VOICES: Small embedded/IoT implementers with limited engineering budgets have no seat at IETF working group meetings in practice; they bear the burden of an expanding de facto baseline (window scaling, SACK, ECN) without having shaped the latitude that produced it. Legacy or minimal-conformance stacks are similarly underrepresented.
% DISAPPEARANCE_RATIONALE: If the latitude were removed and a single invariant implementation mandated (the strict_invariance_reading's world), decades of accumulated performance research (BBR, DCTCP, CUBIC, ECN-based schemes) would become non-conforming overnight; cloud operators would lose the ability to unilaterally improve throughput; protocol research as a field would lose its object. The internet would not stop functioning, but its capacity to improve without a global renegotiation would disappear.
% FOUNDING_PROBLEM: Early TCP congestion collapse events (notably 1986) showed that rigid, universally-mandated behavior was itself dangerous — a single frozen algorithm could not adapt to changing network conditions. The founding problem this reading solves is: how do you standardize interoperability without standardizing (and thereby freezing) the algorithms that determine performance and safety under load.
% FOUNDING_PROBLEM_CORROBORATION: Independent academic network-measurement studies (e.g., ongoing IETF/IRTF congestion control research group publications, and university-run interoperability testbeds not affiliated with any single vendor) continue to document active, beneficial algorithmic diversity within the semantic envelope, corroborating that the latitude is still doing real coordination work rather than having become vestigial.
narrative_ontology:disappearance_verdict(rfc9293_tcp_specification__optimization_latitude_reading, world_rearranges).
narrative_ontology:founding_problem_status(rfc9293_tcp_specification__optimization_latitude_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(rfc9293_tcp_specification__optimization_latitude_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(rfc9293_tcp_specification__optimization_latitude_reading, 'none', 1).
narrative_ontology:epsilon_provenance(rfc9293_tcp_specification__optimization_latitude_reading, 0.08, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(rfc9293_tcp_specification__optimization_latitude_reading_tests).
:- end_tests(rfc9293_tcp_specification__optimization_latitude_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored low (0.08) because under this reading no party structurally profits from the latitude at another's expense — the beneficiaries (operators, vendors, researchers, users) gain through the SAME mechanism that keeps costs to small implementers modest and largely a function of general technical progress, not enforced rent extraction. Suppression is low (0.12): non-conforming or minimal implementations are not coercively excluded, they simply interoperate less well, and the RFC process itself is voluntary and consensus-based. Accessibility collapse is moderate-low (0.25): while a compliant implementation must honor the mandatory semantic core, the optimization space genuinely remains open and has been repeatedly used (Reno, NewReno, CUBIC, BBR, DCTCP), evidencing that alternatives are not suppressed. Resistance is low (0.15): implementers broadly welcome, rather than resist, the freedom to optimize; the mild friction that exists is small-implementer burden, not organized resistance to the arrangement.
 *
 * DIRECTIONALITY LOGIC:
 *   Cloud operators and vendors sit near the full-beneficiary end: they have arbitrage/mobile exit and directly capture performance gains created by the latitude. Protocol researchers are also beneficiaries — their entire professional object depends on the latitude persisting. End users are beneficiaries by outcome despite trapped exit options, because the constraint does not extract from them; it is a pass-through of gains they cannot themselves negotiate. Small independent implementers are the one payer seat: their exit is constrained (they must track the expanding de facto baseline or accept degraded interoperability), which is a real but modest cost, not an extraction of the coordination surplus generated elsewhere.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (avoiding congestion collapse from a single frozen algorithm) remains empirically live — network conditions keep changing and new congestion control research keeps arriving — so this reading shows no mandatrophy: the mandate (leave implementation open) still tracks a real ongoing need, corroborated by independent, non-beneficiary sources (academic interoperability testbeds, IRTF measurement literature).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    latitude_boundary_stability,
    'Is the line between ''mandatory semantic contract'' and ''optional implementation latitude'' itself stable over time, or does de facto practice (e.g., near-universal expectation of SACK/window-scaling/ECN support) gradually convert optional latitude into practically mandatory baseline — effectively narrowing the latitude this reading depends on?',
    'Longitudinal survey of interoperability test suite requirements and default OS TCP stack configurations across releases; track whether ''optional'' RFC extensions become de facto required for acceptable performance parity.',
    'If the boundary is migrating such that latitude becomes obligation, the small_independent_implementers payer burden would be rising over time (not static as currently authored), and the constraint would be drifting toward the tangled_rope profile the strict_invariance_reading already claims.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(latitude_boundary_stability, empirical, 'Whether de facto practice erodes the optional/mandatory boundary this reading relies on.').

omega_variable(
    reading_selection_is_a_framing_choice,
    'Is the choice to read RFC 9293 through the optimization_latitude lens (rather than strict_invariance or middlebox_realism) itself a neutral structural observation, or does it reflect which community''s vantage point (protocol designers and large operators vs. network operators dealing with middlebox interference vs. implementers demanding exact conformance) is treated as authoritative?',
    'Compare which reading each named party (IETF authors, middlebox vendors, small implementers) would themselves endorse as the operative account of the RFC''s authority; a reading endorsed mainly by the parties who benefit from latitude is weaker evidence than one corroborated by parties who bear its costs.',
    'If the optimization_latitude reading is disproportionately the account preferred by the very beneficiaries (cloud operators, researchers) who profit from it, that would not change this file''s own epsilon (ε is reading-indexed, not resolved by consensus) but would strengthen the case that the sibling readings deserve equal or greater authoritative weight in any aggregate assessment of ''the RFC 9293 constraint'' as a colloquial label.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_selection_is_a_framing_choice, conceptual, 'Whether reading selection tracks beneficiary interest rather than neutral structural fact.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(rfc9293_tcp_specification__optimization_latitude_reading, 1981, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(rfc9_tr_t1981, rfc9293_tcp_specification__optimization_latitude_reading, theater_ratio, 1981, 0.03).
narrative_ontology:measurement(rfc9_tr_t1995, rfc9293_tcp_specification__optimization_latitude_reading, theater_ratio, 1995, 0.03).
narrative_ontology:measurement(rfc9_tr_t2004, rfc9293_tcp_specification__optimization_latitude_reading, theater_ratio, 2004, 0.04).
narrative_ontology:measurement(rfc9_tr_t2012, rfc9293_tcp_specification__optimization_latitude_reading, theater_ratio, 2012, 0.04).
narrative_ontology:measurement(rfc9_tr_t2018, rfc9293_tcp_specification__optimization_latitude_reading, theater_ratio, 2018, 0.05).
narrative_ontology:measurement(rfc9_tr_t2024, rfc9293_tcp_specification__optimization_latitude_reading, theater_ratio, 2024, 0.05).

% Extraction over time
narrative_ontology:measurement(rfc9_be_t1981, rfc9293_tcp_specification__optimization_latitude_reading, base_extractiveness, 1981, 0.05).
narrative_ontology:measurement(rfc9_be_t1995, rfc9293_tcp_specification__optimization_latitude_reading, base_extractiveness, 1995, 0.06).
narrative_ontology:measurement(rfc9_be_t2004, rfc9293_tcp_specification__optimization_latitude_reading, base_extractiveness, 2004, 0.07).
narrative_ontology:measurement(rfc9_be_t2012, rfc9293_tcp_specification__optimization_latitude_reading, base_extractiveness, 2012, 0.07).
narrative_ontology:measurement(rfc9_be_t2018, rfc9293_tcp_specification__optimization_latitude_reading, base_extractiveness, 2018, 0.08).
narrative_ontology:measurement(rfc9_be_t2024, rfc9293_tcp_specification__optimization_latitude_reading, base_extractiveness, 2024, 0.08).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(rfc9293_tcp_specification__optimization_latitude_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(rfc9293_tcp_specification__optimization_latitude_reading, information_standard).
narrative_ontology:boltzmann_floor_override(rfc9293_tcp_specification__optimization_latitude_reading, 0.03).
narrative_ontology:affects_constraint(rfc9293_tcp_specification__optimization_latitude_reading, rfc9293_tcp_specification__strict_invariance_reading).
narrative_ontology:affects_constraint(rfc9293_tcp_specification__optimization_latitude_reading, rfc9293_tcp_specification__middlebox_realism_reading).

% DUAL FORMULATION NOTE:
% This story is one of three sibling constraints decomposing the colloquial label 'the RFC 9293 constraint' per the epsilon-invariance principle. strict_invariance_reading treats the specification as an invariant state machine (higher suppression, lower latitude); middlebox_realism_reading treats deployed middlebox behavior as the operative authority superseding the text. This file's epsilon (0.08, Rope) is not averaged with or reconciled to the siblings' epsilon values — each reading is a structurally distinct constraint with its own beneficiary/victim structure, linked here for contamination-propagation and family-tracing purposes only.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
