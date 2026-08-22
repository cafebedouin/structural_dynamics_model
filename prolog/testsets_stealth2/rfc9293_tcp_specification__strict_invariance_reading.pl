% ============================================================================
% CONSTRAINT STORY: rfc9293_tcp_specification__strict_invariance_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-04
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_rfc9293_tcp_specification__strict_invariance_reading, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
 *   constraint_id: rfc9293_tcp_specification__strict_invariance_reading
 *   human_readable: RFC 9293 TCP Specification — Strict Invariance Reading (Exact State Machine Replication)
 *   domain: technological/standards_coordination
 *
 * SUMMARY:
 *   RFC 9293 consolidates the TCP specification into a single documented
 *   state machine: connection establishment and teardown, sequence numbering,
 *   retransmission, flow control, and exceptional-condition handling, written
 *   precisely enough that an implementation which replicates the state
 *   machine exactly interoperates with every other implementation everywhere.
 *   This story authors that demand as the strict invariance reading takes it:
 *   the text binds exactly, and on-path modification of TCP semantics is a
 *   violation of it. The standing arrangement the story is about contains
 *   endpoint implementations replicating the state machine; a standards
 *   process maintaining the text as the single reference; a deployed
 *   population of on-path appliances whose commercial functions inspect or
 *   alter TCP state and which the invariance demand places outside conformant
 *   behavior; and an application-and-user population relying on the
 *   guarantees the text makes. The arrangement transfers no resources between
 *   seats: its costs are mutual conformance burdens and its benefit —
 *   interoperability by construction — accrues to every participant and is
 *   collected by no one. Extractiveness for this referent is authored near
 *   the coordination floor; the one declared victim group is harmed not by
 *   the demand itself but by the gap between the demand and the deployed
 *   path.
 *
 * KEY AGENTS:
 *   - ietf_tcpm_working_group: agenda setter (institutional/constrained) — maintains the invariant text as the single reference and judges every extension against it
 *   - tcp_stack_implementers: primary beneficiary and compliance-cost bearer (powerful/constrained) — replicate the state machine exactly in exchange for universal interoperability
 *   - spec_guarantee_dependent_implementations: primary bearer of the gap between text and path (moderate/trapped) — build on documented guarantees that on-path deviation breaks
 *   - traffic_engineering_middlebox_operators: bound party (organized/constrained) — the invariance demand forbids functions its equipment exists to perform
 *   - internet_application_developers: beneficiary (moderate/constrained) — writes once against the documented byte stream and deploys everywhere
 *   - end_users_of_reliable_transport: diffuse beneficiary (powerless/constrained) — receives uniform transport behavior with no seat in the process
 *   - network_measurement_researchers: analytical observer (analytical/analytical) — documents divergence between the specified and the deployed
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(rfc9293_tcp_specification__strict_invariance_reading, 0.06).
domain_priors:suppression_score(rfc9293_tcp_specification__strict_invariance_reading, 0.18).
domain_priors:theater_ratio(rfc9293_tcp_specification__strict_invariance_reading, 0.04).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(rfc9293_tcp_specification__strict_invariance_reading, extractiveness, 0.06).
narrative_ontology:constraint_metric(rfc9293_tcp_specification__strict_invariance_reading, suppression_requirement, 0.18).
narrative_ontology:constraint_metric(rfc9293_tcp_specification__strict_invariance_reading, theater_ratio, 0.04).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(rfc9293_tcp_specification__strict_invariance_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(rfc9293_tcp_specification__strict_invariance_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(rfc9293_tcp_specification__strict_invariance_reading, rope).
narrative_ontology:human_readable(rfc9293_tcp_specification__strict_invariance_reading, "RFC 9293 TCP Specification — Strict Invariance Reading (Exact State Machine Replication)").
narrative_ontology:topic_domain(rfc9293_tcp_specification__strict_invariance_reading, "technological/standards_coordination").

domain_priors:requires_active_enforcement(rfc9293_tcp_specification__strict_invariance_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(rfc9293_tcp_specification__strict_invariance_reading, '4e799ae9-4b5c-4f8d-9e50-e00ae30fdf72').
narrative_ontology:cs_kernel_codification('4e799ae9-4b5c-4f8d-9e50-e00ae30fdf72', fixed_text).
narrative_ontology:cs_authority_grounding('4e799ae9-4b5c-4f8d-9e50-e00ae30fdf72', expertise).
narrative_ontology:cs_interpretation_layer_present('4e799ae9-4b5c-4f8d-9e50-e00ae30fdf72').
narrative_ontology:cs_reading_relation('4e799ae9-4b5c-4f8d-9e50-e00ae30fdf72', rfc9293_tcp_specification__optimization_latitude_reading, influences).
narrative_ontology:cs_reading_relation('4e799ae9-4b5c-4f8d-9e50-e00ae30fdf72', rfc9293_tcp_specification__middlebox_realism_reading, coexists_with).
narrative_ontology:cs_axiom('4e799ae9-4b5c-4f8d-9e50-e00ae30fdf72', foundational, state_machine_text_is_binding_specification).
narrative_ontology:cs_axiom_status(state_machine_text_is_binding_specification, holdable).
narrative_ontology:cs_axiom_grounding('4e799ae9-4b5c-4f8d-9e50-e00ae30fdf72', state_machine_text_is_binding_specification, empirically_contingent).
narrative_ontology:cs_axiom('4e799ae9-4b5c-4f8d-9e50-e00ae30fdf72', secondary, path_semantic_modification_is_violation).
narrative_ontology:cs_axiom_status(path_semantic_modification_is_violation, holdable).
narrative_ontology:cs_axiom_grounding('4e799ae9-4b5c-4f8d-9e50-e00ae30fdf72', path_semantic_modification_is_violation, empirically_contingent).
narrative_ontology:cs_reference_frame('4e799ae9-4b5c-4f8d-9e50-e00ae30fdf72', exact_state_machine_replication).
narrative_ontology:cs_drift_state('4e799ae9-4b5c-4f8d-9e50-e00ae30fdf72', contemporary_deployed_internet, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('4e799ae9-4b5c-4f8d-9e50-e00ae30fdf72', '').
narrative_ontology:cs_kernel_id(rfc9293_tcp_specification__strict_invariance_reading, rfc9293_tcp_specification).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(rfc9293_tcp_specification__strict_invariance_reading, tcp_stack_implementers).
narrative_ontology:constraint_beneficiary(rfc9293_tcp_specification__strict_invariance_reading, internet_application_developers).
narrative_ontology:constraint_beneficiary(rfc9293_tcp_specification__strict_invariance_reading, end_users_of_reliable_transport).
narrative_ontology:constraint_victim(rfc9293_tcp_specification__strict_invariance_reading, spec_guarantee_dependent_implementations).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(rfc9293_tcp_specification__strict_invariance_reading, tcp_stack_implementers).
narrative_ontology:constraint_victim(rfc9293_tcp_specification__strict_invariance_reading, traffic_engineering_middlebox_operators).
narrative_ontology:constraint_vindicates(rfc9293_tcp_specification__strict_invariance_reading, exact_replication_preserves_interoperability).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintains the specification text: processes errata, judges proposed extensions against the documented state machine, and publishes revisions such as the 2022 consolidation of RFC 793 into RFC 9293. Its standing depends on the text remaining the single reference every implementer cites; it can revise behaviors through standards action but cannot abandon the shared state machine without dissolving its own authority. Its costs are process maintenance; its return is a transport layer that behaves as written.
narrative_ontology:constraint_stakeholder(rfc9293_tcp_specification__strict_invariance_reading, ietf_tcpm_working_group, agenda_setter,
    institutional, generational, constrained, global).

% Write and maintain kernel and library TCP implementations that replicate the documented state machine exactly. They receive universal interoperability with every other conformant stack and with the deployed application base. They bear the cost of exact replication, including legacy behaviors kept for wire compatibility and security mechanisms layered onto the original text, and they spend heavily diagnosing failures caused by path elements that do not preserve TCP state. Leaving the arrangement means abandoning the installed base their products exist to serve.
narrative_ontology:constraint_stakeholder(rfc9293_tcp_specification__strict_invariance_reading, tcp_stack_implementers, beneficiary,
    powerful, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(rfc9293_tcp_specification__strict_invariance_reading, tcp_stack_implementers, payer).

% Build features that assume the documented state machine holds on every path: TCP Fast Open, multipath operation, explicit congestion notification, deterministic connection teardown, latency-sensitive timing assumptions. When a path element strips options, injects resets, or rewrites sequence state, their features fail nondeterministically and they have no way to see or repair the intervening hop. Their exit is abandoning the feature or tunneling around the path, both of which surrender the guarantee they built on.
narrative_ontology:constraint_stakeholder(rfc9293_tcp_specification__strict_invariance_reading, spec_guarantee_dependent_implementations, payer,
    moderate, biographical, trapped, global).

% Operate network appliances — address translators, firewalls, load balancers, inspection systems — whose commercial value lies in inspecting or altering traffic on path. The invariance demand forbids exactly the functions much of this equipment performs, and some deployed behavior rewrites or resets TCP state that endpoints rely on. The functions cannot be relocated off path, and the equipment is embedded in revenue-bearing networks, so the prohibition binds where these operators run.
narrative_ontology:constraint_stakeholder(rfc9293_tcp_specification__strict_invariance_reading, traffic_engineering_middlebox_operators, payer,
    organized, biographical, constrained, global).

% Write applications against the documented reliable byte stream and deploy them to the whole internet without per-destination adaptation. They receive write-once portability across networks and stacks. They bear cost when path behavior diverges from the specification: defensive retransmission logic, disabled features, and support burden for failures they cannot reproduce.
narrative_ontology:constraint_stakeholder(rfc9293_tcp_specification__strict_invariance_reading, internet_application_developers, beneficiary,
    moderate, biographical, constrained, global).

% Use applications that depend on connections working identically on every network. They receive uniform transport behavior without configuring or testing anything. They cannot observe protocol state and have no channel into the standards process; when a path breaks a transport guarantee, they experience it as an application failure — a stalled upload, a dropped session — with no recourse beyond retrying or switching networks.
narrative_ontology:constraint_stakeholder(rfc9293_tcp_specification__strict_invariance_reading, end_users_of_reliable_transport, beneficiary,
    powerless, immediate, constrained, global).

% Measure deployed path behavior against the specification: option stripping rates, reset injection, state rewriting. They publish divergence measurements that are the main external check on how the documented state machine relates to the network as it operates. They collect nothing from the arrangement and bear none of its costs; their standing depends on neither the specification nor the appliance industry prevailing.
narrative_ontology:constraint_stakeholder(rfc9293_tcp_specification__strict_invariance_reading, network_measurement_researchers, observer,
    analytical, biographical, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(rfc9293_tcp_specification__strict_invariance_reading, diffuse).
narrative_ontology:fixing_cost_class(rfc9293_tcp_specification__strict_invariance_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the problem of reliable byte-stream transport between arbitrary hosts that have never been introduced: by fixing one exact state machine that every implementation replicates, it removes the need for per-pair negotiation, capability testing, or per-path adaptation before any two stacks can communicate.
% TRANSFER_FUNCTION: Transfers no money, work, attention, or status between parties. It allocates obligation: every implementer and every path element owes exact conformance to the shared state machine, and receives in exchange interoperability with all others. The compliance burden is mutual and nothing is collected — no seat receives what any seat pays.
% ABSENT_VOICES: End users would object to silent breakage of the guarantees they implicitly rely on but have no seat; they are represented only through vendors. Small and embedded-stack implementers without IETF participation inherit exact-replication burdens they cannot influence. Appliance operators are commercially present but their preference — latitude to modify path behavior — has no standing in a process whose premise is the invariant text. None of these seats can currently force the specification to reconsider the invariance demand itself.
% DISAPPEARANCE_RATIONALE: If the requirement that implementations replicate the specified state machine vanished overnight, the property that any two stacks interoperate by construction would dissolve. Each implementation would drift under its own performance and compatibility pressures, every communicating pair would need discovery or negotiation before reliable transfer, and the application base built on uniform TCP semantics would fail at every divergent boundary. Reassembling the interoperability the invariant provides would mean re-coordinating a globally deployed base — a rearrangement measured in decades.
% FOUNDING_PROBLEM: Early packet networks had no shared reliable transport: loss, reordering, duplication, and flow control had to be handled per host pair, and every new stack had to be tested against every peer. The specification was built to end this by fixing one exact state machine — a single written artifact every implementation would replicate, so reliability became a solved, shared property rather than a per-pair negotiation.
% FOUNDING_PROBLEM_CORROBORATION: The working group and the implementer community attest the founding problem from inside the beneficiary set. Outside it: the network measurement literature documents that divergence from the documented state machine — option stripping, reset injection, state rewriting — correlates with connection failure and broken application features; operator operational experience treats middlebox-induced transport failure as a routine diagnostic category; and transports designed since, QUIC among them, again specified an exact state machine rather than relying on per-path latitude, which is behavioral corroboration that the founding problem — non-interoperable transport fails — remains live.
narrative_ontology:disappearance_verdict(rfc9293_tcp_specification__strict_invariance_reading, world_rearranges).
narrative_ontology:founding_problem_status(rfc9293_tcp_specification__strict_invariance_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(rfc9293_tcp_specification__strict_invariance_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(rfc9293_tcp_specification__strict_invariance_reading, 'none', 1).
narrative_ontology:epsilon_provenance(rfc9293_tcp_specification__strict_invariance_reading, 0.06, 'stealth/ox-alpha', 'none', direct).

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
 *   Extractiveness is authored at 0.06, just above the information_standard coordination floor of 0.02: the arrangement transfers nothing between seats, and the excess over the floor is the legacy-accretion tax — mandated obsolete behaviors and security mechanisms layered onto the original text that implementers must replicate exactly without receiving current function for them. Suppression is 0.18: the constraint coerces nothing; its only sanction is interoperability failure, which is the coordination mechanism itself rather than an externally applied penalty. Theater is 0.04: the specification's function is performed continuously and verifiably — conformance is testable, interoperability is observable — with a small ceremonial residue in checkbox implementation of dead features. Accessibility collapse is 0.58: alternative transports exist at the ecosystem level, but none delivers interoperability with the deployed base, so once the requirement is understood the practical alternative set collapses to exact conformance. Resistance is 0.40: the on-path appliance population's commercial functions are what the invariance demand forbids, and its deviation is persistent and large-scale; endpoint implementers by contrast mostly comply. All three measurement series share one grid. Base extractiveness creeps upward with specification accretion between RFC 793 and RFC 9293. The suppression_requirement series is authored because this story specifically tracks enforcement capacity: a small early community could find and remediate deviating implementations; the appliance boom outran any enforcement reach; capacity partially reconstituted in the measurement era as path behavior became documentable and encrypted transports changed the enforcement landscape — an enforcement-decay-then-partial-rebuild trajectory, not a ratchet. The claimed type (rope) is authored from structure — mutual obligation, no transfer, no capturing seat — independently of these metric values.
 *
 * PERSPECTIVAL GAP:
 *   The same text is a different arrangement from different seats. From the working group's seat the invariant is the artifact that makes the network possible and its maintenance is stewardship. From the guarantee-dependent implementer's seat the invariant is a promise the deployed path breaks — the text protects them on paper while the path harms them in fact, so their seat computes high directionality against a near-zero-extractiveness constraint. From the appliance operator's seat the invariant is a prohibition aimed at its product's core function. Same-level divergence: TCP stack implementers and guarantee-dependent implementers are both implementers at adjacent power levels, but the OS vendors hold fallback levers — bundled stacks, feature negotiation, direct vendor relationships with network operators — while the guarantee-dependent seat is trapped behind path behavior it cannot observe or negotiate with. Identical nominal activity, different exit, different computed seat.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations (stack implementers, application developers, end users) derive low directionality — the constraint subsidizes them with interoperability by construction. The victim declaration (spec_guarantee_dependent_implementations) derives high directionality: in the standing arrangement they bear the costs of the invariant being violated on path, and their exit is trapped behind path behavior they cannot see. The appliance operator seat is the deliberate override: it bears the prohibition's binding — its equipment's functions are what the demand forbids — which the beneficiary/victim arrays do not express, because under this reading nothing it forgoes is collected by anyone; d is overridden to 0.75 to seat that binding honestly, while the near-floor epsilon keeps its effective extraction low. The working group sits near symmetric: it maintains at cost and receives the standard's functioning. Gain flow is authored as diffuse after checking every seat: no seat captures what the arrangement produces or what compliance costs; the interoperability surplus is mutual and the appliance operators' foregone functions accrue to no one.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — per-pair transport negotiation in a multi-vendor network — remains live and is solved by the arrangement, so the mandate has not outlived its function and no mandatrophy is declared; the R5 cell (live status, world_rearranges verdict) is the healthy one. The classification work this story performs is boundary-keeping in both directions. Against mislabeling as pure extraction: the conformance burden is real and asymmetric in experience — the trapped guarantee-dependent seat, the bound appliance seat — but it is obligation, not transfer; no seat receives what any seat pays, which is what keeps this a coordination structure rather than a capture structure even with a declared victim. Against mislabeling as natural law: the invariant feels like a law of the internet ('TCP is TCP'), but it is a written, maintained, and revisable artifact — RFC 793 was revised into RFC 9293 through standards action — so it is authored as constructed coordination with emerges_naturally false. The receipt surface lands in the prohibitive-fixing/diffuse-gain cell, which is piton-adjacent; this constraint is not a piton because its function is emphatically live — theater_ratio 0.04, coordination performed on every connection — and the prohibitive fixing cost reflects the value of what the invariant holds together (a globally deployed base) rather than atrophy of what it does.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'This story instantiates the strict_invariance_reading of the rfc9293_tcp_specification kernel: the specification text binds as an exact, invariant state machine, and path modification of TCP semantics is a violation. What would the sibling readings change structurally, and where is the disagreement located?',
    'Framing commitment rather than data: the optimization latitude reading treats the text as outcome-specifying and admits implementation latitude within semantic bounds, shrinking the conformance-burden cost toward the coordination floor and narrowing the victim set to semantic-boundary crossings; the middlebox realism reading subordinates specification authority to deployed path behavior, relocating the violation claim from the path to the text itself. The disagreement is located in the normative force of the specification text — binding exact replication versus outcome sketch. Empirical evidence (how often latitude-taking breaks interoperability versus improves performance) informs but cannot settle the framing question.',
    'Adopting a sibling reading changes epsilon and the victim set: latitude shrinks the exact-replication burden toward the coordination floor; realism reassigns victim status from guarantee-reliant implementations to specification-conformant ones and re-sites the arrangement''s costs in the specification''s authority claim. This story''s epsilon is authored only for the strict reading''s referent.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Committer structure: one reading of the TCP specification kernel; sibling readings would relocate epsilon and the victim set.').

omega_variable(
    path_invariant_enforceability,
    'Is the invariance demand enforceable against on-path elements at all, or does endpoint conformance exhaust its enforceable scope?',
    'Measurement: quantify path interference rates over time and observe whether encrypted transports, which remove path visibility, reduce interference or merely displace it to unencrypted flows; any conformance regime that successfully binds path elements would demonstrate enforceability.',
    'If path-side enforcement is structurally unavailable, the constraint operates as two constraints — a fully enforced endpoint invariant and an unenforced path invariant — and the guarantee-reliant victim seat is harmed by precisely the unenforced half; per-seat classifications diverge accordingly.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(path_invariant_enforceability, empirical, 'Whether the no-modification demand binds path elements in practice or only on paper.').

omega_variable(
    legacy_accretion_burden,
    'How much of the exact-replication burden serves current function versus legacy wire-image compatibility — mandated obsolete behaviors, retained header fields, security mechanisms layered onto the original text?',
    'Implementation cost analysis separating conformance effort for legacy behaviors from effort for current-function behaviors; historical tracking of specification accretion between RFC 793 and RFC 9293.',
    'If a large share of the burden is legacy-only, base extractiveness is understated and the implementer seat drifts toward a hybrid coordination/extraction reading; if the burden is mostly current-function, the near-floor epsilon stands.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legacy_accretion_burden, empirical, 'Whether the growing conformance burden is coordination cost or accreted legacy tax.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(rfc9293_tcp_specification__strict_invariance_reading, 0, 41).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tcp_strict_invariance_tr_t0, rfc9293_tcp_specification__strict_invariance_reading, theater_ratio, 0, 0.02).
narrative_ontology:measurement_basis(tcp_strict_invariance_tr_t0, observed).
narrative_ontology:measurement(tcp_strict_invariance_tr_t8, rfc9293_tcp_specification__strict_invariance_reading, theater_ratio, 8, 0.02).
narrative_ontology:measurement_basis(tcp_strict_invariance_tr_t8, observed).
narrative_ontology:measurement(tcp_strict_invariance_tr_t16, rfc9293_tcp_specification__strict_invariance_reading, theater_ratio, 16, 0.03).
narrative_ontology:measurement_basis(tcp_strict_invariance_tr_t16, observed).
narrative_ontology:measurement(tcp_strict_invariance_tr_t24, rfc9293_tcp_specification__strict_invariance_reading, theater_ratio, 24, 0.03).
narrative_ontology:measurement_basis(tcp_strict_invariance_tr_t24, observed).
narrative_ontology:measurement(tcp_strict_invariance_tr_t32, rfc9293_tcp_specification__strict_invariance_reading, theater_ratio, 32, 0.04).
narrative_ontology:measurement_basis(tcp_strict_invariance_tr_t32, observed).
narrative_ontology:measurement(tcp_strict_invariance_tr_t41, rfc9293_tcp_specification__strict_invariance_reading, theater_ratio, 41, 0.04).
narrative_ontology:measurement_basis(tcp_strict_invariance_tr_t41, observed).

% Extraction over time
narrative_ontology:measurement(tcp_strict_invariance_be_t0, rfc9293_tcp_specification__strict_invariance_reading, base_extractiveness, 0, 0.03).
narrative_ontology:measurement_basis(tcp_strict_invariance_be_t0, observed).
narrative_ontology:measurement(tcp_strict_invariance_be_t8, rfc9293_tcp_specification__strict_invariance_reading, base_extractiveness, 8, 0.04).
narrative_ontology:measurement_basis(tcp_strict_invariance_be_t8, observed).
narrative_ontology:measurement(tcp_strict_invariance_be_t16, rfc9293_tcp_specification__strict_invariance_reading, base_extractiveness, 16, 0.05).
narrative_ontology:measurement_basis(tcp_strict_invariance_be_t16, observed).
narrative_ontology:measurement(tcp_strict_invariance_be_t24, rfc9293_tcp_specification__strict_invariance_reading, base_extractiveness, 24, 0.05).
narrative_ontology:measurement_basis(tcp_strict_invariance_be_t24, observed).
narrative_ontology:measurement(tcp_strict_invariance_be_t32, rfc9293_tcp_specification__strict_invariance_reading, base_extractiveness, 32, 0.06).
narrative_ontology:measurement_basis(tcp_strict_invariance_be_t32, observed).
narrative_ontology:measurement(tcp_strict_invariance_be_t41, rfc9293_tcp_specification__strict_invariance_reading, base_extractiveness, 41, 0.06).
narrative_ontology:measurement_basis(tcp_strict_invariance_be_t41, observed).

% Suppression requirement over time
narrative_ontology:measurement(tcp_strict_invariance_su_t0, rfc9293_tcp_specification__strict_invariance_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement_basis(tcp_strict_invariance_su_t0, observed).
narrative_ontology:measurement(tcp_strict_invariance_su_t8, rfc9293_tcp_specification__strict_invariance_reading, suppression_requirement, 8, 0.28).
narrative_ontology:measurement_basis(tcp_strict_invariance_su_t8, observed).
narrative_ontology:measurement(tcp_strict_invariance_su_t16, rfc9293_tcp_specification__strict_invariance_reading, suppression_requirement, 16, 0.22).
narrative_ontology:measurement_basis(tcp_strict_invariance_su_t16, observed).
narrative_ontology:measurement(tcp_strict_invariance_su_t24, rfc9293_tcp_specification__strict_invariance_reading, suppression_requirement, 24, 0.16).
narrative_ontology:measurement_basis(tcp_strict_invariance_su_t24, observed).
narrative_ontology:measurement(tcp_strict_invariance_su_t32, rfc9293_tcp_specification__strict_invariance_reading, suppression_requirement, 32, 0.14).
narrative_ontology:measurement_basis(tcp_strict_invariance_su_t32, observed).
narrative_ontology:measurement(tcp_strict_invariance_su_t41, rfc9293_tcp_specification__strict_invariance_reading, suppression_requirement, 41, 0.18).
narrative_ontology:measurement_basis(tcp_strict_invariance_su_t41, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(rfc9293_tcp_specification__strict_invariance_reading, information_standard).
narrative_ontology:affects_constraint(rfc9293_tcp_specification__strict_invariance_reading, rfc9293_tcp_specification__optimization_latitude_reading).
narrative_ontology:affects_constraint(rfc9293_tcp_specification__strict_invariance_reading, rfc9293_tcp_specification__middlebox_realism_reading).

% DUAL FORMULATION NOTE:
% The natural-language label 'the TCP specification' covers three structurally distinct claims about the same text, decomposed into a three-story constraint family per the epsilon-invariance principle: (1) this story — the text binds as an exact invariant state machine, epsilon near the coordination floor, victim set limited to implementations relying on strict guarantees; (2) the optimization latitude reading — the text specifies outcomes and permits latitude within semantic bounds, different conformance-burden structure and narrower victim set; (3) the middlebox realism reading — deployed path behavior is the operative protocol, relocating the arrangement's costs into the specification's authority claim itself. Each member carries its own epsilon, beneficiaries, and victims; the upstream member (this reading, highest empirical confidence in the text's binding force) influences the latitude sibling through conformance infrastructure and coexists with the realism sibling as rival authority accounts held by different communities.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(rfc9293_tcp_specification__strict_invariance_reading, organized, 0.75).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
