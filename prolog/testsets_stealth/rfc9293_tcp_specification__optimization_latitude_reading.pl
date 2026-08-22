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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
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
 *   constraint_id: rfc9293_tcp_specification__optimization_latitude_reading
 *   human_readable: RFC 9293 TCP Outcome Contract with Implementation Latitude
 *   domain: technological/coordination
 *
 * SUMMARY:
 *   Under this reading, RFC 9293 is an outcome contract: it binds
 *   implementations to what peers can observe — connection semantics,
 *   reliable in-order delivery, congestion-safe sending — while explicitly
 *   leaving internal algorithms, buffering, and performance mechanisms to
 *   implementer choice. That division of normative labor is the constraint's
 *   content: a new congestion-control algorithm can ship in one stack and
 *   interoperate with every conformant peer, which is exactly the operating
 *   condition under which DCTCP entered datacenters and BBR entered
 *   production. The arrangement's parties all net-benefit: implementers trade
 *   conformance labor for universal reach, applications get transport they
 *   never build, operators get endpoint discipline, algorithm designers get a
 *   deployable canvas, and end users get working communication they never
 *   think about. This story is one member of a linked constraint family
 *   decomposing the TCP-specification kernel; the family relationship and the
 *   sibling readings are recorded in commentary.kernel_context and the omega
 *   variables, not adjudicated here. Claim and metrics are independent
 *   authored facts: the rope claim reflects the structure (a genuine
 *   collective-action problem, net-benefiting participants, unsuppressed
 *   alternatives), and the metrics describe observed operation without being
 *   tuned to any predicted engine output.
 *
 * KEY AGENTS:
 *   - ietf_tcp_working_group: agenda-setter (institutional/constrained) — authors and maintains the outcome contract through consensus; can revise the text, cannot command deployed stacks
 *   - tcp_stack_implementers: primary participant-beneficiary bearing compliance costs (powerful/mobile) — builds the stacks, chooses the algorithms, could build userspace alternatives
 *   - application_developers: pure beneficiary (moderate/constrained) — consumes the byte-stream abstraction without conformance labor
 *   - network_operators: beneficiary bearing optimization externalities (organized/constrained) — gains endpoint discipline, absorbs queueing and fairness costs
 *   - internet_end_users: passive beneficiary (powerless/constrained) — receives the arrangement's output with no seat in its administration
 *   - congestion_control_designers: beneficiary through latitude (moderate/mobile) — the implementation latitude is their deployable design space
 *   - interoperability_test_labs: analytical observer — measures conformance and divergence, adjudicates nothing, collects nothing from the arrangement's operation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(rfc9293_tcp_specification__optimization_latitude_reading, 0.08).
domain_priors:suppression_score(rfc9293_tcp_specification__optimization_latitude_reading, 0.05).
domain_priors:theater_ratio(rfc9293_tcp_specification__optimization_latitude_reading, 0.08).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(rfc9293_tcp_specification__optimization_latitude_reading, extractiveness, 0.08).
narrative_ontology:constraint_metric(rfc9293_tcp_specification__optimization_latitude_reading, suppression_requirement, 0.05).
narrative_ontology:constraint_metric(rfc9293_tcp_specification__optimization_latitude_reading, theater_ratio, 0.08).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(rfc9293_tcp_specification__optimization_latitude_reading, accessibility_collapse, 0.12).
narrative_ontology:constraint_metric(rfc9293_tcp_specification__optimization_latitude_reading, resistance, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(rfc9293_tcp_specification__optimization_latitude_reading, rope).
narrative_ontology:human_readable(rfc9293_tcp_specification__optimization_latitude_reading, "RFC 9293 TCP Outcome Contract with Implementation Latitude").
narrative_ontology:topic_domain(rfc9293_tcp_specification__optimization_latitude_reading, "technological/coordination").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(rfc9293_tcp_specification__optimization_latitude_reading, '89e248ab-71ba-4083-908b-c88dd75a4f6f').
narrative_ontology:cs_kernel_codification('89e248ab-71ba-4083-908b-c88dd75a4f6f', fixed_text).
narrative_ontology:cs_authority_grounding('89e248ab-71ba-4083-908b-c88dd75a4f6f', expertise).
narrative_ontology:cs_interpretation_layer_present('89e248ab-71ba-4083-908b-c88dd75a4f6f').
narrative_ontology:cs_reading_relation('89e248ab-71ba-4083-908b-c88dd75a4f6f', rfc9293_tcp_specification__strict_invariance_reading, coexists_with).
narrative_ontology:cs_reading_relation('89e248ab-71ba-4083-908b-c88dd75a4f6f', rfc9293_tcp_specification__middlebox_realism_reading, influences).
narrative_ontology:cs_axiom('89e248ab-71ba-4083-908b-c88dd75a4f6f', foundational, outcome_contract_implementation_latitude).
narrative_ontology:cs_axiom_status(outcome_contract_implementation_latitude, holdable).
narrative_ontology:cs_axiom_grounding('89e248ab-71ba-4083-908b-c88dd75a4f6f', outcome_contract_implementation_latitude, conventional).
narrative_ontology:cs_axiom('89e248ab-71ba-4083-908b-c88dd75a4f6f', secondary, latitude_serves_interoperability).
narrative_ontology:cs_axiom_status(latitude_serves_interoperability, holdable).
narrative_ontology:cs_axiom_grounding('89e248ab-71ba-4083-908b-c88dd75a4f6f', latitude_serves_interoperability, instrumental).
narrative_ontology:cs_reference_frame('89e248ab-71ba-4083-908b-c88dd75a4f6f', semantic_outcome_contract).
narrative_ontology:cs_drift_state('89e248ab-71ba-4083-908b-c88dd75a4f6f', post_bbr_deployment_era, gap(practice_drift, minor, true)).
narrative_ontology:cs_created_at('89e248ab-71ba-4083-908b-c88dd75a4f6f', '').
narrative_ontology:cs_kernel_id(rfc9293_tcp_specification__optimization_latitude_reading, rfc9293_tcp_specification).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(rfc9293_tcp_specification__optimization_latitude_reading, tcp_stack_implementers).
narrative_ontology:constraint_beneficiary(rfc9293_tcp_specification__optimization_latitude_reading, application_developers).
narrative_ontology:constraint_beneficiary(rfc9293_tcp_specification__optimization_latitude_reading, network_operators).
narrative_ontology:constraint_beneficiary(rfc9293_tcp_specification__optimization_latitude_reading, internet_end_users).
narrative_ontology:constraint_beneficiary(rfc9293_tcp_specification__optimization_latitude_reading, congestion_control_designers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(rfc9293_tcp_specification__optimization_latitude_reading, tcp_stack_implementers).
narrative_ontology:constraint_victim(rfc9293_tcp_specification__optimization_latitude_reading, network_operators).
narrative_ontology:constraint_vindicates(rfc9293_tcp_specification__optimization_latitude_reading, end_to_end_principle).
narrative_ontology:constraint_vindicates(rfc9293_tcp_specification__optimization_latitude_reading, outcome_specification_sufficiency).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Authors and maintains the TCP specification through the IETF's working-group and consensus process: decides which behaviors are normative requirements, which are recommendations, and which are left to implementer choice, and consolidates decades of updates from RFC 793 through RFC 9293. Its members operate the specification's normative keyword structure; the group can revise the text but cannot dictate what deployed implementations do, and its standing rests on demonstrated engineering competence and open participation rather than mandate.
narrative_ontology:constraint_stakeholder(rfc9293_tcp_specification__optimization_latitude_reading, ietf_tcp_working_group, agenda_setter,
    institutional, generational, constrained, global).

% Operating-system and kernel teams (Linux, Windows, BSD, embedded vendors) implement the connection semantics and ship the world's TCP stacks. They spend engineering effort conforming to the behavioral contract — state handling, reliable reassembly, congestion-safe sending — and in exchange every conformant peer on the internet can talk to their stacks. Inside the contract they choose their own algorithms and tune aggressively; outside it they can build userspace transports (as the QUIC effort demonstrated), though the installed base of peers pulls them back toward conformance.
narrative_ontology:constraint_stakeholder(rfc9293_tcp_specification__optimization_latitude_reading, tcp_stack_implementers, beneficiary,
    powerful, generational, mobile, global).
narrative_ontology:stakeholder_secondary_role(rfc9293_tcp_specification__optimization_latitude_reading, tcp_stack_implementers, payer).

% Build on the reliable, ordered byte-stream abstraction without implementing transport themselves; the shared behavioral contract is what lets one codebase talk to every peer operating system. Their choice set is bounded by the sockets-style APIs their platforms expose, though userspace transports are widening it. They bear no conformance labor of their own.
narrative_ontology:constraint_stakeholder(rfc9293_tcp_specification__optimization_latitude_reading, application_developers, beneficiary,
    moderate, biographical, constrained, global).

% Carry TCP traffic at scale and rely on endpoints' shared congestion discipline to keep queues manageable and flows shareable. They gain predictable endpoint behavior; they also absorb the costs when optimizations push burden into the network — deep buffers and latency inflation, fairness disputes between algorithm generations — and their main levers are queue management and traffic shaping rather than leaving.
narrative_ontology:constraint_stakeholder(rfc9293_tcp_specification__optimization_latitude_reading, network_operators, beneficiary,
    organized, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(rfc9293_tcp_specification__optimization_latitude_reading, network_operators, payer).

% Get working communication across every network and device without knowing the transport exists. They pay nothing directly and choose nothing directly; their experience of the arrangement is that connections work, and occasionally that they are slow when optimization choices push latency into shared queues.
narrative_ontology:constraint_stakeholder(rfc9293_tcp_specification__optimization_latitude_reading, internet_end_users, beneficiary,
    powerless, biographical, constrained, global).

% Researchers and engineers who design senders — Reno, CUBIC, DCTCP, BBR and successors. The specification's choice to bind outcomes rather than algorithms is what makes their work deployable at all: a new algorithm can ship in one stack and interoperate with every conformant peer. Their exit is genuinely open — they publish, prototype, and can carry designs to other transport efforts.
narrative_ontology:constraint_stakeholder(rfc9293_tcp_specification__optimization_latitude_reading, congestion_control_designers, beneficiary,
    moderate, biographical, mobile, global).

% Independent laboratories that run conformance suites and cross-vendor interoperation events, documenting where stacks diverge from the behavioral contract and where divergence is harmless. They are paid by their customers either way and adjudicate nothing; their measurements are the shared evidence base every seat cites.
narrative_ontology:constraint_stakeholder(rfc9293_tcp_specification__optimization_latitude_reading, interoperability_test_labs, observer,
    moderate, biographical, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(rfc9293_tcp_specification__optimization_latitude_reading, diffuse).
narrative_ontology:fixing_cost_class(rfc9293_tcp_specification__optimization_latitude_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the interoperability collective-action problem: independently developed TCP stacks worldwide interoperate because all conform to the same observable behavioral contract — connection establishment and teardown semantics, reliable in-order byte-stream delivery, congestion-safe sending — while each optimizes internal algorithms, buffering, and pacing freely inside those bounds.
% TRANSFER_FUNCTION: Moves conformance labor and design discipline from each implementing stack into a shared behavioral contract, and returns universal interoperability and a stable application platform to every participant; optimization externalities (queueing burden, fairness disputes) move from sender choices onto network operators and coexisting flows. No monetary transfer occurs.
% ABSENT_VOICES: End users and small-stack implementers are underrepresented when the community adjudicates where the semantic bounds sit — the bounds debates are dominated by large-stack maintainers and major platform operators. Operators who absorb optimization externalities participate through industry bodies but rarely as equals of the big sender designers. No party is formally barred; the process is open, and the asymmetry is in attention and standing.
% DISAPPEARANCE_RATIONALE: If the outcome contract vanished overnight, independently developed stacks could no longer assume common connection semantics; every operating-system pair would need bespoke negotiation or a successor standard before traffic flowed, the application layer's reliable-byte-stream assumption would break until a replacement contract deployed, and the loss of shared congestion discipline would reopen the door to congestion collapse. The internet's transport layer would reorganize around whatever successor arrangement emerged.
% FOUNDING_PROBLEM: Hosts from different manufacturers needed a common host-to-host protocol providing reliable, ordered byte streams over a lossy packet network (RFC 793, 1981); early operational experience — above all the 1986 congestion collapse — added the requirement that endpoints share the burden of congestion control rather than pushing it onto the network.
% FOUNDING_PROBLEM_CORROBORATION: Interoperability test laboratories and the academic network-measurement literature corroborate the founding problem from outside the beneficiary set: measurement studies continue to document congestion-collapse episodes and flow-unfairness failures when endpoint discipline lapses or new sender designs misbehave. No party disputes that the problem is real; contest exists only over where the semantic bounds of sender responsibility sit.
narrative_ontology:disappearance_verdict(rfc9293_tcp_specification__optimization_latitude_reading, world_rearranges).
narrative_ontology:founding_problem_status(rfc9293_tcp_specification__optimization_latitude_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(rfc9293_tcp_specification__optimization_latitude_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(rfc9293_tcp_specification__optimization_latitude_reading, 'none', 1).
narrative_ontology:epsilon_provenance(rfc9293_tcp_specification__optimization_latitude_reading, 0.08, 'stealth/ox-alpha', 'none', direct).

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
 *   Extractiveness is low (0.08) because the contract takes only conformance labor, and that labor is repaid in interoperability to the same parties who spend it — there is no seat the arrangement systematically takes from and gives to another. Suppression is very low (0.05): alternatives (UDP, userspace transports, private datacenter protocols) are not barred by this arrangement, and under this reading their openness is the point. Theater ratio is low (0.08): interoperation events, conformance suites, and the specification's own maintenance test and exercise real behavior; the small residual is ritual in standards-process ceremony. Accessibility collapse is low (0.12): understanding the contract does not foreclose substitutes — workable alternatives exist at moderate friction, which is precisely what the QUIC migration demonstrated. Resistance is low (0.10): disputes concentrate at the boundary of the latitude (algorithm-generation fairness, loss-response obligations), not against the contract itself. Suppression is authored as a raw structural property, unscaled; only extractiveness is scaled by the engine through directionality and scope. The measurement series run on one shared six-point grid (1981–2025) so both tracked metrics are authored at every examined time point; suppression_requirement series are deliberately omitted because the enforcement picture is static and self-enforcing (non-conformant stacks lose peers), a picture already carried by the scalar suppression value.
 *
 * PERSPECTIVAL GAP:
 *   Seat divergence here is mild because every seat nets benefit — the divergence shows up in cost incidence, not in experienced type. Implementers experience the arrangement as conformance labor plus reach (their secondary payer role lifts their directionality slightly above the pure-beneficiary floor); operators experience endpoint discipline as a gain but optimization externalities as a cost they did not choose; end users experience only the benefit and hold no voice in bounds adjudication; the working group experiences authority plus the maintenance burden of adjudicating boundary disputes. The sharper perspectival gaps for this kernel live in the sibling stories' seats, where the same text reads as an invariant machine to replicate or as a description subordinate to deployed-path behavior; those divergences are computed by the engine from the siblings' structural data, not asserted in this file.
 *
 * DIRECTIONALITY LOGIC:
 *   Every declared party is a net beneficiary, so derived directionality sits near the beneficiary end across seats. The secondary payer declarations on tcp_stack_implementers and network_operators lift those seats modestly above the floor — they bear real costs (conformance labor; externalized queueing burden) alongside their gains. Exit options differentiate the rest: congestion_control_designers and tcp_stack_implementers hold mobile exit (arbitrage-grade alternatives exist and have been exercised), which pushes them toward the beneficiary end; internet_end_users and application_developers are constrained but unharmed. No directionality overrides are needed: the derivation from declared roles plus exit options matches the structural read, so the override chain is left untouched.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem is live: heterogeneous hosts over lossy networks still need exactly this contract, and the measurement record shows the failure mode (congestion collapse) returning when endpoint discipline lapses. Function and mandate coincide, nothing has atrophied, and the theater series stays flat and low across the whole interval — the opposite of the rising-theater signature that would indicate performance replacing function. The rope classification guards against two mislabels in both directions: reading the compliance costs as extraction (they are the price of a good every seat collects) and reading the latitude as absence of constraint (the outcome contract binds; only the means are free). The migration risk this story carries is not atrophy but externality accumulation: if optimization costs pushed into networks grow with deployment scale (omega optimization_externality_trajectory), the operator and end-user seats' effective extraction rises and the story could migrate toward a hybrid coordination/extraction classification. That migration would show in the temporal series before it showed in the claim.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'This story instantiates one reading of the kernel rfc9293_tcp_specification: that the specification binds observable behavioral outcomes while leaving implementation paths open. Do the sibling readings — strict_invariance_reading (the text mandates an invariant state machine replicated exactly) and middlebox_realism_reading (specification authority is subordinate to the deployed middlebox population) — describe the same text better, and where exactly does the disagreement bind?',
    'Corpus comparison of the three sibling stories'' computed classifications, anchored on the empirical discriminator: whether latitude-exercising variants (BBR, DCTCP) interoperate without specification revision (supports this reading), fail interoperability (supports strict invariance), or interoperate only through per-path accommodation (supports middlebox realism).',
    'Under the strict-invariance sibling the same text''s epsilon rises sharply — latitude becomes non-conformance — and this story''s classification dissolves; under the middlebox-realism sibling the epsilon referent moves from the text to the deployed path and this constraint is absorbed into the sibling''s story.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Which reading of the TCP-specification kernel the text itself best supports.').

omega_variable(
    semantic_bounds_location,
    'Where exactly do the semantic bounds sit: does the contract bind only the byte-stream and connection-management semantics, or also sender responsibilities such as loss response, ECN reaction, and inter-flow fairness?',
    'Standards-track adjudication and deployment evidence: the BBRv1 fairness controversy and its BBRv2/v3 convergence, AQM/ECN deployment studies, and cross-vendor interoperation events reveal which sender behaviors the community treats as contract-bound versus discretionary.',
    'A wider reading of the bounds raises effective extraction at sender seats (more latitude foreclosed) without changing the coordination function; a narrower reading keeps extraction low and shifts more sender behavior into voluntary recommendation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(semantic_bounds_location, conceptual, 'The boundary of implementer latitude inside the outcome contract.').

omega_variable(
    optimization_externality_trajectory,
    'Are the costs optimizations push into the network — queueing latency from deep buffers, fairness disputes between algorithm generations — bounded, or growing with deployment scale?',
    'Longitudinal measurement of queueing latency and flow fairness under the deployed algorithm mix: bufferbloat measurement series, operator telemetry, and cross-deployment comparison as BBR-class senders spread.',
    'Growing externalities raise effective extraction at the operator and end-user seats and could migrate the story toward a hybrid coordination/extraction classification; stable externalities keep the pure-coordination reading intact.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(optimization_externality_trajectory, empirical, 'Whether optimization externalities are accumulating across the deployed population.').

omega_variable(
    self_enforcement_sufficiency,
    'Does interoperability alone still enforce conformance as the implementation population grows, or is active conformance policing (certification regimes, procurement requirements) becoming load-bearing?',
    'Track whether procurement and certification regimes attach to TCP conformance, and whether non-conformant stacks still lose peers in practice; the QUIC migration and embedded-stack proliferation are the natural test cases.',
    'If active policing becomes load-bearing, suppression rises and the enforcement picture flips, moving the story away from pure coordination; if self-enforcement holds, the low-suppression profile stands.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(self_enforcement_sufficiency, empirical, 'Whether conformance remains self-enforcing at current deployment scale.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(rfc9293_tcp_specification__optimization_latitude_reading, 1981, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(rfc9_tr_t1981, rfc9293_tcp_specification__optimization_latitude_reading, theater_ratio, 1981, 0.04).
narrative_ontology:measurement_basis(rfc9_tr_t1981, observed).
narrative_ontology:measurement(rfc9_tr_t1990, rfc9293_tcp_specification__optimization_latitude_reading, theater_ratio, 1990, 0.05).
narrative_ontology:measurement_basis(rfc9_tr_t1990, observed).
narrative_ontology:measurement(rfc9_tr_t1999, rfc9293_tcp_specification__optimization_latitude_reading, theater_ratio, 1999, 0.06).
narrative_ontology:measurement_basis(rfc9_tr_t1999, observed).
narrative_ontology:measurement(rfc9_tr_t2008, rfc9293_tcp_specification__optimization_latitude_reading, theater_ratio, 2008, 0.06).
narrative_ontology:measurement_basis(rfc9_tr_t2008, observed).
narrative_ontology:measurement(rfc9_tr_t2016, rfc9293_tcp_specification__optimization_latitude_reading, theater_ratio, 2016, 0.08).
narrative_ontology:measurement_basis(rfc9_tr_t2016, observed).
narrative_ontology:measurement(rfc9_tr_t2025, rfc9293_tcp_specification__optimization_latitude_reading, theater_ratio, 2025, 0.08).
narrative_ontology:measurement_basis(rfc9_tr_t2025, observed).

% Extraction over time
narrative_ontology:measurement(rfc9_be_t1981, rfc9293_tcp_specification__optimization_latitude_reading, base_extractiveness, 1981, 0.05).
narrative_ontology:measurement_basis(rfc9_be_t1981, observed).
narrative_ontology:measurement(rfc9_be_t1990, rfc9293_tcp_specification__optimization_latitude_reading, base_extractiveness, 1990, 0.06).
narrative_ontology:measurement_basis(rfc9_be_t1990, observed).
narrative_ontology:measurement(rfc9_be_t1999, rfc9293_tcp_specification__optimization_latitude_reading, base_extractiveness, 1999, 0.08).
narrative_ontology:measurement_basis(rfc9_be_t1999, observed).
narrative_ontology:measurement(rfc9_be_t2008, rfc9293_tcp_specification__optimization_latitude_reading, base_extractiveness, 2008, 0.08).
narrative_ontology:measurement_basis(rfc9_be_t2008, observed).
narrative_ontology:measurement(rfc9_be_t2016, rfc9293_tcp_specification__optimization_latitude_reading, base_extractiveness, 2016, 0.09).
narrative_ontology:measurement_basis(rfc9_be_t2016, observed).
narrative_ontology:measurement(rfc9_be_t2025, rfc9293_tcp_specification__optimization_latitude_reading, base_extractiveness, 2025, 0.08).
narrative_ontology:measurement_basis(rfc9_be_t2025, observed).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(rfc9293_tcp_specification__optimization_latitude_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(rfc9293_tcp_specification__optimization_latitude_reading, information_standard).
narrative_ontology:affects_constraint(rfc9293_tcp_specification__optimization_latitude_reading, strict_invariance_reading).
narrative_ontology:affects_constraint(rfc9293_tcp_specification__optimization_latitude_reading, middlebox_realism_reading).

% DUAL FORMULATION NOTE:
% The natural-language label 'the TCP specification' covers at least three structurally distinct claims about what the text is and what it binds. This story authors the outcome-contract/latitude claim with its own low epsilon (0.08): the text binds observable behavior and opens implementation paths, which is what lets high-performance variants (BBR, DCTCP) ship without breaking interoperability. The strict-invariance sibling authors the exact-replication claim, under which latitude-exercising stacks are non-conformant and epsilon is high; the middlebox-realism sibling authors the path-dependence claim, under which the epsilon referent is the deployed network rather than the text. All three presuppose the same established semantic core (wire format and connection semantics), which is the upstream member of the family; the two contested readings are downstream. Each member carries its own beneficiaries, victims, metrics, and classification, linked through affects_constraints on both sides.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
