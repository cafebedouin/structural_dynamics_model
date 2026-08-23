% ============================================================================
% CONSTRAINT STORY: rfc9293_tcp_specification__strict_invariance_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
    narrative_ontology:affects_constraint/2,
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
 *   constraint_id: rfc9293_tcp_specification__strict_invariance_reading
 *   human_readable: RFC 9293 TCP Specification — Strict Invariance Reading (Invariant State-Machine Mandate)
 *   domain: technological/standards-governance
 *
 * SUMMARY:
 *   RFC 9293 specifies TCP as a precise state machine, and the strict
 *   invariance reading holds that implementations must replicate that machine
 *   exactly — externally observable behavior included — because global
 *   interoperability is a commons that unilateral deviation destroys.
 *   Middlebox modification of flows is classified as violation, not feature.
 *   The claim/metric gap is intentional per the independence rule: the
 *   constraint is CLAIMED as rope (a genuine collective-action solution whose
 *   participants are net beneficiaries and whose alternatives are not
 *   suppressed) while the metrics are authored as descriptively true of its
 *   actual operation — low but nonzero compliance burden, conformity
 *   enforcement that has visibly hardened over four decades, and a small
 *   paying minority. This story is one reading of the
 *   rfc9293_tcp_specification kernel; the optimization latitude and middlebox
 *   realism readings are separate constraints with their own epsilon values
 *   and stakeholder surfaces (see network.dual_formulation_note). KEY AGENTS
 *   (by structural relationship): see commentary.key_agents. Assumptions
 *   stated: interval year 0 corresponds to the original host-to-host
 *   specification's publication (1981) and year 45 to the present
 *   consolidation under RFC 9293 (2026); the strict reading's victim class is
 *   taken to be implementations whose reliance on the published guarantees is
 *   total, principally resource-constrained embedded systems, together with
 *   flow-modifying intermediaries whose existing practice the reading
 *   outlaws.
 *
 * KEY AGENTS:
 *   - - ietf_transport_community: Agenda setter (institutional/mobile) — authors, revises, and polices the invariant through working-group consensus, errata, and interop events; authority is voluntary and demonstrated by its mobility (it chartered QUIC when the incumbent transport bounded progress)
 *   - - mainstream_os_tcp_stacks: Primary beneficiary with real compliance cost (powerful/constrained) — maintains kernel state machines, receives universal operability, hedges via QUIC funding
 *   - - embedded_and_iot_implementers: Principal paying seat (moderate/trapped) — least engineering slack, total reliance on the published guarantees, hardest failures under any deviation
 *   - - middlebox_vendors_and_operators: Paying seat under prohibition (powerful/constrained) — flow-modifying products reclassified as violations; adapts and re-engineers rather than exits
 *   - - application_developers: Beneficiary (moderate/mobile) — consumes the reliable byte-stream abstraction; migrates to HTTP/3 where latency binds
 *   - - internet_end_users: Diffuse beneficiary (organized/immediate) — experiences the commons as 'the Internet just works'; no direct contribution
 *   - - backbone_network_operators: Beneficiary without compliance cost (institutional/constrained) — receives congestion-shaped, analyzable traffic it did nothing to produce
 *   - - quic_http3_community: Exited constituency (powerful/arbitrage) — routed around the invariant over encrypted UDP; standing proof that exit is viable at scale
 *   - - conformance_testing_community: Analytical observer (analytical/analytical) — measures conformity for the strict camp and deviation for the challengers; collects and pays nothing
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(rfc9293_tcp_specification__strict_invariance_reading, 0.16).
domain_priors:suppression_score(rfc9293_tcp_specification__strict_invariance_reading, 0.3).
domain_priors:theater_ratio(rfc9293_tcp_specification__strict_invariance_reading, 0.07).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(rfc9293_tcp_specification__strict_invariance_reading, extractiveness, 0.16).
narrative_ontology:constraint_metric(rfc9293_tcp_specification__strict_invariance_reading, suppression_requirement, 0.3).
narrative_ontology:constraint_metric(rfc9293_tcp_specification__strict_invariance_reading, theater_ratio, 0.07).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(rfc9293_tcp_specification__strict_invariance_reading, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(rfc9293_tcp_specification__strict_invariance_reading, resistance, 0.38).

% --- Constraint claim ---
narrative_ontology:constraint_claim(rfc9293_tcp_specification__strict_invariance_reading, rope).
narrative_ontology:human_readable(rfc9293_tcp_specification__strict_invariance_reading, "RFC 9293 TCP Specification — Strict Invariance Reading (Invariant State-Machine Mandate)").
narrative_ontology:topic_domain(rfc9293_tcp_specification__strict_invariance_reading, "technological/standards-governance").

domain_priors:requires_active_enforcement(rfc9293_tcp_specification__strict_invariance_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(rfc9293_tcp_specification__strict_invariance_reading, 'ce95631b-da87-451d-9769-3a3335195f63').
narrative_ontology:cs_kernel_codification('ce95631b-da87-451d-9769-3a3335195f63', formalized).
narrative_ontology:cs_authority_grounding('ce95631b-da87-451d-9769-3a3335195f63', expertise).
narrative_ontology:cs_interpretation_layer_present('ce95631b-da87-451d-9769-3a3335195f63').
narrative_ontology:cs_reading_relation('ce95631b-da87-451d-9769-3a3335195f63', rfc9293_tcp_specification__optimization_latitude_reading, coexists_with).
narrative_ontology:cs_reading_relation('ce95631b-da87-451d-9769-3a3335195f63', rfc9293_tcp_specification__middlebox_realism_reading, coexists_with).
narrative_ontology:cs_axiom('ce95631b-da87-451d-9769-3a3335195f63', foundational, invariant_machine_replication_mandated).
narrative_ontology:cs_axiom_status(invariant_machine_replication_mandated, holdable).
narrative_ontology:cs_axiom_grounding('ce95631b-da87-451d-9769-3a3335195f63', invariant_machine_replication_mandated, instrumental).
narrative_ontology:cs_axiom('ce95631b-da87-451d-9769-3a3335195f63', foundational, middlebox_modification_is_violation).
narrative_ontology:cs_axiom_status(middlebox_modification_is_violation, holdable).
narrative_ontology:cs_axiom_grounding('ce95631b-da87-451d-9769-3a3335195f63', middlebox_modification_is_violation, deontological).
narrative_ontology:cs_reference_frame('ce95631b-da87-451d-9769-3a3335195f63', invariant_state_machine_canon).
narrative_ontology:cs_drift_state('ce95631b-da87-451d-9769-3a3335195f63', post_quic_middlebox_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('ce95631b-da87-451d-9769-3a3335195f63', '').
narrative_ontology:cs_kernel_id(rfc9293_tcp_specification__strict_invariance_reading, rfc9293_tcp_specification).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(rfc9293_tcp_specification__strict_invariance_reading, mainstream_os_tcp_stacks).
narrative_ontology:constraint_beneficiary(rfc9293_tcp_specification__strict_invariance_reading, application_developers).
narrative_ontology:constraint_beneficiary(rfc9293_tcp_specification__strict_invariance_reading, internet_end_users).
narrative_ontology:constraint_beneficiary(rfc9293_tcp_specification__strict_invariance_reading, backbone_network_operators).
narrative_ontology:constraint_victim(rfc9293_tcp_specification__strict_invariance_reading, embedded_and_iot_implementers).
narrative_ontology:constraint_victim(rfc9293_tcp_specification__strict_invariance_reading, middlebox_vendors_and_operators).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(rfc9293_tcp_specification__strict_invariance_reading, embedded_and_iot_implementers).
narrative_ontology:constraint_victim(rfc9293_tcp_specification__strict_invariance_reading, mainstream_os_tcp_stacks).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Drafts, revises, and publishes the specification through working-group consensus, maintains the errata system, and organizes interop events where implementations are checked against one another. Its authority is voluntary: implementers follow because the process demonstrates competence, not because compliance is compelled. When TCP's shared rules bounded progress on latency and ossification problems, this community chartered QUIC and moved part of its transport work outside the document it administers.
narrative_ontology:constraint_stakeholder(rfc9293_tcp_specification__strict_invariance_reading, ietf_transport_community, agenda_setter,
    institutional, generational, mobile, global).

% Maintains the kernel transport stacks in Linux, Windows, macOS, and the BSDs. Receives guaranteed operability with every other stack on the planet, which is the foundation of its platform value. Pays recurring conformance cost: each specification revision raises some recommendations to requirements, and the stacks must re-verify behavior against the tightening text. Cannot abandon the arrangement because every client and server expects it, though the large stack owners jointly fund QUIC development as a hedge.
narrative_ontology:constraint_stakeholder(rfc9293_tcp_specification__strict_invariance_reading, mainstream_os_tcp_stacks, beneficiary,
    powerful, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(rfc9293_tcp_specification__strict_invariance_reading, mainstream_os_tcp_stacks, payer).

% Builds medical devices, industrial controllers, automotive modules, and consumer IoT firmware that must speak the transport protocol with minimal memory and engineering slack. Depends totally on every peer honoring the published machine, because it cannot afford defensive parsing of hostile or nonstandard behavior; when any node on the path deviates, these devices fail first and hardest. Firmware ships for a decade or more under certification regimes, so switching transports is rarely possible after design freeze. Bears the full conformance cost of each revision with the least capacity to absorb it, while still receiving ordinary operability like everyone else.
narrative_ontology:constraint_stakeholder(rfc9293_tcp_specification__strict_invariance_reading, embedded_and_iot_implementers, payer,
    moderate, biographical, trapped, global).
narrative_ontology:stakeholder_secondary_role(rfc9293_tcp_specification__strict_invariance_reading, embedded_and_iot_implementers, beneficiary).

% Builds and operates firewalls, NATs, load balancers, and WAN optimizers that inspect, rewrite, or terminate transport flows. Under the strict reading of the specification, flow modification is classified as violation rather than feature, and successive revisions have converted what these products routinely did — stripping unfamiliar options, coalescing acknowledgments, rewriting sequence numbers — into documented nonconformance that the text now works around by making formerly optional behaviors mandatory to preserve. Enterprise demand for traffic inspection persists, so these firms adapt and re-engineer rather than exit.
narrative_ontology:constraint_stakeholder(rfc9293_tcp_specification__strict_invariance_reading, middlebox_vendors_and_operators, payer,
    powerful, biographical, constrained, global).

% Writes software against a reliable byte-stream abstraction and never negotiates transport semantics per peer. Gains the largest single economies of the arrangement: one mental model, one debugging literature, universal reach. Where latency sensitivity or head-of-line blocking hurts, the migration path to HTTP/3 over QUIC is real and increasingly paved, so exit is available at the application layer.
narrative_ontology:constraint_stakeholder(rfc9293_tcp_specification__strict_invariance_reading, application_developers, beneficiary,
    moderate, biographical, mobile, global).

% Experiences the arrangement only as the background fact that devices and services interoperate. Pays nothing directly and contributes no conformance effort. Individual voice is negligible; aggregate purchasing and attention decisions shape which stacks and transports platforms ship.
narrative_ontology:constraint_stakeholder(rfc9293_tcp_specification__strict_invariance_reading, internet_end_users, beneficiary,
    organized, immediate, mobile, global).

% Carries the world's transport flows and receives congestion-shaped, statistically predictable traffic without contributing any conformance effort of its own. Capacity planning, peering economics, and outage forensics all rest on sender behavior following the published rules. Cannot decline to carry the protocol; its stake is in the invariant holding everywhere its packets transit.
narrative_ontology:constraint_stakeholder(rfc9293_tcp_specification__strict_invariance_reading, backbone_network_operators, beneficiary,
    institutional, generational, constrained, global).

% Browser, CDN, and cloud vendors that concluded the shared transport rules could not be amended fast enough for their latency and ossification problems, and responded by rebuilding transport over encrypted UDP where intermediaries cannot read or modify state at all. Rather than contesting the document's requirements from inside, they routed around them; their growing traffic share is a standing demonstration that exit from the arrangement is viable at scale.
narrative_ontology:constraint_stakeholder(rfc9293_tcp_specification__strict_invariance_reading, quic_http3_community, excluded,
    powerful, biographical, arbitrage, global).

% Maintains test harnesses and packet-crafting tools, runs interop bake-offs, and publishes measurement studies of deployed behavior. Collects nothing and pays nothing under the arrangement; its findings serve both camps — catching implementer deviation for the strict camp and documenting systematic path behavior for those who study what the network actually does.
narrative_ontology:constraint_stakeholder(rfc9293_tcp_specification__strict_invariance_reading, conformance_testing_community, observer,
    analytical, biographical, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(rfc9293_tcp_specification__strict_invariance_reading, diffuse).
narrative_ontology:fixing_cost_class(rfc9293_tcp_specification__strict_invariance_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides one precisely specified, publicly maintained transport state machine so that independently written, independently evolving implementations interoperate by construction rather than by negotiation; embeds congestion control so that millions of competing flows share finite link capacity without destroying it.
% TRANSFER_FUNCTION: Moves compliance effort and behavioral restraint from every participating implementer into the shared interoperability surface: each stack pays exact-replication cost and forgoes unilateral deviation, and in exchange receives guaranteed mutual operability. No money moves; the goods transferred are engineering effort, design freedom, and conformity.
% ABSENT_VOICES: Middlebox builders were historically outside the room where endpoint correctness was defined — the end-to-end design tradition treated intermediaries as obstacles to be designed around, not constituents. Latency-sensitive application designers and operators of high-loss satellite links objected to inflexible semantics and largely left the conversation by funding and deploying QUIC instead of litigating the text. Embedded-device makers, with the least engineering slack and the most total dependence on the invariant, have the least representation in working-group deliberation.
% DISAPPEARANCE_RATIONALE: If the invariant vanished overnight, stacks would diverge immediately and every cross-vendor connection would renegotiate behavior experimentally. Congestion control would fragment, and shared links would lurch back toward the collapse dynamics the mandates were written to prevent. Middleboxes would optimize against whichever dialects survived in each region. The transport layer would reorganize into incompatible application and regional fiefdoms — the pre-TCP fragmentation the specification exists to have ended.
% FOUNDING_PROBLEM: Heterogeneous host systems on a shared packet-switched network could not interoperate reliably: each vendor shipped ad hoc host-to-host procedures, and the 1986 congestion collapse demonstrated that uncoordinated sending behavior destroyed the shared infrastructure itself.
% FOUNDING_PROBLEM_CORROBORATION: Attested from outside the benefiting parties: independent interop bake-offs and third-party conformance tooling still surface cross-implementation divergence whenever a requirement loosens; the academic measurement literature documents fresh breakage whenever middlebox behavior shifts; and the QUIC design community — a constituency that exited the arrangement — attests in its own charter materials that the shared-fate properties of the incumbent transport were precisely the problem it had to solve differently. No attestation from inside the governance process is required.
narrative_ontology:disappearance_verdict(rfc9293_tcp_specification__strict_invariance_reading, world_rearranges).
narrative_ontology:founding_problem_status(rfc9293_tcp_specification__strict_invariance_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(rfc9293_tcp_specification__strict_invariance_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(rfc9293_tcp_specification__strict_invariance_reading, 'none', 1).
narrative_ontology:epsilon_provenance(rfc9293_tcp_specification__strict_invariance_reading, 0.16, 'stealth/ox-alpha', 'none', direct).

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
 *   Extractiveness is authored low (0.16 at interval end) because the burden the arrangement imposes — exact replication, no unilateral latitude, periodic re-verification as requirements tighten — is real but dwarfed by the operability every seat receives, and no seat converts the arrangement into income; the value sits just above the information_standard coordination floor, which is the honest neighborhood for a protocol whose implementation cost exceeds a naming convention but whose surplus is not captured by anyone. Suppression (0.30) is authored as a raw, unscaled structural property: participation is voluntary, alternatives are not merely tolerated but thriving (QUIC), and no outsider is coerced — what suppression exists is conformity pressure inside the cooperating class, and the suppression_requirement series traces its deliberate hardening (host requirements documents, mandatory congestion control after the 1986 collapse, institutionalized interop testing, and finally the current specification's explicit retreat from liberal acceptance of malformed input). That series is authored because this story specifically tracks enforcement-capacity change: the trajectory rises monotonically as the policing machinery matured, with no oscillation, so no cyclical pattern is claimed. Theater_ratio stays negligible (0.07) because nearly all specified activity is functional — the machine does the work it describes — with a thin residue of pro-forma requirements documented more than tested. Accessibility_collapse (0.42) reflects that understanding the arrangement does not dissolve alternatives: QUIC, SCTP, and raw-UDP designs remain reachable at moderate friction, though kernel-level ubiquity keeps exit expensive for stacks and devices. Resistance (0.38) is real but channeled: it lives in the deployed middlebox population's accumulated behavior, in the latitude constituency's pushback on requirement inflation, and in the exited QUIC community's existence, rather than in open defiance. All three tracked series share one six-point time grid (years 0, 9, 18, 27, 36, 45) with every metric authored at every point. Receipt surface: gain_flow is authored as 'diffuse' after checking every named seat — no seat accrues the arrangement's surplus as income; compliance effort dissipates into maintenance and operability gains are benefit-from, not receipt-of. fixing_cost is authored as 'prohibitive': the only seat able to relax the invariant (the agenda setter) faces removal whose losses — global interoperability collapse — exceed any relief by orders of magnitude, so the cost class of fixing is prohibitive relative to its benefit.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute differently and the engine owns that computation. From the agenda setter's chair the arrangement is a successfully maintained commons it built and keeps repairing. From the mainstream stack seat it is a favorable trade: recurring conformance cost purchased with universal operability. From the embedded seat it is a fragile dependency — a promise of universal conformity on which its devices stake everything, honored by a network that only approximately complies. From the middlebox seat it is a prohibition: a text that reclassifies its products' core behaviors as violations and steadily converts former liberties into documented nonconformance. From the exited QUIC seat it is a solved problem, visible only in retrospect. Same document, same requirements text — five structurally different arrangements depending on where one stands. No authored field reconciles these; the divergence is the measurement.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality follows the beneficiary/victim declarations and exit structure. The four beneficiary groups sit near the subsidized end: end users and backbone operators receive the commons without paying conformance cost (lowest d), application developers pay little beyond adherence, and mainstream stacks sit slightly higher in d because their secondary paying position is real. The embedded implementers are the sharpest paying seat: trapped exit, total guarantee-reliance, and full compliance cost with minimal slack place them near the target end despite their secondary beneficiary position. Middlebox operators also sit near the target end within this reading's frame — but their payment is forfeiture of an interference liberty, not a transfer flowing to anyone else; the arrangement extracts conformity from them without depositing the proceeds anywhere, which is why gain_flow remains diffuse despite two declared victim classes. Global spatial scope applies the engine's modest verification-difficulty amplification to the paying seats' effective burden; the engine owns that arithmetic. Suppression is authored unscaled, as a raw structural property, and the commentary treats it accordingly.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — heterogeneous hosts that cannot interoperate, and shared infrastructure that collapses under uncoordinated sending — is live and permanently so: it regenerates with every new implementer, every new middlebox behavior, and every new application class. Because the founding problem is live and the verdict is world_rearranges, the mismatch consumer finds no dead-problem-plus-world_rearranges flag, and no zombie signature is asserted. The classification discipline this story exercises runs in both directions: calling the arrangement rope prevents the paying seats' real burdens (embedded fragility, middlebox prohibition) from being misread as pure extraction riding a fake coordination story — the coordination function here is as genuine as coordination gets. Symmetrically, the temporal record watches the failure mode specific to ropes: requirement inflation that accumulates compliance cost faster than interoperability benefit (visible as the slow base_extractiveness creep from 0.10 to 0.16 alongside the steeper enforcement hardening), and the QUIC exit as an early-warning indicator that the cooperating population could drain. If a future revision cycle kept tightening requirements after the last interoperability benefit was banked, this rope would begin converting into something with a captured maintenance constituency; the measurements exist to date that transition if it comes.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    conformance_binding_scope,
    'Does the strict reading''s mandate to ''replicate the invariant machine exactly'' bind externally observable protocol behavior, internal algorithm structure, or both?',
    'Specification of conformance-test semantics: a black-box suite (packet-exchange equivalence) versus white-box review (internal timer, buffer, and algorithm audit) would settle which freedoms survive.',
    'A wider binding (internals included) raises epsilon by removing implementer design freedom the narrower reading permits, and shifts some latitude-reading territory into this story''s ledger.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(conformance_binding_scope, conceptual, 'Ambiguity in what exact replication covers — observable wire behavior versus internal implementation structure.').

omega_variable(
    coordination_cost_vs_latent_burden,
    'Is the epsilon above the information_standard floor genuine coordination cost (timers, congestion-control complexity, re-verification cycles), or does requirement inflation impose burden beyond what interoperability needs?',
    'Itemized accounting comparing per-requirement implementation cost against measured interoperability benefit, using interop-event failure data before and after individual requirement tightenings.',
    'If a material share of the burden buys no interoperability, the rope certification weakens and the excess flags as accumulating overhead serving process maintenance rather than participants.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_cost_vs_latent_burden, empirical, 'Whether measured compliance burden tracks the coordination function or exceeds it.').

omega_variable(
    victim_attribution_question,
    'Are strict-guarantee-reliant implementations harmed by the invariant itself, or by other parties'' nonconformance to it — that is, does the declared victim class belong to this reading''s ledger or to the enforcement-gap ledger?',
    'Counterfactual comparison across readings: model embedded-system failure rates under strict, latitude, and realism regimes; if failures fall under looser regimes, the harm attributes to the invariant''s overpromise; if they rise, it attributes to deviation.',
    'Attribution to the invariant itself would push this story''s effective extraction upward and toward a hybrid classification; attribution to enforcement gaps leaves the rope intact with the gap recorded elsewhere.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(victim_attribution_question, conceptual, 'Whether the embedded-implementer victim class is caused by the strict guarantee or by its incomplete observance.').

omega_variable(
    sibling_latitude_structural_delta,
    'What structurally changes under the optimization latitude reading of the same kernel text?',
    'Generate the sibling story: conformance defined at the behavioral-outcome level absorbs much of the compliance cost into permitted latitude, thinning the victim set to parties harmed by outcome-level divergence only, and lowering epsilon below this reading''s 0.16.',
    'Documents the committer delta: the same text yields a different constraint with a smaller paying class and weaker enforcement need; classification comparisons across the pair measure how much of the burden is definitional rather than physical.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_latitude_structural_delta, conceptual, 'Committer-routing omega: the declared structural difference this reading''s latitude sibling would exhibit.').

omega_variable(
    sibling_realism_structural_delta,
    'What structurally changes under the middlebox realism reading of the same kernel text?',
    'Generate the sibling story: the referent shifts from the specification to the deployed-path protocol, the middlebox population moves from violating class to constitutive fact, and epsilon is computed against endpoint-plus-path behavior — plausibly yielding a hybrid classification with identifiable coordinated and paying classes.',
    'Documents the committer delta: authority locus moves from text to deployment; this reading''s victim classes redistribute (middleboxes exit the paying set; strict-reliant implementers remain exposed); the pair''s comparison quantifies the cost of the text-versus-dispute over sovereignty.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_realism_structural_delta, conceptual, 'Committer-routing omega: the declared structural difference this reading''s realism sibling would exhibit.').

omega_variable(
    quic_exit_drain_lifecycle,
    'If encrypted-transport exit continues scaling, does the strict regime''s cooperating population drain to the point where the arrangement persists mainly by inertia?',
    'Transport-share telemetry: track the HTTP/3 fraction of web traffic and the emergence of non-web QUIC applications over the coming decade.',
    'Sustained drain would date a lifecycle transition in which the specification''s function persists but its living constituency shrinks — the characteristic precursor to an inertial, theatrically maintained remainder; a plateau would indicate a stable two-protocol equilibrium.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(quic_exit_drain_lifecycle, empirical, 'Whether the exited QUIC constituency foreshadows a draining of the strict regime''s coordination base.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(rfc9293_tcp_specification__strict_invariance_reading, 0, 45).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(rfc9_tr_t0, rfc9293_tcp_specification__strict_invariance_reading, theater_ratio, 0, 0.04).
narrative_ontology:measurement(rfc9_tr_t9, rfc9293_tcp_specification__strict_invariance_reading, theater_ratio, 9, 0.05).
narrative_ontology:measurement(rfc9_tr_t18, rfc9293_tcp_specification__strict_invariance_reading, theater_ratio, 18, 0.05).
narrative_ontology:measurement(rfc9_tr_t27, rfc9293_tcp_specification__strict_invariance_reading, theater_ratio, 27, 0.06).
narrative_ontology:measurement(rfc9_tr_t36, rfc9293_tcp_specification__strict_invariance_reading, theater_ratio, 36, 0.06).
narrative_ontology:measurement(rfc9_tr_t45, rfc9293_tcp_specification__strict_invariance_reading, theater_ratio, 45, 0.07).

% Extraction over time
narrative_ontology:measurement(rfc9_be_t0, rfc9293_tcp_specification__strict_invariance_reading, base_extractiveness, 0, 0.1).
narrative_ontology:measurement(rfc9_be_t9, rfc9293_tcp_specification__strict_invariance_reading, base_extractiveness, 9, 0.11).
narrative_ontology:measurement(rfc9_be_t18, rfc9293_tcp_specification__strict_invariance_reading, base_extractiveness, 18, 0.13).
narrative_ontology:measurement(rfc9_be_t27, rfc9293_tcp_specification__strict_invariance_reading, base_extractiveness, 27, 0.14).
narrative_ontology:measurement(rfc9_be_t36, rfc9293_tcp_specification__strict_invariance_reading, base_extractiveness, 36, 0.15).
narrative_ontology:measurement(rfc9_be_t45, rfc9293_tcp_specification__strict_invariance_reading, base_extractiveness, 45, 0.16).

% Suppression requirement over time
narrative_ontology:measurement(rfc9_su_t0, rfc9293_tcp_specification__strict_invariance_reading, suppression_requirement, 0, 0.1).
narrative_ontology:measurement(rfc9_su_t9, rfc9293_tcp_specification__strict_invariance_reading, suppression_requirement, 9, 0.15).
narrative_ontology:measurement(rfc9_su_t18, rfc9293_tcp_specification__strict_invariance_reading, suppression_requirement, 18, 0.2).
narrative_ontology:measurement(rfc9_su_t27, rfc9293_tcp_specification__strict_invariance_reading, suppression_requirement, 27, 0.24).
narrative_ontology:measurement(rfc9_su_t36, rfc9293_tcp_specification__strict_invariance_reading, suppression_requirement, 36, 0.27).
narrative_ontology:measurement(rfc9_su_t45, rfc9293_tcp_specification__strict_invariance_reading, suppression_requirement, 45, 0.3).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(rfc9293_tcp_specification__strict_invariance_reading, information_standard).
narrative_ontology:affects_constraint(rfc9293_tcp_specification__strict_invariance_reading, rfc9293_tcp_specification__optimization_latitude_reading).
narrative_ontology:affects_constraint(rfc9293_tcp_specification__strict_invariance_reading, rfc9293_tcp_specification__middlebox_realism_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'the TCP specification' decomposes into three structurally distinct constraints with distinct epsilon values over distinct referents. This story instantiates the strict invariance reading: the specification as a mandated invariant machine, epsilon low (0.16) because the compliance burden is dominated by genuine coordination cost. The optimization latitude reading reads the same text as specifying behavioral outcomes with internal freedom, absorbing much of the compliance cost into permitted latitude (epsilon lower still). The middlebox realism reading takes deployed-path behavior as the real protocol and treats specification authority as subordinate, exposing substantially more contestation and a higher effective epsilon against a referent that includes the middlebox population. Upstream-downstream structure: the strict reading is the established textual baseline; the latitude reading mediates between text and practice; the realism reading is the measurement-driven challenger whose evidence the strict tradition repeatedly absorbs (each tightening round codifies a middlebox behavior as a constraint on endpoints). All three files link one another through affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
