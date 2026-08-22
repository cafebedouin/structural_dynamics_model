% ============================================================================
% CONSTRAINT STORY: rfc9293_tcp_specification__middlebox_realism_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_rfc9293_tcp_specification__middlebox_realism_reading, []).

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
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_axiom_grounding/3,
    narrative_ontology:cs_created_at/2,
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: rfc9293_tcp_specification__middlebox_realism_reading
 *   human_readable: RFC 9293 Middlebox Realism Reading
 *   domain: network protocol engineering / internet standards / distributed systems coordination
 *
 * SUMMARY:
 *   This constraint is the middlebox_realism_reading of the
 *   rfc9293_tcp_specification kernel. RFC 9293 sets out ideal endpoint
 *   behavior, but the operational reality of the internet is that the
 *   deployed middlebox population determines which TCP segments survive,
 *   which options are honored, and what congestion signals mean. ISPs,
 *   enterprises, and state surveillance systems benefit from this arrangement
 *   through traffic visibility and control; endpoint implementers and users
 *   pay with constrained autonomy and unpredictable path behavior. The
 *   constraint claims tangled_rope because genuine coordination (global
 *   reliable delivery) coexists with asymmetric extraction (control by
 *   middlebox operators). Sibling readings include strict_invariance_reading
 *   (the specification as invariant state machine) and
 *   optimization_latitude_reading (endpoint implementation flexibility within
 *   semantic bounds).
 *
 * KEY AGENTS:
 *   - middlebox_operators: Primary agenda setter and beneficiary (institutional/constrained) â extract control through DPI, NAT, and traffic shaping.
 *   - tcp_endpoint_implementers: Primary payer (organized/constrained) â must implement defensively against path modification.
 *   - internet_users: Secondary payer (powerless/constrained) â bear privacy and performance costs invisibly.
 *   - ietf_standards_body: Analytical observer (institutional/analytical) â maintains the specification without enforcement authority.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(rfc9293_tcp_specification__middlebox_realism_reading, 0.72).
domain_priors:suppression_score(rfc9293_tcp_specification__middlebox_realism_reading, 0.76).
domain_priors:theater_ratio(rfc9293_tcp_specification__middlebox_realism_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(rfc9293_tcp_specification__middlebox_realism_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(rfc9293_tcp_specification__middlebox_realism_reading, suppression_requirement, 0.76).
narrative_ontology:constraint_metric(rfc9293_tcp_specification__middlebox_realism_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(rfc9293_tcp_specification__middlebox_realism_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(rfc9293_tcp_specification__middlebox_realism_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(rfc9293_tcp_specification__middlebox_realism_reading, tangled_rope).
narrative_ontology:human_readable(rfc9293_tcp_specification__middlebox_realism_reading, "RFC 9293 Middlebox Realism Reading").
narrative_ontology:topic_domain(rfc9293_tcp_specification__middlebox_realism_reading, "network protocol engineering / internet standards / distributed systems coordination").

domain_priors:requires_active_enforcement(rfc9293_tcp_specification__middlebox_realism_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(rfc9293_tcp_specification__middlebox_realism_reading, '37093eda-d378-44a1-9117-59c39614f149').
narrative_ontology:cs_kernel_codification('37093eda-d378-44a1-9117-59c39614f149', fixed_text).
narrative_ontology:cs_authority_grounding('37093eda-d378-44a1-9117-59c39614f149', practice).
narrative_ontology:cs_reading_relation('37093eda-d378-44a1-9117-59c39614f149', rfc9293_tcp_specification__strict_invariance_reading, influences).
narrative_ontology:cs_reading_relation('37093eda-d378-44a1-9117-59c39614f149', rfc9293_tcp_specification__optimization_latitude_reading, coexists_with).
narrative_ontology:cs_axiom('37093eda-d378-44a1-9117-59c39614f149', foundational, deployed_path_supersedes_fixed_specification).
narrative_ontology:cs_axiom_status(deployed_path_supersedes_fixed_specification, holdable).
narrative_ontology:cs_axiom_grounding('37093eda-d378-44a1-9117-59c39614f149', deployed_path_supersedes_fixed_specification, empirically_contingent).
narrative_ontology:cs_axiom('37093eda-d378-44a1-9117-59c39614f149', foundational, middlebox_control_is_structural_feature).
narrative_ontology:cs_axiom_status(middlebox_control_is_structural_feature, holdable).
narrative_ontology:cs_axiom_grounding('37093eda-d378-44a1-9117-59c39614f149', middlebox_control_is_structural_feature, empirically_contingent).
narrative_ontology:cs_created_at('37093eda-d378-44a1-9117-59c39614f149', '').
narrative_ontology:cs_kernel_id(rfc9293_tcp_specification__middlebox_realism_reading, rfc9293_tcp_specification).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(rfc9293_tcp_specification__middlebox_realism_reading, middlebox_operators).
narrative_ontology:constraint_victim(rfc9293_tcp_specification__middlebox_realism_reading, tcp_endpoint_implementers).
narrative_ontology:constraint_victim(rfc9293_tcp_specification__middlebox_realism_reading, internet_users).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Deploy deep packet inspection, NAT, firewall, and traffic shaping equipment on network paths. Modify TCP headers and segments to enforce local policy, extract metadata for surveillance, or adapt traffic to link constraints. Their collective operational behavior defines what TCP endpoints must tolerate to achieve global connectivity, independent of what RFC 9293 specifies.
narrative_ontology:constraint_stakeholder(rfc9293_tcp_specification__middlebox_realism_reading, middlebox_operators, agenda_setter,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(rfc9293_tcp_specification__middlebox_realism_reading, middlebox_operators, beneficiary).

% Develop and operate TCP stacks in operating systems, servers, and applications. Must code defensively against middlebox modification: disabling TCP options that provoke stripping, avoiding window scaling that middleboxes mishandle, and tolerating segment splitting and retransmission interference. Cannot assume RFC 9293 semantics will survive the path, and cannot opt out of the middlebox-populated internet without abandoning TCP.
narrative_ontology:constraint_stakeholder(rfc9293_tcp_specification__middlebox_realism_reading, tcp_endpoint_implementers, payer,
    organized, biographical, constrained, global).

% Rely on TCP connections for everyday services. Their traffic is subject to invisible inspection and modification by middleboxes. Experience degraded or unpredictable performance when endpoints and middleboxes disagree on semantics. Privacy and autonomy are reduced by pervasive path-level surveillance and policy enforcement they cannot observe or control.
narrative_ontology:constraint_stakeholder(rfc9293_tcp_specification__middlebox_realism_reading, internet_users, payer,
    powerless, biographical, constrained, global).

% Maintains RFC 9293 and the TCP specification canon. Lacks enforcement authority over the deployed middlebox population. Publishes standards and best-current-practice guidance that operational networks frequently disregard. Observes the divergence between specified and operational semantics without capacity to close the gap.
narrative_ontology:constraint_stakeholder(rfc9293_tcp_specification__middlebox_realism_reading, ietf_standards_body, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(rfc9293_tcp_specification__middlebox_realism_reading, middlebox_operators).
narrative_ontology:fixing_cost_class(rfc9293_tcp_specification__middlebox_realism_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a globally interoperable reliable byte-stream abstraction across heterogeneous, independently administered packet-switched networks with varying link characteristics and congestion levels.
% TRANSFER_FUNCTION: Moves control over packet-level semantics and connection metadata from TCP endpoints to in-path middlebox operators, who gain surveillance and policy-enforcement capability at the expense of endpoint autonomy and specification fidelity.
% ABSENT_VOICES: Privacy advocates and end-users are structurally absent from middlebox deployment decisions; encryption-first endpoint developers are partially heard in standards forums but treated as external to the TCP coordination problem.
% DISAPPEARANCE_RATIONALE: If middlebox modification behavior disappeared and endpoints could rely on RFC 9293 semantics, congestion control, window scaling, and option negotiation would operate as specified; the balance of control between endpoints and network operators would shift; encrypted and non-encrypted traffic would flow without path-dependent modification or invisible inspection.
% FOUNDING_PROBLEM: Achieving reliable, ordered data delivery across independent networks with varying link characteristics, congestion levels, and administrative boundaries.
% FOUNDING_PROBLEM_CORROBORATION: Network operators attest the problem remains live and require middlebox mediation for security and policy compliance. Endpoint implementers and academic network measurement communities attest the foundational interoperability problem is largely solved and middlebox behavior now serves extraction and control; independent path-measurement studies from outside the benefiting parties support the shifted-function reading.
narrative_ontology:disappearance_verdict(rfc9293_tcp_specification__middlebox_realism_reading, world_rearranges).
narrative_ontology:founding_problem_status(rfc9293_tcp_specification__middlebox_realism_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(rfc9293_tcp_specification__middlebox_realism_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(rfc9293_tcp_specification__middlebox_realism_reading, 'none', 1).
narrative_ontology:epsilon_provenance(rfc9293_tcp_specification__middlebox_realism_reading, 0.72, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(rfc9293_tcp_specification__middlebox_realism_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(rfc9293_tcp_specification__middlebox_realism_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(rfc9293_tcp_specification__middlebox_realism_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.72) reflects the substantial control middleboxes extract from endpoints. Suppression (0.76) is high because the constraint persists through active packet modification and droppingâendpoints cannot simply opt out of the middlebox-populated internet. Theater_ratio (0.45) captures the performative maintenance of RFC 9293 as governing text while operational reality diverges. Accessibility_collapse (0.60) acknowledges that alternatives (QUIC, full encryption) are emerging but TCP remains deeply embedded. Resistance (0.55) reflects endpoint workarounds and encryption trends. The time series show extraction rising from 0.20 to 0.72 as middleboxes proliferated from the early internet to the present, with theater peaking during the era of maximum IETF-middlebox tension and modestly declining as the industry implicitly accepts the status quo.
 *
 * PERSPECTIVAL GAP:
 *   Middlebox operators experience the constraint as necessary operational practice and security policy; endpoint implementers experience it as an externally imposed tax on compliance and a source of interoperability bugs; the IETF experiences it as normative aspiration without operational teeth. The engine will compute divergent per-seat classifications from these structural positions: the agenda-setter/beneficiary seat computes toward coordination, while the constrained payer seats compute toward extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   Middlebox operators are beneficiaries (low d) because the constraint subsidizes their surveillance and control capability by forcing endpoints to tolerate modification. Endpoint implementers and internet_users are victims (high d) because the constraint extracts autonomy and imposes defensive implementation costs and privacy losses. The IETF sits near neutral/analytical because it neither collects from nor pays into the extraction, but lacks structural power to alter deployed behavior.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problemâreliable delivery across heterogeneous networksâhas been largely solved by TCP/IP. The persistence of middlebox control beyond that coordination need suggests mandatrophy, but the ongoing operational need for congestion management, security policy, and address translation prevents a clean piton classification. The constraint remains a tangled rope: it retains a genuine coordination function (the internet still delivers bytes), but asymmetric extraction (middlebox operator control) is structurally coupled to that function and actively enforced.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    specification_authority_gap,
    'Is the subordination of RFC 9293 specification authority to deployed middlebox behavior a contingent deployment artifact or an inherent property of path-dependent protocol evolution?',
    'Longitudinal measurement of middlebox modification prevalence across network paths; comparison of enterprise versus access network behavior; analysis of whether QUIC and encryption adoption reduces middlebox pressure or merely displaces it to other protocol layers.',
    'If contingent, the constraint is a tangled rope that could unwind toward rope or scaffold as middleboxes deprecate; if inherent, the extraction is structurally embedded in how internet infrastructure evolves.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(specification_authority_gap, empirical, 'Contingency of the specification-deployment gap').

omega_variable(
    middlebox_motivation_ambiguity,
    'Do middlebox operators modify TCP traffic primarily to solve genuine coordination problems (security, congestion management, address translation) or to extract control and surveillance value from endpoints?',
    'Comparative traffic analysis classifying modifications by type (MSS clamping for PPPoE versus RST injection for censorship); correlation with regulatory regimes and business models.',
    'A predominantly coordination-driven profile would shift the classification toward rope; a predominantly control-driven profile would shift toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(middlebox_motivation_ambiguity, conceptual, 'Ambiguity of middlebox operator motivation between coordination and extraction').

omega_variable(
    kernel_reading_decomposition,
    'How does the middlebox_realism_reading relate to its sibling readings of the RFC 9293 kernel?',
    'Cross-reading structural analysis: strict_invariance_reading treats the specification as invariant law (Mountain/Rope); optimization_latitude_reading treats it as performance-bounded guidance (Rope/Scaffold). This reading treats it as aspirational text subordinate to deployed extraction structure.',
    'Reconciling the readings would require either decomposing the kernel into distinct constraints (specification text versus deployed behavior) or accepting irreducible frame-dependence in protocol ontology.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_decomposition, conceptual, 'Frame-dependence of the RFC 9293 kernel across readings').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(rfc9293_tcp_specification__middlebox_realism_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(rfc9293_mbr_tr_t0, rfc9293_tcp_specification__middlebox_realism_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(rfc9293_mbr_tr_t8, rfc9293_tcp_specification__middlebox_realism_reading, theater_ratio, 8, 0.2).
narrative_ontology:measurement(rfc9293_mbr_tr_t16, rfc9293_tcp_specification__middlebox_realism_reading, theater_ratio, 16, 0.35).
narrative_ontology:measurement(rfc9293_mbr_tr_t24, rfc9293_tcp_specification__middlebox_realism_reading, theater_ratio, 24, 0.45).
narrative_ontology:measurement(rfc9293_mbr_tr_t32, rfc9293_tcp_specification__middlebox_realism_reading, theater_ratio, 32, 0.48).
narrative_ontology:measurement(rfc9293_mbr_tr_t40, rfc9293_tcp_specification__middlebox_realism_reading, theater_ratio, 40, 0.45).

% Extraction over time
narrative_ontology:measurement(rfc9293_mbr_be_t0, rfc9293_tcp_specification__middlebox_realism_reading, base_extractiveness, 0, 0.2).
narrative_ontology:measurement(rfc9293_mbr_be_t8, rfc9293_tcp_specification__middlebox_realism_reading, base_extractiveness, 8, 0.35).
narrative_ontology:measurement(rfc9293_mbr_be_t16, rfc9293_tcp_specification__middlebox_realism_reading, base_extractiveness, 16, 0.55).
narrative_ontology:measurement(rfc9293_mbr_be_t24, rfc9293_tcp_specification__middlebox_realism_reading, base_extractiveness, 24, 0.65).
narrative_ontology:measurement(rfc9293_mbr_be_t32, rfc9293_tcp_specification__middlebox_realism_reading, base_extractiveness, 32, 0.7).
narrative_ontology:measurement(rfc9293_mbr_be_t40, rfc9293_tcp_specification__middlebox_realism_reading, base_extractiveness, 40, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(rfc9293_mbr_su_t0, rfc9293_tcp_specification__middlebox_realism_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(rfc9293_mbr_su_t8, rfc9293_tcp_specification__middlebox_realism_reading, suppression_requirement, 8, 0.45).
narrative_ontology:measurement(rfc9293_mbr_su_t16, rfc9293_tcp_specification__middlebox_realism_reading, suppression_requirement, 16, 0.65).
narrative_ontology:measurement(rfc9293_mbr_su_t24, rfc9293_tcp_specification__middlebox_realism_reading, suppression_requirement, 24, 0.75).
narrative_ontology:measurement(rfc9293_mbr_su_t32, rfc9293_tcp_specification__middlebox_realism_reading, suppression_requirement, 32, 0.78).
narrative_ontology:measurement(rfc9293_mbr_su_t40, rfc9293_tcp_specification__middlebox_realism_reading, suppression_requirement, 40, 0.76).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(rfc9293_tcp_specification__middlebox_realism_reading, global_infrastructure).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
