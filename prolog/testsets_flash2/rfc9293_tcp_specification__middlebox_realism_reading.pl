% ============================================================================
% CONSTRAINT STORY: rfc9293_tcp_specification__middlebox_realism_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
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
 *   constraint_id: rfc9293_tcp_specification__middlebox_realism_reading
 *   human_readable: RFC 9293 TCP Specification (Middlebox Realism Reading)
 *   domain: network_protocol_engineering/internet_standards/distributed_systems_coordination
 *
 * SUMMARY:
 *   This constraint describes the reality of TCP operation where RFC 9293,
 *   while an official standard, functions more as an ideal endpoint behavior
 *   rather than an enforceable global invariant. The actual behavior of TCP
 *   is heavily influenced by the pervasive deployment of 'middleboxes'
 *   (firewalls, NATs, proxies, deep packet inspection systems) that modify
 *   traffic for local policy enforcement, security, or surveillance. This
 *   reading highlights how the specification's authority is subordinate to
 *   the operational reality shaped by these intermediaries, leading to a
 *   system where endpoint autonomy is extracted, and the specification itself
 *   becomes a theatrical performance of an ideal that is rarely met.
 *
 * KEY AGENTS:
 *   - middlebox_operators: Primary agenda-setters and beneficiaries (institutional/arbitrage)
 *   - endpoint_users: Primary targets and payers (powerless/trapped)
 *   - application_developers: Secondary targets and payers (moderate/constrained)
 *   - internet_standards_bodies: Analytical observers (organized/analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(rfc9293_tcp_specification__middlebox_realism_reading, 0.78).
domain_priors:suppression_score(rfc9293_tcp_specification__middlebox_realism_reading, 0.85).
domain_priors:theater_ratio(rfc9293_tcp_specification__middlebox_realism_reading, 0.6).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(rfc9293_tcp_specification__middlebox_realism_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(rfc9293_tcp_specification__middlebox_realism_reading, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(rfc9293_tcp_specification__middlebox_realism_reading, theater_ratio, 0.6).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(rfc9293_tcp_specification__middlebox_realism_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(rfc9293_tcp_specification__middlebox_realism_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(rfc9293_tcp_specification__middlebox_realism_reading, tangled_rope).
narrative_ontology:human_readable(rfc9293_tcp_specification__middlebox_realism_reading, "RFC 9293 TCP Specification (Middlebox Realism Reading)").
narrative_ontology:topic_domain(rfc9293_tcp_specification__middlebox_realism_reading, "network_protocol_engineering/internet_standards/distributed_systems_coordination").

domain_priors:requires_active_enforcement(rfc9293_tcp_specification__middlebox_realism_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(rfc9293_tcp_specification__middlebox_realism_reading, '1133e301-9d26-4b5e-98d8-e0f0bc00819e').
narrative_ontology:cs_kernel_codification('1133e301-9d26-4b5e-98d8-e0f0bc00819e', fixed_text).
narrative_ontology:cs_authority_grounding('1133e301-9d26-4b5e-98d8-e0f0bc00819e', extraction).
narrative_ontology:cs_interpretation_layer_present('1133e301-9d26-4b5e-98d8-e0f0bc00819e').
narrative_ontology:cs_reading_relation('1133e301-9d26-4b5e-98d8-e0f0bc00819e', rfc9293_tcp_specification__strict_invariance_reading, forecloses).
narrative_ontology:cs_reading_relation('1133e301-9d26-4b5e-98d8-e0f0bc00819e', rfc9293_tcp_specification__optimization_latitude_reading, influences).
narrative_ontology:cs_axiom('1133e301-9d26-4b5e-98d8-e0f0bc00819e', foundational, network_controls_protocol_behavior).
narrative_ontology:cs_axiom_status(network_controls_protocol_behavior, holdable).
narrative_ontology:cs_axiom_grounding('1133e301-9d26-4b5e-98d8-e0f0bc00819e', network_controls_protocol_behavior, empirically_contingent).
narrative_ontology:cs_axiom('1133e301-9d26-4b5e-98d8-e0f0bc00819e', secondary, specification_is_aspirational).
narrative_ontology:cs_axiom_status(specification_is_aspirational, holdable).
narrative_ontology:cs_axiom_grounding('1133e301-9d26-4b5e-98d8-e0f0bc00819e', specification_is_aspirational, conventional).
narrative_ontology:cs_reference_frame('1133e301-9d26-4b5e-98d8-e0f0bc00819e', middlebox_dominated_internet).
narrative_ontology:cs_drift_state('1133e301-9d26-4b5e-98d8-e0f0bc00819e', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('1133e301-9d26-4b5e-98d8-e0f0bc00819e', '').
narrative_ontology:cs_kernel_id(rfc9293_tcp_specification__middlebox_realism_reading, rfc9293_tcp_specification).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(rfc9293_tcp_specification__middlebox_realism_reading, middlebox_operators).
narrative_ontology:constraint_beneficiary(rfc9293_tcp_specification__middlebox_realism_reading, network_administrators).
narrative_ontology:constraint_victim(rfc9293_tcp_specification__middlebox_realism_reading, endpoint_users).
narrative_ontology:constraint_victim(rfc9293_tcp_specification__middlebox_realism_reading, application_developers).
narrative_ontology:constraint_vindicates(rfc9293_tcp_specification__middlebox_realism_reading, network_neutrality_erosion).
narrative_ontology:constraint_vindicates(rfc9293_tcp_specification__middlebox_realism_reading, surveillance_by_design).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% ISPs, enterprises, and state actors who deploy middleboxes (firewalls, NATs, proxies, DPI systems) that inspect and modify TCP traffic. They prioritize local policy enforcement, security, or surveillance over strict protocol adherence, effectively shaping the 'real' TCP behavior. They benefit from the control and data extraction these modifications enable.
narrative_ontology:constraint_stakeholder(rfc9293_tcp_specification__middlebox_realism_reading, middlebox_operators, agenda_setter,
    institutional, generational, arbitrage, global).

% Individuals and organizations whose internet traffic is subject to middlebox interference. They experience degraded performance, broken applications, or privacy violations due to non-standard TCP behavior. They have no direct control over the network path or middlebox operations.
narrative_ontology:constraint_stakeholder(rfc9293_tcp_specification__middlebox_realism_reading, endpoint_users, payer,
    powerless, biographical, trapped, global).

% Developers of applications that rely on predictable TCP behavior. They must implement workarounds for middlebox-induced anomalies, increasing complexity and reducing reliability. Their ability to innovate is constrained by the lowest common denominator of deployed network behavior.
narrative_ontology:constraint_stakeholder(rfc9293_tcp_specification__middlebox_realism_reading, application_developers, payer,
    moderate, biographical, constrained, global).

% Organizations like the IETF that publish RFCs. They observe the divergence between specified and actual behavior, attempting to document or adapt standards to reflect reality, but lack direct enforcement power over deployed networks. Their authority is aspirational.
narrative_ontology:constraint_stakeholder(rfc9293_tcp_specification__middlebox_realism_reading, internet_standards_bodies, observer,
    organized, generational, analytical, global).

% Professionals responsible for managing local networks. They often deploy middleboxes to enforce security policies, manage traffic, or comply with regulations. While they may also be affected by upstream middleboxes, their local control allows them to benefit from the flexibility of modifying TCP behavior.
narrative_ontology:constraint_stakeholder(rfc9293_tcp_specification__middlebox_realism_reading, network_administrators, beneficiary,
    powerful, biographical, mobile, regional).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The RFC 9293 specification attempts to coordinate endpoint behavior to ensure global interoperability and reliable data transfer across diverse networks.
% TRANSFER_FUNCTION: The middlebox population transfers control over TCP session parameters and data flow from endpoints to network intermediaries, often for policy enforcement, surveillance, or traffic shaping. This also transfers operational burden and complexity to application developers.
% ABSENT_VOICES: The original architects of TCP and proponents of end-to-end transparency, who envisioned a 'dumb network' with intelligence at the edges, are largely absent from the operational decision-making that deploys middleboxes. Their design philosophy is systematically undermined.
% DISAPPEARANCE_RATIONALE: If the middlebox-driven reality of TCP vanished overnight, and all traffic strictly adhered to RFC 9293, network security models would collapse, many enterprise and state policies would fail, and application developers would suddenly find their workarounds unnecessary. The internet's operational and security landscape would fundamentally reorganize.
% FOUNDING_PROBLEM: The original TCP specification aimed to provide a robust, reliable, and interoperable transport layer for the nascent internet, ensuring any two endpoints could communicate predictably.
% FOUNDING_PROBLEM_CORROBORATION: Network engineers and academic researchers widely acknowledge that the original problem of universal interoperability, as envisioned by RFC 9293, is no longer fully addressed by the specification alone due to pervasive middlebox interference. Independent studies and operational experience corroborate that the 'real' TCP is path-dependent, not strictly endpoint-controlled.
narrative_ontology:disappearance_verdict(rfc9293_tcp_specification__middlebox_realism_reading, world_rearranges).
narrative_ontology:founding_problem_status(rfc9293_tcp_specification__middlebox_realism_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(rfc9293_tcp_specification__middlebox_realism_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(rfc9293_tcp_specification__middlebox_realism_reading, 'none', 1).
narrative_ontology:epsilon_provenance(rfc9293_tcp_specification__middlebox_realism_reading, 0.78, 'gemini-2.5-flash', 'none', direct).

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
 *   The extractiveness (0.78) is high because middleboxes effectively 'tax' the end-to-end principle, forcing endpoints to adapt to non-standard behaviors. Suppression (0.85) is severe as endpoints have virtually no recourse or exit from the path-dependent modifications imposed by middleboxes. The theater ratio (0.6) is high because RFC 9293 is still formally maintained and referenced, but its actual prescriptive power over deployed networks is significantly diminished; much of its 'enforcement' is performative, masking the underlying divergence. The claimed type is Tangled Rope because there's a genuine coordination function (reliable byte stream) that middleboxes interfere with, but their operation also involves asymmetric extraction of control and data, requiring active enforcement of local policies.
 *
 * PERSPECTIVAL GAP:
 *   Middlebox operators perceive their actions as necessary for security and policy enforcement, viewing RFC 9293 as a flexible guideline. Endpoint users and application developers experience the same system as a coercive force that undermines the protocol's promises. Standards bodies struggle to reconcile the ideal with the reality, often documenting the divergence without being able to enforce the ideal.
 *
 * DIRECTIONALITY LOGIC:
 *   Middlebox operators are clear beneficiaries (d=0.0-0.1) as they gain control and data. Endpoint users and application developers are clear targets (d=0.9-1.0) as they bear the costs of non-standard behavior and loss of autonomy. Internet standards bodies are analytical observers (d=0.5) as they document the system without directly benefiting or being targeted by the middlebox operations.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification prevents mislabeling the current state of TCP as a pure 'Rope' (simple coordination) or 'Mountain' (natural law of network behavior). By identifying it as a Tangled Rope, it highlights the active extraction and suppression by middleboxes, which are often justified under the guise of security or policy coordination. The high theater ratio indicates that the RFC's continued 'authority' is largely performative, masking the underlying power dynamics. This analysis reveals that the mandate for end-to-end transparency has atrophied, replaced by a de facto regime of intermediary control.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    middlebox_necessity_vs_overreach,
    'To what extent are middlebox modifications genuinely necessary for network security and policy, versus being an overreach for control or surveillance?',
    'Independent audits of middlebox configurations, analysis of security incidents with and without specific middlebox functions, and regulatory oversight requiring justification for traffic modification.',
    'If overreach is dominant, the extractiveness and suppression metrics are fully justified, and the constraint leans more towards a Snare. If necessity is dominant, the coordination function is stronger, and it remains a Tangled Rope with a higher ''necessary'' extraction component.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(middlebox_necessity_vs_overreach, empirical, 'Distinguishing legitimate security functions from excessive control in middlebox operations.').

omega_variable(
    specification_authority_locus,
    'Is the ultimate authority for TCP behavior located in the RFC specification, or in the aggregate behavior of deployed network infrastructure?',
    'Observing how new RFCs are adopted and whether they successfully alter deployed middlebox behavior, or if middlebox behavior continues to dictate de facto standards.',
    'If authority is de facto with infrastructure, the ''strict_invariance_reading'' is foreclosed, and this ''middlebox_realism_reading'' is strongly validated. If RFCs can still effectively shape behavior, the ''strict_invariance_reading'' retains more validity.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(specification_authority_locus, conceptual, 'Ambiguity regarding where the true authority for TCP behavior resides.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (external barriers) or internalized (cognitive patterns that persist after barrier removal)?',
    'Post-exit suppression trajectory: if suppression persists after the extractive mechanism is removed (e.g., if users still avoid certain protocols even after middleboxes are reconfigured), reclassify as partially internalized.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests — the target carries the suppression with them after exit.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism in endpoint behavior.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(rfc9293_tcp_specification__middlebox_realism_reading, 1981, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(rfc9_tr_t1981, rfc9293_tcp_specification__middlebox_realism_reading, theater_ratio, 1981, 0.05).
narrative_ontology:measurement(rfc9_tr_t1990, rfc9293_tcp_specification__middlebox_realism_reading, theater_ratio, 1990, 0.15).
narrative_ontology:measurement(rfc9_tr_t2000, rfc9293_tcp_specification__middlebox_realism_reading, theater_ratio, 2000, 0.3).
narrative_ontology:measurement(rfc9_tr_t2010, rfc9293_tcp_specification__middlebox_realism_reading, theater_ratio, 2010, 0.45).
narrative_ontology:measurement(rfc9_tr_t2024, rfc9293_tcp_specification__middlebox_realism_reading, theater_ratio, 2024, 0.6).

% Extraction over time
narrative_ontology:measurement(rfc9_be_t1981, rfc9293_tcp_specification__middlebox_realism_reading, base_extractiveness, 1981, 0.1).
narrative_ontology:measurement(rfc9_be_t1990, rfc9293_tcp_specification__middlebox_realism_reading, base_extractiveness, 1990, 0.3).
narrative_ontology:measurement(rfc9_be_t2000, rfc9293_tcp_specification__middlebox_realism_reading, base_extractiveness, 2000, 0.55).
narrative_ontology:measurement(rfc9_be_t2010, rfc9293_tcp_specification__middlebox_realism_reading, base_extractiveness, 2010, 0.7).
narrative_ontology:measurement(rfc9_be_t2024, rfc9293_tcp_specification__middlebox_realism_reading, base_extractiveness, 2024, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(rfc9_su_t1981, rfc9293_tcp_specification__middlebox_realism_reading, suppression_requirement, 1981, 0.1).
narrative_ontology:measurement(rfc9_su_t1990, rfc9293_tcp_specification__middlebox_realism_reading, suppression_requirement, 1990, 0.35).
narrative_ontology:measurement(rfc9_su_t2000, rfc9293_tcp_specification__middlebox_realism_reading, suppression_requirement, 2000, 0.6).
narrative_ontology:measurement(rfc9_su_t2010, rfc9293_tcp_specification__middlebox_realism_reading, suppression_requirement, 2010, 0.75).
narrative_ontology:measurement(rfc9_su_t2024, rfc9293_tcp_specification__middlebox_realism_reading, suppression_requirement, 2024, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(rfc9293_tcp_specification__middlebox_realism_reading, global_infrastructure).
narrative_ontology:affects_constraint(rfc9293_tcp_specification__middlebox_realism_reading, internet_end_to_end_principle).
narrative_ontology:affects_constraint(rfc9293_tcp_specification__middlebox_realism_reading, net_neutrality_regulations).
narrative_ontology:affects_constraint(rfc9293_tcp_specification__middlebox_realism_reading, application_layer_innovation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
