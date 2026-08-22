% ============================================================================
% CONSTRAINT STORY: ietf_openness_commitment__capture_substrate_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ietf_openness_commitment__capture_substrate_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: ietf_openness_commitment__capture_substrate_reading
 *   human_readable: IETF Openness Commitment — Capture Substrate Reading
 *   domain: technology_governance/internet_standards/institutional_economics
 *
 * SUMMARY:
 *   The IETF's openness commitment — 'rough consensus and running code' —
 *   presents as a coordination substrate for interoperable internet
 *   standards. In this reading, the process functions as a tangled rope: it
 *   genuinely coordinates interoperability (the rope function) but the
 *   coordination substrate has been captured as a gatekeeping mechanism where
 *   resource-rich platform operators convert engineering capacity and patent
 *   portfolios into standardized proprietary extensions. Large operators
 *   (Google, Microsoft, Cloudflare, Apple) benefit by having their
 *   implementations become de facto mandatory through standardization, while
 *   small implementers and users bear the costs of licensing, complexity, and
 *   lock-in. The constraint requires active enforcement (patent licensing
 *   commitments, working group chair decisions, appeals process) and extracts
 *   asymmetrically.
 *
 * KEY AGENTS:
 *   - large_platform_operators: Primary beneficiaries (institutional/arbitrage) — contribute engineering resources, hold essential patents, drive standardization of their implementations
 *   - small_implementers: Primary victims (moderate/constrained) — must implement standards they had little power to shape, face patent licensing uncertainty, lack resources to influence process
 *   - end_users: Victims (organized/constrained) — bear downstream costs of reduced competition, proprietary lock-in, and degraded interoperability when extensions fragment the commons
 *   - independent_standards_contributors: Victims (moderate/trapped) — contribute technical work but see outcomes steered by resource-advantaged actors; exit requires abandoning professional identity
 *   - patent_holding_entities: Beneficiaries (powerful/arbitrage) — monetize essential patents declared during standardization; capture value from the coordination function
 *   - standards_process_observers: Observers (analytical/analytical) — academic researchers, civil society, regulators who analyze capture dynamics but lack standing in the process
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ietf_openness_commitment__capture_substrate_reading, 0.48).
domain_priors:suppression_score(ietf_openness_commitment__capture_substrate_reading, 0.52).
domain_priors:theater_ratio(ietf_openness_commitment__capture_substrate_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ietf_openness_commitment__capture_substrate_reading, extractiveness, 0.48).
narrative_ontology:constraint_metric(ietf_openness_commitment__capture_substrate_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(ietf_openness_commitment__capture_substrate_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ietf_openness_commitment__capture_substrate_reading, accessibility_collapse, 0.38).
narrative_ontology:constraint_metric(ietf_openness_commitment__capture_substrate_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ietf_openness_commitment__capture_substrate_reading, tangled_rope).
narrative_ontology:human_readable(ietf_openness_commitment__capture_substrate_reading, "IETF Openness Commitment — Capture Substrate Reading").
narrative_ontology:topic_domain(ietf_openness_commitment__capture_substrate_reading, "technology_governance/internet_standards/institutional_economics").

domain_priors:requires_active_enforcement(ietf_openness_commitment__capture_substrate_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ietf_openness_commitment__capture_substrate_reading, '69ce11b0-7cb8-47f5-8c1e-1a960a49311a').
narrative_ontology:cs_kernel_codification('69ce11b0-7cb8-47f5-8c1e-1a960a49311a', formalized).
narrative_ontology:cs_authority_grounding('69ce11b0-7cb8-47f5-8c1e-1a960a49311a', practice).
narrative_ontology:cs_interpretation_layer_present('69ce11b0-7cb8-47f5-8c1e-1a960a49311a').
narrative_ontology:cs_reading_relation('69ce11b0-7cb8-47f5-8c1e-1a960a49311a', ietf_openness_commitment__commons_stewardship_reading, coexists_with).
narrative_ontology:cs_reading_relation('69ce11b0-7cb8-47f5-8c1e-1a960a49311a', ietf_openness_commitment__legitimacy_erosion_reading, influences).
narrative_ontology:cs_axiom('69ce11b0-7cb8-47f5-8c1e-1a960a49311a', foundational, standards_process_captured_by_resource_advantage).
narrative_ontology:cs_axiom_status(standards_process_captured_by_resource_advantage, holdable).
narrative_ontology:cs_axiom_grounding('69ce11b0-7cb8-47f5-8c1e-1a960a49311a', standards_process_captured_by_resource_advantage, empirically_contingent).
narrative_ontology:cs_axiom('69ce11b0-7cb8-47f5-8c1e-1a960a49311a', secondary, openness_commitment_legitimates_gatekeeping).
narrative_ontology:cs_axiom_status(openness_commitment_legitimates_gatekeeping, holdable).
narrative_ontology:cs_axiom_grounding('69ce11b0-7cb8-47f5-8c1e-1a960a49311a', openness_commitment_legitimates_gatekeeping, conventional).
narrative_ontology:cs_reference_frame('69ce11b0-7cb8-47f5-8c1e-1a960a49311a', open_consensus_coordination).
narrative_ontology:cs_drift_state('69ce11b0-7cb8-47f5-8c1e-1a960a49311a', platform_dominance_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('69ce11b0-7cb8-47f5-8c1e-1a960a49311a', '').
narrative_ontology:cs_kernel_id(ietf_openness_commitment__capture_substrate_reading, ietf_openness_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ietf_openness_commitment__capture_substrate_reading, large_platform_operators).
narrative_ontology:constraint_beneficiary(ietf_openness_commitment__capture_substrate_reading, patent_holding_entities).
narrative_ontology:constraint_victim(ietf_openness_commitment__capture_substrate_reading, small_implementers).
narrative_ontology:constraint_victim(ietf_openness_commitment__capture_substrate_reading, end_users).
narrative_ontology:constraint_victim(ietf_openness_commitment__capture_substrate_reading, independent_standards_contributors).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(ietf_openness_commitment__capture_substrate_reading, independent_standards_contributors).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Contribute disproportionate engineering resources to standards development, hold essential patent portfolios, and drive standardization of their proprietary implementations. They collect rents when their implementations become mandatory through standardization, control the implementation agenda, and can fork or build parallel infrastructure if the process resists. Their participation shapes outcomes; the process legitimates their gatekeeping.
narrative_ontology:constraint_stakeholder(ietf_openness_commitment__capture_substrate_reading, large_platform_operators, beneficiary,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(ietf_openness_commitment__capture_substrate_reading, large_platform_operators, agenda_setter).

% Must implement standards they had little power to shape, face patent licensing uncertainty and FRAND ambiguity, and lack resources to sustain effective participation in working groups. Non-compliance means incompatibility with the dominant ecosystem. Their exit is constrained by market necessity — they must implement to survive, but implementation locks them into the standardized proprietary extensions.
narrative_ontology:constraint_stakeholder(ietf_openness_commitment__capture_substrate_reading, small_implementers, payer,
    moderate, biographical, constrained, global).

% Bear downstream costs of reduced competition, proprietary lock-in, and degraded interoperability when standardized extensions fragment the commons. They have no direct standing in the standards process and cannot opt out of the infrastructure their applications and devices depend on. Their exit is constrained by the ubiquity of standardized protocols.
narrative_ontology:constraint_stakeholder(ietf_openness_commitment__capture_substrate_reading, end_users, payer,
    organized, biographical, constrained, global).

% Contribute technical expertise and editorial labor to standards development, gaining professional reputation and network access (beneficiary aspect). But they see outcomes steered by resource-advantaged actors, their objections overridden by consensus determinations that correlate with contributor affiliation, and their professional identity fused to the standards process — leaving it means abandoning a career-defining commitment.
narrative_ontology:constraint_stakeholder(ietf_openness_commitment__capture_substrate_reading, independent_standards_contributors, payer,
    moderate, biographical, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(ietf_openness_commitment__capture_substrate_reading, independent_standards_contributors, beneficiary).

% Monetize essential patents declared during standardization through FRAND licensing. They capture value from the coordination function by holding patents that become essential to the standard. Their exit is arbitrage-grade — they can license, litigate, or sell portfolios independent of the standards process.
narrative_ontology:constraint_stakeholder(ietf_openness_commitment__capture_substrate_reading, patent_holding_entities, beneficiary,
    powerful, generational, arbitrage, global).

% Academic researchers, civil society organizations, and regulators who analyze capture dynamics, document participation asymmetries, and advocate for process reforms. They lack standing in the IETF process (no voting rights, no implementation capacity) and cannot directly influence outcomes. Their role is analytical: they observe, measure, and publish.
narrative_ontology:constraint_stakeholder(ietf_openness_commitment__capture_substrate_reading, standards_process_observers, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(ietf_openness_commitment__capture_substrate_reading, large_platform_operators).
narrative_ontology:fixing_cost_class(ietf_openness_commitment__capture_substrate_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates interoperable internet protocol implementations across diverse vendors and platforms, enabling global communication infrastructure through shared technical specifications.
% TRANSFER_FUNCTION: Moves implementation burden, patent licensing costs, and ecosystem lock-in from large platform operators (who shape standards to match their implementations) to small implementers and end users (who must adopt the standardized proprietary extensions). Also moves standardization legitimacy and de facto mandate to proprietary approaches that would otherwise face market resistance.
% ABSENT_VOICES: Users in jurisdictions with limited internet infrastructure who bear disproportionate costs from proprietary extensions (bandwidth, compute, licensing). Future implementers not yet in the market. Civil society organizations excluded from working groups by resource and travel barriers. These voices would object to capture but are structurally absent from the consensus determination.
% DISAPPEARANCE_RATIONALE: If the IETF openness commitment vanished overnight, large operators would immediately formalize their proprietary protocols as de facto standards through market power, small implementers would lose even the nominal FRAND protections and participation rights they currently hold, and the coordination substrate would fragment into competing walled gardens. The world would rearrange into a less interoperable, more extractive equilibrium.
% FOUNDING_PROBLEM: Early internet standardization needed a lightweight, open process to coordinate interoperable protocols across academic, government, and early commercial networks without centralized control — the 'rough consensus and running code' ethos solved this by privileging working implementations over formal voting.
% FOUNDING_PROBLEM_CORROBORATION: The IETF leadership attests the founding problem is live (interoperability remains the core mission). Independent researchers (e.g., RFC 8280 authors, academic studies of standards capture) and civil society organizations (EFF, Article 19) attest the founding problem has been substantially captured — the process now serves to legitimate resource-driven gatekeeping. Competition authorities (EU DMA, US FTC scrutiny) corroborate the shifted-function reading through market investigations.
narrative_ontology:disappearance_verdict(ietf_openness_commitment__capture_substrate_reading, world_rearranges).
narrative_ontology:founding_problem_status(ietf_openness_commitment__capture_substrate_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ietf_openness_commitment__capture_substrate_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(ietf_openness_commitment__capture_substrate_reading, 'none', 1).
narrative_ontology:epsilon_provenance(ietf_openness_commitment__capture_substrate_reading, 0.48, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ietf_openness_commitment__capture_substrate_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(ietf_openness_commitment__capture_substrate_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(ietf_openness_commitment__capture_substrate_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.48) reflects moderate but growing extraction: the coordination function is real and valuable (interoperability), but the margin between coordination cost and extracted value has widened as platform operators standardize their proprietary stacks. Suppression (0.52) is moderate — alternatives are not formally banned but are structurally disadvantaged by participation asymmetry, patent thickets, and implementation complexity. Theater ratio (0.41) is significant: the 'rough consensus' process increasingly performs openness while outcomes are pre-shaped by resource advantage. Accessibility collapse (0.38) is moderate — alternatives exist but require disproportionate effort. Resistance (0.55) is notable: small implementers, civil society, and some regulators actively contest capture, but the process lacks effective countermeasures.
 *
 * PERSPECTIVAL GAP:
 *   From the large operator seat, the constraint is genuine coordination (rope-like): they invest heavily in the process and receive interoperability benefits. From the small implementer seat, the same constraint is extraction (snare-like): they must adopt standards shaped by others' interests. From the independent contributor seat, the constraint is a piton — the process performs openness while the substantive function has atrophied into resource-driven gatekeeping. The engine computes this seat divergence from the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Large platform operators are structural beneficiaries (d ~ 0.15): they collect rents from standardized proprietary extensions, control implementation agendas, and have arbitrage-grade exit (can fork or build parallel infrastructure). Patent-holding entities are secondary beneficiaries (d ~ 0.20). Small implementers are primary targets (d ~ 0.85): they pay licensing costs, bear implementation burden, and have constrained exit (must implement to be compatible). End users are diffuse targets (d ~ 0.75): they bear downstream costs but lack direct standing. Independent contributors are identity-locked targets (d ~ 0.80): professional identity fused to standards work makes exit unthinkable. Observers are analytical (d = 0.5).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (interoperable internet infrastructure) remains live but the arrangement has drifted: the openness commitment now primarily serves to legitimate capture rather than prevent it. The coordination function is real but subordinate to the extraction function. This is not pure extraction (snare) because the coordination substrate would collapse without the process — but it is not pure coordination (rope) because the benefits are asymmetrically captured. Tangled rope is the structurally honest classification.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_framing,
    'Does the IETF openness commitment function as a coordination substrate that large operators capture, or as a commons stewardship constraint that preserves interoperability?',
    'Comparative analysis of standardization outcomes where large operators pushed proprietary extensions vs. cases where the process rejected such extensions; trace resource flows to beneficiaries.',
    'If capture_substrate_reading is correct, the constraint is a tangled_rope with identifiable beneficiaries (large operators) and victims (small implementers/users). If commons_stewardship_reading holds, the constraint is closer to rope with diffuse beneficiaries and minimal extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_framing, conceptual, 'Commitment kernel framing ambiguity: capture substrate vs. commons stewardship').

omega_variable(
    resource_advantage_to_gatekeeping_mechanism,
    'Through what specific mechanisms does resource advantage (engineering headcount, patent portfolios, implementation capacity) translate into encoded gatekeeping in the standards process?',
    'Process-tracing of specific standardization efforts (e.g., QUIC, WebRTC, TLS 1.3, DOH) where large operators'' proposals became standards despite objections; measure participation asymmetry and outcome correlation.',
    'If resource advantage directly determines standardization outcomes, the extraction mechanism is structural and persistent. If outcomes correlate with technical merit independent of resources, the capture reading overstates extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(resource_advantage_to_gatekeeping_mechanism, empirical, 'Mechanism of resource advantage conversion to gatekeeping power').

omega_variable(
    suppression_vs_legitimate_process,
    'Is the measured suppression (0.52) structural exclusion of alternatives, or the legitimate friction of a consensus process that necessarily filters proposals?',
    'Compare rejection rates and revision cycles for proposals from large operators vs. small implementers; analyze whether rejected alternatives had viable technical paths that were foreclosed by process rather than merit.',
    'If suppression is structural exclusion, the constraint extracts by foreclosing alternatives. If suppression is legitimate filtering, the extraction component is lower and the coordination function dominates.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_vs_legitimate_process, conceptual, 'Structural exclusion vs. legitimate consensus friction ambiguity').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ietf_openness_commitment__capture_substrate_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ietf_tr_t0, ietf_openness_commitment__capture_substrate_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement(ietf_tr_t5, ietf_openness_commitment__capture_substrate_reading, theater_ratio, 5, 0.22).
narrative_ontology:measurement(ietf_tr_t10, ietf_openness_commitment__capture_substrate_reading, theater_ratio, 10, 0.28).
narrative_ontology:measurement(ietf_tr_t15, ietf_openness_commitment__capture_substrate_reading, theater_ratio, 15, 0.34).
narrative_ontology:measurement(ietf_tr_t20, ietf_openness_commitment__capture_substrate_reading, theater_ratio, 20, 0.38).
narrative_ontology:measurement(ietf_tr_t25, ietf_openness_commitment__capture_substrate_reading, theater_ratio, 25, 0.41).

% Extraction over time
narrative_ontology:measurement(ietf_be_t0, ietf_openness_commitment__capture_substrate_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(ietf_be_t5, ietf_openness_commitment__capture_substrate_reading, base_extractiveness, 5, 0.32).
narrative_ontology:measurement(ietf_be_t10, ietf_openness_commitment__capture_substrate_reading, base_extractiveness, 10, 0.38).
narrative_ontology:measurement(ietf_be_t15, ietf_openness_commitment__capture_substrate_reading, base_extractiveness, 15, 0.43).
narrative_ontology:measurement(ietf_be_t20, ietf_openness_commitment__capture_substrate_reading, base_extractiveness, 20, 0.46).
narrative_ontology:measurement(ietf_be_t25, ietf_openness_commitment__capture_substrate_reading, base_extractiveness, 25, 0.48).

% Suppression requirement over time
narrative_ontology:measurement(ietf_su_t0, ietf_openness_commitment__capture_substrate_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(ietf_su_t5, ietf_openness_commitment__capture_substrate_reading, suppression_requirement, 5, 0.38).
narrative_ontology:measurement(ietf_su_t10, ietf_openness_commitment__capture_substrate_reading, suppression_requirement, 10, 0.42).
narrative_ontology:measurement(ietf_su_t15, ietf_openness_commitment__capture_substrate_reading, suppression_requirement, 15, 0.47).
narrative_ontology:measurement(ietf_su_t20, ietf_openness_commitment__capture_substrate_reading, suppression_requirement, 20, 0.5).
narrative_ontology:measurement(ietf_su_t25, ietf_openness_commitment__capture_substrate_reading, suppression_requirement, 25, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ietf_openness_commitment__capture_substrate_reading, information_standard).
narrative_ontology:boltzmann_floor_override(ietf_openness_commitment__capture_substrate_reading, 0.03).
narrative_ontology:affects_constraint(ietf_openness_commitment__capture_substrate_reading, web_platform_standardization).
narrative_ontology:affects_constraint(ietf_openness_commitment__capture_substrate_reading, patent_policy_governance).
narrative_ontology:affects_constraint(ietf_openness_commitment__capture_substrate_reading, interoperability_mandates).

% DUAL FORMULATION NOTE:
% Part of the ietf_openness_commitment constraint family with commons_stewardship_reading and legitimacy_erosion_reading. This reading (capture_substrate) has moderate extractiveness (0.48) and tangible beneficiaries (large operators). The commons_stewardship reading would author lower extractiveness (~0.15) and diffuse beneficiaries. The legitimacy_erosion reading would author higher suppression (~0.70) and process-focused victims. All three share the same kernel but instantiate different constraints with different ε values.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(ietf_openness_commitment__capture_substrate_reading, institutional, 0.15).
constraint_indexing:directionality_override(ietf_openness_commitment__capture_substrate_reading, moderate, 0.82).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
