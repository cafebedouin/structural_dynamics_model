% ============================================================================
% CONSTRAINT STORY: ietf_openness_commitment__commons_stewardship_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ietf_openness_commitment__commons_stewardship_reading, []).

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
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: ietf_openness_commitment__commons_stewardship_reading
 *   human_readable: IETF Openness Commitment — Commons Stewardship Reading
 *   domain: technology_governance/internet_standards/institutional_economics
 *
 * SUMMARY:
 *   The IETF's openness commitment — that Internet standards shall be open
 *   for anyone to implement, use, and improve without permission or royalty —
 *   is read here as a mountain-like constraint: a structural feature of the
 *   protocol commons that preserves interoperability as a non-rival good.
 *   This reading holds that the standards process, governed by rough
 *   consensus and running code, produces specifications that constrain large
 *   and small implementers equally toward the interoperability attractor. No
 *   structural beneficiary class extracts from the arrangement; the
 *   constraint's persistence derives from the protocol physics of
 *   interoperability itself, not from enforcement machinery. The ε = 0.12
 *   reflects the marginal cost of participating in the standards process
 *   (mailing list attention, document review, interop testing) — a
 *   coordination cost, not an extractive transfer. This is one of three
 *   readings of the contested kernel 'ietf_openness_commitment'; the sibling
 *   readings (capture_substrate_reading, legitimacy_erosion_reading)
 *   instantiate different constraints with higher ε and declared
 *   beneficiaries/victims.
 *
 * KEY AGENTS:
 *   - standards_editors: agenda_setter (expertise/analytical) — steward the process, no extractive benefit
 *   - large_implementers: payer/beneficiary (institutional/organized) — bear participation costs, gain interoperability
 *   - small_implementers: beneficiary (moderate/organized) — gain interoperability at lower absolute cost
 *   - protocol_users: beneficiary (analytical/universal) — gain interoperable ecosystem without direct participation
 *   - standards_observers: observer (analytical) — track process integrity
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ietf_openness_commitment__commons_stewardship_reading, 0.12).
domain_priors:suppression_score(ietf_openness_commitment__commons_stewardship_reading, 0.08).
domain_priors:theater_ratio(ietf_openness_commitment__commons_stewardship_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ietf_openness_commitment__commons_stewardship_reading, extractiveness, 0.12).
narrative_ontology:constraint_metric(ietf_openness_commitment__commons_stewardship_reading, suppression_requirement, 0.08).
narrative_ontology:constraint_metric(ietf_openness_commitment__commons_stewardship_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ietf_openness_commitment__commons_stewardship_reading, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(ietf_openness_commitment__commons_stewardship_reading, resistance, 0.03).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ietf_openness_commitment__commons_stewardship_reading, mountain).
narrative_ontology:human_readable(ietf_openness_commitment__commons_stewardship_reading, "IETF Openness Commitment — Commons Stewardship Reading").
narrative_ontology:topic_domain(ietf_openness_commitment__commons_stewardship_reading, "technology_governance/internet_standards/institutional_economics").

domain_priors:emerges_naturally(ietf_openness_commitment__commons_stewardship_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ietf_openness_commitment__commons_stewardship_reading, '35c8a448-6c85-41d6-8f5e-80e9abd4aee6').
narrative_ontology:cs_kernel_codification('35c8a448-6c85-41d6-8f5e-80e9abd4aee6', fixed_text).
narrative_ontology:cs_authority_grounding('35c8a448-6c85-41d6-8f5e-80e9abd4aee6', expertise).
narrative_ontology:cs_interpretation_layer_present('35c8a448-6c85-41d6-8f5e-80e9abd4aee6').
narrative_ontology:cs_reading_relation('35c8a448-6c85-41d6-8f5e-80e9abd4aee6', ietf_openness_commitment__capture_substrate_reading, coexists_with).
narrative_ontology:cs_reading_relation('35c8a448-6c85-41d6-8f5e-80e9abd4aee6', ietf_openness_commitment__legitimacy_erosion_reading, influences).
narrative_ontology:cs_axiom('35c8a448-6c85-41d6-8f5e-80e9abd4aee6', foundational, open_standards_are_public_infrastructure).
narrative_ontology:cs_axiom_status(open_standards_are_public_infrastructure, holdable).
narrative_ontology:cs_axiom_grounding('35c8a448-6c85-41d6-8f5e-80e9abd4aee6', open_standards_are_public_infrastructure, conventional).
narrative_ontology:cs_axiom('35c8a448-6c85-41d6-8f5e-80e9abd4aee6', foundational, interoperability_is_non_rival_good).
narrative_ontology:cs_axiom_status(interoperability_is_non_rival_good, holdable).
narrative_ontology:cs_axiom_grounding('35c8a448-6c85-41d6-8f5e-80e9abd4aee6', interoperability_is_non_rival_good, empirically_contingent).
narrative_ontology:cs_reference_frame('35c8a448-6c85-41d6-8f5e-80e9abd4aee6', rfc_3935_mission).
narrative_ontology:cs_drift_state('35c8a448-6c85-41d6-8f5e-80e9abd4aee6', contemporary_complexity_era, gap(practice_drift, minor, true)).
narrative_ontology:cs_created_at('35c8a448-6c85-41d6-8f5e-80e9abd4aee6', '').
narrative_ontology:cs_kernel_id(ietf_openness_commitment__commons_stewardship_reading, ietf_openness_commitment).

% --- Structural relationships ---
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(ietf_openness_commitment__commons_stewardship_reading, large_implementers).
narrative_ontology:constraint_beneficiary(ietf_openness_commitment__commons_stewardship_reading, small_implementers).
narrative_ontology:constraint_beneficiary(ietf_openness_commitment__commons_stewardship_reading, protocol_users).
narrative_ontology:constraint_victim(ietf_openness_commitment__commons_stewardship_reading, large_implementers).
narrative_ontology:constraint_vindicates(ietf_openness_commitment__commons_stewardship_reading, open_standards_as_public_infrastructure).
narrative_ontology:constraint_vindicates(ietf_openness_commitment__commons_stewardship_reading, interoperability_as_non_rival_good).
narrative_ontology:constraint_vindicates(ietf_openness_commitment__commons_stewardship_reading, rough_consensus_and_running_code).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Steward the standards process through rough consensus and running code. They edit documents, manage working groups, and adjudicate technical disputes. They derive no extractive benefit from the constraint; their authority derives from demonstrated expertise and community trust. Exit is analytical — they can disengage from the role without personal cost.
narrative_ontology:constraint_stakeholder(ietf_openness_commitment__commons_stewardship_reading, standards_editors, agenda_setter,
    analytical, biographical, analytical, universal).

% Invest heavily in standards participation (engineering time, patent portfolios, interop testing). They bear the highest absolute costs but also gain the most from a stable interoperable platform. Their exit is mobile — they could fork or withdraw, but the network effects of the open platform make exit costly. They are structurally symmetric: they pay participation costs and receive interoperability benefits in proportion to their scale.
narrative_ontology:constraint_stakeholder(ietf_openness_commitment__commons_stewardship_reading, large_implementers, payer,
    institutional, generational, mobile, universal).
narrative_ontology:stakeholder_secondary_role(ietf_openness_commitment__commons_stewardship_reading, large_implementers, beneficiary).

% Gain interoperability at lower absolute participation cost than large implementers. The open constraint lowers barriers to entry — they can implement from public specifications without licensing fees or permission. Their exit is constrained by the same network effects, but they have less invested capital at stake.
narrative_ontology:constraint_stakeholder(ietf_openness_commitment__commons_stewardship_reading, small_implementers, beneficiary,
    moderate, biographical, constrained, universal).

% End users of the interoperable ecosystem (developers, organizations, individuals). They gain the full benefit of open interoperability without bearing any direct participation costs. Their exit is arbitrage-grade — they can switch between interoperable implementations freely.
narrative_ontology:constraint_stakeholder(ietf_openness_commitment__commons_stewardship_reading, protocol_users, beneficiary,
    powerless, immediate, arbitrage, universal).

% Academic researchers, civil society groups, and regulatory observers who track the standards process for integrity, inclusivity, and public interest alignment. They neither pay nor collect; they observe.
narrative_ontology:constraint_stakeholder(ietf_openness_commitment__commons_stewardship_reading, standards_observers, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(ietf_openness_commitment__commons_stewardship_reading, diffuse).
narrative_ontology:fixing_cost_class(ietf_openness_commitment__commons_stewardship_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Produces and maintains technical specifications that enable global interoperability without central permission — a non-rival, non-excludable good that any implementer can build to and any user can benefit from.
% TRANSFER_FUNCTION: Moves participation effort (engineering review, interop testing, document authorship) from all implementers toward the commons, and returns interoperability benefits to all implementers and users. No net transfer of value to a beneficiary class; the arrangement is positive-sum.
% ABSENT_VOICES: Would-be implementers in jurisdictions with restricted Internet access or export controls on cryptography — they are excluded not by the openness constraint but by external political constraints. The openness constraint itself has no excluded implementers by design.
% DISAPPEARANCE_RATIONALE: If the openness commitment vanished overnight, the interoperable Internet would not immediately collapse — but the assurance that any implementer can build to public specifications without permission would be lost. Over time, the ecosystem would likely fragment into licensed, proprietary, or permissioned silos, and the non-rival character of interoperability would erode. The world rearranges because the constraint is the governance substrate that keeps the interoperability attractor stable.
% FOUNDING_PROBLEM: Early Internet development required a way to coordinate protocol evolution across independent, competing, and geographically distributed implementations without creating a central gatekeeper who could extract rents or block competition.
% FOUNDING_PROBLEM_CORROBORATION: The IETF's own mission statement and RFC 3935 attest the problem is live. Independent corroboration: the continued growth of open protocol ecosystems (HTTP, TLS, QUIC, Matrix, ActivityPub) demonstrates the problem persists — new interoperability challenges emerge as the network expands. No party outside the standards community disputes that interoperability remains a coordination problem; the contest is over whether the current process solves it without capture.
narrative_ontology:disappearance_verdict(ietf_openness_commitment__commons_stewardship_reading, world_rearranges).
narrative_ontology:founding_problem_status(ietf_openness_commitment__commons_stewardship_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ietf_openness_commitment__commons_stewardship_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(ietf_openness_commitment__commons_stewardship_reading, 'none', 1).
narrative_ontology:epsilon_provenance(ietf_openness_commitment__commons_stewardship_reading, 0.12, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ietf_openness_commitment__commons_stewardship_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(ietf_openness_commitment__commons_stewardship_reading, ExtMetricName, E),
    domain_priors:suppression_score(ietf_openness_commitment__commons_stewardship_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(ietf_openness_commitment__commons_stewardship_reading),
    narrative_ontology:constraint_metric(ietf_openness_commitment__commons_stewardship_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(ietf_openness_commitment__commons_stewardship_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(ietf_openness_commitment__commons_stewardship_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The metrics describe a constraint with near-zero extraction (0.12 = participation cost), negligible suppression (0.08 = no coercion beyond the logic of interop), near-zero theater (0.05), near-total accessibility collapse for alternatives (0.92 = no viable alternative to open interoperability for global networking), and near-zero resistance (0.03 = no organized opposition to openness itself). These are the structural signatures of a mountain: the constraint would persist regardless of who defends it, and no party collects from its operation. The vindicated propositions (open_standards_as_public_infrastructure, interoperability_as_non_rival_good, rough_consensus_and_running_code) are doctrines the constraint's operation vindicates — they are not beneficiaries. The slight upward drift in extractiveness and theater over 40 time units reflects the growing complexity of the standards surface, not a shift in the constraint's nature.
 *
 * PERSPECTIVAL GAP:
 *   All seats compute as mountain or near-mountain: even large implementers, who bear the highest absolute participation costs, experience the constraint as a coordination substrate they depend on. The engine will compute per-seat types from the structural data; the divergence between this reading's computed types and the capture_substrate_reading's computed types is the measurement of the kernel's contested structure.
 *
 * DIRECTIONALITY LOGIC:
 *   No beneficiary class extracts from the arrangement. Large and small implementers both bear participation costs proportional to their engagement, and both gain interoperability benefits that are non-rival and non-excludable. The directionality for all implementer seats is near-symmetric (d ≈ 0.5); protocol users are beneficiaries (d ≈ 0.0) but bear no costs. The standards editors are agenda_setters with analytical exit — they steward the process but do not capture rents from it.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — achieving interoperable networking without central permission — remains live. The arrangement has not outlived its function; the interoperability attractor is the ongoing coordination target. Mandatrophy does not apply because the constraint's function has not atrophied — the protocol physics that make interoperability a non-rival good are persistent.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_structure,
    'Is this constraint one reading of a contested kernel (ietf_openness_commitment), and what structural elements distinguish it from the capture_substrate_reading and legitimacy_erosion_reading?',
    'Compare ε values, beneficiary/victim structures, and authority_grounding across the three readings. This reading instantiates low ε (0.12), no beneficiary class, and authority_grounding:expertise. The capture_substrate_reading would instantiate higher ε, declared beneficiaries (large implementers), and authority_grounding:extraction. The legitimacy_erosion_reading would instantiate contested authority_grounding:distributed and moderate ε.',
    'If the three readings author substantially different structural profiles, the kernel is confirmed as contested and each reading is a distinct constraint. If profiles converge, the contest may be rhetorical rather than structural.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_structure, conceptual, 'Structural differentiation of this reading from sibling readings of the ietf_openness_commitment kernel').

omega_variable(
    natural_law_vs_constructed_governance,
    'Is the interoperability produced by open standards a genuine natural-law-like property of protocol physics (a mountain), or a continuously maintained governance achievement (a rope/scaffold)?',
    'Track fork survival rates and interoperability decay when governance attention withdraws. If standards interoperate without active IETF maintenance (e.g., legacy RFCs still in operation decades later), the mountain claim strengthens. If interoperability requires continuous stewardship, the coordination function is ongoing.',
    'If interoperability is self-sustaining once encoded, the mountain classification holds. If it requires active stewardship, the constraint is at minimum a rope, and the claimed mountain type is a false summit.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_governance, empirical, 'Whether interoperability is a natural-law-like attractor or a maintained governance achievement').

omega_variable(
    resource_advantage_translation_boundary,
    'At what point does resource advantage in the standards process translate into encoded gatekeeping that structurally benefits large implementers?',
    'Measure specification complexity growth, IPR disclosure patterns, and implementation divergence in standards where large implementers held editorial control vs. community-driven standards. Correlate with extractiveness metrics for capture_substrate_reading.',
    'If resource advantage systematically encodes gatekeeping, the commons_stewardship reading''s ''no beneficiary class'' claim is false for those standards — the kernel reading would be the capture_substrate_reading instead. This omega marks the boundary condition between readings.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(resource_advantage_translation_boundary, empirical, 'Threshold where resource advantage becomes structural extraction in standards governance').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ietf_openness_commitment__commons_stewardship_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ietf_openness_commons_tr_t0, ietf_openness_commitment__commons_stewardship_reading, theater_ratio, 0, 0.03).
narrative_ontology:measurement(ietf_openness_commons_tr_t10, ietf_openness_commitment__commons_stewardship_reading, theater_ratio, 10, 0.03).
narrative_ontology:measurement(ietf_openness_commons_tr_t20, ietf_openness_commitment__commons_stewardship_reading, theater_ratio, 20, 0.04).
narrative_ontology:measurement(ietf_openness_commons_tr_t30, ietf_openness_commitment__commons_stewardship_reading, theater_ratio, 30, 0.04).
narrative_ontology:measurement(ietf_openness_commons_tr_t40, ietf_openness_commitment__commons_stewardship_reading, theater_ratio, 40, 0.05).

% Extraction over time
narrative_ontology:measurement(ietf_openness_commons_be_t0, ietf_openness_commitment__commons_stewardship_reading, base_extractiveness, 0, 0.08).
narrative_ontology:measurement(ietf_openness_commons_be_t10, ietf_openness_commitment__commons_stewardship_reading, base_extractiveness, 10, 0.09).
narrative_ontology:measurement(ietf_openness_commons_be_t20, ietf_openness_commitment__commons_stewardship_reading, base_extractiveness, 20, 0.1).
narrative_ontology:measurement(ietf_openness_commons_be_t30, ietf_openness_commitment__commons_stewardship_reading, base_extractiveness, 30, 0.11).
narrative_ontology:measurement(ietf_openness_commons_be_t40, ietf_openness_commitment__commons_stewardship_reading, base_extractiveness, 40, 0.12).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(ietf_openness_commitment__commons_stewardship_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ietf_openness_commitment__commons_stewardship_reading, information_standard).
narrative_ontology:boltzmann_floor_override(ietf_openness_commitment__commons_stewardship_reading, 0.02).
narrative_ontology:affects_constraint(ietf_openness_commitment__commons_stewardship_reading, ietf_openness_commitment__capture_substrate_reading).
narrative_ontology:affects_constraint(ietf_openness_commitment__commons_stewardship_reading, ietf_openness_commitment__legitimacy_erosion_reading).

% DUAL FORMULATION NOTE:
% This is one of three constraint stories decomposing the ietf_openness_commitment kernel. The commons_stewardship_reading instantiates the mountain interpretation (low ε, no beneficiaries, expertise-grounded). The capture_substrate_reading instantiates the extraction interpretation (higher ε, large_implementers as beneficiaries, extraction-grounded). The legitimacy_erosion_reading instantiates the contested-governance interpretation (moderate ε, distributed authority, process vulnerability). All three share the same referent (the IETF openness commitment) but author different structural profiles — per the ε-invariance principle, they are distinct constraints linked by network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
