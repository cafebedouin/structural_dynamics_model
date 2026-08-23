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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: ietf_openness_commitment__capture_substrate_reading
 *   human_readable: IETF Openness Commitment — Capture Substrate Reading
 *   domain: technology_governance/internet_standards/institutional_economics
 *
 * SUMMARY:
 *   The IETF's openness commitment — its foundational claim that anyone can
 *   participate, anyone can implement, and decisions are made by rough
 *   consensus on technical merit — functions as a coordination substrate for
 *   internet protocol development. This reading (capture_substrate_reading)
 *   argues that resource advantages held by large platform operators (browser
 *   vendors, cloud providers, mobile OS vendors) have systematically
 *   translated into encoded gatekeeping within this open process. The
 *   mechanism: sustained engineering investment in standards participation
 *   lets these actors shape specifications to align with their proprietary
 *   implementations; the resulting specifications then function as de facto
 *   requirements that competing implementations must match. The openness
 *   commitment itself provides the legitimacy cover — the process is open, so
 *   the outcomes are presumed fair. But the cost of meaningful participation
 *   filters for resource-advantaged actors, and the rough-consensus model
 *   weights implementation leverage. Small implementers and users bear the
 *   costs: tracking complex specifications driven by platform roadmaps,
 *   implementing proprietary extensions that become mandatory for
 *   compatibility, and losing choice as the ecosystem consolidates around
 *   platform-controlled infrastructure. The constraint is a tangled rope
 *   because it retains genuine coordination function (interoperable protocols
 *   do get produced) while simultaneously operating as asymmetric extraction
 *   (value and control flow toward resource-advantaged platforms).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ietf_openness_commitment__capture_substrate_reading, 0.58).
domain_priors:suppression_score(ietf_openness_commitment__capture_substrate_reading, 0.42).
domain_priors:theater_ratio(ietf_openness_commitment__capture_substrate_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ietf_openness_commitment__capture_substrate_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(ietf_openness_commitment__capture_substrate_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(ietf_openness_commitment__capture_substrate_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ietf_openness_commitment__capture_substrate_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(ietf_openness_commitment__capture_substrate_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ietf_openness_commitment__capture_substrate_reading, tangled_rope).
narrative_ontology:human_readable(ietf_openness_commitment__capture_substrate_reading, "IETF Openness Commitment — Capture Substrate Reading").
narrative_ontology:topic_domain(ietf_openness_commitment__capture_substrate_reading, "technology_governance/internet_standards/institutional_economics").

domain_priors:requires_active_enforcement(ietf_openness_commitment__capture_substrate_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ietf_openness_commitment__capture_substrate_reading, '2b5bc9f8-1b03-4f21-8a7e-0fe4ad875051').
narrative_ontology:cs_kernel_codification('2b5bc9f8-1b03-4f21-8a7e-0fe4ad875051', formalized).
narrative_ontology:cs_authority_grounding('2b5bc9f8-1b03-4f21-8a7e-0fe4ad875051', practice).
narrative_ontology:cs_interpretation_layer_present('2b5bc9f8-1b03-4f21-8a7e-0fe4ad875051').
narrative_ontology:cs_reading_relation('2b5bc9f8-1b03-4f21-8a7e-0fe4ad875051', ietf_openness_commitment__commons_stewardship_reading, coexists_with).
narrative_ontology:cs_reading_relation('2b5bc9f8-1b03-4f21-8a7e-0fe4ad875051', ietf_openness_commitment__legitimacy_erosion_reading, influences).
narrative_ontology:cs_axiom('2b5bc9f8-1b03-4f21-8a7e-0fe4ad875051', foundational, resource_advantage_encodes_gatekeeping).
narrative_ontology:cs_axiom_status(resource_advantage_encodes_gatekeeping, holdable).
narrative_ontology:cs_axiom_grounding('2b5bc9f8-1b03-4f21-8a7e-0fe4ad875051', resource_advantage_encodes_gatekeeping, instrumental).
narrative_ontology:cs_axiom('2b5bc9f8-1b03-4f21-8a7e-0fe4ad875051', secondary, open_process_masks_proprietary_capture).
narrative_ontology:cs_axiom_status(open_process_masks_proprietary_capture, holdable).
narrative_ontology:cs_axiom_grounding('2b5bc9f8-1b03-4f21-8a7e-0fe4ad875051', open_process_masks_proprietary_capture, empirically_contingent).
narrative_ontology:cs_reference_frame('2b5bc9f8-1b03-4f21-8a7e-0fe4ad875051', rough_consensus_running_code).
narrative_ontology:cs_drift_state('2b5bc9f8-1b03-4f21-8a7e-0fe4ad875051', platform_centralization_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('2b5bc9f8-1b03-4f21-8a7e-0fe4ad875051', '').
narrative_ontology:cs_kernel_id(ietf_openness_commitment__capture_substrate_reading, ietf_openness_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ietf_openness_commitment__capture_substrate_reading, large_platform_operators).
narrative_ontology:constraint_beneficiary(ietf_openness_commitment__capture_substrate_reading, standards_editors).
narrative_ontology:constraint_victim(ietf_openness_commitment__capture_substrate_reading, small_implementers).
narrative_ontology:constraint_victim(ietf_openness_commitment__capture_substrate_reading, end_users).
narrative_ontology:constraint_vindicates(ietf_openness_commitment__capture_substrate_reading, open_process_legitimacy).
narrative_ontology:constraint_vindicates(ietf_openness_commitment__capture_substrate_reading, running_code_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Major technology platforms (browser vendors, cloud providers, mobile OS vendors) invest heavily in standards participation. They contribute engineering resources that drive specification development, but also shape specifications to align with their proprietary implementations. Their resource advantage lets them encode preferences as de facto requirements, creating gatekeeping effects where competing implementations must match their behavior to be compatible. They collect network effects and lock-in value from this alignment.
narrative_ontology:constraint_stakeholder(ietf_openness_commitment__capture_substrate_reading, large_platform_operators, beneficiary,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(ietf_openness_commitment__capture_substrate_reading, large_platform_operators, agenda_setter).

% Working group chairs, document editors, and area directors who manage the standards process. They hold procedural authority over consensus determination and document progression. While formally neutral, their institutional affiliations and career incentives align with large platform operators who fund their positions. They maintain the process's legitimacy while enabling the resource-to-gatekeeping translation.
narrative_ontology:constraint_stakeholder(ietf_openness_commitment__capture_substrate_reading, standards_editors, agenda_setter,
    organized, biographical, constrained, global).

% Independent developers, smaller companies, and open-source projects implementing standards. They lack the resources to sustain full-time standards participation or to influence specification direction. They bear the cost of tracking evolving specifications, implementing features driven by large platform roadmaps, and maintaining compatibility with proprietary extensions that become de facto required. Exit means abandoning interoperability or accepting second-class implementation status.
narrative_ontology:constraint_stakeholder(ietf_openness_commitment__capture_substrate_reading, small_implementers, payer,
    moderate, biographical, constrained, global).

% Users of internet services and applications who experience the downstream effects: reduced browser choice, vendor lock-in, degraded privacy when proprietary extensions become mandatory for service access, and ecosystem consolidation. They have no direct voice in standards processes and cannot practically exit the internet. The costs are diffuse but structural — they pay through reduced competition and enforced dependency on platform-controlled infrastructure.
narrative_ontology:constraint_stakeholder(ietf_openness_commitment__capture_substrate_reading, end_users, payer,
    powerless, biographical, trapped, global).

% Public interest groups, digital rights organizations, and user advocacy groups that attempt to participate in standards processes. They face structural barriers: participation requires sustained technical engagement and travel funding they often lack. Their objections to capture dynamics are noted but rarely alter outcomes because they lack the implementation leverage that drives consensus in the rough-consensus model.
narrative_ontology:constraint_stakeholder(ietf_openness_commitment__capture_substrate_reading, civil_society_organizations, excluded,
    organized, generational, constrained, global).

% Scholars studying internet governance, standards politics, and protocol economics. They analyze capture dynamics from outside the process, producing evidence that documents the resource-to-gatekeeping translation. Their work informs policy and regulatory attention but does not directly alter standards outcomes. They hold the analytical seat with no stake in the constraint's operation.
narrative_ontology:constraint_stakeholder(ietf_openness_commitment__capture_substrate_reading, academic_researchers, observer,
    moderate, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The IETF standards process coordinates interoperable implementation of internet protocols across diverse vendors and platforms, enabling a global network where any compliant implementation can communicate with any other. This solves the fundamental coordination problem of distributed protocol development without central planning.
% TRANSFER_FUNCTION: The process transfers control over protocol evolution and implementation requirements from the distributed implementer community toward large platform operators. Resource investment (engineering staff, testing infrastructure, meeting attendance) translates into specification influence, which encodes platform-specific behaviors as de facto standards. Value flows from small implementers and users (who lose choice and bear compatibility costs) to large platforms (who gain lock-in and competitive moats).
% ABSENT_VOICES: End users and small implementers who would object to proprietary extensions masquerading as open standards are structurally excluded by the participation-cost barrier. Civil society organizations attempting to represent them lack the implementation leverage that the rough-consensus model weights. Future users who will inherit a more consolidated ecosystem have no voice at all.
% DISAPPEARANCE_RATIONALE: If the IETF openness commitment vanished overnight, the coordination substrate for internet protocols would collapse. Large platforms would likely formalize proprietary protocols within their ecosystems, fragmenting the internet into incompatible walled gardens. Small implementers would lose the shared reference points that enable competitive entry. Users would face reduced choice and increased lock-in. The world would rearrange toward Balkanized protocol stacks controlled by the current resource-advantaged actors.
% FOUNDING_PROBLEM: The early internet needed a way to develop interoperable protocols without central authority, proprietary control, or vendor lock-in. The IETF's rough-consensus-and-running-code model was built to solve this: open participation, transparent process, and merit-based technical decision-making would produce standards that any implementer could adopt freely.
% FOUNDING_PROBLEM_CORROBORATION: The IETF's own historical documents (RFC 3935, RFC 7282) and founding participants attest the problem was interoperability without central control. Independent scholars (e.g., Mueller, DeNardis) and civil society analyses document that the coordination problem persists but the solution has been captured — the process still produces standards, but the resource-advantaged actors now shape them for competitive advantage. No corroborating source outside the beneficiary set affirms the process functions as originally designed.
narrative_ontology:disappearance_verdict(ietf_openness_commitment__capture_substrate_reading, world_rearranges).
narrative_ontology:founding_problem_status(ietf_openness_commitment__capture_substrate_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ietf_openness_commitment__capture_substrate_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(ietf_openness_commitment__capture_substrate_reading, 'none', 1).
narrative_ontology:epsilon_provenance(ietf_openness_commitment__capture_substrate_reading, 0.58, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Extractiveness is moderate (0.58) because the coordination function is real and valuable — the internet still interoperates — but a growing share of specification complexity serves platform-specific interests. Suppression is moderate (0.42) because alternatives exist (forking, alternative venues) but are costly; the suppression is structural (participation cost, consensus weighting) not overt coercion. Theater ratio (0.38) reflects that the open process rituals (mailing lists, meetings, consensus calls) are real but increasingly performative — the substantive decisions track resource investment. Accessibility collapse (0.45) is partial: small implementers can still participate but cannot shape outcomes. Resistance (0.55) is significant: alternative venues (W3C, WHATWG, informal consortia), regulatory scrutiny, and fork threats exist but have not reversed the trend.
 *
 * PERSPECTIVAL GAP:
 *   From the large platform operator seat, the process is genuine coordination they sustain through engineering investment — the openness commitment is real and they are its primary stewards. From the small implementer seat, the same process operates as a filter that converts resource advantage into specification control — the openness commitment is the mechanism that legitimates the extraction. The engine computes this divergence from the structural data: beneficiaries with arbitrage exit vs. payers with constrained/trapped exit, same constraint.
 *
 * DIRECTIONALITY LOGIC:
 *   Large platform operators are structural beneficiaries (d near 0.15): they collect network effects and lock-in value from specifications aligned with their implementations. Standards editors are secondary beneficiaries/agenda-setters (d near 0.25): they hold procedural authority and their careers align with platform interests. Small implementers are targets (d near 0.75): they pay compatibility costs and lack exit leverage. End users are trapped targets (d near 0.9): they bear diffuse structural costs with no voice and no exit. Civil society is excluded (d near 0.6): they would oppose but lack implementation leverage. Academics are analytical observers (d=0.5): they see the structure but do not participate in it.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (interoperability without central control) remains live — the internet still needs open standards. But the arrangement built to solve it has been captured: the resource-advantaged actors who were supposed to be participants have become the architects. The coordination function persists but now serves extraction. This is not mandatrophy (where the problem is gone but the arrangement persists) — it is capture: the problem remains, the arrangement persists, but the arrangement now serves different masters. The mandate has not atrophied; it has been redirected.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_framing,
    'Does the IETF openness commitment instantiate a single constraint with multiple observer perspectives, or multiple structurally distinct constraints (one per reading)?',
    'Apply the epsilon-invariance test: if measuring extractiveness from the commons_stewardship reading yields a fundamentally different epsilon than from the capture_substrate reading, they are different constraints. The engine already models them as separate constraint stories linked by network.affects_constraints.',
    'If the kernel produces multiple epsilon-invariant constraints, the framework correctly decomposes it. If epsilon is observer-relative within one constraint, the framework''s core principle is violated.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_framing, conceptual, 'Whether the kernel decomposes into multiple epsilon-invariant constraints or one constraint with multiple readings').

omega_variable(
    resource_to_gatekeeping_mechanism,
    'What is the precise causal mechanism by which resource investment in standards participation translates into specification influence that functions as gatekeeping?',
    'Process tracing of specific protocol developments (e.g., HTTP/2, TLS 1.3, WebTransport, WebCodecs) comparing platform proposal adoption rates vs. independent proposals, controlling for technical merit indicators.',
    'If mechanism is documented, the capture_substrate reading gains empirical grounding. If mechanism is indeterminate, the reading remains a structural hypothesis.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(resource_to_gatekeeping_mechanism, empirical, 'Causal mechanism linking resource investment to gatekeeping outcomes in standards').

omega_variable(
    coordination_extraction_separability,
    'Are the coordination function (interoperability) and extraction function (platform advantage encoding) structurally separable, or does the extraction require the coordination substrate?',
    'Counterfactual analysis: would a forked process without platform participation produce interoperable standards? Historical comparison to W3C/WHATWG split and IETF-IRTF relationship.',
    'If separable, the extraction is parasitic on coordination and could be removed. If inseparable, the coordination substrate itself encodes the extraction — the constraint is fundamentally tangled.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coordination_extraction_separability, conceptual, 'Whether the coordination and extraction components can be disentangled').

omega_variable(
    suppression_mechanism_internalization,
    'Is the suppression experienced by small implementers structural (participation costs, consensus weighting) or internalized (belief that the process is fair, acceptance of platform-driven outcomes as technical necessity)?',
    'Survey and interview study of small implementer perceptions: do they attribute their lack of influence to structural barriers or to legitimate technical consensus?',
    'If internalized suppression is significant, the constraint''s effective suppression exceeds the structural measure — the targets carry the suppression with them as cognitive frames.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalization, empirical, 'Structural vs. internalized suppression for small implementers in standards processes').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ietf_openness_commitment__capture_substrate_reading, 1992, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ietf_capture_tr_t1992, ietf_openness_commitment__capture_substrate_reading, theater_ratio, 1992, 0.08).
narrative_ontology:measurement(ietf_capture_tr_t1998, ietf_openness_commitment__capture_substrate_reading, theater_ratio, 1998, 0.12).
narrative_ontology:measurement(ietf_capture_tr_t2004, ietf_openness_commitment__capture_substrate_reading, theater_ratio, 2004, 0.18).
narrative_ontology:measurement(ietf_capture_tr_t2010, ietf_openness_commitment__capture_substrate_reading, theater_ratio, 2010, 0.25).
narrative_ontology:measurement(ietf_capture_tr_t2016, ietf_openness_commitment__capture_substrate_reading, theater_ratio, 2016, 0.31).
narrative_ontology:measurement(ietf_capture_tr_t2020, ietf_openness_commitment__capture_substrate_reading, theater_ratio, 2020, 0.35).
narrative_ontology:measurement(ietf_capture_tr_t2024, ietf_openness_commitment__capture_substrate_reading, theater_ratio, 2024, 0.38).

% Extraction over time
narrative_ontology:measurement(ietf_capture_be_t1992, ietf_openness_commitment__capture_substrate_reading, base_extractiveness, 1992, 0.15).
narrative_ontology:measurement(ietf_capture_be_t1998, ietf_openness_commitment__capture_substrate_reading, base_extractiveness, 1998, 0.22).
narrative_ontology:measurement(ietf_capture_be_t2004, ietf_openness_commitment__capture_substrate_reading, base_extractiveness, 2004, 0.31).
narrative_ontology:measurement(ietf_capture_be_t2010, ietf_openness_commitment__capture_substrate_reading, base_extractiveness, 2010, 0.42).
narrative_ontology:measurement(ietf_capture_be_t2016, ietf_openness_commitment__capture_substrate_reading, base_extractiveness, 2016, 0.51).
narrative_ontology:measurement(ietf_capture_be_t2020, ietf_openness_commitment__capture_substrate_reading, base_extractiveness, 2020, 0.55).
narrative_ontology:measurement(ietf_capture_be_t2024, ietf_openness_commitment__capture_substrate_reading, base_extractiveness, 2024, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(ietf_capture_su_t1992, ietf_openness_commitment__capture_substrate_reading, suppression_requirement, 1992, 0.12).
narrative_ontology:measurement(ietf_capture_su_t1998, ietf_openness_commitment__capture_substrate_reading, suppression_requirement, 1998, 0.18).
narrative_ontology:measurement(ietf_capture_su_t2004, ietf_openness_commitment__capture_substrate_reading, suppression_requirement, 2004, 0.25).
narrative_ontology:measurement(ietf_capture_su_t2010, ietf_openness_commitment__capture_substrate_reading, suppression_requirement, 2010, 0.33).
narrative_ontology:measurement(ietf_capture_su_t2016, ietf_openness_commitment__capture_substrate_reading, suppression_requirement, 2016, 0.38).
narrative_ontology:measurement(ietf_capture_su_t2020, ietf_openness_commitment__capture_substrate_reading, suppression_requirement, 2020, 0.4).
narrative_ontology:measurement(ietf_capture_su_t2024, ietf_openness_commitment__capture_substrate_reading, suppression_requirement, 2024, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ietf_openness_commitment__capture_substrate_reading, information_standard).
narrative_ontology:boltzmann_floor_override(ietf_openness_commitment__capture_substrate_reading, 0.03).
narrative_ontology:affects_constraint(ietf_openness_commitment__capture_substrate_reading, ietf_openness_commitment__commons_stewardship_reading).
narrative_ontology:affects_constraint(ietf_openness_commitment__capture_substrate_reading, ietf_openness_commitment__legitimacy_erosion_reading).

% DUAL FORMULATION NOTE:
% This constraint family decomposes the IETF openness commitment kernel into three readings with distinct epsilon values and stakeholder structures. capture_substrate_reading sees moderate extractiveness with platform beneficiaries; commons_stewardship_reading sees low extractiveness with universal beneficiaries; legitimacy_erosion_reading sees the consensus mechanism itself as the contested element. The upstream commons_stewardship claim (the process works) is often cited as evidence against the downstream capture_substrate claim (the process is captured), creating an affects edge from commons_stewardship to capture_substrate.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
