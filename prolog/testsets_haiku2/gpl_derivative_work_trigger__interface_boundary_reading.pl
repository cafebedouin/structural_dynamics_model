% ============================================================================
% CONSTRAINT STORY: gpl_derivative_work_trigger__interface_boundary_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_gpl_derivative_work_trigger__interface_boundary_reading, []).

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
 *   constraint_id: gpl_derivative_work_trigger__interface_boundary_reading
 *   human_readable: GPL Derivative Work Trigger: Clean API Boundary Reading
 *   domain: legal/licensing/open-source
 *
 * SUMMARY:
 *   This constraint instantiates the interface-boundary reading of GPL
 *   derivative-work trigger: the legal and technical claim that clean API
 *   boundaries between GPL-licensed and proprietary software components
 *   constitute non-derivative aggregation even under tight runtime coupling.
 *   The reading emerges as a de facto standard in major technology ecosystems
 *   (Android, embedded Linux, proprietary firmware). It is ONE of three
 *   competing readings of the GPL kernel—each reading defines a different
 *   constraint with a different beneficiary/victim structure and different
 *   persistence mechanism. The broad-copyleft reading holds that any
 *   substantial linking propagates GPL obligations; the
 *   narrow-linking-permissive reading treats linking itself as neutral
 *   aggregation and only source modification as triggering copyleft. This
 *   constraint (interface-boundary) sits between them: API cleanliness is the
 *   trigger, not linking per se and not modification per se.
 *
 * KEY AGENTS:
 *   - Ecosystem integrators (Android, embedded manufacturers): benefit from interface-boundary reading; defend clean-API doctrine
 *   - Proprietary downstream vendors: benefit from interface-boundary reading; adopt it as license compliance strategy
 *   - GPL copyleft advocates (FSF, open-source governance): bear the cost; argue the broad reading captures the GPL's intent
 *   - Open-source users expecting full disclosure: trapped; their reasonable expectations are violated by the reading
 *   - Courts and arbitration bodies: agenda-setters; their rulings instantiate which reading governs in jurisdiction-specific cases
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gpl_derivative_work_trigger__interface_boundary_reading, 0.38).
domain_priors:suppression_score(gpl_derivative_work_trigger__interface_boundary_reading, 0.22).
domain_priors:theater_ratio(gpl_derivative_work_trigger__interface_boundary_reading, 0.18).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gpl_derivative_work_trigger__interface_boundary_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(gpl_derivative_work_trigger__interface_boundary_reading, suppression_requirement, 0.22).
narrative_ontology:constraint_metric(gpl_derivative_work_trigger__interface_boundary_reading, theater_ratio, 0.18).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gpl_derivative_work_trigger__interface_boundary_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(gpl_derivative_work_trigger__interface_boundary_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gpl_derivative_work_trigger__interface_boundary_reading, scaffold).
narrative_ontology:human_readable(gpl_derivative_work_trigger__interface_boundary_reading, "GPL Derivative Work Trigger: Clean API Boundary Reading").
narrative_ontology:topic_domain(gpl_derivative_work_trigger__interface_boundary_reading, "legal/licensing/open-source").

domain_priors:requires_active_enforcement(gpl_derivative_work_trigger__interface_boundary_reading).
narrative_ontology:has_sunset_clause(gpl_derivative_work_trigger__interface_boundary_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gpl_derivative_work_trigger__interface_boundary_reading, '1e0e9d1d-5977-415c-9823-7cb242d4f983').
narrative_ontology:cs_kernel_codification('1e0e9d1d-5977-415c-9823-7cb242d4f983', fixed_text).
narrative_ontology:cs_authority_grounding('1e0e9d1d-5977-415c-9823-7cb242d4f983', distributed).
narrative_ontology:cs_reading_relation('1e0e9d1d-5977-415c-9823-7cb242d4f983', gpl_derivative_work_trigger__broad_copyleft_reading, coexists_with).
narrative_ontology:cs_reading_relation('1e0e9d1d-5977-415c-9823-7cb242d4f983', gpl_derivative_work_trigger__narrow_linking_permissive_reading, coexists_with).
narrative_ontology:cs_axiom('1e0e9d1d-5977-415c-9823-7cb242d4f983', foundational, api_boundary_suffices_for_aggregation).
narrative_ontology:cs_axiom_status(api_boundary_suffices_for_aggregation, holdable).
narrative_ontology:cs_axiom_grounding('1e0e9d1d-5977-415c-9823-7cb242d4f983', api_boundary_suffices_for_aggregation, conventional).
narrative_ontology:cs_axiom('1e0e9d1d-5977-415c-9823-7cb242d4f983', foundational, technical_clean_boundary_gates_copyleft).
narrative_ontology:cs_axiom_status(technical_clean_boundary_gates_copyleft, holdable).
narrative_ontology:cs_axiom_grounding('1e0e9d1d-5977-415c-9823-7cb242d4f983', technical_clean_boundary_gates_copyleft, deontological).
narrative_ontology:cs_reference_frame('1e0e9d1d-5977-415c-9823-7cb242d4f983', gpl_text_as_written_v2_v3).
narrative_ontology:cs_drift_state('1e0e9d1d-5977-415c-9823-7cb242d4f983', contemporary_embedded_linux_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('1e0e9d1d-5977-415c-9823-7cb242d4f983', '').
narrative_ontology:cs_kernel_id(gpl_derivative_work_trigger__interface_boundary_reading, gpl_derivative_work_trigger).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gpl_derivative_work_trigger__interface_boundary_reading, ecosystem_integrators).
narrative_ontology:constraint_beneficiary(gpl_derivative_work_trigger__interface_boundary_reading, proprietary_downstream_vendors).
narrative_ontology:constraint_victim(gpl_derivative_work_trigger__interface_boundary_reading, gpl_copyleft_advocates).
narrative_ontology:constraint_victim(gpl_derivative_work_trigger__interface_boundary_reading, open_source_users_expecting_full_disclosure).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Companies like Google (Android), embedded system vendors, and firmware developers that build ecosystems combining GPL-licensed components (Linux kernel, GPL drivers) with proprietary layers (HAL, firmware, closed applications). They benefit from the interface-boundary reading because it permits them to satisfy GPL obligations at the boundary (releasing kernel + drivers) while keeping their proprietary integration layer closed. This reading is their de facto license compliance strategy. They have arbitrage-grade exit: if the interface-boundary reading were foreclosed, they could switch to non-GPL kernels or fully proprietary stacks, though at significant switching cost.
narrative_ontology:constraint_stakeholder(gpl_derivative_work_trigger__interface_boundary_reading, ecosystem_integrators, beneficiary,
    institutional, generational, arbitrage, global).

% Software and hardware companies shipping products that use GPL libraries at clean boundaries (Qt, Apache, web-server stacks). They benefit because they can use GPL components without disclosing their proprietary code. They have mobile exit: they can substitute non-GPL libraries, pay for proprietary licenses, or rewrite components, though at development cost. Their beneficiary status is stable across jurisdictions because the interface-boundary reading is de facto global in practice.
narrative_ontology:constraint_stakeholder(gpl_derivative_work_trigger__interface_boundary_reading, proprietary_downstream_vendors, beneficiary,
    powerful, biographical, mobile, global).

% Free Software Foundation, GPL stewards, open-source governance bodies, and copyleft-philosophy advocates. They hold the GPL as a tool to preserve software commons and ensure full-stack source disclosure. The interface-boundary reading contradicts their stated intent: they argue that tight integration with GPL components should trigger full copyleft regardless of API formalism. They bear the cost of defending the broad-copyleft reading in litigation and lobbying. Their exit is constrained: they are locked into the copyleft mandate by their organizational mission; they cannot easily switch to a different license philosophy without betraying their founding principles.
narrative_ontology:constraint_stakeholder(gpl_derivative_work_trigger__interface_boundary_reading, gpl_copyleft_advocates, payer,
    organized, generational, constrained, global).

% End users of devices and software incorporating GPL components who reasonably expect, under the GPL's spirit and stated terms, that the entire work will be source-disclosed. Examples: Android phone users, IoT device owners, embedded Linux system users. They are trapped: they depend on products using the GPL, but the interface-boundary reading permits vendors to keep user-facing code proprietary. They have no exit: they cannot easily choose alternative products, and they have no say in license interpretation decisions.
narrative_ontology:constraint_stakeholder(gpl_derivative_work_trigger__interface_boundary_reading, open_source_users_expecting_full_disclosure, payer,
    powerless, biographical, trapped, global).
narrative_ontology:stakeholder_secondary_role(gpl_derivative_work_trigger__interface_boundary_reading, open_source_users_expecting_full_disclosure, excluded).

% Courts, arbitration bodies, and legal authorities in different jurisdictions (US Federal Courts, German BGH, French courts, EU authorities) that adjudicate GPL license disputes. They set which reading governs through precedent. The interface-boundary reading is their construction: they must interpret the GPL text in context of specific cases (e.g., Android litigation, embedded firmware disputes). Different jurisdictions have different precedents and standards. They have analytical exit: their role is to apply law, not to choose it; they are constrained by precedent and legal doctrine.
narrative_ontology:constraint_stakeholder(gpl_derivative_work_trigger__interface_boundary_reading, courts_and_legal_arbiters, agenda_setter,
    institutional, generational, analytical, national).

% FSF, GPL maintainers, and open-source governance bodies. They author and maintain the GPL text and provide official guidance. Under the current reading, they are observers: courts enforce the interface-boundary reading despite FSF's stated preference for broad copyleft. FSF guidance (published interpretations) favors the broad reading, but courts and vendors treat it as advisory, not canonical. Their analytical exit is limited: they can revise the GPL (GPL v4), but that requires adoption by courts and vendors, which is not guaranteed.
narrative_ontology:constraint_stakeholder(gpl_derivative_work_trigger__interface_boundary_reading, gpl_license_stewards, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(gpl_derivative_work_trigger__interface_boundary_reading, ecosystem_integrators).
narrative_ontology:fixing_cost_class(gpl_derivative_work_trigger__interface_boundary_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a legal framework for modular software architecture with mixed licensing: GPL-licensed components can be integrated with proprietary code at clean API boundaries without forcing the entire stack into a single license. Solves the problem of software composition across license regimes—how to build ecosystems combining open and proprietary code.
% TRANSFER_FUNCTION: Transfers legal risk and compliance burden from ecosystem integrators and proprietary vendors to GPL copyleft advocates and end users. Integrators and vendors gain freedom to keep their code closed while using GPL components; advocates and users lose the ability to require full-stack source disclosure through GPL propagation. The transfer is enforced through the reading's standing interpretation—vendors adopt it as their license compliance strategy.
% ABSENT_VOICES: Technical standardization bodies (IEEE, ISO, W3C, IETF) that could formally define 'clean API boundary' are largely absent from the dispute. Device manufacturers operating the tightest GPL/proprietary boundaries (firmware developers, embedded systems integrators) are present as defendants in litigation but not as equal participants in the interpretive conversation. Downstream users are structurally excluded: they are not parties to license negotiations and have no formal voice in how the GPL is interpreted.
% DISAPPEARANCE_RATIONALE: If the interface-boundary reading were invalidated overnight (e.g., by a broad-copyleft court ruling or GPL v4 revision), major technology ecosystems would require immediate redesign. Android and embedded Linux stacks would face the choice: disclose proprietary firmware/integration layers or remove GPL components. Many vendors would choose non-GPL kernels or full proprietary alternatives. The modular architecture that currently depends on the interface-boundary reading would fracture. Current business models built on this reading would become unviable without relicensing or redesign.
% FOUNDING_PROBLEM: GPL v2 (1991) and v3 (2007) do not formally define the boundary between derivative works (triggering copyleft obligations) and aggregations (remaining independent licenses). Early GPL enforcement focused on direct source modification. As embedded systems, dynamic linking, and complex integration architectures became standard, the ambiguity sharpened: does linking or integration with a GPL component across a clean API boundary constitute derivation (triggering full copyleft) or aggregation (preserving independence)?
% FOUNDING_PROBLEM_CORROBORATION: Both FSF (GPL v3 preamble, official guidance) and major technology companies (Android developers, embedded vendors) acknowledge the ambiguity. Academic analyses (law review articles, GPL expert witnesses in litigation) confirm the text is structurally under-determined. Judicial opinions (German BGH cases, U.S. cases citing GPL, European authorities) document the unresolved status and different interpretations applied in different jurisdictions.
narrative_ontology:disappearance_verdict(gpl_derivative_work_trigger__interface_boundary_reading, world_rearranges).
narrative_ontology:founding_problem_status(gpl_derivative_work_trigger__interface_boundary_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gpl_derivative_work_trigger__interface_boundary_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(gpl_derivative_work_trigger__interface_boundary_reading, 'none', 1).
narrative_ontology:epsilon_provenance(gpl_derivative_work_trigger__interface_boundary_reading, 0.38, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gpl_derivative_work_trigger__interface_boundary_reading_tests).
:- end_tests(gpl_derivative_work_trigger__interface_boundary_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   This is authored as a SCAFFOLD because the interface-boundary reading is structurally a transitional accommodation: it enables current modular architectures but carries a sunset condition (either the broad-copyleft reading will eventually win through case law and license revision, or the narrow-linking-permissive reading will crystallize as the new consensus). The reading is not self-justifying; it is justified by a specific historical moment—mixed proprietary/open ecosystems—and will not survive the next legal clarification or license revision. Extractiveness is moderate (0.38 at interval end) because the beneficiaries (ecosystem integrators, proprietary vendors) do extract legal optionality by avoiding full-stack GPL obligations, but the extraction is bounded: they must still comply with GPL terms for the components they use, and the arrangement does solve a genuine technical problem (modular architecture). Suppression is low (0.22) because copyleft advocates can and do argue the alternative reading; litigation is open and hostile. Theater is low (0.18) because the justification ('clean API boundaries preserve aggregation') is genuinely applied, not purely performative, though it is applied selectively (a vendor can call their boundary 'clean' even when the proprietary layer tightly couples to GPL internals). The measurement series show gradual accumulation of extractiveness from 1991–2007 (the embedded Linux explosion) and slower growth 2007–2025 as the interface-boundary reading becomes entrenched through practice despite FSF's stated broad-copyleft preference. Theater_ratio rises slowly because vendors gradually invent 'API cleanliness' justifications for designs that are technically or legally questionable, but the core technical claim (clean boundaries enable modular licensing) remains honest.
 *
 * PERSPECTIVAL GAP:
 *   The beneficiary seats (ecosystem integrators, proprietary vendors) experience this reading as stabilizing, enabling their business model. From their seat, the interface-boundary reading is a clarification that resolves ambiguity in their favor. The payer seats (copyleft advocates, end users) experience the same reading as a betrayal: the GPL was meant to propagate; the interface-boundary reading permits vendors to nullify GPL's primary covenant (source disclosure). The courts, as agenda-setters, occupy a different seat altogether: they are not beneficiaries or payers but adjudicators who must decide which reading is 'correct' under the text. The engine will compute different per-seat types: from the integrator's seat, this may compute as a rope (solving genuine coordination); from the advocate's seat, as a snare (extracting legal freedom while suppressing the copyleft alternative). The claimed type (scaffold) asserts the structural fact that the reading is transitional, not a permanent settlement.
 *
 * DIRECTIONALITY LOGIC:
 *   Ecosystem integrators and proprietary vendors are beneficiaries (d near 0.0): they gain legal optionality and freedom to keep their stacks closed-source. Copyleft advocates bear the cost (d near 1.0): their ability to enforce full-stack disclosure is curtailed, and they must continuously defend the broad reading in litigation and governance forums. End users are victims of a different order (d near 1.0): they are trapped in the reading's outcome without having authored it or chosen it; their reasonable expectation of transparency is violated. Courts are agenda-setters (power=institutional, exit=analytical): they set the reading through adjudication. No beneficiary has 'mobile' exit here because all exit routes converge on the same reading: a vendor cannot easily abandon the interface-boundary interpretation without redesigning their entire product. No payer has arbitrage-grade exit because the reading is globally entrenched in major ecosystems; a single advocate cannot switch to a different reading without litigation or regulatory intervention.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (GPL ambiguity on derivatives) is live—courts and companies still dispute it. The interface-boundary reading persists because it serves the current epoch's technology architecture (mixed proprietary/open ecosystems, embedded systems with firmware/driver boundaries). However, the reading is not self-justifying as a permanent settlement: it is a compromise that benefits one party (vendors) at the cost of another (advocates). The classification as SCAFFOLD captures this: the reading solves a real problem (how to modularize with mixed licenses) but is not a stable equilibrium. Either the next license revision will explicitly close or open the boundary (invalidating the reading's force), or case law will accumulate enough precedent to settle the question canonically. The mandatrophy signal is weak but present: the reading is maintained through vendor practice and selective enforcement, not through universal acceptance of its justice.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    api_boundary_formalism,
    'What makes an API boundary ''clean'' enough to constitute aggregation rather than derivation? No formal definition exists; vendors, lawyers, and courts apply different standards.',
    'A license revision or authoritative court ruling defining ''clean boundary'' formally (e.g., requiring IDL specs, defined ABI, no private-header access). Or technical standardization bodies (ISO, W3C, IETF) establishing formal API boundary criteria.',
    'If boundaries are formalized strictly, fewer integrations qualify as aggregation, and the broad-copyleft reading gains ground. If formalized permissively, the interface-boundary reading is strengthened as standard.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(api_boundary_formalism, conceptual, 'The absence of formal definition for ''clean API boundary'' is the reading''s structural vulnerability.').

omega_variable(
    intent_vs_text_gpl_interpretation,
    'Does the GPL''s canonical intent (FSF''s stated goal of full-stack copyleft) override the text''s ambiguity, or does the text''s ambiguity permit the interface-boundary reading as a valid interpretation?',
    'GPL v4 license revision clarifying derivative-work definition. Or accumulated case law establishing whether intent or text governs in specific jurisdictions.',
    'If intent governs, the broad-copyleft reading becomes canonical and the interface-boundary reading is overridden. If text governs, the interface-boundary reading persists as a legitimate interpretation until the text is clarified.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(intent_vs_text_gpl_interpretation, conceptual, 'The hermeneutic question of whether GPL interpretation privileges FSF intent or textual ambiguity.').

omega_variable(
    tight_coupling_as_derivative_trigger,
    'Does tight runtime coupling (deep function calls, shared memory, intimate internal-API use) across an API boundary constitute practical derivation even if the formal interface is clean?',
    'Case law establishing thresholds for ''tight coupling'' (e.g., German BGH precedent on embedded systems, U.S. CFAA derivative-work standards). Or technical analysis of specific products (Android, firmware stacks) establishing whether their coupling depth violates the spirit of the GPL.',
    'If tight coupling overrides formal API cleanliness, the broad-copyleft reading gains ground and the interface-boundary reading is limited to loosely-coupled aggregations. If coupling is irrelevant to the copyleft question, the interface-boundary reading persists regardless of coupling depth.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(tight_coupling_as_derivative_trigger, empirical, 'The empirical question of whether technical coupling is a relevant copyleft trigger.').

omega_variable(
    reading_stability_vs_ecosystem_dependence,
    'The interface-boundary reading is entrenched in major ecosystems (Android, embedded Linux). If the reading is invalidated by new case law or license revision, how many deployed products become non-compliant?',
    'A hypothetical broad-copyleft court ruling followed by empirical measurement of vendor compliance: how many products would be forced to disclose or redesign?',
    'If massive compliance disruption would result, courts and license stewards have incentive to preserve the reading despite FSF preference for broad copyleft. If minimal disruption, the reading is more vulnerable to override.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_stability_vs_ecosystem_dependence, empirical, 'The economic and technical path-dependence that sustains the reading despite contrary FSF intent.').

omega_variable(
    committer_kernel_reading_instability,
    'The interface-boundary reading exists BECAUSE the GPL text is ambiguous. If one of the sibling readings becomes dominant through case law or license revision, this reading becomes either canonical (integrated into the settled text) or foreclosed (ruled out). Which outcome is structurally likely?',
    'Historical trend analysis: GPL v3 revision (2007) attempted to clarify but left the ambiguity partially unresolved. Next license revision (GPL v4 or successor) will either formalize the interface-boundary reading or foreclose it. Case law accumulation may stabilize one reading before license revision.',
    'If the reading is foreclosed, this constraint becomes a zombie—maintained by practice but contradicted by canonical authority. If canonicalized, the reading transitions from SCAFFOLD to ROPE (a permanent coordination mechanism). If another reading (broad or narrow) becomes dominant, this reading''s extractive benefits are nullified.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(committer_kernel_reading_instability, conceptual, 'The kernel-reading lifecycle question: will the interface-boundary reading persist, be canonicalized, or be foreclosed?').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gpl_derivative_work_trigger__interface_boundary_reading, 1991, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gpl__tr_t1991, gpl_derivative_work_trigger__interface_boundary_reading, theater_ratio, 1991, 0.02).
narrative_ontology:measurement(gpl__tr_t2001, gpl_derivative_work_trigger__interface_boundary_reading, theater_ratio, 2001, 0.08).
narrative_ontology:measurement(gpl__tr_t2007, gpl_derivative_work_trigger__interface_boundary_reading, theater_ratio, 2007, 0.11).
narrative_ontology:measurement(gpl__tr_t2013, gpl_derivative_work_trigger__interface_boundary_reading, theater_ratio, 2013, 0.14).
narrative_ontology:measurement(gpl__tr_t2019, gpl_derivative_work_trigger__interface_boundary_reading, theater_ratio, 2019, 0.17).
narrative_ontology:measurement(gpl__tr_t2025, gpl_derivative_work_trigger__interface_boundary_reading, theater_ratio, 2025, 0.18).

% Extraction over time
narrative_ontology:measurement(gpl__be_t1991, gpl_derivative_work_trigger__interface_boundary_reading, base_extractiveness, 1991, 0.05).
narrative_ontology:measurement(gpl__be_t2001, gpl_derivative_work_trigger__interface_boundary_reading, base_extractiveness, 2001, 0.15).
narrative_ontology:measurement(gpl__be_t2007, gpl_derivative_work_trigger__interface_boundary_reading, base_extractiveness, 2007, 0.22).
narrative_ontology:measurement(gpl__be_t2013, gpl_derivative_work_trigger__interface_boundary_reading, base_extractiveness, 2013, 0.32).
narrative_ontology:measurement(gpl__be_t2019, gpl_derivative_work_trigger__interface_boundary_reading, base_extractiveness, 2019, 0.36).
narrative_ontology:measurement(gpl__be_t2025, gpl_derivative_work_trigger__interface_boundary_reading, base_extractiveness, 2025, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(gpl__su_t1991, gpl_derivative_work_trigger__interface_boundary_reading, suppression_requirement, 1991, 0.1).
narrative_ontology:measurement(gpl__su_t2001, gpl_derivative_work_trigger__interface_boundary_reading, suppression_requirement, 2001, 0.16).
narrative_ontology:measurement(gpl__su_t2007, gpl_derivative_work_trigger__interface_boundary_reading, suppression_requirement, 2007, 0.18).
narrative_ontology:measurement(gpl__su_t2013, gpl_derivative_work_trigger__interface_boundary_reading, suppression_requirement, 2013, 0.2).
narrative_ontology:measurement(gpl__su_t2019, gpl_derivative_work_trigger__interface_boundary_reading, suppression_requirement, 2019, 0.21).
narrative_ontology:measurement(gpl__su_t2025, gpl_derivative_work_trigger__interface_boundary_reading, suppression_requirement, 2025, 0.22).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gpl_derivative_work_trigger__interface_boundary_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(gpl_derivative_work_trigger__interface_boundary_reading, 0.12).
narrative_ontology:affects_constraint(gpl_derivative_work_trigger__interface_boundary_reading, gpl_derivative_work_trigger__broad_copyleft_reading).
narrative_ontology:affects_constraint(gpl_derivative_work_trigger__interface_boundary_reading, gpl_derivative_work_trigger__narrow_linking_permissive_reading).

% DUAL FORMULATION NOTE:
% The GPL derivative-work trigger decomposes into three structurally distinct constraints, one per reading of the ambiguous GPL text. The interface-boundary reading (this constraint) sits between the broad-copyleft reading (tightest interpretation, highest extraction from vendors) and the narrow-linking-permissive reading (most permissive, lowest extraction). All three readings share the same referent (the GPL text) but instantiate different ε values, different beneficiary/victim structures, and different persistence mechanisms. They form a constraint family linked by network.affects_constraints. Each reading competes for canonical status in different jurisdictions and governance forums.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
