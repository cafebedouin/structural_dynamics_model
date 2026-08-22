% ============================================================================
% CONSTRAINT STORY: gpl_derivative_work_trigger__interface_boundary_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:measurement_basis/2,
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
 *   constraint_id: gpl_derivative_work_trigger__interface_boundary_reading
 *   human_readable: GPL Derivative Work Trigger: Clean Interface Boundary Reading
 *   domain: intellectual_property/open_source_governance
 *
 * SUMMARY:
 *   The GPL derivative-work question asks: when does linking or tightly
 *   coupling GPL-licensed code to proprietary code trigger the GPL's
 *   source-disclosure obligation? This constraint instantiates ONE
 *   READING—the interface-boundary reading—which answers: clean API
 *   boundaries (dynamic linking, inter-process communication, module
 *   separation) permit aggregation without derivative-work formation, even
 *   with tight architectural coupling. Under this reading, proprietary
 *   software can incorporate GPL libraries without releasing its own code, as
 *   long as the GPL and proprietary code are separated at a technical
 *   interface. This reading competes with two sibling readings: the
 *   broad-copyleft reading (any functional coupling triggers derivative-work
 *   obligations) and the narrow-linking-permissive reading (only source-code
 *   modifications trigger obligations, not linking). The interface-boundary
 *   reading is currently the dominant technical and operational standard in
 *   industry practice (Android, embedded systems, cloud infrastructure) but
 *   remains contested in legal doctrine and by GPL stewards committed to
 *   broader copyleft reach.
 *
 * KEY AGENTS:
 *   - Ecosystem integrators (Android maintainers, embedded-systems platforms, cloud operators): benefit from modularity permission; architect the systems
 *   - Proprietary software vendors (closed-source application and integration layers): benefit from using GPL infrastructure without source disclosure
 *   - GPL stewards (FSF, SFC, Linux Foundation, individual GPL authors): maintain and interpret the license; contest the reading
 *   - Source transparency advocates (open-source development communities, GPL-committed projects): argue for broad copyleft; pay the cost of narrowed interpretation
 *   - End users (platform users, embedded-device owners): expect GPL to guarantee source transparency; instead get mixed-license stacks
 *   - Linked GPL authors (library maintainers, kernel developers): accept the reading as permitting wider adoption; some contest it as intent violation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gpl_derivative_work_trigger__interface_boundary_reading, 0.42).
domain_priors:suppression_score(gpl_derivative_work_trigger__interface_boundary_reading, 0.38).
domain_priors:theater_ratio(gpl_derivative_work_trigger__interface_boundary_reading, 0.29).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gpl_derivative_work_trigger__interface_boundary_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(gpl_derivative_work_trigger__interface_boundary_reading, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(gpl_derivative_work_trigger__interface_boundary_reading, theater_ratio, 0.29).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gpl_derivative_work_trigger__interface_boundary_reading, accessibility_collapse, 0.51).
narrative_ontology:constraint_metric(gpl_derivative_work_trigger__interface_boundary_reading, resistance, 0.67).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gpl_derivative_work_trigger__interface_boundary_reading, scaffold).
narrative_ontology:human_readable(gpl_derivative_work_trigger__interface_boundary_reading, "GPL Derivative Work Trigger: Clean Interface Boundary Reading").
narrative_ontology:topic_domain(gpl_derivative_work_trigger__interface_boundary_reading, "intellectual_property/open_source_governance").

domain_priors:requires_active_enforcement(gpl_derivative_work_trigger__interface_boundary_reading).
narrative_ontology:has_sunset_clause(gpl_derivative_work_trigger__interface_boundary_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gpl_derivative_work_trigger__interface_boundary_reading, '4ea85cf0-0b89-47e6-be14-db3906839aa5').
narrative_ontology:cs_kernel_codification('4ea85cf0-0b89-47e6-be14-db3906839aa5', fixed_text).
narrative_ontology:cs_authority_grounding('4ea85cf0-0b89-47e6-be14-db3906839aa5', extraction).
narrative_ontology:cs_interpretation_layer_present('4ea85cf0-0b89-47e6-be14-db3906839aa5').
narrative_ontology:cs_reading_relation('4ea85cf0-0b89-47e6-be14-db3906839aa5', gpl_derivative_work_trigger__broad_copyleft_reading, coexists_with).
narrative_ontology:cs_reading_relation('4ea85cf0-0b89-47e6-be14-db3906839aa5', gpl_derivative_work_trigger__narrow_linking_permissive_reading, influences).
narrative_ontology:cs_axiom('4ea85cf0-0b89-47e6-be14-db3906839aa5', foundational, clean_api_boundaries_defeat_derivative_formation).
narrative_ontology:cs_axiom_status(clean_api_boundaries_defeat_derivative_formation, holdable).
narrative_ontology:cs_axiom_grounding('4ea85cf0-0b89-47e6-be14-db3906839aa5', clean_api_boundaries_defeat_derivative_formation, conventional).
narrative_ontology:cs_axiom('4ea85cf0-0b89-47e6-be14-db3906839aa5', foundational, gpl_obligation_module_scoped_not_system_scoped).
narrative_ontology:cs_axiom_status(gpl_obligation_module_scoped_not_system_scoped, holdable).
narrative_ontology:cs_axiom_grounding('4ea85cf0-0b89-47e6-be14-db3906839aa5', gpl_obligation_module_scoped_not_system_scoped, conventional).
narrative_ontology:cs_reference_frame('4ea85cf0-0b89-47e6-be14-db3906839aa5', gpl_copyleft_obligation_over_aggregation).
narrative_ontology:cs_drift_state('4ea85cf0-0b89-47e6-be14-db3906839aa5', contemporary_mixed_license_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('4ea85cf0-0b89-47e6-be14-db3906839aa5', '').
narrative_ontology:cs_kernel_id(gpl_derivative_work_trigger__interface_boundary_reading, gpl_derivative_work_trigger).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gpl_derivative_work_trigger__interface_boundary_reading, ecosystem_integrators).
narrative_ontology:constraint_beneficiary(gpl_derivative_work_trigger__interface_boundary_reading, proprietary_software_vendors).
narrative_ontology:constraint_victim(gpl_derivative_work_trigger__interface_boundary_reading, source_transparency_advocates).
narrative_ontology:constraint_victim(gpl_derivative_work_trigger__interface_boundary_reading, users_expecting_copyleft_stack).
narrative_ontology:constraint_vindicates(gpl_derivative_work_trigger__interface_boundary_reading, software_modularity_principle).
narrative_ontology:constraint_vindicates(gpl_derivative_work_trigger__interface_boundary_reading, clean_abstraction_boundaries).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Platform maintainers, framework developers, and system integrators who layer GPL libraries under proprietary or differently-licensed top-level code. Benefit from the reading because it permits architectural modularity without triggering copyleft obligations for the integrating layer. Their business model depends on mixing licensed components without source-code cross-contamination. Examples: Android (using Linux kernel), embedded systems combining GPL toolchains with proprietary application layers, cloud platforms using GPL infrastructure beneath closed-source management planes.
narrative_ontology:constraint_stakeholder(gpl_derivative_work_trigger__interface_boundary_reading, ecosystem_integrators, beneficiary,
    organized, generational, mobile, global).

% Commercial software companies that incorporate GPL libraries via clean interfaces (dynamic linking, IPC, separate processes) without releasing their own code. The interface-boundary reading permits this use pattern. Their incentive is to extract value from GPL infrastructure while preserving proprietary differentiation in the integration layer.
narrative_ontology:constraint_stakeholder(gpl_derivative_work_trigger__interface_boundary_reading, proprietary_software_vendors, beneficiary,
    powerful, generational, arbitrage, global).

% Open-source developers, GPL stewards, and users committed to source-availability principles. They argue that tight coupling—even across clean interfaces—creates derivative works and should trigger disclosure. Under this reading, their principled objection is overridden by the interface-boundary framing, which permits the integration pattern they oppose. They lack institutional power to enforce their reading; the ecosystem integrators control the technical architecture.
narrative_ontology:constraint_stakeholder(gpl_derivative_work_trigger__interface_boundary_reading, source_transparency_advocates, payer,
    moderate, biographical, constrained, global).

% End users of platforms and systems that combine GPL and proprietary components under the interface-boundary reading. They expect GPL licenses to guarantee source transparency across the full stack; instead, proprietary layers remain closed. They cannot audit the full system, cannot fork or modify the entire stack, and cannot exercise GPL freedoms above the interface boundary. Their voice is absent from the technical governance that interprets the GPL license.
narrative_ontology:constraint_stakeholder(gpl_derivative_work_trigger__interface_boundary_reading, users_expecting_copyleft_stack, payer,
    powerless, biographical, trapped, global).
narrative_ontology:stakeholder_secondary_role(gpl_derivative_work_trigger__interface_boundary_reading, users_expecting_copyleft_stack, excluded).

% Legal and technical bodies (FSF, SFC, GPL license authors, courts) that define and enforce GPL derivative-work interpretations. The interface-boundary reading competes with other GPL readings for canonical status. Stewards have authority to issue guidance and defend GPL in litigation, but lack unified position—different courts, jurisdictions, and OSI bodies have endorsed different readings. The reading under this story has institutional legitimacy in technical practice even where legal precedent remains mixed.
narrative_ontology:constraint_stakeholder(gpl_derivative_work_trigger__interface_boundary_reading, gpl_steward_organizations, agenda_setter,
    institutional, generational, constrained, global).

% Authors of GPL libraries that are incorporated via clean interfaces into proprietary systems. Under this reading, their GPL choice does not force derivative works to disclose; they accept that architectural modularity may weaken copyleft reach. Some view this as a feature (permitting wider adoption); others view it as a license-intent violation. Their ability to police downstream use is limited by the reading's legitimacy.
narrative_ontology:constraint_stakeholder(gpl_derivative_work_trigger__interface_boundary_reading, linked_gpl_authors, observer,
    moderate, biographical, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(gpl_derivative_work_trigger__interface_boundary_reading, ecosystem_integrators).
narrative_ontology:fixing_cost_class(gpl_derivative_work_trigger__interface_boundary_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Permits modular software architecture where GPL and proprietary components can coexist at separate abstraction boundaries (e.g., kernel/userspace, library/application, service/client). Solves the integration problem: how to build systems that benefit from GPL tooling and infrastructure without triggering source-disclosure obligations for the full stack. Creates a technical partition mechanism that aligns with standard architectural practice.
% TRANSFER_FUNCTION: Transfers GPL-licensed code's freedoms (source transparency, modification rights, redistribution rights) only to the GPL-licensed component and its direct modifications; does not transfer those freedoms across clean interface boundaries to proprietary or differently-licensed integration layers. The reading extracts a restriction-reversal: GPL authors' intended copyleft obligation is narrowed to a single module, not the whole system.
% ABSENT_VOICES: End users who expect GPL licenses to guarantee transparency of systems they use; GPL stewards in rival readings (broad copyleft, narrow linking permissive) who interpret the license differently; jurisdictions whose courts have not yet ruled on the interface-boundary reading; GPL authors whose intent was full-stack copyleft, not modular isolation.
% DISAPPEARANCE_RATIONALE: If this reading disappeared and the broad-copyleft reading prevailed, proprietary software stacks would need to either release source or remove GPL components. Many commercial systems (Android, embedded devices, cloud platforms) would be forced to choose: GPL-license the entire stack, or decouple from GPL infrastructure. Some would choose full-stack GPL release; others would migrate to non-GPL alternatives. The software ecosystem would reorganize around GPL-only or all-proprietary stacks, collapsing the mixed-license modular architecture this reading enables.
% FOUNDING_PROBLEM: Early GPL license language (GPLv2 1991, GPLv3 2007) did not explicitly address architectural modularity, dynamic linking, or inter-process communication. Licensors and courts had to infer whether tight coupling across clean interfaces constituted derivative-work formation. The founding problem is: how do GPL freedoms apply when code is integrated architecturally but separated technically? The interface-boundary reading answers: the GPL obligation stops at the interface.
% FOUNDING_PROBLEM_CORROBORATION: GPL stewards (FSF, SFC, Linux Foundation) acknowledge the problem remains contested and unresolved in law. Some court precedents (Gpl.linuxdevices.com analysis of Jacobsen v. Katzer, Hellwig v. VMware) have supported narrow copyleft readings in similar contexts. Academic GPL scholarship (Moglen, Fontana, Kuhn) documents the interpretive gap. Independent tooling communities (embedded Linux, Android) have adopted the interface-boundary reading in practice, attesting its operational legitimacy even where legal precedent is thin. GPL authors outside the benefiting parties (Free Software Foundation, Conservancy members) contest the reading as license-intent betrayal.
narrative_ontology:disappearance_verdict(gpl_derivative_work_trigger__interface_boundary_reading, world_rearranges).
narrative_ontology:founding_problem_status(gpl_derivative_work_trigger__interface_boundary_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gpl_derivative_work_trigger__interface_boundary_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(gpl_derivative_work_trigger__interface_boundary_reading, 'none', 1).
narrative_ontology:epsilon_provenance(gpl_derivative_work_trigger__interface_boundary_reading, 0.42, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness (0.42 at interval end) reflects the reading's core extraction: GPL obligations are narrowed from full-stack to single-module scope, extracting copyleft reach while retaining GPL branding and adoption benefit. The measurement trajectory rises gradually (0.25 → 0.42) because the reading's operational legitimacy has been accumulating over 15+ years of industry practice (2005–2020); the plateau near t=20 indicates saturation—the reading is now standard in major platforms and is unlikely to face broad reversal. Suppression (0.38) is moderate because the reading is enforced primarily through architectural practice and vendor lock-in (systems are designed to the interface boundary), not through coercive legal machinery. Theater (0.29) is modest because genuine technical architecture does justify the interface boundary—it is not pure performance—but the boundary is often drawn to permit proprietary integration rather than from pure technical necessity. Accessibility collapse (0.51) is near-symmetric because alternatives remain: vendors could choose GPL-only, use non-GPL libraries, or license proprietary libraries separately; the constraint does not eliminate alternatives, only makes one pathway (mixed-license modularity) very convenient. Resistance (0.67) is substantial because GPL stewards, source-transparency advocates, and copyleft-committed developers actively contest the reading in documentation, licensing debates, and occasionally in litigation.
 *
 * PERSPECTIVAL GAP:
 *   The ecosystem integrators and proprietary vendors perceive this reading as enabling legitimate modular architecture—a technical achievement. GPL stewards and source-transparency advocates perceive it as license-intent violation—a narrowing of copyleft obligations that betrays the GPL's founding principle of source transparency. The engine computes this perspectival divergence from stakeholder power, exit options, and roles: beneficiaries sit at high organizational power with mobile exit (can choose architectures, can adopt alternatives), while payers sit at moderate power with constrained exit (must work within the architectures beneficiaries build). The reading's legitimacy depends on sustaining the architectural framing; if the framing breaks (courts rule broadly, or major GPL projects migrate away), the reading's effectiveness collapses.
 *
 * DIRECTIONALITY LOGIC:
 *   Ecosystem integrators and proprietary vendors hold beneficiary roles: they collect the extraction (narrowed GPL obligations, permission to use GPL code without source release). Their directionality is low (near 0.0)—the constraint subsidizes them. Source transparency advocates and users expecting full-stack copyleft hold payer roles: they bear the cost (weakened copyleft reach, closed proprietary layers they cannot audit or modify). Their directionality is high (near 1.0)—the constraint extracts from them. GPL stewards are agenda-setters: they maintain the license text and issue guidance; they are structurally divided on the reading, but they hold institutional authority. No directionality override is needed; the structural derivation captures the relationships accurately.
 *
 * MANDATROPHY ANALYSIS:
 *   This is a Scaffold—a transitional arrangement meant to solve the founding problem of 'how to integrate GPL and proprietary code without legal deadlock' during an era when GPL precedent was thin and modularity was not yet standard. The reading has a sunset implicit in its design: as GPL law clarifies (through court precedent, legislative amendment, or consensus among stewards), the interface-boundary reading would either be ratified into permanent doctrine or superseded by a different interpretation. The measurement plateau (t=20 onward) indicates the reading is currently stable—not actively degrading—but the constraint is fundamentally transitory: it enables mixed-license modularity as a bridge solution while underlying questions about GPL intent remain open. The theater ratio staying low-to-moderate (0.29 vs. 0.5+ would indicate pure performance) reflects that genuine technical architecture justifies the boundary, but the boundary is also strategically drawn to permit proprietary integration. The mandatrophy resolution depends on legal clarification: if courts rule broadly on the GPL's derivative-work scope, the scaffold's temporary status will be confirmed (either ratified or replaced).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    interface_cleanliness_ambiguity,
    'What constitutes a ''clean'' API boundary sufficient to defeat derivative-work formation? Is it a matter of technical architecture (separate processes, dynamic linking, IPC) or functional independence (data independence, behavioral independence, coupling tightness)?',
    'Court precedent or FSF/SFC guidance clarifying derivative-work thresholds in specific architectural contexts (e.g., kernel modules, shared libraries, microservices).',
    'If functional coupling alone determines derivative status regardless of technical boundaries, the interface-boundary reading collapses and GPL obligations broaden. If technical boundaries alone suffice, the reading is strengthened. If a hybrid test applies (boundary + coupling), the threshold shifts but the reading''s core logic persists.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(interface_cleanliness_ambiguity, conceptual, 'Definitional ambiguity about what boundary conditions defeat derivative-work formation.').

omega_variable(
    license_intent_vs_structure,
    'Did GPL authors intend for the license to guarantee source transparency across entire systems, or only across modified GPL code? Does the interface-boundary reading honor GPL intent or circumvent it?',
    'GPL historical documentation (Stallman, FSF founding writings), GPL steward authoritative guidance, legal precedent interpreting license intent, field surveys of GPL authors'' own understanding.',
    'If GPL intent was full-stack transparency, the interface-boundary reading is license-intent violation and should be overridden by a broader reading. If intent was to protect modifications (not integrations), the reading aligns with intent. The answer is historically contested and varies by GPL author.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(license_intent_vs_structure, preference, 'Normative dispute about GPL''s founding intent regarding integration and derivative works.').

omega_variable(
    architectural_necessity_vs_vendor_choice,
    'Are interface boundaries architecturally necessary for modular software design, or are they strategically chosen by vendors to permit proprietary integration?',
    'Analysis of systems designed with and without interface boundaries; comparative case studies of GPL-obligated vs. interface-boundary-permitted integrations; technical necessity audits.',
    'If boundaries are architecturally necessary, the constraint enables legitimate technical practice and suppression is justified. If boundaries are primarily vendor choices to evade GPL, the constraint becomes pure extraction and should be reclassified toward snare. This determines whether the theater ratio is accurately low or artificially low.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(architectural_necessity_vs_vendor_choice, empirical, 'Whether interface boundaries reflect technical necessity or strategic vendor architecture choice.').

omega_variable(
    sibling_reading_contention,
    'Is the interface-boundary reading logically incompatible with the broad-copyleft reading (forecloses), or do they represent different parties'' simultaneous positions (coexists_with)?',
    'Formal logical analysis of the GPL license text; court rulings that establish which reading has canonical authority in binding law; consensus emergence among GPL stewards and major projects.',
    'If forecloses: the broad-copyleft reading would need to be overridden in any unified legal framework; the winner becomes the binding derivative-work definition. If coexists_with: both readings remain live options in different jurisdictions and projects; the contest persists indefinitely. Coexist_with is more likely given current case-law and steward diversity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_contention, conceptual, 'Logical relationship between this reading and its broad-copyleft sibling reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gpl_derivative_work_trigger__interface_boundary_reading, 0, 35).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gpl__tr_t0, gpl_derivative_work_trigger__interface_boundary_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement_basis(gpl__tr_t0, observed).
narrative_ontology:measurement(gpl__tr_t5, gpl_derivative_work_trigger__interface_boundary_reading, theater_ratio, 5, 0.2).
narrative_ontology:measurement_basis(gpl__tr_t5, observed).
narrative_ontology:measurement(gpl__tr_t10, gpl_derivative_work_trigger__interface_boundary_reading, theater_ratio, 10, 0.22).
narrative_ontology:measurement_basis(gpl__tr_t10, observed).
narrative_ontology:measurement(gpl__tr_t15, gpl_derivative_work_trigger__interface_boundary_reading, theater_ratio, 15, 0.25).
narrative_ontology:measurement_basis(gpl__tr_t15, observed).
narrative_ontology:measurement(gpl__tr_t20, gpl_derivative_work_trigger__interface_boundary_reading, theater_ratio, 20, 0.27).
narrative_ontology:measurement_basis(gpl__tr_t20, projected).
narrative_ontology:measurement(gpl__tr_t25, gpl_derivative_work_trigger__interface_boundary_reading, theater_ratio, 25, 0.28).
narrative_ontology:measurement_basis(gpl__tr_t25, projected).
narrative_ontology:measurement(gpl__tr_t30, gpl_derivative_work_trigger__interface_boundary_reading, theater_ratio, 30, 0.29).
narrative_ontology:measurement_basis(gpl__tr_t30, projected).
narrative_ontology:measurement(gpl__tr_t35, gpl_derivative_work_trigger__interface_boundary_reading, theater_ratio, 35, 0.29).
narrative_ontology:measurement_basis(gpl__tr_t35, projected).

% Extraction over time
narrative_ontology:measurement(gpl__be_t0, gpl_derivative_work_trigger__interface_boundary_reading, base_extractiveness, 0, 0.25).
narrative_ontology:measurement_basis(gpl__be_t0, observed).
narrative_ontology:measurement(gpl__be_t5, gpl_derivative_work_trigger__interface_boundary_reading, base_extractiveness, 5, 0.29).
narrative_ontology:measurement_basis(gpl__be_t5, observed).
narrative_ontology:measurement(gpl__be_t10, gpl_derivative_work_trigger__interface_boundary_reading, base_extractiveness, 10, 0.34).
narrative_ontology:measurement_basis(gpl__be_t10, observed).
narrative_ontology:measurement(gpl__be_t15, gpl_derivative_work_trigger__interface_boundary_reading, base_extractiveness, 15, 0.38).
narrative_ontology:measurement_basis(gpl__be_t15, observed).
narrative_ontology:measurement(gpl__be_t20, gpl_derivative_work_trigger__interface_boundary_reading, base_extractiveness, 20, 0.4).
narrative_ontology:measurement_basis(gpl__be_t20, projected).
narrative_ontology:measurement(gpl__be_t25, gpl_derivative_work_trigger__interface_boundary_reading, base_extractiveness, 25, 0.41).
narrative_ontology:measurement_basis(gpl__be_t25, projected).
narrative_ontology:measurement(gpl__be_t30, gpl_derivative_work_trigger__interface_boundary_reading, base_extractiveness, 30, 0.42).
narrative_ontology:measurement_basis(gpl__be_t30, projected).
narrative_ontology:measurement(gpl__be_t35, gpl_derivative_work_trigger__interface_boundary_reading, base_extractiveness, 35, 0.42).
narrative_ontology:measurement_basis(gpl__be_t35, projected).

% Suppression requirement over time
narrative_ontology:measurement(gpl__su_t0, gpl_derivative_work_trigger__interface_boundary_reading, suppression_requirement, 0, 0.32).
narrative_ontology:measurement_basis(gpl__su_t0, observed).
narrative_ontology:measurement(gpl__su_t5, gpl_derivative_work_trigger__interface_boundary_reading, suppression_requirement, 5, 0.34).
narrative_ontology:measurement_basis(gpl__su_t5, observed).
narrative_ontology:measurement(gpl__su_t10, gpl_derivative_work_trigger__interface_boundary_reading, suppression_requirement, 10, 0.36).
narrative_ontology:measurement_basis(gpl__su_t10, observed).
narrative_ontology:measurement(gpl__su_t15, gpl_derivative_work_trigger__interface_boundary_reading, suppression_requirement, 15, 0.37).
narrative_ontology:measurement_basis(gpl__su_t15, observed).
narrative_ontology:measurement(gpl__su_t20, gpl_derivative_work_trigger__interface_boundary_reading, suppression_requirement, 20, 0.38).
narrative_ontology:measurement_basis(gpl__su_t20, projected).
narrative_ontology:measurement(gpl__su_t25, gpl_derivative_work_trigger__interface_boundary_reading, suppression_requirement, 25, 0.38).
narrative_ontology:measurement_basis(gpl__su_t25, projected).
narrative_ontology:measurement(gpl__su_t30, gpl_derivative_work_trigger__interface_boundary_reading, suppression_requirement, 30, 0.38).
narrative_ontology:measurement_basis(gpl__su_t30, projected).
narrative_ontology:measurement(gpl__su_t35, gpl_derivative_work_trigger__interface_boundary_reading, suppression_requirement, 35, 0.38).
narrative_ontology:measurement_basis(gpl__su_t35, projected).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gpl_derivative_work_trigger__interface_boundary_reading, information_standard).
narrative_ontology:boltzmann_floor_override(gpl_derivative_work_trigger__interface_boundary_reading, 0.05).
narrative_ontology:affects_constraint(gpl_derivative_work_trigger__interface_boundary_reading, gpl_derivative_work_trigger__broad_copyleft_reading).
narrative_ontology:affects_constraint(gpl_derivative_work_trigger__interface_boundary_reading, gpl_derivative_work_trigger__narrow_linking_permissive_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the GPL derivative-work kernel. The broad_copyleft_reading interprets GPL linking obligations expansively (any coupling triggers source obligations); the narrow_linking_permissive_reading interprets them narrowly (only source modifications trigger obligations). The interface_boundary_reading (this story) splits the difference: technical boundaries permit aggregation even with tight coupling. All three readings share the same referent (GPL license interpretation) but produce different ε values, different beneficiary/victim structures, and different types. Each is a structurally distinct constraint with its own classification. Network edges link them as a constraint family where underlying legal clarification would likely consolidate or replace one reading with another.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
