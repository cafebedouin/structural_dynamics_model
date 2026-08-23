% ============================================================================
% CONSTRAINT STORY: gpl_derivative_work_trigger__interface_boundary_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
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
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: gpl_derivative_work_trigger__interface_boundary_reading
 *   human_readable: Clean API Boundary Non-Derivative Aggregation Reading
 *   domain: legal/software_licensing
 *
 * SUMMARY:
 *   This constraint story captures the 'interface boundary reading' of the
 *   GPL derivative work trigger — the interpretation that clean API
 *   boundaries constitute non-derivative aggregation (mere aggregation under
 *   GPL §2) even when the coupling is tight (dynamic linking, shared address
 *   space, frequent calls). This reading emerged from practical necessity:
 *   the Linux kernel's module interface, Android's userspace/kernel boundary,
 *   and plugin ecosystems all depend on it. It functions as a scaffold — a
 *   transitional legal interpretation that enabled modular mixed-licensing
 *   architectures to flourish — but lacks a formal sunset. The claimed type
 *   is scaffold (transitional coordination with sunset), while metrics show
 *   low extraction (0.35), low suppression (0.2), and low theater (0.15),
 *   consistent with a genuine coordination function that has persisted beyond
 *   its transitional justification.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gpl_derivative_work_trigger__interface_boundary_reading, 0.35).
domain_priors:suppression_score(gpl_derivative_work_trigger__interface_boundary_reading, 0.2).
domain_priors:theater_ratio(gpl_derivative_work_trigger__interface_boundary_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gpl_derivative_work_trigger__interface_boundary_reading, extractiveness, 0.35).
narrative_ontology:constraint_metric(gpl_derivative_work_trigger__interface_boundary_reading, suppression_requirement, 0.2).
narrative_ontology:constraint_metric(gpl_derivative_work_trigger__interface_boundary_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gpl_derivative_work_trigger__interface_boundary_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(gpl_derivative_work_trigger__interface_boundary_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gpl_derivative_work_trigger__interface_boundary_reading, scaffold).
narrative_ontology:human_readable(gpl_derivative_work_trigger__interface_boundary_reading, "Clean API Boundary Non-Derivative Aggregation Reading").
narrative_ontology:topic_domain(gpl_derivative_work_trigger__interface_boundary_reading, "legal/software_licensing").

narrative_ontology:has_sunset_clause(gpl_derivative_work_trigger__interface_boundary_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gpl_derivative_work_trigger__interface_boundary_reading, '9c05a26d-8ed6-45e6-86b7-0552a70fea78').
narrative_ontology:cs_kernel_codification('9c05a26d-8ed6-45e6-86b7-0552a70fea78', formalized).
narrative_ontology:cs_authority_grounding('9c05a26d-8ed6-45e6-86b7-0552a70fea78', lineage).
narrative_ontology:cs_interpretation_layer_present('9c05a26d-8ed6-45e6-86b7-0552a70fea78').
narrative_ontology:cs_reading_relation('9c05a26d-8ed6-45e6-86b7-0552a70fea78', gpl_derivative_work_trigger__broad_copyleft_reading, coexists_with).
narrative_ontology:cs_reading_relation('9c05a26d-8ed6-45e6-86b7-0552a70fea78', gpl_derivative_work_trigger__narrow_linking_permissive_reading, coexists_with).
narrative_ontology:cs_axiom('9c05a26d-8ed6-45e6-86b7-0552a70fea78', foundational, api_boundary_non_derivative).
narrative_ontology:cs_axiom_status(api_boundary_non_derivative, holdable).
narrative_ontology:cs_axiom_grounding('9c05a26d-8ed6-45e6-86b7-0552a70fea78', api_boundary_non_derivative, conventional).
narrative_ontology:cs_axiom('9c05a26d-8ed6-45e6-86b7-0552a70fea78', secondary, modular_composition_permissibility).
narrative_ontology:cs_axiom_status(modular_composition_permissibility, holdable).
narrative_ontology:cs_axiom_grounding('9c05a26d-8ed6-45e6-86b7-0552a70fea78', modular_composition_permissibility, conventional).
narrative_ontology:cs_reference_frame('9c05a26d-8ed6-45e6-86b7-0552a70fea78', gplv2_original_copyleft_intent).
narrative_ontology:cs_drift_state('9c05a26d-8ed6-45e6-86b7-0552a70fea78', post_gplv3_and_cloud_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('9c05a26d-8ed6-45e6-86b7-0552a70fea78', '').
narrative_ontology:cs_kernel_id(gpl_derivative_work_trigger__interface_boundary_reading, gpl_derivative_work_trigger).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gpl_derivative_work_trigger__interface_boundary_reading, ecosystem_integrators).
narrative_ontology:constraint_victim(gpl_derivative_work_trigger__interface_boundary_reading, users_expecting_full_stack_source).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(gpl_derivative_work_trigger__interface_boundary_reading, gpl_community_developers).
narrative_ontology:constraint_vindicates(gpl_derivative_work_trigger__interface_boundary_reading, modular_architecture_permissibility).
narrative_ontology:constraint_vindicates(gpl_derivative_work_trigger__interface_boundary_reading, api_boundary_non_derivative).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Companies and projects that build modular systems combining GPL and proprietary components through clean APIs (e.g., Android vendors, Linux kernel module authors, database extension developers). They gain legal certainty to ship mixed-licensing products without opening proprietary modules. Exit means switching to permissive-licensed alternatives or redesigning architecture — feasible but costly.
narrative_ontology:constraint_stakeholder(gpl_derivative_work_trigger__interface_boundary_reading, ecosystem_integrators, beneficiary,
    powerful, biographical, mobile, global).

% End users and downstream distributors who expect GPL's copyleft to propagate through the full software stack. Under this reading, proprietary modules linked via clean APIs remain closed, denying users the ability to study, modify, or redistribute the complete system. Exit means avoiding such systems or demanding fully-free alternatives — constrained by market dominance of mixed-licensing platforms.
narrative_ontology:constraint_stakeholder(gpl_derivative_work_trigger__interface_boundary_reading, users_expecting_full_stack_source, payer,
    moderate, biographical, constrained, global).

% Developers who contribute to GPL projects and shape community norms around linking boundaries. They benefit from ecosystem growth attracted by modular flexibility, but also police the boundary to prevent abuse. Their exit is constrained by identity investment in the GPL ecosystem and the network effects of their contributions.
narrative_ontology:constraint_stakeholder(gpl_derivative_work_trigger__interface_boundary_reading, gpl_community_developers, agenda_setter,
    organized, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(gpl_derivative_work_trigger__interface_boundary_reading, gpl_community_developers, beneficiary).

% Free Software Foundation and aligned advocates who maintain that dynamic linking creates derivative works. They are structurally excluded from the governance of this reading — courts and industry practice have moved toward interface boundaries despite FSF guidance. Their identity is fused with the broad copyleft frame; exit would mean abandoning the theoretical core of their advocacy.
narrative_ontology:constraint_stakeholder(gpl_derivative_work_trigger__interface_boundary_reading, fsf_copyleft_advocates, excluded,
    institutional, generational, identity_locked, global).

% Judicial bodies that adjudicate GPL disputes (e.g., US courts on API copyrightability, European courts on interoperability). They observe the constraint's operation through litigation but do not directly collect or pay. Their analytical seat shapes the constraint's evolution through precedent.
narrative_ontology:constraint_stakeholder(gpl_derivative_work_trigger__interface_boundary_reading, courts_legal_authorities, observer,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Enables modular software architecture where independently licensed components interoperate through defined interfaces without triggering full copyleft propagation — solving the coordination problem of how GPL code can participate in plugin, kernel-module, and microservice ecosystems.
% TRANSFER_FUNCTION: Permits proprietary/commercial modules to link with GPL code through clean API boundaries without requiring source disclosure of the proprietary modules. The transfer is: ecosystem integrators gain legal freedom to keep modules closed; users lose full-stack source access that the broad reading would guarantee.
% ABSENT_VOICES: End users who expected GPL's copyleft to guarantee full source availability for the entire running system; downstream recipients of integrated appliances (routers, IoT devices, mobile phones) who cannot access or modify proprietary modules; small developers who lack leverage to negotiate source access from integrators.
% DISAPPEARANCE_RATIONALE: If this reading vanished overnight, the legal basis for mixed-licensing modular architectures (Android userspace/kernel, Linux proprietary kernel modules, database extensions, VS Code extensions) would collapse. Integrators would face immediate litigation risk, forcing either full source disclosure or architectural rewrites to avoid GPL code — the software ecosystem would reorganize around either permissive licenses or strict copyleft compliance.
% FOUNDING_PROBLEM: Early GPL enforcement uncertainty around dynamic linking, plugin architectures, and kernel modules chilled modular ecosystem development — companies avoided GPL code entirely rather than risk copyleft contagion, reducing GPL software's reach and collaborative improvement.
% FOUNDING_PROBLEM_CORROBORATION: Linux kernel's EXPORT_SYMBOL_GPL vs non-GPL distinction (Linus Torvalds, 2004+) demonstrates the kernel community's practical adoption of interface boundaries. Android's userspace/kernel boundary (Google, 2008+) shows commercial reliance. FSF's own FAQ evolution (from 'linking creates derivative' to nuanced 'it depends on intimacy of communication') shows internal contestation. Independent corroboration: European Court of Justice SAS Institute v World Programming (2012) on API non-copyrightability; US Federal Circuit Google v Oracle (2021) fair use ruling on API reimplementation.
narrative_ontology:disappearance_verdict(gpl_derivative_work_trigger__interface_boundary_reading, world_rearranges).
narrative_ontology:founding_problem_status(gpl_derivative_work_trigger__interface_boundary_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gpl_derivative_work_trigger__interface_boundary_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(gpl_derivative_work_trigger__interface_boundary_reading, 'none', 1).
narrative_ontology:epsilon_provenance(gpl_derivative_work_trigger__interface_boundary_reading, 0.35, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Extractiveness is moderate (0.35) because the reading transfers value from users (who lose full-stack source) to integrators (who gain proprietary module freedom) — but the transfer is partial and bounded by the API boundary definition. Suppression is low (0.2) because alternatives exist: fully-free stacks (PureOS, Guix), permissive-licensed alternatives (BSD, MIT), and the broad copyleft reading itself remains legally arguable. Theater is low (0.15) — the coordination function (modular interoperability) is real and actively used, not performative. The scaffold has persisted 30+ years without a declared sunset, raising mandatrophy questions.
 *
 * PERSPECTIVAL GAP:
 *   From integrator seats, this is a rope — genuine coordination enabling modular ecosystems. From user seats, it's a snare — extraction of source rights they were promised by GPL's text. From FSF seats, it's a false summit — a constructed interpretation masquerading as the natural reading of the license. The engine computes this divergence from the structural data; the authored claim (scaffold) captures the temporal dimension: a coordination function that was meant to be transitional but has become entrenched.
 *
 * DIRECTIONALITY LOGIC:
 *   Ecosystem integrators are beneficiaries (d ~0.15) — they collect the freedom to keep modules closed while using GPL infrastructure. Users expecting full-stack source are payers (d ~0.75) — they bear the cost of lost source access, with constrained exit due to market dominance of mixed platforms. GPL community developers sit near symmetric (d ~0.5) — they both set the agenda and benefit from ecosystem growth. FSF advocates are identity-locked excluded (d ~0.9) — their advocacy frame is structurally incompatible with this reading. Courts are analytical observers (d ~0.5) — they adjudicate but don't directly collect or pay.
 *
 * MANDATROPHY ANALYSIS:
 *   The scaffold's founding problem (GPL linking uncertainty chilling modular ecosystems) is contested — some argue modular ecosystems now thrive independently; others say the reading still enables vital commercial participation. The sunset clause is implicit (the reading was justified as 'until courts clarify' or 'until GPLv3 fixes this') but never formalized. GPLv3 (§2, §13) attempted to clarify but the interface boundary practice persists. Mandatrophy is unresolved: the constraint persists because integrators benefit, users lack coalition power, and courts have not definitively ruled.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    api_boundary_objectivity,
    'Are clean API boundaries objectively definable in software architecture, or is ''clean boundary'' inherently a matter of degree that collapses under scrutiny?',
    'Technical analysis of real-world module interfaces (kernel syscalls, library ABIs, plugin APIs, microservice contracts) to measure coupling metrics (call frequency, shared state, data structure interdependence) and test whether a natural clustering exists.',
    'If boundaries are objectively definable, the scaffold rests on a stable technical foundation. If inherently fuzzy, the reading''s coordination function is unstable — any boundary can be argued as ''not clean enough'', reverting to broad copyleft or requiring case-by-case litigation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(api_boundary_objectivity, empirical, 'Whether the API boundary delimiter has objective technical reality').

omega_variable(
    scaffold_permanence,
    'Has this scaffold''s transitional justification (awaiting legal clarity) expired, making it a de facto permanent regime, or does it retain genuine transitional character?',
    'Track whether major jurisdictions'' courts or the FSF have issued definitive guidance that would obsolete the scaffold, and whether industry practice has converged on a stable boundary independent of the reading.',
    'If the scaffold has become permanent without declaring itself so, it operates as a piton — inertial maintenance of a transitional regime. If genuinely transitional, it should show declining relevance as legal clarity emerges.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(scaffold_permanence, conceptual, 'Whether the scaffold''s sunset has effectively passed without acknowledgment').

omega_variable(
    user_coalition_potential,
    'Can users expecting full-stack source form an effective coalition to challenge this reading, or are they structurally fragmented across jurisdictions, use-cases, and technical literacy?',
    'Analyze historical litigation patterns (user class actions, regulatory complaints, right-to-repair movements) and measure whether user interests have ever cohered into sustained legal pressure on the interface boundary.',
    'If coalition is impossible, the payer seat remains permanently weak — the scaffold faces no effective counterweight. If coalition is possible, the reading''s stability depends on continued legal ambiguity.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(user_coalition_potential, empirical, 'Whether the victim class can organize to contest the extraction').

omega_variable(
    kernel_reading_framing_ambiguity,
    'Does this reading''s framing as ''non-derivative aggregation'' accurately capture its structural operation, or does it obscure a de facto linking exception that the GPL text does not support?',
    'Comparative textual analysis: GPLv2 §2 ''mere aggregation'' language vs. GPLv3 §13 ''interact through well-defined interfaces'' vs. actual industry practice. Test whether the reading''s boundary aligns with any textual anchor or creates a new category.',
    'If the reading creates a category not in the license text, it is a judicial/industry innovation — a scaffold with no textual sunset. If it faithfully implements ''mere aggregation'', its legitimacy is textual, not pragmatic.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_framing_ambiguity, conceptual, 'Whether the reading''s legal framing matches its structural operation').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gpl_derivative_work_trigger__interface_boundary_reading, 1991, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gpl__tr_t1991, gpl_derivative_work_trigger__interface_boundary_reading, theater_ratio, 1991, 0.05).
narrative_ontology:measurement(gpl__tr_t1999, gpl_derivative_work_trigger__interface_boundary_reading, theater_ratio, 1999, 0.08).
narrative_ontology:measurement(gpl__tr_t2004, gpl_derivative_work_trigger__interface_boundary_reading, theater_ratio, 2004, 0.12).
narrative_ontology:measurement(gpl__tr_t2007, gpl_derivative_work_trigger__interface_boundary_reading, theater_ratio, 2007, 0.14).
narrative_ontology:measurement(gpl__tr_t2012, gpl_derivative_work_trigger__interface_boundary_reading, theater_ratio, 2012, 0.15).
narrative_ontology:measurement(gpl__tr_t2018, gpl_derivative_work_trigger__interface_boundary_reading, theater_ratio, 2018, 0.15).
narrative_ontology:measurement(gpl__tr_t2024, gpl_derivative_work_trigger__interface_boundary_reading, theater_ratio, 2024, 0.15).

% Extraction over time
narrative_ontology:measurement(gpl__be_t1991, gpl_derivative_work_trigger__interface_boundary_reading, base_extractiveness, 1991, 0.1).
narrative_ontology:measurement(gpl__be_t1999, gpl_derivative_work_trigger__interface_boundary_reading, base_extractiveness, 1999, 0.15).
narrative_ontology:measurement(gpl__be_t2004, gpl_derivative_work_trigger__interface_boundary_reading, base_extractiveness, 2004, 0.25).
narrative_ontology:measurement(gpl__be_t2007, gpl_derivative_work_trigger__interface_boundary_reading, base_extractiveness, 2007, 0.3).
narrative_ontology:measurement(gpl__be_t2012, gpl_derivative_work_trigger__interface_boundary_reading, base_extractiveness, 2012, 0.33).
narrative_ontology:measurement(gpl__be_t2018, gpl_derivative_work_trigger__interface_boundary_reading, base_extractiveness, 2018, 0.35).
narrative_ontology:measurement(gpl__be_t2024, gpl_derivative_work_trigger__interface_boundary_reading, base_extractiveness, 2024, 0.35).

% Suppression requirement over time
narrative_ontology:measurement(gpl__su_t1991, gpl_derivative_work_trigger__interface_boundary_reading, suppression_requirement, 1991, 0.1).
narrative_ontology:measurement(gpl__su_t1999, gpl_derivative_work_trigger__interface_boundary_reading, suppression_requirement, 1999, 0.15).
narrative_ontology:measurement(gpl__su_t2004, gpl_derivative_work_trigger__interface_boundary_reading, suppression_requirement, 2004, 0.18).
narrative_ontology:measurement(gpl__su_t2007, gpl_derivative_work_trigger__interface_boundary_reading, suppression_requirement, 2007, 0.2).
narrative_ontology:measurement(gpl__su_t2012, gpl_derivative_work_trigger__interface_boundary_reading, suppression_requirement, 2012, 0.2).
narrative_ontology:measurement(gpl__su_t2018, gpl_derivative_work_trigger__interface_boundary_reading, suppression_requirement, 2018, 0.2).
narrative_ontology:measurement(gpl__su_t2024, gpl_derivative_work_trigger__interface_boundary_reading, suppression_requirement, 2024, 0.2).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gpl_derivative_work_trigger__interface_boundary_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(gpl_derivative_work_trigger__interface_boundary_reading, 0.1).
narrative_ontology:affects_constraint(gpl_derivative_work_trigger__interface_boundary_reading, gpl_derivative_work_trigger__broad_copyleft_reading).
narrative_ontology:affects_constraint(gpl_derivative_work_trigger__interface_boundary_reading, gpl_derivative_work_trigger__narrow_linking_permissive_reading).

% DUAL FORMULATION NOTE:
% This constraint family decomposes the GPL derivative work trigger into three structurally distinct readings linked by the same kernel (GPL text). The interface_boundary_reading enables a scaffold of modular mixed-licensing (moderate extraction, low suppression). The broad_copyleft_reading would be a snare for integrators (high extraction, high suppression). The narrow_linking_permissive_reading would be a rope for developers (low extraction, coordination-only). Their ε values differ structurally — they are not measurement variants of one constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(gpl_derivative_work_trigger__interface_boundary_reading, institutional, 0.25).
constraint_indexing:directionality_override(gpl_derivative_work_trigger__interface_boundary_reading, organized, 0.45).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
