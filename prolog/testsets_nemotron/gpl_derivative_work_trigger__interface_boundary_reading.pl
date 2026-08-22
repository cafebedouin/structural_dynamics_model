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
 *   constraint_id: gpl_derivative_work_trigger__interface_boundary_reading
 *   human_readable: GPL Derivative Work Trigger — Interface Boundary Reading
 *   domain: software_licensing/copyright_law/open_source_governance
 *
 * SUMMARY:
 *   This constraint story captures the 'interface boundary reading' of the
 *   GPL's derivative work trigger — the position that clean API boundaries
 *   (well-defined interfaces, stable ABIs, documented contracts) constitute
 *   mere aggregation rather than derivative works, even when the coupling
 *   between modules is tight (frequent calls, shared data structures,
 *   synchronous interaction). This reading emerged as a practical
 *   accommodation in the 1990s-2000s to allow proprietary kernel modules,
 *   commercial extensions, and mixed-licensing ecosystems (Linux kernel +
 *   proprietary drivers, GCC + proprietary plugins, Qt dual-licensing). The
 *   reading functions as a scaffold: it enables a transitional modular
 *   architecture where GPL and proprietary code coexist, justified by the
 *   transition to componentized software ecosystems, but carries an implicit
 *   sunset — if courts or the FSF definitively rule that tight coupling
 *   across API boundaries creates derivative works, the scaffold collapses
 *   and the broad copyleft reading takes over. Beneficiaries are ecosystem
 *   integrators (Linux distributors, Android OEMs, commercial framework
 *   vendors) who build modular stacks mixing GPL and proprietary components.
 *   Victims are users and community contributors who expect the GPL's
 *   copyleft to propagate across module boundaries, securing full-stack
 *   source freedom. The constraint requires active enforcement (FSF
 *   compliance lab, kernel community social pressure, dual-license vendor
 *   audits) to maintain the boundary.
 *
 * KEY AGENTS:
 *   - ecosystem_integrators: Primary beneficiary (institutional/arbitrage) — builds mixed-licensing stacks on the boundary
 *   - commercial_dual_license_vendors: Primary beneficiary (institutional/arbitrage) — sells proprietary licenses enabled by the boundary
 *   - modular_framework_maintainers: Secondary beneficiary (organized/constrained) — gains adoption through proprietary-friendly boundaries
 *   - copyleft_expectant_users: Primary victim (powerless/trapped) — loses full-stack source when boundary is invoked
 *   - community_contributors_expecting_full_stack_freedom: Primary victim (organized/identity_locked) — contributes under GPL expecting copyleft propagation
 *   - fsf_compliance_lab: Agenda setter (institutional/analytical) — defines and enforces the boundary through guidance and litigation
 *   - kernel_maintainers: Agenda setter (institutional/constrained) — sets practical boundary through symbol export policy (EXPORT_SYMBOL vs EXPORT_SYMBOL_GPL)
 *   - court_system: Observer (institutional/analytical) — ultimate arbiter of derivative work definition
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gpl_derivative_work_trigger__interface_boundary_reading, 0.22).
domain_priors:suppression_score(gpl_derivative_work_trigger__interface_boundary_reading, 0.18).
domain_priors:theater_ratio(gpl_derivative_work_trigger__interface_boundary_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gpl_derivative_work_trigger__interface_boundary_reading, extractiveness, 0.22).
narrative_ontology:constraint_metric(gpl_derivative_work_trigger__interface_boundary_reading, suppression_requirement, 0.18).
narrative_ontology:constraint_metric(gpl_derivative_work_trigger__interface_boundary_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gpl_derivative_work_trigger__interface_boundary_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(gpl_derivative_work_trigger__interface_boundary_reading, resistance, 0.52).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gpl_derivative_work_trigger__interface_boundary_reading, scaffold).
narrative_ontology:human_readable(gpl_derivative_work_trigger__interface_boundary_reading, "GPL Derivative Work Trigger — Interface Boundary Reading").
narrative_ontology:topic_domain(gpl_derivative_work_trigger__interface_boundary_reading, "software_licensing/copyright_law/open_source_governance").

domain_priors:requires_active_enforcement(gpl_derivative_work_trigger__interface_boundary_reading).
narrative_ontology:has_sunset_clause(gpl_derivative_work_trigger__interface_boundary_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gpl_derivative_work_trigger__interface_boundary_reading, 'b2886539-6180-4d0f-b1d1-7a2ae54faf59').
narrative_ontology:cs_kernel_codification('b2886539-6180-4d0f-b1d1-7a2ae54faf59', formalized).
narrative_ontology:cs_authority_grounding('b2886539-6180-4d0f-b1d1-7a2ae54faf59', lineage).
narrative_ontology:cs_interpretation_layer_present('b2886539-6180-4d0f-b1d1-7a2ae54faf59').
narrative_ontology:cs_reading_relation('b2886539-6180-4d0f-b1d1-7a2ae54faf59', gpl_derivative_work_trigger__broad_copyleft_reading, coexists_with).
narrative_ontology:cs_reading_relation('b2886539-6180-4d0f-b1d1-7a2ae54faf59', gpl_derivative_work_trigger__narrow_linking_permissive_reading, influences).
narrative_ontology:cs_axiom('b2886539-6180-4d0f-b1d1-7a2ae54faf59', foundational, interface_specification_is_not_derivative_work).
narrative_ontology:cs_axiom_status(interface_specification_is_not_derivative_work, holdable).
narrative_ontology:cs_axiom_grounding('b2886539-6180-4d0f-b1d1-7a2ae54faf59', interface_specification_is_not_derivative_work, conventional).
narrative_ontology:cs_axiom('b2886539-6180-4d0f-b1d1-7a2ae54faf59', foundational, modular_architecture_requires_licensing_boundaries).
narrative_ontology:cs_axiom_status(modular_architecture_requires_licensing_boundaries, holdable).
narrative_ontology:cs_axiom_grounding('b2886539-6180-4d0f-b1d1-7a2ae54faf59', modular_architecture_requires_licensing_boundaries, instrumental).
narrative_ontology:cs_reference_frame('b2886539-6180-4d0f-b1d1-7a2ae54faf59', gplv2_mere_aggregation_doctrine).
narrative_ontology:cs_drift_state('b2886539-6180-4d0f-b1d1-7a2ae54faf59', post_gplv3_agpl_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('b2886539-6180-4d0f-b1d1-7a2ae54faf59', '').
narrative_ontology:cs_kernel_id(gpl_derivative_work_trigger__interface_boundary_reading, gpl_derivative_work_trigger).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gpl_derivative_work_trigger__interface_boundary_reading, ecosystem_integrators).
narrative_ontology:constraint_beneficiary(gpl_derivative_work_trigger__interface_boundary_reading, commercial_dual_license_vendors).
narrative_ontology:constraint_beneficiary(gpl_derivative_work_trigger__interface_boundary_reading, modular_framework_maintainers).
narrative_ontology:constraint_victim(gpl_derivative_work_trigger__interface_boundary_reading, copyleft_expectant_users).
narrative_ontology:constraint_victim(gpl_derivative_work_trigger__interface_boundary_reading, community_contributors_expecting_full_stack_freedom).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(gpl_derivative_work_trigger__interface_boundary_reading, modular_framework_maintainers).
narrative_ontology:constraint_vindicates(gpl_derivative_work_trigger__interface_boundary_reading, modular_architecture_enables_mixed_licensing).
narrative_ontology:constraint_vindicates(gpl_derivative_work_trigger__interface_boundary_reading, interface_specification_is_not_derivative_work).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Build and distribute mixed-licensing software stacks (Linux distributions, Android, embedded systems) that combine GPL core with proprietary modules. They rely on the interface boundary reading to avoid GPL obligations on proprietary components. They can switch to permissive-licensed alternatives (BSD, MIT) but lose the GPL ecosystem's scale and maturity.
narrative_ontology:constraint_stakeholder(gpl_derivative_work_trigger__interface_boundary_reading, ecosystem_integrators, beneficiary,
    institutional, generational, arbitrage, global).

% Sell proprietary licenses for GPL-licensed frameworks (Qt, MySQL, Redis Enterprise) enabled by the interface boundary reading. The boundary lets them keep proprietary modules closed while the GPL core drives adoption. They can relicense or open-source but lose the dual-license revenue model.
narrative_ontology:constraint_stakeholder(gpl_derivative_work_trigger__interface_boundary_reading, commercial_dual_license_vendors, beneficiary,
    institutional, generational, arbitrage, global).

% Maintain GPL frameworks (GCC, GNOME, KDE) that gain adoption through proprietary-friendly plugin boundaries. They benefit from ecosystem growth but bear maintenance costs and community pressure to tighten boundaries. Their exit is constrained by contributor dependence and project governance.
narrative_ontology:constraint_stakeholder(gpl_derivative_work_trigger__interface_boundary_reading, modular_framework_maintainers, beneficiary,
    organized, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(gpl_derivative_work_trigger__interface_boundary_reading, modular_framework_maintainers, payer).

% Use GPL software expecting full-stack source freedom. When vendors invoke the interface boundary to keep drivers/plugins proprietary, users lose the ability to modify, audit, or replace those components. They cannot practically reconstruct full stacks; switching to fully-free alternatives often means accepting inferior hardware support or features.
narrative_ontology:constraint_stakeholder(gpl_derivative_work_trigger__interface_boundary_reading, copyleft_expectant_users, payer,
    powerless, biographical, trapped, global).

% Contribute code to GPL projects under the expectation that copyleft propagates across module boundaries. When the interface boundary reading shields proprietary modules, their contributions indirectly enable commercial capture. Their identity is fused with the GPL's promise — leaving the community means abandoning their professional and ideological commitment.
narrative_ontology:constraint_stakeholder(gpl_derivative_work_trigger__interface_boundary_reading, community_contributors_expecting_full_stack_freedom, payer,
    organized, biographical, identity_locked, global).

% Publishes guidance on derivative work boundaries, pursues compliance actions, and shapes the FSF's official reading. They administer the constraint through interpretive authority but are constrained by the kernel's text and community legitimacy. Their exit is analytical — they observe and shape but do not personally bear extraction.
narrative_ontology:constraint_stakeholder(gpl_derivative_work_trigger__interface_boundary_reading, fsf_compliance_lab, agenda_setter,
    institutional, generational, analytical, global).

% Set practical boundaries through Linux kernel symbol export policy (EXPORT_SYMBOL vs EXPORT_SYMBOL_GPL). They directly define which interfaces are 'clean' enough for proprietary modules. They face pressure from both commercial vendors (want more symbols) and copyleft advocates (want fewer). Their exit is constrained by the kernel's institutional position.
narrative_ontology:constraint_stakeholder(gpl_derivative_work_trigger__interface_boundary_reading, kernel_maintainers, agenda_setter,
    institutional, generational, constrained, global).

% Ultimate arbiter of derivative work definition in copyright law. Rulings on API copyrightability (Google v. Oracle) and software structure/sequence/organization determine whether the interface boundary reading holds legally. They observe the dispute but do not collect from or pay into the constraint.
narrative_ontology:constraint_stakeholder(gpl_derivative_work_trigger__interface_boundary_reading, court_system, observer,
    institutional, civilizational, analytical, national).

% Would compete to provide proprietary modules but are bound by the boundary definitions set by kernel maintainers and FSF. They benefit from the reading's existence but have no voice in shaping it — they are price-takers of the boundary definition.
narrative_ontology:constraint_stakeholder(gpl_derivative_work_trigger__interface_boundary_reading, proprietary_module_vendors, excluded,
    powerful, biographical, trapped, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(gpl_derivative_work_trigger__interface_boundary_reading, commercial_dual_license_vendors).
narrative_ontology:fixing_cost_class(gpl_derivative_work_trigger__interface_boundary_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Enables modular software architecture where GPL-licensed cores and proprietary modules interoperate through stable interfaces, allowing componentized development and commercial sustainability without requiring full-source disclosure.
% TRANSFER_FUNCTION: Moves the value of proprietary module differentiation (features, hardware support, performance) from users (who lose modification freedom) to commercial vendors (who capture monopoly rents on proprietary modules), while the GPL core provides the coordination substrate (shared infrastructure, community maintenance).
% ABSENT_VOICES: End users of embedded systems (routers, IoT devices, mobile phones) who cannot practically exercise GPL rights even when source is available; they would object to proprietary modules blocking device ownership but are structurally excluded from licensing discourse. Also absent: future contributors who would have joined a fully-free ecosystem but are deterred by the mixed-licensing equilibrium.
% DISAPPEARANCE_RATIONALE: If the interface boundary reading vanished overnight (courts/FSF definitively rule tight coupling = derivative work), proprietary kernel modules and plugins would face immediate GPL compliance demands. Vendors would either open-source modules, withdraw them, or migrate to permissive-licensed kernels. The Linux/Android/embedded ecosystem would reorganize around either full copyleft or permissive foundations — a major structural rearrangement.
% FOUNDING_PROBLEM: Early GPL enforcement (1990s) threatened to prevent modular architectures where proprietary components (drivers, plugins, libraries) needed to interoperate with GPL cores. The interface boundary reading emerged as a practical accommodation to allow Linux kernel modules, GCC plugins, and Qt commercial licensing — enabling the componentized software ecosystems that dominated 2000s-2010s.
% FOUNDING_PROBLEM_CORROBORATION: Linux kernel maintainers and commercial vendors attest the problem remains live (hardware diversity requires proprietary drivers). FSF and copyleft advocates attest the problem is substantially solved by permissive-licensed alternatives and cloud deployment models. Independent analysis (academic licensing studies, EU Commission reports) corroborates the shifted-function reading: the coordination benefit persists but the extraction profile has tilted toward commercial capture.
narrative_ontology:disappearance_verdict(gpl_derivative_work_trigger__interface_boundary_reading, world_rearranges).
narrative_ontology:founding_problem_status(gpl_derivative_work_trigger__interface_boundary_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gpl_derivative_work_trigger__interface_boundary_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(gpl_derivative_work_trigger__interface_boundary_reading, 'none', 1).
narrative_ontology:epsilon_provenance(gpl_derivative_work_trigger__interface_boundary_reading, 0.22, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Extractiveness is low (0.22) because the reading primarily enables coordination (modular architecture) rather than extracting value — the extraction that occurs is the differential value captured by commercial vendors who keep proprietary modules closed while benefiting from GPL infrastructure. Suppression is low (0.18) because compliance is largely voluntary and community-norm-driven; active litigation is rare. Theater ratio is moderate-high (0.41) because the 'clean API boundary' criterion is inherently vague and strategically deployed — integrators claim clean boundaries while maintaining deep coupling, and the boundary definition shifts with legal risk. Accessibility collapse is moderate (0.35) because alternatives exist (fully free stacks, permissive-licensed alternatives) but are often practically inferior. Resistance is moderate-high (0.52) because the copyleft community actively contests this reading and pushes for broader derivative work definitions. The claimed type is scaffold because the reading explicitly serves as a transitional accommodation for modular architecture with a sunset clause (GPLv3's anti-tivoization provisions and FSF's evolving guidance signal the transition).
 *
 * PERSPECTIVAL GAP:
 *   From the ecosystem integrator seat (arbitrage exit, institutional power), the constraint is a genuine coordination scaffold enabling modular innovation. From the copyleft-expectant user seat (trapped exit, powerless), the same constraint is a snare that extracts their expected freedom through definitional capture. From the kernel maintainer seat (constrained exit, institutional power), the constraint is a tangled rope — they coordinate the module interface but bear pressure from both sides. The engine computes these divergent seat classifications from the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (ecosystem_integrators, commercial_dual_license_vendors, modular_framework_maintainers) collect value from the boundary — they build businesses on mixed licensing, gain adoption, avoid GPL obligations on proprietary modules. Their exit is arbitrage/constrained (they can switch licenses or frameworks but lose ecosystem position). Victims (copyleft_expectant_users, community_contributors) bear the cost — they lose source access and modification freedom for proprietary modules that the boundary shields. Their exit is trapped/identity_locked (users cannot practically reconstruct full stacks; contributors have fused identity with the GPL's promise). Agenda setters (FSF, kernel maintainers) administer the boundary — FSF through guidance and litigation, maintainers through symbol export policy. Their directionality is near-symmetric (they both enforce and are constrained by the boundary). Rival proprietary module vendors are excluded — they would compete but the boundary defines the terms of their participation.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's founding problem (enabling modular architecture in a world where GPL's viral copyleft would otherwise prevent mixed-licensing ecosystems) was live in 1991-2010. By 2025, the problem is contested: permissive-licensed alternatives (BSD, MIT, Apache) now provide modular foundations without copyleft ambiguity; cloud deployment models shift the derivative work question to network boundaries (AGPL); and the FSF's guidance has narrowed. The scaffold's sunset clause (GPLv3's anti-tivoization, FSF's 'mere aggregation' FAQ narrowing) is being triggered but the transition is incomplete — the constraint persists as a piton candidate in mature ecosystems (Linux kernel, Qt) where the modular architecture is entrenched. Mandatrophy is partially resolved: the coordination function remains but the extraction profile has shifted toward commercial capture.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is this constraint a distinct reading of the GPL derivative work trigger kernel, or an independent constraint?',
    'Structural comparison of beneficiary/victim sets and ε values across declared readings of gpl_derivative_work_trigger kernel',
    'If same kernel, ε differs from broad_copyleft_reading (high) and narrow_linking_permissive_reading (low); linkage via network.affects_constraints required',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Commitment kernel identity for this reading').

omega_variable(
    scaffold_transition_target,
    'What stable state does this scaffold transition toward — broad copyleft enforcement, narrow linking permission, or persistent mixed-licensing equilibrium?',
    'Track judicial rulings, FSF guidance evolution, and ecosystem licensing patterns over the next decade',
    'If transition target is broad copyleft, scaffold resolves to tangled_rope/snare; if narrow linking, resolves to rope; if equilibrium, becomes piton',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(scaffold_transition_target, empirical, 'Scaffold sunset resolution ambiguity').

omega_variable(
    interface_boundary_definition,
    'What constitutes a ''clean API boundary'' sufficient to avoid derivative work status under this reading?',
    'Circuit court rulings on API copyrightability (Google v. Oracle lineage) and FSF FAQ updates',
    'Boundary vagueness enables strategic ambiguity — integrators claim clean boundaries while maintaining tight coupling; clarification would shift extraction profile',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(interface_boundary_definition, conceptual, 'Structural ambiguity of the coordination function''s boundary condition').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (legal threat, license termination) or internalized (community norm compliance, fear of ostracism)?',
    'Post-litigation behavior: if developers comply without active enforcement, reclassify as partially internalized',
    'If internalized, effective suppression is higher than structural measure suggests — the constraint persists without active enforcement',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression in license compliance').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gpl_derivative_work_trigger__interface_boundary_reading, 1991, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gpl_derivative_work_trigger__interface_boundary_reading_tr_t1991, gpl_derivative_work_trigger__interface_boundary_reading, theater_ratio, 1991, 0.15).
narrative_ontology:measurement(gpl_derivative_work_trigger__interface_boundary_reading_tr_t1999, gpl_derivative_work_trigger__interface_boundary_reading, theater_ratio, 1999, 0.22).
narrative_ontology:measurement(gpl_derivative_work_trigger__interface_boundary_reading_tr_t2007, gpl_derivative_work_trigger__interface_boundary_reading, theater_ratio, 2007, 0.35).
narrative_ontology:measurement(gpl_derivative_work_trigger__interface_boundary_reading_tr_t2015, gpl_derivative_work_trigger__interface_boundary_reading, theater_ratio, 2015, 0.41).
narrative_ontology:measurement(gpl_derivative_work_trigger__interface_boundary_reading_tr_t2020, gpl_derivative_work_trigger__interface_boundary_reading, theater_ratio, 2020, 0.41).
narrative_ontology:measurement(gpl_derivative_work_trigger__interface_boundary_reading_tr_t2025, gpl_derivative_work_trigger__interface_boundary_reading, theater_ratio, 2025, 0.41).

% Extraction over time
narrative_ontology:measurement(gpl_derivative_work_trigger__interface_boundary_reading_be_t1991, gpl_derivative_work_trigger__interface_boundary_reading, base_extractiveness, 1991, 0.08).
narrative_ontology:measurement(gpl_derivative_work_trigger__interface_boundary_reading_be_t1999, gpl_derivative_work_trigger__interface_boundary_reading, base_extractiveness, 1999, 0.12).
narrative_ontology:measurement(gpl_derivative_work_trigger__interface_boundary_reading_be_t2007, gpl_derivative_work_trigger__interface_boundary_reading, base_extractiveness, 2007, 0.18).
narrative_ontology:measurement(gpl_derivative_work_trigger__interface_boundary_reading_be_t2015, gpl_derivative_work_trigger__interface_boundary_reading, base_extractiveness, 2015, 0.22).
narrative_ontology:measurement(gpl_derivative_work_trigger__interface_boundary_reading_be_t2020, gpl_derivative_work_trigger__interface_boundary_reading, base_extractiveness, 2020, 0.22).
narrative_ontology:measurement(gpl_derivative_work_trigger__interface_boundary_reading_be_t2025, gpl_derivative_work_trigger__interface_boundary_reading, base_extractiveness, 2025, 0.22).

% Suppression requirement over time
narrative_ontology:measurement(gpl_derivative_work_trigger__interface_boundary_reading_su_t1991, gpl_derivative_work_trigger__interface_boundary_reading, suppression_requirement, 1991, 0.05).
narrative_ontology:measurement(gpl_derivative_work_trigger__interface_boundary_reading_su_t1999, gpl_derivative_work_trigger__interface_boundary_reading, suppression_requirement, 1999, 0.08).
narrative_ontology:measurement(gpl_derivative_work_trigger__interface_boundary_reading_su_t2007, gpl_derivative_work_trigger__interface_boundary_reading, suppression_requirement, 2007, 0.12).
narrative_ontology:measurement(gpl_derivative_work_trigger__interface_boundary_reading_su_t2015, gpl_derivative_work_trigger__interface_boundary_reading, suppression_requirement, 2015, 0.18).
narrative_ontology:measurement(gpl_derivative_work_trigger__interface_boundary_reading_su_t2020, gpl_derivative_work_trigger__interface_boundary_reading, suppression_requirement, 2020, 0.18).
narrative_ontology:measurement(gpl_derivative_work_trigger__interface_boundary_reading_su_t2025, gpl_derivative_work_trigger__interface_boundary_reading, suppression_requirement, 2025, 0.18).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gpl_derivative_work_trigger__interface_boundary_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(gpl_derivative_work_trigger__interface_boundary_reading, 0.15).
narrative_ontology:affects_constraint(gpl_derivative_work_trigger__interface_boundary_reading, gpl_derivative_work_trigger__broad_copyleft_reading).
narrative_ontology:affects_constraint(gpl_derivative_work_trigger__interface_boundary_reading, gpl_derivative_work_trigger__narrow_linking_permissive_reading).
narrative_ontology:affects_constraint(gpl_derivative_work_trigger__interface_boundary_reading, gplv3_anti_tivoization_provision).
narrative_ontology:affects_constraint(gpl_derivative_work_trigger__interface_boundary_reading, agpl_network_boundary_trigger).
narrative_ontology:affects_constraint(gpl_derivative_work_trigger__interface_boundary_reading, linux_kernel_symbol_export_policy).
narrative_ontology:affects_constraint(gpl_derivative_work_trigger__interface_boundary_reading, qt_dual_licensing_model).

% DUAL FORMULATION NOTE:
% This constraint family (gpl_derivative_work_trigger kernel) decomposes the single colloquial claim 'GPL derivative work trigger' into three structurally distinct constraints with different ε values, beneficiary sets, and classifications. The interface_boundary_reading (this story) is a scaffold (ε=0.22) enabling mixed licensing. The broad_copyleft_reading is a tangled_rope/snare (high ε) enforcing full copyleft propagation. The narrow_linking_permissive_reading is a rope (low ε) enabling permissive linking. They are linked via network.affects_constraints because the upstream kernel claim is cited as evidence for each downstream reading.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(gpl_derivative_work_trigger__interface_boundary_reading, institutional, 0.15).
constraint_indexing:directionality_override(gpl_derivative_work_trigger__interface_boundary_reading, powerless, 0.9).
constraint_indexing:directionality_override(gpl_derivative_work_trigger__interface_boundary_reading, organized, 0.7).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
