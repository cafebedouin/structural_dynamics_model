% ============================================================================
% CONSTRAINT STORY: gpl_derivative_work_trigger__narrow_linking_permissive_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_gpl_derivative_work_trigger__narrow_linking_permissive_reading, []).

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
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: gpl_derivative_work_trigger__narrow_linking_permissive_reading
 *   human_readable: GPL Derivative Work Trigger — Narrow Linking Permissive Reading
 *   domain: software_licensing/copyright_law/open_source_governance
 *
 * SUMMARY:
 *   The GPL's derivative work trigger is a contested kernel. This reading —
 *   narrow_linking_permissive_reading — asserts that linking (static or
 *   dynamic) to a GPL-covered library constitutes mere aggregation, not
 *   derivation, so only modifications to the GPL code itself propagate
 *   copyleft obligations. The reading creates a structural wall protecting
 *   proprietary modules: vendors may incorporate GPL libraries into
 *   proprietary products without releasing their own source. Users of those
 *   proprietary modules lose the source-availability guarantee the GPL aims
 *   to secure; the FSF's propagation goal is frustrated. The reading claims
 *   to be a clarifying coordination mechanism (defining a clean boundary),
 *   but its operation extracts value from the commons for proprietary benefit
 *   — a tangled rope.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gpl_derivative_work_trigger__narrow_linking_permissive_reading, 0.68).
domain_priors:suppression_score(gpl_derivative_work_trigger__narrow_linking_permissive_reading, 0.22).
domain_priors:theater_ratio(gpl_derivative_work_trigger__narrow_linking_permissive_reading, 0.31).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gpl_derivative_work_trigger__narrow_linking_permissive_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(gpl_derivative_work_trigger__narrow_linking_permissive_reading, suppression_requirement, 0.22).
narrative_ontology:constraint_metric(gpl_derivative_work_trigger__narrow_linking_permissive_reading, theater_ratio, 0.31).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gpl_derivative_work_trigger__narrow_linking_permissive_reading, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(gpl_derivative_work_trigger__narrow_linking_permissive_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gpl_derivative_work_trigger__narrow_linking_permissive_reading, tangled_rope).
narrative_ontology:human_readable(gpl_derivative_work_trigger__narrow_linking_permissive_reading, "GPL Derivative Work Trigger — Narrow Linking Permissive Reading").
narrative_ontology:topic_domain(gpl_derivative_work_trigger__narrow_linking_permissive_reading, "software_licensing/copyright_law/open_source_governance").

domain_priors:requires_active_enforcement(gpl_derivative_work_trigger__narrow_linking_permissive_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gpl_derivative_work_trigger__narrow_linking_permissive_reading, '2da93159-e8d1-4e9e-9e1c-b569b769c7de').
narrative_ontology:cs_kernel_codification('2da93159-e8d1-4e9e-9e1c-b569b769c7de', fixed_text).
narrative_ontology:cs_authority_grounding('2da93159-e8d1-4e9e-9e1c-b569b769c7de', lineage).
narrative_ontology:cs_interpretation_layer_present('2da93159-e8d1-4e9e-9e1c-b569b769c7de').
narrative_ontology:cs_reading_relation('2da93159-e8d1-4e9e-9e1c-b569b769c7de', gpl_derivative_work_trigger__broad_copyleft_reading, forecloses).
narrative_ontology:cs_reading_relation('2da93159-e8d1-4e9e-9e1c-b569b769c7de', gpl_derivative_work_trigger__interface_boundary_reading, coexists_with).
narrative_ontology:cs_axiom('2da93159-e8d1-4e9e-9e1c-b569b769c7de', foundational, linking_is_aggregation_not_derivation).
narrative_ontology:cs_axiom_status(linking_is_aggregation_not_derivation, holdable).
narrative_ontology:cs_axiom_grounding('2da93159-e8d1-4e9e-9e1c-b569b769c7de', linking_is_aggregation_not_derivation, conventional).
narrative_ontology:cs_axiom('2da93159-e8d1-4e9e-9e1c-b569b769c7de', secondary, only_modifications_trigger_gpl_obligations).
narrative_ontology:cs_axiom_status(only_modifications_trigger_gpl_obligations, holdable).
narrative_ontology:cs_axiom_grounding('2da93159-e8d1-4e9e-9e1c-b569b769c7de', only_modifications_trigger_gpl_obligations, conventional).
narrative_ontology:cs_reference_frame('2da93159-e8d1-4e9e-9e1c-b569b769c7de', linking_aggregation_boundary).
narrative_ontology:cs_drift_state('2da93159-e8d1-4e9e-9e1c-b569b769c7de', contemporary_court_decisions, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('2da93159-e8d1-4e9e-9e1c-b569b769c7de', '').
narrative_ontology:cs_kernel_id(gpl_derivative_work_trigger__narrow_linking_permissive_reading, gpl_derivative_work_trigger).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gpl_derivative_work_trigger__narrow_linking_permissive_reading, proprietary_software_vendors).
narrative_ontology:constraint_beneficiary(gpl_derivative_work_trigger__narrow_linking_permissive_reading, corporate_legal_departments).
narrative_ontology:constraint_victim(gpl_derivative_work_trigger__narrow_linking_permissive_reading, end_users_of_proprietary_modules).
narrative_ontology:constraint_victim(gpl_derivative_work_trigger__narrow_linking_permissive_reading, free_software_developers).
narrative_ontology:constraint_victim(gpl_derivative_work_trigger__narrow_linking_permissive_reading, fsf_copyleft_goal).
narrative_ontology:constraint_vindicates(gpl_derivative_work_trigger__narrow_linking_permissive_reading, copyright_law_allows_aggregation_exception).
narrative_ontology:constraint_vindicates(gpl_derivative_work_trigger__narrow_linking_permissive_reading, dynamic_linking_is_not_derivative_work).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Incorporate GPL-covered libraries into proprietary products via dynamic linking without releasing proprietary source. They advocate the narrow reading in court and compliance programs. Exit is arbitrage-grade: they can switch to permissively-licensed alternatives if the reading is rejected.
narrative_ontology:constraint_stakeholder(gpl_derivative_work_trigger__narrow_linking_permissive_reading, proprietary_software_vendors, beneficiary,
    powerful, biographical, arbitrage, global).

% Advise on GPL compliance strategies that rely on the narrow reading. They shape internal policies and external advocacy (e.g., through trade associations). Exit is mobile: they can adopt conservative compliance if legal risk rises.
narrative_ontology:constraint_stakeholder(gpl_derivative_work_trigger__narrow_linking_permissive_reading, corporate_legal_departments, beneficiary,
    institutional, biographical, mobile, global).
narrative_ontology:stakeholder_secondary_role(gpl_derivative_work_trigger__narrow_linking_permissive_reading, corporate_legal_departments, agenda_setter).

% Use proprietary software that incorporates GPL libraries under the narrow reading. They receive no source code, cannot modify or repair the GPL components, and depend on the vendor for updates. Exit is trapped: switching costs are high, alternatives may not exist.
narrative_ontology:constraint_stakeholder(gpl_derivative_work_trigger__narrow_linking_permissive_reading, end_users_of_proprietary_modules, payer,
    powerless, immediate, trapped, global).

% Contribute code to GPL projects expecting copyleft propagation. The narrow reading allows their work to be embedded in proprietary products without reciprocity. Exit is constrained: they can relicense new contributions but cannot retroactively change existing GPL code.
narrative_ontology:constraint_stakeholder(gpl_derivative_work_trigger__narrow_linking_permissive_reading, free_software_developers, payer,
    organized, generational, constrained, global).

% The Free Software Foundation's mission to ensure all users have the four freedoms. The narrow reading directly frustrates this goal by permitting proprietary enclosures. The FSF is not a party to the linking transaction but its doctrinal objective is impaired; it would object if present in the compliance decision.
narrative_ontology:constraint_stakeholder(gpl_derivative_work_trigger__narrow_linking_permissive_reading, fsf_copyleft_goal, excluded,
    institutional, generational, analytical, global).
narrative_ontology:stakeholder_non_agent(gpl_derivative_work_trigger__narrow_linking_permissive_reading, fsf_copyleft_goal).

% Adjudicate GPL enforcement cases involving dynamic linking. Their rulings determine which reading becomes binding precedent in each jurisdiction. They neither collect extraction nor pay it; they observe and decide.
narrative_ontology:constraint_stakeholder(gpl_derivative_work_trigger__narrow_linking_permissive_reading, courts, observer,
    institutional, generational, analytical, national).

% Produce academic analysis of the derivative work boundary in software copyright. Their work influences courts and compliance practice but they have no direct stake in the extraction.
narrative_ontology:constraint_stakeholder(gpl_derivative_work_trigger__narrow_linking_permissive_reading, legal_scholars, observer,
    organized, biographical, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(gpl_derivative_work_trigger__narrow_linking_permissive_reading, proprietary_software_vendors).
narrative_ontology:fixing_cost_class(gpl_derivative_work_trigger__narrow_linking_permissive_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Defines a clear, bright-line boundary between aggregation and derivation in software copyright, reducing litigation risk and enabling composable software development across license boundaries.
% TRANSFER_FUNCTION: Moves the value of GPL-covered code (development effort, network effects, user base) from the free software commons into proprietary products, without reciprocal source disclosure. The transfer is from free software developers and end users to proprietary vendors.
% ABSENT_VOICES: End users of proprietary modules are structurally excluded from the compliance decision — they have no seat at the table when vendors and lawyers decide whether to rely on the narrow reading. The FSF's copyleft goal is also excluded as a non-agent doctrinal commitment.
% DISAPPEARANCE_RATIONALE: If the narrow reading vanished overnight (e.g., a supreme court ruling that linking creates derivative works), proprietary vendors would have to either release source for linked modules, rewrite using alternative libraries, or negotiate LGPL/exception licenses. The mobile/embedded/IoT software supply chain would reorganize rapidly. The commons would regain source-availability guarantees for linked code.
% FOUNDING_PROBLEM: Early free software licenses lacked a clear boundary for 'derivative work' in the context of dynamic linking and library usage. The GPLv2 (1991) and GPLv3 (2007) left the boundary ambiguous, creating legal uncertainty for developers who wanted to combine GPL and proprietary code.
% FOUNDING_PROBLEM_CORROBORATION: Proprietary vendors and some legal scholars attest the boundary uncertainty persists and the narrow reading resolves it. The FSF, most free software developers, and several appellate decisions (e.g., Artifex v. Hancom, VMware v. Hellwig) attest the founding problem is the proprietary enclosure itself, not uncertainty — the narrow reading is a solution that benefits the enclosers. No neutral third party corroborates the narrow reading as a pure coordination fix.
narrative_ontology:disappearance_verdict(gpl_derivative_work_trigger__narrow_linking_permissive_reading, world_rearranges).
narrative_ontology:founding_problem_status(gpl_derivative_work_trigger__narrow_linking_permissive_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gpl_derivative_work_trigger__narrow_linking_permissive_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(gpl_derivative_work_trigger__narrow_linking_permissive_reading, 'none', 1).
narrative_ontology:epsilon_provenance(gpl_derivative_work_trigger__narrow_linking_permissive_reading, 0.68, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gpl_derivative_work_trigger__narrow_linking_permissive_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(gpl_derivative_work_trigger__narrow_linking_permissive_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(gpl_derivative_work_trigger__narrow_linking_permissive_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68) is high because the reading enables proprietary capture of GPL'd code's value without reciprocal contribution. Suppression (0.22) is low — the reading is permissive, it does not coerce; it removes a coercive threat (GPL enforcement against linking). Theater ratio (0.31) is moderate: the 'clean boundary' rhetoric performs coordination while the material effect is extraction. Accessibility collapse (0.42) is moderate: alternative licensing (LGPL, permissive licenses) exists but the GPL's dominance makes the boundary practically significant. Resistance (0.55) is substantial: the FSF, many developers, and some courts reject this reading.
 *
 * PERSPECTIVAL GAP:
 *   From the proprietary vendor seat, the reading looks like a rope (genuine coordination: a clear, stable boundary that enables innovation). From the end-user and free-developer seats, it looks like a snare (extraction disguised as interpretation). The engine computes this divergence from the declared beneficiaries/victims and exit options; the authored claim (tangled_rope) acknowledges both coordination and extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   Proprietary vendors and their legal departments are structural beneficiaries (d near 0.0): they gain incorporation rights without cost. End users of proprietary modules are payers (d near 1.0): they lose source access and modification rights. Free software developers are payers (d ~0.7): their contributed code becomes a free input for proprietary products. The FSF is excluded (not a party to the transaction but its mission is impaired). Courts are observers (analytical seat) — they adjudicate but do not collect or pay.
 *
 * MANDATROPHY ANALYSIS:
 *   The GPL's founding problem (preventing proprietary enclosure of free code) is live. This reading claims the problem is solved for linking scenarios, but the corroboration from outside beneficiaries (FSF, user advocates, many jurisdictions) says the problem persists. The mismatch (founding_problem_status=live, disappearance_verdict=world_rearranges) flags capture risk: the arrangement persists because it benefits powerful vendors, not because the coordination problem is solved.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    legal_uncertainty_derivative_work_boundary,
    'Does the narrow linking permissive reading reflect settled copyright law or an unsettled judicial question?',
    'Appellate court rulings on GPL enforcement cases involving dynamic linking, or legislative clarification of derivative work definition in software context.',
    'If settled law, the reading is a mountain (fixed legal boundary); if unsettled, it is a contested interpretation whose adoption extracts value from the commons — classification shifts from tangled_rope toward snare or rope depending on enforcement dynamics.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legal_uncertainty_derivative_work_boundary, empirical, 'Whether the legal boundary between linking and derivation is fixed or contested.').

omega_variable(
    committer_frame_narrow_linking_permissive,
    'This constraint is one reading of the contested kernel ''gpl_derivative_work_trigger''. What structural elements differ between this reading and its siblings?',
    'Compare the beneficiary/victim sets, extractiveness metrics, and coordination claims across the three declared readings (broad_copyleft_reading, interface_boundary_reading, narrow_linking_permissive_reading).',
    'If sibling readings produce materially different ε values or beneficiary structures, the kernel decomposes into distinct constraints per ε-invariance; if they converge, the kernel may be a single constraint with observational variance.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(committer_frame_narrow_linking_permissive, conceptual, 'Commitment-system framing: this reading instantiates narrow_linking_permissive_reading of kernel gpl_derivative_work_trigger; siblings are broad_copyleft_reading and interface_boundary_reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gpl_derivative_work_trigger__narrow_linking_permissive_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gpl_derivative_work_trigger__narrow_linking_permissive_reading_tr_t0, gpl_derivative_work_trigger__narrow_linking_permissive_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement(gpl_derivative_work_trigger__narrow_linking_permissive_reading_tr_t10, gpl_derivative_work_trigger__narrow_linking_permissive_reading, theater_ratio, 10, 0.24).
narrative_ontology:measurement(gpl_derivative_work_trigger__narrow_linking_permissive_reading_tr_t20, gpl_derivative_work_trigger__narrow_linking_permissive_reading, theater_ratio, 20, 0.28).
narrative_ontology:measurement(gpl_derivative_work_trigger__narrow_linking_permissive_reading_tr_t30, gpl_derivative_work_trigger__narrow_linking_permissive_reading, theater_ratio, 30, 0.31).

% Extraction over time
narrative_ontology:measurement(gpl_derivative_work_trigger__narrow_linking_permissive_reading_be_t0, gpl_derivative_work_trigger__narrow_linking_permissive_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(gpl_derivative_work_trigger__narrow_linking_permissive_reading_be_t10, gpl_derivative_work_trigger__narrow_linking_permissive_reading, base_extractiveness, 10, 0.55).
narrative_ontology:measurement(gpl_derivative_work_trigger__narrow_linking_permissive_reading_be_t20, gpl_derivative_work_trigger__narrow_linking_permissive_reading, base_extractiveness, 20, 0.62).
narrative_ontology:measurement(gpl_derivative_work_trigger__narrow_linking_permissive_reading_be_t30, gpl_derivative_work_trigger__narrow_linking_permissive_reading, base_extractiveness, 30, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(gpl_derivative_work_trigger__narrow_linking_permissive_reading_su_t0, gpl_derivative_work_trigger__narrow_linking_permissive_reading, suppression_requirement, 0, 0.15).
narrative_ontology:measurement(gpl_derivative_work_trigger__narrow_linking_permissive_reading_su_t10, gpl_derivative_work_trigger__narrow_linking_permissive_reading, suppression_requirement, 10, 0.18).
narrative_ontology:measurement(gpl_derivative_work_trigger__narrow_linking_permissive_reading_su_t20, gpl_derivative_work_trigger__narrow_linking_permissive_reading, suppression_requirement, 20, 0.2).
narrative_ontology:measurement(gpl_derivative_work_trigger__narrow_linking_permissive_reading_su_t30, gpl_derivative_work_trigger__narrow_linking_permissive_reading, suppression_requirement, 30, 0.22).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gpl_derivative_work_trigger__narrow_linking_permissive_reading, information_standard).
narrative_ontology:boltzmann_floor_override(gpl_derivative_work_trigger__narrow_linking_permissive_reading, 0.02).
narrative_ontology:affects_constraint(gpl_derivative_work_trigger__narrow_linking_permissive_reading, gpl_derivative_work_trigger__broad_copyleft_reading).
narrative_ontology:affects_constraint(gpl_derivative_work_trigger__narrow_linking_permissive_reading, gpl_derivative_work_trigger__interface_boundary_reading).
narrative_ontology:affects_constraint(gpl_derivative_work_trigger__narrow_linking_permissive_reading, lgpl_library_exception).
narrative_ontology:affects_constraint(gpl_derivative_work_trigger__narrow_linking_permissive_reading, permissive_license_adoption).

% DUAL FORMULATION NOTE:
% This reading decomposes the GPL derivative work trigger kernel with broad_copyleft_reading and interface_boundary_reading. The narrow reading's ε (0.68) is substantially higher than the broad reading's (near 0) because it enables proprietary extraction; the interface_boundary_reading sits between. All three share the same kernel text but instantiate different constraints per ε-invariance.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(gpl_derivative_work_trigger__narrow_linking_permissive_reading, institutional, 0.15).
constraint_indexing:directionality_override(gpl_derivative_work_trigger__narrow_linking_permissive_reading, powerful, 0.12).
constraint_indexing:directionality_override(gpl_derivative_work_trigger__narrow_linking_permissive_reading, powerless, 0.92).
constraint_indexing:directionality_override(gpl_derivative_work_trigger__narrow_linking_permissive_reading, organized, 0.68).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
