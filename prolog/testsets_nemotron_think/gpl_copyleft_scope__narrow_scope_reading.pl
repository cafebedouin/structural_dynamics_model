% ============================================================================
% CONSTRAINT STORY: gpl_copyleft_scope__narrow_scope_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_gpl_copyleft_scope__narrow_scope_reading, []).

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
 *   constraint_id: gpl_copyleft_scope__narrow_scope_reading
 *   human_readable: GPL Section 2(b) Narrow Derivative Work Boundary
 *   domain: software_licensing/intellectual_property/open_source_governance
 *
 * SUMMARY:
 *   GPL Section 2(b) states that the license applies to 'the whole work, and
 *   all its parts, regardless of how they are packaged' — but only for works
 *   'based on the Program' (derivative works). The narrow_scope_reading
 *   interprets 'based on' through traditional copyright's derivative work
 *   doctrine: mere aggregation on a storage medium, plugin architectures with
 *   defined APIs, and dynamic linking against stable interfaces do not create
 *   derivative works. This reading has been the de facto industry standard
 *   since the 1990s, enabling the entire commercial Linux ecosystem. The FSF
 *   advocates the strong_copyleft_reading (any linking triggers copyleft) but
 *   has never litigated it to precedent. The enforcement_vacuum_reading
 *   treats the ambiguity as a licensed plurality where local interpretive
 *   communities decide. This story instantiates ONLY the narrow reading as a
 *   clean ε-invariant constraint.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gpl_copyleft_scope__narrow_scope_reading, 0.35).
domain_priors:suppression_score(gpl_copyleft_scope__narrow_scope_reading, 0.15).
domain_priors:theater_ratio(gpl_copyleft_scope__narrow_scope_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gpl_copyleft_scope__narrow_scope_reading, extractiveness, 0.35).
narrative_ontology:constraint_metric(gpl_copyleft_scope__narrow_scope_reading, suppression_requirement, 0.15).
narrative_ontology:constraint_metric(gpl_copyleft_scope__narrow_scope_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gpl_copyleft_scope__narrow_scope_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(gpl_copyleft_scope__narrow_scope_reading, resistance, 0.25).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gpl_copyleft_scope__narrow_scope_reading, rope).
narrative_ontology:human_readable(gpl_copyleft_scope__narrow_scope_reading, "GPL Section 2(b) Narrow Derivative Work Boundary").
narrative_ontology:topic_domain(gpl_copyleft_scope__narrow_scope_reading, "software_licensing/intellectual_property/open_source_governance").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gpl_copyleft_scope__narrow_scope_reading, '420e35f5-234b-4109-9ea0-8640ceb44d12').
narrative_ontology:cs_kernel_codification('420e35f5-234b-4109-9ea0-8640ceb44d12', formalized).
narrative_ontology:cs_authority_grounding('420e35f5-234b-4109-9ea0-8640ceb44d12', lineage).
narrative_ontology:cs_interpretation_layer_present('420e35f5-234b-4109-9ea0-8640ceb44d12').
narrative_ontology:cs_reading_relation('420e35f5-234b-4109-9ea0-8640ceb44d12', gpl_copyleft_scope__strong_copyleft_reading, coexists_with).
narrative_ontology:cs_reading_relation('420e35f5-234b-4109-9ea0-8640ceb44d12', gpl_copyleft_scope__enforcement_vacuum_reading, influences).
narrative_ontology:cs_axiom('420e35f5-234b-4109-9ea0-8640ceb44d12', foundational, aggregation_exempt_from_copyleft).
narrative_ontology:cs_axiom_status(aggregation_exempt_from_copyleft, holdable).
narrative_ontology:cs_axiom_grounding('420e35f5-234b-4109-9ea0-8640ceb44d12', aggregation_exempt_from_copyleft, conventional).
narrative_ontology:cs_axiom('420e35f5-234b-4109-9ea0-8640ceb44d12', foundational, dynamic_linking_not_derivative_per_se).
narrative_ontology:cs_axiom_status(dynamic_linking_not_derivative_per_se, holdable).
narrative_ontology:cs_axiom_grounding('420e35f5-234b-4109-9ea0-8640ceb44d12', dynamic_linking_not_derivative_per_se, conventional).
narrative_ontology:cs_axiom('420e35f5-234b-4109-9ea0-8640ceb44d12', secondary, traditional_copyright_doctrine_governs).
narrative_ontology:cs_axiom_status(traditional_copyright_doctrine_governs, holdable).
narrative_ontology:cs_axiom_grounding('420e35f5-234b-4109-9ea0-8640ceb44d12', traditional_copyright_doctrine_governs, conventional).
narrative_ontology:cs_reference_frame('420e35f5-234b-4109-9ea0-8640ceb44d12', gplv2_textual_baseline).
narrative_ontology:cs_drift_state('420e35f5-234b-4109-9ea0-8640ceb44d12', contemporary_cloud_mobile_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('420e35f5-234b-4109-9ea0-8640ceb44d12', '').
narrative_ontology:cs_kernel_id(gpl_copyleft_scope__narrow_scope_reading, gpl_copyleft_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gpl_copyleft_scope__narrow_scope_reading, commercial_firms).
narrative_ontology:constraint_beneficiary(gpl_copyleft_scope__narrow_scope_reading, proprietary_software_vendors).
narrative_ontology:constraint_beneficiary(gpl_copyleft_scope__narrow_scope_reading, platform_operators).
narrative_ontology:constraint_victim(gpl_copyleft_scope__narrow_scope_reading, copyleft_advocates).
narrative_ontology:constraint_victim(gpl_copyleft_scope__narrow_scope_reading, free_software_communities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(gpl_copyleft_scope__narrow_scope_reading, application_developers).
narrative_ontology:constraint_victim(gpl_copyleft_scope__narrow_scope_reading, application_developers).
narrative_ontology:constraint_vindicates(gpl_copyleft_scope__narrow_scope_reading, traditional_copyright_doctrine).
narrative_ontology:constraint_vindicates(gpl_copyleft_scope__narrow_scope_reading, permissionless_innovation).
narrative_ontology:constraint_vindicates(gpl_copyleft_scope__narrow_scope_reading, aggregation_exemption).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Integrate GPL-licensed components into proprietary products via plugin architectures, dynamic linking, and aggregation without triggering copyleft. Retain full proprietary control over their own code while leveraging GPL libraries. Can choose licensing strategies per component; exit is trivial — they simply structure integration to stay outside the derivative work boundary.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__narrow_scope_reading, commercial_firms, beneficiary,
    powerful, biographical, arbitrage, global).

% Build commercial products atop GPL infrastructure (Linux kernel, GCC, core utilities) without contributing modifications back. Shape industry practice through legal teams and lobbying; their integration patterns become de facto standards. The narrow reading is the commercial baseline — they lose nothing if it disappears because they never accepted the strong reading.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__narrow_scope_reading, proprietary_software_vendors, beneficiary,
    powerful, biographical, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(gpl_copyleft_scope__narrow_scope_reading, proprietary_software_vendors, agenda_setter).

% Operate app stores, cloud platforms, and OS distributions that bundle GPL and proprietary components. The narrow boundary lets them curate mixed-license ecosystems without forcing entire stacks open. They set platform policies that effectively codify the narrow reading for their ecosystems.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__narrow_scope_reading, platform_operators, beneficiary,
    institutional, generational, mobile, global).

% Expect GPL to propagate to all combined works; see narrow reading as gutting the license's purpose. Invest in legal education, compliance tooling, and enforcement actions — but enforcement against dynamic linking is rare and costly. Their exit is constrained: they cannot practically fork the entire GPL ecosystem, and abandoning copyleft contradicts their mission.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__narrow_scope_reading, copyleft_advocates, payer,
    organized, generational, constrained, global).

% Develop and maintain GPL-licensed projects expecting reciprocal sharing. Watch commercial entities extract value via narrow-boundary integration without contributing back. Some projects migrate to AGPL or add exception clauses; many accept the status quo because the coordination value of GPL (common baseline) outweighs the leakage.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__narrow_scope_reading, free_software_communities, payer,
    moderate, biographical, constrained, global).

% Adjudicate derivative work boundary when disputes reach litigation. U.S. courts have not ruled definitively on dynamic linking; European courts lean toward functional analysis. Their silence is the enforcement vacuum — the narrow reading persists because no authoritative judicial precedent forecloses it.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__narrow_scope_reading, courts_legal_system, observer,
    institutional, generational, analytical, national).

% Author and steward the GPL text; publish FAQ and compliance guides asserting the strong reading. Their interpretive authority is moral and historical, not legal — they cannot bind courts. They maintain the license text unchanged (v2 since 1991, v3 since 2007) rather than codify the boundary, preserving the enforcement vacuum as strategic ambiguity.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__narrow_scope_reading, fsf_gnu_project, agenda_setter,
    organized, generational, constrained, global).

% Write applications that link to GPL libraries (readline, ffmpeg, etc.). Benefit from clear permission to keep their application code proprietary under the narrow reading. But face uncertainty: if a court adopts the strong reading, their licensing strategy collapses. Some dual-license or avoid GPL dependencies entirely as insurance.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__narrow_scope_reading, application_developers, beneficiary,
    moderate, biographical, mobile, global).
narrative_ontology:stakeholder_secondary_role(gpl_copyleft_scope__narrow_scope_reading, application_developers, payer).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a stable, legally recognizable boundary between 'mere aggregation' and 'derivative work' that lets independent codebases coexist in a single distributed system without license contamination — the coordination problem of mixed-license software composition.
% TRANSFER_FUNCTION: Transfers licensing flexibility from the copyleft expectation (all combined code must be free) to commercial integrators (proprietary layers may sit atop GPL foundations via aggregation/plugin/dynamic-linking boundaries). The transfer is not monetary but legal: the right to keep derivative boundaries closed.
% ABSENT_VOICES: End users who would benefit from stronger copyleft propagation (e.g., right to repair, inspection, modification of the full software stack they use). They are not represented in the legal/industry interpretive community and have no standing in license interpretation disputes.
% DISAPPEARANCE_RATIONALE: If the narrow reading vanished and courts adopted the strong reading overnight, commercial integration patterns would require immediate re-licensing or architectural separation (process boundaries, RPC, microservices). Entire product categories (embedded Linux devices, proprietary apps on Linux, cloud services bundling GPL) would face compliance crises. The GPL ecosystem's commercial adoption would likely contract sharply.
% FOUNDING_PROBLEM: How to define 'derivative work' for software when traditional copyright doctrine (literary works) does not cleanly map to linking, plugins, and runtime composition — the GPL needed a boundary that courts could recognize without banning all combination of free and proprietary code.
% FOUNDING_PROBLEM_CORROBORATION: Legal scholars (Pamela Samuelson, Lawrence Rosen) document the genuine ambiguity in applying copyright's derivative work concept to software linking. Court records show no definitive ruling on dynamic linking in three decades. The FSF's own FAQ history shows evolving interpretations. No corroboration exists from outside the beneficiary set that the narrow boundary was the *intended* founding solution — only that the ambiguity was recognized and left unresolved.
narrative_ontology:disappearance_verdict(gpl_copyleft_scope__narrow_scope_reading, world_rearranges).
narrative_ontology:founding_problem_status(gpl_copyleft_scope__narrow_scope_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gpl_copyleft_scope__narrow_scope_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(gpl_copyleft_scope__narrow_scope_reading, 'none', 1).
narrative_ontology:epsilon_provenance(gpl_copyleft_scope__narrow_scope_reading, 0.35, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gpl_copyleft_scope__narrow_scope_reading_tests).
:- end_tests(gpl_copyleft_scope__narrow_scope_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.35): commercial firms extract value from GPL infrastructure without reciprocating, but the constraint also enables genuine coordination — a stable baseline that lets thousands of independent projects interoperate. Suppression is low (0.15): no active enforcement prevents strong-reading adoption; projects can choose AGPL, add linking exceptions, or use process boundaries. Theater is minimal (0.10): the narrow reading is not performative — it reflects actual industry practice and legal opinion. Accessibility collapse is moderate (0.30): alternatives exist (strong copyleft, permissive licenses, architectural separation) but require deliberate choice. Resistance is low (0.25): copyleft advocates object but lack enforcement leverage; most developers accept the status quo.
 *
 * PERSPECTIVAL GAP:
 *   From the commercial beneficiary seats, this is a genuine coordination rope — a stable, low-friction standard that solves the mixed-codebase problem. From the copyleft advocate payer seats, the same structure operates as a snare — the license's core promise (reciprocal sharing) is structurally unenforceable against the most commercially significant integration patterns. The engine computes this divergence from the declared roles and exit options; the claimed_type 'rope' reflects the beneficiary-seat reality.
 *
 * DIRECTIONALITY LOGIC:
 *   Commercial firms, proprietary vendors, and platform operators are structural beneficiaries (d ≈ 0.15): they collect the flexibility rent, control integration patterns, and face trivial exit. Copyleft advocates and free software communities are payers (d ≈ 0.85): they bear the cost of weakened reciprocity, invest in compliance with diminishing returns, and face constrained exit (mission lock-in). Courts are analytical observers (d = 0.5): they could resolve the ambiguity but have not. FSF is an agenda setter with constrained exit — they authored the license but cannot bind interpretation. Application developers are dual-positioned: beneficiaries of flexibility today, potential payers if courts shift.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (defining derivative work for software) remains contested — not dead. The narrow reading did not resolve it; it exploited the ambiguity. No mandatrophy resolution: the constraint persists because it serves a real coordination function (enabling commercial adoption of GPL code) while the extraction (commercial free-riding) is tolerated as the price of that adoption. The arrangement is not vestigial — it is actively maintained by industry practice.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Does the narrow_scope_reading instantiate a structurally distinct constraint from the strong_copyleft_reading, or are they observer-relative perspectives on one constraint?',
    'ε-invariance test: if the narrow reading''s extractiveness (commercial flexibility without reciprocity) and the strong reading''s extractiveness (universal code sharing imposed on integrators) are measured against the SAME referent (the standing GPL regime), they differ — confirming two constraints. If they are measured against different referents, the kernel decomposition is validated.',
    'If one constraint: the corpus double-counts. If two: each gets its own ε, stakeholders, and classification. The narrow reading is a rope; the strong reading would be a tangled_rope (coordination + extraction from commercial firms).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Whether kernel readings are distinct constraints per ε-invariance principle.').

omega_variable(
    derivative_work_boundary_ambiguity,
    'Where exactly does the derivative work boundary fall for dynamic linking, plugin architectures, and RPC boundaries — and does the answer change the constraint''s structural classification?',
    'Definitive appellate ruling on GPL dynamic linking, or legislative clarification of software derivative works. Absent that, track industry practice convergence: if major ecosystems (Android, Linux kernel, cloud platforms) standardize on process-boundary separation as the safe harbor, the narrow reading hardens into custom.',
    'If courts adopt strong reading: narrow reading collapses, commercial integrators face prohibitive fixing_cost, classification shifts toward snare for current beneficiaries. If custom hardens narrow reading: extraction becomes entrenched coordination practice, rope classification stabilizes.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(derivative_work_boundary_ambiguity, empirical, 'Location of the derivative work boundary and its classification consequences.').

omega_variable(
    enforcement_vacuum_persistence,
    'Why has the FSF never litigated the dynamic linking question to precedent in three decades — strategic ambiguity, resource constraints, or fear of adverse precedent?',
    'Internal FSF/ GNU Project archives, oral histories from compliance lawyers, analysis of settlement vs. litigation patterns in GPL enforcement.',
    'If strategic: the vacuum is a feature — the strong reading is a negotiating position, not a legal claim. If resource-constrained: the vacuum is a bug — the strong reading is real but undefended. If fear of adverse precedent: the strong reading is legally fragile.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_vacuum_persistence, preference, 'FSF''s non-litigation of the derivative work boundary — strategy or constraint.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gpl_copyleft_scope__narrow_scope_reading, 1991, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gpl_copyleft_scope__narrow_scope_reading_tr_t1991, gpl_copyleft_scope__narrow_scope_reading, theater_ratio, 1991, 0.05).
narrative_ontology:measurement(gpl_copyleft_scope__narrow_scope_reading_tr_t1998, gpl_copyleft_scope__narrow_scope_reading, theater_ratio, 1998, 0.08).
narrative_ontology:measurement(gpl_copyleft_scope__narrow_scope_reading_tr_t2005, gpl_copyleft_scope__narrow_scope_reading, theater_ratio, 2005, 0.1).
narrative_ontology:measurement(gpl_copyleft_scope__narrow_scope_reading_tr_t2010, gpl_copyleft_scope__narrow_scope_reading, theater_ratio, 2010, 0.1).
narrative_ontology:measurement(gpl_copyleft_scope__narrow_scope_reading_tr_t2015, gpl_copyleft_scope__narrow_scope_reading, theater_ratio, 2015, 0.1).
narrative_ontology:measurement(gpl_copyleft_scope__narrow_scope_reading_tr_t2020, gpl_copyleft_scope__narrow_scope_reading, theater_ratio, 2020, 0.1).
narrative_ontology:measurement(gpl_copyleft_scope__narrow_scope_reading_tr_t2024, gpl_copyleft_scope__narrow_scope_reading, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(gpl_copyleft_scope__narrow_scope_reading_be_t1991, gpl_copyleft_scope__narrow_scope_reading, base_extractiveness, 1991, 0.15).
narrative_ontology:measurement(gpl_copyleft_scope__narrow_scope_reading_be_t1998, gpl_copyleft_scope__narrow_scope_reading, base_extractiveness, 1998, 0.2).
narrative_ontology:measurement(gpl_copyleft_scope__narrow_scope_reading_be_t2005, gpl_copyleft_scope__narrow_scope_reading, base_extractiveness, 2005, 0.28).
narrative_ontology:measurement(gpl_copyleft_scope__narrow_scope_reading_be_t2010, gpl_copyleft_scope__narrow_scope_reading, base_extractiveness, 2010, 0.32).
narrative_ontology:measurement(gpl_copyleft_scope__narrow_scope_reading_be_t2015, gpl_copyleft_scope__narrow_scope_reading, base_extractiveness, 2015, 0.34).
narrative_ontology:measurement(gpl_copyleft_scope__narrow_scope_reading_be_t2020, gpl_copyleft_scope__narrow_scope_reading, base_extractiveness, 2020, 0.35).
narrative_ontology:measurement(gpl_copyleft_scope__narrow_scope_reading_be_t2024, gpl_copyleft_scope__narrow_scope_reading, base_extractiveness, 2024, 0.35).

% Suppression requirement over time
narrative_ontology:measurement(gpl_copyleft_scope__narrow_scope_reading_su_t1991, gpl_copyleft_scope__narrow_scope_reading, suppression_requirement, 1991, 0.05).
narrative_ontology:measurement(gpl_copyleft_scope__narrow_scope_reading_su_t1998, gpl_copyleft_scope__narrow_scope_reading, suppression_requirement, 1998, 0.08).
narrative_ontology:measurement(gpl_copyleft_scope__narrow_scope_reading_su_t2005, gpl_copyleft_scope__narrow_scope_reading, suppression_requirement, 2005, 0.12).
narrative_ontology:measurement(gpl_copyleft_scope__narrow_scope_reading_su_t2010, gpl_copyleft_scope__narrow_scope_reading, suppression_requirement, 2010, 0.15).
narrative_ontology:measurement(gpl_copyleft_scope__narrow_scope_reading_su_t2015, gpl_copyleft_scope__narrow_scope_reading, suppression_requirement, 2015, 0.15).
narrative_ontology:measurement(gpl_copyleft_scope__narrow_scope_reading_su_t2020, gpl_copyleft_scope__narrow_scope_reading, suppression_requirement, 2020, 0.15).
narrative_ontology:measurement(gpl_copyleft_scope__narrow_scope_reading_su_t2024, gpl_copyleft_scope__narrow_scope_reading, suppression_requirement, 2024, 0.15).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gpl_copyleft_scope__narrow_scope_reading, information_standard).
narrative_ontology:boltzmann_floor_override(gpl_copyleft_scope__narrow_scope_reading, 0.02).
narrative_ontology:affects_constraint(gpl_copyleft_scope__narrow_scope_reading, gpl_copyleft_scope__strong_copyleft_reading).
narrative_ontology:affects_constraint(gpl_copyleft_scope__narrow_scope_reading, gpl_copyleft_scope__enforcement_vacuum_reading).
narrative_ontology:affects_constraint(gpl_copyleft_scope__narrow_scope_reading, agpl_network_copyleft).
narrative_ontology:affects_constraint(gpl_copyleft_scope__narrow_scope_reading, lgpl_library_exception).
narrative_ontology:affects_constraint(gpl_copyleft_scope__narrow_scope_reading, permissive_license_adoption).

% DUAL FORMULATION NOTE:
% This constraint family (gpl_copyleft_scope) decomposes the single label 'GPL copyleft scope' into three structurally distinct readings with different ε values and stakeholder distributions. The narrow reading (this file) is a moderate-ε rope enabling commercial integration. The strong reading would be a higher-ε tangled_rope (coordination + extraction from commercial firms). The enforcement vacuum reading is a meta-constraint about interpretive plurality.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(gpl_copyleft_scope__narrow_scope_reading, organized, 0.2).
constraint_indexing:directionality_override(gpl_copyleft_scope__narrow_scope_reading, moderate, 0.45).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
