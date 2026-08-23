% ============================================================================
% CONSTRAINT STORY: gpl_derivative_work_trigger__broad_copyleft_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_gpl_derivative_work_trigger__broad_copyleft_reading, []).

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
 *   constraint_id: gpl_derivative_work_trigger__broad_copyleft_reading
 *   human_readable: GPL Broad Copyleft: Linking Creates Derivative Work
 *   domain: software_licensing/copyright_law/open_source_governance
 *
 * SUMMARY:
 *   The GPL's broad copyleft reading asserts that any work that links to a
 *   GPL-covered library (statically or dynamically) becomes a derivative work
 *   and must be licensed under GPL-compatible terms. This reading is
 *   championed by the Free Software Foundation and accepted by many in the
 *   copyleft community. It functions as a coordination mechanism that pulls
 *   dependent code into the commons, guaranteeing downstream users source
 *   access. However, it imposes substantial compliance costs on proprietary
 *   vendors and closed-source developers who must either open their code or
 *   engineer around GPL dependencies. The constraint is actively enforced
 *   through copyright law and community policing. The claimed type is
 *   tangled_rope because the arrangement simultaneously coordinates a global
 *   free-software commons and extracts compliance costs from proprietary
 *   actors.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gpl_derivative_work_trigger__broad_copyleft_reading, 0.68).
domain_priors:suppression_score(gpl_derivative_work_trigger__broad_copyleft_reading, 0.72).
domain_priors:theater_ratio(gpl_derivative_work_trigger__broad_copyleft_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gpl_derivative_work_trigger__broad_copyleft_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(gpl_derivative_work_trigger__broad_copyleft_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(gpl_derivative_work_trigger__broad_copyleft_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gpl_derivative_work_trigger__broad_copyleft_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(gpl_derivative_work_trigger__broad_copyleft_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gpl_derivative_work_trigger__broad_copyleft_reading, tangled_rope).
narrative_ontology:human_readable(gpl_derivative_work_trigger__broad_copyleft_reading, "GPL Broad Copyleft: Linking Creates Derivative Work").
narrative_ontology:topic_domain(gpl_derivative_work_trigger__broad_copyleft_reading, "software_licensing/copyright_law/open_source_governance").

domain_priors:requires_active_enforcement(gpl_derivative_work_trigger__broad_copyleft_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gpl_derivative_work_trigger__broad_copyleft_reading, '7ed730ed-8408-48ff-806c-48380043b005').
narrative_ontology:cs_kernel_codification('7ed730ed-8408-48ff-806c-48380043b005', fixed_text).
narrative_ontology:cs_authority_grounding('7ed730ed-8408-48ff-806c-48380043b005', lineage).
narrative_ontology:cs_interpretation_layer_present('7ed730ed-8408-48ff-806c-48380043b005').
narrative_ontology:cs_reading_relation('7ed730ed-8408-48ff-806c-48380043b005', gpl_derivative_work_trigger__narrow_linking_permissive_reading, coexists_with).
narrative_ontology:cs_reading_relation('7ed730ed-8408-48ff-806c-48380043b005', gpl_derivative_work_trigger__interface_boundary_reading, coexists_with).
narrative_ontology:cs_axiom('7ed730ed-8408-48ff-806c-48380043b005', foundational, linking_creates_derivative_work).
narrative_ontology:cs_axiom_status(linking_creates_derivative_work, holdable).
narrative_ontology:cs_axiom_grounding('7ed730ed-8408-48ff-806c-48380043b005', linking_creates_derivative_work, conventional).
narrative_ontology:cs_axiom('7ed730ed-8408-48ff-806c-48380043b005', secondary, source_disclosure_obligation_extends_to_linked_works).
narrative_ontology:cs_axiom_status(source_disclosure_obligation_extends_to_linked_works, holdable).
narrative_ontology:cs_axiom_grounding('7ed730ed-8408-48ff-806c-48380043b005', source_disclosure_obligation_extends_to_linked_works, conventional).
narrative_ontology:cs_reference_frame('7ed730ed-8408-48ff-806c-48380043b005', gpl_copyleft_intent_framework).
narrative_ontology:cs_drift_state('7ed730ed-8408-48ff-806c-48380043b005', contemporary_legal_uncertainty, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('7ed730ed-8408-48ff-806c-48380043b005', '2026-08-03T12:00:00Z').
narrative_ontology:cs_kernel_id(gpl_derivative_work_trigger__broad_copyleft_reading, gpl_derivative_work_trigger).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gpl_derivative_work_trigger__broad_copyleft_reading, free_software_users).
narrative_ontology:constraint_beneficiary(gpl_derivative_work_trigger__broad_copyleft_reading, downstream_developers).
narrative_ontology:constraint_beneficiary(gpl_derivative_work_trigger__broad_copyleft_reading, gpl_community).
narrative_ontology:constraint_victim(gpl_derivative_work_trigger__broad_copyleft_reading, proprietary_vendors).
narrative_ontology:constraint_victim(gpl_derivative_work_trigger__broad_copyleft_reading, closed_source_developers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(gpl_derivative_work_trigger__broad_copyleft_reading, downstream_developers).
narrative_ontology:constraint_vindicates(gpl_derivative_work_trigger__broad_copyleft_reading, copyleft_as_commons_preservation).
narrative_ontology:constraint_vindicates(gpl_derivative_work_trigger__broad_copyleft_reading, source_disclosure_as_user_freedom).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Stewards the GPL license text, publishes FAQs and legal guidance asserting that linking (static or dynamic) creates a derivative work. Enforces compliance through copyright litigation and community pressure. Collects no direct revenue but derives institutional legitimacy from license adoption.
narrative_ontology:constraint_stakeholder(gpl_derivative_work_trigger__broad_copyleft_reading, free_software_foundation, agenda_setter,
    institutional, generational, arbitrage, global).

% Gain guaranteed access to source code of GPL-covered works and any works that link to them. Can modify, redistribute, and study the software. Exit is easy: they can choose GPL software or alternatives.
narrative_ontology:constraint_stakeholder(gpl_derivative_work_trigger__broad_copyleft_reading, free_software_users, beneficiary,
    organized, biographical, mobile, global).

% Benefit from a large commons of reusable code. However, if they link to GPL libraries, their own code must be GPL-compatible, which may conflict with other licensing choices or business models. They can avoid the constraint by using non-GPL alternatives, but ecosystem effects make exit costly.
narrative_ontology:constraint_stakeholder(gpl_derivative_work_trigger__broad_copyleft_reading, downstream_developers, beneficiary,
    moderate, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(gpl_derivative_work_trigger__broad_copyleft_reading, downstream_developers, payer).

% Want to incorporate GPL-covered libraries into proprietary products without disclosing their own source code. Face compliance cost (rewriting, finding alternatives, or opening source) or legal risk. Their exit is constrained by the ubiquity of GPL infrastructure (e.g., Linux kernel, GCC, core libraries).
narrative_ontology:constraint_stakeholder(gpl_derivative_work_trigger__broad_copyleft_reading, proprietary_vendors, payer,
    powerful, biographical, constrained, global).

% Develop proprietary applications that may dynamically link to GPL libraries. Must either comply (open their code) or engineer around (use LGPL/BSD alternatives, isolate via IPC). The constraint raises development costs and limits library choices.
narrative_ontology:constraint_stakeholder(gpl_derivative_work_trigger__broad_copyleft_reading, closed_source_developers, payer,
    moderate, biographical, constrained, global).

% Adjudicate copyright infringement cases involving GPL linking. Their rulings define the legal boundary of derivative work. They do not directly benefit or pay but their decisions determine the constraint's enforcement scope.
narrative_ontology:constraint_stakeholder(gpl_derivative_work_trigger__broad_copyleft_reading, courts_and_legislatures, observer,
    institutional, generational, analytical, national).

% Advocate for licenses like MIT/BSD that permit proprietary linking. They argue the broad reading harms innovation by creating license incompatibility. They are excluded from the GPL drafting process but influence ecosystem choices.
narrative_ontology:constraint_stakeholder(gpl_derivative_work_trigger__broad_copyleft_reading, permissive_license_advocates, excluded,
    organized, biographical, mobile, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Ensures that modifications and extensions of GPL-covered software remain free by treating linking as derivation, thereby growing the commons and preventing proprietary enclosure of community code.
% TRANSFER_FUNCTION: Moves source disclosure obligations from proprietary vendors to the commons: vendors must either release their linked code under GPL-compatible terms or bear the cost of avoiding GPL dependencies.
% ABSENT_VOICES: Per missive license advocates and commercial developers who would prefer a narrower derivative-work definition are structurally excluded from the GPL drafting process; they express dissent through license choice and lobbying.
% DISAPPEARANCE_RATIONALE: If the broad linking interpretation vanished, proprietary vendors could freely link to GPL libraries without source disclosure, shrinking the commons and shifting the ecosystem toward permissive-licensed alternatives. The GPL's copyleft mechanism would lose its primary expansion vector.
% FOUNDING_PROBLEM: Proprietary software was enclosing community-contributed code by making minor modifications or linking without contributing back, breaking the reciprocal sharing ethic of free software.
% FOUNDING_PROBLEM_CORROBORATION: The FSF and many community historians attest the problem persists: proprietary forks and linking without contribution remain common. Corporate open-source offices and some legal scholars argue the problem is overstated and that the broad reading now deters more contribution than it captures.
narrative_ontology:disappearance_verdict(gpl_derivative_work_trigger__broad_copyleft_reading, world_rearranges).
narrative_ontology:founding_problem_status(gpl_derivative_work_trigger__broad_copyleft_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gpl_derivative_work_trigger__broad_copyleft_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(gpl_derivative_work_trigger__broad_copyleft_reading, 'none', 1).
narrative_ontology:epsilon_provenance(gpl_derivative_work_trigger__broad_copyleft_reading, 0.68, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gpl_derivative_work_trigger__broad_copyleft_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(gpl_derivative_work_trigger__broad_copyleft_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(gpl_derivative_work_trigger__broad_copyleft_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68) reflects the significant compliance burden on proprietary actors who cannot easily avoid GPL infrastructure. Suppression (0.72) is high because the constraint's persistence depends on legal enforcement and the difficulty of replacing GPL-covered components. Theater ratio (0.28) is moderate: the FSF's legal guidance and compliance efforts are largely functional, but some enforcement actions serve signaling more than commons growth. Accessibility collapse (0.65) is substantial because once a project links to GPL code, the alternative of proprietary licensing collapses. Resistance (0.55) is moderate: proprietary vendors resist through lobbying, license migration, and technical workarounds, but the constraint remains dominant in key infrastructure.
 *
 * PERSPECTIVAL GAP:
 *   From the FSF and free-software-user seats, the constraint is a rope: it solves the collective-action problem of commons preservation with minimal coercion. From proprietary-vendor seats, it is a snare: the coordination story is cover for forcing source disclosure. The engine will compute this divergence from the structural data (beneficiaries vs. victims, exit options).
 *
 * DIRECTIONALITY LOGIC:
 *   The FSF (agenda_setter) sits near the beneficiary end (d ~ 0.15): it sets the rules and gains institutional legitimacy. Free software users and downstream developers are beneficiaries (d ~ 0.2-0.3) but downstream developers also pay compliance costs (secondary_role payer). Proprietary vendors and closed-source developers are payers (d ~ 0.8-0.9) with constrained exit. Courts are analytical observers (d = 0.5). Permissive-license advocates are excluded (d not computed).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (proprietary enclosure of community code) remains live, but the broad linking interpretation has expanded beyond its original scope (dynamic linking of system libraries). The constraint now extracts from actors who never intended to enclose the commons (e.g., application developers linking to GLIBC). This drift suggests mandatrophy: the mandate has outgrown its function. The engine's mandatrophy_resolved flag should be false.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    linking_derivative_work_ambiguity,
    'Does dynamic linking legally constitute creation of a derivative work under copyright law in major jurisdictions?',
    'Definitive appellate court rulings on GPL enforcement cases involving dynamic linking (e.g., a Supreme Court decision or harmonized EU directive).',
    'If courts reject the broad reading, the constraint''s extraction drops sharply (proprietary vendors can link freely) and the coordination function shrinks (commons growth slows). If courts affirm, extraction and suppression remain high.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(linking_derivative_work_ambiguity, empirical, 'Legal status of the core derivative-work claim').

omega_variable(
    commons_growth_vs_deterrence,
    'Does the broad linking interpretation grow the commons more than it deters new contributions (by making GPL libraries toxic to proprietary-dependent projects)?',
    'Longitudinal study of GPL-licensed library adoption rates vs. permissive-licensed alternatives in ecosystems with mixed proprietary/open-source development.',
    'If deterrence dominates, the constraint''s net coordination benefit is negative and it may reclassify toward snare. If commons growth dominates, tangled_rope holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(commons_growth_vs_deterrence, empirical, 'Net coordination benefit of the broad reading').

omega_variable(
    kernel_reading_contestation,
    'Which reading of the GPL derivative-work trigger will become the dominant legal interpretation?',
    'Convergence of court rulings, industry practice, and license steward guidance over the next decade.',
    'If narrow_linking_permissive_reading prevails, this constraint''s extraction collapses and it becomes a rope (or mountain if unenforced). If interface_boundary_reading prevails, extraction shifts to boundary-definition disputes.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'Outcome of the kernel contest among three readings').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gpl_derivative_work_trigger__broad_copyleft_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gpl_derivative_work_trigger__broad_copyleft_reading_tr_t0, gpl_derivative_work_trigger__broad_copyleft_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(gpl_derivative_work_trigger__broad_copyleft_reading_tr_t6, gpl_derivative_work_trigger__broad_copyleft_reading, theater_ratio, 6, 0.18).
narrative_ontology:measurement(gpl_derivative_work_trigger__broad_copyleft_reading_tr_t12, gpl_derivative_work_trigger__broad_copyleft_reading, theater_ratio, 12, 0.22).
narrative_ontology:measurement(gpl_derivative_work_trigger__broad_copyleft_reading_tr_t18, gpl_derivative_work_trigger__broad_copyleft_reading, theater_ratio, 18, 0.25).
narrative_ontology:measurement(gpl_derivative_work_trigger__broad_copyleft_reading_tr_t24, gpl_derivative_work_trigger__broad_copyleft_reading, theater_ratio, 24, 0.27).
narrative_ontology:measurement(gpl_derivative_work_trigger__broad_copyleft_reading_tr_t30, gpl_derivative_work_trigger__broad_copyleft_reading, theater_ratio, 30, 0.28).

% Extraction over time
narrative_ontology:measurement(gpl_derivative_work_trigger__broad_copyleft_reading_be_t0, gpl_derivative_work_trigger__broad_copyleft_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(gpl_derivative_work_trigger__broad_copyleft_reading_be_t6, gpl_derivative_work_trigger__broad_copyleft_reading, base_extractiveness, 6, 0.52).
narrative_ontology:measurement(gpl_derivative_work_trigger__broad_copyleft_reading_be_t12, gpl_derivative_work_trigger__broad_copyleft_reading, base_extractiveness, 12, 0.58).
narrative_ontology:measurement(gpl_derivative_work_trigger__broad_copyleft_reading_be_t18, gpl_derivative_work_trigger__broad_copyleft_reading, base_extractiveness, 18, 0.62).
narrative_ontology:measurement(gpl_derivative_work_trigger__broad_copyleft_reading_be_t24, gpl_derivative_work_trigger__broad_copyleft_reading, base_extractiveness, 24, 0.65).
narrative_ontology:measurement(gpl_derivative_work_trigger__broad_copyleft_reading_be_t30, gpl_derivative_work_trigger__broad_copyleft_reading, base_extractiveness, 30, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(gpl_derivative_work_trigger__broad_copyleft_reading_su_t0, gpl_derivative_work_trigger__broad_copyleft_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(gpl_derivative_work_trigger__broad_copyleft_reading_su_t6, gpl_derivative_work_trigger__broad_copyleft_reading, suppression_requirement, 6, 0.6).
narrative_ontology:measurement(gpl_derivative_work_trigger__broad_copyleft_reading_su_t12, gpl_derivative_work_trigger__broad_copyleft_reading, suppression_requirement, 12, 0.65).
narrative_ontology:measurement(gpl_derivative_work_trigger__broad_copyleft_reading_su_t18, gpl_derivative_work_trigger__broad_copyleft_reading, suppression_requirement, 18, 0.68).
narrative_ontology:measurement(gpl_derivative_work_trigger__broad_copyleft_reading_su_t24, gpl_derivative_work_trigger__broad_copyleft_reading, suppression_requirement, 24, 0.7).
narrative_ontology:measurement(gpl_derivative_work_trigger__broad_copyleft_reading_su_t30, gpl_derivative_work_trigger__broad_copyleft_reading, suppression_requirement, 30, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gpl_derivative_work_trigger__broad_copyleft_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(gpl_derivative_work_trigger__broad_copyleft_reading, 0.12).
narrative_ontology:affects_constraint(gpl_derivative_work_trigger__broad_copyleft_reading, gpl_derivative_work_trigger__narrow_linking_permissive_reading).
narrative_ontology:affects_constraint(gpl_derivative_work_trigger__broad_copyleft_reading, gpl_derivative_work_trigger__interface_boundary_reading).
narrative_ontology:affects_constraint(gpl_derivative_work_trigger__broad_copyleft_reading, lgpl_library_exception).
narrative_ontology:affects_constraint(gpl_derivative_work_trigger__broad_copyleft_reading, agpl_network_copyleft).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the gpl_derivative_work_trigger kernel. The broad reading maximizes commons pull and extraction; the narrow reading minimizes both; the interface_boundary reading draws a technical line. They form a constraint family linked by affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(gpl_derivative_work_trigger__broad_copyleft_reading, institutional, 0.15).
constraint_indexing:directionality_override(gpl_derivative_work_trigger__broad_copyleft_reading, powerful, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
