% ============================================================================
% CONSTRAINT STORY: gpl_derivative_work_trigger__broad_copyleft_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
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
 *   human_readable: GPL Derivative Work Trigger — Broad Copyleft Reading (Linking = Derivation)
 *   domain: legal/technological
 *
 * SUMMARY:
 *   The GPL's derivative-work trigger is the central mechanism by which
 *   copyleft propagates. The broad copyleft reading — championed by the FSF
 *   and widely adopted in community practice — holds that linking a
 *   proprietary program to a GPL library (even dynamically, at runtime)
 *   creates a combined work that is a derivative of the GPL code, triggering
 *   full source disclosure. This reading pulls dependent code into the
 *   commons, benefiting downstream users but imposing substantial compliance
 *   costs on proprietary vendors. The constraint is a tangled rope: it
 *   performs genuine coordination (protecting the commons from proprietary
 *   enclosure) while extracting asymmetric costs from commercial actors who
 *   wish to build on GPL infrastructure without reciprocating.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gpl_derivative_work_trigger__broad_copyleft_reading, 0.42).
domain_priors:suppression_score(gpl_derivative_work_trigger__broad_copyleft_reading, 0.68).
domain_priors:theater_ratio(gpl_derivative_work_trigger__broad_copyleft_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gpl_derivative_work_trigger__broad_copyleft_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(gpl_derivative_work_trigger__broad_copyleft_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(gpl_derivative_work_trigger__broad_copyleft_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gpl_derivative_work_trigger__broad_copyleft_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(gpl_derivative_work_trigger__broad_copyleft_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gpl_derivative_work_trigger__broad_copyleft_reading, tangled_rope).
narrative_ontology:human_readable(gpl_derivative_work_trigger__broad_copyleft_reading, "GPL Derivative Work Trigger — Broad Copyleft Reading (Linking = Derivation)").
narrative_ontology:topic_domain(gpl_derivative_work_trigger__broad_copyleft_reading, "legal/technological").

domain_priors:requires_active_enforcement(gpl_derivative_work_trigger__broad_copyleft_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gpl_derivative_work_trigger__broad_copyleft_reading, '1cc02aed-a9c0-4c95-a3b2-20abae20263d').
narrative_ontology:cs_kernel_codification('1cc02aed-a9c0-4c95-a3b2-20abae20263d', fixed_text).
narrative_ontology:cs_authority_grounding('1cc02aed-a9c0-4c95-a3b2-20abae20263d', lineage).
narrative_ontology:cs_interpretation_layer_present('1cc02aed-a9c0-4c95-a3b2-20abae20263d').
narrative_ontology:cs_reading_relation('1cc02aed-a9c0-4c95-a3b2-20abae20263d', gpl_derivative_work_trigger__narrow_linking_permissive_reading, forecloses).
narrative_ontology:cs_reading_relation('1cc02aed-a9c0-4c95-a3b2-20abae20263d', gpl_derivative_work_trigger__interface_boundary_reading, coexists_with).
narrative_ontology:cs_axiom('1cc02aed-a9c0-4c95-a3b2-20abae20263d', foundational, linking_creates_derivative_work).
narrative_ontology:cs_axiom_status(linking_creates_derivative_work, holdable).
narrative_ontology:cs_axiom_grounding('1cc02aed-a9c0-4c95-a3b2-20abae20263d', linking_creates_derivative_work, conventional).
narrative_ontology:cs_axiom('1cc02aed-a9c0-4c95-a3b2-20abae20263d', foundational, commons_protection_requires_viral_boundary).
narrative_ontology:cs_axiom_status(commons_protection_requires_viral_boundary, holdable).
narrative_ontology:cs_axiom_grounding('1cc02aed-a9c0-4c95-a3b2-20abae20263d', commons_protection_requires_viral_boundary, instrumental).
narrative_ontology:cs_reference_frame('1cc02aed-a9c0-4c95-a3b2-20abae20263d', fsf_copyleft_orthodoxy).
narrative_ontology:cs_drift_state('1cc02aed-a9c0-4c95-a3b2-20abae20263d', contemporary_cloud_native_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('1cc02aed-a9c0-4c95-a3b2-20abae20263d', '').
narrative_ontology:cs_kernel_id(gpl_derivative_work_trigger__broad_copyleft_reading, gpl_derivative_work_trigger).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gpl_derivative_work_trigger__broad_copyleft_reading, free_software_community).
narrative_ontology:constraint_beneficiary(gpl_derivative_work_trigger__broad_copyleft_reading, downstream_users).
narrative_ontology:constraint_beneficiary(gpl_derivative_work_trigger__broad_copyleft_reading, copyleft_advocates).
narrative_ontology:constraint_victim(gpl_derivative_work_trigger__broad_copyleft_reading, proprietary_vendors).
narrative_ontology:constraint_victim(gpl_derivative_work_trigger__broad_copyleft_reading, commercial_integrators).
narrative_ontology:constraint_vindicates(gpl_derivative_work_trigger__broad_copyleft_reading, strong_copyleft_doctrine).
narrative_ontology:constraint_vindicates(gpl_derivative_work_trigger__broad_copyleft_reading, community_over_proprietary_norm).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintain the GPL license text, publish FAQs and compliance guides interpreting linking as derivative work creation, and pursue enforcement actions against non-compliant distributors. They define the authoritative reading that shapes community expectations and legal risk.
narrative_ontology:constraint_stakeholder(gpl_derivative_work_trigger__broad_copyleft_reading, fsf_and_copyleft_advocates, agenda_setter,
    institutional, generational, analytical, global).

% Developers who release code under GPL gain assurance that proprietary forks cannot silently incorporate their work through dynamic linking. The constraint protects the commons they contribute to, but binds their own distribution choices.
narrative_ontology:constraint_stakeholder(gpl_derivative_work_trigger__broad_copyleft_reading, free_software_community, beneficiary,
    organized, biographical, identity_locked, global).

% End users and redistributors receive source code access for combined works that would otherwise remain proprietary. They benefit from the constraint's extraction without bearing its compliance costs.
narrative_ontology:constraint_stakeholder(gpl_derivative_work_trigger__broad_copyleft_reading, downstream_users, beneficiary,
    moderate, biographical, mobile, global).

% Companies wishing to link proprietary applications to GPL libraries face a binary choice: open their entire codebase or rewrite/replace the GPL dependency. The compliance cost is high and exit is constrained by the ubiquity of GPL infrastructure (Linux, GCC, core libraries).
narrative_ontology:constraint_stakeholder(gpl_derivative_work_trigger__broad_copyleft_reading, proprietary_vendors, payer,
    powerful, biographical, constrained, global).

% System integrators and SaaS providers who combine GPL components with proprietary modules must either disclose source for the whole or restructure architectures to avoid linking. Their margins absorb the compliance burden; switching costs are significant.
narrative_ontology:constraint_stakeholder(gpl_derivative_work_trigger__broad_copyleft_reading, commercial_integrators, payer,
    moderate, biographical, constrained, global).

% Projects under MIT, BSD, Apache licenses are structurally excluded from the GPL's viral reach — they can be linked freely. Their maintainers would argue for a narrower derivative-work boundary but have no standing in GPL enforcement; they route around by avoiding GPL dependencies.
narrative_ontology:constraint_stakeholder(gpl_derivative_work_trigger__broad_copyleft_reading, permissive_license_ecosystems, excluded,
    organized, generational, arbitrage, global).

% Judicial bodies in multiple jurisdictions have not definitively ruled on whether dynamic linking creates a derivative work under copyright law. Their eventual rulings will validate or invalidate this reading's core premise.
narrative_ontology:constraint_stakeholder(gpl_derivative_work_trigger__broad_copyleft_reading, courts_and_legislatures, observer,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Ensures that improvements to free software remain free by extending copyleft obligations to works that combine with GPL code through linking — prevents proprietary enclosure of the commons via technical composition.
% TRANSFER_FUNCTION: Moves source code disclosure obligations and distribution rights from proprietary vendors/commercial integrators to the free software community and downstream users. The constraint transfers control over combined-work distribution from the proprietary party to the commons.
% ABSENT_VOICES: Proprietary vendors and commercial integrators who would argue for a narrow derivative-work definition are not party to the license's creation or community interpretation process. Courts — the ultimate arbiters — have been largely absent from the debate, leaving the FSF's reading as the de facto standard.
% DISAPPEARANCE_RATIONALE: If the broad linking reading vanished overnight, proprietary vendors would immediately incorporate GPL libraries into closed products without source disclosure. The free software commons would lose its primary defense against proprietary enclosure via technical composition. The software ecosystem would reorganize around permissive licenses or contract-based copyleft.
% FOUNDING_PROBLEM: Early free software projects (GCC, Emacs, Linux) faced the risk that companies would take their code, make proprietary improvements, and distribute only binaries — capturing the value of community labor without contributing back. The GPL's derivative-work trigger was designed to close this loophole by making any work 'based on the Program' subject to copyleft.
% FOUNDING_PROBLEM_CORROBORATION: The FSF and GPL drafters attest the problem remains live — proprietary enclosure via linking is an active threat. Proprietary vendors and some legal scholars attest the problem is substantially solved by modern permissive-license infrastructure (LLVM/Clang, BSD kernels, Rust ecosystem) and that the broad reading now primarily extracts compliance costs rather than protecting the commons. Independent legal analysis (e.g., Nimmer on Copyright, academic articles) supports both readings; no consensus exists.
narrative_ontology:disappearance_verdict(gpl_derivative_work_trigger__broad_copyleft_reading, world_rearranges).
narrative_ontology:founding_problem_status(gpl_derivative_work_trigger__broad_copyleft_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gpl_derivative_work_trigger__broad_copyleft_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(gpl_derivative_work_trigger__broad_copyleft_reading, 'none', 1).
narrative_ontology:epsilon_provenance(gpl_derivative_work_trigger__broad_copyleft_reading, 0.42, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gpl_derivative_work_trigger__broad_copyleft_reading_tests).
:- end_tests(gpl_derivative_work_trigger__broad_copyleft_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.42) reflects the real but bounded transfer: proprietary vendors pay compliance costs or rewrite costs, but the constraint does not capture their core revenue. Suppression (0.68) is high because the constraint's persistence depends on active enforcement (cease-and-desist, litigation, compliance pressure) and the practical difficulty of avoiding GPL infrastructure. Theater ratio (0.22) is modest — the coordination function is real but a growing share of enforcement energy defends the linking boundary rather than direct code copying. Accessibility collapse (0.35) is moderate: alternatives exist (permissive licenses, clean-room reimplementation, architectural separation) but are costly. Resistance (0.55) is significant: proprietary vendors actively lobby, litigate, and engineer around the constraint.
 *
 * PERSPECTIVAL GAP:
 *   From the FSF/community seat, the constraint is a rope (genuine coordination protecting the commons). From the proprietary vendor seat, it is a snare (extraction via ambiguous legal threat). From the commercial integrator seat, it is a tangled rope (coordination benefit exists but costs are asymmetric). The engine computes this divergence from the structural data — the claimed type (tangled_rope) represents the authoring seat's structural judgment.
 *
 * DIRECTIONALITY LOGIC:
 *   FSF/copyleft advocates are agenda-setters with institutional power and analytical exit — they define the reading. Free software community and downstream users are beneficiaries: the former identity-locked (their professional identity is fused with copyleft), the latter mobile (they can switch ecosystems). Proprietary vendors and commercial integrators are payers: powerful but constrained exit (GPL infrastructure is pervasive), moderate power but similarly constrained. Permissive-license ecosystems are excluded — they benefit from the constraint's absence but have no voice in its interpretation. Courts are analytical observers whose eventual rulings will resolve the structural ambiguity.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (proprietary enclosure of community code) remains contested. The broad reading persists partly because the FSF's institutional authority depends on maintaining a strong copyleft boundary — weakening it would erode the GPL's distinguishing feature. Meanwhile, the permissive-license ecosystem has grown substantially, offering a viable alternative coordination mechanism that does not impose viral obligations. The constraint shows mandatrophy signals: the original enclosure threat has diminished, but the enforcement machinery has intensified (rising suppression_requirement) and the boundary has expanded (dynamic linking, network services via AGPL). This is not pure mandatrophy — the coordination function remains live — but the extraction-to-coordination ratio has drifted upward.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    linking_as_derivative_work_legal_status,
    'Does dynamic linking legally create a derivative work under copyright law in major jurisdictions?',
    'Definitive appellate or supreme court ruling on GPL enforcement case where dynamic linking is the sole basis for derivative-work claim.',
    'If courts reject the broad reading, the constraint''s suppression mechanism collapses — compliance becomes voluntary. If courts affirm, extraction and suppression both increase as enforcement risk becomes concrete.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(linking_as_derivative_work_legal_status, empirical, 'Core legal ambiguity: whether the FSF''s interpretation matches statutory copyright law.').

omega_variable(
    coordination_extraction_separability,
    'Can the commons-protection coordination function be achieved without the viral linking boundary (e.g., via file-level copyleft like MPL, or contract-based copyleft)?',
    'Comparative analysis of commons vitality in ecosystems using file-level copyleft (Mozilla Public License) vs. viral copyleft (GPL) over 20+ years.',
    'If file-level copyleft achieves equivalent commons protection with lower extraction, the GPL''s linking boundary is excess extraction. If viral copyleft uniquely sustains the commons, the extraction is the price of coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_extraction_separability, empirical, 'Whether the constraint''s coordination and extraction components are structurally separable.').

omega_variable(
    committer_structure_kernel_reading,
    'How does this reading''s structural relationship to its sibling readings affect its classification stability?',
    'Track judicial and legislative developments: if a major jurisdiction adopts the narrow reading, the broad reading''s suppression drops and its classification may shift toward piton (inertial maintenance of a legally weakened position).',
    'The kernel''s contested status means this constraint''s metrics are reading-indexed — a different reading of the same kernel would author different ε, different beneficiaries/victims, different claimed_type. The engine treats each reading as a separate constraint; this omega documents that structural fact.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(committer_structure_kernel_reading, conceptual, 'This constraint is one reading of a contested kernel; its metrics and classification are valid only for this reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gpl_derivative_work_trigger__broad_copyleft_reading, 1991, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gpl_dwt_broad_tr_t1991, gpl_derivative_work_trigger__broad_copyleft_reading, theater_ratio, 1991, 0.08).
narrative_ontology:measurement(gpl_dwt_broad_tr_t1998, gpl_derivative_work_trigger__broad_copyleft_reading, theater_ratio, 1998, 0.1).
narrative_ontology:measurement(gpl_dwt_broad_tr_t2007, gpl_derivative_work_trigger__broad_copyleft_reading, theater_ratio, 2007, 0.15).
narrative_ontology:measurement(gpl_dwt_broad_tr_t2015, gpl_derivative_work_trigger__broad_copyleft_reading, theater_ratio, 2015, 0.18).
narrative_ontology:measurement(gpl_dwt_broad_tr_t2020, gpl_derivative_work_trigger__broad_copyleft_reading, theater_ratio, 2020, 0.2).
narrative_ontology:measurement(gpl_dwt_broad_tr_t2025, gpl_derivative_work_trigger__broad_copyleft_reading, theater_ratio, 2025, 0.22).

% Extraction over time
narrative_ontology:measurement(gpl_dwt_broad_be_t1991, gpl_derivative_work_trigger__broad_copyleft_reading, base_extractiveness, 1991, 0.18).
narrative_ontology:measurement(gpl_dwt_broad_be_t1998, gpl_derivative_work_trigger__broad_copyleft_reading, base_extractiveness, 1998, 0.22).
narrative_ontology:measurement(gpl_dwt_broad_be_t2007, gpl_derivative_work_trigger__broad_copyleft_reading, base_extractiveness, 2007, 0.31).
narrative_ontology:measurement(gpl_dwt_broad_be_t2015, gpl_derivative_work_trigger__broad_copyleft_reading, base_extractiveness, 2015, 0.38).
narrative_ontology:measurement(gpl_dwt_broad_be_t2020, gpl_derivative_work_trigger__broad_copyleft_reading, base_extractiveness, 2020, 0.4).
narrative_ontology:measurement(gpl_dwt_broad_be_t2025, gpl_derivative_work_trigger__broad_copyleft_reading, base_extractiveness, 2025, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(gpl_dwt_broad_su_t1991, gpl_derivative_work_trigger__broad_copyleft_reading, suppression_requirement, 1991, 0.45).
narrative_ontology:measurement(gpl_dwt_broad_su_t1998, gpl_derivative_work_trigger__broad_copyleft_reading, suppression_requirement, 1998, 0.52).
narrative_ontology:measurement(gpl_dwt_broad_su_t2007, gpl_derivative_work_trigger__broad_copyleft_reading, suppression_requirement, 2007, 0.58).
narrative_ontology:measurement(gpl_dwt_broad_su_t2015, gpl_derivative_work_trigger__broad_copyleft_reading, suppression_requirement, 2015, 0.62).
narrative_ontology:measurement(gpl_dwt_broad_su_t2020, gpl_derivative_work_trigger__broad_copyleft_reading, suppression_requirement, 2020, 0.65).
narrative_ontology:measurement(gpl_dwt_broad_su_t2025, gpl_derivative_work_trigger__broad_copyleft_reading, suppression_requirement, 2025, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gpl_derivative_work_trigger__broad_copyleft_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(gpl_derivative_work_trigger__broad_copyleft_reading, 0.1).
narrative_ontology:affects_constraint(gpl_derivative_work_trigger__broad_copyleft_reading, gpl_derivative_work_trigger__narrow_linking_permissive_reading).
narrative_ontology:affects_constraint(gpl_derivative_work_trigger__broad_copyleft_reading, gpl_derivative_work_trigger__interface_boundary_reading).
narrative_ontology:affects_constraint(gpl_derivative_work_trigger__broad_copyleft_reading, agpl_network_service_trigger).
narrative_ontology:affects_constraint(gpl_derivative_work_trigger__broad_copyleft_reading, lgpl_library_exception_boundary).

% DUAL FORMULATION NOTE:
% This constraint is the broad_copyleft_reading in the gpl_derivative_work_trigger kernel family. The narrow reading treats linking as aggregation (lower ε, rope-like). The interface boundary reading treats clean APIs as blocking derivation (context-dependent ε). All three share the kernel but instantiate different constraints with different beneficiary/victim structures and different ε values. This decomposition follows the ε-invariance principle: the label 'GPL derivative work' covers structurally distinct claims.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(gpl_derivative_work_trigger__broad_copyleft_reading, organized, 0.3).
constraint_indexing:directionality_override(gpl_derivative_work_trigger__broad_copyleft_reading, moderate, 0.65).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
