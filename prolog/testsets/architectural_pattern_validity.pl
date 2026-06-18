% ============================================================================
% CONSTRAINT STORY: architectural_pattern_validity
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2025-01-09
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_architectural_pattern_validity, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    domain_priors:emerges_naturally/1,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_axiom_grounding/3,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:cs_created_at/2,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: architectural_pattern_validity
 *   human_readable: Architectural Pattern Validity Independent of Organizational Reality
 *   domain: technology_governance/standards_development/organizational_epistemology
 *
 * SUMMARY:
 *   The Polaris document presents novel architectural patterns (collaboration
 *   scale governing component precipitation; wire-buys-privacy vs
 *   trust-root-buys-privilege; sovereignty-as-classification) whose
 *   organizational provenance is ambiguous across four readings. This
 *   constraint addresses whether the patterns represent valid engineering
 *   insights independent of that ambiguity. The constraint is claimed as
 *   mountain because pattern validity is determined by technical criteria
 *   (applicability to real systems, independent adoption, coherence under
 *   stress testing) that are orthogonal to the source's organizational
 *   status. A valid architectural pattern works when implemented, regardless
 *   of whether its source organization exists, is fictional, is a conceptual
 *   framework, or is pre-public. KEY AGENTS (by structural relationship): -
 *   architectural_pattern_adopters: Primary beneficiaries (organized/mobile)
 *   — gain access to potentially valuable design insights without requiring
 *   organizational verification - distributed_systems_researchers: Primary
 *   beneficiaries (organized/mobile) — gain novel conceptual vocabulary for
 *   systems thinking - polaris_document_interpreters: Analytical observers
 *   (analytical/analytical) — separate architectural claims from
 *   organizational claims - standards_body_practitioners: Institutional
 *   observers (institutional/mobile) — assess innovation in standards
 *   methodology
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(architectural_pattern_validity, 0.08).
domain_priors:suppression_score(architectural_pattern_validity, 0.12).
domain_priors:theater_ratio(architectural_pattern_validity, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(architectural_pattern_validity, extractiveness, 0.08).
narrative_ontology:constraint_metric(architectural_pattern_validity, suppression_requirement, 0.12).
narrative_ontology:constraint_metric(architectural_pattern_validity, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(architectural_pattern_validity, accessibility_collapse, 0.88).
narrative_ontology:constraint_metric(architectural_pattern_validity, resistance, 0.09).

% --- Constraint claim ---
narrative_ontology:constraint_claim(architectural_pattern_validity, mountain).
narrative_ontology:human_readable(architectural_pattern_validity, "Architectural Pattern Validity Independent of Organizational Reality").
narrative_ontology:topic_domain(architectural_pattern_validity, "technology_governance/standards_development/organizational_epistemology").

domain_priors:emerges_naturally(architectural_pattern_validity).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(architectural_pattern_validity, 'aede526b-1c72-4740-8a6f-619d8b162212').
narrative_ontology:cs_kernel_codification('aede526b-1c72-4740-8a6f-619d8b162212', fixed_text).
narrative_ontology:cs_authority_grounding('aede526b-1c72-4740-8a6f-619d8b162212', expertise).
narrative_ontology:cs_interpretation_layer_present('aede526b-1c72-4740-8a6f-619d8b162212').
narrative_ontology:cs_reading_relation('aede526b-1c72-4740-8a6f-619d8b162212', architectural_pattern_validity__authoritative_specification_reading, coexists_with).
narrative_ontology:cs_reading_relation('aede526b-1c72-4740-8a6f-619d8b162212', architectural_pattern_validity__fictional_construct_reading, coexists_with).
narrative_ontology:cs_reading_relation('aede526b-1c72-4740-8a6f-619d8b162212', architectural_pattern_validity__pre_public_initiative_reading, coexists_with).
narrative_ontology:cs_axiom('aede526b-1c72-4740-8a6f-619d8b162212', foundational, pattern_validity_orthogonal_to_organizational_reality).
narrative_ontology:cs_axiom_status(pattern_validity_orthogonal_to_organizational_reality, holdable).
narrative_ontology:cs_axiom_grounding('aede526b-1c72-4740-8a6f-619d8b162212', pattern_validity_orthogonal_to_organizational_reality, empirically_contingent).
narrative_ontology:cs_axiom('aede526b-1c72-4740-8a6f-619d8b162212', secondary, technical_merit_independently_verifiable).
narrative_ontology:cs_axiom_status(technical_merit_independently_verifiable, holdable).
narrative_ontology:cs_axiom_grounding('aede526b-1c72-4740-8a6f-619d8b162212', technical_merit_independently_verifiable, empirically_contingent).
narrative_ontology:cs_reference_frame('aede526b-1c72-4740-8a6f-619d8b162212', epistemic_utility_independent_of_instantiation).
narrative_ontology:cs_drift_state('aede526b-1c72-4740-8a6f-619d8b162212', contemporary_verification_attempt, gap(stable, minor, true)).
narrative_ontology:cs_created_at('aede526b-1c72-4740-8a6f-619d8b162212', '').

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(architectural_pattern_validity, architectural_pattern_adopters).
narrative_ontology:constraint_beneficiary(architectural_pattern_validity, distributed_systems_researchers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Engineers and architects who evaluate the patterns (collaboration scale governing component precipitation; wire-buys-privacy vs trust-root-buys-privilege; sovereignty-as-classification) for applicability to their own systems. They benefit if the patterns are valid engineering insights regardless of Polaris's organizational status, because valid patterns solve real design problems. Their adoption decision depends on technical coherence under stress testing, not on whether Polaris exists as described.
narrative_ontology:constraint_stakeholder(architectural_pattern_validity, architectural_pattern_adopters, beneficiary,
    organized, biographical, mobile, global).

% Academic and industrial researchers working on distributed systems architecture, trust models, and sovereignty frameworks. They benefit from novel architectural patterns that advance the field's conceptual vocabulary, independent of the source's organizational reality. Their interest is in whether the patterns represent genuine contributions to systems thinking, testable through formal analysis and independent implementation.
narrative_ontology:constraint_stakeholder(architectural_pattern_validity, distributed_systems_researchers, beneficiary,
    organized, generational, mobile, global).

% Analysts attempting to determine the document's status across the four readings (authoritative specification, conceptual framework, fictional construct, pre-public initiative). They observe that pattern validity is orthogonal to organizational reality: the patterns either solve real engineering problems or they don't, regardless of whether Polaris exists. Their analytical task is separating the architectural claims from the organizational claims.
narrative_ontology:constraint_stakeholder(architectural_pattern_validity, polaris_document_interpreters, observer,
    analytical, biographical, analytical, global).

% Practitioners from established standards organizations (IETF, W3C, ISO) who evaluate whether the architectural patterns represent genuine innovations in standards development methodology. They can assess technical merit independently of Polaris's organizational status, because pattern validity is determined by applicability to real coordination problems, not by the authority of the source.
narrative_ontology:constraint_stakeholder(architectural_pattern_validity, standards_body_practitioners, observer,
    institutional, generational, mobile, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The constraint coordinates understanding of whether novel architectural patterns constitute valid engineering insights by establishing that pattern validity is testable through independent adoption, technical coherence under stress, and applicability to real systems—criteria that are independent of the source document's organizational status.
% TRANSFER_FUNCTION: No material transfer occurs. The constraint describes a structural feature of engineering knowledge: valid patterns are those that work when implemented, regardless of their source's organizational reality. The 'cost' is cognitive effort to evaluate patterns on technical merit rather than organizational authority; the 'benefit' is access to potentially valuable design insights without requiring verification of organizational claims.
% ABSENT_VOICES: Parties who would benefit from conflating pattern validity with organizational authority are structurally absent: no entity profits from requiring that architectural patterns only be valid if their source organization exists. The constraint's operation makes such extraction impossible because technical validity is independently verifiable.
% DISAPPEARANCE_RATIONALE: If the constraint disappeared and pattern validity became dependent on organizational reality, engineers would still evaluate patterns through implementation and stress testing—the same technical criteria. The constraint describes what engineers already do: assess architectural patterns on technical merit. Its disappearance would not change engineering practice because the independence of pattern validity from organizational status is a structural feature of how technical knowledge works, not a constructed rule.
% FOUNDING_PROBLEM: The founding problem is epistemological: how to evaluate architectural patterns when the source document's organizational status is ambiguous across four readings. Without the constraint, evaluators might incorrectly believe they must first resolve the organizational question before assessing technical merit.
% FOUNDING_PROBLEM_CORROBORATION: The problem is corroborated by the commitment system recognition block itself, which documents the four-reading ambiguity as an unresolved interpretive question. Independent verification: the analytical task of separating architectural claims from organizational claims is a standard problem in technology assessment, documented in academic literature on design pattern evaluation and standards development methodology (sources outside any benefiting party).
narrative_ontology:disappearance_verdict(architectural_pattern_validity, world_unchanged).
narrative_ontology:founding_problem_status(architectural_pattern_validity, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(architectural_pattern_validity, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-17',
    'unspecified', 'agent/example_platform_commission.json',
    'claude-sonnet-4-5-20250929', 'unspecified').
narrative_ontology:story_seed(architectural_pattern_validity, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(architectural_pattern_validity_tests).

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(architectural_pattern_validity, ExtMetricName, E),
    domain_priors:suppression_score(architectural_pattern_validity, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(architectural_pattern_validity),
    narrative_ontology:constraint_metric(architectural_pattern_validity, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(architectural_pattern_validity, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(architectural_pattern_validity_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is very low (0.08) because the constraint describes a structural feature of engineering knowledge rather than extracting from participants. No party collects rents from the principle that pattern validity is independent of organizational reality. Suppression is low (0.12) because evaluators are free to assess patterns on any criteria they choose; the constraint merely describes what technical validity consists of, not what evaluators must do. Theater ratio is very low (0.05) because the constraint's operation is the actual practice of pattern evaluation, not performance. Accessibility collapse is high (0.88) because once the independence of pattern validity from organizational status is understood, alternative framings (requiring organizational verification before technical assessment) become structurally incoherent. Resistance is low (0.09) because the constraint aligns with existing engineering practice. The flat measurement trajectories reflect that this is a structural feature of how technical knowledge works, not a time-varying enforcement regime.
 *
 * PERSPECTIVAL GAP:
 *   All seats should compute as mountain because the constraint describes a structural feature of technical knowledge that operates identically from every position. Pattern validity is determined by implementation success and technical coherence, criteria that are observer-independent. The beneficiary seats gain more from the constraint's operation (access to insights without organizational verification cost) but this is asymmetric benefit from a natural feature, not extraction. The constraint's mountain character is that it would persist regardless of who defends it: engineers would continue to evaluate patterns through implementation and stress testing even if no one articulated the principle.
 *
 * DIRECTIONALITY LOGIC:
 *   Architectural pattern adopters and distributed systems researchers are beneficiaries because they gain access to potentially valuable insights without the cost of organizational verification. Their directionality is near the beneficiary end (low d) because the constraint removes a potential barrier (organizational verification) from technical assessment. Observers are analytical seats with d at the analytical default. No seat is a target or victim because the constraint extracts from no one—it describes how pattern validity works, which is a feature of engineering epistemology, not a constructed rule that could extract.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint is not mandatrophy because its function (enabling pattern evaluation independent of organizational status) remains live. The founding problem—how to assess architectural patterns when organizational provenance is ambiguous—is an active epistemological question in the Polaris case. The constraint prevents mislabeling this natural feature of engineering knowledge as extraction: no party profits from the principle that valid patterns work when implemented. The low extractiveness and suppression reflect that this is a description of how technical validity operates, not a constructed coordination mechanism that could degrade into rent-seeking.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    pattern_independent_adoption,
    'Have the architectural patterns been independently adopted by implementers outside the Polaris document''s immediate context, demonstrating validity through real-world application?',
    'Survey of distributed systems implementations in the wild: do systems built by parties with no connection to Polaris exhibit the patterns (collaboration scale governing component precipitation, wire-buys-privacy vs trust-root-buys-privilege, sovereignty-as-classification)? Independent adoption would establish the patterns as valid engineering insights; absence of adoption would suggest the patterns are either too novel for diffusion or not actually valid.',
    'Independent adoption would strengthen the mountain classification by demonstrating that the patterns work in practice across diverse contexts. Absence of adoption would not necessarily invalidate the patterns (they may be too recent for diffusion) but would leave their validity status unresolved, potentially shifting the constraint toward a contested empirical claim rather than a structural feature.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(pattern_independent_adoption, empirical, 'Whether independent implementers have adopted the patterns, establishing validity through real-world application.').

omega_variable(
    formal_coherence_under_stress,
    'Do the architectural patterns maintain formal coherence when subjected to adversarial stress testing, or do they break down under edge cases that reveal hidden assumptions or contradictions?',
    'Formal analysis and adversarial testing: model the patterns in a formal framework (process calculi, temporal logic, game-theoretic models) and probe for contradictions, hidden assumptions, or failure modes. Coherence under stress would establish the patterns as structurally sound; breakdown would reveal them as heuristics that work only under unstated conditions.',
    'Formal coherence would support the mountain classification by demonstrating that the patterns are not merely plausible-sounding heuristics but structurally sound principles. Breakdown under stress testing would shift the constraint toward a contested claim, potentially revealing that what appears as a natural feature is actually a constructed heuristic with limited applicability.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(formal_coherence_under_stress, empirical, 'Whether the patterns maintain formal coherence under adversarial stress testing.').

omega_variable(
    organizational_reality_coupling,
    'Is there any structural coupling between the patterns'' technical validity and Polaris''s organizational reality, such that the patterns only work if Polaris exists as described, or are they genuinely orthogonal?',
    'Counterfactual analysis: if Polaris were definitively established as fictional (or definitively established as real), would the patterns'' technical validity change? If validity is truly independent, the answer must be no. If validity depends on organizational reality, that dependence would reveal hidden extraction or coordination functions not captured in the current analysis.',
    'Genuine orthogonality would confirm the mountain classification and support the conceptual framework reading. Discovery of coupling would shift the constraint toward tangled rope or snare, revealing that what appears as a natural feature actually depends on constructed organizational reality, with potential beneficiaries (Polaris itself, if real) extracting legitimacy from the patterns'' apparent validity.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(organizational_reality_coupling, conceptual, 'Whether pattern validity is genuinely independent of Polaris''s organizational status or structurally coupled to it.').

omega_variable(
    beneficiary_ambiguity_mountain_fsm,
    'Does the presence of identifiable beneficiaries (architectural pattern adopters, distributed systems researchers) indicate that this constraint is a false summit—a constructed coordination mechanism presented as natural law—or do the beneficiaries merely gain asymmetric access to a genuine natural feature?',
    'FSM evaluation: assess whether the beneficiaries'' gains depend on the constraint''s enforcement (indicating construction) or merely on their position relative to a natural feature (indicating genuine mountain). If the constraint disappeared and pattern validity became dependent on organizational reality, would the beneficiaries lose their gains because the constraint was protecting them, or would they continue to evaluate patterns through implementation because that''s how technical validity works?',
    'If the beneficiaries'' gains depend on enforcement, the constraint is a false summit and should reclassify to tangled rope, revealing hidden coordination or extraction. If the gains are merely asymmetric access to a natural feature (some parties are better positioned to exploit the fact that pattern validity is independent of organizational status), the mountain classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_ambiguity_mountain_fsm, conceptual, 'Whether beneficiary presence indicates false summit or asymmetric access to a genuine natural feature.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(architectural_pattern_validity, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(arch_tr_t0, architectural_pattern_validity, theater_ratio, 0, 0.05).
narrative_ontology:measurement_basis(arch_tr_t0, observed).
narrative_ontology:measurement(arch_tr_t5, architectural_pattern_validity, theater_ratio, 5, 0.05).
narrative_ontology:measurement_basis(arch_tr_t5, observed).
narrative_ontology:measurement(arch_tr_t10, architectural_pattern_validity, theater_ratio, 10, 0.05).
narrative_ontology:measurement_basis(arch_tr_t10, observed).
narrative_ontology:measurement(arch_tr_t15, architectural_pattern_validity, theater_ratio, 15, 0.05).
narrative_ontology:measurement_basis(arch_tr_t15, observed).
narrative_ontology:measurement(arch_tr_t20, architectural_pattern_validity, theater_ratio, 20, 0.05).
narrative_ontology:measurement_basis(arch_tr_t20, observed).

% Extraction over time
narrative_ontology:measurement(arch_be_t0, architectural_pattern_validity, base_extractiveness, 0, 0.08).
narrative_ontology:measurement_basis(arch_be_t0, observed).
narrative_ontology:measurement(arch_be_t5, architectural_pattern_validity, base_extractiveness, 5, 0.08).
narrative_ontology:measurement_basis(arch_be_t5, observed).
narrative_ontology:measurement(arch_be_t10, architectural_pattern_validity, base_extractiveness, 10, 0.08).
narrative_ontology:measurement_basis(arch_be_t10, observed).
narrative_ontology:measurement(arch_be_t15, architectural_pattern_validity, base_extractiveness, 15, 0.08).
narrative_ontology:measurement_basis(arch_be_t15, observed).
narrative_ontology:measurement(arch_be_t20, architectural_pattern_validity, base_extractiveness, 20, 0.08).
narrative_ontology:measurement_basis(arch_be_t20, observed).

% Suppression requirement over time
narrative_ontology:measurement(arch_su_t0, architectural_pattern_validity, suppression_requirement, 0, 0.12).
narrative_ontology:measurement_basis(arch_su_t0, observed).
narrative_ontology:measurement(arch_su_t5, architectural_pattern_validity, suppression_requirement, 5, 0.12).
narrative_ontology:measurement_basis(arch_su_t5, observed).
narrative_ontology:measurement(arch_su_t10, architectural_pattern_validity, suppression_requirement, 10, 0.12).
narrative_ontology:measurement_basis(arch_su_t10, observed).
narrative_ontology:measurement(arch_su_t15, architectural_pattern_validity, suppression_requirement, 15, 0.12).
narrative_ontology:measurement_basis(arch_su_t15, observed).
narrative_ontology:measurement(arch_su_t20, architectural_pattern_validity, suppression_requirement, 20, 0.12).
narrative_ontology:measurement_basis(arch_su_t20, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(architectural_pattern_validity, information_standard).
narrative_ontology:affects_constraint(architectural_pattern_validity, polaris_organizational_reality).
narrative_ontology:affects_constraint(architectural_pattern_validity, specification_authority_grounding).
narrative_ontology:affects_constraint(architectural_pattern_validity, certification_model_viability).

% DUAL FORMULATION NOTE:
% This constraint is one member of the Polaris document status constraint family. It addresses pattern validity independent of organizational reality (the conceptual framework reading's core claim). Sibling constraints address organizational reality itself (whether Polaris exists), specification authority (what grounds the specs' legitimacy), and certification model viability (whether the revenue model is operational). All four constraints share the contested kernel 'polaris_document_status' but decompose into separate stories because they have different ε values: pattern validity is a low-extraction structural feature (mountain candidate), while organizational reality and specification authority are higher-extraction contested claims (likely tangled rope or contested mountain). The network edges reflect that pattern validity influences but does not determine the other constraints: valid patterns strengthen the case for Polaris's reality but do not prove it; invalid patterns would undermine specification authority but their validity does not establish it.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
