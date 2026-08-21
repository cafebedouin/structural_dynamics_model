% ============================================================================
% CONSTRAINT STORY: ip_category_emergence__thinkability_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ip_category_emergence__thinkability_reading, []).

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
 *   constraint_id: ip_category_emergence__thinkability_reading
 *   human_readable: IP Category Emergence: Thinkability of Ownable Expression (1710)
 *   domain: legal_philosophy/intellectual_property/historical_jurisprudence
 *
 * SUMMARY:
 *   This constraint describes the conceptual emergence of 'ownable
 *   expression' as a legally coherent category, marked by the Statute of Anne
 *   in 1710. It is a 'thinkability' reading of the broader 'IP category
 *   emergence' kernel. Before 1710, the legal and philosophical tools to
 *   articulate a 'right' to one's creative work, distinct from a printer's
 *   privilege or a state-granted monopoly, were largely absent. The
 *   constraint itself is the conceptual boundary that shifted, making new
 *   legal claims possible. It is classified as a Mountain because it
 *   represents a fundamental shift in legal-conceptual space, not a
 *   human-constructed or actively enforced rule in the traditional sense. Its
 *   'naturalness' refers to the internal coherence and self-evidence of the
 *   concept once it emerged, rather than a physical law.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ip_category_emergence__thinkability_reading, 0.05).
domain_priors:suppression_score(ip_category_emergence__thinkability_reading, 0.02).
domain_priors:theater_ratio(ip_category_emergence__thinkability_reading, 0.01).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ip_category_emergence__thinkability_reading, extractiveness, 0.05).
narrative_ontology:constraint_metric(ip_category_emergence__thinkability_reading, suppression_requirement, 0.02).
narrative_ontology:constraint_metric(ip_category_emergence__thinkability_reading, theater_ratio, 0.01).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ip_category_emergence__thinkability_reading, accessibility_collapse, 0.95).
narrative_ontology:constraint_metric(ip_category_emergence__thinkability_reading, resistance, 0.01).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ip_category_emergence__thinkability_reading, mountain).
narrative_ontology:human_readable(ip_category_emergence__thinkability_reading, "IP Category Emergence: Thinkability of Ownable Expression (1710)").
narrative_ontology:topic_domain(ip_category_emergence__thinkability_reading, "legal_philosophy/intellectual_property/historical_jurisprudence").

domain_priors:emerges_naturally(ip_category_emergence__thinkability_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ip_category_emergence__thinkability_reading, 'c8cef62b-fe79-4fde-8ace-73bf16f81625').
narrative_ontology:cs_kernel_codification('c8cef62b-fe79-4fde-8ace-73bf16f81625', formalized).
narrative_ontology:cs_authority_grounding('c8cef62b-fe79-4fde-8ace-73bf16f81625', lineage).
narrative_ontology:cs_interpretation_layer_present('c8cef62b-fe79-4fde-8ace-73bf16f81625').
narrative_ontology:cs_reading_relation('c8cef62b-fe79-4fde-8ace-73bf16f81625', ip_category_emergence__first_holding_reading, influences).
narrative_ontology:cs_reading_relation('c8cef62b-fe79-4fde-8ace-73bf16f81625', ip_category_emergence__synchronic_diachronic_seam, coexists_with).
narrative_ontology:cs_axiom('c8cef62b-fe79-4fde-8ace-73bf16f81625', foundational, conceptual_coherence_precedes_legal_enforcement).
narrative_ontology:cs_axiom_status(conceptual_coherence_precedes_legal_enforcement, holdable).
narrative_ontology:cs_axiom_grounding('c8cef62b-fe79-4fde-8ace-73bf16f81625', conceptual_coherence_precedes_legal_enforcement, conventional).
narrative_ontology:cs_reference_frame('c8cef62b-fe79-4fde-8ace-73bf16f81625', pre_1710_conceptual_void).
narrative_ontology:cs_drift_state('c8cef62b-fe79-4fde-8ace-73bf16f81625', post_statute_of_anne, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('c8cef62b-fe79-4fde-8ace-73bf16f81625', '').
narrative_ontology:cs_kernel_id(ip_category_emergence__thinkability_reading, ip_category_emergence).

% --- Structural relationships ---
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(ip_category_emergence__thinkability_reading, post_1710_authors_and_publishers).
narrative_ontology:constraint_vindicates(ip_category_emergence__thinkability_reading, conceptual_space_evolution).
narrative_ontology:constraint_vindicates(ip_category_emergence__thinkability_reading, legal_coherence_as_natural_limit).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Analyze the historical development of legal concepts, identifying the point at which 'ownable expression' became a coherent legal category. They observe the constraint's emergence as a conceptual shift.
narrative_ontology:constraint_stakeholder(ip_category_emergence__thinkability_reading, legal_historians, observer,
    analytical, generational, analytical, global).

% Operated under a regime of guild privileges and censorship, lacking the conceptual framework to claim 'copyright' as an inherent right to their expression. Their disputes were framed in terms of monopolies or licenses, not authorial ownership.
narrative_ontology:constraint_stakeholder(ip_category_emergence__thinkability_reading, pre_1710_printers_and_authors, excluded,
    powerless, biographical, trapped, local).

% Gained a new conceptual tool ('copy right') to assert ownership over their works, distinct from prior guild-based privileges. This conceptual shift enabled new forms of legal and economic action.
narrative_ontology:constraint_stakeholder(ip_category_emergence__thinkability_reading, post_1710_authors_and_publishers, beneficiary,
    moderate, biographical, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared conceptual framework for understanding and discussing the legal status of creative works, enabling coordinated legal and economic activity around 'ownable expression'.
% TRANSFER_FUNCTION: The constraint itself does not transfer anything, but its emergence enabled the transfer of rights and economic value from the public domain (or prior privilege holders) to authors and publishers.
% ABSENT_VOICES: Pre-1710 authors and printers, whose disputes were framed in terms of guild privileges or censorship, lacked the conceptual vocabulary to articulate claims of 'ownable expression'. Their 'absence' is a structural feature of the pre-emergence conceptual space.
% DISAPPEARANCE_RATIONALE: The conceptual emergence of 'ownable expression' is a historical fact; it cannot 'disappear'. If the concept were somehow un-thought, the legal and economic structures built upon it would collapse, but the historical moment of its emergence would remain a fixed point.
% FOUNDING_PROBLEM: The problem of how to legally recognize and protect the rights of authors to their creative works, beyond mere printing privileges or censorship controls.
% FOUNDING_PROBLEM_CORROBORATION: Legal scholars and historians widely corroborate the historical problem of recognizing authorial rights. The ongoing evolution of IP law (e.g., digital rights, AI-generated content) demonstrates the problem remains live, even if its specific manifestations change.
narrative_ontology:disappearance_verdict(ip_category_emergence__thinkability_reading, world_unchanged).
narrative_ontology:founding_problem_status(ip_category_emergence__thinkability_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ip_category_emergence__thinkability_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(ip_category_emergence__thinkability_reading, 'none', 1).
narrative_ontology:epsilon_provenance(ip_category_emergence__thinkability_reading, 0.05, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ip_category_emergence__thinkability_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(ip_category_emergence__thinkability_reading, ExtMetricName, E),
    domain_priors:suppression_score(ip_category_emergence__thinkability_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(ip_category_emergence__thinkability_reading),
    narrative_ontology:constraint_metric(ip_category_emergence__thinkability_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(ip_category_emergence__thinkability_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(ip_category_emergence__thinkability_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is very low (0.05) because the constraint itself is a conceptual boundary, not an extractive mechanism. It enables extraction, but does not perform it. Suppression is also very low (0.02) as it's a conceptual shift, not a coercive force. Accessibility collapse is high (0.95) because once the concept of 'ownable expression' became coherent, the prior conceptual space (where such a right was unthinkable) largely collapsed as a viable framework for legal discourse. Resistance is negligible (0.01) as one cannot 'resist' a conceptual emergence. The claimed type is Mountain because it describes a fundamental, irreducible shift in the legal-conceptual landscape, a new 'point' in conceptual space that, once established, is fixed.
 *
 * PERSPECTIVAL GAP:
 *   The constraint is experienced as a fundamental shift by those who study legal history (observers) and as an enabling condition by those who subsequently benefit from the new legal framework (post-1710 authors). For pre-1710 actors, it was an 'absent' concept. The engine's classification will reflect this conceptual, rather than directly extractive, nature.
 *
 * DIRECTIONALITY LOGIC:
 *   Legal historians are observers. Pre-1710 actors were 'excluded' from the conceptual space of 'ownable expression' by definition, as the concept had not yet emerged. Post-1710 authors and publishers are beneficiaries, as the new conceptual framework provided them with a powerful new legal tool. The constraint itself, as a conceptual emergence, does not have direct beneficiaries or victims in the sense of active extraction, but it enables new forms of benefit and cost for those operating within the new conceptual space.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    conceptual_vs_material_causation,
    'To what extent was the conceptual emergence of ''ownable expression'' a driver of new legal and economic practices, versus a reflection of pre-existing material changes in printing technology and markets?',
    'Detailed historical analysis tracing the causal pathways between conceptual shifts (e.g., philosophical treatises on authorship) and legislative action, compared against economic histories of the printing industry.',
    'If primarily a driver, the constraint''s ''naturalness'' as a conceptual Mountain is strengthened. If primarily a reflection, it might be re-read as a Snare or Tangled Rope, where the conceptual shift served to legitimize pre-existing extractive practices.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(conceptual_vs_material_causation, empirical, 'Distinguishing conceptual emergence as cause vs. effect in legal history.').

omega_variable(
    natural_law_vs_social_construct,
    'Is the ''thinkability'' of ownable expression a discovery of a natural legal principle, or a contingent social construct that could have developed differently?',
    'Comparative legal history across diverse cultures and legal traditions, examining whether similar concepts emerged independently or if the Western trajectory was unique.',
    'If a natural principle, the Mountain classification is robust. If a contingent construct, the ''naturalness'' claim is weakened, potentially reclassifying it as a Rope or Tangled Rope (a coordinated social construct).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(natural_law_vs_social_construct, conceptual, 'The philosophical status of ''ownable expression'' as natural law or social construct.').

omega_variable(
    reading_framing_underdetermination,
    'Does the ''thinkability_reading'' adequately capture the full structural dynamics of IP category emergence, or does the ''first_holding_reading'' offer a more complete account of the constraint''s initial impact?',
    'Analysis of the downstream legal and economic consequences: if the primary impact was the creation of new rights-holders and the re-allocation of value, the ''first_holding_reading'' might be more structurally salient. If the primary impact was the opening of a new conceptual space for legal discourse, the ''thinkability_reading'' is more salient.',
    'If the ''first_holding_reading'' is more salient, the overall ''ip_category_emergence'' kernel might be better represented by a constraint with higher extractiveness and more active enforcement, reflecting the immediate re-allocation of value. This ''thinkability_reading'' would then be seen as a foundational but less directly impactful component.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_framing_underdetermination, conceptual, 'Underdetermination between the ''thinkability'' and ''first_holding'' readings of IP category emergence.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ip_category_emergence__thinkability_reading, 1700, 1720).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ip_c_tr_t1700, ip_category_emergence__thinkability_reading, theater_ratio, 1700, 0.01).
narrative_ontology:measurement(ip_c_tr_t1710, ip_category_emergence__thinkability_reading, theater_ratio, 1710, 0.01).
narrative_ontology:measurement(ip_c_tr_t1720, ip_category_emergence__thinkability_reading, theater_ratio, 1720, 0.01).

% Extraction over time
narrative_ontology:measurement(ip_c_be_t1700, ip_category_emergence__thinkability_reading, base_extractiveness, 1700, 0.01).
narrative_ontology:measurement(ip_c_be_t1710, ip_category_emergence__thinkability_reading, base_extractiveness, 1710, 0.05).
narrative_ontology:measurement(ip_c_be_t1720, ip_category_emergence__thinkability_reading, base_extractiveness, 1720, 0.05).

% Suppression requirement over time
narrative_ontology:measurement(ip_c_su_t1700, ip_category_emergence__thinkability_reading, suppression_requirement, 1700, 0.01).
narrative_ontology:measurement(ip_c_su_t1710, ip_category_emergence__thinkability_reading, suppression_requirement, 1710, 0.02).
narrative_ontology:measurement(ip_c_su_t1720, ip_category_emergence__thinkability_reading, suppression_requirement, 1720, 0.02).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ip_category_emergence__thinkability_reading, information_standard).
narrative_ontology:affects_constraint(ip_category_emergence__thinkability_reading, ip_category_emergence__first_holding_reading).
narrative_ontology:affects_constraint(ip_category_emergence__thinkability_reading, ip_category_emergence__synchronic_diachronic_seam).

% DUAL FORMULATION NOTE:
% This constraint is the 'thinkability_reading' of the 'ip_category_emergence' kernel. It focuses on the conceptual shift that made 'ownable expression' coherent, distinct from the 'first_holding_reading' (which focuses on the re-allocation of rights) and the 'synchronic_diachronic_seam' (which analyzes the relationship between conceptual and practical emergence).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
