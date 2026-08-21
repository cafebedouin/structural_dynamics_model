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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_non_agent/2,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:fixing_cost_class/2,
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
 *   human_readable: IP Category Emergence: Thinkability Reading
 *   domain: legal_philosophy/intellectual_property/historical_jurisprudence
 *
 * SUMMARY:
 *   This constraint story describes the emergence of 'ownable expression' as
 *   a legally coherent category, primarily marked by the Statute of Anne in
 *   1710. Prior to this, disputes over copying were largely framed within
 *   guild privileges or royal grants, lacking a unified concept of authorial
 *   'right.' The Statute provided the conceptual and linguistic tools to
 *   articulate and enforce such rights, making the category 'thinkable'
 *   within the legal system. This reading emphasizes the shift in conceptual
 *   space and legal coherence rather than the immediate economic impact or
 *   the specific rights granted.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ip_category_emergence__thinkability_reading, 0.05).
domain_priors:suppression_score(ip_category_emergence__thinkability_reading, 0.1).
domain_priors:theater_ratio(ip_category_emergence__thinkability_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ip_category_emergence__thinkability_reading, extractiveness, 0.05).
narrative_ontology:constraint_metric(ip_category_emergence__thinkability_reading, suppression_requirement, 0.1).
narrative_ontology:constraint_metric(ip_category_emergence__thinkability_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ip_category_emergence__thinkability_reading, accessibility_collapse, 0.9).
narrative_ontology:constraint_metric(ip_category_emergence__thinkability_reading, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ip_category_emergence__thinkability_reading, mountain).
narrative_ontology:human_readable(ip_category_emergence__thinkability_reading, "IP Category Emergence: Thinkability Reading").
narrative_ontology:topic_domain(ip_category_emergence__thinkability_reading, "legal_philosophy/intellectual_property/historical_jurisprudence").

domain_priors:emerges_naturally(ip_category_emergence__thinkability_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ip_category_emergence__thinkability_reading, 'af31f53f-0faf-43ac-a765-11905df743d9').
narrative_ontology:cs_kernel_codification('af31f53f-0faf-43ac-a765-11905df743d9', formalized).
narrative_ontology:cs_authority_grounding('af31f53f-0faf-43ac-a765-11905df743d9', lineage).
narrative_ontology:cs_interpretation_layer_present('af31f53f-0faf-43ac-a765-11905df743d9').
narrative_ontology:cs_reading_relation('af31f53f-0faf-43ac-a765-11905df743d9', ip_category_emergence__first_holding_reading, coexists_with).
narrative_ontology:cs_reading_relation('af31f53f-0faf-43ac-a765-11905df743d9', ip_category_emergence__synchronic_diachronic_seam, coexists_with).
narrative_ontology:cs_axiom('af31f53f-0faf-43ac-a765-11905df743d9', foundational, legal_categories_are_constitutive).
narrative_ontology:cs_axiom_status(legal_categories_are_constitutive, holdable).
narrative_ontology:cs_axiom_grounding('af31f53f-0faf-43ac-a765-11905df743d9', legal_categories_are_constitutive, conventional).
narrative_ontology:cs_reference_frame('af31f53f-0faf-43ac-a765-11905df743d9', post_statute_of_anne_coherence).
narrative_ontology:cs_drift_state('af31f53f-0faf-43ac-a765-11905df743d9', contemporary_legal_discourse, gap(stable, minor, true)).
narrative_ontology:cs_created_at('af31f53f-0faf-43ac-a765-11905df743d9', '').
narrative_ontology:cs_kernel_id(ip_category_emergence__thinkability_reading, ip_category_emergence).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ip_category_emergence__thinkability_reading, authors_and_publishers).
narrative_ontology:constraint_beneficiary(ip_category_emergence__thinkability_reading, legal_system).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The legislative body that passed the Statute of Anne in 1710, formally establishing 'copy right' as a distinct legal category, thereby making the concept legally coherent and actionable.
narrative_ontology:constraint_stakeholder(ip_category_emergence__thinkability_reading, parliament_of_great_britain, agenda_setter,
    institutional, generational, analytical, national).

% Benefited from the emergence of a clear legal category for 'ownable expression,' which provided a stable basis for asserting and transferring rights over their works, replacing prior guild-based privileges.
narrative_ontology:constraint_stakeholder(ip_category_emergence__thinkability_reading, authors_and_publishers, beneficiary,
    organized, biographical, mobile, national).

% Benefited from the conceptual clarity, allowing for more consistent adjudication and development of intellectual property law, moving beyond ad-hoc arrangements.
narrative_ontology:constraint_stakeholder(ip_category_emergence__thinkability_reading, legal_system, beneficiary,
    institutional, civilizational, identity_locked, national).

% Represents the collective body of works freely available for use. While not an agent, its 'interests' were implicitly affected as the conceptual space for 'ownable expression' expanded, setting the stage for future limitations on free access.
narrative_ontology:constraint_stakeholder(ip_category_emergence__thinkability_reading, public_domain, excluded,
    powerless, generational, trapped, universal).
narrative_ontology:stakeholder_non_agent(ip_category_emergence__thinkability_reading, public_domain).

% Analyze the historical development of legal concepts, including the shift in the understanding of intellectual property around 1710, and its implications for legal coherence.
narrative_ontology:constraint_stakeholder(ip_category_emergence__thinkability_reading, legal_historians, observer,
    analytical, generational, analytical, global).

% Study the theoretical and practical aspects of intellectual property, including the foundational conceptual shifts that made modern IP law possible.
narrative_ontology:constraint_stakeholder(ip_category_emergence__thinkability_reading, intellectual_property_scholars, observer,
    analytical, biographical, analytical, global).

narrative_ontology:fixing_cost_class(ip_category_emergence__thinkability_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provided a coherent conceptual and legal framework for understanding and discussing 'ownable expression,' enabling coordinated legal action, market development, and scholarly discourse around creative works.
% TRANSFER_FUNCTION: Established the conceptual and legal basis for the transfer of rights over creative works from a previously diffuse or guild-controlled state to individual authors and publishers, making such transfers legally intelligible.
% ABSENT_VOICES: The pre-1710 public, whose implicit free access to works was not framed as a 'right' in opposition to authorial claims, and future advocates for a more robust public domain, whose arguments would be shaped by the new conceptual framework.
% DISAPPEARANCE_RATIONALE: If the legal coherence of 'ownable expression' vanished, the entire edifice of modern intellectual property law and the markets built upon it would collapse. The legal and economic landscape for creative works would be fundamentally unrecognizable, reverting to a state of conceptual fragmentation or guild-based privilege.
% FOUNDING_PROBLEM: The lack of a clear, unified, and legally coherent concept for authorial rights, leading to fragmented and often contested claims over copying and publishing privileges that were previously managed by monopolies or ad-hoc arrangements.
% FOUNDING_PROBLEM_CORROBORATION: Legal historians and intellectual property scholars widely corroborate the pre-1710 conceptual fragmentation and the Statute of Anne's pivotal role in establishing a new, coherent legal category. While the *specific problems* of IP are ongoing, the *foundational conceptual problem* of 'what is ownable expression' was addressed by this emergence.
narrative_ontology:disappearance_verdict(ip_category_emergence__thinkability_reading, world_rearranges).
narrative_ontology:founding_problem_status(ip_category_emergence__thinkability_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ip_category_emergence__thinkability_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
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
 *   The constraint is claimed as a Mountain because the conceptual coherence of 'ownable expression' became a foundational, almost immutable, aspect of the legal landscape post-1710. Its extractiveness, suppression, and theater ratio are very low because the constraint itself is the *emergence of a concept*, not a mechanism for direct extraction or performance. The high accessibility collapse reflects that once the concept is legally coherent, it becomes the default framework for discussing rights over creative works, making it difficult to operate outside this conceptual space. Resistance is low because the resistance is typically directed at the *application* and *scope* of IP rights, not at the fundamental coherence of the category itself.
 *
 * PERSPECTIVAL GAP:
 *   While the conceptual emergence itself is a historical fact, different stakeholders (e.g., legal historians vs. IP advocates) might emphasize different aspects of its 'naturalness' or 'constructedness.' The engine's classification will highlight the tension between the claimed 'mountain' status and the presence of beneficiaries, triggering False Summit Mountain detection.
 *
 * DIRECTIONALITY LOGIC:
 *   The Parliament of Great Britain acted as the agenda-setter, formalizing the conceptual shift. Authors and publishers, along with the legal system itself, are beneficiaries, gaining clarity and a stable framework for their activities. The 'public domain' is an excluded entity, as its implicit prior state of free access was not framed as a 'right' in the pre-1710 discourse, and its 'interests' were not directly represented in the conceptual shift, though it was structurally affected.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    conceptual_vs_social_construction,
    'Is the ''emergence'' of a legal category a ''natural'' conceptual development, or a social and political construction that could have unfolded differently?',
    'Comparative legal history analysis: examining how other legal systems developed similar concepts, or counterfactual historical analysis of alternative legislative paths.',
    'If primarily a social construction, the ''mountain'' claim is weakened, suggesting a more ''rope-like'' or ''tangled_rope'' nature where human choice and power dynamics were more central to its establishment.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(conceptual_vs_social_construction, conceptual, 'Ambiguity between natural conceptual evolution and human-driven legal construction.').

omega_variable(
    thinkability_vs_first_holding_causality,
    'Did the conceptual coherence (thinkability) of ''ownable expression'' precede and enable the establishment of authorial rights (first holding), or were they co-constitutive?',
    'Detailed historical-philosophical analysis of legal texts and debates leading up to and immediately following the Statute of Anne, tracing the causal flow between conceptual articulation and legal enactment.',
    'If thinkability strictly preceded first holding, this reading''s foundational role is strengthened. If co-constitutive, the readings are more deeply intertwined, potentially blurring the boundaries between them.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(thinkability_vs_first_holding_causality, empirical, 'Causal relationship between conceptual emergence and the establishment of rights.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ip_category_emergence__thinkability_reading, 1700, 1720).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ip_c_tr_t1700, ip_category_emergence__thinkability_reading, theater_ratio, 1700, 0.03).
narrative_ontology:measurement(ip_c_tr_t1705, ip_category_emergence__thinkability_reading, theater_ratio, 1705, 0.04).
narrative_ontology:measurement(ip_c_tr_t1710, ip_category_emergence__thinkability_reading, theater_ratio, 1710, 0.05).
narrative_ontology:measurement(ip_c_tr_t1715, ip_category_emergence__thinkability_reading, theater_ratio, 1715, 0.05).
narrative_ontology:measurement(ip_c_tr_t1720, ip_category_emergence__thinkability_reading, theater_ratio, 1720, 0.05).

% Extraction over time
narrative_ontology:measurement(ip_c_be_t1700, ip_category_emergence__thinkability_reading, base_extractiveness, 1700, 0.03).
narrative_ontology:measurement(ip_c_be_t1705, ip_category_emergence__thinkability_reading, base_extractiveness, 1705, 0.04).
narrative_ontology:measurement(ip_c_be_t1710, ip_category_emergence__thinkability_reading, base_extractiveness, 1710, 0.05).
narrative_ontology:measurement(ip_c_be_t1715, ip_category_emergence__thinkability_reading, base_extractiveness, 1715, 0.05).
narrative_ontology:measurement(ip_c_be_t1720, ip_category_emergence__thinkability_reading, base_extractiveness, 1720, 0.05).

% Suppression requirement over time
narrative_ontology:measurement(ip_c_su_t1700, ip_category_emergence__thinkability_reading, suppression_requirement, 1700, 0.08).
narrative_ontology:measurement(ip_c_su_t1705, ip_category_emergence__thinkability_reading, suppression_requirement, 1705, 0.09).
narrative_ontology:measurement(ip_c_su_t1710, ip_category_emergence__thinkability_reading, suppression_requirement, 1710, 0.1).
narrative_ontology:measurement(ip_c_su_t1715, ip_category_emergence__thinkability_reading, suppression_requirement, 1715, 0.1).
narrative_ontology:measurement(ip_c_su_t1720, ip_category_emergence__thinkability_reading, suppression_requirement, 1720, 0.1).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ip_category_emergence__thinkability_reading, information_standard).
narrative_ontology:affects_constraint(ip_category_emergence__thinkability_reading, ip_category_emergence__first_holding_reading).
narrative_ontology:affects_constraint(ip_category_emergence__thinkability_reading, ip_category_emergence__synchronic_diachronic_seam).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'ip_category_emergence' kernel, focusing on the conceptual coherence of 'ownable expression.' The other readings address the establishment of authorial rights and the relationship between these two aspects.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
