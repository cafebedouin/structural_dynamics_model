% ============================================================================
% CONSTRAINT STORY: ip_category_emergence__synchronic_diachronic_seam
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ip_category_emergence__synchronic_diachronic_seam, []).

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
    domain_priors:emerges_naturally/1,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_kernel_id/2,
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
 *   constraint_id: ip_category_emergence__synchronic_diachronic_seam
 *   human_readable: IP Category Emergence: Synchronic-Diachronic Seam
 *   domain: legal_philosophy/intellectual_property/historical_jurisprudence
 *
 * SUMMARY:
 *   This constraint examines the relationship between the 'thinkability' of
 *   an intellectual property category (its conceptual coherence) and the
 *   'first holding' of a right within that category (its initial legal
 *   recognition). It posits that these two events are either formally
 *   independent or merely appear to co-occur due to temporal framing
 *   artifacts. This reading aims to test whether the structure of IP
 *   categories is authentic or a spurious consequence of historical
 *   narrative. It is a Mountain because it describes a fundamental conceptual
 *   distinction in legal philosophy, not a human-made or enforced rule.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ip_category_emergence__synchronic_diachronic_seam, 0.2).
domain_priors:suppression_score(ip_category_emergence__synchronic_diachronic_seam, 0.1).
domain_priors:theater_ratio(ip_category_emergence__synchronic_diachronic_seam, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ip_category_emergence__synchronic_diachronic_seam, extractiveness, 0.2).
narrative_ontology:constraint_metric(ip_category_emergence__synchronic_diachronic_seam, suppression_requirement, 0.1).
narrative_ontology:constraint_metric(ip_category_emergence__synchronic_diachronic_seam, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ip_category_emergence__synchronic_diachronic_seam, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(ip_category_emergence__synchronic_diachronic_seam, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ip_category_emergence__synchronic_diachronic_seam, mountain).
narrative_ontology:human_readable(ip_category_emergence__synchronic_diachronic_seam, "IP Category Emergence: Synchronic-Diachronic Seam").
narrative_ontology:topic_domain(ip_category_emergence__synchronic_diachronic_seam, "legal_philosophy/intellectual_property/historical_jurisprudence").

domain_priors:emerges_naturally(ip_category_emergence__synchronic_diachronic_seam).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ip_category_emergence__synchronic_diachronic_seam, 'd7084000-1531-4b5f-b28c-eebfed7e9822').
narrative_ontology:cs_kernel_codification('d7084000-1531-4b5f-b28c-eebfed7e9822', distributed).
narrative_ontology:cs_authority_grounding('d7084000-1531-4b5f-b28c-eebfed7e9822', expertise).
narrative_ontology:cs_reading_relation('d7084000-1531-4b5f-b28c-eebfed7e9822', ip_category_emergence__thinkability_reading, influences).
narrative_ontology:cs_reading_relation('d7084000-1531-4b5f-b28c-eebfed7e9822', ip_category_emergence__first_holding_reading, influences).
narrative_ontology:cs_axiom('d7084000-1531-4b5f-b28c-eebfed7e9822', foundational, conceptual_emergence_is_distinct_from_legal_occupancy).
narrative_ontology:cs_axiom_status(conceptual_emergence_is_distinct_from_legal_occupancy, holdable).
narrative_ontology:cs_axiom_grounding('d7084000-1531-4b5f-b28c-eebfed7e9822', conceptual_emergence_is_distinct_from_legal_occupancy, empirically_contingent).
narrative_ontology:cs_reference_frame('d7084000-1531-4b5f-b28c-eebfed7e9822', analytical_separability_of_legal_concepts).
narrative_ontology:cs_drift_state('d7084000-1531-4b5f-b28c-eebfed7e9822', contemporary_interdisciplinary_legal_studies, gap(stable, minor, true)).
narrative_ontology:cs_created_at('d7084000-1531-4b5f-b28c-eebfed7e9822', '').
narrative_ontology:cs_kernel_id(ip_category_emergence__synchronic_diachronic_seam, ip_category_emergence).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ip_category_emergence__synchronic_diachronic_seam, legal_historians).
narrative_ontology:constraint_beneficiary(ip_category_emergence__synchronic_diachronic_seam, ip_theorists).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefit from a clearer understanding of whether IP categories emerge independently of their first occupancy, allowing for more precise historical analysis of legal change.
narrative_ontology:constraint_stakeholder(ip_category_emergence__synchronic_diachronic_seam, legal_historians, beneficiary,
    analytical, generational, analytical, global).

% Benefit from a refined conceptual framework for IP, distinguishing between the 'thinkability' of a category and its 'first holding,' which impacts theories of ownership and innovation.
narrative_ontology:constraint_stakeholder(ip_category_emergence__synchronic_diachronic_seam, ip_theorists, beneficiary,
    analytical, generational, analytical, global).

% The primary source material for analysis, passively reflecting the legal and conceptual structures of their time. They do not actively participate but are the object of interpretation.
narrative_ontology:constraint_stakeholder(ip_category_emergence__synchronic_diachronic_seam, historical_legal_texts, observer,
    powerless, civilizational, trapped, universal).
narrative_ontology:stakeholder_non_agent(ip_category_emergence__synchronic_diachronic_seam, historical_legal_texts).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a framework for distinguishing between the conceptual emergence of an intellectual property category and the historical moment of its first legal recognition or holding, enabling more precise historical and theoretical discourse.
% TRANSFER_FUNCTION: Clarifies the conceptual transfer of legal coherence from an 'unthinkable' state to a 'thinkable' one, and the subsequent transfer of rights from a diffuse public domain to specific claimants.
% ABSENT_VOICES: The historical actors who lived through these legal transitions, whose implicit understandings might offer a different perspective on the perceived independence or co-occurrence of thinkability and first-holding.
% DISAPPEARANCE_RATIONALE: If this distinction vanished, legal history and IP theory would lose a crucial analytical tool, leading to conflated understandings of legal evolution and the nature of intellectual property rights. The academic discourse would become less precise.
% FOUNDING_PROBLEM: The ambiguity in historical legal analysis regarding whether the conceptual possibility of owning something (thinkability) necessarily coincides with the first instance of it being legally owned (first-holding).
% FOUNDING_PROBLEM_CORROBORATION: Legal historians and philosophers outside the immediate IP field corroborate this as a persistent challenge in understanding the evolution of legal concepts, particularly in areas where new forms of property emerge. The problem is attested by ongoing debates in historical jurisprudence.
narrative_ontology:disappearance_verdict(ip_category_emergence__synchronic_diachronic_seam, world_rearranges).
narrative_ontology:founding_problem_status(ip_category_emergence__synchronic_diachronic_seam, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ip_category_emergence__synchronic_diachronic_seam, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(ip_category_emergence__synchronic_diachronic_seam, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ip_category_emergence__synchronic_diachronic_seam_tests).

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(ip_category_emergence__synchronic_diachronic_seam, ExtMetricName, E),
    domain_priors:suppression_score(ip_category_emergence__synchronic_diachronic_seam, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(ip_category_emergence__synchronic_diachronic_seam),
    narrative_ontology:constraint_metric(ip_category_emergence__synchronic_diachronic_seam, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(ip_category_emergence__synchronic_diachronic_seam, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(ip_category_emergence__synchronic_diachronic_seam_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The constraint's extractiveness is negligible because it describes a conceptual distinction, not an active mechanism of extraction. Suppression is low as it's an analytical framework, not enforced. Theater ratio is minimal as its utility is purely in clarifying analysis. Accessibility collapse is high because once the distinction is understood, alternatives for precise historical analysis are limited. Resistance is low because it's a theoretical tool, not a policy. The metrics reflect its nature as a conceptual 'Mountain' in legal theory.
 *
 * PERSPECTIVAL GAP:
 *   As a conceptual constraint, there is little perspectival gap among those who engage with it analytically. Its value is in clarifying distinctions, which benefits all who seek a more rigorous understanding of legal history and IP theory.
 *
 * DIRECTIONALITY LOGIC:
 *   Legal historians and IP theorists are beneficiaries because this analytical framework provides clarity and precision to their work, allowing for more robust theories and interpretations. Historical legal texts are observers, as they are the data being analyzed, not active agents. There are no victims as this is a conceptual tool, not an extractive mechanism.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    empirical_independence_of_thinkability_and_holding,
    'Can historical research empirically demonstrate instances where the ''thinkability'' of an IP category clearly precedes its ''first holding,'' or vice versa, without being a mere artifact of available evidence?',
    'Detailed historical case studies of emerging IP categories (e.g., software, genetic material) that explicitly track the conceptual discourse alongside legal recognition, seeking clear temporal dissociations.',
    'If independence is robustly demonstrated, it strengthens the claim that IP''s underlying structure is not merely a historical contingency. If they always co-occur, it suggests the distinction is a conceptual artifact, not a structural feature.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(empirical_independence_of_thinkability_and_holding, empirical, 'Tests the empirical separability of conceptual emergence and legal recognition in IP history.').

omega_variable(
    conceptual_vs_temporal_framing,
    'Is the perceived independence or co-occurrence of ''thinkability'' and ''first holding'' a genuine conceptual distinction, or an artifact of how historical narratives are constructed and framed?',
    'Comparative analysis of legal historical methodologies: examining how different narrative choices (e.g., focusing on legislative intent vs. social practice) influence the perceived relationship between these two events.',
    'If primarily a framing artifact, the constraint''s ''mountain'' status is weakened, suggesting a constructed rather than natural conceptual boundary. If a genuine conceptual distinction, its analytical power is affirmed.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(conceptual_vs_temporal_framing, conceptual, 'Distinguishes between genuine conceptual independence and narrative-induced correlation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ip_category_emergence__synchronic_diachronic_seam, 1600, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ip_c_tr_t1600, ip_category_emergence__synchronic_diachronic_seam, theater_ratio, 1600, 0.05).
narrative_ontology:measurement(ip_c_tr_t1710, ip_category_emergence__synchronic_diachronic_seam, theater_ratio, 1710, 0.05).
narrative_ontology:measurement(ip_c_tr_t1850, ip_category_emergence__synchronic_diachronic_seam, theater_ratio, 1850, 0.05).
narrative_ontology:measurement(ip_c_tr_t2024, ip_category_emergence__synchronic_diachronic_seam, theater_ratio, 2024, 0.05).

% Extraction over time
narrative_ontology:measurement(ip_c_be_t1600, ip_category_emergence__synchronic_diachronic_seam, base_extractiveness, 1600, 0.2).
narrative_ontology:measurement(ip_c_be_t1710, ip_category_emergence__synchronic_diachronic_seam, base_extractiveness, 1710, 0.2).
narrative_ontology:measurement(ip_c_be_t1850, ip_category_emergence__synchronic_diachronic_seam, base_extractiveness, 1850, 0.2).
narrative_ontology:measurement(ip_c_be_t2024, ip_category_emergence__synchronic_diachronic_seam, base_extractiveness, 2024, 0.2).

% Suppression requirement over time
narrative_ontology:measurement(ip_c_su_t1600, ip_category_emergence__synchronic_diachronic_seam, suppression_requirement, 1600, 0.1).
narrative_ontology:measurement(ip_c_su_t1710, ip_category_emergence__synchronic_diachronic_seam, suppression_requirement, 1710, 0.1).
narrative_ontology:measurement(ip_c_su_t1850, ip_category_emergence__synchronic_diachronic_seam, suppression_requirement, 1850, 0.1).
narrative_ontology:measurement(ip_c_su_t2024, ip_category_emergence__synchronic_diachronic_seam, suppression_requirement, 2024, 0.1).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ip_category_emergence__synchronic_diachronic_seam, information_standard).
narrative_ontology:affects_constraint(ip_category_emergence__synchronic_diachronic_seam, ip_category_emergence__thinkability_reading).
narrative_ontology:affects_constraint(ip_category_emergence__synchronic_diachronic_seam, ip_category_emergence__first_holding_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'ip_category_emergence' kernel, focusing on the synchronic-diachronic seam between 'thinkability' and 'first holding.' It influences and is influenced by the 'thinkability_reading' and 'first_holding_reading' by providing a framework for their interrelation.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
