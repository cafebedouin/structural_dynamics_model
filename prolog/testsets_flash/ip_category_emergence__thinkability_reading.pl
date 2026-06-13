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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: ip_category_emergence__thinkability_reading
 *   human_readable: IP Category Emergence: Thinkability Reading (1710 Statute of Anne)
 *   domain: legal_philosophy/intellectual_property/historical_jurisprudence
 *
 * SUMMARY:
 *   This constraint describes the historical emergence of 'intellectual
 *   property' as a distinct, thinkable legal category, specifically marked by
 *   the 1710 Statute of Anne. This 'thinkability reading' emphasizes the
 *   conceptual space gaining a new point: before 1710, disputes over literary
 *   works were framed in terms of guild privileges or royal grants; after
 *   1710, the concept of 'copy right' as an ownable expression became legally
 *   coherent and deployable. It is a Mountain because it describes a
 *   historical fact of conceptual evolution, not an actively enforced human
 *   construct.
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
narrative_ontology:human_readable(ip_category_emergence__thinkability_reading, "IP Category Emergence: Thinkability Reading (1710 Statute of Anne)").
narrative_ontology:topic_domain(ip_category_emergence__thinkability_reading, "legal_philosophy/intellectual_property/historical_jurisprudence").

domain_priors:emerges_naturally(ip_category_emergence__thinkability_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ip_category_emergence__thinkability_reading, 'a68f14c9-5906-44ff-b979-8bff282dafa7').
narrative_ontology:cs_kernel_codification('a68f14c9-5906-44ff-b979-8bff282dafa7', fixed_text).
narrative_ontology:cs_authority_grounding('a68f14c9-5906-44ff-b979-8bff282dafa7', lineage).
narrative_ontology:cs_interpretation_layer_present('a68f14c9-5906-44ff-b979-8bff282dafa7').
narrative_ontology:cs_reading_relation('a68f14c9-5906-44ff-b979-8bff282dafa7', ip_category_emergence__first_holding_reading, coexists_with).
narrative_ontology:cs_reading_relation('a68f14c9-5906-44ff-b979-8bff282dafa7', ip_category_emergence__synchronic_diachronic_seam, coexists_with).
narrative_ontology:cs_axiom('a68f14c9-5906-44ff-b979-8bff282dafa7', foundational, conceptual_coherence_precedes_formal_right).
narrative_ontology:cs_axiom_status(conceptual_coherence_precedes_formal_right, holdable).
narrative_ontology:cs_axiom_grounding('a68f14c9-5906-44ff-b979-8bff282dafa7', conceptual_coherence_precedes_formal_right, deontological).
narrative_ontology:cs_reference_frame('a68f14c9-5906-44ff-b979-8bff282dafa7', pre_statute_conceptual_ambiguity).
narrative_ontology:cs_drift_state('a68f14c9-5906-44ff-b979-8bff282dafa7', post_statute_of_anne, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('a68f14c9-5906-44ff-b979-8bff282dafa7', '').
narrative_ontology:cs_kernel_id(ip_category_emergence__thinkability_reading, ip_category_emergence).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ip_category_emergence__thinkability_reading, legal_historians).
narrative_ontology:constraint_beneficiary(ip_category_emergence__thinkability_reading, intellectual_property_scholars).
narrative_ontology:constraint_vindicates(ip_category_emergence__thinkability_reading, conceptual_space_evolution_theory).
narrative_ontology:constraint_vindicates(ip_category_emergence__thinkability_reading, legal_category_formation_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefit from a clear historical marker for the emergence of a distinct legal category, allowing for structured analysis of legal evolution. The constraint provides a stable point of reference for their research.
narrative_ontology:constraint_stakeholder(ip_category_emergence__thinkability_reading, legal_historians, beneficiary,
    analytical, generational, analytical, global).

% Gain a foundational understanding of when 'intellectual property' became a coherent concept, distinct from other forms of privilege or ownership. This informs their theoretical frameworks and historical analyses of IP law.
narrative_ontology:constraint_stakeholder(ip_category_emergence__thinkability_reading, intellectual_property_scholars, beneficiary,
    analytical, generational, analytical, global).

% Operated in a legal landscape where 'copy right' as a distinct, ownable expression was not yet a coherent concept. Their disputes over literary works were framed in terms of guild privileges or royal grants, not inherent authorial rights. They are 'excluded' from the post-1710 conceptual space.
narrative_ontology:constraint_stakeholder(ip_category_emergence__thinkability_reading, pre_1710_authors_and_printers, excluded,
    powerless, biographical, trapped, local).

% While operating under modern IP law, they observe the historical emergence of the category as a background to current doctrines. The constraint itself does not directly govern their practice, but its historical reality shapes the legal system they inhabit.
narrative_ontology:constraint_stakeholder(ip_category_emergence__thinkability_reading, contemporary_legal_practitioners, observer,
    institutional, immediate, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared conceptual framework for understanding the historical shift in legal thought regarding intellectual creations, allowing scholars to coordinate their research and arguments around a common historical reference point.
% TRANSFER_FUNCTION: Transfers conceptual clarity and analytical structure to the field of legal history and IP theory, enabling more precise discussions about the evolution of rights over expression.
% ABSENT_VOICES: The 'voices' of pre-1710 legal actors are absent from the post-1710 conceptual space of 'copy right' as a distinct category. They would not have understood the concept in its modern form, as their legal language and frameworks were different.
% DISAPPEARANCE_RATIONALE: The historical fact of the conceptual emergence of IP in 1710 is a settled historical and philosophical observation. Its 'disappearance' would mean a fundamental re-writing of legal history, which is not possible; the world of legal scholarship would remain unchanged in its understanding of this past event.
% FOUNDING_PROBLEM: The problem was the lack of a distinct legal category and vocabulary to articulate rights over intellectual creations, leading to conceptual ambiguity and reliance on guild privileges or royal grants for protection.
% FOUNDING_PROBLEM_CORROBORATION: Legal historians and philosophers universally corroborate that the conceptual problem of 'thinkability' for IP was resolved by the Statute of Anne in 1710, which introduced the concept of 'copy right' as a distinct legal category. This is a widely accepted historical fact, not contested by any benefiting parties.
narrative_ontology:disappearance_verdict(ip_category_emergence__thinkability_reading, world_unchanged).
narrative_ontology:founding_problem_status(ip_category_emergence__thinkability_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ip_category_emergence__thinkability_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(ip_category_emergence__thinkability_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ip_category_emergence__thinkability_reading_tests).

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
 *   The constraint's metrics reflect its nature as a historical observation: extractiveness, suppression, and theater ratio are all very low because it's a description of a past conceptual shift, not an active mechanism. Accessibility collapse is high (0.95) because once the conceptual shift is understood, there's no 'alternative' way to frame the pre-1710/post-1710 distinction in this specific 'thinkability' sense. Resistance is low (0.01) because the historical fact of this conceptual emergence is not contested.
 *
 * PERSPECTIVAL GAP:
 *   There is no significant perspectival gap for this constraint, as it describes a widely accepted historical conceptual shift. The 'thinkability' is a shared analytical observation.
 *
 * DIRECTIONALITY LOGIC:
 *   Legal historians and IP scholars are beneficiaries (d near 0.0) because the constraint provides a clear, stable conceptual anchor for their work. Pre-1710 actors are 'excluded' from the post-1710 conceptual space, but not 'victims' of this specific constraint, which is about the conceptual shift itself. Contemporary practitioners are observers.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    conceptual_vs_material_emergence,
    'Is the 1710 Statute of Anne primarily a marker of conceptual emergence (thinkability) or of material legal change (first holding of rights)?',
    'Analysis of pre-1710 legal texts for implicit conceptual structures vs. explicit post-1710 legal claims and enforcement actions. If pre-1710 texts show implicit ''copy right'' concepts, the ''thinkability'' reading is weakened.',
    'If primarily material, this constraint''s ''mountain'' classification would be challenged, potentially shifting towards a ''rope'' or ''tangled_rope'' for the ''first_holding_reading'' sibling, as it would imply an active legal construction rather than a conceptual discovery.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(conceptual_vs_material_emergence, conceptual, 'Distinguishing conceptual shift from material legal enactment.').

omega_variable(
    synchronic_diachronic_relationship,
    'Is the ''thinkability'' reading truly distinct from the ''first_holding_reading'', or are they two facets of a single, complex historical event, making their separation an analytical artifact?',
    'Further philosophical and historical analysis to determine if the conceptual and material shifts are causally independent or inextricably linked. This relates to the ''synchronic_diachronic_seam'' sibling reading.',
    'If inextricably linked, the ''thinkability_reading'' might be subsumed into a broader ''category_formation'' constraint, potentially altering its classification if the broader constraint involves more active human construction and enforcement.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(synchronic_diachronic_relationship, conceptual, 'Relationship between conceptual and material aspects of IP emergence.').


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
narrative_ontology:measurement(ip_c_be_t1700, ip_category_emergence__thinkability_reading, base_extractiveness, 1700, 0.05).
narrative_ontology:measurement(ip_c_be_t1710, ip_category_emergence__thinkability_reading, base_extractiveness, 1710, 0.05).
narrative_ontology:measurement(ip_c_be_t1720, ip_category_emergence__thinkability_reading, base_extractiveness, 1720, 0.05).

% Suppression requirement over time
narrative_ontology:measurement(ip_c_su_t1700, ip_category_emergence__thinkability_reading, suppression_requirement, 1700, 0.02).
narrative_ontology:measurement(ip_c_su_t1710, ip_category_emergence__thinkability_reading, suppression_requirement, 1710, 0.02).
narrative_ontology:measurement(ip_c_su_t1720, ip_category_emergence__thinkability_reading, suppression_requirement, 1720, 0.02).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
