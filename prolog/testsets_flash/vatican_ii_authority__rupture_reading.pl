% ============================================================================
% CONSTRAINT STORY: vatican_ii_authority__rupture_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_vatican_ii_authority__rupture_reading, []).

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
 *   constraint_id: vatican_ii_authority__rupture_reading
 *   human_readable: Vatican II as Rupture with Tradition
 *   domain: theology/ecclesiology/religious_authority
 *
 * SUMMARY:
 *   This constraint story instantiates the 'rupture' reading of Vatican II,
 *   where the Council's documents are seen as containing doctrinal errors or
 *   irreconcilable contradictions with prior Catholic teaching. This reading
 *   asserts that Vatican II represents a substantive break with tradition,
 *   leading to a crisis in the post-conciliar Church. The SSPX position is a
 *   key instantiation of this reading. The constraint is highly extractive,
 *   as it demands a fundamental reorientation of traditional Catholic
 *   identity and practice, enforced by institutional pressure and the
 *   marginalization of dissenters.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vatican_ii_authority__rupture_reading, 0.85).
domain_priors:suppression_score(vatican_ii_authority__rupture_reading, 0.7).
domain_priors:theater_ratio(vatican_ii_authority__rupture_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vatican_ii_authority__rupture_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(vatican_ii_authority__rupture_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(vatican_ii_authority__rupture_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vatican_ii_authority__rupture_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(vatican_ii_authority__rupture_reading, resistance, 0.8).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vatican_ii_authority__rupture_reading, snare).
narrative_ontology:human_readable(vatican_ii_authority__rupture_reading, "Vatican II as Rupture with Tradition").
narrative_ontology:topic_domain(vatican_ii_authority__rupture_reading, "theology/ecclesiology/religious_authority").

domain_priors:requires_active_enforcement(vatican_ii_authority__rupture_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vatican_ii_authority__rupture_reading, 'cd3197b2-8a60-4429-adf8-c2f40d6019c1').
narrative_ontology:cs_kernel_codification('cd3197b2-8a60-4429-adf8-c2f40d6019c1', fixed_text).
narrative_ontology:cs_authority_grounding('cd3197b2-8a60-4429-adf8-c2f40d6019c1', lineage).
narrative_ontology:cs_interpretation_layer_present('cd3197b2-8a60-4429-adf8-c2f40d6019c1').
narrative_ontology:cs_reading_relation('cd3197b2-8a60-4429-adf8-c2f40d6019c1', vatican_ii_authority__continuity_reading, forecloses).
narrative_ontology:cs_reading_relation('cd3197b2-8a60-4429-adf8-c2f40d6019c1', vatican_ii_authority__composite_overdetermination_reading, coexists_with).
narrative_ontology:cs_axiom('cd3197b2-8a60-4429-adf8-c2f40d6019c1', foundational, doctrinal_infallibility_of_prior_magisterium).
narrative_ontology:cs_axiom_status(doctrinal_infallibility_of_prior_magisterium, holdable).
narrative_ontology:cs_axiom_grounding('cd3197b2-8a60-4429-adf8-c2f40d6019c1', doctrinal_infallibility_of_prior_magisterium, deontological).
narrative_ontology:cs_axiom('cd3197b2-8a60-4429-adf8-c2f40d6019c1', foundational, vatican_ii_documents_contain_error).
narrative_ontology:cs_axiom_status(vatican_ii_documents_contain_error, holdable).
narrative_ontology:cs_axiom_grounding('cd3197b2-8a60-4429-adf8-c2f40d6019c1', vatican_ii_documents_contain_error, empirically_contingent).
narrative_ontology:cs_reference_frame('cd3197b2-8a60-4429-adf8-c2f40d6019c1', pre_conciliar_doctrinal_unity).
narrative_ontology:cs_drift_state('cd3197b2-8a60-4429-adf8-c2f40d6019c1', post_conciliar_era, gap(axiom_overriding, severe, false)).
narrative_ontology:cs_created_at('cd3197b2-8a60-4429-adf8-c2f40d6019c1', '').
narrative_ontology:cs_kernel_id(vatican_ii_authority__rupture_reading, vatican_ii_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vatican_ii_authority__rupture_reading, modernist_faction).
narrative_ontology:constraint_beneficiary(vatican_ii_authority__rupture_reading, liberal_theologians).
narrative_ontology:constraint_victim(vatican_ii_authority__rupture_reading, traditional_catholics).
narrative_ontology:constraint_victim(vatican_ii_authority__rupture_reading, doctrinal_stability).
narrative_ontology:constraint_victim(vatican_ii_authority__rupture_reading, traditional_clergy).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vatican_ii_authority__rupture_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(vatican_ii_authority__rupture_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(vatican_ii_authority__rupture_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(vatican_ii_authority__rupture_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(vatican_ii_authority__rupture_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.85) because this reading implies a profound loss of doctrinal certainty and traditional identity for those who adhere to pre-conciliar Catholicism. Suppression (0.7) is significant, as traditionalists are often marginalized or disciplined for rejecting the Council's perceived innovations. Theater ratio (0.4) reflects that while some efforts are made to present the Council as continuous, the actual impact and interpretation, from this perspective, are often performative attempts to mask a deeper break. Resistance is high (0.8) due to ongoing, organized opposition from traditionalist groups.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the 'modernist faction' (beneficiaries), Vatican II is a legitimate and necessary development, and the 'rupture' is a positive evolution. From the 'traditional Catholics' (victims), it is a catastrophic betrayal. The engine will compute these divergent classifications based on their structural positions.
 *
 * DIRECTIONALITY LOGIC:
 *   The 'modernist faction' and 'liberal theologians' are beneficiaries (low d) as they gain influence and justification for their views. 'Traditional Catholics' and 'doctrinal stability' are victims (high d) as they bear the costs of doctrinal confusion and alienation. 'Traditional clergy' are payers (high d) facing career and conscience costs. SSPX adherents, while promoting this reading, also bear costs of separation, but their active role in maintaining an alternative structure gives them an 'agenda_setter' role within their own sphere.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is a snare because its coordination story (updating the Church for the modern world) is, from this reading's perspective, a cover for a profound extraction of traditional identity and doctrinal stability. The persistence of the constraint relies on suppressing traditionalist dissent and enforcing the new theological paradigm. The 'founding problem' of Church relevance is contested, suggesting the constraint's original mandate has been superseded by an extractive function.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    doctrinal_contradiction_objectivity,
    'Are the alleged doctrinal errors and contradictions in Vatican II documents objectively irreconcilable with prior teaching, or are they matters of theological interpretation?',
    'A definitive, universally accepted magisterial pronouncement clarifying the disputed points, or a consensus among theologians across the spectrum that such contradictions are either real or merely apparent.',
    'If objectively irreconcilable, the Council''s authority is fundamentally undermined, strengthening the snare classification. If reconcilable through interpretation, the extractiveness might be re-evaluated as a ''tangled rope'' of contested interpretation rather than outright rupture.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(doctrinal_contradiction_objectivity, conceptual, 'The objective status of alleged doctrinal contradictions.').

omega_variable(
    identity_fusion_traditionalists,
    'To what extent is the ''identity_locked'' exit option for traditional Catholics a result of genuine theological conviction versus social/communal identity fusion within traditionalist enclaves?',
    'Longitudinal studies of individuals who leave traditionalist communities: if their theological convictions persist but their social identity shifts, it suggests a stronger social fusion component.',
    'If identity fusion is a dominant mechanism, the effective suppression and extractiveness are higher, as exit costs are amplified by the loss of self-concept and community, making the snare more potent.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_fusion_traditionalists, empirical, 'Structural vs. internalized suppression mechanism for traditional Catholics.').

omega_variable(
    kernel_framing_underdetermination,
    'Is the ''rupture'' framing the most accurate lens for understanding Vatican II, or does the ''composite_overdetermination'' reading better capture the structural ambiguity and multiple, incompatible shifts?',
    'A comprehensive historical and theological analysis that accounts for all major doctrinal and pastoral shifts, demonstrating whether they cohere into a single ''rupture'' or remain as an unresolvable composite.',
    'If the composite reading is adopted, the constraint might be re-framed as a ''tangled rope'' of competing interpretations rather than a clear ''snare'' of rupture, shifting the focus from intentional error to structural ambiguity and its extractive consequences.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_framing_underdetermination, conceptual, 'Alternative framings of the Vatican II kernel and their classification impact.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vatican_ii_authority__rupture_reading, 1962, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vati_tr_t1962, vatican_ii_authority__rupture_reading, theater_ratio, 1962, 0.1).
narrative_ontology:measurement(vati_tr_t1975, vatican_ii_authority__rupture_reading, theater_ratio, 1975, 0.25).
narrative_ontology:measurement(vati_tr_t1990, vatican_ii_authority__rupture_reading, theater_ratio, 1990, 0.35).
narrative_ontology:measurement(vati_tr_t2005, vatican_ii_authority__rupture_reading, theater_ratio, 2005, 0.4).
narrative_ontology:measurement(vati_tr_t2024, vatican_ii_authority__rupture_reading, theater_ratio, 2024, 0.4).

% Extraction over time
narrative_ontology:measurement(vati_be_t1962, vatican_ii_authority__rupture_reading, base_extractiveness, 1962, 0.6).
narrative_ontology:measurement(vati_be_t1975, vatican_ii_authority__rupture_reading, base_extractiveness, 1975, 0.75).
narrative_ontology:measurement(vati_be_t1990, vatican_ii_authority__rupture_reading, base_extractiveness, 1990, 0.82).
narrative_ontology:measurement(vati_be_t2005, vatican_ii_authority__rupture_reading, base_extractiveness, 2005, 0.85).
narrative_ontology:measurement(vati_be_t2024, vatican_ii_authority__rupture_reading, base_extractiveness, 2024, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(vati_su_t1962, vatican_ii_authority__rupture_reading, suppression_requirement, 1962, 0.4).
narrative_ontology:measurement(vati_su_t1975, vatican_ii_authority__rupture_reading, suppression_requirement, 1975, 0.6).
narrative_ontology:measurement(vati_su_t1990, vatican_ii_authority__rupture_reading, suppression_requirement, 1990, 0.7).
narrative_ontology:measurement(vati_su_t2005, vatican_ii_authority__rupture_reading, suppression_requirement, 2005, 0.7).
narrative_ontology:measurement(vati_su_t2024, vatican_ii_authority__rupture_reading, suppression_requirement, 2024, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vatican_ii_authority__rupture_reading, identity_coordination).
narrative_ontology:affects_constraint(vatican_ii_authority__rupture_reading, vatican_ii_authority__continuity_reading).
narrative_ontology:affects_constraint(vatican_ii_authority__rupture_reading, vatican_ii_authority__composite_overdetermination_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'vatican_ii_authority' kernel. This 'rupture_reading' focuses on the perceived doctrinal breaks and contradictions, leading to a snare classification. The 'continuity_reading' (a rope or scaffold) and 'composite_overdetermination_reading' (a tangled rope) offer alternative interpretations with different structural implications.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
