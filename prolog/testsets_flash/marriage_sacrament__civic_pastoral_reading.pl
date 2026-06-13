% ============================================================================
% CONSTRAINT STORY: marriage_sacrament__civic_pastoral_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_marriage_sacrament__civic_pastoral_reading, []).

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
 *   constraint_id: marriage_sacrament__civic_pastoral_reading
 *   human_readable: Marriage as Pastoral Relationship (Civic-Pastoral Reading)
 *   domain: religious_doctrine/canon_law/political_sociology
 *
 * SUMMARY:
 *   This constraint describes the 'civic-pastoral' reading of the Catholic
 *   Church's marriage sacrament, where indissolubility is treated as an ideal
 *   requiring compassionate discernment in individual cases, rather than an
 *   absolute, ontologically fixed reality. This reading emerged as a response
 *   to the perceived rigidity of traditional doctrine and aims to provide
 *   pastoral care to those in complex marital situations. It is one reading
 *   of the 'marriage_sacrament' kernel, distinct from the
 *   'hierarchical_indissolubility_reading'.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(marriage_sacrament__civic_pastoral_reading, 0.45).
domain_priors:suppression_score(marriage_sacrament__civic_pastoral_reading, 0.3).
domain_priors:theater_ratio(marriage_sacrament__civic_pastoral_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(marriage_sacrament__civic_pastoral_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(marriage_sacrament__civic_pastoral_reading, suppression_requirement, 0.3).
narrative_ontology:constraint_metric(marriage_sacrament__civic_pastoral_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(marriage_sacrament__civic_pastoral_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(marriage_sacrament__civic_pastoral_reading, resistance, 0.25).

% --- Constraint claim ---
narrative_ontology:constraint_claim(marriage_sacrament__civic_pastoral_reading, tangled_rope).
narrative_ontology:human_readable(marriage_sacrament__civic_pastoral_reading, "Marriage as Pastoral Relationship (Civic-Pastoral Reading)").
narrative_ontology:topic_domain(marriage_sacrament__civic_pastoral_reading, "religious_doctrine/canon_law/political_sociology").

domain_priors:requires_active_enforcement(marriage_sacrament__civic_pastoral_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(marriage_sacrament__civic_pastoral_reading, 'c698e994-c818-4c0c-87fe-e4e164ee8b43').
narrative_ontology:cs_kernel_codification('c698e994-c818-4c0c-87fe-e4e164ee8b43', formalized).
narrative_ontology:cs_authority_grounding('c698e994-c818-4c0c-87fe-e4e164ee8b43', lineage).
narrative_ontology:cs_interpretation_layer_present('c698e994-c818-4c0c-87fe-e4e164ee8b43').
narrative_ontology:cs_reading_relation('c698e994-c818-4c0c-87fe-e4e164ee8b43', marriage_sacrament__hierarchical_indissolubility_reading, coexists_with).
narrative_ontology:cs_axiom('c698e994-c818-4c0c-87fe-e4e164ee8b43', foundational, indissolubility_as_pastoral_ideal).
narrative_ontology:cs_axiom_status(indissolubility_as_pastoral_ideal, holdable).
narrative_ontology:cs_axiom_grounding('c698e994-c818-4c0c-87fe-e4e164ee8b43', indissolubility_as_pastoral_ideal, deontological).
narrative_ontology:cs_axiom('c698e994-c818-4c0c-87fe-e4e164ee8b43', foundational, primacy_of_pastoral_discernment).
narrative_ontology:cs_axiom_status(primacy_of_pastoral_discernment, holdable).
narrative_ontology:cs_axiom_grounding('c698e994-c818-4c0c-87fe-e4e164ee8b43', primacy_of_pastoral_discernment, conventional).
narrative_ontology:cs_reference_frame('c698e994-c818-4c0c-87fe-e4e164ee8b43', pastoral_accompaniment_framework).
narrative_ontology:cs_drift_state('c698e994-c818-4c0c-87fe-e4e164ee8b43', contemporary_synodal_process, gap(stable, minor, true)).
narrative_ontology:cs_created_at('c698e994-c818-4c0c-87fe-e4e164ee8b43', '').
narrative_ontology:cs_kernel_id(marriage_sacrament__civic_pastoral_reading, marriage_sacrament).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(marriage_sacrament__civic_pastoral_reading, pastoral_clergy).
narrative_ontology:constraint_beneficiary(marriage_sacrament__civic_pastoral_reading, laity_seeking_discernment).
narrative_ontology:constraint_victim(marriage_sacrament__civic_pastoral_reading, traditional_catholics).
narrative_ontology:constraint_victim(marriage_sacrament__civic_pastoral_reading, doctrinal_conservatives).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(marriage_sacrament__civic_pastoral_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(marriage_sacrament__civic_pastoral_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(marriage_sacrament__civic_pastoral_reading_tests).
:- end_tests(marriage_sacrament__civic_pastoral_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.45) is moderate, reflecting the cost borne by traditional Catholics who experience doctrinal relativization and a loss of normative clarity. Suppression (0.30) is relatively low, as this reading seeks to reduce the coercive pressure on individuals, but still requires active enforcement to manage internal dissent and maintain a semblance of doctrinal consistency. The theater ratio (0.20) is present because the nominal ideal of indissolubility is maintained, even as pastoral practice introduces significant flexibility, creating a gap between stated doctrine and lived experience. The declining suppression_requirement over time reflects a shift away from strict enforcement towards a more pastoral approach.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of pastoral clergy and laity seeking discernment, this reading functions as a compassionate rope, offering support and pathways for reconciliation. From the perspective of traditional Catholics, it operates as a tangled rope or even a snare, extracting doctrinal certainty and imposing ambiguity, while benefiting those who seek to circumvent traditional norms. The engine's per-seat classification will capture this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Pastoral clergy and laity seeking discernment are beneficiaries (d near 0.0-0.2), gaining flexibility and compassion. Traditional Catholics and doctrinal conservatives are victims (d near 0.8-1.0), experiencing a loss of doctrinal stability and clarity. The magisterial authority, while nominally upholding indissolubility, benefits from maintaining institutional relevance and unity, but also bears the cost of internal dissent, placing its d closer to symmetric (0.4-0.6).
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    doctrinal_coherence_vs_pastoral_flexibility,
    'Can the ideal of indissolubility be genuinely upheld while simultaneously allowing for extensive pastoral discernment in individual cases, without undermining the doctrine''s coherence?',
    'Long-term theological analysis of the internal consistency of magisterial documents and the lived experience of the faithful. If the two readings prove irreconcilable, a formal doctrinal clarification would be required.',
    'If irreconcilable, this reading would be reclassified as a snare for traditionalists, as the coordination story (upholding indissolubility) would be revealed as cover for a de facto change in doctrine. If reconcilable, its rope-like qualities would be strengthened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(doctrinal_coherence_vs_pastoral_flexibility, conceptual, 'The internal consistency of the civic-pastoral reading with core doctrine.').

omega_variable(
    mandatrophy_of_indissolubility,
    'Has the original mandate of indissolubility (as an absolute, ontological reality) atrophied, and is the ''pastoral discernment'' framework a new mandate layered over a vestigial structure?',
    'Historical-theological analysis of the evolution of marriage doctrine and pastoral practice, comparing the stated justifications for indissolubility across different eras with contemporary applications.',
    'If the original mandate is found to be dead, and the new pastoral approach is merely a way to maintain institutional relevance without genuine doctrinal change, the constraint would lean more towards a piton or snare for traditionalists, as the ''ideal'' becomes theatrical.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mandatrophy_of_indissolubility, empirical, 'Whether the original mandate of indissolubility has atrophied.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(marriage_sacrament__civic_pastoral_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(marr_tr_t0, marriage_sacrament__civic_pastoral_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(marr_tr_t5, marriage_sacrament__civic_pastoral_reading, theater_ratio, 5, 0.15).
narrative_ontology:measurement(marr_tr_t10, marriage_sacrament__civic_pastoral_reading, theater_ratio, 10, 0.18).
narrative_ontology:measurement(marr_tr_t15, marriage_sacrament__civic_pastoral_reading, theater_ratio, 15, 0.2).
narrative_ontology:measurement(marr_tr_t20, marriage_sacrament__civic_pastoral_reading, theater_ratio, 20, 0.2).

% Extraction over time
narrative_ontology:measurement(marr_be_t0, marriage_sacrament__civic_pastoral_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(marr_be_t5, marriage_sacrament__civic_pastoral_reading, base_extractiveness, 5, 0.35).
narrative_ontology:measurement(marr_be_t10, marriage_sacrament__civic_pastoral_reading, base_extractiveness, 10, 0.4).
narrative_ontology:measurement(marr_be_t15, marriage_sacrament__civic_pastoral_reading, base_extractiveness, 15, 0.43).
narrative_ontology:measurement(marr_be_t20, marriage_sacrament__civic_pastoral_reading, base_extractiveness, 20, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(marr_su_t0, marriage_sacrament__civic_pastoral_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(marr_su_t5, marriage_sacrament__civic_pastoral_reading, suppression_requirement, 5, 0.35).
narrative_ontology:measurement(marr_su_t10, marriage_sacrament__civic_pastoral_reading, suppression_requirement, 10, 0.32).
narrative_ontology:measurement(marr_su_t15, marriage_sacrament__civic_pastoral_reading, suppression_requirement, 15, 0.3).
narrative_ontology:measurement(marr_su_t20, marriage_sacrament__civic_pastoral_reading, suppression_requirement, 20, 0.3).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(marriage_sacrament__civic_pastoral_reading, attachment_coordination).
narrative_ontology:affects_constraint(marriage_sacrament__civic_pastoral_reading, marriage_sacrament__hierarchical_indissolubility_reading).

% DUAL FORMULATION NOTE:
% This constraint is the 'civic_pastoral_reading' of the 'marriage_sacrament' kernel, which also includes the 'hierarchical_indissolubility_reading'. The two readings represent different interpretations of the same core doctrine, with distinct structural implications for stakeholders.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
