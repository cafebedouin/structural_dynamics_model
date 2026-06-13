% ============================================================================
% CONSTRAINT STORY: family_law_authority__parsi_zoroastrian_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_family_law_authority__parsi_zoroastrian_reading, []).

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
 *   constraint_id: family_law_authority__parsi_zoroastrian_reading
 *   human_readable: Parsi Zoroastrian Marriage Law
 *   domain: comparative_law/religious_governance
 *
 * SUMMARY:
 *   This constraint describes the Parsi Zoroastrian community's marriage
 *   laws, which are primarily designed to preserve the community's distinct
 *   ethno-religious identity through endogamy. It is a reading of the broader
 *   'family_law_authority' kernel, emphasizing community preservation and
 *   priestly authority. The constraint operates as a Tangled Rope, providing
 *   coordination for community survival while extracting social and religious
 *   status from individuals who deviate from endogamous norms.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(family_law_authority__parsi_zoroastrian_reading, 0.4).
domain_priors:suppression_score(family_law_authority__parsi_zoroastrian_reading, 0.6).
domain_priors:theater_ratio(family_law_authority__parsi_zoroastrian_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(family_law_authority__parsi_zoroastrian_reading, extractiveness, 0.4).
narrative_ontology:constraint_metric(family_law_authority__parsi_zoroastrian_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(family_law_authority__parsi_zoroastrian_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(family_law_authority__parsi_zoroastrian_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(family_law_authority__parsi_zoroastrian_reading, resistance, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(family_law_authority__parsi_zoroastrian_reading, tangled_rope).
narrative_ontology:human_readable(family_law_authority__parsi_zoroastrian_reading, "Parsi Zoroastrian Marriage Law").
narrative_ontology:topic_domain(family_law_authority__parsi_zoroastrian_reading, "comparative_law/religious_governance").

domain_priors:requires_active_enforcement(family_law_authority__parsi_zoroastrian_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(family_law_authority__parsi_zoroastrian_reading, 'a6631180-7e17-463b-9c1e-09f66cd41eec').
narrative_ontology:cs_kernel_codification('a6631180-7e17-463b-9c1e-09f66cd41eec', formalized).
narrative_ontology:cs_authority_grounding('a6631180-7e17-463b-9c1e-09f66cd41eec', lineage).
narrative_ontology:cs_interpretation_layer_present('a6631180-7e17-463b-9c1e-09f66cd41eec').
narrative_ontology:cs_reading_relation('a6631180-7e17-463b-9c1e-09f66cd41eec', family_law_authority__christian_canonical_reading, coexists_with).
narrative_ontology:cs_reading_relation('a6631180-7e17-463b-9c1e-09f66cd41eec', family_law_authority__hindu_dharmashastra_reading, coexists_with).
narrative_ontology:cs_reading_relation('a6631180-7e17-463b-9c1e-09f66cd41eec', family_law_authority__muslim_shariat_reading, coexists_with).
narrative_ontology:cs_reading_relation('a6631180-7e17-463b-9c1e-09f66cd41eec', family_law_authority__secular_contractual_reading, coexists_with).
narrative_ontology:cs_axiom('a6631180-7e17-463b-9c1e-09f66cd41eec', foundational, endogamy_preserves_community_identity).
narrative_ontology:cs_axiom_status(endogamy_preserves_community_identity, holdable).
narrative_ontology:cs_axiom_grounding('a6631180-7e17-463b-9c1e-09f66cd41eec', endogamy_preserves_community_identity, empirically_contingent).
narrative_ontology:cs_axiom('a6631180-7e17-463b-9c1e-09f66cd41eec', foundational, priestly_authority_defines_ritual_validity).
narrative_ontology:cs_axiom_status(priestly_authority_defines_ritual_validity, holdable).
narrative_ontology:cs_axiom_grounding('a6631180-7e17-463b-9c1e-09f66cd41eec', priestly_authority_defines_ritual_validity, conventional).
narrative_ontology:cs_reference_frame('a6631180-7e17-463b-9c1e-09f66cd41eec', traditional_zoroastrian_community_norms).
narrative_ontology:cs_drift_state('a6631180-7e17-463b-9c1e-09f66cd41eec', contemporary_globalized_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('a6631180-7e17-463b-9c1e-09f66cd41eec', '').
narrative_ontology:cs_kernel_id(family_law_authority__parsi_zoroastrian_reading, family_law_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(family_law_authority__parsi_zoroastrian_reading, parsi_zoroastrian_community).
narrative_ontology:constraint_beneficiary(family_law_authority__parsi_zoroastrian_reading, zoroastrian_priesthood).
narrative_ontology:constraint_victim(family_law_authority__parsi_zoroastrian_reading, parsi_zoroastrian_individuals_seeking_intermarriage).
narrative_ontology:constraint_victim(family_law_authority__parsi_zoroastrian_reading, children_of_interfaith_marriages).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(family_law_authority__parsi_zoroastrian_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(family_law_authority__parsi_zoroastrian_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(family_law_authority__parsi_zoroastrian_reading_tests).
:- end_tests(family_law_authority__parsi_zoroastrian_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.4) as it primarily extracts social and religious status rather than direct economic value, but this is significant within the community. Suppression is higher (0.6) due to strong social pressure and the priesthood's authority in denying religious recognition for interfaith marriages and their offspring. Theater ratio is low (0.1) because the community preservation function is genuinely active and the rules are consistently enforced, not merely performed. Accessibility collapse is high (0.7) because for individuals seeking full community membership, there are few recognized alternatives to endogamous marriage.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the Parsi Zoroastrian community and priesthood, the constraint is a necessary mechanism for cultural and religious survival (beneficiary seat). From the perspective of individuals seeking intermarriage, it is a restrictive and exclusionary force that imposes significant personal costs (payer/victim seats). The engine's per-seat classification will reflect this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   The Parsi Zoroastrian community and priesthood are beneficiaries (d near 0.0) as they gain from the preservation of their identity and the maintenance of their authority. Individuals seeking intermarriage and their children are victims/payers (d near 1.0) as they bear the costs of social exclusion and denial of religious status. Secular legal systems are observers, not directly affected by the internal religious rules.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    endogamy_necessity_for_survival,
    'Is strict endogamy truly necessary for the long-term survival of the Parsi Zoroastrian community, or are there alternative, more inclusive strategies for identity preservation?',
    'Comparative sociological studies of other small diaspora communities that have adopted more inclusive marriage practices while maintaining cultural identity, or internal community debates leading to policy changes.',
    'If endogamy is found not to be strictly necessary, the ''community preservation'' justification for the constraint''s extractiveness would weaken, potentially reclassifying it closer to a Snare for individuals.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(endogamy_necessity_for_survival, empirical, 'The empirical necessity of endogamy for community survival.').

omega_variable(
    priestly_authority_legitimacy,
    'To what extent is the priesthood''s authority in defining community membership and marriage validity accepted by all segments of the Parsi Zoroastrian community, particularly younger generations and those in diaspora?',
    'Surveys of community members, analysis of internal dissent and reform movements, and observation of adherence to priestly rulings in practice.',
    'If priestly authority is widely contested, the constraint''s effective suppression might be lower than measured, and its persistence would rely more on social inertia than active enforcement, pushing it towards a Piton or a more contested Tangled Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(priestly_authority_legitimacy, empirical, 'The internal legitimacy of priestly authority over marriage.').

omega_variable(
    framing_under_determination_community_vs_individual,
    'Is the primary framing of marriage as a community-preserving institution (Parsi Zoroastrian reading) or as an individual contractual right (secular contractual reading) the more appropriate lens for analysis?',
    'This is a conceptual omega. Resolution depends on the normative priorities of the observer: prioritizing collective identity vs. individual autonomy. No empirical resolution.',
    'If the individual contractual framing were adopted, the constraint would be reclassified as a Snare, as its coordination function (community preservation) would be seen as a cover for extraction from individuals'' autonomy.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(framing_under_determination_community_vs_individual, conceptual, 'Conceptual choice between community-centric and individual-centric framings of marriage.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(family_law_authority__parsi_zoroastrian_reading, 1900, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fami_tr_t1900, family_law_authority__parsi_zoroastrian_reading, theater_ratio, 1900, 0.05).
narrative_ontology:measurement(fami_tr_t1950, family_law_authority__parsi_zoroastrian_reading, theater_ratio, 1950, 0.08).
narrative_ontology:measurement(fami_tr_t2000, family_law_authority__parsi_zoroastrian_reading, theater_ratio, 2000, 0.09).
narrative_ontology:measurement(fami_tr_t2024, family_law_authority__parsi_zoroastrian_reading, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(fami_be_t1900, family_law_authority__parsi_zoroastrian_reading, base_extractiveness, 1900, 0.3).
narrative_ontology:measurement(fami_be_t1950, family_law_authority__parsi_zoroastrian_reading, base_extractiveness, 1950, 0.35).
narrative_ontology:measurement(fami_be_t2000, family_law_authority__parsi_zoroastrian_reading, base_extractiveness, 2000, 0.38).
narrative_ontology:measurement(fami_be_t2024, family_law_authority__parsi_zoroastrian_reading, base_extractiveness, 2024, 0.4).

% Suppression requirement over time
narrative_ontology:measurement(fami_su_t1900, family_law_authority__parsi_zoroastrian_reading, suppression_requirement, 1900, 0.5).
narrative_ontology:measurement(fami_su_t1950, family_law_authority__parsi_zoroastrian_reading, suppression_requirement, 1950, 0.55).
narrative_ontology:measurement(fami_su_t2000, family_law_authority__parsi_zoroastrian_reading, suppression_requirement, 2000, 0.58).
narrative_ontology:measurement(fami_su_t2024, family_law_authority__parsi_zoroastrian_reading, suppression_requirement, 2024, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(family_law_authority__parsi_zoroastrian_reading, identity_coordination).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'family_law_authority' kernel, focusing on the Parsi Zoroastrian community's specific interpretation of marriage law for identity preservation. It is structurally distinct from other religious and secular readings of family law.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
