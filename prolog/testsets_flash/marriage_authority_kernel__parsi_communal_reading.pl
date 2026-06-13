% ============================================================================
% CONSTRAINT STORY: marriage_authority_kernel__parsi_communal_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_marriage_authority_kernel__parsi_communal_reading, []).

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
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
 *   constraint_id: marriage_authority_kernel__parsi_communal_reading
 *   human_readable: Parsi Marriage and Divorce Act 1936 (Communal Reading)
 *   domain: comparative_law/constitutional_pluralism/religious_governance
 *
 * SUMMARY:
 *   This constraint describes the authority of marriage and family law within
 *   the Parsi community in India, as derived from their customs and codified
 *   by the Parsi Marriage and Divorce Act 1936. It is one reading of the
 *   broader 'marriage_authority_kernel' in India, which encompasses various
 *   religious and secular legal frameworks. This reading emphasizes communal
 *   self-governance, endogamy for community preservation, and historically
 *   high gender equity within its specific legal framework, though
 *   demographic decline poses challenges to its long-term viability.
 *
 * KEY AGENTS:
 *   - parsi_community_elders: Agenda setter (institutional/generational) — administer the Act and customary law.
 *   - parsi_individuals: Payer/Beneficiary (moderate/biographical) — adhere to the Act for marriage, benefit from community identity, but face restrictions like endogamy.
 *   - indian_civil_courts: Observer (institutional/generational) — interpret the Act in cases, ensuring it aligns with constitutional principles.
 *   - secular_marriage_seekers: Excluded (powerless/biographical) — Parsi individuals who wish to marry outside the community or the Act's provisions, finding their choices constrained.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(marriage_authority_kernel__parsi_communal_reading, 0.3).
domain_priors:suppression_score(marriage_authority_kernel__parsi_communal_reading, 0.4).
domain_priors:theater_ratio(marriage_authority_kernel__parsi_communal_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(marriage_authority_kernel__parsi_communal_reading, extractiveness, 0.3).
narrative_ontology:constraint_metric(marriage_authority_kernel__parsi_communal_reading, suppression_requirement, 0.4).
narrative_ontology:constraint_metric(marriage_authority_kernel__parsi_communal_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(marriage_authority_kernel__parsi_communal_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(marriage_authority_kernel__parsi_communal_reading, resistance, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(marriage_authority_kernel__parsi_communal_reading, rope).
narrative_ontology:human_readable(marriage_authority_kernel__parsi_communal_reading, "Parsi Marriage and Divorce Act 1936 (Communal Reading)").
narrative_ontology:topic_domain(marriage_authority_kernel__parsi_communal_reading, "comparative_law/constitutional_pluralism/religious_governance").

domain_priors:requires_active_enforcement(marriage_authority_kernel__parsi_communal_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(marriage_authority_kernel__parsi_communal_reading, 'd9c3893a-ebb8-4f2c-a729-83047d3528e6').
narrative_ontology:cs_kernel_codification('d9c3893a-ebb8-4f2c-a729-83047d3528e6', formalized).
narrative_ontology:cs_authority_grounding('d9c3893a-ebb8-4f2c-a729-83047d3528e6', lineage).
narrative_ontology:cs_interpretation_layer_present('d9c3893a-ebb8-4f2c-a729-83047d3528e6').
narrative_ontology:cs_reading_relation('d9c3893a-ebb8-4f2c-a729-83047d3528e6', marriage_authority_kernel__hindu_codified_reading, coexists_with).
narrative_ontology:cs_reading_relation('d9c3893a-ebb8-4f2c-a729-83047d3528e6', marriage_authority_kernel__muslim_shariat_reading, coexists_with).
narrative_ontology:cs_reading_relation('d9c3893a-ebb8-4f2c-a729-83047d3528e6', marriage_authority_kernel__christian_canonical_reading, coexists_with).
narrative_ontology:cs_reading_relation('d9c3893a-ebb8-4f2c-a729-83047d3528e6', marriage_authority_kernel__secular_civil_reading, coexists_with).
narrative_ontology:cs_axiom('d9c3893a-ebb8-4f2c-a729-83047d3528e6', foundational, parsi_communal_identity_preservation).
narrative_ontology:cs_axiom_status(parsi_communal_identity_preservation, holdable).
narrative_ontology:cs_axiom_grounding('d9c3893a-ebb8-4f2c-a729-83047d3528e6', parsi_communal_identity_preservation, conventional).
narrative_ontology:cs_axiom('d9c3893a-ebb8-4f2c-a729-83047d3528e6', secondary, endogamy_as_community_boundary).
narrative_ontology:cs_axiom_status(endogamy_as_community_boundary, holdable).
narrative_ontology:cs_axiom_grounding('d9c3893a-ebb8-4f2c-a729-83047d3528e6', endogamy_as_community_boundary, conventional).
narrative_ontology:cs_reference_frame('d9c3893a-ebb8-4f2c-a729-83047d3528e6', parsi_customary_autonomy).
narrative_ontology:cs_drift_state('d9c3893a-ebb8-4f2c-a729-83047d3528e6', contemporary_demographic_decline_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('d9c3893a-ebb8-4f2c-a729-83047d3528e6', '').
narrative_ontology:cs_kernel_id(marriage_authority_kernel__parsi_communal_reading, marriage_authority_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(marriage_authority_kernel__parsi_communal_reading, parsi_community_elders).
narrative_ontology:constraint_beneficiary(marriage_authority_kernel__parsi_communal_reading, parsi_families).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(marriage_authority_kernel__parsi_communal_reading, parsi_individuals).
narrative_ontology:constraint_victim(marriage_authority_kernel__parsi_communal_reading, parsi_individuals).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administer the Parsi Marriage and Divorce Act 1936 and customary law, ensuring adherence to community norms, including endogamy. They are responsible for preserving Parsi identity and traditions through legal and social means.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__parsi_communal_reading, parsi_community_elders, agenda_setter,
    institutional, generational, constrained, national).

% Adhere to the Act for marriage and divorce, benefiting from a distinct communal identity and legal framework. However, they face restrictions, particularly endogamy, which limits their choice of spouse within the community. They can opt for the Special Marriage Act but lose communal recognition.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__parsi_communal_reading, parsi_individuals, payer,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(marriage_authority_kernel__parsi_communal_reading, parsi_individuals, beneficiary).

% Interpret and apply the Parsi Marriage and Divorce Act 1936 in cases, ensuring its provisions align with the broader Indian constitutional framework. They act as an external check on communal authority.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__parsi_communal_reading, indian_civil_courts, observer,
    institutional, generational, analytical, national).

% Parsi individuals who wish to marry outside the community or the Act's provisions, often due to exogamous relationships. They find their choices constrained by the communal law and may have to forgo communal recognition by marrying under the Special Marriage Act.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__parsi_communal_reading, secular_marriage_seekers, excluded,
    powerless, biographical, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(marriage_authority_kernel__parsi_communal_reading, parsi_community_elders).
narrative_ontology:fixing_cost_class(marriage_authority_kernel__parsi_communal_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates marriage and divorce practices for the Parsi community, preserving its distinct religious and cultural identity, ensuring lineage, and providing a recognized legal framework for family matters.
% TRANSFER_FUNCTION: Transfers social and cultural capital to the Parsi community by maintaining its distinct identity and legal autonomy. It transfers adherence to communal norms from individuals to the collective, and restricts individual marital choices in favor of community preservation.
% ABSENT_VOICES: Parsi individuals who advocate for greater individual autonomy in marriage, particularly regarding exogamy, are often marginalized in communal decision-making processes. Their voices are present in individual legal challenges but not in the communal legislative or customary authority.
% DISAPPEARANCE_RATIONALE: If the Parsi Marriage and Divorce Act 1936 vanished, the Parsi community would lose its distinct legal framework for marriage and divorce, forcing all members to marry under the Special Marriage Act or other personal laws. This would significantly erode communal identity and self-governance, leading to a fundamental reorganization of Parsi social and legal structures.
% FOUNDING_PROBLEM: The Parsi community sought to preserve its distinct religious and cultural identity and ensure its legal autonomy within a pluralistic Indian legal system, particularly concerning marriage, divorce, and inheritance, which were governed by diverse customs.
% FOUNDING_PROBLEM_CORROBORATION: Parsi community elders and traditionalists attest that the founding problem of community preservation and distinct identity remains live, especially given demographic decline. However, some Parsi individuals and legal scholars argue that while the problem of identity was once live, the Act's rigidities now create new problems for individual autonomy, making its status contested. Constitutional legal analysis from outside the community also questions the extent to which communal laws can restrict fundamental rights.
narrative_ontology:disappearance_verdict(marriage_authority_kernel__parsi_communal_reading, world_rearranges).
narrative_ontology:founding_problem_status(marriage_authority_kernel__parsi_communal_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(marriage_authority_kernel__parsi_communal_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(marriage_authority_kernel__parsi_communal_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(marriage_authority_kernel__parsi_communal_reading_tests).
:- end_tests(marriage_authority_kernel__parsi_communal_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The constraint is classified as a Rope because it primarily serves a coordination function for the Parsi community, preserving its distinct identity and customs through a codified legal framework. Extractiveness (0.3) is moderate, reflecting the costs of adherence to communal norms (e.g., endogamy) but also the benefits of a stable, recognized legal system. Suppression (0.4) is present due to the enforcement of endogamy and the limited alternatives for Parsi individuals seeking marriage outside the community's framework. Theater ratio is low (0.1) as the Act genuinely functions to regulate marriage and divorce within the community, with minimal performative maintenance. Accessibility collapse (0.6) is moderate, as individuals can opt for the Special Marriage Act, but doing so means stepping outside the communal framework. Resistance (0.2) is low, as most community members accept the framework, though individual challenges may arise.
 *
 * PERSPECTIVAL GAP:
 *   Parsi community elders and families largely experience this as a beneficial Rope, preserving their unique cultural and religious identity. However, individual Parsi members, particularly those seeking exogamous marriages, may experience it as more extractive due to the endogamy requirements, pushing their seat towards a Tangled Rope or even Snare. The Indian civil courts observe it as a specific personal law within a pluralistic legal system, evaluating its constitutional compliance.
 *
 * DIRECTIONALITY LOGIC:
 *   Parsi community elders are beneficiaries (d=0.0-0.1) as they administer and benefit from the preservation of communal identity and authority. Parsi individuals are both beneficiaries and payers (d=0.4-0.6): they benefit from a clear legal framework and community identity but pay through restrictions like endogamy. Secular marriage seekers are targets (d=0.8-0.9) as the Act's provisions directly restrict their choices, pushing them towards alternative legal paths. Indian civil courts are analytical observers (d=0.5).
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate to preserve Parsi identity and customs remains live, though challenged by demographic decline. The classification as a Rope prevents mislabeling it as pure extraction, acknowledging its genuine coordination function for a minority community. However, the omegas highlight potential for drift towards extraction if endogamy enforcement becomes overly rigid or if the Act fails to adapt to evolving communal needs, especially in the face of demographic pressures. The 'contested' status of the founding problem reflects this tension between historical function and contemporary relevance.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is this constraint a genuine expression of Parsi communal custom, or has the 1936 Act ossified custom into a rigid, potentially extractive legal form?',
    'Sociological study of contemporary Parsi community practices versus the Act''s provisions; historical analysis of custom evolution pre- and post-1936.',
    'If ossified, the constraint''s ''customary'' grounding is theatrical, increasing its effective extractiveness and shifting its classification towards a Tangled Rope for those whose evolving customs are suppressed. If genuine, it remains a Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_identity, empirical, 'Ambiguity between living custom and codified law for the Parsi community.').

omega_variable(
    endogamy_enforcement_legitimacy,
    'Does the enforcement of endogamy within the Parsi community, supported by the Act''s framework, constitute a legitimate communal boundary or an undue restriction on individual autonomy?',
    'Legal challenges to endogamy provisions based on constitutional rights; community discourse and evolving social norms.',
    'If deemed an undue restriction, the constraint''s suppression metric would rise for individuals seeking exogamous marriage, and its classification would shift towards a Snare for those individuals. If legitimate, it remains a coordination mechanism for community preservation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(endogamy_enforcement_legitimacy, preference, 'The legitimacy of endogamy enforcement as a communal boundary.').

omega_variable(
    demographic_decline_impact,
    'How does the demographic decline of the Parsi community impact the perceived necessity and function of the 1936 Act?',
    'Analysis of community leadership statements, legal reform proposals, and individual choices in response to demographic trends.',
    'If decline leads to increased pressure for strict adherence to the Act''s provisions (e.g., endogamy), it could increase suppression and extractiveness for individuals. If it leads to calls for liberalization, the constraint''s rigidity might be challenged, potentially leading to a reclassification as a Piton if its original function atrophies.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(demographic_decline_impact, empirical, 'Impact of demographic decline on the Act''s function and enforcement.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(marriage_authority_kernel__parsi_communal_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(marr_tr_t0, marriage_authority_kernel__parsi_communal_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(marr_tr_t10, marriage_authority_kernel__parsi_communal_reading, theater_ratio, 10, 0.1).
narrative_ontology:measurement(marr_tr_t20, marriage_authority_kernel__parsi_communal_reading, theater_ratio, 20, 0.1).
narrative_ontology:measurement(marr_tr_t30, marriage_authority_kernel__parsi_communal_reading, theater_ratio, 30, 0.1).

% Extraction over time
narrative_ontology:measurement(marr_be_t0, marriage_authority_kernel__parsi_communal_reading, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(marr_be_t10, marriage_authority_kernel__parsi_communal_reading, base_extractiveness, 10, 0.28).
narrative_ontology:measurement(marr_be_t20, marriage_authority_kernel__parsi_communal_reading, base_extractiveness, 20, 0.3).
narrative_ontology:measurement(marr_be_t30, marriage_authority_kernel__parsi_communal_reading, base_extractiveness, 30, 0.3).

% Suppression requirement over time
narrative_ontology:measurement(marr_su_t0, marriage_authority_kernel__parsi_communal_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(marr_su_t10, marriage_authority_kernel__parsi_communal_reading, suppression_requirement, 10, 0.38).
narrative_ontology:measurement(marr_su_t20, marriage_authority_kernel__parsi_communal_reading, suppression_requirement, 20, 0.4).
narrative_ontology:measurement(marr_su_t30, marriage_authority_kernel__parsi_communal_reading, suppression_requirement, 30, 0.4).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(marriage_authority_kernel__parsi_communal_reading, identity_coordination).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'marriage_authority_kernel', focusing on the Parsi communal legal framework. It is distinct from other readings (Hindu, Muslim, Christian, Secular Civil) which derive authority from different sources and have different structural properties.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
