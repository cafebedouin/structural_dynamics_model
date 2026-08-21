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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
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
 *   constraint_id: marriage_authority_kernel__parsi_communal_reading
 *   human_readable: Parsi Marriage and Divorce Act 1936 (Communal Reading)
 *   domain: comparative_law/constitutional_pluralism/religious_governance
 *
 * SUMMARY:
 *   This constraint describes the Parsi communal reading of marriage and
 *   family law authority, as codified in the Parsi Marriage and Divorce Act
 *   1936. It is one reading of the broader 'marriage_authority_kernel' in
 *   India, which encompasses multiple religious and secular legal frameworks.
 *   This reading emphasizes community custom, endogamy, and internal gender
 *   equity, while facing challenges from demographic decline and individual
 *   rights claims. The constraint is claimed as a Rope, reflecting its
 *   genuine coordination function for the community, but with increasing
 *   extractiveness and suppression for those seeking interfaith marriages.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(marriage_authority_kernel__parsi_communal_reading, 0.35).
domain_priors:suppression_score(marriage_authority_kernel__parsi_communal_reading, 0.45).
domain_priors:theater_ratio(marriage_authority_kernel__parsi_communal_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(marriage_authority_kernel__parsi_communal_reading, extractiveness, 0.35).
narrative_ontology:constraint_metric(marriage_authority_kernel__parsi_communal_reading, suppression_requirement, 0.45).
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
narrative_ontology:cs_story_uid(marriage_authority_kernel__parsi_communal_reading, 'fcc8888a-d3cd-41a8-a5ea-b30c1138cef9').
narrative_ontology:cs_kernel_codification('fcc8888a-d3cd-41a8-a5ea-b30c1138cef9', formalized).
narrative_ontology:cs_authority_grounding('fcc8888a-d3cd-41a8-a5ea-b30c1138cef9', lineage).
narrative_ontology:cs_interpretation_layer_present('fcc8888a-d3cd-41a8-a5ea-b30c1138cef9').
narrative_ontology:cs_reading_relation('fcc8888a-d3cd-41a8-a5ea-b30c1138cef9', marriage_authority_kernel__hindu_codified_reading, coexists_with).
narrative_ontology:cs_reading_relation('fcc8888a-d3cd-41a8-a5ea-b30c1138cef9', marriage_authority_kernel__muslim_shariat_reading, coexists_with).
narrative_ontology:cs_reading_relation('fcc8888a-d3cd-41a8-a5ea-b30c1138cef9', marriage_authority_kernel__christian_canonical_reading, coexists_with).
narrative_ontology:cs_reading_relation('fcc8888a-d3cd-41a8-a5ea-b30c1138cef9', marriage_authority_kernel__secular_civil_reading, coexists_with).
narrative_ontology:cs_axiom('fcc8888a-d3cd-41a8-a5ea-b30c1138cef9', foundational, parsi_custom_as_supreme_marital_law).
narrative_ontology:cs_axiom_status(parsi_custom_as_supreme_marital_law, holdable).
narrative_ontology:cs_axiom_grounding('fcc8888a-d3cd-41a8-a5ea-b30c1138cef9', parsi_custom_as_supreme_marital_law, conventional).
narrative_ontology:cs_axiom('fcc8888a-d3cd-41a8-a5ea-b30c1138cef9', foundational, endogamy_as_identity_preservation).
narrative_ontology:cs_axiom_status(endogamy_as_identity_preservation, holdable).
narrative_ontology:cs_axiom_grounding('fcc8888a-d3cd-41a8-a5ea-b30c1138cef9', endogamy_as_identity_preservation, instrumental).
narrative_ontology:cs_reference_frame('fcc8888a-d3cd-41a8-a5ea-b30c1138cef9', parsi_communal_autonomy_1936).
narrative_ontology:cs_drift_state('fcc8888a-d3cd-41a8-a5ea-b30c1138cef9', contemporary_pluralist_india, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('fcc8888a-d3cd-41a8-a5ea-b30c1138cef9', '').
narrative_ontology:cs_kernel_id(marriage_authority_kernel__parsi_communal_reading, marriage_authority_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(marriage_authority_kernel__parsi_communal_reading, parsi_community_elders).
narrative_ontology:constraint_beneficiary(marriage_authority_kernel__parsi_communal_reading, parsi_community_members).
narrative_ontology:constraint_victim(marriage_authority_kernel__parsi_communal_reading, parsi_individuals_seeking_interfaith_marriage).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administer the Parsi Marriage and Divorce Act, interpreting community custom and enforcing endogamy rules. They benefit from maintaining the distinct identity and traditions of the Parsi community, which the Act helps preserve.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__parsi_communal_reading, parsi_community_elders, agenda_setter,
    institutional, generational, constrained, national).

% Benefit from a clear, community-specific legal framework for marriage and divorce that respects their cultural and religious traditions. They experience high gender equity within this framework, but are bound by its endogamy requirements.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__parsi_communal_reading, parsi_community_members, beneficiary,
    moderate, biographical, constrained, national).

% Face social and legal pressure if they seek to marry outside the Parsi community, as such marriages are not recognized under the Act and may lead to ostracization. Their identity is deeply tied to the community, making exit difficult.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__parsi_communal_reading, parsi_individuals_seeking_interfaith_marriage, payer,
    powerless, immediate, identity_locked, local).

% Adjudicate disputes arising under the Parsi Marriage and Divorce Act, ensuring its application is consistent with constitutional principles, particularly regarding gender equity. They act as an external check on communal authority.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__parsi_communal_reading, indian_civil_courts, observer,
    institutional, generational, analytical, national).

% Advocate for a uniform civil code that would supersede religion-specific personal laws, including the Parsi Act, to ensure equal rights for all citizens regardless of religious affiliation. They are excluded from the direct administration of the Parsi Act but influence the broader legal discourse.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__parsi_communal_reading, secular_marriage_advocates, excluded,
    organized, generational, mobile, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a distinct, culturally specific legal framework for marriage and divorce that coordinates social norms and religious customs for the Parsi community, preserving its unique identity and traditions.
% TRANSFER_FUNCTION: Transfers authority over marital and familial matters from the general civil code to community-specific tribunals and customs, in exchange for maintaining communal cohesion and identity.
% ABSENT_VOICES: Parsi individuals who wish to marry outside the community without losing their communal identity or legal recognition are structurally marginalized; their voices are often suppressed by communal pressure and the legal framework itself.
% DISAPPEARANCE_RATIONALE: If the Parsi Marriage and Divorce Act vanished, the Parsi community would lose its distinct legal identity for marriage, forcing members into the secular civil code or other religious frameworks. This would significantly alter communal cohesion and accelerate demographic decline, fundamentally reorganizing Parsi social structures.
% FOUNDING_PROBLEM: The Parsi community sought to preserve its distinct religious and cultural identity within a diverse legal landscape, requiring a specific legal framework for marriage and divorce that reflected its customs and traditions.
% FOUNDING_PROBLEM_CORROBORATION: Parsi community leaders and many members attest that the problem of preserving identity and tradition is still live, especially given demographic decline. External legal scholars and constitutional experts corroborate the historical context of the Act's creation to protect minority rights, though they may question its contemporary implications for individual autonomy.
narrative_ontology:disappearance_verdict(marriage_authority_kernel__parsi_communal_reading, world_rearranges).
narrative_ontology:founding_problem_status(marriage_authority_kernel__parsi_communal_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(marriage_authority_kernel__parsi_communal_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(marriage_authority_kernel__parsi_communal_reading, 'none', 1).
narrative_ontology:epsilon_provenance(marriage_authority_kernel__parsi_communal_reading, 0.35, 'gemini-2.5-flash', 'none', direct).

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
 *   Extractiveness (0.35) is moderate, reflecting the costs borne by individuals whose marital choices conflict with communal norms, particularly endogamy. Suppression (0.45) is also moderate, stemming from social pressure and the lack of legal recognition for non-communal marriages within the Parsi framework. Theater ratio (0.1) is low, as the Act genuinely functions to preserve Parsi identity, though its enforcement mechanisms are increasingly challenged. The increasing extractiveness and suppression over time reflect growing societal pressure for individual autonomy against communal traditions.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of Parsi community elders, the Act is a vital Rope, essential for cultural preservation and coordination. From the perspective of individuals seeking interfaith marriages, it operates with significant suppressive and extractive force, limiting their autonomy. The engine's per-seat classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Parsi community elders are beneficiaries and agenda-setters, as they administer the Act and maintain communal identity. Parsi community members are largely beneficiaries, gaining a clear cultural framework, but face costs if their choices diverge. Individuals seeking interfaith marriages are victims, bearing the costs of non-recognition and social ostracization, with their identity-locked exit options making their directionality high. Civil courts and secular advocates act as observers or excluded parties, influencing the broader legal context but not directly benefiting or paying under this specific communal framework.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    demographic_decline_impact,
    'How does the ongoing demographic decline of the Parsi community impact the perceived legitimacy and functional necessity of the Parsi Marriage and Divorce Act?',
    'Sociological studies on community cohesion and legal analysis of the Act''s role in population trends; surveys of Parsi youth attitudes towards endogamy.',
    'If demographic decline is exacerbated by strict endogamy, the Act''s perceived coordination function may diminish, increasing its effective extractiveness and potentially reclassifying it towards a Snare or Piton if its primary function becomes merely theatrical preservation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(demographic_decline_impact, empirical, 'Impact of demographic decline on the Act''s legitimacy and function.').

omega_variable(
    individual_autonomy_vs_communal_identity,
    'Is the enforcement of endogamy a legitimate exercise of communal self-preservation, or an undue restriction on individual autonomy and choice?',
    'Constitutional court rulings on the balance between minority group rights and individual fundamental rights; philosophical analysis of collective vs. individual identity formation.',
    'If individual autonomy is prioritized, the Act''s suppressive elements would be re-evaluated as illegitimate, increasing its effective extractiveness and potentially reclassifying it as a Snare. If communal identity is prioritized, the current classification as a Rope with moderate extraction would be reinforced.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(individual_autonomy_vs_communal_identity, preference, 'Conceptual tension between communal identity preservation and individual autonomy.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (legal non-recognition, social ostracization) or internalized (identity fusion, fear of communal exclusion)?',
    'Post-exit suppression trajectory for individuals who marry outside the community: if suppression persists after the legal/social barriers are removed (e.g., in diaspora communities), reclassify as partially internalized.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests — the target carries the suppression with them after exit, making the constraint more resilient to external challenges.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for endogamy.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(marriage_authority_kernel__parsi_communal_reading, 1936, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(marr_tr_t1936, marriage_authority_kernel__parsi_communal_reading, theater_ratio, 1936, 0.05).
narrative_ontology:measurement(marr_tr_t1960, marriage_authority_kernel__parsi_communal_reading, theater_ratio, 1960, 0.07).
narrative_ontology:measurement(marr_tr_t1980, marriage_authority_kernel__parsi_communal_reading, theater_ratio, 1980, 0.08).
narrative_ontology:measurement(marr_tr_t2000, marriage_authority_kernel__parsi_communal_reading, theater_ratio, 2000, 0.09).
narrative_ontology:measurement(marr_tr_t2024, marriage_authority_kernel__parsi_communal_reading, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(marr_be_t1936, marriage_authority_kernel__parsi_communal_reading, base_extractiveness, 1936, 0.25).
narrative_ontology:measurement(marr_be_t1960, marriage_authority_kernel__parsi_communal_reading, base_extractiveness, 1960, 0.28).
narrative_ontology:measurement(marr_be_t1980, marriage_authority_kernel__parsi_communal_reading, base_extractiveness, 1980, 0.3).
narrative_ontology:measurement(marr_be_t2000, marriage_authority_kernel__parsi_communal_reading, base_extractiveness, 2000, 0.32).
narrative_ontology:measurement(marr_be_t2024, marriage_authority_kernel__parsi_communal_reading, base_extractiveness, 2024, 0.35).

% Suppression requirement over time
narrative_ontology:measurement(marr_su_t1936, marriage_authority_kernel__parsi_communal_reading, suppression_requirement, 1936, 0.3).
narrative_ontology:measurement(marr_su_t1960, marriage_authority_kernel__parsi_communal_reading, suppression_requirement, 1960, 0.35).
narrative_ontology:measurement(marr_su_t1980, marriage_authority_kernel__parsi_communal_reading, suppression_requirement, 1980, 0.4).
narrative_ontology:measurement(marr_su_t2000, marriage_authority_kernel__parsi_communal_reading, suppression_requirement, 2000, 0.42).
narrative_ontology:measurement(marr_su_t2024, marriage_authority_kernel__parsi_communal_reading, suppression_requirement, 2024, 0.45).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(marriage_authority_kernel__parsi_communal_reading, identity_coordination).
narrative_ontology:affects_constraint(marriage_authority_kernel__parsi_communal_reading, marriage_authority_kernel__hindu_codified_reading).
narrative_ontology:affects_constraint(marriage_authority_kernel__parsi_communal_reading, marriage_authority_kernel__muslim_shariat_reading).
narrative_ontology:affects_constraint(marriage_authority_kernel__parsi_communal_reading, marriage_authority_kernel__christian_canonical_reading).
narrative_ontology:affects_constraint(marriage_authority_kernel__parsi_communal_reading, marriage_authority_kernel__secular_civil_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'marriage_authority_kernel', focusing on Parsi communal custom. It is linked to other religious and secular readings of the same kernel, which represent alternative legal frameworks for marriage in India.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
