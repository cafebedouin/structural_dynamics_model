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
    narrative_ontology:constraint_vindicates/2,
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
 *   human_readable: Parsi Communal Marriage Authority (Parsi Marriage and Divorce Act 1936)
 *   domain: comparative_law/constitutional_pluralism/religious_governance
 *
 * SUMMARY:
 *   This constraint describes the authority of Parsi community custom, as
 *   codified in the Parsi Marriage and Divorce Act 1936, over marriage and
 *   family law for the Parsi community in India. It is one reading of the
 *   broader 'marriage_authority_kernel' in India, which encompasses various
 *   religious and secular legal frameworks. This reading emphasizes community
 *   autonomy, endogamy, and internal gender equity, while facing challenges
 *   from demographic decline and the broader secular legal context.
 *
 * KEY AGENTS:
 *   - parsi_community_elders: Agenda setter (institutional/constrained) — administers the Act, maintains customs.
 *   - parsi_community_members: Beneficiary (moderate/identity_locked) — benefits from cultural preservation, bound by communal ties.
 *   - indian_state_judiciary: Observer (institutional/analytical) — upholds the Act within constitutional limits.
 *   - parsi_youth: Payer (moderate/constrained) — bears costs of endogamy pressure.
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
narrative_ontology:human_readable(marriage_authority_kernel__parsi_communal_reading, "Parsi Communal Marriage Authority (Parsi Marriage and Divorce Act 1936)").
narrative_ontology:topic_domain(marriage_authority_kernel__parsi_communal_reading, "comparative_law/constitutional_pluralism/religious_governance").

domain_priors:requires_active_enforcement(marriage_authority_kernel__parsi_communal_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(marriage_authority_kernel__parsi_communal_reading, '978b322f-f01f-4842-bd6b-ec7153b94584').
narrative_ontology:cs_kernel_codification('978b322f-f01f-4842-bd6b-ec7153b94584', formalized).
narrative_ontology:cs_authority_grounding('978b322f-f01f-4842-bd6b-ec7153b94584', lineage).
narrative_ontology:cs_interpretation_layer_present('978b322f-f01f-4842-bd6b-ec7153b94584').
narrative_ontology:cs_reading_relation('978b322f-f01f-4842-bd6b-ec7153b94584', marriage_authority_kernel__christian_canonical_reading, coexists_with).
narrative_ontology:cs_reading_relation('978b322f-f01f-4842-bd6b-ec7153b94584', marriage_authority_kernel__hindu_codified_reading, coexists_with).
narrative_ontology:cs_reading_relation('978b322f-f01f-4842-bd6b-ec7153b94584', marriage_authority_kernel__muslim_shariat_reading, coexists_with).
narrative_ontology:cs_reading_relation('978b322f-f01f-4842-bd6b-ec7153b94584', marriage_authority_kernel__secular_civil_reading, coexists_with).
narrative_ontology:cs_axiom('978b322f-f01f-4842-bd6b-ec7153b94584', foundational, parsi_custom_as_primary_marital_law).
narrative_ontology:cs_axiom_status(parsi_custom_as_primary_marital_law, holdable).
narrative_ontology:cs_axiom_grounding('978b322f-f01f-4842-bd6b-ec7153b94584', parsi_custom_as_primary_marital_law, conventional).
narrative_ontology:cs_axiom('978b322f-f01f-4842-bd6b-ec7153b94584', secondary, endogamy_as_community_preservation).
narrative_ontology:cs_axiom_status(endogamy_as_community_preservation, holdable).
narrative_ontology:cs_axiom_grounding('978b322f-f01f-4842-bd6b-ec7153b94584', endogamy_as_community_preservation, conventional).
narrative_ontology:cs_reference_frame('978b322f-f01f-4842-bd6b-ec7153b94584', parsi_community_autonomy_framework).
narrative_ontology:cs_drift_state('978b322f-f01f-4842-bd6b-ec7153b94584', contemporary_pluralistic_india, gap(stable, minor, true)).
narrative_ontology:cs_created_at('978b322f-f01f-4842-bd6b-ec7153b94584', '').
narrative_ontology:cs_kernel_id(marriage_authority_kernel__parsi_communal_reading, marriage_authority_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(marriage_authority_kernel__parsi_communal_reading, parsi_community_elders).
narrative_ontology:constraint_beneficiary(marriage_authority_kernel__parsi_communal_reading, parsi_community_members).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(marriage_authority_kernel__parsi_communal_reading, parsi_youth).
narrative_ontology:constraint_vindicates(marriage_authority_kernel__parsi_communal_reading, religious_pluralism_doctrine).
narrative_ontology:constraint_vindicates(marriage_authority_kernel__parsi_communal_reading, community_autonomy_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administer the Parsi Marriage and Divorce Act 1936, ensuring adherence to community customs, particularly regarding endogamy. They benefit from maintaining community identity and cohesion, but face challenges from demographic decline and external legal pressures.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__parsi_communal_reading, parsi_community_elders, agenda_setter,
    institutional, generational, constrained, local).

% Benefit from a clear, culturally specific framework for marriage and family life that reinforces their unique identity. They are identity-locked by strong communal ties and cultural heritage, making exit from the community's legal framework a significant personal cost.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__parsi_communal_reading, parsi_community_members, beneficiary,
    moderate, biographical, identity_locked, local).

% Interprets and upholds the Parsi Marriage and Divorce Act 1936 as part of India's pluralistic legal system. They ensure the Act's provisions align with broader constitutional principles, particularly regarding gender equity, but generally defer to community autonomy where possible.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__parsi_communal_reading, indian_state_judiciary, observer,
    institutional, civilizational, analytical, national).

% Experience pressure to conform to endogamous marriage customs to maintain community identity, which can limit their personal choices. While benefiting from community support, they bear the cost of restricted marital options in a diverse society.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__parsi_communal_reading, parsi_youth, payer,
    moderate, biographical, constrained, local).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a stable, culturally specific framework for marriage and family within the Parsi community, preserving unique customs and identity amidst a diverse national legal landscape.
% TRANSFER_FUNCTION: Transfers authority over marital and familial matters from the general civil code to community-specific tribunals and customs, reinforcing communal identity and autonomy.
% ABSENT_VOICES: Individuals seeking inter-community marriages who wish to remain within the Parsi legal framework are implicitly excluded; their voices would challenge the endogamy enforcement but are often marginalized by communal pressure.
% DISAPPEARANCE_RATIONALE: If the Parsi Marriage and Divorce Act 1936 vanished, the Parsi community would lose its distinct legal identity for marriage, forcing members into the secular civil code or other religious personal laws. This would fundamentally alter the community's social structure and cultural preservation efforts.
% FOUNDING_PROBLEM: The Parsi community, a distinct religious and ethnic minority in India, required a legal framework that recognized and preserved its unique customs for marriage and divorce, distinct from other religious or secular laws.
% FOUNDING_PROBLEM_CORROBORATION: Parsi community leaders and historians attest that the problem of preserving a distinct Parsi identity and legal framework remains live, especially given demographic challenges. Legal scholars and constitutional experts corroborate the historical context of pluralism that necessitated such acts.
narrative_ontology:disappearance_verdict(marriage_authority_kernel__parsi_communal_reading, world_rearranges).
narrative_ontology:founding_problem_status(marriage_authority_kernel__parsi_communal_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(marriage_authority_kernel__parsi_communal_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(marriage_authority_kernel__parsi_communal_reading, 'none', 1).
narrative_ontology:epsilon_provenance(marriage_authority_kernel__parsi_communal_reading, 0.3, 'gemini-2.5-flash', 'none', direct).

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
 *   The constraint is classified as a Rope because it primarily serves a coordination function for the Parsi community, enabling the preservation of their distinct cultural and religious identity through a recognized legal framework. Extractiveness is moderate (0.3) due to the pressure for endogamy, which limits individual choice for community members, particularly the youth. Suppression is also moderate (0.4), as adherence is largely driven by social cohesion and identity rather than overt coercion, though community tribunals enforce the Act. The theater ratio is low (0.1) as the Act genuinely functions to coordinate community life. Accessibility collapse is moderate (0.6) because while community members can opt for secular marriage, doing so often entails a significant social and identity cost. Resistance is low (0.2) as the community largely supports the framework, despite individual pressures.
 *
 * PERSPECTIVAL GAP:
 *   Parsi community elders and members largely perceive this as a beneficial Rope, essential for cultural survival. However, Parsi youth may experience it with higher extractiveness due to the pressure to conform to endogamous marriage customs, which can feel more like a Snare from their individual perspective, limiting personal autonomy.
 *
 * DIRECTIONALITY LOGIC:
 *   Parsi community elders are beneficiaries as they maintain their authority and the community's distinct identity. Parsi community members are also beneficiaries, gaining a clear cultural framework, but are identity-locked by strong communal ties. Parsi youth, while beneficiaries of community support, also act as payers due to the constraints on marital choice. The Indian state judiciary acts as an observer, ensuring legal consistency without direct benefit or cost from the community's internal operations.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate to preserve Parsi identity and customs remains live, preventing mislabeling as a Piton. While demographic decline poses a viability challenge, the Act's function is still actively pursued by the community. The classification as a Rope acknowledges its coordination function while the moderate extractiveness and suppression metrics capture the internal pressures and costs, preventing it from being mislabeled as a pure Snare.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    endogamy_vs_individual_autonomy,
    'To what extent does the enforcement of endogamy within the Parsi Marriage and Divorce Act 1936 conflict with individual autonomy and choice, particularly for Parsi youth?',
    'Sociological studies on marriage patterns and individual testimonials within the Parsi community, alongside legal challenges to endogamy provisions.',
    'If the conflict is severe, the extractiveness for Parsi youth would be higher, potentially shifting their seat classification towards a Snare. If community support for endogamy is genuinely high and perceived as beneficial, the current Rope classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(endogamy_vs_individual_autonomy, empirical, 'Assessing the balance between communal preservation and individual rights regarding endogamy.').

omega_variable(
    demographic_decline_impact,
    'How does the ongoing demographic decline of the Parsi community impact the long-term viability and perceived legitimacy of the Parsi Marriage and Divorce Act 1936?',
    'Longitudinal demographic studies and analysis of community engagement with the Act over time. Legal reforms or community initiatives to address demographic challenges.',
    'If demographic decline severely undermines the Act''s practical application or leads to widespread non-compliance, its theater_ratio could increase, and its overall classification might drift towards a Piton due to functional atrophy.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(demographic_decline_impact, empirical, 'Impact of demographic trends on the constraint''s functional relevance and persistence.').

omega_variable(
    kernel_reading_identification,
    'Is this constraint accurately identified as the ''parsi_communal_reading'' of the ''marriage_authority_kernel'', or does it conflate distinct legal and social mechanisms?',
    'Comparative legal analysis of the Parsi Marriage and Divorce Act 1936 against other personal laws and community customs, identifying unique structural elements and their grounding.',
    'If the reading is found to conflate distinct mechanisms, it would require decomposition into multiple, more granular constraint stories, each with its own ε and classification. If the identification is robust, the current analysis holds.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identification, conceptual, 'Verifying the precise scope and boundaries of this specific reading within the broader kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(marriage_authority_kernel__parsi_communal_reading, 1936, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Extraction over time
narrative_ontology:measurement(marr_be_t1936, marriage_authority_kernel__parsi_communal_reading, base_extractiveness, 1936, 0.2).
narrative_ontology:measurement(marr_be_t1960, marriage_authority_kernel__parsi_communal_reading, base_extractiveness, 1960, 0.25).
narrative_ontology:measurement(marr_be_t1980, marriage_authority_kernel__parsi_communal_reading, base_extractiveness, 1980, 0.28).
narrative_ontology:measurement(marr_be_t2000, marriage_authority_kernel__parsi_communal_reading, base_extractiveness, 2000, 0.3).
narrative_ontology:measurement(marr_be_t2024, marriage_authority_kernel__parsi_communal_reading, base_extractiveness, 2024, 0.3).

% Suppression requirement over time
narrative_ontology:measurement(marr_su_t1936, marriage_authority_kernel__parsi_communal_reading, suppression_requirement, 1936, 0.3).
narrative_ontology:measurement(marr_su_t1960, marriage_authority_kernel__parsi_communal_reading, suppression_requirement, 1960, 0.35).
narrative_ontology:measurement(marr_su_t1980, marriage_authority_kernel__parsi_communal_reading, suppression_requirement, 1980, 0.38).
narrative_ontology:measurement(marr_su_t2000, marriage_authority_kernel__parsi_communal_reading, suppression_requirement, 2000, 0.4).
narrative_ontology:measurement(marr_su_t2024, marriage_authority_kernel__parsi_communal_reading, suppression_requirement, 2024, 0.4).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(marriage_authority_kernel__parsi_communal_reading, identity_coordination).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
