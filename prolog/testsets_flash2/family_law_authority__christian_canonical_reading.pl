% ============================================================================
% CONSTRAINT STORY: family_law_authority__christian_canonical_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_family_law_authority__christian_canonical_reading, []).

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
 *   constraint_id: family_law_authority__christian_canonical_reading
 *   human_readable: Christian Canonical Marriage (Catholic/Protestant)
 *   domain: religious_governance/family_law
 *
 * SUMMARY:
 *   This constraint describes the institution of marriage as defined and
 *   governed by Christian canonical law, encompassing both the strict
 *   sacramental view of Catholicism (indissoluble, requiring annulment for
 *   remarriage) and the more varied, often less rigid, denominational
 *   governance within Protestantism (where divorce and remarriage may be
 *   permitted under certain conditions). It is a reading of the broader
 *   'family_law_authority' kernel, focusing on the ecclesiastical authority's
 *   role in defining and enforcing marital norms. The constraint functions as
 *   a Tangled Rope, providing coordination for religious communities while
 *   extracting costs from individuals whose marital situations diverge from
 *   canonical norms.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(family_law_authority__christian_canonical_reading, 0.45).
domain_priors:suppression_score(family_law_authority__christian_canonical_reading, 0.6).
domain_priors:theater_ratio(family_law_authority__christian_canonical_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(family_law_authority__christian_canonical_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(family_law_authority__christian_canonical_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(family_law_authority__christian_canonical_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(family_law_authority__christian_canonical_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(family_law_authority__christian_canonical_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(family_law_authority__christian_canonical_reading, tangled_rope).
narrative_ontology:human_readable(family_law_authority__christian_canonical_reading, "Christian Canonical Marriage (Catholic/Protestant)").
narrative_ontology:topic_domain(family_law_authority__christian_canonical_reading, "religious_governance/family_law").

domain_priors:requires_active_enforcement(family_law_authority__christian_canonical_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(family_law_authority__christian_canonical_reading, '4250344c-7f61-4926-8ffc-d68e84d879c5').
narrative_ontology:cs_kernel_codification('4250344c-7f61-4926-8ffc-d68e84d879c5', formalized).
narrative_ontology:cs_authority_grounding('4250344c-7f61-4926-8ffc-d68e84d879c5', lineage).
narrative_ontology:cs_interpretation_layer_present('4250344c-7f61-4926-8ffc-d68e84d879c5').
narrative_ontology:cs_reading_relation('4250344c-7f61-4926-8ffc-d68e84d879c5', family_law_authority__hindu_dharmashastra_reading, coexists_with).
narrative_ontology:cs_reading_relation('4250344c-7f61-4926-8ffc-d68e84d879c5', family_law_authority__muslim_shariat_reading, coexists_with).
narrative_ontology:cs_reading_relation('4250344c-7f61-4926-8ffc-d68e84d879c5', family_law_authority__parsi_zoroastrian_reading, coexists_with).
narrative_ontology:cs_reading_relation('4250344c-7f61-4926-8ffc-d68e84d879c5', family_law_authority__secular_contractual_reading, influences).
narrative_ontology:cs_axiom('4250344c-7f61-4926-8ffc-d68e84d879c5', foundational, marriage_as_sacrament_or_divine_covenant).
narrative_ontology:cs_axiom_status(marriage_as_sacrament_or_divine_covenant, holdable).
narrative_ontology:cs_axiom_grounding('4250344c-7f61-4926-8ffc-d68e84d879c5', marriage_as_sacrament_or_divine_covenant, theological).
narrative_ontology:cs_axiom('4250344c-7f61-4926-8ffc-d68e84d879c5', foundational, ecclesiastical_authority_over_marital_validity).
narrative_ontology:cs_axiom_status(ecclesiastical_authority_over_marital_validity, holdable).
narrative_ontology:cs_axiom_grounding('4250344c-7f61-4926-8ffc-d68e84d879c5', ecclesiastical_authority_over_marital_validity, conventional).
narrative_ontology:cs_reference_frame('4250344c-7f61-4926-8ffc-d68e84d879c5', traditional_christian_marital_doctrine).
narrative_ontology:cs_drift_state('4250344c-7f61-4926-8ffc-d68e84d879c5', contemporary_secular_society, gap(authority_erosion, substantial, true)).
narrative_ontology:cs_created_at('4250344c-7f61-4926-8ffc-d68e84d879c5', '').
narrative_ontology:cs_kernel_id(family_law_authority__christian_canonical_reading, family_law_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(family_law_authority__christian_canonical_reading, ecclesiastical_authorities).
narrative_ontology:constraint_beneficiary(family_law_authority__christian_canonical_reading, religious_communities).
narrative_ontology:constraint_victim(family_law_authority__christian_canonical_reading, married_individuals_seeking_divorce).
narrative_ontology:constraint_victim(family_law_authority__christian_canonical_reading, individuals_seeking_remarriage).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Catholic authorities define marriage as an indissoluble sacrament, while Protestant denominations vary in their allowance for divorce and remarriage. They administer the rites, interpret canonical law, and enforce adherence through pastoral guidance and disciplinary measures. Their authority is grounded in theological tradition and institutional structure.
narrative_ontology:constraint_stakeholder(family_law_authority__christian_canonical_reading, ecclesiastical_authorities, agenda_setter,
    institutional, generational, identity_locked, global).

% Benefit from the stability and normative clarity provided by the canonical understanding of marriage, which reinforces community values and social cohesion. They exert social pressure on members to conform to religious teachings on marriage and family life.
narrative_ontology:constraint_stakeholder(family_law_authority__christian_canonical_reading, religious_communities, beneficiary,
    organized, generational, constrained, local).

% Face significant spiritual, social, and sometimes legal penalties for seeking divorce or remarriage outside of ecclesiastical recognition, particularly in Catholic contexts where annulment is the only path to remarriage within the church. Their identity is often deeply intertwined with their religious affiliation.
narrative_ontology:constraint_stakeholder(family_law_authority__christian_canonical_reading, married_individuals_seeking_divorce, payer,
    powerless, biographical, identity_locked, local).

% Are often barred from remarriage within their religious tradition if their previous marriage is not ecclesiastically dissolved or annulled, leading to social exclusion or a perceived state of sin. This can lead to a profound sense of identity conflict.
narrative_ontology:constraint_stakeholder(family_law_authority__christian_canonical_reading, individuals_seeking_remarriage, payer,
    powerless, biographical, identity_locked, local).

% Recognize civil marriage and divorce, often in tension with ecclesiastical rules. They observe the impact of religious marriage laws on individuals' civil rights and social welfare, sometimes intervening through legislation or judicial rulings.
narrative_ontology:constraint_stakeholder(family_law_authority__christian_canonical_reading, secular_legal_systems, observer,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates family structure, sexual ethics, and intergenerational transmission of religious values within Christian communities, providing a stable framework for social reproduction and spiritual life.
% TRANSFER_FUNCTION: Transfers authority over marital status and family formation from individuals to ecclesiastical bodies, in exchange for spiritual legitimacy and community belonging. It also transfers social capital and moral standing to those who conform, and imposes costs on those who do not.
% ABSENT_VOICES: Individuals who prioritize personal autonomy and secular legal rights over ecclesiastical authority in matters of marriage and divorce are often marginalized or excluded from the internal discourse of religious communities. They would advocate for greater individual freedom and less institutional control over marital decisions.
% DISAPPEARANCE_RATIONALE: If Christian canonical marriage vanished overnight, the social fabric of many religious communities would be profoundly altered. Family structures, inheritance practices, and the moral authority of religious institutions would undergo significant reorganization, leading to a more individualized and secular approach to marital unions within these communities.
% FOUNDING_PROBLEM: To establish a divinely ordained and stable institution for procreation, mutual support, and the prevention of promiscuity, ensuring the moral and social order of Christian society.
% FOUNDING_PROBLEM_CORROBORATION: Ecclesiastical authorities and many religious adherents attest that the founding problems of moral order, family stability, and spiritual integrity remain live. Secular observers and some former adherents argue that while some aspects remain relevant, the constraint's primary function has shifted towards maintaining institutional power and traditional social hierarchies, rather than solely addressing original moral concerns.
narrative_ontology:disappearance_verdict(family_law_authority__christian_canonical_reading, world_rearranges).
narrative_ontology:founding_problem_status(family_law_authority__christian_canonical_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(family_law_authority__christian_canonical_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(family_law_authority__christian_canonical_reading, 'none', 1).
narrative_ontology:epsilon_provenance(family_law_authority__christian_canonical_reading, 0.45, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(family_law_authority__christian_canonical_reading_tests).
:- end_tests(family_law_authority__christian_canonical_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.45) due to the significant social and spiritual costs imposed on individuals who do not conform to canonical norms, particularly regarding divorce and remarriage. Suppression is high (0.60) because the authority of religious institutions is deeply embedded in the identity and social fabric of their communities, making exit or non-compliance difficult without significant personal cost. The theater ratio is low (0.10) as the enforcement of canonical marriage is generally sincere and functional within its theological framework, not primarily performative. The slight decrease in suppression over time reflects a gradual erosion of strict ecclesiastical control in some contexts due to secularization and individual autonomy.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of ecclesiastical authorities, this constraint is a divinely ordained and beneficial structure for moral order. From the perspective of individuals seeking divorce or remarriage, it can be an oppressive and extractive force that limits personal freedom and causes significant suffering. The engine's classification will reflect this divergence based on the structural positions of the stakeholders.
 *
 * DIRECTIONALITY LOGIC:
 *   Ecclesiastical authorities and religious communities are beneficiaries, gaining social cohesion and moral authority from the constraint. Individuals seeking divorce or remarriage are payers, bearing the costs of non-conformity, often experiencing identity-lock due to their religious affiliation. Secular legal systems act as observers, recognizing civil marriage but not necessarily the ecclesiastical definitions.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sacramental_vs_contractual_nature,
    'Is marriage fundamentally a sacrament (divinely instituted, indissoluble) or a contract (human agreement, dissoluble)?',
    'Theological and philosophical debate, as well as the lived experience of adherents and the evolving interpretations within different Christian traditions. No single empirical resolution is possible.',
    'If primarily sacramental, the constraint''s permanence and ecclesiastical authority are reinforced, increasing extractiveness for those seeking exit. If primarily contractual, it aligns more with secular views, potentially reducing suppression and opening pathways for individual autonomy.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sacramental_vs_contractual_nature, conceptual, 'Ambiguity regarding the fundamental nature of marriage (sacrament vs. contract).').

omega_variable(
    internalized_vs_structural_suppression,
    'To what extent is the measured suppression structural (ecclesiastical penalties, social exclusion) versus internalized (guilt, fear of spiritual consequences, identity fusion)?',
    'Post-exit suppression trajectory: if individuals continue to experience guilt or identity conflict after leaving the religious community, it indicates a significant internalized component. Sociological studies on ex-members'' well-being.',
    'If internalized suppression is high, the constraint''s effective suppression is higher than structural measures suggest, as individuals carry the suppression with them even after formal exit. This would amplify the perceived extractiveness for affected individuals.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(internalized_vs_structural_suppression, empirical, 'Structural vs. internalized suppression mechanism in religious contexts.').

omega_variable(
    denominational_variance_impact,
    'How does the variance in divorce/remarriage policies across Protestant denominations affect the overall extractiveness and suppression of this ''Christian Canonical'' reading?',
    'Comparative analysis of individual experiences and exit options in different Protestant denominations versus the Catholic Church. Quantitative studies on divorce rates and social acceptance within various traditions.',
    'Greater denominational variance could lower the overall extractiveness and suppression for some individuals by providing more ''mobile'' exit options within the broader Christian framework, making the constraint less uniformly ''Tangled Rope'' across all Christian adherents.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(denominational_variance_impact, empirical, 'Impact of denominational differences on constraint''s severity.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(family_law_authority__christian_canonical_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fami_tr_t0, family_law_authority__christian_canonical_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(fami_tr_t10, family_law_authority__christian_canonical_reading, theater_ratio, 10, 0.1).
narrative_ontology:measurement(fami_tr_t20, family_law_authority__christian_canonical_reading, theater_ratio, 20, 0.1).
narrative_ontology:measurement(fami_tr_t30, family_law_authority__christian_canonical_reading, theater_ratio, 30, 0.1).
narrative_ontology:measurement(fami_tr_t40, family_law_authority__christian_canonical_reading, theater_ratio, 40, 0.1).
narrative_ontology:measurement(fami_tr_t50, family_law_authority__christian_canonical_reading, theater_ratio, 50, 0.1).

% Extraction over time
narrative_ontology:measurement(fami_be_t0, family_law_authority__christian_canonical_reading, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(fami_be_t10, family_law_authority__christian_canonical_reading, base_extractiveness, 10, 0.42).
narrative_ontology:measurement(fami_be_t20, family_law_authority__christian_canonical_reading, base_extractiveness, 20, 0.44).
narrative_ontology:measurement(fami_be_t30, family_law_authority__christian_canonical_reading, base_extractiveness, 30, 0.45).
narrative_ontology:measurement(fami_be_t40, family_law_authority__christian_canonical_reading, base_extractiveness, 40, 0.45).
narrative_ontology:measurement(fami_be_t50, family_law_authority__christian_canonical_reading, base_extractiveness, 50, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(fami_su_t0, family_law_authority__christian_canonical_reading, suppression_requirement, 0, 0.7).
narrative_ontology:measurement(fami_su_t10, family_law_authority__christian_canonical_reading, suppression_requirement, 10, 0.68).
narrative_ontology:measurement(fami_su_t20, family_law_authority__christian_canonical_reading, suppression_requirement, 20, 0.65).
narrative_ontology:measurement(fami_su_t30, family_law_authority__christian_canonical_reading, suppression_requirement, 30, 0.62).
narrative_ontology:measurement(fami_su_t40, family_law_authority__christian_canonical_reading, suppression_requirement, 40, 0.6).
narrative_ontology:measurement(fami_su_t50, family_law_authority__christian_canonical_reading, suppression_requirement, 50, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(family_law_authority__christian_canonical_reading, identity_coordination).
narrative_ontology:affects_constraint(family_law_authority__christian_canonical_reading, family_law_authority__secular_contractual_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'family_law_authority' kernel. Its ecclesiastical authority and sacramental view of marriage stand in tension with secular, contractual understandings, influencing and being influenced by state legal systems.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
