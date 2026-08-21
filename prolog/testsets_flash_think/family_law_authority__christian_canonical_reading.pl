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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: family_law_authority__christian_canonical_reading
 *   human_readable: Christian Canonical Reading of Marriage Authority
 *   domain: religious/social/legal
 *
 * SUMMARY:
 *   This constraint represents the Christian canonical understanding of
 *   marriage, encompassing both the Catholic view of marriage as an
 *   indissoluble sacrament under ecclesiastical authority and the Protestant
 *   view of marriage as a sacred covenant under denominational governance,
 *   with varying stances on divorce. It is one reading of the broader
 *   'family_law_authority' kernel. The constraint coordinates identity and
 *   attachment within religious communities but extracts significantly from
 *   those who do not conform to its strictures, requiring active enforcement
 *   through church discipline and social pressure.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(family_law_authority__christian_canonical_reading, 0.7).
domain_priors:suppression_score(family_law_authority__christian_canonical_reading, 0.8).
domain_priors:theater_ratio(family_law_authority__christian_canonical_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(family_law_authority__christian_canonical_reading, extractiveness, 0.7).
narrative_ontology:constraint_metric(family_law_authority__christian_canonical_reading, suppression_requirement, 0.8).
narrative_ontology:constraint_metric(family_law_authority__christian_canonical_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(family_law_authority__christian_canonical_reading, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(family_law_authority__christian_canonical_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(family_law_authority__christian_canonical_reading, tangled_rope).
narrative_ontology:human_readable(family_law_authority__christian_canonical_reading, "Christian Canonical Reading of Marriage Authority").
narrative_ontology:topic_domain(family_law_authority__christian_canonical_reading, "religious/social/legal").

domain_priors:requires_active_enforcement(family_law_authority__christian_canonical_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(family_law_authority__christian_canonical_reading, '4e763b4b-1c1b-411a-b428-f00f5ec9b9f7').
narrative_ontology:cs_kernel_codification('4e763b4b-1c1b-411a-b428-f00f5ec9b9f7', formalized).
narrative_ontology:cs_authority_grounding('4e763b4b-1c1b-411a-b428-f00f5ec9b9f7', lineage).
narrative_ontology:cs_interpretation_layer_present('4e763b4b-1c1b-411a-b428-f00f5ec9b9f7').
narrative_ontology:cs_reading_relation('4e763b4b-1c1b-411a-b428-f00f5ec9b9f7', family_law_authority__hindu_dharmashastra_reading, coexists_with).
narrative_ontology:cs_reading_relation('4e763b4b-1c1b-411a-b428-f00f5ec9b9f7', family_law_authority__muslim_shariat_reading, coexists_with).
narrative_ontology:cs_reading_relation('4e763b4b-1c1b-411a-b428-f00f5ec9b9f7', family_law_authority__parsi_zoroastrian_reading, coexists_with).
narrative_ontology:cs_reading_relation('4e763b4b-1c1b-411a-b428-f00f5ec9b9f7', family_law_authority__secular_contractual_reading, coexists_with).
narrative_ontology:cs_axiom('4e763b4b-1c1b-411a-b428-f00f5ec9b9f7', foundational, marriage_as_divine_institution).
narrative_ontology:cs_axiom_status(marriage_as_divine_institution, holdable).
narrative_ontology:cs_axiom_grounding('4e763b4b-1c1b-411a-b428-f00f5ec9b9f7', marriage_as_divine_institution, theological).
narrative_ontology:cs_axiom('4e763b4b-1c1b-411a-b428-f00f5ec9b9f7', foundational, indissolubility_of_sacramental_marriage).
narrative_ontology:cs_axiom_status(indissolubility_of_sacramental_marriage, holdable).
narrative_ontology:cs_axiom_grounding('4e763b4b-1c1b-411a-b428-f00f5ec9b9f7', indissolubility_of_sacramental_marriage, deontological).
narrative_ontology:cs_reference_frame('4e763b4b-1c1b-411a-b428-f00f5ec9b9f7', divine_institution_indissoluble_union).
narrative_ontology:cs_drift_state('4e763b4b-1c1b-411a-b428-f00f5ec9b9f7', contemporary_secular_society, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('4e763b4b-1c1b-411a-b428-f00f5ec9b9f7', '').
narrative_ontology:cs_kernel_id(family_law_authority__christian_canonical_reading, family_law_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(family_law_authority__christian_canonical_reading, ecclesiastical_authorities).
narrative_ontology:constraint_beneficiary(family_law_authority__christian_canonical_reading, traditional_families).
narrative_ontology:constraint_beneficiary(family_law_authority__christian_canonical_reading, faithful_adherents).
narrative_ontology:constraint_victim(family_law_authority__christian_canonical_reading, divorced_remarried_individuals).
narrative_ontology:constraint_victim(family_law_authority__christian_canonical_reading, lgbtq_couples).
narrative_ontology:constraint_victim(family_law_authority__christian_canonical_reading, non_conforming_individuals).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(family_law_authority__christian_canonical_reading, faithful_adherents).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Define, interpret, and enforce the canonical understanding of marriage through church law, pastoral guidance, and sacramental discipline. They benefit from the social order and moral authority derived from this framework.
narrative_ontology:constraint_stakeholder(family_law_authority__christian_canonical_reading, ecclesiastical_authorities, agenda_setter,
    institutional, generational, arbitrage, global).

% Adhere to and uphold the canonical definition of marriage, often serving as exemplars within the religious community. They benefit from the social stability, community support, and moral validation provided by the constraint.
narrative_ontology:constraint_stakeholder(family_law_authority__christian_canonical_reading, traditional_families, beneficiary,
    organized, generational, constrained, local).

% Seek to live in accordance with the canonical understanding of marriage, often internalizing its norms. They bear the costs of conforming to strictures (e.g., indissolubility) and face social/spiritual pressure if they deviate, but also receive community and spiritual benefits.
narrative_ontology:constraint_stakeholder(family_law_authority__christian_canonical_reading, faithful_adherents, payer,
    moderate, biographical, identity_locked, local).

% Are often excluded from full participation in religious life (e.g., denied sacraments, leadership roles) if their civil divorce and remarriage are not recognized by ecclesiastical authority. They bear significant social and spiritual costs.
narrative_ontology:constraint_stakeholder(family_law_authority__christian_canonical_reading, divorced_remarried_individuals, payer,
    powerless, biographical, constrained, local).

% Their unions are not recognized as marriage under this canonical reading, and they often face condemnation, exclusion, and denial of religious rites. They are structurally absent from the conversation about the constraint's definition.
narrative_ontology:constraint_stakeholder(family_law_authority__christian_canonical_reading, lgbtq_couples, excluded,
    powerless, biographical, trapped, local).

% Includes individuals who cohabit outside marriage, marry outside the church, or otherwise do not align with the canonical ideal. They may face social stigma, pastoral admonishment, or limited participation in religious community life.
narrative_ontology:constraint_stakeholder(family_law_authority__christian_canonical_reading, non_conforming_individuals, payer,
    powerless, biographical, constrained, local).

% Operate independently of ecclesiastical authority, defining marriage as a civil contract. They observe the impact of religious marriage constraints on their citizens and may legislate in ways that conflict with or accommodate religious definitions.
narrative_ontology:constraint_stakeholder(family_law_authority__christian_canonical_reading, secular_legal_systems, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(family_law_authority__christian_canonical_reading, ecclesiastical_authorities).
narrative_ontology:fixing_cost_class(family_law_authority__christian_canonical_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a divinely ordained, stable, and exclusive union for procreation and mutual support, providing a framework for family life and community identity within Christian traditions.
% TRANSFER_FUNCTION: Transfers social legitimacy, spiritual grace, and community support to those whose unions conform to canonical norms; transfers social exclusion, spiritual censure, and denial of sacraments to those whose unions do not conform.
% ABSENT_VOICES: Advocates for marriage equality, secular definitions of marriage, and individual autonomy in marital choices are structurally excluded from the internal theological and canonical discourse that defines this constraint.
% DISAPPEARANCE_RATIONALE: If this canonical understanding of marriage vanished overnight, the social and spiritual structures of many Christian communities would be profoundly altered, impacting family formation, inheritance, and the foundational role of religious institutions in adherents' lives.
% FOUNDING_PROBLEM: To establish a stable, divinely sanctioned institution for procreation, the raising of children in faith, and the mutual sanctification of spouses, reflecting theological principles and ensuring social order within the religious community.
% FOUNDING_PROBLEM_CORROBORATION: Ecclesiastical authorities and many traditional adherents attest that the founding problem (maintaining a divinely ordered family structure) is still live. However, secular legal systems and human rights advocates attest that the core social functions are now largely addressed by state law, and the religious constraint primarily serves institutional power and traditional identity, leading to significant social costs for non-conforming individuals.
narrative_ontology:disappearance_verdict(family_law_authority__christian_canonical_reading, world_rearranges).
narrative_ontology:founding_problem_status(family_law_authority__christian_canonical_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(family_law_authority__christian_canonical_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(family_law_authority__christian_canonical_reading, 'none', 1).
narrative_ontology:epsilon_provenance(family_law_authority__christian_canonical_reading, 0.7, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(family_law_authority__christian_canonical_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(family_law_authority__christian_canonical_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(family_law_authority__christian_canonical_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The high extractiveness (0.7) reflects the significant social, spiritual, and personal costs imposed on individuals whose marital situations (e.g., divorce and remarriage, same-sex unions) do not align with canonical doctrine. Suppression (0.8) is high due to the institutional power of ecclesiastical authorities to deny sacraments, exclude from community roles, and exert strong social pressure. The moderate theater ratio (0.4) acknowledges genuine pastoral care and theological grounding, but also the performative aspect of maintaining doctrinal purity in the face of internal dissent and external societal shifts. The increasing trend in extractiveness and suppression over time reflects a hardening of positions by some ecclesiastical authorities in response to secularization and internal challenges.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of ecclesiastical authorities and traditional adherents, this constraint is a divinely ordained framework for moral order and spiritual well-being. From the perspective of divorced/remarried individuals, LGBTQ+ couples, and non-conforming individuals, it operates as a deeply extractive and suppressive force that denies recognition and full participation based on immutable or deeply personal life choices. The engine's per-seat classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Ecclesiastical authorities are clear beneficiaries and agenda-setters, defining and enforcing the constraint. Traditional families and faithful adherents also benefit from the social and spiritual order it provides, though adherents also bear costs of conformity. Divorced/remarried individuals, LGBTQ+ couples, and other non-conforming individuals are clear targets, facing exclusion and censure. Secular legal systems act as external observers, often in tension with the religious framework.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    catholic_protestant_divergence,
    'How does the internal divergence between Catholic (indissoluble sacrament) and Protestant (covenant, often permitting divorce) views affect the constraint''s effective extractiveness and suppression?',
    'Comparative analysis of lived experiences and institutional enforcement mechanisms within specific Catholic dioceses versus various Protestant denominations, quantifying differences in social exclusion and access to religious rites.',
    'If the Catholic view is significantly more extractive/suppressive due to its indissolubility, the ''christian_canonical_reading'' might need to be decomposed into two distinct constraints to capture the structural difference in ε. If the differences are primarily in enforcement, it would modulate the effective extraction for specific sub-groups.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(catholic_protestant_divergence, empirical, 'Internal consistency of ''Christian canonical'' given denominational differences on divorce.').

omega_variable(
    sacramental_vs_civil_jurisdiction,
    'To what extent does the claim of divine institution and sacramental nature of marriage conflict with or supersede secular civil law regarding marriage, divorce, and family rights?',
    'Legal analysis of church-state conflicts, court rulings on religious exemptions, and the practical impact on individuals navigating both legal systems (e.g., annulment vs. civil divorce).',
    'If ecclesiastical authority consistently overrides or significantly complicates civil legal processes for adherents, the constraint''s effective suppression and accessibility collapse are higher than if the two systems largely operate in parallel without direct conflict.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sacramental_vs_civil_jurisdiction, conceptual, 'Ambiguity over the ultimate authority in marital matters (religious vs. secular).').

omega_variable(
    internalized_suppression_mechanism,
    'Is the measured suppression primarily structural (denial of sacraments, social exclusion) or internalized (adherents'' belief in the moral necessity of conformity, fear of divine judgment)?',
    'Sociological studies on ex-adherents'' post-exit psychological states and continued adherence to norms, or surveys on internal motivations for conformity versus external pressures.',
    'If internalized suppression is significant, the constraint''s effective suppression is higher than the structural measures suggest, as individuals carry the suppression with them even when external barriers are reduced. This would also impact the difficulty of exit for ''identity_locked'' stakeholders.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(internalized_suppression_mechanism, empirical, 'Structural vs. internalized suppression mechanism for adherents.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(family_law_authority__christian_canonical_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fami_tr_t0, family_law_authority__christian_canonical_reading, theater_ratio, 0, 0.3).
narrative_ontology:measurement(fami_tr_t10, family_law_authority__christian_canonical_reading, theater_ratio, 10, 0.33).
narrative_ontology:measurement(fami_tr_t20, family_law_authority__christian_canonical_reading, theater_ratio, 20, 0.36).
narrative_ontology:measurement(fami_tr_t30, family_law_authority__christian_canonical_reading, theater_ratio, 30, 0.38).
narrative_ontology:measurement(fami_tr_t40, family_law_authority__christian_canonical_reading, theater_ratio, 40, 0.39).
narrative_ontology:measurement(fami_tr_t50, family_law_authority__christian_canonical_reading, theater_ratio, 50, 0.4).

% Extraction over time
narrative_ontology:measurement(fami_be_t0, family_law_authority__christian_canonical_reading, base_extractiveness, 0, 0.6).
narrative_ontology:measurement(fami_be_t10, family_law_authority__christian_canonical_reading, base_extractiveness, 10, 0.63).
narrative_ontology:measurement(fami_be_t20, family_law_authority__christian_canonical_reading, base_extractiveness, 20, 0.66).
narrative_ontology:measurement(fami_be_t30, family_law_authority__christian_canonical_reading, base_extractiveness, 30, 0.68).
narrative_ontology:measurement(fami_be_t40, family_law_authority__christian_canonical_reading, base_extractiveness, 40, 0.69).
narrative_ontology:measurement(fami_be_t50, family_law_authority__christian_canonical_reading, base_extractiveness, 50, 0.7).

% Suppression requirement over time
narrative_ontology:measurement(fami_su_t0, family_law_authority__christian_canonical_reading, suppression_requirement, 0, 0.7).
narrative_ontology:measurement(fami_su_t10, family_law_authority__christian_canonical_reading, suppression_requirement, 10, 0.73).
narrative_ontology:measurement(fami_su_t20, family_law_authority__christian_canonical_reading, suppression_requirement, 20, 0.76).
narrative_ontology:measurement(fami_su_t30, family_law_authority__christian_canonical_reading, suppression_requirement, 30, 0.78).
narrative_ontology:measurement(fami_su_t40, family_law_authority__christian_canonical_reading, suppression_requirement, 40, 0.79).
narrative_ontology:measurement(fami_su_t50, family_law_authority__christian_canonical_reading, suppression_requirement, 50, 0.8).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(family_law_authority__christian_canonical_reading, identity_coordination).
narrative_ontology:affects_constraint(family_law_authority__christian_canonical_reading, family_law_authority__secular_contractual_reading).
narrative_ontology:affects_constraint(family_law_authority__christian_canonical_reading, lgbtq_rights_recognition).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
