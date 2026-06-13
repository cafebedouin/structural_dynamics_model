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
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: family_law_authority__christian_canonical_reading
 *   human_readable: Christian Canonical Marriage Authority
 *   domain: comparative_law/political_theory/religious_governance
 *
 * SUMMARY:
 *   This constraint describes marriage as defined and governed by Christian
 *   canonical authority, encompassing both the Catholic understanding of
 *   marriage as an indissoluble sacrament and the varied denominational
 *   governance in Protestant traditions that may permit divorce under
 *   specific conditions. It is one reading of the broader
 *   'family_law_authority' kernel, which includes diverse religious and
 *   secular interpretations. The constraint's persistence relies on active
 *   enforcement by ecclesiastical bodies, which define validity, dissolution,
 *   and remarriage within their respective faith communities.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(family_law_authority__christian_canonical_reading, 0.6).
domain_priors:suppression_score(family_law_authority__christian_canonical_reading, 0.7).
domain_priors:theater_ratio(family_law_authority__christian_canonical_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(family_law_authority__christian_canonical_reading, extractiveness, 0.6).
narrative_ontology:constraint_metric(family_law_authority__christian_canonical_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(family_law_authority__christian_canonical_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(family_law_authority__christian_canonical_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(family_law_authority__christian_canonical_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(family_law_authority__christian_canonical_reading, tangled_rope).
narrative_ontology:human_readable(family_law_authority__christian_canonical_reading, "Christian Canonical Marriage Authority").
narrative_ontology:topic_domain(family_law_authority__christian_canonical_reading, "comparative_law/political_theory/religious_governance").

domain_priors:requires_active_enforcement(family_law_authority__christian_canonical_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(family_law_authority__christian_canonical_reading, 'c71866e2-d225-4464-85ba-f1fa38131835').
narrative_ontology:cs_kernel_codification('c71866e2-d225-4464-85ba-f1fa38131835', fixed_text).
narrative_ontology:cs_authority_grounding('c71866e2-d225-4464-85ba-f1fa38131835', lineage).
narrative_ontology:cs_interpretation_layer_present('c71866e2-d225-4464-85ba-f1fa38131835').
narrative_ontology:cs_reading_relation('c71866e2-d225-4464-85ba-f1fa38131835', family_law_authority__hindu_dharmashastra_reading, coexists_with).
narrative_ontology:cs_reading_relation('c71866e2-d225-4464-85ba-f1fa38131835', family_law_authority__muslim_shariat_reading, coexists_with).
narrative_ontology:cs_reading_relation('c71866e2-d225-4464-85ba-f1fa38131835', family_law_authority__parsi_zoroastrian_reading, coexists_with).
narrative_ontology:cs_reading_relation('c71866e2-d225-4464-85ba-f1fa38131835', family_law_authority__secular_contractual_reading, coexists_with).
narrative_ontology:cs_axiom('c71866e2-d225-4464-85ba-f1fa38131835', foundational, marriage_as_sacrament_or_divine_institution).
narrative_ontology:cs_axiom_status(marriage_as_sacrament_or_divine_institution, holdable).
narrative_ontology:cs_axiom_grounding('c71866e2-d225-4464-85ba-f1fa38131835', marriage_as_sacrament_or_divine_institution, theological).
narrative_ontology:cs_axiom('c71866e2-d225-4464-85ba-f1fa38131835', foundational, ecclesiastical_authority_over_marital_validity).
narrative_ontology:cs_axiom_status(ecclesiastical_authority_over_marital_validity, holdable).
narrative_ontology:cs_axiom_grounding('c71866e2-d225-4464-85ba-f1fa38131835', ecclesiastical_authority_over_marital_validity, conventional).
narrative_ontology:cs_reference_frame('c71866e2-d225-4464-85ba-f1fa38131835', early_church_sacramental_doctrine).
narrative_ontology:cs_drift_state('c71866e2-d225-4464-85ba-f1fa38131835', contemporary_secular_era, gap(authority_erosion, substantial, true)).
narrative_ontology:cs_created_at('c71866e2-d225-4464-85ba-f1fa38131835', '').
narrative_ontology:cs_kernel_id(family_law_authority__christian_canonical_reading, family_law_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(family_law_authority__christian_canonical_reading, ecclesiastical_authorities).
narrative_ontology:constraint_beneficiary(family_law_authority__christian_canonical_reading, denominational_governance_bodies).
narrative_ontology:constraint_victim(family_law_authority__christian_canonical_reading, married_individuals_seeking_divorce).
narrative_ontology:constraint_victim(family_law_authority__christian_canonical_reading, individuals_seeking_remarriage).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(family_law_authority__christian_canonical_reading, christian_communities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Catholic Church hierarchy and other Christian denominational leaders who define, interpret, and enforce the canonical understanding of marriage. They derive authority from tradition and scripture, maintaining the sacramental nature of marriage and its indissolubility (Catholic) or specific conditions for dissolution (Protestant).
narrative_ontology:constraint_stakeholder(family_law_authority__christian_canonical_reading, ecclesiastical_authorities, agenda_setter,
    institutional, generational, identity_locked, global).

% Individuals who, having married under Christian canonical authority, seek to dissolve their marriage. They face significant social, spiritual, and sometimes legal barriers (e.g., annulment processes, excommunication, or denial of remarriage within the church) if their request does not align with canonical rules.
narrative_ontology:constraint_stakeholder(family_law_authority__christian_canonical_reading, married_individuals_seeking_divorce, payer,
    powerless, biographical, constrained, local).

% Individuals who have divorced under civil law but are prevented from remarrying within their Christian denomination due to canonical restrictions. They may face social stigma, exclusion from sacraments, or be forced to leave their faith community to remarry.
narrative_ontology:constraint_stakeholder(family_law_authority__christian_canonical_reading, individuals_seeking_remarriage, payer,
    powerless, biographical, constrained, local).

% Protestant church councils, synods, and other bodies that establish and uphold denominational marriage doctrines. They benefit from the stability and moral authority derived from these doctrines, which reinforce community identity and adherence to religious norms.
narrative_ontology:constraint_stakeholder(family_law_authority__christian_canonical_reading, denominational_governance_bodies, beneficiary,
    organized, generational, identity_locked, national).

% State legal systems that define marriage as a civil contract and permit divorce. While they may recognize religious marriages, they do not defer to ecclesiastical authority on matters of dissolution or validity, creating a parallel, often conflicting, legal framework. They are excluded from the internal governance of canonical marriage.
narrative_ontology:constraint_stakeholder(family_law_authority__christian_canonical_reading, secular_legal_systems, excluded,
    institutional, generational, mobile, national).

% Congregations and broader communities that uphold the canonical understanding of marriage. They benefit from the perceived moral order, stability, and shared identity that these doctrines provide, reinforcing communal bonds and values.
narrative_ontology:constraint_stakeholder(family_law_authority__christian_canonical_reading, christian_communities, beneficiary,
    organized, generational, identity_locked, local).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the understanding and practice of marriage within Christian communities, providing a stable framework for family formation, moral conduct, and spiritual life, ensuring consistency with theological doctrines.
% TRANSFER_FUNCTION: Transfers authority over marital status and family structure from individuals to ecclesiastical bodies. It also transfers social and spiritual capital to those who conform to canonical norms, and imposes costs (social exclusion, spiritual penalties) on those who do not.
% ABSENT_VOICES: Individuals who reject the theological basis of marriage or seek full autonomy over their marital status are structurally excluded from the canonical discourse. They would argue for marriage as a purely civil or personal matter, free from religious strictures.
% DISAPPEARANCE_RATIONALE: If Christian canonical marriage authority vanished overnight, the internal governance of Christian denominations would be profoundly altered. Many individuals would seek civil divorce and remarriage without ecclesiastical impediment, leading to a significant shift in family structures and religious practice within these communities.
% FOUNDING_PROBLEM: To establish a divinely ordained, stable, and morally upright institution for procreation and family life, distinct from secular or pagan practices, and to provide spiritual guidance for marital relationships.
% FOUNDING_PROBLEM_CORROBORATION: Ecclesiastical authorities and many adherents attest that the founding problem of maintaining a sacred and stable marital institution in a changing world is still live. Critics (e.g., secular legal scholars, human rights advocates) argue that while the problem of family stability remains, the canonical solution is outdated and imposes undue burdens, but they do not deny the original intent.
narrative_ontology:disappearance_verdict(family_law_authority__christian_canonical_reading, world_rearranges).
narrative_ontology:founding_problem_status(family_law_authority__christian_canonical_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(family_law_authority__christian_canonical_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(family_law_authority__christian_canonical_reading, 'none', 1).

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
 *   Extractiveness is moderate-high (0.6) because individuals seeking divorce or remarriage outside canonical norms face significant costs, including social exclusion and spiritual penalties. Suppression is high (0.7) due to the strong social and spiritual pressure exerted by religious communities and authorities, making exit or non-compliance difficult for identity-locked individuals. Theater ratio is low (0.2) as the enforcement of canonical marriage is a core, active function of ecclesiastical bodies, not merely performative. The historical measurements reflect a period of increasing ecclesiastical control and later, a slight decrease in extractiveness and suppression as secular alternatives became more prevalent, but the core function remains.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of ecclesiastical authorities, this constraint is a necessary 'rope' for spiritual and social order. From the perspective of individuals seeking divorce or remarriage, it operates as a 'snare' due to the high costs and limited exit options. The engine's computation of per-seat classification will reflect this divergence based on the declared structural relationships.
 *
 * DIRECTIONALITY LOGIC:
 *   Ecclesiastical authorities and denominational governance bodies are primary beneficiaries (d near 0.0) as they maintain their authority and the stability of their communities. Married individuals seeking divorce or remarriage are targets (d near 1.0) as they bear the costs of non-compliance. Christian communities are beneficiaries through the perceived moral order and shared identity. Secular legal systems are excluded, as their authority is not recognized in this canonical framework.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sacramental_vs_contractual_nature,
    'Is marriage fundamentally a sacrament (divinely instituted and indissoluble) or a civil contract (human-made and dissoluble)?',
    'This is a conceptual omega, resolvable only by theological or philosophical commitment, not empirical data. Resolution would depend on the adopted foundational worldview.',
    'If resolved as purely contractual, the ecclesiastical authority''s claims to govern dissolution would be undermined, reclassifying the constraint closer to a snare for individuals seeking exit. If resolved as purely sacramental, the constraint''s ''rope'' aspects for believers would be reinforced.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sacramental_vs_contractual_nature, conceptual, 'The fundamental nature of marriage as sacrament vs. contract.').

omega_variable(
    ecclesiastical_vs_secular_jurisdiction,
    'To what extent should ecclesiastical authority supersede or coexist with secular legal systems in defining and governing marriage?',
    'Legal and political developments, such as court rulings on religious freedom vs. state authority, or legislative changes regarding the recognition of religious divorces. Empirical observation of jurisdictional conflicts and their outcomes.',
    'If secular jurisdiction is deemed supreme, the constraint''s suppression and extractiveness would decrease for individuals, as civil alternatives would be fully recognized. If ecclesiastical jurisdiction is upheld, the constraint''s force would be amplified.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ecclesiastical_vs_secular_jurisdiction, empirical, 'The boundary of authority between religious and state law in marriage.').

omega_variable(
    denominational_variance_impact,
    'How does the variance in divorce permissibility across Christian denominations (e.g., Catholic indissolubility vs. some Protestant allowances) affect the overall extractiveness and suppression of this reading?',
    'Comparative analysis of exit options and social costs for individuals in different Christian denominations, quantifying the differential impact of their specific canonical rules.',
    'If denominational variance creates meaningful ''arbitrage'' opportunities for individuals to switch denominations for easier divorce/remarriage, the overall suppression and extractiveness of the ''Christian canonical'' reading would be lower than if all denominations enforced strict indissolubility.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(denominational_variance_impact, empirical, 'Impact of denominational differences on constraint severity.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(family_law_authority__christian_canonical_reading, 100, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fami_tr_t100, family_law_authority__christian_canonical_reading, theater_ratio, 100, 0.1).
narrative_ontology:measurement(fami_tr_t500, family_law_authority__christian_canonical_reading, theater_ratio, 500, 0.1).
narrative_ontology:measurement(fami_tr_t1000, family_law_authority__christian_canonical_reading, theater_ratio, 1000, 0.15).
narrative_ontology:measurement(fami_tr_t1500, family_law_authority__christian_canonical_reading, theater_ratio, 1500, 0.2).
narrative_ontology:measurement(fami_tr_t1800, family_law_authority__christian_canonical_reading, theater_ratio, 1800, 0.25).
narrative_ontology:measurement(fami_tr_t2024, family_law_authority__christian_canonical_reading, theater_ratio, 2024, 0.2).

% Extraction over time
narrative_ontology:measurement(fami_be_t100, family_law_authority__christian_canonical_reading, base_extractiveness, 100, 0.4).
narrative_ontology:measurement(fami_be_t500, family_law_authority__christian_canonical_reading, base_extractiveness, 500, 0.5).
narrative_ontology:measurement(fami_be_t1000, family_law_authority__christian_canonical_reading, base_extractiveness, 1000, 0.6).
narrative_ontology:measurement(fami_be_t1500, family_law_authority__christian_canonical_reading, base_extractiveness, 1500, 0.65).
narrative_ontology:measurement(fami_be_t1800, family_law_authority__christian_canonical_reading, base_extractiveness, 1800, 0.7).
narrative_ontology:measurement(fami_be_t2024, family_law_authority__christian_canonical_reading, base_extractiveness, 2024, 0.6).

% Suppression requirement over time
narrative_ontology:measurement(fami_su_t100, family_law_authority__christian_canonical_reading, suppression_requirement, 100, 0.5).
narrative_ontology:measurement(fami_su_t500, family_law_authority__christian_canonical_reading, suppression_requirement, 500, 0.6).
narrative_ontology:measurement(fami_su_t1000, family_law_authority__christian_canonical_reading, suppression_requirement, 1000, 0.7).
narrative_ontology:measurement(fami_su_t1500, family_law_authority__christian_canonical_reading, suppression_requirement, 1500, 0.8).
narrative_ontology:measurement(fami_su_t1800, family_law_authority__christian_canonical_reading, suppression_requirement, 1800, 0.85).
narrative_ontology:measurement(fami_su_t2024, family_law_authority__christian_canonical_reading, suppression_requirement, 2024, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(family_law_authority__christian_canonical_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(family_law_authority__christian_canonical_reading, 0.08).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'family_law_authority' kernel, focusing on Christian canonical interpretations. It is structurally distinct from other religious and secular readings of marriage.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
