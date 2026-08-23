% ============================================================================
% CONSTRAINT STORY: marriage_authority_kernel__christian_canonical_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_marriage_authority_kernel__christian_canonical_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: marriage_authority_kernel__christian_canonical_reading
 *   human_readable: Christian Canonical Marriage Authority (Indian Christian Marriage Act 1872)
 *   domain: legal/religious/constitutional
 *
 * SUMMARY:
 *   The Indian Christian Marriage Act 1872 codifies Anglican canonical
 *   marriage law for Indian Christians, creating a personal law system
 *   recognized by the colonial state and continued post-independence under
 *   Articles 25-26 and the concurrent list. The constraint operates through
 *   two parallel tracks: civil marriage solemnization under the 1872 Act, and
 *   ecclesiastical annulment tribunals operating under canon law (recognized
 *   but not regulated by the state). The Indian Divorce Act 1869 (amended
 *   2001) provides fault-based civil divorce. The system coordinates marriage
 *   for a religious minority but extracts asymmetrically through restrictive
 *   exit rules that disadvantage women. The claimed type is tangled_rope:
 *   genuine coordination function (community marriage framework) with
 *   asymmetric extraction (gendered fault requirements, clerical control of
 *   annulment).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(marriage_authority_kernel__christian_canonical_reading, 0.58).
domain_priors:suppression_score(marriage_authority_kernel__christian_canonical_reading, 0.62).
domain_priors:theater_ratio(marriage_authority_kernel__christian_canonical_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(marriage_authority_kernel__christian_canonical_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(marriage_authority_kernel__christian_canonical_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(marriage_authority_kernel__christian_canonical_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(marriage_authority_kernel__christian_canonical_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(marriage_authority_kernel__christian_canonical_reading, resistance, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(marriage_authority_kernel__christian_canonical_reading, tangled_rope).
narrative_ontology:human_readable(marriage_authority_kernel__christian_canonical_reading, "Christian Canonical Marriage Authority (Indian Christian Marriage Act 1872)").
narrative_ontology:topic_domain(marriage_authority_kernel__christian_canonical_reading, "legal/religious/constitutional").

domain_priors:requires_active_enforcement(marriage_authority_kernel__christian_canonical_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(marriage_authority_kernel__christian_canonical_reading, '1c7bfb59-e8b8-4d68-92b0-d49397f701b2').
narrative_ontology:cs_kernel_codification('1c7bfb59-e8b8-4d68-92b0-d49397f701b2', formalized).
narrative_ontology:cs_authority_grounding('1c7bfb59-e8b8-4d68-92b0-d49397f701b2', lineage).
narrative_ontology:cs_interpretation_layer_present('1c7bfb59-e8b8-4d68-92b0-d49397f701b2').
narrative_ontology:cs_reading_relation('1c7bfb59-e8b8-4d68-92b0-d49397f701b2', marriage_authority_kernel__hindu_codified_reading, coexists_with).
narrative_ontology:cs_reading_relation('1c7bfb59-e8b8-4d68-92b0-d49397f701b2', marriage_authority_kernel__muslim_shariat_reading, coexists_with).
narrative_ontology:cs_reading_relation('1c7bfb59-e8b8-4d68-92b0-d49397f701b2', marriage_authority_kernel__parsi_communal_reading, coexists_with).
narrative_ontology:cs_reading_relation('1c7bfb59-e8b8-4d68-92b0-d49397f701b2', marriage_authority_kernel__secular_civil_reading, influences).
narrative_ontology:cs_axiom('1c7bfb59-e8b8-4d68-92b0-d49397f701b2', foundational, marriage_as_indissoluble_sacrament).
narrative_ontology:cs_axiom_status(marriage_as_indissoluble_sacrament, holdable).
narrative_ontology:cs_axiom_grounding('1c7bfb59-e8b8-4d68-92b0-d49397f701b2', marriage_as_indissoluble_sacrament, deontological).
narrative_ontology:cs_axiom('1c7bfb59-e8b8-4d68-92b0-d49397f701b2', foundational, ecclesiastical_tribunal_exclusive_competence_annulment).
narrative_ontology:cs_axiom_status(ecclesiastical_tribunal_exclusive_competence_annulment, holdable).
narrative_ontology:cs_axiom_grounding('1c7bfb59-e8b8-4d68-92b0-d49397f701b2', ecclesiastical_tribunal_exclusive_competence_annulment, conventional).
narrative_ontology:cs_axiom('1c7bfb59-e8b8-4d68-92b0-d49397f701b2', secondary, fault_based_divorce_as_moral_necessity).
narrative_ontology:cs_axiom_status(fault_based_divorce_as_moral_necessity, holdable).
narrative_ontology:cs_axiom_grounding('1c7bfb59-e8b8-4d68-92b0-d49397f701b2', fault_based_divorce_as_moral_necessity, deontological).
narrative_ontology:cs_reference_frame('1c7bfb59-e8b8-4d68-92b0-d49397f701b2', anglican_canonical_marriage_1872).
narrative_ontology:cs_drift_state('1c7bfb59-e8b8-4d68-92b0-d49397f701b2', post_independence_constitutional_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('1c7bfb59-e8b8-4d68-92b0-d49397f701b2', '').
narrative_ontology:cs_kernel_id(marriage_authority_kernel__christian_canonical_reading, marriage_authority_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(marriage_authority_kernel__christian_canonical_reading, church_ecclesiastical_authorities).
narrative_ontology:constraint_beneficiary(marriage_authority_kernel__christian_canonical_reading, christian_male_household_heads).
narrative_ontology:constraint_victim(marriage_authority_kernel__christian_canonical_reading, christian_women_seeking_divorce).
narrative_ontology:constraint_victim(marriage_authority_kernel__christian_canonical_reading, christian_spouses_in_abusive_marriages).
narrative_ontology:constraint_victim(marriage_authority_kernel__christian_canonical_reading, interfaith_couples_seeking_civil_remarriage).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(marriage_authority_kernel__christian_canonical_reading, christian_laity_reform_advocates).
narrative_ontology:constraint_victim(marriage_authority_kernel__christian_canonical_reading, christian_laity_reform_advocates).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administer marriage tribunals, grant annulments on canonical grounds, and maintain doctrinal control over marriage validity. Their authority is recognized by the 1872 Act and subsequent amendments. They set the procedural and substantive rules for annulment petitions.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__christian_canonical_reading, church_ecclesiastical_authorities, agenda_setter,
    institutional, generational, analytical, national).

% Benefit from fault-based divorce framework that makes unilateral exit difficult for wives; retain control over marital property and child custody under canonical presumptions. Can access civil divorce under Indian Divorce Act 1869 (amended 2001) but canonical annulment remains primary religious remedy.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__christian_canonical_reading, christian_male_household_heads, beneficiary,
    organized, biographical, mobile, local).
narrative_ontology:stakeholder_secondary_role(marriage_authority_kernel__christian_canonical_reading, christian_male_household_heads, agenda_setter).

% Face procedural and evidentiary barriers in both canonical annulment (requiring proof of canonical defect at inception) and civil fault-based divorce (cruelty, adultery, desertion). Limited exit: civil divorce available but socially stigmatized; canonical remarriage blocked without annulment. Economic dependency often compounds exit difficulty.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__christian_canonical_reading, christian_women_seeking_divorce, payer,
    moderate, biographical, constrained, local).

% Experience the constraint's suppression most acutely: fault requirements demand public proof of specific acts; canonical process is slow, opaque, and controlled by clergy; civil remedies exist but require navigating parallel legal systems. Children and property ties deepen entrapment.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__christian_canonical_reading, christian_spouses_in_abusive_marriages, payer,
    powerless, immediate, trapped, local).

% Cannot access Christian canonical marriage without conversion; canonical annulment of prior marriage required for religious remarriage but civil divorce may not satisfy canonical grounds. Caught between personal law systems with no unified exit pathway.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__christian_canonical_reading, interfaith_couples_seeking_civil_remarriage, excluded,
    moderate, biographical, constrained, national).

% Administer the Indian Divorce Act 1869 (amended 2001) which provides civil divorce for Christians; Supreme Court has read down gender-asymmetric provisions. Legislature retains power to reform but defers to community consensus. Judiciary mediates between canonical norms and constitutional equality guarantees.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__christian_canonical_reading, state_courts_and_legislature, observer,
    institutional, generational, analytical, national).

% Organize within community for gender-equitable reform of personal law; benefit from community membership but pay costs of internal opposition. Push for mutual consent divorce, equal property rights, and canonical process transparency. Their voice is contested by ecclesiastical authorities as unrepresentative.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__christian_canonical_reading, christian_laity_reform_advocates, beneficiary,
    organized, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(marriage_authority_kernel__christian_canonical_reading, christian_laity_reform_advocates, payer).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a recognized marital framework for the Indian Christian community: solemnization, registration, and religious validity of marriage; ecclesiastical tribunals for annulment; a shared normative language for family life across denominations.
% TRANSFER_FUNCTION: Transfers exit autonomy from individual spouses (especially wives) to ecclesiastical authorities and male household heads: fault-based divorce requires proving specific marital offenses; annulment requires proving canonical defect at inception; both processes are controlled by church tribunals, not the parties.
% ABSENT_VOICES: Christian women in rural and low-income communities who lack access to legal representation and canonical process; LGBTQ+ Christians for whom the sacramental marriage framework is structurally exclusive; Christian converts from other faiths whose prior marriages create canonical impediments. These voices are absent from synodal deliberations and legislative consultations.
% DISAPPEARANCE_RATIONALE: If the canonical marriage framework and its state recognition vanished overnight, Christian couples would fall back on the Special Marriage Act 1954 for civil marriage; ecclesiastical annulment would lose civil effect; property, custody, and succession would be governed by secular law. The community would lose its distinct personal law identity but gain gender-equitable exit rights.
% FOUNDING_PROBLEM: After the 1857 rebellion, the British Raj needed a marriage law for Indian Christians that respected their religious consciousness while providing legal certainty for property and succession. The 1872 Act codified Anglican canonical norms for a diverse Indian Christian population, replacing missionary-administered informal practices.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem (legal certainty for a missionary-era community) is attested by colonial legislative records and Anglican mission archives. Christian reform groups and women's organizations attest the problem is dead: the community is now diverse, indigenous, and constitutionally entitled to equality. Ecclesiastical authorities attest it is live: canonical marriage remains a sacramental reality requiring protection. No neutral arbiter has settled this.
narrative_ontology:disappearance_verdict(marriage_authority_kernel__christian_canonical_reading, world_rearranges).
narrative_ontology:founding_problem_status(marriage_authority_kernel__christian_canonical_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(marriage_authority_kernel__christian_canonical_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(marriage_authority_kernel__christian_canonical_reading, 'none', 1).
narrative_ontology:epsilon_provenance(marriage_authority_kernel__christian_canonical_reading, 0.58, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(marriage_authority_kernel__christian_canonical_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(marriage_authority_kernel__christian_canonical_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(marriage_authority_kernel__christian_canonical_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58) reflects the transfer of exit autonomy to ecclesiastical authorities and male household heads. Suppression (0.62) captures the active enforcement: canonical defects must be proven before church tribunals; civil fault grounds require public litigation; social stigma deters exit. Theater ratio (0.28) is moderate: the coordination function (solemnization, community recognition) is real, but a growing share of enforcement energy defends the fault-based structure against reform. Accessibility collapse (0.55) and resistance (0.48) reflect that alternatives exist (Special Marriage Act, civil divorce) but are socially and religiously costly. Measurements show gradual extraction accumulation post-independence as constitutional equality jurisprudence conflicts with static canonical norms.
 *
 * PERSPECTIVAL GAP:
 *   From the ecclesiastical seat, the constraint is a rope: it coordinates sacramental marriage for a minority community under constitutional protection. From the payer seats (women seeking exit), it is a snare: fault requirements and clerical gatekeeping extract autonomy. The engine computes this divergence from the structural data — the canonical coordination story and the gendered extraction story operate simultaneously through the same tribunals.
 *
 * DIRECTIONALITY LOGIC:
 *   Church authorities are structural beneficiaries (d near 0.15): they control the annulment process, collect fees, maintain jurisdictional authority. Male household heads are beneficiaries (d ~0.25): fault rules favor husbands who control evidence and resources. Women seeking divorce are targets (d ~0.85): bear procedural, evidentiary, and social costs; exit options are constrained by economic dependency and canonical remarriage bars. Spouses in abusive marriages are trapped targets (d ~0.95). Interfaith couples are excluded (d ~0.9). State courts are analytical observers (d=0.5). Reform advocates are dual-positioned: benefit from community but pay costs of opposition.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (colonial legal certainty for a missionary community) is substantially dead — the community is now indigenous, diverse, and constitutionally equal. Yet the arrangement persists because ecclesiastical authorities extract jurisdictional rents and male household heads extract patriarchal control. The mandatrophy is unresolved: the coordination function could be served by a gender-equitable civil framework (Special Marriage Act), but the extraction function blocks reform. The constraint is not a piton — it is actively defended, not merely performed.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is this constraint a genuine coordination mechanism for a religious minority, or an extractive structure that uses religious identity as a cover for gendered control?',
    'Comparative analysis of exit outcomes: if Christian women use the Special Marriage Act at rates significantly higher than other communities'' women use their personal laws, the canonical framework is extractive. If reform from within (e.g., mutual consent divorce in canon law) gains traction, coordination function is genuine.',
    'If extractive, the constraint is a snare masquerading as a tangled_rope; if coordination is genuine, the tangled_rope classification holds but extraction must be reduced via reform.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Whether the Christian canonical reading''s coordination function is genuine or a cover for extraction.').

omega_variable(
    canonical_civil_parallel_operation,
    'Do the canonical annulment tribunals and civil divorce courts operate as complementary forums or as competing jurisdictions that trap parties in forum-shopping?',
    'Empirical study of litigant trajectories: track whether parties must pursue both canonical annulment and civil divorce sequentially, and whether outcomes conflict.',
    'If complementary, extraction is lower (parties choose forum). If competing, suppression is higher (double burden, conflicting orders).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(canonical_civil_parallel_operation, empirical, 'Relationship between ecclesiastical and civil marriage dissolution fora.').

omega_variable(
    state_recognition_legitimacy,
    'Does state recognition of canonical annulment (for civil effects) constitute establishment of religion, or permissible accommodation of minority rights?',
    'Supreme Court jurisprudence on Articles 25-26 vs. Article 14/15; legislative history of the 2001 amendment.',
    'If establishment, the constraint''s legitimacy collapses constitutionally. If accommodation, reform must come from within the community.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(state_recognition_legitimacy, conceptual, 'Constitutional legitimacy of state recognition of ecclesiastical tribunals.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(marriage_authority_kernel__christian_canonical_reading, 1872, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(marriage_authority_christian_tr_t1872, marriage_authority_kernel__christian_canonical_reading, theater_ratio, 1872, 0.15).
narrative_ontology:measurement(marriage_authority_christian_tr_t1910, marriage_authority_kernel__christian_canonical_reading, theater_ratio, 1910, 0.18).
narrative_ontology:measurement(marriage_authority_christian_tr_t1947, marriage_authority_kernel__christian_canonical_reading, theater_ratio, 1947, 0.22).
narrative_ontology:measurement(marriage_authority_christian_tr_t1976, marriage_authority_kernel__christian_canonical_reading, theater_ratio, 1976, 0.25).
narrative_ontology:measurement(marriage_authority_christian_tr_t2001, marriage_authority_kernel__christian_canonical_reading, theater_ratio, 2001, 0.27).
narrative_ontology:measurement(marriage_authority_christian_tr_t2024, marriage_authority_kernel__christian_canonical_reading, theater_ratio, 2024, 0.28).

% Extraction over time
narrative_ontology:measurement(marriage_authority_christian_be_t1872, marriage_authority_kernel__christian_canonical_reading, base_extractiveness, 1872, 0.45).
narrative_ontology:measurement(marriage_authority_christian_be_t1910, marriage_authority_kernel__christian_canonical_reading, base_extractiveness, 1910, 0.48).
narrative_ontology:measurement(marriage_authority_christian_be_t1947, marriage_authority_kernel__christian_canonical_reading, base_extractiveness, 1947, 0.52).
narrative_ontology:measurement(marriage_authority_christian_be_t1976, marriage_authority_kernel__christian_canonical_reading, base_extractiveness, 1976, 0.55).
narrative_ontology:measurement(marriage_authority_christian_be_t2001, marriage_authority_kernel__christian_canonical_reading, base_extractiveness, 2001, 0.57).
narrative_ontology:measurement(marriage_authority_christian_be_t2024, marriage_authority_kernel__christian_canonical_reading, base_extractiveness, 2024, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(marriage_authority_christian_su_t1872, marriage_authority_kernel__christian_canonical_reading, suppression_requirement, 1872, 0.55).
narrative_ontology:measurement(marriage_authority_christian_su_t1910, marriage_authority_kernel__christian_canonical_reading, suppression_requirement, 1910, 0.58).
narrative_ontology:measurement(marriage_authority_christian_su_t1947, marriage_authority_kernel__christian_canonical_reading, suppression_requirement, 1947, 0.6).
narrative_ontology:measurement(marriage_authority_christian_su_t1976, marriage_authority_kernel__christian_canonical_reading, suppression_requirement, 1976, 0.61).
narrative_ontology:measurement(marriage_authority_christian_su_t2001, marriage_authority_kernel__christian_canonical_reading, suppression_requirement, 2001, 0.61).
narrative_ontology:measurement(marriage_authority_christian_su_t2024, marriage_authority_kernel__christian_canonical_reading, suppression_requirement, 2024, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(marriage_authority_kernel__christian_canonical_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(marriage_authority_kernel__christian_canonical_reading, 0.08).
narrative_ontology:affects_constraint(marriage_authority_kernel__christian_canonical_reading, marriage_authority_kernel__hindu_codified_reading).
narrative_ontology:affects_constraint(marriage_authority_kernel__christian_canonical_reading, marriage_authority_kernel__muslim_shariat_reading).
narrative_ontology:affects_constraint(marriage_authority_kernel__christian_canonical_reading, marriage_authority_kernel__parsi_communal_reading).
narrative_ontology:affects_constraint(marriage_authority_kernel__christian_canonical_reading, marriage_authority_kernel__secular_civil_reading).
narrative_ontology:affects_constraint(marriage_authority_kernel__christian_canonical_reading, special_marriage_act_1954).
narrative_ontology:affects_constraint(marriage_authority_kernel__christian_canonical_reading, indian_divorce_act_1869_amended_2001).

% DUAL FORMULATION NOTE:
% This reading and its siblings decompose the 'Indian personal law' label into structurally distinct constraints. The Christian reading's ε (0.58) differs from the secular civil reading's ε (~0.15) because the former maintains fault-based exit and ecclesiastical gatekeeping while the latter provides mutual consent divorce. They are linked because the secular option is the exit alternative for all personal law subjects, and because Supreme Court jurisprudence on one personal law affects others (cross-index coupling).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(marriage_authority_kernel__christian_canonical_reading, institutional, 0.15).
constraint_indexing:directionality_override(marriage_authority_kernel__christian_canonical_reading, organized, 0.25).
constraint_indexing:directionality_override(marriage_authority_kernel__christian_canonical_reading, moderate, 0.85).
constraint_indexing:directionality_override(marriage_authority_kernel__christian_canonical_reading, powerless, 0.95).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
