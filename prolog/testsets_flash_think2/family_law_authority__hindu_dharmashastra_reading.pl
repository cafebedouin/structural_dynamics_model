% ============================================================================
% CONSTRAINT STORY: family_law_authority__hindu_dharmashastra_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_family_law_authority__hindu_dharmashastra_reading, []).

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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: family_law_authority__hindu_dharmashastra_reading
 *   human_readable: Hindu Marriage as Sacramental Samskara (Dharmashastra Reading)
 *   domain: comparative_law/political_theory/religious_governance
 *
 * SUMMARY:
 *   This constraint story analyzes marriage within Hindu tradition as a
 *   sacramental samskara (rite of passage) governed by Dharmashastra texts
 *   and customary practice, representing one reading of the broader
 *   'family_law_authority' kernel. Historically, this reading emphasized
 *   indissolubility, caste endogamy, joint family property rules, and the
 *   wife's role as a ritual participant rather than an autonomous contractor.
 *   While the Hindu Marriage Act of 1955 introduced significant legal
 *   reforms, this reading continues to influence social norms and personal
 *   commitments for many adherents, leading to a persistent, albeit evolving,
 *   structure of coordination and extraction.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(family_law_authority__hindu_dharmashastra_reading, 0.85).
domain_priors:suppression_score(family_law_authority__hindu_dharmashastra_reading, 0.9).
domain_priors:theater_ratio(family_law_authority__hindu_dharmashastra_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(family_law_authority__hindu_dharmashastra_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(family_law_authority__hindu_dharmashastra_reading, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(family_law_authority__hindu_dharmashastra_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(family_law_authority__hindu_dharmashastra_reading, accessibility_collapse, 0.9).
narrative_ontology:constraint_metric(family_law_authority__hindu_dharmashastra_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(family_law_authority__hindu_dharmashastra_reading, tangled_rope).
narrative_ontology:human_readable(family_law_authority__hindu_dharmashastra_reading, "Hindu Marriage as Sacramental Samskara (Dharmashastra Reading)").
narrative_ontology:topic_domain(family_law_authority__hindu_dharmashastra_reading, "comparative_law/political_theory/religious_governance").

domain_priors:requires_active_enforcement(family_law_authority__hindu_dharmashastra_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(family_law_authority__hindu_dharmashastra_reading, 'e401828f-59a2-401e-925a-560b30ace859').
narrative_ontology:cs_kernel_codification('e401828f-59a2-401e-925a-560b30ace859', fixed_text).
narrative_ontology:cs_authority_grounding('e401828f-59a2-401e-925a-560b30ace859', lineage).
narrative_ontology:cs_interpretation_layer_present('e401828f-59a2-401e-925a-560b30ace859').
narrative_ontology:cs_reading_relation('e401828f-59a2-401e-925a-560b30ace859', family_law_authority__muslim_shariat_reading, coexists_with).
narrative_ontology:cs_reading_relation('e401828f-59a2-401e-925a-560b30ace859', family_law_authority__christian_canonical_reading, coexists_with).
narrative_ontology:cs_reading_relation('e401828f-59a2-401e-925a-560b30ace859', family_law_authority__parsi_zoroastrian_reading, coexists_with).
narrative_ontology:cs_reading_relation('e401828f-59a2-401e-925a-560b30ace859', family_law_authority__secular_contractual_reading, influences).
narrative_ontology:cs_axiom('e401828f-59a2-401e-925a-560b30ace859', foundational, marriage_as_indissoluble_samskara).
narrative_ontology:cs_axiom_status(marriage_as_indissoluble_samskara, holdable).
narrative_ontology:cs_axiom_grounding('e401828f-59a2-401e-925a-560b30ace859', marriage_as_indissoluble_samskara, theological).
narrative_ontology:cs_axiom('e401828f-59a2-401e-925a-560b30ace859', foundational, caste_endogamy_as_dharma).
narrative_ontology:cs_axiom_status(caste_endogamy_as_dharma, holdable).
narrative_ontology:cs_axiom_grounding('e401828f-59a2-401e-925a-560b30ace859', caste_endogamy_as_dharma, conventional).
narrative_ontology:cs_reference_frame('e401828f-59a2-401e-925a-560b30ace859', traditional_dharmic_order).
narrative_ontology:cs_drift_state('e401828f-59a2-401e-925a-560b30ace859', post_1955_legal_reforms, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('e401828f-59a2-401e-925a-560b30ace859', '').
narrative_ontology:cs_kernel_id(family_law_authority__hindu_dharmashastra_reading, family_law_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(family_law_authority__hindu_dharmashastra_reading, hindu_joint_family).
narrative_ontology:constraint_beneficiary(family_law_authority__hindu_dharmashastra_reading, hindu_husbands).
narrative_ontology:constraint_beneficiary(family_law_authority__hindu_dharmashastra_reading, dharmic_religious_authorities).
narrative_ontology:constraint_victim(family_law_authority__hindu_dharmashastra_reading, hindu_wives).
narrative_ontology:constraint_victim(family_law_authority__hindu_dharmashastra_reading, individuals_seeking_inter_caste_marriage).
narrative_ontology:constraint_victim(family_law_authority__hindu_dharmashastra_reading, individuals_seeking_divorce).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The primary social unit that benefits from and enforces the traditional structure of marriage, ensuring lineage continuity, property consolidation, and adherence to customary norms. It actively maintains the constraint through social pressure and resource allocation.
narrative_ontology:constraint_stakeholder(family_law_authority__hindu_dharmashastra_reading, hindu_joint_family, agenda_setter,
    organized, generational, constrained, local).

% Historically subject to limited autonomy, restricted property rights, and sacramental indissolubility of marriage. Their social and religious identity is deeply intertwined with their marital status within the traditional framework, making exit extremely difficult and socially costly.
narrative_ontology:constraint_stakeholder(family_law_authority__hindu_dharmashastra_reading, hindu_wives, payer,
    powerless, biographical, identity_locked, local).

% Benefit from patriarchal authority, lineage continuation, and control over family resources within the traditional marriage structure. They face fewer social sanctions for non-adherence to certain norms compared to wives.
narrative_ontology:constraint_stakeholder(family_law_authority__hindu_dharmashastra_reading, hindu_husbands, beneficiary,
    powerful, biographical, mobile, local).

% Interpret and transmit the Dharmashastra texts, providing religious sanction and guidance for marriage practices. They maintain the legitimacy of the sacramental view and influence community adherence through religious discourse and social authority.
narrative_ontology:constraint_stakeholder(family_law_authority__hindu_dharmashastra_reading, dharmic_religious_authorities, agenda_setter,
    institutional, generational, analytical, national).

% Enforce customary practices and social norms related to marriage, including caste endogamy and ritual observance. They wield significant social influence and can impose sanctions for deviations from tradition.
narrative_ontology:constraint_stakeholder(family_law_authority__hindu_dharmashastra_reading, community_elders, agenda_setter,
    organized, biographical, constrained, local).

% Face severe social ostracization, family disapproval, and potential violence for attempting to marry outside prescribed caste boundaries, which are deeply embedded in traditional dharmic practice. Their choices are suppressed by community norms.
narrative_ontology:constraint_stakeholder(family_law_authority__hindu_dharmashastra_reading, individuals_seeking_inter_caste_marriage, payer,
    powerless, immediate, constrained, local).
narrative_ontology:stakeholder_secondary_role(family_law_authority__hindu_dharmashastra_reading, individuals_seeking_inter_caste_marriage, excluded).

% Historically faced extreme difficulty or impossibility in dissolving a sacramental marriage, leading to social stigma and lack of legal recourse prior to modern reforms. Even post-reform, social pressure can make divorce highly punitive.
narrative_ontology:constraint_stakeholder(family_law_authority__hindu_dharmashastra_reading, individuals_seeking_divorce, payer,
    powerless, immediate, constrained, local).
narrative_ontology:stakeholder_secondary_role(family_law_authority__hindu_dharmashastra_reading, individuals_seeking_divorce, excluded).

% Post-1955, this system provides an alternative, legally binding framework for marriage and divorce that often conflicts with traditional dharmic interpretations. It observes and, at times, overrides customary practices, but does not fully displace the religious reading for many adherents.
narrative_ontology:constraint_stakeholder(family_law_authority__hindu_dharmashastra_reading, secular_legal_system, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(family_law_authority__hindu_dharmashastra_reading, hindu_joint_family).
narrative_ontology:fixing_cost_class(family_law_authority__hindu_dharmashastra_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a stable social order, ensures lineage continuity, defines family structure, and provides a ritual framework for life stages within the Hindu community, grounding social relations in religious tradition.
% TRANSFER_FUNCTION: Transfers social status, property rights (historically favoring males), ritual obligations, and control over women's labor and reproductive capacity within the joint family structure, from individuals to the collective and male lineage.
% ABSENT_VOICES: Women advocating for individual autonomy and equal rights, individuals seeking marriage outside traditional caste or religious norms, and those prioritizing individual choice over community or religious dictates are structurally marginalized or excluded from the conversation.
% DISAPPEARANCE_RATIONALE: If marriage as a sacramental samskara governed by dharmic texts and customary practice vanished overnight, the fundamental social, familial, and property structures within Hindu communities would profoundly reorganize. Lineage, inheritance, and social identity would lose their traditional anchors, leading to widespread societal upheaval and the emergence of entirely new forms of social organization.
% FOUNDING_PROBLEM: To establish a stable, ritually pure social order, ensure the continuation of lineage (especially male), define clear roles and obligations within the family, and provide a framework for spiritual progression (dharma) through the institution of marriage.
% FOUNDING_PROBLEM_CORROBORATION: Dharmashastra texts, ancient commentaries, and historical social practices attest to the founding problem. However, modern legal scholars, women's rights activists, and secular reformers contest its contemporary relevance, arguing that many original problems are either solved by modern law or superseded by evolving social values and individual rights. This contestation is evident in legislative debates and social movements.
narrative_ontology:disappearance_verdict(family_law_authority__hindu_dharmashastra_reading, world_rearranges).
narrative_ontology:founding_problem_status(family_law_authority__hindu_dharmashastra_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(family_law_authority__hindu_dharmashastra_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(family_law_authority__hindu_dharmashastra_reading, 'none', 1).
narrative_ontology:epsilon_provenance(family_law_authority__hindu_dharmashastra_reading, 0.85, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(family_law_authority__hindu_dharmashastra_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(family_law_authority__hindu_dharmashastra_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(family_law_authority__hindu_dharmashastra_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The constraint is classified as a Tangled Rope because it genuinely coordinates social and religious life (lineage, ritual, community identity) but does so with significant asymmetric extraction, particularly from women and those seeking to deviate from caste norms. Base extractiveness and suppression were historically very high, reflecting the limited autonomy and severe social sanctions. While legal reforms (e.g., the 1955 Act) have reduced the *legal* force of some aspects, the *social and religious* enforcement remains substantial, leading to a gradual decline in measured extractiveness and suppression over the interval, but not their elimination. The theater ratio is low because the rituals and customs are deeply meaningful to adherents, not merely performative, though some aspects may be maintained more for tradition than original function.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of beneficiaries (e.g., the joint family, religious authorities), this arrangement is a sacred and necessary coordination mechanism for social and spiritual order. From the perspective of victims (e.g., wives, those seeking inter-caste marriage), it is a system of enforced extraction and suppression that limits individual freedom and perpetuates inequality. The engine's per-seat classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   The Hindu joint family, male lineage, and religious authorities are structural beneficiaries, gaining social stability, property control, and interpretive authority. Hindu wives and individuals seeking inter-caste marriage or divorce are targets, bearing the costs of limited autonomy, social ostracization, and restricted choice. The 'identity_locked' exit option for wives reflects the deep fusion of personal identity with marital status within this traditional framework, making exit profoundly difficult even when legal alternatives exist.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    pre_post_1955_legal_impact,
    'To what extent do the Dharmashastra-based norms of marriage persist as a de facto constraint despite the legal reforms introduced by the Hindu Marriage Act of 1955?',
    'Empirical studies on marriage practices, divorce rates, property rights, and social sanctions in contemporary Hindu communities, disaggregated by region and socio-economic status.',
    'If persistence is high, the constraint''s effective extractiveness and suppression remain higher than purely legal analysis would suggest; if low, the secular legal system has substantially displaced the traditional reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(pre_post_1955_legal_impact, empirical, 'The gap between legal reform and social practice in Hindu marriage.').

omega_variable(
    custom_vs_text_authority,
    'What is the relative weight of Dharmashastra texts versus evolving customary practice in shaping contemporary Hindu marriage norms?',
    'Analysis of judicial decisions that reference custom, ethnographic studies of community-level norm enforcement, and surveys of religious leaders'' interpretations.',
    'If custom dominates, the constraint is more fluid and locally variable; if texts dominate, it is more rigid and resistant to change, potentially increasing suppression for those deviating from textual interpretations.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(custom_vs_text_authority, conceptual, 'The interplay of textual and customary authority in Hindu marriage.').

omega_variable(
    internalized_vs_structural_suppression,
    'Is the measured suppression primarily structural (e.g., social ostracization, economic dependency) or internalized (e.g., deeply held religious beliefs, identity fusion with traditional roles)?',
    'Post-exit suppression trajectory: if individuals who leave traditional marital structures continue to experience self-imposed limitations or identity crises, it suggests a significant internalized component.',
    'If internalized, the constraint''s effective suppression is higher than external measures suggest, as individuals carry the suppression with them after structural exit. This impacts the efficacy of legal reforms.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(internalized_vs_structural_suppression, empirical, 'Structural vs. internalized suppression mechanisms in Hindu marriage.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(family_law_authority__hindu_dharmashastra_reading, 1900, 2020).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fami_tr_t1900, family_law_authority__hindu_dharmashastra_reading, theater_ratio, 1900, 0.1).
narrative_ontology:measurement(fami_tr_t1930, family_law_authority__hindu_dharmashastra_reading, theater_ratio, 1930, 0.12).
narrative_ontology:measurement(fami_tr_t1955, family_law_authority__hindu_dharmashastra_reading, theater_ratio, 1955, 0.15).
narrative_ontology:measurement(fami_tr_t1980, family_law_authority__hindu_dharmashastra_reading, theater_ratio, 1980, 0.18).
narrative_ontology:measurement(fami_tr_t2000, family_law_authority__hindu_dharmashastra_reading, theater_ratio, 2000, 0.2).
narrative_ontology:measurement(fami_tr_t2020, family_law_authority__hindu_dharmashastra_reading, theater_ratio, 2020, 0.22).

% Extraction over time
narrative_ontology:measurement(fami_be_t1900, family_law_authority__hindu_dharmashastra_reading, base_extractiveness, 1900, 0.9).
narrative_ontology:measurement(fami_be_t1930, family_law_authority__hindu_dharmashastra_reading, base_extractiveness, 1930, 0.88).
narrative_ontology:measurement(fami_be_t1955, family_law_authority__hindu_dharmashastra_reading, base_extractiveness, 1955, 0.75).
narrative_ontology:measurement(fami_be_t1980, family_law_authority__hindu_dharmashastra_reading, base_extractiveness, 1980, 0.65).
narrative_ontology:measurement(fami_be_t2000, family_law_authority__hindu_dharmashastra_reading, base_extractiveness, 2000, 0.58).
narrative_ontology:measurement(fami_be_t2020, family_law_authority__hindu_dharmashastra_reading, base_extractiveness, 2020, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(fami_su_t1900, family_law_authority__hindu_dharmashastra_reading, suppression_requirement, 1900, 0.95).
narrative_ontology:measurement(fami_su_t1930, family_law_authority__hindu_dharmashastra_reading, suppression_requirement, 1930, 0.92).
narrative_ontology:measurement(fami_su_t1955, family_law_authority__hindu_dharmashastra_reading, suppression_requirement, 1955, 0.78).
narrative_ontology:measurement(fami_su_t1980, family_law_authority__hindu_dharmashastra_reading, suppression_requirement, 1980, 0.68).
narrative_ontology:measurement(fami_su_t2000, family_law_authority__hindu_dharmashastra_reading, suppression_requirement, 2000, 0.6).
narrative_ontology:measurement(fami_su_t2020, family_law_authority__hindu_dharmashastra_reading, suppression_requirement, 2020, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(family_law_authority__hindu_dharmashastra_reading, identity_coordination).
narrative_ontology:affects_constraint(family_law_authority__hindu_dharmashastra_reading, hindu_succession_laws).
narrative_ontology:affects_constraint(family_law_authority__hindu_dharmashastra_reading, caste_endogamy_norms).
narrative_ontology:affects_constraint(family_law_authority__hindu_dharmashastra_reading, joint_family_property_rules).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
