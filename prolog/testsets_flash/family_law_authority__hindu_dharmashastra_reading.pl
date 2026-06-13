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
 *   constraint_id: family_law_authority__hindu_dharmashastra_reading
 *   human_readable: Hindu Dharmashastra Reading of Marriage as Sacramental Samskara
 *   domain: comparative_law/political_theory/religious_governance
 *
 * SUMMARY:
 *   This constraint models the Hindu Dharmashastra reading of marriage as a
 *   sacramental samskara (sacrament) in India, particularly in the period
 *   before the Hindu Marriage Act of 1955. This reading emphasizes
 *   indissolubility, caste endogamy, and joint family property rules,
 *   positioning the wife as a ritual participant rather than an autonomous
 *   contractor. It is one reading of the broader 'family_law_authority'
 *   kernel, which is contested by other religious and secular
 *   interpretations.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(family_law_authority__hindu_dharmashastra_reading, 0.65).
domain_priors:suppression_score(family_law_authority__hindu_dharmashastra_reading, 0.75).
domain_priors:theater_ratio(family_law_authority__hindu_dharmashastra_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(family_law_authority__hindu_dharmashastra_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(family_law_authority__hindu_dharmashastra_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(family_law_authority__hindu_dharmashastra_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(family_law_authority__hindu_dharmashastra_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(family_law_authority__hindu_dharmashastra_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(family_law_authority__hindu_dharmashastra_reading, tangled_rope).
narrative_ontology:human_readable(family_law_authority__hindu_dharmashastra_reading, "Hindu Dharmashastra Reading of Marriage as Sacramental Samskara").
narrative_ontology:topic_domain(family_law_authority__hindu_dharmashastra_reading, "comparative_law/political_theory/religious_governance").

domain_priors:requires_active_enforcement(family_law_authority__hindu_dharmashastra_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(family_law_authority__hindu_dharmashastra_reading, '9a1d038e-6c9f-43aa-bf62-fd15ab635437').
narrative_ontology:cs_kernel_codification('9a1d038e-6c9f-43aa-bf62-fd15ab635437', fixed_text).
narrative_ontology:cs_authority_grounding('9a1d038e-6c9f-43aa-bf62-fd15ab635437', lineage).
narrative_ontology:cs_interpretation_layer_present('9a1d038e-6c9f-43aa-bf62-fd15ab635437').
narrative_ontology:cs_reading_relation('9a1d038e-6c9f-43aa-bf62-fd15ab635437', family_law_authority__muslim_shariat_reading, coexists_with).
narrative_ontology:cs_reading_relation('9a1d038e-6c9f-43aa-bf62-fd15ab635437', family_law_authority__christian_canonical_reading, coexists_with).
narrative_ontology:cs_reading_relation('9a1d038e-6c9f-43aa-bf62-fd15ab635437', family_law_authority__parsi_zoroastrian_reading, coexists_with).
narrative_ontology:cs_reading_relation('9a1d038e-6c9f-43aa-bf62-fd15ab635437', family_law_authority__secular_contractual_reading, influences).
narrative_ontology:cs_axiom('9a1d038e-6c9f-43aa-bf62-fd15ab635437', foundational, marriage_as_indissoluble_samskara).
narrative_ontology:cs_axiom_status(marriage_as_indissoluble_samskara, holdable).
narrative_ontology:cs_axiom_grounding('9a1d038e-6c9f-43aa-bf62-fd15ab635437', marriage_as_indissoluble_samskara, theological).
narrative_ontology:cs_axiom('9a1d038e-6c9f-43aa-bf62-fd15ab635437', foundational, caste_endogamy_as_ritual_purity).
narrative_ontology:cs_axiom_status(caste_endogamy_as_ritual_purity, holdable).
narrative_ontology:cs_axiom_grounding('9a1d038e-6c9f-43aa-bf62-fd15ab635437', caste_endogamy_as_ritual_purity, conventional).
narrative_ontology:cs_reference_frame('9a1d038e-6c9f-43aa-bf62-fd15ab635437', traditional_dharmic_order).
narrative_ontology:cs_drift_state('9a1d038e-6c9f-43aa-bf62-fd15ab635437', pre_1955_legal_reforms, gap(stable, minor, false)).
narrative_ontology:cs_created_at('9a1d038e-6c9f-43aa-bf62-fd15ab635437', '').
narrative_ontology:cs_kernel_id(family_law_authority__hindu_dharmashastra_reading, family_law_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(family_law_authority__hindu_dharmashastra_reading, joint_family_elders).
narrative_ontology:constraint_beneficiary(family_law_authority__hindu_dharmashastra_reading, caste_community_leaders).
narrative_ontology:constraint_beneficiary(family_law_authority__hindu_dharmashastra_reading, brahmin_priests).
narrative_ontology:constraint_victim(family_law_authority__hindu_dharmashastra_reading, hindu_women).
narrative_ontology:constraint_victim(family_law_authority__hindu_dharmashastra_reading, lower_caste_individuals).
narrative_ontology:constraint_victim(family_law_authority__hindu_dharmashastra_reading, inter_caste_couples).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interpret and transmit the dharmic texts and customary practices that define marriage as a sacramental samskara, emphasizing its indissolubility and ritual significance. Their authority is grounded in lineage and tradition.
narrative_ontology:constraint_stakeholder(family_law_authority__hindu_dharmashastra_reading, hindu_dharmashastra_scholars, agenda_setter,
    institutional, generational, identity_locked, national).

% Benefit from the preservation of joint family property structures and the social stability derived from arranged marriages and caste endogamy. They enforce customary practices within their families and communities.
narrative_ontology:constraint_stakeholder(family_law_authority__hindu_dharmashastra_reading, joint_family_elders, beneficiary,
    organized, biographical, constrained, local).

% Benefit from the maintenance of caste endogamy norms, which reinforce their social and ritual status. They exert significant social pressure to ensure compliance with traditional marriage practices.
narrative_ontology:constraint_stakeholder(family_law_authority__hindu_dharmashastra_reading, caste_community_leaders, beneficiary,
    organized, generational, constrained, regional).

% Are essential for performing the sacramental rites of marriage, deriving social standing and economic support from their role. They uphold the ritualistic interpretation of marriage.
narrative_ontology:constraint_stakeholder(family_law_authority__hindu_dharmashastra_reading, brahmin_priests, beneficiary,
    moderate, biographical, identity_locked, local).

% Bear the costs of sacramental indissolubility (pre-1955), limited property rights, and the expectation of ritual participation over individual autonomy. Their identity is often fused with their marital and familial roles, making exit difficult.
narrative_ontology:constraint_stakeholder(family_law_authority__hindu_dharmashastra_reading, hindu_women, payer,
    powerless, biographical, identity_locked, local).

% Are subject to strict caste endogamy norms, which limit their marital choices and social mobility. Violations can lead to ostracization and violence, making exit from these norms extremely costly.
narrative_ontology:constraint_stakeholder(family_law_authority__hindu_dharmashastra_reading, lower_caste_individuals, payer,
    powerless, generational, trapped, local).

% Face severe social ostracization, threats, and violence for violating caste endogamy norms, often leading to forced separation or honor killings. Their choices are suppressed by community enforcement.
narrative_ontology:constraint_stakeholder(family_law_authority__hindu_dharmashastra_reading, inter_caste_couples, payer,
    powerless, biographical, trapped, local).

% Advocate for a uniform civil code and individual contractual rights in marriage, challenging the authority of religious personal laws. Their proposals are often resisted by religious and community leaders who benefit from the existing system.
narrative_ontology:constraint_stakeholder(family_law_authority__hindu_dharmashastra_reading, secular_legal_reformers, excluded,
    organized, generational, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates family and caste structures by defining marital roles, property inheritance, and social obligations, ensuring continuity of lineage and ritual practice within the Hindu community.
% TRANSFER_FUNCTION: Transfers authority over marital decisions from individuals to family and community elders, and transfers property rights and ritual obligations within the joint family system, primarily from women to the male lineage.
% ABSENT_VOICES: Secular legal reformers and advocates for individual autonomy in marriage are often excluded from the discourse on dharmic marriage, as their foundational premises challenge the very authority of the dharmashastra. Inter-caste couples, though directly impacted, are often silenced or violently suppressed.
% DISAPPEARANCE_RATIONALE: If the dharmashastra's authority over marriage vanished, the social fabric of many Hindu communities would undergo profound rearrangement. Joint family property systems would be challenged, caste endogamy norms would erode, and individual autonomy in marriage would increase, leading to significant shifts in social power and economic structures.
% FOUNDING_PROBLEM: To establish a stable social order, ensure lineage continuity, regulate property inheritance, and maintain ritual purity within the Hindu community through religiously sanctioned marital practices.
% FOUNDING_PROBLEM_CORROBORATION: Dharmashastra scholars and community elders assert the problem is live, emphasizing the need for social and ritual continuity. Secular legal reformers and human rights advocates, citing the Hindu Marriage Act of 1955 and subsequent reforms, argue that the original problem has been addressed by state law, and the persistence of traditional norms now serves primarily to maintain patriarchal and caste hierarchies. Independent sociological studies corroborate the latter view.
narrative_ontology:disappearance_verdict(family_law_authority__hindu_dharmashastra_reading, world_rearranges).
narrative_ontology:founding_problem_status(family_law_authority__hindu_dharmashastra_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(family_law_authority__hindu_dharmashastra_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(family_law_authority__hindu_dharmashastra_reading, 'none', 1).

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
 *   The constraint is classified as a Tangled Rope because it genuinely coordinates social and ritual functions (lineage, caste stability) while simultaneously extracting significant costs from specific groups, particularly Hindu women and lower-caste individuals. Extraction is high (0.65) due to the denial of individual autonomy, property rights, and freedom of marital choice. Suppression is also high (0.75) due to intense social pressure, community enforcement, and the threat of ostracization or violence for non-compliance. Theater ratio is low (0.20) as the ritual and social functions are genuinely performed and believed, but a portion of the enforcement serves to maintain the extractive aspects.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the beneficiaries (elders, priests, community leaders), this constraint is a necessary framework for social and ritual order. From the perspective of the victims (women, lower-caste individuals, inter-caste couples), it is a deeply extractive and suppressive system that denies fundamental rights. The engine's per-seat classification will reflect this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Joint family elders, caste community leaders, and Brahmin priests are beneficiaries, as the constraint reinforces their authority, social status, and economic interests. Hindu women, lower-caste individuals, and inter-caste couples are victims, bearing the costs of restricted autonomy, limited rights, and severe social penalties. Secular legal reformers are excluded, as their proposals directly challenge the foundational premises of this reading.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate to maintain social and ritual order is still 'contested' (as per the six questions), but its function has shifted significantly. While it once served a primary coordination role in a pre-modern context, its persistence in the mid-20th century (before the 1955 Act) increasingly served to maintain existing power structures and extraction, rather than solely solving a collective action problem. The high extractiveness and suppression, coupled with the contested founding problem status, indicate a strong potential for mandatrophy, where the coordination story masks ongoing extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sacramental_vs_contractual_nature,
    'Is marriage fundamentally a sacrament (samskara) or a civil contract in contemporary Hindu society?',
    'Analysis of post-1955 legal reforms and their societal adoption, as well as evolving social practices and individual interpretations of marriage.',
    'If predominantly contractual, the ''sacramental'' framing becomes a theatrical cover for extraction; if sacramental belief persists, it grounds a portion of the constraint''s legitimacy, even if extractive.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sacramental_vs_contractual_nature, conceptual, 'Ambiguity in the fundamental nature of Hindu marriage.').

omega_variable(
    caste_endogamy_enforcement_source,
    'To what extent is caste endogamy enforced by religious texts versus social custom and community pressure?',
    'Sociological studies on the mechanisms of enforcement for inter-caste marriage prohibitions, distinguishing textual injunctions from community-level social sanctions.',
    'If primarily social custom, the ''dharmic'' justification for endogamy is weakened, exposing it as a purely social snare; if textually mandated, it reinforces the religious grounding of the constraint.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(caste_endogamy_enforcement_source, empirical, 'Source of enforcement for caste endogamy norms.').

omega_variable(
    identity_lock_vs_structural_trap,
    'For Hindu women and lower-caste individuals, is the ''identity_locked'' exit option primarily due to internalized identity fusion or overwhelming structural barriers?',
    'Post-exit trajectory analysis: if suppression persists after structural barriers are removed (e.g., legal reforms), it indicates internalized identity lock. If exit becomes genuinely mobile, it was primarily structural.',
    'If internalized, the effective suppression is higher than structural measures suggest, as the constraint''s effects are carried by the individual. If purely structural, legal and social reforms are more directly effective.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_vs_structural_trap, empirical, 'Structural vs. internalized suppression mechanism for identity-locked agents.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(family_law_authority__hindu_dharmashastra_reading, 1900, 1955).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fami_tr_t1900, family_law_authority__hindu_dharmashastra_reading, theater_ratio, 1900, 0.15).
narrative_ontology:measurement(fami_tr_t1910, family_law_authority__hindu_dharmashastra_reading, theater_ratio, 1910, 0.16).
narrative_ontology:measurement(fami_tr_t1920, family_law_authority__hindu_dharmashastra_reading, theater_ratio, 1920, 0.17).
narrative_ontology:measurement(fami_tr_t1930, family_law_authority__hindu_dharmashastra_reading, theater_ratio, 1930, 0.18).
narrative_ontology:measurement(fami_tr_t1940, family_law_authority__hindu_dharmashastra_reading, theater_ratio, 1940, 0.19).
narrative_ontology:measurement(fami_tr_t1955, family_law_authority__hindu_dharmashastra_reading, theater_ratio, 1955, 0.2).

% Extraction over time
narrative_ontology:measurement(fami_be_t1900, family_law_authority__hindu_dharmashastra_reading, base_extractiveness, 1900, 0.7).
narrative_ontology:measurement(fami_be_t1910, family_law_authority__hindu_dharmashastra_reading, base_extractiveness, 1910, 0.68).
narrative_ontology:measurement(fami_be_t1920, family_law_authority__hindu_dharmashastra_reading, base_extractiveness, 1920, 0.67).
narrative_ontology:measurement(fami_be_t1930, family_law_authority__hindu_dharmashastra_reading, base_extractiveness, 1930, 0.66).
narrative_ontology:measurement(fami_be_t1940, family_law_authority__hindu_dharmashastra_reading, base_extractiveness, 1940, 0.66).
narrative_ontology:measurement(fami_be_t1955, family_law_authority__hindu_dharmashastra_reading, base_extractiveness, 1955, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(fami_su_t1900, family_law_authority__hindu_dharmashastra_reading, suppression_requirement, 1900, 0.8).
narrative_ontology:measurement(fami_su_t1910, family_law_authority__hindu_dharmashastra_reading, suppression_requirement, 1910, 0.78).
narrative_ontology:measurement(fami_su_t1920, family_law_authority__hindu_dharmashastra_reading, suppression_requirement, 1920, 0.77).
narrative_ontology:measurement(fami_su_t1930, family_law_authority__hindu_dharmashastra_reading, suppression_requirement, 1930, 0.76).
narrative_ontology:measurement(fami_su_t1940, family_law_authority__hindu_dharmashastra_reading, suppression_requirement, 1940, 0.75).
narrative_ontology:measurement(fami_su_t1955, family_law_authority__hindu_dharmashastra_reading, suppression_requirement, 1955, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(family_law_authority__hindu_dharmashastra_reading, identity_coordination).
narrative_ontology:affects_constraint(family_law_authority__hindu_dharmashastra_reading, family_law_authority__muslim_shariat_reading).
narrative_ontology:affects_constraint(family_law_authority__hindu_dharmashastra_reading, family_law_authority__christian_canonical_reading).
narrative_ontology:affects_constraint(family_law_authority__hindu_dharmashastra_reading, family_law_authority__parsi_zoroastrian_reading).
narrative_ontology:affects_constraint(family_law_authority__hindu_dharmashastra_reading, family_law_authority__secular_contractual_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'family_law_authority' kernel. Its structural properties and metrics are distinct from other religious and secular readings, which are modeled as separate constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
