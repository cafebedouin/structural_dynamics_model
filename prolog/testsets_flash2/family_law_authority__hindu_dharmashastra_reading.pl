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
 *   constraint_id: family_law_authority__hindu_dharmashastra_reading
 *   human_readable: Hindu Marriage as Sacramental Samskara (Dharmashastra Reading)
 *   domain: comparative_law/political_theory/religious_governance
 *
 * SUMMARY:
 *   This constraint describes the Hindu understanding of marriage as a
 *   sacramental samskara (sacred rite) governed by Dharmashastra texts and
 *   customary practices, particularly in the pre-Hindu Marriage Act of 1955
 *   era. It emphasizes indissolubility, caste endogamy, and the wife's role
 *   as a ritual participant within the joint family, rather than an
 *   autonomous contractor. This is one reading of the broader
 *   'family_law_authority' kernel, which encompasses diverse religious and
 *   secular interpretations of marriage.
 *
 * KEY AGENTS:
 *   - patriarchal_family_elders: Agenda setter (institutional/constrained) — enforce traditional norms.
 *   - hindu_women: Payer (powerless/identity_locked) — bear the costs of constrained autonomy.
 *   - lower_caste_individuals: Payer (powerless/trapped) — subject to endogamy norms.
 *   - caste_associations: Beneficiary (organized/constrained) — benefit from preserved social hierarchy.
 *   - hindu_priests: Agenda setter (moderate/constrained) — reinforce religious sanctity.
 *   - secular_legal_system: Observer (institutional/analytical) — attempts reform post-1955.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(family_law_authority__hindu_dharmashastra_reading, 0.65).
domain_priors:suppression_score(family_law_authority__hindu_dharmashastra_reading, 0.7).
domain_priors:theater_ratio(family_law_authority__hindu_dharmashastra_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(family_law_authority__hindu_dharmashastra_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(family_law_authority__hindu_dharmashastra_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(family_law_authority__hindu_dharmashastra_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(family_law_authority__hindu_dharmashastra_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(family_law_authority__hindu_dharmashastra_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(family_law_authority__hindu_dharmashastra_reading, tangled_rope).
narrative_ontology:human_readable(family_law_authority__hindu_dharmashastra_reading, "Hindu Marriage as Sacramental Samskara (Dharmashastra Reading)").
narrative_ontology:topic_domain(family_law_authority__hindu_dharmashastra_reading, "comparative_law/political_theory/religious_governance").

domain_priors:requires_active_enforcement(family_law_authority__hindu_dharmashastra_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(family_law_authority__hindu_dharmashastra_reading, '761cf22b-64a0-4e93-8c32-23d11f99f35e').
narrative_ontology:cs_kernel_codification('761cf22b-64a0-4e93-8c32-23d11f99f35e', fixed_text).
narrative_ontology:cs_authority_grounding('761cf22b-64a0-4e93-8c32-23d11f99f35e', lineage).
narrative_ontology:cs_interpretation_layer_present('761cf22b-64a0-4e93-8c32-23d11f99f35e').
narrative_ontology:cs_reading_relation('761cf22b-64a0-4e93-8c32-23d11f99f35e', family_law_authority__muslim_shariat_reading, coexists_with).
narrative_ontology:cs_reading_relation('761cf22b-64a0-4e93-8c32-23d11f99f35e', family_law_authority__christian_canonical_reading, coexists_with).
narrative_ontology:cs_reading_relation('761cf22b-64a0-4e93-8c32-23d11f99f35e', family_law_authority__parsi_zoroastrian_reading, coexists_with).
narrative_ontology:cs_reading_relation('761cf22b-64a0-4e93-8c32-23d11f99f35e', family_law_authority__secular_contractual_reading, coexists_with).
narrative_ontology:cs_axiom('761cf22b-64a0-4e93-8c32-23d11f99f35e', foundational, marriage_as_indissoluble_samskara).
narrative_ontology:cs_axiom_status(marriage_as_indissoluble_samskara, holdable).
narrative_ontology:cs_axiom_grounding('761cf22b-64a0-4e93-8c32-23d11f99f35e', marriage_as_indissoluble_samskara, theological).
narrative_ontology:cs_axiom('761cf22b-64a0-4e93-8c32-23d11f99f35e', foundational, caste_endogamy_for_social_order).
narrative_ontology:cs_axiom_status(caste_endogamy_for_social_order, holdable).
narrative_ontology:cs_axiom_grounding('761cf22b-64a0-4e93-8c32-23d11f99f35e', caste_endogamy_for_social_order, conventional).
narrative_ontology:cs_reference_frame('761cf22b-64a0-4e93-8c32-23d11f99f35e', traditional_dharmic_social_order).
narrative_ontology:cs_drift_state('761cf22b-64a0-4e93-8c32-23d11f99f35e', post_1955_legal_reforms, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('761cf22b-64a0-4e93-8c32-23d11f99f35e', '').
narrative_ontology:cs_kernel_id(family_law_authority__hindu_dharmashastra_reading, family_law_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(family_law_authority__hindu_dharmashastra_reading, patriarchal_family_elders).
narrative_ontology:constraint_beneficiary(family_law_authority__hindu_dharmashastra_reading, caste_associations).
narrative_ontology:constraint_victim(family_law_authority__hindu_dharmashastra_reading, hindu_women).
narrative_ontology:constraint_victim(family_law_authority__hindu_dharmashastra_reading, lower_caste_individuals).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interpret and enforce dharmic texts and customary practices, particularly regarding caste endogamy and joint family property. They benefit from the preservation of traditional social structures and control over family assets.
narrative_ontology:constraint_stakeholder(family_law_authority__hindu_dharmashastra_reading, patriarchal_family_elders, agenda_setter,
    institutional, generational, constrained, local).

% Are primarily defined by their role within the family and marriage as a sacred duty (samskara). Their autonomy is constrained by traditional expectations, joint family property rules, and limited rights to divorce or property inheritance (pre-1955 context). Exit is difficult due to social stigma and economic dependence.
narrative_ontology:constraint_stakeholder(family_law_authority__hindu_dharmashastra_reading, hindu_women, payer,
    powerless, biographical, identity_locked, local).

% Are subject to strict caste endogamy norms, which limit their marital choices and social mobility. Inter-caste marriages are often met with severe social ostracism and violence, reinforcing their trapped status within the traditional system.
narrative_ontology:constraint_stakeholder(family_law_authority__hindu_dharmashastra_reading, lower_caste_individuals, payer,
    powerless, generational, trapped, regional).

% Benefit from the enforcement of caste endogamy, which preserves their social hierarchy and political influence. They actively promote and enforce customary practices that uphold traditional marital norms.
narrative_ontology:constraint_stakeholder(family_law_authority__hindu_dharmashastra_reading, caste_associations, beneficiary,
    organized, generational, constrained, regional).

% Perform the sacramental rites and interpret dharmic injunctions, reinforcing the religious sanctity and indissolubility of marriage. They derive authority and social standing from their role in upholding these traditions.
narrative_ontology:constraint_stakeholder(family_law_authority__hindu_dharmashastra_reading, hindu_priests, agenda_setter,
    moderate, generational, constrained, local).

% Observes and, in the post-1955 era, attempts to reform aspects of Hindu personal law. Prior to the Hindu Marriage Act of 1955, its influence was limited, and customary practices often held sway. It now seeks to balance religious freedom with principles of equality and justice.
narrative_ontology:constraint_stakeholder(family_law_authority__hindu_dharmashastra_reading, secular_legal_system, observer,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates social order, family lineage, and ritual continuity by defining marital roles, property rights, and social obligations within the Hindu community, ensuring the transmission of cultural and religious values across generations.
% TRANSFER_FUNCTION: Transfers social status, ritual duties, and property rights, primarily from women and lower-caste individuals to patriarchal family structures and higher-caste groups, maintaining a hierarchical social order.
% ABSENT_VOICES: Advocates for gender equality, individual autonomy, and inter-caste marriage were historically excluded from the interpretive and enforcement mechanisms of Dharmashastra. Their voices would challenge the sacramental indissolubility, caste endogamy, and patriarchal property rules.
% DISAPPEARANCE_RATIONALE: If the Dharmashastra reading of marriage vanished overnight, the social fabric of many Hindu communities would undergo profound reorganization. Traditional family structures, caste hierarchies, and property arrangements would lose their foundational legitimacy, leading to a rapid shift towards more individualistic and egalitarian marital practices, potentially causing significant social upheaval and redefinition of identity.
% FOUNDING_PROBLEM: To establish a stable social and ritual order, ensure lineage continuity, and define the roles and responsibilities within families according to dharmic principles, preventing social chaos and moral decay.
% FOUNDING_PROBLEM_CORROBORATION: Traditionalists and religious scholars within the Hindu community attest that the founding problem of maintaining dharma and social order is still live. However, secular legal scholars, women's rights activists, and social reformers (outside the directly benefiting parties) argue that while social order is still a concern, the specific mechanisms of Dharmashastra have become obsolete or harmful, and the problem is now primarily one of maintaining patriarchal and caste-based power structures.
narrative_ontology:disappearance_verdict(family_law_authority__hindu_dharmashastra_reading, world_rearranges).
narrative_ontology:founding_problem_status(family_law_authority__hindu_dharmashastra_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(family_law_authority__hindu_dharmashastra_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(family_law_authority__hindu_dharmashastra_reading, 'none', 1).
narrative_ontology:epsilon_provenance(family_law_authority__hindu_dharmashastra_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

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
 *   The constraint is classified as a Tangled Rope because it genuinely coordinates social and ritual order (beneficiaries: patriarchal family elders, caste associations) but does so through asymmetric extraction (victims: Hindu women, lower-caste individuals) and requires active enforcement by social and religious authorities. Extractiveness is high (0.65) due to the significant limitations on individual autonomy and property rights, particularly for women. Suppression (0.70) is also high, maintained through social ostracism, religious injunctions, and community pressure against deviations from customary practice. Theater ratio is low (0.20) as the rituals and social enforcement are genuinely functional in maintaining the traditional order, not merely performative.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of patriarchal family elders and caste associations, this system is a legitimate and necessary framework for preserving dharma and social stability. For Hindu women and lower-caste individuals, it is an extractive system that limits their freedom and perpetuates inequality. The engine's classification will reflect this divergence based on their structural positions.
 *
 * DIRECTIONALITY LOGIC:
 *   Patriarchal family elders and caste associations are beneficiaries (low d) as they gain social control and status from the system. Hindu women and lower-caste individuals are targets (high d) as they bear the primary costs of restricted autonomy and social mobility. Hindu priests, while also agenda-setters, benefit from the system's perpetuation through their ritual authority.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification prevents mislabeling the constraint as a pure Mountain (natural law) or a simple Rope (pure coordination). While it claims religious and natural authority, the identifiable beneficiaries and victims, coupled with active enforcement, reveal its constructed and extractive nature. The 'contested' status of the founding problem further highlights the ongoing debate about its current function versus its historical justification.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sacramental_vs_contractual_nature,
    'Is Hindu marriage fundamentally a sacrament (samskara) or can it be reinterpreted as a civil contract between autonomous individuals?',
    'Evolution of legal frameworks (e.g., Hindu Marriage Act 1955 introducing divorce) and shifts in social acceptance of individual agency in marital decisions.',
    'If reinterpreted as a contract, the constraint''s indissolubility and patriarchal control would collapse, significantly reducing extractiveness and suppression. If it remains purely sacramental, the current high extractiveness and suppression persist.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sacramental_vs_contractual_nature, conceptual, 'Ambiguity in the fundamental nature of Hindu marriage.').

omega_variable(
    caste_endogamy_legitimacy,
    'Is caste endogamy a legitimate and necessary component of Dharmashastra-governed marriage, or is it a social custom that can be decoupled from religious injunctions?',
    'Judicial rulings on inter-caste marriages, social movements challenging caste discrimination, and evolving interpretations by religious authorities.',
    'If decoupled, the constraint''s suppression and extractiveness on lower-caste individuals would significantly decrease. If maintained, it continues to be a major source of extraction and social control.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(caste_endogamy_legitimacy, empirical, 'The role and legitimacy of caste endogamy in Hindu marriage.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (social ostracism, economic dependence) or internalized (belief in dharma, duty to family)?',
    'Post-exit suppression trajectory: if suppression persists after structural barriers are removed (e.g., legal reforms), reclassify as partially internalized.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests — individuals carry the suppression with them after exit, making reform efforts less effective without cultural shifts.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism in Hindu marriage.').


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
narrative_ontology:measurement(fami_be_t1900, family_law_authority__hindu_dharmashastra_reading, base_extractiveness, 1900, 0.6).
narrative_ontology:measurement(fami_be_t1910, family_law_authority__hindu_dharmashastra_reading, base_extractiveness, 1910, 0.62).
narrative_ontology:measurement(fami_be_t1920, family_law_authority__hindu_dharmashastra_reading, base_extractiveness, 1920, 0.63).
narrative_ontology:measurement(fami_be_t1930, family_law_authority__hindu_dharmashastra_reading, base_extractiveness, 1930, 0.64).
narrative_ontology:measurement(fami_be_t1940, family_law_authority__hindu_dharmashastra_reading, base_extractiveness, 1940, 0.65).
narrative_ontology:measurement(fami_be_t1955, family_law_authority__hindu_dharmashastra_reading, base_extractiveness, 1955, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(fami_su_t1900, family_law_authority__hindu_dharmashastra_reading, suppression_requirement, 1900, 0.65).
narrative_ontology:measurement(fami_su_t1910, family_law_authority__hindu_dharmashastra_reading, suppression_requirement, 1910, 0.67).
narrative_ontology:measurement(fami_su_t1920, family_law_authority__hindu_dharmashastra_reading, suppression_requirement, 1920, 0.68).
narrative_ontology:measurement(fami_su_t1930, family_law_authority__hindu_dharmashastra_reading, suppression_requirement, 1930, 0.69).
narrative_ontology:measurement(fami_su_t1940, family_law_authority__hindu_dharmashastra_reading, suppression_requirement, 1940, 0.7).
narrative_ontology:measurement(fami_su_t1955, family_law_authority__hindu_dharmashastra_reading, suppression_requirement, 1955, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(family_law_authority__hindu_dharmashastra_reading, identity_coordination).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'family_law_authority' kernel, focusing on the Hindu Dharmashastra interpretation. It is structurally distinct from other religious and secular readings of marriage law.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
