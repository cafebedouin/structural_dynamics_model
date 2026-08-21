% ============================================================================
% CONSTRAINT STORY: marriage_authority__secularist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_marriage_authority__secularist_reading, []).

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
 *   constraint_id: marriage_authority__secularist_reading
 *   human_readable: Secularist Reading: Legislative Supremacy in Marriage Law
 *   domain: legal/political/social
 *
 * SUMMARY:
 *   This constraint represents the secularist reading of marriage authority,
 *   asserting that the democratic legislature holds ultimate power over
 *   family law, and that legal pluralism (religious personal laws) is a
 *   temporary anomaly to be eliminated by a Uniform Civil Code (UCC). This
 *   reading frames the UCC as a necessary step for national integration and
 *   equality, while actively suppressing alternative claims to communal
 *   autonomy. The constraint is a Tangled Rope because it claims a
 *   coordination function (uniformity, equality) but involves significant
 *   asymmetric extraction from minority religious communities.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(marriage_authority__secularist_reading, 0.78).
domain_priors:suppression_score(marriage_authority__secularist_reading, 0.85).
domain_priors:theater_ratio(marriage_authority__secularist_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(marriage_authority__secularist_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(marriage_authority__secularist_reading, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(marriage_authority__secularist_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(marriage_authority__secularist_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(marriage_authority__secularist_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(marriage_authority__secularist_reading, tangled_rope).
narrative_ontology:human_readable(marriage_authority__secularist_reading, "Secularist Reading: Legislative Supremacy in Marriage Law").
narrative_ontology:topic_domain(marriage_authority__secularist_reading, "legal/political/social").

domain_priors:requires_active_enforcement(marriage_authority__secularist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(marriage_authority__secularist_reading, 'fa2121ab-a28a-4577-991a-d0e8d2739bf0').
narrative_ontology:cs_kernel_codification('fa2121ab-a28a-4577-991a-d0e8d2739bf0', formalized).
narrative_ontology:cs_authority_grounding('fa2121ab-a28a-4577-991a-d0e8d2739bf0', lineage).
narrative_ontology:cs_interpretation_layer_present('fa2121ab-a28a-4577-991a-d0e8d2739bf0').
narrative_ontology:cs_reading_relation('fa2121ab-a28a-4577-991a-d0e8d2739bf0', marriage_authority__communal_autonomy_reading, forecloses).
narrative_ontology:cs_reading_relation('fa2121ab-a28a-4577-991a-d0e8d2739bf0', marriage_authority__federalist_millet_reading, forecloses).
narrative_ontology:cs_reading_relation('fa2121ab-a28a-4577-991a-d0e8d2739bf0', marriage_authority__gender_rights_reading, influences).
narrative_ontology:cs_reading_relation('fa2121ab-a28a-4577-991a-d0e8d2739bf0', marriage_authority__judicial_harmonization_reading, influences).
narrative_ontology:cs_axiom('fa2121ab-a28a-4577-991a-d0e8d2739bf0', foundational, legislative_supremacy_in_family_law).
narrative_ontology:cs_axiom_status(legislative_supremacy_in_family_law, holdable).
narrative_ontology:cs_axiom_grounding('fa2121ab-a28a-4577-991a-d0e8d2739bf0', legislative_supremacy_in_family_law, conventional).
narrative_ontology:cs_axiom('fa2121ab-a28a-4577-991a-d0e8d2739bf0', foundational, uniformity_as_national_integration).
narrative_ontology:cs_axiom_status(uniformity_as_national_integration, holdable).
narrative_ontology:cs_axiom_grounding('fa2121ab-a28a-4577-991a-d0e8d2739bf0', uniformity_as_national_integration, instrumental).
narrative_ontology:cs_reference_frame('fa2121ab-a28a-4577-991a-d0e8d2739bf0', post_independence_secular_state).
narrative_ontology:cs_drift_state('fa2121ab-a28a-4577-991a-d0e8d2739bf0', contemporary_political_discourse, gap(stable, minor, true)).
narrative_ontology:cs_created_at('fa2121ab-a28a-4577-991a-d0e8d2739bf0', '').
narrative_ontology:cs_kernel_id(marriage_authority__secularist_reading, marriage_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(marriage_authority__secularist_reading, secular_modernist_coalition).
narrative_ontology:constraint_beneficiary(marriage_authority__secularist_reading, state_legislature).
narrative_ontology:constraint_victim(marriage_authority__secularist_reading, minority_religious_communities).
narrative_ontology:constraint_victim(marriage_authority__secularist_reading, religious_personal_law_boards).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(marriage_authority__secularist_reading, gender_equality_advocates).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The primary body asserting authority to legislate on all matters of marriage and family law, including the eventual implementation of a Uniform Civil Code. Benefits from consolidating power over a domain previously fragmented by religious personal laws.
narrative_ontology:constraint_stakeholder(marriage_authority__secularist_reading, state_legislature, agenda_setter,
    institutional, generational, mobile, national).

% Advocates for a Uniform Civil Code as a means to achieve national integration, gender equality, and secular governance. Benefits from the erosion of religious personal laws and the assertion of state authority in this domain.
narrative_ontology:constraint_stakeholder(marriage_authority__secularist_reading, secular_modernist_coalition, beneficiary,
    organized, generational, mobile, national).

% Bear the cost of losing their traditional autonomy in family law, viewing it as an infringement on religious freedom and cultural identity. Their adherence to community-specific norms is directly challenged by the push for a UCC.
narrative_ontology:constraint_stakeholder(marriage_authority__secularist_reading, minority_religious_communities, payer,
    powerless, generational, identity_locked, national).

% Historically administered personal laws for their respective communities. They face the loss of their institutional authority and function as the state asserts legislative supremacy. Their resistance is often framed as defending religious freedom.
narrative_ontology:constraint_stakeholder(marriage_authority__secularist_reading, religious_personal_law_boards, payer,
    moderate, biographical, constrained, national).

% Support the UCC as a tool to eliminate discriminatory practices within various personal laws, particularly those affecting women. They benefit from the potential for a unified, egalitarian legal framework.
narrative_ontology:constraint_stakeholder(marriage_authority__secularist_reading, gender_equality_advocates, beneficiary,
    organized, generational, mobile, national).

% Interprets the constitutionality of personal laws and legislative attempts to implement a UCC. Its rulings can either affirm legislative supremacy or protect communal autonomy, shaping the constraint's evolution.
narrative_ontology:constraint_stakeholder(marriage_authority__secularist_reading, constitutional_court, observer,
    institutional, civilizational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Aims to coordinate diverse religious and cultural practices under a single, uniform legal framework for marriage and family, reducing legal complexity and promoting national unity.
% TRANSFER_FUNCTION: Transfers authority over marriage and family law from diverse religious communities and their traditional institutions to the secular state legislature, consolidating legal power.
% ABSENT_VOICES: Many traditional religious leaders and community elders, particularly from minority groups, are often excluded from the legislative drafting process for a UCC, despite being directly impacted. Their voices would emphasize the importance of religious identity and communal self-governance.
% DISAPPEARANCE_RATIONALE: If the secularist assertion of legislative supremacy vanished, the legal landscape would revert to a more fragmented system of personal laws, with religious communities reasserting greater autonomy. The push for a UCC would cease, and legal pluralism would be re-entrenched.
% FOUNDING_PROBLEM: The existence of multiple, often conflicting, religious personal laws created legal disparities, particularly for women, and hindered national integration, leading to a call for a unified civil code.
% FOUNDING_PROBLEM_CORROBORATION: Secular legal scholars, women's rights organizations, and a segment of the judiciary corroborate that legal disparities and integration challenges remain live problems, justifying the continued pursuit of a UCC. Minority religious groups, however, contest the framing of their personal laws as a 'problem' to be solved by the state.
narrative_ontology:disappearance_verdict(marriage_authority__secularist_reading, world_rearranges).
narrative_ontology:founding_problem_status(marriage_authority__secularist_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(marriage_authority__secularist_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(marriage_authority__secularist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(marriage_authority__secularist_reading, 0.78, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(marriage_authority__secularist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(marriage_authority__secularist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(marriage_authority__secularist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.78) because it seeks to dismantle existing, deeply held communal legal structures, imposing a state-centric model that benefits the secular-modernist coalition. Suppression is very high (0.85) as it requires active legislative and judicial enforcement to override religious traditions and prevent their continued application. Theater ratio is low (0.15) because the legislative push for a UCC is a direct, functional effort, not primarily performative. Accessibility collapse is moderate (0.65) as alternatives (communal autonomy) are actively suppressed but not entirely eradicated, leading to ongoing resistance. Resistance is high (0.7) due to strong opposition from religious communities.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the secular-modernist coalition, this is a necessary and progressive 'Rope' for national integration and equality. From the perspective of minority religious communities, it is a 'Snare' designed to dismantle their cultural and religious identity. The engine's classification as Tangled Rope reflects the hybrid nature: a genuine coordination claim (uniformity) coupled with substantial, enforced extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   The state legislature and the secular-modernist coalition are clear beneficiaries (d near 0.0), gaining power and achieving ideological goals. Minority religious communities and their personal law boards are the primary targets (d near 1.0), facing the loss of autonomy and institutional function. Gender equality advocates are also beneficiaries, as the UCC is seen as a tool for reform. The constitutional court acts as an observer, mediating the contest.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    uniformity_vs_diversity_value,
    'Is legal uniformity in family law inherently superior to legal pluralism, or does pluralism offer distinct benefits (e.g., cultural preservation, minority rights)?',
    'Comparative legal studies examining outcomes in jurisdictions with different approaches to family law, assessed against a multi-criteria framework including equality, cultural vitality, and social cohesion.',
    'If pluralism is found to offer significant, non-replicable benefits, the ''coordination'' function of the UCC is weakened, potentially reclassifying the constraint closer to a Snare. If uniformity''s benefits are overwhelming, the Tangled Rope classification is reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(uniformity_vs_diversity_value, preference, 'The normative value assigned to legal uniformity versus diversity in family law.').

omega_variable(
    secularism_definition_ambiguity,
    'Is the ''secular'' state''s role one of strict separation from religion (non-interference) or active reform of religious practices (intervention for equality)?',
    'Analysis of constitutional jurisprudence and political discourse regarding the ''positive'' versus ''negative'' conceptions of secularism, and their practical implications for state-religion relations.',
    'If the ''positive'' (interventionist) secularism is deemed a valid interpretation, the legislative assertion of authority is strengthened. If ''negative'' (non-interference) secularism is prioritized, the constraint''s suppression of religious personal laws becomes more problematic, pushing it towards a Snare.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(secularism_definition_ambiguity, conceptual, 'Ambiguity in the definition and role of secularism in state governance.').

omega_variable(
    gender_equality_outcome_uncertainty,
    'Will a Uniform Civil Code genuinely improve gender equality across all communities, or will it create new forms of inequality or resistance?',
    'Empirical studies of the implementation and impact of existing uniform laws (where applicable) or gender-neutral provisions, disaggregated by community and socio-economic status.',
    'If the UCC fails to deliver on its gender equality promises, the coordination claim is undermined, making the extraction from minority communities less justifiable and pushing the constraint towards a Snare. If it demonstrably improves equality, the Tangled Rope classification is strengthened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(gender_equality_outcome_uncertainty, empirical, 'Uncertainty regarding the actual gender equality outcomes of a Uniform Civil Code.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(marriage_authority__secularist_reading, 1950, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(marr_tr_t1950, marriage_authority__secularist_reading, theater_ratio, 1950, 0.1).
narrative_ontology:measurement(marr_tr_t1970, marriage_authority__secularist_reading, theater_ratio, 1970, 0.12).
narrative_ontology:measurement(marr_tr_t1990, marriage_authority__secularist_reading, theater_ratio, 1990, 0.13).
narrative_ontology:measurement(marr_tr_t2010, marriage_authority__secularist_reading, theater_ratio, 2010, 0.14).
narrative_ontology:measurement(marr_tr_t2024, marriage_authority__secularist_reading, theater_ratio, 2024, 0.15).

% Extraction over time
narrative_ontology:measurement(marr_be_t1950, marriage_authority__secularist_reading, base_extractiveness, 1950, 0.6).
narrative_ontology:measurement(marr_be_t1970, marriage_authority__secularist_reading, base_extractiveness, 1970, 0.68).
narrative_ontology:measurement(marr_be_t1990, marriage_authority__secularist_reading, base_extractiveness, 1990, 0.72).
narrative_ontology:measurement(marr_be_t2010, marriage_authority__secularist_reading, base_extractiveness, 2010, 0.75).
narrative_ontology:measurement(marr_be_t2024, marriage_authority__secularist_reading, base_extractiveness, 2024, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(marr_su_t1950, marriage_authority__secularist_reading, suppression_requirement, 1950, 0.7).
narrative_ontology:measurement(marr_su_t1970, marriage_authority__secularist_reading, suppression_requirement, 1970, 0.75).
narrative_ontology:measurement(marr_su_t1990, marriage_authority__secularist_reading, suppression_requirement, 1990, 0.8).
narrative_ontology:measurement(marr_su_t2010, marriage_authority__secularist_reading, suppression_requirement, 2010, 0.83).
narrative_ontology:measurement(marr_su_t2024, marriage_authority__secularist_reading, suppression_requirement, 2024, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(marriage_authority__secularist_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(marriage_authority__secularist_reading, communal_autonomy_reading).
narrative_ontology:affects_constraint(marriage_authority__secularist_reading, gender_rights_reading).
narrative_ontology:affects_constraint(marriage_authority__secularist_reading, federalist_millet_reading).
narrative_ontology:affects_constraint(marriage_authority__secularist_reading, judicial_harmonization_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'marriage_authority' kernel. Its high extractiveness and suppression directly challenge the 'communal_autonomy_reading' and 'federalist_millet_reading', while influencing the 'gender_rights_reading' and 'judicial_harmonization_reading' by setting the legislative agenda.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
