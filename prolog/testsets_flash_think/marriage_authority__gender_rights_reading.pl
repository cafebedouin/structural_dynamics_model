% ============================================================================
% CONSTRAINT STORY: marriage_authority__gender_rights_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_marriage_authority__gender_rights_reading, []).

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
 *   constraint_id: marriage_authority__gender_rights_reading
 *   human_readable: Marriage Authority (Gender Rights Reading)
 *   domain: legal/constitutional/social
 *
 * SUMMARY:
 *   This constraint story analyzes the 'marriage authority' kernel through
 *   the lens of a 'gender rights reading,' focusing on the contestation of
 *   intra-community gender inequality within personal laws and the role of
 *   judicial expansion of constitutional equality guarantees. The constraint,
 *   from this reading's perspective, is the existing, often discriminatory,
 *   marriage authority structure. It is classified as a Snare due to its high
 *   extraction from women and the active suppression required to maintain
 *   traditional norms against constitutional challenges. The metrics reflect
 *   the persistent, high burden on women, even as judicial efforts slowly
 *   chip away at its severity.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(marriage_authority__gender_rights_reading, 0.85).
domain_priors:suppression_score(marriage_authority__gender_rights_reading, 0.78).
domain_priors:theater_ratio(marriage_authority__gender_rights_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(marriage_authority__gender_rights_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(marriage_authority__gender_rights_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(marriage_authority__gender_rights_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(marriage_authority__gender_rights_reading, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(marriage_authority__gender_rights_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(marriage_authority__gender_rights_reading, snare).
narrative_ontology:human_readable(marriage_authority__gender_rights_reading, "Marriage Authority (Gender Rights Reading)").
narrative_ontology:topic_domain(marriage_authority__gender_rights_reading, "legal/constitutional/social").

domain_priors:requires_active_enforcement(marriage_authority__gender_rights_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(marriage_authority__gender_rights_reading, '15ce1a7f-7b06-4971-9384-d885ee405772').
narrative_ontology:cs_kernel_codification('15ce1a7f-7b06-4971-9384-d885ee405772', formalized).
narrative_ontology:cs_authority_grounding('15ce1a7f-7b06-4971-9384-d885ee405772', lineage).
narrative_ontology:cs_interpretation_layer_present('15ce1a7f-7b06-4971-9384-d885ee405772').
narrative_ontology:cs_reading_relation('15ce1a7f-7b06-4971-9384-d885ee405772', marriage_authority__communal_autonomy_reading, forecloses).
narrative_ontology:cs_reading_relation('15ce1a7f-7b06-4971-9384-d885ee405772', marriage_authority__federalist_millet_reading, forecloses).
narrative_ontology:cs_reading_relation('15ce1a7f-7b06-4971-9384-d885ee405772', marriage_authority__judicial_harmonization_reading, coexists_with).
narrative_ontology:cs_reading_relation('15ce1a7f-7b06-4971-9384-d885ee405772', marriage_authority__secularist_reading, influences).
narrative_ontology:cs_axiom('15ce1a7f-7b06-4971-9384-d885ee405772', foundational, gender_equality_is_fundamental_right).
narrative_ontology:cs_axiom_status(gender_equality_is_fundamental_right, holdable).
narrative_ontology:cs_axiom_grounding('15ce1a7f-7b06-4971-9384-d885ee405772', gender_equality_is_fundamental_right, deontological).
narrative_ontology:cs_axiom('15ce1a7f-7b06-4971-9384-d885ee405772', foundational, constitutional_supremacy_over_personal_law).
narrative_ontology:cs_axiom_status(constitutional_supremacy_over_personal_law, holdable).
narrative_ontology:cs_axiom_grounding('15ce1a7f-7b06-4971-9384-d885ee405772', constitutional_supremacy_over_personal_law, conventional).
narrative_ontology:cs_reference_frame('15ce1a7f-7b06-4971-9384-d885ee405772', constitutional_equality_principle).
narrative_ontology:cs_drift_state('15ce1a7f-7b06-4971-9384-d885ee405772', contemporary_judicial_activism, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('15ce1a7f-7b06-4971-9384-d885ee405772', '').
narrative_ontology:cs_kernel_id(marriage_authority__gender_rights_reading, marriage_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(marriage_authority__gender_rights_reading, women_rights_advocates).
narrative_ontology:constraint_victim(marriage_authority__gender_rights_reading, women_within_patriarchal_personal_law).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(marriage_authority__gender_rights_reading, secularist_reformers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Actively litigate and advocate for reforms to personal laws, seeking to expand constitutional equality guarantees to women within religious communities. They benefit from judicial victories that affirm gender equality.
narrative_ontology:constraint_stakeholder(marriage_authority__gender_rights_reading, women_rights_advocates, beneficiary,
    organized, biographical, mobile, national).

% Bear the direct costs of unequal personal laws, such as discriminatory divorce practices (e.g., triple talaq), unequal maintenance, and property rights. Their options for redress are limited, often requiring challenging deeply entrenched community norms and legal structures.
narrative_ontology:constraint_stakeholder(marriage_authority__gender_rights_reading, women_within_patriarchal_personal_law, payer,
    powerless, biographical, trapped, local).

% Interpret and enforce personal laws based on religious traditions, often resisting judicial interference as an infringement on communal autonomy. They benefit from maintaining their authority and the existing social order.
narrative_ontology:constraint_stakeholder(marriage_authority__gender_rights_reading, traditional_religious_authorities, agenda_setter,
    institutional, generational, constrained, regional).

% Serve as the primary interpreters of constitutional equality guarantees, actively expanding their application to personal law. They drive reform by striking down discriminatory practices and setting new legal precedents.
narrative_ontology:constraint_stakeholder(marriage_authority__gender_rights_reading, constitutional_courts, agenda_setter,
    institutional, generational, analytical, national).

% Possess the power to enact a Uniform Civil Code or specific reforms to personal laws but often defer to judicial action due to the political sensitivity of religious and communal issues. They observe and react to judicial developments.
narrative_ontology:constraint_stakeholder(marriage_authority__gender_rights_reading, state_legislatures, observer,
    institutional, biographical, mobile, national).

% Advocate for a complete separation of religion and state in family law, often through a Uniform Civil Code. They view judicial expansion of gender equality as a positive step towards their broader goals.
narrative_ontology:constraint_stakeholder(marriage_authority__gender_rights_reading, secularist_reformers, beneficiary,
    organized, biographical, mobile, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Aims to coordinate legal interpretations across diverse personal law systems to ensure a minimum standard of gender equality, resolving conflicts between constitutional principles and traditional norms.
% TRANSFER_FUNCTION: Transfers legal rights and protections from traditional communal authority to individual women, backed by state constitutional authority. It also transfers power from traditional religious bodies to the judiciary.
% ABSENT_VOICES: Women deeply embedded in traditional communities who may not perceive their situation as unjust, or who prioritize community cohesion over individual legal rights, are often absent from the direct advocacy and litigation. Their perspectives are mediated by community leaders or advocates.
% DISAPPEARANCE_RATIONALE: If judicial expansion of constitutional equality guarantees vanished, personal law systems would revert to their more traditional, often unequal, forms, and women's rights would significantly regress in these communities, leading to a reorganization of legal and social power dynamics.
% FOUNDING_PROBLEM: The historical conflict between constitutional guarantees of equality and the existence of diverse personal laws that often enshrine gender inequality within religious communities.
% FOUNDING_PROBLEM_CORROBORATION: Women's rights organizations, international human rights bodies, and independent legal scholars consistently attest to the ongoing problem of gender inequality within personal laws, corroborating the judiciary's role in addressing it. Traditional religious authorities, however, contest the premise that these laws are problematic.
narrative_ontology:disappearance_verdict(marriage_authority__gender_rights_reading, world_rearranges).
narrative_ontology:founding_problem_status(marriage_authority__gender_rights_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(marriage_authority__gender_rights_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(marriage_authority__gender_rights_reading, 'none', 1).
narrative_ontology:epsilon_provenance(marriage_authority__gender_rights_reading, 0.85, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(marriage_authority__gender_rights_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(marriage_authority__gender_rights_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(marriage_authority__gender_rights_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The base extractiveness (0.85) is high, reflecting the significant disadvantages and burdens placed on women by patriarchal personal laws concerning divorce, maintenance, and property. Suppression (0.78) is also high, as traditional authorities actively resist reforms and women often face social and legal barriers to challenging these norms. The theater ratio (0.15) is low because the inequality is a direct, functional outcome, not a performative one. Accessibility collapse (0.80) is high due to the limited legal and social alternatives for women within these systems. Resistance (0.70) is substantial, driven by women's rights advocates and judicial activism. The temporal measurements show a very slow, almost stable, decline in extractiveness and suppression, indicating the deep-seated nature of the constraint and the incremental pace of reform.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of traditional religious authorities, the marriage authority is a legitimate, divinely ordained, or historically established system that ensures communal identity and order. From the gender rights reading, the same authority is a deeply extractive and suppressive mechanism that violates fundamental constitutional rights. The engine's classification will highlight this divergence by computing a Snare for the victims and a more benign type for the traditional authorities, reflecting the structural asymmetry.
 *
 * DIRECTIONALITY LOGIC:
 *   Women's rights advocates are beneficiaries as they gain legal ground and influence through judicial reforms (low d). Women within patriarchal personal law are clear victims, bearing the direct costs of inequality (high d). Traditional religious authorities are agenda-setters who benefit from maintaining the status quo and bear costs when their authority is challenged (d near beneficiary, but shifting towards symmetric as they face resistance). Constitutional courts are agenda-setters driving change, acting as a force against the constraint's extractive nature (low d).
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    judicial_legitimacy_vs_communal_autonomy,
    'Is judicial intervention into personal laws seen as legitimate constitutional enforcement or as overreach into communal autonomy and religious freedom?',
    'Analysis of public opinion, legal scholarship, and political discourse regarding specific judicial rulings; assessment of compliance rates and social backlash.',
    'If widely seen as overreach, judicial reforms may face greater resistance and lower compliance, potentially increasing the constraint''s effective suppression. If seen as legitimate, reforms may be more readily adopted, reducing extraction over time.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(judicial_legitimacy_vs_communal_autonomy, conceptual, 'Ambiguity regarding the legitimacy of judicial intervention in personal law.').

omega_variable(
    pace_of_reform_vs_social_acceptance,
    'Is the pace of judicial reform outstripping social acceptance within affected communities, leading to backlash or non-compliance?',
    'Sociological studies on community attitudes towards reforms, analysis of local implementation challenges, and observation of informal dispute resolution mechanisms.',
    'If reforms outpace acceptance, the effective suppression and extractiveness may remain high due to informal enforcement of traditional norms, despite legal changes. If social acceptance keeps pace, the constraint''s severity could genuinely diminish.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(pace_of_reform_vs_social_acceptance, empirical, 'Whether judicial reforms are effectively translated into social practice.').

omega_variable(
    internalized_inequality_vs_structural_suppression,
    'To what extent is the inequality experienced by women within personal law due to structural legal barriers versus internalized norms and lack of agency within communities?',
    'Post-reform studies: if inequality persists significantly after legal barriers are removed, it suggests a stronger internalized component. Qualitative research on women''s perceptions and choices.',
    'If internalized inequality is a major factor, the constraint''s effective suppression is higher than structural measures suggest, as women may not utilize available legal remedies. This would require broader social interventions beyond legal reform to address.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(internalized_inequality_vs_structural_suppression, empirical, 'Distinguishing structural from internalized components of gender inequality.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(marriage_authority__gender_rights_reading, 1990, 2020).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(marr_tr_t1990, marriage_authority__gender_rights_reading, theater_ratio, 1990, 0.15).
narrative_ontology:measurement(marr_tr_t1996, marriage_authority__gender_rights_reading, theater_ratio, 1996, 0.15).
narrative_ontology:measurement(marr_tr_t2002, marriage_authority__gender_rights_reading, theater_ratio, 2002, 0.15).
narrative_ontology:measurement(marr_tr_t2008, marriage_authority__gender_rights_reading, theater_ratio, 2008, 0.15).
narrative_ontology:measurement(marr_tr_t2014, marriage_authority__gender_rights_reading, theater_ratio, 2014, 0.15).
narrative_ontology:measurement(marr_tr_t2020, marriage_authority__gender_rights_reading, theater_ratio, 2020, 0.15).

% Extraction over time
narrative_ontology:measurement(marr_be_t1990, marriage_authority__gender_rights_reading, base_extractiveness, 1990, 0.88).
narrative_ontology:measurement(marr_be_t1996, marriage_authority__gender_rights_reading, base_extractiveness, 1996, 0.87).
narrative_ontology:measurement(marr_be_t2002, marriage_authority__gender_rights_reading, base_extractiveness, 2002, 0.86).
narrative_ontology:measurement(marr_be_t2008, marriage_authority__gender_rights_reading, base_extractiveness, 2008, 0.85).
narrative_ontology:measurement(marr_be_t2014, marriage_authority__gender_rights_reading, base_extractiveness, 2014, 0.85).
narrative_ontology:measurement(marr_be_t2020, marriage_authority__gender_rights_reading, base_extractiveness, 2020, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(marr_su_t1990, marriage_authority__gender_rights_reading, suppression_requirement, 1990, 0.8).
narrative_ontology:measurement(marr_su_t1996, marriage_authority__gender_rights_reading, suppression_requirement, 1996, 0.79).
narrative_ontology:measurement(marr_su_t2002, marriage_authority__gender_rights_reading, suppression_requirement, 2002, 0.78).
narrative_ontology:measurement(marr_su_t2008, marriage_authority__gender_rights_reading, suppression_requirement, 2008, 0.78).
narrative_ontology:measurement(marr_su_t2014, marriage_authority__gender_rights_reading, suppression_requirement, 2014, 0.78).
narrative_ontology:measurement(marr_su_t2020, marriage_authority__gender_rights_reading, suppression_requirement, 2020, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
