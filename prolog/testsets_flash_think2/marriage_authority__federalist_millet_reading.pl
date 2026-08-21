% ============================================================================
% CONSTRAINT STORY: marriage_authority__federalist_millet_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2023-10-27
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_marriage_authority__federalist_millet_reading, []).

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
 *   constraint_id: marriage_authority__federalist_millet_reading
 *   human_readable: Federalist Millet System for Marriage Authority
 *   domain: legal/political/social
 *
 * SUMMARY:
 *   This constraint instantiates the 'federalist_millet_reading' of the
 *   'marriage_authority' kernel, which views fragmented legal authority over
 *   marriage as a deliberate consociational mechanism to prevent majoritarian
 *   domination. Sibling readings include 'communal_autonomy_reading',
 *   'secularist_reading', 'gender_rights_reading', and
 *   'judicial_harmonization_reading'. This reading frames legal pluralism in
 *   marriage as a deliberate fragmentation to prevent majoritarian
 *   domination, acting as a consociational anti-tyranny mechanism. It is a
 *   low-extraction Rope, benefiting minority communities and political elites
 *   by ensuring stability, with legislative paralysis seen as a feature
 *   rather than a bug.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(marriage_authority__federalist_millet_reading, 0.2).
domain_priors:suppression_score(marriage_authority__federalist_millet_reading, 0.25).
domain_priors:theater_ratio(marriage_authority__federalist_millet_reading, 0.13).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(marriage_authority__federalist_millet_reading, extractiveness, 0.2).
narrative_ontology:constraint_metric(marriage_authority__federalist_millet_reading, suppression_requirement, 0.25).
narrative_ontology:constraint_metric(marriage_authority__federalist_millet_reading, theater_ratio, 0.13).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(marriage_authority__federalist_millet_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(marriage_authority__federalist_millet_reading, resistance, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(marriage_authority__federalist_millet_reading, rope).
narrative_ontology:human_readable(marriage_authority__federalist_millet_reading, "Federalist Millet System for Marriage Authority").
narrative_ontology:topic_domain(marriage_authority__federalist_millet_reading, "legal/political/social").

domain_priors:requires_active_enforcement(marriage_authority__federalist_millet_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(marriage_authority__federalist_millet_reading, 'b794c17b-65c1-4b28-afe7-3a011c80f909').
narrative_ontology:cs_kernel_codification('b794c17b-65c1-4b28-afe7-3a011c80f909', formalized).
narrative_ontology:cs_authority_grounding('b794c17b-65c1-4b28-afe7-3a011c80f909', lineage).
narrative_ontology:cs_interpretation_layer_present('b794c17b-65c1-4b28-afe7-3a011c80f909').
narrative_ontology:cs_reading_relation('b794c17b-65c1-4b28-afe7-3a011c80f909', marriage_authority__communal_autonomy_reading, coexists_with).
narrative_ontology:cs_reading_relation('b794c17b-65c1-4b28-afe7-3a011c80f909', marriage_authority__secularist_reading, coexists_with).
narrative_ontology:cs_reading_relation('b794c17b-65c1-4b28-afe7-3a011c80f909', marriage_authority__gender_rights_reading, influences).
narrative_ontology:cs_reading_relation('b794c17b-65c1-4b28-afe7-3a011c80f909', marriage_authority__judicial_harmonization_reading, influences).
narrative_ontology:cs_axiom('b794c17b-65c1-4b28-afe7-3a011c80f909', foundational, pluralism_as_anti_tyranny).
narrative_ontology:cs_axiom_status(pluralism_as_anti_tyranny, holdable).
narrative_ontology:cs_axiom_grounding('b794c17b-65c1-4b28-afe7-3a011c80f909', pluralism_as_anti_tyranny, deontological).
narrative_ontology:cs_axiom('b794c17b-65c1-4b28-afe7-3a011c80f909', foundational, consociational_stability_is_good).
narrative_ontology:cs_axiom_status(consociational_stability_is_good, holdable).
narrative_ontology:cs_axiom_grounding('b794c17b-65c1-4b28-afe7-3a011c80f909', consociational_stability_is_good, instrumental).
narrative_ontology:cs_reference_frame('b794c17b-65c1-4b28-afe7-3a011c80f909', consociational_pluralism_framework).
narrative_ontology:cs_drift_state('b794c17b-65c1-4b28-afe7-3a011c80f909', contemporary_era, gap(stable, minor, true)).
narrative_ontology:cs_created_at('b794c17b-65c1-4b28-afe7-3a011c80f909', '').
narrative_ontology:cs_kernel_id(marriage_authority__federalist_millet_reading, marriage_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(marriage_authority__federalist_millet_reading, minority_religious_communities).
narrative_ontology:constraint_beneficiary(marriage_authority__federalist_millet_reading, political_elites).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(marriage_authority__federalist_millet_reading, majority_community).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefit from the autonomy to govern their own marriage and family laws according to their traditions, protected from majoritarian imposition. Their exit options are limited by their identity and desire to preserve cultural distinctiveness.
narrative_ontology:constraint_stakeholder(marriage_authority__federalist_millet_reading, minority_religious_communities, beneficiary,
    moderate, generational, constrained, national).

% Administer the consociational framework, benefiting from the political stability and reduced inter-communal conflict that this fragmented authority provides. They actively maintain the system to prevent majoritarian overreach.
narrative_ontology:constraint_stakeholder(marriage_authority__federalist_millet_reading, political_elites, agenda_setter,
    institutional, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(marriage_authority__federalist_millet_reading, political_elites, beneficiary).

% Bears the 'cost' of not having a unified civil code, which some members may desire for national integration or secular principles. Their ability to impose a uniform system is constrained by the consociational bargain.
narrative_ontology:constraint_stakeholder(marriage_authority__federalist_millet_reading, majority_community, payer,
    powerful, biographical, constrained, national).

% Advocate for a uniform civil code and secular governance of marriage, viewing legal pluralism as an impediment to individual rights and national unity. They are structurally marginalized from the decision-making process regarding the maintenance of the millet system.
narrative_ontology:constraint_stakeholder(marriage_authority__federalist_millet_reading, secular_reformers, excluded,
    organized, biographical, constrained, national).

% Acts as an arbiter, interpreting the constitutional limits of both communal autonomy and state intervention in personal law. While not directly setting the rules, its rulings shape the boundaries of the fragmented authority.
narrative_ontology:constraint_stakeholder(marriage_authority__federalist_millet_reading, constitutional_court, observer,
    institutional, civilizational, analytical, national).
narrative_ontology:stakeholder_secondary_role(marriage_authority__federalist_millet_reading, constitutional_court, agenda_setter).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates diverse religious and cultural marriage practices under a single state, preventing majoritarian domination and ensuring political stability by granting autonomy to minority communities in family law matters.
% TRANSFER_FUNCTION: Transfers the power to define and administer marriage law from a centralized state to various recognized communities, and transfers political stability and reduced inter-communal conflict to the state and its political elites.
% ABSENT_VOICES: Secular reformers and gender rights advocates are structurally marginalized; they would argue for a uniform civil code or greater individual rights within communities, but their concerns are often subordinated to the imperative of consociational stability.
% DISAPPEARANCE_RATIONALE: If this fragmented authority vanished overnight, it would likely lead to immediate political instability, inter-communal conflict over marriage norms, and a collapse of the consociational bargain, forcing a rapid, potentially violent, re-centralization or further fragmentation of legal authority.
% FOUNDING_PROBLEM: Preventing majoritarian tyranny and inter-communal conflict over deeply held religious and cultural norms related to family law, particularly in post-colonial or deeply plural societies with diverse populations.
% FOUNDING_PROBLEM_CORROBORATION: Political scientists specializing in consociationalism, comparative constitutional lawyers, and leaders of minority communities corroborate the ongoing relevance of this problem, citing persistent threats of majoritarianism and the need for mechanisms to manage diversity.
narrative_ontology:disappearance_verdict(marriage_authority__federalist_millet_reading, world_rearranges).
narrative_ontology:founding_problem_status(marriage_authority__federalist_millet_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(marriage_authority__federalist_millet_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(marriage_authority__federalist_millet_reading, 'none', 1).
narrative_ontology:epsilon_provenance(marriage_authority__federalist_millet_reading, 0.2, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(marriage_authority__federalist_millet_reading_tests).
:- end_tests(marriage_authority__federalist_millet_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The constraint is classified as a Rope due to its genuine coordination function in managing deep societal divisions and preventing majoritarian overreach. Extractiveness is low (0.20) because its primary function is to distribute authority and protect minority interests, rather than to extract rents. Suppression is also low (0.25) as it actively prevents majoritarian suppression, though it requires state enforcement to maintain the pluralistic system itself. Theater ratio is low (0.13) as its function is genuinely performed. The slight increases in metrics over time reflect minor institutional drift and the ongoing need for active management in a dynamic political landscape, but the core function remains stable.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of political elites and minority communities, this system is a successful and necessary mechanism for stability and protection. From the perspective of secular reformers and some within the majority community, it is an outdated impediment to national unity and individual rights. The engine's classification as a Rope reflects the structural benefits of the consociational bargain, while omegas address the contested nature of its outcomes for other seats.
 *
 * DIRECTIONALITY LOGIC:
 *   Minority religious communities are clear beneficiaries (d near 0.0) as the system grants them autonomy over their personal laws. Political elites are also beneficiaries (d near 0.0) due to the political stability and reduced conflict the system provides. The majority community and secular reformers are effectively payers or excluded (d near 1.0) as they bear the 'cost' of a non-uniform system and are marginalized from its maintenance, respectively. The constitutional court acts as an analytical observer and occasional agenda-setter, upholding the framework.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate — preventing majoritarian domination and inter-communal conflict over marriage laws — is still live. The threat of majoritarianism persists in many plural societies, and the consociational arrangement continues to serve its original purpose of managing this tension. Therefore, there is no evidence of mandatrophy; the constraint's function has not atrophied.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    elite_bargain_vs_individual_rights,
    'Is this federalist millet system primarily an anti-tyranny mechanism for communities, or does it entrench elite bargains that may suppress individual rights within those communities?',
    'Comparative legal analysis of individual rights protections within different personal law codes, and empirical studies on the agency and exit options of individuals (especially women and marginalized groups) within these communities.',
    'If it primarily entrenches elite bargains at the expense of individual rights, the effective extractiveness for individuals within communities would be higher, potentially shifting the classification for those seats towards a Snare or Tangled Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(elite_bargain_vs_individual_rights, conceptual, 'Ambiguity between communal protection and individual suppression.').

omega_variable(
    legislative_paralysis_feature_or_bug,
    'Is the legislative paralysis regarding a uniform civil code a ''feature'' (ensuring stability by preventing majoritarian imposition) or a ''bug'' (preventing necessary reforms and adaptation)?',
    'Analysis of legislative attempts at reform, public discourse on the UCC, and the political consequences of both maintaining and attempting to alter the status quo. This is a preference-driven question.',
    'If viewed as a ''bug'', the system''s resistance to change would be seen as a source of extraction for those desiring reform, increasing effective extractiveness for secular reformers and potentially shifting the overall classification towards a Tangled Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legislative_paralysis_feature_or_bug, preference, 'Contested interpretation of legislative inaction.').

omega_variable(
    state_neutrality_vs_enforcement,
    'How does the state balance its claim of neutrality in religious matters with its active enforcement of diverse personal law codes, and does this enforcement inadvertently legitimize potentially discriminatory practices?',
    'Legal and sociological studies examining the state''s role in adjudicating personal law disputes and the impact of state-sanctioned religious courts on individual liberties.',
    'If state enforcement is found to actively legitimize discriminatory practices, the suppression metric for affected individuals would increase, and the constraint''s effective extractiveness would rise for those seats.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(state_neutrality_vs_enforcement, empirical, 'State''s role in legitimizing communal laws.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(marriage_authority__federalist_millet_reading, 1947, 2023).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(marr_tr_t1947, marriage_authority__federalist_millet_reading, theater_ratio, 1947, 0.08).
narrative_ontology:measurement(marr_tr_t1960, marriage_authority__federalist_millet_reading, theater_ratio, 1960, 0.09).
narrative_ontology:measurement(marr_tr_t1975, marriage_authority__federalist_millet_reading, theater_ratio, 1975, 0.1).
narrative_ontology:measurement(marr_tr_t1990, marriage_authority__federalist_millet_reading, theater_ratio, 1990, 0.11).
narrative_ontology:measurement(marr_tr_t2005, marriage_authority__federalist_millet_reading, theater_ratio, 2005, 0.12).
narrative_ontology:measurement(marr_tr_t2023, marriage_authority__federalist_millet_reading, theater_ratio, 2023, 0.13).

% Extraction over time
narrative_ontology:measurement(marr_be_t1947, marriage_authority__federalist_millet_reading, base_extractiveness, 1947, 0.15).
narrative_ontology:measurement(marr_be_t1960, marriage_authority__federalist_millet_reading, base_extractiveness, 1960, 0.16).
narrative_ontology:measurement(marr_be_t1975, marriage_authority__federalist_millet_reading, base_extractiveness, 1975, 0.17).
narrative_ontology:measurement(marr_be_t1990, marriage_authority__federalist_millet_reading, base_extractiveness, 1990, 0.18).
narrative_ontology:measurement(marr_be_t2005, marriage_authority__federalist_millet_reading, base_extractiveness, 2005, 0.19).
narrative_ontology:measurement(marr_be_t2023, marriage_authority__federalist_millet_reading, base_extractiveness, 2023, 0.2).

% Suppression requirement over time
narrative_ontology:measurement(marr_su_t1947, marriage_authority__federalist_millet_reading, suppression_requirement, 1947, 0.2).
narrative_ontology:measurement(marr_su_t1960, marriage_authority__federalist_millet_reading, suppression_requirement, 1960, 0.21).
narrative_ontology:measurement(marr_su_t1975, marriage_authority__federalist_millet_reading, suppression_requirement, 1975, 0.22).
narrative_ontology:measurement(marr_su_t1990, marriage_authority__federalist_millet_reading, suppression_requirement, 1990, 0.23).
narrative_ontology:measurement(marr_su_t2005, marriage_authority__federalist_millet_reading, suppression_requirement, 2005, 0.24).
narrative_ontology:measurement(marr_su_t2023, marriage_authority__federalist_millet_reading, suppression_requirement, 2023, 0.25).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(marriage_authority__federalist_millet_reading, identity_coordination).
narrative_ontology:affects_constraint(marriage_authority__federalist_millet_reading, marriage_authority__communal_autonomy_reading).
narrative_ontology:affects_constraint(marriage_authority__federalist_millet_reading, marriage_authority__secularist_reading).
narrative_ontology:affects_constraint(marriage_authority__federalist_millet_reading, marriage_authority__gender_rights_reading).
narrative_ontology:affects_constraint(marriage_authority__federalist_millet_reading, marriage_authority__judicial_harmonization_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'marriage_authority' kernel, focusing on legal pluralism as a consociational anti-tyranny mechanism. It is part of a family of constraints that interpret the same kernel from different perspectives, each with distinct structural properties and classifications.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
